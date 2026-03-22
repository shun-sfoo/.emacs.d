;;; rmpv.el --- Control mpv via socket IPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Neo

;; Author: Neo <neo@example.com>
;; Maintainer: Neo <neo@example.com>
;; URL: https://github.com/neo/rmpv.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; License: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; rmpv.el provides modern mpv integration for Emacs via Unix socket IPC.
;; Features include:
;; - Video/audio playback control
;; - Subtitle loading and frame display
;; - Real-time subtitle highlighting

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

(defgroup rmpv nil
  "Modern mpv player interface for Emacs."
  :group 'multimedia
  :link '(url-link "https://github.com/neo/rmpv.el"))

(defcustom rmpv-mpv-path
  (or (executable-find "mpv") "mpv")
  "Path to the mpv executable."
  :type 'file
  :group 'rmpv)

(defcustom rmpv-socket-directory
  (format "/tmp/mpv-ipc-%s.sock" (emacs-pid))
  "Directory for mpv IPC socket files."
  :type 'directory
  :group 'rmpv)

(defcustom rmpv-subtitle-update-interval
  0.1
  "Interval in seconds for subtitle highlight updates."
  :type 'float
  :group 'rmpv)

(defface rmpv-subtitle-highlight-face
  '((t (:background "dark slate gray" :foreground "white")))
  "Face for subtitle highlight."
  :group 'rmpv)

(cl-defstruct (rmpv--subtitle (:constructor rmpv--subtitle-create))
  (start 0.0 :type float)
  (end 0.0 :type float)
  (text "" :type string))

(cl-defstruct (rmpv--callback (:constructor rmpv--callback-create))
  (id 0 :type integer)
  (fn nil :type (or null function)))

(defvar rmpv--process nil)
(defvar rmpv--socket-path nil)
(defvar rmpv--socket-proc nil)
(defvar rmpv--socket-buffer " *rmpv-ipc*")
(defvar rmpv--command-id 0)
(defvar rmpv--last-time -1.0)
(defvar rmpv--pending-callbacks nil)
(defvar rmpv--stopping nil)

(defvar rmpv-subtitle-timer nil)
(defvar rmpv-subtitle-current-index nil)
(defvar rmpv-subtitle-entries nil)
(defvar rmpv-subtitle-frame nil)
(defvar rmpv-subtitle-last-index -1)
(defvar-local rmpv-subtitle-video-path nil)

(defcustom rmpv-debug nil
  "Enable debug logging for rmpv."
  :type 'boolean
  :group 'rmpv)

(defmacro rmpv--debug (&rest args)
  "Print debug message when `rmpv-debug' is non-nil."
  (declare (indent 1))
  `(when rmpv-debug
     (message "[rmpv] %s" (format ,@args))))

(defun rmpv--filter (proc output)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output)
    (rmpv--process-output)))

(defun rmpv--process-output ()
  (goto-char (point-min))
  (while (re-search-forward "{[^}]+}\n?" nil t)
    (when-let* ((json-str (match-string 0))
                (json (ignore-errors (json-parse-string json-str :object-type 'alist))))
      (rmpv--handle-message json))))

(defun rmpv--handle-message (msg)
  (let ((reply-id (alist-get 'reply_id msg))
        (event (alist-get 'event msg))
        (data (alist-get 'data msg))
        (name (alist-get 'name msg))
        (value (alist-get 'value msg)))
    (pcase event
      ("property-change"
       (when (string= name "time-pos")
         (let ((new-time (or value data)))
           (when (numberp new-time)
             (setq rmpv--last-time new-time)))))
      ("idle"
       (rmpv--debug "idle event, cleaning up")
       (rmpv--cleanup))
      ("end-file"
       (rmpv--debug "end-file event")
       (rmpv--cleanup))
      ("file-loaded"
       (rmpv--debug "file loaded")
       (setq rmpv--last-time 0.0
             rmpv-subtitle-last-index -1))
      (_
       (when reply-id
         (when-let ((cb (seq-some (lambda (c)
                                    (when (= (rmpv--callback-id c) reply-id) c))
                                  rmpv--pending-callbacks)))
           (funcall (rmpv--callback-fn cb) (alist-get 'error msg) data)
           (setq rmpv--pending-callbacks
                 (seq-remove (lambda (c) (= (rmpv--callback-id c) reply-id))
                             rmpv--pending-callbacks))))))
    (when (and (listp data) (string= (alist-get 'type data) "time"))
      (setq rmpv--last-time (alist-get 'value data)))))

(defun rmpv--cleanup ()
  "Clean up all resources."
  (rmpv-subtitle-stop-update)
  (when rmpv--socket-proc
    (delete-process rmpv--socket-proc)
    (setq rmpv--socket-proc nil))
  (setq rmpv--process nil
        rmpv--last-time -1.0
        rmpv--stopping nil))

(defun rmpv--sentinel (proc status)
  (message "rmpv: %s" status)
  (when (string-match "closed\\|exited\\|terminated" status)
    (rmpv--cleanup)))

(defun rmpv--send (obj)
  (when (and rmpv--socket-proc (process-live-p rmpv--socket-proc))
    (process-send-string rmpv--socket-proc
                        (concat (json-encode obj) "\n"))))

(defun rmpv--connect ()
  (when (and rmpv--socket-path (file-exists-p rmpv--socket-path))
    (condition-case-unless-debug _
        (progn
          (setq rmpv--socket-proc
                (make-network-process
                 :name "rmpv-ipc"
                 :buffer rmpv--socket-buffer
                 :family 'local
                 :service rmpv--socket-path
                 :filter #'rmpv--filter
                 :sentinel #'rmpv--sentinel))
          (rmpv--observe-time-pos))
      (error (run-with-timer 0.1 nil #'rmpv--connect)))))

(defun rmpv--observe-time-pos ()
  "Start observing time-pos property."
  (rmpv--send (list (cons 'command (vector "observe_property" 1 "time-pos"))
                    (cons 'request_id (cl-incf rmpv--command-id)))))

;;;###autoload
(defun rmpv-play (path)
  "Play video file PATH."
  (interactive "fVideo file: ")
  (rmpv--cleanup)
  (setq rmpv--socket-path (format "/tmp/mpv-ipc-%s.sock" (emacs-pid)))
  (when (file-exists-p rmpv--socket-path)
    (delete-file rmpv--socket-path))
  (setq rmpv--process
        (make-process
         :name "rmpv"
         :buffer (generate-new-buffer " *rmpv*")
         :command (list rmpv-mpv-path
                        "--no-terminal"
                        "--osc=no"
                        "--osd-bar=no"
                        "--sub-auto=no"
                        (format "--input-ipc-server=%s" rmpv--socket-path)
                        (expand-file-name path))
         :sentinel (lambda (p s)
                     (message "rmpv: mpv %s" s)
                     (when (string-match "exited\\|terminated\\|closed" s)
                       (rmpv--cleanup)))))
  (run-with-timer 0.1 nil #'rmpv--connect))

;;;###autoload
(defun rmpv-pause ()
  "Pause playback."
  (interactive)
  (rmpv--send (list (cons 'command (vector "set" "pause" "yes")))))

;;;###autoload
(defun rmpv-resume ()
  "Resume playback."
  (interactive)
  (rmpv--send (list (cons 'command (vector "set" "pause" "no")))))

;;;###autoload
(defun rmpv-stop ()
  "Stop playback."
  (interactive)
  (unless rmpv--stopping
    (setq rmpv--stopping t)
    (rmpv--send (list (cons 'command (vector "quit"))))
    (sleep-for 0.1)
    (rmpv--cleanup)
    (when (and rmpv--socket-path (file-exists-p rmpv--socket-path))
      (delete-file rmpv--socket-path))))

(defun rmpv-get-time (&optional callback)
  "Get current time. If CALLBACK is provided, call it with the time value."
  (let ((req-id (cl-incf rmpv--command-id)))
    (rmpv--send (list (cons 'command (vector "get_property" "time-pos"))
                      (cons 'request_id req-id)))
    (if callback
        (push (rmpv--callback-create :id req-id :fn callback) rmpv--pending-callbacks)
      (let ((end (+ (float-time) 0.5)))
        (while (and (not rmpv--last-time) (< (float-time) end)))
        rmpv--last-time))))

;;;###autoload
(defun rmpv-load-subtitle (path)
  "Load subtitle into video."
  (interactive "fSubtitle file: ")
  (rmpv--send (list (cons 'command (vector "sub-add" (expand-file-name path))))))

(define-derived-mode rmpv-subtitle-mode special-mode "Subtitle"
  "Major mode for displaying subtitles."
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(defun rmpv-parse-timestamp (ts)
  "Parse SRT timestamp like '00:00:01,500' to seconds as float."
  (when ts
    (let ((cleaned (replace-regexp-in-string "[ \t\r\n]+" "" ts)))
      (rmpv--debug "parse-timestamp input: %S -> cleaned: %S" ts cleaned)
      (if (string-match (concat "\\`\\([0-9]+\\):\\([0-9]+\\):"
                                "\\([0-9]+\\)\\([,.]\\)\\([0-9]+\\)\\'") cleaned)
          (let ((result (+ (* 3600 (string-to-number (match-string 1 cleaned)))
                           (* 60 (string-to-number (match-string 2 cleaned)))
                           (string-to-number (match-string 3 cleaned))
                           (/ (string-to-number (match-string 5 cleaned)) 1000.0))))
            (rmpv--debug "parse-timestamp result: %s" result)
            result)
        (rmpv--debug "parse-timestamp FAILED for: %S" cleaned)
        nil))))

(defun rmpv-parse-srt-from-string (content)
  "Parse raw SRT string CONTENT into a list of subtitle entries."
  (seq-into
   (seq-map (lambda (block)
               (when (string-match (concat "\\`[0-9]+[ \t]*\n"
                                          "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)"
                                          "[ \t]+-->?[ \t]+"
                                          "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)") block)
                 (let* ((start-str (match-string 1 block))
                        (end-str (match-string 2 block))
                        (start (or (rmpv-parse-timestamp start-str) 0.0))
                        (end (or (rmpv-parse-timestamp end-str) start))
                        (text (string-trim (car (last (split-string block "\n" t))))))
                   (rmpv--debug "parsed block: start=%S end=%S text=%S" start end text)
                   (rmpv--subtitle-create :start start :end end :text text))))
            (split-string content "\r?\n\r?\n" t))
   'list))

(defun rmpv-load-subtitle-file (srt-path)
  "Load SRT file from SRT-PATH and display in subtitle frame."
  (rmpv--debug "loading subtitle: %s" srt-path)
  (let* ((raw-entries (with-temp-buffer
                        (insert-file-contents srt-path)
                        (rmpv-parse-srt-from-string (buffer-string))))
         (entries (seq-filter #'identity raw-entries))
         (buf-name " *rmpv Subtitle*"))
    (rmpv--debug "parsed %d entries from subtitle" (length entries))
    (setq rmpv-subtitle-entries entries
          rmpv-subtitle-last-index -1)
    (when (and rmpv-subtitle-frame (frame-live-p rmpv-subtitle-frame))
      (delete-frame rmpv-subtitle-frame))
    (let ((buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)
        (rmpv-subtitle-mode)
        (dolist (entry entries)
          (insert (format "[%05.1f] %s\n"
                          (rmpv--subtitle-start entry)
                          (rmpv--subtitle-text entry))))
        (setq buffer-read-only t
              rmpv-subtitle-current-index nil))
      (setq rmpv-subtitle-frame
            (make-frame '((name . "rmpv Subtitle")
                          (minibuffer . nil)
                          (auto-raise . t)
                          (frame-resize-pixelwise . t))))
      (select-frame rmpv-subtitle-frame)
      (switch-to-buffer buf))
    rmpv-subtitle-frame))

(defun rmpv-find-subtitle-at-time (time)
  "Find subtitle entry at TIME (in seconds)."
  (seq-find (lambda (entry)
              (and (numberp (rmpv--subtitle-start entry))
                   (numberp (rmpv--subtitle-end entry))
                   (<= (rmpv--subtitle-start entry) time)
                   (< time (rmpv--subtitle-end entry))))
            rmpv-subtitle-entries))

(defun rmpv-get-subtitle-index (time)
  "Get the index of subtitle entry at TIME (in seconds)."
  (seq-position rmpv-subtitle-entries
                (rmpv-find-subtitle-at-time time)
                #'rmpv--subtitle-equal))

(defun rmpv--subtitle-equal (a b)
  "Compare two subtitles for equality."
  (and (equal (rmpv--subtitle-start a) (rmpv--subtitle-start b))
       (equal (rmpv--subtitle-end a) (rmpv--subtitle-end b))
       (equal (rmpv--subtitle-text a) (rmpv--subtitle-text b))))

(defun rmpv-update-subtitle-highlight ()
  "Update subtitle highlight based on current playback time."
  (let ((time rmpv--last-time)
        (entries-len (length (or rmpv-subtitle-entries [])))
        (frame-valid (and rmpv-subtitle-frame
                          (frame-live-p rmpv-subtitle-frame))))
    (when (and (numberp time) (>= time 0)
               entries-len frame-valid)
      (if-let* ((entry (rmpv-find-subtitle-at-time time))
               (idx (seq-position rmpv-subtitle-entries entry
                                 #'rmpv--subtitle-equal))
               ((/= idx rmpv-subtitle-last-index)))
          (progn
            (rmpv--debug "highlight: idx=%s text=%S" idx (rmpv--subtitle-text entry))
            (setq rmpv-subtitle-last-index idx)
            (with-current-buffer (window-buffer (frame-first-window rmpv-subtitle-frame))
              (remove-overlays (point-min) (point-max) 'rmpv-subtitle-highlight t)
              (goto-char (point-min))
              (forward-line idx)
              (let ((ov (make-overlay (point) (line-end-position))))
                (overlay-put ov 'rmpv-subtitle-highlight t)
                (overlay-put ov 'face 'rmpv-subtitle-highlight-face)
                (setq rmpv-subtitle-current-index (point)))))
        (rmpv--debug "no match: time=%s entries=%d first=[%s-%s] %S"
                      time entries-len
                      (if entries-len (rmpv--subtitle-start (car rmpv-subtitle-entries)) nil)
                      (if entries-len (rmpv--subtitle-end (car rmpv-subtitle-entries)) nil)
                      (if entries-len (rmpv--subtitle-text (car rmpv-subtitle-entries)) nil))))))

(defun rmpv-subtitle-auto-update ()
  "Start auto-update timer for subtitle highlight."
  (setq rmpv-subtitle-timer
        (run-with-timer 0 rmpv-subtitle-update-interval
                        #'rmpv-update-subtitle-highlight)))

(defun rmpv-subtitle-stop-update ()
  "Stop auto-update timer."
  (when rmpv-subtitle-timer
    (cancel-timer rmpv-subtitle-timer)
    (setq rmpv-subtitle-timer nil))
  (when rmpv-subtitle-frame
    (when (frame-live-p rmpv-subtitle-frame)
      (delete-frame rmpv-subtitle-frame))
    (setq rmpv-subtitle-frame nil
          rmpv-subtitle-entries nil
          rmpv-subtitle-last-index -1)))

;;;###autoload
(defun rmpv-play-with-subtitle (video-path &optional external-only)
  "Play VIDEO-PATH and load corresponding subtitle if exists.
With prefix arg EXTERNAL-ONLY, only display subtitle in frame."
  (interactive "fVideo file: \np")
  (rmpv-play video-path)
  (setq rmpv-subtitle-video-path video-path)
  (if-let ((srt-path (concat (file-name-sans-extension video-path) ".srt"))
           ((file-exists-p srt-path)))
      (progn
        (rmpv-load-subtitle-file srt-path)
        (rmpv-subtitle-auto-update)
        (unless external-only
          (rmpv-load-subtitle srt-path)))
    (message "No subtitle file found for %s" video-path)))

(provide 'rmpv)

;;; rmpv.el ends here
