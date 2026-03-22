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

(eval-when-compile
  (require 'seq)
  (require 'subr-x))

(require 'json)
(require 'cl-lib)

(define-error 'rmpv-error "rmpv error")
(define-error 'rmpv-connect-failed "Connection failed" 'rmpv-error)
(define-error 'rmpv-timeout "Operation timeout" 'rmpv-error)

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

(cl-defstruct (rmpv-session (:constructor rmpv-session--create))
  "Session structure for mpv playback state."
  (process nil :type (or null process))
  (socket-proc nil :type (or null process))
  (socket-buffer " *rmpv-ipc*" :type string)
  (socket-path nil :type (or null string))
  (command-id 0 :type integer)
  (last-time -1.0 :type float)
  (pending-callbacks nil :type list)
  (stopping nil :type boolean)
  (subtitle-timer nil :type (or null timer))
  (subtitle-current-index nil :type (or null integer))
  (subtitle-entries nil :type list)
  (subtitle-frame nil :type (or null frame))
  (subtitle-last-index -1 :type integer))

(defvar rmpv-session nil "Current mpv session.")

(defcustom rmpv-debug nil
  "Enable debug logging for rmpv."
  :type 'boolean
  :group 'rmpv)

(defmacro rmpv--debug (&rest args)
  "Print debug message when `rmpv-debug' is non-nil."
  (declare (indent 1))
  `(when rmpv-debug
     (message "[rmpv] %s" (format ,@args))))

(defmacro rmpv--with-session (&rest body)
  "Execute BODY only when rmpv-session is active."
  (declare (indent 0))
  `(when rmpv-session ,@body))

(defun rmpv--next-id ()
  "Get next command ID."
  (when rmpv-session
    (setf (rmpv-session-command-id rmpv-session)
          (1+ (rmpv-session-command-id rmpv-session)))
    (rmpv-session-command-id rmpv-session)))

(cl-defun rmpv--cmd (command &key callback)
  "Send COMMAND to mpv.
COMMAND is a list representing the mpv command.
If CALLBACK is provided, it will be called with (error data)."
  (when (and rmpv-session
             (rmpv-session-socket-proc rmpv-session)
             (process-live-p (rmpv-session-socket-proc rmpv-session)))
    (let ((req-id (rmpv--next-id)))
      (when callback
        (push (rmpv--callback-create :id req-id :fn callback)
              (rmpv-session-pending-callbacks rmpv-session)))
      (process-send-string (rmpv-session-socket-proc rmpv-session)
                           (concat (json-encode `((command . ,command)
                                                  (request_id . ,req-id)))
                                   "\n")))))

(cl-defun rmpv--property (name &key observe callback)
  "Get or observe property NAME.
If OBSERVE is non-nil, start observing the property.
If CALLBACK is provided, call it with the property value."
  (rmpv--cmd (if observe
                 (list "observe_property" (or observe 1) name)
               (list "get_property" name))
             :callback callback))

(defvar-keymap rmpv-mode-map
  :doc "Keymap for rmpv minor mode."
  "SPC" #'rmpv-pause
  "s" #'rmpv-stop
  "f" #'rmpv-seek-forward
  "b" #'rmpv-seek-backward)

(define-minor-mode rmpv-mode
  "Minor mode for rmpv playback controls.
When called with a prefix argument, enable if ARG is positive."
  :lighter " rmpv"
  :keymap rmpv-mode-map
  :group 'rmpv
  (if rmpv-mode
      (rmpv--mode-on)
    (rmpv--mode-off)))

(defun rmpv--mode-on ()
  "Enable rmpv mode."
  (rmpv--debug "mode enabled"))

(defun rmpv--mode-off ()
  "Disable rmpv mode."
  (rmpv--debug "mode disabled"))

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
  (when rmpv-session
    (let ((reply-id (alist-get 'reply_id msg))
          (data (alist-get 'data msg))
          (event (alist-get 'event msg))
          (name (alist-get 'name msg))
          (value (alist-get 'value msg)))
      (pcase event
        ("property-change"
         (when (string= name "time-pos")
           (let ((new-time (or value data)))
             (when (numberp new-time)
               (setf (rmpv-session-last-time rmpv-session) new-time)))))
        ("idle"
         (rmpv--debug "idle event, cleaning up")
         (rmpv--cleanup))
        ("end-file"
         (rmpv--debug "end-file event")
         (rmpv--cleanup))
        ("file-loaded"
         (setf (rmpv-session-last-time rmpv-session) 0.0
               (rmpv-session-subtitle-last-index rmpv-session) -1))
        (_
         (when (and reply-id (listp data))
           (when-let ((cb (seq-some (lambda (c)
                                      (when (= (rmpv--callback-id c) reply-id) c))
                                    (rmpv-session-pending-callbacks rmpv-session))))
             (funcall (rmpv--callback-fn cb) (alist-get 'error msg) data)
             (setf (rmpv-session-pending-callbacks rmpv-session)
                   (seq-remove (lambda (c) (= (rmpv--callback-id c) reply-id))
                               (rmpv-session-pending-callbacks rmpv-session)))))))
      (when (and (listp data) (string= (alist-get 'type data) "time"))
        (setf (rmpv-session-last-time rmpv-session) (alist-get 'value data))))))

(defun rmpv--cancel-subtitle-timer ()
  "Cancel subtitle timer if running."
  (when (and rmpv-session (rmpv-session-subtitle-timer rmpv-session))
    (cancel-timer (rmpv-session-subtitle-timer rmpv-session))
    (setf (rmpv-session-subtitle-timer rmpv-session) nil)))

(defun rmpv--close-socket ()
  "Close socket connection."
  (when (and rmpv-session (rmpv-session-socket-proc rmpv-session))
    (delete-process (rmpv-session-socket-proc rmpv-session))
    (setf (rmpv-session-socket-proc rmpv-session) nil)))

(defun rmpv--cleanup ()
  "Clean up all resources."
  (rmpv--cancel-subtitle-timer)
  (rmpv--close-socket)
  (when rmpv-session
    (setf (rmpv-session-process rmpv-session) nil
          (rmpv-session-last-time rmpv-session) -1.0
          (rmpv-session-stopping rmpv-session) nil
          (rmpv-session-subtitle-frame rmpv-session) nil
          (rmpv-session-subtitle-entries rmpv-session) nil
          (rmpv-session-subtitle-last-index rmpv-session) -1))
  (setq rmpv-session nil
        rmpv-mode nil))

(defun rmpv--sentinel (_proc status)
  (message "rmpv: %s" status)
  (when (string-match "closed\\|exited\\|terminated" status)
    (rmpv--cleanup)))

(defun rmpv--connect ()
  (when (and rmpv-session
             (rmpv-session-socket-path rmpv-session)
             (file-exists-p (rmpv-session-socket-path rmpv-session)))
    (condition-case-unless-debug _
        (progn
          (setf (rmpv-session-socket-proc rmpv-session)
                (make-network-process
                 :name "rmpv-ipc"
                 :buffer (rmpv-session-socket-buffer rmpv-session)
                 :family 'local
                 :service (rmpv-session-socket-path rmpv-session)
                 :filter #'rmpv--filter
                 :sentinel #'rmpv--sentinel))
          (rmpv--property "time-pos" :observe 1))
      (error
       (run-with-timer 0.2 nil #'rmpv--connect)))))

;;;###autoload
(defun rmpv-play (path)
  "Play video file PATH."
  (interactive "fVideo file: ")
  (rmpv--cleanup)
  (unwind-protect
      (progn
        (setq rmpv-session
              (rmpv-session--create
               :socket-path (format "/tmp/mpv-ipc-%s.sock" (emacs-pid))))
        (when (file-exists-p (rmpv-session-socket-path rmpv-session))
          (delete-file (rmpv-session-socket-path rmpv-session)))
        (setf (rmpv-session-process rmpv-session)
              (make-process
               :name "rmpv"
               :buffer (generate-new-buffer " *rmpv*")
               :command (list rmpv-mpv-path
                              "--no-terminal"
                              "--osc=no"
                              "--osd-bar=no"
                              "--sub-auto=no"
                              (format "--input-ipc-server=%s" (rmpv-session-socket-path rmpv-session))
                              (expand-file-name path))
               :sentinel (lambda (_p s)
                           (message "rmpv: mpv %s" s)
                           (when (string-match "exited\\|terminated\\|closed" s)
                             (rmpv--cleanup)))))
        (run-with-timer 0.1 nil #'rmpv--connect)
        (rmpv-mode 1))
    (unless rmpv-session
      (when-let ((sock (format "/tmp/mpv-ipc-%s.sock" (emacs-pid))))
        (when (file-exists-p sock)
          (ignore-errors (delete-file sock)))))))

;;;###autoload
(defun rmpv-pause ()
  "Pause playback."
  (interactive)
  (rmpv--cmd '("set" "pause" "yes")))

;;;###autoload
(defun rmpv-resume ()
  "Resume playback."
  (interactive)
  (rmpv--cmd '("set" "pause" "no")))

;;;###autoload
(defun rmpv-stop ()
  "Stop playback."
  (interactive)
  (when rmpv-session
    (unless (rmpv-session-stopping rmpv-session)
      (setf (rmpv-session-stopping rmpv-session) t)
      (rmpv--cmd '("quit"))
      (sleep-for 0.1)
      (rmpv--cleanup)
      (when (and (rmpv-session-socket-path rmpv-session)
                 (file-exists-p (rmpv-session-socket-path rmpv-session)))
        (delete-file (rmpv-session-socket-path rmpv-session))))))

(defun rmpv-get-time (&optional callback)
  "Get current time. If CALLBACK is provided, call it with the time value."
  (when rmpv-session
    (if callback
        (rmpv--property "time-pos" :callback callback)
      (let ((end (+ (float-time) 0.5)))
        (while (and (null (rmpv-session-last-time rmpv-session))
                    (< (float-time) end)))
        (rmpv-session-last-time rmpv-session)))))

;;;###autoload
(defun rmpv-seek-forward (&optional seconds)
  "Seek forward by SECONDS seconds (default 10)."
  (interactive "p")
  (rmpv--cmd (list "seek" (or seconds 10) "relative")))

;;;###autoload
(defun rmpv-seek-backward (&optional seconds)
  "Seek backward by SECONDS seconds (default 10)."
  (interactive "p")
  (rmpv--cmd (list "seek" (or seconds 10) "relative")))

;;;###autoload
(defun rmpv-load-subtitle (path)
  "Load subtitle into video."
  (interactive "fSubtitle file: ")
  (rmpv--cmd (list "sub-add" (expand-file-name path))))

(define-derived-mode rmpv-subtitle-mode special-mode "Subtitle"
  "Major mode for displaying subtitles."
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(defun rmpv-parse-timestamp (ts)
  "Parse SRT timestamp like '00:00:01,500' to seconds as float."
  (when ts
    (let ((cleaned (replace-regexp-in-string "[ \t\r\n]+" "" ts)))
      (if (string-match (concat "\\`\\([0-9]+\\):\\([0-9]+\\):"
                                "\\([0-9]+\\)\\([,.]\\)\\([0-9]+\\)\\'") cleaned)
          (+ (* 3600 (string-to-number (match-string 1 cleaned)))
             (* 60 (string-to-number (match-string 2 cleaned)))
             (string-to-number (match-string 3 cleaned))
             (/ (string-to-number (match-string 5 cleaned)) 1000.0))
        nil))))

(defun rmpv-parse-srt-from-string (content)
  "Parse raw SRT string CONTENT into a list of subtitle entries."
  (let ((normalized (replace-regexp-in-string "\r\n?" "\n" content)))
    (seq-into
     (seq-filter #'identity
                 (seq-map (lambda (block)
                            (when (string-match (concat "\\`[0-9]+[ \t]*\n"
                                                       "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)"
                                                       "[ \t]+-->?[ \t]+"
                                                       "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)"
                                                       "\\(?:[ \t]*\n\\([^\0]*\\)\\)?\\'") block)
                              (let* ((start-str (match-string 1 block))
                                     (end-str (match-string 2 block))
                                     (text (match-string 3 block))
                                     (start (or (rmpv-parse-timestamp start-str) 0.0))
                                     (end (or (rmpv-parse-timestamp end-str) start))
                                     (text-clean (if text
                                                     (string-trim text)
                                                   "")))
                                (rmpv--subtitle-create :start start :end end :text text-clean))))
                           (split-string normalized "\n\n" t)))
     'list)))

(defun rmpv-load-subtitle-file (srt-path)
  "Load SRT file from SRT-PATH and display in subtitle frame."
  (when rmpv-session
    (let* ((raw-entries (with-temp-buffer
                          (insert-file-contents srt-path)
                          (rmpv-parse-srt-from-string (buffer-string))))
           (entries (seq-filter #'identity raw-entries))
           (buf-name " *rmpv Subtitle*"))
      (rmpv--debug "loaded %d subtitle entries" (length entries))
      (setf (rmpv-session-subtitle-entries rmpv-session) entries
            (rmpv-session-subtitle-last-index rmpv-session) -1
            (rmpv-session-subtitle-current-index rmpv-session) nil)
      (when (and (rmpv-session-subtitle-frame rmpv-session)
                 (frame-live-p (rmpv-session-subtitle-frame rmpv-session)))
        (delete-frame (rmpv-session-subtitle-frame rmpv-session)))
      (let ((buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (rmpv-subtitle-mode)
          (dolist (entry entries)
            (insert (format "[%05.1f] %s\n"
                            (rmpv--subtitle-start entry)
                            (rmpv--subtitle-text entry))))
          (setq buffer-read-only t))
        (setf (rmpv-session-subtitle-frame rmpv-session)
              (make-frame '((name . "rmpv Subtitle")
                            (minibuffer . nil)
                            (auto-raise . t)
                            (frame-resize-pixelwise . t))))
        (select-frame (rmpv-session-subtitle-frame rmpv-session))
        (switch-to-buffer buf))
      (rmpv-session-subtitle-frame rmpv-session))))

(defun rmpv-find-subtitle-at-time (time)
  "Find subtitle entry at TIME (in seconds)."
  (when (and rmpv-session (rmpv-session-subtitle-entries rmpv-session))
    (seq-find (lambda (entry)
                (and (numberp (rmpv--subtitle-start entry))
                     (numberp (rmpv--subtitle-end entry))
                     (<= (rmpv--subtitle-start entry) time)
                     (< time (rmpv--subtitle-end entry))))
              (rmpv-session-subtitle-entries rmpv-session))))

(defun rmpv-get-subtitle-index (time)
  "Get the index of subtitle entry at TIME (in seconds)."
  (when rmpv-session
    (seq-position (rmpv-session-subtitle-entries rmpv-session)
                  (rmpv-find-subtitle-at-time time)
                  #'rmpv--subtitle-equal)))

(defun rmpv--subtitle-equal (a b)
  "Compare two subtitles for equality."
  (and (equal (rmpv--subtitle-start a) (rmpv--subtitle-start b))
       (equal (rmpv--subtitle-end a) (rmpv--subtitle-end b))
       (equal (rmpv--subtitle-text a) (rmpv--subtitle-text b))))

(defun rmpv-update-subtitle-highlight ()
  "Update subtitle highlight based on current playback time."
  (when rmpv-session
    (let ((time (rmpv-session-last-time rmpv-session))
          (entries (rmpv-session-subtitle-entries rmpv-session))
          (entries-len (length (or (rmpv-session-subtitle-entries rmpv-session) [])))
          (frame (rmpv-session-subtitle-frame rmpv-session))
          (frame-valid (and (rmpv-session-subtitle-frame rmpv-session)
                            (frame-live-p (rmpv-session-subtitle-frame rmpv-session)))))
      (when (and (numberp time) (>= time 0)
                 entries-len frame-valid)
        (if-let* ((entry (rmpv-find-subtitle-at-time time))
                  (idx (seq-position entries entry #'rmpv--subtitle-equal))
                  ((/= idx (rmpv-session-subtitle-last-index rmpv-session))))
            (progn
              (rmpv--debug "highlight: idx=%s text=%S" idx (rmpv--subtitle-text entry))
              (setf (rmpv-session-subtitle-last-index rmpv-session) idx)
              (with-current-buffer (window-buffer (frame-first-window frame))
                (remove-overlays (point-min) (point-max) 'rmpv-subtitle-highlight t)
                (goto-char (point-min))
                (forward-line idx)
                (let ((ov (make-overlay (point) (line-end-position))))
                  (overlay-put ov 'rmpv-subtitle-highlight t)
                  (overlay-put ov 'face 'rmpv-subtitle-highlight-face)
                  (setf (rmpv-session-subtitle-current-index rmpv-session) (point))))))))))

(defun rmpv-subtitle-auto-update ()
  "Start auto-update timer for subtitle highlight.
Cancels any existing timer before starting a new one."
  (rmpv--with-session
   (rmpv--cancel-subtitle-timer)
   (setf (rmpv-session-subtitle-timer rmpv-session)
         (run-with-timer 0 rmpv-subtitle-update-interval
                         #'rmpv-update-subtitle-highlight))))

(defun rmpv-subtitle-stop-update ()
  "Stop auto-update timer and clean up frame."
  (rmpv--with-session
   (rmpv--cancel-subtitle-timer)
   (when-let ((frame (rmpv-session-subtitle-frame rmpv-session)))
     (when (frame-live-p frame)
       (delete-frame frame)))
   (setf (rmpv-session-subtitle-frame rmpv-session) nil
         (rmpv-session-subtitle-entries rmpv-session) nil
         (rmpv-session-subtitle-last-index rmpv-session) -1)))

;;;###autoload
(defun rmpv-play-with-subtitle (video-path &optional external-only)
  "Play VIDEO-PATH and load corresponding subtitle if exists.
With prefix arg EXTERNAL-ONLY, only display subtitle in frame."
  (interactive "fVideo file: \np")
  (rmpv-play video-path)
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
