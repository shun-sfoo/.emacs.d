;;; mmpv.el --- Modern mpv player for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Neo

;; Author: Neo <neo@example.com>
;; Maintainer: Neo <neo@example.com>
;; URL: https://github.com/neo/mmpv.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (seq "3.0"))
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

;; mmpv.el provides modern mpv integration for Emacs via Unix socket IPC.
;;
;; Features:
;; - Video/audio playback control
;; - Subtitle loading and frame display
;; - Real-time subtitle highlighting
;;
;; Usage:
;;   (require 'mmpv)
;;   (mmpv-play "/path/to/video.mp4")
;;   (mmpv-play-with-subtitle "/path/to/video.mp4")

;;; Code:

(eval-when-compile
  (require 'seq)
  (require 'subr-x))

(require 'json)
(require 'cl-lib)

(define-error 'mmpv-error "mmpv error")
(define-error 'mmpv-connect-failed "Connection failed" 'mmpv-error)
(define-error 'mmpv-timeout "Operation timeout" 'mmpv-error)

(defgroup mmpv nil
  "Modern mpv player interface for Emacs."
  :group 'multimedia
  :link '(url-link "https://github.com/neo/mmpv.el"))

(defcustom mmpv-mpv-path
  (or (executable-find "mpv") "mpv")
  "Path to the mpv executable."
  :type 'file
  :group 'mmpv)

(defcustom mmpv-socket-directory
  (format "/tmp/mmpv-%s.sock" (emacs-pid))
  "Path for mmpv IPC socket."
  :type 'directory
  :group 'mmpv)

(defcustom mmpv-subtitle-update-interval
  0.1
  "Interval in seconds for subtitle highlight updates."
  :type 'float
  :group 'mmpv)

(defface mmpv-subtitle-highlight-face
  '((t (:background "dark slate gray" :foreground "white")))
  "Face for subtitle highlight."
  :group 'mmpv)

(cl-defstruct (mmpv--subtitle (:constructor mmpv--subtitle-create))
  (start 0.0 :type float)
  (end 0.0 :type float)
  (text "" :type string))

(cl-defstruct (mmpv--callback (:constructor mmpv--callback-create))
  (id 0 :type integer)
  (fn nil :type (or null function)))

(cl-defstruct (mmpv-session (:constructor mmpv-session--create))
  "Session structure for mpv playback state."
  (process nil :type (or null process))
  (socket-proc nil :type (or null process))
  (socket-buffer " *mmpv-ipc*" :type string)
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

(defvar mmpv-session nil "Current mmpv session.")

(defcustom mmpv-debug nil
  "Enable debug logging for mmpv."
  :type 'boolean
  :group 'mmpv)

(defmacro mmpv--debug (&rest args)
  "Print debug message when `mmpv-debug' is non-nil."
  (declare (indent 1))
  `(when mmpv-debug
     (message "[mmpv] %s" (format ,@args))))

(defmacro mmpv--with-session (&rest body)
  "Execute BODY only when mmpv-session is active."
  (declare (indent 0))
  `(when mmpv-session ,@body))

(defun mmpv--next-id ()
  "Get next command ID."
  (when mmpv-session
    (setf (mmpv-session-command-id mmpv-session)
          (1+ (mmpv-session-command-id mmpv-session)))
    (mmpv-session-command-id mmpv-session)))

(cl-defun mmpv--cmd (command &key callback)
  "Send COMMAND to mpv.
COMMAND is a list representing the mpv command.
If CALLBACK is provided, it will be called with (error data)."
  (when (and mmpv-session
             (mmpv-session-socket-proc mmpv-session)
             (process-live-p (mmpv-session-socket-proc mmpv-session)))
    (let ((req-id (mmpv--next-id)))
      (when callback
        (push (mmpv--callback-create :id req-id :fn callback)
              (mmpv-session-pending-callbacks mmpv-session)))
      (process-send-string (mmpv-session-socket-proc mmpv-session)
                           (concat (json-encode `((command . ,command)
                                                  (request_id . ,req-id)))
                                   "\n")))))

(cl-defun mmpv--property (name &key observe callback)
  "Get or observe property NAME.
If OBSERVE is non-nil, start observing the property.
If CALLBACK is provided, call it with the property value."
  (mmpv--cmd (if observe
                 (list "observe_property" (or observe 1) name)
               (list "get_property" name))
             :callback callback))

(defvar-keymap mmpv-mode-map
  :doc "Keymap for mmpv minor mode."
  "SPC" #'mmpv-pause
  "s" #'mmpv-stop
  "f" #'mmpv-seek-forward
  "b" #'mmpv-seek-backward)

(define-minor-mode mmpv-mode
  "Minor mode for mmpv playback controls."
  :lighter " mmpv"
  :keymap mmpv-mode-map
  :group 'mmpv
  (if mmpv-mode
      (mmpv--mode-on)
    (mmpv--mode-off)))

(defun mmpv--mode-on ()
  "Enable mmpv mode."
  (mmpv--debug "mode enabled"))

(defun mmpv--mode-off ()
  "Disable mmpv mode."
  (mmpv--debug "mode disabled"))

(defun mmpv--filter (proc output)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output)
    (mmpv--process-output)))

(defun mmpv--process-output ()
  (goto-char (point-min))
  (while (re-search-forward "{[^}]+}\n?" nil t)
    (when-let* ((json-str (match-string 0))
                (json (ignore-errors (json-parse-string json-str :object-type 'alist))))
      (mmpv--handle-message json))))

(defun mmpv--handle-message (msg)
  (when mmpv-session
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
               (setf (mmpv-session-last-time mmpv-session) new-time)))))
        ("idle"
         (mmpv--debug "idle event, cleaning up")
         (mmpv--cleanup))
        ("end-file"
         (mmpv--debug "end-file event")
         (mmpv--cleanup))
        ("file-loaded"
         (setf (mmpv-session-last-time mmpv-session) 0.0
               (mmpv-session-subtitle-last-index mmpv-session) -1))
        (_
         (when (and reply-id (listp data))
           (when-let ((cb (seq-some (lambda (c)
                                      (when (= (mmpv--callback-id c) reply-id) c))
                                    (mmpv-session-pending-callbacks mmpv-session))))
             (funcall (mmpv--callback-fn cb) (alist-get 'error msg) data)
             (setf (mmpv-session-pending-callbacks mmpv-session)
                   (seq-remove (lambda (c) (= (mmpv--callback-id c) reply-id))
                               (mmpv-session-pending-callbacks mmpv-session)))))))
      (when (and (listp data) (string= (alist-get 'type data) "time"))
        (setf (mmpv-session-last-time mmpv-session) (alist-get 'value data))))))

(defun mmpv--cancel-subtitle-timer ()
  "Cancel subtitle timer if running."
  (when (and mmpv-session (mmpv-session-subtitle-timer mmpv-session))
    (cancel-timer (mmpv-session-subtitle-timer mmpv-session))
    (setf (mmpv-session-subtitle-timer mmpv-session) nil)))

(defun mmpv--close-socket ()
  "Close socket connection."
  (when (and mmpv-session (mmpv-session-socket-proc mmpv-session))
    (delete-process (mmpv-session-socket-proc mmpv-session))
    (setf (mmpv-session-socket-proc mmpv-session) nil)))

(defun mmpv--cleanup ()
  "Clean up all resources."
  (mmpv--cancel-subtitle-timer)
  (mmpv--close-socket)
  (when mmpv-session
    (setf (mmpv-session-process mmpv-session) nil
          (mmpv-session-last-time mmpv-session) -1.0
          (mmpv-session-stopping mmpv-session) nil
          (mmpv-session-subtitle-frame mmpv-session) nil
          (mmpv-session-subtitle-entries mmpv-session) nil
          (mmpv-session-subtitle-last-index mmpv-session) -1))
  (setq mmpv-session nil
        mmpv-mode nil))

(defun mmpv--sentinel (_proc status)
  (message "mmpv: %s" status)
  (when (string-match "closed\\|exited\\|terminated" status)
    (mmpv--cleanup)))

(defun mmpv--connect ()
  (when (and mmpv-session
             (mmpv-session-socket-path mmpv-session)
             (file-exists-p (mmpv-session-socket-path mmpv-session)))
    (condition-case-unless-debug _
        (progn
          (setf (mmpv-session-socket-proc mmpv-session)
                (make-network-process
                 :name "mmpv-ipc"
                 :buffer (mmpv-session-socket-buffer mmpv-session)
                 :family 'local
                 :service (mmpv-session-socket-path mmpv-session)
                 :filter #'mmpv--filter
                 :sentinel #'mmpv--sentinel))
          (mmpv--property "time-pos" :observe 1))
      (error
       (run-with-timer 0.2 nil #'mmpv--connect)))))

;;;###autoload
(defun mmpv-play (path)
  "Play video file PATH."
  (interactive "fVideo file: ")
  (mmpv--cleanup)
  (unwind-protect
      (progn
        (setq mmpv-session
              (mmpv-session--create
               :socket-path (format "%s/mmpv-%s.sock" mmpv-socket-directory (emacs-pid))))
        (make-directory mmpv-socket-directory t)
        (when (file-exists-p (mmpv-session-socket-path mmpv-session))
          (delete-file (mmpv-session-socket-path mmpv-session)))
        (setf (mmpv-session-process mmpv-session)
              (make-process
               :name "mmpv"
               :buffer (generate-new-buffer " *mmpv*")
               :command (list mmpv-mpv-path
                              "--no-terminal"
                              "--osc=no"
                              "--osd-bar=no"
                              "--sub-auto=no"
                              (format "--input-ipc-server=%s" (mmpv-session-socket-path mmpv-session))
                              (expand-file-name path))
               :sentinel (lambda (_p s)
                            (message "mmpv: mpv %s" s)
                            (when (string-match "exited\\|terminated\\|closed" s)
                              (mmpv--cleanup)))))
        (run-with-timer 0.1 nil #'mmpv--connect)
        (mmpv-mode 1))
    (unless mmpv-session
      (when-let ((sock (mmpv-session-socket-path (mmpv-session--create :socket-path (format "%s/mmpv-%s.sock" mmpv-socket-directory (emacs-pid)))))
                 (file-exists-p sock))
        (ignore-errors (delete-file sock))))))

;;;###autoload
(defun mmpv-pause ()
  "Pause playback."
  (interactive)
  (mmpv--cmd '("set" "pause" "yes")))

;;;###autoload
(defun mmpv-resume ()
  "Resume playback."
  (interactive)
  (mmpv--cmd '("set" "pause" "no")))

;;;###autoload
(defun mmpv-stop ()
  "Stop playback."
  (interactive)
  (when mmpv-session
    (unless (mmpv-session-stopping mmpv-session)
      (setf (mmpv-session-stopping mmpv-session) t)
      (mmpv--cmd '("quit"))
      (sleep-for 0.1)
      (mmpv--cleanup)
      (when (and (mmpv-session-socket-path mmpv-session)
                 (file-exists-p (mmpv-session-socket-path mmpv-session)))
        (delete-file (mmpv-session-socket-path mmpv-session))))))

(defun mmpv-get-time (&optional callback)
  "Get current time. If CALLBACK is provided, call it with the time value."
  (when mmpv-session
    (if callback
        (mmpv--property "time-pos" :callback callback)
      (let ((end (+ (float-time) 0.5)))
        (while (and (null (mmpv-session-last-time mmpv-session))
                    (< (float-time) end)))
        (mmpv-session-last-time mmpv-session)))))

;;;###autoload
(defun mmpv-seek-forward (&optional seconds)
  "Seek forward by SECONDS seconds (default 10)."
  (interactive "p")
  (mmpv--cmd (list "seek" (or seconds 10) "relative")))

;;;###autoload
(defun mmpv-seek-backward (&optional seconds)
  "Seek backward by SECONDS seconds (default 10)."
  (interactive "p")
  (mmpv--cmd (list "seek" (or seconds 10) "relative")))

;;;###autoload
(defun mmpv-load-subtitle (path)
  "Load subtitle into video."
  (interactive "fSubtitle file: ")
  (mmpv--cmd (list "sub-add" (expand-file-name path))))

(define-derived-mode mmpv-subtitle-mode special-mode "Subtitle"
  "Major mode for displaying subtitles."
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(defun mmpv-parse-timestamp (ts)
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

(defun mmpv-parse-srt-from-string (content)
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
                                     (start (or (mmpv-parse-timestamp start-str) 0.0))
                                     (end (or (mmpv-parse-timestamp end-str) start))
                                     (text-clean (if text
                                                     (string-trim text)
                                                   "")))
                                (mmpv--subtitle-create :start start :end end :text text-clean))))
                           (split-string normalized "\n\n" t)))
     'list)))

(defun mmpv-load-subtitle-file (srt-path)
  "Load SRT file from SRT-PATH and display in subtitle frame."
  (when mmpv-session
    (let* ((raw-entries (with-temp-buffer
                          (insert-file-contents srt-path)
                          (mmpv-parse-srt-from-string (buffer-string))))
           (entries (seq-filter #'identity raw-entries))
           (buf-name " *mmpv Subtitle*"))
      (mmpv--debug "loaded %d subtitle entries" (length entries))
      (setf (mmpv-session-subtitle-entries mmpv-session) entries
            (mmpv-session-subtitle-last-index mmpv-session) -1
            (mmpv-session-subtitle-current-index mmpv-session) nil)
      (when (and (mmpv-session-subtitle-frame mmpv-session)
                 (frame-live-p (mmpv-session-subtitle-frame mmpv-session)))
        (delete-frame (mmpv-session-subtitle-frame mmpv-session)))
      (let ((buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (mmpv-subtitle-mode)
          (dolist (entry entries)
            (insert (format "[%05.1f] %s\n"
                            (mmpv--subtitle-start entry)
                            (mmpv--subtitle-text entry))))
          (setq buffer-read-only t))
        (setf (mmpv-session-subtitle-frame mmpv-session)
              (make-frame '((name . "mmpv Subtitle")
                            (minibuffer . nil)
                            (auto-raise . t)
                            (frame-resize-pixelwise . t))))
        (select-frame (mmpv-session-subtitle-frame mmpv-session))
        (switch-to-buffer buf))
      (mmpv-session-subtitle-frame mmpv-session))))

(defun mmpv-find-subtitle-at-time (time)
  "Find subtitle entry at TIME (in seconds)."
  (when (and mmpv-session (mmpv-session-subtitle-entries mmpv-session))
    (seq-find (lambda (entry)
                (and (numberp (mmpv--subtitle-start entry))
                     (numberp (mmpv--subtitle-end entry))
                     (<= (mmpv--subtitle-start entry) time)
                     (< time (mmpv--subtitle-end entry))))
              (mmpv-session-subtitle-entries mmpv-session))))

(defun mmpv-get-subtitle-index (time)
  "Get the index of subtitle entry at TIME (in seconds)."
  (when mmpv-session
    (seq-position (mmpv-session-subtitle-entries mmpv-session)
                  (mmpv-find-subtitle-at-time time)
                  #'mmpv--subtitle-equal)))

(defun mmpv--subtitle-equal (a b)
  "Compare two subtitles for equality."
  (and (equal (mmpv--subtitle-start a) (mmpv--subtitle-start b))
       (equal (mmpv--subtitle-end a) (mmpv--subtitle-end b))
       (equal (mmpv--subtitle-text a) (mmpv--subtitle-text b))))

(defun mmpv-update-subtitle-highlight ()
  "Update subtitle highlight based on current playback time."
  (when mmpv-session
    (let ((time (mmpv-session-last-time mmpv-session))
          (entries (mmpv-session-subtitle-entries mmpv-session))
          (entries-len (length (or (mmpv-session-subtitle-entries mmpv-session) [])))
          (frame (mmpv-session-subtitle-frame mmpv-session))
          (frame-valid (and (mmpv-session-subtitle-frame mmpv-session)
                            (frame-live-p (mmpv-session-subtitle-frame mmpv-session)))))
      (when (and (numberp time) (>= time 0)
                 entries-len frame-valid)
        (if-let* ((entry (mmpv-find-subtitle-at-time time))
                  (idx (seq-position entries entry #'mmpv--subtitle-equal))
                  ((/= idx (mmpv-session-subtitle-last-index mmpv-session))))
            (progn
              (mmpv--debug "highlight: idx=%s text=%S" idx (mmpv--subtitle-text entry))
              (setf (mmpv-session-subtitle-last-index mmpv-session) idx)
              (with-current-buffer (window-buffer (frame-first-window frame))
                (remove-overlays (point-min) (point-max) 'mmpv-subtitle-highlight t)
                (goto-char (point-min))
                (forward-line idx)
                (let ((ov (make-overlay (point) (line-end-position))))
                  (overlay-put ov 'mmpv-subtitle-highlight t)
                  (overlay-put ov 'face 'mmpv-subtitle-highlight-face)
                  (setf (mmpv-session-subtitle-current-index mmpv-session) (point))))))))))

(defun mmpv-subtitle-auto-update ()
  "Start auto-update timer for subtitle highlight."
  (mmpv--with-session
   (mmpv--cancel-subtitle-timer)
   (setf (mmpv-session-subtitle-timer mmpv-session)
         (run-with-timer 0 mmpv-subtitle-update-interval
                         #'mmpv-update-subtitle-highlight))))

(defun mmpv-subtitle-stop-update ()
  "Stop auto-update timer and clean up frame."
  (mmpv--with-session
   (mmpv--cancel-subtitle-timer)
   (when-let ((frame (mmpv-session-subtitle-frame mmpv-session)))
     (when (frame-live-p frame)
       (delete-frame frame)))
   (setf (mmpv-session-subtitle-frame mmpv-session) nil
         (mmpv-session-subtitle-entries mmpv-session) nil
         (mmpv-session-subtitle-last-index mmpv-session) -1)))

;;;###autoload
(defun mmpv-play-with-subtitle (video-path &optional external-only)
  "Play VIDEO-PATH and load corresponding subtitle if exists.
With prefix arg EXTERNAL-ONLY, only display subtitle in frame."
  (interactive "fVideo file: \np")
  (mmpv-play video-path)
  (if-let ((srt-path (concat (file-name-sans-extension video-path) ".srt"))
           ((file-exists-p srt-path)))
      (progn
        (mmpv-load-subtitle-file srt-path)
        (mmpv-subtitle-auto-update)
        (unless external-only
          (mmpv-load-subtitle srt-path)))
    (message "No subtitle file found for %s" video-path)))

(provide 'mmpv)

;;; mmpv.el ends here
