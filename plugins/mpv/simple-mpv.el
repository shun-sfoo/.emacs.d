;;; simple-mpv.el --- Simple mpv player for Emacs  -*- lexical-binding: t; -*-

;; Usage:
;;   (require 'simple-mpv)
;;   (simple-mpv-play "/path/to/video.mp4")

(require 'json)

(defgroup simple-mpv nil
  "Simple mpv player interface."
  :group 'multimedia)

(defcustom simple-mpv-mpv-path
  (or (executable-find "mpv") "mpv")
  "Path to mpv executable."
  :type 'file
  :group 'simple-mpv)

(defcustom simple-mpv-socket-file
  (format "/tmp/simple-mpv-%s.sock" (emacs-pid))
  "Socket file path."
  :type 'file
  :group 'simple-mpv)

;;; Session state

(defvar simple-mpv--proc nil "mpv process.")
(defvar simple-mpv--socket nil "Socket connection.")
(defvar simple-mpv--time -1.0 "Current playback time.")
(defvar simple-mpv--stopping nil "Flag for stop operation.")

;;; Core functions

(defun simple-mpv--cleanup ()
  "Clean up all resources."
  (setq simple-mpv--time -1.0
        simple-mpv--stopping nil)
  (when simple-mpv--socket
    (delete-process simple-mpv--socket)
    (setq simple-mpv--socket nil))
  (when simple-mpv--proc
    (delete-process simple-mpv--proc)
    (setq simple-mpv--proc nil)))

(defun simple-mpv--send (cmd)
  "Send CMD list to mpv."
  (when (and simple-mpv--socket (process-live-p simple-mpv--socket))
    (process-send-string simple-mpv--socket
                         (concat (json-encode `((command . ,cmd))) "\n"))))

(defun simple-mpv--handle-output (output)
  "Handle mpv OUTPUT."
  (with-current-buffer (process-buffer simple-mpv--socket)
    (insert output)
    (goto-char (point-min))
    (while (re-search-forward "{.*?}\n?" nil t)
      (ignore-errors
        (let ((msg (json-parse-string (match-string 0) :object-type 'alist)))
          (when (string= (alist-get 'event msg) "property-change")
            (let ((time (alist-get 'data msg)))
              (when (numberp time)
                (setq simple-mpv--time time)))))))))

(defun simple-mpv--connect ()
  "Connect to mpv socket."
  (when (file-exists-p simple-mpv-socket-file)
    (condition-case _
        (setq simple-mpv--socket
              (make-network-process
               :name "simple-mpv-socket"
               :buffer " *simple-mpv*"
               :family 'local
               :service simple-mpv-socket-file
               :filter #'simple-mpv--handle-output
               :sentinel (lambda (_proc status)
                           (message "simple-mpv: %s" status)
                           (when (string-match "closed" status)
                             (simple-mpv--cleanup)))))
      (error (run-with-timer 0.2 nil #'simple-mpv--connect)))))

;;; Public API

;;;###autoload
(defun simple-mpv-play (file)
  "Play FILE."
  (interactive "fVideo file: ")
  (simple-mpv--cleanup)
  (when (file-exists-p simple-mpv-socket-file)
    (delete-file simple-mpv-socket-file))
  (setq simple-mpv--proc
        (make-process
         :name "simple-mpv"
         :command (list simple-mpv-mpv-path
                        "--no-terminal"
                        "--input-ipc-server" simple-mpv-socket-file
                        (expand-file-name file))
         :sentinel (lambda (_proc status)
                     (message "simple-mpv: mpv %s" (string-trim status))
                     (simple-mpv--cleanup))))
  (run-with-timer 0.1 nil #'simple-mpv--connect))

;;;###autoload
(defun simple-mpv-pause ()
  "Pause playback."
  (interactive)
  (simple-mpv--send '("set" "pause" "yes")))

;;;###autoload
(defun simple-mpv-resume ()
  "Resume playback."
  (interactive)
  (simple-mpv--send '("set" "pause" "no")))

;;;###autoload
(defun simple-mpv-toggle-pause ()
  "Toggle pause."
  (interactive)
  (simple-mpv--send '("cycle" "pause")))

;;;###autoload
(defun simple-mpv-stop ()
  "Stop playback."
  (interactive)
  (unless simple-mpv--stopping
    (setq simple-mpv--stopping t)
    (simple-mpv--send '("quit"))
    (sleep-for 0.1)
    (simple-mpv--cleanup)))

;;;###autoload
(defun simple-mpv-seek (seconds)
  "Seek by SECONDS."
  (interactive "nSeconds: ")
  (simple-mpv--send (list "seek" seconds "relative")))

;;;###autoload
(defun simple-mpv-seek-forward ()
  "Seek forward 10 seconds."
  (interactive)
  (simple-mpv-seek 10))

;;;###autoload
(defun simple-mpv-seek-backward ()
  "Seek backward 10 seconds."
  (interactive)
  (simple-mpv-seek -10))

;;;###autoload
(defun simple-mpv-volume (level)
  "Set volume to LEVEL (0-100)."
  (interactive "nVolume (0-100): ")
  (simple-mpv--send (list "set" "volume" level)))

;;;###autoload
(defun simple-mpv-load-subtitle (file)
  "Load subtitle FILE."
  (interactive "fSubtitle file: ")
  (simple-mpv--send (list "sub-add" (expand-file-name file))))

;;;###autoload
(defun simple-mpv-get-time ()
  "Get current time."
  (if (>= simple-mpv--time 0)
      simple-mpv--time
    (message "Not playing")))

(provide 'simple-mpv)

;;; simple-mpv.el ends here
