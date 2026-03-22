;;; empv.el --- Modern mpv media player plugin for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Your Name

;; Author: Your Name <your@email.com>
;; Maintainer: Your Name <your@email.com>
;; URL: https://github.com/your/empv.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
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

;; empv.el provides modern mpv integration for Emacs with support for:
;; - Audio/video playback with mpv
;; - Playlist management
;; - Subtitle synchronization
;; - Overlay OSD display

;;; Code:

(eval-when-compile
  (require 'seq)
  (require 'subr-x))

(defgroup empv nil
  "Modern mpv media player interface."
  :group 'multimedia
  :link '(url-link "https://github.com/your/empv.el"))

(defcustom empv-mpv-path
  (or (executable-find "mpv") "mpv")
  "Path to the mpv executable."
  :type 'file
  :group 'empv)

(defcustom empv-ipc-socket "~/.cache/empv.sock"
  "Socket path for mpv IPC communication."
  :type 'string
  :group 'empv)

(defcustom empv-startup-args
  '("--idle=yes" "--input-ipc-server=%S")
  "Arguments passed to mpv on startup."
  :type '(repeat string)
  :group 'empv)

(defvar empv--process nil)
(defvar empv--socket-file nil)
(defvar empv--current-file nil)
(defvar empv--playlist nil)
(defvar empv--status 'stopped)
(defvar empv--volume 100)
(defvar empv--position 0)
(defvar empv--duration 0)

(cl-defstruct (empv-playlist-item (:constructor empv-playlist-item--create))
  (file nil :type string)
  (title nil :type (or null string))
  (duration nil :type (or null number)))

(defun empv--ensure-process ()
  "Ensure mpv process is running."
  (unless (and empv--process (process-live-p empv--process))
    (let ((socket (expand-file-name empv--socket-file)))
      (make-directory (file-name-directory socket) t)
      (when (file-exists-p socket)
        (delete-file socket))
      (let ((args (seq-map
                   (lambda (arg)
                     (if (string-search "%S" arg)
                         (replace-regexp-in-string "%S" (shell-quote-argument socket) arg)
                       arg))
                   empv-startup-args)))
        (setq empv--process
              (make-process
               :name "empv-mpv"
               :command (cons empv-mpv-path args)
               :filter #'empv--process-filter
               :sentinel #'empv--process-sentinel))
        (setq empv--socket-file socket)))))

(defun empv--process-filter (process output)
  "Handle mpv process OUTPUT."
  (when (string-match "event: log" output)
    (message "[empv] %s" (string-trim output))))

(defun empv--process-sentinel (process event)
  "Handle mpv process EVENT."
  (pcase event
    ("finished\n" (setq empv--status 'stopped))
    ("killed\n" (setq empv--status 'stopped))
    (_ (message "[empv] Process event: %s" (string-trim event)))))

(defun empv--send-command (&rest args)
  "Send command ARGS to mpv via IPC."
  (empv--ensure-process)
  (let* ((json (json-serialize `((command . ,(vconcat args)))))
         (proc (open-network-stream "empv-ipc" nil
                                    (expand-file-name empv--socket-file)
                                    )))
    (process-send-string proc (format "%s\n" json))
    (sleep-for 0.1)
    (delete-process proc)))

(defun empv-play (file)
  "Play FILE."
  (interactive "fMedia file: ")
  (setq empv--current-file file
        empv--status 'playing)
  (empv--send-command "loadfile" file "replace")
  (message "[empv] Playing: %s" (file-name-nondirectory file)))

(defun empv-toggle-pause ()
  "Toggle playback pause state."
  (interactive)
  (empv--send-command "cycle" "pause"))

(defun empv-seek (seconds)
  "Seek by SECONDS."
  (interactive "nSeconds: ")
  (empv--send-command "seek" seconds))

(defun empv-set-volume (volume)
  "Set volume to VOLUME (0-100)."
  (interactive "nVolume: ")
  (empv--send-command "set" "volume" volume)
  (setq empv--volume volume))

(defun empv-quit ()
  "Quit mpv process."
  (interactive)
  (when empv--process
    (empv--send-command "quit")
    (sleep-for 0.1)
    (delete-process empv--process)
    (setq empv--process nil
          empv--status 'stopped)))

(defun empv-rotate-subtitle (offset)
  "Rotate subtitle by OFFSET lines."
  (interactive "nOffset: ")
  (empv--send-command "add" "sub-delay" offset))

(defun empv-rotate-audio (offset)
  "Rotate audio track by OFFSET."
  (interactive "p")
  (empv--send-command "add" "audio-delay" offset))

;;;###autoload
(define-minor-mode empv-mode
  "Minor mode for empv media controls."
  :lighter " empv"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") #'empv-toggle-pause)
            (define-key map (kbd "M-<right>") (lambda () (interactive) (empv-seek 10)))
            (define-key map (kbd "M-<left>") (lambda () (interactive) (empv-seek -10)))
            (define-key map (kbd "M-<up>") (lambda () (interactive) (empv-set-volume (min 100 (+ empv--volume 10)))))
            (define-key map (kbd "M-<down>") (lambda () (interactive) (empv-set-volume (max 0 (- empv--volume 10)))))
            (define-key map (kbd "[") #'empv-rotate-subtitle)
            map))

(provide 'empv)

;;; empv.el ends here
