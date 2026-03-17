;;; mpv-subtitle.el --- Subtitle display and sync for mpv -*- lexical-binding: t -*-

(require 'xml)
(require 'subr-x)
(require 'cl-lib)

(defun mpv-load-module ()
  "Load the mpv module."
  (load "/home/neo/.emacs.d/plugins/mpv/target/debug/libmpv.so"))

(defvar mpv-subtitle-timer nil
  "Timer for subtitle highlight update.")

(defvar mpv-subtitle-current-index nil
  "Current highlighted subtitle index.")

(defvar mpv-subtitle-entries nil
  "List of parsed subtitle entries.")

(defvar-local mpv-subtitle-video-path nil
  "Path to the currently playing video.")

(define-derived-mode mpv-subtitle-mode special-mode "Subtitle"
  "Major mode for displaying subtitles."
  (setq truncate-lines t)
  (setq buffer-read-only nil))

(defun mpv-parse-srt (srt-content)
  "Parse SRT content and return a list of entries.
Each entry is: (start-time end-time text)"
  (let ((entries nil))
    (dolist (entry (xml-parse-region (point-min) (point-max)))
      (when (eq (car entry) 'p)
        (let ((start (string-to-number (or (car (alist-get 'begin (nth 2 entry))) "0")))
              (end (string-to-number (or (car (alist-get 'end (nth 2 entry))) "0")))
              (text (mapconcat #'caddr (cddr entry) "\n")))
          (push (list start end text) entries))))
    (nreverse entries)))

(defun mpv-load-subtitle-file (srt-path)
  "Load SRT file from SRT-PATH and display in subtitle buffer."
  (let ((buf (get-buffer-create "*MPV Subtitle*"))
        (entries (with-temp-buffer
                   (insert-file-contents srt-path)
                   (mpv-parse-srt-from-string (buffer-string)))))
    (setq mpv-subtitle-entries entries)
    (setq mpv-subtitle-last-index -1)
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (mpv-subtitle-mode)
      (dolist (entry entries)
        (let ((start (car entry))
              (text (caddr entry)))
          (insert (format "[%05.1f] %s\n" start text))))
      (setq buffer-read-only t)
      (setq mpv-subtitle-current-index nil))
    (display-buffer buf)
    buf))

(defun mpv-parse-srt-from-string (content)
  "Parse raw SRT string CONTENT into a list of entries.
Each entry: (start-time end-time text)"
  (let ((entries nil)
        (blocks (split-string content "\r?\n\r?\n" t)))
    (dolist (block blocks)
      (when (string-match "\\`[0-9]+\n\\([0-9:,]+\\) --> \\([0-9:,]+\\)" block)
        (let* ((start-str (match-string 1 block))
               (end-str (match-string 2 block))
               (start (mpv-parse-timestamp start-str))
               (end (mpv-parse-timestamp end-str))
               (text (car (last (split-string block "\n" t)))))
          (when start
            (push (list start (or end start) text) entries)))))
    (nreverse entries)))

(defun mpv-parse-timestamp (ts)
  "Parse SRT timestamp like '00:00:01,500' to seconds as float."
  (when ts
    (if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[,\\.]\\([0-9]+\\)" ts)
        (let ((h (string-to-number (match-string 1 ts)))
              (m (string-to-number (match-string 2 ts)))
              (s (string-to-number (match-string 3 ts)))
              (ms (string-to-number (match-string 4 ts))))
          (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))
      nil)))

(defun mpv-find-subtitle-at-time (time)
  "Find subtitle entry at TIME (in seconds)."
  (seq-find (lambda (entry)
              (and (>= time (car entry))
                   (< time (cadr entry))))
            mpv-subtitle-entries))

(defun mpv-get-subtitle-index (time)
  "Get the index of subtitle entry at TIME (in seconds)."
  (seq-position mpv-subtitle-entries
                (mpv-find-subtitle-at-time time)
                #'equal))

(defvar mpv-subtitle-last-index -1
  "Last highlighted subtitle index.")

(defun mpv-update-subtitle-highlight ()
  "Update subtitle highlight based on current playback time."
  (let ((time (ignore-errors (mpv-get-time))))
    (when (and time mpv-subtitle-entries (get-buffer "*MPV Subtitle*"))
      (let* ((matches (cl-loop for e in mpv-subtitle-entries
                               for i from 0
                               when (and (<= (car e) time) (< time (cadr e)))
                               collect i))
             (idx (car matches)))
        (when (and idx (numberp idx) (/= idx mpv-subtitle-last-index))
          (setq mpv-subtitle-last-index idx)
          (with-current-buffer "*MPV Subtitle*"
            (remove-overlays (point-min) (point-max) 'mpv-subtitle-highlight t)
            (goto-char (point-min))
            (forward-line idx)
            (let ((ov (make-overlay (point) (line-end-position))))
              (overlay-put ov 'mpv-subtitle-highlight t)
              (overlay-put ov 'face '(:background "dark slate gray" :foreground "white"))
              (setq mpv-subtitle-current-index (point)))))))))

(defun mpv-subtitle-auto-update ()
  "Start auto-update timer for subtitle highlight."
  (setq mpv-subtitle-timer
        (run-with-timer 0 0.1 #'mpv-update-subtitle-highlight)))

(defun mpv-subtitle-stop-update ()
  "Stop auto-update timer."
  (when mpv-subtitle-timer
    (cancel-timer mpv-subtitle-timer)
    (setq mpv-subtitle-timer nil)))

(defun mpv-play-with-subtitle (video-path)
  "Play VIDEO-PATH and load corresponding subtitle if exists."
  (condition-case err
      (progn
        (or (fboundp 'mpv-get-time)
            (load "/home/neo/.emacs.d/plugins/mpv/target/debug/libmpv.so"))
        (mpv-play video-path)
        (setq mpv-subtitle-video-path video-path)
        (let ((srt-path (concat (file-name-sans-extension video-path) ".srt")))
          (if (file-exists-p srt-path)
              (progn
                (mpv-load-subtitle-file srt-path)
                (mpv-subtitle-auto-update))
            (message "No subtitle file found for %s" video-path))))
    (error (message "Error: %s" err))))

(provide 'mpv-subtitle)

;;; mpv-subtitle.el ends here