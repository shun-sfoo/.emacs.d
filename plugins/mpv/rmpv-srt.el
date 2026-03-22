;;; rmpv-srt.el --- SRT subtitle parser  -*- lexical-binding: t; -*-

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

;; rmpv-srt.el provides SRT (SubRip) subtitle file parsing.
;; It extracts timing and text data from SRT files for use with video players.

;;; Code:

(require 'cl-lib)
(require 'seq)

;;;; Struct Definition

(cl-defstruct (rmpv-srt-entry (:constructor rmpv-srt-entry-create))
  (index 0 :type integer)
  (start 0.0 :type float)
  (end 0.0 :type float)
  (text "" :type string))

;;;; Timestamp Parsing

(defun rmpv-srt-parse-timestamp (ts)
  "Parse SRT timestamp like '00:00:01,500' to seconds as float.

TS should be a string in the format HH:MM:SS,mmm or HH:MM:SS.mmm.
Returns a float representing seconds, or nil if parsing fails."
  (when (and (stringp ts) (not (string-blank-p ts)))
    (let* ((cleaned (replace-regexp-in-string "[ \t\r\n]" "" ts))
           (pattern (concat "\\`\\([0-9]+\\):\\([0-9]+\\):"
                            "\\([0-9]+\\)[,.]\\([0-9]+\\)\\'")))
      (when (string-match pattern cleaned)
        (let ((h (string-to-number (match-string 1 cleaned)))
              (m (string-to-number (match-string 2 cleaned)))
              (s (string-to-number (match-string 3 cleaned)))
              (ms (string-to-number (match-string 4 cleaned))))
          (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))))))

(defun rmpv-srt-format-timestamp (seconds)
  "Format SECONDS (float) as SRT timestamp string 'HH:MM:SS,mmm'."
  (when (numberp seconds)
    (let* ((h (floor (/ seconds 3600)))
           (m (floor (/ (- seconds (* 3600 h)) 60)))
           (s (floor (- seconds (* 3600 h) (* 60 m))))
           (ms (floor (* (- seconds (floor seconds)) 1000))))
      (format "%02d:%02d:%02d,%03d" h m s ms))))

;;;; Entry Accessors

(defun rmpv-srt-entry-duration (entry)
  "Return duration of ENTRY in seconds."
  (- (rmpv-srt-entry-end entry) (rmpv-srt-entry-start entry)))

(defun rmpv-srt-entry-matches-time (entry time)
  "Return t if ENTRY is active at TIME (inclusive start, exclusive end)."
  (and (numberp (rmpv-srt-entry-start entry))
       (numberp (rmpv-srt-entry-end entry))
       (<= (rmpv-srt-entry-start entry) time)
       (< time (rmpv-srt-entry-end entry))))

;;;; Block Parsing

(defun rmpv-srt-parse-block (block)
  "Parse a single SRT BLOCK (string) into an rmpv-srt-entry.

BLOCK should contain index, timestamp line, and text lines.
Returns an rmpv-srt-entry or nil if parsing fails."
  (when (stringp block)
    (let ((trimmed (string-trim block)))
      (when (string-match (concat "\\`\\([0-9]+\\)[ \t]*\n"
                                  "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)"
                                  "[ \t]+-->?[ \t]+"
                                  "\\([0-9]+:[0-9]+:[0-9]+[,.:][0-9]+\\)")
                          trimmed)
        (let* ((index (string-to-number (match-string 1 trimmed)))
               (start-str (match-string 2 trimmed))
               (end-str (match-string 3 trimmed))
               (text-start (match-end 0))
               (rest (substring trimmed text-start))
               (text (string-trim rest))
               (start (or (rmpv-srt-parse-timestamp start-str) 0.0))
               (end (or (rmpv-srt-parse-timestamp end-str) start)))
          (rmpv-srt-entry-create :index index
                                 :start start
                                 :end end
                                 :text text))))))

;;;; File Parsing

(defun rmpv-srt-parse-from-string (content)
  "Parse raw SRT string CONTENT into a list of rmpv-srt-entry structs.

Blocks are separated by blank lines (one or more \\n or \\r\\n).
Returns a list of rmpv-srt-entry structs."
  (when (stringp content)
    (seq-into
     (seq-map #'rmpv-srt-parse-block
              (split-string content "\\(\r?\n\\)\r?\n" t))
     'list)))

(defun rmpv-srt-parse-file (path)
  "Parse SRT file at PATH and return a list of rmpv-srt-entry structs."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (rmpv-srt-parse-from-string (buffer-string)))))

;;;; Searching

(defun rmpv-srt-find-at-time (entries time)
  "Find the first ENTRY in ENTRIES that is active at TIME.
Returns the matching entry or nil."
  (seq-find (lambda (e) (rmpv-srt-entry-matches-time e time)) entries))

(defun rmpv-srt-find-index-at-time (entries time)
  "Find the index of the first ENTRY in ENTRIES active at TIME.
Returns the index (0-based) or nil."
  (let ((entry (rmpv-srt-find-at-time entries time)))
    (when entry
      (seq-position entries entry))))

(defun rmpv-srt-find-in-range (entries start-time end-time)
  "Find all ENTRIES that overlap with time range [START-TIME, END-TIME).
Returns a list of matching entries."
  (seq-filter (lambda (entry)
                (and (< (rmpv-srt-entry-start entry) end-time)
                     (> (rmpv-srt-entry-end entry) start-time)))
              entries))

;;;; Utilities

(defun rmpv-srt-total-duration (entries)
  "Return total duration of ENTRIES from first start to last end."
  (when entries
    (let ((first-start (apply #'min (seq-map #'rmpv-srt-entry-start entries)))
          (last-end (apply #'max (seq-map #'rmpv-srt-entry-end entries))))
      (- last-end first-start))))

(defun rmpv-srt-export-text (entries &optional separator)
  "Export ENTRIES text as a single string joined by SEPARATOR.
Default separator is two newlines."
  (mapconcat #'rmpv-srt-entry-text entries (or separator "\n\n")))

(defun rmpv-srt-describe-entry (entry)
  "Return a formatted description string for ENTRY."
  (format "[%d] %s --> %s\n%s"
          (rmpv-srt-entry-index entry)
          (rmpv-srt-format-timestamp (rmpv-srt-entry-start entry))
          (rmpv-srt-format-timestamp (rmpv-srt-entry-end entry))
          (rmpv-srt-entry-text entry)))

(provide 'rmpv-srt)

;;; rmpv-srt.el ends here
