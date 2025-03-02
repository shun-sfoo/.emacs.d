;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *installed_package-enable* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(setq custom-file (locate-user-emacs-file "custom.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time
(require 'init-utils)
(require 'init-site-lisp)

(if *installed_package-enable*
    (require 'init-elpa))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
;;(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
;;(setq read-process-output-max (* 4 1024 1024))
;;(setq process-adaptive-read-buffering nil)


(defun pulse-line (&rest _)
  "Pulse the current line."
  (interactive)
      (pulse-momentary-highlight-one-line (point)))

;;(advice-add 'kill-ring-save :after #'pulse-line)

(setq flymake-show-diagnostics-at-end-of-line 'short)
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s ")

(add-hook 'after-init-hook (lambda ()
			     (global-display-line-numbers-mode 1)
			     (global-hl-line-mode 1)
			     (global-auto-revert-mode 1)
			     (which-key-mode)
			     (icomplete-mode 1)
			     (icomplete-vertical-mode 1)
			     (global-completion-preview-mode 1)
			     (pixel-scroll-mode 1)
			     (pixel-scroll-precision-mode 1)
			     (recentf-mode 1)
			     (savehist-mode 1)
			     (save-place-mode 1)
			     ))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))


(add-hook 'prog-mode-hook (lambda ()
			    (prettify-symbols-mode)
			    (electric-pair-mode)
			    (flymake-mode 1)
			    (whitespace-mode 1)
			    ))

(setq whitespace-style '(face trailing))

(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "/\\.git/" (buffer-file-name))
              (setq-local make-backup-files nil))))

;; 快速打开配置文件
(defun open-init-file()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(add-hook 'emacs-startup-hook (lambda ()
				(load-theme 'modus-operandi-tinted)))

;; --- Typography stack -------------------------------------------------------
;;(add-to-list 'default-frame-alist '(font . "LXGW WenKai Mono-13"))
(set-face-attribute 'default nil
                    :height 110 :weight 'light :family "Operator Mono SSm Lig")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                   (font-spec :family "LXGW WenKai Mono" :size 16)))

;;(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
;;(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
