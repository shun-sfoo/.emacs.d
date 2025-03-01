;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time
(require 'init-utils)
(require 'init-site-lisp)

(setq custom-file (locate-user-emacs-file "custom.el"))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; 定义常量控制拼写检查
(defconst *spell-check-support-enabled* nil)  ;; 默认禁用
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection threshold for early startup (see use of gcmh below)
(setq gc-cons-threshold (* 128 1024 1024))

;; Process performance tuning
(setq read-process-output-max (* 4 1024 1024))
(setq process-adaptive-read-buffering nil)

;;(require 'init-utils)

(defun pulse-line (&rest _)
  "Pulse the current line."
  (interactive)
      (pulse-momentary-highlight-one-line (point)))

;; (advice-add 'kill-ring-save :after #'pulse-line)

(setq flymake-show-diagnostics-at-end-of-line 'short)

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
			     ))

(add-hook 'prog-mode-hook (lambda ()
			    (prettify-symbols-mode)
			    (electric-pair-mode)
			    (flymake-mode 1)
			    ))

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

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(provide 'init)
;;; init.el ends here
