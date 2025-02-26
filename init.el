;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

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


(defun pulse-line (&rest _)
  "Pulse the current line."
  (interactive)
      (pulse-momentary-highlight-one-line (point)))

;; (advice-add 'kill-ring-save :after #'pulse-line)

(add-hook 'after-init-hook (lambda ()
			     (global-display-line-numbers-mode 1)
			     (global-hl-line-mode 1)
			     (global-auto-revert-mode 1)
			     (which-key-mode)
			     (icomplete-mode 1)
			     (icomplete-vertical-mode 1)
			     ))

(add-hook 'prog-mode-hook (lambda ()
			    (prettify-symbols-mode)
			    (electric-pair-mode)
			    ))

(add-hook 'find-file-hook
          (lambda ()
            (when (string-match-p "/\\.git/" (buffer-file-name))
              (setq-local make-backup-files nil))))
;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(load-theme 'modus-operandi t)

;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 110 :weight 'light :family "Operator Mono SSm Lig")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(provide 'init)
