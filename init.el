;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst *installed_package-enable* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(setq custom-file (locate-user-emacs-file "custom.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
				     recenter-top-bottom other-window
				     kill-line))
  (advice-add command :after #'pulse-line))

(defun pulse-region (start end &rest _)
  "Pulse region."
  (pulse-momentary-highlight-region start end))
(advice-add 'kill-ring-save :after #'pulse-region)

(setq flymake-show-diagnostics-at-end-of-line 'short)
(setq isearch-lazy-count t)
(setq make-backup-files nil)

(add-hook 'after-init-hook (lambda ()
			     (global-display-line-numbers-mode 1)
			     (global-hl-line-mode 1)
			     (global-auto-revert-mode 1)
			     (which-key-mode)
			     (icomplete-mode 1)
			     (icomplete-vertical-mode 1)
			     (global-completion-preview-mode 1)
			     (fido-vertical-mode 1)
			     (pixel-scroll-precision-mode 1)
			     (recentf-mode 1)
			     (savehist-mode 1)
			     (delete-selection-mode 1)
			     (save-place-mode 1)
			     (repeat-mode 1)
			     ))

(add-hook 'prog-mode-hook (lambda ()
			    (prettify-symbols-mode)
			    (electric-pair-mode)
			    (flymake-mode 1)
			    ))

(defun open-init-file()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(add-hook 'emacs-startup-hook (lambda ()
				(load-theme 'modus-operandi-tinted)))

(set-face-attribute 'default nil
                    :height 110 :weight 'light :family "Operator Mono SSm Lig")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font) charset
                    (font-spec :family "LXGW WenKai Mono" :size 16)))

;; set key
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;; org mode
(setq org-agenda-files '("~/org/daily.org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)


;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
