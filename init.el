;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-after-load 'cl-lib nil)
(defconst *installed-package-enable* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(setq custom-file (locate-user-emacs-file "custom.el"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq make-backup-files nil
      isearch-lazy-count t
      flymake-show-diagnostics-at-end-of-line 'short)

(add-hook 'after-init-hook (lambda ()
			     (progn
			       (global-display-line-numbers-mode 1)
			       (global-hl-line-mode 1)
			       (global-auto-revert-mode 1)
			       (icomplete-mode 1)
			       (icomplete-vertical-mode 1)
			       (fido-vertical-mode 1)
			       (pixel-scroll-precision-mode 1)
			       (recentf-mode 1)
			       (when (fboundp 'global-completion-preview-mode)
				 (global-completion-preview-mode))
			       (when (fboundp 'which-key-mode)
				 (which-key-mode))
			       (savehist-mode 1)
			       (delete-selection-mode 1)
			       (save-place-mode 1)
			       (repeat-mode 1))
			     ))

(add-hook 'prog-mode-hook (lambda ()
			    (prettify-symbols-mode)
			    (electric-pair-mode)
			    (flymake-mode 1)
			    ))

(add-hook 'emacs-startup-hook (lambda ()
				(if (< emacs-major-version 30)
				    (load-theme 'modus-operandi)
				  (load-theme 'modus-operandi-tinted))))

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun setup-fonts ()
  "Setup fonts."
  ;; Set default font
  (cl-loop for font in '("Operator Mono SSm Lig" "Fira Code" "Consolas")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :family font
				      :weight 'light
                                      :height (cond (sys/macp 130)
                                                    (sys/win32p 110)
                                                    (t 110))))
  ;; Specify font for all unicode characters
  (cl-loop for font in '("Apple Symbols" "Segoe UI Symbol" "Symbola" "Symbol")
           when (font-installed-p font)
           return (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
  ;; Emoji
  (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
           when (font-installed-p font)
           return (set-fontset-font t
                                    (if (< emacs-major-version 28)'symbol 'emoji)
                                    (font-spec :family font) nil 'prepend))
  ;; Specify font for Chinese characters
  (cl-loop for font in '("LXGW WenKai Mono Screen" "WenQuanYi Micro Hei Mono" "PingFang SC" "Microsoft Yahei UI" "Simhei")
           when (font-installed-p font)
           return (progn
                    (setq face-font-rescale-alist `((,font . 1.2)))
                    (set-fontset-font t 'han (font-spec :family font)))))

(add-hook 'window-setup-hook
	  (lambda ()
	    (when (display-graphic-p)
	      (setup-fonts))))

;; set key
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "<f2>") 'open-init-file)

;;; custom function
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
				     recenter-top-bottom other-window
				     kill-line))
  (advice-add command :after #'pulse-line))

(defun pulse-region (start end &rest _)
  "Pulse START to END region."
  (pulse-momentary-highlight-region start end))
(advice-add 'kill-ring-save :after #'pulse-region)

(defun open-init-file()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; org mode
(setq org-agenda-files '("~/org/daily.org"
			 "~/org/TaskManager.org"))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
