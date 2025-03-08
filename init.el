;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

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


(if (< emacs-major-version 30)
    (add-hook 'emacs-startup-hook (lambda ()
				    (load-theme 'modus-operandi)))
  (progn
    (add-hook 'after-init-hook #'global-completion-preview-mode)
    (add-hook 'emacs-startup-hook (lambda ()
				    (load-theme 'modus-operandi-tinted)))))

(defun open-init-file()
  "Open init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)


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

(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
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
                      (set-fontset-font t 'han (font-spec :family font))))))

(centaur-setup-fonts)

;; set key
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

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
