;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package package
  :unless  (eq system-type "window-nt")
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

(use-package emacs
  :ensure nil
  :custom
  (mac-option-modifier 'meta))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package global-line-numbers
  :ensure nil
  :hook (after-init . global-display-line-numbers-mode))

(use-package icomplete
  :ensure nil
  :hook (after-init . fido-vertical-mode))

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-type 'relative))

(use-package completion-preview
  :ensure nil
  :if (version< "30" emacs-version)
  :hook
  (after-init . global-completion-preview-mode))

(use-package which-key
  :ensure nil
  :if (version< "30" emacs-version)
  :hook
  (after-init . which-key-mode))

(use-package pixel-scroll
  :ensure nil
  :hook
  (after-init . pixel-scroll-precision-mode))

(use-package files
  :ensure nil
  :hook
  (after-init . auto-save-visited-mode)
  (after-save . executable-make-buffer-file-executable-if-script-p)
  :custom
  (make-backup-files nil)
  (auto-save-default t)
  (auto-save-visited-interval 5))

(use-package repeat
  :ensure nil
  :hook
  (after-init . repeat-mode))

;; (use-package  startup
;;   :ensure nil
;;   :custom
;;   (inhibit-startup-screen t))

(use-package saveplace
  :ensure nil
  :hook
  (after-init . save-place-mode))

(use-package which-func
  :ensure nil
  :hook
  (after-init . which-function-mode))

(use-package elec-pair
  :ensure nil
  :hook
  (after-init . electric-pair-mode))

(use-package electric
  :ensure nil
  :hook
  (after-init . electric-indent-mode))

(use-package flymake
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :config
  (when (version< "30" emacs-version)
    (setq flymake-show-diagnostics-at-end-of-line 'short)))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t))

(use-package simple
  :ensure nil
  :hook (after-init . (lambda ()
                        (line-number-mode)
                        (column-number-mode)
                        (size-indication-mode)
                        ))
  :hook (prog-mode . (lambda ()
                       (prettify-symbols-mode)
                       ))
  :custom
  (auto-save-interval 300)
  (auto-save-timeout 30)
  (indent-tabs-mode nil)
  (kill-do-not-save-duplicates t))


(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))

(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-auto-revert-buffer t))

(use-package esh-mode
  :ensure nil
  :custom
  (eshell-scroll-to-bottom-on-input 'this))

(use-package window
  :ensure nil
  :custom
  (switch-to-buffer-in-dedicated-window 'pop)
  (switch-to-buffer-obey-display-actions t)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*Dictionary\\*"
                 (display-buffer-in-side-window)
                 (side . left)
                 (window-width . 70)))
  (add-to-list 'display-buffer-alist
               '("\\*Help\\*"
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  )

(use-package ibuffer
  :ensure nil
  :bind ([remap list-buffers] . #'ibuffer-list-buffers)
  :custom
  (ibuffer-movement-cycle nil)
  (ibuffer-old-time 24))

;; (use-package indent
;;   :ensure nil
;;   :custom
;;   (tab-always-indent 'complete)
;;   )

(use-package minibuffer
  :ensure nil
  :custom
  (completion-cycle-threshold 3)
  (completion-category-overrides '((file (styles . (partial-completion)))))
  (completions-detailed t))

(use-package xref
  :ensure nil
  :custom
  (xref-show-definitions-function  #'xref-show-definitions-completing-read))

(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$"))
  :config
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90))

(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)
   )
  )

(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

(use-package faces
  :ensure nil
  :config
  (if (eq system-type 'darwin)
      (set-face-attribute 'default nil :height 150 :font "Operator Mono Lig Book")
    (set-face-attribute 'default nil :height 130 :font "Operator Mono Lig Book Light")
    (set-face-attribute 'bold nil :height 130 :font "Operator Mono Lig Book"))
  (set-fontset-font t 'han (font-spec :family "LXGW WenKai Mono")))

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (major-mode-remap-alist
   '((python-mode . python-ts-mode)
     (js-mode . js-ts-mode)
     (css-mode . css-ts-mode)
     (c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode)
     (sh-mode . bash-ts-mode)))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          )
        )
  )

;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(use-package ef-themes
  :ensure t
  :init
  (mapc #'disable-theme custom-enabled-themes)
  :config
  (load-theme 'ef-dream t)    
  )

;; Enables automatic indentation of code while typing
(use-package aggressive-indent
  :ensure t
  :commands aggressive-indent-mode
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

;; Highlights function and variable definitions in Emacs Lisp mode
(use-package highlight-defined
  :ensure t
  :commands highlight-defined-mode
  :hook
  (emacs-lisp-mode . highlight-defined-mode))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :hook
  (emacs-lisp-mode . paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "RET") nil))

(use-package org-capture
  :ensure nil
  :bind  (("C-c c" . org-capture))
  :custom
  (org-capture-templates '(("t" "TODO" entry (file+headline "~/org/gtd.org" "Tasks")
  			  "* TODO %?\n %i\n %a" :clock-in t :clock-resume t)
  			 ("r" "READING" entry (file+headline "~/org/reading.org" "Reading")
  			  "* TODO %?\n %i\n %a" :clock-in t :clock-resume t)
  			 ("b" "BLOG" entry (file+headline "~/org/blog.org" "Blog")
  			  "* TODO %?\n %i\n %a" :clock-in t :clock-resume t)
  			 ))
  )

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda))
  :custom
  (org-agenda-files '("~/org/gtd.org" "~/org/reading.org" "~/org/blog.org"))
  )

(use-package org-modern
  :ensure t
  :init
  (with-eval-after-load 'org (global-org-modern-mode)))

(use-package diff-hl
  :ensure t)

(use-package magit
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c m c" . mc/edit-beginnings-of-lines)
         ("C-c m e" . mc/edit-ends-of-lines))
  :config
  (setq mc/list-file "～/.emacs.d/.mc-lists.el")) ; 可选：保存宏

(use-package indent-bars
  :ensure t
  :hook ((prog-mode) . indent-bars-mode))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
