(setq mac-option-modifier 'meta)

;;; Buffer
(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode t)

(customize-set-variable 'dired-dwim-target t)
(customize-set-variable 'dired-auto-revert-buffer t)
(customize-set-variable 'eshell-scroll-to-bottom-on-input 'this)
(customize-set-variable 'switch-to-buffer-in-dedicated-window 'pop)
(customize-set-variable 'switch-to-buffer-obey-display-actions t)
(keymap-global-set "<remap> <list-buffers>" #'ibuffer-list-buffers)
(customize-set-variable 'ibuffer-movement-cycle nil)
(customize-set-variable 'ibuffer-old-time 24)

;;; Completion settings

(fido-vertical-mode)

(when (version< "30" emacs-version)
  (global-completion-preview-mode 1)
  (keymap-set completion-preview-active-mode-map "M-n" #'completion-preview-next-candidate)
  (keymap-set completion-preview-active-mode-map "M-p" #'completion-preview-prev-candidate))

;; No matter which completion mode is used:
(customize-set-variable 'tab-always-indent 'complete)
(customize-set-variable 'completion-cycle-threshold 3)
(customize-set-variable 'completion-category-overrides
                        '((file (styles . (partial-completion)))))
(customize-set-variable 'completions-detailed t)

;; use completion system instead of popup window
(customize-set-variable 'xref-show-definitions-function
                        #'xref-show-definitions-completing-read)

;;; Editing

;; Typed text replaces the selection if the selection is active,
;; pressing delete or backspace deletes the selection.
(delete-selection-mode)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; define a key to define the word at point.
(keymap-set global-map "M-#" #'dictionary-lookup-definition)

;; Show dictionary definition on the left
(add-to-list 'display-buffer-alist
             '("^\\*Dictionary\\*"
               (display-buffer-in-side-window)
               (side . left)
               (window-width . 70)))

;; turn on spell checking, if available.
(with-eval-after-load 'ispell
  (when (executable-find ispell-program-name)
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)))

;;; Persistence between sessions

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)

;; Enable savehist-mode for command history
(savehist-mode 1)

;; save the bookmarks file every time a bookmark is made or deleted
;; rather than waiting for Emacs to be killed.  Useful especially when
;; Emacs is a long running process.
(customize-set-variable 'bookmark-save-flag 1)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)


;; open man pages in their own window, and switch to that window to
;; facilitate reading and closing the man page.
(customize-set-variable 'Man-notify-method 'aggressive)

;; keep the Ediff control panel in the same frame
(customize-set-variable 'ediff-window-setup-function
                        'ediff-setup-windows-plain)

;; Window configuration for special windows.
(add-to-list 'display-buffer-alist
             '("\\*Help\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)))

(add-to-list 'display-buffer-alist
             '("\\*Completions\\*"
               (display-buffer-reuse-window display-buffer-pop-up-window)
               (inhibit-same-window . t)
               (window-height . 10)))

;;; Miscellaneous

;; Load source (.el) or the compiled (.elc or .eln) file whichever is
;; newest
(customize-set-variable 'load-prefer-newer t)

;; Make shebang (#!) file executable when saved
;; 保存一个包含 shebang 行的文件时，自动将其设为可执行文件(即运行 chmod +x)
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; Turn on repeat mode to allow certain keys to repeat on the last
;; keystroke. For example, C-x [ to page backward, after pressing this
;; keystroke once, pressing repeated [ keys will continue paging
;; backward. `repeat-mode' is exited with the normal C-g, by movement
;; keys, typing, or pressing ESC three times.
(repeat-mode 1)

(global-hl-line-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(line-number-mode)
(column-number-mode)
(setq inhibit-startup-screen t)

(save-place-mode)

(which-function-mode 1)
(electric-indent-mode t)
(electric-pair-mode t)

(set-face-attribute 'default nil :height 170 :font "Operator Mono Lig Book")
;;(set-face-attribute 'bold nil :height 140 :font "Operator Mono Lig Book")
;;(set-face-attribute 'default nil :height 130 :font "Monaco")

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'gruber-darker t)
;;(load-theme 'carbon t)

(if (version< "30" emacs-version)
    (load-theme 'modus-vivendi-deuteranopia t)
  (load-theme 'modus-vivendi t))


(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (css-mode . css-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (c-or-c++-mode . c-or-c++-ts-mode)
        (sh-mode . bash-ts-mode)))


