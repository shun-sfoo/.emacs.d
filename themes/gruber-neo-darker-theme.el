;;; gruber-neo-darker-theme.el --- 自定义主题 (Emacs 30) -*- lexical-binding: t; -*-

(deftheme gruber-neo-darker "我的自定义主题")

(let* ((fg       "#e4e4ef")
       (fg+1     "#f4f4ff")
       (white    "#ffffff")
       (black    "#000000")
       (bg-1     "#101010")
       (bg       "#181818")
       (bg+1     "#282828")
       (bg+2     "#453d41")
       (bg+3     "#484848")
       (bg+4     "#52494e")
       (red-1    "#c73c3f")
       (red      "#f43841")
       (red+1    "#ff4f58")
       (green    "#73c936")
       (yellow   "#ffdd33")
       (orange   "#F57F29")
       (brown    "#cc8c3c")
       (quartz   "#95a99f")
       (niagara-2 "#303540")
       (niagara  "#96a6c8")
       (wisteria "#9e95c7")
       )

  (custom-theme-set-variables
   'gruber-neo-darker
   '(frame-background-mode 'dark))

  (custom-theme-set-faces
   'gruber-neo-darker

   ;; ========== 基础 ==========
   '(default ((((type graphic)) (:foreground "#e4e4ef" :background "#181818"))))
   '(cursor ((((type graphic)) (:background "#ffdd33"))))
   '(fringe ((((type graphic)) (:background unspecified :foreground "#453d41"))))
   '(vertical-border ((((type graphic)) (:foreground "#453d41"))))
   '(border ((((type graphic)) (:background "#101010" :foreground "#453d41"))))
   '(region ((((type graphic)) (:background "#484848" :foreground unspecified))))
   '(secondary-selection ((((type graphic)) (:background "#484848" :foreground unspecified))))
   '(highlight ((((type graphic)) (:background "#282828"))))
   '(minibuffer-prompt ((((type graphic)) (:foreground "#96a6c8"))))
   '(link ((((type graphic)) (:foreground "#96a6c8" :underline t))))
   '(link-visited ((((type graphic)) (:foreground "#9e95c7" :underline t))))
   '(match ((((type graphic)) (:background "#52494e"))))
   '(shadow ((((type graphic)) (:foreground "#52494e"))))
   '(trailing-whitespace ((((type graphic)) (:foreground "#000000" :background "#f43841"))))
   '(tooltip ((((type graphic)) (:background "#52494e" :foreground "#ffffff"))))
   '(error ((((type graphic)) (:foreground "#ff4f58"))))
   '(warning ((((type graphic)) (:foreground "#ffdd33"))))
   '(success ((((type graphic)) (:foreground "#73c936"))))

   ;; ========== 字体锁定 ==========
   '(font-lock-keyword-face ((((type graphic)) (:foreground "#ffdd33" :bold t))))
   '(font-lock-builtin-face ((((type graphic)) (:foreground "#ffdd33"))))
   '(font-lock-function-name-face ((((type graphic)) (:foreground "#96a6c8"))))
   '(font-lock-variable-name-face ((((type graphic)) (:foreground "#f4f4ff"))))
   '(font-lock-comment-face ((((type graphic)) (:foreground "#cc8c3c"))))
   '(font-lock-comment-delimiter-face ((((type graphic)) (:foreground "#cc8c3c"))))
   '(font-lock-string-face ((((type graphic)) (:foreground "#73c936"))))
   '(font-lock-doc-face ((((type graphic)) (:foreground "#73c936"))))
   '(font-lock-doc-string-face ((((type graphic)) (:foreground "#73c936"))))
   '(font-lock-constant-face ((((type graphic)) (:foreground "#95a99f"))))
   '(font-lock-type-face ((((type graphic)) (:foreground "#95a99f"))))
   '(font-lock-warning-face ((((type graphic)) (:foreground "#f43841"))))
   '(font-lock-preprocessor-face ((((type graphic)) (:foreground "#95a99f"))))
   '(font-lock-reference-face ((((type graphic)) (:foreground "#95a99f"))))

   ;; ========== C 语言特有 ==========
   '(c-label-face ((((type graphic)) (:foreground "#95a99f" :bold t))))
   '(c-annotation-face ((((type graphic)) (:foreground "#f43841"))))

   ;; ========== 搜索 ==========
   '(isearch ((((type graphic)) (:foreground "#000000" :background "#f4f4ff"))))
   '(isearch-fail ((((type graphic)) (:foreground "#000000" :background "#f43841"))))
   '(lazy-highlight ((((type graphic)) (:foreground "#f4f4ff" :background "#303540"))))

   ;; ========== Mode Line ==========
   '(mode-line ((((type graphic)) (:background "#282828" :foreground "#ffffff"))))
   '(mode-line-inactive ((((type graphic)) (:background "#282828" :foreground "#95a99f"))))
   '(mode-line-buffer-id ((((type graphic)) (:background "#282828" :foreground "#ffffff"))))
   '(header-line ((((type graphic)) (:inherit mode-line))))

   ;; ========== 括号匹配 ==========
   '(show-paren-match-face ((((type graphic)) (:background "#52494e"))))
   '(show-paren-mismatch-face ((((type graphic)) (:background "#c73c3f"))))

   ;; ========== 行号 ==========
   '(line-number ((((type graphic)) (:inherit default :foreground "#52494e"))))
   '(line-number-current-line ((((type graphic)) (:inherit line-number :foreground "#ffdd33"))))
   '(linum ((((type graphic)) (:foreground "#95a99f" :background "#181818"))))

   ;; ========== 高亮当前行 ==========
   '(highlight-current-line-face ((((type graphic)) (:background "#282828" :foreground unspecified))))

   ;; ========== Tab Bar ==========
   '(tab-bar ((((type graphic)) (:background "#282828" :foreground "#52494e"))))
   '(tab-bar-tab ((((type graphic)) (:background unspecified :foreground "#ffdd33" :weight bold))))
   '(tab-bar-tab-inactive ((((type graphic)) (:background unspecified))))

   ;; ========== Org Mode ==========
   '(org-document-title ((((type graphic)) (:foreground "#9e95c7" :weight bold :height 1.4))))
   '(org-document-info ((((type graphic)) (:foreground "#73c936"))))
   '(org-level-1 ((((type graphic)) (:foreground "#96a6c8" :bold t))))
   '(org-level-2 ((((type graphic)) (:foreground "#73c936" :bold t))))
   '(org-level-3 ((((type graphic)) (:foreground "#ffdd33"))))
   '(org-level-4 ((((type graphic)) (:foreground "#9e95c7"))))
   '(org-level-5 ((((type graphic)) (:foreground "#cc8c3c"))))
   '(org-level-6 ((((type graphic)) (:foreground "#95a99f"))))
   '(org-block ((((type graphic)) (:background "#181818" :foreground "#e4e4ef"))))
   '(org-code ((((type graphic)) (:inherit font-lock-constant-face))))
   '(org-verbatim ((((type graphic)) (:foreground "#95a99f"))))
   '(org-todo ((((type graphic)) (:foreground "#c73c3f"))))
   '(org-done ((((type graphic)) (:foreground "#73c936"))))
   '(org-agenda-done ((((type graphic)) (:foreground "#73c936"))))
   '(org-agenda-structure ((((type graphic)) (:foreground "#96a6c8" :height 1.1))))
   '(org-column ((((type graphic)) (:background "#101010"))))
   '(org-column-title ((((type graphic)) (:background "#101010" :underline t :weight bold))))
   '(org-upcoming-deadline ((((type graphic)) (:foreground "#ffdd33"))))
   '(org-scheduled-previously ((((type graphic)) (:foreground "#F57F29"))))
   '(org-table ((((type graphic)) (:foreground "#9e95c7"))))

   ;; ========== Outline ==========
   '(outline-1 ((((type graphic)) (:foreground "#96a6c8"))))
   '(outline-2 ((((type graphic)) (:foreground "#73c936"))))
   '(outline-3 ((((type graphic)) (:foreground "#ffdd33"))))
   '(outline-4 ((((type graphic)) (:foreground "#9e95c7"))))
   '(outline-5 ((((type graphic)) (:foreground "#cc8c3c"))))
   '(outline-6 ((((type graphic)) (:foreground "#95a99f"))))
   '(outline-7 ((((type graphic)) (:foreground "#ffdd33"))))
   '(outline-8 ((((type graphic)) (:foreground "#73c936"))))

   ;; ========== 终端颜色 ==========
   '(term-color-black ((((type graphic)) (:foreground "#484848" :background "#52494e"))))
   '(term-color-red ((((type graphic)) (:foreground "#c73c3f" :background "#c73c3f"))))
   '(term-color-green ((((type graphic)) (:foreground "#73c936" :background "#73c936"))))
   '(term-color-yellow ((((type graphic)) (:foreground "#ffdd33" :background "#ffdd33"))))
   '(term-color-blue ((((type graphic)) (:foreground "#96a6c8" :background "#96a6c8"))))
   '(term-color-magenta ((((type graphic)) (:foreground "#9e95c7" :background "#9e95c7"))))
   '(term-color-cyan ((((type graphic)) (:foreground "#95a99f" :background "#95a99f"))))
   '(term-color-white ((((type graphic)) (:foreground "#e4e4ef" :background "#ffffff"))))

   ;; ========== Diff ==========
   '(diff-added ((((type graphic)) (:foreground "#73c936" :background unspecified))))
   '(diff-removed ((((type graphic)) (:foreground "#ff4f58" :background unspecified))))
   '(diff-header ((((type graphic)) (:background "#453d41"))))

   ;; ========== 补全 (内置) ==========
   '(completions-annotations ((((type graphic)) (:inherit shadow))))

   ;; ========== Dired ==========
   '(dired-directory ((((type graphic)) (:foreground "#96a6c8" :weight bold))))
   '(dired-ignored ((((type graphic)) (:foreground "#95a99f"))))

   ;; ========== Message ==========
   '(message-header-name ((((type graphic)) (:foreground "#73c936"))))
   '(message-header-subject ((((type graphic)) (:foreground "#96a6c8"))))
   '(message-header-to ((((type graphic)) (:foreground "#ffdd33"))))
   '(message-header-other ((((type graphic)) (:foreground "#e4e4ef"))))
   '(message-separator ((((type graphic)) (:foreground "#9e95c7"))))

   ;; ========== Info ==========
   '(info-xref ((((type graphic)) (:foreground "#96a6c8"))))
   '(info-visited ((((type graphic)) (:foreground "#9e95c7"))))
   '(Info-quoted ((((type graphic)) (:inherit font-lock-constant-face))))
   '(info-menu-header ((((type graphic)) (:foreground "#73c936" :weight bold :height 1.4))))
   '(info-menu-star ((((type graphic)) (:foreground "#ffdd33"))))
   '(info-node ((((type graphic)) (:foreground "#73c936" :weight bold :slant italic))))
   '(info-title-1 ((((type graphic)) (:weight bold :height 1.4))))
   '(info-title-2 ((((type graphic)) (:weight bold :height 1.2))))
   '(info-title-3 ((((type graphic)) (:weight bold :foreground "#cc8c3c"))))
   '(info-title-4 ((((type graphic)) (:weight bold :foreground "#9e95c7"))))

   ;; ========== Which Function ==========
   '(which-func ((((type graphic)) (:foreground "#9e95c7"))))

   ;; ========== 编译 ==========
   '(compilation-info ((((type graphic)) (:foreground "#73c936" :inherit unspecified))))
   '(compilation-warning ((((type graphic)) (:foreground "#cc8c3c" :bold t :inherit unspecified))))
   '(compilation-error ((((type graphic)) (:foreground "#ff4f58"))))
   '(compilation-mode-line-fail ((((type graphic)) (:foreground "#f43841" :weight bold :inherit unspecified))))
   '(compilation-mode-line-exit ((((type graphic)) (:foreground "#73c936" :weight bold :inherit unspecified))))

   ;; ========== Flymake ==========
   '(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#f43841") :foreground unspecified :background unspecified :inherit unspecified))
      (t (:foreground "#f43841" :weight bold :underline t))))
   '(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#ffdd33") :foreground unspecified :background unspecified :inherit unspecified))
      (t (:foreground "#ffdd33" :weight bold :underline t))))
   '(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#73c936") :foreground unspecified :background unspecified :inherit unspecified))
      (t (:foreground "#73c936" :weight bold :underline t))))

   ;; ========== Flyspell ==========
   '(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#f43841") :inherit unspecified))
      (t (:foreground "#f43841" :weight bold :underline t))))
   '(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color "#ffdd33") :inherit unspecified))
      (t (:foreground "#ffdd33" :weight bold :underline t))))

   ;; ========== Whitespace ==========
   '(whitespace-space ((((type graphic)) (:background "#181818" :foreground "#282828"))))
   '(whitespace-tab ((((type graphic)) (:background "#181818" :foreground "#282828"))))
   '(whitespace-hspace ((((type graphic)) (:background "#181818" :foreground "#453d41"))))
   '(whitespace-line ((((type graphic)) (:background "#453d41" :foreground "#ff4f58"))))
   '(whitespace-newline ((((type graphic)) (:background "#181818" :foreground "#453d41"))))
   '(whitespace-trailing ((((type graphic)) (:background "#f43841" :foreground "#f43841"))))
   '(whitespace-empty ((((type graphic)) (:background "#ffdd33" :foreground "#ffdd33"))))
   '(whitespace-indentation ((((type graphic)) (:background "#ffdd33" :foreground "#f43841"))))
   '(whitespace-space-after-tab ((((type graphic)) (:background "#ffdd33" :foreground "#ffdd33"))))
   '(whitespace-space-before-tab ((((type graphic)) (:background "#cc8c3c" :foreground "#cc8c3c"))))

   ;; ========== Window Divider ==========
   '(window-divider ((((type graphic)) (:foreground "#52494e"))))
   '(window-divider-first-pixel ((((type graphic)) (:foreground "#52494e"))))
   '(window-divider-last-pixel ((((type graphic)) (:foreground "#52494e"))))

   ;; ========== Calendar ==========
   '(holiday-face ((((type graphic)) (:foreground "#f43841"))))

   ;; ========== EShell ==========
   '(eshell-ls-backup ((((type graphic)) (:foreground "#95a99f"))))
   '(eshell-ls-directory ((((type graphic)) (:foreground "#96a6c8"))))
   '(eshell-ls-executable ((((type graphic)) (:foreground "#73c936"))))
   '(eshell-ls-symlink ((((type graphic)) (:foreground "#ffdd33"))))

   ;; ========== Custom ==========
   '(custom-state ((((type graphic)) (:foreground "#73c936"))))

   ;; ========== sh ==========
   '(sh-quoted-exec ((((type graphic)) (:foreground "#ff4f58"))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'gruber-neo-darker)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; gruber-neo-darker-theme.el ends here