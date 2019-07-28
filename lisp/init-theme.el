(provide 'init-theme)

;;========================================================================================
;;                                       加载主题
;;========================================================================================
(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
      doom-themes-enable-italic t)   ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-nord-light t)
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
;; (doom-themes-neotree-config)
;; or for treemacs users
;; (doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; 透明设置
;; (set-frame-parameter (selected-frame) 'alpha '(92 70))
;; (add-to-list 'default-frame-alist '(alpha 92 70))

;; 根据时间自动切换主题
;; (setq day-theme 'sanityinc-tomorrow-day)  ;; 白天使用的主题
;; (setq dark-theme 'sanityinc-tomorrow-bright)  ;;晚上使用的主题
;; (defun synchronize-theme ()
;;     (setq hour
;;         (string-to-number
;;             (substring (current-time-string) 11 13)))
;;     (if (member hour (number-sequence 6 18))
;;         (setq now day-theme)
;;         (setq now dark-theme))
;;     (load-theme now t)
;; )
;; (run-with-timer 0 3600 'synchronize-theme)
