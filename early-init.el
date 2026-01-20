(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'after-init-hook
	  (lambda ()
	  	  (setq gc-cons-threshold (* 100 1024 1024)
		  	gc-cons-percentage 0.1)))

(setq vc-handle-backends '(Git))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))

