;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
;; 设置环境为UTF-8编码
(set-language-environment "UTF-8")

(setq gc-cons-threshold (* 50 1000 1000))
(package-initialize)
;;========================================================================================
;;                                   配置文件模块化配置
;;========================================================================================
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-ui)
(require 'init-batter-default)
(require 'init-packages)
(require 'init-evil)
(require 'init-theme)
(require 'init-company)
(require 'init-org)
(require 'init-keybindings)
(require 'init-chinese)
(require 'lang-racket)

;; 设定customize-group自动设置的变量的值存放到哪个文件中
(setq custom-file (expand-file-name "lisp/init-customize-groups.el" user-emacs-directory))
(load-file custom-file)

; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
