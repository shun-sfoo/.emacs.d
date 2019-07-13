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

;; 设定customize-group自动设置的变量的值存放到哪个文件中
(setq custom-file (expand-file-name "lisp/init-customize-groups.el" user-emacs-directory))
(load-file custom-file)
