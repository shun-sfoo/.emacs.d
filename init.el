;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;; 定义常量控制拼写检查
(defconst *spell-check-support-enabled* nil)  ;; 默认禁用

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
(menu-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)


(icomplete-mode 1)
(icomplete-vertical-mode 1)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

(load-theme 'modus-operandi t)

;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 110 :weight 'light :family "Operator Mono SSm Lig")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(global-auto-revert-mode 1)
(prettify-symbols-mode 1)
(electric-pair-mode 1)

(provide 'init)
