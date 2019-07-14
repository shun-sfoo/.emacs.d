(provide 'init-evil)

;;=========================================================================================
;;                             配置Evil,Evil是一个模拟Vim的插件
;;=========================================================================================
(require 'evil)
(evil-mode t)
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)
;; evil-surround是一个可以快速的将选中区域进行匹配的操作的插件
;; 例如选中区域两边同时进行添加或修改括号,引号等操作
(require 'evil-surround)
(global-evil-surround-mode t)
;; 然后在normal模式下按 vi w S  ' 就可以把选中区域两边加 ' 号
;; cs ' 就可以取消两边的 ' 号
;; vi w S (  两边加括号( 内容 ) ,这样加的括号,内容两边有空格
;; vi w S )  两边加括号(内容), 这样加的括号,内容两边没有空格
;; cs ( 或 ) 取消两边的括号

;; 开启 evil-nerd-commenter,它是一个快速注释和取消注释的插件
(evilnc-default-hotkeys)

;; 以下配置可以让emacs state下可以使用occur-mode的按键
(add-hook 'occur-mode-hook
          (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/") 'evil-search-forward
              (kbd "n") 'evil-search-nex
              (kbd "N") 'evil-search-previous
              (kbd "C-d") 'evil-scroll-down
              (kbd "C-u") 'evil-scroll-up
              )))

;; 如果你希望某个模式也能用occur-mode的按键,就指定它打开时为emacs state即可
;; 下面的配置可以指定哪些mode打开的时候使用emacs state
(dolist (mode '(ag-mode
                ;; flycheck-error-list-mode
                occur-mode
                git-rebase-mode
                dired-mode))
  (add-to-list 'evil-emacs-state-modes mode))
