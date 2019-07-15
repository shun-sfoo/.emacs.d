(provide 'init-ui)

;;========================================================================================
;;                        GUI模式下才有用,非GUI模式不要开启,会报错
;;========================================================================================

(when (memq window-system '(x))
  (progn
    (set-face-attribute 'default nil :font "Monaco" :height 110)
    (set-fontset-font t 'han "Noto Sancs CJK SC")))

(when (memq window-system '(w32))
  (progn
    (set-face-attribute 'default nil :font "Noto Mono" :height 110)
    (set-fontset-font t 'han "微软雅黑")))

;; 设置背景色
;; (set-background-color "black")

;;关闭工具栏
(tool-bar-mode -1)

;;关闭侧边滚动栏
(scroll-bar-mode -1)

;; 关闭菜单栏
(menu-bar-mode -1)

;; 关闭启动画面
(setq inhibit-splash-screen t)

;; 设置光标的样式为竖线
(setq-default cursor-type 'bar)

