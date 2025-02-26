(setq package-enable-at-startup nil)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)
(menu-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; So we can detect this having been loaded
(provide 'early-init)
