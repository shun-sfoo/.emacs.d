(provide 'init-chinese)
;;========================================================================================
;;                               pyim 相关配置
;;========================================================================================
(require 'pyim)
(require 'pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置
(setq default-input-method "pyim")
;; 使用全拼
(setq pyim-default-scheme 'quanpin)
