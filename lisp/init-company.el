(provide 'init-company)

;;========================================================================================
;;                                        company
;;========================================================================================
;; 全局加载company-mode
(global-company-mode t)
;; 补全时能识别简写，这个是说如果我写了 "import tensorflow as tf" ，那么我再输入 "tf." 的时候能自动补全
;; (setq jedi:use-shortcuts t)
;; 让补全列表里的各项左右对齐
(setq company-tooltip-align-annotations t)
;; 补全列表里的项按照使用的频次排序，这样经常使用到的会放在前面，减少按键次数
(setq company-transformers '(company-sort-by-occurrence))
;; 解决删除一个字符后补全提示就没有了的问题
(add-to-list 'company-begin-commands  'backward-delete-char-untabify)
(add-to-list 'company-begin-commands  'backward-kill-word)

;; Number the candidates (use M-1, M-2 etc to select completions).
;; (setq company-show-numbers t)
