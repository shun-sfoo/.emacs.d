(provide 'init-org)

;; Org 模式相关设定
(with-eval-after-load 'org
  ;; 让Org-mode中#+BEGIN_SRC块语法高亮
  
  (setq org-src-fontify-natively t)
  ;; Agenda功能,用它可以来记录你的日程安排
  (setq org-agenda-files '("~/.emacs.d/org"))

  ;; 使用此函数可以快速创建gtd.org文件
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/.emacs.d/org/gtd.org" "时间管理")
	   "* TODO [#B] %?\n  %i\n"
	   :empty-lines 1)))
  )
