(provide 'init-packages)

;;========================================================================================
;;      基础快捷键说明: M (Alt) C (Ctrl) S(shift) s(super) M-x (Alt+x) C-x (Ctrl+x)
;;========================================================================================
;;                                  插件管理器配置
;;========================================================================================
;;
;;  M-x package-list-packages 列出插件列表
;;  i  - 选择要安装的包
;;  d  - 选择要删除的包
;;  U  - 升级已安装的包
;;  x  - 执行操作
;;  M-x package－install 插件名(这种方法也可以安装插件)
;;  C-s 搜索插件,再次按则向下继续搜索
;;  M-x package-autoremove 自动移除不需要的插件
;;========================================================================================
(require 'cl)

;; 配置elpa和melpa源
(when (>= emacs-major-version 24)
  ;; emacs-china 源
  (add-to-list 'package-archives '("gnu-elpa-china" . "https://elpa.emacs-china.org/gnu/") t)
  (add-to-list 'package-archives '("melpa-china" . "https://elpa.emacs-china.org/melpa/") t)
  ;; 清华源
  (add-to-list 'package-archives '("melpa-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
  (add-to-list 'package-archives '("org-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/") t)
  (add-to-list 'package-archives '("gnu-cn" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") t)
  ;; 鹅厂
  (add-to-list 'package-archives '("melpa-qq" . "http://mirrors.cloud.tencent.com/elpa/melpa/") t)
  (add-to-list 'package-archives '("org-qq" . "http://mirrors.cloud.tencent.com/elpa/org/") t)
  (add-to-list 'package-archives '("gnu-qq" . "http://mirrors.cloud.tencent.com/elpa/gnu/") t)
  ;; 网易源 
  (add-to-list 'package-archives '("melpa-163" . "http://mirrors.163.com/elpa/melpa/") t)
  (add-to-list 'package-archives '("org-163" . "http://mirrors.163.com/elpa/org/") t)
  (add-to-list 'package-archives '("gnu-163" . "http://mirrors.163.com/elpa/gnu/") t)
  )

;; 列表中定义的插件,如果没安装则自动安装
(defvar jojo/packages '(
                        doom-themes
                        auto-yasnippet
                        company
                        ;;company-anaconda
                        counsel
                        evil
                        evil-leader
                        evil-nerd-commenter
                        evil-surround
                        exec-path-from-shell
                        expand-region
                        flycheck
                        ;;htmlize
                        hungry-delete
                        iedit
                        ;;js2-mode
                        org-bullets
                        org-pomodoro
                        popwin
                        ;;py-autopep8
                        ;;reveal-in-osx-finder
                        smartparens
                        swiper
                        ;;web-mode
                        which-key
                        window-numbering
                        yasnippet
                        youdao-dictionary
                        ;;magit
                        ;;emmet-mode
                        company-quickhelp
                        pyim
                        dashboard
                        all-the-icons
                        markdown-mode
                        racket-mode
                        rainbow-delimiters
                        doom-modeline
                        diredfl ;;colorful dired mode
                        all-the-icons-dired
                        ;;company-tabnine
                        ;;projectile
                        ) "Default packages")
(setq package-selected-packages jojo/packages)
(defun jojo/packages-installed-p ()
  (loop for pkg in jojo/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))
(unless (jojo/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg jojo/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;========================================================================================
;;                               优化org-mode标题样式的插件
;;========================================================================================
(add-hook 'org-mode-hook 'org-bullets-mode)

;;========================================================================================
;;                                   快速切换窗口插件
;;========================================================================================
(window-numbering-mode t)

;;========================================================================================
;;                                 快速删除行尾空格的插件
;;========================================================================================
(global-hungry-delete-mode)

;;========================================================================================
;;                            swiper是一款能够增强搜索功能的插件
;;========================================================================================
(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;;========================================================================================
;;           smartparens是一款自动匹配符号的插件,当你输入左边的自动生成右边的
;;========================================================================================
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
(sp-local-pair 'org-mode "'" nil :actions nil)

;;========================================================================================
;;                 当是Mac OS系统的时候,开启exec-path-from-shell这个插件
;;========================================================================================
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;========================================================================================
;;               当打开一个新窗口时,自动移动到该新窗口的插件,按q可以退出
;;========================================================================================
(require 'popwin)
(popwin-mode t)

;;========================================================================================
;;                            org-pomodoro番茄时间管理法的插件
;;========================================================================================
(require 'org-pomodoro)

;;========================================================================================
;;                                     配置YASnippet
;;========================================================================================
;; YASnippet 是一个代码块补全的插件,使用下面的配置文件将其在所有的编程语言的模式中激活
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)  ;; 对于所有编程语言开启

;;========================================================================================
;;            which-key 可以显示当前组合键下所有可以使用的全部组合键的选项
;;========================================================================================
;; 使用这个插件可以很好的解决快捷键太多的问题,我们无需在记忆任何快捷键
;; 而是根据自己的需求使用不同分组的快捷键后再在其中去需找自己需要的功能
;; 然后当我们按下一个组合键的前缀时,会列出该组合键下绑定的所有按键
(which-key-mode t)

;;========================================================================================
;;                                 快速显示帮助文档的插件
;;========================================================================================
(company-quickhelp-mode)

;;=========================================================================================
;;   如果是MAC OSX 系统,想要在finder中打开当前文件,直接输入M-x reveal-in-osx-finder即可
;;=========================================================================================


;;========================================================================================
;;                               dashboard 相关配置
;;========================================================================================
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-center-content t)

(setq dashboard-startup-banner 3)

;;========================================================================================
;;                               all-the-icons 相关配置
;;========================================================================================
(require 'all-the-icons)
;;========================================================================================
;;                              markdown-mode 相关配置
;;========================================================================================
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;;========================================================================================
;;                              doom-modeline 相关配置 
;;========================================================================================
(require 'doom-modeline)
(doom-modeline-mode 1)

(add-hook 'dired-mode-hook 'diredfl-mode)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
