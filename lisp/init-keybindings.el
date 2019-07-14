(provide 'init-keybindings)

;;========================================================================================
;;                    Evil leader key使用不同的键(前缀)将不同的操作分组
;;========================================================================================
(global-evil-leader-mode)
(evil-leader/set-key
  ;;==============================================================  
  ;;s 键(prefix)前缀 关于搜索(search)的操作
  ;;==============================================================  
  "ss"  'swiper                       ;;搜索当前buffer
  "so"  'occur-dwim                   ;;搜索并列出文档中所有匹配的字符串,按e可进行编辑,C-c C-c保存,q退出
  "si"  'iedit-mode                   ;;搜索匹配字符进行批量代码重构
  "sf"  'counsel-imenu                ;;搜索buffer中的函数,用于快速跳转
  "sg"  'counsel-git                  ;;搜索被git控制的文件
  "sp"  'counsel-ag                   ;;搜索整个工程下的文件
  
  ;;==============================================================  
  ;;f 键(prefix)前缀 关于文件(file)的操作
  ;;==============================================================  

  "ff"  'counsel-find-file            ;;查找并打开本地文件
  "fr"  'recentf-open-files           ;;打开常用文件列表
  "fei" 'open-my-init-file            ;;打开init.el
  "fek" 'open-my-keybinds-file        ;;打开init-keybindings.el
  "fep" 'open-my-packages-file        ;;打开init-packages.el
  "feu" 'open-my-ui-file              ;;打开init-ui.el
  "feb" 'open-my-built-file           ;;打开init-built-in-exts.el
  "fec" 'open-my-custom-file          ;;打开init-customize-groups.el
  "feo" 'open-my-org-file             ;;打开init-org.el
  "foo" 'dired-jump                   ;;打开dired

  ;;============================================================== 
  ;;o 键(prefix)前缀 关于Org-Mode的操作
  ;;==============================================================
  
  "oc" 'org-capture                  ;;打开org-capture
  "oa" 'org-agenda                   ;;打开org-agenda
  "oo" 'open-my-gtd-file             ;;打开gtd.org
  
  ;;==============================================================  
  ;;b 键(prefix)前缀 关于缓冲区(Buffer)的操作
  ;;==============================================================  

  "bb"  'helm-buffers-list            ;;打开buffer列表
  "bs"  'switch-to-buffer             ;;切换buffer
  "bk"  'kill-buffer                  ;;关闭buffer

  ;;==============================================================  
  ;;h 键(prefix)前缀 关于帮助(help)的操作
  ;;==============================================================  

  "hv" 'describe-variable             ;;查看变量的值
  "hf" 'describe-function             ;;查看函数绑定了什么按键
  "hk" 'describe-key                  ;;查看按键绑定了什么函数
  "hdf" 'find-function                ;;查看函数定义在哪个文件
  "hdv" 'find-variable                ;;查看变量定义在哪个文件
  "hdk" 'find-function-on-key         ;;查看按键绑定定义在哪个文件

  ;;==============================================================  
  ;;w 键(prefix)前缀 关于窗口(window)的操作
  ;;==============================================================  

  "wl"  'split-window-right           ;;水平分割窗口
  "wh"  'split-window-below           ;;垂直分割窗口
  "wo"  'other-window                 ;;切换其他窗口
  "w-"  'delete-window                ;;关闭当前窗口
  "w_"  'delete-other-windows         ;;关闭其他窗口
  "0"   'select-window-0              ;;切换0号窗口
  "1"   'select-window-1              ;;切换1号窗口
  "2"   'select-window-2              ;;切换2号窗口
  "3"   'select-window-3              ;;切换3号窗口
  "4"   'select-window-4              ;;切换4号窗口
  "5"   'select-window-5              ;;切换5号窗口
  "6"   'select-window-6              ;;切换6号窗口
  "7"   'select-window-7              ;;切换7号窗口
  "8"   'select-window-8              ;;切换8号窗口
  "9"   'select-window-9              ;;切换9号窗口

  ;;==============================================================  
  ;;F 键(prefix)前缀 关于格式(format)的操作
  ;;==============================================================  

  "FF"  'indent-region-or-buffer      ;;格式化整个buffer
  "FD"  'remove-dos-eol               ;;删除Windows下的换行符^M

  ;;==============================================================  
  ;;p 键(prefix)前缀 关于包(package)的操作
  ;;==============================================================  

  "pi"  'package-install              ;;安装package
  "pr"  'package-autoremove           ;;移除package
  "pl"  'package-list-packages        ;;列出package列表
  "px"  'customize-group              ;;定义package的customize-group选项

  ;;==============================================================  
  ;;g 键(prefix)前缀 关于Git的操作
  ;;==============================================================  

  ;;  "gi"  'magit-init                   ;;初始化git仓库
  ;;  "gs"  'magit-status                 ;;查看git状态
  
  ;;==============================================================  
  ;;U 键(prefix)前缀 关于更新相关的操作
  ;;==============================================================  

  ;; "Um"  'elpamr-create-mirror-for-installed  ;;更新本地melpa存储库
  
  ;;==============================================================  
  ;;other其他 键(prefix)前缀
  ;;==============================================================  

  "?"   'youdao-dictionary-search-at-point+     ;;有道翻译
  ":"   'counsel-M-x                  ;;增强M-x

  ;;==============================================================  
  ;;q 键(prefix)前缀 关于退出(quit)的操作
  ;;==============================================================  
  "qw"  'select-tags-table-quit       ;;退出当前窗口
  "qq"  'save-buffers-kill-terminal   ;;保存并退出
  )


;;========================================================================================
;;                                  Emacs bindings
;;========================================================================================
(global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)  ;;在Evil Mode normal state中使用 M-; 快速注释和取消注释
(global-set-key (kbd "C-w") 'backward-kill-word)                 ;;在Evil Mode insert state中使用 C-w 快速删除一个单词
(global-set-key (kbd "C-x C-f") 'counsel-find-file)              ;;替换emacs的C-x C-f键绑定
(global-set-key (kbd "M-x") 'counsel-M-x)                        ;;替换emacs的M-x绑定
;;(global-set-key (kbd "C-x C-b") 'helm-buffers-list)              ;;替换emacs的C-x C-b键绑定
(global-set-key (kbd "M-/") 'company-complete)                   ;;company-mode 补全快捷键
(global-set-key (kbd "C-a") 'er/expand-region)                   ;;快速区域选择
(global-set-key (kbd "C-c C-c") 'eshell)                         ;;打开eshell
