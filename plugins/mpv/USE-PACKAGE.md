# Emacs use-package 管理动态模块

## 基础加载方式

### 直接 load
```elisp
;; 加载动态模块
(load "/path/to/mpv.so")

;; 或添加路径后加载
(add-to-list 'load-path "/path/to/")
(load "mpv")
```

### 使用 require
```elisp
;; 模块会自动 provide "mpv"
(require 'mpv)
```

---

## 使用 use-package 管理

### 基本配置
```elisp
(use-package mpv
  :load-path "/home/neo/.emacs.d/plugins/mpv/target/debug/")
```

### 完整配置
```elisp
(use-package mpv
  :load-path "/home/neo/.emacs.d/plugins/mpv/target/debug/"
  :config
  (message "mpv module loaded"))
```

### 延迟加载
```elisp
(use-package mpv
  :load-path "/home/neo/.emacs.d/plugins/mpv/target/debug/"
  :defer t)
```

---

## 实际配置示例

### 方式1：固定路径
```elisp
(use-package mpv
  :load-path "~/.emacs.d/plugins/mpv/target/debug/"
  :config
  (progn
    (message "mpv plugin ready")
    ;; 可选：绑定快捷键
    (global-set-key (kbd "C-c p") 'mpv-play)))
```

### 方式2：动态路径（推荐）
```elisp
(defvar mpv-module-path
  (expand-file-name "~/.emacs.d/plugins/mpv/target/debug/mpv.so"))

(use-package mpv
  :load-path (file-name-directory mpv-module-path)
  :demand t
  :config
  (message "mpv module loaded"))
```

### 方式3：条件加载
```elisp
(when (file-exists-p "~/.emacs.d/plugins/mpv/target/debug/mpv.so")
  (use-package mpv
    :load-path "~/.emacs.d/plugins/mpv/target/debug/"
    :demand t))
```

---

## 开发时热重载

### 重新加载模块
```elisp
(defun reload-mpv-module ()
  "Reload mpv dynamic module."
  (interactive)
  (unload-feature 'mpv 'force)
  (load "/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so")
  (message "mpv module reloaded"))
```

### 自动重新加载（开发用）
```elisp
(defun reload-mpv-on-change (&optional file)
  "Reload mpv module when .so file changes."
  (when (string-match-p "mpv\\.so$" (or file ""))
    (when (featurep 'mpv)
      (unload-feature 'mpv 'force))
    (load "/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so")
    (message "mpv module reloaded")))

;; 使用 watch 命令监听文件变化
;; watch exec -e /home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so -c "emacsclient -e '(reload-mpv-on-change)'")
```

---

## 完整配置模板

```elisp
;;;; mpv 模块配置

(defvar my-mpv-module-path
  (expand-file-name "~/.emacs.d/plugins/mpv/target/debug/mpv.so"))

(defvar my-mpv-enabled
  (file-exists-p my-mpv-module-path))

(when my-mpv-enabled
  (use-package mpv
    :load-path (file-name-directory my-mpv-module-path)
    :demand t
    
    ;; 加载后执行的配置
    :config
    (progn
      (message "mpv module loaded successfully")
      
      ;; 绑定快捷键
      (global-set-key (kbd "C-c p p") 'mpv-play)
      (global-set-key (kbd "C-c p s") 'mpv-stop)
      (global-set-key (kbd "C-c p .") 'mpv-pause)
      (global-set-key (kbd "C-c p ,") 'mpv-resume))
    
    ;; 自定义变量
    :custom
    (mpv-default-path "/home/neo/Videos/")))

;; 开发时重新加载命令
(defun my/reload-mpv-module ()
  "Reload mpv module from disk."
  (interactive)
  (when (featurep 'mpv)
    (unload-feature 'mpv 'force))
  (load my-mpv-module-path)
  (message "mpv module reloaded"))
```

---

## 常见问题

### 1. 模块加载但不工作
```elisp
;; 需要先 load 再 require
(load "/path/to/mpv.so")
(require 'mpv)

;; 或使用 :demand 强制加载
(use-package mpv
  :load-path "/path/to/"
  :demand t)
```

### 2. 路径问题
```elisp
;; 使用绝对路径
(expand-file-name "~/path/to/mpv.so")

;; 或使用 emacs.d 路径
(expand-file-name "plugins/mpv/target/debug/mpv.so"
                 user-emacs-directory)
```

### 3. 模块版本更新后
```elisp
;; 需要先卸载再重新加载
(unload-feature 'mpv 'force)
(load "/path/to/new/mpv.so")
```
