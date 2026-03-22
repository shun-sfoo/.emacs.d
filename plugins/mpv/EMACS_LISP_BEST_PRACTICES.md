# Emacs Lisp 现代最佳实践指南

> 基于 Emacs 28+ 的现代 Emacs Lisp 编程规范

## 目录

1. [词法作用域 (lexical-binding)](#1-词法作用域-lexical-binding)
2. [seq 库](#2-seq-库)
3. [defcustom 与 defgroup](#3-defcustom-与-defgroup)
4. [cl-defstruct 结构体](#4-cl-defstruct-结构体)
5. [autoload 延迟加载](#5-autoload-延迟加载)
6. [pcase 模式匹配](#6-pcase-模式匹配)
7. [define-minor-mode](#7-define-minor-mode)
8. [condition-case 错误处理](#8-condition-case-错误处理)
9. [命名空间前缀](#9-命名空间前缀)
10. [标准包头部](#10-标准包头部)

---

## 1. 词法作用域 (lexical-binding)

### 作用
词法作用域让闭包正确捕获变量值，避免动态作用域带来的意外修改。

### 旧写法（非最佳实践）

```elisp
;; 无 lexical-binding
(defun make-counter ()
  "创建计数器。" 
  (let ((count 0))
    (lambda () (setq count (1+ count)))))

;; 动态作用域下，闭包可能无法正确捕获变量
(setq counter (make-counter))
(funcall counter)  ; 结果不确定
```

### 新写法（最佳实践）

```elisp
;;; foo.el --- 示例  -*- lexical-binding: t; -*-

(defun make-counter ()
  "创建计数器。"
  (let ((count 0))
    (lambda ()
      (setq count (1+ count))
      count)))

(let ((counter (make-counter)))
  (funcall counter)  ; => 1
  (funcall counter)  ; => 2
  (funcall counter)) ; => 3
```

### 核心概念

```elisp
;; 词法作用域下，lambda 正确捕获外部变量
(defun make-adder (x)
  (lambda (y) (+ x y)))  ; x 被正确捕获

(let ((add5 (make-adder 5)))
  (funcall add5 10))  ; => 15

;; 替代动态作用域的变量查找
(defvar x 10)  ; 动态变量
(let ((x 5))   ; 词法作用域中 x=5，但 dynamic-var 仍指向全局
  ...)
```

### API

| 启用方式 | 说明 |
|---------|------|
| 文件头 | `;;; foo.el -*- lexical-binding: t; -*-` |
| 代码块 | `(let ((lexical-binding t)) ...)` |

---

## 2. seq 库

### 作用
Emacs 原生的序列操作库，替代 `cl-lib` 中的列表函数，更符合 Emacs 的序列抽象。

### 旧写法（非最佳实践）

```elisp
(require 'cl)  ; 或 (require 'cl-lib)

;; 使用 cl 的 mapcar
(mapcar #'1+ '(1 2 3))           ; => (2 3 4)

;; 使用 cl 的 remove-if
(cl-remove-if #'evenp '(1 2 3 4)) ; => (1 3)

;; 使用 cl 的 reduce
(cl-reduce #'+ '(1 2 3 4))        ; => 10

;; 使用 cl 的 every/some
(cl-every #'numberp '(1 2 3))    ; => t
(cl-some #'oddp '(1 2 3))         ; => t
```

### 新写法（最佳实践）

```elisp
(require 'seq)

;; 基本映射
(seq-map #'1+ '(1 2 3))                    ; => (2 3 4)
(seq-map-indexed (lambda (elt idx) (list idx elt)) '(a b c))
                                          ; => ((0 a) (1 b) (2 c))

;; 过滤
(seq-filter #'evenp '(1 2 3 4))            ; => (2 4)
(seq-remove #'evenp '(1 2 3 4))            ; => (1 3)

;; 归约
(seq-reduce #'+ '(1 2 3 4) 0)              ; => 10
(seq-reduce (lambda (acc x) (cons x acc)) '(1 2 3) '())
                                          ; => (3 2 1)

;; 判断
(seq-every-p #'numberp '(1 2 3))           ; => t
(seq-some #'oddp '(1 2 3))                 ; => t
(seq-empty-p '())                         ; => t

;; 查找
(seq-find #'evenp '(1 2 3 4))              ; => 2
(seq-position '(a b c) 'b)                 ; => 1
(seq-contains-p '(a b c) 'b)              ; => t

;; 取子序列
(seq-take '(1 2 3 4 5) 3)                  ; => (1 2 3)
(seq-drop '(1 2 3 4 5) 2)                  ; => (3 4 5)
(seq-subseq '(1 2 3 4 5) 1 3)             ; => (2 3)

;; 分割
(seq-partition '(1 2 3 4 5 6) 2)          ; => ((1 2) (3 4) (5 6))
(seq-group-by #'evenp '(1 2 3 4 5 6))     ; => ((nil 1 3 5) (t 2 4 6))

;; 去重
(seq-distinct '(1 2 2 3 1 4))              ; => (1 2 3 4)

;; 组合
(seq-concatenate 'list '(1 2) [3 4] "56")  ; => (1 2 3 4 ?5 ?6)
(seq-union '(a b) '(b c))                  ; => (a b c)
(seq-intersection '(a b c) '(b c d))      ; => (b c)
(seq-difference '(a b c) '(b))             ; => (a c)
```

### seq 与 cl-lib 对照表

| seq 函数 | cl-lib 对应 | 说明 |
|---------|------------|------|
| `seq-map` | `cl-mapcar` | 映射 |
| `seq-filter` | `cl-remove-if` | 过滤 |
| `seq-reduce` | `cl-reduce` | 归约 |
| `seq-every-p` | `cl-every` | 全满足 |
| `seq-some` | `cl-some` | 存在满足 |
| `seq-find` | `cl-find` | 查找 |
| `seq-flatten` | - | 扁平化 |
| `seq-uniq` | `cl-remove-duplicates` | 去重 |
| `seq-sort` | `cl-sort` | 排序 |
| `seq-partition` | - | 分组 |

### 字符串/向量专用函数

```elisp
;; 字符串操作
(seq-map #'string-to-number '("1" "2" "3"))  ; => (1 2 3)
(seq-includes? "hello" ?l)                   ; => t (Emacs 29+)

;; 向量操作
(seq-into [1 2 3] 'list)                     ; => (1 2 3)
(seq-into '(1 2 3) 'vector)                  ; => [1 2 3]
```

---

## 3. defcustom 与 defgroup

### 作用
提供标准化的用户定制接口，与 Emacs 的 `customize` 系统集成。

### 旧写法（非最佳实践）

```elisp
(defvar my-plugin-auto-save t
  "Whether to auto save.")

(defvar my-plugin-timeout 30
  "Timeout in seconds.")

;; 用户必须手动设置
(setq my-plugin-timeout 60)
```

### 新写法（最佳实践）

```elisp
(defgroup my-plugin nil
  "My awesome plugin."
  :group 'tools
  :link '(url-link "https://github.com/user/my-plugin")
  :prefix "my-plugin-")

(defcustom my-plugin-auto-save t
  "Whether to automatically save changes."
  :type 'boolean
  :group 'my-plugin
  :safe #'booleanp)

(defcustom my-plugin-timeout 30
  "Timeout in seconds."
  :type 'integer
  :group 'my-plugin
  :safe #'integerp)

(defcustom my-plugin-mode 'normal
  "Operation mode."
  :type '(choice (const :tag "Normal" normal)
                 (const :tag "Quiet" quiet)
                 (const :tag "Debug" debug))
  :group 'my-plugin)

(defcustom my-plugin-exclude-list '("*.tmp")
  "Files to exclude."
  :type '(repeat string)
  :group 'my-plugin)

(defcustom my-plugin-keymap (make-sparse-keymap)
  "Keymap for my-plugin."
  :type 'keymap
  :group 'my-plugin)

(defcustom my-plugin-hook nil
  "Hook run after activation."
  :type 'hook
  :group 'my-plugin)
```

### defcustom 类型系统

```elisp
;; 基本类型
:type 'boolean     ; t 或 nil
:type 'integer     ; 整数
:type 'number       ; 数字
:type 'float       ; 浮点数
:type 'string      ; 字符串
:type 'file        ; 文件路径
:type 'directory   ; 目录路径
:type 'function    ; 函数
:type 'symbol      ; 符号
:type 'sexp        ; 任意 S 表达式

;; 复杂类型
:type '(choice (const :tag "Option A" a)
               (const :tag "Option B" b)
               (string :tag "Custom"))       ; 多选一

:type '(repeat integer)                       ; 整数列表
:type '(alist :key-type string :value-type symbol)  ; 关联列表
:type '(plist :key-type symbol :value-type t) ; 属性列表

:type '(radio :tag "Choose"
              (const :tag "Fast" fast)
              (const :tag "Slow" slow))

:type '(set (const :tag "Verbose" verbose)
            (const :tag "Debug" debug)
            (const :tag "Quiet" quiet))
```

### 高级用法

```elisp
(defcustom my-plugin-colors
  '((foreground . "white")
    (background . "black"))
  "Color configuration."
  :type '(alist :key-type (symbol :tag "Key")
                :value-type (string :tag "Value"))
  :group 'my-plugin)

(defcustom my-plugin-style
  '((indent-tabs-mode . nil)
    (fill-column . 80))
  "Style settings."
  :type '(alist :key-type symbol
                :value-type (choice (boolean :tag "Boolean")
                                    (integer :tag "Integer")
                                    (string :tag "String")))
  :group 'my-plugin)

;; 带 setter 的自定义
(defcustom my-plugin-directory
  (expand-file-name "my-plugin" user-emacs-directory)
  "Plugin data directory."
  :type 'directory
  :group 'my-plugin
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'my-plugin--on-setting-change)
           (my-plugin--on-setting-change val))))
```

---

## 4. cl-defstruct 结构体

### 作用
定义结构化数据类型，替代笨拙的关联列表访问方式。

### 旧写法（非最佳实践）

```elisp
;; 使用alist模拟结构
(defun make-person (name age)
  (list :name name :age age))

(defun person-name (p)
  (plist-get p :name))

(defun person-age (p)
  (plist-get p :age))

(defun person-set-age (p age)
  (plist-put p :age age))

;; 问题：类型不安全，无编译时检查，容易写错
(person-name '(:name "Alice" :age 30))  ; OK
(person-name '(:age 30 :name "Bob"))     ; 无警告
```

### 新写法（最佳实践）

```elisp
(require 'cl-lib)

;; 基本结构体
(cl-defstruct person
  name age occupation)

(defvar alice (make-person :name "Alice" :age 30 :occupation "Engineer"))

(person-name alice)              ; => "Alice"
(person-age alice)               ; => 30
(person-p alice)                 ; => t (类型检查)

(setf (person-age alice) 31)     ; 修改字段
(person-age alice)               ; => 31

(copy-person alice)              ; 复制结构体
```

### 高级结构体

```elisp
;; 带默认值的字段
(cl-defstruct window-config
  (width 80 :read-only t)
  (height 24 :read-only t)
  title
  (bg-color "black" :read-only t))

(make-window-config :title "Terminal")
;; => #s(window-config 80 24 "Terminal" "black")

;; 命名构造函数
(cl-defstruct (empv-track (:constructor empv-track--create)
                          (:constructor empv-track-new))
  (file nil :type string)
  (title nil :type (or null string))
  (duration nil :type (or null number))
  (artist nil :type (or null string))
  (album nil :type (or null string)))

(empv-track-new :file "/path/to/song.mp3" :title "Song")
(empv-track--create :file "/path/to/song.mp3" :title "Song")  ; 两种构造方式

;; 类型限制
(cl-defstruct validated-number
  (value 0 :type number)
  (precision 2 :type (integer 0 10)))

;; 继承
(cl-defstruct (employee (:include person))
  salary department)

(defvar bob (make-employee :name "Bob" :age 25 :salary 50000 :department "IT"))

(person-name bob)    ; => "Bob"  (继承自 person)
(employee-salary bob) ; => 50000
```

### 结构体与 alist/plist 对照

| 操作 | 结构体 | alist | plist |
|------|--------|-------|-------|
| 创建 | `make-xxx` | `list` | `list` |
| 读取 | `xxx-field` | `alist-get` | `plist-get` |
| 写入 | `setf` | `setf` | `setf` |
| 类型检查 | `xxx-p` | 无 | 无 |
| 编译检查 | 有 | 无 | 无 |
| 性能 | 略快 | 略慢 | 略慢 |

---

## 5. autoload 延迟加载

### 作用
延迟加载代码直到真正需要，显著加快 Emacs 启动速度。

### 旧写法（非最佳实践）

```elisp
(require 'my-plugin)  ; 启动时立即加载全部代码

;; 放入 init.el，导致每次启动都加载
(setq my-plugin-enabled t)
```

### 新写法（最佳实践）

```elisp
;;; my-plugin.el ---

(defgroup my-plugin nil ...)

(defcustom my-plugin-mode nil
  "Enable my-plugin mode."
  :type 'boolean
  :group 'my-plugin
  :set (lambda (sym val)
         (set-default sym val)
         (if val
             (my-plugin--enable)
           (my-plugin--disable))))

;; 使用 autoload 声明
;;;###autoload
(defun my-plugin-command ()
  "My plugin main command."
  (interactive)
  (require 'my-plugin)  ; 内部再 require
  (my-plugin--do-something))

;;;###autoload
(define-minor-mode my-plugin-mode
  "Toggle my-plugin mode."
  :lighter " my"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c p") #'my-plugin-command)
            map))

;; 批量 autoload
;;;###autoload (autoload 'my-plugin-mode "my-plugin" nil t)
;;;###autoload (autoload 'my-plugin-command "my-plugin" nil t)

;; 或使用 generate-file-autoloads 生成 autoload 文件
```

### magic.autoload 文件

```elisp
;; my-plugin-pkg.el (或 my-plugin-autoloads.el)
;;; Code:
(add-to-list 'load-path (directory-file-name (file-name-directory #$)))
(add-to-list 'generated-autoload-file (concat (file-name-directory load-file-name)
                                              "my-plugin-autoloads.el"))
(autoload 'my-plugin-mode "my-plugin" nil t)
(autoload 'my-plugin-command "my-plugin" nil t)
```

### 实际使用

```elisp
;; 在 loaddefs.el 或 _loaddefs.el 中自动生成
;; M-x generate-file-autoloads

;; 用户配置
(use-package my-plugin
  :commands (my-plugin-mode my-plugin-command)
  :hook (after-init . my-plugin-mode))
```

### autoload 技巧

```elisp
;; 条件 autoload
;;;###autoload
(autoload 'company-mode "company" nil t)

;; 懒加载宏
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

;; 动态加载依赖
;;;###autoload
(defun my-plugin-smart-command ()
  (interactive)
  (unless (featurep 'dash)  ; 检查特性是否已加载
    (require 'dash))
  (my-plugin--do-something))
```

---

## 6. pcase 模式匹配

### 作用
比 `cond` 更强大的条件分支，支持结构化模式匹配。

### 旧写法（非最佳实践）

```elisp
;; 使用 cond 和类型检查
(cond
 ((and (listp x) (= (length x) 3))
  (let ((a (car x)) (b (cadr x)) (c (caddr x)))
    (+ a b c)))
 ((and (vectorp x) (= (length x) 3))
  (apply #'+ (append x nil)))
 ((symbolp x)
  (symbol-value x))
 (t (error "Unknown type")))

;; 使用 cl-case（只能匹配字面量）
(cl-case command
  ('start (do-start))
  ('stop (do-stop))
  ('restart (do-restart))
  (t (error "Unknown command")))
```

### 新写法（最佳实践）

```elisp
(require 'subr-x)  ; for pcase

;; 基本模式匹配
(pcase (get-something)
  ('nil "Got nil")
  ('t "Got t")
  ((pred numberp) "Got a number")
  ((pred stringp) "Got a string")
  (_ "Got something else"))

;; 匹配常量和值
(pcase status
  ('idle "Not doing anything")
  ('running "In progress")
  ('done "Completed")
  ('failed "Something went wrong")
  (_ (format "Unknown status: %S" status)))

;; 解构列表
(pcase-let (([first second third] '(1 2 3)))
  (+ first second third))  ; => 6

(pcase '(1 2 3)
  (`(,a ,b ,c) (+ a b c))     ; => 6
  (`(,a ,b) (+ a b))
  (_ "Other"))

;; 解构关联列表
(pcase-let ((((name . name-val) (age . age-val)) '((name . "Alice") (age . 30))))
  (format "%s is %d years old" name-val age-val))
; => "Alice is 30 years old"

;; 模式 guard
(pcase (get-value)
  ((and (pred numberp) (guard (> 10))) "Big number")
  ((and (pred numberp) (guard (< 10))) "Small number")
  (_ "Not a big or small number"))

;; 绑定变量
(pcase '(1 "hello")
  (`(,n ,(pred stringp)) (format "%d: %s" n (match-string 1 "hello"))))

;; 绑定复杂结构
(pcase-let* ((data '((user . ((name . "Alice") (age . 30)))
                    (status . active))))
  (pcase data
    (`((user . ((name . ,name) (age . ,age)))
       (status . ,status))
     (format "%s (%d) - %s" name age status))))
;; => "Alice (30) - active"

;; 匹配类型
(pcase-dolist (`(,type . ,value) '((number . 42)
                                    (string . "hello")
                                    (symbol . quote)))
  (pcase type
    ('number (message "Number: %d" value))
    ('string (message "String: %s" value))
    ('symbol (message "Symbol: %S" value))))
```

### pcase 模式类型

| 模式 | 示例 | 说明 |
|-----|------|------|
| `quote` | `'foo` | 匹配字面量 |
| `pred` | `(pred evenp)` | 匹配谓词 |
| `guard` | `(guard (> n 10))` | 额外条件 |
| `app` | `(app car 1)` | 应用函数后匹配 |
| `and` | `(and (pred numberp) ,n)` | 组合 |
| `or` | `(or `(+ ,a) `(- ,a))` | 或模式 |
| `let` | `(let ,val)` | 绑定值 |
| `map` | `(map :name ,name)` | 匹配 plist 键 |
| `assoc` | `(assoc 'key ,val)` | 匹配 alist |
| `rx` | `(rx "foo" (group (+ digit)))` | 正则匹配 |
| `backquote` | <code>`(,a ,b)</code> | 解构列表 |

### 宏包装

```elisp
(defmacro pcase-case (expr &rest cases)
  "Enhanced pcase with better error reporting."
  (declare (debug (form &rest (pcase-PAT body))))
  `(pcase ,expr
     ,@cases
     (_ (error "Unhandled case: %S" ,expr))))

(pcase-case (get-status)
  ('success (format "Result: %S" result))
  ('error (format "Error: %S" error-msg))
  (_ (error "Unknown status")))
```

---

## 7. define-minor-mode

### 作用
标准化的 minor mode 定义，自动提供切换命令和正确的事件处理。

### 旧写法（非最佳实践）

```elisp
(defvar my-mode nil)
(make-variable-buffer-local 'my-mode)

(defun my-mode-on ()
  (unless my-mode
    (setq my-mode t)
    (run-hooks 'my-mode-hook)
    (my-mode--setup)))

(defun my-mode-off ()
  (when my-mode
    (setq my-mode nil)
    (my-mode--cleanup)))

(defun my-mode-toggle ()
  (interactive)
  (if my-mode
      (my-mode-off)
    (my-mode-on)))

(define-minor-mode my-mode
  "My minor mode.")
```

### 新写法（最佳实践）

```elisp
(require 'easymenu)  ; 如果需要菜单

(define-minor-mode my-mode
  "Toggle my-mode.
With prefix ARG, enable my-mode if ARG is positive,
otherwise disable.
\\{my-mode-map}"
  :lighter " my"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m") #'my-mode-command)
            (define-key map (kbd "C-c M") #'my-mode-other-command)
            map)
  :group 'my-mode
  :require 'my-mode
  :interactive (lambda ()
                 (list (or current-prefix-arg 'toggle)))
  :after-hook (run-hooks 'my-mode-hook))

;; 如果需要菜单
(easy-menu-define my-mode-menu my-mode-map
  "My mode menu."
  '("My Mode"
    ["Command A" my-mode-command]
    ["Command B" my-mode-other-command]
    "--"
    ["Customize..." (customize-group 'my-mode)]))
```

### major-mode 模板

```elisp
(define-derived-mode my-major-mode prog-mode
  "MyMode"
  "Major mode for MyMode files."
  :syntax-table my-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '(my-mode-font-lock-rules))
  (setq-local imenu-generic-expression my-mode-imenu-alist)
  (when (fboundp 'treesit-available-p)
    (when (treesit-ready-p 'my-lang)
      (my-major-mode--setup-treesit))))
```

### mode hook 约定

```elisp
;; 标准 hook 命名
(defvar my-mode-hook nil
  "Hook run after entering my-mode.")

(defvar my-mode-exit-hook nil
  "Hook run after exiting my-mode.")

;; 在 enable/disable 中调用
(define-minor-mode my-mode
  "Toggle my-mode."
  :lighter " my"
  :keymap my-mode-map
  (if my-mode
      (progn
        (my-mode--enable)
        (run-hooks 'my-mode-hook))
    (my-mode--disable)
    (run-hooks 'my-mode-exit-hook)))
```

---

## 8. condition-case 错误处理

### 作用
结构化的错误处理，比简单的 `ignore-errors` 更精细。

### 旧写法（非最佳实践）

```elisp
;; 忽略所有错误
(ignore-errors
  (do-something)
  (do-something-else))

;; 没有错误处理
(setq result (some-function arg))
;; 如果出错，整个 Emacs 可能崩溃
```

### 新写法（最佳实践）

```elisp
;; 基本用法
(condition-case err
    (progn
      (validate-input)
      (do-something)
      (do-something-else))
  (error
   (message "Error: %s" (error-message-string err))
   nil))

;; 多类型处理
(condition-case-unless-debug err
    (risky-operation)
  (file-error
   (message "File not found: %s" (car (cdr err)))
   nil)
  (network-error
   (message "Network issue: %s" (error-message-string err))
   nil)
  (error
   (message "Unexpected error: %s" (error-message-string err))
   nil))

;; 捕获特定错误
(condition-case err
    (let ((num (string-to-number user-input)))
      (when (zerop num)
        (signal 'my-plugin-invalid-input (list user-input)))
      (/ 100 num))
  (my-plugin-invalid-input
   (message "Invalid input: %s" (cadr err)))
  (arith-error
   (message "Division by zero"))
  (error
   (message "Error: %s" (error-message-string err))))

;; 捕获并恢复
(condition-case err
    (with-temp-buffer
      (insert-file-contents "huge-file.txt")
      (process-large-file (buffer-string)))
  (file-error
   (message "Could not read file")
   nil)
  (buffer-file-too-large
   (message "File is too large, processing in chunks...")
   (process-file-in-chunks "huge-file.txt")))

;; 使用 unwind-protect 清理
(let ((temp-file (make-temp-file "my-plugin-")))
  (unwind-protect
      (condition-case err
          (progn
            (write-to-temp temp-file)
            (process temp-file))
        (error
         (message "Processing failed: %s" err)
         nil))
    (when (file-exists-p temp-file)
      (delete-file temp-file))))
```

### 信号与错误

```elisp
;; 定义自定义错误类型
(define-error 'my-plugin-error "Generic my-plugin error")
(define-error 'my-plugin-invalid-input "Invalid input" 'my-plugin-error)
(define-error 'my-plugin-timeout "Operation timed out" 'my-plugin-error)

;; 抛出错误
(signal 'my-plugin-invalid-input '("invalid value"))

;; 使用 debug 命令
(condition-case err
    (progn
      (validate-input)
      (risky-code))
  (debug
   (signal (car err) (cdr err)))  ; 重新抛出，进入 debugger
  (error
   (message "Caught: %s" err)))
```

### pcase + error

```elisp
(pcase (condition-case err
           ( risky-operation )
         (error (list 'error (error-message-string err))))
  (`(error ,msg) (message "Failed: %s" msg))
  (`(ok ,result) (format "Success: %S" result))
  (_ "Unknown state"))
```

---

## 9. 命名空间前缀

### 作用
避免命名冲突，符合 Emacs 包命名规范。

### 旧写法（非最佳实践）

```elisp
;; 无前缀，容易冲突
(defvar mode nil)
(defvar enable nil)
(defvar hook nil)

(defun setup () ...)
```

### 新写法（最佳实践）

```elisp
(defgroup my-plugin nil
  "My plugin."
  :prefix "my-plugin-")

(defcustom my-plugin-mode nil
  "Enable my-plugin mode."
  :group 'my-plugin
  :prefix "my-plugin-")

(defvar my-plugin--internal nil
  "Internal state (单破折号 = buffer-local 或内部)")

(defvar my-plugin-alist nil
  "Public variable (双破折号 = 内部实现)")

(defun my-plugin-command ()
  "Public function."
  (my-plugin--internal-helper))

(defun my-plugin--internal-helper ()
  "Internal helper (双破折号).")

(cl-defstruct my-plugin--state
  "Internal state structure."
  count active)

(defvar my-plugin-state nil
  "Current state object.")
```

### 命名约定

| 前缀类型 | 示例 | 用途 |
|---------|------|------|
| `package-` | `my-plugin-enable` | 公开 API |
| `package--` | `my-plugin--internal` | 内部实现 |
| `package-/` | `my-plugin/config-` | 子模块/配置 |
| `:package` | `:tag` | 关键字（org-mode 风格） |

### 使用 cl-lib 命名

```elisp
(require 'seq)

;; 使用 seq- 而非 cl-
(seq-map #'1+ '(1 2 3))      ; 推荐
(cl-mapcar #'1+ '(1 2 3))    ; 避免

;; 自定义时使用前缀
(defun my-plugin-seq-sorted-by (pred seq)
  "Sort SEQ by PRED."
  (seq-sort pred seq))
```

---

## 10. 标准包头部

### 作用
符合 GNU ELPA/MELPA 规范的元数据，便于包管理器解析。

### 完整模板

```elisp
;;; my-plugin.el --- Brief description  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Author Name

;; Author: Author Name <author@example.com>
;; Maintainer: Author Name <author@example.com>
;; URL: https://github.com/username/my-plugin
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (seq "3.0"))
;; Created: 2024-01-15
;; Keywords: convenience multimedia
;; License: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; {{{1 安装
;; M-x package-install RET my-plugin RET
;;
;; {{{1 用法
;; M-x my-plugin-command

;; {{{1 配置示例
;; (use-package my-plugin
;;   :config
;;   (setq my-plugin-option t))

;;; Code:

;; ... 代码 ...

(provide 'my-plugin)

;;; my-plugin.el ends here
```

### 头部字段说明

| 字段 | 说明 | 示例 |
|------|------|------|
| `;; -*- lexical-binding: t; -*-` | 词法作用域 | 必须 |
| `URL` | 项目地址 | `https://...` |
| `Version` | 语义版本 | `1.0.0` |
| `Package-Requires` | 依赖 | `((emacs "28.1"))` |
| `Keywords` | 分类 | `convenience tools` |
| `License` | 许可证 | `GPL-3.0-or-later` |

### use-package 集成

```elisp
;; 使用 use-package 声明依赖
(use-package my-plugin
  :straight (my-plugin :type git :host github :repo "user/my-plugin")
  :commands (my-plugin-mode my-plugin-command)
  :bind ("C-c p" . my-plugin-command)
  :hook (after-init . my-plugin-mode)
  :custom (my-plugin-option t)
  :config (my-plugin--setup)
  :diminish my-plugin-mode)
```

---

## 综合示例

```elisp
;;; empv.el --- Modern mpv media player  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Author

;; Author: Author <author@example.com>
;; Maintainer: Author <author@example.com>
;; URL: https://github.com/author/empv.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (seq "3.0"))
;; License: GPL-3.0-or-later

;;; Commentary:
;; Modern mpv media player interface for Emacs.

;;; Code:
(eval-when-compile
  (require 'seq)
  (require 'subr-x))

(defgroup empv nil
  "Modern mpv media player."
  :group 'multimedia)

(defcustom empv-mpv-path
  (or (executable-find "mpv") "mpv")
  "Path to mpv executable."
  :type 'file
  :group 'empv)

(defcustom empv-startup-args
  '("--idle=yes")
  "Arguments passed to mpv."
  :type '(repeat string)
  :group 'empv)

(cl-defstruct (empv--track (:constructor empv--track-create))
  (file nil :type string)
  (title nil :type (or null string))
  (duration nil :type (or null number)))

(defvar empv--process nil)
(defvar empv--current-track nil)

;;;###autoload
(define-minor-mode empv-mode
  "Minor mode for empv controls."
  :lighter " empv"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "SPC") #'empv-pause)
            map))

(defun empv-pause ()
  "Toggle pause."
  (interactive)
  (message "Pause toggled"))

(provide 'empv)

;;; empv.el ends here
```

---

## 最佳实践速查表

| 实践 | 优先级 | Emacs 版本 |
|------|--------|-----------|
| `lexical-binding: t` | 必须 | 24.1+ |
| `seq` 库 | 推荐 | 25.1+ |
| `defcustom`/`defgroup` | 必须 | 所有 |
| `cl-defstruct` | 推荐 | 24.3+ |
| `autoload` | 推荐 | 所有 |
| `pcase` | 推荐 | 24.1+ |
| `define-minor-mode` | 必须 | 所有 |
| 命名前缀 | 必须 | 所有 |
| 标准头部 | 必须 | 所有 |
