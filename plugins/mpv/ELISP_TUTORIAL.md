# Elisp 编程指南

> 基于 empv.el 代码实例讲解 Elisp 编程

## 目录

1. [基础语法](#1-基础语法)
2. [数据结构](#2-数据结构)
3. [常用函数](#3-常用函数)
4. [流程控制](#4-流程控制)
5. [作用域和闭包](#5-作用域和闭包)
6. [进程通信](#6-进程通信)
7. [JSON 处理](#7-json-处理)
8. [正则表达式](#8-正则表达式)
9. [高级特性](#9-高级特性)
10. [empv.el 完整解析](#10-empvel-完整解析)

---

## 1. 基础语法

### 1.1 括号与前缀表达式

Elisp 使用**括号**包围一切，采用**前缀表达式**（运算符在操作数前面）：

```elisp
;; 基本算术
(+ 1 2)              ; => 3  (相当于 1 + 2)
(- 5 3)              ; => 2  (相当于 5 - 3)
(* 2 3)              ; => 6  (相当于 2 * 3)
(/ 10 3)             ; => 3  (整数除法)
(% 10 3)             ; => 1  (取模)

;; 多个操作数
(+ 1 2 3 4)          ; => 10 (1 + 2 + 3 + 4)

;; 嵌套表达式
(* 2 (+ 3 4))        ; => 14 (2 * (3 + 4))
(- (* 3 5) (+ 1 2))  ; => 12 ((3 * 5) - (1 + 2))
```

### 1.2 定义变量

```elisp
;; defvar - 定义全局变量
(defvar empv--process nil)           ; 初始值为 nil

(defvar empv--socket-path nil)       ; Unix socket 路径

;; 带文档字符串
(defvar empv--command-id 0
  "Command ID counter for IPC requests.")  ; 变量说明

;; defvar-local - 定义 buffer-local 变量
;; 每个 buffer 有独立的值，互不影响
(defvar-local empv-subtitle-video-path nil
  "Current video path for subtitle tracking.")
```

### 1.3 定义函数

```elisp
;; 基本函数定义
(defun function-name (arg1 arg2)
  "Documentation string describing the function."  ; docstring
  (body...))

;; 示例
(defun empv-pause ()
  "Pause playback."
  (interactive)              ; M-x 可执行此函数
  (empv--send-command (vector "set" "pause" "yes") (lambda (_ _))))

;; 带参数
(defun empv-play (path)
  "Play video file PATH."
  (interactive "fVideo file: ")  ; interactive 声明参数
  ;; "f" = file, 提示用户选择文件
  ...)

;; 多参数 + 前缀参数
(defun empv-play-with-subtitle (video-path &optional external-only)
  "Play VIDEO-PATH and load subtitle.
With EXTERNAL-ONLY prefix, only display in frame."
  (interactive "fVideo file: \np")  ; "p" = prefix argument
  ...)
```

### 1.4 赋值

```elisp
;; setq - 赋值
(setq x 10)                  ; x = 10
(setq x 1 y 2)              ; 连续赋值: x=1, y=2

;; setq-local - 当前 buffer 赋值
(setq-local buffer-specific-var "value")

;; setq-default - 设置默认值
(setq-default major-mode 'text-mode)
```

### 1.5 注释

```elisp
;;; 三个分号 - 章节注释（通常文件级别）

;; 两个分号 - 普通注释

; 一个分号 - 行内注释（不推荐）
(setq x 10)  ; 这是注释
```

---

## 2. 数据结构

### 2.1 列表 (List)

列表是 Elisp 最基本的数据结构：

```elisp
;; 字面量列表 (quote 阻止求值)
'(1 2 3)                 ; => (1 2 3)
'(a b c)                 ; => (a b c)
'()                      ; => nil (空列表)

;; list 函数动态创建
(list 1 2 3)             ; => (1 2 3)
(list "a" "b" "c")        ; => ("a" "b" "c")

;; 列表可以是任意类型混合
'(1 "hello" t nil 'symbol)
```

### 2.2 cons 细胞和列表

cons 是列表的基础单元：

```elisp
;; cons 创建序对 (car . cdr)
(cons 1 2)                ; => (1 . 2)  ; 点对
(cons 1 '(2 3))          ; => (1 2 3)  ; 列表

;; car - 获取第一个元素
(car '(1 2 3))           ; => 1
(car '(a b c))           ; => a

;; cdr - 获取剩余部分
(cdr '(1 2 3))           ; => (2 3)
(cdr '(a b c))          ; => (b c)

;; 常用快捷函数
(cadr '(1 2 3))          ; => 2  (第二个)
(caddr '(1 2 3))         ; => 3  (第三个)
(cddr '(1 2 3))          ; => (3)  (除了前两个)

;; cons 添加元素到头部
(cons 0 '(1 2 3))        ; => (0 1 2 3)

;; append 合并列表
(append '(1 2) '(3 4))   ; => (1 2 3 4)

;; push 添加到列表头部（修改原列表）
(setq my-list '(2 3))
(push 1 my-list)         ; my-list => (1 2 3)
```

### 2.3 关联列表 (Alist)

键值对数据结构，类似其他语言的字典：

```elisp
;; alist 格式: ((key . value) ...)
'((name . "mpv")
  (version . "1.0")
  (running . t))

;; cons 创建键值对
(cons 'key "value")      ; => (key . "value")

;; alist-get 获取值
(setq my-alist '((name . "mpv") (version . "1.0")))
(alist-get 'name my-alist)           ; => "mpv"
(alist-get 'version my-alist)        ; => "1.0"
(alist-get 'missing my-alist)        ; => nil

;; 带默认值
(alist-get 'missing my-alist :default "N/A")  ; => "N/A"

;; empv 中的实际用法
(let ((reply-id (alist-get 'reply_id msg))
      (event (alist-get 'event msg)))
  ...)
```

### 2.4 向量 (Vector)

类似数组，索引访问：

```elisp
;; 字面量向量
[1 2 3]                   ; => [1 2 3]
["a" "b" "c"]            ; => ["a" "b" "c"]

;; make-vector 创建
(make-vector 5 0)         ; => [0 0 0 0 0]

;; vnth 访问元素
(aref [1 2 3] 0)          ; => 1
(aref [1 2 3] 2)          ; => 3

;; aset 设置元素
(setq arr [1 2 3])
(aset arr 0 10)           ; arr => [10 2 3]

;; empv 中的向量用法
(vector "set" "pause" "yes")  ; mpv IPC 命令格式
;; => ["set" "pause" "yes"]
```

### 2.5 哈希表

键值对，查找效率 O(1)：

```elisp
;; 创建
(setq ht (make-hash-table :test 'equal))

;; 设置
(puthash "key" "value" ht)
(puthash "name" "mpv" ht)

;; 获取
(gethash "key" ht)               ; => "value"
(gethash "missing" ht)            ; => nil

;; 带默认值
(gethash "missing" ht "default")  ; => "default"

;; 检查存在
(remhash "key" ht)   ; 删除
(clrhash ht)         ; 清空
```

---

## 3. 常用函数

### 3.1 类型判断

```elisp
(integerp 10)          ; => t (是整数？)
(numberp 10.5)         ; => t (是数字？)
(stringp "hello")      ; => t (是字符串？)
(symbolp 'abc)         ; => t (是符号？)
(listp '(1 2))         ; => t (是列表？)
(consp '(1 . 2))       ; => t (是 cons？)
(null nil)             ; => t (是空？)
(booleanp t)           ; => t (是布尔？)
(functionp #'car)     ; => t (是函数？)
```

### 3.2 转换函数

```elisp
;; 字符串 <-> 数字
(string-to-number "123")    ; => 123
(number-to-string 123)      ; => "123"

;; 字符串 <-> 符号
(intern "hello")            ; => 'hello (字符串转符号)
(symbol-name 'hello)        ; => "hello" (符号转字符串)

;; 字符串 <-> 列表
(split-string "a,b,c" ",")  ; => ("a" "b" "c")
(string-join '("a" "b" "c") ",")  ; => "a,b,c"

;; 字符串修整
(string-trim "  hello  ")   ; => "hello"
(string-trim-left "  hello") ; => "hello"
(string-trim-right "hello  ") ; => "hello"
```

### 3.3 序列函数 (seq-*)

Emacs 25+ 引入的现代序列函数库：

```elisp
(require 'seq)              ; 引入 seq 库

;; seq-find - 查找第一个满足条件的元素
(seq-find #'evenp '(1 2 3 4 5))    ; => 2
(seq-find (lambda (x) (> x 10)) '(1 5 20 3))  ; => 20

;; seq-filter - 过滤
(seq-filter #'evenp '(1 2 3 4 5 6))  ; => (2 4 6)
(seq-remove #'oddp '(1 2 3 4 5))     ; => (2 4)

;; seq-map - 映射
(seq-map #'1+ '(1 2 3))              ; => (2 3 4)
(seq-map-indexed (lambda (x i) (cons i x)) '(a b c))
;; => ((0 . a) (1 . b) (2 . c))

;; seq-reduce - 归约
(seq-reduce #'+ '(1 2 3 4) 0)        ; => 10
(seq-reduce #'cons '(1 2 3) '())    ; => (3 2 1)

;; seq-position - 查找位置
(seq-position '(a b c d) 'c)        ; => 2

;; seq-contains - 检查包含
(seq-contains '(a b c) 'b)           ; => b

;; seq-empty-p - 是否为空
(seq-empty-p '())                    ; => t
(seq-empty-p '(1))                   ; => nil

;; empv 中的实际应用
(seq-find (lambda (entry)
            (and (<= (car entry) time)  ; 开始时间 <= 当前时间
                 (< time (cadr entry)))) ; 结束时间 > 当前时间
          empv-subtitle-entries)
```

### 3.4 列表操作

```elisp
;; 遍历 - dolist
(dolist (item '(1 2 3) result)
  (setq result (cons item result)))
;; result => (3 2 1)

;; 遍历并收集 - mapcar (旧式，推荐用 seq-map)
(mapcar #'1+ '(1 2 3))         ; => (2 3 4)
(mapcar #'+ '(1 2 3) '(10 20 30))  ; => (11 22 33)

;; 条件收集 - seq-filter (推荐)
(seq-filter #'plusp '(1 -2 3 -4 5))  ; => (1 3 5)

;; 排序
(sort '(3 1 4 1 5 9 2 6) #'<)   ; => (1 1 2 3 4 5 6 9)

;; 删除重复
(delete-dups '(1 2 2 3 3 3))    ; => (1 2 3)
```

### 3.5 字符串操作

```elisp
;; 基本操作
(string= "hello" "hello")        ; => t (相等)
(string< "a" "b")               ; => t (小于)
(concat "hello" " " "world")    ; => "hello world"
(substring "hello" 1 4)         ; => "ell" [1, 4)
(length "hello")                ; => 5

;; 格式化
(format "Hello %s, you have %d messages" "Neo" 5)
;; => "Hello Neo, you have 5 messages"

(format "%05d" 42)               ; => "00042"
(format "%0.2f" 3.14159)        ; => "3.14"

;; empv 中的用法
(format "/tmp/mpv-ipc-%s.sock" (emacs-pid))
;; => "/tmp/mpv-ipc-12345.sock"

(format "[%05.1f] %s\n" 65.5 "Subtitle text")
;; => "[065.5] Subtitle text\n"
```

---

## 4. 流程控制

### 4.1 条件判断

```elisp
;; if - 基本条件
(if (> 3 2)
    "three is greater"
  "two is greater")
;; => "three is greater"

;; if + progn (多条语句)
(if (> 3 2)
    (progn
      (setq x 10)
      (message "Condition true"))
  (message "Condition false"))

;; when - 条件为真执行多条
(when (> 3 2)
  (setq x 10)
  (message "Also true!"))

;; unless - 条件为假执行多条
(unless (> 2 3)
  (message "2 is not greater than 3"))

;; cond - 多条件分支
(cond
 ((= x 1) "one")
 ((= x 2) "two")
 ((= x 3) "three")
 (t "other"))

;; pcase - 模式匹配 (高级)
(pcase (read (user-input))
  (`(,a ,b) (+ a b))           ; 匹配 (x y) 格式
  (`(1 . ,rest) rest)           ; 匹配 (1 . rest)
  (_ "default"))                ; 默认
```

### 4.2 循环

```elisp
;; dolist - 遍历列表
(dolist (item '(a b c))
  (print item))
;; 输出: a, b, c

;; dotimes - 计数循环
(dotimes (i 3)
  (print i))
;; 输出: 0, 1, 2

;; cl-loop - 强大的循环 (需要 cl-lib)
(require 'cl-lib)

;; 基础循环
(cl-loop for i from 1 to 10
         do (print i))

;; 收集结果
(cl-loop for i from 1 to 10
         collect (* i i))
;; => (1 4 9 16 25 36 49 64 81 100)

;; 条件收集
(cl-loop for i from 1 to 10
         when (evenp i)
         collect i)
;; => (2 4 6 8 10)

;; 嵌套循环
(cl-loop for x in '(a b c)
         for y in '(1 2 3)
         collect (list x y))
;; => ((a 1) (b 2) (c 3))

;; while 循环
(setq count 0)
(while (< count 5)
  (print count)
  (setq count (1+ count)))

;; empv 中的应用
(cl-loop for e in empv-subtitle-entries
         for i from 0
         when (and (<= (car e) time) (< time (cadr e)))
         collect i)
```

### 4.3 逻辑运算

```elisp
;; and - 短路与
(and t t)              ; => t
(and t nil)            ; => nil
(and (> 3 1) (< 3 5))   ; => t (两边都成立)

;; or - 短路或
(or nil nil t)         ; => t
(or (gethash "a" ht) "default")  ; 有值返回值，无值返回 default

;; not - 取反
(not nil)              ; => t
(not t)                ; => nil
(null nil)             ; => t
```

---

## 5. 作用域和闭包

### 5.1 let - 局部变量

```elisp
;; let - 并行绑定 (绑定不互相依赖)
(let ((x 1)
      (y 2))
  (+ x y))
;; => 3

;; let* - 顺序绑定 (后面的可以依赖前面)
(let* ((x 1)
       (y (+ x 10)))
  y)
;; => 11

;; 正确用法示例
(let* ((entries (with-temp-buffer
                  (insert-file-contents srt-path)
                  (empv-parse-srt-from-string (buffer-string))))
       (buf-name " *empv Subtitle*"))
  ;; 这里 entries 已经定义好了
  (setq empv-subtitle-entries entries)
  ...)
```

### 5.2 闭包 - 捕获外部变量

```elisp
;; 匿名函数 (lambda) 可以捕获定义时的环境
(setq multiplier 10)
(setq multiply-by-ten (lambda (x) (* x multiplier)))
(funcall multiply-by-ten 5)  ; => 50

;; empv 中的 sentinel 闭包示例
:sentinel (lambda (p s)           ; p=进程, s=状态字符串
            (message "empv: %s" s) ; 访问外部函数
            (when (string-match "exited" s)
              (empv-subtitle-stop-update)  ; 调用外部函数
              (setq empv--process nil)))   ; 修改外部变量
```

### 5.3 lexical-binding 词法作用域

```elisp
;; 文件开头声明
;;; empv.el --- ... -*- lexical-binding: t; -*-

;; 词法作用域下，lambda 可以正确捕获变量
(let ((counter 0))
  (setq increment (lambda ()
                    (setq counter (1+ counter)))))
;; 在动态作用域下无法实现
```

---

## 6. 进程通信

### 6.1 创建子进程

```elisp
;; make-process - 创建子进程
(make-process
 :name "empv"                      ; 进程名
 :buffer (generate-new-buffer " *empv*")  ; 缓冲区
 :command '("mpv" "video.mp4")     ; 要执行的命令
 :sentinel #'empv--sentinel)       ; 退出时回调

;; 进程过滤器 - 数据到达时调用
(defun empv--filter (proc output)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output)
    (empv--process-output)))

;; 进程哨兵 - 状态变化时调用
(defun empv--sentinel (proc status)
  (message "empv: %s" status)
  (when (string-match "exited\\|terminated" status)
    (empv-subtitle-stop-update)
    (setq empv--process nil)))
```

### 6.2 网络进程 (Unix Socket)

```elisp
;; make-network-process - 创建网络进程
(make-network-process
 :name "empv-ipc"                 ; 进程名
 :buffer empv--socket-buffer     ; 缓冲区
 :family 'local                   ; 本地 socket
 :service empv--socket-path       ; socket 路径
 :filter #'empv--filter           ; 数据过滤器
 :sentinel #'empv--sentinel)      ; 状态哨兵

;; 向进程发送数据
(process-send-string proc "data\n")
(process-send proc data-object)  ; 发送数据

;; 检查进程状态
(process-live-p proc)             ; 是否运行中
(process-status proc)            ; :run :exit :signal 等
```

### 6.3 empv 完整的播放流程

```elisp
(defun empv-play (path)
  "Play video file PATH."
  ;; 1. 生成 socket 路径
  (setq empv--socket-path (format "/tmp/mpv-ipc-%s.sock" (emacs-pid)))
  
  ;; 2. 清理旧的 socket
  (when (file-exists-p empv--socket-path)
    (delete-file empv--socket-path))
  
  ;; 3. 启动 mpv 进程
  (setq empv--process
        (make-process
         :name "empv"
         :buffer (generate-new-buffer " *empv*")
         :command (list "mpv" 
                        "--no-terminal"
                        "--input-ipc-server=/tmp/mpv.sock"
                        path)
         :sentinel (lambda (p s) ...)))
  
  ;; 4. 延迟连接，等待 socket 创建
  (run-with-timer 0.1 nil #'empv--connect))
```

---

## 7. JSON 处理

### 7.1 编码和解码

```elisp
(require 'json)  ; 需要引入

;; 编码: Elisp -> JSON
(json-encode '((name . "mpv") (version . "1.0")))
;; => "{\"name\":\"mpv\",\"version\":\"1.0\"}"

;; 解码: JSON -> Elisp
(json-parse-string "{\"name\":\"mpv\"}" :object-type 'alist)
;; => ((name . "mpv"))

(json-parse-string "[1, 2, 3]" :array-type 'list)
;; => [1 2 3]  (向量)
```

### 7.2 mpv IPC 协议

```elisp
;; mpv 命令格式
;; 发送:
`((command . ["set" "pause" "yes"])
  (request_id . 5))
;; JSON: {"command":["set","pause","yes"],"request_id":5}

;; 发送:
`((command . ["get_property" "time-pos"])
  (request_id . 6))
;; JSON: {"command":["get_property","time-pos"],"request_id":6}

;; 接收响应:
(let-alist msg
  (when .reply_id ...)      ; 命令响应
  (when .event ...)         ; 事件通知
  (when .data ...))         ; 数据负载
```

### 7.3 let-alist 宏

```elisp
;; 解构 alist 的简洁方式
(let-alist '((name . "mpv") (version . "1.0"))
  (message "%s version %s" .name .version))
;; => "mpv version 1.0"

;; 嵌套访问
(let-alist '((data . ((time . 100) (title . "Video"))))
  (message "Time: %s" (alist-get 'time .data)))
```

---

## 8. 正则表达式

### 8.1 基本匹配

```elisp
;; string-match - 正则匹配
(string-match "\\([0-9]+\\):\\([0-9]+\\)" "12:34")
;; 返回匹配的起始位置

;; match-string - 获取捕获组
(match-string 0 "12:34")   ; 整个匹配 => "12:34"
(match-string 1 "12:34")   ; 第一个 () => "12"
(match-string 2 "12:34")   ; 第二个 () => "34"

;; 常用字符类
"[0-9]"       ; 数字
"[a-zA-Z]"    ; 字母
"[^,]+"       ; 非逗号字符
"\\s-"        ; 空白字符
"\\w"         ; 单词字符
```

### 8.2 empv 中的正则应用

```elisp
;; 解析 SRT 时间戳 "00:00:01,500"
(string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[,\\.]\\([0-9]+\\)" 
              "00:00:01,500")
(match-string 1 "00:00:01,500")  ; "00" (小时)
(match-string 2 "00:00:01,500")  ; "00" (分钟)
(match-string 3 "00:00:01,500")  ; "01" (秒)
(match-string 4 "00:00:01,500")  ; "500" (毫秒)

;; 解析字幕块
(string-match "\\`[0-9]+\n\\([0-9:,]+\\) --> \\([0-9:,]+\\)" 
              "1\n00:00:01,500 --> 00:00:03,000\nHello")

;; split-string - 分割字符串
(split-string "line1\nline2\nline3" "\n")
;; => ("line1" "line2" "line3")

(split-string "a,b,c" ",")
;; => ("a" "b" "c")

;; 支持正则分割
(split-string "a,b;c" ",\\|;")  ; 按逗号或分号分割
;; => ("a" "b" "c")
```

---

## 9. 高级特性

### 9.1 宏 backquote (反引号)

```elisp
;; ` - 类似 '，但允许 , 求值
(setq x 10)
`(+ ,x 1)           ; => (+ 10 1)  求值了 x
'(+ x 1)           ; => (+ x 1)   未求值

;; ,@ - 展开列表
(setq nums '(1 2 3))
`(+ ,@nums)        ; => (+ 1 2 3)

;; empv 中的用法
`((command . ["observe_property" 1 "time-pos"])
  (request_id . ,(cl-incf empv--command-id)))
;; request_id 会求值为递增后的数字
```

### 9.2 pcase 模式匹配

```elisp
(require 'pcase)

;; 基本模式
(pcase 1
  (1 "one")
  (2 "two")
  (_ "other"))
;; => "one"

;; 匹配列表
(pcase '(1 2 3)
  (`(,a ,b ,c) (list a b c)))
;; => (1 2 3)

;; 匹配 cons
(pcase '(a . b)
  (`(,x . ,y) (cons y x)))
;; => (b . a)

;; empv 中的用法
(seq-remove (pcase-lambda (`(,id . ,_)) (= id .reply_id))
            empv--pending-callbacks)
;; 匹配 (id . callback) 格式的列表元素
```

### 9.3 定时器

```elisp
;; run-with-timer - 一次性定时器
(run-with-timer 2 nil (lambda () (message "After 2 seconds")))

;; run-with-timer - 重复定时器
(run-with-timer 0 0.1 #'empv-update-subtitle-highlight)
;; 0: 首次延迟, 0.1: 重复间隔(秒)

;; run-with-idle-timer - 空闲时执行
(run-with-idle-timer 1 t #'auto-save)

;; 取消定时器
(cancel-timer timer-object)

;; empv 中的应用
(setq empv-subtitle-timer
      (run-with-timer 0 0.1 #'empv-update-subtitle-highlight))
```

### 9.4 Buffer 操作

```elisp
;; 创建缓冲区
(get-buffer-create "*temp*")           ; 获取或创建
(generate-new-buffer "*temp*")        ; 始终新建

;; 读取文件
(with-temp-buffer
  (insert-file-contents "file.txt")
  (buffer-string))                     ; 返回内容

;; 修改缓冲区
(insert "text")                        ; 插入
(erase-buffer)                         ; 清空
(goto-char (point-min))               ; 移动到开头
(goto-char (point-max))               ; 移动到结尾

;; 窗口操作
(switch-to-buffer buf)                 ; 切换
(current-buffer)                      ; 当前
(window-buffer (frame-first-window f)) ; frame 的主窗口

;; empv 中的用法
(with-current-buffer buf
  (setq buffer-read-only nil)
  (erase-buffer)
  (dolist (entry entries)
    (insert (format "[%05.1f] %s\n" (car entry) (caddr entry))))
  (setq buffer-read-only t))
```

### 9.5 Overlay 高亮

```emacs-lisp
;; 创建 overlay
(let ((ov (make-overlay (point) (line-end-position))))
  (overlay-put ov 'face '(:background "yellow"))
  (overlay-put ov 'highlight t))

;; 查找 overlay
(overlays-at (point))                  ; 当前位置的 overlays
(overlays-in start end)                ; 范围内的 overlays

;; 删除 overlay
(remove-overlays start end 'highlight t)  ; 按属性删除
(delete-overlay ov)                   ; 删除单个

;; 常用 face
'(:background "yellow")
'(:foreground "red" :bold t)
'(region)                              ; 使用 region 样式
'(highlight)                           ; 高亮样式
```

---

## 10. empv.el 完整解析

### 10.1 文件结构概览

```elisp
;;; 头部声明
;;; empv.el --- Control mpv via socket IPC -*- lexical-binding: t; -*-

(require 'json)      ; JSON 解析
(require 'subr-x)    ; 字符串扩展
(require 'seq)       ; 序列函数

;;; 变量定义
(defvar empv--process nil)          ; mpv 进程
(defvar empv--socket-path nil)      ; socket 路径
(defvar empv--socket-proc nil)      ; socket 进程
(defvar empv--command-id 0)        ; 命令 ID
(defvar empv--pending-callbacks nil) ; 待处理回调

;;; 函数定义
;; 进程通信
(defun empv--filter ...)           ; 数据过滤器
(defun empv--process-output ...)   ; 处理输出
(defun empv--handle-message ...)   ; 处理消息
(defun empv--send ...)             ; 发送命令
(defun empv--connect ...)          ; 连接 socket
(defun empv--observe-time-pos ...) ; 订阅属性

;; 播放控制
(defun empv-play ...)              ; 播放
(defun empv-pause ...)             ; 暂停
(defun empv-resume ...)            ; 恢复
(defun empv-stop ...)              ; 停止

;; 字幕功能
(defun empv-load-subtitle ...)     ; 加载字幕
(defun empv-parse-srt-from-string ...) ; 解析 SRT
(defun empv-load-subtitle-file ...) ; 显示字幕
(defun empv-update-subtitle-highlight ...) ; 高亮更新

;;; 提供功能
(provide 'empv)
```

### 10.2 核心通信流程

```elisp
;; 1. 启动 mpv，创建 socket
empv-play
  ↓
  make-process (启动 mpv 进程)
  ↓
  run-with-timer 0.1 后调用 empv--connect
  ↓
  2. 连接 Unix socket
  empv--connect
  ↓
  make-network-process (连接 socket)
  ↓
  empv--observe-time-pos (订阅 time-pos)
  ↓
  3. 数据循环
  empv--filter (收到数据)
  ↓
  empv--process-output (解析 JSON)
  ↓
  empv--handle-message (处理消息)
  ↓
  回调或更新状态
```

### 10.3 回调机制

```elisp
;; 发送命令并注册回调
(defun empv--send-command (cmd callback)
  (let ((req-id (cl-incf empv--command-id)))
    ;; 注册回调
    (push (cons req-id callback) empv--pending-callbacks)
    ;; 发送命令
    (empv--send `((command . ,cmd) (request_id . ,req-id)))))

;; 接收响应时调用回调
(defun empv--handle-message (msg)
  (let-alist msg
    (when .reply_id
      ;; 查找对应回调
      (when-let (cb (alist-get .reply_id empv--pending-callbacks))
        ;; 调用回调，传入错误和数据
        (funcall cb .error .data))
      ;; 移除已处理的回调
      (setq empv--pending-callbacks
            (seq-remove (pcase-lambda (`(,id . ,_)) (= id .reply_id))
                        empv--pending-callbacks)))))
```

---

## 练习题

### 初级

1. 计算 1 到 100 的和
2. 找出列表 `(3 1 4 1 5 9 2 6)` 中的最大值
3. 写一个函数反转字符串

### 中级

4. 实现一个简单的学生成绩管理（使用 alist）
5. 解析 JSON 文件并提取特定字段

### 高级

6. 使用进程通信实现简单的 echo 服务器
7. 实现一个带超时的异步请求函数

---

## 参考资源

- Emacs 官方手册: `(info "(elisp) Top")`
- Elisp 简介: `M-x info-emacs-manual`
- 交互式练习: `M-x ielm`

---

> 本文档基于 empv.el v1.0.0 重构版本生成
