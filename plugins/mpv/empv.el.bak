;;; empv.el --- Control mpv via socket IPC -*- lexical-binding: t -*-

;; empv.el: 用Elisp控制mpv播放器
;; 通过Unix socket IPC与mpv通信，实现播放控制、字幕高亮等功能

;; ============================================================
;; 1. 必要库加载
;; ============================================================

;; json: 解析和编码JSON数据，mpv IPC使用JSON格式通信
(require 'json)

;; subr-x: 字符串处理扩展，提供string-trim等函数
(require 'subr-x)

;; cl-lib: Common Lisp库，提供cl-loop、cl-incf等函数
(require 'cl-lib)

;; ============================================================
;; 2. 内部变量定义 (以--开头表示内部变量)
;; ============================================================

;; empv--process: 保存mpv子进程对象
(defvar empv--process nil)

;; empv--socket-path: Unix socket文件路径，用于IPC通信
(defvar empv--socket-path nil)

;; empv--socket-proc: 保存socket网络进程对象
(defvar empv--socket-proc nil)

;; empv--socket-buffer: socket通信缓冲区名称
;; 空格开头表示临时缓冲区，不显示在buffer列表
(defvar empv--socket-buffer " *empv-ipc*")

;; empv--command-id: 命令ID计数器，用于匹配请求和响应
(defvar empv--command-id 0)

;; empv--last-time: 记录当前播放时间(秒)
(defvar empv--last-time -1.0)

;; empv--pending-callbacks: 待处理回调队列
;; 格式: ((request-id . callback-func) ...)
(defvar empv--pending-callbacks nil)

;; empv--stopping: 停止标志，防止重复清理
(defvar empv--stopping nil)

;; ============================================================
;; 3. 字幕相关变量
;; ============================================================

;; empv-subtitle-timer: 字幕高亮更新定时器
(defvar empv-subtitle-timer nil)

;; empv-subtitle-current-index: 当前高亮的字幕索引
(defvar empv-subtitle-current-index nil)

;; empv-subtitle-entries: 解析后的字幕条目列表
;; 格式: ((start-time end-time text) ...)
(defvar empv-subtitle-entries nil)

;; empv-subtitle-frame: 字幕显示frame
(defvar empv-subtitle-frame nil)

;; empv-subtitle-last-index: 上次高亮的字幕索引
(defvar empv-subtitle-last-index -1)

;; empv-subtitle-video-path: 当前播放的视频路径
;; defvar-local创建 buffer-local 变量，每个buffer有独立值
(defvar-local empv-subtitle-video-path nil)

;; ============================================================
;; 4. 网络过滤器 (Process Filter)
;; ============================================================

;; empv--filter: 网络进程过滤器，当收到数据时自动调用
;; proc: 进程对象
;; output: 新收到的数据字符串
(defun empv--filter (proc output)
  ;; process-buffer: 获取进程的关联缓冲区
  (with-current-buffer (process-buffer proc)
    ;; 移动到缓冲区末尾
    (goto-char (point-max))
    ;; 插入新数据
    (insert output)
    ;; 处理输出
    (empv--process-output)))

;; ============================================================
;; 5. JSON消息处理
;; ============================================================

;; empv--process-output: 解析缓冲区中的JSON消息
;; 逐行查找JSON对象并处理
(defun empv--process-output ()
  ;; 移动到缓冲区开始
  (goto-char (point-min))
  ;; 正则匹配JSON对象 { ... }\n
  ;; re-search-forward: 从当前位置向后搜索
  ;; nil t: 不限定搜索次数，直到搜索失败
  (while (re-search-forward "{[^}]+}\n?" nil t)
    ;; match-string 0: 匹配的整个字符串
    (let* ((json-str (match-string 0))
           ;; json-parse-string: 将JSON字符串解析为Elisp数据结构
           ;; :object-type 'alist: 将JSON对象解析为关联列表((key . value) ...)
           (json (ignore-errors (json-parse-string json-str :object-type 'alist))))
      ;; when: 条件为真时执行body
      (when json
        ;; 处理JSON消息
        (empv--handle-message json)))))

;; empv--handle-message: 处理收到的JSON消息
;; msg: 解析后的消息(alist)
(defun empv--handle-message (msg)
  ;; let: 绑定局部变量
  ;; alist-get: 从关联列表获取值，类似其他语言的dict[key]
  (let ((reply-id (alist-get 'reply_id msg))
        (event (alist-get 'event msg))
        (data (alist-get 'data msg))
        (name (alist-get 'name msg))
        (value (alist-get 'value msg)))
    
    ;; --- 处理命令响应 ---
    ;; 当收到命令响应时，调用对应的回调函数
    (when reply-id
      ;; 从待处理回调中查找对应的callback
      (let ((cb (alist-get reply-id empv--pending-callbacks)))
        ;; funcall: 调用函数，类似于JS的cb()
        (when cb (funcall cb (alist-get 'error msg) data))
        ;; cl-delete-if: 删除满足条件的元素
        (setq empv--pending-callbacks 
              (cl-delete-if (lambda (p) (= (car p) reply-id)) 
                           empv--pending-callbacks))))
    
    ;; --- 处理属性变化事件 ---
    ;; property-change: mpv属性变化时触发
    (when (and event (string= event "property-change"))
      ;; time-pos: 播放位置属性
      (when (string= name "time-pos")
        ;; 更新当前播放时间
        ;; if: 三元表达式，(if 条件 真值 假值)
        (setq empv--last-time (if (numberp value) value data))))
    
    ;; --- 处理播放结束事件 ---
    ;; idle: mpv进入空闲状态(播放完毕)
    (when (and event (string= event "idle") empv--stopping)
      ;; 清理字幕
      (empv-subtitle-stop-update)
      ;; 关闭socket进程
      (when empv--socket-proc
        (delete-process empv--socket-proc)
        (setq empv--socket-proc nil))
      (setq empv--process nil)
      (setq empv--last-time -1.0)
      (setq empv--stopping nil))
    
    ;; --- 处理时间数据 ---
    ;; 有些响应中时间在data的嵌套结构里
    (when (and data (listp data) (alist-get 'type data))
      (when (string= (alist-get 'type data) "time")
        (setq empv--last-time (alist-get 'value data))))))

;; ============================================================
;; 6. 进程哨兵 (Sentinel)
;; ============================================================

;; empv--sentinel: 进程状态变化时调用
;; proc: 进程对象
;; status: 状态字符串，如"exitednormally"
(defun empv--sentinel (proc status)
  ;; message: 类似printf，打印到echo area和*Messages*缓冲区
  (message "empv: %s" status)
  ;; string-match: 正则匹配，成功返回起始位置
  (when (string-match "closed\\|exited\\|terminated" status)
    ;; 清理字幕相关资源
    (empv-subtitle-stop-update)
    (setq empv--socket-proc nil)
    (setq empv--process nil)
    (setq empv--last-time -1.0)))

;; ============================================================
;; 7. 发送命令
;; ============================================================

;; empv--send: 向mpv发送JSON命令
;; obj: 要发送的数据(会被编码为JSON)
(defun empv--send (obj)
  ;; and: 短路与，两个条件都满足才执行
  (when (and empv--socket-proc 
             ;; process-live-p: 检查进程是否运行中
             (process-live-p empv--socket-proc))
    ;; process-send-string: 发送字符串到进程
    ;; json-encode: 将Elisp对象编码为JSON字符串
    (process-send-string empv--socket-proc 
                        (concat (json-encode obj) "\n"))))

;; ============================================================
;; 8. 连接管理
;; ============================================================

;; empv--connect: 连接到mpv的Unix socket
(defun empv--connect ()
  ;; 检查socket文件是否存在
  (when (and empv--socket-path 
             (file-exists-p empv--socket-path))
    ;; condition-case: 错误处理，类似于try-catch
    ;; (condition-case 变量 body 错误类型 ...)
    ;; nil表示不捕获任何错误
    (condition-case nil
        (progn
          ;; make-network-process: 创建网络进程(本地socket)
          (setq empv--socket-proc
                (make-network-process
                 :name "empv-ipc"           ; 进程名
                 :buffer empv--socket-buffer ; 缓冲区
                 :family 'local              ; 本地socket
                 :service empv--socket-path  ; socket路径
                 :filter #'empv--filter      ; 数据过滤器
                 :sentinel #'empv--sentinel)) ; 状态哨兵
          ;; 开始监听播放时间变化
          (empv--observe-time-pos))
      ;; error: 捕获所有错误
      (error 
       ;; run-with-timer: 设置定时器
       ;; 0.1秒后重试连接
       (run-with-timer 0.1 nil #'empv--connect)))))

;; empv--observe-time-pos: 订阅time-pos属性变化
;; observe_property: mpv IPC命令，订阅属性变化事件
;; 1: 事件ID，用于标识这次订阅
;; time-pos: 要监听的时间位置属性
(defun empv--observe-time-pos ()
  "Start observing time-pos property."
  (empv--send (list (cons 'command (vector "observe_property" 1 "time-pos"))
                   (cons 'request_id (cl-incf empv--command-id)))))

;; ============================================================
;; 9. 播放控制命令
;; ============================================================

;; ;;;###autoload: 让M-x可以自动补全此命令
;; empv-play: 播放视频文件
;; path: 视频文件路径
(defun empv-play (path)
  "Play video file PATH."
  ;; interactive "f": 提示用户选择文件
  (interactive "fVideo file: ")
  
  ;; format: 格式化字符串，类似printf
  ;; emacs-pid: 当前Emacs进程ID
  (setq empv--socket-path (format "/tmp/mpv-ipc-%s.sock" (emacs-pid)))
  
  ;; 清理旧的socket文件
  (when (file-exists-p empv--socket-path)
    (delete-file empv--socket-path))
  
  ;; make-process: 创建子进程
  (setq empv--process
        (make-process
         :name "empv"                     ; 进程名
         :buffer (generate-new-buffer " *empv*") ; 进程缓冲区
         ;; command: 要执行的命令列表
         :command (list "mpv" 
                        "--no-terminal"    ; 不使用终端
                        "--osc=no"         ; 禁用OSD控制
                        "--osd-bar=no"     ; 禁用OSD进度条
                        "--sub-auto=no"    ; 不自动加载字幕
                        ;; --input-ipc-server: 启用IPC服务器
                        (format "--input-ipc-server=%s" empv--socket-path)
                        (expand-file-name path))
         ;; sentinel: 进程退出时执行的回调
         :sentinel (lambda (p s)
                     (message "empv: mpv %s" s)
                     (when (string-match "exited\\|terminated\\|closed" s)
                       (empv-subtitle-stop-update)
                       (setq empv--process nil)
                       (when empv--socket-proc
                         (delete-process empv--socket-proc)
                         (setq empv--socket-proc nil))))))
  
  ;; 延迟0.1秒后尝试连接，让mpv有时间创建socket
  (run-with-timer 0.1 nil #'empv--connect))

;; empv-pause: 暂停播放
(defun empv-pause ()
  "Pause playback."
  (interactive)
  ;; 向mpv发送pause命令
  ;; vector: 创建向量，类似数组
  (empv--send (list (cons 'command (vector "set" "pause" "yes")))))

;; empv-resume: 恢复播放
(defun empv-resume ()
  "Resume playback."
  (interactive)
  (empv--send (list (cons 'command (vector "set" "pause" "no")))))

;; empv-stop: 停止播放并清理资源
(defun empv-stop ()
  "Stop playback."
  (interactive)
  ;; 防止重复执行
  (when (not empv--stopping)
    (setq empv--stopping t)
    ;; 先停止字幕更新
    (empv-subtitle-stop-update)
    ;; 发送quit命令
    (empv--send (list (cons 'command (vector "quit"))))
    ;; 等待一下让mpv处理quit
    (sleep-for 0.1)
    ;; 强制删除mpv进程
    (when empv--process
      (delete-process empv--process)
      (setq empv--process nil))
    ;; 删除socket进程
    (when empv--socket-proc
      (delete-process empv--socket-proc)
      (setq empv--socket-proc nil))
    ;; 删除socket文件
    (when (and empv--socket-path (file-exists-p empv--socket-path))
      (delete-file empv--socket-path))
    ;; 重置播放时间
    (setq empv--last-time -1.0)
    (setq empv--stopping nil)))

;; empv-get-time: 获取当前播放时间
;; callback: 可选的回调函数
(defun empv-get-time (&optional callback)
  "Get current time. If CALLBACK is provided, call it with the time value."
  ;; cl-incf: 原子递增，类似 ++i
  (let ((req-id (cl-incf empv--command-id)))
    ;; 发送获取时间的命令
    (empv--send (list (cons 'command (vector "get_property" "time-pos"))
                     (cons 'request_id req-id)))
    ;; 如果提供了回调，异步处理
    (if callback
        (push (cons req-id callback) empv--pending-callbacks)
      ;; 否则同步等待响应
      ;; float-time: 返回当前时间戳(秒)
      (let ((end (+ (float-time) 0.5)))
        ;; 轮询等待响应，最多0.5秒
        (while (and (not empv--last-time) (< (float-time) end)))
        ;; 返回最后更新时间
        empv--last-time))))

;; empv-load-subtitle: 加载字幕到mpv
(defun empv-load-subtitle (path)
  "Load subtitle into video."
  (interactive "fSubtitle file: ")
  ;; sub-add: mpv命令，添加字幕轨道
  (empv--send (list (cons 'command (vector "sub-add" (expand-file-name path))))))

;; ============================================================
;; 10. 字幕模式定义
;; ============================================================

;; define-derived-mode: 基于已有mode创建新mode
;; special-mode: 用于显示内容的只读buffer
(define-derived-mode empv-subtitle-mode special-mode "Subtitle"
  "Major mode for displaying subtitles."
  (setq truncate-lines t)      ; 禁止行回绕
  (setq buffer-read-only nil)) ; 暂时允许写入

;; ============================================================
;; 11. SRT字幕解析
;; ============================================================

;; empv-parse-srt-from-string: 解析SRT文件内容
;; content: SRT文件字符串
;; 返回格式: ((start end text) ...)
(defun empv-parse-srt-from-string (content)
  "Parse raw SRT string CONTENT into a list of entries.
Each entry: (start-time end-time text)"
  (let ((entries nil)
        ;; split-string: 按正则分割字符串
        ;; "\\r?\\n\\r?\\n": 空行分割(支持Windows/Unix换行)
        ;; t: 去除空字符串
        (blocks (split-string content "\r?\n\r?\n" t)))
    ;; dolist: 遍历列表，类似for-each
    (dolist (block blocks)
      ;; 匹配字幕块: 序号 + 时间行 + 文本
      (when (string-match "\\`[0-9]+\n\\([0-9:,]+\\) --> \\([0-9:,]+\\)" block)
        ;; match-string: 获取正则捕获组
        (let* ((start-str (match-string 1 block))
               (end-str (match-string 2 block))
               ;; 解析时间戳为秒数
               (start (empv-parse-timestamp start-str))
               (end (empv-parse-timestamp end-str))
               ;; 取最后一行作为文本(可能有多个文本行)
               (text (car (last (split-string block "\n" t)))))
          (when start
            ;; push: 添加到列表开头
            (push (list start (or end start) text) entries)))))
    ;; nreverse: 反转列表(因为是push添加的)
    (nreverse entries)))

;; empv-parse-timestamp: 解析SRT时间戳
;; ts: 时间字符串，如 "00:00:01,500"
;; 返回: 秒数(浮点数)
(defun empv-parse-timestamp (ts)
  "Parse SRT timestamp like '00:00:01,500' to seconds as float."
  (when ts
    ;; 匹配时:分:秒,毫秒
    (if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[,\\.]\\([0-9]+\\)" ts)
        (let ((h (string-to-number (match-string 1 ts)))
              (m (string-to-number (match-string 2 ts)))
              (s (string-to-number (match-string 3 ts)))
              (ms (string-to-number (match-string 4 ts))))
          ;; 计算总秒数
          (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))
      nil)))

;; ============================================================
;; 12. 字幕Frame显示
;; ============================================================

;; empv-load-subtitle-file: 加载字幕文件并显示在frame中
(defun empv-load-subtitle-file (srt-path)
  "Load SRT file from SRT-PATH and display in subtitle frame."
  ;; let*: 顺序绑定变量(后面可以用前面的变量)
  (let* ((entries (with-temp-buffer
                    ;; insert-file-contents: 读取文件内容到缓冲区
                    (insert-file-contents srt-path)
                    (empv-parse-srt-from-string (buffer-string))))
         (buf-name " *empv Subtitle*"))
    ;; 设置全局变量
    (setq empv-subtitle-entries entries)
    (setq empv-subtitle-last-index -1)
    
    ;; 如果已有frame，先删除
    (when (and empv-subtitle-frame (frame-live-p empv-subtitle-frame))
      (delete-frame empv-subtitle-frame))
    
    ;; 创建显示字幕的buffer
    (let ((buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (setq buffer-read-only nil)
        (erase-buffer)                      ; 清空缓冲区
        (empv-subtitle-mode)                ; 设置mode
        ;; 逐条插入字幕
        (dolist (entry entries)
          (let ((start (car entry))
                (text (caddr entry)))
            ;; format: 格式化字符串，%05.1f表示5位小数点后1位
            (insert (format "[%05.1f] %s\n" start text))))
        (setq buffer-read-only t)
        (setq empv-subtitle-current-index nil))
      
      ;; 创建独立frame显示字幕
      (setq empv-subtitle-frame
            (make-frame `((name . "empv Subtitle")
                          (minibuffer . nil)     ; 无minibuffer
                          (auto-raise . t)        ; 总是置顶
                          (frame-resize-pixelwise . t))))
      ;; 切换到新frame和buffer
      (select-frame empv-subtitle-frame)
      (switch-to-buffer buf))
    empv-subtitle-frame))

;; empv-find-subtitle-at-time: 查找指定时间的字幕
(defun empv-find-subtitle-at-time (time)
  "Find subtitle entry at TIME (in seconds)."
  ;; seq-find: 查找满足条件的第一个元素
  (seq-find (lambda (entry)
              (and (>= time (car entry))    ; 开始时间 <= 当前时间
                   (< time (cadr entry))))   ; 结束时间 > 当前时间
            empv-subtitle-entries))

;; empv-get-subtitle-index: 获取字幕索引
(defun empv-get-subtitle-index (time)
  "Get the index of subtitle entry at TIME (in seconds)."
  ;; seq-position: 查找元素位置
  (seq-position empv-subtitle-entries
                (empv-find-subtitle-at-time time)
                #'equal))

;; ============================================================
;; 13. 字幕高亮更新
;; ============================================================

;; empv-update-subtitle-highlight: 更新字幕高亮
;; 每0.1秒被定时器调用
(defun empv-update-subtitle-highlight ()
  "Update subtitle highlight based on current playback time."
  (let ((time empv--last-time))
    (when (and time 
               empv-subtitle-entries 
               empv-subtitle-frame 
               (frame-live-p empv-subtitle-frame))
      ;; cl-loop: 循环，收集满足条件的索引
      (let* ((matches (cl-loop for e in empv-subtitle-entries
                               for i from 0
                               when (and (<= (car e) time) (< time (cadr e)))
                               collect i))
             (idx (car matches)))
        2        ;; 当索引变化时更新高亮
        (when (and idx (numberp idx) (/= idx empv-subtitle-last-index))
          (setq empv-subtitle-last-index idx)
          ;; 切换到字幕buffer
          (with-current-buffer (window-buffer (frame-first-window empv-subtitle-frame))
            ;; 移除旧的高亮overlay
            (remove-overlays (point-min) (point-max) 'empv-subtitle-highlight t)
            ;; 移动到指定行
            (goto-char (point-min))
            (forward-line idx)
            ;; 创建新的overlay高亮
            (let ((ov (make-overlay (point) (line-end-position))))
              (overlay-put ov 'empv-subtitle-highlight t)
              ;; 设置高亮样式
              (overlay-put ov 'face '(:background "dark slate gray" :foreground "white"))
              (setq empv-subtitle-current-index (point)))))))))

;; empv-subtitle-auto-update: 启动自动更新
(defun empv-subtitle-auto-update ()
  "Start auto-update timer for subtitle highlight."
  ;; run-with-timer: 创建定时器
  ;; 0: 首次执行延迟(立即)
  ;; 0.1: 重复间隔(秒)
  ;; #: 引用函数(不求值)
  (setq empv-subtitle-timer
        (run-with-timer 0 0.1 #'empv-update-subtitle-highlight)))

;; empv-subtitle-stop-update: 停止自动更新
(defun empv-subtitle-stop-update ()
  "Stop auto-update timer."
  ;; 取消定时器
  (when empv-subtitle-timer
    (cancel-timer empv-subtitle-timer)
    (setq empv-subtitle-timer nil))
  ;; 删除字幕frame
  (when (and empv-subtitle-frame (frame-live-p empv-subtitle-frame))
    (delete-frame empv-subtitle-frame)
    (setq empv-subtitle-frame nil)))

;; ============================================================
;; 14. 主入口命令
;; ============================================================

;; empv-play-with-subtitle: 播放视频并显示字幕
;; video-path: 视频文件路径
;; external-only: 前缀参数，是否只显示在frame中
(defun empv-play-with-subtitle (video-path &optional external-only)
  "Play VIDEO-PATH and load corresponding subtitle if exists.
With prefix arg EXTERNAL-ONLY, only display subtitle in frame without loading into video."
  ;; interactive "fVideo file: \np": 
  ;;   f: 文件选择
  ;;   p: 前缀参数(数字)
  (interactive "fVideo file: \np")
  (empv-play video-path)
  (setq empv-subtitle-video-path video-path)
  ;; 查找同名.srt文件
  (let ((srt-path (concat (file-name-sans-extension video-path) ".srt")))
    (if (file-exists-p srt-path)
        (progn
          ;; 加载字幕到frame显示
          (empv-load-subtitle-file srt-path)
          ;; 启动高亮更新
          (empv-subtitle-auto-update)
          ;; 如果不是仅外部模式，加载到视频中
          (when (not external-only)
            (empv-load-subtitle srt-path)))
      (message "No subtitle file found for %s" video-path))))

;; ============================================================
;; 15. 模块提供
;; ============================================================

;; provide: 声明此文件提供的功能
(provide 'empv)

;;; empv.el ends here
