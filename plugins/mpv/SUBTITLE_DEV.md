# MPV Emacs 字幕模块开发总结

## 目录
1. [项目概述](#1-项目概述)
2. [Rust 模块实现](#2-rust-模块实现)
3. [Elisp 模块实现](#3-elisp-模块实现)
4. [核心函数详解](#4-核心函数详解)
5. [使用说明](#5-使用说明)

---

## 1. 项目概述

本项目实现了一个 Emacs 插件，用于：
1. 通过 libmpv 控制视频播放
2. 加载 SRT 字幕文件
3. 实时同步高亮当前播放时间的字幕

### 文件结构
```
mpv/
├── Cargo.toml          # Rust 项目配置
├── src/lib.rs          # Rust 核心实现（Emacs 模块）
├── mpv-subtitle.el     # Elisp 字幕处理模块
├── test_video.mp4      # 测试用视频
└── test_video.srt      # 测试用字幕
```

---

## 2. Rust 模块实现

### Cargo.toml 配置
```toml
[package]
name = "mpv"
version = "0.1.0"
edition = "2024"

[lib]
crate-type = ["cdylib"]
name = "mpv"

[dependencies]
anyhow = "1.0.102"
emacs = { version = "0.20.0", features = ["debug"] }
libmpv = { version = "2.0.2-fork.1", package = "libmpv-sirno" }
once_cell = "1.21.4"
```

关键点：
- `crate-type = ["cdylib"]` 编译成 C 动态库，供 Emacs 调用
- 使用 `libmpv-sirno` 替代原版 libmpv 以支持新版 mpv

### 核心实现 (src/lib.rs)

```rust
use emacs::{defun, Env, Result, Value};
use libmpv::Mpv;
use once_cell::sync::Lazy;
use std::sync::Mutex;

// 全局 mpv 实例，使用 Mutex 保证线程安全
static MPV: Lazy<Mutex<Option<Mpv>>> = Lazy::new(|| Mutex::new(None));

// 用于存储当前播放时间的后台监听
static CURRENT_TIME: Lazy<Mutex<f64>> = Lazy::new(|| Mutex::new(-1.0));
static LISTENER_STARTED: Lazy<Mutex<bool>> = Lazy::new(|| Mutex::new(false));

#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
    // 初始化 mpv 实例
    let mut mpv_guard = MPV.lock().unwrap();
    match Mpv::new() {
        Ok(mpv) => {
            *mpv_guard = Some(mpv);
            env.message("mpv plugin loaded successfully")
        }
        Err(e) => {
            let msg = format!("Failed to create mpv instance: {}", e);
            env.message(&msg)?;
            anyhow::bail!(msg)
        }
    }
}
```

#### 关键函数说明：

1. **`play(path: String)`** - 播放视频
   - 调用 mpv 的 `loadfile` 命令加载视频文件

2. **`pause()` / `resume()`** - 暂停/恢复播放

3. **`get_time() -> f64`** - 获取当前播放时间（秒）
   - 内部启动后台线程每 50ms 更新 CURRENT_TIME
   - 这样 Emacs 端调用时能获取到最新的时间值

4. **`load_subtitle(path: String)`** - 加载字幕文件
   - 通过 mpv 的 `sub-file` 属性加载外部字幕

---

## 3. Elisp 模块实现

### 文件：mpv-subtitle.el

#### 3.1 变量定义

```elisp
(defvar mpv-subtitle-timer nil
  "定时器，用于定期更新字幕高亮")

(defvar mpv-subtitle-entries nil
  "解析后的字幕条目列表，每个元素为 (start-time end-time text)")

(defvar mpv-subtitle-last-index -1
  "上一次高亮的字幕索引，用于避免重复处理")
```

#### 3.2 SRT 解析

```elisp
(defun mpv-parse-srt-from-string (content)
  "解析 SRT 字符串，返回 ((start end text) ...)"
  (let ((entries nil)
        (blocks (split-string content "\r?\n\r?\n" t)))  ; 按空行分割
    (dolist (block blocks)
      (when (string-match "\\`[0-9]+\n\\([0-9:,]+\\) --> \\([0-9:,]+\\)" block)
        ;; 使用正则提取字幕块中的时间戳
        (let* ((start-str (match-string 1 block))
               (end-str (match-string 2 block))
               (start (mpv-parse-timestamp start-str))
               (end (mpv-parse-timestamp end-str))
               (text (car (last (split-string block "\n" t)))))
          (when start
            (push (list start (or end start) text) entries)))))
    (nreverse entries)))
```

关键点：
- SRT 格式：`序号\n开始时间 --> 结束时间\n文本内容\n\n`
- 使用 `split-string` 按空行分割成多个 block
- 每个 block 用正则提取时间戳和文本

```elisp
(defun mpv-parse-timestamp (ts)
  "将 '00:00:01,500' 转换为秒数 1.5"
  (when ts
    (if (string-match "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)[,\\.]\\([0-9]+\\)" ts)
        (let ((h (string-to-number (match-string 1 ts)))
              (m (string-to-number (match-string 2 ts)))
              (s (string-to-number (match-string 3 ts)))
              (ms (string-to-number (match-string 4 ts))))
          (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))
      nil)))
```

#### 3.3 字幕加载

```elisp
(defun mpv-load-subtitle-file (srt-path)
  "加载 SRT 文件并显示在 buffer 中"
  (let ((buf (get-buffer-create "*MPV Subtitle*"))
        (entries (with-temp-buffer
                   (insert-file-contents srt-path)
                   (mpv-parse-srt-from-string (buffer-string)))))
    (setq mpv-subtitle-entries entries)
    (with-current-buffer buf
      (erase-buffer)
      (insert-file-contents srt-path)  ; 直接插入原始内容
      ...)))
```

#### 3.4 字幕高亮更新（核心）

```elisp
(defun mpv-update-subtitle-highlight ()
  "根据当前播放时间更新字幕高亮"
  (let ((time (ignore-errors (mpv-get-time))))
    (when (and time mpv-subtitle-entries (get-buffer "*MPV Subtitle*"))
      ;; 找到当前时间对应的字幕索引
      (let* ((matches (cl-loop for e in mpv-subtitle-entries
                               for i from 0
                               when (and (<= (car e) time) (< time (cadr e)))
                               collect i))
             (idx (car matches)))
        ;; 只有索引变化时才更新高亮
        (when (and idx (numberp idx) (/= idx mpv-subtitle-last-index))
          (setq mpv-subtitle-last-index idx)
          (with-current-buffer "*MPV Subtitle*"
            ;; 移除旧的高亮
            (remove-overlays (point-min) (point-max) 'mpv-subtitle-highlight t)
            ;; 跳转到对应行
            (goto-char (point-min))
            (forward-line idx)
            ;; 创建新的高亮 overlay
            (let ((ov (make-overlay (point) (line-end-position))))
              (overlay-put ov 'mpv-subtitle-highlight t)
              (overlay-put ov 'face '(background-color . "yellow")))))))))
```

关键点：
1. 调用 Rust 的 `mpv-get-time` 获取当前播放时间
2. 用 `cl-loop` 遍历所有字幕条目，找到时间范围内的索引
3. 用 overlay 实现高亮：
   - `make-overlay` 创建高亮区域
   - `overlay-put` 设置属性，`face` 指定背景色
4. 记录 `mpv-subtitle-last-index` 避免重复更新

#### 3.5 自动更新定时器

```elisp
(defun mpv-subtitle-auto-update ()
  "启动定时器，每 0.1 秒更新一次高亮"
  (setq mpv-subtitle-timer
        (run-with-timer 0 0.1 #'mpv-update-subtitle-highlight)))
```

---

## 4. 核心函数详解

### 4.1 播放并加载字幕

```elisp
(defun mpv-play-with-subtitle (video-path)
  "播放视频并自动加载同名的 .srt 字幕"
  (condition-case err
      (progn
        ;; 确保模块已加载
        (or (fboundp 'mpv-get-time)
            (load "/path/to/libmpv.so"))
        ;; 播放视频
        (mpv-play video-path)
        ;; 查找同名 SRT 文件
        (let ((srt-path (concat (file-name-sans-extension video-path) ".srt")))
          (if (file-exists-p srt-path)
              (progn
                (mpv-load-subtitle-file srt-path)
                (mpv-subtitle-auto-update))
            (message "No subtitle file found"))))
    (error (message "Error: %s" err))))
```

---

## 5. 使用说明

### 5.1 编译 Rust 模块

```bash
cd /home/neo/.emacs.d/plugins/mpv
cargo build
```

### 5.2 在 Emacs 中使用

```elisp
;; 加载模块
(add-to-list 'load-path "/home/neo/.emacs.d/plugins/mpv/")
(require 'mpv-subtitle)

;; 播放视频（会自动加载同名字幕）
(mpv-play-with-subtitle "/path/to/video.mkv")

;; 或手动加载字幕
(mpv-load-subtitle-file "/path/to/video.srt")
```

### 5.3 测试

项目已包含测试文件：
- `test_video.mp4` - 10秒测试视频
- `test_video.srt` - 对应字幕

```elisp
(mpv-play-with-subtitle "/home/neo/.emacs.d/plugins/mpv/test_video.mp4")
```

---

## 6. 调试记录

### 问题1：SRT 解析 end time 为空
- **原因**：解析正则未正确捕获结束时间
- **解决**：修改正则表达式，确保同时捕获 start 和 end

### 问题2：字幕不匹配
- **原因**：end time 解析后与 start 相同（都是 0.0, 2.5 等）
- **解决**：检查正则匹配，正确传递 `match-string` 的参数

### 问题3：高亮不更新
- **原因**：条件判断 `(/= idx mpv-subtitle-last-index)` 未正确比较
- **解决**：初始化 `mpv-subtitle-last-index` 为 -1

### 问题4：Emacs 崩溃（rime 动态库冲突）
- **原因**：动态库符号冲突
- **解决**：延迟加载模块，确保 rime 先加载