# MPV Emacs 插件开发调试总结

## 问题概述

通过 libmpv 库实现 Emacs 对 mpv 播放器的控制功能。

---

## 调试过程

### 1. 初次构建与加载

#### 1.1 编译项目
```bash
cd /home/neo/.emacs.d/plugins/mpv
cargo build
```

#### 1.2 检查生成的库文件
```bash
ls -la target/debug/*.so
# 输出: libmpv.so (注意：不是 mpv.so)
```

#### 1.3 验证 Emacs 加载模块
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/libmpv.so\")"
```

---

### 2. 版本不匹配问题

#### 2.1 错误信息
```
Failed to create mpv instance: VersionMismatch { linked: 65644, loaded: 131077 }
Error during initialization: "Failed to create mpv instance: VersionMismatch..."
Module initialization failed: "...mpv.so", 1
```

#### 2.2 定位问题

**分析步骤：**

1. **理解错误信息**
   - `linked: 65644` = 0x10014 = mpv API version 1.108
   - `loaded: 131077` = 0x20005 = mpv API version 2.0.5

2. **验证系统 mpv 版本**
   ```bash
   # 方法1: pkg-config
   pkg-config --modversion mpv
   # 输出: 2.5.0
   
   # 方法2: 查看 so 文件
   ls -l /usr/lib/libmpv.so*
   # 输出: libmpv.so.2 -> libmpv.so.2.5.0
   
   # 方法3: ldd 查看依赖
   ldd target/debug/libmpv.so | grep mpv
   ```

3. **检查 Rust 依赖版本**
   ```bash
   # 查看 Cargo.lock 中 libmpv-sys 的版本
   grep -A5 'name = "libmpv-sys"' Cargo.lock
   # 或直接查看
   cargo tree -p libmpv-sys
   ```

#### 2.3 解决版本不匹配

1. 搜索兼容的 libmpv 包：
   ```bash
   cargo search libmpv --limit 10
   ```

2. 尝试 libmpv-sirno（社区维护的新版本）：
   ```bash
   # 查看可用版本
   cargo info libmpv-sirno
   ```

3. 修改 Cargo.toml：
   ```toml
   # 原配置
   libmpv = "2.0.1"
   
   # 修改后
   libmpv = { version = "2.0.2-fork.1", package = "libmpv-sirno" }
   ```

4. 重新构建：
   ```bash
   cargo clean && cargo build
   ```

---

### 3. 库文件名问题

#### 3.1 问题
编译后生成的是 `libmpv.so`，但 Emacs 期望 `mpv.so`

#### 3.2 验证
```bash
ls -la target/debug/*.so
# 输出: libmpv.so (不是 mpv.so)
```

#### 3.3 解决
```bash
# 方法1: 创建符号链接
ln -sf target/debug/libmpv.so target/debug/mpv.so

# 方法2: 修改 Cargo.toml 添加 lib name
[lib]
crate-type = ["cdylib"]
name = "mpv"
```

---

### 4. Emacs 模块加载与函数调用问题

#### 4.1 初次加载测试
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")"
```

**输出：**
```
Loading /home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so (module)...
mpv plugin loaded successfully
```

#### 4.2 调用函数失败
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")" \
       --eval "(mpv-play \"/tmp/test.mp4\")"
```

**错误：**
```
Symbol's function definition is void: mpv-play
```

#### 4.3 调试步骤

**步骤1：检查符号是否被创建**
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(intern \"mpv:say-hello\")"
```
**输出：** `mpv:say-hello` (符号存在)

**步骤2：检查符号是否有函数定义**
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(symbol-function (intern \"mpv:say-hello\"))"
```
**输出：** `nil` (没有函数定义！)

**步骤3：列出所有导出的函数**
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(all-completions \"mpv\" obarray 'functionp)"
```
**输出：** `("mpv-say-hello")`

**结论：** 函数名是 `mpv-say-hello`（用连字符），不是 `mpv:say-hello`（用冒号）

#### 4.4 验证正确函数名
```bash
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(mpv-say-hello \"World\")"
```
**输出：** `Hello, World!`

---

### 5. 函数名前缀重复问题

#### 5.1 问题
函数 `mpv_play` 在 Emacs 中变成 `mpv-mpv-play`

#### 5.2 验证
```rust
#[defun]
fn mpv_play(env: &Env, path: String) -> Result<Value<'_>> { ... }
```

```bash
emacs --batch --eval "(load-file \".../mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(all-completions \"mpv-\" obarray 'functionp)"
```
**输出：** `("mpv-mpv-stop" "mpv-mpv-pause" "mpv-mpv-resume" "mpv-mpv-play")`

#### 5.3 解决
将函数名改为不包含模块名前缀：
```rust
#[defun]
fn play(env: &Env, path: String) -> Result<Value<'_>> { ... }
// 调用时: (mpv-play "path")
```

---

### 6. require 的重要性

#### 6.1 测试对比

**错误方式：**
```bash
emacs --batch --eval "(load-file \".../mpv.so\")" \
       --eval "(mpv-play \"test.mp4\")"
```
**结果：** `void-function`

**正确方式：**
```bash
emacs --batch --eval "(load-file \".../mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(mpv-play \"test.mp4\")"
```
**结果：** `Playing: test.mp4`

---

### 7. 启用调试功能

#### 7.1 添加 debug feature
```toml
emacs = { version = "0.20.0", features = ["debug"] }
```

#### 7.2 重新构建
```bash
cargo build
ln -sf target/debug/libmpv.so target/debug/mpv.so
```

#### 7.3 查看调试输出
```bash
emacs --batch --eval "(load-file \".../mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(mpv-play \"test.mp4\")"
```

**输出示例：**
```
Loading .../mpv.so (module)...
mpv plugin loaded successfully
Unrooting 55 values protected by Env { ... }
Playing: test.mp4
```

---

### 8. 查看生成的符号

使用 `nm` 工具查看导出的符号：
```bash
nm -C target/debug/mpv.so | grep -E "emacs|mpv|init"
```

**输出：**
```
0000000000033470 T emacs_module_init
00000000000334f0 T emacs_rs_module_init
0000000000032370 t _ZN3mpv17__emr_O_say_hello...
0000000000033330 T _ZN4init17h448bced00ef29583E
```

---

## 最终代码结构

### Cargo.toml

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

### src/lib.rs

```rust
use emacs::{defun, Env, Result, Value};
use libmpv::Mpv;
use once_cell::sync::Lazy;
use std::sync::Mutex;

emacs::plugin_is_GPL_compatible!();

static MPV: Lazy<Mutex<Option<Mpv>>> = Lazy::new(|| Mutex::new(None));

#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
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

#[defun]
fn play(env: &Env, path: String) -> Result<Value<'_>> { ... }

#[defun]
fn stop(env: &Env) -> Result<Value<'_>> { ... }

#[defun]
fn pause(env: &Env) -> Result<Value<'_>> { ... }

#[defun]
fn resume(env: &Env) -> Result<Value<'_>> { ... }
```

---

## 构建与使用

### 构建
```bash
cd /home/neo/.emacs.d/plugins/mpv
cargo build
ln -sf target/debug/libmpv.so target/debug/mpv.so
```

### 验证模块
```bash
# 基本加载测试
emacs --batch --eval "(load-file \"/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so\")"

# 列出所有函数
emacs --batch --eval "(load-file \".../mpv.so\")" \
       --eval "(require 'mpv)" \
       --eval "(all-completions \"mpv-\" obarray 'functionp)"
```

### 在 Emacs 中使用
```elisp
;; 加载模块
(load "/home/neo/.emacs.d/plugins/mpv/target/debug/mpv.so")

;; 或者添加到配置
(add-to-list 'load-path "/home/neo/.emacs.d/plugins/mpv/target/debug/")
(require 'mpv)

;; 播放视频
(mpv-play "/path/to/video.mp4")

;; 暂停
(mpv-pause)

;; 继续
(mpv-resume)

;; 停止
(mpv-stop)
```

---

## 关键经验总结

1. **版本匹配是核心**：系统 mpv 库版本必须与 Rust libmpv 版本兼容
   - 查看系统版本：`pkg-config --modversion mpv`
   - 错误信息中的数字需要转换为十六进制来理解

2. **Emacs 模块函数命名规则**：
   - 使用连字符 `-` 分隔，而非冒号 `:`
   - 函数名不要重复模块名前缀

3. **模块加载需要 require**：加载后必须执行 `(require 'mpv)` 才能调用函数

4. **启用 debug feature**：开发时启用可以获得有价值的调试信息

5. **lib 名称配置**：使用 `name = "mpv"` 确保生成正确的 .so 文件名

6. **常用调试命令**：
   ```bash
   # 查看依赖
   ldd target/debug/mpv.so | grep mpv
   
   # 查看符号
   nm -C target/debug/mpv.so | grep -E "init|mpv"
   
   # 批量测试 Emacs 函数
   emacs --batch --eval "(load-file \"...mpv.so\")" --eval "(require 'mpv)" \
          --eval "(message \"%S\" (all-completions \"mpv-\" obarray 'functionp))"
   ```
