# emacs-module-rs 开发步骤与错误处理总结

## 一、开发步骤

### 1.1 项目初始化

#### 创建 Cargo 项目
```bash
cargo new --lib mpv-emacs
cd mpv-emacs
```

#### 配置 Cargo.toml
```toml
[package]
name = "mpv"
version = "0.1.0"
edition = "2024"

[lib]
crate-type = ["cdylib"]
name = "mpv"  # 必须指定，生成正确的 .so 文件名

[dependencies]
emacs = "0.20.0"  # 核心依赖
anyhow = "1.0.102" # 错误处理
once_cell = "1.21.4"  # 静态全局变量
```

#### 启用调试功能（开发时）
```toml
emacs = { version = "0.20.0", features = ["debug"] }
```

---

### 1.2 基础代码结构

```rust
use emacs::{defun, Env, Result, Value};

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("模块加载成功")
}

#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    env.message(&format!("Hello, {}!", name))
}
```

---

### 1.3 构建与测试

#### 构建
```bash
cargo build
```

#### 创建符号链接
```bash
ln -sf target/debug/libmpv.so target/debug/mpv.so
```

#### 测试加载
```bash
emacs --batch --eval "(load-file \"/path/to/mpv.so\")"
```

#### 列出导出的函数
```bash
emacs --batch \
  --eval "(load-file \"/path/to/mpv.so\")" \
  --eval "(require 'mpv)" \
  --eval "(all-completions \"mpv-\" obarray 'functionp)"
```

---

## 二、错误处理

### 2.1 错误类型概述

emacs-module-rs 中的错误处理有三种方式：

| 方式 | 说明 | 使用场景 |
|------|------|----------|
| `env.message()` | 显示信息，不中断执行 | 成功/失败的状态提示 |
| `anyhow::bail!()` | 返回错误，中断执行 | 业务逻辑失败 |
| `env.signal()` | 抛出 Lisp 错误 | 需要 Lisp 端捕获 |

---

### 2.2 方式一：env.message() - 信息提示

**作用**：向 Emacs minibuffer 输出信息，执行继续

```rust
#[defun]
fn say_hello(env: &Env, name: String) -> Result<Value<'_>> {
    // 显示成功消息
    env.message(&format!("Hello, {}!", name))
}
```

**Emacs 中调用**：
```elisp
(mpv-say-hello "World")
;; minibuffer 显示: Hello, World!
```

---

### 2.3 方式二：anyhow::bail!() - 返回错误

**作用**：返回错误，Emacs 端会显示错误信息

```rust
#[defun]
fn play(env: &Env, path: String) -> Result<Value<'_>> {
    let mpv_guard = MPV.lock().unwrap();
    let mpv = match mpv_guard.as_ref() {
        Some(m) => m,
        None => {
            // 使用 anyhow::bail! 返回错误
            anyhow::bail!("mpv not initialized")
        }
    };
    
    if let Err(e) = mpv.command("loadfile", &[&path]) {
        anyhow::bail!(format!("Failed to play: {}", e));
    }
    
    env.message(&format!("Playing: {}", path))
}
```

**Emacs 中调用**：
```elisp
(mpv-play "/nonexistent/video.mp4")
;; 报错: Wrong type argument: error, "Failed to play: ..."
```

**注意**：
- `anyhow::bail!()` 需要 `use anyhow::bail;`
- 错误信息会被 Emacs 包装为 `error` 信号

---

### 2.4 方式三：env.signal() - Lisp 错误信号（不推荐）

**作用**：显式抛出 Lisp 错误信号

```rust
// 注意：env.signal 在 emacs 0.20.0 中的 API 有变化
// 建议使用 anyhow::bail! 代替
```

---

### 2.5 完整错误处理模式

```rust
use emacs::{defun, Env, Result, Value};
use anyhow::bail;

#[defun]
fn my_function(env: &Env, arg: String) -> Result<Value<'_>> {
    // 1. 先显示开始处理的信息（可选）
    env.message(&format!("Processing: {}", arg))?;

    // 2. 业务逻辑
    let result = do_something(&arg)?;

    // 3. 显示成功结果
    env.message(&format!("Result: {}", result))
}
```

---

### 2.6 init 函数中的错误处理

init 函数的错误会导致模块加载失败：

```rust
#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
    match some_initialization() {
        Ok(_) => {
            // 成功：显示消息并返回
            env.message("Module loaded successfully")
        }
        Err(e) => {
            // 失败：显示错误消息，然后返回错误
            let msg = format!("Init failed: {}", e);
            env.message(&msg)?;  // 先显示错误
            bail!(msg)          // 再返回错误
        }
    }
}
```

---

## 三、常用调试命令

### 3.1 批量测试脚本

```bash
#!/bin/bash
# test-module.sh

SO_FILE="/path/to/mpv.so"

# 测试1: 加载模块
echo "=== Test 1: Load module ==="
emacs --batch --eval "(load-file \"$SO_FILE\")"

# 测试2: 列出函数
echo "=== Test 2: List functions ==="
emacs --batch \
  --eval "(load-file \"$SO_FILE\")" \
  --eval "(require 'mpv)" \
  --eval "(message \"%S\" (all-completions \"mpv-\" obarray 'functionp))"

# 测试3: 调用函数
echo "=== Test 3: Call function ==="
emacs --batch \
  --eval "(load-file \"$SO_FILE\")" \
  --eval "(require 'mpv)" \
  --eval "(mpv-play \"/tmp/test.mp4\")"
```

### 3.2 查看符号

```bash
# 查看导出的符号
nm -C target/debug/mpv.so | grep -E "emacs|init|defun"

# 查看依赖
ldd target/debug/mpv.so | grep mpv
```

### 3.3 检查版本

```bash
# 系统 mpv 版本
pkg-config --modversion mpv

# Rust 依赖版本
cargo tree -p libmpv
```

---

## 四、常见问题

### 4.1 函数名找不到

**问题**：`void-function` 错误

**解决**：确保使用 `require 'mpv`

```elisp
;; 错误
(load-file "mpv.so")
(mpv-play "video.mp4")  ; void-function

;; 正确
(load-file "mpv.so")
(require 'mpv)
(mpv-play "video.mp4")
```

### 4.2 函数名前缀重复

**问题**：函数 `mpv_play` 变成 `mpv-mpv-play`

**解决**：函数名不要包含模块名

```rust
// 错误
#[defun]
fn mpv_play(env: &Env, path: String) -> Result<Value<'_>> {}

// 正确
#[defun]
fn play(env: &Env, path: String) -> Result<Value<'_>> {}
```

### 4.3 版本不匹配

**问题**：`VersionMismatch` 错误

**解决**：使用与系统 mpv 兼容的 libmpv 版本

```bash
# 查看系统版本
pkg-config --modversion mpv
# 输出: 2.5.0

# 修改 Cargo.toml 使用兼容版本
libmpv = { version = "2.0.2-fork.1", package = "libmpv-sirno" }
```

---

## 五、最佳实践

### 5.1 错误处理原则

1. **始终返回结果**：使用 `Result<Value<'_>>` 返回类型
2. **先显示消息**：在返回错误前先用 `env.message()` 显示信息
3. **使用 anyhow**：`anyhow::bail!()` 简化错误传播

### 5.2 代码组织

```rust
// 1. 导入
use emacs::{defun, Env, Result, Value};
use anyhow::bail;

// 2. GPL 声明
emacs::plugin_is_GPL_compatible!();

// 3. 模块初始化
#[emacs::module(name = "mpv")]
fn init(env: &Env) -> Result<Value<'_>> {
    // 初始化代码
}

// 4. 导出函数
#[defun]
fn function_name(env: &Env, param: Type) -> Result<Value<'_>> {
    // 处理参数
    // 执行业务逻辑
    // 返回结果
}
```

### 5.3 开发时启用 debug

```toml
# Cargo.toml
[profile.dev]
opt-level = 0

[dependencies]
emacs = { version = "0.20.0", features = ["debug"] }
```

调试输出会显示：
- 值保护（rooting）信息
- 内存管理详情
- 初始化过程
