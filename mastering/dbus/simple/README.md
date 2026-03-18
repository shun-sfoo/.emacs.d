# DBus 简单封装 (C++17)

基于 libdbus 的轻量级封装，提供类型安全的 C++ 接口。

## 文件说明

| 文件 | 说明 | 命名空间 |
|------|------|----------|
| `dbus_sync.h` | 同步调用 | `dbus_sync` |
| `dbus_async.h` | 异步调用 | `dbus_async` |

## 类说明

### Conn (连接类)
封装 D-Bus 连接，管理 Session/System Bus。

| 方法 | 说明 |
|------|------|
| `Conn(type)` | 构造函数，type 默认为 `DBUS_BUS_SESSION` |
| `call(dest, path, iface, method, timeout_ms)` | 同步调用远程方法，返回 `Msg` (仅 sync) |
| `callAsync(dest, path, iface, method, timeout_ms)` | 异步调用，返回 `PendingCall` (仅 async) |
| `hasPendingCall()` | 检查是否有待处理的调用 (仅 async) |
| `raw()` | 获取原始 `DBusConnection*` |

### Msg (消息类)
封装 D-Bus 消息，支持类型安全的参数读取。

| 方法 | 说明 |
|------|------|
| `get<T>()` | 获取返回值，支持 `int32_t`, `int64_t`, `double`, `bool`, `std::string` |
| `getArray<T>()` | 获取数组，返回 `std::vector<T>` |
| `getStringArray()` | 获取字符串数组 |
| `raw()` | 获取原始 `DBusMessage*` |

### PendingCall (异步调用类)
管理异步调用。

| 方法 | 说明 |
|------|------|
| `isReady()` | 检查回复是否已到达 |
| `stealReply()` | 获取回复消息，返回 `Msg` |
| `cancel()` | 取消异步调用 |
| `operator bool()` | 检查是否有效 |

### MsgBuilder (消息构建器)
构建方法调用消息，支持链式调用。

| 方法 | 说明 |
|------|------|
| `createMethodCall(dest, path, iface, method)` | 静态方法，创建方法调用 |
| `append(value)` | 添加参数，支持链式 |
| `raw()` | 获取原始 `DBusMessage*` |

### DbType<T> (类型映射)
模板特化映射 C++ 类型到 D-Bus 类型。

支持的类型：`int32_t`, `int64_t`, `double`, `bool`, `std::string`

## 使用示例

### 同步调用 (dbus_sync.h)
```cpp
#include "dbus_sync.h"
#include <iostream>

int main() {
    dbus_sync::Conn conn;
    
    auto reply = conn.call("com.example.Service", "/com/example/Calculator",
                           "com.example.Calculator", "Add");
    int64_t result = reply.get<int64_t>();
    std::cout << result << std::endl;
    
    return 0;
}
```

### 异步调用 (dbus_async.h)
```cpp
#include "dbus_async.h"
#include <iostream>

int main() {
    dbus_async::Conn conn;
    
    auto pending = conn.callAsync("com.example.Service", "/com/example/Object",
                                   "com.example.Interface", "GetValue");
    
    // ... 做其他事情 ...
    
    if (pending.isReady()) {
        auto reply = pending.stealReply();
        auto result = reply.get<std::string>();
        std::cout << result << std::endl;
    }
    
    return 0;
}
```

## 编译

```bash
make
```

或手动编译：
```bash
g++ -std=c++17 -o client client.cpp $(pkg-config --cflags --libs dbus-1)
```

### Makefile 自动变量

| 变量 | 含义 |
|------|------|
| `$@` | 目标文件 |
| `$<` | 第一个依赖文件 |
| `$^` | 所有依赖文件 |
| `$?` | 所有比目标更新的依赖文件 |

示例：
```makefile
$(CXX) $(CXXFLAGS) -o $@ $< $(LDFLAGS)
# 展开为: g++ -std=c++17 ... -o client_simple client_simple.cpp -ldbus-1
```

## 封装原因

### 1. RAII 资源管理
原生 API 需要手动调用 `dbus_connection_unref()` / `dbus_message_unref()`，封装后自动管理内存，避免泄漏。

### 2. 类型安全
原生 `dbus_message_get_args()` 需要手动指定类型码，用模板 `get<T>()` 自动推导。

### 3. 异常处理
原生 API 通过 `DBusError` 返回错误，封装后抛异常，简化错误处理。

### 4. 移动语义
`Msg` 支持移动，避免复制，底层 `DBusMessage*` 转移所有权。

### 5. 智能指针 vs 当前实现

libdbus 本身有引用计数 (`dbus_message_ref()` / `dbus_message_unref()`)。

**如果用智能指针**：
```cpp
using DBusConn = std::unique_ptr<DBusConnection, decltype(&dbus_connection_unref)>;
using DBusMsg = std::unique_ptr<DBusMessage, decltype(&dbus_message_unref)>;

DBusConn conn(dbus_bus_get(DBUS_BUS_SESSION, &error), dbus_connection_unref);
```

**对比**：

| | 智能指针 | 当前实现 |
|--|---------|---------|
| 代码量 | 多一行别名 | 相同 |
| 可读性 | 更 idiomatic | 稍繁琐 |
| 性能 | 相同 | 相同 |
| libdbus 重叠 | 有 | 无 |

**结论**：当前实现够用。libdbus 本身是 C 风格引用计数，再包一层智能指针意义不大。

### 对比

**原生 C API**:
```cpp
DBusError error;
dbus_error_init(&error);
DBusConnection* conn = dbus_bus_get(DBUS_BUS_SESSION, &error);
// ... 各种检查 ...
dbus_message_unref(msg);
dbus_connection_unref(conn);
```

**封装后**:
```cpp
dbus_sync::Conn conn;
auto reply = conn.call(...);
auto result = reply.get<int64_t>();
```

## Session vs System 总线

### 1. 通过总线类型判断连接

```cpp
// Session 总线
DBusConnection* session = dbus_bus_get(DBUS_BUS_SESSION, &error);

// System 总线  
DBusConnection* system = dbus_bus_get(DBUS_BUS_SYSTEM, &error);
```

### 2. 查看服务名

```bash
# 查看 Session 总线上的服务
dbus-send --session --dest=org.freedesktop.DBus \
  /org/freedesktop/DBus org.freedesktop.DBus.ListNames

# 查看 System 总线上的服务
dbus-send --system --dest=org.freedesktop.DBus \
  /org/freedesktop/DBus org.freedesktop.DBus.ListNames
```

### 常见服务分布

| 服务 | Session | System |
|------|---------|--------|
| `org.freedesktop.Notifications` | ✓ | |
| `org.bluez` | | ✓ |
| `org.freedesktop.login1` | | ✓ |
| `org.gnome.*` | ✓ | |
| `com.example.MyApp` | ✓ | ✓ (取决于配置) |

### 代码中判断

```cpp
bool isSessionBus(DBusConnection* conn) {
    // D-Bus 没有直接 API，可以通过尝试获取 bus address 判断
    // 或者维护一个已知服务列表
    return true; // 简化处理
}
```

**注意**：实际使用中，连接哪个总线是开发者决定的，不是自动发现的。
