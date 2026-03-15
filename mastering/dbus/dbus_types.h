#pragma once

#include <string>
#include <vector>
#include <memory>
#include <functional>
#include <optional>
#include <variant>
#include <chrono>
#include <future>
#include <map>
#include <algorithm>
#include <cstdint>

namespace dbus {

// 类型别名 - 使用 C++17特性
template<typename... Ts>
using Variant = std::variant<Ts...>;

using String = std::string;
using Int32 = int32_t;
using Int64 = int64_t;
using Double = double;
using Boolean = bool;
using ObjectPath = std::string;

template<typename T>
struct Array : public std::vector<T> {
    using std::vector<T>::vector;
};

template<typename K, typename V>
struct Dict : public std::map<K, V> {
    using std::map<K, V>::map;
};

// 异常类
class DbusException : public std::runtime_error {
public:
    explicit DbusException(const std::string& msg) : std::runtime_error(msg) {}
};

class ConnectionError : public DbusException {
public:
    explicit ConnectionError(const std::string& msg) : DbusException(msg) {}
};

class MethodCallError : public DbusException {
public:
    explicit MethodCallError(const std::string& msg) : DbusException(msg) {}
};

} // namespace dbus
