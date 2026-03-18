#pragma once

#include <dbus/dbus.h>
#include <string>
#include <vector>
#include <stdexcept>
#include <type_traits>

namespace dbus_sync {

class Msg;

class Conn {
public:
    Conn(DBusBusType type = DBUS_BUS_SESSION) {
        dbus_error_init(&error_);
        conn_ = dbus_bus_get(type, &error_);
        if (dbus_error_is_set(&error_)) {
            throw std::runtime_error(std::string("Connection error: ") + error_.message);
        }
    }

    ~Conn() {
        if (conn_) dbus_connection_unref(conn_);
    }

    Conn(const Conn&) = delete;
    Conn& operator=(const Conn&) = delete;

    DBusConnection* raw() { return conn_; }

    Msg call(const std::string& dest, const std::string& path,
             const std::string& iface, const std::string& method,
             int timeout_ms = 1000);

private:
    DBusConnection* conn_;
    DBusError error_;
};

class Msg {
public:
    Msg(DBusMessage* msg = nullptr) : msg_(msg) {}
    ~Msg() { if (msg_) dbus_message_unref(msg_); }

    Msg(const Msg&) = delete;
    Msg& operator=(const Msg&) = delete;

    Msg(Msg&& other) noexcept : msg_(other.msg_) { other.msg_ = nullptr; }
    Msg& operator=(Msg&& other) noexcept {
        if (this != &other) {
            if (msg_) dbus_message_unref(msg_);
            msg_ = other.msg_;
            other.msg_ = nullptr;
        }
        return *this;
    }

    DBusMessage* raw() { return msg_; }
    explicit operator bool() const { return msg_ != nullptr; }

    template<typename T>
    T get() {
        if constexpr (std::is_same_v<T, std::string>) {
            char* str;
            if (!dbus_message_get_args(msg_, &error_, DBUS_TYPE_STRING, &str, DBUS_TYPE_INVALID)) {
                throw std::runtime_error("Failed to get string argument");
            }
            return std::string(str);
        } else {
            T value;
            int type = DBUS_TYPE_INVALID;
            if constexpr (std::is_same_v<T, int32_t>) type = DBUS_TYPE_INT32;
            else if constexpr (std::is_same_v<T, int64_t>) type = DBUS_TYPE_INT64;
            else if constexpr (std::is_same_v<T, double>) type = DBUS_TYPE_DOUBLE;
            else if constexpr (std::is_same_v<T, bool>) type = DBUS_TYPE_BOOLEAN;
            
            if (!dbus_message_get_args(msg_, &error_, type, &value, DBUS_TYPE_INVALID)) {
                throw std::runtime_error("Failed to get argument");
            }
            return value;
        }
    }

    template<typename T>
    std::vector<T> getArray() {
        DBusMessageIter iter;
        dbus_message_iter_init(msg_, &iter);

        std::vector<T> result;
        if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_ARRAY) {
            DBusMessageIter sub;
            dbus_message_iter_recurse(&iter, &sub);
            while (dbus_message_iter_get_arg_type(&sub) != DBUS_TYPE_INVALID) {
                T value;
                dbus_message_iter_get_basic(&sub, &value);
                result.push_back(value);
                dbus_message_iter_next(&sub);
            }
        }
        return result;
    }

    std::vector<std::string> getStringArray() {
        DBusMessageIter iter;
        dbus_message_iter_init(msg_, &iter);

        std::vector<std::string> result;
        if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_ARRAY) {
            DBusMessageIter sub;
            dbus_message_iter_recurse(&iter, &sub);
            while (dbus_message_iter_get_arg_type(&sub) != DBUS_TYPE_INVALID) {
                char* str;
                dbus_message_iter_get_basic(&sub, &str);
                result.push_back(str);
                dbus_message_iter_next(&sub);
            }
        }
        return result;
    }

private:
    DBusMessage* msg_;
    DBusError error_;
};

inline Msg Conn::call(const std::string& dest, const std::string& path,
                      const std::string& iface, const std::string& method,
                      int timeout_ms) {
    DBusMessage* msg = dbus_message_new_method_call(dest.c_str(), path.c_str(),
                                                      iface.c_str(), method.c_str());
    if (!msg) throw std::runtime_error("Failed to create message");

    DBusMessage* reply = dbus_connection_send_with_reply_and_block(conn_, msg, timeout_ms, &error_);
    dbus_message_unref(msg);

    if (dbus_error_is_set(&error_)) {
        throw std::runtime_error(std::string("Method call error: ") + error_.message);
    }

    return Msg(reply);
}

template<typename T>
struct DbType { static int value() { return DBUS_TYPE_INVALID; } };

template<> struct DbType<int32_t> { static int value() { return DBUS_TYPE_INT32; } };
template<> struct DbType<int64_t> { static int value() { return DBUS_TYPE_INT64; } };
template<> struct DbType<double> { static int value() { return DBUS_TYPE_DOUBLE; } };
template<> struct DbType<bool> { static int value() { return DBUS_TYPE_BOOLEAN; } };
template<> struct DbType<std::string> { static int value() { return DBUS_TYPE_STRING; } };

class MsgBuilder {
public:
    MsgBuilder() = default;

    static MsgBuilder createMethodCall(const std::string& dest,
                                        const std::string& path,
                                        const std::string& iface,
                                        const std::string& method) {
        MsgBuilder builder;
        builder.msg_ = dbus_message_new_method_call(dest.c_str(), path.c_str(),
                                                      iface.c_str(), method.c_str());
        return builder;
    }

    ~MsgBuilder() { if (msg_) dbus_message_unref(msg_); }

    MsgBuilder(const MsgBuilder&) = delete;
    MsgBuilder& operator=(const MsgBuilder&) = delete;

    MsgBuilder(MsgBuilder&& other) noexcept : msg_(other.msg_) { other.msg_ = nullptr; }
    MsgBuilder& operator=(MsgBuilder&& other) noexcept {
        if (this != &other) {
            if (msg_) dbus_message_unref(msg_);
            msg_ = other.msg_;
            other.msg_ = nullptr;
        }
        return *this;
    }

    template<typename T>
    MsgBuilder& append(T value) {
        dbus_message_append_args(msg_, DbType<T>::value(), &value, DBUS_TYPE_INVALID);
        return *this;
    }

    MsgBuilder& append(const std::string& value) {
        const char* str = value.c_str();
        dbus_message_append_args(msg_, DBUS_TYPE_STRING, &str, DBUS_TYPE_INVALID);
        return *this;
    }

    DBusMessage* raw() { return msg_; }

private:
    DBusMessage* msg_ = nullptr;
};

} // namespace dbus_sync
