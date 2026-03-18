#pragma once

#include <dbus/dbus.h>
#include <string>
#include <vector>
#include <stdexcept>
#include <type_traits>

namespace dbus_async {

class Msg;
class PendingCall;

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

    PendingCall callAsync(const std::string& dest, const std::string& path,
                          const std::string& iface, const std::string& method,
                          int timeout_ms = 1000);

    bool hasPendingCall();

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

class PendingCall {
public:
    PendingCall(DBusPendingCall* pending = nullptr) : pending_(pending) {}
    ~PendingCall() { if (pending_) dbus_pending_call_unref(pending_); }

    PendingCall(const PendingCall&) = delete;
    PendingCall& operator=(const PendingCall&) = delete;

    PendingCall(PendingCall&& other) noexcept : pending_(other.pending_) { other.pending_ = nullptr; }
    PendingCall& operator=(PendingCall&& other) noexcept {
        if (this != &other) {
            if (pending_) dbus_pending_call_unref(pending_);
            pending_ = other.pending_;
            other.pending_ = nullptr;
        }
        return *this;
    }

    bool isReady() const { return pending_ && dbus_pending_call_get_completed(pending_); }

    Msg stealReply() {
        if (!pending_) return Msg(nullptr);
        DBusMessage* reply = dbus_pending_call_steal_reply(pending_);
        return Msg(reply);
    }

    void cancel() {
        if (pending_) dbus_pending_call_cancel(pending_);
    }

    explicit operator bool() const { return pending_ != nullptr; }

private:
    DBusPendingCall* pending_;
};

inline PendingCall Conn::callAsync(const std::string& dest, const std::string& path,
                                   const std::string& iface, const std::string& method,
                                   int timeout_ms) {
    DBusMessage* msg = dbus_message_new_method_call(dest.c_str(), path.c_str(),
                                                      iface.c_str(), method.c_str());
    if (!msg) throw std::runtime_error("Failed to create message");

    DBusPendingCall* pending = nullptr;
    if (!dbus_connection_send_with_reply(conn_, msg, &pending, timeout_ms)) {
        dbus_message_unref(msg);
        throw std::runtime_error("Failed to send async call");
    }
    dbus_message_unref(msg);

    if (!pending) {
        throw std::runtime_error("Failed to create pending call");
    }

    return PendingCall(pending);
}

inline bool Conn::hasPendingCall() {
    return dbus_connection_get_dispatch_status(conn_) != DBUS_DISPATCH_COMPLETE;
}

} // namespace dbus_async
