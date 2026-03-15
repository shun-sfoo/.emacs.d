#pragma once

#include "dbus_types.h"
#include <dbus/dbus.h>
#include <memory>
#include <functional>
#include <string>
#include <vector>
#include <optional>
#include <variant>
#include <chrono>
#include <future>
#include <iostream>
#include <array>
#include <cstring>

namespace dbus {

template<typename T, void(*FreeFunc)(T*)>
class UniquePtr {
public:
    UniquePtr() = default;
    explicit UniquePtr(T* ptr) : ptr_(ptr) {}
    
    ~UniquePtr() {
        if (ptr_) FreeFunc(ptr_);
    }
    
    UniquePtr(const UniquePtr&) = delete;
    UniquePtr& operator=(const UniquePtr&) = delete;
    
    UniquePtr(UniquePtr&& other) noexcept : ptr_(other.ptr_) {
        other.ptr_ = nullptr;
    }
    
    UniquePtr& operator=(UniquePtr&& other) noexcept {
        if (this != &other) {
            if (ptr_) FreeFunc(ptr_);
            ptr_ = other.ptr_;
            other.ptr_ = nullptr;
        }
        return *this;
    }
    
    T* get() const { return ptr_; }
    T* operator->() const { return ptr_; }
    explicit operator bool() const { return ptr_ != nullptr; }
    
    void reset(T* ptr = nullptr) {
        if (ptr_) FreeFunc(ptr_);
        ptr_ = ptr;
    }
    
    T* release() {
        T* tmp = ptr_;
        ptr_ = nullptr;
        return tmp;
    }

private:
    T* ptr_ = nullptr;
};

using ConnectionPtr = UniquePtr<DBusConnection, &dbus_connection_unref>;
using MessagePtr = UniquePtr<DBusMessage, &dbus_message_unref>;
using ErrorPtr = UniquePtr<DBusError, &dbus_error_free>;

enum class BusType : int {
    Session = DBUS_BUS_SESSION,
    System = DBUS_BUS_SYSTEM,
    Starter = DBUS_BUS_STARTER
};

enum class MessageType : int {
    MethodCall = DBUS_MESSAGE_TYPE_METHOD_CALL,
    MethodReturn = DBUS_MESSAGE_TYPE_METHOD_RETURN,
    Signal = DBUS_MESSAGE_TYPE_SIGNAL,
    Error = DBUS_MESSAGE_TYPE_ERROR
};

class Connection {
public:
    explicit Connection(BusType busType) {
        DBusError error;
        dbus_error_init(&error);
        
        connection_ = ConnectionPtr(dbus_bus_get(static_cast<DBusBusType>(busType), &error));
        if (!connection_) {
            throw ConnectionError(error.message ? error.message : "Failed to connect to bus");
        }
        dbus_error_free(&error);
    }
    
    Connection(const Connection&) = delete;
    Connection& operator=(const Connection&) = delete;
    Connection(Connection&&) = default;
    Connection& operator=(Connection&&) = default;
    
    DBusConnection* raw() const { return connection_.get(); }
    
    void requestName(const std::string& name, uint32_t flags = 0) {
        DBusError error;
        dbus_error_init(&error);
        
        int ret = dbus_bus_request_name(raw(), name.c_str(), flags, &error);
        if (dbus_error_is_set(&error)) {
            throw ConnectionError(error.message);
        }
        
        if (ret != DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER) {
            throw ConnectionError("Failed to acquire service name: " + name);
        }
        dbus_error_free(&error);
    }
    
    void releaseName(const std::string& name) {
        DBusError error;
        dbus_error_init(&error);
        dbus_bus_release_name(raw(), name.c_str(), &error);
        if (dbus_error_is_set(&error)) {
            std::cerr << "Warning: " << error.message << std::endl;
        }
        dbus_error_free(&error);
    }
    
    void addMatch(const std::string& rule) {
        DBusError error;
        dbus_error_init(&error);
        dbus_bus_add_match(raw(), rule.c_str(), &error);
        if (dbus_error_is_set(&error)) {
            throw ConnectionError(error.message);
        }
        dbus_error_free(&error);
    }
    
    MessagePtr sendWithReply(MessagePtr& message, int timeout = -1) {
        DBusError error;
        dbus_error_init(&error);
        
        auto* reply = dbus_connection_send_with_reply_and_block(
            raw(), message.get(), timeout, &error
        );
        
        if (dbus_error_is_set(&error)) {
            throw MethodCallError(error.message);
        }
        dbus_error_free(&error);
        
        return MessagePtr(reply);
    }
    
    bool send(MessagePtr& message, uint32_t* serial = nullptr) {
        return dbus_connection_send(raw(), message.get(), serial);
    }
    
    void enterLoop() {
        while (true) {
            dbus_connection_read_write(raw(), 0);
            auto* msg = dbus_connection_borrow_message(raw());
            if (msg) {
                dbus_connection_return_message(raw(), msg);
                break;
            }
        }
    }

private:
    ConnectionPtr connection_;
};

class Message {
public:
    static MessagePtr createMethodCall(
        const std::string& destination,
        const std::string& path,
        const std::string& interface,
        const std::string& method
    ) {
        auto* msg = dbus_message_new_method_call(
            destination.c_str(),
            path.c_str(),
            interface.c_str(),
            method.c_str()
        );
        
        if (!msg) {
            throw DbusException("Failed to create method call message");
        }
        return MessagePtr(msg);
    }
    
    static MessagePtr createSignal(
        const std::string& path,
        const std::string& interface,
        const std::string& signalName
    ) {
        auto* msg = dbus_message_new_signal(
            path.c_str(),
            interface.c_str(),
            signalName.c_str()
        );
        
        if (!msg) {
            throw DbusException("Failed to create signal message");
        }
        return MessagePtr(msg);
    }
    
    static MessagePtr createMethodReturn(const MessagePtr& methodCall) {
        auto* msg = dbus_message_new_method_return(methodCall.get());
        if (!msg) {
            throw DbusException("Failed to create method return message");
        }
        return MessagePtr(msg);
    }
    
    static MessagePtr createError(
        const MessagePtr& methodCall,
        const std::string& errorName,
        const std::string& errorMessage
    ) {
        auto* msg = dbus_message_new_error(
            methodCall.get(),
            errorName.c_str(),
            errorMessage.c_str()
        );
        if (!msg) {
            throw DbusException("Failed to create error message");
        }
        return MessagePtr(msg);
    }
    
    static MessageType getType(const MessagePtr& msg) {
        return static_cast<MessageType>(dbus_message_get_type(msg.get()));
    }
    
    static std::string getInterface(const MessagePtr& msg) {
        const char* iface = dbus_message_get_interface(msg.get());
        return iface ? std::string(iface) : std::string();
    }
    
    static std::string getMember(const MessagePtr& msg) {
        const char* member = dbus_message_get_member(msg.get());
        return member ? std::string(member) : std::string();
    }
    
    static std::string getPath(const MessagePtr& msg) {
        const char* path = dbus_message_get_path(msg.get());
        return path ? std::string(path) : std::string();
    }
    
    static std::string getSender(const MessagePtr& msg) {
        const char* sender = dbus_message_get_sender(msg.get());
        return sender ? std::string(sender) : std::string();
    }
    
    static bool isMethodCall(const MessagePtr& msg, 
                             const std::string& iface,
                             const std::string& method) {
        return dbus_message_is_method_call(
            msg.get(), 
            iface.c_str(), 
            method.c_str()
        ) != 0;
    }
    
    static bool isSignal(const MessagePtr& msg,
                         const std::string& iface,
                         const std::string& signalName) {
        return dbus_message_is_signal(
            msg.get(),
            iface.c_str(),
            signalName.c_str()
        ) != 0;
    }
};

class MessageIter {
public:
    // 默认构造函数
    MessageIter() = default;
    
    // 用于读取的构造函数
    explicit MessageIter(const MessagePtr& message) {
        dbus_message_iter_init(message.get(), &iter_);
    }
    
    explicit MessageIter(DBusMessage* message) {
        dbus_message_iter_init(message, &iter_);
    }
    
    explicit MessageIter(DBusMessage** message) {
        dbus_message_iter_init(*message, &iter_);
    }
    
    // 用于写入的静态方法
    static MessageIter forWriting(MessagePtr& message) {
        MessageIter iter;
        dbus_message_iter_init_append(message.get(), &iter.iter_);
        return iter;
    }
    
    bool hasNext() const {
        return dbus_message_iter_has_next(const_cast<DBusMessageIter*>(&iter_)) != 0;
    }
    
    void next() {
        dbus_message_iter_next(&iter_);
    }
    
    int getArgType() const {
        return dbus_message_iter_get_arg_type(const_cast<DBusMessageIter*>(&iter_));
    }
    
    template<typename T>
    T readBasic() {
        T value{};
        dbus_message_iter_get_basic(&iter_, &value);
        return value;
    }
    
    template<typename T>
    void writeBasic(T value) {
        dbus_message_iter_append_basic(&iter_, 
            getDbusType<T>(), &value);
    }
    
    void writeString(const std::string& str) {
        const char* s = str.c_str();
        dbus_message_iter_append_basic(&iter_, DBUS_TYPE_STRING, &s);
    }
    
    std::string readString() {
        char* str = nullptr;
        dbus_message_iter_get_basic(&iter_, &str);
        return str ? std::string(str) : "";
    }
    
    template<typename T>
    Array<T> readArray() {
        Array<T> result;
        
        DBusMessageIter sub;
        dbus_message_iter_recurse(&iter_, &sub);
        
        while (dbus_message_iter_has_next(&sub)) {
            dbus_message_iter_get_basic(&sub, &result.emplace_back());
            dbus_message_iter_next(&sub);
        }
        
        return result;
    }
    
    template<typename T>
    void writeArray(const Array<T>& arr) {
        DBusMessageIter sub;
        char signature = getDbusTypeSignature<T>();
        dbus_message_iter_open_container(&iter_, 
            DBUS_TYPE_ARRAY, &signature, &sub);
        
        for (const auto& item : arr) {
            dbus_message_iter_append_basic(&sub, 
                getDbusType<T>(), &item);
        }
        
        dbus_message_iter_close_container(&iter_, &sub);
    }
    
    std::vector<std::string> readStringArray() {
        std::vector<std::string> result;
        
        DBusMessageIter sub;
        dbus_message_iter_recurse(&iter_, &sub);
        
        while (dbus_message_iter_has_next(&sub)) {
            char* str = nullptr;
            dbus_message_iter_get_basic(&sub, &str);
            if (str) {
                result.emplace_back(str);
            }
            dbus_message_iter_next(&sub);
        }
        
        return result;
    }
    
    void writeStringArray(const std::vector<std::string>& arr) {
        DBusMessageIter sub;
        char signature[] = "as";
        dbus_message_iter_open_container(&iter_, 
            DBUS_TYPE_ARRAY, signature, &sub);
        
        for (const auto& str : arr) {
            const char* s = str.c_str();
            dbus_message_iter_append_basic(&sub, 
                DBUS_TYPE_STRING, &s);
        }
        
        dbus_message_iter_close_container(&iter_, &sub);
    }

private:
    DBusMessageIter iter_;
    
    template<typename T>
    static int getDbusType() {
        if constexpr (std::is_same_v<T, int32_t>) return DBUS_TYPE_INT32;
        else if constexpr (std::is_same_v<T, uint32_t>) return DBUS_TYPE_UINT32;
        else if constexpr (std::is_same_v<T, int64_t>) return DBUS_TYPE_INT64;
        else if constexpr (std::is_same_v<T, uint64_t>) return DBUS_TYPE_UINT64;
        else if constexpr (std::is_same_v<T, double>) return DBUS_TYPE_DOUBLE;
        else if constexpr (std::is_same_v<T, bool>) return DBUS_TYPE_BOOLEAN;
        else if constexpr (std::is_same_v<T, char*> || 
                          std::is_same_v<T, const char*>) return DBUS_TYPE_STRING;
        return DBUS_TYPE_INVALID;
    }
    
    template<typename T>
    static char getDbusTypeSignature() {
        if constexpr (std::is_same_v<T, int32_t>) return 'i';
        else if constexpr (std::is_same_v<T, uint32_t>) return 'u';
        else if constexpr (std::is_same_v<T, int64_t>) return 'x';
        else if constexpr (std::is_same_v<T, uint64_t>) return 't';
        else if constexpr (std::is_same_v<T, double>) return 'd';
        else if constexpr (std::is_same_v<T, bool>) return 'b';
        else if constexpr (std::is_same_v<T, char*> || 
                          std::is_same_v<T, const char*>) return 's';
        return '?';
    }
};

} // namespace dbus
