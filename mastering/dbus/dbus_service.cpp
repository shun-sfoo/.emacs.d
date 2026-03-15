#include "dbus_client.h"
#include <iostream>
#include <iomanip>
#include <sstream>
#include <ctime>
#include <filesystem>
#include <fstream>
#include <thread>
#include <atomic>
#include <cstring>
#include <csignal>

namespace dbus_example {

class CalculatorInterface {
public:
    static constexpr const char* interfaceName = "com.example.Calculator";
    static constexpr const char* objectPath = "/com/example/Calculator";
    
    int64_t add(int64_t a, int64_t b);
    int64_t subtract(int64_t a, int64_t b);
    int64_t multiply(int64_t a, int64_t b);
    double divide(double a, double b);
    std::vector<int64_t> fibonacci(int32_t n);
    std::vector<std::string> getHistory();
    void clearHistory();
    
private:
    int64_t lastResult_ = 0;
    int64_t operationCount_ = 0;
    std::vector<std::string> history_;
    std::mutex mutex_;
};

class SystemInfoInterface {
public:
    static constexpr const char* interfaceName = "com.example.SystemInfo";
    static constexpr const char* objectPath = "/com/example/SystemInfo";
    
    std::string getHostname();
    std::string getUptime();
    std::tuple<uint64_t, uint64_t, uint64_t> getMemoryInfo();
    int32_t getCpuCount();
    bool setLabel(const std::string& label);
    std::string getLabel();
    
private:
    std::string label_ = "Default";
    std::mutex mutex_;
};

class NotificationsInterface {
public:
    static constexpr const char* interfaceName = "com.example.Notifications";
    static constexpr const char* objectPath = "/com/example/Notifications";
    
    uint32_t showNotification(const std::string& title, const std::string& body, int32_t urgency);
    void closeNotification(uint32_t id);
    
private:
    std::atomic<uint32_t> nextId_{1};
};

int64_t CalculatorInterface::add(int64_t a, int64_t b) {
    std::lock_guard<std::mutex> lock(mutex_);
    lastResult_ = a + b;
    operationCount_++;
    history_.push_back(std::to_string(a) + " + " + std::to_string(b) + " = " + std::to_string(lastResult_));
    return lastResult_;
}

int64_t CalculatorInterface::subtract(int64_t a, int64_t b) {
    std::lock_guard<std::mutex> lock(mutex_);
    lastResult_ = a - b;
    operationCount_++;
    history_.push_back(std::to_string(a) + " - " + std::to_string(b) + " = " + std::to_string(lastResult_));
    return lastResult_;
}

int64_t CalculatorInterface::multiply(int64_t a, int64_t b) {
    std::lock_guard<std::mutex> lock(mutex_);
    lastResult_ = a * b;
    operationCount_++;
    history_.push_back(std::to_string(a) + " * " + std::to_string(b) + " = " + std::to_string(lastResult_));
    return lastResult_;
}

double CalculatorInterface::divide(double a, double b) {
    std::lock_guard<std::mutex> lock(mutex_);
    lastResult_ = static_cast<int64_t>(a / b);
    operationCount_++;
    history_.push_back(std::to_string(static_cast<int64_t>(a)) + " / " + 
                       std::to_string(static_cast<int64_t>(b)) + " = " + 
                       std::to_string(lastResult_));
    return a / b;
}

std::vector<int64_t> CalculatorInterface::fibonacci(int32_t n) {
    std::lock_guard<std::mutex> lock(mutex_);
    operationCount_++;
    
    std::vector<int64_t> result;
    if (n <= 0) return result;
    
    result.reserve(n);
    int64_t a = 0, b = 1;
    for (int32_t i = 0; i < n; i++) {
        result.push_back(a);
        int64_t next = a + b;
        a = b;
        b = next;
    }
    
    history_.push_back("fibonacci(" + std::to_string(n) + ") computed");
    return result;
}

std::vector<std::string> CalculatorInterface::getHistory() {
    std::lock_guard<std::mutex> lock(mutex_);
    return history_;
}

void CalculatorInterface::clearHistory() {
    std::lock_guard<std::mutex> lock(mutex_);
    history_.clear();
    lastResult_ = 0;
    operationCount_ = 0;
}

std::string SystemInfoInterface::getHostname() {
    std::ifstream hostnameFile("/etc/hostname");
    std::string hostname;
    std::getline(hostnameFile, hostname);
    return hostname;
}

std::string SystemInfoInterface::getUptime() {
    std::ifstream uptimeFile("/proc/uptime");
    double uptimeSeconds = 0;
    uptimeFile >> uptimeSeconds;
    
    auto duration = std::chrono::duration_cast<std::chrono::seconds>(
        std::chrono::duration<double>(uptimeSeconds)
    );
    
    int days = duration.count() / 86400;
    int hours = (duration.count() % 86400) / 3600;
    int minutes = (duration.count() % 3600) / 60;
    
    std::ostringstream oss;
    oss << days << " days, " << hours << " hours, " << minutes << " minutes";
    return oss.str();
}

std::tuple<uint64_t, uint64_t, uint64_t> SystemInfoInterface::getMemoryInfo() {
    std::ifstream memFile("/proc/meminfo");
    std::string line;
    uint64_t memTotal = 0, memFree = 0, memAvailable = 0;
    
    while (std::getline(memFile, line)) {
        std::istringstream iss(line);
        std::string key;
        uint64_t value;
        iss >> key >> value;
        
        if (key == "MemTotal:") memTotal = value * 1024;
        else if (key == "MemFree:") memFree = value * 1024;
        else if (key == "MemAvailable:") memAvailable = value * 1024;
    }
    
    return {memTotal, memFree, memAvailable};
}

int32_t SystemInfoInterface::getCpuCount() {
    return std::thread::hardware_concurrency();
}

bool SystemInfoInterface::setLabel(const std::string& label) {
    std::lock_guard<std::mutex> lock(mutex_);
    label_ = label;
    return true;
}

std::string SystemInfoInterface::getLabel() {
    std::lock_guard<std::mutex> lock(mutex_);
    return label_;
}

uint32_t NotificationsInterface::showNotification(const std::string& title, 
                                                    const std::string& body, 
                                                    int32_t urgency) {
    uint32_t id = nextId_.fetch_add(1);
    std::cout << "\n[Notification #" << id << "]"
              << "\n  Title: " << title
              << "\n  Body: " << body
              << "\n  Urgency: " << urgency << "\n" << std::endl;
    return id;
}

void NotificationsInterface::closeNotification(uint32_t id) {
    std::cout << "[Notification #" << id << " closed]\n" << std::endl;
}

class DbusService {
public:
    DbusService() : connection_(dbus::BusType::Session) {}
    
    ~DbusService() {
        stop();
    }
    
    void init() {
        connection_.requestName("com.example.DbusService");
        registerObjects();
        std::cout << "D-Bus Service initialized successfully!" << std::endl;
        std::cout << "Service Name: com.example.DbusService" << std::endl;
    }
    
    void registerObjects() {
        connection_.addMatch("type='method_call'");
        
        std::cout << "Objects registered:" << std::endl;
        std::cout << "  - " << CalculatorInterface::objectPath << std::endl;
        std::cout << "  - " << SystemInfoInterface::objectPath << std::endl;
        std::cout << "  - " << NotificationsInterface::objectPath << std::endl;
    }
    
    void run() {
        std::cout << "\nService is running... Press Ctrl+C to exit." << std::endl;
        
        while (running_) {
            dbus_connection_read_write(connection_.raw(), 100);
            
            auto* msg = dbus_connection_pop_message(connection_.raw());
            
            if (msg) {
                handleMessage(msg);
                dbus_message_unref(msg);
            }
        }
    }
    
    void stop() {
        running_ = false;
    }

private:
    void handleMessage(DBusMessage* msg) {
        const char* path = dbus_message_get_path(msg);
        const char* iface = dbus_message_get_interface(msg);
        const char* member = dbus_message_get_member(msg);
        
        if (!path || !iface || !member) return;
        
        std::cout << "\nIncoming method call:" << std::endl;
        std::cout << "  Path: " << path << std::endl;
        std::cout << "  Interface: " << iface << std::endl;
        std::cout << "  Method: " << member << std::endl;
        
        DBusMessage* reply = nullptr;
        
        if (std::string(path) == CalculatorInterface::objectPath) {
            if (strcmp(member, "Add") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                int64_t a = 0, b = 0;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &a);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &b);
                }
                
                int64_t result = calculator_.add(a, b);
                
                reply = dbus_message_new_method_return(msg);
                dbus_message_iter_init_append(reply, &iter);
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT64, &result);
            } else if (strcmp(member, "Subtract") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                int64_t a = 0, b = 0;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &a);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &b);
                }
                
                int64_t result = calculator_.subtract(a, b);
                
                reply = dbus_message_new_method_return(msg);
                dbus_message_iter_init_append(reply, &iter);
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT64, &result);
            } else if (strcmp(member, "Multiply") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                int64_t a = 0, b = 0;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &a);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &b);
                }
                
                int64_t result = calculator_.multiply(a, b);
                
                reply = dbus_message_new_method_return(msg);
                dbus_message_iter_init_append(reply, &iter);
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT64, &result);
            } else if (strcmp(member, "Divide") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                int64_t a = 0, b = 0;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &a);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT64) {
                    dbus_message_iter_get_basic(&iter, &b);
                }
                
                if (b == 0) {
                    reply = dbus_message_new_error(msg, "com.example.Calculator.DivideByZero", 
                                                   "Cannot divide by zero");
                } else {
                    double result = calculator_.divide(static_cast<double>(a), static_cast<double>(b));
                    
                    reply = dbus_message_new_method_return(msg);
                    dbus_message_iter_init_append(reply, &iter);
                    dbus_message_iter_append_basic(&iter, DBUS_TYPE_DOUBLE, &result);
                }
            } else if (strcmp(member, "Fibonacci") == 0) {
                DBusMessageIter inIter;
                dbus_message_iter_init(msg, &inIter);
                
                int32_t n = 0;
                if (dbus_message_iter_get_arg_type(&inIter) == DBUS_TYPE_INT32) {
                    dbus_message_iter_get_basic(&inIter, &n);
                }
                
                auto result = calculator_.fibonacci(n);
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter outIter;
                dbus_message_iter_init_append(reply, &outIter);
                
                DBusMessageIter arrIter;
                const char* sig = "x";  // int64 元素类型 (null-terminated)
                dbus_message_iter_open_container(&outIter, DBUS_TYPE_ARRAY, sig, &arrIter);
                
                for (const auto& val : result) {
                    dbus_message_iter_append_basic(&arrIter, DBUS_TYPE_INT64, &val);
                }
                
                dbus_message_iter_close_container(&outIter, &arrIter);
            } else if (strcmp(member, "GetHistory") == 0) {
                auto history = calculator_.getHistory();
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter iter;
                dbus_message_iter_init_append(reply, &iter);
                
                DBusMessageIter arrIter;
                const char* sig = "s";  // string 元素类型 (null-terminated)
                dbus_message_iter_open_container(&iter, DBUS_TYPE_ARRAY, sig, &arrIter);
                
                for (const auto& h : history) {
                    const char* s = h.c_str();
                    dbus_message_iter_append_basic(&arrIter, DBUS_TYPE_STRING, &s);
                }
                
                dbus_message_iter_close_container(&iter, &arrIter);
            } else if (strcmp(member, "ClearHistory") == 0) {
                calculator_.clearHistory();
                reply = dbus_message_new_method_return(msg);
            }
        } else if (std::string(path) == SystemInfoInterface::objectPath) {
            if (strcmp(member, "GetHostname") == 0) {
                std::string hostname = systemInfo_.getHostname();
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter iter;
                dbus_message_iter_init_append(reply, &iter);
                const char* s = hostname.c_str();
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &s);
            } else if (strcmp(member, "GetUptime") == 0) {
                std::string uptime = systemInfo_.getUptime();
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter iter;
                dbus_message_iter_init_append(reply, &iter);
                const char* s = uptime.c_str();
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &s);
            } else if (strcmp(member, "GetCpuCount") == 0) {
                int32_t count = systemInfo_.getCpuCount();
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter iter;
                dbus_message_iter_init_append(reply, &iter);
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_INT32, &count);
            } else if (strcmp(member, "SetLabel") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                char* label = nullptr;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_STRING) {
                    dbus_message_iter_get_basic(&iter, &label);
                }
                
                bool success = systemInfo_.setLabel(label ? label : "");
                
                reply = dbus_message_new_method_return(msg);
                dbus_message_iter_init_append(reply, &iter);
                dbus_bool_t s = success ? 1 : 0;
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_BOOLEAN, &s);
            } else if (strcmp(member, "GetLabel") == 0) {
                std::string label = systemInfo_.getLabel();
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter iter;
                dbus_message_iter_init_append(reply, &iter);
                const char* s = label.c_str();
                dbus_message_iter_append_basic(&iter, DBUS_TYPE_STRING, &s);
            }
        } else if (std::string(path) == NotificationsInterface::objectPath) {
            if (strcmp(member, "ShowNotification") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                char* title = nullptr;
                char* body = nullptr;
                int32_t urgency = 1;
                
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_STRING) {
                    dbus_message_iter_get_basic(&iter, &title);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_STRING) {
                    dbus_message_iter_get_basic(&iter, &body);
                    dbus_message_iter_next(&iter);
                }
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_INT32) {
                    dbus_message_iter_get_basic(&iter, &urgency);
                }
                
                uint32_t id = notifications_.showNotification(
                    title ? title : "",
                    body ? body : "",
                    urgency
                );
                
                reply = dbus_message_new_method_return(msg);
                DBusMessageIter replyIter;
                dbus_message_iter_init_append(reply, &replyIter);
                dbus_message_iter_append_basic(&replyIter, DBUS_TYPE_UINT32, &id);
            } else if (strcmp(member, "CloseNotification") == 0) {
                DBusMessageIter iter;
                dbus_message_iter_init(msg, &iter);
                
                uint32_t id = 0;
                if (dbus_message_iter_get_arg_type(&iter) == DBUS_TYPE_UINT32) {
                    dbus_message_iter_get_basic(&iter, &id);
                }
                
                notifications_.closeNotification(id);
                
                reply = dbus_message_new_method_return(msg);
            }
        }
        
        if (reply) {
            dbus_connection_send(connection_.raw(), reply, nullptr);
            dbus_message_unref(reply);
        }
    }
    
    dbus::Connection connection_;
    CalculatorInterface calculator_;
    SystemInfoInterface systemInfo_;
    NotificationsInterface notifications_;
    std::atomic<bool> running_{true};
};

void runDbusService() {
    DbusService service;
    
    signal(SIGINT, [](int) {
        std::cout << "\nShutting down..." << std::endl;
        exit(0);
    });
    
    try {
        service.init();
        service.run();
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
}

} // namespace dbus_example
