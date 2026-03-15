#include "dbus_client.h"
#include <iostream>
#include <iomanip>
#include <thread>
#include <chrono>
#include <vector>
#include <optional>

namespace dbus_example {

// ============================================
// D-Bus 客户端封装类
// ============================================

class DbusClient {
public:
    DbusClient() : connection_(dbus::BusType::Session) {}
    
    // 调用 Calculator 接口
    class CalculatorProxy {
    public:
        CalculatorProxy(DbusClient& client) : client_(client) {}
        
        int64_t add(int64_t a, int64_t b) {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "Add"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeBasic(a);
            iter.writeBasic(b);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<int64_t>();
        }
        
        int64_t subtract(int64_t a, int64_t b) {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "Subtract"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeBasic(a);
            iter.writeBasic(b);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<int64_t>();
        }
        
        int64_t multiply(int64_t a, int64_t b) {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "Multiply"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeBasic(a);
            iter.writeBasic(b);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<int64_t>();
        }
        
        double divide(int64_t a, int64_t b) {
            if (b == 0) {
                throw dbus::DbusException("Cannot divide by zero");
            }
            
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "Divide"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeBasic(a);
            iter.writeBasic(b);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<double>();
        }
        
        std::vector<int64_t> fibonacci(int32_t n) {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "Fibonacci"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeBasic(n);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readArray<int64_t>();
        }
        
        std::vector<std::string> getHistory() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "GetHistory"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readStringArray();
        }
        
        void clearHistory() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/Calculator",
                "com.example.Calculator",
                "ClearHistory"
            );
            
            client_.connection_.sendWithReply(msg);
        }
        
    private:
        DbusClient& client_;
    };
    
    // 调用 SystemInfo 接口
    class SystemInfoProxy {
    public:
        SystemInfoProxy(DbusClient& client) : client_(client) {}
        
        std::string getHostname() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetHostname"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readString();
        }
        
        std::string getUptime() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetUptime"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readString();
        }
        
        struct MemoryInfo {
            uint64_t total;
            uint64_t free;
            uint64_t available;
        };
        
        MemoryInfo getMemoryInfo() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetMemoryInfo"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            
            MemoryInfo info = {0, 0, 0};
            return info;
        }
        
        int32_t getCpuCount() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetCpuCount"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<int32_t>();
        }
        
        std::tuple<double, double, double> getLoadAverage() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetLoadAverage"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            return {0.0, 0.0, 0.0};
        }
        
        bool setLabel(const std::string& label) {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "SetLabel"
            );
            
            auto iter = dbus::MessageIter::forWriting(msg);
            iter.writeString(label);
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readBasic<bool>();
        }
        
        std::string getLabel() {
            auto msg = dbus::Message::createMethodCall(
                "com.example.DbusService",
                "/com/example/SystemInfo",
                "com.example.SystemInfo",
                "GetLabel"
            );
            
            auto reply = client_.connection_.sendWithReply(msg);
            dbus::MessageIter replyIter(reply);
            return replyIter.readString();
        }
        
    private:
        DbusClient& client_;
    };
    
    // 通知接口 - 暂时注释掉，因为有问题
    class NotificationsProxy {
    public:
        NotificationsProxy(DbusClient& client) : client_(client) {}
        
    private:
        DbusClient& client_;
    };
    
    // 代理对象访问
    CalculatorProxy calculator() { return CalculatorProxy(*this); }
    SystemInfoProxy systemInfo() { return SystemInfoProxy(*this); }
    NotificationsProxy notifications() { return NotificationsProxy(*this); }
    
    // 等待服务启动
    bool waitForService(int timeoutSeconds = 10) {
        auto start = std::chrono::steady_clock::now();
        
        while (true) {
            auto now = std::chrono::steady_clock::now();
            auto elapsed = std::chrono::duration_cast<std::chrono::seconds>(
                now - start
            ).count();
            
            if (elapsed > timeoutSeconds) {
                return false;
            }
            
            try {
                systemInfo().getHostname();
                return true;
            } catch (const std::exception&) {
                std::this_thread::sleep_for(std::chrono::milliseconds(500));
            }
        }
    }

private:
    dbus::Connection connection_;
};

// 打印分隔线
void printSeparator(const std::string& title) {
    std::cout << "\n" << std::string(50, '=') << "\n";
    std::cout << "  " << title << "\n";
    std::cout << std::string(50, '=') << "\n";
}

void runClientDemo() {
    DbusClient client;
    
    std::cout << "\nWaiting for D-Bus service to start..." << std::endl;
    
    if (!client.waitForService(10)) {
        std::cerr << "Error: Service not available. Make sure dbus_service is running." << std::endl;
        return;
    }
    
    std::cout << "Service found! Running demo...\n" << std::endl;
    
    try {
        // ============ Calculator 接口演示 ============
        printSeparator("Calculator Interface");
        
        auto calc = client.calculator();
        
        // 加法
        int64_t sum = calc.add(100, 200);
        std::cout << "100 + 200 = " << sum << std::endl;
        
        // 减法
        int64_t diff = calc.subtract(500, 150);
        std::cout << "500 - 150 = " << diff << std::endl;
        
        // 乘法
        int64_t prod = calc.multiply(25, 4);
        std::cout << "25 * 4 = " << prod << std::endl;
        
        // 除法
        double quotient = calc.divide(100, 3);
        std::cout << "100 / 3 = " << std::fixed << std::setprecision(2) << quotient << std::endl;
        
        // 斐波那契
        auto fib = calc.fibonacci(10);
        std::cout << "Fibonacci(10): ";
        for (size_t i = 0; i < fib.size(); i++) {
            std::cout << fib[i];
            if (i < fib.size() - 1) std::cout << ", ";
        }
        std::cout << std::endl;
        
        // 历史记录
        auto history = calc.getHistory();
        std::cout << "\nCalculation History:" << std::endl;
        for (const auto& h : history) {
            std::cout << "  - " << h << std::endl;
        }
        
        // ============ SystemInfo 接口演示 ============
        printSeparator("SystemInfo Interface");
        
        auto sys = client.systemInfo();
        
        // 主机名
        std::cout << "Hostname: " << sys.getHostname() << std::endl;
        
        // 运行时间
        std::cout << "Uptime: " << sys.getUptime() << std::endl;
        
        // CPU 核心数
        std::cout << "CPU Count: " << sys.getCpuCount() << std::endl;
        
        // 再次获取计算器历史
        printSeparator("Final Calculator History");
        history = calc.getHistory();
        for (const auto& h : history) {
            std::cout << "  - " << h << std::endl;
        }
        
        std::cout << "\nDemo completed successfully!" << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
    }
}

} // namespace dbus_example
