#include <iostream>
#include <cstring>
#include <csignal>
#include <getopt.h>

namespace dbus_example {
    void runDbusService();
    void runClientDemo();
}

void printUsage(const char* programName) {
    std::cout << "Usage: " << programName << " [OPTIONS]\n\n"
              << "Options:\n"
              << "  -s, --server    Run as D-Bus service (server mode)\n"
              << "  -c, --client    Run as D-Bus client (connect to service)\n"
              << "  -h, --help      Show this help message\n\n"
              << "Examples:\n"
              << "  " << programName << " --server    # Start D-Bus service\n"
              << "  " << programName << " --client    # Run client demo\n"
              << std::endl;
}

int main(int argc, char* argv[]) {
    // 命令行参数解析
    static struct option longOptions[] = {
        {"server", no_argument, 0, 's'},
        {"client", no_argument, 0, 'c'},
        {"help", no_argument, 0, 'h'},
        {0, 0, 0, 0}
    };
    
    int optionIndex = 0;
    int c;
    
    enum class Mode {
        Auto,   // 默认：自动检测
        Server, // 服务器模式
        Client  // 客户端模式
    };
    
    Mode mode = Mode::Auto;
    
    while ((c = getopt_long(argc, argv, "sch", longOptions, &optionIndex)) != -1) {
        switch (c) {
            case 's':
                mode = Mode::Server;
                break;
            case 'c':
                mode = Mode::Client;
                break;
            case 'h':
                printUsage(argv[0]);
                return 0;
            default:
                printUsage(argv[0]);
                return 1;
        }
    }
    
    // 根据模式运行程序
    if (mode == Mode::Server || mode == Mode::Auto) {
        try {
            std::cout << "Starting D-Bus Service...\n";
            dbus_example::runDbusService();
        } catch (const std::exception& e) {
            std::cerr << "Failed to start service: " << e.what() << std::endl;
            return 1;
        }
    } else {
        // 客户端模式
        dbus_example::runClientDemo();
    }
    
    return 0;
}
