#include "dbus_simple.h"
#include <iostream>

int main() {
    dbus_simple::Conn conn;
    
    auto reply = conn.call("com.example.DbusService", "/com/example/Calculator",
                           "com.example.Calculator", "Add");
    
    int64_t result = reply.get<int64_t>();
    std::cout << "Result: " << result << std::endl;
    
    return 0;
}
