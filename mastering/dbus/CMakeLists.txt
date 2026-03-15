cmake_minimum_required(VERSION 3.16)
project(dbus_example VERSION 1.0.0 LANGUAGES CXX)

# C++17 标准
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# 查找 D-Bus 库
find_package(PkgConfig REQUIRED)
pkg_check_modules(DBUS REQUIRED dbus-1)

# 打印 D-Bus 配置信息
message(STATUS "DBus include dirs: ${DBUS_INCLUDE_DIRS}")
message(STATUS "DBus libraries: ${DBUS_LIBRARIES}")
message(STATUS "DBus cflags: ${DBUS_CFLAGS}")

# 编译选项
add_compile_options(
    -Wall
    -Wextra
    -Wpedantic
    -Werror
)

# 可执行文件
add_executable(dbus_example
    main.cpp
    dbus_service.cpp
    dbus_client.cpp
)

# 包含目录
target_include_directories(dbus_example PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${DBUS_INCLUDE_DIRS}
)

# 链接库
target_link_libraries(dbus_example
    ${DBUS_LIBRARIES}
    pthread
)

# 安装目标
install(TARGETS dbus_example
    RUNTIME DESTINATION bin
)

# 打印配置信息
message(STATUS "")
message(STATUS "=== DBus Example Configuration ===")
message(STATUS "C++ Standard: ${CMAKE_CXX_STANDARD}")
message(STATUS "Compiler: ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "===================================")
