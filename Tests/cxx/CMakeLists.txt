cmake_minimum_required(VERSION 3.17)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED true)

project(MyTest)

find_package(EssaMath 1.0.1 CONFIG REQUIRED)

add_executable(MyTest main.cpp)

add_compile_options(-Wall -O3-fsanitize=address)

target_link_libraries(MyTest PRIVATE essamath)
