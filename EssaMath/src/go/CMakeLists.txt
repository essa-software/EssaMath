cmake_minimum_required(VERSION 3.17)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED true)
set(BUILD_SHARED_LIBS ON)

project(EssaMathGo)

find_package(EssaMath 1.0.1 CONFIG REQUIRED)

# Target
add_library(essamath_go
  EssaMath_cxx/EssaMath.cpp
  EssaMath_cxx/SymbolTable.cpp
  EssaMath_cxx/Expression.cpp
  EssaMath_cxx/Parser.cpp
  EssaMath_cxx/OperatorUtils.cpp
)

target_link_libraries(essamath_go PRIVATE essamath)



install(
    TARGETS              "essamath_go"
    EXPORT               "essamath_go"
    LIBRARY DESTINATION  "\\usr\\lib\\"
)

