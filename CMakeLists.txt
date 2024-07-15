cmake_minimum_required(VERSION 3.27.0)

# Create EssaMathBuildsystem project
project(
    EssaMathBuildsystem
    VERSION 1.0
    DESCRIPTION
    LANGUAGES CXX
)

# Set global C++ standard
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" "${CMAKE_SOURCE_DIR}/cmake/Modules/")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED true)
set(BUILD_SHARED_LIBS ON)
set(CMAKE_CXX_EXTENSIONS OFF)

find_package(ECL REQUIRED)
include_directories(${ECL_INCLUDE_DIR})

# Set standard project settings
include(cmake/StandardProjectSettings.cmake)

# set project options
include(cmake/ProjectOptions.cmake)

# set static analysers
include(cmake/StaticAnalysers.cmake)

# Add the EssaMath directory to the build
add_subdirectory(essamath)

# Enable CTest
enable_testing()

# Add the test subdirectory to the build
add_subdirectory(test)
