# Create test executable
add_executable(essamath_test essamath_test.cpp)

# Link test executable to dependent libraries
target_link_libraries(essamath_test
    PRIVATE
        GTest::gtest_main
        ProjectOptions
        EssaMath
)

# Enable test discovery
gtest_discover_tests(essamath_test)