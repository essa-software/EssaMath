# Locate Google Test
find_package(GTest REQUIRED)

# Include Google Test module
include(GoogleTest)

# create EssaMath test function
function(create_test TEST_NAME)
    # Create test executable
    add_executable(essamath_test_${TEST_NAME} ${TEST_NAME}/${TEST_NAME}_test.cpp common/test_utils.cpp)

    # Add include directory
    target_include_directories(essamath_test_${TEST_NAME}
        PRIVATE
            common/include
    )
    
    # Link test executable to dependent libraries
    target_link_libraries(essamath_test_${TEST_NAME}
        PRIVATE
            GTest::gtest_main
            ProjectOptions
            EssaMath
    )

    # Enable test discovery
    gtest_discover_tests(essamath_test_${TEST_NAME})
endfunction()

# Add tests
create_test(arithmetic)
create_test(05_data_types_and_structures)

