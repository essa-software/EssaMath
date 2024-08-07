
function(set_project_warnings project_name)
    option(WARNINGS_AS_ERRORS "compiler warnings as errors" TRUE)

    set(CLANG_WARNINGS
        # -Weverything

        -Wall
        -Wextra
        # -Wpedantic
        -Werror

        -Wno-unsafe-buffer-usage
        -Wno-unused-macros
        -Wno-disabled-macro-expansion
        -Wno-format
    )

    if(WARNINGS_AS_ERRORS)
        set(CLANG_WARNINGS ${CLANG_WARNINGS} -Werror)
    endif()

    target_compile_options(${project_name} INTERFACE ${CLANG_WARNINGS})

endfunction()
