# Target
add_library(essamath
  ${SOURCES}
  ${HEADERS_PUBLIC}
  ${HEADERS_PRIVATE}
  )

# Alias:
#   - Foo::foo alias of foo
add_library(Essa::Math ALIAS essamath)

add_custom_target(maxima ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/maxima.a)
add_dependencies(essamath maxima)

target_link_libraries(essamath
  ${ECL_LIBRARIES}
  ${CMAKE_CURRENT_BINARY_DIR}/maxima.a
  m  # Link against the math library
)

# C++11
target_compile_features(essamath PUBLIC cxx_std_11)

target_compile_definitions(essamath PUBLIC
  "${PROJECT_NAME_UPPERCASE}_DEBUG=$<CONFIG:Debug>")

# Global includes. Used by all targets
# Note:
#   - header can be included by C++ code `#include <foo/foo.h>`
#   - header location in project: ${CMAKE_CURRENT_BINARY_DIR}/generated_headers
target_include_directories(
  essamath PUBLIC
    "$<BUILD_INTERFACE:${PROJECT_SOURCE_DIR}>"
    "$<BUILD_INTERFACE:${GENERATED_HEADERS_DIR}>"
    "$<INSTALL_INTERFACE:.>"
)

install(
    TARGETS              "essamath"
    EXPORT               "${TARGETS_EXPORT_NAME}"
    LIBRARY DESTINATION  "\\usr\\lib\\"
    ARCHIVE DESTINATION  "${CMAKE_INSTALL_LIBDIR}"
    RUNTIME DESTINATION  "${CMAKE_INSTALL_BINDIR}"
    INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

install(
    FILES ${HEADERS_PUBLIC}
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/EssaMath"
)

set(MAXIMA_DIR "/usr/local/share/maxima/5.47.0")
IF(EXISTS "${MAXIMA_DIR}" AND IS_DIRECTORY "${MAXIMA_DIR}")
    message("Maxima already installed!")
else()
  install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/maxima/"
          DESTINATION MAXIMA_DIR
          FILES_MATCHING PATTERN "*")
endif()

IF(EXISTS "/usr/local/share/info/maxima-index.lisp" AND EXISTS "/usr/local/share/info/maxima-index-html.lisp")
    message("Maxima info already installed!")
else()
  install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/src/maxima/doc/info"
          DESTINATION "/usr/local/share"
          FILES_MATCHING PATTERN "*")
endif()

install(
    FILES       "${GENERATED_HEADERS_DIR}/essamath/version.h"
    DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}/EssaMath"
)

install(
    FILES       "${PROJECT_CONFIG_FILE}"
                "${VERSION_CONFIG_FILE}"
    DESTINATION "${CONFIG_INSTALL_DIR}"
)

# Config
#   - <prefix>/lib/cmake/Foo/FooTargets.cmake
install(
  EXPORT      "${TARGETS_EXPORT_NAME}"
  FILE        "$EssaMathTargets.cmake"
  DESTINATION "${CONFIG_INSTALL_DIR}"
  # NAMESPACE   "$Essa::"
)
