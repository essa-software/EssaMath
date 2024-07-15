project(EssaMath C)

file(GLOB_RECURSE SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/cxx/*.c")
file(GLOB_RECURSE HEADERS_PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include/*.h")
file(GLOB_RECURSE CORE_LISP_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/maxima/src/*.lisp" "${CMAKE_CURRENT_SOURCE_DIR}/maxima/src/*.asd")

add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/maxima.a
  COMMAND ${ECL_BIN_PATH} --norc
  --eval '(require :asdf)'
  --eval '(push \"${CMAKE_CURRENT_SOURCE_DIR}/maxima/src/\" asdf:*central-registry*)'
  --eval '(asdf:make-build :maxima :type :static-library :move-here \"${CMAKE_CURRENT_BINARY_DIR}\" :init-name \"init_lib_MAXIMA\")'
  --eval '(quit)'
  DEPENDS ${CORE_LISP_SOURCES})

  # Ensure that the sources are not empty
  if(NOT SOURCES)
    message(FATAL_ERROR "No source files found for the EssaMath library.")
  endif()

  # Create EssaMath library
add_library(EssaMath SHARED ${SOURCES} ${HEADERS_PUBLIC})

# Add include directories for building and installing EssaMath
target_include_directories(EssaMath PUBLIC  
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>
)

# Ensure the linker language is set correctly
# set_target_properties(EssaMath PROPERTIES LINKER_LANGUAGE C)

add_custom_target(maxima ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/maxima.a)
add_dependencies(EssaMath maxima)

target_link_libraries(EssaMath PRIVATE
  ProjectOptions
  ${ECL_LIBRARIES}
  ${CMAKE_CURRENT_BINARY_DIR}/maxima.a
  m  # Link against the math library
)

# Include module for GNU standard installation directories
include(GNUInstallDirs)

# Install library and executable and export as a set
install(TARGETS
EssaMath
    EXPORT EssaMathExportSet
)

# Install the export set
install(EXPORT EssaMathExportSet
    FILE EssaMathTargets.cmake
    NAMESPACE EssaMath::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/EssaMath
)
