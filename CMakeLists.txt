cmake_minimum_required(VERSION 3.21.0)

# Allow users to specify compiler (gcc or clang)
# ==============================================
# Usage: cmake -DCMAKE_C_COMPILER=gcc ... or cmake -DCMAKE_C_COMPILER=clang ...
# Default compiler is gcc if not specified
if(NOT CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER gcc)
endif()

# Choose between Debug, RelWithDebInfo, Release
# =============================================
# Usage: cmake -DCMAKE_BUILD_TYPE=Debug ...
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE RelWithDebInfo)
endif()

# Be strictly C23 compliant
# =========================
set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)

# Generate ps_version.h in build/include from VERSION and include/ps_version.h.in
# ===============================================================================
file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/VERSION" PS_VERSION_STRING LIMIT_COUNT 1)
string(STRIP "${PS_VERSION_STRING}" PS_VERSION_STRING)
if(NOT PS_VERSION_STRING MATCHES "^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)$")
    message(FATAL_ERROR "VERSION must contain four numeric components, e.g. 0.1.2.3")
endif()
set(PS_VERSION_MAJOR "${CMAKE_MATCH_1}")
set(PS_VERSION_MINOR "${CMAKE_MATCH_2}")
set(PS_VERSION_PATCH "${CMAKE_MATCH_3}")
set(PS_VERSION_BUILD "${CMAKE_MATCH_4}")
project(pascalscript VERSION ${PS_VERSION_MAJOR}.${PS_VERSION_MINOR}.${PS_VERSION_PATCH}.${PS_VERSION_BUILD} LANGUAGES C)

# Enable AddressSanitizer for all project targets
# ==============================================
option(ENABLE_ASAN "Build with AddressSanitizer" OFF)
if(ENABLE_ASAN)
  if(NOT CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    message(FATAL_ERROR "ENABLE_ASAN requires GCC or Clang")
  endif()
  add_compile_options(-fsanitize=address -fno-omit-frame-pointer)
  add_link_options(-fsanitize=address)
endif()

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/include/ps_version.h.in ${CMAKE_CURRENT_BINARY_DIR}/include/ps_version.h)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}/include)

# Build src/*.c as library and use object files for pascalscript and C tests
# ==========================================================================
file(GLOB CORE_SOURCES "${CMAKE_CURRENT_SOURCE_DIR}/src/*.c")
add_library(pascalscript_core STATIC ${CORE_SOURCES})
target_compile_options(pascalscript_core PRIVATE -Wall -Wextra -Wpedantic)
target_link_libraries(pascalscript_core PUBLIC m)

# Build PascalScript CLI from pascalscript.c and "core"
# =====================================================
add_executable(pascalscript ${CMAKE_CURRENT_SOURCE_DIR}/pascalscript.c)
target_compile_options(pascalscript PRIVATE -Wall -Wextra -Wpedantic)
target_link_libraries(pascalscript PRIVATE pascalscript_core)

# Configure CTest's MemCheck mode when Valgrind is available
# ==========================================================
# cmake -S . -B build -DCMAKE_BUILD_TYPE=Debug
# cmake --build build
# ctest --test-dir build -T memcheck --output-on-failure
find_program(MEMORYCHECK_COMMAND NAMES valgrind)
if(MEMORYCHECK_COMMAND)
  set(MEMORYCHECK_COMMAND_OPTIONS
      "--leak-check=full --show-leak-kinds=all --errors-for-leak-kinds=definite,indirect --error-exitcode=1"
      CACHE STRING "Arguments passed to Valgrind by CTest MemCheck")
endif()

# Enable CTest testing and customize some CTest properties
# ========================================================
include(CTest)
enable_testing()
configure_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/CTestCustom.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/CTestCustom.cmake
    COPYONLY
)

# Add C tests from executable test/test_*.c files
# ===============================================

function(add_c_tests DIR LABEL)
    file(GLOB C_FILES "${DIR}/test_*.c")
    foreach(C_FILE IN LISTS C_FILES)
        get_filename_component(TEST_NAME "${C_FILE}" NAME_WE)
        get_filename_component(TEST_FILE "${C_FILE}" NAME)
        set(SHOULD_ADD TRUE)
        if(UNIX)
            execute_process(
                COMMAND /usr/bin/env sh -c "test -x \"$1\"" -- "${C_FILE}"
                RESULT_VARIABLE EXEC_CHECK
            )
            if(NOT EXEC_CHECK EQUAL 0)
                set(SHOULD_ADD FALSE)
            endif()
        endif()
        if(SHOULD_ADD)
            add_executable("${TEST_NAME}" "${C_FILE}")
            target_link_libraries("${TEST_NAME}" PRIVATE pascalscript_core)
            add_test(NAME "${LABEL}_${TEST_NAME}" COMMAND "${TEST_NAME}")
            set_tests_properties("${LABEL}_${TEST_NAME}" PROPERTIES FAIL_REGULAR_EXPRESSION "FAILED!")
            message(STATUS "Added ${LABEL}_${TEST_NAME} for ${DIR}/${TEST_FILE}")
        endif()
    endforeach()
endfunction()

add_c_tests("${CMAKE_SOURCE_DIR}/test" "c")

# Add Pascal tests from executable example/*.pas files
# ====================================================

function(add_executable_pascal_examples DIR LABEL)
    file(GLOB PASCAL_FILES "${CMAKE_SOURCE_DIR}/${DIR}/*.pas")
    foreach(PASCAL_FILE IN LISTS PASCAL_FILES)
        get_filename_component(TEST_NAME "${PASCAL_FILE}" NAME_WE)
        get_filename_component(TEST_FILE "${PASCAL_FILE}" NAME)
        set(SHOULD_ADD TRUE)
        if(UNIX)
            execute_process(
                COMMAND /usr/bin/env sh -c "test -x \"$1\"" -- "${PASCAL_FILE}"
                RESULT_VARIABLE EXEC_CHECK
            )
            if(NOT EXEC_CHECK EQUAL 0)
                set(SHOULD_ADD FALSE)
            endif()
        endif()
        if(SHOULD_ADD)
            add_test(
                NAME "${LABEL}_${TEST_NAME}"
                COMMAND $<TARGET_FILE:pascalscript> "${CMAKE_CURRENT_SOURCE_DIR}/${DIR}/${TEST_FILE}"
            )
            set_tests_properties(
                "${LABEL}_${TEST_NAME}"
                PROPERTIES FAIL_REGULAR_EXPRESSION "FAILED!"
            )
            message(
                STATUS
                "Added ${LABEL}_${TEST_NAME} for ${DIR}/${TEST_FILE}"
            )
        endif()
    endforeach()
endfunction()

add_executable_pascal_examples("examples" "pascal")

# Simple test to check absolute path works for CLI
add_test(
    NAME cli_absolute_path
    COMMAND $<TARGET_FILE:pascalscript> "${CMAKE_CURRENT_SOURCE_DIR}/examples/000-minimal.pas"
)

# EOF
