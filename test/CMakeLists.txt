cmake_minimum_required(VERSION 3.10)

project(Tests)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
set(CMAKE_C_FLAGS "-m32 -Wall -Wextra -Wpedantic")
file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/../src/*.c)
# message(STATUS "Sources: ${SOURCES}")

# Enable testing
enable_testing()

# Add each test executable
add_executable(test_10_buffer       test_10_buffer.c       ${SOURCES})
add_executable(test_20_lexer        test_20_lexer.c        ${SOURCES})
add_executable(test_30_string       test_30_string.c       ${SOURCES})
add_executable(test_40_value        test_40_value.c        ${SOURCES})
add_executable(test_50_symbol_table test_50_symbol_table.c ${SOURCES})
add_executable(test_60_string_heap  test_60_string_heap.c  ${SOURCES})
add_executable(test_70_system_lib   test_70_system_lib.c   ${SOURCES})
add_executable(test_80_for_type_check test_80_for_type_check.c ${SOURCES})
add_executable(test_90_ast          test_90_ast.c          ${SOURCES})

# Add math library to each test executable
target_link_libraries(test_10_buffer       m)
target_link_libraries(test_20_lexer        m)
target_link_libraries(test_30_string       m)
target_link_libraries(test_40_value        m)
target_link_libraries(test_50_symbol_table m)
target_link_libraries(test_60_string_heap  m)
target_link_libraries(test_70_system_lib   m)
target_link_libraries(test_80_for_type_check m)
target_link_libraries(test_90_ast          m)

# Add tests to CTest
add_test(NAME Test10Buffer      COMMAND test_10_buffer)
add_test(NAME Test20Lexer       COMMAND test_20_lexer)
add_test(NAME Test30String      COMMAND test_30_string)
add_test(NAME Test40Value       COMMAND test_40_value)
add_test(NAME Test50SymbolTable COMMAND test_50_symbol_table)
add_test(NAME Test60StringHeap  COMMAND test_60_string_heap)
add_test(NAME Test70SystemLib   COMMAND test_70_system_lib)
add_test(NAME Test80ForTypeCheck COMMAND test_80_for_type_check)
add_test(NAME Test90Ast          COMMAND test_90_ast)
