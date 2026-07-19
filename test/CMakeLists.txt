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
add_executable(test_60_symbol_table test_60_symbol_table.c ${SOURCES})

# Add math library to each test executable
target_link_libraries(test_10_buffer       m)
target_link_libraries(test_20_lexer        m)
target_link_libraries(test_30_string       m)
target_link_libraries(test_40_value        m)
target_link_libraries(test_60_symbol_table m)

# Add tests to CTest
add_test(NAME Test10Buffer      COMMAND test_10_buffer)
add_test(NAME Test20Lexer       COMMAND test_20_lexer)
add_test(NAME Test30String      COMMAND test_30_string)
add_test(NAME Test40Value       COMMAND test_40_value)
add_test(NAME Test60SymbolTable COMMAND test_60_symbol_table)
