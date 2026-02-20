cmake_minimum_required(VERSION 3.10.0)

# Allow users to specify compiler (gcc or clang)
# Usage: cmake -DCMAKE_C_COMPILER=gcc .. or cmake -DCMAKE_C_COMPILER=clang ..
# Default compiler is gcc if not specified
if(NOT CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER gcc)
endif()

project(pascalscript VERSION 0.1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)
# set(CMAKE_C_FLAGS "-m32 -Wall -Wextra -Wpedantic -ggdb")
set(CMAKE_C_FLAGS "-m32 -Wall -Wextra -Wpedantic -ggdb -O3")

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

add_executable(pascalscript ${SOURCES})

# target_compile_options(pascalscript PRIVATE -Wall -Wextra -Wpedantic -O3 -m32)
#  -save-temps -fverbose-asm)
target_link_libraries(pascalscript m)
