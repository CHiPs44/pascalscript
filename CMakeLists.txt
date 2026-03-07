cmake_minimum_required(VERSION 3.10.0)

# Allow users to specify compiler (gcc or clang)
# Usage: cmake -DCMAKE_C_COMPILER=gcc .. or cmake -DCMAKE_C_COMPILER=clang ..
# Default compiler is gcc if not specified
if(NOT CMAKE_C_COMPILER)
  set(CMAKE_C_COMPILER gcc)
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Debug)
endif()

set(PS_VERSION_MAJOR 0)
set(PS_VERSION_MINOR 1)
set(PS_VERSION_PATCH 2)
set(PS_VERSION_BUILD 3)

project(pascalscript VERSION ${PS_VERSION_MAJOR}.${PS_VERSION_MINOR}.${PS_VERSION_PATCH}.${PS_VERSION_BUILD} LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_FLAGS "-m32 -Wall -Wextra -Wpedantic -O3")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/include/ps_version.h.in ${CMAKE_CURRENT_SOURCE_DIR}/include/ps_version.h)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)

file(GLOB SOURCES ${CMAKE_CURRENT_SOURCE_DIR}/src/*.c)

add_executable(pascalscript ${SOURCES})

target_link_libraries(pascalscript m)
