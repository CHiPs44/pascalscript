cmake_minimum_required(VERSION 3.10)

project(pascalscript C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -m32")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -m32")
set(CMAKE_BUILD_TYPE Release)

file(GLOB SOURCES "src/*.c" "*.c")

if(IS_DIRECTORY ${CMAKE_SOURCE_DIR}/include)
    # target_include_directories(pascalscript PUBLIC include)
endif()

add_executable(pascalscript ${SOURCES})

message(STATUS "Project configured. To build with Ninja, run 'ninja' in the build directory.")
