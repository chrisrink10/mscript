cmake_minimum_required(VERSION 3.3)
project(mscript)

set(EXECUTABLE_OUTPUT_PATH "${PROJECT_SOURCE_DIR}/bin/")
set(LIBRARY_OUTPUT_PATH "${PROJECT_SOURCE_DIR}/bin/")

set(C_WARNING_FLAGS "-Wall -Wextra -pedantic")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 ${C_WARNING_FLAGS}")

set(SOURCE_FILES src/main.c)
add_executable(mscript ${SOURCE_FILES})