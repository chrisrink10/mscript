#######################################################################
#    Copyright 2016 Chris Rink
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#######################################################################

#######################################################################
# PROJECT METADATA
#######################################################################

cmake_minimum_required(VERSION 3.3)
project(mscript)

enable_testing()

# Directory control
set(EXECUTABLE_OUTPUT_PATH "${PROJECT_SOURCE_DIR}/bin/")
set(LIBRARY_OUTPUT_PATH "${PROJECT_SOURCE_DIR}/bin/")
include_directories(${PROJECT_SOURCE_DIR}/deps)
include_directories(${PROJECT_SOURCE_DIR}/lib)

# Compiler and Linker flags
set(C_WARNING_FLAGS "-Wall -Wextra -pedantic")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99 ${C_WARNING_FLAGS}")

#######################################################################
# MAIN EXECUTABLE
#######################################################################

# Library code
set(STREAM_SOURCE_FILES lib/stream/streamreader.c)

# mscript source files
set(MSCRIPT_SOURCE_FILES src/main.c)

add_executable(mscript ${MSCRIPT_SOURCE_FILES}
                       ${STREAM_SOURCE_FILES})

#######################################################################
# TEST EXECUTABLE
#######################################################################

# Special flags for tests
set(C_TEST_WARNING_FLAGS "${CMAKE_C_FLAGS} -Wno-unused-parameter")

# Testing code
set(TESTING_SOURCE_FILES deps/munit/munit.c
                         lib/stream/streamreader.c test/streamreader.c
                         test/main.c)

add_executable(mscript_test ${TESTING_SOURCE_FILES})
set_target_properties(mscript_test PROPERTIES
                                   COMPILE_FLAGS ${C_TEST_WARNING_FLAGS})