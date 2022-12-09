cmake_minimum_required(VERSION 3.25.0)
project(student-accommodation)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_FLAGS "-Wall -Wextra -fdiagnostics-color=always")
set(CMAKE_BUILD_TYPE Debug)
enable_testing()

add_executable("sa-test"
   student-accommodation.cpp
   test.cpp)

add_test(NAME "Student Accommodation test suite"
   COMMAND "sa-test")
