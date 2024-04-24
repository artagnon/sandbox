cmake_minimum_required(VERSION 3.6)
project(Mesosched)

set(CMAKE_CXX_FLAGS "-Werror -g -O0 -std=c++14")
set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(UNITTEST_EXE "${PROJECT_BINARY_DIR}/unittest/UnitTest")
set(VALGRIND_EXE "/usr/local/bin/valgrind")

include_directories("${PROJECT_SOURCE_DIR}/include")
add_subdirectory(src)
add_subdirectory(gtest)
add_subdirectory(unittest)

add_custom_target(check "${UNITTEST_EXE}"
  COMMENT "Running unittests ..."
  DEPENDS UnitTest
  USES_TERMINAL)

add_custom_target(valgrind
  COMMAND "${VALGRIND_EXE}" "--" "${UNITTEST_EXE}"
  COMMENT "Running valgrind tests ..."
  DEPENDS UnitTest
  USES_TERMINAL)
