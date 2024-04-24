include_directories("${PROJECT_SOURCE_DIR}/include")

add_library(Toplevel
    "${CMAKE_CURRENT_SOURCE_DIR}/TupleParser.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/Sched.cpp")
set_target_properties(Toplevel PROPERTIES
  LINKER_LANGUAGE CXX)
