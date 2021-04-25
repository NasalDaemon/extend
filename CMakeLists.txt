cmake_minimum_required(VERSION 3.0)

project(extend VERSION 0.1)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)

add_compile_options(
  "-Wall" "-Wpedantic" "-Wextra"
)

add_executable(
  test_xtd
  test/main.cpp
  )

target_link_libraries(
  test_xtd
  ${CONAN_LIBS_boost}
  )