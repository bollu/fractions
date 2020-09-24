cmake_minimum_required(VERSION 3.4.3)
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard to conform to")
project(fractions  CXX)

# Colors when compiling with clang
# set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always -fno-rtti -g -Wall -Werror")
set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always -fno-rtti -g -Wall")

add_executable(fractions
    fraction.cpp)

target_link_libraries(fractions -lstdc++)
set_target_properties(fractions PROPERTIES CXX_STANDARD 17)
