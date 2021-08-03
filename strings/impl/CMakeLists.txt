cmake_minimum_required(VERSION 3.20)
project(ward C CXX)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS_DEBUG} -g -fdiagnostics-color=always -fsanitize=address,undefined")

# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fdiagnostics-color=always -O2")

add_executable(kmp kmp.cpp)
add_executable(burrowswheeler burrowswheeler.cpp)
add_executable(z z.cpp)
add_executable(duel duel.cpp)
add_executable(suffixtrie suffixtrie.cpp)
add_executable(lcp lcp.cpp)
