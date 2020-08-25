cmake_minimum_required(VERSION 3.4.3)
project(builder C CXX)


set(CMAKE_CXX_FLAGS "-fsanitize=address,undefined -static-libasan -g -O0 -Wall -Werror")
#set(CMAKE_CXX_FLAGS "-O3 -g -Wall")
add_executable(builder builder.cpp utf8.c duktape/duktape.c)
set_target_properties(builder PROPERTIES CXX_STANDARD 14)
