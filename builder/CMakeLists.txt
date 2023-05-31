cmake_minimum_required(VERSION 3.4.3)
project(builder C CXX)



set(CMAKE_CXX_FLAGS_DEBUG_INIT "-g -O0 -fsanitize=address,undefined -static-libasan")
set(CMAKE_CXX_FLAGS "-O0 -Wall -Werror -Wno-error=unused-result")
set(CMAKE_CXX_FLAGS "-O3 -g -Wall")

add_executable(builder builder.cpp utf8.c duktape/duktape.c)
set_target_properties(builder PROPERTIES CXX_STANDARD 14)

# https://cmake.org/cmake/help/latest/variable/CMAKE_CURRENT_LIST_DIR.html
set(BLOG_ROOT_FOLDER_TRAILING_SLASH "none" CACHE PATH "path to root folder of blog (with README.txt)")
# need an escape, bcause we want to the macro to be '#define VAR "value"', not '#define VAR value'. So we need a layer of `""`.
# https://cmake.cmake.narkive.com/MqLqrjcA/add-definitions-string-values
# Yes, and for increased readability, I would suggest you to use:
#  add_definitions(-DFOO="\\"foo\\"")
# so that you get
# gcc -DFOO="\"foo\"" yourfile.c
# when CMake compiles your file and, obviously, FOO="foo" in C.
target_compile_definitions(builder PUBLIC BLOG_ROOT_FOLDER_TRAILING_SLASH="\"${BLOG_ROOT_FOLDER_TRAILING_SLASH}\"")
message(STATUS "blog root folder (with README.txt): '${BLOG_ROOT_FOLDER_TRAILING_SLASH}'")

install(TARGETS builder DESTINATION bin)
