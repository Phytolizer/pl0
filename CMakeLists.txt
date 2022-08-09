cmake_minimum_required(VERSION 3.23)

project(pl0 LANGUAGES C)

add_executable(pl0 src/main.c)
target_link_libraries(pl0 PRIVATE libhammer.a)
