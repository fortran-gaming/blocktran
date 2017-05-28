cmake_minimum_required(VERSION 2.8.12)
project(tetran Fortran)

add_compile_options(-std=f2008ts -mtune=native -Werror=array-bounds -fexceptions
                    -Wall -Wextra -Wpedantic -fbacktrace -g)
# -fstack-protector-all  # not for Cygwin

add_library(cinter cinter.f90)  # Ncurses interface

add_executable(tetran blocks.f90 main.f90)
target_link_libraries(tetran cinter ncurses)

add_executable(keys test_key.f90)
target_link_libraries(keys cinter ncurses)
