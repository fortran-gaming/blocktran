cmake_minimum_required(VERSION 2.8.12)
project(tetran Fortran)

#----- options
add_compile_options(-std=f2008ts -mtune=native -Werror=array-bounds -fexceptions
                    -Wall -Wextra -Wpedantic -fbacktrace -g)
if (NOT ${CYGWIN})
  add_compile_options(-fstack-protector-all)
endif()

#---- libraries
add_library(cinter cinter.f90)  # Ncurses interface

#---- executables
add_executable(tetran blocks.f90 main.f90)
target_link_libraries(tetran cinter ncurses)

add_executable(keys test_key.f90)
target_link_libraries(keys cinter ncurses)
