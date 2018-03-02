cmake_minimum_required(VERSION 2.8.12)
project(tetran Fortran)

#----- options
add_compile_options(-mtune=native -g)
if (NOT ${CYGWIN})
  add_compile_options(-fstack-protector-all)
endif()

if(${CMAKE_Fortran_COMPILER_ID} MATCHES "GNU")
  add_compile_options(-std=f2008ts -Wall -Wextra -Wpedantic 
  -fexceptions -Werror=array-bounds  -fbacktrace
  -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-real=nan
  -fall-intrinsics
  -Wconversion -fimplicit-none) #-Warray-temporaries 
elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES "Intel")
  add_compile_options(-warn -fpe0 -traceback -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} MATCHES "PGI")  # Flang
  add_compile_options(-Mallocatable=03)
endif()

#---- libraries
add_library(cinter cinter.f90)  # Ncurses interface
target_link_libraries(cinter PRIVATE ncurses)

#---- executables
add_executable(tetran blocks.f90 main.f90)
target_link_libraries(tetran cinter)

add_executable(keys keytest.f90)
target_link_libraries(keys cinter)
