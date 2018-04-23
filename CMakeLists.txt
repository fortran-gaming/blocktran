cmake_minimum_required(VERSION 3.0)
project(tetran Fortran)

set(CTEST_OUTPUT_ON_FAILURE ON)

#----- options
add_compile_options(-mtune=native -g)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  add_compile_options(-std=f2008ts -Wall -Wextra -Wpedantic 
  -fexceptions -Werror=array-bounds  -fbacktrace
  -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-real=nan
  -fall-intrinsics
  -Wconversion -fimplicit-none) #-Warray-temporaries \
  if (NOT CYGWIN AND NOT WIN32)
    add_compile_options(-fstack-protector-all)
  endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  add_compile_options(-warn -fpe0 -traceback -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  add_compile_options(-Mallocatable=03)
  list(APPEND FFLAGS -static-flang-libs)
endif()

#---- libraries
find_package(Curses REQUIRED)

add_library(cinter cinter.f90)  # Ncurses interface
target_link_libraries(cinter PRIVATE ${CURSES_LIBRARIES} ${FFLAGS})

#---- executables
add_executable(tetran blocks.f90 main.f90)
target_link_libraries(tetran cinter)

add_executable(keys keytest.f90)
target_link_libraries(keys cinter) 

#---- tests
enable_testing()

if (NOT WIN32)
  add_test(NAME ShortGame COMMAND sh -c "./tetran 10 < ../tests/test_dims.txt")
else()
  # someday if CursesW is implemented...
endif()
