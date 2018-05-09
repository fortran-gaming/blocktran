cmake_minimum_required(VERSION 3.7)
project(tetran Fortran)
enable_testing()

#----- options
if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O0)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  add_compile_options(-march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fbacktrace -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow 
  -finit-real=nan -Wconversion -fimplicit-none) #-Warray-temporaries
  if (NOT CYGWIN AND NOT WIN32)
    add_compile_options(-fstack-protector-all)
  endif()
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 8.1)
    add_compile_options(-std=f2018)
  else()
     add_compile_options(-std=f2008ts)
  endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  add_compile_options(-warn -fpe0 -traceback -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  add_compile_options(-Mallocatable=03)
  list(APPEND FFLAGS -static-flang-libs)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  set(FFLAGS -Mallocatable=03)
endif()

set(okcomp GNU Intel)
#---- libraries
find_package(Curses REQUIRED)

if(CMAKE_Fortran_COMPILER_ID IN_LIST okcomp)
  add_library(cinter cinter.f90)
else()
  add_library(cinter cinter_legacy.f90)
endif()
target_link_libraries(cinter PRIVATE ${CURSES_LIBRARIES} ${FFLAGS})

#---- executables
if(CMAKE_Fortran_COMPILER_ID IN_LIST okcomp)
  add_executable(tetran blocks.f90 main.f90)
else()
  add_executable(tetran blocks.f90 main_legacy.f90)
endif()
target_link_libraries(tetran PRIVATE cinter)

add_executable(keys keytest.f90)
target_link_libraries(keys PRIVATE cinter) 

#---- tests
if (NOT WIN32)
  add_test(NAME ShortGame COMMAND sh -c "./tetran 10 < ../tests/test_dims.txt")
else()
  # someday if CursesW is implemented...
endif()
