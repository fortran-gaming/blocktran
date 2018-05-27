cmake_minimum_required(VERSION 3.7)
project(tetran Fortran C)
enable_testing()

#----- options
if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL GNU)
  list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fbacktrace -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow 
  -finit-real=nan -Wconversion -fimplicit-none) #-Warray-temporaries
  if (NOT CYGWIN AND NOT WIN32)
    list(APPEND FFLAGS -fstack-protector-all)
  endif()
  
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 8.1)
    list(APPEND FFLAGS -std=f2018)
  else()
    list(APPEND FFLAGS -std=f2008ts)
  endif()
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Intel)
  list(APPEND FFLAGS -warn -fpe0 -traceback -debug extended -check all
       -parallel)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  list(APPEND FFLAGS -Mallocatable=03)
  list(APPEND FLIBS -static-flang-libs)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  list(APPEND FFLAGS -Mallocatable=03)
endif()

set(okcomp GNU Intel)
#---- libraries
find_package(Curses REQUIRED)

add_library(shapes block_init.f90)

add_library(blocks blocks.f90)
target_compile_options(blocks PRIVATE ${FFLAGS})
target_link_libraries(blocks PRIVATE cinter shapes)

add_library(keys keys.f90)
target_compile_options(keys PRIVATE ${FFLAGS})
target_link_libraries(keys PRIVATE cinter blocks)

add_library(cinterc cinter.c)
target_compile_options(cinterc PRIVATE ${CFLAGS})
target_link_libraries(cinterc PRIVATE ${CURSES_LIBRARIES})

if(CMAKE_Fortran_COMPILER_ID IN_LIST okcomp)
  add_library(cinter cinter.f90)
else()
  add_library(cinter cinter_legacy.f90)
endif()
target_compile_options(cinter PRIVATE ${FFLAGS})
target_link_libraries(cinter PRIVATE cinterc ${CURSES_LIBRARIES} ${FLIBS})

#---- executables
add_executable(tetran main.f90)
target_compile_options(tetran PRIVATE ${FFLAGS})
target_link_libraries(tetran PRIVATE cinter blocks keys)
if (NOT WIN32)
  add_test(NAME ShortGame COMMAND sh -c "./tetran -d 10. < ../tests/test_dims.txt")
  set_tests_properties(ShortGame PROPERTIES TIMEOUT 10) 
else()
  # someday if CursesW is implemented...
endif()

add_executable(testkeys tests/keytest.f90)
target_compile_options(testkeys PRIVATE ${FFLAGS})
target_link_libraries(testkeys PRIVATE cinter) 

add_executable(blockrand tests/randtest.f90)
target_compile_options(blockrand PRIVATE ${FFLAGS})
target_link_libraries(blockrand PRIVATE cinter blocks)
add_test(NAME RandomBlocks COMMAND blockrand)

add_executable(shapetest tests/test_shapes.f90 block_init.f90)
target_compile_options(shapetest PRIVATE ${FFLAGS})
target_link_libraries(shapetest PRIVATE cinter)
add_test(NAME BlockShapes COMMAND shapetest)

