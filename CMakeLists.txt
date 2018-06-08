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
  list(APPEND FFLAGS -warn -fpe0 -traceback)# -debug extended -check all)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  list(APPEND FFLAGS -Mallocatable=03)
  list(APPEND FLIBS -static-flang-libs)
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL PGI)
  list(APPEND FFLAGS -Mallocatable=03)
endif()

set(okcomp GNU Intel)
#---- libraries
find_package(Curses)

if(CURSES_FOUND)
  if(CMAKE_Fortran_COMPILER_ID IN_LIST okcomp)
    add_library(errs err.f90)
  else()
    add_library(errs err_legacy.f90)
  endif()
  target_compile_options(errs PRIVATE ${FFLAGS})
  target_link_libraries(errs PRIVATE ${CURSES_LIBRARIES} ${FLIBS})

  add_library(cinterc cinter.c)
  target_compile_options(cinterc PRIVATE ${CFLAGS})
  target_link_libraries(cinterc PRIVATE errs ${CURSES_LIBRARIES})

  add_library(cinter cinter.f90)
  target_compile_options(cinter PRIVATE ${FFLAGS})
  target_link_libraries(cinter PRIVATE cinterc ${CURSES_LIBRARIES} ${FLIBS})

  add_executable(tetran main.f90)
  target_compile_options(tetran PRIVATE ${FFLAGS})
  target_link_libraries(tetran PRIVATE cinter blocks keys errs)
  add_test(NAME ShortGame COMMAND tetran -d 100 -s 4 4)
  set_tests_properties(ShortGame PROPERTIES TIMEOUT 10)
  
  add_library(blocks blocks.f90)
  target_compile_options(blocks PRIVATE ${FFLAGS})
  target_link_libraries(blocks PRIVATE errs cinter shapes)

  add_library(keys keys.f90)
  target_compile_options(keys PRIVATE ${FFLAGS})
  target_link_libraries(keys PRIVATE cinter blocks)

#  executables

  add_executable(testkeys tests/keytest.f90)
  target_compile_options(testkeys PRIVATE ${FFLAGS})
  target_link_libraries(testkeys PRIVATE errs cinter)
  
  add_executable(blockrand tests/randtest.f90)
  target_compile_options(blockrand PRIVATE ${FFLAGS})
  target_link_libraries(blockrand PRIVATE errs blocks)
  add_test(NAME RandomBlocks COMMAND blockrand)
endif()

add_library(shapes block_init.f90)
target_compile_options(shapes PRIVATE ${FFLAGS})

add_executable(shapetest tests/test_shapes.f90)
target_compile_options(shapetest PRIVATE ${FFLAGS})
target_link_libraries(shapetest PRIVATE shapes)
add_test(NAME BlockShapes COMMAND shapetest)

add_executable(motiontest tests/test_motion.f90)
target_compile_options(motiontest PRIVATE ${FFLAGS})
target_link_libraries(motiontest PRIVATE shapes)
add_test(NAME BlockMotion COMMAND motiontest)
