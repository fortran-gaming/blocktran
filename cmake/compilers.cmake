set(CMAKE_CONFIGURATION_TYPES "Release;RelWithDebInfo;Debug" CACHE STRING "Build type selections" FORCE)

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    add_compile_options(/arch:native)
    string(APPEND CMAKE_Fortran_FLAGS " /stand:f18 /traceback /warn /heap-arrays")
  else()
    add_compile_options(-march=native)
    string(APPEND CMAKE_Fortran_FLAGS " -stand f18 -traceback -warn -heap-arrays")
  endif()

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fpe0 -debug extended -check all")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-mtune=native)
  string(APPEND CMAKE_Fortran_FLAGS " -Wall -Wextra -Werror=array-bounds -finit-real=nan -Wconversion -fimplicit-none")

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fexceptions -ffpe-trap=invalid,zero,overflow -fcheck=all")

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    string(APPEND CMAKE_Fortran_FLAGS " -std=f2018")
  endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  string(APPEND CMAKE_Fortran_FLAGS " -C -Mdclchk")
endif()

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("implicit none (external); end" f2018impnone SRC_EXT f90)
if(NOT f2018impnone)
  message(FATAL_ERROR "Compiler does not support Fortran 2018 IMPLICIT NONE (EXTERNAL): ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif()

check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)
