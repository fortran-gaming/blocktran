include(CheckFortranSourceCompiles)
include(CheckFortranCompilerFlag)

set(CMAKE_CONFIGURATION_TYPES "Release;RelWithDebInfo;Debug" CACHE STRING "Build type selections" FORCE)

check_fortran_source_compiles("implicit none (type, external); end" f2018impnone SRC_EXT f90)
if(NOT f2018impnone)
  message(FATAL_ERROR "Compiler does not support Fortran 2018 IMPLICIT NONE (type, external): ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
endif()

check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)

# --- static flags avoid users needing libgfortran etc. on their system.
set(static_flag)
set(static_link_flag)
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  set(static_flag -static)
  set(static_link_flag -static-libgfortran)
  if(MINGW)
    # MinGW / MSYS2 special considerations: https://www.msys2.org/news/#2021-01-31-aslr-enabled-by-default
    list(APPEND static_link_flag -Wl,--default-image-base-low)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel AND NOT WIN32)
  # this test doesn't fail on Windows, even though the flag is not for Windows
  set(static_flag -static-intel)
endif()

# always do compiler options after all FindXXX and checks

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

  if(WIN32)
    add_compile_options(/QxHost)
    string(APPEND CMAKE_Fortran_FLAGS " /stand:f18 /traceback /warn /heap-arrays")
  else()
    add_compile_options(-xHost)
    string(APPEND CMAKE_Fortran_FLAGS " -stand f18 -traceback -warn -heap-arrays")
  endif()

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fpe0 -debug extended -check all")

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)

  add_compile_options(-mtune=native)
  string(APPEND CMAKE_Fortran_FLAGS " -Wall -Wextra -Werror=array-bounds -finit-real=nan -Wconversion -fimplicit-none")

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fexceptions -ffpe-trap=invalid,zero,overflow -fcheck=all")

  check_fortran_compiler_flag(-std=f2018 HAS_F2018)
  if(HAS_F2018)
    string(APPEND CMAKE_Fortran_FLAGS " -std=f2018")
  endif()

endif()
