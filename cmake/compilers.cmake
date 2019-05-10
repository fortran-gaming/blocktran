if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds
      -finit-real=nan -Wconversion -fimplicit-none
      -fcheck=all) #-Warray-temporaries

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fexceptions -ffpe-trap=invalid,zero,overflow)
  endif()

  if (UNIX AND NOT (APPLE OR CYGWIN))
    list(APPEND FFLAGS -fstack-protector-all)
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    list(APPEND FFLAGS /4Yd /traceback /warn)
  else()
    list(APPEND FFLAGS -implicitnone -traceback -warn)
  endif()

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fpe0 -debug extended -check all)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)

endif()

# ---

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)
