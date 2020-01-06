if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds
      -finit-real=nan -Wconversion -fimplicit-none) #-Warray-temporaries

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fexceptions -ffpe-trap=invalid,zero,overflow -fcheck=all)
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    list(APPEND FFLAGS /traceback /warn /stand:f18)
  else()
    list(APPEND FFLAGS -traceback -warn -stand f18)
  endif()

  if(CMAKE_BUILD_TYPE STREQUAL Debug)
    list(APPEND FFLAGS -fpe0 -debug extended -check all)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(FFLAGS -C -Mdclchk)
endif()

# ---

include(CheckFortranSourceCompiles)
check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)
