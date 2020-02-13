if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-march=native)
  set(CMAKE_Fortran_FLAGS "-Wall -Wextra -Werror=array-bounds -finit-real=nan -Wconversion -fimplicit-none ")

  set(CMAKE_Fortran_FLAGS_DEBUG "-fexceptions -ffpe-trap=invalid,zero,overflow -fcheck=all ")

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    string(APPEND CMAKE_Fortran_FLAGS "-std=f2018 ")
  endif()

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  if(WIN32)
    set(CMAKE_Fortran_FLAGS "/traceback /warn /stand:f18 ")
  else()
    set(CMAKE_Fortran_FLAGS "-traceback -warn -stand f18 ")
  endif()

  set(CMAKE_Fortran_FLAGS_DEBUG "-fpe0 -debug extended -check all ")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  set(CMAKE_Fortran_FLAGS "-C -Mdclchk ")
endif()


include(CheckFortranSourceCompiles)
check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)
