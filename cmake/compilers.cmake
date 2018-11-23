if(CMAKE_BUILD_TYPE STREQUAL Debug)
  add_compile_options(-g -O0)
else()
  add_compile_options(-O3)
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  list(APPEND FFLAGS -march=native -Wall -Wextra -Wpedantic -Werror=array-bounds -fcheck=all
  -fexceptions -ffpe-trap=invalid,zero,overflow
  -finit-real=nan -Wconversion -fimplicit-none) #-Warray-temporaries
  
  if (UNIX AND NOT (APPLE OR CYGWIN))
    list(APPEND FFLAGS -fstack-protector-all)
  endif()

  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8)
    list(APPEND FFLAGS -std=f2018)
  endif()
  
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  list(APPEND FFLAGS -warn -fpe0 -traceback)# -debug extended -check all)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Flang)  # https://github.com/flang-compiler/flang/wiki/Fortran-2008
  list(APPEND FFLAGS -Mallocatable=03)
  list(APPEND FLIBS -static-flang-libs)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)

endif()

# ---

include(CheckFortranSourceCompiles)

check_fortran_source_compiles("program es; error stop; end" f2008
                              SRC_EXT f90)

check_fortran_source_compiles("program ri; call random_init(); end" f2018
                              SRC_EXT f90)
