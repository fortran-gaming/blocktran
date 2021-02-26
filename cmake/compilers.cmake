include(CheckFortranSourceCompiles)

set(CMAKE_EXPORT_COMPILE_COMMANDS on)

set(CMAKE_CONFIGURATION_TYPES "Release;RelWithDebInfo;Debug" CACHE STRING "Build type selections" FORCE)

# check C-Fortran ABI compatibility
if(NOT abi_ok)
  message(CHECK_START "checking that C and Fortran compilers can link")
  try_compile(abi_ok ${CMAKE_CURRENT_BINARY_DIR}/abi_check ${CMAKE_CURRENT_LIST_DIR}/abi_check abi_check)
  if(abi_ok)
    message(CHECK_PASS "OK")
  else()
    message(FATAL ERROR "C compiler {CMAKE_C_COMPILER_ID} {CMAKE_C_COMPILER_VERSION} and Fortran compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION} are ABI-incompatible.")
  endif()
endif()


check_fortran_source_compiles("call random_init(.false., .false.); end" f18random SRC_EXT f90)

# --- static flags avoid users needing libgfortran etc. on their Windows system
# MacOS and Linux needs more caution as true static linking is an advanced topic.
set(static_flag)
set(static_link_flag)
if(MINGW AND CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  set(static_flag -static)
  set(static_link_flag -static-libgfortran)
  # MinGW / MSYS2 special considerations: https://www.msys2.org/news/#2021-01-31-aslr-enabled-by-default
  list(APPEND static_link_flag -Wl,--default-image-base-low)
endif()

# always do compiler options after all FindXXX and checks

if(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)

  if(WIN32)
    add_compile_options(/QxHost)
    string(APPEND CMAKE_Fortran_FLAGS " /traceback /warn /heap-arrays")
  else()
    add_compile_options(-xHost)
    string(APPEND CMAKE_Fortran_FLAGS " -traceback -warn -heap-arrays")
  endif()

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fpe0 -debug extended -check all")

elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  string(APPEND CMAKE_Fortran_FLAGS " -mtune=native -Wall -Wextra -Werror=array-bounds -finit-real=nan -Wconversion -fimplicit-none")

  string(APPEND CMAKE_Fortran_FLAGS_DEBUG " -fexceptions -ffpe-trap=invalid,zero,overflow -fcheck=all")
endif()
