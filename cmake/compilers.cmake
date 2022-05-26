include(CheckSourceCompiles)

# check C-Fortran ABI compatibility
if(NOT abi_ok)
  message(CHECK_START "checking that C ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION} and
    Fortran ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION} can link")
  try_compile(abi_ok ${CMAKE_CURRENT_BINARY_DIR}/abi_check ${CMAKE_CURRENT_LIST_DIR}/abi_check abi_check)
  if(abi_ok)
    message(CHECK_PASS "OK")
  else()
    message(FATAL_ERROR "C compiler and Fortran compiler are ABI-incompatible.")
  endif()
endif()

check_source_compiles(Fortran "
program test
call random_init(.false., .false.)
end program"
f18random)

# always do compiler options after all FindXXX and checks

if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  add_compile_options(
  "$<$<COMPILE_LANGUAGE:Fortran>:-traceback;-warn;-heap-arrays>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fpe0;-debug;-check>"
  )
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-Wall -Wextra
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-fno-backtrace>"
  "$<$<COMPILE_LANGUAGE:Fortran>:-Werror=array-bounds;-Wconversion;-fimplicit-none>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fexceptions;-ffpe-trap=invalid,zero,overflow;-fcheck=all>"
  )
endif()
