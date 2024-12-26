include(CheckSourceCompiles)

function(check_abi)

if(CMAKE_VERSION VERSION_GREATER_EQUAL 3.25)
# check C and Fortran compiler ABI compatibility

if(NOT abi_ok)
  message(CHECK_START "checking that C, C++, and Fortran compilers can link")
  try_compile(abi_ok
    PROJECT abi_check
    SOURCE_DIR ${CMAKE_CURRENT_LIST_DIR}/abi_check
  )
  if(abi_ok)
    message(CHECK_PASS "OK")
  else()
    message(FATAL_ERROR "ABI-incompatible compilers:
    C compiler ${CMAKE_C_COMPILER_ID} ${CMAKE_C_COMPILER_VERSION}
    C++ compiler ${CMAKE_CXX_COMPILER_ID} ${CMAKE_CXX_COMPILER_VERSION}
    Fortran compiler ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}
    "
    )
  endif()
endif()

endif()

set(CMAKE_TRY_COMPILE_TARGET_TYPE "STATIC_LIBRARY")

check_source_compiles(Fortran
"program test
intrinsic :: random_init
end program"
HAVE_RANDOM_INIT
)

endfunction(check_abi)

check_abi()

# always do compiler options after all FindXXX and checks

if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  set(compile_opts
  "$<$<COMPILE_LANGUAGE:Fortran>:-traceback;-warn>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fpe0;-debug;-check>"
  "$<$<COMPILE_LANGUAGE:Fortran>:-fpscomp;logicals>"
  )
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(compile_opts -Wall -Wextra
  "$<$<VERSION_LESS:$<Fortran_COMPILER_VERSION>,12.0>:-Wno-maybe-uninitialized>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-fno-backtrace>"
  "$<$<COMPILE_LANGUAGE:Fortran>:-Werror=array-bounds;-Wconversion;-fimplicit-none>"
  "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-fexceptions;-ffpe-trap=invalid,zero,overflow;-fcheck=all>"
  )
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
  set(compile_opts "$<$<COMPILE_LANGUAGE:Fortran>:-Munixlogical>")
endif()
