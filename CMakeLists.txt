cmake_minimum_required(VERSION 3.12)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(tetran
  LANGUAGES C Fortran
  VERSION 1.3.2
  DESCRIPTION "Falling-block game in Fortran + Curses"
  HOMEPAGE_URL https://www.github.com/fortran-gaming/tetran)
enable_testing()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/curses.cmake)

add_subdirectory(src)
add_subdirectory(src/tests)

set(CI $ENV{CI})  # have to use internal variable, not ENV{} directly

if(CURSES_FOUND)
  add_executable(tetran src/main.f90)
  target_link_libraries(tetran PRIVATE AI cinterc cinter menu shapes blocks keys errs)
  install(TARGETS tetran
    RUNTIME DESTINATION bin)
  if(NOT CI)
    add_test(NAME ShortGameGUI COMMAND $<TARGET_FILE:tetran> -d 100 -s 6 6)
    set_tests_properties(ShortGameGUI PROPERTIES TIMEOUT 30)
  endif()
endif()
