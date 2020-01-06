cmake_minimum_required(VERSION 3.12)
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Debug or Release")
endif()
project(tetran
  LANGUAGES C Fortran
  VERSION 1.3.0
  DESCRIPTION "Falling-block game in Fortran + Curses"
  HOMEPAGE_URL https://www.github.com/fortran-gaming/tetran)
enable_testing()

include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/compilers.cmake)

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX "~/.local/bin" CACHE PATH "..." FORCE)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})



include(cmake/curses.cmake)

add_subdirectory(src)

if(CURSES_FOUND)
  add_executable(tetran src/main.f90)
  target_compile_options(tetran PRIVATE ${FFLAGS})
  target_link_libraries(tetran PRIVATE cinter menu shapes blocks keys errs)
  add_test(NAME ShortGameGUI COMMAND tetran -d 100 -s 6 6)
  set_tests_properties(ShortGameGUI PROPERTIES TIMEOUT 30)
  install(TARGETS tetran
          RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX})
endif()
