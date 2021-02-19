cmake_minimum_required(VERSION 3.14)

project(tetran
LANGUAGES C Fortran
VERSION 1.5.0
DESCRIPTION "Falling-block game in Fortran + Curses"
HOMEPAGE_URL https://www.github.com/fortran-gaming/tetran)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

include(cmake/compilers.cmake)

find_package(Curses)

if(NOT CURSES_FOUND)
  include(cmake/curses_external.cmake)
endif()

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

add_executable(tetran src/main.f90)
target_link_libraries(tetran PRIVATE game AI cinterc cinter menu shapes blocks keys errs sleep random)
install(TARGETS tetran
  RUNTIME DESTINATION bin)

if(NOT CI)
add_test(NAME gui:game COMMAND $<TARGET_FILE:tetran> -d 100 -s 6 6)
set_tests_properties(gui:game PROPERTIES TIMEOUT 30 RESOURCE_LOCK display)
endif()
