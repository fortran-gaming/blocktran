cmake_minimum_required(VERSION 3.19...3.23)

file(READ ${CMAKE_CURRENT_LIST_DIR}/codemeta.json _j)
string(JSON PROJECT_VERSION GET ${_j} version)

project(blocktran
LANGUAGES C Fortran
VERSION ${PROJECT_VERSION}
DESCRIPTION "Falling-block game in Fortran + Curses"
HOMEPAGE_URL https://www.github.com/fortran-gaming/blocktran
)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

set(CI $ENV{CI})

include(cmake/compilers.cmake)
include(cmake/options.cmake)

if(NOT curses_external)
  find_package(Curses)
endif()

if(NOT CURSES_FOUND)
  include(cmake/curses.cmake)
endif()

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(src/tests)
endif()

# static flags help avoid users needing libgfortran etc. on their Windows system
# True static linking is an advanced topic.

add_executable(blocktran src/main.f90)
target_link_options(blocktran PRIVATE "$<$<AND:$<BOOL:${MINGW}>,$<Fortran_COMPILER_ID:GNU>>:-static;-static-libgfortran>")
target_link_libraries(blocktran PRIVATE game AI cinterc cinter menu shapes blocks keys errs sleep random)

install(TARGETS blocktran EXPORT ${PROJECT_NAME}-targets)

if(BUILD_TESTING)
  add_test(NAME game COMMAND blocktran -d 100 -s 6 6)
  set_tests_properties(game PROPERTIES
  LABELS gui
  DISABLED $<BOOL:${CI}>
  TIMEOUT 30
  RESOURCE_LOCK display
  DEPENDS "shapes;motion;random"
  )
endif()

include(cmake/install.cmake)
