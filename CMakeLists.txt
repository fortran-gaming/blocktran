cmake_minimum_required(VERSION 3.20)

file(READ ${CMAKE_CURRENT_LIST_DIR}/codemeta.json _j)
string(JSON PROJECT_VERSION GET ${_j} version)

project(blocktran
LANGUAGES C Fortran
VERSION ${PROJECT_VERSION}
DESCRIPTION "Falling-block game in Fortran + Curses"
HOMEPAGE_URL https://www.github.com/fortran-gaming/blocktran
)

include(CTest)

set(CI $ENV{CI})

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR} CACHE PATH "default install path" FORCE)
endif()

include(cmake/compilers.cmake)

include(cmake/curses.cmake)

add_executable(blocktran app/main.f90)
target_link_libraries(blocktran PRIVATE game AI cinterc cinter menu shapes blocks keys errs sleep random)

add_subdirectory(src)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()

if(NOT BUILD_SHARED_LIBS)
  # static flags help avoid users needing libgfortran etc. on their Windows system
  # True static linking is an advanced topic.
  target_link_options(blocktran PRIVATE "$<$<AND:$<BOOL:${MINGW}>,$<Fortran_COMPILER_ID:GNU>>:-static;-static-libgfortran>")
endif()

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
