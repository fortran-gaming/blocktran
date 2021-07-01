include(ExternalProject)

set(curses_external true CACHE BOOL "build curses")

if(NOT CURSES_ROOT)
  if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set(CURSES_ROOT ${PROJECT_BINARY_DIR} CACHE PATH "CURSES_ROOT")
  else()
    set(CURSES_ROOT ${CMAKE_INSTALL_PREFIX})
  endif()
endif()

set(CURSES_INCLUDE_DIRS ${CURSES_ROOT}/include)
set(CURSES_LIBRARIES ${CURSES_ROOT}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}pdcurses${CMAKE_STATIC_LIBRARY_SUFFIX})

ExternalProject_Add(CURSES
  GIT_REPOSITORY https://github.com/scivision/PDcurses.git
  GIT_TAG 74f5203890baa3160f9552e568970b59ed760919
  INACTIVITY_TIMEOUT 15
  CONFIGURE_HANDLED_BY_BUILD ON
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX:PATH=${CURSES_ROOT} -DBUILD_TESTING:BOOL=off -DCMAKE_BUILD_TYPE=Release
  BUILD_BYPRODUCTS ${CURSES_LIBRARIES}
)

file(MAKE_DIRECTORY ${CURSES_INCLUDE_DIRS})

add_library(CURSES::CURSES IMPORTED INTERFACE)
target_link_libraries(CURSES::CURSES INTERFACE ${CURSES_LIBRARIES})
target_include_directories(CURSES::CURSES INTERFACE ${CURSES_INCLUDE_DIRS})

if(MSVC)
  target_link_libraries(CURSES::CURSES INTERFACE Advapi32)
endif(MSVC)

add_dependencies(CURSES::CURSES CURSES)
