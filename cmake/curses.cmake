include(ExternalProject)

set(curses_external true CACHE BOOL "build curses")

set(CURSES_INCLUDE_DIRS ${CMAKE_INSTALL_PREFIX}/include)

if(BUILD_SHARED_LIBS)
  if(WIN32)
    set(CURSES_LIBRARIES ${CMAKE_INSTALL_PREFIX}/bin/${CMAKE_SHARED_LIBRARY_PREFIX}pdcurses${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(CURSES_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_SHARED_LIBRARY_PREFIX}pdcurses${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()

else()
  set(CURSES_LIBRARIES ${CMAKE_INSTALL_PREFIX}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}pdcurses${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(curses_args
-DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
-DBUILD_TESTING:BOOL=off
-DCMAKE_BUILD_TYPE=Release
-DBUILD_SHARED_LIBS:BOOL=${BUILD_SHARED_LIBS}
)

ExternalProject_Add(CURSES
GIT_REPOSITORY https://github.com/scivision/PDcurses.git
GIT_TAG 74f5203890baa3160f9552e568970b59ed760919
INACTIVITY_TIMEOUT 15
CONFIGURE_HANDLED_BY_BUILD ON
CMAKE_ARGS ${curses_args}
BUILD_BYPRODUCTS ${CURSES_LIBRARIES}
)

file(MAKE_DIRECTORY ${CURSES_INCLUDE_DIRS})

add_library(CURSES::CURSES IMPORTED INTERFACE)
target_link_libraries(CURSES::CURSES INTERFACE ${CURSES_LIBRARIES}
$<$<BOOL:${MSVC}>:Advapi32>
)
target_include_directories(CURSES::CURSES INTERFACE ${CURSES_INCLUDE_DIRS})

add_dependencies(CURSES::CURSES CURSES)