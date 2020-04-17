find_package(PkgConfig)

find_package(Curses REQUIRED)
# CMake FindCurses is goofed up on some platforms, missing the include directory!
pkg_check_modules(pc_ncurses ncurses QUIET)
find_path(_ncinc NAMES curses.h
  HINTS ${pc_ncurses_INCLUDE_DIRS} ${CURSES_INCLUDE_DIRS}/ncurses)

if(_ncinc)
  list(APPEND CURSES_INCLUDE_DIRS ${_ncinc})
endif()

message(STATUS "CURSES include: ${CURSES_INCLUDE_DIRS}")
