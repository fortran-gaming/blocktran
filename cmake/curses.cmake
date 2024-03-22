include(FetchContent)

if(find)
  find_package(Curses)
endif()

if(CURSES_FOUND)
  if(NOT EXISTS ${CURSES_INCLUDE_DIRS}/curses.h)
    find_path(curses_inc
    NAMES curses.h
    HINTS ${CURSES_INCLUDE_DIRS}/ncurses ${CURSES_INCLUDE_DIRS}/pdcurses
    REQUIRED
    )
    list(APPEND CURSES_INCLUDE_DIRS ${curses_inc})
  endif()

  if(NOT TARGET CURSES::CURSES)
    add_library(CURSES::CURSES INTERFACE IMPORTED)
    target_link_libraries(CURSES::CURSES INTERFACE ${CURSES_LIBRARIES})
    target_include_directories(CURSES::CURSES INTERFACE ${CURSES_INCLUDE_DIRS})
  endif()
else()

  FetchContent_Declare(CURSES
  GIT_REPOSITORY https://github.com/scivision/PDCurses.git
  GIT_TAG 743ba4cf0f14c3f416934b11e3c22702068d260b
  GIT_SHALLOW true
  )

  FetchContent_MakeAvailable(CURSES)
endif()

# always true
# check_include_file("${curses_SOURCE_DIR}/curses.h" HAVE_CURSES_H)
# always true
# check_symbol_exists(getch "${curses_SOURCE_DIR}/curses.h" HAVE_GETCH)

# NCurses has symbol getch(), but PDCurses currently uses a macro in curses.h that Fortran can't see.
# Rather than hard-code this fact, test dynamically (maybe PDCurses will implement this differently in the future).
set(CMAKE_REQUIRED_LIBRARIES ${CURSES_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${CURSES_INCLUDE_DIRS})
check_source_compiles(Fortran
"program test
use, intrinsic :: iso_c_binding, only : C_INT
implicit none

interface
integer(C_INT) function getch() result (ch) bind(C)
import C_INT
end function
end interface

integer(C_INT) :: ch

ch = getch()

end program
"
HAVE_Fortran_GETCH
)
