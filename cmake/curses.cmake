include(FetchContent)

if(find)
  find_package(Curses)
endif()

if(CURSES_FOUND)
# CMake FindCurses may not find all needed include directories and may need definitions
find_package(Curses REQUIRED)

if(NOT CURSES_HAVE_CURSES_H)
  foreach(d IN LISTS CURSES_INCLUDE_DIRS)
    find_path(curses_inc
    NAMES curses.h
    HINTS ${d}/ncurses ${d}/pdcurses
    NO_DEFAULT_PATH
    )
    if(curses_inc)
      list(APPEND CURSES_INCLUDE_DIRS ${curses_inc})
      break()
    endif()
  endforeach()
endif()

if(NOT TARGET Curses::Curses)
  add_library(Curses::Curses INTERFACE IMPORTED)
  target_link_libraries(Curses::Curses INTERFACE ${CURSES_LIBRARIES})
  target_include_directories(Curses::Curses INTERFACE ${CURSES_INCLUDE_DIRS})
  if(CURSES_HAVE_NCURSES_H OR CURSES_HAVE_NCURSES_NCURSES_H)
    # https://stackoverflow.com/a/67964252/4236751
    # avoids MinGW link failure
    target_compile_definitions(Curses::Curses INTERFACE $<$<BOOL:${MINGW}>:NCURSES_STATIC>)
  endif()
endif()

else()

  FetchContent_Declare(CURSES
  GIT_REPOSITORY https://github.com/scivision/PDCurses.git
  GIT_TAG 743ba4cf0f14c3f416934b11e3c22702068d260b
  GIT_SHALLOW true
  )

  FetchContent_MakeAvailable(CURSES)
endif()


function(check_getch)

# always true
# check_include_file("${curses_SOURCE_DIR}/curses.h" HAVE_CURSES_H)
# always true
# check_symbol_exists(getch "${curses_SOURCE_DIR}/curses.h" HAVE_GETCH)

# NCurses has symbol getch(), but PDCurses currently uses a macro in curses.h that Fortran can't see.
# Rather than hard-code this fact, test dynamically (maybe PDCurses will implement this differently in the future).
set(CMAKE_REQUIRED_LIBRARIES ${CURSES_LIBRARIES})
set(CMAKE_REQUIRED_INCLUDES ${CURSES_INCLUDE_DIRS})

set(CMAKE_TRY_COMPILE_TARGET_TYPE "EXECUTABLE")

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

endfunction(check_getch)

check_getch()
