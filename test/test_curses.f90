program test_curses
! Test Fortran CURSES interface
use, intrinsic :: iso_c_binding, only: c_ptr, c_int
use cinter, only : initscr, endwin, mvprintw, printw, refresh, LINES, COLS
use sleep_std, only : sleep_ms

implicit none

integer(c_int) :: ierr
type(c_ptr) :: stdscr

stdscr = initscr()

ierr = printw('hi from Curses')
call refresh()
call sleep_ms(1000)

call endwin()

print *,'terminal window detected size: lines',lines,'cols',cols

end program
