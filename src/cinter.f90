module cinter

use, intrinsic:: iso_c_binding, only: c_int, c_char, c_ptr, c_bool
use oscinter, only: kbhit

implicit none (external)

integer(c_int)  :: LINES, COLS
type(c_ptr) :: stdscr,curscr
integer(c_int), parameter :: FAIL = -1

interface

!--- function that use cinter.c

! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses_from_Fortran.html
function f_initscr() result (initscr__OUT) bind(C, name='initscr')
import
type(c_ptr):: initscr__OUT         ! WINDOW *initscr
end function f_initscr

subroutine getmaxyx(win,y,x) bind(C, name='macro_getmaxyx')
import
type (c_ptr), value :: win
integer(c_int) :: y,x
end subroutine getmaxyx

!--- functions that interface directly with Curses

subroutine endwin() bind(C)
! ncurses restores previous terminal contents (before program was run)
end subroutine endwin

integer(c_int) function getch() result (ch) bind(C)
import
end function getch

subroutine flushinp() bind (c)
end subroutine flushinp

subroutine timeout(delay) bind (C)
!! timeout(0) => non-blocking getch() (-1 if no keypress)
import
integer(c_int), value :: delay
end subroutine timeout

function nodelay(win, bf) result (ierr) bind(C, name='nodelay')
!! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses.f90
import
INTEGER(C_INT) :: ierr
type(C_PTR) ,value:: win                  ! const WINDOW *win
logical(C_BOOL) ,value:: bf                   ! bool bf
end function nodelay


function keypad(win, bf) result (ierr) bind(C, name='keypad')
!! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses.f90
import
INTEGER(C_INT) :: ierr          ! int keypad
type(C_PTR) ,value:: win                  ! const WINDOW *win
logical(C_BOOL) ,value:: bf                   ! bool bf
end function keypad
!--------------------


integer(c_int) function f_addch(ch) result (addch__OUT) bind(c, name='addch')
import
character(kind=c_char) , value, intent(in):: ch
end function f_addch


subroutine mvaddch(y, x, ch) bind (C, name='mvaddch')
import
integer(c_int), intent(in), value :: y, x
character(kind=c_char), intent(in), value :: ch
end subroutine mvaddch


subroutine refresh() bind(C)
end subroutine refresh


subroutine clear() bind(C)
!! clear entire screen
end subroutine clear

subroutine border() bind(C)
!! draw border
end subroutine border

subroutine noecho() bind (C)
! don't echo keypresses to screen
end subroutine noecho

subroutine cbreak() bind (C)
! disable line buffer
end subroutine cbreak

subroutine mvprintw(y, x, str) bind (C)
import
integer(c_int), intent(in), value :: y, x
character(kind=c_char),intent(in) :: str
end subroutine mvprintw

integer(c_int) function printw(str) bind (C)
import
character(kind=c_char),intent(in) :: str
end function printw

subroutine usleep(time) bind (C)
import
integer(c_int), value :: time
end subroutine usleep

end interface

contains

function initscr() result (stdscr__OUT) ! call initscr() but set global variables too
! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses_from_Fortran.html
type(C_PTR)           :: stdscr__OUT
stdscr = f_initscr()
!stdscr=returnstd()
!curscr=returncur()
stdscr__OUT=stdscr
call getmaxyx(stdscr, LINES, COLS)
if(LINES < 0) error stop 'Curses: could not get LINES'
if(COLS < 0) error stop 'Curses: could not get COLS'
end function initscr


subroutine addch(ch)
character(kind=c_char), value, intent(in):: ch  !< const chtype ch
integer(c_int) :: ierr

ierr = f_addch(ch)

if (ierr == FAIL) error stop 'addch'
end subroutine addch

end module cinter
