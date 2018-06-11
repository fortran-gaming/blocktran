module cinter
use, intrinsic:: iso_c_binding, only: c_int, c_char, c_ptr
use errs, only: err
implicit none

integer(c_int)  :: LINES,COLS
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


integer(c_int) function getch() result (ch) bind(C)
  import
end function getch

subroutine flushinp() bind (c)
end subroutine flushinp

subroutine timeout(delay) bind (C)
  import
  integer(c_int), value :: delay
end subroutine timeout


integer(c_int) function f_addch(ch) result (addch__OUT) bind(c, name='addch')
  import
  character(kind=c_char) , value, intent(in):: ch
end function f_addch


subroutine mvaddch(y, x, ch) bind (C, name='mvaddch')
  import
  integer(c_int), intent(in), value :: y, x
  character(kind=c_char), intent(in), value :: ch
end subroutine mvaddch


subroutine refresh() bind(C, name='refresh')
end subroutine refresh


subroutine clear() bind (C)
end subroutine clear

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

subroutine usleep(time) bind (C)
  import
  integer(c_int), value :: time
end subroutine usleep

end interface

contains

function initscr() result (stdscr__OUT) ! call initscr() but set global variables too
! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses_from_Fortran.html
  type(C_PTR)           :: stdscr__OUT
  stdscr=f_initscr()
  !stdscr=returnstd()
  !curscr=returncur()
  stdscr__OUT=stdscr
  call getmaxyx(stdscr,LINES,COLS)
end function initscr


subroutine addch(ch)
  character(kind=c_char), value, intent(in):: ch                   ! const chtype ch
  integer(c_int) :: addch__OUT
  
  addch__OUT = f_addch(ch) 
   
  if (addch__OUT == FAIL) call err('addch')
end subroutine addch

end module cinter
