module cinter
use, intrinsic:: iso_c_binding, only: c_int, c_char, c_ptr
implicit none

integer(C_INT)  :: LINES,COLS
type(C_PTR) :: stdscr,curscr

interface

! http://www.urbanjost.altervista.org/LIBRARY/libscreen/ncurses/pdsrc/ncurses_from_Fortran.html
function f_initscr() result (initscr__OUT) bind(C, name='initscr')
   import c_ptr
   type(C_PTR):: initscr__OUT         ! WINDOW *initscr
end function f_initscr

subroutine getmaxyx(win,y,x) bind(C, name='macro_getmaxyx')
  import c_ptr,c_int
  type (C_PTR), value :: win
  integer(C_INT) :: y,x
end subroutine getmaxyx


!-----------

    subroutine endwin() bind(C)
    ! ncurses restares previous terminal contents (before program was run)
    end subroutine endwin

    integer(c_int) function getch() result (ch) bind(C)
      import c_int
    end function getch

    subroutine flushinp() bind (c)
    end subroutine flushinp

    subroutine timeout(delay) bind (C)
      import c_int
      integer(c_int), value :: delay
    end subroutine timeout

    subroutine addch(ch) bind (C)
      import c_char
      character(c_char), intent(in), value :: ch
    end subroutine addch

    subroutine mvaddch(y, x, ch) bind (C)
      import c_int, c_char
      integer(c_int), intent(in), value :: y, x
      character(c_char), intent(in), value :: ch
    end subroutine mvaddch

    subroutine clear() bind (C)
    end subroutine clear

    subroutine noecho() bind (C)
    ! don't echo keypresses to screen
    end subroutine noecho

    subroutine cbreak() bind (C)
    ! disable line buffer
    end subroutine cbreak

    subroutine mvprintw(y, x, str) bind (C)
      import c_int, c_char
      integer(c_int), intent(in), value :: y, x
      character(c_char),intent(in) :: str
    end subroutine mvprintw

    subroutine usleep(time) bind (C)
      import c_int
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

end module cinter
