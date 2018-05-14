module cinter
  use, intrinsic:: iso_c_binding, only: c_int, c_char
  use, intrinsic:: iso_fortran_env, only: error_unit
  
  interface
    subroutine initscr() bind(C)
    end subroutine initscr

    subroutine endwin() bind(C)
    ! ncurses restares previous terminal contents (before program was run)
    end subroutine endwin

    function getch() result (ch) bind(C)
      import c_int
      integer(c_int) :: ch
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
  
  subroutine err(msg)
    character(*),intent(in) :: msg
    
    call endwin()
    
    write(error_unit,*) msg
    stop 'abnormal TETRAN termination'
  end subroutine err
end module cinter
