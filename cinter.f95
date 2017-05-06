module cinter
    interface
        subroutine initscr() bind(C)
        end subroutine initscr

        subroutine endwin() bind(C)
        end subroutine endwin

        function getch() result (ch) bind(C)
            use iso_c_binding, only: c_int
            integer(c_int) :: ch
        end function getch

        subroutine timeout(delay) bind (C)
            use iso_c_binding, only: c_int
            integer(c_int), value :: delay
        end subroutine timeout

        subroutine addch(ch) bind (C)
            use iso_c_binding, only: c_char
            character(c_char), value :: ch
        end subroutine addch

        subroutine mvaddch(y, x, ch) bind (C)
            use iso_c_binding, only: c_int, c_char
            integer(c_int), value :: y, x
            character(c_char), value :: ch
        end subroutine mvaddch

        subroutine clear() bind (C)
        end subroutine clear

        subroutine mvprintw(y, x, str) bind (C)
            use iso_c_binding, only: c_int, c_char
            integer(c_int), value :: y, x
            character(c_char) :: str
        end subroutine mvprintw

        subroutine usleep(time) bind (C)
            use iso_c_binding, only: c_int
            integer(c_int), value :: time
        end subroutine usleep
    end interface
end module cinter
