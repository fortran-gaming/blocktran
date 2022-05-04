program test_menu

use, intrinsic:: iso_c_binding, only: c_ptr, c_int
use cinter, only: initscr, endwin, nodelay
use menu, only: title

implicit none (type, external)

type(c_ptr) :: stdscr

stdscr = initscr()


call title()

call endwin()

end program
