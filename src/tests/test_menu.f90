program test_menu

  use, intrinsic:: iso_fortran_env, only: stdin=>input_Unit
  use, intrinsic:: iso_c_binding, only: c_ptr
  use cinter, only: initscr, refresh, usleep
  use errs, only: endwin
  use menu, only: title
  use shapes, only: field

implicit none

type(c_ptr) :: stdscr

stdscr = initscr()

call title()

call endwin()

end program
