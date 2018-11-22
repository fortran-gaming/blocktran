module errs

use, intrinsic:: iso_fortran_env, only: error_unit

implicit none

interface
  subroutine endwin() bind(C)
  ! ncurses restores previous terminal contents (before program was run)
  end subroutine endwin
end interface

contains

subroutine err(msg)
  character(*),intent(in) :: msg

  call endwin()

  write(error_unit,*) msg
  stop 1
end subroutine err



end module
