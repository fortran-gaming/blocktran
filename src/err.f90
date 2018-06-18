module errs

use, intrinsic:: iso_fortran_env, only: error_unit,compiler_version, compiler_options

implicit none

interface

subroutine endwin() bind(C)
! ncurses restares previous terminal contents (before program was run)
end subroutine endwin

end interface

contains

subroutine err(msg)
  character(*),intent(in) :: msg

  call endwin()

  write(error_unit,*) msg
  error stop
end subroutine err


subroutine printopts()

  print *, compiler_version()
  !print *, compiler_options()

end subroutine printopts

end module
