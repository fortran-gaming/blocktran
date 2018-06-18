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
  write(error_unit,*) 'ERROR STOP'
  
  stop -1
end subroutine err


subroutine printopts()

end subroutine printopts

end module
