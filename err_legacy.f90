module errs

use, intrinsic:: iso_fortran_env, only: error_unit,compiler_version, compiler_options
use cinter, only: endwin

implicit none

contains

subroutine err(msg)
  character(*),intent(in) :: msg

  call endwin()

  write(error_unit,*) msg
  stop 'abnormal TETRAN termination'
end subroutine err


subroutine printopts()

end subroutine printopts

end module
