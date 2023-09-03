module errs

use, intrinsic:: iso_fortran_env, only: error_unit
use cinter, only: endwin

implicit none

contains

subroutine err(msg)

character(*),intent(in) :: msg

call endwin()
write(error_unit,*) msg
error stop

end subroutine err

end module
