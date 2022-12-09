module sleep_std

use, intrinsic :: iso_c_binding, only : C_INT

implicit none (type, external)

private
public :: sleep_ms

interface
subroutine sleep_ms(millseconds) bind(C, name="c_sleep")
import C_INT
integer(C_INT), intent(in) :: millseconds
end subroutine
end interface

end module sleep_std
