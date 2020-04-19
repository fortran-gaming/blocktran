module oscinter
use, intrinsic:: iso_c_binding, only: c_int

implicit none (type, external)

interface

integer(c_int) function kbhit() bind(c, name='_kbhit')
import
end function kbhit

end interface

end module oscinter
