module oscinter
use, intrinsic:: iso_c_binding, only: c_int
implicit none

contains

integer(c_int) function kbhit()
kbhit = -1
end function kbhit

end module oscinter