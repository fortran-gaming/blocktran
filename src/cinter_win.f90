submodule (cinter) cinter_os

implicit none (type, external)

interface
integer(c_int) function kbhit_win() bind(c, name='_kbhit')
import c_int
end function kbhit_win
end interface

contains

module procedure kbhit
kbhit = kbhit_win()
end procedure kbhit


end submodule cinter_os
