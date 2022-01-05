submodule (cinter) cinter_os

implicit none (type, external)

contains

module procedure kbhit
kbhit = -1
end procedure kbhit

end submodule cinter_os
