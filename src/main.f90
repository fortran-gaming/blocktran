program tetran

use random, only: rand_init
use game, only : cmd_parse, main_game

implicit none (type, external)

integer :: W, H

call rand_init(.false., .false.)

call cmd_parse(W=W, H=H)

call main_game(W=W, H=W)

end program
