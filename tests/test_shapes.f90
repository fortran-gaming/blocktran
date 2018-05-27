program testshapes

use block_init
use cinter, only: err

implicit none

if (.not.all(shape(zee)==[4,4,2])) call err('Z shape error')
if (.not.all(shape(line)==[4,4,2])) call err('I shape error')
if (.not.all(shape(jay)==[4,4,4])) call err('J shape error')
if (.not.all(shape(ess)==[4,4,2])) call err('S shape error')
if (.not.all(shape(ell)==[4,4,4])) call err('L shape error')
if (.not.all(shape(square)==[4,4])) call err('block shape error')
if (.not.all(shape(tee)==[4,4,4])) call err('T shape error')

print *,'OK shapes'
end program
