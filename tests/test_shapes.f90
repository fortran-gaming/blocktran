program testshapes

use shapes
use errs, only: err

implicit none

type(piece) :: S,line,tee,ell,jay,ess,zee,sqr

integer, parameter :: Ny=4, Nx=4, W=6,H=10
integer, parameter :: Ntest = 100 ! arbitrary
integer :: i
integer :: xarr(Ntest) ! test for randomness etc.
!------- shape essentials
do i=1,Ntest
  call S%init("I",W,H)
  xarr(i) = S%x
enddo
if(.not.(minval(xarr) >= 1 .and. maxval(xarr) <= W-1)) call err('shape initial X position out of playfield')
if(all(xarr==xarr(1))) call err('non-random shape initial x-position')
if(.not.S%y==-1) call err('shape initial y-position not at top of playfield')

! ----------- construct shapes
call line%init("I",W,H)
if(.not.line%btype=='I') call err('I type')
if(.not.line%rot==0) call err('I rotation')
if(.not.allocated(line%values)) call err('I init')
if(.not.all(shape(line%values)==[Ny, Nx,2])) call err('I shape')
if(.not.all(shape(line%val())==[Ny, Nx])) call err('I val')


call tee%init("T",W,H)
if(.not.tee%btype=='T') call err('T type')
if(.not.tee%rot==0) call err('T rotation')
if(.not.allocated(tee%values)) call err('T init')
if (.not.all(shape(tee%values)==[Ny, Nx,4])) call err('T shape')
if(.not.all(shape(tee%val())==[Ny, Nx])) call err('T val')

call ell%init("L",W,H)
if(.not.ell%btype=='L') call err('L type')
if(.not.ell%rot==0) call err('L rotation')
if(.not.allocated(ell%values)) call err('L init')
if (.not.all(shape(ell%values)==[Ny, Nx,4])) call err('L shape')
if(.not.all(shape(ell%val())==[Ny, Nx])) call err('L val')

call jay%init("J",W,H)
if(.not.jay%btype=='J') call err('J type')
if(.not.jay%rot==0) call err('J rotation')
if(.not.allocated(jay%values)) call err('J init')
if (.not.all(shape(jay%values)==[Ny, Nx,4])) call err('J shape')
if(.not.all(shape(jay%val())==[Ny, Nx])) call err('J val')

call ess%init("S",W,H)
if(.not.ess%btype=='S') call err('S type')
if(.not.ess%rot==0) call err('S rotation')
if(.not.allocated(ess%values)) call err('S init')
if (.not.all(shape(ess%values)==[Ny, Nx,2])) call err('S shape')
if(.not.all(shape(ess%val())==[Ny, Nx])) call err('S val')

call zee%init("Z",W,H)
if(.not.zee%btype=='Z') call err('Z type')
if(.not.zee%rot==0) call err('Z rotation')
if(.not.allocated(zee%values)) call err('Z init')
if (.not.all(shape(zee%values)==[Ny, Nx,2])) call err('Z shape')
if(.not.all(shape(zee%val())==[Ny, Nx])) call err('Z val')

call sqr%init("B",W,H)
if(.not.sqr%btype=='B') call err('B type')
if(.not.sqr%rot==0) call err('B rotation')
if(.not.allocated(sqr%values)) call err('B init')
if (.not.all(shape(sqr%values)==[Ny, Nx,1])) call err('B shape')
if(.not.all(shape(sqr%val())==[Ny, Nx])) call err('B val')


print *,'OK shapes'
end program
