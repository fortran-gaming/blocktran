program testshapes

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use shapes
use rand, only: random_init

implicit none

type(piece) :: S,line,tee,ell,jay,ess,zee,sqr

integer, parameter :: Ny=4, Nx=4, W=6,H=10
integer, parameter :: Ntest = 100 ! arbitrary
integer :: i
integer :: xarr(Ntest) ! test for randomness etc.

call random_init()

!------- shape essentials

! -- initial x position
do i=1,Ntest
  call S%init("I",W=W,H=H)
  xarr(i) = S%x
enddo
call check_x(xarr)

! -- random shape types
if(all(xarr==xarr(1))) call err('non-random shape initial x-position')

! -- initial y position
if(.not.S%y==-1) then
  write(stderr,'(A,I3)') 'initial y position ', S%y
  call err('shape initial y-position not at top of playfield (-1)')
endif
! ----------- construct shapes
call line%init("I",W,H)
if(.not.line%btype=='I') call err('I type')
if(.not.allocated(line%values)) call err('I init')
if(.not.all(shape(line%values)==[Ny, Nx])) call err('I shape')
if(.not.all(shape(line%values)==[Ny, Nx])) call err('I val')


call tee%init("T",W,H)
if(.not.tee%btype=='T') call err('T type')
if(.not.allocated(tee%values)) call err('T init')
if (.not.all(shape(tee%values)==[Ny, Nx])) call err('T shape')
if(.not.all(shape(tee%values)==[Ny, Nx])) call err('T val')

call ell%init("L",W,H)
if(.not.ell%btype=='L') call err('L type')
if(.not.allocated(ell%values)) call err('L init')
if (.not.all(shape(ell%values)==[Ny, Nx])) call err('L shape')
if(.not.all(shape(ell%values)==[Ny, Nx])) call err('L val')

call jay%init("J",W,H)
if(.not.jay%btype=='J') call err('J type')
if(.not.allocated(jay%values)) call err('J init')
if (.not.all(shape(jay%values)==[Ny, Nx])) call err('J shape')
if(.not.all(shape(jay%values)==[Ny, Nx])) call err('J val')

call ess%init("S",W,H)
if(.not.ess%btype=='S') call err('S type')
if(.not.allocated(ess%values)) call err('S init')
if (.not.all(shape(ess%values)==[Ny, Nx])) call err('S shape')
if(.not.all(shape(ess%values)==[Ny, Nx])) call err('S val')

call zee%init("Z",W,H)
if(.not.zee%btype=='Z') call err('Z type')
if(.not.allocated(zee%values)) call err('Z init')
if (.not.all(shape(zee%values)==[Ny, Nx])) call err('Z shape')
if(.not.all(shape(zee%values)==[Ny, Nx])) call err('Z val')

call sqr%init("B",W,H)
if(.not.sqr%btype=='B') call err('B type')
if(.not.allocated(sqr%values)) call err('B init')
if (.not.all(shape(sqr%values)==[Ny, Nx])) call err('B shape')
if(.not.all(shape(sqr%values)==[Ny, Nx])) call err('B val')


print *,'OK shapes'

contains


subroutine check_x(x)

integer, intent(in) :: x(:)

if(.not.(minval(x) >= 1 .and. maxval(x) <= W-Nx)) then
  write(stderr,'(A,I3,A,I3)') 'min(x) ',minval(x),' max(x) ',maxval(x)
  call err('shape initial X position out of playfield')
endif

if(all(x==x(1))) then
  write(stderr,'(A,I1)') 'all X are identically ',x(1)
  call err('non random x')
endif

end subroutine check_x


subroutine err(msg)
  character(*),intent(in) :: msg

  write(stderr,*) msg
  
  stop -1
  
end subroutine err

end program
