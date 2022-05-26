program test_shapes

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use shapes, only: piece
use fields, only: field

implicit none (type, external)

type(field) :: F
type(piece) :: S,line,tee,ell,jay,ess,zee,oh

integer, parameter :: Ny=4, Nx=4, W=6,H=10
integer, parameter :: Ntest = 100 ! arbitrary
integer :: i
integer :: xarr(Ntest) ! test for randomness etc.

call random_init(.false., .false.)

call F%setup(W=W, H=H)  ! must generate playfield before any pieces, or they will die when they realize they're outside a playfield

!print '(6I1)',transpose(F%screen)
print *,'H ',F%H, 'W ',F%W
!------- shape essentials

print *, 'initial x-position test....'
do i=1,Ntest
  call S%init(F, "I")
  xarr(i) = S%x
enddo
call check_x(xarr)

print *, 'random shape types test...'
if(all(xarr==xarr(1))) error stop 'non-random shape initial x-position'

print *,'initial y position test...'
if(.not.S%y==-1) then
  write(stderr,'(A,I3)') 'initial y position ', S%y
  error stop 'shape initial y-position not at top of playfield (-1)'
endif

print *, 'test I'
call line%init(F, "I")
if(.not.line%btype=='I') error stop 'I type'
if(.not.allocated(line%values)) error stop 'I init'
if(.not.all(shape(line%values)==[Ny, Nx])) error stop 'I shape'

print *, 'test T'
call tee%init(F, "T")
if(.not.tee%btype=='T') error stop 'T type'
if(.not.allocated(tee%values)) error stop 'T init'
if (.not.all(shape(tee%values)==[Ny-1, Nx-1])) error stop 'T shape'

print *, 'test L'
call ell%init(F, "L")
if(.not.ell%btype=='L') error stop 'L type'
if(.not.allocated(ell%values)) error stop 'L init'
if(.not.all(shape(ell%values)==[Ny-1, Nx-1])) error stop 'L shape'

print *, 'test J'
call jay%init(F, "J")
if(.not.jay%btype=='J') error stop 'J type'
if(.not.allocated(jay%values)) error stop 'J init'
if (.not.all(shape(jay%values)==[Ny-1, Nx-1])) error stop 'J shape'

print *, 'test S'
call ess%init(F, "S")
if(.not.ess%btype=='S') error stop 'S type'
if(.not.allocated(ess%values)) error stop 'S init'
if (.not.all(shape(ess%values)==[Ny-1, Nx-1])) error stop 'S shape'

print *, 'test Z'
call zee%init(F, "Z")
if(.not.zee%btype=='Z') error stop 'Z type'
if(.not.allocated(zee%values)) error stop 'Z init'
if (.not.all(shape(zee%values)==[Ny-1, Nx-1])) error stop 'Z shape'

print *, 'test O'
call oh%init(F, "O")
if(.not. oh%btype=='O') error stop 'O type'
if(.not.allocated(oh%values)) error stop 'O init'
if (.not.all(shape(oh%values)==[Ny-2, Nx-2])) error stop 'O shape'


print *,'OK shapes'

contains


subroutine check_x(x)

integer, intent(in) :: x(:)

if(.not.(minval(x) >= 1 .and. maxval(x) <= W-Nx)) then
  write(stderr,'(A,I3,A,I3)') 'min(x) ',minval(x),' max(x) ',maxval(x)
  error stop 'shape initial X position out of playfield'
endif

if(all(x==x(1))) then
  write(stderr,'(A,I1)') 'all X are identically ',x(1)
  error stop 'non random x'
endif

end subroutine check_x

end program
