program randblock
! confirms random distribution of block types
use blocks, only: generate_next_type
use rand, only: random_init
use errs, only: err
implicit none

character(*), parameter :: types = 'ITLJSZB'
integer,parameter :: Ntypes=len(types)
real, parameter :: rtol = 0.01
real :: ideal
character,allocatable :: b(:)
real, allocatable :: e(:)
integer :: i,n, c(Ntypes)
character(32) :: buf

call random_init()

N=1000000
call get_command_argument(1,buf,status=i)
if (i==0) read(buf,*) N

ideal = N/Ntypes

allocate(b(N), e(N))

call generate_next_type(b)

! results
print *,''
print *,'ideal count:',int(ideal)
print *,''
print '(A5,A16,A10)','Block','Count','Error %'
print '(A5,A16,A10)','-----','-----','-------'
do i=1,Ntypes
!do concurrent (i=1:Ntypes) ! even with ifort -parallel, still single core (with print commented)
  c(i) = count(b==types(i:i))
  e(i) = abs(c(i)-ideal) / ideal
  print '(A5,I16,F10.3)',types(i:i), c(i), e(i)*100
enddo

! randomness simple check -- sufficiently uniformly random
if (any(e > rtol)) then 
  call err('non-uniform randomness posssible. Is N > 1000000?')
endif


end program
