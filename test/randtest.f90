program rand_test
! confirms random distribution of block types
use, intrinsic:: iso_fortran_env, only: dp=>real64
use shapes, only: gen_type
use random, only: randint, std, mean

implicit none

character(*), parameter :: types = 'ITLJSZOD'
integer,parameter :: Ntypes=len(types)
real, parameter :: rtol = 0.05
real :: ideal
character,allocatable :: b(:)
real, allocatable :: e(:)
integer, allocatable :: f(:), g(:)
integer :: i,n, c(Ntypes), u
character(16) :: buf

call random_init(.false., .false.)

do i=1,10
  print*,randint(1,8)
enddo

N=1000000
call get_command_argument(1,buf,status=i)
if (i==0) read(buf,*) N

ideal = N/Ntypes

allocate(b(N), e(Ntypes), f(N), g(N))

do i = 1,N
  b(i) = gen_type()
enddo
! results
print '(/,A,I6)', 'ideal count:',int(ideal)
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
  error stop 'non-uniform randomness posssible. Is N > 1000000?'
endif

! -----------

do i = 1,N
  f(i) = randint(-1073741823,  1073741823)
enddo

print '(/,A,I15,A,F15.3)','huge(int)',huge(0), 'huge(real)',huge(0.)
print *,'expected std, mean',real(huge(0), dp) / sqrt(12._dp), 0.
print *,'std, mean randint()',std(f), mean(f)
print *,'a few values',f(:6)

open(newunit=u, file='/dev/urandom', access="stream", form="unformatted", action="read", status="old", iostat=i)
if (i==0) then
  read(u) g
  close(u)

  print '(/,A)','/dev/urandom a few values...'
  print *,'std, mean /dev/urandom',std(g), mean(g)
endif

! valgrind cleanup
deallocate(b, e, f, g)

end program
