module random

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit

implicit none

interface std
  procedure std_int, std_real
end interface 

interface mean
  procedure mean_int, mean_real
end interface

!interface
!  module subroutine random_init()
!  end subroutine
!end interface

contains

impure elemental integer function randint(lo, hi)
integer, intent(in) :: lo, hi
real :: r

call random_number(r)

randint = floor(r * (hi + 1 - lo)) + lo

end function randint

!=========================================
pure real function std_real(A) result(std)
real, intent(in) :: A(:)

std = sqrt(sum(abs(A-mean(A))**2) / (size(A)-1))

end function std_real


pure real function std_int(A) result(std)
integer, intent(in) :: A(:)

std = sqrt(sum(abs(A-mean(A))**2) / size(A))

end function std_int
!==============================================

pure real function mean_real(A) result(mean)
real, intent(in) :: A(:)

mean = sum(A) / size(A)

end function mean_real


pure real function mean_int(A) result(mean)
integer, intent(in) :: A(:)

mean = sum(A) / real(size(A))  ! real coerces

end function mean_int

subroutine random_init
! NOTE: this subroutine is replaced by "call random_init()" in Fortran 2018
integer :: i,n, u,ios
integer, allocatable :: seed(:)

character(*), parameter :: randfn = '/dev/urandom'

call random_seed(size=n)
allocate(seed(n))


open(newunit=u, file=randfn, access="stream", form="unformatted", action="read", status="old", iostat=ios)
if (ios==0) then
  read(u,iostat=ios) seed
  close(u)
endif
  
if (ios/=0) then
  write(stderr,*) 'falling back to internal random number generator'
  do i = 1,n
    seed(i) = randint(-1073741823, 1073741823) 
  enddo
endif

call random_seed(put=seed)

end subroutine random_init


end module random
