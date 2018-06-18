module random

implicit none

interface std 
  module procedure std_int, std_real
end interface 

interface mean
  module procedure mean_int, mean_real
end interface

contains

impure elemental integer function randint(lo, hi)
integer, intent(in), optional :: lo, hi
integer :: L,H
real :: r

 ! /2 avoids overflow
L = -huge(0)/2
H =  huge(0)/2

if(present(lo)) L = lo
if(present(hi)) H = hi

call random_number(r)

randint = floor(r * (H+1 - L)) + L

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

end module random
