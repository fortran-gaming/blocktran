module random

use, intrinsic:: iso_fortran_env, only: stderr=>error_unit

implicit none (type, external)

interface std
procedure std_int, std_real
end interface

interface mean
procedure mean_int, mean_real
end interface

contains

subroutine rand_init(repeatable, image_distinct)
!! if intrinsic random_init available, use it.
logical, intent(in) :: repeatable, image_distinct

@_random_init@

end subroutine rand_init

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


end module random
