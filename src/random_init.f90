subroutine random_init(repeatable, image_distinct)
!! very rudimentary
implicit none

logical, intent(in) :: repeatable, image_distinct

integer :: N, i
integer, allocatable :: seed(:)

call random_seed(size=N)

allocate(seed(N))

if (repeatable) then
  call random_seed(put=[(i, i=0, N-1)])
else
  call system_clock(seed(1))
  seed(2:N) = seed(1) + [(i, i=2, N)]
  call random_seed(put=seed)
end if

if(image_distinct) error stop "use a Fortran complier with intrinsic random_init()"


end subroutine
