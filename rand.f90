module rand
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit

implicit none

contains

subroutine init_random_seed(debug)
! NOTE: this subroutine is replaced by "call random_init()" in Fortran 2018
logical, intent(in), optional :: debug
integer :: n, u,ios
integer, allocatable :: seed(:)
logical :: dbg

character(*), parameter :: randfn = '/dev/urandom'

dbg = .false.
if (present(debug)) dbg=debug

call random_seed(size=n)
allocate(seed(n))

open(newunit=u, file=randfn, access="stream", &
             form="unformatted", action="read", status="old", iostat=ios)
if (ios/=0) call err('failed to open random source generator file: '//randfn)

read(u,iostat=ios) seed
if (ios/=0) call err('failed to read random source generator file: '//randfn)

close(u)

call random_seed(put=seed)

if (dbg) then
  call random_seed(get=seed)
  print *, 'seed:',seed
endif
end subroutine


subroutine err(msg)
character(*),intent(in) :: msg

write(stderr,*) msg

stop -1

end subroutine err


end module
