program test_key

use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit
use, intrinsic:: iso_c_binding, only: c_int,c_ptr

use cinter, only: initscr,getch, usleep
use errs, only: endwin

implicit none

type(c_ptr) :: stdscr
integer(c_int) :: ic
logical :: lastEsc=.false.


print *,'press Esc twice to exit. Prints keys pressed and their code'
call usleep(2000000)

stdscr = initscr()

do
  ic = getch()  ! 4-byte integer, automatically prints character!
! read(stdin,*) ic !Nope
  write(stdout,'(I4,A1,A1)',advance='no') ic,' ',achar(13)
  flush(stdout)
  
  if(lastEsc) then
    if(ic==27) exit
    lastEsc=.false.
  else
    if(ic==27) lastEsc=.true.
  endif
  
  call usleep(200000)
end do

call endwin()

end program
