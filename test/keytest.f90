program keytest

use, intrinsic :: iso_c_binding, only: c_int,c_ptr, c_char, c_bool

use cinter, only : initscr,getch, cbreak, timeout, printw, refresh, noecho, flushinp, keypad, clear
use sleep_std, only : sleep_ms
use errs, only : endwin

implicit none

type(c_ptr) :: stdscr
integer(c_int) :: ic, ierr
character(kind=c_char, len=7) :: buf
logical(c_bool), parameter :: true=.true.!, false=.false.

stdscr = initscr()
call noecho()
call cbreak()
call timeout(0)
ierr = keypad(stdscr, true)
if (ierr /= 0) then
  call endwin()
  error stop 'problem setting keypad mode'
endif

ierr = printw('press Esc to exit. Prints keys pressed and their code'//new_line(''))
call refresh()

do
  ic = getch()  ! 4-byte integer
  select case (ic)
  case (-1)
    !ierr = printw('waiting for getch ')
    call sleep_ms(200)
    cycle
  case (27)
    exit
  case default
    call sleep_ms(200)
  end select

! read(stdin,*) ic ! Don't do this

! use printw instead.
  !write(stdout,'(I4,A1,A1)',advance='no') ic,' ',achar(13)
  !flush(stdout)

  write(buf, '(A1,I4,2X)') achar(ic), ic
  ierr = printw(buf)

  call refresh()
end do

call endwin()

end program
