module menu

use, intrinsic :: iso_c_binding, only: c_int, c_ptr
use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
use random, only: randint
use cinter, only: refresh, clear, getch, noecho, cbreak, timeout, printw, kbhit
use sleep_std, only : sleep
use shapes, only: Piece
use fields, only: field
use blocks, only: draw_piece

implicit none (type, external)

character(14) :: buf
integer(c_int), parameter:: y0 = 5, L = 10, W = 80, H = 60
integer :: Nstates
integer(c_int) :: ic

contains

subroutine title(Fld)
class(field), intent(in), optional :: Fld
type(field) :: F
integer(c_int) :: x, i, ierr
type(piece) :: T0, E, T1, R, A, N


if(present(fld)) F = Fld
call F%setup(W=W, H=H)

x=5
T0 = makeLetter(F, y0, x,  "t")
E = makeLetter(F, y0, x+(L+1), "e")
T1 = makeLetter(F, y0, x+2*(L+1), "t")
R = makeLetter(F, y0, x+3*(L+1), "r")
A = makeLetter(F, y0, x+4*(L+1), "a")
N = makeLetter(F, y0, x+5*(L+1), "n")

Nstates = size(T0%ch)

call noecho()
call cbreak()
call timeout(0)
call refresh()
call sleep(250)

do i = 1,Nstates
  if (kbhit() /= 0) then
    if (getch() /= -1) exit
  endif

  call clear()
  if (F%debug) then
    write(buf,'(A6,I2,A3,I3)') 'Loop #', i,' / ',Nstates
    ierr = printw(buf)
  endif

  call dissolve(T0)
  call dissolve(E)
  call dissolve(T1)
  call dissolve(R)
  call dissolve(A)
  call dissolve(N)

  call refresh()
  call sleep(150)
enddo

end subroutine title


type(piece) function makeLetter(F, y0, x0, letter) result(S)

type(field), intent(in) :: F
integer(c_int), intent(in) :: y0, x0
character, intent(in) :: letter


call S%init(F, letter, x=x0, y=y0)

call draw_piece(S)

end function makeLetter


recursive subroutine dissolve(P)
class(piece), intent(inout) :: P
integer :: i
character(10) :: buf2

call P%dissolver()
!! updates random character for each pixel of this piece

do i = 1, randint(0, P%H / (L+1))
  call P%move_down()

  if(P%debug) then
    if (P%landed) then
      write(buf2,'(A6,I2)') 'Move #', i
      if (any(P%screen/=0)) error stop 'screen should be == 0'
      write (stderr,*) buf2//buf//P%btype//' letter was landed during dissolve '//P%why
    endif
  endif

enddo

call draw_piece(P)

end subroutine dissolve

end module
