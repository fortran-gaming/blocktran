module menu
use, intrinsic:: iso_c_binding, only: c_int, c_ptr
use rand, only: randint
use cinter, only: mvaddch, usleep, refresh, clear, getch, noecho, cbreak, timeout
use errs, only: err
use shapes, only: Piece
use fields, only: field
use blocks, only: draw_piece
implicit none

character(10) :: buf
integer(c_int), parameter:: y0 = 5, L = 10, W = 80, H = 60

contains

subroutine title(Fld)
  class(field), intent(in), optional :: Fld
  type(field) :: F
  integer(c_int) :: x, i
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

call noecho()
call cbreak()
call timeout(0)
call refresh()
call usleep(250000)

do i = 1,size(T0%ch)
  if (getch() /= -1) exit

  call clear()
  write(buf,'(A6,I2)') 'Loop #', i

  call dissolve(T0)
  call dissolve(E)
  call dissolve(T1)
  call dissolve(R)
  call dissolve(A)
  call dissolve(N)

  call refresh()
  call usleep(150000)
enddo

end subroutine title


type(piece) function makeLetter(F, y0, x0, letter) result(S)

  type(field), intent(in) :: F
  integer(c_int), intent(in) :: y0, x0
  character, intent(in) :: letter


call S%init(F, letter, x=x0, y=y0)

call draw_piece(S)

end function makeLetter


subroutine dissolve(P)
  class(piece), intent(inout) :: P
  integer :: i
  character(10) :: buf2

  call P%dissolve()

  do i = 1, randint(0, P%H / (L+1))
    call P%move_down()
    
    if (P%landed) then
      write(buf2,'(A6,I2)') 'Move #', i
      if (any(P%screen/=0)) call err('screen should be == 0')
      call err(buf2//buf//P%btype//' letter was landed during dissolve '//P%why)
    endif
    
  enddo

  call draw_piece(P)

end subroutine dissolve

end module



