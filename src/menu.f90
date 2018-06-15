module menu
use, intrinsic:: iso_c_binding, only: c_int
use cinter, only: mvaddch, usleep, refresh, clear
use shapes, only: Piece, Field
use blocks, only: draw_piece
implicit none

integer(c_int), parameter:: y0 = 5, L = 10, W = 80, H = 60

contains

subroutine title(Fld)

  type(field), intent(in), optional :: Fld
  type(field) :: F
  integer(c_int) :: x, i
  type(piece) :: T0, E, T1, R, A, N

if(present(fld)) F = Fld
call F%init_(W=W, H=H)

x=5
T0 =  makeLetter(F, y0, x,  "t")
E = makeLetter(F, y0, x+(L+1), "e")
T1 = makeLetter(F, y0, x+2*(L+1), "t")
R = makeLetter(F, y0, x+3*(L+1), "r")
A = makeLetter(F, y0, x+4*(L+1), "a")
N = makeLetter(F, y0, x+5*(L+1), "n")

call refresh()
call usleep(250000)
do i = 1,size(T0%ch)
  call clear()

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
type(piece), intent(inout) :: P
real :: r
integer :: i

call P%dissolve()

call random_number(r)
do i = 1, floor(r*P%H/10)
  call P%move_down()
enddo
call draw_piece(P)


end subroutine dissolve

end module



