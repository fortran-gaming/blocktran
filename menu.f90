module menu
use, intrinsic:: iso_c_binding, only: c_int
use cinter, only: mvaddch, usleep, refresh, clear
use shapes, only: Piece
use blocks, only: draw_piece
implicit none

integer(c_int), parameter:: y0 = 5, L = 10, H=60, W=80
integer(c_int) :: screen(H, W) = 0

contains

subroutine title()

integer(c_int) :: x, i
type(piece) :: T0, E, T1, R, A, N

x=5
T0 =  makeLetter(y0, x,  "t")
E = makeLetter(y0, x+(L+1), "e")
T1 = makeLetter(y0, x+2*(L+1), "t")
R = makeLetter(y0, x+3*(L+1), "r")
A = makeLetter(y0, x+4*(L+1), "a")
N = makeLetter(y0, x+5*(L+1), "n")

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


type(piece) function makeLetter(y0, x0, letter) result(S)
integer(c_int), intent(in) :: y0, x0
character, intent(in) :: letter


call S%init(letter, W=W, H=H, x=x0, y=y0)

call draw_piece(S)
end function makeLetter


subroutine dissolve(P)
type(piece), intent(inout) :: P
real :: r
integer :: i

call P%dissolve()

call random_number(r)
do i = 1, floor(r*H/12)
  call P%move_down(screen)
enddo
call draw_piece(P)


end subroutine dissolve

end module



