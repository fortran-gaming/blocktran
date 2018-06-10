module menu
use, intrinsic:: iso_c_binding, only: c_int
use cinter, only: mvaddch
implicit none

contains

subroutine title

integer(c_int), parameter:: y0 = 2, L = 10

call drawT(y0, 2, L)
call drawE(y0, 15, L)
call drawT(y0, 28, L)
call drawR(y0, 41, L)
call drawA(y0, 54, L)
call drawN(y0, 67, L)

end subroutine title


subroutine drawT(y0, x0, L)
integer(c_int), intent(in) :: y0, x0, L
integer(c_int) :: y,x

y = y0
do x = x0,x0+L
  call mvaddch(y, x, '#')
enddo
x = x0 + L/2
do y = y0,y0+L
  call mvaddch(y, x, '#')
enddo

end subroutine


subroutine drawE(y0, x0, L)
integer(c_int), intent(in) :: y0, x0, L
integer(c_int) :: y,x

do y = y0,y0+L,5
  do x = x0,x0+L
    call mvaddch(y, x, '#')
  enddo
enddo

x = x0
do y = y0,y0+L
  call mvaddch(y, x, '#')
enddo

end subroutine drawE


subroutine drawR(y0, x0, L)
integer(c_int), intent(in) :: y0, x0, L
integer(c_int) :: y,x
real :: yf

y=y0
do x = x0,x0+L
  call mvaddch(y, x, '#')
enddo
y = y0 + L/2
do x = x0,x0+L
  call mvaddch(y, x, '#')
enddo
x = x0+L
do y = y0, y0 + L/2
  call mvaddch(y, x, '#')
enddo
yf = y
do x = x0,x0+L
  call mvaddch(y, x, '\')
  yf = yf + 0.45
  y = floor(yf) 
enddo

x = x0
do y = y0,y0+L
  call mvaddch(y, x, '#')
enddo

end subroutine drawR


subroutine drawA(y0, x0, L)
integer(c_int), intent(in) :: y0, x0, L
integer(c_int) :: y,x

x=x0
do y = y0, y0+L
  call mvaddch(y, x, '#')
enddo
x=x0+L
do y = y0, y0+L
  call mvaddch(y, x, '#')
enddo
y=y0
do x = x0, x0+L
  call mvaddch(y, x, '#')
enddo
y=y0+L/2
do x = x0, x0+L
  call mvaddch(y, x, '#')
enddo

end subroutine drawA


subroutine drawN(y0, x0, L)
integer(c_int), intent(in) :: y0, x0, L
integer(c_int) :: y,x
real :: yf

x=x0
do y = y0, y0+L
  call mvaddch(y, x, '#')
enddo
x=x0+L
do y = y0, y0+L
  call mvaddch(y, x, '#')
enddo


y = y0
yf = y
do x = x0,x0+L
  call mvaddch(y, x, '\')
  yf = yf + 1
  y = floor(yf) 
enddo

end subroutine drawN


end module
