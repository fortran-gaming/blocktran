module keys

use, intrinsic:: iso_c_binding, only: c_int
use ai, only: AI_input
use cinter, only: getch, flushinp, kbhit, printw, endwin
use blocks, only: game_over
use shapes, only: piece
use fields, only: field

implicit none (external)

contains

subroutine key_input(F, P, NP)

class(field), intent(inout) :: F
class(piece), intent(inout) :: P
class(piece), intent(inout), optional :: NP

integer :: i
!integer(c_int) :: ierr

if (P%landed) then
  call endwin()
  error stop 'should not have requested input after landing'
endif

if (F%AI) then
  call AI_input(F, P)
  return
endif

if (kbhit() /= 0) then
  ! ierr = printw('waiting on getch')
  i = getch()
else
  i = -1
endif

! keypad mode for arrow keys
select case(i)
case (259)
  i = iachar("w")
case (260)
  i = iachar("a")
case (258)
  i = iachar("s")
case (261)
  i = iachar("d")
end select

P%movereq=.true. ! rather than typing it for each case
select case (i)

case (iachar("a"))  ! A - left
  call P%move_left()
case (iachar("l")) ! L - slam left
  call P%move_left(slam=.true.)

case (iachar("s")) ! S - down
  call P%move_down()
case (iachar("x")) ! X - slam down
  call P%move_down(slam=.true.)

case (iachar("d")) ! D - right
  call P%move_right()
case (iachar("r")) ! R - slam right
  call P%move_right(slam=.true.)

case (iachar("w")) ! W - rotate
  call P%rotate()
case (iachar("v")) ! V - flip vertically
  call P%vertflip()
case (iachar("h")) ! H - flip horizontally
  call P%horizflip()

case (iachar("q")) ! Q - quit
  call game_over(F)

case (iachar("t")) ! CHEAT   T - reset current piece position y to top, preserving x position
  P%y = 0
  F%cheat = .true.
case (iachar("n")) ! CHEAT    N - pick a new piece type for the NEXT block
  if(present(NP)) then
    call NP%init(F, x=F%W+5, y=F%H/2)
  endif
  F%cheat = .true.
case (iachar("c")) ! CHEAT  C - clear lowest line (subtract 100 points as penalty)
  F%screen(F%H,:) = 1
  call F%clear_lines()
  P%screen = F%screen  ! since we didn't generate a new piece (alternatively, could have made a new piece right here)
  F%cheat = .true.

case default ! do nothing
  P%movereq = .false.
end select

call flushinp()  ! clear repeating keys from stdin buffer

end subroutine key_input


end module keys
