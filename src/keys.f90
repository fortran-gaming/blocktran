module keys

use cinter, only: getch, flushinp
use blocks, only: field, piece, spawn_block, game_over, generate_next_type

implicit none

contains

subroutine handle_input(F, P, NP)
  class(field), intent(in) :: F
  class(piece), intent(inout) :: P
  class(piece), intent(inout), optional :: NP

  character :: next
  integer :: inp_chr

  inp_chr = getch()

  ! esc is first part of three part arrow key sequence
  if (inp_chr == 27) then
      inp_chr = getch()

      if (inp_chr == 91) then
       inp_chr = getch()
       if (inp_chr==65) inp_chr = 87
       if (inp_chr==68) inp_chr = 65
      endif
  endif

  P%movereq=.true. ! rather than typing it for each case
  select case (inp_chr)
  ! yes this handles upper and lower case, for clever clogs.
    case (iachar("A"),iachar("a"))  ! A - left
      call P%move_left()
    case (iachar("S"), iachar("s"),66) ! S - down
      call P%move_down()
    case (iachar("D"), iachar("d"),67) ! D - right
      call P%move_right()
    case (iachar("W"), iachar("w")) ! W - rotate
      call P%rotate()
    case (iachar("Q"), iachar("q")) ! Q - quit
      call game_over(F)

    case (iachar("T"), iachar("t")) ! CHEAT   T - reset current piece position y to top, preserving x position
      P%y = 0
    case (iachar("N"), iachar("n")) ! CHEAT    N - pick a new piece type for the NEXT block
      if(present(NP)) then
        call generate_next_type(next) ! don't pass Nblocks here!
        call NP%init(F, next, x=F%W+5, y=F%H/2)
      endif
    case default ! do nothing
      P%movereq = .false.
  end select

  call flushinp()  ! clear repeating keys from stdin buffer

end subroutine handle_input

end module
