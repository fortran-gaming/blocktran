module keys

use cinter, only: getch, flushinp
use blocks

implicit none

contains
  subroutine handle_input(moved,landed,cur_y,next_type)
    logical, intent(out) :: moved, landed
    integer, intent(inout) :: cur_y
    character, intent(inout) :: next_type
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

    moved=.true. ! rather than typing it for each case
    landed=.false.
    select case (inp_chr)
    ! yes this handles upper and lower case, for clever clogs.
      case (97,65)  ! A - left
        call move_left()
      case (115,83,66) ! S - down
        landed = move_down()
      case (100,68,67) ! D - right
        call move_right()
      case (119,87) ! W - rotate
        call rotate_piece()
      case (113,81) ! Q - quit
        call game_over()
      case (116,84) ! CHEAT   T - reset current piece position y to top, preserving x position
        cur_y = 0
      case (78,110) ! CHEAT    N - pick a new piece type for the NEXT block
        call generate_next_type(next_type)  ! don't pass Nblocks here!
      case default ! do nothing
        moved = .false.
    end select
    
    call flushinp()  ! clear repeating keys from stdin buffer
    
  end subroutine handle_input
  
end module
