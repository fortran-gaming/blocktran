module keys

use cinter, only: getch, flushinp
use blocks, only: piece, screen, spawn_block, game_over, generate_next_type

implicit none

contains
  subroutine handle_input(cur_piece, next_piece)
    class(piece), intent(inout) :: cur_piece
    class(piece), intent(inout), optional :: next_piece
    
    character :: next_type
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

    cur_piece%movereq=.true. ! rather than typing it for each case
    select case (inp_chr)
    ! yes this handles upper and lower case, for clever clogs.
      case (97,65)  ! A - left
        call cur_piece%move_left(screen)
      case (115,83,66) ! S - down
        call cur_piece%move_down(screen)
      case (100,68,67) ! D - right
        call cur_piece%move_right(screen)
      case (119,87) ! W - rotate
        call cur_piece%rotate(screen)
      case (113,81) ! Q - quit
        call game_over()
      case (116,84) ! CHEAT   T - reset current piece position y to top, preserving x position
        cur_piece%y = 0
      case (78,110) ! CHEAT    N - pick a new piece type for the NEXT block
        if(present(next_piece)) then
          call generate_next_type(next_type) ! don't pass Nblocks here!
          call next_piece%init(next_type, W=next_piece%W, H=next_piece%H, &
                               x=next_piece%W+5, y=next_piece%H/2)
        endif  
      case default ! do nothing
        cur_piece%movereq = .false.
    end select

    call flushinp()  ! clear repeating keys from stdin buffer

  end subroutine handle_input

end module
