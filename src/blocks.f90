module blocks
use, intrinsic:: iso_fortran_env, only: error_unit

use errs, only: err, endwin, printopts
use cinter, only: mvaddch
use shapes, only: Piece
use fields, only: field

implicit none
private

public :: game_over, draw_piece, freeze

contains

subroutine draw_piece(P)
  class(piece), intent(in) :: P

  integer :: i, j, x, y

! not concurrent since "mvaddch" remembers its position
  do i = 1, P%Ny
    y = (i-1) + P%y
    if (y<0) cycle
    
    do j = 1, P%Nx
      x = (j-1) + P%x

      if (P%values(i, j) /= 0) then
        ! zero-indexed, so another -1
        call mvaddch(y=y-1, x=(P%x0-1) + x-1, &
                     ch=P%ch(P%values(i, j)))
      endif
    end do
    
  end do
end subroutine draw_piece


subroutine freeze(F, P, NP)
! Called when a piece has hit another and freezes
  class(piece), intent(inout) :: P, NP
  class(field), intent(inout) :: F

  integer :: i, y, ix, x, Nx, ixs
  character(120):: buf

  if(.not. P%landed) return

  x = P%x
  Nx = P%Nx

! not concurrent due to impure "game over"
  ix = max(1, x)
  ixs = min(F%W, ix + Nx - (ix-x) - 1)
  do i = 1, P%Ny
    if (all(P%values(i, :) == 0)) cycle

    y = i-1 + P%y

    if (y <= 1)  then
      write(buf,'(A12,I3,A3,I3)') 'freeze: x=',x,'y=',y
      call game_over(F, P, buf)
    endif

    where(F%screen(y, ix:ixs) == 1 .or. P%values(i,ix-x+1:ixs-x+1) ==1)
          F%screen(y, ix:ixs) = 1
    endwhere
  end do

  call F%clear_lines()
  call P%spawn_block(F, NP)
end subroutine freeze


subroutine game_over(F, P, msg)
  class(field), intent(in) :: F
  class(piece), intent(in), optional :: P
  character(*), intent(in), optional :: msg
  integer :: i

  call endwin()

  call printopts()

  do i = 1,size(F%screen,1)
    print '(100I1)',F%screen(i,:)
  enddo

  if(present(P)) print *, P%why
  if(present(msg)) print *, msg


  print *,' '
  print *, 'Level:', F%level
  Print *, 'Score:', F%score
  print *, 'Number of Blocks:', F%Nblock
  print *, 'Number of Lines Cleared:', F%Ncleared
  print *, 'Block Sequence: ', F%blockseq(:F%Nblock)

  if (F%debug) then
    write(F%udbg,*) 'Block Sequence: ', F%blockseq(:F%Nblock)
    close(F%udbg)
  endif


  stop 'Goodbye from Tetran'

end subroutine game_over



end module blocks
