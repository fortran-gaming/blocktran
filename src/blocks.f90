module blocks
use, intrinsic:: iso_fortran_env, only: error_unit

use errs, only: err, endwin, printopts
use cinter, only: mvaddch
use shapes, only: Piece

implicit none
public

logical :: debug=.false.
integer :: udbg


integer, parameter :: Tmax = 10000 ! maximum number of pieces to log
integer :: Ncleared = 0 ! total number of lines cleared
integer, parameter :: bonus(0:4) = [0,40,100,300,1200]

integer :: level=1  ! game level, increases difficulty over time


integer :: H,W  ! playfield height, width
! Playfield: 0 for blank, 1 for block
integer, allocatable :: screen(:,:)

character(*), parameter :: Btypes = 'ITLJSZB'

! Current score
integer :: score = 0, Nblock=0 ! first go around draws two blocks
character(1) :: blockseq(Tmax) = "" ! record of blocks player experienced
! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns


logical :: newhit = .false.


contains

subroutine draw_piece(P)
  class(piece), intent(in) :: P

  integer :: i, j, x, y

! not concurrent since "mvaddch" remembers its position
  do i = 1, P%Ny
    y = i + P%y - P%Ny/2
    do j = 1, P%Nx
      x = j + P%x - P%Nx/2
      
      if (y >= 0 .and. P%values(i, j) /= 0) then
        call mvaddch(y, x, P%ch(P%values(i, j)))
      endif
    end do
  end do
end subroutine draw_piece



impure elemental subroutine generate_next_type(next_type, Nblock)
  character, intent(out) :: next_type
  integer, intent(inout), optional :: Nblock
  real :: r
  integer :: i

  if(present(Nblock)) Nblock = Nblock + 1  ! for game stats

  call random_number(r)

  i = floor(r * len(Btypes)) + 1 ! set this line constant to debug shapes

  next_type = Btypes(i:i)
end subroutine generate_next_type


subroutine freeze(P, NP)
! Called when a piece has hit another and freezes
  class(piece), intent(inout) :: P, NP

  integer :: i, y, ix, x, Nx, ixs
  character(120):: buf

  if(.not. P%landed) return

  x = P%x
  Nx = P%Nx

! not concurrent due to impure "game over"
  ix = max(1, x)
  ixs = min(W, ix + Nx - (ix-x) - 1)
  do i = 1, P%Ny
    if (all(P%values(i, :) == 0)) cycle
    
    y = i-1 + P%y   
   
    if (y <= 1)  then
      write(buf,'(A12,I3,A3,I3)') 'freeze: x=',x,'y=',y
      call game_over(P, buf)
    endif

    where(screen(y, ix:ixs) == 1 .or. P%values(i,ix-x+1:ixs-x+1) ==1)
          screen(y, ix:ixs) = 1
    endwhere
  end do

  call handle_clearing_lines()
  call spawn_block(P, NP)
end subroutine freeze


subroutine spawn_block(P, NP)
  class(piece), intent(inout) :: P
  class(piece), intent(inout), optional :: NP
  integer :: ib
  character :: next_type

  ! make new current piece -- have to do this since "=" copys pointers, NOT deep copy for derived types!
  call P%init(NP%btype,W=W,H=H, debug=debug)

  call generate_next_type(next_type, Nblock)
  call NP%init(next_type, W=W, H=H, x=W+5, y=H/2, debug=debug)

! ----- logging ---------
  if (Nblock>Tmax) then
    ib = Tmax
    blockseq = eoshift(blockseq,1)  !OK array-temp
  else
    ib = Nblock
  endif

  blockseq(ib) = P%btype
! ------ end logging


end subroutine spawn_block


subroutine handle_clearing_lines()
  logical :: lines_to_clear(H)
  integer :: i, counter

  lines_to_clear = all(screen==1,2) ! mask of lines that need clearing
  counter = count(lines_to_clear)   ! how many lines are cleared
  if (counter == 0) return

  Ncleared = Ncleared + counter
  if (debug) write(udbg,*) lines_to_clear, counter

  score = score + bonus(counter)
! not concurrent since it could clear lines above shifted by other concurrent iterations
! i.e. in some cases, it would check an OK line that turns bad after clearing by another elemental iteration.
! also note non-adjacent lines can be cleared at once.
  do i = 1, H
    if (.not.lines_to_clear(i)) cycle
    newhit = .true.
    screen(i,:) = 0 ! wipe away cleared lines
    screen(:i, :) = cshift(screen(:i, :), shift=-1, dim=1)
    ! Bring everything down
  end do
end subroutine handle_clearing_lines


subroutine game_over(cur_piece, msg)
  class(piece), intent(in), optional :: cur_piece
  character(*), intent(in), optional :: msg
  integer :: i
  
  call endwin()
  
  call printopts()

  do i = 1,size(screen,1)
    print '(100I1)',screen(i,:)
  enddo

  if(present(cur_piece)) print *, cur_piece%why
  if(present(msg)) print *, msg
  

  print *,' '
  print *, 'Level:', level
  Print *, 'Score:', score
  print *, 'Number of Blocks:',Nblock
  print *, 'Number of Lines Cleared:',Ncleared
  print *, 'Block Sequence: ',blockseq(:Nblock)

  if (debug) then
    write(udbg,*) 'Block Sequence: ',blockseq(:Nblock)
    close(udbg)
  endif


  stop 'Goodbye from Tetran'

end subroutine game_over



end module blocks
