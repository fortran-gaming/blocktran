module blocks
use, intrinsic:: iso_fortran_env, only: error_unit

use errs, only: err, endwin, printopts
use cinter, only: mvaddch
use shapes, only: Field,Piece, btypes

implicit none
public

logical :: newhit = .false.

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
        call mvaddch(y=y-1, x=x-1, &
                     ch=P%ch(P%values(i, j)))
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

  call handle_clearing_lines(F)
  call spawn_block(F, P, NP)
end subroutine freeze


subroutine spawn_block(F, P, NP)
  class(field) , intent(inout) :: F
  class(piece), intent(inout) :: P
  class(piece), intent(inout), optional :: NP
  integer :: ib
  character :: next

  ! make new current piece -- have to do this since "=" copys pointers, NOT deep copy for derived types!
  call P%init(F, NP%btype)

  call generate_next_type(next, F%Nblock)
  call NP%init(F, next, x=F%W+5, y=F%H/2)

! ----- logging ---------
  if (F%Nblock > size(F%blockseq)) then
    ib = size(F%blockseq)
    F%blockseq = eoshift(F%blockseq,1)  !OK array-temp
  else
    ib = F%Nblock
  endif

  F%blockseq(ib) = P%btype
! ------ end logging
end subroutine spawn_block


subroutine handle_clearing_lines(F)
  class(field), intent(inout) :: F
  logical :: lines_to_clear(F%H)
  integer :: i, counter

  lines_to_clear = all(F%screen==1,2) ! mask of lines that need clearing
  counter = count(lines_to_clear)   ! how many lines are cleared
  if (counter == 0) return

  F%Ncleared = F%Ncleared + counter
  if (F%debug) write(F%udbg,*) lines_to_clear, counter

  F%score = F%score + F%bonus(counter)
! not concurrent since it could clear lines above shifted by other concurrent iterations
! i.e. in some cases, it would check an OK line that turns bad after clearing by another elemental iteration.
! also note non-adjacent lines can be cleared at once.
  do i = 1, F%H
    if (.not.lines_to_clear(i)) cycle
    newhit = .true.
    F%screen(i,:) = 0 ! wipe away cleared lines
    F%screen(:i, :) = cshift(F%screen(:i, :), shift=-1, dim=1)
    ! Bring everything down
  end do
end subroutine handle_clearing_lines


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
