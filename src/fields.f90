module fields
use, intrinsic:: iso_c_binding, only: c_int
implicit none
private

type, public :: Field
  ! Microseconds between each automatic downward move
  real :: move_time = 0.5 ! seconds
  integer(c_int) :: sleep_incr = 50000 !  keyboard polling and screen refresh interval (microseconds).
  ! 1e6 microsec: mushy controls. 1e5 microsec a little laggy. 5e4 about right. 1e4 microsec screen flicker.
  real :: difffact = 1.
  integer :: level = 1
  real :: diffinc = 1.2 ! factor by which level jumps difficulty

  integer :: score = 0
  integer :: Nblock = 0
  integer :: Ncleared = 0 ! total number of lines cleared
  integer :: lines_per_level = 10 ! how many lines to clear to advance to next level
  integer :: bonus(-1:4) = [-100,0,40,100,300,1200]

  character(1) :: blockseq(10000) = "" ! record of blocks player experienced
! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns

  integer :: toc, tic ! for each field's update time tracking
  integer :: H,W  ! playfield height, width
  integer :: x0  ! master horizontal origin display coordinate for each playfield
  ! Playfield: 0 for blank
  integer, allocatable :: screen(:,:)

  logical :: debug = .false.
  integer :: udbg
  
  logical :: newhit = .false.
  logical :: cheat = .false.
  logical :: AI

contains

! one per line for PGI, FLang
  procedure, public :: setup
  procedure, public :: levelup
  procedure, public :: clear_lines

end type


contains

subroutine setup(self, W, H, x0, AI, difffact, debug)

  class(Field), intent(inout) :: self

  integer, intent(in) :: H,W
  integer, intent(in), optional :: x0
  logical, intent(in), optional :: AI
  real, intent(in), optional :: difffact
  logical, intent(in), optional :: debug

  self%H = H
  self%W = W

  self%x0 = 1
  if (present(x0)) self%x0 = x0
  
  self%AI = .false.
  if (present(AI)) self%AI = AI
  
  if (present(difffact)) self%difffact = difffact

  if(present(debug)) self%debug = debug

  allocate(self%screen(self%H, self%W))
  self%screen = 0

end subroutine setup


subroutine levelup(self)

  class(field), intent(inout) :: self

  self%newhit = .false.
  
  self%level = self%level + 1
  self%difffact = self%difffact * self%diffinc
  self%move_time = self%move_time / self%difffact
end subroutine levelup


subroutine clear_lines(self)
  class(field), intent(inout) :: self
  logical :: lines_to_clear(self%H)
  integer :: i, counter

  lines_to_clear = all(self%screen==1,2) ! mask of lines that need clearing
  
  counter = count(lines_to_clear)   ! how many lines are cleared
  if (counter == 0) return
  
  if (self%cheat) then
    counter = -1  ! penalty
    self%cheat = .false.
  endif

  self%Ncleared = self%Ncleared + counter
  if (self%debug) write(self%udbg,*) lines_to_clear, counter

  self%score = self%score + self%bonus(counter)
! not concurrent since it could clear lines above shifted by other concurrent iterations
! i.e. in some cases, it would check an OK line that turns bad after clearing by another elemental iteration.
! also note non-adjacent lines can be cleared at once.
  do i = 1, self%H
    if (.not.lines_to_clear(i)) cycle
    self%newhit = .true.
    self%screen(i,:) = 0 ! wipe away cleared lines
    self%screen(:i, :) = cshift(self%screen(:i, :), shift=-1, dim=1)
    ! Bring everything down
  end do
end subroutine clear_lines

end module fields

