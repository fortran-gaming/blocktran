module shapes
use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stderr=>error_unit

implicit none
private

type,public :: Piece
 character :: btype
 character(80) :: why
 integer :: rot ! current rotation
 integer :: Nx,Ny ! dims of current realization of piece
 integer :: W,H   ! dims of playfield piece exists in
 integer :: x,y   ! location of piece in playfield
 integer, allocatable :: values(:,:,:) ! pixels of piece (third dim is rotation)
 logical :: landed ! this piece cannot move anymore
 logical :: movereq ! piece has requested to move in any direction, need to evaluate if collision first.
 logical :: debug=.false.
contains
 procedure, public :: init
 procedure, public :: val
 procedure, public :: check_collision
 procedure, public :: move_right
 procedure, public :: move_left
 procedure, public:: rotate
 procedure, public :: move_down
 procedure, private :: randomx
 procedure, private :: tell_why
end type

contains

subroutine init(self,btype,W,H,x,y, debug)

  class(Piece), intent(inout) :: self
  character, intent(in) :: btype
  integer, intent(in) :: W,H
  integer, intent(in), optional :: x,y
  logical, intent(in), optional :: debug

  integer, parameter :: Ny=4,Nx=4

! LINE BLOCK
integer, parameter :: line(Ny,Nx,0:1) = reshape( &
    [0, 0, 0, 0, &
     1, 1, 1, 1, &
     0, 0, 0, 0, &
     0, 0, 0, 0, &

     0, 0, 1, 0, &
     0, 0, 1, 0, &
     0, 0, 1, 0, &
     0, 0, 1, 0], &
     shape(line))

! T BLOCK
integer, parameter :: tee(Ny,Nx,0:3) = reshape( &
    [0, 0, 0, 0, &
     1, 1, 1, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0, &

     0, 1, 0, 0, &
     1, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0, &

     0, 1, 0, 0, &
     1, 1, 1, 0, &
     0, 0, 0, 0, &
     0, 0, 0, 0, &

     0, 1, 0, 0, &
     0, 1, 1, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0], &
     shape(tee))

! L BLOCK
integer, parameter :: ell(Ny,Nx,0:3) = reshape( &
    [0, 0, 0, 0, &
     1, 1, 1, 0, &
     1, 0, 0, 0, &
     0, 0, 0, 0, &

     1, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0, &

     0, 0, 0, 0, &
     0, 0, 1, 0, &
     1, 1, 1, 0, &
     0, 0, 0, 0, &

     0, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 1, 1, 0, &
     0, 0, 0, 0], &
     shape(ell))

! J BLOCK
integer, parameter :: jay(Ny,Nx,0:3) = reshape( &
    [0, 0, 0, 0, &
     1, 1, 1, 0, &
     0, 0, 1, 0, &
     0, 0, 0, 0, &

     0, 1, 0, 0, &
     0, 1, 0, 0, &
     1, 1, 0, 0, &
     0, 0, 0, 0, &

     0, 0, 0, 0, &
     1, 0, 0, 0, &
     1, 1, 1, 0, &
     0, 0, 0, 0, &

     0, 1, 1, 0, &
     0, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0], &
     shape(jay))

! S BLOCK
integer, parameter :: ess(Ny,Nx,0:1) = reshape( &
    [0, 0, 0, 0, &
     0, 1, 1, 0, &
     1, 1, 0, 0, &
     0, 0, 0, 0, &

     1, 0, 0, 0, &
     1, 1, 0, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0], &
     shape(ess))

! Z BLOCK
integer, parameter :: zee(Ny,Nx,0:1) = reshape( &
    [0, 0, 0, 0, &
     1, 1, 0, 0, &
     0, 1, 1, 0, &
     0, 0, 0, 0, &

     0, 0, 1, 0, &
     0, 1, 1, 0, &
     0, 1, 0, 0, &
     0, 0, 0, 0], &
     shape(zee))

! SQUARE BLOCK
integer, parameter :: sqr(Ny,Nx,0:0) = reshape( &
    [0, 1, 1, 0, &
     0, 1, 1, 0, &
     0, 0, 0, 0, &
     0, 0, 0, 0], &
     shape(sqr))
!===============================================================================
  self%landed = .false.
  self%movereq = .false.

  self%Ny = Ny
  self%Nx = Nx

  self%H = H
  self%W = W

  self%y = -1
  if(present(y)) self%y = y

  self%x = self%randomx()
  if(present(x)) self%x = x
  
  self%rot = 0

  self%btype = btype

  ! Fortran 2003+ allocate-on-assign
  select case (self%btype)
    case ("I")
      self%values = line
    case ("T")
      self%values = tee
    case ("L")
      self%values = ell
    case ("J")
      self%values = jay
    case ("S")
      self%values = ess
    case ("Z")
      self%values = zee
    case ("B")
      self%values = sqr
    case ('default')
      call err('unknown shape '//self%btype)
  end select
  
  if(present(debug)) self%debug = debug

end subroutine init


pure integer function val(self)
  class(Piece), intent(in) :: self
  dimension :: val(self%Ny, self%Nx)

  val = self%values(:,:,self%rot)
end function val


subroutine move_left(self, screen)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: screen(:,:)

  if (.not. self%check_collision(self%x-1, self%y, screen)) then
    self%x = self%x - 1
  else
     call self%tell_why()
  endif
end subroutine move_left


subroutine move_right(self, screen)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: screen(:,:)

  if (.not. self%check_collision(self%x+1, self%y, screen)) then
    self%x = self%x + 1
  else
    call self%tell_why()
  endif
end subroutine move_right


subroutine move_down(self, screen)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: screen(:,:)

  self%landed = self%check_collision(self%x, self%y + 1, screen)

  if (.not.self%landed) then
    self%y = self%y + 1
  else
    call self%tell_why()
  endif
end subroutine move_down


subroutine rotate(self, screen)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: screen(:,:)

  self%rot = modulo(self%rot + 1, size(self%values, 3))

  if (self%check_collision(self%x, self%y, screen)) then
    call self%tell_why('NO rotation:')
    self%rot = self%rot - 1
  endif
end subroutine rotate


logical function check_collision(self, x, y, screen) result (collided)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: x, y, screen(:,:)

  integer :: block(self%Ny, self%Nx)
  integer :: i, ix, ixs


  block = self%val()
  collided = .false.
! always check all, in case rotation

! Floor check
  do i = 1,self%Ny
    if (all(block(i,:) == 0)) cycle
    
    collided = y + (i-1) > self%H
    if (collided) then
      write(self%why,'(A20,I3,A3,I3)') 'floor hit, y0=',y,'y=',y+(i-1)
      return
    endif 
  enddo


  do i = 1,self%Nx
    if(all(block(:,i) == 0)) cycle
    
    collided = x + (i-1) < 1 .or. x + (i-1) > self%W
    if (collided) then
      write(self%why,'(A21,I3,A3,I3)') 'wall hit, x0=',x,'x=',x+(i-1)
      return
    endif
  enddo

! other block collision
  ix = max(1,x)
  ixs = min(self%W, ix + self%Nx - (ix-x) - 1)
  do i = 1, self%Ny 
    if (y + (i-1) < 1) cycle        ! this block row above playfield
    if (all(block(i,:) == 0)) cycle ! no part of block in this block row
    
    collided = any(screen(y + (i-1), ix:ixs) + block(i,ix-x+1:ixs-x+1) == 2)
    if (collided) then
      write(self%why,'(A20,I3,A4,I3)')  'block hit, x=',x,' y=',y
      return
    endif
  enddo
  
end function check_collision


integer function randomx(self)
  class(Piece), intent(in) :: self
  real :: r

  call random_number(r)
  randomx = floor(r*(self%W-self%Nx)) + 1   ! 1 to screen width, minus block width
end function randomx


subroutine tell_why(self, msg)
  class(Piece), intent(in) :: self
  character(*), intent(in), optional :: msg
  
  character(:), allocatable :: str
  
  if (.not.self%debug) return

  if (present(msg)) then 
    str = trim(msg)//' '//trim(self%why)
  else
    str = self%why
  endif
  
  write(stdout,'(A50,A1)', advance='no') str, achar(13)
  flush(stdout)

end subroutine tell_why


subroutine err(msg)
  character(*),intent(in) :: msg

  write(stderr,*) msg
  
  stop -1
  
end subroutine err

end module shapes
