module shapes
use, intrinsic:: iso_c_binding, only: c_int
use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stderr=>error_unit
use rotflip, only: rot90
implicit none
private

type, public :: Field
  ! Microseconds between each automatic downward move
  real :: move_time = 0.5 ! seconds
  integer(c_int) :: sleep_incr = 5e4 !  keyboard polling and screen refresh interval (microseconds).
  ! 1e6 microsec: mushy controls. 1e5 microsec a little laggy. 5e4 about right. 1e4 microsec screen flicker.
  real :: difffact = 1.
  integer :: level = 1
  real :: diffinc = 1.2 ! factor by which level jumps difficulty

  integer :: score = 0
  integer :: Nblock = 0
  integer :: Ncleared = 0 ! total number of lines cleared
  integer :: lines_per_level = 10 ! how many lines to clear to advance to next level
  integer :: bonus(0:4) = [0,40,100,300,1200]

  character(1) :: blockseq(10000) = "" ! record of blocks player experienced
! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns


  integer :: H,W  ! playfield height, width
  ! Playfield: 0 for blank
  integer, allocatable :: screen(:,:)

  logical :: debug = .false.
  integer :: udbg

contains

  procedure, public :: init_
  procedure, public :: levelup

end type

type,extends(Field),public :: Piece
 character :: btype
 character(80) :: why
 character :: ch(12)
 integer :: Nx,Ny ! dims of current realization of piece
 integer :: x,y   ! location of piece in playfield
 integer, allocatable :: values(:,:) ! pixels of piece (third dim is rotation)
 logical :: landed ! this piece cannot move anymore
 logical :: movereq ! piece has requested to move in any direction, need to evaluate if collision first.
contains
 procedure, public :: init
 procedure, public :: check_collision
 procedure, public :: move_right
 procedure, public :: move_left
 procedure, public:: rotate
 procedure, public :: move_down
 procedure, public :: dissolve
 procedure, private :: randomx
 procedure, private :: tell_why
end type

character(*), parameter, public :: Btypes = 'ITLJSZB'

contains

subroutine init_(self, W, H, difffact, debug)

  class(Field), intent(inout) :: self

  integer, intent(in) :: H,W
  real, intent(in), optional :: difffact
  logical, intent(in), optional :: debug

  self%H = H
  self%W = W

  if (present(difffact)) self%difffact = difffact

  if(present(debug)) self%debug = debug

  allocate(self%screen(self%H, self%W))
  self%screen = 0

end subroutine init_


subroutine levelup(self)

  class(field), intent(inout) :: self

  self%level = self%level + 1
  self%difffact = self%difffact * self%diffinc
  self%move_time = self%move_time / self%difffact
end subroutine levelup
!===================================

subroutine init(self,F,btype,x,y)
  class(field), intent(in) :: F
  class(Piece), intent(inout) :: self
  character, intent(in) :: btype
  integer, intent(in), optional :: x,y

  integer :: i,j
  integer, parameter :: Nl = 10

  ! Flang / PGF chokes on backslash, so do achar(92).
  ! also FLang / PGF wants defined length.
  character, parameter :: ch(12) = ["#","$","@","%","&","^","-","/","|", achar(92), "*", "."]



! dynamic generated shapes
integer :: line(4,4) = 0
integer :: tee(3,3) = 0
integer :: ell(3,3) = 0
integer :: jay(3,3) = 0
integer :: ess(3,3) = 0
integer :: zee(3,3) = 0
integer :: sqr(2,2) = 1

integer :: Lt(Nl, Nl) = 0
integer :: Le(Nl, Nl) = 0
integer :: Lr(Nl, Nl) = 0
integer :: La(Nl, Nl) = 0
integer :: Ln(Nl, Nl) = 0
!-----

line(2,:) = 1

tee(2,:) = 1
tee(3,2) = 1

ell(2,1:3) = 1
ell(3,1) = 1

jay(2,1:3) = 1
jay(3,3) = 1

ess(2,2:3) = 1
ess(3,1:2) = 1

zee(2,1:2) = 1
zee(3,2:3) = 1

Lt(1,:) = 1
Lt(:, Nl/2) = 1

Le(::Nl/2,:) = 1
Le(Nl,:) = 1
Le(:,1) = 1


Lr(::Nl/2,:) = 1
Lr(:,1) = 1
Lr(1:Nl/2, Nl) = 1
j = Nl/2
do i = Nl/2+1,Nl
  j = j+1
  Lr(i,j) = 1
enddo

La(:,1) = 1
La(:,Nl) = 1
La(:Nl-1:Nl/2,:) = 1

Ln(:,1) = 1
Ln(:,Nl) = 1
j = 0
do i =1,Nl
  j = j+1
  Ln(i,j) = 1
enddo
!===============================================================================
  if(.not.allocated(F%screen)) call err('must initialize playfield before piece')
  self%screen = F%screen

  self%W = size(self%screen,2)
  self%H = size(self%screen,1)


  self%landed = .false.
  self%movereq = .false.

  self%y = -1
  if(present(y)) self%y = y

  self%btype = btype

  self%ch = ch

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
    case ("t")
      self%values = Lt
    case ("e")
      self%values = Le
    case ("r")
      self%values = Lr
    case ("a")
      self%values = La
    case ("n")
      self%values = Ln
    case ('default')
      call err('unknown shape '//self%btype)
  end select

  self%Ny = size(self%values, 1)
  self%Nx = size(self%values, 2)

  !-------- must come after self%Nx assigned!
  self%x = self%randomx()
  if(present(x)) self%x = x
  !--------

  if(self%debug) then
    print *,'shape ',self%btype,': Ny,Nx ',self%Ny,self%Nx
  endif

end subroutine init


subroutine dissolve(self)
  class(Piece), intent(inout) :: self

  where (self%values /= 0)
     self%values = modulo(self%values + 1, size(self%ch)+1)
  endwhere

end subroutine dissolve


subroutine move_left(self)
  class(Piece), intent(inout) :: self

  if (.not. self%check_collision(self%x-1, self%y)) then
    self%x = self%x - 1
  else
     call self%tell_why()
  endif
end subroutine move_left


subroutine move_right(self)
  class(Piece), intent(inout) :: self

  if (.not. self%check_collision(self%x+1, self%y)) then
    self%x = self%x + 1
  else
    call self%tell_why()
  endif
end subroutine move_right


subroutine move_down(self)
  class(Piece), intent(inout) :: self

  self%landed = self%check_collision(self%x, self%y + 1)

  if (.not.self%landed) then
    self%y = self%y + 1
  else
    call self%tell_why()
  endif
end subroutine move_down


subroutine rotate(self)
  class(Piece), intent(inout) :: self

  self%values = rot90(self%values, 1)

  if (self%check_collision(self%x, self%y)) then
    call self%tell_why('NO rotation:')
    self%values = rot90(self%values, -1)
  endif
end subroutine rotate


logical function check_collision(self, x, y) result (collided)
  class(Piece), intent(inout) :: self
  integer, intent(in) :: x, y

  integer :: B(self%Ny, self%Nx)
  integer :: i, ix, ixs


  B = self%values
  collided = .false.
! always check all, in case rotation

! Floor check
  do i = 1,self%Ny
    if (all(B(i,:) == 0)) cycle

    collided = y + (i-1) > self%H
    if (collided) then
      write(self%why,'(A20,I3,A3,I3)') 'floor hit, y0=',y,'y=',y+(i-1)
      return
    endif
  enddo

  do i = 1,self%Nx
    if(all(B(:,i) == 0)) cycle

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
    if (all(B(i,:) == 0)) cycle ! no part of block in this block row

    collided = any(self%screen(y + (i-1), ix:ixs) + B(i,ix-x+1:ixs-x+1) == 2)
    if (collided) then
      write(self%why,'(A20,I3,A4,I3)')  'block hit, x=',x,' y=',y
      return
    endif
  enddo

end function check_collision


integer function randomx(self)
! NOTE: even if elemental, because it's part of a class, have to %init() then %randomx() each time, if using externally (which would be unusual)
  class(Piece), intent(in) :: self
  real :: r

  if (self%W==0) call err('playfield has zero width. Be sure to intialize playfield before piece?')
  if (.not.allocated(self%values)) call err('piece was not allocated')
  if (self%Nx <1 .or. self%Nx >= self%W) then
    write(stderr,'(A,I3,A,I3)') 'Nx',self%Nx,'  W',self%W
    call err('piece outside playfield @ initial x position')
  endif

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
