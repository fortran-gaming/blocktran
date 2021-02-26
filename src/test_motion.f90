program test_motion

! -------- shape horiz. movement & rotation
! FIXME: Each shape should be tested.
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use shapes, only: piece
use fields, only: field

implicit none (type, external)

integer, parameter :: W=10,H=10

if (initial("I")) error stop 'initial'
if (test_floor("I")) error stop 'floor'
if (left_wall("I")) error stop 'left'
if (right_wall("I")) error stop 'right'
if (block_hit("I")) error stop 'block'

print *,'OK motion'


contains

logical function initial(next) result(failed)
character, intent(in) :: next
type(field) :: F
type(piece) :: P

failed = .true.

print *,'intiial position test...'

call F%setup(W=W, H=H)
call P%init(F, btype=next, x=W/2)

if(.not.(P%y==-1)) then
  write(stderr,*) next//' bad initial Y position'
  return
endif

if(P%landed) then
  write(stderr,*) next//' was initially landed'
  return
endif

call print_piece(P)

failed = .false.

end function initial


logical function test_floor(next) result(failed)
character, intent(in) :: next
type(field) :: F
type(piece) :: P
integer :: y ! test positions

failed = .true.

print *, 'floor hit test...'

call F%setup(W=W, H=H)
call P%init(F, next, W/2)

call P%move_down() ! y=0
y = P%y
call P%move_down() ! y=1
if(.not.(y==P%y-1) .and.P%y==1) then
  write(stderr,*) next//' failed to move down',P%why
  return
endif

call P%move_down() ! y=2
call P%move_down() ! y=3
call P%move_down() ! y=4
call P%move_down() ! y=5
call P%move_down() ! y=6
call P%move_down() ! y=7
call P%move_down() ! y=8
call P%move_down() ! y=9 at bottom (allow sliding along floor before final freeze)

if(P%landed) then
  write(stderr,*)  next//' floor before rotation) failing to slide along floor',P%why
  return
endif


call P%move_down() ! y=10-1 landed (frozen) on bottom

if(.not.P%landed) then
  write(stderr,*) next//' (floor before rotation)  failed to land.  Y=',P%y
  return
endif
print *,'OK:  ',P%why


y = P%y
call P%move_down() ! y=10-1 frozen
if(.not.(y==P%y)) then
  write(stderr,*) next//' passed through floor!',P%why
  return
endif
print *,'OK:  ',P%why

y = P%y
P%y=P%y-1; P%landed = .false. ! y=9 manually unfreeze
call P%rotate()
if(P%landed) then
  write(stderr,*) next//' (after rotation) early landing',P%why
  return
endif
!call print_piece(P)
call P%move_down() ! y=6

if(P%landed) then
  ! print *,'x,y', P%x, P%y
  write(stderr,*) next//' (after rotation and movedown) failing to slide along floor',P%why
  return
endif
call P%move_down() ! y=7 landed (frozen) on bottom
if(.not.P%landed) then
  write(stderr,*) next//' (after rotation) failed to land',P%why
  return
endif

y = P%y
call P%move_down() ! y=7 pressing on bottom
if(.not.(y==P%y)) then
  write(stderr,*) next//' passed through floor!',P%why
  return
endif
y = P%y

failed = .false.
end function test_floor


logical function left_wall(next) result(failed)
character, intent(in) :: next
type(field) :: F
type(piece) :: P
integer :: x ! test positions

failed = .true.

print *,'left wall test...'

! -- Left wall
call F%setup(W, H)
call P%init(F, btype=next, x=W/2)

x = P%x
call P%move_left() ! x=4
if (P%x /= x-1) then
  write(stderr,*) next//' failed to move left',P%why
  return
endif

call P%move_left() ! x=3
call P%move_left() ! x=2
call P%move_left() ! x=1 at left wall
if (P%x /= 1) then
  write(stderr,*) next//' passing through left wall!',P%why
  return
endif

x=P%x
!call print_piece(line)
! -- rotate left wall
call P%rotate()
!call print_piece(line)
call P%move_left() ! x=0 at left wall

if (P%x /= 0) then
  write(stderr,*) next//' I move left after rotate',P%why
  return
endif

call P%move_left() ! at left wall
call P%move_left() ! pushing on left wall

if (P%x /= -1) then
  write(stderr,*) next//' I move left collision detection',P%why
  return
endif

failed = .false.
end function left_wall


logical function right_wall(next) result(failed)
character, intent(in) :: next
type(field) :: F
type(piece) :: P
integer :: x ! test positions

failed = .true.

print *,'right wall test...'

call F%setup(W, H)
call P%init(F, next, W/2)

x = P%x
!call print_piece(line)
call P%move_right() ! x=5
if (P%x /= x+1) then
  write(stderr,*) next//' failed to move right',P%why
  return
endif

call P%move_right() ! x=6
call P%move_right() ! x=6
call P%move_right() ! x=7 at right wall
call P%move_right() ! x=7 pushing on right wall
if (P%x /= W-3) then
  write(stderr,*) next//' (before rotate) I move right collision detection',P%why
  return
endif

call P%rotate()
call P%move_right() ! at right wall
call P%move_right() ! pushing on right wall
if (P%x/=W-2) then
  write(stderr,*) next//' (after rotate) I move right collision detection',P%why
  return
endif

failed = .false.
end function right_wall


logical function block_hit(next) result(failed)
character, intent(in) :: next
type(field) :: F
type(piece) :: P
integer :: y ! test positions

failed = .true.

print *,'block hit test...'

call F%setup(W, H)
!-- single pixel object in center bottom of floor
F%screen(H-2,W/2) = 1

!call print_block(F%screen)
call P%init(F, btype=next, x=W/2)
call P%rotate()

!call print_piece(P)
call P%move_down() ! y=0
y = P%y
call P%move_down() ! y=1
if(.not.(y==P%y-1) .and.P%y==1) then
  write(stderr,*) next//' (obj) failure to move down',P%why
  return
endif

call P%move_down() ! y=2
call P%move_down() ! y=3
call P%move_down() ! y=4
call P%move_down() ! y=5 next to bottom (allow sliding along floor before final freeze)
if(P%landed) then
  write(stderr,*) 'x,y', P%x, P%y
  write(stderr,*) next//' (obj before rotation) failing to slide along object',P%why
  return
endif

call P%move_down() ! y=5 landed (frozen) on bottom
  call P%move_down() ! y=5 landed (frozen) on bottom
    call P%move_down() ! y=5 landed (frozen) on bottom
if(.not.P%landed) then
  write(stderr,*) next//' (obj before rotation)  failed to land',P%why
  return
endif

y = P%y
call P%move_down() ! y=5 pressing on bottom
if(.not.(y==P%y)) then
  write(stderr,*) next//' obj passed through floor!',P%why
  return
endif
y = P%y

failed = .false.
end function block_hit


subroutine print_piece(P)
class(piece), intent(in) :: P

call print_block(P%values)
end subroutine print_piece


subroutine print_block(B)
integer, intent(in) :: B(:,:)
integer :: i

do i = 1,size(B,1)
  print '(8I1)', B(i,:)
enddo
end subroutine print_block

end program
