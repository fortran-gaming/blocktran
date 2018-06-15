program motiontest
! -------- shape horiz. movement & rotation
! FIXME: Each shape should be tested.
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use shapes, only: piece, field

implicit none

integer, parameter :: W=8,H=10


call initial("I")
call test_floor("I")
call left_wall("I")
call right_wall("I")
call block_hit("I")

print *,'OK motion'

contains

subroutine initial(next)
  character, intent(in) :: next
  type(field) :: F
  type(piece) :: P

  print *,'intiial position test...'

  call F%init_(W=W, H=H)
  call P%init(F, next, x=W/2)

  if(.not.(P%y==-1)) call err('initial Y position')
  if(P%landed) call err('initial landed')
  call print_piece(P)
end subroutine initial

subroutine test_floor(next)
  character, intent(in) :: next
  type(field) :: F
  type(piece) :: P
  integer :: y ! test positions

  print *, 'floor hit test...'

  call F%init_(W=W, H=H)
  call P%init(F, next, W/2)

  call P%move_down() ! y=0
  y = P%y
  call P%move_down() ! y=1
  if(.not.(y==P%y-1) .and.P%y==1) call err('failure to move down')
  call P%move_down() ! y=2
  call P%move_down() ! y=3
  call P%move_down() ! y=4
  call P%move_down() ! y=5
  call P%move_down() ! y=6
  call P%move_down() ! y=7 next to bottom (allow sliding along floor before final freeze)
  if(P%landed) then
    ! print *,'x,y', P%x, P%y
    call err('(floor before rotation) failing to slide along floor')
  endif
  call P%move_down() ! y=7 landed (frozen) on bottom
      call P%move_down() ! y=7 landed (frozen) on bottom
      call P%move_down() ! y=7 landed (frozen) on bottom
  if(.not.P%landed) call err('(floor before rotation)  failed to land')
  y = P%y
  call P%move_down() ! y=7 pressing on bottom
  if(.not.(y==P%y)) call err('passed through floor!')
  y = P%y

  P%y=P%y-1; P%landed = .false. ! y=5 manually unfreeze
  call P%rotate()
  if(P%landed) call err('(after rotation) early landing')
  !call print_piece(P)
  call P%move_down() ! y=6

  if(P%landed) then
    ! print *,'x,y', P%x, P%y
    call err('(after rotation and movedown) failing to slide along floor')
  endif
  call P%move_down() ! y=7 landed (frozen) on bottom
  if(.not.P%landed) call err('(after rotation) failed to land')
  y = P%y
  call P%move_down() ! y=7 pressing on bottom
  if(.not.(y==P%y)) call err('passed through floor!')
  y = P%y
end subroutine test_floor


subroutine left_wall(next)
  character, intent(in) :: next
  type(field) :: F
  type(piece) :: P
  integer :: x ! test positions

  print *,'left wall test...'

  ! -- Left wall
  call F%init_(W, H)
  call P%init(F, next, W/2)

  x = P%x
  call P%move_left() ! x=3
  if (P%x /= x-1) call err('I move left')
  call P%move_left() ! x=2
  call P%move_left() ! x=1 at left wall
  if (P%x /= 1) call err('passing through left wall!')
  x=P%x
  !call print_piece(line)
  ! -- rotate left wall
  call P%rotate()
  !call print_piece(line)
  call P%move_left() ! x=0 at left wall

  if (P%x /= 0) call err('I move left after rotate')
  call P%move_left() ! at left wall
  call P%move_left() ! pushing on left wall

  if (P%x /= -1) call err('I move left collision detection')
end subroutine left_wall


subroutine right_wall(next)
  character, intent(in) :: next
  type(field) :: F
  type(piece) :: P
  integer :: x ! test positions

  print *,'right wall test...'

  call F%init_(W, H)
  call P%init(F, next, W/2)

  x = P%x
  !call print_piece(line)
  call P%move_right() ! x=5
  if (P%x /= x+1) call err('I move right')
  call P%move_right() ! x=6
  call P%move_right() ! x=6
  call P%move_right() ! x=7 at right wall
  call P%move_right() ! x=7 pushing on right wall
  if (P%x /= W-3) call err('(before rotate) I move right collision detection')
  call P%rotate()
  call P%move_right() ! at right wall
  call P%move_right() ! pushing on right wall
  if (P%x/=W-2) call err('(after rotate) I move right collision detection')

  print *,'OK right bolock test.'
end subroutine right_wall


subroutine block_hit(next)
  character, intent(in) :: next
  type(field) :: F
  type(piece) :: P
  integer :: y ! test positions

  print *,'block hit test...'

  call F%init_(W, H)
  !-- single pixel object in center bottom of floor
  F%screen(H-2,W/2) = 1

  !call print_block(F%screen)
  call P%init(F, next, W/2)
  call P%rotate()

  !call print_piece(P)
  call P%move_down() ! y=0
  y = P%y
  call P%move_down() ! y=1
  if(.not.(y==P%y-1) .and.P%y==1) call err('(obj) failure to move down')
  call P%move_down() ! y=2
  call P%move_down() ! y=3
  call P%move_down() ! y=4
  call P%move_down() ! y=5 next to bottom (allow sliding along floor before final freeze)
  if(P%landed) then
    print *,'x,y', P%x, P%y
    call err('(obj before rotation) failing to slide along object')
  endif
  call P%move_down() ! y=5 landed (frozen) on bottom
    call P%move_down() ! y=5 landed (frozen) on bottom
      call P%move_down() ! y=5 landed (frozen) on bottom
  if(.not.P%landed) call err('(obj before rotation)  failed to land')
  y = P%y
  call P%move_down() ! y=5 pressing on bottom
  if(.not.(y==P%y)) call err('obj passed through floor!')
  y = P%y
end subroutine block_hit

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


subroutine err(msg)
  character(*),intent(in) :: msg

  write(stderr,*) msg

  stop -1

end subroutine err

end program
