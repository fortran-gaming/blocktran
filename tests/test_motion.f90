program motiontest
! -------- shape horiz. movement & rotation
! FIXME: Each shape should be tested.
use, intrinsic:: iso_fortran_env, only: stderr=>error_unit
use shapes, only: piece

implicit none

type(piece) :: line!,tee,ell,jay,ess,zee,sqr

integer, parameter :: W=8,H=10

call initial(line)
call test_floor(line)
call left_wall(line)
call right_wall(line)
call block_hit(line)

print *,'OK motion'

contains

subroutine initial(P)
  class(piece), intent(inout) :: P
  
  print *,'intiial position test...'
  
  call P%init("I",W,H,W/2)
  P%debug=.false.
  if(.not.(P%y==-1)) call err('initial Y position')
  if(.not.(P%rot==0)) call err('initial rotation')
  if(P%landed) call err('initial landed')
  call print_piece(line)
end subroutine initial

subroutine test_floor(P)
  class(piece), intent(inout) :: P
  integer :: screen(H,W)  ! playfield`
  integer :: y ! test positions
  
  print *, 'floor hit test...'

  screen = 0

  call P%init("I",W,H,W/2)
  P%debug=.false.
  call P%move_down(screen) ! y=0
  y = P%y
  call P%move_down(screen) ! y=1
  if(.not.(y==P%y-1) .and.P%y==1) call err('failure to move down')
  call P%move_down(screen) ! y=2
  call P%move_down(screen) ! y=3
  call P%move_down(screen) ! y=4
  call P%move_down(screen) ! y=5
  call P%move_down(screen) ! y=6
  call P%move_down(screen) ! y=7 next to bottom (allow sliding along floor before final freeze)
  if(P%landed) then
    ! print *,'x,y', P%x, P%y
    call err('(before rotation) failing to slide along floor')
  endif
  call P%move_down(screen) ! y=7 landed (frozen) on bottom
  if(.not.P%landed) call err('(before rotation)  failed to land')
  y = P%y
  call P%move_down(screen) ! y=7 pressing on bottom
  if(.not.(y==P%y)) call err('passed through floor!')
  y = P%y

  P%y=P%y-1; P%landed = .false. ! y=5 manually unfreeze
  call P%rotate(screen)
  if(.not.(P%rot==1)) call err('failed to rotate after vertical reset')
  if(P%landed) call err('(after rotation) early landing')
  !call print_piece(line)
  call P%move_down(screen) ! y=6
  call P%move_down(screen) ! y=7 next to bottom (allow sliding along floor before final freeze)
  if(P%landed) then
    ! print *,'x,y', P%x, P%y
    call err('(after rotation and movedown) failing to slide along floor')
  endif
  call P%move_down(screen) ! y=7 landed (frozen) on bottom
  if(.not.P%landed) call err('(after rotation) failed to land')
  y = P%y
  call P%move_down(screen) ! y=7 pressing on bottom
  if(.not.(y==P%y)) call err('passed through floor!')
  y = P%y
end subroutine test_floor


subroutine left_wall(P)
  class(piece), intent(inout) :: P
  integer :: screen(H,W)  ! playfield`
  integer :: x ! test positions

  print *,'left wall test...'
  
  screen = 0
  ! -- Left wall
  call P%init("I", W, H, W/2)
  if(.not.(P%rot==0)) call err('left wall: initial rotation')
  P%debug=.false.
  x = P%x
  call P%move_left(screen) ! x=3
  if (P%x /= x-1) call err('I move left')
  call P%move_left(screen) ! x=2
  call P%move_left(screen) ! x=1 at left wall
  call P%move_left(screen) ! x=1 pushing on left wall
  if (P%x /= 0) call err('passing through left wall!')
  x=P%x
  !call print_piece(line)
  ! -- rotate left wall
  call P%rotate(screen)
  !call print_piece(line)
  if(.not.P%rot==0) call err('(left wall) I should not be able to rotate')
  call P%move_left(screen) ! x=0 at left wall
  if (P%x /= x) call err('I move left after rotate')
  call P%move_left(screen) ! at left wall
  call P%move_left(screen) ! pushing on left wall
  if (P%x /= 0) call err('I move left collision detection')
end subroutine left_wall

subroutine right_wall(P)
  class(piece), intent(inout) :: P
  integer :: screen(H,W)  ! playfield`
  integer :: x ! test positions
  
  print *,'right wall test...'

  screen = 0
  
  call P%init("I",W,H, W/2)
  if(.not.(P%rot==0)) call err('initial rotation')
  P%debug=.false.
  x = P%x
  !call print_piece(line)
  call P%move_right(screen) ! x=5
  if (P%x /= x+1) call err('I move right')
  call P%move_right(screen) ! x=6
  call P%move_right(screen) ! x=6
  call P%move_right(screen) ! x=7 at right wall
  call P%move_right(screen) ! x=7 pushing on right wall
  if (P%x /= W-1) call err('(before rotate) I move right collision detection')
  call P%rotate(screen)
  if(.not.P%rot==0) call err('(left wall) I should not be able to rotate')
  call P%move_right(screen) ! at right wall
  call P%move_right(screen) ! pushing on right wall
  if (P%x/=W-1) call err('(after rotate) I move right collision detection')
  
  print *,'OK right bolock test.'
end subroutine right_wall


subroutine block_hit(P)
  class(piece), intent(inout) :: P
  integer :: screen(H,W)  ! playfield`
  integer :: y ! test positions
  
  print *,'block hit test...'

  screen = 0
  
  !-- single pixel object in center bottom of floor
  screen(H-2,W/2) = 1
  !call print_block(screen)
  call P%init("I",W,H,W/2)
  call P%rotate(screen)
  P%debug=.false.
  !call print_piece(line)
  call P%move_down(screen) ! y=0
  y = P%y
  call P%move_down(screen) ! y=1
  if(.not.(y==P%y-1) .and.P%y==1) call err('(obj) failure to move down')
  call P%move_down(screen) ! y=2
  call P%move_down(screen) ! y=3
  call P%move_down(screen) ! y=4
  call P%move_down(screen) ! y=5 next to bottom (allow sliding along floor before final freeze)
  if(P%landed) then
    print *,'x,y', P%x, P%y
    call err('(obj before rotation) failing to slide along object')
  endif
  call P%move_down(screen) ! y=5 landed (frozen) on bottom
  if(.not.P%landed) call err('(obj before rotation)  failed to land')
  y = P%y
  call P%move_down(screen) ! y=5 pressing on bottom
  if(.not.(y==P%y)) call err('obj passed through floor!')
  y = P%y
end subroutine block_hit

subroutine print_piece(P)
  class(piece), intent(in) :: P
  integer :: B(P%Ny,P%Nx)

  B = P%val()

  call print_block(B)
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
