module AI

use fields, only: field
use shapes, only: piece

implicit none

contains

subroutine AI_input(F,P)
  class(field), intent(in) :: F
  class(piece), intent(inout) :: P
  
! check if easy case--clear to bottom (FIXME: predict outcome) 
  if (project_down(F,P) >= F%H-P%Ny) then
    call down_random(P)
    return
  endif
! harder cases
  call seek_deep(F,P)
  
end subroutine AI_input


integer function project_down(F,P) result(y)
! returns freezing Y position for this ONE piece X position
  class(field), intent(in) :: F
  class(piece), intent(in) :: P
  type(piece) :: T
  integer :: i, j,h(4)

  ! create test piece
  call T%init(F, P%btype, x=P%x, y=P%y)
  ! see how low the piece can go for this X position
  y = F%H
  h = 1
  rot: do i = 1, size(h)
    call T%rotate()
    
    hght: do j = 1,F%H
      T%y = j
      if (T%check_collision()) then
        h(i) = j - 1
        exit hght
      endif  
    enddo hght
    
  enddo rot
  
  where (h==1) h = 9999
  y = min(y, maxval(h))
  
end function project_down


subroutine seek_deep(F, P)
  class(field), intent(in) :: F
  class(piece), intent(inout) :: P
  integer :: k(1), x_center, x_seek
  real :: r
  real, parameter :: dumbness = 0.  ! 0..1  lower number, less dumb

!-- maybe do something random instead of smart
  call random_number(r)
  if (r < dumbness) then ! act randomly instead
    call move_random(P)
    return
  endif
  
!-- compute smarter action 
  x_center = P%x - P%Nx/2   ! FIXME: approximately, what about rotated "I"

  ! for now, seek the "deepest" place
  k = maxloc(height(F, P))
  x_seek = k(1)
  
!--- take action
  if (x_seek <= x_center) then 
    call P%move_left()
  elseif (x_seek >= x_center) then
    call P%move_right()
  endif
  
end subroutine seek_deep


subroutine down_random(P)
  class(piece), intent(inout) :: P
  real :: r
  integer :: i
  
  call random_number(r)
  
  if (r >= 0 .and. r < 0.25) then
  
    call random_number(r)
    do i = 1,floor(2*r)
      call P%move_down()
    enddo
    
  elseif (r >= 0.25 .and. r < 0.3) then
  
    call P%move_down(slam=.true.)
    
  endif

end subroutine down_random


subroutine move_random(P)
  class(piece), intent(inout) :: P
  real :: r
  integer :: i
  real, parameter :: patience = 0.9  ! 0..1 if > patience, move. 
  
  call random_number(r)
  if (r < patience) return
  
  call random_number(r)
  
  if (r < 0.33) then
    call P%move_left()
  elseif (r >= 0.33 .and. r < 0.66) then
    call P%move_right()
  else
    call P%rotate()
    
    call random_number(r)
    if (r >=0 .and. r < 0.25) then
      do i = 1,floor(8*r)
        call P%move_left()
      enddo
    elseif (r >= 0.25 .and. r < 0.5) then
      do i = 1,floor(8*r)
        call P%move_right()
      enddo
    endif
  endif

end subroutine move_random


integer function height(F, P)
! "Height" in tetran is distance from top of screen -- smaller is higher
! checks how well piece fits 
  class(field), intent(in) :: F
  class(piece), intent(in) :: P
  dimension :: height(P%W)
  integer :: i
  type(piece) :: T
  
  ! create test piece
  call T%init(F, P%btype, x=P%x, y=P%y)
  
  do i = 1,F%W
    T%x = i
    height(i) = project_down(F, T)
  enddo
  
  where (height==1) height = 9999
  
end function height


pure integer function diff(A)
! first difference
  integer, intent(in) :: A(:)
  dimension :: diff(size(A))
  
  diff = A(2:) - A(:size(A)-1)
end function diff


end module AI
