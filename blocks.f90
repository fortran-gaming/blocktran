module blocks
  implicit none
  public
  ! Stores the shape of the blocks at each of their rotations
  ! http://tetris.wikia.com/wiki/ORS

  ! LINE BLOCK
  integer, parameter :: line(4,4,2) = reshape( &
      (/ 0, 0, 0, 0, &
         1, 1, 1, 1, &
         0, 0, 0, 0, &
         0, 0, 0, 0, &
         0, 0, 1, 0, &
         0, 0, 1, 0, &
         0, 0, 1, 0, &
         0, 0, 1, 0 /), &
         shape(line))

  ! T BLOCK
  integer, parameter :: tee(4,4,4) = reshape( &
      (/ 0, 0, 0, 0, &
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
         0, 0, 0, 0 /), &
         shape(tee))

  ! L BLOCK
  integer, parameter :: ell(4,4,4) = reshape( &
      (/ 0, 0, 0, 0, &
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
         0, 0, 0, 0 /), &
         shape(ell))

  ! J BLOCK
  integer, parameter :: jay(4,4,4) = reshape( &
      (/ 0, 0, 0, 0, &
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
         0, 0, 0, 0 /), &
         shape(jay))

  ! S BLOCK
  integer, parameter :: ess(4,4,2) = reshape( &
      (/ 0, 0, 0, 0, &
         0, 1, 1, 0, &
         1, 1, 0, 0, &
         0, 0, 0, 0, &
         1, 0, 0, 0, &
         1, 1, 0, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0 /), &
         shape(ess))

  ! Z BLOCK
  integer, parameter :: zee(4,4,2) = reshape( &
      (/ 0, 0, 0, 0, &
         1, 1, 0, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0, &
         0, 0, 1, 0, &
         0, 1, 1, 0, &
         0, 1, 0, 0, &
         0, 0, 0, 0 /), &
         shape(zee))

  ! SQUARE BLOCK
  integer, parameter :: square(4,4) = reshape( &
      (/ 0, 1, 1, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0, &
         0, 0, 0, 0 /), &
         shape(square))

  contains

    ! Mutates rotation
    subroutine get_shape(block_type, rotation, bshape)
       
      integer, intent(out) :: bshape(4,4)
      integer, intent(inout) :: rotation
      integer, intent(in) :: block_type


      select case (block_type)
        case (0)
          bshape = line(:,:,rotation+1)
          rotation = modulo(rotation, 2) ! pass by reference (intent(inout)) is convenient
        case (1)
          bshape = tee(:,:,rotation+1)
          rotation = modulo(rotation, 4)
        case (2)
          bshape = ell(:,:,rotation+1)
          rotation = modulo(rotation, 4)
        case (3)
          bshape = jay(:,:,rotation+1)
          rotation = modulo(rotation, 4)
        case (4)
          bshape = ess(:,:,rotation+1)
          rotation = modulo(rotation, 2)
        case (5)
          bshape = zee(:,:,rotation+1)
          rotation = modulo(rotation, 2)
        case (6)
          bshape = square
          rotation = 0
        case default
          call err('unknown shape called')
      end select
    end subroutine get_shape

end module blocks
