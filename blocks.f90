module blocks
  use, intrinsic:: iso_fortran_env, only: error_unit
  use cinter, only: err
  implicit none
  public
  ! Stores the shape of the blocks at each of their rotations
  ! http://tetris.wikia.com/wiki/ORS

  ! LINE BLOCK
  integer, parameter :: line(4,4,0:1) = reshape( &
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
  integer, parameter :: tee(4,4,0:3) = reshape( &
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
  integer, parameter :: ell(4,4,0:3) = reshape( &
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
  integer, parameter :: jay(4,4,0:3) = reshape( &
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
  integer, parameter :: ess(4,4,0:1) = reshape( &
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
  integer, parameter :: zee(4,4,0:1) = reshape( &
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

  integer, parameter :: Ny=size(square,1), Nx=size(square,2)

contains

  ! Mutates rotation
  subroutine get_shape(block_type, rotation, bshape)
  ! FIXME: make object oriented
    integer, intent(out) :: bshape(Ny, Nx)
    integer, intent(inout) :: rotation
    integer, intent(in) :: block_type
    character(80) :: errmsg


    select case (block_type)
      case (0)
        rotation = modulo(rotation, size(line,3))
        bshape = line(:,:,rotation)
      case (1)
        rotation = modulo(rotation, size(tee,3))
        bshape = tee(:,:,rotation)
      case (2)
        rotation = modulo(rotation, size(ell,3))
        bshape = ell(:,:,rotation)
      case (3)
        rotation = modulo(rotation, size(jay,3))
        bshape = jay(:,:,rotation)
      case (4)
        rotation = modulo(rotation, size(ess,3))
        bshape = ess(:,:,rotation)
      case (5)
        rotation = modulo(rotation, size(zee,3))
        bshape = zee(:,:,rotation)
      case (6)
        bshape = square
        rotation = 0
      case default
        write(errmsg,*) 'unknown shape index: ',block_type
        call err(errmsg)
    end select
  end subroutine get_shape

end module blocks
