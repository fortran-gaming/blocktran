module block_init

implicit none

public

  ! Stores the shape of the blocks at each of their rotations

  ! LINE BLOCK
  integer, parameter :: line(4,4,0:1) = reshape( &
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
  integer, parameter :: tee(4,4,0:3) = reshape( &
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
  integer, parameter :: ell(4,4,0:3) = reshape( &
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
  integer, parameter :: jay(4,4,0:3) = reshape( &
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
  integer, parameter :: ess(4,4,0:1) = reshape( &
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
  integer, parameter :: zee(4,4,0:1) = reshape( &
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
  integer, parameter :: square(4,4) = reshape( &
        [0, 1, 1, 0, &
         0, 1, 1, 0, &
         0, 0, 0, 0, &
         0, 0, 0, 0], &
         shape(square))


end module block_init
