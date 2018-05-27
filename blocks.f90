module blocks
  use, intrinsic:: iso_fortran_env, only: error_unit
  use cinter, only: err
  implicit none

    integer, parameter :: Ntypes = 7
  
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

  integer, parameter :: Ny=size(square,1), Nx=size(square,2)

contains

  ! Mutates rotation
  subroutine get_shape(block_type, rotation, bshape)
  ! FIXME: make object oriented
    integer, intent(out) :: bshape(Ny, Nx)
    integer, intent(inout) :: rotation
    character, intent(in) :: block_type
    character(80) :: errmsg


    select case (block_type)
      case ("I")
        rotation = modulo(rotation, size(line,3))
        bshape = line(:,:,rotation)
      case ("T")
        rotation = modulo(rotation, size(tee,3))
        bshape = tee(:,:,rotation)
      case ("L")
        rotation = modulo(rotation, size(ell,3))
        bshape = ell(:,:,rotation)
      case ("J")
        rotation = modulo(rotation, size(jay,3))
        bshape = jay(:,:,rotation)
      case ("S")
        rotation = modulo(rotation, size(ess,3))
        bshape = ess(:,:,rotation)
      case ("Z")
        rotation = modulo(rotation, size(zee,3))
        bshape = zee(:,:,rotation)
      case ("B")
        bshape = square
        rotation = 0
      case default
        write(errmsg,*) 'unknown shape index: ',block_type
        call err(errmsg)
    end select
  end subroutine get_shape
  
 
  impure elemental subroutine generate_next_type(next_type, Nblock)
    character, intent(out) :: next_type
    integer, intent(inout), optional :: Nblock
    real :: r

    if(present(Nblock)) Nblock = Nblock + 1  ! for game stats

    call random_number(r)

    next_type = int2block(floor(r * Ntypes))  ! set this line constant to debug shapes
  end subroutine generate_next_type
  
  
  impure elemental character function int2block(i) result(b)
    integer, intent(in) :: i

    select case (i)
      case (0)
        b = "I"
      case(1)
        b = "T"
      case(2)
        b = "L"
      case(3)
        b = "J"
      case(4)
        b = "S"
      case(5)
        b = "Z"
      case(6)
        b = "B"
      case default
        call err('impossible block type')
    end select

  end function int2block
  
  
  subroutine init_random_seed(debug)
    ! NOTE: this subroutine is replaced by "call random_init()" in Fortran 2018
    logical, intent(in), optional :: debug
    integer :: n, u,ios
    integer, allocatable :: seed(:)
    logical :: dbg
    
    character(*), parameter :: randfn = '/dev/urandom'
    
    dbg = .false.
    if (present(debug)) dbg=debug
    
    call random_seed(size=n)
    allocate(seed(n))
    
    open(newunit=u, file=randfn, access="stream", &
                 form="unformatted", action="read", status="old", iostat=ios)
    if (ios/=0) call err('failed to open random source generator file: '//randfn)
    
    read(u,iostat=ios) seed
    if (ios/=0) call err('failed to read random source generator file: '//randfn)
    
    close(u)
    
    call random_seed(put=seed)

    if (dbg) then
      call random_seed(get=seed)
      print *, 'seed:',seed
    endif
  end subroutine

end module blocks
