module blocks
  use, intrinsic:: iso_fortran_env, only: error_unit
  
  use cinter, only: err,endwin,printopts
  use block_init, only: line,tee,jay,ess,ell,zee,square
  
  implicit none
  private :: check_collision, line, tee, jay, ess, ell, zee, square
  
    logical :: debug=.false.
    integer :: udbg

  
    integer, parameter :: Tmax = 10000 ! maximum number of pieces to log
    integer :: Ncleared = 0 ! total number of lines cleared
    integer, parameter :: bonus(0:4) = [0,40,100,300,1200]
    
    integer :: level=1  ! game level, increases difficulty over time

  
    integer :: H,W  ! playfield height, width
    ! Playfield: 0 for blank, 1 for block
    integer, allocatable :: screen(:,:)

    character(*), parameter :: Btypes = 'ITLJSZB'
    
    ! Current x/y of the falling piece
    integer :: cur_x, cur_y
 
    ! Rotation of falling piece
    integer :: cur_rotation = 0
  
    ! Type of falling piece
    ! 0/I: Line, 1/B: Square, 2: T, 3: S, 4: Z, 5: J, 6: L
    character :: cur_type, next_type
    
    ! Current score
    integer :: score = 0, Nblock=0 ! first go around draws two blocks
    character(1) :: blockseq(Tmax) = "" ! record of blocks player experienced
    ! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns
    
    
    logical :: newhit = .false.
  
  public

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
    integer :: i

    if(present(Nblock)) Nblock = Nblock + 1  ! for game stats

    call random_number(r)

    i = floor(r * len(Btypes))  ! set this line constant to debug shapes
    
    next_type = Btypes(i+1:i+1)
  end subroutine generate_next_type
  
  
  subroutine move_left()
    integer :: x
    x = cur_x - 1
    if (.not. check_collision(x, cur_y, cur_rotation)) cur_x = cur_x - 1
  end subroutine move_left


  subroutine move_right()
    integer :: x
    x = cur_x + 1
    if (.not. check_collision(x, cur_y, cur_rotation)) cur_x = cur_x + 1
  end subroutine move_right


  logical function move_down() result (landed)
    integer :: y
    
    y = cur_y + 1
    if (.not. check_collision(cur_x, y, cur_rotation)) then
      cur_y = cur_y + 1
      landed = .false.
    else
      landed = .true.
    end if
  end function move_down


  subroutine rotate_piece()
    integer :: r
    r = cur_rotation + 1
    if (.not. check_collision(cur_x, cur_y, r)) cur_rotation = cur_rotation + 1
  end subroutine rotate_piece
  
  
  logical function check_collision(x, y, rotation) result (collided)
    integer, intent(in) :: x, y
    integer, intent(inout) :: rotation

    integer :: block(Ny, Nx)
    integer :: i, j, jx, iy

    collided = .false.
    call get_shape(cur_type, rotation, block)

! neither do loop is "concurrent" because of "exit" statements
    iloop: do i = 1, Ny
      iy = i + y - 2
      
      if (any(block(i,:) == 1) .and. iy >= H) then
      ! piece hit the floor
        collided = .true.
        return
      end if
      
      do j = 1, Nx
        jx = j + x - 2
        if (block(i, j) == 1) then
          ! Handling left/right boundaries
          if (jx < 0 .or. jx >= W) then
            collided = .true.
            return
          end if

          ! Other blocks
          if (iy > 0 .and. iy < H) then
            if (screen(iy + 1, jx + 1) == 1) then
              collided = .true.
              return
            end if
          end if
        end if
      end do
    end do iloop
  end function check_collision


 subroutine piece_hit()
  ! Called when a piece has hit another and is solidifying
    integer :: block(Ny,Nx)
    integer :: i, j, x, y

    call get_shape(cur_type, cur_rotation, block)

! not concurrent due to impure "game_over"
    do i = 1, Ny
      y = i + cur_y - 1
      do j = 1, Nx
        x = j + cur_x - 1
        if (block(i, j) == 1) then
          if (y <= 1)  call game_over()
          screen(y, x) = 1
        end if
      end do
    end do

    call handle_clearing_lines()
    call spawn_block()
  end subroutine piece_hit


  subroutine spawn_block()
    integer :: ib
    real :: r

    call random_number(r)
    cur_x = nint(r*(W-Nx) + Nx/2)
    cur_y = -1
    cur_type = next_type
    cur_rotation = 0
 ! ----- logging ---------
    if (Nblock>Tmax) then
      ib = Tmax
      blockseq = eoshift(blockseq,1)  !OK array-temp
    else
      ib = Nblock
    endif

    blockseq(ib) = cur_type
  ! ------ end logging

    call generate_next_type(next_type, Nblock)
  end subroutine spawn_block


  subroutine handle_clearing_lines()
    logical :: lines_to_clear(H)
    integer :: i, counter

    lines_to_clear = all(screen==1,2) ! mask of lines that need clearing
    counter = count(lines_to_clear)   ! how many lines are cleared
    if (counter == 0) return

    Ncleared = Ncleared + counter
    if (debug) write(udbg,*) lines_to_clear, counter

    score = score + bonus(counter)
! not concurrent since it could clear lines above shifted by other concurrent iterations
! i.e. in some cases, it would check an OK line that turns bad after clearing by another elemental iteration.
! also note non-adjacent lines can be cleared at once.
    do i = 1, H
      if (.not.lines_to_clear(i)) cycle
      newhit = .true.
      screen(i,:) = 0 ! wipe away cleared lines
      screen(:i, :) = cshift(screen(:i, :), shift=-1, dim=1)
      ! Bring everything down
    end do
  end subroutine handle_clearing_lines
  
  
  subroutine game_over()
    call endwin()
    
    call printopts()

    print *,''
    print *, 'Level:', level
    Print *, 'Score:', score
    print *, 'Number of Blocks:',Nblock
    print *, 'Number of Lines Cleared:',Ncleared
    print *, 'Block Sequence: ',blockseq(:Nblock)

    if (debug) then
      write(udbg,*) 'Block Sequence: ',blockseq(:Nblock)
      close(udbg)
    endif
    

    stop 'Goodbye from Tetran'

  end subroutine game_over
  
  
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
