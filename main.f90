program add
  use cinter
  use blocks
  use iso_fortran_env, only : error_unit
  implicit none

  logical :: debug=.false.
  integer, parameter :: Tmax = 10000 ! maximum number of pieces to log

  integer, parameter :: H=20, W=10
  ! 0 for blank, 1 for block
  integer :: screen(H, W)

  ! Current x/y of the falling piece
  integer :: cur_x, cur_y

  ! Type of falling piece
  ! 0: Line, 1: Square, 2: T, 3: S, 4: Z, 5: J, 6: L
  integer :: cur_type, next_type

  ! Rotation of falling piece
  integer :: cur_rotation = 0

  ! Current score
  integer :: score = 0, Nblock=0 ! first go around draws two blocks
  character(1) :: blockseq(Tmax)  ! record of blocks player experienced
  ! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns

  integer, parameter :: next_disp_x = 15, next_disp_y = 5
  integer :: next_disp_rotation = 0

  integer, parameter :: Ntypes = 7

  ! Microseconds between each automatic downward move
  integer :: move_time = 500000 ! 0.5 sec. http://www.colinfahey.com/tetris/tetris.html
  integer, parameter :: sleep_incr = 10000 ! 10 ms
  integer :: tcount = 0

  integer :: u
  integer :: difficulty_factor=1


  call cmd_parse()

  print *,'piece update time (ms)', move_time/1000
!------- initialize
  call initscr()
  call noecho()
  call cbreak()
  call timeout(0)

  screen(:,:) = 0
  blockseq(:) = ""

  call init_random_seed()

  call generate_next_type()
  call spawn_block()
  Nblock = Nblock-1   ! offset creation of first block
!--------- main loop
  do
    call clear()
    call draw_screen()
    ! Draw the falling block
    call draw_piece(cur_x, cur_y, cur_type, cur_rotation)
    ! Draw next block
    call draw_piece(next_disp_x, next_disp_y, next_type, next_disp_rotation)

    call draw_score()
    call handle_input()

    if (tcount > move_time) then
      call move_down()
      tcount = 0
    end if

    call usleep(sleep_incr)
    tcount = tcount + sleep_incr
  end do

contains

  subroutine cmd_parse()
    integer :: i,argc
    character(*),parameter :: logfn='tetran.log'
    character(16) :: arg
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone

!------- argv positional
    argc = command_argument_count()
    if (argc>0) then
      call get_command_argument(1,arg)
      read(arg,*, err=9) difficulty_factor
      if ((difficulty_factor>1).or.(difficulty_factor<100)) then
        move_time = move_time / difficulty_factor
      endif
9        endif  ! flag instead of value
!-------- argv flags
    do i = 1,argc
      call get_command_argument(i,arg)
      select case (arg)
        case ('-d','--debug','-v','--verbose')
          debug=.true.
          print *,'debug enabled, writing to ', logfn
          open(newunit=u,file=logfn, action='Write', &
               form='formatted', status='unknown',   &
               position='append')

          call date_and_time(date,time,zone)
          write(u,*) '--------------------------------------------'
          write(u,*) 'start: ', date,'T', time, zone
        case default
          write(error_unit,*) 'unknown command line option: ',arg
      end select
    enddo
  end subroutine cmd_parse

  subroutine init_random_seed()
    integer :: i, n, clock
    integer, allocatable :: seed(:)

    call random_seed(size=n)
    allocate(seed(n))
    call system_clock(count=clock)

    do concurrent (i=1:n)
      seed(i) = clock + 37 * (i-1)
    enddo

    call random_seed(put=seed)

    if (debug) then
      call random_seed(get=seed)
      write(u,*) 'seed:',seed
      write(u,*) 'Lines to clear                                 Counter'
    endif
  end subroutine

  subroutine err(msg)
      character(len=*),intent(in) :: msg
      call endwin()
      write(error_unit,*) msg
      if (debug) then
        write(u,*) msg
        close(u)
      endif

      error stop 'abnormal TETRAN termination'
  end subroutine err

  subroutine game_over()
      call endwin()

      Print *, 'Score:', score
      print *, 'Number of Blocks:',Nblock
      print *, 'Block Sequence: ',blockseq(:Nblock)

      if (debug) then
        write(u,*) 'Block Sequence: ',blockseq(:Nblock)
        close(u)
      endif

      stop 'Goodbye from Tetran'

  end subroutine game_over

  subroutine draw_screen()
    integer :: i, j

    do i = 1, H
      do j = 1, W
        if (screen(i, j) == 1) then
          call addch('@')
        else
          call addch('.')  ! background selection  (some like '.')
        end if
      end do
      call addch(NEW_LINE(' '))
    end do
  end subroutine draw_screen

  subroutine draw_score()
    character(len=16) :: msg = ""

    write (msg, "(I10)") score
    call mvprintw(H, 0, msg)

    write (msg, "(I10)") Nblock
    call mvprintw(H, W, msg)
  end subroutine draw_score

  subroutine handle_input()
    integer :: inp_chr

    inp_chr = getch()

    select case (inp_chr)
      ! A - left
      case (97)
        call move_left()
      ! S - down
      case (115)
        call move_down()
      ! D - right
      case (100)
        call move_right()
      ! W - rotate
      case (119)
        call rotate_piece()
      ! Q - quit
      case (113)
        call game_over()
    end select
  end subroutine handle_input

  subroutine move_left()
    integer :: x
    x = cur_x - 1
    if (.not. check_collision(x, cur_y, cur_rotation)) then
      cur_x = cur_x - 1
    end if
  end subroutine move_left

  subroutine move_right()
    integer :: x
    x = cur_x + 1
    if (.not. check_collision(x, cur_y, cur_rotation)) then
      cur_x = cur_x + 1
    end if
  end subroutine move_right

  subroutine move_down()
    integer :: y
    y = cur_y + 1
    if (.not. check_collision(cur_x, y, cur_rotation)) then
      cur_y = cur_y + 1
    else
      call piece_hit()
    end if
  end subroutine move_down

  subroutine rotate_piece()
    integer :: rotation
    rotation = cur_rotation + 1
    if (.not. check_collision(cur_x, cur_y, rotation)) then
      cur_rotation = cur_rotation + 1
    end if
  end subroutine rotate_piece

  logical function check_collision(x, y, rotation) result (collided)
    integer, intent(in) :: x, y
    integer, intent(inout) :: rotation
    integer :: block(4,4)
    integer :: i, j, jx, iy

    collided = .false.
    call get_shape(cur_type, rotation, block)

    iloop: do i = 1, 4
      iy = i + y - 2
      do j = 1, 4
        jx = j + x - 2
        if (block(i, j) == 1) then
          ! Handling left/right boundaries
          if (jx < 0 .or. jx >= W) then
            collided = .true.
            exit iloop
          end if

          ! Floor
          if (iy >= H) then
            collided = .true.
            exit iloop
          end if

          ! Other blocks
          if (iy > 0 .and. iy < H) then
            if (screen(iy + 1, jx + 1) == 1) then
              collided = .true.
              exit iloop
            end if
          end if
        end if
      end do
    end do iloop
  end function check_collision

  subroutine draw_piece(offset_x, offset_y, piece_type, piece_rotation)
    integer, intent(in) :: offset_x, offset_y, piece_type
    integer, intent(inout) :: piece_rotation
    integer :: block(4,4)
    integer :: i, j, x, y

    call get_shape(piece_type, piece_rotation, block)

    do i = 1, 4
      y = i + offset_y - 2
      do j = 1, 4
        x = j + offset_x - 2
        if (y >= 0 .and. block(i, j) == 1) call mvaddch(y, x, '#')
      end do
    end do
  end subroutine draw_piece

  ! Called when a piece has hit another and is solidifying
  subroutine piece_hit()
    integer :: block(4,4)
    integer :: i, j, x, y

    call get_shape(cur_type, cur_rotation, block)

    do i = 1, 4
      y = i + cur_y - 1
      do j = 1, 4
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

  subroutine generate_next_type()
    real :: r
    Nblock = Nblock + 1
    call random_number(r)
    next_type = floor(r * Ntypes)  ! set this line constant for debug shapes
  end subroutine generate_next_type

  subroutine spawn_block()
    integer :: ib
    cur_x = 4
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

    select case (cur_type)
      case (0)
        blockseq(ib) = "I"
      case(1)
        blockseq(ib) = "T"
      case(2)
        blockseq(ib)= "L"
      case(3)
        blockseq(ib)= "J"
      case(4)
        blockseq(ib)= "S"
      case(5)
        blockseq(ib)= "Z"
      case(6)
        blockseq(ib)= "B"
      case default
        call err('impossible block type')
    end select
  ! ------ end logging

    call generate_next_type()
  end subroutine spawn_block

  subroutine handle_clearing_lines()
    logical :: lines_to_clear(H)
    integer :: i, counter

    lines_to_clear = all(screen==1,2) ! mask of lines that need clearing
    counter = count(lines_to_clear)   ! how many lines are cleared
    if (debug) write(u,*) lines_to_clear, counter

    select case (counter)
      case (0)
      case (1)
        score = score + 40
      case (2)
        score = score + 100
      case (3)
        score = score + 300
      case (4)
        score = score + 1200
      case default
        call err('impossible count of cleared lines')
    end select

    do i = 1, H
      if (lines_to_clear(i)) then
        screen(i,:) = 0 ! wipe away cleared lines
        screen(:i, :) = cshift(screen(:i, :), shift=-1, dim=1)
      endif
      ! Bring everything down
    end do
  end subroutine handle_clearing_lines
end program add
