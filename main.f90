program tetran
  use cinter, only:  initscr,getch,noecho,flushinp,mvprintw,addch,printopts, &
    mvaddch,endwin, clear,timeout,usleep,cbreak, &
    maxH=>LINES, maxW=>COLS
  use blocks
  use, intrinsic:: iso_c_binding, only: c_int,c_ptr
  use, intrinsic:: iso_fortran_env, only: error_unit, input_unit
  implicit none

  logical :: debug=.false.
  integer, parameter :: Tmax = 10000 ! maximum number of pieces to log

  integer :: H,W  ! playfield height, width
  ! 0 for blank, 1 for block
  integer, allocatable :: screen(:,:)
  type(c_ptr) :: stdscr

  ! Current x/y of the falling piece
  integer :: cur_x, cur_y

  ! Type of falling piece
  ! 0/I: Line, 1/B: Square, 2: T, 3: S, 4: Z, 5: J, 6: L
  character :: cur_type, next_type

  ! Rotation of falling piece
  integer :: cur_rotation = 0

  ! Current score
  integer :: score = 0, Nblock=0 ! first go around draws two blocks
  character(1) :: blockseq(Tmax) = "" ! record of blocks player experienced
  ! NOTE: uses eoshift to avoid indexing beyond array, discarding earliest turns

  integer :: next_disp_x
  integer, parameter :: next_disp_y = 5
  integer :: next_disp_rotation = 0

  ! Microseconds between each automatic downward move
  real :: move_time = 0.5 ! seconds
  integer(c_int), parameter :: sleep_incr = 5e4 !  keyboard polling and screen refresh interval (microseconds).  
  ! 1e6 microsec: mushy controls. 1e5 microsec a little laggy. 5e4 about right. 1e4 microsec screen flicker.
  integer :: toc, tic,trate  ! elapsed time

  integer :: udbg

  integer :: level=1  ! game level, increases difficulty over time
  integer, parameter :: lines_per_level = 10 ! how many lines to clear to advance to next level
  real, parameter :: difficulty_increase = 1.2 ! factor by which level jumps difficulty
  integer :: Ncleared = 0 ! total number of lines cleared
  logical :: newhit = .false., moved=.false.
  real :: difficulty_factor=1.

  integer, parameter :: bonus(0:4) = [0,40,100,300,1200]


  call cmd_parse()
  move_time = move_time / difficulty_factor

  print *,'Initial piece update time (seconds)', move_time

  call get_dim(H,W)


  allocate(screen(H,W))
  screen = 0
  next_disp_x = W + 5

!------- initialize
  stdscr = initscr()

! too big -- FIXME generate new window for game
  if (H+3 > maxH) call err('playfield height too tall for terminal window')
  if (W+10 > maxW) call err('playfield width too wide for terminal window')


  call noecho()
  call cbreak()
  call timeout(0)

  call init_random_seed(debug)
  if(debug) write(udbg,*) 'Lines to clear                                 Counter'

  call generate_next_type(next_type, Nblock)
  call spawn_block()
  Nblock = Nblock-1   ! offset creation of first block

  call redraw()
  call system_clock(count=tic, count_rate=trate)
  !--------- main loop
  do

    call handle_input()  ! was a key pressed?
    if (moved) call redraw()
    
    call system_clock(count=toc)
    
    if(debug) print *,toc-tic, toc, tic  ! in lower right corder of screen
    


    if ( (toc-tic) / real(trate) > move_time) then ! time's up, move piece one step down. real(trate) is necessary for float time comparison!
      call move_down()
      call redraw()


      if (newhit.and.modulo(Ncleared,lines_per_level)==0) then ! advance level
        newhit=.false.
        level = level + 1
        difficulty_factor = difficulty_factor*difficulty_increase
        move_time = move_time / difficulty_factor
      endif
      
      call system_clock(count=tic)
    endif

    call usleep(sleep_incr)
  end do

contains

  subroutine redraw()
  
    call clear()
    call draw_screen()
    ! Draw the falling block
    call draw_piece(cur_x, cur_y, cur_type, cur_rotation)
    ! Draw next block
    call draw_piece(next_disp_x, next_disp_y, next_type, next_disp_rotation)

    call draw_score()
  
  end subroutine redraw

  subroutine get_dim(H,W)
    
    integer, intent(out) :: H,W
    integer :: ios

    print *,'Playfield height, width?'
    read(input_unit,*,iostat=ios) H,W
     
    ! too small
    if (H<4.or.W<4.or.ios > 0) stop 'Height and width must each be at least 4'
  
  end subroutine get_dim 
 

  subroutine cmd_parse()
    ! reads flag command line arguments
    integer :: i,argc
    character(*),parameter :: logfn='tetran.log'
    character(256) :: arg
    character(8)  :: date
    character(10) :: time
    character(5)  :: zone
    logical :: lastok

    argc = command_argument_count()
       
    lastok=.false.
    do i = 1,argc
      call get_command_argument(i,arg)
      select case (arg)
      
        case ('-d','--difficulty')
          call get_command_argument(i+1,arg)
          read(arg,'(F4.1)') difficulty_factor
          if (difficulty_factor<=0) call err('difficulty must be positive')
          lastok=.true.
          
        case ('--debug','-v','--verbose')
          debug=.true.
          print *,'debug enabled, writing to ', logfn
          open(newunit=udbg,file=logfn, action='Write', &
               form='formatted', status='unknown',   &
               position='append')

          call date_and_time(date,time,zone)
          write(udbg,*) '--------------------------------------------'
          write(udbg,*) 'start: ', date,'T', time, zone
          write(udbg,*) 'Lines to clear                                 Counter'
         
        case default
          if(lastok) then
            lastok=.false.
            cycle
          endif
          
          write(error_unit,*) 'unknown command line option: ',arg
      end select
    enddo
    
  end subroutine cmd_parse


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


  subroutine draw_screen()
    integer :: i, j

! not concurrent (and not where() ) since "addch" has memory of position
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
    ! prints on line under bottom of playfield:
    !  score
    !  count of blocks played in this game
    character(16), save :: msg=""
    ! this save variable is necessary to prevent garbage on screen

    write (msg, "(I10)") score
    call mvprintw(H, 0, msg)

    write (msg, "(I10)") Nblock
    call mvprintw(H+1, 0, msg)

    write (msg, "(I2)") level
    call mvprintw(H+2, 0, msg)

    write (msg, "(I4)") Ncleared
    call mvprintw(H+2, W-4, msg)
  end subroutine draw_score


  subroutine handle_input()
    integer :: inp_chr

    inp_chr = getch()

    ! esc is first part of three part arrow key sequence
    if (inp_chr == 27) then
        inp_chr = getch()

        if (inp_chr == 91) then
         inp_chr = getch()
         if (inp_chr==65) inp_chr = 87
         if (inp_chr==68) inp_chr = 65
        endif
    endif

    moved=.true. ! rather than typing it for each case
    select case (inp_chr)
    ! yes this handles upper and lower case, for clever clogs.
      case (97,65)  ! A - left
        call move_left()
      case (115,83,66) ! S - down
        call move_down()
      case (100,68,67) ! D - right
        call move_right()
      case (119,87) ! W - rotate
        call rotate_piece()
      case (113,81) ! Q - quit
        call game_over()
      case (116,84) ! CHEAT   T - reset current piece position y to top, preserving x position
        cur_y = 0
      case default ! do nothing
        moved = .false.
    end select
    
    call flushinp()  ! clear repeating keys from stdin buffer
    
  end subroutine handle_input


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
      do j = 1, Nx
        jx = j + x - 2
        if (block(i, j) == 1) then
          ! Handling left/right boundaries
          if (jx < 0 .or. jx >= W) then
            collided = .true.
            return
          end if

          ! Floor
          if (iy >= H) then
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


  subroutine draw_piece(offset_x, offset_y, piece_type, piece_rotation)
    integer, intent(in) :: offset_x, offset_y
    character, intent(in) :: piece_type
    integer, intent(inout) :: piece_rotation
    integer :: block(Ny,Nx)
    integer :: i, j, x, y

    call get_shape(piece_type, piece_rotation, block)

! not concurrent since "mvaddch" remembers its position
    do i = 1, Ny
      y = i + offset_y - 2
      do j = 1, Nx
        x = j + offset_x - 2
        if (y >= 0 .and. block(i, j) == 1) call mvaddch(y, x, '#')
      end do
    end do
  end subroutine draw_piece


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

end program
