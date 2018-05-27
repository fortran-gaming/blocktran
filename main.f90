program tetran
  use cinter, only:  initscr,getch,noecho,flushinp,mvprintw,addch, &
    mvaddch,clear,timeout,usleep,cbreak, &
    maxH=>LINES, maxW=>COLS
  use blocks
  use keys, only: handle_input
  use, intrinsic:: iso_c_binding, only: c_int,c_ptr
  use, intrinsic:: iso_fortran_env, only: error_unit, input_unit
  implicit none

  type(c_ptr) :: stdscr

  integer :: next_disp_x
  integer, parameter :: next_disp_y = 5
  integer :: next_disp_rotation = 0

  ! Microseconds between each automatic downward move
  real :: move_time = 0.5 ! seconds
  integer(c_int), parameter :: sleep_incr = 5e4 !  keyboard polling and screen refresh interval (microseconds).  
  ! 1e6 microsec: mushy controls. 1e5 microsec a little laggy. 5e4 about right. 1e4 microsec screen flicker.
  integer :: toc, tic,trate  ! elapsed time

  integer, parameter :: lines_per_level = 10 ! how many lines to clear to advance to next level
  real, parameter :: difficulty_increase = 1.2 ! factor by which level jumps difficulty

  logical :: moved=.false., landed=.false.
  real :: difficulty_factor=1.


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

    call handle_input(moved,landed,cur_y,next_type)  ! was a key pressed?
    if (landed) call piece_hit()
    if (moved) call redraw()
    
    call system_clock(count=toc)
    
    if(debug) print *,toc-tic, toc, tic  ! in lower right corder of screen
    


    if ( (toc-tic) / real(trate) > move_time) then ! time's up, move piece one step down. real(trate) is necessary for float time comparison!
      if(move_down()) call piece_hit()
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
  

end program
