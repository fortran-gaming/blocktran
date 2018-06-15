program tetran
use menu, only: title
use cinter, only:  initscr,getch,noecho,flushinp,mvprintw,addch, &
  clear,timeout,usleep,cbreak, &
  maxH=>LINES, maxW=>COLS
use errs, only: err
use blocks, only: Field,Piece, spawn_block, freeze, &
                  newhit, generate_next_type, draw_piece
use rand, only: random_init
use keys, only: handle_input
use, intrinsic:: iso_c_binding, only: c_int,c_ptr
use, intrinsic:: iso_fortran_env, only: error_unit, input_unit
implicit none

type(c_ptr) :: stdscr

integer :: toc, tic,trate  ! elapsed time

real :: difffact
integer :: W, H
logical :: debug

character :: next

type(piece) :: P, NP
type(field) :: F
!==========================================
call random_init()

call cmd_parse(W=W, H=H, difffact=difffact, debug=debug)

call F%init_(W=W, H=H, difffact=difffact, debug=debug)

!--- initialize Curses
stdscr = initscr()

! too big -- FIXME generate new window for game
if (H+3 > maxH) call err('playfield height too tall for terminal window')
if (W+10 > maxW) call err('playfield width too wide for terminal window')

call noecho()
call cbreak()
call timeout(0)

!--- show title screen
call title()
!--- create first block
call generate_next_type(next)
call NP%init(F, next)
call spawn_block(F, P, NP)

call redraw()
call system_clock(count=tic, count_rate=trate)
!--------- main loop
do

  call handle_input(F, P, NP)  ! was a key pressed?
  call freeze(F, P, NP)
  if (P%movereq) call redraw()

  call system_clock(count=toc)

!    if(debug) print *,toc-tic, toc, tic  ! in lower right corder of screen

  if ( (toc-tic) / real(trate) > F%move_time) then ! time's up, move piece one step down. real(trate) is necessary for float time comparison!
    call P%move_down()
    call redraw()

    if (newhit.and.modulo(F%Ncleared, F%lines_per_level)==0) then ! advance level
      newhit=.false.
      call F%levelup()
    endif

    call system_clock(count=tic)
  endif

  call usleep(F%sleep_incr)
end do

contains

subroutine redraw()

  call clear()
  call draw_screen(F)
  ! Draw the falling block
  call draw_piece(P)
  ! Draw next block
  call draw_piece(NP)

  call draw_score(F)

end subroutine redraw


subroutine cmd_parse(W, H, difffact, debug)
  ! reads flag command line arguments
  integer, intent(out) :: W, H
  real, intent(out) :: difffact
  logical, intent(out) :: debug
  integer :: i,argc
  character(*),parameter :: logfn='tetran.log'
  character(256) :: arg
  character(8)  :: date
  character(10) :: time
  character(5)  :: zone
  real :: r
! --- default values
  call random_number(r)
  W = 8 + int(r*15)
  call random_number(r)
  H = 6 + int(r*14)

  difffact = 1.
  debug = .false.
! --- process options
  argc = command_argument_count()

  do i = 1,argc
    call get_command_argument(i,arg)
    select case (arg)

      case ('-s','--size')  ! set playfield size
        call get_command_argument(i+1,arg)
        read(arg,'(I3)') W
        call get_command_argument(i+2,arg)
        read(arg,'(I3)') H
        if (H<4 .or. W<4) call err('Height and width must each be at least 4')

      case ('-d','--difficulty')
        call get_command_argument(i+1,arg)
        read(arg,'(F4.1)') difffact
        if (difffact<=0) call err('difficulty must be > 0')

      case ('--debug','-v','--verbose')
        debug=.true.
        print *,'debug enabled, writing to ', logfn
        open(newunit=F%udbg,file=logfn, action='Write', &
             form='formatted', status='unknown',   &
             position='append')

        call date_and_time(date,time,zone)
        write(F%udbg,*) '--------------------------------------------'
        write(F%udbg,*) 'start: ', date,'T', time, zone
        write(F%udbg,*) 'Lines to clear                                 Counter'

    end select
  enddo

end subroutine cmd_parse


subroutine draw_screen(F)
  class(field), intent(in) :: F
  integer :: i, j

! not concurrent (and not where() ) since "addch" has memory of position
  do i = 1, F%H
    do j = 1, F%W
      select case (F%screen(i, j))
        case (1)
          call addch('@')  ! frozen piece
        case (0)
          call addch('.')  ! background
        case default
          call err('unknown screen state')
      end select
    end do
    call addch(new_line(' '))
  end do
end subroutine draw_screen


subroutine draw_score(F)
  class(field), intent(in) :: F
  ! prints on line under bottom of playfield:
  !  score
  !  count of blocks played in this game
  character(16), save :: msg=""
  ! this save variable is necessary to prevent garbage on screen

  write (msg, "(I10)") F%score
  call mvprintw(H, 0, msg)

  write (msg, "(I10)") F%Nblock
  call mvprintw(H+1, 0, msg)

  write (msg, "(I2)") F%level
  call mvprintw(H+2, 0, msg)

  write (msg, "(I4)") F%Ncleared
  call mvprintw(H+2, W-4, msg)
end subroutine draw_score


end program
