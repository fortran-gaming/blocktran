module game

use, intrinsic:: iso_c_binding, only: c_ptr, c_bool, c_int

use menu, only: title
use cinter, only:  initscr,noecho, mvprintw, mvaddch, &
  clear,timeout, cbreak, keypad, endwin, refresh, &
  maxH=>LINES, maxW=>COLS
use sleep_std, only : sleep
use errs, only: err
use blocks, only: freeze, draw_piece
use shapes, only: piece
use fields, only: field
use random, only: randint
use keys, only: key_input

implicit none (type, external)

type(field) :: F(2)
integer :: trate  !< elapsed time
integer :: players
logical :: update = .false.
logical :: debug = .false.
real :: difffact

contains

subroutine main_game(W, H)

integer, intent(in) :: H, W

type(c_ptr) :: stdscr

type(piece) :: P(2), NP(2)

integer(c_int) :: ierr
integer :: x0(2)
integer :: i

logical,parameter :: isAI(2) = [.false., .true.]
logical(c_bool), parameter :: true = .true.

!> get tick rae
call system_clock(count_rate=trate)

!> initialize Curses
stdscr = initscr()

! Game Playfield too big
if (H+3 > maxH) call err('playfield height too tall for terminal window')
if (W+10 > maxW) call err('playfield width too wide for terminal window')
x0 = [1, W+15]

call noecho()
call cbreak()
call timeout(0)
ierr = keypad(stdscr, true)

!--- show title screen
call title()

!--- begin game

do i = 1,players
  call F(i)%setup(W=W, H=H, x0=x0(i), AI=isAI(i), difffact=difffact, debug=debug)
  !--- create first block
  call NP(i)%init(F(i))
  call P(i)%spawn_block(F(i), NP(i))
  call redraw(F(i), P(i), NP(i))
  call system_clock(count=F(i)%tic)
enddo

!--------- main loop
main: do

  do i = 1,players
    call mainsub(F(i), P(i), NP(i))
  end do

  if (update) then
    !call clear()
    do i = 1,players
      call redraw(F(i), P(i), NP(i))
    enddo
    update=.false.
  end if

  call sleep(F(1)%sleep_incr)


end do main

end subroutine main_game


subroutine mainsub(F, P, NP)
class(field) :: F
type(piece) :: P, NP

call key_input(F, P, NP)  ! was a key pressed?
call freeze(F, P, NP)

  update = P%movereq

call system_clock(count=F%toc)

!    if(debug) print *,F%toc-F%tic, F%toc, F%tic  ! in lower right corder of screen

if ( (F%toc - F%tic) / real(trate) > F%move_time) then
  !! time's up, move piece one step down. real(trate) is necessary for float time comparison!
  update=.true.
  call P%move_down()
  call freeze(F, P, NP)

  if (F%newhit) then
    if (modulo(F%Ncleared, F%lines_per_level)==0) call F%levelup()
  endif

  call system_clock(count=F%tic)
endif

end subroutine mainsub


subroutine redraw(F, P, NP)
class(field), intent(in) :: F
class(piece), intent(in) :: P, NP


call draw_screen(F)
! Draw the falling block
call draw_piece(P)
! Draw next block
call draw_piece(NP)

call draw_score(F)

call refresh()

end subroutine redraw


subroutine cmd_parse(W, H)
! reads flag command line arguments
integer, intent(out) :: W, H
integer :: i,argc
character(*),parameter :: logfn='blocktran.log'
character(32) :: arg
character(8)  :: date
character(10) :: time
! --- default values
W = randint(10,15)
H = randint(15,20)

difffact = 1.
players = 1
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
    if (H<6 .or. W<6) call err('Height and width must each be at least 6')

  case ('-d','--difficulty')
    call get_command_argument(i+1,arg)
    read(arg,'(F4.1)') difffact
    if (difffact<=0) call err('difficulty must be > 0')

  case('-p','--players')
    call get_command_argument(i+1,arg)
    read(arg,'(I1)') players
    if (players > 2 .or. players < 1) call err('only 1 or 2 players')

  case ('--debug','-v','--verbose')
    debug=.true.
    print *,'debug enabled, writing to ', logfn
    open(newunit=F(1)%udbg,file=logfn, action='Write', &
          form='formatted', status='unknown',   &
          position='append')

    call date_and_time(date,time)
    write(F(1)%udbg,*) '--------------------------------------------'
    write(F(1)%udbg,*) 'start: ', date,'T', time
    write(F(1)%udbg,*) 'Lines to clear                                 Counter'
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
      call mvaddch(y=(i-1), x=F%x0+(j-1)-1, ch='@')  ! frozen piece
    case (0)
      call mvaddch(y=(i-1), x=F%x0+(j-1)-1, ch='.')  ! background
    case default
      call err('unknown screen state')
    end select

  end do
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
call mvprintw(y=F%H, x=F%x0+0, str=msg)

write (msg, "(I10)") F%Nblock
call mvprintw(y=F%H+1, x=F%x0+0, str=msg)

write (msg, "(I2)") F%level
call mvprintw(y=F%H+2, x=F%x0+0, str=msg)

write (msg, "(I4)") F%Ncleared
call mvprintw(y=F%H+2, x=F%x0+F%W-4, str=msg)
end subroutine draw_score

end module game
