program test_key

  use cinter, only: initscr,getch, usleep

  implicit none

  integer :: ic

  call initscr()

  print *,'Ctrl-c to stop program, which prints keys pressed and their code'
  call usleep(2000000)

  do
    ic = getch()
    print *,ic
    call usleep(100000)
  end do

end program
