program test_key

  use cinter, only: initscr,getch, usleep
  use, intrinsic:: iso_fortran_env, only: stdout=>output_unit, stdin=>input_unit

  implicit none

  integer :: ic
  
  !Close(stdin) first is not correct.
  ! open(stdin,access='stream') ! error in gfortran. 

  print *,'Ctrl-c to exit. Prints keys pressed and their code'
  print *,'Special keys like arrows need nested "select case".'
  call usleep(2000000)
  
  call initscr()
  

  do
    ic = getch()  ! 4-byte integer, automatically prints character!
  ! read(stdin,*) ic !Nope
    write(stdout,'(I4,A1)',advance='no') ic,achar(13)
    flush(stdout)
    
    call usleep(200000)
  end do
  
  call endwin()

end program
