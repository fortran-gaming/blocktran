program test_key

    use cinter

    call initscr()

    do 
        ic = getch()
        print*,ic
        call usleep(100000)
    end do
    
    call endwin()


end program
