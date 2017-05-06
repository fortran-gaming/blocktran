program add
    use cinter
    use blocks
    implicit none

    ! 1 for left, 2 for down, 3 for right, 4 for rotate
    integer(kind=1) :: instruction

    ! 0 for blank, 1 for block
    integer(kind=1), dimension(20, 10) :: screen

    ! Current x/y of the falling piece
    integer(kind=1) :: cur_x, cur_y

    ! Type of falling piece
    ! 0: Line, 1: Square, 2: T, 3: S, 4: Z, 5: J, 6: L
    integer(kind=1) :: cur_type, next_type

    ! Rotation of falling piece
    integer(kind=1) :: cur_rotation = 0

    ! Current score
    integer :: score = 0

    integer(kind=1), parameter :: next_display_x = 15, next_display_y = 5
    integer(kind=1) :: next_display_rotation = 0

    integer(kind=1), parameter :: number_of_types = 7

    ! Milliseconds between each automatic downward move
    integer, parameter :: ms_move_time = 500
    integer :: ms_count = 0

    call initscr()
    call timeout(0)
    call init_screen_array()
    
    call random_seed()

    call generate_next_type()
    call spawn_block()

    do while (.true.)
        call clear()
        call draw_screen()
        ! Draw the falling block
        call draw_piece(cur_x, cur_y, cur_type, cur_rotation)
        ! Draw next block
        call draw_piece(next_display_x, next_display_y, next_type, &
            next_display_rotation)
        call draw_score()
        call handle_input()

        if (ms_count > ms_move_time) then
            call move_down()
            ms_count = 0
        end if

        call usleep(1)
        ms_count = ms_count + 1
    end do

    contains
        subroutine game_over()
            call endwin()
            Print *, 'Score: ', score
            call exit()
        end subroutine game_over

        ! Sets all the values in the screen array to 0
        subroutine init_screen_array()
            integer :: i, j

            do i = 1, 20
                do j = 1, 10
                    screen(i, j) = 0
                end do
            end do
        end subroutine init_screen_array

        subroutine draw_screen()
            integer :: i, j

            do i = 1, 20
                do j = 1, 10
                    if (screen(i, j) == 1) then
                        call addch('@')
                    else
                        call addch('.')
                    end if
                end do
                call addch(NEW_LINE(' '))
            end do
        end subroutine draw_screen

        subroutine draw_score()
            character(len=16) :: score_msg = ""
            write (score_msg, "(I10)"), score

            call mvprintw(20, 0, score_msg)
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
            integer(kind=1) :: x
            x = cur_x - 1
            if (.not. check_collision(x, cur_y, cur_rotation)) then
                cur_x = cur_x - 1
            end if
        end subroutine move_left

        subroutine move_right()
            integer(kind=1) :: x
            x = cur_x + 1
            if (.not. check_collision(x, cur_y, cur_rotation)) then
                cur_x = cur_x + 1
            end if
        end subroutine move_right

        subroutine move_down()
            integer(kind=1) :: y
            y = cur_y + 1
            if (.not. check_collision(cur_x, y, cur_rotation)) then
                cur_y = cur_y + 1
            else
                call piece_hit()
            end if
        end subroutine move_down

        subroutine rotate_piece()
            integer(kind=1) :: rotation
            rotation = cur_rotation + 1
            if (.not. check_collision(cur_x, cur_y, rotation)) then
                cur_rotation = cur_rotation + 1
            end if
        end subroutine rotate_piece

        function check_collision(x, y, rotation) result (collided)
            logical :: collided
            integer(kind=1) :: x, y, rotation
            integer(kind=1), dimension(4,4) :: block
            integer :: i, j, jx, iy

            collided = .false.
            block = get_shape(cur_type, rotation)

            iloop: do i = 1, 4
                iy = i + y - 2
                do j = 1, 4
                    jx = j + x - 2
                    if (block(i, j) == 1) then
                        ! Handling left/right boundaries
                        if (jx < 0 .or. jx >= 10) then
                            collided = .true.
                            exit iloop
                        end if

                        ! Floor
                        if (iy >= 20) then
                            collided = .true.
                            exit iloop
                        end if

                        ! Other blocks
                        if (iy > 0 .and. iy < 20) then
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
            integer(kind=1) :: offset_x, offset_y, piece_type, piece_rotation
            integer(kind=1), dimension(4,4) :: block
            integer :: i, j, x, y

            block = get_shape(piece_type, piece_rotation)

            do i = 1, 4
                y = i + offset_y - 2
                do j = 1, 4
                    x = j + offset_x - 2
                    if (y >= 0 .and. block(i, j) == 1) then
                        call mvaddch(y, x, '#')
                    end if
                end do
            end do
        end subroutine draw_piece

        ! Called when a piece has hit another and is solidifying
        subroutine piece_hit()
            integer(kind=1), dimension(4,4) :: block
            integer :: i, j, x, y

            block = get_shape(cur_type, cur_rotation)

            do i = 1, 4
                y = i + cur_y - 1
                do j = 1, 4
                    x = j + cur_x - 1
                    if (block(i, j) == 1) then
                        if (y <= 1) then
                            call game_over()
                        end if
                        screen(y, x) = 1
                    end if
                end do
            end do

            call handle_clearing_lines()
            call spawn_block()
        end subroutine piece_hit

        subroutine generate_next_type()
            real :: r
            call random_number(r)
            next_type = floor(r * number_of_types)
        end subroutine

        subroutine spawn_block()
            cur_x = 4
            cur_y = -1
            cur_type = next_type
            cur_rotation = 0

            call generate_next_type()
        end subroutine spawn_block

        subroutine handle_clearing_lines()
            integer(kind=1), dimension(4) :: lines_to_clear
            integer(kind=1) :: i, j, counter, line
            logical :: clear_row

            counter = 0

            do i = 1, 20
                clear_row = .true.
                jloop: do j = 1, 10
                    if (screen(i, j) /= 1) then
                        clear_row = .false.
                        exit jloop
                    end if
                end do jloop

                if (clear_row) then
                    counter = counter + 1
                    lines_to_clear(counter) = i
                end if
            end do

            select case (counter)
                case (1)
                    score = score + 40
                case (2)
                    score = score + 100
                case (3)
                    score = score + 300
                case (4)
                    score = score + 1200
            end select

            do i = 1, counter
                line = lines_to_clear(i) + i - 1
                do j = 1, 10
                    screen(line, j) = 0
                end do
                ! Bring everything down
                screen(1:line, :) = cshift(screen(1:line, :), shift=-1, dim=1)
            end do
        end subroutine handle_clearing_lines
end program add
