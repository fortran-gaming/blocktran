module blocks
    ! Stores the shape of the blocks at each of their rotations

    ! LINE BLOCK
    integer(kind=1), dimension(4,4), parameter :: line_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 1, 1, 1, &
           0, 0, 0, 0, &
           0, 0, 0, 0 /), &
           shape(line_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: line_rot_2 = reshape( &
        (/ 0, 0, 1, 0, &
           0, 0, 1, 0, &
           0, 0, 1, 0, &
           0, 0, 1, 0 /), &
           shape(line_rot_1), order=(/2, 1/))

    ! T BLOCK
    integer(kind=1), dimension(4,4), parameter :: t_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 1, 1, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(t_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: t_rot_2 = reshape( &
        (/ 0, 1, 0, 0, &
           1, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(t_rot_2), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: t_rot_3 = reshape( &
        (/ 0, 1, 0, 0, &
           1, 1, 1, 0, &
           0, 0, 0, 0, &
           0, 0, 0, 0 /), &
           shape(t_rot_3), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: t_rot_4 = reshape( &
        (/ 0, 1, 0, 0, &
           0, 1, 1, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(t_rot_4), order=(/2, 1/))

    ! L BLOCK
    integer(kind=1), dimension(4,4), parameter :: l_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 1, 1, 0, &
           1, 0, 0, 0, &
           0, 0, 0, 0 /), &
           shape(l_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: l_rot_2 = reshape( &
        (/ 1, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(l_rot_2), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: l_rot_3 = reshape( &
        (/ 0, 0, 0, 0, &
           0, 0, 1, 0, &
           1, 1, 1, 0, &
           0, 0, 0, 0 /), &
           shape(l_rot_3), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: l_rot_4 = reshape( &
        (/ 0, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 1, 1, 0, &
           0, 0, 0, 0 /), &
           shape(l_rot_4), order=(/2, 1/))

    ! J BLOCK
    integer(kind=1), dimension(4,4), parameter :: j_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 1, 1, 0, &
           0, 0, 1, 0, &
           0, 0, 0, 0 /), &
           shape(j_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: j_rot_2 = reshape( &
        (/ 0, 1, 0, 0, &
           0, 1, 0, 0, &
           1, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(j_rot_2), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: j_rot_3 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 0, 0, 0, &
           1, 1, 1, 0, &
           0, 0, 0, 0 /), &
           shape(j_rot_3), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: j_rot_4 = reshape( &
        (/ 0, 1, 1, 0, &
           0, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(j_rot_4), order=(/2, 1/))

    ! S BLOCK
    integer(kind=1), dimension(4,4), parameter :: s_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           0, 1, 1, 0, &
           1, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(s_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: s_rot_2 = reshape( &
        (/ 1, 0, 0, 0, &
           1, 1, 0, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(s_rot_1), order=(/2, 1/))

    ! Z BLOCK
    integer(kind=1), dimension(4,4), parameter :: z_rot_1 = reshape( &
        (/ 0, 0, 0, 0, &
           1, 1, 0, 0, &
           0, 1, 1, 0, &
           0, 0, 0, 0 /), &
           shape(z_rot_1), order=(/2, 1/))
    integer(kind=1), dimension(4,4), parameter :: z_rot_2 = reshape( &
        (/ 0, 0, 1, 0, &
           0, 1, 1, 0, &
           0, 1, 0, 0, &
           0, 0, 0, 0 /), &
           shape(z_rot_1), order=(/2, 1/))

    ! SQUARE BLOCK
    integer(kind=1), dimension(4,4), parameter :: square_rot_1 = reshape( &
        (/ 0, 1, 1, 0, &
           0, 1, 1, 0, &
           0, 0, 0, 0, &
           0, 0, 0, 0 /), &
           shape(square_rot_1), order=(/2, 1/))

    contains
        ! Mutates rotation
        function get_shape(block_type, rotation)
            integer(kind=1), dimension(4,4) :: get_shape
            integer(kind=1) :: rotation, block_type
            
            select case (block_type)
                case (0)
                    get_shape = get_line_shape(rotation)
                case (1)
                    get_shape = get_t_shape(rotation)
                case (2)
                    get_shape = get_l_shape(rotation)
                case (3)
                    get_shape = get_j_shape(rotation)
                case (4)
                    get_shape = get_s_shape(rotation)
                case (5)
                    get_shape = get_z_shape(rotation)
                case (6)
                    get_shape = get_square_shape(rotation)
            end select
        end function get_shape

        function get_line_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_line_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 2)

            select case (rotation)
                case (0)
                    get_line_shape = line_rot_1
                case (1)
                    get_line_shape = line_rot_2
            end select
        end function get_line_shape

        function get_t_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_t_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 4)

            select case (rotation)
                case (0)
                    get_t_shape = t_rot_1
                case (1)
                    get_t_shape = t_rot_2
                case (2)
                    get_t_shape = t_rot_3
                case (3)
                    get_t_shape = t_rot_4
            end select
        end function get_t_shape

        function get_l_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_l_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 4)

            select case (rotation)
                case (0)
                    get_l_shape = l_rot_1
                case (1)
                    get_l_shape = l_rot_2
                case (2)
                    get_l_shape = l_rot_3
                case (3)
                    get_l_shape = l_rot_4
            end select
        end function get_l_shape

        function get_j_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_j_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 4)

            select case (rotation)
                case (0)
                    get_j_shape = j_rot_1
                case (1)
                    get_j_shape = j_rot_2
                case (2)
                    get_j_shape = j_rot_3
                case (3)
                    get_j_shape = j_rot_4
            end select
        end function get_j_shape

        function get_s_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_s_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 2)

            select case (rotation)
                case (0)
                    get_s_shape = s_rot_1
                case (1)
                    get_s_shape = s_rot_2
            end select
        end function get_s_shape

        function get_z_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_z_shape
            integer(kind=1) :: rotation

            rotation = modulo(rotation, 2)

            select case (rotation)
                case (0)
                    get_z_shape = z_rot_1
                case (1)
                    get_z_shape = z_rot_2
            end select
        end function get_z_shape

        function get_square_shape(rotation)
            integer(kind=1), dimension(4,4) :: get_square_shape
            integer(kind=1) :: rotation

            rotation = 0

            get_square_shape = square_rot_1
        end function get_square_shape
end module blocks
