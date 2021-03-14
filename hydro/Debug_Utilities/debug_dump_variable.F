module debug_dump_variable
#ifdef MPP_LAND
    use module_mpp_land
#endif

    implicit none

    contains

    ! output a land surface dimensioned array to a test file for comparison

    subroutine dump_float_2d(target_array, filepath)
        implicit none
        ! the array to be written to file
        real, allocatable, dimension(:,:), intent(in) :: target_array

        ! the location to write the file
        character(len=*), intent(in) :: filepath

#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: out_buffer

        ! if we are in an mpi enviorment what needs to be done depends of if
        ! this process is the IO process
        ! if we are not the IO process write_IO_real will send data to the IO process
        ! if we are the IO process write_IO_RT_real will return the global array for this
        ! variable

        call write_IO_RT_real(target_array,out_buffer)
        if ( my_id .eq. IO_id ) then
            call debug_write_float_to_file(out_buffer,filepath)
        end if
#else
        call debug_write_float_to_file(target_array, filepath)
#endif

    end subroutine dump_float_2d

    ! output a routing dimensioned array to a test file for comparison

    subroutine dump_float_2d_rt(target_array, filepath)
        implicit none
        ! the array to be written to file
        real, allocatable, dimension(:,:), intent(in) :: target_array

        ! the location to write the file
        character(len=*), intent(in) :: filepath

#ifdef MPP_LAND
    real, dimension(global_rt_nx,global_rt_ny) :: out_buffer

        ! if we are in an mpi enviorment what needs to be done depends of if
        ! this process is the IO process
        ! if we are not the IO process write_IO_RT_real will send data to the IO process
        ! if we are the IO process write_IO_RT_real will return the global array for this
        ! variable

        call write_IO_real(target_array,out_buffer)
        if ( my_id .eq. IO_id ) then
            call debug_write_float_to_file(out_buffer,filepath)
        end if
#else
        call debug_write_float_to_file(target_array, filepath)
#endif

    end subroutine dump_float_2d_rt

    subroutine debug_write_float_to_file(target_array, filepath)
        ! parameters
        real, dimension(:,:) :: target_array
        character(len=*) :: filepath

        ! local variables
        integer :: nx
        integer :: ny

        integer :: x_pos, y_pos

        nx = size(target_array,1)
        ny = size(target_array,2)

        ! open the file for writing
        open(unit=237, file=filepath, form='formatted', status="replace", action="write")

        ! write the array to the file
        do y_pos = 1, ny
            do x_pos = 1, nx
                write(237,'(F12.5,4X)',advance='no') target_array(x_pos,y_pos)
            end do
            write(237,*) '' ! write the end line
        end do

        close(237)

    end subroutine



end module debug_dump_variable
