!
! Public domain.
!

program test_io_mpi

    use module_io_int_idx, only: io_int_index, r_info
    use module_io_int_read, only: io_int_fetch_data
    use mpi

    implicit none

    integer, parameter :: llong_t   = selected_int_kind(16)   ! int64_t

    integer                            :: argc
    integer                            :: ierr
    integer                            :: i
    integer                            :: iunit
    integer(kind=llong_t)              :: offset
    integer                            :: count
    character(len=256)                 :: filename
    type(r_info), pointer              :: records(:) => NULL()
    integer                            :: nrecords
    character(len=256)                 :: var
    character(len=256)                 :: value

    argc     = 0
    filename = ''
    ierr     = 0
    offset   = 0
    count    = 0
    nrecords = 0
    var      = ''
    value    = ''

    call mpi_init(ierr)

    ! Get the file name to index
    argc = iargc()
    if (argc .lt. 2 ) then
        write(0, *) 'Must supply a filename to index and a variable to get.'
        write(0, *) 'i.e.: test_io_mpi MMINLU'
        call exit(1)
    endif
    call getarg(1, filename)
    call getarg(2, var)

    call io_int_index(filename, records, ierr)
    if (ierr .ne. 0) then
        call exit(ierr)
    endif

    call mpi_file_open(mpi_comm_world, trim(filename), &
                       mpi_mode_rdonly, mpi_info_null, &
                       iunit, ierr)

    call io_int_fetch_data(iunit, records, trim(var), value, ierr)
    write(6,*) trim(var), ': ', trim(value)

    call mpi_file_close(iunit, ierr)
    call mpi_finalize(ierr)

end program test_io_mpi

!
subroutine wrf_message(message)

    implicit none

    character(len=*), intent(in) :: message

    write(0,*) trim(message)
end subroutine wrf_message

subroutine wrf_error_fatal3(file, line, message)

    implicit none

    character(len=*), intent(in) :: file
    integer,          intent(in) :: line
    character(len=*), intent(in) :: message

    write(0,*) trim(file), 'line: ', line, ': ', trim(message)
    stop(1)
end subroutine wrf_error_fatal3
