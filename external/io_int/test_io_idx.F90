#if defined ( NO_IEEE_MODULE )      /* stub out entire program */
program test_io_idx
   print *,'NO TEST PROGRAM MADE for test_io_idx'
   print *,'NO TEST PROGRAM MADE for test_io_idx'
   print *,'NO TEST PROGRAM MADE for test_io_idx'
   print *,'NO TEST PROGRAM MADE for test_io_idx'
end program test_io_idx

#else
!
! Public domain.
!

program test_io_idx

    use module_io_int_idx

    implicit none

    integer, parameter :: STR_LEN = 132

    integer, parameter :: int_t     = selected_int_kind(8)    ! int32_t
    integer, parameter :: llong_t   = selected_int_kind(16)   ! int64_t

    integer, parameter :: float_t   = selected_real_kind(6)   ! float
    integer, parameter :: double_t  = selected_real_kind(15)  ! double

    integer                            :: argc
    integer                            :: ierr
    integer                            :: i
    integer(kind=llong_t)              :: offset
    integer                            :: count
    character(len=256)                 :: filename
    type(r_info), pointer              :: records(:) => NULL()
    integer                            :: nrecords


    argc     = 0
    filename = ''
    ierr     = 0
    offset   = 0
    count    = 0
    nrecords = 0

    ! Get the file name to index
    argc = command_argument_count()
    if (argc .lt. 1 ) then
        write(0, *) 'Must supply a filename to index'
        call exit(1)
    endif
    call getarg(1, filename)


    call io_int_index(filename, records, ierr)
    if (ierr .ne. 0) then
        call exit(ierr)
    endif

    open(2)
    write(unit=2, fmt='(A4,A10,A10,A8,A6,X,A32,X,A)') &
          '#', 'Offset', 'Data', 'Count', 'Type', 'Name', 'Date'
    do i=1,size(records)
        write(unit=2, fmt='(I4,I10,I10,I8,I6,X,A32,X,A)')   &
              i, records(i)%offset, records(i)%data_offset, &
              records(i)%data_count, records(i)%data_type,  &
              trim(io_int_string(records(i)%name)),         &
              trim(io_int_string(records(i)%date))
    enddo
    close(2)

    ! XLF does not play nice with C pointers. GFortran, ifort, pgi, lahey
    ! all do so, one can deallocate the record list with them.
    !deallocate(records)

    ! Either XLF or LSF seems to exit with err code 144 even when ierr is 0
!    call exit(ierr)
end program test_io_idx

!
subroutine wrf_message(message)

    implicit none

    character(len=*), intent(in) :: message

    write(0,*) trim(message)
end subroutine wrf_message
#endif      /* stub out entire program */
