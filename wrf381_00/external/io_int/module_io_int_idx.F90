#if defined ( NO_ISO_C_SUPPORT ) 
module module_io_int_idx
   private
   contains
      subroutine dummy
      end subroutine dummy
end module module_io_int_idx

#else
!
! Public domain.
!

!>
!! Module to retrieve an index from a WRF I/O Internal format file.
!!
!! This module is a binding to the C routines to perform the
!! index generation.
!!
!! The index generation will only parse records that have a size
!! of 2048 bytes. The index will consist of:
!! - offset The absolute offset to the start of the record within the file.
!! - data_offset The offset to the start of the data section.
!! - data_count  The number of data entries.
!! - data_type   The WRF type of the data entry.
!! - name   The record name.
!! - date   The date if the record is time dependent.
!
!
module module_io_int_idx

    use, intrinsic :: iso_c_binding,                                    &
                      only: c_char, c_ptr, c_int32_t, c_int64_t, c_loc, &
                            c_null_char, c_null_ptr, c_f_pointer

    implicit none
    private
    public :: r_info, io_int_index, io_int_loc, io_int_string

    integer, parameter :: F_LEN   = 2048
    integer, parameter :: STR_LEN = 132

    integer, parameter :: int_t    = selected_int_kind(8)   ! int32_t
    integer, parameter :: llong_t  = selected_int_kind(16)  ! int64_t
    integer, parameter :: float_t  = selected_real_kind(6)  ! float
    integer, parameter :: double_t = selected_real_kind(15) ! double

    !>
    !! r_info type definition for passing records meta data
    !! to and from C.
    !! The meta data consists of
    !! - offset The absolute offset to the start of the record within the file.
    !! - data_offset The offset to the start of the data section.
    !! - data_count  The number of data entries.
    !! - data_type   The WRF type of the data entry.
    !! - name   The record name.
    !! - date   The date if the record is time dependent.
    type, bind(c) :: r_info
        integer(c_int64_t)                         :: offset
        integer(c_int64_t)                         :: data_offset
        integer(c_int32_t)                         :: data_count
        integer(c_int32_t)                         :: data_type
        character(kind=c_char), dimension(STR_LEN) :: name
        character(kind=c_char), dimension(STR_LEN) :: date
    end type r_info

    !> All the interfaces are to the C functions. They all have
    !! suffixes _c to indicate this.
    interface

        integer(c_int32_t)                                        &
        function io_int_index_c                                   &
                   (filename, records, nrecords)                  &
                   bind(c, name='io_int_index')
            import :: c_ptr, c_char, c_int32_t, c_int64_t
            character(kind=c_char), dimension(*), intent(in)    :: filename
            type(c_ptr),                          intent(out)   :: records
            integer(c_int32_t),                   intent(out)   :: nrecords
        end function io_int_index_c

        integer(c_int32_t)                                        &
        function io_int_loc_c                                     &
                   (record, records, n, offset, count)            &
                   bind(c, name='io_int_loc')
            import :: c_ptr, c_char, c_int32_t, c_int64_t, r_info
            character(kind=c_char), dimension(*), intent(in)    :: record
            integer(c_int32_t), value,            intent(in)    :: n
            type(r_info),                         intent(in)    :: records(n)
            integer(c_int64_t),                   intent(out)   :: offset
            integer(c_int32_t),                   intent(out)   :: count
        end function io_int_loc_c

    end interface

    contains

    !>
    !! io_int_index generates an index of record names and
    !! offsets in a WRF IO binary file.
    !!
    !! There is a one-to-one mapping of offsets and record names.
    !! That is offset(i) corresponds to name(1), etc.
    !!
    !! \param[in]  filename The filename of binary file.
    !! \param[out] records  A struct of r_info record information.
    !! \param[out] nrecords The number of records within the file.
    !! \param[out] ierr     Return error status,
    !!                      0 If it was sucessful.
    !!                      1 If there was any error.
    !
    subroutine io_int_index(filename, records, ierr)
        implicit none

        character(len=*),      intent(in)  :: filename
        type(r_info), pointer, intent(out) :: records(:)
        integer,               intent(out) :: ierr

        type(c_ptr)                        :: r          ! Pointer to records
        integer                            :: nrecords   ! Number of records
        character(len=1024)                :: message    ! Error string

        nrecords = 0 ! Set the number of records to zero
        ierr     = 0 ! Clear the error status

        ierr = io_int_index_c(trim(filename)//c_null_char, r, nrecords)
        if (ierr .ne. 0) then
            write(message, *) 'Unable to index WRF binary file'
            call wrf_message(message)
            return
        endif
        call c_f_pointer(r, records, [nrecords])

    end subroutine io_int_index

    !>
    !! io_int_loc locates a record in the index of records,
    !! returning the record offset and element count.
    !!
    !! \param[in]  record  The record name to lookup.
    !! \param[in]  records A list of records information.
    !! \param[out] offset  The data start offset in the file.
    !! \param[out] count   The number of elements in the data.
    !! \param[out] ierr     Return error status,
    !!                      0 If it was sucessful.
    !!                      1 If there was any error.
    !
    subroutine io_int_loc(record, records, offset, count, ierr)
        implicit none

        character(len=*),         intent(in)  :: record
        type(r_info),             intent(in)  :: records(:)
        integer(kind=llong_t),    intent(out) :: offset
        integer,                  intent(out) :: count
        integer,                  intent(out) :: ierr

        character(len=1024)                   :: message    ! Error string

        ierr   = 0 ! Clear the error status
        offset = 0 ! Set the offset to zero
        count  = 0 ! Set the count to zero

        ierr = io_int_loc_c(trim(record)//c_null_char, records, &
                                size(records),  offset, count)
        if (ierr .ne. 0) then
            write(message, *) 'Unable find ', trim(record)
            call wrf_message(message)
            return
        endif

    end subroutine io_int_loc

    !>
    !! io_int_string converts an array of characters into a
    !! string.
    !!
    !! This function is needed due to some fortran compliers not
    !! working well when the C binding returns a character array
    !! and interpreting it as a string.
    !!
    !! \param[in]  arr The character array.
    !! \returns    str The fortran string.
    !
    function io_int_string(arr) result(str)
        character(kind=c_char),               intent(in)  :: arr(STR_LEN)
        character(len=STR_LEN)                            :: str

        integer                            :: i ! Temporary loop indexer

        i = 1
        do i=1,STR_LEN
            str(i:i)   = arr(i)
        enddo

    end function io_int_string

end module module_io_int_idx
#endif
