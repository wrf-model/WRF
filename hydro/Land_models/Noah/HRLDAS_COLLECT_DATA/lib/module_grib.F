!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_grib
  use module_grib1
  use module_grib2
  use module_grib_common
  implicit none

! Many of these are wrapper functions which call the appropriate GRIB1
! or GRIB2 routines as necessary.

contains


!=================================================================================
!=================================================================================

  subroutine gribopen(filename, fd, ierr)
    implicit none
    !
    ! Purpose:
    !      Open the GRIB2 file for reading, and do a few set-up tasks.
    !
    ! Input:
    !      filename:  Name of the GRIB2 file to open.
    !
    ! Output:
    !      fd:        Integer value for the c pointer to a memory location for 
    !                 a structure containing file information, among which is 
    !                 the FILE pointer.
    !
    !      ierr:      An error flag.  ierr == 0 -- File opened successfully.
    !                                 ierr == 1 -- There was some problem opening the file.
    !                                 ierr == 2 -- The specified file does not exist.
    ! 
    ! Side effects:
    !      - File <filename> is opened for reading
    !      - The file stream pointer for <filename>, contained within structure <fd>,
    !        is positioned at the beginning of the file.
    !
    character(len=*), intent(in)  :: filename
    integer(kind=8),  intent(out) :: fd
    integer,          intent(out) :: ierr

    logical :: lexist

    ierr = 0

    ! First check the existence of the file.
    inquire(file=trim(filename), exist=lexist)
    if (.not. lexist) then
       ierr = 2
       return
    endif
    
    call c_gribopen(trim(filename)//char(0), fd, ierr)

  end subroutine gribopen

!=================================================================================
!=================================================================================

  subroutine gribclose(fd)
    implicit none
    integer(kind=8), intent(in) :: fd
    call c_close(fd)
  end subroutine gribclose

!=================================================================================
!=================================================================================

  subroutine getgrib(fd, grib, ierr)
    implicit none
    ! 
    ! Purpose:
    !      Read a single grib record, but do no unpacking and no interpretation
    !      of header information beyond the size and edition nuber from GRIB 
    !      section zero.
    !
    integer(kind=8), intent(in)      :: fd
    type (GribStruct), intent(inout) :: grib
    integer, intent(out)             :: ierr

    integer :: extra
    integer :: iloc
    integer :: iread
    integer :: seventest

    logical, save                 :: grib1_tables_read = .FALSE.
    logical, save                 :: grib2_tables_read = .FALSE.

    !
    ! Position our file pointer at the beginning of the next GRIB record.
    !

    call findgrib(fd, grib, iloc)
    if (iloc < 0) then
       ierr = 1
       return
    endif

    ! Read the full grib record.
    call io_fread(fd, grib%buffer(17:), grib%size-16, iread, ierr)
    if (iread /= grib%size-16) then
       iloc = -7
       return
    endif
    if (ierr /= 0) then
       iloc = -8
       return
    endif

    grib%iskip = 0

    extra = 4 - mod(grib%size,4)
    call swap4f(grib%buffer, grib%size+extra)

    !
    !  Check the last four bytes
    !
    call gbyte(grib%buffer, seventest, (grib%size-4)*8, 32)
    if (seventest /= string_sevens) then
       write(*,'("Cannot find marker (7777) for end of GRIB record")')
       stop "Problem finding end of GRIB record"
    endif

    ! Read grib tables.
    if (grib%edition == 1) then
       if (.not. grib1_tables_read) then
          call grib1_read_parameter_tables()
          grib1_tables_read = .TRUE.
       endif
    else
       if (.not. grib2_tables_read) then
          call grib2_read_parameter_tables()
          grib2_tables_read = .TRUE.
       endif
    endif

  end subroutine getgrib

!=================================================================================
!=================================================================================

  subroutine findgrib(fd, grib, iloc)
    implicit none
    ! Purpose:
    !      Seek in an open GRIB file for the string "GRIB", which 
    !      indicates the beginning of a GRIB record.
    ! Input:
    !      fd: A file stream pointer to a file opened by subroutine GRIBOPEN
    !
    ! Output:
    !      iloc:  The location (by byte count) in the file stream <fd> 
    !             at the beginning of this particular GRIB record.  Or,
    !             a negative number indicating a read problem (probably we 
    !             hit the end of the file.
    !
    ! Side Effects:
    !      Position the c file pointer at the beginning of the grib record.

    integer(kind=8), intent(in) :: fd
    type (GribStruct) :: grib
    integer, intent(out) :: iloc

    integer :: ierr
    integer :: iread
    integer :: itell
    character(len=4) :: h4
    character(len=1), dimension(12) :: buf12
    integer :: extra
    integer :: i

    call io_fread(fd, h4, 4, iread, ierr)

    if (iread /= 4) then
       ! Probably, we've hit the end of file without finding a GRIB record
       iloc = -1
       return
    endif

    if (ierr /= 0) then
       ! Probably, we've hit the end of file without finding a GRIB record
       iloc = -2
       return
    endif

    do while ( h4 /= "GRIB") 
       ! Adjust our buffer and read one more byte
       h4(1:3) = h4(2:4)
       call io_fread(fd, h4(4:4), 1, iread, ierr)
       if (iread /= 1) then
          iloc = -3;
          return
       endif
       if (ierr /= 0) then
          iloc = -4;
          return
       endif
    enddo

    !
    !   To have got this far, we must have found the beginning of a GRIB
    !   record.  
    !

    !
    ! Read the next 12 bytes to determine GRIB record size
    !

    call io_fread(fd, buf12, 12, iread, ierr)
    if (iread /= 12) then
       iloc = -5
       return
    endif
    if (ierr /= 0) then
       iloc = -6
       return
    endif

    call swap4f(buf12, 12)

    ! Get the grib edition number.
    call gbyte(buf12, grib%edition, 24, 8)
    if (grib%edition == 2) then
       call gbyte(buf12, grib%size, 64, 32)
    else if (grib%edition == 1) then
       call gbyte(buf12, grib%size, 0, 24)
    else
       write(*, '("Grib Edition:  ", I10)') grib%edition
       stop "Grib Edition."
    endif
    ! write(*, '("Grib Edition:  ", I10, "  Record size:  ", I10)') grib%edition, grib%size

    !
    ! Allocate space for the full grib record.  The "extra" space allocated
    ! insures that we have a multiple of four bytes in our buffer, so the 
    ! byte-swapping will work correctly without out-of-bounds array references.
    !

    extra = 4 - mod(grib%size,4)
    allocate(grib%buffer(grib%size + extra), stat=ierr)
    if (ierr /= 0) then
       write(*,'("Problem allocating buffer for GRIB data in subroutine GETGRIB")')
       stop
    endif
    do i = 1, 4
       grib%buffer(i) = h4(i:i)
    enddo
    call swap4f(grib%buffer, 4)

    do i = 1, 12
       grib%buffer(i+4) = buf12(i)
    enddo

    ! Swap the bytes here, to make things consistent when we read the rest of the buffer.
    call swap4f(grib%buffer, 16)

    !
    !  Position the file stream pointer to the beginning of the GRIB record.
    !
    ! call io_fseek(fd, -16, 0);

    !
    ! Find the stream pointer position
    !

    call io_ftell(fd, itell)

    !
    !  Return the byte position of the beginning of this GRIB record
    !
    iloc = itell


  end subroutine findgrib

!=================================================================================
!=================================================================================

  subroutine grib_next_field(fd, grib, ierr)
    implicit none
    ! 
    ! Purpose:
    !      Unpack the next field in a GRIB file.  The field may already be
    !      available in <grib%buffer>; if not, we read from the file into 
    !      <grib%buffer>.
    ! 

    integer(kind=8), intent(in) :: fd
    type (GribStruct), intent(inout) :: grib
    integer, intent(out) :: ierr
    integer :: iloc
    integer :: isection
    integer :: test_sevens

    ierr = 0

    FIELD_LOOP : do

       if ( .not. associated (grib%buffer) ) then
          ! print*, 'Try to read a new record'
          ! Get the next complete GRIB record (which may contain more than one field,
          ! which is why we have the complication of the outer do loop here).
          call getgrib(fd, grib, ierr) 
          if (ierr /= 0) return
       else
          ! See if we're at "7777"  If so, deallocate things and read a new record.
          call gbyte(grib%buffer, test_sevens, grib%iskip, 32)
          if (test_sevens == string_sevens) then
             call deallogrib(grib)
             cycle FIELD_LOOP
          endif
       endif

       select case (grib%edition)
       case default

          write(*, '("MODULE_GRIB:  GRIB_NEXT_FIELD:  Unrecognized edition = ", I10)') grib%edition
          stop

       case (1)
          ! print*, 'Unpacking section 0'

          ! GRIB Edition 1 is a lot simpler, because we do not pack more than one field
          ! into a GRIB record.  Just process the whole record and be done with it.
          call grib1_unpack_sec0(grib%buffer, size(grib%buffer), grib%iskip)
          call grib1_unpack_sec1(grib%buffer, size(grib%buffer), grib%iskip, grib%g1sec1)
          if (grib%g1sec1%ifgds) call grib1_unpack_sec2(grib%buffer, size(grib%buffer), grib%iskip, grib%g1sec2)
          if (grib%g1sec1%ifbms) call grib1_unpack_sec3(grib)
          call grib1_unpack_sec4(grib%buffer, size(grib%buffer), grib%iskip, grib%g1sec4)
          ! call grib1_unpack_sec5(grib%buffer, size(grib%buffer), grib%iskip)
          ! We have unpacked a GRIB record's data field.  Time to return.
          return

       case (2)

          call unpack_next_grib2_section(isection, grib)

          do while ( isection < 7 )
             call unpack_next_grib2_section(isection, grib)
          enddo

          if (isection == 7) then
             ! We have unpacked a GRIB record's data field.  Time to return.
             return
          else if (isection == 8) then
             call deallogrib(grib)
          endif
       end select

    enddo FIELD_LOOP

  end subroutine grib_next_field

!=================================================================================
!=================================================================================

  subroutine gribdata(grib)
    ! Unpack the field and put in into the <grib->array> memory.
    implicit none
    type (GribStruct), intent(inout) :: grib

    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIBDATA:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_gribdata(grib)
    case (2)
       call grib2_gribdata(grib)
    end select

  end subroutine gribdata

!=================================================================================
!=================================================================================

  subroutine grib_parameter_text_information(grib, name, units, description)
    implicit none
    type (GribStruct) :: grib
    character(len=64) :: name
    character(len=256) :: units
    character(len=256) :: description

    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIB_PARAMETER_TEXT_INFORMATION:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_parameter_text_information(grib%g1sec1, name, units, description)
    case (2)
       call grib2_parameter_text_information(grib, name, units, description)
    end select
  end subroutine grib_parameter_text_information

!=================================================================================
!=================================================================================

  subroutine grib_level_information(grib, level_type, level_units, level_value, level2_value)
    implicit none
    type (GribStruct),  intent(in)  :: grib
    character(len=256), intent(out) :: level_type
    character(len=256), intent(out) :: level_units
    real,               intent(out) :: level_value
    real,               intent(out) :: level2_value

    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIB_LEVEL_INFORMATION:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_level_information(grib, level_type, level_units, level_value, level2_value)
    case (2)
       call grib2_level_information(grib, level_type, level_units, level_value, level2_value)
    end select

  end subroutine grib_level_information

!=================================================================================
!=================================================================================
  
  subroutine grib_time_information(grib, reference_date, valid_date, process, processing, p1_seconds, p2_seconds)
    implicit none
    type (GribStruct),  intent(in)  :: grib
    character(len=19), intent(out)  :: reference_date
    character(len=19), intent(out)  :: valid_date
    character(len=256), intent(out) :: process
    character(len=256), intent(out) :: processing
    integer,            intent(out) :: p1_seconds
    integer,            intent(out) :: p2_seconds
    
    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIB_TIME_INFORMATION:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_time_information(grib, reference_date, valid_date, process, processing, p1_seconds, p2_seconds)
    case (2)
       call grib2_time_information(grib, reference_date, valid_date, process, processing, p1_seconds, p2_seconds)
    end select
    
  end subroutine grib_time_information

!=================================================================================
!=================================================================================

  subroutine grib_valid_date(grib, validdate)
    ! Hmmmm.  This could also be a subfunction of unpacking
    ! GRIB2 section 4 (GRIB1 Section 1), and making <validdate> 
    ! an element of <grib> at the top level
    implicit none
    type (GribStruct), intent(in)  :: grib
    character(len=19), intent(out) :: validdate
    ! 

    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIB_VALID_DATE:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_valid_date(grib%g1sec1, validdate)
    case (2)
       call grib2_valid_date(grib, validdate)
    end select
  end subroutine grib_valid_date

!=================================================================================
!=================================================================================

  subroutine grib_map_information(grib)
    implicit none
    type(GribStruct), intent(inout) :: grib
    select case (grib%edition)
    case default
       write(*,'("MODULE_GRIB:  GRIB_MAP_INFORMATION:  Unrecognized grib%edition", I10)') grib%edition
       stop
    case (1)
       call grib1_map_information(grib%g1sec2, grib%mapinfo)
    case (2)
       call grib2_map_information(grib)
    end select

  end subroutine grib_map_information

!=================================================================================
!=================================================================================

end module module_grib
