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

module module_grib2
  use module_mapinfo
  use module_grib2_tables
  use kwm_date_utilities
  use module_grib_common
  implicit none

contains

!==============================================================================
!==============================================================================
  subroutine grib2_gribdata(grib)
    implicit none
    type (GribStruct), intent(inout) :: grib
    ! Reorient data as necessary to put grib%array(1,1) at the lower left corner
  end subroutine grib2_gribdata

!==============================================================================
!==============================================================================

  subroutine grib2_valid_date(grib, validdate)
    ! Hmmmm.  This could also be a subfunction of unpacking
    ! GRIB2 section 4, and making <validdate> an element of <grib>
    implicit none
    type (GribStruct), intent(in)  :: grib
    character(len=19), intent(out) :: validdate
    ! 
    !Local:
    character(len=19) :: refdate
    integer :: fcst_time
    integer :: time_factor

    refdate = grib%sec1%hdate

    select case (grib%sec1%srt) ! Check the "significance of reference time"
    case default
       write(*,'("WARNING: Unrecognized ''Significance of Reference Time'' (GRIB2, section 1, octet 12):  ", I10)') grib%sec1%srt
       stop "FATAL ERROR: In module module_grib2.F -- Unrecognized Significance of Reference Time"

    case (0:1)  ! <grib%sec1%hdate> refers to the analysis time(0) or forecast start time (1)

       select case (grib%sec4%product_template_number) ! Check the product template number
       case default
          write(*,'("WARNING: Unrecognized Product Definition Template: ", I8)') grib%sec4%product_template_number
       case (0) ! Analysis or forecast at a point in time
          fcst_time = grib%sec4%time
          select case (grib%sec4%time_range_indicator) ! Check the units on the time
          case default
             write(*,'("WARNING: Unrecognized time range indicator: ", I8)') grib%sec4%time_range_indicator
             stop "FATAL ERROR: In module module_grib2.F -- Unrecognized time range indicator"
          case (0) ! Time in minutes
             time_factor = 60
             call geth_newdate(validdate(1:19), refdate(1:19), fcst_time*time_factor)
          case (1) ! Time in hours
             time_factor = 3600
             call geth_newdate(validdate(1:19), refdate(1:19), fcst_time*time_factor)
          end select
       end select
!       case (2)  ! <grib%sec1%hdate> refers to the forecast valid time
    end select
  end subroutine grib2_valid_date

!=================================================================================
!=================================================================================

  subroutine grib2_map_information(grib)
    implicit none
    type(GribStruct), intent(inout) :: grib

    select case (grib%sec3%grid_template_number)
    case default
       write(*, '("WARNING: Unrecognized GRIB 2 grid template number:  ", I12)') grib%sec3%grid_template_number
       stop "FATAL ERROR: In module module_grib2.F -- GRIB2_MAP_INFORMATION:  Unrecognized grid template number."
    case (0)
       ! call print_sec3(grib, 10000)
       grib%mapinfo%hproj = "CE"
       grib%mapinfo%supmap_jproj = 8
       grib%mapinfo%nx = grib%sec3%nx
       grib%mapinfo%ny = grib%sec3%ny
       grib%mapinfo%dx = grib%sec3%gt_3_0%dx
       grib%mapinfo%dy = grib%sec3%gt_3_0%dy * grib%sec3%gt_3_0%j_scan_direction
       grib%mapinfo%lat1 = grib%sec3%GT_3_0%la1
       grib%mapinfo%lon1 = grib%sec3%GT_3_0%lo1
       grib%mapinfo%xlonc = -1.E33
       grib%mapinfo%truelat1 = -1.E33
       grib%mapinfo%truelat1 = -1.E33
    case (30)
       grib%mapinfo%hproj = "LC"
       grib%mapinfo%supmap_jproj = 3
       grib%mapinfo%nx = grib%sec3%nx
       grib%mapinfo%ny = grib%sec3%ny
       grib%mapinfo%dx = grib%sec3%GT_3_30%dx
       grib%mapinfo%dy = grib%sec3%GT_3_30%dy
       grib%mapinfo%lat1 = grib%sec3%GT_3_30%la1
       grib%mapinfo%lon1 = grib%sec3%GT_3_30%lo1
       grib%mapinfo%xlonc = grib%sec3%GT_3_30%lov
       grib%mapinfo%truelat1 = grib%sec3%GT_3_30%latin1
       grib%mapinfo%truelat2 = grib%sec3%GT_3_30%latin2
    end select
  end subroutine grib2_map_information


!=================================================================================
!=================================================================================

  subroutine fortran_decode_jpeg2000(buffer, buffer_length, decoded)
    implicit none
    ! 
    ! Purpose:
    !      Take a character array <buffer> which contains a JPEG2000-encoded field,
    !      and return the decoded field as an integer array.  This subroutine is 
    !      simply a wrapper for the c code that decodes jpeg2000 streams.
    !
    ! Input:
    !      buffer:        Character buffer which holds the JPEG2000-encoded field.
    !      buffer_length: Size of character array <buffer>.
    !
    ! Output
    !      decoded:       Pointer to an integer array representing the decoded field.
    !
    ! Side Effects:
    !      Any?  Probably some in the subroutines called from this routine.
    !
    integer,                                    intent(in) :: buffer_length
    character(len=1), dimension(buffer_length), intent(in) :: buffer
    integer, pointer, dimension(:)                         :: decoded

    ! Local:
    integer         :: width
    integer         :: height
    integer(kind=8) :: image_pointer
    integer(kind=8) :: stream_pointer
    integer         :: allocation_status

    call info_jpeg2000(buffer, buffer_length, width, height, image_pointer, stream_pointer)

    allocate(decoded(width*height), stat=allocation_status)
    if (allocation_status /= 0) then
       write(*,'("WARNING: Problem allocating space in FORTRAN_DECODE_JPEG2000.")')
       stop "FATAL ERROR: In module module_grib2.F -- Problem allocating space in FORTRAN_DECODE_JPEG2000"
    endif

    call decode_jpeg2000(image_pointer, stream_pointer, width, height, decoded)

  end subroutine fortran_decode_jpeg2000

!=================================================================================
!=================================================================================

  subroutine grib2_parameter_text_information(grib, abbr, units, description)
    implicit none
    ! This could really be considered a substep of unpacking section 4.  Let's 
    ! move it there sometime, and create grib%abbr, grib%units, grib%description!
    type (GribStruct), intent(in)  :: grib
    character(len=16), intent(out) :: abbr
    character(len=256), intent(out) :: units
    character(len=256), intent(out) :: description
    character(len=256) :: category_name

    call get_parameter_table_information(grib%discipline, &
         grib%sec4%parameter_category, grib%sec4%parameter_number, &
         category_name, description, units)
    abbr = "ABBR"

  end subroutine grib2_parameter_text_information

!=================================================================================
!=================================================================================

  subroutine grib2_time_information(grib, reference_date, valid_date, process, processing, p1_seconds, p2_seconds)
    implicit none
    type (GribStruct), intent(in)   :: grib
    character(len=19), intent(out)  :: reference_date
    character(len=19)  :: valid_date
    character(len=256) :: process
    character(len=256) :: processing
    integer, intent(out)            :: p1_seconds
    integer, intent(out)            :: p2_seconds

    character(len=256) :: text3

    reference_date = grib%sec1%hdate

    select case (grib%sec1%srt)
    case default
       write(process,'("Unrecognized significance of reference time")')
       write(*,'(A)') trim(process)
    case (0)
       write(process,'("Analysis time:   ", A, 1x, A)') grib%sec1%hdate(1:10), grib%sec1%hdate(12:19)
    case (1)
       write(process,'("Start of forecast:   ", A, 1x, A)') grib%sec1%hdate(1:10), grib%sec1%hdate(12:19)
    case (2)
       write(process,'("Verifying time of forecast:   ", A, 1x, A)') grib%sec1%hdate(1:10), grib%sec1%hdate(12:19)
    case (3)
       write(process,'("Observation time:  ", A, 1x, A)') grib%sec1%hdate(1:10), grib%sec1%hdate(12:19)
    end select

    processing = " "
    text3 = " "
    select case (grib%sec4%product_template_number)
    case default
       write(processing,'("Unrecognized Product Definition Template: ", I8)') grib%sec4%product_template_number
    case (0)
       select case (grib%sec4%time_range_indicator)
       case default
          write(processing,'("Unrecognized time range indicator:", I8)') grib%sec4%time_range_indicator
          valid_date = "0000-00-00_00:00:00"
          p1_seconds = 0
          p2_seconds = 0
       case (0)
          write(processing,'("Forecast time:  ",I8," minutes")') grib%sec4%time
          p1_seconds = grib%sec4%time*60
          p2_seconds = 0
          call geth_newdate(valid_date, grib%sec1%hdate, p1_seconds)
          write(text3, '("Forecast valid time:  ", A, 1x, A)') valid_date(1:10), valid_date(12:19)
       case (1)
          write(processing,'("Forecast time:  ",I8," hours")') grib%sec4%time
          p1_seconds = grib%sec4%time*3600
          p2_seconds = 0
          call geth_newdate(valid_date, grib%sec1%hdate, p1_seconds)
          write(text3, '("Forecast valid time:  ", A, 1x, A)') valid_date(1:10), valid_date(12:19)
       case (2)
          write(processing,'("Forecast time:  ",I8," days")') grib%sec4%time
          p1_seconds = grib%sec4%time*86400
          p2_seconds = 0
          call geth_newdate(valid_date, grib%sec1%hdate, p1_seconds)
          write(text3, '("Forecast valid time:  ", A, 1x, A)') valid_date(1:10), valid_date(12:19)
       case (13)
          write(processing,'("Forecast time:  ",I8," seconds")') grib%sec4%time
          p1_seconds = grib%sec4%time
          p2_seconds = 0
          call geth_newdate(valid_date, grib%sec1%hdate, grib%sec4%time)
          write(text3, '("Forecast valid time:  ", A, 1x, A)') valid_date(1:10), valid_date(12:19)
          
       end select
    case (8)
       select case (grib%sec4%PDT_4_8%statistical_process)
       case default
          write(processing, '("Unrecognized statistical process: ",I4)') grib%sec4%PDT_4_8%statistical_process
       case (0)
          write(processing, '("Average over ", I6, 1x, A)') &
               grib%sec4%PDT_4_8%length_of_time_range, interpret_time_unit(grib%sec4%PDT_4_8%time_range_unit)
          write(text3, '("from ", A, " to ", A)') &
               grib%sec4%PDT_4_8%begin_hdate, grib%sec4%PDT_4_8%end_hdate
          processing = trim(processing)//" "//trim(text3)
       case (1)
          write(processing, '("Accumulation over ", I6, 1x, A)') &
               grib%sec4%PDT_4_8%length_of_time_range, interpret_time_unit(grib%sec4%PDT_4_8%time_range_unit)
          write(text3, '("from ", A, " to ", A)') &
               grib%sec4%PDT_4_8%begin_hdate, grib%sec4%PDT_4_8%end_hdate
          processing = trim(processing)//" "//trim(text3)
          valid_date = grib%sec4%PDT_4_8%end_hdate

          select case (grib%sec4%time_range_indicator)
          case default
             write(processing,'("Unrecognized time range indicator for accumulation:", I8)') grib%sec4%time_range_indicator
             stop "FATAL ERROR: In module module_grib2.F -- Unrecognized time range indicator for accumulation"
          case (0)
             p1_seconds = grib%sec4%time*60
          case (1)
             p1_seconds = grib%sec4%time*3600
          case (2)
             p1_seconds = grib%sec4%time*86400
          case (13)
             p1_seconds = grib%sec4%time
          end select

          select case (grib%sec4%PDT_4_8%time_range_unit)
          case default
             write(processing,'("Unrecognized time range unit for accumulation:", I8)') grib%sec4%PDT_4_8%time_range_unit
             stop "FATAL ERROR: In module module_grib2.F -- Unrecognized time range unit for accumulation"
          case (0)
             p2_seconds = p1_seconds + (grib%sec4%PDT_4_8%length_of_time_range) * 60
          case (1)
             p2_seconds = p1_seconds + (grib%sec4%PDT_4_8%length_of_time_range) * 3600
          case (2)
             p2_seconds = p1_seconds + (grib%sec4%PDT_4_8%length_of_time_range) * 86400
          case (13)
             p1_seconds = p1_seconds + (grib%sec4%PDT_4_8%length_of_time_range)
          end select
          
       case (2)
          write(processing, '("Maximum in ", I6, 1x, A)') &
               grib%sec4%PDT_4_8%length_of_time_range, interpret_time_unit(grib%sec4%PDT_4_8%time_range_unit)
       case (3)
          write(processing, '("Minimum in ", I6, 1x, A)') &
               grib%sec4%PDT_4_8%length_of_time_range, interpret_time_unit(grib%sec4%PDT_4_8%time_range_unit)
       case (4)
          write(processing, '("Difference over ", I6, 1x, A)') & 
              grib%sec4%PDT_4_8%length_of_time_range, interpret_time_unit(grib%sec4%PDT_4_8%time_range_unit)
       end select
    end select
  end subroutine grib2_time_information

!=================================================================================
!=================================================================================

#ifdef _DEAD_
  subroutine get_grib_bitmap(grib, bitmap)
    implicit none
    type (GribStruct), intent(in) :: grib
    integer, pointer, dimension(:,:) :: bitmap

    if (grib%sec6%bit_map_indicator /= 255) then
       bitmap => grib%sec6%bitmap
    else
       nullify(bitmap)
    endif

  end subroutine get_grib_bitmap
#endif
!=================================================================================
!=================================================================================

  subroutine get_grib_dimensions(grib, idim, jdim)
    ! Probably not needed, since we define a mapinfo structure.
    implicit none
    type (GribStruct), intent(in)  :: grib
    integer,           intent(out) :: idim
    integer,           intent(out) :: jdim
    idim = grib%sec3%nx
    jdim = grib%sec3%ny
  end subroutine get_grib_dimensions

!=================================================================================
!=================================================================================

  subroutine get_grib_data_array(grib, array)
    implicit none
    type (GribStruct), intent(in) :: grib
    real, pointer, dimension(:,:) :: array
    array => grib%array
  end subroutine get_grib_data_array


!=================================================================================
!=================================================================================

  subroutine deallogrib(grib)
    type (GribStruct) :: grib
    if (associated(grib%buffer)) then
       deallocate(grib%buffer)
       nullify(grib%buffer)
    endif
    if (associated(grib%bitmap)) then
       deallocate(grib%bitmap)
       nullify(grib%bitmap)
    endif
    if (associated(grib%array)) then
       deallocate(grib%array)
       nullify(grib%array)
    endif
    if (associated(grib%sec7%floated)) then
       deallocate(grib%sec7%floated)
       nullify(grib%sec7%floated)
    endif
    grib%size = 0
  end subroutine deallogrib

!=================================================================================
!=================================================================================

  subroutine grib2_level_information(grib, level_type, level_units, level_value, level2_value)
    implicit none
    type (GribStruct),  intent(in)  :: grib
    character(len=256), intent(out) :: level_type
    character(len=256), intent(out) :: level_units
    real,               intent(out) :: level_value
    real,               intent(out) :: level2_value

    level2_value = -1.E36
    select case (grib%sec4%product_template_number)
    case default
       write(*,'("WARNING: MODULE_GRIB2:  GRIB2_LEVEL_INFORMATION:  Unrecognized product_template_number: ", I4)') &
            grib%sec4%product_template_number
       stop "FATAL ERROR: In module module_grib2.F -- GRIB2_LEVEL_INFORMATION: Unrecognized product_template_number"
    case (0)
       call get_level_string(grib%sec4%ltype1, level_type, level_units)
       level_value  = grib%sec4%level1
       if (grib%sec4%ltype2 /= 255) then
          level2_value = grib%sec4%level2
       endif
    case (8)
       call get_level_string(grib%sec4%ltype1, level_type, level_units)
       level_value  = grib%sec4%level1
       if (grib%sec4%ltype2 /= 255) then
          level2_value = grib%sec4%level2
       endif
    end select

    ! write(*,'("MODULE_GRIB2:  GRIB2_LEVEL_INFORMATION:  To do.")')
    ! stop
  end subroutine grib2_level_information

!=================================================================================
!=================================================================================



!=================================================================================
!=================================================================================

  subroutine unpack_next_grib2_section(isection, grib)

    !  Returns the section number of the section just unpacked 
    implicit none
    integer, intent(out) :: isection
    type(GribStruct), intent(inout) :: grib
    integer :: iread
    character(len=4) :: hh
    integer :: isize
    integer :: ierr
    integer :: n
    integer :: edition
    integer :: gribsizeA, gribsizeB
    integer(kind=8) :: gribsize
    character(len=1), dimension(12) :: buf

    ! Take four bytes:

    isize = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)

    if (isize == string_grib) then
       grib%iskip = grib%iskip + 16
       isection = 0
       grib%discipline = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       edition    = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       gribsizeA  = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       gribsizeB  = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       if (gribsizeA /= 0) then
          stop "FATAL ERROR: In module module_grib2.F -- UNPACK_NEXT_GRIB2_SECTION:  Large size.  Despair!"
       endif
       gribsize = gribsizeB
       return
    else if (isize == string_sevens) then
       isection = 8
       return
    else
       isection = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       select case (isection)
       case (1)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec1(grib);
          ! call print_sec1(grib, 10000);
       case (3)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec3(grib);
          ! call print_sec3(grib, 10000);
       case (4)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec4(grib);
          ! call print_sec4(grib);
       case (5)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec5(grib);
          ! call print_sec5(grib);
       case (6)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec6(grib);
          ! call print_sec6(grib,10000);
       case (7)
          grib%iskip = grib%iskip - 5*8
          call unpack_sec7(grib);
          ! call print_sec7(grib);
       case default
          write(*,'("WARNING: Section?  ", I10)') isection
          stop "FATAL ERROR: In module module_grib2.F -- Section?"
       end select
    endif

  end subroutine unpack_next_grib2_section

!=================================================================================
!=================================================================================

  subroutine unpack_sec1(grib)
    implicit none
    type (GribStruct), intent(inout) :: grib

    character(len=8) :: hdate
    integer :: section

    grib%sec1%size              = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section                     = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%center            = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
    grib%sec1%subcenter         = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
    grib%sec1%mtvn              = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%ltvn              = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)  
    grib%sec1%srt               = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%year              = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
    grib%sec1%month             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%day               = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%hour              = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%minute            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%second            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%production_status = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%sec1%data_type         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)  

    write(grib%sec1%hdate, '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
         grib%sec1%year, grib%sec1%month, grib%sec1%day, grib%sec1%hour, grib%sec1%minute, grib%sec1%second

  end subroutine unpack_sec1

!=================================================================================
!=================================================================================

  subroutine print_sec1(grib, verbosity)
    implicit none
    type(GribStruct), intent(in)  :: grib
    integer,          intent(in)  :: verbosity

    if (verbosity >= 10) then
       write(*,'("Section 1:")')
       if (verbosity >= 100) then
          write(*,'("   Length of section:               ",I4)') grib%sec1%size
          write(*,'("   Originating Center:              ",I4)') grib%sec1%center
          write(*,'("   Originating Subcenter:           ",I4)') grib%sec1%subcenter
          write(*,'("   Master Tables Version Number:    ",I4)') grib%sec1%mtvn
          write(*,'("   Local Tables Version Number:     ",I4)') grib%sec1%ltvn
          write(*,'("   Significance of Reference Time:  ",I4,":  ")', advance="no") grib%sec1%srt

          select case (grib%sec1%srt)
          case default
             write( *, '("Unrecognized significance of reference time")' )
          case (0)
             write(*,'("analysis time.  ", A, 1x, A)')
          case (1)
             write(*,'("forecast start time.  ", A, 1x, A)')
          case (2)
             write(*,'("verifying time of forecast.  ", A, 1x, A)')
          case (3)
             write(*,'("observation time.  ", A, 1x, A)')
          end select

          if (verbosity >= 500) then
             write(*,'("   Year (Reference Time):           ",I4)') grib%sec1%year
             write(*,'("   Month (Reference Time):          ",I4)') grib%sec1%month
             write(*,'("   Day (Reference Time):            ",I4)') grib%sec1%day
             write(*,'("   Hour (Reference Time):           ",I4)') grib%sec1%hour
             write(*,'("   Minute (Reference Time):         ",I4)') grib%sec1%minute
             write(*,'("   Second (Reference Time):         ",I4)') grib%sec1%second
          endif
          write(*,'("   Reference Time:                  ",A)') grib%sec1%hdate
          write(*,'("   Production Status:               ",I4)') grib%sec1%production_status
          write(*,'("   Data Type:                       ",I4)') grib%sec1%data_type
       endif

       !
       ! Data Type
       !

       select case (grib%sec1%data_type)
       case (0)
          write(*,'("  Analysis products")')
       case (1)
          write(*,'("  Forecast products")')
       case (2)
          write(*,'("  Analysis and Forecast products")')
       case (3)
          write(*,'("  Control Forecast products")')
       case (4)
          write(*,'("  Perturbed Forecast products")')
       case (5)
          write(*,'("  Control and Perturbed Forecast products")')
       case (6)
          write(*,'("  Processed satellite observations")')
       case (7)
          write(*,'("  Processed radar observations")')
       case (8:191)
          write(*,'("   Data Type:                       ",I4)') grib%sec1%data_type
       case (192:254)
          write(*,'("   Data Type:                       ",I4)') grib%sec1%data_type
       case (255)
          write(*,'("   Data Type:                       ",I4)') grib%sec1%data_type
       end select

       !
       ! Significance of Reference Time
       !
       select case (grib%sec1%srt) 
       case (0)
          write(*,'("  Analysis at time ",A)') grib%sec1%hdate
       case (1)
          write(*,'("  Forecast initialized at time ",A)') grib%sec1%hdate
       case (2)
          write(*,'("  Forecast verifying at time ",A)') grib%sec1%hdate
       case (3)
          write(*,'("  Observation time ",A)') grib%sec1%hdate
       case (4:191)
          write(*,'("  Reference time ",A)') grib%sec1%hdate
       case (192:254)
          write(*,'("  Reference time ",A)') grib%sec1%hdate
       case (255)
          write(*,'("  Reference time ",A)') grib%sec1%hdate
       end select

    endif

  end subroutine print_sec1

!=================================================================================
!=================================================================================

  subroutine unpack_sec3(grib)
    implicit none
    type(GribStruct), intent(inout), target :: grib

    integer :: section
    type (Section3Struct), pointer :: sec3
    type (GT_3_0_Struct),  pointer :: GT_3_0
    type (GT_3_30_Struct), pointer :: GT_3_30

    integer :: i

    ! Temporary integers to hold 
    integer :: ila1
    integer :: ilo1
    integer :: ila2
    integer :: ilo2
    integer :: ilad
    integer :: ilov
    integer :: iplat
    integer :: iplon
    integer :: ilatin1
    integer :: ilatin2
    integer :: idi
    integer :: idj

    sec3 => grib%sec3

    sec3%size = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)

    if (section /= 3) then
       write(*,'("WARNING: Section 3:  We are lost!  ",I4)') section
       stop "FATAL ERROR: In module module_grib2.F -- Section 3: We are lost"
    endif

    sec3%grid_definition_source   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    sec3%number_of_data_points    = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    sec3%octets_for_optional_list = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    sec3%list_interpretation      = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    sec3%grid_template_number     = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)

    select case (sec3%grid_template_number)
    case (0)

       ! Cylindrical Equidistant Grid

       GT_3_0 => sec3%GT_3_0

       Sec3%shape_of_earth                   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_0%scale_factor_of_radius         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_0%scaled_value_of_radius         = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_0%scale_factor_major_axis        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_0%scaled_value_major_axis        = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_0%scale_factor_minor_axis        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_0%scaled_value_minor_axis        = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec3%nx                               = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec3%ny                               = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_0%basic_angle                    = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_0%subdivisions_of_basic_angle    = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       ila1                                  = unpack_signed_integer  (grib%buffer, 4, grib%iskip)
       ilo1                                  = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_0%resolution_and_component_flags = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       ila2                                  = unpack_signed_integer  (grib%buffer, 4, grib%iskip)
       ilo2                                  = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       idi                                   = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       idj                                   = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       GT_3_0%scanning_mode                  = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)

       GT_3_0%la1 = ila1 * 1.E-6
       GT_3_0%lo1 = ilo1 * 1.E-6
       GT_3_0%la2 = ila2 * 1.E-6
       GT_3_0%lo2 = ilo2 * 1.E-6
       GT_3_0%dx  = idi  * 1.E-6
       GT_3_0%dy  = idj  * 1.E-6

       ! 0/1:  I direction increments given?
       if (btest(GT_3_0%resolution_and_component_flags,5)) then
          GT_3_0%i_direction_increments_given = 1
       else
          GT_3_0%i_direction_increments_given = 0
       endif

       ! 0/1:  J direction increments given?
       if (btest(GT_3_0%resolution_and_component_flags,4)) then
          GT_3_0%j_direction_increments_given = 1
       else
          GT_3_0%j_direction_increments_given = 0
       endif

       ! 0/1:  Earth-relative/Grid-relative winds
       if (btest(GT_3_0%resolution_and_component_flags,3)) then
          GT_3_0%winds_grid_relative = 1
       else
          GT_3_0%winds_grid_relative = 0
       endif

       if (btest(GT_3_0%scanning_mode,7)) then
          GT_3_0%i_scan_direction = -1
       else
          GT_3_0%i_scan_direction = 1
       endif

       if (btest(GT_3_0%scanning_mode,6)) then
          GT_3_0%j_scan_direction = 1
       else
          GT_3_0%j_scan_direction = -1
       endif

       if (btest(GT_3_0%scanning_mode,5)) then
          GT_3_0%i_scan_consecutive=0
       else
          GT_3_0%i_scan_consecutive=1
       endif

       if (btest(GT_3_0%scanning_mode,4)) then
          GT_3_0%boustrophedon=1
       else
          GT_3_0%boustrophedon=0
       endif

       nullify(GT_3_0)

    case (30)

       ! Lambert Conformal Grid 

       GT_3_30 => sec3%GT_3_30

       sec3%shape_of_earth                    = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_30%scale_factor_of_radius         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_30%scaled_value_of_radius         = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_30%scale_factor_major_axis        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_30%scaled_value_major_axis        = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       GT_3_30%scale_factor_minor_axis        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_30%scaled_value_minor_axis        = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec3%nx                                = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec3%ny                                = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       ila1                                   = unpack_signed_integer  (grib%buffer, 4, grib%iskip)
       ilo1                                   = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       GT_3_30%resolution_and_component_flags = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       ilad                                   = unpack_signed_integer  (grib%buffer, 4, grib%iskip)     
       ilov                                   = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       idi                                    = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       idj                                    = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       GT_3_30%projection_center_flag         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       GT_3_30%scanning_mode                  = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       ilatin1                                = unpack_signed_integer  (grib%buffer, 4, grib%iskip)     
       ilatin2                                = unpack_signed_integer  (grib%buffer, 4, grib%iskip)     
       iplat                                  = unpack_signed_integer  (grib%buffer, 4, grib%iskip)     
       iplon                                  = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     

       GT_3_30%la1            = ila1    * 1.E-6
       GT_3_30%lo1            = ilo1    * 1.E-6
       GT_3_30%lad            = ilad    * 1.E-6
       GT_3_30%lov            = ilov    * 1.E-6
       GT_3_30%dx             = idi     * 1.E-6
       GT_3_30%dy             = idj     * 1.E-6
       GT_3_30%latin1         = ilatin1 * 1.E-6
       GT_3_30%latin2         = ilatin2 * 1.E-6
       GT_3_30%pole_latitude  = iplat   * 1.E-6
       GT_3_30%pole_longitude = iplon   * 1.E-6

       ! 0/1:  I direction increments given?
       if (btest(GT_3_30%resolution_and_component_flags,5)) then
          GT_3_30%i_direction_increments_given = 1
       else
          GT_3_30%i_direction_increments_given = 0
       endif

       ! 0/1:  J direction increments given?
       if (btest(GT_3_30%resolution_and_component_flags,4)) then
          GT_3_30%j_direction_increments_given = 1
       else
          GT_3_30%j_direction_increments_given = 0
       endif

       ! 0/1:  Earth-relative/Grid-relative winds
       if (btest(GT_3_30%resolution_and_component_flags,3)) then
          GT_3_30%winds_grid_relative = 1
       else
          GT_3_30%winds_grid_relative = 0
       endif

       if (btest(GT_3_30%scanning_mode,7)) then
          GT_3_30%i_scan_direction = -1
       else
          GT_3_30%i_scan_direction = 1
       endif

       if (btest(GT_3_30%scanning_mode,6)) then
          GT_3_30%j_scan_direction = 1
       else
          GT_3_30%j_scan_direction = -1
       endif

       if (btest(GT_3_30%scanning_mode,5)) then
          GT_3_30%i_scan_consecutive=0
       else
          GT_3_30%i_scan_consecutive=1
       endif

       if (btest(GT_3_30%scanning_mode,4)) then
          GT_3_30%boustrophedon=1
       else
          GT_3_30%boustrophedon=0
       endif



       nullify(GT_3_30)

    case default
       write(*,'("WARNING: Unknown grid_template_number: ",I4)') sec3%grid_template_number
       stop "FATAL ERROR: In module module_grib2.F -- Unknown grid_template_number."
    end select

    nullify(sec3)

  end subroutine unpack_sec3

!=================================================================================
!=================================================================================

  subroutine print_sec3(grib, verbosity)
    implicit none
    type(GribStruct), target, intent(in) ::  grib
    integer,          intent(in) :: verbosity

    type(Section3Struct), pointer :: sec3
    sec3 => grib%sec3
    if (verbosity >= 10) then
       write(*,'("Section 3")')
       if (verbosity >= 100) then
          write(*,'("   Length of section:               ",I4)') sec3%size
          write(*,'("   Source of Grid Definition:       ",I4)') sec3%grid_definition_source
          write(*,'("   Number of data points:           ",I8)') sec3%number_of_data_points
          write(*,'("   Octets for optional list:        ",I4)') sec3%octets_for_optional_list
          write(*,'("   Interpretation of optional list: ",I4)') sec3%list_interpretation
          write(*,'("   Grid Definition Template Number: ",I4)') sec3%grid_template_number
          write(*,'("        Shape of earth           : ",I4)') sec3%shape_of_earth
          select case (sec3%shape_of_earth)
          case (6)
             write(*,'("         Earth assumed spherical with radius of 6371229.0 m")')
          case default
             write(*,'("         Unrecognized shape_of_earth:  ", I4)') sec3%shape_of_earth
          end select
          select case (sec3%grid_template_number)
          case (0)
             write(*,'("  Cylindrical Equidistant Grid:")')
             write(*,'("     Scale factor of radius:    ",I8)') sec3%GT_3_0%scale_factor_of_radius
             write(*,'("     Scaled value of radius:    ",I8)') sec3%GT_3_0%scaled_value_of_radius
             write(*,'("     Scale factor major axis:   ",I8)') sec3%GT_3_0%scale_factor_major_axis
             write(*,'("     Scaled value major axis:   ",I8)') sec3%GT_3_0%scaled_value_major_axis
             write(*,'("     Scale factor minor axis:   ",I8)') sec3%GT_3_0%scale_factor_minor_axis
             write(*,'("     Scaled value minor axis:   ",I8)') sec3%GT_3_0%scaled_value_minor_axis
             write(*,'("     Ni                         ",I8)') sec3%nx
             write(*,'("     Nj                         ",I8)') sec3%ny
             write(*,'("     basic angle:               ",I8)') sec3%GT_3_0%basic_angle
             write(*,'("     subdivisions of angle:     ",I8)') sec3%GT_3_0%subdivisions_of_basic_angle
             write(*,'("     La1:                       ",F10.6)') sec3%GT_3_0%la1
             write(*,'("     Lo1:                       ",F10.6)') sec3%GT_3_0%lo1
             write(*,'("     Resolution/Component flags ",I4, 1x, B8.8)') sec3%GT_3_0%resolution_and_component_flags, &
                  sec3%GT_3_0%resolution_and_component_flags
             if (sec3%GT_3_0%i_direction_increments_given == 1) then
                write(*,'("                 I direction increments given")')
             else
                write(*,'("                 I direction increments not given")')
             endif
             if (sec3%GT_3_0%j_direction_increments_given == 1) then
                write(*,'("                 J direction increments given")')
             else
                write(*,'("                 J direction increments not given")')
             endif
             if (sec3%GT_3_0%winds_grid_relative == 1) then
                write(*,'("                 Horizontal wind components are grid-relative")')
             else
                write(*,'("                 Horizontal wind components are earth-relative")')
             endif
             write(*,'("     La2:                       ",F10.6)') sec3%GT_3_0%la2
             write(*,'("     Lo2:                       ",F10.6)') sec3%GT_3_0%lo2
             write(*,'("     Dx :                       ",F10.6)') sec3%GT_3_0%dx
             write(*,'("     Dy:                        ",F10.6)') sec3%GT_3_0%dy
             write(*,'("     Scanning Mode:             ",I4, 1x, B8.8)') sec3%GT_3_0%scanning_mode, sec3%GT_3_0%scanning_mode
             write(*,'("             I scan direction   = ", I4)') sec3%GT_3_0%i_scan_direction
             write(*,'("             J scan direction   = ", I4)') sec3%GT_3_0%j_scan_direction
             write(*,'("             I scan consecutive = ", I4)') sec3%GT_3_0%i_scan_consecutive
             write(*,'("             Boustrophedon      = ", I4)') sec3%GT_3_0%boustrophedon
          case (30)
             write(*,'("  Lambert Conformal Grid: ")')
             write(*,'("     Scale factor of radius:    ",I8)') sec3%GT_3_30%scale_factor_of_radius
             write(*,'("     Scaled value of radius:    ",I8)') sec3%GT_3_30%scaled_value_of_radius
             write(*,'("     Scale factor major axis:   ",I8)') sec3%GT_3_30%scale_factor_major_axis
             write(*,'("     Scaled value major axis:   ",I8)') sec3%GT_3_30%scaled_value_major_axis
             write(*,'("     Scale factor minor axis:   ",I8)') sec3%GT_3_30%scale_factor_minor_axis
             write(*,'("     Scaled value minor axis:   ",I8)') sec3%GT_3_30%scaled_value_minor_axis
             write(*,'("     Ni                         ",I8)') sec3%nx
             write(*,'("     Nj                         ",I8)') sec3%ny
             write(*,'("     La1:                       ",F10.6)') sec3%GT_3_30%la1
             write(*,'("     Lo1:                       ",F10.6)') sec3%GT_3_30%lo1
             write(*,'("     Resolution/Component flags ",I4,": ", B8.8)') sec3%GT_3_30%resolution_and_component_flags, &
                  sec3%GT_3_30%resolution_and_component_flags
             if (sec3%GT_3_30%i_direction_increments_given == 1) then
                write(*,'("                 I direction increments given")')
             else
                write(*,'("                 I direction increments not given")')
             endif
             if (sec3%GT_3_30%j_direction_increments_given == 1) then
                write(*,'("                 J direction increments given")')
             else
                write(*,'("                 J direction increments not given")')
             endif
             if (sec3%GT_3_30%winds_grid_relative == 1) then
                write(*,'("                 Horizontal wind components are grid-relative")')
             else
                write(*,'("                 Horizontal wind components are earth-relative")')
             endif
             write(*,'("     Lad:                       ",F10.6)') sec3%GT_3_30%lad
             write(*,'("     Lov:                       ",F10.6)') sec3%GT_3_30%lov
             write(*,'("     Dx :                       ",F10.6)') sec3%GT_3_30%dx
             write(*,'("     Dy:                        ",F10.6)') sec3%GT_3_30%dy
             write(*,'("     Projection Center Flag     ",I8)') sec3%GT_3_30%projection_center_flag
             write(*,'("     Scanning Mode:             ",I8, ": ", B8.8)') sec3%GT_3_30%scanning_mode, sec3%GT_3_30%scanning_mode
             write(*,'("             I scan direction   = ", I8)') sec3%GT_3_30%i_scan_direction
             write(*,'("             J scan direction   = ", I8)') sec3%GT_3_30%j_scan_direction
             write(*,'("             I scan consecutive = ", I8)') sec3%GT_3_30%i_scan_consecutive
             write(*,'("             Boustrophedon      = ", I8)') sec3%GT_3_30%boustrophedon
             write(*,'("     Latin1:                    ",F10.6)') sec3%GT_3_30%latin1
             write(*,'("     Latin2:                    ",F10.6)') sec3%GT_3_30%latin2
             write(*,'("     Southern Pole Latitude:    ",F10.6)') sec3%GT_3_30%pole_latitude
             write(*,'("     Southern Pole Longitude:   ",F10.6)') sec3%GT_3_30%pole_longitude
          case default
             write(*,'("Unknown grid_template_number: ",I8)') sec3%grid_template_number
             stop
          end select
       endif
    endif

  end subroutine print_sec3

!=================================================================================
!=================================================================================

  subroutine unpack_sec4(grib)
    implicit none
    type (GribStruct), target, intent(inout) :: grib
    integer :: section
    type (Section4Struct), pointer :: sec4
!    type (PDT_4_0_Struct), pointer :: PDT_4_0
    type (PDT_4_8_Struct), pointer :: PDT_4_8

    integer :: time_conversion_to_seconds

    sec4 => grib%sec4

    sec4%size = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)  
    if (section /= 4) then
       write(*,'("WARNING: Section 4:  We are lost!  ", I4)') section
       stop "FATAL ERROR: In module module_grib2.F -- Section 4: Problem"
    endif

    sec4%number_of_coord_values  = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
    sec4%product_template_number = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)

    select case (sec4%product_template_number)
    case (0)

       ! PDT_4_0 => sec4%PDT_4_0

       sec4%parameter_category                   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%parameter_number                     = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%type_of_generating_process        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%background_process_id             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%generating_process_id             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%data_cutoff_hours                 = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
       sec4%data_cutoff_minutes               = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%time_range_indicator              = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%time                              = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec4%ltype1                            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%lscale1                           = unpack_signed_integer(grib%buffer, 1, grib%iskip)
       sec4%lvalue1                           = unpack_signed_integer(grib%buffer, 4, grib%iskip)
       sec4%level1                            = real(sec4%lvalue1) / (10.**real(sec4%lscale1))
       sec4%ltype2                            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%lscale2                           = unpack_signed_integer(grib%buffer, 1, grib%iskip)
       sec4%lvalue2                           = unpack_signed_integer(grib%buffer, 4, grib%iskip)
       sec4%level2                            = real(sec4%lvalue2) / (10.**real(sec4%lscale2))

    case (8)

       PDT_4_8 => sec4%PDT_4_8

       sec4%parameter_category                   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%parameter_number                     = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%type_of_generating_process        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%background_process_id             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%generating_process_id             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%data_cutoff_hours                 = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
       sec4%data_cutoff_minutes               = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%time_range_indicator              = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%time                              = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       sec4%ltype1                            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%lscale1                           = unpack_signed_integer(grib%buffer, 1, grib%iskip)
       sec4%lvalue1                           = unpack_signed_integer(grib%buffer, 4, grib%iskip)
       sec4%level1                            = real(sec4%lvalue1) / (10.**real(sec4%lscale1))
       sec4%ltype2                            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec4%lscale2                           = unpack_signed_integer(grib%buffer, 1, grib%iskip)
       sec4%lvalue2                           = unpack_signed_integer(grib%buffer, 4, grib%iskip)
       sec4%level2                            = real(sec4%lvalue2) / (10.**real(sec4%lscale2))
       PDT_4_8%end_year                          = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
       PDT_4_8%end_month                         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%end_day                           = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%end_hour                          = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%end_minute                        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%end_second                        = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       select case (sec4%time_range_indicator)
       case default
          write(*,'("WARNING: sec4%time_range_indicator = ", I4)') sec4%time_range_indicator
          stop "FATAL ERROR: In module module_grib2.F -- time conversion error"
       case (0)
          time_conversion_to_seconds = 60
       case (1)
          time_conversion_to_seconds = 3600
       case (2)
          time_conversion_to_seconds = 86400
       case (13)
          time_conversion_to_seconds = 1
       end select
       call geth_newdate(PDT_4_8%begin_hdate, grib%sec1%hdate, sec4%time*time_conversion_to_seconds)
       write(PDT_4_8%end_hdate, '(I4.4,2("-",I2.2),"_",2(I2.2,":"),I2.2)') &
            PDT_4_8%end_year, PDT_4_8%end_month, PDT_4_8%end_day, PDT_4_8%end_hour, PDT_4_8%end_minute, PDT_4_8%end_second
       PDT_4_8%number_of_time_range_specifications  = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%number_missing                       = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       PDT_4_8%statistical_process                  = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%type_of_time_increment               = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%time_range_unit                      = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%length_of_time_range                 = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       PDT_4_8%time_increment_unit                  = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       PDT_4_8%time_increment                       = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)

       if (PDT_4_8%number_of_time_range_specifications > 1) then
          write(*,'("WARNING: Hmmmm.  number of time range specifications = ", I8)') PDT_4_8%number_of_time_range_specifications
          stop "FATAL ERROR: In module module_grib2.F -- number of time range specifications"
       endif

    case default
       write(*,'("WARNING: Unrecognized product_template_number: ", I4)') sec4%product_template_number
       stop "FATAL ERROR: In module module_grib2.F -- Unrecognized product_template_number"
    end select

  end subroutine unpack_sec4

!=================================================================================
!=================================================================================

  subroutine print_sec4(grib)
    implicit none
    type (GribStruct), target, intent(in) :: grib
    type (Section4Struct), pointer :: sec4

    sec4 => grib%sec4

    write(*,'("Section 4")')
    write(*,'("   Length of section:                   ",I8)') sec4%size
    write(*,'("   Number of Coordinate Values:         ",I8)') sec4%number_of_coord_values
    write(*,'("   Product Definition Template Number:  ",I8)') sec4%product_template_number
    select case (sec4%product_template_number)
    case (0)
       write(*,'("  PDT 4.0:  Analysis or fcst at a horiz lvl or lyr at a point in time")')
       write(*,'("      parameter category:           ",I8)') sec4%parameter_category
       write(*,'("      parameter number:             ",I8)') sec4%parameter_number
       write(*,'("      type of generatiing process:  ",I8)') sec4%type_of_generating_process
       write(*,'("      background process id:        ",I8)') sec4%background_process_id
       write(*,'("      generating process id:        ",I8)') sec4%generating_process_id
       write(*,'("      data cutoff hours:            ",I8)') sec4%data_cutoff_hours
       write(*,'("      data cutoff minutes:          ",I8)') sec4%data_cutoff_minutes
       write(*,'("      Time Range Indicator:         ",I8)') sec4%time_range_indicator
       write(*,'("      time:                         ",I8)') sec4%time
       write(*,'("      type of first fixed surface:  ",I8)') sec4%ltype1
       write(*,'("      scale factor of first surface:  ",I8)') sec4%lscale1
       write(*,'("      scaled value of first surface:  ",I8)') sec4%lvalue1
       write(*,'("      First level                    ",F20.4)') sec4%level1
       if (sec4%ltype2 /= 255) then
          write(*,'("      type of second fixed surface:  ",I8)') sec4%ltype2
          write(*,'("      scale factor of second surface:  ",I8)') sec4%lscale2
          write(*,'("      scaled value of second surface:  ",I8)') sec4%lvalue2
          write(*,'("      Second level                   ",F8.2)') sec4%level2
       endif
    case (8)
       write(*,'("  PDT 4.8:  Average, accumulation ....")')
       write(*,'("      parameter category:           ",I8)') sec4%parameter_category
       write(*,'("      parameter number:             ",I8)') sec4%parameter_number
       write(*,'("      type of generatiing process:  ",I8)') sec4%type_of_generating_process
       write(*,'("      background process id:        ",I8)') sec4%background_process_id
       write(*,'("      generating process id:        ",I8)') sec4%generating_process_id
       write(*,'("      data cutoff hours:            ",I8)') sec4%data_cutoff_hours
       write(*,'("      data cutoff minutes:          ",I8)') sec4%data_cutoff_minutes
       write(*,'("      Time Range Indicator:         ",I8)') sec4%time_range_indicator
       write(*,'("      time:                         ",I8)') sec4%time
       write(*,'("      type of first fixed surface:  ",I8)') sec4%ltype1
       write(*,'("      scale factor of first surface:  ",I8)') sec4%lscale1
       write(*,'("      scaled value of first surface:  ",I8)') sec4%lvalue1
       write(*,'("      First level                    ",F10.2)') sec4%level1
       if (sec4%ltype2 /= 255) then
          write(*,'("      type of second fixed surface:  ",I8)') sec4%ltype2
          write(*,'("      scale factor of second surface:  ",I8)') sec4%lscale2
          write(*,'("      scaled value of second surface:  ",I8)') sec4%lvalue2
          write(*,'("      Second level                  ",F10.2)') sec4%level2
       endif
       write(*,'("      End Year:                        ",I8)') sec4%PDT_4_8%end_year
       write(*,'("      End Month:                       ",I8)') sec4%PDT_4_8%end_month
       write(*,'("      End Day:                         ",I8)') sec4%PDT_4_8%end_day
       write(*,'("      End Hour:                        ",I8)') sec4%PDT_4_8%end_hour
       write(*,'("      End Minute:                      ",I8)') sec4%PDT_4_8%end_minute
       write(*,'("      End Second:                      ",I8)') sec4%PDT_4_8%end_second
       write(*,'("      Ene Hdate:                       ", A)') sec4%PDT_4_8%end_hdate
       write(*,'("      Number of time range specifications: ",I8)') sec4%PDT_4_8%number_of_time_range_specifications
       write(*,'("      Number missing:                  ",I8)') sec4%PDT_4_8%number_missing
       write(*,'("      Statistical Process:              ",I8)') sec4%PDT_4_8%statistical_process
       write(*,'("      Type of time increment:           ",I8)') sec4%PDT_4_8%type_of_time_increment
       write(*,'("      time range unit:                  ",I8)') sec4%PDT_4_8%time_range_unit
       write(*,'("      length of time range:             ",I8)') sec4%PDT_4_8%length_of_time_range
       write(*,'("      time increment unit:              ",I8)') sec4%PDT_4_8%time_increment_unit
       write(*,'("      time increment:                   ",I8)') sec4%PDT_4_8%time_increment
    case default
       write(*,'("WARNING: Unrecognized product_template_number: ",I8)') sec4%product_template_number
       stop "FATAL ERROR: Unrecognized product_template_number."
    end select
  end subroutine print_sec4

!=================================================================================
!=================================================================================

  subroutine unpack_sec5(grib)
    implicit none
    type (GribStruct), target, intent(inout) :: grib
    type (Section5Struct), pointer :: sec5
    integer :: section
    integer :: isign
    integer :: iref
    integer :: iref40
    real    :: xref40
    integer :: iref2
    real    :: xref2
    equivalence (iref40, xref40)
    equivalence (iref2, xref2)

    sec5 => grib%sec5

    sec5%size = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    if (section /= 5) then
       write(*,'("WARNING: Section 5:  We are lost!  ", I8)') section
       stop "FATAL ERROR: In module module_grib2.F -- Section 5 Problem"
    endif

    sec5%nval                 = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    sec5%data_template_number = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)
    select case (sec5%data_template_number)
    case (0)
       iref = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
       write(*, '("iref = ", I16)') iref

       sec5%DRT_5_0%binary_scale_factor  = unpack_signed_integer(grib%buffer, 2, grib%iskip)
       sec5%DRT_5_0%decimal_scale_factor = unpack_signed_integer(grib%buffer, 2, grib%iskip)
       sec5%DRT_5_0%nbits                = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec5%DRT_5_0%data_type            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       stop "Problem"

    case (2)
       iref2                             = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%reference_value      = xref2
       sec5%DRT_5_2%binary_scale_factor  = unpack_signed_integer  (grib%buffer, 2, grib%iskip)
       sec5%DRT_5_2%decimal_scale_factor = unpack_signed_integer  (grib%buffer, 2, grib%iskip)
       sec5%DRT_5_2%nbits                = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec5%DRT_5_2%data_type            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%group_splitting_method = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%missing_value_management = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%substitute1              = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%substitute2              = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%ng                       = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%widths_reference         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%widths_nbits             = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%lengths_reference        = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%length_increment         = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     
       sec5%DRT_5_2%length_of_last_group     = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_2%lengths_nbits            = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)     

    case (40)

       iref40 = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)     
       sec5%DRT_5_40%reference_value = xref40

       sec5%DRT_5_40%binary_scale_factor = unpack_signed_integer(grib%buffer, 2, grib%iskip)
       sec5%DRT_5_40%decimal_scale_factor = unpack_signed_integer(grib%buffer, 2, grib%iskip)
       sec5%DRT_5_40%nbits = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec5%DRT_5_40%data_type = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec5%DRT_5_40%compression_type = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
       sec5%DRT_5_40%target_compression_ratio = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    case default
       write(*,'("WARNING: Unrecognized Data Representation Template Number: ", I8)') sec5%data_template_number
       stop "FATAL ERROR: In module module_grib2.F -- Unrecognized Data Representation Template Number"
    end select

  end subroutine unpack_sec5

!=================================================================================
!=================================================================================

  subroutine print_sec5(grib)
    implicit none
    type (GribStruct), target, intent(in) :: grib
    type (Section5Struct), pointer :: sec5
    sec5 => grib%sec5
    write(*,'("Section 5")')
    write(*,'("   Length of section:                   ",I8)') sec5%size
    write(*,'("   Number of data points:               ",I8)') sec5%nval
    write(*,'("   Data Representation Template Number: ",I8)') sec5%data_template_number
    select case (sec5%data_template_number)
    case (2)


       write(*,'("     reference value:               ", G20.8)') sec5%DRT_5_2%reference_value
       write(*,'("     binary scale factor:           ", I8)') sec5%DRT_5_2%binary_scale_factor
       write(*,'("     decimal scale factor           ", I8)') sec5%DRT_5_2%decimal_scale_factor
       write(*,'("     nbits:                         ", I8)') sec5%DRT_5_2%nbits
       write(*,'("     data type:                     ", I8)') sec5%DRT_5_2%data_type
       write(*,'("     group splitting method         ", I8)') sec5%DRT_5_2%group_splitting_method
       write(*,'("     missing value management       ", I8)') sec5%DRT_5_2%missing_value_management
       write(*,'("     primary substitute:            ", I8)') sec5%DRT_5_2%substitute1
       write(*,'("     secondary substitute:          ", I8)') sec5%DRT_5_2%substitute2
       write(*,'("     NG                             ", I8)') sec5%DRT_5_2%ng
       write(*,'("     widths_reference               ", I8)') sec5%DRT_5_2%widths_reference
       write(*,'("     widths_nbits                   ", I8)') sec5%DRT_5_2%widths_nbits
       write(*,'("     lengths_reference              ", I8)') sec5%DRT_5_2%lengths_reference
       write(*,'("     length increment               ", I8)') sec5%DRT_5_2%length_increment
       write(*,'("     last length                    ", I8)') sec5%DRT_5_2%length_of_last_group
       write(*,'("     lengths_nbits                  ", I8)') sec5%DRT_5_2%lengths_nbits


    case (40)
       write(*,'("     reference value:               ", G20.8)') sec5%DRT_5_40%reference_value
       write(*,'("     binary scale factor:           ", I8)') sec5%DRT_5_40%binary_scale_factor
       write(*,'("     decimal scale factor:          ", I8)') sec5%DRT_5_40%decimal_scale_factor
       write(*,'("     nbits:                         ", I8)') sec5%DRT_5_40%nbits
       write(*,'("     data type:                     ", I8)') sec5%DRT_5_40%data_type
       write(*,'("     compression type:              ", I8)') sec5%DRT_5_40%compression_type
       write(*,'("     target compression ratio:      ", I8)') sec5%DRT_5_40%target_compression_ratio
    case default
       write(*,'("WARNING: Unrecognized Data Representation Template Number: ",I8)') sec5%data_template_number
       stop "FATAL ERROR: In module in module_grib2.F -- Problem"
    end select
  end subroutine print_sec5

!=================================================================================
!=================================================================================

  subroutine unpack_sec6(grib)
    implicit none
    type (GribStruct), target, intent(inout) :: grib

    integer :: section
    integer :: section_start

    type (Section6Struct), pointer :: sec6


    section_start = grib%iskip

    sec6 => grib%sec6

    sec6%size = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)  
    if (section /= 6) then
       write(*,'("WARNING: Section 6:  We are lost!  ", I8)') section
       stop "FATAL ERROR: In module module_grib2.F -- Section 6"
    endif

    sec6%bit_map_indicator = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)  
    select case (sec6%bit_map_indicator)
    case default
       write(*,'("WARNING: Oooooh, bit mapped field:  ",I8)')  sec6%bit_map_indicator
       stop "FATAL ERROR: In module module_grib2.F -- Unrecognized bit_map_indicator"
    case (255)
       ! No bitmap.
       ! No action needed here.
    case (254)
       ! Bitmap previously defined in this GRIB record. 
       ! No action needed here.  The previously-defined bitmap
       ! should still be stored and used as necessary.
    case (0)
       ! write(*,'("Read a new bitmap:  ", I8, I8)') sec6%bit_map_indicator, sec6%size-6
       if (associated(grib%bitmap)) then
          deallocate(grib%bitmap)
          nullify(grib%bitmap)
       endif
       allocate(grib%bitmap(grib%sec3%nx, grib%sec3%ny))
       call gbytes(grib%buffer, grib%bitmap, grib%iskip, 1, 0, grib%sec3%number_of_data_points)
       ! grib%iskip = grib%iskip + grib%sec3%number_of_data_points
    end select

    ! Position ourselves at the end of the section.
    grib%iskip = section_start + (sec6%size*8)

  end subroutine unpack_sec6

!=================================================================================
!=================================================================================

  subroutine print_sec6(grib, verbosity)
    implicit none
    type (GribStruct), intent(in), target :: grib
    integer,           intent(in)         :: verbosity

    type (Section6Struct), pointer :: sec6
    sec6 => grib%sec6
    if (verbosity >= 10) then
       write(*,'("Section 6")')
       if (verbosity >= 100) then
          write(*,'("   Length of section:                   ",I8)') sec6%size
          write(*,'("   Bit Map Indicator:                   ",I8)') sec6%bit_map_indicator
          select case (sec6%bit_map_indicator)
          case default
             write(*,'("Oooooh, bit mapped field:  ",I8)') sec6%bit_map_indicator
          case (0)
             write(*,'("      A bit map applies and is defined here.")')
             ! write(*,'("      Bitmap of length: ",I8)') size(sec6%packed_bitmap)
          case (254)
             write(*,'("      A previously-defined bit map applies.")')
          case (255)
             write(*,'("      A bit map does not apply")')
          end select
       endif
    endif
  end subroutine print_sec6

!=================================================================================
!=================================================================================

  subroutine unpack_sec7(grib)
    implicit none
    integer ::  section
    type (GribStruct), target, intent(inout) :: grib
    type (Section5Struct), pointer :: sec5
    type (Section7Struct), pointer :: sec7

    integer, pointer, dimension(:)  :: decoded
    integer, allocatable, dimension(:) :: group
    integer :: decoded_length
    integer :: i
    integer :: j
    integer :: k
    integer :: n
    integer :: allostat
    integer ::  ng
    integer :: nbits
    integer :: tot
    integer, allocatable, dimension(:) :: x1
    integer, allocatable, dimension(:) :: widths
    integer, allocatable, dimension(:) :: lengths
    integer, allocatable, dimension(:) :: L

    integer :: sec7_beginning

    sec7_beginning = grib%iskip/8

    sec7 => grib%sec7
    sec5 => grib%sec5

    sec7%size = unpack_unsigned_integer(grib%buffer, 4, grib%iskip)
    section   = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    if (section /= 7) then
       write(*,'("WARNING: Section 7:  We are lost!  ", I8)') section
       stop "FATAL ERROR: In module module_grib2.F -- Section 7"
    endif

    select case (sec5%data_template_number)
    case default
       write(*, '("WARNING: Unrecognized Data Representation Template Number: ", I8)') sec5%data_template_number
       stop "FATAL ERROR: In module module_grib2.F -- Unrecognized Data Representation Template Number"
    case (2)
       ! Grid-point data -- complex packing
       ng = sec5%DRT_5_2%ng
       nbits = sec5%DRT_5_2%nbits

       allocate(x1(ng))
       call gbytes(grib%buffer, x1, grib%iskip, nbits, 0, ng)
       grib%iskip = grib%iskip + (nbits*ng)

       ! Make sure we align at a byte boundary
       if (mod(grib%iskip,8)>0) then
          grib%iskip = grib%iskip + 8-mod(grib%iskip,8)
       endif

       allocate(widths(ng))
       call gbytes(grib%buffer, widths, grib%iskip, sec5%DRT_5_2%widths_nbits, 0, ng)
       grib%iskip = grib%iskip + (sec5%DRT_5_2%widths_nbits*ng)

       ! Make sure we align at a byte boundary
       if (mod(grib%iskip,8)>0) then
          grib%iskip = grib%iskip + 8-mod(grib%iskip,8)
       endif

       allocate(lengths(ng))
       call gbytes(grib%buffer, lengths, grib%iskip, sec5%DRT_5_2%lengths_nbits, 0, ng)
       grib%iskip = grib%iskip + (sec5%DRT_5_2%lengths_nbits *  ng)

       ! Make sure we align at a byte boundary
       if (mod(grib%iskip,8)>0) then
          grib%iskip = grib%iskip + 8-mod(grib%iskip,8)
       endif

       allocate(decoded(sec5%nval))
       if (associated(sec7%floated)) then
          deallocate(sec7%floated)
          nullify(sec7%floated)
       endif
       allocate(sec7%floated(sec5%nval))
       allocate(L(ng))
       tot = 0
       j = 1
       do i = 1, ng
          L(i) = sec5%DRT_5_2%lengths_reference + lengths(i) * sec5%DRT_5_2%length_increment
          if (i==ng) L(i) = sec5%DRT_5_2%length_of_last_group
          tot = tot + (widths(i)*L(i))
          ! write(*, '("i, x1, widths, lengths, L = ", 10I)') i, x1(i), widths(i), lengths(i), L(i), tot, tot/8

          ! For each group <i>, read <L> values of size <widths> bits.
          if (widths(i) > 0) then
             call gbytes(grib%buffer, decoded(j), grib%iskip, widths(i), 0, L(i))
             grib%iskip = grib%iskip + widths(i)*L(i)
             do k = 0, L(i)-1
                sec7%floated(j+k) = sec5%DRT_5_2%reference_value + (real(x1(i) + decoded(j+k)) * real(2**sec5%DRT_5_2%binary_scale_factor))
                sec7%floated(j+k) = sec7%floated(j+k) / real(10**(sec5%DRT_5_2%decimal_scale_factor))
             enddo
          endif
          j = j + L(i)
       enddo
       deallocate(x1)
       deallocate(lengths)
       deallocate(widths)
       deallocate(decoded)
       nullify(decoded)
       deallocate(L)

    case (40)

       ! Grid-point data -- JPEG 2000

       if (sec5%DRT_5_40%nbits == 0) then
          if (associated(sec7%floated)) then
             deallocate(sec7%floated)
             nullify(sec7%floated)
          endif
          allocate(sec7%floated(grib%sec3%nx * grib%sec3%ny), stat=allostat)
          if (allostat /= 0) stop "FATAL ERROR: In module module_grib2.F -- Allocation problem 1"
          do i = 1, ( grib%sec3%nx * grib%sec3%ny )
             sec7%floated(i) = sec5%DRT_5_40%reference_value * (10.0 ** (-sec5%DRT_5_40%decimal_scale_factor))
          enddo

       else

          ! Hack:  Unswap our previously swapped bytes.
          ! Need to find a better way of handling swapping
          call swap4f(grib%buffer, size(grib%buffer))

          grib%iskip = grib%iskip + (sec7%size-5)*8

          ! call fortran_decode_jpeg2000(grib%buffer(6:), sec7%size-5, decoded)
          call fortran_decode_jpeg2000(grib%buffer(sec7_beginning+6:), sec7%size-5, decoded)

          if (.not. associated(decoded)) then
             write(*,'("Problem decoding jpeg2000.")')
             ! stop "Problem"
          else
             if (associated(sec7%floated)) then
                deallocate(sec7%floated)
                nullify(sec7%floated)
             endif
             allocate(sec7%floated(size(decoded)))
             do i=1,  size(decoded)
                sec7%floated(i) = sec5%DRT_5_40%reference_value + decoded(i) * 2.0 ** real(sec5%DRT_5_40%binary_scale_factor)
                sec7%floated(i) = sec7%floated(i) * 10.0 ** real(-sec5%DRT_5_40%decimal_scale_factor)
             enddo
             deallocate(decoded)
             nullify(decoded)
          endif
          ! Reswap
          call swap4f(grib%buffer, size(grib%buffer))
       endif

    end select

    ! Build the full 2-d array, unpacking from the bitmap if necessary

    if (associated(grib%array)) then
       deallocate(grib%array)
       nullify(grib%array)
    endif
    allocate(grib%array(grib%sec3%nx, grib%sec3%ny))
    grib%array = 0.0

    if (grib%sec6%bit_map_indicator == 255) then
       do i = 1, grib%sec3%nx
          do j = 1, grib%sec3%ny
             n = (j-1)*grib%sec3%nx + i
             grib%array(i,j) = sec7%floated(n)
          enddo
       enddo
    else
       n = 0
       do j = 1, grib%sec3%ny
          do i = 1, grib%sec3%nx
             if (grib%bitmap(i,j)==1) then
                n = n + 1
                grib%array(i,j) = sec7%floated(n)
             endif
          enddo
       enddo
    endif

    ! Position ourselves at the end of the section.
    grib%iskip = (sec7_beginning + sec7%size) * 8
  end subroutine unpack_sec7

!=================================================================================
!=================================================================================

  subroutine print_sec7(grib)
    implicit none
    type (GribStruct), target, intent(in) :: grib
    type (Section7Struct), pointer :: sec7
    sec7 => grib%sec7
    write(*,'("Section 7")')
    write(*,'("   Length of section:                   ",I8)') sec7%size
    if (associated(sec7%floated)) then
       write(*,'("   First value =                        ", F15.8)') sec7%floated(1)
    endif
  end subroutine print_sec7

!=================================================================================
!=================================================================================

end module module_grib2
