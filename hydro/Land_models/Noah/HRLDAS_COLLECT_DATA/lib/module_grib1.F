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

module module_grib1
  use module_mapinfo
  use kwm_date_utilities
  use module_grib_common
  implicit none

  type grib1_parameter_table_entry
     character(len=256) :: name
     character(len=64)  :: units
     character(len=16)  :: abbr
  end type grib1_parameter_table_entry
  type(grib1_parameter_table_entry), dimension(1:255) :: grib1_parameter_table




contains

!=================================================================================
!=================================================================================

  subroutine grib1_parameter_text_information(sec1, name, units, description)
    implicit none
    ! Need to read tables and get information from stored table data
    type (G1Section1Struct), intent(in)  :: sec1
    character(len=64),       intent(out) :: name
    character(len=64),       intent(out) :: units
    character(len=64),       intent(out) :: description

    name = trim(grib1_parameter_table(sec1%parameter)%abbr)
    units = trim(grib1_parameter_table(sec1%parameter)%units)
    description = trim(grib1_parameter_table(sec1%parameter)%name)
    return

  end subroutine grib1_parameter_text_information

!=================================================================================
!=================================================================================
  
  subroutine grib1_time_information(grib, reference_date, valid_date, process, processing, p1_seconds, p2_seconds)
    implicit none
    type (GribStruct),  intent(in)  :: grib
    character(len=19), intent(out)  :: reference_date
    character(len=19), intent(out)  :: valid_date
    character(len=256), intent(out) :: process
    character(len=256), intent(out) :: processing
    integer,            intent(out) :: p1_seconds
    integer,            intent(out) :: p2_seconds

    reference_date = grib%g1sec1%hdate

    call grib1_valid_date(grib%g1sec1, valid_date)

    select case (grib%g1sec1%tri) ! Time Range Indicator
    case default
       write(process,'("MODULE_GRIB1:  GRIB1_TIME_INFORMATION:  Unrecognized GRIB1 Time Range Indicator ", I10)') grib%g1sec1%tri
       write(processing,'("MODULE_GRIB1:  GRIB1_TIME_INFORMATION:  Unrecognized GRIB1 Time Range Indicator ", I10)') grib%g1sec1%tri
       print*, trim(process)
       stop
    case (0)
       process = "Forecast product"
       processing = "Forecast product"
    case (1,10)
       process = "Analysis product"
       processing = "Analysis product"
    case (3)
       process = "Average product"
       processing = "Average product"
    case (4)
       process = "Accumulation"
       processing = "Accumulation"
    end select

    select case (grib%g1sec1%ftu) ! Forecast Time Units
    case default
       write(*,'("Unrecognized GRIB1 forecast time units:  ",I10)') grib%g1sec1%ftu
       stop
    case (0) ! Minute
       p1_seconds = grib%g1sec1%p1 * 60
       p2_seconds = grib%g1sec1%p2 * 60
    case (1) ! Hour
       p1_seconds = grib%g1sec1%p1 * 3600
       p2_seconds = grib%g1sec1%p2 * 3600
    end select

  end subroutine grib1_time_information

!=================================================================================
!=================================================================================

  subroutine grib1_level_information(grib, level_type, level_units, level_value, level2_value)
    implicit none
    type (GribStruct),  intent(in)  :: grib
    character(len=256), intent(out) :: level_type
    character(len=256), intent(out) :: level_units
    real,               intent(out) :: level_value
    real,               intent(out) :: level2_value

    level_value = -1.E36
    level2_value = -1.E36
    level_units = " "
    select case (grib%g1sec1%leveltyp)
    case default
       write(level_type, '("Unrecognized level type:  ", I10)') grib%g1sec1%leveltyp
    case (1)
       level_type = "Ground or water surface"
    case (2)
       level_type = "Cloud base level"
    case (3)
       level_type = "Cloud top level"
    case (4)
       level_type = "Level of 0{o} C isotherm"
    case (5)
       level_type = "Level of adiabatic condensation lifted from surface"
    case (6)
       level_type = "Maximum wind level"
    case (7)
       level_type = "Tropopause"
    case (8)
       level_type = "Nominal top of atmosphere"
    case (9)
       level_type = "Sea Bottom"
    case (20)
       level_type = "Isothermal level"
       level_units = "K"
       level_value = grib%g1sec1%levelval 
    case (100)
       level_type = "Isobaric level"
       level_units = "Pa"
       level_value = grib%g1sec1%levelval
    case (101)
       level_type = "Layer between two isobaric levels"
       level_units = "kPa"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (102)
       level_type = "Mean Sea Level"
       level_units = "MSL"
    case (103)
       level_type = "Specified altitude above MSL"
       level_units = "m"
       level_value = grib%g1sec1%levelval
    case (104)
       level_type = "Layer between two specified altitudes above MSL"
       level_units = "hm"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (105)
       level_type = "Specified height level above ground"
       level_units = "m"
       level_value = grib%g1sec1%levelval
    case (106)
       level_type = "Layer between two specified height levels above ground"
       level_units = "hm"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (107)
       level_type = "Sigma level"
       level_units = "Sigma value"
       level_value = grib%g1sec1%levelval
    case (108)
       level_type = "Layer between two sigma levels"
       level_units = "Sigma value"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (109)
       level_type = "Hybrid level"
       level_units = "Level number"
       level_value = grib%g1sec1%levelval
    case (110)
       level_type = "Layer between two hybrid levels"
       level_units = "Level number"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (111)
       level_type = "Depth below land surface"
       level_units = "m" ! Original data has cm, we convert to m
       level_value = grib%g1sec1%levelval * 1.E-2 ! Converted to m
    case (112)
       level_type = "Layer between two depths below land surface"
       level_units = "m"! Original data has cm, we convert to m
       level_value = grib%g1sec1%levelval * 1.E-2   ! Converted to m
       level2_value = grib%g1sec1%level2val * 1.E-2 ! Converted to m
    case (113)
       level_type = "Isentropic level"
       level_units = "K"
       level_value = grib%g1sec1%levelval
    case (114)
       level_type = "Layer between two isentropic levels"
       level_units = "K"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (115)
       level_type = "Level at specified pressure difference from ground"
       level_units = "hPa"
       level_value = grib%g1sec1%levelval
    case (116)
       level_type = "Layer between two levels at specified pressure differences from ground"
       level_units = "hPa"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (117)
       level_type = "PV Surface"
       level_units = "10{-6} K m{2} kg{-1} s{-1}"
       level_value = grib%g1sec1%levelval
    case (119)
       level_type = "Eta level"
       level_units = "Eta Value"
       level_value = grib%g1sec1%levelval
    case (120)
       level_type = "Layer between two Eta levels"
       level_units = "Eta Value"
       level_value = grib%g1sec1%levelval
       level2_value = grib%g1sec1%level2val
    case (200)
       level_type = "Entire atmosphere as a single column"
    case (204)
       level_type = "Highest tropospheric freezing level"
    case (209)
       level_type = "Boundary-layer cloud bottom level"
    case (210)
       level_type = "Boundary-layer cloud top level"
    case (211)
       level_type = "Boundary-layer cloud layer"
    case (212)
       level_type = "Low cloud bottom level"
    case (213)
       level_type = "Low cloud top level"
    case (214)
       level_type = "Low cloud layer"
    case (222)
       level_type = "Middle cloud bottom level"
    case (223)
       level_type = "Middle cloud top level"
    case (224)
       level_type = "Middle cloud layer"
    case (232)
       level_type = "High cloud bottom level"
    case (233)
       level_type = "High cloud top level"
    case (234)
       level_type = "High cloud layer"
    case (242)
       level_type = "Convective cloud bottom level"
    case (243)
       level_type = "Convective cloud top level"
    case (244)
       level_type = "Convective cloud layer"
    end select
    
  end subroutine grib1_level_information

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec0(buffer, buffsize, iskip)
    implicit none
    integer,                               intent(in)    :: buffsize
    character(len=1), dimension(buffsize), intent(in)    :: buffer
    integer,                               intent(inout) :: iskip
    iskip = iskip + 8*8
  end subroutine grib1_unpack_sec0

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec1(buffer, buffsize, iskip, sec1)
    implicit none
    integer,                               intent(in)    :: buffsize
    character(len=1), dimension(buffsize), intent(in)    :: buffer
    integer,                               intent(inout) :: iskip
    type (G1Section1Struct),               intent(out)   :: sec1

    integer :: yy
    integer :: century
    integer :: gdsbms

    sec1%isize      = unpack_unsigned_integer(buffer, 3, iskip)
    sec1%ptvn       = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%center     = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%process    = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%grid       = unpack_unsigned_integer(buffer, 1, iskip)
    gdsbms          = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%parameter  = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%leveltyp   = unpack_unsigned_integer(buffer, 1, iskip)
    select case (sec1%leveltyp)
    case default
       sec1%levelval   = unpack_unsigned_integer(buffer, 2, iskip)
       sec1%level2val  = -1.E36
    case (107)
       sec1%levelval   = 1.E-5 * unpack_unsigned_integer(buffer, 2, iskip) ! Sigma value conversion
       sec1%level2val  = -1.E36
    case (20)
       sec1%levelval   = 1.E-2 * unpack_unsigned_integer(buffer, 2, iskip)
       sec1%level2val  = -1.E36
    case (108)
       sec1%levelval   = 1.E-2 * unpack_unsigned_integer(buffer, 1, iskip)
       sec1%level2val  = 1.E-2 * unpack_unsigned_integer(buffer, 1, iskip)
    case (119)
       sec1%levelval   = 1.E-5 * unpack_unsigned_integer(buffer, 2, iskip) ! Eta value conversion
       sec1%level2val  = -1.E36
    case (120)
       sec1%levelval   = 1.E-2 * unpack_unsigned_integer(buffer, 1, iskip) ! Eta value conversion
       sec1%level2val  = 1.E-2 * unpack_unsigned_integer(buffer, 1, iskip) ! Eta value conversion
    case (101,102:104,106,110,112,114,116,121,128,141)
       sec1%levelval   = unpack_unsigned_integer(buffer, 1, iskip)
       sec1%level2val  = unpack_unsigned_integer(buffer, 1, iskip)
    end select
    yy              = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%month      = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%day        = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%hour       = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%minute     = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%ftu        = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%p1         = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%p2         = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%tri        = unpack_unsigned_integer(buffer, 1, iskip)
    if ( sec1%tri == 10 ) then
       ! The P1 value extends over two bytes.  P2 is not in the header.
       sec1%p1 = ( sec1%p1 * 256 ) + sec1%p2
       sec1%p2 = 0
    endif
    sec1%navg       = unpack_unsigned_integer(buffer, 2, iskip)
    sec1%nmissavg   = unpack_unsigned_integer(buffer, 1, iskip)
    century         = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%subcenter  = unpack_unsigned_integer(buffer, 1, iskip)
    sec1%decimal_scale_factor  = unpack_signed_integer(buffer, 2, iskip)

    select case (gdsbms)
    case default
       write(*,'("GRIB1_UNPACK_SEC1:  GRIB1 Section 1:  Unrecognized GDS/BMS flag: ", I10)') gdsbms
       stop
    case (0)
       sec1%ifgds = .FALSE.
       sec1%ifbms = .FALSE.
    case (64)
       sec1%ifgds = .FALSE.
       sec1%ifbms = .TRUE.
    case (128)
       sec1%ifgds = .TRUE.
       sec1%ifbms = .FALSE.
    case (192)
       sec1%ifgds = .TRUE.
       sec1%ifbms = .TRUE.
    end select

    if ( yy > 0 ) then
       sec1%year       = (century-1) * 100 + yy
    else
       sec1%year       = century * 100
    endif

    write(sec1%hdate, '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)') &
         sec1%year, sec1%month, sec1%day, sec1%hour, sec1%minute, 00

    return
  end subroutine grib1_unpack_sec1

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec2(buffer, buffsize, iskip, sec2)
    implicit none

    integer,                               intent(in)    :: buffsize
    character(len=1), dimension(buffsize), intent(in)    :: buffer
    integer,                               intent(inout) :: iskip
    type (G1Section2Struct),               intent(inout) :: sec2
    integer :: idum

    sec2%isize      = unpack_unsigned_integer(buffer, 3, iskip)
    sec2%nv         = unpack_unsigned_integer(buffer, 1, iskip)
    sec2%pv         = unpack_unsigned_integer(buffer, 1, iskip)
    sec2%drt        = unpack_unsigned_integer(buffer, 1, iskip)

    select case (sec2%drt)
    case (0) ! Lat/Lon grid

       sec2%nx = unpack_unsigned_integer(buffer, 2, iskip)
       sec2%ny = unpack_unsigned_integer(buffer, 2, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lat1 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lon1 = real(idum)*1.E-3
       sec2%rac = unpack_unsigned_integer(buffer, 1, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lat2 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lon2 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 2, iskip)
       sec2%dx = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 2, iskip)
       sec2%dy = real(idum)*1.E-3
       sec2%scanning_mode = unpack_unsigned_integer(buffer, 1, iskip)

       if ( btest(sec2%scanning_mode, 7) ) then
          sec2%i_scan_direction = -1
       else
          sec2%i_scan_direction = 1
       endif

       if ( btest(sec2%scanning_mode, 6) ) then
          sec2%j_scan_direction = 1
       else
          sec2%j_scan_direction = -1
       endif

       if ( btest(sec2%scanning_mode, 5) ) then
          sec2%orientation = -1
       else
          sec2%orientation = 1
       endif
       
       ! Skip those last four bytes:
       iskip = iskip + (4*8)

       sec2%latin1 = -1.E36
       sec2%latin2 = -1.E36

    case (3) ! Lambert Conformal grid

       sec2%nx = unpack_unsigned_integer(buffer, 2, iskip)
       sec2%ny = unpack_unsigned_integer(buffer, 2, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lat1 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lon1 = real(idum)*1.E-3
       sec2%rac = unpack_unsigned_integer(buffer, 1, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lov = real(idum)*1.E-3
       idum = unpack_unsigned_integer(buffer, 3, iskip)
       sec2%dx = real(idum)*1.E-3
       idum = unpack_unsigned_integer(buffer, 3, iskip)
       sec2%dy = real(idum)*1.E-3
       sec2%center = unpack_unsigned_integer(buffer, 1, iskip)
       sec2%scanning_mode = unpack_unsigned_integer(buffer, 1, iskip)

       if ( btest(sec2%scanning_mode, 7) ) then
          sec2%i_scan_direction = -1
       else
          sec2%i_scan_direction = 1
       endif

       if ( btest(sec2%scanning_mode, 6) ) then
          sec2%j_scan_direction = 1
       else
          sec2%j_scan_direction = -1
       endif
       
       if ( btest(sec2%scanning_mode, 5) ) then
          sec2%orientation = -1
       else
          sec2%orientation = 1
       endif
       
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%latin1 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%latin2 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%polelat = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%polelon = real(idum)*1.E-3
       ! Skip those last two bytes:
       iskip = iskip + 16
       ! print*, 'nx, ny = ', sec2%nx, sec2%ny
       ! print*, 'lat1, lon1 = ', sec2%lat1, sec2%lon1
       ! print*, 'lov, truelat1, truelat2 = ', sec2%lov, sec2%latin1, sec2%latin2
       ! print*, 'dx, dy = ', sec2%dx, sec2%dy

    case (5) ! Polar Stereographic grid

       sec2%nx = unpack_unsigned_integer(buffer, 2, iskip)
       sec2%ny = unpack_unsigned_integer(buffer, 2, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lat1 = real(idum)*1.E-3
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lon1 = real(idum)*1.E-3
       sec2%rac = unpack_unsigned_integer(buffer, 1, iskip)
       idum          = unpack_signed_integer(buffer, 3, iskip)
       sec2%lov = real(idum)*1.E-3
       idum = unpack_unsigned_integer(buffer, 3, iskip)
       sec2%dx = real(idum) * 1.E-3
       idum = unpack_unsigned_integer(buffer, 3, iskip)
       sec2%dy = real(idum) * 1.E-3
       sec2%center = unpack_unsigned_integer(buffer, 1, iskip)
       sec2%scanning_mode = unpack_unsigned_integer(buffer, 1, iskip)

       if ( btest(sec2%scanning_mode, 7) ) then
          sec2%i_scan_direction = -1
       else
          sec2%i_scan_direction = 1
       endif

       if ( btest(sec2%scanning_mode, 6) ) then
          sec2%j_scan_direction = 1
       else
          sec2%j_scan_direction = -1
       endif
       
       if ( btest(sec2%scanning_mode, 5) ) then
          sec2%orientation = -1
       else
          sec2%orientation = 1
       endif
       
       if (sec2%center==0) then
          sec2%latin1 = 60.0
       else
          sec2%latin1 = -60.0
       endif
       sec2%latin2 = -1.E36
       ! Skip those last four bytes:
       iskip = iskip + (4*8)
       ! print*, 'nx, ny = ', sec2%nx, sec2%ny
       ! print*, 'lat1, lon1 = ', sec2%lat1, sec2%lon1
       ! print*, 'lov, truelat1, truelat2 = ', sec2%lov, sec2%latin1, sec2%latin2
       ! print*, 'dx, dy = ', sec2%dx, sec2%dy

    case default

       write(*,'("GRIB1_UNPACK_SEC2:  Unrecognized GRIB1 grid type (drt):  ", I10)') sec2%drt
       stop

    end select

  end subroutine grib1_unpack_sec2

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec3(grib)
    ! Bit Map Section (bitmap section)
    implicit none
    type (GribStruct),                     intent(inout) :: grib

    integer :: bitmap_beginning
    integer :: bitmap_length
    integer :: idim, jdim

    grib%g1sec3%isize      = unpack_unsigned_integer(grib%buffer, 3, grib%iskip)
    grib%g1sec3%nunused    = unpack_unsigned_integer(grib%buffer, 1, grib%iskip)
    grib%g1sec3%numeric    = unpack_unsigned_integer(grib%buffer, 2, grib%iskip)

    grib%g1sec3%bitmap_beginning = grib%iskip/8
    bitmap_length    = grib%g1sec3%isize-6

    ! I may need to reorient the bitmap, too, depending on data ordering....
    idim = grib%g1sec2%nx
    jdim = grib%g1sec2%ny
    allocate(grib%bitmap(idim,jdim))
    call gbytes(grib%buffer, grib%bitmap, grib%g1sec3%bitmap_beginning*8, 1, 0, idim*jdim)

    ! And skip to the end of the section
    grib%iskip = grib%iskip + (bitmap_length*8)

  end subroutine grib1_unpack_sec3

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec4(buffer, buffsize, iskip, sec4)
    implicit none
    integer,                               intent(in)    :: buffsize
    character(len=1), dimension(buffsize), intent(in)    :: buffer
    integer,                               intent(inout) :: iskip
    type (G1Section4Struct),               intent(out)   :: sec4

    integer :: n
    real    :: xref2
    integer :: isign, characteristic, mantissa

    sec4%isize      = unpack_unsigned_integer(buffer, 3, iskip)

    do n = 1, 4
       call gbyte(buffer, sec4%flag(n), iskip, 1)
       iskip = iskip + 1
    enddo
    call gbyte(buffer, sec4%nunused, iskip, 4)
    iskip = iskip + 4

    sec4%binary_scale_factor  = unpack_signed_integer(buffer, 2, iskip)

    ! Unpack the floating-point number
    call gbyte(buffer, isign, iskip, 1)
    iskip = iskip + 1
    call gbyte(buffer, characteristic, iskip, 7)
    iskip = iskip + 7
    call gbyte(buffer, mantissa, iskip, 24)
    iskip = iskip + 24
    ! print*, 'isign = ', isign
    ! print*, 'characteristic = ', characteristic
    ! print*, 'mantissa = ', mantissa
    xref2 = real(dble(mantissa) / dble(2**24) * dble(16 ** (characteristic-64)))
    if (isign==1) xref2 = -xref2
    ! print*, 'Reference value = ', xref2
    sec4%reference_value = xref2
    sec4%nbits = unpack_unsigned_integer(buffer, 1, iskip)

    sec4%start_of_packed_data = iskip/8

    ! Skip to the end of this section
    iskip = iskip + (sec4%isize-11)*8

  end subroutine grib1_unpack_sec4

!=================================================================================
!=================================================================================

  subroutine grib1_unpack_sec5(buffer, buffsize, iskip)
    implicit none
    ! Real simple.  Just check for the "7777" flag which marks the end of the 
    ! GRIB1 record.
    integer,                               intent(in)    :: buffsize
    character(len=1), dimension(buffsize), intent(in)    :: buffer
    integer,                               intent(inout) :: iskip

    integer :: test_sevens

    test_sevens = unpack_unsigned_integer(buffer, 4, iskip)
    if (test_sevens /= string_sevens) then
       write(*, '(" MODULE_GRIB1:  GRIB1_UNPACK_SEC5:  Problem:  Lost the way in GRIB1 record.")')
       stop
    endif

  end subroutine grib1_unpack_sec5

!=================================================================================
!=================================================================================

  subroutine grib1_gribdata(grib)
    implicit none
    type (GribStruct)                     :: grib
    ! real, pointer, dimension(:,:) :: array

    if (grib%g1sec4%flag(1)==0) then
       ! Grid-point data
       if (grib%g1sec4%flag(2) == 0) then
          ! Simple packing
          if (grib%g1sec4%flag(3)==0) then
             ! Original field held floating-point values
             if (grib%g1sec4%flag(4)==0) then
                ! No additional flags in octet 14
                allocate(grib%array(grib%mapinfo%nx,grib%mapinfo%ny))
                if (grib%g1sec1%ifbms) then
                   call grib1_sgup_bitmap(grib, grib%array, grib%bitmap, grib%mapinfo%nx, grib%mapinfo%ny)
                else
                   call grib1_sgup_nobitmap(grib, grib%array, grib%mapinfo%nx, grib%mapinfo%ny)
                endif
             else
                stop "GRIB1:  Section 4:  Additional flags in octed 14"
             endif
          else
             stop "GRIB1:  Section 4:  Original field held integer values."
          endif
       else
          stop "GRIB1:  Section 4:  Complex packing"
       endif
    else
       stop "GRIB1:  Section 4:  Spherical Harmonic coefficients"
    endif

  end subroutine grib1_gribdata

!=================================================================================
!=================================================================================

  subroutine grib1_map_information(sec2, mapinfo)
    implicit none
    type(G1Section2Struct), intent(in) :: sec2
    type(MapInfoStruct), intent(out)   :: mapinfo
    select case (sec2%drt)
    case default
       write(*, '("Unrecognized GRIB1 grid data representation type:  ", I12)') sec2%drt
       stop "GRIB1_MAP_INFORMATION:  Unrecognized grid data representation type."
    case (0) ! Cylindrical Equidistant
       mapinfo%hproj = "CE"
       mapinfo%supmap_jproj = 8
       mapinfo%nx   = sec2%nx
       mapinfo%ny   = sec2%ny
       mapinfo%dx   = sec2%dx
       mapinfo%dy   = sec2%dy
       mapinfo%lat1 = sec2%lat1
       mapinfo%lon1 = sec2%lon1
       mapinfo%lat2 = sec2%lat2
       mapinfo%lon2 = sec2%lon2
       mapinfo%xlonc    = -1.E33
       mapinfo%truelat1 = -1.E33
       mapinfo%truelat2 = -1.E33
    case (3) ! Lambert Conformal 
       mapinfo%hproj = "LC"
       mapinfo%supmap_jproj = 3
       mapinfo%nx   = sec2%nx
       mapinfo%ny   = sec2%ny
       mapinfo%dx   = sec2%dx
       mapinfo%dy   = sec2%dy
       mapinfo%lat1 = sec2%lat1
       mapinfo%lon1 = sec2%lon1
       mapinfo%lat2 = -1.E36
       mapinfo%lon2 = -1.E36
       mapinfo%xlonc = sec2%lov
       mapinfo%truelat1 = sec2%latin1
       mapinfo%truelat2 = sec2%latin2
    case (5) ! Polar Stereographic
       mapinfo%hproj = "ST"
       mapinfo%supmap_jproj = 1
       mapinfo%nx   = sec2%nx
       mapinfo%ny   = sec2%ny
       mapinfo%dx   = sec2%dx
       mapinfo%dy   = sec2%dy
       mapinfo%lat1 = sec2%lat1
       mapinfo%lon1 = sec2%lon1
       mapinfo%lat2 = -1.E36
       mapinfo%lon2 = -1.E36
       mapinfo%xlonc = sec2%lov
       mapinfo%truelat1 = sec2%latin1
       mapinfo%truelat2 = sec2%latin2
    end select
  end subroutine grib1_map_information

!=================================================================================
!=================================================================================

  subroutine GRIB1_SGUP_NOBITMAP(grib, array, idim, jdim)
    ! GRIB 1 Data unpacking:  Simple grid-point unpacking without a bitmap
    implicit none
    type (GribStruct) :: grib
    integer, intent(in) :: idim, jdim
    real, dimension(idim*jdim), intent(out) :: array

    integer, dimension(idim*jdim) :: IX
    double precision :: dfac, bfac
    integer :: iskip

    integer :: i, j, m, n

    DFAC = 1.0 / (10.D0**(grib%g1sec1%decimal_scale_factor))
    BFAC = 2.D0**(grib%g1sec4%binary_scale_factor)

! sec4%nbits is the number of bits used per datum.
! If sec4%nbits = 255, assume they mean sec4%nbits = 0

    if (grib%g1sec4%nbits == 255) grib%g1sec4%nbits = 0

    if (grib%g1sec4%nbits > 0) then
       call gbytes(grib%buffer, IX, grib%g1sec4%start_of_packed_data*8, grib%g1sec4%nbits, 0, idim*jdim)
    endif

! If sec4(8) is 0, assume datarray is constant, scaled from the value of sec4%reference_value

    if (grib%g1sec4%nbits == 0) then
       array = DFAC * grib%g1sec4%reference_value
    else
       ! Check point order!!!

       if (grib%g1sec2%orientation == 1) then
          ! We're good.  Fortran(i,j)
       elseif (grib%g1sec2%orientation == -1) then
          write(*,'("grib%g1sec2%orientation = ", I12)') grib%g1sec2%orientation
          stop "TODO:  GRIB1_SGUP_NOBITMAP:  grib%g1sec2%orientation = -1"
       else
          write(*,'("grib%g1sec2%orientation = ", I12)') grib%g1sec2%orientation
          stop "TODO:  GRIB1_SGUP_NOBITMAP:  Problem recognizing grib%g1sec2%orientation"
       endif

       if (grib%g1sec2%i_scan_direction == 1) then
          ! We're good.
       elseif (grib%g1sec2%i_scan_direction == -1) then
          write(*,'("grib%g1sec2%i_scan_direction = ", I12)') grib%g1sec2%i_scan_direction
          stop "TODO:  GRIB1_SGUP_NOBITMAP:  grib%g1sec2%i_scan_direction = -1"
       else
          write(*,'("grib%g1sec2%i_scan_direction = ", I12)') grib%g1sec2%i_scan_direction
          stop "GRIB1_SGUP_NOBITMAP:   Problem recognizing grib%g1sec2%i_scan_direction"
       endif
       
       if (grib%g1sec2%j_scan_direction == 1) then
          array = DFAC * (dble(grib%g1sec4%reference_value) + (dble(IX)*BFAC))
       elseif (grib%g1sec2%j_scan_direction == -1) then
          do j = 1, jdim
             do i = 1, idim
                n = (j-1)*idim + i
                m = (jdim-j)*idim + i
                array(n) = DFAC * (dble(grib%g1sec4%reference_value) + (dble(IX(m))*BFAC))
             enddo
          enddo
       else
          write(*,'("grib%g1sec2%j_scan_direction = ", I12)') grib%g1sec2%j_scan_direction
          stop "GRIB1_SGUP_NOBITMAP:  Problem recognizing grib%g1sec2%j_scan_direction"
       endif
    endif

  end subroutine GRIB1_SGUP_NOBITMAP

!=================================================================================
!=================================================================================

  subroutine GRIB1_SGUP_BITMAP(grib, array, bitmap, nx, ny)
    ! Simple grid-point unpacking, with a bitmap.
    implicit none
    type (GribStruct)      :: grib
    integer, intent(in) :: nx, ny
    real, dimension(nx*ny), intent(out) :: array
    integer, dimension(nx*ny), intent(in) :: bitmap

    type(G1Section1Struct), pointer :: sec1
    type(G1Section3Struct), pointer :: sec3
    type(G1Section4Struct), pointer :: sec4

    integer :: ndat ! Number of data points in the final grid.
    double precision :: dfac, bfac
    integer :: iskip, nbm, i, nn

    integer, allocatable, dimension(:) :: bmdat

    array = -1.E30

    ndat = nx * ny

!
! Compute the parameters involved with packing
!
    DFAC = 1.0 / 10.**(grib%g1sec1%decimal_scale_factor)
    BFAC = 2.**grib%g1sec4%binary_scale_factor

! grib%g1sec4%isize   : The number of bytes in the whole of GRIB Grib%g1section 4.
! grib%g1sec4%nunused : The number of unused bits at the end of GRIB Section 4.
! GRIB%G1SEC4%nbits : The number of bits per data value.


! 1) There are fewer than NDAT data values, because a bitmap was used.  
!    Compute the number of data values (NBM).  There are 11 extra bytes
!    in the header section 4.  NBM equals the total number of data bits (not
!    counting the header bits), minus the number of unused buts, and then
!    divided by the number of bits per data value.

! If grib%g1sec4%nbits is 0, assume <array> is constant value of DFAC * grib%g1sec4%reference_value

    if (grib%g1sec4%nbits.eq.0) then
       where(bitmap(1:ndat)==1) array = DFAC * grib%g1sec4%reference_value
       return
    endif
    nbm = ((grib%g1sec4%isize-11)*8-grib%g1sec4%nunused)/grib%g1sec4%nbits
    allocate(bmdat(nbm))
    if (nbm /= count(bitmap==1)) then
       print*, "NBM, count(bitmap==1):  ", nbm, count(bitmap==1)
       write(*,'("GRIB1_SGUP_BITMAP:  Problem with the number of data values in the bitmap.")')
       stop
    endif

! grib%g1sec4%nbits is the number of bits used per datum value.
! If grib%g1sec4%nbits = 255, assume they mean grib%g1sec4%nbits = 0
    if (grib%g1sec4%nbits == 255) grib%g1sec4%nbits = 0

! Read the data from the <buffer> array
    call gbytes(grib%buffer, bmdat, grib%g1sec4%start_of_packed_data*8, grib%g1sec4%nbits, 0, nbm)

    if (grib%g1sec2%orientation == 1) then
       ! We're good.  Fortran(i,j)
    elseif (grib%g1sec2%orientation == -1) then
       write(*,'("grib%g1sec2%orientation = ", I12)') grib%g1sec2%orientation
       stop "TODO:  GRIB1_SGUP_BITMAP:  grib%g1sec2%orientation = -1"
    else
       write(*,'("grib%g1sec2%orientation = ", I12)') grib%g1sec2%orientation
       stop "TODO:  GRIB1_SGUP_BITMAP:  Problem recognizing grib%g1sec2%orientation"
    endif

! Check the i scan direction:
    if (grib%g1sec2%i_scan_direction == 1) then
       ! We're good.
    elseif (grib%g1sec2%i_scan_direction == -1) then
       write(*,'("grib%g1sec2%i_scan_direction = ", I12)') grib%g1sec2%i_scan_direction
       stop "TODO:  GRIB1_SGUP_BITMAP:  grib%g1sec2%i_scan_direction = -1"
    else
       write(*,'("grib%g1sec2%i_scan_direction = ", I12)') grib%g1sec2%i_scan_direction
       stop "GRIB1_SGUP_BITMAP:  Problem recognizing grib%g1sec2%i_scan_direction"
    endif

! Check the j scan direction:
    if (grib%g1sec2%j_scan_direction == 1) then
       ! We're good.
    elseif (grib%g1sec2%j_scan_direction == -1) then
       write(*,'("grib%g1sec2%j_scan_direction = ", I12)') grib%g1sec2%j_scan_direction
       stop "TODO:  GRIB1_SGUP_BITMAP:  grib%g1sec2%j_scan_direction = -1"
    else
       write(*,'("grib%g1sec2%j_scan_direction = ", I12)') grib%g1sec2%j_scan_direction
       stop "GRIB1_SGUP_BITMAP:  Problem recognizing grib%g1sec2%j_scan_direction"
    endif
       


! Unpack the data according to packing parameters DFAC, BFAC, and XEC4(1), 
! and masked by the bitmap BITMAP.
    nn = 0
    do i = 1, ndat
       if (bitmap(i)==1) then
          nn = nn + 1
          array(i) = DFAC * (dble(grib%g1sec4%reference_value) + (dble(bmdat(nn))*BFAC))
       endif
    enddo

! Deallocate the scratch BMDAT array
    deallocate(bmdat)
  end subroutine GRIB1_SGUP_BITMAP

!=================================================================================
!=================================================================================

  subroutine grib1_valid_date(sec1, validdate)
    implicit none
    type(G1Section1Struct), intent(in) :: sec1
    character(len=19), intent(out) :: validdate

    character(len=19) :: refdate
    integer :: tfac

    refdate = sec1%hdate

    ! Find TFAC, which is a factor to convert the given time offset
    ! to seconds.
    select case (sec1%ftu)
    case (0)
       tfac = 60
    case (1)
       tfac = 3600
    case (2)
       tfac = 86400
    case (10)
       tfac = 10800
    case (11)
       tfac = 21600
    case (12)
       tfac = 43200
    case (254)
       tfac = 1
    case default
       write(*, '("Unrecognized GRIB1 Forecast Time Unit: ", I6)') sec1%ftu
       stop
    end select

    select case (sec1%tri) ! "Time Range Indicator"
    case default
       write(*, '("Unrecognized GRIB1 Time Range Indicator: ", I6)') sec1%tri
       stop
    case (3) ! Average
       ! Average from (reference_time + p1) to (reference time + p2)
       call geth_newdate(validdate, refdate, tfac*(sec1%p1+sec1%p2)/2)
    case(4) ! Accumulation from refdate + P1 to refdate + P2, 
       ! valid at refdate + P2
       ! call geth_newdate(fdate,     refdate, tfac*sec1(17))
       call geth_newdate(validdate, refdate, tfac*sec1%p2)
!       if (present(offset)) then
!          offset = tfac*(sec1%p2-sec1%p1)
!       endif
    case(5) ! Difference, valid at refdate + P2
       call geth_newdate(validdate, refdate, tfac*sec1%p2)
    case (0:1,10) ! Product valid at refdate + P1
       call geth_newdate(validdate, refdate, tfac*sec1%p1)
    end select

  end subroutine grib1_valid_date

!=================================================================================
!=================================================================================

  subroutine grib1_read_parameter_tables
    implicit none
    character(len=256) :: grib_root
    character(len=256) :: table_filename
    integer :: iunit
    integer :: ierr
    integer :: p1
    integer :: p2
    integer :: p3
    integer, external :: get_unused_unit

    character(len=200) :: string
    character(len=256) :: parameter_name
    character(len=64)  :: parameter_units
    character(len=8)   :: parameter_abbr
    integer            :: parameter_index

    call getenv("GRIB_ROOT", grib_root)
    if (trim(grib_root)==" ") then
       write(*,'("Not finding environment variable GRIB_ROOT.")')
       write(*,'("Program does not know where to find GRIB parameter tables")')
       stop
    endif
    table_filename = trim(grib_root)//"/GRIB1_PARAMETER_TABLE"

    iunit = get_unused_unit()

    open(iunit, file=trim(table_filename), status='old', form='formatted', action='read', iostat=ierr)
    if (ierr /= 0) then
       write(*,'(/," ***** ERROR *****",/)')
       write(*,'(" ***** Problem opening file ''", A, "''")') trim(table_filename)
       write(*,'(" ***** Does file exist?  Is file readable?",/)')
       stop
    endif

    READ_TABLE : do
       read(iunit, '(A200)', iostat=ierr) string
       if (ierr /= 0) exit READ_TABLE
       if (string(1:1) == "#") cycle READ_TABLE

       p1 = index(string, "|") - 1
       p2 = p1 + index(string(p1+2:), "|")
       p3 = p2 + index(string(p2+2:), "|")
       read(string(1:p1),*) parameter_index
       parameter_name = trim(adjustl(string(p1+2:p2)))
       parameter_units = trim(adjustl(string(p2+2:p3)))
       parameter_abbr = trim(adjustl(string(p3+2:)))
       ! write(*, '(I3,1x,"#",A,"#",1x,"#",A,"#",1x,"#",A,"#")') parameter_index, trim(parameter_name), trim(parameter_units), trim(parameter_abbr)
       grib1_parameter_table(parameter_index)%name  = trim(parameter_name)
       grib1_parameter_table(parameter_index)%units = trim(parameter_units)
       grib1_parameter_table(parameter_index)%abbr  = trim(parameter_abbr)

    enddo READ_TABLE
    close(iunit)
  end subroutine grib1_read_parameter_tables

!=================================================================================
!=================================================================================

end module module_grib1
