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

module module_grib_common
  use module_mapinfo
  ! Low-level stuff used by both module_grib1 and module_grib2
  implicit none
  integer, parameter :: string_sevens = Z"37373737" ! Encodes "7777", which marks the end of a GRIB record.
  integer, parameter :: string_grib   = Z"47524942" ! Encodes "GRIB", which marks the beginning of a GRIB record

  type G1Section1Struct
     integer :: isize
     integer :: ptvn
     integer :: center
     integer :: process
     integer :: grid
     logical :: ifgds
     logical :: ifbms
     integer :: parameter
     integer :: leveltyp
     real    :: levelval
     real    :: level2val
     integer :: year
     integer :: month
     integer :: day
     integer :: hour
     integer :: minute
     integer :: ftu
     integer :: p1
     integer :: p2
     integer :: tri
     integer :: navg
     integer :: nmissavg
     integer :: subcenter
     integer :: decimal_scale_factor
     character(len=19) :: hdate
  end type G1Section1Struct

  type G1Section2Struct
     integer :: isize
     integer :: nv
     integer :: pv
     integer :: drt
     integer :: nx
     integer :: ny
     real    :: lat1
     real    :: lon1
     integer :: rac
     real    :: lat2
     real    :: lon2
     real    :: lov
     real    :: dx
     real    :: dy
     integer :: center
     integer :: scanning_mode
     integer :: i_scan_direction
     integer :: j_scan_direction
     integer :: orientation
     real    :: latin1
     real    :: latin2
     real    :: polelat
     real    :: polelon
  end type G1Section2Struct

  type G1Section3Struct
     integer :: isize
     integer :: nunused
     integer :: numeric
     integer :: bitmap_beginning
  end type G1Section3Struct

  type G1Section4Struct
     integer :: isize
     integer, dimension(4) :: flag
     integer :: nunused
     integer :: binary_scale_factor
     real    :: reference_value
     integer :: nbits
     integer :: start_of_packed_data
  end type G1Section4Struct

  type Section1Struct
     integer :: size
     integer :: center
     integer :: subcenter
     integer :: mtvn
     integer :: ltvn
     integer :: srt
     integer :: year
     integer :: month
     integer :: day
     integer :: hour
     integer :: minute
     integer :: second
     integer :: production_status
     integer :: data_type
     character(len=19) :: hdate
  end type Section1Struct

  type GT_3_0_Struct
     integer :: scale_factor_of_radius
     integer :: scaled_value_of_radius
     integer :: scale_factor_major_axis
     integer :: scaled_value_major_axis
     integer :: scale_factor_minor_axis
     integer :: scaled_value_minor_axis
     integer :: basic_angle
     integer :: subdivisions_of_basic_angle
     real    :: la1
     real    :: lo1
     integer :: resolution_and_component_flags
     real    :: la2
     real    :: lo2
     real    :: dx
     real    :: dy
     integer :: scanning_mode
     integer :: i_direction_increments_given
     integer :: j_direction_increments_given
     integer :: winds_grid_relative
     integer :: i_scan_direction
     integer :: j_scan_direction
     integer :: i_scan_consecutive
     integer :: boustrophedon
  end type GT_3_0_Struct

  type GT_3_30_Struct
     integer :: shape_of_earth
     integer :: scale_factor_of_radius
     integer :: scaled_value_of_radius
     integer :: scale_factor_major_axis
     integer :: scaled_value_major_axis
     integer :: scale_factor_minor_axis
     integer :: scaled_value_minor_axis
     real    :: la1
     real    :: lo1
     integer :: resolution_and_component_flags
     real    :: lad
     real    :: lov
     real    :: dx
     real    :: dy
     integer :: projection_center_flag
     integer :: scanning_mode
     real    :: latin1
     real    :: latin2
     real    :: pole_latitude
     real    :: pole_longitude
     integer :: i_direction_increments_given
     integer :: j_direction_increments_given
     integer :: winds_grid_relative
     integer :: i_scan_direction
     integer :: j_scan_direction
     integer :: i_scan_consecutive
     integer :: boustrophedon
  end type GT_3_30_Struct

  type Section3Struct
     integer :: size
     integer :: grid_definition_source
     integer :: number_of_data_points
     integer :: octets_for_optional_list
     integer :: list_interpretation
     integer :: grid_template_number
     integer :: nx
     integer :: ny
     integer :: shape_of_earth
     type (GT_3_0_Struct) :: GT_3_0
     type (GT_3_30_Struct) :: GT_3_30
  end type Section3Struct

!  type PDT_4_0_Struct 
!  end type PDT_4_0_Struct

  type PDT_4_8_Struct 
     integer :: end_year
     integer :: end_month
     integer :: end_day
     integer :: end_hour
     integer :: end_minute
     integer :: end_second
     character(len=19) :: begin_hdate
     character(len=19) :: end_hdate
     integer :: number_of_time_range_specifications
     integer :: number_missing
     integer :: statistical_process
     integer :: type_of_time_increment
     integer :: time_range_unit
     integer :: length_of_time_range
     integer :: time_increment_unit
     integer :: time_increment
  end type PDT_4_8_Struct

  type Section4Struct
     integer :: size
     integer :: number_of_coord_values
     integer :: product_template_number
     integer :: parameter_category
     integer :: parameter_number
     integer :: type_of_generating_process
     integer :: background_process_id
     integer :: generating_process_id
     integer :: data_cutoff_hours
     integer :: data_cutoff_minutes
     integer :: time_range_indicator
     integer :: time
     integer :: ltype1
     integer :: lscale1
     integer :: lvalue1
     real    :: level1
     integer :: ltype2
     integer :: lscale2
     integer :: lvalue2
     real    :: level2
     ! type (PDT_4_0_Struct) :: PDT_4_0
     type (PDT_4_8_Struct) :: PDT_4_8

  end type Section4Struct

  type DRT_5_0_Struct
     integer :: binary_scale_factor
     integer :: decimal_scale_factor
     integer :: nbits
     integer :: data_type
  end type DRT_5_0_Struct

  type DRT_5_2_Struct
     real    :: reference_value
     integer :: binary_scale_factor
     integer :: decimal_scale_factor
     integer :: nbits
     integer :: data_type
     integer :: group_splitting_method
     integer :: missing_value_management
     integer :: substitute1
     integer :: substitute2
     integer :: ng
     integer :: widths_reference
     integer :: widths_nbits
     integer :: lengths_reference
     integer :: length_increment
     integer :: length_of_last_group
     integer :: lengths_nbits
  end type DRT_5_2_Struct

  type DRT_5_40_Struct
     real :: reference_value
     integer :: binary_scale_factor
     integer :: decimal_scale_factor
     integer :: nbits
     integer :: data_type
     integer :: compression_type
     integer :: target_compression_ratio
  end type DRT_5_40_Struct

  type Section5Struct
     integer :: size
     integer :: nval
     integer :: data_template_number
     type (DRT_5_0_Struct) :: DRT_5_0
     type (DRT_5_2_Struct) :: DRT_5_2
     type (DRT_5_40_Struct) :: DRT_5_40
  end type Section5Struct

  type Section6Struct
     integer :: size
     integer :: bit_map_indicator
  end type Section6Struct

  type Section7Struct
     integer :: size
     real, pointer, dimension(:)   :: floated
  end type Section7Struct

  type GribStruct
     integer :: edition
     integer :: size
     character(len=1), pointer, dimension(:)   :: buffer
     real,             pointer, dimension(:,:) :: array
     integer,          pointer, dimension(:,:) :: bitmap
     integer :: iskip
     integer :: discipline

     type(G1Section1Struct) :: g1sec1
     type(G1Section2Struct) :: g1sec2
     type(G1Section3Struct) :: g1sec3
     type(G1Section4Struct) :: g1sec4

     type(Section1Struct) :: sec1
     type(Section3Struct) :: sec3
     type(Section4Struct) :: sec4
     type(Section5Struct) :: sec5
     type(Section6Struct) :: sec6
     type(Section7Struct) :: sec7
     type(MapInfoStruct)  :: mapinfo
  end type GribStruct

contains

!=================================================================================
!=================================================================================

  integer function unpack_signed_integer(buffer, nbytes, iskip) RESULT (signed_integer)
    implicit none
    !
    ! Purpose:  
    !      From a string of bytes in a buffer array, unpack a stream of bytes into an 
    !      integer variable, with the a priori knowledge that this stream of bytes
    !      represents a signed integer, the sign indicated by the first bit in the stream. 
    !      The <iskip> indicator is then incremented to refer to the position in the 
    !      <buffer> array at the end of the integer just read.
    !
    ! Input:
    !      buffer:  A buffer array from which we unpack data.
    !      nbytes:  The number of bytes to read from <buffer> as representing a signed integer
    !      iskip:   The number of bytes to skip in <buffer> before unpacking <nbytes> of data.
    !
    ! Output:
    !      iskip:   The number of bits in <buffer> up to and including the signed integer 
    !               just read.
    !
    ! Return Value:
    !      The integer unpacked from <buffer>
    !
    ! Side effects:
    !      None
    !
    character(len=1), dimension(*), intent(in)    :: buffer
    integer,                        intent(in)    :: nbytes
    integer,                        intent(inout) :: iskip

    integer :: isign

    ! Read the first bit, to determine whether this integer is negative (<isign>==1)
    ! or non-negative (<isign>==0)
    call gbyte(buffer, isign, iskip, 1)

    ! Read the remaining bits of the integer
    call gbyte(buffer, signed_integer, iskip+1, (nbytes*8)-1)

    ! Apply the sign we read before to the integer value.
    if (isign == 1) then
       signed_integer = -signed_integer
    endif

    ! Update the <iskip> indicator.
    iskip = iskip + (nbytes*8)

  end function unpack_signed_integer

!=================================================================================
!=================================================================================

  integer function unpack_unsigned_integer(buffer, nbytes, iskip) RESULT (unsigned_integer)
    implicit none
    !
    ! Purpose:  
    !      From a string of bytes in a buffer array, unpack a stream of bytes into an 
    !      integer variable, with the a priori knowledge that this stream of bytes
    !      represents an _unsigned_ integer.  The <iskip> indicator is then incremented 
    !      to refer to the position in the <buffer> array at the end of the integer just read.
    !
    ! Input:
    !      buffer:  A buffer array from which we unpack data.
    !      nbytes:  The number of bytes to read from <buffer> as representing an unsigned integer
    !      iskip:   The number of bytes to skip in <buffer> before unpacking <nbytes> of data.
    !
    ! Output:
    !      iskip:   The number of bits in <buffer> up to and including the unsigned integer 
    !               just read.
    !
    ! Return Value:
    !      The integer unpacked from <buffer>
    !
    ! Side effects:
    !      None
    !
    character(len=1), dimension(*), intent(in)    :: buffer
    integer,                        intent(in)    :: nbytes
    integer,                        intent(inout) :: iskip

    call gbyte(buffer, unsigned_integer, iskip, nbytes*8)

    ! Update the <iskip> indicator.
    iskip = iskip + (nbytes*8)

  end function unpack_unsigned_integer

!=================================================================================
!=================================================================================

end module module_grib_common
