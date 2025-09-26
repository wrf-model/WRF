module datatype_module

   use bitarray_module
   use module_debug

   ! Return values for comparison functions primary_cmp() and secondary_cmp()
   integer, parameter :: LESS = -1, &
                         EQUAL = 0, &
                         GREATER = 1, &
                         NOT_EQUAL = 2

   type header_info
      integer :: version

      !  YYYY?MM?DD?HH?mm?ss
      character (len=32) :: date
      logical :: time_dependent, mask_field, constant_field

      !  Set = 0 if this is an analysis.
      real :: forecast_hour

      !  AVN, GFS, ETA???, ARW, NMM, AGRMET, NAM, RUC, SST
      character (len=32) :: fg_source

      character (len=128) :: field
      character (len=128) :: units
      character (len=128) :: description

      !  PRESSURE, SIGMA, NATIVE, HYBRID
      character (len=32) :: vertical_coord
      integer :: vertical_level

      !  XY, YX - ENOUGH INFO?
      character (len=32) :: array_order
      integer, dimension(2) :: dim1, dim2

      logical :: is_wind_grid_rel
      logical :: array_has_missing_values

      integer :: sr_x, sr_y
   end type header_info

   type map_info
      !  Mercator, Polar Stereographic, Lambert, Gaussian, Lat Lon
      character (len=32) :: projection

      integer :: projection_flag

      ! For ARW: M, U, or V; for NMM: H or V
      integer :: stagger  

      real :: knownlat, knownlon, deltalat, deltalon
      real :: deltax, deltay, xlonc, truelat1, truelat2
      real :: lat1, lon1
   end type map_info

   ! This is the datatype that is understood by data_storage module
   type fg_input
      ! BEGIN any types we want to keep and use for sorting in storage module 
      type (header_info) :: header
      type (map_info) :: map
      ! END any types we want to keep and use for sorting in storage module 
 
      real, dimension(:,:), pointer :: r_arr     !!!!! REQUIRED !!!!!
      type (bitarray), pointer :: valid_mask, modified_mask
   end type fg_input

   ! This type is used for the nodes of the secondary linked lists, the ones that
   !   actually store data
   type data_node
      type (fg_input) :: fg_data

      type (data_node), pointer :: next, prev
      integer, dimension(2) :: field_shape

      ! If non-zero, the array is actually stored in a Fortran unit 
      integer :: filenumber

      ! The following two are used by heaps
      integer :: last_used
      integer :: heap_index
   end type data_node

   ! This type is used for the nodes in the primary linked lists, and thus has head
   !   and tail pointers for secondary linked lists
   type head_node
      type (fg_input) :: fg_data
      type (head_node), pointer :: next, prev
      type (data_node), pointer :: fieldlist_head, fieldlist_tail
   end type head_node


   contains


   ! Compares two fg_input types; returns EQUAL if the two should 
   !   belong to the same secondary linked list, and NOT_EQUAL otherwise
   function primary_cmp(a, b)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a, b

      ! Return value
      integer :: primary_cmp 

!      if ((a%header%date          == b%header%date) .and. &
!          (a%header%forecast_hour == b%header%forecast_hour) .and. &
!          (a%header%fg_source     == b%header%fg_source) .and. &
!          (a%header%field         == b%header%field)) then
      if (a%header%field         == b%header%field) then
         primary_cmp = EQUAL
      else
         primary_cmp = NOT_EQUAL
      end if

   end function primary_cmp


   ! Compares two fg_input types; returns EQUAL if the two belong 
   !   at the same position in a secondary linked list, LESS if "a" belongs 
   !   after "b", and GREATER if "a" belongs before "b"
   function secondary_cmp(a, b)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a, b

      ! Return value
      integer :: secondary_cmp 

! BUG: Eventually, we only want to sort pressure-level data this way, and 
!      all others the opposite way, as in the else case below.
      if (a%header%time_dependent .or. a%header%constant_field) then
         if (a%header%vertical_level > b%header%vertical_level) then
            secondary_cmp = LESS
         else if (a%header%vertical_level == b%header%vertical_level) then
            secondary_cmp = EQUAL
         else
            secondary_cmp = GREATER
         end if

      else
         if (a%header%vertical_level < b%header%vertical_level) then
            secondary_cmp = LESS
         else if (a%header%vertical_level == b%header%vertical_level) then
            secondary_cmp = EQUAL
         else
            secondary_cmp = GREATER
         end if
      end if

   end function secondary_cmp


   ! Duplicates an fg_input type
   subroutine dup(src, dst)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: src
      type (fg_input), intent(out) :: dst

      dst%header = src%header
      dst%map = src%map
      dst%r_arr => src%r_arr
      dst%valid_mask => src%valid_mask
      dst%modified_mask => src%modified_mask

   end subroutine dup

  
   function is_time_dependent(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      logical :: is_time_dependent

      is_time_dependent = a%header%time_dependent 

   end function is_time_dependent


   function is_mask_field(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      logical :: is_mask_field

      is_mask_field = a%header%mask_field

   end function is_mask_field


   function is_constant_field(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      logical :: is_constant_field

      is_constant_field = a%header%constant_field

   end function is_constant_field


   ! Returns the vertical level of an fg_input type
   function get_level(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      integer :: get_level

      get_level = a%header%vertical_level

   end function get_level


   ! Returns the description string of an fg_input type
   function get_description(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      character (len=128) :: get_description

      get_description = a%header%description

   end function get_description


   ! Returns the units string of an fg_input type
   function get_units(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      character (len=128) :: get_units

      get_units = a%header%units

   end function get_units


   ! Returns the field staggering an fg_input type
   function get_staggering(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      integer :: get_staggering

      get_staggering = a%map%stagger

   end function get_staggering


   ! Returns the fieldname string of an fg_input type
   function get_fieldname(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      ! Return value
      character (len=128) :: get_fieldname

      get_fieldname = a%header%field

   end function get_fieldname

   
   ! Gives starting and ending indices for a field
   subroutine get_dims(a, start_mem_1, end_mem_1, start_mem_2, end_mem_2)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a
      integer, intent(out) :: start_mem_1, end_mem_1, start_mem_2, end_mem_2

      start_mem_1 = a%header%dim1(1) 
      end_mem_1 = a%header%dim1(2) 
      start_mem_2 = a%header%dim2(1) 
      end_mem_2 = a%header%dim2(2) 

   end subroutine get_dims


   ! Prints relevant information from the headers of an fg_input type; mainly
   !   used for debugging
   subroutine print_header(a)

      implicit none

      ! Arguments
      type (fg_input), intent(in) :: a

      call mprintf(.true.,DEBUG,'FIELD  : %s',s1=trim(a%header%field))
      call mprintf(.true.,DEBUG,'DATE   : %s',s1=trim(a%header%date))
      call mprintf(.true.,DEBUG,'SOURCE : %s',s1=trim(a%header%fg_source))
      call mprintf(.true.,DEBUG,'FCST HR: %f',f1=a%header%forecast_hour)

   end subroutine print_header

end module datatype_module
