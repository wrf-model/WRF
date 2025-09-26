!
! This file contains a module, which is used to pass around information
! from the Vtable from one program component to another
!
module Table
  implicit none

! We have parameterized the maximum number of variables we expect to want to 
! read:
  integer, parameter :: maxlines=100

! Each variable has a name.  The names are stored in array NAMVAR.
! Initialize the NAMVAR field to blanks:
  character (LEN=9) , dimension(maxlines) :: namvar = ' '

! Array DUNITS holds the unit strings for the fields.
  character (LEN=25), dimension(maxlines) :: Dunits = ' '

! Array DDESC holds the description strings for the fields.
  character (LEN=46), dimension(maxlines) :: Ddesc =  ' '

! Most of the fields are output, but some are not.  The names of the
! fields we want to output are stored in NAMEOUT.  Initialize the 
! NAMEOUT field to blanks:
  character (LEN=9) , dimension(maxlines) :: nameout = ' '
  character (LEN=25), dimension(maxlines) :: unitout = ' '
  character (LEN=46), dimension(maxlines) :: descout = ' '

! MAXVAR is the count of variables we have read.  It is initialized to ZERO.
  integer :: maxvar = 0

! MAXOUT is the count of the variables we want to output.
! Initialize it to zero.
  integer :: maxout = 0

! Array GCODE holds the GRIB1 param numbers of the fields we want to access:
  integer, dimension(maxlines) :: gcode
! Array LCODE holds the GRIB1 level types of the params we want to access:
  integer, dimension(maxlines) :: lcode

! Array G2_GCODE holds the GRIB2 param numbers of the fields we want to access
! and the GRIB2 level types of the params we want to access:
  integer, dimension(5,maxlines) :: g2code

! Array LEVEL1 holds the Level-1 values of the fields we want:
! If the Vtable has a '*' for the Level-1 value, LEVEL1 has 
! the value -99.
  integer, dimension(maxlines) :: level1

! Array LEVEL2 holds the Level-2 values of the fields we want.
! If LEVEL2 is not needed for a particular field, it is set to 
! -99.
  integer, dimension(maxlines) :: level2

! Array IPRTY holds the priority values of the fields:
! Priorities are used with surface fields, when we have 
! encountered the situation where a field may be stored in two different
! ways in a file.  Ultimately, the field with the lower priority number
! (i.e., higher priority) is what is output.
  integer, dimension(maxlines) :: iprty

  integer :: blankcode = -99
  integer :: splatcode = -88

end module Table
