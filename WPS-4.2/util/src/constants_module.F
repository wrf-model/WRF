!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE CONSTANTS_MODULE
!
! This module defines constants that are used by other modules 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module constants_module

   real, parameter :: PI = 3.141592653589793
   real, parameter :: OMEGA_E = 7.292e-5 ! Angular rotation rate of the earth

   real, parameter :: DEG_PER_RAD = 180./PI
   real, parameter :: RAD_PER_DEG = PI/180.
 
   ! Mean Earth Radius in m.  The value below is consistent
   ! with NCEP's routines and grids.
   real, parameter :: A_WGS84  = 6378137.
   real, parameter :: B_WGS84  = 6356752.314
   real, parameter :: RE_WGS84 = A_WGS84
   real, parameter :: E_WGS84  = 0.081819192

   real, parameter :: A_NAD83  = 6378137.
   real, parameter :: RE_NAD83 = A_NAD83
   real, parameter :: E_NAD83  = 0.0818187034

   real, parameter :: EARTH_RADIUS_M = 6370000.   ! same as MM5 system
   real, parameter :: EARTH_CIRC_M = 2.*PI*EARTH_RADIUS_M

   real, parameter :: P0 = 1.0e5  ! Reference surface pressure, Pa
   real, parameter :: RD = 287.0  ! Gas constant for dry air
   real, parameter :: CP = 1004.0 ! Heat capacity for dry air at const. pressure

end module constants_module
