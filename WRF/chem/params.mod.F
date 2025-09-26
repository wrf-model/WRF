      module module_params

      implicit none

! BROADLY USED PARAMETERS:
!_________________________________________________
! i/o file unit numbers
! output
      INTEGER, PARAMETER :: kout=53
! input
      INTEGER, PARAMETER :: kin=12
!_________________________________________________
! altitude, wavelength, time (or solar zenith angle) grids
! altitude
      integer, PARAMETER :: kz=125
! wavelength
      integer, PARAMETER :: kw=1000
! time/sza
      integer, PARAMETER :: kt=100
!_________________________________________________
! number of weighting functions
!  wavelength dependent
      integer, PARAMETER :: ks=60
!  wavelength and altitude dependent
      integer, PARAMETER :: kj=150
!  wavelength dependent DOM (dissolved organic matter) spectra
      integer, PARAMETER :: kdom=200
! delta for adding points at beginning or end of data grids
      real, PARAMETER :: deltax = 1.E-5

! some constants...

! pi:
      real, PARAMETER :: pi=3.1415926535898

! radius of the earth, km:
      real, PARAMETER :: radius=6.371E+3

! Planck constant x speed of light, J m
      real, PARAMETER :: hc = 6.626068E-34 * 2.99792458E8

! largest number of the machine:
      real, PARAMETER :: largest=1.E+36

! small numbers (positive and negative)
      real, PARAMETER :: pzero = +10./largest
      real, PARAMETER :: nzero = -10./largest

! machine precision
      real, PARAMETER :: precis = 1.e-7

      end module module_params
