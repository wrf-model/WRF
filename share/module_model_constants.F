!WRF:MODEL_LAYER:CONSTANTS
!

 MODULE module_model_constants

   !  2. Following are constants for use in defining real number bounds.

   !  A really small number.

   REAL    , PARAMETER :: epsilon         = 1.E-15

   !  4. Following is information related to the physical constants.

   !  These are the physical constants used within the model.

! JM NOTE -- can we name this grav instead?
   REAL    , PARAMETER :: g = 9.81  ! acceleration due to gravity (m {s}^-2)

#if ( NMM_CORE == 1 )
   REAL    , PARAMETER :: r_d          = 287.04
   REAL    , PARAMETER :: cp           = 1004.6
#else
   REAL    , PARAMETER :: r_d          = 287.
   REAL    , PARAMETER :: cp           = 7.*r_d/2.
#endif

   REAL    , PARAMETER :: r_v          = 461.6
   REAL    , PARAMETER :: cv           = cp-r_d
   REAL    , PARAMETER :: cpv          = 4.*r_v
   REAL    , PARAMETER :: cvv          = cpv-r_v
   REAL    , PARAMETER :: cvpm         = -cv/cp
   REAL    , PARAMETER :: cliq         = 4190.
   REAL    , PARAMETER :: cice         = 2106.
   REAL    , PARAMETER :: psat         = 610.78
   REAL    , PARAMETER :: rcv          = r_d/cv
   REAL    , PARAMETER :: rcp          = r_d/cp
   REAL    , PARAMETER :: rovg         = r_d/g
   REAL    , PARAMETER :: c2           = cp * rcv
   real    , parameter :: mwdry        = 28.966 ! molecular weight of dry air (g/mole)

   REAL    , PARAMETER :: p1000mb      = 100000.
   REAL    , PARAMETER :: t0           = 300.
   REAL    , PARAMETER :: p0           = p1000mb
   REAL    , PARAMETER :: cpovcv       = cp/(cp-r_d)
   REAL    , PARAMETER :: cvovcp       = 1./cpovcv
   REAL    , PARAMETER :: rvovrd       = r_v/r_d

   REAL    , PARAMETER :: reradius     = 1./6370.0e03 

   REAL    , PARAMETER :: asselin      = .025
!   REAL    , PARAMETER :: asselin      = .0
   REAL    , PARAMETER :: cb           = 25.

   REAL    , PARAMETER :: XLV0         = 3.15E6
   REAL    , PARAMETER :: XLV1         = 2370.
   REAL    , PARAMETER :: XLS0         = 2.905E6
   REAL    , PARAMETER :: XLS1         = 259.532

   REAL    , PARAMETER :: XLS          = 2.85E6
   REAL    , PARAMETER :: XLV          = 2.5E6
   REAL    , PARAMETER :: XLF          = 3.50E5

   REAL    , PARAMETER :: rhowater     = 1000.
   REAL    , PARAMETER :: rhosnow      = 100.
   REAL    , PARAMETER :: rhoair0      = 1.28
!
! Now namelist-specified parameter: ccn_conc - RAS
!   REAL    , PARAMETER :: n_ccn0       = 1.0E8
!
   REAL    , PARAMETER :: piconst      = 3.1415926535897932384626433
   REAL    , PARAMETER :: DEGRAD       = piconst/180.
   REAL    , PARAMETER :: DPD          = 360./365.

   REAL    , PARAMETER ::  SVP1=0.6112
   REAL    , PARAMETER ::  SVP2=17.67
   REAL    , PARAMETER ::  SVP3=29.65
   REAL    , PARAMETER ::  SVPT0=273.15
   REAL    , PARAMETER ::  EP_1=R_v/R_d-1.
   REAL    , PARAMETER ::  EP_2=R_d/R_v
   REAL    , PARAMETER ::  KARMAN=0.4
   REAL    , PARAMETER ::  EOMEG=7.2921E-5
   REAL    , PARAMETER ::  STBOLT=5.67051E-8

   REAL    , PARAMETER ::  prandtl = 1./3.0
                                         ! constants for w-damping option
   REAL    , PARAMETER ::  w_alpha = 0.3 ! strength m/s/s
   REAL    , PARAMETER ::  w_beta  = 1.0 ! activation cfl number

       REAL , PARAMETER ::  pq0=379.90516
       REAL , PARAMETER ::  epsq2=0.2
       REAL , PARAMETER ::  a2=17.2693882
       REAL , PARAMETER ::  a3=273.16
       REAL , PARAMETER ::  a4=35.86
       REAL , PARAMETER ::  epsq=1.e-12
       REAL , PARAMETER ::  p608=rvovrd-1.
!#if ( NMM_CORE == 1 )
       REAL , PARAMETER ::  climit=1.e-20
       REAL , PARAMETER ::  cm1=2937.4
       REAL , PARAMETER ::  cm2=4.9283
       REAL , PARAMETER ::  cm3=23.5518
!       REAL , PARAMETER ::  defc=8.0
!       REAL , PARAMETER ::  defm=32.0
       REAL , PARAMETER ::  defc=0.0
       REAL , PARAMETER ::  defm=99999.0
       REAL , PARAMETER ::  epsfc=1./1.05
       REAL , PARAMETER ::  epswet=0.0
       REAL , PARAMETER ::  fcdif=1./3.
#if ( HWRF == 1 )
       REAL , PARAMETER ::  fcm=0.0
#else
       REAL , PARAMETER ::  fcm=0.00003
#endif
       REAL , PARAMETER ::  gma=-r_d*(1.-rcp)*0.5
       REAL , PARAMETER ::  p400=40000.0
       REAL , PARAMETER ::  phitp=15000.0
       REAL , PARAMETER ::  pi2=2.*3.1415926, pi1=3.1415926
       REAL , PARAMETER ::  plbtm=105000.0
       REAL , PARAMETER ::  plomd=64200.0
       REAL , PARAMETER ::  pmdhi=35000.0
       REAL , PARAMETER ::  q2ini=0.50
       REAL , PARAMETER ::  rfcp=0.25/cp
       REAL , PARAMETER ::  rhcrit_land=0.75
       REAL , PARAMETER ::  rhcrit_sea=0.80
       REAL , PARAMETER ::  rlag=14.8125
       REAL , PARAMETER ::  rlx=0.90
       REAL , PARAMETER ::  scq2=50.0
       REAL , PARAMETER ::  slopht=0.001
       REAL , PARAMETER ::  tlc=2.*0.703972477
       REAL , PARAMETER ::  wa=0.15
       REAL , PARAMETER ::  wght=0.35
       REAL , PARAMETER ::  wpc=0.075
       REAL , PARAMETER ::  z0land=0.10
#if ( HWRF == 1 ) 
       REAL , PARAMETER ::  z0max=0.01
#else
       REAL , PARAMETER ::  z0max=0.008
#endif
       REAL , PARAMETER ::  z0sea=0.001
!#endif


   !  Earth

   !  The value for P2SI *must* be set to 1.0 for Earth
   !  Although, now we may not need this declaration here (see above)
   !REAL    , PARAMETER :: P2SI         = 1.0

   !  Orbital constants:

   INTEGER , PARAMETER :: PLANET_YEAR = 365
   REAL , PARAMETER :: OBLIQUITY = 23.5
   REAL , PARAMETER :: ECCENTRICITY = 0.014
   REAL , PARAMETER :: SEMIMAJORAXIS = 1.0 ! In AU
   ! Don't know the following values, so we'll fake them for now
   REAL , PARAMETER :: zero_date = 0.0   ! Time of perihelion passage
   !  Fraction into the year (from perhelion) of the
   !  occurrence of the Northern Spring Equinox
   REAL , PARAMETER :: EQUINOX_FRACTION= 0.0

! 2012103
#if (EM_CORE == 1)
! for calls to set_tiles
   INTEGER, PARAMETER :: ZONE_SOLVE_EM = 1
   INTEGER, PARAMETER :: ZONE_SFS = 2
#endif

 CONTAINS
   SUBROUTINE init_module_model_constants
   END SUBROUTINE init_module_model_constants
 END MODULE module_model_constants
