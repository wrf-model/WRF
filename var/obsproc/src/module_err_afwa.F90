MODULE module_err_afwa

!-----------------------------------------------------------------------------!
! Assign observational error at every level from input error profiles read 
! in file "filein".
!
! Error profiles actually used are written out in files:
! PRES.txt: pressure         
! RH.txt:   relative humidity
! TEMP.txt: temperature and dew point
! UV.txt:   u and v components
! SPD.txt:  wind speed
! DIR.txt:  wind direction
!------------------------------------------------------------------------------
!  HISTORY: 
!
! F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/10/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!         06/30/2006 -   Updated for AIRS retrievals       Syed  RH  Rizvi
!         11/09/2006 -   Updated for GPS RO                Y.-R. Guo
!
!         03/07/2014 - Read errors of u/v and spd/dir
!                      for wind_uv and wind_sd separately    FENG  GAO
!
!------------------------------------------------------------------------------
   TYPE field_type
        REAL :: height
        REAL :: uv 
        REAL :: spd 
        REAL :: dir
        REAL :: temp
        REAL :: rh
        REAL :: pres
   END TYPE field_type

   TYPE obs_type
        REAL              :: level
        TYPE (field_type) :: synop
        TYPE (field_type) :: ships
        TYPE (field_type) :: buoys
        TYPE (field_type) :: metar
        TYPE (field_type) :: airep
        TYPE (field_type) :: tamdar
        TYPE (field_type) :: pilot
        TYPE (field_type) :: profl
        TYPE (field_type) :: sound
        TYPE (field_type) :: satem
        TYPE (field_type) :: satob
        TYPE (field_type) :: gpspw
        TYPE (field_type) :: ssmt1
        TYPE (field_type) :: ssmt2
        TYPE (field_type) :: ssmi
        TYPE (field_type) :: tovs
        TYPE (field_type) :: airs
        TYPE (field_type) :: other
   END TYPE obs_type

CONTAINS

SUBROUTINE obs_err_afwa (filein,nobs_max, obs, number_of_obs)

!-------------------------------------------------------------------------------

  USE module_type
  USE module_func
  USE module_err_ncep
  USE module_intp
  USE module_namelist
! USE module_rh

  IMPLICIT NONE

  CHARACTER (LEN=80)                               :: filein
  INTEGER                                          :: iunit
  INTEGER, INTENT (in)                             :: nobs_max
  TYPE (report), INTENT (inout), DIMENSION (nobs_max) :: obs
  TYPE (measurement ) , POINTER                    :: current
  INTEGER, INTENT (in)                             :: number_of_obs

  INTEGER                                          :: loop_index, is_sound
  INTEGER                                          :: nvalids, nsoundings
  INTEGER                                          :: nsurfaces, nlevels
  REAL                                             :: pres, temp, error_rh
  REAL                                             :: t9,  p9, rh9, qv9
  REAL                                             :: t,   p,  rh,  qv
  REAL                                             :: es9, qs9
  REAL                                             :: es,  qs

  real                                             :: latt, rr 
  CHARACTER (LEN=40)  :: platform
  INTEGER             :: i,fm
  CHARACTER (LEN=80)  :: keyword
  CHARACTER (LEN=80)  :: endword
  CHARACTER (LEN=80)  :: fmt_err 

  CHARACTER (LEN= 5)  :: bogus_type

  TYPE (obs_type), DIMENSION (15) :: err
  TYPE (obs_type), DIMENSION (26) :: err_wind

  INCLUDE 'platform_interface.inc'
  INCLUDE 'missing.inc'
  INCLUDE 'constants.inc'

!------------------------------------------------------------------------------!
  WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
  WRITE ( UNIT = 0, FMT = '(/,A,/)') '<AFWA> OBSERVATIONAL ERRORS PER TYPE:'

! 1. LOAD OBSERVATIONAL ERROR FROM INPUT FILE
! ===========================================

! 1.1 INITIALISE PRESSURE LEVELS FOR HEIGHT, PRESSURE, TEMPERATURE AND RH
!     -------------------------------------------------------------------

      !  Pressure levels in Pa

      err ( 1) % level =  1000. * 100.
      err ( 2) % level =   850. * 100.
      err ( 3) % level =   700. * 100.
      err ( 4) % level =   500. * 100.
      err ( 5) % level =   400. * 100.
      err ( 6) % level =   300. * 100.
      err ( 7) % level =   250. * 100.
      err ( 8) % level =   200. * 100.
      err ( 9) % level =   150. * 100.
      err (10) % level =   100. * 100.
      err (11) % level =    70. * 100.
      err (12) % level =    50. * 100.
      err (13) % level =    30. * 100.
      err (14) % level =    20. * 100.
      err (15) % level =    10. * 100.

! 1.2 INITIALISE PRESSURE LEVELS FOR WIND
!     -----------------------------------

      !  Pressure levels in Pa

      err_wind ( 1) % level =  1100. * 100.
      err_wind ( 2) % level =  1050. * 100.
      err_wind ( 3) % level =  1000. * 100.
      err_wind ( 4) % level =   950. * 100.
      err_wind ( 5) % level =   900. * 100.
      err_wind ( 6) % level =   850. * 100.
      err_wind ( 7) % level =   800. * 100.
      err_wind ( 8) % level =   750. * 100.
      err_wind ( 9) % level =   700. * 100.
      err_wind (10) % level =   650. * 100.
      err_wind (11) % level =   600. * 100.
      err_wind (12) % level =   550. * 100.
      err_wind (13) % level =   500. * 100.
      err_wind (14) % level =   450. * 100.
      err_wind (15) % level =   400. * 100.
      err_wind (16) % level =   350. * 100.
      err_wind (17) % level =   300. * 100.
      err_wind (18) % level =   250. * 100.
      err_wind (19) % level =   200. * 100.
      err_wind (20) % level =   150. * 100.
      err_wind (21) % level =   100. * 100.
      err_wind (22) % level =    50. * 100.
      err_wind (23) % level =    40. * 100.
      err_wind (24) % level =    30. * 100.
      err_wind (25) % level =    20. * 100.
      err_wind (26) % level =    10. * 100.

! 1.3 INITIALISE ERRORS TO NCEP VALUES FOR PRESSURE, TEMPERATURE AND RH
!     -----------------------------------------------------------------

      DO i = 1, 15

         !  Height error

         err (i) % synop % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % metar % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % ships % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % buoys % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % sound % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % pilot % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % profl % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % satem % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % satob % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % airep % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % tamdar % height= intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % ssmt1 % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % ssmt2 % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % ssmi  % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % tovs  % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % airs  % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         err (i) % other % height = intplin (err (i) % level, err_k (1:JPERR),&
                                                              err_h (1:JPERR))
         !  Pressure error

         err (i) % synop % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % metar % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % ships % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % buoys % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % sound % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % pilot % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % profl % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % satem % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % satob % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % airep % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % tamdar % pres= intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % ssmt1 % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % ssmt2 % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % ssmi  % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % tovs  % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % airs  % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))
         err (i) % other % pres = intplin (err (i) % level, err_k (1:JPERR),&
                                                            err_p (1:JPERR))

         !  Temperature error

         err (i) % synop % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % metar % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % ships % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % buoys % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % sound % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % pilot % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % profl % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % satem % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % satob % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % airep % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % tamdar % temp= intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % ssmt1 % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % ssmt2 % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % ssmi  % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % tovs  % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % airs  % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         err (i) % other % temp = intplog (err (i) % level, err_k (1:JPERR), &
                                                            err_t (1:JPERR))
         !  Relative humidity error

         err (i) % synop % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % metar % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % ships % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % buoys % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % sound % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % pilot % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % profl % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % satem % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % satob % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % airep % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % tamdar % rh= intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % ssmt1 % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % ssmt2 % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % ssmi  % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % tovs  % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % airs  % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))
         err (i) % other % rh = intplog (err (i) % level, err_k  (1:JPERR), &
                                                          err_rh (1:JPERR))

      ENDDO

! 1.4 INITIALISE ERRORS TO NCEP VALUES FOR WIND
!     -----------------------------------------

      DO i = 1, 26

         !  U/V component error

         err_wind (i) % synop % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % metar % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ships % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % buoys % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % sound % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % pilot % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % profl % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % satem % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % satob % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % airep % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % tamdar % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt1 % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt2 % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmi  % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % tovs  % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % other % uv = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))

         !  Wind speed error
         err_wind (i) % synop % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % metar % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % ships % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % buoys % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % sound % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % pilot % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % profl % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % satem % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % satob % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % airep % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % tamdar % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt1 % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt2 % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmi  % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % tovs  % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % other % spd = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 

         !   Wind direction error

         err_wind (i) % synop % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % metar % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % ships % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % buoys % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % sound % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % pilot % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % profl % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % satem % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % satob % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 
         err_wind (i) % airep % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % tamdar % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt1 % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmt2 % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % ssmi  % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % tovs  % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR))
         err_wind (i) % other % dir  = intplin (err_wind (i) % level, &
                                                err_k (1:JPERR),err_u (1:JPERR)) 

      ENDDO

      iunit= 99

! 1.5 HEIGHT OBSERVATIONAL ERROR
!     --------------------------

      keyword = 'HEIGHT SENSOR ERRORS'
      endword = 'UV SENSOR ERRORS'
      fmt_err = '(5(1X,F5.1))'

      CALL read_obserr_height (iunit, filein, keyword, endword, fmt_err, err)

! 1.6 WIND - UV OBSERVATIONAL ERROR
!     ------------------------

      keyword = 'UV SENSOR ERRORS'
      endword = 'SPD SENSOR ERRORS'
      fmt_err = '(7(1X,F5.1))'
      CALL read_obserr_uv (iunit, filein, keyword, endword, fmt_err, err_wind)

! 1.7 WIND - SPD OBSERVATIONAL ERROR
!     ------------------------

      keyword = 'SPD SENSOR ERRORS'
      endword = 'DIR SENSOR ERRORS'
      fmt_err = '(7(1X,F5.1))'
      CALL read_obserr_spd (iunit, filein, keyword, endword, fmt_err, err_wind)

! 1.8 WIND - DIR OBSERVATIONAL ERROR
!     ------------------------

      keyword = 'DIR SENSOR ERRORS'
      endword = 'TEMP SENSOR ERRORS'
      fmt_err = '(7(1X,F5.1))'
      CALL read_obserr_dir (iunit, filein, keyword, endword, fmt_err, err_wind)

! 1.9 TEMPERATURE OBSERVATIONAL ERROR
!     -------------------------------

      keyword = 'TEMP SENSOR ERRORS'
      endword = 'RH SENSOR ERRORS ( % )'
      fmt_err = '(5(1X,F5.1))'

      CALL read_obserr_temp (iunit, filein, keyword, endword, fmt_err, err)

! 1.10 RH OBSERVATIONAL ERROR
!     -------------------

      keyword = 'RH SENSOR ERRORS ( % )'
      endword = 'PRESSURE SENSOR ERRORS'
      fmt_err = '(5(1X,F5.2))'

      CALL read_obserr_rh (iunit, filein, keyword, endword, fmt_err, err)

! 1.11 PRESSURE OBSERVATIONAL ERROR
!     ----------------------------

      keyword = 'PRESSURE SENSOR ERRORS'
      endword = '*.'
      fmt_err = '(5(1X,F5.1))'

      CALL read_obserr_pres (iunit, filein, keyword, endword, fmt_err, err)



! 2. LOOP OVER STATIONS
! =====================

      nvalids    = 0
      nsoundings = 0
      nsurfaces  = 0
      nlevels    = 0


record: DO loop_index = 1, number_of_obs


! 2.1 Check if record valid
!     ---------------------

record_valid: IF (obs(loop_index)%info%discard) THEN

      CYCLE  record

      ELSE record_valid

! 2.2 Count valid record
!     ------------------

      nvalids = nvalids + 1 

! 2.2 Type of observation
!     ------------------

      ! Platform code xx

       READ (obs (loop_index) % info % platform (4:6), '(I3)') fm

! 2.2 interpret code
!     ---------------

       CALL  fm_decoder (fm, platform)
       if (fm == 135) bogus_type = obs (loop_index) % info % platform (8:12)

! 2.3 2D fields observational error
!     -----------------------------

      obs (loop_index) % ground  % slp % error = 200. ! 2hPa
! For TC bogus, slp error is obtained from psfc%data, name:"BOGUS" is not TC bogus:
      IF (fm == 135 .and. bogus_type /= 'BOGUS') &
        obs (loop_index) % ground  % slp % error = obs (loop_index) % ground  % psfc % data

!     Some PW or ZTD data are read with their errors, don't modify them

      IF (obs (loop_index) % ground  % pw  % error .LE. 0.) THEN
        obs (loop_index) % ground  % pw  % error = 0.2   ! 2. mm for GPSPW
        IF (fm == 114) THEN
! the unit of ZTD is also cm, so the default error is 0.5 cm (YRG, 05/09/2008):
          obs (loop_index) % ground  % pw  % error = 0.5 ! .5 cm for GP{SZTD
        ENDIF
        if (eps_equal(obs(loop_index)%ground %pw %data,missing_r,1.)) THEN
          obs (loop_index) % ground  % pw  % qc = missing
        else
          if (fm == 125) then
            obs (loop_index) % ground  % pw  % qc = 0 ! error assigned for SSMI
          else
            obs (loop_index) % ground  % pw  % qc = 1 ! error assigned.
          endif
        endif
      ENDIF

      obs (loop_index) % ground  % tb19v % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb19h % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb22v % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb37v % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb37h % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb85v % error = err (1) % ssmi % temp
      obs (loop_index) % ground  % tb85h % error = err (1) % ssmi % temp

      ! SSMI Tb varies from 2 to 5 K from channel 19 to 85

      obs (loop_index) % ground  % tb19v % error = 1.00
      obs (loop_index) % ground  % tb19h % error = 1.00
      obs (loop_index) % ground  % tb22v % error = 2.33
      obs (loop_index) % ground  % tb37v % error = 3.66
      obs (loop_index) % ground  % tb37h % error = 3.66
      obs (loop_index) % ground  % tb85v % error = 5.00
      obs (loop_index) % ground  % tb85h % error = 5.00


! 2.4 Initialise upper level pointer to surface level
!     -----------------------------------------------

      current => obs (loop_index) % surface

! 3. LOOP ON UPPER-AIR LEVELS (FIRST LEVEL IS SURFACE)
! ====================================================

      is_sound   = -1

upper_level: DO WHILE (ASSOCIATED (current))


! 3.1 Turn on the sounding flag and count the number of level
!     -------------------------------------------------------

      is_sound = is_sound + 1
      nlevels  = nlevels  + 1

! 3.2 Pressure 
!     --------

      pres = current%meas%pressure%data

      hh   = current%meas%height%data

! 3.3 Check if pressure is present
!     ----------------------------

      IF ((eps_equal (pres, missing_r, 1.))  .OR. &
          (eps_equal (pres, 0.,        1.))) THEN

           WRITE (0,'(A,A,1X,A)') 'Internal error obs ', &
                                   TRIM (obs (loop_index) % location % id), &
                                   TRIM (obs (loop_index) % location % name)
           WRITE (0,'(A,F12.3)')  'Pressure = ', pres
           STOP                   'in obs_err_afwa.F90.F'

      ENDIF


! 4.  VERTICAL INTERPOLATION OF OBSERVATIONAL ERROR UPON OBSERVATION TYPE
! =======================================================================

! 4.0 Check if wind_sd is employed
!     ----------------------------

      IF ( wind_sd ) THEN
         wind_sd_buoy   = .true.
         wind_sd_synop  = .true.
         wind_sd_ships  = .true.
         wind_sd_metar  = .true.
         wind_sd_sound  = .true.
         wind_sd_pilot  = .true.
         wind_sd_airep  = .true.
         wind_sd_qscat  = .true.
         wind_sd_tamdar = .true.
         wind_sd_geoamv = .true.
       wind_sd_profiler = .true.
      END IF

      SELECT CASE (TRIM (platform))

! 4.1 Synoptic obs
!     ------------

!     CASE  ('SYNOP','SYNOP MOBIL')
      CASE  ('SYNOP')

          !  Wind
          IF (wind_sd_synop) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % synop % dir)
             current % meas % speed % error = intplin (pres, err_wind % level,&
                                                       err_wind % synop % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % synop % uv)
             current % meas % speed % error = intplin (pres, err_wind % level,&
                                                       err_wind % synop % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % synop % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % synop % uv)

          !  Pressure

          current % meas % pressure % error = intplin  (pres,err % level,    & 
                                                        err % synop % pres)

          !  Height

          current % meas % height % error = intplin  (pres,err % level,    & 
                                                      err % synop % height)
          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % synop % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % synop % rh)

! 4.2 Ships obs
!     ---------

      CASE  ('SHIP')

          !  Wind
          IF (wind_sd_ships) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % ships % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % ships % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % ships % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % ships % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % ships % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % ships % uv)

          !  Pressure

          current % meas % pressure % error = intplin  (pres,err % level,    & 
                                                        err % ships % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % ships % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % ships % temp)

          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ships % rh)

! 4.2.1 Buoys obs
!     ---------

      CASE  ('BUOY')

          !  Wind

          IF (wind_sd_buoy) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % buoys % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % buoys % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % buoys % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % buoys % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % buoys % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % buoys % uv)

          !  Pressure

          current % meas % pressure % error = intplin  (pres,err % level,    & 
                                                        err % buoys % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % buoys % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % buoys % temp)

          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % buoys % rh)

! 4.3 Metar obs
!     ---------

!     CASE  ('METAR','SPECI')
      CASE  ('METAR')

          !  Wind

          IF (wind_sd_metar) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % metar % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % metar % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % metar % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % metar % uv)
          END IF
 
          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % metar % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % metar % uv)

          !  Pressure

          current % meas % pressure % error = intplin  (pres,err % level,    & 
                                                        err % metar % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % metar % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % metar % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error


          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % metar % rh)

! 4.4 Pilot
!     -----

!     CASE  ('PILOT','PILOT SHIP','PILOT MOBIL')
      CASE  ('PILOT')

          !  Wind

          IF (wind_sd_pilot) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % pilot % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % pilot % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % pilot % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % pilot % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % pilot % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % pilot % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level,   &
                                                       err % pilot % pres)

          !  height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % pilot % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres,err % level, &
                                                          err % pilot % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error


          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % pilot % rh)

! 4.4.1 Profilers
!     -----

      CASE  ('PROFL')

          !  Wind

          IF (wind_sd_profiler) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % profl % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % profl % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % profl % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % profl % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level,  &
                                                err_wind % profl % uv)
          current % meas % v % error = intplin (pres, err_wind % level,  &
                                                err_wind % profl % uv)
          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level,   &
                                                       err % profl % pres)

          !  height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % profl % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres,err % level, &
                                                          err % profl % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error


          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % profl % rh)
! 4.5 Sounding obs
!     ------------

!     CASE  ('TEMP','TEMP SHIP','TEMP DROP','TEMP MOBIL')
      CASE  ('SOUND')

          !  Wind

          IF (wind_sd_sound) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % sound % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % sound % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % sound % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % sound % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % sound % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % sound % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % sound % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % sound % height)
          !  Temperature

          current % meas % temperature % error = intplog (pres,err % level, &
                                                          err % sound % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % sound % rh)


! 4.5.1 Bogus Sounding
!     ----------------

!     CASE  ('BOGUS')
      CASE  ('BOGUS')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % sound % dir)

          IF (bogus_type /= 'BOGUS') then

            current % meas % speed     % error = current % meas % u % data

            current % meas % u % error = current % meas % u % data

            current % meas % v % error = current % meas % v % data
            
            current % meas % temperature % error = current % meas % dew_point % data
            
            current % meas % dew_point % error = current % meas % temperature % error

            current % meas % rh % error = current % meas % thickness % data

            current % meas % pressure % error = 0.0
            current % meas % height % error = 0.0
!            current % meas % pressure % error = intplin (pres, err % level, &
!                                                       err % sound % pres)
!
!            current % meas % height % error = intplog (pres, err % level, &
!                                                     err % sound % height)
          else

            current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                        err_wind % sound % spd)
            current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % sound % uv)
            current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % sound % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % sound % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % sound % height)
          !  Temperature

          current % meas % temperature % error = intplog (pres,err % level, &
                                                          err % sound % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % sound % rh)

          endif

! 4.6 Satem obs
!     ---------

      CASE  ('SATEM')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % satem % dir)
          current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                        err_wind % satem % spd)
          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % satem % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % satem % uv)
          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % satem % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % satem % height)

          current % meas % thickness % error = intplog (pres, err % level, &
                                                     err % satem % height)

          obs (loop_index) % ground % pw  % error = intplog (  &
                         obs(loop_index)%ground%ref_pres%data,     &
                         err % level, err % satem % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % satem % temp)

          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % satem % rh)



! 4.8 Satobs obs
!     ----------

      CASE  ('SATOB')

          !  Wind

          IF (wind_sd_geoamv) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % satob % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % satob % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % satob % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % satob % uv)  
          END IF

          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % satob % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % satob % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % satob % pres)

          !  height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % satob % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % satob % temp)

          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % satob % rh)


! 4.9 Airep
!     -----

      CASE  ('AIREP','AMDAR')

          !  Wind

          IF (wind_sd_airep) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % airep % dir) 
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % airep % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % airep % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % airep % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % airep % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % airep % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % airep % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % airep % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % airep % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % airep % rh)


! 4.10 Tamdar
!     -----

      CASE  ('TAMDAR')

          !  Wind

          IF (wind_sd_tamdar) THEN

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % tamdar % dir)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % tamdar % spd)
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level,&
                                                           err_wind % tamdar % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                           err_wind % tamdar % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % tamdar % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % tamdar % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % tamdar % pres)

          !  Height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % tamdar % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % tamdar % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % tamdar % rh)


! 4.11 SSMT1
!     -----

      CASE  ('SSMT1')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % ssmt1 % dir)
          current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                        err_wind % ssmt1 % spd)
          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt1 % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt1 % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % ssmt1 % pres)

          !  height

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ssmt1 % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % ssmt1 % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ssmt1 % rh)



! 4.12 SSMT2
!      -----

      CASE  ('SSMT2')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % ssmt2 % dir)
          current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                        err_wind % ssmt2 % spd)
          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt2 % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt2 % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % ssmt2 % pres)

          !  height

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ssmt2 % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % ssmt2 % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ssmt2 % rh)



! 4.13 SSMI
!      -----

      CASE  ('SSMI')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % ssmi % dir)

          ! SSMI wind speed can come with its own error

          IF (current % meas % speed % error .LE. 0.) &
          current % meas % speed % error = intplin (pres, err_wind % level,&
                                                    err_wind % ssmi % spd)
          current % meas % u % error = intplin (pres, err_wind  % level, &
                                                err_wind % ssmi % uv)
          current % meas % v % error = intplin (pres, err_wind  % level, &
                                                err_wind % ssmi % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err  % level, &
                                                       err % ssmi % pres)

          !  Height

          current % meas % height % error = intplog (pres, err  % level, &
                                                     err % ssmi % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err  % level, &
                                                          err % ssmi % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err  % level, &
                                                 err % ssmi % rh)



! 4.14 TOVS
!      -----

      CASE  ('TOVS')

          !  Wind

          current % meas % direction % error = intplin (pres, err_wind % level,&
                                                        err_wind % tovs % dir)
          current % meas % speed     % error = intplin (pres, err_wind % level,&
                                                        err_wind % tovs % spd)
          current % meas % u % error = intplin (pres, err_wind  % level, &
                                                err_wind % tovs % uv)
          current % meas % v % error = intplin (pres, err_wind  % level, &
                                                err_wind % tovs % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err  % level, &
                                                       err % tovs % pres)

          !  Height

          current % meas % height % error = intplog (pres, err  % level, &
                                                 err % tovs % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err  % level, &
                                                          err % tovs % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err  % level, &
                                                 err % tovs % rh)


! 4.15 ASCAT, QSCAT
!      -----

      CASE  ('ASCAT','QSCAT')

          !  Wind

          IF (wind_sd_qscat) THEN

             current % meas % direction % error = intplin (pres, err_wind % level, &
                                                           err_wind % tovs % dir) 
             current % meas % speed     % error = intplin (pres, err_wind % level, &
                                                           err_wind % tovs % spd) 
          ELSE

             current % meas % direction % error = intplin (pres, err_wind % level, &
                                                           err_wind % tovs % uv)
             current % meas % speed     % error = intplin (pres, err_wind % level, &
                                                           err_wind % tovs % uv)
          END IF

          current % meas % u % error = intplin (pres, err_wind  % level, &
                                                err_wind % tovs % uv)
          current % meas % v % error = intplin (pres, err_wind  % level, &
                                                err_wind % tovs % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err  % level, &
                                                       err % tovs % pres)

          !  Height

          current % meas % height % error = intplog (pres, err  % level, &
                                                 err % tovs % height)

          !  Temperature

          current % meas % temperature % error = intplog (pres, err  % level, &
                                                          err % tovs % temp)
          !  Dew point as temperature

          current % meas % dew_point % error = &
          current % meas % temperature % error

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err  % level, &
                                                 err % tovs % rh)

! 4.16 GPSRF, GPSEP (like SSMT/1)
!      -----

     CASE  ('GPSRF', 'GPSEP')

         !  Wind

          current % meas % direction % error = 35. ! 35 degrees
          current % meas % speed     % error = intplin (pres,err_wind % level,&
                                                        err_wind % satob % spd)
          current % meas % u % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt1 % uv)
          current % meas % v % error = intplin (pres, err_wind % level, &
                                                err_wind % ssmt1 % uv)

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % ssmt1 % pres)

          !  height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % ssmt1 % height)
  
          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % ssmt1 % temp)

          ! Refractivity (the 'dew point' is used to store the refractivity)

          latt = abs(current % meas % u % data)

 !         Refractivity (the 'dew point' is used to store the ref_obs)
          rr   =(current % meas % dew_point % data) *0.01 
 
          if ( hh >= ha ) then
            current % meas % dew_point % error =rr*ea
          else
            erh90 = (ea-ed)*(hh-ha)/(ha-hd)+ea
            if (hh >= hb) then
              erh0 = (ea-eb)*(hh-ha)/(ha-hb)+ea
            else if (hh >=hc) then
              erh0 = (eb-ec)*(hh-hb)/(hb-hc)+eb
            else
              erh0 =2.5
            endif

            err90=rr*erh90
            err0 =rr*erh0

            current % meas % dew_point % error =err90-(1-(abs(latt))/90)*(err90-err0)
          endif

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % ssmt1 % rh)

! 4.15 AIRS (like SSMT/1)
!      -----

     CASE  ('AIRSRET')

          !  Pressure

          current % meas % pressure % error = intplin (pres, err % level, &
                                                       err % airs  % pres)

          !  height

          current % meas % height % error = intplog (pres, err % level, &
                                                     err % airs  % height)
  
          !  Temperature

          current % meas % temperature % error = intplog (pres, err % level, &
                                                          err % airs  % temp)

          !  Relative humidity

          current % meas % rh % error = intplog (pres, err % level, &
                                                 err % airs  % rh)

      !CASE  ('GPSPW')


! 4.13 Others, use ncep values
!      -----------------------

      CASE DEFAULT

     ! WRITE (UNIT = 0, FMT = '(A,A,A)') &
     !'Unknown platform:',TRIM (platform),' use NCEP observational errors.'


      !  wind

      current % meas % direction % error = 35. ! 35 degrees
      current % meas % speed     % error = intplin (pres, err_k (1:JPERR), &
                                                          err_u (1:JPERR))
      current % meas % u % error = intplin (pres, err_k (1:JPERR), &
                                                  err_u (1:JPERR))
      current % meas % v % error = intplin (pres, err_k (1:JPERR), &
                                                  err_u (1:JPERR))
      !  Pressure

      current % meas % pressure % error = intplin (pres, err_k (1:JPERR), &
                                                         err_p (1:JPERR))

      !  Relative humidity
      
      current % meas % rh % error = intplog (pres, err_k  (1:JPERR), &
                                                   err_rh (1:JPERR))

      !  Height

      current % meas % height   % error = intplin (pres, err_k (1:JPERR), &
                                                         err_h (1:JPERR))

      !  Temperature

      current % meas % temperature % error = intplog (pres, err_k (1:JPERR),&
                                                            err_t (1:JPERR))

      !  Dew point as temperature

      current % meas % dew_point % error = current % meas % temperature % error



      END SELECT


! 5.  MIXING RATIO ERROR DERIVED FROM RELATIVE HUMIDITY, TEMPERATURE, AND
!     PRESSURE
! =====================================================

   IF ((.NOT.eps_equal (current % meas % pressure % data,missing_r,1.))   .AND.&
       (.NOT.eps_equal (current % meas % temperature % data,missing_r,1.)).AND.&
       (.NOT.eps_equal (current % meas % rh % data, missing_r, 1.))) THEN

!       current % meas % qv % error = E_QV_FROM_RH (current % meas % rh % error, &
!                                       current % meas % temperature % error, &
!                                       current % meas % pressure % error, &
!                                       current % meas % rh % data, &
!                                       current % meas % temperature % data, &
!                                       current % meas % pressure % data)

!       current % meas % qv % error = MAX (current % meas % qv % error, 0.001)

        p9  = current % meas % pressure % data  / 100
        p   = current % meas % pressure % error / 100
        t9  = current % meas % temperature % data
        t   = current % meas % temperature % error
        rh9 = current % meas % rh % data
        rh  = current % meas % rh % error

        es9 = 6.112 * EXP (17.67*(t9-273.15) &
                                /(t9-273.15+243.5))
        es = 6.112 &
           * EXP ( 17.67*(t9-273.15) /  (t9-273.15+243.5) ) &
           *     (+17.67* t          /  (t9-273.15+243.5)  &
                  -17.67*(t9-273.15) / ((t9-273.15+243.5)*(t9-273.15+243.5)))

        qs9 = 0.622 * es9 / (p9-es9)
        qs  = 0.622 * es  / (p9-es9) &
            - 0.622 * es9 * (p -es ) / ((p9-es9)*(p9-es9))

        qv9 = 0.01*rh9*qs9
        qv  = 0.01*rh *qs9 + 0.01*rh9 *qs

        !  Error should not be lower than 1g/kg

        current % meas % qv % error = MAX (qv, 0.001)

   ELSE

        current % meas % qv % error = missing_r

   ENDIF


!  IF (eps_equal (current % meas % temperature % data,missing_r,1.))
!      temp = t_from_p_icao (p)
!  ELSE
!      temp = current % meas % temperature % data
!  ENDIF

!  current % meas % qv % error = F_QV_FROM_RH  &
!                               (current % meas % rh % error, temp,&
!                                current % meas % pressure % data)

! 6.  GO TOP NEXT LEVEL
! =====================

      current => current%next

      ENDDO upper_level


! 7.  GO TO NEXT STATIONS
! =======================

! 7.1 Increment the surface or sounding counter
!     -----------------------------------------

      if (is_sound .gt. 0) then
          nsoundings = nsoundings + 1
      else 
          nsurfaces  = nsurfaces + 1
      endif

! 7.2 Go to next valid record
!     -----------------------

      ENDIF  record_valid

! 7.3 Go to next record
!     -----------------
      ENDDO  record


! 8.  PRINT DIAGNOSTIC
! ====================
 
      WRITE (UNIT = 0 , FMT = '(A)' ) ' '
      WRITE (UNIT = 0 , FMT = '(A,I7,A,I7,A)' ) &
     "Number of processed stations:           ",nvalids,&
     " = ",nlevels," levels."
      WRITE (UNIT = 0 , FMT = '(A,I7,A,I7,A)' ) &
     "Number of processed surface stations:   ",nsurfaces,&
     " = ",nsurfaces," surface levels."
      WRITE (UNIT = 0 , FMT = '(A,I7,A,I7,A,/)' ) &
     "Number of processed upper-air stations: ",nsoundings,&
     " = ",nlevels-nsurfaces," upper-air levels."

! 9.  END
! =======
      RETURN

      END SUBROUTINE obs_err_afwa

SUBROUTINE READ_OBSERR_HEIGHT (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read height observational error defined on the following 15 levels (in hPa):
!
!  1000,  850,  700,  500,  400, 
!   300,  250,  200,  150,  100, 
!    70,   50,   30,   20,   10
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'HEIGHT SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'UV SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(5(1X,F5.1))'
   TYPE (obs_type), DIMENSION (15) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found

      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 1 line
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1

         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If temp obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.
             WRITE (UNIT = 0, FMT = '(A,A)',ADVANCE='no') &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF
        
         !  If temp obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type


         SELECT CASE (TRIM (sensor (line1)))


         CASE ('RAOBS')  ! Sound

                WRITE (UNIT = 0, FMT = '(1X,2A)', ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % sound % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % sound % height, i = 11, 15)

         CASE ('PIBALS')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % pilot % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % pilot % height, i = 11, 15)

         CASE ('AIREPS')  ! AIREPS

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airep % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airep % height, i = 11, 15)

         CASE ('TAMDARS')  ! TAMDARS

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tamdar % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tamdar % height, i = 11, 15)

         CASE ('AIRSRET')  ! AIRS retrievals

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airs  % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airs  % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airs  % height, i = 11, 15)

         CASE ('SURFACE LAND')  ! synop and metar

                READ (line1, fmt_err) (err (i) % synop % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % synop % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % synop % height, i = 11, 15)

                READ (line1, fmt_err) (err (i) % metar % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % metar % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % metar % height, i = 11, 15)

         CASE ('SURFACE SHIP')  ! ships

                WRITE (UNIT = 0, FMT = '(1X,2A)', ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ships % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ships % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ships % height, i = 11, 15)

         CASE ('BUOY')  ! BUOY

                WRITE (UNIT = 0, FMT = '(1X,2A)', ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % buoys % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % buoys % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % buoys % height, i = 11, 15)

         CASE ('NOAA - A RETRIEVAL')   ! Satob and Satem

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satob % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satob % height, i = 11, 15)

!               READ (line1, fmt_err) (err (i) % satem % height, i =  1,  5)
!               READ (line2, fmt_err) (err (i) % satem % height, i =  6, 10)
!               READ (line3, fmt_err) (err (i) % satem % height, i = 11, 15)

         CASE ('NOAA - CLEAR PATH RETRIEVAL')   ! Satem clear path

!                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                WRITE (UNIT = 0, FMT = '(1X,2A)') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satem % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satem % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satem % height, i = 11, 15)

         CASE ('DMSP - A RETRIEVAL')   ! ssmt1, ssmt2, ssmi and tovs

!                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                WRITE (UNIT = 0, FMT = '(1X,2A)') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ssmt1 % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt1 % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt1 % height, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmt2 % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt2 % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt2 % height, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmi  % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmi  % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmi  % height, i = 11, 15)

                READ (line1, fmt_err) (err (i) % tovs  % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tovs  % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tovs  % height, i = 11, 15)

         CASE ('PROFL')  ! Profilers

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % profl % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % profl % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % profl % height, i = 11, 15)


         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % height, i =  1,  5)
                READ (line2, fmt_err) (err (i) % other % height, i =  6, 10)
                READ (line3, fmt_err) (err (i) % other % height, i = 11, 15)

         END SELECT

      ENDDO

      WRITE (UNIT = 0, FMT = '(A)') ' '

      !  If obs type is not found, print it

      IF (.NOT. found) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)

      ENDIF

! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)

! 4.  WRITE VALUES
! ================

      fileout = keyword (1:6)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')   ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  tovs ','  ssmi ',' airsr ',&
    ' other '

      DO i = 15, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % height, &
                err (i) % ships % height, &
                err (i) % buoys % height, &
                err (i) % metar % height, &
                err (i) % pilot % height, &
                err (i) % profl % height, &
                err (i) % sound % height, &
                err (i) % satem % height, &
                err (i) % satob % height, &
                err (i) % airep % height, &
                err (i) % tamdar % height, &
                err (i) % ssmt1 % height, &
                err (i) % ssmt2 % height, &
                err (i) % tovs  % height, &
                err (i) % ssmi  % height, &
                err (i) % airs  % height, &
                err (i) % other % height

      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_HEIGHT


SUBROUTINE READ_OBSERR_UV (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read wind obsevational error defined on the following 26 pressure levels
! (in hPa):
!
!   10,   20,   30,   40,   50,  100,  150,
!  200,  250,  300,  350,  400,  450,  500,
!  550,  600,  650,  700,  750,  800,  850,
!  900,  950, 1000, 1050, 1100, xxxx, yyyy
!
! The last two values are place holders.
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'UV  SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'SPD SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(7(1X,F5.1))'
   TYPE (obs_type), DIMENSION (26) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout
   REAL                :: xxxx, yyyy

!------------------------------------------------------------------------------!
! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found
      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 4 line record

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1
         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines
         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature,

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If wind obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no') &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF

         !  If wind obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type

         SELECT CASE (TRIM (sensor (line1)))

         CASE ('RAOBS')              ! Sound, synop, metar and ships

         WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % sound % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % sound % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % sound % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % synop % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % synop % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % synop % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % synop % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ships % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ships % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ships % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ships % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % metar % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % metar % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % metar % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % metar % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('PIBALS')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % pilot % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % pilot % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % pilot % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('PROFL')  ! Profilers

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % profl % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % profl % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % profl % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % profl % uv, i =  5,  1,-1), &
                                       xxxx,yyyy


         CASE ('BUOY')  ! BUOY

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % buoys % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % buoys % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % buoys % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % buoys % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('US LOW LEVEL WINDS') ! Satob, satem, ssmt1, ssmt2, ssmi, tovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satob % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satob % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satob % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % satem % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satem % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satem % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satem % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt1 % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt1 % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt1 % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt1 % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt2 % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt2 % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt2 % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt2 % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmi  % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmi  % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmi  % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmi  % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % tovs  % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tovs  % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tovs  % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tovs  % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % airep % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % airep % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % airep % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('TAMDARS')  ! TAMDAR

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tamdar % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tamdar % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tamdar % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % uv, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % other % uv, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % other % uv, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % other % uv, i =  5,  1,-1), &
                                       xxxx,yyyy

         END SELECT

      ENDDO

      WRITE (UNIT = 0, FMT = '(A)') ' '

      IF (.NOT. found) THEN

          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)

      ENDIF


! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)


! 4.  WRITE VALUES
! ================

      fileout = keyword (1:2)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF

      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(1X,16A)') ' level',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ',' tamdar ',&
    ' ssmt1 ',' ssmt2 ','  ssmi ','  tovs ',' other '

      DO i = 26, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,16(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % uv, &
                err (i) % ships % uv, &
                err (i) % buoys % uv, &
                err (i) % metar % uv, &
                err (i) % pilot % uv, &
                err (i) % profl % uv, &
                err (i) % sound % uv, &
                err (i) % satem % uv, &
                err (i) % satob % uv, &
                err (i) % airep % uv, &
                err (i) % tamdar % uv, &
                err (i) % ssmt1 % uv, &
                err (i) % ssmt2 % uv, &
                err (i) % ssmi  % uv, &
                err (i) % tovs  % uv, &
                err (i) % other % uv


      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_UV


SUBROUTINE READ_OBSERR_SPD (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read wind obsevational error defined on the following 26 pressure levels
! (in hPa):
!
!   10,   20,   30,   40,   50,  100,  150, 
!  200,  250,  300,  350,  400,  450,  500, 
!  550,  600,  650,  700,  750,  800,  850, 
!  900,  950, 1000, 1050, 1100, xxxx, yyyy
!
! The last two values are place holders.
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'SPD SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'DIR SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(7(1X,F5.1))'
   TYPE (obs_type), DIMENSION (26) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout
   REAL                :: xxxx, yyyy

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found
      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 4 line record
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1
         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If wind obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no') &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF
        
         !  If wind obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type

         SELECT CASE (TRIM (sensor (line1)))

         CASE ('RAOBS')              ! Sound, synop, metar and ships

         WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % sound % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % sound % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % sound % spd, i =  5,  1,-1), &
                                       xxxx,yyyy
                                       
                READ (line1, fmt_err) (err (i) % synop % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % synop % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % synop % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % synop % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ships % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ships % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ships % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ships % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % metar % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % metar % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % metar % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % metar % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('PIBALS')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % pilot % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % pilot % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % pilot % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('PROFL')  ! Profilers

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % profl % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % profl % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % profl % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % profl % spd, i =  5,  1,-1), &
                                       xxxx,yyyy


         CASE ('BUOY')  ! BUOY      

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % buoys % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % buoys % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % buoys % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % buoys % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('US LOW LEVEL WINDS')  ! Satobs and Satem ssmt1, ssmt2, ssmi, tovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satob % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satob % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satob % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % satem % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satem % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satem % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satem % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt1 % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt1 % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt1 % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt1 % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt2 % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt2 % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt2 % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt2 % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmi  % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmi  % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmi  % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmi  % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % tovs  % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tovs  % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tovs  % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tovs  % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % airep % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % airep % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % airep % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('TAMDARS')  ! TAMDAR

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tamdar % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tamdar % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tamdar % spd, i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % spd, i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % other % spd, i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % other % spd, i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % other % spd, i =  5,  1,-1), &
                                       xxxx,yyyy


         END SELECT

      ENDDO

      WRITE (UNIT = 0, FMT = '(A)') ' '

      IF (.NOT. found) THEN

          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)

      ENDIF


! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)


! 4.  WRITE VALUES
! ================

      fileout = keyword (1:3)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')   ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  ssmi ','  tovs ',' airsr ',' other '

      DO i = 26, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % spd, &
                err (i) % ships % spd, &
                err (i) % buoys % spd, &
                err (i) % metar % spd, &
                err (i) % pilot % spd, &
                err (i) % profl % spd, &
                err (i) % sound % spd, &
                err (i) % satem % spd, &
                err (i) % satob % spd, &
                err (i) % airep % spd, &
                err (i) % tamdar % spd, &
                err (i) % ssmt1 % spd, &
                err (i) % ssmt2 % spd, &
                err (i) % ssmi  % spd, &
                err (i) % tovs  % spd, &
                -99.9, &
                err (i) % other % spd


      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_SPD


SUBROUTINE READ_OBSERR_DIR (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read wind obsevational error defined on the following 26 pressure levels
! (in hPa):
!
!   10,   20,   30,   40,   50,  100,  150, 
!  200,  250,  300,  350,  400,  450,  500, 
!  550,  600,  650,  700,  750,  800,  850, 
!  900,  950, 1000, 1050, 1100, xxxx, yyyy
!
! The last two values are place holders.
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'DIR SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'TEMP SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(7(1X,F5.1))'
   TYPE (obs_type), DIMENSION (26) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout
   REAL                :: xxxx, yyyy

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found
      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 4 line record
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1
         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If wind obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no') &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF
        
         !  If wind obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type

         SELECT CASE (TRIM (sensor (line1)))

         CASE ('RAOBS')              ! Sound, synop, metar and ships

         WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % sound % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % sound % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % sound % dir , i =  5,  1,-1), &
                                       xxxx,yyyy
                                       
                READ (line1, fmt_err) (err (i) % synop % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % synop % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % synop % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % synop % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ships % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ships % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ships % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ships % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % metar % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % metar % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % metar % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % metar % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('PIBALS')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % pilot % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % pilot % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % pilot % dir , i =  5,  1,-1), &
                                       xxxx,yyyy


         CASE ('PROFL')  ! Profilers

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % profl % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % profl % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % profl % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % profl % dir , i =  5,  1,-1), &
                                       xxxx,yyyy


         CASE ('BUOY')  ! BUOY      

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % buoys % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % buoys % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % buoys % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % buoys % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('US LOW LEVEL WINDS')  ! Satobs and Satem ssmt1, ssmt2, ssmi, tovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satob % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satob % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satob % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % satem % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % satem % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % satem % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % satem % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt1 % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt1 % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt1 % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt1 % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmt2 % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmt2 % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmt2 % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmt2 % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % ssmi  % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % ssmi  % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % ssmi  % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % ssmi  % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

                READ (line1, fmt_err) (err (i) % tovs  % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tovs  % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tovs  % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tovs  % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % airep % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % airep % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % airep % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE ('TAMDARS')  ! Tamdar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % tamdar % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % tamdar % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % tamdar % dir , i =  5,  1,-1), &
                                       xxxx,yyyy

         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % dir , i = 26, 20,-1)
                READ (line2, fmt_err) (err (i) % other % dir , i = 19, 13,-1)
                READ (line3, fmt_err) (err (i) % other % dir , i = 12,  6,-1)
                READ (line4, fmt_err) (err (i) % other % dir , i =  5,  1,-1), &
                                       xxxx,yyyy


         END SELECT

      ENDDO

      WRITE (UNIT = 0, FMT = '(A)') ' '

      IF (.NOT. found) THEN

          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)

      ENDIF


! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)


! 4.  WRITE VALUES
! ================

      fileout = keyword (1:3)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')  ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  ssmi ','  tovs ',' airsr ',' other '

      DO i = 26, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % dir , &
                err (i) % ships % dir , &
                err (i) % buoys % dir , &
                err (i) % metar % dir , &
                err (i) % pilot % dir , &
                err (i) % profl % dir , &
                err (i) % sound % dir , &
                err (i) % satem % dir , &
                err (i) % satob % dir , &
                err (i) % airep % dir , &
                err (i) % tamdar % dir , &
                err (i) % ssmt1 % dir , &
                err (i) % ssmt2 % dir , &
                err (i) % ssmi  % dir , &
                err (i) % tovs  % dir , &
                -99.9, &
                err (i) % other % dir 


      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_DIR


SUBROUTINE READ_OBSERR_TEMP (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read temp observational error defined on the following 15 levels (in hPa):
!
!  1000,  850,  700,  500,  400, 
!   300,  250,  200,  150,  100, 
!    70,   50,   30,   20,   10
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'TEMP SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'RH SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(5(1X,F5.1))'
   TYPE (obs_type), DIMENSION (15) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found

      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 1 line
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1

         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If temp obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no' ) &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF
        
         !  If temp obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type


         SELECT CASE (TRIM (sensor (line1)))

         CASE ('SURFACE LAND') ! synop and metar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % synop % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % synop % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % synop % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % metar % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % metar % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % metar % temp, i = 11, 15)

         CASE ('SURFACE SHIP') ! ships

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ships % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ships % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ships % temp, i = 11, 15)

         CASE ('BUOY') ! BUOY

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % buoys % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % buoys % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % buoys % temp, i = 11, 15)

         CASE ('RAOBS')              ! Sound

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % sound % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % sound % temp, i = 11, 15)

         CASE ('PIBALS','PROFL')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % pilot % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % pilot % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % profl % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % profl % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % profl % temp, i = 11, 15)

         CASE ('NOAA - A RETRIEVAL')  ! Satobs and Satem

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satob % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satob % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % satem % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satem % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satem % temp, i = 11, 15)

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airep % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airep % temp, i = 11, 15)


         CASE ('TAMDARS')  ! Tamdar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tamdar % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tamdar % temp, i = 11, 15)

         CASE ('AIRSRET')  ! AIRS retrievals

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airs  % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airs  % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airs  % temp, i = 11, 15)

         CASE ('DMSP - A RETRIEVAL')  ! ssmt1, ssmt2, ssmi and ttovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ssmt1 % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt1 % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt1 % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmt2 % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt2 % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt2 % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmi  % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmi  % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmi  % temp, i = 11, 15)

                READ (line1, fmt_err) (err (i) % tovs  % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tovs  % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tovs  % temp, i = 11, 15)

         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % temp, i =  1,  5)
                READ (line2, fmt_err) (err (i) % other % temp, i =  6, 10)
                READ (line3, fmt_err) (err (i) % other % temp, i = 11, 15)

         END SELECT

      ENDDO

      WRITE (UNIT = 0, FMT = '(A)')  ' '

      !  If obs type is not found, print it

      IF (.NOT. found) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)
      ENDIF


! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)

! 4.  WRITE VALUES
! ================

      fileout = keyword (1:4)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')   ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  tovs ','  ssmi ',' airsr ', &
    ' other '


      DO i = 15, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % temp, &
                err (i) % ships % temp, &
                err (i) % buoys % temp, &
                err (i) % metar % temp, &
                err (i) % pilot % temp, &
                err (i) % profl % temp, &
                err (i) % sound % temp, &
                err (i) % satem % temp, &
                err (i) % satob % temp, &
                err (i) % airep % temp, &
                err (i) % tamdar % temp, &
                err (i) % ssmt1 % temp, &
                err (i) % ssmt2 % temp, &
                err (i) % tovs  % temp, &
                err (i) % ssmi  % temp, &
                err (i) % airs  % temp, &
                err (i) % other % temp

      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_TEMP

SUBROUTINE READ_OBSERR_RH (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read rh observational error defined on the following 15 levels (in hPa):
!
!  1000,  850,  700,  500,  400, 
!   300,  250,  200,  150,  100, 
!    70,   50,   30,   20,   10
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'RH SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'PRESSURE SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(5(1X,F5.2))'
   TYPE (obs_type), DIMENSION (15) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found

      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0)

         !  Read 3 line record
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  If rh obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no' ) &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF

        
         !  If rh obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type

         SELECT CASE (TRIM (sensor (line1)))

         CASE ('SURFACE LAND')  ! synop, ships, buoys and metar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % synop % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % synop % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % synop % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ships % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ships % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ships % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % buoys % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % buoys % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % buoys % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % metar % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % metar % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % metar % rh, i = 11, 15)

         CASE ('RAOBS')  ! Sound

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % sound % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % sound % rh, i = 11, 15)

         CASE ('PIBALS','PROFL')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % pilot % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % pilot % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % profl % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % profl % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % profl % rh, i = 11, 15)


         CASE ('NOAA - A RETRIEVAL')  ! Satobs and Satem

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satob % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satob % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % satem % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satem % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satem % rh, i = 11, 15)

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airep % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airep % rh, i = 11, 15)

         CASE ('TAMDARS')  ! Tamdar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tamdar % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tamdar % rh, i = 11, 15)

         CASE ('AIRSRET')  ! AIRS retrievals 

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airs % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airs % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airs % rh, i = 11, 15)

         CASE ('DMSP - A RETRIEVAL')  ! ssmt1, ssmt2, ssmi and ttovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ssmt1 % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt1 % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt1 % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmt2 % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt2 % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt2 % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmi  % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmi  % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmi  % rh, i = 11, 15)

                READ (line1, fmt_err) (err (i) % tovs  % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tovs  % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tovs  % rh, i = 11, 15)

         CASE DEFAULT  ! Other

                READ (line1, fmt_err) (err (i) % other % rh, i =  1,  5)
                READ (line2, fmt_err) (err (i) % other % rh, i =  6, 10)
                READ (line3, fmt_err) (err (i) % other % rh, i = 11, 15)

         END SELECT

      ENDDO

      WRITE (0,'(A)') ' '

      !  If obs type is not found, print it

      IF (.NOT. found) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)
      ENDIF


! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)


! 4.  WRITE VALUES
! ================

      fileout = keyword (1:2)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')   ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  tovs ','  ssmi ',' airsr ',&
    ' other '

      DO i = 15, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % rh, &
                err (i) % ships % rh, &
                err (i) % buoys % rh, &
                err (i) % metar % rh, &
                err (i) % pilot % rh, &
                err (i) % profl % rh, &
                err (i) % sound % rh, &
                err (i) % satem % rh, &
                err (i) % satob % rh, &
                err (i) % airep % rh, &
                err (i) % tamdar % rh, &
                err (i) % ssmt1 % rh, &
                err (i) % ssmt2 % rh, &
                err (i) % tovs  % rh, &
                err (i) % ssmi  % rh, &
                err (i) % airs  % rh, &
                err (i) % other % rh

      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_RH

SUBROUTINE READ_OBSERR_PRES (iunit, filein, keyword, endword, fmt_err, err)
!------------------------------------------------------------------------------!
!
! Read pressure observational error defined on the following 15 levels (in hPa):
!
!  1000,  850,  700,  500,  400, 
!   300,  250,  200,  150,  100, 
!    70,   50,   30,   20,   10
!
!------------------------------------------------------------------------------!
   IMPLICIT NONE
!------------------------------------------------------------------------------!

   INTEGER             :: iunit   != 99
   CHARACTER (LEN=80)  :: filein  != 'obserr.txt'
   CHARACTER (LEN=80)  :: keyword != 'PRESSURE SENSOR ERRORS'
   CHARACTER (LEN=80)  :: endword != 'dire SENSOR ERRORS'
   CHARACTER (LEN=80)  :: fmt_err != '(5(1X,F5.1))'
   TYPE (obs_type), DIMENSION (15) :: err

   INTEGER             :: io_error, i
   CHARACTER (LEN=80)  :: line1, line2, line3, line4
   LOGICAL             :: found, wind
   CHARACTER (LEN=80)  :: fileout

!------------------------------------------------------------------------------!

! 1.  OPEN INPUT FILE
! ===================

      OPEN (UNIT = iunit , FILE = filein , FORM = 'FORMATTED'  , &
            ACTION = 'READ' , STATUS = 'OLD', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open input observational error file ',TRIM (filein)
          STOP
      ENDIF


! 2.  READ DATA
! =============

!     Read file until keyword is found

      found   = .FALSE.
      wind    = .FALSE.
      io_error= 0

      DO WHILE (io_error .EQ. 0.)

         !  Read 1 line
     
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line1

         !  Exit when error or at end of file

         IF (io_error .NE. 0) EXIT

         !  Winds are given over 4 lines, any other data are given over 3 lines

         IF ((TRIM (obstype (line1)) .EQ. 'UV SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'SPD SENSOR ERRORS') .OR. &
             (TRIM (obstype (line1)) .EQ. 'DIR SENSOR ERRORS')) THEN

             wind = .TRUE.

         !  Winds ends at temperature, 

         ELSE IF (TRIM (obstype (line1)) .EQ. 'TEMP SENSOR ERRORS') THEN

             wind = .false.

         ENDIF

         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line2
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line3
         IF (wind) &
         READ (UNIT = iunit, IOSTAT = io_error, FMT = '(A)') line4

         !  If temp obstype is found, read error data

         IF (TRIM (obstype (line1)) .EQ. TRIM (keyword)) THEN

             found = .TRUE.

             WRITE (UNIT = 0, FMT = '(/,A,A)', ADVANCE = 'no' ) &
                    TRIM (obstype (line1)),': '

         ELSE IF (((TRIM (obstype (line1)) .EQ. TRIM (endword)) .OR. &
                   (line1 (1:2) .EQ. '*.'))) THEN

             EXIT

         ENDIF
        
         !  If temp obstype is not found, keep on reading

         IF (.NOT. found) CYCLE

         !  Keyword has been found, Error at mandatory pressure levels follow
         !  Break down data upon obs type


         SELECT CASE (TRIM (sensor (line1)))

         CASE ('SURFACE LAND') ! synop and metar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % synop % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % synop % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % synop % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % metar % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % metar % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % metar % pres, i = 11, 15)

         CASE ('SURFACE SHIP') ! ships

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ships % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ships % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ships % pres, i = 11, 15)

         CASE ('RAOBS') ! Sounding

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % sound % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % sound % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % sound % pres, i = 11, 15)

         CASE ('PIBALS','PROFL')  ! Pilot

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % pilot % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % pilot % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % pilot % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % profl % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % profl % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % profl % pres, i = 11, 15)

         CASE ('NOAA - A RETRIEVAL')  !  Satobs and Satem

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % satob % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satob % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satob % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % satem % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % satem % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % satem % pres, i = 11, 15)

         CASE ('AIREPS')  ! Airep

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airep % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airep % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airep % pres, i = 11, 15)

         CASE ('TAMDARS')  ! Tamdar

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % tamdar % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tamdar % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tamdar % pres, i = 11, 15)

         CASE ('AIRSRET')  ! AIRS retrievals

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % airs % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % airs % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % airs % pres, i = 11, 15)

         CASE ('DMSP - A RETRIEVAL')  !  ssmt1, ssmt2, ssmi and ttovs

                WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
                       TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % ssmt1 % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt1 % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt1 % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmt2 % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmt2 % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmt2 % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % ssmi  % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % ssmi  % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % ssmi  % pres, i = 11, 15)

                READ (line1, fmt_err) (err (i) % tovs  % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % tovs  % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % tovs  % pres, i = 11, 15)

         CASE DEFAULT  ! Other

!               WRITE (UNIT = 0, FMT = '(1X,2A)',ADVANCE='no') &
!                      TRIM (sensor (line1)),','

                READ (line1, fmt_err) (err (i) % other % pres, i =  1,  5)
                READ (line2, fmt_err) (err (i) % other % pres, i =  6, 10)
                READ (line3, fmt_err) (err (i) % other % pres, i = 11, 15)

         END SELECT

      ENDDO

      WRITE (0,'(A)') ' '

      !  If obs type is not found, print it

      IF (.NOT. found) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A)') &
        ' Observational errors for ',TRIM (keyword)
          WRITE (UNIT = 0, FMT = '(A,A,/)') &
        ' were not found in file ',  TRIM (filein)
      ENDIF

! 3.  CLOSE INPUT FILE
! ====================

      CLOSE (UNIT = iunit)

! 4.  WRITE VALUES
! ================

      fileout = keyword (1:4)//'.txt'

      OPEN (UNIT = iunit , FILE = fileout , FORM = 'FORMATTED'  , &
            ACTION = 'WRITE' , STATUS = 'REPLACE', IOSTAT = io_error)

      IF (io_error .NE. 0) THEN
          WRITE (UNIT = 0, FMT = '(/,A,A,/)') &
         'Unable to open output observational error file ',TRIM (fileout)
          STOP
      ENDIF


      WRITE (UNIT = iunit, FMT = '(20X,A)') TRIM (keyword)

      WRITE (UNIT = iunit, FMT = '(18A)')   ' level ',&
    ' synop ','  ship ','  buoy ',' metar ',' pilot ',' profl ',&
    ' sound ',' satem ',' satob ',' airep ','tamdar ',&
    ' ssmt1 ',' ssmt2 ','  tovs ','  ssmi ',' airsr ', &
    ' other '

      DO i = 15, 1, -1
         WRITE (UNIT = iunit, FMT = '(F6.0,17(2X,F5.1))') &
                err (i) % level / 100., &
                err (i) % synop % pres / 100., &
                err (i) % ships % pres / 100., &
                err (i) % buoys % pres / 100., &
                err (i) % metar % pres / 100., &
                err (i) % pilot % pres / 100., &
                err (i) % profl % pres / 100., &
                err (i) % sound % pres / 100., &
                err (i) % satem % pres / 100., &
                err (i) % satob % pres / 100., &
                err (i) % airep % pres / 100., &
                err (i) % tamdar % pres / 100., &
                err (i) % ssmt1 % pres / 100., &
                err (i) % ssmt2 % pres / 100., &
                err (i) % tovs  % pres / 100., &
                err (i) % ssmi  % pres / 100., &
                err (i) % airs  % pres / 100., &
                err (i) % other % pres / 100.

      ENDDO

      CLOSE (UNIT = iunit)

END SUBROUTINE READ_OBSERR_PRES

FUNCTION obstype (line) RESULT (f_obstype)
!------------------------------------------------------------------------------!

      ! Read in a line the string present after keyword 'BOGUS'

!------------------------------------------------------------------------------!
      IMPLICIT NONE
      CHARACTER (LEN= 80) :: line, f_obstype
      INTEGER            :: b,c
!------------------------------------------------------------------------------!

!  Find keyword bogus

        DO c = 1, LEN_TRIM (line) - 5
           IF (line (c:c+4) .EQ. 'BOGUS') EXIT
        ENDDO

        c = c + 5

!  Skip blank until next word

        DO b = c, LEN_TRIM (line)
           IF (line (b:b) .NE. ' ') EXIT
        ENDDO

!  String follows

        f_obstype = TRIM (line (b:LEN_TRIM (line)))

END FUNCTION obstype

!------------------------------------------------------------------------------!
FUNCTION sensor (line) RESULT (f_sensor)

      ! Read first in a string after numbers

!------------------------------------------------------------------------------!
      IMPLICIT NONE
      CHARACTER (LEN= 80) :: line, f_sensor
      INTEGER             :: b,c
!------------------------------------------------------------------------------!

!  Find the first non-blank, non point and non-number character

       DO c = 1, LEN_TRIM (line)
       IF (((iachar (line(c:c)) .NE. 32)  .AND. &
            (iachar (line(c:c)) .NE. 46)) .AND. &
           ((iachar (line(c:c)) .LT. 48)  .OR.  &   
            (iachar (line(c:c)) .GT. 57)))      &
             EXIT
       ENDDO

!  String is start after blank untill end of line

        f_sensor = line (c:LEN_TRIM (line))

END FUNCTION sensor

END MODULE MODULE_ERR_AFWA
