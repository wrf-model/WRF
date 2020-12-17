      MODULE MODULE_ERR_NCEP
!!!--------------------------------------------------------------------------CCC
!!!
!!!                       MODULE MODULE_NCEP_ERR
!!!                     **************************
!!!
!!!  PURPOSE:
!!!  -------
!!!     OBSERVATIONAL ERROR DEFINITION
!!!
!!   METHOD:
!!   ------
!!      OBSERVATIONAL ERROR IS DEFINED AT 1000, 700, 500, 300, 100, 50mb
!!      FOR U, V, T, RH AND P
!!      VALUES AT ACTUAL OBS POSITION ARE OBTAINED BY INTERPOLATION BETWEEN 
!!      THESE LEVELS (LOGARITHMIC FOR U ANF V) AND LINEAR FOR (T AND RH)
!!      ERRORS ARE EXTRAPOLATED BELOW 1000mb AND ABOVE 50mb
!!
!!   INPUT:
!!   -----
!!   OUTPUT:
!!   ------
!!   COMMON:
!!   -------
!!   EXTERNAL:         NO
!!   --------
!!
!!   REFERENCES:
!!   -----------
!!   PARRISH S. AND J. DERBER 1992: THE NATIONAL METEOROLOGICAL CENTERS
!!                                  SPECTRAL STATISTICAL ANALYSIS-INTERPOLATION
!!                                  SYSTEM. MONTH. WEA. REV., 120,1747-1763.
!!
!!
!!  MODIFICATIONS:
!!   ------------
!!       ORIGINAL :  99-08 (F. VANDENBERGHE)
!!----------------------------------------------------------------------------CC
!
      INTEGER    JPERR
      PARAMETER (JPERR = 6)
      REAL ERR_K (0:JPERR+1)
      REAL ERR_U (0:JPERR+1), ERR_V  (0:JPERR+1)
      REAL ERR_T (0:JPERR+1), ERR_RH (0:JPERR+1)
      REAL ERR_P (0:JPERR+1)
      REAL ERR_H (0:JPERR+1)

!     PARAMETER (JPERR = 6)
!     REAL ERR_K (1:JPERR)
!     REAL ERR_U (1:JPERR), ERR_V  (1:JPERR)
!     REAL ERR_T (1:JPERR), ERR_RH (1:JPERR)
!     REAL ERR_P (1:JPERR)
!
!...NCEP ERRORS (U in m/s, V in m/s, T in K, H in %, P in Pa)
!...PRESSURE IS OBTAINED FROM SURFACE VALUE LINEARLY DECREASING 
!...TO 10% OF ITS VALUE AT 10hPA
!...RH HAS BEEN DIVIDED BY 2
!
      DATA ERR_K  (1:JPERR) &
                 /    100000.,70000.,50000.,30000.,10000.,5000. /
      DATA ERR_U  (1:JPERR) &
                    /   1.4,   2.4,   2.8,   3.4,   2.5,  2.7   /
      DATA ERR_V  (1:JPERR) &
                    /   1.4,   2.4,   2.8,   3.4,   2.5,  2.7   /
      DATA ERR_T  (1:JPERR) &
                    /   1.8,   1.3,   1.3,   2.0,   3.1,  4.0   /
      DATA ERR_RH (1:JPERR) &
                     / 10.0,  10.0,  10.0,  10.0,  10.0,  10.0  /
      DATA ERR_P  (1:JPERR) &
                    / 100.0,  72.7,  54.5,  36.4,  18.2,  13.6  /
      DATA ERR_H  (1:JPERR) &
                    /   7.0,   8.0,   8.6,  18.8,  39.4,  59.3  /

! REFRACTIVITY ERROR INTERPOLATION dN = NBOT*EXP[-Z/AA] 
! (Adapted from Francois Vandenberghe by Y.-R. Guo  06/05/2004)

!      REAL, PARAMETER :: PMAX = 1000., PMSL = 100000. ! TOP AND BOTTOM PRES PA
!      REAL, PARAMETER :: NTOP =    3., NBOT =     10. ! TOP AND BOTTOM ERROR IN
                                                      ! REFRACTIVITY IN N UNITS
!      REAL :: AA

      real:: ntop , nbot 
      real:: aa0 , aa90 , nbot0 ,nbot90  
      real, parameter :: pmax    = 15000. , pmsl   = 100000.
      real, parameter :: ntopp   = 0.27   , nbotp0   = 3.0   , nbotp90=1.5
      REAL :: AA

       real :: erh90 ,erh0,err0,err90,hh
       real,parameter :: ha = 12000. , hb = 5500. , hc = 2500., hd=0.
       real,parameter :: ea = 0.3    , eb = 1.3   , ec = 2.5  , ed = 1.5

      CONTAINS

SUBROUTINE obs_err_ncep (nobs_max, obs, number_of_obs)

!-------------------------------------------------------------------------------

  USE module_type
  USE module_func
  USE module_intp

  IMPLICIT NONE

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

  INCLUDE 'missing.inc'
  INCLUDE 'constants.inc'

!------------------------------------------------------------------------------!

  WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
  WRITE ( UNIT = 0, FMT = '(A,/)') '<NCEP> OBSERVATIONAL ERROR:'

! REFRACTIVITY ERROR INTERPOLATION dN = NBOT*EXP[-Z/AA]
! (Adapted from Francois Vandenberghe by Y.-R. Guo  06/05/2004)

!  AA = (PMAX - PMSL) / LOG (NTOP/NBOT)

! 1. LIST RECORDS
! ===============

      nvalids    = 0
      nsoundings = 0
      nsurfaces  = 0
      nlevels   = 0


record: DO loop_index = 1, number_of_obs


! 1.1 CHECK IF RECORD VALID
!     ---------------------

record_valid: IF (obs(loop_index)%info%discard ) THEN

      CYCLE  record

      ELSE record_valid

! 1.2 COUNT VALID RECORD
!     ------------------

      nvalids = nvalids + 1 

! 1.3 2D FIELDS
!     ---------

      obs (loop_index) % ground  % slp % error = 200. ! 2hPa

!     Some PW or ZTD data are read with their errors, don't modify them

      IF (obs (loop_index) % ground  % pw  % error .LE. 0.) THEN
        IF (obs(loop_index)%info%platform(1:6) == 'FM-114') THEN
          obs (loop_index) % ground  % pw  % error = 0.005 ! .5 cm for GPSZTD
        ELSE 
          obs (loop_index) % ground  % pw  % error = 0.2  ! 2. mm for GPSPW
        ENDIF
        if (eps_equal(obs(loop_index)%ground %pw %data,missing_r,1.)) THEN
          obs (loop_index) % ground  % pw  % qc = missing
        else
          obs (loop_index) % ground  % pw  % qc = 1  ! error assigned.
        endif
      ENDIF

      ! SSMI Tb

!     obs (loop_index) % ground  % tb19v % error = err_t (1)
!     obs (loop_index) % ground  % tb19h % error = err_t (1)
!     obs (loop_index) % ground  % tb22v % error = err_t (1)
!     obs (loop_index) % ground  % tb37v % error = err_t (1)
!     obs (loop_index) % ground  % tb37h % error = err_t (1)
!     obs (loop_index) % ground  % tb85v % error = err_t (1)
!     obs (loop_index) % ground  % tb85h % error = err_t (1)

      ! SSMI Tb varies from 2 to 5 K from channel 19 to 85

      obs (loop_index) % ground  % tb19v % error = 1.00
      obs (loop_index) % ground  % tb19h % error = 1.00
      obs (loop_index) % ground  % tb22v % error = 2.33
      obs (loop_index) % ground  % tb37v % error = 3.66
      obs (loop_index) % ground  % tb37h % error = 3.66
      obs (loop_index) % ground  % tb85v % error = 5.00
      obs (loop_index) % ground  % tb85h % error = 5.00

! 1.4 INITIALISE UPPER LEVEL POINTER TO SURFACE LEVEL
!     -----------------------------------------------

      current => obs (loop_index) % surface


! 2. LOOP ON UPPER-AIR LEVELS (FIRST LEVEL IS SURFACE)
! ====================================================

      is_sound   = -1

upper_level: DO WHILE (ASSOCIATED (current))


! 2.1 TURN ON THE SOUNDING FLAG AND COUNT THE NUMBER OF LEVEL
!     -------------------------------------------------------

      is_sound = is_sound + 1
      nlevels  = nlevels  + 1


! 2.2 CHECK IF OBS PRESSURE IS NOT MISSING
!     ------------------------------------

      IF ((eps_equal (current%meas%pressure%data, missing_r, 1.))  .OR. &
          (eps_equal (current%meas%pressure%data, 0.,        1.))) THEN

           WRITE (0,'(A,A,1X,A)') 'Internal error obs ', &
                                   TRIM (obs (loop_index) % location % id), &
                                   TRIM (obs (loop_index) % location % name)
           WRITE (0,'(A,F12.3)')  'Pressure = ', current%meas%pressure%data
           STOP                   'in obs_err_ncep.F'

      ENDIF

! 2.3 PRESSURE 
!     --------

      pres = current%meas%pressure%data


! 3.  VERTICAL INTERPOLATION OF NCEP OBSERVATIONAL ERROR
! ======================================================

! 3.1 WIND DIRECTION FIXE TO 5 DEGREES
!     --------------------------------

      current % meas % direction % error = 5.

! 3.2 WIND SPEED
!     ----------

!     Some wind speed data are read with their errors, don't modify them

      IF (current % meas  % speed % error .LE. 0.) THEN
          current % meas % speed % error = intplin (pres, err_k (1:JPERR), &
                                                          err_u (1:JPERR))
      ENDIF

! 3.3 U-WIND COMPONENTS AS WIND SPEED
!     -------------------------------

      current % meas % u % error = intplin (pres, err_k (1:JPERR), &
                                                  err_u (1:JPERR))

! 3.4 V-WIND COMPONENT AS WIND SPEED
!     ------------------------------

      current % meas % v % error = intplin (pres, err_k (1:JPERR), &
                                                  err_v (1:JPERR))

! 3.5 PRESSURE
!     --------
      current % meas % pressure % error = intplin (pres, err_k (1:JPERR), &
                                                         err_p (1:JPERR))
! 3.6 HEIGHT
!     -------
      current % meas % height   % error = intplog (pres, err_k (1:JPERR), &
                                                         err_h (1:JPERR))

! 3.7 TEMPERATURE
!     -----------
      current % meas % temperature % error = intplog (pres, err_k (1:JPERR), &
                                                            err_t (1:JPERR))

! 3.8 DEW POINT AS TEMPERATURE
!     ------------------------
      current % meas % dew_point % error = current % meas % temperature % error

! 3.9 RELATIVE HUMIDITY
!     -----------------

!     current % meas % rh % error = ERROR_INTP (pres,'RH','LINEAR')
      current % meas % rh % error = intplog (pres, err_k (1:JPERR), &
                                                   err_rh (1:JPERR))

! 3.10 MIXING RATIO ERROR DERIVED FROM RELATIVE HUMIDITY, TEMPERATURE, PRESSURE
! =============================================================================

   IF ((.NOT.eps_equal (current % meas % pressure % data,missing_r,1.))   .AND.&
       (.NOT.eps_equal (current % meas % temperature % data,missing_r,1.)).AND.&
       (.NOT.eps_equal (current % meas % rh % data, missing_r, 1.))) THEN

!       Linearize the Qv calculation

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
    

! 4.  GO TOP NEXT LEVEL
! =====================

      current => current%next

      ENDDO upper_level


! 5.  GO TO NEXT RECORD
! ======================

! 5.1 INCREMENT THE SURFACE OR SOUNDING COUNTER
!     -----------------------------------------

      if (is_sound .gt. 0) then
          nsoundings = nsoundings + 1
      else 
          nsurfaces  = nsurfaces + 1
      endif

! 5.2 GO TO NEXT VALID RECORD
!     -----------------------

      ENDIF  record_valid

! 5.3 GO TO NEXT RECORD
!     -----------------
      ENDDO  record


! 6.  PRINT DIAGNOSTIC
! ====================
 
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A)' ) &
     "Number of processed stations:           ",nvalids,&
     " = ",nlevels," levels."
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A)' ) &
     "Number of processed surface stations:   ",nsurfaces,&
     " = ",nsurfaces," surface levels."
      WRITE (UNIT = 0 , FMT = '(A,I6,A,I6,A,/)' ) &
     "Number of processed upper-air stations: ",nsoundings,&
     " = ",nlevels-nsurfaces," upper-air levels."

! 7.  END
! =======
      RETURN

      END SUBROUTINE obs_err_ncep

      FUNCTION ERROR_INTP (PZ, CDVAR, CDTYPE)
!!!--------------------------------------------------------------------------CCC
!!!
!!!                       FUNCTION ERROR_INTP
!!!                     ***********************
!!!
!!!  PURPOSE:
!!!  -------
!!!     VERTICAL INTERPOLATION OF OBSERVATIONAL ERROR
!!!
!!   METHOD:
!!   ------
!!      LINEAR OR LOGARITHMIC VERTICAL INTERPOLATION
!!      OUT OF BOUND LOCATIONS ARE EXTRAPOLATED
!!
!!   INPUT:
!!   -----
!!      PZ:            VERTICAL POSITION WHERE O INTERPOLATE
!!      CDTYPE         TYPE OF VARIABLE (CURRENTLY 'U','V','T','RH','P')
!!      CDTYPE         TYPE OF INTERPOLATION ('LOGAR' OR 'LINEAR')
!!
!!   OUTPUT:
!!   ------
!!      ERROR_INTP:  VALUE OF ERROR AT VERTICAL LOCATION PZ
!!
!!   COMMON:
!!   -------
!!      SEE MODULE MODULE_NCEP_ERR
!!
!!   EXTERNAL:         NO                   
!!   --------
!!
!!   REFERENCES:
!!   -----------
!!   PARRISH S. AND J. DERBER 1992: THE NATIONAL METEOROLOGICAL CENTERS
!!                                  SPECTRAL STATISTICAL ANALYSIS-INTERPOLATION
!!                                  SYSTEM. MONTH. WEA. REV., 120,1747-1763.
!!  MODIFICATIONS:
!!   ------------
!!       ORIGINAL :  99-08 (F. VANDENBERGHE)
!!----------------------------------------------------------------------------CC
!
! ARGUMENT
! -------
      REAL          PZ
      CHARACTER*(*) CDVAR, CDTYPE
      REAL ERROR_INTP
      INTEGER IK, JK
      REAL DZINF, DZSUP, DZDIF
!
! ERRORS ON MANDATORY LEVELS DEFINED IN MODULE MODULE_NCEP_ERR
!
!------------------------------------------------------------------------------C
!
!
! 1.  EXTRAPOLATE ERROR DEFINITION BELOW AND BEYOND LEVELS
! --------------------------------------------------------
!
      ERR_K  (0)       = 200000.      ! SET HIGHEST PRESSURE TO 20000 PA
      ERR_K  (JPERR+1) = 1.           ! SET LOWEST  PRESSURE TO     1 Pa
!
      ERR_U  (0)       = ERR_U  (1)
      ERR_U  (JPERR+1) = ERR_U  (JPERR)
      ERR_V  (0)       = ERR_V  (1)
      ERR_V  (JPERR+1) = ERR_V  (JPERR)
      ERR_T  (0)       = ERR_T  (1)
      ERR_T  (JPERR+1) = ERR_T  (JPERR)
      ERR_RH (0)       = ERR_RH (1)
      ERR_RH (JPERR+1) = ERR_RH (JPERR)
      ERR_P  (0)       = ERR_P  (1)
      ERR_P  (JPERR+1) = ERR_P  (JPERR)
      ERR_H  (0)       = ERR_H  (1)
      ERR_H  (JPERR+1) = ERR_H  (JPERR)
!
!
! 2.  FIND INDEX FOR INTERPOLATION (PRESSURE IS DECREASING WITH INDEX K)
! ---------------------------------------------------------------------
!                        Z
!     2000      1000     +      700    500     300    100     50    1   P in mb 
!      |-----------------|-------------------------------------------------->
!      0         K       |      K+1     3       4      5      6     7   K
!                |<----->+<----->|
!                  DZINF   DZSUP
!           
!             VAL = DZINF * ERR_NCEP (K+1) + DZSUP * ERR_NCEP (K)
! ---------------------------------------------------------------------
!
      IK = -1
!
      DO JK = 0, JPERR
!
      IF ((ERR_K (JK) .GE. PZ) .AND. (PZ .GT. ERR_K (JK+1))) &
      THEN
           IK = JK
           EXIT
      ENDIF
!
      ENDDO
!
      IF ((IK .LT. 0)  .OR. (IK .GT. JPERR+1)) THEN
           WRITE (0,'(/,A,A,A,F10.2,/)') ' ERROR OBS ',CDVAR, &
                                         ' POSITION PZ = ',PZ 
           STOP  ' in error_intp.F'
      ENDIF
!
!
! 3.  WHEN SPECIFIED: LOGARITHMIC INTERPOLATION COEFICIENTS
! ---------------------------------------------------------
!
      IF ((CDTYPE (1:5) .EQ. 'LOGAR') .OR. &
          (CDTYPE (1:5) .EQ. 'logar')) THEN
!
           DZINF = -LOG (PZ / ERR_K (IK  ))
           DZSUP =  LOG (PZ / ERR_K (IK+1))
           DZDIF =  LOG (ERR_K (IK) / ERR_K (IK+1))
!
      ELSE
!
! 4.  OR DEFAULT IS LINEAR INTERPOLATION COEFICIENTS
!     ----------------------------------------------
!
           DZINF = -(PZ - ERR_K (IK  ))
           DZSUP =   PZ - ERR_K (IK+1)
           DZDIF =   ERR_K (IK) - ERR_K (IK+1)
!
      ENDIF
!
! 5.  INTERPOLATION
!     -------------
!
      DZINF = DZINF / DZDIF
      DZSUP = DZSUP / DZDIF
!
      IF (CDVAR (1:1) .EQ. 'U') THEN
!
           ERROR_INTP = DZSUP * ERR_U  (IK)  &
                      + DZINF * ERR_U  (IK+1)
!
      ELSE IF (CDVAR (1:1) .EQ. 'V') THEN
!
           ERROR_INTP = DZSUP * ERR_V  (IK)  &
                      + DZINF * ERR_V  (IK+1)
!
      ELSE IF (CDVAR (1:1) .EQ. 'T') THEN
!
           ERROR_INTP = DZSUP * ERR_T  (IK)  &
                      + DZINF * ERR_T  (IK+1)
!
      ELSE IF (CDVAR (1:2) .EQ. 'RH') THEN
!
           ERROR_INTP = DZSUP * ERR_RH (IK)  &
                      + DZINF * ERR_RH (IK+1)
!
      ELSE IF (CDVAR (1:1) .EQ. 'P') THEN
!
           ERROR_INTP = DZSUP * ERR_P  (IK)  &
                      + DZINF * ERR_P  (IK+1)
!
      ELSE IF (CDVAR (1:1) .EQ. 'H') THEN
!
           ERROR_INTP = DZSUP * ERR_H  (IK)  &
                      + DZINF * ERR_H  (IK+1)
!
      ELSE 
           WRITE (0,'(/,A,A,/)') ' ERROR VARIABLE TYPE ',CDVAR
           STOP  ' in error_intp.F'
      ENDIF
!
!
! 6.  EXIT
! ---------
!
      RETURN
      END FUNCTION ERROR_INTP

      END MODULE MODULE_ERR_NCEP
