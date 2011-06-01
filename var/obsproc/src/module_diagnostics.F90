MODULE module_diagnostics

!------------------------------------------------------------------------------
! Compute wind components, dew point, relative humidity and mixing ratio
! when the corresponding input variables are present.
!
! D. GILL,         April 1998
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------

CONTAINS
!------------------------------------------------------------------------------

 SUBROUTINE derived_quantities (max_number_of_obs , obs , number_of_obs)
!------------------------------------------------------------------------------

   USE module_type
   USE module_func
   USE module_mm5
   USE module_map
   USE module_intp

   IMPLICIT NONE

   INTEGER,       INTENT (in)                   :: max_number_of_obs
   TYPE (report), DIMENSION (max_number_of_obs) :: obs
   INTEGER ,      INTENT (in)                   :: number_of_obs

   TYPE (measurement), POINTER                  :: current
   INTEGER                                      :: i, fm
   REAL                                         :: xj, yi
   CHARACTER (LEN= 5)  :: bogus_type
   INCLUDE 'missing.inc'
!-------------------------------------------------------------------

      WRITE (0,'(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'DIAGNOSTICS: U, V, RH, QV etc...:'

! 1.  DERIVED MODEL DEPENDENT VARIABLES
! =====================================
 
loop_all_obs: &
      DO i = 1, number_of_obs

        READ (obs(i) % info % platform (4:6), '(I3)') fm
        if (fm == 135) bogus_type = obs (i) % info % platform (8:12)
 
valid_obs1:&
         IF (.NOT. obs(i)%info%discard) THEN

! 1.2 Loop over upper levels
!     ----------------------

valid_obs2:&
        IF (ASSOCIATED (obs(i)%surface)) THEN

         current => obs(i)%surface

all_levels:&
               DO WHILE (ASSOCIATED (current))

                  if ( fm == 281 ) then
!  Set missing_r to rh (originally it is max_likehood_est for Quikscat):
                    current%meas%rh%data = missing_r
                    current%meas%rh%qc   = missing
                  else
!  With fm = 116 (gpsref) and fm = 118 (gpseph), the field "dew_point" was 
! used to store gpsref, and ssmi_retrieval fm=125, no "diagnostics_moist" is allowed.
                    if (fm /= 116 .and. fm /= 118.and.fm /=125) then
                      if (fm ==135.and.bogus_type /='BOGUS') then
!                        nothing to do if TCBOG...
                      else  
                         CALL diagnostics_moist (current%meas)
                         CALL diagnostics_wind  (current%meas, &
                                           obs(i)%location%longitude)
                      endif
                    endif
                  endif
                  current => current%next

               ENDDO all_levels
         
         ENDIF valid_obs2

         ENDIF valid_obs1

      ENDDO loop_all_obs

 END SUBROUTINE derived_quantities
!------------------------------------------------------------------------------

SUBROUTINE diagnostics_moist (new)
!------------------------------------------------------------------------------

!  This routine computes the derived diagnostic moisture fields Td, Rh, Qv

   USE module_type
   USE module_func

   IMPLICIT NONE

   TYPE ( meas_data )    :: new

   REAL                  :: t , td, p
   REAL                  :: es, qs

   INCLUDE 'constants.inc'
   INCLUDE 'missing.inc'

!------------------------------------------------------------------------------

   !  Given a valid temperature and dew point, the relative humidity is
   !  computed.  The relative humidity QC flag is arbitrarily set to the
   !  temperature flag value.


! 1.  DISCARD NEGATIVE OR NULL TEMPERATURE AND DEW POINT
! ======================================================

   IF ((eps_equal (new%temperature%data, 0., 1.)) .OR. &
                  (new%temperature%data .LT. 0.)) THEN

                   new%temperature%data = missing_r
                   new%temperature%qc   = zero_t_td

   ENDIF

   !  Discard negative or null dew point

   IF ((eps_equal (new%dew_point%data, 0., 1.)) .OR. &
                  (new%dew_point%data .LT. 0.)) THEN

                  new%dew_point%data = missing_r
                  new%dew_point%qc   = zero_t_td

   ENDIF

! 2.  COMPUTE DIAGNOSTICS
! ========================

   !  Compute RH if missing when both temperature and dew point are not missing

   IF ((.NOT. eps_equal (new%temperature%data, missing_r , 1.)) .AND. &
       (.NOT. eps_equal (new%dew_point%data,   missing_r , 1.)) .AND. &
       (      eps_equal (new%rh%data,          missing_r , 1.))) THEN

         t  = new%temperature%data
         td = new%dew_point%data

         new%rh%data = 100. * exp ( L_over_Rv * (1./T - 1./Td) )
         new%rh%qc   = new%temperature%qc

         !  Compute Qv if missing when pressure is not missing

!         IF ((.NOT. eps_equal (new%pressure%data, missing_r , 1.)) .AND. &
         IF ((new%pressure%qc >= 0) .AND. &
             (      eps_equal (new%qv%data,       missing_r , 1.))) THEN

               p  = new%pressure%data / 100  ! P in mb

               es = 6.112 * EXP (17.67*(t-273.15) &
                              /(t-273.15+243.5))
               qs = 0.622 * es /(p-es)

               new%qv%data = MAX (0.01*new%rh%data*qs,1.E-06);
               new%qv%qc   = new%temperature%qc

         ELSE

               new%qv%data = missing_r
               new%qv%qc   = zero_t_td

         ENDIF

   ELSE
        IF (eps_equal (new%rh%data,          missing_r , 1.)) THEN
          new%rh%data = missing_r
          new%rh%qc   = zero_t_td
        ENDIF

        new%qv%data = missing_r
        new%qv%qc   = zero_t_td

   END IF

! 3.  CHECK ON PERMISSIBLE VALUES
! ==============================

   ! Rh must be between and 100

   IF (.NOT. eps_equal (new%rh%data, missing_r , 1.)) THEN
       new%rh%data = MAX (new%rh%data, 1.E-06 )
       new%rh%data = MIN (new%rh%data, 100.)
   ENDIF

   ! Qv must be positive

   IF (.NOT. eps_equal (new%qv%data, missing_r , 1.)) THEN
       new%qv%data = MAX (new%qv%data,  1.E-06)
   ENDIF

   ! Td must be lower than T

   IF (.NOT. eps_equal (new%dew_point%data, missing_r , 1.)) THEN
       new%dew_point%data = MIN (new%dew_point%data, new%temperature%data)
   ENDIF
    

END SUBROUTINE diagnostics_moist

SUBROUTINE diagnostics_wind (new, longitude)
!------------------------------------------------------------------------------

!  This routine performs some check on wind speed and direction,
!  and compute the wind components on the MM5 grid.

   USE module_type
   USE module_func
   USE module_map

   IMPLICIT NONE

   TYPE ( meas_data )    :: new
   REAL                  :: longitude
   LOGICAL               :: bad_wind

   INCLUDE 'constants.inc'
   INCLUDE 'missing.inc'

!------------------------------------------------------------------------------
   bad_wind = .false.

   !  The wind components are computed in two steps.  First we need the
   !  meteorological u and v from the speed and direction.  Second, those
   !  values of u and v are rotated to the model grid.

   IF      ((eps_equal (new%speed%data,     missing_r , 1.)) .OR. &
            (eps_equal (new%direction%data, missing_r , 1.))) THEN

             new%u%data = missing_r
             new%u%qc   = missing
             new%v%data = missing_r
             new%v%qc   = missing
             bad_wind = .true.
   ENDIF

   IF (eps_equal (new%speed%data, 0., 0.1)) THEN

            new%speed%qc = zero_spd
            new%u%qc     = zero_spd
            new%v%qc     = zero_spd
            new%u%data   = missing_r
            new%v%data   = missing_r
            bad_wind = .true.
   ENDIF

   IF (new%speed%data .LT. 0.) THEN

            new%speed%qc = negative_spd
            new%u%qc     = negative_spd
            new%v%qc     = negative_spd
            new%u%data   = missing_r
            new%v%data   = missing_r
            bad_wind = .true.
   ENDIF
 
   IF ((new%direction%data .LE.   1.e-3)  .OR. &
            (new%direction%data .GT. 360.0)) THEN
!
! Y.-R. Guo 04/22/2004: Rizvi said WMO don't allow to report
!            a zero of wind direction, here modified to be a value of 1.e-3.

             new%direction%qc = wrong_direction
             new%u%qc         = wrong_direction
             new%v%qc         = wrong_direction
             new%u%data       = missing_r
             new%v%data       = missing_r
             bad_wind = .true.
   ENDIF

! Y.-R. Guo 04/27/2004: to avoid assimilation of a bad wind 
!            with a meaningless value, set them to be missing.
   if (bad_wind) then
         new%direction%data = missing_r
         new%direction%qc   = missing
         new%speed    %data = missing_r
         new%speed    %qc   = missing
         return
   endif

             CALL FFDDUV (new%speed%data, new%direction%data,&
                          new%u%data, new%v%data, longitude, 1)

END SUBROUTINE diagnostics_wind

SUBROUTINE FFDDUV (F,D,U,V,YLON,ID)
!------------------------------------------------------------------------------!
! When ID =  1
! Convert wind speed (F in m/s) and direction (D in degree 0-360) into MM5
! wind (U-V in m/s) components
!
! When ID = -1
! Convert MM5 wind (U-V in m/s) components into wind speed (F in m/s) and 
! direction (D in degree 0-360)
!
! Need MM5 map projection parameters from module modulke_map.F
!
! IPROJ: Projection type
! PHIC:  Central latitude 
! XLONC: Central longitude
! XN:    Cone projection
! CONV:  180/Pi
!
!------------------------------------------------------------------------------!

    USE MODULE_MAP

    IMPLICIT NONE

    REAL,    INTENT (inout) :: F,D
    REAL,    INTENT (inout) :: U, V
    REAL,    INTENT (in)    :: YLON
    INTEGER, INTENT (in)    :: ID

    REAL :: AEARTH, UEARTH, VEARTH
    REAL :: XLONRT, ANG

    INCLUDE 'constants.inc'

!------------------------------------------------------------------------------!

    SELECT CASE (ID)

      CASE (1);

!     CONVERT WIND MODULE/DIRECTION INTO U/V WIND COMPONENTS ON EARTH,
!     THEN CONVERT U/V WIND COMPONENTS ON EARTH INTO LAMBERT CONFORMAL OR
!     POLAR STEREOGRAPHIC PROJECTION U/V WIND COMPONENTS.
!
!     PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE CONSTANT, XN
!     EQUATIONS REMAIN THE SAME.

      AEARTH = D/CONV

      UEARTH = -F*SIN(AEARTH)
      VEARTH = -F*COS(AEARTH)

!     FOR CONVERSION TO GRID COORDINATES,
!     SEE PROGRAM DATAMAP, SUBR VECT, AND
!     ANTHES' METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.

      XLONRT = XLONC-YLON

      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.

      ANG=XLONRT*XN/CONV

!   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES

      IF(IPROJ.EQ.3) ANG=0.

      IF(PHIC.LT.0.0) ANG=-ANG

      U = VEARTH*SIN(ANG) + UEARTH*COS(ANG)
      V = VEARTH*COS(ANG) - UEARTH*SIN(ANG)


!     CONVERT LAMBERT CONFORMAL OR POLAR STEREOGRAPHIC PROJECTION U/V
!     WIND COMPONENTS INTO U/V WIND COMPONENTS ON EART
!     THEN CONVERT U/V WIND COMPONENTS ON EARTH INTO WIND MODULE/DIRECTION
!
!     PROJECTIONS CHANGE REQUIRES ONLY A CHANGE OF THE CONE CONSTANT, XN

      CASE (-1);

      XLONRT = XLONC-YLON

      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.

      ANG=XLONRT*XN/CONV
!
!   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES
!
      IF(IPROJ .EQ.  3) ANG = 0.
      IF(PHIC  .LT. 0.) ANG = -ANG

      UEARTH = U*COS(ANG) - V*SIN(ANG)
      VEARTH = U*SIN(ANG) + V*COS(ANG)

      F = SQRT(UEARTH*UEARTH + VEARTH*VEARTH)

      IF (F .EQ. 0.0) THEN
         D = 0.
         RETURN
      ENDIF

      IF (VEARTH .EQ. 0.) THEN

         IF (UEARTH .GT. 0.) D = 270.
         IF (UEARTH .LT. 0.) D =  90.

      ELSE

         AEARTH = ATAN (UEARTH/VEARTH)*CONV

         IF (UEARTH .LE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH
         IF (UEARTH .LE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .GE. 0.0 ) D = AEARTH + 180.0
         IF (UEARTH .GE. 0.0 .AND. VEARTH .LE. 0.0 ) D = AEARTH + 360.0

      ENDIF

      CASE DEFAULT

           WRITE (0,'(/,A,I2,/)') ' UNKNOWN OPTION ',ID
           STOP ' in ffdduv.F'

    END SELECT

END SUBROUTINE FFDDUV
!------------------------------------------------------------------------------!
END MODULE module_diagnostics
