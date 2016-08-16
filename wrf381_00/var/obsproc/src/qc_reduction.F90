SUBROUTINE qc_reduction (nobs_max, obs, number_of_obs)

!-------------------------------------------------------------------------------

  USE module_type
  USE module_func

  IMPLICIT NONE

  INTEGER, INTENT (in)                             :: nobs_max
  TYPE (report), INTENT (inout), DIMENSION (nobs_max) :: obs
  INTEGER, INTENT (in)                             :: number_of_obs

  TYPE (measurement ) , POINTER                    :: current
  INTEGER                                          :: loop_index, nlevel
  
   INTEGER             :: fm

  include 'missing.inc'
!------------------------------------------------------------------------------!

              WRITE (UNIT = 0, FMT = '(A)')  &
'------------------------------------------------------------------------------'
      WRITE (UNIT = 0, FMT = '(A,/)') 'REDUCE QC FROM 7 TO 2 DIGITS:'


! 1. LOOP OVER STATIONS
! ====================

stations: &
      DO loop_index = 1, number_of_obs


! 1.1 Check if record is valid
!     ------------------------

stations_valid: &
      IF (obs(loop_index)%info%discard ) THEN

      CYCLE  stations

      ELSE stations_valid

! 1.2 Ground info qc reduction
!     ------------------------

      CALL reduce_qc (obs (loop_index) % ground % slp % qc)
      CALL reduce_qc (obs (loop_index) % ground % pw  % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb19v % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb19h % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb22v % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb37v % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb37h % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb85v % qc)
      CALL reduce_qc (obs (loop_index) % ground % tb85h % qc)


! 1.3 Initialise upper level pointer to surface level
!     -----------------------------------------------

      current => obs (loop_index) % surface


! 2. LOOP ON UPPER-AIR LEVELS (FIRST LEVEL IS SURFACE)
! ====================================================

upper_level: DO WHILE (ASSOCIATED (current))

      CALL reduce_qc (current % meas % speed      % qc)
      CALL reduce_qc (current % meas % direction  % qc)
      CALL reduce_qc (current % meas % u  % qc)
      CALL reduce_qc (current % meas % v  % qc)
      CALL reduce_qc (current % meas % height % qc)
      CALL reduce_qc (current % meas % pressure % qc)
      CALL reduce_qc (current % meas % temperature % qc)
      CALL reduce_qc (current % meas % dew_point % qc)
      CALL reduce_qc (current % meas % rh % qc)
      CALL reduce_qc (current % meas % qv % qc)

! 3.  GO TOP NEXT LEVEL
! =====================

        current => current%next

      ENDDO upper_level


! 6.  GO TO NEXT STATION
! ======================


! 6.1 Go to next valid station
!     ------------------------

      ENDIF  stations_valid

! 6.1 Go to next station
!     ------------------

      ENDDO  stations

! 7.  END
! =======
      RETURN

      END SUBROUTINE qc_reduction
! ----------------------------------------------------------------
SUBROUTINE reduce_qc (QC)

!------------------------------------------------------------------------------!
      INTEGER, INTENT (inout) :: qc

      include 'missing.inc'

!------------------------------------------------------------------------------!

     IF        (qc .LT. 0) THEN

                qc = -88               ! Missing data

      ELSE IF  (qc .EQ. 0) THEN

                qc =   0               ! Good data

      ELSE IF  (qc .GE. outside_of_domain) THEN

                qc = -77               ! Outside of horizontal domain

      ELSE IF ((qc .LT. outside_of_domain) .AND. (qc .GE. wrong_direction))&
      THEN

                qc = -15               ! Wind direction <0 or > 360 degrees

      ELSE IF ((qc .LT. wrong_direction) .AND. (qc .GE. negative_spd))&
      THEN

                qc = -14               ! Negative wind speed vector norm

      ELSE IF ((qc .LT. negative_spd) .AND. (qc .GE. zero_spd))&
      THEN

                qc = -13               ! Null wind speed vector norm

      ELSE IF ((qc .LT. zero_spd) .AND. (qc .GE. wrong_wind_data))&
      THEN

                qc = -12               ! Spike in the wind profile

      ELSE IF ((qc .LT. wrong_wind_data) .AND. (qc .GE. zero_t_td))&
      THEN

                qc = -11               ! Null temperature or dew point

      ELSE IF ((qc .LT. zero_t_td) .AND. (qc .GE. t_fail_supa_inver))&
      THEN

                qc = -10               ! Superadiabatic temperature

      ELSE IF ((qc .LT. t_fail_supa_inver) .AND. (qc .GE. wrong_t_sign))&
      THEN

                qc =  -9               ! Spike in Temperature profile

      ELSE IF ((qc .LT. t_fail_supa_inver) .AND. (qc .GE. above_model_lid))&
      THEN

                qc =  -8               ! Height higher than model lid's height

      ELSE IF ((qc .LT. above_model_lid).AND.(qc .GE. reference_atmosphere))&
      THEN
                qc =  -5               ! h,p or T from  standard atmosphere

      ELSE IF ((qc .LT. reference_atmosphere) .AND. (qc .GE. from_background)) &
      THEN

                qc =  -4               ! h,p or T from background

      ELSE IF ((qc .LT. from_background) .AND. (qc .GE. convective_adjustment))&
      THEN

                qc =   1               ! convective adjustement correction

      ELSE IF ((qc .LT. convective_adjustment).AND.(qc .GE. Hydrostatic_recover)) THEN

                qc =   3               ! Height recovery from hydrostaic + OBS

      ELSE IF ((qc .LT. Hydrostatic_recover).AND.(qc .GE. surface_correction)) THEN
             
                qc =   2               ! surface_correction

      ELSE IF ((qc .LT. surface_correction).AND.(qc .GE. Reference_OBS_scaled)) THEN
 
                qc =   4               ! Height recovery from reference + OBS
     
      ELSE

                qc =  88               ! Any other check

      ENDIF

END SUBROUTINE reduce_qc
