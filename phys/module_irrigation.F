MODULE module_irrigation
CONTAINS


SUBROUTINE irr_calc_phase(irr_ph,phase,irr_rand_field_val,i,j,IRRIGATION,irr_freq)
  IMPLICIT NONE
  INTEGER :: irr_ph,irr_freq
  REAL, INTENT(OUT) :: phase
  REAL, OPTIONAL::   IRRIGATION
  INTEGER, OPTIONAL     :: i,j

  INTEGER, INTENT(INOUT) :: irr_rand_field_val
   IF(irr_ph .EQ. 1)phase=modulo(int(i*j*IRRIGATION),irr_freq)
   IF(irr_ph .EQ. 0)phase=0 
   IF(irr_ph .EQ. 2)phase=irr_rand_field_val
 RETURN
END SUBROUTINE irr_calc_phase


  SUBROUTINE drip_irrigation( julian_in                               & 
     &          ,irrigation,sf_surf_irr_scheme, irr_daily_amount      &    
     &          ,irr_start_hour,irr_num_hours,irr_start_julianday     & 
     &          ,irr_end_julianday,irr_freq,irr_ph                    & 
     &          ,i,j,RAINBL,IRRIGATION_CHANNEL,gmt,xtime,dt,irr_rand_field_val )
   IMPLICIT NONE
 !declarations
   REAL:: dt,xtime,gmt
   INTEGER :: j,i,tloc, jmonth,timing,end_hour,xt24,irr_day, sf_surf_irr_scheme,irr_start_hour,irr_num_hours,irr_start_julianday,irr_end_julianday,irr_freq,irr_ph
   REAL :: constants_irrigation,phase
   REAL, INTENT(INOUT) :: IRRIGATION_CHANNEL
   REAL, INTENT(INOUT) :: RAINBL
   REAL, INTENT(IN)::  IRRIGATION
   REAL::  irr_daily_amount
   REAL, INTENT(IN) :: julian_in
   INTEGER, INTENT(INOUT) :: irr_rand_field_val
  IRRIGATION_CHANNEL=0.
  IF(RAINBL.LE.0.01 .AND. IRRIGATION.GE.0.001) THEN
   end_hour=irr_start_hour+irr_num_hours    
   constants_irrigation=irr_freq*irr_daily_amount*0.000277778*0.01/irr_num_hours  ! hours in second:1/3600=0.000277778 
   phase=0.
   timing=modulo((int(julian_in)-irr_start_julianday),irr_freq)
   CALL irr_calc_phase(irr_ph,phase,irr_rand_field_val,i,j,IRRIGATION,irr_freq)
   IRRIGATION_CHANNEL=0.
   xt24=mod(xtime,1440.)
   tloc=floor(gmt+xt24/60.)
   if(tloc.lt.0) tloc=tloc+24
   IF ((tloc.GE.irr_start_hour .AND. tloc.LT.end_hour) .AND. (julian_in.GE.irr_start_julianday  .AND. julian_in.LT.irr_end_julianday) .AND. timing.EQ.0.  ) THEN
       IF(irr_ph.EQ.0) THEN
          RAINBL =RAINBL +dt*IRRIGATION*constants_irrigation
          IRRIGATION_CHANNEL=0.
       ELSE
          IF(timing.EQ.int(phase)) THEN
           RAINBL =RAINBL +dt*IRRIGATION*constants_irrigation
          ELSE
           IRRIGATION_CHANNEL=0.
          ENDIF
       ENDIF
   ENDIF
 ENDIF
RETURN
END SUBROUTINE drip_irrigation



SUBROUTINE channel_irrigation(  julian_in                             & !ARI
     &          ,irrigation,sf_surf_irr_scheme, irr_daily_amount      & !ARI   
     &          ,irr_start_hour,irr_num_hours,irr_start_julianday     & !ARI
     &          ,irr_end_julianday,irr_freq,irr_ph                    &
     &          ,i,j,RAINBL,IRRIGATION_CHANNEL,gmt,xtime,dt,irr_rand_field_val )
   IMPLICIT NONE
 !declarations
   REAL:: dt,xtime,gmt
   INTEGER :: j,i,tloc, jmonth,timing,end_hour,xt24,irr_day, sf_surf_irr_scheme,irr_start_hour,irr_num_hours,irr_start_julianday,irr_end_julianday,irr_freq,irr_ph
   REAL :: constants_irrigation,phase
   REAL, INTENT(INOUT) :: IRRIGATION_CHANNEL
   REAL, INTENT(INOUT) :: RAINBL
   REAL, INTENT(IN)::  IRRIGATION
   REAL::  irr_daily_amount
   REAL, INTENT(IN) :: julian_in
   INTEGER, INTENT(INOUT) :: irr_rand_field_val
  IRRIGATION_CHANNEL=0.
  IF(RAINBL.LE.0.01 .AND. IRRIGATION.GE.0.001) THEN
   end_hour=irr_start_hour+irr_num_hours
   constants_irrigation=irr_freq*irr_daily_amount*0.000277778*0.01/irr_num_hours  ! hours in second:1/3600=0.000277778 
   phase=0.
   timing=modulo((int(julian_in)-irr_start_julianday),irr_freq)
   CALL irr_calc_phase(irr_ph,phase,irr_rand_field_val,i,j,IRRIGATION,irr_freq)
   IRRIGATION_CHANNEL=0.
   xt24=mod(xtime,1440.)
   tloc=floor(gmt+xt24/60.)
   if(tloc.lt.0) tloc=tloc+24
   IF ((tloc.GE.irr_start_hour .AND. tloc.LT.end_hour) .AND. (julian_in.GE.irr_start_julianday .AND. julian_in.LT.irr_end_julianday) .AND. timing.EQ.0.  ) THEN
     IF(irr_ph.EQ.0) THEN
             IRRIGATION_CHANNEL=dt*IRRIGATION*constants_irrigation
     ELSE
             IF(timing.EQ.int(phase)) THEN
                  IRRIGATION_CHANNEL=dt*IRRIGATION*constants_irrigation
             ELSE
                  IRRIGATION_CHANNEL=0.
             ENDIF
     ENDIF
   ELSE
     IRRIGATION_CHANNEL=0.
   ENDIF
 ENDIF
RETURN
END SUBROUTINE channel_irrigation

!
SUBROUTINE sprinkler_irrigation(  julian_in                                           &
     &          ,irrigation, irr_daily_amount,rho,dz8w                                &    
     &          ,irr_start_hour,irr_num_hours,irr_start_julianday,irr_end_julianday   &
     &          ,irr_freq,irr_ph,qr_curr                                              &
     &          ,gmt,xtime,dt,irr_rand_field_val                                      &
     &          ,ids,ide, jds,jde                                                     & ! domain dims
     &          ,ims,ime, jms,jme, kms,kme                                            & ! memory dims
     &          ,its,ite, jts,jte               ) ! tile   dims
                                                           

   IMPLICIT NONE
   INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde,  &
                                     ims,ime, jms,jme, kms,kme, &
                                     its,ite, jts,jte 
!
   REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN),OPTIONAL:: irrigation !ARI
   REAL,  INTENT(IN)::  irr_daily_amount, xtime, gmt, julian_in
   INTEGER, INTENT(IN ),OPTIONAL::  irr_start_hour, irr_num_hours,irr_start_julianday,irr_freq,irr_ph,irr_end_julianday
   INTEGER  :: end_hour,a,b,xt24,irr_day,timing
   REAL :: constants_irrigation,tloc,irr_start,phase
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT):: irr_rand_field_val
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(IN   ) ::                                       &
                                                           rho, &
                                                          dz8w
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),              &
         INTENT(INOUT) ::                                       &
                                                        qr_curr
   REAL, INTENT(IN   ) :: dt
   end_hour=irr_start_hour+irr_num_hours

   xt24=mod(xtime,1440.)
   tloc=floor(gmt+xt24/60.)
   if(tloc.lt.0) tloc=tloc+24
!  
   timing=modulo((int(julian_in)),irr_freq)
   DO a=its, ite
     DO b=jts,jte

      constants_irrigation=irr_freq*irr_daily_amount/(irr_num_hours*3600*rho(a,kms,b)*dz8w(a,kms,b)*100)
      IF ( (tloc.GE.irr_start_hour .AND. tloc.LT.end_hour) .AND. &
          (irrigation(a,b).GE.0.1) .AND. &
          (julian_in.GE.irr_start_julianday .AND. julian_in.LT.irr_end_julianday ) ) THEN
        CALL irr_calc_phase(irr_ph,phase,irr_rand_field_val(a,b),a,b,irrigation(a,b),irr_freq)
        IF(irr_ph.EQ.0) THEN
              qr_curr(a,kms,b)=qr_curr(a,kms,b)+irrigation(a,b)*constants_irrigation*dt
        ELSE
              IF(timing.EQ.int(phase))  THEN
                   qr_curr(a,kms,b)=qr_curr(a,kms,b)+irrigation(a,b)*constants_irrigation*dt
              END IF
        END IF
      END IF
    END DO
   END DO
!
RETURN

END SUBROUTINE sprinkler_irrigation
END MODULE module_irrigation
