#if (NMM_CORE == 1)
MODULE module_diag_cl
CONTAINS
   SUBROUTINE diag_cl_stub
   END SUBROUTINE diag_cl_stub
END MODULE module_diag_cl
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_cl
CONTAINS

   SUBROUTINE clwrf_output_calc(                                      &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,is_restart                                      & ! CLWRF
                     ,clwrfH,t2,q2,u10,v10, skintemp                  & ! CLWRF
                     ,t2clmin,t2clmax,tt2clmin,tt2clmax               & ! CLWRF
                     ,t2clmean,t2clstd                                & ! CLWRF
                     ,q2clmin,q2clmax,tq2clmin,tq2clmax               & ! CLWRF
                     ,q2clmean,q2clstd                                & ! CLWRF
                     ,u10clmax,v10clmax,spduv10clmax,tspduv10clmax    & ! CLWRF
                     ,u10clmean,v10clmean,spduv10clmean               & ! CLWRF
                     ,u10clstd,v10clstd,spduv10clstd                  & ! CLWRF
                     ,raincclmax,rainncclmax,traincclmax,trainncclmax & ! CLWRF
                     ,raincclmean,rainncclmean,raincclstd,rainncclstd & ! CLWRF
                     ,skintempclmin,skintempclmax                     & ! CLWRF
                     ,tskintempclmin,tskintempclmax                   & ! CLWRF
                     ,skintempclmean,skintempclstd                    & ! CLWRF
                     ,raincv,rainncv                                  &
                     ,dt,xtime,curr_secs2                             &
                     ,nsteps                                          &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_configure 

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DT            time step (second)
!-- XTIME         forecast time
!-- curr_secs2    current time in seconds since simulation restart
!
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
! CLWRF-UC May.09 definitions
!-----------
! is_restart: whether if simulation is a restart
! clwrfH: Interval (hour) of accumulation for computations 
! [var]cl[min/max]: [minimum/maximum] of variable [var] during interval
! t[var]cl[min/max]: Time (minutes) of [minimum/maximum] of variable 
!    [var] during interval 
! [var]clmean: mean of variable [var] during interval
! [var]clstd: standard dev. of variable [var] during interval
!    Variables are written on aux_hist_out7 (established
!    in Registry)
!
!======================================================================

   INTEGER,      INTENT(IN   )                     ::            &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN)       :: i_start,   &
                                      i_end,j_start,j_end

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           & 
                                      RAINNCV, RAINCV, SKINTEMP 

   REAL,  INTENT(IN   )                            :: DT, XTIME
   REAL,  INTENT(IN   )                            :: curr_secs2

! LOCAL  VAR

   INTEGER                                   :: i,j,k,its,ite,jts,jte,ij
   INTEGER                                   :: idp,jdp

   REAL                                      :: xtimep
   LOGICAL, EXTERNAL                         :: wrf_dm_on_monitor

!!-------------------
!! CLWRF-UC Nov.09

   CHARACTER (LEN=80)                        :: timestr

   REAL, DIMENSION( ims:ime , jms:jme ),                                          & 
                          INTENT(IN)         :: t2, q2, u10, v10 
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: t2clmin, t2clmax, tt2clmin,       &
                          tt2clmax, t2clmean, t2clstd,                            & 
                          q2clmin, q2clmax, tq2clmin, tq2clmax, q2clmean, q2clstd,&
                          u10clmax, v10clmax, spduv10clmax, tspduv10clmax,        &
                          u10clmean, v10clmean, spduv10clmean,                    &
                          u10clstd, v10clstd, spduv10clstd, skintempclmin,        &
                          skintempclmax, tskintempclmin, tskintempclmax,          &
                          skintempclmean, skintempclstd
   REAL, DIMENSION( ims:ime , jms:jme ),                                          &
                          INTENT(OUT)        :: raincclmax, rainncclmax,          &
                          traincclmax, trainncclmax, raincclmean, rainncclmean,   & 
                          raincclstd, rainncclstd 
   REAL, PARAMETER                           :: minimum0= 1000000.,               &
                          maximum0= -1000000. 
   REAL                                      :: value
   INTEGER, INTENT(IN)                       :: clwrfH
   CHARACTER (LEN=1024)                      :: message
   INTEGER, INTENT(INOUT)                    :: nsteps
   LOGICAL                                   :: is_restart
! local vars
   REAL                                      :: t273

!-----------------------------------------------------------------

   t273  = 273.

! Initialize [var] values
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
!  IF ( MOD(NINT(XTIME), clwrfH) == 0 ) THEN
! IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) .AND. (.NOT.is_restart)) THEN
  IF (( MOD(NINT(curr_secs2/dt),NINT(clwrfH*60./dt)) == 0) ) THEN
    DO ij = 1 , num_tiles
      IF  ( wrf_dm_on_monitor() ) THEN
          CALL wrf_debug(0, 'Re-initializing accumulation arrays')
      ENDIF
      nsteps = 1
      DO j = j_start(ij), j_end(ij)
        DO i = i_start(ij), i_end(ij)
          t2clmin(i,j)=t2(i,j)-t273
          t2clmax(i,j)=t2(i,j)-t273
          t2clmean(i,j)=t2(i,j)-t273
          t2clstd(i,j)=(t2(i,j)-t273)*(t2(i,j)-t273)
          q2clmin(i,j)=q2(i,j)
          q2clmax(i,j)=q2(i,j)
          q2clmean(i,j)=q2(i,j)
          q2clstd(i,j)=q2(i,j)*q2(i,j)
          spduv10clmax(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clmean(i,j)=u10(i,j)
          v10clmean(i,j)=v10(i,j)
          spduv10clmean(i,j)=sqrt(u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j))
          u10clstd(i,j)=u10(i,j)*u10(i,j)
          v10clstd(i,j)=v10(i,j)*v10(i,j)
          spduv10clstd(i,j)=u10(i,j)*u10(i,j)+v10(i,j)*v10(i,j)
          raincclmax(i,j)=raincv(i,j)/dt
          rainncclmax(i,j)=rainncv(i,j)/dt
          raincclmean(i,j)=raincv(i,j)/dt
          rainncclmean(i,j)=rainncv(i,j)/dt
          raincclstd(i,j)=(raincv(i,j)/dt)*(raincv(i,j)/dt)
          rainncclstd(i,j)=(rainncv(i,j)/dt)*(rainncv(i,j)/dt)
          skintempclmin(i,j)=skintemp(i,j)-t273
          skintempclmax(i,j)=skintemp(i,j)-t273
          skintempclmean(i,j)=skintemp(i,j)-t273
          skintempclstd(i,j)=(skintemp(i,j)-t273)*(skintemp(i,j)-t273)
        ENDDO
      ENDDO
    ENDDO
!    nsteps=clwrfH*60./dt
  ELSE
    xtimep = xtime + dt/60.   ! value at end of timestep for time info
    nsteps=nsteps+1
! Temperature
          CALL varstatistics(t2-t273,xtimep,ime-ims+1,jme-jms+1,t2clmin,t2clmax,   &
            tt2clmin,tt2clmax,t2clmean,t2clstd)
! Water vapor mixing ratio
          CALL varstatistics(q2,xtimep,ime-ims+1,jme-jms+1,q2clmin,q2clmax,   &
            tq2clmin,tq2clmax,q2clmean,q2clstd)
! Wind speed
          CALL varstatisticsWIND(u10,v10,xtimep,ime-ims+1,jme-jms+1,u10clmax, &
            v10clmax,spduv10clmax,tspduv10clmax,u10clmean,v10clmean,         &
            spduv10clmean,u10clstd,v10clstd,spduv10clstd)
! Precipitation flux
          CALL varstatisticsMAX(raincv/dt,xtimep,ime-ims+1,jme-jms+1,         &
            raincclmax,traincclmax,raincclmean,raincclstd) 
          CALL varstatisticsMAX(rainncv/dt,xtimep,ime-ims+1,jme-jms+1,        &
            rainncclmax,trainncclmax,rainncclmean,rainncclstd)
! Skin Temperature 
          CALL varstatistics(skintemp-t273,xtimep,ime-ims+1,jme-jms+1,skintempclmin,&
            skintempclmax,tskintempclmin,tskintempclmax,skintempclmean,       &
            skintempclstd)

!          IF (MOD(NINT(XTIME),clwrfH) == 0) THEN
!          IF (MOD(NINT(XTIME+dt/60.),clwrfH) == 0) THEN
           IF (MOD(NINT((curr_secs2+dt)/dt),NINT(clwrfH*60./dt)) == 0) THEN
             IF  ( wrf_dm_on_monitor() ) &
               PRINT *,'nsteps=',nsteps,' xtime:', xtime,' clwrfH:',clwrfH
               t2clmean=t2clmean/nsteps
               t2clstd=SQRT(MAX(t2clstd/nsteps-t2clmean**2.,0.))
               t2clmean=t2clmean+t273
               t2clmin=t2clmin+t273
               t2clmax=t2clmax+t273
               q2clmean=q2clmean/nsteps
               q2clstd=q2clstd/nsteps-q2clmean*q2clmean
               q2clstd=MAX(q2clstd,0.)
               q2clstd=SQRT(q2clstd)
               u10clmean=u10clmean/nsteps
               v10clmean=v10clmean/nsteps
               spduv10clmean=spduv10clmean/nsteps
               u10clstd=SQRT(MAX(u10clstd/nsteps-u10clmean**2., 0.))
               v10clstd=SQRT(MAX(v10clstd/nsteps-v10clmean**2., 0.))
               spduv10clstd=SQRT(MAX(spduv10clstd/nsteps-                        &
                 spduv10clmean**2, 0.))
               raincclmean=raincclmean/nsteps
               rainncclmean=rainncclmean/nsteps
               raincclstd=SQRT(MAX(raincclstd/nsteps-raincclmean**2., 0.))
               rainncclstd=SQRT(MAX(rainncclstd/nsteps-rainncclmean**2., 0.))
               skintempclmean=skintempclmean/nsteps
               skintempclstd=skintempclstd/nsteps-skintempclmean*skintempclmean
               skintempclstd=MAX(skintempclstd,0.)
               skintempclstd=SQRT(skintempclstd)
               skintempclmean=skintempclmean+t273
               skintempclmin=skintempclmin+t273
               skintempclmax=skintempclmax+t273
             IF  ( wrf_dm_on_monitor() ) THEN
               DO ij = 1 , num_tiles
               idp = i_start(ij)+(i_end(ij)-i_start(ij))/2
               jdp = j_start(ij)+(j_end(ij)-j_start(ij))/2
               WRITE(message, *)'CLWRFdiag - T2; tile: ',ij,          &
                 ' T2clmin:', t2clmin(idp,jdp),                       &
                 ' T2clmax:', t2clmax(idp,jdp),                       &
                 ' TT2clmin:', tt2clmin(idp,jdp),                     &
                 ' TT2clmax:', tt2clmax(idp,jdp),                     &
                 ' T2clmean:', t2clmean(idp,jdp),                     &
                 ' T2clstd:', t2clstd(idp,jdp)
               CALL wrf_debug(0, message)
               WRITE(message, *)'CLWRFdiag - Q2; tile: ',ij,          &
                 ' Q2clmin:', q2clmin(idp,jdp),                       &
                 ' Q2clmax:', q2clmax(idp,jdp),                       &
                 ' TQ2clmin:', tq2clmin(idp,jdp),                     &
                 ' TQ2clmax:', tq2clmax(idp,jdp),                     &
                 ' Q2clmean:', q2clmean(idp,jdp),                     &
                 ' Q2clstd:', q2clstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message, *)'CLWRFdiag - WINDSPEED; tile: ',ij,   &
                 ' U10clmax:', u10clmax(idp,jdp),                     &
                 ' V10clmax:', v10clmax(idp,jdp),                     &
                 ' SPDUV10clmax:', spduv10clmax(idp,jdp),             &
                 ' TSPDUV10clmax:', tspduv10clmax(idp,jdp),           &
                 ' U10clmean:', u10clmean(idp,jdp),                   &
                 ' V10clmean:', v10clmean(idp,jdp),                   &
                 ' SPDUV10clmean:', spduv10clmean(idp,jdp),           &
                 ' U10clstd:', u10clstd(idp,jdp),                     &
                 ' V10clstd:', v10clstd(idp,jdp),                     &
                 ' SPDUV10clstd:', spduv10clstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message, *)'CLWRFdiag - RAIN; tile: ',ij,        &
                 ' RAINCclmax:',raincclmax(idp,jdp),                  &
                 ' RAINNCclmax:',rainncclmax(idp,jdp),                &
                 ' TRAINCclmax:',traincclmax(idp,jdp),                &
                 ' TRAINNCclmax:',trainncclmax(idp,jdp),              &
                 ' RAINCclmean:',raincclmean(idp,jdp),                &
                 ' RAINNCclmean:',rainncclmean(idp,jdp),              &
                 ' RAINCclstd:',raincclstd(idp,jdp),                  &
                 ' RAINNCclstd:',rainncclstd(idp,jdp)
               CALL wrf_debug(75, message)
               WRITE(message,*)'CLWRFdiag - SKINTEMP; tile: ',ij,     &
                 ' SKINTEMPclmin:',skintempclmin(idp,jdp),            &
                 ' SKINTEMPclmax:',skintempclmax(idp,jdp),            &
                 ' TSKINTEMPclmin:',tskintempclmin(idp,jdp),          &
                 ' TSKINTEMPclmax:',tskintempclmax(idp,jdp),          &
                 ' SKINTEMPclmean:',skintempclmean(idp,jdp),          &
                 ' SKINTEMPclstd:',skintempclstd(idp,jdp)
               CALL wrf_debug(75, message)
               ENDDO
             ENDIF
           END IF
!        ENDDO
!      ENDDO
  ENDIF
!  !$OMP END PARALLEL DO

   END SUBROUTINE clwrf_output_calc

! UC.CLWRF Nov.09
SUBROUTINE varstatisticsWIND(varu, varv, tt, dx, dy, varumax, varvmax,       &
  varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd, varvstd,     & 
  varuvstd) 
! Subroutine to compute variable statistics for a wind somponents 

IMPLICIT NONE

INTEGER                                                        :: i, j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: varu, varv
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varumax,   &
  varvmax, varuvmax, tvaruvmax, varumean, varvmean, varuvmean, varustd,      & 
  varvstd, varuvstd
REAL                                                           :: varuv

DO i=1,dx
  DO j=1,dy
    varuv=sqrt(varu(i,j)*varu(i,j)+varv(i,j)*varv(i,j))
      IF (varuv > varuvmax(i,j)) THEN
        varumax(i,j)=varu(i,j)
        varvmax(i,j)=varv(i,j)
        varuvmax(i,j)=varuv
        tvaruvmax(i,j)=tt
      END IF
    varuvmean(i,j)=varuvmean(i,j)+varuv
    varuvstd(i,j)=varuvstd(i,j)+varuv**2
  END DO
END DO
varumean=varumean+varu
varvmean=varvmean+varv
varustd=varustd+varu*varu
varvstd=varvstd+varv*varv

END SUBROUTINE varstatisticsWIND

SUBROUTINE varstatisticsMAX(var, tt, dx, dy, varmax, tvarmax, varmean,       &
   varstd)
! Subroutine to compute variable statistics for a max only variable values

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmax,    &
  tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatisticsMAX 

SUBROUTINE varstatistics(var, tt, dx, dy, varmin, varmax, tvarmin, tvarmax,  & 
  varmean, varstd) 
! Subroutine to compute variable statistics

IMPLICIT NONE

INTEGER                                                        :: i,j
INTEGER, INTENT(IN)                                            :: dx, dy
REAL, DIMENSION(dx,dy), INTENT(IN)                             :: var
REAL, INTENT(IN)                                               :: tt
REAL, DIMENSION(dx,dy), INTENT(INOUT)                          :: varmin,    &
  varmax, tvarmin, tvarmax, varmean, varstd

DO i=1,dx
  DO j=1,dy
    IF (var(i,j) < varmin(i,j)) THEN
      varmin(i,j)=var(i,j)
      tvarmin(i,j)=tt
    END IF
    IF (var(i,j) > varmax(i,j)) THEN
      varmax(i,j)=var(i,j)
      tvarmax(i,j)=tt
    END IF
  END DO
END DO
varmean=varmean+var
varstd=varstd+var*var

END SUBROUTINE varstatistics

END MODULE module_diag_cl
#endif
