!wrf:model_layer:physics
!
!ckay=KIRAN ALAPATY @ US EPA -- November 01, 2015
!
! Tim Glotfelty@CNSU; AJ Deng@PSU
!modified for use with FASDAS 
!Flux Adjusting Surface Data Assimilation System to assimilate 
!surface layer and soil layers temperature and moisture using
! surfance reanalsys 
!Reference: Alapaty et al., 2008: Development of the flux-adjusting surface
! data assimilation system for mesoscale models. JAMC, 47, 2331-2350

!
!
MODULE module_fdda_psufddagd

CONTAINS
!
!-------------------------------------------------------------------
!
   SUBROUTINE fddagd(itimestep,dx,dt,xtime,  &
               id,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min,  &

               grid_sfdda, &
               analysis_interval_sfc, end_fdda_hour_sfc, guv_sfc, gt_sfc, gq_sfc, &
               rinblw, &

               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,                &
               u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,mu_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,mu_ndg_new,       &
     u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
     rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old, &
     u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
     rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new, &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
               fasdas, SDA_HFX, SDA_QFX,                            & ! fasdas
               HFX_FDDA,dz8w,                                       & ! fasdas
               pblh, ht, regime, znt, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte                        )

!-------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------
!
!   This code was implemented by Aijun Deng (Penn State).  The 3-D analysis nudging was comleted 
!   and released in December 2006.  The surface analysis nudging capability was added and 
!   released in March 2009 with WRFV3.1.
!
!-- u3d         3d u-velocity staggered on u points
!-- v3d         3d v-velocity staggered on v points
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- pi3d        3d exner function (dimensionless)
!-- rundgdten   staggered u tendency due to
!               fdda grid nudging (m/s/s)
!-- rvndgdten   staggered v tendency due to
!               fdda grid nudging (m/s/s)
!-- rthndgdten  theta tendency due to
!               fdda grid nudging (K/s)
!-- rqvndgdten  qv tendency due to
!               fdda grid nudging (kg/kg/s)
!-- rmundgdten  mu tendency due to
!               fdda grid nudging (Pa/s)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------
!
   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      analysis_interval_sfc, end_fdda_hour_sfc
   INTEGER,  INTENT(IN)   ::      grid_sfdda

   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      DT, dx, xtime, dtramp_min

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                   qv3d, &
                                               p3d, &
                                              pi3d, &
                                              th3d, &
                                               t3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rqvndgdten
!
! FASDAS
!
   INTEGER,  INTENT(IN)   ::      fasdas
   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT)   ::           SDA_HFX, &
                                          SDA_QFX
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           HFX_FDDA
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                   dz8w
!
! END FASDAS
!

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT)   ::          rmundgdten

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)      ::           u_ndg_old, &
                                          v_ndg_old, &
                                          t_ndg_old, &
                                          q_ndg_old, &
                                          u_ndg_new, &
                                          v_ndg_new, &
                                          t_ndg_new, &
                                          q_ndg_new
                                                           
   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT) ::   mu_ndg_old, &
                                mu_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &   
               INTENT(IN)       ::                       odis_ndg_old, odis_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                       ht, &
                                                       regime, &
                                                       znt

   REAL, INTENT(IN)    :: guv, gt, gq
   REAL, INTENT(IN)    :: guv_sfc, gt_sfc, gq_sfc, rinblw

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, k0, j0
   REAL                :: xtime_old, xtime_new, coef, val_analysis
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min
   !BPR BEGIN
   REAL                :: tfac_sfc, actual_end_fdda_min_sfc
   !BPR END
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ) :: wpbl  ! 1: u, 2: v, 3: t, 4: q
   REAL, DIMENSION( kts:kte, 4 )                   :: wzfac ! 1: u, 2: v, 3: t, 4: q

!  TWG 2015 Pseudo Radiative Flux
   REAL, DIMENSION( its:ite, kts:kte, jts:jte) :: rho_air
!  END TWG 2015

   LOGICAL , EXTERNAL  :: wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message
   INTEGER :: int4


   int4 = 1  ! 1: temporal interpolation. else: target nudging toward *_ndg_new values

   actual_end_fdda_min = end_fdda_hour*60.0
   !BPR BEGIN
   actual_end_fdda_min_sfc = end_fdda_hour*60.0
   !BPR END
   IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
       actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       !BPR BEGIN
       actual_end_fdda_min_sfc = end_fdda_hour_sfc*60.0 + ABS(dtramp_min)
       !BPR END
   !BPR BEGIN
   !IF( xtime > actual_end_fdda_min ) THEN
   IF( ( xtime > actual_end_fdda_min ) .AND. ( xtime > actual_end_fdda_min_sfc ) ) THEN
   !BPR END
!  If xtime is greater than the end time, no need to calculate tendencies. Just set the tendencies 
!  to zero to turn off nudging and return.
     DO j = jts, jte
     DO k = kts, kte
     DO i = its, ite
       RUNDGDTEN(i,k,j) = 0.0
       RVNDGDTEN(i,k,j) = 0.0
       RTHNDGDTEN(i,k,j) = 0.0
       RQVNDGDTEN(i,k,j) = 0.0
       IF( k .EQ. kts ) RMUNDGDTEN(i,j) = 0.
     ENDDO
     ENDDO
     ENDDO
     RETURN
   ENDIF

   IF( analysis_interval <= 0 )CALL wrf_error_fatal('In grid FDDA, gfdda_interval_m must be > 0')
   xtime_old = FLOOR(xtime/analysis_interval) * analysis_interval * 1.0
   xtime_new = xtime_old + analysis_interval
   IF( int4 == 1 ) THEN
     coef = (xtime-xtime_old)/(xtime_new-xtime_old)
   ELSE
     coef = 1.0          ! Nudging toward a target value (*_ndg_new values)
   ENDIF

   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour*60.0 ) THEN
         WRITE(message,'(a,i1,a,f10.3,a)') &
          'D0',id,' 3-D analysis nudging reads new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(message) )
         WRITE(message,'(a,i1,a,2f8.2,a)') &
          'D0',id,' 3-D analysis nudging bracketing times = ', xtime_old, xtime_new, ' min.'
         CALL wrf_message( TRIM(message) )
       ENDIF

       actual_end_fdda_min = end_fdda_hour*60.0
       IF( if_ramping == 1 .AND. dtramp_min > 0.0 ) &
           actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)

       IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min ) THEN
!        Find the mid point of the tile and print out the sample values
         i0 = (ite-its)/2+its
         j0 = (jte-jts)/2+jts 

         IF( guv > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' u_ndg_old=', u_ndg_old(i0,k,j0), ' u_ndg_new=', u_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
           WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
             ' mu_ndg_old=', mu_ndg_old(i0,j0), ' mu_ndg_new=', mu_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' v_ndg_old=', v_ndg_old(i0,k,j0), ' v_ndg_new=', v_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gt > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' t_ndg_old=', t_ndg_old(i0,k,j0), ' t_ndg_new=', t_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gq > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample 3-D analysis values at i,k,j=', i0, k, j0, &
               ' q_ndg_old=', q_ndg_old(i0,k,j0), ' q_ndg_new=', q_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

        IF( int4 == 1 ) then
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' 3-D nudging towards the temporally interpolated analysis'
         ELSE
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' 3-D nudging towards the target analysis'
         ENDIF

       ENDIF
     ENDIF
   ENDIF

   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)
!
! If the user-defined namelist switches (if_no_pbl_nudging_uv, if_no_pbl_nudging_t, 
! if_no_pbl_nudging_q switches) are set to 1, compute the weighting function, wpbl(:,k,:,:), 
! based on the PBL depth.  wpbl = 1 above the PBL and wpbl = 0 in the PBL.  If all 
! the switches are set to zero, wpbl = 1 (default value).
!
   wpbl(:,:,:,:) = 1.0

   IF( if_no_pbl_nudging_uv == 1 .OR. grid_sfdda >= 1 ) THEN

     DO j=jts,jtf 
     DO i=itsu,itf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i-1,j) + pblh(i,j) )

       loop_ku: DO k=kts,ktf 
!        zagl     = 0.5 * ( z(i-1,k,j)-ht(i-1,j) + z(i,k,j)-ht(i,j) )
         zagl_bot = 0.5 * ( z_at_w(i-1,k,  j)-ht(i-1,j) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i-1,k+1,j)-ht(i-1,j) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_ku
         ENDIF
       ENDDO loop_ku

       DO k=kts,ktf 
         IF( k <= kpbl   ) wpbl(i, k, j, 1) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 1) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 1) = 1.0
       ENDDO

     ENDDO
     ENDDO

     DO i=its,itf
     DO j=jtsv,jtf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i,j-1) + pblh(i,j) )

       loop_kv: DO k=kts,ktf
!        zagl     = 0.5 * ( z(i,k,j-1)-ht(i,j-1) + z(i,k,j)-ht(i,j) )
         zagl_bot = 0.5 * ( z_at_w(i,k,  j-1)-ht(i,j-1) + z_at_w(i,k,  j)-ht(i,j) )
         zagl_top = 0.5 * ( z_at_w(i,k+1,j-1)-ht(i,j-1) + z_at_w(i,k+1,j)-ht(i,j) )
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kv
         ENDIF
       ENDDO loop_kv

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 2) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 2) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 2) = 1.0
       ENDDO

     ENDDO
     ENDDO

   ENDIF

   IF( if_no_pbl_nudging_t == 1 .OR. grid_sfdda >= 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
        
       loop_kt: DO k=kts,ktf
!        zagl     = z(i,k,j)-ht(i,j)
         zagl_bot = z_at_w(i,k,  j)-ht(i,j)
         zagl_top = z_at_w(i,k+1,j)-ht(i,j)
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kt
         ENDIF
       ENDDO loop_kt

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 3) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 3) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 3) = 1.0
       ENDDO 
        
     ENDDO
     ENDDO

   ENDIF

   IF( if_no_pbl_nudging_q == 1 .OR. grid_sfdda >= 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
          
       loop_kq: DO k=kts,ktf
!        zagl     = z(i,k,j)-ht(i,j)
         zagl_bot = z_at_w(i,k,  j)-ht(i,j)
         zagl_top = z_at_w(i,k+1,j)-ht(i,j)
         IF( zpbl >= zagl_bot .AND. zpbl < zagl_top ) THEN
           kpbl = k
           EXIT loop_kq
         ENDIF
       ENDDO loop_kq

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 4) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 4) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 4) = 1.0
       ENDDO 
            
     ENDDO  
     ENDDO
        
   ENDIF
!
! If the user-defined namelist switches (if_zfac_uv, if_zfac_t,
! if_zfac_q swithes) are set to 1, compute the weighting function, wzfac(k,:),
! based on the namelist specified k values (k_zfac_uv, k_zfac_t and k_zfac_q) below which analysis 
! nudging is turned off (wzfac = 1 above k_zfac_x and = 0 in below k_zfac_x).  If all
! the switche are set to zero, wzfac = 1 (default value).
!
   wzfac(:,:) = 1.0

   IF( if_zfac_uv == 1 ) THEN

     DO j=jts,jtf
     DO i=itsu,itf
     DO k=kts,ktf
       IF( k <= k_zfac_uv   ) wzfac(k, 1:2) = 0.0
       IF( k == k_zfac_uv+1 ) wzfac(k, 1:2) = 0.1
       IF( k >  k_zfac_uv+1 ) wzfac(k, 1:2) = 1.0
     ENDDO
     ENDDO
     ENDDO

   ENDIF

   IF( if_zfac_t == 1 ) THEN

     DO j=jts,jtf
     DO i=itsu,itf
     DO k=kts,ktf
       IF( k <= k_zfac_t   ) wzfac(k, 3) = 0.0
       IF( k == k_zfac_t+1 ) wzfac(k, 3) = 0.1
       IF( k >  k_zfac_t+1 ) wzfac(k, 3) = 1.0
     ENDDO
     ENDDO
     ENDDO

   ENDIF

   IF( if_zfac_q == 1 ) THEN
       
     DO j=jts,jtf
     DO i=itsu,itf 
     DO k=kts,ktf
       IF( k <= k_zfac_q   ) wzfac(k, 4) = 0.0
       IF( k == k_zfac_q+1 ) wzfac(k, 4) = 0.1 
       IF( k >  k_zfac_q+1 ) wzfac(k, 4) = 1.0
     ENDDO  
     ENDDO
     ENDDO

   ENDIF
!
! If if_ramping and dtramp_min are defined by user, compute a time weighting function, tfac, 
! for analysis nudging so that at the end of the nudging period (which has to be at a 
! analysis time) we ramp down the nudging coefficient, based on the use-defined sign of dtramp_min.
!
! When dtramp_min is negative, ramping ends at end_fdda_hour and starts at 
! end_fdda_hour-ABS(dtramp_min).  
!
! When dtramp_min is positive, ramping starts at end_fdda_hour and ends at 
! end_fdda_hour+ABS(dtramp_min). 
! BPR BEGIN
! In this case, during the rampdown we nudge towards the most recent past analysis and ignore
! the future analysis (implemented by setting coef = 0.0)
! THE FOLLOWING COMMENT IS NO LONGER APPLICABLE
! In this case, the obs values are extrapolated using 
! the obs tendency saved from the previous FDDA window.  More specifically for extrapolation, 
! coef (see codes below) is recalculated to reflect extrapolation during the ramping period.
! THE PRECEDING COMMENT IS NOT LONGER APPLICABLE
! BPR END
!
   tfac = 1.0
   !BPR BEGIN
   tfac_sfc = 1.0
   !BPR END

   IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
 
     IF( dtramp_min <= 0.0 ) THEN
       actual_end_fdda_min = end_fdda_hour*60.0
     ELSE
       actual_end_fdda_min = end_fdda_hour*60.0 + dtramp_min
     ENDIF

     IF( xtime < actual_end_fdda_min-ABS(dtramp_min) )THEN 
       tfac = 1.0
     ELSEIF( xtime >= actual_end_fdda_min-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min )THEN
       tfac = ( actual_end_fdda_min - xtime ) / ABS(dtramp_min)
       !BPR BEGIN
       !The original method assumed that to be here in the code with dtramp_min>0
       !meant that we were after the valid time of *_ndg_new and so used the
       !values *_ndg_old and *_ndg_new to extrapolate a current analysis value.
       !HOWEVER, in practice once we get to the valid time of *_ndg_new WRF will
       !read in the next pair of *_ndg_old and *_ndg_new values, even if we are
       !at the beginning of the rampdown period. Since the *_ndg_new values have
       !a valid time after the beginning of the rampdown period we do not want
       !to use them as we would be using analyses valid after the supposed end
       !time of the analysis nudging.
       !IF( dtramp_min > 0.0 ) coef = (xtime-xtime_old+analysis_interval)/(analysis_interval*1.0)
       !Use only the *_ndg_old values
       IF( dtramp_min > 0.0 ) coef = 0.0 
       !BPR END
     ELSE                                                     
       tfac = 0.0
     ENDIF

     !BPR BEGIN
     !Now calculate the same quantities for surface analysis nudging
     !Add actual_end_fdda_min_sfc, tfac_sfc
     IF( dtramp_min <= 0.0 ) THEN
       actual_end_fdda_min_sfc = end_fdda_hour_sfc*60.0
     ELSE
       actual_end_fdda_min_sfc = end_fdda_hour_sfc*60.0 + dtramp_min
     ENDIF

     IF( xtime < actual_end_fdda_min_sfc-ABS(dtramp_min) )THEN 
       tfac_sfc = 1.0
     ELSEIF( xtime >= actual_end_fdda_min_sfc-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min_sfc )THEN
       tfac_sfc = ( actual_end_fdda_min_sfc - xtime ) / ABS(dtramp_min)
     ELSE                                                     
       tfac_sfc = 0.0
     ENDIF
     !BPR END

   ENDIF                                                  

!
! Surface Analysis Nudging
!
   IF( grid_sfdda >= 1 ) THEN
     CALL SFDDAGD(itimestep,dx,dt,xtime, id, &
     analysis_interval_sfc, end_fdda_hour_sfc, guv_sfc, gt_sfc, gq_sfc, &
     rinblw, &
               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,        &
     u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
     rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old,  &
     u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
     rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new,  &
     RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
     pblh, ht, regime, znt, z, z_at_w,                             &
     ids,ide, jds,jde, kds,kde,                           &
     ims,ime, jms,jme, kms,kme,                           &
     its,ite, jts,jte, kts,kte, wpbl, wzfac, if_ramping, dtramp_min, &
!BPR BEGIN
!    actual_end_fdda_min, tfac &
     actual_end_fdda_min_sfc, tfac_sfc &
!BPR END
!
! FASDAS
!
        ,fasdas, SDA_HFX, SDA_QFX     &
!
! END FASDAS
!
                                    )
   ENDIF
!
! Compute 3-D nudging tendencies for u, v, t and q
!
   DO j=jts,jtf
   DO k=kts,ktf
   DO i=itsu,itf
     val_analysis = u_ndg_old(i,k,j) *( 1.0 - coef ) + u_ndg_new(i,k,j) * coef
     RUNDGDTEN(i,k,j) = RUNDGDTEN(i,k,j) + guv * wpbl(i,k,j,1) * wzfac(k,1) * tfac * &
                         ( val_analysis - u3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jtsv,jtf
   DO k=kts,ktf
   DO i=its,itf
     val_analysis = v_ndg_old(i,k,j) *( 1.0 - coef ) + v_ndg_new(i,k,j) * coef
     RVNDGDTEN(i,k,j) = RVNDGDTEN(i,k,j) + guv * wpbl(i,k,j,2) * wzfac(k,2) * tfac * &
                       ( val_analysis - v3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf


     val_analysis = t_ndg_old(i,k,j) *( 1.0 - coef ) + t_ndg_new(i,k,j) * coef
     RTHNDGDTEN(i,k,j) = RTHNDGDTEN(i,k,j) +   gt * wpbl(i,k,j,3) * wzfac(k,3) * tfac * &
                          ( val_analysis - th3d(i,k,j) + 300.0 )
!
!FASDAS
!
!TWG 2015 Pseudo Radiative Flux

     rho_air(i,k,j) = p3d(i,k,j)/(287.0*th3d(i,k,j))
     HFX_FDDA(i,k,j) = rho_air(i,k,j)*1004.0*RTHNDGDTEN(i,k,j)*dz8w(i,k,j)

!TWG 2015 END
!
!END FASDAS
!
     val_analysis = q_ndg_old(i,k,j) *( 1.0 - coef ) + q_ndg_new(i,k,j) * coef
     RQVNDGDTEN(i,k,j) = RQVNDGDTEN(i,k,j) + gq * wpbl(i,k,j,4) * wzfac(k,4) * tfac * &
                          ( val_analysis - qv3d(i,k,j) )

   ENDDO
   ENDDO
   ENDDO

   !BPR BEGIN 
   !Diagnostic print
   IF ( wrf_dm_on_monitor()) THEN
    IF( dbg_level .GE. 10 ) THEN
     i0 = (ite-its)/2+its
     j0 = (jte-jts)/2+jts 
     k0 = (kte-kts)/2+kts
     WRITE(message,'(a,i1,a,f7.2,a,i4,a,i4,a,i4,a,f7.2,a,f4.2,a,f7.2,a,f4.2,a,f4.2 )') &
      '    D0',id,' At xtime=',xtime,' FDDA sample pot. temp. analysis (i=',i0,', j=',j0,' k=', &
      k0,') = (t_ndg_old=', t_ndg_old(i0,k0,j0),') * ',1.0-coef,' + (t_ndg_new=', &
      t_ndg_new(i0,k0,j0),') * ',coef, ' where tfac=',tfac
     CALL wrf_message( TRIM(message) )
     WRITE(message,'(a,i1,a,f7.2,a,i4,a,f7.2,a,f7.2)') &
     '    D0',id,'  xtime_old=',xtime_old,' analysis_interval=',analysis_interval,' actual_end_fdda_min=', &
     actual_end_fdda_min,' dtramp_min=',dtramp_min
     CALL wrf_message( TRIM(message) )
    END IF
   END IF
   !BPR END 

   END SUBROUTINE fddagd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE sfddagd(itimestep,dx,dt,xtime,  &
               id, analysis_interval_sfc, end_fdda_hour_sfc, &
               guv_sfc, gt_sfc, gq_sfc, rinblw,  &
               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,                &
               u10_ndg_old, v10_ndg_old, t2_ndg_old, th2_ndg_old, q2_ndg_old, &
               rh_ndg_old, psl_ndg_old, ps_ndg_old, tob_ndg_old, odis_ndg_old,  &
               u10_ndg_new, v10_ndg_new, t2_ndg_new, th2_ndg_new, q2_ndg_new, &
               rh_ndg_new, psl_ndg_new, ps_ndg_new, tob_ndg_new, odis_ndg_new,  &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN, &
               pblh, ht, regime, znt, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte, wpbl, wzfac, if_ramping, dtramp_min, &
!BPR BEGIN
!              actual_end_fdda_min, tfac &
               actual_end_fdda_min_sfc, tfac_sfc &
!BPR END
!
! FASDAS
!
               ,fasdas, SDA_HFX, SDA_QFX   &
!
! END FASDAS
!
                                              )
!-------------------------------------------------------------------
   USE module_model_constants
 
   implicit none

!-------------------------------------------------------------------
!  
!   This code was implemented by Aijun Deng (Penn State).  The 3-D analysis nudging was comleted
!   and released in December 2006.  The surface analysis nudging capability was added and
!   released in March 2009 with WRFV3.1.
!
!-- u3d         3d u-velocity staggered on u points
!-- v3d         3d v-velocity staggered on v points
!-- th3d        3d potential temperature (k)
!-- t3d         temperature (k)
!-- qv3d        3d water vapor mixing ratio (kg/kg)
!-- p3d         3d pressure (pa)
!-- pi3d        3d exner function (dimensionless)
!-- rundgdten   staggered u tendency due to
!               fdda grid nudging (m/s/s)
!-- rvndgdten   staggered v tendency due to
!               fdda grid nudging (m/s/s)
!-- rthndgdten  theta tendency due to
!               fdda grid nudging (K/s)
!-- rqvndgdten  qv tendency due to
!               fdda grid nudging (kg/kg/s)
!-- rmundgdten  mu tendency due to
!               fdda grid nudging (Pa/s)
!-- ids         start index for i in domain
!-- ide         end index for i in domain
!-- jds         start index for j in domain
!-- jde         end index for j in domain
!-- kds         start index for k in domain
!-- kde         end index for k in domain
!-- ims         start index for i in memory
!-- ime         end index for i in memory
!-- jms         start index for j in memory
!-- jme         end index for j in memory
!-- kms         start index for k in memory
!-- kme         end index for k in memory
!-- its         start index for i in tile
!-- ite         end index for i in tile
!-- jts         start index for j in tile
!-- jte         end index for j in tile
!-- kts         start index for k in tile
!-- kte         end index for k in tile
!-------------------------------------------------------------------
!
   INTEGER,  INTENT(IN)   ::      itimestep, analysis_interval_sfc, end_fdda_hour_sfc

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      dx,DT, xtime

   INTEGER,  INTENT(IN)   ::      ids,ide, jds,jde, kds,kde, &
                                  ims,ime, jms,jme, kms,kme, &
                                  its,ite, jts,jte, kts,kte
 
   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                   qv3d, &
                                               p3d, &
                                              pi3d, &
                                              th3d, &
                                               t3d, &
                                                 z, &
                                            z_at_w

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           rundgdten, &
                                          rvndgdten, &
                                         rthndgdten, &
                                         rqvndgdten

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT)   ::          rmundgdten

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       u10_ndg_old,  &
                                                         v10_ndg_old,  &
                                                         t2_ndg_old,   &
                                                         th2_ndg_old,  &
                                                         q2_ndg_old,   &
                                                         rh_ndg_old,   &
                                                         psl_ndg_old,  &
                                                         ps_ndg_old,   &
                                                         u10_ndg_new,  &
                                                         v10_ndg_new,  &
                                                         t2_ndg_new,   &
                                                         th2_ndg_new,  &
                                                         q2_ndg_new,   &
                                                         rh_ndg_new,   &
                                                         psl_ndg_new,  &
                                                         ps_ndg_new

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)       ::                       tob_ndg_old,  &
                                                         tob_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,       DIMENSION( ims:ime, jms:jme ),            &
               INTENT(IN)         ::                    odis_ndg_old, odis_ndg_new

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                       ht,   &
                                                       regime, &
                                                       znt

   REAL, INTENT(IN)    :: guv_sfc, gt_sfc, gq_sfc, rinblw

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, j0
   REAL                :: xtime_old_sfc, xtime_new_sfc, coef, val_analysis, es
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac_sfc, actual_end_fdda_min_sfc

   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ), &
         INTENT(INout)                                   :: wpbl  ! 1: u, 2: v, 3: t, 4: q
   REAL, DIMENSION( kts:kte, 4 ),  &
         INTENT(IN)                                   :: wzfac ! 1: u, 2: v, 3: t, 4: q
   REAL, DIMENSION( its:ite, jts:jte)                 :: wndcor_u, wndcor_v
   REAL, DIMENSION( its-2:ite+2, jts-2:jte+2)         :: blw_old, blw_new
   REAL, DIMENSION( its:ite, kts:kte, jts:jte)        :: qsat

   REAL                                               :: m, b=1.8, blw, rindx, x

   REAL :: difz, wr14, wr1z, wr24, wr2z, wndfac, reg, znt0
   INTEGER,  INTENT(IN)   :: if_ramping
   REAL, INTENT(IN)       :: dtramp_min

   LOGICAL , EXTERNAL     :: wrf_dm_on_monitor

   CHARACTER (LEN=256)    :: message
   INTEGER :: iwinds, idd, iqsat, int4
!
! FASDAS
!
   INTEGER,  INTENT(IN)   ::      fasdas
   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(  OUT)   ::           SDA_HFX, &
                                          SDA_QFX
   REAL :: stabFac, exf, DiffTN
   REAL, DIMENSION( its:ite, kts:kte, jts:jte ) :: wpblfasdas
!
! END FASDAS
!
   iwinds = 1    ! 1: Scale the surface wind analysis to the lowest model level, 
                 ! if the first model half-layer is greater than 10 meters 
                 ! and we are in the free convection regime (REGIME=4.0). else: no
   idd = 1       ! 1: Obs data density correction is applied. else: no
   iqsat = 1     ! 1: Remove super saturation. eles: no
   int4 = 1      ! 1: temporal ionterpolation. else: target nudging toward *_ndg_new values

   IF( analysis_interval_sfc <= 0 )CALL wrf_error_fatal('In grid sfc FDDA, sgfdda_interval_m must be > 0')
   xtime_old_sfc = FLOOR(xtime/analysis_interval_sfc) * analysis_interval_sfc * 1.0
   xtime_new_sfc = xtime_old_sfc + analysis_interval_sfc
   IF( int4 == 1 ) THEN
     coef = (xtime-xtime_old_sfc)/(xtime_new_sfc-xtime_old_sfc)  ! Temporal interpolation
   ELSE
     coef = 1.0          ! Nudging toward a target value (*_ndg_new values)
   ENDIF

   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour_sfc*60.0 ) THEN
         WRITE(message,'(a,i1,a,f10.3,a)') &
          'D0',id,' surface analysis nudging reads new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(message) )
         WRITE(message,'(a,i1,a,2f8.2,a)') &
          'D0',id,' surface analysis nudging bracketing times = ', xtime_old_sfc, xtime_new_sfc, ' min.'
         CALL wrf_message( TRIM(message) )
       ENDIF

       IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min_sfc ) THEN
!        Find the mid point of the tile and print out the sample values
         i0 = (ite-its)/2+its
         j0 = (jte-jts)/2+jts 

         IF( guv_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' u10_ndg_old=', u10_ndg_old(i0,j0), ' u10_ndg_new=', u10_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' v10_ndg_old=', v10_ndg_old(i0,j0), ' v10_ndg_new=', v10_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( gt_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' th2_ndg_old=', th2_ndg_old(i0,j0), ' th2_ndg_new=', th2_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( gq_sfc > 0.0 ) THEN
           WRITE(message,'(a,i1,a,2i4,a,f10.4,a,f10.4)') &
             '    D0',id,' sample surface analysis values at i,j=', i0, j0, &
             ' q2_ndg_old=', q2_ndg_old(i0,j0), ' q2_ndg_new=', q2_ndg_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF

         IF( iwinds ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface wind analysis s scaled to the lowest model level, if dz1 > 10m and REGIME=4.'

         IF( idd ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' obs data density is used for additional weighting function'

         IF( iqsat ==  1 ) &
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' super saturation is not allowed for q analysis'

         IF( int4 ==  1 ) then
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface nudging towards the temporally interpolated analysis'
         ELSE
           WRITE(message,'(a,i1,a)') '    D0',id, &
              ' surface nudging towards the target analysis'
         ENDIF

       ENDIF
     ENDIF
   ENDIF


   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)

!
! Compute the vertical weighting function to scale the surface wind analysis to 
! the lowest model level, if the first model half-layer is greater
! than 10 meters and we are in the free convection regime (REGIME=4.0). 
!
   IF( iwinds == 1 ) THEN
     wndcor_u(:,:) = 1.0
     DO j=jts,jtf
     DO i=itsu,itf
       reg =  0.5 * ( regime(i-1,  j) + regime(i,  j) )
       difz = 0.5 * ( z(i-1,1,j) - ht(i-1,j)  &
                    + z(i,  1,j) - ht(i,  j)    ) 
       IF( reg > 3.5 .AND. difz > 10.0 ) THEN
         znt0 = 0.5 * (    znt(i-1,  j) +    znt(i,  j) )
         IF( znt0 <= 0.2) THEN
           wndcor_u(i,j) = 1.0+0.320*znt0**0.2
         ELSE
           wndcor_u(i,j) = 1.169+0.315*znt0
         ENDIF

         wr14 = log(40.0/0.05)
         wr1z = log(difz/0.05)
         wr24 = log(40.0/1.0)
         wr2z = log(difz/1.0)
         wndfac = 0.5*(WR1Z/WR14+WR2Z/WR24) 
         wndcor_u(i,j) = wndfac*wndcor_u(i,j)
       ENDIF
     ENDDO
     ENDDO

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min_sfc ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,a,f10.4)') &
             '    D0',id,' sample wndcor_u values at i,j=', i0, j0, &
             ' wndcor_u=', wndcor_u(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF
   ELSE 
     wndcor_u(:,:) = 1.0
   ENDIF

   IF( iwinds == 1 ) THEN
     wndcor_v(:,:) = 1.0
     DO j=jtsv,jtf
     DO i=its,itf
       reg =  0.5 * ( regime(i,  j-1) + regime(i,  j) )
       difz = 0.5 * ( z(i,1,j-1) - ht(i,j-1)  &
                  +   z(i,1,j  ) - ht(i,j  )    )
       IF( reg > 3.5 .AND. difz > 10.0 ) THEN 
         znt0 = 0.5 * (    znt(i,  j-1) +    znt(i,  j) )
         IF( znt0 <= 0.2) THEN
           wndcor_v(i,j) = 1.0+0.320*znt0**0.2
         ELSE
           wndcor_v(i,j) = 1.169+0.315*znt0
         ENDIF
       
         wr14 = log(40.0/0.05)
         wr1z = log(difz/0.05)
         wr24 = log(40.0/1.0)
         wr2z = log(difz/1.0)
         wndfac = 0.5*(WR1Z/WR14+WR2Z/WR24)
           wndcor_v(i,j) = wndfac*wndcor_v(i,j)
       ENDIF
     ENDDO
     ENDDO
     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min_sfc ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,a,f10.4)') &
             '    D0',id,' sample wndcor_v values at i,j=', i0, j0, &
             ' wndcor_v=', wndcor_v(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF
   ELSE 
     wndcor_v(:,:) = 1.0
   ENDIF
!
! Compute saturation mixing ratio so that nudging to a super-saturated state
! is not allowed.
!
   IF( iqsat == 1 ) THEN
     DO j=jts,jtf
     DO k=kts,ktf
     DO i=its,itf
       es = SVP1*EXP(SVP2*(t3d(i,k,j)-SVPT0)/(t3d(i,k,j)-SVP3))  * 10.0    ! mb
       qsat(i,k,j) = EP_2*es/(p3d(i,k,j)/100.0-es)
     ENDDO
     ENDDO
     ENDDO

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min_sfc ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts 
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample moisture values (g/kg) at i,k,j=', i0, k, j0, &
               ' qv3d=', qv3d(i0,k,j0)*1000.0, ' qsat=', qsat(i0,k,j0)*1000.0
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF
       ENDIF
     ENDIF
   ENDIF
!
! Obs data density weighting.
!
   IF( idd == 1 ) THEN

     IF( rinblw < 0.001 ) THEN
       IF ( wrf_dm_on_monitor()) THEN
         WRITE(message,'(a)') 'Error in rinblw, please specify a reasonable value ***'
         CALL wrf_message( TRIM(message) )
       ENDIF
       CALL wrf_error_fatal('In grid FDDA')
     ENDIF

     rindx = rinblw*1000.0/dx
     m = -0.8*2.0/rindx

     DO j=MAX(jts-2,jds),MIN(jte+2,jde-1)
     DO i=MAX(its-2,ids),MIN(ite+2,ide-1)
       IF( odis_ndg_old(i,j) < 0.5*rinblw ) THEN
         blw_old(i,j) = 1.0
       ELSE
         x = min( odis_ndg_old(i,j)*1000./dx, rindx )
         blw_old(i,j) = m * x + b
       ENDIF

       IF( odis_ndg_new(i,j) < 0.5*rinblw ) THEN
         blw_new(i,j) = 1.0
       ELSE
         x = min( odis_ndg_new(i,j)*1000./dx, rindx )
         blw_new(i,j) = m * x + b
       ENDIF
     ENDDO
     ENDDO

! Smoother applies one point outside the tile, but one point in from boundaries
     CALL smther(blw_old, its-2,ite+2, jts-2,jte+2, 1, &
                 MAX(its-1,ids+1), MIN(ite+1,ide-2), MAX(jts-1,jds+1), MIN(jte+1,jde-2))
     CALL smther(blw_new, its-2,ite+2, jts-2,jte+2, 1, &
                 MAX(its-1,ids+1), MIN(ite+1,ide-2), MAX(jts-1,jds+1), MIN(jte+1,jde-2))

     WHERE ( blw_old > 1.0)
        blw_old = 1.0
     END WHERE
     WHERE ( blw_new > 1.0)
        blw_new = 1.0
     END WHERE
     WHERE ( blw_old < 0.0)
        blw_old = 0.0
     END WHERE
     WHERE ( blw_new < 0.0)
        blw_new = 0.0
     END WHERE

     IF ( wrf_dm_on_monitor()) THEN
       IF( xtime-xtime_old_sfc < 0.5*dt/60.0 ) THEN
         IF( dbg_level .GE. 10 .AND. xtime <= actual_end_fdda_min_sfc ) THEN
           i0 = (ite-its)/2+its
           j0 = (jte-jts)/2+jts
           WRITE(message,'(a,i1,a,2i4,4(a,f10.4))') &
             '    D0',id,' sample blw values at i,j=', i0, j0, &
             ' odis_ndg_old=', odis_ndg_old(i0,j0), ' km odis_ndg_new=', odis_ndg_new(i0,j0), &
             ' km  blw_old=', blw_old(i0,j0), ' blw_new=', blw_new(i0,j0)
           CALL wrf_message( TRIM(message) )
         ENDIF
       ENDIF
     ENDIF

   ENDIF
!
! tfac_sfc for surface analysis nudging
!
   IF( xtime >= actual_end_fdda_min_sfc-ABS(dtramp_min) .AND. xtime <= actual_end_fdda_min_sfc &
       .AND. dtramp_min > 0.0 .AND. if_ramping == 1 )  &

    !BPR BEGIN
    !The original method assumed that to be here in the code with dtramp_min>0
    !meant that we were after the valid time of *_ndg_new and so used the
    !values *_ndg_old and *_ndg_new to extrapolate a current analysis value.
    !HOWEVER, in practice once we get to the valid time of *_ndg_new WRF will
    !read in the next pair of *_ndg_old and *_ndg_new values, even if we are
    !at the beginning of the rampdown period. Since the *_ndg_new values have
    !a valid time after the beginning of the rampdown period we do not want
    !to use them as we would be using analyses valid after the supposed end
    !time of the analysis nudging.
    !coef = (xtime-xtime_old_sfc+analysis_interval_sfc)/(analysis_interval_sfc*1.0)
    !Use only the *_ndg_old values
    coef = 0.0 
    !BPR END

!  print*, 'coef =', xtime_old_sfc, xtime, xtime_new_sfc, coef
!              
! Compute surface analysis nudging tendencies for u, v, t and q
!              
! FASDAS
!
     IF( fasdas == 1 ) THEN

      !Edit TWG2015 Add new variable wpblfasdas to avoid altering global wpbl
      !field when using the FASDAS option

       wpblfasdas = 1.0
       DO j=jts,jtf
       DO k=kts,ktf
       DO i=its,itf
          if( k == 1 ) then
            wpblfasdas(i, k, j) = 0.0
          else
            wpblfasdas(i, k, j) = 1.0
          endif
       ENDDO
       ENDDO
       ENDDO

     ENDIF
!
! END FASDAS
!
   DO j=jts,jtf
   DO k=kts,ktf
   DO i=itsu,itf
     IF( idd == 1 ) THEN
       blw = 0.5* (blw_old(i-1,j)+blw_old(i,j)) * ( 1.0 - coef ) &
           + 0.5* (blw_new(i-1,j)+blw_new(i,j)) * coef
     ELSE
       blw = 1.0
     ENDIF
     val_analysis = u10_ndg_old(i,j) *( 1.0 - coef ) + u10_ndg_new(i,j) * coef
     val_analysis = val_analysis * wndcor_u(i,j)
     RUNDGDTEN(i,k,j) = guv_sfc * (1.0-wpbl(i,k,j,1)) * wzfac(k,1) * tfac_sfc * blw * &
                         ( val_analysis - u3d(i,1,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jtsv,jtf
   DO k=kts,ktf 
   DO i=its,itf

     IF( idd == 1 ) THEN
       blw = 0.5* (blw_old(i,j-1)+blw_old(i,j)) * ( 1.0 - coef ) &
           + 0.5* (blw_new(i,j-1)+blw_new(i,j)) * coef
     ELSE
       blw = 1.0
     ENDIF
     val_analysis = v10_ndg_old(i,j) *( 1.0 - coef ) + v10_ndg_new(i,j) * coef
     val_analysis = val_analysis * wndcor_v(i,j)
     RVNDGDTEN(i,k,j) = guv_sfc * (1.0-wpbl(i,k,j,2)) * wzfac(k,2) * tfac_sfc * blw * &
                       ( val_analysis - v3d(i,1,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
     IF( idd == 1 ) THEN
       blw = blw_old(i,j) * ( 1.0 - coef ) + blw_new(i,j) * coef
     ELSE
       blw = 1.0
     ENDIF

!ckay2015 and TWG2015 add stability factor for stable regime, ensure
!consistency between Direct and Indirect nudging, and Convert potential
!temperature tendency to temperature tendency
!
! FASDAS
!
     IF( fasdas == 1 ) THEN
         if(regime(i,j).le.1.1) then
          stabFac = 1.0/3.0
         else
          stabFac = 1.0
         end if

     val_analysis = th2_ndg_old(i,j) *( 1.0 - coef ) + th2_ndg_new(i,j) * coef
     !BPR BEGIN
     !RTHNDGDTEN(i,k,j) = stabFac*gt_sfc * (1.0-wpblfasdas(i,k,j)) * wzfac(k,3) * tfac * blw * &
     RTHNDGDTEN(i,k,j) = stabFac*gt_sfc * (1.0-wpblfasdas(i,k,j)) * wzfac(k,3) * tfac_sfc * blw * &
     !BPR END
                          ( val_analysis - th3d(i,1,j))

     DiffTN = val_analysis - th3d(i,1,j)
     if(k.eq.1) then
        exf = (1.0E05/p3d(i,k,j))**(287./1004.)
        SDA_HFX(i,j) = RTHNDGDTEN(i,k,j)/exf !TWG 2015
     else
        RTHNDGDTEN(i,k,j) = 0.0
     end if

     val_analysis = q2_ndg_old(i,j) *( 1.0 - coef ) + q2_ndg_new(i,j) * coef
     IF( iqsat ==  1 .AND. val_analysis > qsat(i,k,j) ) val_analysis = qsat(i,k,j)
     !BPR BEGIN
     !RQVNDGDTEN(i,k,j) = stabFac*gq_sfc * (1.0-wpblfasdas(i,k,j)) * wzfac(k,4) * tfac * blw * &
     RQVNDGDTEN(i,k,j) = stabFac*gq_sfc * (1.0-wpblfasdas(i,k,j)) * wzfac(k,4) * tfac_sfc * blw * &
     !BPR END
                          ( val_analysis - qv3d(i,k,j) )
     if(k.eq.1) then
         SDA_QFX(i,j) = RQVNDGDTEN(i,k,j)
     else
         RQVNDGDTEN(i,k,j) = 0.0
     end if

     ELSE

     val_analysis = th2_ndg_old(i,j) *( 1.0 - coef ) + th2_ndg_new(i,j) * coef
     !BPR BEGIN
     !RTHNDGDTEN(i,k,j) = gt_sfc * (1.0-wpbl(i,k,j,3)) * wzfac(k,3) * tfac * blw * &
     RTHNDGDTEN(i,k,j) = gt_sfc * (1.0-wpbl(i,k,j,3)) * wzfac(k,3) * tfac_sfc * blw * &
     !BPR END
                          ( val_analysis - th3d(i,1,j))

     val_analysis = q2_ndg_old(i,j) *( 1.0 - coef ) + q2_ndg_new(i,j) * coef
     IF( iqsat ==  1 .AND. val_analysis > qsat(i,k,j) ) val_analysis = qsat(i,k,j)
     !BPR BEGIN
     !RQVNDGDTEN(i,k,j) = gq_sfc * (1.0-wpbl(i,k,j,4)) * wzfac(k,4) * tfac * blw * &
     RQVNDGDTEN(i,k,j) = gq_sfc * (1.0-wpbl(i,k,j,4)) * wzfac(k,4) * tfac_sfc * blw * &
     !BPR END
                          ( val_analysis - qv3d(i,k,j) )

    ENDIF
!
! END FASDAS
!
   ENDDO
   ENDDO
   ENDDO

   !BPR BEGIN 
   !Diagnostic print
   IF ( wrf_dm_on_monitor()) THEN
    IF( dbg_level .GE. 10 ) THEN
     i0 = (ite-its)/2+its
     j0 = (jte-jts)/2+jts 
     WRITE(message,'(a,i1,a,f7.2,a,i4,a,i4,a,f7.2,a,f4.2,a,f7.2,a,f4.2,a,f4.2 )') &
      '    D0',id,' At xtime=',xtime,' SFC FDDA sample TH2 analysis (i=',i0,', j=',j0,') = (th2_ndg_old=', &
      th2_ndg_old(i0,j0),') * ',1.0-coef,' + (th2_ndg_new=',th2_ndg_new(i0,j0),') * ',coef, &
      ' where tfac_sfc=',tfac_sfc
     CALL wrf_message( TRIM(message) )
     WRITE(message,'(a,i1,a,f7.2,a,i4,a,f7.2,a,f7.2)') &
      '    D0',id,'  xtime_old_sfc=',xtime_old_sfc,' analysis_interval_sfc=',analysis_interval_sfc, &
      ' actual_end_fdda_min_sfc=', actual_end_fdda_min_sfc,' dtramp_min=',dtramp_min
     CALL wrf_message( TRIM(message) )
    END IF
   END IF
   !BPR END 

   END SUBROUTINE sfddagd

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
               SDA_HFX, SDA_QFX, QNORM, HFX_BOTH, QFX_BOTH, fasdas,& !fasdas
               HFX_FDDA,                                           & !fasdas
               run_hours,  &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min, end_fdda_hour, &
               grid_sfdda, guv_sfc, gt_sfc, gq_sfc,                &
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
!
   INTEGER , INTENT(IN)         ::  id
   LOGICAL, INTENT(IN)          ::  restart, allowed_to_read
   INTEGER, INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                       rundgdten, &
                                                       rvndgdten, &
                                                      rthndgdten, &
                                                      rqvndgdten
!
! FASDAS
!
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: &
                                                       SDA_HFX, &
                                                       SDA_QFX, &
                                                       QNORM, HFX_BOTH, QFX_BOTH
   INTEGER, INTENT(IN   )           ::  fasdas
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                  HFX_FDDA
!
! END FASDAS
!
   INTEGER,  INTENT(IN)   ::      run_hours
   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping, grid_sfdda
   REAL,     INTENT(IN)   ::      dtramp_min
   REAL, INTENT(IN)       ::      guv, gt, gq
   REAL, INTENT(IN)       ::      guv_sfc, gt_sfc, gq_sfc
   REAL                   ::      actual_end_fdda_min

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: rmundgdten
   INTEGER :: i, j, k

   LOGICAL , EXTERNAL     ::      wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message

   IF ( wrf_dm_on_monitor() ) THEN  

     IF( guv > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for wind is applied and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ELSE IF( guv < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Guv must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for wind is not applied and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for temperature is applied and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ELSE IF( gt < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Gt must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' 3-D analysis nudging for temperature is not applied and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is applied and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ELSE IF( gq < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Gq must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is not applied and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_no_pbl_nudging_uv == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' 3-D analysis nudging for wind is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_no_pbl_nudging_t == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' 3-D analysis nudging for temperature is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_no_pbl_nudging_q == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
         'D0',id,' 3-D analysis nudging for water vapor mixing ratio is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_zfac_uv == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' 3-D analysis nudging for wind is turned off below layer', k_zfac_uv
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_zfac_t == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' 3-D analysis nudging for temperature is turned off below layer', k_zfac_t
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_zfac_q == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
          'D0',id,' 3-D analysis nudging for water vapor mixing ratio is turned off below layer', &
           k_zfac_q
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( grid_sfdda >=1 ) THEN
       IF( guv_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for wind is applied and Guv_sfc= ', guv_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( guv_sfc < 0.0 ) THEN
         CALL wrf_error_fatal('In grid FDDA, Guv_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for wind is not applied and Guv_sfc= ', guv_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

       IF( gt_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surface analysis nudging for temperature is applied and Gt_sfc= ', gt_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( gt_sfc < 0.0 ) THEN
         CALL wrf_error_fatal('In grid FDDA, Gt_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
             'D0',id,' surafce analysis nudging for temperature is not applied and Gt_sfc= ', gt_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

       IF( gq_sfc > 0.0 ) THEN
         WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' surface analysis nudging for water vapor mixing ratio is applied and Gq_sfc= ', gq_sfc
         CALL wrf_message(TRIM(message))
       ELSE IF( gq_sfc < 0.0 ) THEN
         CALL wrf_error_fatal('In grid FDDA, Gq_sfc must be positive.')
       ELSE
         WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' surface analysis nudging for water vapor mixing ratio is not applied and Gq_sfc= ', gq_sfc
         CALL wrf_message(TRIM(message))
       ENDIF

     ENDIF

     IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
       IF( dtramp_min <= 0.0 ) THEN
         actual_end_fdda_min = end_fdda_hour*60.0
       ELSE
         actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       ENDIF

       IF( actual_end_fdda_min <= run_hours*60. ) THEN
          WRITE(message,'(a,i1,a)') &
            'D0',id,' analysis nudging is ramped down near the end of the nudging period,'
          CALL wrf_message(TRIM(message))

          WRITE(message,'(a,f6.2,a,f6.2,a)') &
             '      starting at ', (actual_end_fdda_min - ABS(dtramp_min))/60.0, &
             'h, ending at ', actual_end_fdda_min/60.0,'h.'
          CALL wrf_message(TRIM(message))
       ENDIF
     ENDIF

   ENDIF

   IF(.not.restart) THEN
     DO j = jts,jte
     DO k = kts,kte
     DO i = its,ite
        rundgdten(i,k,j) = 0.
        rvndgdten(i,k,j) = 0.
        rthndgdten(i,k,j) = 0.
        rqvndgdten(i,k,j) = 0.
! TWG 2015 Psuedo radiative flux
        HFX_FDDA(i,k,j) = 0.
! TWG END
        if(k.eq.kts) rmundgdten(i,j) = 0.
     ENDDO
     ENDDO
     ENDDO
!
! FASDAS
!
     IF( fasdas == 1 ) THEN
       DO j = jts,jte
       DO i = its,ite
          SDA_HFX(I,J)  = 0.0
          SDA_QFX(I,J)  = 0.0
          QNORM(I,J)    = 0.0
          HFX_BOTH(I,J) = 0.0
          QFX_BOTH(I,J) = 0.0  
       ENDDO
       ENDDO
     ENDIF
!
! END FASDAS
!
   ENDIF

   END SUBROUTINE fddagdinit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE smther(slab, idimst, idimnd, jdimst, jdimnd, npass, ist, ind, jst, jnd)
!
!    PURPOSE: SPATIALLY SMOOTH DATA IN SLAB TO DAMPEN SHORT
!             WAVELENGTH COMPONENTS.
!
!    Implemented based on the same smoothing subroutine used in MM5, with modifications to
!    remove the extra loop that causes unneccessary desmoothing.  Aijun Deng (Penn State),
!    December 2008 
!
    IMPLICIT NONE

    INTEGER :: idimst, idimnd, jdimst, jdimnd, npass, ist, ind, jst, jnd
    INTEGER :: i, j, k, kk
    REAL    :: avg
    REAL, DIMENSION(idimst:idimnd, jdimst:jdimnd) :: SLAB
    REAL, DIMENSION(idimst:idimnd, jdimst:jdimnd) :: SLAB_ORIG
    REAL, DIMENSION(2)                            :: XNU

    IF(NPASS.EQ.0)RETURN
    XNU(1)=0.50
    XNU(2)=-0.52
    DO K=1,NPASS
      KK = 2 - MOD(K,2)

      DO J=JDIMST,JDIMND
        DO I=IDIMST,IDIMND
           SLAB_ORIG(I,J) = SLAB(I,J)
        END DO
      END DO

      DO J=JST,JND
        DO I=IST,IND
          AVG = ( SLAB_ORIG(I+1,J  ) + &
                  SLAB_ORIG(I-1,J  ) + &
                  SLAB_ORIG(I  ,J+1) + &
                  SLAB_ORIG(I  ,J-1) ) * 0.25
          SLAB(I,J)=SLAB_ORIG(I,J)+XNU(KK)*(AVG - SLAB_ORIG(I,J))
        ENDDO
      ENDDO

    ENDDO

  END SUBROUTINE smther

!-------------------------------------------------------------------
END MODULE module_fdda_psufddagd
