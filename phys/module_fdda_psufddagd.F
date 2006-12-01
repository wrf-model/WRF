!wrf:model_layer:physics
!
!
!
MODULE module_fdda_psufddagd

  USE module_dm

CONTAINS
!
!-------------------------------------------------------------------
!
   SUBROUTINE fddagd(itimestep,dt,xtime,id,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min,  &
               u3d,v3d,th3d,t3d,                 &
               qv3d,     &
               p3d,pi3d,                &
               u_ndg_old,v_ndg_old,t_ndg_old,q_ndg_old,mu_ndg_old,       &
               u_ndg_new,v_ndg_new,t_ndg_new,q_ndg_new,mu_ndg_new,       &
               RUNDGDTEN,RVNDGDTEN,RTHNDGDTEN,RQVNDGDTEN,RMUNDGDTEN,&
               pblh, ht, z, z_at_w,                             &
               ids,ide, jds,jde, kds,kde,                           &
               ims,ime, jms,jme, kms,kme,                           &
               its,ite, jts,jte, kts,kte                        )

!-------------------------------------------------------------------
   implicit none
!-------------------------------------------------------------------
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

   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping

   INTEGER , INTENT(IN)   ::      id
   REAL,     INTENT(IN)   ::      DT, xtime, dtramp_min

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

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(INOUT)   ::           u_ndg_old, &
                                          v_ndg_old, &
                                          t_ndg_old, &
                                          q_ndg_old, &
                                          u_ndg_new, &
                                          v_ndg_new, &
                                          t_ndg_new, &
                                          q_ndg_new

   REAL,     DIMENSION( ims:ime, jms:jme ), &
             INTENT(INOUT) ::   mu_ndg_old, &
                                mu_ndg_new

   REAL,     DIMENSION( ims:ime, kms:kme, jms:jme ), &
             INTENT(IN)   ::                    u3d, &
                                                v3d

   REAL,  DIMENSION( ims:ime, jms:jme ), INTENT(IN) :: pblh, &
                                                         ht

   REAL, INTENT(IN)    :: guv, gt, gq

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf, i0, k0, j0
   REAL                :: xtime_old, xtime_new, coef, val_analysis
   INTEGER             :: kpbl, dbg_level

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, tfac, actual_end_fdda_min
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ) :: wpbl  ! 1: u, 2: v, 3: t, 4: q
   REAL, DIMENSION( kts:kte, 4 )                   :: wzfac ! 1: u, 2: v, 3: t, 4: q

   LOGICAL , EXTERNAL  :: wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message

   xtime_old = FLOOR(xtime/analysis_interval) * analysis_interval * 1.0
   xtime_new = xtime_old + analysis_interval
   coef = (xtime-xtime_old)/(xtime_new-xtime_old)


   IF ( wrf_dm_on_monitor()) THEN

     CALL get_wrf_debug_level( dbg_level )

     IF( xtime-xtime_old < 0.5*dt/60.0 ) THEN

       IF( xtime < end_fdda_hour*60.0 ) THEN
         WRITE(message,'(a,i1,a,f10.3,a)') &
          'D0',id,' Analysis nudging read in new data at time = ', xtime, ' min.'
         CALL wrf_message( TRIM(message) )
         WRITE(message,'(a,i1,a,2f8.2,a)') &
          'D0',id,' Analysis nudging bracketing times = ', xtime_old, xtime_new, ' min.'
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
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' u_ndg_old=', u_ndg_old(i0,k,j0), ' u_ndg_new=', u_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' v_ndg_old=', v_ndg_old(i0,k,j0), ' v_ndg_new=', v_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gt > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' t_ndg_old=', t_ndg_old(i0,k,j0), ' t_ndg_new=', t_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
         ENDIF

         IF( gq > 0.0 ) THEN
           DO k = kts, kte
             WRITE(message,'(a,i1,a,3i4,a,f10.4,a,f10.4)') &
               '    D0',id,' sample analysis values at i,k,j=', i0, k, j0, &
               ' q_ndg_old=', q_ndg_old(i0,k,j0), ' q_ndg_new=', q_ndg_new(i0,k,j0)
             CALL wrf_message( TRIM(message) )
           ENDDO
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
! if_no_pbl_nudging_q swithes) are set to 1, compute the weighting function, wpbl(:,k,:,:), 
! based on the PBL depth.  wpbl = 1 above the PBL and wpbl = 0 in the PBL.  If all 
! the switche are set to zero, wpbl = 1 (default value).
!
   wpbl(:,:,:,:) = 1.0

   IF( if_no_pbl_nudging_uv == 1 ) THEN

     DO j=jts,jtf 
     DO i=itsu,itf

       kpbl = 1
       zpbl = 0.5 * ( pblh(i-1,j) + pblh(i,j) )

       loop_ku: DO k=kts,ktf 
         zagl     = 0.5 * ( z(i-1,k,j)-ht(i-1,j) + z(i,k,j)-ht(i,j) )
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
         zagl     = 0.5 * ( z(i,k,j-1)-ht(i,j-1) + z(i,k,j)-ht(i,j) )
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

   IF( if_no_pbl_nudging_t == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
        
       loop_kt: DO k=kts,ktf
         zagl     = z(i,k,j)-ht(i,j)
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

   IF( if_no_pbl_nudging_q == 1 ) THEN
   
     DO j=jts,jtf
     DO i=its,itf

       kpbl = 1
       zpbl = pblh(i,j)
          
       loop_kq: DO k=kts,ktf
         zagl     = z(i,k,j)-ht(i,j)
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
! If if_ramping and dtramp_min are defined by user, comput a time weighting function, tfac, 
! for analysis nudging so that at the end of the nudging period (which has to be at a 
! analysis time) we ramp down the nudging coefficient, based on the use-defined sign of dtramp_min.
!
! When dtramp_min is negative, ramping ends at end_fdda_hour and starts at 
! end_fdda_hour-ABS(dtramp_min).  
!
! When dtramp_min is positive, ramping starts at end_fdda_hour and ends at 
! end_fdda_hour+ABS(dtramp_min). In this case, the obs values are extrapolated using 
! the obs tendency saved from the previous FDDA wondow.  More specifically for extrapolation, 
! coef (see codes below) is recalculated to reflect extrapolation during the ramping period.
!
   tfac = 1.0

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
       IF( dtramp_min > 0.0 ) coef = (xtime-xtime_old+analysis_interval*60.0)/(analysis_interval*60.0)
     ELSE                                                     
       tfac = 0.0
     ENDIF

   ENDIF                                                  
!
! Compute 3-D nudging tendencies for u, v, t and q
!
   DO j=jts,jtf
   DO k=kts,ktf
   DO i=itsu,itf
     val_analysis = u_ndg_old(i,k,j) *( 1.0 - coef ) + u_ndg_new(i,k,j) * coef
     RUNDGDTEN(i,k,j) = guv * wpbl(i,k,j,1) * wzfac(k,1) * tfac * &
                         ( val_analysis - u3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jtsv,jtf
   DO k=kts,ktf
   DO i=its,itf
     val_analysis = v_ndg_old(i,k,j) *( 1.0 - coef ) + v_ndg_new(i,k,j) * coef
     RVNDGDTEN(i,k,j) = guv * wpbl(i,k,j,2) * wzfac(k,2) * tfac * &
                       ( val_analysis - v3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   DO j=jts,jtf
   DO k=kts,ktf
   DO i=its,itf
     val_analysis = t_ndg_old(i,k,j) *( 1.0 - coef ) + t_ndg_new(i,k,j) * coef
     RTHNDGDTEN(i,k,j) =  gt * wpbl(i,k,j,3) * wzfac(k,3) * tfac * &
                          ( val_analysis - th3d(i,k,j) + 300.0 )

     val_analysis = q_ndg_old(i,k,j) *( 1.0 - coef ) + q_ndg_new(i,k,j) * coef
     RQVNDGDTEN(i,k,j) =  gq * wpbl(i,k,j,4) * wzfac(k,4) * tfac * &
                          ( val_analysis - qv3d(i,k,j) )
   ENDDO
   ENDDO
   ENDDO

   END SUBROUTINE fddagd


   SUBROUTINE fddagdinit(id,rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
               run_hours,  &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, if_ramping, dtramp_min, end_fdda_hour, &
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
   INTEGER,  INTENT(IN)   ::      run_hours
   INTEGER,  INTENT(IN)   ::      if_no_pbl_nudging_uv, if_no_pbl_nudging_t, &
                                  if_no_pbl_nudging_q, end_fdda_hour
   INTEGER,  INTENT(IN)   ::      if_zfac_uv, if_zfac_t, if_zfac_q
   INTEGER,  INTENT(IN)   ::      k_zfac_uv,  k_zfac_t,  k_zfac_q
   INTEGER,  INTENT(IN)   ::      if_ramping
   REAL,     INTENT(IN)   ::      dtramp_min
   REAL, INTENT(IN)       ::      guv, gt, gq
   REAL                   ::      actual_end_fdda_min

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: rmundgdten
   INTEGER :: i, j, k

   LOGICAL , EXTERNAL     ::      wrf_dm_on_monitor

   CHARACTER (LEN=256) :: message

   IF ( wrf_dm_on_monitor() ) THEN  

     IF( guv > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Analysis nudging for wind is turned on and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ELSE IF( guv < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Guv must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Analysis nudging for wind is turned off and Guv= ', guv
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Analysis nudging for temperature is turned on and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ELSE IF( gt < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Gt must be positive.')
     ELSE 
       WRITE(message,'(a,i1,a,e12.4)') &
           'D0',id,' Analysis nudging for temperature is turned off and Gt= ', gt
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 ) THEN
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' Analysis nudging for water vapor mixing ratio is turned on and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ELSE IF( gq < 0.0 ) THEN
       CALL wrf_error_fatal('In grid FDDA, Gq must be positive.')
     ELSE
       WRITE(message,'(a,i1,a,e12.4)') &
         'D0',id,' Analysis nudging for water vapor mixing ratio is turned off and Gq= ', gq
       CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_no_pbl_nudging_uv == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' Analysis nudging for wind is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_no_pbl_nudging_t == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
           'D0',id,' Analysis nudging for temperature is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_no_pbl_nudging_q == 1 ) THEN
        WRITE(message,'(a,i1,a)') &
         'D0',id,' Analysis nudging for water vapor mixing ratio is turned off within the PBL.'
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( guv > 0.0 .AND. if_zfac_uv == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' Analysis nudging for wind is turned off below layer', k_zfac_uv
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gt > 0.0 .AND. if_zfac_t == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
           'D0',id,' Analysis nudging for temperature is turned off below layer', k_zfac_t
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( gq > 0.0 .AND. if_zfac_q == 1 ) THEN
        WRITE(message,'(a,i1,a,i3)') &
          'D0',id,' Analysis nudging for water vapor mixing ratio is turned off below layer', &
           k_zfac_q
        CALL wrf_message(TRIM(message))
     ENDIF

     IF( if_ramping == 1 .AND. ABS(dtramp_min) > 0.0 ) THEN
       IF( dtramp_min <= 0.0 ) THEN
         actual_end_fdda_min = end_fdda_hour*60.0
       ELSE
         actual_end_fdda_min = end_fdda_hour*60.0 + ABS(dtramp_min)
       ENDIF

       IF( actual_end_fdda_min <= run_hours*60. ) THEN
          WRITE(message,'(a,i1,a)') &
            'D0',id,' Analysis nudging is ramped down near the end of the nudging period,'
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
        if(k.eq.kts) rmundgdten(i,j) = 0.
     ENDDO
     ENDDO
     ENDDO
   ENDIF

   END SUBROUTINE fddagdinit
!-------------------------------------------------------------------
END MODULE module_fdda_psufddagd
