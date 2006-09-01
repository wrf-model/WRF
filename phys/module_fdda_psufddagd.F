!wrf:model_layer:physics
!
!
!
MODULE module_fdda_psufddagd
CONTAINS
!
!-------------------------------------------------------------------
!
   SUBROUTINE fddagd(itimestep,dt,xtime,analysis_interval, end_fdda_hour, &
               if_no_pbl_nudging_uv, if_no_pbl_nudging_t, if_no_pbl_nudging_q, &
               if_zfac_uv, k_zfac_uv, if_zfac_t, k_zfac_t, if_zfac_q, k_zfac_q, &
               guv, gt, gq, idynin, dtramp_min,  &
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
   INTEGER,  INTENT(IN)   ::      idynin

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

   INTEGER             :: i, j, k, itsu, jtsv, itf, jtf, ktf
   REAL                :: xtime_old, xtime_new, xtime_prev, coef, val_analysis
   INTEGER             :: kpbl

   REAL                :: zpbl, zagl, zagl_bot, zagl_top, dtr, tfac, actual_end_fdda_min
   REAL, DIMENSION( its:ite, kts:kte, jts:jte, 4 ) :: wpbl  ! 1: u, 2: v, 3: t, 4: q
   REAL, DIMENSION( kts:kte, 4 )                   :: wzfac ! 1: u, 2: v, 3: t, 4: q

   xtime_prev = (itimestep-1)*dt/60.0
   xtime_old = FLOOR(xtime/analysis_interval) * analysis_interval * 1.0
   xtime_new = xtime_old + analysis_interval
   coef = (xtime-xtime_old)/(xtime_new-xtime_old)

   jtsv=MAX0(jts,jds+1)
   itsu=MAX0(its,ids+1)

   jtf=MIN0(jte,jde-1)
   ktf=MIN0(kte,kde-1)
   itf=MIN0(ite,ide-1)
   IF( itimestep == 1 ) PRINT*, 'Analysis nudging is on and Guv, Gt, Gq = ', guv, gt, gq
!
! If the user-defined namelist switches (if_no_pbl_nudging_uv, if_no_pbl_nudging_t, 
! if_no_pbl_nudging_q swithes) are set to 1, compute the weighting function, wpbl(:,k,:,:), 
! based on the PBL depth.  wpbl = 1 above the PBL and wpbl = 0 in the PBL.  If all 
! the switche are set to zero, wpbl = 1 (default value).
!
   wpbl(:,:,:,:) = 1.0

   IF( if_no_pbl_nudging_uv == 1 ) THEN

     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for wind is turned off within the PBL.'

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

!      if( j==30 .and. i==30 ) print*, 'kpbl in u branch= ', kpbl

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

!      if( j==30 .and. i==30 ) print*, 'kpbl in v branch= ', kpbl

       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 2) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 2) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 2) = 1.0
       ENDDO

     ENDDO
     ENDDO

     print*, 'itimestep, xtime, pblh= ', itimestep, xtime, pblh(30,30), end_fdda_hour, &
             idynin, dtramp_min

   ENDIF

   IF( if_no_pbl_nudging_t == 1 ) THEN
   
     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for temperature is turned off within the PBL.'

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

!      if( j==30 .and. i==30 ) print*, 'kpbl in t branch= ', kpbl
        
       DO k=kts,ktf
         IF( k <= kpbl   ) wpbl(i, k, j, 3) = 0.0
         IF( k == kpbl+1 ) wpbl(i, k, j, 3) = 0.1
         IF( k >  kpbl+1 ) wpbl(i, k, j, 3) = 1.0
       ENDDO 
        
     ENDDO
     ENDDO

   ENDIF

   IF( if_no_pbl_nudging_q == 1 ) THEN
   
     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for water vapor mixing ratio is turned off within the PBL.'
        
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

!      if( j==30 .and. i==30 ) print*, 'kpbl in q branch= ', kpbl

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

     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for wind is turned off below layer ', k_zfac_uv 

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

     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for temperature is turned off below layer ', k_zfac_t

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
       
     IF( itimestep == 1 ) &
       PRINT*, 'Analysis nudging for water vapor mixing ratio is turned off below layer ', k_zfac_q

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
! Comput a time weighting function for analysis nudging so that at the end of the
! nudging period we ramp down the nudging coefficient.
!
   tfac = 1.0

   IF( idynin == 1 ) THEN
 
     actual_end_fdda_min = end_fdda_hour*60.0

     dtr = ABS( dtramp_min )

     IF( itimestep == 1 ) THEN
       PRINT*, 'Allowing nudging ramping down near the end of the nudging period.'
       PRINT*, 'and ramping down starts at hour ', (actual_end_fdda_min - dtr)/60.0, &
               ' and end at hour ', actual_end_fdda_min/60.0
     ENDIF

     IF( xtime < actual_end_fdda_min - dtr )THEN 
       tfac = 1.0
     ELSEIF( xtime >= actual_end_fdda_min - dtr .AND. xtime <= actual_end_fdda_min )THEN
       tfac = ( actual_end_fdda_min - xtime ) / dtr
     ELSE                                                     
       tfac = 0.0
     ENDIF

!     print*, 'xtime, tfac=', xtime, tfac, actual_end_fdda_min

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


   SUBROUTINE fddagdinit(rundgdten,rvndgdten,rthndgdten,rqvndgdten,rmundgdten,&
                      restart, allowed_to_read,                    &
                      ids, ide, jds, jde, kds, kde,                &
                      ims, ime, jms, jme, kms, kme,                &
                      its, ite, jts, jte, kts, kte                 )
!-------------------------------------------------------------------
   IMPLICIT NONE
!-------------------------------------------------------------------
!
   LOGICAL, INTENT(IN)          :: restart, allowed_to_read
   INTEGER, INTENT(IN)          ::  ids, ide, jds, jde, kds, kde, &
                                    ims, ime, jms, jme, kms, kme, &
                                    its, ite, jts, jte, kts, kte
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(OUT) :: &
                                                       rundgdten, &
                                                       rvndgdten, &
                                                      rthndgdten, &
                                                      rqvndgdten
   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: rmundgdten
   INTEGER :: i, j, k
!
!
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

