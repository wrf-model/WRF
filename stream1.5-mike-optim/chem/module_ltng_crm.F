! wrF:MODEL_LAYER:PHYSICS
!
MODULE module_ltng_crm

! ++ mcb Modified so that icnox and cgnox variables are in the scalar array

  IMPLICIT NONE

  INTEGER, PARAMETER, PRIVATE :: flash_length = 0 

CONTAINS

 SUBROUTINE ltng_decaria(itimestep,dt,w,t,p,                       &
                     rho,qr,qs,qg,c_no,s_ic,s_cg,                  &
                      tracer_opt,                                  &
                      ids,ide, jds,jde, kds,kde,                   &
                      ims,ime, jms,jme, kms,kme,                   &
                      ips,ipe, jps,jpe, kps,kpe,                   &
                      dx,dy,z,ht,xlat,xlon,                        &
                      dbz,                                         & 
                      lightning_time_step,                         &
                      lightning_start_seconds,temp_upper,          &
                      temp_lower,N_IC,N_CG,                        &
                      passive_ltng,flashrate_factor,               &
                      flashrate_method,iccg_method,                &
                      cellcount_method,                            &
                      ic_fr,cg_fr)

  USE module_state_description
! Driver layer modules
  USE module_dm , only : wrf_dm_max_real, wrf_dm_min_real, wrf_dm_sum_real

 INTEGER, INTENT (IN) ::   ids,ide, jds,jde, kds,kde,              &
                      ims,ime, jms,jme, kms,kme,                   &
                      ips,ipe, jps,jpe, kps,kpe,                   &
                      tracer_opt, itimestep,                       &
                      lightning_time_step,                         &  
                      lightning_start_seconds, passive_ltng

 REAL, INTENT (IN) ::   dt

  REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                  &
        INTENT(IN) ::                                         qr,  &
                                                              qs,  &
                                                              qg,  &
                                                              w,   &
                                                              t,   &
                                                              p,   &
                                                              rho, &
                                                              z,   &
                                                              dbz

 REAL, DIMENSION (ims:ime, kms:kme, jms:jme ),                    &
        INTENT(INOUT) ::                               c_no    

 REAL, DIMENSION (ims:ime, kms:kme, jms:jme ),                    &
        INTENT(INOUT) ::                               s_ic,      &
                                                       s_cg        

 REAL , DIMENSION( ims:ime , jms:jme ) ,                          &
        INTENT(INOUT)   ::                             ic_fr,     &
                                                       cg_fr

 REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   :: ht,xlat,xlon

 REAL, INTENT(IN) ::  dx, dy, temp_upper,temp_lower,N_IC,N_CG

 REAL,    INTENT(IN) ::  flashrate_factor
 INTEGER, INTENT(IN) ::  flashrate_method
 INTEGER, INTENT(IN) ::  iccg_method
 INTEGER, INTENT(IN) ::  cellcount_method
 INTEGER             ::  cellcount_opt

 REAL, DIMENSION (kms:kme) :: cellcount

 REAL :: kbot,ktop,k_lower,k_upper,totalcells, cell_length

 INTEGER :: i,j,k,ksfc,lightning_record,flashrate_CG,flashrate_IC,m

 REAL :: xcnoh,xcnos,zsdryc,zswetc,zhdryc,zhwetc,dadr,dads,dadh,   &
      cnoh,cnos,cnor,cr1,hwdn,swdn,rwdn,hwdnsq,swdnsq,rwdnsq,      &
      dhmin,dielf,pie,qrmin,qsmin,qhmin,tfr,tfrh,zrc,              &
      tem,gmh,gms,gmr,zrain,zswet,zhwet,zsdry,zhdry,reflectmin,    &
      z_sum,reflect,num_cells

 REAL :: sum_LNOx
 REAL :: term2,B

 REAL, DIMENSION (kps:kpe)  ::  temp_avg, pressure, count,f_dist,   &
                      f_dist_2, delta_mix_IC, delta_mix_CG,rho_avg, &
                      sumz, countz, avgz, dz
!mcb 1d variables sumz, countz, avgz declared, plus scalars below

 INTEGER :: flashrates_ic, flashrates_cg   
 REAL :: wmax, up_5ms, totalflashes  
 REAL :: tile_length

 REAL :: temp_min 
 INTEGER :: k_tmin 

 CHARACTER (LEN=80) :: message

!mcb
 LOGICAL :: end_of_file 
 
!*********************************************************************************
!*** The lightning subroutine estimates lightning NO production using observed ***
!*** IC and CG flashrates and a specified production scenario of NO per flash. ***
!*** NO is distributed bimodally in the vertical for IC flashes, and using a   ***
!*** single mode for CG flashes. At each level, NO is distributed equally      ***
!*** among all gridcells within the 20 dbz contour.  Parameters set by user:   ***
!*** lightning_time_step - seconds, can be same as model time step or multiple ***
!*** lightning_start_seconds - time, in seconds, to start calculating          ***
!***    lightning NO in model. Should be selected by comparing simulated storm ***       
!***    development with observations.                                         ***
!*** temp_upper - temperature in degrees C of upper mode isotherm for IC dist. ***
!*** temp_lower - temperature in degrees C of lower mode isotherm for IC and   ***
!***    CG distributions.                                                      ***
!*** N_IC - moles of NO produced per IC flash                                  ***
!*** N_CG - moles of NO produced per CG flash                                  ***
!*** flash_length - run the version of the parameterization based on flash     ***
!***    length (not added yet)                                                 ***
!*********************************************************************************

  tracltng_select:  SELECT CASE(tracer_opt)
   CASE (TRACER_TEST3)
! Do only for first call of lightning       
     IF (dt*itimestep .eq. float(lightning_start_seconds)) THEN 
        s_ic(ips:ipe,kps:kpe,jps:jpe) = 0.
        s_cg(ips:ipe,kps:kpe,jps:jpe) = 0.
     ELSE
        WHERE (s_ic(ips:ipe,kps:kpe,jps:jpe) .lt. 0.)
               s_ic(ips:ipe,kps:kpe,jps:jpe) = 0.
        END WHERE
    
        WHERE (s_cg(ips:ipe,kps:kpe,jps:jpe) .lt. 0.)
               s_cg(ips:ipe,kps:kpe,jps:jpe) = 0.
        END WHERE
     END IF 
   CASE DEFAULT
     call wrf_debug(15,'nothing done with lightning tracers here')
  END SELECT tracltng_select

! ******************************
! choice of cellcount_method
! ******************************

 IF (cellcount_method .eq. 1) THEN
  
! this is the default
! the followings will choose the cellcount_method for the user,
! either tile_wide (2) or domain_wide (3) 

    tile_length = (ipe-ips+1)*dx

    if (tile_length .gt. 10000.) then
       cellcount_opt = 2
    else
       cellcount_opt = 3 
    end if
 ELSE IF (cellcount_method .eq. 2) THEN
    cellcount_opt = 2
 ELSE IF (cellcount_method .eq. 3) THEN
    cellcount_opt = 3
 END IF

 IF (dt*itimestep .eq. float(lightning_start_seconds)) THEN
    write(message,'("WARNING:ltng_decaria: using cellcount_method = ",i2)') cellcount_opt 
    CALL WRF_MESSAGE (message)
 END IF

! ***********************
! get dz
! ***********************

 sumz(kps:kpe)=0.
 countz(kps:kpe)=0.

 DO j = jps, jpe
  DO k = kps, kpe
   DO i = ips, ipe
      sumz(k) = sumz(k) + (z(i,k,j) - ht(i,j))
      countz(k) = countz(k) + 1.
   END DO
  END DO
 END DO

 avgz(kps:kpe) = sumz(kps:kpe)/countz(kps:kpe)

 dz(kps) = 0.5*(avgz(kps+1) - avgz(kps))
 dz(kpe) = 0.5*(avgz(kpe) - avgz(kpe-1))

 DO k = kps+1, kpe-1
    dz(k) = 0.5*(avgz(k+1) + avgz(k)) - 0.5*(avgz(k) + avgz(k-1))
 END DO

! ***********************************
! get number of flashrates
! ***********************************

 IF (flashrate_method .eq. 0) THEN

    CALL WRF_ERROR_FATAL ('ltng_decaria: flashrate_method=0 is experimental, please contact Mary Barth') 

 ELSE IF (flashrate_method .eq. 1) THEN
       
    wmax = MAXVAL(w(ips:ipe,kps:kpe,jps:jpe))

    if (cellcount_opt .eq.3) then
       wmax = wrf_dm_max_real(wmax)      
    end if

    totalflashes = flashrate_factor * 5.7e-6 * wmax**4.5

 ELSE IF (flashrate_method .eq. 2) THEN

!-- mcb get flashrates from 5 m/s updraft volume based on Barthe et al 2008

    up_5ms = 0.

    DO j = jps, jpe
       DO k = kps, kpe
          DO i = ips, ipe
             if (t(i,k,j) .lt. 273.15-5.) then
                if (w(i,k,j) .gt. 5.) then
                   up_5ms = up_5ms + dx * dy * dz(k)
                endif
             endif
          END DO
       END DO
    END DO

    if (cellcount_opt .eq. 3) then
       up_5ms = wrf_dm_sum_real(up_5ms)
    end if 

    totalflashes = 5.82e-11 * up_5ms

 END IF  ! flashrate_method (default 1)

! ************************************************
! mcb multiply dz by 100 for delta_mix calculation
! ************************************************

 DO k = kps, kpe
   dz(k) = 100.*dz(k)
 END DO

! ***********************************
! get number IC & CG flashes
! ***********************************

 IF (iccg_method .eq. 0) THEN

! flashrates read from file

 ELSE IF (iccg_method .eq. 1) THEN

!-- prescribe CG/IC ratio

    flashrates_cg = nint(0.1*totalflashes)
    flashrates_ic = nint(totalflashes) - flashrates_cg

 ELSE IF (iccg_method .eq. 2) THEN

!-- prescribe CG/IC ratio -- this gives a rough assignment to the climatology of Boccippio et al 2001

    if (xlat(ips,jps) .gt. 25.) then                                  !northern Mexico (near Monterrey)
        if (xlon(ips,jps) .gt. -90.) then                             !east of New Orleans
           flashrates_cg = nint(0.4*totalflashes)              ! IC:CG = 1.5
        elseif (xlon(ips,jps) .gt. -105.) then                        !east of Boulder ; west of New Orleans
           flashrates_cg = nint(0.22*totalflashes)             ! IC:CG = 3.5
        else
           flashrates_cg = nint(0.4*totalflashes)
        endif
    else
       flashrates_cg = nint(0.4*totalflashes)
    endif

    flashrates_ic = nint(totalflashes) - flashrates_cg

 END IF  ! iccg_method (default 1)

! if no flashrates at all, return

if (((flashrates_cg .le. 0) .and. (flashrates_ic .le. 0)) ) return

!  print *,'itimestep= ', itimestep

!  write(message,'(" IC,CG flashrates,... ",2i4,2f9.2)') flashrates_cg,    &
!      flashrates_ic, totalflashes, wmax
!  CALL WRF_MESSAGE (message)

! *****************
! get cellcount(k)
! *****************

! *************************************************
! mcb - accumulate flashrate number
!  get number of grid cells in patch with dbz >= 20
!   and record location of highest dbz
! *************************************************

 cellcount(kps:kpe) = 0.

 DO j = jps, jpe
  DO k = kps, kpe
   DO i = ips, ipe
      IF (dbz(i,k,j) >= 20.) THEN
         cellcount(k) = cellcount(k)+1.
      END IF
   END DO
  END DO
 END DO

! ******************************
! make sure there is lightning
! ******************************

if (MAXVAL(cellcount(kps:kpe)) .eq. 0.) return

!  print *,'MAXVAL(cellcount)= ', MAXVAL(cellcount(kps:kpe))

! *****************************
! get temp_avg(k), pressure(k)
! *****************************

 temp_avg(kps:kpe) = 0.
 pressure(kps:kpe) = 0.
 count(kps:kpe) = 0.

 DO j = jps, jpe
  DO k = kps, kpe
   DO i = ips, ipe
      IF (t(i,k,j) > 0. .and. (p(i,k,j) > 0.)) THEN
         temp_avg(k) = temp_avg(k) + t(i,k,j)
         pressure(k) = pressure(k) + p(i,k,j)
         count(k) = count(k) + 1   
      END IF
   END DO
  END DO
 END DO

 DO k = kps, kpe
    IF (count(k) .gt. 0) THEN
       temp_avg(k) = temp_avg(k)/count(k)
       pressure(k) = pressure(k)/count(k)
    ELSE 
      temp_avg(k) = 0.
      pressure(k) = 0.
    END IF
 END DO

! ****************************************************************
! get k indexes of cloud top, cloud bottom, temp_upper, temp_lower
! ****************************************************************

 ksfc = kps

 ! *** finding k index for top of cloud ***

 k = kpe
 DO WHILE ((MAXVAL(dbz(ips:ipe,k,jps:jpe)) .lt. 20.) .and. (k .gt. kps))
    k = k-1
 END DO
 ktop = REAL(k)

 ! *** finding k index for bottom of cloud ***

 k=kps
 DO WHILE ((MAXVAL(dbz(ips:ipe,k,jps:jpe)) .lt. 20.) .and. (k .lt. kpe))
    k = k+1
 END DO
 kbot = REAL(k)

!++mcb  need to find indice of min temperature and then find k_upper
temp_min = temp_avg(kps)
k_tmin = kps
do k=kps+1,kpe
   if(temp_avg(k) .lt. temp_min) then
      temp_min = temp_avg(k)
      k_tmin = k
   endif
end do
 
 k = k_tmin
!--mcb
! k = kpe-1 
 DO WHILE (temp_avg(k) .lt. (temp_upper + 273.15))
    k = k - 1
 END DO   
 k_upper =REAL(k)

 DO WHILE (temp_avg(k) .lt. (temp_lower + 273.15))
    k = k - 1
 END DO
 k_lower = REAL(k)

! ******************************
! choice of cellcount_opt
! ******************************

 totalcells = 0.
     
 IF (cellcount_opt .eq. 2) THEN

    DO k = kps, kpe                                   
       totalcells = totalcells + cellcount(k)
    END DO

 ELSE IF (cellcount_opt .eq. 3) THEN

    DO k = kps, kpe                                   
       cellcount(k)=wrf_dm_sum_real(cellcount(k))
       totalcells = totalcells + cellcount(k)
    END DO

    totalcells = wrf_dm_sum_real(totalcells) 
    kbot = wrf_dm_min_real(kbot)
    ktop = wrf_dm_max_real(ktop)
    k_lower = wrf_dm_max_real(k_lower)
    k_upper = wrf_dm_max_real(k_upper)

 END IF

!  write(message,'(" global kbot,ktop,... ",4f9.2,f13.1)') kbot,ktop,k_lower,k_upper, totalcells
!  CALL WRF_MESSAGE (message)

! ******************************
! get term2
!
! used in calculating NO to be added to gridcells
! ******************************

 term2 = 8.3145E+06/(dx*dy*100*100)*1.e6

! *************************
! IC Lightning Calculation
! *************************

 IF ((flashrates_ic .gt. 0) .and. ((ktop .gt. k_lower) .and. (kbot .lt. k_lower))) THEN

    call bellcurve(1,nint(ktop),nint(k_lower),nint(kbot),ksfc,f_dist,z,ht,&
                   ims,ime,kms,kme,jms,jme,dz,ips,ipe,kps,kpe,jps,jpe)

!-- IF cloud is tall enough, create a double peaked vertical dist. for IC NOx

    IF (ktop .gt. k_upper) THEN

       call bellcurve(1,nint(ktop),nint(k_upper),nint(kbot),ksfc,f_dist_2,z,ht,&
                      ims,ime,kms,kme,jms,jme,dz,ips,ipe,kps,kpe,jps,jpe)
  
       f_dist = 0.5*(f_dist + f_dist_2)
    END IF

    B = N_IC/sum(f_dist(nint(kbot):nint(ktop))*pressure(nint(kbot):nint(ktop))) ! *** used in calculating NO

    k = nint(ktop)                !-- for loop for IC NO placement

    trltng_select:  SELECT CASE(tracer_opt)
     CASE (TRACER_TEST3)
       DO WHILE (k .ge. nint(kbot))
          IF (cellcount(k) .ne. 0.) THEN
             delta_mix_IC(k) = term2*B*f_dist(k)*temp_avg(k)*flashrates_ic/cellcount(k)/dz(k)        
             WHERE (dbz(ips:ipe,k,jps:jpe) .ge. 20.)
                    s_ic(ips:ipe,k,jps:jpe) =  s_ic(ips:ipe,k,jps:jpe) + delta_mix_IC(k)
             END WHERE
          END IF
          k = k - 1
       END DO   ! do while
     CASE DEFAULT
       call wrf_debug(15,'nothing done with lightning tracers here')
    END SELECT trltng_select

    k = nint(ktop)                !-- for loop for IC NO placement
    IF (passive_ltng .ne. 1) THEN
       DO WHILE (k .ge. nint(kbot))

          IF (cellcount(k) .ne. 0.) THEN
  
             delta_mix_IC(k) = term2*B*f_dist(k)*temp_avg(k)*flashrates_ic/cellcount(k)/dz(k)        

! save this print statement in case of needed diagnosis
!            if (k.eq.22) then
!               print*, 'IC ',k,avgz(k),term2, B, f_dist(k), temp_avg(k), flashrates_ic, cellcount(k), dz(k), delta_mix_IC(k)
!            end if

             WHERE (dbz(ips:ipe,k,jps:jpe) .ge. 20.)
                    c_no(ips:ipe,k,jps:jpe) =  c_no(ips:ipe,k,jps:jpe) + delta_mix_IC(k)
             END WHERE
          END IF
          k = k - 1
       END DO   ! do while
    END IF   ! not passive_ltng 

 END IF   ! IC lightning

! *************************
! CG Lightning Calculation
! *************************

 IF ((flashrates_cg .gt. 0) .and. ((ktop .gt. k_lower) .and. (kbot .lt. k_lower))) THEN

    call bellcurve(0,nint(ktop),nint(k_lower),nint(kbot),ksfc,f_dist,z,ht,&
                   ims,ime,kms,kme,jms,jme,dz,ips,ipe,kps,kpe,jps,jpe)

    B = N_CG/sum(f_dist(ksfc:nint(ktop))*pressure(ksfc:nint(ktop)))

    k = nint(ktop)
 
  tltng_select:  SELECT CASE(tracer_opt)
   CASE (TRACER_TEST3)
    DO WHILE (k .ge. ksfc)
       IF (cellcount(k) .ne. 0.) THEN
          delta_mix_CG(k) = term2*B*f_dist(k)*temp_avg(k)*flashrates_cg/cellcount(k)/dz(k)
            WHERE (dbz(ips:ipe,k,jps:jpe) .ge. 20.)
                   s_cg(ips:ipe,k,jps:jpe) =  s_cg(ips:ipe,k,jps:jpe) + delta_mix_CG(k)
            END WHERE
       END IF
       k = k-1
    END DO   ! do while
   CASE DEFAULT
     call wrf_debug(15,'nothing done with lightning tracers here')
  END SELECT tltng_select


    k = nint(ktop)
    IF (passive_ltng .ne. 1) THEN
       DO WHILE (k .ge. ksfc)

          IF (cellcount(k) .ne. 0.) THEN

             delta_mix_CG(k) = term2*B*f_dist(k)*temp_avg(k)*flashrates_cg/cellcount(k)/dz(k)

! save this print statement in case of needed diagnosis
!            if (k.eq.22) then
!               print*, 'CG ',k,z(ips,k,jps),term2, B, f_dist(k), temp_avg(k), flashrates_cg, cellcount(k), dz(k), delta_mix_CG(k)
!            end if

             WHERE (dbz(ips:ipe,k,jps:jpe) .ge. 20.)
                    c_no(ips:ipe,k,jps:jpe) =  c_no(ips:ipe,k,jps:jpe) + delta_mix_CG(k)
             END WHERE
          END IF
          k = k-1
       END DO   ! do while
    END IF   ! passive_ltng

 END IF   ! CG lightning

!print*,'ltng end   ',itimestep,MAXVAL(s_ic(ips:ipe,kps:kpe,jps:jpe)),SUM(s_ic(ips:ipe,kps:kpe,jps:jpe)), &
!                               MAXVAL(s_cg(ips:ipe,kps:kpe,jps:jpe)),SUM(s_cg(ips:ipe,kps:kpe,jps:jpe)), &
!                               MAXVAL(c_no(ips:ipe,kps:kpe,jps:jpe)),SUM(c_no(ips:ipe,kps:kpe,jps:jpe))

END SUBROUTINE ltng_decaria


!**********************************************************************

 SUBROUTINE bellcurve (ltflag,k_top,k_mu,k_bot,ksfc,f,z,ht,         &
                       ims,ime,kms,kme,jms,jme,dz,                  &
                       ips,ipe,kps,kpe,jps,jpe)

 INTEGER, INTENT(IN) :: ksfc,k_top, k_mu, k_bot,ltflag,             &
                        ims,ime,kms,kme,jms,jme,                    &
                        ips,ipe,kps,kpe,jps,jpe

 REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),                    &
         INTENT(IN   ) ::                                          z

 REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN)   ::   ht

 REAL, DIMENSION(kps:kpe), INTENT (OUT) :: f,dz
 INTEGER :: i,j,k
 REAL, DIMENSION (kps:kpe) :: ex,avgz,countz,sumz
 REAL :: sigma, mu,cum_f_dist
 REAL, PARAMETER :: piX2=6.2831854

!************************************************************************
!*** This subroutine calculates a normal distribution for vertical    ***
!*** placement of lightning NOx.                                      ***
!************************************************************************

 sumz(kps:kpe)=0
 countz(kps:kpe)=0

 f(kps:kpe) = 0.

 DO j = jps, jpe
  DO k = kps, kpe
   DO i = ips, ipe

    sumz(k) = sumz(k) + 100*(z(i,k,j) - ht(i,j))
    countz(k) = countz(k) + 1

   END DO
  END DO
 END DO

 avgz(kps:kpe) = sumz(kps:kpe)/countz(kps:kpe)

 mu = avgz(k_mu)

 IF (ltflag .eq. 1) THEN
  sigma = AMIN1(avgz(k_top)-mu,mu-avgz(k_bot))/3.0
 ELSE
  sigma = AMIN1(avgz(k_top)-mu,mu-avgz(ksfc))/3.0
 END IF

 ex(kps:kpe) = (avgz(kps:kpe)-mu)/sigma
 f = (1.0/(SQRT(piX2)*sigma))*exp(-ex*ex/2.0)

 dz(kps) = (avgz(kps+1) - avgz(kps))/2
 dz(kpe) = (avgz(kpe) - avgz(kpe-1))/2

 DO k = kps+1, kpe-1
  dz(k) = (avgz(k+1) + avgz(k))/2 - (avgz(k) + avgz(k-1))/2
 END DO

 cum_f_dist = 0
 
 IF (ltflag .eq. 1) THEN
  DO k = k_bot, k_top
   cum_f_dist = cum_f_dist + dz(k)*f(k)
  END DO
 ELSE
  DO k = ksfc, k_top
   cum_f_dist = cum_f_dist + dz(k)*f(k)
  END DO 
 END IF

 f(kps:kpe) = f(kps:kpe)/cum_f_dist
 
 END SUBROUTINE bellcurve

!***********************************************************************

 SUBROUTINE radar (t,p,rho,qr,qs,qg,dbz,                       &
                  ids,ide, jds,jde, kds,kde,                   &
                  ims,ime, jms,jme, kms,kme,                   &
                  ips,ipe, jps,jpe, kps,kpe                    )

 INTEGER, INTENT (IN) ::   ids,ide, jds,jde, kds,kde,          &
                           ims,ime, jms,jme, kms,kme,          &
                           ips,ipe, jps,jpe, kps,kpe

 REAL, DIMENSION( ims:ime , kms:kme , jms:jme ),               &
        INTENT(IN) ::                                     qr,  &
                                                          qs,  &
                                                          qg,  &
                                                          t,   &
                                                          p,   &
                                                          rho

 REAL, DIMENSION (ims:ime, kms:kme, jms:jme),                  &
        INTENT(INOUT) ::                                  dbz

 INTEGER :: i,j,k

 REAL :: xcnoh,xcnos,zsdryc,zswetc,zhdryc,zhwetc,dadr,dads,dadh,    &
      cnoh,cnos,cnor,cr1,hwdn,swdn,rwdn,hwdnsq,swdnsq,rwdnsq,       &
      dhmin,dielf,pie,qrmin,qsmin,qhmin,tfr,tfrh,zrc,               &
      tem,gmh,gms,gmr,zrain,zswet,zhwet,zsdry,zhdry,reflectmin,     &
      z_sum,reflect,dens

!************************************************************************
!*** Radar reflectivity scheme taken from wrf_user_fortran_util_0.f   ***
!*** from Lou Wicker.  This routine calculates radar reflectivity and ***
!*** the number of cells on each patch with reflectivity greater than ***
!*** or equal to 20 dbz. Top and bottom of cloud on each patch is     ***
!*** found based on 20 dbz contour, and the level of the upper and    ***
!*** lower mode isotherms of the vertical lightning distribution are  ***
!*** found for each patch.                                            ***
!************************************************************************

 cnoh = 4.0e+04
 cnor = 8.0e+06
 cnos = 3.0e+06
 cr1  = 7.2e+20
 hwdn = 660.0
 swdn = 100.0
 rwdn = 1000.0
 hwdnsq = hwdn*hwdn
 swdnsq = swdn*swdn
 rwdnsq = rwdn*rwdn
 dhmin = 0.005
 dielf = 0.21/0.93
 pie   = 4.0*atan(1.0)
 qrmin = 1.0e-06
 qsmin = 1.0e-07
 qhmin = 1.0e-06
 tfr   = 273.16
 tfrh  = tfr - 8.0
 zrc   = cr1*cnor

 DO j = jps, jpe
  DO k = kps, kpe
   DO i = ips, ipe

     IF (rho(i,k,j) .ge. 0.) THEN
       dens = rho(i,k,j)
     ELSE 
       dens = 0.
     END IF

     IF ((t(i,k,j) > 0.) .and. (dens > 0.)) THEN  

       IF (t(i,k,j) .lt. tfr) THEN
 
         xcnoh = cnoh*exp(-0.025*(t(i,k,j) - tfr))
         xcnos = cnos*exp(-0.038*(t(i,k,j) - tfr))
       ELSE
         xcnoh = cnoh*exp(-0.075*(t(i,k,j) - tfr))
         xcnos = cnos*exp(-0.088*(t(i,k,j) - tfr))
       END IF

       dadh = ( dens /(pie*hwdn*xcnoh) )**.25
       dads = ( dens /(pie*swdn*xcnos) )**.25
       dadr = ( dens /(pie*rwdn*cnor)  )**.25
       zhdryc = dielf*cr1*(hwdnsq/rwdnsq)*xcnoh
       zhwetc = cr1*(xcnoh)**.95
       zsdryc = dielf*cr1*swdnsq/rwdnsq*xcnos
       zswetc = cr1*xcnos

       zrain = 0.
       zswet = 0.
       zsdry = 0.
       zhwet = 0.
       zhdry = 0.
       gmr = 0.0
       gms = 0.0
       gmh = 0.0

       reflectmin = 0.001

       IF ( qr(i,k,j) .gt. qrmin ) THEN
         gmr = dadr*(qr(i,k,j))**.25
       END IF

       zrain = zrc*gmr**7

       IF ( qs(i,k,j) .gt. qsmin ) THEN
         gms = dads*(qs(i,k,j))**.25
       END IF

       IF ( t(i,k,j) .lt. tfr ) THEN
         zsdry = zsdryc*gms**7
       ELSE
         zswet = zswetc*gms**7
       END IF

       IF ( qg(i,k,j) .gt. qhmin ) THEN
         gmh = dadh*(qg(i,k,j))**.25
       END IF

       IF ( t(i,k,j) .gt. tfr ) THEN
         zhwet = (zhwetc*(gmh**7))**.95
       ELSE
         zhdry = zhdryc*gmh**7
       END IF

       z_sum =  zrain + zswet + zsdry + zhwet + zhdry

       IF ( z_sum .gt. 0. ) THEN
         reflect = 10.0 * alog10(z_sum)
         dbz(i,k,j) = max(reflect, reflectmin)
       ELSE
         dbz(i,k,j) = 0.
       END IF

     ELSE
       dbz(i,k,j) = 0.
     END IF

   END DO
  END DO
 END DO

 END SUBROUTINE radar

END MODULE module_ltng_crm

