!WRF:MODEL_LAYER:CHEMISTRY
!
! Contains subroutine for converting flash rates into NO emissions
! based on Decaria 2000 vertical distirbutions.
!
! Input: flashes (#/s)
! Output: tendency (ppmv/s)
!
! The output will be muliplied by timestep and used to incremeent NO
! concentration and the respective passive tracer in lightning_nox_driver.
!
! See module_lightning_nox_driver for more info.
!
! Contact: M. Barth <barthm@ucar.edu>, J. Wong <johnwong@ucar.edu>
!
!**********************************************************************
 MODULE module_lightning_nox_decaria

 IMPLICIT NONE

 CONTAINS

!**********************************************************************
!
! DeCaria et al, 2000
!
! DeCaria, A. J., K. E. Pickering, G. L. Stenchikov, and L. E. Ott (2005),
! Lightning-generated NOX and its impact on tropospheric ozone production:
! A three-dimensional modeling study of a Stratosphere-Troposphere Experiment:
! Radiation, Aerosols and Ozone (STERAO-A) thunderstorm, J. Geophys. Res.,
! 110, D14303, doi:10.1029/2004JD005556.
!
!**********************************************************************
 SUBROUTINE lightning_nox_decaria ( &
                          ! Frequently used prognostics
                            dx, dy, xland, ht, t, rho, z, p,      &
                            ic_flashrate, cg_flashrate,           & ! flashes (#/s)
                          ! Scheme specific prognostics
                            refl,                                 &
                          ! Namelist inputs
                            N_IC, N_CG,                           &
                            ltng_temp_upper,ltng_temp_lower,      &
                            cellcount_method,                     &
                          ! Order dependent args for domain, mem, and tile dims
                            ids, ide, jds, jde, kds, kde,         &
                            ims, ime, jms, jme, kms, kme,         &
                            ips, ipe, jps, jpe, kps, kpe,         &
                          ! outputs
                            lnox_ic_tend, lnox_cg_tend            & ! tendency (ppmv/s)
                          )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants
 USE module_wrf_error

 USE module_dm, only: wrf_dm_max_real, wrf_dm_min_real, wrf_dm_sum_real

! Lightning method
 USE module_lightning_driver, only: countCells

 IMPLICIT NONE
!-----------------------------------------------------------------

! Frequently used prognostics
 REAL,    INTENT(IN   )    ::       dx, dy

 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: xland, ht
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: t, rho, z, p
 REAL,    DIMENSION( ims:ime,          jms:jme ), INTENT(IN   ) :: ic_flashrate  , cg_flashrate ! #/sec


! Scheme specific prognostics
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN   ) :: refl

! Scheme specific namelist inputs
 REAL,    INTENT(IN   )    ::       N_IC, N_CG
 REAL,    INTENT(IN   )    ::       ltng_temp_lower, ltng_temp_upper
 INTEGER, INTENT(IN   )    ::       cellcount_method

! Order dependent args for domain, mem, and tile (patch) dims
 INTEGER, INTENT(IN   )    ::       ids,ide, jds,jde, kds,kde
 INTEGER, INTENT(IN   )    ::       ims,ime, jms,jme, kms,kme
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Mandatory outputs for all quantitative schemes
 REAL,    DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(  OUT) :: lnox_ic_tend,lnox_cg_tend

! Local variables
 INTEGER :: i,k,j
 INTEGER :: ktop,kbtm,kupper,klower
 REAL :: ic_fr, cg_fr, delta ! reconsolidated flashrates
 REAL :: reflmax, cellmax
 REAL :: term2, B, B_denom
 CHARACTER (LEN=250) :: message
 REAL, DIMENSION( kps:kpe ) :: cellcount
 REAL, DIMENSION( kps:kpe ) :: z_average, t_average, p_average, rho_average, conv
 REAL, DIMENSION( kps:kpe ) :: fd, fd2, dz ! fd = distribution

 REAL, PARAMETER :: refl_threshold = 20.

!-----------------------------------------------------------------

 lnox_ic_tend (ips:ipe,kps:kpe,jps:jpe ) = 0.
 lnox_cg_tend (ips:ipe,kps:kpe,jps:jpe ) = 0.

! Determine cloud extents. Also calculated in physics but cellcount is not persistent
 CALL countCells( &
      ! Inputs
        refl, refl_threshold, cellcount_method,     &
      ! Order dependent args for domain, mem, and tile dims
        ids, ide, jds, jde, kds, kde,              &
        ims, ime, jms, jme, kms, kme,              &
        ips, ipe, jps, jpe, kps, kpe,              &
      ! Output
        cellcount )

! Reconsolidate flash counts
 ic_fr = sum(ic_flashrate(ips:ipe,jps:jpe))
 cg_fr = sum(cg_flashrate(ips:ipe,jps:jpe))
 if ( cellcount_method .eq. 2 ) then
    ic_fr = wrf_dm_sum_real(ic_fr)
    cg_fr = wrf_dm_sum_real(cg_fr)
 ENDIF
 reflmax = maxval(refl(ips:ipe,kps:kpe,jps:jpe))
 cellmax = maxval(cellcount(kps:kpe))
 WRITE(message, * ) ' LNOx tracer: max_refl, max_cellcount, ic_fr = ',  reflmax, cellmax, ic_fr
 CALL wrf_debug ( 100, message )

!-----------------------------------------------------------------

! Average z, t, p, rho
 CALL horizontalAverage( z( ips:ipe,kps:kpe,jps:jpe ), ips, ipe, kps, kpe, jps, jpe, z_average )
 CALL horizontalAverage( t( ips:ipe,kps:kpe,jps:jpe ), ips, ipe, kps, kpe, jps, jpe, t_average )
 CALL horizontalAverage( p( ips:ipe,kps:kpe,jps:jpe ), ips, ipe, kps, kpe, jps, jpe, p_average )
 CALL horizontalAverage( rho( ips:ipe,kps:kpe,jps:jpe ), ips, ipe, kps, kpe, jps, jpe, rho_average )

! molesofair(kps:kpe) = rho_average(kps:kpe) * 1E3 * dx * dy / .02897 ! # moles per km in z
! term2 = 30 * 8.3145E6/dx/dy/28.96/100./100.

 conv(kps:kpe) = 8.314 *t_average(kps:kpe) / (dx * dy)                !  conversion term with units J/(mol-m2)


 CALL  kfind ( cellcount, t_average,            &
               ltng_temp_upper,ltng_temp_lower, cellcount_method, &
                ips, ipe, jps, jpe, kps, kpe,              &
              ! Outputs
                ktop,kbtm,kupper,klower )

! Calculates IC distribution
!IF (( ic_fr .gt. 0 ) .and. (( ktop .gt. klower ) .and. (kbtm .lt. ktop) ) )THEN
 IF (( ic_fr > 0 ) .and. (( ktop > klower ) .and. (kbtm < klower) ) )THEN
   call bellcurve(kbtm,ktop,klower,z_average, kps,kpe, fd, dz)
   if (ktop .gt. kupper) then
     call bellcurve(kbtm,ktop,kupper,z_average, kps,kpe, fd2, dz)
     fd(kbtm:ktop) = 0.5*( fd(kbtm:ktop) + fd2(kbtm:ktop) )         ! unitless
   endif
!   B = N_IC/sum(f(kbtm:ktop)*p_average(kbtm:ktop))             ! *** used in calculating NO
   B_denom = DOT_PRODUCT( fd(kbtm:ktop),p_average(kbtm:ktop) )      ! N/m2


   DO k=kbtm,ktop
     if ( cellcount(k) .gt. 0. ) THEN
!       delta = term2*B*fd(k)*t_average(k)*ic_fr/cellcount(k)/dz(k)/100.
        !*  implementation note: 1) ic_fr * N_IC/cellcount gives moles of NO in the column
        !*                       2) Multiplying by fd gives the # moles per level
        !*                       3) Convert to mol NO/mol air per minute
        !*                       4) Multiply by 1E6 gives ppmv

        delta = (ic_fr * N_IC / cellcount(k)) * fd(k) / B_denom * conv(k)/dz(k) * 1E6
!units:      flash/sec * mol/flash /()       * m2/ N      * J/(mol-m2) / m * ppmv/(mol NO/mol air)
!units:      flash/sec * mol/flash           * m2/ N      * N-m/(mol-m3) * ppmv/(mol NO/mol air) 
!units:      ppmv/sec 

        where(refl(ips:ipe,k,jps:jpe) .gt. refl_threshold )
          lnox_ic_tend(ips:ipe,k,jps:jpe) = delta
        endwhere
     ENDIF
   ENDDO

 ENDIF ! IC lightning

!-----------------------------------------------------------------
! Calculates CG distribution
!IF ((cg_fr .gt. 0 ) .and. (( ktop .gt. klower ) .and. (kbtm .lt. ktop) ) ) THEN
 IF ((cg_fr > 0 ) .and. (( ktop > klower ) .and. (kbtm < klower) ) ) THEN
   call bellcurve(kps,ktop,klower,z_average, kps,kpe, fd, dz)
!   B = N_CG/(sum(fd(kps:ktop)*p_average(kps:ktop)))

   B_denom = DOT_PRODUCT( fd(kbtm:ktop),p_average(kbtm:ktop) )      ! N/m2

   k = ktop

   DO WHILE (k .ge. kps)
     IF (cellcount(k) .gt. 0) THEN

!      delta = (cg_fr * N_CG / cellcount(k)) * fd(k) / (molesofair(k)*dz(k)) * 1E6
      delta = (cg_fr * N_CG / cellcount(k)) * fd(k) / B_denom * conv(k)/dz(k) * 1E6
!units:      flash/sec * mol/flash /()       * m2/ N      * J/(mol-m2) / m * ppmv/(mol NO/mol air) 
!units:      flash/sec * mol/flash           * m2/ N      * N-m/(mol-m3) * ppmv/(mol NO/mol air) 
!units:      ppmv/sec 

       where( refl(ips:ipe,k,jps:jpe) .gt. refl_threshold )
          lnox_cg_tend(ips:ipe,k,jps:jpe) = delta
       endwhere

     ENDIF

     k = k - 1      !07/23/14 KAC added

   ENDDO

 ENDIF

 END SUBROUTINE lightning_nox_decaria



!************************************************************************
! This subroutine prepares a normal distribution between k_min and
! k_max centered at k_mu. Distribution for each level is
! normalized to \int^{z_at_w(k_max}_{z_at_w(k_min-1)}f(z)dz
!
! Unit of f is fraction of total column
!
! Modified from v3.4.1 module_ltng_crm.F, kept the math but changed
! the implementation for better clarity. Removed patch-wide averaging
! of z.
!************************************************************************

 SUBROUTINE bellcurve ( k_min, k_max, k_mu, z, kps,kpe, f, dz )
!-----------------------------------------------------------------

 IMPLICIT NONE
 INTEGER,                      INTENT(IN   ) :: k_min, k_max, k_mu
 REAL,   DIMENSION( kps:kpe ), INTENT(IN   ) :: z       ! at phy
 INTEGER,                      INTENT(IN   ) :: kps,kpe

 REAL,   DIMENSION( kps:kpe ), INTENT(  OUT) :: f, dz

 INTEGER :: i,j,k
 REAL, DIMENSION( kps:kpe ) :: ex
 REAL :: sigma, z_mu, cuml_f_dist
 REAL, PARAMETER :: two_pi = 6.2831854

!-----------------------------------------------------------------

 f(kps:kpe) = 0.
 z_mu = z(k_mu)
 sigma = AMIN1(z(k_max)-z_mu,z_mu-z(k_min))/3.0

 ! distance from mean
 ex(k_min:k_max) = (z(k_min:k_max)-z_mu)/sigma
 
 ! Truncated Gaussian at 3 sigma
 f(k_min:k_max) = (1.0/(sqrt(two_pi)*sigma))*exp(-ex(k_min:k_max)*ex(k_min:k_max)/2.0)

!++mcb   We do need dz at bottom and top of domain
!   dz(kps) = 0. ! safe as long as k_min != kps
!   dz(kpe) = 0. ! safe as long as k_max != kpe
 dz(kps) = (z(kps+1) - z(kps))*.5             
 dz(kpe) = (z(kpe) - z(kpe-1))*.5             
 DO k=kps+1,kpe-1
!  dz(k) = (z(k+1)+z(k))/2. - (z(k)+z(k-1))/2.
   dz(k) = (z(k+1) - z(k-1))*.5
 ENDDO

 ! Normalize
 cuml_f_dist = DOT_PRODUCT(dz(k_min:k_max),f(k_min:k_max))
 f(k_min:k_max) = f(k_min:k_max)*dz(k_min:k_max)/cuml_f_dist

 END SUBROUTINE bellcurve

!************************************************************************
! This subroutine finds the vertical indices (phy grid) of the follow
! within a column:
! 1) ktop - reflectivity cloud top
! 2) kbtm - reflectivity cloud bottom
! 3) kupper - upper mode isotherm
! 3) klower - lower mode isotherm
!************************************************************************
 SUBROUTINE kfind ( &
              ! Prognostics
                cellcount, t,                         &
              ! Namelist settings
                ltng_temp_upper,ltng_temp_lower,      &
                cellcount_method,                     &
              ! Order dependent args for domain, mem, and tile dims
                ips, ipe, jps, jpe, kps, kpe,          &
              ! Outputs
                ktop,kbtm,kupper,klower               &
            )
!-----------------------------------------------------------------
! Framework
 USE module_state_description

! Model layer
 USE module_model_constants

 USE module_dm, only: wrf_dm_max_real, wrf_dm_min_real, wrf_dm_sum_real

 IMPLICIT NONE
!-----------------------------------------------------------------

! Prognostics
 REAL, DIMENSION( kps:kpe ), INTENT(IN   ) :: cellcount
 REAL, DIMENSION( kps:kpe ), INTENT(IN   ) :: t

! Namelist settings
 REAL,    INTENT(IN   )    ::       ltng_temp_lower, ltng_temp_upper
 INTEGER, INTENT(IN   )    ::       cellcount_method

! Order dependent args for domain, mem, and tile (patch) dims
 INTEGER, INTENT(IN   )    ::       ips,ipe, jps,jpe, kps,kpe

! Outputs
 INTEGER, INTENT(  OUT)    ::       ktop,kbtm,kupper,klower

! Local vars
 CHARACTER (LEN=250) :: message
 REAL    :: ktop_r, kbtm_r, kupper_r, klower_r
 INTEGER :: k

!-----------------------------------------------------------------
 ktop = kps
 kbtm = kps
 kupper = kps
 klower = kps

 ! look for ktop
 k = kpe
 DO WHILE ( cellcount(k) .eq. 0 .and. k .gt. kps)
  k = k-1
 ENDDO
 ktop = k

 ! Look for kbtm
 k = kps
 DO WHILE( cellcount(k) .eq. 0 .and. k .le. ktop )
  k = k+1
 ENDDO
 kbtm = k
! if (kbtm .eq. kps) kbtm = kpe

 ! Look for kupper
 k = kps
 DO WHILE ( t(k) .gt. ltng_temp_lower + 273.15 .and. k .lt. kpe )
   k = k + 1
 ENDDO
 klower = k

 DO WHILE ( t(k) .gt. ltng_temp_upper + 273.15 .and. k .lt. kpe )
   k = k + 1
 ENDDO
 kupper = k

 WRITE(message, * ) ' LNOx_driver: kbtm, ktop, klower, kupper = ', kbtm, ktop, klower, kupper
 CALL wrf_debug ( 100, message )
 
 IF ( cellcount_method .eq. 2 ) THEN
   kbtm_r = real(kbtm)
   ktop_r = real(ktop)
   klower_r = real(klower)
   kupper_r = real(kupper)
   kbtm = nint(wrf_dm_min_real(kbtm_r))
   ktop = nint(wrf_dm_max_real(ktop_r))
   klower = nint(wrf_dm_max_real(klower_r))
   kupper = nint(wrf_dm_max_real(kupper_r))

   WRITE(message, * ) ' lightning_driver: kbtm, ktop, klower, kupper = ', kbtm, ktop, klower, kupper
   CALL wrf_debug ( 100, message )
 endif

 END SUBROUTINE kfind

!************************************************************************
! This function averages out a 3D array into 1D in the 2nd dimension
!************************************************************************
 SUBROUTINE horizontalAverage( array3D, ips, ipe, kps, kpe, jps, jpe, array1D )
!-----------------------------------------------------------------
 IMPLICIT NONE
 REAL, DIMENSION(ips:ipe,kps:kpe,jps:jpe), INTENT(IN) :: array3D
 INTEGER, INTENT(IN) :: ips,ipe,kps,kpe,jps,jpe

 INTEGER :: k
 REAL, DIMENSION(kps:kpe), INTENT(OUT) :: array1D
!-----------------------------------------------------------------
 DO k=kps,kpe
   array1D(k) = sum(array3D(ips:ipe,k,jps:jpe))/((ipe-ips+1)*(jpe-jps+1))
 ENDDO
    
 END SUBROUTINE horizontalAverage

 END MODULE module_lightning_nox_decaria
