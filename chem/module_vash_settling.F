MODULE MODULE_VASH_SETTLING


! 07/10/24 - A. Ukhov, bug fix: ash mass balance was
! violated in the "settling" routine. I.e. prev. discrtetisation scheme 
! did not conserve the mass of ash. Code clean-up. 

CONTAINS

SUBROUTINE vash_settling_driver(dt,config_flags,t_phy,moist,               &
         chem,rho_phy,dz8w,p8w,p_phy,                                      &
         ash_fall,dx,g,                                                    &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  
  USE module_configure
  USE module_state_description
  USE module_model_constants, ONLY: mwdry
  IMPLICIT NONE
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) ::                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
    REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::  t_phy,p_phy,dz8w,p8w,rho_phy
   REAL,  DIMENSION( ims:ime , jms:jme ),                        &
          INTENT(INOUT   ) ::  ash_fall

  REAL, INTENT(IN   ) :: dt,dx,g
  integer :: nmx,i,j,k,kk,lmx
  real*8, DIMENSION (1,1,kte-kts+1) :: tmp,airden,airmas,p_mid,delz,rh
  real*8, DIMENSION (1,1,kte-kts+1,4) :: sea_salt
!srf
  real*8, DIMENSION (1,1,kte-kts+1,10) :: ash
  real*8, DIMENSION (10), PARAMETER :: den_ash(10)=(/2500.,2500.,2500.,2500.,2500., &
                                                     2500.,2500.,2500.,2500.,2500. /)
  real*8, DIMENSION (10), PARAMETER :: reff_ash(10)=(/0.5000D-3,&! 1.00 mm diameter 
                                                      0.3750D-3,&! 0.75 mm
						      0.1875D-3,&!
						      93.750D-6,&!
						      46.875D-6,&!
						      23.437D-6,&!
						      11.719D-6,&!
						      05.859D-6,&!
						      02.930D-6,&!
						      00.975D-6 /)! 3.9 um
  real*8, DIMENSION (10) :: bstl_ash
!
! bstl is for budgets
!

       lmx=kte-kts+1
       
       do j=jts,jte
       do i=its,ite
        kk=0
	        
        bstl_ash(:)=0.
	      ash(:,:,:,:)=0.
    
          do k=kts,kte
            kk=kk+1
            p_mid(1,1,kk)=.01*p_phy(i,kte-k+kts,j)
            delz(1,1,kk)=dz8w(i,kte-k+kts,j)
            airmas(1,1,kk)=-(p8w(i,k+1,j)-p8w(i,k,j))/g
            airden(1,1,kk)=rho_phy(i,k,j)
            tmp(1,1,kk)=t_phy(i,k,j)
            rh(1,1,kk) = .95
            rh(1,1,kk) = MIN( .95, moist(i,k,j,p_qv) / &
                (3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
                (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))))
            rh(1,1,kk)=max(1.0D-1,rh(1,1,kk))
          enddo

          kk=0
          do k=kts,kte
            kk=kk+1
            ash(1,1,kk,7)=chem(i,k,j,p_vash_7)
            ash(1,1,kk,8)=chem(i,k,j,p_vash_8)
            ash(1,1,kk,9)=chem(i,k,j,p_vash_9)
            ash(1,1,kk,10)=chem(i,k,j,p_vash_10)
          enddo
          
          if(config_flags%chem_opt == 400  .or. config_flags%chem_opt == 402 ) then
          kk=0
            do k=kts,kte
              kk=kk+1
              ash(1,1,kk,1)=chem(i,k,j,p_vash_1)
              ash(1,1,kk,2)=chem(i,k,j,p_vash_2)
              ash(1,1,kk,3)=chem(i,k,j,p_vash_3)
              ash(1,1,kk,4)=chem(i,k,j,p_vash_4)
              ash(1,1,kk,5)=chem(i,k,j,p_vash_5)
              ash(1,1,kk,6)=chem(i,k,j,p_vash_6)
            enddo
          endif

          call vsettling(1, 1, lmx, 10, g, ash, tmp, p_mid, delz, airmas, &
                    den_ash, reff_ash, dt, bstl_ash, rh, airden)

          !bstl_ash [ug/m2]. total ash grav. set [kg/m2]
          ash_fall(i,j) = ash_fall(i,j) + sum(1.e-9 * bstl_ash(1:10))      

          kk=0
          do k=kts,kte
            kk=kk+1
            chem(i,k,j,p_vash_7)=ash(1,1,kk,7)
            chem(i,k,j,p_vash_8)=ash(1,1,kk,8)
            chem(i,k,j,p_vash_9)=ash(1,1,kk,9)
            chem(i,k,j,p_vash_10)=ash(1,1,kk,10)
          enddo
          
          if(config_flags%chem_opt == 400  .or. config_flags%chem_opt == 402 ) then
            kk=0
            do k=kts,kte
              kk=kk+1
              chem(i,k,j,p_vash_1)=ash(1,1,kk,1)
              chem(i,k,j,p_vash_2)=ash(1,1,kk,2)
              chem(i,k,j,p_vash_3)=ash(1,1,kk,3)
              chem(i,k,j,p_vash_4)=ash(1,1,kk,4)
              chem(i,k,j,p_vash_5)=ash(1,1,kk,5)
              chem(i,k,j,p_vash_6)=ash(1,1,kk,6)
            enddo
          endif

!ash settling end

       enddo
       enddo
END SUBROUTINE vash_settling_driver


subroutine vsettling(imx,jmx, lmx, nmx, g0, &
         tc, tmp, p_mid, delz, airmas, &
         den, reff, dt, bstl, rh,airden)
! ****************************************************************************
! *                                                                          *
! *  Calculate the loss by settling, using an implicit method                *
! *                                                                          *
! *  Input variables:                                                        *
! *    SIGE(k)         - sigma coordinate of the vertical edges              *
! *    PS(i,j)         - Surface pressure (mb)                               *
! *    TMP(i,j,k)      - Air temperature  (K)                                *
! *    CT(i,j)         - Surface exchange coeff for moisture
! *                                                                          *
! **************************************************************************** 


  IMPLICIT  NONE

  INTEGER, INTENT(IN) :: imx, jmx, lmx, nmx
  INTEGER :: ntdt
  REAL, INTENT(IN) :: dt,g0
  REAL*8,    INTENT(IN) :: tmp(imx,jmx,lmx), delz(imx,jmx,lmx),  &
                         airmas(imx,jmx,lmx), rh(imx,jmx,lmx), &
                         den(nmx), reff(nmx), p_mid(imx,jmx,lmx),airden(imx,jmx,lmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(OUT)   :: bstl(imx,jmx,nmx)

  REAL*8    :: dt_settl(nmx), rho(nmx)
  INTEGER :: ndt_settl(nmx)
  REAL*8    :: vd_cor(lmx)
  REAL*8    :: dzmin, vsettl, dtmax, rwet(nmx)
  REAL*8    :: addmass,c_stokes, free_path, c_cun, viscosity
  REAL,    PARAMETER :: dyn_visc = 1.5E-5
  INTEGER :: k, n, i, j, l, l2

  ! for OMP:
  REAL*8 :: rwet_priv(nmx), rho_priv(nmx)

  ! executable statements

  WHERE (tc(:,:,:,:) < 0.0) tc(:,:,:,:) = 1.0d-32

  dzmin = MINVAL(delz(:,:,:))
  
  DO k = 1,nmx ! number of bins

     ! Settling velocity (m/s) for each tracer (Stokes Law)
     ! DEN         density                        (kg/m3)
     ! REFF        effective radius               (m)
     ! dyn_visc    dynamic viscosity              (kg/m/s)
     ! g0          gravity                        (m/s2)

     vsettl = 4.0/9.0 * g0 * den(k) * reff(k)**2 / dyn_visc

     ! Determine the maximum time-step satisying the CFL condition:
     ! dt <= (dz)_min / v_settl
     ntdt = INT(dt)
     dtmax = dzmin / vsettl
     ndt_settl(k) = MAX( 1, INT( ntdt /dtmax) )
     ! limit maximum number of iterations
     IF (ndt_settl(k) > 12) ndt_settl(k) = 12
     dt_settl(k) = REAL(ntdt) / REAL(ndt_settl(k))

     ! Particles radius in centimeters
      rwet(k) = reff(k)
      rho(k) = den(k)
  END DO

!$OMP PARALLEL DO &
!$OMP DEFAULT( SHARED ) &
!$OMP PRIVATE( i,   j,   l,   l2, n,   k, rwet_priv, c_stokes)&
!$OMP PRIVATE( free_path, c_cun, viscosity, rho_priv, vd_cor )

  
  DO j = 1,jmx                     ! lat loop
  DO i = 1,imx                     ! lon loop
     DO k = 1,nmx                   ! bin loop
        rwet_priv(k) = rwet(k)
        rho_priv(k)  = rho(k)

        bstl(i,j,k)=0.

        DO n = 1,ndt_settl(k)
           DO l = lmx,1,-1          ! height loop, from top
              l2 = lmx - l + 1

              ! Dynamic viscosity
              c_stokes = 1.458E-6 * tmp(i,j,l)**1.5/(tmp(i,j,l) + 110.4) 

              ! Mean free path as a function of pressure (mb) and 
              ! temperature (K)
              ! order of p_mid is top->sfc
              free_path = 1.1E-3/p_mid(i,j,l2)/SQRT(tmp(i,j,l))

              ! Slip Correction Factor
              c_cun = 1.0+ free_path/rwet_priv(k)*(1.257 + 0.4*EXP(-1.1*rwet_priv(k)/free_path))

              ! Corrected dynamic viscosity (kg/m/s)
              viscosity = c_stokes / c_cun

              ! Settling velocity, depends on temp
              vd_cor(l) = 2.0/9.0*g0*rho_priv(k)*rwet_priv(k)**2/viscosity

              ! Update mixing ratio. Order of delz is top->sfc
              IF (l == lmx) THEN
                tc(i,j,l,k) = tc(i,j,l,k) / (1.0 + dt_settl(k)*vd_cor(l)/delz(i,j,l2))
              ELSE
                if (l==1) then
                  bstl(i,j,k) = bstl(i,j,k) + (tc(i,j,l,k) * dt_settl(k) * vd_cor(l)/delz(i,j,l2)) * &
                                               airden(i,j,l) * delz(i,j,l2)  !ug/m2
                endif
                tc(i,j,l,k) = 1.0/(1.0+dt_settl(k) * vd_cor(l)/delz(i,j,l2)) * &
                      (tc(i,j,l,k) + tc(i,j,l+1,k) * dt_settl(k) * vd_cor(l+1) /delz(i,j,l2-1) * &
                      ((airden(i,j,l+1)*delz(i,j,l2-1))/(airden(i,j,l)*delz(i,j,l2))))
              END IF

           END DO  !l, height
        END DO  !n, time
     END DO  !k, bin
 
  END DO   !i
  END DO   !j
!$OMP END PARALLEL DO

END SUBROUTINE vsettling

END MODULE MODULE_VASH_SETTLING
