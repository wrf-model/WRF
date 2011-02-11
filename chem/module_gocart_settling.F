MODULE MODULE_GOCART_SETTLING

CONTAINS

SUBROUTINE gocart_settling_driver(dt,config_flags,t_phy,moist,  &
         chem,rho_phy,dz8w,p8w,p_phy,         &
         dustin,seasin,dx,g, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  USE module_configure
  USE module_state_description
  USE module_data_gocart_dust
  USE module_data_gocart_seas
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
   REAL,  DIMENSION( ims:ime , jms:jme, 5 ),                        &
          INTENT(IN   ) ::  dustin,seasin

  REAL, INTENT(IN   ) :: dt,dx,g
  integer :: kkk,nmx,i,j,k,kk,lmx,iseas,idust
  real*8, DIMENSION (1,1,kte-kts+1) :: tmp,airden,airmas,p_mid,delz,rh
  real*8, DIMENSION (1,1,kte-kts+1,5) :: ddust
  real*8, DIMENSION (1,1,kte-kts+1,4) :: sea_salt
!
! bstl is for budgets
!
! real*8, DIMENSION (5), PARAMETER :: den_dust(5)=(/2500.,2650.,2650.,2650.,2650./)
! real*8, DIMENSION (5), PARAMETER :: reff_dust(5)=(/0.73D-6,1.4D-6,2.4D-6,4.5D-6,8.0D-6/)
! real*8, DIMENSION (4), PARAMETER :: den_seas(4)=(/2200.,2200.,2200.,2290./)
! real*8, DIMENSION (4), PARAMETER :: reff_seas(4)=(/0.30D-6,1.00D-6,3.25D-6,7.50D-6/)
  real*8, DIMENSION (5) :: bstl_dust
  real*8, DIMENSION (4) :: bstl_seas
  real*8 conver,converi
       conver=1.e-9
       converi=1.e9
       lmx=kte-kts+1
       ddust(:,:,:,:)=0.
       sea_salt(:,:,:,:)=0.
       do j=jts,jte
       do i=its,ite
          kk=0
          bstl_dust(:)=0.
          bstl_seas(:)=0.
          if(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11) then
           do kkk=1,5
              ddust(1,1,kts,kkk)=dustin(i,j,kkk)*conver
           enddo
          else
             do k=kts,kte
                kk=kk+1
                ddust(1,1,kk,1)=chem(i,k,j,p_dust_1)*conver
                ddust(1,1,kk,2)=chem(i,k,j,p_dust_2)*conver
                ddust(1,1,kk,3)=chem(i,k,j,p_dust_3)*conver
                ddust(1,1,kk,4)=chem(i,k,j,p_dust_4)*conver
                ddust(1,1,kk,5)=chem(i,k,j,p_dust_5)*conver
             enddo
          endif
          kk=0
          do k=kts,kte
          kk=kk+1
          p_mid(1,1,kk)=.01*p_phy(i,kte-k+kts,j)
          delz(1,1,kk)=dz8w(i,kte-k+kts,j)
          airmas(1,1,kk)=-(p8w(i,k+1,j)-p8w(i,k,j))*dx*dx/g
          airden(1,1,kk)=rho_phy(i,k,j)
          tmp(1,1,kk)=t_phy(i,k,j)
          rh(1,1,kk) = .95
          rh(1,1,kk) = MIN( .95, moist(i,k,j,p_qv) / &
               (3.80*exp(17.27*(t_phy(i,k,j)-273.)/ &
               (t_phy(i,k,j)-36.))/(.01*p_phy(i,k,j))))
          rh(1,1,kk)=max(1.0D-1,rh(1,1,kk))
          enddo
!         tmp(1,1,1)=244.3541
!         tmp(1,1,1)=246.72290
!         tmp(1,1,3)=245.79040
!den=   2650.00000000000
!reff=  8.000000000000000E-006

          iseas=0
          idust=1

          call settling(1, 1, lmx, 5,g,dyn_visc, &
                    ddust, tmp, p_mid, delz, airmas, &
                    den_dust, reff_dust, dt, bstl_dust, rh, idust, iseas)
          if(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11 ) then
            do kkk=1,4
              sea_salt(1,1,kts,kkk)=seasin(i,j,kkk)*conver
            enddo
            kk=1
            do kkk=1,5
            if(ddust(1,1,kk,kkk) .ge. dustin(i,j,kkk))ddust(1,1,kk,kkk)=dustin(i,j,kkk)
            enddo
            chem(i,kts,j,p_p25i)=chem(i,kts,j,p_p25i) &
                         +.25*(ddust(1,1,kk,1)+.286*ddust(1,1,kk,2))*converi
            chem(i,kts,j,p_p25i)=max(chem(i,kts,j,p_p25i),1.e-16)
            chem(i,kts,j,p_p25j)=chem(i,kts,j,p_p25j) &
                         +.75*(ddust(1,1,kk,1)+.286*ddust(1,1,kk,2))*converi
            chem(i,kts,j,p_p25j)=max(chem(i,kts,j,p_p25j),1.e-16)
            chem(i,kts,j,p_soila)=chem(i,kts,j,p_soila) &
                         +(.714*ddust(1,1,kk,2)+ddust(1,1,kk,3))*converi
            chem(i,kts,j,p_soila)=max(chem(i,kts,j,p_soila),1.e-16)
          else
             kk=0
             do k=kts,kte
                kk=kk+1
                chem(i,k,j,p_dust_1)=ddust(1,1,kk,1)*converi
                chem(i,k,j,p_dust_2)=ddust(1,1,kk,2)*converi
                chem(i,k,j,p_dust_3)=ddust(1,1,kk,3)*converi
                chem(i,k,j,p_dust_4)=ddust(1,1,kk,4)*converi
                chem(i,k,j,p_dust_5)=ddust(1,1,kk,5)*converi
                sea_salt(1,1,kk,1)=chem(i,k,j,p_seas_1)*conver
                sea_salt(1,1,kk,2)=chem(i,k,j,p_seas_2)*conver
                sea_salt(1,1,kk,3)=chem(i,k,j,p_seas_3)*conver
                sea_salt(1,1,kk,4)=chem(i,k,j,p_seas_4)*conver
             enddo
          endif
!         write(0,*)i,j,bstl_dust(3),bstl_dust(4),chem(i,1,j,p_dust_4)
          iseas=1
          idust=0
          call settling(1, 1, lmx, 4, g,dyn_visc,&
                    sea_salt, tmp, p_mid, delz, airmas, &
                    den_seas, reff_seas, dt, bstl_seas, rh, idust, iseas)
          if(config_flags%chem_opt == 2 .or. config_flags%chem_opt == 11 ) then
            kk=1
            do kkk=1,4
              if(sea_salt(1,1,kk,kkk) .ge. seasin(i,j,kkk))sea_salt(1,1,kk,kkk)=seasin(i,j,kkk)
            enddo
            chem(i,kts,j,p_naai)=chem(i,kts,j,p_naai) &
                         +.25*(sea_salt(1,1,kk,1)+.942*sea_salt(1,1,kk,2))*converi
            chem(i,kts,j,p_naai)=max(1.e-16,chem(i,kts,j,p_naai))
            chem(i,kts,j,p_naaj)=chem(i,kts,j,p_naaj) &
                         +.75*(sea_salt(1,1,kk,1)+.942*sea_salt(1,1,kk,2))*converi
            chem(i,kts,j,p_naaj)=max(1.e-16,chem(i,kts,j,p_naaj))
            chem(i,kts,j,p_seas)=chem(i,kts,j,p_seas) &
                         +(.058*sea_salt(1,1,kk,2)+sea_salt(1,1,kk,3))*converi
            chem(i,kts,j,p_seas)=max(1.e-16,chem(i,kts,j,p_seas))
          else
             kk=0
             do k=kts,kte
                kk=kk+1
                chem(i,k,j,p_seas_1)=sea_salt(1,1,kk,1)*converi
                chem(i,k,j,p_seas_2)=sea_salt(1,1,kk,2)*converi
                chem(i,k,j,p_seas_3)=sea_salt(1,1,kk,3)*converi
                chem(i,k,j,p_seas_4)=sea_salt(1,1,kk,4)*converi
          enddo
          endif
       enddo
       enddo
END SUBROUTINE gocart_settling_driver


          subroutine settling(imx,jmx, lmx, nmx,g0,dyn_visc, &
                    tc, tmp, p_mid, delz, airmas, &
                    den, reff, dt, bstl, rh, idust, iseas)
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

  INTEGER, INTENT(IN) :: imx, jmx, lmx, nmx,iseas,idust
  INTEGER :: ntdt
  REAL, INTENT(IN) :: dt,g0,dyn_visc
  REAL*8,    INTENT(IN) :: tmp(imx,jmx,lmx), delz(imx,jmx,lmx),  &
                         airmas(imx,jmx,lmx), rh(imx,jmx,lmx), &
                         den(nmx), reff(nmx), p_mid(imx,jmx,lmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(OUT)   :: bstl(imx,jmx,nmx)

  REAL*8    :: tc1(imx,jmx,lmx,nmx), dt_settl(nmx), rcm(nmx), rho(nmx)
  INTEGER :: ndt_settl(nmx)
  REAL*8    :: dzmin, vsettl, dtmax, pres, rhb, rwet(nmx), ratio_r(nmx)
  REAL*8    :: c_stokes, free_path, c_cun, viscosity, vd_cor, growth_fac
  INTEGER :: k, n, i, j, l, l2
  ! for sea-salt:
  REAL*8, PARAMETER :: c1=0.7674, c2=3.079, c3=2.573E-11, c4=-1.424 

  ! for OMP:
  REAL*8 :: rwet_priv(nmx), rho_priv(nmx)

  ! executable statements

! IF (type) /= 'dust' .AND. TRIM(aero_type) /= 'sea_salt') RETURN
  if(idust.ne.1.and.iseas.ne.1)return

!!!  WHERE (tc(:,:,:,:) < 0.0) tc(:,:,:,:) = 1.0E-32

  dzmin = MINVAL(delz(:,:,:))
  IF (idust == 1)     growth_fac = 1.0
  IF (iseas == 1)     growth_fac = 3.0

  DO k = 1,nmx

     ! Settling velocity (m/s) for each tracer (Stokes Law)
     ! DEN         density                        (kg/m3)
     ! REFF        effective radius               (m)
     ! dyn_visc    dynamic viscosity              (kg/m/s)
     ! g0          gravity                        (m/s2)
     ! 3.0         corresponds to a growth of a factor 3 of radius with 100% RH
     ! 0.5         upper limit with temp correction

     tc1(:,:,:,k) = tc(:,:,:,k)
     vsettl = 2.0/9.0 * g0 * den(k) * (growth_fac*reff(k))**2 / &
              (0.5*dyn_visc)

     ! Determine the maximum time-step satisying the CFL condition:
     ! dt <= (dz)_min / v_settl
     ntdt=INT(dt)
     dtmax = dzmin / vsettl
     ndt_settl(k) = MAX( 1, INT( ntdt /dtmax) )
     ! limit maximum number of iterations
     IF (ndt_settl(k) > 12) ndt_settl(k) = 12
     dt_settl(k) = REAL(ntdt) / REAL(ndt_settl(k))

     ! Particles radius in centimeters
     IF (iseas.eq.1)rcm(k) = reff(k)*100.0
     IF (idust.eq.1)then
          rwet(k) = reff(k)
          ratio_r(k) = 1.0
          rho(k) = den(k)
      endif
  END DO

  ! Solve the bidiagonal matrix (l,l)

!$OMP PARALLEL DO &
!$OMP DEFAULT( SHARED ) &
!$OMP PRIVATE( i,   j,   l,   l2, n,   k,   rhb, rwet_priv, ratio_r, c_stokes)&
!$OMP PRIVATE( free_path, c_cun, viscosity, rho_priv, vd_cor )

  ! Loop over latitudes
  DO j = 1,jmx
 
     DO k = 1,nmx
        IF (idust.eq.1) THEN
           rwet_priv(k) = rwet(k)
           rho_priv(k)  = rho(k)
        END IF

        DO n = 1,ndt_settl(k)

           ! Solve each vertical layer successively (layer l)
      
           DO l = lmx,1,-1
              l2 = lmx - l + 1

!           DO j = 1,jmx
              DO i = 1,imx

                 IF (iseas.eq.1) THEN
                    rhb = MIN(9.9D-1, rh(i,j,l))  
                    ! Aerosol growth with relative humidity (Gerber, 1985)
! td 
! changed to LOG10
                    rwet_priv(k) = 0.01*(c1*rcm(k)**c2/(c3*rcm(k)**c4 - &
                         LOG10(rhb)) + rcm(k)**3)**0.33
                    ratio_r(k) = (reff(k)/rwet_priv(k))**3.0
                 END IF

                 ! Dynamic viscosity
                 c_stokes = 1.458E-6 * tmp(i,j,l)**1.5/(tmp(i,j,l) + 110.4) 

                 ! Mean free path as a function of pressure (mb) and 
                 ! temperature (K)
                 ! order of p_mid is top->sfc
                 free_path = 1.1E-3/p_mid(i,j,l2)/SQRT(tmp(i,j,l))
!!!                 free_path = 1.1E-3/p_edge(i,j,l2)/SQRT(tmp(i,j,l))

                 ! Slip Correction Factor
                 c_cun = 1.0+ free_path/rwet_priv(k)* &
                      (1.257 + 0.4*EXP(-1.1*rwet_priv(k)/free_path))

                 ! Corrected dynamic viscosity (kg/m/s)
                 viscosity = c_stokes / c_cun

                 ! Settling velocity
                 IF (iseas.eq.1) THEN
                    rho_priv(k) = ratio_r(k)*den(k) + (1.0 - ratio_r(k))*1000.0
                 END IF

                 vd_cor = 2.0/9.0*g0*rho_priv(k)*rwet_priv(k)**2/viscosity

                 ! Update mixing ratio
                 ! Order of delz is top->sfc
                 IF (l == lmx) THEN
                    tc(i,j,l,k) = tc(i,j,l,k) / &
                         (1.0 + dt_settl(k)*vd_cor/delz(i,j,l2))
                 ELSE
                    tc(i,j,l,k) = 1.0/(1.0+dt_settl(k)*vd_cor/delz(i,j,l2))&
                         *(tc(i,j,l,k) + dt_settl(k)*vd_cor /delz(i,j,l2-1) &
                         * tc(i,j,l+1,k))
                 END IF
              END DO   !i
!           END DO   !j
        END DO  !l

     END DO  !n
  END DO  !k

  END DO   !j
!$OMP END PARALLEL DO

  DO n = 1,nmx
     DO i = 1,imx
        DO j = 1,jmx
           bstl(i,j,n) = 0.0
           DO l = 1,lmx
              IF (tc(i,j,l,n) < 1.0D-32) tc(i,j,l,n) = 1.0D-32
              bstl(i,j,n) = bstl(i,j,n) + &
                   (tc(i,j,l,n) - tc1(i,j,l,n)) * airmas(i,j,l)
           END DO
        END DO
     END DO
  END DO
  
END SUBROUTINE settling

END MODULE MODULE_GOCART_SETTLING
