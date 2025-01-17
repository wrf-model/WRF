! 07/10/24 - A. Ukhov (KAUST), bug fix: ash mass balance was
! violated in the "settling" routine. I.e. prev. discrtetisation scheme 
! did not conserve the mass of ash. Code clean-up. 

! 30/10/2024. A. UKhov (KAUST) Added dry deposition for volcanic ash, so2, sulf
! 30/10/2024. A. UKhov (KAUST) Added large scale scaveging for ash, so2, sulf 

MODULE MODULE_VASH_SETTLING

  USE module_state_description, only: p_vash_1,p_vash_2,     &
              p_vash_3,p_vash_4,p_vash_5,p_vash_6,p_vash_7,  &
              p_vash_8,p_vash_9,p_vash_10,p_sulf,p_so2,      &
              chem_vash,chem_volc,chem_volc_4bin,p_qc     ! A. Ukhov.                              

!Vertical grid indeces
!     _
!i+3 |_| l2-3
!i+2 |_| l2-2
!i+1 |_| l2-1
! i  |_|  l2
!i-1 |_| l2+1
!i-2 |_| l2+2
!i-3 |_| l2+3
!////////////
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
  real*8, DIMENSION (1,1,kte-kts+1) :: tmp,airden,p_mid,delz,rh
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
  !real*8, DIMENSION (10,55) :: ash_speed

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
          
          if(config_flags%chem_opt == CHEM_VASH  .or. config_flags%chem_opt == CHEM_VOLC ) then
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

          call vsettling(1, 1, lmx, 10, g, ash, tmp, p_mid, delz, &
                    den_ash, reff_ash, dt, bstl_ash, rh, airden)!,ash_speed)

          !if(i==20.and.j==20) then
          !  write(*,*)'1',ash_speed(1,:)
          !  write(*,*)'2',ash_speed(2,:)
          !          ...
          !  write(*,*)'10',ash_speed(10,:)
          !endif

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
          
          if(config_flags%chem_opt == CHEM_VASH  .or. config_flags%chem_opt == CHEM_VOLC ) then
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
         tc, tmp, p_mid, delz, den, reff, dt, bstl, rh,airden)!,ash_speed)

  IMPLICIT  NONE

  INTEGER, INTENT(IN) :: imx, jmx, lmx, nmx
  INTEGER :: ntdt
  REAL, INTENT(IN) :: dt,g0
  REAL*8,    INTENT(IN) :: tmp(imx,jmx,lmx), delz(imx,jmx,lmx),   &
                         rh(imx,jmx,lmx), den(nmx), reff(nmx),    &
                         p_mid(imx,jmx,lmx),airden(imx,jmx,lmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(OUT)   :: bstl(imx,jmx,nmx)
  !real*8, INTENT(INOUT) :: ash_speed(10,55)

  REAL*8    :: dt_settl(nmx), rho(nmx)
  INTEGER :: ndt_settl(nmx)
  REAL*8    :: vd_cor(lmx)
  REAL*8    :: dzmin, vsettl, dtmax, rwet(nmx)
  REAL*8    :: addmass,c_stokes, free_path, c_cun, viscosity,R_tilde
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

              ! Correction of ash settling speed for partciles with radii>20 micron
              ! details in Mailler, Sylvain, et al. 2023
              if (k < 7) then              
                    R_tilde = airden(i,j,l) * (2 * rwet_priv(k)) * vd_cor(l) / (2.*viscosity)
                    if(R_tilde>0.0116)then
                      vd_cor(l) = vd_cor(l) * (1 - (1 + (R_tilde /2.440)**(-0.4335))**(-1.905))
                    endif
              END IF

              ! Update mixing ratio. Order of delz is top->sfc
              IF (l == lmx) THEN
                tc(i,j,l,k) = tc(i,j,l,k) / (1.0 + dt_settl(k) * vd_cor(l)/delz(i,j,l2))
              ELSE
                if (l==1) then
                  bstl(i,j,k) = bstl(i,j,k) + (tc(i,j,l,k) * dt_settl(k) * vd_cor(l)/delz(i,j,l2)) * &
                                               airden(i,j,l) * delz(i,j,l2)  !ug/m2
                endif
                tc(i,j,l,k) = 1.0/(1.0 + dt_settl(k) * vd_cor(l)/delz(i,j,l2)) * &
                      (tc(i,j,l,k) + tc(i,j,l+1,k) * dt_settl(k) * vd_cor(l+1) /delz(i,j,l2-1) * &
                      ((airden(i,j,l+1)*delz(i,j,l2-1))/(airden(i,j,l)*delz(i,j,l2))))
              END IF

              !ash_speed(k,l)=vd_cor(l)
              !write(*,*)'SA ',i,j,l2,k,vd_cor(l)!,delz(i,j,l2)

           END DO  !l, height
        END DO  !n, time
     END DO  !k, bin
 
  END DO   !i
  END DO   !j
!$OMP END PARALLEL DO

END SUBROUTINE vsettling

! A. Ukhov 30 October.
! Volcanic ash, so2, sulf dry deposition. Based on gocart_drydep_driver()
subroutine volc_ash_sulf_so2_drydep_driver(dtstep,      &
        config_flags,numgas,                            &
        t_phy,moist,p8w,t8w,rmol,aer_res,               &
        p_phy,chem,rho_phy,dz8w,ddvel,xland,hfx,        &
        tsk,pbl,ust,znt,                                &
        ashdrydep,so2_dry_dep,sulf_dry_dep,             &
        ids,ide, jds,jde, kds,kde,                      &
        ims,ime, jms,jme, kms,kme,                      &
        its,ite, jts,jte, kts,kte                       )

USE module_model_constants
USE module_configure
USE module_state_description

IMPLICIT NONE

INTEGER, INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,    &
                     ims,ime, jms,jme, kms,kme,    &
                     its,ite, jts,jte, kts,kte,numgas
!INTEGER, DIMENSION( ims:ime , jms:jme ),  INTENT(IN) :: ivgtyp

REAL,      INTENT(IN   ) ::                    dtstep
REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),     &
INTENT(IN ) ::                                   moist
REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),      &
INTENT(INOUT ) ::                                 chem
REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
INTENT(IN   ) ::             t_phy, p_phy,dz8w,t8w,p8w,rho_phy
REAL, DIMENSION( its:ite, jts:jte, num_chem ),               &
INTENT(INOUT) ::                            ddvel
REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
INTENT(IN) :: tsk,pbl,ust,rmol,xland,znt,hfx
REAL, DIMENSION( its:ite, jts:jte ),  INTENT(IN)  ::  aer_res
REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) :: ashdrydep, so2_dry_dep,sulf_dry_dep                 

!! .. Local Scalars ..
INTEGER :: n, nr, ipr, imx,jmx,lmx

integer :: ii,jj,kk,i,j,k,nv
integer,dimension (1,1) :: ilwi,ireg

REAL ::  rad, ta, z1
real*8, dimension (1,1) :: z0,airden,delz_sfc,hflux,ts,pblz,ustar,ps
REAL*8  :: dvel(1,1)
LOGICAL :: highnh3, rainflag, vegflag, wetflag

TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

  do nv=numgas+1,num_chem
    do j=jts,jte
    do i=its,ite
      ddvel(i,j,nv) = 0.
    enddo
    enddo
  enddo

  imx = 1
  jmx = 1
  lmx = 1
  do j=jts,jte
  do i=its,ite
    ipr = 0
    dvel(1,1) = 0._8
    
    if( xland(i,j) > 1.5 ) then
      ilwi(1,1) = 1
    else
      ilwi(1,1) = 0
    endif
    ii = 1
    ireg(1,1) = 1
    airden(1,1)   = real( rho_phy(i,kts,j),kind=8 )
    delz_sfc(1,1) = real( dz8w(i,kts,j),kind=8 )
    ustar(1,1) = max( 1.e-1_8,real(ust(i,j),kind=8) )
    hflux(1,1) = real( hfx(i,j),kind=8 )
    pblz(1,1)  = real( pbl(i,j),kind=8 )
    ps(1,1) = real(p8w(i,kts,j),kind=8)*.01_8
    z0(1,1) = real( znt(i,j),kind=8 )
    ts(1,1) = real( tsk(i,j),kind=8 )

    call dry_depvel_ash_sulf_so2(config_flags,ipr,ii,imx,jmx,lmx,&
                airden, delz_sfc, pblz, ts, ustar, hflux, ilwi, &
                ps, z0, dvel,g,rmol(i,j),aer_res(i,j))
    
    if(config_flags%chem_opt == chem_vash  .or. config_flags%chem_opt == chem_volc ) then            
      do nv = p_vash_1,p_vash_10
        ddvel(i,j,nv) = real( dvel(1,1),kind=4 )
      enddo
    endif

    if(config_flags%chem_opt == chem_volc_4bin) then            
      do nv = p_vash_7,p_vash_10
        ddvel(i,j,nv) = real( dvel(1,1),kind=4 )
      enddo
    endif

    if(config_flags%chem_opt == chem_volc ) then
      ddvel(i,j,p_sulf) = real( dvel(1,1),kind=4 )
      !SO2
      so2_dry_dep(i,j)=so2_dry_dep(i,j) + chem(i,1,j,p_so2) * airden(1,1) * ddvel(i,j,p_so2) * dtstep * 1.E-6 / 28.97 ![mol m^-2]
      !sulf 
      sulf_dry_dep(i,j)=sulf_dry_dep(i,j) + chem(i,1,j,p_sulf) * airden(1,1) * ddvel(i,j,p_sulf) * dtstep * 1.E-6 / 28.97 ![mol m^-2]
    endif

    !drydep [kg/m2] = drydep [kg/m2]+1.e-9*dt[s]*dvel [m/s] * airden [kg/m3] * chem [ug/kg]
    !ashdrydep(i,j) = ashdrydep(i,j) + 1.e-9 * dtstep * dvel(1,1) * airden(1,1) * chem(i,1,j,p_vash_10)

    ashdrydep(i,j) = ashdrydep(i,j) + 1.e-9 * dtstep * dvel(1,1) * airden(1,1) * &
    (chem(i,1,j,p_vash_7) + chem(i,1,j,p_vash_8) + chem(i,1,j,p_vash_9) + chem(i,1,j,p_vash_10))

    if(config_flags%chem_opt == chem_vash  .or. config_flags%chem_opt == chem_volc ) then
      ashdrydep(i,j) = ashdrydep(i,j) + 1.e-9 * dtstep * dvel(1,1) * airden(1,1) * &
      (chem(i,1,j,p_vash_1) + chem(i,1,j,p_vash_2) + chem(i,1,j,p_vash_3) + chem(i,1,j,p_vash_4)+ &
       chem(i,1,j,p_vash_5) + chem(i,1,j,p_vash_6))
  
    endif

  enddo
  enddo

end subroutine volc_ash_sulf_so2_drydep_driver

SUBROUTINE dry_depvel_ash_sulf_so2( config_flags,ipr,ii,imx,jmx,lmx, &
             airden, delz_sfc, pblz, ts, ustar, hflux, ilwi, &
             ps, z0, dvel,g0,rmol,aer_res)

  ! ****************************************************************************
  ! *                                                                          *
  ! *  Calculate dry deposition velocity.                                      *
  ! *                                                                          *
  ! *  Input variables:                                                        *
  ! *    AEROSOL(k)      - Logical, T = aerosol species, F = gas species       *
  ! *    IREG(i,j)       - # of landtypes in grid square                       *
  ! *    ILAND(i,j,ldt)  - Land type ID for element ldt =1,IREG(i,j)           *
  ! *    IUSE(i,j,ldt)   - Fraction of gridbox area occupied by land type      *
  ! *                      element ldt                                         *
  ! *    USTAR(i,j)      - Friction velocity (m s-1)                           *
  ! *    DELZ_SFC(i,j)   - Thickness of layer above surface                    *
  ! *    PBLZ(i,j)       - Mixing depth (m)                                    *
  ! *    Z0(i,j)         - Roughness height (m)                                *
  ! *                                                                          *
  ! *  Determined in this subroutine (local):                                  *
  ! *    OBK             - Monin-Obukhov length (m): set to 1.E5 m under       *
  ! *                      neutral conditions                                  *
  ! *    Rs(ldt)         - Bulk surface resistance(s m-1) for species k to     * 
  ! *                      surface ldt                                         *
  ! *    Ra              - Aerodynamic resistance.                             *
  ! *    Rb              - Sublayer resistance.                                *
  ! *    Rs              - Surface resistance.                                 *
  ! *    Rttl            - Total deposition resistance (s m-1) for species k   *
  ! *                      Rttl(k) = Ra + Rb + Rs.                             *
  ! *                                                                          *
  ! *  Returned:                                                               *
  ! *    DVEL(i,j,k)     - Deposition velocity (m s-1) of species k            *
  ! *    DRYDf(i,j,k)    - Deposition frequency (s-1) of species k,            *
  ! *                    = DVEL / DELZ_SFC                                     *
  ! *                                                                          *
  ! **************************************************************************** 

  USE module_configure

    IMPLICIT NONE

    REAL*8,    INTENT(IN)  :: airden(imx,jmx), delz_sfc(imx,jmx)
    REAL*8,    INTENT(IN)  :: hflux(imx,jmx), ts(imx,jmx)
    REAL*8,    INTENT(IN)  :: ustar(imx,jmx), pblz(imx,jmx)
    REAL*8,    INTENT(IN)  :: ps(imx,jmx)
    INTEGER, INTENT(IN)  :: ilwi(imx,jmx)
    INTEGER, INTENT(IN)  :: imx,jmx,lmx
    REAL*8,    INTENT(IN)  :: z0(imx,jmx)
    REAL,    INTENT(IN)  :: g0,rmol,aer_res
    REAL*8,    INTENT(OUT) :: dvel(imx,jmx)!, drydf(imx,jmx)

    TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

    REAL*8    :: obk, vds, czh, rttl, frac, logmfrac, psi_h, cz, eps
    REAL*8    :: vd, ra, rb, rs  
    INTEGER :: ipr,i, j, k, ldt, iolson, ii
    CHARACTER(LEN=50) :: msg
    REAL*8     :: prss, tempk, tempc, xnu, ckustr, reyno, aird, diam, xm, z
    REAL*8     :: frpath, speed, dg, dw, rt
    REAL*8     :: rad0, rix, gfact, gfaci, rdc, rixx, rluxx, rgsx, rclx
    REAL*8     :: dtmp1, dtmp2, dtmp3, dtmp4
    REAL*8     :: biofit,vk

    ! executable statements
    j_loop: DO j = 1,jmx               
    i_loop: DO i = 1,imx            
      vk    = .4_8
      vd    = 0._8
      ra    = 0._8
      rb    = 0._8 ! only required for gases (SO2)
      rs    = 0.0_8

      ! ****************************************************************************
      ! *  Compute the the Monin-Obhukov length.                                   *
      ! *  The direct computation of the Monin-Obhukov length is:                  *
      ! *                                                                          *
      ! *           - Air density * Cp * T(surface air) * Ustar^3                  *
      ! *   OBK =   ----------------------------------------------                 *
      ! *                    vK   * g  * Sensible Heat flux                        *
      ! *                                                                          *
      ! *    Cp = 1000 J/kg/K    = specific heat at constant pressure              *
      ! *    vK = 0.4            = von Karman's constant                           *
      ! ****************************************************************************

      IF (abs(hflux(i,j)) <= 1.e-5_8) THEN
        obk = 1.0E5_8
      ELSE
        ! MINVAL(hflux), MINVAL(airden), MINVAL(ustar) =??
        obk = -airden(i,j) * 1000.0_8 * ts(i,j) * (ustar(i,j))**3 &
              / (vk * real(g0,kind=8) * hflux(i,j)) 
        ! -- debug:
        IF ( obk == 0.0_8 ) WRITE(*,211) obk, i, j
        211        FORMAT(1X,'OBK=', E11.2, 1X,' i,j = ', 2I4)
      END IF
        !       write(0,*)1./obk,rmol

      if(rmol.ne.0.)then
        obk=1._8/real(rmol,kind=8)
      else
        obk=1.e5_8
      endif

      !       cz = delz_sfc(i,j) / 2.0_8 ! center of the grid box above surface
      cz = 2._8

      ! ****************************************************************************
      ! *  (1) Aerosodynamic resistance Ra and sublayer resistance Rb.             *
      ! *                                                                          *
      ! *  The Reynolds number REYNO diagnoses whether a surface is                *
      ! *  aerodynamically rough (REYNO > 10) or smooth.  Surface is               *
      ! *  rough in all cases except over water with low wind speeds.              *
      ! *                                                                          *
      ! *  For gas species over land and ice (REYNO >= 10) and for aerosol         *
      ! *  species for all surfaces:                                               *
      ! *                                                                          *
      ! *      Ra = 1./VT          (VT from GEOS Kzz at L=1, m/s).                 *
      ! *                                                                          *
      ! *  The following equations are from Walcek et al, 1986:                    *
      ! *                                                                          *
      ! *  For gas species when REYNO < 10 (smooth), Ra and Rb are combined        *
      ! *  as Ra:                                                                  *
      ! *                                                                          *
      ! *      Ra = { ln(ku* z1/Dg) - Sh } / ku*           eq.(13)                 *
      ! *                                                                          *
      ! *      where z1 is the altitude at the center of the lowest model layer    *
      ! *               (CZ);                                                      *
      ! *            Sh is a stability correction function;                        *
      ! *            k  is the von Karman constant (0.4, vK);                      *
      ! *            u* is the friction velocity (USTAR).                          *
      ! *                                                                          *
      ! *   Sh is computed as a function of z1 and L       eq ( 4) and (5)):       *
      ! *                                                                          *
      ! *    0 < z1/L <= 1:     Sh = -5 * z1/L                                     *
      ! *    z1/L < 0:          Sh = exp{ 0.598 + 0.39*ln(E) - 0.09(ln(E))^2 }     *
      ! *                       where E = min(1,-z1/L) (Balkanski, thesis).        *
      ! *                                                                          *
      ! *   For gas species when REYNO >= 10,                                      *
      ! *                                                                          *
      ! *      Rb = 2/ku* (Dair/Dg)**(2/3)                 eq.(12)                 *
      ! *      where Dg is the gas diffusivity, and                                *
      ! *            Dair is the air diffusivity.                                  *
      ! *                                                                          *
      ! *  For aerosol species, Rb is combined with surface resistance as Rs.      *
      ! *                                                                          *
      ! ****************************************************************************

      frac = cz / obk
      IF (frac > 1.0_8) frac = 1.0_8
      IF (frac > 0.0_8 .AND. frac <= 1.0_8) THEN 
        psi_h = -5.0_8*frac
      ELSE IF (frac < 0.0_8) THEN
        eps = MIN(1.0_8, -frac)
        logmfrac = LOG(eps)
        psi_h = EXP( 0.598_8 + 0.39_8 * logmfrac - 0.09_8 * (logmfrac)**2 )
      END IF
      !--------------------------------------------------------------
      !  Aerosol species, Rs here is the combination of Rb and Rs.

      ra = (LOG(cz/z0(i,j)) - psi_h) / (vk*ustar(i,j))

      vds = 0.002_8*ustar(i,j)
      IF (obk < 0.0_8) vds = vds * (1.0_8+(-300.0_8/obk)**0.6667_8)

      czh  = pblz(i,j)/obk
      IF (czh < -30.0_8) vds = 0.0009_8*ustar(i,j)*(-czh)**0.6667_8

      IF( config_flags%chem_opt /= CHEM_VASH      .and.                  &
        config_flags%chem_opt /= chem_volc_4bin           )THEN
        ra = real(aer_res,kind=8)
      ENDIF

      ! --Set Vds to be less than VDSMAX (entry in input file divided --
      !   by 1.E4). VDSMAX is taken from Table 2 of Walcek et al. [1986].
      !   Invert to get corresponding R
      !          if(ii.eq.1) then
      !             rs=1.0_8/MIN(vds,2.0e-2_8)
      !          else
      rs=1.0_8/MIN(vds,2.0e-3_8)
      !          endif

      ! ------ Set max and min values for bulk surface resistances ------

      rs= MAX(1.0_8, MIN(rs, 9.9990e+3_8))

      ! ****************************************************************************
      ! *                                                                          *
      ! *  Compute dry deposition velocity.                                        *
      ! *                                                                          *
      ! *  IUSE is the fraction of the grid square occupied by surface ldt in      *
      ! *  units of per mil (IUSE=500 -> 50% of the grid square).  Add the         *
      ! *  contribution of surface type ldt to the deposition velocity; this is    *
      ! *  a loop over all surface types in the gridbox.                           *
      ! *                                                                          *
      ! *  Total resistance = Ra + Rb + Rs.
      ! *                                                                          *
      ! ****************************************************************************

      rttl = ra + rb + rs
      vd   = vd + 1._8/rttl

      ! ------ Load array DVEL ------
      !if(ipr.eq.1) write(0,*)rs,ra,rb,vd
      
      dvel(i,j) = vd !* 1.2

      ! -- Set a minimum value for DVEL
      !    MIN(VdSO2)      = 2.0e-3 m/s  over ice
      !                    = 3.0e-3 m/s  over land
      !    MIN(vd_aerosol) = 1.0e-4 m/s

      IF (dvel(i,j) < 1.0E-4_8) dvel(i,j) = 1.0E-4_8
      
      !drydf(i,j) = dvel(i,j) / delz_sfc(i,j)

    END DO i_loop
    END DO j_loop

END SUBROUTINE dry_depvel_ash_sulf_so2

! A. Ukhov 30 October 2024
! Based on wetdep_ls()
subroutine wetdep_ls_volc(dt,var,rain,moist,rho,num_moist, &
  num_chem,numgas,dz8w,vvel,chem_opt,             &
  wd_ash_sc,wd_so2_sc,wd_sulf_sc, & ! A. Ukhov. for Ash, SO2  and Sulf LS scaveging



! A. Ukhov 30/10/2024. Large Scale scaveging Ash, SO2 and Sulf aerosols
! Based on wetdep_ls() for GOCART aerosols
subroutine wetdep_ls_volc(dt,var,rain,moist,rho,num_moist,          &
  num_chem,numgas,dz8w,vvel,chem_opt,                               &
  wd_ash_sc,wd_so2_sc,wd_sulf_sc,                                   & 
  ids,ide, jds,jde, kds,kde,                                        &
  ims,ime, jms,jme, kms,kme,                                        &
  its,ite, jts,jte, kts,kte                                         )

  USE module_model_constants, ONLY: mwdry
IMPLICIT NONE

INTEGER,      INTENT(IN   ) :: num_chem,numgas,num_moist,               &
                           chem_opt,                                &
                           ids,ide, jds,jde, kds,kde,               &
                           ims,ime, jms,jme, kms,kme,               &
                           its,ite, jts,jte, kts,kte
real, INTENT(IN ) :: dt
REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
  INTENT(IN ) ::                                   moist
REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
   INTENT(IN   ) :: rho,dz8w,vvel        
REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ,1:num_chem),              &
   INTENT(INOUT) :: var        
REAL,  DIMENSION( ims:ime , jms:jme ),                                  &
   INTENT(IN   ) :: rain

!Accumulated large scale scaveged so2 and sulf
REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: wd_so2_sc,wd_sulf_sc

REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) :: wd_ash_sc
REAL,  DIMENSION( its:ite ,  jts:jte ) :: var_sum,var_rmv
REAL,  DIMENSION( its:ite ,  jts:jte ) :: frc,var_sum_clw,rain_clw     
real :: dvar,factor,clsum,alpha!,rho_water
integer :: nv,i,j,k,km

!write(0,*) 'in wetdepls, numgas,num_chem = ',numgas,num_chem,chem_opt
!in wetdepls, numgas,num_chem =            3          13         402
  do nv=1,num_chem
    
    !if(nv.le. numgas .and. nv.ne.p_sulf)cycle
    alpha = .5    ! scavenging factor

    if(nv.eq.p_sulf .or. nv.eq.p_so2)alpha=1.
    
    do i=its,ite
    do j=jts,jte
        var_sum_clw(i,j)=0.
        var_sum(i,j)=0.
        !var_rmvl(i,:,j)=0.
        frc(i,j)=0.
        rain_clw(i,j)=0.
        if(rain(i,j).gt.1.e-10)then
        ! convert rain back to rate
            rain_clw(i,j)=rain(i,j)/dt
        ! total cloud water
        !
            do k=1,kte-1
              dvar=max(0.,moist(i,k,j,p_qc)*rho(i,k,j)*vvel(i,k,j)*dz8w(i,k,j))
              var_sum_clw(i,j)=var_sum_clw(i,j)+dvar
              var_sum(i,j)=var_sum(i,j)+var(i,k,j,nv)*rho(i,k,j)
            enddo
            if(var_sum(i,j).gt.1.e-10 .and. var_sum_clw(i,j).gt.1.e-10 ) then
        !        assuming that frc is onstant, it is my conversion factor 
        !       (just like in convec. parameterization
              frc(i,j)=rain_clw(i,j)/var_sum_clw(i,j)
        !    write(0,*)'frc ', frc(i,j),var_sum_clw(i,j),var_sum(i,j)
              frc(i,j)=max(1.e-6,min(frc(i,j),.005))
            endif
        endif
    enddo   !j
    enddo   !i
    !
    ! get rid of it
    !
    do i=its,ite
    do j=jts,jte
    if(rain(i,j).gt.1.e-10 .and. var_sum(i,j).gt.1.e-10 .and. var_sum_clw(i,j).gt.1.e-10)then
      do k=kts,kte-2
          if(var(i,k,j,nv).gt.1.e-16 .and. moist(i,k,j,p_qc).gt.0.)then
            factor = max(0.,frc(i,j)*rho(i,k,j)*dz8w(i,k,j)*vvel(i,k,j))
        !        write(0,*)'var before ',k,km,var(i,k,j,nv),factor
            dvar=alpha*factor/(1+factor)*var(i,k,j,nv)
            var(i,k,j,nv)=max(1.e-16,var(i,k,j,nv)-dvar)
        !        write(0,*)'var after ',km,var(i,k,j,nv),dvar

            ! accumulated scaveged volcanic SO2 and Sulf
            if (chem_opt .eq. chem_volc) then
                if(nv.eq.p_so2) then
                  wd_so2_sc(i,j) = wd_so2_sc(i,j) + dvar *  rho(i,k,j) * dz8w(i,k,j)/mwdry ! mmol/m2
                endif

                if(nv.eq.p_sulf) then
                  wd_sulf_sc(i,j) = wd_sulf_sc(i,j) + dvar * rho(i,k,j) * dz8w(i,k,j)/mwdry ! mmol/m2
                endif
            endif

            ! accumulated scaveged volcanic ash
            if (chem_opt .eq. chem_vash .OR. chem_opt .eq. chem_volc) then
                if(nv.eq.p_vash_1 .or. nv.eq.p_vash_2 .or. nv.eq.p_vash_3 .or. nv.eq.p_vash_4 .or. &
                  nv.eq.p_vash_5 .or. nv.eq.p_vash_6 .or. nv.eq.p_vash_7 .or. nv.eq.p_vash_8 .or. &
                  nv.eq.p_vash_9 .or. nv.eq.p_vash_10) then
                  wd_ash_sc(i,j) = wd_ash_sc(i,j) + 1.E-6 * dvar * rho(i,k,j) * dz8w(i,k,j) !g/m2
                endif
            endif

            if(chem_opt == chem_volc_4bin) then
                if(nv.eq.p_vash_7 .or. nv.eq.p_vash_8 .or. nv.eq.p_vash_9 .or. nv.eq.p_vash_10) then
                  wd_ash_sc(i,j) = wd_ash_sc(i,j) + 1.E-6 * dvar * rho(i,k,j) * dz8w(i,k,j) !g/m2
                endif
            endif  
          endif
      enddo !k
    !      var_rmv(i,j)=var_rmv(i,j)+var_rmvl(i,j)
    endif   !rain...
    enddo   !j
    enddo   !i
  enddo   !nv
END SUBROUTINE WETDEP_LS_VOLC


END MODULE MODULE_VASH_SETTLING
