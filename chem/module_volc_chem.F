! 17 Oct. 2024. A. Ukhov (KAUST). Oxidation of SO2 into Sulf using OH and H2O2
! based on module_gocart_chem.
! 13 Dec. 2025. A. Ukhov (KAUST). Contribution of volcanic Sulf and ash into PM2.5 and PM10

MODULE MODULE_VOLC_CHEM

   CONTAINS
   
     subroutine gocart_volc_chem_driver(curr_secs,dt,config_flags,            &
            gmt,julday,t_phy,moist,                                           &
            chem,rho_phy,dz8w,p8w,backg_oh,backg_h2o2,                        &
            gd_cldf,dx,dy,mapfac_mx,mapfac_my,g,xlat,xlong,ttday,tcosz,       &
            volc_diags ,                                                      &
            ids,ide, jds,jde, kds,kde,                                        &
            ims,ime, jms,jme, kms,kme,                                        &
            its,ite, jts,jte, kts,kte                                         )
     USE module_configure
     USE module_state_description
     USE module_gocart_chem, only: szangle
     IMPLICIT NONE
      TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   
      INTEGER,      INTENT(IN   ) :: julday,                                  &
                                     ids,ide, jds,jde, kds,kde,               &
                                     ims,ime, jms,jme, kms,kme,               &
                                     its,ite, jts,jte, kts,kte
       REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
            INTENT(IN ) ::                                   moist
      REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
            INTENT(INOUT ) ::                                   chem
      REAL,  DIMENSION( ims:ime , jms:jme ),                        &
             INTENT(IN   ) ::                                                 &
                 xlat,xlong,ttday,tcosz,mapfac_mx,mapfac_my
   
      REAL, DIMENSION( ims:ime , jms:jme ,1:9), INTENT(INOUT) :: volc_diags
   
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
             OPTIONAL,                                                        &
             INTENT(IN   ) ::                     gd_cldf
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
             INTENT(IN   ) :: t_phy,backg_oh,backg_h2o2,dz8w,p8w,             &
                                                 rho_phy
   REAL(KIND=8), INTENT(IN) :: curr_secs
   REAL, INTENT(IN   ) :: dt,dx,dy,g,gmt
   integer :: nmx,i,j,k,imx,jmx,lmx
   real*8, DIMENSION (1,1,1) :: tmp,airden,airmas,oh,h2o2,    &
                                 chlso2_oh,chlso2_aq, cldf!chpso4
   real*8, DIMENSION (1,1) :: cossza
   real, DIMENSION (1,1) :: sza,cosszax
   real*8, DIMENSION (1,1,1,2) :: tc
   real(kind=8) :: xtime,xhour
   real:: rlat,xlonn,area
   real :: xmin,gmtp
   integer(kind=8) :: ixhour
   
      imx=1
      jmx=1
      lmx=1
      nmx=4
   
      xtime=curr_secs/60._8
      ixhour=int(gmt+.01,8)+int(xtime/60._8,8)
      xhour=real(ixhour,8)
      xmin=60.*gmt+real(xtime-xhour*60._8,8)
      gmtp=mod(xhour,24._8)
      gmtp=gmtp+xmin/60.
   
      do j=jts,jte
      do i=its,ite
         rlat=xlat(i,j)*3.1415926535590/180.
         xlonn=xlong(i,j)
         CALL szangle(1, 1, julday, gmtp, sza, cosszax,xlonn,rlat)
         cossza(1,1)=cosszax(1,1)
      !
         do k=kts,kte-1
            !chpso4=0.
            chlso2_oh=0.
            chlso2_aq=0.
            if (present(gd_cldf) ) then
               cldf(1,1,1)=gd_cldf(i,k,j)
            else
               cldf(1,1,1)=0.
            endif
   
            if(p_qc.gt.1 .and. p_qi.gt.1)then
            if(moist(i,k,j,p_qc).gt.0.or.moist(i,k,j,p_qi).gt.0.)cldf(1,1,1)=1.
            elseif(p_qc.gt.1 .and. p_qi.le.1)then
            if(moist(i,k,j,p_qc).gt.0.)cldf(1,1,1)=1.
            endif
                     
            area=(dx/mapfac_mx(i,j))*(dy/mapfac_my(i,j))         
            airmas(1,1,1)=-(p8w(i,k+1,j)-p8w(i,k,j))*area/g ! air mass inside cell (kg)
            !airmas(1,1,1)=-(p8w(i,k+1,j)-p8w(i,k,j))*dx*dx/g
   
            airden(1,1,1)=rho_phy(i,k,j)
            tmp(1,1,1)=t_phy(i,k,j)
            !oh(1,1,1)=86400./dt*cossza(1,1)*backg_oh(i,k,j)/tcosz(i,j) !backg_oh  in ppv
            oh(1,1,1)=86400./dt*cossza(1,1)*backg_oh(i,k,j)/tcosz(i,j) !backg_oh  in molecules/cm3
   
            h2o2(1,1,1)=backg_h2o2(i,k,j)          !ppv
            tc(1,1,1,1)=chem(i,k,j,p_so2) *1.d-6   !ppv
            tc(1,1,1,2)=chem(i,k,j,p_sulf)*1.d-6   !ppv
            
            call chmdrv_volc_su( imx,jmx,lmx, nmx, dt, tmp, airden, airmas, &
                     oh, h2o2, cldf, tc, chlso2_oh, chlso2_aq)!chpso4
   
            chem(i,k,j,p_so2)= tc(1,1,1,1)*1.e6    !ppmv
            chem(i,k,j,p_sulf)=tc(1,1,1,2)*1.e6    !ppmv
   
            volc_diags(i,j,p_so2_oh_loss) = volc_diags(i,j,p_so2_oh_loss) + chlso2_oh(1,1,1)  !loss (kg/dt)
            volc_diags(i,j,p_so2_h2o2_loss) = volc_diags(i,j,p_so2_h2o2_loss) + chlso2_aq(1,1,1)  !loss (kg/dt)
         enddo
      enddo
      enddo   
   end subroutine gocart_volc_chem_driver
   
   
   SUBROUTINE chmdrv_volc_su( imx,jmx,lmx, nmx, dt1, tmp, airden, airmas, &
                  oh, h2o2, cldf, tc, chlso2_oh, chlso2_aq)!chpso4
     
     IMPLICIT NONE
   
     INTEGER, INTENT(IN) :: nmx,imx,jmx,lmx
     integer :: ndt1
     real, intent(in) :: dt1
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: tmp, airden, airmas
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: oh, cldf
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: h2o2
     REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chlso2_oh, chlso2_aq!chpso4
   
     ndt1=ifix(dt1)
     if(ndt1.le.0)stop
   
      CALL chem_volc_so2_so4(imx,jmx,lmx, nmx, ndt1, tmp, airden, airmas, &
             oh, h2o2, cldf, tc, chlso2_oh, chlso2_aq)!, chpso4,pso4_so2)
     
   END SUBROUTINE chmdrv_volc_su
   
   
   SUBROUTINE chem_volc_so2_so4(imx,jmx,lmx, nmx, ndt1, tmp, airden, airmas, &
               oh, h2o2, cldf, tc, chlso2_oh, chlso2_aq)!,chpso4, pso4_so2)
   
   ! ****************************************************************************
   ! *                                                                          *
   ! *  This is SO2 chemistry subroutine.                                       *
   ! *                                                                          *
   ! *  SO2 loss:                                                               * 
   ! *    SO2 + OH  -> SO4                                                      *
   ! *    SO2 + H2O2  -> SO4                                          *
   ! *                                                                          *
   ! *  If there is cloud in the gridbox (fraction = fc), then the aqueous      *
   ! *  phase chemistry also takes place in cloud. The amount of SO2 oxidized   *
   ! *  by H2O2 in cloud is limited by the available H2O2;                      *
   ! *                                                                          *
   ! ****************************************************************************
   
     IMPLICIT NONE
   
     INTEGER, INTENT(IN) ::  nmx, ndt1,imx,jmx,lmx
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: tmp, airden,airmas
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: cldf, oh
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: h2o2
     REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
   
     REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chlso2_oh, chlso2_aq !chpso4
   !  REAL*8, INTENT(OUT) :: pso4_so2(imx,jmx,lmx)
     REAL*8, DIMENSION(imx,jmx,lmx) :: pso4_so2
   
   
     REAL*8 ::  k0, kinf, kk, m, l1, l2!, ld
     INTEGER               :: NSO2=1
     INTEGER               :: NSO4=2
     REAL,    PARAMETER :: airmw    = 28.97
     REAL,    PARAMETER :: smw      = 32.00 
     ! Factor to convert AIRDEN from kgair/m3 to molecules/cm3: 
     REAL*8, PARAMETER :: f  = 1000. / airmw * 6.022D23 * 1.0D-6
     !REAL*8, PARAMETER :: ki = 1.5D-12
     INTEGER :: i, j, l
     REAL*8 :: so20,so40, tk, f1,  rk1, rkt, so2_cd, fc, so2,so4
   
     ! executable statements
   
     DO l = 1,lmx
        DO j = 1,jmx
           DO i = 1,imx
              
              so20 = tc(i,j,l,NSO2)       !mol/mol
              so40 = tc(i,j,l,NSO4)       !mol/mol
   
              ! RK: SO2 + OH(g), in s-1 
              tk = tmp(i,j,l)
              k0 = 2.9D-31 * (298/tk)**4.1   !cm6 molecule−2 s−1
              kinf = 1.7D-12 * (298/tk)**(-0.2)    !cm3 molecule−1 s−1
              m  = airden(i,j,l) * f
              kk = k0 * m / kinf
              f1 = ( 1.0+ ( LOG10(kk) )**2 )**(-1)
              
               ! if OH in ppv convert it to molecules/cm3
              !rk1 = ( k0 * m / (1.0 + kk) ) * 0.6**f1 * oh(i,j,l) * airden(i,j,l)*f
   
              ! if OH in [molecules/cm3] do nothing
              rk1 = ( k0 * m / (1.0 + kk) ) * 0.6**f1 * oh(i,j,l)
              !rk2 = 0.0
   
              !rk  = (rk1 + rk2)
              rkt =  rk1  * REAL(ndt1)
   
   ! ****************************************************************************
   ! *  Update SO2 concentration after gas phase chemistry
   ! ****************************************************************************
   
              IF (rkt > 0.0) THEN
                 so2_cd = so20 * EXP(-rkt)            !mol/mol
                 l1     = (so20 - so2_cd) !* rk1/rk    ! l1 loss SO2 [mol/mol]
              ELSE
                 so2_cd = so20
                 l1 = 0.0
              END IF
   
   ! ****************************************************************************
   ! *  Update SO2 concentration after cloud chemistry.                         *
   ! *  SO2 chemical loss rate  = SO4 production rate (MixingRatio/timestep).   *
   ! ****************************************************************************
   
              ! Cloud chemistry (above 258K): 
              fc = cldf(i,j,l)
              IF (fc > 0.0 .AND. so2_cd > 0.0 .AND. tk > 258.0) THEN
   
                 IF (so2_cd > h2o2(i,j,l)) THEN       !mol/mol
                    fc = fc * (h2o2(i,j,l)/so2_cd)
                    !h2o2(i,j,l) = h2o2(i,j,l) * (1.0 - cldf(i,j,l))
                 !ELSE
                    !h2o2(i,j,l) = h2o2(i,j,l) * &
                    !     (1.0 - cldf(i,j,l)*so2_cd/h2o2(i,j,l))
                 END IF
                 so2 = so2_cd * (1.0 - fc)
                 ! Aqueous phase SO2 loss rate (MixingRatio/timestep): 
                 l2  = so2_cd * fc                    ! l2 loss SO2 !mol/mol
              ELSE
                 so2 = so2_cd
                 l2 = 0.0
              END IF
   
              so2    = MAX(so2, 1.0D-32)
              tc(i,j,l,NSO2) = so2
   
   ! ****************************************************************************
   ! *  SO2 chemical loss rate  = SO4 production rate (MixingRatio/timestep).   *
   ! ****************************************************************************
   
              pso4_so2(i,j,l) = max(0.0D0,l1 + l2)       !mol/mol
              so4 = so40 + pso4_so2(i,j,l)
   
              if(pso4_so2(i,j,l).lt.0.)then
                write(0,*)'so4 routine, pso4 = ',pso4_so2(i,j,l),so4,so40
              endif
   
              so4    = MAX(so4, 1.0D-32)
              tc(i,j,l,NSO4) = so4
   
              !    ---------------------------------------------------------------
              !    DIAGNOSTICS:      SO2 gas-phase loss       (kgS/timestep)  
              !                      SO2 aqueous-phase loss   (kgS/timestep) 
              !                      !SO4 production           (kgS/timestep) 
              !    ---------------------------------------------------------------
              chlso2_oh(i,j,l) = chlso2_oh(i,j,l) + l1 * airmas(i,j,l) / airmw * smw
              chlso2_aq(i,j,l) = chlso2_aq(i,j,l) + l2 * airmas(i,j,l) / airmw * smw
              
              !chpso4(i,j,l) = chpso4(i,j,l) + pso4_so2(i,j,l) * airmas(i,j,l) / airmw * smw
              
           END DO
        END DO
     END DO
   
   END SUBROUTINE chem_volc_so2_so4
   
   ! Compute pm2_5 and pm10 from volcanic ash and sulfate
   subroutine sum_pm_volc (                                      &
      alt, chem,pm2_5_dry, pm10,                                 &
      ids,ide, jds,jde, kds,kde,                                 &
      ims,ime, jms,jme, kms,kme,                                 &
      its,ite, jts,jte, kts,kte                                  )
   USE module_configure
   USE module_state_description
   USE module_data_gocartchem, only: nh4_mfac
   USE module_model_constants, only: mwdry
   IMPLICIT NONE
   
   REAL, PARAMETER :: mwso4 = 96.0576
   INTEGER,      INTENT(IN   ) :: ids,ide, jds,jde, kds,kde,     &
                            ims,ime, jms,jme, kms,kme,           &
                            its,ite, jts,jte, kts,kte
   
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(INOUT ) :: pm2_5_dry, pm10
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN ) :: alt
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), INTENT(IN ) :: chem
   
   real sulfate
   integer i,j,k,ii,jj
   
      pm2_5_dry(its:ite, kts:kte, jts:jte) = 0.
      pm10(its:ite, kts:kte, jts:jte)    = 0.
   
      do j=jts,jte                    
      jj=min(jde-1,j)              
      do k=kts,kte
      do i=its,ite
         ii=min(ide-1,i)
         sulfate = chem(ii,k,jj,p_sulf) * (mwso4/mwdry) * 1.e3  !ppmv -> (ug/kg)
         
         !vash10:[0.0-3.906] diam. (um) => log(2.5-0)/log(3.906-0)=0.672
         pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) + sulfate * nh4_mfac  &
                           + 0.672 * chem(ii,k,jj,p_vash_10)
   
         ! vash8:[7.812-15.625] diam. (um) => (log(10)-log(7.812))/(log(15.625)-log(7.812))=0.356
         pm10(i,k,j) = pm10(i,k,j) + sulfate * nh4_mfac        &
                     + chem(ii,k,jj,p_vash_10) + chem(ii,k,jj,p_vash_9)    &
                     + 0.356 * chem(ii,k,jj,p_vash_8)
   
         !Convert from mixing ratio (ug/kg) to concentration (ug m^-3)      
         pm2_5_dry(i,k,j) = pm2_5_dry(i,k,j) / alt(ii,k,jj)
         pm10(i,k,j) = pm10(i,k,j)/ alt(ii,k,jj)
      enddo
      enddo
      enddo
   
   end subroutine sum_pm_volc
   
   END MODULE MODULE_volc_CHEM
   