MODULE MODULE_GOCART_CHEM

CONTAINS

  subroutine gocart_chem_driver(curr_secs,dt,config_flags,            &
         gmt,julday,t_phy,moist,                                      &
         chem,rho_phy,dz8w,p8w,backg_oh,backg_h2o2,backg_no3,         &
         gd_cldf,dx,g,xlat,xlong,ttday,tcosz, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  USE module_configure
  USE module_state_description
  USE module_phot_mad, only : calc_zenith
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
              xlat,xlong,ttday,tcosz
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          OPTIONAL,                                                        &
          INTENT(IN   ) ::                     gd_cldf
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::                     t_phy,                      &
                              backg_oh,backg_h2o2,backg_no3,dz8w,p8w,      &
                                              rho_phy
  REAL(KIND=8), INTENT(IN) :: curr_secs
  REAL, INTENT(IN   ) :: dt,dx,g,gmt
  integer :: nmx,i,j,k,imx,jmx,lmx
  real*8, DIMENSION (1,1,1) :: tmp,airden,airmas,oh,xno3,h2o2,chldms_oh,    &
                               chldms_no3,chldms_x,chpso2,chpmsa,chpso4,    &
                               chlso2_oh,chlso2_aq,cldf
  real*8, DIMENSION (1,1,4) :: tdry
  real*8, DIMENSION (1,1) :: cossza
  real, DIMENSION (1,1) :: sza,cosszax
  real*8, DIMENSION (1,1,1,4) :: tc,bems
  real*8, dimension (1) :: dxy
  real(kind=8) :: xtime,xhour
  real:: rlat,xlonn
  real :: zenith,zenita,azimuth,xmin,xtimin,gmtp
  integer(kind=8) :: ixhour

  imx=1
  jmx=1
  lmx=1
  nmx=4
  tdry=0.d0

  xtime=curr_secs/60._8
  ixhour=int(gmt+.01,8)+int(xtime/60._8,8)
  xhour=real(ixhour,8)
  xmin=60.*gmt+real(xtime-xhour*60._8,8)
  gmtp=mod(xhour,24._8)
  gmtp=gmtp+xmin/60.
 
  dxy(1)=dx*dx
!
! following arrays for busget stuff only
!
!
!
       chem_select: SELECT CASE(config_flags%chem_opt)
          CASE (GOCART_SIMPLE)
           CALL wrf_debug(15,'calling gocart chemistry ')
       do j=jts,jte
       do i=its,ite
        zenith=0.
        zenita=0.
        azimuth=0.
        rlat=xlat(i,j)*3.1415926535590/180.
        xlonn=xlong(i,j)
        CALL szangle(1, 1, julday, gmtp, sza, cosszax,xlonn,rlat)
        cossza(1,1)=cosszax(1,1)
!
       do k=kts,kte-1
       chldms_oh=0.
       chldms_no3=0.
       chldms_x=0.
       chpso2=0.
       chpmsa=0.
       chpso4=0.
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
          tc(1,1,1,1)=chem(i,k,j,p_dms)*1.d-6
          tc(1,1,1,2)=chem(i,k,j,p_so2)*1.d-6
          tc(1,1,1,3)=chem(i,k,j,p_sulf)*1.d-6
          tc(1,1,1,4)=chem(i,k,j,p_msa)*1.d-6
          airmas(1,1,1)=-(p8w(i,k+1,j)-p8w(i,k,j))*dx*dx/g
          airden(1,1,1)=rho_phy(i,k,j)
          tmp(1,1,1)=t_phy(i,k,j)
          oh(1,1,1)=86400./dt*cossza(1,1)*backg_oh(i,k,j)/tcosz(i,j)
          h2o2(1,1,1)=backg_h2o2(i,k,j)
           IF (COSSZA(1,1) > 0.0) THEN
              XNO3(1,1,1) = 0.0
           ELSE
              ! -- Fraction of night
              ! fnight       = 1.0 - TTDAY(i,j)/86400.0
              ! The original xno3 values have been averaged over daytime
              ! as well => divide by fnight to get the appropriate night-time
              ! fraction from the monthly average
              ! fnight/=0.0 (for fnight=0: all cosszax (including current
              ! cossza) > 0.0)
              xno3(1,1,1) = backg_no3(i,k,j) / (1.0 - TTDAY(i,j)/86400.)
           END IF
!         if(i.eq.19.and.j.eq.19.and.k.eq.kts)then
!          write(0,*)backg_oh(i,k,j),backg_no3(i,k,j),ttday(i,j),tcosz(i,j)
!         endif

          call chmdrv_su( imx,jmx,lmx,&
               nmx, dt, tmp, airden, airmas, &
               oh, xno3, h2o2, cldf, tc, tdry,cossza,  &
               chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa, chpso4, &
               chlso2_oh, chlso2_aq)
          chem(i,k,j,p_dms)=tc(1,1,1,1)*1.e6
          chem(i,k,j,p_so2)=tc(1,1,1,2)*1.e6
          chem(i,k,j,p_sulf)=tc(1,1,1,3)*1.e6
          chem(i,k,j,p_msa)=tc(1,1,1,4)*1.e6
       enddo
       enddo
       enddo
     CASE (GOCARTRACM_KPP,GOCARTRADM2)
       CALL wrf_debug(15,'calling gocart chemistry in addition to racm_kpp')
       do j=jts,jte
       do i=its,ite
        zenith=0.
        zenita=0.
        azimuth=0.
        rlat=xlat(i,j)*3.1415926535590/180.
        xlonn=xlong(i,j)
        CALL szangle(1, 1, julday, gmtp, sza, cosszax,xlonn,rlat)
        cossza(1,1)=cosszax(1,1)
       do k=kts,kte-1
       chldms_oh=0.
       chldms_no3=0.
       chldms_x=0.
       chpso2=0.
       chpmsa=0.
       chpso4=0.
       chlso2_oh=0.
       chlso2_aq=0.
          if( present(gd_cldf) ) then
            cldf(1,1,1)=gd_cldf(i,k,j)
          else
            cldf(1,1,1)=0.
          endif
          if(p_qi.gt.1)then
          if(moist(i,k,j,p_qc).gt.0.or.moist(i,k,j,p_qi).gt.0.)cldf(1,1,1)=1.
          elseif(p_qc.gt.1)then
          if(moist(i,k,j,p_qc).gt.0.)cldf(1,1,1)=1.
          endif
          tc(1,1,1,1)=chem(i,k,j,p_dms)*1.d-6
          tc(1,1,1,2)=chem(i,k,j,p_so2)*1.d-6
          tc(1,1,1,3)=chem(i,k,j,p_sulf)*1.d-6
          tc(1,1,1,4)=chem(i,k,j,p_msa)*1.d-6
          airmas(1,1,1)=-(p8w(i,k+1,j)-p8w(i,k,j))*dx*dx/g
          airden(1,1,1)=rho_phy(i,k,j)
          tmp(1,1,1)=t_phy(i,k,j)
          oh(1,1,1)=chem(i,k,j,p_ho)*1.d-6
          h2o2(1,1,1)=chem(i,k,j,p_h2o2)*1.d-6
          xno3(1,1,1) = chem(i,k,j,p_no3)*1.d-6
          IF (COSSZA(1,1) > 0.0)xno3(1,1,1) = 0. 
!         if(i.eq.19.and.j.eq.19.and.k.eq.kts)then
!          write(0,*)backg_oh(i,k,j),backg_no3(i,k,j),ttday(i,j),tcosz(i,j)
!         endif

          call chmdrv_su( imx,jmx,lmx,&
               nmx, dt, tmp, airden, airmas, &
               oh, xno3, h2o2, cldf, tc, tdry,cossza,  &
               chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa, chpso4, &
               chlso2_oh, chlso2_aq)
          chem(i,k,j,p_dms)=tc(1,1,1,1)*1.e6
          chem(i,k,j,p_so2)=tc(1,1,1,2)*1.e6
          chem(i,k,j,p_sulf)=tc(1,1,1,3)*1.e6
          chem(i,k,j,p_msa)=tc(1,1,1,4)*1.e6
          chem(i,k,j,p_h2o2)=h2o2(1,1,1)*1.e6
       enddo
       enddo
       enddo
   END SELECT chem_select
end subroutine gocart_chem_driver

!SUBROUTINE chmdrv_su( &
!     imx, jmx, lmx, nmx, ndt1, tmp, drydf, airden, airmas, &
!     oh, xno3, h2o2, cldf, tc, tdry, depso2, depso4, depmsa, &
!     chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa, chpso4, &
!     chlso2_oh, chlso2_aq)

!We don't apply losses due to dry deposition here, this is done in vertical mixing
SUBROUTINE chmdrv_su( imx,jmx,lmx,&
     nmx, dt1, tmp, airden, airmas, &
     oh, xno3, h2o2, cldf, tc, tdry,cossza,  &
     chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa, chpso4, &
     chlso2_oh, chlso2_aq)

! ****************************************************************************
! **                                                                        **
! **  Chemistry subroutine.  For tracers with dry deposition, the loss      **
! **  rate of dry dep is combined in chem loss term.                        **
! **                                                                        **
! ****************************************************************************

! USE module_data_gocart
  
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nmx,imx,jmx,lmx
  integer :: ndt1
  real, intent(in) :: dt1
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: tmp, airden, airmas
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: oh, xno3, cldf
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: h2o2
!  REAL*8, INTENT(IN)    :: drydf(imx,jmx,nmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(INOUT) :: tdry(imx,jmx,nmx)
  real*8, DIMENSION (imx,jmx),INTENT(IN) :: cossza
!  REAL*8, DIMENSION(imx,jmx),     INTENT(INOUT) :: depso2, depso4, depmsa
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chldms_oh, chldms_no3
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chldms_x, chpso2, chpmsa
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chpso4, chlso2_oh, chlso2_aq

  REAL*8, DIMENSION(imx,jmx,lmx) :: pso2_dms, pmsa_dms, pso4_so2

  ! executable statements
  ndt1=ifix(dt1)
  if(ndt1.le.0)stop

     CALL chem_dms(imx,jmx,lmx,nmx, ndt1, tmp, airden, airmas, oh, xno3, &
          tc, chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa,cossza, &
          pso2_dms, pmsa_dms)
!     WRITE(*,*) 'after CHEM_DMS'
     CALL chem_so2(imx,jmx,lmx,nmx, ndt1, tmp, airden, airmas, &
          cldf, oh, h2o2, tc, tdry, cossza,&
          chpso4, chlso2_oh, chlso2_aq, pso2_dms, pso4_so2)
!          depso2, chpso4, chlso2_oh, chlso2_aq, pso2_dms, pso4_so2)
!     WRITE(*,*) 'after CHEM_SO2'
     CALL chem_so4(imx,jmx,lmx,nmx, ndt1, airmas, tc, tdry,cossza, &
          pso4_so2)
!          depso4, pso4_so2)
!     WRITE(*,*) 'after CHEM_SO4'
     CALL chem_msa(imx,jmx,lmx,nmx, ndt1, airmas, tc, tdry, cossza,&
          pmsa_dms)
!          depmsa, pmsa_dms)
!     WRITE(*,*) 'after CHEM_MSA'
  
END SUBROUTINE chmdrv_su

!=============================================================================
SUBROUTINE chem_dms( imx,jmx,lmx,&
     nmx, ndt1, tmp, airden, airmas, oh, xno3, &
     tc, chldms_oh, chldms_no3, chldms_x, chpso2, chpmsa,cossza, &
     pso2_dms, pmsa_dms)

! ****************************************************************************
! *                                                                          *
! *  This is DMS chemistry subroutine.                                       *
! *                                                                          *
! *  R1:    DMS + OH  -> a*SO2 + b*MSA                OH addition channel    *
! *         k1 = { 1.7e-42*exp(7810/T)*[O2] / (1+5.5e-31*exp(7460/T)*[O2] }  *
! *         a = 0.75, b = 0.25                                               *
! *                                                                          *
! *  R2:    DMS + OH  ->   SO2 + ...                  OH abstraction channel *
! *         k2 = 1.2e-11*exp(-260/T)                                         *
! *                                                                          *
! *     DMS_OH = DMS0 * exp(-(r1+r2)*NDT1)                                   *
! *         where DMS0 is the DMS concentration at the beginning,            *
! *         r1 = k1*[OH], r2 = k2*[OH].                                      *
! *                                                                          *
! *  R3:    DMS + NO3 ->   SO2 + ...                                         *
! *         k3 = 1.9e-13*exp(500/T)                                          *
! *                                                                          *
! *     DMS = DMS_OH * exp(-r3*NDT1)                                         *
! *         where r3 = k3*[NO3].                                             *
! *                                                                          *
! *  R4:    DMS + X   ->   SO2 + ...                                         *
! *         assume to be at the rate of DMS+OH and DMS+NO3 combined.         *
! *                                                                          *
! *  The production of SO2 and MSA here, PSO2_DMS and PMSA_DMS, are saved    *
! *  for use in CHEM_SO2 and CHEM_MSA subroutines as a source term.  They    *
! *  are in unit of MixingRatio/timestep.                                    *
! *                                                                          *
! **************************************************************************** 

  USE module_data_gocartchem

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nmx, ndt1,imx,jmx,lmx
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: tmp, airden, airmas
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: oh, xno3
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chldms_oh, chldms_no3
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chldms_x, chpso2, chpmsa
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(OUT)   :: pso2_dms, pmsa_dms
  real*8, DIMENSION (imx,jmx),INTENT(IN) :: cossza

  REAL*8, PARAMETER :: fx = 1.0 
  REAL*8, PARAMETER :: a = 0.75
  REAL*8, PARAMETER :: b = 0.25
  
  ! From D4: only 0.8 efficiency, also some goes to DMSO and lost. 
  ! So we assume 0.75 efficiency for DMS addtion channel to form    
  ! products.                                                       
  
  REAL*8, PARAMETER :: eff = 1.0
  ! -- Factor to convert AIRDEN from kgair/m3 to molecules/cm3: 
  REAL*8, PARAMETER :: f = 1000.0 / airmw * 6.022D23 * 1.0D-6
  INTEGER :: i, j, l
  REAL(KIND=8) :: tk, o2, dms0, rk1, rk2, rk3, dms_oh, dms, xoh, xn3, xx
  
  ! executable statements
  
  DO l = 1,lmx
!CMIC$ doall autoscope
     DO j = 1,jmx
        DO i = 1,imx
           
           tk = tmp(i,j,l)
           o2 = airden(i,j,l) * f * 0.21
           dms0 = tc(i,j,l,NDMS)
           
! ****************************************************************************
! *  (1) DMS + OH:  RK1 - addition channel;  RK2 - abstraction channel.      *
! ****************************************************************************

           rk1 = 0.0d0
           rk2 = 0.0d0
           rk3 = 0.0d0
           
           IF (oh(i,j,l) > 0.0) THEN
!             IF (TRIM(oh_units) == 'mol/mol') THEN
                 ! mozech: oh is in mol/mol
                 ! convert to molecules/cm3
                 rk1 = (1.7D-42 * EXP(7810.0/tk) * o2) / &
                      (1.0 + 5.5D-31 * EXP(7460.0/tk) * o2 ) * oh(i,j,l) * &
                      airden(i,j,l)*f
                 rk2 = 1.2D-11*EXP(-260.0/tk) * oh(i,j,l)*airden(i,j,l)*f
!             ELSE
!                rk1 = (1.7D-42 * EXP(7810.0/tk) * o2) / &
!                     (1.0 + 5.5D-31 * EXP(7460.0/tk) * o2 ) * oh(i,j,l)
!                rk2 = 1.2D-11*EXP(-260.0/tk) * oh(i,j,l) 
!             END IF
           END IF
           
! ****************************************************************************
! *  (2) DMS + NO3 (only happens at night):                                  *
! ****************************************************************************

           IF (cossza(i,j) <= 0.0) THEN

!             IF (TRIM(no3_units) == 'cm-3') THEN
!                ! IMAGES: XNO3 is in molecules/cm3.     
!                rk3 = 1.9D-13 * EXP(500.0/tk) * xno3(i,j,l)

!             ELSE
                 ! GEOSCHEM (mergechem) and mozech: XNO3 is in mol/mol (v/v)
                 ! convert xno3 from volume mixing ratio to molecules/cm3 
                 rk3 = 1.9D-13 * EXP(500.0/tk) * xno3(i,j,l) * &
                      airden(i,j,l) * f
!             END IF
              
           END IF

! ****************************************************************************
! *  Update DMS concentrations after reaction with OH and NO3, and also      *
! *  account for DMS + X assuming at a rate as (DMS+OH)*Fx in the day and    *
! *  (DMS+NO3)*Fx at night:                                                  *
! *       DMS_OH       :  DMS concentration after reaction with OH           *
! *       DMS          :  DMS concentration after reaction with NO3          *
! *                           (min(DMS) = 1.0E-32)                           *
! ****************************************************************************

           dms_oh = dms0   * EXP( -(rk1 + rk2) * fx * REAL(ndt1) )
           dms    = dms_oh * EXP( -(rk3) * fx * REAL(ndt1) )
           dms    = MAX(dms, 1.0D-32)
           
           tc(i,j,l,NDMS) = dms
           
! ****************************************************************************
! *  Save SO2 and MSA production from DMS oxidation                          * 
! *  (in MixingRatio/timestep):                                              *
! *                                                                          *
! *  SO2 is formed in DMS + OH addition (0.85) and abstraction (1.0)         *
! *      channels as well as DMS + NO3 reaction.  We also assume that        *
! *      SO2 yield from DMS + X is 1.0.                                      *
! *  MSA is formed in DMS + OH addition (0.15) channel.                      *
! ****************************************************************************
       
           IF ((rk1 + rk2) == 0.0) THEN
              pmsa_dms(i,j,l) = 0.0D0
           ELSE
!       pmsa_dms(i,j,l) = (dms0 - dms_oh) * b*rk1/((rk1+rk2)*fx)
              pmsa_dms(i,j,l) = max(0.0D0,(dms0 - dms_oh) * b*rk1/((rk1+rk2) * fx) * eff)
           END IF
       pso2_dms(i,j,l) =  max(0.0D0,dms0 - dms - pmsa_dms(i,j,l))
!      pso2_dms(i,j,l) =  (dms0 - dms - pmsa_dms(i,j,l)/eff) * eff

           !    ------------------------------------------------------------
           !    DIAGNOSTICS:      DMS loss       (kgS/timstep)          
           !                      SO2 production (kgS/timestep)         
           !                      MSA production (kgS/timestep)         
           !    ------------------------------------------------------------
           xoh  = (dms0   - dms_oh) / fx  * airmas(i,j,l)/airmw*smw
           xn3  = (dms_oh - dms)    / fx  * airmas(i,j,l)/airmw*smw
           xx   = (dms0 - dms) * airmas(i,j,l)/airmw*smw - xoh - xn3

           chldms_oh (i,j,l) = chldms_oh (i,j,l) + xoh
           chldms_no3(i,j,l) = chldms_no3(i,j,l) + xn3
           chldms_x  (i,j,l) = chldms_x  (i,j,l) + xx
           
           chpso2(i,j,l) = chpso2(i,j,l) + pso2_dms(i,j,l) &
                * airmas(i,j,l) / airmw * smw
           chpmsa(i,j,l) = chpmsa(i,j,l) + pmsa_dms(i,j,l) &
                * airmas(i,j,l) / airmw * smw
           
        END DO
     END DO
  END DO
  
END SUBROUTINE chem_dms
      
!=============================================================================

SUBROUTINE chem_so2( imx,jmx,lmx,&
     nmx, ndt1, tmp, airden, airmas, &
     cldf, oh, h2o2, tc, tdry, cossza,&
     chpso4, chlso2_oh, chlso2_aq, pso2_dms, pso4_so2)
!     depso2, chpso4, chlso2_oh, chlso2_aq, pso2_dms, pso4_so2)

! ****************************************************************************
! *                                                                          *
! *  This is SO2 chemistry subroutine.                                       *
! *                                                                          *
! *  SO2 production:                                                         *
! *    DMS + OH, DMS + NO3 (saved in CHEM_DMS)                               * 
! *                                                                          *
! *  SO2 loss:                                                               * 
! *    SO2 + OH  -> SO4                                                      *
! *    SO2       -> drydep (NOT USED IN WRF/CHEM                             *
! *    SO2 + H2O2 or O3 (aq) -> SO4                                          *
! *                                                                          *
! *  SO2 = SO2_0 * exp(-bt)                                                  *
! *      + PSO2_DMS/bt * [1-exp(-bt)]                                        *
! *    where b is the sum of the reaction rate of SO2 + OH and the dry       *
! *    deposition rate of SO2, PSO2_DMS is SO2 production from DMS in        *
! *    MixingRatio/timestep.                                                 *
! *                                                                          *
! *  If there is cloud in the gridbox (fraction = fc), then the aqueous      *
! *  phase chemistry also takes place in cloud. The amount of SO2 oxidized   *
! *  by H2O2 in cloud is limited by the available H2O2; the rest may be      *
! *  oxidized due to additional chemistry, e.g, reaction with O3 or O2       *
! *  (catalyzed by trace metal).                                             *
! *                                                                          *
! ****************************************************************************
  USE module_data_gocartchem

  IMPLICIT NONE

  INTEGER, INTENT(IN) ::  nmx, ndt1,imx,jmx,lmx
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: tmp, airden, airmas
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: cldf, oh
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: h2o2
  real*8, DIMENSION (imx,jmx),INTENT(IN) :: cossza
!  REAL*8, INTENT(IN)    :: drydf(imx,jmx,nmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(INOUT) :: tdry(imx,jmx,nmx)

!  REAL*8, DIMENSION(imx,jmx),     INTENT(INOUT) :: depso2
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(INOUT) :: chpso4, chlso2_oh, chlso2_aq
  REAL*8, INTENT(IN)  :: pso2_dms(imx,jmx,lmx)
  REAL*8, INTENT(OUT) :: pso4_so2(imx,jmx,lmx)

  REAL*8 ::  k0, kk, m, l1, l2, ld
  ! Factor to convert AIRDEN from kgair/m3 to molecules/cm3: 
  REAL*8, PARAMETER :: f  = 1000. / airmw * 6.022D23 * 1.0D-6
  REAL*8, PARAMETER :: ki = 1.5D-12
  INTEGER :: i, j, l
  REAL*8 :: so20, tk, f1, rk1, rk2, rk, rkt, so2_cd, fc, so2

  ! executable statements

  DO l = 1,lmx
     DO j = 1,jmx
        DO i = 1,imx
           
           so20 = tc(i,j,l,NSO2)

           ! RK1: SO2 + OH(g), in s-1 
           tk = tmp(i,j,l)
           k0 = 3.0D-31 * (300.0/tk)**3.3
           m  = airden(i,j,l) * f
           kk = k0 * m / ki
           f1 = ( 1.0+ ( LOG10(kk) )**2 )**(-1)
!          IF (TRIM(oh_units) == 'mol/mol') THEN 
              ! mozech: oh is in mol/mol
              ! convert to molecules/cm3
              rk1 = ( k0 * m / (1.0 + kk) ) * 0.6**f1 * &
                   oh(i,j,l)*airden(i,j,l)*f
!          ELSE
!             rk1 = ( k0 * m / (1.0 + kk) ) * 0.6**f1 * oh(i,j,l)
!          END IF
      
           ! RK2: SO2 drydep frequency, s-1 
!           IF (l == 1) THEN ! at the surface
!              rk2 = drydf(i,j,NSO2)
!           ELSE
              rk2 = 0.0
!           END IF
           
           rk  = (rk1 + rk2)
           rkt =  rk * REAL(ndt1)

! ****************************************************************************
! *  Update SO2 concentration after gas phase chemistry and deposition.      *
! ****************************************************************************

           IF (rk > 0.0) THEN
              so2_cd = so20 * EXP(-rkt) &
                   + pso2_dms(i,j,l) * (1.0 - EXP(-rkt)) / rkt
              l1     = (so20 - so2_cd + pso2_dms(i,j,l)) * rk1/rk
              IF (l == 1) THEN
                 ld    = (so20 - so2_cd + pso2_dms(i,j,l)) * rk2/rk
              ELSE
                 ld    = 0.0
              END IF
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

              IF (so2_cd > h2o2(i,j,l)) THEN
                 fc = fc * (h2o2(i,j,l)/so2_cd)
                 h2o2(i,j,l) = h2o2(i,j,l) * (1.0 - cldf(i,j,l))
              ELSE
                 h2o2(i,j,l) = h2o2(i,j,l) * &
                      (1.0 - cldf(i,j,l)*so2_cd/h2o2(i,j,l))
              END IF
              so2 = so2_cd * (1.0 - fc)
              ! Aqueous phase SO2 loss rate (MixingRatio/timestep): 
              l2  = so2_cd * fc 
           ELSE
              so2 = so2_cd
              l2 = 0.0
           END IF

           so2    = MAX(so2, 1.0D-32)
           tc(i,j,l,NSO2) = so2

! ****************************************************************************
! *  SO2 chemical loss rate  = SO4 production rate (MixingRatio/timestep).   *
! ****************************************************************************

           pso4_so2(i,j,l) = max(0.0D0,l1 + l2)

           !    ---------------------------------------------------------------
           !    DIAGNOSTICS:      SO2 gas-phase loss       (kgS/timestep)  
           !                      SO2 aqueous-phase loss   (kgS/timestep) 
           !                      SO2 dry deposition loss  (kgS/timestep) 
           !                      SO4 production           (kgS/timestep) 
           !    ---------------------------------------------------------------
           chlso2_oh(i,j,l) = chlso2_oh(i,j,l) &
                + l1 * airmas(i,j,l) / airmw * smw
           chlso2_aq(i,j,l) = chlso2_aq(i,j,l) &
                + l2 * airmas(i,j,l) / airmw * smw
           IF (l == 1) &
!                depso2(i,j) = depso2(i,j) + ld * airmas(i,j,l) / airmw * smw

           chpso4(i,j,l) = chpso4(i,j,l) + pso4_so2(i,j,l) &
                * airmas(i,j,l) / airmw * smw
           
        END DO
     END DO
  END DO

!  tdry(:,:,NSO2) = depso2(:,:)*tcmw(NSO2)/smw ! kg of SO2

END SUBROUTINE chem_so2

!=============================================================================

SUBROUTINE chem_so4( imx,jmx,lmx,&
     nmx, ndt1, airmas, tc, tdry, cossza,&
     pso4_so2)
!     depso4, pso4_so2)

! ****************************************************************************
! *                                                                          *
! *  This is SO4 chemistry subroutine.                                       *
! *                                                                          *
! *  The Only production is from SO2 oxidation (save in CHEM_SO2), and the   *
! *  only loss is dry depsition here.  Wet deposition will be treated in     *
! *  WETDEP subroutine.                                                      *
! *                                                                          *
! *  SO4 = SO4_0 * exp(-kt) + PSO4_SO2/kt * (1.-exp(-kt))                    *
! *    where k = dry deposition.                                             *
! *                                                                          *
! ****************************************************************************
  USE module_data_gocartchem

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nmx, ndt1,imx,jmx,lmx
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: airmas
!  REAL*8, INTENT(IN)    :: drydf(imx,jmx,nmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(INOUT) :: tdry(imx,jmx,nmx)

!  REAL*8, DIMENSION(imx,jmx), INTENT(INOUT) :: depso4
  REAL*8, INTENT(IN) :: pso4_so2(imx,jmx,lmx)
  real*8, DIMENSION (imx,jmx),INTENT(IN) :: cossza

  INTEGER :: i, j, l
  REAL*8 :: so40, rk, rkt, so4 

  ! executable statements

  DO l = 1,lmx
     DO j = 1,jmx
        DO i = 1,imx

           so40 = tc(i,j,l,NSO4)

           ! RK: SO4 drydep frequency, s-1 
!           IF (l == 1) THEN
!              rk  = drydf(i,j,NSO4)
!              rkt = rk * REAL(ndt1)
!
!              so4 = so40 * EXP(-rkt) + pso4_so2(i,j,l)/rkt * (1.0 - EXP(-rkt))
!           ELSE
              so4 = so40 + pso4_so2(i,j,l)
!           END IF
           if(pso4_so2(i,j,l).lt.0.)then
             write(0,*)'so4 routine, pso4 = ',pso4_so2(i,j,l),so4,so40
           endif

           so4    = MAX(so4, 1.0D-32)
           tc(i,j,l,NSO4) = so4

           !  -------------------------------------------------------------- 
           !  DIAGNOSTICS:      SO4 dry deposition  (kgS/timestep)      
           !  -------------------------------------------------------------- 
!           IF (l == 1) &
!                depso4(i,j) = depso4(i,j) + (so40 - so4 + pso4_so2(i,j,l)) &
!                * airmas(i,j,l) / airmw * smw
           
        END DO
     END DO
  END DO

 ! tdry(:,:,NSO4) = depso4(:,:)*tcmw(NSO4)/smw ! kg of SO4

END SUBROUTINE chem_so4

!=============================================================================

SUBROUTINE chem_msa( imx,jmx,lmx,&
     nmx, ndt1, airmas, tc, tdry, cossza,&
     pmsa_dms)
!     depmsa, pmsa_dms)

! ****************************************************************************
! *                                                                          *
! *  This is MSA chemistry subroutine.                                       *
! *                                                                          *
! *  The Only production is from DMS oxidation (save in CHEM_DMS), and the   *
! *  only loss is dry depsition here.  Wet deposition will be treated in     *
! *  WETDEP subroutine.                                                      *
! *                                                                          *
! *  MSA = MSA_0 * exp(-dt) + PMSA_DMS/kt * (1.-exp(-kt))                    *
! *    where k = dry deposition.                                             *
! *                                                                          *
! ****************************************************************************
  USE module_data_gocartchem

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: nmx, ndt1,imx,jmx,lmx
  REAL*8, DIMENSION(imx,jmx,lmx), INTENT(IN) :: airmas
  REAL*8, DIMENSION(imx,jmx), INTENT(IN) :: cossza
!  REAL, INTENT(IN)    :: drydf(imx,jmx,nmx)
  REAL*8, INTENT(INOUT) :: tc(imx,jmx,lmx,nmx)
  REAL*8, INTENT(INOUT) :: tdry(imx,jmx,nmx)
!  REAL, DIMENSION(imx,jmx), INTENT(INOUT) :: depmsa
  REAL*8, INTENT(IN) :: pmsa_dms(imx,jmx,lmx)

  REAL*8 :: msa0, msa, rk, rkt
  INTEGER :: i, j, l
  
  ! executable statements
  
  DO l = 1,lmx
     DO j = 1,jmx
        DO i = 1,imx

           msa0 = tc(i,j,l,NMSA)

           ! RK: MSA drydep frequency, s-1 
!           IF (l == 1) THEN
!              rk  = drydf(i,j,NMSA)
!              rkt = rk * REAL(ndt1)
!
!              msa = msa0 * EXP(-rkt) &
!                   + pmsa_dms(i,j,l)/rkt * (1.0 - EXP(-rkt))
!
!           ELSE
              msa = msa0 + pmsa_dms(i,j,l)
!           END IF

           msa    = MAX(msa, 1.0D-32)
           tc(i,j,l,NMSA) = msa
           
           !  -------------------------------------------------------------- 
           !  DIAGNOSTICS:      MSA dry deposition  (kgS/timestep)     
           !  -------------------------------------------------------------- 
!           IF (l == 1) &
!                depmsa(i,j) = depmsa(i,j) + (msa0 - msa + pmsa_dms(i,j,l)) &
!                * airmas(i,j,l) / airmw * smw

        END DO
     END DO
  END DO

!  tdry(:,:,NMSA) = depmsa(:,:)*tcmw(NMSA)/smw ! kg of MSA

END SUBROUTINE chem_msa
SUBROUTINE szangle(imx, jmx, doy, xhour, sza, cossza,xlon,rlat)

!
! ****************************************************************************
! **                                                                        **
! **  This subroutine computes solar zenith angle (SZA):                    **
! **                                                                        **
! **      cos(SZA) = sin(LAT)*sin(DEC) + cos(LAT)*cos(DEC)*cos(AHR)         **
! **                                                                        **
! **  where LAT is the latitude angle, DEC is the solar declination angle,  **
! **  and AHR is the hour angle, all in radius.                             **
! **                                                                        **
! **  DOY = day-of-year, XHOUR = UT time (hrs).                             **
! **  XLON = longitude in degree, RLAT = latitude in radian.                **
! ****************************************************************************
!

  IMPLICIT NONE

  INTEGER, INTENT(IN)    :: imx, jmx
  INTEGER, INTENT(IN)    :: doy
  REAL,    INTENT(IN)    :: xhour
  REAL,    INTENT(OUT)   :: sza(imx,jmx), cossza(imx,jmx)

  REAL    :: a0, a1, a2, a3, b1, b2, b3, r, dec, timloc, ahr,xlon,rlat
  real, parameter :: pi=3.14
  INTEGER :: i, j

  ! executable statements

  ! ***************************************************************************
  ! *  Solar declination angle:                                               *
  ! ***************************************************************************
  a0 = 0.006918
  a1 = 0.399912
  a2 = 0.006758
  a3 = 0.002697
  b1 = 0.070257
  b2 = 0.000907
  b3 = 0.000148
  r  = 2.0* pi * REAL(doy-1)/365.0
  !
  dec = a0 - a1*COS(  r)   + b1*SIN(  r)   &
           - a2*COS(2.0*r) + b2*SIN(2.0*r) &
           - a3*COS(3.0*r) + b3*SIN(3.0*r)
  !
  DO i = 1,imx
     ! ************************************************************************
     ! *  Hour angle (AHR) is a function of longitude.  AHR is zero at        *
     ! *  solar noon, and increases by 15 deg for every hour before or        *
     ! *  after solar noon.                                                   *
     ! ************************************************************************
     ! -- Local time in hours
     timloc  = xhour + xlon/15.0
     !      IF (timloc < 0.0) timloc = 24.0 + timloc
     IF (timloc > 24.0) timloc = timloc - 24.0
     !
     ! -- Hour angle
     ahr = ABS(timloc - 12.0) * 15.0 * pi/180.0
     !
     DO j = 1,jmx
        ! -- Solar zenith angle      
        cossza(i,j) = SIN(rlat) * SIN(dec) + &
                      COS(rlat) * COS(dec) * COS(ahr)
        sza(i,j)    = ACOS(cossza(i,j)) * 180.0/pi
        IF (cossza(i,j) < 0.0)   cossza(i,j) = 0.0
        !
     END do
  END DO
     
END subroutine szangle

END MODULE MODULE_GOCART_CHEM
