MODULE uoc_dust
!----------------------------------------------------------------------------
! Dust emission module developed at the University of Cologne, Germany. 
! Dust emission schemes and framework developed by Y Shao (yshao@uni-koeln.de)
! Implementation into WRF and modifications by JY Kang (jy.kang@kiaps.org), 
! CL Wu (wuchenglai@mail.iap.ac.cn), and M Klose (mklose@uni-koeln.de).
! 
! For references and available schemes, see module_qf03.F
! Martina Klose, 29 May 2013
! 
!----------------------------------------------------------------------------
  USE module_data_gocart_dust
  USE qf03
  USE module_soilpsd
  USE module_sf_noahlsm, ONLY:DRYSMC
  USE NOAHMP_TABLES, ONLY: DRYSMC_nmp => SMCDRY_TABLE
  USE module_sf_ruclsm,  ONLY:DRYSMC_ruc => DRYSMC
  
  CONTAINS
  subroutine uoc_dust_driver(ktau,dt,config_flags,                         &
         chem,rho_phy,dz8w,smois,ust,                                      &
         isltyp,vegfra,g,emis_dust,                                        &
         ust_t_min, imod, rough_cor, smois_cor,                            &
         soil_top_cat, erod,                                               &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
  USE module_configure
  USE module_state_description
  USE module_model_constants, ONLY: mwdry
  IMPLICIT NONE
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: ktau, imod,                              &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER, DIMENSION( ims:ime , jms:jme ) ,                               &
          INTENT(IN   ) ::                                 isltyp 

   REAL, DIMENSION(ims:ime,1:config_flags%num_soil_cat,jms:jme) ,          &
          INTENT(IN   ) ::                           soil_top_cat   
          
   REAL,  DIMENSION( ims:ime , jms:jme, 3 ) ,                              &
          INTENT(IN   ) ::                           erod 

! ust_t_min is calculated value from qf03
   REAL,  DIMENSION( ims:ime , jms:jme ) ,                                 &
          INTENT(INOUT) ::                           ust_t_min,            &
                                                     rough_cor,            &   
                                                     smois_cor
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_dust),OPTIONAL,           &
         INTENT(INOUT ) ::                              emis_dust
   REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,     &
         INTENT(INOUT) ::                               smois
   REAL,  DIMENSION( ims:ime , jms:jme )  ,                                &
          INTENT(IN   ) ::                            ust,                 &
                                                      vegfra
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ),                        &
          INTENT(IN   ) ::                           dz8w,                 &
                                                     rho_phy         
 
  REAL, INTENT(IN   ) :: dt,g
!
! local variables
!
  integer, parameter :: imax=100              ! No. of particle size intervals for psd
  integer, parameter :: jmax=4                ! No. of log-normal distributions for constructing psd
  integer, parameter :: stype=12               ! No. of soil texture classes
  real(8), dimension(0:imax) :: d0
  real(8), dimension(imax)   :: dd 
  real(8), dimension(imax)   :: psdm, dpsdm, ppsdm
  real(8), dimension(imax)   :: psdf, dpsdf, ppsdf
  real(8), dimension(imax,stype)   :: psd_m, dpsd_m, ppsd_m
  real(8), dimension(imax,stype)   :: psd_f, dpsd_f, ppsd_f
  real(8), parameter :: dcut=20.d0            ! dust cutoff particle size
  real(8), parameter :: rhop = 2650.d0        ! particle density [kg/m3]
  
  
  integer             :: i,j,k,p,idst
  integer, parameter  :: nmx = 5  ! No. of dust bins
  real                :: ust_grid, airden, dz_lowest, smc
  real, dimension(nmx):: tc, bems
  real(8)             :: gwet, cf
  real(8)             :: conver,converi
  real(8)             :: ust_min, rough_cor_in, smois_cor_in
  real, dimension(16) :: soilc
  real                :: tot_soilc
  integer             :: domsoilc
  integer             :: cc    
! 
  real(8)             :: ustart0_out, lambda, sigma
  real(8), dimension(imax) :: ustart
  real, dimension(12) :: thr
  data thr/0.001, 0.003, 0.037, 0.061, 0.072, 0.049, 0.084, 0.110, 0.095, 0.126, 0.141, 0.156/
  
!   character*4, dimension(12):: s_type
!   data s_type /'sand', 'losa', 'salo', 'silo', 'silt', 'loam', 'sclo', 'sicl', 'cloa', &
!   &            'sacl', 'silc', 'clay'/
! saturation soil moisture from SOILPARM.TBL
!***************************************************************************
! initialization
   
  conver=1.e-9
  converi=1.e9

  k=kts   !in the bottom layer
  
! calculate soil-psd once for all 12 types
! initialize
   psd_m(:,:) = 0.
   psd_f(:,:) = 0.
   dpsd_m(:,:) = 0.
   dpsd_f(:,:) = 0.
   ppsd_m(:,:) = 0.
   ppsd_f(:,:) = 0.

   do p = 1, stype
!     write(6,*) p
      if (p.eq.1) then            ! sand
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csandm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csandf, jmax)
      elseif (p.eq.2) then        ! loamy sand
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, closam, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, closaf, jmax)
      elseif (p.eq.3) then        ! sandy loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csalom, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csalof, jmax)
      elseif (p.eq.4) then        ! silt loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csilom, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csilof, jmax)
      elseif (p.eq.5) then        ! silt
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csiltm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csiltf, jmax)
      elseif (p.eq.6) then        ! loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, cloamm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, cloamf, jmax)
      elseif (p.eq.7) then        ! sandy clay loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csclom, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csclof, jmax)
      elseif (p.eq.8) then        ! silty clay loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csiclm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csiclf, jmax)
      elseif (p.eq.9) then        ! clay loam
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, ccloam, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, ccloaf, jmax)
      elseif (p.eq.10) then        ! sandy clay
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csaclm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csaclf, jmax)
      elseif (p.eq.11) then        ! silty clay
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, csilcm, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, csilcf, jmax)         
      elseif (p.eq.12) then        ! clay
         call psd_create(d0, dd, psdm, dpsdm, ppsdm, imax, cclaym, jmax)
         call psd_create(d0, dd, psdf, dpsdf, ppsdf, imax, cclayf, jmax)       
      endif
         psd_m(:,p) = psdm(:)
         psd_f(:,p) = psdf(:)
         dpsd_m(:,p) = dpsdm(:)
         dpsd_f(:,p) = dpsdf(:)
         ppsd_m(:,p) = ppsdm(:)
         ppsd_f(:,p) = ppsdf(:)      
   enddo  
!    stop
!    
! Before calculating dust emission, some parameters should be set.
   j = 0
1  j = j+1
   if ( dd(j) .le. dcut ) then
      idst = j
      goto 1
   endif
! 
  do j=jts,jte
  do i=its,ite

! do dust over dust source area only
     if (sum(erod(i,j,:)).gt.0.) then    !use fraction of erodible surface area as dust source indicator    
        tc(1)=chem(i,kts,j,p_dust_1)*conver   ![kg/kg-dryair]
        tc(2)=chem(i,kts,j,p_dust_2)*conver
        tc(3)=chem(i,kts,j,p_dust_3)*conver
        tc(4)=chem(i,kts,j,p_dust_4)*conver
        tc(5)=chem(i,kts,j,p_dust_5)*conver

        ust_grid=ust(i,j)   ! u* for one grid

! delta z for the lowest layer is needed for unit conversion
        dz_lowest = dz8w(i,1,j)
 
! for soil moisture using volumetric soil moisture (smois)
        if (smois(i,1,j).gt.1.) then 
           smois(i,1,j) = 1.
           CALL wrf_message('UoC CTDE WARNING: vol. soil moisture > 1, reset')
        endif 

        gwet=smois(i,1,j)
        airden=rho_phy(i,kts,j)                       ![kg/m3]
        cf=vegfra(i,j)                                ! in [%]
        cf = cf/100.d0                                ! fraction 

! initialization
         tot_soilc=0.
         soilc = 0.

         do cc = 1, 12 
            soilc(cc) = soil_top_cat(i,cc,j)
            tot_soilc = tot_soilc + soilc(cc)
         enddo
!
         domsoilc = isltyp(i,j)
         if (  config_flags%sf_surface_physics .eq. 3   ) then
             DRYSMC = DRYSMC_ruc
         elseif (  config_flags%sf_surface_physics .eq. 4   ) then
             DRYSMC = DRYSMC_nmp                       
         elseif ( config_flags%sf_surface_physics .eq. 1 .or. & 
         &        config_flags%sf_surface_physics .eq. 5 .or. & 
         &        config_flags%sf_surface_physics .eq. 7 .or. & 
         &        config_flags%sf_surface_physics .eq. 8 .or. & 
         &        config_flags%sf_surface_physics .eq. 0) then 
             DRYSMC(1:12) = thr             
             CALL wrf_message('UoC dust: DRYSMC reset for dust emission')
         endif           
!------------------------------------------------------------------------
         call qf03_driver( nmx, idst, g, rhop, airden, dt,        &
                  ust_grid, gwet, cf, ust_min, imod, dz_lowest, &
                  soilc, tot_soilc, domsoilc,                   &
                  tc, bems, rough_cor_in, smois_cor_in, DRYSMC(1:12),    & 
                  d0, dd, psd_m, dpsd_m, ppsd_m, psd_f, dpsd_f, ppsd_f, imax, stype)
                  
!                   
         chem(i,kts,j,p_dust_1)=tc(1)*converi   ![ug/kg-dryair]
         chem(i,kts,j,p_dust_2)=tc(2)*converi
         chem(i,kts,j,p_dust_3)=tc(3)*converi
         chem(i,kts,j,p_dust_4)=tc(4)*converi
         chem(i,kts,j,p_dust_5)=tc(5)*converi
! for output diagnostics
         emis_dust(i,1,j,p_edust1)=bems(1)*converi 
         emis_dust(i,1,j,p_edust2)=bems(2)*converi
         emis_dust(i,1,j,p_edust3)=bems(3)*converi 
         emis_dust(i,1,j,p_edust4)=bems(4)*converi
         emis_dust(i,1,j,p_edust5)=bems(5)*converi      ![kg/m2/s] -> [ug/m2/s] 

     else   ! no dust source
         emis_dust(i,1,j,p_edust1)=0.
         emis_dust(i,1,j,p_edust2)=0.
         emis_dust(i,1,j,p_edust3)=0.
         emis_dust(i,1,j,p_edust4)=0.
         emis_dust(i,1,j,p_edust5)=0.
!          if (imod .ne. 4) then
            ust_min = -9999.d0
            rough_cor_in = 1.d0
            smois_cor_in = 1.d0
!          endif
     endif !dsource/erod

        ust_t_min(i,j) = ust_min
        rough_cor(i,j) = rough_cor_in
        smois_cor(i,j) = smois_cor_in
  enddo ! i loop
  enddo ! j loop
!

end subroutine UoC_dust_driver


!*****************************************************************************
      subroutine psd_create(d, dm, psd, dpsd, ppsd, imax, cmtrix, jmax)
!
!----------------------------------------------------------------------------
! Yaping Shao, 13 June 2000
!
! - Generate particle size distribution density function 
!   (both minimally-dispersed and fully-dispersed as the 
!   sum of four log-normal distributions.
!
! d(0,imax):	output, particle size at 0, 1, 2, ..., imax points	[um]
! dm(imax):	output, particle size at middle of 0-1, 1-2, etc      [um]
! psd(imax):    output, particle size distribution density at dm	[um^-1]
! dpsd(imax):   output, Delta P for sections 0-1, 1-2, etc.  		[ ]
! ppsd(imax):   output, P for sections 0-1, 1-2, etc.  		[ ]
! imax:         input, length dm, psd, dpsdm, ppsd, etc.
! cmtrix: 	jmaxx coefficient matrix 
!               e.g.
!		w1   = cmtrix(1, 1): weight for first log-normal distribution
!               dln1 = cmtrix(2, 1): mean log-particle size of first log-normal distribution 	
!               sig1 = cmtrix(3, 1): sigma of log-particle size for first log-normal distribution
!		etc. 
!		careful with the dimension of dln and sig
!----------------------------------------------------------------------------
!
      integer :: i, j, imax, jmax	
      real(8), dimension(3, jmax) :: cmtrix
      real(8) :: d(0:imax), dm(imax)                 
      real(8) :: psd(imax), dpsd(imax), ppsd(imax)   ! for p(d), Delta P(d) and P(d)
      real(8) :: p, pp, w, dln, sig
      real(8) :: cn
      real(8), parameter :: eps=1.d-7
      real(8), parameter :: dref=1000.d0
      real(8) :: fu, fd, phi
!
      cn = 1.d0/dsqrt(2.d0*3.14159d0)
!
! initialise psd, dpsd, ppsd
!
      psd = 0.d0
      dpsd = 0.d0
      ppsd = 0.d0
!
! Estimate d using phi scale. phi varies between from 9 to -1
! with increment 0.1. Reference particle size d0 = 1000 um
!
      fu = 10.d0
      fd = -1.d0
      do i = 0, imax
	phi = fu - i*(fu-fd)/imax
	d(i) = dref/2.d0**phi
      enddo 	
!
      do i = 1, imax
        dm(i) = (d(i)-d(i-1))/dlog(d(i)/d(i-1))           ! d = delta(d) / delta(log d), because del (log d) = (del d) / d   ! mklose [17122014]

        pp = 0.d0
	do j = 1, jmax
	  w = cmtrix(1, j)
          dln = cmtrix(2, j)
          sig = cmtrix(3, j)
	  if ( (w.gt.eps) .and. (sig.ne.0.) ) then
	    p = w*cn/sig*dexp( -(dlog(dm(i))-dln)**2/(2*sig**2) )
          else
            p=0.d0	
          endif
	  pp = pp + p
	enddo
!
	dpsd(i) = pp*( dlog(d(i)) - dlog(d(i-1)) )        ! Delta P over i
	if (i.eq.1) then 
          ppsd(i) = 0.d0 + dpsd(i)                        ! P(d), with P(0) = 0
	else
          ppsd(i) = ppsd(i-1) + dpsd(i)      
	endif
        psd(i) = pp/dm(i)                                 ! p(d), particle size distribution density

      enddo
!
! Normalize in case ppsd(imax) is not 1
!
      dpsd = dpsd/ppsd(imax)
      psd  = psd/ppsd(imax)
      ppsd = ppsd/ppsd(imax)

!
     end subroutine
!*****************************************************************************

END MODULE uoc_dust
