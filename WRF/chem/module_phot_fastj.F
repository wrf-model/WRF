!************************************************************************
! This computer software was prepared by Battelle Memorial Institute,
! hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830 with
! the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
! CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
! LIABILITY FOR THE USE OF THIS SOFTWARE.
!
! Photolysis Option:  Fast-J
! * Primary investigators: Elaine G. Chapman and James C. Barnard
! * Co-investigators: Jerome D. Fast, William I. Gustafson Jr.
! Last update: February 2009
!
! Contact:
! Jerome D. Fast, PhD
! Staff Scientist
! Pacific Northwest National Laboratory
! P.O. Box 999, MSIN K9-30
! Richland, WA, 99352
! Phone: (509) 372-6116
! Email: Jerome.Fast@pnl.gov
!
! The original Fast-J code was provided by Oliver Wild (Univ. of Calif.
! Irvine); however, substantial modifications were necessary to make it
! compatible with WRF-Chem and to include the effect of prognostic
! aerosols on photolysis rates.
!
! Please report any bugs or problems to Jerome Fast, the WRF-chem
! implmentation team leader for PNNL.
!
! References: 
! * Wild, O., X. Zhu, M.J. Prather, (2000), Accurate simulation of in-
!   and below cloud photolysis in tropospheric chemical models, J. Atmos.
!   Chem., 37, 245-282.
! * Barnard, J.C., E.G. Chapman, J.D. Fast, J.R. Schmelzer, J.R.
!   Schulsser, and R.E. Shetter (2004), An evaluation of the FAST-J
!   photolysis model for predicting nitrogen dioxide photolysis rates
!   under clear and cloudy sky conditions, Atmos. Environ., 38,
!   3393-3403.
! * Fast, J.D., W.I. Gustafson Jr., R.C. Easter, R.A. Zaveri, J.C.
!   Barnard, E.G. Chapman, G.A. Grell, and S.E. Peckham (2005), Evolution
!   of ozone, particulates, and aerosol direct radiative forcing in the
!   vicinity of Houston using a fully-coupled meteorology-chemistry-
!   aerosol model, J. Geophys. Res., 111, D21305,
!   doi:10.1029/2005JD006721.
! * Barnard, J.C., J.D. Fast, G. Paredes-Miranda, W.P. Arnott, and
!   A. Laskin (2010), Technical note: evaluation of the WRF-Chem
!   "aerosol chemical to aerosol optical properties" module using data
!   from the MILAGRO campaign, Atmos. Chem. Phys., 10, 7325-7340,
!   doi:10.5194/acp-10-7325-2010.
!
! Contact Jerome Fast for updates on the status of manuscripts under
! review.
!
! Additional information:
! *  www.pnl.gov/atmospheric/research/wrf-chem
!
! Support:
! Funding for adapting Fast-J was provided by the U.S. Department of
! Energy under the auspices of Atmospheric Science Program of the Office
! of Biological and Environmental Research the PNNL Laboratory Research
! and Directed Research and Development program.
!************************************************************************

!WRF:MODEL_LAYER:CHEMISTRY
!
	module module_phot_fastj
	integer, parameter :: lunerr = -1

	contains
!***********************************************************************
       subroutine fastj_driver(id,curr_secs,dtstep,config_flags,       &
               gmt,julday,t_phy,moist,p8w,p_phy,                       &
               chem,rho_phy,dz8w,xlat,xlong,z_at_w,                    &
               ph_o2,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,  &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ph_n2o5,                                                &
               tauaer1,tauaer2,tauaer3,tauaer4,                        &
               gaer1,gaer2,gaer3,gaer4,                                &
               waer1,waer2,waer3,waer4,                                &
               bscoef1,bscoef2,bscoef3,bscoef4,                        &
               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                    &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )


   USE module_configure
   USE module_state_description
   USE module_data_mosaic_therm, only: nbin_a, nbin_a_maxd
!  USE module_data_mosaic_asect
   USE module_data_mosaic_other, only: kmaxd, nsubareas
   USE module_fastj_mie
!  USE module_mosaic_therm, only: aerosol_optical_properties
!  USE module_mosaic_driver, only: mapaer_tofrom_host
   USE module_fastj_data, only:  nb, nc

   implicit none
!jdf
!  integer, parameter :: iprint = 0
   integer, parameter :: single = 4        !compiler dependent value real*4
   integer, parameter :: double = 8        !compiler dependent value real*8
   integer,parameter :: ipar_fastj=1,jpar=1
   integer,parameter :: jppj=14        !Number of photolytic reactions supplied
   logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
   integer,save :: lpar           !Number of levels in CTM
   integer,save :: jpnl           !Number of levels requiring chemistry
   real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
   real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
   real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
   real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
   real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
   real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
   integer month_fastj        !  Number of month (1-12)
   integer iday_fastj         !  Day of year
   integer nspint           ! Num of spectral intervals across solar 
   parameter ( nspint = 4 ) ! spectrum for FAST-J
   real, dimension (nspint),save :: wavmid !cm
   real, dimension (nspint, kmaxd+1),save :: sizeaer,extaer,waer,gaer,tauaer,bscoef
   real, dimension (nspint, kmaxd+1),save :: l2,l3,l4,l5,l6,l7
   data wavmid     &
       / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
   integer nphoto_fastj
   parameter (nphoto_fastj = 14)
   integer   &
   lfastj_no2,   lfastj_o3a,   lfastj_o3b,    lfastj_h2o2,   &
   lfastj_hchoa, lfastj_hchob, lfastj_ch3ooh, lfastj_no3x,   &
   lfastj_no3l,  lfastj_hono,  lfastj_n2o5,   lfastj_hno3,   &
   lfastj_hno4     
   parameter( lfastj_no2   = 1 )
   parameter( lfastj_o3a   = 2 )
   parameter( lfastj_o3b   = 3 )
   parameter( lfastj_h2o2  = 4 )
   parameter( lfastj_hchoa = 5 )
   parameter( lfastj_hchob = 6 )
   parameter( lfastj_ch3ooh= 7 )
   parameter( lfastj_no3x  = 8 )
   parameter( lfastj_no3l  = 9 )
   parameter( lfastj_hono  = 10 )
   parameter( lfastj_n2o5  = 11 )
   parameter( lfastj_hno3  = 12 )
   parameter( lfastj_hno4  = 13 )
!jdf
   INTEGER,      INTENT(IN   ) :: id,julday,                    &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(INOUT ) ::                                          &
           ph_o2,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,     &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,          &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob, &
           ph_n2o5
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                    &
         INTENT(IN ) ::                                             &
           tauaer1,tauaer2,tauaer3,tauaer4,                         &
           gaer1,gaer2,gaer3,gaer4,                                 &
           waer1,waer2,waer3,waer4,                                 &
           bscoef1,bscoef2,bscoef3,bscoef4                          
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, 1:4 ),               &
         INTENT(IN ) ::                                             &
           l2aer,l3aer,l4aer,l5aer,l6aer,l7aer                      

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                      p_phy,    &
                                                       dz8w,    &
                                                        p8w,    &
                                                    rho_phy,    &
                                                      z_at_w
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &     
          INTENT(IN   ) ::                             xlat,    &
                                                      xlong
   REAL,      INTENT(IN   ) ::                                  &
                                                 dtstep,gmt

   TYPE(grid_config_rec_type), INTENT(IN  ) :: config_flags


!local variables

      integer iclm, jclm

      real number_bin(nbin_a_maxd,kmaxd) !These have to be the full kmaxd to
      real radius_wet(nbin_a_maxd,kmaxd) !match arrays in MOSAIC routines.
      complex refindx(nbin_a_maxd,kmaxd)

      integer(kind=8) :: ixhour
      integer i,j, k, nsub
	  real(kind=8) :: xtime, xhour
      real xmin, gmtp, tmidh
      real sla, slo
      real psfc

      real cos_sza
      real, dimension(kts:kte) :: temp, ozone, dz 
      real, dimension(0:kte) :: pbnd
      real, dimension(kts:kte) :: cloudmr, airdensity, relhum
      real, dimension(kts:kte+1) :: zatw
 	     
      real valuej(kte,nphoto_fastj)

      logical processingAerosols

!   set "pegasus" grid size variables
!	itot = ite
!	jtot = jte
    lpar = kte
    jpnl = kte
    nb   = lpar + 1 !for module module_fastj_data
    nc   = 2*nb     !ditto, and don't confuse this with nc in module_fastj_mie
	nsubareas = 1
	if ((kte > kmaxd) .or. (lpar <= 0)) then
	    write( wrf_err_message, '(a,4i5)' )   &
		'*** subr fastj_driver -- ' //   &
		'lpar, kmaxd, kts, kte', lpar, kmaxd, kts, kte
        call wrf_message( trim(wrf_err_message) )
	    wrf_err_message = '*** subr fastj_driver -- ' //   &
             'kte>kmaxd OR lpar<=0'
	    call wrf_error_fatal( wrf_err_message )
	end if

! Determine if aerosol data is provided in the chem array. Currently,
! only MOSAIC will work. The Mie routine does not know how to handle
! SORGAM aerosols.
    select case (config_flags%chem_opt)
    case ( CBMZ_MOSAIC_4BIN,    CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_KPP,  &
           CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
           CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, &
           CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
           MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP, &
           CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP, &!BSINGH(12/05/2013): Added for SAPRC 8 bin vbs
           SAPRC99_MOSAIC_8BIN_VBS2_KPP )!BSINGH(04/07/2014): Added for SAPRC 8 bin vbs non-aq
       processingAerosols = .true.
    case default
       processingAerosols = .false.
    end select

! Set nbin_a = ntot_amode and check nbin_a <= nbin_a_maxd.
! This duplicates the same assignment and check in module_mosaic_therm.F,
! but photolysis is called before aeorosols so this must be set too.
!
! rce 2004-dec-07 -- nbin_a is initialized elsewhere
!!$      nbin_a = ntot_amode
!!$      if ((nbin_a .gt. nbin_a_maxd) .or. (nbin_a .le. 0)) then
!!$          write( wrf_err_message, '(a,2(1x,i4))' )   &
!!$              '*** subr fastj_driver -- nbin_a, nbin_a_maxd =',   &
!!$              nbin_a, nbin_a_maxd
!!$          call wrf_message( wrf_err_message )
!!$          call wrf_error_fatal(   &
!!$              '*** subr fastj_driver -- BAD VALUE for nbin_a' )
!!$      end if

! determine current time of day in Greenwich Mean Time at middle 
! of current time step, tmidh.  do this by computing the number of minutes
! from beginning of run to middle of current time step
    xtime    = curr_secs/60._8 + real(dtstep,8)/120._8
    ixhour   = int(gmt + 0.01,8) + int(xtime/60._8,8)
    xhour    = real(ixhour,8)	!current hour
    xmin     = 60.*gmt + real(xtime-xhour*60_8,8)
    gmtp     = mod(xhour,24._8)
    tmidh    = gmtp + xmin/60.

! execute for each i,j column and each nsub subarea
    do nsub = 1, nsubareas
	do jclm = jts, jte
	do iclm = its, ite

       do k = kts, lpar
          dz(k) = dz8w(iclm, k, jclm)	! cell depth (m)
       end do

       if( processingAerosols ) then
         do k = kts, lpar
           l2(1,k)=l2aer(iclm,k,jclm,1)
           l2(2,k)=l2aer(iclm,k,jclm,2)
           l2(3,k)=l2aer(iclm,k,jclm,3)
           l2(4,k)=l2aer(iclm,k,jclm,4)
           l3(1,k)=l3aer(iclm,k,jclm,1)
           l3(2,k)=l3aer(iclm,k,jclm,2)
           l3(3,k)=l3aer(iclm,k,jclm,3)
           l3(4,k)=l3aer(iclm,k,jclm,4)
           l4(1,k)=l4aer(iclm,k,jclm,1)
           l4(2,k)=l4aer(iclm,k,jclm,2)
           l4(3,k)=l4aer(iclm,k,jclm,3)
           l4(4,k)=l4aer(iclm,k,jclm,4)
           l5(1,k)=l5aer(iclm,k,jclm,1)
           l5(2,k)=l5aer(iclm,k,jclm,2)
           l5(3,k)=l5aer(iclm,k,jclm,3)
           l5(4,k)=l5aer(iclm,k,jclm,4)
           l6(1,k)=l6aer(iclm,k,jclm,1)
           l6(2,k)=l6aer(iclm,k,jclm,2)
           l6(3,k)=l6aer(iclm,k,jclm,3)
           l6(4,k)=l6aer(iclm,k,jclm,4)
           l7(1,k)=l7aer(iclm,k,jclm,1)
           l7(2,k)=l7aer(iclm,k,jclm,2)
           l7(3,k)=l7aer(iclm,k,jclm,3)
           l7(4,k)=l7aer(iclm,k,jclm,4)
         enddo
! take chem data and extract 1 column to create rsub(l,k,m) array	  
!         call mapaer_tofrom_host( 0,                     &
!              ims,ime, jms,jme, kms,kme,                    &
!              its,ite, jts,jte, kts,kte,                    &
!              iclm, jclm, kts,lpar,                         &
!              num_moist, num_chem, moist, chem,             &
!              t_phy, p_phy, rho_phy                         )
! generate aerosol optical properties for cells in this column
! subroutine is located in file module_mosaic_therm
!         call aerosol_optical_properties(iclm, jclm, lpar, refindx, &
!              radius_wet, number_bin)
! execute mie code , located in file module_fastj_mie
!         CALL wrf_debug(250,'fastj_driver: calling mieaer')
!         call mieaer(                            &
!              id, iclm, jclm, nbin_a,            &
!              number_bin, radius_wet, refindx,   &
!              dz, curr_secs, lpar,               &
!              sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7,bscoef)
       end if

! set lat, long
	  sla = xlat(iclm,jclm)
	  slo = xlong(iclm,jclm)
! set column pressures, temperature, and ozone
	  psfc = p8w(iclm,1,jclm) * 10. ! convert pascals to dynes/cm2
	  do k = kts, lpar
	    pbnd(k) = p8w(iclm,k+1,jclm) *10.  ! convert pascals to dynes/cm2
	    temp(k) = t_phy(iclm,k,jclm)
	    ozone(k) = chem(iclm,k,jclm,p_o3) / 1.0e6	! ppm->mol/mol air
        cloudmr(k) = moist(iclm,k,jclm,p_qc)/0.622
        airdensity(k) = rho_phy(iclm,k,jclm)/28.966e3
        relhum(k) = MIN( .95, moist(iclm,k,jclm,p_qv) / &
                       (3.80*exp(17.27*(t_phy(iclm,k,jclm)-273.)/ &
                       (t_phy(iclm,k,jclm)-36.))/(.01*p_phy(iclm,k,jclm))))
        relhum(k) = MAX(.001,relhum(k))
        zatw(k)=z_at_w(iclm,k,jclm)
	  end do
      zatw(lpar+1)=z_at_w(iclm,lpar+1,jclm)
! call interface_fastj
	  CALL wrf_debug(250,'fastj_driver: calling interface_fastj')
	  call interface_fastj(tmidh,sla,slo,julday,           &
           pbnd, psfc, temp, ozone,                        &
           dz, cloudmr, airdensity, relhum, zatw,          &
           iclm, jclm, lpar, jpnl,                         &
           curr_secs, valuej, cos_sza, processingAerosols, &
           sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
! put column photolysis rates (valuej) into wrf photolysis (i,k,j) arrays
      CALL wrf_debug(250,'fastj_driver: calling mapJrates_tofrom_host')
      call mapJrates_tofrom_host( 0,                         	    &
           ims,ime, jms,jme, kms,kme,                    		    &
           its,ite, jts,jte, kts,kte,                    		    &
           iclm, jclm, kts,lpar,                 		            &
           valuej,  						                        &
           ph_o2,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,   	&
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,        	&
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob, &
           ph_n2o5                                                  )
! put the aerosol optical properties into the wrf arrays (this is hard-
! coded to 4 spectral bins, nspint)
!     do k=kts,kte
!        tauaer1(iclm,k,jclm) = tauaer(1,k)
!        tauaer2(iclm,k,jclm) = tauaer(2,k)
!        tauaer3(iclm,k,jclm) = tauaer(3,k)
!        tauaer4(iclm,k,jclm) = tauaer(4,k)
!        gaer1(iclm,k,jclm)   = gaer(1,k)
!        gaer2(iclm,k,jclm)   = gaer(2,k)
!        gaer3(iclm,k,jclm)   = gaer(3,k)
!        gaer4(iclm,k,jclm)   = gaer(4,k)
!        waer1(iclm,k,jclm)   = waer(1,k)
!        waer2(iclm,k,jclm)   = waer(2,k)
!        waer3(iclm,k,jclm)   = waer(3,k)
!        waer4(iclm,k,jclm)   = waer(4,k)
!        bscoef1(iclm,k,jclm) = bscoef(1,k)
!        bscoef2(iclm,k,jclm) = bscoef(2,k)
!        bscoef3(iclm,k,jclm) = bscoef(3,k)
!        bscoef4(iclm,k,jclm) = bscoef(4,k)
!     end do

    end do
    end do
    end do
    
    return
    end subroutine  fastj_driver	     

!-----------------------------------------------------------------------
	subroutine mapJrates_tofrom_host( iflag,                        &
		ims,ime, jms,jme, kms,kme,                    		        &
		its,ite, jts,jte, kts,kte,                    		        &
        iclm, jclm, ktmaps,ktmape,              		            &
        valuej,  						                            &
        ph_o2,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,      &
        ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,   	    &
        ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,        	    &
        ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
        ph_n2o5                                                     )

	USE module_data_cbmz

   implicit none
!jdf
   integer nphoto_fastj
   parameter (nphoto_fastj = 14)
   integer   &
   lfastj_no2,   lfastj_o3a,   lfastj_o3b,    lfastj_h2o2,   &
   lfastj_hchoa, lfastj_hchob, lfastj_ch3ooh, lfastj_no3x,   &
   lfastj_no3l,  lfastj_hono,  lfastj_n2o5,   lfastj_hno3,   &
   lfastj_hno4     
   parameter( lfastj_no2   = 1 )
   parameter( lfastj_o3a   = 2 )
   parameter( lfastj_o3b   = 3 )
   parameter( lfastj_h2o2  = 4 )
   parameter( lfastj_hchoa = 5 )
   parameter( lfastj_hchob = 6 )
   parameter( lfastj_ch3ooh= 7 )
   parameter( lfastj_no3x  = 8 )
   parameter( lfastj_no3l  = 9 )
   parameter( lfastj_hono  = 10 )
   parameter( lfastj_n2o5  = 11 )
   parameter( lfastj_hno3  = 12 )
   parameter( lfastj_hno4  = 13 )
!jdf
   INTEGER,      INTENT(IN   ) :: iflag,                        &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte, 	&
                                  iclm, jclm, ktmaps, ktmape
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                   &
         INTENT(INOUT ) ::                                         &
           ph_o2,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,  &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
           ph_n2o5

   REAL, DIMENSION( kte,nphoto_fastj ), INTENT(INOUT) :: valuej

! local variables
	real ft
	integer kt

	ft = 60.

    if (iflag .gt. 0) go to 2000
! flag is <=0, put pegasus column J rates (in 1/sec) into WRF arrays (in 1/min)
	do kt = ktmaps, ktmape
	  ph_no2(iclm,kt,jclm)       = valuej(kt,lfastj_no2) * ft
	  ph_no3o(iclm,kt,jclm)      = valuej(kt,lfastj_no3x) * ft
	  ph_no3o2(iclm,kt,jclm)     = valuej(kt,lfastj_no3l) * ft
	  ph_o33p(iclm,kt,jclm)      = valuej(kt,lfastj_o3a) * ft
	  ph_o31d(iclm,kt,jclm)      = valuej(kt,lfastj_o3b) * ft
	  ph_hno2(iclm,kt,jclm)      = valuej(kt,lfastj_hono) * ft
	  ph_hno3(iclm,kt,jclm)      = valuej(kt,lfastj_hno3) * ft
	  ph_hno4(iclm,kt,jclm)      = valuej(kt,lfastj_hno4) * ft
	  ph_h2o2(iclm,kt,jclm)      = valuej(kt,lfastj_h2o2) * ft
	  ph_ch3o2h(iclm,kt,jclm)    = valuej(kt,lfastj_ch3ooh) * ft
	  ph_ch2or(iclm,kt,jclm)     = valuej(kt,lfastj_hchoa) * ft
	  ph_ch2om(iclm,kt,jclm)     = valuej(kt,lfastj_hchob) * ft
	  ph_n2o5(iclm,kt,jclm)      = valuej(kt,lfastj_n2o5) * ft

	  ph_o2(iclm,kt,jclm)        = 0.0
	  ph_ch3cho(iclm,kt,jclm)    = 0.0
	  ph_ch3coch3(iclm,kt,jclm)  = 0.0
	  ph_ch3coc2h5(iclm,kt,jclm) = 0.0
	  ph_hcocho(iclm,kt,jclm)    = 0.0
	  ph_ch3cocho(iclm,kt,jclm)  = 0.0
	  ph_hcochest(iclm,kt,jclm)  = 0.0
	  ph_ch3coo2h(iclm,kt,jclm)  = 0.0
	  ph_ch3ono2(iclm,kt,jclm)   = 0.0
	  ph_hcochob(iclm,kt,jclm)   = 0.0

	end do
	return 	!finished peg-> wrf mapping

2000	continue
! iflag > 0 ; put wrf ph_xxx Jrates (1/min) into pegasus column	valuej (1/sec)
 	do kt = ktmaps, ktmape
	  valuej(kt,lfastj_no2)    = ph_no2(iclm,kt,jclm) /  ft
	  valuej(kt,lfastj_no3x)   = ph_no3o(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_no3l)   = ph_no3o2(iclm,kt,jclm)/ ft
	  valuej(kt,lfastj_o3a)    = ph_o33p(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_o3b)    = ph_o31d(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_hono)   = ph_hno2(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_hno3)   = ph_hno3(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_hno4)   = ph_hno4(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_h2o2)   = ph_h2o2(iclm,kt,jclm) / ft
	  valuej(kt,lfastj_ch3ooh) = ph_ch3o2h(iclm,kt,jclm)/ft
	  valuej(kt,lfastj_hchoa)  = ph_ch2or(iclm,kt,jclm)/ ft
	  valuej(kt,lfastj_hchob)  = ph_ch2om(iclm,kt,jclm)/ ft
	  valuej(kt,lfastj_n2o5)   = ph_n2o5(iclm,kt,jclm) / ft
	end do

    return	!finished wrf->peg mapping

    end subroutine mapJrates_tofrom_host
!-----------------------------------------------------------------------


      	     	
!***********************************************************************
        subroutine interface_fastj(tmidh,sla,slo,julian_day,   &
             pbnd, psfc, temp, ozone,                          &
             dz, cloudmr, airdensity, relhum, zatw,            &
             isvode, jsvode, lpar, jpnl,                       &
      	     curr_secs, valuej, cos_sza, processingAerosols,   &
             sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------------------------------
! sets parameters for fastj call. 
! inputs
!	tmidh -- GMT time in decimal hours at which to calculate
!                photolysis rates
!	sla -- latitude, decimal degrees in real*8
!	slo -- negative of the longitude, decimal degrees in real*8
!	julian_day -- day of the year in julian days
!    pbnd(0:lpar) = pressure at top boundary of cell k (dynes/cm^2).
!    psfc = surface pressure (dynes/cm^2).
!    temp(lpar)= mid-cell temperature values (deg K)
!    ozone(lpar) = mid-cell ozone mixing ratios 
!    surface_albedo -- broadband albedo (dimensionless)
!    curr_secs -- elapsed time from start of simulation in seconds.
!    isvode,jsvode  -- current column i,j.
!
!    lpar -- vertical extent of column (from module_fastj_cmnh)
!
! outputs
!	cos_sza -- cosine of solar zenith angle.
!	valuej(lpar,nphoto_fastj-1) -- array of photolysis rates, s-1.
!
!	
! local variables
!    surface_pressure_mb -- surface pressure (mb). equal to col_press_mb(1).
!    col_press_mb(lpar) -- for the column, grid cell boundary pressures
!          (not at cell centers) up until the bottom pressure for the
!          top cell (mb).
!    col_temp_K(lpar+1) -- for the column, grid cell center temperature (deg K)
!    col_ozone(lpar+1) -- for the column, grid cell center ozone mixing
!  	   ratios (dimensionless)
!    col_optical_depth(lpar+1) -- for the column, grid cell center cloud
!	   optical depths (dimensionless).SET TO ZERO IN THIS VERSION
!    tauaer_550 -- aerosol optical thickness at 550 nm.
! 	note:  photolysis rates are calculated at centers of model layers
! 	the pressures are given at the boundaries defining
!       the top and bottom of the layers
! 	so the number of pressure values is equal
!	to the (number of layers) + 1 ; the last pressure is set = 0 in fastj code.
! 	pressures from the surface up to the bottom of the top (lpar<=kmaxd) cell
! 	******** pressure 2
!	layer 1 - temperature,optical depth, and O3 given here
!	******** pressure 1
! the optical depth is appropriate for the layer depth
! conversion factor:   1 dyne/cm2 = 0.001 mb
!-----------------------------------------------------------------
        USE module_data_mosaic_other, only : kmaxd
	USE module_peg_util, only:  peg_message, peg_error_fatal
	
 	IMPLICIT NONE

!jdf
   integer, parameter :: iprint = 0
   integer, parameter :: single = 4        !compiler dependent value real*4
   integer, parameter :: double = 8        !compiler dependent value real*8
   integer,parameter :: ipar_fastj=1,jpar=1
   integer,parameter :: jppj=14        !Number of photolytic reactions supplied
   logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
   integer lpar           !Number of levels in CTM
   integer jpnl           !Number of levels requiring chemistry
   real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
   real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
   real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
   real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
   real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
   real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
   integer month_fastj        !  Number of month (1-12)
   integer iday_fastj         !  Day of year
   integer nphoto_fastj
   parameter (nphoto_fastj = 14)
   integer   &
   lfastj_no2,   lfastj_o3a,   lfastj_o3b,    lfastj_h2o2,   &
   lfastj_hchoa, lfastj_hchob, lfastj_ch3ooh, lfastj_no3x,   &
   lfastj_no3l,  lfastj_hono,  lfastj_n2o5,   lfastj_hno3,   &
   lfastj_hno4
   parameter( lfastj_no2   = 1 )
   parameter( lfastj_o3a   = 2 )
   parameter( lfastj_o3b   = 3 )
   parameter( lfastj_h2o2  = 4 )
   parameter( lfastj_hchoa = 5 )
   parameter( lfastj_hchob = 6 )
   parameter( lfastj_ch3ooh= 7 )
   parameter( lfastj_no3x  = 8 )
   parameter( lfastj_no3l  = 9 )
   parameter( lfastj_hono  = 10 )
   parameter( lfastj_n2o5  = 11 )
   parameter( lfastj_hno3  = 12 )
   parameter( lfastj_hno4  = 13 )
   integer nspint           ! Num of spectral intervals across solar 
   parameter ( nspint = 4 ) ! spectrum for FAST-J
   real, dimension (nspint),save :: wavmid !cm
   real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
   real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
   data wavmid     &
       / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
	real pbnd(0:lpar), psfc
	real temp(lpar), ozone(lpar), surface_albedo
	real dz(lpar), cloudmr(lpar), airdensity(lpar), relhum(lpar), zatw(lpar+1)
    real(kind=8) :: curr_secs
	integer isvode, jsvode
	
	real cos_sza
    integer,parameter :: lunout=41
		
	real valuej(lpar,nphoto_fastj)
	
    real hl,rhl,factor1,part1,part2,cfrac,rhfrac
    real emziohl(lpar+1),clwp(lpar)
!ec material to check output
 	real valuej_no3rate(lpar)

	real*8 lat,lon
    real*8 jvalue(lpar,nphoto_fastj)
    real sza
	real tau1
	real tmidh, sla, slo	

	integer julian_day,iozone1
    integer,parameter :: nfastj_rxns = 14
	integer k, l
 		
	real surface_pressure_mb, tauaer_550,   &
         col_press_mb,col_temp_K,col_ozone,col_optical_depth
	dimension col_press_mb(lpar+2),col_temp_K(lpar+1),   &
        	col_ozone(lpar+1),col_optical_depth(lpar+1)
    character*80 msg	

! define logical processingAerosols
! if processingAerosols = true, uses values calculated in subroutine
! mieaer for variables & arrays in common block mie.
! if processingAerosols = false, sets all variables & arrays in common
! block mie to zero.  (JCB-revised Fast-J requires common block mie info,
! regardless of whether aerosols are present or not.  Original Wild Fast-J
! did not use common block mie info.)

      logical processingAerosols

! set lat and longitude as real*8 for consistency with fastj code.
! variables lat and lon previously declared as reals
	lat  = sla
	lon  = slo
!    
! cloud optical depths currently treated by using fractional cloudiness 
! based on relative humidity. cloudmr set up to use cloud liquid water
! but hooks into microphysics need to be tested - for now set cloudmr=0
!
! parameters to calculate 'typical' liquid cloudwater path values for
! non convective clouds based on approximations in NCAR's CCM2
! 0.18 = reference liquid water concentration (gh2o/m3)
! hl = liquid water scale height (m)
!
    hl=1080.+2000.0*cos(lat*0.017454329)
    rhl=1.0/hl
	do k =1, lpar+1
       emziohl(k)=exp(-zatw(k)*rhl)
    enddo
	do k =1, lpar
       clwp(k)=0.18*hl*(emziohl(k)-emziohl(k+1))
    enddo
! assume radius of cloud droplets is constant at 10 microns (0.001 cm) and
! that density of water is constant at 1 g2ho/cm3
!       factor1=3./2./0.001/1.
    factor1=1500.0
	do k =1, lpar
	   col_optical_depth(k) = 0.0 		
       cfrac=0.0
       cloudmr(k)=0.0
       if(cloudmr(k).gt.0.0) cfrac=1.0
! 18.0*airdensity converts mole h2o/mole air to g h2o/cm3, part1 is in g h2o/cm2
       part1=cloudmr(k)*cfrac*18.0*airdensity(k)*dz(k)*100.0
       if(relhum(k).lt.0.8) then
          rhfrac=0.0
       elseif(relhum(k).le.1.0.and.relhum(k).ge.0.8) then
!            rhfrac=(relhum(k)-0.8)/(1.0-0.8)
          rhfrac=(relhum(k)-0.8)/0.2
       else
          rhfrac=1.0
       endif
       if(rhfrac.ge.0.01) then
! factor 1.0e4 converts clwp of g h2o/m2 to g h2o/cm2
          part2=rhfrac*clwp(k)/1.e4
       else
          part2=0.0
       endif
       if(cfrac.gt.0) part2=0.0
       col_optical_depth(k) = factor1*(part1+part2)
!          col_optical_depth(k) = 0.0 		
!       if(isvode.eq.33.and.jsvode.eq.34) &
!          print*,'jdf opt',isvode,jsvode,k,col_optical_depth(k), &
!          cfrac,rhfrac,relhum(k),clwp(k)
	end do
    col_optical_depth(lpar+1) = 0.0 		
	if (.not.processingAerosols) then
! set common block mie variables to 0 if
	   call set_common_mie(lpar, &
           sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
	end if      			 ! processingAerosols=false

! set pressure, temperature, ozone of each cell in the column
! set iozone1 = lpar to allow replacement of climatological ozone with model
! predicted ozone to top of chemistry column; standard fastj climatological o3
! thereafter.	
	surface_pressure_mb = psfc * 0.001
	tau1 = tmidh
	col_press_mb(1) = psfc * 0.001	
	iozone1 = lpar
	do k =1, lpar
       col_press_mb(k+1) = pbnd(k) * 0.001
	   col_temp_K(k) = temp(k)
	   col_ozone(k) = ozone(k)
	end do

 	surface_albedo=0.055

! set aerosol parameters needed by Fast-J	                         	         	
        if (processingAerosols) then
            tauaer_550 = 0.0 	! needed parameters already calculated by subroutine
              			! mieaer and passed into proper parts of fastj code
              			! via module_fastj_cmnmie
         else
            tauaer_550 = 0.05 	! no aerosols, assume typical constant aerosol optical thickness
         end if
	
	  CALL wrf_debug(250,'interface_fastj: calling fastj')
      call fastj(isvode,jsvode,lat,lon,surface_pressure_mb,surface_albedo,   &
           julian_day,  tau1,   &
          col_press_mb, col_temp_K, col_optical_depth, col_ozone,   &
          iozone1,tauaer_550,jvalue,sza,lpar,jpnl, &
          sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
     	
	!cos_sza = cosd(sza)
	cos_sza = cos(sza*3.141592653/180.)
	

! array jvalue (real*8) is returned from fastj.  array valuej(unspecified
! real, default of real*4) is sent on to
! other chemistry subroutines 
	   do k = 1, lpar
	     valuej(k, lfastj_no2) = jvalue(k,lfastj_no2)
	     valuej(k, lfastj_o3a) = jvalue(k,lfastj_o3a)
	     valuej(k, lfastj_o3b) = jvalue(k,lfastj_o3b)
	     valuej(k, lfastj_h2o2) = jvalue(k,lfastj_h2o2)
	     valuej(k, lfastj_hchoa) = jvalue(k,lfastj_hchoa)
	     valuej(k, lfastj_hchob) = jvalue(k,lfastj_hchob)
	     valuej(k, lfastj_ch3ooh) = jvalue(k,lfastj_ch3ooh)
	     valuej(k, lfastj_no3x) = jvalue(k,lfastj_no3x)
	     valuej(k, lfastj_no3l) = jvalue(k,lfastj_no3l)
	     valuej(k, lfastj_hono) = jvalue(k,lfastj_hono)
	     valuej(k, lfastj_n2o5) = jvalue(k,lfastj_n2o5)
	     valuej(k, lfastj_hno3) = jvalue(k,lfastj_hno3)
	     valuej(k, lfastj_hno4) = jvalue(k,lfastj_hno4)
	  end do
! diagnostic output and zeroed value if negative photolysis rates returned
	  do k = 1, lpar
         valuej(k,nphoto_fastj)=0.0
         do l = 1, nphoto_fastj-1
            if (valuej(k,l) .lt. 0) then
               write( msg, '(a,f14.2,4i4,1x,e11.4)' )   &
                    'FASTJ negative Jrate ' //   &
                    'tsec i j k l J(k,l)', curr_secs,isvode,jsvode,k,l,valuej(k,l)
               call peg_message( lunerr, msg )
               valuej(k,l) = 0.0
! following code used if want run stopped with negative Jrate                 
!                 msg = '*** subr interface_fastj -- ' //   &
!                   'Negative J rate returned from Fast-J'
!                 call peg_error_fatal( lunerr, msg )
            end if
         end do
      end do
! compute overall no3 photolysis rate
! wig: commented out since it is not used anywhere
!	do k = 1, lpar
!	   valuej_no3rate(k) =   &
!         	 valuej(k, lfastj_no3x) + valuej(k,lfastj_no3l)
!	end do

	end subroutine interface_fastj                          

!***********************************************************************
	subroutine set_common_mie(lpar, &
          sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
! for use when aerosols are not included in a model run.  sets variables
! in common block mie to zero, except for wavelengths.	
! OUTPUT: in module_fastj_cmnmie
!	wavmid ! fast-J wavelengths (cm)
!       tauaer ! aerosol optical depth
!       waer  ! aerosol single scattering albedo
!       gaer  ! aerosol asymmetery factor
!	extaer ! aerosol extinction
!	l2,l3,l4,l5,l6,l7 ! Legendre coefficients, numbered 0,1,2,......
!	sizeaer ! average wet radius
! INPUTS
!	lpar = total number of vertical layers in chemistry model.  Passed
!	  via module_fastf_cmnh
!	NB = total vertical layers + 1 considered by FastJ (lpar+1=kmaxd+1).
!	   passed via module_fast	j_data
!------------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	
	IMPLICIT NONE
!jdf
        integer lpar             ! Number of levels in CTM
        integer nspint           ! Num of spectral intervals across solar 
        parameter ( nspint = 4 ) ! spectrum for FAST-J
        real, dimension (nspint),save :: wavmid !cm
        real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
        real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
        data wavmid     &
            / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf

! LOCAL VARIABLES
        integer klevel   ! vertical level index
        integer ns       ! spectral loop index


!  aerosol optical properties: set everything = 0 when no aerosol
        do 1000 ns=1,nspint
        do 1000 klevel = 1, lpar
          tauaer(ns,klevel)=0.
          waer(ns,klevel)=0.
          gaer(ns,klevel)=0.
	  sizeaer(ns,klevel)=0.0
	  extaer(ns,klevel)=0.0
	  l2(ns,klevel)=0.0
	  l3(ns,klevel)=0.0
  	  l4(ns,klevel)=0.0
	  l5(ns,klevel)=0.0
	  l6(ns,klevel)=0.0
	  l7(ns,klevel)=0.0
1000    continue

	return
	end subroutine set_common_mie      
!***********************************************************************
    subroutine fastj(isvode,jsvode,lat,lon,surface_pressure,surface_albedo,   &
      julian_day,tau1, pressure, temperature, optical_depth, my_ozone1,   &
      iozone1,tauaer_550_1,jvalue,SZA_dum,lpar,jpnl, &
      sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
! input:
! 	lat = latitute; must be real*8
! 	lon = longitude; must be real*8
! 	surface_pressure (mb); real*4
! 	surface_albedo (broadband albedo); real*4
! 	julian_day; integer
! 	tau1 = time of calculation (GMT); real*4
! 	pressure (mb) = vector of pressure values, pressure(NB);
! 	   real*4; NB is the number of model layers;
! 	   pressure (NB+1) is defined as 0 mb in model
! 	temperature (degree K)= vector of temperature values, temperature(NB);
!	   real*4
! 	optical_depth (dimensionless) = vector of cloud optical depths,
!	   optical_depth(NB); real*4
! 	my_ozone1 (volume mixing ratio) = ozone at layer center
!	   ozone(iozone); real*4; if iozone1 <= NB-1, then climatology is
!	   used in the upper layers
! 	tauaer_550; real*4  aerosol optical thickness @ 550 nm
! input note: NB is the number of model layers -- photolysis rates are calculated
! 	at layer centers while pressures  are given at the boundaries defining
!       the top and bottom of the layers.  The number of pressure values =
!	(number of layers) + 1 ; see below
! 	******** pressure 2
!	layer 1 - optical depth, O3, and temperature given here
!	******** pressure 1
!   temperature and o3 are defined at the layer center. optical depth is
!   appropriate for the layer depth.
! output:
! 	jvalue = photolysis rates, an array of dimension jvalue(jpnl,jppj) where
! 	  jpnl = # of models level at which photolysis rates are calculated
! 	  note: level 1 = first level of model (adjacent to ground)
! 	  jppj = # of chemical species for which photolysis rates are calculated;
! 	  this is fixed and is not easy to change on the fly
!	  jpnl land jppl are defined in the common block "cmn_h.f"
!       SZA_dum = solar zenith angle
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
       integer, parameter :: iprint = 0
       integer, parameter :: single = 4        !compiler dependent value real*4
!      integer, parameter :: double = 8        !compiler dependent value real*8
! following specific for fastj
! jppj will be gas phase mechanism dependent       
       integer,parameter :: ipar_fastj=1,jpar=1
!      integer,parameter :: jppj=14        !Number of photolytic reactions supplied
       logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
! The vertical level variables are set in fastj_driver.
       integer lpar           !Number of levels in CTM
       integer jpnl           !Number of levels requiring chemistry
! following should be available from other wrf modules and passed into
! photodriver
       real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
       real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
       real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
       real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
       real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
       real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
       integer month_fastj        !  Number of month (1-12)
       integer iday_fastj         !  Day of year
       real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
       real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
       real(kind=double) my_ozone(kmaxd)       !real*8 version of ozone mixing ratios
       real tauaer_550
       integer iozone
       integer nslat               !  Latitude of current profile point
       integer nslon               !  Longitude of current profile point
       save :: nslat, nslon
       integer nspint           ! Num of spectral intervals across solar 
       parameter ( nspint = 4 ) ! spectrum for FAST-J
       real, dimension (nspint),save :: wavmid !cm
       real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
       real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
       data wavmid     &
           / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
	
    integer julian_day
    real surface_pressure,surface_albedo,pressure(lpar+2),   &
         temperature(lpar+1)
	real optical_depth(lpar+1)
	real tau1
    real*8 pi_fastj,lat,lon,timej,jvalue(lpar,jppj)
    integer isvode,jsvode

	integer iozone1,i
	real my_ozone1(lpar+1)

	real tauaer_550_1
	real sza_dum
	
	integer ientryno_fastj
	save ientryno_fastj
        data ientryno_fastj / 0 /

	

!  Just focus on one column
      nslat = 1
      nslon = 1
      pi_fastj=3.141592653589793D0
!

! JCB - note that pj(NB+1) = p and is defined such elsewhere
! wig: v3.0.0 release has do loop from 1:lpar+1 but T is never
!      initialized at lpar+1 leading to model instabilities. T and OD
!      are ultimately never used at lpar+1. So, I changed this loop to
!      1:lpar and then just assign pj a value at lpar+1.
        do i=1,lpar
           pj(i)=pressure(i)
           T(nslon,nslat,i)=temperature(i)
           OD(nslon,nslat,i)=optical_depth(i)
        enddo
        pj(lpar+1) = pressure(i)
     
! surface albedo
	SA(nslon,nslat)=surface_albedo
!
	iozone=iozone1
	do i=1,iozone1
       my_ozone(i)=my_ozone1(i)
	enddo
!
	tau_fastj=tau1 ! fix time
	iday_fastj=julian_day	
! fix optical depth for situations where aerosols not considered
	tauaer_550=tauaer_550_1
!
      month_fastj=int(dble(iday_fastj)*12.d0/365.d0)+1    !  Approximately
      xgrd(nslon)=lon*pi_fastj/180.d0
      ygrd(nslat)=lat*pi_fastj/180.d0
      ydgrd(nslat)=lat

!  Initial call to Fast-J to set things up--done only once
	if (ientryno_fastj .eq. 0) then
            call inphot2
            ientryno_fastj = 1
        end if    
!
!  Now call fastj as appropriate
	timej=0.0 ! manually set offset to zero (JCB: 14 November 2001)
    call photoj(isvode,jsvode,jvalue,timej,nslat,nslon,iozone,tauaer_550, &
        my_ozone,p,t,od,sa,lpar,jpnl, &
        xgrd,ygrd,tau_fastj,month_fastj,iday_fastj,ydgrd, &
        sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
	sza_dum=SZA

	return
      end subroutine fastj
!**********************************************************************
!---(pphot.f)-------generic CTM shell from UCIrvine (p-code 4.0, 7/99)
!---------PPHOT calculates photolysis rates with the Fast-J scheme
!---------subroutines:  inphot, photoj, Fast-J schemes...
!-----------------------------------------------------------------------
!
      subroutine inphot2
!-----------------------------------------------------------------------
!  Routine to initialise photolysis rate data, called directly from the
!  cinit routine in ASAD. Currently use it to read the JPL spectral data
!  and standard O3 and T profiles and to set the appropriate reaction index.
!-----------------------------------------------------------------------
!
!     iph       Channel number for reading all data files
!     rad       Radius of Earth (cm)
!     zzht      Effective scale height above top of atmosphere (cm)
!     dtaumax   Maximum opt.depth above which a new level should be inserted
!     dtausub   No. of opt.depths at top of cloud requiring subdivision
!     dsubdiv   Number of additional levels to add at top of cloud
!     szamax    Solar zenith angle cut-off, above which to skip calculation
!
!-----------------------------------------------------------------------
!

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data

	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
        integer, parameter :: iprint = 0
        integer, parameter :: single = 4        !compiler dependent value real*4
!       integer, parameter :: double = 8        !compiler dependent value real*8
       integer,parameter :: ipar_fastj=1,jpar=1
!      integer,parameter :: jppj=14        !Number of photolytic reactions supplied
       logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
       integer lpar           !Number of levels in CTM
       integer jpnl           !Number of levels requiring chemistry
       real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
       real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
       real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
       real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
       real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
       real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
       integer month_fastj        !  Number of month (1-12)
       integer iday_fastj         !  Day of year
!jdf
!
! Set labels of photolysis rates required
!ec032504      CALL RD_JS(iph,path_fastj_ratjd)
!	call rd_js2
!
! Read in JPL spectral data set
!ec032504      CALL RD_TJPL(iph,path_fastj_jvspec)
	call rd_tjpl2
!
! Read in T & O3 climatology
!ec032504      CALL RD_PROF(iph,path_fastj_jvatms)
!	call rd_prof2
!
! Select Aerosol/Cloud types to be used
!         call set_aer2

      return
      end subroutine inphot2
!*************************************************************************

      subroutine photoj(isvode,jsvode,zpj,timej,nslat,nslon,iozone,tauaer_550_1, &
        my_ozone,p,t,od,sa,lpar,jpnl,xgrd,ygrd,tau_fastj,month_fastj,iday_fastj, &
        ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------------------------------------
!----jv_trop.f:  new FAST J-Value code, troposphere only (mjprather 6/96)
!----     uses special wavelength quadrature spectral data (jv_spec.dat)
!---      that includes only 289 nm - 800 nm  (later a single 205 nm add-on)
!---      uses special compact Mie code based on Feautrier/Auer/Prather vers.
!-----------------------------------------------------------------------
!
!     zpj      External array providing J-values to main CTM code
!     timej    Offset in hours from start of timestep to time J-values
!              required for - take as half timestep for mid-step Js.
!     solf     Solar distance factor, for scaling; normally given by:
!                      1.0-(0.034*cos(real(iday_fastj-186)*2.0*pi_fastj/365.))
!
!----------basic common blocks:-----------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
       integer, parameter :: iprint = 0
       integer, parameter :: single = 4        !compiler dependent value real*4
!      integer, parameter :: double = 8        !compiler dependent value real*8
       integer,parameter :: ipar_fastj=1,jpar=1
!      integer,parameter :: jppj=14        !Number of photolytic reactions supplied
       logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
       integer lpar           !Number of levels in CTM
       integer jpnl           !Number of levels requiring chemistry
       real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
       real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
       real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
       real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
       real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
       real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
       integer month_fastj        !  Number of month (1-12)
       integer iday_fastj         !  Day of year
       real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
       real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
       real(kind=double) my_ozone(kmaxd)       !real*8 version of ozone mixing ratios
       real tauaer_550_1
       integer iozone
       integer nslat               !  Latitude of current profile point
       integer nslon               !  Longitude of current profile point
       integer nspint           ! Num of spectral intervals across solar 
       parameter ( nspint = 4 ) ! spectrum for FAST-J
       real, dimension (nspint),save :: wavmid !cm
       real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
       real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
       data wavmid     &
           / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
      real*8 zpj(lpar,jppj),timej,solf
	real*8 pi_fastj

	integer i,j
    integer isvode,jsvode

!-----------------------------------------------------------------------
!
      do i=1,jpnl
        do j=1,jppj
          zj(i,j)=0.D0
          zpj(i,j)=0.D0
        enddo
      enddo
!
!---Calculate new solar zenith angle
      CALL SOLAR2(timej,nslat,nslon, &
      xgrd,ygrd,tau_fastj,month_fastj,iday_fastj)
      if(SZA.gt.szamax) go to 10
!
!---Set up profiles on model levels
      CALL SET_PROF(isvode,jsvode,nslat,nslon,iozone,tauaer_550_1, &
        my_ozone,p,t,od,sa,lpar,jpnl,month_fastj,ydgrd)
!
!---Print out atmosphere
       if(iprint.ne.0)CALL PRTATM(3,nslat,nslon,tau_fastj,month_fastj,ydgrd)  ! code change jcb
!	call prtatm(0)
!
!-----------------------------------------------------------------------
      CALL JVALUE(isvode,jsvode,lpar,jpnl, &
        ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------------------------------------
!---Print solar flux terms
!      WRITE(6,'(A16,I5,20I9)') '  wave (beg/end)',(i,i=1,jpnl)
!      DO j=NW1,NW2
!        WRITE(6,'(2F8.2,20F9.6)') WBIN(j),WBIN(j+1),
!     $                            (FFF(j,i)/FL(j),i=1,jpnl)
!      ENDDO
!
!---Include variation in distance to sun
	pi_fastj=3.1415926536d0
       solf=1.d0-(0.034d0*cos(dble(iday_fastj-186)*2.d0   &
             *pi_fastj/365.d0))
	if(iprint.ne.0)then
!	   write(6,'('' solf = '', f10.5)')solf
          write(*,'('' solf = '', f10.5)')solf
	endif
!      solf=1.d0 ! code change jcb
!-----------------------------------------------------------------------
      CALL JRATET(solf,nslat,nslon,p,t,od,sa,lpar,jpnl)
!-----------------------------------------------------------------------
!

!  "zj" updated in JRATET - pass this back to ASAD as "zpj"
      do i=1,jpnl
        do j=1,jppj
          zpj(i,j)= zj(i,j)
        enddo
      enddo

!
!---Output selected values
  10  if((.not.ldeg45.and.nslon.eq.37.and.nslat.eq.36).or.   &
               (ldeg45.and.nslon.eq.19.and.nslat.eq.18)) then
        i=min(jppj,8)
!        write(6,1000)iday_fastj,tau_fastj+timej,sza,jlabel(i),zpj(1,i)
      endif
!
      return
! 1000 format(' Photolysis on day ',i4,' at ',f4.1,' hrs: SZA = ',f7.3,   &
!                     ' J',a7,'= ',1pE10.3)
      end subroutine photoj           

!*************************************************************************
      subroutine set_prof(isvode,jsvode,nslat,nslon,iozone,tauaer_550, &
        my_ozone,p,t,od,sa,lpar,jpnl,month_fastj,ydgrd)
!-----------------------------------------------------------------------
!  Routine to set up atmospheric profiles required by Fast-J using a
!  doubled version of the level scheme used in the CTM. First pressure
!  and z* altitude are defined, then O3 and T are taken from the supplied
!  climatology and integrated to the CTM levels (may be overwritten with
!  values directly from the CTM, if desired) and then black carbon and
!  aerosol profiles are constructed.
!                                                     Oliver (04/07/99)
!-----------------------------------------------------------------------
!
!     pj       Pressure at boundaries of model levels (hPa)
!     z        Altitude of boundaries of model levels (cm)
!     odcol    Optical depth at each model level
!     masfac   Conversion factor for pressure to column density
!
!     TJ       Temperature profile on model grid
!     DM       Air column for each model level (molecules.cm-2)
!     DO3      Ozone column for each model level (molecules.cm-2)
!     DBC      Mass of Black Carbon at each model level (g.cm-3)  !  .....!
!     PSTD     Approximate pressures of levels for supplied climatology
!
!-----------------------------------------------------------------------
      
      USE module_data_mosaic_other, only : kmaxd
      USE module_fastj_data
      
      IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
      real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
      real(kind=double) my_ozone(kmaxd)       !real*8 version of ozone mixing ratios
      real tauaer_550
      integer iozone
      integer nslat               !  Latitude of current profile point
      integer nslon               !  Longitude of current profile point
!jdf
      real*8  pstd(52),oref2(51),tref2(51),bref2(51)
      real*8  odcol(lpar),dlogp,f0,t0,b0,pb,pc,xc,masfac,scaleh
      real vis, aerd1, aerd2

      integer  i, k, l, m
      integer isvode,jsvode

       pj(NB+1) = 0.d0 ! define top level
!
!  Set up cloud and surface properties
      call CLDSRF(isvode,jsvode,odcol,nslat,nslon,p,t,od,sa,lpar,jpnl)

!  Mass factor - delta-Pressure (mbars) to delta-Column (molecules.cm-2)
      masfac=100.d0*6.022d+23/(28.97d0*9.8d0*10.d0)
!
!  Set up pressure levels for O3/T climatology - assume that value
!  given for each 2 km z* level applies from 1 km below to 1 km above,
!  so select pressures at these boundaries. Surface level values at
!  1000 mb are assumed to extend down to the actual P(nslon,nslat).
!
      pstd(1) = max(pj(1),1000.d0)
      pstd(2) = 1000.d0*10.d0**(-1.d0/16.d0)
      dlogp = 10.d0**(-2.d0/16.d0)
      do i=3,51
        pstd(i) = pstd(i-1)*dlogp
      enddo
      pstd(52) = 0.d0
!
!  Select appropriate monthly and latitudinal profiles
      m = max(1,min(12,month_fastj))
      l = max(1,min(18,(int(ydgrd(nslat))+99)/10))
!
!  Temporary arrays for climatology data
      do i=1,51
        oref2(i)=oref(i,l,m)
        tref2(i)=tref(i,l,m)
        bref2(i)=bref(i)
      enddo
!
!  Apportion O3 and T on supplied climatology z* levels onto CTM levels
!  with mass (pressure) weighting, assuming constant mixing ratio and
!  temperature half a layer on either side of the point supplied.
!
      do i = 1,NB
        F0 = 0.d0
        T0 = 0.d0
        B0 = 0.d0
        do k = 1,51
          PC = min(pj(i),pstd(k))
          PB = max(pj(i+1),pstd(k+1))
          if(PC.gt.PB) then
            XC = (PC-PB)/(pj(i)-pj(i+1))
            F0 = F0 + oref2(k)*XC
            T0 = T0 + tref2(k)*XC
            B0 = B0 + bref2(k)*XC
          endif
        enddo
        TJ(i) = T0
        DO3(i)= F0*1.d-6
        DBC(i)= B0
      enddo
!
!  Insert model values here to replace or supplement climatology.
!  Note that CTM temperature is always used in x-section calculations
!  (see JRATET); TJ is used in actinic flux calculation only.
!	
        do i=1,lpar ! JCB code change; just use climatlogy for upper levels
        if(i.le.iozone)DO3(i) = my_ozone(i) ! Volume Mixing Ratio
!        TJ(i)  = T(nslon,nslat,I)   ! Kelvin
! JCB - overwrite climatology
!	TJ(i)  = (T(nslon,nslat,i)+T(nslon,nslat,i+1))/2. ! JCB - take midpoint
! code change - now take temperature as appropriate for midpoint of layer
	TJ(i)=T(nslon,nslat,i)
        enddo
	if(lpar+1.le.iozone)then
        DO3(lpar+1) = my_ozone(lpar+1)  ! Above top of model (or use climatology)
	endif
!       TJ(lpar+1)  = my_temp(lpar)   ! Above top of model (or use climatology)
!wig 26-Aug-2000: Comment out following line so that climatology is used for 
!                 above the model top.
!	TJ(lpar+1) = T(nslon,nslat,NB)  ! JCB - just use climatology or given temperature
! JCB read in O3
!
!
!  Calculate effective altitudes using scale height at each level
      z(1) = 0.d0
      do i=1,lpar
        scaleh=1.3806d-19*masfac*TJ(i)
        z(i+1) = z(i)-(log(pj(i+1)/pj(i))*scaleh)
      enddo
!
!  Add Aerosol Column - include aerosol types here. Currently use soot
!  water and ice; assume black carbon x-section of 10 m2/g, independent
!  of wavelength; assume limiting temperature for ice of -40 deg C.

      do i=1,lpar
!        AER(1,i) = DBC(i)*10.d0*(z(i+1)-z(i)) ! DBC must be g/m^3
! calculate AER(1,i) according to aerosol density - use trap rule
	vis=23.0
	call aeroden(z(i)/100000.,vis,aerd1) ! convert cm to km
	call aeroden(z(i+1)/100000.,vis,aerd2)
! trap rule used here; convert cm to km; divide by 100000.
	AER(1,i)=(z(i+1)-z(i))/100000.*(aerd1+aerd2)/2./4287.55*tauaer_550
!	write(6,*)i,z(i)/100000.,aerd1,aerd2,tauaer_550,AER(1,i)
!	
        if(T(nslon,nslat,I).gt.233.d0) then
          AER(2,i) = odcol(i)
          AER(3,i) = 0.d0
        else
          AER(2,i) = 0.d0
          AER(3,i) = odcol(i)
        endif
      enddo
      do k=1,MX
        AER(k,lpar+1) = 0.d0 ! just set equal to zero
      enddo

	AER(1,lpar+1)=2.0*AER(1,lpar) ! kludge
!
!  Calculate column quantities for Fast-J
      do i=1,NB
        DM(i)  = (PJ(i)-PJ(i+1))*masfac
        DO3(i) = DO3(i)*DM(i)
      enddo
!
      end subroutine set_prof

!******************************************************************
!     SUBROUTINE CLDSRF(odcol)
      SUBROUTINE CLDSRF(isvode,jsvode,odcol,nslat,nslon,p,t,od,sa, &
         lpar,jpnl)
!-----------------------------------------------------------------------
!  Routine to set cloud and surface properties
!-----------------------------------------------------------------------
!     rflect   Surface albedo (Lambertian)
!     odmax    Maximum allowed optical depth, above which they are scaled
!     odcol    Optical depth at each model level
!     odsum    Column optical depth
!     nlbatm   Level of lower photolysis boundary - usually surface
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      integer nslat               !  Latitude of current profile point
      integer nslon               !  Longitude of current profile point
      real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
      real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
!jdf
      integer i, j, k
      integer isvode, jsvode
      real*8  odcol(lpar), odsum, odmax, odtot
!
! Default lower photolysis boundary as bottom of level 1
      nlbatm = 1
!
! Set surface albedo
      RFLECT = dble(SA(nslon,nslat))
      RFLECT = max(0.d0,min(1.d0,RFLECT))
!
! Zero aerosol column
      do k=1,MX
        do i=1,NB
          AER(k,i) = 0.d0
        enddo
      enddo
!
! Scale optical depths as appropriate - limit column to 'odmax'
      odmax = 200.d0
      odsum =   0.d0
      do i=1,lpar
        odcol(i) = dble(OD(nslon,nslat,i))
        odsum    = odsum + odcol(i)
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf odcol',odcol(i),odsum
      enddo
      if(odsum.gt.odmax) then
        odsum = odmax/odsum
        do i=1,lpar
          odcol(i) = odcol(i)*odsum
        enddo
        odsum = odmax
      endif
! Set sub-division switch if appropriate
      odtot=0.d0
      jadsub(nb)=0
      jadsub(nb-1)=0
      do i=nb-1,1,-1
        k=2*i
        jadsub(k)=0
        jadsub(k-1)=0
        odtot=odtot+odcol(i)
        if(odcol(i).gt.0.d0.and.dtausub.gt.0.d0) then
          if(odtot.le.dtausub) then
            jadsub(k)=1
            jadsub(k-1)=1
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf in cldsf1',i,k,jadsub(k)
          elseif(odtot.gt.dtausub) then
            jadsub(k)=1
            jadsub(k-1)=0
            do j=1,2*(i-1)
              jadsub(j)=0
            enddo
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf in cldsf2',i,k,jadsub(k)
            go to 20
          endif
        endif
      enddo
 20   continue
!
      return
      end SUBROUTINE CLDSRF       

!********************************************************************
      subroutine solar2(timej,nslat,nslon, &
      xgrd,ygrd,tau_fastj,month_fastj,iday_fastj)
!-----------------------------------------------------------------------
!  Routine to set up SZA for given lat, lon and time
!-----------------------------------------------------------------------
!     timej    Offset in hours from start of timestep to time J-values
!              required for - take as half timestep for mid-step Js.
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      integer nslat               !  Latitude of current profile point
      integer nslon               !  Longitude of current profile point
!jdf
      real*8 pi_fastj, pi180, loct, timej
      real*8 sindec, soldek, cosdec, sinlat, sollat, coslat, cosz
!
      pi_fastj=3.141592653589793D0
      pi180=pi_fastj/180.d0
      sindec=0.3978d0*sin(0.9863d0*(dble(iday_fastj)-80.d0)*pi180)
      soldek=asin(sindec)
      cosdec=cos(soldek)
      sinlat=sin(ygrd(nslat))
      sollat=asin(sinlat)
      coslat=cos(sollat)
!
      loct = (((tau_fastj+timej)*15.d0)-180.d0)*pi180 + xgrd(nslon)
      cosz = cosdec*coslat*cos(loct) + sindec*sinlat
      sza  = acos(cosz)/pi180
      U0 = cos(SZA*pi180)
!
      return
      end subroutine solar2       

!**********************************************************************


      SUBROUTINE JRATET(SOLF,nslat,nslon,p,t,od,sa,lpar,jpnl)
!-----------------------------------------------------------------------
!  Calculate and print J-values. Note that the loop in this routine
!  only covers the jpnl levels actually needed by the CTM.
!-----------------------------------------------------------------------
!
!     FFF    Actinic flux at each level for each wavelength bin
!     QQQ    Cross sections for species (read in in RD_TJPL)
!     SOLF   Solar distance factor, for scaling; normally given by:
!                      1.0-(0.034*cos(real(iday_fastj-186)*2.0*pi_fastj/365.))
!                      Assumes aphelion day 186, perihelion day 3.
!     TQQ    Temperatures at which QQQ cross sections supplied
!
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      integer nslat               !  Latitude of current profile point
      integer nslon               !  Longitude of current profile point
      real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
      real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
!jdf
      integer i, j, k
      real*8 qo2tot, qo3tot, qo31d, qo33p, qqqt
!      real*8 xseco2, xseco3, xsec1d, solf, tfact
      real*8  solf, tfact
!
      do I=1,jpnl
       VALJ(1) = 0.d0
       VALJ(2) = 0.d0
       VALJ(3) = 0.d0
       do K=NW1,NW2                       ! Using model 'T's here
         QO2TOT= xseco2(K,dble(T(nslon,nslat,I)))
         VALJ(1) = VALJ(1) + QO2TOT*FFF(K,I)
         QO3TOT= xseco3(K,dble(T(nslon,nslat,I)))
         QO31D = xsec1d(K,dble(T(nslon,nslat,I)))*QO3TOT
         QO33P = QO3TOT - QO31D
         VALJ(2) = VALJ(2) + QO33P*FFF(K,I)
         VALJ(3) = VALJ(3) + QO31D*FFF(K,I)
       enddo
!------Calculate remaining J-values with T-dep X-sections
       do J=4,NJVAL
         VALJ(J) = 0.d0
         TFACT = 0.d0
         if(TQQ(2,J).gt.TQQ(1,J)) TFACT = max(0.d0,min(1.d0,   &
              (T(nslon,nslat,I)-TQQ(1,J))/(TQQ(2,J)-TQQ(1,J)) ))
         do K=NW1,NW2
           QQQT = QQQ(K,1,J-3) + (QQQ(K,2,J-3) - QQQ(K,1,J-3))*TFACT
           VALJ(J) = VALJ(J) + QQQT*FFF(K,I)
!------Additional code for pressure dependencies
!           if(jpdep(J).ne.0) then
!             VALJ(J) = VALJ(J) + QQQT*FFF(K,I)*
!     $                   (zpdep(K,L)*(pj(i)+pj(i+1))*0.5d0)
!           endif
         enddo
       enddo
       do j=1,jppj
         zj(i,j)=VALJ(jind(j))*jfacta(j)*SOLF
       enddo
!  Herzberg bin
       do j=1,nhz
         zj(i,hzind(j))=hztoa(j)*fhz(i)*SOLF
       enddo
      enddo
      return
      end SUBROUTINE JRATET

!*********************************************************************


      SUBROUTINE PRTATM(N,nslat,nslon,tau_fastj,month_fastj,ydgrd)
!-----------------------------------------------------------------------
!  Print out the atmosphere and calculate appropriate columns
!     N=1    Print out column totals only
!     N=2    Print out full columns
!     N=3    Print out full columns and climatology
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      integer nslat               !  Latitude of current profile point
      integer nslon               !  Longitude of current profile point
!jdf
      integer n, i, k, l, m
      real*8 COLO3(NB),COLO2(NB),COLAX(MX,NB),ZKM,ZSTAR,PJC
      real*8 climat(9),masfac,dlogp
      if(N.eq.0) return
!---Calculate columns, for diagnostic output only:
      COLO3(NB) = DO3(NB)
      COLO2(NB) = DM(NB)*0.20948d0
      do K=1,MX
        COLAX(K,NB) = AER(K,NB)
      enddo
      do I=NB-1,1,-1
        COLO3(i) = COLO3(i+1)+DO3(i)
        COLO2(i) = COLO2(i+1)+DM(i)*0.20948d0
        do K=1,MX
          COLAX(k,i) = COLAX(k,i+1)+AER(k,i)
        enddo
      enddo
      write(*,1200) ' Tau=',tau_fastj,'  SZA=',sza
      write(*,1200) ' O3-column(DU)=',COLO3(1)/2.687d16,   &
                    '  column aerosol @1000nm=',(COLAX(K,1),K=1,MX)
!---Print out atmosphere
      if(N.gt.1) then
        write(*,1000) (' AER-X ','col-AER',k=1,mx)
        do I=NB,1,-1
          PJC = PJ(I)
          ZKM =1.d-5*Z(I)
          ZSTAR = 16.d0*DLOG10(1013.d0/PJC)
          write(*,1100) I,ZKM,ZSTAR,DM(I),DO3(I),1.d6*DO3(I)/DM(I),   &
               TJ(I),PJC,COLO3(I),COLO2(I),(AER(K,I),COLAX(K,I),K=1,MX)
        enddo
      endif
!
!---Print out climatology
      if(N.gt.2) then
        do i=1,9
          climat(i)=0.d0
        enddo
        m = max(1,min(12,month_fastj))
        l = max(1,min(18,(int(ydgrd(nslat))+99)/10))
        masfac=100.d0*6.022d+23/(28.97d0*9.8d0*10.d0)
        write(*,*) 'Specified Climatology'
        write(*,1000)
        do i=51,1,-1
          dlogp=10.d0**(-1.d0/16.d0)
          PJC = 1000.d0*dlogp**(2*i-2)
          climat(1) = 16.d0*DLOG10(1000.D0/PJC)
          climat(2) = climat(1)
          climat(3) = PJC*(1.d0/dlogp-dlogp)*masfac
          if(i.eq.1) climat(3)=PJC*(1.d0-dlogp)*masfac
          climat(4)=climat(3)*oref(i,l,m)*1.d-6
          climat(5)=oref(i,l,m)
          climat(6)=tref(i,l,m)
          climat(7)=PJC
          climat(8)=climat(8)+climat(4)
          climat(9)=climat(9)+climat(3)*0.20948d0
          write(*,1100) I,(climat(k),k=1,9)
        enddo
        write(*,1200) ' O3-column(DU)=',climat(8)/2.687d16
      endif
      return
 1000 format(5X,'Zkm',3X,'Z*',8X,'M',8X,'O3',6X,'f-O3',5X,'T',7X,'P',6x,   &
          'col-O3',3X,'col-O2',2X,10(a7,2x))
 1100 format(1X,I2,0P,2F6.2,1P,2E10.3,0P,F7.3,F8.2,F10.4,1P,10E9.2)
 1200 format(A,F8.1,A,10(1pE10.3))
      end SUBROUTINE PRTATM   

      SUBROUTINE JVALUE(isvode,jsvode,lpar,jpnl, &
        ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------------------------------------
!  Calculate the actinic flux at each level for the current SZA value.
!        quit when SZA > 98.0 deg ==> tangent height = 63 km
!             or         99.                           80 km
!-----------------------------------------------------------------------
!
!     AVGF   Attenuation of beam at each level for each wavelength
!     FFF    Actinic flux at each desired level
!     FHZ    Actinic flux in Herzberg bin
!     WAVE   Effective wavelength of each wavelength bin
!     XQO2   Absorption cross-section of O2
!     XQO3   Absorption cross-section of O3
!
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	USE module_peg_util, only:  peg_message
!jdf
! Print Fast-J diagnostics if iprint /= 0
        integer, parameter :: iprint = 0
        integer, parameter :: single = 4        !compiler dependent value real*4
!       integer, parameter :: double = 8        !compiler dependent value real*8
       integer,parameter :: ipar_fastj=1,jpar=1
!      integer,parameter :: jppj=14        !Number of photolytic reactions supplied
       logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
       integer lpar           !Number of levels in CTM
       integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      real(kind=double), dimension(ipar_fastj,jpar) ::   P , SA
      real(kind=double), dimension(ipar_fastj,jpar,kmaxd+1) :: T, OD
      integer nspint           ! Num of spectral intervals across solar 
      parameter ( nspint = 4 ) ! spectrum for FAST-J
      real, dimension (nspint),save :: wavmid !cm
      real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
      real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
      data wavmid     &
        / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
!jdf
      integer j, k
!      real*8  wave, xseco3, xseco2
      real*8  wave
      real*8  AVGF(lpar),XQO3(NB),XQO2(NB)
! diagnostics for error situations
!      integer lunout
!      parameter (lunout = 41)
    integer isvode,jsvode
	character*80 msg
!
      do J=1,jpnl
        do K=NW1,NW2
          FFF(K,J) = 0.d0
        enddo
        FHZ(J) = 0.d0
      enddo
!
!---SZA check
      if(SZA.gt.szamax) GOTO 99
!
!---Calculate spherical weighting functions
      CALL SPHERE(lpar)
!
!---Loop over all wavelength bins
      do K=NW1,NW2
        WAVE = WL(K)
        do J=1,NB
          XQO3(J) = xseco3(K,dble(TJ(J)))
        enddo
        do J=1,NB
          XQO2(J) = xseco2(K,dble(TJ(J)))
        enddo
!-----------------------------------------
        CALL OPMIE(isvode,jsvode,K,WAVE,XQO2,XQO3,AVGF,lpar,jpnl, &
          ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------
        do J=1,jpnl
          FFF(K,J) = FFF(K,J) + FL(K)*AVGF(J)
! diagnostic 
          if ( FFF(K,J) .lt. 0) then         
	     write( msg, '(a,2i4,e14.6)' )   &
                  'FASTJ neg actinic flux ' //   &
                  'k j FFF(K,J) ', k, j, fff(k,j)
             call peg_message( lunerr, msg )         
          end if
! end diagnostic
        enddo
      enddo
!
!---Herzberg continuum bin above 10 km, if required
      if(NHZ.gt.0) then
        K=NW2+1
        WAVE = 204.d0
        do J=1,NB
          XQO3(J) = HZO3
          XQO2(J) = HZO2
        enddo
        CALL OPMIE(isvode,jsvode,K,WAVE,XQO2,XQO3,AVGF,lpar,jpnl, &
          ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
        do J=1,jpnl
          if(z(j).gt.1.d6) FHZ(J)=AVGF(J)
        enddo
      endif
!
   99 continue
 1000 format('  SZA=',f6.1,' Reflectvty=',f6.3,' OD=',10(1pe10.3))

      return
      end SUBROUTINE JVALUE

      FUNCTION xseco3(K,TTT)
!-----------------------------------------------------------------------
!  Cross-sections for O3 for all processes interpolated across 3 temps
!-----------------------------------------------------------------------

	USE module_fastj_data
	
      integer k
!      real*8 ttt, flint, xseco3
      real*8 ttt, xseco3
      xseco3  =   &
        flint(TTT,TQQ(1,2),TQQ(2,2),TQQ(3,2),QO3(K,1),QO3(K,2),QO3(K,3))
      return
      end FUNCTION xseco3       

      FUNCTION xsec1d(K,TTT)
!-----------------------------------------------------------------------
!  Quantum yields for O3 --> O2 + O(1D) interpolated across 3 temps
!-----------------------------------------------------------------------

	USE module_fastj_data
	
      integer k
!      real*8 ttt, flint, xsec1d
      real*8 ttt,  xsec1d
      xsec1d =   &
        flint(TTT,TQQ(1,3),TQQ(2,3),TQQ(3,3),Q1D(K,1),Q1D(K,2),Q1D(K,3))
      return
      end FUNCTION xsec1d       

      FUNCTION xseco2(K,TTT)
!-----------------------------------------------------------------------
!  Cross-sections for O2 interpolated across 3 temps; No S_R Bands yet!
!-----------------------------------------------------------------------

	USE module_fastj_data
	
      integer k
!      real*8 ttt, flint, xseco2
      real*8 ttt,  xseco2
      xseco2 =   &
        flint(TTT,TQQ(1,1),TQQ(2,1),TQQ(3,1),QO2(K,1),QO2(K,2),QO2(K,3))
      return
      end FUNCTION xseco2       

      REAL*8 FUNCTION flint (TINT,T1,T2,T3,F1,F2,F3)
!-----------------------------------------------------------------------
!  Three-point linear interpolation function
!-----------------------------------------------------------------------
      real*8 TINT,T1,T2,T3,F1,F2,F3
      IF (TINT .LE. T2)  THEN
        IF (TINT .LE. T1)  THEN
          flint  = F1
        ELSE
          flint = F1 + (F2 - F1)*(TINT -T1)/(T2 -T1)
        ENDIF
      ELSE
        IF (TINT .GE. T3)  THEN
          flint  = F3
        ELSE
          flint = F2 + (F3 - F2)*(TINT -T2)/(T3 -T2)
        ENDIF
      ENDIF
      return
      end FUNCTION flint

      SUBROUTINE SPHERE(lpar)
!-----------------------------------------------------------------------
!  Calculation of spherical geometry; derive tangent heights, slant path
!  lengths and air mass factor for each layer. Not called when
!  SZA > 98 degrees.  Beyond 90 degrees, include treatment of emergent
!  beam (where tangent height is below altitude J-value desired at).
!-----------------------------------------------------------------------
!
!     GMU     MU, cos(solar zenith angle)
!     RZ      Distance from centre of Earth to each point (cm)
!     RQ      Square of radius ratios
!     TANHT   Tangent height for the current SZA
!     XL      Slant path between points
!     AMF     Air mass factor for slab between level and level above
!
!-----------------------------------------------------------------------

	USE module_fastj_data     
      
!jdf
      integer lpar
!jdf
      integer i, j, k, ii
      real*8 airmas, gmu, xmu1, xmu2, xl, diff
      REAL*8 Ux,H,RZ(NB),RQ(NB),ZBYR
!
!  Inlined air mass factor function for top of atmosphere
      AIRMAS(Ux,H) = (1.0d0+H)/SQRT(Ux*Ux+2.0d0*H*(1.0d0-   &
               0.6817d0*EXP(-57.3d0*ABS(Ux)/SQRT(1.0d0+5500.d0*H))/   &
                                                   (1.0d0+0.625d0*H)))
!
      GMU = U0
      RZ(1)=RAD+Z(1)
      ZBYR = ZZHT/RAD
      DO 2 II=2,NB
        RZ(II) = RAD + Z(II)
        RQ(II-1) = (RZ(II-1)/RZ(II))**2
    2 CONTINUE
      IF (GMU.LT.0.0D0) THEN
        TANHT = RZ(nlbatm)/DSQRT(1.0D0-GMU**2)
      ELSE
        TANHT = RZ(nlbatm)
      ENDIF
!
!  Go up from the surface calculating the slant paths between each level
!  and the level above, and deriving the appropriate Air Mass Factor
      DO 16 J=1,NB
        DO K=1,NB
          AMF(K,J)=0.D0
        ENDDO
!
!  Air Mass Factors all zero if below the tangent height
        IF (RZ(J).LT.TANHT) GOTO 16
!  Ascend from layer J calculating AMFs
        XMU1=ABS(GMU)
        DO 12 I=J,lpar
          XMU2=DSQRT(1.0D0-RQ(I)*(1.0D0-XMU1**2))
          XL=RZ(I+1)*XMU2-RZ(I)*XMU1
          AMF(I,J)=XL/(RZ(I+1)-RZ(I))
          XMU1=XMU2
   12   CONTINUE
!  Use function and scale height to provide AMF above top of model
        AMF(NB,J)=AIRMAS(XMU1,ZBYR)
!
!  Twilight case - Emergent Beam
        IF (GMU.GE.0.0D0) GOTO 16
        XMU1=ABS(GMU)
!  Descend from layer J
        DO 14 II=J-1,1,-1
          DIFF=RZ(II+1)*DSQRT(1.0D0-XMU1**2)-RZ(II)
          if(II.eq.1) DIFF=max(DIFF,0.d0)   ! filter
!  Tangent height below current level - beam passes through twice
          IF (DIFF.LT.0.0D0) THEN
            XMU2=DSQRT(1.0D0-(1.0D0-XMU1**2)/RQ(II))
            XL=ABS(RZ(II+1)*XMU1-RZ(II)*XMU2)
            AMF(II,J)=2.d0*XL/(RZ(II+1)-RZ(II))
            XMU1=XMU2
!  Lowest level intersected by emergent beam
          ELSE
            XL=RZ(II+1)*XMU1*2.0D0
!            WTING=DIFF/(RZ(II+1)-RZ(II))
!            AMF(II,J)=(1.0D0-WTING)*2.D0**XL/(RZ(II+1)-RZ(II))
            AMF(II,J)=XL/(RZ(II+1)-RZ(II))
            GOTO 16
          ENDIF
   14   CONTINUE
!
   16 CONTINUE
      RETURN
      END SUBROUTINE SPHERE


      SUBROUTINE OPMIE(isvode,jsvode,KW,WAVEL,XQO2,XQO3,FMEAN,lpar,jpnl, &
        ydgrd,sizeaer,extaer,waer,gaer,tauaer,l2,l3,l4,l5,l6,l7)
!-----------------------------------------------------------------------
!  NEW Mie code for J's, only uses 8-term expansion, 4-Gauss pts
!  Currently allow up to NP aerosol phase functions (at all altitudes) to
!  be associated with optical depth AER(1:NC) = aerosol opt.depth @ 1000 nm
!
!  Pick Mie-wavelength with phase function and Qext:
!
!  01 RAYLE = Rayleigh phase
!  02 ISOTR = isotropic
!  03 ABSRB = fully absorbing 'soot', wavelength indep.
!  04 S_Bkg = backgrnd stratospheric sulfate (n=1.46,log-norm:r=.09um/sigma=.6)
!  05 S_Vol = volcanic stratospheric sulfate (n=1.46,log-norm:r=.08um/sigma=.8)
!  06 W_H01 = water haze (H1/Deirm.) (n=1.335, gamma:  r-mode=0.1um /alpha=2)
!  07 W_H04 = water haze (H1/Deirm.) (n=1.335, gamma:  r-mode=0.4um /alpha=2)
!  08 W_C02 = water cloud (C1/Deirm.) (n=1.335, gamma:  r-mode=2.0um /alpha=6)
!  09 W_C04 = water cloud (C1/Deirm.) (n=1.335, gamma:  r-mode=4.0um /alpha=6)
!  10 W_C08 = water cloud (C1/Deirm.) (n=1.335, gamma:  r-mode=8.0um /alpha=6)
!  11 W_C13 = water cloud (C1/Deirm.) (n=1.335, gamma:  r-mode=13.3um /alpha=6)
!  12 W_L06 = water cloud (Lacis) (n=1.335, r-mode=5.5um / alpha=11/3)
!  13 Ice-H = hexagonal ice cloud (Mishchenko)
!  14 Ice-I = irregular ice cloud (Mishchenko)
!
!  Choice of aerosol index MIEDX is made in SET_AER; optical depths are
!  apportioned to the AER array in SET_PROF
!
!-----------------------------------------------------------------------
!  FUNCTION RAYLAY(WAVE)---RAYLEIGH CROSS-SECTION for wave > 170 nm
!       WSQI = 1.E6/(WAVE*WAVE)
!       REFRM1 = 1.0E-6*(64.328+29498.1/(146.-WSQI)+255.4/(41.-WSQI))
!       RAYLAY = 5.40E-21*(REFRM1*WSQI)**2
!-----------------------------------------------------------------------
!
!     DTAUX    Local optical depth of each CTM level
!     PIRAY    Contribution of Rayleigh scattering to extinction
!     PIAER    Contribution of Aerosol scattering to extinction
!     TTAU     Optical depth of air vertically above each point (to top of atm)
!     FTAU     Attenuation of solar beam
!     POMEGA   Scattering phase function
!     FMEAN    Mean actinic flux at desired levels
!
!-----------------------------------------------------------------------
      
        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	USE module_peg_util, only:  peg_message, peg_error_fatal    
!jdf
! Print Fast-J diagnostics if iprint /= 0
      integer, parameter :: iprint = 0
      integer, parameter :: single = 4        !compiler dependent value real*4
!     integer, parameter :: double = 8        !compiler dependent value real*8
      integer,parameter :: ipar_fastj=1,jpar=1
!     integer,parameter :: jppj=14        !Number of photolytic reactions supplied
      logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
      integer lpar           !Number of levels in CTM
      integer jpnl           !Number of levels requiring chemistry
      real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
      real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
      real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
      real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
      real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
      real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
      integer month_fastj        !  Number of month (1-12)
      integer iday_fastj         !  Day of year
      integer nspint           ! Num of spectral intervals across solar 
      parameter ( nspint = 4 ) ! spectrum for FAST-J
      real, dimension (nspint),save :: wavmid !cm
      real, dimension (nspint, kmaxd+1) :: sizeaer,extaer,waer,gaer,tauaer
      real, dimension (nspint, kmaxd+1) :: l2,l3,l4,l5,l6,l7
      data wavmid     &
          / 0.30e-4, 0.40e-4, 0.60e-4 ,0.999e-04 /
      INTEGER    NL, N__, M__
      PARAMETER (NL=500, N__=2*NL, M__=4)  !wig increased nl from 350 to 500, 31-Oct-2005
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer jndlev(lpar),jaddlv(nc),jaddto(nc+1)
      integer KW,km,i,j,k,l,ix,j1
      integer isvode,jsvode
      real*8 QXMIE(MX),XLAER(MX),SSALB(MX)
      real*8 xlo2,xlo3,xlray,xltau,zk,taudn,tauup,zk2
      real*8 WAVEL,XQO2(NB),XQO3(NB),FMEAN(lpar),POMEGAJ(2*M__,NC+1)
      real*8 DTAUX(NB),PIRAY(NB),PIAER(MX,NB),TTAU(NC+1),FTAU(NC+1)
      real*8 ftaulog,dttau,dpomega(2*M__)
      real*8 ftaulog2,dttau2,dpomega2(2*M__)
! JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
	real*8 PIAER_MX1(NB)
! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
	 character*80 msg
!
!---Pick nearest Mie wavelength, no interpolation--------------
                              KM=1
      if( WAVEL .gt. 355.d0 ) KM=2
      if( WAVEL .gt. 500.d0 ) KM=3
!     if( WAVEL .gt. 800.d0 ) KM=4  !drop the 1000 nm wavelength
!
!---For Mie code scale extinction at 1000 nm to wavelength WAVEL (QXMIE)
! define angstrom exponent
! JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
	ang=log(QAA(1,MIEDX(1))/QAA(4,MIEDX(1)))/log(300./999.)
       do I=1,MX
! QAA is extinction efficiency
         QXMIE(I) = QAA(KM,MIEDX(I))/QAA(4,MIEDX(I))
! scale to 550 nm using angstrom relationship
! note that this gives QXMIE at 550.0 nm = 1.0, aerosol optical thickness
! is defined at 550 nm
! convention -- I = 1 is aerosol, I > 1 are clouds
         if(I.eq.1) QXMIE(I) = (WAVEL/550.0)**ang
         SSALB(I) = SSA(KM,MIEDX(I)) ! single scattering albedo
      enddo
! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
!
!---Reinitialize arrays
      do j=1,nc+1
        ttau(j)=0.d0
        ftau(j)=0.d0
      enddo
!
!---Set up total optical depth over each CTM level, DTAUX
      J1 = NLBATM
      do J=J1,NB
        XLO3=DO3(J)*XQO3(J)
        XLO2=DM(J)*XQO2(J)*0.20948d0
        XLRAY=DM(J)*QRAYL(KW)
!  Zero absorption for testing purposes
!        call NOABS(XLO3,XLO2,XLRAY,AER(1,j),RFLECT)
        do I=1,MX
! I is aerosol type, j is level, AER(I,J)*QXMIE(I) is the layer aerosol optical thickness
! at 1000 nm , AER(I,J), times extinction efficiency for the layer (normalized to be one at 1000 nm)
! therefore xlaer(i) is the layer optical depth at the wavelength index KM
          XLAER(I)=AER(I,J)*QXMIE(I)
        enddo
!  Total optical depth from all elements
        DTAUX(J)=XLO3+XLO2+XLRAY
        do I=1,MX
          DTAUX(J)=DTAUX(J)+XLAER(I)
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf xlaer',&
!       i,j,km,dtaux(j),xlaer(i),aer(i,j),qxmie(i),xlo3,xlo2,xlray
        enddo
! JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
! add in new aerosol information from Mie code
! layer aerosol optical thickness at wavelength index KM, layer j
!	tauaer(km,j)=0.0
	dtaux(j)=dtaux(j)+tauaer(km,j)
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf dtaux',&
!       j,km,dtaux(j),tauaer(km,j)
! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
!  Fractional extinction for Rayleigh scattering and each aerosol type
        PIRAY(J)=XLRAY/DTAUX(J)
        do I=1,MX
          PIAER(I,J)=SSALB(I)*XLAER(I)/DTAUX(J)
        enddo
! JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
! note the level is now important
        PIAER_MX1(J)=waer(km,j)*tauaer(km,j)/DTAUX(J)
! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
      enddo ! of the level "j" loop
!
!---Define the scattering phase fn. with mix of Rayleigh(1) & Mie(MIEDX)
!   No. of quadrature pts fixed at 4 (M__), expansion of phase fn @ 8
      N = M__
      MFIT = 2*M__
      do j=j1,NB  ! jcb: layer index
        do i=1,MFIT
          pomegaj(i,j) = PIRAY(J)*PAA(I,KM,1) ! jcb: paa are the expansion coefficients of the phase function
          do k=1,MX ! jcb: mx is # of aerosols
            pomegaj(i,j) = pomegaj(i,j) + PIAER(K,J)*PAA(I,KM,MIEDX(K))
          enddo
        enddo
! JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ
! i is the # of coefficients, KM is the wavelength index, j is the level
! note the level in now relevant because we allow aerosol properties to
! vary by level
           pomegaj(1,j) = pomegaj(1,j) + PIAER_MX1(J)*1.0 ! 1.0 is l0
           pomegaj(2,j) = pomegaj(2,j) + PIAER_MX1(J)*gaer(KM,j)*3.0 ! the three converts gear to l1
           pomegaj(3,j) = pomegaj(3,j) + PIAER_MX1(J)*l2(KM,j)
           pomegaj(4,j) = pomegaj(4,j) + PIAER_MX1(J)*l3(KM,j)
           pomegaj(5,j) = pomegaj(5,j) + PIAER_MX1(J)*l4(KM,j)
           pomegaj(6,j) = pomegaj(6,j) + PIAER_MX1(J)*l5(KM,j)
           pomegaj(7,j) = pomegaj(7,j) + PIAER_MX1(J)*l6(KM,j)
           pomegaj(8,j) = pomegaj(7,j) + PIAER_MX1(J)*l7(KM,j)
! BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB

      enddo
!
!---Calculate attenuated incident beam EXP(-TTAU/U0) and flux on surface
      do J=J1,NB
        if(AMF(J,J).gt.0.0D0) then
          XLTAU=0.0D0
          do I=1,NB
            XLTAU=XLTAU + DTAUX(I)*AMF(I,J)
          enddo
          if(XLTAU.gt.450.d0) then   ! for compilers with no underflow trapping
            FTAU(j)=0.d0
          else
            FTAU(J)=DEXP(-XLTAU)
          endif
        else
          FTAU(J)=0.0D0
        endif
      enddo
      if(U0.gt.0.D0) then
        ZFLUX = U0*FTAU(J1)*RFLECT/(1.d0+RFLECT)
      else
        ZFLUX = 0.d0
      endif
!
!------------------------------------------------------------------------
!  Take optical properties on CTM layers and convert to a photolysis
!  level grid corresponding to layer centres and boundaries. This is
!  required so that J-values can be calculated for the centre of CTM
!  layers; the index of these layers is kept in the jndlev array.
!------------------------------------------------------------------------
!
!  Set lower boundary and levels to calculate J-values at
      J1=2*J1-1
      do j=1,lpar
        jndlev(j)=2*j
      enddo
!
!  Calculate column optical depths above each level, TTAU
      TTAU(NC+1)=0.0D0
      do J=NC,J1,-1
        I=(J+1)/2
        TTAU(J)=TTAU(J+1) + 0.5d0*DTAUX(I)
        jaddlv(j)=int(0.5d0*DTAUX(I)/dtaumax)
!  Subdivide cloud-top levels if required
        if(jadsub(j).gt.0) then
          jadsub(j)=min(jaddlv(j)+1,nint(dtausub))*(nint(dsubdiv)-1)
          jaddlv(j)=jaddlv(j)+jadsub(j)
        endif
!       if(isvode.eq.8.and.jsvode.eq.1) print*,'jdf in opmie',&
!       j,jadsub(j),jaddlv(j),ttau(j),dtaux(i)
      enddo
!
!  Calculate attenuated beam, FTAU, level boundaries then level centres
      FTAU(NC+1)=1.0D0
      do J=NC-1,J1,-2
        I=(J+1)/2
        FTAU(J)=FTAU(I)
      enddo
      do J=NC,J1,-2
        FTAU(J)=sqrt(FTAU(J+1)*FTAU(J-1))
      enddo
!
!  Calculate scattering properties, level centres then level boundaries
!  using an inverse interpolation to give correctly-weighted values
      do j=NC,J1,-2
        do i=1,MFIT
          pomegaj(i,j) = pomegaj(i,j/2)
        enddo
      enddo
      do j=J1+2,nc,2
        taudn = ttau(j-1)-ttau(j)
        tauup = ttau(j)-ttau(j+1)
        do i=1,MFIT
          pomegaj(i,j) = (pomegaj(i,j-1)*taudn +   &
                          pomegaj(i,j+1)*tauup) / (taudn+tauup)
        enddo
      enddo
!  Define lower and upper boundaries
      do i=1,MFIT
        pomegaj(i,J1)   = pomegaj(i,J1+1)
        pomegaj(i,nc+1) = pomegaj(i,nc)
      enddo
!
!------------------------------------------------------------------------
!  Calculate cumulative total and define levels we want J-values at.
!  Sum upwards for levels, and then downwards for Mie code readjustments.
!
!     jaddlv(i)   Number of new levels to add between (i) and (i+1)
!     jaddto(i)   Total number of new levels to add to and above level (i)
!     jndlev(j)   Level needed for J-value for CTM layer (j)
!
!------------------------------------------------------------------------
!
!  Reinitialize level arrays
      do j=1,nc+1
        jaddto(j)=0
      enddo
!
      jaddto(J1)=jaddlv(J1)
      do j=J1+1,nc
        jaddto(j)=jaddto(j-1)+jaddlv(j)
      enddo
      if((jaddto(nc)+nc).gt.nl) then
!         print*,'jdf mie',isvode,jsvode,jaddto(nc),nc,nl
         write ( msg, '(a, 2i6)' )  &
           'FASTJ  Max NL exceeded '  //  &
           'jaddto(nc)+nc NL', jaddto(nc)+nc,NL
         call peg_message( lunerr, msg )
         msg = 'FASTJ subr OPMIE error. Max NL exceeded'
         call peg_error_fatal( lunerr, msg )      
!         write(6,1500)  jaddto(nc)+nc, 'NL',NL
!         stop
      endif
      do i=1,lpar
        jndlev(i)=jndlev(i)+jaddto(jndlev(i)-1)
      enddo
      jaddto(nc)=jaddlv(nc)
      do j=nc-1,J1,-1
        jaddto(j)=jaddto(j+1)+jaddlv(j)
      enddo
!
!---------------------SET UP FOR MIE CODE-------------------------------
!
!  Transpose the ascending TTAU grid to a descending ZTAU grid.
!  Double the resolution - TTAU points become the odd points on the
!  ZTAU grid, even points needed for asymm phase fn soln, contain 'h'.
!  Odd point added at top of grid for unattenuated beam   (Z='inf')
!
!        Surface:   TTAU(1)   now use ZTAU(2*NC+1)
!        Top:       TTAU(NC)  now use ZTAU(3)
!        Infinity:            now use ZTAU(1)
!
!  Mie scattering code only used from surface to level NC
!------------------------------------------------------------------------
!
!  Initialise all Fast-J optical property arrays
      do k=1,N__
        do i=1,MFIT
          pomega(i,k) = 0.d0
        enddo
        ztau(k) = 0.d0
        fz(k)   = 0.d0
      enddo
!
!  Ascend through atmosphere transposing grid and adding extra points
      do j=J1,nc+1
        k = 2*(nc+1-j)+2*jaddto(j)+1
        ztau(k)= ttau(j)
        fz(k)  = ftau(j)
        do i=1,MFIT
          pomega(i,k) = pomegaj(i,j)
        enddo
      enddo
!
!  Check profiles if desired
!      ND = 2*(NC+jaddto(J1)-J1)  + 3
!      if(kw.eq.1) call CH_PROF
!
!------------------------------------------------------------------------
!    Insert new levels, working downwards from the top of the atmosphere
!  to the surface (down in 'j', up in 'k'). This allows ztau and pomega
!  to be incremented linearly (in a +ve sense), and the flux fz to be
!  attenuated top-down (avoiding problems where lower level fluxes are
!  zero).
!
!    zk        fractional increment in level
!    dttau     change in ttau per increment    (linear, positive)
!    dpomega   change in pomega per increment  (linear)
!    ftaulog   change in ftau per increment    (exponential, normally < 1)
!
!------------------------------------------------------------------------
!
      do j=nc,J1,-1
          zk = 0.5d0/(1.d0+dble(jaddlv(j)-jadsub(j)))
          dttau = (ttau(j)-ttau(j+1))*zk
          do i=1,MFIT
            dpomega(i) = (pomegaj(i,j)-pomegaj(i,j+1))*zk
          enddo
!  Filter attenuation factor - set minimum at 1.0d-05
          if(ftau(j+1).eq.0.d0) then
            ftaulog=0.d0
          else
            ftaulog = ftau(j)/ftau(j+1)
            if(ftaulog.lt.1.d-150) then
              ftaulog=1.0d-05
            else
              ftaulog=exp(log(ftaulog)*zk)
            endif
          endif
          k = 2*(nc-j+jaddto(j)-jaddlv(j))+1   !  k at level j+1
          l = 0
!  Additional subdivision of first level if required
          if(jadsub(j).ne.0) then
            l=jadsub(j)/nint(dsubdiv-1)
            zk2=1.d0/dsubdiv
            dttau2=dttau*zk2
            ftaulog2=ftaulog**zk2
            do i=1,MFIT
              dpomega2(i)=dpomega(i)*zk2
            enddo
            do ix=1,2*(jadsub(j)+l)
              ztau(k+1) = ztau(k) + dttau2
              fz(k+1) = fz(k)*ftaulog2
              do i=1,MFIT
                pomega(i,k+1) = pomega(i,k) + dpomega2(i)
              enddo
              k = k+1
            enddo
          endif
          l = 2*(jaddlv(j)-jadsub(j)-l)+1
!
!  Add values at all intermediate levels
          do ix=1,l
            ztau(k+1) = ztau(k) + dttau
            fz(k+1) = fz(k)*ftaulog
            do i=1,MFIT
              pomega(i,k+1) = pomega(i,k) + dpomega(i)
            enddo
            k = k+1
          enddo
!
!  Alternate method to attenuate fluxes, fz, using 2nd-order finite
!  difference scheme - just need to comment in section below
!          ix = 2*(jaddlv(j)-jadsub(j))+1
!          if(l.le.0) then
!            l=k-ix-1
!          else
!            l=k-ix
!          endif
!          call efold(ftau(j+1),ftau(j),ix+1,fz(l))
!          if(jadsub(j).ne.0) then
!            k = 2*(nc-j+jaddto(j)-jaddlv(j))+1 !  k at level j+1
!            ix=2*(jadsub(j)+(jadsub(j)/nint(dsubdiv-1)))
!            call efold(ftau(j+1),fz(k+ix),ix,fz(k))
!          endif
!
      enddo
!
!---Update total number of levels and check doesn't exceed N__
      ND = 2*(NC+jaddto(J1)-J1)  + 3
      if(nd.gt.N__) then      
         write ( msg, '(a, 2i6)' )  &
           'FASTJ  Max N__ exceeded '  //  &
           'ND N__', ND, N__
         call peg_message( lunerr, msg )
         msg = 'FASTJ subr OPMIE error. Max N__ exceeded'
         call peg_error_fatal( lunerr, msg )
!        write(6,1500) ND, 'N__',N__
!        stop
      endif
!
!---Add boundary/ground layer to ensure no negative J's caused by
!---too large a TTAU-step in the 2nd-order lower b.c.
      ZTAU(ND+1) = ZTAU(ND)*1.000005d0
      ZTAU(ND+2) = ZTAU(ND)*1.000010d0
      zk=max(abs(U0),0.01d0)
      zk=dexp(-ZTAU(ND)*5.d-6/zk)
      FZ(ND+1) = FZ(ND)*zk
      FZ(ND+2) = FZ(ND+1)*zk
      do I=1,MFIT
        POMEGA(I,ND+1)   = POMEGA(I,ND)
        POMEGA(I,ND+2)   = POMEGA(I,ND)
      enddo
      ND = ND+2
!
      ZU0 = U0
      ZREFL = RFLECT
!
!-----------------------------------------
      CALL MIESCT(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU,ZFLUX, &
        ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!-----------------------------------------
!  Accumulate attenuation for selected levels
      l=2*(NC+jaddto(J1))+3
      do j=1,lpar
        k=l-(2*jndlev(j))
        if(k.gt.ND-2) then
          FMEAN(j) = 0.d0
        else
          FMEAN(j) = FJ(k)
        endif
      enddo
!
      return
 1000 format(1x,i3,3(2x,1pe10.4),1x,i3)
 1300 format(1x,50(i3))
 1500 format(' Too many levels in photolysis code: need ',i3,' but ',a,   &
             ' dimensioned as ',i3)
      END SUBROUTINE OPMIE                          

!*********************************************************************
      subroutine EFOLD (F0, F1, N, F)
!-----------------------------------------------------------------------
!---  calculate the e-fold between two boundaries, given the value
!---     at both boundaries F0(x=0) = top, F1(x=1) = bottom.
!---  presume that F(x) proportional to exp[-A*x] for x=0 to x=1
!---          d2F/dx2 = A*A*F  and thus expect F1 = F0 * exp[-A]
!---           alternatively, could define A = ln[F0/F1]
!---  let X = A*x, d2F/dX2 = F
!---  assume equal spacing (not necessary, but makes this easier)
!---      with N-1 intermediate points (and N layers of thickness dX = A/N)
!---
!---  2nd-order finite difference:  (F(i-1) - 2F(i) + F(i+1)) / dX*dX = F(i)
!---      let D = 1 / dX*dX:
!
!  1  |   1        0        0        0        0        0   |    | F0 |
!     |                                                    |    | 0  |
!  2  |  -D      2D+1      -D        0        0        0   |    | 0  |
!     |                                                    |    | 0  |
!  3  |   0       -D      2D+1      -D        0        0   |    | 0  |
!     |                                                    |    | 0  |
!     |   0        0       -D      2D+1      -D        0   |    | 0  |
!     |                                                    |    | 0  |
!  N  |   0        0        0       -D      2D+1      -D   |    | 0  |
!     |                                                    |    | 0  |
! N+1 |   0        0        0        0        0        1   |    | F1 |
!
!-----------------------------------------------------------------------
!  Advantage of scheme over simple attenuation factor: conserves total
!  number of photons - very useful when using scheme for heating rates.
!  Disadvantage: although reproduces e-folds very well for small flux
!  differences, starts to drift off when many orders of magnitude are
!  involved.
!-----------------------------------------------------------------------
      implicit none
      real*8 F0,F1,F(250)  !F(N+1)
      integer N
      integer I
      real*8 A,DX,D,DSQ,DDP1, B(101),R(101)
!
      if(F0.eq.0.d0) then
        do I=1,N
          F(I)=0.d0
        enddo
        return
      elseif(F1.eq.0.d0) then
        A = DLOG(F0/1.d-250)
      else
        A = DLOG(F0/F1)
      endif
!
      DX = float(N)/A
      D = DX*DX
      DSQ = D*D
      DDP1 = D+D+1.d0
!
      B(2) = DDP1
      R(2) = +D*F0
      do I=3,N
        B(I) = DDP1 - DSQ/B(I-1)
        R(I) = +D*R(I-1)/B(I-1)
      enddo
      F(N+1) = F1
      do I=N,2,-1
        F(I) = (R(I) + D*F(I+1))/B(I)
      enddo
      F(1) = F0
      return
      end subroutine EFOLD               


      subroutine CH_PROF(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
        ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!-----------------------------------------------------------------------
!  Check profiles to be passed to MIESCT
!-----------------------------------------------------------------------

      USE module_peg_util, only:  peg_message
      
      implicit none
!jdf
      integer, parameter :: single = 4      !compiler dependent value real*4
      integer, parameter :: double = 8      !compiler dependent value real*8
      INTEGER    NL, N__, M__
      PARAMETER (NL=350, N__=2*NL, M__=4)
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer i,j
      character*80 msg
!      write(6,1100) 'lev','ztau','fz  ','pomega( )'
      do i=1,ND
        if(ztau(i).ne.0.d0) then
          write ( msg, '(a, i3, 2(1x,1pe9.3))' ) 	&
              'FASTJ subr CH_PROF ztau ne 0. check pomega. ' //  &
              'k ztau(k) fz(k) ', i,ztau(i),fz(i)
          call peg_message( lunerr, msg )
!          write(6,1200) i,ztau(i),fz(i),(pomega(j,i),j=1,8)
        endif
      enddo
      return
 1100 format(1x,a3,4(a9,2x))
 1200 format(1x,i3,11(1x,1pe9.3))
      end subroutine CH_PROF


      SUBROUTINE MIESCT(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
        ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!     SUBROUTINE MIESCT
!-----------------------------------------------------------------------
!   This is an adaption of the Prather radiative transfer code, (mjp, 10/95)
!     Prather, 1974, Astrophys. J. 192, 787-792.
!         Sol'n of inhomogeneous Rayleigh scattering atmosphere.
!         (original Rayleigh w/ polarization)
!     Cochran and Trafton, 1978, Ap.J., 219, 756-762.
!         Raman scattering in the atmospheres of the major planets.
!         (first use of anisotropic code)
!     Jacob, Gottlieb and Prather, 1989, J.Geophys.Res., 94, 12975-13002.
!         Chemistry of a polluted cloudy boundary layer,
!         (documentation of extension to anisotropic scattering)
!
!    takes atmospheric structure and source terms from std J-code
!    ALSO limited to 4 Gauss points, only calculates mean field!
!
!   mean rad. field ONLY (M=1)
!   initialize variables FIXED/UNUSED in this special version:
!   FTOP = 1.0 = astrophysical flux (unit of pi) at SZA, -ZU0, use for scaling
!   FBOT = 0.0 = external isotropic flux on lower boundary
!   SISOTP = 0.0 = Specific Intensity of isotropic radiation incident from top
!
!   SUBROUTINES:  MIESCT              needs 'jv_mie.cmn'
!                 BLKSLV              needs 'jv_mie.cmn'
!                 GEN (ID)            needs 'jv_mie.cmn'
!                 LEGND0 (X,PL,N)
!                 MATIN4 (A)
!                 GAUSSP (N,XPT,XWT)
!-----------------------------------------------------------------------
 
!      INCLUDE 'jv_mie.h'
      
       implicit none    
!jdf
      integer, parameter :: single = 4      !compiler dependent value real*4
      integer, parameter :: double = 8      !compiler dependent value real*8
      INTEGER    NL, N__, M__
      PARAMETER (NL=350, N__=2*NL, M__=4)
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer i, id, im
      real*8  cmeq1
!-----------------------------------------------------------------------
!---fix scattering to 4 Gauss pts = 8-stream
      CALL GAUSSP (N,EMU,WT)
!---solve eqn of R.T. only for first-order M=1
!      ZFLUX = (ZU0*FZ(ND)*ZREFL+FBOT)/(1.0d0+ZREFL)
      ZFLUX = (ZU0*FZ(ND)*ZREFL)/(1.0d0+ZREFL)
      M=1
      DO I=1,N
        CALL LEGND0 (EMU(I),PM0,MFIT)
        DO IM=M,MFIT
          PM(I,IM) = PM0(IM)
        ENDDO
      ENDDO
!
      CMEQ1 = 0.25D0
      CALL LEGND0 (-ZU0,PM0,MFIT)
      DO IM=M,MFIT
        PM0(IM) = CMEQ1*PM0(IM)
      ENDDO
!
!     CALL BLKSLV
      CALL BLKSLV(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU,ZFLUX, &
        ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!
      DO ID=1,ND,2
        FJ(ID) = 4.0d0*FJ(ID) + FZ(ID)
      ENDDO

      RETURN
      END SUBROUTINE MIESCT

      SUBROUTINE BLKSLV(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
        ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!-----------------------------------------------------------------------
!  Solves the block tri-diagonal system:
!               A(I)*X(I-1) + B(I)*X(I) + C(I)*X(I+1) = H(I)
!-----------------------------------------------------------------------
!      INCLUDE 'jv_mie.h'

	implicit none
!jdf
      integer, parameter :: single = 4      !compiler dependent value real*4
      integer, parameter :: double = 8      !compiler dependent value real*8
      INTEGER    NL, N__, M__
      PARAMETER (NL=350, N__=2*NL, M__=4)
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer i, j, k, id
      real*8  thesum
!-----------UPPER BOUNDARY ID=1
      CALL GEN(1,ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU,ZFLUX, &
        ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
      CALL MATIN4 (B)
      DO I=1,N
         RR(I,1) = 0.0d0
        DO J=1,N
          THESUM = 0.0d0
         DO K=1,N
          THESUM = THESUM - B(I,K)*CC(K,J)
         ENDDO
         DD(I,J,1) = THESUM
         RR(I,1) = RR(I,1) + B(I,J)*H(J)
        ENDDO
      ENDDO
!----------CONTINUE THROUGH ALL DEPTH POINTS ID=2 TO ID=ND-1
      DO ID=2,ND-1
        CALL GEN(ID,ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU,ZFLUX, &
          ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
        DO I=1,N
          DO J=1,N
          B(I,J) = B(I,J) + A(I)*DD(I,J,ID-1)
          ENDDO
          H(I) = H(I) - A(I)*RR(I,ID-1)
        ENDDO
        CALL MATIN4 (B)
        DO I=1,N
          RR(I,ID) = 0.0d0
          DO J=1,N
          RR(I,ID) = RR(I,ID) + B(I,J)*H(J)
          DD(I,J,ID) = - B(I,J)*C1(J)
          ENDDO
        ENDDO
      ENDDO
!---------FINAL DEPTH POINT: ND
      CALL GEN(ND,ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU,ZFLUX, &
        ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
      DO I=1,N
        DO J=1,N
          THESUM = 0.0d0
          DO K=1,N
          THESUM = THESUM + AA(I,K)*DD(K,J,ND-1)
          ENDDO
        B(I,J) = B(I,J) + THESUM
        H(I) = H(I) - AA(I,J)*RR(J,ND-1)
        ENDDO
      ENDDO
      CALL MATIN4 (B)
      DO I=1,N
        RR(I,ND) = 0.0d0
        DO J=1,N
        RR(I,ND) = RR(I,ND) + B(I,J)*H(J)
        ENDDO
      ENDDO
!-----------BACK SOLUTION
      DO ID=ND-1,1,-1
       DO I=1,N
        DO J=1,N
         RR(I,ID) = RR(I,ID) + DD(I,J,ID)*RR(J,ID+1)
        ENDDO
       ENDDO
      ENDDO
!----------MEAN J & H
      DO ID=1,ND,2
        FJ(ID) = 0.0d0
       DO I=1,N
        FJ(ID) = FJ(ID) + RR(I,ID)*WT(I)
       ENDDO
      ENDDO
      DO ID=2,ND,2
        FJ(ID) = 0.0d0
       DO I=1,N
        FJ(ID) = FJ(ID) + RR(I,ID)*WT(I)*EMU(I)
       ENDDO
      ENDDO
! Output fluxes for testing purposes
!      CALL CH_FLUX
!      CALL CH_FLUX(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
!       ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!
      RETURN
      END SUBROUTINE BLKSLV


      SUBROUTINE CH_FLUX(ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
        ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!-----------------------------------------------------------------------
!  Diagnostic routine to check fluxes at each level - makes most sense
!  when running a conservative atmosphere (zero out absorption in
!  OPMIE by calling the NOABS routine below)
!-----------------------------------------------------------------------

!      INCLUDE 'jv_mie.h'
      	IMPLICIT NONE
!jdf
      integer, parameter :: single = 4      !compiler dependent value real*4
      integer, parameter :: double = 8      !compiler dependent value real*8
      INTEGER    NL, N__, M__
      PARAMETER (NL=350, N__=2*NL, M__=4)
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer I,ID
      real*8 FJCHEK(N__),FZMEAN
!
!  Odd (h) levels held as actinic flux, so recalculate irradiances
      DO ID=1,ND,2
        FJCHEK(ID) = 0.0d0
       DO I=1,N
        FJCHEK(ID) = FJCHEK(ID) + RR(I,ID)*WT(I)*EMU(i)
       ENDDO
      ENDDO
!
!  Even (j) levels are already held as irradiances
      DO ID=2,ND,2
       DO I=1,N
        FJCHEK(ID) = FJ(ID)
       ENDDO
      ENDDO
!
!  Output Downward and Upward fluxes down through atmosphere
!     WRITE(6,1200)
      WRITE(34,1200)
      DO ID=2,ND,2
        FZMEAN=sqrt(FZ(ID)*FZ(ID-1))
!       WRITE(6,1000) ID, ZU0*FZMEAN-2.0*(FJCHEK(id)-FJCHEK(id-1)),   &
        WRITE(34,1000) ID, ZU0*FZMEAN-2.0*(FJCHEK(id)-FJCHEK(id-1)),   &
                                     2.0*(FJCHEK(id)+FJCHEK(id-1)),   &
                                     2.0*(FJCHEK(id)+FJCHEK(id-1))/   &
                         (ZU0*FZMEAN-2.0*(FJCHEK(id)-FJCHEK(id-1)))
      ENDDO
      RETURN
 1000 FORMAT(1x,i3,1p,2E12.4,1x,0p,f9.4)
 1200 FORMAT(1x,'Lev',3x,'Downward',4x,'Upward',7x,'Ratio')
      END SUBROUTINE CH_FLUX

      SUBROUTINE NOABS(XLO3,XLO2,XLRAY,BCAER,RFLECT)
!-----------------------------------------------------------------------
!  Zero out absorption terms to check scattering code. Leave a little
!  Rayleigh to provide a minimal optical depth, and set surface albedo
!  to unity.
!-----------------------------------------------------------------------
      IMPLICIT NONE
      real*8 XLO3,XLO2,XLRAY,BCAER,RFLECT
      XLO3=0.d0
      XLO2=0.d0
      XLRAY=XLRAY*1.d-10
      BCAER=0.d0
      RFLECT=1.d0
      RETURN
      END SUBROUTINE NOABS                              

      SUBROUTINE GEN(ID,ND,N,M,MFIT,POMEGA,PM,PM0,FZ,WT,EMU,ZTAU, &
        ZFLUX,ZREFL,FJ,A,C1,H,V1,B,AA,CC,S,W,U1,DD,RR,RADIUS,ZU0)
!-----------------------------------------------------------------------
!  Generates coefficient matrices for the block tri-diagonal system:
!               A(I)*X(I-1) + B(I)*X(I) + C(I)*X(I+1) = H(I)
!-----------------------------------------------------------------------

!      INCLUDE 'jv_mie.h'
	IMPLICIT NONE
!jdf
      integer, parameter :: single = 4      !compiler dependent value real*4
      integer, parameter :: double = 8      !compiler dependent value real*8
      INTEGER    NL, N__, M__
      PARAMETER (NL=350, N__=2*NL, M__=4)
      REAL(kind=double), dimension(M__) :: A,C1,H,V1,WT,EMU
      REAL(kind=double), dimension(M__,M__) :: B,AA,CC,S,W,U1
      REAL(kind=double), dimension(M__,2*M__) :: PM
      REAL(kind=double), dimension(2*M__) :: PM0
      REAL(kind=double), dimension(2*M__,N__) :: POMEGA
      REAL(kind=double), dimension(N__) :: ZTAU, FZ, FJ
      REAL(kind=double), dimension(M__,M__,N__) :: DD
      REAL(kind=double), dimension(M__,N__) :: RR
      REAL(kind=double) :: ZREFL,ZFLUX,RADIUS,ZU0
      INTEGER ND,N,M,MFIT
!jdf
      integer id, id0, id1, im, i, j, k, mstart
      real*8  sum0, sum1, sum2, sum3
      real*8  deltau, d1, d2, surfac
!---------------------------------------------
      IF(ID.EQ.1 .OR. ID.EQ.ND) THEN
!---------calculate generic 2nd-order terms for boundaries
       ID0 = ID
       ID1 = ID+1
       IF(ID.GE.ND) ID1 = ID-1
       DO 10 I=1,N
          SUM0 = 0.0d0
          SUM1 = 0.0d0
          SUM2 = 0.0d0
          SUM3 = 0.0d0
        DO IM=M,MFIT,2
          SUM0 = SUM0 + POMEGA(IM,ID0)*PM(I,IM)*PM0(IM)
          SUM2 = SUM2 + POMEGA(IM,ID1)*PM(I,IM)*PM0(IM)
        ENDDO
        DO IM=M+1,MFIT,2
          SUM1 = SUM1 + POMEGA(IM,ID0)*PM(I,IM)*PM0(IM)
          SUM3 = SUM3 + POMEGA(IM,ID1)*PM(I,IM)*PM0(IM)
        ENDDO
         H(I) = 0.5d0*(SUM0*FZ(ID0) + SUM2*FZ(ID1))
         A(I) = 0.5d0*(SUM1*FZ(ID0) + SUM3*FZ(ID1))
        DO J=1,I
          SUM0 = 0.0d0
          SUM1 = 0.0d0
          SUM2 = 0.0d0
          SUM3 = 0.0d0
         DO IM=M,MFIT,2
          SUM0 = SUM0 + POMEGA(IM,ID0)*PM(I,IM)*PM(J,IM)
          SUM2 = SUM2 + POMEGA(IM,ID1)*PM(I,IM)*PM(J,IM)
         ENDDO
         DO IM=M+1,MFIT,2
          SUM1 = SUM1 + POMEGA(IM,ID0)*PM(I,IM)*PM(J,IM)
          SUM3 = SUM3 + POMEGA(IM,ID1)*PM(I,IM)*PM(J,IM)
         ENDDO
         S(I,J) = - SUM2*WT(J)
         S(J,I) = - SUM2*WT(I)
         W(I,J) = - SUM1*WT(J)
         W(J,I) = - SUM1*WT(I)
         U1(I,J) = - SUM3*WT(J)
         U1(J,I) = - SUM3*WT(I)
          SUM0 = 0.5d0*(SUM0 + SUM2)
         B(I,J) = - SUM0*WT(J)
         B(J,I) = - SUM0*WT(I)
        ENDDO
         S(I,I) = S(I,I) + 1.0d0
         W(I,I) = W(I,I) + 1.0d0
         U1(I,I) = U1(I,I) + 1.0d0
         B(I,I) = B(I,I) + 1.0d0
   10  CONTINUE
       DO I=1,N
         SUM0 = 0.0d0
        DO J=1,N
         SUM0 = SUM0 + S(I,J)*A(J)/EMU(J)
        ENDDO
        C1(I) = SUM0
       ENDDO
       DO I=1,N
        DO J=1,N
          SUM0 = 0.0d0
          SUM2 = 0.0d0
         DO K=1,N
          SUM0 = SUM0 + S(J,K)*W(K,I)/EMU(K)
          SUM2 = SUM2 + S(J,K)*U1(K,I)/EMU(K)
         ENDDO
         A(J) = SUM0
         V1(J) = SUM2
        ENDDO
        DO J=1,N
         W(J,I) = A(J)
         U1(J,I) = V1(J)
        ENDDO
       ENDDO
       IF (ID.EQ.1) THEN
!-------------upper boundary, 2nd-order, C-matrix is full (CC)
        DELTAU = ZTAU(2) - ZTAU(1)
        D2 = 0.25d0*DELTAU
        DO I=1,N
          D1 = EMU(I)/DELTAU
          DO J=1,N
           B(I,J) = B(I,J) + D2*W(I,J)
           CC(I,J) = D2*U1(I,J)
          ENDDO
          B(I,I) = B(I,I) + D1
          CC(I,I) = CC(I,I) - D1
!         H(I) = H(I) + 2.0d0*D2*C1(I) + D1*SISOTP
          H(I) = H(I) + 2.0d0*D2*C1(I)
          A(I) = 0.0d0
        ENDDO
       ELSE
!-------------lower boundary, 2nd-order, A-matrix is full (AA)
        DELTAU = ZTAU(ND) - ZTAU(ND-1)
        D2 = 0.25d0*DELTAU
        SURFAC = 4.0d0*ZREFL/(1.0d0 + ZREFL)
        DO I=1,N
          D1 = EMU(I)/DELTAU
          H(I) = H(I) - 2.0d0*D2*C1(I)
           SUM0 = 0.0d0
          DO J=1,N
           SUM0 = SUM0 + W(I,J)
          ENDDO
           SUM0 = D1 + D2*SUM0
           SUM1 = SURFAC*SUM0
          DO J=1,N
           B(I,J) = B(I,J) + D2*W(I,J) - SUM1*EMU(J)*WT(J)
          ENDDO
          B(I,I) = B(I,I) + D1
          H(I) = H(I) + SUM0*ZFLUX
          DO J=1,N
           AA(I,J) = - D2*U1(I,J)
          ENDDO
           AA(I,I) = AA(I,I) + D1
           C1(I) = 0.0d0
        ENDDO
       ENDIF
!------------intermediate points:  can be even or odd, A & C diagonal
      ELSE
        DELTAU = ZTAU(ID+1) - ZTAU(ID-1)
        MSTART = M + MOD(ID+1,2)
        DO I=1,N
          A(I) = EMU(I)/DELTAU
          C1(I) = -A(I)
           SUM0 = 0.0d0
          DO IM=MSTART,MFIT,2
           SUM0 = SUM0 + POMEGA(IM,ID)*PM(I,IM)*PM0(IM)
          ENDDO
          H(I) = SUM0*FZ(ID)
          DO J=1,I
            SUM0 = 0.0d0
           DO IM=MSTART,MFIT,2
            SUM0 = SUM0 + POMEGA(IM,ID)*PM(I,IM)*PM(J,IM)
           ENDDO
            B(I,J) =  - SUM0*WT(J)
            B(J,I) =  - SUM0*WT(I)
          ENDDO
          B(I,I) = B(I,I) + 1.0d0
        ENDDO
      ENDIF
      RETURN
      END SUBROUTINE GEN    

      SUBROUTINE LEGND0 (X,PL,N)
!---Calculates ORDINARY LEGENDRE fns of X (real)
!---   from P[0] = PL(1) = 1,  P[1] = X, .... P[N-1] = PL(N)
      IMPLICIT NONE
      INTEGER N,I
      REAL*8 X,PL(N),DEN
!---Always does PL(2) = P[1]
        PL(1) = 1.D0
        PL(2) = X
        DO I=3,N
         DEN = (I-1)
         PL(I) = PL(I-1)*X*(2.d0-1.D0/DEN) - PL(I-2)*(1.d0-1.D0/DEN)
        ENDDO
      RETURN
      END SUBROUTINE LEGND0         

      SUBROUTINE MATIN4 (A)
!-----------------------------------------------------------------------
!  invert 4x4 matrix A(4,4) in place with L-U decomposition (mjp, old...)
!-----------------------------------------------------------------------
      IMPLICIT NONE
      REAL*8 A(4,4)
!---SETUP L AND U
      A(2,1) = A(2,1)/A(1,1)
      A(2,2) = A(2,2)-A(2,1)*A(1,2)
      A(2,3) = A(2,3)-A(2,1)*A(1,3)
      A(2,4) = A(2,4)-A(2,1)*A(1,4)
      A(3,1) = A(3,1)/A(1,1)
      A(3,2) = (A(3,2)-A(3,1)*A(1,2))/A(2,2)
      A(3,3) = A(3,3)-A(3,1)*A(1,3)-A(3,2)*A(2,3)
      A(3,4) = A(3,4)-A(3,1)*A(1,4)-A(3,2)*A(2,4)
      A(4,1) = A(4,1)/A(1,1)
      A(4,2) = (A(4,2)-A(4,1)*A(1,2))/A(2,2)
      A(4,3) = (A(4,3)-A(4,1)*A(1,3)-A(4,2)*A(2,3))/A(3,3)
      A(4,4) = A(4,4)-A(4,1)*A(1,4)-A(4,2)*A(2,4)-A(4,3)*A(3,4)
!---INVERT L
      A(4,3) = -A(4,3)
      A(4,2) = -A(4,2)-A(4,3)*A(3,2)
      A(4,1) = -A(4,1)-A(4,2)*A(2,1)-A(4,3)*A(3,1)
      A(3,2) = -A(3,2)
      A(3,1) = -A(3,1)-A(3,2)*A(2,1)
      A(2,1) = -A(2,1)
!---INVERT U
      A(4,4) = 1.D0/A(4,4)
      A(3,4) = -A(3,4)*A(4,4)/A(3,3)
      A(3,3) = 1.D0/A(3,3)
      A(2,4) = -(A(2,3)*A(3,4)+A(2,4)*A(4,4))/A(2,2)
      A(2,3) = -A(2,3)*A(3,3)/A(2,2)
      A(2,2) = 1.D0/A(2,2)
      A(1,4) = -(A(1,2)*A(2,4)+A(1,3)*A(3,4)+A(1,4)*A(4,4))/A(1,1)
      A(1,3) = -(A(1,2)*A(2,3)+A(1,3)*A(3,3))/A(1,1)
      A(1,2) = -A(1,2)*A(2,2)/A(1,1)
      A(1,1) = 1.D0/A(1,1)
!---MULTIPLY (U-INVERSE)*(L-INVERSE)
      A(1,1) = A(1,1)+A(1,2)*A(2,1)+A(1,3)*A(3,1)+A(1,4)*A(4,1)
      A(1,2) = A(1,2)+A(1,3)*A(3,2)+A(1,4)*A(4,2)
      A(1,3) = A(1,3)+A(1,4)*A(4,3)
      A(2,1) = A(2,2)*A(2,1)+A(2,3)*A(3,1)+A(2,4)*A(4,1)
      A(2,2) = A(2,2)+A(2,3)*A(3,2)+A(2,4)*A(4,2)
      A(2,3) = A(2,3)+A(2,4)*A(4,3)
      A(3,1) = A(3,3)*A(3,1)+A(3,4)*A(4,1)
      A(3,2) = A(3,3)*A(3,2)+A(3,4)*A(4,2)
      A(3,3) = A(3,3)+A(3,4)*A(4,3)
      A(4,1) = A(4,4)*A(4,1)
      A(4,2) = A(4,4)*A(4,2)
      A(4,3) = A(4,4)*A(4,3)
      RETURN
      END SUBROUTINE MATIN4    

      SUBROUTINE GAUSSP (N,XPT,XWT)
!-----------------------------------------------------------------------
!  Loads in pre-set Gauss points for 4 angles from 0 to +1 in cos(theta)=mu
!-----------------------------------------------------------------------
      IMPLICIT NONE
      INTEGER N,I
      REAL*8  XPT(N),XWT(N)
      REAL*8 GPT4(4),GWT4(4)
      DATA GPT4/.06943184420297D0,.33000947820757D0,.66999052179243D0,   &
                .93056815579703D0/
      DATA GWT4/.17392742256873D0,.32607257743127D0,.32607257743127D0,   &
                .17392742256873D0/
      N = 4
      DO I=1,N
        XPT(I) = GPT4(I)
        XWT(I) = GWT4(I)
      ENDDO
      RETURN
      END SUBROUTINE GAUSSP            
!
      subroutine aeroden(zz,v,aerd)

! purpose:   find number density of boundary layer aerosols, aerd,
!            at a given altitude, zz, and for a specified visibility
! input:
!   zz       altitude   (km)
!   v        visibility for a horizontal surface path (km)
! output:
!   aerd     aerosol density at altitude z

! the vertical distribution of the boundary layer aerosol density is
! based on the 5s vertical profile models for 5 and 23 km visibility.
! above 5 km, the aden05 and aden23 models are the same
! below 5 km, the models differ as follows;
! aden05     0.99 km scale height (94% of extinction occurs below 5 km)
! aden23     1.45 km scale heigth (80% of extinction occurs below 5 km)
!

	implicit none
	integer mz, nz
        parameter (mz=33)
        real v,aerd
        real*8 zz ! compatability with fastj
        real alt, aden05, aden23, aer05,aer23      
      dimension alt(mz),aden05(mz),aden23(mz)
!jdf  dimension zbaer(*),dbaer(*)
	
	real z, f, wth
	integer k, kp
      save alt,aden05,aden23,nz
      

      data nz/mz/

      data alt/   &
              0.0,        1.0,        2.0,        3.0,        4.0,   &
              5.0,        6.0,        7.0,        8.0,        9.0,   &
             10.0,       11.0,       12.0,       13.0,       14.0,   &
             15.0,       16.0,       17.0,       18.0,       19.0,   &
             20.0,       21.0,       22.0,       23.0,       24.0,   &
             25.0,       30.0,       35.0,       40.0,       45.0,   &
             50.0,       70.0,      100.0/
      data aden05/   &
        1.378E+04,  5.030E+03,  1.844E+03,  6.731E+02,  2.453E+02,   &
        8.987E+01,  6.337E+01,  5.890E+01,  6.069E+01,  5.818E+01,   &
        5.675E+01,  5.317E+01,  5.585E+01,  5.156E+01,  5.048E+01,   &
        4.744E+01,  4.511E+01,  4.458E+01,  4.314E+01,  3.634E+01,   &
        2.667E+01,  1.933E+01,  1.455E+01,  1.113E+01,  8.826E+00,   &
        7.429E+00,  2.238E+00,  5.890E-01,  1.550E-01,  4.082E-02,   &
        1.078E-02,  5.550E-05,  1.969E-08/
      data aden23/   &
        2.828E+03,  1.244E+03,  5.371E+02,  2.256E+02,  1.192E+02,   &
        8.987E+01,  6.337E+01,  5.890E+01,  6.069E+01,  5.818E+01,   &
        5.675E+01,  5.317E+01,  5.585E+01,  5.156E+01,  5.048E+01,   &
        4.744E+01,  4.511E+01,  4.458E+01,  4.314E+01,  3.634E+01,   &
        2.667E+01,  1.933E+01,  1.455E+01,  1.113E+01,  8.826E+00,   &
        7.429E+00,  2.238E+00,  5.890E-01,  1.550E-01,  4.082E-02,   &
        1.078E-02,  5.550E-05,  1.969E-08/
!
      z=max(0.,min(100.,real(zz)))
      aerd=0.
      if(z.gt.alt(nz)) return

      call locate(alt,nz,z,k)
      kp=k+1
      f=(z-alt(k))/(alt(kp)-alt(k))

      if(min(aden05(k),aden05(kp),aden23(k),aden23(kp)).le.0.) then
        aer05=aden05(k)*(1.-f)+aden05(kp)*f
        aer23=aden23(k)*(1.-f)+aden23(kp)*f
      else
        aer05=aden05(k)*(aden05(kp)/aden05(k))**f
        aer23=aden23(k)*(aden23(kp)/aden23(k))**f
      endif

      wth=(1./v-1/5.)/(1./23.-1./5.)
      wth=max(0.,min(1.,wth))

      aerd=(1.-wth)*aer05+wth*aer23

!      write(*,*) 'aeroden k,kp,z,aer05(k),aer05(kp),f,aerd'
!      write(*,'(2i5,1p5e11.3)') k,kp,z,aden05(k),aden05(kp),f,aerd

      return
	end subroutine aeroden           
!=======================================================================
      subroutine locate(xx,n,x,j)
!
! purpose:  given an array xx of length n, and given a value X, returns
!           a value J such that X is between xx(j) and xx(j+1). xx must
!           be monotonic, either increasing of decreasing. this function
!           returns j=1 or j=n-1 if x is out of range.
!c
! input:
!   xx      monitonic table
!   n       size of xx
!   x       single floating point value perhaps within the range of xx
!
! output:
!           function returns index value j, such that
!
!            for an increasing table
!
!                xx(j) .lt. x .le. xx(j+1),
!                j=1    for x .lt. xx(1)
!                j=n-1  for x .gt. xx(n)
!
!            for a decreasing table
!                xx(j) .le. x .lt. xx(j+1)
!                j=n-1  for x .lt. xx(n)
!                j=1    for x .gt. xx(1)
!
      implicit none
      integer j,n
      real x,xx(n)
      integer jl,jm,ju

! . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

      if(x.eq.xx(1)) then
        j=1
        return
      endif
      if(x.eq.xx(n)) then
        j=n-1
        return
      endif
      jl=1
      ju=n
10    if(ju-jl.gt.1) then
        jm=(ju+jl)/2
         if((xx(n).gt.xx(1)).eqv.(x.gt.xx(jm)))then
          jl=jm
        else
          ju=jm
        endif
      goto 10
      endif
      j=jl
      return
      end subroutine locate          
!************************************************************************
      subroutine rd_tjpl2
!-----------------------------------------------------------------------
!  set wavelength bins, solar fluxes, Rayleigh parameters, temperature-
!  dependent cross sections and Rayleigh/aerosol scattering phase functions
!  with temperature dependences. Current data originates from JPL 2000
!-----------------------------------------------------------------------
!
!     NJVAL    Number of species to calculate J-values for
!     NWWW     Number of wavelength bins, from NW1:NW2
!     WBIN     Boundaries of wavelength bins
!     WL       Centres of wavelength bins - 'effective wavelength'
!     FL       Solar flux incident on top of atmosphere (cm-2.s-1)
!     QRAYL    Rayleigh parameters (effective cross-section) (cm2)
!     QBC      Black Carbon absorption extinct. (specific cross-sect.) (m2/g)
!     QO2      O2 cross-sections
!     QO3      O3 cross-sections
!     Q1D      O3 => O(1D) quantum yield
!     TQQ      Temperature for supplied cross sections
!     QQQ      Supplied cross sections in each wavelength bin (cm2)
!     NAA      Number of categories for scattering phase functions
!     QAA      Aerosol scattering phase functions
!     NK       Number of wavelengths at which functions supplied (set as 4)
!     WAA      Wavelengths for the NK supplied phase functions
!     PAA      Phase function: first 8 terms of expansion
!     RAA      Effective radius associated with aerosol type
!     SSA      Single scattering albedo
!
!     npdep    Number of pressure dependencies
!     zpdep    Pressure dependencies by wavelength bin
!     jpdep    Index of cross sections requiring pressure dependence
!     lpdep    Label for pressure dependence
!
!-----------------------------------------------------------------------

        USE module_data_mosaic_other, only : kmaxd
	USE module_fastj_data
	USE module_peg_util, only:  peg_message, peg_error_fatal
	
	IMPLICIT NONE
!jdf
! Print Fast-J diagnostics if iprint /= 0
        integer, parameter :: iprint = 0
        integer, parameter :: single = 4        !compiler dependent value real*4
!       integer, parameter :: double = 8        !compiler dependent value real*8
        integer,parameter :: ipar_fastj=1,jpar=1
!       integer,parameter :: jppj=14        !Number of photolytic reactions supplied
        logical,parameter :: ldeg45=.false. !Logical flag for degraded CTM resolution
        integer lpar           !Number of levels in CTM
        integer jpnl           !Number of levels requiring chemistry
        real(kind=double), dimension(ipar_fastj) :: xgrd !  Longitude (midpoint, radians)
        real(kind=double), dimension(jpar) :: ygrd           !  Latitude  (midpoint, radians)
        real(kind=double), dimension(jpar) :: ydgrd          !  Latitude  (midpoint, degrees)
        real(kind=double), dimension(kmaxd+1) :: etaa    !  Eta(a) value for level boundaries
        real(kind=double), dimension(kmaxd+1) :: etab    !  Eta(b) value for level boundaries
        real(kind=double) ::  tau_fastj          !  Time of Day (hours, GMT)
        integer month_fastj        !  Number of month (1-12)
        integer iday_fastj         !  Day of year
!jdf
         integer i, j, k
	 character*7  lpdep(3)
	 character*80 msg

         if(NJVAL.gt.NS) then
! fastj input files are not set up for current situation   
            write ( msg, '(a, 2i6)' )  &
              'FASTJ  # xsect supplied > max allowed  '  //  &
              'NJVAL NS ', NJVAL, NS
            call peg_message( lunerr, msg )
            msg = 		&
             'FASTJ Setup Error: # xsect supplied > max allowed. Increase NS'
            call peg_error_fatal( lunerr, msg )              
!           write(6,300) NJVAL,NS
!           stop
         endif

         if(NAA.gt.NP) then
            write ( msg, '(a, 2i6)' )  &
              'FASTJ  # aerosol/cloud types > NP  '  //  &
              'NAA NP ', NAA ,NP
            call peg_message( lunerr, msg )
            msg = 		&
              'FASTJ Setup Error: Too many phase functions supplied. Increase NP'
            call peg_error_fatal( lunerr, msg )                       
!            write(6,350) NAA
!            stop
         endif
!---Zero index arrays
      do j=1,jppj
        jind(j)=0
      enddo
      do j=1,NJVAL
        jpdep(j)=0
      enddo
      do j=1,nh
        hzind(j)=0
      enddo
!
!---Set mapping index
      do j=1,NJVAL
        do k=1,jppj
          if(jlabel(k).eq.titlej(1,j)) jind(k)=j
!	write(6,*)k,jind(k)  ! jcb
!	write(6,*)jlabel(k),titlej(1,j)  ! jcb
        enddo
        do k=1,npdep
          if(lpdep(k).eq.titlej(1,j)) jpdep(j)=k
        enddo
      enddo
      do k=1,jppj
        if(jfacta(k).eq.0.d0) then 
!            write(6,*) 'Not using photolysis reaction ',k
           write ( msg, '(a, i6)' )  &
             'FASTJ  Not using photolysis reaction ' , k 
           call peg_message( lunerr, msg ) 
        end if           
        if(jind(k).eq.0) then
          if(jfacta(k).eq.0.d0) then
            jind(k)=1
          else
              write ( msg, '(a, i6)' )  &
               'FASTJ  Which J-rate for photolysis reaction ' , k 
              call peg_message( lunerr, msg ) 
!              write(6,*) 'Which J-rate for photolysis reaction ',k,' ?'
!              stop
              msg = 'FASTJ subr rd_tjpl2 Unknown Jrate. Incorrect FASTJ setup'
              call peg_error_fatal( lunerr, msg )      
          endif
        endif
      enddo
! Herzberg index
      i=0
      do j=1,nhz
        do k=1,jppj
          if(jlabel(k).eq.hzlab(j)) then
            i=i+1
            hzind(i)=k
            hztoa(i)=hztmp(j)*jfacta(k)
          endif
        enddo
      enddo
      nhz=i
      if(nhz.eq.0) then
        if(iprint.ne.0) then
           write ( msg, '(a)' )  &
            'FASTJ  Not using Herzberg bin '    
           call peg_message( lunerr, msg )        
!           write(6,400)
         end if
      else
        if(iprint.ne.0) then
           write ( msg, '(a)' )  & 
              'FASTJ Using Herzberg bin for: ' 
           call peg_message( lunerr, msg )
           write( msg, '(a,10a7)' )	&
              'FASTJ ', (jlabel(hzind(i)),i=1,nhz)     
!          write(6,420) (jlabel(hzind(i)),i=1,nhz)
        end if 
      endif

! 300   format(' Number of x-sections supplied to Fast-J: ',i3,/,   &
!             ' Maximum number allowed (NS) only set to: ',i3,   &
!             ' - increase in jv_cmn.h',/,   &
!             'RESULTS WILL BE IN ERROR' )
! 350   format(' Too many phase functions supplied; increase NP to ',i2,   &
!             /,'RESULTS WILL BE IN ERROR'  )
!  400 format(' Not using Herzberg bin')
!  420 format(' Using Herzberg bin for: ',10a7)


         return
         end subroutine rd_tjpl2
!********************************************************************


end module module_phot_fastj
