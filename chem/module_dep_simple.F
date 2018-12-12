    MODULE module_dep_simple

      IMPLICIT NONE

!--------------------------------------------------
! many of these parameters will depend on the RADM mechanism!
! if you change it, lets talk about it and get it done!!!
!--------------------------------------------------

      INTEGER, PARAMETER :: dep_seasons = 5
      INTEGER, PARAMETER :: nlu = 25
      REAL, parameter    :: small_value = 1.e-36
      REAL, parameter    :: large_value = 1.e36

!--------------------------------------------------
! following currently hardwired to USGS
!--------------------------------------------------
      integer, parameter :: isice_temp   = 24
      integer, parameter :: iswater_temp = 16
      integer, parameter :: wrf2mz_lt_map(nlu) = (/ 1, 2, 2, 2, 2, &
                                                    4, 3, 3, 3, 3, &
                                                    4, 5, 4, 5, 6, &
                                                    7, 9, 6, 8, 9, &
                                                    6, 6, 8, 0, 0 /)
      real, parameter    :: wh2o = 18.0153
      real, parameter    :: wpan = 121.04793
      character(len=4), parameter :: mminlu = 'USGS'

      INTEGER :: month = 0
      INTEGER :: ixxxlu(nlu)
!     include modis landuse 
      INTEGER, allocatable :: luse2usgs(:)
      INTEGER, allocatable :: HL_ndx(:)
!--

      REAL    :: kpart(nlu)
      REAL    :: rac(nlu,dep_seasons), rclo(nlu,dep_seasons), rcls(nlu,dep_seasons)
      REAL    :: rgso(nlu,dep_seasons), rgss(nlu,dep_seasons)
      REAL    :: ri(nlu,dep_seasons), rlu(nlu,dep_seasons)
      REAL    :: ri_pan(5,11)
      real    :: c0_pan(11) = (/ 0.000, 0.006, 0.002, 0.009, 0.015, &
                                 0.006, 0.000, 0.000, 0.000, 0.002, 0.002 /)
      real    :: k_pan (11) = (/ 0.000, 0.010, 0.005, 0.004, 0.003, &
                                 0.005, 0.000, 0.000, 0.000, 0.075, 0.002 /)

!--------------------------------------------------
! NO MORE THAN 1000 SPECIES FOR DEPOSITION
!--------------------------------------------------
      REAL    :: dratio(1000), hstar(1000), hstar4(1000)
      REAL    :: f0(1000), dhr(1000), scpr23(1000)

      type wesely_pft
        integer          :: npft
        integer          :: months
        INTEGER, pointer :: seasonal_wes(:,:,:,:)
        logical          :: is_allocated
      end type wesely_pft

      type(wesely_pft), allocatable :: seasonal_pft(:)

!--------------------------------------------------
! .. Default Accessibility ..
!--------------------------------------------------
    PUBLIC
    PRIVATE :: HL_init

    logical, allocatable :: is_aerosol(:) ! true if field is aerosol (any phase)

    CONTAINS

SUBROUTINE wesely_driver( id, ktau, dtstep, config_flags, current_month,  &
                          gmt, julday, t_phy,moist, p8w, t8w, raincv,     &
                          p_phy, chem, rho_phy, dz8w, ddvel, aer_res_def, &
                          aer_res_zcen, ivgtyp, tsk, gsw, vegfra, pbl,    &
                          rmol, ust, znt, xlat, xlong,                    &
                          z, z_at_w, snowh, numgas,                       &
                          ids,ide, jds,jde, kds,kde,                      &
                          ims,ime, jms,jme, kms,kme,                      &
                          its,ite, jts,jte, kts,kte                       )
!--------------------------------------------------
!  Wesely dry dposition driver
!--------------------------------------------------

  USE module_model_constants 
  USE module_configure
  USE module_state_description                       
  USE module_data_sorgam
  USE module_state_description, only:  param_first_scalar        
   INTEGER,      INTENT(IN   ) :: id,julday,                              &
                                  numgas, current_month,                  &
                                  ids,ide, jds,jde, kds,kde,              &
                                  ims,ime, jms,jme, kms,kme,              &
                                  its,ite, jts,jte, kts,kte     
   INTEGER,      INTENT(IN   ) :: ktau            
      REAL,      INTENT(IN   ) :: dtstep,gmt

!--------------------------------------------------
! advected moisture variables
!--------------------------------------------------
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ), INTENT(IN ) :: &
                                                      moist  
!--------------------------------------------------
! advected chemical species
!--------------------------------------------------
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ), INTENT(IN )    :: &
                                                      chem
!--------------------------------------------------
! deposition velocities
!--------------------------------------------------
   REAL, DIMENSION( its:ite, jts:jte, num_chem ), INTENT(INOUT ) ::      &
                                                      ddvel                     
!--------------------------------------------------
! input from met model
!--------------------------------------------------
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) ::      &
                                                      t_phy,              &
                                                      p_phy,              &
                                                      dz8w,               &
                                                      z,                  &
                                                      t8w,                &
                                                      p8w,                &
                                                      z_at_w,             &
                                                      rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme ), INTENT(IN   ) ::               &
                                                     ivgtyp
   REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(INOUT   ) ::             &
                                                     rmol
   REAL,  DIMENSION( ims:ime , jms:jme ), INTENT(IN )      ::             &
                                                     tsk,                 &
                                                     gsw,                 &
                                                     vegfra,              &
                                                     pbl,                 &
                                                     ust,                 &
                                                     xlat,                &
                                                     xlong,               &
                                                     raincv,              &
                                                     znt
   REAL, intent(inout) ::                            aer_res_def(its:ite,jts:jte)
   REAL, intent(inout) ::                            aer_res_zcen(its:ite,jts:jte)
   REAL,  optional, INTENT(IN)  ::                   snowh(ims:ime,jms:jme)
   TYPE(grid_config_rec_type),  INTENT(IN) ::        config_flags

!--------------------------------------------------
! .. Local Scalars
!--------------------------------------------------
      REAL    ::  clwchem, dvfog, dvpart, pa, rad, dep_vap
      REAL    ::  rhchem, ta, ustar, vegfrac, z1, zntt
      INTEGER :: i, iland, iprt, iseason, j, jce, jcs, n, nr, ipr,jpr,nvr
      LOGICAL :: highnh3, rainflag, vegflag, wetflag
      LOGICAL :: chm_is_moz
!--------------------------------------------------
! .. Local Arrays
!--------------------------------------------------
      REAL :: p(kts:kte)
      REAL :: srfres(numgas)
      REAL :: ddvel0d(numgas)

!-----------------------------------------------------------
! necessary for aerosols (module dependent)         
!-----------------------------------------------------------
      real :: rcx(numgas)

!-----------------------------------------------------------
! .. Intrinsic Functions
!-----------------------------------------------------------
      INTRINSIC max, min                             

      chm_is_moz = config_flags%chem_opt == MOZART_KPP .or. &
                   config_flags%chem_opt == MOZCART_KPP .or. &
                   config_flags%chem_opt == T1_MOZCART_KPP .or. &
                   config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
                   config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP

      dep_vap= config_flags%depo_fact

      CALL wrf_debug(15,'in dry_dep_wesely')

      if( .not. chm_is_moz ) then
         if( julday < 90 .or. julday > 270 ) then
            iseason = 2
            CALL wrf_debug(15,'setting iseason to 2')
         else
            iseason = 1
         endif
      end if


tile_lat_loop : &
      do j = jts,jte 
tile_lon_loop : &
         do i = its,ite 
            iprt  = 0

            iland = luse2usgs( ivgtyp(i,j) )
!--

            if( chm_is_moz ) then
               if( snowh(i,j) < .01 ) then 
                  iseason = seasonal_pft(id)%seasonal_wes(i,j,iland,current_month)
               else
                  iseason = 4
               endif
            end if
            ta    = tsk(i,j)
            rad   = gsw(i,j)
            vegfrac = vegfra(i,j)
            pa      = .01*p_phy(i,kts,j)
            clwchem = moist(i,kts,j,p_qc)
            ustar = ust(i,j)
            zntt  = znt(i,j)
            z1    = z_at_w(i,kts+1,j) - z_at_w(i,kts,j)
!-----------------------------------------------------------
!     Set logical default values
!-----------------------------------------------------------
            rainflag = .FALSE.
            wetflag  = .FALSE.
            highnh3  = .FALSE.
            if(p_qr >= param_first_scalar) then
               if(moist(i,kts,j,p_qr) > 1.e-18 .or. raincv(i,j) > 0.) then
                  rainflag = .true.
               endif
            endif
            rhchem = MIN( 100.,100. * moist(i,kts,j,p_qv) / &
                     (3.80*exp(17.27*(t_phy(i,kts,j)-273.)/(t_phy(i,kts,j)-36.))/pa))
            rhchem = MAX(5.,RHCHEM)
            if (rhchem >= 95.) wetflag = .true.

            if( p_nh3 >= param_first_scalar .and. p_so2 >= param_first_scalar ) then
               if( chem(i,kts,j,p_nh3) > 2.*chem(i,kts,j,p_so2) ) then
                  highnh3 = .true.
               endif
            endif

!-----------------------------------------------------------
!--- deposition
!-----------------------------------------------------------
!     if(snowc(i,j).gt.0.)iseason=4
            CALL rc( rcx, ta, rad, rhchem, iland, &
                     iseason, numgas, wetflag, rainflag, highnh3, &
                     iprt, moist(i,kts,j,p_qv), p8w(i,kts,j), &
                     config_flags%chem_opt, chm_is_moz )
            if( .not. chm_is_moz ) then
               srfres(1:numgas-2) = rcx(1:numgas-2)
               srfres(numgas-1:numgas) = 0.
            else
               srfres(1:numgas) = rcx(1:numgas)
            end if
            CALL deppart( rmol(i,j), ustar, rhchem, clwchem, iland, dvpart, dvfog )
            ddvel0d(1:numgas) = 0.
            aer_res_def(i,j)  = 0.
            aer_res_zcen(i,j) = 0.
            CALL landusevg( ddvel0d, ustar, rmol(i,j), zntt, z1, dvpart, iland,        &
                            numgas, srfres, aer_res_def(i,j), aer_res_zcen(i,j), p_sulf )

!-----------------------------------------------------------
!wig: CBMZ does not have HO and HO2 last so need to copy all species
!      ddvel(i,j,1:numgas-2)=ddvel0d(1:numgas-2)
!-----------------------------------------------------------
            ddvel(i,j,1:numgas) = ddvel0d(1:numgas)
            if ( (config_flags%chem_opt == RADM2           ) .or.   &
                 (config_flags%chem_opt == RADM2SORG       ) .or.   &
                 (config_flags%chem_opt == RADM2SORG_AQ    ) .or.   &
                 (config_flags%chem_opt == RADM2SORG_AQCHEM) ) then
                     ddvel(i,j,p_hcl)         = ddvel(i,j,p_hno3)
            end if
         end do tile_lon_loop
      end do tile_lat_loop
     
!-----------------------------------------------------------
! For the additional CBMZ species, assign similar RADM counter parts for
! now. Short lived species get a zero velocity since dry dep should be
! unimportant.  **ALSO**, treat p_sulf as h2so4 vapor, not aerosol sulfate
!-----------------------------------------------------------
      if ( (config_flags%chem_opt == CBMZ          ) .or.          &
           (config_flags%chem_opt == CBMZ_BB       ) .or.          &
           (config_flags%chem_opt == CBMZ_BB_KPP   ) .or.          &
           (config_flags%chem_opt == CBMZ_MOSAIC_KPP   ) .or.      &
           (config_flags%chem_opt == CBMZ_MOSAIC_4BIN_AQ) .or.     &
           (config_flags%chem_opt == CBMZ_MOSAIC_8BIN_AQ) .or.     &
           (config_flags%chem_opt == CBMZ_MOSAIC_4BIN) .or.        &
           (config_flags%chem_opt == CBMZ_MOSAIC_8BIN) .or.        &
           (config_flags%chem_opt == CBMZ_MOSAIC_DMS_4BIN_AQ) .or. &
           (config_flags%chem_opt == CBMZ_MOSAIC_DMS_8BIN_AQ) .or. &
           (config_flags%chem_opt == CBMZ_MOSAIC_DMS_4BIN) .or.    &
           (config_flags%chem_opt == CBMZ_MOSAIC_DMS_8BIN) .or.    &
           (config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ ) .or.     &
           (config_flags%chem_opt == CBMZ_CAM_MAM3_AQ   ) .or.     &
           (config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ ) .or.     &
           (config_flags%chem_opt == CBMZ_CAM_MAM7_AQ   ) ) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_sulf)        = ddvel(i,j,p_hno3)
               ddvel(i,j,p_hcl)         = ddvel(i,j,p_hno3)
               ddvel(i,j,p_ch3o2)       = 0
               ddvel(i,j,p_ethp)        = 0
               ddvel(i,j,p_ch3oh)       = ddvel(i,j,p_hcho)
               ddvel(i,j,p_c2h5oh)      = ddvel(i,j,p_hcho)
!wig, 1-May-2007 (added par to tables)    ddvel(i,j,p_par)         = ddvel(i,j,p_hc5)
               ddvel(i,j,p_to2)         = 0
               ddvel(i,j,p_cro)         = 0
               ddvel(i,j,p_open)        = ddvel(i,j,p_xyl)
               ddvel(i,j,p_op3)         = ddvel(i,j,p_op2)
               ddvel(i,j,p_c2o3)        = 0
               ddvel(i,j,p_ro2)         = 0
               ddvel(i,j,p_ano2)        = 0
               ddvel(i,j,p_nap)         = 0
               ddvel(i,j,p_xo2)         = 0
               ddvel(i,j,p_xpar)        = 0
               ddvel(i,j,p_isoprd)      = 0
               ddvel(i,j,p_isopp)       = 0
               ddvel(i,j,p_isopn)       = 0
               ddvel(i,j,p_isopo2)      = 0
               if((config_flags%chem_opt == CBMZ ) .or.                   &
                  (config_flags%chem_opt == CBMZ_MOSAIC_DMS_4BIN) .or.    &
                  (config_flags%chem_opt == CBMZ_MOSAIC_DMS_8BIN) .or.    &
                  (config_flags%chem_opt == CBMZ_MOSAIC_DMS_4BIN_AQ) .or. &
                  (config_flags%chem_opt == CBMZ_MOSAIC_DMS_8BIN_AQ) ) then
                  ddvel(i,j,p_dms)         = 0
                  ddvel(i,j,p_msa)         = ddvel(i,j,p_hno3)
                  ddvel(i,j,p_dmso)        = 0
                  ddvel(i,j,p_dmso2)       = 0
                  ddvel(i,j,p_ch3so2h)     = 0
                  ddvel(i,j,p_ch3sch2oo)   = 0
                  ddvel(i,j,p_ch3so2)      = 0
                  ddvel(i,j,p_ch3so3)      = 0
                  ddvel(i,j,p_ch3so2oo)    = 0
                  ddvel(i,j,p_ch3so2ch2oo) = 0
                  ddvel(i,j,p_mtf)         = 0
               end if
               if( ( config_flags%chem_opt == CBMZ_CAM_MAM3_NOAQ ) .or.   &
                   ( config_flags%chem_opt == CBMZ_CAM_MAM3_AQ   ) .or.   &
                   ( config_flags%chem_opt == CBMZ_CAM_MAM7_NOAQ ) .or.   &
                   ( config_flags%chem_opt == CBMZ_CAM_MAM7_AQ   ) ) then
                  ddvel(i,j,p_soag)        = 0.0
               end if
            end do
         end do
      end if
!!!!TUCCELLA
     if  (config_flags%chem_opt == RACM_SOA_VBS_KPP .OR.           &
          config_flags%chem_opt == RACM_SOA_VBS_HET_KPP .OR.           &
          config_flags%chem_opt == RACM_SOA_VBS_AQCHEM_KPP) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_cvasoa1) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa2) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa3) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa4) = dep_vap*ddvel(i,j,p_hno3)

               ddvel(i,j,p_cvbsoa1) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa2) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa3) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa4) = dep_vap*ddvel(i,j,p_hno3)

               ddvel(i,j,p_sesq)    = 0.
               ddvel(i,j,p_mbo)     = 0.
            end do
         end do
      end if

     if  (config_flags%chem_opt == RACM_SOA_VBS_HET_KPP) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_hcl) = dep_vap*ddvel(i,j,p_hno3)
               ddvel(i,j,p_clno2) = 0.0
               ddvel(i,j,p_cl2) = 0.0
               ddvel(i,j,p_fmcl) = 0.0
               ddvel(i,j,p_cl) = 0.0
               ddvel(i,j,p_clo) = 0.0
               ddvel(i,j,p_hocl) = 0.0
               ddvel(i,j,p_ch3cl) = 0.0
            end do
         end do
      end if

!-----------------------------------------------------------
! For the additional CBM4 species, assign similar RADM counter parts for
! now. Short lived species get a zero velocity since dry dep should be
! unimportant.
!-----------------------------------------------------------
      if  (config_flags%chem_opt == CBM4_KPP          )   then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_open)        = ddvel(i,j,p_xyl)
               ddvel(i,j,p_ro2)         = 0
               ddvel(i,j,p_xo2)         = 0
               ddvel(i,j,p_ald2)        = ddvel(i,j,p_ald)
               ddvel(i,j,p_iso)        = 0
            end do
         end do
      end if


      if  (config_flags%chem_opt == CBMZ_MOSAIC_KPP  )   then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_aro1)        = 0
               ddvel(i,j,p_aro2)         = 0
               ddvel(i,j,p_alk1)         = 0
               ddvel(i,j,p_ole1)        = 0
               ddvel(i,j,p_api1)        = 0
               ddvel(i,j,p_api2)        = 0
               ddvel(i,j,p_lim1)        = 0
               ddvel(i,j,p_lim2)        = 0
               ddvel(i,j,p_api)        = 0
               ddvel(i,j,p_lim)        = 0

            end do
         end do
      end if



! For gocartracm,radm
!-----------------------------------------------------------
      if  ((config_flags%chem_opt == GOCARTRACM_KPP)  .OR.     &
           (config_flags%chem_opt == GOCARTRADM2))   then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_sulf)        = 0.
               ddvel(i,j,p_dms)         = 0.
               ddvel(i,j,p_msa)         = ddvel(i,j,p_hno3)
               if( config_flags%chem_opt == GOCARTRADM2 ) then
               ddvel(i,j,p_hcl)         = ddvel(i,j,p_hno3)
               end if
            end do
         end do
      end if
!-----------------------------------------------------------
! For gocartsimple : need msa. On the other hand sulf comes from aerosol routine
!-----------------------------------------------------------
      if  (config_flags%chem_opt == GOCART_SIMPLE          )   then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_msa)         = ddvel(i,j,p_sulf)
               ddvel(i,j,p_sulf)        = 0.
               ddvel(i,j,p_dms)         = 0.
            end do
         end do
      end if
!-----------------------------------------------------------
! For mozart
!-----------------------------------------------------------
      if( chm_is_moz ) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_mpan)    = ddvel(i,j,p_mpan)/3.
               ddvel(i,j,p_mvkooh)  = ddvel(i,j,p_hno3)
               ddvel(i,j,p_isooh)   = ddvel(i,j,p_hno3)
               ddvel(i,j,p_xooh)    = ddvel(i,j,p_hno3)
               ddvel(i,j,p_o)       = 0.
               ddvel(i,j,p_o1d_cb4) = 0.
               ddvel(i,j,p_no3)     = 0.
               ddvel(i,j,p_n2o5)    = 0.
               ddvel(i,j,p_ho)      = 0.
               ddvel(i,j,p_ho2)     = 0.
               ddvel(i,j,p_ch3o2)   = 0.
               ddvel(i,j,p_n2o)     = 0.
               ddvel(i,j,p_ch4)     = 0.
               ddvel(i,j,p_h2)      = 0.
               ddvel(i,j,p_co2)     = 0.
               ddvel(i,j,p_c2h4)    = 0.
               ddvel(i,j,p_c2h6)    = 0.
               ddvel(i,j,p_c3h6)    = 0.
               ddvel(i,j,p_c3h8)    = 0.
               ddvel(i,j,p_aco3)    = 0.
               ddvel(i,j,p_gly)     = 0.2 / 100. ! from Washenfelder et al., 2011
               ddvel(i,j,p_mgly)    = 0. !
               ddvel(i,j,p_macr)    = 0.
               ddvel(i,j,p_mek)     = 0.
               ddvel(i,j,p_eto2)    = 0.
               ddvel(i,j,p_open)    = 0.
               ddvel(i,j,p_eo)      = 0.
               ddvel(i,j,p_pro2)    = 0.
               ddvel(i,j,p_po2)     = 0.
               ddvel(i,j,p_aceto2)  = 0.
               ddvel(i,j,p_bigene)  = 0.
               ddvel(i,j,p_bigalk)  = 0.
               ddvel(i,j,p_eneo2)   = 0.
               ddvel(i,j,p_alko2)   = 0.
               ddvel(i,j,p_isopr)   = 0.
               ddvel(i,j,p_iso2)    = 0.
               ddvel(i,j,p_mvko2)   = 0.
               ddvel(i,j,p_xo2)     = 0.
               ddvel(i,j,p_c10h16)  = 0.
               ddvel(i,j,p_terpo2)  = 0.
               ddvel(i,j,p_tol)     = 0.
               ddvel(i,j,p_to2)     = 0.
               ddvel(i,j,p_xoh)     = 0.
               ddvel(i,j,p_isopn)   = 0.
               ddvel(i,j,p_meko2)   = 0.
               ddvel(i,j,p_dms)     = 0.
               IF ( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .OR. &
                    config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) THEN
                 ddvel(i,j,p_benzene)  = 0.
                 ddvel(i,j,p_phen)     = 0.
                 ddvel(i,j,p_bepomuc)  = 0.
                 ddvel(i,j,p_benzo2)   = 0.
                 ddvel(i,j,p_pheno2)   = 0.
                 ddvel(i,j,p_pheno)    = 0.
                 ddvel(i,j,p_phenooh)  = ddvel(i,j,p_h2o2)
                 ddvel(i,j,p_c6h5o2)   = 0.
                 ddvel(i,j,p_c6h5ooh)  = ddvel(i,j,p_h2o2)
                 ddvel(i,j,p_benzooh)  = ddvel(i,j,p_h2o2)
                 ddvel(i,j,p_bigald1)  = 0.
                 ddvel(i,j,p_bigald2)  = 0.
                 ddvel(i,j,p_bigald3)  = 0.
                 ddvel(i,j,p_bigald4)  = 0.
                 ddvel(i,j,p_malo2)    = 0.
                 ddvel(i,j,p_pbznit)   = 0.
                 ddvel(i,j,p_tepomuc)  = 0.
                 ddvel(i,j,p_bzoo)     = 0.
                 ddvel(i,j,p_bzooh)    = ddvel(i,j,p_h2o2)
                 ddvel(i,j,p_bald)     = 0.
                 ddvel(i,j,p_acbzo2)   = 0.
                 ddvel(i,j,p_dicarbo2) = 0.
                 ddvel(i,j,p_mdialo2)  = 0.
                 ddvel(i,j,p_xyl)      = 0.
                 ddvel(i,j,p_xylol)    = 0.
                 ddvel(i,j,p_xylolo2)  = 0.
                 ddvel(i,j,p_xylolooh) = ddvel(i,j,p_h2o2)
                 ddvel(i,j,p_xyleno2)  = 0.
                 ddvel(i,j,p_xylenooh) = ddvel(i,j,p_h2o2)
                 IF ( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP) THEN
                   ddvel(i,j,p_voca)     = 0.
                   ddvel(i,j,p_vocbb)    = 0.
                   ddvel(i,j,p_smpa)     = 0.
                   ddvel(i,j,p_smpbb)    = 0.
                 ENDIF
               ENDIF
            end do
         end do
         IF ( config_flags%chem_opt /= T1_MOZCART_KPP) THEN
           do j=jts,jte
             ddvel(its:ite,j,p_iso2)    = 0.
           end do
         ELSE
           do j=jts,jte
             ddvel(its:ite,j,p_benzene)  = 0.
             ddvel(its:ite,j,p_bepomuc)  = 0.
             ddvel(its:ite,j,p_benzo2)   = 0.
             ddvel(its:ite,j,p_pheno2)   = 0.
             ddvel(its:ite,j,p_pheno)    = 0.
             ddvel(its:ite,j,p_phenol)   = 0.
             ddvel(its:ite,j,p_phenooh)  = ddvel(its:ite,j,p_h2o2)
             ddvel(its:ite,j,p_c6h5o2)   = 0.
             ddvel(its:ite,j,p_c6h5ooh)  = ddvel(its:ite,j,p_h2o2)
             ddvel(its:ite,j,p_benzooh)  = ddvel(its:ite,j,p_h2o2)
             ddvel(its:ite,j,p_bigald1)  = 0.
             ddvel(its:ite,j,p_bigald2)  = 0.
             ddvel(its:ite,j,p_bigald3)  = 0.
             ddvel(its:ite,j,p_bigald4)  = 0.
             ddvel(its:ite,j,p_bzald)    = 0.
             ddvel(its:ite,j,p_malo2)    = 0.
             ddvel(its:ite,j,p_pbznit)   = 0.
             ddvel(its:ite,j,p_tepomuc)  = 0.
             ddvel(its:ite,j,p_bzoo)     = 0.
             ddvel(its:ite,j,p_bzooh)    = ddvel(its:ite,j,p_h2o2)
             ddvel(its:ite,j,p_acbzo2)   = 0.
             ddvel(its:ite,j,p_dicarbo2) = 0.
             ddvel(its:ite,j,p_mdialo2)  = 0.
             ddvel(its:ite,j,p_xylenes)  = 0.
             ddvel(its:ite,j,p_xylol)    = 0.
             ddvel(its:ite,j,p_xylolo2)  = 0.
             ddvel(its:ite,j,p_xylolooh) = ddvel(its:ite,j,p_h2o2)
             ddvel(its:ite,j,p_xyleno2)  = 0.
             ddvel(its:ite,j,p_xylenooh) = ddvel(its:ite,j,p_h2o2)
           end do
         ENDIF
         IF( config_flags%chem_opt == MOZART_KPP .or. config_flags%chem_opt == MOZCART_KPP ) THEN
           do j=jts,jte
             ddvel(its:ite,j,p_c10h16)  = 0.
             ddvel(its:ite,j,p_xoh)     = 0.
           end do
         ENDIF

         IF( config_flags%chem_opt == MOZART_KPP .or. config_flags%chem_opt == MOZCART_KPP &
                                                 .or. config_flags%chem_opt == T1_MOZCART_KPP ) then
           do j=jts,jte
             ddvel(its:ite,j,p_sulf) = 0.
           end do
         ENDIF
         IF( p_o1d > param_first_scalar ) then
           do j=jts,jte
             ddvel(its:ite,j,p_o1d) = 0.
           end do
         ENDIF
         IF( p_o1d_cb4 > param_first_scalar ) then
           do j=jts,jte
             ddvel(its:ite,j,p_o1d_cb4) = 0.
           end do
         ENDIF
      end if

      
!-----------------------------------------------------------
! For CRI
!-----------------------------------------------------------
      if( config_flags%chem_opt == crimech_kpp .or. &
          config_flags%chem_opt == cri_mosaic_8bin_aq_kpp .or. &
          config_flags%chem_opt == cri_mosaic_4bin_aq_kpp  )   then
         do j=jts,jte
            do i=its,ite
! need to add deposition rates for crimech species here

               ddvel(i,j,p_ch3co2h)  = 0.
               ddvel(i,j,p_clno2)    = 0. 
               !ddvel(i,j,p_n2o5 )    = 0. 
               !ddvel(i,j,p_o1d)    = 0. 
               !ddvel(i,j,p_o3p)    = 0. 
               ddvel(i,j,p_c2h6)    = 0. 
               ddvel(i,j,p_aco3)    = 0. 
               !ddvel(i,j,p_ch3oo)    = 0. 
               ddvel(i,j,p_hso3)    = 0. 
               ddvel(i,j,p_so3)    = 0.       
               ddvel(i,j,p_c3h8)    = 0. 
               ddvel(i,j,p_nc4h10)    = 0. 
               ddvel(i,j,p_c5h8)    = 0. 
               ddvel(i,j,p_benzene)    = 0. 
               ddvel(i,j,p_toluene)    = 0. 
               ddvel(i,j,p_oxyl)    = 0. 
               ddvel(i,j,p_npropol)    = 0. 
               ddvel(i,j,p_c2h2)    = 0. 
               ddvel(i,j,p_c3h6)    = 0. 
               ddvel(i,j,p_c2h4) = 0.                
               ddvel(i,j,p_tbut2ene)    = 0. 
               ddvel(i,j,p_mek)    = 0. 
               ddvel(i,j,p_ipropol)    = 0. 
               ddvel(i,j,p_apinene)    = 0.   
               ddvel(i,j,p_bpinene)    = 0.      
               !ddvel(i,j,p_c2h5co3)    = 0.       
               !ddvel(i,j,p_hoch2co3)    = 0.               
               ddvel(i,j,p_ch3cl)    = 0.   
               ddvel(i,j,p_ch2cl2)    = 0.   
               ddvel(i,j,p_chcl3)    = 0.   
               ddvel(i,j,p_ch3ccl3)    = 0.  
               ddvel(i,j,p_cdicleth)    = 0.  
               ddvel(i,j,p_tdicleth)    = 0.  
               ddvel(i,j,p_tricleth )    = 0.  
               ddvel(i,j,p_tce)    = 0.   
               ddvel(i,j,p_noa)    = 0.    
               ddvel(i,j,p_aroh14)    = 0.    
               ddvel(i,j,p_raroh14)    = 0.   
               ddvel(i,j,p_arnoh14)    = 0.  
               ddvel(i,j,p_aroh17)    = 0.    
               ddvel(i,j,p_raroh17)    = 0.   
               ddvel(i,j,p_arnoh17)    = 0.  
               ddvel(i,j,p_anhy)    = 0.  
               ddvel(i,j,p_ch4)    = 0.  
               ddvel(i,j,p_sulf) = ddvel(i,j,p_hno3) 
               ddvel(i,j,p_hcl)    = ddvel(i,j,p_hno3)  
               ddvel(i,j,p_h2)    = 0.  
               ddvel(i,j,p_tm123b)    = 0. 
               ddvel(i,j,p_tm124b)    = 0. 
               ddvel(i,j,p_tm135b)    = 0. 
               ddvel(i,j,p_oethtol)    = 0. 
               ddvel(i,j,p_methtol)    = 0. 
               ddvel(i,j,p_pethtol)    = 0. 
               ddvel(i,j,p_dime35eb)    = 0. 
               ddvel(i,j,p_dms) = 0. 
               ddvel(i,j,p_ch3sch2oo) = 0.
               ddvel(i,j,p_dmso) = 0. 
               ddvel(i,j,p_ch3s) = 0. 
               ddvel(i,j,p_ch3so) = 0.
               ddvel(i,j,p_ch3so3) = 0.
               ddvel(i,j,p_msa) = 0. 
               ddvel(i,j,p_msia) = 0.
               ddvel(i,j,p_ho) = 0. 
               ddvel(i,j,p_ho2) = 0.
               ddvel(i,j,p_ch3oo) = 0. 
               ddvel(i,j,p_c2h5o2) = 0.       
               ddvel(i,j,p_hoch2ch2o2) = 0.
               ddvel(i,j,p_ic3h7o2) = 0.
               ddvel(i,j,p_rn10o2) = 0.    
               ddvel(i,j,p_rn13o2) = 0.           
               ddvel(i,j,p_rn16o2) = 0.     
               ddvel(i,j,p_rn19o2) = 0.   
               ddvel(i,j,p_rn9o2) = 0.       
               ddvel(i,j,p_rn12o2) = 0.       
               ddvel(i,j,p_rn15o2) = 0.     
               ddvel(i,j,p_rn18o2) = 0.      
               ddvel(i,j,p_nrn6o2) = 0.    
               ddvel(i,j,p_nrn9o2) = 0.      
               ddvel(i,j,p_nrn12o2) = 0.        
               ddvel(i,j,p_rn11o2) = 0.      
               ddvel(i,j,p_rn14o2) = 0.      
               ddvel(i,j,p_rn8o2) = 0.       
               ddvel(i,j,p_rn17o2) = 0.     
               ddvel(i,j,p_rn13ao2) = 0.  
               ddvel(i,j,p_rn16ao2) = 0.      
               ddvel(i,j,p_rn15ao2) = 0.     
               ddvel(i,j,p_rn18ao2) = 0.     
               ddvel(i,j,p_ru14o2) = 0.  
               ddvel(i,j,p_ru12o2) = 0.  
               ddvel(i,j,p_ru10o2) = 0.  
               ddvel(i,j,p_nru14o2) = 0. 
               ddvel(i,j,p_nru12o2) = 0.  
               ddvel(i,j,p_ra13o2) = 0.  
               ddvel(i,j,p_ra16o2) = 0.  
               ddvel(i,j,p_ra19ao2) = 0.   
               ddvel(i,j,p_ra19co2) = 0.   
               ddvel(i,j,p_rtn28o2) = 0.  
               ddvel(i,j,p_rtn26o2) = 0. 
               ddvel(i,j,p_nrtn28o2) = 0. 
               ddvel(i,j,p_rtn25o2) = 0.  
               ddvel(i,j,p_rtn24o2) = 0. 
               ddvel(i,j,p_rtn23o2) = 0.  
               ddvel(i,j,p_rtn14o2) = 0.  
               ddvel(i,j,p_rtn10o2) = 0.  
               ddvel(i,j,p_rtx28o2) = 0.  
               ddvel(i,j,p_rtx24o2) = 0.  
               ddvel(i,j,p_rtx22o2) = 0. 
               ddvel(i,j,p_nrtx28o2) = 0. 
               ddvel(i,j,p_ch3o2no2) = 0. 
               ddvel(i,j,p_ra22ao2) = 0.
               ddvel(i,j,p_ra22bo2) = 0.
               ddvel(i,j,p_ra25o2) = 0.
               ddvel(i,j,p_ch3so2) = 0.
               ddvel(i,j,p_dmso2 ) = 0. 
               

            end do
         end do
      end if

!-----------------------------------------------------------
! For cb05
!-----------------------------------------------------------
!
! For the additional CB05 species, assign similar RADM counter parts for
! now. Short lived species get a zero velocity since dry dep should be
! unimportant.  **ALSO**, treat p_sulf as h2so4 vapor, not aerosol sulfate
!
      if ( (config_flags%chem_opt == CB05_SORG_AQ_KPP) ) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_sulf)        = ddvel(i,j,p_hno3)
               ddvel(i,j,p_hcl)         = ddvel(i,j,p_hno3)
               ddvel(i,j,p_meo2)        = 0.
               ddvel(i,j,p_meoh)        = ddvel(i,j,p_form)
               ddvel(i,j,p_etoh)        = ddvel(i,j,p_form)
               ddvel(i,j,p_to2)         = 0.
               ddvel(i,j,p_cro)         = 0.
               ddvel(i,j,p_open)        = ddvel(i,j,p_xyl)
               ddvel(i,j,p_c2o3)        = 0.
               ddvel(i,j,p_xo2n)        = 0.
               ddvel(i,j,p_xo2)         = 0.
               ddvel(i,j,p_ispd)        = 0.
               ddvel(i,j,p_o1d)         = 0.
               ddvel(i,j,p_o)           = 0.
               ddvel(i,j,p_terp)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_terpaer)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_hum)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_humaer)      = ddvel(i,j,p_isop)
               ddvel(i,j,p_lim)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_limaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_limaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_oci)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_ociaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_ociaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_apin)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer1)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer2)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer3)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer4)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpin)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer1)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer2)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer3)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer4)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer5)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_ter)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_teraer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_teraer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_tolaer1)     = ddvel(i,j,p_tol)
               ddvel(i,j,p_tolaer2)     = ddvel(i,j,p_tol)
               ddvel(i,j,p_cslaer)      = ddvel(i,j,p_cres)
               ddvel(i,j,p_xylaer1)     = ddvel(i,j,p_xyl)
               ddvel(i,j,p_xylaer2)     = ddvel(i,j,p_xyl)
               ddvel(i,j,p_isoaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_isoaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_sulaer)      = ddvel(i,j,p_hno3)
               ddvel(i,j,p_panx)        = ddvel(i,j,p_pan)
               ddvel(i,j,p_hco3)        = 0.
               ddvel(i,j,p_ror)         = 0.
               ddvel(i,j,p_alkh)        = ddvel(i,j,p_par)
               ddvel(i,j,p_alkhaer1)    = ddvel(i,j,p_par)
               ddvel(i,j,p_pah)         = 0.
               ddvel(i,j,p_pahaer1)     = 0.
               ddvel(i,j,p_pahaer2)     = 0.
               ddvel(i,j,p_cl2)         = 0.
               ddvel(i,j,p_cl)          = 0.
               ddvel(i,j,p_hocl)        = 0.
               ddvel(i,j,p_clo)         = 0.
               ddvel(i,j,p_fmcl)        = 0.
               if (ivgtyp(i,j).eq.iswater_temp) then
                  ddvel(i,j,p_hg0)      = 0.00e-2
               else
                  ddvel(i,j,p_hg0)      = 0.01e-2
               end if
               ddvel(i,j,p_hg2)         = 0.50e-2
               end do
         end do
      else if ( (config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) ) then
         do j=jts,jte
            do i=its,ite
               ddvel(i,j,p_sulf)        = ddvel(i,j,p_hno3)
               ddvel(i,j,p_hcl)         = ddvel(i,j,p_hno3)
               ddvel(i,j,p_meo2)        = 0.
               ddvel(i,j,p_meoh)        = ddvel(i,j,p_form)
               ddvel(i,j,p_etoh)        = ddvel(i,j,p_form)
               ddvel(i,j,p_to2)         = 0.
               ddvel(i,j,p_cro)         = 0.
               ddvel(i,j,p_open)        = ddvel(i,j,p_xyl)
               ddvel(i,j,p_c2o3)        = 0.
               ddvel(i,j,p_xo2n)        = 0.
               ddvel(i,j,p_xo2)         = 0.
               ddvel(i,j,p_ispd)        = 0.
               ddvel(i,j,p_o1d)         = 0.
               ddvel(i,j,p_o)           = 0.
               ddvel(i,j,p_terp)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_terpaer)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_hum)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_humaer)      = ddvel(i,j,p_isop)
               ddvel(i,j,p_lim)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_limaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_limaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_oci)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_ociaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_ociaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_apin)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer1)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer2)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer3)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_apinaer4)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpin)        = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer1)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer2)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer3)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer4)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_bpinaer5)    = ddvel(i,j,p_isop)
               ddvel(i,j,p_ter)         = ddvel(i,j,p_isop)
               ddvel(i,j,p_teraer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_teraer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_tolaer1)     = ddvel(i,j,p_tol)
               ddvel(i,j,p_tolaer2)     = ddvel(i,j,p_tol)
               ddvel(i,j,p_cslaer)      = ddvel(i,j,p_cres)
               ddvel(i,j,p_xylaer1)     = ddvel(i,j,p_xyl)
               ddvel(i,j,p_xylaer2)     = ddvel(i,j,p_xyl)
               ddvel(i,j,p_isoaer1)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_isoaer2)     = ddvel(i,j,p_isop)
               ddvel(i,j,p_sulaer)      = ddvel(i,j,p_hno3)
               ddvel(i,j,p_panx)        = ddvel(i,j,p_pan)
               ddvel(i,j,p_hco3)        = 0.
               ddvel(i,j,p_ror)         = 0.
               ddvel(i,j,p_alkh)        = ddvel(i,j,p_par)
               ddvel(i,j,p_alkhaer1)    = ddvel(i,j,p_par)
               ddvel(i,j,p_pah)         = 0.
               ddvel(i,j,p_pahaer1)     = 0.
               ddvel(i,j,p_pahaer2)     = 0.
               ddvel(i,j,p_cl2)         = 0.
               ddvel(i,j,p_cl)          = 0.
               ddvel(i,j,p_hocl)        = 0.
               ddvel(i,j,p_clo)         = 0.
               ddvel(i,j,p_fmcl)        = 0.
               if (ivgtyp(i,j).eq.iswater_temp) then
                  ddvel(i,j,p_hg0)      = 0.00e-2
               else
                  ddvel(i,j,p_hg0)      = 0.01e-2
               end if
               ddvel(i,j,p_hg2)         = 0.50e-2
               ddvel(i,j,p_cvasoa1) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa2) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa3) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvasoa4) = ddvel(i,j,p_hno3)

               ddvel(i,j,p_cvbsoa1) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa2) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa3) = ddvel(i,j,p_hno3)
               ddvel(i,j,p_cvbsoa4) = ddvel(i,j,p_hno3)
               end do
         end do
      end if


END SUBROUTINE wesely_driver

      SUBROUTINE rc( rcx, t, rad, rh, iland, &
                     iseason, numgas, wetflag, rainflag, highnh3, &
                     iprt, spec_hum, p_srf, chem_opt, chm_is_moz )
!----------------------------------------------------------------------
!     THIS SUBROUTINE CALCULATES SURFACE RESISTENCES ACCORDING
!     TO THE MODEL OF
!     M. L. WESELY,
!     ATMOSPHERIC ENVIRONMENT 23 (1989), 1293-1304
!     WITH SOME ADDITIONS ACCORDING TO
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY  WINFRIED SEIDL, APRIL 1997
!     MODYFIED BY WINFRIED SEIDL, MARCH 2000
!                    FOR MM5 VERSION 3
!----------------------------------------------------------------------

  USE module_state_description                       

!----------------------------------------------------------------------
!	... dummy arguments
!----------------------------------------------------------------------
        INTEGER, intent(in) :: iland, iseason, numgas
        INTEGER, intent(in) :: iprt
        INTEGER, intent(in) :: chem_opt
        REAL, intent(in)    :: rad, rh
        REAL, intent(in)    :: t                            ! surface temp (K)
        REAL, intent(in)    :: p_srf                        ! surface pressure (Pa)
        REAL, intent(in)    :: spec_hum                     ! surface specific humidity (kg/kg)
        real, intent(out)   :: rcx(numgas)
        LOGICAL, intent(in) :: highnh3, rainflag, wetflag
        LOGICAL, intent(in) :: chm_is_moz

!----------------------------------------------------------------------
! .. Local Scalars ..
!----------------------------------------------------------------------
        REAL, parameter :: t0    = 298.
        REAL, parameter :: tmelt = 273.16
        INTEGER :: lt, n
        REAL    :: rclx, rdc, resice, rgsx, rluo1, rluo2
        REAL    :: rlux, rmx, rs, rsmx, rdtheta, z, wrk
        REAL    :: qs, es, ws, dewm, dv_pan, drat
        REAL    :: crs, tc
        REAL    :: rs_pan, tc_pan
        LOGICAL :: has_dew
!----------------------------------------------------------------------
! .. Local Arrays ..
!----------------------------------------------------------------------
        REAL :: hstary(numgas)

!----------------------------------------------------------------------
! .. Intrinsic Functions ..
!----------------------------------------------------------------------
        INTRINSIC exp

        rcx(1:numgas) = 1.

        tc = t - 273.15
        rdtheta = 0.

        z = 200./(rad+0.1)

!!!  HARDWIRE VALUES FOR TESTING
!       z=0.4727409
!       tc=22.76083
!       t=tc+273.15
!       rad = 412.8426
!       rainflag=.false.
!       wetflag=.false.

        IF ( tc<=0. .OR. tc>=40. ) THEN
          rs = 9999.
        ELSE
          rs = ri(iland,iseason)*(1+z*z)*(400./(tc*(40.-tc)))
        END IF
        rdc   = 100.*(1. + 1000./(rad + 10.))/(1. + 1000.*rdtheta)
        rluo1 = 1./(1./3000. + 3./rlu(iland,iseason))
        rluo2 = 1./(1./1000. + 3./rlu(iland,iseason))
        resice = 1000.*exp( -(tc + 4.) )
        wrk    = (t0 - t)/(t0*t)


        DO n = 1, numgas
          IF( hstar(n) /= 0. ) then
             hstary(n) = hstar(n)*exp( dhr(n)*wrk )
!----------------------------------------------------------------------
!     SPECIAL TREATMENT FOR HNO3, HNO4, H2O2, PAA
!----------------------------------------------------------------------
is_mozart :  if( chm_is_moz ) then
                 if( n == p_hno3 ) then
                    hstary(n) = 2.6e6*exp( 8700.*wrk )*1.e5
                 else if( n == p_hno4 ) then
                    hstary(n) = 32.e5
                 else if( n == p_h2o2 ) then
                    hstary(n) = hstary(n)*(1. + 2.2e-12*exp( -3730.*wrk )*1.e5)
                 else if( n == p_paa ) then
                    hstary(n) = hstary(n)*(1. + 1.8e-4*exp( -1510.*wrk )*1.e5)
                 end if
             endif is_mozart
             rmx = 1./(hstary(n)/3000. + 100.*f0(n))
             rsmx = rs*dratio(n) + rmx
             if( iseason /= 4 .and. p_pan >= param_first_scalar .and. chm_is_moz ) then
                if( iland /= iswater_temp .and. n == p_pan ) then
!-------------------------------------------------------------------------------------
! 	saturation vapor pressure (Pascals)
! 	saturation mixing ratio
! 	saturation specific humidity
!-------------------------------------------------------------------------------------
                    es = 611.*exp( 5414.77*(t - tmelt)/(tmelt*t) )
                    ws = .622*es/(p_srf - es)
                    qs = ws/(1. + ws)
                    has_dew = .false.
                    if( qs <= spec_hum ) then
                      has_dew = .true.
                    end if
                    if( t < tmelt ) then
                       has_dew = .false.
                    end if
                    if( has_dew .or. rainflag ) then
                       dewm = 3.
                    else
                       dewm = 1.
                    endif
                    drat   = wpan/wh2o
                    tc_pan = t - tmelt
                    if( t > tmelt .and. t < 313.15 ) then
                       crs = (1. + (200./(rad + .1))**2) * (400./(tc_pan*(40. - tc_pan)))
                    else
                       crs = large_value
                    end if
                    lt     = wrf2mz_lt_map(iland)
                    rs_pan = ri_pan(iseason,lt)*crs
                    dv_pan = c0_pan(lt) * (1. - exp( -k_pan(lt)*(dewm*rs_pan*drat)*1.e-2 ))
                    if( dv_pan > 0. ) then
                       rsmx = 1./dv_pan
                    end if
                endif
             endif
             rclx = 1./(1.e-5*hstary(n)/rcls(iland,iseason) &
                        + f0(n)/rclo(iland,iseason)) + resice
             rgsx = 1./(1.e-5*hstary(n)/rgss(iland,iseason) &
                        + f0(n)/rgso(iland,iseason)) + resice
             rlux = rlu(iland,iseason)/(1.e-5*hstary(n) + f0(n)) + resice
             IF( wetflag ) THEN
               rlux = 1./(1./(3.*rlu(iland,iseason)) + 1.e-7*hstary(n) + f0(n)/rluo1)
             END IF
             IF( rainflag ) THEN
               rlux = 1./(1./(3.*rlu(iland,iseason)) + 1.e-7*hstary(n) + f0(n)/rluo2)
             END IF
             rcx(n) = 1./(1./rsmx + 1./rlux + 1./(rdc + rclx) + 1./(rac(iland,iseason) + rgsx))
             rcx(n) = max( 1.,rcx(n) )
          end IF
        END DO

!--------------------------------------------------
!     SPECIAL TREATMENT FOR OZONE
!--------------------------------------------------
        if(p_o3 >= param_first_scalar)then
           hstary(p_o3) = hstar(p_o3)*exp( dhr(p_o3)*(298. - t)/(298.*t) )
           rmx = 1./(hstary(p_o3)/3000.+100.*f0(p_o3))
           rsmx = rs*dratio(p_o3) + rmx
           rlux = rlu(iland,iseason)/(1.E-5*hstary(p_o3)+f0(p_o3)) + resice
           rclx = rclo(iland,iseason) + resice
           rgsx = rgso(iland,iseason) + resice
           IF (wetflag) rlux = rluo1
           IF (rainflag) rlux = rluo2
           rcx(p_o3) = 1. &
                       /(1./rsmx+1./rlux+1./(rdc+rclx)+1./(min(100.,rac(iland,iseason))+rgsx))
           rcx(p_o3) = max( 1.,rcx(p_o3) )
        endif

!     SPECIAL TREATMENT FOR SO2 (Wesely)
!       HSTARY(P_SO2)=HSTAR(P_SO2)*EXP(DHR(P_SO2)*(1./T-1./298.))
!       RMX=1./(HSTARY(P_SO2)/3000.+100.*F0(P_SO2))
!       RSMX=RS*DRATIO(P_SO2)+RMX
!       RLUX=RLU(ILAND,ISEASON)/(1.E-5*HSTARY(P_SO2)+F0(P_SO2))
!    &       +RESICE
!       RCLX=RCLS(ILAND,ISEASON)+RESICE
!       RGSX=RGSS(ILAND,ISEASON)+RESICE
!       IF ((wetflag).OR.(RAINFLAG)) THEN
!         IF (ILAND.EQ.1) THEN
!           RLUX=50.
!         ELSE
!           RLUX=100.
!         END IF
!       END IF
!       RCX(P_SO2)=1./(1./RSMX+1./RLUX+1./(RDC+RCLX)
!    &                +1./(RAC(ILAND,ISEASON)+RGSX))
!       IF (RCX(P_SO2).LT.1.) RCX(P_SO2)=1.

!--------------------------------------------------
!     SO2 according to Erisman et al. 1994
!       R_STOM
!--------------------------------------------------
is_so2 : &
     if( p_so2 >= param_first_scalar ) then
        rsmx = rs*dratio(p_so2)
!--------------------------------------------------
!       R_EXT
!--------------------------------------------------
        IF (tc> -1. ) THEN
          IF (rh<81.3) THEN
            rlux = 25000.*exp(-0.0693*rh)
          ELSE
            rlux = 0.58E12*exp(-0.278*rh)
          END IF
        END IF
        IF (((wetflag) .OR. (rainflag)) .AND. (tc> -1. )) THEN
          rlux = 1.
        END IF
        IF ((tc>= -5. ) .AND. (tc<= -1. )) THEN
          rlux = 200.
        END IF
        IF (tc< -5. ) THEN
          rlux = 500.
        END IF
!--------------------------------------------------
!       INSTEAD OF R_INC R_CL and R_DC of Wesely are used
!--------------------------------------------------
        rclx = rcls(iland,iseason)
!--------------------------------------------------
!       DRY SURFACE
!--------------------------------------------------
        rgsx = 1000.
!--------------------------------------------------
!       WET SURFACE
!--------------------------------------------------
        IF ((wetflag) .OR. (rainflag)) THEN
          IF (highnh3) THEN
            rgsx = 0.
          ELSE
            rgsx = 500.
          END IF
        END IF
!--------------------------------------------------
!       WATER
!--------------------------------------------------
        IF (iland==iswater_temp) THEN
          rgsx = 0.
        END IF
!--------------------------------------------------
!       SNOW
!--------------------------------------------------
        IF( iseason==4 .OR. iland==isice_temp ) THEN
          IF( tc > 2. ) THEN
            rgsx = 0.
          else IF ( tc >= -1. .AND. tc <= 2. ) THEN
            rgsx = 70.*(2. - tc)
          else IF ( tc < -1. ) THEN
            rgsx = 500.
          END IF
        END IF
!--------------------------------------------------
!       TOTAL SURFACE RESISTENCE
!--------------------------------------------------
        IF ((iseason/=4) .AND. (ixxxlu(iland)/=1) .AND. (iland/=iswater_temp) .AND. &
            (iland/=isice_temp)) THEN
          rcx(p_so2) = 1./(1./rsmx+1./rlux+1./(rclx+rdc+rgsx))
        ELSE
          rcx(p_so2) = rgsx
        END IF
        rcx(p_so2) = max( 1.,rcx(p_so2) )
     end if is_so2
!--------------------------------------------------
!     NH3 according to Erisman et al. 1994
!       R_STOM
!--------------------------------------------------
is_nh3: if( p_nh3 >= param_first_scalar ) then
        rsmx = rs*dratio(p_nh3)
!--------------------------------------------------
!       GRASSLAND (PASTURE DURING GRAZING)
!--------------------------------------------------
        IF (ixxxlu(iland)==3) THEN
          IF (iseason==1) THEN
!--------------------------------------------------
!           SUMMER
!--------------------------------------------------
            rcx(p_nh3) = 1000.
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
            IF (tc>-1.) THEN
              IF (rad/=0.) THEN
                rcx(p_nh3) = 50.
              ELSE
                rcx(p_nh3) = 100.
              END IF
              IF ((wetflag) .OR. (rainflag)) THEN
                rcx(p_nh3) = 20.
              END IF
            END IF
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(p_nh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(p_nh3) = 500.
            END IF
          END IF
        END IF
!--------------------------------------------------
!       AGRICULTURAL LAND (CROPS AND UNGRAZED PASTURE)
!--------------------------------------------------
        IF (ixxxlu(iland)==2) THEN
          IF (iseason==1) THEN
!--------------------------------------------------
!           SUMMER
!--------------------------------------------------
            IF (rad/=0.) THEN
              rcx(p_nh3) = rsmx
            ELSE
              rcx(p_nh3) = 200.
            END IF
            IF ((wetflag) .OR. (rainflag)) THEN
              rcx(p_nh3) = 50.
            END IF
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
            IF (tc>-1.) THEN
              IF (rad/=0.) THEN
                rcx(p_nh3) = rsmx
              ELSE
                rcx(p_nh3) = 300.
              END IF
              IF ((wetflag) .OR. (rainflag)) THEN
                rcx(p_nh3) = 100.
              END IF
            END IF
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(p_nh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(p_nh3) = 500.
            END IF
          END IF
        END IF
!--------------------------------------------------
!       SEMI-NATURAL ECOSYSTEMS AND FORESTS
!--------------------------------------------------
        IF ((ixxxlu(iland)==4) .OR. (ixxxlu(iland)==5) .OR. (ixxxlu( &
            iland)==6)) THEN
          IF (rad/=0.) THEN
            rcx(p_nh3) = 500.
          ELSE
            rcx(p_nh3) = 1000.
          END IF
          IF ((wetflag) .OR. (rainflag)) THEN
            IF (highnh3) THEN
              rcx(p_nh3) = 100.
            ELSE
              rcx(p_nh3) = 0.
            END IF
          END IF
          IF ((iseason==2) .OR. (iseason==3) .OR. (iseason==5)) THEN
!--------------------------------------------------
!           WINTER, NO SNOW
!--------------------------------------------------
            IF ((tc>=(-5.)) .AND. (tc<=-1.)) THEN
              rcx(p_nh3) = 200.
            END IF
            IF (tc<(-5.)) THEN
              rcx(p_nh3) = 500.
            END IF
          END IF
        END IF
!--------------------------------------------------
!       WATER
!--------------------------------------------------
        IF (iland==iswater_temp) THEN
          rcx(p_nh3) = 0.
        END IF
!--------------------------------------------------
!       URBAN AND DESERT (SOIL SURFACES)
!--------------------------------------------------
        IF (ixxxlu(iland)==1) THEN
          IF ( .NOT. wetflag) THEN
            rcx(p_nh3) = 50.
          ELSE
            rcx(p_nh3) = 0.
          END IF
        END IF
!--------------------------------------------------
!       SNOW COVERED SURFACES OR PERMANENT ICE
!--------------------------------------------------
        IF ((iseason==4) .OR. (iland==isice_temp)) THEN
          IF (tc>2.) THEN
            rcx(p_nh3) = 0.
          END IF
          IF ((tc>=(-1.)) .AND. (tc<=2.)) THEN
            rcx(p_nh3) = 70.*(2.-tc)
          END IF
          IF (tc<(-1.)) THEN
            rcx(p_nh3) = 500.
          END IF
        END IF
        rcx(p_nh3) = max( 1.,rcx(p_nh3) )
        endif is_nh3
      END SUBROUTINE rc

      SUBROUTINE deppart( rmol, ustar, rh, clw, iland, &
                          dvpart, dvfog )
!--------------------------------------------------
!     THIS SUBROUTINE CALCULATES SURFACE DEPOSITION VELOCITIES
!     FOR FINE AEROSOL PARTICLES ACCORDING TO THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL, AND P. WYERS,
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607
!     WRITTEN BY WINFRIED SEIDL, APRIL 1997
!     MODIFIED BY WINFRIED SEIDL, MARCH 2000
!            FOR MM5 VERSION 3
!--------------------------------------------------

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: iland
        REAL, intent(in)    :: clw, rh, rmol, ustar
        REAL, intent(out)   :: dvfog, dvpart

!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC exp

        dvpart = ustar/kpart(iland)
        IF (rmol<0.) THEN
!--------------------------------------------------
!         UNSTABLE LAYERING CORRECTION
!--------------------------------------------------
          dvpart = dvpart*(1.+(-300.*rmol)**0.66667)
        END IF
        IF (rh>80.) THEN
!--------------------------------------------------
!         HIGH RELATIVE HUMIDITY CORRECTION
!         ACCORDING TO J. W. ERISMAN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 321-332
!--------------------------------------------------
          dvpart = dvpart*(1.+0.37*exp((rh-80.)/20.))
        END IF

!--------------------------------------------------
!       SEDIMENTATION VELOCITY OF FOG WATER ACCORDING TO
!       R. FORKEL, W. SEIDL, R. DLUGI AND E. DEIGELE
!       J. GEOPHYS. RES. 95D (1990), 18501-18515
!--------------------------------------------------
        dvfog = 0.06*clw
        IF (ixxxlu(iland)==5) THEN
!--------------------------------------------------
!         TURBULENT DEPOSITION OF FOG WATER IN CONIFEROUS FOREST ACCORDI
!         A. T. VERMEULEN ET AL.
!         ATMOSPHERIC ENVIRONMENT 31 (1997), 375-386
!--------------------------------------------------
          dvfog = dvfog + 0.195*ustar*ustar
        END IF

      END SUBROUTINE deppart

      SUBROUTINE landusevg( vgs, ustar, rmol, z0, zz, &
                            dvparx, iland, numgas, srfres, aer_res_def, &
                            aer_res_zcen, p_sulf )
!--------------------------------------------------
!     This subroutine calculates the species specific deposition velocit
!     as a function of the local meteorology and land use.  The depositi
!     Velocity is also landuse specific.
!     Reference: Hsieh, C.M., Wesely, M.L. and Walcek, C.J. (1986)
!                A Dry Deposition Module for Regional Acid Deposition
!                EPA report under agreement DW89930060-01
!     Revised version by Darrell Winner (January 1991)
!        Environmental Engineering Science 138-78
!           California Institute of Technology
!              Pasadena, CA  91125
!     Modified by Winfried Seidl (August 1997)
!       Fraunhofer-Institut fuer Atmosphaerische Umweltforschung
!                    Garmisch-Partenkirchen, D-82467
!          for use of Wesely and Erisman surface resistances
!     Inputs:
!        Ustar  : The grid average friction velocity (m/s)
!        Rmol   : Reciprocal of the Monin-Obukhov length (1/m)
!        Z0     : Surface roughness height for the grid square (m)
!        SrfRes : Array of landuse/atmospheric/species resistances (s/m)
!        Slist  : Array of chemical species codes
!        Dvparx : Array of surface deposition velocity of fine aerosol p
!     Outputs:
!        Vgs    : Array of species and landuse specific deposition
!                 velocities (m/s)
!        Vg     : Cell-average deposition velocity by species (m/s)
!     Variables used:
!        SCPR23  : (Schmidt #/Prandtl #)**(2/3) Diffusion correction fac
!        Zr      : Reference Height (m)
!        Iatmo   : Parameter specifying the stabilty class (Function of
!        Z0      : Surface roughness height (m)
!        karman  : Von Karman constant (from module_model_constants)
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: iland, numgas, p_sulf
        REAL, intent(in)    :: dvparx, ustar, z0, zz
        REAL, intent(inout) :: rmol
        REAL, intent(inout) :: aer_res_def
        REAL, intent(inout) :: aer_res_zcen
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(in)  :: srfres(numgas)
        REAL, intent(out) :: vgs(numgas)

!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: jspec
        REAL    :: vgp, vgpart, zr
        REAL    :: rmol_tmp
!--------------------------------------------------
! .. Local Arrays ..
!--------------------------------------------------
        REAL :: vgspec(numgas)

!--------------------------------------------------
!   Calculate aerodynamic resistance for reference
!   height = layer center
!--------------------------------------------------
        zr = zz*.5
        rmol_tmp = rmol
        CALL depvel( numgas, rmol_tmp, zr, z0, ustar, &
                     vgspec, vgpart, aer_res_zcen )
!--------------------------------------------------
!   Set the reference height (2.0 m)
!--------------------------------------------------
!       zr = 10.0
        zr = 2.0

!--------------------------------------------------
!   CALCULATE THE DEPOSITION VELOCITY without any surface
!   resistance term, i.e. 1 / (ra + rb)
!--------------------------------------------------
        CALL depvel( numgas, rmol, zr, z0, ustar, &
                     vgspec, vgpart, aer_res_def )

!--------------------------------------------------
!   Calculate the deposition velocity for each species
!   and grid cell by looping through all the possibile combinations
!   of the two
!--------------------------------------------------
        vgp = 1.0/((1.0/vgpart)+(1.0/dvparx))
!--------------------------------------------------
!   Loop through the various species
!--------------------------------------------------
        DO jspec = 1, numgas
!--------------------------------------------------
!   Add in the surface resistance term, rc (SrfRes)
!--------------------------------------------------
          vgs(jspec) = 1.0/(1.0/vgspec(jspec) + srfres(jspec))
        END DO
        vgs(p_sulf) = vgp

        CALL cellvg( vgs, ustar, zz, zr, rmol, numgas )

      END SUBROUTINE landusevg

      SUBROUTINE cellvg( vgtemp, ustar, dz, zr, rmol, nspec )
!--------------------------------------------------
!     THIS PROGRAM HAS BEEN DESIGNED TO CALCULATE THE CELL AVERAGE
!     DEPOSITION VELOCITY GIVEN THE VALUE OF VG AT SOME REFERENCE
!     HEIGHT ZR WHICH IS MUCH SMALLER THAN THE CELL HEIGHT DZ.
!       PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner    (February 1991)
!.....PROGRAM VARIABLES...
!     VgTemp   - DEPOSITION VELOCITY AT THE REFERENCE HEIGHT
!     USTAR    - FRICTION VELOCITY
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     DZ       - CELL HEIGHT
!     CELLVG   - CELL AVERAGE DEPOSITION VELOCITY
!     VK       - VON KARMAN CONSTANT
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: nspec
        REAL, intent(in)    :: dz, rmol, ustar, zr
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(out) :: vgtemp(nspec)
!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: nss
        REAL    :: a, fac, pdz, pzr, vk
!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC alog, sqrt

!--------------------------------------------------
!     Set the von Karman constant
!--------------------------------------------------
        vk = karman

!--------------------------------------------------
!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE
!--------------------------------------------------
        DO nss = 1, nspec
          IF (rmol < 0.) THEN
            pdz = sqrt(1.0 - 9.0*dz*rmol)
            pzr = sqrt(1.0 - 9.0*zr*rmol)
            fac = ((pdz - 1.0)/(pzr - 1.0))*((pzr + 1.0)/(pdz + 1.0))
            a   = 0.74*dz*alog(fac) + (0.164/rmol)*(pdz-pzr)
          ELSE IF (rmol == 0.) THEN
            a = 0.74*(dz*alog(dz/zr) - dz + zr)
          ELSE
            a = 0.74*(dz*alog(dz/zr) - dz + zr) + (2.35*rmol)*(dz - zr)**2
          END IF
!--------------------------------------------------
!     CALCULATE THE DEPOSITION VELOCITIY
!--------------------------------------------------
          vgtemp(nss) = vgtemp(nss)/(1.0 + vgtemp(nss)*a/(vk*ustar*(dz - zr)))
        END DO

      END SUBROUTINE cellvg

      SUBROUTINE depvel( numgas, rmol, zr, z0, ustar, &
                         depv, vgpart, aer_res )
!--------------------------------------------------
!     THIS FUNCTION HAS BEEN DESIGNED TO EVALUATE AN UPPER LIMIT
!     FOR THE POLLUTANT DEPOSITION VELOCITY AS A FUNCTION OF THE
!     SURFACE ROUGHNESS AND METEOROLOGICAL CONDITIONS.
!     PROGRAM WRITTEN BY GREGORY J.MCRAE (NOVEMBER 1977)
!         Modified by Darrell A. Winner  (Feb. 1991)
!                  by Winfried Seidl     (Aug. 1997)
!.....PROGRAM VARIABLES...
!     RMOL     - RECIPROCAL OF THE MONIN-OBUKHOV LENGTH
!     ZR       - REFERENCE HEIGHT
!     Z0       - SURFACE ROUGHNESS HEIGHT
!     SCPR23   - (Schmidt #/Prandtl #)**(2/3) Diffusion correction fact
!     UBAR     - ABSOLUTE VALUE OF SURFACE WIND SPEED
!     DEPVEL   - POLLUTANT DEPOSITION VELOCITY
!     Vk       - VON KARMAN CONSTANT
!     USTAR    - FRICTION VELOCITY U*
!     POLINT   - POLLUTANT INTEGRAL
!     AER_RES  - AERODYNAMIC RESISTANCE
!.....REFERENCES...
!     MCRAE, G.J. ET AL. (1983) MATHEMATICAL MODELING OF PHOTOCHEMICAL
!       AIR POLLUTION, ENVIRONMENTAL QUALITY LABORATORY REPORT 18,
!       CALIFORNIA INSTITUTE OF TECHNOLOGY, PASADENA, CALIFORNIA.
!.....RESTRICTIONS...
!     1. THE MODEL EDDY DIFFUSIVITIES ARE BASED ON MONIN-OBUKHOV
!        SIMILARITY THEORY AND SO ARE ONLY APPLICABLE IN THE
!        SURFACE LAYER, A HEIGHT OF O(30M).
!     2. ALL INPUT UNITS MUST BE CONSISTENT
!     3. THE PHI FUNCTIONS USED TO CALCULATE THE FRICTION
!        VELOCITY U* AND THE POLLUTANT INTEGRALS ARE BASED
!        ON THE WORK OF BUSINGER ET AL.(1971).
!     4. THE MOMENTUM AND POLLUTANT DIFFUSIVITIES ARE NOT
!        THE SAME FOR THE CASES L<0 AND L>0.
!--------------------------------------------------

        USE module_model_constants, only: karman

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        INTEGER, intent(in) :: numgas
        REAL, intent(in)    :: ustar, z0, zr
        REAL, intent(out)   :: vgpart, aer_res
        REAL, intent(inout) :: rmol
!--------------------------------------------------
! .. Array Arguments ..
!--------------------------------------------------
        REAL, intent(out) :: depv(numgas)
!--------------------------------------------------
! .. Local Scalars ..
!--------------------------------------------------
        INTEGER :: l
        REAL    :: ao, ar, polint, vk
!--------------------------------------------------
! .. Intrinsic Functions ..
!--------------------------------------------------
        INTRINSIC alog
!--------------------------------------------------
!     Set the von Karman constant
!--------------------------------------------------
        vk = karman

!--------------------------------------------------
!     Calculate the diffusion correction factor
!     SCPR23 is calculated as (Sc/Pr)**(2/3) using Sc= 1.15 and Pr= 1.0
!     SCPR23 = 1.10
!--------------------------------------------------
!     DETERMINE THE STABILITY BASED ON THE CONDITIONS
!             1/L < 0 UNSTABLE
!             1/L = 0 NEUTRAL
!             1/L > 0 STABLE
!--------------------------------------------------

        if(abs(rmol) < 1.E-6 ) rmol = 0.

        IF (rmol<0) THEN
          ar = ((1.0-9.0*zr*rmol)**(0.25)+0.001)**2
          ao = ((1.0-9.0*z0*rmol)**(0.25)+0.001)**2
          polint = 0.74*(alog((ar-1.0)/(ar+1.0))-alog((ao-1.0)/(ao+1.0)))
        ELSE IF (rmol==0.) THEN
          polint = 0.74*alog(zr/z0)
        ELSE
          polint = 0.74*alog(zr/z0) + 4.7*rmol*(zr-z0)
        END IF

!--------------------------------------------------
!     CALCULATE THE Maximum DEPOSITION VELOCITY
!--------------------------------------------------
        DO l = 1, numgas
          depv(l) = ustar*vk/(2.0*scpr23(l)+polint)
        END DO
        vgpart = ustar*vk/polint
        aer_res = polint/(karman*max(ustar,1.0e-4))

      END SUBROUTINE depvel

      SUBROUTINE dep_init( id, config_flags, numgas, mminlu_loc, &
                           ips, ipe, jps, jpe, ide, jde )
!--

!--------------------------------------------------
! .. Initialize simple deposition velocity routine
!--------------------------------------------------

  USE module_model_constants
  USE module_configure
  USE module_state_description                       

   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
        character(len=*), intent(in) :: mminlu_loc
!--

!--------------------------------------------------
! .. Scalar Arguments ..
!--------------------------------------------------
        integer, intent(in) :: id, numgas
        integer, intent(in) :: ips, ipe, jps, jpe
        integer, intent(in) :: ide, jde

!--------------------------------------------------
! .. Local Scalars
!--------------------------------------------------
        INTEGER :: iland, iseason, l, m
        integer :: iprt
        integer :: astat
        integer :: ncid
        integer :: dimid
        integer :: varid
        integer :: max_dom
        integer :: cpos, slen
        integer :: lon_e, lat_e
        integer :: iend, jend
        integer, allocatable :: input_wes_seasonal(:,:,:,:)
        REAL    :: sc
        character(len=128) :: err_msg
        character(len=128) :: filename
        character(len=3)   :: id_num
        LOGICAL :: chm_is_moz
!--------------------------------------------------
! .. Local Arrays
!--------------------------------------------------
        REAL :: dat1(nlu,dep_seasons), dat2(nlu,dep_seasons),         &
                dat3(nlu,dep_seasons), dat4(nlu,dep_seasons),         &
                dat5(nlu,dep_seasons), dat6(nlu,dep_seasons),         &
                dat7(nlu,dep_seasons), dvj(numgas)

#ifdef NETCDF
      LOGICAL , EXTERNAL      :: wrf_dm_on_monitor
#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * IWORDSIZE )
include 'netcdf.inc'

#else
      if( config_flags%chem_opt == MOZART_KPP .or. &
          config_flags%chem_opt == MOZCART_KPP .or. &
          config_flags%chem_opt == T1_MOZCART_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
         call wrf_message( 'dep_init: mozart,mozcart chem option requires netcdf' )
         call wrf_abort
      end if
#endif

!--------------------------------------------------
! .. Make sure that the model is being run with a soil model. Otherwise,
!    iland will be zero in deppart, which will try to pull non-exisant
!    array locations.
!--------------------------------------------------
        call nl_get_sf_surface_physics(id,l)
        if( l == 0 ) &
             call wrf_error_fatal("ERROR: Cannot use dry deposition without using a soil model.")

! ..
! .. Data Statements ..
!     RI for stomatal resistance
!      data ((ri(ILAND,ISEASON),ILAND=1,nlu),ISEASON=1,dep_seasons)/0.10E+11, &
        DATA ((dat1(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.60E+02, 0.60E+02, 0.60E+02, 0.60E+02, 0.70E+02, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.12E+03, 0.70E+02, 0.13E+03, 0.70E+02, &
          0.13E+03, 0.10E+03, 0.10E+11, 0.80E+02, 0.10E+03, 0.10E+11, &
          0.80E+02, 0.10E+03, 0.10E+03, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, 0.10E+11, &
          0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, &
          0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.10E+11, &
          0.10E+11, 0.70E+02, 0.25E+03, 0.50E+03, 0.10E+11, 0.10E+11, &
          0.50E+03, 0.10E+11, 0.10E+11, 0.50E+03, 0.50E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.70E+02, 0.40E+03, 0.80E+03, 0.10E+11, &
          0.10E+11, 0.80E+03, 0.10E+11, 0.10E+11, 0.80E+03, 0.80E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.12E+03, 0.12E+03, &
          0.12E+03, 0.12E+03, 0.14E+03, 0.24E+03, 0.24E+03, 0.24E+03, &
          0.12E+03, 0.14E+03, 0.25E+03, 0.70E+02, 0.25E+03, 0.19E+03, &
          0.10E+11, 0.16E+03, 0.19E+03, 0.10E+11, 0.16E+03, 0.19E+03, &
          0.19E+03, 0.10E+11, 0.10E+11, 0.10E+11/
! ..
        IF (nlu/=25) THEN
          call wrf_debug(0, 'number of land use classifications not correct ')
          CALL wrf_error_fatal ( "LAND USE CLASSIFICATIONS NOT 25")
        END IF
        IF (dep_seasons/=5) THEN
          call wrf_debug(0, 'number of dep_seasons not correct ')
          CALL wrf_error_fatal ( "DEP_SEASONS NOT 5")
        END IF

!     SURFACE RESISTANCE DATA FOR DEPOSITION MODEL OF
!     M. L. WESELY, ATMOSPHERIC ENVIRONMENT 23 (1989) 1293-1304

!     Seasonal categories:
!     1: midsummer with lush vegetation
!     2: autumn with unharvested cropland
!     3: late autumn with frost, no snow
!     4: winter, snow on ground and subfreezing
!     5: transitional spring with partially green short annuals

!     Land use types:
!     USGS type                                Wesely type
!      1: Urban and built-up land              1
!      2: Dryland cropland and pasture         2
!      3: Irrigated cropland and pasture       2
!      4: Mix. dry/irrg. cropland and pasture  2
!      5: Cropland/grassland mosaic            2
!      6: Cropland/woodland mosaic             4
!      7: Grassland                            3
!      8: Shrubland                            3
!      9: Mixed shrubland/grassland            3
!     10: Savanna                              3, always summer
!     11: Deciduous broadleaf forest           4
!     12: Deciduous needleleaf forest          5, autumn and winter modi
!     13: Evergreen broadleaf forest           4, always summer
!     14: Evergreen needleleaf forest          5
!     15: Mixed Forest                         6
!     16: Water Bodies                         7
!     17: Herbaceous wetland                   9
!     18: Wooded wetland                       6
!     19: Barren or sparsely vegetated         8
!     20: Herbaceous Tundra                    9
!     21: Wooded Tundra                        6
!     22: Mixed Tundra                         6
!     23: Bare Ground Tundra                   8
!     24: Snow or Ice                          -, always winter
!     25: No data                              8


!     Order of data:
!      |
!      |   seasonal category
!     \|/
!     ---> landuse type
!     1       2       3       4       5       6       7       8       9
!     RLU for outer surfaces in the upper canopy
        DO iseason = 1, dep_seasons
           ri(1:nlu,iseason) = dat1(1:nlu,iseason)
        END DO
!      data ((rlu(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat2(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, &
          0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.40E+04, 0.80E+04, 0.10E+11, 0.90E+04, &
          0.80E+04, 0.10E+11, 0.90E+04, 0.80E+04, 0.80E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.20E+04, 0.60E+04, 0.90E+04, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rlu(1:nlu,iseason) = dat2(1:nlu,iseason)
        END DO
!     RAC for transfer that depends on canopy height and density
!      data ((rac(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+03, &
        DATA ((dat3(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+04, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.00E+00, 0.30E+03, 0.20E+04, 0.00E+00, &
          0.30E+03, 0.20E+04, 0.20E+04, 0.00E+00, 0.00E+00, 0.00E+00, &
          0.10E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+04, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.15E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.17E+04, 0.00E+00, 0.20E+03, 0.17E+04, &
          0.00E+00, 0.20E+03, 0.17E+04, 0.17E+04, 0.00E+00, 0.00E+00, &
          0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, 0.10E+03, &
          0.15E+04, 0.00E+00, 0.10E+03, 0.15E+04, 0.15E+04, 0.00E+00, &
          0.00E+00, 0.00E+00, 0.10E+03, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+02, 0.10E+04, 0.10E+02, 0.10E+02, 0.10E+02, 0.10E+02, &
          0.10E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, 0.00E+00, &
          0.50E+02, 0.15E+04, 0.00E+00, 0.50E+02, 0.15E+04, 0.15E+04, &
          0.00E+00, 0.00E+00, 0.00E+00, 0.10E+03, 0.50E+02, 0.50E+02, &
          0.50E+02, 0.50E+02, 0.12E+04, 0.80E+02, 0.80E+02, 0.80E+02, &
          0.10E+03, 0.12E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.15E+04, &
          0.00E+00, 0.20E+03, 0.15E+04, 0.00E+00, 0.20E+03, 0.15E+04, &
          0.15E+04, 0.00E+00, 0.00E+00, 0.00E+00/
        DO iseason = 1, dep_seasons
           rac(1:nlu,iseason) = dat3(1:nlu,iseason)
        END DO
!     RGSS for ground surface  SO2
!      data ((rgss(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.40E+03, &
        DATA ((dat4(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.40E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, 0.10E+04, &
          0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+04, &
          0.40E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.50E+03, &
          0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.10E+03, 0.10E+01, 0.10E+01, 0.10E+03, &
          0.10E+04, 0.10E+01, 0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, &
          0.10E+04, 0.40E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.35E+03, 0.50E+03, &
          0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, 0.10E+01, 0.10E+01, &
          0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, 0.20E+03, 0.10E+04, &
          0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+03, 0.10E+03, 0.50E+03, 0.10E+03, 0.10E+03, 0.10E+01, &
          0.10E+03, 0.10E+03, 0.10E+04, 0.10E+03, 0.10E+03, 0.10E+03, &
          0.10E+04, 0.10E+03, 0.10E+04, 0.50E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.50E+03, 0.35E+03, 0.35E+03, 0.35E+03, &
          0.35E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, 0.20E+03, &
          0.10E+01, 0.10E+01, 0.20E+03, 0.10E+04, 0.10E+01, 0.20E+03, &
          0.20E+03, 0.10E+04, 0.10E+03, 0.10E+04/
        DO iseason = 1, dep_seasons
           rgss(1:nlu,iseason) = dat4(1:nlu,iseason)
        END DO
!     RGSO for ground surface  O3
!      data ((rgso(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.30E+03, &
        DATA ((dat5(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.30E+03, &
          0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, &
          0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03, &
          0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.80E+03, 0.30E+03, &
          0.40E+03, 0.80E+03, 0.30E+03, 0.30E+03, 0.40E+03, 0.35E+04, &
          0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, 0.15E+03, 0.15E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, 0.20E+04, 0.10E+04, &
          0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, 0.30E+03, 0.40E+03, &
          0.35E+04, 0.40E+03, 0.60E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.35E+04, 0.35E+04, 0.20E+03, 0.35E+04, 0.35E+04, 0.20E+04, &
          0.35E+04, 0.35E+04, 0.40E+03, 0.35E+04, 0.35E+04, 0.35E+04, &
          0.40E+03, 0.35E+04, 0.40E+03, 0.30E+03, 0.15E+03, 0.15E+03, &
          0.15E+03, 0.15E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, &
          0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.20E+03, 0.30E+03, &
          0.20E+04, 0.10E+04, 0.30E+03, 0.40E+03, 0.10E+04, 0.30E+03, &
          0.30E+03, 0.40E+03, 0.35E+04, 0.40E+03/
        DO iseason = 1, dep_seasons
           rgso(1:nlu,iseason) = dat5(1:nlu,iseason)
        END DO
!     RCLS for exposed surfaces in the lower canopy  SO2
!      data ((rcls(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat6(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.20E+04, &
          0.20E+04, 0.20E+04, 0.10E+11, 0.25E+04, 0.20E+04, 0.10E+11, &
          0.25E+04, 0.20E+04, 0.20E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, 0.90E+04, &
          0.20E+04, 0.20E+04, 0.40E+04, 0.10E+11, 0.90E+04, 0.40E+04, &
          0.10E+11, 0.90E+04, 0.40E+04, 0.40E+04, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.90E+04, 0.90E+04, 0.20E+04, 0.90E+04, &
          0.90E+04, 0.20E+04, 0.30E+04, 0.60E+04, 0.10E+11, 0.90E+04, &
          0.60E+04, 0.10E+11, 0.90E+04, 0.60E+04, 0.60E+04, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.90E+04, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.90E+04, 0.90E+04, 0.20E+04, 0.20E+03, 0.40E+03, 0.10E+11, &
          0.90E+04, 0.40E+03, 0.10E+11, 0.90E+04, 0.40E+03, 0.40E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.40E+04, 0.40E+04, &
          0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, 0.40E+04, &
          0.20E+04, 0.40E+04, 0.20E+04, 0.20E+04, 0.20E+04, 0.30E+04, &
          0.10E+11, 0.40E+04, 0.30E+04, 0.10E+11, 0.40E+04, 0.30E+04, &
          0.30E+04, 0.10E+11, 0.10E+11, 0.10E+11/
        DO iseason = 1, dep_seasons
           rcls(1:nlu,iseason) = dat6(1:nlu,iseason)
        END DO
!     RCLO for exposed surfaces in the lower canopy  O3
!      data ((rclo(ILAND,ISEASON),ILAND=1,25),ISEASON=1,5)/0.10E+11, &
        DATA ((dat7(iland,iseason),iland=1,nlu),iseason=1,dep_seasons)/0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+11, &
          0.10E+04, 0.10E+04, 0.10E+04, 0.10E+11, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, 0.40E+03, &
          0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.40E+03, 0.60E+03, &
          0.10E+11, 0.40E+03, 0.60E+03, 0.60E+03, 0.10E+11, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.40E+03, 0.40E+03, 0.10E+04, 0.40E+03, &
          0.40E+03, 0.10E+04, 0.10E+04, 0.60E+03, 0.10E+11, 0.80E+03, &
          0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, 0.10E+11, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.40E+03, 0.10E+04, 0.10E+04, 0.10E+04, 0.10E+04, &
          0.40E+03, 0.40E+03, 0.10E+04, 0.15E+04, 0.60E+03, 0.10E+11, &
          0.80E+03, 0.60E+03, 0.10E+11, 0.80E+03, 0.60E+03, 0.60E+03, &
          0.10E+11, 0.10E+11, 0.10E+11, 0.10E+11, 0.10E+04, 0.10E+04, &
          0.10E+04, 0.10E+04, 0.50E+03, 0.50E+03, 0.50E+03, 0.50E+03, &
          0.10E+04, 0.50E+03, 0.15E+04, 0.10E+04, 0.15E+04, 0.70E+03, &
          0.10E+11, 0.60E+03, 0.70E+03, 0.10E+11, 0.60E+03, 0.70E+03, &
          0.70E+03, 0.10E+11, 0.10E+11, 0.10E+11/

        DO iseason = 1, dep_seasons
           rclo(1:nlu,iseason) = dat7(1:nlu,iseason)
        END DO

!     data ((dat8(iseason,iland),iseason=1,5),iland=1,11) / &
!                          1.e36,  60., 120.,  70., 130., 100.,1.e36,1.e36,  80., 100., 150., &
!                          1.e36,1.e36,1.e36,1.e36, 250., 500.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36,1.e36,1.e36,1.e36, 250., 500.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36,1.e36,1.e36,1.e36, 400., 800.,1.e36,1.e36,1.e36,1.e36,1.e36, &
!                          1.e36, 120., 240., 140., 250., 190.,1.e36,1.e36, 160., 200., 300. /
!       ri_pan(:,:) = dat8(:,:)

!--------------------------------------------------
!	Initialize parameters
!--------------------------------------------------
        hstar(1:numgas)  = 0.
        hstar4(1:numgas) = 0.
        dhr(1:numgas)    = 0.
        f0(1:numgas)     = 0.
        dvj(1:numgas)    = 99.

        if( .not. allocated(luse2usgs) ) then
          allocate( luse2usgs(config_flags%num_land_cat),stat=astat )
          if( astat /= 0 ) then
            CALL wrf_error_fatal( 'dep_init: failed to allocate luse2usgs array' )
          end if
          if( trim(mminlu_loc) == 'USGS' ) then
            luse2usgs(:) = (/ (iland,iland=1,config_flags%num_land_cat) /)
          elseif( trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' ) then
            luse2usgs(:) = (/ 14,13,12,11,15,8,9,10,10,7, &
                              17,4,1,5,24,19,16,21,22,23 /)
          endif
        endif
!--
        chm_is_moz = config_flags%chem_opt == MOZART_KPP .or. &
               config_flags%chem_opt == MOZCART_KPP .or. &
               config_flags%chem_opt == T1_MOZCART_KPP .or. &
               config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
               config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP

!--------------------------------------------------
!     HENRY''S LAW COEFFICIENTS
!     Effective Henry''s law coefficient at pH 7
!     [KH298]=mole/(l atm)
!--------------------------------------------------
is_cbm4_kpp : &
        if( .not. (config_flags%chem_opt == CBM4_KPP .or. &
                   config_flags%chem_opt == CB05_SORG_AQ_KPP .or. &
                   config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) ) then
           if( chm_is_moz ) then
              hstar(p_o3)      = 1.15E-2
              hstar(p_co)      = 1.e-3
              hstar(p_h2o2)    = 8.33E+4
              hstar(p_hcho)    = 6.3e3
              hstar(p_ch3ooh)  = 311.
              hstar(p_ch3oh)   = 220.
              hstar(p_ch3cooh) = 6.3e3
              hstar(p_acet)    = 27.
              hstar(p_paa)     = 837.
              hstar(p_c3h6ooh) = 220.
              hstar(p_pan)     = 5.
              hstar(p_mpan)    = 1.15e-2
              hstar(p_c2h5oh)  = 200.
              hstar(p_etooh)   = 336.
              hstar(p_prooh)   = 336.
              hstar(p_acetp)   = 336.
              hstar(p_onit)    = 1.e3
              hstar(p_onitr)   = 7.51e3
              hstar(p_acetol)  = 6.3e3
              hstar(p_glyald)  = 4.14e4
              hstar(p_hydrald) = 70.
              hstar(p_alkooh)  = 311.
              hstar(p_mekooh)  = 311.
              hstar(p_tolooh)  = 311.
              hstar(p_terpooh) = 311.
              if (config_flags%chem_opt == T1_MOZCART_KPP ) then
                hstar(p_alknit)   = 1.0e+03
                hstar(p_ch3cn)    = 5.3e+01
                hstar(p_eooh)     = 1.7e+06
                hstar(p_hcn)      = 1.2e+01
                hstar(p_honitr)   = 1.0e+03
                hstar(p_hpald)    = 9.5e+02
                hstar(p_iepox)    = 3.0e+07
                hstar(p_isopao2)  = 2.0e+03
                hstar(p_isopbo2)  = 2.0e+03
                hstar(p_isopnita) = 9.5e+02
                hstar(p_isopnitb) = 9.5e+02
                hstar(p_isopnooh) = 3.4e+02
                hstar(p_nc4ch2oh) = 4.0e+04
                hstar(p_nc4cho)   = 1.0e+03
                hstar(p_noa)      = 1.0e+03
                hstar(p_nterpooh) = 1.0e+03
                hstar(p_terpnit)  = 9.5e+02
              endif

              if( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
                  config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
                hstar(p_sulf) = 2.600E+06
                if ( config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
                  hstar(p_cvasoaX) = 0.0
                  hstar(p_cvasoa1) = 1.06E+08
                  hstar(p_cvasoa2) = 1.84E+07
                  hstar(p_cvasoa3) = 3.18E+06
                  hstar(p_cvasoa4) = 5.50E+05
                  hstar(p_cvbsoaX) = 0.0
                  hstar(p_cvbsoa1) = 5.25E+09
                  hstar(p_cvbsoa2) = 7.00E+08
                  hstar(p_cvbsoa3) = 9.33E+07
                  hstar(p_cvbsoa4) = 1.24E+07
                endif
              end if
              
     else if( config_flags%chem_opt == crimech_kpp .or. &
          config_flags%chem_opt == cri_mosaic_8bin_aq_kpp .or. &
          config_flags%chem_opt == cri_mosaic_4bin_aq_kpp )   then
              hstar(p_o3)      = 1.15E-2
              hstar(p_co)      = 1.e-3
              hstar(p_h2o2)    = 8.33E+4
              hstar(p_hcho)    = 6.3e3
              hstar(p_ch3ooh)  = 311.
              hstar(p_ch3oh)   = 220.
              hstar(p_ch3cooh) = 6.3e3
              hstar(p_ket)    = 27.
              hstar(p_paa)     = 837.
               hstar(p_c2h5co3h)    = 837.
               hstar(p_hoch2co3h)    = 837.               
!              hstar(p_c3h6ooh) = 220.
              hstar(p_pan)     = 5.
              hstar(p_mpan)    = 1.15e-2
              hstar(p_ru12pan)    = 1.15e-2
              hstar(p_rtn26pan)    = 1.15e-2
               hstar(p_phan)    = 1.15e-2  
               hstar(p_ppn)    = 1.15e-2 
              hstar(p_c2h5oh)  = 200.
              hstar(p_c2h5ooh)   = 336.
!              hstar(p_ic3h7ooh)   = 336.
!              hstar(p_acetp)   = 336.
!              hstar(p_onit)    = 1.e3
!!              hstar(p_onitr)   = 7.51e3
!              hstar(p_acetol)  = 6.3e3
!              hstar(p_glyald)  = 4.14e4
!              hstar(p_hydrald) = 70.
               hstar(p_hcooh) = 311. 
               hstar(p_prooh) = 311. 
               hstar(p_hoc2h4ooh) = 311. 
               hstar(p_rn10ooh) = 311. 
               hstar(p_rn13ooh) = 311. 
               hstar(p_rn16ooh) = 311. 
               hstar(p_rn19ooh) = 311. 
               hstar(p_rn8ooh) = 311. 
               hstar(p_rn11ooh) = 311. 
               hstar(p_rn14ooh) = 311. 
               hstar(p_rn17ooh) = 311. 
               hstar(p_rn9ooh) = 311. 
               hstar(p_rn12ooh) = 311. 
               hstar(p_rn15ooh) = 311. 
               hstar(p_rn18ooh) = 311. 
               hstar(p_nrn6ooh) = 311. 
               hstar(p_nrn9ooh) = 311. 
               hstar(p_nrn12ooh) = 311. 
               hstar(p_ru14ooh) = 311. 
               hstar(p_ru12ooh) = 311. 
               hstar(p_ru10ooh) = 311. 
               hstar(p_nru14ooh) = 311. 
               hstar(p_nru12ooh) = 311. 
               hstar(p_ra13ooh) = 311. 
               hstar(p_ra16ooh) = 311. 
               hstar(p_ra19ooh) = 311. 
               hstar(p_rtn28ooh) = 311. 
               hstar(p_rtn26ooh) = 311. 
               hstar(p_nrtn28ooh) = 311. 
               hstar(p_rtn25ooh) = 311. 
               hstar(p_rtn24ooh) = 311. 
               hstar(p_rtn23ooh) = 311. 
               hstar(p_rtn14ooh) = 311. 
               hstar(p_rtn10ooh) = 311. 
               hstar(p_rcooh25) = 311. 
               hstar(p_rtx28ooh) = 311. 
               hstar(p_rtx24ooh) = 311. 
               hstar(p_rtx22ooh) = 311. 
               hstar(p_nrtx28ooh) = 311. 
               hstar(p_ra22ooh) = 311. 
               hstar(p_ra25ooh) = 311. 
               hstar(p_ch3no3) =  1.e3 
               hstar(p_c2h5no3) =  1.e3 
               hstar(p_hoc2h4no3) =  1.e3 
               hstar(p_rn10no3) =  1.e3 
               hstar(p_rn13no3) =  1.e3 
               hstar(p_rn19no3) =  1.e3 
               hstar(p_rn9no3) =  1.e3 
               hstar(p_rn12no3) =  1.e3 
               hstar(p_rn15no3) =  1.e3 
               hstar(p_rn18no3) =  1.e3 
               hstar(p_rn16no3) =  1.e3 
               hstar(p_ru14no3) =  1.e3 
               hstar(p_ra13no3) =  1.e3 
               hstar(p_ra16no3) =  1.e3 
               hstar(p_ra19no3) =  1.e3 
               hstar(p_rtn28no3) =  1.e3 
               hstar(p_rtn25no3) =  1.e3 
               hstar(p_rtx28no3) =  1.e3 
               hstar(p_rtx24no3) =  1.e3 
               hstar(p_rtx22no3) =  1.e3 
               hstar(p_rtn23no3) =  1.e3 
               hstar(p_ra22no3) =  1.e3 
               hstar(p_ra25no3) =  1.e3 
               hstar(p_ic3h7no3) =  1.e3
               hstar(p_ch3cho)    = 1.14E+1
               hstar(p_c2h5cho)    = 1.14E+1
               hstar(p_hoch2cho)    = 1.14E+1
               hstar(p_carb14)    = 1.14E+1  
               hstar(p_carb17)    = 1.14E+1       
               hstar(p_carb7)    = 1.14E+1        
               hstar(p_carb10)    = 1.14E+1       
               hstar(p_carb13)    = 1.14E+1       
               hstar(p_carb16)    = 1.14E+1    
               hstar(p_carb3)    = 1.14E+1        
               hstar(p_carb6)    = 1.14E+1        
               hstar(p_carb9)    = 1.14E+1  
               hstar(p_carb12)    = 1.14E+1       
               hstar(p_carb15)    = 1.14E+1     
               hstar(p_ccarb12)    = 1.14E+1   
               hstar(p_ucarb12)    = 1.14E+1  
               hstar(p_ucarb10)    = 1.14E+1  
               hstar(p_nucarb12)    = 1.14E+1  
               hstar(p_udcarb8)    = 1.14E+1   
               hstar(p_udcarb11)    = 1.14E+1    
               hstar(p_udcarb14)    = 1.14E+1   
               hstar(p_tncarb26)    = 1.14E+1  
               hstar(p_tncarb10)    = 1.14E+1   
               hstar(p_tncarb15)    = 1.14E+1  
               hstar(p_txcarb24)    = 1.14E+1   
               hstar(p_txcarb22)    = 1.14E+1  
               hstar(p_carb11a)    = 1.14E+1 
               hstar(p_tncarb12)    = 1.14E+1 
               hstar(p_tncarb11)    = 1.14E+1 
               hstar(p_udcarb17)    = 1.14E+1 
        
           else
              hstar(p_o3)   = 1.13E-2
              hstar(p_co)   = 8.20E-3
              hstar(p_h2o2) = 7.45E+4
              hstar(p_hcho) = 2.97E+3
              hstar(p_pan)  = 2.97
              hstar(p_paa)  = 473.
              hstar(p_onit) = 1.13
           end if
        hstar(p_no2)  = 6.40E-3
        hstar(p_no)   = 1.90E-3
        hstar(p_aco3) = 1.14E+1
        hstar(p_tpan) = 2.97E+0
        hstar(p_hono) = 3.47E+5
        hstar(p_no3)  = 1.50E+1
        hstar(p_hno4) = 2.00E+13
        hstar(p_ald) = 1.14E+1
        hstar(p_op1) = 2.21E+2
        hstar(p_op2) = 1.68E+6
        hstar(p_ket) = 3.30E+1
        hstar(p_gly) = 1.40E+6
        hstar(p_mgly) = 3.71E+3
        hstar(p_dcb)  = 1.40E+6
        hstar(p_so2) = 2.53E+5
        hstar(p_eth) = 2.00E-3
        hstar(p_hc3) = 1.42E-3
        hstar(p_hc5) = 1.13E-3
        hstar(p_hc8) = 1.42E-3
        hstar(p_olt) = 4.76E-3
        hstar(p_oli) = 1.35E-3
        hstar(p_tol) = 1.51E-1
        hstar(p_csl) = 4.00E+5
        hstar(p_xyl) = 1.45E-1
        hstar(p_iso) = 4.76E-3
        hstar(p_hno3) = 2.69E+13
        hstar(p_ora1) = 9.85E+6
        hstar(p_ora2) = 9.63E+5
        hstar(p_nh3)  = 1.04E+4
        hstar(p_n2o5) = 1.00E+10
        if(p_ol2 >= param_first_scalar) hstar(p_ol2) = 4.67E-3
        if(p_par >= param_first_scalar) hstar(p_par) = 1.13E-3  !wig, 1-May-2007: for CBMZ
        if(p_ch4 >= param_first_scalar) then 
           hstar(p_ch4) = 1.50E-3
           dhr(p_ch4)   = 0.
           f0(p_ch4)    = 0.
           dvj(p_ch4)   = 0.250
        end if
        if(p_co2 >= param_first_scalar) then 
           hstar(p_co2) = 1.86E-1
           dhr(p_co2)   = 1636.
           f0(p_co2)    = 0.
           dvj(p_co2)   = 0.151
        end if
!--------------------------------------------------
!  FOLLOWING FOR RACM
!--------------------------------------------------
        if( p_ete >= param_first_scalar ) then
           HSTAR(p_ETE )=4.67E-3
           HSTAR(p_API )=4.76E-3
           HSTAR(p_LIM )=4.76E-3
           HSTAR(p_DIEN)=4.76E-3
           HSTAR(p_MACR)=1.14E+1
           HSTAR(p_UDD )=1.40E+6
           HSTAR(p_HKET)=7.80E+3
           DHR(p_ETE )=    0.
           DHR(p_API )=    0.
           DHR(p_LIM )=    0.
           DHR(p_DIEN)=    0.
           DHR(p_MACR)= 6266.
           DHR(p_UDD )=    0.
           DHR(p_HKET)=    0.
           F0(p_ETE )=0.
           F0(p_API )=0.
           F0(p_LIM )=0.
           F0(p_DIEN)=0.
           F0(p_MACR)=0.
           F0(p_UDD )=0.
           F0(p_HKET)=0.
           DVJ(p_ETE )=0.189
           DVJ(p_API )=0.086
           DVJ(p_LIM )=0.086
           DVJ(p_DIEN)=0.136
           DVJ(p_MACR)=0.120
           DVJ(p_UDD )=0.092
           DVJ(p_HKET)=0.116
        endif
!--------------------------------------------------
!     -DH/R (for temperature correction)
!     [-DH/R]=K
!--------------------------------------------------
        if( chm_is_moz ) then
           dhr(p_o3)      = 2560.
           dhr(p_h2o2)    = 7379.
           dhr(p_hcho)    = 6425.
           dhr(p_ch3ooh)  = 5241.
           dhr(p_ch3oh)   = 4934.
           dhr(p_ch3cooh) = 6425.
           dhr(p_acet)    = 5300.
           dhr(p_paa)     = 5308.
           dhr(p_c3h6ooh) = 5653.
           dhr(p_pan)     = 0.
           dhr(p_mpan)    = 2560.
           dhr(p_c2h5oh)  = 6500.
           dhr(p_etooh)   = 5995.
           dhr(p_prooh)   = 5995.
           dhr(p_acetp)   = 5995.
           dhr(p_onit)    = 6000.
           dhr(p_onitr)   = 6485.
           dhr(p_acetol)  = 6425.
           dhr(p_glyald)  = 4630.
           dhr(p_hydrald) = 6000.
           dhr(p_alkooh)  = 5241.
           dhr(p_mekooh)  = 5241.
           dhr(p_tolooh)  = 5241.
           dhr(p_terpooh) = 5241.
           if (config_flags%chem_opt == T1_MOZCART_KPP ) then
             dhr(p_alknit)   = 0.
             dhr(p_ch3cn)    = 4100.
             dhr(p_eooh)     = 9700.
             dhr(p_hcn)      = 5000.
             dhr(p_honitr)   = 0.
             dhr(p_hpald)    = 0.
             dhr(p_iepox)    = 0.
             dhr(p_isopao2)  = 6600.
             dhr(p_isopbo2)  = 6600.
             dhr(p_isopnita) = 0.
             dhr(p_isopnitb) = 0.
             dhr(p_isopnooh) = 6000.
             dhr(p_nc4ch2oh) = 8600.
             dhr(p_nc4cho)   = 0.
             dhr(p_noa)      = 0.
             dhr(p_nterpooh) = 0.
             dhr(p_terpnit)  = 0.
           elseif( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
               config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             dhr(p_sulf) = 0.000E+00
           end if

           if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             dhr(p_cvasoaX) = 0.
             dhr(p_cvasoa1) = 6014.
             dhr(p_cvasoa2) = 6014.
             dhr(p_cvasoa3) = 6014.
             dhr(p_cvasoa4) = 6014.
             dhr(p_cvbsoaX) = 0.
             dhr(p_cvbsoa1) = 6014.
             dhr(p_cvbsoa2) = 6014.
             dhr(p_cvbsoa3) = 6014.
             dhr(p_cvbsoa4) = 6014.
           endif

     	else if( config_flags%chem_opt == crimech_kpp .or. &
          config_flags%chem_opt == cri_mosaic_8bin_aq_kpp .or. &
          config_flags%chem_opt == cri_mosaic_4bin_aq_kpp )   then
           dhr(p_o3)      = 2560.
           dhr(p_h2o2)    = 7379.
           dhr(p_hcho)    = 6425.
           dhr(p_ch3ooh)  = 5241.
           dhr(p_ch3oh)   = 4934.
           dhr(p_ch3cooh) = 6425.
           dhr(p_ket)    = 5300.
           dhr(p_paa)     = 5308.
            dhr(p_c2h5co3h)    = 5308.
            dhr(p_hoch2co3h)    = 5308.           
!           dhr(p_c3h6ooh) = 5653.
           dhr(p_pan)     = 0.
           dhr(p_mpan)    = 2560.
           dhr(p_ru12pan)    = 2560.
           dhr(p_rtn26pan)    = 2560.
           dhr(p_phan)    = 2560. 
           dhr(p_ppn)    = 2560.           
           dhr(p_c2h5oh)  = 6500.
!              dhr(p_c3h6ooh) = 220.
           dhr(p_c2h5ooh)   = 5995.
!           dhr(p_ic3h7ooh)   = 5995.
!           dhr(p_acetp)   = 5995.
!           dhr(p_onit)    = 6000.
!           dhr(p_onitr)   = 6485.
!           dhr(p_acetol)  = 6425.
!           dhr(p_glyald)  = 4630.
!           dhr(p_hydrald) = 6000.

               dhr(p_hcooh) =  5241.
               dhr(p_prooh) =  5241.
               dhr(p_hoc2h4ooh) =  5241.
               dhr(p_rn10ooh) =  5241.
               dhr(p_rn13ooh) =  5241.
               dhr(p_rn16ooh) =  5241.
               dhr(p_rn19ooh) =  5241.
               dhr(p_rn8ooh) =  5241.
               dhr(p_rn11ooh) =  5241.
               dhr(p_rn14ooh) =  5241.
               dhr(p_rn17ooh) =  5241.
               dhr(p_rn9ooh) =  5241.
               dhr(p_rn12ooh) =  5241.
               dhr(p_rn15ooh) =  5241.
               dhr(p_rn18ooh) =  5241.
               dhr(p_nrn6ooh) =  5241.
               dhr(p_nrn9ooh) =  5241.
               dhr(p_nrn12ooh) =  5241.
               dhr(p_ru14ooh) =  5241.
               dhr(p_ru12ooh) =  5241.
               dhr(p_ru10ooh) =  5241.
               dhr(p_nru14ooh) =  5241.
               dhr(p_nru12ooh) =  5241.
               dhr(p_ra13ooh) =  5241.
               dhr(p_ra16ooh) =  5241.
               dhr(p_ra19ooh) =  5241.
               dhr(p_rtn28ooh) =  5241.
               dhr(p_rtn26ooh) =  5241.
               dhr(p_nrtn28ooh) =  5241.
               dhr(p_rtn25ooh) =  5241.
               dhr(p_rtn24ooh) =  5241.
               dhr(p_rtn23ooh) =  5241.
               dhr(p_rtn14ooh) =  5241.
               dhr(p_rtn10ooh) =  5241.
               dhr(p_rcooh25) =  5241.
               dhr(p_rtx28ooh) =  5241.
               dhr(p_rtx24ooh) =  5241.
               dhr(p_rtx22ooh) =  5241.
               dhr(p_nrtx28ooh) =  5241.
               dhr(p_ra22ooh) =  5241.
               dhr(p_ra25ooh) =  5241.
               dhr(p_ch3no3) = 6000. 
               dhr(p_c2h5no3) = 6000. 
               dhr(p_hoc2h4no3) = 6000. 
               dhr(p_rn10no3) = 6000. 
               dhr(p_rn13no3) = 6000. 
               dhr(p_rn19no3) = 6000. 
               dhr(p_rn9no3) = 6000. 
               dhr(p_rn12no3) = 6000. 
               dhr(p_rn15no3) = 6000. 
               dhr(p_rn18no3) = 6000. 
               dhr(p_rn16no3) = 6000. 
               dhr(p_ru14no3) = 6000. 
               dhr(p_ra13no3) = 6000. 
               dhr(p_ra16no3) = 6000. 
               dhr(p_ra19no3) = 6000. 
               dhr(p_rtn28no3) = 6000. 
               dhr(p_rtn25no3) = 6000. 
               dhr(p_rtx28no3) = 6000. 
               dhr(p_rtx24no3) = 6000. 
               dhr(p_rtx22no3) = 6000. 
               dhr(p_rtn23no3) = 6000. 
               dhr(p_ra22no3) = 6000. 
               dhr(p_ra25no3) = 6000. 
               dhr(p_ic3h7no3) = 6000.
        	   dhr(p_ch3cho)    = 6266.
        	   dhr(p_c2h5cho)    = 6266.
        	   dhr(p_hoch2cho)    = 6266.   
               dhr(p_carb14)    = 6266. 
               dhr(p_carb17)    = 6266.      
               dhr(p_carb7)    = 6266.       
               dhr(p_carb10)    = 6266.      
               dhr(p_carb13)    = 6266.      
               dhr(p_carb16)    = 6266.   
               dhr(p_carb3)    = 6266.       
               dhr(p_carb6)    = 6266.       
               dhr(p_carb9)    = 6266. 
               dhr(p_carb12)    = 6266.      
               dhr(p_carb15)    = 6266.    
               dhr(p_ccarb12)    = 6266.  
               dhr(p_ucarb12)    = 6266. 
               dhr(p_ucarb10)    = 6266. 
               dhr(p_nucarb12)    = 6266. 
               dhr(p_udcarb8)    = 6266.  
               dhr(p_udcarb11)    = 6266.   
               dhr(p_udcarb14)    = 6266.  
               dhr(p_tncarb26)    = 6266. 
               dhr(p_tncarb10)    = 6266.  
               dhr(p_tncarb15)    = 6266. 
               dhr(p_txcarb24)    = 6266.  
               dhr(p_txcarb22)    = 6266. 
               dhr(p_carb11a)    = 6266.
               dhr(p_tncarb12)    = 6266.
               dhr(p_tncarb11)    = 6266.
               dhr(p_udcarb17)    = 6266.
        else
           dhr(p_o3)   = 2300.
           dhr(p_h2o2) = 6615.
           dhr(p_hcho) = 7190.
           dhr(p_pan)  = 5760.
           dhr(p_onit) = 5487.
           dhr(p_paa)  = 6170.
        end if
        dhr(p_no2)  = 2500.
        dhr(p_no)   = 1480.
        dhr(p_aco3) = 6266.
        dhr(p_tpan) = 5760.
        dhr(p_hono) = 3775.
        dhr(p_no3) = 0.
        dhr(p_hno4) = 0.
        dhr(p_co)  = 0.
        dhr(p_ald) = 6266.
        dhr(p_op1) = 5607.
        dhr(p_op2) = 10240.
        dhr(p_ket) = 5773.
        dhr(p_gly) = 0.
        dhr(p_mgly) = 7541.
        dhr(p_dcb) = 0.
        dhr(p_so2) = 5816.
        dhr(p_eth) = 0.
        dhr(p_hc3) = 0.
        dhr(p_hc5) = 0.
        dhr(p_hc8) = 0.
        dhr(p_olt) = 0.
        dhr(p_oli) = 0.
        dhr(p_tol) = 0.
        dhr(p_csl) = 0.
        dhr(p_xyl) = 0.
        dhr(p_iso) = 0.
        dhr(p_hno3) = 8684.
        dhr(p_ora1) = 5716.
        dhr(p_ora2) = 8374.
        dhr(p_nh3)  = 3660.
        dhr(p_n2o5) = 0.
        if(p_ol2 >= param_first_scalar) dhr(p_ol2) = 0.
        if(p_par >= param_first_scalar) dhr(p_par) = 0.   !wig, 1-May-2007: for CBMZ
!--------------------------------------------------
!     REACTIVITY FACTORS
!     [f0]=1
!--------------------------------------------------
        if( chm_is_moz ) then
           f0(p_hcho)    = small_value
           f0(p_ch3ooh)  = .1
           f0(p_ch3oh)   = small_value
           f0(p_ch3cooh) = small_value
           f0(p_acet)    = small_value
           f0(p_c3h6ooh) = .1
           f0(p_mpan)    = 1.
           f0(p_c2h5oh)  = small_value
           f0(p_etooh)   = .1
           f0(p_prooh)   = .1
           f0(p_acetp)   = .1
           f0(p_onit)    = .1
           f0(p_onitr)   = .1
           f0(p_acetol)  = small_value
           f0(p_glyald)  = small_value
           f0(p_hydrald) = small_value
           f0(p_alkooh)  = .1
           f0(p_mekooh)  = .1
           f0(p_tolooh)  = .1
           f0(p_terpooh) = .1
           if (config_flags%chem_opt == T1_MOZCART_KPP ) then
             f0(p_eooh)    = f0(p_hcho)
             f0(p_honitr)  = f0(p_h2o2)
           elseif( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
                   config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             f0(p_sulf) = 0.
           end if
           
           if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             f0(p_cvasoaX) = 0.
             f0(p_cvasoa1) = 0.
             f0(p_cvasoa2) = 0.
             f0(p_cvasoa3) = 0.
             f0(p_cvasoa4) = 0.
             f0(p_cvbsoaX) = 0.
             f0(p_cvbsoa1) = 0.
             f0(p_cvbsoa2) = 0.
             f0(p_cvbsoa3) = 0.
             f0(p_cvbsoa4) = 0.
           endif

     	else if( config_flags%chem_opt == crimech_kpp .or. &
          config_flags%chem_opt == cri_mosaic_8bin_aq_kpp .or. &
          config_flags%chem_opt == cri_mosaic_4bin_aq_kpp )   then


           f0(p_hcho)    = small_value
           f0(p_ch3ooh)  = .1
           f0(p_ch3oh)   = small_value
           f0(p_ch3cooh) = small_value
           f0(p_ket)    = small_value

!           f0(p_c3h6ooh) = .1

           f0(p_mpan)    = .1
           f0(p_ru12pan)    = .1
           f0(p_rtn26pan)    = .1
           f0(p_phan)    = .1
           f0(p_ppn)    = .1           
           f0(p_c2h5oh)  = small_value
!              f0(p_c3h6ooh) = .1
           f0(p_c2h5ooh)   = .1
!           f0(p_ic3h7ooh)   = .1
!           f0(p_acetp)   = .1
!           f0(p_onit)    = .1
!           f0(p_onitr)   = .1
!           f0(p_acetol)  = .1
!           f0(p_glyald)  = .1
!           f0(p_hydrald) = .1

               f0(p_hcooh) = .1
               f0(p_prooh) = .1
               f0(p_hoc2h4ooh) = .1
               f0(p_rn10ooh) = .1
               f0(p_rn13ooh) = .1
               f0(p_rn16ooh) = .1
               f0(p_rn19ooh) = .1
               f0(p_rn8ooh) = .1
               f0(p_rn11ooh) = .1
               f0(p_rn14ooh) = .1
               f0(p_rn17ooh) = .1
               f0(p_rn9ooh) = .1
               f0(p_rn12ooh) = .1
               f0(p_rn15ooh) = .1
               f0(p_rn18ooh) = .1
               f0(p_nrn6ooh) = .1
               f0(p_nrn9ooh) = .1
               f0(p_nrn12ooh) = .1
               f0(p_ru14ooh) = .1
               f0(p_ru12ooh) = .1
               f0(p_ru10ooh) = .1
               f0(p_nru14ooh) = .1
               f0(p_nru12ooh) = .1
               f0(p_ra13ooh) = .1
               f0(p_ra16ooh) = .1
               f0(p_ra19ooh) = .1
               f0(p_rtn28ooh) = .1
               f0(p_rtn26ooh) = .1
               f0(p_nrtn28ooh) = .1
               f0(p_rtn25ooh) = .1
               f0(p_rtn24ooh) = .1
               f0(p_rtn23ooh) = .1
               f0(p_rtn14ooh) = .1
               f0(p_rtn10ooh) = .1
               f0(p_rcooh25) = .1
               f0(p_rtx28ooh) = .1
               f0(p_rtx24ooh) = .1
               f0(p_rtx22ooh) = .1
               f0(p_nrtx28ooh) = .1
               f0(p_ra22ooh) = .1
               f0(p_ra25ooh) = .1
               f0(p_ch3no3) = .1
               f0(p_c2h5no3) = .1
               f0(p_hoc2h4no3) = .1
               f0(p_rn10no3) = .1
               f0(p_rn13no3) = .1
               f0(p_rn19no3) = .1
               f0(p_rn9no3) = .1
               f0(p_rn12no3) = .1
               f0(p_rn15no3) = .1
               f0(p_rn18no3) = .1
               f0(p_rn16no3) = .1
               f0(p_ru14no3) = .1
               f0(p_ra13no3) = .1
               f0(p_ra16no3) = .1
               f0(p_ra19no3) = .1
               f0(p_rtn28no3) = .1
               f0(p_rtn25no3) = .1
               f0(p_rtx28no3) = .1
               f0(p_rtx24no3) = .1
               f0(p_rtx22no3) = .1
               f0(p_rtn23no3) = .1
               f0(p_ra22no3) = .1
               f0(p_ra25no3) = .1
               f0(p_ic3h7no3) = .1
               f0(p_ch3cho)    = 0.
               f0(p_c2h5cho)    = 0.
               f0(p_hoch2cho)    = 0.
               f0(p_carb14)    = 0.
               f0(p_carb17)    = 0.     
               f0(p_carb7)    = 0.      
               f0(p_carb10)    = 0.     
               f0(p_carb13)    = 0.     
               f0(p_carb16)    = 0.  
               f0(p_carb3)    = 0.      
               f0(p_carb6)    = 0.      
               f0(p_carb9)    = 0.
               f0(p_carb12)    = 0.     
               f0(p_carb15)    = 0.   
               f0(p_ccarb12)    = 0. 
               f0(p_ucarb12)    = 0.
               f0(p_ucarb10)    = 0.
               f0(p_nucarb12)    = 0.
               f0(p_udcarb8)    = 0. 
               f0(p_udcarb11)    = 0.  
               f0(p_udcarb14)    = 0. 
               f0(p_tncarb26)    = 0.
               f0(p_tncarb10)    = 0. 
               f0(p_tncarb15)    = 0.
               f0(p_txcarb24)    = 0. 
               f0(p_txcarb22)    = 0.
               f0(p_carb11a)    = 0.
               f0(p_tncarb12)    = 0.
               f0(p_tncarb11)    = 0.
               f0(p_udcarb17)    = 0.
        
        else
           f0(p_hcho) = 0.
           f0(p_onit) = 0.
        end if
        f0(p_no2)  = 0.1
        f0(p_no)   = 0.
        f0(p_pan)  = 0.1
        f0(p_o3)   = 1.
        f0(p_aco3) = 1.
        f0(p_tpan) = 0.1
        f0(p_hono) = 0.1
        f0(p_no3)  = 1.
        f0(p_hno4) = 0.1
        f0(p_h2o2) = 1.
        f0(p_co)   = 0.
        f0(p_ald) = 0.
        f0(p_op1) = 0.1
        f0(p_op2) = 0.1
        f0(p_paa) = 0.1
        f0(p_ket) = 0.
        f0(p_gly) = 0.
        f0(p_mgly) = 0.
        f0(p_dcb)  = 0.
        f0(p_so2) = 0.
        f0(p_eth) = 0.
        f0(p_hc3) = 0.
        f0(p_hc5) = 0.
        f0(p_hc8) = 0.
        f0(p_olt) = 0.
        f0(p_oli) = 0.
        f0(p_tol) = 0.
        f0(p_csl) = 0.
        f0(p_xyl) = 0.
        f0(p_iso) = 0.
        f0(p_hno3) = 0.
        f0(p_ora1) = 0.
        f0(p_ora2) = 0.
        f0(p_nh3) = 0.
        f0(p_n2o5) = 1.
        if(p_ol2 >= param_first_scalar) f0(p_ol2) = 0.
        if(p_par >= param_first_scalar) f0(p_par) = 0.   !wig, 1-May-2007: for CBMZ
!--------------------------------------------------
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)
!--------------------------------------------------
        if( chm_is_moz ) then
           dvj(p_o3)      = 0.144
           dvj(p_h2o2)    = 0.1715
           dvj(p_hcho)    = 0.1825
           dvj(p_ch3ooh)  = 0.144
           dvj(p_ch3oh)   = 0.1767
           dvj(p_ch3cooh) = 0.129
           dvj(p_acet)    = 0.1312
           dvj(p_paa)     = 0.1147
           dvj(p_c3h6ooh) = 0.1042
           dvj(p_mpan)    = 0.0825
           dvj(p_c2h5oh)  = 0.1473
           dvj(p_etooh)   = 0.127
           dvj(p_prooh)   = 0.1146
           dvj(p_acetp)   = 0.1054
           dvj(p_onit)    = 0.0916
           dvj(p_onitr)   = 0.0824
           dvj(p_acetol)  = 0.116
           dvj(p_glyald)  = 0.129
           dvj(p_hydrald) = 0.0999
           dvj(p_alkooh)  = 0.098
           dvj(p_mekooh)  = 0.098
           dvj(p_tolooh)  = 0.084
           dvj(p_terpooh) = 0.073

           if( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
               config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             dvj(p_sulf) = 1.200E-01
           end if

           if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
             dvj(p_cvasoaX) = 0.120 ! ??
             dvj(p_cvasoa1) = 0.120 ! ??
             dvj(p_cvasoa2) = 0.120 ! ??
             dvj(p_cvasoa3) = 0.120 ! ??
             dvj(p_cvasoa4) = 0.120 ! ??
             dvj(p_cvbsoaX) = 0.120 ! ??
             dvj(p_cvbsoa1) = 0.120 ! ??
             dvj(p_cvbsoa2) = 0.120 ! ??
             dvj(p_cvbsoa3) = 0.120 ! ??
             dvj(p_cvbsoa4) = 0.120 ! ??
           endif

     else if( config_flags%chem_opt == crimech_kpp .or. &
          config_flags%chem_opt == cri_mosaic_8bin_aq_kpp .or. &
          config_flags%chem_opt == cri_mosaic_4bin_aq_kpp )   then
           dvj(p_o3)      = 0.144
           dvj(p_h2o2)    = 0.1715
           dvj(p_hcho)    = 0.1825
           dvj(p_ch3ooh)  = 0.144
           dvj(p_ch3oh)   = 0.1767
           dvj(p_ch3cooh) = 0.129
           dvj(p_ket)    = 0.1312
           dvj(p_paa)     = 0.1147
            dvj(p_c2h5co3h)    = 0.1147
            dvj(p_hoch2co3h)    = 0.1147           
!           dvj(p_c3h6ooh) = 0.1042
           dvj(p_mpan)    = 0.0825
            dvj(p_ru12pan)    = 0.0825
            dvj(p_rtn26pan)    = 0.0825
            dvj(p_phan)    = 0.0825
            dvj(p_ppn)    = 0.0825            
           dvj(p_c2h5oh)  = 0.1473

!              dvj(p_c3h6ooh) = 0.0916
           dvj(p_c2h5ooh)   = 0.091627
!           dvj(p_ic3h7ooh)   = 0.0916146
!           dvj(p_acetp)   = 0.1054
!           dvj(p_onit)    = 0.0916
!           dvj(p_onitr)   = 0.0824
!           dvj(p_acetol)  = 0.116
!           dvj(p_glyald)  = 0.129
!           dvj(p_hydrald) = 0.0999

               dvj(p_hcooh) = 0.098
               dvj(p_prooh) = 0.098
               dvj(p_hoc2h4ooh) = 0.098
               dvj(p_rn10ooh) = 0.098
               dvj(p_rn13ooh) = 0.098
               dvj(p_rn16ooh) = 0.098
               dvj(p_rn19ooh) = 0.098
               dvj(p_rn8ooh) = 0.098
               dvj(p_rn11ooh) = 0.098
               dvj(p_rn14ooh) = 0.098
               dvj(p_rn17ooh) = 0.098
               dvj(p_rn9ooh) = 0.098
               dvj(p_rn12ooh) = 0.098
               dvj(p_rn15ooh) = 0.098
               dvj(p_rn18ooh) = 0.098
               dvj(p_nrn6ooh) = 0.098
               dvj(p_nrn9ooh) = 0.098
               dvj(p_nrn12ooh) = 0.098
               dvj(p_ru14ooh) = 0.098
               dvj(p_ru12ooh) = 0.098
               dvj(p_ru10ooh) = 0.098
               dvj(p_nru14ooh) = 0.098
               dvj(p_nru12ooh) = 0.098
               dvj(p_ra13ooh) = 0.084
               dvj(p_ra16ooh) = 0.084
               dvj(p_ra19ooh) = 0.084
               dvj(p_rtn28ooh) = 0.073
               dvj(p_rtn26ooh) = 0.073
               dvj(p_nrtn28ooh) = 0.073
               dvj(p_rtn25ooh) = 0.073
               dvj(p_rtn24ooh) = 0.073
               dvj(p_rtn23ooh) = 0.073
               dvj(p_rtn14ooh) = 0.073
               dvj(p_rtn10ooh) = 0.073
               dvj(p_rcooh25) = 0.073
               dvj(p_rtx28ooh) = 0.073
               dvj(p_rtx24ooh) = 0.073
               dvj(p_rtx22ooh) = 0.073
               dvj(p_nrtx28ooh) = 0.073
               dvj(p_ra22ooh) = 0.084
               dvj(p_ra25ooh) = 0.084
               dvj(p_ch3no3) = 0.0916
               dvj(p_c2h5no3) = 0.0916
               dvj(p_hoc2h4no3) = 0.0916
               dvj(p_rn10no3) = 0.0916
               dvj(p_rn13no3) = 0.0916
               dvj(p_rn19no3) = 0.0916
               dvj(p_rn9no3) = 0.0916
               dvj(p_rn12no3) = 0.0916
               dvj(p_rn15no3) = 0.0916
               dvj(p_rn18no3) = 0.0916
               dvj(p_rn16no3) = 0.0916
               dvj(p_ru14no3) = 0.0916
               dvj(p_ra13no3) = 0.0916
               dvj(p_ra16no3) = 0.0916
               dvj(p_ra19no3) = 0.0916
               dvj(p_rtn28no3) = 0.0916
               dvj(p_rtn25no3) = 0.0916
               dvj(p_rtx28no3) = 0.0916
               dvj(p_rtx24no3) = 0.0916
               dvj(p_rtx22no3) = 0.0916
               dvj(p_rtn23no3) = 0.0916
               dvj(p_ra22no3) = 0.0916
               dvj(p_ra25no3) = 0.0916
               dvj(p_ic3h7no3) = 0.0916
               dvj(p_ch3cho)    = 0.151
               dvj(p_c2h5cho)    = 0.151
               dvj(p_hoch2cho)    = 0.151    
               dvj(p_carb14)    = 0.151
               dvj(p_carb17)    = 0.151     
               dvj(p_carb7)    = 0.151      
               dvj(p_carb10)    = 0.151     
               dvj(p_carb13)    = 0.151     
               dvj(p_carb16)    = 0.151  
               dvj(p_carb3)    = 0.151      
               dvj(p_carb6)    = 0.151      
               dvj(p_carb9)    = 0.151
               dvj(p_carb12)    = 0.151     
               dvj(p_carb15)    = 0.151   
               dvj(p_ccarb12)    = 0.151 
               dvj(p_ucarb12)    = 0.151
               dvj(p_ucarb10)    = 0.151
               dvj(p_nucarb12)    = 0.151
               dvj(p_udcarb8)    = 0.151 
               dvj(p_udcarb11)    = 0.151  
               dvj(p_udcarb14)    = 0.151 
               dvj(p_tncarb26)    = 0.151
               dvj(p_tncarb10)    = 0.151 
               dvj(p_tncarb15)    = 0.151
               dvj(p_txcarb24)    = 0.151 
               dvj(p_txcarb22)    = 0.151
               dvj(p_carb11a)    = 0.151
               dvj(p_tncarb12)    = 0.151
               dvj(p_tncarb11)    = 0.151
               dvj(p_udcarb17)    = 0.151
        
        else
           dvj(p_o3)   = 0.175
           dvj(p_h2o2) = 0.171
           dvj(p_hcho) = 0.183
           dvj(p_paa)  = 0.115
           dvj(p_onit) = 0.092
        end if
        dvj(p_no2)  = 0.147
        dvj(p_no)   = 0.183
        dvj(p_pan)  = 0.091
        dvj(p_aco3) = 0.115
        dvj(p_tpan) = 0.082
        dvj(p_hono) = 0.153
        dvj(p_no3)  = 0.127
        dvj(p_hno4) = 0.113
        dvj(p_co) = 0.189
        dvj(p_ald) = 0.151
        dvj(p_op1) = 0.144
        dvj(p_op2) = 0.127
        dvj(p_ket) = 0.118
        dvj(p_gly) = 0.131
        dvj(p_mgly) = 0.118
        dvj(p_dcb)  = 0.107
        dvj(p_so2) = 0.126
        dvj(p_eth) = 0.183
        dvj(p_hc3) = 0.151
        dvj(p_hc5) = 0.118
        dvj(p_hc8) = 0.094
        dvj(p_olt) = 0.154
        dvj(p_oli) = 0.121
        dvj(p_tol) = 0.104
        dvj(p_csl) = 0.096
        dvj(p_xyl) = 0.097
        dvj(p_iso) = 0.121
        dvj(p_hno3) = 0.126
        dvj(p_ora1) = 0.153
        dvj(p_ora2) = 0.124
        dvj(p_nh3) = 0.227
        dvj(p_n2o5) = 0.110
        dvj(p_ho) = 0.243
        dvj(p_ho2) = 0.174
        if(p_ol2 >= param_first_scalar) dvj(p_ol2) = 0.189
        if(p_par >= param_first_scalar) dvj(p_par) = 0.118   !wig, 1-May-2007: for CBMZ
! Dep constants for SAPRCNOV, produced automatically using create_WRF_dep.m script,05-Oct-2009 pablosaide@uiowa.edu
        if(p_methacro.ge.param_first_scalar)then
! HSTAR
          hstar(p_h2so4) = 2.600E+06
          hstar(p_ccho) = 1.700E+01
          hstar(p_rcho) = 4.200E+03
          hstar(p_etoh) = 2.200E+02
          hstar(p_cco_oh) = 4.100E+03
          hstar(p_rco_oh) = 4.100E+03
          hstar(p_bacl) = 2.700E+01
          hstar(p_bald) = 4.200E+01
          hstar(p_isoprod) = 1.300E-02
          hstar(p_methacro) = 6.500E+00
          hstar(p_prod2) = 2.000E+01
          hstar(p_dcb1) = 4.200E+03
          hstar(p_dcb2) = 4.200E+03
          hstar(p_dcb3) = 4.200E+03
          hstar(p_ethene) = 4.700E-03
          hstar(p_isoprene) = 1.300E-02
          hstar(p_c2h2) = 1.000E-03
          hstar(p_alk3) = 1.200E-03
          hstar(p_alk4) = 1.200E-03
          hstar(p_alk5) = 1.200E-03
          hstar(p_aro1) = 2.100E-01
          hstar(p_aro2) = 2.100E-01
          hstar(p_ole1) = 1.300E-02
          hstar(p_ole2) = 1.300E-02
          hstar(p_terp) = 1.200E-03
          hstar(p_sesq) = 1.200E-03
          hstar(p_rno3) = 2.000E+00
          hstar(p_nphe) = 2.100E-01
          hstar(p_phen) = 1.900E+03
          hstar(p_pan2) = 2.900E+00
          hstar(p_pbzn) = 2.900E+00
          hstar(p_ma_pan) = 2.900E+00
          hstar(p_cco_ooh) = 3.100E+02
          hstar(p_rco_o2) = 4.100E+03
          hstar(p_rco_ooh) = 3.100E+02
          hstar(p_xn) = 1.000E+00
          hstar(p_xc) = 1.000E+00
          hstar(p_c_o2) = 4.000E+03
          hstar(p_cooh) = 3.100E+02
          hstar(p_rooh) = 3.400E+02
          hstar(p_ro2_r) = 4.000E+03
          hstar(p_r2o2) = 4.000E+03
          hstar(p_ro2_n) = 4.000E+03
          hstar(p_cco_o2) = 1.700E+01
          hstar(p_bzco_o2) = 2.100E-01
          hstar(p_ma_rco3) = 6.500E+00
! DHR
          dhr(p_h2so4) = 0.000E+00
          dhr(p_ccho) = 5.000E+03
          dhr(p_rcho) = 0.000E+00
          dhr(p_etoh) = 5.200E+03
          dhr(p_cco_oh) = 6.300E+03
          dhr(p_rco_oh) = 6.300E+03
          dhr(p_bacl) = 0.000E+00
          dhr(p_bald) = 4.600E+03
          dhr(p_isoprod) = 0.000E+00
          dhr(p_methacro) = 0.000E+00
          dhr(p_prod2) = 5.000E+03
          dhr(p_dcb1) = 0.000E+00
          dhr(p_dcb2) = 0.000E+00
          dhr(p_dcb3) = 0.000E+00
          dhr(p_ethene) = 1.800E+03
          dhr(p_isoprene) = 0.000E+00
          dhr(p_c2h2) = 0.000E+00
          dhr(p_alk3) = 3.100E+03
          dhr(p_alk4) = 3.100E+03
          dhr(p_alk5) = 3.100E+03
          dhr(p_aro1) = 3.600E+03
          dhr(p_aro2) = 3.600E+03
          dhr(p_ole1) = 6.400E+03
          dhr(p_ole2) = 6.400E+03
          dhr(p_terp) = 3.100E+03
          dhr(p_sesq) = 3.100E+03
          dhr(p_rno3) = 2.000E+03
          dhr(p_nphe) = 3.600E+03
          dhr(p_phen) = 7.300E+03
          dhr(p_pan2) = 0.000E+00
          dhr(p_pbzn) = 5.900E+03
          dhr(p_ma_pan) = 0.000E+00
          dhr(p_cco_ooh) = 5.200E+03
          dhr(p_rco_o2) = 6.300E+03
          dhr(p_rco_ooh) = 5.200E+03
          dhr(p_xn) = 1.000E+00
          dhr(p_xc) = 1.000E+00
          dhr(p_c_o2) = 5.900E+03
          dhr(p_cooh) = 5.200E+03
          dhr(p_rooh) = 6.000E+03
          dhr(p_ro2_r) = 5.900E+03
          dhr(p_r2o2) = 5.900E+03
          dhr(p_ro2_n) = 5.900E+03
          dhr(p_cco_o2) = 5.000E+03
          dhr(p_bzco_o2) = 3.600E+03
          dhr(p_ma_rco3) = 0.000E+00
! F0
          f0(p_h2so4) = 0.000E+00
          f0(p_ccho) = 0.000E+00
          f0(p_rcho) = 0.000E+00
          f0(p_etoh) = 0.000E+00
          f0(p_cco_oh) = 0.000E+00
          f0(p_rco_oh) = 0.000E+00
          f0(p_bacl) = 0.000E+00
          f0(p_bald) = 0.000E+00
          f0(p_isoprod) = 0.000E+00
          f0(p_methacro) = 0.000E+00
          f0(p_prod2) = 0.000E+00
          f0(p_dcb1) = 0.000E+00
          f0(p_dcb2) = 0.000E+00
          f0(p_dcb3) = 0.000E+00
          f0(p_ethene) = 0.000E+00
          f0(p_isoprene) = 0.000E+00
          f0(p_c2h2) = 0.000E+00
          f0(p_alk3) = 0.000E+00
          f0(p_alk4) = 0.000E+00
          f0(p_alk5) = 0.000E+00
          f0(p_aro1) = 0.000E+00
          f0(p_aro2) = 0.000E+00
          f0(p_ole1) = 0.000E+00
          f0(p_ole2) = 0.000E+00
          f0(p_terp) = 0.000E+00
          f0(p_sesq) = 0.000E+00
          f0(p_rno3) = 0.000E+00
          f0(p_nphe) = 0.000E+00
          f0(p_phen) = 0.000E+00
          f0(p_pan2) = 0.000E+00
          f0(p_pbzn) = 0.000E+00
          f0(p_ma_pan) = 0.000E+00
          f0(p_cco_ooh) = 0.000E+00
          f0(p_rco_o2) = 0.000E+00
          f0(p_rco_ooh) = 0.000E+00
          f0(p_xn) = 0.000E+00
          f0(p_xc) = 0.000E+00
          f0(p_c_o2) = 0.000E+00
          f0(p_cooh) = 0.000E+00
          f0(p_rooh) = 0.000E+00
          f0(p_ro2_r) = 0.000E+00
          f0(p_r2o2) = 0.000E+00
          f0(p_ro2_n) = 0.000E+00
          f0(p_cco_o2) = 0.000E+00
          f0(p_bzco_o2) = 0.000E+00
          f0(p_ma_rco3) = 0.000E+00
! DVJ
          dvj(p_h2so4) = 1.200E-01
          dvj(p_ccho) = 1.420E-01
          dvj(p_rcho) = 1.420E-01
          dvj(p_etoh) = 1.280E-01
          dvj(p_cco_oh) = 1.000E-01
          dvj(p_rco_oh) = 1.000E-01
          dvj(p_bacl) = 1.000E-01
          dvj(p_bald) = 1.420E-01
          dvj(p_isoprod) = 1.000E-01
          dvj(p_methacro) = 1.420E-01
          dvj(p_prod2) = 9.800E-02
          dvj(p_dcb1) = 1.420E-01
          dvj(p_dcb2) = 1.420E-01
          dvj(p_dcb3) = 1.420E-01
          dvj(p_ethene) = 1.780E-01
          dvj(p_isoprene) = 1.000E-01
          dvj(p_c2h2) = 1.000E-01
          dvj(p_alk3) = 2.000E-01
          dvj(p_alk4) = 2.000E-01
          dvj(p_alk5) = 2.000E-01
          dvj(p_aro1) = 9.800E-02
          dvj(p_aro2) = 9.800E-02
          dvj(p_ole1) = 1.780E-01
          dvj(p_ole2) = 1.780E-01
          dvj(p_terp) = 1.000E-01
          dvj(p_sesq) = 1.000E-01
          dvj(p_rno3) = 1.390E-01
          dvj(p_nphe) = 1.000E-01
          dvj(p_phen) = 1.000E-01
          dvj(p_pan2) = 3.600E-02
          dvj(p_pbzn) = 3.600E-02
          dvj(p_ma_pan) = 3.600E-02
          dvj(p_cco_ooh) = 1.500E-01
          dvj(p_rco_o2) = 1.000E-01
          dvj(p_rco_ooh) = 1.500E-01
          dvj(p_xn) = 1.000E-01
          dvj(p_xc) = 1.000E-01
          dvj(p_c_o2) = 1.760E-01
          dvj(p_cooh) = 1.500E-01
          dvj(p_rooh) = 1.500E-01
          dvj(p_ro2_r) = 1.000E-01
          dvj(p_r2o2) = 1.760E-01
          dvj(p_ro2_n) = 1.000E-01
          dvj(p_cco_o2) = 1.000E-01
          dvj(p_bzco_o2) = 1.000E-01
          dvj(p_ma_rco3) = 1.420E-01
          endif

!hstar for SOA species assumed 2700 following CMU approach
     if(p_pcg1_b_c.gt.1)  hstar(p_pcg1_b_c) =2.7E+3
     if(p_pcg2_b_c.gt.1)  hstar(p_pcg2_b_c) =2.7E+3
     if(p_pcg3_b_c.gt.1)  hstar(p_pcg3_b_c)=2.7E+3
     if(p_pcg4_b_c.gt.1)  hstar(p_pcg4_b_c)=2.7E+3
     if(p_pcg5_b_c.gt.1)  hstar(p_pcg5_b_c)=2.7E+3
     if(p_pcg6_b_c.gt.1)  hstar(p_pcg6_b_c)=2.7E+3
     if(p_pcg7_b_c.gt.1)  hstar(p_pcg7_b_c)=2.7E+3
     if(p_pcg8_b_c.gt.1)  hstar(p_pcg8_b_c)=2.7E+3
     if(p_pcg9_b_c.gt.1)  hstar(p_pcg9_b_c)=2.7E+3
     if(p_opcg1_b_c.gt.1)  hstar(p_opcg1_b_c) =2.7E+3
     if(p_opcg2_b_c.gt.1)  hstar(p_opcg2_b_c) =2.7E+3
     if(p_opcg3_b_c.gt.1)  hstar(p_opcg3_b_c)=2.7E+3
     if(p_opcg4_b_c.gt.1)  hstar(p_opcg4_b_c)=2.7E+3
     if(p_opcg5_b_c.gt.1)  hstar(p_opcg5_b_c)=2.7E+3
     if(p_opcg6_b_c.gt.1)  hstar(p_opcg6_b_c)=2.7E+3
     if(p_opcg7_b_c.gt.1)  hstar(p_opcg7_b_c)=2.7E+3
     if(p_opcg8_b_c.gt.1)  hstar(p_opcg8_b_c)=2.7E+3
     if(p_pcg1_b_o.gt.1)  hstar(p_pcg1_b_o) =2.7E+3
     if(p_pcg2_b_o.gt.1)  hstar(p_pcg2_b_o) =2.7E+3
     if(p_pcg3_b_o.gt.1)  hstar(p_pcg3_b_o)=2.7E+3
     if(p_pcg4_b_o.gt.1)  hstar(p_pcg4_b_o)=2.7E+3
     if(p_pcg5_b_o.gt.1)  hstar(p_pcg5_b_o)=2.7E+3
     if(p_pcg6_b_o.gt.1)  hstar(p_pcg6_b_o)=2.7E+3
     if(p_pcg7_b_o.gt.1)  hstar(p_pcg7_b_o)=2.7E+3
     if(p_pcg8_b_o.gt.1)  hstar(p_pcg8_b_o)=2.7E+3
     if(p_pcg9_b_o.gt.1)  hstar(p_pcg9_b_o)=2.7E+3
     if(p_opcg1_b_o.gt.1)  hstar(p_opcg1_b_o) =2.7E+3
     if(p_opcg2_b_o.gt.1)  hstar(p_opcg2_b_o) =2.7E+3
     if(p_opcg3_b_o.gt.1)  hstar(p_opcg3_b_o)=2.7E+3
     if(p_opcg4_b_o.gt.1)  hstar(p_opcg4_b_o)=2.7E+3
     if(p_opcg5_b_o.gt.1)  hstar(p_opcg5_b_o)=2.7E+3
     if(p_opcg6_b_o.gt.1)  hstar(p_opcg6_b_o)=2.7E+3
     if(p_opcg7_b_o.gt.1)  hstar(p_opcg7_b_o)=2.7E+3
     if(p_opcg8_b_o.gt.1)  hstar(p_opcg8_b_o)=2.7E+3
     if(p_pcg1_f_c.gt.1)  hstar(p_pcg1_f_c) =2.7E+3
     if(p_pcg2_f_c.gt.1)  hstar(p_pcg2_f_c) =2.7E+3
     if(p_pcg3_f_c.gt.1)  hstar(p_pcg3_f_c)=2.7E+3
     if(p_pcg4_f_c.gt.1)  hstar(p_pcg4_f_c)=2.7E+3
     if(p_pcg5_f_c.gt.1)  hstar(p_pcg5_f_c)=2.7E+3
     if(p_pcg6_f_c.gt.1)  hstar(p_pcg6_f_c)=2.7E+3
     if(p_pcg7_f_c.gt.1)  hstar(p_pcg7_f_c)=2.7E+3
     if(p_pcg8_f_c.gt.1)  hstar(p_pcg8_f_c)=2.7E+3
     if(p_pcg9_f_c.gt.1)  hstar(p_pcg9_f_c)=2.7E+3
     if(p_opcg1_f_c.gt.1)  hstar(p_opcg1_f_c) =2.7E+3
     if(p_opcg2_f_c.gt.1)  hstar(p_opcg2_f_c) =2.7E+3
     if(p_opcg3_f_c.gt.1)  hstar(p_opcg3_f_c)=2.7E+3
     if(p_opcg4_f_c.gt.1)  hstar(p_opcg4_f_c)=2.7E+3
     if(p_opcg5_f_c.gt.1)  hstar(p_opcg5_f_c)=2.7E+3
     if(p_opcg6_f_c.gt.1)  hstar(p_opcg6_f_c)=2.7E+3
     if(p_opcg7_f_c.gt.1)  hstar(p_opcg7_f_c)=2.7E+3
     if(p_opcg8_f_c.gt.1)  hstar(p_opcg8_f_c)=2.7E+3
     if(p_pcg1_f_o.gt.1)  hstar(p_pcg1_f_o) =2.7E+3
     if(p_pcg2_f_o.gt.1)  hstar(p_pcg2_f_o) =2.7E+3
     if(p_pcg3_f_o.gt.1)  hstar(p_pcg3_f_o)=2.7E+3
     if(p_pcg4_f_o.gt.1)  hstar(p_pcg4_f_o)=2.7E+3
     if(p_pcg5_f_o.gt.1)  hstar(p_pcg5_f_o)=2.7E+3
     if(p_pcg6_f_o.gt.1)  hstar(p_pcg6_f_o)=2.7E+3
     if(p_pcg7_f_o.gt.1)  hstar(p_pcg7_f_o)=2.7E+3
     if(p_pcg8_f_o.gt.1)  hstar(p_pcg8_f_o)=2.7E+3
     if(p_pcg9_f_o.gt.1)  hstar(p_pcg9_f_o)=2.7E+3
     if(p_opcg1_f_o.gt.1)  hstar(p_opcg1_f_o) =2.7E+3
     if(p_opcg2_f_o.gt.1)  hstar(p_opcg2_f_o) =2.7E+3
     if(p_opcg3_f_o.gt.1)  hstar(p_opcg3_f_o)=2.7E+3
     if(p_opcg4_f_o.gt.1)  hstar(p_opcg4_f_o)=2.7E+3
     if(p_opcg5_f_o.gt.1)  hstar(p_opcg5_f_o)=2.7E+3
     if(p_opcg6_f_o.gt.1)  hstar(p_opcg6_f_o)=2.7E+3
     if(p_opcg7_f_o.gt.1)  hstar(p_opcg7_f_o)=2.7E+3
     if(p_opcg8_f_o.gt.1)  hstar(p_opcg8_f_o)=2.7E+3
     if(p_ant1_c.gt.1)     hstar(p_ant1_c)=2.7E+3
     if(p_ant2_c.gt.1)     hstar(p_ant2_c)=2.7E+3
     if(p_ant3_c.gt.1)     hstar(p_ant3_c)=2.7E+3
     if(p_ant4_c.gt.1)     hstar(p_ant4_c)=2.7E+3
     if(p_ant1_o.gt.1)     hstar(p_ant1_o)=2.7E+3
     if(p_ant2_o.gt.1)     hstar(p_ant2_o)=2.7E+3
     if(p_ant3_o.gt.1)     hstar(p_ant3_o)=2.7E+3
     if(p_ant4_o.gt.1)     hstar(p_ant4_o)=2.7E+3
     if(p_biog1_c.gt.1)    hstar(p_biog1_c)=2.7E+3
     if(p_biog2_c.gt.1)    hstar(p_biog2_c)=2.7E+3
     if(p_biog3_c.gt.1)    hstar(p_biog3_c)=2.7E+3
     if(p_biog4_c.gt.1)    hstar(p_biog4_c)=2.7E+3
     if(p_biog1_o.gt.1)    hstar(p_biog1_o)=2.7E+3
     if(p_biog2_o.gt.1)    hstar(p_biog2_o)=2.7E+3
     if(p_biog3_o.gt.1)    hstar(p_biog3_o)=2.7E+3
     if(p_biog4_o.gt.1)    hstar(p_biog4_o)=2.7E+3

     if(p_pcg1_b_c.gt.1)  dhr(p_pcg1_b_c) =6.4E+3
     if(p_pcg2_b_c.gt.1)  dhr(p_pcg2_b_c) =6.4E+3
     if(p_pcg3_b_c.gt.1)  dhr(p_pcg3_b_c)=6.4E+3
     if(p_pcg4_b_c.gt.1)  dhr(p_pcg4_b_c)=6.4E+3
     if(p_pcg5_b_c.gt.1)  dhr(p_pcg5_b_c)=6.4E+3
     if(p_pcg6_b_c.gt.1)  dhr(p_pcg6_b_c)=6.4E+3
     if(p_pcg7_b_c.gt.1)  dhr(p_pcg7_b_c)=6.4E+3
     if(p_pcg8_b_c.gt.1)  dhr(p_pcg8_b_c)=6.4E+3
     if(p_pcg9_b_c.gt.1)  dhr(p_pcg9_b_c)=6.4E+3
     if(p_opcg1_b_c.gt.1)  dhr(p_opcg1_b_c) =6.4E+3
     if(p_opcg2_b_c.gt.1)  dhr(p_opcg2_b_c) =6.4E+3
     if(p_opcg3_b_c.gt.1)  dhr(p_opcg3_b_c)=6.4E+3
     if(p_opcg4_b_c.gt.1)  dhr(p_opcg4_b_c)=6.4E+3
     if(p_opcg5_b_c.gt.1)  dhr(p_opcg5_b_c)=6.4E+3
     if(p_opcg6_b_c.gt.1)  dhr(p_opcg6_b_c)=6.4E+3
     if(p_opcg7_b_c.gt.1)  dhr(p_opcg7_b_c)=6.4E+3
     if(p_opcg8_b_c.gt.1)  dhr(p_opcg8_b_c)=6.4E+3
     if(p_pcg1_b_o.gt.1)  dhr(p_pcg1_b_o) =6.4E+3
     if(p_pcg2_b_o.gt.1)  dhr(p_pcg2_b_o) =6.4E+3
     if(p_pcg3_b_o.gt.1)  dhr(p_pcg3_b_o)=6.4E+3
     if(p_pcg4_b_o.gt.1)  dhr(p_pcg4_b_o)=6.4E+3
     if(p_pcg5_b_o.gt.1)  dhr(p_pcg5_b_o)=6.4E+3
     if(p_pcg6_b_o.gt.1)  dhr(p_pcg6_b_o)=6.4E+3
     if(p_pcg7_b_o.gt.1)  dhr(p_pcg7_b_o)=6.4E+3
     if(p_pcg8_b_o.gt.1)  dhr(p_pcg8_b_o)=6.4E+3
     if(p_pcg9_b_o.gt.1)  dhr(p_pcg9_b_o)=6.4E+3
     if(p_opcg1_b_o.gt.1)  dhr(p_opcg1_b_o) =6.4E+3
     if(p_opcg2_b_o.gt.1)  dhr(p_opcg2_b_o) =6.4E+3
     if(p_opcg3_b_o.gt.1)  dhr(p_opcg3_b_o)=6.4E+3
     if(p_opcg4_b_o.gt.1)  dhr(p_opcg4_b_o)=6.4E+3
     if(p_opcg5_b_o.gt.1)  dhr(p_opcg5_b_o)=6.4E+3
     if(p_opcg6_b_o.gt.1)  dhr(p_opcg6_b_o)=6.4E+3
     if(p_opcg7_b_o.gt.1)  dhr(p_opcg7_b_o)=6.4E+3
     if(p_opcg8_b_o.gt.1)  dhr(p_opcg8_b_o)=6.4E+3
     if(p_pcg1_f_c.gt.1)  dhr(p_pcg1_f_c) =6.4E+3
     if(p_pcg2_f_c.gt.1)  dhr(p_pcg2_f_c) =6.4E+3
     if(p_pcg3_f_c.gt.1)  dhr(p_pcg3_f_c)=6.4E+3
     if(p_pcg4_f_c.gt.1)  dhr(p_pcg4_f_c)=6.4E+3
     if(p_pcg5_f_c.gt.1)  dhr(p_pcg5_f_c)=6.4E+3
     if(p_pcg6_f_c.gt.1)  dhr(p_pcg6_f_c)=6.4E+3
     if(p_pcg7_f_c.gt.1)  dhr(p_pcg7_f_c)=6.4E+3
     if(p_pcg8_f_c.gt.1)  dhr(p_pcg8_f_c)=6.4E+3
     if(p_pcg9_f_c.gt.1)  dhr(p_pcg9_f_c)=6.4E+3
     if(p_opcg1_f_c.gt.1)  dhr(p_opcg1_f_c) =6.4E+3
     if(p_opcg2_f_c.gt.1)  dhr(p_opcg2_f_c) =6.4E+3
     if(p_opcg3_f_c.gt.1)  dhr(p_opcg3_f_c)=6.4E+3
     if(p_opcg4_f_c.gt.1)  dhr(p_opcg4_f_c)=6.4E+3
     if(p_opcg5_f_c.gt.1)  dhr(p_opcg5_f_c)=6.4E+3
     if(p_opcg6_f_c.gt.1)  dhr(p_opcg6_f_c)=6.4E+3
     if(p_opcg7_f_c.gt.1)  dhr(p_opcg7_f_c)=6.4E+3
     if(p_opcg8_f_c.gt.1)  dhr(p_opcg8_f_c)=6.4E+3
     if(p_pcg1_f_o.gt.1)  dhr(p_pcg1_f_o) =6.4E+3
     if(p_pcg2_f_o.gt.1)  dhr(p_pcg2_f_o) =6.4E+3
     if(p_pcg3_f_o.gt.1)  dhr(p_pcg3_f_o)=6.4E+3
     if(p_pcg4_f_o.gt.1)  dhr(p_pcg4_f_o)=6.4E+3
     if(p_pcg5_f_o.gt.1)  dhr(p_pcg5_f_o)=6.4E+3
     if(p_pcg6_f_o.gt.1)  dhr(p_pcg6_f_o)=6.4E+3
     if(p_pcg7_f_o.gt.1)  dhr(p_pcg7_f_o)=6.4E+3
     if(p_pcg8_f_o.gt.1)  dhr(p_pcg8_f_o)=6.4E+3
     if(p_pcg9_f_o.gt.1)  dhr(p_pcg9_f_o)=6.4E+3
     if(p_opcg1_f_o.gt.1)  dhr(p_opcg1_f_o) =6.4E+3
     if(p_opcg2_f_o.gt.1)  dhr(p_opcg2_f_o) =6.4E+3
     if(p_opcg3_f_o.gt.1)  dhr(p_opcg3_f_o)=6.4E+3
     if(p_opcg4_f_o.gt.1)  dhr(p_opcg4_f_o)=6.4E+3
     if(p_opcg5_f_o.gt.1)  dhr(p_opcg5_f_o)=6.4E+3
     if(p_opcg6_f_o.gt.1)  dhr(p_opcg6_f_o)=6.4E+3
     if(p_opcg7_f_o.gt.1)  dhr(p_opcg7_f_o)=6.4E+3
     if(p_opcg8_f_o.gt.1)  dhr(p_opcg8_f_o)=6.4E+3
     if(p_ant1_c.gt.1)     dhr(p_ant1_c)=6.4E+3
     if(p_ant2_c.gt.1)     dhr(p_ant2_c)=6.4E+3
     if(p_ant3_c.gt.1)     dhr(p_ant3_c)=6.4E+3
     if(p_ant4_c.gt.1)     dhr(p_ant4_c)=6.4E+3
     if(p_ant1_o.gt.1)     dhr(p_ant1_o)=6.4E+3
     if(p_ant2_o.gt.1)     dhr(p_ant2_o)=6.4E+3
     if(p_ant3_o.gt.1)     dhr(p_ant3_o)=6.4E+3
     if(p_ant4_o.gt.1)     dhr(p_ant4_o)=6.4E+3
     if(p_biog1_c.gt.1)    dhr(p_biog1_c)=6.4E+3
     if(p_biog2_c.gt.1)    dhr(p_biog2_c)=6.4E+3
     if(p_biog3_c.gt.1)    dhr(p_biog3_c)=6.4E+3
     if(p_biog4_c.gt.1)    dhr(p_biog4_c)=6.4E+3
     if(p_biog1_o.gt.1)    dhr(p_biog1_o)=6.4E+3
     if(p_biog2_o.gt.1)    dhr(p_biog2_o)=6.4E+3
     if(p_biog3_o.gt.1)    dhr(p_biog3_o)=6.4E+3
     if(p_biog4_o.gt.1)    dhr(p_biog4_o)=6.4E+3

!Assume reactivity =0 for SOA species

     if(p_pcg1_b_c.gt.1)  f0(p_pcg1_b_c) =0.0000
     if(p_pcg2_b_c.gt.1)  f0(p_pcg2_b_c) =0.0000
     if(p_pcg3_b_c.gt.1)  f0(p_pcg3_b_c)=0.0000
     if(p_pcg4_b_c.gt.1)  f0(p_pcg4_b_c)=0.0000
     if(p_pcg5_b_c.gt.1)  f0(p_pcg5_b_c)=0.0000
     if(p_pcg6_b_c.gt.1)  f0(p_pcg6_b_c)=0.0000
     if(p_pcg7_b_c.gt.1)  f0(p_pcg7_b_c)=0.0000
     if(p_pcg8_b_c.gt.1)  f0(p_pcg8_b_c)=0.0000
     if(p_pcg9_b_c.gt.1)  f0(p_pcg9_b_c)=0.0000
     if(p_opcg1_b_c.gt.1)  f0(p_opcg1_b_c) =0.0000
     if(p_opcg2_b_c.gt.1)  f0(p_opcg2_b_c) =0.0000
     if(p_opcg3_b_c.gt.1)  f0(p_opcg3_b_c)=0.0000
     if(p_opcg4_b_c.gt.1)  f0(p_opcg4_b_c)=0.0000
     if(p_opcg5_b_c.gt.1)  f0(p_opcg5_b_c)=0.0000
     if(p_opcg6_b_c.gt.1)  f0(p_opcg6_b_c)=0.0000
     if(p_opcg7_b_c.gt.1)  f0(p_opcg7_b_c)=0.0000
     if(p_opcg8_b_c.gt.1)  f0(p_opcg8_b_c)=0.0000
     if(p_pcg1_b_o.gt.1)  f0(p_pcg1_b_o) =0.0000
     if(p_pcg2_b_o.gt.1)  f0(p_pcg2_b_o) =0.0000
     if(p_pcg3_b_o.gt.1)  f0(p_pcg3_b_o)=0.0000
     if(p_pcg4_b_o.gt.1)  f0(p_pcg4_b_o)=0.0000
     if(p_pcg5_b_o.gt.1)  f0(p_pcg5_b_o)=0.0000
     if(p_pcg6_b_o.gt.1)  f0(p_pcg6_b_o)=0.0000
     if(p_pcg7_b_o.gt.1)  f0(p_pcg7_b_o)=0.0000
     if(p_pcg8_b_o.gt.1)  f0(p_pcg8_b_o)=0.0000
     if(p_pcg9_b_o.gt.1)  f0(p_pcg9_b_o)=0.0000
     if(p_opcg1_b_o.gt.1)  f0(p_opcg1_b_o) =0.0000
     if(p_opcg2_b_o.gt.1)  f0(p_opcg2_b_o) =0.0000
     if(p_opcg3_b_o.gt.1)  f0(p_opcg3_b_o)=0.0000
     if(p_opcg4_b_o.gt.1)  f0(p_opcg4_b_o)=0.0000
     if(p_opcg5_b_o.gt.1)  f0(p_opcg5_b_o)=0.0000
     if(p_opcg6_b_o.gt.1)  f0(p_opcg6_b_o)=0.0000
     if(p_opcg7_b_o.gt.1)  f0(p_opcg7_b_o)=0.0000
     if(p_opcg8_b_o.gt.1)  f0(p_opcg8_b_o)=0.0000
     if(p_pcg1_f_c.gt.1)  f0(p_pcg1_f_c) =0.0000
     if(p_pcg2_f_c.gt.1)  f0(p_pcg2_f_c) =0.0000
     if(p_pcg3_f_c.gt.1)  f0(p_pcg3_f_c)=0.0000
     if(p_pcg4_f_c.gt.1)  f0(p_pcg4_f_c)=0.0000
     if(p_pcg5_f_c.gt.1)  f0(p_pcg5_f_c)=0.0000
     if(p_pcg6_f_c.gt.1)  f0(p_pcg6_f_c)=0.0000
     if(p_pcg7_f_c.gt.1)  f0(p_pcg7_f_c)=0.0000
     if(p_pcg8_f_c.gt.1)  f0(p_pcg8_f_c)=0.0000
     if(p_pcg9_f_c.gt.1)  f0(p_pcg9_f_c)=0.0000
     if(p_opcg1_f_c.gt.1)  f0(p_opcg1_f_c) =0.0000
     if(p_opcg2_f_c.gt.1)  f0(p_opcg2_f_c) =0.0000
     if(p_opcg3_f_c.gt.1)  f0(p_opcg3_f_c)=0.0000
     if(p_opcg4_f_c.gt.1)  f0(p_opcg4_f_c)=0.0000
     if(p_opcg5_f_c.gt.1)  f0(p_opcg5_f_c)=0.0000
     if(p_opcg6_f_c.gt.1)  f0(p_opcg6_f_c)=0.0000
     if(p_opcg7_f_c.gt.1)  f0(p_opcg7_f_c)=0.0000
     if(p_opcg8_f_c.gt.1)  f0(p_opcg8_f_c)=0.0000
     if(p_pcg1_f_o.gt.1)  f0(p_pcg1_f_o) =0.0000
     if(p_pcg2_f_o.gt.1)  f0(p_pcg2_f_o) =0.0000
     if(p_pcg3_f_o.gt.1)  f0(p_pcg3_f_o)=0.0000
     if(p_pcg4_f_o.gt.1)  f0(p_pcg4_f_o)=0.0000
     if(p_pcg5_f_o.gt.1)  f0(p_pcg5_f_o)=0.0000
     if(p_pcg6_f_o.gt.1)  f0(p_pcg6_f_o)=0.0000
     if(p_pcg7_f_o.gt.1)  f0(p_pcg7_f_o)=0.0000
     if(p_pcg8_f_o.gt.1)  f0(p_pcg8_f_o)=0.0000
     if(p_pcg9_f_o.gt.1)  f0(p_pcg9_f_o)=0.0000
     if(p_opcg1_f_o.gt.1)  f0(p_opcg1_f_o) =0.0000
     if(p_opcg2_f_o.gt.1)  f0(p_opcg2_f_o) =0.0000
     if(p_opcg3_f_o.gt.1)  f0(p_opcg3_f_o)=0.0000
     if(p_opcg4_f_o.gt.1)  f0(p_opcg4_f_o)=0.0000
     if(p_opcg5_f_o.gt.1)  f0(p_opcg5_f_o)=0.0000
     if(p_opcg6_f_o.gt.1)  f0(p_opcg6_f_o)=0.0000
     if(p_opcg7_f_o.gt.1)  f0(p_opcg7_f_o)=0.0000
     if(p_opcg8_f_o.gt.1)  f0(p_opcg8_f_o)=0.0000
     if(p_ant1_c.gt.1)     f0(p_ant1_c)=0.0000
     if(p_ant2_c.gt.1)     f0(p_ant2_c)=0.0000
     if(p_ant3_c.gt.1)     f0(p_ant3_c)=0.0000
     if(p_ant4_c.gt.1)     f0(p_ant4_c)=0.0000
     if(p_ant1_o.gt.1)     f0(p_ant1_o)=0.0000
     if(p_ant2_o.gt.1)     f0(p_ant2_o)=0.0000
     if(p_ant3_o.gt.1)     f0(p_ant3_o)=0.0000
     if(p_ant4_o.gt.1)     f0(p_ant4_o)=0.0000
     if(p_biog1_c.gt.1)    f0(p_biog1_c)=0.0000
     if(p_biog2_c.gt.1)    f0(p_biog2_c)=0.0000
     if(p_biog3_c.gt.1)    f0(p_biog3_c)=0.0000
     if(p_biog4_c.gt.1)    f0(p_biog4_c)=0.0000
     if(p_biog1_o.gt.1)    f0(p_biog1_o)=0.0000
     if(p_biog2_o.gt.1)    f0(p_biog2_o)=0.0000
     if(p_biog3_o.gt.1)    f0(p_biog3_o)=0.0000
     if(p_biog4_o.gt.1)    f0(p_biog4_o)=0.0000


!Diffusion coefficients of SOA species
     if(p_pcg1_b_c.gt.1)  dvj(p_pcg1_b_c) =1.0E-1
     if(p_pcg2_b_c.gt.1)  dvj(p_pcg2_b_c) =1.0E-1
     if(p_pcg3_b_c.gt.1)  dvj(p_pcg3_b_c)=1.0E-1
     if(p_pcg4_b_c.gt.1)  dvj(p_pcg4_b_c)=1.0E-1
     if(p_pcg5_b_c.gt.1)  dvj(p_pcg5_b_c)=1.0E-1
     if(p_pcg6_b_c.gt.1)  dvj(p_pcg6_b_c)=1.0E-1
     if(p_pcg7_b_c.gt.1)  dvj(p_pcg7_b_c)=1.0E-1
     if(p_pcg8_b_c.gt.1)  dvj(p_pcg8_b_c)=1.0E-1
     if(p_pcg9_b_c.gt.1)  dvj(p_pcg9_b_c)=1.0E-1
     if(p_opcg1_b_c.gt.1)  dvj(p_opcg1_b_c) =1.0E-1
     if(p_opcg2_b_c.gt.1)  dvj(p_opcg2_b_c) =1.0E-1
     if(p_opcg3_b_c.gt.1)  dvj(p_opcg3_b_c)=1.0E-1
     if(p_opcg4_b_c.gt.1)  dvj(p_opcg4_b_c)=1.0E-1
     if(p_opcg5_b_c.gt.1)  dvj(p_opcg5_b_c)=1.0E-1
     if(p_opcg6_b_c.gt.1)  dvj(p_opcg6_b_c)=1.0E-1
     if(p_opcg7_b_c.gt.1)  dvj(p_opcg7_b_c)=1.0E-1
     if(p_opcg8_b_c.gt.1)  dvj(p_opcg8_b_c)=1.0E-1
     if(p_pcg1_b_o.gt.1)  dvj(p_pcg1_b_o) =1.0E-1
     if(p_pcg2_b_o.gt.1)  dvj(p_pcg2_b_o) =1.0E-1
     if(p_pcg3_b_o.gt.1)  dvj(p_pcg3_b_o)=1.0E-1
     if(p_pcg4_b_o.gt.1)  dvj(p_pcg4_b_o)=1.0E-1
     if(p_pcg5_b_o.gt.1)  dvj(p_pcg5_b_o)=1.0E-1
     if(p_pcg6_b_o.gt.1)  dvj(p_pcg6_b_o)=1.0E-1
     if(p_pcg7_b_o.gt.1)  dvj(p_pcg7_b_o)=1.0E-1
     if(p_pcg8_b_o.gt.1)  dvj(p_pcg8_b_o)=1.0E-1
     if(p_pcg9_b_o.gt.1)  dvj(p_pcg9_b_o)=1.0E-1
     if(p_opcg1_b_o.gt.1)  dvj(p_opcg1_b_o) =1.0E-1
     if(p_opcg2_b_o.gt.1)  dvj(p_opcg2_b_o) =1.0E-1
     if(p_opcg3_b_o.gt.1)  dvj(p_opcg3_b_o)=1.0E-1
     if(p_opcg4_b_o.gt.1)  dvj(p_opcg4_b_o)=1.0E-1
     if(p_opcg5_b_o.gt.1)  dvj(p_opcg5_b_o)=1.0E-1
     if(p_opcg6_b_o.gt.1)  dvj(p_opcg6_b_o)=1.0E-1
     if(p_opcg7_b_o.gt.1)  dvj(p_opcg7_b_o)=1.0E-1
     if(p_opcg8_b_o.gt.1)  dvj(p_opcg8_b_o)=1.0E-1
     if(p_pcg1_f_c.gt.1)  dvj(p_pcg1_f_c) =1.0E-1
     if(p_pcg2_f_c.gt.1)  dvj(p_pcg2_f_c) =1.0E-1
     if(p_pcg3_f_c.gt.1)  dvj(p_pcg3_f_c)=1.0E-1
     if(p_pcg4_f_c.gt.1)  dvj(p_pcg4_f_c)=1.0E-1
     if(p_pcg5_f_c.gt.1)  dvj(p_pcg5_f_c)=1.0E-1
     if(p_pcg6_f_c.gt.1)  dvj(p_pcg6_f_c)=1.0E-1
     if(p_pcg7_f_c.gt.1)  dvj(p_pcg7_f_c)=1.0E-1
     if(p_pcg8_f_c.gt.1)  dvj(p_pcg8_f_c)=1.0E-1
     if(p_pcg9_f_c.gt.1)  dvj(p_pcg9_f_c)=1.0E-1
     if(p_opcg1_f_c.gt.1)  dvj(p_opcg1_f_c) =1.0E-1
     if(p_opcg2_f_c.gt.1)  dvj(p_opcg2_f_c) =1.0E-1
     if(p_opcg3_f_c.gt.1)  dvj(p_opcg3_f_c)=1.0E-1
     if(p_opcg4_f_c.gt.1)  dvj(p_opcg4_f_c)=1.0E-1
     if(p_opcg5_f_c.gt.1)  dvj(p_opcg5_f_c)=1.0E-1
     if(p_opcg6_f_c.gt.1)  dvj(p_opcg6_f_c)=1.0E-1
     if(p_opcg7_f_c.gt.1)  dvj(p_opcg7_f_c)=1.0E-1
     if(p_opcg8_f_c.gt.1)  dvj(p_opcg8_f_c)=1.0E-1
     if(p_pcg1_f_o.gt.1)  dvj(p_pcg1_f_o) =1.0E-1
     if(p_pcg2_f_o.gt.1)  dvj(p_pcg2_f_o) =1.0E-1
     if(p_pcg3_f_o.gt.1)  dvj(p_pcg3_f_o)=1.0E-1
     if(p_pcg4_f_o.gt.1)  dvj(p_pcg4_f_o)=1.0E-1
     if(p_pcg5_f_o.gt.1)  dvj(p_pcg5_f_o)=1.0E-1
     if(p_pcg6_f_o.gt.1)  dvj(p_pcg6_f_o)=1.0E-1
     if(p_pcg7_f_o.gt.1)  dvj(p_pcg7_f_o)=1.0E-1
     if(p_pcg8_f_o.gt.1)  dvj(p_pcg8_f_o)=1.0E-1
     if(p_pcg9_f_o.gt.1)  dvj(p_pcg9_f_o)=1.0E-1
     if(p_opcg1_f_o.gt.1)  dvj(p_opcg1_f_o) =1.0E-1
     if(p_opcg2_f_o.gt.1)  dvj(p_opcg2_f_o) =1.0E-1
     if(p_opcg3_f_o.gt.1)  dvj(p_opcg3_f_o)=1.0E-1
     if(p_opcg4_f_o.gt.1)  dvj(p_opcg4_f_o)=1.0E-1
     if(p_opcg5_f_o.gt.1)  dvj(p_opcg5_f_o)=1.0E-1
     if(p_opcg6_f_o.gt.1)  dvj(p_opcg6_f_o)=1.0E-1
     if(p_opcg7_f_o.gt.1)  dvj(p_opcg7_f_o)=1.0E-1
     if(p_opcg8_f_o.gt.1)  dvj(p_opcg8_f_o)=1.0E-1
     if(p_ant1_c.gt.1)     dvj(p_ant1_c)=1.0E-1
     if(p_ant2_c.gt.1)     dvj(p_ant2_c)=1.0E-1
     if(p_ant3_c.gt.1)     dvj(p_ant3_c)=1.0E-1
     if(p_ant4_c.gt.1)     dvj(p_ant4_c)=1.0E-1
     if(p_ant1_o.gt.1)     dvj(p_ant1_o)=1.0E-1
     if(p_ant2_o.gt.1)     dvj(p_ant2_o)=1.0E-1
     if(p_ant3_o.gt.1)     dvj(p_ant3_o)=1.0E-1
     if(p_ant4_o.gt.1)     dvj(p_ant4_o)=1.0E-1
     if(p_biog1_c.gt.1)    dvj(p_biog1_c)=1.0E-1
     if(p_biog2_c.gt.1)    dvj(p_biog2_c)=1.0E-1
     if(p_biog3_c.gt.1)    dvj(p_biog3_c)=1.0E-1
     if(p_biog4_c.gt.1)    dvj(p_biog4_c)=1.0E-1
     if(p_biog1_o.gt.1)    dvj(p_biog1_o)=1.0E-1
     if(p_biog2_o.gt.1)    dvj(p_biog2_o)=1.0E-1
     if(p_biog3_o.gt.1)    dvj(p_biog3_o)=1.0E-1
     if(p_biog4_o.gt.1)    dvj(p_biog4_o)=1.0E-1



        DO l = 1, numgas
          hstar4(l) = hstar(l) ! preliminary              
!--------------------------------------------------
! Correction of diff. coefficient
!--------------------------------------------------
          dvj(l) = dvj(l)*(293.15/298.15)**1.75
          sc = 0.15/dvj(l)                             ! Schmidt Number at 20C
          dratio(l) = 0.242/dvj(l)                     ! of water vapor and gas at
!--------------------------------------------------
! Ratio of diffusion coefficient
!--------------------------------------------------
          scpr23(l) = (sc/0.72)**(2./3.)               ! (Schmidt # / Prandtl #)**
        END DO

! start of addition
        else if ( (config_flags%chem_opt == CB05_SORG_AQ_KPP) ) then is_cbm4_kpp

        hstar(p_no2) = 6.40E-3
        hstar(p_no) = 1.90E-3
        hstar(p_pan) = 2.97E+0
        hstar(p_o3) = 1.13E-2
        hstar(p_form) = 2.97E+3
        hstar(p_cxo3) = 1.14E+1
        hstar(p_hono) = 3.47E+5
        hstar(p_no3) = 1.50E+1
        hstar(p_pna) = 2.00E+13
        hstar(p_h2o2) = 7.45E+4
        hstar(p_co) = 8.20E-3
        hstar(p_ald2) = 1.14E+1
        hstar(p_aldx) = 1.14E+1
        hstar(p_mepx) = 2.21E+2
        hstar(p_rooh) = 1.68E+6
        hstar(p_pacd) = 4.73E+2
        hstar(p_mgly) = 3.71E+3
        hstar(p_ntr) = 1.13E+0
        hstar(p_so2) = 2.53E+5
        hstar(p_etha) = 2.00E-3
        hstar(p_ole) = 4.76E-3
        hstar(p_iole) = 1.35E-3
        hstar(p_tol) = 1.51E-1
        hstar(p_cres) = 4.00E+5
        hstar(p_xyl) = 1.45E-1
        hstar(p_isop) = 4.76E-3
        hstar(p_hno3) = 2.69E+13
        hstar(p_facd) = 9.85E+6
        hstar(p_aacd) = 9.63E+5
        hstar(p_nh3) = 1.04E+4
        hstar(p_n2o5) = 1.00E+10
        hstar(p_eth) = 4.67E-3
        hstar(p_par) = 1.13E-3  !wig, 1-May-2007: for CB05

        dhr(p_no2) = 2500.
        dhr(p_no) = 1480.
        dhr(p_pan) = 5760.
        dhr(p_o3) = 2300.
        dhr(p_form) = 7190.
        dhr(p_cxo3) = 6266.
        dhr(p_hono) = 3775.
        dhr(p_no3) = 0.
        dhr(p_pna) = 0.
        dhr(p_h2o2) = 6615.
        dhr(p_co) = 0.
        dhr(p_ald2) = 6266.
        dhr(p_aldx) = 6266.
        dhr(p_mepx) = 5607.
        dhr(p_rooh) = 10240.
        dhr(p_pacd) = 6170.
        dhr(p_mgly) = 7541.
        dhr(p_ntr) = 5487.
        dhr(p_so2) = 5816.
        dhr(p_etha) = 0.
        dhr(p_ole) = 0.
        dhr(p_iole) = 0.
        dhr(p_tol) = 0.
        dhr(p_cres) = 0.
        dhr(p_xyl) = 0.
        dhr(p_isop) = 0.
        dhr(p_hno3) = 8684.
        dhr(p_facd) = 5716.
        dhr(p_aacd) = 8374.
        dhr(p_nh3) = 3660.
        dhr(p_n2o5) = 0.
        dhr(p_eth) = 0.
        dhr(p_par) = 0.   !wig, 1-May-2007: for CB05

        f0(p_no2) = 0.1
        f0(p_no) = 0.
        f0(p_pan) = 0.1
        f0(p_o3) = 1.
        f0(p_form) = 0.
        f0(p_cxo3) = 1.
        f0(p_hono) = 0.1
        f0(p_no3) = 1.
        f0(p_pna) = 0.1
        f0(p_h2o2) = 1.
        f0(p_co) = 0.
        f0(p_ald2) = 0.
        f0(p_aldx) = 0.
        f0(p_mepx) = 0.1
        f0(p_rooh) = 0.1
        f0(p_pacd) = 0.1
        f0(p_mgly) = 0.
        f0(p_ntr) = 0.
        f0(p_so2) = 0.
        f0(p_etha) = 0.
        f0(p_ole) = 0.
        f0(p_iole) = 0.
        f0(p_tol) = 0.
        f0(p_cres) = 0.
        f0(p_xyl) = 0.
        f0(p_isop) = 0.
        f0(p_hno3) = 0.
        f0(p_facd) = 0.
        f0(p_aacd) = 0.
        f0(p_nh3) = 0.
        f0(p_n2o5) = 1.
        f0(p_eth) = 0.
        f0(p_par) = 0.   !wig, 1-May-2007: for CB05
        dvj(p_no2) = 0.147
        dvj(p_no) = 0.183
        dvj(p_pan) = 0.091
        dvj(p_o3)   = 0.175
        dvj(p_form) = 0.183
        dvj(p_cxo3) = 0.115
        dvj(p_hono) = 0.153
        dvj(p_no3) = 0.127
        dvj(p_pna) = 0.113
        dvj(p_h2o2) = 0.171
        dvj(p_co) = 0.189
        dvj(p_ald2) = 0.151
        dvj(p_aldx) = 0.151
        dvj(p_mepx) = 0.144
        dvj(p_rooh) = 0.127
        dvj(p_pacd) = 0.115
        dvj(p_mgly) = 0.118
        dvj(p_ntr) = 0.092
        dvj(p_so2) = 0.126
        dvj(p_etha) = 0.183
        dvj(p_ole) = 0.154
        dvj(p_iole) = 0.121
        dvj(p_tol) = 0.104
        dvj(p_cres) = 0.096
        dvj(p_xyl) = 0.097
        dvj(p_isop) = 0.121
        dvj(p_hno3) = 0.126
        dvj(p_facd) = 0.153
        dvj(p_aacd) = 0.124
        dvj(p_nh3) = 0.227
        dvj(p_n2o5) = 0.110
        dvj(p_ho) = 0.243
        dvj(p_ho2) = 0.174
        dvj(p_eth) = 0.189
        dvj(p_par) = 0.118   !wig, 1-May-2007: for CB05

        DO l = 1, numgas
          hstar4(l) = hstar(l) ! preliminary
! Correction of diff. coeff
          dvj(l) = dvj(l)*(293.15/298.15)**1.75
          sc = 0.15/dvj(l) ! Schmidt Number at 20degC
          dratio(l) = 0.242/dvj(l) !                                            ! of water vapor and gas at
! Ratio of diffusion coeffi
          scpr23(l) = (sc/0.72)**(2./3.) ! (Schmidt # / Prandtl #)**
        END DO

        else if ( (config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) ) then is_cbm4_kpp

        hstar(p_no2) = 6.40E-3
        hstar(p_no) = 1.90E-3
        hstar(p_pan) = 2.97E+0
        hstar(p_o3) = 1.13E-2
        hstar(p_form) = 2.97E+3
        hstar(p_cxo3) = 1.14E+1
        hstar(p_hono) = 3.47E+5
        hstar(p_no3) = 1.50E+1
        hstar(p_pna) = 2.00E+13
        hstar(p_h2o2) = 7.45E+4
        hstar(p_co) = 8.20E-3
        hstar(p_ald2) = 1.14E+1
        hstar(p_aldx) = 1.14E+1
        hstar(p_mepx) = 2.21E+2
        hstar(p_rooh) = 1.68E+6
        hstar(p_pacd) = 4.73E+2
        hstar(p_mgly) = 3.71E+3
        hstar(p_ntr) = 1.13E+0
        hstar(p_so2) = 2.53E+5
        hstar(p_etha) = 2.00E-3
        hstar(p_ole) = 4.76E-3
        hstar(p_iole) = 1.35E-3
        hstar(p_tol) = 1.51E-1
        hstar(p_cres) = 4.00E+5
        hstar(p_xyl) = 1.45E-1
        hstar(p_isop) = 4.76E-3
        hstar(p_hno3) = 2.69E+13
        hstar(p_facd) = 9.85E+6
        hstar(p_aacd) = 9.63E+5
        hstar(p_nh3) = 1.04E+4
        hstar(p_n2o5) = 1.00E+10
        hstar(p_eth) = 4.67E-3
        hstar(p_par) = 1.13E-3  !wig, 1-May-2007: for CB05

        dhr(p_no2) = 2500.
        dhr(p_no) = 1480.
        dhr(p_pan) = 5760.
        dhr(p_o3) = 2300.
        dhr(p_form) = 7190.
        dhr(p_cxo3) = 6266.
        dhr(p_hono) = 3775.
        dhr(p_no3) = 0.
        dhr(p_pna) = 0.
        dhr(p_h2o2) = 6615.
        dhr(p_co) = 0.
        dhr(p_ald2) = 6266.
        dhr(p_aldx) = 6266.
        dhr(p_mepx) = 5607.
        dhr(p_rooh) = 10240.
        dhr(p_pacd) = 6170.
        dhr(p_mgly) = 7541.
        dhr(p_ntr) = 5487.
        dhr(p_so2) = 5816.
        dhr(p_etha) = 0.
        dhr(p_ole) = 0.
        dhr(p_iole) = 0.
        dhr(p_tol) = 0.
        dhr(p_cres) = 0.
        dhr(p_xyl) = 0.
        dhr(p_isop) = 0.
        dhr(p_hno3) = 8684.
        dhr(p_facd) = 5716.
        dhr(p_aacd) = 8374.
        dhr(p_nh3) = 3660.
        dhr(p_n2o5) = 0.
        dhr(p_eth) = 0.
        dhr(p_par) = 0.   !wig, 1-May-2007: for CB05

        f0(p_no2) = 0.1
        f0(p_no) = 0.
        f0(p_pan) = 0.1
        f0(p_o3) = 1.
        f0(p_form) = 0.
        f0(p_cxo3) = 1.
        f0(p_hono) = 0.1
        f0(p_no3) = 1.
        f0(p_pna) = 0.1
        f0(p_h2o2) = 1.
        f0(p_co) = 0.
        f0(p_ald2) = 0.
        f0(p_aldx) = 0.
        f0(p_mepx) = 0.1
        f0(p_rooh) = 0.1
        f0(p_pacd) = 0.1
        f0(p_mgly) = 0.
        f0(p_ntr) = 0.
        f0(p_so2) = 0.
        f0(p_etha) = 0.
        f0(p_ole) = 0.
        f0(p_iole) = 0.
        f0(p_tol) = 0.
        f0(p_cres) = 0.
        f0(p_xyl) = 0.
        f0(p_isop) = 0.
        f0(p_hno3) = 0.
        f0(p_facd) = 0.
        f0(p_aacd) = 0.
        f0(p_nh3) = 0.
        f0(p_n2o5) = 1.
        f0(p_eth) = 0.
        f0(p_par) = 0.   !wig, 1-May-2007: for CB05
        dvj(p_no2) = 0.147
        dvj(p_no) = 0.183
        dvj(p_pan) = 0.091
        dvj(p_o3)   = 0.175
        dvj(p_form) = 0.183
        dvj(p_cxo3) = 0.115
        dvj(p_hono) = 0.153
        dvj(p_no3) = 0.127
        dvj(p_pna) = 0.113
        dvj(p_h2o2) = 0.171
        dvj(p_co) = 0.189
        dvj(p_ald2) = 0.151
        dvj(p_aldx) = 0.151
        dvj(p_mepx) = 0.144
        dvj(p_rooh) = 0.127
        dvj(p_pacd) = 0.115
        dvj(p_mgly) = 0.118
        dvj(p_ntr) = 0.092
        dvj(p_so2) = 0.126
        dvj(p_etha) = 0.183
        dvj(p_ole) = 0.154
        dvj(p_iole) = 0.121
        dvj(p_tol) = 0.104
        dvj(p_cres) = 0.096
        dvj(p_xyl) = 0.097
        dvj(p_isop) = 0.121
        dvj(p_hno3) = 0.126
        dvj(p_facd) = 0.153
        dvj(p_aacd) = 0.124
        dvj(p_nh3) = 0.227
        dvj(p_n2o5) = 0.110
        dvj(p_ho) = 0.243
        dvj(p_ho2) = 0.174
        dvj(p_eth) = 0.189
        dvj(p_par) = 0.118   !wig, 1-May-2007: for CB05

        DO l = 1, numgas
          hstar4(l) = hstar(l) ! preliminary
! Correction of diff. coeff
          dvj(l) = dvj(l)*(293.15/298.15)**1.75
          sc = 0.15/dvj(l) ! Schmidt Number at 20degC
          dratio(l) = 0.242/dvj(l) !                                            ! of water vapor and gas at
! Ratio of diffusion coeffi
          scpr23(l) = (sc/0.72)**(2./3.) ! (Schmidt # / Prandtl #)**
        END DO

! end of addition

        else is_cbm4_kpp

        hstar(p_no2)   = 6.40E-3
        hstar(p_no)    = 1.90E-3
        hstar(p_pan)   = 2.97E+0
        hstar(p_o3)    = 1.13E-2
        hstar(p_hcho)  = 2.97E+3
        hstar(p_hono)  = 3.47E+5
        hstar(p_no3)   = 1.50E+1
        hstar(p_h2o2)  = 7.45E+4
        hstar(p_co)    = 8.20E-3
        hstar(p_ald2)  = 1.14E+1
        hstar(p_onit)  = 1.13E+0
        hstar(p_so2)   = 2.53E+5
        hstar(p_eth)   = 2.00E-3
        hstar(p_ole)   = 3.05E-3 !(average of oli and olt)
        hstar(p_tol)   = 1.51E-1
        hstar(p_cres)  = 4.00E+5
        hstar(p_xyl)   = 1.45E-1
        hstar(p_iso)   = 4.76E-3
        hstar(p_hno3)  = 2.69E+13
        hstar(p_nh3)   = 1.04E+4
        hstar(p_n2o5)  = 1.00E+10
        hstar(p_par)   = 1.13E-3  

!     -DH/R (for temperature correction)
!     [-DH/R]=K

        dhr(p_no2)  = 2500.
        dhr(p_no)   = 1480.
        dhr(p_pan)  = 5760.
        dhr(p_o3)   = 2300.
        dhr(p_hcho) = 7190.
        dhr(p_hono) = 3775.
        dhr(p_no3)  = 0.
        dhr(p_h2o2) = 6615.
        dhr(p_co)   = 0.
        dhr(p_ald2) = 6266.
        dhr(p_onit) = 5487.
        dhr(p_so2)  = 5816.
        dhr(p_eth) = 0.
        dhr(p_ole)  = 0.
        dhr(p_tol)  = 0.
        dhr(p_cres) = 0.
        dhr(p_xyl)  = 0.
        dhr(p_iso)  = 0.
        dhr(p_hno3) = 8684.
        dhr(p_nh3)  = 3660.
        dhr(p_n2o5) = 0.
        dhr(p_par)  = 0.  
!     REACTIVITY FACTORS
!     [f0]=1

        f0(p_no2)  = 0.1
        f0(p_no)   = 0.
        f0(p_pan)  = 0.1
        f0(p_o3)   = 1.
        f0(p_hcho) = 0.
        f0(p_hono) = 0.1
        f0(p_no3)  = 1.
        f0(p_h2o2) = 1.
        f0(p_co)   = 0.
        f0(p_ald2) = 0.
        f0(p_onit) = 0.
        f0(p_so2)  = 0.
        f0(p_eth)  = 0.
        f0(p_ole)  = 0.
        f0(p_tol)  = 0.
        f0(p_csl)  = 0.
        f0(p_xyl)  = 0.
        f0(p_iso)  = 0.
        f0(p_hno3) = 0.
        f0(p_nh3)  = 0.
        f0(p_n2o5) = 1.
        f0(p_par)  = 0.   
!     DIFFUSION COEFFICIENTS
!     [DV]=cm2/s (assumed: 1/SQRT(molar mass) when not known)

        dvj(p_no2)  = 0.147
        dvj(p_no)   = 0.183
        dvj(p_pan)  = 0.091
        dvj(p_o3)   = 0.175
        dvj(p_hcho) = 0.183
        dvj(p_hono) = 0.153
        dvj(p_no3)  = 0.127
        dvj(p_h2o2) = 0.171
        dvj(p_co)   = 0.189
        dvj(p_ald2) = 0.151
        dvj(p_onit) = 0.092
        dvj(p_so2)  = 0.126
        dvj(p_eth)  = 0.183
        dvj(p_ole)  = 0.135
        dvj(p_tol)  = 0.104
        dvj(p_csl)  = 0.096
        dvj(p_xyl)  = 0.097
        dvj(p_iso)  = 0.121
        dvj(p_hno3) = 0.126
        dvj(p_nh3)  = 0.227
        dvj(p_n2o5) = 0.110
        dvj(p_par)  = 0.118  
        DO l = 1, numgas
          hstar4(l) = hstar(l) ! preliminary              
! Correction of diff. coeff
          dvj(l) = dvj(l)*(293.15/298.15)**1.75
          sc = 0.15/dvj(l) ! Schmidt Number at 20C
          dratio(l) = 0.242/dvj(l) !                                            ! of water vapor and gas at
! Ratio of diffusion coeffi
          scpr23(l) = (sc/0.72)**(2./3.) ! (Schmidt # / Prandtl #)**

       end DO
    end if is_cbm4_kpp



!     DATA FOR AEROSOL PARTICLE DEPOSITION FOR THE MODEL OF
!     J. W. ERISMAN, A. VAN PUL AND P. WYERS
!     ATMOSPHERIC ENVIRONMENT 28 (1994), 2595-2607

!     vd = (u* / k) * CORRECTION FACTORS

!     CONSTANT K FOR LANDUSE TYPES:
! urban and built-up land                  
        kpart(1) = 500.
! dryland cropland and pasture             
        kpart(2) = 500.
! irrigated cropland and pasture           
        kpart(3) = 500.
! mixed dryland/irrigated cropland and past
        kpart(4) = 500.
! cropland/grassland mosaic                
        kpart(5) = 500.
! cropland/woodland mosaic                 
        kpart(6) = 100.
! grassland                                
        kpart(7) = 500.
! shrubland                                
        kpart(8) = 500.
! mixed shrubland/grassland                
        kpart(9) = 500.
! savanna                                  
        kpart(10) = 500.
! deciduous broadleaf forest               
        kpart(11) = 100.
! deciduous needleleaf forest              
        kpart(12) = 100.
! evergreen broadleaf forest               
        kpart(13) = 100.
! evergreen needleleaf forest              
        kpart(14) = 100.
! mixed forest                             
        kpart(15) = 100.
! water bodies                             
        kpart(16) = 500.
! herbaceous wetland                       
        kpart(17) = 500.
! wooded wetland                           
        kpart(18) = 500.
! barren or sparsely vegetated             
        kpart(19) = 500.
! herbaceous tundra                        
        kpart(20) = 500.
! wooded tundra                            
        kpart(21) = 100.
! mixed tundra                             
        kpart(22) = 500.
! bare ground tundra                       
        kpart(23) = 500.
! snow or ice                              
        kpart(24) = 500.
!     Comments:
        kpart(25) = 500.
!     Erisman et al. (1994) give
!     k = 500 for low vegetation and k = 100 for forests.

!     For desert k = 500 is taken according to measurements
!     on bare soil by
!     J. Fontan, A. Lopez, E. Lamaud and A. Druilhet (1997)
!     Vertical Flux Measurements of the Submicronic Aerosol Particles
!     and Parametrisation of the Dry Deposition Velocity
!     in: Biosphere-Atmosphere Exchange of Pollutants
!     and Trace Substances
!     Editor: S. Slanina. Springer-Verlag Berlin, Heidelberg, 1997
!     pp. 381-390

!     For coniferous forest the Erisman value of  k = 100 is taken.
!     Measurements of Erisman et al. (1997) in a coniferous forest
!     in the Netherlands, lead to values of k between 20 and 38
!     (Atmospheric Environment 31 (1997), 321-332).
!     However, these high values of vd may be reached during
!     instable cases. The eddy correlation measurements
!     of Gallagher et al. (1997) made during the same experiment
!     show for stable cases (L>0) values of k between 200 and 250
!     at minimum (Atmospheric Environment 31 (1997), 359-373).
!     Fontan et al. (1997) found k = 250 in a forest
!     of maritime pine in southwestern France.

!     For gras, model calculations of Davidson et al. support
!     the value of 500.
!     C. I. Davidson, J. M. Miller and M. A. Pleskov
!     The Influence of Surface Structure on Predicted Particles
!     Dry Deposition to Natural Gras Canopies
!     Water, Air, and Soil Pollution 18 (1982) 25-43

!     Snow covered surface: The experiment of Ibrahim et al. (1983)
!     gives k = 436 for 0.7 um diameter particles.
!     The deposition velocity of Milford and Davidson (1987)
!     gives k = 154 for continental sulfate aerosol.
!     M. Ibrahim, L. A. Barrie and F. Fanaki
!     Atmospheric Environment 17 (1983), 781-788

!     J. B. Milford and C. I. Davidson
!     The Sizes of Particulate Sulfate and Nitrate in the Atmosphere
!     - A Review
!     JAPCA 37 (1987), 125-134
! no data                                  
!       WRITE (0,*) ' return from rcread '
!     *********************************************************

!     Simplified landuse scheme for deposition and biogenic emission
!     subroutines
!     (ISWATER and ISICE are already defined elsewhere,
!     therefore water and ice are not considered here)

!     1 urban or bare soil
!     2 agricultural
!     3 grassland
!     4 deciduous forest
!     5 coniferous and mixed forest
!     6 other natural landuse categories


        IF (mminlu=='OLD ') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 3
          ixxxlu(4) = 4
          ixxxlu(5) = 5
          ixxxlu(6) = 5
          ixxxlu(7) = 0
          ixxxlu(8) = 6
          ixxxlu(9) = 1
          ixxxlu(10) = 6
          ixxxlu(11) = 0
          ixxxlu(12) = 4
          ixxxlu(13) = 6
        END IF
        IF (mminlu=='USGS') THEN
          ixxxlu(1) = 1
          ixxxlu(2) = 2
          ixxxlu(3) = 2
          ixxxlu(4) = 2
          ixxxlu(5) = 2
          ixxxlu(6) = 4
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 3
          ixxxlu(10) = 6
          ixxxlu(11) = 4
          ixxxlu(12) = 5
          ixxxlu(13) = 4
          ixxxlu(14) = 5
          ixxxlu(15) = 5
          ixxxlu(16) = 0
          ixxxlu(17) = 6
          ixxxlu(18) = 4
          ixxxlu(19) = 1
          ixxxlu(20) = 6
          ixxxlu(21) = 4
          ixxxlu(22) = 6
          ixxxlu(23) = 1
          ixxxlu(24) = 0
          ixxxlu(25) = 1
        END IF
        IF (mminlu=='SiB ') THEN
          ixxxlu(1) = 4
          ixxxlu(2) = 4
          ixxxlu(3) = 4
          ixxxlu(4) = 5
          ixxxlu(5) = 5
          ixxxlu(6) = 6
          ixxxlu(7) = 3
          ixxxlu(8) = 6
          ixxxlu(9) = 6
          ixxxlu(10) = 6
          ixxxlu(11) = 1
          ixxxlu(12) = 2
          ixxxlu(13) = 6
          ixxxlu(14) = 1
          ixxxlu(15) = 0
          ixxxlu(16) = 0
          ixxxlu(17) = 1
        END IF

#ifdef NETCDF
!--------------------------------------------------
!	... check for mozart chemistry
!--------------------------------------------------
!---------------------------------------------------------------------
!	... open wesely pft netcdf file
!---------------------------------------------------------------------
is_mozart : &
      if( chm_is_moz ) then
!---------------------------------------------------------------------
!	... allocate column_density type
!---------------------------------------------------------------------
        if( id == 1 .and. .not. allocated(seasonal_pft) ) then
          CALL nl_get_max_dom( 1,max_dom )
          allocate( seasonal_pft(max_dom),stat=astat )
          if( astat /= 0 ) then
            CALL wrf_message( 'dep_init: failed to allocate wesely_pft type seasonal_pft' )
            CALL wrf_abort
          endif
          write(err_msg,*) 'dep_init: initializing for ',max_dom,' domains'
          CALL wrf_message( trim(err_msg) )
          seasonal_pft(:)%is_allocated = .false.
        endif
seasonal_pft_allocated : &
        IF( .not. seasonal_pft(id)%is_allocated ) then
        IF( wrf_dm_on_monitor() ) THEN
         write(id_num,'(i3)') 100+id
         err_msg = 'dep_init: initializing domain ' // id_num(2:3)
         CALL wrf_message( trim(err_msg) )
         cpos = index( config_flags%wes_seasonal_inname, '<domain>' )
         if( cpos > 0 ) then
           filename = ' '
           filename = config_flags%wes_seasonal_inname(:cpos-1) // 'd' // id_num(2:3)
           cpos = cpos + 8
           slen = len_trim( config_flags%wes_seasonal_inname )
           if( cpos < slen ) then
             filename(len_trim(filename)+1:) = config_flags%wes_seasonal_inname(cpos:slen)
           endif
         else
           filename = trim( config_flags%wes_seasonal_inname )
         endif
         err_msg = 'dep_init: failed to open file ' // trim(filename)
         call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
!---------------------------------------------------------------------
!	... get dimensions
!---------------------------------------------------------------------
         err_msg = 'dep_init: failed to get npft id'
         call handle_ncerr( nf_inq_dimid( ncid, 'npft', dimid ), trim(err_msg) ) 
         err_msg = 'dep_init: failed to npft'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, seasonal_pft(id)%npft ), trim(err_msg) )
         err_msg = 'dep_init: failed to get months year id'
         call handle_ncerr( nf_inq_dimid( ncid, 'months', dimid ), trim(err_msg) )
         err_msg = 'dep_init: failed to get months'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, seasonal_pft(id)%months ), trim(err_msg) )
         err_msg = 'ftuv_init: failed to get west_east id'
         call handle_ncerr( nf_inq_dimid( ncid, 'west_east', dimid ), trim(err_msg) ) 
         err_msg = 'ftuv_init: failed to get west_east'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, lon_e ), trim(err_msg) )
         err_msg = 'ftuv_init: failed to get south_north id'
         call handle_ncerr( nf_inq_dimid( ncid, 'south_north', dimid ), trim(err_msg) ) 
         err_msg = 'ftuv_init: failed to get south_north'
         call handle_ncerr( nf_inq_dimlen( ncid, dimid, lat_e ), trim(err_msg) )
        end IF
!---------------------------------------------------------------------
!	... bcast the dimensions
!---------------------------------------------------------------------
         CALL wrf_dm_bcast_bytes ( seasonal_pft(id)%npft , IWORDSIZE )
         CALL wrf_dm_bcast_bytes ( seasonal_pft(id)%months , IWORDSIZE )
         CALL wrf_dm_bcast_bytes ( lon_e , IWORDSIZE )
         CALL wrf_dm_bcast_bytes ( lat_e , IWORDSIZE )
!---------------------------------------------------------------------
!	... allocate arrays
!---------------------------------------------------------------------
         iend = min( ipe,ide-1 )
         jend = min( jpe,jde-1 )
         allocate( input_wes_seasonal(lon_e,lat_e,seasonal_pft(id)%npft,seasonal_pft(id)%months), stat=astat )
         if( astat /= 0 ) then
            call wrf_message( 'dep_init: failed to allocate input_wes_seasonal' )
            call wrf_abort
         end if
         allocate( seasonal_pft(id)%seasonal_wes(ips:iend,jps:jend,seasonal_pft(id)%npft,seasonal_pft(id)%months), stat=astat )
         if( astat /= 0 ) then
            call wrf_message( 'dep_init: failed to allocate seasonal_wes' )
            call wrf_abort
         end if
         seasonal_pft(id)%is_allocated = .true.
!---------------------------------------------------------------------
!	... read array
!---------------------------------------------------------------------
        IF ( wrf_dm_on_monitor() ) THEN
         err_msg = 'dep_init: failed to get seasonal_wes variable id'
         call handle_ncerr( nf_inq_varid( ncid, 'seasonal_wes', varid ), trim(err_msg) )
         err_msg = 'dep_init: failed to read seasonal_wes variable'
         call handle_ncerr( nf_get_var_int( ncid, varid, input_wes_seasonal ), trim(err_msg) )
!---------------------------------------------------------------------
!	... close netcdf file
!---------------------------------------------------------------------
         err_msg = 'dep_init: failed to close file wrf_season_wes_usgs.nc'
         call handle_ncerr( nf_close( ncid ), trim(err_msg) )
        end if

        DM_BCAST_MACRO(input_wes_seasonal)

        seasonal_pft(id)%seasonal_wes(ips:iend,jps:jend,:seasonal_pft(id)%npft,:seasonal_pft(id)%months) = &
                input_wes_seasonal(ips:iend,jps:jend,:seasonal_pft(id)%npft,:seasonal_pft(id)%months)
        deallocate( input_wes_seasonal )
        endif seasonal_pft_allocated
      endif is_mozart
#endif
      END SUBROUTINE dep_init

      SUBROUTINE HL_init( numgas )

      use module_state_description, only : param_first_scalar
      use module_scalar_tables, only : chem_dname_table
      use module_chem_utilities, only : UPCASE
      use module_HLawConst

      integer, intent(in) :: numgas

!----------------------------------------------------------------------
!  local variables
!----------------------------------------------------------------------
      integer :: m, m1
      integer :: astat
      character(len=64) :: HL_tbl_name
      character(len=64) :: wrf_spc_name

is_allocated : &
      if( .not. allocated( HL_ndx ) ) then
!----------------------------------------------------------------------
!  scan HLawConst table for match with chem_opt scheme gas phase species
!----------------------------------------------------------------------
        allocate( HL_ndx(numgas),stat=astat )
        if( astat /= 0 ) then
          call wrf_error_fatal("HL_init: failed to allocate HL_ndx")
        endif
        HL_ndx(:) = 0
        do m = param_first_scalar,numgas
          wrf_spc_name = chem_dname_table(1,m)
          call upcase( wrf_spc_name )
          do m1 = 1,nHLC
            HL_tbl_name = HLC(m1)%name
            call upcase( HL_tbl_name )
            if( trim(HL_tbl_name) == trim(wrf_spc_name) ) then
              HL_ndx(m) = m1
              exit
            endif
          end do
        end do
      endif is_allocated

      END SUBROUTINE HL_init

#ifdef NETCDF

      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: ret
      character(len=*), intent(in) :: mes

include 'netcdf.inc'

      if( ret /= nf_noerr ) then
         call wrf_message( trim(mes) )
         call wrf_message( trim(nf_strerror(ret)) )
         call wrf_abort
      end if

      end subroutine handle_ncerr
#endif

    END MODULE module_dep_simple
