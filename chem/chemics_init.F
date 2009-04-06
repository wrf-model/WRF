!WRF:MODEL_LAYER:INITIALIZATION
!
   subroutine chem_init (id,chem,emis_ant,scalar,dt,bioemdt,photdt,chemdt,stepbioe, &
               stepphot,stepchem,stepfirepl,plumerisefire_frq,z_at_w,xlat,xlong,    &
               g,aerwrf,config_flags,grid,alt,t,p,CONVFAC,ttday,tcosz,julday,gmt,   &
               gd_cloud, gd_cloud2,raincv_a,raincv_b,                               &
               gd_cloud_a, gd_cloud2_a,                              &
               gd_cloud_b, gd_cloud2_b,                              &
               tauaer1,tauaer2,tauaer3,tauaer4,                      &
               gaer1,gaer2,gaer3,gaer4,                              &
               waer1,waer2,waer3,waer4,                              &
               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                  &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               last_chem_time_year, last_chem_time_month,            &
               last_chem_time_day, last_chem_time_hour,              &
               last_chem_time_minute, last_chem_time_second,         &
               chem_in_opt, kemit,                                   &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_phot_mad
   USE module_aerosols_sorgam
   USE module_dep_simple
   USE module_data_gocart_dust
   USE module_data_gocart_seas
   USE module_data_gocartchem
   USE module_gocart_chem ! , only: szangle
   USE module_cbm4_initmixrats, only:     cbm4_init_wrf_mixrats
   USE module_cbmz_initmixrats, only:     cbmz_init_wrf_mixrats
   USE module_mosaic_driver, only:        init_data_mosaic_asect
   USE module_mosaic_initmixrats, only:   mosaic_init_wrf_mixrats
   USE module_input_chem_data, only:      get_last_gas,              &
                                          gasprofile_init_pnnl,      &
                                          last_chem_time,            &
                                          setup_gasprofile_maps
   USE module_mixactivate_wrappers, only: mosaic_mixactivate_init
   IMPLICIT NONE

   real  , intent(in) :: bioemdt,photdt,chemdt,dt,gmt
   INTEGER,      INTENT(IN   ) :: plumerisefire_frq
   INTEGER,      INTENT(IN   ) :: chem_in_opt
   INTEGER,      INTENT(IN   ) :: id,julday,kemit,                   &
                                  last_chem_time_year,               &
                                  last_chem_time_month,              &
                                  last_chem_time_day,                &
                                  last_chem_time_hour,               &
                                  last_chem_time_minute,             &
                                  last_chem_time_second,             &
                                  ids,ide, jds,jde, kds,kde,         &
                                  ims,ime, jms,jme, kms,kme,         &
                                  its,ite, jts,jte, kts,kte
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,   &
                               tauaer1,tauaer2,tauaer3,tauaer4,      &
                               gd_cloud, gd_cloud2,                  &
                               gd_cloud_a, gd_cloud2_a,              &
                               gd_cloud_b, gd_cloud2_b,              &
                               gaer1,gaer2,gaer3,gaer4,              &
                               waer1,waer2,waer3,waer4
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme , 1:4 )   ,         &
          INTENT(INOUT   ) ::                                        &
                               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               z_at_w,t,p,alt,convfac
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_chem ) ,       &
          INTENT(INOUT   ) ::                                        &
                              chem 
   REAL,  DIMENSION( ims:ime , 1:kemit , jms:jme, num_emis_ant ) ,       &
          INTENT(INOUT   ) ::                                        &
                              emis_ant
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme, num_scalar ) ,     &
          INTENT(INOUT   ) ::                                        &
                              scalar
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               aerwrf
   REAL,  DIMENSION( ims:ime ,  jms:jme )         ,         &
          INTENT(INOUT   ) ::                                        &
                               ttday,tcosz,xlat,xlong,raincv_a,raincv_b
   real, INTENT (IN) :: g
   integer, intent(out) :: stepbioe,stepphot,stepchem,stepfirepl
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
   TYPE(domain) , TARGET,        INTENT (in) ::     grid
!
! local stuff
!
   TYPE(WRFU_TimeInterval) :: tmpTimeInterval
   integer :: i,j,k,l,numgas,ixhour,n,ndystep,kk,nv
   real, DIMENSION (1,1) :: sza,cosszax
   real :: xtime,xhour,xmin,gmtp,xlonn,rlat
#ifdef CHEM_DBG_I
    call print_chem_species_index( config_flags%chem_opt )
#endif

call wrf_message("*********************************************************************")
call wrf_message("*             PROGRAM: WRF/CHEM VERSION 3.1                         *")
call wrf_message("*                                                                   *")
call wrf_message("*    PLEASE REPORT ANY BUGS TO WRF/CHEM HELP at                     *")
call wrf_message("*                                                                   *")
call wrf_message("*              wrfchemhelp.gsd@noaa.gov                             *")
call wrf_message("*                                                                   *")
call wrf_message("*********************************************************************")

    numgas = get_last_gas(config_flags%chem_opt)
 
#if ( NMM_CORE == 1 )
call wrf_message("*********************************************************************")
call wrf_message("* WARNING: THE USE OF THE NMM WITH CHEMISTRY IS                     *")
call wrf_message("*          CONSIDERED BETA CODE THAT IS STILL IN DEVELOPMENT.       *")
call wrf_message("*          PLEASE REPORT ANY BUGS TO wrfchemhelp.gsd@noaa.gov       *")
call wrf_message("*********************************************************************")
#endif

   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (GOCART_SIMPLE)
       CALL wrf_debug(15,'calling only gocart aerosols driver from chem_driver')
     CASE (GOCARTRADM2_KPP)
       CALL wrf_debug(15,'calling gocart and radmkpp driver from chem_driver')
     CASE (GOCARTRADM2)
       CALL wrf_debug(15,'calling gocart and radm driver from chem_driver')
     CASE (GOCARTRACM_KPP)
       CALL wrf_debug(15,'calling gocart and racmkpp driver from chem_driver')
     CASE (CBM4_KPP )
       CALL wrf_debug(15,'calling CB4 from chem_driver')
       call wrf_message("WARNING: CB4 chemistry option is highly experimental and not recommended for use.")
!      call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (NMHC9_KPP )
       CALL wrf_debug(15,'calling NMHC9 from chem_driver')
       call wrf_message("WARNING: NMHC9 chemistry option is highly experimental and not recommended for use.")
       call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (RADM2SORG_AQ )
       CALL wrf_debug(15,'calling RADM2/MADE/SORGAM with AQ chemistry from chem_driver')
       call wrf_message("WARNING: RADM2SORG_AQ chemistry option is experimental and not yet fully tested.")
       call wrf_message("         We recommend contacting wrfchemhelp for assistance.")
     CASE (RACMSORG_AQ )
       CALL wrf_debug(15,'calling RACM/MADE/SORGAM from chem_driver')
       call wrf_message("WARNING: RACMSORG_AQ chemistry option is highly experimental and not recommended for use.")
       call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
   END SELECT chem_select

   if ( config_flags%dust_opt == 1 ) then
       call wrf_message("WARNING: dust option 1 currently works only with the GOCART aerosol option.")
   endif

   if ( config_flags%seas_opt == 1 ) then
       call wrf_message("WARNING: sea salt option 1 currently works only with the GOCART aerosol option.")
   endif

   if ( config_flags%biomass_burn_opt == 1 ) then
       CALL wrf_debug(15,'calling biomass burning')
   endif

   if ( config_flags%wetscav_onoff == 1 ) then
      if ( config_flags%chem_opt >= 13 .OR. config_flags%chem_opt <= 8) then
        call wrf_error_fatal("ERROR: wet scavaging option requires chem_opt = 9 through 12 to function.")
      endif
      if ( config_flags%mp_physics /= 2 ) then
        call wrf_error_fatal("ERROR: wet scavaging option requires mp_phys = 2 (Lin et al.) to function.")
      endif
   endif

   if ( config_flags%cldchem_onoff == 1 ) then
      if ( config_flags%chem_opt >= 13 .OR. config_flags%chem_opt <= 8) then
        call wrf_error_fatal("ERROR:  cloud chemistry option requires chem_opt = 9 through 12 to function.")
      endif
      if ( config_flags%mp_physics /= 2 ) then
        call wrf_error_fatal("ERROR: cloud chemistry option requires mp_phys = 2 (Lin et al.) to function.")
      endif
   endif

    IF ( config_flags%chem_opt == 0 .AND. config_flags%aer_ra_feedback .NE. 0 ) THEN
!       config_flags%aer_ra_feedback = 0
        call wrf_error_fatal(" ERROR: CHEM_INIT: FOR CHEM_OPT = 0, AER_RA_FEEDBACK MUST = 0 ")
    ENDIF

!   IF ( config_flags%chem_opt == 2 .AND. config_flags%dust_opt .NE. 2 ) THEN
!       call wrf_error_fatal(" ERROR: USE dust_opt = 2 when using MADE/SORGAM aerosol option ")
!   ENDIF

    if( .NOT. config_flags%restart ) then
       do j=jts,jte
          do k=kts,kte
             do i=its,ite
                tauaer1(i,k,j) = 0.
                tauaer2(i,k,j) = 0.
                tauaer3(i,k,j) = 0.
                tauaer4(i,k,j) = 0.
                gaer1(i,k,j) = 0.
                gaer2(i,k,j) = 0.
                gaer3(i,k,j) = 0.
                gaer4(i,k,j) = 0.
                waer1(i,k,j) = 0.
                waer2(i,k,j) = 0.
                waer3(i,k,j) = 0.
                waer4(i,k,j) = 0.
                l2aer(i,k,j,1) = 0.
                l2aer(i,k,j,2) = 0.
                l2aer(i,k,j,3) = 0.
                l2aer(i,k,j,4) = 0.
                l3aer(i,k,j,1) = 0.
                l3aer(i,k,j,2) = 0.
                l3aer(i,k,j,3) = 0.
                l3aer(i,k,j,4) = 0.
                l4aer(i,k,j,1) = 0.
                l4aer(i,k,j,2) = 0.
                l4aer(i,k,j,3) = 0.
                l4aer(i,k,j,4) = 0.
                l5aer(i,k,j,1) = 0.
                l5aer(i,k,j,2) = 0.
                l5aer(i,k,j,3) = 0.
                l5aer(i,k,j,4) = 0.
                l6aer(i,k,j,1) = 0.
                l6aer(i,k,j,2) = 0.
                l6aer(i,k,j,3) = 0.
                l6aer(i,k,j,4) = 0.
                l7aer(i,k,j,1) = 0.
                l7aer(i,k,j,2) = 0.
                l7aer(i,k,j,3) = 0.
                l7aer(i,k,j,4) = 0.
             end do
          end do
       end do
       do l=1,num_emis_ant
       do j=jts,jte
          do k=1,kemit
             do i=its,ite
                emis_ant(i,k,j,l) = 0.
             end do
          end do
       end do
       end do
    end if
    do j=jts,jte
       do k=kts,kte
          do i=its,ite
             gd_cloud(i,k,j) = 0.
             gd_cloud2(i,k,j) = 0.
             gd_cloud_a(i,k,j) = 0.
             gd_cloud2_a(i,k,j) = 0.
             gd_cloud_b(i,k,j) = 0.
             gd_cloud2_b(i,k,j) = 0.
          end do
       end do
    end do
    do j=jts,jte
          do i=its,ite
            raincv_a(i,j)=0.
            raincv_b(i,j)=0.
       end do
    end do

!   RETURN IF CHEMISTRY IS NOT RUNNING
    IF ( config_flags%chem_opt == 0 ) RETURN

    stepbioe=nint(bioemdt*60./dt)
    stepphot=nint(photdt*60./dt)
    stepchem=nint(chemdt*60./dt)
    stepfirepl=plumerisefire_frq*60/nint(dt)
    stepbioe=max(stepbioe,1)
    stepphot=max(stepphot,1)
    stepchem=max(stepchem,1)
    stepfirepl=max(stepfirepl,1)
    call wrf_debug( 15, 'in chem_init' )
!   print *,'stepbioe,stepphot,stepchem,stepfirepl',stepbioe,stepphot,stepchem,stepfirepl
!
! Setup the cross reference mappings between the default profiles and
! the gas mechanism species (wig, 2-May-2007)
!
   call setup_gasprofile_maps(config_flags%chem_opt,numgas)
!
! An alternative ozone profile option
!
    if ( (config_flags%gas_bc_opt == GAS_BC_PNNL) .or.   &
         (config_flags%gas_ic_opt == GAS_IC_PNNL) ) then
       call gasprofile_init_pnnl( config_flags%chem_opt )
    end if
!
! we are doing initialization here, first photolysis
!
   phot_select: SELECT CASE(config_flags%phot_opt)
     CASE (PHOTMAD)
     CALL wrf_debug(00,'call madronich phot initialization')
       call photmad_init(z_at_w,aerwrf,g,                            &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
  END SELECT phot_select
!
! initialization for aerosols
!

! Technically, we are doing a no-no here. The array is_aerosol is being
! "re-allocated" and overwritten by each domain. Therefore, if different
! chemistry settings are used for each nest level, we will get errors. I
! have a feeling that this is not the only place that will have a problem
! though so I am not worrying about it for now. The dilemma is that we
! cannot declare and allocate is_aerosol via the registry because the
! number of aerosols is not known at compile time. I have not figured out
! a way to allocate a separate array for each nest outside of the
! regiistry. For now, we will do a simple check to trap a change in the
! desired size of num_chem and assume that if the size is the same, the
! species will be too.  wig, 24-Oct-2005
     if( .not.allocated(is_aerosol) ) then
        allocate (is_aerosol(num_chem))
     else
        if( size(is_aerosol) /= num_chem ) &
             call wrf_error_fatal("The number of chemistry species has changed between nests. Are you trying to mix chem_opt settings between nests? Shame on you!")
     end if

   if( .NOT. config_flags%restart ) then
   kpp_select: SELECT CASE(config_flags%chem_opt)
     CASE (GOCARTRACM_KPP,RACM_KPP,RACMPM_KPP,RACMSORG_KPP, RACM_MIM_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=370.
                   chem(i,k,j,p_ch4)=1.7
                   chem(i,k,j,p_ete)=chem(i,k,j,p_olt)
                   chem(i,k,j,p_ete)=epsilc
                   chem(i,k,j,p_udd)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_hket)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_api)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_lim)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_dien)=chem(i,k,j,p_ete)
                   chem(i,k,j,p_macr)=chem(i,k,j,p_ete)
                enddo
             enddo
          enddo
        endif
     CASE (RADM2_KPP,RADM2SORG_KPP,GOCARTRADM2,GOCARTRADM2_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=370.
                   chem(i,k,j,p_ch4)=1.7
                enddo
             enddo
          enddo
        endif
   END SELECT kpp_select
   endif
   aer_select: SELECT CASE(config_flags%chem_opt)
     CASE (RACMPM_KPP)
        if(config_flags%chem_in_opt == 0 )then
          if( .NOT. config_flags%restart ) then
            do l=numgas+1,num_chem
               do j=jts,jte
                  do k=kts,kte
                     do i=its,ite
                        chem(i,k,j,l)=1.
                     enddo
                  enddo
               enddo
            enddo
          endif
        endif
     CASE (GOCARTRACM_KPP,GOCARTRADM2,GOCARTRADM2_KPP)
       CALL wrf_debug(15,'call GOCARTRACM_KPP chem/aerosols initialization')
        ch_dust(:,:)=0.8D-9
        ch_ss(:,:)=1.
        if(config_flags%chem_in_opt == 0 )then
        if( .NOT. config_flags%restart )then
           do j=jts,jte
              do k=kts,kte
                 do i=its,ite
!                  do n=1,num_chem
!                    chem(i,k,j,n)=1.e-12
!                  enddo
                   chem(i,k,j,p_dms)=0.1e-6
                   chem(i,k,j,p_so2)=5.e-6
                   chem(i,k,j,p_sulf)=3.e-6
                   chem(i,k,j,p_msa)=0.1e-6
                   chem(i,k,j,p_bc1)=0.1e-3
                   chem(i,k,j,p_bc2)=0.1e-3
                   chem(i,k,j,p_oc1)=0.1e-3
                   chem(i,k,j,p_oc2)=0.1e-3
                   chem(i,k,j,p_p25)=1.
                 enddo
              enddo
           enddo
         endif
         endif
     CASE (GOCART_SIMPLE)
       if(config_flags%phot_opt .NE. 0 )then
         call wrf_error_fatal("GOCART simple initialization, phot_opt  MUST BE ZERO")
!        config_flags%phot_opt  = 0
       endif
       CALL wrf_debug(15,'call GOCART chem/aerosols initialization')
        ch_dust(:,:)=0.8D-9
        ch_ss(:,:)=1.
        if( .NOT. config_flags%restart )then
        if(config_flags%chem_in_opt == 0 )then
           do j=jts,jte
              do k=kts,kte
                 do i=its,ite
                   do n=1,num_chem
                     chem(i,k,j,n)=1.e-12
                   enddo
                   chem(i,k,j,p_dms)=0.1e-6
                   chem(i,k,j,p_so2)=5.e-6
                   chem(i,k,j,p_sulf)=3.e-6
                   chem(i,k,j,p_msa)=0.1e-6
                   chem(i,k,j,p_bc1)=0.1e-3
                   chem(i,k,j,p_bc2)=0.1e-3
                   chem(i,k,j,p_oc1)=0.1e-3
                   chem(i,k,j,p_oc2)=0.1e-3
                   chem(i,k,j,p_p25)=1.
                 enddo
              enddo
           enddo
         endif
!
! next is done to scale background oh and no3 in dependence on average zenith angle and day/night for no3
! this is done since background values are only available as average/month. It will not be necessary if other
! chemistry packages are used that provide oh,no3,h2o2
!
         endif
           ndystep=86400/ifix(dt)
           do j=jts,jte
                 do i=its,ite
                   tcosz(i,j)=0.
                   ttday(i,j)=0.
                   rlat=xlat(i,j)*3.1415926535590/180.
                   xlonn=xlong(i,j)
                   do n=1,ndystep
                      xtime=n*dt/60.
                      ixhour=ifix(gmt+.01)+ifix(xtime/60.)
                      xhour=float(ixhour)
                      xmin=60.*gmt+(xtime-xhour*60.)
                      gmtp=mod(xhour,24.)
                      gmtp=gmtp+xmin/60.
                      CALL szangle(1, 1, julday, gmtp, sza, cosszax,xlonn,rlat)
                      TCOSZ(i,j)=TCOSZ(I,J)+cosszax(1,1) 
                      if(cosszax(1,1).gt.0.)ttday(i,j)=ttday(i,j)+dt
                    enddo
                 if(i.eq.19.and.j.eq.19)write(0,*)'in cheminit'
                 if(i.eq.19.and.j.eq.19)write(0,*)TCOSZ(i,j),ttday(i,j),julday, gmtp, sza, cosszax,xlonn,rlat
           enddo
          enddo
     CASE (RADM2SORG, RADM2SORG_AQ, RACMSORG_AQ, RADM2SORG_KPP, RACMSORG_KPP)
       CALL wrf_debug(15,'call MADE/SORGAM aerosols initialization')

       call aerosols_sorgam_init(chem,convfac,z_at_w,                &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               chem_in_opt,config_flags%aer_ic_opt,is_aerosol,       &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte, config_flags               )
!...Convert aerosols to mixing ratio
        if( .NOT. config_flags%restart ) then
        if(config_flags%chem_in_opt == 0 .and. num_chem.gt.numgas)then
        do l=numgas+1,num_chem
           do j=jts,jte
              do k=kts,kte
                 kk = min(k,kde-1)
                 do i=its,ite
                    chem(i,k,j,l)=chem(i,kk,j,l)*alt(i,kk,j)
                 enddo
              enddo
           enddo
        enddo
        endif
        endif
        chem(its:ite,kts:min(kte,kde-1),jts:jte,:)=max(chem(its:ite,kts:min(kte,kde-1),jts:jte,:),epsilc)


     CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       call wrf_debug(15,'call MOSAIC aerosols initialization')
       call init_data_mosaic_asect(is_aerosol)
       if(config_flags%chem_in_opt == 0 )then
       if( .NOT. config_flags%restart ) &
            call mosaic_init_wrf_mixrats(                            &
               0, config_flags,                                      &
               chem, alt, z_at_w, g,                                 &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
       endif

   END SELECT aer_select

   progn_sanity_check : SELECT CASE(config_flags%chem_opt)
   CASE (RADM2SORG_AQ, RACMSORG_AQ, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
      if( config_flags%progn /= 1 ) &
           call wrf_error_fatal( &
           "ERROR: When using a ..._AQ chemistry package, progn must be 1")
   END SELECT progn_sanity_check

   do nv=1,num_chem
      do j=jts,jte
         do i=its,ite
            chem(i,kde,j,nv)=chem(i,kde-1,j,nv)
         enddo
      enddo
   enddo

!
! initialization for wesely (gas) dry deposition
!
   drydep_select: SELECT CASE(config_flags%drydep_opt)
     CASE (WESELY)
       CALL wrf_debug(15,'initializing dry dep (wesely)')
       call dep_init(id,config_flags,numgas)

   END SELECT drydep_select
!
! initialization for cbmz gas-phase chemistry
!
   cbmz_select: SELECT CASE(config_flags%chem_opt)
     CASE (CBMZ, CBMZ_BB, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, &
           CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       CALL wrf_debug(15,'initializing cbmz gas-phase chemistry')
       if(config_flags%chem_in_opt == 0 )then
       if( .NOT. config_flags%restart ) &
            call cbmz_init_wrf_mixrats(config_flags,   &
               z_at_w, g,                              &
               chem, numgas,                           &
               ids,ide, jds,jde, kds,kde,              &
               ims,ime, jms,jme, kms,kme,              &
               its,ite, jts,jte, kts,kte               )
       endif
   END SELECT cbmz_select
!
! initialization for cb4 gas-phase chemistry
!
     cb4_select: SELECT CASE(config_flags%chem_opt)
       CASE (CBM4_KPP)
         CALL wrf_debug(15,'initializing cbm4 gas-phase chemistry')
         if(config_flags%chem_in_opt == 0 )then
         if( .NOT. config_flags%restart ) &
              call cbm4_init_wrf_mixrats(config_flags,   &
                 z_at_w, g,                              &
                 chem, numgas,                           &
                 ids,ide, jds,jde, kds,kde,              &
                 ims,ime, jms,jme, kms,kme,              &
                 its,ite, jts,jte, kts,kte               )
         endif
     END SELECT cb4_select
!
! Initialize the cloud droplet number and mass
!
    if( (.not.config_flags%restart) .and. (config_flags%progn > 0) ) then
       call mosaic_mixactivate_init(                   &
            config_flags, chem, scalar,                &
            ims,ime, jms,jme, kms,kme,                 &
            its,ite, jts,jte, kts,kte                  )
    end if
!
! Setup time info for chemistry
!
    if( config_flags%restart ) then
       call wrf_debug( 15, "Setting last_chem_time from restart file" )
! Do not know if I need the following nl_gets.
! domain_get_sim_start_time uses them for simulation_start_...
!          CALL nl_get_start_year   ( 1, last_chem_time_year   )
!          CALL nl_get_start_month  ( 1, last_chem_time_month  )
!          CALL nl_get_start_day    ( 1, last_chem_time_day    )
!          CALL nl_get_start_hour   ( 1, last_chem_time_hour   )
!          CALL nl_get_start_minute ( 1, last_chem_time_minute )
!          CALL nl_get_start_second ( 1, last_chem_time_second )
       call WRFU_TimeSet( last_chem_time(id),         &
                          YY = last_chem_time_year,   &
                          MM = last_chem_time_month,  &
                          DD = last_chem_time_day,    &
                          H  = last_chem_time_hour,   &
                          M  = last_chem_time_minute, &
                          S  = last_chem_time_second  )
    else
       call wrf_debug( 15, "Setting last_chem_time to model start time-dt" )
       call WRFU_TimeIntervalSet(tmpTimeInterval, s_=real(dt,8))
       last_chem_time(id) = domain_get_current_time(grid) - tmpTimeInterval
    end if

    END SUBROUTINE chem_init

!-----------------------------------------------------------------------
#ifdef CHEM_DBG_I
subroutine print_chem_species_index( chem_opt )
  use module_state_description
  implicit none

  integer, intent(in) :: chem_opt

  print*,'Index numbers for chemistry species:'
!
! Gas species...
!
  select case (chem_opt)
  case (RADM2, RADM2SORG, RADM2SORG_AQ, RADM2_KPP, RADM2SORG_KPP, GOCARTRADM2,GOCARTRADM2_KPP)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
  case (RACMSORG_AQ, RACM_KPP, RACMPM_KPP, RACMSORG_KPP)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ete,"ete"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_co2,"co2"
     print*,p_ch4,"ch4"
     print*,p_udd,"udd"
     print*,p_hket,"hket"
     print*,p_api,"api"
     print*,p_lim,"lim"
     print*,p_dien,"dien"
     print*,p_macr,"macr"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
  case (CBMZ)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
     print*,p_hcl,"hcl"
     print*,p_ch3o2,"ch3o2"
     print*,p_ethp,"ethp"
     print*,p_ch3oh,"ch3oh"
     print*,p_c2h5oh,"c2h5oh"
     print*,p_par,"par"
     print*,p_to2,"to2"
     print*,p_cro,"cro"
     print*,p_open,"open"
     print*,p_op3,"op3"
     print*,p_c2o3,"c2o3"
     print*,p_ro2,"ro2"
     print*,p_ano2,"ano2"
     print*,p_nap,"nap"
     print*,p_xo2,"xo2"
     print*,p_xpar,"xpar"
     print*,p_isoprd,"isoprd"
     print*,p_isopp,"isopp"
     print*,p_isopn,"isopn"
     print*,p_isopo2,"isopo2"
     print*,p_dms,"dms"
     print*,p_msa,"msa"
     print*,p_dmso,"dmso"
     print*,p_dmso2,"dmso2"
     print*,p_ch3so2h,"ch3so2h"
     print*,p_ch3sch2oo,"ch3sch2oo"
     print*,p_ch3so2,"ch3so2"
     print*,p_ch3so3,"ch3so3"
     print*,p_ch3so2oo,"ch3so2oo"
     print*,p_ch3so2ch2oo,"ch3so2ch2oo"
     print*,p_mtf,"mtf"
  case (CBMZ_BB, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
     print*,p_so2,"so2"
     print*,p_sulf,"sulf"
     print*,p_no2,"no2"
     print*,p_no,"no"
     print*,p_o3,"o3"
     print*,p_hno3,"hno3"
     print*,p_h2o2,"h2o2"
     print*,p_ald,"ald"
     print*,p_hcho,"hcho"
     print*,p_op1,"op1"
     print*,p_op2,"op2"
     print*,p_paa,"paa"
     print*,p_ora1,"ora1"
     print*,p_ora2,"ora2"
     print*,p_nh3,"nh3"
     print*,p_n2o5,"n2o5"
     print*,p_no3,"no3"
     print*,p_pan,"pan"
     print*,p_hc3,"hc3"
     print*,p_hc5,"hc5"
     print*,p_hc8,"hc8"
     print*,p_eth,"eth"
     print*,p_co,"co"
     print*,p_ol2,"ol2"
     print*,p_olt,"olt"
     print*,p_oli,"oli"
     print*,p_tol,"tol"
     print*,p_xyl,"xyl"
     print*,p_aco3,"aco3"
     print*,p_tpan,"tpan"
     print*,p_hono,"hono"
     print*,p_hno4,"hno4"
     print*,p_ket,"ket"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_dcb,"dcb"
     print*,p_onit,"onit"
     print*,p_csl,"csl"
     print*,p_iso,"iso"
     print*,p_ho,"ho"
     print*,p_ho2,"ho2"
     print*,p_hcl,"hcl"
     print*,p_ch3o2,"ch3o2"
     print*,p_ethp,"ethp"
     print*,p_ch3oh,"ch3oh"
     print*,p_c2h5oh,"c2h5oh"
     print*,p_par,"par"
     print*,p_to2,"to2"
     print*,p_cro,"cro"
     print*,p_open,"open"
     print*,p_op3,"op3"
     print*,p_c2o3,"c2o3"
     print*,p_ro2,"ro2"
     print*,p_ano2,"ano2"
     print*,p_nap,"nap"
     print*,p_xo2,"xo2"
     print*,p_xpar,"xpar"
     print*,p_isoprd,"isoprd"
     print*,p_isopp,"isopp"
     print*,p_isopn,"isopn"
     print*,p_isopo2,"isopo2"
  end select
!
! Aerosol species...
!
  select case (chem_opt)
  case (RADM2SORG, RADM2SORG_AQ, RADM2SORG_KPP, RACMSORG, RACMSORG_AQ, RACMSORG_KPP)
     print*,p_so4aj,"so4aj"
     print*,p_so4ai,"so4ai"
     print*,p_nh4aj,"nh4aj"
     print*,p_nh4ai,"nh4ai"
     print*,p_no3aj,"no3aj"
     print*,p_no3ai,"no3ai"
     print*,p_orgaro1j,"orgaro1j"
     print*,p_orgaro1i,"orgaro1i"
     print*,p_orgaro2j,"orgaro2j"
     print*,p_orgaro2i,"orgaro2i"
     print*,p_orgalk1j,"orgalk1j"
     print*,p_orgalk1i,"orgalk1i"
     print*,p_orgole1j,"orgole1j"
     print*,p_orgole1i,"orgole1i"
     print*,p_orgba1j,"orgba1j"
     print*,p_orgba1i,"orgba1i"
     print*,p_orgba2j,"orgba2j"
     print*,p_orgba2i,"orgba2i"
     print*,p_orgba3j,"orgba3j"
     print*,p_orgba3i,"orgba3i"
     print*,p_orgba4j,"orgba4j"
     print*,p_orgba4i,"orgba4i"
     print*,p_orgpaj,"orgpaj"
     print*,p_orgpai,"orgpai"
     print*,p_ecj,"ecj"
     print*,p_eci,"eci"
     print*,p_p25j,"p25j"
     print*,p_p25i,"p25i"
     print*,p_antha,"antha"
     print*,p_seas,"seas"
     print*,p_soila,"soila"
     print*,p_nu0,"nu0"
     print*,p_ac0,"ac0"
     print*,p_corn,"corn"
  case (CBMZ_MOSAIC_4BIN)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
  case (CBMZ_MOSAIC_8BIN)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
     print*,p_so4_a05,"so4_a05"
     print*,p_no3_a05,"no3_a05"
     print*,p_cl_a05,"cl_a05"
     print*,p_nh4_a05,"nh4_a05"
     print*,p_na_a05,"na_a05"
     print*,p_oin_a05,"oin_a05"
     print*,p_oc_a05,"oc_a05"
     print*,p_bc_a05,"bc_a05"
     print*,p_hysw_a05,"hysw_a05"
     print*,p_water_a05,"water_a05"
     print*,p_num_a05,"num_a05"
     print*,p_so4_a06,"so4_a06"
     print*,p_no3_a06,"no3_a06"
     print*,p_cl_a06,"cl_a06"
     print*,p_nh4_a06,"nh4_a06"
     print*,p_na_a06,"na_a06"
     print*,p_oin_a06,"oin_a06"
     print*,p_oc_a06,"oc_a06"
     print*,p_bc_a06,"bc_a06"
     print*,p_hysw_a06,"hysw_a06"
     print*,p_water_a06,"water_a06"
     print*,p_num_a06,"num_a06"
     print*,p_so4_a07,"so4_a07"
     print*,p_no3_a07,"no3_a07"
     print*,p_cl_a07,"cl_a07"
     print*,p_nh4_a07,"nh4_a07"
     print*,p_na_a07,"na_a07"
     print*,p_oin_a07,"oin_a07"
     print*,p_oc_a07,"oc_a07"
     print*,p_bc_a07,"bc_a07"
     print*,p_hysw_a07,"hysw_a07"
     print*,p_water_a07,"water_a07"
     print*,p_num_a07,"num_a07"
     print*,p_so4_a08,"so4_a08"
     print*,p_no3_a08,"no3_a08"
     print*,p_cl_a08,"cl_a08"
     print*,p_nh4_a08,"nh4_a08"
     print*,p_na_a08,"na_a08"
     print*,p_oin_a08,"oin_a08"
     print*,p_oc_a08,"oc_a08"
     print*,p_bc_a08,"bc_a08"
     print*,p_hysw_a08,"hysw_a08"
     print*,p_water_a08,"water_a08"
     print*,p_num_a08,"num_a08"
  case (CBMZ_MOSAIC_4BIN_AQ)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
     print*,p_so4_cw01,"so4_cw01"
     print*,p_no3_cw01,"no3_cw01"
     print*,p_cl_cw01,"cl_cw01"
     print*,p_nh4_cw01,"nh4_cw01"
     print*,p_na_cw01,"na_cw01"
     print*,p_oin_cw01,"oin_cw01"
     print*,p_oc_cw01,"oc_cw01"
     print*,p_bc_cw01,"bc_cw01"
     print*,p_num_cw01,"num_cw01"
     print*,p_so4_cw02,"so4_cw02"
     print*,p_no3_cw02,"no3_cw02"
     print*,p_cl_cw02,"cl_cw02"
     print*,p_nh4_cw02,"nh4_cw02"
     print*,p_na_cw02,"na_cw02"
     print*,p_oin_cw02,"oin_cw02"
     print*,p_oc_cw02,"oc_cw02"
     print*,p_bc_cw02,"bc_cw02"
     print*,p_num_cw02,"num_cw02"
     print*,p_so4_cw03,"so4_cw03"
     print*,p_no3_cw03,"no3_cw03"
     print*,p_cl_cw03,"cl_cw03"
     print*,p_nh4_cw03,"nh4_cw03"
     print*,p_na_cw03,"na_cw03"
     print*,p_oin_cw03,"oin_cw03"
     print*,p_oc_cw03,"oc_cw03"
     print*,p_bc_cw03,"bc_cw03"
     print*,p_num_cw03,"num_cw03"
     print*,p_so4_cw04,"so4_cw04"
     print*,p_no3_cw04,"no3_cw04"
     print*,p_cl_cw04,"cl_cw04"
     print*,p_nh4_cw04,"nh4_cw04"
     print*,p_na_cw04,"na_cw04"
     print*,p_oin_cw04,"oin_cw04"
     print*,p_oc_cw04,"oc_cw04"
     print*,p_bc_cw04,"bc_cw04"
     print*,p_num_cw04,"num_cw04"

  case (CBMZ_MOSAIC_8BIN_AQ)
     print*,p_so4_a01,"so4_a01"
     print*,p_no3_a01,"no3_a01"
     print*,p_cl_a01,"cl_a01"
     print*,p_nh4_a01,"nh4_a01"
     print*,p_na_a01,"na_a01"
     print*,p_oin_a01,"oin_a01"
     print*,p_oc_a01,"oc_a01"
     print*,p_bc_a01,"bc_a01"
     print*,p_hysw_a01,"hysw_a01"
     print*,p_water_a01,"water_a01"
     print*,p_num_a01,"num_a01"
     print*,p_so4_a02,"so4_a02"
     print*,p_no3_a02,"no3_a02"
     print*,p_cl_a02,"cl_a02"
     print*,p_nh4_a02,"nh4_a02"
     print*,p_na_a02,"na_a02"
     print*,p_oin_a02,"oin_a02"
     print*,p_oc_a02,"oc_a02"
     print*,p_bc_a02,"bc_a02"
     print*,p_hysw_a02,"hysw_a02"
     print*,p_water_a02,"water_a02"
     print*,p_num_a02,"num_a02"
     print*,p_so4_a03,"so4_a03"
     print*,p_no3_a03,"no3_a03"
     print*,p_cl_a03,"cl_a03"
     print*,p_nh4_a03,"nh4_a03"
     print*,p_na_a03,"na_a03"
     print*,p_oin_a03,"oin_a03"
     print*,p_oc_a03,"oc_a03"
     print*,p_bc_a03,"bc_a03"
     print*,p_hysw_a03,"hysw_a03"
     print*,p_water_a03,"water_a03"
     print*,p_num_a03,"num_a03"
     print*,p_so4_a04,"so4_a04"
     print*,p_no3_a04,"no3_a04"
     print*,p_cl_a04,"cl_a04"
     print*,p_nh4_a04,"nh4_a04"
     print*,p_na_a04,"na_a04"
     print*,p_oin_a04,"oin_a04"
     print*,p_oc_a04,"oc_a04"
     print*,p_bc_a04,"bc_a04"
     print*,p_hysw_a04,"hysw_a04"
     print*,p_water_a04,"water_a04"
     print*,p_num_a04,"num_a04"
     print*,p_so4_a05,"so4_a05"
     print*,p_no3_a05,"no3_a05"
     print*,p_cl_a05,"cl_a05"
     print*,p_nh4_a05,"nh4_a05"
     print*,p_na_a05,"na_a05"
     print*,p_oin_a05,"oin_a05"
     print*,p_oc_a05,"oc_a05"
     print*,p_bc_a05,"bc_a05"
     print*,p_hysw_a05,"hysw_a05"
     print*,p_water_a05,"water_a05"
     print*,p_num_a05,"num_a05"
     print*,p_so4_a06,"so4_a06"
     print*,p_no3_a06,"no3_a06"
     print*,p_cl_a06,"cl_a06"
     print*,p_nh4_a06,"nh4_a06"
     print*,p_na_a06,"na_a06"
     print*,p_oin_a06,"oin_a06"
     print*,p_oc_a06,"oc_a06"
     print*,p_bc_a06,"bc_a06"
     print*,p_hysw_a06,"hysw_a06"
     print*,p_water_a06,"water_a06"
     print*,p_num_a06,"num_a06"
     print*,p_so4_a07,"so4_a07"
     print*,p_no3_a07,"no3_a07"
     print*,p_cl_a07,"cl_a07"
     print*,p_nh4_a07,"nh4_a07"
     print*,p_na_a07,"na_a07"
     print*,p_oin_a07,"oin_a07"
     print*,p_oc_a07,"oc_a07"
     print*,p_bc_a07,"bc_a07"
     print*,p_hysw_a07,"hysw_a07"
     print*,p_water_a07,"water_a07"
     print*,p_num_a07,"num_a07"
     print*,p_so4_a08,"so4_a08"
     print*,p_no3_a08,"no3_a08"
     print*,p_cl_a08,"cl_a08"
     print*,p_nh4_a08,"nh4_a08"
     print*,p_na_a08,"na_a08"
     print*,p_oin_a08,"oin_a08"
     print*,p_oc_a08,"oc_a08"
     print*,p_bc_a08,"bc_a08"
     print*,p_hysw_a08,"hysw_a08"
     print*,p_water_a08,"water_a08"
     print*,p_num_a08,"num_a08"
  
     print*,p_so4_cw01,"so4_cw01"
     print*,p_no3_cw01,"no3_cw01"
     print*,p_cl_cw01,"cl_cw01"
     print*,p_nh4_cw01,"nh4_cw01"
     print*,p_na_cw01,"na_cw01"
     print*,p_oin_cw01,"oin_cw01"
     print*,p_oc_cw01,"oc_cw01"
     print*,p_bc_cw01,"bc_cw01"
     print*,p_num_cw01,"num_cw01"
     print*,p_so4_cw02,"so4_cw02"
     print*,p_no3_cw02,"no3_cw02"
     print*,p_cl_cw02,"cl_cw02"
     print*,p_nh4_cw02,"nh4_cw02"
     print*,p_na_cw02,"na_cw02"
     print*,p_oin_cw02,"oin_cw02"
     print*,p_oc_cw02,"oc_cw02"
     print*,p_bc_cw02,"bc_cw02"
     print*,p_num_cw02,"num_cw02"
     print*,p_so4_cw03,"so4_cw03"
     print*,p_no3_cw03,"no3_cw03"
     print*,p_cl_cw03,"cl_cw03"
     print*,p_nh4_cw03,"nh4_cw03"
     print*,p_na_cw03,"na_cw03"
     print*,p_oin_cw03,"oin_cw03"
     print*,p_oc_cw03,"oc_cw03"
     print*,p_bc_cw03,"bc_cw03"
     print*,p_num_cw03,"num_cw03"
     print*,p_so4_cw04,"so4_cw04"
     print*,p_no3_cw04,"no3_cw04"
     print*,p_cl_cw04,"cl_cw04"
     print*,p_nh4_cw04,"nh4_cw04"
     print*,p_na_cw04,"na_cw04"
     print*,p_oin_cw04,"oin_cw04"
     print*,p_oc_cw04,"oc_cw04"
     print*,p_bc_cw04,"bc_cw04"
     print*,p_num_cw04,"num_cw04"
     print*,p_so4_cw05,"so4_cw05"
     print*,p_no3_cw05,"no3_cw05"
     print*,p_cl_cw05,"cl_cw05"
     print*,p_nh4_cw05,"nh4_cw05"
     print*,p_na_cw05,"na_cw05"
     print*,p_oin_cw05,"oin_cw05"
     print*,p_oc_cw05,"oc_cw05"
     print*,p_bc_cw05,"bc_cw05"
     print*,p_num_cw05,"num_cw05"
     print*,p_so4_cw06,"so4_cw06"
     print*,p_no3_cw06,"no3_cw06"
     print*,p_cl_cw06,"cl_cw06"
     print*,p_nh4_cw06,"nh4_cw06"
     print*,p_na_cw06,"na_cw06"
     print*,p_oin_cw06,"oin_cw06"
     print*,p_oc_cw06,"oc_cw06"
     print*,p_bc_cw06,"bc_cw06"
     print*,p_num_cw06,"num_cw06"
     print*,p_so4_cw07,"so4_cw07"
     print*,p_no3_cw07,"no3_cw07"
     print*,p_cl_cw07,"cl_cw07"
     print*,p_nh4_cw07,"nh4_cw07"
     print*,p_na_cw07,"na_cw07"
     print*,p_oin_cw07,"oin_cw07"
     print*,p_oc_cw07,"oc_cw07"
     print*,p_bc_cw07,"bc_cw07"
     print*,p_num_cw07,"num_cw07"
     print*,p_so4_cw08,"so4_cw08"
     print*,p_no3_cw08,"no3_cw08"
     print*,p_cl_cw08,"cl_cw08"
     print*,p_nh4_cw08,"nh4_cw08"
     print*,p_na_cw08,"na_cw08"
     print*,p_oin_cw08,"oin_cw08"
     print*,p_oc_cw08,"oc_cw08"
     print*,p_bc_cw08,"bc_cw08"
     print*,p_num_cw08,"num_cw08"
end select
end subroutine print_chem_species_index
#endif
