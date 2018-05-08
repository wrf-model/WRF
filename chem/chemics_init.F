!WRF:MODEL_LAYER:INITIALIZATION
!
   subroutine chem_init (id,chem,emis_ant,scalar,dt,bioemdt,photdt,chemdt,stepbioe, &
               stepphot,stepchem,stepfirepl,plumerisefire_frq,z_at_w,xlat,xlong,    &
               g,aerwrf,config_flags,grid,alt,t,p,CONVFAC,ttday,tcosz,julday,gmt,   &
               tauaer1,tauaer2,tauaer3,tauaer4,                      &
               gaer1,gaer2,gaer3,gaer4,                              &
               waer1,waer2,waer3,waer4,                              &
               l2aer,l3aer,l4aer,l5aer,l6aer,l7aer,                  &
               extaerlw1,extaerlw2,extaerlw3,extaerlw4,              &
               extaerlw5,extaerlw6,extaerlw7,extaerlw8,              &
               extaerlw9,extaerlw10,extaerlw11,extaerlw12,           &
               extaerlw13,extaerlw14,extaerlw15,extaerlw16,          &
               tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4,              &
               tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8,              &
               tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12,           &
               tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16,          &
               dgnum4d, dgnumwet4d, dgnum_a1, dgnum_a2, dgnum_a3,    & 
               dgnumwet_a1, dgnumwet_a2, dgnumwet_a3,                & 
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               tsoa,asoa,bsoa,                                       &
               last_chem_time_year, last_chem_time_month,            &
               last_chem_time_day, last_chem_time_hour,              &
               last_chem_time_minute, last_chem_time_second,         &
               chem_in_opt, kemit, num_vert_mix,                     &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

   USE module_domain
   USE module_configure
   USE module_state_description
   USE module_phot_mad

   USE module_ftuv_driver, only : ftuv_init
   USE module_phot_tuv,    only : tuv_init
   USE module_mozcart_wetscav, only : wetscav_mozcart_init
   USE module_aerosols_sorgam
   USE module_aerosols_soa_vbs, only: aerosols_soa_vbs_init
   USE module_aerosols_sorgam_vbs, only: aerosols_sorgam_vbs_init
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
                                          mozcart_lbc_init,          &
                                          last_chem_time,            &
                                          setup_gasprofile_maps,     &
                                          initial_pvo3

   USE module_mixactivate_wrappers, only: mosaic_mixactivate_init
   USE module_upper_bc_driver, only: upper_bc_init
   USE module_tropopause,      only: tropopause_init
   USE module_cam_mam_init, only: cam_mam_init
   USE module_cam_mam_initmixrats, only: cam_mam_initmixrats

   USE module_cam_mam_wetscav, only:wetscav_cam_mam_driver_init
   USE module_cam_support, only: numgas_mam, gas_pcnst_modal_aero,gas_pcnst_modal_aero_pos !BSINGH - Fix for non-MAM simulations
   USE module_HLawConst, only: init_HLawConst
   USE module_ctrans_grell, only: conv_tr_wetscav_init

!!! TUCCELLA (BUG)
   USE module_prep_wetscav_sorgam, only: aerosols_sorgam_init_aercld_ptrs, aerosols_soa_vbs_init_aercld_ptrs 

!!CYY
   USE module_model_constants, only:t0

   IMPLICIT NONE

   real  , intent(in) :: bioemdt,photdt,chemdt,dt,gmt
   INTEGER,      INTENT(IN   ) :: plumerisefire_frq
   INTEGER,      INTENT(IN   ) :: chem_in_opt
   INTEGER,      INTENT(INOUT) :: num_vert_mix
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
                               tsoa,asoa,bsoa,                       &
                               tauaer1,tauaer2,tauaer3,tauaer4,      &
                               extaerlw1,extaerlw2,extaerlw3,extaerlw4,      &
                               extaerlw5,extaerlw6,extaerlw7,extaerlw8,      &
                               extaerlw9,extaerlw10,extaerlw11,extaerlw12,   &
                               extaerlw13,extaerlw14,extaerlw15,extaerlw16,  &
                               tauaerlw1,tauaerlw2,tauaerlw3,tauaerlw4,      &
                               tauaerlw5,tauaerlw6,tauaerlw7,tauaerlw8,      &
                               tauaerlw9,tauaerlw10,tauaerlw11,tauaerlw12,   &
                               tauaerlw13,tauaerlw14,tauaerlw15,tauaerlw16,  &
                               gaer1,gaer2,gaer3,gaer4,              &
                               waer1,waer2,waer3,waer4
!-- Arrays needed to output dgnum when restarting
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme, 3 ) ,              &
          INTENT(INOUT ) ::                                         &
                               dgnum4d, dgnumwet4d
   REAL, DIMENSION( ims:ime , kms:kme , jms:jme ) ,                 &
          INTENT(INOUT ) ::                                         &
                               dgnum_a1, dgnum_a2, dgnum_a3,        &
                               dgnumwet_a1, dgnumwet_a2, dgnumwet_a3
!-- end dgnum restart arrays

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
                               ttday,tcosz,xlat,xlong
   real, INTENT (IN) :: g
   integer, intent(out) :: stepbioe,stepphot,stepchem,stepfirepl
   TYPE (grid_config_rec_type) , INTENT (in) ::     config_flags
   TYPE(domain) ,             INTENT (inout) ::     grid
   
   REAL,DIMENSION(ims:ime,kms:kme,jms:jme) :: si_zsigf, si_zsig

!
! local stuff
!
   CHARACTER*256 :: mminlu_loc
   CHARACTER*256 :: message_txt
   TYPE(WRFU_TimeInterval) :: tmpTimeInterval
   integer :: i,j,k,l,numgas,ixhour,n,ndystep,kk,nv
   real, DIMENSION (1,1) :: sza,cosszax
   real :: xtime,xhour,xmin,gmtp,xlonn,rlat
   logical :: is_moz_chm
#include "version_decl"
#ifdef CHEM_DBG_I
    call print_chem_species_index( config_flags%chem_opt )
#endif
   program_name = "*             PROGRAM:WRF-Chem " // TRIM(release_version) // " MODEL"

call wrf_message("*********************************************************************")
call wrf_message(program_name)
call wrf_message("*                                                                   *")
call wrf_message("*    PLEASE REPORT ANY BUGS TO WRF-Chem HELP at                     *")
call wrf_message("*                                                                   *")
call wrf_message("*              wrfchemhelp.gsd@noaa.gov                             *")
call wrf_message("*                                                                   *")
call wrf_message("*********************************************************************")

    numgas = get_last_gas(config_flags%chem_opt)
 
   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (GOCART_SIMPLE)
       CALL wrf_debug(15,'calling only gocart aerosols driver from chem_driver')
     CASE (GOCARTRADM2)
       CALL wrf_debug(15,'calling gocart and radm driver from chem_driver')
     CASE (GOCARTRACM_KPP)
       CALL wrf_debug(15,'calling gocart and racmkpp driver from chem_driver')
     CASE (CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
       CALL wrf_debug(15,'calling crimech driver from chem_driver')
       call wrf_message("WARNING: CRIMECH chemistry option is highly experimental and not recommended for use.")
!       call wrf_error_fatal("ERROR: experimental option selected, please contact G.MCFIGGANS for assistance")
     CASE (CBM4_KPP )
       CALL wrf_debug(15,'calling CB4 from chem_driver')
       call wrf_message("WARNING: CB4 chemistry option is highly experimental and not recommended for use.")
!      call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (NMHC9_KPP )
       CALL wrf_debug(15,'calling NMHC9 from chem_driver')
       call wrf_message("WARNING: NMHC9 chemistry option is highly experimental and not recommended for use.")
       call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (SAPRC99_KPP )
       CALL wrf_debug(15,'calling SAPRC99 from chem_driver')
       call wrf_message("WARNING: SAPRC99 chemistry option is highly experimental and not recommended for use.")
     CASE (SAPRC99_MOSAIC_4BIN_VBS2_KPP )
       CALL wrf_debug(15,'calling SAPRC99_MOSAIC_4BIN_VBS2_4BIN from chem_driver')
       call wrf_message("WARNING: SAPRC99_MOSAIC_4BIN_VBS2_4BIN chemistry option is highly experimental and not recommended for use.")
!jdf   call wrf_error_fatal("WARNING: SAPRC99_MOSAIC_4BIN_VBS2_4BIN chemistry option is useable but does not fully pass regtesting. It is recommended that you contact Jerome.Fast@pnl.gov for information regarding this option.")
       !BSINGH(04/03/2014): Added SAPRC 8 bin non-aq
    CASE (SAPRC99_MOSAIC_8BIN_VBS2_KPP )
       CALL wrf_debug(15,'calling SAPRC99_MOSAIC_8BIN_VBS2_4BIN from chem_driver')
       call wrf_message("WARNING: SAPRC99_MOSAIC_8BIN_VBS2_4BIN chemistry option is highly experimental and not recommended for use.")
       !BSINGH(12/03/2013): Added SAPRC 8 bin aq
    CASE (SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP )
       CALL wrf_debug(15,'calling SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP from chem_driver')
       call wrf_message("WARNING: SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP chemistry option is highly experimental and not recommended for use.")
       !BSINGH -ENDS
     CASE (MOZART_MOSAIC_4BIN_KPP )
       CALL wrf_debug(15,'calling MOZART_MOSAIC_4BIN_KPP from chem_driver')
     CASE (MOZART_MOSAIC_4BIN_AQ_KPP )
       CALL wrf_debug(15,'calling MOZART_MOSAIC_4BIN_AQ_KPP from chem_driver')
     CASE (RADM2SORG_AQ )
       CALL wrf_debug(15,'calling RADM2/MADE/SORGAM with AQ chemistry from chem_driver')
       call wrf_message("WARNING: RADM2SORG_AQ chemistry option is experimental and not yet fully tested.")
       call wrf_message("         We recommend contacting wrfchemhelp for assistance.")
     CASE (RACMSORG_AQ )
       CALL wrf_debug(15,'calling RACM/MADE/SORGAM with AQ chemistry from chem_driver')
       call wrf_message("WARNING: RACMSORG_AQ chemistry option is highly experimental and not recommended for use.")
       call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (RADM2SORG_AQCHEM )
       numgas_mam = numgas
       CALL wrf_debug(15,'calling RADM2/MADE/SORGAM with AQCHEM chemistry from chem_driver')
       call wrf_message("WARNING: RADM2SORG_AQCHEM chemistry option is experimental and not yet fully tested.")
       call wrf_message("         We recommend contacting wrfchemhelp for assistance.")
     CASE (RACMSORG_AQCHEM_KPP )
       numgas_mam = numgas
       CALL wrf_debug(15,'calling RACM/MADE/SORGAM with AQCHEM chemistry from chem_driver')
       call wrf_message("WARNING: RACMSORG_AQCHEM_KPP chemistry option is highly experimental and not recommended for use.")
!      call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (RACM_ESRLSORG_AQCHEM_KPP )
       numgas_mam = numgas
       CALL wrf_debug(15,'calling RACM/MADE/SORGAM with AQCHEM chemistry from chem_driver')
       call wrf_message("WARNING: RACM_ESRLSORG_AQCHEM_KPP chemistry option is highly experimental and not recommended for use.")
!      call wrf_error_fatal("ERROR: experimental option selected, please contact wrfchemhelp for assistance")
     CASE (RACM_SOA_VBS_AQCHEM_KPP )
       numgas_mam = numgas
       CALL wrf_debug(15,'calling RACM/MADE/SOA-VBS with AQCHEM chemistry from chem_driver')
!       call wrf_message("WARNING: RACM_SOA_VBS_AQCHEM_KPP chemistry option is highly experimental and not recommended for use.")
     CASE (CO2_TRACER, GHG_TRACER )
       call wrf_message("WARNING: Users interested in the GHG options should check the comments/references in header of module_ghg_fluxes")
     CASE (CBMZ_CAM_MAM3_NOAQ)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CBMZ_CAM_MAM3_NOAQ chemistry from chem_driver')
     CASE (CBMZ_CAM_MAM3_AQ)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CBMZ_CAM_MAM3_AQ with AQCHEM chemistry from chem_driver')
     CASE (CBMZ_CAM_MAM7_NOAQ)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CBMZ_CAM_MAM7_NOAQ chemistry from chem_driver')
        call wrf_message("WARNING: CBMZ_CAM_MAM7_NOAQ  chemistry option is highly experimental and not recommended for use.")
        call wrf_message("WARNING: In CBMZ_CAM_MAM7_NOAQ  chemistry option, DMS is not implemented yet.")
        call wrf_error_fatal("ERROR: It is recommended that you contact phil.rasch at pnnl.gov for information regarding this option")
     CASE (CBMZ_CAM_MAM7_AQ)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CBMZ_CAM_MAM7_AQ with AQCHEM chemistry from chem_driver')
        call wrf_message("WARNING: CBMZ_CAM_MAM7_AQ  chemistry option is highly experimental and not recommended for use.")
        call wrf_error_fatal("ERROR: It is recommended that you contact phil.rasch at pnnl.gov for information regarding this option")
     CASE(CB05_SORG_AQ_KPP)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CB05_SORG_AQ_KPP chemistry from chem_driver')
     CASE(CB05_SORG_VBS_AQ_KPP)
        numgas_mam = numgas
        CALL wrf_debug(15,'calling CB05_SORG_VBS_AQ_KPP chemistry from chem_driver')
   END SELECT chem_select

   if ( config_flags%dust_opt == 1 ) then
       call wrf_message("WARNING: dust option 1 currently works only with the GOCART aerosol option.")
   endif

   if ( config_flags%dust_opt == 2 ) then
       call wrf_error_fatal("WARNING: dust option 2 currently does not function properly and has been disabled.")
   endif

   if ( config_flags%seas_opt == 1 ) then
       call wrf_message("WARNING: sea salt option 1 currently works only with the GOCART aerosol option.")
   endif

   if ( config_flags%biomass_burn_opt > 0 .or. config_flags%biomass_burn_opt < 4 ) then
       CALL wrf_debug(15,'calling biomass burning')
   endif

   if ( config_flags%biomass_burn_opt == 5 ) then
       CALL wrf_debug(15,'calling biomass burning for GHGs')
   endif

   is_moz_chm = config_flags%chem_opt == MOZCART_KPP .or. &
                config_flags%chem_opt == T1_MOZCART_KPP .or. &
                config_flags%chem_opt == MOZART_KPP .or. &
                config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
                config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP

   if( is_moz_chm ) then
       write(message_txt,*) 'chem_init: calling mozcart_lbc_init for domain ',id
       call wrf_message( trim(message_txt) )
       call mozcart_lbc_init( chem, num_chem, id, &
                              ims, ime, jms, jme, kms, kme,    &
                              its, ite, jts, jte, kts )
   endif
!-----------------------------------------------------------------------
!  Check TUV photolysis for chem_opt compatability
!-----------------------------------------------------------------------
   if (config_flags%phot_opt == TUV ) then
     if (config_flags%chem_opt /= mozart_mosaic_4bin_kpp .and. &
         config_flags%chem_opt /= mozart_mosaic_4bin_aq_kpp .and. &
         config_flags%chem_opt /= mozcart_kpp .and. &
         config_flags%chem_opt /= t1_mozcart_kpp) THEN
       write(message_txt,'(''--- ERROR: chem_opt '',i3,'' not setup for TUV photolysis at this time'')') &
              config_flags%chem_opt
       CALL wrf_error_fatal( trim(message_txt) )
     endif
   endif

   if( is_moz_chm .and. id == 1 ) then
     write(message_txt,*) 'chem_init: calling init_HLawConst for domain ',id
     call wrf_message( trim(message_txt) )
!  Initialize Henrys Law constants
     call init_HLawConst( id )
     if( config_flags%conv_tr_wetscav == 1 ) then
!  Initialize Henrys Law constants
       call conv_tr_wetscav_init( numgas, num_chem )
     endif
   endif

!!! TUCCELLA
   if ( config_flags%wetscav_onoff == 1 ) then
     if( .not. is_moz_chm ) then
       if(  ( config_flags%chem_opt >= 8 .AND. config_flags%chem_opt <= 13) .OR.  &
            ( config_flags%chem_opt >= 31 .AND. config_flags%chem_opt <= 36) .OR. &
            ( config_flags%chem_opt >= 41 .AND. config_flags%chem_opt <= 43) .OR. &
            ( config_flags%chem_opt == 131 ) .OR. ( config_flags%chem_opt == 132 ) .OR. &
            ( config_flags%chem_opt == 503 .OR. config_flags%chem_opt == 504) .OR. &
            ( config_flags%chem_opt == 203).OR.                                    & !BSINGH(12/17/2013): Added for SAPRC 8 bin vbs
            ( config_flags%chem_opt == 601 .OR. config_flags%chem_opt == 611) .OR. &
              (config_flags%chem_opt == 109) ) then
         call wrf_debug( 15, 'Chemics_init: Wet scavenging turned on' )
       else
         call wrf_error_fatal("ERROR: wet scavenging option requires chem_opt = 8 through 13 or 31 to 36 or 41 to 42 or 109 or 503 or 504 or 601 or 611 to function.")
       endif
       if ( config_flags%mp_physics /= 2 .and. config_flags%mp_physics /= 10 .and. config_flags%mp_physics /= 11  &
            .and. config_flags%mp_physics /= 17 .and. config_flags%mp_physics /= 18 .and. config_flags%mp_physics /= 22) then
         call wrf_error_fatal("ERROR: wet scavenging option requires mp_phys = 2 (Lin et al.) or 10 (Morrison) or 11 (CAMMGMP) or 17/18/22 NSSL_2mom to function.")
       endif
     elseif( id == 1 ) then
       if ( config_flags%mp_physics /= 6 .and. config_flags%mp_physics /= 8 .and. config_flags%mp_physics /= 10 .and. config_flags%mp_physics /= 17  &
            .and. config_flags%mp_physics /= 18 .and. config_flags%mp_physics /= 22) then
         call wrf_error_fatal("ERROR: wet scavenging option for MOZART,MOZCART requires mp_phys = 6 (WSM6) or 8 (Thompson) or 10 (Morrison) .or 17/18/22 (NSSL 2-moment) to function.")
       else
         write(message_txt,*) 'chem_init: calling wetscav_mozcart_init for domain ',id
         call wrf_message( trim(message_txt) )
         call wetscav_mozcart_init( id, numgas, config_flags )
       endif
     endif
   endif

!!! TUCCELLA
   if ( config_flags%cldchem_onoff == 1 ) then
      if(  ( config_flags%chem_opt >= 8 .AND. config_flags%chem_opt <= 13)    .OR. &
           ( config_flags%chem_opt >= 31 .AND. config_flags%chem_opt <= 36)   .OR. &
           ( config_flags%chem_opt >= 501 .AND. config_flags%chem_opt <= 504) .OR. &
           ( config_flags%chem_opt >= 41 .AND. config_flags%chem_opt <= 43)  .OR. &
           ( config_flags%chem_opt == 203).OR.                                    &
           ( config_flags%chem_opt == 131 ) .OR. ( config_flags%chem_opt == 132 ) .OR. &
           ( config_flags%chem_opt >= 601 .AND. config_flags%chem_opt <= 611)  .OR. &
             config_flags%chem_opt == 109 .OR. config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
           call wrf_debug( 15, 'Chemics_init: Cloud chemistry turned on' )
      else
        call wrf_error_fatal("ERROR: cloud chemistry option requires chem_opt = 8 through 13 or 31 to 36 or 41 to 43 or 109 to function.")
      endif
       if ( config_flags%mp_physics /= 2 .and. config_flags%mp_physics /= 10 .and. config_flags%mp_physics /= 11  &
            .and. config_flags%mp_physics /= 17 .and. config_flags%mp_physics /= 18 .and. config_flags%mp_physics /= 22 ) then
         call wrf_error_fatal("ERROR: cloud chemistry option requires mp_phys = 2 (Lin et al.) or 10 (Morrison) or 11 (CAMMGMP) or 17/18/22 NSSL_2mom to function.")
       endif
   endif

   !BSINGH - kfcup schme only works with Mosaic aqueoue packages:
   ! *** NOTE ***
   ! KFCUP should in theory work with any chem_opt package that uses MOSAIC and has cloud-borne aerosols (*_aq*).
   ! However, it was only tested with chem_opt=203 (saprc99_mosaic_8bin_vbs2_aq_kpp) 
   ! during implementation into WRF-Chem in April 2017 at PNNL.   
   if ( config_flags%cu_physics == 10)  then
      if(  config_flags%chem_opt /= 9   .and.  config_flags%chem_opt /= 10  .and. &
           config_flags%chem_opt /= 32  .and.  config_flags%chem_opt /= 34  .and.  &
           config_flags%chem_opt /= 202 .and.  config_flags%chem_opt /= 203 .and.  &
           config_flags%chem_opt /= 601 .and.  config_flags%chem_opt /= 611 ) then
         call wrf_error_fatal("ERROR: kfcupscheme requires chem_opt = 9, 10, 32, 34, 202, 203, 601 or 611  to function.")
      endif
   endif


   if ( config_flags%cu_physics == 5 .OR. config_flags%cu_physics == 3) then
   if ( config_flags%cu_diag == 0) then
         call wrf_message(" No time averaged variables. Time averaged chem variables requires cu_diag = 1")
   endif
   endif

   if ( config_flags%cu_diag == 1) then
   if ( config_flags%cu_physics /= 3 .AND. config_flags%cu_physics /= 5 .AND. config_flags%cu_physics /= 10) then !BSINGH(12/17/2013): Added for WRFCUP scheme
         call wrf_error_fatal(" Time averaged variables (cu_diag = 1) requires cu_physics = 3 or 5 or 10")
   endif
   endif

   if ( config_flags%chem_opt /= 0 .AND. &
      ( config_flags%chem_conv_tr == 1 .and. config_flags%cu_diag == 0) ) then
         call wrf_error_fatal("ERROR: chem_conv_tr=1 requires cu_diag=1")
   endif

   if ( config_flags%cu_physics == 10 .and. config_flags%kfcup_diag == 0) then
      call wrf_error_fatal("ERROR: cu_physics == 10 requires kfcup_diag == 1")
   endif

    if ( config_flags%chem_conv_tr == 0 .and. config_flags%kfcup_diag == 1) then
      call wrf_error_fatal("ERROR: kfcup_diag == 1 requires chem_conv_tr == 1")
   endif

   if ( config_flags%cu_physics /= 10 .and. config_flags%kfcup_diag == 1) then
      call wrf_error_fatal("ERROR: kfcup_diag == 1 requires cu_physics == 10")
   endif


   if ( config_flags%bio_emiss_opt .EQ. 3 .AND. config_flags%ne_area .LT. num_chem ) then
      write(message_txt,'(''ERROR: MEGAN biogenics requires ne_area('',i6,'') >= num_chem('',i6,'')'')') config_flags%ne_area,num_chem
!     call wrf_error_fatal("ERROR: MEGAN biogenics requires ne_area to be equal or greater than num_chem")
      call wrf_error_fatal( trim(message_txt) )
   endif

    IF ( config_flags%chem_opt == 0 .AND. config_flags%aer_ra_feedback .NE. 0 ) THEN
!       config_flags%aer_ra_feedback = 0
        call wrf_error_fatal(" ERROR: CHEM_INIT: FOR CHEM_OPT = 0, AER_RA_FEEDBACK MUST = 0 ")
    ENDIF

    IF ( config_flags%calc_clean_atm_diag .gt. 0 .AND. config_flags%aer_ra_feedback .EQ. 0 ) THEN
    	call wrf_error_fatal("ERROR: clean_atm_diag > 0 requires aer_ra_feedback > 0")
    ENDIF

    IF ( config_flags%aer_ra_feedback .EQ. 1   .AND.   &
        ( config_flags%chem_opt == RADM2 .or. &
          config_flags%chem_opt == CBMZ .or. &
          config_flags%chem_opt == CBMZ_BB .or. &
          config_flags%chem_opt == CO2_TRACER .or. &
          config_flags%chem_opt == RADM2_KPP .or. &
          config_flags%chem_opt == RACM_MIM_KPP .or. &
          config_flags%chem_opt == RACM_KPP .or. &
          config_flags%chem_opt == CBM4_KPP .or. &
          config_flags%chem_opt == SAPRC99_KPP  .or. &
          config_flags%chem_opt == NMHC9_KPP  ) ) then
        call wrf_error_fatal(" ERROR: CHEM_INIT: MUST HAVE AEROSOLS TO INCLUDE AEROSOL RADIATION FEEDBACK. SET AER_RA_FEEDBACK = 0 ")
    ENDIF

    if ( config_flags%n2o5_hetchem == 1 )then 
        if( (config_flags%chem_opt >= 7   .AND. config_flags%chem_opt <= 10)  .OR. &
            (config_flags%chem_opt >= 31  .AND. config_flags%chem_opt <= 34)  .OR. &
             config_flags%chem_opt == 170 .OR.  config_flags%chem_opt == 198  .OR. &
             config_flags%chem_opt == 199 .OR.  config_flags%chem_opt == 201  .OR. &
             config_flags%chem_opt == 203 .OR.                                     &
             config_flags%chem_opt == 601 .OR.  config_flags%chem_opt == 611 ) then
            call wrf_debug( 15, 'using N2O5 heterogeneous chemistry without Cl- pathway')
        else
            call wrf_error_fatal("ERROR: N2O5 heterogenous chemistry (without Cl- pathway) must be run with MOSAIC aerosol")
        endif
    elseif ( config_flags%n2o5_hetchem == 2 ) then
        if( config_flags%chem_opt == 601 .OR. config_flags%chem_opt == 611 ) then
            call wrf_debug( 15, 'using full N2O5 heterogeneous chemistry')
        else
            call wrf_error_fatal("ERROR: full N2O5 heterogenous chemistry must be run with MOSAIC aerosol coupled with gas-phase scheme which deals with ClNO2")
        endif
    endif

!   IF ( config_flags%chem_opt == 2 .AND. config_flags%dust_opt .NE. 2 ) THEN
!       call wrf_error_fatal(" ERROR: USE dust_opt = 2 when using MADE/SORGAM aerosol option ")
!   ENDIF

    CALL nl_get_mminlu( 1, mminlu_loc )

    IF (trim(mminlu_loc) /= 'USGS' .and. trim(mminlu_loc) /= 'MODIFIED_IGBP_MODIS_NOAH' ) THEN
      print*,mminlu_loc
      message_txt = " ERROR: CHEM_INIT: Chemistry routines require USGS or MODIS_NOAH land use maps. Need to change land use option."
      call wrf_error_fatal( trim(message_txt) )
    ELSE
      IF (trim(mminlu_loc) == 'USGS' .and. grid%num_land_cat <= 23 ) THEN
        message_txt = " ERROR: CHEM_INIT: USGS land use map should have 24 or more catagories."
        call wrf_error_fatal( trim(message_txt) )
      ELSEIF (trim(mminlu_loc) == 'MODIFIED_IGBP_MODIS_NOAH' .and. grid%num_land_cat <= 19 ) THEN
        message_txt = " ERROR: CHEM_INIT: MODIS_NOAH land use map should have 20 or more catagories."
        call wrf_error_fatal( trim(message_txt) )
      ENDIF
    ENDIF
!--
    
!-- Load dgnum arrays when restart is active
IF ( config_flags%restart ) THEN
       do j=jts,jte
          do k=kts,kte
             do i=its,ite
                dgnum4d(i, k, j, 1) = dgnum_a1(i, k, j)
                dgnum4d(i, k, j, 2) = dgnum_a2(i, k, j)
                dgnum4d(i, k, j, 3) = dgnum_a3(i, k, j)
                
                dgnumwet4d(i, k, j, 1) = dgnumwet_a1(i, k, j)
                dgnumwet4d(i, k, j, 2) = dgnumwet_a2(i, k, j)
                dgnumwet4d(i, k, j, 3) = dgnumwet_a3(i, k, j)
             end do
          end do
       end do
ENDIF
!-- end load dgnum arrays

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
                extaerlw1(i,k,j) = 0.
                extaerlw2(i,k,j) = 0.
                extaerlw3(i,k,j) = 0.
                extaerlw4(i,k,j) = 0.
                extaerlw5(i,k,j) = 0.
                extaerlw6(i,k,j) = 0.
                extaerlw7(i,k,j) = 0.
                extaerlw8(i,k,j) = 0.
                extaerlw9(i,k,j) = 0.
                extaerlw10(i,k,j) = 0.
                extaerlw11(i,k,j) = 0.
                extaerlw12(i,k,j) = 0.
                extaerlw13(i,k,j) = 0.
                extaerlw14(i,k,j) = 0.
                extaerlw15(i,k,j) = 0.
                extaerlw16(i,k,j) = 0.
                tauaerlw1(i,k,j) = 0.
                tauaerlw2(i,k,j) = 0.
                tauaerlw3(i,k,j) = 0.
                tauaerlw4(i,k,j) = 0.
                tauaerlw5(i,k,j) = 0.
                tauaerlw6(i,k,j) = 0.
                tauaerlw7(i,k,j) = 0.
                tauaerlw8(i,k,j) = 0.
                tauaerlw9(i,k,j) = 0.
                tauaerlw10(i,k,j) = 0.
                tauaerlw11(i,k,j) = 0.
                tauaerlw12(i,k,j) = 0.
                tauaerlw13(i,k,j) = 0.
                tauaerlw14(i,k,j) = 0.
                tauaerlw15(i,k,j) = 0.
                tauaerlw16(i,k,j) = 0.
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
! Read VPRM parameters and put them into wrfinput file
!     for diagnostics purposes only!
!write(6,*)'config_flags%chem_opt, CO2_TRACER', config_flags%chem_opt, CO2_TRACER

if  ( config_flags%chem_opt==CO2_TRACER .OR. config_flags%chem_opt==GHG_TRACER )   then
     call VPRM_par_initialize ( grid%rad_vprm, grid%lambda_vprm, grid%alpha_vprm, grid%resp_vprm, config_flags )
     call wrf_message("Warning: the VPRM parameters may need to be optimized depending on the season, year and region!")
     call wrf_message("The parameters provided here should be used for testing purposes only!")
end if
! Read parameters for the termite model and put them into wrfinput file
if (config_flags%chem_opt .EQ. GHG_TRACER) then
   CALL termite_initialize(grid%biomt_par,grid%emit_par,config_flags)
end if
!--------------------------------------------------------------------------------------------------------
!
!   RETURN IF CHEMISTRY IS NOT RUNNING
    IF ( config_flags%chem_opt == 0 ) RETURN

! Set the num_vert_mix variable if using ACM
    num_vert_mix = 0
    IF ( config_flags%bl_pbl_physics == ACMPBLSCHEME ) THEN
         mix_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZSORG_AQ, &
              CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
              CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
            num_vert_mix = numgas
         CASE DEFAULT
            num_vert_mix = num_chem
         END SELECT mix_select
        if(num_vert_mix .gt. config_flags%ndepvel) then
         write(message_txt,'(A30,2(I8,2x))') 'chem_init: num_vert_mix and ndepvel ',num_vert_mix,config_flags%ndepvel
         call wrf_message( trim(message_txt) )
        call wrf_error_fatal(" ERROR: CHEM_INIT: num_vert_mix > ndepvel ")
        endif
    ENDIF

    stepbioe=nint(bioemdt*60./dt)
    stepphot=nint(photdt*60./dt)
    stepchem=nint(chemdt*60./dt)
  ! stepfirepl=plumerisefire_frq*60/nint(dt)
    stepfirepl=nint(plumerisefire_frq*60/dt)
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
      CASE (FTUV)
        CALL wrf_debug(00,'call ftuv phot initialization')

        call ftuv_init( id, its, ite, jts, jte, kte, &
                  ide, jde, config_flags,config_flags%num_land_cat,mminlu_loc )
     CASE (TUV)
       if( config_flags%cld_od_opt > 3 .or. config_flags%cld_od_opt < 1 ) then
         call wrf_error_fatal("cld_od_opt must be {1,2,3}")
       endif
       if( config_flags%pht_cldfrc_opt > 2 .or. config_flags%pht_cldfrc_opt < 1 ) then
         call wrf_error_fatal("pht_cldfrc_opt must be {1,2}")
       endif
       write(message_txt,'(''chemics_init('',i2.2,''): call tuv phot initialization'')') id
       CALL wrf_debug( 0,trim(message_txt) )
       call tuv_init( id, config_flags, z_at_w, aerwrf, g, &
                      grid%af_lambda_start, grid%af_lambda_end,grid%lambda_cutoff,&
                      ids,ide, jds,jde, kds,kde, &
                      ims,ime, jms,jme, kms,kme, &
                      its,ite, jts,jte, kts,kte  )
!---

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

!!! TUCCELLA
   if( .NOT. config_flags%restart ) then
   kpp_select: SELECT CASE(config_flags%chem_opt)
     CASE (GOCARTRACM_KPP,RACM_KPP,RACMPM_KPP,RACMSORG_KPP,RACM_MIM_KPP,RACM_ESRLSORG_KPP, &
           RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=380.
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
     CASE (RADM2_KPP,RADM2SORG_KPP,GOCARTRADM2,SAPRC99_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=380.
                   chem(i,k,j,p_ch4)=1.7
                enddo
             enddo
          enddo
        endif
        CASE (CBMZ_BB_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_ch4)=1.7
                enddo
             enddo
          enddo
        endif
      CASE (CB05_SORG_AQ_KPP)
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
                 chem(i,k,j,p_apin)=chem(i,k,j,p_terp) * 0.248   ! 0.0885
                 chem(i,k,j,p_bpin)=chem(i,k,j,p_terp) * 0.294   ! 0.069
                 chem(i,k,j,p_lim) =chem(i,k,j,p_terp) * 0.164   ! 0.135
                 chem(i,k,j,p_ter) =chem(i,k,j,p_terp) * 0.006   ! 0.158
                 chem(i,k,j,p_oci) =chem(i,k,j,p_terp) * 0.213   ! 0.217
                 chem(i,k,j,p_hum) =chem(i,k,j,p_terp) * 0.074   ! 0.331
                 chem(i,k,j,p_h2)  = 0.5
                 chem(i,k,j,p_ch4) =1.7
              enddo
           enddo
        enddo
      CASE (CB05_SORG_VBS_AQ_KPP)
        do j=jts,jte
           do k=kts,kte
              do i=its,ite
                 chem(i,k,j,p_apin)=chem(i,k,j,p_terp) * 0.248   ! 0.0885
                 chem(i,k,j,p_bpin)=chem(i,k,j,p_terp) * 0.294   ! 0.069
                 chem(i,k,j,p_lim) =chem(i,k,j,p_terp) * 0.164   ! 0.135
                 chem(i,k,j,p_ter) =chem(i,k,j,p_terp) * 0.006   ! 0.158
                 chem(i,k,j,p_oci) =chem(i,k,j,p_terp) * 0.213   ! 0.217
                 chem(i,k,j,p_hum) =chem(i,k,j,p_terp) * 0.074   ! 0.331
                 chem(i,k,j,p_h2)  = 0.5
                 chem(i,k,j,p_ch4) =1.7
              enddo
           enddo
        enddo
        CASE (CBMZ_MOSAIC_KPP)
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_ch4)=1.7
                   chem(i,k,j,p_aro1)=0.0
                   chem(i,k,j,p_aro2)=0.0
                   chem(i,k,j,p_alk1)=0.0
                   chem(i,k,j,p_ole1)=0.0
                   chem(i,k,j,p_api1)=0.0
                   chem(i,k,j,p_api2)=0.0
                   chem(i,k,j,p_lim1)=0.0
                   chem(i,k,j,p_lim2)=0.0
                   chem(i,k,j,p_api)=0.0
                   chem(i,k,j,p_lim)=0.0
                enddo
             enddo
          enddo
        endif 
        CASE (MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP)
        grid%vbs_nbin=0
        if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then
          grid%vbs_nbin=4
        endif
        if(config_flags%chem_in_opt == 0 )then
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
          !         chem(i,k,j,p_co2)=380.
          !         chem(i,k,j,p_ch4)=1.7
         if (p_ant1_c.gt.1)    chem(i,k,j,p_ant1_c)=0.0
         if (p_ant2_c.gt.1)    chem(i,k,j,p_ant2_c)=0.0
         if (p_ant3_c.gt.1)    chem(i,k,j,p_ant3_c)=0.0
         if (p_ant4_c.gt.1)    chem(i,k,j,p_ant4_c)=0.0
         if (p_ant1_o.gt.1)    chem(i,k,j,p_ant1_o)=0.0
         if (p_ant2_o.gt.1)    chem(i,k,j,p_ant2_o)=0.0
         if (p_ant3_o.gt.1)    chem(i,k,j,p_ant3_o)=0.0
         if (p_ant4_o.gt.1)    chem(i,k,j,p_ant4_o)=0.0
         if (p_biog1_c.gt.1)    chem(i,k,j,p_biog1_c)=0.0
         if (p_biog2_c.gt.1)    chem(i,k,j,p_biog2_c)=0.0
         if (p_biog3_c.gt.1)    chem(i,k,j,p_biog3_c)=0.0
         if (p_biog4_c.gt.1)    chem(i,k,j,p_biog4_c)=0.0
         if (p_biog1_o.gt.1)    chem(i,k,j,p_biog1_o)=0.0
         if (p_biog2_o.gt.1)    chem(i,k,j,p_biog2_o)=0.0
         if (p_biog3_o.gt.1)    chem(i,k,j,p_biog3_o)=0.0
         if (p_biog4_o.gt.1)    chem(i,k,j,p_biog4_o)=0.0
         if (p_smpa.gt.1)       chem(i,k,j,p_smpa)=0.0
         if (p_smpbb.gt.1)       chem(i,k,j,p_smpbb)=0.0

         if (p_cvasoaX.gt.1)    chem(i,k,j,p_cvasoaX)=0.0
         if (p_cvasoa1.gt.1)    chem(i,k,j,p_cvasoa1)=0.0
         if (p_cvasoa2.gt.1)    chem(i,k,j,p_cvasoa2)=0.0
         if (p_cvasoa3.gt.1)    chem(i,k,j,p_cvasoa3)=0.0
         if (p_cvasoa4.gt.1)    chem(i,k,j,p_cvasoa4)=0.0
         if (p_cvbsoaX.gt.1)    chem(i,k,j,p_cvbsoaX)=0.0
         if (p_cvbsoa1.gt.1)    chem(i,k,j,p_cvbsoa1)=0.0
         if (p_cvbsoa2.gt.1)    chem(i,k,j,p_cvbsoa2)=0.0
         if (p_cvbsoa3.gt.1)    chem(i,k,j,p_cvbsoa3)=0.0
         if (p_cvbsoa4.gt.1)    chem(i,k,j,p_cvbsoa4)=0.0

         if (p_asoaX_a01.gt.1)    chem(i,k,j,p_asoaX_a01)=0.0
         if (p_asoa1_a01.gt.1)    chem(i,k,j,p_asoa1_a01)=0.0
         if (p_asoa2_a01.gt.1)    chem(i,k,j,p_asoa2_a01)=0.0
         if (p_asoa3_a01.gt.1)    chem(i,k,j,p_asoa3_a01)=0.0
         if (p_asoa4_a01.gt.1)    chem(i,k,j,p_asoa4_a01)=0.0
         if (p_bsoaX_a01.gt.1)    chem(i,k,j,p_bsoaX_a01)=0.0
         if (p_bsoa1_a01.gt.1)    chem(i,k,j,p_bsoa1_a01)=0.0
         if (p_bsoa2_a01.gt.1)    chem(i,k,j,p_bsoa2_a01)=0.0
         if (p_bsoa3_a01.gt.1)    chem(i,k,j,p_bsoa3_a01)=0.0
         if (p_bsoa4_a01.gt.1)    chem(i,k,j,p_bsoa4_a01)=0.0

         if (p_asoaX_a02.gt.1)    chem(i,k,j,p_asoaX_a02)=0.0
         if (p_asoa1_a02.gt.1)    chem(i,k,j,p_asoa1_a02)=0.0
         if (p_asoa2_a02.gt.1)    chem(i,k,j,p_asoa2_a02)=0.0
         if (p_asoa3_a02.gt.1)    chem(i,k,j,p_asoa3_a02)=0.0
         if (p_asoa4_a02.gt.1)    chem(i,k,j,p_asoa4_a02)=0.0
         if (p_bsoaX_a02.gt.1)    chem(i,k,j,p_bsoaX_a02)=0.0
         if (p_bsoa1_a02.gt.1)    chem(i,k,j,p_bsoa1_a02)=0.0
         if (p_bsoa2_a02.gt.1)    chem(i,k,j,p_bsoa2_a02)=0.0
         if (p_bsoa3_a02.gt.1)    chem(i,k,j,p_bsoa3_a02)=0.0
         if (p_bsoa4_a02.gt.1)    chem(i,k,j,p_bsoa4_a02)=0.0

         if (p_asoaX_a03.gt.1)    chem(i,k,j,p_asoaX_a03)=0.0
         if (p_asoa1_a03.gt.1)    chem(i,k,j,p_asoa1_a03)=0.0
         if (p_asoa2_a03.gt.1)    chem(i,k,j,p_asoa2_a03)=0.0
         if (p_asoa3_a03.gt.1)    chem(i,k,j,p_asoa3_a03)=0.0
         if (p_asoa4_a03.gt.1)    chem(i,k,j,p_asoa4_a03)=0.0
         if (p_bsoaX_a03.gt.1)    chem(i,k,j,p_bsoaX_a03)=0.0
         if (p_bsoa1_a03.gt.1)    chem(i,k,j,p_bsoa1_a03)=0.0
         if (p_bsoa2_a03.gt.1)    chem(i,k,j,p_bsoa2_a03)=0.0
         if (p_bsoa3_a03.gt.1)    chem(i,k,j,p_bsoa3_a03)=0.0
         if (p_bsoa4_a03.gt.1)    chem(i,k,j,p_bsoa4_a03)=0.0

         if (p_asoaX_a04.gt.1)    chem(i,k,j,p_asoaX_a04)=0.0
         if (p_asoa1_a04.gt.1)    chem(i,k,j,p_asoa1_a04)=0.0
         if (p_asoa2_a04.gt.1)    chem(i,k,j,p_asoa2_a04)=0.0
         if (p_asoa3_a04.gt.1)    chem(i,k,j,p_asoa3_a04)=0.0
         if (p_asoa4_a04.gt.1)    chem(i,k,j,p_asoa4_a04)=0.0
         if (p_bsoaX_a04.gt.1)    chem(i,k,j,p_bsoaX_a04)=0.0
         if (p_bsoa1_a04.gt.1)    chem(i,k,j,p_bsoa1_a04)=0.0
         if (p_bsoa2_a04.gt.1)    chem(i,k,j,p_bsoa2_a04)=0.0
         if (p_bsoa3_a04.gt.1)    chem(i,k,j,p_bsoa3_a04)=0.0
         if (p_bsoa4_a04.gt.1)    chem(i,k,j,p_bsoa4_a04)=0.0

         if (p_glysoa_r1_a01.gt.1)  chem(i,k,j,p_glysoa_r1_a01)=0.0
         if (p_glysoa_r2_a01.gt.1)  chem(i,k,j,p_glysoa_r2_a01)=0.0
         if (p_glysoa_oh_a01.gt.1) chem(i,k,j,p_glysoa_oh_a01)=0.0
         if (p_glysoa_nh4_a01.gt.1) chem(i,k,j,p_glysoa_nh4_a01)=0.0
         if (p_glysoa_sfc_a01.gt.1) chem(i,k,j,p_glysoa_sfc_a01)=0.0

         if (p_glysoa_r1_a02.gt.1) chem(i,k,j,p_glysoa_r1_a02)=0.0
         if (p_glysoa_r2_a02.gt.1) chem(i,k,j,p_glysoa_r2_a02)=0.0
         if (p_glysoa_oh_a02.gt.1) chem(i,k,j,p_glysoa_oh_a02)=0.0
         if (p_glysoa_nh4_a02.gt.1) chem(i,k,j,p_glysoa_nh4_a02)=0.0
         if (p_glysoa_sfc_a02.gt.1) chem(i,k,j,p_glysoa_sfc_a02)=0.0

         if (p_glysoa_r1_a03.gt.1) chem(i,k,j,p_glysoa_r1_a03)=0.0
         if (p_glysoa_r2_a03.gt.1) chem(i,k,j,p_glysoa_r2_a03)=0.0
         if (p_glysoa_oh_a03.gt.1) chem(i,k,j,p_glysoa_oh_a03)=0.0
         if (p_glysoa_nh4_a03.gt.1) chem(i,k,j,p_glysoa_nh4_a03)=0.0
         if (p_glysoa_sfc_a03.gt.1) chem(i,k,j,p_glysoa_sfc_a03)=0.0

         if (p_glysoa_r1_a04.gt.1) chem(i,k,j,p_glysoa_r1_a04)=0.0
         if (p_glysoa_r2_a04.gt.1) chem(i,k,j,p_glysoa_r2_a04)=0.0
         if (p_glysoa_oh_a04.gt.1) chem(i,k,j,p_glysoa_oh_a04)=0.0
         if (p_glysoa_nh4_a04.gt.1) chem(i,k,j,p_glysoa_nh4_a04)=0.0
         if (p_glysoa_sfc_a04.gt.1) chem(i,k,j,p_glysoa_sfc_a04)=0.0

!         chem(i,k,j,p_voca)=0.0
!         chem(i,k,j,p_vocbb)=0.0
         if (p_voca.gt.1) chem(i,k,j,p_voca)=0.0
         if (p_vocbb.gt.1) chem(i,k,j,p_vocbb)=0.0

         if (p_smpa_a01.gt.1)       chem(i,k,j,p_smpa_a01)=1.e-16
         if (p_smpa_a02.gt.1)       chem(i,k,j,p_smpa_a02)=1.e-16
         if (p_smpa_a03.gt.1)       chem(i,k,j,p_smpa_a03)=1.e-16
         if (p_smpa_a04.gt.1)       chem(i,k,j,p_smpa_a04)=1.e-16
         
         if (p_smpbb_a01.gt.1)       chem(i,k,j,p_smpbb_a01)=1.e-16
         if (p_smpbb_a02.gt.1)       chem(i,k,j,p_smpbb_a02)=1.e-16
         if (p_smpbb_a03.gt.1)       chem(i,k,j,p_smpbb_a03)=1.e-16
         if (p_smpbb_a04.gt.1)       chem(i,k,j,p_smpbb_a04)=1.e-16

         if (p_nh4_a01.gt.1)       chem(i,k,j,p_nh4_a01)=1.e-16
         if (p_nh4_a02.gt.1)       chem(i,k,j,p_nh4_a02)=1.e-16
         if (p_nh4_a03.gt.1)       chem(i,k,j,p_nh4_a03)=1.e-16
         if (p_nh4_a04.gt.1)       chem(i,k,j,p_nh4_a04)=1.e-16


                enddo
             enddo
          enddo
        endif



        CASE (SAPRC99_MOSAIC_4BIN_VBS2_KPP)
        if(config_flags%chem_in_opt == 0 )then
                   grid%vbs_nbin(1)=2
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=380.
                   chem(i,k,j,p_ch4)=1.7
         if (p_pcg1_b_c.gt.1) chem(i,k,j,p_pcg1_b_c)=0.00
         if (p_pcg2_b_c.gt.1) chem(i,k,j,p_pcg2_b_c)=0.00
         if (p_pcg3_b_c.gt.1) chem(i,k,j,p_pcg3_b_c)=0.00
         if (p_pcg4_b_c.gt.1) chem(i,k,j,p_pcg4_b_c)=0.00
         if (p_pcg5_b_c.gt.1) chem(i,k,j,p_pcg5_b_c)=0.00
         if (p_pcg6_b_c.gt.1) chem(i,k,j,p_pcg6_b_c)=0.00
         if (p_pcg7_b_c.gt.1) chem(i,k,j,p_pcg7_b_c)=0.00
         if (p_pcg8_b_c.gt.1) chem(i,k,j,p_pcg8_b_c)=0.00
         if (p_pcg9_b_c.gt.1) chem(i,k,j,p_pcg9_b_c)=0.00
         if (p_pcg1_b_o.gt.1) chem(i,k,j,p_pcg1_b_o)=0.00
         if (p_pcg2_b_o.gt.1) chem(i,k,j,p_pcg2_b_o)=0.00
         if (p_pcg3_b_o.gt.1) chem(i,k,j,p_pcg3_b_o)=0.00
         if (p_pcg4_b_o.gt.1) chem(i,k,j,p_pcg4_b_o)=0.00
         if (p_pcg5_b_o.gt.1) chem(i,k,j,p_pcg5_b_o)=0.00
         if (p_pcg6_b_o.gt.1) chem(i,k,j,p_pcg6_b_o)=0.00
         if (p_pcg7_b_o.gt.1) chem(i,k,j,p_pcg7_b_o)=0.00
         if (p_pcg8_b_o.gt.1) chem(i,k,j,p_pcg8_b_o)=0.00
         if (p_pcg9_b_o.gt.1) chem(i,k,j,p_pcg9_b_o)=0.00
         if (p_opcg1_b_c.gt.1) chem(i,k,j,p_opcg1_b_c)=0.00
         if (p_opcg2_b_c.gt.1) chem(i,k,j,p_opcg2_b_c)=0.00
         if (p_opcg3_b_c.gt.1) chem(i,k,j,p_opcg3_b_c)=0.00
         if (p_opcg4_b_c.gt.1) chem(i,k,j,p_opcg4_b_c)=0.00
         if (p_opcg5_b_c.gt.1) chem(i,k,j,p_opcg5_b_c)=0.00
         if (p_opcg6_b_c.gt.1) chem(i,k,j,p_opcg6_b_c)=0.00
         if (p_opcg7_b_c.gt.1) chem(i,k,j,p_opcg7_b_c)=0.00
         if (p_opcg8_b_c.gt.1) chem(i,k,j,p_opcg8_b_c)=0.00
         if (p_opcg1_b_o.gt.1) chem(i,k,j,p_opcg1_b_o)=0.00
         if (p_opcg2_b_o.gt.1) chem(i,k,j,p_opcg2_b_o)=0.00
         if (p_opcg3_b_o.gt.1) chem(i,k,j,p_opcg3_b_o)=0.00
         if (p_opcg4_b_o.gt.1) chem(i,k,j,p_opcg4_b_o)=0.00
         if (p_opcg5_b_o.gt.1) chem(i,k,j,p_opcg5_b_o)=0.00
         if (p_opcg6_b_o.gt.1) chem(i,k,j,p_opcg6_b_o)=0.00
         if (p_opcg7_b_o.gt.1) chem(i,k,j,p_opcg7_b_o)=0.00
         if (p_opcg8_b_o.gt.1) chem(i,k,j,p_opcg8_b_o)=0.00
         if (p_pcg1_f_c.gt.1) chem(i,k,j,p_pcg1_f_c)=0.00
         if (p_pcg2_f_c.gt.1) chem(i,k,j,p_pcg2_f_c)=0.00
         if (p_pcg3_f_c.gt.1) chem(i,k,j,p_pcg3_f_c)=0.00
         if (p_pcg4_f_c.gt.1) chem(i,k,j,p_pcg4_f_c)=0.00
         if (p_pcg5_f_c.gt.1) chem(i,k,j,p_pcg5_f_c)=0.00
         if (p_pcg6_f_c.gt.1) chem(i,k,j,p_pcg6_f_c)=0.00
         if (p_pcg7_f_c.gt.1) chem(i,k,j,p_pcg7_f_c)=0.00
         if (p_pcg8_f_c.gt.1) chem(i,k,j,p_pcg8_f_c)=0.00
         if (p_pcg9_f_c.gt.1) chem(i,k,j,p_pcg9_f_c)=0.00
         if (p_pcg1_f_o.gt.1) chem(i,k,j,p_pcg1_f_o)=0.00
         if (p_pcg2_f_o.gt.1) chem(i,k,j,p_pcg2_f_o)=0.00
         if (p_pcg3_f_o.gt.1) chem(i,k,j,p_pcg3_f_o)=0.00
         if (p_pcg4_f_o.gt.1) chem(i,k,j,p_pcg4_f_o)=0.00
         if (p_pcg5_f_o.gt.1) chem(i,k,j,p_pcg5_f_o)=0.00
         if (p_pcg6_f_o.gt.1) chem(i,k,j,p_pcg6_f_o)=0.00
         if (p_pcg7_f_o.gt.1) chem(i,k,j,p_pcg7_f_o)=0.00
         if (p_pcg8_f_o.gt.1) chem(i,k,j,p_pcg8_f_o)=0.00
         if (p_pcg9_f_o.gt.1) chem(i,k,j,p_pcg9_f_o)=0.00
         if (p_opcg1_f_c.gt.1) chem(i,k,j,p_opcg1_f_c)=0.00
         if (p_opcg2_f_c.gt.1) chem(i,k,j,p_opcg2_f_c)=0.00
         if (p_opcg3_f_c.gt.1) chem(i,k,j,p_opcg3_f_c)=0.00
         if (p_opcg4_f_c.gt.1) chem(i,k,j,p_opcg4_f_c)=0.00
         if (p_opcg5_f_c.gt.1) chem(i,k,j,p_opcg5_f_c)=0.00
         if (p_opcg6_f_c.gt.1) chem(i,k,j,p_opcg6_f_c)=0.00
         if (p_opcg7_f_c.gt.1) chem(i,k,j,p_opcg7_f_c)=0.00
         if (p_opcg8_f_c.gt.1) chem(i,k,j,p_opcg8_f_c)=0.00
         if (p_opcg1_f_o.gt.1) chem(i,k,j,p_opcg1_f_o)=0.00
         if (p_opcg2_f_o.gt.1) chem(i,k,j,p_opcg2_f_o)=0.00
         if (p_opcg3_f_o.gt.1) chem(i,k,j,p_opcg3_f_o)=0.00
         if (p_opcg4_f_o.gt.1) chem(i,k,j,p_opcg4_f_o)=0.00
         if (p_opcg5_f_o.gt.1) chem(i,k,j,p_opcg5_f_o)=0.00
         if (p_opcg6_f_o.gt.1) chem(i,k,j,p_opcg6_f_o)=0.00
         if (p_opcg7_f_o.gt.1) chem(i,k,j,p_opcg7_f_o)=0.00
         if (p_opcg8_f_o.gt.1) chem(i,k,j,p_opcg8_f_o)=0.00
         if (p_ant1_c.gt.1)    chem(i,k,j,p_ant1_c)=0.0
         if (p_ant2_c.gt.1)    chem(i,k,j,p_ant2_c)=0.0
         if (p_ant3_c.gt.1)    chem(i,k,j,p_ant3_c)=0.0
         if (p_ant4_c.gt.1)    chem(i,k,j,p_ant4_c)=0.0
         if (p_ant1_o.gt.1)    chem(i,k,j,p_ant1_o)=0.0
         if (p_ant2_o.gt.1)    chem(i,k,j,p_ant2_o)=0.0
         if (p_ant3_o.gt.1)    chem(i,k,j,p_ant3_o)=0.0
         if (p_ant4_o.gt.1)    chem(i,k,j,p_ant4_o)=0.0
         if (p_biog1_c.gt.1)    chem(i,k,j,p_biog1_c)=0.0
         if (p_biog2_c.gt.1)    chem(i,k,j,p_biog2_c)=0.0
         if (p_biog3_c.gt.1)    chem(i,k,j,p_biog3_c)=0.0
         if (p_biog4_c.gt.1)    chem(i,k,j,p_biog4_c)=0.0
         if (p_biog1_o.gt.1)    chem(i,k,j,p_biog1_o)=0.0
         if (p_biog2_o.gt.1)    chem(i,k,j,p_biog2_o)=0.0
         if (p_biog3_o.gt.1)    chem(i,k,j,p_biog3_o)=0.0
         if (p_biog4_o.gt.1)    chem(i,k,j,p_biog4_o)=0.0



                enddo
             enddo
          enddo
       endif
       
       !BSINGH(04/03/2014): Added 8 bin vbs non-aq pakage
    CASE (SAPRC99_MOSAIC_8BIN_VBS2_KPP)
       if(config_flags%chem_in_opt == 0 )then
          grid%vbs_nbin=2
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=370.
                   chem(i,k,j,p_ch4)=1.7
                   if (p_pcg1_b_c.gt.1) chem(i,k,j,p_pcg1_b_c)=0.00
                   if (p_pcg2_b_c.gt.1) chem(i,k,j,p_pcg2_b_c)=0.00
                   if (p_pcg3_b_c.gt.1) chem(i,k,j,p_pcg3_b_c)=0.00
                   if (p_pcg4_b_c.gt.1) chem(i,k,j,p_pcg4_b_c)=0.00
                   if (p_pcg5_b_c.gt.1) chem(i,k,j,p_pcg5_b_c)=0.00
                   if (p_pcg6_b_c.gt.1) chem(i,k,j,p_pcg6_b_c)=0.00
                   if (p_pcg7_b_c.gt.1) chem(i,k,j,p_pcg7_b_c)=0.00
                   if (p_pcg8_b_c.gt.1) chem(i,k,j,p_pcg8_b_c)=0.00
                   if (p_pcg9_b_c.gt.1) chem(i,k,j,p_pcg9_b_c)=0.00
                   if (p_pcg1_b_o.gt.1) chem(i,k,j,p_pcg1_b_o)=0.00
                   if (p_pcg2_b_o.gt.1) chem(i,k,j,p_pcg2_b_o)=0.00
                   if (p_pcg3_b_o.gt.1) chem(i,k,j,p_pcg3_b_o)=0.00
                   if (p_pcg4_b_o.gt.1) chem(i,k,j,p_pcg4_b_o)=0.00
                   if (p_pcg5_b_o.gt.1) chem(i,k,j,p_pcg5_b_o)=0.00
                   if (p_pcg6_b_o.gt.1) chem(i,k,j,p_pcg6_b_o)=0.00
                   if (p_pcg7_b_o.gt.1) chem(i,k,j,p_pcg7_b_o)=0.00
                   if (p_pcg8_b_o.gt.1) chem(i,k,j,p_pcg8_b_o)=0.00
                   if (p_pcg9_b_o.gt.1) chem(i,k,j,p_pcg9_b_o)=0.00
                   if (p_opcg1_b_c.gt.1) chem(i,k,j,p_opcg1_b_c)=0.00
                   if (p_opcg2_b_c.gt.1) chem(i,k,j,p_opcg2_b_c)=0.00
                   if (p_opcg3_b_c.gt.1) chem(i,k,j,p_opcg3_b_c)=0.00
                   if (p_opcg4_b_c.gt.1) chem(i,k,j,p_opcg4_b_c)=0.00
                   if (p_opcg5_b_c.gt.1) chem(i,k,j,p_opcg5_b_c)=0.00
                   if (p_opcg6_b_c.gt.1) chem(i,k,j,p_opcg6_b_c)=0.00
                   if (p_opcg7_b_c.gt.1) chem(i,k,j,p_opcg7_b_c)=0.00
                   if (p_opcg8_b_c.gt.1) chem(i,k,j,p_opcg8_b_c)=0.00
                   if (p_opcg1_b_o.gt.1) chem(i,k,j,p_opcg1_b_o)=0.00
                   if (p_opcg2_b_o.gt.1) chem(i,k,j,p_opcg2_b_o)=0.00
                   if (p_opcg3_b_o.gt.1) chem(i,k,j,p_opcg3_b_o)=0.00
                   if (p_opcg4_b_o.gt.1) chem(i,k,j,p_opcg4_b_o)=0.00
                   if (p_opcg5_b_o.gt.1) chem(i,k,j,p_opcg5_b_o)=0.00
                   if (p_opcg6_b_o.gt.1) chem(i,k,j,p_opcg6_b_o)=0.00
                   if (p_opcg7_b_o.gt.1) chem(i,k,j,p_opcg7_b_o)=0.00
                   if (p_opcg8_b_o.gt.1) chem(i,k,j,p_opcg8_b_o)=0.00
                   if (p_pcg1_f_c.gt.1) chem(i,k,j,p_pcg1_f_c)=0.00
                   if (p_pcg2_f_c.gt.1) chem(i,k,j,p_pcg2_f_c)=0.00
                   if (p_pcg3_f_c.gt.1) chem(i,k,j,p_pcg3_f_c)=0.00
                   if (p_pcg4_f_c.gt.1) chem(i,k,j,p_pcg4_f_c)=0.00
                   if (p_pcg5_f_c.gt.1) chem(i,k,j,p_pcg5_f_c)=0.00
                   if (p_pcg6_f_c.gt.1) chem(i,k,j,p_pcg6_f_c)=0.00
                   if (p_pcg7_f_c.gt.1) chem(i,k,j,p_pcg7_f_c)=0.00
                   if (p_pcg8_f_c.gt.1) chem(i,k,j,p_pcg8_f_c)=0.00
                   if (p_pcg9_f_c.gt.1) chem(i,k,j,p_pcg9_f_c)=0.00
                   if (p_pcg1_f_o.gt.1) chem(i,k,j,p_pcg1_f_o)=0.00
                   if (p_pcg2_f_o.gt.1) chem(i,k,j,p_pcg2_f_o)=0.00
                   if (p_pcg3_f_o.gt.1) chem(i,k,j,p_pcg3_f_o)=0.00
                   if (p_pcg4_f_o.gt.1) chem(i,k,j,p_pcg4_f_o)=0.00
                   if (p_pcg5_f_o.gt.1) chem(i,k,j,p_pcg5_f_o)=0.00
                   if (p_pcg6_f_o.gt.1) chem(i,k,j,p_pcg6_f_o)=0.00
                   if (p_pcg7_f_o.gt.1) chem(i,k,j,p_pcg7_f_o)=0.00
                   if (p_pcg8_f_o.gt.1) chem(i,k,j,p_pcg8_f_o)=0.00
                   if (p_pcg9_f_o.gt.1) chem(i,k,j,p_pcg9_f_o)=0.00
                   if (p_opcg1_f_c.gt.1) chem(i,k,j,p_opcg1_f_c)=0.00
                   if (p_opcg2_f_c.gt.1) chem(i,k,j,p_opcg2_f_c)=0.00
                   if (p_opcg3_f_c.gt.1) chem(i,k,j,p_opcg3_f_c)=0.00
                   if (p_opcg4_f_c.gt.1) chem(i,k,j,p_opcg4_f_c)=0.00
                   if (p_opcg5_f_c.gt.1) chem(i,k,j,p_opcg5_f_c)=0.00
                   if (p_opcg6_f_c.gt.1) chem(i,k,j,p_opcg6_f_c)=0.00
                   if (p_opcg7_f_c.gt.1) chem(i,k,j,p_opcg7_f_c)=0.00
                   if (p_opcg8_f_c.gt.1) chem(i,k,j,p_opcg8_f_c)=0.00
                   if (p_opcg1_f_o.gt.1) chem(i,k,j,p_opcg1_f_o)=0.00
                   if (p_opcg2_f_o.gt.1) chem(i,k,j,p_opcg2_f_o)=0.00
                   if (p_opcg3_f_o.gt.1) chem(i,k,j,p_opcg3_f_o)=0.00
                   if (p_opcg4_f_o.gt.1) chem(i,k,j,p_opcg4_f_o)=0.00
                   if (p_opcg5_f_o.gt.1) chem(i,k,j,p_opcg5_f_o)=0.00
                   if (p_opcg6_f_o.gt.1) chem(i,k,j,p_opcg6_f_o)=0.00
                   if (p_opcg7_f_o.gt.1) chem(i,k,j,p_opcg7_f_o)=0.00
                   if (p_opcg8_f_o.gt.1) chem(i,k,j,p_opcg8_f_o)=0.00
                   if (p_ant1_c.gt.1)    chem(i,k,j,p_ant1_c)=0.0
                   if (p_ant2_c.gt.1)    chem(i,k,j,p_ant2_c)=0.0
                   if (p_ant3_c.gt.1)    chem(i,k,j,p_ant3_c)=0.0
                   if (p_ant4_c.gt.1)    chem(i,k,j,p_ant4_c)=0.0
                   if (p_ant1_o.gt.1)    chem(i,k,j,p_ant1_o)=0.0
                   if (p_ant2_o.gt.1)    chem(i,k,j,p_ant2_o)=0.0
                   if (p_ant3_o.gt.1)    chem(i,k,j,p_ant3_o)=0.0
                   if (p_ant4_o.gt.1)    chem(i,k,j,p_ant4_o)=0.0
                   if (p_biog1_c.gt.1)    chem(i,k,j,p_biog1_c)=0.0
                   if (p_biog2_c.gt.1)    chem(i,k,j,p_biog2_c)=0.0
                   if (p_biog3_c.gt.1)    chem(i,k,j,p_biog3_c)=0.0
                   if (p_biog4_c.gt.1)    chem(i,k,j,p_biog4_c)=0.0
                   if (p_biog1_o.gt.1)    chem(i,k,j,p_biog1_o)=0.0
                   if (p_biog2_o.gt.1)    chem(i,k,j,p_biog2_o)=0.0
                   if (p_biog3_o.gt.1)    chem(i,k,j,p_biog3_o)=0.0
                   if (p_biog4_o.gt.1)    chem(i,k,j,p_biog4_o)=0.0
                   if (p_bgas.gt.1)    chem(i,k,j,p_bgas)=0.0
                   if (p_agas.gt.1)    chem(i,k,j,p_agas)=0.0
                   if (p_nume.gt.1)    chem(i,k,j,p_nume)=0.0
                   if (p_den.gt.1)    chem(i,k,j,p_den)=0.0
                   if (p_psd1.gt.1)    chem(i,k,j,p_psd1)=0.0
                   if (p_psd2.gt.1)    chem(i,k,j,p_psd2)=0.0
                   if (p_isoprene.gt.1)    chem(i,k,j,p_isoprene)=0.0
                   if (p_terp.gt.1)    chem(i,k,j,p_terp)=0.0
                   if (p_sesq.gt.1)    chem(i,k,j,p_sesq)=0.0
                   if (p_aro1.gt.1)    chem(i,k,j,p_aro1)=0.0
                   if (p_aro2.gt.1)    chem(i,k,j,p_aro2)=0.0
                   
                   
                   if (p_pcg1_b_c_a01.gt.1)    chem(i,k,j,p_pcg1_b_c_a01)=0.0
                   if (p_pcg1_b_o_a01.gt.1)    chem(i,k,j,p_pcg1_b_o_a01)=0.0
                   if (p_opcg1_b_c_a01.gt.1)    chem(i,k,j,p_opcg1_b_c_a01)=0.0
                   if (p_opcg1_b_o_a01.gt.1)    chem(i,k,j,p_opcg1_b_o_a01)=0.0
                   if (p_pcg1_f_c_a01.gt.1)    chem(i,k,j,p_pcg1_f_c_a01)=0.0
                   if (p_pcg1_f_o_a01.gt.1)    chem(i,k,j,p_pcg1_f_o_a01)=0.0
                   if (p_opcg1_f_c_a01.gt.1)    chem(i,k,j,p_opcg1_f_c_a01)=0.0
                   if (p_opcg1_f_o_a01.gt.1)    chem(i,k,j,p_opcg1_f_o_a01)=0.0
                   if (p_ant1_c_a01.gt.1)    chem(i,k,j,p_ant1_c_a01)=0.0
                   if (p_biog1_c_a01.gt.1)    chem(i,k,j,p_biog1_c_a01)=0.0
                   if (p_ant2_c_a01.gt.1)    chem(i,k,j,p_ant2_c_a01)=0.0
                   if (p_biog2_c_a01.gt.1)    chem(i,k,j,p_biog2_c_a01)=0.0
                   if (p_biog3_c_a01.gt.1)    chem(i,k,j,p_biog3_c_a01)=0.0
                   if (p_biog4_c_a01.gt.1)    chem(i,k,j,p_biog4_c_a01)=0.0
                   if (p_biog1_o_a01.gt.1)    chem(i,k,j,p_biog1_o_a01)=0.0
                   if (p_biog2_o_a01.gt.1)    chem(i,k,j,p_biog2_o_a01)=0.0
                   if (p_ant3_c_a01.gt.1)    chem(i,k,j,p_ant3_c_a01)=0.0
                   if (p_ant4_c_a01.gt.1)    chem(i,k,j,p_ant4_c_a01)=0.0
                   
                   
                   if (p_pcg1_b_c_a02.gt.1)    chem(i,k,j,p_pcg1_b_c_a02)=0.0
                   if (p_pcg1_b_o_a02.gt.1)    chem(i,k,j,p_pcg1_b_o_a02)=0.0
                   if (p_opcg1_b_c_a02.gt.1)    chem(i,k,j,p_opcg1_b_c_a02)=0.0
                   if (p_opcg1_b_o_a02.gt.1)    chem(i,k,j,p_opcg1_b_o_a02)=0.0
                   if (p_pcg1_f_c_a02.gt.1)    chem(i,k,j,p_pcg1_f_c_a02)=0.0
                   if (p_pcg1_f_o_a02.gt.1)    chem(i,k,j,p_pcg1_f_o_a02)=0.0
                   if (p_opcg1_f_c_a02.gt.1)    chem(i,k,j,p_opcg1_f_c_a02)=0.0
                   if (p_opcg1_f_o_a02.gt.1)    chem(i,k,j,p_opcg1_f_o_a02)=0.0
                   if (p_ant1_c_a02.gt.1)    chem(i,k,j,p_ant1_c_a02)=0.0
                   if (p_biog1_c_a02.gt.1)    chem(i,k,j,p_biog1_c_a02)=0.0
                   if (p_ant2_c_a02.gt.1)    chem(i,k,j,p_ant2_c_a02)=0.0
                   if (p_biog2_c_a02.gt.1)    chem(i,k,j,p_biog2_c_a02)=0.0
                   if (p_biog3_c_a02.gt.1)    chem(i,k,j,p_biog3_c_a02)=0.0
                   if (p_biog4_c_a02.gt.1)    chem(i,k,j,p_biog4_c_a02)=0.0
                   if (p_biog1_o_a02.gt.1)    chem(i,k,j,p_biog1_o_a02)=0.0
                   if (p_biog2_o_a02.gt.1)    chem(i,k,j,p_biog2_o_a02)=0.0
                   if (p_ant3_c_a02.gt.1)    chem(i,k,j,p_ant3_c_a02)=0.0
                   if (p_ant4_c_a02.gt.1)    chem(i,k,j,p_ant4_c_a02)=0.0
                   
                   
                   
                   if (p_pcg1_b_c_a03.gt.1)    chem(i,k,j,p_pcg1_b_c_a03)=0.0
                   if (p_pcg1_b_o_a03.gt.1)    chem(i,k,j,p_pcg1_b_o_a03)=0.0
                   if (p_opcg1_b_c_a03.gt.1)    chem(i,k,j,p_opcg1_b_c_a03)=0.0
                   if (p_opcg1_b_o_a03.gt.1)    chem(i,k,j,p_opcg1_b_o_a03)=0.0
                   if (p_pcg1_f_c_a03.gt.1)    chem(i,k,j,p_pcg1_f_c_a03)=0.0
                   if (p_pcg1_f_o_a03.gt.1)    chem(i,k,j,p_pcg1_f_o_a03)=0.0
                   if (p_opcg1_f_c_a03.gt.1)    chem(i,k,j,p_opcg1_f_c_a03)=0.0
                   if (p_opcg1_f_o_a03.gt.1)    chem(i,k,j,p_opcg1_f_o_a03)=0.0
                   if (p_ant1_c_a03.gt.1)    chem(i,k,j,p_ant1_c_a03)=0.0
                   if (p_biog1_c_a03.gt.1)    chem(i,k,j,p_biog1_c_a03)=0.0
                   if (p_ant2_c_a03.gt.1)    chem(i,k,j,p_ant2_c_a03)=0.0
                   if (p_biog2_c_a03.gt.1)    chem(i,k,j,p_biog2_c_a03)=0.0
                   if (p_biog3_c_a03.gt.1)    chem(i,k,j,p_biog3_c_a03)=0.0
                   if (p_biog4_c_a03.gt.1)    chem(i,k,j,p_biog4_c_a03)=0.0
                   if (p_biog1_o_a03.gt.1)    chem(i,k,j,p_biog1_o_a03)=0.0
                   if (p_biog2_o_a03.gt.1)    chem(i,k,j,p_biog2_o_a03)=0.0
                   if (p_ant3_c_a03.gt.1)    chem(i,k,j,p_ant3_c_a03)=0.0
                   if (p_ant4_c_a03.gt.1)    chem(i,k,j,p_ant4_c_a03)=0.0
                   
                   
                   if (p_pcg1_b_c_a04.gt.1)    chem(i,k,j,p_pcg1_b_c_a04)=0.0
                   if (p_pcg1_b_o_a04.gt.1)    chem(i,k,j,p_pcg1_b_o_a04)=0.0
                   if (p_opcg1_b_c_a04.gt.1)    chem(i,k,j,p_opcg1_b_c_a04)=0.0
                   if (p_opcg1_b_o_a04.gt.1)    chem(i,k,j,p_opcg1_b_o_a04)=0.0
                   if (p_pcg1_f_c_a04.gt.1)    chem(i,k,j,p_pcg1_f_c_a04)=0.0
                   if (p_pcg1_f_o_a04.gt.1)    chem(i,k,j,p_pcg1_f_o_a04)=0.0
                   if (p_opcg1_f_c_a04.gt.1)    chem(i,k,j,p_opcg1_f_c_a04)=0.0
                   if (p_opcg1_f_o_a04.gt.1)    chem(i,k,j,p_opcg1_f_o_a04)=0.0
                   if (p_ant1_c_a04.gt.1)    chem(i,k,j,p_ant1_c_a04)=0.0
                   if (p_biog1_c_a04.gt.1)    chem(i,k,j,p_biog1_c_a04)=0.0
                   if (p_ant2_c_a04.gt.1)    chem(i,k,j,p_ant2_c_a04)=0.0
                   if (p_biog2_c_a04.gt.1)    chem(i,k,j,p_biog2_c_a04)=0.0
                   if (p_biog3_c_a04.gt.1)    chem(i,k,j,p_biog3_c_a04)=0.0
                   if (p_biog4_c_a04.gt.1)    chem(i,k,j,p_biog4_c_a04)=0.0
                   if (p_biog1_o_a04.gt.1)    chem(i,k,j,p_biog1_o_a04)=0.0
                   if (p_biog2_o_a04.gt.1)    chem(i,k,j,p_biog2_o_a04)=0.0
                   if (p_ant3_c_a04.gt.1)    chem(i,k,j,p_ant3_c_a04)=0.0
                   if (p_ant4_c_a04.gt.1)    chem(i,k,j,p_ant4_c_a04)=0.0
                   
                   
                   if (p_pcg1_b_c_a05.gt.1)    chem(i,k,j,p_pcg1_b_c_a05)=0.0
                   if (p_pcg1_b_o_a05.gt.1)    chem(i,k,j,p_pcg1_b_o_a05)=0.0
                   if (p_opcg1_b_c_a05.gt.1)    chem(i,k,j,p_opcg1_b_c_a05)=0.0
                   if (p_opcg1_b_o_a05.gt.1)    chem(i,k,j,p_opcg1_b_o_a05)=0.0
                   if (p_pcg1_f_c_a05.gt.1)    chem(i,k,j,p_pcg1_f_c_a05)=0.0
                   if (p_pcg1_f_o_a05.gt.1)    chem(i,k,j,p_pcg1_f_o_a05)=0.0
                   if (p_opcg1_f_c_a05.gt.1)    chem(i,k,j,p_opcg1_f_c_a05)=0.0
                   if (p_opcg1_f_o_a05.gt.1)    chem(i,k,j,p_opcg1_f_o_a05)=0.0
                   if (p_ant1_c_a05.gt.1)    chem(i,k,j,p_ant1_c_a05)=0.0
                   if (p_biog1_c_a05.gt.1)    chem(i,k,j,p_biog1_c_a05)=0.0
                   if (p_ant2_c_a05.gt.1)    chem(i,k,j,p_ant2_c_a05)=0.0
                   if (p_biog2_c_a05.gt.1)    chem(i,k,j,p_biog2_c_a05)=0.0
                   if (p_biog3_c_a05.gt.1)    chem(i,k,j,p_biog3_c_a05)=0.0
                   if (p_biog4_c_a05.gt.1)    chem(i,k,j,p_biog4_c_a05)=0.0
                   if (p_biog1_o_a05.gt.1)    chem(i,k,j,p_biog1_o_a05)=0.0
                   if (p_biog2_o_a05.gt.1)    chem(i,k,j,p_biog2_o_a05)=0.0
                   if (p_ant3_c_a05.gt.1)    chem(i,k,j,p_ant3_c_a05)=0.0
                   if (p_ant4_c_a05.gt.1)    chem(i,k,j,p_ant4_c_a05)=0.0
                   
                   
                   if (p_pcg1_b_c_a06.gt.1)    chem(i,k,j,p_pcg1_b_c_a06)=0.0
                   if (p_pcg1_b_o_a06.gt.1)    chem(i,k,j,p_pcg1_b_o_a06)=0.0
                   if (p_opcg1_b_c_a06.gt.1)    chem(i,k,j,p_opcg1_b_c_a06)=0.0
                   if (p_opcg1_b_o_a06.gt.1)    chem(i,k,j,p_opcg1_b_o_a06)=0.0
                   if (p_pcg1_f_c_a06.gt.1)    chem(i,k,j,p_pcg1_f_c_a06)=0.0
                   if (p_pcg1_f_o_a06.gt.1)    chem(i,k,j,p_pcg1_f_o_a06)=0.0
                   if (p_opcg1_f_c_a06.gt.1)    chem(i,k,j,p_opcg1_f_c_a06)=0.0
                   if (p_opcg1_f_o_a06.gt.1)    chem(i,k,j,p_opcg1_f_o_a06)=0.0
                   if (p_ant1_c_a06.gt.1)    chem(i,k,j,p_ant1_c_a06)=0.0
                   if (p_biog1_c_a06.gt.1)    chem(i,k,j,p_biog1_c_a06)=0.0
                   if (p_ant2_c_a06.gt.1)    chem(i,k,j,p_ant2_c_a06)=0.0
                   if (p_biog2_c_a06.gt.1)    chem(i,k,j,p_biog2_c_a06)=0.0
                   if (p_biog3_c_a06.gt.1)    chem(i,k,j,p_biog3_c_a06)=0.0
                   if (p_biog4_c_a06.gt.1)    chem(i,k,j,p_biog4_c_a06)=0.0
                   if (p_biog1_o_a06.gt.1)    chem(i,k,j,p_biog1_o_a06)=0.0
                   if (p_biog2_o_a06.gt.1)    chem(i,k,j,p_biog2_o_a06)=0.0
                   if (p_ant3_c_a06.gt.1)    chem(i,k,j,p_ant3_c_a06)=0.0
                   if (p_ant4_c_a06.gt.1)    chem(i,k,j,p_ant4_c_a06)=0.0
                   
                   
                   if (p_pcg1_b_c_a07.gt.1)    chem(i,k,j,p_pcg1_b_c_a07)=0.0
                   if (p_pcg1_b_o_a07.gt.1)    chem(i,k,j,p_pcg1_b_o_a07)=0.0
                   if (p_opcg1_b_c_a07.gt.1)    chem(i,k,j,p_opcg1_b_c_a07)=0.0
                   if (p_opcg1_b_o_a07.gt.1)    chem(i,k,j,p_opcg1_b_o_a07)=0.0
                   if (p_pcg1_f_c_a07.gt.1)    chem(i,k,j,p_pcg1_f_c_a07)=0.0
                   if (p_pcg1_f_o_a07.gt.1)    chem(i,k,j,p_pcg1_f_o_a07)=0.0
                   if (p_opcg1_f_c_a07.gt.1)    chem(i,k,j,p_opcg1_f_c_a07)=0.0
                   if (p_opcg1_f_o_a07.gt.1)    chem(i,k,j,p_opcg1_f_o_a07)=0.0
                   if (p_ant1_c_a07.gt.1)    chem(i,k,j,p_ant1_c_a07)=0.0
                   if (p_biog1_c_a07.gt.1)    chem(i,k,j,p_biog1_c_a07)=0.0
                   if (p_ant2_c_a07.gt.1)    chem(i,k,j,p_ant2_c_a07)=0.0
                   if (p_biog2_c_a07.gt.1)    chem(i,k,j,p_biog2_c_a07)=0.0
                   if (p_biog3_c_a07.gt.1)    chem(i,k,j,p_biog3_c_a07)=0.0
                   if (p_biog4_c_a07.gt.1)    chem(i,k,j,p_biog4_c_a07)=0.0
                   if (p_biog1_o_a07.gt.1)    chem(i,k,j,p_biog1_o_a07)=0.0
                   if (p_biog2_o_a07.gt.1)    chem(i,k,j,p_biog2_o_a07)=0.0
                   if (p_ant3_c_a07.gt.1)    chem(i,k,j,p_ant3_c_a07)=0.0
                   if (p_ant4_c_a07.gt.1)    chem(i,k,j,p_ant4_c_a07)=0.0
                   
                   
                   if (p_pcg1_b_c_a08.gt.1)    chem(i,k,j,p_pcg1_b_c_a08)=0.0
                   if (p_pcg1_b_o_a08.gt.1)    chem(i,k,j,p_pcg1_b_o_a08)=0.0
                   if (p_opcg1_b_c_a08.gt.1)    chem(i,k,j,p_opcg1_b_c_a08)=0.0
                   if (p_opcg1_b_o_a08.gt.1)    chem(i,k,j,p_opcg1_b_o_a08)=0.0
                   if (p_pcg1_f_c_a08.gt.1)    chem(i,k,j,p_pcg1_f_c_a08)=0.0
                   if (p_pcg1_f_o_a08.gt.1)    chem(i,k,j,p_pcg1_f_o_a08)=0.0
                   if (p_opcg1_f_c_a08.gt.1)    chem(i,k,j,p_opcg1_f_c_a08)=0.0
                   if (p_opcg1_f_o_a08.gt.1)    chem(i,k,j,p_opcg1_f_o_a08)=0.0
                   if (p_ant1_c_a08.gt.1)    chem(i,k,j,p_ant1_c_a08)=0.0
                   if (p_biog1_c_a08.gt.1)    chem(i,k,j,p_biog1_c_a08)=0.0
                   if (p_ant2_c_a08.gt.1)    chem(i,k,j,p_ant2_c_a08)=0.0
                   if (p_biog2_c_a08.gt.1)    chem(i,k,j,p_biog2_c_a08)=0.0
                   if (p_biog3_c_a08.gt.1)    chem(i,k,j,p_biog3_c_a08)=0.0
                   if (p_biog4_c_a08.gt.1)    chem(i,k,j,p_biog4_c_a08)=0.0
                   if (p_biog1_o_a08.gt.1)    chem(i,k,j,p_biog1_o_a08)=0.0
                   if (p_biog2_o_a08.gt.1)    chem(i,k,j,p_biog2_o_a08)=0.0
                   if (p_ant3_c_a08.gt.1)    chem(i,k,j,p_ant3_c_a08)=0.0
                   if (p_ant4_c_a08.gt.1)    chem(i,k,j,p_ant4_c_a08)=0.0
                   
                   
                   
                enddo
             enddo
          enddo
       endif
       !BSINGH(04/03/2014):ENDS
       
       
       !BSINGH(12/03/2013) - Added case statement for SAPRC 8 bin 
    CASE (SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP )!BSINGH (12/11/13): Got rid of SAPRC99_MOSAIC_4BIN_VBS2_AQ_KPP and SAPRC99_MOSAIC_4BIN_VBS2_KPP
       
       if(config_flags%chem_in_opt == 1 ) grid%vbs_nbin=2
       if(config_flags%chem_in_opt == 0 )then
          grid%vbs_nbin=2
          do j=jts,jte
             do k=kts,kte
                do i=its,ite
                   chem(i,k,j,p_co2)=370.
                   chem(i,k,j,p_ch4)=1.7
                   if (p_pcg1_b_c.gt.1)        chem(i,k,j,p_pcg1_b_c)=0.00
                   if (p_pcg2_b_c.gt.1)        chem(i,k,j,p_pcg2_b_c)=0.00
                   if (p_pcg3_b_c.gt.1)        chem(i,k,j,p_pcg3_b_c)=0.00
                   if (p_pcg4_b_c.gt.1)        chem(i,k,j,p_pcg4_b_c)=0.00
                   if (p_pcg5_b_c.gt.1)        chem(i,k,j,p_pcg5_b_c)=0.00
                   if (p_pcg6_b_c.gt.1)        chem(i,k,j,p_pcg6_b_c)=0.00
                   if (p_pcg7_b_c.gt.1)        chem(i,k,j,p_pcg7_b_c)=0.00
                   if (p_pcg8_b_c.gt.1)        chem(i,k,j,p_pcg8_b_c)=0.00
                   if (p_pcg9_b_c.gt.1)        chem(i,k,j,p_pcg9_b_c)=0.00
                   if (p_pcg1_b_o.gt.1)        chem(i,k,j,p_pcg1_b_o)=0.00
                   if (p_pcg2_b_o.gt.1)        chem(i,k,j,p_pcg2_b_o)=0.00
                   if (p_pcg3_b_o.gt.1)        chem(i,k,j,p_pcg3_b_o)=0.00
                   if (p_pcg4_b_o.gt.1)        chem(i,k,j,p_pcg4_b_o)=0.00
                   if (p_pcg5_b_o.gt.1)        chem(i,k,j,p_pcg5_b_o)=0.00
                   if (p_pcg6_b_o.gt.1)        chem(i,k,j,p_pcg6_b_o)=0.00
                   if (p_pcg7_b_o.gt.1)        chem(i,k,j,p_pcg7_b_o)=0.00
                   if (p_pcg8_b_o.gt.1)        chem(i,k,j,p_pcg8_b_o)=0.00
                   if (p_pcg9_b_o.gt.1)        chem(i,k,j,p_pcg9_b_o)=0.00
                   if (p_opcg1_b_c.gt.1)       chem(i,k,j,p_opcg1_b_c)=0.00
                   if (p_opcg2_b_c.gt.1)       chem(i,k,j,p_opcg2_b_c)=0.00
                   if (p_opcg3_b_c.gt.1)       chem(i,k,j,p_opcg3_b_c)=0.00
                   if (p_opcg4_b_c.gt.1)       chem(i,k,j,p_opcg4_b_c)=0.00
                   if (p_opcg5_b_c.gt.1)       chem(i,k,j,p_opcg5_b_c)=0.00
                   if (p_opcg6_b_c.gt.1)       chem(i,k,j,p_opcg6_b_c)=0.00
                   if (p_opcg7_b_c.gt.1)       chem(i,k,j,p_opcg7_b_c)=0.00
                   if (p_opcg8_b_c.gt.1)       chem(i,k,j,p_opcg8_b_c)=0.00
                   if (p_opcg1_b_o.gt.1)       chem(i,k,j,p_opcg1_b_o)=0.00
                   if (p_opcg2_b_o.gt.1)       chem(i,k,j,p_opcg2_b_o)=0.00
                   if (p_opcg3_b_o.gt.1)       chem(i,k,j,p_opcg3_b_o)=0.00
                   if (p_opcg4_b_o.gt.1)       chem(i,k,j,p_opcg4_b_o)=0.00
                   if (p_opcg5_b_o.gt.1)       chem(i,k,j,p_opcg5_b_o)=0.00
                   if (p_opcg6_b_o.gt.1)       chem(i,k,j,p_opcg6_b_o)=0.00
                   if (p_opcg7_b_o.gt.1)       chem(i,k,j,p_opcg7_b_o)=0.00
                   if (p_opcg8_b_o.gt.1)       chem(i,k,j,p_opcg8_b_o)=0.00
                   if (p_pcg1_f_c.gt.1)        chem(i,k,j,p_pcg1_f_c)=0.00
                   if (p_pcg2_f_c.gt.1)        chem(i,k,j,p_pcg2_f_c)=0.00
                   if (p_pcg3_f_c.gt.1)        chem(i,k,j,p_pcg3_f_c)=0.00
                   if (p_pcg4_f_c.gt.1)        chem(i,k,j,p_pcg4_f_c)=0.00
                   if (p_pcg5_f_c.gt.1)        chem(i,k,j,p_pcg5_f_c)=0.00
                   if (p_pcg6_f_c.gt.1)        chem(i,k,j,p_pcg6_f_c)=0.00
                   if (p_pcg7_f_c.gt.1)        chem(i,k,j,p_pcg7_f_c)=0.00
                   if (p_pcg8_f_c.gt.1)        chem(i,k,j,p_pcg8_f_c)=0.00
                   if (p_pcg9_f_c.gt.1)        chem(i,k,j,p_pcg9_f_c)=0.00
                   if (p_pcg1_f_o.gt.1)        chem(i,k,j,p_pcg1_f_o)=0.00
                   if (p_pcg2_f_o.gt.1)        chem(i,k,j,p_pcg2_f_o)=0.00
                   if (p_pcg3_f_o.gt.1)        chem(i,k,j,p_pcg3_f_o)=0.00
                   if (p_pcg4_f_o.gt.1)        chem(i,k,j,p_pcg4_f_o)=0.00
                   if (p_pcg5_f_o.gt.1)        chem(i,k,j,p_pcg5_f_o)=0.00
                   if (p_pcg6_f_o.gt.1)        chem(i,k,j,p_pcg6_f_o)=0.00
                   if (p_pcg7_f_o.gt.1)        chem(i,k,j,p_pcg7_f_o)=0.00
                   if (p_pcg8_f_o.gt.1)        chem(i,k,j,p_pcg8_f_o)=0.00
                   if (p_pcg9_f_o.gt.1)        chem(i,k,j,p_pcg9_f_o)=0.00
                   if (p_opcg1_f_c.gt.1)       chem(i,k,j,p_opcg1_f_c)=0.00
                   if (p_opcg2_f_c.gt.1)       chem(i,k,j,p_opcg2_f_c)=0.00
                   if (p_opcg3_f_c.gt.1)       chem(i,k,j,p_opcg3_f_c)=0.00
                   if (p_opcg4_f_c.gt.1)       chem(i,k,j,p_opcg4_f_c)=0.00
                   if (p_opcg5_f_c.gt.1)       chem(i,k,j,p_opcg5_f_c)=0.00
                   if (p_opcg6_f_c.gt.1)       chem(i,k,j,p_opcg6_f_c)=0.00
                   if (p_opcg7_f_c.gt.1)       chem(i,k,j,p_opcg7_f_c)=0.00
                   if (p_opcg8_f_c.gt.1)       chem(i,k,j,p_opcg8_f_c)=0.00
                   if (p_opcg1_f_o.gt.1)       chem(i,k,j,p_opcg1_f_o)=0.00
                   if (p_opcg2_f_o.gt.1)       chem(i,k,j,p_opcg2_f_o)=0.00
                   if (p_opcg3_f_o.gt.1)       chem(i,k,j,p_opcg3_f_o)=0.00
                   if (p_opcg4_f_o.gt.1)       chem(i,k,j,p_opcg4_f_o)=0.00
                   if (p_opcg5_f_o.gt.1)       chem(i,k,j,p_opcg5_f_o)=0.00
                   if (p_opcg6_f_o.gt.1)       chem(i,k,j,p_opcg6_f_o)=0.00
                   if (p_opcg7_f_o.gt.1)       chem(i,k,j,p_opcg7_f_o)=0.00
                   if (p_opcg8_f_o.gt.1)       chem(i,k,j,p_opcg8_f_o)=0.00
                   if (p_ant1_c.gt.1)          chem(i,k,j,p_ant1_c)=0.0
                   if (p_ant2_c.gt.1)          chem(i,k,j,p_ant2_c)=0.0
                   if (p_ant3_c.gt.1)          chem(i,k,j,p_ant3_c)=0.0
                   if (p_ant4_c.gt.1)          chem(i,k,j,p_ant4_c)=0.0
                   if (p_ant1_o.gt.1)          chem(i,k,j,p_ant1_o)=0.0
                   if (p_ant2_o.gt.1)          chem(i,k,j,p_ant2_o)=0.0
                   if (p_ant3_o.gt.1)          chem(i,k,j,p_ant3_o)=0.0
                   if (p_ant4_o.gt.1)          chem(i,k,j,p_ant4_o)=0.0
                   if (p_biog1_c.gt.1)         chem(i,k,j,p_biog1_c)=0.0
                   if (p_biog2_c.gt.1)         chem(i,k,j,p_biog2_c)=0.0
                   if (p_biog3_c.gt.1)         chem(i,k,j,p_biog3_c)=0.0
                   if (p_biog4_c.gt.1)         chem(i,k,j,p_biog4_c)=0.0
                   if (p_biog1_o.gt.1)         chem(i,k,j,p_biog1_o)=0.0
                   if (p_biog2_o.gt.1)         chem(i,k,j,p_biog2_o)=0.0
                   if (p_biog3_o.gt.1)         chem(i,k,j,p_biog3_o)=0.0
                   if (p_biog4_o.gt.1)         chem(i,k,j,p_biog4_o)=0.0
                   
                   if (p_pcg1_b_c_a01.gt.1)    chem(i,k,j,p_pcg1_b_c_a01)=0.0
                   if (p_pcg1_b_o_a01.gt.1)    chem(i,k,j,p_pcg1_b_o_a01)=0.0
                   if (p_opcg1_b_c_a01.gt.1)   chem(i,k,j,p_opcg1_b_c_a01)=0.0
                   if (p_opcg1_b_o_a01.gt.1)   chem(i,k,j,p_opcg1_b_o_a01)=0.0
                   if (p_pcg1_f_c_a01.gt.1)    chem(i,k,j,p_pcg1_f_c_a01)=0.0
                   if (p_pcg1_f_o_a01.gt.1)    chem(i,k,j,p_pcg1_f_o_a01)=0.0
                   if (p_opcg1_f_c_a01.gt.1)   chem(i,k,j,p_opcg1_f_c_a01)=0.0
                   if (p_opcg1_f_o_a01.gt.1)   chem(i,k,j,p_opcg1_f_o_a01)=0.0
                   if (p_ant1_c_a01.gt.1)      chem(i,k,j,p_ant1_c_a01)=0.0
                   if (p_biog1_c_a01.gt.1)     chem(i,k,j,p_biog1_c_a01)=0.0
                   
                   if (p_pcg1_b_c_a02.gt.1)    chem(i,k,j,p_pcg1_b_c_a02)=0.0
                   if (p_pcg1_b_o_a02.gt.1)    chem(i,k,j,p_pcg1_b_o_a02)=0.0
                   if (p_opcg1_b_c_a02.gt.1)   chem(i,k,j,p_opcg1_b_c_a02)=0.0
                   if (p_opcg1_b_o_a02.gt.1)   chem(i,k,j,p_opcg1_b_o_a02)=0.0
                   if (p_pcg1_f_c_a02.gt.1)    chem(i,k,j,p_pcg1_f_c_a02)=0.0
                   if (p_pcg1_f_o_a02.gt.1)    chem(i,k,j,p_pcg1_f_o_a02)=0.0
                   if (p_opcg1_f_c_a02.gt.1)   chem(i,k,j,p_opcg1_f_c_a02)=0.0
                   if (p_opcg1_f_o_a02.gt.1)   chem(i,k,j,p_opcg1_f_o_a02)=0.0
                   if (p_ant1_c_a02.gt.1)      chem(i,k,j,p_ant1_c_a02)=0.0
                   if (p_biog1_c_a02.gt.1)     chem(i,k,j,p_biog1_c_a02)=0.0
                   
                   if (p_pcg1_b_c_a03.gt.1)    chem(i,k,j,p_pcg1_b_c_a03)=0.0
                   if (p_pcg1_b_o_a03.gt.1)    chem(i,k,j,p_pcg1_b_o_a03)=0.0
                   if (p_opcg1_b_c_a03.gt.1)   chem(i,k,j,p_opcg1_b_c_a03)=0.0
                   if (p_opcg1_b_o_a03.gt.1)   chem(i,k,j,p_opcg1_b_o_a03)=0.0
                   if (p_pcg1_f_c_a03.gt.1)    chem(i,k,j,p_pcg1_f_c_a03)=0.0
                   if (p_pcg1_f_o_a03.gt.1)    chem(i,k,j,p_pcg1_f_o_a03)=0.0
                   if (p_opcg1_f_c_a03.gt.1)   chem(i,k,j,p_opcg1_f_c_a03)=0.0
                   if (p_opcg1_f_o_a03.gt.1)   chem(i,k,j,p_opcg1_f_o_a03)=0.0
                   if (p_ant1_c_a03.gt.1)      chem(i,k,j,p_ant1_c_a03)=0.0
                   if (p_biog1_c_a03.gt.1)     chem(i,k,j,p_biog1_c_a03)=0.0
                   
                   if (p_pcg1_b_c_a04.gt.1)    chem(i,k,j,p_pcg1_b_c_a04)=0.0
                   if (p_pcg1_b_o_a04.gt.1)    chem(i,k,j,p_pcg1_b_o_a04)=0.0
                   if (p_opcg1_b_c_a04.gt.1)   chem(i,k,j,p_opcg1_b_c_a04)=0.0
                   if (p_opcg1_b_o_a04.gt.1)   chem(i,k,j,p_opcg1_b_o_a04)=0.0
                   if (p_pcg1_f_c_a04.gt.1)    chem(i,k,j,p_pcg1_f_c_a04)=0.0
                   if (p_pcg1_f_o_a04.gt.1)    chem(i,k,j,p_pcg1_f_o_a04)=0.0
                   if (p_opcg1_f_c_a04.gt.1)   chem(i,k,j,p_opcg1_f_c_a04)=0.0
                   if (p_opcg1_f_o_a04.gt.1)   chem(i,k,j,p_opcg1_f_o_a04)=0.0
                   if (p_ant1_c_a04.gt.1)      chem(i,k,j,p_ant1_c_a04)=0.0
                   if (p_biog1_c_a04.gt.1)     chem(i,k,j,p_biog1_c_a04)=0.0
                   
                   if (p_pcg1_b_c_a05.gt.1)    chem(i,k,j,p_pcg1_b_c_a05)=0.0
                   if (p_pcg1_b_o_a05.gt.1)    chem(i,k,j,p_pcg1_b_o_a05)=0.0
                   if (p_opcg1_b_c_a05.gt.1)   chem(i,k,j,p_opcg1_b_c_a05)=0.0
                   if (p_opcg1_b_o_a05.gt.1)   chem(i,k,j,p_opcg1_b_o_a05)=0.0
                   if (p_pcg1_f_c_a05.gt.1)    chem(i,k,j,p_pcg1_f_c_a05)=0.0
                   if (p_pcg1_f_o_a05.gt.1)    chem(i,k,j,p_pcg1_f_o_a05)=0.0
                   if (p_opcg1_f_c_a05.gt.1)   chem(i,k,j,p_opcg1_f_c_a05)=0.0
                   if (p_opcg1_f_o_a05.gt.1)   chem(i,k,j,p_opcg1_f_o_a05)=0.0
                   if (p_ant1_c_a05.gt.1)      chem(i,k,j,p_ant1_c_a05)=0.0
                   if (p_biog1_c_a05.gt.1)     chem(i,k,j,p_biog1_c_a05)=0.0
                   
                   if (p_pcg1_b_c_a06.gt.1)    chem(i,k,j,p_pcg1_b_c_a06)=0.0
                   if (p_pcg1_b_o_a06.gt.1)    chem(i,k,j,p_pcg1_b_o_a06)=0.0
                   if (p_opcg1_b_c_a06.gt.1)   chem(i,k,j,p_opcg1_b_c_a06)=0.0
                   if (p_opcg1_b_o_a06.gt.1)   chem(i,k,j,p_opcg1_b_o_a06)=0.0
                   if (p_pcg1_f_c_a06.gt.1)    chem(i,k,j,p_pcg1_f_c_a06)=0.0
                   if (p_pcg1_f_o_a06.gt.1)    chem(i,k,j,p_pcg1_f_o_a06)=0.0
                   if (p_opcg1_f_c_a06.gt.1)   chem(i,k,j,p_opcg1_f_c_a06)=0.0
                   if (p_opcg1_f_o_a06.gt.1)   chem(i,k,j,p_opcg1_f_o_a06)=0.0
                   if (p_ant1_c_a06.gt.1)      chem(i,k,j,p_ant1_c_a06)=0.0
                   if (p_biog1_c_a06.gt.1)     chem(i,k,j,p_biog1_c_a06)=0.0
                   
                   if (p_pcg1_b_c_a07.gt.1)    chem(i,k,j,p_pcg1_b_c_a07)=0.0
                   if (p_pcg1_b_o_a07.gt.1)    chem(i,k,j,p_pcg1_b_o_a07)=0.0
                   if (p_opcg1_b_c_a07.gt.1)   chem(i,k,j,p_opcg1_b_c_a07)=0.0
                   if (p_opcg1_b_o_a07.gt.1)   chem(i,k,j,p_opcg1_b_o_a07)=0.0
                   if (p_pcg1_f_c_a07.gt.1)    chem(i,k,j,p_pcg1_f_c_a07)=0.0
                   if (p_pcg1_f_o_a07.gt.1)    chem(i,k,j,p_pcg1_f_o_a07)=0.0
                   if (p_opcg1_f_c_a07.gt.1)   chem(i,k,j,p_opcg1_f_c_a07)=0.0
                   if (p_opcg1_f_o_a07.gt.1)   chem(i,k,j,p_opcg1_f_o_a07)=0.0
                   if (p_ant1_c_a07.gt.1)      chem(i,k,j,p_ant1_c_a07)=0.0
                   if (p_biog1_c_a07.gt.1)     chem(i,k,j,p_biog1_c_a07)=0.0
                   
                   if (p_pcg1_b_c_a08.gt.1)    chem(i,k,j,p_pcg1_b_c_a08)=0.0
                   if (p_pcg1_b_o_a08.gt.1)    chem(i,k,j,p_pcg1_b_o_a08)=0.0
                   if (p_opcg1_b_c_a08.gt.1)   chem(i,k,j,p_opcg1_b_c_a08)=0.0
                   if (p_opcg1_b_o_a08.gt.1)   chem(i,k,j,p_opcg1_b_o_a08)=0.0
                   if (p_pcg1_f_c_a08.gt.1)    chem(i,k,j,p_pcg1_f_c_a08)=0.0
                   if (p_pcg1_f_o_a08.gt.1)    chem(i,k,j,p_pcg1_f_o_a08)=0.0
                   if (p_opcg1_f_c_a08.gt.1)   chem(i,k,j,p_opcg1_f_c_a08)=0.0
                   if (p_opcg1_f_o_a08.gt.1)   chem(i,k,j,p_opcg1_f_o_a08)=0.0
                   if (p_ant1_c_a08.gt.1)      chem(i,k,j,p_ant1_c_a08)=0.0
                   if (p_biog1_c_a08.gt.1)     chem(i,k,j,p_biog1_c_a08)=0.0
                   
                   
                   
                   if (p_pcg1_b_c_cw01.gt.1)   chem(i,k,j,p_pcg1_b_c_cw01)=0.0
                   if (p_pcg1_b_o_cw01.gt.1)   chem(i,k,j,p_pcg1_b_o_cw01)=0.0
                   if (p_opcg1_b_c_cw01.gt.1)  chem(i,k,j,p_opcg1_b_c_cw01)=0.0
                   if (p_opcg1_b_o_cw01.gt.1)  chem(i,k,j,p_opcg1_b_o_cw01)=0.0
                   if (p_pcg1_f_c_cw01.gt.1)   chem(i,k,j,p_pcg1_f_c_cw01)=0.0
                   if (p_pcg1_f_o_cw01.gt.1)   chem(i,k,j,p_pcg1_f_o_cw01)=0.0
                   if (p_opcg1_f_c_cw01.gt.1)  chem(i,k,j,p_opcg1_f_c_cw01)=0.0
                   if (p_opcg1_f_o_cw01.gt.1)  chem(i,k,j,p_opcg1_f_o_cw01)=0.0
                   if (p_ant1_c_cw01.gt.1)     chem(i,k,j,p_ant1_c_cw01)=0.0
                   if (p_biog1_c_cw01.gt.1)    chem(i,k,j,p_biog1_c_cw01)=0.0
                   
                   if (p_pcg1_b_c_cw02.gt.1)   chem(i,k,j,p_pcg1_b_c_cw02)=0.0
                   if (p_pcg1_b_o_cw02.gt.1)   chem(i,k,j,p_pcg1_b_o_cw02)=0.0
                   if (p_opcg1_b_c_cw02.gt.1)  chem(i,k,j,p_opcg1_b_c_cw02)=0.0
                   if (p_opcg1_b_o_cw02.gt.1)  chem(i,k,j,p_opcg1_b_o_cw02)=0.0
                   if (p_pcg1_f_c_cw02.gt.1)   chem(i,k,j,p_pcg1_f_c_cw02)=0.0
                   if (p_pcg1_f_o_cw02.gt.1)   chem(i,k,j,p_pcg1_f_o_cw02)=0.0
                   if (p_opcg1_f_c_cw02.gt.1)  chem(i,k,j,p_opcg1_f_c_cw02)=0.0
                   if (p_opcg1_f_o_cw02.gt.1)  chem(i,k,j,p_opcg1_f_o_cw02)=0.0
                   if (p_ant1_c_cw02.gt.1)     chem(i,k,j,p_ant1_c_cw02)=0.0
                   if (p_biog1_c_cw02.gt.1)    chem(i,k,j,p_biog1_c_cw02)=0.0
                   
                   if (p_pcg1_b_c_cw03.gt.1)   chem(i,k,j,p_pcg1_b_c_cw03)=0.0
                   if (p_pcg1_b_o_cw03.gt.1)   chem(i,k,j,p_pcg1_b_o_cw03)=0.0
                   if (p_opcg1_b_c_cw03.gt.1)  chem(i,k,j,p_opcg1_b_c_cw03)=0.0
                   if (p_opcg1_b_o_cw03.gt.1)  chem(i,k,j,p_opcg1_b_o_cw03)=0.0
                   if (p_pcg1_f_c_cw03.gt.1)   chem(i,k,j,p_pcg1_f_c_cw03)=0.0
                   if (p_pcg1_f_o_cw03.gt.1)   chem(i,k,j,p_pcg1_f_o_cw03)=0.0
                   if (p_opcg1_f_c_cw03.gt.1)  chem(i,k,j,p_opcg1_f_c_cw03)=0.0
                   if (p_opcg1_f_o_cw03.gt.1)  chem(i,k,j,p_opcg1_f_o_cw03)=0.0
                   if (p_ant1_c_cw03.gt.1)     chem(i,k,j,p_ant1_c_cw03)=0.0
                   if (p_biog1_c_cw03.gt.1)    chem(i,k,j,p_biog1_c_cw03)=0.0
                   
                   if (p_pcg1_b_c_cw04.gt.1)   chem(i,k,j,p_pcg1_b_c_cw04)=0.0
                   if (p_pcg1_b_o_cw04.gt.1)   chem(i,k,j,p_pcg1_b_o_cw04)=0.0
                   if (p_opcg1_b_c_cw04.gt.1)  chem(i,k,j,p_opcg1_b_c_cw04)=0.0
                   if (p_opcg1_b_o_cw04.gt.1)  chem(i,k,j,p_opcg1_b_o_cw04)=0.0
                   if (p_pcg1_f_c_cw04.gt.1)   chem(i,k,j,p_pcg1_f_c_cw04)=0.0
                   if (p_pcg1_f_o_cw04.gt.1)   chem(i,k,j,p_pcg1_f_o_cw04)=0.0
                   if (p_opcg1_f_c_cw04.gt.1)  chem(i,k,j,p_opcg1_f_c_cw04)=0.0
                   if (p_opcg1_f_o_cw04.gt.1)  chem(i,k,j,p_opcg1_f_o_cw04)=0.0
                   if (p_ant1_c_cw04.gt.1)     chem(i,k,j,p_ant1_c_cw04)=0.0
                   if (p_biog1_c_cw04.gt.1)    chem(i,k,j,p_biog1_c_cw04)=0.0
                   
                   if (p_pcg1_b_c_cw05.gt.1)   chem(i,k,j,p_pcg1_b_c_cw05)=0.0
                   if (p_pcg1_b_o_cw05.gt.1)   chem(i,k,j,p_pcg1_b_o_cw05)=0.0
                   if (p_opcg1_b_c_cw05.gt.1)  chem(i,k,j,p_opcg1_b_c_cw05)=0.0
                   if (p_opcg1_b_o_cw05.gt.1)  chem(i,k,j,p_opcg1_b_o_cw05)=0.0
                   if (p_pcg1_f_c_cw05.gt.1)   chem(i,k,j,p_pcg1_f_c_cw05)=0.0
                   if (p_pcg1_f_o_cw05.gt.1)   chem(i,k,j,p_pcg1_f_o_cw05)=0.0
                   if (p_opcg1_f_c_cw05.gt.1)  chem(i,k,j,p_opcg1_f_c_cw05)=0.0
                   if (p_opcg1_f_o_cw05.gt.1)  chem(i,k,j,p_opcg1_f_o_cw05)=0.0
                   if (p_ant1_c_cw05.gt.1)     chem(i,k,j,p_ant1_c_cw05)=0.0
                   if (p_biog1_c_cw05.gt.1)    chem(i,k,j,p_biog1_c_cw05)=0.0
                   
                   if (p_pcg1_b_c_cw06.gt.1)   chem(i,k,j,p_pcg1_b_c_cw06)=0.0
                   if (p_pcg1_b_o_cw06.gt.1)   chem(i,k,j,p_pcg1_b_o_cw06)=0.0
                   if (p_opcg1_b_c_cw06.gt.1)  chem(i,k,j,p_opcg1_b_c_cw06)=0.0
                   if (p_opcg1_b_o_cw06.gt.1)  chem(i,k,j,p_opcg1_b_o_cw06)=0.0
                   if (p_pcg1_f_c_cw06.gt.1)   chem(i,k,j,p_pcg1_f_c_cw06)=0.0
                   if (p_pcg1_f_o_cw06.gt.1)   chem(i,k,j,p_pcg1_f_o_cw06)=0.0
                   if (p_opcg1_f_c_cw06.gt.1)  chem(i,k,j,p_opcg1_f_c_cw06)=0.0
                   if (p_opcg1_f_o_cw06.gt.1)  chem(i,k,j,p_opcg1_f_o_cw06)=0.0
                   if (p_ant1_c_cw06.gt.1)     chem(i,k,j,p_ant1_c_cw06)=0.0
                   if (p_biog1_c_cw06.gt.1)    chem(i,k,j,p_biog1_c_cw06)=0.0
                   
                   if (p_pcg1_b_c_cw07.gt.1)   chem(i,k,j,p_pcg1_b_c_cw07)=0.0
                   if (p_pcg1_b_o_cw07.gt.1)   chem(i,k,j,p_pcg1_b_o_cw07)=0.0
                   if (p_opcg1_b_c_cw07.gt.1)  chem(i,k,j,p_opcg1_b_c_cw07)=0.0
                   if (p_opcg1_b_o_cw07.gt.1)  chem(i,k,j,p_opcg1_b_o_cw07)=0.0
                   if (p_pcg1_f_c_cw07.gt.1)   chem(i,k,j,p_pcg1_f_c_cw07)=0.0
                   if (p_pcg1_f_o_cw07.gt.1)   chem(i,k,j,p_pcg1_f_o_cw07)=0.0
                   if (p_opcg1_f_c_cw07.gt.1)  chem(i,k,j,p_opcg1_f_c_cw07)=0.0
                   if (p_opcg1_f_o_cw07.gt.1)  chem(i,k,j,p_opcg1_f_o_cw07)=0.0
                   if (p_ant1_c_cw07.gt.1)     chem(i,k,j,p_ant1_c_cw07)=0.0
                   if (p_biog1_c_cw07.gt.1)    chem(i,k,j,p_biog1_c_cw07)=0.0
                   
                   if (p_pcg1_b_c_cw08.gt.1)   chem(i,k,j,p_pcg1_b_c_cw08)=0.0
                   if (p_pcg1_b_o_cw08.gt.1)   chem(i,k,j,p_pcg1_b_o_cw08)=0.0
                   if (p_opcg1_b_c_cw08.gt.1)  chem(i,k,j,p_opcg1_b_c_cw08)=0.0
                   if (p_opcg1_b_o_cw08.gt.1)  chem(i,k,j,p_opcg1_b_o_cw08)=0.0
                   if (p_pcg1_f_c_cw08.gt.1)   chem(i,k,j,p_pcg1_f_c_cw08)=0.0
                   if (p_pcg1_f_o_cw08.gt.1)   chem(i,k,j,p_pcg1_f_o_cw08)=0.0
                   if (p_opcg1_f_c_cw08.gt.1)  chem(i,k,j,p_opcg1_f_c_cw08)=0.0
                   if (p_opcg1_f_o_cw08.gt.1)  chem(i,k,j,p_opcg1_f_o_cw08)=0.0
                   if (p_ant1_c_cw08.gt.1)     chem(i,k,j,p_ant1_c_cw08)=0.0
                   if (p_biog1_c_cw08.gt.1)    chem(i,k,j,p_biog1_c_cw08)=0.0
                enddo
             enddo
          enddo
       endif
       !BSINGH - ENDS

   END SELECT kpp_select
   endif

   IF( config_flags%do_pvozone .and. p_o3.gt.1 .and. (.not. config_flags%restart) ) THEN
      si_zsigf = (grid%ph_1 + grid%phb)/grav
      do k=1,kde-1
         si_zsig(:,k,:) = 0.5 * ( si_zsigf(:,k,:) + si_zsigf(:,k+1,:) )
      enddo
      si_zsig(:,kde,:) = 0.5 * ( 3. * si_zsigf(:,kde,:) - si_zsigf(:,kde-1,:) )
      CALL initial_pvo3 (ims, ime, jms, jme, kms, kme, num_chem, numgas, &
                              grid%chem_opt, si_zsig, chem(:,:,:,p_o3),       &
                              grid%u_2, grid%v_2, grid%t_2+t0, p, grid%pb,    &
                              grid%znu, grid%msft, grid%msfu, grid%msfv,      &
                              grid%f, grid%mub,grid%dx,xlat,grid%julday,      &
                              ids,ide,jds,jde,kds,kde,its,ite,jts,jte,kts,kte)

   ENDIF
   
!! Initialize some greenhouse gas species for 16th and 17th chemistry options:
!! CO2 mixing ratios for the background GHG tracers are set as a constant value.
!! Some spin-up is necessary to get spatial variability right!
!! Warning: initial values for CO2 components have to be subtracted from the output!
!! The purpose of this is to avoid negative values due to the biospheric uptake!
!! This subroutine is called at every initialization, however it's carried out if
!! restart=0 and chem_in_opt=0
ghg_block: IF (config_flags%gas_ic_opt==GAS_IC_GHG) THEN
   IF( (.not. config_flags%restart) .AND. config_flags%chem_in_opt==0 ) THEN
! CO2 tracers:
       do j=jts,jte
        do k=kts,kte
         do i=its,ite
            chem(i,k,j,p_co2_bck)=380.  ! background CO2 tracer
            chem(i,k,j,p_co2_bio)=380.  ! A user should subtract this from the output fields!
            chem(i,k,j,p_co2_oce)=380.   ! A user should subtract this from the output fields!
            chem(i,k,j,p_co2_ant)=380.   ! A user should subtract this from the output fields!
            chem(i,k,j,p_co2_tst)=380.   ! A user should subtract this from the output fields!

            if (p_co2_bbu>1) chem(i,k,j,p_co2_bbu)=380.  ! A user should subtract this from the output fields!

         enddo
        enddo
       enddo

! CO tracers:
       do j=jts,jte
        do k=kts,kte
         do i=its,ite
            chem(i,k,j,p_co_bck)=0.1  ! background CO tracer
            chem(i,k,j,p_co_ant)=0.1  ! A user should subtract this from the output fields!
            if (p_co_tst>1) chem(i,k,j,p_co_tst)=0.1     ! A user should subtract this from the output fields!
            if (p_co_bbu>1) chem(i,k,j,p_co_bbu)=0.1     ! A user should subtract this from the output fields!
         enddo
        enddo
       enddo

! CH4 tracers:
     IF (p_ch4_bck.gt.1) THEN
      do j=jts,jte
       do k=kts,kte
        do i=its,ite
           chem(i,k,j,p_ch4_bck)=1.77   ! background CH4 tracer
           chem(i,k,j,p_ch4_bio)=1.77   ! A user should subtract this from the output fields!
           chem(i,k,j,p_ch4_ant)=1.77   ! A user should subtract this from the output fields!
           chem(i,k,j,p_ch4_bbu)=1.77   ! A user should subtract this from the output fields!
           chem(i,k,j,p_ch4_tst)=1.77   ! A user should subtract this from the output fields!
        enddo
       enddo
      enddo
      ENDIF

   ENDIF
ENDIF ghg_block
!!
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
     CASE (GOCARTRACM_KPP,GOCARTRADM2)
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
     CASE (DUST)
       if(config_flags%phot_opt .NE. 0 )then
         call wrf_error_fatal("Dust only simple initialization, phot_opt  MUST BE ZERO")
       endif
       CALL wrf_debug(15,'call dust aerosols initialization')
        ch_dust(:,:)=0.8D-9
        if(config_flags%chem_in_opt == 0 )then
        if( .NOT. config_flags%restart )then
           do j=jts,jte
              do k=kts,kte
                 do i=its,ite
                   do n=1,num_chem
                     chem(i,k,j,n)=0.
                   enddo
                 enddo
               enddo
           enddo
         endif
         endif
     CASE (GOCART_SIMPLE)
       if(config_flags%phot_opt .NE. 0 )then
         call wrf_error_fatal("GOCART simple initialization, phot_opt  MUST BE ZERO")
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
!                if(i.eq.19.and.j.eq.19)write(0,*)'in cheminit'
!                if(i.eq.19.and.j.eq.19)write(0,*)TCOSZ(i,j),ttday(i,j),julday, gmtp, sza, cosszax,xlonn,rlat
           enddo
          enddo
      CASE (MOZCART_KPP,T1_MOZCART_KPP)
        CALL wrf_debug(15,'MOZCART dust initialization')
        ch_dust(:,:) = 0.8D-9

     CASE (RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, RADM2SORG_KPP, &
           RACMSORG_KPP, RACM_ESRLSORG_KPP, CBMZSORG, CBMZSORG_AQ, &
           CB05_SORG_AQ_KPP)
       CALL wrf_debug(15,'call MADE/SORGAM aerosols initialization')

       call aerosols_sorgam_init(chem,convfac,z_at_w,                &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               chem_in_opt,config_flags%aer_ic_opt,is_aerosol,       &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte, config_flags               )

!!!TUCCELLA (BUG, before it was called in module_aerosols_sorgam.F)
       ! initialize pointers used by aerosol-cloud-interaction routines
        call aerosols_sorgam_init_aercld_ptrs( &
           num_chem, is_aerosol, config_flags )

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
!
    CASE (CB05_SORG_VBS_AQ_KPP)
       CALL wrf_debug(15,'call MADE/SOA_VBS aerosols initialization')

       call aerosols_sorgam_vbs_init(chem,convfac,z_at_w,                &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               tsoa,asoa,bsoa,                                       &
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
    CASE (RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP)
       CALL wrf_debug(15,'call MADE/SOA_VBS aerosols initialization')

       call aerosols_soa_vbs_init(chem,convfac,z_at_w,                &
               pm2_5_dry,pm2_5_water,pm2_5_dry_ec,                   &
               chem_in_opt,config_flags%aer_ic_opt,is_aerosol,       &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte, config_flags               )

!!!TUCCELLA (BUG, before it was called in module_aerosols_soa_vbs.F)
       ! initialize pointers used by aerosol-cloud-interaction routines
        call aerosols_soa_vbs_init_aercld_ptrs( &
           num_chem, is_aerosol, config_flags )

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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     CASE ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
       call wrf_debug(15,'call CAM_MODAL aerosols initialization')
       call cam_mam_init(                                            &
               id, numgas, config_flags,                             &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
       if(config_flags%chem_in_opt == 0 )then
           if( .NOT. config_flags%restart ) &
           call cam_mam_initmixrats(                                 &
               id, numgas, config_flags,                             &
               chem, convfac, alt, z_at_w, g,                        &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
       endif
       if(config_flags%wetscav_onoff == 1  ) then 
          if(config_flags%mp_physics == CAMMGMPSCHEME ) then
             call wetscav_cam_mam_driver_init(ids,ide, jds,jde, kds,kde, &
                  ims,ime, jms,jme, kms,kme,                             &
                  its,ite, jts,jte, kts,kte                              )
          else
             call wrf_error_fatal("ERROR: wet scavaging option requires mp_phys = CAMMGMP SCHEME to function.")
          endif
       endif
     CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
           CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
           SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
           MOZART_MOSAIC_4BIN_KPP,MOZART_MOSAIC_4BIN_AQ_KPP, &
           CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP, &  !BSINGH(12/03/2013): Added SAPRC 8 bin
           SAPRC99_MOSAIC_8BIN_VBS2_KPP) !BSINGH(04/03/2014): Added SAPRC 8 bin non-aq
       call wrf_debug(15,'call MOSAIC aerosols initialization')
       call init_data_mosaic_asect( id, config_flags, grid%vbs_nbin, is_aerosol )
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

!!! TUCCELLA
   progn_sanity_check : SELECT CASE(config_flags%chem_opt)
   CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, &
         RACM_SOA_VBS_AQCHEM_KPP, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZ_MOSAIC_DMS_4BIN_AQ, &
         CBMZ_MOSAIC_DMS_8BIN_AQ,CBMZSORG_AQ,CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, &
         MOZART_MOSAIC_4BIN_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,   &
         CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
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
        ch_dust(:,:)=0.8D-9
        ch_ss(:,:)=1.

!
! initialization for wesely (gas) dry deposition
!
   drydep_select: SELECT CASE(config_flags%gas_drydep_opt)
     CASE (WESELY)
       CALL wrf_debug(15,'initializing dry dep (wesely)')

        call dep_init( id, config_flags, numgas, mminlu_loc, &
                      its, ite, jts, jte, ide, jde )
!--

   END SELECT drydep_select
!
! initialization for cbmz gas-phase chemistry
!
   cbmz_select: SELECT CASE(config_flags%chem_opt)
     CASE (CBMZ, CBMZ_BB, CBMZ_BB_KPP,CBMZ_MOSAIC_KPP,  CBMZ_MOSAIC_4BIN, &
           CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZSORG,CBMZSORG_AQ, &
           CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, & 
           CBMZ_MOSAIC_DMS_8BIN_AQ, CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
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
!BSINGH(12/03/2013): send chem_in_opt as an arg
!    For chem_in_opt==1 modified the mosaic_mixactivate_init
!    The sum of mass from interstitial and cloud borne aerosols is attributed to interstitial aerosols
!    Cloud drop number and cloud aerosol mass are then initialized to zero
       call mosaic_mixactivate_init(                   &
            config_flags, chem, scalar,                &
            chem_in_opt,                               & !BSINGH(12/03/2013):send chem_in_opt as an arg
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

! 
!-- initialization upper bounary and tropopause modules
!
    if( config_flags%have_bcs_upper ) then
        CALL wrf_debug(00,'call upper boundary initialization')
        call upper_bc_init( id, xlat, dt, config_flags,  &
                            ids,ide, jds,jde, kds,kde,   &
                            ims,ime, jms,jme, kms,kme,   &
                            its,ite, jts,jte, kts,kte    )

        call tropopause_init( id, xlat, xlong, config_flags, &
                            ids,ide, jds,jde, kds,kde,       &
                            ims,ime, jms,jme, kms,kme,       &
                            its,ite, jts,jte, kts,kte        )
    end if
    gas_pcnst_modal_aero_pos = max(1,gas_pcnst_modal_aero) !BSINGH - Fix for non-MAM simulations
    END SUBROUTINE chem_init

!---------------------------------------------------------------------------------------------
!Subroutine initializes VPRM parameters based on namelist option for "vprm_opt"
!
SUBROUTINE VPRM_par_initialize(rad_vprm,lambda_vprm,alpha_vprm,resp_vprm,config_flags)

USE module_configure,only:  grid_config_rec_type

! VPRM parameters initialization section
IMPLICIT NONE

REAL, DIMENSION(8), INTENT(OUT) ::  rad_vprm, lambda_vprm, alpha_vprm, resp_vprm
REAL, DIMENSION(8,4)    ::  vprm_table_us, vprm_table_europe, vprm_table_tropics, vprm_par

DATA vprm_table_us &
     / 261.0, 324.0, 206.0, 363.0, 682.0, 757.0, 157.0, 0.0, &
      -0.2492, -0.1729, -0.2555, -0.08736, -0.1141, -0.15330, -0.13335, 0.00000, &
       0.3301, 0.3258, 0.3422, 0.0239, 0.0049, 0.2680, 0.0269, 0.0000, &
       0., 0., 0., 0., 0., 0., 0., 0. /

DATA vprm_table_europe &
     / 270.2, 271.4, 236.6, 363.0, 682.0, 690.3, 229.1, 0.0, &
      -0.3084, -0.1955, -0.2856, -0.0874, -0.1141, -0.1350, -0.1748, 0.0000, &
       0.1797, 0.1495, 0.2258, 0.0239, 0.0049, 0.1699, 0.0881, 0.0000, &
       0.8800, 0.8233, 0.4321, 0.0000, 0.0000, -0.0144, 0.5843, 0.0000 /

! Tropics are still preliminary, too strong SWDOWN might cause too high uptake
DATA vprm_table_tropics &
     / 501.0, 324.0, 206.0, 303.0, 682.0, 646.0, 157.0, 0.0, &
      -0.2101, -0.1729, -0.2555, -0.0874, -0.1141, -0.1209, -0.1334, 0.0000, &
       0.1601, 0.3258, 0.3422, 0.0239, 0.0049, 0.0043, 0.0269, 0.0000, &
       0., 0., 0., 0., 0., 0., 0., 0. /

TYPE (grid_config_rec_type) , INTENT (IN) :: config_flags

sel_pars: SELECT CASE(config_flags%vprm_opt)
CASE ('VPRM_table_US')
  vprm_par=vprm_table_us
CASE ('VPRM_table_EUROPE')
  vprm_par=vprm_table_europe
CASE ('VPRM_table_TROPICS')
  vprm_par=vprm_table_tropics
CASE DEFAULT
  CALL wrf_message("check vprm_opt in namelist.input")
  CALL wrf_error_fatal ( "NO PARAMETER TABLE IS INCLUDED FOR THIS VPRM TABLE OPTION!")
END SELECT sel_pars

rad_vprm=     vprm_par(1:8,1)
lambda_vprm=  vprm_par(1:8,2)
alpha_vprm=   vprm_par(1:8,3)
resp_vprm=    vprm_par(1:8,4)

END SUBROUTINE VPRM_par_initialize
!******************************************************************************
!
SUBROUTINE termite_initialize( biom,emch4,config_flags)
!Subroutine initializes the termite emission parameters provided by Sanderson et al. 1996
! parameters for termite emission initialization

USE module_configure,only:  grid_config_rec_type

IMPLICIT NONE

REAL, DIMENSION(14,3)   ::  term_em
REAL, DIMENSION(14)     ::  biom, emch4

DATA term_em &
      /11.0, 8.0,  11.26, 3.0,  0.96, 10.6,  5.2,  0.98, 8.43, 5.38, 2.25, 5.3, 2.7,  5.3,  &
       5.64, 5.64, 5.64, 1.77, 2.9, 3.2, 1.77, 2.9, 3.2, 3.0, 3.0, 4.13, 4.13, 4.13, &
       6.16, 6.16, 6.16, 1.77, 7.60,  7.00, 1.77, 7.60, 7.0, 3.9, 3.9, 4.13,  4.13,  4.13/

TYPE (grid_config_rec_type) , INTENT (IN) :: config_flags

sel_pars: SELECT CASE(config_flags%term_opt)
 CASE ('CH4_termite_NW')
   biom= term_em(1:14,1)
   emch4 = term_em(1:14,2)
 CASE ('CH4_termite_OW')
   biom= term_em(1:14,1)
   emch4 = term_em(1:14,3)
 CASE DEFAULT
   CALL wrf_error_fatal ( "NO PARAMETER TABLE IS INCLUDED FOR THIS TERMITE CH4 OPTION!")
END SELECT sel_pars

END SUBROUTINE termite_initialize
!-----------------------------------------------------------------------
!
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
  case (RADM2, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2_KPP, RADM2SORG_KPP, GOCARTRADM2)
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
!!! TUCCELLA
  case (RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP,RACM_KPP,RACMPM_KPP, RACMSORG_KPP, &
        RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP)
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
  case (CBMZ_BB, CBMZ_BB_KPP, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZSORG,CBMZSORG_AQ, &
        CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
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
     print*,p_ch4,"ch4"
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

     case (SAPRC99_MOSAIC_8BIN_VBS2_KPP) !BSINGh -  Added for 8bin vbs pkg
     print*,p_o3,"o3"
     print*,p_h2o2,"h2o2"
     print*,p_no,"no"
     print*,p_no2,"no2"
     print*,p_no3,"no3"
     print*,p_n2o5,"n2o5"
     print*,p_hono,"hono"
     print*,p_hno3,"hno3"
     print*,p_hno4,"hno4"
     print*,p_so2,"so2"
     print*,p_h2so4,"h2so4"
     print*,p_co,"co"
     print*,p_hcho,"hcho"
     print*,p_ccho,"ccho"
     print*,p_rcho,"rcho"
     print*,p_acet,"acet"
     print*,p_mek,"mek"
     print*,p_hcooh,"hcooh"
     print*,p_meoh,"meoh"
     print*,p_etoh,"etoh"
     print*,p_cco_oh,"cco_oh"
     print*,p_rco_oh,"rco_oh"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_bacl,"bacl"
     print*,p_cres,"cres"
     print*,p_bald,"bald"
     print*,p_isoprod,"isoprod"
     print*,p_methacro,"methacro"
     print*,p_mvk,"mvk"
     print*,p_prod2,"prod2"
     print*,p_dcb1,"dcb1"
     print*,p_dcb2,"dcb2"
     print*,p_dcb3,"dcb3"
     print*,p_ethene,"ethene"
     print*,p_isoprene,"isoprene"
     print*,p_c2h6,"c2h6"
     print*,p_c3h8,"c3h8"
     print*,p_c2h2,"c2h2"
     print*,p_c3h6,"c3h6"
     print*,p_alk3,"alk3"
     print*,p_alk4,"alk4"
     print*,p_alk5,"alk5"
     print*,p_aro1,"aro1"
     print*,p_aro2,"aro2"
     print*,p_ole1,"ole1"
     print*,p_ole2,"ole2"
     print*,p_terp,"terp"
     print*,p_sesq,"sesq"
     print*,p_rno3,"rno3"
     print*,p_nphe,"nphe"
     print*,p_phen,"phen"
     print*,p_pan,"pan"
     print*,p_pan2,"pan2"
     print*,p_pbzn,"pbzn"
     print*,p_ma_pan,"ma_pan"
     print*,p_bc_spr,"bc_spr"
     print*,p_oc_spr,"oc_spr"
     print*,p_ssf,"ssf"
     print*,p_ssc,"ssc"
     print*,p_pm10_spr,"pm10_spr"
     print*,p_pm25_spr,"pm25_spr"
     print*,p_dms,"dms"
     print*,p_dst1,"dst1"
     print*,p_dst2,"dst2"
     print*,p_dst3,"dst3"
     print*,p_co2,"co2"
     print*,p_cco_ooh,"cco_ooh"
     print*,p_rco_o2,"rco_o2"
     print*,p_rco_ooh,"rco_ooh"
     print*,p_xn,"xn"
     print*,p_xc,"xc"
     print*,p_ho,"oh"
     print*,p_ho2,"ho2"
     print*,p_c_o2,"c_o2"
     print*,p_cooh,"cooh"
     print*,p_rooh,"rooh"
     print*,p_ro2_r,"ro2_r"
     print*,p_r2o2,"r2o2"
     print*,p_ro2_n,"ro2_n"
     print*,p_cco_o2,"cco_o2"
     print*,p_bzco_o2,"bzco_o2"
     print*,p_ma_rco3,"ma_rco3"
     print*,p_ch4,"ch4"

  select case (chem_opt)
  case( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
        print*,p_soag,"soag"
     end select
  end select
  
  case (CBMZ_MOSAIC_KPP)
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
     print*,p_ch4,"ch4"
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
     print*,p_aro1,"aro1"
     print*,p_aro2,"aro2"
     print*,p_alk1,"alk1"
     print*,p_ole1,"ole1"
     print*,p_api1,"api1"
     print*,p_api2,"api2"
     print*,p_lim1,"lim1"
     print*,p_lim2,"lim2"
     print*,p_api,"api"
     print*,p_lim,"lim"

  case (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP,SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin 
     print*,p_o3,"o3"
     print*,p_h2o2,"h2o2"
     print*,p_no,"no"
     print*,p_no2,"no2"
     print*,p_no3,"no3"
     print*,p_n2o5,"n2o5"
     print*,p_hono,"hono"
     print*,p_hno3,"hno3"
     print*,p_hno4,"hno4"
     print*,p_so2,"so2"
     print*,p_h2so4,"h2so4"
     print*,p_co,"co"
     print*,p_hcho,"hcho"
     print*,p_ccho,"ccho"
     print*,p_rcho,"rcho"
     print*,p_acet,"acet"
     print*,p_mek,"mek"
     print*,p_hcooh,"hcooh"
     print*,p_meoh,"meoh"
     print*,p_etoh,"etoh"
     print*,p_cco_oh,"cco_oh"
     print*,p_rco_oh,"rco_oh"
     print*,p_gly,"gly"
     print*,p_mgly,"mgly"
     print*,p_bacl,"bacl"
     print*,p_cres,"cres"
     print*,p_bald,"bald"
     print*,p_isoprod,"isoprod"
     print*,p_methacro,"methacro"
     print*,p_mvk,"mvk"
     print*,p_prod2,"prod2"
     print*,p_dcb1,"dcb1"
     print*,p_dcb2,"dcb2"
     print*,p_dcb3,"dcb3"
     print*,p_ethene,"ethene"
     print*,p_isoprene,"isoprene"
     print*,p_c2h6,"c2h6"
     print*,p_c3h8,"c3h8"
     print*,p_c2h2,"c2h2"
     print*,p_c3h6,"c3h6"
     print*,p_alk3,"alk3"
     print*,p_alk4,"alk4"
     print*,p_alk5,"alk5"
     print*,p_aro1,"aro1"
     print*,p_aro2,"aro2"
     print*,p_ole1,"ole1"
     print*,p_ole2,"ole2"
     print*,p_terp,"terp"
     print*,p_sesq,"sesq"
     print*,p_rno3,"rno3"
     print*,p_nphe,"nphe"
     print*,p_phen,"phen"
     print*,p_pan,"pan"
     print*,p_pan2,"pan2"
     print*,p_pbzn,"pbzn"
     print*,p_ma_pan,"ma_pan"
     print*,p_bc_spr,"bc_spr"
     print*,p_oc_spr,"oc_spr"
     print*,p_ssf,"ssf"
     print*,p_ssc,"ssc"
     print*,p_pm10_spr,"pm10_spr"
     print*,p_pm25_spr,"pm25_spr"
     print*,p_dms,"dms"
     print*,p_dst1,"dst1"
     print*,p_dst2,"dst2"
     print*,p_dst3,"dst3"
     print*,p_co2,"co2"
     print*,p_cco_ooh,"cco_ooh"
     print*,p_rco_o2,"rco_o2"
     print*,p_rco_ooh,"rco_ooh"
     print*,p_xn,"xn"
     print*,p_xc,"xc"
     print*,p_ho,"oh"
     print*,p_ho2,"ho2"
     print*,p_c_o2,"c_o2"
     print*,p_cooh,"cooh"
     print*,p_rooh,"rooh"
     print*,p_ro2_r,"ro2_r"
     print*,p_r2o2,"r2o2"
     print*,p_ro2_n,"ro2_n"
     print*,p_cco_o2,"cco_o2"
     print*,p_bzco_o2,"bzco_o2"
     print*,p_ma_rco3,"ma_rco3"
     print*,p_ch4,"ch4"
  case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
     print*,p_nh3,"nh3"
  end select
!
! Aerosol species...
!
  select case (chem_opt)
  case (RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, &
        RACM_ESRLSORG_AQCHEM_KPP, RACMSORG_KPP, RACM_ESRLSORG_KPP, CBMZSORG, CBMZSORG_AQ,RACMSORG,  &
        CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
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
  case (MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP)
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
  case (CBMZ_MOSAIC_KPP)
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
     print*,p_aro1_a01,"aro1_a01"
     print*,p_aro2_a01,"aro2_a01"
     print*,p_alk1_a01,"alk1_a01"
     print*,p_ole1_a01,"ole1_a01"
     print*,p_api1_a01,"api1_a01"
     print*,p_api2_a01,"api2_a01"
     print*,p_lim1_a01,"lim1_a01"
     print*,p_lim2_a01,"lim2_a01"
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
     print*,p_aro1_a02,"aro1_a02"
     print*,p_aro2_a02,"aro2_a02"
     print*,p_alk1_a02,"alk1_a02"
     print*,p_ole1_a02,"ole1_a02"
     print*,p_api1_a02,"api1_a02"
     print*,p_api2_a02,"api2_a02"
     print*,p_lim1_a02,"lim1_a02"
     print*,p_lim2_a02,"lim2_a02"
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
     print*,p_aro1_a03,"aro1_a03"
     print*,p_aro2_a03,"aro2_a03"
     print*,p_alk1_a03,"alk1_a03"
     print*,p_ole1_a03,"ole1_a03"
     print*,p_api1_a03,"api1_a03"
     print*,p_api2_a03,"api2_a03"
     print*,p_lim1_a03,"lim1_a03"
     print*,p_lim2_a03,"lim2_a03"
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
     print*,p_aro1_a04,"aro1_a04"
     print*,p_aro2_a04,"aro2_a04"
     print*,p_alk1_a04,"alk1_a04"
     print*,p_ole1_a04,"ole1_a04"
     print*,p_api1_a04,"api1_a04"
     print*,p_api2_a04,"api2_a04"
     print*,p_lim1_a04,"lim1_a04"
     print*,p_lim2_a04,"lim2_a04"
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
     print*,p_aro1_a05,"aro1_a05"
     print*,p_aro2_a05,"aro2_a05"
     print*,p_alk1_a05,"alk1_a05"
     print*,p_ole1_a05,"ole1_a05"
     print*,p_api1_a05,"api1_a05"
     print*,p_api2_a05,"api2_a05"
     print*,p_lim1_a05,"lim1_a05"
     print*,p_lim2_a05,"lim2_a05"
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
     print*,p_aro1_a06,"aro1_a06"
     print*,p_aro2_a06,"aro2_a06"
     print*,p_alk1_a06,"alk1_a06"
     print*,p_ole1_a06,"ole1_a06"
     print*,p_api1_a06,"api1_a06"
     print*,p_api2_a06,"api2_a06"
     print*,p_lim1_a06,"lim1_a06"
     print*,p_lim2_a06,"lim2_a06"
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
     print*,p_aro1_a07,"aro1_a07"
     print*,p_aro2_a07,"aro2_a07"
     print*,p_alk1_a07,"alk1_a07"
     print*,p_ole1_a07,"ole1_a07"
     print*,p_api1_a07,"api1_a07"
     print*,p_api2_a07,"api2_a07"
     print*,p_lim1_a07,"lim1_a07"
     print*,p_lim2_a07,"lim2_a07"
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
     print*,p_aro1_a08,"aro1_a08"
     print*,p_aro2_a08,"aro2_a08"
     print*,p_alk1_a08,"alk1_a08"
     print*,p_ole1_a08,"ole1_a08"
     print*,p_api1_a08,"api1_a08"
     print*,p_api2_a08,"api2_a08"
     print*,p_lim1_a08,"lim1_a08"
     print*,p_lim2_a08,"lim2_a08"
     print*,p_ca_a01,"ca_a01"
     print*,p_ca_a02,"ca_a02"
     print*,p_ca_a03,"ca_a03"
     print*,p_ca_a04,"ca_a04"
     print*,p_ca_a05,"ca_a05"
     print*,p_ca_a06,"ca_a06"
     print*,p_ca_a07,"ca_a07"
     print*,p_ca_a08,"ca_a08"
     print*,p_co3_a01,"co3_a01"
     print*,p_co3_a02,"co3_a02"
     print*,p_co3_a03,"co3_a03"
     print*,p_co3_a04,"co3_a04"
     print*,p_co3_a05,"co3_a05"
     print*,p_co3_a06,"co3_a06"
     print*,p_co3_a07,"co3_a07"
     print*,p_co3_a08,"co3_a08"

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

  case ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ )
     print*,p_so4_a1,"so4_a1"
     print*,p_pom_a1,"pom_a1"
     print*,p_soa_a1,"soa_a1"
     print*,p_bc_a1, "bc_a1"
     print*,p_dst_a1,"dst_a1"
     print*,p_ncl_a1,"ncl_a1"
     print*,p_wtr_a1,"wtr_a1"
     print*,p_num_a1,"num_a1"
     print*,p_so4_a2,"so4_a2"
     print*,p_soa_a1,"soa_a2"
     print*,p_wtr_a1,"wtr_a2"
     print*,p_num_a2,"num_a2"
     print*,p_dst_a3,"dst_a3"
     print*,p_ncl_a3,"ncl_a3"
     print*,p_so4_a3,"so4_a3"
     print*,p_wtr_a1,"wtr_a3"
     print*,p_num_a3,"num_a3"

  case ( CBMZ_CAM_MAM3_AQ )
     print*,p_so4_c1,"so4_c1"
     print*,p_pom_c1,"pom_c1"
     print*,p_soa_c1,"soa_c1"
     print*,p_bc_c1, "bc_c1"
     print*,p_dst_c1,"dst_c1"
     print*,p_ncl_c1,"ncl_c1"
     print*,p_num_c1,"num_c1"
     print*,p_so4_c2,"so4_c2"
     print*,p_soa_c1,"soa_c2"
     print*,p_num_c2,"num_c2"
     print*,p_dst_c3,"dst_c3"
     print*,p_ncl_c3,"ncl_c3"
     print*,p_so4_c3,"so4_c3"
     print*,p_num_c3,"num_c3"

  case ( CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
     print*,p_so4_a1,"so4_a1"
     print*,p_nh4_a1,"nh4_a1"
     print*,p_pom_a1,"pom_a1"
     print*,p_soa_a1,"soa_a1"
     print*,p_bc_a1, "bc_a1"
     print*,p_ncl_a1,"ncl_a1"
     print*,p_wtr_a1,"wtr_a1"
     print*,p_num_a1,"num_a1"
     print*,p_so4_a2,"so4_a2"
     print*,p_nh4_a2,"nh4_a2"
     print*,p_soa_a2,"soa_a2"
     print*,p_ncl_a2,"ncl_a2"
     print*,p_wtr_a2,"wtr_a2"
     print*,p_num_a2,"num_a2"
     print*,p_pom_a3,"pom_a3"
     print*,p_bc_a3, "bc_a3"
     print*,p_wtr_a3,"wtr_a3"
     print*,p_num_a3,"num_a3"
     print*,p_ncl_a4,"ncl_a4"
     print*,p_so4_a4,"so4_a4"
     print*,p_nh4_a4,"nh4_a4"
     print*,p_wtr_a4,"wtr_a4"
     print*,p_num_a4,"num_a4"
     print*,p_dst_a5,"dst_a5"
     print*,p_so4_a5,"so4_a5"
     print*,p_nh4_a5,"nh4_a5"
     print*,p_wtr_a5,"wtr_a5"
     print*,p_num_a5,"num_a5"
     print*,p_ncl_a6,"ncl_a6"
     print*,p_so4_a6,"so4_a6"
     print*,p_nh4_a6,"nh4_a6"
     print*,p_wtr_a6,"wtr_a6"
     print*,p_num_a6,"num_a6"
     print*,p_dst_a7,"dst_a7"
     print*,p_so4_a7,"so4_a7"
     print*,p_nh4_a7,"nh4_a7"
     print*,p_wtr_a7,"wtr_a7"
     print*,p_num_a7,"num_a7"

  case ( CBMZ_CAM_MAM7_AQ )
     print*,p_so4_c1,"so4_c1"
     print*,p_nh4_c1,"nh4_c1"
     print*,p_pom_c1,"pom_c1"
     print*,p_soa_c1,"soa_c1"
     print*,p_bc_c1, "bc_c1"
     print*,p_ncl_c1,"ncl_c1"
     print*,p_num_c1,"num_c1"
     print*,p_so4_c2,"so4_c2"
     print*,p_nh4_c2,"nh4_c2"
     print*,p_soa_c2,"soa_c2"
     print*,p_ncl_c2,"ncl_c2"
     print*,p_num_c2,"num_c2"
     print*,p_pom_c3,"pom_c3"
     print*,p_bc_c3, "bc_c3"
     print*,p_num_c3,"num_c3"
     print*,p_ncl_c4,"ncl_c4"
     print*,p_so4_c4,"so4_c4"
     print*,p_nh4_c4,"nh4_c4"
     print*,p_num_c4,"num_c4"
     print*,p_dst_c5,"dst_c5"
     print*,p_so4_c5,"so4_c5"
     print*,p_nh4_c5,"nh4_c5"
     print*,p_num_c5,"num_c5"
     print*,p_ncl_c6,"ncl_c6"
     print*,p_so4_c6,"so4_c6"
     print*,p_nh4_c6,"nh4_c6"
     print*,p_num_c6,"num_c6"
     print*,p_dst_c7,"dst_c7"
     print*,p_so4_c7,"so4_c7"
     print*,p_nh4_c7,"nh4_c7"
     print*,p_num_c7,"num_c7"

  case (RACM_SOA_VBS_KPP,RACM_SOA_VBS_HET_KPP)
     print*,p_so4aj,"so4aj"
     print*,p_so4ai,"so4ai"
     print*,p_nh4aj,"nh4aj"
     print*,p_nh4ai,"nh4ai"
     print*,p_no3aj,"no3aj"
     print*,p_no3ai,"no3ai"

     print*,p_asoa1j,"asoa1j"
     print*,p_asoa1i,"asoa1i"
     print*,p_asoa2j,"asoa2j"
     print*,p_asoa2i,"asoa2i"
     print*,p_asoa3j,"asoa3j"
     print*,p_asoa3i,"asoa3i"
     print*,p_asoa4j,"asoa4j"
     print*,p_asoa4i,"asoa4i"

     print*,p_bsoa1j,"bsoa1j"
     print*,p_bsoa1i,"bsoa1i"
     print*,p_bsoa2j,"bsoa2j"
     print*,p_bsoa2i,"bsoa2i"
     print*,p_bsoa3j,"bsoa3j"
     print*,p_bsoa3i,"bsoa3i"
     print*,p_bsoa4j,"bsoa4j"
     print*,p_bsoa4i,"bsoa4i"

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
!!! TUCCELLA
 case (RACM_SOA_VBS_AQCHEM_KPP)
     print*,p_so4aj,"so4aj"
     print*,p_so4ai,"so4ai"
     print*,p_nh4aj,"nh4aj"
     print*,p_nh4ai,"nh4ai"
     print*,p_no3aj,"no3aj"
     print*,p_no3ai,"no3ai"

     print*,p_asoa1j,"asoa1j"
     print*,p_asoa1i,"asoa1i"
     print*,p_asoa2j,"asoa2j"
     print*,p_asoa2i,"asoa2i"
     print*,p_asoa3j,"asoa3j"
     print*,p_asoa3i,"asoa3i"
     print*,p_asoa4j,"asoa4j"
     print*,p_asoa4i,"asoa4i"

     print*,p_bsoa1j,"bsoa1j"
     print*,p_bsoa1i,"bsoa1i"
     print*,p_bsoa2j,"bsoa2j"
     print*,p_bsoa2i,"bsoa2i"
     print*,p_bsoa3j,"bsoa3j"
     print*,p_bsoa3i,"bsoa3i"
     print*,p_bsoa4j,"bsoa4j"
     print*,p_bsoa4i,"bsoa4i"

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

     print*,p_so4acwj,"so4acwj"
     print*,p_so4acwi,"so4acwi"
     print*,p_nh4acwj,"nh4acwj"
     print*,p_nh4acwi,"nh4acwi"
     print*,p_no3acwj,"no3acwj"
     print*,p_no3acwi,"no3acwi"

     print*,p_asoa1cwj,"asoa1cwj"
     print*,p_asoa1cwi,"asoa1cwi"
     print*,p_asoa2cwj,"asoa2cwj"
     print*,p_asoa2cwi,"asoa2cwi"
     print*,p_asoa3cwj,"asoa3cwj"
     print*,p_asoa3cwi,"asoa3cwi"
     print*,p_asoa4cwj,"asoa4cwj"
     print*,p_asoa4cwi,"asoa4cwi"

     print*,p_bsoa1cwj,"bsoa1cwj"
     print*,p_bsoa1cwi,"bsoa1cwi"
     print*,p_bsoa2cwj,"bsoa2cwj"
     print*,p_bsoa2cwi,"bsoa2cwi"
     print*,p_bsoa3cwj,"bsoa3cwj"
     print*,p_bsoa3cwi,"bsoa3cwi"
     print*,p_bsoa4cwj,"bsoa4cwj"
     print*,p_bsoa4cwi,"bsoa4cwi"

     print*,p_orgpacwj,"orgpacwj"
     print*,p_orgpacwi,"orgpacwi"
     print*,p_eccwj,"eccwj"
     print*,p_eccwi,"eccwi"
     print*,p_p25cwj,"p25cwj"
     print*,p_p25cwi,"p25cwi"
     print*,p_anthacw,"anthacw"
     print*,p_seascw,"seascw"
     print*,p_soilacw,"soilacw"
     print*,p_nu0cw,"nu0cw"
     print*,p_ac0cw,"ac0cw"
     print*,p_corncw,"corncw"
end select
end subroutine print_chem_species_index
#endif
