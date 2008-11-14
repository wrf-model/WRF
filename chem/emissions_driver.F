!
!   WRF-chem V3.0 : Original version of emission_driver written by Georg Grell (ESRL/GSD)
!                   Further developments, bugfixes and improvements  by
!                   William Gustafson (PNNL), Serena Chung (WSU), 
!                   Saulo Freitas (CPTEC), and Georg Grell
!
!
MODULE module_emissions_driver
   IMPLICIT NONE
CONTAINS

    subroutine emissions_driver(id,ktau,dtstep,DX,                         &
         adapt_step_flag,curr_secs,                                        &
         plumerisefire_frq,stepfirepl,                                     &
         bioemdt,stepbioe,                                                 &
         config_flags,gmt,julday,alt,t_phy,moist,p8w,t8w,u_phy,  &
         v_phy,vvel,e_bio,p_phy,chem,rho_phy,dz8w,ne_area,emis_ant,        &
         tsk,erod,g,emis_seas,emis_dust,                                   &
         ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,            &
         ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,       &
         ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,         &
         ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,         &
         ebu_dms,ebu_sulf,mean_fct_agtf,mean_fct_agef,                     &
         mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,          &
         firesize_agsv,firesize_aggr,                                      &
         u10,v10,ivgtyp,isltyp,gsw,vegfra,rmol,ust,znt,dms_0,              &
         xland,xlat,xlong,z_at_w,z,smois,                                  &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &   
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         ! stuff for MEGAN v2.04                                                 
         T2,swdown,                                                        &
         nmegan,EFmegan,                                                   &
         msebio_isop,                                                      &
         mlai,                                                             &
         pftp_bt, pftp_nt, pftp_sb, pftp_hb,                               &
         mtsa,                                                             &
         mswdown,                                                          &
         mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                   &
         mebio_acet, mebio_mbo, mebio_no,                                  &
         current_month,                                                    &
         ! end stuff for MEGAN v2.04
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam, only : mw_so4_aer
  USE module_model_constants, only : mwdry
  USE module_emissions_anthropogenics
  USE module_bioemi_simple
  USE module_bioemi_beis313
  USE module_bioemi_megan2
  USE module_aerosols_sorgam, only: sorgam_addemiss
  USE module_cbmz_addemiss
  USE gocart_dust
  USE gocart_seasalt
  USE module_dms_emis
  USE module_mosaic_addemiss
  USE module_add_emis_cptec
  USE module_add_emiss_burn
  USE module_plumerise1
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,julday, ne_area,                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                          &
                                  ktau,stepbioe,stepfirepl
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                           &
         INTENT(INOUT ) ::                               e_bio
   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant),&
         INTENT(IN ) ::                                                    &
         emis_ant
   REAL, DIMENSION( ims:ime, jms:jme),&
         INTENT(IN ) ::                                                 &
         dms_0,tsk
   REAL, DIMENSION( ims:ime, jms:jme,3),&
         INTENT(IN ) ::                                                 &
         erod
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_dust),OPTIONAL, &
         INTENT(INOUT ) ::                                                 &
         emis_dust
   REAL, DIMENSION( ims:ime, 1, jms:jme,num_emis_seas),                    &
         OPTIONAL,                                                         &
         INTENT(INOUT ) ::                                                 &
         emis_seas
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         OPTIONAL,                                                         &
         INTENT(INOUT ) ::                                                 &
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,       &
            ebu_dms,ebu_sulf

   REAL, DIMENSION( ims:ime,  jms:jme ),                                   &
         OPTIONAL,                                                         &
         INTENT(IN ) ::                                                    &
           mean_fct_agtf,mean_fct_agef,                                    &
           mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,        &
           firesize_agsv,firesize_aggr

!
! 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                        alt,               &
                                                      t_phy,               &
                                                      p_phy,               &
                                                      dz8w,                &
                                              t8w,p8w,z_at_w , z ,         &
                                              u_phy,v_phy,vvel,rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp,               &
                                                     isltyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN   ) ::                                                 &
                                                     u10,                  &
                                                     v10,                  &
                                                     gsw,                  &
                                                  vegfra,                  &
                                                     rmol,                 &
                                                     ust,                  &
                                                     xland,                &
                                                     xlat,                 &
                                                     xlong,                &
                                                     znt
  REAL, DIMENSION( ims:ime, config_flags%num_soil_layers, jms:jme ) ,      &
      INTENT(INOUT) ::                               smois

   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
         OPTIONAL,                                                         &
          INTENT(INOUT   ) ::                                                 &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no

   ! stuff for MEGAN v2.04...most of these arrays are optional and package dependent 
   !  as declared in registry.chem

   integer, intent(in   ) :: nmegan
   real, dimension (ims:ime, jms:jme , nmegan) ,                       &
         OPTIONAL,                                                         &
        intent(inout) ::                                               &
        EFmegan


   real, dimension (ims:ime, jms:jme ) ,                               &
         OPTIONAL,                                                         &
        intent(in) ::                                                  &
        msebio_isop,                                                   &
        pftp_bt, pftp_nt, pftp_sb, pftp_hb

   real, dimension (ims:ime, jms:jme, 12 ) ,                           &
         OPTIONAL,                                                         &
        intent(in) ::                                                  &
        mlai, mtsa, mswdown

   real, dimension (ims:ime, jms:jme ) ,                               &
         OPTIONAL,                                                         &
        intent(inout) ::                                               &
        mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                &
        mebio_acet, mebio_mbo, mebio_no

   real, dimension (ims:ime, jms:jme ) ,                               &
        intent(in) ::                                                  &
        T2, swdown

   integer, intent(in) :: current_month

   ! end stuff for MEGAN v2.04

      REAL(KIND=8), INTENT(IN   ) ::                                   &
           curr_secs

      REAL, INTENT(IN   ) ::                                           &
           bioemdt, dtstep, dx, gmt, g

      INTEGER, INTENT(IN   ) ::                                        &
           plumerisefire_frq

      LOGICAL, INTENT(IN   ) ::                                        &
           adapt_step_flag
!
! Local variables...
!
      INTEGER :: i, j, k, ksub, dust_emiss_active, seasalt_emiss_active
      REAL :: conv
      CHARACTER (LEN=80) :: message
      LOGICAL :: do_bioemiss, do_plumerisefire

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..

! Flags for turning on or off sea salt and/or dust emissions...
!    >0 = sea salt/dust emissions turned on
! As of May 2008 these only are used for MOSAIC and SORGAM
! the gocart dust and seasalt modules are namelist defined
!
      dust_emiss_active    = 1
      seasalt_emiss_active = 1
!
! Setup the timing flags...
! (methodology is adapated from module_radiation_driver.F)
!
      do_plumerisefire = .false.
      IF ( ktau==1 ) then
         do_plumerisefire = .true.
      ELSE IF ( adapt_step_flag ) THEN
         IF ( (plumerisefire_frq<=0) .or. &
              ( curr_secs+real(dtstep,8)+0.01 >= &
              ( INT( curr_secs/real(plumerisefire_frq*60.,8)+1,8 )*real(plumerisefire_frq*60.,8) ) ) &
              ) then
            do_plumerisefire = .true.
         ENDIF
      ELSE IF ( (MOD(ktau,stepfirepl)==0) .or. (stepfirepl==1) ) THEN
         do_plumerisefire = .true.
      ENDIF

      do_bioemiss = .false.
      IF ( ktau==1 ) then
         do_bioemiss = .true.
      ELSE IF ( adapt_step_flag ) THEN
         IF ( (bioemdt<=0) .or. &
              ( curr_secs+real(dtstep,8)+0.01 >= &
              ( INT( curr_secs/real(bioemdt*60.,8)+1,8 )*real(bioemdt*60.,8) ) ) &
              ) then
            do_bioemiss = .true.
         ENDIF
      ELSE IF ( (MOD(ktau,stepbioe)==0) .or. (stepbioe==1) ) THEN
         do_bioemiss = .true.
      ENDIF
!
! Gocart emissions...
!
      seasalt_select:  SELECT CASE(config_flags%seas_opt)
      CASE (SEASGOCART)
        CALL wrf_debug(15,'Gocart sea salt emissions')
         call gocart_seasalt_driver(ktau,dtstep,config_flags,julday,alt,t_phy,moist,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,z_at_w,                  &
         xland,xlat,xlong,dx,g,emis_seas, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )

      CASE DEFAULT 
        if(seasalt_emiss_active.eq.1) then 
           CALL wrf_debug(15,'MOSAIC or SORGAM sea salt emissions')
        else
           CALL wrf_debug(15,'no sea salt emissions')
        end if
      END SELECT seasalt_select

      dust_select:  SELECT CASE(config_flags%dust_opt)
      CASE (DUSTGOCART)
        CALL wrf_debug(15,'Gocart dust emissions')
        call gocart_dust_driver(ktau,dtstep,config_flags,julday,alt,t_phy,moist,u_phy,  &
         v_phy,chem,rho_phy,dz8w,smois,u10,v10,p8w,erod,                  &
         ivgtyp,isltyp,vegfra,xland,xlat,xlong,gsw,dx,g,emis_dust,        &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
      CASE DEFAULT 
        if(dust_emiss_active.eq.1) then
            CALL wrf_debug(15,'MOSAIC or SORGAM dust emissions')
        else
             CALL wrf_debug(15,'no dust emissions')
        end if
      END SELECT dust_select

      dms_select:  SELECT CASE(config_flags%dmsemis_opt)
      CASE (DMSGOCART)
        CALL wrf_debug(15,'Gocart dms emissions')
        call gocart_dmsemis(dtstep,config_flags,alt,t_phy,u_phy,  &
         v_phy,chem,rho_phy,dz8w,u10,v10,p8w,dms_0,tsk,                  &
         ivgtyp,isltyp,xland,dx,g, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
      CASE DEFAULT 
        CALL wrf_debug(15,'no dms emissions')
      END SELECT dms_select

    ksub=0
!!! ***********   FIRE!!!! ****************************************
!
    fire_select:  SELECT CASE(config_flags%biomass_burn_opt)
     CASE (BIOMASSB)
       if( do_plumerisefire )then
          CALL wrf_debug(15,'fire emissions: calling biomassb')
          write(0,*)ktau,stepfirepl
         call plumerise_driver (id,ktau,dtstep,                            &
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,       &
           ebu_dms,ebu_sulf,mean_fct_agtf,mean_fct_agef,                   &
           mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,        &
           firesize_agsv,firesize_aggr,                                    &
           config_flags, t_phy,moist,                                      &
           chem,rho_phy,vvel,u_phy,v_phy,p_phy,                       &
           emis_ant,z_at_w,z,                                                       &
           ids,ide, jds,jde, kds,kde,                                      &
           ims,ime, jms,jme, kms,kme,                                      &
           its,ite, jts,jte, kts,kte                                       )

       endif
       CALL wrf_debug(15,'fire emissions: adding biomassb emissions')
       call add_emis_burn(id,dtstep,ktau,dz8w,rho_phy,chem,&
            julday,gmt,xlat,xlong,t_phy,p_phy,                           &
            ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,       &
            ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,  &
            ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,    &
            ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,    &
            ebu_dms,ebu_sulf,config_flags%chem_opt,                      &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )
     CASE DEFAULT 
       CALL wrf_debug(15,'no biomass burning')
    END SELECT fire_select
!!**************** END FIRE, BEGIN BIOGENIC EMISSIONS
!
    bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
          CALL wrf_debug(15,'biogenic emissions: calling Gunther1')
          call bio_emissions(id,ktau,dtstep,DX,config_flags,               &
               gmt,julday,t_phy,moist,p8w,t8w,                             &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                      &
               ivgtyp,gsw,vegfra,rmol,ust,znt,xlat,xlong,z_at_w,           &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
       endif
     CASE (BEIS313)
       if( do_bioemiss ) then
!        if (config_flags%chem_opt > RACMSORG .AND. config_flags%chem_opt < 100  ) then !<100: kpp mechs, e.g. RACMSORG_KPP
!        the above test is unsafe
         beis313_check_mechanism_ok: SELECT CASE(config_flags%chem_opt)
            CASE (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_KPP, &
                  RACM, RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG, RACMSORG_AQ, RACMSORG_KPP, &
                  CBM4_KPP, NMHC9_KPP, GOCART_SIMPLE, GOCARTRACM_KPP)
            CASE DEFAULT 
               CALL wrf_error_fatal( &
                  "emissions_driver: beis3.1.3 biogenic emis. implemented for RADM2 & RACM only")
         END SELECT beis313_check_mechanism_ok
         CALL wrf_debug(15,'biogenic emissions: calling beis3.1.3')
         call bio_emissions_beis313(id,config_flags,ktau,curr_secs,    &
               dtstep,julday,gmt,xlat,xlong,t_phy,p_phy,gsw,           &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,   &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
       endif

     CASE (MEGAN2) 
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then                        
         CALL wrf_debug(15,'biogenic emissions: calling megan v2.04')  
         call bio_emissions_megan2(id,config_flags,ktau,dtstep,        &
               curr_secs,julday,gmt,xlat,xlong,p_phy,rho_phy,dz8w,     &
               chem,ne_area,                                           &
               current_month,                                          &
               T2,swdown,                                              &
               nmegan, EFmegan, msebio_isop,                           &
               mlai,                                                   &
               pftp_bt, pftp_nt, pftp_sb, pftp_hb,                     &
               mtsa,                                                   &
               mswdown,                                                &
               mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,         &
               mebio_acet, mebio_mbo, mebio_no,                        &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,                    &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_no,           &
               e_bio,                                                  &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
       endif

     CASE DEFAULT 
       if( do_bioemiss ) &
            e_bio(its:ite,jts:jte,1:ne_area) = 0.
!wig: May need to zero out all ebio_xxx arrays too if they are incorporated
!     into CBMZ/MOSAIC.
                                                     
    END SELECT bioem_select

!!! **************** END BIOGENICS, ADD EMISSIONS FOR VARIOUS PACKAGES
!
    gas_addemiss_select: SELECT CASE(config_flags%chem_opt)
    CASE (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_KPP, &
          RACM, RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG, RACMSORG_AQ, RACMSORG_KPP)
       IF(config_flags%emiss_inpt_opt /= 3 ) then
       IF(config_flags%kemit .GT. kte-ksub) THEN
         k=config_flags%kemit
         write(message,'(" WARNING: EMISSIONS_DRIVER: KEMIT > KTE ",3i6)') kme,kte-ksub,k
         CALL WRF_MESSAGE (message)
       ENDIF
       call wrf_debug(15,'emissions_driver calling add_anthropogenics')
       call add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,chem,&
            emis_ant,ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       call wrf_debug(15,'emissions_driver calling add_biogenics')
       ! Do NOT call add_biogenics if using MEGAN v2.04 biogenic emissions
       ! module
       if ( config_flags%bio_emiss_opt .ne. megan2 ) then 
       call add_biogenics(id,dtstep,dz8w,config_flags, rho_phy,chem,    &
            e_bio,ne_area,                                              &
            ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
            ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
            ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       end if 

       end if ! emiss_inpt_opt /= 3

    CASE (CBMZ, CBMZ_BB, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       IF(config_flags%kemit .GT. kte-ksub) THEN
          message = ' EMISSIONS_DRIVER: KEMIT > KME '
          CALL WRF_ERROR_FATAL (message)
       ENDIF
       call wrf_debug(15,'emissions_driver calling cbmz_addemiss_anthro')
       call cbmz_addemiss_anthro( id, dtstep, dz8w, config_flags,        &
            rho_phy, chem,                                               &
            emis_ant,ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )
       call wrf_debug(15,'emissions_driver calling cbmz_addemiss_bio')
       ! Do NOT call add_biogenics if using MEGAN v2.04 biogenic emissions
       ! module
       if ( config_flags%bio_emiss_opt .ne. megan2 ) then 
         call cbmz_addemiss_bio( id, dtstep, dz8w, config_flags,         &
              rho_phy, chem, e_bio, ne_area, emis_ant(ims,kms,jms,p_e_iso),&
              ids,ide, jds,jde, kds,kde,                                 &
              ims,ime, jms,jme, kms,kme,                                 &
              its,ite, jts,jte, kts,kte                                  )
       endif

    CASE (CHEM_TRACER)
       do j=jts,jte  
          do i=its,ite  
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                   &
                     +emis_ant(i,k,j,p_e_so2)*conv
                chem(i,k,j,p_co)  = chem(i,k,j,p_co)                     &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_no)  = chem(i,k,j,p_no)                     &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                   &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_hcho)  = chem(i,k,j,p_hcho)                 &
                     +emis_ant(i,k,j,p_e_co)*conv
                chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                 &
                     +emis_ant(i,k,j,p_e_co)*conv
             end do
          end do
       end do

    CASE (GOCARTRACM_KPP)
       IF(config_flags%emiss_inpt_opt /= 3 ) then
       IF(config_flags%kemit .GT. kte-ksub) THEN
         k=config_flags%kemit
         write(message,'(" WARNING: EMISSIONS_DRIVER: KEMIT > KTE ",3i6)') kme,kte-ksub,k
         CALL WRF_MESSAGE (message)
       ENDIF
       call wrf_debug(15,'emissions_driver calling add_anthropogenics')
       call add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,chem,&
            emis_ant,ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       call wrf_debug(15,'emissions_driver calling add_biogenics')
       ! Do NOT call add_biogenics if using MEGAN v2.04 biogenic emissions
       ! module
       if ( config_flags%bio_emiss_opt .ne. megan2 ) then
       call add_biogenics(id,dtstep,dz8w,config_flags,rho_phy,chem,     &
            e_bio,ne_area,                                              &
            ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
            ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
            ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       end if 

       end if ! emiss_inpt_opt /= 3

!
! simple fix for now
!
       do j=jts,jte  
          do i=its,ite  
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_bc1)  = chem(i,k,j,p_bc1)                     &
                     +(emis_ant(i,k,j,p_e_eci)+emis_ant(i,k,j,p_e_ecj))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_oc1)  = chem(i,k,j,p_oc1)                     &
                     +(emis_ant(i,k,j,p_e_orgj)+emis_ant(i,k,j,p_e_orgi))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_p25)  = chem(i,k,j,p_p25)                     &
                     +(emis_ant(i,k,j,p_e_pm25j)+emis_ant(i,k,j,p_e_pm25i) &
                     + emis_ant(i,k,j,p_e_no3j)+emis_ant(i,k,j,p_e_no3i))  &
                     *alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_sulf)  = chem(i,k,j,p_sulf)                   &
                     +(emis_ant(i,k,j,p_e_so4i)+emis_ant(i,k,j,p_e_so4j))*alt(i,k,j)*dtstep/dz8w(i,k,j)*mwdry/mw_so4_aer*1.e-3
             end do
          end do
       end do

    CASE (GOCART_SIMPLE)
!
! simple fix for now
!
!
       if(config_flags%emiss_opt <=  5  ) then
       do j=jts,jte  
          do i=its,ite  
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                   &
                     +emis_ant(i,k,j,p_e_so2)*conv
                chem(i,k,j,p_bc1)  = chem(i,k,j,p_bc1)                     &
                     +(emis_ant(i,k,j,p_e_eci)+emis_ant(i,k,j,p_e_ecj))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_oc1)  = chem(i,k,j,p_oc1)                     &
                     +(emis_ant(i,k,j,p_e_orgj)+emis_ant(i,k,j,p_e_orgi))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_p25)  = chem(i,k,j,p_p25)                     &
                     +(emis_ant(i,k,j,p_e_pm25j)+emis_ant(i,k,j,p_e_pm25i) &
                     +emis_ant(i,k,j,p_e_no3j)+emis_ant(i,k,j,p_e_no3i))   &
                     *alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_sulf)  = chem(i,k,j,p_sulf)                   &
                     +(emis_ant(i,k,j,p_e_so4i)+emis_ant(i,k,j,p_e_so4j))  &
                     *alt(i,k,j)*dtstep/dz8w(i,k,j)*mwdry/mw_so4_aer*1.e-3
             end do
          end do
       end do
       endif
!
! next for global gocart emissions
!
       if(config_flags%emiss_opt == 6  ) then
       do j=jts,jte  
          do i=its,ite  
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                   &
                     +emis_ant(i,k,j,p_e_so2)*conv
                chem(i,k,j,p_bc1)  = chem(i,k,j,p_bc1)                     &
                     +(emis_ant(i,k,j,p_e_bc))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_oc1)  = chem(i,k,j,p_oc1)                     &
                     +(emis_ant(i,k,j,p_e_oc))*alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_p25)  = chem(i,k,j,p_p25)                     &
                     +(emis_ant(i,k,j,p_e_pm_25))   &
                     *alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_p10)  = chem(i,k,j,p_p10)                     &
                     +(emis_ant(i,k,j,p_e_pm_10))   &
                     *alt(i,k,j)*dtstep/dz8w(i,k,j)
                chem(i,k,j,p_sulf)  = chem(i,k,j,p_sulf)                   &
                     +emis_ant(i,k,j,p_e_sulf)*conv
             end do
          end do
       end do
       endif
! do this here for now, quick and dirty

    CASE DEFAULT
       call wrf_debug(15,'emissions_driver NOT CALLING gas add_... routines')

    END SELECT gas_addemiss_select
!
! special treatment for these emissions. They come in only at one time 
! (global emissions data set used here), and then a durnal variation is added on in this routine
!
    emiss_select: SELECT CASE(config_flags%emiss_inpt_opt)
    CASE (EMISS_INPT_CPTEC)
       call wrf_debug(15,'emissions_driver calling add_emiss_cptec')
       call add_emis_cptec(id,dtstep,ktau,dz8w,config_flags,rho_phy,chem,&
            julday,gmt,xlat,xlong,t_phy,p_phy,                           &
            emis_ant,                                                    &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )
    CASE DEFAULT
       call wrf_debug(15,'emissions_driver not calling add_emiss_cptec')
    END SELECT emiss_select

    aer_addemiss_select: SELECT CASE(config_flags%chem_opt)

    CASE (RADM2SORG,RADM2SORG_AQ,RADM2SORG_KPP, &
          RACMSORG,RACMSORG_AQ,RACMSORG_KPP)
       call wrf_debug(15,'emissions_driver calling sorgam_addemiss')
       call sorgam_addemiss( id, dtstep, u10, v10, alt, dz8w, xland, chem,  &
            slai,ust,smois,ivgtyp,isltyp,                               &
            emis_ant,dust_emiss_active,                                 &
            seasalt_emiss_active,config_flags%kemit,                    &
            config_flags%num_soil_layers,                               &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )

    CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       call wrf_debug(15,'emissions_driver calling mosaic_addemiss')
       call mosaic_addemiss( id, dtstep, u10, v10, alt, dz8w, xland,     &
            config_flags, chem, slai, ust, smois, ivgtyp, isltyp,        &
            emis_ant,                                                    &
            dust_emiss_active, seasalt_emiss_active,                     &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )

    CASE DEFAULT
       call wrf_debug(15,'emissions_driver NOT CALLING aer add_... routines')

    END SELECT aer_addemiss_select

    END subroutine emissions_driver

END module module_emissions_driver
