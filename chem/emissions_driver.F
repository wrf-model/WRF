!WRF:MODEL_LAYER:CHEMICS
!
MODULE module_emissions_driver
   IMPLICIT NONE
CONTAINS

    subroutine emissions_driver(id,ktau,dtstep,DX,stepfirepl,              &
         config_flags, stepbioe,gmt,julday,alt,t_phy,moist,p8w,t8w,u_phy,  &
         v_phy,vvel,e_bio,p_phy,chem,rho_phy,dz8w,ne_area,emis_ant,        &
         ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,            &
         ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,       &
         ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,         &
         ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,mean_fct_agtf,mean_fct_agef,   &
         mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,          &
         firesize_agsv,firesize_aggr,                                      &
         u10,v10,ivgtyp,isltyp,gsw,vegfra,rmol,ust,znt,                    &
         xland,xlat,xlong,z_at_w,z,smois,                                  &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &   
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         !shc stuff for MEGAN v2.04                                                 
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
         !shc end stuff for MEGAN v2.04
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_emissions_anthropogenics
  USE module_bioemi_simple
  USE module_bioemi_beis311
  USE module_bioemi_megan2
  USE module_aerosols_sorgam, only: sorgam_addemiss
  USE module_cbmz_addemiss
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
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme,num_emis_ant),&
         INTENT(IN ) ::                                                    &
         emis_ant
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         INTENT(INOUT ) ::                                                 &
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2

   REAL, DIMENSION( ims:ime,  jms:jme ),                                   &
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
          INTENT(INOUT   ) ::                                                 &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no

   !shc stuff for MEGAN v2.04

   integer, intent(in   ) :: nmegan
   real, dimension (ims:ime, jms:jme , nmegan) ,                       &
        intent(inout) ::                                               &
        EFmegan


   real, dimension (ims:ime, jms:jme ) ,                               &
        intent(in) ::                                                  &
        msebio_isop,                                                   &
        pftp_bt, pftp_nt, pftp_sb, pftp_hb

   real, dimension (ims:ime, jms:jme, 12 ) ,                           &
        intent(in) ::                                                  &
        mlai, mtsa, mswdown

   real, dimension (ims:ime, jms:jme ) ,                               &
        intent(inout) ::                                               &
        mebio_isop, mebio_apin, mebio_bpin, mebio_bcar,                &
        mebio_acet, mebio_mbo, mebio_no

   real, dimension (ims:ime, jms:jme ) ,                               &
        intent(in) ::                                                  &
        T2, swdown

   integer, intent(in) :: current_month

   !shc end stuff for MEGAN v2.04

      REAL,      INTENT(IN   ) ::                                      &
                            dtstep, dx, gmt
!
! Local variables...
!
      INTEGER :: i, j, k, ksub, dust_emiss_active, seasalt_emiss_active
      REAL :: conv
      CHARACTER (LEN=80) :: message 

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..

! Flags for turning on or off sea salt and/or dust emissions...
!    >0 = sea salt/dust emissions turned on
! As of Nov. 2007 this only works for MOSAIC, but is intended to be used
! by other mechanisms when they are linked to these emissions sources.
!
      dust_emiss_active    = 0
      seasalt_emiss_active = 0

!!$    ksub=1
!!$#if ( NMM_CORE == 1 )
    ksub=0
!!$#endif
    fire_select:  SELECT CASE(config_flags%biomass_burn_opt)
     CASE (BIOMASSB)
       if(ktau.eq.1.or.mod(ktau,stepfirepl).eq.0)then
          CALL wrf_debug(15,'fire emissions: calling biomassb')
          write(0,*)ktau,stepfirepl
         call plumerise_driver (id,ktau,dtstep,                            &
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,          &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,     &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,       &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,mean_fct_agtf,mean_fct_agef, &
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
       call add_emis_burn(id,dtstep,ktau,dz8w,config_flags,rho_phy,chem,&
            julday,gmt,xlat,xlong,t_phy,p_phy,                           &
            ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,       &
            ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,  &
            ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,    &
            ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,                          &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )
     CASE DEFAULT 
       CALL wrf_debug(15,'no biomass burning')
    END SELECT fire_select
    bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'biogenic emissions: calling Gunther1')
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
         call bio_emissions(id,ktau,dtstep,DX,config_flags,                &
               gmt,julday,t_phy,moist,p8w,t8w,                             &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                      &
               ivgtyp,gsw,vegfra,rmol,ust,znt,xlat,xlong,z_at_w,           &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
       endif
     CASE (BEIS311)
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
         if(config_flags%chem_opt > RACMSORG .AND. config_flags%chem_opt < 100  ) then !<100: kpp mechs, e.g. RACMSORG_KPP
             CALL wrf_error_fatal( &
              "emissions_driver: beis3.1.1 biogenic emis. not currently implemented for CBMZ")
         endif
         CALL wrf_debug(15,'biogenic emissions: calling beis3.1.1')
         call bio_emissions_beis311(id,config_flags,ktau,dtstep,       &
               julday,gmt,xlat,xlong,t_phy,p_phy,gsw,                  &
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

     CASE (MEGAN2) !shc
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then                        
         CALL wrf_debug(15,'biogenic emissions: calling megan v2.04')  
         call bio_emissions_megan2(id,config_flags,ktau,dtstep,        &
               julday,gmt,xlat,xlong,p_phy,rho_phy,dz8w,               &
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
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0) &
            e_bio(its:ite,jts:jte,1:ne_area) = 0.
!wig: May need to zero out all ebio_xxx arrays too if they are incorporated
!     into CBMZ/MOSAIC.
                                                     
    END SELECT bioem_select

    gas_addemiss_select: SELECT CASE(config_flags%chem_opt)
    CASE (RADM2, RADM2_KPP, RADM2SORG, RACM, RACMSORG,RACM_KPP,RACMPM_KPP,RACMSORG_KPP, RACM_MIM_KPP,RADM2SORG_KPP)
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
       if ( config_flags%bio_emiss_opt .ne. megan2 ) then !shc 
       call add_biogenics(id,dtstep,dz8w,config_flags, rho_phy,chem,    &
            e_bio,ne_area,                                              &
            ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
            ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
            ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       end if !shc

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
       if ( config_flags%bio_emiss_opt .ne. megan2 ) then !shc 
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
!           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,       &
!           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,  &
!           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,    &
!           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,                          &
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
       call sorgam_addemiss( dtstep, u10, v10, alt, dz8w, xland, chem,  &
            emis_ant,                                                   &
            seasalt_emiss_active,config_flags%kemit,                    &
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
