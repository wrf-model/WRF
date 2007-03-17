!WRF:MODEL_LAYER:CHEMICS
!
    subroutine emissions_driver(id,ktau,dtstep,DX,                         &
         config_flags, stepbioe,gmt,julday,alt,t_phy,moist,p8w,t8w,        &
         e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                            &
         e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,  &
         e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,e_nh3,  &
         e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_no2,e_ch3oh,          &
         e_c2h5oh,e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                &
         u10,v10,ivgtyp,gsw,vegfra,rmol,ust,znt,                           &
         xland,xlat,xlong,z_at_w,                                          &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &   
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         numgas,                                                           &
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
  USE module_cbmz_addemiss
  USE module_mosaic_addemiss
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,julday, ne_area,                      &
                                  numgas,                                  &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                          &
                                  ktau,stepbioe
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme, ne_area ),                           &
         INTENT(INOUT ) ::                               e_bio
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme ),            &
         INTENT(IN ) ::                                                    &
          e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,       &
          e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,        &
          e_pm10,e_nh3,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_no2,    &
          e_ch3oh,e_c2h5oh,e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc
!
! 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                        alt,               &
                                                      t_phy,               &
                                                      p_phy,               &
                                                      dz8w,                &
                                              t8w,p8w,z_at_w ,             &
                                                    rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp
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
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(INOUT   ) ::                                                 &
               sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,      &
               sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,      &
               sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,      &
               noag_grow,noag_nongrow,nononag,slai,                    &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,           &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,           &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no

      REAL,      INTENT(IN   ) ::                                          &
                             dtstep,dx,gmt
!
! Local variables...
!
      INTEGER :: i, j, k, ksub
      REAL :: conv
      CHARACTER (LEN=80) :: message 

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
    ksub=1
#if ( NMM_CORE == 1 )
    ksub=0
#endif
    bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'biogenic emissions: calling Gunther1')
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
         call bio_emissions(id,ktau,dtstep,DX,config_flags,                &
               gmt,julday,t_phy,moist,p8w,t8w,                             &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                      &
               ivgtyp,gsw,vegfra,rmol,ust,znt,xlat,xlong,z_at_w,           &
               numgas-1,                                                   &
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

     CASE DEFAULT 
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0) &
            e_bio(its:ite,jts:jte,1:ne_area) = 0.
!wig: May need to zero out all ebio_xxx arrays too if they are incorporated
!     into CBMZ/MOSAIC.
                                                     
    END SELECT bioem_select

    gas_addemiss_select: SELECT CASE(config_flags%chem_opt)
    CASE (RADM2, RADM2_KPP, RADM2SORG, RACM, RACMSORG,RACM_KPP,RACMSORG_KPP, RACM_MIM_KPP,RADM2SORG_KPP)
       IF(config_flags%kemit .GT. kte-ksub) THEN
         message = ' EMISSIONS_DRIVER: KEMIT > KME '
         k=config_flags%kemit
         write(0,*)kme,kte-ksub,k
         CALL WRF_ERROR_FATAL (message)
       ENDIF
       call wrf_debug(15,'emissions_driver calling add_anthropogenics')
       call add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,chem,&
            e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,        &
            e_ol2,e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,    &
            e_pm25,e_pm10,e_nh3,                                        &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )
       call wrf_debug(15,'emissions_driver calling add_biogenics')
       call add_biogenics(id,dtstep,dz8w,config_flags, rho_phy,chem,    &
            e_bio,ne_area,                                              &
            ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
            ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
            ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
            ids,ide, jds,jde, kds,kde,                                  &
            ims,ime, jms,jme, kms,kme,                                  &
            its,ite, jts,jte, kts,kte                                   )

    CASE (CBMZ, CBMZ_BB, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       IF(config_flags%kemit .GT. kte-ksub) THEN
         message = ' EMISSIONS_DRIVER: KEMIT > KME '
         CALL WRF_ERROR_FATAL (message)
       ENDIF
       call wrf_debug(15,'emissions_driver calling cbmz_addemiss_anthro')
       call cbmz_addemiss_anthro( id, dtstep, dz8w, config_flags,        &
            rho_phy, chem,                                               &
            e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,   &
            e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,           &
            e_no2,e_ch3oh,e_c2h5oh,                                      &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )
       call wrf_debug(15,'emissions_driver calling cbmz_addemiss_bio')
       call cbmz_addemiss_bio( id, dtstep, dz8w, config_flags,           &
            rho_phy, chem, e_bio, ne_area, e_iso,                        &
            numgas,                                                      &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )

    CASE (CHEM_TRACER)
       do j=jts,jte  
          do i=its,ite  
             do k=kts,min(config_flags%kemit,kte-ksub)
                conv = 4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
                chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                   &
                     +e_so2(i,k,j)*conv
                chem(i,k,j,p_co)  = chem(i,k,j,p_co)                     &
                     +e_co(i,k,j)*conv
                chem(i,k,j,p_no)  = chem(i,k,j,p_no)                     &
                     +e_co(i,k,j)*conv
                chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                   &
                     +e_co(i,k,j)*conv
                chem(i,k,j,p_hcho)  = chem(i,k,j,p_hcho)                 &
                     +e_co(i,k,j)*conv
                chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                 &
                     +e_co(i,k,j)*conv
             end do
          end do
       end do

    CASE DEFAULT
       call wrf_debug(15,'emissions_driver NOT CALLING gas add_... routines')

    END SELECT gas_addemiss_select

    aer_addemiss_select: SELECT CASE(config_flags%chem_opt)

    CASE (CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       call wrf_debug(15,'emissions_driver calling mosaic_addemiss')
       call mosaic_addemiss( id, dtstep, u10, v10, alt, dz8w, xland,     &
            config_flags, chem,                                          &
            e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,     &
            e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                    &
            ids,ide, jds,jde, kds,kde,                                   &
            ims,ime, jms,jme, kms,kme,                                   &
            its,ite, jts,jte, kts,kte                                    )

    CASE DEFAULT
       call wrf_debug(15,'emissions_driver NOT CALLING aer add_... routines')

    END SELECT aer_addemiss_select

    END subroutine emissions_driver
