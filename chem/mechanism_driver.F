!WRF:MODEL_LAYER:CHEMICS
!
    subroutine mechanism_driver(id,curr_secs,ktau,dtstep,ktauc,dtstepc,&
               config_flags,gmt,julday,t_phy,moist,p8w,t8w,gd_cldfr,   &
               p_phy,chem,rho_phy,dz8w,dx,g,z,z_at_w,xlat,xlong,vdrog3,&
               vcsulf_old,vcso2_old,vch2o2_old,ttday,tcosz,            &
               ph_macr,ph_o31d,ph_o33p,ph_no2,                         &
               ph_cl2,ph_hocl,ph_clno2,ph_fmcl,                        &
               ph_no3o2,ph_no3o,ph_hno2,                               &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ph_n2o5,ph_o2,backg_oh,backg_h2o2,backg_no3,            &
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd,                                              &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam
  USE module_radm
  USE module_gocart_chem
  USE module_aerosols_sorgam
  USE module_cbmz, only:	cbmz_driver
   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday,                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: ktau,ktauc
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL,         INTENT(IN   ) :: dtstep,dtstepc,gmt,dx,g
!
! advected moisture variables
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),            &
         INTENT(IN ) ::                                   moist
!
! advected chemical species
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme ),                       &
         INTENT(IN ) ::                                             &
          xlat,xlong,ttday,tcosz

!
! arrays that hold the photolysis rates
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           ph_macr,ph_o31d,ph_o33p,ph_no2,                             &
           ph_cl2,ph_hocl,ph_clno2,ph_fmcl,                            &
           ph_no3o2,ph_no3o,ph_hno2,                                   &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
           ph_n2o5,ph_o2
!
! RACM radicals
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd


!
! on input from meteorological model 
!

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          INTENT(IN   ) ::                                             &
                                                      t_phy,           &
                                                      p_phy,           &
                                                    dz8w,              &
                        z    ,backg_oh,backg_h2o2,backg_no3,           &
                                              t8w,p8w,z_at_w ,         &
                                                    rho_phy
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          OPTIONAL,                                                    &
          INTENT(IN   ) ::  gd_cldfr
! ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
!
! for interaction of aerosls/chemistry (MADE/SORGAM only)
!
      real ,      INTENT(INOUT   ) ::                                  &
                      vdrog3(ims:ime,kms:kme-0,jms:jme,ldrog)
      real ,      INTENT(INOUT   ) ::                                  &
                      vcsulf_old(ims:ime,kms:kme,jms:jme),             &
                      vcso2_old(ims:ime,kms:kme,jms:jme),              &
                      vch2o2_old(ims:ime,kms:kme,jms:jme)



! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
!
! select chemical mechanism
!
   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (GOCART_SIMPLE,GOCARTRACM_KPP,GOCARTRADM2)
       CALL wrf_debug(15,'calling gocart chem from mechanism_driver')
       call gocart_chem_driver(curr_secs,dtstepc,config_flags,        &
         gmt,julday,t_phy,moist,                                      &
         chem,rho_phy,dz8w,p8w,backg_oh,backg_h2o2,backg_no3,         &
         gd_cldfr,dx,g,xlat,xlong,ttday,tcosz, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
       vcsulf_old(its:ite,kts:kte,jts:jte) = &
            max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
       vcso2_old(its:ite,kts:kte,jts:jte) = &
            max(chem(its:ite,kts:kte,jts:jte,p_so2),epsilc)
       vch2o2_old(its:ite,kts:kte,jts:jte) = &
            max(chem(its:ite,kts:kte,jts:jte,p_h2o2),epsilc)


     CASE (RADM2, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM)
       CALL wrf_debug(15,'calling radm2_driver from mechanism_driver')
       if (config_flags%chem_opt /= RADM2) then
          vcsulf_old(its:ite,kts:kte,jts:jte) = &
             max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
       end if
       call radm_driver(id,curr_secs,dtstepc,config_flags,             &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,                &
               ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,        &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
     CASE(RACMSORG_KPP,RADM2SORG_KPP,RACMSORG_AQ,RACMSORG_AQCHEM_KPP,  &
          RACM_ESRLSORG_AQCHEM_KPP,RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP, &
          RACM_SOA_VBS_HET_KPP)
         vcsulf_old(its:ite,kts:kte,jts:jte) = &
          max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)

     CASE (CBMZ, CBMZ_BB, CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN,      &
                 CBMZ_MOSAIC_4BIN_AQ,     CBMZ_MOSAIC_8BIN_AQ,     &
                 CBMZ_MOSAIC_DMS_4BIN,    CBMZ_MOSAIC_DMS_8BIN,    &
                 CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
                 CBMZSORG, CBMZSORG_AQ, CBMZ_CAM_MAM3_NOAQ,        &
                 CBMZ_CAM_MAM3_AQ,CBMZ_CAM_MAM7_NOAQ,              &
                 CBMZ_CAM_MAM7_AQ)

       CALL wrf_debug(15,'calling cbmz_driver from mechanism_driver')
       vcsulf_old(its:ite,kts:kte,jts:jte) = &
          max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)
       call cbmz_driver(id,curr_secs,ktau,dtstep,ktauc,dtstepc,        &
               config_flags,                                           &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,                &
               ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,        &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,              &
               ph_ch3o2h,ph_n2o5,                                      &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )

     CASE (CB05_SORG_AQ_KPP)
       chem(its:ite,kts:kte,jts:jte,p_apin) = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.248
       chem(its:ite,kts:kte,jts:jte,p_bpin) = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.294
       chem(its:ite,kts:kte,jts:jte,p_lim)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.164
       chem(its:ite,kts:kte,jts:jte,p_ter)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.006
       chem(its:ite,kts:kte,jts:jte,p_oci)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.213
       chem(its:ite,kts:kte,jts:jte,p_hum)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.074
       chem(its:ite,kts:kte,jts:jte,p_ch4) = 1.7
       chem(its:ite,kts:kte,jts:jte,p_h2) = 0.5

       vcsulf_old(its:ite,kts:kte,jts:jte) = &
          max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)

     CASE (CB05_SORG_VBS_AQ_KPP)
       chem(its:ite,kts:kte,jts:jte,p_apin) = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.248
       chem(its:ite,kts:kte,jts:jte,p_bpin) = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.294
       chem(its:ite,kts:kte,jts:jte,p_lim)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.164
       chem(its:ite,kts:kte,jts:jte,p_ter)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.006
       chem(its:ite,kts:kte,jts:jte,p_oci)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.213
       chem(its:ite,kts:kte,jts:jte,p_hum)  = chem(its:ite,kts:kte,jts:jte,p_terp) * 0.074
       chem(its:ite,kts:kte,jts:jte,p_ch4) = 1.7
       chem(its:ite,kts:kte,jts:jte,p_h2) = 0.5

       vcsulf_old(its:ite,kts:kte,jts:jte) = &
          max(chem(its:ite,kts:kte,jts:jte,p_sulf),epsilc)

     CASE DEFAULT
                                                     
   END SELECT chem_select                              


    END subroutine mechanism_driver
