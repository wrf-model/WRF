!WRF:MODEL_LAYER:CHEMICS
!
    subroutine emissions_driver(id,ktau,dtstep,DX,                         &
         config_flags, stepbioe,gmt,julday,t_phy,moist,p8w,t8w,            &
         e_bio,p_phy,chem,rho_phy,dz8w,ne_area,                            &
         e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,  &
         e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,e_nh3,  &
         ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,       &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &   
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
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
  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday, ne_area,                      &
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
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         INTENT(IN ) ::                                                    &
          e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,       &
          e_olt,e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,        &
          e_pm10,e_nh3

!
! 
!


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      t_phy,               &
                                                      p_phy,               &
                                                      dz8w,                &
                                                      z    ,               &
                                              t8w,p8w,z_at_w ,             &
                                                    rho_phy
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,               &
          INTENT(IN   ) ::                                                 &
                                                     ivgtyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,               &
          INTENT(IN   ) ::                                                 &
                                                     tsk,                  &
                                                     gsw,                  &
                                                  vegfra,                  &
                                                     pbl,                  &
                                                     rmol,                 &
                                                     ust,                  &
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
!--- deposition and emissions stuff
! .. Parameters ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
     INTEGER I1,J1

! ..
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
   i1=10
   j1=10
   bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'biogenic emissions: calling Gunther1')
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
         call bio_emissions(id,ktau,dtstep,DX,config_flags,                &
               gmt,julday,t_phy,moist,p8w,t8w,                             &
               e_bio,p_phy,chem,rho_phy,dz8w,ne_area,               &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w, &
               p_ho2-1,                                 &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
       endif
     CASE (BEIS311)
       if(ktau.eq.1.or.mod(ktau,stepbioe).eq.0)then
      CALL wrf_debug(15,'biogenic emissions: calling beis3.1.1')
      call bio_emissions_beis311(id,config_flags,ktau,dtstep,          &
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
                                                     
   END SELECT bioem_select                              

!
! the following may need masking in future.....
!
         call wrf_debug(15,'calling add_anhropogenics WATCH HERE')
         call add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,chem, &
               e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,        &
               e_ol2,e_olt,                                                &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,  &
               e_nh3,ids,ide, jds,jde, kds,kde,                   &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
         call wrf_debug(15,'calling add_biogenics WATCH HERE')
         call add_biogenics(id,dtstep,dz8w,config_flags, rho_phy,chem,     &
               e_bio,ne_area,                                              &
               ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,               &
               ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,               &
               ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,       &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
    END subroutine emissions_driver
