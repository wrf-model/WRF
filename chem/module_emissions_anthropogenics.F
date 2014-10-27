MODULE module_emissions_anthropogenics
!WRF:MODEL_LAYER:CHEMICS
!
CONTAINS
!
! currently this only adds in the emissions...
! this may be done differently for different chemical mechanisms
! in the future. aerosols are already added somewhere else....
!
   subroutine add_anthropogenics(id,dtstep,dz8w,config_flags,rho_phy,alt,  &
               chem, emis_ant, emis_aircraft,                              &
               ids,ide, jds,jde, kds,kde,                                  &
               ims,ime, jms,jme, kms,kme,                                  &
               its,ite, jts,jte, kts,kte                                   )
!----------------------------------------------------------------------
  USE module_configure
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam, only : mw_so4_aer
  USE module_model_constants, only : mwdry
   IMPLICIT NONE

! .. Parameters ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,                                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
      REAL,      INTENT(IN   ) ::                                          &
                             dtstep
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
!
! emissions arrays
!
!   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme,num_emis_ant ),            &
         INTENT(IN ) ::                                                    &
               emis_ant        
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                           &
         INTENT(IN ) ::        rho_phy,                                    &
                               alt

!
! 
!


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      dz8w

! stuff for aircraft emissions

   REAL, DIMENSION( ims:ime, kms:config_flags%kemit_aircraft, jms:jme, num_emis_aircraft),      &
         OPTIONAL,INTENT(IN ) ::  emis_aircraft

    integer i,j,k
    real :: conv_rho, conv_rho_aer
!--- deposition and emissions stuff


! ..
! ..
! .. Intrinsic Functions ..

         call wrf_debug(15,'add_anthropogenics')
!       
! add emissions
!
      do 100 j=jts,jte  
      do 100 i=its,ite  

      DO k=kts,min(config_flags%kemit,kte)
        conv_rho=4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (i <= CHEM_DBG_I .and. i >= CHEM_DBG_I) .and. &
           (j <= CHEM_DBG_J .and. j >= CHEM_DBG_J) .and. &
           (k <= CHEM_DBG_K .and. k >= CHEM_DBG_K)  ) then
          print*
          print*,"Converted emissions for RADM2:"
          print*,"e_csl=",emis_ant(i,k,j,p_e_csl)*conv_rho
          print*,"e_iso=",emis_ant(i,k,j,p_e_iso)*conv_rho
          print*,"e_so2=",emis_ant(i,k,j,p_e_so2)*conv_rho
          print*,"e_no=",emis_ant(i,k,j,p_e_no)*conv_rho
          print*,"e_ald=",emis_ant(i,k,j,p_e_ald)*conv_rho
          print*,"e_hcho=",emis_ant(i,k,j,p_e_hcho)*conv_rho
          print*,"e_ora2=",emis_ant(i,k,j,p_e_ora2)*conv_rho
          print*,"e_nh3=",emis_ant(i,k,j,p_e_nh3)*conv_rho
          print*,"e_hc3=",emis_ant(i,k,j,p_e_hc3)*conv_rho
          print*,"e_hc5=",emis_ant(i,k,j,p_e_hc5)*conv_rho
          print*,"e_hc8=",emis_ant(i,k,j,p_e_hc8)*conv_rho
          print*,"e_eth=",emis_ant(i,k,j,p_e_eth)*conv_rho
          print*,"e_co=",emis_ant(i,k,j,p_e_co)*conv_rho
          print*,"e_ol2=",emis_ant(i,k,j,p_e_ol2)*conv_rho
          print*,"e_olt=",emis_ant(i,k,j,p_e_olt)*conv_rho
          print*,"e_oli=",emis_ant(i,k,j,p_e_oli)*conv_rho
          print*,"e_tol=",emis_ant(i,k,j,p_e_tol)*conv_rho
          print*,"e_xyl=",emis_ant(i,k,j,p_e_xyl)*conv_rho
          print*,"e_ket=",emis_ant(i,k,j,p_e_ket)*conv_rho
       end if
#endif

        chem(i,k,j,p_so2)  = chem(i,k,j,p_so2)                         &
                         +emis_ant(i,k,j,p_e_so2)*conv_rho
        chem(i,k,j,p_co)  = chem(i,k,j,p_co)                           &
                         +emis_ant(i,k,j,p_e_co)*conv_rho
        chem(i,k,j,p_no)   = chem(i,k,j,p_no)                          &
                         +emis_ant(i,k,j,p_e_no)*conv_rho
        chem(i,k,j,p_no2)   = chem(i,k,j,p_no2)                          &
                         +emis_ant(i,k,j,p_e_no2)*conv_rho
        chem(i,k,j,p_nh3)  = chem(i,k,j,p_nh3)                         &
                         +emis_ant(i,k,j,p_e_nh3)*conv_rho
        if( config_flags%chem_opt == MOZART_KPP .or. &
            config_flags%chem_opt == MOZCART_KPP .or. &
            config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
            config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
           conv_rho_aer = alt(i,k,j)*dtstep/dz8w(i,k,j)
           chem(i,k,j,p_bigalk) = chem(i,k,j,p_bigalk) + emis_ant(i,k,j,p_e_bigalk)*conv_rho
           chem(i,k,j,p_bigene) = chem(i,k,j,p_bigene) + emis_ant(i,k,j,p_e_bigene)*conv_rho
           chem(i,k,j,p_c2h4)   = chem(i,k,j,p_c2h4)  + emis_ant(i,k,j,p_e_c2h4)*conv_rho
           chem(i,k,j,p_c2h5oh) = chem(i,k,j,p_c2h5oh) + emis_ant(i,k,j,p_e_c2h5oh)*conv_rho
           chem(i,k,j,p_c2h6)   = chem(i,k,j,p_c2h6) + emis_ant(i,k,j,p_e_c2h6)*conv_rho
           chem(i,k,j,p_c3h6)   = chem(i,k,j,p_c3h6) + emis_ant(i,k,j,p_e_c3h6)*conv_rho
           chem(i,k,j,p_c3h8)   = chem(i,k,j,p_c3h8) + emis_ant(i,k,j,p_e_c3h8)*conv_rho
           chem(i,k,j,p_hcho)   = chem(i,k,j,p_hcho) + emis_ant(i,k,j,p_e_ch2o)*conv_rho
           chem(i,k,j,p_ald)    = chem(i,k,j,p_ald) + emis_ant(i,k,j,p_e_ch3cho)*conv_rho
           chem(i,k,j,p_acet)   = chem(i,k,j,p_acet) + emis_ant(i,k,j,p_e_ch3coch3)*conv_rho
           chem(i,k,j,p_ch3oh)  = chem(i,k,j,p_ch3oh) + emis_ant(i,k,j,p_e_ch3oh)*conv_rho
           chem(i,k,j,p_gly)    = chem(i,k,j,p_gly) + emis_ant(i,k,j,p_e_gly)*conv_rho
           chem(i,k,j,p_macr)    = chem(i,k,j,p_macr) + emis_ant(i,k,j,p_e_macr)*conv_rho
           chem(i,k,j,p_mgly)    = chem(i,k,j,p_mgly) + emis_ant(i,k,j,p_e_mgly)*conv_rho
           chem(i,k,j,p_mvk)    = chem(i,k,j,p_mvk) + emis_ant(i,k,j,p_e_mvk)*conv_rho
           chem(i,k,j,p_c2h2)   = chem(i,k,j,p_c2h2)  + emis_ant(i,k,j,p_e_c2h2)*conv_rho
           chem(i,k,j,p_hcooh)    = chem(i,k,j,p_hcooh) + emis_ant(i,k,j,p_e_hcooh)*conv_rho
           chem(i,k,j,p_mek)    = chem(i,k,j,p_mek) + emis_ant(i,k,j,p_e_mek)*conv_rho
           chem(i,k,j,p_tol)    = chem(i,k,j,p_tol) + emis_ant(i,k,j,p_e_toluene)*conv_rho
           chem(i,k,j,p_benzene) = chem(i,k,j,p_benzene) + emis_ant(i,k,j,p_e_benzene)*conv_rho
           chem(i,k,j,p_xyl) = chem(i,k,j,p_xyl) + emis_ant(i,k,j,p_e_xylene)*conv_rho
           chem(i,k,j,p_cres) = chem(i,k,j,p_cres) + emis_ant(i,k,j,p_e_cres)*conv_rho
           chem(i,k,j,p_bald) = chem(i,k,j,p_bald) + emis_ant(i,k,j,p_e_bald)*conv_rho
           chem(i,k,j,p_hono)  = chem(i,k,j,p_hono) +emis_ant(i,k,j,p_e_hono)*conv_rho
           chem(i,k,j,p_isopr)  = chem(i,k,j,p_isopr) + emis_ant(i,k,j,p_e_isop)*conv_rho
           chem(i,k,j,p_c10h16) = chem(i,k,j,p_c10h16) + emis_ant(i,k,j,p_e_c10h16)*conv_rho
           chem(i,k,j,p_sulf)   = chem(i,k,j,p_sulf) + conv_rho_aer*emis_ant(i,k,j,p_e_sulf)*mwdry/mw_so4_aer*1.e-3
           if( config_flags%chem_opt == MOZCART_KPP ) then
             chem(i,k,j,p_p10) = chem(i,k,j,p_p10) + conv_rho_aer*emis_ant(i,k,j,p_e_pm_10)
             chem(i,k,j,p_p25) = chem(i,k,j,p_p25) + conv_rho_aer*emis_ant(i,k,j,p_e_pm_25)
             chem(i,k,j,p_oc1) = chem(i,k,j,p_oc1) + conv_rho_aer*emis_ant(i,k,j,p_e_oc)
             chem(i,k,j,p_bc1) = chem(i,k,j,p_bc1) + conv_rho_aer*emis_ant(i,k,j,p_e_bc)
           elseif( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP ) then
! emissions should be CO_A and CO_BB with yields instead of VOC_A and VOC_BB
!             chem(i,k,j,p_voca)  = chem(i,k,j,p_voca) + conv_rho*emis_ant(i,k,j,p_e_voca)
!             chem(i,k,j,p_vocbb) = chem(i,k,j,p_vocbb) + conv_rho*emis_ant(i,k,j,p_e_vocbb)
!             chem(i,k,j,p_voca)  = chem(i,k,j,p_voca) + conv_rho_aer*emis_ant(i,k,j,p_e_co_a)*0.04*28./250.
!             chem(i,k,j,p_vocbb) = chem(i,k,j,p_vocbb) + conv_rho_aer*emis_ant(i,k,j,p_e_co_bb)*0.04*28./250.
             chem(i,k,j,p_voca)  = chem(i,k,j,p_voca) + conv_rho*emis_ant(i,k,j,p_e_co_a)*0.04*28./250.
             chem(i,k,j,p_vocbb) = chem(i,k,j,p_vocbb) + conv_rho*emis_ant(i,k,j,p_e_co_bb)*0.04*28./250.
           endif
        else if( config_flags%chem_opt == CRIMECH_KPP &
          .or. config_flags%chem_opt == CRI_MOSAIC_8BIN_AQ_KPP &
          .or. config_flags%chem_opt == CRI_MOSAIC_4BIN_AQ_KPP) then
           chem(i,k,j,p_no2) = chem(i,k,j,p_no2) + emis_ant(i,k,j,p_e_no2)*conv_rho
           chem(i,k,j,p_c2h6) = chem(i,k,j,p_c2h6) + emis_ant(i,k,j,p_e_c2h6)*conv_rho
           chem(i,k,j,p_c2h4) = chem(i,k,j,p_c2h4) + emis_ant(i,k,j,p_e_c2h4)*conv_rho
           chem(i,k,j,p_c5h8) = chem(i,k,j,p_c5h8) + emis_ant(i,k,j,p_e_c5h8)*conv_rho
           chem(i,k,j,p_tm123b) = chem(i,k,j,p_tm123b) + emis_ant(i,k,j,p_e_tm123b)*conv_rho
           chem(i,k,j,p_tm124b) = chem(i,k,j,p_tm124b) + emis_ant(i,k,j,p_e_tm124b)*conv_rho
           chem(i,k,j,p_tm135b) = chem(i,k,j,p_tm135b) + emis_ant(i,k,j,p_e_tm135b)*conv_rho
           chem(i,k,j,p_oethtol) = chem(i,k,j,p_oethtol) + emis_ant(i,k,j,p_e_oethtol)*conv_rho
           chem(i,k,j,p_methtol) = chem(i,k,j,p_methtol) + emis_ant(i,k,j,p_e_methtol)*conv_rho
           chem(i,k,j,p_pethtol) = chem(i,k,j,p_pethtol) + emis_ant(i,k,j,p_e_pethtol)*conv_rho
           chem(i,k,j,p_dime35eb) = chem(i,k,j,p_dime35eb) + emis_ant(i,k,j,p_e_dime35eb)*conv_rho
           chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho) + emis_ant(i,k,j,p_e_hcho)*conv_rho
           chem(i,k,j,p_ch3cho) = chem(i,k,j,p_ch3cho) + emis_ant(i,k,j,p_e_ch3cho)*conv_rho
           chem(i,k,j,p_c2h5cho) = chem(i,k,j,p_c2h5cho) + emis_ant(i,k,j,p_e_c2h5cho)*conv_rho
           chem(i,k,j,p_ket) = chem(i,k,j,p_ket) + emis_ant(i,k,j,p_e_ket)*conv_rho
           chem(i,k,j,p_mek) = chem(i,k,j,p_mek) + emis_ant(i,k,j,p_e_mek)*conv_rho
           chem(i,k,j,p_ch3oh) = chem(i,k,j,p_ch3oh) + emis_ant(i,k,j,p_e_ch3oh)*conv_rho
           chem(i,k,j,p_c2h5oh) = chem(i,k,j,p_c2h5oh) + emis_ant(i,k,j,p_e_c2h5oh)*conv_rho
           chem(i,k,j,p_c3h6) = chem(i,k,j,p_c3h6) + emis_ant(i,k,j,p_e_c3h6)*conv_rho
           chem(i,k,j,p_c2h2) = chem(i,k,j,p_c2h2) + emis_ant(i,k,j,p_e_c2h2)*conv_rho
           chem(i,k,j,p_benzene) = chem(i,k,j,p_benzene) + emis_ant(i,k,j,p_e_benzene)*conv_rho
           chem(i,k,j,p_nc4h10) = chem(i,k,j,p_nc4h10) + emis_ant(i,k,j,p_e_nc4h10)*conv_rho
           chem(i,k,j,p_toluene) = chem(i,k,j,p_toluene) + emis_ant(i,k,j,p_e_toluene)*conv_rho
           chem(i,k,j,p_oxyl) = chem(i,k,j,p_oxyl) + emis_ant(i,k,j,p_e_oxyl)*conv_rho
           chem(i,k,j,p_c3h8) = chem(i,k,j,p_c3h8) + emis_ant(i,k,j,p_e_c3h8)*conv_rho
           chem(i,k,j,p_tbut2ene) = chem(i,k,j,p_tbut2ene) + emis_ant(i,k,j,p_e_tbut2ene)*conv_rho
           chem(i,k,j,p_ch3co2h) = chem(i,k,j,p_ch3co2h) + emis_ant(i,k,j,p_e_ch3co2h)*conv_rho
        else
        chem(i,k,j,p_csl)  =  chem(i,k,j,p_csl)                        &
                         +emis_ant(i,k,j,p_e_csl)*conv_rho
        chem(i,k,j,p_iso)  = chem(i,k,j,p_iso)                         &
                         +emis_ant(i,k,j,p_e_iso)*conv_rho
        chem(i,k,j,p_ald)  = chem(i,k,j,p_ald)                         &
                         +emis_ant(i,k,j,p_e_ald)*conv_rho
        chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho)                        &
                         +emis_ant(i,k,j,p_e_hcho)*conv_rho
        chem(i,k,j,p_ora2)  = chem(i,k,j,p_ora2)                       &
                         +emis_ant(i,k,j,p_e_ora2)*conv_rho
        chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3)                         &
                         +emis_ant(i,k,j,p_e_hc3)*conv_rho
        chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5)                         &
                         +emis_ant(i,k,j,p_e_hc5)*conv_rho
        chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8)                         &
                         +emis_ant(i,k,j,p_e_hc8)*conv_rho
        chem(i,k,j,p_eth)  = chem(i,k,j,p_eth)                         &
                         +emis_ant(i,k,j,p_e_eth)*conv_rho
        if(p_ol2.gt.1)chem(i,k,j,p_ol2)  = chem(i,k,j,p_ol2)           &
                         +emis_ant(i,k,j,p_e_ol2)*conv_rho
        if(p_ete.gt.1)chem(i,k,j,p_ete)  = chem(i,k,j,p_ete)           &
                         +emis_ant(i,k,j,p_e_ol2)*conv_rho
        chem(i,k,j,p_olt)  = chem(i,k,j,p_olt)                         &
                         +emis_ant(i,k,j,p_e_olt)*conv_rho
        chem(i,k,j,p_oli)  = chem(i,k,j,p_oli)                         &
                         +emis_ant(i,k,j,p_e_oli)*conv_rho
        chem(i,k,j,p_tol)  = chem(i,k,j,p_tol)                         &
                         +emis_ant(i,k,j,p_e_tol)*conv_rho
        chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl)                         &      
                         +emis_ant(i,k,j,p_e_xyl)*conv_rho
        chem(i,k,j,p_ket)  =  chem(i,k,j,p_ket)                        &     
                         +emis_ant(i,k,j,p_e_ket)*conv_rho
        if( config_flags%chem_opt == GOCARTRACM_KPP       &
          .or. config_flags%chem_opt == GOCARTRADM2       &
          .or. config_flags%chem_opt == GOCARTRADM2_KPP) then  
          conv_rho_aer = alt(i,k,j)*dtstep/dz8w(i,k,j)
          chem(i,k,j,p_p10) = chem(i,k,j,p_p10) + conv_rho_aer*emis_ant(i,k,j,p_e_pm_10)
          chem(i,k,j,p_p25) = chem(i,k,j,p_p25) + conv_rho_aer*emis_ant(i,k,j,p_e_pm_25)
          chem(i,k,j,p_oc1) = chem(i,k,j,p_oc1) + conv_rho_aer*emis_ant(i,k,j,p_e_oc)
          chem(i,k,j,p_bc1) = chem(i,k,j,p_bc1) + conv_rho_aer*emis_ant(i,k,j,p_e_bc)
        endif
       endif
      END DO                                                          
 100  continue

! add aircraft emissions
   if (config_flags%aircraft_emiss_opt == 1 ) then
      do j=jts,jte  
      do k=kts,min(config_flags%kemit_aircraft,kte)
      do i=its,ite  
         conv_rho=4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
         chem(i,k,j,p_no)  = chem(i,k,j,p_no)  + emis_aircraft(i,k,j,p_eac_no) *conv_rho
         chem(i,k,j,p_co)  = chem(i,k,j,p_co)  + emis_aircraft(i,k,j,p_eac_co) *conv_rho
         chem(i,k,j,p_so2) = chem(i,k,j,p_so2) + emis_aircraft(i,k,j,p_eac_so2)*conv_rho
         chem(i,k,j,p_ch4) = chem(i,k,j,p_ch4) + emis_aircraft(i,k,j,p_eac_ch4)*conv_rho
      enddo
      enddo
      enddo
   endif 

    END subroutine add_anthropogenics
!
!
    subroutine add_biogenics(id,dtstep,dz8w,config_flags,rho_phy,chem, &
         e_bio,ne_area,                                                &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                 &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                 &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,         &
         ebio_sesq,ebio_mbo,                                           & 
         ids,ide, jds,jde, kds,kde,                                    &
         ims,ime, jms,jme, kms,kme,                                    &
         its,ite, jts,jte, kts,kte                                     )
  USE module_configure
  USE module_state_description                                  
  USE module_data_radm2                               
  USE module_aerosols_sorgam 
   IMPLICIT NONE             
   INTEGER,      INTENT(IN   ) :: id,ne_area,                              &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   REAL,         INTENT(IN   ) ::                                          &
                             dtstep
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, jms:jme,ne_area ),                            &
         INTENT(IN ) ::                                                    &
                 e_bio
   REAL, DIMENSION( ims:ime, jms:jme ),                                    &
         INTENT(IN ) ::                                                    &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         ebio_sesq,ebio_mbo 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                          rho_phy,dz8w
    integer i,j,k,n
    real :: conv_rho
    real :: con2ppm(its:ite,jts:jte)
!--- deposition and emissions stuff
! .. Parameters ..       
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags     
!  return
   bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'adding biogenic emissions: Gunther1')
!     DO n = 1, numgas-2
      do 100 j=jts,jte  
      do 100 i=its,ite  
        conv_rho=dtstep/(dz8w(i,kts,j)*60.)
        chem(i,kts,j,p_iso)=chem(i,kts,j,p_iso)+ &
                  e_bio(i,j,p_iso-1)*conv_rho
        chem(i,kts,j,p_oli)=chem(i,kts,j,p_oli)+ &
                  e_bio(i,j,p_oli-1)*conv_rho
        chem(i,kts,j,p_xyl)=chem(i,kts,j,p_xyl)+ &
                  e_bio(i,j,p_xyl-1)*conv_rho
        chem(i,kts,j,p_olt)=chem(i,kts,j,p_olt)+ &
                  e_bio(i,j,p_olt-1)*conv_rho
        chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
                  e_bio(i,j,p_ket-1)*conv_rho
        chem(i,kts,j,p_ald)=chem(i,kts,j,p_ald)+ &
                  e_bio(i,j,p_ald-1)*conv_rho
        chem(i,kts,j,p_hcho)=chem(i,kts,j,p_hcho)+ &
                  e_bio(i,j,p_hcho-1)*conv_rho
        chem(i,kts,j,p_eth)=chem(i,kts,j,p_eth)+ &
                  e_bio(i,j,p_eth-1)*conv_rho
        chem(i,kts,j,p_ora2)=chem(i,kts,j,p_ora2)+ &
                  e_bio(i,j,p_ora2-1)*conv_rho
        chem(i,kts,j,p_co)=chem(i,kts,j,p_co)+ &
                  e_bio(i,j,p_co-1)*conv_rho
        chem(i,kts,j,p_no)=chem(i,kts,j,p_no)+ &
                  e_bio(i,j,p_no-1)*conv_rho
!
! RADM only
!
        if(p_hc3.gt.1) chem(i,kts,j,p_hc3)=chem(i,kts,j,p_hc3)+ &
                  e_bio(i,j,p_hc3-1)*conv_rho
        if(p_ol2.gt.1)chem(i,kts,j,p_ol2)=chem(i,kts,j,p_ol2)+ &
                  e_bio(i,j,p_ol2-1)*conv_rho
! CRIMECH only
        if(p_c5h8.gt.1)chem(i,kts,j,p_c5h8)=chem(i,kts,j,p_c5h8)+ &
                  e_bio(i,j,liso)*conv_rho
        if(p_oxyl.gt.1)chem(i,kts,j,p_oxyl)=chem(i,kts,j,p_oxyl)+ &
                  e_bio(i,j,lxyl)*conv_rho
        if(p_c3h8.gt.1)chem(i,kts,j,p_c3h8)=chem(i,kts,j,p_c3h8)+ &
                  e_bio(i,j,lhc3)*conv_rho
        if(p_ket.gt.1)chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
                  e_bio(i,j,lket)*conv_rho
        if(p_ch3cho.gt.1)chem(i,kts,j,p_ch3cho)=chem(i,kts,j,p_ch3cho)+ &
                  e_bio(i,j,lald)*conv_rho
        if(p_apinene.gt.1)chem(i,kts,j,p_apinene)=chem(i,kts,j,p_apinene)+ &
                  0.667*e_bio(i,j,loli)*conv_rho
!       if(p_bpinene.gt.1)chem(i,kts,j,p_bpinene)=chem(i,kts,j,p_bpinene)+ &
!                 0.333*e_bio(i,j,loli)*conv_rho
        if(p_bpinene.gt.1)chem(i,kts,j,p_bpinene)=chem(i,kts,j,p_bpinene)+ &
                  0.333*e_bio(i,j,loli)*conv_rho + e_bio(i,j,lolt)*conv_rho
        if(p_hcooh.gt.1)chem(i,kts,j,p_hcooh)=chem(i,kts,j,p_hcooh)+ &
                  e_bio(i,j,lora1)*conv_rho
        if(p_ch3co2h.gt.1)chem(i,kts,j,p_ch3co2h)=chem(i,kts,j,p_ch3co2h)+ &
                  e_bio(i,j,lora2)*conv_rho

!BSINGH -  Added for CBMZ
!
!CBMZ Only
!
        if(p_par .gt. 1) chem(i,kts,j,p_par) = chem(i,kts,j,p_par)  &
                + (e_bio(i,j,p_ald-1)*0.4        &
                + e_bio(i,j,p_ket-1)*0.9         &     
                + e_bio(i,j,p_olt-1)*1.8         &
                + e_bio(i,j,p_ora2-1)*1.0 )*conv_rho
        if(p_hc3 .gt. 1) chem(i,kts,j,p_par) = chem(i,kts,j,p_par)  &
                + e_bio(i,j,p_hc3-1)*4.8*conv_rho
 100  continue
!     enddo
     CASE (BEIS314)
     CALL wrf_debug(100,'adding biogenic emissions: beis3.1.4')
      do j=jts,jte  
      do i=its,ite  
        conv_rho=4.828e-4/rho_phy(i,kts,j)*dtstep/(dz8w(i,kts,j)*60.)
        chem(i,kts,j,p_iso)=chem(i,kts,j,p_iso)+ &
                  ebio_iso(i,j)*conv_rho
        chem(i,kts,j,p_oli)=chem(i,kts,j,p_oli)+ &
                  ebio_oli(i,j)*conv_rho
        chem(i,kts,j,p_xyl)=chem(i,kts,j,p_xyl)+ &
                  ebio_xyl(i,j)*conv_rho
        chem(i,kts,j,p_hc3)=chem(i,kts,j,p_hc3)+ &
                  ebio_hc3(i,j)*conv_rho
        chem(i,kts,j,p_olt)=chem(i,kts,j,p_olt)+ &
                  ebio_olt(i,j)*conv_rho
        chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
                  ebio_ket(i,j)*conv_rho
        chem(i,kts,j,p_ald)=chem(i,kts,j,p_ald)+ &
                  ebio_ald(i,j)*conv_rho
        chem(i,kts,j,p_hcho)=chem(i,kts,j,p_hcho)+ &
                  ebio_hcho(i,j)*conv_rho
        chem(i,kts,j,p_eth)=chem(i,kts,j,p_eth)+ &
                  ebio_eth(i,j)*conv_rho
        chem(i,kts,j,p_ora2)=chem(i,kts,j,p_ora2)+ &
                  ebio_ora2(i,j)*conv_rho
        chem(i,kts,j,p_co)=chem(i,kts,j,p_co)+ &
                  ebio_co(i,j)*conv_rho
        chem(i,kts,j,p_no)=chem(i,kts,j,p_no)+ &
                  ebio_no(i,j)*conv_rho
!
! RADM only
!
        if(p_ol2.gt.1)chem(i,kts,j,p_ol2)=chem(i,kts,j,p_ol2)+ &
                  ebio_ete(i,j)*conv_rho
!
! CRIMECH only --- DL 05/08/2013: not used at the moment
!
!        if(p_c5h8.gt.1)chem(i,kts,j,p_c5h8)=chem(i,kts,j,p_c5h8)+ &
!                  e_bio(i,j,p_iso-1)*conv_rho
!        if(p_oxyl.gt.1)chem(i,kts,j,p_oxyl)=chem(i,kts,j,p_oxyl)+ &
!                  e_bio(i,j,p_xyl-1)*conv_rho
!        if(p_c3h8.gt.1)chem(i,kts,j,p_c3h8)=chem(i,kts,j,p_c3h8)+ &
!                  e_bio(i,j,p_hc3-1)*conv_rho
!        if(p_ket.gt.1)chem(i,kts,j,p_ket)=chem(i,kts,j,p_ket)+ &
!                  e_bio(i,j,p_ket-1)*conv_rho
!        if(p_ch3cho.gt.1)chem(i,kts,j,p_ch3cho)=chem(i,kts,j,p_ch3cho)+ &
!                  e_bio(i,j,p_ald-1)*conv_rho
!        if(p_c2h6.gt.1)chem(i,kts,j,p_c2h6)=chem(i,kts,j,p_c2h6)+ &
!                  e_bio(i,j,p_eth-1)*conv_rho
!        if(p_apinene.gt.1)chem(i,kts,j,p_apinene)=chem(i,kts,j,p_apinene)+ &
!                  e_bio(i,j,p_api-1)*conv_rho
!        if(p_bpinene.gt.1)chem(i,kts,j,p_bpinene)=chem(i,kts,j,p_bpinene)+ &
!                  e_bio(i,j,p_api-1)*conv_rho

!
! RACM only
!
        if(p_api.gt.1)chem(i,kts,j,p_api)=chem(i,kts,j,p_api)+ &
                  ebio_api(i,j)*conv_rho
        if(p_lim.gt.1)chem(i,kts,j,p_lim)=chem(i,kts,j,p_lim)+ &
                  ebio_lim(i,j)*conv_rho
        if(p_ete.gt.1)chem(i,kts,j,p_ete)=chem(i,kts,j,p_ete)+ &
                  ebio_ete(i,j)*conv_rho

! SESQ and MBO are added, it works for RACM_SOA_VBS_KPP option
        if(p_sesq.gt.1)chem(i,kts,j,p_sesq)=chem(i,kts,j,p_sesq)+ &
                  ebio_sesq(i,j)*conv_rho
        if(p_mbo.gt.1)chem(i,kts,j,p_mbo)=chem(i,kts,j,p_mbo)+ &
                  ebio_mbo(i,j)*conv_rho

      enddo
      enddo
     CASE (MEGAN2)
      do j = jts,jte
        con2ppm(its:ite,j) = dtstep/(dz8w(its:ite,kts,j)*60.)
      end do
      do n = 1,ne_area
        if( any( e_bio(its:ite,jts:jte,n) /= 0. ) ) then
          do j = jts,jte  
            chem(its:ite,kts,j,n+1) = chem(its:ite,kts,j,n+1) &
                                    + con2ppm(its:ite,j)*e_bio(its:ite,j,n)
          end do
        endif
      end do

     CASE DEFAULT

   END SELECT bioem_select
    END subroutine add_biogenics


END MODULE module_emissions_anthropogenics
