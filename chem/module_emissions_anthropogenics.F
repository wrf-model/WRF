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

    real, parameter :: voc_fac = .04*28./250.
    integer :: i,j,k
    real :: conv_rho(its:ite)
    real :: conv_rho_aer(its:ite)

    logical :: is_moz_chm
    character(len=256) :: msg

!--- deposition and emissions stuff


! ..
! ..
! .. Intrinsic Functions ..

         call wrf_debug(15,'add_anthropogenics')

is_moz_chm = config_flags%chem_opt == MOZART_KPP .or. &
             config_flags%chem_opt == MOZCART_KPP .or. &
             config_flags%chem_opt == T1_MOZCART_KPP .or. &
             config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
             config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP
!       
! add emissions
!
      do j=jts,jte  
k_loop: DO k=kts,min(config_flags%kemit,kte)
          conv_rho(its:ite) = 4.828e-4/rho_phy(its:ite,k,j)*dtstep/(dz8w(its:ite,k,j)*60.)
          conv_rho_aer(its:ite) = alt(its:ite,k,j)*dtstep/dz8w(its:ite,k,j)
          chem(its:ite,k,j,p_so2) = chem(its:ite,k,j,p_so2) + emis_ant(its:ite,k,j,p_e_so2)*conv_rho(its:ite)
          chem(its:ite,k,j,p_co)  = chem(its:ite,k,j,p_co) + emis_ant(its:ite,k,j,p_e_co)*conv_rho(its:ite)
          chem(its:ite,k,j,p_no)  = chem(its:ite,k,j,p_no) + emis_ant(its:ite,k,j,p_e_no)*conv_rho(its:ite)
          chem(its:ite,k,j,p_no2) = chem(its:ite,k,j,p_no2) + emis_ant(its:ite,k,j,p_e_no2)*conv_rho(its:ite)
          chem(its:ite,k,j,p_nh3) = chem(its:ite,k,j,p_nh3) + emis_ant(its:ite,k,j,p_e_nh3)*conv_rho(its:ite)
          chem(its:ite,k,j,p_hcl) = chem(its:ite,k,j,p_hcl) + emis_ant(its:ite,k,j,p_e_hcl)*conv_rho(its:ite)
          chem(its:ite,k,j,p_ch3cl) = chem(its:ite,k,j,p_ch3cl) + emis_ant(its:ite,k,j,p_e_ch3cl)*conv_rho(its:ite)
is_mozart:if( is_moz_chm ) then
            chem(its:ite,k,j,p_bigalk) = chem(its:ite,k,j,p_bigalk) + emis_ant(its:ite,k,j,p_e_bigalk)*conv_rho(its:ite)
            chem(its:ite,k,j,p_bigene) = chem(its:ite,k,j,p_bigene) + emis_ant(its:ite,k,j,p_e_bigene)*conv_rho(its:ite)
            chem(its:ite,k,j,p_c2h4)   = chem(its:ite,k,j,p_c2h4)  + emis_ant(its:ite,k,j,p_e_c2h4)*conv_rho(its:ite)
            chem(its:ite,k,j,p_c2h5oh) = chem(its:ite,k,j,p_c2h5oh) + emis_ant(its:ite,k,j,p_e_c2h5oh)*conv_rho(its:ite)
            chem(its:ite,k,j,p_c2h6)   = chem(its:ite,k,j,p_c2h6) + emis_ant(its:ite,k,j,p_e_c2h6)*conv_rho(its:ite)
            chem(its:ite,k,j,p_c3h6)   = chem(its:ite,k,j,p_c3h6) + emis_ant(its:ite,k,j,p_e_c3h6)*conv_rho(its:ite)
            chem(its:ite,k,j,p_c3h8)   = chem(its:ite,k,j,p_c3h8) + emis_ant(its:ite,k,j,p_e_c3h8)*conv_rho(its:ite)
            chem(its:ite,k,j,p_hcho)   = chem(its:ite,k,j,p_hcho) + emis_ant(its:ite,k,j,p_e_ch2o)*conv_rho(its:ite)
            chem(its:ite,k,j,p_ald)    = chem(its:ite,k,j,p_ald) + emis_ant(its:ite,k,j,p_e_ch3cho)*conv_rho(its:ite)
            chem(its:ite,k,j,p_acet)   = chem(its:ite,k,j,p_acet) + emis_ant(its:ite,k,j,p_e_ch3coch3)*conv_rho(its:ite)
            chem(its:ite,k,j,p_ch3oh)  = chem(its:ite,k,j,p_ch3oh) + emis_ant(its:ite,k,j,p_e_ch3oh)*conv_rho(its:ite)
            chem(its:ite,k,j,p_mek)    = chem(its:ite,k,j,p_mek) + emis_ant(its:ite,k,j,p_e_mek)*conv_rho(its:ite)
            chem(its:ite,k,j,p_tol)    = chem(its:ite,k,j,p_tol) + emis_ant(its:ite,k,j,p_e_toluene)*conv_rho(its:ite)
            chem(its:ite,k,j,p_isopr)  = chem(its:ite,k,j,p_isopr) + emis_ant(its:ite,k,j,p_e_isop)*conv_rho(its:ite)
            chem(its:ite,k,j,p_sulf)   = chem(its:ite,k,j,p_sulf) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_sulf)*mwdry/mw_so4_aer*1.e-3
            if( config_flags%emiss_opt == mozmem ) then
              if( p_gly >= param_first_scalar ) then
                chem(its:ite,k,j,p_gly)  = chem(its:ite,k,j,p_gly) + emis_ant(its:ite,k,j,p_e_gly)*conv_rho(its:ite)
              endif
              if( p_mgly >= param_first_scalar ) then
                chem(its:ite,k,j,p_mgly) = chem(its:ite,k,j,p_mgly) + emis_ant(its:ite,k,j,p_e_mgly)*conv_rho(its:ite)
              endif
              if( p_macr >= param_first_scalar ) then
                chem(its:ite,k,j,p_macr) = chem(its:ite,k,j,p_macr) + emis_ant(its:ite,k,j,p_e_macr)*conv_rho(its:ite)
              endif
              if( p_mvk >= param_first_scalar ) then
                chem(its:ite,k,j,p_mvk)  = chem(its:ite,k,j,p_mvk) + emis_ant(its:ite,k,j,p_e_mvk)*conv_rho(its:ite)
              endif
              if( p_c2h2 >= param_first_scalar ) then
                chem(its:ite,k,j,p_c2h2) = chem(its:ite,k,j,p_c2h2)  + emis_ant(its:ite,k,j,p_e_c2h2)*conv_rho(its:ite)
              endif
              if( p_benzene >= param_first_scalar ) then
                chem(its:ite,k,j,p_benzene) = chem(its:ite,k,j,p_benzene) + emis_ant(its:ite,k,j,p_e_benzene)*conv_rho(its:ite)
              endif
              if( p_xyl >= param_first_scalar ) then
                chem(its:ite,k,j,p_xyl) = chem(its:ite,k,j,p_xyl) + emis_ant(its:ite,k,j,p_e_xylene)*conv_rho(its:ite)
              endif
              if( p_apin >= param_first_scalar ) then
                chem(its:ite,k,j,p_apin) = chem(its:ite,k,j,p_apin) + emis_ant(its:ite,k,j,p_e_apin)*conv_rho(its:ite)
              endif
            endif
            if( config_flags%emiss_opt == mozem  .or. config_flags%emiss_opt == mozcem ) then
              if( p_c10h16 >= param_first_scalar ) then
                chem(its:ite,k,j,p_c10h16) = chem(its:ite,k,j,p_c10h16) + emis_ant(its:ite,k,j,p_e_c10h16)*conv_rho(its:ite)
              endif
            endif
            if( config_flags%chem_opt == MOZCART_KPP .or. config_flags%chem_opt == T1_MOZCART_KPP ) then
              if( config_flags%emiss_opt == mozcem .or. config_flags%emiss_opt == mozc_t1_em ) then
                chem(its:ite,k,j,p_p10) = chem(its:ite,k,j,p_p10) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_pm_10)
                chem(its:ite,k,j,p_p25) = chem(its:ite,k,j,p_p25) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_pm_25)
                chem(its:ite,k,j,p_oc1) = chem(its:ite,k,j,p_oc1) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_oc)
                chem(its:ite,k,j,p_bc1) = chem(its:ite,k,j,p_bc1) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_bc)
              endif
              if( config_flags%chem_opt == T1_MOZCART_KPP .and.  config_flags%emiss_opt == mozc_t1_em ) then
                chem(its:ite,k,j,p_gly)   = chem(its:ite,k,j,p_gly) + emis_ant(its:ite,k,j,p_e_gly)*conv_rho(its:ite)
                chem(its:ite,k,j,p_macr)  = chem(its:ite,k,j,p_macr) + emis_ant(its:ite,k,j,p_e_macr)*conv_rho(its:ite)
                chem(its:ite,k,j,p_mgly)  = chem(its:ite,k,j,p_mgly) + emis_ant(its:ite,k,j,p_e_mgly)*conv_rho(its:ite)
                chem(its:ite,k,j,p_c2h2)  = chem(its:ite,k,j,p_c2h2) + emis_ant(its:ite,k,j,p_e_c2h2)*conv_rho(its:ite)
                chem(its:ite,k,j,p_hcooh) = chem(its:ite,k,j,p_hcooh) + emis_ant(its:ite,k,j,p_e_hcooh)*conv_rho(its:ite)
                chem(its:ite,k,j,p_mvk)   = chem(its:ite,k,j,p_mvk) + emis_ant(its:ite,k,j,p_e_mvk)*conv_rho(its:ite)
                chem(its:ite,k,j,p_benzene) = chem(its:ite,k,j,p_benzene) + emis_ant(its:ite,k,j,p_e_benzene)*conv_rho(its:ite)
                chem(its:ite,k,j,p_xylenes) = chem(its:ite,k,j,p_xylenes) + emis_ant(its:ite,k,j,p_e_xylene)*conv_rho(its:ite)
              endif
            elseif( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
              chem(its:ite,k,j,p_hcooh) = chem(its:ite,k,j,p_hcooh) + emis_ant(its:ite,k,j,p_e_hcooh)*conv_rho(its:ite)
              chem(its:ite,k,j,p_hono) = chem(its:ite,k,j,p_hono) + emis_ant(its:ite,k,j,p_e_hono)*conv_rho(its:ite)
              if( config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP ) then
! emissions should be CO_A and CO_BB with yields instead of VOC_A and VOC_BB
!             chem(i,k,j,p_voca)  = chem(i,k,j,p_voca) + conv_rho*emis_ant(i,k,j,p_e_voca)
!             chem(i,k,j,p_vocbb) = chem(i,k,j,p_vocbb) + conv_rho*emis_ant(i,k,j,p_e_vocbb)
!             chem(i,k,j,p_voca)  = chem(i,k,j,p_voca) + conv_rho_aer*emis_ant(i,k,j,p_e_co_a)*0.04*28./250.
!             chem(i,k,j,p_vocbb) = chem(i,k,j,p_vocbb) + conv_rho_aer*emis_ant(i,k,j,p_e_co_bb)*0.04*28./250.
                chem(its:ite,k,j,p_voca)  = chem(its:ite,k,j,p_voca) + conv_rho(its:ite)*emis_ant(its:ite,k,j,p_e_co_a)*voc_fac
                chem(its:ite,k,j,p_vocbb) = chem(its:ite,k,j,p_vocbb) + conv_rho(its:ite)*emis_ant(its:ite,k,j,p_e_co_bb)*voc_fac
              endif
            endif
          else if( config_flags%chem_opt == CRIMECH_KPP .or. &
                   config_flags%chem_opt == CRI_MOSAIC_8BIN_AQ_KPP .or. &
                   config_flags%chem_opt == CRI_MOSAIC_4BIN_AQ_KPP) then
            do i=its,ite  
              chem(i,k,j,p_c2h6) = chem(i,k,j,p_c2h6) + emis_ant(i,k,j,p_e_c2h6)*conv_rho(i)
              chem(i,k,j,p_c2h4) = chem(i,k,j,p_c2h4) + emis_ant(i,k,j,p_e_c2h4)*conv_rho(i)
              chem(i,k,j,p_c5h8) = chem(i,k,j,p_c5h8) + emis_ant(i,k,j,p_e_c5h8)*conv_rho(i)
              chem(i,k,j,p_tm123b) = chem(i,k,j,p_tm123b) + emis_ant(i,k,j,p_e_tm123b)*conv_rho(i)
              chem(i,k,j,p_tm124b) = chem(i,k,j,p_tm124b) + emis_ant(i,k,j,p_e_tm124b)*conv_rho(i)
              chem(i,k,j,p_tm135b) = chem(i,k,j,p_tm135b) + emis_ant(i,k,j,p_e_tm135b)*conv_rho(i)
              chem(i,k,j,p_oethtol) = chem(i,k,j,p_oethtol) + emis_ant(i,k,j,p_e_oethtol)*conv_rho(i)
              chem(i,k,j,p_methtol) = chem(i,k,j,p_methtol) + emis_ant(i,k,j,p_e_methtol)*conv_rho(i)
              chem(i,k,j,p_pethtol) = chem(i,k,j,p_pethtol) + emis_ant(i,k,j,p_e_pethtol)*conv_rho(i)
              chem(i,k,j,p_dime35eb) = chem(i,k,j,p_dime35eb) + emis_ant(i,k,j,p_e_dime35eb)*conv_rho(i)
              chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho) + emis_ant(i,k,j,p_e_hcho)*conv_rho(i)
              chem(i,k,j,p_ch3cho) = chem(i,k,j,p_ch3cho) + emis_ant(i,k,j,p_e_ch3cho)*conv_rho(i)
              chem(i,k,j,p_c2h5cho) = chem(i,k,j,p_c2h5cho) + emis_ant(i,k,j,p_e_c2h5cho)*conv_rho(i)
              chem(i,k,j,p_ket) = chem(i,k,j,p_ket) + emis_ant(i,k,j,p_e_ket)*conv_rho(i)
              chem(i,k,j,p_mek) = chem(i,k,j,p_mek) + emis_ant(i,k,j,p_e_mek)*conv_rho(i)
              chem(i,k,j,p_ch3oh) = chem(i,k,j,p_ch3oh) + emis_ant(i,k,j,p_e_ch3oh)*conv_rho(i)
              chem(i,k,j,p_c2h5oh) = chem(i,k,j,p_c2h5oh) + emis_ant(i,k,j,p_e_c2h5oh)*conv_rho(i)
              chem(i,k,j,p_c3h6) = chem(i,k,j,p_c3h6) + emis_ant(i,k,j,p_e_c3h6)*conv_rho(i)
              chem(i,k,j,p_c2h2) = chem(i,k,j,p_c2h2) + emis_ant(i,k,j,p_e_c2h2)*conv_rho(i)
              chem(i,k,j,p_benzene) = chem(i,k,j,p_benzene) + emis_ant(i,k,j,p_e_benzene)*conv_rho(i)
              chem(i,k,j,p_nc4h10) = chem(i,k,j,p_nc4h10) + emis_ant(i,k,j,p_e_nc4h10)*conv_rho(i)
              chem(i,k,j,p_toluene) = chem(i,k,j,p_toluene) + emis_ant(i,k,j,p_e_toluene)*conv_rho(i)
              chem(i,k,j,p_oxyl) = chem(i,k,j,p_oxyl) + emis_ant(i,k,j,p_e_oxyl)*conv_rho(i)
              chem(i,k,j,p_c3h8) = chem(i,k,j,p_c3h8) + emis_ant(i,k,j,p_e_c3h8)*conv_rho(i)
              chem(i,k,j,p_tbut2ene) = chem(i,k,j,p_tbut2ene) + emis_ant(i,k,j,p_e_tbut2ene)*conv_rho(i)
              chem(i,k,j,p_ch3co2h) = chem(i,k,j,p_ch3co2h) + emis_ant(i,k,j,p_e_ch3co2h)*conv_rho(i)
            end do
          else is_mozart
            do i=its,ite  
              chem(i,k,j,p_csl)  = chem(i,k,j,p_csl) + emis_ant(i,k,j,p_e_csl)*conv_rho(i)
              chem(i,k,j,p_iso)  = chem(i,k,j,p_iso) + emis_ant(i,k,j,p_e_iso)*conv_rho(i)
              chem(i,k,j,p_ald)  = chem(i,k,j,p_ald) + emis_ant(i,k,j,p_e_ald)*conv_rho(i)
              chem(i,k,j,p_hcho) = chem(i,k,j,p_hcho) + emis_ant(i,k,j,p_e_hcho)*conv_rho(i)
              chem(i,k,j,p_ora2) = chem(i,k,j,p_ora2) + emis_ant(i,k,j,p_e_ora2)*conv_rho(i)
              chem(i,k,j,p_hc3)  = chem(i,k,j,p_hc3) + emis_ant(i,k,j,p_e_hc3)*conv_rho(i)
              chem(i,k,j,p_hc5)  = chem(i,k,j,p_hc5) + emis_ant(i,k,j,p_e_hc5)*conv_rho(i)
              chem(i,k,j,p_hc8)  = chem(i,k,j,p_hc8) + emis_ant(i,k,j,p_e_hc8)*conv_rho(i)
              chem(i,k,j,p_eth)  = chem(i,k,j,p_eth) + emis_ant(i,k,j,p_e_eth)*conv_rho(i)
              chem(i,k,j,p_olt)  = chem(i,k,j,p_olt) + emis_ant(i,k,j,p_e_olt)*conv_rho(i)
              chem(i,k,j,p_oli)  = chem(i,k,j,p_oli) + emis_ant(i,k,j,p_e_oli)*conv_rho(i)
              chem(i,k,j,p_tol)  = chem(i,k,j,p_tol) + emis_ant(i,k,j,p_e_tol)*conv_rho(i)
              chem(i,k,j,p_xyl)  = chem(i,k,j,p_xyl) + emis_ant(i,k,j,p_e_xyl)*conv_rho(i)
              chem(i,k,j,p_ket)  = chem(i,k,j,p_ket) + emis_ant(i,k,j,p_e_ket)*conv_rho(i)
            end do
            if(p_ch4 >= param_first_scalar) then
              chem(its:ite,k,j,p_ch4) = chem(its:ite,k,j,p_ch4) + emis_ant(its:ite,k,j,p_e_ch4)*conv_rho(its:ite)
            end if
            if(p_ol2 >= param_first_scalar) then
              chem(its:ite,k,j,p_ol2) = chem(its:ite,k,j,p_ol2) + emis_ant(its:ite,k,j,p_e_ol2)*conv_rho(its:ite)
            end if
            if(p_ete >= param_first_scalar) then
              chem(its:ite,k,j,p_ete) = chem(its:ite,k,j,p_ete) + emis_ant(its:ite,k,j,p_e_ol2)*conv_rho(its:ite)
            end if
            if( config_flags%chem_opt == GOCARTRACM_KPP .or. config_flags%chem_opt == GOCARTRADM2 ) then
              chem(its:ite,k,j,p_p10) = chem(its:ite,k,j,p_p10) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_pm_10)
              chem(its:ite,k,j,p_p25) = chem(its:ite,k,j,p_p25) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_pm_25)
              chem(its:ite,k,j,p_oc1) = chem(its:ite,k,j,p_oc1) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_oc)
              chem(its:ite,k,j,p_bc1) = chem(its:ite,k,j,p_bc1) + conv_rho_aer(its:ite)*emis_ant(its:ite,k,j,p_e_bc)
            endif
          endif is_mozart
        END DO k_loop
      END DO                                                          

! add aircraft emissions
   if (config_flags%aircraft_emiss_opt == 1 ) then
      do j=jts,jte  
        do k=kts,min(config_flags%kemit_aircraft,kte)
          conv_rho(its:ite)=4.828e-4/rho_phy(i,k,j)*dtstep/(dz8w(i,k,j)*60.)
          if( p_no >= param_first_scalar ) then
            chem(its:ite,k,j,p_no)  = chem(its:ite,k,j,p_no)  + emis_aircraft(its:ite,k,j,p_eac_no) *conv_rho(its:ite)
          endif
          if( p_co >= param_first_scalar ) then
            chem(its:ite,k,j,p_co)  = chem(its:ite,k,j,p_co)  + emis_aircraft(its:ite,k,j,p_eac_co) *conv_rho(its:ite)
          endif
          if( p_so2 >= param_first_scalar ) then
            chem(its:ite,k,j,p_so2) = chem(its:ite,k,j,p_so2) + emis_aircraft(its:ite,k,j,p_eac_so2)*conv_rho(its:ite)
          endif
          if( p_ch4 >= param_first_scalar ) then
            chem(its:ite,k,j,p_ch4) = chem(its:ite,k,j,p_ch4) + emis_aircraft(its:ite,k,j,p_eac_ch4)*conv_rho(its:ite)
          endif
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
    real :: conv_rho(its:ite)
    real :: con2ppm(its:ite,jts:jte)
!--- deposition and emissions stuff
! .. Parameters ..       
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags     

   bioem_select: SELECT CASE(config_flags%bio_emiss_opt)
     CASE (GUNTHER1)
     CALL wrf_debug(15,'adding biogenic emissions: Gunther1')
      do j=jts,jte  
        conv_rho(its:ite) = dtstep/(dz8w(its:ite,kts,j)*60.)
        chem(its:ite,kts,j,p_iso)=chem(its:ite,kts,j,p_iso) + e_bio(its:ite,j,p_iso-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_oli)=chem(its:ite,kts,j,p_oli) + e_bio(its:ite,j,p_oli-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_xyl)=chem(its:ite,kts,j,p_xyl) + e_bio(its:ite,j,p_xyl-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_olt)=chem(its:ite,kts,j,p_olt) + e_bio(its:ite,j,p_olt-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ket)=chem(its:ite,kts,j,p_ket) + e_bio(its:ite,j,p_ket-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ald)=chem(its:ite,kts,j,p_ald) + e_bio(its:ite,j,p_ald-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_hcho)=chem(its:ite,kts,j,p_hcho) + e_bio(its:ite,j,p_hcho-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_eth)=chem(its:ite,kts,j,p_eth) + e_bio(its:ite,j,p_eth-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ora2)=chem(its:ite,kts,j,p_ora2) + e_bio(its:ite,j,p_ora2-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_co)=chem(its:ite,kts,j,p_co) + e_bio(its:ite,j,p_co-1)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_no)=chem(its:ite,kts,j,p_no) + e_bio(its:ite,j,p_no-1)*conv_rho(its:ite)
!
! RADM only
!
        if(p_hc3 >= param_first_scalar) then
          chem(its:ite,kts,j,p_hc3)=chem(its:ite,kts,j,p_hc3)+ e_bio(its:ite,j,p_hc3-1)*conv_rho(its:ite)
        endif
        if(p_ol2 >= param_first_scalar) then
          chem(its:ite,kts,j,p_ol2)=chem(its:ite,kts,j,p_ol2)+ e_bio(its:ite,j,p_ol2-1)*conv_rho(its:ite)
        endif
! CRIMECH only
        if(p_c5h8 >= param_first_scalar) then
          chem(its:ite,kts,j,p_c5h8)=chem(its:ite,kts,j,p_c5h8)+ e_bio(its:ite,j,liso)*conv_rho(its:ite)
        endif
        if(p_oxyl >= param_first_scalar) then
          chem(its:ite,kts,j,p_oxyl)=chem(its:ite,kts,j,p_oxyl)+ e_bio(its:ite,j,lxyl)*conv_rho(its:ite)
        endif
        if(p_c3h8 >= param_first_scalar) then
          chem(its:ite,kts,j,p_c3h8)=chem(its:ite,kts,j,p_c3h8)+ e_bio(its:ite,j,lhc3)*conv_rho(its:ite)
        endif
        if(p_ket >= param_first_scalar) then
          chem(its:ite,kts,j,p_ket)=chem(its:ite,kts,j,p_ket)+ e_bio(its:ite,j,lket)*conv_rho(its:ite)
        endif
        if(p_ch3cho >= param_first_scalar) then
          chem(its:ite,kts,j,p_ch3cho)=chem(its:ite,kts,j,p_ch3cho)+ e_bio(its:ite,j,lald)*conv_rho(its:ite)
        endif
        if(p_apinene >= param_first_scalar) then
          chem(its:ite,kts,j,p_apinene)=chem(its:ite,kts,j,p_apinene)+ 0.667*e_bio(its:ite,j,loli)*conv_rho(its:ite)
        endif
        if(p_bpinene >= param_first_scalar) then
          chem(its:ite,kts,j,p_bpinene)=chem(its:ite,kts,j,p_bpinene)+ .333*(e_bio(its:ite,j,loli) + e_bio(its:ite,j,lolt))*conv_rho(its:ite)
        endif
        if(p_hcooh >= param_first_scalar) then
          chem(its:ite,kts,j,p_hcooh)=chem(its:ite,kts,j,p_hcooh)+ e_bio(its:ite,j,lora1)*conv_rho(its:ite)
        endif
        if(p_ch3co2h >= param_first_scalar) then
          chem(its:ite,kts,j,p_ch3co2h)=chem(its:ite,kts,j,p_ch3co2h)+ e_bio(its:ite,j,lora2)*conv_rho(its:ite)
        endif

!BSINGH -  Added for CBMZ
!
!CBMZ Only
!
        if(p_par .gt. 1) then
          chem(its:ite,kts,j,p_par) = chem(its:ite,kts,j,p_par)  &
                + (e_bio(its:ite,j,p_ald-1)*0.4        &
                + e_bio(its:ite,j,p_ket-1)*0.9         &     
                + e_bio(its:ite,j,p_olt-1)*1.8         &
                + e_bio(its:ite,j,p_ora2-1))*conv_rho(its:ite)
        endif
        if(p_hc3 .gt. 1) then
          chem(its:ite,kts,j,p_par) = chem(its:ite,kts,j,p_par) + e_bio(its:ite,j,p_hc3-1)*4.8*conv_rho(its:ite)
        endif
      end do

     CASE (BEIS314)
     CALL wrf_debug(100,'adding biogenic emissions: beis3.1.4')
      do j=jts,jte  
        conv_rho(its:ite)=4.828e-4/rho_phy(its:ite,kts,j)*dtstep/(dz8w(its:ite,kts,j)*60.)
        chem(its:ite,kts,j,p_iso)=chem(its:ite,kts,j,p_iso)+ ebio_iso(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_oli)=chem(its:ite,kts,j,p_oli)+ ebio_oli(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_xyl)=chem(its:ite,kts,j,p_xyl)+ ebio_xyl(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_hc3)=chem(its:ite,kts,j,p_hc3)+ ebio_hc3(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_olt)=chem(its:ite,kts,j,p_olt)+ ebio_olt(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ket)=chem(its:ite,kts,j,p_ket)+ ebio_ket(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ald)=chem(its:ite,kts,j,p_ald)+ ebio_ald(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_hcho)=chem(its:ite,kts,j,p_hcho)+ ebio_hcho(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_eth)=chem(its:ite,kts,j,p_eth)+ ebio_eth(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_ora2)=chem(its:ite,kts,j,p_ora2)+ ebio_ora2(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_co)=chem(its:ite,kts,j,p_co)+ ebio_co(its:ite,j)*conv_rho(its:ite)
        chem(its:ite,kts,j,p_no)=chem(its:ite,kts,j,p_no)+ ebio_no(its:ite,j)*conv_rho(its:ite)
!
! RADM only
!
        if(p_ol2 >= param_first_scalar) then
          chem(its:ite,kts,j,p_ol2)=chem(its:ite,kts,j,p_ol2)+ ebio_ete(its:ite,j)*conv_rho(its:ite)
        endif
!
! RACM only
!
        if(p_api >= param_first_scalar) then
          chem(its:ite,kts,j,p_api)=chem(its:ite,kts,j,p_api)+ ebio_api(its:ite,j)*conv_rho(its:ite)
        endif
        if(p_lim >= param_first_scalar) then
          chem(its:ite,kts,j,p_lim)=chem(its:ite,kts,j,p_lim)+ ebio_lim(its:ite,j)*conv_rho(its:ite)
        endif
        if(p_ete >= param_first_scalar) then
          chem(its:ite,kts,j,p_ete)=chem(its:ite,kts,j,p_ete)+ ebio_ete(its:ite,j)*conv_rho(its:ite)
        endif
!
! SESQ and MBO are added, it works for RACM_SOA_VBS_KPP option
!
        if(p_sesq >= param_first_scalar) then
          chem(its:ite,kts,j,p_sesq)=chem(its:ite,kts,j,p_sesq)+ ebio_sesq(its:ite,j)*conv_rho(its:ite)
        endif
        if(p_mbo >= param_first_scalar) then
          chem(its:ite,kts,j,p_mbo)=chem(its:ite,kts,j,p_mbo)+ ebio_mbo(its:ite,j)*conv_rho(its:ite)
        endif
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
