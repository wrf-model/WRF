!WRF:MODEL_LAYER:CHEMICS
!
    subroutine mechanism_driver(id,ktau,dtstep,config_flags,           &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,vcsulf_old,&
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,&
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
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
  USE module_data_racm
  USE module_radm
  USE module_racm
  USE module_aerosols_sorgam
   IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday,                           &
                                  ids,ide, jds,jde, kds,kde,           &
                                  ims,ime, jms,jme, kms,kme,           &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                                      &
                                  ktau
      REAL,      INTENT(IN   ) ::                                      &
                             dtstep,gmt
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

!
! arrays that hold the photolysis rates
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,    &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob
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
                                                      dz8w,            &
                                                      z    ,           &
                                              t8w,p8w,z_at_w ,         &
                                                    rho_phy
! ..
   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
!
! for interaction of aerosls/chemistry (MADE/SORGAM only)
!
      real ,      INTENT(INOUT   ) ::                                  &
                      vdrog3(ims:ime,kms:kme-0,jms:jme,ldrog)
      real ,      INTENT(INOUT   ) ::                                  &
                      vcsulf_old(ims:ime,kms:kme-0,jms:jme)



! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
!
! select chemical mechanism
!
   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2)
       CALL wrf_debug(15,'calling radm2 from chem_driver')
       call radm_driver(id,ktau,dtstep, config_flags,                  &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,         &
               ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,  &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )

     CASE (RADM2SORG)
       CALL wrf_debug(15,'calling radm2 from chem_driver')
       vcsulf_old(its:ite,kts:kte-1,jts:jte)=max(chem(its:ite,kts:kte-1,jts:jte,p_sulf),epsilc)
       call radm_driver(id,ktau,dtstep, config_flags,                  &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,                &
               ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,        &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
     CASE (RACM)
       CALL wrf_debug(15,'calling racm from chem_driver')
       call racm_driver(id,ktau,dtstep,config_flags,              &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,         &
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,&
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd,                                              &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
     CASE (RACMSORG)
       CALL wrf_debug(15,'calling racm/sorgam from chem_driver')
       vcsulf_old(its:ite,kts:kte-1,jts:jte)=max(chem(its:ite,kts:kte-1,jts:jte,p_sulf),epsilc)
       call racm_driver(id,ktau,dtstep,config_flags,              &
               gmt,julday,t_phy,moist,p8w,t8w,                         &
               p_phy,chem,rho_phy,dz8w,z,z_at_w,vdrog3,         &
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,&
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd,                                              &
               ids,ide, jds,jde, kds,kde,                              &
               ims,ime, jms,jme, kms,kme,                              &
               its,ite, jts,jte, kts,kte                               )
     CASE DEFAULT 
                                                     
   END SELECT chem_select                              


!     STOP
    END subroutine mechanism_driver
