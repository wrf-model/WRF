      MODULE module_racm
      USE module_data_racm
      USE module_data_sorgam

      CONTAINS

      subroutine racm_driver(id,ktau,dtstep,config_flags,              &
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


  USE module_configure
  USE module_state_description
  USE module_model_constants
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
!  advected chemical tracers
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),             &
         INTENT(INOUT ) ::                                chem

!
! arrays that hold photolysis rates
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
           ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,    &
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob
!
! arrays that hold the radicals
!
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                       &
         INTENT(INOUT ) ::                                             &
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd
!
! on input from met model 
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,           &
          INTENT(IN   ) ::                                             &
                                                      t_phy,           &
                                                      p_phy,           &
                                                      dz8w,            &
                                                      z    ,           &
                                              t8w,p8w,z_at_w ,         &
                                                    rho_phy
!
! for interaction with aerosols (really is output)
!
      real ,      INTENT(INOUT) ::                                     &
                      vdrog3(ims:ime,kms:kme-0,jms:jme,ldrog)
      TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dt60, dtcmax, dtcmin, xtime
      INTEGER :: i,j,k,iprt, jce, jcs, n, nr, ipr,jpr,nvr
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte-1), rh(kts:kte-1),       &
        t(kts:kte-1), vcinp(nspec),wlc(kts:kte-1),p1,wvap
        REAL*8 :: t_in,t_end, rj(nphot)
      real :: PRDROG(ldrog)
	REAL*8 :: ATOL,rtol,temp
!     real *8 :: saverad(its:ite,kts:kte,jts:jte,50)
      real *8 :: rxylho,rtolho,rcslho,rcslno3,rhc8ho,roliho,rolino3,      &
                 rolio3,roltho,roltno3,rolto3,rapiho,rapino3,rapio3,      &
                 rlimho,rlimno3,rlimo3
!     EXTERNAL arr
!     REAL*8 :: arr

!

      INTEGER :: ixhour,iaerosol_sorgam
      real :: xhour,xmin,xtimin
      real :: const2,tinv,pot,pt2
      END SUBROUTINE racm_driver
      END MODULE module_racm
