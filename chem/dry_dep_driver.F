!
!   WRF-chem V3.2 : Original version of dry_dep_driver written by Georg Grell (ESRL/GSD)
!                   VERTMX was originally developed by Mariusz Pagowski and modified by
!                   Richard C. Easter (PNNL)
! 10/11/2011 - Ravan Ahmadov (NOAA) updated to include the RACM_SOA_VBS option
!
!WRF:MODEL_LAYER:CHEMICS
!
MODULE module_dry_dep_driver
  IMPLICIT NONE

CONTAINS

    subroutine dry_dep_driver(id,curr_secs,ktau,dtstep,config_flags,      &
               gmt,julday,t_phy,moist,scalar,p8w,t8w,w,alt,               &
               p_phy,chem,tracer,rho_phy,dz8w,rh,exch_h,hfx,dx,           &  
               cldfra, cldfra_old,raincv,seasin,dustin,                   &
               ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,               &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               xland,ash_fall,h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,       &
               anh3,cvaro1,cvaro2,                                        &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,      &
               ddlen,ddflx,                                               &
               emis_ant,ebu_in,                                           &
               sf_urban_physics,numgas,current_month,dvel,snowh,          & 
               dustdrydep_1,dustdrydep_2,dustdrydep_3,                    &
               dustdrydep_4,dustdrydep_5,                                 &
               depvelocity,                                               &               
               dustgraset_1,dustgraset_2,dustgraset_3,                    &
               dustgraset_4,dustgraset_5,                                 &
               setvel_1,setvel_2,setvel_3,setvel_4,setvel_5, imod,        &         
               is_CAMMGMP_used,                                           &
               dep_vel,num_vert_mix,                                      &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!----------------------------------------------------------------------
  USE module_model_constants
  USE module_configure
  USE module_state_description
  USE module_domain_type, only : domain
  USE module_dep_simple
  USE module_vertmx_wrf
  USE module_data_sorgam
  USE module_aerosols_sorgam
  USE module_gocart_settling
  USE module_vash_settling
  USE module_gocart_drydep
  USE module_mosaic_drydep, only:  mosaic_drydep_driver
  USE module_mixactivate_wrappers, only: mosaic_mixactivate, sorgam_mixactivate, &
                                         sorgam_vbs_mixactivate, soa_vbs_mixactivate
  USE module_aer_drydep
  USE module_aerosols_soa_vbs, only: soa_vbs_depdriver

  USE module_cam_mam_drydep, only:  cam_mam_drydep_driver
  
  use module_cam_support, only: pcnst => pcnst_runtime
  USE module_data_cam_mam_asect, only: lptr_chem_to_q, lptr_chem_to_qqcw !Balwinder.Singh@pnnl.gov: Added to avoid mixing of CHEM array constituents multiple times
  USE modal_aero_data,         only: numptr_amode, lmassptr_amode, ntot_amode, nspec_amode !Added by Balwinder.Singh@pnnl.gov to avoid mixing of CHEM array constituents multiple times
  USE module_cam_mam_drydep, only:  cam_mam_drydep_driver
  use module_scalar_tables,     only: chem_dname_table !Balwinder.Singh@pnnl.gov:Added for MAM aerosols dry deposition
  USE module_aerosols_sorgam_vbs, only: sorgam_vbs_depdriver
  
  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags
   LOGICAL, INTENT(IN)                           :: is_CAMMGMP_used !BSINGH:01/31/2013: Added is_CAMMGMP_used for MAM drydep
   INTEGER,      INTENT(IN   ) :: id,julday,                    &
                                  sf_urban_physics,             &
                                  numgas,                       &
                                  current_month,                &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: ktau
   integer l
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_scalar ),       &
         INTENT(INOUT ) ::                               scalar
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                 chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_tracer ),         &
         INTENT(INOUT ) ::                                 tracer

   REAL, DIMENSION( ims:ime, 1:config_flags%kemit, jms:jme,num_emis_ant),&
         INTENT(IN ) ::                                    emis_ant

   REAL, DIMENSION( ims:ime, 1, jms:jme, num_ebu_in ),                     &
         INTENT(INOUT ) ::                                   ebu_in

   REAL, DIMENSION( ims:ime, config_flags%kdepvel, jms:jme, config_flags%ndepvel ), &
         INTENT(INOUT ) ::                                   dep_vel

   REAL, DIMENSION( ims:ime, config_flags%kdvel, jms:jme, num_dvel ), &
         INTENT(INOUT ) ::                                   dvel


   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                        alt,    &
                                                      p_phy,    &
                                                      dz8w,     &
                                                        rh,     &
                                              t8w,p8w,z_at_w ,  &
                                                            w,  &
                                              exch_h,rho_phy,z
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(INOUT) ::                                      &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2
   INTEGER,DIMENSION( ims:ime , jms:jme )                  ,    &
          INTENT(IN   ) ::                                      &
                                                     ivgtyp
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
          INTENT(INOUT) ::                                      &
                                                     tsk,       &
                                                     gsw,       &
                                                  vegfra,       &
                                                     pbl,       &
                                                     rmol,       &
                                                     ust,       &
                                                     hfx,       &
                                                     xlat,      &
                                                     xlong,     &
                                                     snowh,     &
                                          xland,znt,raincv,ash_fall
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(INOUT ) ::                                     &
                    cldfra,     & ! cloud fraction current timestep
                    cldfra_old    ! cloud fraction previous timestep
   REAL,  DIMENSION( ims:ime , jms:jme, 5 )                   ,    &
          INTENT(IN) ::            seasin,dustin 
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
          INTENT(OUT) ::                                      &
                                                     dep_vel_o3
   REAL,  DIMENSION( ims:ime , jms:jme , num_chem )                   ,    &
          INTENT(INOUT) ::                                      &
                    ddlen, & !dry deposition length
                    ddflx    !dry deposition flux

   REAL, INTENT(OUT), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat

      REAL,      INTENT(IN   ) ::                               &
                             dtstep,gmt,dx

      INTEGER,  INTENT(INOUT) :: num_vert_mix
      
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::        &
                dustdrydep_1, dustdrydep_2, dustdrydep_3,       &
                dustdrydep_4, dustdrydep_5, depvelocity
   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(INOUT) ::        &
               dustgraset_1,dustgraset_2,dustgraset_3,          &
               dustgraset_4,dustgraset_5,                       &
               setvel_1,setvel_2,setvel_3,setvel_4,setvel_5   
   INTEGER, INTENT(IN)  ::     imod
   
!--- deposition and emissions stuff
! .. Parameters ..
! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dvfog, dvpart,  &
        rad, rhchem, ta, ustar, vegfrac, z1,zntt
      REAL ::  old, new, fac

      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                 n, nr, ipr, jpr, nvr,   &
                 idrydep_onoff, aer_mech_id
      INTEGER :: l2,m,lnum,lmass

      LOGICAL :: highnh3, rainflag, vegflag, wetflag
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte)
   REAL, DIMENSION( its:ite, jts:jte, num_chem ) ::   ddvel
   REAL, DIMENSION( num_chem )                   ::   ddmassn

   REAL, DIMENSION( ims:ime, jms:jme, num_chem ) ::   qsrflx ! dry deposition flux of aerosols (explicit aq.-phase cases)

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dryrho_phy
   REAL,  DIMENSION( kms:kme ) :: dryrho_1d

! turbulent transport
   real :: pblst(kts:kte),ekmfull(kts:kte+1),zzfull(kts:kte+1),zz(kts:kte)
   integer :: ii,jj,kk,i,j,k,nv
   integer :: ll   
!
! necessary for aerosols (module dependent)
!
   REAL, DIMENSION( its:ite, jts:jte ) ::   aer_res, aer_res_def, aer_res_zcen

   INTEGER, DIMENSION( pcnst )    ::   lptr_q_to_chem !Balwinder.Singh@pnnl.gov: pointer from Q array to CHEM array
   LOGICAL, DIMENSION( num_chem ) ::   vertMixAero    !Balwinder.Singh@pnnl.gov: Flag (true or false) which decides whether to  vertical mix a constituent or not

   real, parameter :: m2cm = 100.

   REAL RSI                   ! gas constant in SI units (J/mol-K)
   PARAMETER (RSI = 8.314510) ! RSI is the same as RGASUNIV in CONST.EXT

   integer :: k_a, k_c, kmax, m_mam
   real, dimension( its:ite, jts:jte ) :: frac_removed


! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min

      !Balwinder.Singh@pnnl.gov: Following code sets vertMixAero(:) flag to true or false based on whether
      !a particular CHEM array constituent should be mixed (vertically) by dry_dep_driver or not
      !The constituents which are  mixed in CAMMGMP microphysics are NOT mixed in dry_dep_driver. 
      if(is_CAMMGMP_used) then ! If CAMMGMP microphysics is selected
         vertMixAero(:)    = .FALSE. 
         lptr_q_to_chem(:) = -999888777 ! pointer which maps Q array (CAM's STATE%Q array) to CHEM array
         
         do nv = 2, num_chem
            l2 = lptr_chem_to_q(nv)
            if (l2 >= 0) then 
               vertMixAero(nv) = .TRUE. !Set default true
               lptr_q_to_chem(l2) = nv
            end if
         enddo
         
         !Set constitutents, which are mixed in CAMMGMP, False
         do m = 1, ntot_amode
            lnum = numptr_amode(m)
            if( lnum > 0 ) then
               vertMixAero(lptr_q_to_chem(lnum)) = .FALSE.
            endif
            do l = 1, nspec_amode(m)
               lmass = lmassptr_amode(l,m)
               vertMixAero(lptr_q_to_chem(lmass)) = .FALSE.
            enddo
         enddo
      endif

!
! compute dry deposition velocities = ddvel
!
! 28-jun-2005 rce - initialize ddvel=0; call aerosol drydep routine
!           only when gas_drydep_opt == WESELY
!       the wesely_driver routine computes aer_res, and currently
!	    you cannot compute aerosol drydep without it !!
! 08-jul-2005 rce - pass idrydep_onoff to mixactivate routines
!
   ddvel(:,:,:) = 0.0
   idrydep_onoff = 0

   drydep_select: SELECT CASE(config_flags%gas_drydep_opt)

     CASE ( WESELY )
!
! gas_drydep_opt == WESELY means 
!     wesely for gases 
!     other (appropriate) routine for aerosols
!
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES WITH WESELY METHOD')

       IF( config_flags%chem_opt /= CHEM_TRACER    .and.                  &
           config_flags%chem_opt /= CHEM_TRACE2    .and.                  &
           config_flags%chem_opt /= CO2_TRACER     .and.                  & 
           config_flags%chem_opt /= GHG_TRACER     .and.                  &
           config_flags%chem_opt /= CHEM_VASH      .and.                  &
           config_flags%chem_opt /= CHEM_VOLC_4BIN .and.                  &
           config_flags%chem_opt /= DUST           .and.                  &
           config_flags%chem_opt /= GOCART_SIMPLE  .and.                  &
           config_flags%chem_opt /= GOCARTRACM_KPP )THEN
          call wesely_driver(id,ktau,dtstep,                              &
               config_flags,current_month,                                &
               gmt,julday,t_phy,moist,p8w,t8w,raincv,                     &
               p_phy,chem,rho_phy,dz8w,ddvel,aer_res_def,aer_res_zcen,    &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               snowh,numgas,                                              &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!-----------------------------------------------------------------
! this does aerosol species (dust,seas, bc,oc) for gocart only
!-----------------------------------------------------------------
          IF ( config_flags%chem_opt == MOZCART_KPP .or. &
               config_flags%chem_opt == T1_MOZCART_KPP ) then
            call gocart_drydep_driver( dtstep,                            &
                  config_flags, numgas,                                   &
                  t_phy, moist, p8w, t8w, rmol,aer_res_def,               &
                  p_phy, chem, rho_phy, dz8w, ddvel, xland, hfx,          &
                  ivgtyp, tsk, vegfra, pbl, ust, znt, xlat, xlong,        &
                  dustdrydep_1,dustdrydep_2,dustdrydep_3,                 &
                  dustdrydep_4,dustdrydep_5,                              &
                  depvelocity,                                            &                       
                  ids,ide, jds,jde, kds,kde,                              &
                  ims,ime, jms,jme, kms,kme,                              &
                  its,ite, jts,jte, kts,kte )
          ENDIF

         if( config_flags%diagnostic_chem == DEPVEL1 .and. &
              config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP ) then
               do j = jts,jte
                  dvel(its:ite,1,j,p_dvel_o3) = m2cm*ddvel(its:ite,j,p_o3)
                  dvel(its:ite,1,j,p_dvel_no) = m2cm*ddvel(its:ite,j,p_no)
                  dvel(its:ite,1,j,p_dvel_no2) = m2cm*ddvel(its:ite,j,p_no2)
                  dvel(its:ite,1,j,p_dvel_nh3) = m2cm*ddvel(its:ite,j,p_nh3)
                  dvel(its:ite,1,j,p_dvel_so2) = m2cm*ddvel(its:ite,j,p_so2)
                  dvel(its:ite,1,j,p_dvel_so4) = m2cm*ddvel(its:ite,j,p_sulf)
                  dvel(its:ite,1,j,p_dvel_hno3) = m2cm*ddvel(its:ite,j,p_hno3)
                enddo
          endif

          if( config_flags%diagnostic_chem == DEPVEL1 .and. &
              (config_flags%chem_opt == MOZCART_KPP .or. &
               config_flags%chem_opt == T1_MOZCART_KPP .or. &
              config_flags%chem_opt == MOZART_KPP   .or. & 
              config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
              config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) ) then
               do j = jts,jte
                  dvel(its:ite,1,j,p_dvel_o3) = m2cm*ddvel(its:ite,j,p_o3)
                  dvel(its:ite,1,j,p_dvel_no) = m2cm*ddvel(its:ite,j,p_no)
                  dvel(its:ite,1,j,p_dvel_no2) = m2cm*ddvel(its:ite,j,p_no2)
                  dvel(its:ite,1,j,p_dvel_nh3) = m2cm*ddvel(its:ite,j,p_nh3)
                  dvel(its:ite,1,j,p_dvel_hno3) = m2cm*ddvel(its:ite,j,p_hno3)
                  dvel(its:ite,1,j,p_dvel_hno4) = m2cm*ddvel(its:ite,j,p_hno4)
                  dvel(its:ite,1,j,p_dvel_h2o2) = m2cm*ddvel(its:ite,j,p_h2o2)
                  dvel(its:ite,1,j,p_dvel_co) = m2cm*ddvel(its:ite,j,p_co)
                  dvel(its:ite,1,j,p_dvel_ch3ooh) = m2cm*ddvel(its:ite,j,p_ch3ooh)
                  dvel(its:ite,1,j,p_dvel_hcho) = m2cm*ddvel(its:ite,j,p_hcho)
                  dvel(its:ite,1,j,p_dvel_ch3oh) = m2cm*ddvel(its:ite,j,p_ch3oh)
                  dvel(its:ite,1,j,p_dvel_eo2) = m2cm*ddvel(its:ite,j,p_eo2)
                  dvel(its:ite,1,j,p_dvel_ald) = m2cm*ddvel(its:ite,j,p_ald)
                  dvel(its:ite,1,j,p_dvel_ch3cooh) = m2cm*ddvel(its:ite,j,p_ch3cooh)
                  dvel(its:ite,1,j,p_dvel_acet) = m2cm*ddvel(its:ite,j,p_acet)
                  dvel(its:ite,1,j,p_dvel_mgly) = m2cm*ddvel(its:ite,j,p_mgly)
                  dvel(its:ite,1,j,p_dvel_gly) = m2cm*ddvel(its:ite,j,p_gly)
                  dvel(its:ite,1,j,p_dvel_paa) = m2cm*ddvel(its:ite,j,p_paa)
                  dvel(its:ite,1,j,p_dvel_pooh) = m2cm*ddvel(its:ite,j,p_c3h6ooh)
                  dvel(its:ite,1,j,p_dvel_mpan) = m2cm*ddvel(its:ite,j,p_mpan)
                  dvel(its:ite,1,j,p_dvel_mco3) = m2cm*ddvel(its:ite,j,p_mco3)
                  dvel(its:ite,1,j,p_dvel_mvkooh) = m2cm*ddvel(its:ite,j,p_mvkooh)
                  dvel(its:ite,1,j,p_dvel_c2h5oh) = m2cm*ddvel(its:ite,j,p_c2h5oh)
                  dvel(its:ite,1,j,p_dvel_etooh) = m2cm*ddvel(its:ite,j,p_etooh)
                  dvel(its:ite,1,j,p_dvel_prooh) = m2cm*ddvel(its:ite,j,p_prooh)
                  dvel(its:ite,1,j,p_dvel_acetp) = m2cm*ddvel(its:ite,j,p_acetp)
                  dvel(its:ite,1,j,p_dvel_onit) = m2cm*ddvel(its:ite,j,p_onit)
                  dvel(its:ite,1,j,p_dvel_onitr) = m2cm*ddvel(its:ite,j,p_onitr)
                  dvel(its:ite,1,j,p_dvel_isooh) = m2cm*ddvel(its:ite,j,p_isooh)
                  dvel(its:ite,1,j,p_dvel_acetol) = m2cm*ddvel(its:ite,j,p_acetol)
                  dvel(its:ite,1,j,p_dvel_glyald) = m2cm*ddvel(its:ite,j,p_glyald)
                  dvel(its:ite,1,j,p_dvel_hydrald) = m2cm*ddvel(its:ite,j,p_hydrald)
                  dvel(its:ite,1,j,p_dvel_alkooh) = m2cm*ddvel(its:ite,j,p_alkooh)
                  dvel(its:ite,1,j,p_dvel_mekooh) = m2cm*ddvel(its:ite,j,p_mekooh)
                  dvel(its:ite,1,j,p_dvel_tolooh) = m2cm*ddvel(its:ite,j,p_tolooh)
                  dvel(its:ite,1,j,p_dvel_xooh) = m2cm*ddvel(its:ite,j,p_xooh)
                  dvel(its:ite,1,j,p_dvel_so2) = m2cm*ddvel(its:ite,j,p_so2)
                  dvel(its:ite,1,j,p_dvel_so4) = m2cm*ddvel(its:ite,j,p_sulf)
                  dvel(its:ite,1,j,p_dvel_pan) = m2cm*ddvel(its:ite,j,p_pan)
                  dvel(its:ite,1,j,p_dvel_terpooh) = m2cm*ddvel(its:ite,j,p_terpooh)
               enddo
          endif

          if ( config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then
               do j = jts,jte
                   dvel(its:ite,1,j,p_dvel_cvasoaX) = 0.0
                   dvel(its:ite,1,j,p_dvel_cvasoa1) = m2cm*ddvel(its:ite,j,p_cvasoa1)
                   dvel(its:ite,1,j,p_dvel_cvasoa2) = m2cm*ddvel(its:ite,j,p_cvasoa2)
                   dvel(its:ite,1,j,p_dvel_cvasoa3) = m2cm*ddvel(its:ite,j,p_cvasoa3)
                   dvel(its:ite,1,j,p_dvel_cvasoa4) = m2cm*ddvel(its:ite,j,p_cvasoa4)
                   dvel(its:ite,1,j,p_dvel_cvbsoaX) = 0.0
                   dvel(its:ite,1,j,p_dvel_cvbsoa1) = m2cm*ddvel(its:ite,j,p_cvbsoa1)
                   dvel(its:ite,1,j,p_dvel_cvbsoa2) = m2cm*ddvel(its:ite,j,p_cvbsoa2)
                   dvel(its:ite,1,j,p_dvel_cvbsoa3) = m2cm*ddvel(its:ite,j,p_cvbsoa3)
                   dvel(its:ite,1,j,p_dvel_cvbsoa4) = m2cm*ddvel(its:ite,j,p_cvbsoa4)
                enddo
           endif

       ELSEIF ( config_flags%chem_opt == GOCART_SIMPLE ) then
          call wesely_driver(id,ktau,dtstep,                              &
               config_flags,current_month,                                &
               gmt,julday,t_phy,moist,p8w,t8w,raincv,                     &
               p_phy,chem,rho_phy,dz8w,ddvel,aer_res_def,aer_res_zcen,    & 
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               snowh, numgas,                                             &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!
! this does aerosol species (dust,seas, bc,oc) for gocart only
!
         call gocart_drydep_driver(dtstep,                                &
               config_flags,numgas,                                       &
               t_phy,moist,p8w,t8w,rmol,aer_res_def,                      &
               p_phy,chem,rho_phy,dz8w,ddvel,xland,hfx,                   &
               ivgtyp,tsk,vegfra,pbl,ust,znt,xlat,xlong,                  &
               dustdrydep_1,dustdrydep_2,dustdrydep_3,                    &
               dustdrydep_4,dustdrydep_5,                                 &
               depvelocity,                                               &                    
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
       ELSEIF ( config_flags%chem_opt == DUST) then
!
! this does aerosol species (dust) for gocart only
!
         call gocart_drydep_driver(dtstep,                                &
               config_flags,numgas,                                       &
               t_phy,moist,p8w,t8w,rmol,aer_res_def,                      &
               p_phy,chem,rho_phy,dz8w,ddvel,xland,hfx,                   &
               ivgtyp,tsk,vegfra,pbl,ust,znt,xlat,xlong,                  &
               dustdrydep_1,dustdrydep_2,dustdrydep_3,                    &
               dustdrydep_4,dustdrydep_5,                                 &
               depvelocity,                                               &                    
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )


       ELSEIF ( config_flags%chem_opt == GOCARTRACM_KPP) then
!
! this does aerosol species (dust,seas, bc,oc) for gocart only
!
          call wesely_driver(id,ktau,dtstep,                              &
               config_flags,current_month,                                &
               gmt,julday,t_phy,moist,p8w,t8w,raincv,                     &
               p_phy,chem,rho_phy,dz8w,ddvel,aer_res_def,aer_res_zcen,    &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               snowh, numgas,                                             &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
         call gocart_drydep_driver(dtstep,                                &
               config_flags,numgas,                                       &
               t_phy,moist,p8w,t8w,rmol,aer_res_def,                      &
               p_phy,chem,rho_phy,dz8w,ddvel,xland,hfx,                   &
               ivgtyp,tsk,vegfra,pbl,ust,znt,xlat,xlong,                  &
               dustdrydep_1,dustdrydep_2,dustdrydep_3,                    &
               dustdrydep_4,dustdrydep_5,                                 &
               depvelocity,                                               &                    
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )

       ELSE
          !Set dry deposition velocity to zero when using the
          !chemistry tracer mode.
          ddvel(:,:,:) = 0.
       END IF

! diagnose output dry deposition flux
       if( config_flags%diagnostic_dep == 1) then
       do i = its, ite
       do j = jts, jte
       ddflx(i, j,1:numgas)=ddflx(i,j,1:numgas)+chem(i,kts,j,1:numgas)*p_phy(i,kts,j)/(RSI*t_phy(i,kts,j))*ddvel(i,j,1:numgas)*dtstep*1.E-6
       enddo
       enddo
       end if

       if (config_flags%aer_aerodynres_opt == 2) then
          ! use aerodynamic resistance from center of layer kts to surface
          aer_res(:,:) = aer_res_zcen(:,:)
       else
          ! this is the default -- use aerodynamic resistance from 
          ! "default reference height" (currently 2 m) to surface
          aer_res(:,:) = aer_res_def(:,:)
       end if

       idrydep_onoff = 1
       aer_mech_id_select: SELECT CASE(config_flags%chem_opt)

!!! TUCCELLA
          CASE (RADM2SORG,RADM2SORG_AQ,RADM2SORG_AQCHEM,RADM2SORG_KPP, &
                RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP, &
                RACM_SOA_VBS_HET_KPP, &
                CBMZSORG,CBMZSORG_AQ, &
                CB05_SORG_AQ_KPP,CB05_SORG_VBS_AQ_KPP)
             aer_mech_id = 1
          CASE (RACMSORG_AQ,RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP,RACMSORG_KPP)
             aer_mech_id = 2
          CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, &
                 CBMZ_MOSAIC_8BIN_AQ,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
                 CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
                 MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP, &
                 CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP, &!BSINGH(12/03/2013): Added SAPRC 8 bin
                 SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(04/04/2014): Added SAPRC 8 bin non-aq
             aer_mech_id = 3
          CASE ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
             aer_mech_id = 4
          CASE DEFAULT
             aer_mech_id = 0
       END SELECT aer_mech_id_select

!  config_flags%aer_drydep_opt <= 0   -- aerosol depositon velocities are set to zero
!  config_flags%aer_drydep_opt >= 100 -- aerosol depvels are calculated in subr aer_depvel_driver
!                                        (see module_aer_drydep.F for details)
!  config_flags%aer_drydep_opt == 1   -- SORGAM chem packages use subr vdvg_2 of module_aerosols_sorgam.F
!                                        MOSAIC chem packages subr aerosol_depvel_2 of module_mosaic_drydep.F
!  config_flags%aer_drydep_opt == 11  -- SORGAM chem packages use subr vdvg of module_aerosols_sorgam.F

       if ((config_flags%aer_drydep_opt <= 0) .or. (aer_mech_id <= 0)) then
          CALL wrf_debug(15,'AEROSOL DRY DEP VELOCITIES  = 0.0')

       else if (config_flags%aer_drydep_opt <= 99) then

   adrydep_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2SORG,RADM2SORG_AQ,RADM2SORG_AQCHEM,RADM2SORG_KPP,RACM_ESRLSORG_KPP,CBMZSORG,CBMZSORG_AQ)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RADM')
       call sorgam_depdriver (id,config_flags,ktau,dtstep,              &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               numgas,ddflx(:,:,numgas+1:num_chem), &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE (RACMSORG_AQ,RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP,RACMSORG_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RACM')
       call sorgam_depdriver (id,config_flags,ktau,dtstep,              &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               numgas,ddflx(:,:,numgas+1:num_chem), &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE (CB05_SORG_AQ_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/CB05')
       call sorgam_depdriver (id,config_flags,ktau,dtstep,              &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               numgas,ddflx(:,:,numgas+1:num_chem), &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE (CB05_SORG_VBS_AQ_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/CB05')
       call sorgam_vbs_depdriver (id,config_flags,ktau,dtstep,          &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,rh,z,z_at_w,                 &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               numgas,ddflx(:,:,numgas+1:num_chem), &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )

     CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
          CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
          SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
          MOZART_MOSAIC_4BIN_KPP, MOZART_MOSAIC_4BIN_AQ_KPP, &
          CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, &
          SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin and non-aq on 04/04/2014
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR MOSAIC AEROSOLS')
       call mosaic_drydep_driver(                                       &
               id, curr_secs, ktau, dtstep, config_flags,               &
               gmt, julday,                                             &
               t_phy, rho_phy, p_phy,                                   &
               ust, aer_res,                                            &
               moist, chem, ddvel,                                      &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
!!! TUCCELLA
     CASE ( RACM_SOA_VBS_KPP,RACM_SOA_VBS_AQCHEM_KPP,RACM_SOA_VBS_HET_KPP )
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR SOA_VBS AEROSOLS')
       call soa_vbs_depdriver (id,config_flags,ktau,dtstep,             &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,rh,z,z_at_w,                 &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
    CASE (CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR CAM_MAM AEROSOLS')
       call cam_mam_drydep_driver(                                      &
               id, curr_secs, ktau, dtstep, config_flags,               &
               gmt, julday,                                             &
               t_phy, rho_phy, p_phy,                                   &
               ust, aer_res,                                            &
               moist, chem, ddvel,                                      &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
!
     CASE DEFAULT
                                                     
     END SELECT adrydep_select
   else ! (config_flags%aer_drydep_opt > 99)
              CALL wrf_debug(15,'DOING DRY DEP VELOCITIES THRU AER_DRYDEP_DRIVER')
              call aer_drydep_driver(                                          &
                      id, ktau, dtstep, config_flags, aer_mech_id,           &
                      gmt, julday,                                             &
                      t_phy, rho_phy, p_phy,                                   &
                      alt, p8w, t8w, dz8w, z, z_at_w,                          &
                      ust, aer_res, ivgtyp, vegfra, pbl, rmol, znt,            &
                      moist, chem, ddvel,                                      &
                      h2oai, h2oaj, numgas,                                    &
                      ids,ide, jds,jde, kds,kde,                               &
                      ims,ime, jms,jme, kms,kme,                               &
                      its,ite, jts,jte, kts,kte                                )
   end if
       if (config_flags%aer_drydep_opt > 0) then
          if ((aer_mech_id > 0) .and. (aer_mech_id <= 4)) then
             ! limit aerosol ddvels to <= 0.5 m/s
             ! drydep routines occasionally produce unrealistically-large particle
             !    diameter leading to unrealistically-large sedimentation velocity
             ddvel(:,:,numgas+1:num_chem) = min( 0.50, ddvel(:,:,numgas+1:num_chem) )
          end if
       end if

       !For MAM aersols dry deposition
       if ((aer_mech_id == 4)) then
          do m_mam = 1, num_chem
             !Look for species with _a or _c and adjust these species for dry deposition
             k_a   = index(trim(adjustl(chem_dname_table(1,m_mam))),'_a') !To include *_a* species
             k_c   = index(trim(adjustl(chem_dname_table(1,m_mam))),'_c') !To include *_c* species
             kmax = max(k_a, k_c)
             if(kmax > 0 ) then !kmax>0 means a species has _a or _c
                frac_removed(its:ite,jts:jte) = max( 0.0, min( 1.0, ddvel(its:ite,jts:jte,m_mam)*dtstep/dz8w(its:ite,1,jts:jte) ) )
                chem(its:ite,1,jts:jte,m_mam) = chem(its:ite,1,jts:jte,m_mam)*(1.0 - frac_removed(its:ite,jts:jte)) !update lowest layer
             endif
          enddo
       endif
       
    CASE DEFAULT 
                                                     
   END SELECT drydep_select                              

! Add dep_vel here for all aerosol and gas species in the dvel array 
      ll = max( 1, min( config_flags%ndepvel, num_vert_mix ) )
      dep_vel(:,:,:,:) = 0.
      do l=1,ll
      do j=jts,jte
      do k=1,config_flags%kdepvel
      do i=its,ite
        dep_vel(i,k,j,l) = ddvel(i,j,l)
      enddo
      enddo
      enddo
      enddo

!   This will be called later from subgrd_transport_driver.F !!!!!!!!
!
!
      if( config_flags%diagnostic_dep == 1) then
       ddlen(its:ite,jts:jte,:)=ddlen(its:ite,jts:jte,:)+ddvel(its:ite,jts:jte,:)*m2cm
      end if

      dep_vel_o3=0.
      if (num_vert_mix == 0) then
      do 100 j=jts,jte
      do 100 i=its,ite
      pblst=0.
      ddmassn(:) = 0.0
!
!
!-- start with vertical mixing
!
      do k=kts,kte+1
         zzfull(k)=z_at_w(i,k,j)-z_at_w(i,kts,j)
      enddo
      do k=kts,kte
         ekmfull(k)=max(1.e-6,exch_h(i,k,j))
      enddo
      ekmfull(kts)=0.
      ekmfull(kte+1)=0.

!!$! UNCOMMENT THIS AND FINE TUNE LEVELS TO YOUR DOMAIN IF YOU WANT TO
!!$! FORCE MIXING ESPECIALLY OVER URBAN AREAS TO A CERTAIN DEPTH:
!!$!
!!$! --- Mix the emissions up several layers in urban areas if no urban surface physics
!!$!     if e_co > 0., the grid cell should not be over water
!!$!     if e_co > 200, the grid cell should be over a large urban region
!!$!

! this code is wrong - doesn't work if e_co is == param_first_scalar
! (like it happened to be the case for MOZCART)
!     if (p_e_co > param_first_scalar )then
     if (p_e_co >= param_first_scalar )then
       if (sf_urban_physics .eq. 0 ) then
         if (emis_ant(i,kts,j,p_e_co) .gt. 0) then
          ekmfull(kts:kts+10) = max(ekmfull(kts:kts+10),1.)
         endif
         if (emis_ant(i,kts,j,p_e_co) .gt. 200) then
          ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
         endif
         if (p_e_pm25i > param_first_scalar )then
          if (emis_ant(i,kts,j,p_e_pm25i)+ emis_ant(i,kts,j,p_e_pm25j) .GT. 8.19e-4*200) then
           ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
          endif
         endif
         if (p_e_pm_25 > param_first_scalar )then
          if (emis_ant(i,kts,j,p_e_pm_25) .GT. 8.19e-4*200) then
           ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
          endif
         endif
       endif
     endif
!!$! --- Mix the emissions up several layers when satellite data shows a wildfire
!!$!     if ebu_in_e_co > 0., a wildfire exists so increase vertical mixing
!     if (p_ebu_in_co > param_first_scalar )then
     if (p_ebu_in_co >= param_first_scalar )then
         if (ebu_in(i,1,j,p_ebu_in_co) .gt. 0) then
          ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
         endif
     endif

     do k=kts,kte
        zz(k)=z(i,k,j)-z_at_w(i,kts,j)
     enddo
!
!   vertical mixing routine (including deposition)
!   need to be careful here with that dumm tracer in spot 1
!   do not need lho,lho2
!   (03-may-2006 rce - calc dryrho_1d and pass it to vertmx)
!
      dep_vel_o3(i,j)=ddvel(i,j,p_o3)
      do nv=2,num_chem-0
         if(is_CAMMGMP_used .and. .not.vertMixAero(nv))cycle !Balwinder.Singh@pnnl.gov: Do mix constituents which are already mixed by CAMMGMP microphysics
         do k=kts,kte
            pblst(k)=max(epsilc,chem(i,k,j,nv))
            dryrho_1d(k) = 1./alt(i,k,j)
         enddo

         mix_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, CBMZ_MOSAIC_4BIN_AQ, &
              CBMZ_MOSAIC_8BIN_AQ, CBMZSORG_AQ, CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ,  &
              CBMZ_MOSAIC_DMS_8BIN_AQ, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP,     &
              MOZART_MOSAIC_4BIN_AQ_KPP, RACM_SOA_VBS_AQCHEM_KPP,                          &
              SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,                                             &
              CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
            if(.not.is_aerosol(nv))then ! mix gases not aerosol
               call vertmx(dtstep,pblst,ekmfull,dryrho_1d, &
                           zzfull,zz,ddvel(i,j,nv),kts,kte)

            endif
!The default case below does turbulent mixing for all gas and aerosol species
!If aqueous phase is not activated
!It requires ddvel array as a boundary condition near surface for flux of species 
!The top boundary condition for eddy diffusivity is zero
         CASE DEFAULT
            call vertmx(dtstep,pblst,ekmfull,dryrho_1d, &
                        zzfull,zz,ddvel(i,j,nv),kts,kte)

         END SELECT mix_select

         ! chem is in ppmv
         ! dry deposition is combined with vertical mixing, but column independent.
         ! Hence, all molecules lost per column must be dry deposited.

         ! old and new column totals (mol/m2 or ug/m2)
         old = 0.0
         new = 0.0

         do k=kts,kte-1
           fac = 1.0
           if (nv <= numgas) then
             ! from ppmv to mol/m2
             ! fac     = 1e-6 * rho * 1/mw_air * dz
             !                 kg/m3   mol/kg    m
             fac = 1e-6 * dryrho_1d(k) * 1./(mwdry*1.e-3) * dz8w(i,k,j)
           else
             ! from ug/kg to ug/m2
             ! fac     = rho * dz
             !          kg/m3  m
             fac = dryrho_1d(k) * dz8w(i,k,j)
           endif

           old = old + max(epsilc,chem(i,k,j,nv)) * fac
           new = new + max(epsilc,pblst(k)) * fac
         enddo

         ! we ignore (spurious) and add new dry deposition to
         ! existing field (accumulated deposition!)
         ddmassn(nv) =  max( 0.0, (old - new) )

         do k=kts,kte-1
            chem(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
      enddo

      if( config_flags%diagnostic_chem == DEPVEL1 .and. &
          (config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP) ) then
        dvel(i,1,j,p_ddmass_o3) = dvel(i,1,j,p_ddmass_o3) + ddmassn(p_o3)
        dvel(i,1,j,p_ddmass_no) = dvel(i,1,j,p_ddmass_no) + ddmassn(p_no)
        dvel(i,1,j,p_ddmass_no2) = dvel(i,1,j,p_ddmass_no2) + ddmassn(p_no2)
        dvel(i,1,j,p_ddmass_nh3) = dvel(i,1,j,p_ddmass_nh3) + ddmassn(p_nh3)
        dvel(i,1,j,p_ddmass_hno3) = dvel(i,1,j,p_ddmass_hno3) + ddmassn(p_hno3)
        dvel(i,1,j,p_ddmass_so2) = dvel(i,1,j,p_ddmass_so2) + ddmassn(p_so2)
        dvel(i,1,j,p_ddmass_so4) = dvel(i,1,j,p_ddmass_so4) + ddmassn(p_sulf)
        dvel(i,1,j,p_ddmass_so4aj) = dvel(i,1,j,p_ddmass_so4aj) + ddmassn(p_so4aj)
        dvel(i,1,j,p_ddmass_so4ai) = dvel(i,1,j,p_ddmass_so4ai) + ddmassn(p_so4ai)
        dvel(i,1,j,p_ddmass_no3aj) = dvel(i,1,j,p_ddmass_no3aj) + ddmassn(p_no3aj)
        dvel(i,1,j,p_ddmass_no3ai) = dvel(i,1,j,p_ddmass_no3ai) + ddmassn(p_no3ai)
        dvel(i,1,j,p_ddmass_nh4aj) = dvel(i,1,j,p_ddmass_nh4aj) + ddmassn(p_nh4aj)
        dvel(i,1,j,p_ddmass_nh4ai) = dvel(i,1,j,p_ddmass_nh4ai) + ddmassn(p_nh4ai)
      endif

      if( config_flags%diagnostic_chem == DEPVEL1 .and. &
          (config_flags%chem_opt == MOZCART_KPP .or. &
           config_flags%chem_opt == T1_MOZCART_KPP .or. &
           config_flags%chem_opt == MOZART_KPP   .or. &
           config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
           config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) ) then

        dvel(i,1,j,p_ddmass_o3) = dvel(i,1,j,p_ddmass_o3) + ddmassn(p_o3)
        dvel(i,1,j,p_ddmass_no) = dvel(i,1,j,p_ddmass_no) + ddmassn(p_no)
        dvel(i,1,j,p_ddmass_no2) = dvel(i,1,j,p_ddmass_no2) + ddmassn(p_no2)
        dvel(i,1,j,p_ddmass_nh3) = dvel(i,1,j,p_ddmass_nh3) + ddmassn(p_nh3)
        dvel(i,1,j,p_ddmass_hno3) = dvel(i,1,j,p_ddmass_hno3) + ddmassn(p_hno3)
        dvel(i,1,j,p_ddmass_hno4) = dvel(i,1,j,p_ddmass_hno4) + ddmassn(p_hno4)
        dvel(i,1,j,p_ddmass_h2o2) = dvel(i,1,j,p_ddmass_h2o2) + ddmassn(p_h2o2)
        dvel(i,1,j,p_ddmass_co) = dvel(i,1,j,p_ddmass_co) + ddmassn(p_co)
        dvel(i,1,j,p_ddmass_ch3ooh) = dvel(i,1,j,p_ddmass_ch3ooh) + ddmassn(p_ch3ooh)
        dvel(i,1,j,p_ddmass_hcho) = dvel(i,1,j,p_ddmass_hcho) + ddmassn(p_hcho)
        dvel(i,1,j,p_ddmass_ch3oh) = dvel(i,1,j,p_ddmass_ch3oh) + ddmassn(p_ch3oh)
        dvel(i,1,j,p_ddmass_eo2) = dvel(i,1,j,p_ddmass_eo2) + ddmassn(p_eo2)
        dvel(i,1,j,p_ddmass_ald) = dvel(i,1,j,p_ddmass_ald) + ddmassn(p_ald)
        dvel(i,1,j,p_ddmass_ch3cooh) = dvel(i,1,j,p_ddmass_ch3cooh) + ddmassn(p_ch3cooh)
        dvel(i,1,j,p_ddmass_acet) = dvel(i,1,j,p_ddmass_acet) + ddmassn(p_acet)
        dvel(i,1,j,p_ddmass_mgly) = dvel(i,1,j,p_ddmass_mgly) + ddmassn(p_mgly)
        dvel(i,1,j,p_ddmass_gly) = dvel(i,1,j,p_ddmass_gly) + ddmassn(p_gly)
        dvel(i,1,j,p_ddmass_paa) = dvel(i,1,j,p_ddmass_paa) + ddmassn(p_paa)
        dvel(i,1,j,p_ddmass_pooh) = dvel(i,1,j,p_ddmass_pooh) + ddmassn(p_c3h6ooh)
        dvel(i,1,j,p_ddmass_mpan) = dvel(i,1,j,p_ddmass_mpan) + ddmassn(p_mpan)
        dvel(i,1,j,p_ddmass_mco3) = dvel(i,1,j,p_ddmass_mco3) + ddmassn(p_mco3)
        dvel(i,1,j,p_ddmass_mvkooh) = dvel(i,1,j,p_ddmass_mvkooh) + ddmassn(p_mvkooh)
        dvel(i,1,j,p_ddmass_c2h5oh) = dvel(i,1,j,p_ddmass_c2h5oh) + ddmassn(p_c2h5oh)
        dvel(i,1,j,p_ddmass_etooh) = dvel(i,1,j,p_ddmass_etooh) + ddmassn(p_etooh)
        dvel(i,1,j,p_ddmass_prooh) = dvel(i,1,j,p_ddmass_prooh) + ddmassn(p_prooh)
        dvel(i,1,j,p_ddmass_acetp) = dvel(i,1,j,p_ddmass_acetp) + ddmassn(p_acetp)
        dvel(i,1,j,p_ddmass_onit) = dvel(i,1,j,p_ddmass_onit) + ddmassn(p_onit)
        dvel(i,1,j,p_ddmass_onitr) = dvel(i,1,j,p_ddmass_onitr) + ddmassn(p_onitr)
        dvel(i,1,j,p_ddmass_isooh) = dvel(i,1,j,p_ddmass_isooh) + ddmassn(p_isooh)
        dvel(i,1,j,p_ddmass_acetol) = dvel(i,1,j,p_ddmass_acetol) + ddmassn(p_acetol)
        dvel(i,1,j,p_ddmass_glyald) = dvel(i,1,j,p_ddmass_glyald) + ddmassn(p_glyald)
        dvel(i,1,j,p_ddmass_hydrald) = dvel(i,1,j,p_ddmass_hydrald) + ddmassn(p_hydrald)
        dvel(i,1,j,p_ddmass_alkooh) = dvel(i,1,j,p_ddmass_alkooh) + ddmassn(p_alkooh)
        dvel(i,1,j,p_ddmass_mekooh) = dvel(i,1,j,p_ddmass_mekooh) + ddmassn(p_mekooh)
        dvel(i,1,j,p_ddmass_tolooh) = dvel(i,1,j,p_ddmass_tolooh) + ddmassn(p_tolooh)
        dvel(i,1,j,p_ddmass_xooh) = dvel(i,1,j,p_ddmass_xooh) + ddmassn(p_xooh)
        dvel(i,1,j,p_ddmass_so2) = dvel(i,1,j,p_ddmass_so2) + ddmassn(p_so2)
        dvel(i,1,j,p_ddmass_so4) = dvel(i,1,j,p_ddmass_so4) + ddmassn(p_sulf)
        dvel(i,1,j,p_ddmass_pan) = dvel(i,1,j,p_ddmass_pan) + ddmassn(p_pan)
        dvel(i,1,j,p_ddmass_terpooh) = dvel(i,1,j,p_ddmass_terpooh) + ddmassn(p_terpooh)

        if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then
          dvel(i,1,j,p_ddmass_cvasoaX) = dvel(i,1,j,p_ddmass_cvasoaX) + ddmassn(p_cvasoaX)
          dvel(i,1,j,p_ddmass_cvasoa1) = dvel(i,1,j,p_ddmass_cvasoa1) + ddmassn(p_cvasoa1)
          dvel(i,1,j,p_ddmass_cvasoa2) = dvel(i,1,j,p_ddmass_cvasoa2) + ddmassn(p_cvasoa2)
          dvel(i,1,j,p_ddmass_cvasoa3) = dvel(i,1,j,p_ddmass_cvasoa3) + ddmassn(p_cvasoa3)
          dvel(i,1,j,p_ddmass_cvasoa4) = dvel(i,1,j,p_ddmass_cvasoa4) + ddmassn(p_cvasoa4)
          dvel(i,1,j,p_ddmass_cvbsoaX) = dvel(i,1,j,p_ddmass_cvbsoaX) + ddmassn(p_cvbsoaX)
          dvel(i,1,j,p_ddmass_cvbsoa1) = dvel(i,1,j,p_ddmass_cvbsoa1) + ddmassn(p_cvbsoa1)
          dvel(i,1,j,p_ddmass_cvbsoa2) = dvel(i,1,j,p_ddmass_cvbsoa2) + ddmassn(p_cvbsoa2)
          dvel(i,1,j,p_ddmass_cvbsoa3) = dvel(i,1,j,p_ddmass_cvbsoa3) + ddmassn(p_cvbsoa3)
          dvel(i,1,j,p_ddmass_cvbsoa4) = dvel(i,1,j,p_ddmass_cvbsoa4) + ddmassn(p_cvbsoa4)
        endif

        if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .or. &
            config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then

          dvel(i,1,j,p_ddmass_so4_a01) = dvel(i,1,j,p_ddmass_so4_a01) + ddmassn(p_so4_a01)
          dvel(i,1,j,p_ddmass_no3_a01) = dvel(i,1,j,p_ddmass_no3_a01) + ddmassn(p_no3_a01)
          dvel(i,1,j,p_ddmass_cl_a01) = dvel(i,1,j,p_ddmass_cl_a01) + ddmassn(p_cl_a01)
          dvel(i,1,j,p_ddmass_nh4_a01) = dvel(i,1,j,p_ddmass_nh4_a01) + ddmassn(p_nh4_a01)
          dvel(i,1,j,p_ddmass_na_a01) = dvel(i,1,j,p_ddmass_na_a01) + ddmassn(p_na_a01)
          dvel(i,1,j,p_ddmass_oin_a01) = dvel(i,1,j,p_ddmass_oin_a01) + ddmassn(p_oin_a01)
          dvel(i,1,j,p_ddmass_oc_a01) = dvel(i,1,j,p_ddmass_oc_a01) + ddmassn(p_oc_a01)
          dvel(i,1,j,p_ddmass_bc_a01) = dvel(i,1,j,p_ddmass_bc_a01) + ddmassn(p_bc_a01)
          dvel(i,1,j,p_ddmass_so4_a02) = dvel(i,1,j,p_ddmass_so4_a02) + ddmassn(p_so4_a02)
          dvel(i,1,j,p_ddmass_no3_a02) = dvel(i,1,j,p_ddmass_no3_a02) + ddmassn(p_no3_a02)
          dvel(i,1,j,p_ddmass_cl_a02) = dvel(i,1,j,p_ddmass_cl_a02) + ddmassn(p_cl_a02)
          dvel(i,1,j,p_ddmass_nh4_a02) = dvel(i,1,j,p_ddmass_nh4_a02) + ddmassn(p_nh4_a02)
          dvel(i,1,j,p_ddmass_na_a02) = dvel(i,1,j,p_ddmass_na_a02) + ddmassn(p_na_a02)
          dvel(i,1,j,p_ddmass_oin_a02) = dvel(i,1,j,p_ddmass_oin_a02) + ddmassn(p_oin_a02)
          dvel(i,1,j,p_ddmass_oc_a02) = dvel(i,1,j,p_ddmass_oc_a02) + ddmassn(p_oc_a02)
          dvel(i,1,j,p_ddmass_bc_a02) = dvel(i,1,j,p_ddmass_bc_a02) + ddmassn(p_bc_a02)
          dvel(i,1,j,p_ddmass_so4_a03) = dvel(i,1,j,p_ddmass_so4_a03) + ddmassn(p_so4_a03)
          dvel(i,1,j,p_ddmass_no3_a03) = dvel(i,1,j,p_ddmass_no3_a03) + ddmassn(p_no3_a03)
          dvel(i,1,j,p_ddmass_cl_a03) = dvel(i,1,j,p_ddmass_cl_a03) + ddmassn(p_cl_a03)
          dvel(i,1,j,p_ddmass_nh4_a03) = dvel(i,1,j,p_ddmass_nh4_a03) + ddmassn(p_nh4_a03)
          dvel(i,1,j,p_ddmass_na_a03) = dvel(i,1,j,p_ddmass_na_a03) + ddmassn(p_na_a03)
          dvel(i,1,j,p_ddmass_oin_a03) = dvel(i,1,j,p_ddmass_oin_a03) + ddmassn(p_oin_a03)
          dvel(i,1,j,p_ddmass_oc_a03) = dvel(i,1,j,p_ddmass_oc_a03) + ddmassn(p_oc_a03)
          dvel(i,1,j,p_ddmass_bc_a03) = dvel(i,1,j,p_ddmass_bc_a03) + ddmassn(p_bc_a03)
          dvel(i,1,j,p_ddmass_so4_a04) = dvel(i,1,j,p_ddmass_so4_a04) + ddmassn(p_so4_a04)
          dvel(i,1,j,p_ddmass_no3_a04) = dvel(i,1,j,p_ddmass_no3_a04) + ddmassn(p_no3_a04)
          dvel(i,1,j,p_ddmass_cl_a04) = dvel(i,1,j,p_ddmass_cl_a04) + ddmassn(p_cl_a04)
          dvel(i,1,j,p_ddmass_nh4_a04) = dvel(i,1,j,p_ddmass_nh4_a04) + ddmassn(p_nh4_a04)
          dvel(i,1,j,p_ddmass_na_a04) = dvel(i,1,j,p_ddmass_na_a04) + ddmassn(p_na_a04)
          dvel(i,1,j,p_ddmass_oin_a04) = dvel(i,1,j,p_ddmass_oin_a04) + ddmassn(p_oin_a04)
          dvel(i,1,j,p_ddmass_oc_a04) = dvel(i,1,j,p_ddmass_oc_a04) + ddmassn(p_oc_a04)
          dvel(i,1,j,p_ddmass_bc_a04) = dvel(i,1,j,p_ddmass_bc_a04) + ddmassn(p_bc_a04)

          dvel(i,1,j,p_ddmass_ca_a01) = dvel(i,1,j,p_ddmass_ca_a01) + ddmassn(p_ca_a01)
          dvel(i,1,j,p_ddmass_ca_a02) = dvel(i,1,j,p_ddmass_ca_a02) + ddmassn(p_ca_a02)
          dvel(i,1,j,p_ddmass_ca_a03) = dvel(i,1,j,p_ddmass_ca_a03) + ddmassn(p_ca_a03)
          dvel(i,1,j,p_ddmass_ca_a04) = dvel(i,1,j,p_ddmass_ca_a04) + ddmassn(p_ca_a04)

          dvel(i,1,j,p_ddmass_co3_a01) = dvel(i,1,j,p_ddmass_co3_a01) + ddmassn(p_co3_a01)
          dvel(i,1,j,p_ddmass_co3_a02) = dvel(i,1,j,p_ddmass_co3_a02) + ddmassn(p_co3_a02)
          dvel(i,1,j,p_ddmass_co3_a03) = dvel(i,1,j,p_ddmass_co3_a03) + ddmassn(p_co3_a03)
          dvel(i,1,j,p_ddmass_co3_a04) = dvel(i,1,j,p_ddmass_co3_a04) + ddmassn(p_co3_a04)

          if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP .OR. &
              config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then
            dvel(i,1,j,p_ddmass_glysoa_a01) = dvel(i,1,j,p_ddmass_glysoa_a01) + &
                                              ddmassn(p_glysoa_r1_a01) + &
                                              ddmassn(p_glysoa_r2_a01) + &
                                              ddmassn(p_glysoa_oh_a01) + &
                                              ddmassn(p_glysoa_sfc_a01) + &
                                              ddmassn(p_glysoa_nh4_a01)

            dvel(i,1,j,p_ddmass_glysoa_a02) = dvel(i,1,j,p_ddmass_glysoa_a02) + &
                                              ddmassn(p_glysoa_r1_a02) + &
                                              ddmassn(p_glysoa_r2_a02) + &
                                              ddmassn(p_glysoa_oh_a02) + &
                                              ddmassn(p_glysoa_sfc_a02) + &
                                              ddmassn(p_glysoa_nh4_a02)
                                              
            dvel(i,1,j,p_ddmass_glysoa_a03) = dvel(i,1,j,p_ddmass_glysoa_a03) + &
                                              ddmassn(p_glysoa_r1_a03) + &
                                              ddmassn(p_glysoa_r2_a03) + &
                                              ddmassn(p_glysoa_oh_a03) + &
                                              ddmassn(p_glysoa_sfc_a03) + &
                                              ddmassn(p_glysoa_nh4_a03)
                                              
            dvel(i,1,j,p_ddmass_glysoa_a04) = dvel(i,1,j,p_ddmass_glysoa_a04) + &
                                              ddmassn(p_glysoa_r1_a04) + &
                                              ddmassn(p_glysoa_r2_a04) + &
                                              ddmassn(p_glysoa_oh_a04) + &
                                              ddmassn(p_glysoa_sfc_a04) + &
                                              ddmassn(p_glysoa_nh4_a04)
          endif

          if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_KPP) then

            dvel(i,1,j,p_ddmass_smpa_a01) = dvel(i,1,j,p_ddmass_smpa_a01) + ddmassn(p_smpa_a01)
            dvel(i,1,j,p_ddmass_smpbb_a01) = dvel(i,1,j,p_ddmass_smpbb_a01) + ddmassn(p_smpbb_a01)
            dvel(i,1,j,p_ddmass_biog1_c_a01) = dvel(i,1,j,p_ddmass_biog1_c_a01) + ddmassn(p_biog1_c_a01)
            dvel(i,1,j,p_ddmass_biog1_o_a01) = dvel(i,1,j,p_ddmass_biog1_o_a01) + ddmassn(p_biog1_o_a01)

            dvel(i,1,j,p_ddmass_smpa_a02) = dvel(i,1,j,p_ddmass_smpa_a02) + ddmassn(p_smpa_a02)
            dvel(i,1,j,p_ddmass_smpbb_a02) = dvel(i,1,j,p_ddmass_smpbb_a02) + ddmassn(p_smpbb_a02)
            dvel(i,1,j,p_ddmass_biog1_c_a02) = dvel(i,1,j,p_ddmass_biog1_c_a02) + ddmassn(p_biog1_c_a02)
            dvel(i,1,j,p_ddmass_biog1_o_a02) = dvel(i,1,j,p_ddmass_biog1_o_a02) + ddmassn(p_biog1_o_a02)

            dvel(i,1,j,p_ddmass_smpa_a03) = dvel(i,1,j,p_ddmass_smpa_a03) + ddmassn(p_smpa_a03)
            dvel(i,1,j,p_ddmass_smpbb_a03) = dvel(i,1,j,p_ddmass_smpbb_a03) + ddmassn(p_smpbb_a03)
            dvel(i,1,j,p_ddmass_biog1_c_a03) = dvel(i,1,j,p_ddmass_biog1_c_a03) + ddmassn(p_biog1_c_a03)
            dvel(i,1,j,p_ddmass_biog1_o_a03) = dvel(i,1,j,p_ddmass_biog1_o_a03) + ddmassn(p_biog1_o_a03)

            dvel(i,1,j,p_ddmass_smpa_a04) = dvel(i,1,j,p_ddmass_smpa_a04) + ddmassn(p_smpa_a04)
            dvel(i,1,j,p_ddmass_smpbb_a04) = dvel(i,1,j,p_ddmass_smpbb_a04) + ddmassn(p_smpbb_a04)
            dvel(i,1,j,p_ddmass_biog1_c_a04) = dvel(i,1,j,p_ddmass_biog1_c_a04) + ddmassn(p_biog1_c_a04)
            dvel(i,1,j,p_ddmass_biog1_o_a04) = dvel(i,1,j,p_ddmass_biog1_o_a04) + ddmassn(p_biog1_o_a04)

          endif

          if (config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP) then

            dvel(i,1,j,p_ddmass_asoaX_a01) = dvel(i,1,j,p_ddmass_asoaX_a01) + ddmassn(p_asoaX_a01)
            dvel(i,1,j,p_ddmass_asoa1_a01) = dvel(i,1,j,p_ddmass_asoa1_a01) + ddmassn(p_asoa1_a01)
            dvel(i,1,j,p_ddmass_asoa2_a01) = dvel(i,1,j,p_ddmass_asoa2_a01) + ddmassn(p_asoa2_a01)
            dvel(i,1,j,p_ddmass_asoa3_a01) = dvel(i,1,j,p_ddmass_asoa3_a01) + ddmassn(p_asoa3_a01)
            dvel(i,1,j,p_ddmass_asoa4_a01) = dvel(i,1,j,p_ddmass_asoa4_a01) + ddmassn(p_asoa4_a01)
            dvel(i,1,j,p_ddmass_bsoaX_a01) = dvel(i,1,j,p_ddmass_bsoaX_a01) + ddmassn(p_bsoaX_a01)
            dvel(i,1,j,p_ddmass_bsoa1_a01) = dvel(i,1,j,p_ddmass_bsoa1_a01) + ddmassn(p_bsoa1_a01)
            dvel(i,1,j,p_ddmass_bsoa2_a01) = dvel(i,1,j,p_ddmass_bsoa2_a01) + ddmassn(p_bsoa2_a01)
            dvel(i,1,j,p_ddmass_bsoa3_a01) = dvel(i,1,j,p_ddmass_bsoa3_a01) + ddmassn(p_bsoa3_a01)
            dvel(i,1,j,p_ddmass_bsoa4_a01) = dvel(i,1,j,p_ddmass_bsoa4_a01) + ddmassn(p_bsoa4_a01)

            dvel(i,1,j,p_ddmass_asoaX_a02) = dvel(i,1,j,p_ddmass_asoaX_a02) + ddmassn(p_asoaX_a02)
            dvel(i,1,j,p_ddmass_asoa1_a02) = dvel(i,1,j,p_ddmass_asoa1_a02) + ddmassn(p_asoa1_a02)
            dvel(i,1,j,p_ddmass_asoa2_a02) = dvel(i,1,j,p_ddmass_asoa2_a02) + ddmassn(p_asoa2_a02)
            dvel(i,1,j,p_ddmass_asoa3_a02) = dvel(i,1,j,p_ddmass_asoa3_a02) + ddmassn(p_asoa3_a02)
            dvel(i,1,j,p_ddmass_asoa4_a02) = dvel(i,1,j,p_ddmass_asoa4_a02) + ddmassn(p_asoa4_a02)
            dvel(i,1,j,p_ddmass_bsoaX_a02) = dvel(i,1,j,p_ddmass_bsoaX_a02) + ddmassn(p_bsoaX_a02)
            dvel(i,1,j,p_ddmass_bsoa1_a02) = dvel(i,1,j,p_ddmass_bsoa1_a02) + ddmassn(p_bsoa1_a02)
            dvel(i,1,j,p_ddmass_bsoa2_a02) = dvel(i,1,j,p_ddmass_bsoa2_a02) + ddmassn(p_bsoa2_a02)
            dvel(i,1,j,p_ddmass_bsoa3_a02) = dvel(i,1,j,p_ddmass_bsoa3_a02) + ddmassn(p_bsoa3_a02)
            dvel(i,1,j,p_ddmass_bsoa4_a02) = dvel(i,1,j,p_ddmass_bsoa4_a02) + ddmassn(p_bsoa4_a02)

            dvel(i,1,j,p_ddmass_asoaX_a03) = dvel(i,1,j,p_ddmass_asoaX_a03) + ddmassn(p_asoaX_a03)
            dvel(i,1,j,p_ddmass_asoa1_a03) = dvel(i,1,j,p_ddmass_asoa1_a03) + ddmassn(p_asoa1_a03)
            dvel(i,1,j,p_ddmass_asoa2_a03) = dvel(i,1,j,p_ddmass_asoa2_a03) + ddmassn(p_asoa2_a03)
            dvel(i,1,j,p_ddmass_asoa3_a03) = dvel(i,1,j,p_ddmass_asoa3_a03) + ddmassn(p_asoa3_a03)
            dvel(i,1,j,p_ddmass_asoa4_a03) = dvel(i,1,j,p_ddmass_asoa4_a03) + ddmassn(p_asoa4_a03)
            dvel(i,1,j,p_ddmass_bsoaX_a03) = dvel(i,1,j,p_ddmass_bsoaX_a03) + ddmassn(p_bsoaX_a03)
            dvel(i,1,j,p_ddmass_bsoa1_a03) = dvel(i,1,j,p_ddmass_bsoa1_a03) + ddmassn(p_bsoa1_a03)
            dvel(i,1,j,p_ddmass_bsoa2_a03) = dvel(i,1,j,p_ddmass_bsoa2_a03) + ddmassn(p_bsoa2_a03)
            dvel(i,1,j,p_ddmass_bsoa3_a03) = dvel(i,1,j,p_ddmass_bsoa3_a03) + ddmassn(p_bsoa3_a03)
            dvel(i,1,j,p_ddmass_bsoa4_a03) = dvel(i,1,j,p_ddmass_bsoa4_a03) + ddmassn(p_bsoa4_a03)

            dvel(i,1,j,p_ddmass_asoaX_a04) = dvel(i,1,j,p_ddmass_asoaX_a04) + ddmassn(p_asoaX_a04)
            dvel(i,1,j,p_ddmass_asoa1_a04) = dvel(i,1,j,p_ddmass_asoa1_a04) + ddmassn(p_asoa1_a04)
            dvel(i,1,j,p_ddmass_asoa2_a04) = dvel(i,1,j,p_ddmass_asoa2_a04) + ddmassn(p_asoa2_a04)
            dvel(i,1,j,p_ddmass_asoa3_a04) = dvel(i,1,j,p_ddmass_asoa3_a04) + ddmassn(p_asoa3_a04)
            dvel(i,1,j,p_ddmass_asoa4_a04) = dvel(i,1,j,p_ddmass_asoa4_a04) + ddmassn(p_asoa4_a04)
            dvel(i,1,j,p_ddmass_bsoaX_a04) = dvel(i,1,j,p_ddmass_bsoaX_a04) + ddmassn(p_bsoaX_a04)
            dvel(i,1,j,p_ddmass_bsoa1_a04) = dvel(i,1,j,p_ddmass_bsoa1_a04) + ddmassn(p_bsoa1_a04)
            dvel(i,1,j,p_ddmass_bsoa2_a04) = dvel(i,1,j,p_ddmass_bsoa2_a04) + ddmassn(p_bsoa2_a04)
            dvel(i,1,j,p_ddmass_bsoa3_a04) = dvel(i,1,j,p_ddmass_bsoa3_a04) + ddmassn(p_bsoa3_a04)
            dvel(i,1,j,p_ddmass_bsoa4_a04) = dvel(i,1,j,p_ddmass_bsoa4_a04) + ddmassn(p_bsoa4_a04)

          endif

        endif

      endif

       tracer_select: SELECT CASE(config_flags%tracer_opt)
!
! only mixing one fire(smoke) scalar array
!
       CASE (TRACER_SMOKE,TRACER_TEST1,TRACER_TEST2)
        CALL wrf_debug(15,'DOING TRACER MIXING, 1 SPECIE ONLY')
        do nv=2,num_tracer
         do k=kts,kte
            pblst(k)=max(epsilc,tracer(i,k,j,nv))
         enddo

               call vertmx(dtstep,pblst,ekmfull,dryrho_1d, &
                           zzfull,zz,0.,kts,kte)
         do k=kts,kte-1
            tracer(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
        enddo
       CASE DEFAULT
!        CALL wrf_debug(15,'NOT YET DEFINED')
       END SELECT tracer_select

100   continue
      endif   ! num_vert_mix = 0
!
!  vertical mixing and activation of aerosol
!
   where( alt(its:ite,kts:kte,jts:jte) /= 0. )  !get dry density to conserve mass in mixactivate, wig, 24-apr-2006
      dryrho_phy(its:ite,kts:kte,jts:jte) = 1./alt(its:ite,kts:kte,jts:jte)
   elsewhere
      dryrho_phy(its:ite,kts:kte,jts:jte) = 0.
   end where

  qsrflx(:,:,:) = 0.0

   mixactivate_select: SELECT CASE(config_flags%chem_opt)

   CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM_KPP, RACM_ESRLSORG_AQCHEM_KPP, CBMZSORG_AQ, &
         CB05_SORG_AQ_KPP)
      CALL wrf_debug(15,'call mixactivate for sorgam aerosol')
      call sorgam_mixactivate (                        &
		id, ktau, dtstep, config_flags, idrydep_onoff,   &
		dryrho_phy, t_phy, w, cldfra, cldfra_old, &
		ddvel, z, dz8w, p8w, t8w, exch_h,         &
		moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem, &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
		ids,ide, jds,jde, kds,kde,                        &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )
   CASE (RACM_SOA_VBS_AQCHEM_KPP)
      CALL wrf_debug(15,'call mixactivate for soa-vbs aerosol')
      call soa_vbs_mixactivate (                        &
                id, ktau, dtstep, config_flags, idrydep_onoff,   &
                dryrho_phy, t_phy, w, cldfra, cldfra_old, &
                ddvel, z, dz8w, p8w, t8w, exch_h,         &
                moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC),moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem, &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
                ids,ide, jds,jde, kds,kde,                        &
                ims,ime, jms,jme, kms,kme,                        &
                its,ite, jts,jte, kts,kte                         ) 
   CASE (CB05_SORG_VBS_AQ_KPP)
      CALL wrf_debug(15,'call mixactivate for sorgam_vbs aerosol')
      call sorgam_vbs_mixactivate (                        &
                id, ktau, dtstep, config_flags, idrydep_onoff,   &
                dryrho_phy, t_phy, w, cldfra, cldfra_old, &
                ddvel, z, dz8w, p8w, t8w, exch_h,         &
                moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem, &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
                ids,ide, jds,jde, kds,kde,                        &
                ims,ime, jms,jme, kms,kme,                        &
                its,ite, jts,jte, kts,kte                         )
      ! Mixactivate called only for _AQ packages below MS 12/28/2013
   CASE (CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
   			CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP, &
   			MOZART_MOSAIC_4BIN_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP)!BSINGH(12/03/2013): Added SAPRC 8 bin)
      CALL wrf_debug(15,'call mixactivate for mosaic aerosol')
      call mosaic_mixactivate (                        &
		id, ktau, dtstep, config_flags, idrydep_onoff,   &
		dryrho_phy, t_phy, w, cldfra, cldfra_old, &
		ddvel, z, dz8w, p8w, t8w, exch_h,         &
		moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem,   &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,      &
        qsrflx,                                           &
		ids,ide, jds,jde, kds,kde,                        &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )

      if( config_flags%diagnostic_chem == DEPVEL1 .and. &
          config_flags%chem_opt == CB05_SORG_VBS_AQ_KPP ) then

          ! qsrflx is in kg/m2/s, recalculate to match ug/m2 for aerosols
          qsrflx = qsrflx * 1.0e9 * dtstep

          dvel(ims:ime,1,jms:jme,p_ddmass_so4aj) = dvel(ims:ime,1,jms:jme,p_ddmass_so4aj) + qsrflx(ims:ime,jms:jme,p_so4aj)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4ai) = dvel(ims:ime,1,jms:jme,p_ddmass_so4ai) + qsrflx(ims:ime,jms:jme,p_so4ai)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3aj) = dvel(ims:ime,1,jms:jme,p_ddmass_no3aj) + qsrflx(ims:ime,jms:jme,p_no3aj)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3ai) = dvel(ims:ime,1,jms:jme,p_ddmass_no3ai) + qsrflx(ims:ime,jms:jme,p_no3ai)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4aj) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4aj) + qsrflx(ims:ime,jms:jme,p_nh4aj)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4ai) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4ai) + qsrflx(ims:ime,jms:jme,p_nh4ai)

       endif

      if( config_flags%diagnostic_chem == DEPVEL1 .and. &
          config_flags%chem_opt == MOZART_MOSAIC_4BIN_AQ_KPP ) then

          ! qsrflx is in kg/m2/s, recalculate to match ug/m2 for aerosols
          qsrflx = qsrflx * 1.0e9 * dtstep

          dvel(ims:ime,1,jms:jme,p_ddmass_so4_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_a01) + qsrflx(ims:ime,jms:jme,p_so4_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_a01) + qsrflx(ims:ime,jms:jme,p_no3_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_a01) + qsrflx(ims:ime,jms:jme,p_cl_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a01) + qsrflx(ims:ime,jms:jme,p_nh4_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_na_a01) + qsrflx(ims:ime,jms:jme,p_na_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_a01) + qsrflx(ims:ime,jms:jme,p_oin_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_a01) + qsrflx(ims:ime,jms:jme,p_oc_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_a01) + qsrflx(ims:ime,jms:jme,p_bc_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_a02) + qsrflx(ims:ime,jms:jme,p_so4_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_a02) + qsrflx(ims:ime,jms:jme,p_no3_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_a02) + qsrflx(ims:ime,jms:jme,p_cl_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a02) + qsrflx(ims:ime,jms:jme,p_nh4_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_na_a02) + qsrflx(ims:ime,jms:jme,p_na_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_a02) + qsrflx(ims:ime,jms:jme,p_oin_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_a02) + qsrflx(ims:ime,jms:jme,p_oc_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_a02) + qsrflx(ims:ime,jms:jme,p_bc_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_a03) + qsrflx(ims:ime,jms:jme,p_so4_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_a03) + qsrflx(ims:ime,jms:jme,p_no3_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_a03) + qsrflx(ims:ime,jms:jme,p_cl_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a03) + qsrflx(ims:ime,jms:jme,p_nh4_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_na_a03) + qsrflx(ims:ime,jms:jme,p_na_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_a03) + qsrflx(ims:ime,jms:jme,p_oin_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_a03) + qsrflx(ims:ime,jms:jme,p_oc_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_a03) + qsrflx(ims:ime,jms:jme,p_bc_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_a04) + qsrflx(ims:ime,jms:jme,p_so4_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_a04) + qsrflx(ims:ime,jms:jme,p_no3_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_a04) + qsrflx(ims:ime,jms:jme,p_cl_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_a04) + qsrflx(ims:ime,jms:jme,p_nh4_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_na_a04) + qsrflx(ims:ime,jms:jme,p_na_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_a04) + qsrflx(ims:ime,jms:jme,p_oin_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_a04) + qsrflx(ims:ime,jms:jme,p_oc_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_a04) + qsrflx(ims:ime,jms:jme,p_bc_a04)

          dvel(ims:ime,1,jms:jme,p_ddmass_ca_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_a01) + qsrflx(ims:ime,jms:jme,p_ca_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_a02) + qsrflx(ims:ime,jms:jme,p_ca_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_a03) + qsrflx(ims:ime,jms:jme,p_ca_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_a04) + qsrflx(ims:ime,jms:jme,p_ca_a04)

          dvel(ims:ime,1,jms:jme,p_ddmass_co3_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_a01) + qsrflx(ims:ime,jms:jme,p_co3_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_a02) + qsrflx(ims:ime,jms:jme,p_co3_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_a03) + qsrflx(ims:ime,jms:jme,p_co3_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_a04) + qsrflx(ims:ime,jms:jme,p_co3_a04)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_a01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_a01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_a01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_a01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_a01)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_a02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_a02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_a02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_a02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_a02)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_a03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_a03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_a03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_a03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_a03)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_a04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_a04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_a04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_a04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_a04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_a04)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a01) + qsrflx(ims:ime,jms:jme,p_asoaX_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a01) + qsrflx(ims:ime,jms:jme,p_asoa1_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a01) + qsrflx(ims:ime,jms:jme,p_asoa2_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a01) + qsrflx(ims:ime,jms:jme,p_asoa3_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a01) + qsrflx(ims:ime,jms:jme,p_asoa4_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a01) + qsrflx(ims:ime,jms:jme,p_bsoaX_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a01) + qsrflx(ims:ime,jms:jme,p_bsoa1_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a01) + qsrflx(ims:ime,jms:jme,p_bsoa2_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a01) + qsrflx(ims:ime,jms:jme,p_bsoa3_a01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a01) + qsrflx(ims:ime,jms:jme,p_bsoa4_a01)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a02) + qsrflx(ims:ime,jms:jme,p_asoaX_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a02) + qsrflx(ims:ime,jms:jme,p_asoa1_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a02) + qsrflx(ims:ime,jms:jme,p_asoa2_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a02) + qsrflx(ims:ime,jms:jme,p_asoa3_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a02) + qsrflx(ims:ime,jms:jme,p_asoa4_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a02) + qsrflx(ims:ime,jms:jme,p_bsoaX_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a02) + qsrflx(ims:ime,jms:jme,p_bsoa1_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a02) + qsrflx(ims:ime,jms:jme,p_bsoa2_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a02) + qsrflx(ims:ime,jms:jme,p_bsoa3_a02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a02) + qsrflx(ims:ime,jms:jme,p_bsoa4_a02)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a03) + qsrflx(ims:ime,jms:jme,p_asoaX_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a03) + qsrflx(ims:ime,jms:jme,p_asoa1_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a03) + qsrflx(ims:ime,jms:jme,p_asoa2_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a03) + qsrflx(ims:ime,jms:jme,p_asoa3_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a03) + qsrflx(ims:ime,jms:jme,p_asoa4_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a03) + qsrflx(ims:ime,jms:jme,p_bsoaX_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a03) + qsrflx(ims:ime,jms:jme,p_bsoa1_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a03) + qsrflx(ims:ime,jms:jme,p_bsoa2_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a03) + qsrflx(ims:ime,jms:jme,p_bsoa3_a03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a03) + qsrflx(ims:ime,jms:jme,p_bsoa4_a03)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_a04) + qsrflx(ims:ime,jms:jme,p_asoaX_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_a04) + qsrflx(ims:ime,jms:jme,p_asoa1_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_a04) + qsrflx(ims:ime,jms:jme,p_asoa2_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_a04) + qsrflx(ims:ime,jms:jme,p_asoa3_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_a04) + qsrflx(ims:ime,jms:jme,p_asoa4_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_a04) + qsrflx(ims:ime,jms:jme,p_bsoaX_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_a04) + qsrflx(ims:ime,jms:jme,p_bsoa1_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_a04) + qsrflx(ims:ime,jms:jme,p_bsoa2_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_a04) + qsrflx(ims:ime,jms:jme,p_bsoa3_a04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_a04) + qsrflx(ims:ime,jms:jme,p_bsoa4_a04)

! and the same for settling cloud droplets

          dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw01) + qsrflx(ims:ime,jms:jme,p_so4_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw01) + qsrflx(ims:ime,jms:jme,p_no3_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw01) + qsrflx(ims:ime,jms:jme,p_cl_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw01) + qsrflx(ims:ime,jms:jme,p_nh4_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_na_cw01) + qsrflx(ims:ime,jms:jme,p_na_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw01) + qsrflx(ims:ime,jms:jme,p_oin_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw01) + qsrflx(ims:ime,jms:jme,p_oc_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw01) + qsrflx(ims:ime,jms:jme,p_bc_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw02) + qsrflx(ims:ime,jms:jme,p_so4_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw02) + qsrflx(ims:ime,jms:jme,p_no3_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw02) + qsrflx(ims:ime,jms:jme,p_cl_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw02) + qsrflx(ims:ime,jms:jme,p_nh4_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_na_cw02) + qsrflx(ims:ime,jms:jme,p_na_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw02) + qsrflx(ims:ime,jms:jme,p_oin_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw02) + qsrflx(ims:ime,jms:jme,p_oc_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw02) + qsrflx(ims:ime,jms:jme,p_bc_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw03) + qsrflx(ims:ime,jms:jme,p_so4_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw03) + qsrflx(ims:ime,jms:jme,p_no3_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw03) + qsrflx(ims:ime,jms:jme,p_cl_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw03) + qsrflx(ims:ime,jms:jme,p_nh4_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_na_cw03) + qsrflx(ims:ime,jms:jme,p_na_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw03) + qsrflx(ims:ime,jms:jme,p_oin_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw03) + qsrflx(ims:ime,jms:jme,p_oc_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw03) + qsrflx(ims:ime,jms:jme,p_bc_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_so4_cw04) + qsrflx(ims:ime,jms:jme,p_so4_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_no3_cw04) + qsrflx(ims:ime,jms:jme,p_no3_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_cl_cw04) + qsrflx(ims:ime,jms:jme,p_cl_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_nh4_cw04) + qsrflx(ims:ime,jms:jme,p_nh4_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_na_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_na_cw04) + qsrflx(ims:ime,jms:jme,p_na_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_oin_cw04) + qsrflx(ims:ime,jms:jme,p_oin_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_oc_cw04) + qsrflx(ims:ime,jms:jme,p_oc_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bc_cw04) + qsrflx(ims:ime,jms:jme,p_bc_cw04)

          dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw01) + qsrflx(ims:ime,jms:jme,p_ca_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw02) + qsrflx(ims:ime,jms:jme,p_ca_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw03) + qsrflx(ims:ime,jms:jme,p_ca_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_ca_cw04) + qsrflx(ims:ime,jms:jme,p_ca_cw04)

          dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw01) + qsrflx(ims:ime,jms:jme,p_co3_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw02) + qsrflx(ims:ime,jms:jme,p_co3_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw03) + qsrflx(ims:ime,jms:jme,p_co3_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_co3_cw04) + qsrflx(ims:ime,jms:jme,p_co3_cw04)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_cw01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_cw01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_cw01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_cw01) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_cw01)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_cw02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_cw02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_cw02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_cw02) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_cw02)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_cw03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_cw03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_cw03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_cw03) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_cw03)

          dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_glysoa_cw04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r1_cw04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_r2_cw04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_oh_cw04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_sfc_cw04) + &
                                            qsrflx(ims:ime,jms:jme,p_glysoa_nh4_cw04)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw01) + qsrflx(ims:ime,jms:jme,p_asoaX_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw01) + qsrflx(ims:ime,jms:jme,p_asoa1_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw01) + qsrflx(ims:ime,jms:jme,p_asoa2_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw01) + qsrflx(ims:ime,jms:jme,p_asoa3_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw01) + qsrflx(ims:ime,jms:jme,p_asoa4_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw01) + qsrflx(ims:ime,jms:jme,p_bsoaX_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw01) + qsrflx(ims:ime,jms:jme,p_bsoa1_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw01) + qsrflx(ims:ime,jms:jme,p_bsoa2_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw01) + qsrflx(ims:ime,jms:jme,p_bsoa3_cw01)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw01) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw01) + qsrflx(ims:ime,jms:jme,p_bsoa4_cw01)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw02) + qsrflx(ims:ime,jms:jme,p_asoaX_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw02) + qsrflx(ims:ime,jms:jme,p_asoa1_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw02) + qsrflx(ims:ime,jms:jme,p_asoa2_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw02) + qsrflx(ims:ime,jms:jme,p_asoa3_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw02) + qsrflx(ims:ime,jms:jme,p_asoa4_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw02) + qsrflx(ims:ime,jms:jme,p_bsoaX_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw02) + qsrflx(ims:ime,jms:jme,p_bsoa1_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw02) + qsrflx(ims:ime,jms:jme,p_bsoa2_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw02) + qsrflx(ims:ime,jms:jme,p_bsoa3_cw02)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw02) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw02) + qsrflx(ims:ime,jms:jme,p_bsoa4_cw02)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw03) + qsrflx(ims:ime,jms:jme,p_asoaX_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw03) + qsrflx(ims:ime,jms:jme,p_asoa1_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw03) + qsrflx(ims:ime,jms:jme,p_asoa2_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw03) + qsrflx(ims:ime,jms:jme,p_asoa3_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw03) + qsrflx(ims:ime,jms:jme,p_asoa4_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw03) + qsrflx(ims:ime,jms:jme,p_bsoaX_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw03) + qsrflx(ims:ime,jms:jme,p_bsoa1_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw03) + qsrflx(ims:ime,jms:jme,p_bsoa2_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw03) + qsrflx(ims:ime,jms:jme,p_bsoa3_cw03)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw03) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw03) + qsrflx(ims:ime,jms:jme,p_bsoa4_cw03)

          dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoaX_cw04) + qsrflx(ims:ime,jms:jme,p_asoaX_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa1_cw04) + qsrflx(ims:ime,jms:jme,p_asoa1_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa2_cw04) + qsrflx(ims:ime,jms:jme,p_asoa2_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa3_cw04) + qsrflx(ims:ime,jms:jme,p_asoa3_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_asoa4_cw04) + qsrflx(ims:ime,jms:jme,p_asoa4_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoaX_cw04) + qsrflx(ims:ime,jms:jme,p_bsoaX_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa1_cw04) + qsrflx(ims:ime,jms:jme,p_bsoa1_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa2_cw04) + qsrflx(ims:ime,jms:jme,p_bsoa2_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa3_cw04) + qsrflx(ims:ime,jms:jme,p_bsoa3_cw04)
          dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw04) = dvel(ims:ime,1,jms:jme,p_ddmass_bsoa4_cw04) + qsrflx(ims:ime,jms:jme,p_bsoa4_cw04)

      endif

   CASE DEFAULT
   END SELECT mixactivate_select

   IF((config_flags%dust_opt .EQ. 1) .OR. (config_flags%dust_opt .GE. 3) .OR. &
      (config_flags%seas_opt .GE. 1) ) THEN
   settling_select: SELECT CASE(config_flags%chem_opt)
!!! TUCCELLA
   CASE (DUST,GOCART_SIMPLE,GOCARTRACM_KPP,MOZCART_KPP,T1_MOZCART_KPP,RADM2SORG,RADM2SORG_AQ, &
         RADM2SORG_AQCHEM,RACMSORG_AQCHEM_KPP,RACM_ESRLSORG_AQCHEM_KPP,RACM_SOA_VBS_AQCHEM_KPP)
       CALL wrf_debug(15,'call gocart settling routine')
         call gocart_settling_driver(dtstep,config_flags,t_phy,moist,  &
         chem,rho_phy,dz8w,p8w,p_phy,         &
         dustin,seasin,dx,g, &
         dustgraset_1,dustgraset_2,dustgraset_3,                           &
         dustgraset_4,dustgraset_5,                                        &
         setvel_1,setvel_2,setvel_3,setvel_4,setvel_5, imod,               &              
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
   CASE (CHEM_VASH, CHEM_VOLC, CHEM_VOLC_4BIN)
       CALL wrf_debug(15,'call vash settling routine')
         call vash_settling_driver(dtstep,config_flags,t_phy,moist,        &
         chem,rho_phy,dz8w,p8w,p_phy,                                      &
         ash_fall,dx,g,                                                    &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
   CASE DEFAULT
       CALL wrf_debug(15,'no settling routine')
   END SELECT settling_select
   ENDIF

       CALL wrf_debug(15,'end of dry_dep_driver')

END SUBROUTINE dry_dep_driver

END MODULE module_dry_dep_driver
