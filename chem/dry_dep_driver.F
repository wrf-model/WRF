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
               anh3,cvaro1,cvaro2,   &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,      &
               e_co,kemit,sf_urban_physics,numgas,current_month,dvel,     &
               snowh,                                                     &
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
  USE module_mixactivate_wrappers, only: mosaic_mixactivate, sorgam_mixactivate
  USE module_aer_drydep
  USE module_aerosols_soa_vbs, only: soa_vbs_depdriver

  IMPLICIT NONE

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

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


   INTEGER,      INTENT(IN   ) :: kemit
   REAL, DIMENSION( ims:ime, kms:kemit, jms:jme ),            &
         INTENT(IN ) ::                                                    &
          e_co

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
   REAL, INTENT(OUT), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat

      REAL,      INTENT(IN   ) ::                               &
                             dtstep,gmt,dx

!--- deposition and emissions stuff
! .. Parameters ..
! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dvfog, dvpart,  &
        rad, rhchem, ta, ustar, vegfrac, z1,zntt

      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                 n, nr, ipr, jpr, nvr,   &
                 idrydep_onoff, aer_mech_id

      LOGICAL :: highnh3, rainflag, vegflag, wetflag
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte)
   REAL, DIMENSION( its:ite, jts:jte, num_chem ) ::   ddvel

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dryrho_phy
   REAL,  DIMENSION( kms:kme ) :: dryrho_1d

! turbulent transport
      real :: pblst(kts:kte),ekmfull(kts:kte+1),zzfull(kts:kte+1),zz(kts:kte)
      integer :: ii,jj,kk,i,j,k,nv
!
! necessary for aerosols (module dependent)
!
   REAL, DIMENSION( its:ite, jts:jte ) ::   aer_res, aer_res_def, aer_res_zcen


   real, parameter :: m2cm = 100.


! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min

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

       IF( config_flags%chem_opt /= CHEM_TRACER  .and.                   &
           config_flags%chem_opt /= CHEM_TRACE2  .and.                   &
           config_flags%chem_opt /= CO2_TRACER   .and.                   & 
           config_flags%chem_opt /= GHG_TRACER   .and.                   &
           config_flags%chem_opt /= GOCART_SIMPLE  .and.                 &
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
          IF ( config_flags%chem_opt == MOZCART_KPP ) then
            call gocart_drydep_driver( dtstep,                            &
                  config_flags, numgas,                                   &
                  t_phy, moist, p8w, t8w, rmol,aer_res_def,               &
                  p_phy, chem, rho_phy, dz8w, ddvel, xland, hfx,          &
                  ivgtyp, tsk, vegfra, pbl, ust, znt, xlat, xlong,        &
                  ids,ide, jds,jde, kds,kde,                              &
                  ims,ime, jms,jme, kms,kme,                              &
                  its,ite, jts,jte, kts,kte )
          ENDIF
          if( config_flags%diagnostic_chem == DEPVEL1 .and. &
              (config_flags%chem_opt == MOZCART_KPP .or. &
              config_flags%chem_opt == MOZART_KPP   .or. & 
              config_flags%chem_opt == MOZART_MOSAIC_4BIN_VBS0_KPP) ) then
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
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )

       ELSE
          !Set dry deposition velocity to zero when using the
          !chemistry tracer mode.
          ddvel(:,:,:) = 0.
       END IF

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
          CASE (RADM2SORG,RADM2SORG_AQ,RADM2SORG_AQCHEM,RADM2SORG_KPP, &
                RACM_ESRLSORG_KPP,RACM_SOA_VBS_KPP, &
                CBMZSORG,CBMZSORG_AQ)
             aer_mech_id = 1
          CASE (RACMSORG_AQ,RACMSORG_AQCHEM,RACMSORG_KPP)
             aer_mech_id = 2
          CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, &
                 CBMZ_MOSAIC_8BIN_AQ,SAPRC99_MOSAIC_4BIN_VBS2_KPP,MOZART_MOSAIC_4BIN_VBS0_KPP,  &
                 CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ)
             aer_mech_id = 3
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
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE (RACMSORG_AQ,RACMSORG_AQCHEM,RACMSORG_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RACM')
       call sorgam_depdriver (id,config_flags,ktau,dtstep,              &
               ust,t_phy,moist,p8w,t8w,rmol,znt,pbl,                    &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
          CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ, &
          SAPRC99_MOSAIC_4BIN_VBS2_KPP, MOZART_MOSAIC_4BIN_VBS0_KPP  )
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
     CASE ( RACM_SOA_VBS_KPP )
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
          if ((aer_mech_id > 0) .and. (aer_mech_id <= 3)) then
             ! limit aerosol ddvels to <= 0.5 m/s
             ! drydep routines occasionally produce unrealistically-large particle
             !    diameter leading to unrealistically-large sedimentation velocity
             ddvel(:,:,numgas+1:num_chem) = min( 0.50, ddvel(:,:,numgas+1:num_chem) )
          end if
       end if



     CASE DEFAULT 
                                                     
   END SELECT drydep_select                              

! Add dvel here for all aerosol and gas species in the dvel array 
!    l = max( 1, min( num_chem, numddvel ) )
!       do i=its,ite
!        do j=jts,jte
!          do k=1,l
!    dvel(i,j,k) = ddvel(i,j,k)
!        enddo
!         enddo
!        enddo
!
!        do i=its,ite
!        do j=jts,jte
!         do k=l+1,numddvel
!    if (l < numddvel) dvel(i,j,k) = 0.0
!        enddo
!        enddo
!          enddo

!   This will be called later from subgrd_transport_driver.F !!!!!!!!
!
!
      dep_vel_o3=0.
      do 100 j=jts,jte
      do 100 i=its,ite
      pblst=0.
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
!!$! --- Mix the emissions up several layers
!!$!     if e_co > 0., the grid cell should not be over water
!!$!     if e_co > 200, the grid cell should be over a large urban region
!!$!
     if (sf_urban_physics .eq. 0) then
      if (e_co(i,kts,j) .gt. 0) then
          ekmfull(kts:kts+10) = max(ekmfull(kts:kts+10),1.)
       endif
       if (e_co(i,kts,j) .gt. 200) then
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
         do k=kts,kte
            pblst(k)=max(epsilc,chem(i,k,j,nv))
            dryrho_1d(k) = 1./alt(i,k,j)
         enddo

         mix_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, &
              CBMZ_MOSAIC_DMS_4BIN, CBMZ_MOSAIC_DMS_8BIN, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ)
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

         do k=kts,kte-1
            chem(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
      enddo
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
       CALL wrf_debug(15,'NOT YET DEFINED')
       END SELECT tracer_select

100   continue
!
!  vertical mixing and activation of aerosol
!
   where( alt(its:ite,kts:kte,jts:jte) /= 0. )  !get dry density to conserve mass in mixactivate, wig, 24-apr-2006
      dryrho_phy(its:ite,kts:kte,jts:jte) = 1./alt(its:ite,kts:kte,jts:jte)
   elsewhere
      dryrho_phy(its:ite,kts:kte,jts:jte) = 0.
   end where

   mixactivate_select: SELECT CASE(config_flags%chem_opt)

   CASE (RADM2SORG_AQ, RADM2SORG_AQCHEM, RACMSORG_AQ, RACMSORG_AQCHEM)
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
   CASE (CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ, CBMZ_MOSAIC_DMS_4BIN_AQ, CBMZ_MOSAIC_DMS_8BIN_AQ)
      CALL wrf_debug(15,'call mixactivate for mosaic aerosol')
      call mosaic_mixactivate (                        &
		id, ktau, dtstep, config_flags, idrydep_onoff,   &
		dryrho_phy, t_phy, w, cldfra, cldfra_old, &
		ddvel, z, dz8w, p8w, t8w, exch_h,         &
		moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem,   &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,      &
		ids,ide, jds,jde, kds,kde,                        &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )
   CASE DEFAULT
   END SELECT mixactivate_select
   settling_select: SELECT CASE(config_flags%chem_opt)
   CASE (DUST,GOCART_SIMPLE,GOCARTRACM_KPP,MOZCART_KPP,RADM2SORG,RADM2SORG_AQ,RADM2SORG_AQCHEM)
       CALL wrf_debug(0,'call gocart settling routine')
         call gocart_settling_driver(dtstep,config_flags,t_phy,moist,  &
         chem,rho_phy,dz8w,p8w,p_phy,         &
         dustin,seasin,dx,g, &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )
   CASE (CHEM_VASH, CHEM_VOLC)
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

       CALL wrf_debug(15,'end of dry_dep_driver')

END SUBROUTINE dry_dep_driver

END MODULE module_dry_dep_driver
