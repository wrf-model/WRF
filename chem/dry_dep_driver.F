!WRF:MODEL_LAYER:CHEMICS
!
    subroutine dry_dep_driver(id,ktau,dtstep,config_flags,                &
               gmt,julday,t_phy,moist,scalar,p8w,t8w,w,alt,               &
               p_phy,chem,rho_phy,dz8w,exch_h,                            &
               cldfra, cldfra_old,                                        &
               ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,               &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,   &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,      &
               e_co,kemit,numgas,                                         &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
!----------------------------------------------------------------------
  USE module_model_constants
  USE module_configure
  USE module_state_description
  USE module_dep_simple
  USE module_vertmx_wrf
  USE module_data_sorgam
  USE module_aerosols_sorgam
  USE module_mosaic_drydep, only:  mosaic_drydep_driver
  USE module_mixactivate_wrappers, only: mosaic_mixactivate, sorgam_mixactivate
  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday,                    &
                                  numgas,                       &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                               &
                                  ktau
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_scalar ),       &
         INTENT(INOUT ) ::                               scalar
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                 chem

   INTEGER,      INTENT(IN   ) :: kemit
   REAL, DIMENSION( ims:ime, kms:kemit, jms:jme ),            &
         INTENT(IN ) ::                                                    &
          e_co




   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                        alt,    &
                                                      p_phy,    &
                                                      dz8w,     &
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
                                                     xlat,      &
                                                     xlong,     &
                                                     znt
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,        &
          INTENT(INOUT ) ::                                     &
                    cldfra,     & ! cloud fraction current timestep
                    cldfra_old    ! cloud fraction previous timestep
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
          INTENT(OUT) ::                                      &
                                                     dep_vel_o3
   REAL, INTENT(OUT), dimension(ims:ime,kms:kme,jms:jme) :: nsource, &
	     ccn1,ccn2,ccn3,ccn4,ccn5,ccn6  ! number conc of aerosols activated at supersat

      REAL,      INTENT(IN   ) ::                               &
                             dtstep,gmt

!--- deposition and emissions stuff
! .. Parameters ..
! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dvfog, dvpart,  &
        rad, rhchem, ta, ustar, vegfrac, z1,zntt

      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                 n, nr, ipr, jpr, nvr,   &
                 idrydep_onoff

      LOGICAL :: highnh3, rainflag, vegflag, wetflag
!     CHARACTER (4) :: luse_typ,mminlu_loc
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte-1)
   REAL, DIMENSION( its:ite, jts:jte, num_chem ) ::   ddvel

   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ) :: dryrho_phy
   REAL,  DIMENSION( kms:kme ) :: dryrho_1d

! turbulent transport
      real :: pblst(kts:kte-1),ekmfull(kts:kte),zzfull(kts:kte),zz(kts:kte-1)
      integer :: ii,jj,kk,i,j,k,nv
!
! necessary for aerosols (module dependent)
!
   REAL, DIMENSION( its:ite, jts:jte ) ::   aer_res

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags


! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min

!
! compute dry deposition velocities = ddvel
!
! 28-jun-2005 rce - initialize ddvel=0; call aerosol drydep routine
!           only when drydep_opt == WESELY
!       the wesely_driver routine computes aer_res, and currently
!	    you cannot compute aerosol drydep without it !!
! 08-jul-2005 rce - pass idrydep_onoff to mixactivate routines
!
   ddvel(:,:,:) = 0.0
   idrydep_onoff = 0

   drydep_select: SELECT CASE(config_flags%drydep_opt)

     CASE ( WESELY )
!
! drydep_opt == WESELY means 
!     wesely for gases 
!     other (appropriate) routine for aerosols
!
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES WITH WESELY METHOD')

       IF( config_flags%chem_opt /= CHEM_TRACER ) THEN
          call wesely_driver(id,ktau,dtstep,                                 &
               config_flags,                                              &
               gmt,julday,t_phy,moist,p8w,t8w,                            &
               p_phy,chem,rho_phy,dz8w,ddvel,aer_res,                     &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               numgas,                                                    &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte                                  )
       ELSE
          !Set dry deposition velocity to zero when using the
          !chemistry tracer mode.
          ddvel(:,:,:) = 0.
       END IF

       idrydep_onoff = 1


   adrydep_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2SORG,RADM2SORG_AQ,RADM2SORG_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RADM')
       call sorgam_depdriver (id,ktau,dtstep,                           &
               ust,t_phy,moist,p8w,t8w,                                 &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE (RACMSORG,RACMSORG_AQ,RACMSORG_KPP)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RACM')
       call sorgam_depdriver (id,ktau,dtstep,                           &
               ust,t_phy,moist,p8w,t8w,                                 &
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,                    &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,               &
               aer_res,ddvel(:,:,numgas+1:num_chem),                    &
               num_chem-numgas,                                         &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE ( CBMZ_MOSAIC_4BIN, CBMZ_MOSAIC_8BIN, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ )
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR MOSAIC AEROSOLS')
       call mosaic_drydep_driver(                                       &
               id, ktau, dtstep, config_flags,                          &
               gmt, julday,                                             &
               t_phy, rho_phy, p_phy,                                   &
               ust, aer_res,                                            &
               moist, chem, ddvel,                                      &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               its,ite, jts,jte, kts,kte                                )
     CASE DEFAULT
                                                     
   END SELECT adrydep_select

     CASE DEFAULT 
                                                     
   END SELECT drydep_select                              



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
      do k=kts,kte
         zzfull(k)=z_at_w(i,k,j)-z_at_w(i,kts,j)
         ekmfull(k)=max(1.e-6,exch_h(i,k,j))
      enddo

!!$! UNCOMMENT THIS AND FINE TUNE LEVELS TO YOUR DOMAIN IF YOU WANT TO
!!$! FORCE MIXING TO A CERTAIN DEPTH:
!!$!
!!$! --- Mix the emissions up several layers
!!$!     if e_co > 0., the grid cell should not be over water
!!$!     if e_co > 200, the grid cell should be over a large urban region
!!$!
!       if (e_co(i,kts,j) .gt. 0) then
!          ekmfull(kts:kts+10) = max(ekmfull(kts:kts+10),1.)
!       endif
!       if (e_co(i,kts,j) .gt. 200) then
!          ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
!       endif
!!$!
!
!
      do k=kts,kte-1
         zz(k)=z(i,k,j)-z_at_w(i,kts,j)
      enddo
      ekmfull(kts)=0.
      ekmfull(kte)=0.
!
!   vertical mixing routine (including deposition)
!   need to be careful here with that dumm tracer in spot 1
!   do not need lho,lho2
!   (03-may-2006 rce - calc dryrho_1d and pass it to vertmx)
!
      dep_vel_o3(i,j)=ddvel(i,j,p_o3)
      do nv=2,num_chem-0
         do k=kts,kte-1
!           pblst(k)=max(epsilc,chem(i,k,j,nv))
            pblst(k)=max(epsilc,chem(i,k,j,nv)/alt(i,k,j))
            dryrho_1d(k) = 1./alt(i,k,j)
         enddo
!        if(ktau.ge.20)then
!           write(0,*)i,j,nv,ddvel(i,j,nv),e_co(i,kts,j)
!        endif

         mix_select: SELECT CASE(config_flags%chem_opt)
         CASE (RADM2SORG_AQ, RACMSORG_AQ, CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
            if(.not.is_aerosol(nv))then ! mix gases not aerosol
!              call vertmx(dtstep,pblst,ekmfull,dryrho_1d, &
!                          zzfull,zz,ddvel(i,j,nv),kts,kte-1)
               call vertmx(dtstep,pblst,ekmfull,zzfull,zz,ddvel(i,j,nv),kts,kte-1)

            endif
!        CASE (PRESCRIBE_AEROSOL)

         CASE DEFAULT
            call vertmx(dtstep,pblst,ekmfull,zzfull,zz,ddvel(i,j,nv),kts,kte-1)

         END SELECT mix_select

         do k=kts,kte-2

!           chem(i,k,j,nv)=max(epsilc,pblst(k))
            chem(i,k,j,nv)=max(epsilc,pblst(k)*alt(i,k,j))
         enddo
      enddo
100   continue
!
!  vertical mixing and activation of aerosol
!
   where( alt(its:ite,kts:kte,jts:jte) /= 0. )  !get dry density to conserve mass in mixactivate, wig, 24-apr-2006
      dryrho_phy(its:ite,kts:kte,jts:jte) = 1./alt(its:ite,kts:kte,jts:jte)
      dryrho_phy(its:ite,kts:kte,jts:jte) = 1.
   elsewhere
      dryrho_phy(its:ite,kts:kte,jts:jte) = 0.
   end where

   mixactivate_select: SELECT CASE(config_flags%chem_opt)

   CASE (RADM2SORG_AQ, RACMSORG_AQ)
      call sorgam_mixactivate (                        &
		id, ktau, dtstep, config_flags, idrydep_onoff,   &
		dryrho_phy, t_phy, w, cldfra, cldfra_old, &
		ddvel, z, dz8w, p8w, t8w, exch_h,         &
		moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem, &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,       &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )
   CASE (CBMZ_MOSAIC_4BIN_AQ, CBMZ_MOSAIC_8BIN_AQ)
       CALL wrf_debug(15,'call mixactive for mosaic aerosol')
      call mosaic_mixactivate (                        &
		id, ktau, dtstep, config_flags, idrydep_onoff,   &
		dryrho_phy, t_phy, w, cldfra, cldfra_old, &
		ddvel, z, dz8w, p8w, t8w, exch_h,         &
		moist(ims,kms,jms,P_QV), moist(ims,kms,jms,P_QC), moist(ims,kms,jms,P_QI), &
        scalar(ims,kms,jms,P_QNDROP), f_qc, f_qi, chem,   &
        ccn1, ccn2, ccn3, ccn4, ccn5, ccn6, nsource,      &
		ims,ime, jms,jme, kms,kme,                        &
		its,ite, jts,jte, kts,kte                         )
   CASE DEFAULT
   END SELECT mixactivate_select
       CALL wrf_debug(15,'end of dry_dep_driver')

END SUBROUTINE dry_dep_driver
