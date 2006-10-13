!WRF:MODEL_LAYER:CHEMICS
!
    subroutine dry_dep_driver(id,ktau,dtstep,config_flags,                &
               gmt,julday,t_phy,moist,p8w,t8w,alt,                        &
               p_phy,chem,rho_phy,dz8w,exch_h,                            &
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
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                   chem

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
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &
          INTENT(OUT) ::                                      &
                                                     dep_vel_o3
      REAL,      INTENT(IN   ) ::                               &
                             dtstep,gmt

!--- deposition and emissions stuff
! .. Parameters ..
! ..
! .. Local Scalars ..
      REAL ::  clwchem,  dvfog, dvpart,  &
        rad, rhchem, ta, ustar, vegfrac, z1,zntt

      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                                              n, nr, ipr,jpr,nvr
      LOGICAL :: highnh3, rainflag, vegflag, wetflag
!     CHARACTER (4) :: luse_typ,mminlu_loc
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte-1)
   REAL, DIMENSION( its:ite, jts:jte, num_chem ) ::   ddvel

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

! ..
   ddvel(:,:,:) = 0.
   IF( config_flags%chem_opt /= CHEM_TRACER ) THEN
      drydep_select: SELECT CASE(config_flags%drydep_opt)
      CASE (WESELY)
         CALL wrf_debug(15,'DOING DRY DEP VELOCITIES WITH WESELY METHOD')
         call wesely_driver(id,ktau,dtstep,                                 &
              config_flags,                                              &
              gmt,julday,t_phy,moist,p8w,t8w,                            &
              p_phy,chem,rho_phy,dz8w,ddvel,aer_res,                     &
              ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
              numgas,                                                    &
              ids,ide, jds,jde, kds,kde,                                 &
              ims,ime, jms,jme, kms,kme,                                 &
              its,ite, jts,jte, kts,kte                                  )

      CASE DEFAULT 
                                                     
      END SELECT drydep_select
   ELSE
      !Set dry deposition velocity to zero when using the
      !chemistry tracer mode.
   END IF

   adrydep_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2SORG,RADM2SORG_KPP)
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
     CASE (RACMSORG,RACMSORG_KPP)
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
     CASE ( CBMZ_MOSAIC_AA, CBMZ_MOSAIC_BB )
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

!
! --- Mix the emissions up several layers
!     if e_co > 0., the grid cell should not be over water
!     if e_co > 200, the grid cell should be over a large urban region
!
        if (e_co(i,kts,j) .gt. 0) then
           ekmfull(kts:kts+10) = max(ekmfull(kts:kts+10),1.)
        endif
        if (e_co(i,kts,j) .gt. 200) then
           ekmfull(kts:kte/2) = max(ekmfull(kts:kte/2),2.)
        endif
!
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
!
      dep_vel_o3(i,j)=ddvel(i,j,p_o3)
      do nv=2,num_chem-0
         do k=kts,kte-1
            pblst(k)=max(epsilc,chem(i,k,j,nv))
         enddo
!        if(ktau.ge.20)then
!           write(0,*)i,j,nv,ddvel(i,j,nv),e_co(i,kts,j)
!        endif

         call vertmx(dtstep,pblst,ekmfull,zzfull,zz,ddvel(i,j,nv),kts,kte-1)

         do k=kts,kte-2

            chem(i,k,j,nv)=max(epsilc,pblst(k))
         enddo
      enddo
100   continue
END SUBROUTINE dry_dep_driver
