!WRF:MODEL_LAYER:CHEMICS
!
    subroutine dry_dep_driver(id,ktau,dtstep, &
               config_flags, &
               gmt,julday,t_phy,moist,p8w,t8w,alt, &
               p_phy,chem,rho_phy,dz8w,exch_h, &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3, &
               imicrogram,                                           &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
!----------------------------------------------------------------------
  USE module_model_constants
  USE module_configure
  USE module_state_description
! USE module_data_radm2
  USE module_dep_simple
  USE module_vertmx_wrf
  USE module_data_sorgam
  USE module_aerosols_sorgam
  IMPLICIT NONE

   INTEGER,      INTENT(IN   ) :: id,julday,imicrogram,         &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) ::                               &
                                  ktau
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),         &
         INTENT(INOUT ) ::                                   chem





   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      t_phy,    &
                                                      p_phy,    &
                                                 alt, dz8w,     &
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
          INTENT(IN   ) ::                                      &
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
!  integer numchem
!  parameter (numchem=p_ho2-1)
      INTEGER :: iland, iprt, iseason, jce, jcs,  &
                                              n, nr, ipr,jpr,nvr
      LOGICAL :: highnh3, rainflag, vegflag, wetflag
!     CHARACTER (4) :: luse_typ,mminlu_loc
! ..
! .. Local Arrays ..
      REAL :: p(kts:kte-1)
   REAL, DIMENSION( its:ite, jts:jte, p_ho2 ) ::   vgs
!
!
   REAL, DIMENSION( its:ite, jts:jte, numaer ) ::   vgsa
! turbulent transport
      real :: pblst(kts:kte-1),ekmfull(kts:kte),zzfull(kts:kte),zz(kts:kte-1)
      real :: vgsinp
      integer :: ii,jj,kk,i,j,k,nv
!
! necessary for aerosols (module dependent)
!
   REAL, DIMENSION( its:ite, jts:jte ) ::   aer_res

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags


! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
!     numchem=p_ho2-1
! ..
   drydep_select: SELECT CASE(config_flags%drydep_opt)
     CASE (WESELY)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES WITH WESELY METHOD')
!      print *,'p_ho2-1'
!      print *,p_ho2-1
       call wesely_driver(id,ktau,dtstep, &
               config_flags, &
               gmt,julday,t_phy,moist,p8w,t8w, &
               p_phy,chem,rho_phy,dz8w,vgs,aer_res, &
               ivgtyp,tsk,gsw,vegfra,pbl,rmol,ust,znt,xlat,xlong,z,z_at_w,&
               p_ho2,                             &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )

     CASE DEFAULT 
                                                     
   END SELECT drydep_select                              
   adrydep_select: SELECT CASE(config_flags%chem_opt)
       CASE (RADM2SORG)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RADM')
       if(imicrogram == 1 ) THEN
          CALL wrf_error_fatal ( 'dry_dep_driver: IMICROGRAM==1')
       endif
       call sorgam_depdriver (id,ktau,dtstep, &
               ust,t_phy,moist,p8w,t8w, & 
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,&
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    & 
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,&
               aer_res,vgsa,                                               &
               imicrogram,                                                 &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
       CASE (RACMSORG)
       CALL wrf_debug(15,'DOING DRY DEP VELOCITIES FOR AEROSOLS/RACM')
       call sorgam_depdriver (id,ktau,dtstep, &
               ust,t_phy,moist,p8w,t8w, & 
               alt,p_phy,chem,rho_phy,dz8w,z,z_at_w,&
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    & 
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,&
               aer_res,vgsa,                                               &
               imicrogram,                                                 &
               ids,ide, jds,jde, kds,kde,                            &
               ims,ime, jms,jme, kms,kme,                            &
               its,ite, jts,jte, kts,kte                             )
     CASE DEFAULT 
                                                     
   END SELECT adrydep_select                              
!   This will be called later from subgrd_transport_driver.F !!!!!!!!
!
!
      dep_vel_o3=0.
!     kts=k_start
!     kte=k_end
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
      do k=kts,kte-1
       zz(k)=z(i,k,j)-z_at_w(i,kts,j)
      enddo
      ekmfull(kts)=0.
      ekmfull(kte)=0.
      do k=kts+1,kte-1
        ekmfull(k)=ekmfull(k)*(zzfull(k)-zzfull(k-1))
      enddo
!
!   vertical mixing routine (including deposition)
!   need to be careful here with that dumm tracer in spot 1
!   do not need lho,lho2
!
      dep_vel_o3(i,j)=vgs(i,j,p_o3)
      do nv=2,num_chem-0
      do k=kts,kte-1
       pblst(k)=max(epsilc,chem(i,k,j,nv))
      enddo
      if(p_nu0.gt.1.and.nv.gt.p_ho2)then
         vgsinp=vgsa(i,j,nv-p_ho2)
      elseif(p_nu0.gt.1.and.nv.le.p_ho2)then
         vgsinp=vgs(i,j,nv)
      elseif(p_nu0.eq.1.and.nv.gt.p_ho2)then
         print *,'trying turbulent mixing in _driver'
         CALL wrf_error_fatal ( 'dry_dep_driver: TURBULENT MIXING')
      elseif(p_nu0.eq.1.and.nv.le.p_ho2)then      ! SEP
         vgsinp=vgs(i,j,nv)
      endif

      call vertmx(dtstep,pblst,ekmfull,zzfull,zz,vgsinp,kts,kte-1)

      do k=kts,kte-1
       chem(i,k,j,nv)=max(epsilc,pblst(k))
      enddo
      enddo
100   continue

END SUBROUTINE dry_dep_driver
