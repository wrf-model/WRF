!WRF:MODEL_LAYER:CHEMICS
!
    subroutine chem_driver ( grid , config_flags , &
!
#include "em_dummy_args.inc"
!
                 )
!----------------------------------------------------------------------
  USE module_domain
  USE module_configure
  USE module_driver_constants
  USE module_machine
  USE module_tiles
  USE module_dm
  USE module_model_constants
  USE module_state_description
  USE module_data_radm2
  USE module_data_sorgam
  USE module_radm
  USE module_dep_simple
  USE module_bioemi_simple
  USE module_phot_mad
  USE module_aerosols_sorgam
  USE module_chem_utilities
  USE module_ctrans_grell
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid
   !
   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>
#include <em_i1_decl.inc>

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER                     :: ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  ips,ipe, jps,jpe, kps,kpe,    &
                                  its,ite, jts,jte, kts,kte
! ..
! .. Local Scalars ..
      INTEGER :: i,j,k,kk,nv,n, nr,ktauc, ktau,k_start,k_end
! ................................................................
! ..
!
! necessary for aerosols (module dependent)
!
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::vcsulf_old
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,ldrog) ::vdrog3
    real :: dtstepc,convfac
   INTEGER                         :: ij,l 
   INTEGER                         :: im , num_3d_m , ic , num_3d_c
   INTEGER                         :: ijds, ijde
   INTEGER                         :: ksubt
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
!  De-reference dimension information stored in the grid data structure.
#include <em_scalar_derefs.inc>
  ksubt=0
   
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )
  ktau = itimestep
   
  k_start         = kps
  k_end           = kpe

  ijds = min(ids, jds)
  ijde = max(ide, jde)

  num_3d_m        = num_moist
  num_3d_c        = num_chem
   !  Compute these starting and stopping locations for each tile and number of tiles.

  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2)
       CALL wrf_debug(15,'calling radm2 from chem_driver')
     CASE (RADM2SORG)
       CALL wrf_debug(15,'calling aerosols driver from chem_driver')
     CASE (RACMSORG)
       CALL wrf_debug(15,'calling aerosols driver from chem_driver')
     CASE DEFAULT 
                                                     
   END SELECT chem_select                              
!
!  transport part is in mixing ratio
!
      if(imicrogram == 1)then
!    write(0,*)'change imicrogram from 1 to 0 in chem_driver'


   if ( p_so4aj >= PARAM_FIRST_SCALAR ) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,its,ite,jts,jte,kte)
  aerosol_decouple_loop :  DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)
      do l=p_so4aj,num_chem
      do j=jts,jte
      do k=k_start,k_end
      kk=min(k,kde-1)
      do i=its,ite
        chem(i,k,j,l)=chem(i,kk,j,l)*alt(i,kk,j)
      enddo
      enddo
      enddo
      enddo
    enddo aerosol_decouple_loop
    endif
    imicrogram=0
    endif

!     CALL get_mminlu( mminlu_loc )
!     if(mminlu_loc.eq.'USGS')isice=24
!     luse_typ=mminlu_loc
!
!
! make more sure , even on halo....
!
      do nv=1,num_chem
         do j=jps,min(jde-1,jpe)
            do k=kps,kpe
               do i=ips,min(ide-1,ipe)
                  chem(i,k,j,nv)=max(chem(i,k,j,nv),epsilc)
               enddo
            enddo
         enddo
      enddo
      do j=jps,min(jde-1,jpe)
         do k=kps,kpe
             do i=ips,min(ide-1,ipe)
               if(chem(i,k,j,p_nu0).lt.1.e07)then
                  chem(i,k,j,p_nu0)=1.e7
               endif
            enddo
         enddo
      enddo
!     do nv=1,num_chem
!        do j=jps,jpe
!           do i=ips,ipe
!                 chem(i,kte,j,nv)=chem(i,kte-1,j,nv)
!           enddo
!        enddo
!     enddo


      vdrog3=0.
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )
   chem_tile_loop_1: DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)
          CALL wrf_debug ( 15 , ' call chem_prep' )
         CALL chem_prep ( config_flags,                           &
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist, num_3d_m, &
                         mu_3d, rho,                             &
                         th_phy, p_phy,  u_phy, v_phy,    &
                         p8w, t_phy, t8w, z, z_at_w,             &
                         dz8w, fnm, fnp,                         &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                         )


!--- emissions
      call wrf_debug(15,'calling emissions driver')
      call emissions_driver(grid%id,ktau,dt,DX,                            &
         config_flags, stepbioe,                                          &
         gmt,julday,t_phy,moist,p8w,t8w,                                  & 
         e_bio,p_phy,chem,rho,dz8w,ne_area,                    &
         e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt,  &
         e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,e_nh3,  &
         ivgtyp,tsk,gsw,vegfra,pblh,rmol,ust,znt,xlat,xlong,z,z_at_w,      &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,                &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,                &
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,                &
         noag_grow,noag_nongrow,nononag,slai,                              &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                     &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                     &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,             &
         ids,ide, jds,jde, kds,kde,                                       &
         ims,ime, jms,jme, kms,kme,                                       &
         grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
         grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
         k_start    , min(k_end,kde-ksubt)                     )
!
! calculate photolysis rates
!
      if(ktau.eq.1.or.mod(ktau,stepphot).eq.0)then
         call wrf_debug(15,'calling photolysis driver')
         call photolysis_driver (grid%id,ktau,dt,config_flags,             &
              gmt,julday,t_phy,moist,aerwrf,p8w,t8w,p_phy,         &
              chem,rho,dz8w,xlat,xlong,z,z_at_w,                      &
              ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,      &
              ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
              ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
              ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
              aer_dry,aer_water,                                          &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
              k_start    , min(k_end,kde-ksubt)                     )
   
      endif
         if(ktau.gt.2)then
         call wrf_debug(100,'calling dry_deposition_driver')
         call dry_dep_driver(grid%id,ktau,dt,                              &
              config_flags,                                               &
              gmt,julday,t_phy,moist,p8w,t8w,                             &
              alt,p_phy,chem,rho,dz8w,exch_h,                    &
              ivgtyp,tsk,gsw,vegfra,pblh,rmol,ust,znt,xlat,xlong,z,z_at_w, &
              h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
              cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,       &
              imicrogram,                                                 &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
              k_start    , min(k_end,kde-ksubt)                     )
          endif
!
!   convective transport/wet deposition
!

         call wrf_debug(15,'calling conv transport')
         call grelldrvct(DT,ktau,DX,grid%id,config_flags,                 &
              rho,RAINCV,chem,                                        &
              U_phy,V_phy,t_phy,W_2,moist,dz8w,           &
              p_phy,XLV0,CP,G,r_v,z,cu_co_ten,                  &
              ids,ide, jds,jde, kds,kde,               &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
              k_start    , min(k_end,kde-1)                     )
!
!
!
!
      kts=k_start
      kte=k_end
      if(ktau.eq.1.or.mod(ktau,stepchem).eq.0)then
      call wrf_debug(15,'calling mechanism')
        dtstepc=dt*float(stepchem)
        ktauc=max(ktau/stepchem,1)
        if(ktau.eq.1)dtstepc=dt
!
! chemical mechanisms
!
        call mechanism_driver(grid%id,ktauc,dtstepc,config_flags,            &
              gmt,julday,t_phy,moist,p8w,t8w,                          &
              p_phy,chem,rho,dz8w,z,z_at_w,vdrog3,vcsulf_old,&
              ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
              ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,     &
              ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,          &
              ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob, &
               addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp, &
               xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,&
               ketp,olnd,                                              &
              ids,ide, jds,jde, kds,kde,                               &
              ims,ime, jms,jme, kms,kme,                               &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
              k_start    , min(k_end,kde-ksubt)                     )
!
!   now do aerosols
!
      call wrf_debug(15,'calling aerosols')
      call aerosols_driver (grid%id,ktauc,config_flags,dtstepc,               &
               alt,t_phy,moist,aerwrf,p8w,t8w,                              &
               p_phy,chem,rho,dz8w,z,z_at_w,                 &
               h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2, &
               cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,    &
               e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,        &
               e_so4i,e_so4j,e_no3i,e_no3j,                             &
               vdrog3,aer_dry,aer_water,                                &
               imicrogram,                                              &
               ids,ide, jds,jde, kds,kde,                               &
               ims,ime, jms,jme, kms,kme,                               &
               grid%i_start(ij), min(grid%i_end(ij),ide-1),      &
               grid%j_start(ij), min(grid%j_end(ij),jde-1),      &
               k_start    , min(k_end,kde-ksubt)                     )
!
! sum up for pm2_5 and pm10 output, units here should still be ug/m3
!
      call wrf_debug(15,'calculating pm')
        do j=grid%j_start(ij),grid%j_end(ij)
           do k=k_start,k_end
              do i=grid%i_start(ij),grid%i_end(ij)
                pm2_5(i,k,j)=0.
              enddo
           enddo
        enddo
        do nv=p_so4aj,p_p25i
             do j=jts,jte
                do k=kts,kte
                   kk=min(k,kte-1)
                   do i=its,ite
                      pm2_5(i,k,j)=pm2_5(i,k,j)+chem(i,kk,j,nv)
                   enddo
                enddo
             enddo
          enddo
      call wrf_debug(15,'calculating pm 2')
        do j=jts,jte
            do k=kts,kte
               kk=min(k,kte-1)
               do i=its,ite
                      pm10(i,k,j)= pm2_5(i,k,j)                      &
                                  +chem(i,kk,j,p_antha)     &
                                  +chem(i,kk,j,p_soila)    &
                                  +chem(i,kk,j,p_seas)
               enddo
            enddo
        enddo
      endif
! Fill top level to prevent spurious interpolation results (no extrapolation)
! should this be done on halo too????
      do nv=1,num_chem
         do j=jts,jte
            do i=its,ite
                  chem(i,kte,j,nv)=chem(i,kte-1,j,nv)
            enddo
         enddo
      enddo
      call wrf_debug(15,'done tileloop in chem_driver')
   END DO chem_tile_loop_1
!
!  if aerosols were not called, make sure units are in micrograms/m3 for
!  output, just in case.....
!
      if(imicrogram == 0)then
!    write(0,*)'change imicrogram from 0 to 1 in chem_driver'

   if ( p_so4aj >= PARAM_FIRST_SCALAR ) then
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij ,its,ite,jts,jte,kte)
  aerosol_couple_loop :  DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)
      do l=p_so4aj,num_chem
      do j=jts,jte
      do k=k_start,k_end
      kk=min(k,kde-1)
      do i=its,ite
        chem(i,k,j,l)=chem(i,kk,j,l)/alt(i,kk,j)
      enddo
      enddo
      enddo
      enddo
    enddo aerosol_couple_loop
    endif
    imicrogram=1
    endif


    END subroutine chem_driver
