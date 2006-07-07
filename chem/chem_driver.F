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
  USE module_input_chem_data, only:                 &
#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
                                     chem_dbg,      &
#endif
                                     get_last_gas
   IMPLICIT NONE

   !  Input data.

   TYPE(domain) , TARGET          :: grid
   !
   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>
#define NO_I1_OLD
#include <em_i1_decl.inc>

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER                     :: ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  ips,ipe, jps,jpe, kps,kpe,    &
                                  its,ite, jts,jte, kts,kte
! ..
! .. Local Scalars ..
      INTEGER :: i,j,k,numgas,nv,n, nr,ktauc, ktau,k_start,k_end

! ................................................................
! ..
!
! necessary for aerosols (module dependent)
!
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::vcsulf_old
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,ldrog) ::vdrog3

      INTEGER                         :: ij 
      INTEGER                         :: im , num_3d_m , ic , num_3d_c
      INTEGER                         :: ijds, ijde
      INTEGER                         :: ksubt

      REAL :: chem_minval, dtstepc

      LOGICAL :: haveaer
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..
!  De-reference dimension information stored in the grid data structure.
#include <em_scalar_derefs.inc>

! Number of levels to exclude from the chem calculations counting from
! the model top.
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
  numgas          = get_last_gas(config_flags%chem_opt)

   !  Compute these starting and stopping locations for each tile and number of tiles.

  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

!~Begin nesting test bypass code...
!!$return
!!$
!!$      print*,'Grid id=',grid%id
!!$     !$OMP PARALLEL DO   &
!!$     !$OMP PRIVATE ( ij, its, ite, jts, jte )
!!$      DO ij = 1 , grid%num_tiles
!!$         its = max(grid%i_start(ij),ids)
!!$         ite = min(grid%i_end(ij),ide-1)
!!$         jts = max(grid%j_start(ij),jds)
!!$         jte = min(grid%j_end(ij),jde-1)
!!$         if( grid%id == 1 ) then
!!$            print*,'Setting outer nest chem values to 15.'
!!$            write(*,'(6i15)') its,ite,          &
!!$                 k_start,                       &
!!$                 min(k_end,kde-0),              &
!!$                 jts,jte
!!$            chem( its:ite, k_start:k_end, jts:jte, 2:num_3d_c ) = 15.
!!$         else if( grid%id == 2 ) then
!!$            print*,'Setting nested domain to -10.'
!!$            write(*,'(6i15)') its,ite,          &
!!$                 k_start,                       &
!!$                 min(k_end,kde-0),              &
!!$                 jts,jte
!!$!            chem( its+1:ite-1, k_start:k_end, jts+1:jte-1, 2:num_3d_c ) = -10.  !shows rings around processors and allows to see feedback
!!$            chem( its:ite, k_start:k_end, jts:jte, 2:num_3d_c ) = -10.  !overwrite up to the edge of the processor's tile
!!$         else
!!$            call wrf_error_fatal("Unexpected grid number in chem_driver testing code")
!!$         end if
!!$      end DO
!!$      return
!~End nesting test bypass code.

  chem_minval = epsilc !chem_minval can be case dependant and set below...
   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2)
       CALL wrf_debug(15,'calling radm2 from chem_driver')
       haveaer = .false.
     CASE (RADM2SORG)
       CALL wrf_debug(15,'calling radm2sorg aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RACM)
       CALL wrf_debug(15,'calling racm from chem_driver')
       haveaer = .false.
     CASE (RACMSORG)
       CALL wrf_debug(15,'calling racmsorg aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ)
       CALL wrf_debug(15,'calling cbmz from chem_driver')
       haveaer = .false.
     CASE (CBMZ_BB)
       CALL wrf_debug(15,'calling cbmz_bb from chem_driver')
       haveaer = .false.
     CASE (CBMZ_MOSAIC_AA)
       CALL wrf_debug(15,'calling cbmz_mosaic_aa aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ_MOSAIC_BB)
       CALL wrf_debug(15,'calling cbmz_mosaic_bb aerosols driver from chem_driver')
       haveaer = .true.
    CASE (CHEM_TRACER)
       CALL wrf_debug(15,'tracer mode: only doing emissions and dry dep in chem_driver')
     CASE DEFAULT
       CALL wrf_debug(15,'calling chem_opt=??? from chem_driver')
   END SELECT chem_select                              
!     CALL get_mminlu( mminlu_loc )
!     if(mminlu_loc.eq.'USGS')isice=24
!     luse_typ=mminlu_loc
!
!
!
      do nv=1,num_chem
         do j=jps,min(jde-1,jpe)
            do k=kps,kpe
               do i=ips,min(ide-1,ipe)
                  chem(i,k,j,nv)=max(chem(i,k,j,nv),chem_minval)
               enddo
            enddo
         enddo
      enddo
      select case (config_flags%chem_opt)
      case (RADM2SORG, RACMSORG)
         do j=jps,min(jde-1,jpe)
            do k=kps,kpe
               do i=ips,min(ide-1,ipe)
                  if(chem(i,k,j,p_nu0).lt.1.e07) then
                     chem(i,k,j,p_nu0)=1.e7
                  endif
               enddo
            enddo
         enddo
      end select
      do nv=1,num_chem
         do j=jps,min(jde-1,jpe)
            do i=ips,min(ide-1,ipe)
                  chem(i,kme,j,nv)=chem(i,kme-1,j,nv)
            enddo
         enddo
      enddo


      vdrog3=0.
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )
   chem_tile_loop_1: DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids)
       ite = min(grid%i_end(ij),ide-1)
       jts = max(grid%j_start(ij),jds)
       jte = min(grid%j_end(ij),jde-1)
         CALL wrf_debug ( 15 , ' call chem_prep' )
         CALL chem_prep ( config_flags,                          &
                         mut, u_2, v_2, p, pb, alt,              &
                         ph_2, phb, t_2, tsk, moist, num_3d_m,   &
                         mu_3d, rho,                             &
                         th_phy, p_phy,  u_phy, v_phy,           &
                         p8w, t_phy, t8w, z, z_at_w,             &
                         dz8w, fnm, fnp,                         &
                         ids, ide, jds, jde, kds, kde,           &
                         ims, ime, jms, jme, kms, kme,           &
                         grid%i_start(ij), grid%i_end(ij),       &
                         grid%j_start(ij), grid%j_end(ij),       &
                         k_start, k_end                          )

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,"calling chem_dbg at top of chem_driver")
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
               dz8w,t_phy,p_phy,rho,chem,                                 &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
               e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, k_start,k_end,                           &
               config_flags%kemit                                         )
       end if
#endif

!--- emissions
      call wrf_debug(15,'calling emissions driver')
      call emissions_driver(grid%id,ktau,grid%dt,grid%DX,                 &
         config_flags, stepbioe,                                          &
         grid%gmt,grid%julday,alt,t_phy,moist,p8w,t8w,                    &
         e_bio,p_phy,chem,rho,dz8w,grid%ne_area,                          &
         e_iso,e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
         e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_pm25,e_pm10,e_nh3, &
         e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_no2,e_ch3oh,         &
         e_c2h5oh,e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,               &
         u10,v10,ivgtyp,tsk,gsw,vegfra,pblh,rmol,ust,znt,                 &
         xland,xlat,xlong,z,z_at_w,                                       &
         sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,               &
         sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,               &
         sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,               &
         noag_grow,noag_nongrow,nononag,slai,                             &
         ebio_iso,ebio_oli,ebio_api,ebio_lim,ebio_xyl,                    &
         ebio_hc3,ebio_ete,ebio_olt,ebio_ket,ebio_ald,                    &
         ebio_hcho,ebio_eth,ebio_ora2,ebio_co,ebio_nr,ebio_no,            &
         numgas,                                                          &
         ids,ide, jds,jde, kds,kde,                                       &
         ims,ime, jms,jme, kms,kme,                                       &
         grid%i_start(ij), min(grid%i_end(ij),ide-1),                     &
         grid%j_start(ij), min(grid%j_end(ij),jde-1),                     &
         k_start         , min(k_end,kde-ksubt)                           )

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after emissions_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
               dz8w,t_phy,p_phy,rho,chem,                                 &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
               e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, k_start,k_end,                           &
               config_flags%kemit                                         )
       end if
#endif
!
! calculate photolysis rates
!
      if((ktau.eq.1 .or. mod(ktau,stepphot).eq.0)  &
           .and. config_flags%chem_opt /= CHEM_TRACER) then
         call wrf_debug(15,'calling photolysis driver')
         call photolysis_driver (grid%id,ktau,grid%dt,config_flags,haveaer, &
              grid%gmt,grid%julday,t_phy,moist,aerwrf,p8w,t8w,p_phy,      &
              chem,rho,dz8w,xlat,xlong,z,z_at_w,                          &
              ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,    &
              ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
              ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
              ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
              ph_n2o5,ph_o2,                                              &
              tauaer1,tauaer2,tauaer3,tauaer4,                            &
              gaer1,gaer2,gaer3,gaer4,                                    &
              waer1,waer2,waer3,waer4,                                    &
              pm2_5_dry,pm2_5_water,                                      &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),                &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),                &
              k_start         , min(k_end,kde-ksubt)                      )
   
#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
         if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
             (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
             (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after photolysis_driver')
            call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
                 dz8w,t_phy,p_phy,rho,chem,                                 &
                 e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
                 e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
                 e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
                 e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
                 e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
                 ids,ide, jds,jde, kds,kde,                                 &
                 ims,ime, jms,jme, kms,kme,                                 &
                 its,ite, jts,jte, k_start,k_end,                           &
                 config_flags%kemit,                                        &
                 ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
                 ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,       &
                 ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,            &
                 ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,   &
                 ph_n2o5,ph_o2                                              )
       end if
#endif
      endif
!
! calculate dry deposition velocities
!
         if (ktau.gt.2) then
            call wrf_debug(15,'calling dry_deposition_driver')
            call dry_dep_driver(grid%id,ktau,grid%dt,config_flags,           &
                 grid%gmt,grid%julday,t_phy,moist,p8w,t8w,                   &
                 alt,p_phy,chem,rho,dz8w,exch_h,                             &
                 ivgtyp,tsk,gsw,vegfra,pblh,rmol,ust,znt,xlat,xlong,z,z_at_w,&
                 h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
                 cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,       &
                 e_co,config_flags%kemit,numgas,                             &
                 ids,ide, jds,jde, kds,kde,                                  &
                 ims,ime, jms,jme, kms,kme,                                  &
                 grid%i_start(ij), min(grid%i_end(ij),ide-1),                &
                 grid%j_start(ij), min(grid%j_end(ij),jde-1),                &
                 k_start         , min(k_end,kde-ksubt)                      )
         end if

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after dry_deposition_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
               dz8w,t_phy,p_phy,rho,chem,                                 &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
               e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, k_start,k_end,                           &
               config_flags%kemit                                         )
       end if
#endif
!
!   convective transport/wet deposition
!
! Turn off for now - G. Grell 14 Dec 2005
!
!       call wrf_debug(15,'calling conv transport')
!       call grelldrvct(grid%DT,ktau,grid%DX,grid%id,config_flags,       &
!            rho,RAINCV,chem,                                            &
!            U_phy,V_phy,t_phy,W_2,moist,dz8w,                           &
!            p_phy,XLV0,CP,G,r_v,z,cu_co_ten,                            &
!            numgas,                                                     &
!            ids,ide, jds,jde, kds,kde,                                  &
!            ims,ime, jms,jme, kms,kme,                                  &
!            grid%i_start(ij), min(grid%i_end(ij),ide-1),                &
!            grid%j_start(ij), min(grid%j_end(ij),jde-1),                &
!            k_start    , min(k_end,kde-1)                               )
!
!
!
!

!
! For the chemistry tracer mode, only emissions and vertical mixing are done.
! So, finish any remaining tiles and then skip to the end of chem_driver.
!
       if( config_flags%chem_opt == CHEM_TRACER ) cycle chem_tile_loop_1
!
      kts=k_start
      kte=k_end
      if(ktau.eq.1.or.mod(ktau,stepchem).eq.0)then
        dtstepc=grid%dt*float(stepchem)
        ktauc=max(ktau/stepchem,1)
        if(ktau.eq.1)dtstepc=grid%dt
!
! chemical mechanisms
!
        call mechanism_driver(grid%id,ktau,grid%dt,ktauc,dtstepc,config_flags, &
              grid%gmt,grid%julday,t_phy,moist,p8w,t8w,                   &
              p_phy,chem,rho,dz8w,z,z_at_w,vdrog3,vcsulf_old,             &
              ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,    &
              ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
              ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
              ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
              ph_n2o5,ph_o2,                                              &
              addt,addx,addc,etep,oltp,olip,cslp,limp,hc5p,hc8p,tolp,     &
              xylp,apip,isop,hc3p,ethp,o3p,tco3,mo2,o1d,olnn,rpho,xo2,    &
              ketp,olnd,                                                  &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),                &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),                &
              k_start         , min(k_end,kde-ksubt)                      )

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after mechanism_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
               dz8w,t_phy,p_phy,rho,chem,                                 &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
               e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, k_start,k_end,                           &
               config_flags%kemit                                         )
       end if
#endif
!
!   now do aerosols
!
        call aerosols_driver (grid%id,ktau,grid%dt,ktauc,config_flags,dtstepc, &
              alt,t_phy,moist,aerwrf,p8w,t8w,                             &
              p_phy,chem,rho,dz8w,z,z_at_w,                               &
              h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
              cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,       &
              e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,           &
              e_so4i,e_so4j,e_no3i,e_no3j,                                &
              vdrog3,                                                     &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),                &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),                &
              k_start         , min(k_end,kde-ksubt)                      )

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after aerosols_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,itimestep, &
               dz8w,t_phy,p_phy,rho,chem,                                 &
               e_so2,e_no,e_co,e_eth,e_hc3,e_hc5,e_hc8,e_xyl,e_ol2,e_olt, &
               e_oli,e_tol,e_csl,e_hcho,e_ald,e_ket,e_ora2,e_nh3,         &
               e_pm10,e_pm25,e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,   &
               e_no2,e_ch3oh,e_c2h5oh,e_iso,                              &
               e_so4j,e_so4c,e_no3j,e_no3c,e_orgc,e_ecc,                  &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, k_start,k_end,                           &
               config_flags%kemit,                                        &
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,       &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,            &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,   &
               ph_n2o5,ph_o2                                              )
       end if
#endif
      end if !Chemistry time step check
!
! Sum up the aerosol mass for radiation and diagnostic purposes. Unlike
! aerosol_driver, which is called every dtchem, this must be done every
! time step because of emissions and deposition.
!
      call sum_pm_driver ( config_flags,                              &
           alt, chem, h2oaj, h2oai,                                   &
           pm2_5_dry, pm2_5_water, pm2_5_dry_ec, pm10,                &
           ids,ide, jds,jde, kds,kde,                                 &
           ims,ime, jms,jme, kms,kme,                                 &
           its,ite, jts,jte, k_start,k_end                            )

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


    END subroutine chem_driver
