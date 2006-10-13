!WRF:MODEL_LAYER:CHEMICS
!
#if ( NMM_CORE == 1 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!NCEP_MESO:MEDIATION_LAYER:SOLVER
!
!-----------------------------------------------------------------------
#include "../dyn_nmm/nmm_loop_basemacros.h"
#include "../dyn_nmm/nmm_loop_macros.h"
!-----------------------------------------------------------------------
#endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    subroutine chem_driver ( grid , config_flags , &
!
#if ( EM_CORE == 1 )
#include "em_dummy_args.inc"
#endif

#if ( NMM_CORE == 1 )
#include "nmm_dummy_args.inc"
#endif
!
                 )
!----------------------------------------------------------------------
  USE module_domain
  USE module_configure
#if ( EM_CORE == 1 )
  USE module_driver_constants
  USE module_machine
  USE module_tiles
#endif
  USE module_dm
  USE module_model_constants
  USE module_state_description
#if ( NMM_CORE == 1 )
  USE MODULE_PHYSICS_CALLS 
#endif
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
#if ( EM_CORE == 1 )
#include <em_dummy_decl.inc>
#define NO_I1_OLD
#include <em_i1_decl.inc>
#endif
#if ( NMM_CORE == 1 )
#include <nmm_dummy_decl.inc>
#endif

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER                     :: ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  ips,ipe, jps,jpe, kps,kpe,    &
                                  its,ite, jts,jte, kts,kte
! ..
! .. Local Scalars ..
      INTEGER :: i,j,k,numgas,nv,n, nr,ktauc, ktau,k_start,k_end,idf,jdf,kdf

! ................................................................
! ..
!
! necessary for aerosols (module dependent)
!
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::vcsulf_old
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,ldrog) ::vdrog3


!!! rate for n2o5 hyrolysis (calculated outside the chemistry solver routines)
    real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::n2o5_het
#if ( NMM_CORE == 1 )
!   met-variables needed when using NMM
!
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) :: &
                                                              DZ8W,P_PHY,PI_PHY    &
                                                             ,rho,alt,T8W,P8W,z_nmm                 &
                                                             ,TH_PHY,T_PHY,CLFR,U_PHY,V_PHY
!
! 
      REAL,DIMENSION(grid%sm32:grid%em32-1) :: QL,TL
!
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: REXNSFC,GSW,FACTRS                &
                                        ,TOT,TSFC,XLAND,XLAT,XLONG,TSK,ust,raincv
!
      REAL :: DAYI,DPL,FICE,FRAIN,HOUR,PLYR          &
     &       ,QI,QR,QW,RADT,TIMES,WC,TDUM,wmsk,rwmsk
#endif
 

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
#if ( EM_CORE == 1 )
#include <em_scalar_derefs.inc>
#endif
#if ( NMM_CORE == 1 )
#define COPY_IN
#include <nmm_scalar_derefs.inc>
#endif

! Number of levels to exclude from the chem calculations counting from
! the model top.
  ksubt=0
! return
   
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


#if ( EM_CORE == 1 )

   !  Compute these starting and stopping locations for each tile and number of tiles.
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )

#endif
#if ( NMM_CORE == 1 )
!***  IN NMM SET CONTROLS FOR TILES TO PATCHES
!
!-----------------------------------------------------------------------
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)

#endif


  chem_minval = epsilc !chem_minval can be case dependant and set below...
   chem_select: SELECT CASE(config_flags%chem_opt)
     CASE (RADM2)
       CALL wrf_debug(15,'calling radm2 from chem_driver')
       haveaer = .false.
     CASE (RADM2_KPP)
       CALL wrf_debug(15,'calling radm2_kpp from chem_driver')
       haveaer = .false.
     CASE (RADM2SORG)
       CALL wrf_debug(15,'calling radm2sorg aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RADM2SORG_KPP)
       CALL wrf_debug(15,'calling radm2sorg aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RACM)
       CALL wrf_debug(15,'calling racm from chem_driver')
       haveaer = .false.
     CASE (RACM_KPP)
       CALL wrf_debug(15,'calling racm_kpp from chem_driver')
       haveaer = .false.
     CASE (RACM_MIM_KPP)
       CALL wrf_debug(15,'calling racm_mim_kpp from chem_driver')
       haveaer = .false.
     CASE (RACMSORG)
       CALL wrf_debug(15,'calling racmsorg aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RACMSORG_KPP)
       CALL wrf_debug(15,'calling racmsorg_kpp aerosols driver from chem_driver')
       haveaer = .false.
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

!
!
!
#if ( NMM_CORE == 1 )
! this should be in seperate routine!!!!!!
      GRID%SIGMA=1
      HYDRO=.FALSE.
      its=max(its,MYIS1)
      jts=max(jts,MYJS2)
      ite=min(ite,MYIE1)
      jte=min(jte,MYJE2)
      DO J=jts,jte
      DO I=its,ite
!
!       PDSL=PD(I,J)*RES(I,J)
!-----------------------------------------------------------------------
!*** LONG AND SHORTWAVE FLUX AT GROUND SURFACE
!-----------------------------------------------------------------------
        IF(CZMEAN(I,J)>0.) THEN
          FACTRS(I,J)=CZEN(I,J)/CZMEAN(I,J)
        ELSE
!         if(i.eq.MYIS1.and.j.eq.MYJS2)write(0,*)'csmean2'
          FACTRS(I,J)=0.
        ENDIF
!         if(i.eq.MYIS1.and.j.eq.MYJS2)write(0,*)'gsw'
        GSW(I,J)=(RSWIN(I,J)-RSWOUT(I,J))*HBM2(I,J)*FACTRS(I,J)
        P8W(I,KTE+1,J)=PT
        XLAT(I,J)=GLAT(I,J)/DEGRAD
        XLONG(I,J)=GLON(I,J)/DEGRAD
        XLAND(I,J)=SM(I,J)+1.
        PSFC(i,j)=PD(I,J)+PDTOP+PT
        UST(I,J)=USTAR(I,J)
        REXNSFC(I,J)=(PSFC(i,j)*1.E-5)**CAPA
        TSFC(I,J)=THS(I,J)*REXNSFC(I,J)
        TSK(I,J)=TSFC(I,J)
!       if(i.eq.its.and.j.eq.jts)then
!        write(0,*)'***********************************'
!        write(0,*)PSFC(i,j),CAPA,THS(I,J),REXNSFC(I,J),TSFC(I,J),PD(I,J)+PDTOP+PT
!       endif
        T8W(I,1,J)=TSFC(I,J)
        P8W(I,KTS,J)=ETA1(KTS)*PDTOP+ETA2(KTS)*PDSL(i,j)+PT
!         if(i.eq.MYIS1.and.j.eq.MYJS2)write(0,*)'p8w'
!
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
        DO K=KTS,KTE
           DPL=DETA1(K)*PDTOP+DETA2(K)*PDSL(i,j)
          QL(K)=AMAX1(Q(I,K,J),EPSQ)
          PLYR=AETA1(K)*PDTOP+AETA2(K)*PDSL(i,j)+PT
          TL(K)=T(I,K,J)
!
! here alt is inverse density!
!
          RHO(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          ALT(I,K,J)=1./RHO(i,k,j)
          T_PHY(I,K,J)=TL(K)
          moist(I,K,J,P_QV)=QL(K)/(1.-QL(K))
!         TH_PHY(I,K,J)=TL(K)*(1.E5/PLYR)**CAPA
          P8W(I,K+1,J)=ETA1(K+1)*PDTOP+ETA2(K+1)*PDSL(i,j)+PT
          P_PHY(I,K,J)=PLYR
          PI_PHY(I,K,J)=(PLYR*1.E-5)**CAPA
          DZ8W(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
! the z-level fix
          IF(K<KTE)Z_NMM(I,K,J)=.5*(Z(I,K,J)+Z(I,K+1,J))
         ENDDO
! 
        DO K=KTS+1,KTE
          T8W(I,K,J)=0.5*(TL(K-1)+TL(K))
        ENDDO
        T8W(I,KTE+1,J)=-1.E20
        Z_NMM(I,KTE,J)=Z(I,KTE,J)
!
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
!
!***  COMPUTE VELOCITY COMPONENTS AT MASS POINTS
!
!-----------------------------------------------------------------------
!$omp parallel do                                                       &
!$omp& private(i,j,k,rwmsk,wmsk)
      DO J=MYJS1_P1,MYJE1_P1
!
        DO K=KTS,KTE
          DO I=MYIS_P1,MYIE_P1
            WMSK=VTM(I+IHE(J),K,J)+VTM(I+IHW(J),K,J)                    &
     &          +VTM(I,K,J+1)+VTM(I,K,J-1)
            IF(WMSK>0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(U(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +U(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +U(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +U(I,K,J-1)*VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(V(I+IHE(J),K,J)*VTM(I+IHE(J),K,J)           &
     &                         +V(I+IHW(J),K,J)*VTM(I+IHW(J),K,J)       &
     &                         +V(I,K,J+1)*VTM(I,K,J+1)                 &
     &                         +V(I,K,J-1)*VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
        ENDDO
      ENDDO



#endif
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
      case (RADM2SORG, RADM2SORG_KPP,RACMSORG,RACMSORG_KPP)
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
#if ( EM_CORE == 1 )

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )
   chem_tile_loop_1: DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids+1)
       ite = min(grid%i_end(ij),ide-2)
       jts = max(grid%j_start(ij),jds+1)
       jte = min(grid%j_end(ij),jde-2)
      kts=k_start
      kte=k_end
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
#endif
!        write(0,*)'1',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)

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
         u10,v10,ivgtyp,gsw,vegfra,rmol,ust,znt,                          &
         xland,xlat,xlong,                                                &
#if ( EM_CORE == 1 )
         z_at_w,                                                          &
#endif
#if ( NMM_CORE == 1 )
         z,                                                               &
#endif
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
         its,ite,jts,jte,                                                 &
         k_start         , min(k_end,kde-ksubt)                           )
!        write(0,*)'2',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)

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
              chem,rho,dz8w,xlat,xlong,                                   &
#if ( EM_CORE == 1 )
         z_at_w,                                                          &
#endif
#if ( NMM_CORE == 1 )
         z,                                                               &
#endif
              gd_cloud,gd_cloud2,                                         &
              ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,    &
              ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,        &
              ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,             &
              ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,    &
              ph_n2o5,ph_o2,                                              &
              tauaer1,tauaer2,tauaer3,tauaer4,                            &
              gaer1,gaer2,gaer3,gaer4,                                    &
              waer1,waer2,waer3,waer4,                                    &
              pm2_5_dry,pm2_5_water,uvrad,                                &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
         its,ite,jts,jte,                                                 &
              k_start         , min(k_end,kde-ksubt)                      )
!        write(0,*)'3',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)
   
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
                 ivgtyp,tsk,gsw,vegfra,pblh,rmol,ust,znt,xlat,xlong,         &
#if ( EM_CORE == 1 )
                 z,z_at_w,&
#endif
#if ( NMM_CORE == 1 )
                 z_nmm,z,&
#endif
                 h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
                 cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,dep_vel_o3,       &
                 e_co,config_flags%kemit,numgas,                             &
                 ids,ide, jds,jde, kds,kde,                                  &
                 ims,ime, jms,jme, kms,kme,                                  &
         its,ite,jts,jte,                                                 &
                 k_start         , min(k_end,kde-ksubt)                      )
         end if
!        write(0,*)'4',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)

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
        call wrf_debug(15,'calling conv transport')
        call grelldrvct(grid%DT,ktau,grid%DX,grid%id,config_flags,       &
             rho,RAINCV,chem,                                            &
             U_phy,V_phy,t_phy,moist,dz8w,                               &
             p_phy,XLV,CP,G,r_v,                                         &
#if ( EM_CORE == 1 )
             z_at_w,&
#endif
#if ( NMM_CORE == 1 )
             z,&
#endif
             cu_co_ten,                            &
             num_chem,                                                     &
             ids,ide, jds,jde, kds,kde,                                  &
             ims,ime, jms,jme, kms,kme,                                  &
         its,ite,jts,jte,                                                 &
             k_start    , min(k_end,kde-1)                               )
!        write(0,*)'5',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)
!
!
!
!
n2o5_het=0.
! Calculate rate of n2o5 hydrolysis 
       call wrf_debug(15,'calling calc_het_n2o5')



!
! For the chemistry tracer mode, only emissions and vertical mixing are done.
! So, finish any remaining tiles and then skip to the end of chem_driver.
!
      kts=k_start
      kte=k_end
      if((ktau.eq.1.or.mod(ktau,stepchem).eq.0)    &
           .and. config_flags%chem_opt /= CHEM_TRACER) then
        dtstepc=grid%dt*float(stepchem)
        ktauc=max(ktau/stepchem,1)
        if(ktau.eq.1)dtstepc=grid%dt
!
! chemical mechanisms
!
        call mechanism_driver(grid%id,ktau,grid%dt,ktauc,dtstepc,config_flags, &
              grid%gmt,grid%julday,t_phy,moist,p8w,t8w,                   &
              p_phy,chem,rho,dz8w,                                        &
#if ( EM_CORE == 1 )
              z,z_at_w,&
#endif
#if ( NMM_CORE == 1 )
              z_nmm,z,&
#endif
              vdrog3,vcsulf_old,             &
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
         its,ite,jts,jte,                                                 &
              k_start         , min(k_end,kde-ksubt)                      )



!        write(0,*)'6',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)
!cms++
!


#ifdef WRF_KPP
   CALL wrf_debug(15,'calling kpp_mechanism_driver')


CALL kpp_mechanism_driver(   chem,                        &
   grid%id,dtstepc,config_flags,                         & 
   p_phy,t_phy,rho,moist,                                &
   vdrog3, ldrog,                                        &
   n2o5_het,                                             &
!                    
#include <call_to_kpp_mech_drive.inc>
!
     ids,ide, jds,jde, kds,kde,                          &
     ims,ime, jms,jme, kms,kme,                          &
         its,ite,jts,jte,                                                 &
     k_start    , min(k_end,kde-ksubt)                   )  


!cms--

!

#endif



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
              p_phy,chem,rho,dz8w,                                        &
#if ( EM_CORE == 1 )
              z,z_at_w,                                                   &
#endif
#if ( NMM_CORE == 1 )
              z_nmm,z,                                                    &
#endif
              h2oaj,h2oai,nu3,ac3,cor3,asulf,ahno3,anh3,cvaro1,cvaro2,    &
              cvalk1,cvole1,cvapi1,cvapi2,cvlim1,cvlim2,vcsulf_old,       &
              e_pm25i,e_pm25j,e_eci,e_ecj,e_orgi,e_orgj,e_pm10,           &
              e_so4i,e_so4j,e_no3i,e_no3j,                                &
              vdrog3,                                                     &
              ids,ide, jds,jde, kds,kde,                                  &
              ims,ime, jms,jme, kms,kme,                                  &
         its,ite,jts,jte,                                                 &
              k_start         , min(k_end,kde-ksubt)                      )
!        write(0,*)'7',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)

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
!        write(0,*)'8',chem(its,kts:kte,jts,p_so4ai),chem(its,kts,jts,p_o3)

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
# if ( EM_CORE == 1 ) 
   END DO chem_tile_loop_1
#endif


    END subroutine chem_driver
