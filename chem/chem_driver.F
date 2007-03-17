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
    subroutine chem_driver ( grid , config_flags   &
 
#if ( EM_CORE == 1 )
#include "em_dummy_new_args.inc"
#endif

#if ( NMM_CORE == 1 )
#include "nmm_dummy_new_args.inc"
#endif
 
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
  USE module_wetscav_driver, only: wetscav_driver
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
#include <em_dummy_new_decl.inc>
#define NO_I1_OLD
!#include <em_i1_decl_new.inc>
#endif
#if ( NMM_CORE == 1 )
#include <nmm_dummy_new_decl.inc>
#ifdef DM_PARALLEL
      INCLUDE "mpif.h"
#endif
#endif

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER                     :: ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  ips,ipe, jps,jpe, kps,kpe,    &
                                  its,ite, jts,jte, kts,kte
! ..
! .. Local Scalars ..
      INTEGER :: stepave,i,j,k,numgas,nv,n, nr,ktauc, ktau,k_start,k_end,idf,jdf,kdf

! ................................................................
! ..
!
! necessary for aerosols (module dependent)
!
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::vcsulf_old
      real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33,ldrog) ::vdrog3


!!! rate for n2o5 hyrolysis (calculated outside the chemistry solver routines)
    real, dimension(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::n2o5_het
!   met-variables needed 
!
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm32:grid%em32,grid%sm33:grid%em33) ::              &
                                                              p_phy,u_phy,v_phy                   &
                                                             ,t_phy,dz8w,t8w,p8w                  &
                                                             ,rho,rri,z_at_w,vvel,zmid
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: pbl_h
#if ( NMM_CORE == 1 )
!   met-variables needed when using NMM
! 
      REAL,DIMENSION(grid%sm32:grid%em32-1) :: QL,TL
!
      REAL,DIMENSION(grid%sm31:grid%em31,grid%sm33:grid%em33) :: REXNSFC,FACTRS                &
                                        ,TOT,TSFC
!
      REAL :: DAYI,DPL,FICE,FRAIN,HOUR,PLYR          &
     &       ,QI,QR,QW,RADT,TIMES,WC,TDUM,WMSK,RWMSK
#endif
 

      INTEGER                         :: ij 
      INTEGER                         :: im , num_3d_m , ic , num_3d_c
      INTEGER                         :: ijds, ijde
      INTEGER                         :: ksubt

      REAL :: chem_minval, dtstepc

      INTEGER :: numgas_aqfrac = 0    ! last dimension of gas_aqfrac
      REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: gas_aqfrac
                                      ! fraction of gas that is in cloud water

      LOGICAL :: haveaer
! ..
! .. Intrinsic Functions ..
      INTRINSIC max, min
! ..

! Number of levels to exclude from the chem calculations counting from
! the model top.
  ksubt=0
  stepave=1800./grid%dt
  CALL get_ijk_from_grid (  grid ,                   &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            ips, ipe, jps, jpe, kps, kpe    )
  ktau = grid%itimestep
!initialize
!
#if ( NMM_CORE == 1 )
!***  IN NMM SET CONTROLS FOR TILES TO PATCHES
!
!-----------------------------------------------------------------------
      KTAU=GRID%NMM_NTSD
      IDF=IDE-1
      JDF=JDE-1
      KDF=KDE-1
      ITS=IPS
      ITE=MIN(IPE,IDF)
      JTS=JPS
      JTE=MIN(JPE,JDF)
      KTS=KPS
      KTE=MIN(KPE,KDF)

#endif
  if(ktau.le.1)then
    grid%gd_cloud_a=0.
    grid%gd_cloud2_a=0.
    grid%gd_cloud_b=0.
    grid%gd_cloud2_b=0.
    grid%raincv_a=0.
    grid%raincv_b=0.
  endif
   


  num_3d_m        = num_moist
  num_3d_c        = num_chem
  numgas          = get_last_gas(config_flags%chem_opt)


#if ( EM_CORE == 1 )

   !  Compute these starting and stopping locations for each tile and number of tiles.
  CALL set_tiles ( grid , ids , ide , jds , jde , ips , ipe , jps , jpe )
  k_start         = kps
  k_end           = kpe

#endif
  ijds = min(ids, jds)
  ijde = max(ide, jde)


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
       haveaer = .true.
     CASE (RADM2SORG_KPP)
       CALL wrf_debug(15,'calling radm2sorg aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RADM2SORG_AQ)
       CALL wrf_debug(15,'calling radm2sorg_aq aerosols driver from chem_driver')
       haveaer = .true.
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
       haveaer = .true.
     CASE (RACMSORG_KPP)
       CALL wrf_debug(15,'calling racmsorg_kpp aerosols driver from chem_driver')
       haveaer = .false.
     CASE (RACMSORG_AQ)
       CALL wrf_debug(15,'calling racmsorg_aq aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ)
       CALL wrf_debug(15,'calling cbmz from chem_driver')
       haveaer = .false.
     CASE (CBMZ_BB)
       CALL wrf_debug(15,'calling cbmz_bb from chem_driver')
       haveaer = .false.
     CASE (CBMZ_MOSAIC_4BIN)
       CALL wrf_debug(15,'calling cbmz_mosaic_4bin aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ_MOSAIC_8BIN)
       CALL wrf_debug(15,'calling cbmz_mosaic_8bin aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ_MOSAIC_4BIN_AQ)
       CALL wrf_debug(15,'calling cbmz_mosaic_4bin_aq aerosols driver from chem_driver')
       haveaer = .true.
     CASE (CBMZ_MOSAIC_8BIN_AQ)
       CALL wrf_debug(15,'calling cbmz_mosaic_8bin_aq aerosols driver from chem_driver')
       haveaer = .true.
    CASE (CHEM_TRACER)
       CALL wrf_debug(15,'tracer mode: only doing emissions and dry dep in chem_driver')
     CASE DEFAULT
       CALL wrf_debug(15,'calling chem_opt=? from chem_driver')
   END SELECT chem_select                              

!
!
!
#if ( NMM_CORE == 1 )
  k_start         = kts
  k_end           = kte
! this should be in seperate routine!!!!!!
      GRID%SIGMA=1
      grid%nmm_HYDRO=.FALSE.
      its=max(its,MYIS1)
      jts=max(jts,MYJS2)
      ite=min(ite,MYIE1)
      jte=min(jte,MYJE2)
      DO J=jts,jte
      DO I=its,ite
        pbl_h(i,j)=grid%nmm_pblh(i,j)
!
!       PDSL=PD(I,J)*RES(I,J)
!-----------------------------------------------------------------------
!*** LONG AND SHORTWAVE FLUX AT GROUND SURFACE
!-----------------------------------------------------------------------
        IF(grid%nmm_CZMEAN(I,J)>0.) THEN
          FACTRS(I,J)=grid%nmm_CZEN(I,J)/grid%nmm_CZMEAN(I,J)
        ELSE
          FACTRS(I,J)=0.
        ENDIF
        grid%GSW(I,J)=(grid%nmm_RSWIN(I,J)-grid%nmm_RSWOUT(I,J))*grid%nmm_HBM2(I,J)*FACTRS(I,J)
        P8W(I,KTE+1,J)=grid%nmm_PT
        grid%XLAT(I,J)=grid%nmm_GLAT(I,J)/DEGRAD
        grid%XLONG(I,J)=grid%nmm_GLON(I,J)/DEGRAD
        grid%XLAND(I,J)=grid%nmm_SM(I,J)+1.
        grid%PSFC(i,j)=grid%nmm_PD(I,J)+grid%nmm_PDTOP+grid%nmm_PT
        grid%UST(I,J)=grid%nmm_USTAR(I,J)
        REXNSFC(I,J)=(grid%PSFC(i,j)*1.E-5)**CAPA
        TSFC(I,J)=grid%nmm_THS(I,J)*REXNSFC(I,J)
        grid%TSK(I,J)=TSFC(I,J)

        T8W(I,1,J)=TSFC(I,J) 
        P8W(I,KTS,J)=grid%nmm_ETA1(KTS)*grid%nmm_PDTOP+grid%nmm_ETA2(KTS)*grid%nmm_PDSL(i,j)+grid%nmm_PT
!       
!-----------------------------------------------------------------------
!***  FILL THE SINGLE-COLUMN INPUT
!-----------------------------------------------------------------------
!
        z_at_w(i,kts,j)=grid%nmm_fis(i,j)/g
        DO K=KTS,KTE
           vvel(i,k,j)=grid%nmm_w(i,k,j)
           DPL=grid%nmm_DETA1(K)*grid%nmm_PDTOP+grid%nmm_DETA2(K)*grid%nmm_PDSL(i,j)
          QL(K)=AMAX1(grid%nmm_Q(I,K,J),EPSQ)
          PLYR=grid%nmm_AETA1(K)*grid%nmm_PDTOP+grid%nmm_AETA2(K)*grid%nmm_PDSL(i,j)+grid%nmm_PT
          TL(K)=grid%nmm_T(I,K,J)
!
! here rri is inverse density!
!
          RHO(I,K,J)=PLYR/(R_D*TL(K)*(1.+P608*QL(K)))
          RRI(I,K,J)=1./RHO(i,k,j)
          T_PHY(I,K,J)=TL(K)
          moist(I,K,J,P_QV)=QL(K)/(1.-QL(K))
          P8W(I,K+1,J)=grid%nmm_ETA1(K+1)*grid%nmm_PDTOP+grid%nmm_ETA2(K+1)*grid%nmm_PDSL(i,j)+grid%nmm_PT
          P_PHY(I,K,J)=PLYR
          DZ8W(I,K,J)=TL(K)*(P608*QL(K)+1.)*R_D                           &
     &                 *(P8W(I,K,J)-P8W(I,K+1,J))                       &
     &                 /(P_PHY(I,K,J)*G)
          if(K.gt.kts)then
            Z_AT_W(i,k,j)=Z_AT_W(I,k-1,j)+DZ8W(I,K-1,J)
            ZMID(I,K-1,J)=.5*(Z_AT_W(I,K-1,J)+Z_AT_W(I,K,J))
          endif

         ENDDO
!         
        DO K=KTS+1,KTE
          T8W(I,K,J)=0.5*(TL(K-1)+TL(K))
        ENDDO 
        T8W(I,KTE+1,J)=-1.E20
        ZMID(I,KTE,J)=Z_AT_W(I,KTE,J)
!         
      ENDDO
      ENDDO
!-----------------------------------------------------------------------
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
            WMSK=grid%nmm_VTM(I+grid%nmm_IHE(J),K,J)+grid%nmm_VTM(I+grid%nmm_IHW(J),K,J)                    &
     &          +grid%nmm_VTM(I,K,J+1)+grid%nmm_VTM(I,K,J-1)
            IF(WMSK>0.)THEN
              RWMSK=1./WMSK
              U_PHY(I,K,J)=(grid%nmm_U(I+grid%nmm_IHE(J),K,J)*grid%nmm_VTM(I+grid%nmm_IHE(J),K,J)           &
     &                     +grid%nmm_U(I+grid%nmm_IHW(J),K,J)*grid%nmm_VTM(I+grid%nmm_IHW(J),K,J)       &
     &                     +grid%nmm_U(I,K,J+1)*grid%nmm_VTM(I,K,J+1)                 &
     &                     +grid%nmm_U(I,K,J-1)*grid%nmm_VTM(I,K,J-1))*RWMSK
              V_PHY(I,K,J)=(grid%nmm_V(I+grid%nmm_IHE(J),K,J)*grid%nmm_VTM(I+grid%nmm_IHE(J),K,J)           &
     &                     +grid%nmm_V(I+grid%nmm_IHW(J),K,J)*grid%nmm_VTM(I+grid%nmm_IHW(J),K,J)       &
     &                     +grid%nmm_V(I,K,J+1)*grid%nmm_VTM(I,K,J+1)                 &
     &                     +grid%nmm_V(I,K,J-1)*grid%nmm_VTM(I,K,J-1))*RWMSK
            ELSE
              U_PHY(I,K,J)=0.
              V_PHY(I,K,J)=0.
            ENDIF
          ENDDO
        ENDDO
      ENDDO     
            
              
              
#endif

      do nv=1,num_chem
         do j=jps,jpe
            do k=kps,kpe
               do i=ips,ipe
                  chem(i,k,j,nv)=max(chem(i,k,j,nv),chem_minval)
               enddo
            enddo
         enddo
      enddo
      select case (config_flags%chem_opt)
      case (RADM2SORG, RADM2SORG_KPP,RACMSORG,RACMSORG_KPP)
         do j=jps,jpe
            do k=kps,kpe
               do i=ips,ipe
                  if(chem(i,k,j,p_nu0).lt.1.e07) then
                     chem(i,k,j,p_nu0)=1.e7
                  endif
               enddo
            enddo
         enddo
      end select


      vdrog3=0.
#if ( EM_CORE == 1 )
      do j=jps,min(jde-1,jpe)
         do k=kps,kpe
            do i=ips,min(ide-1,ipe)
              rri(i,k,j)=grid%em_alt(i,k,j)
              vvel(i,k,j)=grid%em_w_2(i,k,j)
              zmid(i,k,j)=grid%em_z(i,k,j)
            enddo
         enddo
      enddo
      do j=jps,min(jde-1,jpe)
         do i=ips,min(ide-1,ipe)
            pbl_h(i,j)=grid%pblh(i,j)
         enddo
      enddo

     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, its, ite, jts, jte )
   chem_tile_loop_1: DO ij = 1 , grid%num_tiles
       its = max(grid%i_start(ij),ids+1)
       ite = min(grid%i_end(ij),ide-2)
       jts = max(grid%j_start(ij),jds+1)
       jte = min(grid%j_end(ij),jde-2)
       kts=k_start
       kte=k_end
#endif
! 
! no time average available in first half hour
!   
       if( config_flags%chem_conv_tr>0)then
         if(ktau.le.stepave)then
           do j=jts,jte
                do i=its,ite
                     grid%raincv_b(i,j)=grid%raincv(i,j)
                enddo
           enddo
          endif
! 
! build time average, and stored in raincv_b to be used by convective transport routine
!   
          if(mod(ktau,stepave).ne.0)then
           do j=jts,jte
                do i=its,ite
                   grid%raincv_a(i,j)=grid%raincv_a(i,j)+grid%raincv(i,j)
                enddo
           enddo
          else if(mod(ktau,stepave).eq.0)then
           do j=jts,jte
                do i=its,ite
                   grid%raincv_b(i,j)=grid%raincv_a(i,j)/float(stepave)
                   grid%raincv_a(i,j)=0.
                enddo
           enddo
         endif
       endif   ! chem_conv_tr
!
! do the same for convwective parameterization cloud water mix ratio, 
! currently only for cu_physics=3, used by both photolysis and atmospheric radiation
!
       if( config_flags%cu_physics == 3 )then
         if(ktau.le.stepave)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                          grid%gd_cloud_b(i,k,j)=grid%gd_cloud(i,k,j)
                          grid%gd_cloud2_b(i,k,j)=grid%gd_cloud2(i,k,j)
                     enddo
                enddo
           enddo
         endif   ! stepave
!
! 
!
          if(mod(ktau,stepave).ne.0)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                       grid%gd_cloud_a(i,k,j)=grid%gd_cloud_a(i,k,j)+grid%gd_cloud(i,k,j)
                       grid%gd_cloud2_a(i,k,j)=grid%gd_cloud2_a(i,k,j)+grid%gd_cloud2(i,k,j)
                     enddo
                enddo
           enddo
          else if(mod(ktau,stepave).eq.0)then
           do j=jts,jte
                do k=kts,kte
                     do i=its,ite
                       grid%gd_cloud_b(i,k,j)=grid%gd_cloud_a(i,k,j)/float(stepave)
                       grid%gd_cloud_a(i,k,j)=0.
                       grid%gd_cloud2_b(i,k,j)=grid%gd_cloud2_a(i,k,j)/float(stepave)
                       grid%gd_cloud2_a(i,k,j)=0.
                     enddo
                enddo
           enddo
          endif !stepave
       endif ! cu_physics
!
!
#if ( EM_CORE == 1 )
         CALL wrf_debug ( 15 , ' call chem_prep' )
         CALL chem_prep ( config_flags,                                               &
                         grid%em_u_2, grid%em_v_2, grid%em_p, grid%em_pb,             &
                         grid%em_alt,grid%em_ph_2, grid%em_phb, grid%em_t_2,          &
                         moist, num_3d_m, rho,                                        &
                         p_phy,  u_phy, v_phy,                                        &
                         p8w, t_phy, t8w, grid%em_z, z_at_w,                          &
                         dz8w, grid%em_fnm, grid%em_fnp,                              &
                         ids, ide, jds, jde, kds, kde,                                &
                         ims, ime, jms, jme, kms, kme,                                &
                         its,ite,jts,jte,                                             &
                         k_start, k_end                                               )
#endif

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and.                          &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and.                          &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,"calling chem_dbg at top of chem_driver")
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit                                                     )
       end if
#endif

!--- emissions
    if(config_flags%emiss_inpt_opt > 0)then
      call wrf_debug(15,'calling emissions driver')
      call emissions_driver(grid%id,ktau,grid%dt,grid%DX,                             &
         config_flags, grid%stepbioe,                                                 &
         grid%gmt,grid%julday,rri,t_phy,moist,p8w,t8w,                                &
         grid%e_bio,p_phy,chem,rho,dz8w,grid%ne_area,                                 &
         grid%e_iso,grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,             &
         grid%e_hc5,grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                      &
         grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,          &
         grid%e_ora2,grid%e_pm25,grid%e_pm10,grid%e_nh3,                              &
         grid%e_pm25i,grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,                 &
         grid%e_orgj,grid%e_no2,grid%e_ch3oh,                                         &
         grid%e_c2h5oh,grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,               &
         grid%e_orgc,grid%e_ecc,                                                      &
         grid%u10,grid%v10,grid%ivgtyp,grid%gsw,grid%vegfra,grid%rmol,                &
         grid%ust,grid%znt,                                                           &
         grid%xland,grid%xlat,grid%xlong,                                             &
         z_at_w,                                                                      &
         grid%sebio_iso,grid%sebio_oli,grid%sebio_api,grid%sebio_lim,                 &
         grid%sebio_xyl,grid%sebio_hc3,grid%sebio_ete,grid%sebio_olt,                 &
         grid%sebio_ket,grid%sebio_ald,grid%sebio_hcho,grid%sebio_eth,                &
         grid%sebio_ora2,grid%sebio_co,grid%sebio_nr,                                 &
         grid%noag_grow,grid%noag_nongrow,grid%nononag,grid%slai,                     &
         grid%ebio_iso,grid%ebio_oli,grid%ebio_api,grid%ebio_lim,grid%ebio_xyl,       &
         grid%ebio_hc3,grid%ebio_ete,grid%ebio_olt,grid%ebio_ket,grid%ebio_ald,       &
         grid%ebio_hcho,grid%ebio_eth,grid%ebio_ora2,grid%ebio_co,grid%ebio_nr,       &
         grid%ebio_no,numgas,                                                         &
         ids,ide, jds,jde, kds,kde,                                                   &
         ims,ime, jms,jme, kms,kme,                                                   &
         its,ite,jts,jte,kts,kte)        

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and.                          &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and.                          &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after emissions_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit                                                     )
       end if
#endif
      endif

!
! calculate photolysis rates
!o
!     print *,'stepphot = ',grid%stepphot
      if((ktau.eq.1 .or. mod(ktau,grid%stepphot).eq.0)                                &
           .and. config_flags%chem_opt /= CHEM_TRACER) then
         call wrf_debug(15,'calling photolysis driver')
         call photolysis_driver (grid%id,ktau,grid%dt,config_flags,haveaer,           &
              grid%gmt,grid%julday,t_phy,moist,grid%aerwrf,p8w,t8w,p_phy,             &
              chem,rho,dz8w,grid%xlat,grid%xlong,                                     &
              z_at_w,                                                                 &
              grid%gd_cloud_b,grid%gd_cloud2_b,                                       &
              grid%ph_macr,grid%ph_o31d,grid%ph_o33p,grid%ph_no2,grid%ph_no3o2,       &
              grid%ph_no3o,grid%ph_hno2,grid%ph_hno3,grid%ph_hno4,grid%ph_h2o2,       &
              grid%ph_ch2or,grid%ph_ch2om,grid%ph_ch3cho,grid%ph_ch3coch3,            &
              grid%ph_ch3coc2h5,grid%ph_hcocho,grid%ph_ch3cocho,                      &
              grid%ph_hcochest,grid%ph_ch3o2h,grid%ph_ch3coo2h,grid%ph_ch3ono2,       &
              grid%ph_hcochob,grid%ph_n2o5,grid%ph_o2,                                &
              grid%tauaer1,grid%tauaer2,grid%tauaer3,grid%tauaer4,                    &
              grid%gaer1,grid%gaer2,grid%gaer3,grid%gaer4,                            &
              grid%waer1,grid%waer2,grid%waer3,grid%waer4,                            &
              grid%pm2_5_dry,grid%pm2_5_water,grid%uvrad,                             &
              ids,ide, jds,jde, kds,kde,                                              &
              ims,ime, jms,jme, kms,kme,                                              &
              its,ite,jts,jte,kts,kte)
   
#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
         if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
             (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
             (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after photolysis_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit,                                                    &
               grid%ph_macr,grid%ph_o31d,grid%ph_o33p,grid%ph_no2,grid%ph_no3o2,      &
               grid%ph_no3o,grid%ph_hno2,grid%ph_hno3,grid%ph_hno4,grid%ph_h2o2,      &
               grid%ph_ch2or,grid%ph_ch2om,grid%ph_ch3cho,grid%ph_ch3coch3,           &
               grid%ph_ch3coc2h5,grid%ph_hcocho,grid%ph_ch3cocho,                     &
               grid%ph_hcochest,grid%ph_ch3o2h,grid%ph_ch3coo2h,                      &
               grid%ph_ch3ono2,grid%ph_hcochob,grid%ph_n2o5,grid%ph_o2                &
                                                                                      )
       end if
#endif
      endif
!
! do vertical mixing with dry deposition
! 28-jun-2005 rce - added vertmix_onoff to turn vertical mixing on/off
!
      if (config_flags%vertmix_onoff>0) then
         if (ktau.gt.2) then
            call wrf_debug(15,'calling dry_deposition_driver')
            call dry_dep_driver(grid%id,ktau,grid%dt,config_flags,                    &
                 grid%gmt,grid%julday,t_phy,moist,scalar,p8w,t8w,vvel,                &
                 rri,p_phy,chem,rho,dz8w,grid%exch_h,                                 &
                 grid%cldfra, grid%cldfra_old,                                        &
	         grid%ccn1, grid%ccn2, grid%ccn3, grid%ccn4, grid%ccn5, grid%ccn6,    &
                 grid%qndropsource,grid%ivgtyp,grid%tsk,grid%gsw,grid%vegfra,pbl_h,   &
                 grid%rmol,grid%ust,grid%znt,grid%xlat,grid%xlong,                    &
                 zmid,z_at_w,                                                         &
                 grid%h2oaj,grid%h2oai,grid%nu3,grid%ac3,grid%cor3,grid%asulf,        &
                 grid%ahno3,grid%anh3,grid%cvaro1,grid%cvaro2,grid%cvalk1,grid%cvole1,&
                 grid%cvapi1,grid%cvapi2,grid%cvlim1,grid%cvlim2,grid%dep_vel_o3,     &
                 grid%e_co,config_flags%kemit,numgas,                                 &
                 ids,ide, jds,jde, kds,kde,                                           &
                 ims,ime, jms,jme, kms,kme,                                           &
                 its,ite,jts,jte,kts,kte)
!                k_start         , min(k_end,kde-ksubt)                               )
         end if

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and. &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and. &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after dry_deposition_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit,                                                    &
                                                                                      )
       end if
#endif
      end if


!
!   convective transport/wet deposition
!
!
!
     if( config_flags%cu_physics>0 .and. config_flags%chem_conv_tr>0)then
        call wrf_debug(15,'calling conv transport')
        call grelldrvct(grid%DT,ktau,grid%DX,grid%id,config_flags,                    &
             rho,grid%RAINCV_B,chem,                                                  &
             U_phy,V_phy,t_phy,moist,dz8w,                                            &
             p_phy,XLV,CP,G,r_v,                                                      &
             z_at_w,                                                                  &
             grid%cu_co_ten,                                                          &
             num_chem,                                                                &
             ids,ide, jds,jde, kds,kde,                                               &
             ims,ime, jms,jme, kms,kme,                                               &
             its,ite,jts,jte,kts,kte)
!            k_start    , min(k_end,kde-1)                                            )
     end if
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
      if((ktau.eq.1.or.mod(ktau,grid%stepchem).eq.0)                                  &
           .and. config_flags%chem_opt /= CHEM_TRACER) then
        dtstepc=grid%dt*float(grid%stepchem)
        ktauc=max(ktau/grid%stepchem,1)
        if(ktau.eq.1)dtstepc=grid%dt
        if(config_flags%gaschem_onoff>0)then
!
! chemical mechanisms
!
        call mechanism_driver(grid%id,ktau,grid%dt,ktauc,dtstepc,config_flags,        &
              grid%gmt,grid%julday,t_phy,moist,p8w,t8w,                               &
              p_phy,chem,rho,dz8w,                                                    &
              zmid,z_at_w,                                                            &
              vdrog3,vcsulf_old,                                                      &
              grid%ph_macr,grid%ph_o31d,grid%ph_o33p,grid%ph_no2,grid%ph_no3o2,       &
              grid%ph_no3o,grid%ph_hno2,grid%ph_hno3,grid%ph_hno4,grid%ph_h2o2,       &
              grid%ph_ch2or,grid%ph_ch2om,grid%ph_ch3cho,grid%ph_ch3coch3,            &
              grid%ph_ch3coc2h5,grid%ph_hcocho,grid%ph_ch3cocho,grid%ph_hcochest,     &
              grid%ph_ch3o2h,grid%ph_ch3coo2h,grid%ph_ch3ono2,grid%ph_hcochob,        &
              grid%ph_n2o5,grid%ph_o2,grid%addt,grid%addx,grid%addc,grid%etep,        &
              grid%oltp,grid%olip,grid%cslp,grid%limp,grid%hc5p,grid%hc8p,grid%tolp,  &
              grid%xylp,grid%apip,grid%isop,grid%hc3p,grid%ethp,grid%o3p,grid%tco3,   &
              grid%mo2,grid%o1d,grid%olnn,grid%rpho,grid%xo2,                         &
              grid%ketp,grid%olnd,                                                    &
              ids,ide, jds,jde, kds,kde,                                              &
              ims,ime, jms,jme, kms,kme,                                              &
              its,ite,jts,jte,kts,kte        )
          

!cms++
!


#ifdef WRF_KPP
   CALL wrf_debug(15,'calling kpp_mechanism_driver')


CALL kpp_mechanism_driver (chem,                                                      &
   grid%id,dtstepc,config_flags,                                                      &
   p_phy,t_phy,rho,moist,                                                             &
   vdrog3, ldrog,                                                                     &
   n2o5_het,                                                                          &
!
#include <call_to_kpp_mech_drive.inc>
!
   ids,ide, jds,jde, kds,kde,                                                         &
   ims,ime, jms,jme, kms,kme,                                                         &
   its,ite,jts,jte,kts,kte)
!  k_start    , min(k_end,kde-ksubt)                                                  )


!cms--
!
#endif
#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and.                          &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and.                          &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after mechanism_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit,                                                    &
               grid%ph_macr,grid%ph_o31d,grid%ph_o33p,grid%ph_no2,grid%ph_no3o2,      &
               grid%ph_no3o,grid%ph_hno2,grid%ph_hno3,grid%ph_hno4,grid%ph_h2o2,      &
               grid%ph_ch2or,grid%ph_ch2om,grid%ph_ch3cho,grid%ph_ch3coch3,           &
               grid%ph_ch3coc2h5,grid%ph_hcocho,grid%ph_ch3cocho,                     &
               grid%ph_hcochest,grid%ph_ch3o2h,grid%ph_ch3coo2h,                      &
               grid%ph_ch3ono2,grid%ph_hcochob,grid%ph_n2o5,grid%ph_o2                &
                                                                                      )
       end if
#endif
       endif



!   allocate gas_aqfrac if either cldchem or wetscav is on
        if ( (config_flags%cldchem_onoff > 0) .or.                                    &
             (config_flags%wetscav_onoff > 0) ) then
            numgas_aqfrac = max( numgas, 1 )
            allocate( gas_aqfrac( grid%sm31:grid%em31, grid%sm32:grid%em32,           &
                                  grid%sm33:grid%em33, numgas_aqfrac ) )
            gas_aqfrac(:,:,:,:) = 0.0
        end if

!
!   now do cloud chemistry
!
        if (config_flags%cldchem_onoff > 0) then

        call cloudchem_driver(                                                        &
               grid%id, ktau, ktauc, grid%dt, dtstepc, config_flags,                  &
               t_phy, p_phy, rho, rri,                                                &
               moist, grid%cldfra, grid%ph_no2,                                       &
               chem, gas_aqfrac, numgas_aqfrac,                                       &
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, kts,kte                                              )
       
       endif


!
!   now do aerosols
!
	if(config_flags%aerchem_onoff>0)then
        call aerosols_driver (grid%id,ktau,grid%dt,ktauc,config_flags,dtstepc,        &
              rri,t_phy,moist,grid%aerwrf,p8w,t8w,                                    &
              p_phy,chem,rho,dz8w,                                                    &
              zmid,z_at_w,                                                            &
              grid%h2oaj,grid%h2oai,grid%nu3,grid%ac3,grid%cor3,grid%asulf,           &
              grid%ahno3,grid%anh3,grid%cvaro1,grid%cvaro2,grid%cvalk1,grid%cvole1,   &
              grid%cvapi1,grid%cvapi2,grid%cvlim1,grid%cvlim2,vcsulf_old,             &
              grid%e_pm25i,grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,            &
              grid%e_orgj,grid%e_pm10,grid%e_so4i,grid%e_so4j,grid%e_no3i,grid%e_no3j,&
              vdrog3,                                                                 &
              ids,ide, jds,jde, kds,kde,                                              &
              ims,ime, jms,jme, kms,kme,                                              &
              its,ite,jts,jte,kts,kte)

#if (defined(CHEM_DBG_I) && defined(CHEM_DBG_J) && defined(CHEM_DBG_K))
       if( (its <= CHEM_DBG_I .and. ite >= CHEM_DBG_I) .and.                          &
           (jts <= CHEM_DBG_J .and. jte >= CHEM_DBG_J) .and.                          &
           (k_start <= CHEM_DBG_K .and. k_end >= CHEM_DBG_K)  ) then
          call wrf_debug(15,'calling chem_dbg after aerosols_driver')
          call chem_dbg(CHEM_DBG_I,CHEM_DBG_J,CHEM_DBG_K,grid%dt,ktau,                &
               dz8w,t_phy,p_phy,rho,chem,                                             &
               grid%e_so2,grid%e_no,grid%e_co,grid%e_eth,grid%e_hc3,grid%e_hc5,       &
               grid%e_hc8,grid%e_xyl,grid%e_ol2,grid%e_olt,                           &
               grid%e_oli,grid%e_tol,grid%e_csl,grid%e_hcho,grid%e_ald,grid%e_ket,    &
               grid%e_ora2,grid%e_nh3,grid%e_pm10,grid%e_pm25,grid%e_pm25i,           &
               grid%e_pm25j,grid%e_eci,grid%e_ecj,grid%e_orgi,grid%e_orgj,            &
               grid%e_no2,grid%e_ch3oh,grid%e_c2h5oh,grid%e_iso,                      &
               grid%e_so4j,grid%e_so4c,grid%e_no3j,grid%e_no3c,grid%e_orgc,grid%e_ecc,&
               ids,ide, jds,jde, kds,kde,                                             &
               ims,ime, jms,jme, kms,kme,                                             &
               its,ite, jts,jte, k_start,k_end,                                       &
               config_flags%kemit,
               grid%ph_macr,grid%ph_o31d,grid%ph_o33p,grid%ph_no2,grid%ph_no3o2,      &
               grid%ph_no3o,grid%ph_hno2,grid%ph_hno3,grid%ph_hno4,grid%ph_h2o2,      &
               grid%ph_ch2or,grid%ph_ch2om,grid%ph_ch3cho,grid%ph_ch3coch3,           &
               grid%ph_ch3coc2h5,grid%ph_hcocho,grid%ph_ch3cocho,                     &
               grid%ph_hcochest,grid%ph_ch3o2h,grid%ph_ch3coo2h,                      &
               grid%ph_ch3ono2,grid%ph_hcochob,grid%ph_n2o5,grid%ph_o2                &
                                                                                      )
       end if
#endif
       endif


!
!   now do wet removal
!
	if (config_flags%wetscav_onoff > 0) then
        call wetscav_driver (grid%id,ktau,grid%dt,ktauc,config_flags,dtstepc,         &
              rri,t_phy,moist,p8w,t8w,                                                &
              p_phy,chem,rho,grid%cldfra,                                             &
              grid%qlsink,grid%precr,grid%preci,grid%precs,grid%precg,                &
              gas_aqfrac, numgas_aqfrac,                                              &
              grid%h2oaj,grid%h2oai,grid%nu3,grid%ac3,grid%cor3,                      &
              grid%asulf,grid%ahno3,grid%anh3,grid%cvaro1,grid%cvaro2,                &
              grid%cvalk1,grid%cvole1,grid%cvapi1,grid%cvapi2,                        &
              grid%cvlim1,grid%cvlim2,                                                &
              ids,ide, jds,jde, kds,kde,                                              &
              ims,ime, jms,jme, kms,kme,                                              &
              grid%i_start(ij), min(grid%i_end(ij),ide-1),                            &
              grid%j_start(ij), min(grid%j_end(ij),jde-1),                            &
              k_start         , min(k_end,kde-ksubt)                                  )
       
       endif

       if (numgas_aqfrac > 0) then
           deallocate( gas_aqfrac )
           numgas_aqfrac = 0
       end if

end if !Chemistry time step check
!
! Sum up the aerosol mass for radiation and diagnostic purposes. Unlike
! aerosol_driver, which is called every dtchem, this must be done every
! time step because of emissions and deposition.
!
      call sum_pm_driver ( config_flags,                                              &
           rri, chem, grid%h2oaj, grid%h2oai,                                         &
           grid%pm2_5_dry, grid%pm2_5_water, grid%pm2_5_dry_ec, grid%pm10,            &
           ids,ide, jds,jde, kds,kde,                                                 &
           ims,ime, jms,jme, kms,kme,                                                 &
           its,ite, jts,jte, k_start,k_end                                            )

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
