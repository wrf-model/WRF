#if ( NMM_CORE == 1)
MODULE module_diag_misc
CONTAINS
   SUBROUTINE diag_misc_stub
   END SUBROUTINE diag_misc_stub
END MODULE module_diag_misc
#else
!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_diag_misc
      PRIVATE :: WGAMMA
      PRIVATE :: GAMMLN
CONTAINS
   SUBROUTINE diagnostic_output_calc(                                 &
                      ids,ide, jds,jde, kds,kde,                      &
                      ims,ime, jms,jme, kms,kme,                      &
                      ips,ipe, jps,jpe, kps,kpe,                      & ! patch  dims
                      i_start,i_end,j_start,j_end,kts,kte,num_tiles   &
                     ,dpsdt,dmudt                                     &
                     ,p8w,pk1m,mu_2,mu_2m                             &
                     ,u,v, temp                                       &
                     ,raincv,rainncv,rainc,rainnc                     &
                     ,i_rainc,i_rainnc                                &
                     ,hfx,sfcevp,lh                                   &
                     ,ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC               & ! Optional
                     ,ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC               & ! Optional
                     ,ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC               & ! Optional
                     ,ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC               & ! Optional
                     ,I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC       & ! Optional
                     ,I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC       & ! Optional
                     ,I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC       & ! Optional
                     ,I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC       & ! Optional
                     ,dt,xtime,sbw,t2                                 &
                     ,diag_print                                      &
                     ,bucket_mm, bucket_J                             &
                     ,mphysics_opt                                    &
                     ,gsfcgce_hail, gsfcgce_2ice                      &
                     ,mpuse_hail                                      &
                     ,nssl_cnoh, nssl_cnohl                           &
                     ,nssl_rho_qh, nssl_rho_qhl                       &
                     ,nssl_alphah, nssl_alphahl                       &
                     ,prec_acc_c, prec_acc_nc, snow_acc_nc            &
                     ,snowncv, prec_acc_dt, curr_secs2                &
                     ,nwp_diagnostics, diagflag                       &
                     ,history_interval                                &
                     ,itimestep                                       &
                     ,u10,v10,w                                       &
                     ,wspd10max                                       &
                     ,up_heli_max                                     &
                     ,w_up_max,w_dn_max                               &
                     ,znw,w_colmean                                   &
                     ,numcolpts,w_mean                                &
                     ,grpl_max,grpl_colint,refd_max,refl_10cm         &
                     ,hail_maxk1,hail_max2d                           &
                     ,qg_curr                                         &
                     ,ng_curr,qh_curr,nh_curr,qr_curr,nr_curr         & !  Optional (gthompsn)
                     ,rho,ph,phb,g                                    &
                                                                      )
!----------------------------------------------------------------------

  USE module_dm, ONLY: wrf_dm_sum_real, wrf_dm_maxval
  USE module_state_description, ONLY :                                  &
      KESSLERSCHEME, LINSCHEME, SBU_YLINSCHEME, WSM3SCHEME, WSM5SCHEME, &
      WSM6SCHEME, ETAMPNEW, THOMPSON, THOMPSONAERO,                     &
      MORR_TWO_MOMENT, GSFCGCESCHEME, WDM5SCHEME, WDM6SCHEME,           &
      NSSL_2MOM, NSSL_2MOMCCN, NSSL_1MOM, NSSL_1MOMLFO,                 &
      MILBRANDT2MOM , CAMMGMPSCHEME, FAST_KHAIN_LYNN, FULL_KHAIN_LYNN    !,MILBRANDT3MOM, NSSL_3MOM

   IMPLICIT NONE
!======================================================================
! Definitions
!-----------
!-- DIAG_PRINT    print control: 0 - no diagnostics; 1 - dmudt only; 2 - all
!-- DT            time step (second)
!-- XTIME         forecast time
!-- SBW           specified boundary width - used later
!
!-- P8W           3D pressure array at full eta levels
!-- MU            dry column hydrostatic pressure
!-- RAINC         cumulus scheme precipitation since hour 0
!-- RAINCV        cumulus scheme precipitation in one time step (mm)
!-- RAINNC        explicit scheme precipitation since hour 0
!-- RAINNCV       explicit scheme precipitation in one time step (mm)
!-- SNOWNCV       explicit scheme snow in one time step (mm)
!-- HFX           surface sensible heat flux
!-- LH            surface latent heat flux
!-- SFCEVP        total surface evaporation
!-- U             u component of wind - to be used later to compute k.e.
!-- V             v component of wind - to be used later to compute k.e.
!-- PREC_ACC_C    accumulated convective precip over accumulation time prec_acc_dt
!-- PREC_ACC_NC   accumulated explicit precip over accumulation time prec_acc_dt
!-- SNOW_ACC_NC   accumulated explicit snow precip over accumulation time prec_acc_dt
!-- PREC_ACC_DT   precip accumulation time, default is 60 min
!-- CURR_SECS2    Time (s) since the beginning of the restart
!-- NWP_DIAGNOSTICS  = 1, compute hourly maximum fields
!-- DIAGFLAG      logical flag to indicate if this is a history output time
!-- U10, V10      10 m wind components
!-- WSPD10MAX     10 m max wind speed
!-- UP_HELI_MAX   max updraft helicity
!-- W_UP_MAX      max updraft vertical velocity
!-- W_DN_MAX      max downdraft vertical velocity
!-- W_COLMEAN     column mean vertical velocity
!-- NUMCOLPTS     no of column points
!-- GRPL_MAX      max column-integrated graupel
!-- GRPL_COLINT   column-integrated graupel
!-- REF_MAX       max derived radar reflectivity
!-- REFL_10CM     model computed 3D reflectivity
!
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- ips           start index for i in patch
!-- ipe           end index for i in patch
!-- jps           start index for j in patch
!-- jpe           end index for j in patch
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- i_start       start indices for i in tile
!-- i_end         end indices for i in tile
!-- j_start       start indices for j in tile
!-- j_end         end indices for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!-- num_tiles     number of tiles
!
!======================================================================

   INTEGER,      INTENT(IN   )    ::                             &
                                      ids,ide, jds,jde, kds,kde, &
                                      ims,ime, jms,jme, kms,kme, &
                                      ips,ipe, jps,jpe, kps,kpe, &
                                                        kts,kte, &
                                                      num_tiles

   INTEGER, DIMENSION(num_tiles), INTENT(IN) ::                  &
     &           i_start,i_end,j_start,j_end

   INTEGER,      INTENT(IN   )    ::   diag_print
   REAL,      INTENT(IN   )    ::   bucket_mm, bucket_J
   INTEGER,   INTENT(IN   )    ::   mphysics_opt
   INTEGER, INTENT(IN) :: gsfcgce_hail, gsfcgce_2ice, mpuse_hail
   REAL, INTENT(IN)    ::   nssl_cnoh, nssl_cnohl                &
                           ,nssl_rho_qh, nssl_rho_qhl            &
                           ,nssl_alphah, nssl_alphahl
   INTEGER,   INTENT(IN   )    ::   nwp_diagnostics
   LOGICAL,   INTENT(IN   )    ::   diagflag

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(IN ) ::                                       u  &
                                                    ,         v  &
                                                    ,       p8w

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN) ::           &
                                                           MU_2  &
                                                    ,   RAINNCV  &
                                                    ,    RAINCV  &
                                                    ,   SNOWNCV  &
                                                    ,       HFX  &
                                                    ,        LH  &
                                                    ,    SFCEVP  &  
                                                    ,        T2     

   REAL, DIMENSION( ims:ime , jms:jme ),                         &
          INTENT(INOUT) ::                                DPSDT  &
                                                    ,     DMUDT  &
                                                    ,    RAINNC  &
                                                    ,     RAINC  &
                                                    ,     MU_2M  &
                                                    ,      PK1M
 
   REAL,  INTENT(IN   ) :: DT, XTIME
   INTEGER,  INTENT(IN   ) :: SBW
   INTEGER, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::     &
                                                       I_RAINC,  &
                                                       I_RAINNC
   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      ACSWUPT,ACSWUPTC,ACSWDNT,ACSWDNTC,          &
                      ACSWUPB,ACSWUPBC,ACSWDNB,ACSWDNBC,          &
                      ACLWUPT,ACLWUPTC,ACLWDNT,ACLWDNTC,          &
                      ACLWUPB,ACLWUPBC,ACLWDNB,ACLWDNBC
   INTEGER, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      I_ACSWUPT,I_ACSWUPTC,I_ACSWDNT,I_ACSWDNTC,  &
                      I_ACSWUPB,I_ACSWUPBC,I_ACSWDNB,I_ACSWDNBC,  &
                      I_ACLWUPT,I_ACLWUPTC,I_ACLWDNT,I_ACLWDNTC,  &
                      I_ACLWUPB,I_ACLWUPBC,I_ACLWDNB,I_ACLWDNBC

   REAL, DIMENSION( ims:ime, jms:jme ), OPTIONAL, INTENT(INOUT) ::&
                      PREC_ACC_C, PREC_ACC_NC, SNOW_ACC_NC

   REAL, OPTIONAL, INTENT(IN)::  PREC_ACC_DT, CURR_SECS2

   INTEGER :: i,j,k,its,ite,jts,jte,ij
   INTEGER :: idp,jdp,irc,jrc,irnc,jrnc,isnh,jsnh
   INTEGER :: prfreq

   REAL              :: no_points
   REAL              :: dpsdt_sum, dmudt_sum, dardt_sum, drcdt_sum, drndt_sum
   REAL              :: hfx_sum, lh_sum, sfcevp_sum, rainc_sum, rainnc_sum, raint_sum
   REAL              :: dmumax, raincmax, rainncmax, snowhmax
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   CHARACTER*256     :: outstring
   CHARACTER*6       :: grid_str

   INTEGER, INTENT(IN) ::                                        &
                                     history_interval,itimestep

   REAL, DIMENSION( kms:kme ), INTENT(IN) ::                     &
                                                            znw

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ), INTENT(IN) ::   &
                                                              w  &
                                                          ,temp  &
                                                       ,qg_curr  &
                                                           ,rho  &
                                                     ,refl_10cm  &
                                                        ,ph,phb

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme), OPTIONAL, INTENT(IN) ::    &
                                       ng_curr, qh_curr, nh_curr        &
                                               ,qr_curr, nr_curr

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN) ::            &
                                                            u10  &
                                                           ,v10

   REAL, INTENT(IN) :: g

   REAL, DIMENSION( ims:ime , jms:jme ), INTENT(INOUT) ::        &
                                                      wspd10max  &
                                                   ,up_heli_max  &
                                             ,w_up_max,w_dn_max  &
                                    ,w_colmean,numcolpts,w_mean  &
                                          ,grpl_max,grpl_colint  &
                                         ,hail_maxk1,hail_max2d  &
                                                      ,refd_max

   REAL, DIMENSION(ims:ime,kms:kme,jms:jme):: temp_qg, temp_ng, temp_qr, temp_nr

   INTEGER :: idump

   REAL :: wind_vel
   REAL :: depth

      DOUBLE PRECISION:: hail_max
      REAL:: hail_max_sp
      DOUBLE PRECISION, PARAMETER:: thresh_conc = 0.0001d0                 ! number conc. of graupel/hail per cubic meter
      LOGICAL:: scheme_has_graupel
      INTEGER, PARAMETER:: ngbins=50
      DOUBLE PRECISION, DIMENSION(ngbins+1):: xxDx
      DOUBLE PRECISION, DIMENSION(ngbins):: xxDg, xdtg
      REAL:: xrho_g, xam_g, xbm_g, xmu_g
      REAL, DIMENSION(3):: cge, cgg
      DOUBLE PRECISION:: f_d, sum_ng, lamg, ilamg, N0_g, lam_exp, N0exp
      DOUBLE PRECISION:: lamr, N0min
      REAL:: mvd_r, xslw1, ygra1, zans1
      INTEGER:: ng, n

!-----------------------------------------------------------------
! Handle accumulations with buckets to prevent round-off truncation in long runs
! This is done every 360 minutes assuming time step fits exactly into 360 minutes
   IF(bucket_mm .gt. 0. .AND. MOD(NINT(XTIME),360) .EQ. 0)THEN
! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      IF (xtime .eq. 0.0)THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_rainnc(i,j) = 0
          i_rainc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(rainnc(i,j) .gt. bucket_mm)THEN
          rainnc(i,j) = rainnc(i,j) - bucket_mm
          i_rainnc(i,j) =  i_rainnc(i,j) + 1
        ENDIF
        IF(rainc(i,j) .gt. bucket_mm)THEN
          rainc(i,j) = rainc(i,j) - bucket_mm
          i_rainc(i,j) =  i_rainc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO

      IF (xtime .eq. 0.0 .and. bucket_J .gt. 0.0 .and. PRESENT(ACSWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_acswupt(i,j) = 0
          i_acswuptc(i,j) = 0
          i_acswdnt(i,j) = 0
          i_acswdntc(i,j) = 0
          i_acswupb(i,j) = 0
          i_acswupbc(i,j) = 0
          i_acswdnb(i,j) = 0
          i_acswdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (xtime .eq. 0.0  .and. bucket_J .gt. 0.0 .and. PRESENT(ACLWUPT))THEN
        DO j=j_start(ij),j_end(ij)
        DO i=i_start(ij),i_end(ij)
          i_aclwupt(i,j) = 0
          i_aclwuptc(i,j) = 0
          i_aclwdnt(i,j) = 0
          i_aclwdntc(i,j) = 0
          i_aclwupb(i,j) = 0
          i_aclwupbc(i,j) = 0
          i_aclwdnb(i,j) = 0
          i_aclwdnbc(i,j) = 0
        ENDDO      
        ENDDO
      ENDIF
      IF (PRESENT(ACSWUPT) .and. bucket_J .gt. 0.0)THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(acswupt(i,j) .gt. bucket_J)THEN
          acswupt(i,j) = acswupt(i,j) - bucket_J
          i_acswupt(i,j) =  i_acswupt(i,j) + 1
        ENDIF
        IF(acswuptc(i,j) .gt. bucket_J)THEN
          acswuptc(i,j) = acswuptc(i,j) - bucket_J
          i_acswuptc(i,j) =  i_acswuptc(i,j) + 1
        ENDIF
        IF(acswdnt(i,j) .gt. bucket_J)THEN
          acswdnt(i,j) = acswdnt(i,j) - bucket_J
          i_acswdnt(i,j) =  i_acswdnt(i,j) + 1
        ENDIF
        IF(acswdntc(i,j) .gt. bucket_J)THEN
          acswdntc(i,j) = acswdntc(i,j) - bucket_J
          i_acswdntc(i,j) =  i_acswdntc(i,j) + 1
        ENDIF
        IF(acswupb(i,j) .gt. bucket_J)THEN
          acswupb(i,j) = acswupb(i,j) - bucket_J
          i_acswupb(i,j) =  i_acswupb(i,j) + 1
        ENDIF
        IF(acswupbc(i,j) .gt. bucket_J)THEN
          acswupbc(i,j) = acswupbc(i,j) - bucket_J
          i_acswupbc(i,j) =  i_acswupbc(i,j) + 1
        ENDIF
        IF(acswdnb(i,j) .gt. bucket_J)THEN
          acswdnb(i,j) = acswdnb(i,j) - bucket_J
          i_acswdnb(i,j) =  i_acswdnb(i,j) + 1
        ENDIF
        IF(acswdnbc(i,j) .gt. bucket_J)THEN
          acswdnbc(i,j) = acswdnbc(i,j) - bucket_J
          i_acswdnbc(i,j) =  i_acswdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
      IF (PRESENT(ACLWUPT) .and. bucket_J .gt. 0.0)THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
        IF(aclwupt(i,j) .gt. bucket_J)THEN
          aclwupt(i,j) = aclwupt(i,j) - bucket_J
          i_aclwupt(i,j) =  i_aclwupt(i,j) + 1
        ENDIF
        IF(aclwuptc(i,j) .gt. bucket_J)THEN
          aclwuptc(i,j) = aclwuptc(i,j) - bucket_J
          i_aclwuptc(i,j) =  i_aclwuptc(i,j) + 1
        ENDIF
        IF(aclwdnt(i,j) .gt. bucket_J)THEN
          aclwdnt(i,j) = aclwdnt(i,j) - bucket_J
          i_aclwdnt(i,j) =  i_aclwdnt(i,j) + 1
        ENDIF
        IF(aclwdntc(i,j) .gt. bucket_J)THEN
          aclwdntc(i,j) = aclwdntc(i,j) - bucket_J
          i_aclwdntc(i,j) =  i_aclwdntc(i,j) + 1
        ENDIF
        IF(aclwupb(i,j) .gt. bucket_J)THEN
          aclwupb(i,j) = aclwupb(i,j) - bucket_J
          i_aclwupb(i,j) =  i_aclwupb(i,j) + 1
        ENDIF
        IF(aclwupbc(i,j) .gt. bucket_J)THEN
          aclwupbc(i,j) = aclwupbc(i,j) - bucket_J
          i_aclwupbc(i,j) =  i_aclwupbc(i,j) + 1
        ENDIF
        IF(aclwdnb(i,j) .gt. bucket_J)THEN
          aclwdnb(i,j) = aclwdnb(i,j) - bucket_J
          i_aclwdnb(i,j) =  i_aclwdnb(i,j) + 1
        ENDIF
        IF(aclwdnbc(i,j) .gt. bucket_J)THEN
          aclwdnbc(i,j) = aclwdnbc(i,j) - bucket_J
          i_aclwdnbc(i,j) =  i_aclwdnbc(i,j) + 1
        ENDIF
      ENDDO      
      ENDDO
      ENDIF
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

! Compute precipitation accumulation in a given time window: prec_acc_dt
   IF (prec_acc_dt .gt. 0.) THEN

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         IF (mod(curr_secs2, 60.* prec_acc_dt) == 0.) THEN
            prec_acc_c(i,j)  = 0.
            prec_acc_nc(i,j) = 0.
            snow_acc_nc(i,j)  = 0.
         ENDIF
         prec_acc_c(i,j)  = prec_acc_c(i,j)  +  RAINCV(i,j)
         prec_acc_nc(i,j) = prec_acc_nc(i,j) + RAINNCV(i,j)
         prec_acc_c(i,j)  = MAX (prec_acc_c(i,j), 0.0)
         prec_acc_nc(i,j) = MAX (prec_acc_nc(i,j), 0.0)
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) + SNOWNCV(I,J)
! add convective precip to snow bucket if t2 < 273.15
         IF ( t2(i,j) .lt. 273.15 ) THEN
         snow_acc_nc(i,j)   = snow_acc_nc(i,j) +  RAINCV(i,j)
         snow_acc_nc(i,j)   = MAX (snow_acc_nc(i,j), 0.0)
         ENDIF
      ENDDO     
      ENDDO     

   ENDDO     

!  !$OMP END PARALLEL DO
   ENDIF



! NSSL

   IF ( nwp_diagnostics .EQ. 1 ) THEN

     idump = (history_interval * 60.) / dt

!   print *,' history_interval = ', history_interval
!   print *,' itimestep        = ', itimestep
!   print *,' idump            = ', idump
!   print *,' xtime            = ', xtime


! IF ( MOD(itimestep, idump) .eq. 0 ) THEN
!    WRITE(outstring,*) 'Computing PH0 for this domain with curr_secs2 = ', curr_secs2
!    CALL wrf_message ( TRIM(outstring) )

   IF ( MOD((itimestep - 1), idump) .eq. 0 ) THEN
     WRITE(outstring,*) 'NSSL Diagnostics: Resetting max arrays for domain with dt = ', dt
     CALL wrf_debug ( 10,TRIM(outstring) )

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
     DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
         wspd10max(i,j)   = 0.
         up_heli_max(i,j) = 0.
         w_up_max(i,j)    = 0.
         w_dn_max(i,j)    = 0.
         w_mean(i,j)      = 0.
         grpl_max(i,j)    = 0.
         refd_max(i,j)    = 0.
         hail_maxk1(i,j)  = 0.
         hail_max2d(i,j)  = 0.
       ENDDO
       ENDDO
     ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO i=i_start(ij),i_end(ij)

! Zero some accounting arrays that will be used below

       w_colmean(i,j)   = 0.
       numcolpts(i,j)   = 0.
       grpl_colint(i,j) = 0.
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme
     DO i=i_start(ij),i_end(ij)

! Find vertical velocity max (up and down) below 400 mb

       IF ( p8w(i,k,j) .GT. 40000. .AND. w(i,k,j) .GT. w_up_max(i,j) ) THEN
         w_up_max(i,j) = w(i,k,j)
       ENDIF

       IF ( p8w(i,k,j) .GT. 40000. .AND. w(i,k,j) .LT. w_dn_max(i,j) ) THEN
         w_dn_max(i,j) = w(i,k,j)
       ENDIF

! For the column mean vertical velocity calculation, first
! total the vertical velocity between sigma levels 0.5 and 0.8

       IF ( znw(k) .GE. 0.5 .AND. znw(k) .LE. 0.8 ) THEN
         w_colmean(i,j) = w_colmean(i,j) + w(i,k,j)
         numcolpts(i,j) = numcolpts(i,j) + 1
       ENDIF
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)

! Calculate the column integrated graupel

       depth = ( ( ph(i,k+1,j) + phb(i,k+1,j) ) / g ) - &
               ( ( ph(i,k  ,j) + phb(i,k  ,j) ) / g )
       grpl_colint(i,j) = grpl_colint(i,j) + qg_curr(i,k,j) * depth * rho(i,k,j)
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO i=i_start(ij),i_end(ij)

! Calculate the max 10 m wind speed between output times

       wind_vel = sqrt ( u10(i,j)*u10(i,j) + v10(i,j)*v10(i,j) )
       IF ( wind_vel .GT. wspd10max(i,j) ) THEN
         wspd10max(i,j) = wind_vel
       ENDIF

! Calculate the column mean vertical velocity between output times

       w_mean(i,j) = w_mean(i,j) + w_colmean(i,j) / numcolpts(i,j)

       IF ( MOD(itimestep, idump) .eq. 0 ) THEN
         w_mean(i,j) = w_mean(i,j) / idump
       ENDIF

! Calculate the max column integrated graupel between output times

       IF ( grpl_colint(i,j) .gt. grpl_max(i,j) ) THEN
          grpl_max(i,j) = grpl_colint(i,j)
       ENDIF

   ! Calculate the max radar reflectivity between output times

       IF ( refl_10cm(i,kms,j) .GT. refd_max(i,j) ) THEN
         refd_max(i,j) = refl_10cm(i,kms,j)
       ENDIF
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO



!+---+-----------------------------------------------------------------+ 
!+---+-----------------------------------------------------------------+ 
!..Calculate a maximum hail diameter from the characteristics of the
!.. graupel category mixing ratio and number concentration (or hail, if
!.. available).  This diagnostic uses the actual spectral distribution
!.. assumptions, calculated by breaking the distribution into 50 bins
!.. from 0.5mm to 7.5cm.  Once a minimum number concentration of 0.01
!.. particle per cubic meter of air is reached, from the upper size
!.. limit, then this bin is considered the max size.
!+---+-----------------------------------------------------------------+ 

      WRITE(outstring,*) 'GT-Diagnostics, computing max-hail diameter'
      CALL wrf_debug (100, TRIM(outstring))


   IF (PRESENT(qh_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has hail mixing ratio'
   CALL wrf_debug (150, TRIM(outstring))
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_qg(i,k,j) = MAX(1.E-12, qh_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSE
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_qg(i,k,j) = MAX(1.E-12, qg_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF

   IF (PRESENT(nh_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has hail number concentration'
   CALL wrf_debug (150, TRIM(outstring))
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = MAX(1.E-8, nh_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSEIF (PRESENT(ng_curr)) THEN
   WRITE(outstring,*) 'GT-Debug, this mp scheme, ', mphysics_opt, ' has graupel number concentration'
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = MAX(1.E-8, ng_curr(i,k,j)*rho(i,k,j))
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ELSE
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
   DO ij = 1 , num_tiles
     DO j=j_start(ij),j_end(ij)
     DO k=kms,kme-1
     DO i=i_start(ij),i_end(ij)
        temp_ng(i,k,j) = 1.E-8
     ENDDO
     ENDDO
     ENDDO
   ENDDO
!  !$OMP END PARALLEL DO
   ENDIF


      ! Calculate the max hail size from graupel/hail parameters in microphysics scheme (gthompsn 12Mar2015)
      ! First, we do know multiple schemes have assumed inverse-exponential distribution with constant
      ! intercept parameter and particle density.  Please leave next 4 settings alone for common
      ! use and copy these and cge constants to re-assign per scheme if needed (e.g. NSSL).

      xrho_g = 500.
      xam_g = 3.1415926536/6.0*xrho_g     ! Assumed m(D) = a*D**b, where a=PI/6*rho_g and b=3
      xbm_g = 3.                          ! in other words, spherical graupel/hail
      xmu_g = 0.
      scheme_has_graupel = .false.

      !..Some constants. These *MUST* get changed below per scheme
      !.. *IF* either xbm_g or xmu_g value is changed from 3 and zero, respectively.

      cge(1) = xbm_g + 1.
      cge(2) = xmu_g + 1.
      cge(3) = xbm_g + xmu_g + 1.
      do n = 1, 3
         cgg(n) = WGAMMA(cge(n))
      enddo

   mp_select: SELECT CASE(mphysics_opt)

     CASE (KESSLERSCHEME)
!      nothing to do here

     CASE (LINSCHEME)
       scheme_has_graupel = .true.
       xrho_g = 917.
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e4
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (WSM3SCHEME)
!      nothing to do here

     CASE (WSM5SCHEME)
!      nothing to do here

     CASE (WSM6SCHEME)
       scheme_has_graupel = .true.
       xrho_g = 500.
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e6
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (WDM5SCHEME)
!      nothing to do here

     CASE (WDM6SCHEME)
       scheme_has_graupel = .true.
       xrho_g = 500.
       N0exp = 4.e6
       if (mpuse_hail .eq. 1) then
         xrho_g = 700.
         N0exp = 4.e4
       endif
       xam_g = 3.1415926536/6.0*xrho_g
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO

     CASE (GSFCGCESCHEME)
      if (gsfcgce_2ice.eq.0 .OR. gsfcgce_2ice.eq.2) then
       scheme_has_graupel = .true.
       if (gsfcgce_hail .eq. 1) then
          xrho_g = 900.
       else
          xrho_g = 400.
       endif
       xam_g = 3.1415926536/6.0*xrho_g
       N0exp = 4.e4
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO
      endif

     CASE (SBU_YLINSCHEME)
!      scheme_has_graupel = .true.  ! Can be calculated from rime fraction variable.
!      no time to implement

     CASE (ETAMPNEW)
!      scheme_has_graupel = .true.  ! Can be calculated from rime fraction variable.
!      no time to implement

     CASE (THOMPSON, THOMPSONAERO)

       scheme_has_graupel = .true.

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO k=kms,kme-1
       DO i=i_start(ij),i_end(ij)
          temp_qr(i,k,j) = MAX(1.E-10, qr_curr(i,k,j)*rho(i,k,j))
          temp_nr(i,k,j) = MAX(1.E-8, nr_curr(i,k,j)*rho(i,k,j))
       ENDDO
       ENDDO
       ENDDO
      ENDDO
!  !$OMP END PARALLEL DO

       ! Technically, the equation below for lambda-r should have constants
       ! for the rain mass-diameter relation and mu, but these are currently
       ! the same as graupel, so we are cheating to avoid passing more
       ! constants from mp_thompson.

!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
       DO j=j_start(ij),j_end(ij)
       DO i=i_start(ij),i_end(ij)
        N0min = 1.E6
        DO k=kme-1, kms, -1
         if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
         lamr = (1000.0*3.1415926536/6.0*cgg(3)/cgg(2)*temp_nr(i,k,j)/temp_qr(i,k,j))**(1./3.)
         mvd_r = 3.672 / lamr   ! Technically this should have (+mu_r)
         mvd_r = MAX(100.E-6, MIN(mvd_r, 2.5E-3))
         if (temp(i,k,j).lt.270.65 .and. mvd_r.gt.100.E-6) then
            xslw1 = 4.01 + alog10(mvd_r)
         else
            xslw1 = 0.01
         endif
         ygra1 = 4.31 + alog10(max(5.E-5, temp_qg(i,k,j)))
         zans1 = 3.1 + (100./(300.*xslw1*ygra1/(10./xslw1+1.+0.25*ygra1)+30.+10.*ygra1))
         N0exp = 10.**(zans1)
         N0exp = MAX(DBLE(1.E4), MIN(N0exp, DBLE(1.E6)))
         N0min = MIN(N0exp, N0min)
         N0exp = N0min
         lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
         lamg = lam_exp * (cgg(3)/cgg(2)/cgg(1))**(1./xbm_g)
         N0_g = N0exp/(cgg(2)*lam_exp) * lamg**cge(2)
         temp_ng(i,k,j) = N0_g*cgg(2)*lamg**(-cge(2))
         if (N0exp .ge. 1.E4 .AND. N0exp.le.1.E5) then
         WRITE(outstring,*) 'GT-Debug, ', N0exp, temp_qr(i,k,j)*1000., temp_ng(i,k,j)
         CALL wrf_debug (850, TRIM(outstring))
         endif
        ENDDO
       ENDDO
       ENDDO
      ENDDO
!  !$OMP END PARALLEL DO


!    CASE (MORR_MILB_P3)
!      scheme_has_graupel = .true.
!      either Hugh or Jason need to implement.

     CASE (MORR_TWO_MOMENT)
       scheme_has_graupel = .true.
       xrho_g = 400.
       if (mpuse_hail .eq. 1) xrho_g = 900.
       xam_g = 3.1415926536/6.0*xrho_g

     CASE (MILBRANDT2MOM)
       WRITE(outstring,*) 'GT-Debug, using Milbrandt2mom, which has 2-moment hail'
       CALL wrf_debug (150, TRIM(outstring))
       scheme_has_graupel = .true.
       xrho_g = 900.
       xam_g = 3.1415926536/6.0*xrho_g

!    CASE (MILBRANDT3MOM)
!      coming in future?

     CASE (NSSL_1MOMLFO, NSSL_1MOM, NSSL_2MOM, NSSL_2MOMCCN)

       scheme_has_graupel = .true.
       xrho_g = nssl_rho_qh
       N0exp = nssl_cnoh
       if (PRESENT(qh_curr)) then
          xrho_g = nssl_rho_qhl
          N0exp = nssl_cnohl
       endif
       xam_g = 3.1415926536/6.0*xrho_g

       if (PRESENT(ng_curr)) xmu_g = nssl_alphah
       if (PRESENT(nh_curr)) xmu_g = nssl_alphahl

       if (xmu_g .NE. 0.) then
          cge(1) = xbm_g + 1.
          cge(2) = xmu_g + 1.
          cge(3) = xbm_g + xmu_g + 1.
          do n = 1, 3
             cgg(n) = WGAMMA(cge(n))
          enddo
       endif

       ! NSSL scheme has many options, but, if single-moment, just fill
       ! in the number array for graupel from built-in assumptions.

       if (.NOT.(PRESENT(nh_curr).OR.PRESENT(ng_curr)) ) then
!      !$OMP PARALLEL DO   &
!      !$OMP PRIVATE ( ij )
       DO ij = 1 , num_tiles
         DO j=j_start(ij),j_end(ij)
         DO k=kme-1, kms, -1
         DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lam_exp = (N0exp*xam_g*cgg(1)/temp_qg(i,k,j))**(1./cge(1))
           temp_ng(i,k,j) = N0exp*cgg(2)*lam_exp**(-cge(2))
         ENDDO
         ENDDO
         ENDDO
       ENDDO
!      !$OMP END PARALLEL DO
       endif

!    CASE (NSSL_3MOM)
!      coming in future?

     CASE (CAMMGMPSCHEME)
!      nothing to do here

     CASE (FULL_KHAIN_LYNN)
!      explicit bin scheme so code below not applicable
!      scheme authors need to implement if desired.

     CASE (FAST_KHAIN_LYNN)
!      explicit bin scheme so code below not applicable
!      scheme authors need to implement if desired.

     CASE DEFAULT

   END SELECT mp_select


   if (scheme_has_graupel) then

!..Create bins of graupel/hail (from 500 microns up to 7.5 cm).
      xxDx(1) = 500.D-6
      xxDx(ngbins+1) = 0.075d0
      do n = 2, ngbins
         xxDx(n) = DEXP(DFLOAT(n-1)/DFLOAT(ngbins) &
                  *DLOG(xxDx(ngbins+1)/xxDx(1)) +DLOG(xxDx(1)))
      enddo
      do n = 1, ngbins
         xxDg(n) = DSQRT(xxDx(n)*xxDx(n+1))
         xdtg(n) = xxDx(n+1) - xxDx(n)
      enddo


!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )
      DO ij = 1 , num_tiles
        DO j=j_start(ij),j_end(ij)
        DO k=kms,kme-1
        DO i=i_start(ij),i_end(ij)
           if (temp_qg(i,k,j) .LT. 1.E-6) CYCLE
           lamg = (xam_g*cgg(3)/cgg(2)*temp_ng(i,k,j)/temp_qg(i,k,j))**(1./xbm_g)
           N0_g = temp_ng(i,k,j)/cgg(2)*lamg**cge(2)
           sum_ng = 0.0d0
           do ng = ngbins, 1, -1
              f_d = N0_g*xxDg(ng)**xmu_g * DEXP(-lamg*xxDg(ng))*xdtg(ng)
              sum_ng = sum_ng + f_d
              if (sum_ng .gt. thresh_conc) then
                 exit
              endif
           enddo
           if (ng .ge. ngbins) then
              hail_max = xxDg(ngbins)
           elseif (xxDg(ng+1) .gt. 1.E-3) then
              hail_max = xxDg(ng+1)
           else
              hail_max = 1.E-4
           endif
           if (hail_max .gt. 1E-2) then
            WRITE(outstring,*) 'GT-Debug-Hail, ', hail_max*1000.
            CALL wrf_debug (350, TRIM(outstring))
           endif
           hail_max_sp = hail_max
           if (k.eq.kms) then
              hail_maxk1(i,j) = MAX(hail_maxk1(i,j), hail_max_sp)
           endif
           hail_max2d(i,j) = MAX(hail_max2d(i,j), hail_max_sp)
        ENDDO
        ENDDO
        ENDDO
      ENDDO
!  !$OMP END PARALLEL DO

   endif


   ENDIF
! NSSL

   if (diag_print .eq. 0 ) return

   IF ( xtime .ne. 0. ) THEN

    if(diag_print.eq.1) then
       prfreq = dt
!      prfreq = max(2,int(dt/60.))   ! in min
    else
       prfreq=10                   ! in min
    endif
   
    IF (MOD(nint(dt),prfreq) == 0) THEN

! COMPUTE THE NUMBER OF MASS GRID POINTS
   no_points = float((ide-ids)*(jde-jds))

! SET START AND END POINTS FOR TILES
!  !$OMP PARALLEL DO   &
!  !$OMP PRIVATE ( ij )

   dmumax = 0.
   DO ij = 1 , num_tiles

!     print *, i_start(ij),i_end(ij),j_start(ij),j_end(ij)
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=(p8w(i,kms,j)-pk1m(i,j))/dt
         dmudt(i,j)=(mu_2(i,j)-mu_2m(i,j))/dt
         if(abs(dmudt(i,j)*dt).gt.dmumax)then
           dmumax=abs(dmudt(i,j)*dt)
           idp=i
           jdp=j
         endif
      ENDDO      
      ENDDO

   ENDDO
!  !$OMP END PARALLEL DO

! convert DMUMAX from (PA) to (bars) per time step
   dmumax = dmumax*1.e-5
! compute global MAX
   CALL wrf_dm_maxval ( dmumax,  idp, jdp )

!  print *, 'p8w(30,1,30),pk1m(30,30) : ', p8w(30,1,30),pk1m(30,30)
!  print *, 'mu_2(30,30),mu_2m(30,30) : ', mu_2(30,30),mu_2m(30,30)
   dpsdt_sum = 0.
   dmudt_sum = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       dpsdt_sum = dpsdt_sum + abs(dpsdt(i,j))
       dmudt_sum = dmudt_sum + abs(dmudt(i,j))
     ENDDO
   ENDDO

! compute global sum
   dpsdt_sum = wrf_dm_sum_real ( dpsdt_sum )
   dmudt_sum = wrf_dm_sum_real ( dmudt_sum )

!  print *, 'dpsdt, dmudt : ', dpsdt_sum, dmudt_sum

   IF ( diag_print .eq. 2 ) THEN
   dardt_sum = 0.
   drcdt_sum = 0.
   drndt_sum = 0.
   rainc_sum = 0.
   raint_sum = 0.
   rainnc_sum = 0.
   sfcevp_sum = 0.
   hfx_sum = 0.
   lh_sum = 0.
   raincmax = 0.
   rainncmax = 0.

   DO j = jps, min(jpe,jde-1)
     DO i = ips, min(ipe,ide-1)
       drcdt_sum = drcdt_sum + abs(raincv(i,j))
       drndt_sum = drndt_sum + abs(rainncv(i,j))
       dardt_sum = dardt_sum + abs(raincv(i,j)) + abs(rainncv(i,j))
       rainc_sum = rainc_sum + abs(rainc(i,j))
! MAX for accumulated conv precip
       IF(rainc(i,j).gt.raincmax)then
          raincmax=rainc(i,j)
          irc=i
          jrc=j
       ENDIF
       rainnc_sum = rainnc_sum + abs(rainnc(i,j))
! MAX for accumulated resolved precip
       IF(rainnc(i,j).gt.rainncmax)then
          rainncmax=rainnc(i,j)
          irnc=i
          jrnc=j
       ENDIF
       raint_sum = raint_sum + abs(rainc(i,j)) + abs(rainnc(i,j))
       sfcevp_sum = sfcevp_sum + abs(sfcevp(i,j))
       hfx_sum = hfx_sum + abs(hfx(i,j))
       lh_sum = lh_sum + abs(lh(i,j))
     ENDDO
   ENDDO

! compute global MAX
   CALL wrf_dm_maxval ( raincmax, irc, jrc )
   CALL wrf_dm_maxval ( rainncmax, irnc, jrnc )

! compute global sum
   drcdt_sum = wrf_dm_sum_real ( drcdt_sum )
   drndt_sum = wrf_dm_sum_real ( drndt_sum )
   dardt_sum = wrf_dm_sum_real ( dardt_sum )
   rainc_sum = wrf_dm_sum_real ( rainc_sum )
   rainnc_sum = wrf_dm_sum_real ( rainnc_sum )
   raint_sum = wrf_dm_sum_real ( raint_sum )
   sfcevp_sum = wrf_dm_sum_real ( sfcevp_sum )
   hfx_sum = wrf_dm_sum_real ( hfx_sum )
   lh_sum = wrf_dm_sum_real ( lh_sum )

   ENDIF

! print out the average values

   CALL get_current_grid_name( grid_str )

#ifdef DM_PARALLEL
   IF ( wrf_dm_on_monitor() ) THEN
#endif
     WRITE(outstring,*) grid_str,'Domain average of dpsdt, dmudt (mb/3h): ', xtime, &
           dpsdt_sum/no_points*108., &
           dmudt_sum/no_points*108.
     CALL wrf_message ( TRIM(outstring) )

     WRITE(outstring,*) grid_str,'Max mu change time step: ', idp,jdp,dmumax
     CALL wrf_message ( TRIM(outstring) )

     IF ( diag_print .eq. 2) THEN
     WRITE(outstring,*) grid_str,'Domain average of dardt, drcdt, drndt (mm/sec): ', xtime, &
           dardt_sum/dt/no_points, &
           drcdt_sum/dt/no_points, &
           drndt_sum/dt/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of rt_sum, rc_sum, rnc_sum (mm): ', xtime, &
           raint_sum/no_points, &
           rainc_sum/no_points, &
           rainnc_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Resolved Precip,   I,J  (mm): '               ,&
           rainncmax,irnc,jrnc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Max Accum Convective Precip,   I,J  (mm): '             ,&
           raincmax,irc,jrc
     CALL wrf_message ( TRIM(outstring) )
     WRITE(outstring,*) grid_str,'Domain average of sfcevp, hfx, lh: ', xtime, &
           sfcevp_sum/no_points, &
           hfx_sum/no_points, &
           lh_sum/no_points
     CALL wrf_message ( TRIM(outstring) )
     ENDIF
#ifdef DM_PARALLEL
   ENDIF
#endif

    ENDIF        ! print frequency
   ENDIF

! save values at this time step
   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j )
   DO ij = 1 , num_tiles

      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         pk1m(i,j)=p8w(i,kms,j)
         mu_2m(i,j)=mu_2(i,j)
      ENDDO
      ENDDO

      IF ( xtime .lt. 0.0001 ) THEN
      DO j=j_start(ij),j_end(ij)
      DO i=i_start(ij),i_end(ij)
         dpsdt(i,j)=0.
         dmudt(i,j)=0.
      ENDDO
      ENDDO
      ENDIF

   ENDDO
   !$OMP END PARALLEL DO

   END SUBROUTINE diagnostic_output_calc


!+---+-----------------------------------------------------------------+ 
      REAL FUNCTION GAMMLN(XX)
!     --- RETURNS THE VALUE LN(GAMMA(XX)) FOR XX > 0.
      IMPLICIT NONE
      REAL, INTENT(IN):: XX
      DOUBLE PRECISION, PARAMETER:: STP = 2.5066282746310005D0
      DOUBLE PRECISION, DIMENSION(6), PARAMETER:: &
               COF = (/76.18009172947146D0, -86.50532032941677D0, &
                       24.01409824083091D0, -1.231739572450155D0, &
                      .1208650973866179D-2, -.5395239384953D-5/)
      DOUBLE PRECISION:: SER,TMP,X,Y
      INTEGER:: J

      X=XX
      Y=X
      TMP=X+5.5D0
      TMP=(X+0.5D0)*LOG(TMP)-TMP
      SER=1.000000000190015D0
      DO 11 J=1,6
        Y=Y+1.D0
        SER=SER+COF(J)/Y
11    CONTINUE
      GAMMLN=TMP+LOG(STP*SER/X)
      END FUNCTION GAMMLN
!  (C) Copr. 1986-92 Numerical Recipes Software 2.02
!+---+-----------------------------------------------------------------+ 
      REAL FUNCTION WGAMMA(y)

      IMPLICIT NONE
      REAL, INTENT(IN):: y

      WGAMMA = EXP(GAMMLN(y))

      END FUNCTION WGAMMA
!+---+-----------------------------------------------------------------+ 

!-------------------------------------------------------
! Amy Solomon Feb 2015
! RASM statistics subroutines added from here to the end
! controlled through namelist parameters
!-------------------------------------------------------
  SUBROUTINE mean_output_calc(                    &
       ids, ide, jds, jde, kds, kde               &
       ,ims, ime, jms, jme, kms, kme              &
       ,ips, ipe, jps, jpe, kps, kpe              & ! patch  dims
       ,i_start, i_end                            &
       ,j_start, j_end, num_tiles, is_restart     &
       ,mean_interval, psfc, psfcmean, dt, xtime  &
       ,tsk , tskmean, t2, tmp2mean, t, tmean       &
       ,p, pb, moist, ht, pmslmean, hfx, hfxmean  & 
       ,lh, lhmean, swdnb, swdnbmean, glw         & 
       ,glwmean, lwupb, lwupbmean, swupb          &
       ,swupbmean, u, umean, v, vmean, qvmean     &
       ,shummean                                  &
#ifdef CESMCOUPLED
       ,swndr, swndf, swvdr, swvdf                &
#endif
       ,swupt                                     &
       ,swdnt, lwupt, lwdnt                       &
       ,swndrmean, swndfmean                      &
       ,swvdrmean, swvdfmean                      &
       ,swuptmean, swdntmean                      &
       ,lwuptmean, lwdntmean, currenttime         &
       ,output_freq, avgoutalarm, avgOutDateStr   &
       , nsteps    &
       )
    !----------------------------------------------------------------------

    ! USES:
    USE module_utility
    USE module_model_constants

    IMPLICIT NONE
    !======================================================================
    ! Definitions
    !-----------
    !-- DT            time step (second)
    !-- XTIME         forecast time
    !-- ims           start index for i in memory
    !-- ime           end index for i in memory
    !-- jms           start index for j in memory
    !-- jme           end index for j in memory    
    !-- i_start       start indices for i in tile
    !-- i_end         end indices for i in tile
    !-- j_start       start indices for j in tile
    !-- j_end         end indices for j in tile
    !-- num_tiles     number of tiles
    !
    !======================================================================

    INTEGER, INTENT(IN)                       :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN)                       :: ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(IN)                       :: ips, ipe, jps, jpe, kps, kpe
    INTEGER, INTENT(IN)                       :: num_tiles
    INTEGER, INTENT(IN)                       :: mean_interval
    INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start, i_end, j_start, j_end
    TYPE(WRFU_Time), INTENT(IN)               :: currentTime
    TYPE(WRFU_Alarm), INTENT(INOUT)           :: avgOutAlarm
    INTEGER, INTENT(IN)                       :: output_freq            ! interval type
    INTEGER, INTENT(INOUT)                    :: nsteps                 ! number of step accumulated
    CHARACTER(*), INTENT(INOUT)               :: avgOutDateStr

    REAL, INTENT(IN)                                  :: dt, xtime
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  :: psfc, tsk, t2, ht, hfx, lh, swdnb
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  :: glw, lwupb, swupb
#ifdef CESMCOUPLED
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  :: swndr, swndf, swvdr, swvdf
#endif
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  :: swupt
    REAL, DIMENSION( ims:ime , jms:jme ), INTENT(IN)  :: swdnt, lwupt, lwdnt
    REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), INTENT(IN) :: t, p, pb, moist, u, v
    
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: psfcMean, tskMean, tmp2mean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: hfxMean, lhMean, swdnbMean 
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: glwMean, lwupbMean, swupbMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: tMean, pmslMean, uMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: vMean, qvMean, shumMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swndrMean, swndfMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swvdrMean, swvdfMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: swuptMean, swdntMean
    DOUBLE PRECISION, DIMENSION( ims:ime , jms:jme ), INTENT(OUT) :: lwuptMean, lwdntMean   

    ! LOCAL  VAR
    INTEGER     :: i, j, ij
    REAL        :: value
    LOGICAL     :: is_restart
    INTEGER     :: rc

    LOGICAL     :: is_reset               ! reset averages
    LOGICAL     :: compute_avg            ! compute averages

    ! DEBUG LOCAL  VAR
    CHARACTER (LEN=1024)  :: message
 
    CALL getResetState(currentTime, xtime, dt, mean_interval, output_freq, is_reset)

    IF (is_reset) THEN
       DO ij = 1 , num_tiles
          DO j = j_start(ij), j_end(ij)
             DO i = i_start(ij), i_end(ij)
                psfcMean(i,j)=0.0D0
                tskMean(i,j)=0.0D0
                tmp2mean(i,j)=0.0D0
                tMean(i,j)=0.0D0
                pmslMean(i,j)=0.0D0
                hfxMean(i,j)=0.0D0
                lhMean(i,j)=0.0D0
                swdnbMean(i,j)=0.0D0
                glwMean(i,j)=0.0D0
                lwupbMean(i,j)=0.0D0
                swupbMean(i,j)=0.0D0
                uMean(i,j)=0.0D0
                vMean(i,j)=0.0D0
                uMean(i+1,j)=0.0D0
                vMean(i,j+1)=0.0D0
                qvMean(i,j)=0.0D0
                shumMean(i,j)=0.0D0
                swndrMean(i,j)=0.0D0
                swndfMean(i,j)=0.0D0
                swvdrMean(i,j)=0.0D0
                swvdfMean(i,j)=0.0D0
                swuptMean(i,j)=0.0D0
                swdntMean(i,j)=0.0D0
                lwuptMean(i,j)=0.0D0
                lwdntMean(i,j)=0.0D0
             ENDDO
          ENDDO
       ENDDO

       ! restart step count 
       nsteps = 0.0

       WRITE(message, *) "RASM Statistics: RESET accumaltions and means ..................... nsteps=", nsteps
       CALL wrf_debug(200, message)

    ENDIF
    
    nsteps = nsteps+1.0

    WRITE(message, *) "RASM Statistics: Start accumulate .........................................................."
    CALL wrf_debug(200, message)
    WRITE(message, *) "RASM Statistics: nsteps=",nsteps, "time_step=", (xtime+dt/60.)*60./dt, "xtime=", xtime
    CALL wrf_debug(200, message)  

    ! Surface Pressure
    CALL var_accum_2d(psfc,ime-ims+1,jme-jms+1,psfcMean)
    
    ! Surface Skin Temperature
    CALL var_accum_2d(tsk,ime-ims+1,jme-jms+1,tskMean)
    
    ! Temperature at 2M
    CALL var_accum_2d(t2,ime-ims+1,jme-jms+1,tmp2mean)

    ! SENSIBLE HEAT FLUX AT THE SURFACE
    CALL var_accum_2d(hfx,ime-ims+1,jme-jms+1,hfxMean)

    ! LATENT HEAT FLUX AT THE SURFACE
    CALL var_accum_2d(lh,ime-ims+1,jme-jms+1,lhMean)

    ! INCOMING SOLAR AT SURFACE (SHORTWAVE DOWN)
    CALL var_accum_2d(swdnb,ime-ims+1,jme-jms+1,swdnbMean)

    ! INCOMING LONGWAVE AT SURFACE (LONGWAVE DOWN)
    CALL var_accum_2d(glw,ime-ims+1,jme-jms+1,glwMean)

    ! OUTGOING LONGWAVE AT SURFACE (LONGWAVE FLUX UP)
    CALL var_accum_2d(lwupb,ime-ims+1,jme-jms+1,lwupbMean)

    ! REFLECTIVE SHORTWAVE AT SURFACE 
    CALL var_accum_2d(swupb,ime-ims+1,jme-jms+1,swupbMean)

#ifdef CESMCOUPLED

    ! SWNDR - shortwave down at surface, near infrared direct component
    CALL var_accum_2d(swndr,ime-ims+1,jme-jms+1,swndrMean)

    ! SWNDF - shortwave down at surface, near infrared diffuse component
    CALL var_accum_2d(swndf,ime-ims+1,jme-jms+1,swndfMean)

    ! SWVDR - shortwave down at surface, visible direct component
    CALL var_accum_2d(swvdr,ime-ims+1,jme-jms+1,swvdrMean)

    ! SWVDF - shortwave down at surface, visible diffuse component
    CALL var_accum_2d(swvdf,ime-ims+1,jme-jms+1,swvdfMean)

#endif

    ! SWUPT - UPWELLING SHORTWAVE FLUX AT TOP
    CALL var_accum_2d(swupt,ime-ims+1,jme-jms+1,swuptMean)

    ! SWDNT - DOWNWELLING SHORTWAVE FLUX AT TOP
    CALL var_accum_2d(swdnt,ime-ims+1,jme-jms+1,swdntMean)

    ! LWUPT - UPWELLING LONGWAVE FLUX AT TOP
    CALL var_accum_2d(lwupt,ime-ims+1,jme-jms+1,lwuptMean)

    ! LWDNT - DOWNWELLING LONGWAVE FLUX AT TOP
    CALL var_accum_2d(lwdnt,ime-ims+1,jme-jms+1,lwdntMean)

    ! U SURFACE WIND SPEED (X-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         u, uMean) 

    ! V SURFACE WIND SPEED (Y-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         v, vMean)

    ! SURFACE HUMIDITY
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         moist, qvMean) 

    ! SPECIFIC SURFACE HUMIDITY 
    CALL shum_accum_01(ims, ime, jms, jme, kms, kme,    &
                       ide, jde, ips, ipe, jps, jpe,    &
                       moist, shumMean)

    ! PMSL & Surface temperature (at lowest level)
    CALL PMSL_T_accum_01( ims, ime, jms, jme, kms, kme,    &
                          ide, jde, ips, ipe, jps, jpe,    &
                          t, p, pb, moist, ht, psfc, pmslMean, tMean)

    CALL getAvgState(currentTime, xtime, dt, mean_interval, output_freq, compute_avg, avgOutDateStr)
    IF (compute_avg) THEN
       psfcMean=psfcMean/nsteps
       tskMean=tskMean/nsteps
       tmp2mean=tmp2mean/nsteps
       tMean=tMean/nsteps
       pmslMean=pmslMean/nsteps
       hfxMean=hfxMean/nsteps
       lhMean=lhMean/nsteps
       swdnbMean=swdnbMean/nsteps
       glwMean=glwMean/nsteps
       lwupbMean=lwupbMean/nsteps
       swupbMean=swupbMean/nsteps
       uMean=uMean/nsteps
       vMean=vMean/nsteps
       qvMean=qvMean/nsteps
       shumMean=shumMean/nsteps
#ifdef CESMCOUPLED 
       swndrMean=swndrMean/nsteps
       swndfMean=swndfMean/nsteps
       swvdrMean=swvdrMean/nsteps
       swvdfMean=swvdfMean/nsteps
#endif
       swuptMean=swuptMean/nsteps
       swdntMean=swdntMean/nsteps
       lwuptMean=lwuptMean/nsteps
       lwdntMean=lwdntMean/nsteps
      
       if ( output_freq .EQ. MONTHLY) then
             WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL turn ON ALARM to generate output ........................"
             CALL wrf_debug(200, message)
       endif

       CALL WRFU_AlarmRingerOn (avgOutAlarm, rc=rc)
      
       WRITE(message, *) "RASM Statistics: Mean computed .........................................................."
       CALL wrf_debug(200, message)

    END IF

  END SUBROUTINE mean_output_calc

  ! RASM: Diurnal
  SUBROUTINE diurnalcycle_output_calc(            &
       ids, ide, jds, jde, kds, kde               &
       ,ims, ime, jms, jme, kms, kme              &
       ,ips, ipe, jps, jpe, kps, kpe              & ! patch  dims
       ,i_start, i_end, j_start, j_end, num_tiles &
       ,is_restart, psfc, psfcmean, dt, xtime     &
       ,tsk , tskmean, t2, tmp2mean, t, tmean       &
       ,p, pb, moist, hfx, hfxmean                & 
       ,lh, lhmean, swdnb, swdnbmean, glw         & 
       ,glwmean, lwupb, lwupbmean, swupb          &
       ,swupbmean, u, umean, v, vmean, qvmean     &
       ,shummean                                  &
#ifdef CESMCOUPLED 
       ,swndr, swndf, swvdr, swvdf                  &
#endif
       ,swndrmean, swndfmean, swvdrmean, swvdfmean  &
       ,u10, u10mean, v10, v10mean                  &
       ,q2, q2mean,currenttime, avgoutalarm         &
       ,diurnOutDateStr, avg_nsteps                 &
       ,diurnal_nsteps, psfcdiurnal, tskdiurnal     &
       ,t2diurnal, tdiurnal, hfxdiurnal, lhdiurnal  &
       ,swdnbdiurnal, glwdiurnal, lwupbdiurnal      &
       ,swupbdiurnal, udiurnal, vdiurnal, qvdiurnal &
       ,shumdiurnal                                 &
       ,swndrdiurnal, swndfdiurnal                  &
       ,swvdrdiurnal, swvdfdiurnal                  &
       ,u10diurnal                                  &
       ,v10diurnal, q2diurnal                       &
       )
    !----------------------------------------------------------------------

    ! USES:
    USE module_utility
    USE module_model_constants

    IMPLICIT NONE
    !======================================================================
    ! Definitions
    !-----------
    !-- DT            time step (second)
    !-- XTIME         forecast time
    !-- ims           start index for i in memory
    !-- ime           end index for i in memory
    !-- jms           start index for j in memory
    !-- jme           end index for j in memory    
    !-- i_start       start indices for i in tile
    !-- i_end         end indices for i in tile
    !-- j_start       start indices for j in tile
    !-- j_end         end indices for j in tile
    !-- num_tiles     number of tiles
    !
    !======================================================================

    INTEGER, INTENT(IN)                       :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN)                       :: ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(IN)                       :: ips, ipe, jps, jpe, kps, kpe
   
    INTEGER, INTENT(IN)                       :: num_tiles
    INTEGER, DIMENSION(num_tiles), INTENT(IN) :: i_start, i_end, j_start, j_end
    TYPE(WRFU_Time), INTENT(IN)               :: currentTime
    TYPE(WRFU_Alarm), INTENT(INOUT)           :: avgOutAlarm
    INTEGER, INTENT(INOUT)                    :: avg_nsteps             ! number of avg steps accumulated
    INTEGER, INTENT(INOUT)                    :: diurnal_nsteps         ! number of diurnal steps accumulated
    CHARACTER(*), INTENT(INOUT)               :: diurnOutDateStr

    REAL, INTENT(IN)                                  :: dt, xtime
    REAL, DIMENSION( ims:ime ,jms:jme ), INTENT(IN)  :: psfc, tsk, t2, hfx, lh, swdnb
    REAL, DIMENSION( ims:ime ,jms:jme ), INTENT(IN)  :: glw, lwupb, swupb
#ifdef CESMCOUPLED
    REAL, DIMENSION( ims:ime ,jms:jme ), INTENT(IN)  :: swndr, swndf, swvdr, swvdf
#endif
    REAL, DIMENSION( ims:ime ,jms:jme ), INTENT(IN)  :: u10, v10, q2
    REAL, DIMENSION( ims:ime ,kms:kme, jms:jme ), INTENT(IN) :: t, p, pb, moist, u, v
    
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: psfcMean, tskMean, tmp2mean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: hfxMean, lhMean, swdnbMean 
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: glwMean, lwupbMean, swupbMean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: tMean, uMean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: vMean, qvMean, shumMean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: swndrMean, swndfMean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: swvdrMean, swvdfMean
    DOUBLE PRECISION, DIMENSION( ims:ime, jms:jme ), INTENT(OUT) :: u10Mean, v10Mean, q2Mean
   
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: psfcDiurnal, tskDiurnal
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: t2Diurnal, tDiurnal    
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: hfxDiurnal, lhDiurnal, swdnbDiurnal 
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: glwDiurnal, lwupbDiurnal, swupbDiurnal 
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: uDiurnal, vDiurnal, qvDiurnal 
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: shumDiurnal, swndrDiurnal
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: swndfDiurnal, swvdrDiurnal, swvdfDiurnal
    DOUBLE PRECISION, DIMENSION( ims:ime, 1:NUM_DIURN_CYCLES, jms:jme ), INTENT(OUT) :: u10Diurnal, v10Diurnal, q2Diurnal

    ! LOCAL  VAR
    INTEGER     :: i, j, k, ij
    REAL        :: value
    LOGICAL     :: is_restart
    INTEGER     :: rc
    INTEGER     :: current_diurn_cycle
    INTEGER     :: diurnal_output_freq    ! diurnal interval type
    INTEGER     :: mean_output_freq       ! mean interval type
    INTEGER     :: mean_interval          ! mean interval
    LOGICAL     :: is_avg_reset           ! reset averages
    LOGICAL     :: is_diurnal_reset       ! reset dirurnal cycle
    LOGICAL     :: compute_avg            ! compute averages
    LOGICAL     :: compute_diurnalcycle   ! compute dirurnal cycle
 
    ! DEBUG LOCAL  VAR
    CHARACTER (LEN=1024)  :: message
    CHARACTER (LEN=1024)  :: EmptyStr
    LOGICAL               :: diurn_test
    INTEGER               :: diurn_interval 
   
    ! initialize frequencies and intervals
    mean_interval = DIURNAL_3HR
    mean_output_freq = HRS
    diurn_interval = 2 * 24 * 60 * 60 ! dummy variable only used when testing
    diurnal_output_freq = MONTHLY
    EmptyStr =""                      ! empty string used for processing 

    ! intitialize 2 day test 
    diurn_test = .false.       
    ! if testing generate daily instead of monthy     
    if (diurn_test) then
       diurnal_output_freq = DAYS
    else
       diurnal_output_freq = MONTHLY
    endif

    ! get the average state
    CALL getResetState(currentTime, xtime, dt, mean_interval, mean_output_freq, is_avg_reset)
    IF (is_avg_reset) THEN
       DO ij = 1 , num_tiles
          DO j = j_start(ij), j_end(ij)
             DO i = i_start(ij), i_end(ij)
                psfcMean(i,j)=0.0D0
                tskMean(i,j)=0.0D0
                tmp2mean(i,j)=0.0D0
                tMean(i,j)=0.0D0
                hfxMean(i,j)=0.0D0
                lhMean(i,j)=0.0D0
                swdnbMean(i,j)=0.0D0
                glwMean(i,j)=0.0D0
                lwupbMean(i,j)=0.0D0
                swupbMean(i,j)=0.0D0
                uMean(i,j)=0.0D0
                vMean(i,j)=0.0D0
                uMean(i+1,j)=0.0D0
                vMean(i,j+1)=0.0D0
                qvMean(i,j)=0.0D0
                shumMean(i,j)=0.0D0
                swndrMean(i,j)=0.0D0
                swndfMean(i,j)=0.0D0
                swvdrMean(i,j)=0.0D0
                swvdfMean(i,j)=0.0D0
                u10Mean(i,j)=0.0D0
                v10Mean(i,j)=0.0D0
                q2Mean(i,j)=0.0D0
             ENDDO
          ENDDO
       ENDDO

       ! restart step count 
       avg_nsteps = 0.0

       WRITE(message, *) "RASM Statistics: RESET accumaltions and means ..................... avg_nsteps=", avg_nsteps
       CALL wrf_debug(200, message)

    ENDIF

    ! get the diurnal state
    CALL getResetState(currentTime, xtime, dt, diurn_interval, diurnal_output_freq, is_diurnal_reset)
    IF (is_diurnal_reset) THEN
       DO ij = 1 , num_tiles
          DO k = 1 , NUM_DIURN_CYCLES
             DO j = j_start(ij), j_end(ij)
                DO i = i_start(ij), i_end(ij)
                   psfcDiurnal(i,k,j)=0.0D0
                   tskDiurnal(i,k,j)=0.0D0
                   t2Diurnal(i,k,j)=0.0D0
                   tDiurnal(i,k,j)=0.0D0
                   hfxDiurnal(i,k,j)=0.0D0
                   lhDiurnal(i,k,j)=0.0D0
                   swdnbDiurnal(i,k,j)=0.0D0
                   glwDiurnal(i,k,j)=0.0D0
                   lwupbDiurnal(i,k,j)=0.0D0
                   swupbDiurnal(i,k,j)=0.0D0
                   uDiurnal(i,k,j)=0.0D0
                   vDiurnal(i,k,j)=0.0D0
                   uDiurnal(i+1,k,j)=0.0D0
                   vDiurnal(i,k,j+1)=0.0D0
                   qvDiurnal(i,k,j)=0.0D0
                   shumDiurnal(i,k,j)=0.0D0
                   swndrDiurnal(i,k,j)=0.0D0
                   swndfDiurnal(i,k,j)=0.0D0
                   swvdrDiurnal(i,k,j)=0.0D0
                   swvdfDiurnal(i,k,j)=0.0D0
                   u10Diurnal(i,k,j)=0.0D0
                   v10Diurnal(i,k,j)=0.0D0
                   q2Diurnal(i,k,j)=0.0D0
                ENDDO
             ENDDO
          ENDDO
       ENDDO

       ! restart step count 
       diurnal_nsteps = 0.0

       WRITE(message, *) "RASM Statistics: RESET Diurnal means ..................... diurnal_nsteps=", diurnal_nsteps
       CALL wrf_debug(200, message)

    ENDIF
    
    avg_nsteps = avg_nsteps+1.0

    ! Surface Pressure
    CALL var_accum_2d(psfc,ime-ims+1,jme-jms+1,psfcMean)
    
    ! Surface Skin Temperature
    CALL var_accum_2d(tsk,ime-ims+1,jme-jms+1,tskMean)
    
    ! Temperature at 2M
    CALL var_accum_2d(t2,ime-ims+1,jme-jms+1,tmp2mean)

    ! SENSIBLE HEAT FLUX AT THE SURFACE
    CALL var_accum_2d(hfx,ime-ims+1,jme-jms+1,hfxMean)

    ! LATENT HEAT FLUX AT THE SURFACE
    CALL var_accum_2d(lh,ime-ims+1,jme-jms+1,lhMean)

    ! INCOMING SOLAR AT SURFACE (SHORTWAVE DOWN)
    CALL var_accum_2d(swdnb,ime-ims+1,jme-jms+1,swdnbMean)

    ! INCOMING LONGWAVE AT SURFACE (LONGWAVE DOWN)
    CALL var_accum_2d(glw,ime-ims+1,jme-jms+1,glwMean)

    ! OUTGOING LONGWAVE AT SURFACE (LONGWAVE FLUX UP)
    CALL var_accum_2d(lwupb,ime-ims+1,jme-jms+1,lwupbMean)

    ! REFLECTIVE SHORTWAVE AT SURFACE 
    CALL var_accum_2d(swupb,ime-ims+1,jme-jms+1,swupbMean)

#ifdef CESMCOUPLED

    ! SWNDR - shortwave down at surface, near infrared direct component
    CALL var_accum_2d(swndr,ime-ims+1,jme-jms+1,swndrMean)

    ! SWNDF - shortwave down at surface, near infrared diffuse component
    CALL var_accum_2d(swndf,ime-ims+1,jme-jms+1,swndfMean)

    ! SWVDR - shortwave down at surface, visible direct component
    CALL var_accum_2d(swvdr,ime-ims+1,jme-jms+1,swvdrMean)

    ! SWVDF - shortwave down at surface, visible diffuse component
    CALL var_accum_2d(swvdf,ime-ims+1,jme-jms+1,swvdfMean)

#endif

    ! U SURFACE WIND SPEED (X-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         u, uMean) 

    ! V SURFACE WIND SPEED (Y-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    & 
                         v, vMean)

    ! SURFACE HUMIDITY
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         moist, qvMean) 

    ! U10 WIND SPEED (X-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         u10, u10Mean) 

    ! V10 WIND SPEED (Y-WIND COMPONENT)
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         v10, v10Mean)

    ! SPECIFIC HUMIDITY at 2M
    CALL var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         q2, q2Mean)

    ! SPECIFIC SURFACE HUMIDITY 
    CALL shum_accum_01(ims, ime, jms, jme, kms, kme,    &
                       ide, jde, ips, ipe, jps, jpe,    &
                       moist, shumMean)

    ! Surface temperature (at lowest level)
    CALL T_accum_01( ims, ime, jms, jme, kms, kme,    &
                     ide, jde, ips, ipe, jps, jpe,    &
                     t, p, pb, tMean)
  
    ! get average state
    CALL getAvgState(currentTime, xtime, dt, mean_interval, mean_output_freq, compute_avg, EmptyStr)
    IF (compute_avg) THEN
       psfcMean=psfcMean/avg_nsteps
       tskMean=tskMean/avg_nsteps
       tmp2mean=tmp2mean/avg_nsteps
       tMean=tMean/avg_nsteps
       hfxMean=hfxMean/avg_nsteps
       lhMean=lhMean/avg_nsteps
       swdnbMean=swdnbMean/avg_nsteps
       glwMean=glwMean/avg_nsteps
       lwupbMean=lwupbMean/avg_nsteps
       swupbMean=swupbMean/avg_nsteps
       uMean=uMean/avg_nsteps
       vMean=vMean/avg_nsteps
       qvMean=qvMean/avg_nsteps
       shumMean=shumMean/avg_nsteps
#ifdef CESMCOUPLED
       swndrMean=swndrMean/avg_nsteps
       swndfMean=swndfMean/avg_nsteps
       swvdrMean=swvdrMean/avg_nsteps
       swvdfMean=swvdfMean/avg_nsteps
#endif
       u10Mean=u10Mean/avg_nsteps
       v10Mean=v10Mean/avg_nsteps
       q2Mean=q2Mean/avg_nsteps
       
       CALL get_diurn_cycle(currentTime, xtime, dt, current_diurn_cycle)
       ! accummulate averages, increment counter by one
       CALL var_accum_diurnal(psfcMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, psfcDiurnal) 
       CALL var_accum_diurnal(tskMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, tskDiurnal)
       CALL var_accum_diurnal(tmp2mean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, t2Diurnal)
       CALL var_accum_diurnal(tMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, tDiurnal)
       CALL var_accum_diurnal(hfxMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, hfxDiurnal) 
       CALL var_accum_diurnal(lhMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lhDiurnal) 
       CALL var_accum_diurnal(swdnbMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swdnbDiurnal) 
       CALL var_accum_diurnal(glwMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, glwDiurnal) 
       CALL var_accum_diurnal(lwupbMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, lwupbDiurnal) 
       CALL var_accum_diurnal(swupbMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swupbDiurnal) 
       CALL var_accum_diurnal(uMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, uDiurnal) 
       CALL var_accum_diurnal(vMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, vDiurnal) 
       CALL var_accum_diurnal(qvMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, qvDiurnal) 
       CALL var_accum_diurnal(shumMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, shumDiurnal) 
#ifdef CESMCOUPLED
       CALL var_accum_diurnal(swndrMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swndrDiurnal) 
       CALL var_accum_diurnal(swndfMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swndfDiurnal) 
       CALL var_accum_diurnal(swvdrMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swvdrDiurnal) 
       CALL var_accum_diurnal(swvdfMean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, swvdfDiurnal)
#endif 
       CALL var_accum_diurnal(u10Mean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, u10Diurnal) 
       CALL var_accum_diurnal(v10Mean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, v10Diurnal) 
       CALL var_accum_diurnal(q2Mean, ime-ims+1,  NUM_DIURN_CYCLES, jme-jms+1, current_diurn_cycle, q2Diurnal) 
       
       ! increment diurnal count once a day, assuming 3hr cycles 
       if (current_diurn_cycle .eq. 8) then
          diurnal_nsteps = diurnal_nsteps + 1.0
       endif

    END IF

    ! get Diurnal average state
    CALL getDiurnalState(currentTime, xtime, dt, diurn_interval, diurnal_output_freq, compute_diurnalcycle, diurnOutDateStr)
    IF (compute_diurnalcycle) THEN
       psfcDiurnal=psfcDiurnal/diurnal_nsteps
       tskDiurnal=tskDiurnal/diurnal_nsteps
       t2Diurnal=t2Diurnal/diurnal_nsteps
       tDiurnal=tDiurnal/diurnal_nsteps
       hfxDiurnal=hfxDiurnal/diurnal_nsteps
       lhDiurnal=lhDiurnal/diurnal_nsteps
       swdnbDiurnal=swdnbDiurnal/diurnal_nsteps
       glwDiurnal=glwDiurnal/diurnal_nsteps
       lwupbDiurnal=lwupbDiurnal/diurnal_nsteps
       swupbDiurnal=swupbDiurnal/diurnal_nsteps
       uDiurnal=uDiurnal/diurnal_nsteps
       vDiurnal=vDiurnal/diurnal_nsteps
       qvDiurnal=qvDiurnal/diurnal_nsteps
       shumDiurnal=shumDiurnal/diurnal_nsteps
#ifdef CESMCOUPLED 
       swndrDiurnal=swndrDiurnal/diurnal_nsteps
       swndfDiurnal=swndfDiurnal/diurnal_nsteps
       swvdrDiurnal=swvdrDiurnal/diurnal_nsteps
       swvdfDiurnal=swvdfDiurnal/diurnal_nsteps
#endif
       u10Diurnal=u10Diurnal/diurnal_nsteps
       v10Diurnal=v10Diurnal/diurnal_nsteps
       q2Diurnal=q2Diurnal/diurnal_nsteps

       CALL WRFU_AlarmRingerOn (avgOutAlarm, rc=rc)
      
       WRITE(message, *) "RASM Statistics: Diurnal Mean Cycle computed .........................................................."
       CALL wrf_debug(200, message)

    END IF

  END SUBROUTINE diurnalcycle_output_calc

  SUBROUTINE var_accum_diurnal(var, dx, dz, dy, current_cycle, var_accum) 
    ! Subroutine accumulates diurnal variable

    IMPLICIT NONE

    INTEGER, INTENT(IN)                   :: dx, dz, dy, current_cycle
    DOUBLE PRECISION, DIMENSION(dx,dy), INTENT(IN)         :: var
    DOUBLE PRECISION, DIMENSION(dx, dz, dy), INTENT(INOUT) :: var_accum
 
    !local
    INTEGER :: k, i, j
 
    k = current_cycle ! current cycle
    DO j=1,dy
       DO i=1,dx
          var_accum(i, k, j) = var_accum(i, k, j) + var(i,j)
       ENDDO
    ENDDO

  END SUBROUTINE var_accum_diurnal

  SUBROUTINE var_accum_2d(var, dx, dy, var_accum) 
    ! Subroutine accumulates 2D variable

    IMPLICIT NONE

    INTEGER, INTENT(IN)                   :: dx, dy
    REAL, DIMENSION(dx,dy), INTENT(IN)    :: var
    DOUBLE PRECISION, DIMENSION(dx,dy), INTENT(INOUT) :: var_accum
    
    var_accum = var_accum + (var *  1.0D0 ) 

  END SUBROUTINE var_accum_2d

  SUBROUTINE var_accum_3d_01(ims, ime, jms, jme, kms, kme,    &
                             ide, jde, ips, ipe, jps, jpe,    &
                             var, var_accum) 
    ! Subroutine accumulates 3D variable at lowest level resulting in 2D output

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe

    REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), INTENT(IN):: var
    DOUBLE PRECISION, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: var_accum
    
    !local
    INTEGER              :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message

    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1

    k=1 ! lowest level 
    DO j=jps, j_end            
       DO i=ips, i_end
          var_accum(i,j) = var_accum(i,j) + ( var(i,k,j) *  1.0D0 )
       ENDDO
    ENDDO
 
  END SUBROUTINE var_accum_3d_01

  SUBROUTINE shum_accum_01(ims, ime, jms, jme, kms, kme,    &
                           ide, jde, ips, ipe, jps, jpe,    &
                           moist, var_accum) 
    ! Subroutine accumulates specific humidity at lowest level resulting in 2D output

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe

    REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), INTENT(IN):: moist
    DOUBLE PRECISION, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: var_accum

    !local
    DOUBLE PRECISION :: tmp_shum
    INTEGER              :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message

    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1

    k=1 ! lowest level 
    DO j=jps, j_end            
       DO i=ips, i_end
          if( moist(i,k,j) .gt. 0 ) then
             tmp_shum = moist(i,k,j) / (1+moist(i,k,j)) 
          else
             tmp_shum = 0.0
          endif
          var_accum(i,j) = var_accum(i,j) + (tmp_shum * 1.0D0)
       ENDDO
    ENDDO
 
  END SUBROUTINE shum_accum_01

  SUBROUTINE T_accum_01( ims, ime, jms, jme, kms, kme,    &
                         ide, jde, ips, ipe, jps, jpe,    &
                         t, p, pb, t_accum) 
    ! Subroutine accumulates Temperature at lowest level resulting in 2D output
   
    USE module_model_constants, only: t0,p0
    USE shr_const_mod

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe

    REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), INTENT(IN):: t, p, pb
    DOUBLE PRECISION, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: t_accum

    ! local
    DOUBLE PRECISION     :: t_tmp, cp, rd
    INTEGER              :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message

    rd=SHR_CONST_RDAIR
    cp=SHR_CONST_CPDAIR

    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1

    k=1 ! lowest level 
    DO j=jps, j_end            
       DO i=ips, i_end
          ! calulate surface tempature at lowest level
          t_tmp = (t(i,k,j) + t0) * (((p(i,k,j) + pb(i,k,j))/p0) ** (rd/cp))
          ! accumulate surface tempature at lowest level
          t_accum(i,j) = t_accum(i,j) + t_tmp
       ENDDO
    ENDDO

  END SUBROUTINE T_accum_01


  SUBROUTINE PMSL_T_accum_01( ims, ime, jms, jme, kms, kme,    &
                              ide, jde, ips, ipe, jps, jpe,    &
                              t, p, pb, moist, ht, psfc, pmsl_accum, t_accum)
    ! Subroutine accumulates PMSL at lowest level resulting in 2D output
   
    USE module_model_constants, only: t0,p0
    USE shr_const_mod

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
    INTEGER, INTENT(IN) :: ide, jde, ips, ipe, jps, jpe

    REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), INTENT(IN):: t, p, pb, moist
    REAL, DIMENSION(  ims:ime , jms:jme ), INTENT(IN):: ht, psfc
    DOUBLE PRECISION, DIMENSION(ims:ime , jms:jme), INTENT(INOUT) :: pmsl_accum, t_accum

    ! local
    DOUBLE PRECISION     :: t_tmp, tmp_pmsl, z, tv, g, cp, rd, xlapse
    DOUBLE PRECISION     :: alpha, tstar, tt0, alph, beta, psfc_tmp, p_tmp
    INTEGER              :: k, i, j, j_end, i_end
    CHARACTER (LEN=1024) :: message

    xlapse = 6.5D-3
    rd=SHR_CONST_RDAIR
    g=SHR_CONST_G
    cp=SHR_CONST_CPDAIR

    j_end = jpe
    i_end = ipe
    if(j_end.eq.jde) j_end=j_end-1
    if(i_end.eq.ide) i_end=i_end-1

    k=1 ! lowest level 
    do j=jps, j_end            
       do i=ips, i_end

             ! calulate surface tempature at lowest level
             t_tmp = (t(i,k,j) + t0) * (((p(i,k,j) + pb(i,k,j))/p0) ** (rd/cp))
                  
             ! accumulate surface tempature at lowest level
             t_accum(i,j) = t_accum(i,j) + t_tmp
             ! calculate virtual temperature at lowest model level
             tv = t_tmp * (1 + moist(i,k,j) * 0.61D0) * 1.0D0

             ! Calculate geopotential at surface in m2 s-2
             z = ht(i,j) * g ! terrain height in m

             alpha = rd * xlapse/g ! 0.1903, no units

             ! Get surface pressure in Pa
             psfc_tmp = psfc(i,j) * 1.0D0

             ! Calculate pressure in WRF at lowest model level in Pa
             p_tmp = (p(i,k,j) + pb(i,k,j)) * 1.0D0

             ! Use surface pressure for PMSL if we are at sea level.
             if ( abs(z/g) < 1.0D-4 )then
                tmp_pmsl = psfc_tmp
                ! Othewise, calculate based on ECMWF method
             else
                tstar = tv * (1.0D0 + alpha * (psfc_tmp/p_tmp-1.0D0)) 
                tt0 = tstar + xlapse*z/g

                if ( tstar <= 290.5D0 .and. tt0 > 290.5D0 ) then     
                   alph = rd/z * (290.5D0 - tstar)  
                else if (tstar > 290.5D0  .and. tt0 > 290.5D0) then  
                   alph = 0.0D0
                   tstar = 0.5D0 * (290.5D0 + tstar)  
                else  
                   alph=alpha  
                   if (tstar < 255.0D0) then  
                      tstar = 0.5D0 * (255.0D0 + tstar)             
                   endif
                endif
                beta = z/(rd*tstar)
                tmp_pmsl = psfc_tmp * exp( beta*(1.0D0 - alph * beta/2.0D0 + ((alph*beta)**2)/3.0D0))
             end if

             ! accumulate pmsl at lowest level
             pmsl_accum(i,j) = pmsl_accum(i,j) +  tmp_pmsl
         
          ENDDO
       ENDDO

  END SUBROUTINE PMSL_T_accum_01


  SUBROUTINE getResetState(currentTime, xtime, dt, mean_interval, output_freq, is_reset)
    ! Subroutine DESCRIPTION:
    ! Determine if data stats are to be reset at the current time step
    ! True implies that stats are reinitialized

    ! USES:
    USE module_utility
    USE module_model_constants
    !USE ESMF_Mod

    IMPLICIT NONE

    TYPE(WRFU_Time), INTENT(IN)   :: currentTime
    INTEGER, INTENT(IN)           :: mean_interval
    REAL, INTENT(IN)              :: dt, xtime 
    INTEGER, INTENT(IN)           :: output_freq
    LOGICAL, INTENT(INOUT)        :: is_reset
   
    ! LOCAL VARIABLES:
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time)         :: prevTime
 
    integer :: yr         !nstep year
    integer :: mon        !nstep months (1 -> 12)   
    integer :: prevMon    !nstep-1 months (1 -> 12)
    integer :: day        !nstep days (1 -> 31)
    integer :: dtime

    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=10) ::str_day
    CHARACTER (LEN=80) ::filedate

    CHARACTER (LEN=1024) :: message

    dtime = INT (dt)

    ! Determine if time to reset data stats
    is_reset = .false.

    if (output_freq .eq. MONTHLY) then
       ! get date for current time_step
       call WRFU_TimeGet( currentTime, mm=mon)   

       ! get date for previous time_step
       call WRFU_TimeIntervalSet( off, s=dtime)
       prevTime = currentTime - off
       call WRFU_TimeGet( prevTime, mm=prevMon)

       if ( (mon-prevMon) /= 0) then
          is_reset = .true.

          WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL RESET condition met (return TRUE) "
          CALL wrf_debug(200, message) 
       endif
    else
       if ( MOD(NINT(xtime*60./dt),NINT(mean_interval/dt)) == 0 ) then
          is_reset = .true.
   
          WRITE(message, *) "RASM Statistics: STATIC_INTERVAL RESET condition met (return TRUE) "
          CALL wrf_debug(200, message) 
       endif
    endif

  END SUBROUTINE getResetState

  SUBROUTINE getAvgState(currentTime, xtime, dt, mean_interval, output_freq, compute_avg, OutDateStr)
    ! Subroutine DESCRIPTION:
    ! Determine if data averages are to be calculated at the current time step
    ! True implies calculate avergaes

   ! USES:
    USE module_utility
    USE module_model_constants
    !USE ESMF_Mod

    IMPLICIT NONE

    TYPE(WRFU_Time), INTENT(IN)   :: currentTime
    INTEGER, INTENT(IN)           :: mean_interval
    REAL, INTENT(IN)              :: dt, xtime 
    INTEGER, INTENT(IN)           :: output_freq
    LOGICAL, INTENT(INOUT)        :: compute_avg
    CHARACTER(*), INTENT(INOUT)   :: OutDateStr

    ! LOCAL VARIABLES:
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time)         :: nextTime
    TYPE(WRFU_Time)         :: prevTime
 
    integer :: yr         !nstep year
    integer :: mon        !nstep months (1 -> 12)
    integer :: nextMon    !nstep+1 months (1 -> 12)
    integer :: prevMon    !nstep-1 months (1 -> 12)
    integer :: day        !nstep days (1 -> 31)
    integer :: hr         !nstep hrs
    integer :: min        !nstep mins 
    integer :: sec        !nstep secs
    integer :: totalsec   !nstep total secs
    integer :: dtime

    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=10) ::str_day
    CHARACTER (LEN=10) ::str_sec
    CHARACTER (LEN=80) ::filedate

    CHARACTER (LEN=1024) :: message

    dtime = INT (dt)

    ! Determine if time to average data 
    compute_avg = .false.
    if ( output_freq .EQ. MONTHLY) then

       ! get date for current time_step 
       call WRFU_TimeGet( currentTime, mm=mon)   

       ! get date for next time_step
       call WRFU_TimeIntervalSet( off, s=dtime)
       nextTime = currentTime + off
       call WRFU_TimeGet( nextTime, mm=nextMon)

       if ( (nextMon-mon) /= 0)  then
          compute_avg = .true.

          WRITE(message, *) "RASM Statistics: MONTHLY_INTERVAL AVG condition met (return TRUE) "
          CALL wrf_debug(200, message) 
       endif

    else
       if ((MOD(NINT((xtime+dt/60.)*60./dt),NINT(mean_interval/dt)) == 0)) then
          compute_avg = .true.
   
          WRITE(message, *) "RASM Statistics: STATIC_INTERVAL AVG condition met (return TRUE) "
          CALL wrf_debug(200, message) 
       endif
    endif

    ! generate date used for hourly, min and sec averages
    if (compute_avg) then
       IF ( (output_freq .ne. MONTHLY)  .and. (output_freq .ne. DAYS)) THEN
  
          ! get date for next time_step
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon, dd=day, h=hr, m=min, s=sec)   

          WRITE(str_yr, '(I4.4)'), yr
          WRITE(str_mon, '(I2.2)'), mon
          WRITE(str_day, '(I2.2)'), day
          totalsec = (hr * 60 * 60) + (min * 60) + sec 
          WRITE(str_sec, '(I5.5)'), totalsec
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)//"-"//trim(str_sec) 
          OutDateStr = filedate

          WRITE(message, *) "RASM Statistics:  STATIC_INTERVAL AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message)

       ELSE IF ( output_freq .eq. MONTHLY ) THEN
          ! get avg date 
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon)  
          IF (mon .eq. 1) THEN
             mon = 12
             yr = yr - 1
          ELSE
             mon = mon - 1
          ENDIF
          WRITE(str_yr, '(I4.4)'), yr
          WRITE(str_mon, '(I2.2)'), mon
          filedate = trim(str_yr)//"-"//trim(str_mon)
          OutDateStr = filedate
   
          WRITE(message, *) "RASM Statistics:  AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message) 
          
       ELSE IF (output_freq .eq. DAYS ) THEN
          ! get avg date 
          call WRFU_TimeIntervalSet( off, s=mean_interval-dtime)
          prevTime = currentTime - off
          call WRFU_TimeGet( prevTime, yy=yr, mm=mon, dd=day)   
          WRITE(str_yr, '(I4.4)'), yr
          WRITE(str_mon, '(I2.2)'), mon
          WRITE(str_day, '(I2.2)'), day
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)
          OutDateStr = filedate
          
          WRITE(message, *) "RASM Statistics:  AVG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message) 
       ENDIF
    endif

  END SUBROUTINE getAvgState

  SUBROUTINE getDiurnalState(currentTime, xtime, dt, diurn_interval, output_freq, compute_diurn, OutDateStr)
    ! Subroutine DESCRIPTION:
    ! Determine if data diurnal averages are to be calculated at the current time step
    ! True implies that diurnal averages are to be calculated 

   ! USES:
    USE module_utility
    USE module_model_constants
    !USE ESMF_Mod

    IMPLICIT NONE

    TYPE(WRFU_Time), INTENT(IN)   :: currentTime
    REAL, INTENT(IN)              :: dt, xtime 
    INTEGER, INTENT(IN)           :: output_freq
    integer, INTENT(IN)           :: diurn_interval
    LOGICAL, INTENT(INOUT)        :: compute_diurn
    CHARACTER(*), INTENT(INOUT)   :: OutDateStr

    ! LOCAL VARIABLES:
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time)         :: nextTime
    TYPE(WRFU_Time)         :: prevTime
 
    integer :: yr         !nstep year
    integer :: mon        !nstep months (1 -> 12)
    integer :: nextMon    !nstep+1 months (1 -> 12)
    integer :: dtime
    

    CHARACTER (LEN=10) ::str_yr
    CHARACTER (LEN=10) ::str_mon
    CHARACTER (LEN=80) ::filedate

    CHARACTER (LEN=1024) :: message
   
    integer :: mean_interval
    CHARACTER (LEN=10) ::str_day
    integer :: day        !nstep days (1 -> 31)

    dtime = INT (dt)

    ! Determine if time to average data 
    compute_diurn = .false.
   
    if ( output_freq .EQ. MONTHLY) then
       ! get date for current time_step 
       call WRFU_TimeGet( currentTime, mm=mon)   

       ! get date for next time_step
       call WRFU_TimeIntervalSet( off, s=dtime)
       nextTime = currentTime + off
       call WRFU_TimeGet( nextTime, mm=nextMon)
       
       if ( (nextMon-mon) /= 0)  then
          compute_diurn = .true.

          WRITE(message, *) "RASM Statistics: Diurnal AVG condition met (return TRUE) "
          CALL wrf_debug(200, message) 
       endif
    else
       if ((MOD(NINT((xtime+dt/60.)*60./dt),NINT(diurn_interval/dt)) == 0)) then
          compute_diurn = .true.
   
          WRITE(message, *) "RASM Statistics: Diurnal AVG condition met  DAILY TEST (return TRUE) "
          CALL wrf_debug(200, message) 
       endif
    endif

    ! generate date used for hourly, min and sec averages
    if (compute_diurn) then

       if ( output_freq .EQ. MONTHLY) then
          ! get date 
          call WRFU_TimeIntervalSet( off, s=dtime)
          nextTime = currentTime + off
          call WRFU_TimeGet( nextTime, yy=yr, mm=mon)  
          IF (mon .eq. 1) THEN
             mon = 12
             yr = yr - 1
          ELSE
             mon = mon - 1
          ENDIF
          WRITE(str_yr, '(I4.4)'), yr
          WRITE(str_mon, '(I2.2)'), mon
          filedate = trim(str_yr)//"-"//trim(str_mon)
          OutDateStr = filedate
   
          WRITE(message, *) "RASM Statistics:  Diurnal ACG condition met ......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message) 
       else
          ! get avg date 
          call WRFU_TimeIntervalSet( off, s=diurn_interval-dtime)
          prevTime = currentTime - off
          call WRFU_TimeGet( prevTime, yy=yr, mm=mon, dd=day)   
          WRITE(str_yr, '(I4.4)'), yr
          WRITE(str_mon, '(I2.2)'), mon
          WRITE(str_day, '(I2.2)'), day
          filedate = trim(str_yr)//"-"//trim(str_mon)//"-"//trim(str_day)
          OutDateStr = filedate
          
          WRITE(message, *) "RASM Statistics:  Diurnal AVG condition met DAILY TEST......... avgOutDateStr:", trim(OutDateStr)
          CALL wrf_debug(200, message) 
       endif
    endif

  END SUBROUTINE getDiurnalState

  SUBROUTINE  get_diurn_cycle(currentTime, xtime, dt, diurn_cycle)
    ! Subroutine DESCRIPTION:
    ! Get the current diurnal cycle

    ! USES:
    USE module_utility
    USE module_model_constants
    !USE ESMF_Mod

    IMPLICIT NONE

    TYPE(WRFU_Time), INTENT(IN)   :: currentTime
    REAL, INTENT(IN)              :: dt, xtime 
    INTEGER, INTENT(INOUT)        :: diurn_cycle

    ! LOCAL VARIABLES:
    TYPE(WRFU_TimeInterval) :: off
    TYPE(WRFU_Time)         :: nextTime
    TYPE(WRFU_Time)         :: prevTime
 
    integer :: yr         !nstep year
    integer :: mon        !nstep months (1 -> 12)
    integer :: day        !nstep days (1 -> 31)
    integer :: hr         !nstep hrs
    integer :: dtime


    CHARACTER (LEN=1024) :: message

    dtime = INT (dt)
    diurn_cycle = -1

    ! get date for next time_step
    call WRFU_TimeIntervalSet( off, s=dtime)
    nextTime = currentTime + off
    call WRFU_TimeGet( nextTime, yy=yr, mm=mon, dd=day, h=hr)   
  
    ! This is a 3hr cycle, therfore it the diurn_cycle 
    ! hr should be either (0,3,6,9,12,15,18 or 21)
    if (hr .eq. 3) then
       diurn_cycle = 1
    else if (hr .eq. 6) then
       diurn_cycle = 2
    else if (hr .eq. 9) then
       diurn_cycle = 3
    else if (hr .eq. 12) then
       diurn_cycle = 4
    else if (hr .eq. 15) then
       diurn_cycle = 5
    else if (hr .eq. 18) then
       diurn_cycle = 6
    else if (hr .eq. 21) then
       diurn_cycle = 7
    else if (hr .eq. 0) then
       diurn_cycle = 8
    else
       WRITE (message, * )"RASM Statistics:: DIURNAL ERROR -- error -- ERROR -- error : Did not find valid diurnal cycle"
       CALL wrf_debug(0, message) 
       WRITE (message, * )"RASM Statistics:: DIURNAL ERROR -- Valid diurnal cycles (0,3,6,9,12,15,18 or 21) ... reported ",  diurn_cycle
       CALL wrf_error_fatal ( TRIM(message) )  
    endif

  END SUBROUTINE  get_diurn_cycle
!-------------------------------------------------------
! end Amy Solomon 
!-------------------------------------------------------

END MODULE module_diag_misc
#endif
