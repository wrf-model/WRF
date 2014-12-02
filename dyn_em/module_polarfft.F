MODULE module_polarfft

  USE module_model_constants
  USE module_wrf_error
  CHARACTER (LEN=256) , PRIVATE :: a_message

CONTAINS

SUBROUTINE couple_scalars_for_filter ( field    &
                 ,mu,mub                        &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

   INTEGER :: i , j , k

   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)*(mu(i,j)+mub(i,j))
   END DO
   END DO
   END DO

END SUBROUTINE couple_scalars_for_filter

SUBROUTINE uncouple_scalars_for_filter ( field    &
                 ,mu,mub                        &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe
   REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: field
   REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: mu,mub

   INTEGER :: i , j , k

   DO j = jps, MIN(jpe,jde-1)
   DO k = kps, kpe-1
   DO i = ips, MIN(ipe,ide-1)
      field(i,k,j)=field(i,k,j)/(mu(i,j)+mub(i,j))
   END DO
   END DO
   END DO

END SUBROUTINE uncouple_scalars_for_filter

SUBROUTINE pxft ( grid                          &
                 ,lineno       &
                 ,flag_uv,flag_rurv             &
                 ,flag_wph,flag_ww              &
                 ,flag_t                        &
                 ,flag_mu,flag_mut              &
                 ,flag_moist                    &
                 ,flag_chem                     &
                 ,flag_tracer                   &
                 ,flag_scalar                   &
                 ,fft_filter_lat, dclat         &
                 ,actual_distance_average       &
                 ,pos_def                       &
                 ,swap_pole_with_next_j         &
                 ,moist,chem,tracer,scalar      &
                 ,ids,ide,jds,jde,kds,kde       &
                 ,ims,ime,jms,jme,kms,kme       &
                 ,ips,ipe,jps,jpe,kps,kpe       &
                 ,imsx,imex,jmsx,jmex,kmsx,kmex &
                 ,ipsx,ipex,jpsx,jpex,kpsx,kpex )
   USE module_state_description
   USE module_domain, ONLY : domain
#ifdef DM_PARALLEL
   USE module_dm, ONLY : local_communicator, mytask, ntasks, ntasks_x, ntasks_y &
                       , local_communicator_periodic, itrace                    &
                       , local_communicator_x 
   USE module_driver_constants
#if 0
   USE module_comm_dm, ONLY : &
                 XPOSE_POLAR_FILTER_V_z2x_sub           &
                ,XPOSE_POLAR_FILTER_V_x2z_sub           &
                ,XPOSE_POLAR_FILTER_U_z2x_sub           &
                ,XPOSE_POLAR_FILTER_U_x2z_sub           &
                ,XPOSE_POLAR_FILTER_T_z2x_sub           &
                ,XPOSE_POLAR_FILTER_T_x2z_sub           &
                ,XPOSE_POLAR_FILTER_W_z2x_sub           &
                ,XPOSE_POLAR_FILTER_W_x2z_sub           &
                ,XPOSE_POLAR_FILTER_PH_z2x_sub          &
                ,XPOSE_POLAR_FILTER_PH_x2z_sub          &
                ,XPOSE_POLAR_FILTER_WW_z2x_sub          &
                ,XPOSE_POLAR_FILTER_WW_x2z_sub          &
                ,XPOSE_POLAR_FILTER_RV_z2x_sub          &
                ,XPOSE_POLAR_FILTER_RV_x2z_sub          &
                ,XPOSE_POLAR_FILTER_RU_z2x_sub          &
                ,XPOSE_POLAR_FILTER_RU_x2z_sub          &
                ,XPOSE_POLAR_FILTER_MOIST_z2x_sub       &
                ,XPOSE_POLAR_FILTER_MOIST_x2z_sub       &
                ,XPOSE_POLAR_FILTER_CHEM_z2x_sub        &
                ,XPOSE_POLAR_FILTER_MOIST_x2z_sub       &
                ,XPOSE_POLAR_FILTER_TRACER_z2x_sub      &
                ,XPOSE_POLAR_FILTER_TRACER_x2z_sub      &
                ,XPOSE_POLAR_FILTER_SCALAR_z2x_sub      &
                ,XPOSE_POLAR_FILTER_SCALAR_x2z_sub
#endif
#else
   USE module_dm
#endif
   IMPLICIT NONE
   !  Input data.
   TYPE(domain) , TARGET          :: grid
integer, intent(in) :: lineno
integer myproc, i, j, k
   LOGICAL, INTENT(IN) :: actual_distance_average
   LOGICAL, INTENT(IN) :: pos_def
   LOGICAL, INTENT(IN) :: swap_pole_with_next_j
   INTEGER, INTENT(IN) :: ids,ide,jds,jde,kds,kde       &
                         ,ims,ime,jms,jme,kms,kme       &
                         ,ips,ipe,jps,jpe,kps,kpe       &
                         ,imsx,imex,jmsx,jmex,kmsx,kmex &
                         ,ipsx,ipex,jpsx,jpex,kpsx,kpex
   REAL  , INTENT(IN) :: fft_filter_lat
   REAL,    INTENT(IN) :: dclat
   INTEGER, INTENT(IN) :: flag_uv                       &
                         ,flag_rurv                     &
                         ,flag_ww                       &
                         ,flag_t,flag_wph               &
                         ,flag_mu,flag_mut              &
                         ,flag_moist                    &
                         ,flag_chem                     &
                         ,flag_tracer                   &
                         ,flag_scalar
    REAL, DIMENSION(ims:ime,kms:kme,jms:jme,*) , INTENT(INOUT) :: moist, chem, scalar,tracer

   ! Local
   LOGICAL piggyback_mu, piggyback_mut
   INTEGER ij, k_end

#ifdef DM_PARALLEL
#else
   INTEGER itrace
#endif


   piggyback_mu  = flag_mu .EQ. 1
   piggyback_mut = flag_mut .EQ. 1

!<DESCRIPTION>
!
! The idea is that this parallel transpose fft routine can be
! called at various points in the solver (solve_em) and it will filter
! the correct fields based on the flag arguments.  There are two 2d
! fields mu_2 and mut  that may need to be filtered too. Since a two-d
! parallel transpose would be inefficient and because the fields that are
! not staggered in z have an extra layer anyway, these can be
! piggybacked.  This is handled using latches to makes sure that *somebody*
! carries one or both of these on their back through the filtering and then
! copies them back afterwards. IMPORTANT NOTE: for simplicity, this routine
! is not completely general.  It makes the following assumptions:
! 
! 1) If both flag_mu and flag_mut are specified then flag_uv is also specified
! 
! 2) If flag_uv is not specified, then only flag_mu and not flag_mut can be
! 
! 3) If flag_mu is specified than either flag_uv or flag_t must be
!
! This is designed to keep the clutter to a minimum in solve_em. 
! This is not intended to be a general abstraction of the polar filtering
! calls in in WRF solvers or if the solve_em algorithms change.
! If the needs of the calling solver change, this logic may have to be
! rewritten.
!
!</DESCRIPTION>
!write(0,*)__FILE__,__LINE__,' short circuit '
!return

!write(0,*)'pxft called from ',lineno
call wrf_get_myproc(myproc)
!write(20+myproc,*)ipex-ipsx+1,jpex-jpsx+1,' clat_xxx '
!do j = jpsx, jpex
!do i = ipsx, ipex
!write(20+myproc,*)grid%clat_xxx(i,j)
!enddo
!enddo

!!!!!!!!!!!!!!!!!!!!!!!
! U & V
   IF ( flag_uv .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%u_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe) 
     ENDIF
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_V_z2x.inc"

     CALL polar_filter_3d( grid%v_xxx, grid%clat_xxx, .false.,     &
                                fft_filter_lat, dclat,                 &
                                ids, ide, jds, jde, kds, kde-1,         &
                                imsx, imex, jmsx, jmex, kmsx, kmex,     &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kde-1,kpex ) )

# include "XPOSE_POLAR_FILTER_V_x2z.inc"
# include "XPOSE_POLAR_FILTER_U_z2x.inc"
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%u_xxx, grid%clat_xxx, piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )

# include "XPOSE_POLAR_FILTER_U_x2z.inc"
#else

     CALL polar_filter_3d( grid%v_2, grid%clat, .false.,     &
                                fft_filter_lat, dclat,             &
                                ids, ide, jds, jde, kds, kde,       &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, MIN(kde-1,kpe) )

     k_end = MIN(kde-1,kpe)
     IF ( piggyback_mu ) k_end = MIN(kde,kpe)

     CALL polar_filter_3d( grid%u_2, grid%clat, piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,     &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, k_end )

#endif

     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%u_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! T
   IF ( flag_t .EQ. 1 ) THEN
     IF ( piggyback_mu ) THEN
       grid%t_2(ips:ipe,kde,jps:jpe) = grid%mu_2(ips:ipe,jps:jpe)
     ENDIF
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_T_z2x.inc"
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mu ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%t_xxx, grid%clat_xxx,piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,     &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%t_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF

# include "XPOSE_POLAR_FILTER_T_x2z.inc"
#else
     k_end = MIN(kde-1,kpe)
     IF ( piggyback_mu ) k_end = MIN(kde,kpe)

     CALL polar_filter_3d( grid%t_2, grid%clat, piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,     &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, k_end )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%t_2 , grid%clat , grid%msft , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, k_end )
     END IF

#endif
     IF ( piggyback_mu ) THEN
       grid%mu_2(ips:ipe,jps:jpe) = grid%t_2(ips:ipe,kde,jps:jpe)
       piggyback_mu = .FALSE.
     ENDIF
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! W and PH
   IF ( flag_wph .EQ. 1 ) THEN
      ! W AND PH USE ALL LEVELS SO NEVER PIGGYBACK, MU IS OUT OF LUCK HERE
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_W_z2x.inc"
      CALL polar_filter_3d( grid%w_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )

# include "XPOSE_POLAR_FILTER_W_x2z.inc"
# include "XPOSE_POLAR_FILTER_PH_z2x.inc"

      CALL polar_filter_3d( grid%ph_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )

# include "XPOSE_POLAR_FILTER_PH_x2z.inc"
#else

      CALL polar_filter_3d( grid%w_2, grid%clat,  .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe )

      CALL polar_filter_3d( grid%ph_2, grid%clat, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe )
#endif
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! WW
   IF ( flag_ww .EQ. 1 ) THEN
      ! WW USES ALL LEVELS SO NEVER PIGGYBACK, MU IS OUT OF LUCK HERE
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_WW_z2x.inc"
      CALL polar_filter_3d( grid%ww_xxx, grid%clat_xxx, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 imsx, imex, jmsx, jmex, kmsx, kmex, &
                                 ipsx, ipex, jpsx, jpex, kpsx, kpex )

# include "XPOSE_POLAR_FILTER_WW_x2z.inc"
#else
      CALL polar_filter_3d( grid%ww_m, grid%clat, .false.,     &
                                 fft_filter_lat, 0.,                &
                                 ids, ide, jds, jde, kds, kde,       &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe )
#endif
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! RU AND RV
   IF ( flag_rurv .EQ. 1 ) THEN
     IF ( piggyback_mut ) THEN
       grid%ru_m(ips:ipe,kde,jps:jpe) = grid%mut(ips:ipe,jps:jpe)
     ENDIF
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_RV_z2x.inc"
     CALL polar_filter_3d( grid%rv_xxx, grid%clat_xxx, .false.,     &
                                fft_filter_lat, dclat,             &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )

# include "XPOSE_POLAR_FILTER_RV_x2z.inc"
# include "XPOSE_POLAR_FILTER_RU_z2x.inc"
     k_end = MIN(kde-1,kpex)
     IF ( piggyback_mut ) k_end = MIN(kde,kpex)

     CALL polar_filter_3d( grid%ru_xxx, grid%clat_xxx, piggyback_mut,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde,       &
                                imsx, imex, jmsx, jmex, kmsx, kmex, &
                                ipsx, ipex, jpsx, jpex, kpsx, k_end )
#include "XPOSE_POLAR_FILTER_RU_x2z.inc"
#else

     CALL polar_filter_3d( grid%rv_m, grid%clat, .false.,     &
                                fft_filter_lat, dclat,             &
                                ids, ide, jds, jde, kds, kde,       &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, MIN(kde-1,kpe) )

     k_end = MIN(kde-1,kpe)
     IF ( piggyback_mut ) k_end = MIN(kde,kpe)

     CALL polar_filter_3d( grid%ru_m, grid%clat, piggyback_mut,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,       &
                                ims, ime, jms, jme, kms, kme, &
                                ips, ipe, jps, jpe, kps, k_end )
#endif
     IF ( piggyback_mut ) THEN
       grid%mut(ips:ipe,jps:jpe) = grid%ru_m(ips:ipe,kde,jps:jpe)
       piggyback_mut = .FALSE.
     ENDIF
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! MOIST
   IF ( flag_moist .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_moist
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_MOIST_z2x.inc"
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
# include "XPOSE_POLAR_FILTER_MOIST_x2z.inc"
#else
     CALL polar_filter_3d( moist(ims,kms,jms,itrace), grid%clat, .false.,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( moist(ims,kms,jms,itrace) , grid%clat , grid%msft , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )
     END IF
#endif
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! CHEM
   IF ( flag_chem .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_chem
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_CHEM_z2x.inc"
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
# include "XPOSE_POLAR_FILTER_CHEM_x2z.inc"
#else
     CALL polar_filter_3d( chem(ims,kms,jms,itrace), grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( chem(ims,kms,jms,itrace) , grid%clat , grid%msft , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )
     END IF
#endif
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! TRACER
   IF ( flag_tracer .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_tracer
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_TRACER_z2x.inc"

     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF

# include "XPOSE_POLAR_FILTER_TRACER_x2z.inc"
#else
     CALL polar_filter_3d( tracer(ims,kms,jms,itrace), grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( tracer(ims,kms,jms,itrace) , grid%clat , grid%msft , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )
     END IF
#endif
   ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
! SCALAR
   IF ( flag_scalar .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_scalar
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_SCALAR_z2x.inc"
     CALL polar_filter_3d( grid%fourd_xxx , grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( grid%fourd_xxx , grid%clat_xxx , grid%mf_xxx , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             imsx, imex, jmsx, jmex, kmsx, kmex, &
                             ipsx, ipex, jpsx, jpex, kpsx, kpex )
     END IF
# include "XPOSE_POLAR_FILTER_SCALAR_x2z.inc"
#else
     CALL polar_filter_3d( scalar(ims,kms,jms,itrace) , grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )

     IF ( actual_distance_average ) THEN
        CALL filter_tracer ( scalar(ims,kms,jms,itrace) , grid%clat , grid%msft , &
                             grid%fft_filter_lat , grid%mf_fft , &
                             pos_def, swap_pole_with_next_j , &
                             ids, ide, jds, jde, kds, kde , & 
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, MIN(kpe,kde-1) )
     END IF
#endif
   ENDIF

   IF ( flag_mu .EQ. 1 .AND. piggyback_mu ) THEN
      CALL wrf_error_fatal("mu needed to get piggybacked on a transpose and did not")
   ENDIF
   IF ( flag_mut .EQ. 1 .AND. piggyback_mut ) THEN
      CALL wrf_error_fatal("mut needed to get piggybacked on a transpose and did not")
   ENDIF

!write(0,*)'pxft back to ',lineno
   RETURN
END SUBROUTINE pxft

SUBROUTINE polar_filter_3d( f, xlat, piggyback, fft_filter_lat, dvlat, &
                            ids, ide, jds, jde, kds, kde,    &
                            ims, ime, jms, jme, kms, kme,    &
                            its, ite, jts, jte, kts, kte     )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
  REAL   ,       INTENT(IN   ) :: fft_filter_lat

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  xlat
  REAL , INTENT(IN) ::  dvlat
  LOGICAL , INTENT(IN) :: piggyback

  REAL , DIMENSION(1:ide-ids,1:kte-kts+1) :: sheet
  REAL , DIMENSION(1:kte-kts+1) :: sheet_total
  REAL :: lat, avg, rnboxw
  INTEGER :: ig, jg, i, j, j_end, nx, ny, nmax, kw
  INTEGER :: k, nboxw, nbox2, istart, iend, overlap
  INTEGER, DIMENSION(6) :: wavenumber = (/ 1, 3, 7, 10, 13, 16 /)

  INTEGER :: fftflag

  ! Variables will stay in domain form since this routine is meaningless
  ! unless tile extent is the same as domain extent in E/W direction, i.e.,
  ! the processor has access to all grid points in E/W direction.
  ! There may be other ways of doing FFTs, but we haven't learned them yet...

  ! Check to make sure we have full access to all E/W points
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_polarfft: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
  END IF

  fftflag= 1  ! call double-precision fft
! fftflag= 0  ! call single-precision fft

  nx = ide-ids ! "U" stagger variables will be repeated by periodic BCs
  ny = kte-kts+1 ! we can filter extra level for variables that are non-Z-staggered
  lat = 0.
  j_end = MIN(jte, jde-1)
  IF (dvlat /= 0. .and. j_end == jde-1) j_end = jde
  DO j = jts, j_end
     ! jg is the J index in the global (domain) span of the array.
     jg = j-jds+1

     ! determine whether or not to filter the data

     if(xlat(ids,j).gt.0.) then
        lat = xlat(ids,j)-dvlat
     else
        lat = xlat(ids,j)+dvlat
     endif

     IF (abs(lat) >= fft_filter_lat) THEN

        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
        END DO

        call polar_filter_fft_2d_ncar(nx,ny,sheet,lat,fft_filter_lat,piggyback,fftflag)

        DO k=kts,kte
           DO i=ids,ide-1
              f(i,k,j) = sheet(i-ids+1,k-kts+1)
           END DO
           ! setting up ims-ime with x periodicity:
           ! enforce periodicity as in set_physical_bc3d

           DO i=1,ids-ims
              f(ids-i,k,j)=f(ide-i,k,j)
           END DO
           DO i=1,ime-ide+1
              f(ide+i-1,k,j)=f(ids+i-1,k,j)
           END DO
        END DO

     END IF
  END DO ! outer j (latitude) loop

END SUBROUTINE polar_filter_3d

!------------------------------------------------------------------------------

SUBROUTINE polar_filter_fft_2d_ncar(nx,ny,fin,lat,filter_latitude,piggyback,fftflag)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin
  REAL , INTENT(IN) :: lat, filter_latitude
  LOGICAL, INTENT(IN) :: piggyback

  REAL :: pi, rcosref, freq, c, cf

  INTEGER :: k, fftflag
  REAL, DIMENSION(NX,NY) :: work
  REAL, DIMENSION(NX+15) :: wsave
  REAL(KIND=8), DIMENSION(NX,NY) :: fin8, work8 
  REAL(KIND=8), DIMENSION(NX+15) :: wsave8

  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp

  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk

  REAL, PARAMETER :: alpha = 0.0
  REAL :: factor_k

  INTEGER :: ntop

  pi = ACOS(-1.)
  rcosref = 1./COS(filter_latitude*pi/180.)

!  we are following the naming convention of the fftpack5 routines

  n = nx
  lot = ny
  lensav = n+15
  inc = 1
  lenr = nx*ny
  jump = nx
  lenwrk = lenr
  ntop = ny
  IF(piggyback) ntop = ny-1

!  forward transform
!  initialize coefficients, place in wsave 
!   (should place this in init and save wsave at program start)

  if(fftflag.eq.0) then
     call rfftmi(n,wsave,lensav,ier)
  else
     call dfft1i(n,wsave8,lensav,ier)
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmi ',ier
    CALL wrf_message ( a_message ) 
  END IF

!  do the forward transform

  if(fftflag.eq.0) then
     call rfftmf(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else 
     fin8 = fin
     do k=1,ny 
        call dfft1f(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo 
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmf ',ier
    CALL wrf_message ( a_message ) 
  END IF

  if(MOD(n,2) == 0) then
    nh = n/2 - 1
  else
    nh = (n-1)/2
  end if

  DO j=1,ny
   fp(1,j) = 1.
  ENDDO

  DO i=2,nh+1
    freq=REAL(i-1)/REAL(n)
    c = (rcosref*COS(lat*pi/180.)/SIN(freq*pi))**2

!    c = MAX(0.,MIN(1.,c))
    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(2*(i-1),j) = cf
      fp(2*(i-1)+1,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(2*(i-1),ny) = cf
      fp(2*(i-1)+1,ny) = cf
    endif
  END DO

  IF(MOD(n,2) == 0) THEN
    c = (rcosref*COS(lat*pi/180.))**2
!    c = MAX(0.,MIN(1.,c))
    do j=1,ntop
      factor_k = (1.-alpha)+alpha*min(1.,float(ntop - j)/10.)
      cf = c*factor_k*factor_k
      cf = MAX(0.,MIN(1.,cf))
      fp(n,j) = cf
    enddo
    if(piggyback) then
      cf = MAX(0.,MIN(1.,c))
      fp(n,ny) = cf
    endif
  END IF

  if(fftflag.eq.0) then
     do j=1,ny
       do i=1,nx
          fin(i,j) = fp(i,j)*fin(i,j)
       enddo
     enddo
  else
     do j=1,ny
       do i=1,nx
          fin8(i,j) = fp(i,j)*fin8(i,j)
       enddo
     enddo
  endif

!  do the backward transform

  if(fftflag.eq.0) then
     call rfftmb(lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier)
  else
     do k=1,ny 
        call dfft1b(n, inc, fin8(1,k), lenr, wsave8, lensav, work8, lenwrk, ier)
     enddo
     fin= fin8     
  endif

  IF(ier /= 0) THEN
    write(a_message,*) ' error in rfftmb ',ier
    CALL wrf_message ( a_message ) 
  END IF

END SUBROUTINE polar_filter_fft_2d_ncar

!---------------------------------------------------------------------

   SUBROUTINE filter_tracer ( tr3d_in , xlat , msftx , &
                              fft_filter_lat , mf_fft , &
                              pos_def , swap_pole_with_next_j , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat , mf_fft
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: tr3d_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx
      LOGICAL , INTENT(IN) :: pos_def , swap_pole_with_next_j


      !  Local vars

      INTEGER :: i , j , j_lat_pos , j_lat_neg , k
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      INTEGER :: i_left , i_right , ii , count
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: tr_in, tr_out
      CHARACTER (LEN=256) :: message

      !  The filtering is a simple average on a latitude loop.  Possibly a LONG list of
      !  numbers.  We assume that ALL of the 2d arrays have been transposed so that
      !  each patch has the entire domain size of the i-dimension available.

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal ( 'filtering assumes all values on X' )
      END IF

      !  Starting at the south pole, we find where the
      !  grid distance is big enough, then go back a point.  Continuing to the
      !  north pole, we find the first small grid distance.  These are the
      !  computational latitude loops and the associated computational poles.

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = MIN(jde-1,jte) , jts , -1
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .GE. fft_filter_lat ) THEN
               j_lat_neg = j 
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      !  Initialize the starting values for the averages.

      DO k = kts, kte
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr_in(i,j) = tr3d_in(i,k,j)
               tr_out(i,j) = tr_in(i,j)
            END DO
         END DO

         !  Filter the fields at the negative lats.
   
         DO j = MIN(j_lat_neg,jte) , jts , -1
!           i_kicker = MIN( MAX ( NINT(msftx(its,j)/2) , 1 ) , (ide - ids) / 2 )
            i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
!           WRITE (message,FMT='(a,i4,a,i4,f6.2,1x,f6.1,f6.1)') 'DAVE SOUTH j = ' , j, ', kicker = ',i_kicker,xlat(its,j),msftx(its,j),mf_fft
!           CALL wrf_debug ( 0 , TRIM(message) )
            DO i = its , MIN(ide-1,ite)
               sum = 0.
               count = 0
               DO ik = 1 , i_kicker/2
                  ii = i-ik
                  IF ( ii .GE. ids ) THEN
                     i_left = ii
                  ELSE
                     i_left = ( ii - ids ) + (ide-1)+1
                  END IF
                  ii = i+ik
                  IF ( ii .LE. ide-1 ) THEN
                     i_right = ii
                  ELSE
                     i_right = ( ii - (ide-1) ) + its-1
                  END IF
                  sum = sum + tr_in(i_left,j) + tr_in(i_right,j)
                  count = count + 1
               END DO
               tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * count + 1 )
            END DO
         END DO
   
         !  Filter the fields at the positive lats.
   
         DO j = MAX(j_lat_pos,jts) , MIN(jde-1,jte)
!           i_kicker = MIN( MAX ( NINT(msftx(its,j)/2) , 1 ) , (ide - ids) / 2 )
            i_kicker = MIN( MAX ( NINT(msftx(its,j)/mf_fft/2) , 1 ) , (ide - ids) / 2 )
!           WRITE (message,FMT='(a,i4,a,i4,f6.2,1x,f6.1,f6.1)') 'DAVE NORTH j = ' , j, ', kicker = ',i_kicker,xlat(its,j),msftx(its,j),mf_fft
!           CALL wrf_debug ( 0 , TRIM(message) )
            DO i = its , MIN(ide-1,ite)
               count = 0
               sum = 0.
               DO ik = 1 , i_kicker/2
                  ii = i-ik
                  IF ( ii .GE. ids ) THEN
                     i_left = ii
                  ELSE
                     i_left = ( ii - ids ) + (ide-1)+1
                  END IF
                  ii = i+ik
                  IF ( ii .LE. ide-1 ) THEN
                     i_right = ii
                  ELSE
                     i_right = ( ii - (ide-1) ) + its-1
                  END IF
                  sum = sum + tr_in(i_left,j) + tr_in(i_right,j)
                  count = count + 1
               END DO
               tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * count + 1 )
            END DO
         END DO
   
         !  Set output values for whole patch.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr3d_in(i,k,j) = tr_out(i,j)
            END DO
         END DO
   
         !  Positive definite on scalars?
   
         IF ( pos_def ) THEN
            DO j = jts , MIN(jde-1,jte)
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,j) = MAX( tr3d_in(i,k,j) , 0. )
               END DO
            END DO
         END IF
   
         !  Remove values at j=1 and j=jde-1 locations, set them to the rows just next to them.
   
         IF ( swap_pole_with_next_j ) THEN
            IF  ( jts .EQ. jds ) THEN
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,jts) = tr3d_in(i,k,jts+1)
               END DO
            END IF
   
            IF  ( jte .EQ. jde ) THEN
               DO i = its , MIN(ide-1,ite)
                  tr3d_in(i,k,jte-1) = tr3d_in(i,k,jte-2)
               END DO
            END IF
         END IF

      END DO ! k-loop

   END SUBROUTINE filter_tracer

!---------------------------------------------------------------------

   SUBROUTINE filter_tracer_old ( tr3d_in , xlat , msftx , fft_filter_lat , &
                              ids , ide , jds , jde , kds , kde , &
                              ims , ime , jms , jme , kms , kme , &
                              its , ite , jts , jte , kts , kte )

      IMPLICIT NONE

      INTEGER , INTENT(IN) ::  ids , ide , jds , jde , kds , kde , &
                               ims , ime , jms , jme , kms , kme , &
                               its , ite , jts , jte , kts , kte

      REAL , INTENT(IN) :: fft_filter_lat
      REAL , DIMENSION(ims:ime,kms:kme,jms:jme) , INTENT(INOUT) :: tr3d_in
      REAL , DIMENSION(ims:ime,jms:jme) , INTENT(IN) :: xlat , msftx


      !  Local vars

      INTEGER :: i , j , j_lat_pos , j_lat_neg , k
      INTEGER :: i_kicker , ik , i1, i2, i3, i4
      REAL :: length_scale , sum
      REAL , DIMENSION(its:ite,jts:jte) :: tr_in, tr_out

      !  The filtering is a simple average on a latitude loop.  Possibly a LONG list of
      !  numbers.  We assume that ALL of the 2d arrays have been transposed so that
      !  each patch has the entire domain size of the i-dim local.

      IF ( ( its .NE. ids ) .OR. ( ite .NE. ide ) ) THEN
         CALL wrf_error_fatal ( 'filtering assumes all values on X' )
      END IF

      !  Starting at the south pole, we find where the
      !  grid distance is big enough, then go back a point.  Continuing to the
      !  north pole, we find the first small grid distance.  These are the
      !  computational latitude loops and the associated computational poles.

      j_lat_neg = 0
      j_lat_pos = jde + 1
      loop_neg : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .LT. 0.0 ) THEN
            IF ( ABS(xlat(its,j)) .LT. fft_filter_lat ) THEN
               j_lat_neg = j - 1
               EXIT loop_neg
            END IF
         END IF
      END DO loop_neg

      loop_pos : DO j = jts , MIN(jde-1,jte)
         IF ( xlat(its,j) .GT. 0.0 ) THEN
            IF ( xlat(its,j) .GE. fft_filter_lat ) THEN
               j_lat_pos = j
               EXIT loop_pos
            END IF
         END IF
      END DO loop_pos

      !  Set output values to initial input topo values for whole patch.

      DO k = kts, kte
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr_in(i,j) = tr3d_in(i,k,j)
               tr_out(i,j) = tr_in(i,j)
            END DO
         END DO

         !  Filter the topo at the negative lats.
   
         DO j = j_lat_neg , jts , -1
            i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
!           print *,'j = ' , j, ', kicker = ',i_kicker
            DO i = its , MIN(ide-1,ite)
               IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j) + tr_in(i-ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j)
                  END DO
                  i1 = i - i_kicker + ide -1
                  i2 = ide-1
                  i3 = ids
                  i4 = i-1
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i-ik,j)
                  END DO
                  i1 = i+1
                  i2 = ide-1
                  i3 = ids
                  i4 = ids + ( i_kicker+i ) - ide
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               END IF
            END DO
         END DO
   
         !  Filter the topo at the positive lats.
   
         DO j = j_lat_pos , MIN(jde-1,jte)
            i_kicker = MIN( MAX ( NINT(msftx(its,j)) , 1 ) , (ide - ids) / 2 )
!           print *,'j = ' , j, ', kicker = ',i_kicker
            DO i = its , MIN(ide-1,ite)
               IF      ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j) + tr_in(i-ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .LT. its ) .AND. ( i + i_kicker .LE. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i+ik,j)
                  END DO
                  i1 = i - i_kicker + ide -1
                  i2 = ide-1
                  i3 = ids
                  i4 = i-1
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               ELSE IF ( ( i - i_kicker .GE. its ) .AND. ( i + i_kicker .GT. ide-1 ) ) THEN
                  sum = 0.0
                  DO ik = 1 , i_kicker
                     sum = sum + tr_in(i-ik,j)
                  END DO
                  i1 = i+1
                  i2 = ide-1
                  i3 = ids
                  i4 = ids + ( i_kicker+i ) - ide
                  DO ik = i1 , i2
                     sum = sum + tr_in(ik,j)
                  END DO
                  DO ik = i3 , i4
                     sum = sum + tr_in(ik,j)
                  END DO
                  tr_out(i,j) = ( tr_in(i,j) + sum ) / REAL ( 2 * i_kicker + 1 )
               END IF
            END DO
         END DO
   
         !  Set output values to initial input topo values for whole patch.
   
         DO j = jts , MIN(jde-1,jte)
            DO i = its , MIN(ide-1,ite)
               tr3d_in(i,k,j) = tr_out(i,j)
            END DO
         END DO
      END DO ! k-loop

   END SUBROUTINE filter_tracer_old

!---------------------------------------------------------------------
END MODULE module_polarfft

