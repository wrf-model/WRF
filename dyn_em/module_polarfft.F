MODULE module_polarfft

  USE module_model_constants
  USE module_wrf_error

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
                 ,positive_definite             &
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
   LOGICAL, INTENT(IN) :: positive_definite
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
# include "XPOSE_POLAR_FILTER_T_x2z.inc"
#else
     k_end = MIN(kde-1,kpe)
     IF ( piggyback_mu ) k_end = MIN(kde,kpe)
     CALL polar_filter_3d( grid%t_2, grid%clat, piggyback_mu,     &
                                fft_filter_lat, 0.,                &
                                ids, ide, jds, jde, kds, kde-1,     &
                                ims, ime, jms, jme, kms, kme,       &
                                ips, ipe, jps, jpe, kps, k_end )
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
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )
# include "XPOSE_POLAR_FILTER_MOIST_x2z.inc"
#else
     CALL polar_filter_3d( moist(ims,kms,jms,itrace), grid%clat, .false.,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1), &
                           positive_definite = positive_definite )
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
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )
# include "XPOSE_POLAR_FILTER_CHEM_x2z.inc"
#else
     CALL polar_filter_3d( chem(ims,kms,jms,itrace), grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1), &
                           positive_definite = positive_definite )
#endif
   ENDIF

! tracer
   IF ( flag_tracer .GE. PARAM_FIRST_SCALAR ) THEN
     itrace = flag_tracer
#if ( defined( DM_PARALLEL ) && ( ! defined( STUBMPI ) ) )
# include "XPOSE_POLAR_FILTER_TRACER_z2x.inc"
     CALL polar_filter_3d( grid%fourd_xxx, grid%clat_xxx, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           imsx, imex, jmsx, jmex, kmsx, kmex, &
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )
# include "XPOSE_POLAR_FILTER_TRACER_x2z.inc"
#else
     CALL polar_filter_3d( tracer(ims,kms,jms,itrace), grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1), &
                           positive_definite = positive_definite )
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
                           ipsx, ipex, jpsx, jpex, kpsx, MIN(kpex,kde-1), &
                           positive_definite = positive_definite )
# include "XPOSE_POLAR_FILTER_SCALAR_x2z.inc"
#else
     CALL polar_filter_3d( scalar(ims,kms,jms,itrace) , grid%clat, .false. ,     &
                           fft_filter_lat, 0.,                &
                           ids, ide, jds, jde, kds, kde,       &
                           ims, ime, jms, jme, kms, kme, &
                           ips, ipe, jps, jpe, kps, MIN(kpe,kde-1), &
                           positive_definite = positive_definite )
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
                            its, ite, jts, jte, kts, kte,    &
                            positive_definite               )

  IMPLICIT NONE

  INTEGER ,       INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                   ims, ime, jms, jme, kms, kme, &
                                   its, ite, jts, jte, kts, kte
  REAL   ,       INTENT(IN   ) :: fft_filter_lat

  REAL , DIMENSION( ims:ime , kms:kme, jms:jme ) , INTENT(INOUT) ::  f
  REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) ::  xlat
  REAL , INTENT(IN) ::  dvlat
  LOGICAL , INTENT(IN), OPTIONAL :: positive_definite
  LOGICAL , INTENT(IN) :: piggyback

  REAL , DIMENSION(1:ide-ids,1:kte-kts+1) :: sheet
  REAL , DIMENSION(1:kte-kts+1) :: sheet_total
  REAL :: lat, avg, rnboxw
  INTEGER :: ig, jg, i, j, j_end, nx, ny, nmax, kw
  INTEGER :: k, nboxw, nbox2, istart, iend, overlap
  INTEGER, DIMENSION(6) :: wavenumber = (/ 1, 3, 7, 10, 13, 16 /)

  ! Variables will stay in domain form since this routine is meaningless
  ! unless tile extent is the same as domain extent in E/W direction, i.e.,
  ! the processor has access to all grid points in E/W direction.
  ! There may be other ways of doing FFTs, but we haven't learned them yet...

  ! Check to make sure we have full access to all E/W points
  IF ((its /= ids) .OR. (ite /= ide)) THEN
     WRITE ( wrf_err_message , * ) 'module_polarfft: 3d: (its /= ids) or (ite /= ide)',its,ids,ite,ide
     CALL wrf_error_fatal ( TRIM( wrf_err_message ) )
  END IF


  nx = ide-ids ! "U" stagger variables will be repeated by periodic BCs
  ny = kte-kts+1 ! we can filter extra level for variables that are non-Z-staggered
  lat = 0.
  j_end = MIN(jte, jde-1)
  IF (dvlat /= 0. .and. j_end == jde-1) j_end = jde
  DO j = jts, j_end
     ! jg is the J index in the global (domain) span of the array.
     jg = j-jds+1

     ! determine whether or not to filter the data

     lat = xlat(ids,j)-dvlat
     IF (abs(lat) >= fft_filter_lat) THEN
        DO k=kts,kte
        DO i=ids,ide-1
           sheet(i-ids+1,k-kts+1) = f(i,k,j)
        END DO
        END DO

        CALL polar_filter_fft_2d_ncar(nx,ny,sheet,lat,fft_filter_lat,piggyback)

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

SUBROUTINE polar_filter_fft_2d_ncar(nx,ny,fin,lat,filter_latitude,piggyback)
  IMPLICIT NONE
  INTEGER , INTENT(IN) :: nx, ny
  REAL , DIMENSION(nx,ny), INTENT(INOUT) :: fin
  REAL , INTENT(IN) :: lat, filter_latitude
  LOGICAL, INTENT(IN) :: piggyback

  REAL :: pi, rcosref, freq, c, cf
  INTEGER :: i, j
  REAL, dimension(nx,ny) :: fp

  INTEGER :: lensave, ier, nh, n1
  INTEGER :: lot, jump, n, inc, lenr, lensav, lenwrk
  REAL, DIMENSION(nx+15) :: wsave
  REAL, DIMENSION(nx,ny) :: work
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

  call rfftmi(n,wsave,lensav,ier)
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmi ',ier
  END IF

!  do the forward transform

  call rfftmf( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmf ',ier
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

  DO j=1,ny
    DO i=1,nx
      fin(i,j) = fp(i,j)*fin(i,j)
    ENDDO
  ENDDO

!  do the backward transform

  call rfftmb( lot, jump, n, inc, fin, lenr, wsave, lensav, work, lenwrk, ier )
  IF(ier /= 0) THEN
    write(0,*) ' error in rfftmb ',ier
  END IF

END SUBROUTINE polar_filter_fft_2d_ncar

!------------------------------------------------------------------------------

END MODULE module_polarfft

