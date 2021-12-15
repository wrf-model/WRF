
SUBROUTINE med_nest_move ( parent, nest )
  ! Driver layer
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move
   USE module_driver_constants, ONLY : max_nests
   USE module_utility
   USE module_timing
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_state_description
!   USE module_io_domain
#if defined(DM_PARALLEL) && ! defined(STUBMPI)
   USE module_dm, ONLY : wrf_dm_move_nest,nest_task_offsets,mpi_comm_to_kid,mpi_comm_to_mom, which_kid
#endif
   IMPLICIT NONE
   TYPE(domain) , POINTER                     :: parent, nest, grid
   INTEGER dx, dy, origdy       ! number of parent domain points to move
#ifdef MOVE_NESTS
  ! Local 
   CHARACTER*256 mess
   INTEGER i, j, k, p, parent_grid_ratio
   INTEGER px, py       ! number and direction of nd points to move
   INTEGER                         :: ids , ide , jds , jde , kds , kde , &
                                      ims , ime , jms , jme , kms , kme , &
                                      ips , ipe , jps , jpe , kps , kpe
   INTEGER ierr, fid, comzilla, kid
   LOGICAL input_from_hires
   LOGICAL saved_restart_value
   TYPE (grid_config_rec_type)   :: config_flags
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
   LOGICAL, EXTERNAL :: should_not_move

   INTERFACE
     SUBROUTINE med_interp_domain ( parent , nest )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) , POINTER                 :: parent , nest
     END SUBROUTINE med_interp_domain
     SUBROUTINE start_domain ( grid , allowed_to_move )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) :: grid
        LOGICAL, INTENT(IN) :: allowed_to_move
     END SUBROUTINE start_domain
#if ( EM_CORE == 1 )
     SUBROUTINE shift_domain_em ( grid, disp_x, disp_y  &
!
# include "dummy_new_args.inc"
!
                           )
        USE module_domain, ONLY : domain
        USE module_state_description
        IMPLICIT NONE
        INTEGER disp_x, disp_y
        TYPE(domain) , POINTER                 :: grid
# include "dummy_new_decl.inc"
     END SUBROUTINE shift_domain_em
#endif
     LOGICAL FUNCTION time_for_move ( parent , nest , dx , dy )
        USE module_domain, ONLY : domain
        IMPLICIT NONE
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION time_for_move

     SUBROUTINE  input_terrain_rsmas ( grid ,                  &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       USE module_domain, ONLY : domain
       IMPLICIT NONE
       TYPE ( domain ) :: grid
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
     END SUBROUTINE input_terrain_rsmas
     SUBROUTINE med_nest_feedback ( parent , nest , config_flags )
       USE module_domain, ONLY : domain
       USE module_configure, ONLY : grid_config_rec_type
        IMPLICIT NONE
       TYPE (domain), POINTER ::  nest , parent
       TYPE (grid_config_rec_type) config_flags
     END SUBROUTINE med_nest_feedback
     SUBROUTINE  blend_terrain ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       IMPLICIT NONE
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_interpolated
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_input
     END SUBROUTINE blend_terrain
     SUBROUTINE  copy_3d_field ( ter_interpolated , ter_input , &
                           ids , ide , jds , jde , kds , kde , &
                           ims , ime , jms , jme , kms , kme , &
                           ips , ipe , jps , jpe , kps , kpe )
       IMPLICIT NONE
       INTEGER                           :: ids , ide , jds , jde , kds , kde , &
                                            ims , ime , jms , jme , kms , kme , &
                                            ips , ipe , jps , jpe , kps , kpe
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_interpolated
       REAL , DIMENSION(ims:ime,jms:jme) :: ter_input
     END SUBROUTINE copy_3d_field
   END INTERFACE

  ! set grid pointer for code in deref_kludge (if used)
   grid => nest

   IF ( should_not_move( nest%id ) ) THEN
      CALL wrf_message( 'Nest movement is disabled because of namelist settings' )
      RETURN
   ENDIF

! if the nest has stopped don't do all this
   IF ( WRFU_ClockIsStopTime(nest%domain_clock ,rc=ierr) ) RETURN

! mask should be defined in nest domain

  check_for_move: IF ( time_for_move ( parent , nest , dx, dy ) ) THEN

#if ( EM_CORE == 1 )
     IF ( (dx .gt. 1 .or. dx .lt. -1 ) .or.  &
          (dy .gt. 1 .or. dy .lt. -1 ) ) THEN
       WRITE(mess,*)' invalid move: dx, dy ', dx, dy
       CALL wrf_error_fatal( mess )
     ENDIF
#endif

     IF (  wrf_dm_on_monitor() ) THEN
       WRITE(mess,*)' moving ',grid%id,dx,dy
       CALL wrf_message(mess)
     ENDIF

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

     CALL wrf_dm_move_nest ( parent, nest%intermediate_grid, dx, dy )

     CALL adjust_domain_dims_for_move( nest%intermediate_grid , dx, dy )

     CALL get_ijk_from_grid (  grid ,                   &
                               ids, ide, jds, jde, kds, kde,    &
                               ims, ime, jms, jme, kms, kme,    &
                               ips, ipe, jps, jpe, kps, kpe    )

     grid => nest 

#if ( EM_CORE == 1 )
     CALL shift_domain_em( grid, dx, dy  &
!
# include "actual_new_args.inc"
!
                           )
#endif

     px = grid%parent_grid_ratio*dx
     py = grid%parent_grid_ratio*dy

     grid%i_parent_start = grid%i_parent_start + px / grid%parent_grid_ratio 
     grid%j_parent_start = grid%j_parent_start + py / grid%parent_grid_ratio

#if defined(DM_PARALLEL) && ! defined(STUBMPI)
     IF ( (parent%active_this_task .OR. grid%active_this_task) ) THEN
       IF ( parent%active_this_task ) THEN
         comzilla = mpi_comm_to_kid( which_kid( grid%id ) , parent%id )
       ELSE
         comzilla = mpi_comm_to_mom( grid%id )
       ENDIF
       CALL BYTE_BCAST_FROM_ROOT( grid%i_parent_start, IWORDSIZE, nest_task_offsets(nest%id), comzilla )  !
       CALL BYTE_BCAST_FROM_ROOT( grid%j_parent_start, IWORDSIZE, nest_task_offsets(nest%id), comzilla )  !
     ENDIF

     CALL nl_set_i_parent_start( grid%id, grid%i_parent_start )
     CALL nl_set_j_parent_start( grid%id, grid%j_parent_start )

     CALL push_communicators_for_domain(grid%id)
     IF ( wrf_dm_on_monitor() ) THEN
       write(mess,*)  &
         'Grid ',grid%id,' New SW corner (in parent x and y):',grid%i_parent_start, grid%j_parent_start
       CALL wrf_message(TRIM(mess))
     ENDIF
     CALL pop_communicators_for_domain(grid%id)
#endif

     CALL med_interp_domain( parent, nest )

#if ( EM_CORE == 1 )
     CALL nl_get_input_from_hires( nest%id , input_from_hires ) 
     IF ( input_from_hires ) THEN

       IF ( nest%active_this_task ) THEN
! store horizontally interpolated terrain in temp location
       CALL  copy_3d_field ( nest%ht_fine , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  copy_3d_field ( nest%mub_fine , nest%mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  copy_3d_field ( nest%phb_fine , nest%phb , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe )

       CALL  input_terrain_rsmas ( nest,                               &
                                   ids , ide , jds , jde , 1   , 1   , &
                                   ims , ime , jms , jme , 1   , 1   , &
                                   ips , ipe , jps , jpe , 1   , 1   )

       CALL  blend_terrain ( nest%ht_fine , nest%ht , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  blend_terrain ( nest%mub_fine , nest%mub , &
                             ids , ide , jds , jde , 1   , 1   , &
                             ims , ime , jms , jme , 1   , 1   , &
                             ips , ipe , jps , jpe , 1   , 1   )
       CALL  blend_terrain ( nest%phb_fine , nest%phb , &
                             ids , ide , jds , jde , kds , kde , &
                             ims , ime , jms , jme , kms , kme , &
                             ips , ipe , jps , jpe , kps , kpe )
       ENDIF
!
       CALL model_to_grid_config_rec ( parent%id , model_config_rec , config_flags )

       CALL med_nest_feedback ( parent , nest , config_flags )
       parent%imask_nostag = 1
       parent%imask_xstag = 1
       parent%imask_ystag = 1
       parent%imask_xystag = 1

! start_domain will key off "restart". Even if this is a restart run
! we don't want it to here. Save the value, set it to false, and restore afterwards
       saved_restart_value = config_flags%restart
       config_flags%restart = .FALSE.
       grid%restart = .FALSE.
       CALL nl_set_restart ( 1, .FALSE. )
       grid%press_adj = .FALSE.
       CALL start_domain ( parent , .FALSE. )
       config_flags%restart = saved_restart_value
       grid%restart = saved_restart_value
       CALL nl_set_restart ( 1,  saved_restart_value )

     ENDIF
#endif

!
! masks associated with nest will have been set by shift_domain_em above
     nest%moved = .true.
! start_domain will key off "restart". Even if this is a restart run
! we don't want it to here. Save the value, set it to false, and restore afterwards
     saved_restart_value = config_flags%restart
     config_flags%restart = .FALSE.
     CALL nl_set_restart ( 1, .FALSE. )
     grid%restart = .FALSE.
#if ( EM_CORE == 1 )
     nest%press_adj = .FALSE.
#endif

#if defined(DM_PARALLEL) && ! defined(STUBMPI)

     IF ( nest%active_this_task ) THEN
       CALL push_communicators_for_domain(nest%id)
       CALL start_domain ( nest , .FALSE. )
       CALL pop_communicators_for_domain
     ENDIF
#endif
     config_flags%restart = saved_restart_value
     grid%restart = saved_restart_value
     CALL nl_set_restart ( 1,  saved_restart_value )
     nest%moved = .false.
      
!
! copy time level 2 to time level 1 in new regions of multi-time level fields
! this should be registry generated.
!
#if ( EM_CORE == 1 )
     IF ( nest%active_this_task ) THEN
      do k = kms,kme
        where ( nest%imask_xstag  .EQ. 1 ) nest%u_1(:,k,:)   = nest%u_2(:,k,:)
        where ( nest%imask_ystag  .EQ. 1 ) nest%v_1(:,k,:)   = nest%v_2(:,k,:)
        where ( nest%imask_nostag .EQ. 1 ) nest%t_1(:,k,:)   = nest%t_2(:,k,:)
        where ( nest%imask_nostag .EQ. 1 ) nest%w_1(:,k,:)   = nest%w_2(:,k,:)
        where ( nest%imask_nostag .EQ. 1 ) nest%ph_1(:,k,:)  = nest%ph_2(:,k,:)
        where ( nest%imask_nostag .EQ. 1 ) nest%tke_1(:,k,:) = nest%tke_2(:,k,:)
      enddo
      where ( nest%imask_nostag .EQ. 1 ) nest%mu_1(:,:)  = nest%mu_2(:,:)
     ENDIF
#endif
!
   ENDIF check_for_move

#endif
END SUBROUTINE med_nest_move

LOGICAL FUNCTION time_for_move2 ( parent , grid , move_cd_x, move_cd_y )
  ! Driver layer
   USE module_domain, ONLY : domain, domain_clock_get, get_ijk_from_grid, adjust_domain_dims_for_move
!   USE module_configure
   USE module_driver_constants, ONLY : max_moves
   USE module_compute_geop
   USE module_dm, ONLY : wrf_dm_max_real, wrf_dm_move_nest
   USE module_utility
   USE module_streams, ONLY : compute_vortex_center_alarm
   IMPLICIT NONE
! Arguments
   TYPE(domain) , POINTER    :: parent, grid
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
#ifdef MOVE_NESTS
! Local
   INTEGER  num_moves, rc
   INTEGER  move_interval , move_id
   TYPE(WRFU_Time) :: ct, st
   TYPE(WRFU_TimeInterval) :: ti
   CHARACTER*256 mess, timestr
   INTEGER     :: ids, ide, jds, jde, kds, kde, &
                  ims, ime, jms, jme, kms, kme, &
                  ips, ipe, jps, jpe, kps, kpe
   INTEGER :: is, ie, js, je, ierr
   REAL    :: ipbar, pbar, jpbar, fact
   REAL    :: last_vc_i , last_vc_j

   REAL, ALLOCATABLE, DIMENSION(:,:) :: height_l, height
   REAL, ALLOCATABLE, DIMENSION(:,:) :: psfc, xlat, xlong, terrain
   REAL :: minh, maxh
   INTEGER :: mini, minj, maxi, maxj, i, j, pgr, irad
   REAL :: disp_x, disp_y, lag, radius, center_i, center_j, dx
   REAL :: dijsmooth, vmax, vmin, a, b
   REAL :: dc_i, dc_j   ! domain center
   REAL :: maxws, ws
   REAL :: pmin
   INTEGER imploc, jmploc 

   INTEGER :: fje, fjs, fie, fis, fimloc, fjmloc, imloc, jmloc
   INTEGER :: i_parent_start, j_parent_start
   INTEGER :: max_vortex_speed, vortex_interval  ! meters per second and seconds
   INTEGER :: track_level
   REAL    :: rsmooth = 100000.  ! in meters

   LOGICAL, EXTERNAL :: wrf_dm_on_monitor

character*256 message, message2

!#define MOVING_DIAGS
# ifdef VORTEX_CENTER


   CALL nl_get_parent_grid_ratio ( grid%id , pgr )
   CALL nl_get_i_parent_start    ( grid%id , i_parent_start )
   CALL nl_get_j_parent_start    ( grid%id , j_parent_start )
   CALL nl_get_track_level       ( grid%id , track_level )

!  WRITE(mess,*)'Vortex is tracked at ', track_level
!  CALL wrf_message(mess)

   CALL get_ijk_from_grid (  grid ,                        &
                             ids, ide, jds, jde, kds, kde, &
                             ims, ime, jms, jme, kms, kme, &
                             ips, ipe, jps, jpe, kps, kpe  )

! If the alarm is ringing, recompute the Vortex Center (VC); otherwise
! use the previous position of VC.  VC is not recomputed ever step to
! save on cost for global collection of height field and broadcast
! of new center.

#  ifdef MOVING_DIAGS
write(message,*)'Check to see if COMPUTE_VORTEX_CENTER_ALARM is ringing? '
call wrf_debug(1,message)
#  endif
   CALL nl_get_parent_grid_ratio ( grid%id , pgr )
   CALL nl_get_dx ( grid%id , dx )

   IF ( WRFU_AlarmIsRinging( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc ) ) THEN

#  ifdef MOVING_DIAGS
     write(message,*)'COMPUTE_VORTEX_CENTER_ALARM is ringing  '
     call wrf_debug(1,message)
#  endif
     CALL WRFU_AlarmRingerOff( grid%alarms( COMPUTE_VORTEX_CENTER_ALARM ), rc=rc )
     CALL domain_clock_get( grid, current_timestr=timestr )

     last_vc_i = grid%vc_i
     last_vc_j = grid%vc_j

     ALLOCATE ( height_l ( ims:ime , jms:jme ), STAT=ierr )
     IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating height_l in time_for_move2')
     IF ( wrf_dm_on_monitor() ) THEN
       ALLOCATE ( height   ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating height in time_for_move2')
       ALLOCATE ( psfc     ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating psfc in time_for_move2')
       ALLOCATE ( xlat     ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating xlat in time_for_move2')
       ALLOCATE ( xlong    ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating xlong in time_for_move2')
       ALLOCATE ( terrain  ( ids:ide , jds:jde ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating terrain in time_for_move2')
     ELSE
       ALLOCATE ( height   ( 1:1 , 1:1 ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating height in time_for_move2')
       ALLOCATE ( psfc     ( 1:1 , 1:1 ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating psfc in time_for_move2')
       ALLOCATE ( xlat     ( 1:1 , 1:1 ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating xlat in time_for_move2')
       ALLOCATE ( xlong    ( 1:1 , 1:1 ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating xlong in time_for_move2')
       ALLOCATE ( terrain  ( 1:1 , 1:1 ), STAT=ierr )
       IF ( ierr .ne. 0 ) CALL wrf_error_fatal ('allocating terrain in time_for_move2')
     ENDIF

#  if (EM_CORE == 1)
     CALL compute_500mb_height ( grid%ph_2 , grid%phb, grid%p, grid%pb, height_l , &
                                 track_level,                  &
                                 ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 ips, ipe, jps, jpe, kps, kpe  )
#  endif

     CALL wrf_patch_to_global_real ( height_l , height , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )
     CALL wrf_patch_to_global_real ( grid%psfc , psfc , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )
     CALL wrf_patch_to_global_real ( grid%xlat , xlat , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )
     CALL wrf_patch_to_global_real ( grid%xlong , xlong , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )
     CALL wrf_patch_to_global_real ( grid%ht , terrain , grid%domdesc, "z", "xy", &
                                     ids, ide-1 , jds , jde-1 , 1 , 1 , &
                                     ims, ime   , jms , jme   , 1 , 1 , &
                                     ips, ipe   , jps , jpe   , 1 , 1   )

! calculate max wind speed
     maxws = 0.
     do j = jps, jpe
       do i = ips, ipe
         ws = grid%u10(i,j) * grid%u10(i,j) + grid%v10(i,j) * grid%v10(i,j)
         if ( ws > maxws ) maxws = ws
       enddo
     enddo
     maxws = sqrt ( maxws )
     maxws = wrf_dm_max_real ( maxws )

     monitor_only : IF ( wrf_dm_on_monitor() ) THEN

!
! This vortex center finding code adapted from the Hurricane version of MM5,
! Courtesy:
!
!   Shuyi Chen et al., Rosenstiel School of Marine and Atmos. Sci., U. Miami.
!   Spring, 2005
!
! Get the first guess vortex center about which we do our search
! as mini and minh; minimum value is minh
!

       CALL nl_get_vortex_interval( grid%id , vortex_interval )
       CALL nl_get_max_vortex_speed( grid%id , max_vortex_speed )

       IF ( grid%vc_i < 0. .AND. grid%vc_j < 0. ) THEN
          ! first time through
          is = ids
          ie = ide-1
          js = jds
          je = jde-1
       ELSE
          ! limit the search to an area around the vortex
          ! that is limited by max_vortex_speed (default 40) meters per second from
          ! previous location over vortex_interval (default 15 mins)

          is = max( grid%vc_i - 60 * vortex_interval * max_vortex_speed / dx , 1.0 * ids )
          js = max( grid%vc_j - 60 * vortex_interval * max_vortex_speed / dx , 1.0 * jds )
          ie = min( grid%vc_i + 60 * vortex_interval * max_vortex_speed / dx , 1.0 * (ide-1) )
          je = min( grid%vc_j + 60 * vortex_interval * max_vortex_speed / dx , 1.0 * (jde-1) )

       ENDIF

#  ifdef MOVING_DIAGS
write(message,*)'search set around last position '
call wrf_debug(1,message)
write(message,*)'   is, ids-1,  ie,  ide-1 ', is, ids-1, ie, ide-1
call wrf_debug(1,message)
write(message,*)'   js, jds-1,  je,  jde-1 ', js, jds-1, je, jde-1
call wrf_debug(1,message)
#  endif

       imploc = -1
       jmploc = -1

       ! find minimum psfc
       pmin = 99999999.0     ! make this very large to be sure we find a minumum
       DO j = js, je
       DO i = is, ie
         ! adjust approximately to sea level pressure (same as below: ATCF)
         psfc(i,j)=psfc(i,j)+11.38*terrain(i,j)
         IF ( psfc(i,j) .LT. pmin ) THEN
           pmin = psfc(i,j)
           imploc = i
           jmploc = j
         ENDIF
       ENDDO
       ENDDO

       IF ( imploc .EQ. -1 .OR. jmploc .EQ. -1 ) THEN  ! if we fail to find a min there is something seriously wrong
         WRITE(mess,*)'i,j,is,ie,js,je,imploc,jmploc ',i,j,is,ie,js,je,imploc,jmploc
         CALL wrf_message(mess)
         CALL wrf_error_fatal('time_for_move2: Method failure searching for minimum psfc.')
       ENDIF

       imloc = -1
       jmloc = -1
       maxi = -1
       maxj = -1

       ! find local min, max
       vmin =  99999999.0
       vmax = -99999999.0
       DO j = js, je
       DO i = is, ie
         IF ( height(i,j) .LT. vmin ) THEN
           vmin = height(i,j)
           imloc = i
           jmloc = j
         ENDIF
         IF ( height(i,j) .GT. vmax ) THEN
           vmax = height(i,j)
           maxi = i
           maxj = j
         ENDIF
       ENDDO
       ENDDO

       IF ( imloc .EQ. -1 .OR. jmloc .EQ. -1 .OR. maxi .EQ. -1 .OR. maxj .EQ. -1 ) THEN
         WRITE(mess,*)'i,j,is,ie,js,je,imloc,jmloc,maxi,maxj ',i,j,is,ie,js,je,imloc,jmloc,maxi,maxj
         CALL wrf_message(mess)
         CALL wrf_error_fatal('time_for_move2: Method failure searching max/min of height.')
       ENDIF

       fimloc = imloc
       fjmloc = jmloc

       if ( grid%xi .EQ. -1. ) grid%xi = fimloc
       if ( grid%xj .EQ. -1. ) grid%xj = fjmloc

       dijsmooth = rsmooth / dx

       fjs = max(fjmloc-dijsmooth,1.0)
       fje = min(fjmloc+dijsmooth,jde-2.0)
       fis = max(fimloc-dijsmooth,1.0)
       fie = min(fimloc+dijsmooth,ide-2.0)
       js = fjs
       je = fje
       is = fis
       ie = fie

       vmin =  1000000.0
       vmax = -1000000.0
       DO j = js, je
       DO i = is, ie
         IF ( height(i,j) .GT. vmax ) THEN
           vmax = height(i,j)
         ENDIF
       ENDDO
       ENDDO

       pbar  = 0.0
       ipbar = 0.0
       jpbar = 0.0

       do j=js,je
          do i=is,ie
             fact = vmax - height(i,j)
             pbar  = pbar + fact
             ipbar = ipbar + fact*(i-is)
             jpbar = jpbar + fact*(j-js)
          enddo
       enddo

      IF ( pbar .NE. 0. ) THEN

!     Compute an adjusted, smoothed, vortex center location in cross
!     point index space.
!     Time average. A is coef for old information; B is new
!     If pbar is zero then just skip this, leave xi and xj alone,
!     result will be no movement.
         a = 0.0
         b = 1.0
         grid%xi =  (a * grid%xi + b * (is + ipbar / pbar))  / ( a + b )
         grid%xj =  (a * grid%xj + b * (js + jpbar / pbar))  / ( a + b )

         grid%vc_i = grid%xi + .5
         grid%vc_j = grid%xj + .5


      ENDIF

#  ifdef MOVING_DIAGS
write(message,*)'computed grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
call wrf_debug(1,message)
i = grid%vc_i ; j = grid%vc_j ; height( i,j ) = height(i,j) * 1.2   !mark the center
CALL domain_clock_get( grid, current_timestr=message2 )
WRITE ( message , FMT = '(A," on domain ",I3)' ) TRIM(message2), grid%id
#  endif

! 
        i = INT(grid%xi+.5)
        j = INT(grid%xj+.5)
        write(mess,'("ATCF"," ",A19," ",f8.2," ",f8.2," ",f6.1," ",f6.1)')                &
                                       timestr(1:19),                               &
                                       xlat(i,j),                                   &
                                       xlong(i,j),                                  &
                                       0.01*pmin,                                   &
!already computed above                0.01*pmin+0.1138*terrain(imploc,jmploc),     &
                                       maxws*1.94
        CALL wrf_message(TRIM(mess))
                            


     ENDIF monitor_only

     DEALLOCATE ( psfc )
     DEALLOCATE ( xlat )
     DEALLOCATE ( xlong )
     DEALLOCATE ( terrain )
     DEALLOCATE ( height )
     DEALLOCATE ( height_l )

     CALL wrf_dm_bcast_real( grid%vc_i , 1 )
     CALL wrf_dm_bcast_real( grid%vc_j , 1 )

     CALL wrf_dm_bcast_real( pmin , 1 )
     CALL wrf_dm_bcast_integer( imploc , 1 )
     CALL wrf_dm_bcast_integer( jmploc , 1 )

#  ifdef MOVING_DIAGS
write(message,*)'after bcast : grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
call wrf_debug(1,message)
#  endif


   ENDIF   ! COMPUTE_VORTEX_CENTER_ALARM ringing

#  ifdef MOVING_DIAGS
write(message,*)'After ENDIF : grid%vc_i, grid%vc_j ',grid%vc_i, grid%vc_j
call wrf_debug(1,message)
#  endif

   dc_i = (ide-ids+1)/2.    ! domain center
   dc_j = (jde-jds+1)/2.

   disp_x = grid%vc_i - dc_i * 1.0
   disp_y = grid%vc_j - dc_j * 1.0

#if 0
! This appears to be an old, redundant, and perhaps even misnamed parameter. 
! Remove it from the namelist and Registry and just hard code it to 
! the default of 6. JM 20050721
   CALL nl_get_vortex_search_radius( 1, irad )
#else
   irad = 6
#endif

   radius = irad

   if ( disp_x .GT. 0 ) disp_x = min( disp_x , radius )
   if ( disp_y .GT. 0 ) disp_y = min( disp_y , radius )

   if ( disp_x .LT. 0 ) disp_x = max( disp_x , -radius )
   if ( disp_y .LT. 0 ) disp_y = max( disp_y , -radius )

   move_cd_x = int ( disp_x  / pgr )
   move_cd_y = int ( disp_y  / pgr )

   IF ( move_cd_x .GT. 0 ) move_cd_x = min ( move_cd_x , 1 )
   IF ( move_cd_y .GT. 0 ) move_cd_y = min ( move_cd_y , 1 )
   IF ( move_cd_x .LT. 0 ) move_cd_x = max ( move_cd_x , -1 )
   IF ( move_cd_y .LT. 0 ) move_cd_y = max ( move_cd_y , -1 )

   CALL domain_clock_get( grid, current_timestr=timestr )

   IF ( wrf_dm_on_monitor() ) THEN
     WRITE(mess,*)timestr(1:19),' vortex center (in nest x and y): ',grid%vc_i, grid%vc_j
     CALL wrf_message(TRIM(mess))
     WRITE(mess,*)timestr(1:19),' grid   center (in nest x and y): ',     dc_i,      dc_j
     CALL wrf_message(TRIM(mess))
     WRITE(mess,*)timestr(1:19),' disp          : ',   disp_x,    disp_y
     CALL wrf_message(TRIM(mess))
     WRITE(mess,*)timestr(1:19),' move (rel cd) : ',move_cd_x, move_cd_y
     CALL wrf_message(TRIM(mess))
   ENDIF

   grid%vc_i = grid%vc_i - move_cd_x * pgr
   grid%vc_j = grid%vc_j - move_cd_y * pgr

#  ifdef MOVING_DIAGS
   IF ( wrf_dm_on_monitor() ) THEN
write(mess,*)' changing grid%vc_i,  move_cd_x * pgr ', grid%vc_i, move_cd_x * pgr, move_cd_x, pgr
call wrf_debug(1,mess)
   ENDIF
#  endif

   IF ( ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 ) ) THEN
     time_for_move2 = .TRUE.
   ELSE
     time_for_move2 = .FALSE.
   ENDIF

# else
! from namelist
   move_cd_x = 0
   move_cd_y = 0
   time_for_move2 = .FALSE.
   CALL domain_clock_get( grid, current_time=ct, start_time=st )
   CALL nl_get_num_moves( 1, num_moves )
   IF ( num_moves .GT. max_moves ) THEN
     WRITE(mess,*)'time_for_moves2: num_moves (',num_moves,') .GT. max_moves (',max_moves,')'
     CALL wrf_error_fatal( TRIM(mess) )
   ENDIF
   DO i = 1, num_moves
     CALL nl_get_move_id( i, move_id )
     IF ( move_id .EQ. grid%id ) THEN
       CALL nl_get_move_interval( i, move_interval )
       IF ( move_interval .LT. 999999999 ) THEN
         CALL WRFU_TimeIntervalSet ( ti, M=move_interval, rc=rc )
         IF ( ct .GE. st + ti ) THEN
           CALL nl_get_move_cd_x ( i, move_cd_x )
           CALL nl_get_move_cd_y ( i, move_cd_y )
           CALL nl_set_move_interval ( i, 999999999 )
           time_for_move2 = .TRUE.
           EXIT
         ENDIF
       ENDIF
     ENDIF
   ENDDO
# endif
   RETURN
#else
   time_for_move2 = .FALSE.
#endif
END FUNCTION time_for_move2

LOGICAL FUNCTION time_for_move ( parent , grid , move_cd_x, move_cd_y )
   USE module_domain, ONLY : domain, get_ijk_from_grid, adjust_domain_dims_for_move
!   USE module_configure
   USE module_dm, ONLY : wrf_dm_move_nest
USE module_timing
   USE module_utility
   IMPLICIT NONE
! arguments
   TYPE(domain) , POINTER    :: parent, grid, par, nst
   INTEGER, INTENT(OUT)      :: move_cd_x , move_cd_y
#ifdef MOVE_NESTS
! local
   INTEGER     :: corral_dist, kid
   INTEGER     :: dw, de, ds, dn, pgr
   INTEGER     :: would_move_x, would_move_y
   INTEGER     :: cids, cide, cjds, cjde, ckds, ckde, &
                  cims, cime, cjms, cjme, ckms, ckme, &
                  cips, cipe, cjps, cjpe, ckps, ckpe, &
                  nids, nide, njds, njde, nkds, nkde, &
                  nims, nime, njms, njme, nkms, nkme, &
                  nips, nipe, njps, njpe, nkps, nkpe
   REAL        :: xtime, time_to_move
! interface
   INTERFACE
     LOGICAL FUNCTION time_for_move2 ( parent , nest , dx , dy )
        USE module_domain, ONLY : domain
        TYPE(domain) , POINTER    :: parent , nest
        INTEGER, INTENT(OUT)      :: dx , dy
     END FUNCTION time_for_move2
   END INTERFACE
! executable
! 
! Simplifying assumption: domains in moving nest simulations have only 
! one parent and only one child.

   IF   ( grid%num_nests .GT. 1 ) THEN
     CALL wrf_error_fatal ( 'domains in moving nest simulations can have only 1 nest' )
   ENDIF
   kid = 1

#if ( EM_CORE == 1 )
!  Check if it is time to move the nest
      xtime = grid%xtime
      CALL nl_get_time_to_move ( grid%id , time_to_move )
      if ( xtime .lt. time_to_move ) then
         time_for_move = .FALSE.
         move_cd_x = 0
         move_cd_y = 0
!        write(0,*) 'it is not the time to move ', xtime, time_to_move
         return
      endif
#endif
! 
! find out if this is the innermost nest (will not have kids)
   IF   ( grid%num_nests .EQ. 0 ) THEN
     ! code that executes on innermost nest
     time_for_move = time_for_move2 ( parent , grid , move_cd_x, move_cd_y )

     ! Make sure the parent can move before allowing the nest to approach
     ! its boundary
     par => grid%parents(1)%ptr
     nst => grid

     would_move_x = move_cd_x 
     would_move_y = move_cd_y

     ! top of until loop
100  CONTINUE
       CALL nl_get_corral_dist ( nst%id , corral_dist )
       CALL get_ijk_from_grid (  nst ,                               &
                                 nids, nide, njds, njde, nkds, nkde, &
                                 nims, nime, njms, njme, nkms, nkme, &
                                 nips, nipe, njps, njpe, nkps, nkpe  )
       CALL get_ijk_from_grid (  par ,                               &
                                 cids, cide, cjds, cjde, ckds, ckde, &
                                 cims, cime, cjms, cjme, ckms, ckme, &
                                 cips, cipe, cjps, cjpe, ckps, ckpe  )
       CALL nl_get_parent_grid_ratio ( nst%id , pgr )
       ! perform measurements...
       !  from western boundary
       dw = nst%i_parent_start + would_move_x - cids
       !  from southern boundary
       ds = nst%j_parent_start + would_move_y - cjds
       !  from eastern boundary
       de = cide - ( nst%i_parent_start + (nide-nids+1)/pgr + would_move_x )
       !  from northern boundary
       dn = cjde - ( nst%j_parent_start + (njde-njds+1)/pgr + would_move_y )

       ! would this generate a move on the parent?
       would_move_x = 0
       would_move_y = 0
       if ( dw .LE. corral_dist ) would_move_x = would_move_x - 1
       if ( de .LE. corral_dist ) would_move_x = would_move_x + 1
       if ( ds .LE. corral_dist ) would_move_y = would_move_y - 1
       if ( dn .LE. corral_dist ) would_move_y = would_move_y + 1

     IF ( par%id .EQ. 1 ) THEN
         IF ( would_move_x .NE. 0 .AND. move_cd_x .NE. 0 ) THEN
           CALL wrf_message('MOAD can not move. Cancelling nest move in X')
           if ( grid%num_nests .eq. 0 ) grid%vc_i = grid%vc_i + move_cd_x * pgr  ! cancel effect of move
           move_cd_x = 0
         ENDIF
         IF ( would_move_y .NE. 0 .AND. move_cd_y .NE. 0 ) THEN
           CALL wrf_message('MOAD can not move. Cancelling nest move in Y')
           if ( grid%num_nests .eq. 0 ) grid%vc_j = grid%vc_j + move_cd_y * pgr  ! cancel effect of move
           move_cd_y = 0
         ENDIF
     ELSE
         nst => par
         par => nst%parents(1)%ptr
         GOTO 100
     ENDIF

! bottom of until loop
     time_for_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

   ELSE
     ! code that executes on parent to see if parent needs to move
     ! get closest number of cells we'll allow nest edge to approach parent bdy
     CALL nl_get_corral_dist ( grid%nests(kid)%ptr%id , corral_dist )
     ! get dims
     CALL get_ijk_from_grid (  grid%nests(kid)%ptr ,               &
                               nids, nide, njds, njde, nkds, nkde, &
                               nims, nime, njms, njme, nkms, nkme, &
                               nips, nipe, njps, njpe, nkps, nkpe  )
     CALL get_ijk_from_grid (  grid ,                              &
                               cids, cide, cjds, cjde, ckds, ckde, &
                               cims, cime, cjms, cjme, ckms, ckme, &
                               cips, cipe, cjps, cjpe, ckps, ckpe  )
     CALL nl_get_parent_grid_ratio ( grid%nests(kid)%ptr%id , pgr )
     ! perform measurements...
     !  from western boundary
     dw = grid%nests(kid)%ptr%i_parent_start - 1
     !  from southern boundary
     ds = grid%nests(kid)%ptr%j_parent_start - 1
     !  from eastern boundary
     de = cide - ( grid%nests(kid)%ptr%i_parent_start + (nide-nids+1)/pgr )
     !  from northern boundary
     dn = cjde - ( grid%nests(kid)%ptr%j_parent_start + (njde-njds+1)/pgr )

     ! move this domain (the parent containing the moving nest)
     ! in a direction that reestablishes the distance from 
     ! the boundary.
     move_cd_x = 0
     move_cd_y = 0
     if ( dw .LE. corral_dist ) move_cd_x = move_cd_x - 1
     if ( de .LE. corral_dist ) move_cd_x = move_cd_x + 1
     if ( ds .LE. corral_dist ) move_cd_y = move_cd_y - 1
     if ( dn .LE. corral_dist ) move_cd_y = move_cd_y + 1

     time_for_move = ( move_cd_x .NE. 0 ) .OR. ( move_cd_y .NE. 0 )

     IF ( time_for_move ) THEN
       IF ( grid%id .EQ. 1 ) THEN

         CALL wrf_message( 'DANGER: Nest has moved too close to boundary of outermost domain.' )
         time_for_move = .FALSE.

       ELSE
         ! need to adjust the intermediate domain of the nest in relation to this
         ! domain since we're moving

         CALL wrf_dm_move_nest ( grid , grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         CALL adjust_domain_dims_for_move( grid%nests(kid)%ptr%intermediate_grid , -move_cd_x*pgr, -move_cd_y*pgr )
         grid%nests(kid)%ptr%i_parent_start = grid%nests(kid)%ptr%i_parent_start - move_cd_x*pgr
         grid%nests(kid)%ptr%j_parent_start = grid%nests(kid)%ptr%j_parent_start - move_cd_y*pgr

         CALL nl_set_i_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%i_parent_start )
         CALL nl_set_j_parent_start( grid%nests(kid)%ptr%id, grid%nests(kid)%ptr%j_parent_start )

       ENDIF
     ENDIF 

   ENDIF

   RETURN
#else
   time_for_move = .FALSE.
#endif
END FUNCTION time_for_move

! Put any tests for non-moving options or conditions in here
LOGICAL FUNCTION should_not_move ( id )
  USE module_state_description
!  USE module_configure
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: id
 ! Local
  LOGICAL retval
  INTEGER cu_physics, ra_sw_physics, ra_lw_physics, sf_urban_physics, sf_surface_physics, obs_nudge_opt

  retval = .FALSE.
! check for GD ensemble cumulus, which can not move
  CALL nl_get_cu_physics( id , cu_physics )
  IF ( cu_physics .EQ. GDSCHEME ) THEN
    CALL wrf_message('Grell cumulus can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
! check for CAM radiation scheme , which can not move
  CALL nl_get_ra_sw_physics( id , ra_sw_physics )
  IF ( ra_sw_physics .EQ. CAMSWSCHEME ) THEN
    CALL wrf_message('CAM SW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
  CALL nl_get_ra_lw_physics( id , ra_lw_physics )
  IF ( ra_lw_physics .EQ. CAMLWSCHEME ) THEN
    CALL wrf_message('CAM LW radiation can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
! check for urban canopy Noah LSM, which can not move
  CALL nl_get_sf_urban_physics( id , sf_urban_physics )
  IF ( sf_urban_physics .EQ. 1 .OR. sf_urban_physics .EQ. 2 ) THEN
    CALL wrf_message('UCMs Noah LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
! check for PX lsm scheme, which can not move
  CALL nl_get_sf_surface_physics( id , sf_surface_physics )
  IF ( sf_surface_physics .EQ. PXLSMSCHEME ) THEN
    CALL wrf_message('PX LSM can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
#if ( EM_CORE == 1 )
! check for observation nudging, which can not move
  CALL nl_get_obs_nudge_opt( id , obs_nudge_opt )
  IF ( obs_nudge_opt .EQ. 1 ) THEN
    CALL wrf_message('Observation nudging can not be specified with moving nests. Movement disabled.')
    retval = .TRUE.
  ENDIF
#endif
  should_not_move = retval
END FUNCTION

SUBROUTINE reconcile_nest_positions_over_tasks ( grid )
   USE module_driver_constants, ONLY : max_nests, max_domains
   USE module_domain, ONLY : domain, find_grid_by_id
   USE module_utility
   USE module_configure, ONLY : grid_config_rec_type, model_config_rec, model_to_grid_config_rec
   USE module_state_description
#ifdef DM_PARALLEL
   USE module_dm, ONLY : wrf_dm_move_nest, nest_task_offsets,mpi_comm_to_kid,mpi_comm_to_mom, which_kid &
                        ,comm_start, nest_pes_x, nest_pes_y,local_communicator
   IMPLICIT NONE
   TYPE(domain) , POINTER                     :: grid, result_grid
!local
   INTEGER kid
   INTEGER itask
   INTEGER max_dom, id
   INTEGER buf(max_domains,2)

   CALL nl_get_max_dom( 1 , max_dom )
   IF ( grid%num_nests .GT. 1 ) THEN
    IF ( grid%active_this_task ) THEN
     DO kid = 1, max_nests
     ! which task is active?
       IF ( ASSOCIATED( grid%nests(kid)%ptr ) ) THEN
          ! check to see if the starting task for the nest is within the range of tasks
          ! of the parent's local communicator; that task should be the root of the bcast on this grid's (the parent's) communicator
          ! since it is active in both the parent and nest and should have valid parent_start information;
          ! otoh, if it is outside then the parent and nest are not sharing processors
          itask = comm_start( grid%nests(kid)%ptr%id ) - comm_start( grid%id )
          buf(:,1) = model_config_rec%i_parent_start
          buf(:,2) = model_config_rec%j_parent_start
          IF ( itask .GE. 0 .AND. itask .LT. nest_pes_x(grid%id)*nest_pes_y(grid%id) ) THEN
            CALL push_communicators_for_domain(grid%id)
            CALL BYTE_BCAST_FROM_ROOT( buf, 2*max_domains*IWORDSIZE, itask, local_communicator)
            CALL pop_communicators_for_domain
          ENDIF
          DO id = 1, max_dom
            CALL find_grid_by_id ( id, grid%nests(kid)%ptr, result_grid )
            IF ( ASSOCIATED(result_grid) .AND. .NOT. result_grid%active_this_task ) THEN
              model_config_rec%i_parent_start(id) = buf(id,1)
              model_config_rec%j_parent_start(id) = buf(id,2)
              result_grid%i_parent_start = model_config_rec%i_parent_start(id)
              result_grid%j_parent_start = model_config_rec%j_parent_start(id)
            ENDIF
          ENDDO
       END IF
     END DO
    ENDIF
   ENDIF
#endif
END SUBROUTINE reconcile_nest_positions_over_tasks

