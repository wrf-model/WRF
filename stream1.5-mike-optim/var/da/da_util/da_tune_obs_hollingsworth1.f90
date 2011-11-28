program da_tune_obs_hollingsworth1

   !-----------------------------------------------------------
   ! Author: Syed RH Rizvi   org: UCAR/NCAR/MMM
   ! Purpose: Preparing necessary diagnostic output for 
   !          Observation error tuning  (Hollingsworh method)
   !     Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
   ! Update:
   !    01/03/2007   GPS refractivity update      Syed RH Rizvi
   !-----------------------------------------------------------

   use da_control, only : filename_len

   implicit none

   integer, parameter            :: y_unit = 50
   integer, parameter            :: unit1 = 35
   integer, parameter            :: unit2 = 36
   integer, parameter            :: unit3 = 37
   integer, parameter            :: unit4 = 38
   integer, parameter            :: unit5 = 39

   integer, parameter            :: obs_qc_pointer = 0   
   real, parameter               :: missing_r = -888888.0

   character(len=filename_len)   :: filename
   integer                       :: n, k, current_time

   type info_type
      character*5                :: id
      integer                    :: time
      real                       :: lat
      real                       :: lon
   end type info_type
   
   type field_type
      real                       :: yo
      real                       :: omb
      integer                    :: qc
      real                       :: err
      real                       :: oma
   end type field_type

   type surfc_type
      type (info_type)           :: info
      real                       :: pressure
      type (field_type)          :: u
      type (field_type)          :: v
      type (field_type)          :: t
      type (field_type)          :: p
      type (field_type)          :: q
   end type surfc_type
   
   type qscat_type
      type (info_type)           :: info
      real                       :: height   
      type (field_type)          :: u
      type (field_type)          :: v
   end type qscat_type

   type geoamv_type
      type (info_type)           :: info
      real                       :: pressure 
      type (field_type)          :: u
      type (field_type)          :: v
   end type geoamv_type

   type polaramv_type
      type (info_type)           :: info
      character*2                :: channel 
      character*1                :: landmask
      real                       :: pressure
      type (field_type)          :: u
      type (field_type)          :: v
   end type polaramv_type
   
   type gpspw_type
      type (info_type)           :: info
      type (field_type)          :: tpw
   end type gpspw_type

   type sound_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
   end type sound_type

   type airsr_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
   end type airsr_type
   
   
   type airep_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
   end type airep_type

   type pilot_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
   end type pilot_type

   type ssmir_type
      type (info_type)           :: info
      type (field_type)          :: speed
      type (field_type)          :: tpw
   end type ssmir_type
   
   type satem_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: thickness(:)
   end type satem_type

   type ssmt1_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: height(:)
      type (field_type), pointer :: t(:)
   end type ssmt1_type
   
   type ssmt2_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: height(:)
      type (field_type), pointer :: rh(:)
   end type ssmt2_type
   type bogus_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
      type (field_type)          :: slp 
   end type bogus_type

   type gpsref_type
      type (info_type)           :: info
      integer                    :: numlevs
      real, pointer              :: pressure(:)
      type (field_type), pointer :: ref(:)
   end type gpsref_type

   type iv_type
      integer                    :: num_synop, num_metar, num_ships, &
                                    num_polaramv, num_qscat, num_geoamv, num_gpspw, num_sound, &
                                    num_airep, num_pilot, num_ssmir, num_airsret, &
                                    num_satem, num_ssmt1, num_ssmt2, &
                                    num_sonde_sfc, num_buoy, num_profiler, num_bogus, num_gpsref

      type (surfc_type), pointer :: synop(:)
      type (surfc_type), pointer :: metar(:)
      type (surfc_type), pointer :: ships(:)
      type (surfc_type), pointer :: sonde_sfc(:)
      type (surfc_type), pointer :: buoy(:)
      type (polaramv_type), pointer :: polaramv(:)
      type (geoamv_type), pointer :: geoamv(:)
      type (qscat_type), pointer :: qscat(:)
      type (gpspw_type), pointer :: gpspw(:)
      type (sound_type), pointer :: sound(:)
      type (airsr_type), pointer :: airsr(:)
      type (airep_type), pointer :: airep(:)
      type (pilot_type), pointer :: pilot(:)
      type (pilot_type), pointer :: profiler(:)
      type (ssmir_type), pointer :: ssmir(:)
      type (satem_type), pointer :: satem(:)
      type (ssmt1_type), pointer :: ssmt1(:)
      type (ssmt2_type), pointer :: ssmt2(:)
      type (bogus_type), pointer :: bogus(:)
      type (gpsref_type), pointer :: gpsref(:)
   end type iv_type

   type (iv_type)       :: ob
   
!--------------------------------------------------------------------------
!  [1.0] Count total number of observations and allocate arrays:
!--------------------------------------------------------------------------

   filename = 'hollingsworth1.in'
   open( y_unit, file = filename, status = 'old' )

   call da_count_obs( y_unit, ob )

!--------------------------------------------------------------------------
!  [2.0] Read in observation data:
!--------------------------------------------------------------------------

   call da_read_y( y_unit, ob )

!--------------------------------------------------------------------------
!  [3.0] Perform basic statistics for each ob type:
!--------------------------------------------------------------------------

   if ( ob % num_synop > 0 ) then
      call da_calc_stats( 'synop u  ', ob % num_synop, ob % synop % u )
      call da_calc_stats( 'synop v  ', ob % num_synop, ob % synop % v )
      call da_calc_stats( 'synop t  ', ob % num_synop, ob % synop % t )
      call da_calc_stats( 'synop p  ', ob % num_synop, ob % synop % p )
      call da_calc_stats( 'synop q  ', ob % num_synop, ob % synop % q )
      write(6,*)
   end if

   if ( ob % num_buoy > 0 ) then
      call da_calc_stats( 'buoy u   ', ob % num_buoy, ob % buoy % u )
      call da_calc_stats( 'buoy v   ', ob % num_buoy, ob % buoy % v )
      call da_calc_stats( 'buoy t   ', ob % num_buoy, ob % buoy % t )
      call da_calc_stats( 'buoy p   ', ob % num_buoy, ob % buoy % p )
      call da_calc_stats( 'buoy q   ', ob % num_buoy, ob % buoy % q )
      write(6,*)
   end if

   if ( ob % num_sonde_sfc > 0 ) then
      call da_calc_stats( 'sonde_sfc u', ob % num_sonde_sfc, ob % sonde_sfc % u )
      call da_calc_stats( 'sonde_sfc v', ob % num_sonde_sfc, ob % sonde_sfc % v )
      call da_calc_stats( 'sonde_sfc t', ob % num_sonde_sfc, ob % sonde_sfc % t )
      call da_calc_stats( 'sonde_sfc p', ob % num_sonde_sfc, ob % sonde_sfc % p )
      call da_calc_stats( 'sonde_sfc q', ob % num_sonde_sfc, ob % sonde_sfc % q )
      write(6,*)
   end if
   
   if ( ob % num_metar > 0 ) then
      call da_calc_stats( 'metar u  ', ob % num_metar, ob % metar % u )
      call da_calc_stats( 'metar v  ', ob % num_metar, ob % metar % v )
      call da_calc_stats( 'metar t  ', ob % num_metar, ob % metar % t )
      call da_calc_stats( 'metar p  ', ob % num_metar, ob % metar % p )
      call da_calc_stats( 'metar q  ', ob % num_metar, ob % metar % q )
      write(6,*)
   end if

   if ( ob % num_ships > 0 ) then
      call da_calc_stats( 'ships u  ', ob % num_ships, ob % ships % u )
      call da_calc_stats( 'ships v  ', ob % num_ships, ob % ships % v )
      call da_calc_stats( 'ships t  ', ob % num_ships, ob % ships % t )
      call da_calc_stats( 'ships p  ', ob % num_ships, ob % ships % p )
      call da_calc_stats( 'ships q  ', ob % num_ships, ob % ships % q )
      write(6,*)
   end if
   
   if ( ob % num_polaramv > 0 ) then
      call da_calc_stats( 'polaramv u  ', ob % num_polaramv, ob % polaramv % u )
      call da_calc_stats( 'polaramv v  ', ob % num_polaramv, ob % polaramv % v )
      write(6,*)
   end if

   if ( ob % num_geoamv > 0 ) then
      call da_calc_stats( 'geoamv u  ', ob % num_geoamv, ob % geoamv % u )
      call da_calc_stats( 'geoamv v  ', ob % num_geoamv, ob % geoamv % v )
      write(6,*)
   end if

   if ( ob % num_qscat > 0 ) then
      call da_calc_stats( 'qscat u  ', ob % num_qscat, ob % qscat % u )
      call da_calc_stats( 'qscat v  ', ob % num_qscat, ob % qscat % v )
      write(6,*)
   end if

   if ( ob % num_gpspw > 0 ) then
      call da_calc_stats( 'gpspw tpw', ob % num_gpspw, ob % gpspw % tpw)
      write(6,*)
   end if

   if ( ob % num_ssmir > 0 ) then
      call da_calc_stats( 'ssmir spd', ob % num_ssmir, ob % ssmir % speed)
      call da_calc_stats( 'ssmir tpw', ob % num_ssmir, ob % ssmir % tpw)
      write(6,*)
   end if
   
   if ( ob % num_sound > 0 ) call da_calc_stats_sound( ob % num_sound, ob % sound )
   if ( ob % num_airsret> 0 ) call da_calc_stats_airsr( ob % num_airsret, ob % airsr )
   if ( ob % num_airep > 0 ) call da_calc_stats_airep( ob % num_airep, ob % airep ) 
   if ( ob % num_pilot > 0 ) call da_calc_stats_pilot( ob % num_pilot, ob % pilot )
   
   if ( ob % num_profiler > 0 ) call da_calc_stats_profiler( ob % num_profiler, ob % profiler )
   if ( ob % num_satem > 0 ) call da_calc_stats_satem( ob % num_satem, ob % satem )
   if ( ob % num_ssmt1 > 0 ) call da_calc_stats_ssmt1( ob % num_ssmt1, ob % ssmt1 )
   if ( ob % num_ssmt2 > 0 ) call da_calc_stats_ssmt2( ob % num_ssmt2, ob % ssmt2 )
   if ( ob % num_bogus > 0 ) call da_calc_stats_bogus( ob % num_bogus, ob % bogus )
   if ( ob % num_gpsref > 0 ) call da_calc_stats_gpsref( ob % num_gpsref, ob % gpsref )

!--------------------------------------------------------------------------
!  [4.0] Write data for post-processing:
!--------------------------------------------------------------------------
!  [4.1] Sonde O-B:
 if ( ob % num_sound > 0 ) then
   open( unit1, file = 'sound_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'sound_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'sound_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'sound_q_omb.dat', status = 'unknown' )
   current_time = 1
   do n = 1, ob % num_sound
      do k = 1, ob % sound(n) % numlevs
         call da_write_data( k, current_time, unit1, ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % u(k) )
         call da_write_data( k, current_time, unit2, ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % v(k) )
         call da_write_data( k, current_time, unit3, ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % t(k) )
         call da_write_data( k, current_time, unit4, ob % sound(n) % info, &
                             ob % sound(n) % pressure(k), ob % sound(n) % q(k) )

      end do
      current_time = ob % sound(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 )

 end if
!  [4.2] Synop O-B:
   if ( ob % num_synop  > 0 ) then

   open( unit1, file = 'synop_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'synop_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'synop_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'synop_p_omb.dat', status = 'unknown' )
   open( unit5, file = 'synop_q_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_synop
      call da_write_data( 1, current_time, unit1, ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % u )
      call da_write_data( 1, current_time, unit2, ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % v )
      call da_write_data( 1, current_time, unit3, ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % t )
      call da_write_data( 1, current_time, unit4, ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % p )
      call da_write_data( 1, current_time, unit5, ob % synop(n) % info, &
                          ob % synop(n) % pressure, ob % synop(n) % q )
      current_time = ob % synop(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )
 end if
!  [4.3] Metar O-B:
 if ( ob % num_metar  > 0 ) then

   open( unit1, file = 'metar_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'metar_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'metar_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'metar_p_omb.dat', status = 'unknown' )
   open( unit5, file = 'metar_q_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_metar
      call da_write_data( 1, current_time, unit1, ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % u )
      call da_write_data( 1, current_time, unit2, ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % v )
      call da_write_data( 1, current_time, unit3, ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % t )
      call da_write_data( 1, current_time, unit4, ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % p )
      call da_write_data( 1, current_time, unit5, ob % metar(n) % info, &
                          ob % metar(n) % pressure, ob % metar(n) % q )
      current_time = ob % metar(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )
 end if
!  [4.4] Polar AMV O-B:
 if ( ob % num_polaramv  > 0 ) then
   open( unit1, file = 'polaramv_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'polaramv_v_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_polaramv
      call da_write_data( 1, current_time, unit1, ob % polaramv(n) % info, &
                          ob % polaramv(n) % pressure, ob % polaramv(n) % u )
      call da_write_data( 1, current_time, unit2, ob % polaramv(n) % info, &
                          ob % polaramv(n) % pressure, ob % polaramv(n) % v )
      current_time = ob % polaramv(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )
 end if
!  [4.5] Geo AMV O-B:
 if ( ob % num_geoamv  > 0 ) then

   open( unit1, file = 'geoamv_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'geoamvv_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_geoamv
      call da_write_data( 1, current_time, unit1, ob % geoamv(n) % info, &
                          ob % geoamv(n) % pressure, ob % geoamv(n) % u )
      call da_write_data( 1, current_time, unit2, ob % geoamv(n) % info, &
                          ob % geoamv(n) % pressure, ob % geoamv(n) % v )
      current_time = ob % geoamv(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )

 end if
!  [4.6] Buoy  O-B:

 if ( ob % num_buoy > 0 ) then
   open( unit1, file = 'buoy_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'buoy_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'buoy_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'buoy_p_omb.dat', status = 'unknown' )
   open( unit5, file = 'buoy_q_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_buoy
      call da_write_data( 1, current_time, unit1, ob % buoy(n) % info, &
                          ob % buoy(n) % pressure, ob % buoy(n) % u )
      call da_write_data( 1, current_time, unit2, ob % buoy(n) % info, &
                          ob % buoy(n) % pressure, ob % buoy(n) % v )
      call da_write_data( 1, current_time, unit3, ob % buoy(n) % info, &
                          ob % buoy(n) % pressure, ob % buoy(n) % t )
      call da_write_data( 1, current_time, unit4, ob % buoy(n) % info, &
                          ob % buoy(n) % pressure, ob % buoy(n) % p )
      call da_write_data( 1, current_time, unit5, ob % buoy(n) % info, &
                          ob % buoy(n) % pressure, ob % buoy(n) % q )
      current_time = ob % buoy(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )
 end if

!  [4.7] sonde_sfc  O-B:

 if ( ob % num_sonde_sfc > 0 ) then
   open( unit1, file = 'sonde_sfc_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'sonde_sfc_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'sonde_sfc_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'sonde_sfc_p_omb.dat', status = 'unknown' )
   open( unit5, file = 'sonde_sfc_q_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_sonde_sfc
      call da_write_data( 1, current_time, unit1, ob % sonde_sfc(n) % info, &
                          ob % sonde_sfc(n) % pressure, ob % sonde_sfc(n) % u )
      call da_write_data( 1, current_time, unit2, ob % sonde_sfc(n) % info, &
                          ob % sonde_sfc(n) % pressure, ob % sonde_sfc(n) % v )
      call da_write_data( 1, current_time, unit3, ob % sonde_sfc(n) % info, &
                          ob % sonde_sfc(n) % pressure, ob % sonde_sfc(n) % t )
      call da_write_data( 1, current_time, unit4, ob % sonde_sfc(n) % info, &
                          ob % sonde_sfc(n) % pressure, ob % sonde_sfc(n) % p )
      call da_write_data( 1, current_time, unit5, ob % sonde_sfc(n) % info, &
                          ob % sonde_sfc(n) % pressure, ob % sonde_sfc(n) % q )
      current_time = ob % sonde_sfc(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )
 end if
!  [4.8] Profiler  O-B:
 if ( ob % num_profiler  > 0 ) then
   open( unit1, file = 'profiler_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'profiler_v_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_profiler
      do k = 1, ob % profiler(n) % numlevs
      call da_write_data( k, current_time, unit1, ob % profiler(n) % info, &
                          ob % profiler(n) % pressure(k), ob % profiler(n) % u(k) )
      call da_write_data( k, current_time, unit2, ob % profiler(n) % info, &
                          ob % profiler(n) % pressure(k), ob % profiler(n) % v(k) )
      end do
      current_time = ob % profiler(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )
 end if
!  [4.9] AIRS retrievals  O-B:
 if ( ob % num_airsret > 0 ) then
   open( unit1, file = 'airsr_t_omb.dat', status = 'unknown' )
   open( unit2, file = 'airsr_q_omb.dat', status = 'unknown' )
   current_time = 1
   do n = 1, ob % num_airsret
      do k = 1, ob % airsr(n) % numlevs
         call da_write_data( k, current_time, unit1, ob % airsr(n) % info, &
                             ob % airsr(n) % pressure(k), ob % airsr(n) % t(k) )
         call da_write_data( k, current_time, unit2, ob % airsr(n) % info, &
                             ob % airsr(n) % pressure(k), ob % airsr(n) % q(k) )

      end do
      current_time = ob % airsr(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )
 end if
!  [4.10] Pilot  O-B:
 if ( ob % num_pilot  > 0 ) then
   open( unit1, file = 'pilot_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'pilot_v_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_pilot
      do k = 1, ob % pilot(n) % numlevs
      call da_write_data( k, current_time, unit1, ob % pilot(n) % info, &
                          ob % pilot(n) % pressure(k), ob % pilot(n) % u(k) )
      call da_write_data( k, current_time, unit2, ob % pilot(n) % info, &
                          ob % pilot(n) % pressure(k), ob % pilot(n) % v(k) )
      end do
      current_time = ob % pilot(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 )
 end if
!--------------------------------------------------------------------------
!  [4.11] For Bogus O-B:
 if ( ob % num_bogus > 0 ) then
   open( unit1, file = 'bogus_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'bogus_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'bogus_t_omb.dat', status = 'unknown' )
   open( unit4, file = 'bogus_q_omb.dat', status = 'unknown' )
   open( unit5, file = 'bogus_slp_omb.dat', status = 'unknown' )
   current_time = 1
   do n = 1, ob % num_bogus
      do k = 1, ob % bogus(n) % numlevs
         call da_write_data( k, current_time, unit1, ob % bogus(n) % info, &
                             ob % bogus(n) % pressure(k), ob % bogus(n) % u(k) )
         call da_write_data( k, current_time, unit2, ob % bogus(n) % info, &
                             ob % bogus(n) % pressure(k), ob % bogus(n) % v(k) )
         call da_write_data( k, current_time, unit3, ob % bogus(n) % info, &
                             ob % bogus(n) % pressure(k), ob % bogus(n) % t(k) )
         call da_write_data( k, current_time, unit4, ob % bogus(n) % info, &
                             ob % bogus(n) % pressure(k), ob % bogus(n) % q(k) )

      end do
      call da_write_data( 1, current_time, unit5, ob % bogus(n) % info, &
                          missing_r, ob % bogus(n) % slp )
      current_time = ob % bogus(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit4,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit5,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 ); close( unit4 ); close( unit5 )

 end if
!--------------------------------------------------------------------------
!  [4.11] For Airep O-B:
 if ( ob % num_airep > 0 ) then
   open( unit1, file = 'airep_u_omb.dat', status = 'unknown' )
   open( unit2, file = 'airep_v_omb.dat', status = 'unknown' )
   open( unit3, file = 'airep_t_omb.dat', status = 'unknown' )
   current_time = 1
   do n = 1, ob % num_airep
      do k = 1, ob % airep(n) % numlevs
         call da_write_data( k, current_time, unit1, ob % airep(n) % info, &
                             ob % airep(n) % pressure(k), ob % airep(n) % u(k) )
         call da_write_data( k, current_time, unit2, ob % airep(n) % info, &
                             ob % airep(n) % pressure(k), ob % airep(n) % v(k) )
         call da_write_data( k, current_time, unit3, ob % airep(n) % info, &
                             ob % airep(n) % pressure(k), ob % airep(n) % t(k) )
      end do
      current_time = ob % airep(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit2,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0
   write(unit3,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 ); close( unit2 ); close( unit3 )

 end if
!  [4.12] gpsref    O-B:
 if ( ob % num_gpsref  > 0 ) then
   open( unit1, file = 'gpsref_ref_omb.dat', status = 'unknown' )

   current_time = 1
   do n = 1, ob % num_gpsref   
      do k = 1, ob % gpsref(n) % numlevs
      call da_write_data( k, current_time, unit1, ob % gpsref(n) % info, &
                          ob % gpsref(n) % pressure(k), ob % gpsref(n) % ref(k) )
      end do
      current_time = ob % gpsref(n) % info % time
   end do

   write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

   close( unit1 )
 end if

!  [4.13] Gpspw O-B:
   if ( ob % num_gpspw > 0 ) then
      open( unit1, file = 'gpspw_tpw_omb.dat', status = 'unknown' )

      current_time = 1
      do n = 1, ob % num_gpspw
         call da_write_data( 1, current_time, unit1, ob % gpspw(n) % info, &
                             missing_r, ob % gpspw(n) % tpw )
         current_time = ob % gpspw(n) % info % time
      end do

      write(unit1,'(a5,2f9.3,3f17.7,i8)')'*end*', 0., 0., 0., 0., 0., 0

      close( unit1 )

   endif
contains

subroutine da_write_data( lev, current_time, ounit, info, p, field )

   implicit none

   integer, intent(in)           :: lev
   integer, intent(in)           :: current_time
   integer, intent(in)           :: ounit
   type (info_type), intent(in)  :: info
   real, intent(in)              :: p
   type (field_type), intent(in) :: field

   if ( info % time == current_time ) then  ! New ob at same time:
      write(ounit,'(a5,2f9.3,3f17.7,i8)') info % id, info % lat, info % lon, &
                                          p, field % omb, field % err, field % qc
   else
      if ( lev == 1)write(ounit,'(a5,2f9.3,3f17.7,i8)')'*****', 0., 0., 0., 0., 0., 0
      write(ounit,'(a5,2f9.3,3f17.7,i8)') info % id, info % lat, info % lon, &
                                          p, field % omb, field % err, field % qc
   end if
   
end subroutine da_write_data

!--------------------------------------------------------------------------

subroutine da_count_obs( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (iv_type), intent(inout)     :: ob

   character*20         :: ob_name, dummy
   integer              :: num_obs, num_times, levels,k, kk

!  [1] Initialize ob numbers:

   ob % num_synop = 0
   ob % num_metar = 0
   ob % num_ships = 0
   ob % num_polaramv = 0
   ob % num_geoamv = 0
   ob % num_qscat = 0
   ob % num_gpspw = 0
   ob % num_sound = 0
   ob % num_airsret = 0
   ob % num_airep = 0
   ob % num_pilot = 0
   ob % num_ssmir = 0
   ob % num_satem = 0
   ob % num_ssmt1 = 0
   ob % num_ssmt2 = 0
   ob % num_sonde_sfc = 0
   ob % num_buoy = 0
   ob % num_profiler = 0
   ob % num_bogus = 0
   ob % num_gpsref = 0
  
   num_times = 0
      
!  [2] Loop through input file to count number of obs:

   do       ! loop over entire input file: (ends in *end*)
      do    ! loop over particular time (ends in *****)

         read(y_unit,'(a20,i8)')ob_name, num_obs
 
         if ( index( ob_name,'*****') > 0 .or. index( ob_name,'*end*') > 0 ) exit  

         if ( index( ob_name,'synop') > 0 ) then
            ob % num_synop = ob % num_synop + num_obs
         else if ( index( ob_name,'metar') > 0 ) then
            ob % num_metar = ob % num_metar + num_obs
         else if ( index( ob_name,'ships') > 0 ) then
            ob % num_ships = ob % num_ships + num_obs
         else if ( index( ob_name,'polaramv') > 0 ) then
            ob % num_polaramv = ob % num_polaramv + num_obs
         else if ( index( ob_name,'geoamv') > 0 ) then
            ob % num_geoamv = ob % num_geoamv + num_obs
         else if ( index( ob_name,'qscat') > 0 ) then
            ob % num_qscat = ob % num_qscat + num_obs
         else if ( index( ob_name,'gpspw') > 0 ) then
            ob % num_gpspw = ob % num_gpspw + num_obs
         else if ( index( ob_name,'sound') > 0 ) then
            ob % num_sound = ob % num_sound + num_obs
         else if ( index( ob_name,'airsr') > 0 ) then
            ob % num_airsret = ob % num_airsret + num_obs
         else if ( index( ob_name,'airep') > 0 ) then
             ob % num_airep = ob % num_airep + num_obs
         else if ( index( ob_name,'pilot') > 0 ) then
             ob % num_pilot = ob % num_pilot + num_obs
         else if ( index( ob_name,'ssmir') > 0 ) then
             ob % num_ssmir = ob % num_ssmir + num_obs
         else if ( index( ob_name,'satem') > 0 ) then
             ob % num_satem = ob % num_satem + num_obs
         else if ( index( ob_name,'ssmt1') > 0 ) then
             ob % num_ssmt1 = ob % num_ssmt1 + num_obs
         else if ( index( ob_name,'ssmt2') > 0 ) then
             ob % num_ssmt2 = ob % num_ssmt2 + num_obs
         else if ( index( ob_name,'sonde_sfc') > 0 ) then
             ob % num_sonde_sfc = ob % num_sonde_sfc + num_obs
         else if ( index( ob_name,'buoy') > 0 ) then
             ob % num_buoy = ob % num_buoy  + num_obs
         else if ( index( ob_name,'profiler') > 0 ) then
             ob % num_profiler = ob % num_profiler + num_obs
         else if ( index( ob_name,'bogus') > 0 ) then
             ob % num_bogus = ob % num_bogus + num_obs
         else if ( index( ob_name,'gpsref') > 0 ) then
             ob % num_gpsref = ob % num_gpsref + num_obs
         else
             print*,' unknown obs type: ',trim(ob_name),' found in diagnostics.in file ' 
         end if
           
          do kk = 1, num_obs
           if( index( ob_name,'bogus')  > 0  ) then
            read(y_unit,'(i8)') levels
            read(y_unit,'(a20)') dummy 
           end if
            read(y_unit,'(i8)') levels
            do k=1,levels
            read(y_unit,'(a20)') dummy 
            end do
          end do
      end do
      
      if ( index( ob_name,'*end*') > 0 ) then  
         exit
      else
         num_times = num_times + 1
      end if
   end do

   write(6,'(a,i8)')' Number of times read= ', num_times

!  [3] Allocate ob structures where obs exist:

   if ( ob % num_synop > 0 ) then
      allocate( ob % synop(1:ob % num_synop) )
      write(6,'(a,i8)')' Number of synop obs = ', ob % num_synop
   end if
   
   if ( ob % num_metar > 0 ) then
      allocate( ob % metar(1:ob % num_metar) )
      write(6,'(a,i8)')' Number of metar obs = ', ob % num_metar
   end if
   
   if ( ob % num_ships > 0 ) then
      allocate( ob % ships(1:ob % num_ships) )
      write(6,'(a,i8)')' Number of ships obs = ', ob % num_ships
   end if
   
   if ( ob % num_polaramv > 0 ) then
      allocate( ob % polaramv(1:ob % num_polaramv) )
      write(6,'(a,i8)')' Number of polaramv obs = ', ob % num_polaramv
   end if
   
   if ( ob % num_geoamv > 0 ) then
      allocate( ob % geoamv(1:ob % num_geoamv) )
      write(6,'(a,i8)')' Number of geoamv obs = ', ob % num_geoamv
   end if

   if ( ob % num_qscat > 0 ) then
      allocate( ob % qscat(1:ob % num_qscat) )
      write(6,'(a,i8)')' Number of qscat obs = ', ob % num_qscat
   end if

   if ( ob % num_gpspw > 0 ) then
      allocate( ob % gpspw(1:ob % num_gpspw) )
      write(6,'(a,i8)')' Number of gpspw obs = ', ob % num_gpspw
   end if
   
   if ( ob % num_sound > 0 ) then
      allocate( ob % sound(1:ob % num_sound) )
      write(6,'(a,i8)')' Number of sound obs = ', ob % num_sound
   end if
   
   if ( ob % num_airsret > 0 ) then
      allocate( ob % airsr(1:ob % num_airsret) )
      write(6,'(a,i8)')' Number of AIRS retrievals = ', ob % num_airsret
   end if
   
   if ( ob % num_airep > 0 ) then
      allocate( ob % airep(1:ob % num_airep) )
      write(6,'(a,i8)')' Number of airep obs = ', ob % num_airep
   end if
   
   if ( ob % num_pilot > 0 ) then
      allocate( ob % pilot(1:ob % num_pilot) )
      write(6,'(a,i8)')' Number of pilot obs = ', ob % num_pilot
   end if
   
   if ( ob % num_ssmir > 0 ) then
      allocate( ob % ssmir(1:ob % num_ssmir) )
      write(6,'(a,i8)')' Number of ssmir obs = ', ob % num_ssmir
   end if
   
   if ( ob % num_satem > 0 ) then
      allocate( ob % satem(1:ob % num_satem) )
      write(6,'(a,i8)')' Number of satem obs = ', ob % num_satem
   end if
   
   if ( ob % num_ssmt1 > 0 ) then
      allocate( ob % ssmt1(1:ob % num_ssmt1) )
      write(6,'(a,i8)')' Number of ssmt1 obs = ', ob % num_ssmt1
   end if
   
   if ( ob % num_ssmt2 > 0 ) then
      allocate( ob % ssmt2(1:ob % num_ssmt2) )
      write(6,'(a,i8)')' Number of ssmt2 obs = ', ob % num_ssmt2
   end if
 
   if ( ob % num_buoy > 0 ) then
      allocate( ob % buoy(1:ob % num_buoy) )
      write(6,'(a,i8)')' Number of buoy obs = ', ob % num_buoy
   end if
   
   if ( ob % num_sonde_sfc > 0 ) then
      allocate( ob % sonde_sfc(1:ob % num_sonde_sfc) )
      write(6,'(a,i8)')' Number of sonde_sfc obs = ', ob % num_sonde_sfc
   end if
   
   if ( ob % num_profiler > 0 ) then
      allocate( ob % profiler(1:ob % num_profiler) )
      write(6,'(a,i8)')' Number of profiler obs = ', ob % num_profiler
   end if
   if ( ob % num_gpsref > 0 ) then
      allocate( ob % gpsref(1:ob % num_gpsref) )
      write(6,'(a,i8)')' Number of gpsref obs = ', ob % num_gpsref
   end if
   
   
   if ( ob % num_bogus > 0 ) then
      allocate( ob % bogus(1:ob % num_bogus) )
      write(6,'(a,i8)')' Number of bogus obs = ', ob % num_bogus
   end if
   
  write(6,*)
 
end subroutine da_count_obs
subroutine da_read_y( y_unit, ob )

   implicit none

   integer, intent(in)               :: y_unit
   type (iv_type), intent(inout)     :: ob

   character*20         :: ob_name, dummy
   integer              :: n, ndum, k, kdum, num_obs, num_levs
   integer              :: num_obs_sonde_sfc, num_obs_buoy, num_obs_profiler, num_obs_bogus
   integer              :: num_obs_synop, num_obs_metar, num_obs_ships, num_obs_gpsref
   integer              :: num_obs_qscat,  num_obs_polaramv, num_obs_geoamv
   integer              :: num_obs_gpspw, num_obs_sound, num_obs_airsr, num_obs_airep, num_obs_pilot
   integer              :: num_obs_ssmir, num_obs_satem, num_obs_ssmt1, num_obs_ssmt2
   integer              :: synopt, metart, shipst, polaramvt, geoamvt, qscatt, gpspwt, soundt, airsrt
   integer              :: sonde_sfct, buoyt, profilert, bogust, gpsreft
   integer              :: airept, pilott, ssmirt, satemt, ssmt1t, ssmt2t
   real                 :: rdum

   rewind (y_unit)
   num_obs_buoy = 0; num_obs_sonde_sfc = 0; num_obs_profiler = 0 ; num_obs_bogus = 0
   num_obs_synop = 0; num_obs_metar = 0; num_obs_ships = 0 ; num_obs_gpsref = 0
   num_obs_polaramv = 0; num_obs_geoamv = 0; num_obs_qscat = 0
   num_obs_gpspw = 0; num_obs_sound = 0; num_obs_airsr = 0; num_obs_airep = 0; num_obs_pilot = 0
   num_obs_ssmir = 0; num_obs_satem = 0; num_obs_ssmt1 = 0; num_obs_ssmt2 = 0
   buoyt = 0; sonde_sfct = 0; profilert = 0; bogust = 0 ; gpsreft = 0
   synopt = 0; metart = 0; shipst = 0; polaramvt = 0; geoamvt = 0; qscatt = 0
   gpspwt = 0; soundt = 0; airsrt = 0; airept = 0; pilott = 0
   ssmirt = 0; satemt = 0; ssmt1t = 0; ssmt2t = 0
!
   do       ! loop over entire input file: (ends in *end*)
      do    ! loop over particular time (ends in *****)

         read(y_unit,'(a20,i8)')ob_name, num_obs
      if ( index( ob_name,'*****') > 0 .or. index( ob_name,'*end*') > 0 ) exit  

      if ( index( ob_name,'synop') > 0 ) then
         synopt = synopt + 1
         do n = num_obs_synop + 1, num_obs_synop + num_obs

            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % synop(n) % info % id, &        ! Station
                        ob % synop(n) % info % lat, &       ! Latitude
                        ob % synop(n) % info % lon, &       ! Longitude
                        ob % synop(n) % pressure, &         ! Obs height
                        ob % synop(n) % u, &                ! O, O-B, O-A
                        ob % synop(n) % v, &                ! O, O-B, O-A
                        ob % synop(n) % t, &                ! O, O-B, O-A
                        ob % synop(n) % p, &                ! O, O-B, O-A
                        ob % synop(n) % q                   ! O, O-B, O-A
            end do
            ob % synop(n) % info % time = synopt
         end do
         num_obs_synop = num_obs_synop + num_obs

      elseif ( index( ob_name,'metar') > 0 ) then
         metart = metart + 1
         do n = num_obs_metar + 1, num_obs_metar + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % metar(n) % info % id, &        ! Station
                        ob % metar(n) % info % lat, &       ! Latitude
                        ob % metar(n) % info % lon, &       ! Longitude
                        ob % metar(n) % pressure, &         ! Obs height
                        ob % metar(n) % u, &                ! O, O-B, O-A
                        ob % metar(n) % v, &                ! O, O-B, O-A
                        ob % metar(n) % t, &                ! O, O-B, O-A
                        ob % metar(n) % p, &                ! O, O-B, O-A
                        ob % metar(n) % q                   ! O, O-B, O-A
            end do
            ob % metar(n) % info % time = metart
         end do
         num_obs_metar = num_obs_metar + num_obs
      elseif ( index( ob_name,'ships') > 0 ) then
         shipst = shipst + 1
         do n = num_obs_ships + 1, num_obs_ships + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % ships(n) % info % id, &        ! Station
                        ob % ships(n) % info % lat, &       ! Latitude
                        ob % ships(n) % info % lon, &       ! Longitude
                        ob % ships(n) % pressure, &         ! Obs height
                        ob % ships(n) % u, &                ! O, O-B, O-A
                        ob % ships(n) % v, &                ! O, O-B, O-A
                        ob % ships(n) % t, &                ! O, O-B, O-A
                        ob % ships(n) % p, &                ! O, O-B, O-A
                        ob % ships(n) % q                   ! O, O-B, O-A
            end do
            ob % ships(n) % info % time = shipst
         end do
         num_obs_ships = num_obs_ships + num_obs

      elseif ( index( ob_name,'polaramv') > 0 ) then
         polaramvt = polaramvt + 1
         do n = num_obs_polaramv + 1, num_obs_polaramv + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % polaramv(n) % info % id, &        ! Station
                        ob % polaramv(n) % info % lat, &       ! Latitude
                        ob % polaramv(n) % info % lon, &       ! Longitude
                        ob % polaramv(n) % pressure, &         ! Obs pressure
                        ob % polaramv(n) % u, &                ! O, O-B, O-A
                        ob % polaramv(n) % v
            ob % polaramv(n) % info % time = polaramvt
            ob % polaramv(n) % channel = ob % polaramv(n) % info % id(3:4)
            ob % polaramv(n) % landmask = ob % polaramv(n) % info % id(5:5)
            end do
         end do
         num_obs_polaramv = num_obs_polaramv + num_obs

      elseif ( index( ob_name,'geoamv') > 0 ) then
         geoamvt = geoamvt + 1
         do n = num_obs_geoamv + 1, num_obs_geoamv + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % geoamv(n) % info % id,  &       ! Station
                        ob % geoamv(n) % info % lat, &       ! Latitude
                        ob % geoamv(n) % info % lon, &       ! Longitude
                        ob % geoamv(n) % pressure,   &       ! Obs pressure
                        ob % geoamv(n) % u, &                ! O, O-B, O-A
                        ob % geoamv(n) % v
            end do
            ob % geoamv(n) % info % time = geoamvt
         end do
         num_obs_geoamv = num_obs_geoamv + num_obs
      elseif ( index( ob_name,'qscat') > 0 ) then
         qscatt = qscatt + 1
         do n = num_obs_qscat + 1, num_obs_qscat + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % qscat(n) % info % id, &        ! Station
                        ob % qscat(n) % info % lat, &       ! Latitude
                        ob % qscat(n) % info % lon, &       ! Longitude
                        ob % qscat(n) % height,    &        ! Obs height/pressure
                        ob % qscat(n) % u, &                ! O, O-B, O-A
                        ob % qscat(n) % v
            ob % qscat(n) % info % time = qscatt
            end do
         end do
         num_obs_qscat = num_obs_qscat + num_obs

      elseif ( index( ob_name,'gpspw') > 0 ) then
         gpspwt = gpspwt + 1
         do n = num_obs_gpspw + 1, num_obs_gpspw + num_obs
            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % gpspw(n) % info % id, &        ! Station
                        ob % gpspw(n) % info % lat, &       ! Latitude
                        ob % gpspw(n) % info % lon, &       ! Longitude
                        rdum, &                             ! Obs height
                        ob % gpspw(n) % tpw
            ob % gpspw(n) % info % time = gpspwt
            end do
         end do
         num_obs_gpspw = num_obs_gpspw + num_obs

      elseif ( index( ob_name,'sound') > 0 ) then
         soundt = soundt + 1
         do n = num_obs_sound + 1, num_obs_sound + num_obs
            read(y_unit,'(i8)')num_levs
            ob % sound(n) % numlevs = num_levs
            allocate( ob % sound(n) % pressure(1:num_levs) )
            allocate( ob % sound(n) % u(1:num_levs) )
            allocate( ob % sound(n) % v(1:num_levs) )
            allocate( ob % sound(n) % t(1:num_levs) )
            allocate( ob % sound(n) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % sound(n) % info % id, &     ! Station
                        ob % sound(n) % info % lat, &       ! Latitude
                        ob % sound(n) % info % lon, &       ! Longitude
                        ob % sound(n) % pressure(k), &      ! Obs height
                        ob % sound(n) % u(k), &             ! O, O-B, O-A
                        ob % sound(n) % v(k), &             ! O, O-B, O-A
                        ob % sound(n) % t(k), &             ! O, O-B, O-A
                        ob % sound(n) % q(k)                ! O, O-B, O-A
            end do
            ob % sound(n) % info % time = soundt
         end do
         num_obs_sound = num_obs_sound + num_obs

      elseif ( index( ob_name,'airsr') > 0 ) then
         airsrt = airsrt + 1
         do n = num_obs_airsr + 1, num_obs_airsr + num_obs
            read(y_unit,'(i8)')num_levs
            ob % airsr(n) % numlevs = num_levs
            allocate( ob % airsr(n) % pressure(1:num_levs) )
            allocate( ob % airsr(n) % t(1:num_levs) )
            allocate( ob % airsr(n) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % airsr(n) % info % id, &     ! Station
                        ob % airsr(n) % info % lat, &       ! Latitude
                        ob % airsr(n) % info % lon, &       ! Longitude
                        ob % airsr(n) % pressure(k), &      ! Obs height
                        ob % airsr(n) % t(k), &             ! O, O-B, O-A
                        ob % airsr(n) % q(k)                ! O, O-B, O-A
            end do
            ob % airsr(n) % info % time = airsrt
         end do
         num_obs_airsr = num_obs_airsr + num_obs

      elseif ( index( ob_name,'airep') > 0 ) then
         airept = airept + 1
         do n = num_obs_airep + 1, num_obs_airep + num_obs
            read(y_unit,'(i8)')num_levs
            ob % airep(n) % numlevs = num_levs
            allocate( ob % airep(n) % pressure(1:num_levs) )
            allocate( ob % airep(n) % u(1:num_levs) )
            allocate( ob % airep(n) % v(1:num_levs) )
            allocate( ob % airep(n) % t(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % airep(n) % info % id, &     ! Station
                        ob % airep(n) % info % lat, &       ! Latitude
                        ob % airep(n) % info % lon, &       ! Longitude
                        ob % airep(n) % pressure(k), &      ! Obs height
                        ob % airep(n) % u(k), &             ! O, O-B, O-A
                        ob % airep(n) % v(k), &             ! O, O-B, O-A
                        ob % airep(n) % t(k)                ! O, O-B, O-A
            end do
            ob % airep(n) % info % time = airept
         end do
         num_obs_airep = num_obs_airep + num_obs

      elseif ( index( ob_name,'pilot') > 0 ) then
         pilott = pilott + 1
         do n = num_obs_pilot + 1, num_obs_pilot + num_obs
            read(y_unit,'(i8)')num_levs
            ob % pilot(n) % numlevs = num_levs
            allocate( ob % pilot(n) % pressure(1:num_levs) )
            allocate( ob % pilot(n) % u(1:num_levs) )
            allocate( ob % pilot(n) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % pilot(n) % info % id, &     ! Station
                        ob % pilot(n) % info % lat, &       ! Latitude
                        ob % pilot(n) % info % lon, &       ! Longitude
                        ob % pilot(n) % pressure(k), &      ! Obs height
                        ob % pilot(n) % u(k), &             ! O, O-B, O-A
                        ob % pilot(n) % v(k)
            end do
            ob % pilot(n) % info % time = pilott
         end do
         num_obs_pilot = num_obs_pilot + num_obs

      elseif ( index( ob_name,'ssmir') > 0 ) then
         ssmirt = ssmirt + 1
         do n = num_obs_ssmir + 1, num_obs_ssmir + num_obs
            read(y_unit,'(i8)')num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % ssmir(n) % info % id, &     ! Station
                        ob % ssmir(n) % info % lat, &       ! Latitude
                        ob % ssmir(n) % info % lon, &       ! Longitude
                        rdum, &                             ! Obs height
                        ob % ssmir(n) % speed, &            ! O, O-B, O-A
                        ob % ssmir(n) % tpw
            end do
            ob % ssmir(n) % info % time = ssmirt
         end do
         num_obs_ssmir = num_obs_ssmir + num_obs

      elseif ( index( ob_name,'satem') > 0 ) then
         satemt = satemt + 1
         do n = num_obs_satem + 1, num_obs_satem + num_obs
            read(y_unit,'(i8)')num_levs
            ob % satem(n) % numlevs = num_levs
            allocate( ob % satem(n) % pressure(1:num_levs) )
            allocate( ob % satem(n) % thickness(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % satem(n) % info % id, &     ! Station
                        ob % satem(n) % info % lat, &       ! Latitude
                        ob % satem(n) % info % lon, &       ! Longitude
                        ob % satem(n) % pressure(k), &      ! Obs height
                        ob % satem(n) % thickness(k)        ! O, O-B, O-A
            end do
             ob % satem(n) % info % time = satemt
         end do
         num_obs_satem = num_obs_satem + num_obs

      elseif ( index( ob_name,'ssmt1') > 0 ) then
         ssmt1t = ssmt1t + 1
         do n = num_obs_ssmt1 + 1, num_obs_ssmt1 + num_obs
            read(y_unit,'(i8)')num_levs
            ob % ssmt1(n) % numlevs = num_levs
            allocate( ob % ssmt1(n) % height(1:num_levs) )
            allocate( ob % ssmt1(n) % t(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % ssmt1(n) % info % id, &     ! Station
                        ob % ssmt1(n) % info % lat, &       ! Latitude
                        ob % ssmt1(n) % info % lon, &       ! Longitude
                        ob % ssmt1(n) % height(k), &        ! Obs height
                        ob % ssmt1(n) % t(k)                ! O, O-B, O-A
            end do
            ob % ssmt1(n) % info % time = ssmt1t
         end do
         num_obs_ssmt1 = num_obs_ssmt1 + num_obs

      elseif ( index( ob_name,'ssmt2') > 0 ) then
         ssmt2t = ssmt2t + 1
         do n = num_obs_ssmt2 + 1, num_obs_ssmt2 + num_obs
            read(y_unit,'(i8)')num_levs
            ob % ssmt2(n) % numlevs = num_levs
            allocate( ob % ssmt2(n) % height(1:num_levs) )
            allocate( ob % ssmt2(n) % rh(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % ssmt2(n) % info % id, &     ! Station
                        ob % ssmt2(n) % info % lat, &       ! Latitude
                        ob % ssmt2(n) % info % lon, &       ! Longitude
                        ob % ssmt2(n) % height(k), &        ! Obs height
                        ob % ssmt2(n) % rh(k)               ! O, O-B, O-A
            end do
            ob % ssmt2(n) % info % time = ssmt2t
         end do
         num_obs_ssmt2 = num_obs_ssmt2 + num_obs

     elseif ( index( ob_name,'sonde_sfc') > 0 ) then
         sonde_sfct = sonde_sfct + 1
         do n = num_obs_sonde_sfc + 1, num_obs_sonde_sfc + num_obs
             read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % sonde_sfc(n) % info % id, &        ! Station
                        ob % sonde_sfc(n) % info % lat, &       ! Latitude
                        ob % sonde_sfc(n) % info % lon, &       ! Longitude
                        ob % sonde_sfc(n) % pressure, &         ! Obs height
                        ob % sonde_sfc(n) % u, &                ! O, O-B, O-A
                        ob % sonde_sfc(n) % v, &                ! O, O-B, O-A
                        ob % sonde_sfc(n) % t, &                ! O, O-B, O-A
                        ob % sonde_sfc(n) % p, &                ! O, O-B, O-A
                        ob % sonde_sfc(n) % q                   ! O, O-B, O-A
            end do
            ob % sonde_sfc(n) % info % time = sonde_sfct
         end do
         num_obs_sonde_sfc = num_obs_sonde_sfc + num_obs

      elseif ( index( ob_name,'buoy') > 0 ) then
         buoyt = buoyt + 1
         do n = num_obs_buoy + 1, num_obs_buoy + num_obs

            read(y_unit,'(i8)') num_levs
            do k = 1, num_levs
            read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
            ndum, kdum, ob % buoy(n) % info % id, &        ! Station
                        ob % buoy(n) % info % lat, &       ! Latitude
                        ob % buoy(n) % info % lon, &       ! Longitude
                        ob % buoy(n) % pressure, &         ! Obs height
                        ob % buoy(n) % u, &                ! O, O-B, O-A
                        ob % buoy(n) % v, &                ! O, O-B, O-A
                        ob % buoy(n) % t, &                ! O, O-B, O-A
                        ob % buoy(n) % p, &                ! O, O-B, O-A
                        ob % buoy(n) % q                   ! O, O-B, O-A
            end do
            ob % buoy(n) % info % time = buoyt
         end do
         num_obs_buoy = num_obs_buoy + num_obs
      elseif ( index( ob_name,'profiler') > 0 ) then
         profilert = profilert + 1
         do n = num_obs_profiler + 1, num_obs_profiler + num_obs
            read(y_unit,'(i8)')num_levs
            ob % profiler(n) % numlevs = num_levs
            allocate( ob % profiler(n) % pressure(1:num_levs) )
            allocate( ob % profiler(n) % u(1:num_levs) )
            allocate( ob % profiler(n) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % profiler(n) % info % id, &     ! Station
                        ob % profiler(n) % info % lat,   &     ! Latitude
                        ob % profiler(n) % info % lon,   &     ! Longitude
                        ob % profiler(n) % pressure(k),  &     ! Obs height
                        ob % profiler(n) % u(k), &             ! O, O-B, O-A
                        ob % profiler(n) % v(k)
            end do
            ob % profiler(n) % info % time = profilert
         end do
         num_obs_profiler = num_obs_profiler + num_obs

      elseif ( index( ob_name,'bogus') > 0 ) then
         bogust = bogust + 1
         do n = num_obs_bogus + 1, num_obs_bogus + num_obs
            read(y_unit,'(i8)')num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % bogus(n) % info % id, &     ! Station
                        ob % bogus(n) % info % lat, &       ! Latitude
                        ob % bogus(n) % info % lon, &       ! Longitude
                        dummy,                      &
                        ob % bogus(n) % slp                 ! O, O-B, O-A


            read(y_unit,'(i8)')num_levs
            ob % bogus(n) % numlevs = num_levs
            allocate( ob % bogus(n) % pressure(1:num_levs) )
            allocate( ob % bogus(n) % u(1:num_levs) )
            allocate( ob % bogus(n) % v(1:num_levs) )
            allocate( ob % bogus(n) % t(1:num_levs) )
            allocate( ob % bogus(n) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % bogus(n) % info % id, &     ! Station
                        ob % bogus(n) % info % lat, &       ! Latitude
                        ob % bogus(n) % info % lon, &       ! Longitude
                        ob % bogus(n) % pressure(k), &      ! Obs height
                        ob % bogus(n) % u(k), &             ! O, O-B, O-A
                        ob % bogus(n) % v(k), &             ! O, O-B, O-A
                        ob % bogus(n) % t(k), &             ! O, O-B, O-A
                        ob % bogus(n) % q(k)                ! O, O-B, O-A
            end do
            ob % bogus(n) % info % time = bogust
          end do
         num_obs_bogus = num_obs_bogus + num_obs

      elseif ( index( ob_name,'ssmiT') > 0 ) then
         do n = 1, num_obs
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(a)')dummy
         end do
      elseif ( index( ob_name,'gpsref') > 0 ) then
         gpsreft = gpsreft + 1
         do n = num_obs_gpsref + 1, num_obs_gpsref + num_obs
            read(y_unit,'(i8)')num_levs
            ob % gpsref(n) % numlevs = num_levs
            allocate( ob % gpsref(n) % pressure(1:num_levs) )
            allocate( ob % gpsref(n) % ref(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))') &
               ndum, kdum, ob % gpsref(n) % info % id, &     ! Station
                        ob % gpsref(n) % info % lat,   &     ! Latitude
                        ob % gpsref(n) % info % lon,   &     ! Longitude
                        ob % gpsref(n) % pressure(k),  &     ! Obs height
                        ob % gpsref(n) % ref(k)              ! O, O-B, O-A
            end do
            ob % gpsref(n) % info % time = gpsreft
         end do
         num_obs_gpsref = num_obs_gpsref + num_obs

      else
      print*,' Got unknown obs_tye : ',trim(ob_name)
      stop
      end if
    end do     ! loop over particular time (ends in *****)
      if ( index( ob_name,'*end*') > 0 ) exit  
  end do      ! loop over entire input file: (ends in *end*)

end subroutine da_read_y

subroutine da_calc_stats( ob_name, num_obs, field )


   implicit none

   character*9, intent(in)       :: ob_name ! Ob name   
   integer, intent(in)           :: num_obs ! Number of observations
   type (field_type), intent(in) :: field(:)! Obs data.

   integer                       :: n, count
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then
      count = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0

      do n = 1, num_obs   
         call da_increment_stats( field(n), count, &
                                  sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                  mean_omb, mean_oma, stdv_omb, stdv_oma )
      end do

      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')ob_name, count, '/', num_obs, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

   end if

end subroutine da_calc_stats

!--------------------------------------------------------------------------

subroutine da_calc_stats_sound( num_obs, sound )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (sound_type), intent(in) :: sound(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then
 
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'sound u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'sound v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'sound t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, sound(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( sound(n) % q(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'sound q  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_sound

!--------------------------------------------------------------------------

subroutine da_calc_stats_airsr( num_obs, airsr )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (airsr_type), intent(in) :: airsr(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma
!--------------------------------------------------------------------------

   if ( num_obs > 0 ) then
 
 
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airsr(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( airsr(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'airsr t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airsr(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( airsr(n) % q(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'airsr q  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_airsr

subroutine da_calc_stats_bogus( num_obs, bogus )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (bogus_type), intent(in) :: bogus(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then
 
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, bogus(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( bogus(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'bogus u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, bogus(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( bogus(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'bogus v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, bogus(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( bogus(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'bogus t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, bogus(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( bogus(n) % q(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'bogus q  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         count1 = count1 + 1
         call da_increment_stats( bogus(n) % slp, count, &
                                  sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                  mean_omb, mean_oma, stdv_omb, stdv_oma  )
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'bogus slp', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_bogus

subroutine da_calc_stats_airep( num_obs, airep )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (airep_type), intent(in) :: airep(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % u(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'airep u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % v(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'airep v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, airep(n) % numlevs
           count1 = count1 + 1
           call da_increment_stats( airep(n) % t(k), count, &
                                    sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                    mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'airep t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_airep

subroutine da_calc_stats_pilot( num_obs, pilot )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (pilot_type), intent(in) :: pilot(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, pilot(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( pilot(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'pilot u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, pilot(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( pilot(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'pilot v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if
end subroutine da_calc_stats_pilot

subroutine da_calc_stats_profiler( num_obs, profiler )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (pilot_type), intent(in) :: profiler(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, profiler(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( profiler(n) % u(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'profiler u  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, profiler(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( profiler(n) % v(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'profiler v  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if
end subroutine da_calc_stats_profiler

subroutine da_calc_stats_gpsref( num_obs, gpsref )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (gpsref_type), intent(in) :: gpsref(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, gpsref(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( gpsref(n) % ref(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'gpsref re', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if
end subroutine da_calc_stats_gpsref  

subroutine da_calc_stats_satem( num_obs, satem )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (satem_type), intent(in) :: satem(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, satem(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( satem(n) % thickness(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'satem thk', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_satem

subroutine da_calc_stats_ssmt1( num_obs, ssmt1 )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (ssmt1_type), intent(in) :: ssmt1(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, ssmt1(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( ssmt1(n) % t(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'ssmt1 t  ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_ssmt1
 
subroutine da_calc_stats_ssmt2( num_obs, ssmt2 )

   implicit none

   integer, intent(in)           :: num_obs               ! Number of obs.
   type (ssmt2_type), intent(in) :: ssmt2(1:num_obs)      ! Obs data.

   integer                       :: n, k, count, count1
   real                          :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real                          :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( num_obs > 0 ) then

      count = 0; count1 = 0
      sum_omb = 0.0; sum_oma = 0.0; sum_omb2 = 0.0; sum_oma2 = 0.0
      do n = 1, num_obs
         do k = 1, ssmt2(n) % numlevs
            count1 = count1 + 1
            call da_increment_stats( ssmt2(n) % rh(k), count, &
                                     sum_omb, sum_oma, sum_omb2, sum_oma2, &
                                     mean_omb, mean_oma, stdv_omb, stdv_oma  )
         end do
      end do
      write(6,'(1x,a9,i10,a,i10,a,4f10.4)')'ssmt2 rh ', count, '/', count1, &
                                        '. O-B(m,s), O-A(m,s) =', &
                                        mean_omb, stdv_omb, mean_oma, stdv_oma
      write(6,*)

   end if

end subroutine da_calc_stats_ssmt2

subroutine da_increment_stats( field, count, &
                               sum_omb, sum_oma, sum_omb2, sum_oma2, &
                               mean_omb, mean_oma, stdv_omb, stdv_oma )

   implicit none

   type (field_type), intent(in) :: field
   integer, intent(inout)        :: count
   real, intent(inout)           :: sum_omb, sum_oma, sum_omb2, sum_oma2
   real, intent(out)             :: mean_omb, mean_oma, stdv_omb, stdv_oma

   if ( field % qc >= obs_qc_pointer ) then
      count = count + 1
      sum_omb = sum_omb + field % omb
      sum_oma = sum_oma + field % oma
      sum_omb2 = sum_omb2 + field % omb**2
      sum_oma2 = sum_oma2 + field % oma**2
   end if

   if ( count > 0 ) then
      mean_omb = sum_omb / real(count)
      mean_oma = sum_oma / real(count)
   end if

   ! Cannot calculate standard deviations for first observation
   if ( count > 1 ) then
      stdv_omb = sqrt( sum_omb2/ real(count) - mean_omb**2 )
      stdv_oma = sqrt( sum_oma2/ real(count) - mean_oma**2 )
   end if

end subroutine da_increment_stats

end program da_tune_obs_hollingsworth1
