program da_tune_obs_desroziers
   !---------------------------------------------------------------------
   ! Abstract:
   !   Purpose: Observation error tuning  (Desroziers method)
   !       Ref: QJRMS (2001), 127, pp. 1433-1452   
   !            Gerald Desroziers and Serguei Ivanov
   !---------------------------------------------------------------------

   implicit none

   integer,parameter :: filename_len = 200
   integer, parameter            :: rand_unit = 45
   integer, parameter            :: yp_unit   = 46
   integer, parameter            :: y_unit    = 47
   integer, parameter            :: jo_unit   = 48
   integer, parameter            :: in_unit   = 49
   integer, parameter            :: ninst     = 35  ! max sensor number
   integer, Parameter            :: nplatforms = 20
   real, parameter               :: missing_r = -888888.0
   ! below copy from RTTOV
   ! platform names
   Character (len=8), Parameter :: platform_name(nplatforms) = &
       (/ 'noaa    ', 'dmsp    ', 'meteosat', 'goes    ', 'gms     ', &
          'fy2     ', 'trmm    ', 'ers     ', 'eos     ', 'metop   ', &
          'envisat ', 'msg     ', 'fy1     ', 'adeos   ', 'mtsat   ', &
          'coriolis', 'npoess  ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx'/)

   ! List of instruments  !!!! HIRS is number 0
   Character (len=8), Dimension(0:ninst-1) :: inst_name  =                &
       & (/ 'hirs    ', 'msu     ', 'ssu     ', 'amsua   ', 'amsub   ',  &
       &    'avhrr   ', 'ssmi    ', 'vtpr1   ', 'vtpr2   ', 'tmi     ',  &
       &    'ssmis   ', 'airs    ', 'hsb     ', 'modis   ', 'atsr    ',  &
       &    'mhs     ', 'iasi    ', 'amsr    ', 'imager  ', 'atms    ',  &
       &    'mviri   ', 'seviri  ', 'imager  ', 'sounder ', 'imager  ',  &
       &    'vissr   ', 'mvisr   ', 'cris    ', 'cmis    ', 'viirs   ',  &
       &    'windsat ', 'gifts   ', 'xxxxxxxx', 'xxxxxxxx', 'xxxxxxxx'   /)

   integer                       :: n,ipixel

   ! radiance namelist variables

   integer                       :: rtminit_nsensor
   integer, dimension(ninst)     :: rtminit_platform, &
                                    rtminit_satid,    &
                                    rtminit_sensor,   &
                                    rtminit_nchan
   ! radiance relevant variables
   character*1          :: str1,str2,str3
   character*5          :: platform
   character*5          :: sensor
   integer              :: platform_id,satellite_id,sensor_id,ichan
   
   type field_type
      real                       :: yp
      real                       :: y
      real                       :: error
      real                       :: pert
   end type field_type
   
   type surfc_type
      type (field_type)          :: u
      type (field_type)          :: v
      type (field_type)          :: t
      type (field_type)          :: p
      type (field_type)          :: q
   end type surfc_type
   
   type geoamv_type
      integer                    :: numlevs
      type (field_type),pointer  :: u(:)
      type (field_type),pointer  :: v(:)
   end type geoamv_type
   
   type polaramv_type
      integer                    :: numlevs
      type (field_type),pointer  :: u(:)
      type (field_type),pointer  :: v(:)
   end type polaramv_type
   
   type gpspw_type
      type (field_type)          :: tpw
   end type gpspw_type

   type sound_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
   end type sound_type

   type airsr_type
      integer                    :: numlevs
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
   end type airsr_type  
   
   type airep_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
   end type airep_type

   type pilot_type
      integer                    :: numlevs
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
   end type pilot_type

   type ssmir_type
      type (field_type) :: speed
      type (field_type) :: tpw
   end type ssmir_type
   
   type satem_type
      integer                    :: numlevs
      type (field_type), pointer :: thickness(:)
   end type satem_type

   type ssmt1_type
      integer                    :: numlevs
      type (field_type), pointer :: t(:)
   end type ssmt1_type
   
   type ssmt2_type
      integer                    :: numlevs
      type (field_type), pointer :: rh(:)
   end type ssmt2_type 

   type qscat_type
      type (field_type)          :: u
      type (field_type)          :: v
   end type qscat_type

   type bogus_type
      integer                    :: numlevs
      type (field_type)          :: slp
      type (field_type), pointer :: u(:)
      type (field_type), pointer :: v(:)
      type (field_type), pointer :: t(:)
      type (field_type), pointer :: q(:)
   end type bogus_type

   type pixel_type
      type (field_type), pointer :: pixel(:)   ! dimension: num_rad_tot(ichan)
   end type pixel_type

   type radiance_type
      character*20               :: rttovid_string
      integer                    :: nchan
      integer, pointer           :: num_rad_tot(:)        ! dimension: nchan
      real   , pointer           :: trace_rad  (:), &     ! dimension: nchan
                                    jo_rad     (:), &     ! dimension: nchan
                                    joa_rad    (:), &     ! dimension: nchan
                                    factor_rad (:)        ! dimension: nchan
      type (pixel_type), pointer :: tb(:)                 ! dimension: nchan
   end type radiance_type

   type gpsref_type
      integer                    :: numlevs
      type (field_type), pointer :: ref(:)      
   end type gpsref_type


   type iv_type
      integer :: total_obs
      integer :: num_synop, num_metar, num_ships, &
                 num_polaramv, num_geoamv,num_gpspw, num_sound, &
                 num_airep, num_pilot, num_ssmir, num_airsr, &
                 num_satem, num_ssmt1, num_ssmt2, num_gpsref, &
                 num_buoy, num_qscat, num_sonde_sfc, num_bogus, num_profiler
      integer :: num_synop_tot, num_metar_tot, num_ships_tot, &
                 num_polaramv_tot, num_geoamv_tot, num_gpspw_tot, &
                 num_sound_tot, num_airsr_tot, &
                 num_airep_tot, num_pilot_tot, num_ssmir_tot, &
                 num_satem_tot, num_ssmt1_tot, num_ssmt2_tot, &
                 num_buoy_tot, num_qscat_tot, num_sonde_sfc_tot, num_bogus_tot, &
                 num_profiler_tot, num_gpsref_tot

      real    :: trace_total
      real    :: trace_synop, trace_metar, trace_ships, &
                 trace_polaramv, trace_geoamv, trace_gpspw, trace_sound, &
                 trace_airep, trace_pilot, trace_ssmir, trace_airsr, &
                 trace_satem, trace_ssmt1, trace_ssmt2, &
                 trace_buoy, trace_qscat, trace_sonde_sfc, trace_bogus, &
                 trace_profiler, trace_gpsref
      real    :: jo_synop_u, jo_synop_v, jo_synop_t, jo_synop_p, jo_synop_q
      real    :: jo_metar_u, jo_metar_v, jo_metar_t, jo_metar_p, jo_metar_q
      real    :: jo_ships_u, jo_ships_v, jo_ships_t, jo_ships_p, jo_ships_q
      real    :: jo_polaramv_u, jo_polaramv_v, jo_geoamv_u, jo_geoamv_v, jo_gpspw_tpw
      real    :: jo_sound_u, jo_sound_v, jo_sound_t, jo_sound_q
      real    :: jo_gpsref_ref
      real    :: jo_airsr_t, jo_airsr_q
      real    :: jo_airep_u, jo_airep_v, jo_airep_t
      real    :: jo_pilot_u, jo_pilot_v
      real    :: jo_ssmir_speed, jo_ssmir_tpw, jo_satem_thickness
      real    :: jo_ssmt1_t, jo_ssmt2_rh
      real    :: jo_buoy_u, jo_buoy_v, jo_buoy_t, jo_buoy_p, jo_buoy_q
      real    :: jo_bogus_u, jo_bogus_v, jo_bogus_t, jo_bogus_q, jo_bogus_slp
      real    :: jo_qscat_u, jo_qscat_v
      real    :: jo_sonde_sfc_u, jo_sonde_sfc_v, jo_sonde_sfc_t, jo_sonde_sfc_p, jo_sonde_sfc_q
      real    :: jo_profiler_u, jo_profiler_v
      
      real    :: joa_synop_u, joa_synop_v, joa_synop_t, joa_synop_p, joa_synop_q
      real    :: joa_metar_u, joa_metar_v, joa_metar_t, joa_metar_p, joa_metar_q
      real    :: joa_ships_u, joa_ships_v, joa_ships_t, joa_ships_p, joa_ships_q
      real    :: joa_polaramv_u, joa_polaramv_v, joa_geoamv_u, joa_geoamv_v
      real    :: joa_gpspw_tpw
      real    :: joa_sound_u, joa_sound_v, joa_sound_t, joa_sound_q
      real    :: joa_gpsref_ref
      real    :: joa_airsr_t, joa_airsr_q
      real    :: joa_airep_u, joa_airep_v, joa_airep_t
      real    :: joa_pilot_u, joa_pilot_v
      real    :: joa_ssmir_speed, joa_ssmir_tpw, joa_satem_thickness
      real    :: joa_ssmt1_t, joa_ssmt2_rh
      real    :: joa_buoy_u, joa_buoy_v, joa_buoy_t, joa_buoy_p, joa_buoy_q
      real    :: joa_bogus_u, joa_bogus_v, joa_bogus_t, joa_bogus_q, joa_bogus_slp
      real    :: joa_qscat_u, joa_qscat_v
      real    :: joa_sonde_sfc_u, joa_sonde_sfc_v, joa_sonde_sfc_t
      real    :: joa_sonde_sfc_p, joa_sonde_sfc_q
      real    :: joa_profiler_u, joa_profiler_v

      real    :: ef_synop_u, ef_synop_v, ef_synop_t, ef_synop_p, ef_synop_q
      real    :: ef_metar_u, ef_metar_v, ef_metar_t, ef_metar_p, ef_metar_q
      real    :: ef_ships_u, ef_ships_v, ef_ships_t, ef_ships_p, ef_ships_q
      real    :: ef_polaramv_u, ef_polaramv_v, ef_geoamv_u, ef_geoamv_v
      real    :: ef_gpspw_tpw
      real    :: ef_sound_u, ef_sound_v, ef_sound_t, ef_sound_q
      real    :: ef_gpsref_ref
      real    :: ef_airsr_t, ef_airsr_q
      real    :: ef_airep_u, ef_airep_v, ef_airep_t
      real    :: ef_pilot_u, ef_pilot_v
      real    :: ef_ssmir_speed, ef_ssmir_tpw, ef_satem_thickness
      real    :: ef_ssmt1_t, ef_ssmt2_rh
      real    :: ef_buoy_u, ef_buoy_v, ef_buoy_t, ef_buoy_p, ef_buoy_q
      real    :: ef_bogus_u, ef_bogus_v, ef_bogus_t, ef_bogus_q, ef_bogus_slp
      real    :: ef_qscat_u, ef_qscat_v
      real    :: ef_sonde_sfc_u, ef_sonde_sfc_v, ef_sonde_sfc_t, ef_sonde_sfc_p
      real    :: ef_sonde_sfc_q
      real    :: ef_profiler_u, ef_profiler_v

      type (surfc_type), pointer :: synop(:)
      type (surfc_type), pointer :: metar(:)
      type (surfc_type), pointer :: ships(:)
      type (polaramv_type), pointer :: polaramv(:)
      type (geoamv_type), pointer :: geoamv(:)
      type (gpspw_type), pointer :: gpspw(:)
      type (sound_type), pointer :: sound(:)
      type (gpsref_type), pointer :: gpsref(:)
      type (airsr_type), pointer :: airsr(:)
      type (airep_type), pointer :: airep(:)
      type (pilot_type), pointer :: pilot(:)
      type (ssmir_type), pointer :: ssmir(:)
      type (satem_type), pointer :: satem(:)
      type (ssmt1_type), pointer :: ssmt1(:)
      type (ssmt2_type), pointer :: ssmt2(:)
      type (surfc_type), pointer :: sonde_sfc(:)
      type (surfc_type), pointer :: buoy(:)
      type (qscat_type), pointer :: qscat(:)
      type (pilot_type), pointer :: profiler(:)
      type (bogus_type), pointer :: bogus(:)
      type (radiance_type), pointer :: rad(:)

      end type iv_type
   type (iv_type)       :: ob
   
   !--------------------------------------------------------------------------
   ! Initialise the counter
   !--------------------------------------------------------------------------

   ! ob % totla_obs         = 0
   ! ob % num_synop_tot     = 0
   ! ob % num_metar_tot     = 0
   ! ob % num_ships_tot     = 0
   ! ob % num_polaramv_tot  = 0 
   ! ob % num_geoamv_tot    = 0
   ! ob % num_gpspw_tot     = 0
   ! ob % num_sound_tot     = 0
   ! ob % num_airsr_tot     = 0
   ! ob % num_airep_tot     = 0
   ! ob % num_pilot_tot     = 0
   ! ob % num_ssmir_tot     = 0
   ! ob % num_satem_tot     = 0
   ! ob % num_ssmt1_tot     = 0
   ! ob % num_ssmt2_tot     = 0
   ! ob % num_buoy_tot      = 0
   ! ob % num_sonde_sfc_tot = 0
   ! ob % num_qscat_tot     = 0
   ! ob % num_profiler_tot  = 0
   ! ob% num_bogus_tot      = 0

   !--------------------------------------------------------------------------
   ! [1.0] Count total number of observations and allocate arrays:
   !--------------------------------------------------------------------------

   call da_count_obs( y_unit, ob )

   !--------------------------------------------------------------------------
   ! [2.0] Read in UNperturbed y = H(x_inc) from each ob type:
   !--------------------------------------------------------------------------

   call da_read_y( y_unit, ob )
   
   !--------------------------------------------------------------------------
   ! [3.0] Read in perturbed yp = H(x_inc_p) from each ob type:
   !--------------------------------------------------------------------------
 
   call da_read_yp( yp_unit, ob )

   !--------------------------------------------------------------------------
   ! [4.0] Read in perturbations and errors from each ob type:
   !--------------------------------------------------------------------------

   call da_read_obs_rand( rand_unit, ob )

   !--------------------------------------------------------------------------
   ! [5.0] Calculate expected cost function J values:
   !--------------------------------------------------------------------------

   call da_calc_jo_expected( ob )

   !--------------------------------------------------------------------------
   ! [6.0] Read actual cost function J and error tuning factors used:
   !--------------------------------------------------------------------------

   call da_read_jo_actual( ob )

   !--------------------------------------------------------------------------
   ! [7.0] Calculate observation error tuning factors:
   !--------------------------------------------------------------------------

   call da_calc_new_factors( ob )

   !--------------------------------------------------------------------------
   ! [8.0] Calculate observation error tuning factors:
   !--------------------------------------------------------------------------

   call da_get_j( ob )

contains

subroutine da_count_obs( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (iv_type), intent(inout)     :: ob

   character*20         :: ob_name, dummy
   integer              :: times, num_obs, k, num_levs, iost

   ! [1] Initialize ob numbers:

   ob % num_synop = 0
   ob % num_metar = 0
   ob % num_ships = 0
   ob % num_polaramv = 0
   ob % num_geoamv = 0
   ob % num_gpspw = 0
   ob % num_sound = 0
   ob % num_airsr = 0
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
   ob % num_qscat = 0
   ob % num_gpsref = 0
   ! [1.2] Initialize satellite instrument numbers
   !       and channel numbers for each instrument

   call read_namelist_radiance

   times = 0 
   open ( unit   = y_unit, &
          status = 'old' , access = 'sequential', &
          form   = 'formatted', action = 'read', &
          iostat = iost )
   if ( iost /= 0 ) stop ' error in opening namelist file '

   !  [2] Loop through input file to count number of obs:

   do   
      read(y_unit,*, end = 1000)ob_name, num_obs
!      read(y_unit,'(a20,i8)', end = 1000)ob_name, num_obs
      if( num_obs > 0) times = times + 1 
      if ( index( ob_name,'synop') > 0 ) then
         ob % num_synop = ob % num_synop + num_obs
      elseif ( index( ob_name,'metar') > 0 ) then
         ob % num_metar = ob % num_metar + num_obs
      elseif ( index( ob_name,'ships') > 0 ) then
         ob % num_ships = ob % num_ships + num_obs
      elseif ( index( ob_name,'polaramv') > 0 ) then
         ob % num_polaramv = ob % num_polaramv + num_obs
      elseif ( index( ob_name,'geoamv') > 0 ) then
         ob % num_geoamv = ob % num_geoamv + num_obs
      elseif ( index( ob_name,'gpspw') > 0 ) then
         ob % num_gpspw = ob % num_gpspw + num_obs
      elseif ( index( ob_name,'sound') > 0 ) then
         ob % num_sound = ob % num_sound + num_obs
      elseif ( index( ob_name,'airsr') > 0 ) then
         ob % num_airsr = ob % num_airsr + num_obs
      elseif ( index( ob_name,'airep') > 0 ) then
         ob % num_airep = ob % num_airep + num_obs
      elseif ( index( ob_name,'pilot') > 0 ) then
         ob % num_pilot = ob % num_pilot + num_obs
      elseif ( index( ob_name,'ssmir') > 0 ) then
         ob % num_ssmir = ob % num_ssmir + num_obs
      elseif ( index( ob_name,'satem') > 0 ) then
         ob % num_satem = ob % num_satem + num_obs
      elseif ( index( ob_name,'ssmt1') > 0 ) then
         ob % num_ssmt1 = ob % num_ssmt1 + num_obs
      elseif ( index( ob_name,'ssmt2') > 0 ) then
         ob % num_ssmt2 = ob % num_ssmt2 + num_obs
      else if ( index( ob_name,'qscat') > 0 ) then
         ob % num_qscat = ob % num_qscat + num_obs
      else if ( index( ob_name,'sonde_sfc') > 0 ) then
         ob % num_sonde_sfc = ob % num_sonde_sfc + num_obs
      else if ( index( ob_name,'buoy') > 0 ) then
          ob % num_buoy = ob % num_buoy  + num_obs
      else if ( index( ob_name,'profiler') > 0 ) then
          ob % num_profiler = ob % num_profiler + num_obs
      else if ( index( ob_name,'bogus') > 0 ) then
          ob % num_bogus = ob % num_bogus + num_obs
      elseif ( index( ob_name,'gpsref') > 0 ) then
         ob % num_gpsref = ob % num_gpsref + num_obs
         ! Radiance obs: consistent with RTTOV triplet and WRF-VAR
         !--------------------------------------------------------------------
      else if ( index( ob_name,'noaa') > 0 .or. index( ob_name,'eos') > 0 .or. &
                index( ob_name,'dmsp') > 0 .or. index( ob_name,'metop') > 0 ) then
         call get_sat_info(adjustl(ob_name),platform_id,satellite_id,sensor_id,ichan)
         do n = 1, rtminit_nsensor
            if (    platform_id  == rtminit_platform(n) &
             .and. satellite_id == rtminit_satid(n)    &
             .and. sensor_id    == rtminit_sensor(n)    ) then
                ob%rad(n)%num_rad_tot(ichan) = ob%rad(n)%num_rad_tot(ichan) + &
                   num_obs
               exit
            end if
         end do

      elseif ( index( ob_name,'*****') > 0 ) then 
         print*,' scaned for time ', times
         exit
      else
         print*,' unknown obs type: ',trim(ob_name),' found on unit ',y_unit
      end if

      if ( index( ob_name,'noaa') > 0 .or. index( ob_name,'eos') > 0 .or. &
           index( ob_name,'dmsp') > 0 .or. index( ob_name,'metop') > 0 ) then
         do k = 1, num_obs
            read(y_unit,'(a20)')dummy              
         end do
      else
         do n = 1, num_obs
            read(y_unit,'(i8)')num_levs
            do k = 1, num_levs 
               read(y_unit,'(a20)')dummy              
            end do
         end do
      end if    
   end do

1000 print*,' end of file reached on unit ',y_unit

   ! [3] Allocate ob structures where obs exist:

   if ( rtminit_nsensor > 0 ) then
      do n=1,rtminit_nsensor
         do ichan=1,ob%rad(n)%nchan
            if ( ob%rad(n)%num_rad_tot(ichan) > 0 ) then
               allocate(ob%rad(n)%tb(ichan)%pixel &
                  (1:ob % rad(n)%num_rad_tot(ichan)))
               write(6,'(a,i5,a,i10)') &
                  ' Number of '//trim(ob%rad(n)%rttovid_string)//&
                  ' channel ', ichan , ' = ', &
                  ob%rad(n)%num_rad_tot(ichan)
            end if
         end do
      end do
   end if

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
      write(6,'(a,i8)')' Number of Polar AMV obs = ', ob % num_polaramv
   end if
   
   if ( ob % num_geoamv > 0 ) then
      allocate( ob % geoamv(1:ob % num_geoamv) )
      write(6,'(a,i8)')' Number of Geo AMV obs = ', ob % num_geoamv
   end if
   
   if ( ob % num_gpspw > 0 ) then
      allocate( ob % gpspw(1:ob % num_gpspw) )
      write(6,'(a,i8)')' Number of gpspw obs = ', ob % num_gpspw
   end if
   
   if ( ob % num_sound > 0 ) then
      allocate( ob % sound(1:ob % num_sound) )
      write(6,'(a,i8)')' Number of sound obs = ', ob % num_sound
   end if
   if ( ob % num_airsr > 0 ) then
      allocate( ob % airsr(1:ob % num_airsr) )
      write(6,'(a,i8)')' Number of AIRS retrievals = ', ob % num_airsr
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
   if ( ob % num_bogus > 0 ) then
      allocate( ob % bogus(1:ob % num_bogus) )
      write(6,'(a,i8)')' Number of bogus obs = ', ob % num_bogus
   end if
   if ( ob % num_buoy > 0 ) then
      allocate( ob % buoy(1:ob % num_buoy) )
      write(6,'(a,i8)')' Number of buoy obs = ', ob % num_buoy
   end if
   if ( ob % num_sonde_sfc > 0 ) then
      allocate( ob % sonde_sfc(1:ob % num_sonde_sfc) )
      write(6,'(a,i8)')' Number of sonde_sfc obs = ', ob % num_sonde_sfc
   end if
   if ( ob % num_qscat > 0 ) then
      allocate( ob % qscat(1:ob % num_qscat) )
      write(6,'(a,i8)')' Number of qscat obs = ', ob % num_qscat
   end if
   if ( ob % num_profiler> 0 ) then
      allocate( ob % profiler(1:ob % num_profiler) )
      write(6,'(a,i8)')' Number of  Profiler obs = ', ob % num_profiler
   end if
   if ( ob % num_gpsref > 0 ) then
      allocate( ob % gpsref(1:ob % num_gpsref) )
      write(6,'(a,i8)')' Number of gpsref obs = ', ob % num_gpsref
   end if
   
end subroutine da_count_obs

subroutine da_read_y( y_unit, ob )

   implicit none
   
   integer, intent(in)               :: y_unit
   type (iv_type), intent(inout)     :: ob

   character*20 :: ob_name
   integer      :: n, ndum, k, kdum, num_obs, num_levs   
   integer      :: isynop, imetar, iships, ipolaramv, igeoamv, igpspw, isound, &
                   iairep, ipilot, issmir, isatem, issmt1, issmt2, iairsr, &
                   ibuoy, isonde_sfc, iqscat, ibogus, iprofiler, igpsref

   rewind (y_unit)

   isynop = 0
   imetar = 0
   iships = 0
   ipolaramv = 0
   igeoamv = 0
   igpspw = 0
   isound = 0
   iairsr = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0
   ibuoy = 0
   isonde_sfc = 0 
   iqscat = 0 
   ibogus = 0 
   iprofiler = 0
   igpsref = 0

   do n = 1,rtminit_nsensor
      ob%rad(n)%num_rad_tot(:) = 0
   end do

   do  
      read(y_unit,'(a20,i8)', end = 1000)ob_name, num_obs
      
      if ( index( ob_name,'synop') > 0 ) then
         do n = 1, num_obs
            isynop = isynop + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % synop(isynop) % u % y, &
                                        ob % synop(isynop) % v % y, &
                                        ob % synop(isynop) % t % y, &
                                        ob % synop(isynop) % p % y, &
                                        ob % synop(isynop) % q % y
         end do
      elseif ( index( ob_name,'metar') > 0 ) then
         do n = 1, num_obs
            imetar = imetar + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % metar(imetar) % u % y, &
                                        ob % metar(imetar) % v % y, &
                                        ob % metar(imetar) % t % y, &
                                        ob % metar(imetar) % p % y, &
                                        ob % metar(imetar) % q % y
         end do
      elseif ( index( ob_name,'ships') > 0 ) then
         do n = 1, num_obs
            iships = iships + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % ships(iships) % u % y, &
                                        ob % ships(iships) % v % y, &
                                        ob % ships(iships) % t % y, &
                                        ob % ships(iships) % p % y, &
                                        ob % ships(iships) % q % y
         end do

      elseif ( index( ob_name,'geoamv') > 0 ) then

         do n = 1, num_obs
            igeoamv = igeoamv + 1
            read(y_unit,'(i8)')num_levs
            ob % geoamv(igeoamv) % numlevs = num_levs
            allocate( ob % geoamv(igeoamv) % u(1:num_levs) )
            allocate( ob % geoamv(igeoamv) % v(1:num_levs) )
            do k = 1, num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                        ob % geoamv(igeoamv) % u(k) % y, &
                                        ob % geoamv(igeoamv) % v(k) % y
            end do
         end do

      elseif ( index( ob_name,'polaramv') > 0 ) then

         do n = 1, num_obs
            ipolaramv = ipolaramv + 1
            read(y_unit,'(i8)')num_levs
            ob % polaramv(ipolaramv) % numlevs = num_levs
            allocate( ob % polaramv(ipolaramv) % u(1:num_levs) )
            allocate( ob % polaramv(ipolaramv) % v(1:num_levs) )
            do k = 1, num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                        ob % polaramv(ipolaramv) % u(k) % y, &
                                        ob % polaramv(ipolaramv) % v(k) % y
            end do
         end do

      elseif ( index( ob_name,'gpspw') > 0 ) then

         do n = 1, num_obs
            igpspw = igpspw + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                        ob % gpspw(igpspw) % tpw % y
         end do

      elseif ( index( ob_name,'sound') > 0 ) then
         do n = 1, num_obs
            isound = isound + 1
            read(y_unit,'(i8)')num_levs
            ob % sound(isound) % numlevs = num_levs
            allocate( ob % sound(isound) % u(1:num_levs) )
            allocate( ob % sound(isound) % v(1:num_levs) )
            allocate( ob % sound(isound) % t(1:num_levs) )
            allocate( ob % sound(isound) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % sound(isound) % u(k) % y, &
                                           ob % sound(isound) % v(k) % y, &
                                           ob % sound(isound) % t(k) % y, &
                                           ob % sound(isound) % q(k) % y
            end do
         end do

      elseif ( index( ob_name,'airsr') > 0 ) then
         do n = 1, num_obs
            iairsr = iairsr + 1
            read(y_unit,'(i8)')num_levs
            ob % airsr(iairsr) % numlevs = num_levs
            allocate( ob % airsr(iairsr) % t(1:num_levs) )
            allocate( ob % airsr(iairsr) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % airsr(iairsr) % t(k) % y, &
                                           ob % airsr(iairsr) % q(k) % y
            end do
         end do
      elseif ( index( ob_name,'airep') > 0 ) then
         do n = 1, num_obs
            iairep = iairep + 1
            read(y_unit,'(i8)')num_levs
            ob % airep(iairep) % numlevs = num_levs
            allocate( ob % airep(iairep) % u(1:num_levs) )
            allocate( ob % airep(iairep) % v(1:num_levs) )
            allocate( ob % airep(iairep) % t(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % airep(iairep) % u(k) % y, &
                                           ob % airep(iairep) % v(k) % y, &
                                           ob % airep(iairep) % t(k) % y
            end do
         end do
      elseif ( index( ob_name,'pilot') > 0 ) then
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(y_unit,'(i8)')num_levs
            ob % pilot(ipilot) % numlevs = num_levs
            allocate( ob % pilot(ipilot) % u(1:num_levs) )
            allocate( ob % pilot(ipilot) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % pilot(ipilot) % u(k) % y, &
                                           ob % pilot(ipilot) % v(k) % y
            end do
         end do
      elseif ( index( ob_name,'ssmir') > 0 ) then
         do n = 1, num_obs
            issmir = issmir + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                        ob % ssmir(issmir) % speed % y, &
                                        ob % ssmir(issmir) % tpw % y
         end do
      elseif ( index( ob_name,'satem') > 0 ) then
         do n = 1, num_obs
            isatem = isatem + 1
            read(y_unit,'(i8)')num_levs
            ob % satem(isatem) % numlevs = num_levs
            allocate( ob % satem(isatem) % thickness(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % satem(isatem) % thickness(k) % y
            end do
         end do
      elseif ( index( ob_name,'ssmt1') > 0 ) then
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(y_unit,'(i8)')num_levs
            ob % ssmt1(issmt1) % numlevs = num_levs
            allocate( ob % ssmt1(issmt1) % t(1:num_levs) )
            
            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % ssmt1(issmt1) % t(k) % y
            end do
         end do
      elseif ( index( ob_name,'ssmt2') > 0 ) then
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(y_unit,'(i8)')num_levs
            ob % ssmt2(issmt2) % numlevs = num_levs
            allocate( ob % ssmt2(issmt2) % rh(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % ssmt2(issmt2) % rh(k) % y
            end do
         end do
      elseif ( index( ob_name,'buoy') > 0 ) then
         do n = 1, num_obs
            ibuoy = ibuoy + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % buoy(ibuoy) % u % y, &
                                        ob % buoy(ibuoy) % v % y, &
                                        ob % buoy(ibuoy) % t % y, &
                                        ob % buoy(ibuoy) % p % y, &
                                        ob % buoy(ibuoy) % q % y
         end do
      elseif ( index( ob_name,'sonde_sfc') > 0 ) then
         do n = 1, num_obs
            isonde_sfc = isonde_sfc + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % sonde_sfc(isonde_sfc) % u % y, &
                                        ob % sonde_sfc(isonde_sfc) % v % y, &
                                        ob % sonde_sfc(isonde_sfc) % t % y, &
                                        ob % sonde_sfc(isonde_sfc) % p % y, &
                                        ob % sonde_sfc(isonde_sfc) % q % y
         end do

      elseif ( index( ob_name,'qscat') > 0 ) then
         do n = 1, num_obs
            iqscat = iqscat + 1
            read(y_unit,'(i8)')num_levs
            read(y_unit,'(2i8,7e15.7)')ndum, kdum, ob % qscat(iqscat) % u % y, &
                                        ob % qscat(iqscat) % v % y
         end do
      elseif ( index( ob_name,'profiler') > 0 ) then
         do n = 1, num_obs
            iprofiler = iprofiler + 1
            read(y_unit,'(i8)')num_levs
            ob % profiler(iprofiler) % numlevs = num_levs
            allocate( ob % profiler(iprofiler) % u(1:num_levs) )
            allocate( ob % profiler(iprofiler) % v(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % profiler(iprofiler) % u(k) % y, &
                                           ob % profiler(iprofiler) % v(k) % y
            end do
         end do
      elseif ( index( ob_name,'bogus') > 0 ) then
         do n = 1, num_obs
            ibogus = ibogus + 1
            read(y_unit,'(i8)')kdum     
            read(y_unit,'(2i8,e15.7)')ndum, kdum, &
                                   ob % bogus(ibogus) % slp % y
            read(y_unit,'(i8)')num_levs
            ob % bogus(ibogus) % numlevs = num_levs
            allocate( ob % bogus(ibogus) % u(1:num_levs) )
            allocate( ob % bogus(ibogus) % v(1:num_levs) )
            allocate( ob % bogus(ibogus) % t(1:num_levs) )
            allocate( ob % bogus(ibogus) % q(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,4e15.7)')ndum, kdum, &
                                           ob % bogus(ibogus) % u(k) % y, &
                                           ob % bogus(ibogus) % v(k) % y, &
                                           ob % bogus(ibogus) % t(k) % y, &
                                           ob % bogus(ibogus) % q(k) % y
            end do
         end do
      elseif ( index( ob_name,'gpsref') > 0 ) then
         do n = 1, num_obs
            igpsref = igpsref + 1
            read(y_unit,'(i8)')num_levs
            ob % gpsref(igpsref) % numlevs = num_levs
            allocate( ob % gpsref(igpsref) % ref(1:num_levs) )

            do k = 1, num_levs
               read(y_unit,'(2i8,7e15.7)')ndum, kdum, &
                                           ob % gpsref(igpsref) % ref(k) % y
            end do
         end do
         ! Radiance obs: consistent with RTTOV triplet and WRF-VAR
         !--------------------------------------------------------------------
      else if ( index( ob_name,'noaa') > 0 .or. index( ob_name,'eos') > 0 .or. &
                index( ob_name,'dmsp') > 0 .or. index( ob_name,'metop') > 0 ) then
         call get_sat_info(adjustl(ob_name),platform_id,satellite_id,sensor_id,ichan)
         do n = 1, rtminit_nsensor
           if (    platform_id  == rtminit_platform(n) &
             .and. satellite_id == rtminit_satid(n)    &
             .and. sensor_id    == rtminit_sensor(n)    ) then
                do k = 1,num_obs
                  ob%rad(n)%num_rad_tot(ichan) = ob%rad(n)%num_rad_tot(ichan) + 1
                  read(y_unit,'(2i8,e15.7)') ipixel,kdum,  &
                        ob%rad(n)%tb(ichan)%pixel(ob%rad(n)%num_rad_tot(ichan))%y
                end do
                exit
           end if
         end do

      elseif ( index( ob_name,'*****') > 0 ) then 
         exit
      else
         print*,' unknown obs type: ',trim(ob_name),' found on unit ',y_unit
      end if
   end do
1000 print*,' end of file reached on unit ',y_unit
   
end subroutine da_read_y



subroutine da_read_yp( yp_unit, ob )

   implicit none
   
   integer, intent(in)               :: yp_unit
   type (iv_type), intent(inout)     :: ob

   character*20 :: ob_name
   integer      :: n, ndum, k, kdum, num_obs, num_levs   
   integer      :: isynop, imetar, iships, ipolaramv, igeoamv, igpspw, isound, &
                   iairep, ipilot, issmir, isatem, issmt1, issmt2, iairsr, &
                   ibuoy, isonde_sfc, iqscat, ibogus, iprofiler, igpsref

   rewind (yp_unit)

   isynop = 0
   imetar = 0
   iships = 0
   ipolaramv = 0
   igeoamv = 0
   igpspw = 0
   isound = 0
   iairsr = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0
   ibuoy = 0
   isonde_sfc = 0 
   iqscat = 0 
   ibogus = 0 
   iprofiler = 0
   igpsref = 0

   do n = 1,rtminit_nsensor
      ob%rad(n)%num_rad_tot(:) = 0
   end do

   do 
      read(yp_unit,'(a20,i8)', end=1000)ob_name, num_obs
      
      if ( index( ob_name,'synop') > 0 ) then
         do n = 1, num_obs
            isynop = isynop + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)') &
               ndum, kdum, ob % synop(isynop) % u % yp, &
               ob % synop(isynop) % v % yp, &
               ob % synop(isynop) % t % yp, &
               ob % synop(isynop) % p % yp, &
               ob % synop(isynop) % q % yp
         end do
      elseif ( index( ob_name,'metar') > 0 ) then
         do n = 1, num_obs
            imetar = imetar + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)') &
               ndum, kdum, ob % metar(imetar) % u % yp, &
               ob % metar(imetar) % v % yp, &
               ob % metar(imetar) % t % yp, &
               ob % metar(imetar) % p % yp, &
               ob % metar(imetar) % q % yp
         end do
      elseif ( index( ob_name,'ships') > 0 ) then
         do n = 1, num_obs
            iships = iships + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, ob % ships(iships) % u % yp, &
               ob % ships(iships) % v % yp, &
               ob % ships(iships) % t % yp, &
               ob % ships(iships) % p % yp, &
               ob % ships(iships) % q % yp
         end do

      elseif ( index( ob_name,'geoamv') > 0 ) then
         do n = 1, num_obs
            igeoamv = igeoamv + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
               ob % geoamv(igeoamv) % u(k) % yp, &
               ob % geoamv(igeoamv) % v(k) % yp
           end do
         end do

      elseif ( index( ob_name,'polaramv') > 0 ) then
         do n = 1, num_obs
            ipolaramv = ipolaramv + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
               ob % polaramv(ipolaramv) % u(k) % yp, &
               ob % polaramv(ipolaramv) % v(k) % yp
           end do
         end do

      elseif ( index( ob_name,'gpspw') > 0 ) then

         do n = 1, num_obs
            igpspw = igpspw + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                                        ob % gpspw(igpspw) % tpw % yp
         end do
         
      elseif ( index( ob_name,'sound') > 0 ) then
         do n = 1, num_obs
            isound = isound + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % sound(isound) % u(k) % yp, &
                  ob % sound(isound) % v(k) % yp, &
                  ob % sound(isound) % t(k) % yp, &
                  ob % sound(isound) % q(k) % yp
            end do
         end do
         
      elseif ( index( ob_name,'airsr') > 0 ) then
         do n = 1, num_obs
            iairsr = iairsr + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % airsr(iairsr) % t(k) % yp, &
                  ob % airsr(iairsr) % q(k) % yp
            end do
         end do
      elseif ( index( ob_name,'airep') > 0 ) then
         do n = 1, num_obs
            iairep = iairep + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % airep(iairep) % u(k) % yp, &
                  ob % airep(iairep) % v(k) % yp, &
                  ob % airep(iairep) % t(k) % yp
            end do
         end do
      elseif ( index( ob_name,'pilot') > 0 ) then
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % pilot(ipilot) % u(k) % yp, &
                  ob % pilot(ipilot) % v(k) % yp
            end do
         end do
      elseif ( index( ob_name,'ssmir') > 0 ) then
         do n = 1, num_obs
            issmir = issmir + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
               ob % ssmir(issmir) % speed % yp, &
               ob % ssmir(issmir) % tpw % yp
         end do
      elseif ( index( ob_name,'satem') > 0 ) then
         do n = 1, num_obs
            isatem = isatem + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % satem(isatem) % thickness(k) % yp
            end do
         end do
      elseif ( index( ob_name,'ssmt1') > 0 ) then
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % ssmt1(issmt1) % t(k) % yp
            end do
         end do
      elseif ( index( ob_name,'ssmt2') > 0 ) then
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % ssmt2(issmt2) % rh(k) % yp
            end do
         end do
      elseif ( index( ob_name,'buoy') > 0 ) then
         do n = 1, num_obs
            ibuoy = ibuoy + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, ob % buoy(ibuoy) % u % yp, &
               ob % buoy(ibuoy) % v % yp, &
               ob % buoy(ibuoy) % t % yp, &
               ob % buoy(ibuoy) % p % yp, &
               ob % buoy(ibuoy) % q % yp
         end do
      elseif ( index( ob_name,'sonde_sfc') > 0 ) then
         do n = 1, num_obs
            isonde_sfc = isonde_sfc + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, ob % sonde_sfc(isonde_sfc) % u % yp, &
               ob % sonde_sfc(isonde_sfc) % v % yp, &
               ob % sonde_sfc(isonde_sfc) % t % yp, &
               ob % sonde_sfc(isonde_sfc) % p % yp, &
               ob % sonde_sfc(isonde_sfc) % q % yp
         end do

      elseif ( index( ob_name,'qscat') > 0 ) then
         do n = 1, num_obs
            iqscat = iqscat + 1
            read(yp_unit,'(i8)')num_levs
            read(yp_unit,'(2i8,7e15.7)')ndum, kdum, ob % qscat(iqscat) % u % yp, &
               ob % qscat(iqscat) % v % yp
         end do
      elseif ( index( ob_name,'profiler') > 0 ) then
         do n = 1, num_obs
            iprofiler = iprofiler + 1
            read(yp_unit,'(i8)')num_levs
            ob % profiler(iprofiler) % numlevs = num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % profiler(iprofiler) % u(k) % yp, &
                  ob % profiler(iprofiler) % v(k) % yp
            end do
         end do
      elseif ( index( ob_name,'bogus') > 0 ) then
         do n = 1, num_obs
            ibogus = ibogus + 1
            read(yp_unit,'(i8)') kdum
            read(yp_unit,'(2i8,e15.7)')ndum, kdum, ob%bogus(ibogus)%slp%yp  
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,4e15.7)')ndum, kdum, &
                  ob % bogus(ibogus) % u(k) % yp, &
                  ob % bogus(ibogus) % v(k) % yp, &
                  ob % bogus(ibogus) % t(k) % yp, &
                  ob % bogus(ibogus) % q(k) % yp
            end do
         end do
      elseif ( index( ob_name,'gpsref') > 0 ) then
         do n = 1, num_obs
            igpsref = igpsref + 1
            read(yp_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(yp_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % gpsref(igpsref) % ref(k) % yp
            end do
         end do
         ! Radiance obs: consistent with RTTOV triplet and WRF-VAR
         !--------------------------------------------------------------------
      else if ( index( ob_name,'noaa') > 0 .or. index( ob_name,'eos') > 0 .or. &
                index( ob_name,'dmsp') > 0 .or. index( ob_name,'metop') > 0 ) then
         call get_sat_info(adjustl(ob_name),platform_id,satellite_id,sensor_id,ichan)
         do n = 1, rtminit_nsensor
           if (    platform_id  == rtminit_platform(n) &
             .and. satellite_id == rtminit_satid(n)    &
             .and. sensor_id    == rtminit_sensor(n)    ) then
                do k = 1,num_obs
                  ob%rad(n)%num_rad_tot(ichan) = ob%rad(n)%num_rad_tot(ichan)+1
                  read(yp_unit,'(2i8,e15.7)') ipixel, kdum, &
                     ob%rad(n)%tb(ichan)%pixel(ob%rad(n)%num_rad_tot(ichan))%yp
                end do
                exit
           end if
         end do

      elseif ( index( ob_name,'*****') > 0 ) then 
         exit
      else
      print*,' unknown obs type: ',trim(ob_name),' found on unit ',yp_unit
      end if
   end do
1000 print*,' end of file reached on unit ',yp_unit
   
end subroutine da_read_yp
  


subroutine da_read_obs_rand( rand_unit, ob )

   implicit none
   
   integer, intent(in)               :: rand_unit
   type (iv_type), intent(inout)     :: ob

   character*20 :: ob_name
   integer      :: n, ndum, k, kdum, num_obs, num_levs
   integer      :: isynop, imetar, iships, ipolaramv, igeoamv, igpspw, isound, &
                   iairep, ipilot, issmir, isatem, issmt1, issmt2, iairsr, &
                   ibuoy, isonde_sfc, iqscat, ibogus, iprofiler, igpsref

   isynop = 0
   imetar = 0
   iships = 0
   ipolaramv = 0
   igeoamv = 0
   igpspw = 0
   isound = 0
   iairsr = 0
   iairep = 0
   ipilot = 0
   issmir = 0
   isatem = 0
   issmt1 = 0
   issmt2 = 0   
   ibuoy = 0
   isonde_sfc = 0 
   iqscat = 0 
   ibogus = 0 
   iprofiler = 0
   igpsref = 0
   do n = 1,rtminit_nsensor
     ob%rad(n)%num_rad_tot(:) = 0
   end do

   rewind( rand_unit )

   do
      read(rand_unit,'(a20,i8)',end=1000)ob_name, num_obs
      if ( index( ob_name,'synop') > 0 ) then
         do n = 1, num_obs
            isynop = isynop + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % synop(isynop) % u % error, ob % synop(isynop) % u % pert, &
            ob % synop(isynop) % v % error, ob % synop(isynop) % v % pert, &
            ob % synop(isynop) % t % error, ob % synop(isynop) % t % pert, &
            ob % synop(isynop) % p % error, ob % synop(isynop) % p % pert, &
            ob % synop(isynop) % q % error, ob % synop(isynop) % q % pert
         end do
      elseif ( index( ob_name,'metar') > 0 ) then
         
         do n = 1, num_obs
            imetar = imetar + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % metar(imetar) % u % error, ob % metar(imetar) % u % pert, &
            ob % metar(imetar) % v % error, ob % metar(imetar) % v % pert, &
            ob % metar(imetar) % t % error, ob % metar(imetar) % t % pert, &
            ob % metar(imetar) % p % error, ob % metar(imetar) % p % pert, &
            ob % metar(imetar) % q % error, ob % metar(imetar) % q % pert
         end do
         
      elseif ( index( ob_name,'ships') > 0 ) then
         
         do n = 1, num_obs
            iships = iships + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % ships(iships) % u % error, ob % ships(iships) % u % pert, &
            ob % ships(iships) % v % error, ob % ships(iships) % v % pert, &
            ob % ships(iships) % t % error, ob % ships(iships) % t % pert, &
            ob % ships(iships) % p % error, ob % ships(iships) % p % pert, &
            ob % ships(iships) % q % error, ob % ships(iships) % q % pert
         end do

      elseif ( index( ob_name,'geoamv') > 0 ) then
         
         do n = 1, num_obs
            igeoamv = igeoamv + 1
            read(rand_unit,'(i8)')num_levs
            do k = 1, num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
            ob % geoamv(igeoamv) % u(k) % error, ob % geoamv(igeoamv) % u(k) % pert, &
            ob % geoamv(igeoamv) % v(k) % error, ob % geoamv(igeoamv) % v(k) % pert
            end do
         end do

      elseif ( index( ob_name,'polaramv') > 0 ) then
         
         do n = 1, num_obs
            ipolaramv = ipolaramv + 1
            read(rand_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % polaramv(ipolaramv) % u(k) % error, &
                  ob % polaramv(ipolaramv) % u(k) % pert, &
                  ob % polaramv(ipolaramv) % v(k) % error, &
                  ob % polaramv(ipolaramv) % v(k) % pert
            end do
         end do

      elseif ( index( ob_name,'gpspw') > 0 ) then
         
         do n = 1, num_obs
            igpspw = igpspw + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % gpspw(igpspw) % tpw % error, ob % gpspw(igpspw) % tpw % pert
         end do

      elseif ( index( ob_name,'sound') > 0 ) then
         
         do n = 1, num_obs
            isound = isound + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % sound(isound) % u(k) % error, &
                  ob % sound(isound) % u(k) % pert, &
                  ob % sound(isound) % v(k) % error, &
                  ob % sound(isound) % v(k) % pert, &
                  ob % sound(isound) % t(k) % error, &
                  ob % sound(isound) % t(k) % pert, &
                  ob % sound(isound) % q(k) % error, &
                  ob % sound(isound) % q(k) % pert
            end do
         end do

      elseif ( index( ob_name,'airsr') > 0 ) then
         
         do n = 1, num_obs
            iairsr = iairsr + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % airsr(iairsr) % t(k) % error, &
                  ob % airsr(iairsr) % t(k) % pert, &
                  ob % airsr(iairsr) % q(k) % error, &
                  ob % airsr(iairsr) % q(k) % pert
            end do
         end do

      elseif ( index( ob_name,'airep') > 0 ) then
         
         do n = 1, num_obs
            iairep = iairep + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % airep(iairep) % u(k) % error, &
                  ob % airep(iairep) % u(k) % pert, &
                  ob % airep(iairep) % v(k) % error, &
                  ob % airep(iairep) % v(k) % pert, &
                  ob % airep(iairep) % t(k) % error, &
                  ob % airep(iairep) % t(k) % pert
            end do
         end do

      elseif ( index( ob_name,'pilot') > 0 ) then
         
         do n = 1, num_obs
            ipilot = ipilot + 1
            read(rand_unit,'(i8)')num_levs
            
            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % pilot(ipilot) % u(k) % error, &
                  ob % pilot(ipilot) % u(k) % pert, &
                  ob % pilot(ipilot) % v(k) % error, &
                  ob % pilot(ipilot) % v(k) % pert
            end do
         end do
         
      elseif ( index( ob_name,'ssmir') > 0 ) then

         do n = 1, num_obs
            issmir = issmir + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % ssmir(issmir) % speed % error, &
               ob % ssmir(issmir) % speed % pert, &
               ob % ssmir(issmir) % tpw % error, &
               ob % ssmir(issmir) % tpw % pert
         end do
      
      elseif ( index( ob_name,'satem') > 0 ) then
    
         do n = 1, num_obs
            isatem = isatem + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % satem(isatem) % thickness(k) % error, &
                  ob % satem(isatem) % thickness(k) % pert
            end do
         end do
         
      elseif ( index( ob_name,'ssmt1') > 0 ) then
         
         do n = 1, num_obs
            issmt1 = issmt1 + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % ssmt1(issmt1) % t % error, ob % ssmt1(issmt1) % t % pert
            end do
         end do

      elseif ( index( ob_name,'ssmt2') > 0 ) then
         
         do n = 1, num_obs
            issmt2 = issmt2 + 1
            read(rand_unit,'(i8)')num_levs

            do k = 1, num_levs
               read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                  ob % ssmt2(issmt2) % rh % error, &
                  ob % ssmt2(issmt2) % rh % pert
            end do
         end do
      elseif ( index( ob_name,'buoy') > 0 ) then
         do n = 1, num_obs
            ibuoy = ibuoy + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % buoy(ibuoy) % u % error, ob % buoy(ibuoy) % u % pert, &
               ob % buoy(ibuoy) % v % error, ob % buoy(ibuoy) % v % pert, &
               ob % buoy(ibuoy) % t % error, ob % buoy(ibuoy) % t % pert, &
               ob % buoy(ibuoy) % p % error, ob % buoy(ibuoy) % p % pert, &
               ob % buoy(ibuoy) % q % error, ob % buoy(ibuoy) % q % pert
         end do
      elseif ( index( ob_name,'sonde_sfc') > 0 ) then
         do n = 1, num_obs
            isonde_sfc = isonde_sfc + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % sonde_sfc(isonde_sfc) % u % error, &
               ob % sonde_sfc(isonde_sfc) % u % pert, &
               ob % sonde_sfc(isonde_sfc) % v % error, &
               ob % sonde_sfc(isonde_sfc) % v % pert, &
               ob % sonde_sfc(isonde_sfc) % t % error, &
               ob % sonde_sfc(isonde_sfc) % t % pert, &
               ob % sonde_sfc(isonde_sfc) % p % error, &
               ob % sonde_sfc(isonde_sfc) % p % pert, &
               ob % sonde_sfc(isonde_sfc) % q % error, &
               ob % sonde_sfc(isonde_sfc) % q % pert
         end do

      elseif ( index( ob_name,'qscat') > 0 ) then
         do n = 1, num_obs
            iqscat = iqscat + 1
            read(rand_unit,'(i8)')num_levs
            read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
               ob % qscat(iqscat) % u % error, ob % qscat(iqscat) % u % pert, &
               ob % qscat(iqscat) % v % error, ob % qscat(iqscat) % v % pert
         end do
      elseif ( index( ob_name,'profiler') > 0 ) then
         do n = 1, num_obs
            iprofiler = iprofiler + 1
            read(rand_unit,'(i8)')num_levs
             do k = 1, num_levs
                read(rand_unit,'(2i8,10e15.7)')ndum, kdum, &
                   ob % profiler(iprofiler) % u(k) % error, &
                   ob % profiler(iprofiler) % u(k) % pert, &
                   ob % profiler(iprofiler) % v(k) % error, &
                   ob % profiler(iprofiler) % v(k) % pert
             end do
         end do
      elseif ( index( ob_name,'bogus') > 0 ) then
         do n = 1, num_obs
            ibogus = ibogus + 1
            read(rand_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(rand_unit,'(2i8,8e15.7)')ndum, kdum, &
                  ob % bogus(ibogus) % u(k) % error, &
                  ob % bogus(ibogus) % u(k) % pert, &
                  ob % bogus(ibogus) % v(k) % error, &
                  ob % bogus(ibogus) % v(k) % pert, &
                  ob % bogus(ibogus) % t(k) % error, &
                  ob % bogus(ibogus) % t(k) % pert, &
                  ob % bogus(ibogus) % q(k) % error, &
                  ob % bogus(ibogus) % q(k) % pert
            end do
            read(rand_unit,'(2i8,2e15.7)')ndum, kdum, &
               ob % bogus(ibogus) % slp % error, ob % bogus(ibogus) % slp % pert
         end do
      elseif ( index( ob_name,'gpsref') > 0 ) then
         do n = 1, num_obs
            igpsref = igpsref + 1
            read(rand_unit,'(i8)')num_levs
            do k = 1, num_levs
               read(rand_unit,'(2i8,7e15.7)')ndum, kdum, &
                  ob % gpsref(igpsref) % ref(k) % error, &
                  ob % gpsref(igpsref) % ref(k) % pert
            end do
         end do

         ! Radiance obs: consistent with RTTOV triplet and WRF-VAR
         !--------------------------------------------------------------------
      else if ( index( ob_name,'noaa') > 0 .or. index( ob_name,'eos') > 0 .or. &
                index( ob_name,'dmsp') > 0 .or. index( ob_name,'metop') > 0 ) then
         call get_sat_info(adjustl(ob_name),platform_id,satellite_id,sensor_id,ichan)
         do n = 1, rtminit_nsensor
            if (    platform_id  == rtminit_platform(n) &
               .and. satellite_id == rtminit_satid(n)    &
               .and. sensor_id    == rtminit_sensor(n)    ) then
               do k = 1,num_obs
                  ob%rad(n)%num_rad_tot(ichan) = ob%rad(n)%num_rad_tot(ichan) + 1
                  read(rand_unit,'(2i8,f10.3,e15.7)') ipixel, kdum,     &
                     ob%rad(n)%tb(ichan)%pixel( &
                     ob%rad(n)%num_rad_tot(ichan))%error, &
                     ob%rad(n)%tb(ichan)%pixel( &
                     ob%rad(n)%num_rad_tot(ichan))%pert
               end do
               exit
            end if
         end do

      elseif ( index( ob_name,'*****') > 0 ) then 
         exit
      else
         print*,' unknown obs type: ',trim(ob_name),' found on unit ',rand_unit
      end if
   end do
1000 print*,' end of file reached on unit ',rand_unit
   
end subroutine da_read_obs_rand

subroutine da_calc_jo_expected( ob )

   implicit none

   type (iv_type), intent(inout)     :: ob

   integer              :: n, k
   integer              :: count1, count2, count3, count4, count5
   real                 :: trace1, trace2, trace3, trace4, trace5
   
   ob % trace_total = 0
   
   ob % num_synop_tot = 0
   ob % trace_synop   = 0
   if ( ob % num_synop > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_synop
         call da_calc_trace_single( ob % synop(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % synop(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % synop(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % synop(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % synop(n) % q, count5, trace5 )
      end do
      
      ob % jo_synop_u = 0.5 * ( count1 - trace1 )
      ob % jo_synop_v = 0.5 * ( count2 - trace2 )
      ob % jo_synop_t = 0.5 * ( count3 - trace3 )
      ob % jo_synop_p = 0.5 * ( count4 - trace4 )
      ob % jo_synop_q = 0.5 * ( count5 - trace5 )
      ob % trace_synop   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_synop < 0.0 ) &
      write(6,'(a,f15.5)')' Warning: ob % trace_synop < 0 = ', ob % trace_synop

      ob % trace_total   = ob % trace_total + ob % trace_synop
      ob % num_synop_tot = count1 + count2 + count3 + count4 + count5
   end if

   ob % num_metar_tot = 0
   ob % trace_metar   = 0
   if ( ob % num_metar > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_metar
         call da_calc_trace_single( ob % metar(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % metar(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % metar(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % metar(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % metar(n) % q, count5, trace5 )
      end do
      
      ob % jo_metar_u = 0.5 * ( count1 - trace1 )
      ob % jo_metar_v = 0.5 * ( count2 - trace2 )
      ob % jo_metar_t = 0.5 * ( count3 - trace3 )
      ob % jo_metar_p = 0.5 * ( count4 - trace4 )
      ob % jo_metar_q = 0.5 * ( count5 - trace5 )
      ob % trace_metar   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_metar < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_metar < 0 = ', ob % trace_metar

      ob % trace_total = ob % trace_total + ob % trace_metar
      ob % num_metar_tot = count1 + count2 + count3 + count4 + count5

   end if

   ob % num_ships_tot = 0
   ob % trace_ships   = 0
   if ( ob % num_ships > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_ships
         call da_calc_trace_single( ob % ships(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % ships(n) % v, count2, trace2 )
         call da_calc_trace_single( ob % ships(n) % t, count3, trace3 )
         call da_calc_trace_single( ob % ships(n) % p, count4, trace4 )
         call da_calc_trace_single( ob % ships(n) % q, count5, trace5 )
      end do
      
      ob % jo_ships_u = 0.5 * ( count1 - trace1 )
      ob % jo_ships_v = 0.5 * ( count2 - trace2 )
      ob % jo_ships_t = 0.5 * ( count3 - trace3 )
      ob % jo_ships_p = 0.5 * ( count4 - trace4 )
      ob % jo_ships_q = 0.5 * ( count5 - trace5 )
      ob % trace_ships   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_ships < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_ships < 0 = ', ob % trace_ships
      ob % trace_total = ob % trace_total + ob % trace_ships
      ob % num_ships_tot = count1 + count2 + count3 + count4 + count5
   end if

   ob % num_geoamv_tot = 0
   ob % trace_geoamv   = 0
   if ( ob % num_geoamv > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_geoamv
         do k = 1, ob % geoamv(n) % numlevs
            call da_calc_trace_single( ob % geoamv(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % geoamv(n) % v(k), count2, trace2 )
         end do
      end do
      
      ob % jo_geoamv_u = 0.5 * ( count1 - trace1 )
      ob % jo_geoamv_v = 0.5 * ( count2 - trace2 )
      ob % trace_geoamv   = trace1 + trace2

      if ( ob % trace_geoamv < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_geoamv < 0 = ', ob % trace_geoamv

      ob % trace_total = ob % trace_total + ob % trace_geoamv
      ob % num_geoamv_tot = count1 + count2
   end if

   ob % num_polaramv_tot = 0
   ob % trace_polaramv   = 0
   if ( ob % num_polaramv > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_polaramv
         do k = 1, ob % polaramv(n) % numlevs
         call da_calc_trace_single( ob % polaramv(n) % u(k), count1, trace1 )
         call da_calc_trace_single( ob % polaramv(n) % v(k), count2, trace2 )
         end do
      end do
      
      ob % jo_polaramv_u = 0.5 * ( count1 - trace1 )
      ob % jo_polaramv_v = 0.5 * ( count2 - trace2 )
      ob % trace_polaramv   = trace1 + trace2

      if ( ob % trace_polaramv < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_polaramv < 0 = ', ob % trace_polaramv

      ob % trace_total = ob % trace_total + ob % trace_polaramv
      ob % num_polaramv_tot = count1 + count2
   end if

   ob % num_gpspw_tot = 0
   ob % trace_gpspw   = 0
   if ( ob % num_gpspw > 0 ) then
      trace1 = 0.0
      count1 = 0
 
      do n = 1, ob % num_gpspw
         call da_calc_trace_single( ob % gpspw(n) % tpw, count1, trace1 )
      end do

      ob % jo_gpspw_tpw = 0.5 * ( count1 - trace1 )
      ob % trace_gpspw = trace1

      if ( ob % trace_gpspw < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_gpspw < 0 = ', ob % trace_gpspw

      ob % trace_total = ob % trace_total + ob % trace_gpspw
      ob % num_gpspw_tot = count1
   end if

   ob % num_sound_tot = 0
   ob % trace_sound   = 0
   if ( ob % num_sound > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0

      do n = 1, ob % num_sound
         do k = 1, ob % sound(n) % numlevs
            call da_calc_trace_single( ob % sound(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % sound(n) % v(k), count2, trace2 )
            call da_calc_trace_single( ob % sound(n) % t(k), count3, trace3 )
            call da_calc_trace_single( ob % sound(n) % q(k), count4, trace4 )
         end do
      end do
      
      ob % jo_sound_u = 0.5 * ( count1 - trace1 )
      ob % jo_sound_v = 0.5 * ( count2 - trace2 )
      ob % jo_sound_t = 0.5 * ( count3 - trace3 )
      ob % jo_sound_q = 0.5 * ( count4 - trace4 )
      ob % trace_sound = trace1 + trace2 + trace3 + trace4 

      if ( ob % trace_sound < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_sound < 0 = ', ob % trace_sound

      ob % trace_total = ob % trace_total + ob % trace_sound
      ob % num_sound_tot = count1 + count2 + count3 + count4 

   end if
   
   ob % num_airsr_tot = 0
   ob % trace_airsr   = 0
   if ( ob % num_airsr > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_airsr
         do k = 1, ob % airsr(n) % numlevs
            call da_calc_trace_single( ob % airsr(n) % t(k), count1, trace1 )
            call da_calc_trace_single( ob % airsr(n) % q(k), count2, trace2 )
         end do
      end do
      
      ob % jo_airsr_t = 0.5 * ( count1 - trace1 )
      ob % jo_airsr_q = 0.5 * ( count2 - trace2 )
      ob % trace_airsr = trace1 + trace2

      if ( ob % trace_airsr < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_airsr < 0 = ', ob % trace_airsr

      ob % trace_total = ob % trace_total + ob % trace_airsr
      ob % num_airsr_tot = count1 + count2 
   end if

   ob % num_airep_tot = 0
   ob % trace_airep   = 0
   if ( ob % num_airep > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0

      do n = 1, ob % num_airep
         do k = 1, ob % airep(n) % numlevs
            call da_calc_trace_single( ob % airep(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % airep(n) % v(k), count2, trace2 )
            call da_calc_trace_single( ob % airep(n) % t(k), count3, trace3 )
         end do
      end do
      
      ob % jo_airep_u = 0.5 * ( count1 - trace1 )
      ob % jo_airep_v = 0.5 * ( count2 - trace2 )
      ob % jo_airep_t = 0.5 * ( count3 - trace3 )
      ob % trace_airep   = trace1 + trace2 + trace3

      if ( ob % trace_airep < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_airep < 0 = ', ob % trace_airep

      ob % trace_total = ob % trace_total + ob % trace_airep
      ob % num_airep_tot = count1 + count2 + count3
   end if
   
   ob % num_pilot_tot = 0
   ob % trace_pilot   = 0
   if ( ob % num_pilot > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_pilot
         do k = 1, ob % pilot(n) % numlevs
            call da_calc_trace_single( ob % pilot(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % pilot(n) % v(k), count2, trace2 )
         end do
      end do

      ob % jo_pilot_u = 0.5 * ( count1 - trace1 )
      ob % jo_pilot_v = 0.5 * ( count2 - trace2 )
      ob % trace_pilot = trace1 + trace2

      if ( ob % trace_pilot < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_pilot < 0 = ', ob % trace_pilot

      ob % trace_total = ob % trace_total + ob % trace_pilot
      ob % num_pilot_tot = count1 + count2

   end if
   
   ob % num_ssmir_tot = 0
   ob % trace_ssmir   = 0
   if ( ob % num_ssmir > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1= 0; count2 = 0

      do n = 1, ob % num_ssmir
         call da_calc_trace_single( ob % ssmir(n) % speed, count1, trace1 )
         call da_calc_trace_single( ob % ssmir(n) % tpw,   count2, trace2 )
      end do
   
      ob % jo_ssmir_speed = 0.5 * ( count1 - trace1 )
      ob % jo_ssmir_tpw   = 0.5 * ( count2 - trace2 )
      ob % trace_ssmir    = trace1 + trace2

      if ( ob % trace_ssmir < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_ssmir < 0 = ', ob % trace_ssmir

      ob % trace_total    = ob % trace_total + ob % trace_ssmir
      ob % num_ssmir_tot  = count1 + count2
   end if
   
   ob % num_satem_tot = 0
   ob % trace_satem   = 0
   if ( ob % num_satem > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_satem
         do k = 1, ob % satem(n) % numlevs
           call da_calc_trace_single( ob % satem(n) % thickness(k), count1, trace1 )
         end do
      end do
   
      ob % jo_satem_thickness = 0.5 * ( count1 - trace1 )
      ob % trace_satem = trace1

      if ( ob % trace_satem< 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_satem < 0 = ', ob % trace_satem

      ob % trace_total = ob % trace_total + ob % trace_satem
      ob % num_satem_tot = count1
   end if
   
   ob % num_ssmt1_tot = 0
   ob % trace_ssmt1   = 0
   if ( ob % num_ssmt1 > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_ssmt1
         do k = 1, ob % ssmt1(n) % numlevs
            call da_calc_trace_single( ob % ssmt1(n) % t(k), count1, trace1 )
         end do
      end do
   
      ob % jo_ssmt1_t  = 0.5 * ( count1 - trace1 )
      ob % trace_ssmt1 = trace1

      if ( ob % trace_ssmt1< 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_ssmt1 < 0 = ', ob % trace_ssmt1

      ob % trace_total = ob % trace_total + ob % trace_ssmt1
      ob % num_ssmt1_tot = count1
   end if
   
   ob % num_ssmt2_tot = 0
   ob % trace_ssmt2   = 0
   if ( ob % num_ssmt2 > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_ssmt2
         do k = 1, ob % ssmt2(n) % numlevs
            call da_calc_trace_single( ob % ssmt2(n) % rh(k), count1, trace1 )
         end do
      end do
   
      ob % jo_ssmt2_rh = 0.5 * ( count1 - trace1 )
      ob % trace_ssmt2    = trace1

      if ( ob % trace_ssmt2< 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_ssmt2 < 0 = ', ob % trace_ssmt2

      ob % trace_total = ob % trace_total + ob % trace_ssmt2
      ob % num_ssmt2_tot = count1
   end if
   
   ob % num_buoy_tot = 0
   ob % trace_buoy   = 0
   if ( ob % num_buoy > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_buoy 
         call da_calc_trace_single( ob % buoy (n) % u, count1, trace1 )
         call da_calc_trace_single( ob % buoy (n) % v, count2, trace2 )
         call da_calc_trace_single( ob % buoy (n) % t, count3, trace3 )
         call da_calc_trace_single( ob % buoy (n) % p, count4, trace4 )
         call da_calc_trace_single( ob % buoy (n) % q, count5, trace5 )
      end do
      
      ob % jo_buoy_u = 0.5 * ( count1 - trace1 )
      ob % jo_buoy_v = 0.5 * ( count2 - trace2 )
      ob % jo_buoy_t = 0.5 * ( count3 - trace3 )
      ob % jo_buoy_p = 0.5 * ( count4 - trace4 )
      ob % jo_buoy_q = 0.5 * ( count5 - trace5 )
      ob % trace_buoy   = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_buoy < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_buoy < 0 = ', ob % trace_buoy

      ob % trace_total   = ob % trace_total + ob % trace_buoy
      ob % num_buoy_tot = count1 + count2 + count3 + count4 + count5
   end if
   ob % num_sonde_sfc_tot = 0
   ob % trace_sonde_sfc   = 0
   if ( ob % num_sonde_sfc > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0
 
      do n = 1, ob % num_sonde_sfc 
         call da_calc_trace_single( ob % sonde_sfc (n) % u, count1, trace1 )
         call da_calc_trace_single( ob % sonde_sfc (n) % v, count2, trace2 )
         call da_calc_trace_single( ob % sonde_sfc (n) % t, count3, trace3 )
         call da_calc_trace_single( ob % sonde_sfc (n) % p, count4, trace4 )
         call da_calc_trace_single( ob % sonde_sfc (n) % q, count5, trace5 )
      end do
      
      ob % jo_sonde_sfc_u = 0.5 * ( count1 - trace1 )
      ob % jo_sonde_sfc_v = 0.5 * ( count2 - trace2 )
      ob % jo_sonde_sfc_t = 0.5 * ( count3 - trace3 )
      ob % jo_sonde_sfc_p = 0.5 * ( count4 - trace4 )
      ob % jo_sonde_sfc_q = 0.5 * ( count5 - trace5 )
      ob % trace_sonde_sfc   = trace1 + trace2 + trace3 + trace4 + trace5
      if ( ob % trace_sonde_sfc < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_sonde_sfc < 0 = ', ob % trace_sonde_sfc
      
      ob % trace_total   = ob % trace_total + ob % trace_sonde_sfc
      ob % num_sonde_sfc_tot = count1 + count2 + count3 + count4 + count5
   end if
   ob % num_profiler_tot = 0
   ob % trace_profiler   = 0
   if ( ob % num_profiler > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_profiler
         do k = 1, ob % profiler(n) % numlevs
            call da_calc_trace_single( ob % profiler(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % profiler(n) % v(k), count2, trace2 )
         end do
      end do

      ob % jo_profiler_u = 0.5 * ( count1 - trace1 )
      ob % jo_profiler_v = 0.5 * ( count2 - trace2 )
      ob % trace_profiler = trace1 + trace2

      if ( ob % trace_profiler < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_profiler < 0 = ', ob % trace_profiler

      ob % trace_total = ob % trace_total + ob % trace_profiler
      ob % num_profiler_tot = count1 + count2
   end if

   ob % num_qscat_tot = 0
   ob % trace_qscat   = 0
   if ( ob % num_qscat > 0 ) then
      trace1 = 0.0; trace2 = 0.0
      count1 = 0; count2 = 0

      do n = 1, ob % num_qscat
         call da_calc_trace_single( ob % qscat(n) % u, count1, trace1 )
         call da_calc_trace_single( ob % qscat(n) % v, count2, trace2 )
      end do

      ob % jo_qscat_u = 0.5 * ( count1 - trace1 )
      ob % jo_qscat_v = 0.5 * ( count2 - trace2 )
      ob % trace_qscat = trace1 + trace2

      if ( ob % trace_qscat < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_qscat < 0 = ', ob % trace_qscat

      ob % trace_total = ob % trace_total + ob % trace_qscat
      ob % num_qscat_tot = count1 + count2

   end if

   ob % num_bogus_tot = 0
   ob % trace_bogus   = 0
   if ( ob % num_bogus > 0 ) then
      trace1 = 0.0; trace2 = 0.0; trace3 = 0.0; trace4 = 0.0; trace5 = 0.0
      count1 = 0; count2 = 0; count3 = 0; count4 = 0; count5 = 0

      do n = 1, ob % num_bogus
         do k = 1, ob % bogus(n) % numlevs
            call da_calc_trace_single( ob % bogus(n) % u(k), count1, trace1 )
            call da_calc_trace_single( ob % bogus(n) % v(k), count2, trace2 )
            call da_calc_trace_single( ob % bogus(n) % t(k), count3, trace3 )
            call da_calc_trace_single( ob % bogus(n) % q(k), count4, trace4 )
         end do
         call da_calc_trace_single( ob % bogus(n) % slp, count5, trace5 )
      end do

      ob % jo_bogus_u = 0.5 * ( count1 - trace1 )
      ob % jo_bogus_v = 0.5 * ( count2 - trace2 )
      ob % jo_bogus_t = 0.5 * ( count3 - trace3 )
      ob % jo_bogus_q = 0.5 * ( count4 - trace4 )
      ob % jo_bogus_slp = 0.5 * ( count5 - trace5 )
      ob % trace_bogus = trace1 + trace2 + trace3 + trace4 + trace5

      if ( ob % trace_bogus < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_bogus < 0 = ', ob % trace_bogus

      ob % trace_total = ob % trace_total + ob % trace_bogus
      ob % num_bogus_tot = count1 + count2 + count3 + count4 + count5
   end if

   ob % num_gpsref_tot = 0
   ob % trace_gpsref   = 0
   if ( ob % num_gpsref > 0 ) then
      trace1 = 0.0
      count1 = 0

      do n = 1, ob % num_gpsref
         do k = 1, ob % gpsref(n) % numlevs
            call da_calc_trace_single( ob % gpsref(n) % ref(k), count1, trace1 )
         end do
      end do

      ob % jo_gpsref_ref = 0.5 * ( count1 - trace1 )
      ob % trace_gpsref  = trace1 

      if ( ob % trace_gpsref < 0.0 ) &
         write(6,'(a,f15.5)') &
            ' Warning: ob % trace_gpsref < 0 = ', ob % trace_gpsref

      ob % trace_total = ob % trace_total + ob % trace_gpsref
      ob % num_gpsref_tot = count1 
   end if

   ob % total_obs = ob % num_synop_tot + ob % num_metar_tot + &
      ob % num_ships_tot + ob % num_polaramv_tot + ob % num_geoamv_tot + &
      ob % num_gpspw_tot + ob % num_sound_tot + ob % num_airep_tot + &
      ob % num_pilot_tot + ob % num_ssmir_tot + ob % num_satem_tot + &
      ob % num_ssmt1_tot + ob % num_ssmt2_tot + ob % num_buoy_tot + &
      ob % num_sonde_sfc_tot + ob % num_qscat_tot + ob % num_profiler_tot + &
      ob% num_bogus_tot + ob% num_gpsref_tot

   !  radiance part
   if ( rtminit_nsensor > 0 ) then

      do n = 1, rtminit_nsensor
         do ichan = 1, ob % rad(n) % nchan
            if ( ob % rad(n) % num_rad_tot(ichan) > 0 ) then
               trace1 = 0.0
               count1 = 0
               do k=1,ob % rad(n) % num_rad_tot(ichan)
                  call da_calc_trace_single( &
                     ob % rad(n) % tb(ichan)%pixel(k), count1, trace1 )
               end do
               ob % rad(n) % jo_rad(ichan)    = 0.5 * ( count1 - trace1 )
               ob % rad(n) % trace_rad(ichan) = trace1
               if ( ob % rad(n) % trace_rad(ichan) < 0.0 ) &
                  write(6,'(a,i3,a,f15.5)') &
                     ' Warning: '//trim(ob%rad(n)%rttovid_string), ichan, &
                     ' Trace(HK) < 0 = ', ob%rad(n)%trace_rad(ichan)
               ob % trace_total = ob % trace_total + ob%rad(n)%trace_rad(ichan)
               ob % total_obs   = ob % total_obs + ob%rad(n)%num_rad_tot(ichan)
            end if
         end do
      end do
   end if

end subroutine da_calc_jo_expected



subroutine da_calc_trace_single( field, count, trace )

   implicit none
   
   type (field_type), intent(in)   :: field
   integer, intent(inout)          :: count
   real, intent(inout)             :: trace
      
   if ( field % yp /= missing_r .and. field % y /= missing_r ) then
      count = count + 1
      trace = trace + ( field % yp  - field % y ) * field % pert / field % error
   end if
   
end subroutine da_calc_trace_single



subroutine da_read_jo_actual( ob )

   implicit none

   type (iv_type), intent(inout) :: ob
   
   character (len=46)            :: str
   character (len=15)            :: str1, str2, str3, str4, str5
   character (len=15)            :: str6, str7, str8, str9, str10
   character (len=5)             :: ob_name
   real                          :: dum1, dum2, dum3, dum4, dum5, jo
   integer                       :: n, num_obs

   ob % joa_synop_u = 0.0
   ob % joa_synop_v = 0.0
   ob % joa_synop_t = 0.0
   ob % joa_synop_p = 0.0
   ob % joa_synop_q = 0.0
   ob % joa_metar_u = 0.0
   ob % joa_metar_v = 0.0
   ob % joa_metar_t = 0.0
   ob % joa_metar_p = 0.0
   ob % joa_metar_q = 0.0
   ob % joa_ships_u = 0.0
   ob % joa_ships_v = 0.0
   ob % joa_ships_t = 0.0
   ob % joa_ships_p = 0.0
   ob % joa_ships_q = 0.0
   ob % joa_polaramv_u = 0.0
   ob % joa_polaramv_v = 0.0
   ob % joa_geoamv_u = 0.0
   ob % joa_geoamv_v = 0.0
   ob % joa_gpspw_tpw = 0.0
   ob % joa_sound_u = 0.0
   ob % joa_sound_v = 0.0
   ob % joa_sound_t = 0.0
   ob % joa_sound_q = 0.0
   ob % joa_airep_u = 0.0
   ob % joa_airep_v = 0.0
   ob % joa_airep_t = 0.0
   ob % joa_pilot_u = 0.0
   ob % joa_pilot_v = 0.0
   ob % joa_ssmir_speed = 0.0
   ob % joa_ssmir_tpw = 0.0
   ob % joa_satem_thickness = 0.0
   ob % joa_ssmt1_t = 0.0
   ob % joa_ssmt2_rh = 0.0
   ob % joa_buoy_u = 0.0
   ob % joa_buoy_v = 0.0
   ob % joa_buoy_t = 0.0
   ob % joa_buoy_p = 0.0
   ob % joa_buoy_q = 0.0
   ob % joa_sonde_sfc_u = 0.0
   ob % joa_sonde_sfc_v = 0.0
   ob % joa_sonde_sfc_t = 0.0
   ob % joa_sonde_sfc_p = 0.0
   ob % joa_sonde_sfc_q = 0.0
   ob % joa_qscat_u = 0.0
   ob % joa_qscat_v = 0.0
   ob % joa_profiler_u = 0.0
   ob % joa_profiler_v = 0.0
   ob % joa_bogus_u = 0.0
   ob % joa_bogus_v = 0.0
   ob % joa_bogus_t = 0.0
   ob % joa_bogus_q = 0.0
   ob % joa_bogus_slp = 0.0
   ob % joa_airsr_t = 0.0
   ob % joa_airsr_q = 0.0
   ob % joa_gpsref_ref = 0.0

   if ( rtminit_nsensor > 0 ) then   
      do n = 1,rtminit_nsensor
         ob%rad(n)%num_rad_tot(:) = 0
         ob%rad(n)%joa_rad(:) = 0.0
      end do
   end if

   rewind(jo_unit)

   do
      read(jo_unit,'(a46,10a15)')str, str1, str2, str3, str4, str5, &
                                 str6, str7, str8, str9, str10
      ob_name = adjustl(str)

      if ( ob_name == 'synop' .and. index(str,'actual') > 0 ) then
            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                     str6, str7, str8, str9, str10, &
                                     ob % joa_synop_u, ob % joa_synop_v, &
                                     ob % joa_synop_t, ob % joa_synop_p, &
                                     ob % joa_synop_q )

      else if ( ob_name == 'metar' .and. index(str,'actual') > 0 ) then
            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                     str6, str7, str8, str9, str10, &
                                     ob % joa_metar_u, ob % joa_metar_v, &
                                     ob % joa_metar_t, ob % joa_metar_p, &
                                     ob % joa_metar_q )

      else if ( ob_name == 'ships' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ships_u, ob % joa_ships_v, &
                                  ob % joa_ships_t, ob % joa_ships_p, &
                                  ob % joa_ships_q )

      else if ( ob_name == 'polar' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_polaramv_u, ob % joa_polaramv_v, &
                                  dum3, dum4, dum5 )

      else if ( ob_name == 'geoam' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_geoamv_u, ob % joa_geoamv_v, &
                                  dum3, dum4, dum5 )

      else if ( ob_name == 'gpspw' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_gpspw_tpw, dum2, dum3, dum4, dum5 )

      else if ( ob_name == 'sound' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
            str6, str7, str8, str9, str10, &
            ob % joa_sound_u, ob % joa_sound_v, ob % joa_sound_t, &
            ob % joa_sound_q, dum1 )

      else if ( ob_name == 'airsr' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_airsr_t, ob % joa_airsr_q,&
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'airep' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_airep_u, ob % joa_airep_v, &
                                  ob % joa_airep_t, dum1, dum2 )

      else if ( ob_name == 'pilot' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_pilot_u, ob % joa_pilot_v, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'ssmir' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmir_speed, ob % joa_ssmir_tpw, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'satem' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_satem_thickness, dum1, dum2, &
                                  dum3, dum4 )

      else if ( ob_name == 'ssmt1' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmt1_t, dum1, dum2, dum3, dum4 )

      else if ( ob_name == 'ssmt2' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_ssmt2_rh, dum1, dum2, dum3, dum4 )

      else if ( ob_name == 'buoy ' .and. index(str,'actual') > 0 ) then
            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                     str6, str7, str8, str9, str10, &
                                     ob % joa_buoy_u, ob % joa_buoy_v, &
                                     ob % joa_buoy_t, ob % joa_buoy_p, &
                                     ob % joa_buoy_q )
      else if ( ob_name == 'sonde' .and. index(str,'actual') > 0 ) then
            call da_read_jo_actual1( str1, str2, str3, str4, str5, &
               str6, str7, str8, str9, str10, &
               ob % joa_sonde_sfc_u, ob % joa_sonde_sfc_v, &
               ob % joa_sonde_sfc_t, ob % joa_sonde_sfc_p, &
               ob % joa_sonde_sfc_q )

      else if ( ob_name == 'profi' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_profiler_u, ob % joa_profiler_v, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'qscat' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_qscat_u, ob % joa_qscat_v, &
                                  dum1, dum2, dum3 )

      else if ( ob_name == 'bogus' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_bogus_u, ob % joa_bogus_v, &
                                  ob % joa_bogus_t, ob % joa_bogus_q, &
                                  ob % joa_bogus_slp)
      else if ( ob_name == 'gpsre' .and. index(str,'actual') > 0 ) then
         call da_read_jo_actual1( str1, str2, str3, str4, str5, &
                                  str6, str7, str8, str9, str10, &
                                  ob % joa_gpsref_ref, &
                                  dum1, dum2, dum3, dum4 )
      else if ( ob_name == 'radia' .and. index(str,'actual') > 0 ) then
         call get_sat_info(str(31:),platform_id,satellite_id,sensor_id)
         read (str1,'(i5,i10)')ichan, num_obs                     
         read (str2,'(f15.5)')jo                                 
	 do n = 1, rtminit_nsensor
            if (    platform_id  == rtminit_platform(n) &
                .and. satellite_id == rtminit_satid(n)    &
                .and. sensor_id    == rtminit_sensor(n)    ) then
               ob%rad(n)%joa_rad(ichan) = ob%rad(n)%joa_rad(ichan) + jo
               ob%rad(n)%num_rad_tot(ichan) = ob%rad(n)%num_rad_tot(ichan) + &
                  num_obs
               exit
            end if
         end do

      else if ( str(1:5) == '*****' ) then
         exit
      end if

   end do
               
end subroutine da_read_jo_actual



subroutine da_read_jo_actual1( str1, str2, str3, str4, str5, &
                               str6, str7, str8, str9, str10, &
                               j1, j2, j3, j4, j5 )
                            
   implicit none
         
   character (len=15), intent(in) :: str1, str2, str3, str4, str5
   character (len=15), intent(in) :: str6, str7, str8, str9, str10
   real, intent(inout)            :: j1, j2, j3, j4, j5
   
   real                           :: j1n, j2n, j3n, j4n, j5n
   real                           :: f1n, f2n, f3n, f4n, f5n

   read(str1,*)j1n
   read(str2,*)f1n
   read(str3,*)j2n
   read(str4,*)f2n
   read(str5,*)j3n
   read(str6,*)f3n
   read(str7,*)j4n
   read(str8,*)f4n
   read(str9,*)j5n
   read(str10,*)f5n

   !  Jo(actual) is scaled by the square of the input error factor used.

   j1 = j1 + f1n**2 * j1n
   j2 = j2 + f2n**2 * j2n
   j3 = j3 + f3n**2 * j3n
   j4 = j4 + f4n**2 * j4n
   j5 = j5 + f5n**2 * j5n

end subroutine da_read_jo_actual1



subroutine da_calc_new_factors( ob )

   implicit none

   type (iv_type), intent(inout)     :: ob
   integer                           :: n, ichan

   write(6,*)

   if ( ob % num_synop > 0 ) then
      call da_calc_new_factors1( 'synop', ob % num_synop, ob % num_synop_tot, &
         ob % jo_synop_u, ob % jo_synop_v, ob % jo_synop_t, ob % jo_synop_p, &
         ob % jo_synop_q, &
         ob % joa_synop_u, ob % joa_synop_v, ob % joa_synop_t, &
         ob % joa_synop_p, ob % joa_synop_q )
   end if

   if ( ob % num_metar > 0 ) then
      call da_calc_new_factors1( 'metar', ob % num_metar, ob % num_metar_tot, &
         ob % jo_metar_u, ob % jo_metar_v, ob % jo_metar_t, ob % jo_metar_p, &
         ob % jo_metar_q, &
         ob % joa_metar_u, ob % joa_metar_v, ob % joa_metar_t, &
         ob % joa_metar_p, ob % joa_metar_q )
   end if

   if ( ob % num_ships > 0 ) then
      call da_calc_new_factors1( 'ships', ob % num_ships, ob % num_ships_tot, &
         ob % jo_ships_u, ob % jo_ships_v, ob % jo_ships_t, ob % jo_ships_p, &
         ob % jo_ships_q, &
         ob % joa_ships_u, ob % joa_ships_v, ob % joa_ships_t, &
         ob % joa_ships_p, ob % joa_ships_q )
   end if

   if ( ob % num_geoamv > 0 ) then
      call da_calc_new_factors1( 'geoamv', ob % num_geoamv, &
         ob % num_geoamv_tot, &
         ob % jo_geoamv_u, ob % jo_geoamv_v, 0.0, 0.0, 0.0, &
         ob % joa_geoamv_u, ob % joa_geoamv_v, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_polaramv > 0 ) then
      call da_calc_new_factors1( 'polaramv', ob % num_polaramv, &
         ob % num_polaramv_tot, &
         ob % jo_polaramv_u, ob % jo_polaramv_v, 0.0, 0.0, 0.0, &
         ob % joa_polaramv_u, ob % joa_polaramv_v, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_gpspw > 0 ) then
      call da_calc_new_factors1( 'gpspw', ob % num_gpspw, ob % num_gpspw_tot, &
                            ob % jo_gpspw_tpw, 0.0, 0.0, 0.0, 0.0, &
                            ob % joa_gpspw_tpw, 0.0, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_sound > 0 ) then
      call da_calc_new_factors1( 'sound', ob % num_sound, ob % num_sound_tot, &
         ob % jo_sound_u, ob % jo_sound_v, ob % jo_sound_t, ob % jo_sound_q, &
         0.0, &
         ob % joa_sound_u, ob % joa_sound_v, ob % joa_sound_t, &
         ob % joa_sound_q, 0.0)
   end if
   
   if ( ob % num_airep > 0 ) then
      call da_calc_new_factors1( 'airep', ob % num_airep, ob % num_airep_tot, &
         ob % jo_airep_u, ob % jo_airep_v, ob % jo_airep_t, 0.0, 0.0, &
         ob % joa_airep_u, ob % joa_airep_v, ob % joa_airep_t, 0.0, 0.0 )
   end if
   
   if ( ob % num_airsr > 0 ) then
      call da_calc_new_factors1( 'airsr', ob % num_airsr, ob % num_airsr_tot, &
         ob % jo_airsr_t, ob % jo_airsr_q, 0.0, 0.0, 0.0, &
         ob % joa_airsr_t, ob % joa_airsr_q, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_pilot > 0 ) then
      call da_calc_new_factors1( 'pilot', ob % num_pilot, ob % num_pilot_tot, &
         ob % jo_pilot_u, ob % jo_pilot_v, 0.0, 0.0, 0.0, &
         ob % joa_pilot_u, ob % joa_pilot_v, 0.0, 0.0, 0.0 )
   end if
   
   if ( ob % num_ssmir > 0 ) then
      call da_calc_new_factors1( 'ssmir', ob % num_ssmir, ob % num_ssmir_tot, &
         ob % jo_ssmir_speed, ob % jo_ssmir_tpw, 0.0, 0.0, 0.0, &
         ob % joa_ssmir_speed, ob % joa_ssmir_tpw, 0.0, 0.0, 0.0 )
   end if
   
   if ( ob % num_satem > 0 ) then
      call da_calc_new_factors1( 'satem', ob % num_satem, ob % num_satem_tot, &
         ob % jo_satem_thickness, 0.0, 0.0, 0.0, 0.0, &
         ob % joa_satem_thickness, 0.0, 0.0, 0.0, 0.0 )
   end if
   
   if ( ob % num_ssmt1 > 0 ) then
      call da_calc_new_factors1( 'ssmt1', ob % num_ssmt1, ob % num_ssmt1_tot, &
         ob % jo_ssmt1_t, 0.0, 0.0, 0.0, 0.0, &
         ob % joa_ssmt1_t, 0.0, 0.0, 0.0, 0.0 )
   end if
   
   if ( ob % num_ssmt2 > 0 ) then
      call da_calc_new_factors1( 'ssmt2', ob % num_ssmt2, ob % num_ssmt2_tot, &
         ob % jo_ssmt2_rh, 0.0, 0.0, 0.0, 0.0, &
         ob % joa_ssmt2_rh, 0.0, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_buoy > 0 ) then
      call da_calc_new_factors1( 'buoy ', ob % num_buoy, ob % num_buoy_tot, &
         ob % jo_buoy_u, ob % jo_buoy_v, ob % jo_buoy_t, ob % jo_buoy_p, &
         ob % jo_buoy_q, &
         ob % joa_buoy_u, ob % joa_buoy_v, ob % joa_buoy_t, ob % joa_buoy_p, &
         ob % joa_buoy_q )
   end if

   if ( ob % num_sonde_sfc > 0 ) then
      call da_calc_new_factors1( 'sonde', ob % num_sonde_sfc, &
         ob % num_sonde_sfc_tot, &
         ob % jo_sonde_sfc_u, ob % jo_sonde_sfc_v, ob % jo_sonde_sfc_t, &
         ob % jo_sonde_sfc_p, ob % jo_sonde_sfc_q, &
         ob % joa_sonde_sfc_u, ob % joa_sonde_sfc_v, ob % joa_sonde_sfc_t, &
         ob % joa_sonde_sfc_p, ob % joa_sonde_sfc_q )
   end if

   if ( ob % num_profiler > 0 ) then
      call da_calc_new_factors1( 'profi', ob % num_profiler, &
         ob % num_profiler_tot, &
         ob % jo_profiler_u, ob % jo_profiler_v, 0.0, 0.0, 0.0, &
         ob % joa_profiler_u, ob % joa_profiler_v, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_qscat > 0 ) then
      call da_calc_new_factors1( 'qscat', ob % num_qscat, ob % num_qscat_tot, &
         ob % jo_qscat_u, ob % jo_qscat_v, 0.0, 0.0, 0.0, &
         ob % joa_qscat_u, ob % joa_qscat_v, 0.0, 0.0, 0.0 )
   end if

   if ( ob % num_bogus > 0 ) then
      call da_calc_new_factors1( 'bogus', ob % num_bogus, ob % num_bogus_tot, &
         ob % jo_bogus_u, ob % jo_bogus_v, ob % jo_bogus_t, ob % jo_bogus_q, &
         ob % jo_bogus_slp, &
         ob % joa_bogus_u, ob % joa_bogus_v, ob % joa_bogus_t, &
         ob % joa_bogus_q, ob % joa_bogus_slp )
   end if

   if ( ob % num_gpsref > 0 ) then
      call da_calc_new_factors1( 'gpsre', ob % num_gpsref, ob % num_gpsref_tot, &
         ob % jo_gpsref_ref,   0.0, 0.0, 0.0, 0.0, &
         ob % joa_gpsref_ref, 0.0, 0.0, 0.0, 0.0 )
   end if
  
   if ( rtminit_nsensor > 0 ) then 
      write(6,*) &
         '     sensor        chan   num       Jo_mini         Jo_exp       trace(HK)  factor'
      do n = 1, rtminit_nsensor
         do ichan = 1, ob % rad(n) % nchan
            if ( ob % rad(n) % num_rad_tot(ichan) > 0 ) then
               ob % rad(n) % factor_rad(ichan) = &
                  sqrt( ob % rad(n) % joa_rad(ichan) / ob % rad(n) % jo_rad(ichan) )
               write(6,'(a15,i8,i8,3f15.5,f8.3)')   &
                  trim(ob%rad(n)%rttovid_string), &
                  ichan,                        &
                  ob%rad(n)%num_rad_tot(ichan), &
                  ob%rad(n)%joa_rad(ichan),      &
                  ob%rad(n)%jo_rad(ichan),     &
                  ob%rad(n)%trace_rad(ichan),   &
                  ob%rad(n)%factor_rad(ichan)
            end if
         end do
      end do
   end if

end subroutine da_calc_new_factors



subroutine da_calc_new_factors1( ob_name, ob_num, ob_num_tot, &
                                 j1e, j2e, j3e, j4e, j5e, &
                                 j1a, j2a, j3a, j4a, j5a )
                             
   implicit none
         
   character (len=5), intent(in)  :: ob_name
   integer, intent(in)            :: ob_num, ob_num_tot
   real, intent(in)               :: j1e, j2e, j3e, j4e, j5e
   real, intent(in)               :: j1a, j2a, j3a, j4a, j5a

   real                           :: f1, f2, f3, f4, f5
         
   f1 = 1.0; f2 = 1.0; f3 = 1.0; f4 = 1.0; f5 = 1.0
                             
   if ( j1e > 0.0 ) f1 = sqrt( j1a / j1e )
   if ( j2e > 0.0 ) f2 = sqrt( j2a / j2e )
   if ( j3e > 0.0 ) f3 = sqrt( j3a / j3e )
   if ( j4e > 0.0 ) f4 = sqrt( j4a / j4e )
   if ( j5e > 0.0 ) f5 = sqrt( j5a / j5e )
   
   write(6,'(1x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Jo (expected)= ', &
                                   ob_num, ob_num_tot, &
                                   j1e, j2e, j3e, j4e, j5e

   write(6,'(1x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Jo (actual)  = ', &
                                   ob_num, ob_num_tot, &
                                   j1a, j2a, j3a, j4a, j5a

   write(6,'(1x,a5,a21,2i8,6f15.5)')ob_name, ' obs, Error Factor = ', &
                                   ob_num, ob_num_tot, f1, f2, f3, f4, f5
   write(6,*)
   
end subroutine da_calc_new_factors1



subroutine da_get_j( ob )

   implicit none

   type (iv_type), intent(in)    :: ob
   
   character (len=80)            :: str
   integer                       :: icount
   real                          :: j, jo, jb, jn, jon, jbn, j_e, jo_e, jb_e
   real                          :: jb_factor_old, jb_factor_oldn, jb_factor_new

   rewind(in_unit)

   j = 0.0; jo = 0.0; jb = 0.0; j_e = 0.0; jo_e = 0.0; jb_e = 0.0
   icount = 0 
   jb_factor_old = 0
   jb_factor_new = 0
     
   do
      read(in_unit,'(a80)')str     
      if ( index(str,'Final value of J ') > 0 ) then
         read(str(index(str,'=')+1:80),*)jn
         j = j + jn
      else if ( index(str,'Final value of Jo') > 0 ) then
         read(str(index(str,'=')+1:80),*)jon
         jo = jo + jon
      else if ( index(str,'Final value of Jb') > 0 ) then
         read(str(index(str,'=')+1:80),*)jbn
         jb = jb + jbn
      else if ( index(str,'Jb factor used(1)') > 0 ) then
         read(str(index(str,'=')+1:80),*)jb_factor_oldn
         jb_factor_old = jb_factor_old + jb_factor_oldn
         icount = icount + 1
      else if ( str(1:5) == '*****' ) then
         exit
      end if     
   end do

   jb_factor_old = jb_factor_old / real(icount)

   write(6,'(/a,i8)')    ' Total number of obs.    = ', ob % total_obs
   write(6,'(/a,i8)')    ' Total number of cases   = ', icount
   j_e  = 0.5 * ob % total_obs
   jo_e = 0.5 * ( ob % total_obs -  ob % trace_total )
   jb_e = 0.5 * ob % trace_total

   write(6,'(a,f15.5)')' Total J (actual)        = ', j
   write(6,'(a,f15.5)')' Total J (expected)      = ', j_e
   write(6,*)

   write(6,'(a,f15.5)')' Total Jo(actual)        = ', jo
   write(6,'(a,f15.5)')' Total Jo(expected)      = ', jo_e
   write(6,'(a,f15.5)')' Total Jo factor         = ', sqrt(jo/jo_e)

   write(6,*)
   write(6,'(a,f15.5)')' Total Jb(actual)        = ', jb
   write(6,'(a,f15.5)')' Total Jb(expected)      = ', jb_e
   write(6,'(a,f15.5)')' Total Jb factor (old)   = ', jb_factor_old

   if ( jb_e < 0.0 ) then
      write(6,'(a)')' Warning: Tr(HK) < 0.0 Too small a sample?'
      stop
   end if
   jb_factor_new = sqrt(jb/jb_e) * jb_factor_old
   write(6,'(a,f15.5)')' Total Jb factor (new)   = ', jb_factor_new
   write(6,*)
   
end subroutine da_get_j


subroutine read_namelist_radiance

   implicit none

   character(len=filename_len) :: namelist_file      ! Input namelist filename.
   integer, parameter        :: namelist_unit = 7  ! Input namelist unit.
   integer                   :: iost               ! Error code.

   namelist /rtminit/  rtminit_nsensor, rtminit_platform, &
                       rtminit_satid, rtminit_sensor, rtminit_nchan

   namelist_file = 'namelist.radiance'
   write (6, '(3x,a,a)' ) ' radiance namelist file : ', namelist_file
   iost = 0

   open ( file   = trim(namelist_file), unit   = namelist_unit, &
          status = 'old' , access = 'sequential', &
          form   = 'formatted', action = 'read', &
          iostat = iost )
   if ( iost /= 0 ) stop ' error in opening namelist file '

   iost = 0

   read  ( unit = namelist_unit, nml = rtminit,  iostat = iost)
      write(6,'(a,i4  )') ' rtminit_nsensor  = ', rtminit_nsensor
      write(6,'(a,10i4)') ' rtminit_platform = ', rtminit_platform(1:rtminit_nsensor)
      write(6,'(a,10i4)') ' rtminit_satid    = ', rtminit_satid   (1:rtminit_nsensor)
      write(6,'(a,10i4)') ' rtminit_sensor   = ', rtminit_sensor  (1:rtminit_nsensor)
      write(6,'(a,10i4)') ' rtminit_nchan    = ', rtminit_nchan   (1:rtminit_nsensor)

   if ( iost /= 0 ) stop ' error in reading namelist file '

   close (namelist_unit)

   if ( rtminit_nsensor > 0 ) then
      allocate ( ob % rad(rtminit_nsensor) )
      do n = 1,rtminit_nsensor
         ob % rad(n) % nchan = rtminit_nchan(n)
         allocate ( ob % rad(n) % num_rad_tot(ob % rad(n) % nchan) )
         allocate ( ob % rad(n) % jo_rad(ob % rad(n) % nchan) )
         allocate ( ob % rad(n) % joa_rad(ob % rad(n) % nchan) )
         allocate ( ob % rad(n) % factor_rad(ob % rad(n) % nchan) )
         allocate ( ob % rad(n) % trace_rad(ob % rad(n) % nchan) )
         allocate ( ob % rad(n) % tb(ob % rad(n) % nchan) )
         ob % rad(n) % num_rad_tot(:) = 0
	 if (rtminit_satid(n) < 10) then	 
            write(ob%rad(n)%rttovid_string, '(a,i1,a)')  &
              trim( platform_name(rtminit_platform(n)) )//'-',  &
               rtminit_satid(n),     &
               '-'//trim( inst_name(rtminit_sensor(n)) )
	 else
            write(ob%rad(n)%rttovid_string, '(a,i2.2,a)')  &
              trim( platform_name(rtminit_platform(n)) )//'-',  &
               rtminit_satid(n),     &
               '-'//trim( inst_name(rtminit_sensor(n)) )
	 end if   
         write(6,*) 'Tuning radiance error for ', trim(ob%rad(n)%rttovid_string)
      end do
   end if

end subroutine read_namelist_radiance

subroutine get_sat_info(ob_name,platform_id,satellite_id,sensor_id,ichan)

   implicit none

   character(len=*),  intent(in)  :: ob_name
   integer,           intent(out) :: platform_id, satellite_id, sensor_id
   integer, optional, intent(out) :: ichan
   character(len=5)               :: platform, sensor
   integer                        :: strlen, dashloc1, dashloc2, dashloc3

   strlen = len_trim(ob_name)

   ! retrieve platform name
   dashloc1 = index(ob_name,"-")
   platform = ob_name(1:dashloc1-1)
   select case (platform)
   case ('noaa')
      platform_id = 1
   case ('eos')
      platform_id = 9
   case ('dmsp')
      platform_id = 2
   case ('metop')
      platform_id = 10
   case default
      write(0,*)' Unrecognized platform ',platform
      stop
   end select

   ! retrieve satellite id
   dashloc2 = index(ob_name(dashloc1+1:strlen),"-")
   read(ob_name(dashloc1+1:dashloc1+dashloc2-1),'(i3)') satellite_id

   ! retrieve sensor name
   if ( present(ichan) ) then
      dashloc3 = index(ob_name,"-",.true.)
      sensor = ob_name(dashloc1+dashloc2+1:dashloc3-1)
   else
      sensor = ob_name(dashloc1+dashloc2+1:)
   end if
   select case (sensor)
   case ('amsua')
      sensor_id = 3
   case ('amsub')
      sensor_id = 4
   case ('mhs')
      sensor_id = 15
   case ('airs')
      sensor_id = 11
   case ('ssmis')
      sensor_id = 10
   case default
      write(0,*)' Unrecognized sensor ',sensor
      stop
   end select

   if ( present(ichan) ) then
      ! retrieve channel index
      read(ob_name(dashloc3+1:),'(i4)') ichan
   end if

end subroutine get_sat_info

end program da_tune_obs_desroziers

