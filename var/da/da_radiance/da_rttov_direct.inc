#ifdef RTTOV
subroutine da_rttov_direct(inst, isflg, nchanl, nprofiles, &
!                          nfrequencies, nchannels, nbtout , &
                          con_vars, aux_vars, &
!                          tb, calcemis_out, emisv, emish, emissivity_out)
!!!                          tb, emisv, emish, emissivity_out)
                          tb, rad_xb, rad_ovc, emisv, emish, emissivity_out)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the forward subroutine of RTTOV
   !---------------------------------------------------------------------------





#ifdef RTTOV
    Use parkind1, Only : jpim    ,jprb
#endif

   implicit none

#include "rttov_direct.interface"

   integer             ,  intent (in) :: inst, isflg, nchanl, nprofiles
!   integer             ,  intent (in) :: nfrequencies, nchannels, nbtout
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
!   real                , intent (out) :: tb(nprofiles, nchanl)
!   real                , intent (inout) :: tb(nchanl,nprofiles)
   real                , intent (inout) :: tb(nchanl)
   real                , intent (inout) :: rad_xb(nchanl)
   real                , intent (inout) :: rad_ovc(nchanl,kts:kte)
!   logical             , intent (out) :: calcemis_out(nprofiles*nchanl)
   real             ,  intent    (in) :: emisv(nprofiles*nchanl)
   real             ,  intent    (in) :: emish(nprofiles*nchanl)
   real             ,  intent   (out) :: emissivity_out(:)

   ! local variables
   integer             :: nfrequencies, nchannels, nbtout
   integer             :: n, nc, i, ichannel, pol_id, k
 ! Integer             :: alloc_status(40)
   Integer             :: alloc_status(140)                            !RTTOV9_3
   Integer(Kind=jpim)  :: nstreams  !RTTOV9_3
   
  
   ! RTTOV input parameters
   ! integer             :: nfrequencies, nchannels, nbtout
   integer             :: nchan(nprofiles)
   integer , pointer   :: lprofiles(:)
   type(rttov_coef)    :: coef
   type(rttov_coef_scatt_ir) :: coef_scatt_ir  !RTTOV9_3
   type(rttov_optpar_ir) :: optp  !RTTOV9_3
   type(profile_type)  :: profiles(nprofiles) 
   logical             :: addcloud, addinterp, lsun, laerosl
   real*8, allocatable    :: surfem(:)
   integer , pointer   :: channels (:), polarisations(:,:)
   logical, allocatable :: calcemis (:)

   ! RTTOV out parameters
   integer             :: errorstatus(nprofiles)

   ! RTTOV inout parameters
   real*8  , pointer        :: emissivity (:)
   type (radiance_type)     :: radiance
   type (transmission_type) :: transmission


   call da_trace_entry("da_rttov_direct")

   nchan (:) = nchanl 
   coef = coefs(inst)
   coef_scatt_ir = coefs_scatt_ir(inst)  !RTTOV9_3
   optp = optps(inst)
   addcloud = .false.
   addinterp = .false.
   lsun = .false.
   laerosl = .false.
 
   alloc_status (:) = 0

   do n = 1, nprofiles
      profiles(n) % nlevels    = con_vars(n) % nlevels
      allocate (profiles(n)%p(profiles(n) % nlevels), stat=alloc_status(1))
      allocate (profiles(n)%t(profiles(n) % nlevels), stat=alloc_status(2))
      allocate (profiles(n)%q(profiles(n) % nlevels), stat=alloc_status(3))
      allocate (profiles(n)%o3(profiles(n) % nlevels), stat=alloc_status(4))
      allocate (profiles(n)%co2(profiles(n) % nlevels), stat=alloc_status(5))
      allocate (profiles(n)%clw(profiles(n) % nlevels), stat=alloc_status(6))
!RTTOV9_3
      allocate (profiles(n)%n2o(profiles(n) % nlevels), stat=alloc_status(41))
      allocate (profiles(n)%co(profiles(n) % nlevels), stat=alloc_status(42))
      allocate (profiles(n)%ch4(profiles(n) % nlevels),stat=alloc_status(43))
      allocate (profiles(n)%aerosols(11,profiles(n) % nlevels),stat=alloc_status(44))
      allocate (profiles(n)%cloud(6,profiles(n) % nlevels),stat=alloc_status(45))
      allocate (profiles(n)%cfrac(6,profiles(n) % nlevels),stat=alloc_status(46))
!end of RTTOV9_3

      if (any(alloc_status /= 0)) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to profile",n
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      profiles(n) % ozone_data = .false.
      profiles(n) % co2_data   = .false.
      profiles(n) % clw_data   = .false. 
!RTTOV9_3
      profiles(n) % n2o_data   = .false.
      profiles(n) % co_data    = .false.
      profiles(n) % ch4_data   = .false.
      profiles(n) % aer_data   = .false.
      profiles(n) % cld_data   = .false.
!end of RTTOV9_3
      profiles(n) % p(:)       = coef%ref_prfl_p(:)
      profiles(n) % t(:)       = con_vars(n)%t(:)
      profiles(n) % q(:)       = con_vars(n)%q(:)
      profiles(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)
!RTTOV9_3
      profiles(n) % n2o(:)     = 0.0
      profiles(n) % co(:)      = 0.0
      profiles(n) % ch4(:)     = 0.0
      profiles(n) % aerosols(:,:)   = 0.0
      profiles(n) % cloud(:,:)      = 0.0
      profiles(n) % cfrac(:,:)      = 0.0
      profiles(n) % idg             = 1
      profiles(n) % ish             = 1
!end of RTTOV9_3    
      !if seap > 0.99 watertype is ocean water                          RTTOV9_3
      profiles(n) % skin % watertype = 1

      if (isflg == 0 .or. isflg == 4) then  ! sea
         !if seap > 0.99 watertype is ocean water                          RTTOV9_3
         if (isflg == 0) profiles(n) % skin % watertype = 1
         !if seap < 0.99 watertype is freshwater
         if (isflg == 4) profiles(n) % skin % watertype = 0           !     end of RTTOV9_3

         profiles(n) % skin % surftype   = 1 ! aux_vars (n) % surftype
         ! profiles(n) % skin % fastem (:) = 0.0
      else if (isflg == 1 .or. isflg == 5) then  ! sea-ice with snow
         profiles(n) % skin % surftype   = 2
         ! profiles(n) % skin % fastem (1) = 2.2
         ! profiles(n) % skin % fastem (2) = 3.7
         ! profiles(n) % skin % fastem (3) = 122.0
         ! profiles(n) % skin % fastem (4) = 0.0
         ! profiles(n) % skin % fastem (5) = 0.15
      else
         profiles(n) % skin % surftype   = 0 ! land (Deep dry snow)
         !    profiles(n) % skin % fastem (1) = 3.0
         !    profiles(n) % skin % fastem (2) = 24.0
         !    profiles(n) % skin % fastem (3) = 60.0
         !    profiles(n) % skin % fastem (4) = 0.1
         !    profiles(n) % skin % fastem (5) = 0.15
      end if
      !    profiles(n) % skin % surftype   = aux_vars (n) % surftype   
      profiles(n) % skin % t          = aux_vars (n) % surft    
      profiles(n) % skin % fastem (:) = 0.0 ! aux_vars (n) % fastem (:)

      profiles(n) % s2m  % t    = aux_vars (n) % t2m
      profiles(n) % s2m  % q    = aux_vars (n) % q2m
      profiles(n) % s2m  % o    = 0.0 !aux_vars (n) % o3
      profiles(n) % s2m  % p    = con_vars (n) % ps
      profiles(n) % s2m  % u    = aux_vars (n) % u10
      profiles(n) % s2m  % v    = aux_vars (n) % v10
      profiles(n) % s2m  % wfetc  =   100000.0              !RTTOV9_3

      profiles(n) % zenangle    = aux_vars (n) % satzen
      profiles(n) % azangle     = aux_vars (n) % satazi
!RTTOV9_3

      profiles(n) % sunzenangle = aux_vars (n) % solzen     !50.0                      !RTTOV9_3
      profiles(n) % sunazangle  = aux_vars (n) % solazi     !86.0                      !RTTOV9_3
      profiles(n) % elevation = 0.001* aux_vars (n) % elevation   !800
      profiles(n) % latitude    = aux_vars (n) % rlat       !35.0
 

      profiles(n) % addsolar    = .false.
      profiles(n) % addrefrac   = .false.
      profiles(n) % addaerosl   = .false.
      profiles(n) % addwatrcl   = .false.
      profiles(n) % Be          = 0.35
      profiles(n) % cosbk       = 0.0
!end for RTTOV9_3
      profiles(n) % ctp         = 500.0
      profiles(n) % cfraction   = 0.0
   end do

!#ifdef RTTOV


 call da_rttov_setupchan(nprofiles, nchan, coef, &             ! in
                   nfrequencies, nchannels, nbtout)       ! out
   nchannels = nfrequencies           !RTTOV9_3
!#endif


   allocate (lprofiles(nfrequencies), stat = alloc_status(31))
   allocate (channels (nfrequencies), stat = alloc_status(32))
   allocate (polarisations(nchannels, 3), stat = alloc_status(33))
   allocate (emissivity(nchannels), stat = alloc_status(34))
   allocate (calcemis(nchannels), stat = alloc_status(35))
   allocate (surfem(nchannels), stat = alloc_status(36))

   emissivity = 0.0
  
   nstreams = 0_jpim  !RTTOV9_3
   ! allocate transmittance structure
   allocate (transmission % tau_surf      (0 : nstreams,nchannels)        ,stat= alloc_status(8))
   allocate (transmission % tau_layer     (coef % nlevels, 0 :  nstreams, nchannels) ,    stat= alloc_status(9))
   allocate (transmission % od_singlelayer(coef % nlevels, 0 :  nstreams, nchannels ),  stat= alloc_status(10))
!RTTOV9_3
   allocate (transmission % tau_total(nchannels),  stat = alloc_status(51))
   allocate (transmission % tau_layers(coef % nlevels, nchannels), stat = alloc_status(52))
   allocate (transmission % od_layer (coef % nlevels,0 : nstreams, nchannels), stat = alloc_status(53))
  ! allocate (transmission % od_layer(coef % nlevels, 0 :  nstreams, nchannels ),  stat= alloc_status(11))

   allocate (transmission % odsun_singlelayer(coef % nlevels, 0 : nstreams, nchannels), stat = alloc_status(54))
   allocate (transmission % tausun_surf ( 0 : nstreams, nchannels),stat = alloc_status(55)) 
   allocate (transmission % tausun_layer( coef % nlevels,0 :  nstreams, nchannels), stat = alloc_status(56))
   allocate (transmission % od_sfrac( 0 : nstreams, nchannels), stat = alloc_status(57))
   allocate (transmission % odsun_sfrac( 0 : nstreams, nchannels), stat = alloc_status(58))
   allocate (transmission % od_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(59))
   allocate (transmission % odsun_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(60))
   allocate (transmission % tau_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(61))
   allocate (transmission % tau_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(62))
   allocate (transmission % tau_ref_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(63))
   allocate (transmission % tau_ref_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(64))
   allocate (transmission % od_frac_t( 0 : nstreams, nchannels), stat = alloc_status(65))  
   allocate (transmission % odsun_frac_t( 0 : nstreams, nchannels), stat = alloc_status(66))   
   allocate (transmission % tau_surf_t( 0 : nstreams, nchannels), stat = alloc_status(67))
   allocate (transmission % tausun_surf_t( 0 : nstreams, nchannels), stat = alloc_status(68))
   allocate (transmission % tau_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(69))
   allocate (transmission % tausun_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(70))

!end of RTTOV9_3
   transmission % tau_surf       = 0.0
   transmission % tau_layer      = 0.0
   transmission % od_singlelayer = 0.0
!RTTOV9_3
   transmission % tau_total         = 0.0
   transmission % tau_layers        = 0.0
   transmission % od_layer          = 0.0
   transmission % odsun_singlelayer = 0.0
   transmission % tausun_surf       = 0.0
   transmission % tausun_layer      = 0.0
   transmission % od_sfrac          = 0.0
   transmission % odsun_sfrac       = 0.0
   transmission % od_frac_ac        = 0.0
   transmission % odsun_frac_ac     = 0.0
   transmission % tau_surf_ac       = 0.0
   transmission % tau_surf_acsun    = 0.0
   transmission % tau_ref_surf_ac   = 0.0
   transmission % tau_ref_surf_acsun = 0.0
   transmission % od_frac_t         = 0.0
   transmission % odsun_frac_t      = 0.0
   transmission % tau_surf_t        = 0.0
   transmission % tausun_surf_t     = 0.0
   transmission % tau_ref_surf_t    = 0.0
   transmission % tausun_ref_surf_t = 0.0
!end of RTTOV9_3
   ! allocate radiance results arrays with number of channels
   allocate (radiance % clear    (nchannels) ,stat= alloc_status(11))
   allocate (radiance % cloudy   (nchannels) ,stat= alloc_status(12))
   allocate (radiance % total    (nchannels) ,stat= alloc_status(13))
   allocate (radiance % bt       (nchannels) ,stat= alloc_status(14))
   allocate (radiance % bt_clear (nchannels) ,stat= alloc_status(15))
   allocate (radiance % upclear  (nchannels) ,stat= alloc_status(16))
   allocate (radiance % dnclear  (nchannels) ,stat= alloc_status(17))
   allocate (radiance % reflclear(nchannels) ,stat= alloc_status(18))
   allocate (radiance % overcast (coef % nlevels, nchannels) ,stat= alloc_status(19))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance % downcld  (coef % nlevels, nchannels) ,stat= alloc_status(20))

   allocate (radiance % out      (nbtout) ,stat= alloc_status(21))
   allocate (radiance % out_clear(nbtout) ,stat= alloc_status(22))
   allocate (radiance % total_out(nbtout) ,stat= alloc_status(23))
   allocate (radiance % clear_out(nbtout) ,stat= alloc_status(24))
!RTTOV9_3
  allocate (radiance % up(coef % nlevels, nchannels) ,stat= alloc_status(81))
  allocate (radiance % down(coef % nlevels, nchannels) ,stat= alloc_status(82))
  allocate (radiance % surf(coef % nlevels, nchannels) ,stat= alloc_status(83))
!end of RTTOV9_3
   radiance % clear     = 0.0
   radiance % cloudy    = 0.0
   radiance % total     = 0.0
   radiance % bt        = 0.0
   radiance % bt_clear  = 0.0
   radiance % upclear   = 0.0
   radiance % dnclear   = 0.0
   radiance % reflclear = 0.0
   radiance % overcast  = 0.0
   radiance % downcld   = 0.0
   radiance % out       = 0.0
   radiance % out_clear = 0.0
   radiance % total_out = 0.0
   radiance % clear_out = 0.0
!RTTOV9_3
   radiance % up        = 0.0
   radiance % down      = 0.0
   radiance % surf      = 0.0
   radiance % lcloud    = .false.
!end of RTTOV9_3
   if (any(alloc_status /= 0)) then
      call da_error(__FILE__,__LINE__, &
         (/"mem allocation error prior to rttov_direct"/))
   end if

   surfem (:) = 0.0
!#ifdef RTTOV
   call da_rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in
                nchannels, nbtout, coef, surfem,  &          ! in
                lprofiles, channels, polarisations,     &    ! out
                emissivity )                                ! out                       
!#endif


   !  surface emissivity scheme
   !----------------------------------------------------------

   !  For Infrared sensors
   !-----------------------------
   if (coef%id_sensor == 1 .or. coef%id_sensor == 3) then 
      if (profiles(1) % skin % surftype == 1) then  
         calcemis (1:nchannels)   = .true.           ! using ISSEM over sea
         emissivity (1:nchannels) = 0.0             
      else if (profiles(1) % skin % surftype == 2) then
         calcemis (1:nchannels)   = .false.          
         emissivity (1:nchannels) = 0.98             ! over sea-ice
      else                                        
         if (isflg == 2 .or. isflg == 6) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 0.97           ! land without snow
         end if
         if (isflg == 3 .or. isflg == 7) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 1.0            ! land with snow
         end if
      end if
   end if

   !  For Microwave sensors
   !-----------------------------
   if (coef%id_sensor == 2) then
      !
      !  1.0 over sea
      !
      if (profiles(1) % skin % surftype == 1) then 
         if (mw_emis_sea == 0) then
            calcemis  (1:nchannels) = .true.
            emissivity(1:nchannels) = -1.0   ! RTTOV fastem-2
         else                              ! Weng's model
            calcemis  (1:nchannels) = .false.
            do i = 1, nchanl   !  loop for channels
 !RTTOV9_3 comment below line and add one line        
 !      ichannel = polarisations(i,1)  ! position of first polar at chanl i
 !              pol_id = coef%fastem_polar(i) + 1
 !              if (polarisations(i,3) == 2) then ! number of polar at chanl i
 !                 emissivity(ichannel) = emisv(i)
 !                 emissivity(ichannel+1) = emish(i)
 !              else if (polarisations(i,3) == 4) then
 !                 emissivity(ichannel)   = emisv(i)
 !                 emissivity(ichannel+1) = emish(i)
 !                 emissivity(ichannel+2) = 0.0
 !                 emissivity(ichannel+3) = 0.0
 !              else   ! == 1 V or H polar
 !                 if (pol_id == 4) emissivity(ichannel)   = emisv(i)
 !                 if (pol_id == 5) emissivity(ichannel)   = emish(i)
 !              end if
            emissivity(i) = emisv(i)                                 !RTTOV9_3
!END OF RTTOV9_3
            end do
         end if
      
         !  1.0 over sea-ice/snow/land
      else 
         calcemis  (1:nchannels) = .false.
         do i = 1, nchanl   !  loop for channels
 !RTTOV9_3 comment below lines and add one line      
 !        ichannel = polarisations(i,1)  ! position of first polar at chanl i
 !           pol_id = coef%fastem_polar(i) + 1
 !           if (polarisations(i,3) == 2) then ! number of polar at chanl i
 !              emissivity(ichannel) = emisv(i)
 !              emissivity(ichannel+1) = emish(i)
 !           else if (polarisations(i,3) == 4) then
 !              emissivity(ichannel)   = emisv(i)
 !              emissivity(ichannel+1) = emish(i)
 !              emissivity(ichannel+2) = 0.0
 !              emissivity(ichannel+3) = 0.0
 !           else   ! == 1 V or H polar
 !              if (pol_id == 4) emissivity(ichannel)   = emisv(i)
 !              if (pol_id == 5) emissivity(ichannel)   = emish(i)
 !           end if
            emissivity(i) = emisv(i)                                 !RTTOV9_3                                   
!END OF RTTOV9_3                                                           
        end do
      end if
   end if

   !-----------------------------------
   !  calling RTTOV forward model
   !----------------------------------

   !new rttov_direct for RTTOV9_3
#ifdef RTTOV
   !call rttov_direct(&
   !& errorstatus,     &! out
   !   & nfrequencies,    &! in
   !   & nchannels,       &! in
   !   & nbtout,          &! in
   !   & nprofiles,       &! in
   !   & channels,        &! in
   !   & polarisations,   &! in
   !   & lprofiles,       &! in
   !   & profiles,        &! in
   !   & coef,            &! in
   !   & addcloud,        &! in
   !   & calcemis,        &! in
   !   & emissivity,      &! inout
   !   & transmission,    &! inout
   !   & radiance)        ! inout


   call rttov_direct(&
        & errorstatus,       &! out
          & nprofiles,         &! in
  !       & nchannels,         &! in       if nchannels runtime error
          & nbtout,           &! in
          & channels,          &! in
          & lprofiles,         &! in
          & addinterp,         &! in
          & profiles,          &! in
          & coef,              &! in
          & coef_scatt_ir,     &! in
          & optp,              &! in
          & lsun,              &! in
          & laerosl,           &! in
          & addcloud,          &! in
          & calcemis,          &! in
          & emissivity,        &! inout
          & transmission,      &! inout
          & radiance)           ! inout
   

#endif


   ! rttov87 generates warnings we want to ignore
   if (print_detail_rad .and. (any(errorstatus(:) == errorstatus_fatal))) then
      WRITE (UNIT=stderr,FMT=*) 'rttov_direct error code = ', errorstatus(:)
      WRITE (UNIT=stderr,FMT=*) 'nfrequencies = ', nfrequencies
      WRITE (UNIT=stderr,FMT=*) 'nchannels    = ', nchannels
      WRITE (UNIT=stderr,FMT=*) 'nbtout       = ', nbtout
      WRITE (UNIT=stderr,FMT=*) 'nprofiles    = ', nprofiles
      WRITE (UNIT=stderr,FMT=*) 'channels     = ', channels
      WRITE (UNIT=stderr,FMT=*) 'polarisations= ', polarisations
      WRITE (UNIT=stderr,FMT=*) 'lprofiles    = ', lprofiles
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%t = ', profiles(1)%s2m%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%q = ', profiles(1)%s2m%q
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%o = ', profiles(1)%s2m%o
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%p = ', profiles(1)%s2m%p
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%u = ', profiles(1)%s2m%u
      WRITE (UNIT=stderr,FMT=*) 'profiles%s2m%v = ', profiles(1)%s2m%v
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%surftype = ', profiles(1)%skin%surftype
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%t        = ', profiles(1)%skin%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%skin%fastem   = ', profiles(1)%skin%fastem
      WRITE (UNIT=stderr,FMT=*) 'profiles%zenangle = ', profiles(1)%zenangle
      WRITE (UNIT=stderr,FMT=*) 'profiles%azangle = ', profiles(1)%azangle
      WRITE (UNIT=stderr,FMT=*) 'profiles%p   = ', profiles(1)%p
      WRITE (UNIT=stderr,FMT=*) 'profiles%t   = ', profiles(1)%t
      WRITE (UNIT=stderr,FMT=*) 'profiles%q   = ', profiles(1)%q
      do i=coef%nlevels,1,-1
         write(UNIT=stderr,FMT='(i4,3f12.2)') i, profiles(1)%p(i),profiles(1)%t(i),profiles(1)%q(i)
      end do
      !variables for RTTOV9_3
      WRITE (UNIT=stderr,FMT=*) 'addinterp    = ', addinterp  !RTTOV9_3
      WRITE (UNIT=stderr,FMT=*) 'lsun         = ', lsun       !RTTOV9_3
      WRITE (UNIT=stderr,FMT=*) 'laerosl      = ', laerosl    !RTTOV9_3
      WRITE (UNIT=stderr,FMT=*) 'addcloud     = ', addcloud
      WRITE (UNIT=stderr,FMT=*) 'calcemis     = ', calcemis
      WRITE (UNIT=stderr,FMT=*) 'emissivity   = ', emissivity
      WRITE (UNIT=stderr,FMT=*) 'radiance = ', radiance%out_clear
      call da_warning(__FILE__,__LINE__,(/"Problem in rttov_direct"/))
   end if

   nc = nbtout / nprofiles
   do n = 1, nprofiles
      !out_clear is 0 in RTTOV9_3,bt_clear is right
      !tb(1:nc) = radiance % out_clear((n-1)*nc+1:n*nc)
      tb(1:nc) = radiance % bt_clear((n-1)*nc+1:n*nc)
      rad_xb(1:nc) = radiance % clear((n-1)*nc+1:n*nc)
      do k = kts, kte
         rad_ovc(1:nc,k) = radiance % overcast(k,(n-1)*nc+1:n*nc)
      end do	
   end do

!  calcemis_out  (:) = calcemis(:)
   emissivity_out(:) = emissivity(:)



   deallocate (lprofiles)
   deallocate (channels)
   deallocate (polarisations)
   deallocate (emissivity)
   deallocate (calcemis)
   deallocate (surfem)
   do n = 1, nprofiles 
      deallocate (profiles(n)%p)
      deallocate (profiles(n)%t)
      deallocate (profiles(n)%q)
      deallocate (profiles(n)%o3)
      deallocate (profiles(n)%co2)
      deallocate (profiles(n)%clw)
!RTTOV9_3
      deallocate (profiles(n)%n2o)
      deallocate (profiles(n)%co)
      deallocate (profiles(n)%ch4)
      deallocate (profiles(n)%aerosols)
      deallocate (profiles(n)%cloud)
      deallocate (profiles(n)%cfrac)
!end of RTTOV9_3

   end do

   ! deallocate transmittance structure
!**************************************************************************************************
!********just transmission % tau_total and transmission % tau_layers need to be deallocated********
!********************because others have been deallocated by the rttov_alloc_transmission.F90******
!**************************************************************************************************



   deallocate (transmission % tau_total ,stat= alloc_status(91))
   deallocate (transmission % tau_layers,stat= alloc_status(92)  )



   ! deallocate radiance results arrays with number of channels
   deallocate (radiance % clear    ,stat=alloc_status(9))
   deallocate (radiance % cloudy   ,stat=alloc_status(10))
   deallocate (radiance % total    ,stat=alloc_status(11))
   deallocate (radiance % bt       ,stat=alloc_status(12))
   deallocate (radiance % bt_clear ,stat=alloc_status(13))
   deallocate (radiance % upclear  ,stat=alloc_status(14))
   deallocate (radiance % dnclear  ,stat=alloc_status(15))
   deallocate (radiance % reflclear,stat=alloc_status(16))
   deallocate (radiance % overcast ,stat=alloc_status(17))
   deallocate (radiance % downcld  ,stat=alloc_status(18))
   deallocate (radiance % out       ,stat= alloc_status(19))
   deallocate (radiance % out_clear ,stat= alloc_status(20))
   deallocate (radiance % total_out ,stat= alloc_status(21))
   deallocate (radiance % clear_out ,stat= alloc_status(22))
!RTTOV9_3
   deallocate (radiance % up       ,stat= alloc_status(23))
   deallocate (radiance % down       ,stat= alloc_status(24))
   deallocate (radiance % surf       ,stat= alloc_status(19))
!end of RTTOV9_3


   if (any(alloc_status /= 0)) then
      call da_error(__FILE__,__LINE__, &
        (/"mem deallocation error"/))
   end if

   call da_trace_exit("da_rttov_direct")
   
end subroutine da_rttov_direct
#endif
