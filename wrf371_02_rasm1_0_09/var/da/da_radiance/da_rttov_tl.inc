#ifdef RTTOV
subroutine da_rttov_tl(inst, nchanl, nprofiles, con_vars, aux_vars, &
                      con_vars_tl, aux_vars_tl, tb)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the tangent linear subroutine of RTTOV
   !---------------------------------------------------------------------------

   implicit none

   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (con_vars_type),  intent (in) :: con_vars_tl (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars_tl (nprofiles)
   real                , intent (out) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, j, asw, nlevels, nchanprof
   integer             :: alloc_status(10)

   ! RTTOV input parameters
   type (rttov_chanprof), allocatable :: chanprof(:)
   type (profile_type),   allocatable :: profiles(:), profiles_tl(:) 
   logical,               allocatable :: calcemis(:)
   type (rttov_emissivity), allocatable :: emissivity(:), emissivity_tl(:)

   ! RTTOV out parameters
   integer                            :: errorstatus

   ! RTTOV inout parameters
   type (radiance_type)               :: radiance, radiance_tl
   type (transmission_type)           :: transmission, transmission_tl

   call da_trace_entry("da_rttov_tl")

   nchanprof = nchanl*nprofiles

   ! generate the chanprof array
   allocate(chanprof(nchanprof))
   do n = 1, nprofiles
      chanprof((n-1)*nchanl+1:n*nchanl)%prof = n
      chanprof((n-1)*nchanl+1:n*nchanl)%chan = (/ (j, j=1,nchanl) /)
   end do

   alloc_status(:) = 0

   nlevels = con_vars(1) % nlevels

   ! allocate input profile arrays with the number of levels
   asw = 1  ! allocate
   allocate (profiles(nprofiles), stat=alloc_status(1))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles,                &
      & nlevels,                 &
      & opts(inst),              &
      & asw,                     &
      & coefs = coefs(inst),     &  ! mandatory if either opts%addclouds or opts%addaerosl is true
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(1) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for profile arrays"/))
   end if

   asw = 1  ! allocate
   allocate (profiles_tl(nprofiles), stat=alloc_status(2))
   call rttov_alloc_prof(        &
      & errorstatus,             &
      & nprofiles,               &
      & profiles_tl,             &
      & nlevels,                 &
      & opts(inst),              &
      & asw,                     &
      & coefs = coefs(inst),     &  ! mandatory if either opts%addclouds or opts%addaerosl is true
      & init = .true.            )  ! additionally initialize profiles structure
   if ( errorstatus /= errorstatus_success .or. alloc_status(2) /= 0 ) then
     call da_error(__FILE__,__LINE__, &
        (/"memory allocation error for profile TL arrays"/))
   end if

   do n = 1, nprofiles
      profiles(n) % p(:)       = coefs(inst)%coef%ref_prfl_p(:)
      profiles(n) % t(:)       = con_vars(n)%t(:)
      profiles(n) % q(:)       = con_vars(n)%q(:)
      if ( opts(inst) % rt_mw % clw_data)  profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)
      if ( opts_rt_ir(inst) % ozone_data ) profiles(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      if ( opts_rt_ir(inst) % co2_data )   profiles(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      if ( opts_rt_ir(inst) % n2o_data )   profiles(n) % n2o(:)     = 0.0
      if ( opts_rt_ir(inst) % co_data )    profiles(n) % co(:)      = 0.0
      if ( opts_rt_ir(inst) % co_data )    profiles(n) % ch4(:)     = 0.0
      if ( opts_rt_ir(inst) % addaerosl )  profiles(n) % aerosols(:,:)   = 0.0
      if ( opts_rt_ir(inst) % addclouds )  then
         profiles(n) % cloud(:,:)      = 0.0
         profiles(n) % cfrac(:)        = 0.0
         profiles(n) % idg             = 1
         profiles(n) % ish             = 1
      end if

      profiles(n)% skin % surftype   = aux_vars(n) % surftype
      if ( profiles(n)% skin % surftype == 1 ) then
         if ( opts_rt_ir(inst) % addsolar ) then
            ! if refelcted solar radiation is to be included in the SWIR channels, then
            ! specification of fresh or salt water needs to be provided
            profiles(n) % skin % watertype = 1
         end if
      end if

      if ( coefs(inst)%coef%id_sensor == sensor_id_mw ) then
         if ( profiles(n) % skin % surftype == 2 ) then
            profiles(n) % skin % fastem (1) = 2.2
            profiles(n) % skin % fastem (2) = 3.7
            profiles(n) % skin % fastem (3) = 122.0
            profiles(n) % skin % fastem (4) = 0.0
            profiles(n) % skin % fastem (5) = 0.15
         else if ( profiles(n) % skin % surftype == 0 ) then
            profiles(n) % skin % fastem (1) = 3.0
            profiles(n) % skin % fastem (2) = 5.0
            profiles(n) % skin % fastem (3) = 15.0
            profiles(n) % skin % fastem (4) = 0.1
            profiles(n) % skin % fastem (5) = 0.3
         end if
      end if

      profiles(n) % skin % t          = aux_vars (n) % surft
      !profiles(n) % skin % fastem (:) = 0.0 ! aux_vars (n) % fastem (:)

      profiles(n) % s2m % t   = aux_vars (n) % t2m
      profiles(n) % s2m % q   = aux_vars (n) % q2m
      profiles(n) % s2m % o   = 0.0                 ! o3, never used
      profiles(n) % s2m % p   = con_vars (n) % ps
      profiles(n) % s2m % u   = aux_vars (n) % u10
      profiles(n) % s2m % v   = aux_vars (n) % v10

      profiles(n) % zenangle  = aux_vars (n) % satzen
      profiles(n) % elevation = 0.001* aux_vars(n) % elevation   ! km
      profiles(n) % latitude  = aux_vars(n) % rlat

      if ( opts_rt_ir(inst) % addsolar ) then
         profiles(n) % azangle     = aux_vars (n) % satazi
         profiles(n) % sunzenangle = aux_vars (n) % solzen     !50.0
         profiles(n) % sunazangle  = aux_vars (n) % solazi     !86.0
         profiles(n) % s2m % wfetc = 100000.0  ! m
      end if

      profiles(n) % Be          = 0.35   ! optional, for zeeman effect for ssmis and amsua
      profiles(n) % cosbk       = 0.0    ! optional, for zeeman effect for ssmis and amsua

      profiles(n) % ctp         = 500.0  ! hPa, optional, for simple cloud
      profiles(n) % cfraction   = 0.0    ! 0-1, optional, for simple cloud

      profiles_tl(n) % t(:)       = con_vars_tl(n)%t(:)
      profiles_tl(n) % q(:)       = con_vars_tl(n)%q(:)
      profiles_tl(n) % s2m % p    = con_vars_tl(n)%ps

   end do

   allocate (emissivity(nchanprof), stat=alloc_status(3))
   allocate (emissivity_tl(nchanprof), stat=alloc_status(4))
   allocate (calcemis(nchanprof), stat=alloc_status(7))
   if ( any( alloc_status /= 0 ) ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for emissivity or calcemis arrays"/))
   end if

   ! allocate transmittance structure
   asw = 1 ! allocate
   call rttov_alloc_transmission( &
      & errorstatus,              &
      & transmission,             &
      & nlevels-1,                &
      & nchanprof,                &
      & asw,                      &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for transmittance arrays"/))
   end if

   asw = 1 ! allocate
   call rttov_alloc_transmission( &
      & errorstatus,              &
      & transmission_tl,          &
      & nlevels-1,                &
      & nchanprof,                &
      & asw,                      &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for transmittance TL arrays"/))
   end if

   ! allocate radiance results arrays with number of channels
   asw = 1 ! allocate
   call rttov_alloc_rad( &
      & errorstatus,     &
      & nchanprof,       &
      & radiance,        &
      & nlevels-1,       &
      & asw,             &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for radiance arrays"/))
   end if

   asw = 1 ! allocate
   call rttov_alloc_rad( &
      & errorstatus,     &
      & nchanprof,       &
      & radiance_tl,     &
      & nlevels-1,       &
      & asw,             &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for radiance TL arrays"/))
   end if

   if ( coefs(inst)%coef%id_sensor == 1 .or. coefs(inst)%coef%id_sensor == 3 ) then  ! infrared sensor 
      calcemis(1:nchanprof)   = .true.
      emissivity(1:nchanprof)%emis_in = 0.0
      emissivity_tl(1:nchanprof)%emis_in = 0.0
   else if ( coefs(inst)%coef%id_sensor == 2 ) then   ! microwave sensor
      do n = 1, nprofiles
         if ( profiles(n) % skin % surftype == 1 ) then  ! sea  
            calcemis((n-1)*nchanl+1:n*nchanl) = .true.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
            emissivity_tl((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
         else                                            ! 0:land ; 2:sea-ice
            calcemis((n-1)*nchanl+1:n*nchanl) = .false.
            emissivity((n-1)*nchanl+1:n*nchanl)%emis_in = 0.9
            emissivity_tl((n-1)*nchanl+1:n*nchanl)%emis_in = 0.0
         end if
      end do
   end if

   call  rttov_tl(          &
      & errorstatus,        & ! out
      & chanprof,           & ! in      chanprof(nchanprof)
      & opts(inst),         & ! in
      & profiles,           & ! in      profiles(nprof)
      & profiles_tl,        & ! inout   profiles_tl(nprof)
      & coefs(inst),        & ! in
      & transmission,       & ! inout
      & transmission_tl,    & ! inout
      & radiance,           & ! inout
      & radiance_tl,        & ! inout
      & calcemis,           & ! in,    optional   calcemis(nchanprof)
      & emissivity,         & ! inout, optional   emissivity(nchanprof)
      & emissivity_tl)        ! inout, optional   emissivity_tl(nchanprof)

   if ( print_detail_rad .or. errorstatus /= errorstatus_success ) then
       write (message(1),*)  'rttov_tl error code     = ', errorstatus
       write (message(2),*)  'nchanl                  = ', nchanl
       write (message(3),*)  'nprofiles               = ', nprofiles
       write (message(4),*)  'calcemis                = ', calcemis
       write (message(5),*)  'profiles%s2m            = ', profiles(1)%s2m
       write (message(6),*)  'profiles%skin           = ', profiles(1)%skin
       write (message(7),*)  'profiles%zenangle       = ', profiles(1)%zenangle
       write (message(8),*)  'profiles%azangle        = ', profiles(1)%azangle
       write (message(9),*)  'profiles%p              = ', profiles(1)%p
       write (message(10),*) 'profiles%t              = ', profiles(1)%t
       write (message(11),*) 'profiles%q              = ', profiles(1)%q
       write (message(12),*) 'emissivity_out          = ', emissivity(:)%emis_out
       write (message(13),*) 'radiance                = ', radiance%bt_clear
       write (message(14),*) 'profiles_tl%s2m         = ', profiles_tl(1)%s2m
       write (message(15),*) 'profiles_tl%skin        = ', profiles_tl(1)%skin
       write (message(16),*) 'profiles_tl%zenangle    = ', profiles_tl(1)%zenangle
       write (message(17),*) 'profiles_tl%azangle     = ', profiles_tl(1)%azangle
       write (message(18),*) 'profiles_tl%p           = ', profiles_tl(1)%p 
       write (message(19),*) 'profiles_tl%t           = ', profiles_tl(1)%t 
       write (message(20),*) 'profiles_tl%q           = ', profiles_tl(1)%q 
       write (message(21),*) 'emissivity_out_tl       = ', emissivity_tl(:)%emis_out    
       write (message(22),*) 'radiance_tl             = ', radiance_tl%bt_clear
       if ( errorstatus /= errorstatus_success ) call da_error(__FILE__,__LINE__,message(1:22))
   end if

   do n = 1, nprofiles
     tb(1:nchanl,n) = radiance_tl % bt_clear((n-1)*nchanl+1:n*nchanl)
   end do

   deallocate (emissivity)
   deallocate (emissivity_tl)
   deallocate (calcemis)
   deallocate (chanprof)

   asw = 0 ! deallocation
   ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels-1,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if
   call rttov_alloc_rad (errorstatus,nchanprof,radiance_tl,nlevels-1,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance TL deallocation error"/))
   end if

   ! deallocate transmission arrays
   call rttov_alloc_transmission (errorstatus,transmission,nlevels-1,nchanprof,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"transmission deallocation error"/))
   end if
   call rttov_alloc_transmission (errorstatus,transmission_tl,nlevels-1,nchanprof,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"transmission TL deallocation error"/))
   end if

   ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw)
   deallocate (profiles, stat=alloc_status(8))
   if ( errorstatus /= errorstatus_success .or. alloc_status(8) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if
   call rttov_alloc_prof (errorstatus,nprofiles,profiles_tl,nlevels,opts(inst),asw)
   deallocate (profiles_tl, stat=alloc_status(9))
   if ( errorstatus /= errorstatus_success .or. alloc_status(9) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile TL deallocation error"/))
   end if

   call da_trace_exit("da_rttov_tl")

end subroutine da_rttov_tl
#endif
