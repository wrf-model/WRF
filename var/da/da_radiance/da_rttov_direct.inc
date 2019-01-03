#ifdef RTTOV
subroutine da_rttov_direct(inst, nchanl, nprofiles, nlevels, &
                          con_vars, aux_vars, &
                          tb, rad_xb, rad_ovc, emissivity)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the forward subroutine of RTTOV
   !---------------------------------------------------------------------------

   implicit none

   integer             , intent (in) :: inst, nchanl, nprofiles, nlevels
   type (con_vars_type), intent (in) :: con_vars(nprofiles)
   type (aux_vars_type), intent (in) :: aux_vars(nprofiles)
   real                , intent (inout) :: tb(nchanl,nprofiles)
   real                , intent (inout) :: rad_xb(nchanl,nprofiles)
   real                , intent (inout) :: rad_ovc(nchanl,nlevels-1,nprofiles)
   type (rttov_emissivity), intent (inout) :: emissivity(nchanl*nprofiles)

   ! local variables
   integer             :: nchanprof, asw
   integer             :: n, i, j, k
   integer             :: alloc_status(4)
  
   ! RTTOV input parameters
   type (rttov_chanprof), allocatable :: chanprof(:)
   type (rttov_profile),   allocatable :: profiles(:) 
   logical, allocatable               :: calcemis(:)

   ! RTTOV out parameters
   integer             :: errorstatus

   ! RTTOV inout parameters
   type (rttov_radiance)     :: radiance
   type (rttov_transmission) :: transmission

   call da_trace_entry("da_rttov_direct")

   nchanprof = nchanl*nprofiles

   ! generate the chanprof array
   allocate(chanprof(nchanprof))
   do n = 1, nprofiles
      chanprof((n-1)*nchanl+1:n*nchanl)%prof = n
      chanprof((n-1)*nchanl+1:n*nchanl)%chan = (/ (j, j=1,nchanl) /)
   end do

   alloc_status (:) = 0

   ! allocate input profile arrays with the number of levels.
   asw = 1  ! allocate
   allocate( profiles(nprofiles),stat= alloc_status(1))
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

   do n = 1, nprofiles
      profiles(n) % p(:)       = coefs(inst)%coef%ref_prfl_p(:)
      profiles(n) % t(:)       = con_vars(n)%t(:)
      profiles(n) % q(:)       = con_vars(n)%q(:)
      if ( opts(inst) % rt_mw % clw_data ) profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)
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
         profiles(n) % ice_scheme      = 1
      end if

      profiles(n)% skin % surftype   = aux_vars(n) % surftype
      if ( profiles(n)% skin % surftype == 1 ) then
         if ( opts_rt_ir(inst) % addsolar ) then
            ! if refelcted solar radiation is to be included in the SWIR channels, then
            ! specification of fresh or salt water needs to be provided
            profiles(n) % skin % watertype = 1
         end if
      end if

      ! for microwave channels, land/sea-ce emissivity is computed
      ! from coefs in prof%skin%fastem, if calcemis = True
      if ( coefs(inst)%coef%id_sensor == sensor_id_mw .or. &
           coefs(inst)%coef%id_sensor == sensor_id_po ) then
         if ( profiles(n) % skin % surftype == 2 ) then  ! sea-ice
            profiles(n) % skin % fastem (1) = 2.2
            profiles(n) % skin % fastem (2) = 3.7
            profiles(n) % skin % fastem (3) = 122.0
            profiles(n) % skin % fastem (4) = 0.0
            profiles(n) % skin % fastem (5) = 0.15
         else if ( profiles(n) % skin % surftype == 0 ) then  ! land
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
   end do

   ! allocate radiance results arrays with number of channels
   asw = 1 ! allocate
   call rttov_alloc_rad( &
      & errorstatus,     &
      & nchanprof,       &
      & radiance,        &
      & nlevels,         &
      & asw,             &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for radiance arrays"/))
   end if

   allocate (calcemis(nchanprof), stat = alloc_status(3))
   if ( any( alloc_status /= 0 ) ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for calcemis arrays"/))
   end if

   ! allocate transmittance structure
   asw = 1 ! allocate
   call rttov_alloc_transmission( &
      & errorstatus,              &
      & transmission,             &
      & nlevels,                  &
      & nchanprof,                &
      & asw,                      &
      & init = .true. )
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
         (/"memory allocation error for transmittance arrays"/))
   end if

   ! calculate emissivity where the input emissivity value is less than 0.01
   calcemis(:) = emissivity(:)%emis_in < 0.01

   !-----------------------------------
   !  calling RTTOV forward model
   !----------------------------------

   call rttov_direct(      &
      & errorstatus,       &   ! out
      & chanprof,          &   ! in     chanprof(nchanprof)
      & opts(inst),        &   ! in
      & profiles,          &   ! in     profiles(nprof)
      & coefs(inst),       &   ! in
      & transmission,      &   ! inout
      & radiance,          &   ! inout
      & calcemis=calcemis, &   ! in,    optional   calcemis(nchanprof)
      & emissivity=emissivity) ! inout, optional   emissivity(nchanprof)

   if ( print_detail_rad .or. errorstatus /= errorstatus_success ) then
      WRITE (UNIT=stderr,FMT=*) 'rttov_direct error code = ', errorstatus
      WRITE (UNIT=stderr,FMT=*) 'nchanprof    = ', nchanprof
      WRITE (UNIT=stderr,FMT=*) 'nprofiles    = ', nprofiles
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
      WRITE (UNIT=stderr,FMT=*) 'calcemis     = ', calcemis
      WRITE (UNIT=stderr,FMT=*) 'emissivity   = ', emissivity(:)%emis_in
      WRITE (UNIT=stderr,FMT=*) 'emissivity_out = ', emissivity(:)%emis_out
      WRITE (UNIT=stderr,FMT=*) 'radiance = ', radiance%bt_clear
      if ( errorstatus /= errorstatus_success) call da_error(__FILE__,__LINE__,(/"Problem in rttov_direct"/))
   end if

   do n = 1, nprofiles
      tb(1:nchanl,n) = radiance % bt_clear((n-1)*nchanl+1:n*nchanl)
      rad_xb(1:nchanl,n) = radiance % clear((n-1)*nchanl+1:n*nchanl)
      do k = 1, nlevels-1
         rad_ovc(1:nchanl,k,n) = radiance % overcast(k,(n-1)*nchanl+1:n*nchanl)
      end do	
   end do

   deallocate (calcemis)
   deallocate (chanprof)

   asw = 0 ! deallocate radiance arrays
   call rttov_alloc_rad (errorstatus,nchanprof,radiance,nlevels,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"radiance deallocation error"/))
   end if

   asw = 0 ! deallocate transmission arrays
   call rttov_alloc_transmission (errorstatus,transmission,nlevels,nchanprof,asw)
   if ( errorstatus /= errorstatus_success ) then
      call da_error(__FILE__,__LINE__, &
        (/"transmission deallocation error"/))
   end if

   asw = 0 ! deallocate profile arrays
   call rttov_alloc_prof (errorstatus,nprofiles,profiles,nlevels,opts(inst),asw)
   deallocate(profiles,stat=alloc_status(4))
   if ( errorstatus /= errorstatus_success .or. alloc_status(4) /= 0 ) then
      call da_error(__FILE__,__LINE__, &
        (/"profile deallocation error"/))
   end if

   call da_trace_exit("da_rttov_direct")
   
end subroutine da_rttov_direct
#endif
