#ifdef RTTOV
subroutine da_rttov_tl(inst, nchanl, nprofiles, con_vars, aux_vars, &
                      con_vars_tl, aux_vars_tl, tb)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the tangent linear subroutine of RTTOV
   !---------------------------------------------------------------------------

   implicit none

#include "rttov_tl.interface"

   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (con_vars_type),  intent (in) :: con_vars_tl (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars_tl (nprofiles)
   real                , intent (out) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, nc
   Integer             :: alloc_status(140)

   ! RTTOV input parameters
   integer             :: nfrequencies, nchannels, nbtout
   integer             :: nchan(nprofiles)
   integer , pointer   :: lprofiles(:)
   type(rttov_coef)    :: coef
   type(profile_type)  :: profiles(nprofiles), profiles_tl(nprofiles) 
   logical             :: addcloud
   real*8 , allocatable  :: surfem(:)
   integer , pointer   :: channels (:), polarisations(:,:)
   logical, allocatable :: calcemis (:)

   ! RTTOV out parameters
   integer             :: errorstatus(nprofiles)

   ! RTTOV inout parameters
   real*8  , pointer        :: emissivity (:), emissivity_tl (:)
   type (radiance_type)     :: radiance, radiance_tl
   type (transmission_type) :: transmission, transmission_tl

   call da_trace_entry("da_rttov_tl")

   nchan (:) = nchanl 
   coef = coefs(inst)
   addcloud = .false. 
   alloc_status(:) = 0

   do n = 1, nprofiles
      profiles(n) % nlevels    = con_vars(n) % nlevels
      allocate (profiles(n)%p(profiles(n) % nlevels), stat=alloc_status(1))
      allocate (profiles(n)%t(profiles(n) % nlevels), stat=alloc_status(2))
      allocate (profiles(n)%q(profiles(n) % nlevels), stat=alloc_status(3))
      allocate (profiles(n)%o3(profiles(n) % nlevels), stat=alloc_status(4))
      allocate (profiles(n)%co2(profiles(n) % nlevels), stat=alloc_status(5))
      allocate (profiles(n)%clw(profiles(n) % nlevels), stat=alloc_status(6))
      if (any(alloc_status /= 0)) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to for profiles",n
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      profiles(n) % ozone_data = .false.
      profiles(n) % co2_data   = .false.
      profiles(n) % clw_data   = .false. 

      profiles(n) % p(:)       = coef%ref_prfl_p(:)
      profiles(n) % t(:)       = con_vars(n)%t(:)
      profiles(n) % q(:)       = con_vars(n)%q(:)
      profiles(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)

      profiles(n) % skin % surftype   = aux_vars (n) % surftype
      profiles(n) % skin % t          = aux_vars (n) % surft    
      profiles(n) % skin % fastem (:) = 0.0  ! aux_vars (n) % fastem (:)

      profiles(n) % s2m  % t    = aux_vars (n) % t2m
      profiles(n) % s2m  % q    = aux_vars (n) % q2m
      profiles(n) % s2m  % o    = 0.0 !aux_vars (n) % o3
      profiles(n) % s2m  % p    = con_vars (n) % ps
      profiles(n) % s2m  % u    = aux_vars (n) % u10
      profiles(n) % s2m  % v    = aux_vars (n) % v10

      profiles(n) % zenangle    = aux_vars (n) % satzen
      profiles(n) % azangle     = aux_vars (n) % satazi

      profiles(n) % ctp         = 500.0
      profiles(n) % cfraction   = 0.0

      profiles_tl(n) % nlevels    = con_vars_tl(n) % nlevels
      allocate (profiles_tl(n)%p(profiles_tl(n) % nlevels), stat=alloc_status(1))
      allocate (profiles_tl(n)%t(profiles_tl(n) % nlevels), stat=alloc_status(2))
      allocate (profiles_tl(n)%q(profiles_tl(n) % nlevels), stat=alloc_status(3))
      allocate (profiles_tl(n)%o3(profiles_tl(n) % nlevels), stat=alloc_status(4))
      allocate (profiles_tl(n)%co2(profiles_tl(n) % nlevels), stat=alloc_status(5))
      allocate (profiles_tl(n)%clw(profiles_tl(n) % nlevels), stat=alloc_status(6))
      if (any(alloc_status /= 0)) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to for profiles_tl",n
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      profiles_tl(n) % ozone_data = .false.
      profiles_tl(n) % co2_data   = .false.
      profiles_tl(n) % clw_data   = .false.

      profiles_tl(n) % p(:)       = 0.0
      profiles_tl(n) % t(:)       = con_vars_tl(n)%t(:)
      profiles_tl(n) % q(:)       = con_vars_tl(n)%q(:)
      profiles_tl(n) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles_tl(n) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles_tl(n) % clw(:)     = 0.0 !con_vars(n)%clw(:)

      profiles_tl(n) % skin % surftype   = -1
      profiles_tl(n) % skin % t          = 0.0  !aux_vars_tl (n) % surft
      profiles_tl(n) % skin % fastem (:) = 0.0  ! aux_vars (n) % fastem (:)

      profiles_tl(n) % s2m  % t    = 0.0 !aux_vars_tl (n) % t2m
      profiles_tl(n) % s2m  % q    = 0.0 !aux_vars_tl (n) % q2m
      profiles_tl(n) % s2m  % o    = 0.0 !aux_vars_tl (n) % o3
      profiles_tl(n) % s2m  % p    = con_vars_tl (n) % ps
      profiles_tl(n) % s2m  % u    = 0.0 !aux_vars_tl (n) % u10
      profiles_tl(n) % s2m  % v    = 0.0 !aux_vars_tl (n) % v10

      profiles_tl(n) % zenangle    = -1
      profiles_tl(n) % azangle     = -1

      profiles_tl(n) % ctp         = 0.0 !500.
      profiles_tl(n) % cfraction   = 0.0

   end do

#ifdef RTTOV
   call rttov_setupchan(nprofiles, nchan, coef, &             ! in
                     nfrequencies, nchannels, nbtout)       ! out
#endif


   allocate (lprofiles(nfrequencies), stat = alloc_status(31))
   allocate (channels (nfrequencies), stat = alloc_status(32))
   allocate (polarisations(nchannels, 3), stat = alloc_status(33))
   allocate (emissivity(nchannels), stat = alloc_status(34))
   allocate (emissivity_tl(nchannels), stat = alloc_status(134))
   allocate (calcemis(nchannels), stat = alloc_status(35))
   allocate (surfem (nchannels), stat = alloc_status(36))

   ! allocate transmittance structure
   allocate (transmission % tau_surf      (nchannels)                 ,stat= alloc_status(8))
   allocate (transmission % tau_layer     (coef % nlevels, nchannels) ,stat= alloc_status(9))
   allocate (transmission % od_singlelayer(coef % nlevels, nchannels ),stat= alloc_status(10))

   allocate (transmission_tl % tau_surf      (nchannels)                 ,stat= alloc_status(108))
   allocate (transmission_tl % tau_layer     (coef % nlevels, nchannels) ,stat= alloc_status(109))
   allocate (transmission_tl % od_singlelayer(coef % nlevels, nchannels ),stat= alloc_status(110))


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

   allocate (radiance % out      (nbtout) ,stat= alloc_status(121))
   allocate (radiance % out_clear(nbtout) ,stat= alloc_status(122))
   allocate (radiance % total_out(nbtout) ,stat= alloc_status(123))
   allocate (radiance % clear_out(nbtout) ,stat= alloc_status(124))

   ! allocate radiance results arrays with number of channels
   allocate (radiance_tl % clear    (nchannels) ,stat= alloc_status(111))
   allocate (radiance_tl % cloudy   (nchannels) ,stat= alloc_status(112))
   allocate (radiance_tl % total    (nchannels) ,stat= alloc_status(113))
   allocate (radiance_tl % bt       (nchannels) ,stat= alloc_status(114))
   allocate (radiance_tl % bt_clear (nchannels) ,stat= alloc_status(115))
   allocate (radiance_tl % upclear  (nchannels) ,stat= alloc_status(116))
   allocate (radiance_tl % dnclear  (nchannels) ,stat= alloc_status(117))
   allocate (radiance_tl % reflclear(nchannels) ,stat= alloc_status(118))
   allocate (radiance_tl % overcast (coef % nlevels, nchannels) ,stat= alloc_status(119))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance_tl % downcld  (coef % nlevels, nchannels) ,stat= alloc_status(120))

   allocate (radiance_tl % out      (nbtout) ,stat= alloc_status(121))
   allocate (radiance_tl % out_clear(nbtout) ,stat= alloc_status(122))
   allocate (radiance_tl % total_out(nbtout) ,stat= alloc_status(123))
   allocate (radiance_tl % clear_out(nbtout) ,stat= alloc_status(124))

   if (any(alloc_status /= 0)) then
      call da_error(__FILE__,__LINE__, &
         (/"mem allocation error prior to rttov_tl"/))
   end if

   surfem (:) = 0.0
#ifdef RTTOV
   call rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in
                nchannels, nbtout, coef, surfem,  &          ! in
                lprofiles, channels, polarisations,     &    ! out
                emissivity )                                ! out                       
#endif

   nc = nchannels/nprofiles

   if (coef%id_sensor == 1) then        ! infrared sensor 
      calcemis (1:nchannels)   = .true.
      emissivity (1:nchannels) = 0.0
      emissivity_tl (1:nchannels) = 0.0
   else if (coef%id_sensor == 2) then   ! microwave sensor
      do n = 1, nprofiles
         if (profiles(n) % skin % surftype == 1) then  ! sea  
            calcemis ((n-1)*nc+1:n*nc) = .true.
            emissivity ((n-1)*nc+1:n*nc) = 0.0
            emissivity_tl ((n-1)*nc+1:n*nc) = 0.0
         else                                       ! 0:land ; 2:sea-ice
            calcemis ((n-1)*nc+1:n*nc) = .false.
            emissivity ((n-1)*nc+1:n*nc) = 0.9
            emissivity_tl ((n-1)*nc+1:n*nc) = 0.0
         end if
      end do
   end if

#ifdef RTTOV
   call rttov_tl(& 
      & errorstatus,   &! out
      & nfrequencies,  &! in
      & nchannels,     &! in
      & nbtout,        &! in
      & nprofiles,     &! in
      & channels,      &! in
      & polarisations, &! in
      & lprofiles,     &! in
      & profiles,      &! in
      & coef,          &! in
      & addcloud,      &! in
      & calcemis,      &! in
      & emissivity,    &! inout
      & profiles_tl,     &! in
      & emissivity_tl,   &! inout
      & transmission,    &! inout
      & transmission_tl, &! inout
      & radiance,    &! inout
      & radiance_tl) ! inout
#endif

   ! rttov87 generates warnings we want to ignore
   if (print_detail_rad .and. (any(errorstatus(:) == errorstatus_fatal))) then
       write (message(1),*)  'rttov_direct error code = ', errorstatus(:)
       write (message(2),*)  'nfrequencies            = ', nfrequencies
       write (message(3),*)  'nchannels               = ', nchannels
       write (message(4),*)  'nbtout                  = ', nbtout
       write (message(5),*)  'nprofiles               = ', nprofiles
       write (message(6),*)  'channels                = ', channels
       write (message(7),*)  'polarisations           = ', polarisations
       write (message(8),*)  'lprofiles               = ', lprofiles
       write (message(9),*)  'addcloud                = ', addcloud
       write (message(10),*) 'calcemis                = ', calcemis
       write (message(11),*) 'profiles%s2m            = ', profiles(1)%s2m
       write (message(12),*) 'profiles%skin           = ', profiles(1)%skin
       write (message(13),*) 'profiles%zenangle       = ', profiles(1)%zenangle
       write (message(14),*) 'profiles%azangle        = ', profiles(1)%azangle
       write (message(15),*) 'profiles%p              = ', profiles(1)%p
       write (message(16),*) 'profiles%t              = ', profiles(1)%t
       write (message(17),*) 'profiles%q              = ', profiles(1)%q
       write (message(18),*) 'emissivity              = ', emissivity
       write (message(19),*) 'radiance                = ', radiance%out_clear
       write (message(20),*) 'profiles_tl%s2m         = ', profiles_tl(1)%s2m
       write (message(21),*) 'profiles_tl%skin        = ', profiles_tl(1)%skin
       write (message(22),*) 'profiles_tl%zenangle    = ', profiles_tl(1)%zenangle
       write (message(23),*) 'profiles_tl%azangle     = ', profiles_tl(1)%azangle
       write (message(24),*) 'profiles_tl%p           = ', profiles_tl(1)%p 
       write (message(25),*) 'profiles_tl%t           = ', profiles_tl(1)%t 
       write (message(26),*) 'profiles_tl%q           = ', profiles_tl(1)%q 
       write (message(27),*) 'emissivity_tl           = ', emissivity_tl    
       write (message(28),*) 'radiance_tl             = ', radiance_tl%out_clear
       call da_warning(__FILE__,__LINE__,message(1:28)) 
   end if

   nc = nbtout / nprofiles
   do n = 1, nprofiles
      tb(1:nc,n) = radiance_tl % out_clear((n-1)*nc+1:n*nc)
   end do

   deallocate (lprofiles)
   deallocate (channels)
   deallocate (polarisations)
   deallocate (emissivity)
   deallocate (emissivity_tl)
   deallocate (calcemis)
   deallocate (surfem)
   do n = 1, nprofiles 
      deallocate (profiles(n)%p)
      deallocate (profiles(n)%t)
      deallocate (profiles(n)%q)
      deallocate (profiles(n)%o3)
      deallocate (profiles(n)%co2)
      deallocate (profiles(n)%clw)

      deallocate (profiles_tl(n)%p)
      deallocate (profiles_tl(n)%t)
      deallocate (profiles_tl(n)%q)
      deallocate (profiles_tl(n)%o3)
      deallocate (profiles_tl(n)%co2)
      deallocate (profiles_tl(n)%clw)
   end do

   ! deallocate transmittance structure
   deallocate (transmission % tau_surf      ,stat= alloc_status(6))
   deallocate (transmission % tau_layer     ,stat= alloc_status(7))
   deallocate (transmission % od_singlelayer,stat= alloc_status(8))

   ! deallocate transmittance structure
   deallocate (transmission_tl % tau_surf      ,stat= alloc_status(106))
   deallocate (transmission_tl % tau_layer     ,stat= alloc_status(107))
   deallocate (transmission_tl % od_singlelayer,stat= alloc_status(108))

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

   deallocate (radiance_tl % clear    ,stat=alloc_status(109))
   deallocate (radiance_tl % cloudy   ,stat=alloc_status(110))
   deallocate (radiance_tl % total    ,stat=alloc_status(111))
   deallocate (radiance_tl % bt       ,stat=alloc_status(112))
   deallocate (radiance_tl % bt_clear ,stat=alloc_status(113))
   deallocate (radiance_tl % upclear  ,stat=alloc_status(114))
   deallocate (radiance_tl % dnclear  ,stat=alloc_status(115))
   deallocate (radiance_tl % reflclear,stat=alloc_status(116))
   deallocate (radiance_tl % overcast ,stat=alloc_status(117))
   deallocate (radiance_tl % downcld  ,stat=alloc_status(118))
   deallocate (radiance_tl % out       ,stat= alloc_status(119))
   deallocate (radiance_tl % out_clear ,stat= alloc_status(120))
   deallocate (radiance_tl % total_out ,stat= alloc_status(121))
   deallocate (radiance_tl % clear_out ,stat= alloc_status(122))


   if (any(alloc_status /= 0)) then
     call da_error(__FILE__,__LINE__, &
       (/"mem deallocation error"/))
   end if

   call da_trace_exit("da_rttov_tl")

end subroutine da_rttov_tl
#endif
