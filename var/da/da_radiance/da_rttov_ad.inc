#ifdef RTTOV
subroutine da_rttov_ad( inst, nchanl, nprofiles, con_vars, &
                      aux_vars, con_vars_ad, tb )

   !---------------------------------------------------------------------------
   ! PURPOSE: interface to the adjoint subroutine of RTTOV
   !---------------------------------------------------------------------------





#ifdef RTTOV
     Use parkind1, Only  :  jpim   ,jprb
#endif

   implicit none

#include "rttov_ad.interface"

   integer             ,  intent (in) :: inst, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (con_vars_type),  intent (inout) :: con_vars_ad (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   real                ,  intent (in) :: tb(nchanl,nprofiles)

   ! local variables
   integer             :: n, nc
   Integer             :: alloc_status(140)
   Integer (Kind = jpim) :: nstreams            !RTTOV9_3  

   ! RTTOV input parameters
   integer             :: nfrequencies, nchannels, nbtout
   integer             :: nchan(nprofiles)
   integer , pointer   :: lprofiles(:)
   type(rttov_coef)    :: coef
   type(rttov_coef_scatt_ir)   :: coef_scatt_ir                                     !RTTOV9_3
   type(rttov_optpar_ir)       :: optp                                              !RTTOV9_3
   type(profile_type)  :: profiles(nprofiles), profiles_ad(nprofiles) 
   logical             :: addcloud, switchrad, addinterp, lsun, laerosl             !RTTOV9_3 
   real*8, allocatable    :: surfem(:)
   integer , pointer   :: channels (:), polarisations(:,:)
   logical, allocatable :: calcemis (:)

   ! RTTOV out parameters
   integer             :: errorstatus(nprofiles)

   ! RTTOV inout parameters
   real*8  , pointer        :: emissivity (:), emissivity_ad (:)
   type (radiance_type)     :: radiance, radiance_ad
   type (transmission_type) :: transmission, transmission_ad


   call da_trace_entry("da_rttov_ad")

   nchan (:) = nchanl 
   coef = coefs(inst)
   coef_scatt_ir = coefs_scatt_ir(inst)                                            !RTTOV9_3
   optp = optps(inst)                                                              !RTTOV9_3
   addinterp = .false.                                                             !RTTOV9_3
   lsun = .false.                                                                  !RTTOV9_3
   laerosl = .false.                                                               !RTTOV9_3
   addcloud = .false.                                                              
   switchrad = .TRUE.
   alloc_status (:) = 0.0

   do n = 1, nprofiles
      profiles(n) % nlevels    = con_vars(n) % nlevels
      allocate ( profiles(n)%p(profiles(n) % nlevels), stat=alloc_status(1) )
      allocate ( profiles(n)%t(profiles(n) % nlevels), stat=alloc_status(2) )
      allocate ( profiles(n)%q(profiles(n) % nlevels), stat=alloc_status(3) )
      allocate ( profiles(n)%o3(profiles(n) % nlevels), stat=alloc_status(4) )
      allocate ( profiles(n)%co2(profiles(n) % nlevels), stat=alloc_status(5) )
      allocate ( profiles(n)%clw(profiles(n) % nlevels), stat=alloc_status(6) )
!RTTOV9_3
      allocate (profiles(n)%n2o(profiles(n) % nlevels), stat=alloc_status(41))
      allocate (profiles(n)%co(profiles(n) % nlevels), stat=alloc_status(42))
      allocate (profiles(n)%ch4(profiles(n) % nlevels),stat=alloc_status(43))
      allocate (profiles(n)%aerosols(11,profiles(n) % nlevels),stat=alloc_status(44))
      allocate (profiles(n)%cloud(6,profiles(n) % nlevels),stat=alloc_status(45))
      allocate (profiles(n)%cfrac(6,profiles(n) % nlevels),stat=alloc_status(46))
!end of RTTOV9_3
      if ( Any(alloc_status /= 0) ) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "ad mem allocation error to profile",n
         call da_error(__FILE__,__LINE__,message(1:1))
      End if

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
      

      profiles(n) % skin % surftype   = aux_vars (n) % surftype
      profiles(n) % skin % t          = aux_vars (n) % surft    
      profiles(n) % skin % fastem (:) = 0.0  ! aux_vars (n) % fastem (:)

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
      profiles(n) % elevation =  0.001* aux_vars (n) % elevation  !800.0

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

      profiles_ad(n) % nlevels    = con_vars(n) % nlevels
      allocate ( profiles_ad(n)%p(profiles_ad(n) % nlevels), stat=alloc_status(1) )
      allocate ( profiles_ad(n)%t(profiles_ad(n) % nlevels), stat=alloc_status(2) )
      allocate ( profiles_ad(n)%q(profiles_ad(n) % nlevels), stat=alloc_status(3) )
      allocate ( profiles_ad(n)%o3(profiles_ad(n) % nlevels), stat=alloc_status(4) )
      allocate ( profiles_ad(n)%co2(profiles_ad(n) % nlevels), stat=alloc_status(5) )
      allocate ( profiles_ad(n)%clw(profiles_ad(n) % nlevels), stat=alloc_status(6) )
 !RTTOV9_3
      allocate (profiles_ad(n)%n2o(profiles(n) % nlevels), stat=alloc_status(41))
      allocate (profiles_ad(n)%co(profiles(n) % nlevels), stat=alloc_status(42))
      allocate (profiles_ad(n)%ch4(profiles(n) % nlevels),stat=alloc_status(43))
      allocate (profiles_ad(n)%aerosols(11,profiles(n) % nlevels),stat=alloc_status(44))
      allocate (profiles_ad(n)%cloud(6,profiles(n) % nlevels),stat=alloc_status(45))
      allocate (profiles_ad(n)%cfrac(6,profiles(n) % nlevels),stat=alloc_status(46))
!end of RTTOV9_3

     if (Any(alloc_status /= 0) ) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to profiles_ad",n
         call da_error(__FILE__,__LINE__,message(1:1))
      End if

      profiles_ad(n) % ozone_Data = .False.  ! no meaning
      profiles_ad(n) % co2_Data   = .False.  ! no meaning
      profiles_ad(n) % clw_Data   = .False.  ! no meaning
      profiles_ad(n) % zenangle   = -1       ! no meaning
      profiles_ad(n) % p(:)   = 0.0 ! no AD on pressure levels
      profiles_ad(n) % t(:)   = 0.0 ! temperarure
      profiles_ad(n) % o3(:)  = 0.0 ! O3 ppmv
      profiles_ad(n) % clw(:) = 0.0 ! clw
      profiles_ad(n) % q(:)   = 0.0 ! WV
      profiles_ad(n) % s2m % t = 0.0 ! temperarure
      profiles_ad(n) % s2m % q = 0      ! WV
      profiles_ad(n) % s2m % p = 0.0 ! pressure
      profiles_ad(n) % s2m % u = 0.0 ! wind components
      profiles_ad(n) % s2m % v = 0.0 ! wind components
      profiles_ad(n) % skin % surftype = -1  ! no meaning
      profiles_ad(n) % skin % t        = 0.0  ! on temperarure
      profiles_ad(n) % skin % fastem   = 0.0  ! Fastem
      profiles_ad(n) % ctp       = 0.0  ! cloud top pressure
      profiles_ad(n) % cfraction = 0.0  ! cloud fraction
!RTTVO9_3
      profiles_ad(n) % n2o_data   = .false.
      profiles_ad(n) % co_data    = .false.
      profiles_ad(n) % ch4_data   = .false.
      profiles_ad(n) % aer_data   = .false.
      profiles_ad(n) % cld_data   = .false.
      profiles_ad(n) % n2o(:)     = 0.0
      profiles_ad(n) % co(:)      = 0.0
      profiles_ad(n) % ch4(:)     = 0.0
      profiles_ad(n) % aerosols(:,:)   = 0.0
      profiles_ad(n) % cloud(:,:)      = 0.0
      profiles_ad(n) % cfrac(:,:)      = 0.0
      profiles_ad(n) % idg             = -1_jpim
      profiles_ad(n) % ish             = -1_jpim
      profiles_ad(n) % skin % watertype = -1
      profiles_ad(n) % s2m  % wfetc  =  0.0              !RTTOV9_3 100000.0
      profiles_ad(n) % sunzenangle = -1                           !RTTOV9_3
      profiles_ad(n) % sunazangle  = -1                           !RTTOV9_3
      profiles_ad(n) % elevation   = -1
      profiles_ad(n) % latitude    = -1 
      profiles_ad(n) % addsolar    = .false.
      profiles_ad(n) % addrefrac   = .false.
      profiles_ad(n) % addaerosl   = .false.
      profiles_ad(n) % addwatrcl   = .false.
      profiles_ad(n) % Be          = -1
      profiles_ad(n) % cosbk       = -1
      
!end of RTTOV9_3


   end do

!#ifdef RTTOV
   call da_rttov_setupchan(nprofiles, nchan, coef, &             ! in                     RTTOV9_3
                     nfrequencies, nchannels, nbtout )       ! out
   nchannels = nfrequencies                               !RTTOV9_3
!#endif

   allocate (lprofiles(nfrequencies), stat = alloc_status(31) )
   allocate (channels (nfrequencies), stat = alloc_status(32) )
   allocate (polarisations(nchannels, 3), stat = alloc_status(33) )
   allocate (emissivity( nchannels ), stat = alloc_status(34) )
   allocate (emissivity_ad( nchannels ), stat = alloc_status(134) )
   allocate (calcemis( nchannels ), stat = alloc_status(35) )
   allocate (surfem( nchannels ), stat = alloc_status(36) )
   
   nstreams = 0_jpim                                                                            !RTTOV9_3
   ! allocate transmittance structure
   allocate (transmission % tau_surf      (0 : nstreams,nchannels)        ,stat= alloc_status(8))
   allocate (transmission % tau_layer     (coef % nlevels, 0 :  nstreams, nchannels) ,    stat= alloc_status(9))
   allocate (transmission % od_singlelayer(coef % nlevels, 0 :  nstreams, nchannels ),  stat= alloc_status(10))
!RTTOV9_3
   allocate (transmission % tau_total(nchannels),  stat = alloc_status(51))
   allocate (transmission % tau_layers(coef % nlevels, nchannels), stat = alloc_status(52))
   allocate (transmission % od_layer (coef % nlevels,0 : nstreams, nchannels), stat = alloc_status(53))
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

   allocate (transmission_ad % tau_surf      (0 : nstreams,nchannels)        ,stat= alloc_status(8))
   allocate (transmission_ad % tau_layer     (coef % nlevels, 0 : nstreams, nchannels) ,  stat= alloc_status(9))
   allocate (transmission_ad % od_singlelayer(coef % nlevels, 0 : nstreams, nchannels ),  stat= alloc_status(10))
!RTTOV9_3
   allocate (transmission_ad % tau_total(nchannels),  stat = alloc_status(51))
   allocate (transmission_ad % tau_layers(coef % nlevels, nchannels), stat = alloc_status(52))
   allocate (transmission_ad % od_layer (coef % nlevels,0 : nstreams, nchannels), stat = alloc_status(53))
   allocate (transmission_ad % odsun_singlelayer(coef % nlevels, 0 : nstreams, nchannels), & 
            stat = alloc_status(54))
   allocate (transmission_ad % tausun_surf ( 0 : nstreams, nchannels),stat = alloc_status(55))
   allocate (transmission_ad % tausun_layer( coef % nlevels,0 :  nstreams, nchannels), stat = alloc_status(56))
   allocate (transmission_ad % od_sfrac( 0 : nstreams, nchannels), stat = alloc_status(57))
   allocate (transmission_ad % odsun_sfrac( 0 : nstreams, nchannels), stat = alloc_status(58))
   allocate (transmission_ad % od_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(59))
   allocate (transmission_ad % odsun_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(60))
   allocate (transmission_ad % tau_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(61))
   allocate (transmission_ad % tau_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(62))
   allocate (transmission_ad % tau_ref_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(63))
   allocate (transmission_ad % tau_ref_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(64))
   allocate (transmission_ad % od_frac_t( 0 : nstreams, nchannels), stat = alloc_status(65))
   allocate (transmission_ad % odsun_frac_t( 0 : nstreams, nchannels), stat = alloc_status(66))
   allocate (transmission_ad % tau_surf_t( 0 : nstreams, nchannels), stat = alloc_status(67))
   allocate (transmission_ad % tausun_surf_t( 0 : nstreams, nchannels), stat = alloc_status(68))
   allocate (transmission_ad % tau_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(69))
   allocate (transmission_ad % tausun_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(70))
 
!end of RTTOV9_3


   ! allocate radiance results arrays with number of channels
   allocate (radiance % clear    ( nchannels ) ,stat= alloc_status(11))
   allocate (radiance % cloudy   ( nchannels ) ,stat= alloc_status(12))
   allocate (radiance % total    ( nchannels ) ,stat= alloc_status(13))
   allocate (radiance % bt       ( nchannels ) ,stat= alloc_status(14))
   allocate (radiance % bt_clear ( nchannels ) ,stat= alloc_status(15))
   allocate (radiance % upclear  ( nchannels ) ,stat= alloc_status(16))
   allocate (radiance % dnclear  ( nchannels ) ,stat= alloc_status(17))
   allocate (radiance % reflclear( nchannels ) ,stat= alloc_status(18))
   allocate (radiance % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(19))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(20))

   allocate (radiance % out      ( nbtout ) ,stat= alloc_status(121))
   allocate (radiance % out_clear( nbtout ) ,stat= alloc_status(122))
   allocate (radiance % total_out( nbtout ) ,stat= alloc_status(123))
   allocate (radiance % clear_out( nbtout ) ,stat= alloc_status(124))

!RTTOV9_3
  allocate (radiance % up(coef % nlevels, nchannels) ,stat= alloc_status(81))
  allocate (radiance % down(coef % nlevels, nchannels) ,stat= alloc_status(82))
  allocate (radiance % surf(coef % nlevels, nchannels) ,stat= alloc_status(83))
!end of RTTOV9_3


   ! allocate radiance results arrays with number of channels
   allocate (radiance_ad % clear    ( nchannels ) ,stat= alloc_status(111))
   allocate (radiance_ad % cloudy   ( nchannels ) ,stat= alloc_status(112))
   allocate (radiance_ad % total    ( nchannels ) ,stat= alloc_status(113))
   allocate (radiance_ad % bt       ( nchannels ) ,stat= alloc_status(114))
   allocate (radiance_ad % bt_clear ( nchannels ) ,stat= alloc_status(115))
   allocate (radiance_ad % upclear  ( nchannels ) ,stat= alloc_status(116))
   allocate (radiance_ad % dnclear  ( nchannels ) ,stat= alloc_status(117))
   allocate (radiance_ad % reflclear( nchannels ) ,stat= alloc_status(118))
   allocate (radiance_ad % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(119))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance_ad % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(120))

   allocate (radiance_ad % out      ( nbtout ) ,stat= alloc_status(121))
   allocate (radiance_ad % out_clear( nbtout ) ,stat= alloc_status(122))
   allocate (radiance_ad % total_out( nbtout ) ,stat= alloc_status(123))
   allocate (radiance_ad % clear_out( nbtout ) ,stat= alloc_status(124))

!RTTOV9_3
  allocate (radiance_ad % up(coef % nlevels, nchannels) ,stat= alloc_status(81))
  allocate (radiance_ad % down(coef % nlevels, nchannels) ,stat= alloc_status(82))
  allocate (radiance_ad % surf(coef % nlevels, nchannels) ,stat= alloc_status(83))
!end of RTTOV9_3


   if ( Any(alloc_status /= 0) ) then
      call da_error(__FILE__,__LINE__, &
         (/"mem allocation error prior to rttov_ad"/))
   End if

   ! initialise all radiance increments to 0
   radiance_ad % clear(:)      = 0.0
   radiance_ad % clear_out(:)  = 0.0
   radiance_ad % cloudy(:)     = 0.0
   radiance_ad % total(:)      = 0.0
   radiance_ad % total_out(:)  = 0.0
   radiance_ad % bt(:)         = 0.0
   radiance_ad % bt_clear(:)   = 0.0
   radiance_ad % out(:)        = 0.0
   radiance_ad % out_clear(:)  = 0.0
   radiance_ad % upclear(:)    = 0.0
   radiance_ad % reflclear(:)  = 0.0
   radiance_ad % overcast(:,:) = 0.0
   radiance_ad % downcld(:,:)  = 0.0
!RTTOV9_3
   radiance_ad % dnclear  = 0.0
   radiance_ad % up        = 0.0
   radiance_ad % down      = 0.0
   radiance_ad % surf      = 0.0
!end of RTTOV9_3

   transmission_ad % tau_surf(:,:)   = 0.0
   transmission_ad % tau_layer(:,:,:)= 0.0
   transmission_ad % od_singlelayer(:,:,:) = 0.0
!RTTOV9_3
   transmission_ad % tau_total         = 0.0
   transmission_ad % tau_layers        = 0.0
   transmission_ad % od_layer          = 0.0
   transmission_ad % odsun_singlelayer = 0.0
   transmission_ad % tausun_surf       = 0.0
   transmission_ad % tausun_layer      = 0.0
   transmission_ad % od_sfrac          = 0.0
   transmission_ad % odsun_sfrac       = 0.0
   transmission_ad % od_frac_ac        = 0.0
   transmission_ad % odsun_frac_ac     = 0.0
   transmission_ad % tau_surf_ac       = 0.0
   transmission_ad % tau_surf_acsun    = 0.0
   transmission_ad % tau_ref_surf_ac   = 0.0
   transmission_ad % tau_ref_surf_acsun = 0.0
   transmission_ad % od_frac_t         = 0.0
   transmission_ad % odsun_frac_t      = 0.0
   transmission_ad % tau_surf_t        = 0.0
   transmission_ad % tausun_surf_t     = 0.0
   transmission_ad % tau_ref_surf_t    = 0.0
   transmission_ad % tausun_ref_surf_t = 0.0
!end of RTTOV9_3


   nc = nbtout / nprofiles
   do n = 1, nprofiles
 !     radiance_ad % out((n-1)*nc+1:n*nc) = tb(1:nc,n)  !RTTOV9_3
       radiance_ad % bt ((n-1)*nc+1:n*nc) = tb(1:nc,n)  !RTTOV9_3
   end do 
   surfem (:) = 0.0
!#ifdef RTTOV
   call da_rttov_setupindex(nchan, nprofiles, nfrequencies, &    ! in                           RTTOV9_3
                nchannels, nbtout, coef, surfem,  &          ! in
                lprofiles, channels, polarisations,     &    ! out
                emissivity  )                                ! out                       
!#endif

   nc = nchannels/nprofiles

   if (coef%id_sensor == 1 .or. coef%id_sensor == 3)  then   ! infrared sensor 
      calcemis (1:nchannels)   = .true.
      emissivity (1:nchannels) = 0.0
      emissivity_ad (1:nchannels) = 0.0
   else if (coef%id_sensor == 2)  then   ! microwave sensor
      do n = 1, nprofiles
         if ( profiles(n) % skin % surftype == 1) then  ! sea  
            calcemis ((n-1)*nc+1:n*nc) = .true.
            emissivity ((n-1)*nc+1:n*nc) = 0.0
            emissivity_ad ((n-1)*nc+1:n*nc) = 0.0
         else                                       ! 0:land ; 2:sea-ice
            calcemis ((n-1)*nc+1:n*nc) = .false.
            emissivity ((n-1)*nc+1:n*nc) = 0.9
            emissivity_ad ((n-1)*nc+1:n*nc) = 0.0
         end if
      end do
   end if

#ifdef RTTOV
   !New arguments for RTTOV9_3
  ! call rttov_ad( & 
  !    & errorstatus,     &! out
  !    & nfrequencies,    &! in
  !    & nchannels,       &! in
  !    & nbtout,          &! in
  !    & nprofiles,       &! in
  !    & channels,        &! in
  !    & polarisations,   &! in
  !    & lprofiles,       &! in
  !    & profiles,        &! in
  !    & coef,            &! in
  !    & addcloud,        &! in
  !    & switchrad,       &! in
  !    & calcemis,        &! in
  !    & emissivity,      &! inout  direct model
  !    & profiles_ad,     &! inout  adjoint
  !    & emissivity_ad,   &! inout  adjoint
  !    & transmission,    &! inout  direct model
  !    & transmission_ad, &! inout  adjoint input
  !    & radiance,    &! inout  direct model   (input due to pointers alloc)
  !    & radiance_ad ) ! inout  adjoint input  (output if converstion Bt -> rad)


    call rttov_ad( &
       & errorstatus,     &! out
       & nprofiles,       &! in
    !   & nchannels,       &! in
       & nbtout,          & ! in
       & channels,        &! in
       & lprofiles,       &! in
       & addinterp,       &! in
       & profiles,        &! in 
       & profiles_ad,     &! in
       & coef,            &! in
       & coef_scatt_ir,   &! in
       & optp,            &! in
       & lsun,            &! in
       & laerosl,         &! in
       & addcloud,        &! in
       & switchrad,       &! in
       & calcemis,        &! in
       & emissivity,      &! inout
       & emissivity_ad,   &! inout
       & transmission,    &! inout
       & transmission_ad, &! inout
       & radiance,        &! inout
       & radiance_ad)      ! inout 
#endif


   ! rttov87 generates warnings we want to ignore
   if (print_detail_rad .and. (any(errorstatus(:) == errorstatus_fatal))) then
       write (message(1),*) 'rttov_direct error code = ', errorstatus(:)
       write (message(2),*) 'nfrequencies            = ', nfrequencies
       write (message(3),*) 'nchannels               = ', nchannels
       write (message(4),*) 'nbtout                  = ', nbtout
       write (message(5),*) 'nprofiles               = ', nprofiles
       write (message(6),*) 'channels                = ', channels
       write (message(7),*) 'polarisations           = ', polarisations
       write (message(8),*) 'lprofiles               = ', lprofiles
       write (message(9),*) 'addcloud                = ', addcloud
       write (message(10),*) 'switchrad              = ', switchrad
       write (message(11),*) 'calcemis               = ', calcemis
       write (message(12),*) 'profiles%s2m           = ', profiles(1)%s2m
       write (message(13),*) 'profiles%skin          = ', profiles(1)%skin
       write (message(14),*) 'profiles%zenangle      = ', profiles(1)%zenangle
       write (message(15),*) 'profiles%azangle       = ', profiles(1)%azangle
       write (message(16),*) 'profiles%p             = ', profiles(1)%p
       write (message(17),*) 'profiles%t             = ', profiles(1)%t
       write (message(18),*) 'profiles%q             = ', profiles(1)%q
       write (message(19),*) 'profiles%o3            = ', profiles(1)%o3
       write (message(20),*) 'profiles%co2           = ', profiles(1)%co2
       write (message(21),*) 'profiles%clw           = ', profiles(1)%clw
       write (message(22),*) 'emissivity             = ', emissivity
       write (message(23),*) 'radiance               = ', radiance%out_clear
       write (message(24),*) 'profiles_ad%s2m        = ', profiles_ad(1)%s2m
       write (message(25),*) 'profiles_ad%skin       = ', profiles_ad(1)%skin
       write (message(26),*) 'profiles_ad%zenangle   = ', profiles_ad(1)%zenangle
       write (message(27),*) 'profiles_ad%azangle    = ', profiles_ad(1)%azangle
       write (message(28),*) 'profiles_ad%p          = ', profiles_ad(1)%p
       write (message(29),*) 'profiles_ad%t          = ', profiles_ad(1)%t
       write (message(30),*) 'profiles_ad%q          = ', profiles_ad(1)%q
       write (message(31),*) 'profiles_ad%o3         = ', profiles_ad(1)%o3
       write (message(32),*) 'profiles_ad%co2        = ', profiles_ad(1)%co2
       write (message(33),*) 'profiles_ad%clw        = ', profiles_ad(1)%clw
       write (message(34),*) 'emissivity_ad          = ', emissivity_ad
       write (message(35),*) 'radiance_ad            = ', radiance_ad%out_clear
       write (message(36),*) 'addinterp              = ', addinterp                            !RTTOV9_3
       write (message(37),*) 'lsun                   = ', lsun                                 !RTTOV9_3
       write (message(38),*) 'laerosl                = ', laerosl                              !RTTOV9_3

      call da_warning(__FILE__,__LINE__,message(1:38))                                         !RTTOV9_3
   end if

   do n = 1, nprofiles
      con_vars_ad(n)%t(:)         = profiles_ad(n) % t(:)
      con_vars_ad(n)%q(:)         = profiles_ad(n) % q(:)
      con_vars_ad(n)%ps           = profiles_ad(n) % s2m  % p
   end do

   deallocate (lprofiles )
   deallocate (channels )
   deallocate (polarisations )
   deallocate (emissivity )
   deallocate (emissivity_ad )
   deallocate (calcemis )
   deallocate (surfem)

   do n = 1, nprofiles 
      deallocate (profiles(n)%p )
      deallocate (profiles(n)%t )
      deallocate (profiles(n)%q )
      deallocate (profiles(n)%o3 )
      deallocate (profiles(n)%co2 )
      deallocate (profiles(n)%clw )

      deallocate (profiles_ad(n)%p )
      deallocate (profiles_ad(n)%t )
      deallocate (profiles_ad(n)%q )
      deallocate (profiles_ad(n)%o3 )
      deallocate (profiles_ad(n)%co2 )
      deallocate (profiles_ad(n)%clw )
!RTTOV9_3
      deallocate (profiles(n)%n2o)
      deallocate (profiles(n)%co)
      deallocate (profiles(n)%ch4)
      deallocate (profiles(n)%aerosols)
      deallocate (profiles(n)%cloud)
      deallocate (profiles(n)%cfrac)

      deallocate (profiles_ad(n)%n2o)
      deallocate (profiles_ad(n)%co)
      deallocate (profiles_ad(n)%ch4)
      deallocate (profiles_ad(n)%aerosols)
      deallocate (profiles_ad(n)%cloud)
      deallocate (profiles_ad(n)%cfrac)

!end of RTTOV9_3

   end do

   ! deallocate transmittance structure
!*********************************************************************************************
!********just transmission % tau_total and transmission % tau_layers need to be deallocated***
!***************because others have been deallocated by the rttov_alloc_transmission.F90******
!*********************************************************************************************

 !  deallocate (transmission % tau_surf      ,stat= alloc_status(6))
 !  deallocate (transmission % tau_layer     ,stat= alloc_status(7))
 !  deallocate (transmission % od_singlelayer,stat= alloc_status(8))
!RTTOV9_3
 
   deallocate (transmission % tau_total ,stat = alloc_status(101))
   deallocate (transmission % tau_layers ,stat = alloc_status(102) )
 
!end of RTTOV9_3

   ! deallocate adjoint transmittance structure
 !  deallocate (transmission_ad % tau_surf      ,stat= alloc_status(106))
 !  deallocate (transmission_ad % tau_layer     ,stat= alloc_status(107))
 !  deallocate (transmission_ad % od_singlelayer,stat= alloc_status(108))
!RTTOV9_3
 
   deallocate (transmission_ad % tau_total, stat = alloc_status(103) )
   deallocate (transmission_ad % tau_layers,stat = alloc_status(104)  )
 
!end of RTTOV9_3

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

   deallocate (radiance_ad % clear    ,stat=alloc_status(109))
   deallocate (radiance_ad % cloudy   ,stat=alloc_status(110))
   deallocate (radiance_ad % total    ,stat=alloc_status(111))
   deallocate (radiance_ad % bt       ,stat=alloc_status(112))
   deallocate (radiance_ad % bt_clear ,stat=alloc_status(113))
   deallocate (radiance_ad % upclear  ,stat=alloc_status(114))
   deallocate (radiance_ad % dnclear  ,stat=alloc_status(115))
   deallocate (radiance_ad % reflclear,stat=alloc_status(116))
   deallocate (radiance_ad % overcast ,stat=alloc_status(117))
   deallocate (radiance_ad % downcld  ,stat=alloc_status(118))
   deallocate (radiance_ad % out       ,stat= alloc_status(119))
   deallocate (radiance_ad % out_clear ,stat= alloc_status(120))
   deallocate (radiance_ad % total_out ,stat= alloc_status(121))
   deallocate (radiance_ad % clear_out ,stat= alloc_status(122))
!RTTOV9_3
   deallocate (radiance_ad % up       ,stat= alloc_status(23))
   deallocate (radiance_ad % down       ,stat= alloc_status(24))
   deallocate (radiance_ad % surf       ,stat= alloc_status(19))
!end of RTTOV9_3

   if (Any(alloc_status /= 0) ) then
     call da_error(__FILE__,__LINE__, &
       (/"mem deallocation error"/))
   End if

   call da_trace_exit("da_rttov_ad")
   

end subroutine da_rttov_ad
#endif

