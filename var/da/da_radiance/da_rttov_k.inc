#ifdef RTTOV
subroutine da_rttov_k(inst, isflg, nchanl, nprofiles, &
                          con_vars, aux_vars, &
                          tb,rad_xb, rad_ovc, emisv, emish, emissivity_out,iv,n)

   !---------------------------------------------------------------------------
   !  PURPOSE: interface to the k matrix subroutine of RTTOV
   !---------------------------------------------------------------------------





#ifdef RTTOV
    Use parkind1, Only : jpim    ,jprb
#endif

   implicit none

#include "rttov_k.interface"

   integer             ,  intent (in) :: inst, isflg, nchanl, nprofiles
   type (con_vars_type),  intent (in) :: con_vars (nprofiles)
   type (aux_vars_type),  intent (in) :: aux_vars (nprofiles)
   real                , intent (inout) :: tb(nchanl)
   real                , intent (inout) :: rad_xb(nchanl)
   real                , intent (inout) :: rad_ovc(nchanl,kts:kte)
   real             ,  intent    (in) :: emisv(nprofiles*nchanl)
   real             ,  intent    (in) :: emish(nprofiles*nchanl)
   real             ,  intent   (out) :: emissivity_out(:)
   type (iv_type),    intent(inout) :: iv       ! O-B structure.  

   ! local variables
   integer             :: nfrequencies, nchannels, nbtout
   integer             :: n, nc, i, ichannel, pol_id, k,l,jp
 ! Integer             :: alloc_status(40)
   Integer             :: alloc_status(140)                            !RTTOV9_3
   Integer(Kind=jpim)  :: nstreams  !RTTOV9_3
   Character (len=80) :: errMessage
   Character (len=10) :: NameOfRoutine = 'rttov_k  '
  
  
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
   logical :: switchrad

   ! RTTOV out parameters
   integer             :: errorstatus(nprofiles)

   ! RTTOV inout parameters
   type(profile_type)  :: profiles_k(nchanl,nprofiles)
   real*8  , pointer        :: emissivity (:)
   real*8  , pointer        :: emissivity_k (:)
   type (radiance_type)     :: radiance
   type (radiance_type)     :: radiance_k
   type (transmission_type) :: transmission
   type (transmission_type) :: transmission_k

   call da_trace_entry("da_rttov_k")

   nchan (:) = nchanl 
   coef = coefs(inst)
   coef_scatt_ir = coefs_scatt_ir(inst)  !RTTOV9_3
   optp = optps(inst)
   addcloud = .false.
   addinterp = .false.
   lsun = .false.
   laerosl = .false.
 
   alloc_status (:) = 0

   do jp = 1, nprofiles
      profiles(jp) % nlevels    = con_vars(jp) % nlevels
      allocate (profiles(jp)%p(profiles(jp) % nlevels), stat=alloc_status(1))
      allocate (profiles(jp)%t(profiles(jp) % nlevels), stat=alloc_status(2))
      allocate (profiles(jp)%q(profiles(jp) % nlevels), stat=alloc_status(3))
      allocate (profiles(jp)%o3(profiles(jp) % nlevels), stat=alloc_status(4))
      allocate (profiles(jp)%co2(profiles(jp) % nlevels), stat=alloc_status(5))
      allocate (profiles(jp)%clw(profiles(jp) % nlevels), stat=alloc_status(6))
!RTTOV9_3
      allocate (profiles(jp)%n2o(profiles(jp) % nlevels), stat=alloc_status(41))
      allocate (profiles(jp)%co(profiles(jp) % nlevels), stat=alloc_status(42))
      allocate (profiles(jp)%ch4(profiles(jp) % nlevels),stat=alloc_status(43))
      allocate (profiles(jp)%aerosols(11,profiles(jp) % nlevels),stat=alloc_status(44))
      allocate (profiles(jp)%cloud(6,profiles(jp) % nlevels),stat=alloc_status(45))
      allocate (profiles(jp)%cfrac(6,profiles(jp) % nlevels),stat=alloc_status(46))
!end of RTTOV9_3

      if (any(alloc_status /= 0)) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to profile",n
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      profiles(jp) % ozone_data = .false.
      profiles(jp) % co2_data   = .false.
      profiles(jp) % clw_data   = .false. 
!RTTOV9_3
      profiles(jp) % n2o_data   = .false.
      profiles(jp) % co_data    = .false.
      profiles(jp) % ch4_data   = .false.
      profiles(jp) % aer_data   = .false.
      profiles(jp) % cld_data   = .false.
!end of RTTOV9_3
      profiles(jp) % p(:)       = coef%ref_prfl_p(:)
      profiles(jp) % t(:)       = con_vars(jp)%t(:)
      profiles(jp) % q(:)       = con_vars(jp)%q(:)
      profiles(jp) % o3(:)      = 0.0 !con_vars(n)%o3(:)
      profiles(jp) % co2(:)     = 0.0 !con_vars(n)%co2(:)
      profiles(jp) % clw(:)     = 0.0 !con_vars(n)%clw(:)
!RTTOV9_3
      profiles(jp) % n2o(:)     = 0.0
      profiles(jp) % co(:)      = 0.0
      profiles(jp) % ch4(:)     = 0.0
      profiles(jp) % aerosols(:,:)   = 0.0
      profiles(jp) % cloud(:,:)      = 0.0
      profiles(jp) % cfrac(:,:)      = 0.0
      profiles(jp) % idg             = 1
      profiles(jp) % ish             = 1
!end of RTTOV9_3    
      !if seap > 0.99 watertype is ocean water                          RTTOV9_3
      profiles(jp) % skin % watertype = 1

      if (isflg == 0 .or. isflg == 4) then  ! sea
         !if seap > 0.99 watertype is ocean water                          RTTOV9_3
         if (isflg == 0) profiles(jp) % skin % watertype = 1
         !if seap < 0.99 watertype is freshwater
         if (isflg == 4) profiles(jp) % skin % watertype = 0           !     end of RTTOV9_3

         profiles(jp) % skin % surftype   = 1 ! aux_vars (n) % surftype
         ! profiles(n) % skin % fastem (:) = 0.0
      else if (isflg == 1 .or. isflg == 5) then  ! sea-ice with snow
         profiles(jp) % skin % surftype   = 2
         ! profiles(n) % skin % fastem (1) = 2.2
         ! profiles(n) % skin % fastem (2) = 3.7
         ! profiles(n) % skin % fastem (3) = 122.0
         ! profiles(n) % skin % fastem (4) = 0.0
         ! profiles(n) % skin % fastem (5) = 0.15
      else
         profiles(jp) % skin % surftype   = 0 ! land (Deep dry snow)
         !    profiles(n) % skin % fastem (1) = 3.0
         !    profiles(n) % skin % fastem (2) = 24.0
         !    profiles(n) % skin % fastem (3) = 60.0
         !    profiles(n) % skin % fastem (4) = 0.1
         !    profiles(n) % skin % fastem (5) = 0.15
      end if
      !    profiles(n) % skin % surftype   = aux_vars (n) % surftype   
      profiles(jp) % skin % t          = aux_vars (jp) % surft    
      profiles(jp) % skin % fastem (:) = 0.0 ! aux_vars (n) % fastem (:)

      profiles(jp) % s2m  % t    = aux_vars (jp) % t2m
      profiles(jp) % s2m  % q    = aux_vars (jp) % q2m
      profiles(jp) % s2m  % o    = 0.0 !aux_vars (n) % o3
      profiles(jp) % s2m  % p    = con_vars (jp) % ps
      profiles(jp) % s2m  % u    = aux_vars (jp) % u10
      profiles(jp) % s2m  % v    = aux_vars (jp) % v10
      profiles(jp) % s2m  % wfetc  =   100000.0              !RTTOV9_3

      profiles(jp) % zenangle    = aux_vars (jp) % satzen
      profiles(jp) % azangle     = aux_vars (jp) % satazi
!RTTOV9_3

      profiles(jp) % sunzenangle = aux_vars (jp) % solzen     !50.0                      !RTTOV9_3
      profiles(jp) % sunazangle  = aux_vars (jp) % solazi     !86.0                      !RTTOV9_3
      profiles(jp) % elevation = 0.001* aux_vars (jp) % elevation   !800
      profiles(jp) % latitude    = aux_vars (jp) % rlat       !35.0    
      profiles(jp) % addsolar    = .false.
      profiles(jp) % addrefrac   = .false.
      profiles(jp) % addaerosl   = .false.
      profiles(jp) % addwatrcl   = .false.
      profiles(jp) % Be          = 0.35
      profiles(jp) % cosbk       = 0.0
!end for RTTOV9_3
      profiles(jp) % ctp         = 500.0
      profiles(jp) % cfraction   = 0.0

   end do


 !allocate and assign profile_k in kmatrix
   do i = 1, nchanl
     do jp = 1,nprofiles
      profiles_k(i,jp) % nlevels  = con_vars(jp) % nlevels
      allocate ( profiles_k(i,jp)%p(profiles_k(i,jp) % nlevels), stat = alloc_status(1) )
      allocate ( profiles_k(i,jp)%t(profiles_k(i,jp) % nlevels), stat = alloc_status(2) )
      allocate ( profiles_k(i,jp)%q(profiles_k(i,jp) % nlevels), stat = alloc_status(3) )
      allocate ( profiles_k(i,jp)%o3(profiles_k(i,jp) % nlevels), stat = alloc_status(4) )
      allocate ( profiles_k(i,jp)%co2(profiles_k(i,jp) % nlevels), stat = alloc_status(5) )
      allocate ( profiles_k(i,jp)%clw(profiles_k(i,jp) % nlevels), stat = alloc_status(6) )
      allocate ( profiles_k(i,jp)%n2o(profiles_k(i,jp) % nlevels), stat = alloc_status(7) )
      allocate ( profiles_k(i,jp)%co(profiles_k(i,jp) % nlevels), stat = alloc_status(9) )
      allocate ( profiles_k(i,jp)%ch4(profiles_k(i,jp) % nlevels), stat = alloc_status(9) )
      allocate ( profiles_k(i,jp)%aerosols(11,profiles_k(i,jp) % nlevels), stat = alloc_status(10) )
      allocate ( profiles_k(i,jp)%cloud(6,profiles_k(i,jp) % nlevels), stat = alloc_status(11) )
      allocate ( profiles_k(i,jp)%cfrac(6,profiles_k(i,jp) % nlevels), stat = alloc_status(12) )
 
      if (Any(alloc_status /= 0) ) then
         WRITE(UNIT=message(1),FMT='(A,I5)') &
            "mem allocation error to profiles_k",jp
         call da_error(__FILE__,__LINE__,message(1:1))
      End if
 
      profiles_k(i,jp) % ozone_Data = .False.  ! no meaning
      profiles_k(i,jp) % co2_Data   = .False.  ! no meaning
      profiles_k(i,jp) % clw_Data   = .False.  ! no meaning
      profiles_k(i,jp) % zenangle   = -1       ! no meaning
      profiles_k(i,jp) % p(:)   = 0.0 ! no AD on pressure levels
      profiles_k(i,jp) % t(:)   = 0.0 ! temperarure
      profiles_k(i,jp) % o3(:)  = 0.0 ! O3 ppmv
      profiles_k(i,jp) % clw(:) = 0.0 ! clw
      profiles_k(i,jp) % q(:)   = 0.0 ! WV
      profiles_k(i,jp) % s2m % t = 0.0 ! temperarure
      profiles_k(i,jp) % s2m % q = 0      ! WV
      profiles_k(i,jp) % s2m % p = 0.0 ! pressure
      profiles_k(i,jp) % s2m % u = 0.0 ! wind components
      profiles_k(i,jp) % s2m % v = 0.0 ! wind components
      profiles_k(i,jp) % skin % surftype = -1  ! no meaning
      profiles_k(i,jp) % skin % t        = 0.0  ! on temperarure
      profiles_k(i,jp) % skin % fastem   = 0.0  ! Fastem
      profiles_k(i,jp) % ctp       = 0.0  ! cloud top pressure
      profiles_k(i,jp) % cfraction = 0.0  ! cloud fraction
      profiles_k(i,jp) % n2o_data   = .false.
      profiles_k(i,jp) % co_data    = .false.
      profiles_k(i,jp) % ch4_data   = .false.
      profiles_k(i,jp) % aer_data   = .false.
      profiles_k(i,jp) % cld_data   = .false.
      profiles_k(i,jp) % n2o(:)     = 0.0
      profiles_k(i,jp) % co(:)      = 0.0
      profiles_k(i,jp) % ch4(:)     = 0.0
      profiles_k(i,jp) % aerosols(:,:)   = 0.0
      profiles_k(i,jp) % cloud(:,:)      = 0.0
      profiles_k(i,jp) % cfrac(:,:)      = 0.0
      profiles_k(i,jp) % idg             = -1_jpim
      profiles_k(i,jp) % ish             = -1_jpim
      profiles_k(i,jp) % skin % watertype = -1
      profiles_k(i,jp) % s2m  % wfetc  =  0.0              !RTTOV9_3 100000.0
      profiles_k(i,jp) % sunzenangle = -1                          !RTTOV9_3
      profiles_k(i,jp) % sunazangle  = -1                           !RTTOV9_3
      profiles_k(i,jp) % elevation   = -1
      profiles_k(i,jp) % latitude    = -1
      profiles_k(i,jp) % addsolar    = .false.
      profiles_k(i,jp) % addrefrac   = .false.
      profiles_k(i,jp) % addaerosl   = .false.
      profiles_k(i,jp) % addwatrcl   = .false.
      profiles_k(i,jp) % Be          = -1
      profiles_k(i,jp) % cosbk       = -1
 
 
!end of allocate and assign profile_k in kmatrix
  end do  ! end of nprofiles

 end do   ! end of nchanl

!#ifdef RTTOV


 call da_rttov_setupchan(nprofiles, nchan, coef, &             ! in
                   nfrequencies, nchannels, nbtout)       ! out
   nchannels = nfrequencies           !RTTOV9_3
!#endif


   allocate (lprofiles(nfrequencies), stat = alloc_status(31))
   allocate (channels (nfrequencies), stat = alloc_status(32))
   allocate (polarisations(nchannels, 3), stat = alloc_status(33))
   allocate (emissivity(nchannels), stat = alloc_status(34))
   allocate (emissivity_k(nchannels), stat = alloc_status(134))
   allocate (calcemis(nchannels), stat = alloc_status(35))
   allocate (surfem(nchannels), stat = alloc_status(36))

   emissivity = 0.0
  
   nstreams = 0_jpim  !RTTOV9_3
   ! allocate transmittance structure
   allocate (transmission % tau_surf     (0 : nstreams,nchannels)        ,stat= alloc_status(8))
   allocate (transmission % tau_layer  (coef % nlevels, 0 :  nstreams, nchannels) ,    stat= alloc_status(9))
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
   
 !allocate and assign transmissione_k in kmatrix
   allocate (transmission_k % tau_surf      (0 : nstreams,nchannels)        ,stat= alloc_status(8))
   allocate (transmission_k % tau_layer     (coef % nlevels, 0 : nstreams, nchannels) ,  stat= alloc_status(9))
   allocate (transmission_k % od_singlelayer(coef % nlevels, 0 : nstreams, nchannels ),  stat= alloc_status(10))
!RTTOV9_3
   allocate (transmission_k % tau_total(nchannels),  stat = alloc_status(51))
   allocate (transmission_k % tau_layers(coef % nlevels, nchannels), stat = alloc_status(52))
   allocate (transmission_k % od_layer (coef % nlevels,0 : nstreams, nchannels), stat = alloc_status(53))
   allocate (transmission_k % odsun_singlelayer(coef % nlevels, 0 : nstreams, nchannels), & 
           stat = alloc_status(54))
   allocate (transmission_k % tausun_surf ( 0 : nstreams, nchannels),stat = alloc_status(55))
   allocate (transmission_k % tausun_layer( coef % nlevels,0 :  nstreams, nchannels), stat = alloc_status(56))
   allocate (transmission_k % od_sfrac( 0 : nstreams, nchannels), stat = alloc_status(57))
   allocate (transmission_k % odsun_sfrac( 0 : nstreams, nchannels), stat = alloc_status(58))
   allocate (transmission_k % od_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(59))
   allocate (transmission_k % odsun_frac_ac( 0 : nstreams, nchannels), stat = alloc_status(60))
   allocate (transmission_k % tau_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(61))
   allocate (transmission_k % tau_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(62))
   allocate (transmission_k % tau_ref_surf_ac( 0 : nstreams, nchannels), stat = alloc_status(63))
   allocate (transmission_k % tau_ref_surf_acsun( 0 : nstreams, nchannels), stat = alloc_status(64))
   allocate (transmission_k % od_frac_t( 0 : nstreams, nchannels), stat = alloc_status(65))
   allocate (transmission_k % odsun_frac_t( 0 : nstreams, nchannels), stat = alloc_status(66))
   allocate (transmission_k % tau_surf_t( 0 : nstreams, nchannels), stat = alloc_status(67))
   allocate (transmission_k % tausun_surf_t( 0 : nstreams, nchannels), stat = alloc_status(68))
   allocate (transmission_k % tau_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(69))
   allocate (transmission_k % tausun_ref_surf_t( 0 : nstreams, nchannels), stat = alloc_status(70))

!end of RTTOV9_3
   transmission_k % tau_surf(:,:)   = 0.0
   transmission_k % tau_layer(:,:,:)= 0.0
   transmission_k % od_singlelayer(:,:,:) = 0.0
!RTTOV9_3
   transmission_k % tau_total         = 0.0
   transmission_k % tau_layers        = 0.0
   transmission_k % od_layer          = 0.0
   transmission_k % odsun_singlelayer = 0.0
   transmission_k % tausun_surf       = 0.0
   transmission_k % tausun_layer      = 0.0
   transmission_k % od_sfrac          = 0.0
   transmission_k % odsun_sfrac       = 0.0
   transmission_k % od_frac_ac        = 0.0
   transmission_k % odsun_frac_ac     = 0.0
   transmission_k % tau_surf_ac       = 0.0
   transmission_k % tau_surf_acsun    = 0.0
   transmission_k % tau_ref_surf_ac   = 0.0
   transmission_k % tau_ref_surf_acsun = 0.0
   transmission_k % od_frac_t         = 0.0
   transmission_k % odsun_frac_t      = 0.0
   transmission_k % tau_surf_t        = 0.0
   transmission_k % tausun_surf_t     = 0.0
   transmission_k % tau_ref_surf_t    = 0.0
   transmission_k % tausun_ref_surf_t = 0.0
!end of RTTOV9_3

  
 !end of allocate and assign transmissione_k in kmatrix

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
         (/"mem allocation error prior to rttov_k"/))
   end if

  ! allocate radiance results arrays with number of channels
   allocate (radiance_k % clear    ( nchannels ) ,stat= alloc_status(111))
   allocate (radiance_k % cloudy   ( nchannels ) ,stat= alloc_status(112))
   allocate (radiance_k % total    ( nchannels ) ,stat= alloc_status(113))
   allocate (radiance_k % bt       ( nchannels ) ,stat= alloc_status(114))
   allocate (radiance_k % bt_clear ( nchannels ) ,stat= alloc_status(115))
   allocate (radiance_k % upclear  ( nchannels ) ,stat= alloc_status(116))
   allocate (radiance_k % dnclear  ( nchannels ) ,stat= alloc_status(117))
   allocate (radiance_k % reflclear( nchannels ) ,stat= alloc_status(118))
   allocate (radiance_k % overcast ( coef % nlevels, nchannels ) ,stat= alloc_status(119))
   ! allocate the cloudy radiances with full size even
   ! if not used
   allocate (radiance_k % downcld  ( coef % nlevels, nchannels ) ,stat= alloc_status(120))
 
   allocate (radiance_k % out      ( nbtout ) ,stat= alloc_status(121))
   allocate (radiance_k % out_clear( nbtout ) ,stat= alloc_status(122))
   allocate (radiance_k % total_out( nbtout ) ,stat= alloc_status(123))
   allocate (radiance_k % clear_out( nbtout ) ,stat= alloc_status(124))
 
!RTTOV9_3
  allocate (radiance_k % up(coef % nlevels, nchannels) ,stat= alloc_status(81))
  allocate (radiance_k % down(coef % nlevels, nchannels) ,stat= alloc_status(82))
  allocate (radiance_k % surf(coef % nlevels, nchannels) ,stat= alloc_status(83))
!end of RTTOV9_3
 
 
   if ( Any(alloc_status /= 0) ) then
      call da_error(__FILE__,__LINE__, &
         (/"mem allocation error prior to rttov_k"/))
   End if
 
   ! initialise all radiance increments to 0
   radiance_k % clear(:)      = 0.0
   radiance_k % clear_out(:)  = 0.0
   radiance_k % cloudy(:)     = 0.0
   radiance_k % total(:)      = 0.0
   radiance_k % total_out(:)  = 0.0
   radiance_k % bt(:)         = 0.0
   radiance_k % bt_clear(:)   = 0.0   !need to check,orginal value = 1.0 maybe 0 is correct
   radiance_k % out(:)        = 0.0
   radiance_k % out_clear(:)  = 0.0
   radiance_k % upclear(:)    = 0.0
   radiance_k % reflclear(:)  = 0.0
   radiance_k % overcast(:,:) = 0.0
   radiance_k % downcld(:,:)  = 0.0
!RTTOV9_3
   radiance_k % dnclear  = 0.0
   radiance_k % up        = 0.0
   radiance_k % down      = 0.0
   radiance_k % surf      = 0.0
!end of RTTOV9_3


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
         emissivity_k (1:nchannels) = 0.0             
      else if (profiles(1) % skin % surftype == 2) then
         calcemis (1:nchannels)   = .false.          
         emissivity (1:nchannels) = 0.98             ! over sea-ice
         emissivity_k (1:nchannels) = 0.0
      else                                        
         if (isflg == 2 .or. isflg == 6) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 0.97           ! land without snow
            emissivity_k (1:nchannels) = 0.0
         end if
         if (isflg == 3 .or. isflg == 7) then
            calcemis (1:nchannels)   = .false.
            emissivity (1:nchannels) = 1.0            ! land with snow
            emissivity_k (1:nchannels) = 0.0
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
            emissivity_k (1:nchannels) = 0.0
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
            emissivity_k (i) = 0.0
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
            emissivity_k (i) = 0.0                                  
!END OF RTTOV9_3                                                           
        end do
      end if
   end if

   !-----------------------------------
   !  calling RTTOV k matrix
   !----------------------------------

#ifdef RTTOV
   switchrad= .true.


   call rttov_k(&
        & errorstatus,       &! out
          & nprofiles,         &! in
          & nchannels,         &! in       if nchannels runtime error
          & channels,          &! in
          & lprofiles,         &! in
          & addinterp,         &! in
          & profiles,          &! in
          & profiles_k,         &! inout  adjoint
          & coef,              &! in
          & coef_scatt_ir,     &! in
          & optp,              &! in
          & lsun,              &! in
          & laerosl,           &! in
          & addcloud,          &! in
          & switchrad,          &! in
          & calcemis,          &! in
          & emissivity,        &! inout
          & emissivity_k,       &! inout  adjoint
          & transmission,      &! inout
          & transmission_k,     &! inout  adjoint
          & radiance)           ! inout



   
#endif

        If( any(errorstatus == errorstatus_fatal ) ) then
          Do jp = 1, nprofiles
              If( errorstatus(jp) == errorstatus_fatal ) then
                 Write( errMessage, '( "fatal error in rttov_k")' )
                 Call Rttov_ErrorReport (errorstatus(jp), errMessage, NameOfRoutine)
              End If
           End Do
           Stop
        End If
       If( any( errorstatus /= errorstatus_success ) ) then
          Write( errMessage, '( "warning in rttov_k")' )
        End If


       do l = 1, nchannels
 
               iv%instid(inst)%ts_jacobian(l,n) = profiles_k(l,1)%skin%t
               iv%instid(inst)%emiss_jacobian(l,n) = emissivity_k(l)
      end do
   


! PS Jacobian
            do l=1,nchanl
               iv%instid(inst)%ps_jacobian(l,n)  = profiles_k(l,1)%s2m%p
            end do

! T, Q Jacobian
            do k=1,profiles_k(1,1)%nlevels
               do l=1,nchanl
                  iv%instid(inst)%t_jacobian(l,k,n) = profiles_k(l,1)%t(k)
                  iv%instid(inst)%q_jacobian(l,k,n) = profiles_k(l,1)%q(k)
               end do
            end do

   



             
             !     do l=1,nchanl
            !         iv%instid(inst)%water_jacobian(l,k,n) =   &
           !             profiles_k(l,1) % cloud(1,k) 
          !           iv%instid(inst)%ice_jacobian(l,k,n) =     &
         !               profiles_k(l,1) % cloud(2,k)
        !             iv%instid(inst)%rain_jacobian(l,k,n) =    &
       !                 profiles_k(l,1) % cloud(3,k)
      !               iv%instid(inst)%snow_jacobian(l,k,n) =    &
     !                   profiles_k(l,1) % cloud(4,k)
    !                 iv%instid(inst)%graupel_jacobian(l,k,n) = &
   !                     profiles_k(l,1) % cloud(5,k)
  !                   iv%instid(inst)%hail_jacobian(l,k,n) =    &
 !                       profiles_k(l,1) % cloud(6,k)
!
  !                end do
!end of Jacobian

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
   do jp = 1, nprofiles
      !out_clear is 0 in RTTOV9_3,bt_clear is right
      !tb(1:nc) = radiance % out_clear((jp-1)*nc+1:jp*nc)
      tb(1:nc) = radiance % bt_clear((jp-1)*nc+1:jp*nc)
      rad_xb(1:nc) = radiance % clear((jp-1)*nc+1:jp*nc)
      do k = kts, kte
         rad_ovc(1:nc,k) = radiance % overcast(k,(jp-1)*nc+1:jp*nc)
      end do	
   end do

!  calcemis_out  (:) = calcemis(:)
   emissivity_out(:) = emissivity(:)






   deallocate (lprofiles)
   deallocate (channels)
   deallocate (polarisations)
   deallocate (emissivity)
   deallocate (emissivity_k)
   deallocate (calcemis)
   deallocate (surfem)
   do jp = 1, nprofiles 
      deallocate (profiles(jp)%p)
      deallocate (profiles(jp)%t)
      deallocate (profiles(jp)%q)
      deallocate (profiles(jp)%o3)
      deallocate (profiles(jp)%co2)
      deallocate (profiles(jp)%clw)
!RTTOV9_3
      deallocate (profiles(jp)%n2o)
      deallocate (profiles(jp)%co)
      deallocate (profiles(jp)%ch4)
      deallocate (profiles(jp)%aerosols)
      deallocate (profiles(jp)%cloud)
      deallocate (profiles(jp)%cfrac)

!end of RTTOV9_3
   do l = 1, nchanl
      deallocate (profiles_k(l,jp)%p)
      deallocate (profiles_k(l,jp)%t)
      deallocate (profiles_k(l,jp)%q)
      deallocate (profiles_k(l,jp)%o3)
      deallocate (profiles_k(l,jp)%co2)
      deallocate (profiles_k(l,jp)%clw)
      deallocate (profiles_k(l,jp)%n2o)
      deallocate (profiles_k(l,jp)%co)
      deallocate (profiles_k(l,jp)%ch4)
      deallocate (profiles_k(l,jp)%aerosols)
      deallocate (profiles_k(l,jp)%cloud)
      deallocate (profiles_k(l,jp)%cfrac)
    end do
    
   end do

   ! deallocate transmittance structure
!**************************************************************************************************
!********just transmission % tau_total and transmission % tau_layers need to be deallocated********
!********************because others have been deallocated by the rttov_alloc_transmission.F90******
!**************************************************************************************************



   deallocate (transmission % tau_total ,stat= alloc_status(91))
   deallocate (transmission % tau_layers,stat= alloc_status(92)  )
   
   deallocate (transmission_k % tau_total ,stat= alloc_status(91))
   deallocate (transmission_k % tau_layers,stat= alloc_status(92)  )


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

   deallocate (radiance_k % clear    ,stat=alloc_status(9))
   deallocate (radiance_k % cloudy   ,stat=alloc_status(10))
   deallocate (radiance_k % total    ,stat=alloc_status(11))
   deallocate (radiance_k % bt       ,stat=alloc_status(12))
   deallocate (radiance_k % bt_clear ,stat=alloc_status(13))
   deallocate (radiance_k % upclear  ,stat=alloc_status(14))
   deallocate (radiance_k % dnclear  ,stat=alloc_status(15))
   deallocate (radiance_k % reflclear,stat=alloc_status(16))
   deallocate (radiance_k % overcast ,stat=alloc_status(17))
   deallocate (radiance_k % downcld  ,stat=alloc_status(18))
   deallocate (radiance_k % out       ,stat= alloc_status(19))
   deallocate (radiance_k % out_clear ,stat= alloc_status(20))
   deallocate (radiance_k % total_out ,stat= alloc_status(21))
   deallocate (radiance_k % clear_out ,stat= alloc_status(22))
   deallocate (radiance_k % up       ,stat= alloc_status(23))
   deallocate (radiance_k % down       ,stat= alloc_status(24))
   deallocate (radiance_k % surf       ,stat= alloc_status(19))



   if (any(alloc_status /= 0)) then
      call da_error(__FILE__,__LINE__, &
        (/"mem deallocation error"/))
   end if

   call da_trace_exit("da_rttov_direct")
   
end subroutine da_rttov_k
#endif
