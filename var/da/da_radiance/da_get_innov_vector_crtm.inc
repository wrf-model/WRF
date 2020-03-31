subroutine da_get_innov_vector_crtm ( it, grid, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate grid%xb to obs location
   !       2. call foreward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !  HISTORY: 12/15/2008 effective radius unit is micron     Zhiquan Liu
   !---------------------------------------------------------------------------

   implicit none
   
   integer,           intent(in)    :: it       ! External iteration.
   type (domain),     intent(in)    :: grid     ! first guess state.
   type (y_type),     intent(inout) :: ob       ! Observation structure.
   type (iv_type),    intent(inout) :: iv       ! O-B structure.

   integer, parameter             :: AIRS_Max_Channels = 281

   integer :: n, icld  ! Loop counter.
   integer :: i, j, k  ! Index dimension.
   integer :: l        ! Index dimension.
   integer :: num_levs ! Number of obs levels.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   integer :: alloc_status(40)

   real, allocatable :: model_u10(:)
   real, allocatable :: model_v10(:)
   real, allocatable :: model_psfc(:)
   real              :: model_ptop
   real, allocatable :: model_ts(:)
   real, allocatable :: model_elv(:)
   real, allocatable :: model_smois(:)
   real, allocatable :: model_tslb(:)
   real, allocatable :: model_snowh(:)
   real, allocatable :: model_vegfra(:)
   real    :: model_isltyp, model_ivgtyp
   integer :: model_isflg

   real    :: model_qcw(kms:kme)
   real    :: model_rho(kms:kme)
   real, allocatable :: model_snow(:)  ! snow water equivalent, different from model_snowh,
                                       ! used in calculating reff_water
   real, allocatable :: em_mspps(:)    ! emissivity caluclated using MSPPS algorithm
   real              :: ts_mspps       ! surface temperature calcualted using MSPPS algorithm
   character(len=19) :: date_char

   integer :: month, crtm_climat

   ! real    :: model_tm(kms:kme)

   integer :: inst, nchanl, n1,n2
   integer :: ipred, npred, gammapred

   ! variables for computing clwp
   real    :: clw(kms:kme), dpf(kms:kme)
   real    :: clwp

   real    :: total_od

   ! CRTM local varaibles and types
   integer :: Allocate_Status
   integer :: n_layers, n_absorbers, n_clouds, n_aerosols
!! for ecmwf cloud
   real    :: t_tropp(ims:ime,jms:jme),temp1
   integer :: k_tropp(ims:ime,jms:jme)
   type (CRTM_RTSolution_type), allocatable :: RTSolution(:,:)
   type (CRTM_Atmosphere_type)   :: Atmosphere(1)
   type (CRTM_Surface_type)      :: Surface(1)
   type (CRTM_Geometry_type)     :: GeometryInfo(1)
   type (CRTM_Options_type)      :: Options(1)
   type (CRTM_RTSolution_type), allocatable :: RTSolution_K(:,:)
   type (CRTM_Atmosphere_type), allocatable :: Atmosphere_K(:,:)
   type (CRTM_Surface_type),    allocatable :: Surface_K(:,:)

   real :: t(1), a(1), p(1)

   integer, allocatable :: wrf_to_crtm_mw(:)
   integer :: n_vegtype

!! for crtm cloud
   real :: qcw(1),qrn(1), qci(1),qsn(1),qgr(1)
   logical :: calc_tb_clr
   type (CRTM_RTSolution_type), allocatable :: RTSolution_clr(:,:)

   ! Initializations for Cloud Detection
   integer           :: ilev, jlev, nclouds
   real, allocatable :: hessian(:,:)
   real*8, allocatable :: eignvec(:,:), eignval(:)
   real              :: rad_clr, rad_ovc_ilev, rad_ovc_jlev
   
   integer           :: Band_Size(5), Bands(AIRS_Max_Channels,5) 
  
      Band_Size(1:5) = (/86, 0, 0, 16, 0 /)
      Bands(:,:)     = 0  
      Bands(1:Band_Size(1),1) = &
&    (/                                                 &              !&      1,   6,   7,  10,  11,  15,  16,  17,  20,  21, &
&                                                       &              !&     22,  24,  27,  28,  30,  36,  39,  40,  42,  51, &
&                                                       &              !&     52,  54,  55,  56,  59,  62,  63,  68,  69,  71, &
&                                                       &              !&     72,  73,  74,  75,  76,  77,  78,  79,  80,  82, &
&                     92,  93,  98,  99, 101, 104, 105, &              !&     83,  84,  86,  92,  93,  98,  99, 101, 104, 105, &
&     108, 110, 111, 113, 116, 117, 123, 124, 128, 129, &
&     138, 139, 144, 145, 150, 151, 156, 157, 159, 162, &
&     165, 168, 169, 170, 172, 173, 174, 175, 177, 179, &
&     180, 182, 185, 186, 190, 192,      198, 201, 204, &              !&     180, 182, 185, 186, 190, 192, 193, 198, 201, 204, &
&     207, 210,      215, 216,      221,      226, 227, &              !&     207, 210, 213, 215, 216, 218, 221, 224, 226, 227, &
&     232,                     252, 253, 256, 257, 261, &              !&     232, 239, 248, 250, 251, 252, 253, 256, 257, 261, &
&     262, 267, 272, 295, 299,      305,           310, &              !&     262, 267, 272, 295, 299, 300, 305, 308, 309, 310, &
&          321, 325, 333, 338, 355, 362, 375, 453, 475, &              !&     318, 321, 325, 333, 338, 355, 362, 375, 453, 475, &
&     484, 497, 528, 587, 672, 787, 791, 843, 870, 914, &
&     950 /)

!      Bands(1:Band_Size(2),2) = &
!&    (/ 1003, 1012, 1019, 1024, 1030, 1038, 1048, 1069, 1079, 1082,  &
!&       1083, 1088, 1090, 1092, 1095, 1104, 1111, 1115, 1116, 1119,  &
!&       1120, 1123, 1130, 1138, 1142, 1178, 1199, 1206, 1221, 1237,  &
!&       1252, 1260, 1263, 1266, 1278, 1285 /)

!      Bands(1:Band_Size(3),3) = &
!&    (/       1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  !&    1290, 1301, 1304, 1329, 1371, 1382, 1415, 1424, 1449, 1455, &  
!&       1466,       1477,             1500, 1519,       1538, 1545, &  !&    1466, 1471, 1477, 1479, 1488, 1500, 1519, 1520, 1538, 1545, &  
!&       1565, 1574, 1583, 1593,       1627, 1636,       1652, 1669, &  !&    1565, 1574, 1583, 1593, 1614, 1627, 1636, 1644, 1652, 1669, & 
!&                   1694, 1708,       1723, 1740, 1748,       1756, &  !&    1674, 1681, 1694, 1708, 1717, 1723, 1740, 1748, 1751, 1756, &
!&             1766, 1771, 1777,       1783, 1794, 1800,       1806, &  !&    1763, 1766, 1771, 1777, 1780, 1783, 1794, 1800, 1803, 1806, &
!&             1826, 1843  /)                                           !&    1812, 1826, 1843  /)

      Bands(1:Band_Size(4),4) = &
&    (/ 1852, 1865, 1866,       1868, 1869, 1872, 1873,       1876, &  !&    1852, 1865, 1866, 1867, 1868, 1869, 1872, 1873, 1875, 1876, 
&             1881, 1882, 1883,                   1911, 1917, 1918, &  !&    1877, 1881, 1882, 1883, 1884, 1897, 1901, 1911, 1917, 1918, &
&                   1924, 1928        /)                               !&    1921, 1923, 1924, 1928, 1937  /)   

!      Bands(1:Band_Size(5),5) = &
!&    (/ 1938, 1939, 1941, 1946, 1947, 1948, 1958, 1971, 1973, 1988, &
!&       1995, 2084, 2085, 2097, 2098, 2099, 2100, 2101, 2103, 2104, &
!&       2106, 2107, 2108, 2109, 2110, 2111, 2112, 2113, 2114, 2115, &
!&       2116, 2117, 2118, 2119, 2120, 2121, 2122, 2123, 2128, 2134, &
!&       2141, 2145, 2149, 2153, 2164, 2189, 2197, 2209, 2226, 2234, &
!&       2280, 2318, 2321, 2325, 2328, 2333, 2339, 2348, 2353, 2355, &
!&       2363, 2370, 2371, 2377  /)  

   ! WHY? use argument it
   if (it==0) then; write(unit=stdout,fmt='(A)') "WHY? have argument it to"//__FILE__; end if

   alloc_status (:) = 0

   if (trace_use) call da_trace_entry("da_get_innov_vector_crtm")

   ! CRTM allocation

   ! CRTM Atmosphere
   n_layers    = (kte-kts)+1   ! number of vertical levels
   n_absorbers = 2
   n_aerosols  = 0
   n_clouds    = 0
   if ( crtm_cloud ) n_clouds = 6

   call CRTM_Atmosphere_Create ( Atmosphere(1), &
                                 n_layers,      &
                                 n_absorbers,   &
                                 n_clouds,      &
                                 n_aerosols )
   IF ( .NOT. CRTM_Atmosphere_Associated( Atmosphere(1) ) ) THEN 
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocating CRTM Atmosphere Structure"/))
   END IF 

   Atmosphere(1)%Absorber_ID(1)=H2O_ID
   Atmosphere(1)%Absorber_ID(2)=O3_ID
   Atmosphere(1)%Absorber_Units(1) = MASS_MIXING_RATIO_UNITS
   Atmosphere(1)%Absorber_Units(2) = VOLUME_MIXING_RATIO_UNITS

   if (crtm_cloud) then
      Atmosphere(1)%Cloud(1)%Type=WATER_CLOUD
      Atmosphere(1)%Cloud(2)%Type=ICE_CLOUD
      Atmosphere(1)%Cloud(3)%Type=RAIN_CLOUD
      Atmosphere(1)%Cloud(4)%Type=SNOW_CLOUD
      Atmosphere(1)%Cloud(5)%Type=GRAUPEL_CLOUD
      Atmosphere(1)%Cloud(6)%Type=HAIL_CLOUD
   end if

   select case (trim(grid%mminlu))
      case ('USGS')
         n_vegtype = USGS_n_type
      case ('MODIFIED_IGBP_MODIS_NOAH')
         n_vegtype = IGBP_n_type
      case default
         call da_error(__FILE__,__LINE__,(/"Unknown land use type"/))
   end select
   allocate(wrf_to_crtm_mw(n_vegtype))
   if ( n_vegtype == USGS_n_type ) then
         wrf_to_crtm_mw = usgs_to_crtm_mw
   else if ( n_vegtype == IGBP_n_type ) then
         wrf_to_crtm_mw = igbp_to_crtm_mw
   end if

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------
   do inst = 1, iv%num_inst                 ! loop for sensor
      ! if ( iv%instid(inst)%num_rad < 1 ) cycle
      if (iv%instid(inst)%info%n2 < iv%instid(inst)%info%n1) cycle
      num_levs  = kte-kts+1 
      nchanl    = ChannelInfo(inst)%n_channels

      if ( ChannelInfo(inst)%Sensor_Type == 2 ) then  ! INFRARED_SENSOR=2
         if ( index(grid%mminlu, trim(CRTM_IRlandCoeff_Classification())) <= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"modify crtm_irland_coef to use the coef file that is consistent with your model land use type"/))
         end if
      end if

      ! Allocate forward model solution RTSolution array to number of channels
      allocate (RTSolution(ChannelInfo(inst)%n_Channels,1), STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, (/"Error in allocating RTSolution"/))
      end if

      call CRTM_RTSolution_Create( RTSolution , &                  !Output
                                   Atmosphere(1)%n_layers )        !Input
      if ( .NOT.(ANY(crtm_rtsolution_associated(RTSolution))) ) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in allocating CRTM RTSolution Stucture"/))
      end if

      calc_tb_clr = .false.
      if ( crtm_cloud .and. &
           trim( crtm_sensor_name(rtminit_sensor(inst))) == 'amsr2' ) then
         !Tb_clear_sky is only needed for symmetric obs error model
         !symmetric obs error model only implemented for amsr2 for now
         calc_tb_clr = .true.
      end if

      if ( calc_tb_clr ) then
         allocate (RTSolution_clr(ChannelInfo(inst)%n_Channels,1), STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, (/"Error in allocating RTSolution_clr"/))
         end if
         call CRTM_RTSolution_Create( RTSolution_clr , &              !Output
                                      Atmosphere(1)%n_layers )        !Input
         if ( .NOT.(ANY(crtm_rtsolution_associated(RTSolution_clr))) ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in allocating CRTM RTSolution_clr Stucture"/))
         end if
      end if !calc_tb_clr

      if (use_crtm_kmatrix) then
         allocate (RTSolution_K(ChannelInfo(inst)%n_Channels,1), &
                   Atmosphere_K(ChannelInfo(inst)%n_Channels,1), &
                   Surface_K(ChannelInfo(inst)%n_Channels,1),    &
                   STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, (/"Error in allocating RTSolution_K"/))
         end if
         call CRTM_RTSolution_Create( RTSolution_K , &                !Output
                                      Atmosphere(1)%n_layers )        !Input
         if ( .NOT.(ANY(crtm_rtsolution_associated(RTSolution_K))) ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in allocating CRTM RTSolution_K Stucture"/))
         end if
      end if
      
      call CRTM_Surface_Create ( Surface(1), & ! Output
                                 nchanl )      ! Input
      IF ( .NOT. CRTM_Surface_Associated( Surface(1) ) ) THEN 
         call da_error(__FILE__,__LINE__, &
            (/"Error in allocating CRTM Surface Structure"/))
      end if

      ! CRTM Options structure
      call CRTM_Options_Create( Options(1), &  ! Output
                                nchanl )       ! Input
      IF ( .NOT. CRTM_Options_Associated( Options(1) ) ) THEN 
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Options Structure"/))
      endif
      if ( use_antcorr(inst) ) Options(1)%Use_Antenna_Correction = .true.

      ! do n= 1, iv%instid(inst)%num_rad           ! loop for pixel

      n1 = iv%instid(inst)%info%n1
      n2 = iv%instid(inst)%info%n2

      allocate (model_u10(n1:n2))
      allocate (model_v10(n1:n2))
      allocate (model_psfc(n1:n2))
      allocate (model_ts(n1:n2))
      allocate (model_elv(n1:n2))
      allocate (model_smois(n1:n2))
      allocate (model_tslb(n1:n2))
      allocate (model_snowh(n1:n2))
      allocate (model_snow(n1:n2))
      allocate (model_vegfra(n1:n2))

      model_u10(:)    = 0.0
      model_v10(:)    = 0.0
      model_psfc(:)   = 0.0
      model_ts(:)     = 0.0
      model_elv(:)    = 0.0
      model_smois(:)  = 0.0
      model_tslb(:)   = 0.0
      model_snowh(:)  = 0.0
      model_snow(:)   = 0.0
      model_vegfra(:) = 0.0

      ! Gamma correction from VarBC
      !----------------------------
!#ifdef CRTM_MODIF
      if ( use_varbc .or. freeze_varbc ) then
         gammapred = iv%instid(inst)%varbc_info%gammapred 
         do k = 1, nchanl
            npred = iv%instid(inst)%varbc(k)%npred
            if (npred <= 0) cycle                                           ! VarBC channels only
            if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle     ! Gamma channels only
            if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle     ! Gamma channels only
	    do ipred = 1, npred
	       if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
	       RTSolution(k,1)%Gamma = iv%instid(inst)%varbc(k)%param(ipred)
            end do		  
         end do
      end if
      RTSolution_K(:,1)%Gamma = 0.0
!#endif

      if ( use_mspps_emis(inst) ) allocate(em_mspps(nchanl))

      pixel_loop: do n=n1,n2
         ! if ( n > iv%instid(inst)%num_rad ) exit

         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%info%i(1,n)
         j = iv%instid(inst)%info%j(1,n)
         dx = iv%instid(inst)%info%dx(1,n)
         dy = iv%instid(inst)%info%dy(1,n)
         dxm = iv%instid(inst)%info%dxm(1,n)
         dym = iv%instid(inst)%info%dym(1,n)

         ! determine CRTM climatology based on latitude and month
         date_char=iv%instid(inst)%info%date_char(n)
         read(unit=date_char(6:7), fmt='(i2.2)') month
         call da_det_crtm_climat(iv%instid(inst)%info%lat(1,n), month, crtm_climat)
         iv%instid(inst)%crtm_climat(n) = crtm_climat
         Atmosphere(1)%Climatology = iv%instid(inst)%crtm_climat(n)

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp ( grid%xb%snow, grid%xb%xice, grid%xb%landmask,  &
            grid%xb%ivgtyp, grid%xb%isltyp, &
            ims, ime, jms, jme, &
            i, j, dx, dy, dxm, dym, &
            model_isflg,model_ivgtyp, model_isltyp, &
            Surface(1)%Water_Coverage, Surface(1)%Ice_Coverage, &
            Surface(1)%Land_Coverage, Surface(1)%Snow_Coverage )

         call da_interp_2d_partial (grid%xb%snow, iv%instid(inst)%info, 1, n, n, model_snow(n:n))

         ! [1.2] Interpolate horizontally to ob:
         do k=kts,kte ! from bottom to top
            call da_interp_2d_partial (grid%xb%p(:,:,k), iv%instid(inst)%info,k,n,n,p)
            call da_interp_2d_partial (grid%xb%t(:,:,k), iv%instid(inst)%info,k,n,n,t)
            call da_interp_2d_partial (grid%xb%q(:,:,k), iv%instid(inst)%info,k,n,n,a) 

            Atmosphere(1)%Pressure(kte-k+1)    = 0.01   * p(1)  ! convert Pa to hPa
            Atmosphere(1)%Temperature(kte-k+1) = t(1)
            Atmosphere(1)%Absorber(kte-k+1,1)  = 1000.0 * a(1)/(1.0-a(1))  ! in g/kg mixing ratio

            ! NOTE: WRF high-level q values seems too big, replaced by constants
            if (p(1)*0.01 < 75.0) Atmosphere(1)%Absorber(kte-k+1,1) = 0.001

            call da_interp_2d_partial (grid%xb%qcw(:,:,k), iv%instid(inst)%info,k,n,n, model_qcw(kte-k+1:kte-k+1))
            
            if (crtm_cloud) then

               call da_interp_2d_partial (grid%xb%qci(:,:,k), iv%instid(inst)%info,k,n,n,qci)

               call da_interp_2d_partial (grid%xb%qrn(:,:,k), iv%instid(inst)%info,k,n,n,qrn)
 
               call da_interp_2d_partial (grid%xb%qsn(:,:,k), iv%instid(inst)%info, k,n,n,qsn)
 
               call da_interp_2d_partial (grid%xb%qgr(:,:,k), iv%instid(inst)%info, k,n,n,qgr)

               Atmosphere(1)%Cloud(1)%Water_Content(kte-k+1)=model_qcw(kte-k+1)
               Atmosphere(1)%Cloud(2)%Water_Content(kte-k+1)=qci(1)
               Atmosphere(1)%Cloud(3)%Water_Content(kte-k+1)=qrn(1)
               Atmosphere(1)%Cloud(4)%Water_Content(kte-k+1)=qsn(1)
               Atmosphere(1)%Cloud(5)%Water_Content(kte-k+1)=qgr(1)
               Atmosphere(1)%Cloud(6)%Water_Content(kte-k+1)=0.0

               call da_interp_2d_partial (grid%xb%rho(:,:,k), iv%instid(inst)%info, k,n,n, &
                  model_rho(k:k) )

               call da_cld_eff_radius(Atmosphere(1)%Temperature(kte-k+1),model_rho(k),&
                                      Atmosphere(1)%Cloud(2)%Water_Content(kte-k+1),  &  !qci
                                      Atmosphere(1)%Cloud(3)%Water_Content(kte-k+1),  &  !qrn
                                      Atmosphere(1)%Cloud(4)%Water_Content(kte-k+1),  &  !qsn
                                      Atmosphere(1)%Cloud(5)%Water_Content(kte-k+1),  &  !qgr
                                      model_snow(n),                                  &
                                      Surface(1)%Ice_Coverage, Surface(1)%Land_Coverage, 1, &
                                      Atmosphere(1)%Cloud(1)%Effective_Radius(kte-k+1), &
                                      Atmosphere(1)%Cloud(2)%Effective_Radius(kte-k+1), &
                                      Atmosphere(1)%Cloud(3)%Effective_Radius(kte-k+1), &
                                      Atmosphere(1)%Cloud(4)%Effective_Radius(kte-k+1), &
                                      Atmosphere(1)%Cloud(5)%Effective_Radius(kte-k+1) )

               ! reset the da_cld_eff_radius calcualted effective radius to constants if desired

               Atmosphere(1)%Cloud(1)%Effective_Radius(kte-k+1)=10.0  ! in micron
               Atmosphere(1)%Cloud(2)%Effective_Radius(kte-k+1)=30.0  ! in micron
               !Atmosphere(1)%Cloud(3)%Effective_Radius(kte-k+1)=300
               !Atmosphere(1)%Cloud(4)%Effective_Radius(kte-k+1)=600
               !Atmosphere(1)%Cloud(5)%Effective_Radius(kte-k+1)=600
               Atmosphere(1)%Cloud(6)%Effective_Radius(kte-k+1)=600

            end if
         end do

         call da_interp_2d_partial (grid%xb%u10,  iv%instid(inst)%info, 1, n, n, model_u10(n:n))
         call da_interp_2d_partial (grid%xb%v10,  iv%instid(inst)%info, 1, n, n, model_v10(n:n))
         call da_interp_2d_partial (grid%xb%psfc, iv%instid(inst)%info, 1, n, n, model_psfc(n:n))

         model_psfc(n) = 0.01*model_psfc(n)           ! convert to hPa
         model_ptop    = 0.01*grid%xb%ptop

         ! get CRTM levels (0.005hPa at top) /model full level
         Atmosphere(1)%Level_Pressure(0)                      = model_ptop    ! to sigma level 51-->sigmaf=0
         Atmosphere(1)%Level_Pressure(Atmosphere(1)%n_Layers) = model_psfc(n) ! to sigma level 1->sigmaf=1

         ! calculating full-level P from half-level P using
         ! linear interpolation: (pf-pf1)/(sigmaf-sigmaf1)=(ph2-ph1)/(sigmah2-sigmah1)
         ! note: sigma is from bottem to top; pressure is from top to bottem
         do k=kts,kte-1 ! from top to bottem
            Atmosphere(1)%Level_Pressure(k)= (Atmosphere(1)%Pressure(k+1) - Atmosphere(1)%Pressure(k))/ &
                                             (grid%xb%sigmah(kte-k) - grid%xb%sigmah(kte-k+1)) * &
                                             (grid%xb%sigmaf(kte-k+1) - grid%xb%sigmah(kte-k+1)) + &
                                              Atmosphere(1)%Pressure(k)
         end do
 
         ! check for pressure monotonicity
         do k = kte, kts+1, -1  ! from bottom to top
            if ( Atmosphere(1)%Level_Pressure(k) <= Atmosphere(1)%Level_Pressure(k-1) ) then
               write(message(1),'(a,2i5.0,a)') ' Skipping the pixel at loc ', i, j,  &
                  ' where the pressure is not monotonic'
               call da_warning(__FILE__,__LINE__,message(1:1))
               iv%instid(inst)%tb_inv(:,n) = missing_r
               iv%instid(inst)%info%proc_domain(:,n) = .false.
               cycle pixel_loop
            end if
         end do

         ! convert cloud content unit from kg/kg to kg/m^2        
         if (crtm_cloud) then
            do k=kts,kte
               do icld=1,Atmosphere(1)%n_Clouds
                  Atmosphere(1)%Cloud(icld)%Water_Content(k)= Atmosphere(1)%Cloud(icld)%Water_Content(k)* &
                     (Atmosphere(1)%Level_Pressure(k)- Atmosphere(1)%Level_Pressure(k-1))*100.0/gravity 
               end do
            end do
         end if

         if ( model_isflg == 0 ) then   ! over sea using SST
            call da_interp_2d_partial (grid%xb % tgrn, iv%instid(inst)%info, 1, n, n, model_ts(n:n))
         else
            call da_interp_2d_partial (grid%xb % tsk,  iv%instid(inst)%info, 1, n, n, model_ts(n:n))
         end if

         call da_interp_2d_partial (grid%xb % terr, iv%instid(inst)%info, 1, n, n, model_elv(n:n))

         ! variables for emissivity calculations
         !---------------------------------------- 
         call da_interp_2d_partial (grid%xb % smois,  iv%instid(inst)%info, 1, n, n, model_smois(n:n) )
         call da_interp_2d_partial (grid%xb % tslb,   iv%instid(inst)%info, 1, n, n, model_tslb(n:n) )
         call da_interp_2d_partial (grid%xb % snowh,  iv%instid(inst)%info, 1, n, n, model_snowh(n:n) )
         call da_interp_2d_partial (grid%xb % vegfra, iv%instid(inst)%info, 1, n, n, model_vegfra(n:n) )

         ! model_snowh(n) = model_snowh(n)*100.0   ! convert from m to mm
         model_vegfra(n) = 0.01*model_vegfra(n)  ! convert range to 0~1

         ! ADD for computing cloud liquid water path (mm) from guess
         clwp = 0.0
         do k = kts,kte ! from top to bottom
            dpf(k) = 100.0*(Atmosphere(1)%level_pressure(k) - Atmosphere(1)%level_pressure(k-1))
            clw  (k) = model_qcw(k)*dpf(k)/gravity ! kg/m2 or mm
            if (Atmosphere(1)%pressure(k)<100.0) clw(k) = 0.0
            clwp  = clwp + clw(k)
         end do

         ! CRTM GeometryInfo Structure
         GeometryInfo(1)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
         GeometryInfo(1)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
         GeometryInfo(1)%iFOV=iv%instid(inst)%scanpos(n)
         ! GeometryInfo(1)%Satellite_Height=830.0
         ! GeometryInfo(1)%Sensor_Scan_Angle=
         ! GeometryInfo(1)%Sensor_Zenith_Angle=
         ! GeometryInfo(1)%Sensor_Scan_Angle=
         ! GeometryInfo(1)%Source_Zenith_Angle=

         ! CRTM Surface parameter data

         if ( use_mspps_ts(inst) ) then
            ! only for AMSU-A over land
            if ( trim(crtm_sensor_name(rtminit_sensor(inst))) == 'amsua' &
                 .and. model_isflg == 2 ) then
               call da_mspps_ts(ob%instid(inst)%tb(1:nchanl,n), nchanl,  &
                                iv%instid(inst)%satzen(n), ts_mspps)
               ! ts_mspps is initilaized as negative values in the
               ! da_mspps_ts subroutine.  Apply only valid values here.
               if ( ts_mspps > 0.0 ) then
                  model_ts(n) = ts_mspps
               end if
            end if
         end if

         if (Surface(1)%Land_Coverage > 0.0) then
            Surface(1)%Land_Type       = nint(model_ivgtyp)
            Surface(1)%Vegetation_Type = max(1,wrf_to_crtm_mw(nint(model_ivgtyp)))
            Surface(1)%Soil_Type       = max(1,wrf_to_crtm_soil(nint(model_isltyp)))
            ! glacial land ice soil type and glacial vegetation type are not valid land types
            if ( surface(1)%Soil_Type == 9 .or. surface(1)%Vegetation_Type == 13 ) then
               surface(1)%ice_coverage = min(surface(1)%ice_coverage + surface(1)%land_coverage, 1.0)
               surface(1)%land_coverage = 0
            end if
            ! reset water-dominated land type
            if ( Surface(1)%Land_Type == grid%iswater ) Surface(1)%Land_Type = 1
            Surface(1)%Land_Temperature=model_ts(n)      ! K
            Surface(1)%Soil_Moisture_Content= model_smois(n) !0.05    ! volumetric water content (g/cm**3)
            ! Surface(1)%Canopy_Water_Content=0.05      ! gravimetric water content
            Surface(1)%Vegetation_Fraction=model_vegfra(n)
            Surface(1)%Soil_Temperature=model_tslb(n)
         end if
         if (Surface(1)%Water_Coverage > 0.0) then
            ! Surface%Water_Type=SEA_WATER          ! (Currently NOT used)
            Surface(1)%Water_Temperature=model_ts(n)     ! K
            Surface(1)%Wind_Speed=sqrt(model_u10(n)**2+model_v10(n)**2)  ! m/sec
            ! surface(1)%Wind_Direction=0.0            ! NOT used
            Surface(1)%Salinity=33.0                   ! ppmv
         end if

         if (Surface(1)%Snow_Coverage > 0.0) then
            Surface(1)%Snow_Type=2 !NEW_SNOW
            Surface(1)%Snow_Temperature=model_ts(n)      ! K
            Surface(1)%Snow_Depth=model_snowh(n)         ! mm
            ! Surface(1)%Snow_Density=0.2               ! g/cm**3
            ! Surface(1)%Snow_Grain_Size=2.0            ! mm
         end if
         if (Surface(1)%Ice_Coverage > 0.0) then
            ! Surface(1)%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
            Surface(1)%Ice_Temperature=model_ts(n)       ! K
            Surface(1)%Ice_Thickness=10.0              ! mm
            ! Surface(1)%Ice_Density=0.9                ! g/cm**3
            ! Surface(1)%Ice_Roughness=0.0               ! NO Table offered, single example is ZERO

         end if
         if (nchanl > 0) then
            Surface(1)%SensorData%n_channels        = nchanl
            Surface(1)%SensorData%Sensor_Channel    = ChannelInfo(inst)%Sensor_Channel
            Surface(1)%SensorData%Sensor_Id         = ChannelInfo(inst)%Sensor_Id
            Surface(1)%SensorData%WMO_Satellite_Id  = ChannelInfo(inst)%WMO_Satellite_Id
            Surface(1)%SensorData%WMO_Sensor_Id     = ChannelInfo(inst)%WMO_Sensor_Id
            Surface(1)%SensorData%Tb(1:nchanl)      = ob%instid(inst)%tb(1:nchanl,n)
         end if

         ! user emissivity options
         Options(1)%use_emissivity = .false.
         if ( use_mspps_emis(inst) ) then
            ! Only for AMSU-A over land
            if ( trim(crtm_sensor_name(rtminit_sensor(inst))) == 'amsua' &
                 .and. model_isflg == 2 ) then
               call da_mspps_emis(ob%instid(inst)%tb(1:nchanl,n), nchanl, em_mspps)
               Options(1)%use_emissivity = .true.
               Options(1)%emissivity(1:nchanl) = em_mspps(1:nchanl)
            end if
         end if

         if (use_crtm_kmatrix) then

            ! CRTM surface/atmosphere K initialization
            do l = 1, ChannelInfo(inst)%n_Channels
               ! -- Copy the adjoint atmosphere structure
               Atmosphere_K(l,1) = Atmosphere(1)
 
               ! -- Copy the adjoint surface structure
               Surface_K(l,1) = Surface(1)
            end do

            ! -- Zero the Adjoint outputs
            ! Important: adjoint variables must be initialized
            call CRTM_Atmosphere_Zero( Atmosphere_K )
            call CRTM_Surface_Zero( Surface_K )

            ! Assign tb = R^-1 Re :
            RTSolution_K(:,1)%brightness_temperature = 1.
            RTSolution_K(:,1)%radiance = 0.
	    
            ! [1.3] Call RTM K-Matrix model
            call da_crtm_k(1, nchanl, 1, Atmosphere,   &
                               Surface,      &
                               RTSolution_K,&
                               GeometryInfo, &
                               ChannelInfo(inst),  &
                               Atmosphere_K,&
                               Surface_K,   &
                               RTSolution,  &
                               Options)
         else
	 
            ! [1.3] Call RTM forward model
            call da_crtm_direct(1, nchanl, 1, Atmosphere,   &
               Surface,      &
               GeometryInfo, &
               ChannelInfo(inst:inst),  &
               RTSolution,              &
               Options)
	    
	 end if

! Compute Overcast Radiances for MMR in Auligné (2014).or. PF in Xu et al., (2016).or. MW03 ecmwf method in McNally AP and Watts PD (2003) 
         !----------------------------------------------------------------
         if (use_clddet==1 .or. use_clddet==2.or. use_clddet==3 ) then
            do i = 1, nchanl
               !crtm_2.2.3 contains Upwelling_Overcast_Radiance
               iv%instid(inst)%rad_ovc(i,kts:kte,n)=RTSolution(i,1)%Upwelling_Overcast_Radiance(:)
            end do
         end if

   !---------------------------------------------------------------------------
   ! Precondition satellite control variable (Cloud Cover(s)):
   !---------------------------------------------------------------------------
         if (.false.) then  
!         if (use_satcv(2)) then  
            nclouds = iv%instid(inst)%cv_index(n)%nclouds
	    allocate (hessian(nclouds,nclouds))
	    allocate (eignvec(nclouds,nclouds))
	    allocate (eignval(nclouds))
            hessian(:,:)= 0.0      
            do ilev=1, nclouds
               do jlev=ilev, nclouds
	          do k = 1, nchanl
                     if (ALL(iv%instid(inst)%ichan(k) /=  Bands(:,1))) cycle   ! Only Channels in Band 1 	       
                        rad_clr      = iv%instid(inst)%rad_xb(k,n)             
                        rad_ovc_ilev = iv%instid(inst)%rad_ovc(k,kte-nclouds+ilev,n)
                        rad_ovc_jlev = iv%instid(inst)%rad_ovc(k,kte-nclouds+jlev,n)
                        hessian(ilev,jlev) = hessian(ilev,jlev) + (rad_ovc_ilev-rad_clr) * (rad_ovc_jlev-rad_clr)
      	          end do			     	 
                 hessian(jlev,ilev) = hessian(ilev,jlev)   
               end do
            end do  
	      
            call da_eof_decomposition(nclouds, hessian, eignvec, eignval)
	      
!	    if (ANY(eignval <= 0)) write(unit=stdout,fmt='(3A,I4,A,100F18.5)')      &
!	       'SATCV: non-positive Hessian for ', trim(iv%instid(inst)%rttovid_string), ' ,pixel ',n,'--> Eigenvalues =',eignval 
 	      
	    do ilev = 1, nclouds
	       do jlev = ilev, nclouds
                  iv%instid(inst)%cv_index(n)%vtox(ilev,jlev) = &
		     sum( eignvec(ilev,:) * sqrt(1.0/eignval(:)) * eignvec(jlev,:), mask = eignval >0 )
		       
		  iv%instid(inst)%cv_index(n)%vtox(jlev,ilev) = iv%instid(inst)%cv_index(n)%vtox(ilev,jlev) 
               end do
	    end do
	      	 
            deallocate(hessian,eignvec,eignval)
         end if   

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------
         do k = 1, nchanl
            iv%instid(inst)%tb_xb(k,n)  = RTSolution(k,1)%Brightness_Temperature
            iv%instid(inst)%rad_xb(k,n) = RTSolution(k,1)%Radiance
            iv%instid(inst)%emiss(k,n)  = RTSolution(k,1)%surface_emissivity
      
            if (use_pseudo_rad .or. use_simulated_rad) then ! input is innovation
              ob%instid(inst)%tb(k,n) = missing_r
	      if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) &
                ob%instid(inst)%tb(k,n) = RTSolution(k,1)%Brightness_Temperature + iv%instid(inst)%tb_inv(k,n)
            else
               if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) & 
                  iv%instid(inst)%tb_inv(k,n) = ob%instid(inst)%tb(k,n) - iv%instid(inst)%tb_xb(k,n)	    
	    end if

            if (use_crtm_kmatrix) then
               ! surface Jacobian
               iv%instid(inst)%ts_jacobian(k,n) = Surface_k(k,1)%water_temperature
               iv%instid(inst)%windspeed_jacobian(k,n) = Surface_k(k,1)%wind_speed
               iv%instid(inst)%emiss_jacobian(k,n) = RTSolution_k(k,1)%surface_emissivity
!#ifdef CRTM_MODIF
               iv%instid(inst)%gamma_jacobian(k,n) = RTSolution_k(k,1)%Gamma
!#endif
	    end if   
         end do

         !----------------------------------------------------------------
         ! [3.0] store base state (and Jacobian) to innovation structure
         !----------------------------------------------------------------
         ! full level pressures
         iv%instid(inst)%pf(0,n)  = Atmosphere(1)%level_pressure(0)
         if (use_crtm_kmatrix) then
            ! PS Jacobian
            do l=1,nchanl
               iv%instid(inst)%ps_jacobian(l,n)  = Atmosphere_k(l,1)%level_pressure(Atmosphere(1)%n_layers)
            end do
	 end if
         do k=1,Atmosphere(1)%n_layers
            iv%instid(inst)%pm(k,n)  = Atmosphere(1)%pressure(k)
            iv%instid(inst)%pf(k,n)  = Atmosphere(1)%level_pressure(k)
            iv%instid(inst)%tm(k,n)  = Atmosphere(1)%temperature(k)
            iv%instid(inst)%qm(k,n)  = Atmosphere(1)%absorber(k,1)

            if (use_crtm_kmatrix) then
               ! T, Q Jacobian
               do l=1,nchanl
                  iv%instid(inst)%t_jacobian(l,k,n) = Atmosphere_k(l,1)%temperature(k)
                  iv%instid(inst)%q_jacobian(l,k,n) = Atmosphere_k(l,1)%absorber(k,1)
               end do
	    end if
         end do
	       
         if (crtm_cloud) then
            do k=1,Atmosphere(1)%n_layers
               iv%instid(inst)%qcw(k,n) = Atmosphere(1)%cloud(1)%water_content(k)
               iv%instid(inst)%qci(k,n) = Atmosphere(1)%cloud(2)%water_content(k)
               iv%instid(inst)%qrn(k,n) = Atmosphere(1)%cloud(3)%water_content(k)
               iv%instid(inst)%qsn(k,n) = Atmosphere(1)%cloud(4)%water_content(k)
               iv%instid(inst)%qgr(k,n) = Atmosphere(1)%cloud(5)%water_content(k)
               iv%instid(inst)%qhl(k,n) = Atmosphere(1)%cloud(6)%water_content(k)
               iv%instid(inst)%rcw(k,n) = Atmosphere(1)%cloud(1)%effective_radius(k)
               iv%instid(inst)%rci(k,n) = Atmosphere(1)%cloud(2)%effective_radius(k)
               iv%instid(inst)%rrn(k,n) = Atmosphere(1)%cloud(3)%effective_radius(k)
               iv%instid(inst)%rsn(k,n) = Atmosphere(1)%cloud(4)%effective_radius(k)
               iv%instid(inst)%rgr(k,n) = Atmosphere(1)%cloud(5)%effective_radius(k)
               iv%instid(inst)%rhl(k,n) = Atmosphere(1)%cloud(6)%effective_radius(k)
            
               if (use_crtm_kmatrix) then
	          ! Cloud Jacobian
                  do l=1,nchanl
                     iv%instid(inst)%water_jacobian(l,k,n)    = Atmosphere_k(l,1)%cloud(1)%water_content(k)
                     iv%instid(inst)%ice_jacobian(l,k,n)      = Atmosphere_k(l,1)%cloud(2)%water_content(k)
                     iv%instid(inst)%rain_jacobian(l,k,n)     = Atmosphere_k(l,1)%cloud(3)%water_content(k)
                     iv%instid(inst)%snow_jacobian(l,k,n)     = Atmosphere_k(l,1)%cloud(4)%water_content(k)
                     iv%instid(inst)%graupel_jacobian(l,k,n)  = Atmosphere_k(l,1)%cloud(5)%water_content(k)
                     iv%instid(inst)%hail_jacobian(l,k,n)     = Atmosphere_k(l,1)%cloud(6)%water_content(k)

                     iv%instid(inst)%water_r_jacobian(l,k,n)  = Atmosphere_k(l,1)%cloud(1)%effective_radius(k)
                     iv%instid(inst)%ice_r_jacobian(l,k,n)    = Atmosphere_k(l,1)%cloud(2)%effective_radius(k)
                     iv%instid(inst)%rain_r_jacobian(l,k,n)   = Atmosphere_k(l,1)%cloud(3)%effective_radius(k)
                     iv%instid(inst)%snow_r_jacobian(l,k,n)   = Atmosphere_k(l,1)%cloud(4)%effective_radius(k)
                     iv%instid(inst)%graupel_r_jacobian(l,k,n)= Atmosphere_k(l,1)%cloud(5)%effective_radius(k)
                     iv%instid(inst)%hail_r_jacobian(l,k,n)   = Atmosphere_k(l,1)%cloud(6)%effective_radius(k)
                  end do
	       end if	  
            end do
         end if

         if ( calc_weightfunc .and. use_crtm_kmatrix ) then
            do l = 1, nchanl
               total_od = 0.0
               do k = 1, Atmosphere(1)%n_layers
                  iv%instid(inst)%lod(l,k,n) = RTSolution(l,1)%layer_optical_depth(k)
                  iv%instid(inst)%lod_jacobian(l,k,n) = RTSolution_K(l,1)%layer_optical_depth(k)
                  total_od = total_od + RTSolution(l,1)%layer_optical_depth(k)
                  iv%instid(inst)%trans(l,k,n) = exp(-total_od)
                  iv%instid(inst)%trans_jacobian(l,k,n) = -iv%instid(inst)%lod_jacobian(l,k,n)*exp(iv%instid(inst)%lod(l,k,n))
               end do
               iv%instid(inst)%der_trans(l,1,n) = 0.0
               do k= 2, Atmosphere(1)%n_layers
                  iv%instid(inst)%der_trans(l,k,n) =  abs( (iv%instid(inst)%trans(l,k-1,n)-iv%instid(inst)%trans(l,k,n))/ &
                                                           (log(iv%instid(inst)%pm(k-1,n))-log(iv%instid(inst)%pm(k,n))) )
               end do
            end do
         end if

         if (use_clddet==3) then  !begin ecmwf cloud detection
            !kmin_t,kmax_p corresponding tropopause and KBPL
            ! find the k index of tropopause
            t_tropp = 999.0  ! initialize
            k_tropp = kte    ! initialize the k index of tropopause
            do k = kte, kts, -1
               do j = jts, jte
                  do i = its, ite
                     if ( grid%xb%t(i,j,k) < t_tropp(i,j) .and.  &
                          grid%xb%p(i,j,k) > 5000.0 ) then  ! tropopause should not be higher than 50 hPa
                         t_tropp(i,j) = grid%xb%t(i,j,k)
                         k_tropp(i,j) = k
                         iv%instid(inst)%kmin_t(n)=Atmosphere(1)%n_layers-k_tropp(i,j)+1
                     end if
                  end do
               end do
            end do
            iv%instid(inst)%kmax_p(n)=Atmosphere(1)%n_layers-grid%kpbl(i,j)+1
            !write(unit=stdout,fmt='(A)') &
            !   'preparing ECMWF cloud detection input using cloud sensitivity to rank channels'
            do l=1,nchanl
               temp1=0
               do k=1,Atmosphere(1)%n_layers  !rad_ovc(l,1,n) top
                  iv%instid(inst)%sensitivity_ratio(l,k,n) = abs(iv%instid(inst)%rad_xb(l,n)-iv%instid(inst)%rad_ovc(l,k,n))
                  iv%instid(inst)%sensitivity_ratio(l,k,n) = iv%instid(inst)%sensitivity_ratio(l,k,n)/iv%instid(inst)%rad_xb(l,n)
               end do
            end do
            do l=1,nchanl
               temp1=0
               do k=Atmosphere(1)%n_layers,1,-1 ! sensitivity_ratio(l,1,n) top
                  if( iv%instid(inst)%sensitivity_ratio(l,k,n) > 0.01 ) then
                     iv%instid(inst)%p_chan_level(l,n) = real(k) ! the peak level
                     exit
                  end if
               end do
            end do
         end if     !end ecmwf cloud detection

         !----------------------------------------------
         ! [4.0] store surface information to innovation structure
         !----------------------------------------------
         iv%instid(inst)%u10(n)       = model_u10(n)
         iv%instid(inst)%v10(n)       = model_v10(n)
         iv%instid(inst)%t2m(n)       = 0.01*missing_r !model_t2m
         iv%instid(inst)%mr2m(n)      = 0.01*missing_r !model_mr2m
         iv%instid(inst)%ps(n)        = model_psfc(n)
         iv%instid(inst)%ts(n)        = model_ts(n)
         iv%instid(inst)%smois(n)     = model_smois(n)
         iv%instid(inst)%tslb(n)      = model_tslb(n)
         iv%instid(inst)%snowh(n)     = model_snowh(n)
         iv%instid(inst)%isflg(n)     = model_isflg
         iv%instid(inst)%elevation(n) = model_elv(n)
         iv%instid(inst)%soiltyp(n)   = model_isltyp
         iv%instid(inst)%vegtyp(n)    = model_ivgtyp
         iv%instid(inst)%vegfra(n)    = model_vegfra(n)
         iv%instid(inst)%clwp(n)      = clwp
         iv%instid(inst)%water_coverage(n) = Surface(1)%water_coverage
         iv%instid(inst)%land_coverage(n)  = Surface(1)%land_coverage
         iv%instid(inst)%ice_coverage(n)   = Surface(1)%ice_coverage                              
         iv%instid(inst)%snow_coverage(n)  = Surface(1)%snow_coverage

         if ( calc_tb_clr ) then
            atmosphere(1)%n_clouds = 0
            call da_crtm_direct(1, nchanl, 1, Atmosphere,   &
               Surface,                                     &
               GeometryInfo,                                &
               ChannelInfo(inst:inst),                      &
               RTSolution_clr,                              &
               Options)
            do k = 1, nchanl
               iv%instid(inst)%tb_xb_clr(k,n) = RTSolution_clr(k,1)%Brightness_Temperature
            end do
            ! resetting n_clouds
            atmosphere(1)%n_clouds = n_clouds
         end if !calc_tb_clr

      end do pixel_loop      !  end loop for pixels

      if ( use_mspps_emis(inst) ) deallocate(em_mspps)

      deallocate (model_u10)
      deallocate (model_v10)
      deallocate (model_psfc)
      deallocate (model_ts)
      deallocate (model_tslb)
      deallocate (model_snowh)
      deallocate (model_snow)
      deallocate (model_elv)
      deallocate (model_vegfra)
      deallocate (model_smois)

      call CRTM_RTSolution_Destroy(RTSolution)
      deallocate( RTSolution, STAT = Allocate_Status )
      if (Allocate_Status /= 0) &
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating RTSolution"/))

      if ( calc_tb_clr ) then
         call CRTM_RTSolution_Destroy(RTSolution_clr)
         deallocate( RTSolution_clr, STAT = Allocate_Status )
         if (Allocate_Status /= 0) &
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocating RTSolution_clr"/))
      end if !calc_tb_clr

      if (use_crtm_kmatrix) then
         call CRTM_RTSolution_Destroy(RTSolution_K)
         deallocate( RTSolution_K, STAT = Allocate_Status )
         if (Allocate_Status /= 0) &
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocating RTSolution_K"/))
      end if	 

      call CRTM_Options_Destroy(Options)
      IF ( CRTM_Options_Associated( Options(1) ) ) &
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating CRTM Options Structure"/))

      call CRTM_Surface_Destroy(Surface)
      IF ( CRTM_Surface_Associated( Surface(1) ) ) &
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating CRTM Surface Structure"/))

      if (use_crtm_kmatrix) then
         call CRTM_Surface_Destroy(Surface_K)
         IF ( ANY( CRTM_Surface_Associated( Surface_K(1,:)) ) ) &
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface_K Structure"/))
      endif

      if (use_crtm_kmatrix) then
         call CRTM_Atmosphere_Destroy( Atmosphere_K )
         IF ( ANY( CRTM_Atmosphere_Associated( Atmosphere_K(1,:)) ) ) &
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Atmosphere_K Structure"/))
      endif

      if (use_crtm_kmatrix) then
         deallocate( Atmosphere_K, Surface_K, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) &
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface_K Structure"/))
      endif

   end do        ! end loop for sensor

   deallocate (wrf_to_crtm_mw)

   call CRTM_Atmosphere_Destroy (Atmosphere)
   IF ( CRTM_Atmosphere_Associated( Atmosphere(1) ) ) &
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocating CRTM Atmosphere Structure"/))

   if (trace_use) call da_trace_exit("da_get_innov_vector_crtm")
 
end subroutine da_get_innov_vector_crtm

