subroutine da_transform_xtoy_crtm (cv_size, cv, grid, iv, y )

   !---------------------------------------------------------------------------
   !  PURPOSE: transform from analysis increment to 
   !                          pertubation radiance.
   !
   !  METHOD:  delta_y = H delta_x
   !           1. input reference state of CRTM_TL
   !           2. interpolate analysis increment to obs location
   !           3. Call CRTM_TL
   !
   !  HISTORY: 11/16/2006 - Creation                        Zhiquan Liu
   !           11/25/2008 - Zero Jacobian for top level     Tom Auligne
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: cv_size         ! Size of cv array.
   real, intent(in)               :: cv(1:cv_size)   ! control variables.
   type (domain),  intent(in)     :: grid
   type (y_type),  intent(inout)  :: y        ! H' delta_x
   type (iv_type), intent(in)     :: iv       ! O-B structure.

   integer, parameter             :: AIRS_Max_Channels = 281

#ifdef CRTM
   integer                        :: k, l  ! Index dimension.

   integer           :: inst, num_rad, nchanl, n, icld
   integer           :: ipred, npred, gammapred, id
   real, allocatable :: temperature(:,:)
   real, allocatable :: absorber(:,:), xb_q(:,:)
   real, allocatable :: psfc(:)
!! for crtm_cloud
   real, allocatable :: qcw(:,:),qci(:,:),qrn(:,:),qsn(:,:),qgr(:,:)

   ! CRTM local variables and types
   integer :: Allocate_Status
   integer :: n_layers, n_absorbers, n_clouds, n_aerosols
   type( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:,:),RTSolution_TL(:,:)
   type( CRTM_Atmosphere_type ), allocatable :: Atmosphere(:), Atmosphere_TL(:)
   type( CRTM_Surface_type ),    allocatable :: Surface(:), Surface_TL(:)
   type( CRTM_Geometry_type ),   allocatable :: GeometryInfo(:)
   type( CRTM_Options_type ), allocatable    :: Options(:)
   
   integer                        :: ts_index
   integer                        :: nclouds, ncv
   real, allocatable              :: cc_tl(:)
   real*8                         :: rad_clr, rad_cld   ! RT clear/cloudy radiances
   real, allocatable              :: rad_ovc(:)         ! RT overcast radiances
   real*8                         :: rad_tl, tb_tl

   ! Initializations for AIRS (MMR) Cloud Detection
   integer                        :: Band_Size(5), Bands(AIRS_Max_Channels,5) 
  
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

!---------------------------------------------------------

   if ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm")

   sensor_loop : do inst = 1, iv%num_inst                 ! loop for sensor

      num_rad = iv%instid(inst)%info%n2 - iv%instid(inst)%info%n1 + 1
      if ( num_rad < 1 ) cycle

      allocate (Atmosphere   (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                Atmosphere_TL(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Atmosphere"/))
      end if
      allocate (Surface      (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                Surface_TL   (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Surface"/))
      end if
      allocate (GeometryInfo (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting GeometryInfo"/))
      end if
      allocate (Options      (iv%instid(inst)%info%n1:iv%instid(inst)%info%n2), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting Options"/))
      end if

!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure

      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         n_layers    = (kte-kts)+1   ! number of vertical levels
         n_absorbers = 2
         n_aerosols  = 0
         n_clouds    = 0
         if ( crtm_cloud ) n_clouds = 6

         call CRTM_Atmosphere_Create( Atmosphere(n), &
                                      n_layers,      &
                                      n_absorbers,   &
                                      n_clouds,      &
                                      n_aerosols )
         IF ( .NOT. CRTM_Atmosphere_Associated( Atmosphere(n) ) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in allocatting CRTM Atmosphere Structure"/))
         end if

         Atmosphere(n)%Absorber_ID(1)=H2O_ID
         Atmosphere(n)%Absorber_ID(2)=O3_ID
         Atmosphere(n)%Absorber_Units(1) = MASS_MIXING_RATIO_UNITS
         Atmosphere(n)%Absorber_Units(2) = VOLUME_MIXING_RATIO_UNITS
         Atmosphere(n)%Climatology=iv%instid(inst)%crtm_climat(n)

         if (crtm_cloud) then
            Atmosphere(n)%Cloud(1)%Type=WATER_CLOUD
            Atmosphere(n)%Cloud(2)%Type=ICE_CLOUD
            Atmosphere(n)%Cloud(3)%Type=RAIN_CLOUD
            Atmosphere(n)%Cloud(4)%Type=SNOW_CLOUD
            Atmosphere(n)%Cloud(5)%Type=GRAUPEL_CLOUD
            Atmosphere(n)%Cloud(6)%Type=HAIL_CLOUD
         end if

      end do

!-------------------------------------------------------------------------------

      nchanl    = ChannelInfo(inst)%n_channels
                                        
  ! Allocate forward model solution RTSolution array to number of channels
      allocate( RTSolution( ChannelInfo(inst)%n_Channels, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2 ), &
                RTSolution_TL( ChannelInfo(inst)%n_Channels, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2 ), &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in allocatting RTSolution"/))
      END IF

      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         call CRTM_Surface_Create( Surface(n),  & ! Output
                                   nchanl )       ! Input
         IF ( .NOT. CRTM_Surface_Associated( Surface(n) ) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in allocatting CRTM Surface Structure"/))
         end if

  ! CRTM Options structure
         call CRTM_Options_Create( Options(n),  &  ! Output
                                   nchanl )        ! Input
         IF ( .NOT. CRTM_Options_Associated( Options(n) ) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in allocatting CRTM Options Structure"/))
         endif
         if ( use_antcorr(inst) ) Options(n)%Use_Antenna_Correction = .true.

      end do

      allocate (temperature(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (absorber(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (xb_q(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (psfc(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))

      temperature(:,:) = 0.0
      absorber(:,:)    = 0.0
      xb_q(:,:)        = 0.0
      psfc(:)          = 0.0

      if (crtm_cloud) then

         allocate (qcw(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qci(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qrn(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qsn(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qgr(Atmosphere(iv%instid(inst)%info%n1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         qcw(:,:) = 0.0
         qci(:,:) = 0.0
         qrn(:,:) = 0.0
         qsn(:,:) = 0.0
         qgr(:,:) = 0.0

      end if

      do k=kts,kte ! from bottom to top
         call da_interp_2d_partial (grid%xa%t(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            temperature(kte-k+1,:))
         call da_interp_2d_partial (grid%xa%q(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            absorber(kte-k+1,:))

         if ( crtm_cloud .and. cloud_cv_options > 0 ) then

            call da_interp_2d_partial (grid%xa%qcw(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
               qcw(kte-k+1,:))
            call da_interp_2d_partial (grid%xa%qrn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
               qrn(kte-k+1,:))
            if ( cloud_cv_options > 1 ) then
               call da_interp_2d_partial (grid%xa%qci(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
                  qci(kte-k+1,:))
               call da_interp_2d_partial (grid%xa%qsn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
                  qsn(kte-k+1,:))
               call da_interp_2d_partial (grid%xa%qgr(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
                  qgr(kte-k+1,:))
            end if

         end if

      end do

      call da_interp_2d_partial (grid%xa%psfc, iv%instid(inst)%info, 1, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, psfc(:))

      ! Gamma correction from VarBC
      !----------------------------
!#ifdef CRTM_MODIF
      if (use_varbc) then
        gammapred = iv%instid(inst)%varbc_info%gammapred 
        do k = 1, nchanl
           npred = iv%instid(inst)%varbc(k)%npred
           if (npred <= 0) cycle                                               ! VarBC channels only
           if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle         ! Gamma channels only
           if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle         ! Gamma channels only
           do ipred = 1, npred
              if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
              id = iv%instid(inst)%varbc(k)%index(ipred)
              RTSolution(k,:)%Gamma    = iv%instid(inst)%varbc(k)%param(ipred)
              RTSolution_TL(k,:)%Gamma = SUM(cv(id) * iv%instid(inst)%varbc(k)%vtox(ipred,1:npred))
           end do	  
        end do
      end if
!#endif

      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
            
      ! [1.1] Get horizontal interpolation weights:

      ! [1.3] Extract base state Atmosphere variables 
         Atmosphere(n)%level_pressure(0) = iv%instid(inst)%pf(0,n)
         do k=1,Atmosphere(n)%n_layers
            Atmosphere(n)%pressure(k)       = iv%instid(inst)%pm(k,n)
            Atmosphere(n)%level_pressure(k) = iv%instid(inst)%pf(k,n)
            Atmosphere(n)%temperature(k)    = iv%instid(inst)%tm(k,n)
            Atmosphere(n)%absorber(k,1)     = iv%instid(inst)%qm(k,n)
            xb_q(k,n) = 0.001*iv%instid(inst)%qm(k,n)/(1.0+iv%instid(inst)%qm(k,n)) ! specific humidity
         end do
         if (crtm_cloud) then
            do k=1,Atmosphere(n)%n_layers
              Atmosphere(n)%cloud(1)%water_content(k)=iv%instid(inst)%qcw(k,n)
              Atmosphere(n)%cloud(2)%water_content(k)=iv%instid(inst)%qci(k,n)
              Atmosphere(n)%cloud(3)%water_content(k)=iv%instid(inst)%qrn(k,n)
              Atmosphere(n)%cloud(4)%water_content(k)=iv%instid(inst)%qsn(k,n)
              Atmosphere(n)%cloud(5)%water_content(k)=iv%instid(inst)%qgr(k,n)
              Atmosphere(n)%cloud(6)%water_content(k)=iv%instid(inst)%qhl(k,n)
              Atmosphere(n)%cloud(1)%effective_radius(k)=iv%instid(inst)%rcw(k,n)
              Atmosphere(n)%cloud(2)%effective_radius(k)=iv%instid(inst)%rci(k,n)
              Atmosphere(n)%cloud(3)%effective_radius(k)=iv%instid(inst)%rrn(k,n)
              Atmosphere(n)%cloud(4)%effective_radius(k)=iv%instid(inst)%rsn(k,n)
              Atmosphere(n)%cloud(5)%effective_radius(k)=iv%instid(inst)%rgr(k,n)
              Atmosphere(n)%cloud(6)%effective_radius(k)=iv%instid(inst)%rhl(k,n)
           end do
         end if

      ! [1.4] User-supplied emissivity
           !Options%emissivity_switch = 1
           !Options%emissivity(1:Options%n_channels) = &
           !    iv%instid(inst)%emiss(1:Options%n_channels,n)

      ! [1.4] CRTM Surface parameter data
         Surface(n)%Land_Coverage=iv%instid(inst)%land_coverage(n) 
         Surface(n)%Water_Coverage=iv%instid(inst)%water_coverage(n) 
         Surface(n)%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
         Surface(n)%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

         if (Surface(n)%Land_Coverage > 0.0) then
            Surface(n)%Land_Type=11 !GRASS_SOIL
            Surface(n)%Land_Temperature=iv%instid(inst)%ts(n)      ! K
            Surface(n)%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
            !Surface(n)%Canopy_Water_Content=0.05      ! gravimetric water content
            Surface(n)%Vegetation_Fraction=iv%instid(inst)%vegfra(n)
            Surface(n)%Soil_Temperature=iv%instid(inst)%tslb(n)
         end if
         if (Surface(n)%Water_Coverage > 0.0) then
            !Surface(n)%Water_Type=SEA_WATER          ! (Currently NOT used)
            Surface(n)%Water_Temperature=iv%instid(inst)%ts(n)     ! K
            Surface(n)%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                                      (iv%instid(inst)%v10(n))**2)  ! m/sec
            !surface(n)%Wind_Direction=0.0            ! NOT used
            Surface(n)%Salinity=33.                   ! ppmv
         end if
         if (Surface(n)%Snow_Coverage > 0.0) then
            Surface(n)%Snow_Type=2 !NEW_SNOW
            Surface(n)%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
            Surface(n)%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
            !Surface(n)%Snow_Density=0.2               ! g/cm**3
            !Surface(n)%Snow_Grain_Size=2.0            ! mm
         end if
         if (Surface(n)%Ice_Coverage > 0.0) then
            !Surface(n)%Ice_Type=FRESH_ICE             ! NO Table offered, single egrid%xample is FRESH_ICE
            Surface(n)%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
            Surface(n)%Ice_Thickness=10.0              ! mm
            !Surface(n)%Ice_Density=0.9                ! g/cm**3
            !Surface(n)%Ice_Roughness=0.0               ! NO Table offered, single egrid%xample is ZERO
         end if
         Surface(n)%SensorData%n_channels        = nchanl
         Surface(n)%SensorData%Sensor_Channel    = ChannelInfo(inst)%Sensor_Channel
         Surface(n)%SensorData%Sensor_Id         = ChannelInfo(inst)%Sensor_Id
         Surface(n)%SensorData%WMO_Satellite_Id  = ChannelInfo(inst)%WMO_Satellite_Id
         Surface(n)%SensorData%WMO_Sensor_Id     = ChannelInfo(inst)%WMO_Sensor_Id
         Surface(n)%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                              iv%instid(inst)%tb_xb(1:nchanl,n)

      ! -- Copy the TL atmosphere structure
         Atmosphere_TL(n) = Atmosphere(n)

      ! -- Copy the TL surface structure
         Surface_TL(n) = Surface(n)

    ! -- Zero the TL inputs
    ! Important: adjoint variables must be initialized
        call CRTM_Atmosphere_Zero( Atmosphere_TL(n) )
        call CRTM_Surface_Zero( Surface_TL(n) )

        ! Humidity in g/kg. Zero Jacobian for top level(s) (above 75hPa)
        do k = kts, kte
          if ( iv%instid(inst)%pm(k,n) < 75.0 ) absorber(k,n) = 0.0
        end do
        Atmosphere_TL(n)%Absorber(kts+1:kte,1)  = 1000.0 / (1.0-xb_q(kts+1:kte,n))**2  &
                                                  * absorber(kts+1:kte,n)

        Atmosphere_TL(n)%Temperature(kts+1:kte) = temperature(kts+1:kte,n)       ! Zero Jacobian for top level
        Atmosphere_TL(n)%Level_Pressure(Atmosphere_TL(n)%n_Layers) = 0.01 * psfc(n)

        if (crtm_cloud) then
           Atmosphere_TL(n)%Cloud(1)%Water_Content(kts:kte)             = qcw(kts:kte,n)
           Atmosphere_TL(n)%Cloud(2)%Water_Content(kts:kte)             = qci(kts:kte,n)
           Atmosphere_TL(n)%Cloud(3)%Water_Content(kts:kte)             = qrn(kts:kte,n)
           Atmosphere_TL(n)%Cloud(4)%Water_Content(kts:kte)             = qsn(kts:kte,n)
           Atmosphere_TL(n)%Cloud(5)%Water_Content(kts:kte)             = qgr(kts:kte,n)
           Atmosphere_TL(n)%Cloud(6)%Water_Content(kts:kte)             = 0.
        ! convert cloud content unit from kg/kg to kg/m^2
           do k=kts,kte
              do icld=1,Atmosphere(n)%n_Clouds

                 Atmosphere_TL(n)%Cloud(icld)%Water_Content(k)=  Atmosphere_TL(n)%Cloud(icld)%Water_Content(k)* &
                                (Atmosphere(n)%Level_Pressure(k)- Atmosphere(n)%Level_Pressure(k-1))*100./gravity
 
              enddo
           enddo
        end if

      ! Skin Temperature
      !-----------------
        if (use_satcv(1)) then
           ts_index = iv%instid(inst)%cv_index(n)%ts 	 
  	   if (Surface(n)%Land_Coverage  > 0.0) Surface_TL(n)%Land_Temperature  = cv(ts_index)
           if (Surface(n)%Water_Coverage > 0.0) Surface_TL(n)%Water_Temperature = cv(ts_index)
           if (Surface(n)%Snow_Coverage  > 0.0) Surface_TL(n)%Snow_Temperature  = cv(ts_index)
           if (Surface(n)%Ice_Coverage   > 0.0) Surface_TL(n)%Ice_Temperature   = cv(ts_index)	 
        end if 
	 
      ! Cloud cover(s)
      !---------------
        if (use_satcv(2)) then
	   nclouds = iv%instid(inst)%cv_index(n)%nclouds
	   ncv     = iv%instid(inst)%cv_index(n)%ncv
	   allocate (rad_ovc (nclouds))
	   allocate (cc_tl   (nclouds))

           cc_tl(:) = cv(iv%instid(inst)%cv_index(n)%cc(:))
!	    !---------------------------------------------------------------
!           ! Change of variable (preconditioning) 
!           !---------------------------------------------------------------
!	   do icld = 1, nclouds
!    	      cc_tl(icld) = SUM( cv(iv%instid(inst)%cv_index(n)%cc) * &
!	                            iv%instid(inst)%cv_index(n)%vtox(icld,1:ncv) )
!	   end do
	    	
	   do k = 1, nchanl
              if (ALL(iv%instid(inst)%ichan(k) /=  Bands(:,1))) cycle   ! Only Channels in Band 1 	       
              rad_clr    = iv%instid(inst)%rad_xb(k,n)             
              rad_ovc(:) = iv%instid(inst)%rad_ovc(k,kte-nclouds+1:kte,n)
              rad_cld    = SUM(cc_tl*rad_ovc) + (1.0 - SUM(cc_tl))*rad_clr
	      rad_tl     = SUM(cc_tl*(rad_ovc-rad_clr))
 	      call CRTM_Planck_Temperature_TL(inst,k,rad_clr,rad_tl,tb_tl)
	      y%instid(inst)%tb(k,n) = y%instid(inst)%tb(k,n) + tb_tl	          
   	   end do			     	 

	   deallocate(cc_tl, rad_ovc)
	else
	   y%instid(inst)%tb(:,n) = 0.0  
        end if 

      ! [1.5] CRTM GeometryInfo Structure
        GeometryInfo(n)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo(n)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
        GeometryInfo(n)%iFOV=iv%instid(inst)%scanpos(n)
   !    GeometryInfo(n)%Satellite_Height=830.0
   !    GeometryInfo(n)%Sensor_Scan_Angle=
   !    GeometryInfo(n)%Sensor_Zenith_Angle=
   !    GeometryInfo(n)%Sensor_Scan_Angle=
   !    GeometryInfo(n)%Source_Zenith_Angle=

      end do


      ! [1.6] Call CRTM_TL model

      !$OMP PARALLEL DO &
      !$OMP PRIVATE ( n )
      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
         if (.not. use_crtm_kmatrix) then
            call da_crtm_tl (1, nchanl, 1, Atmosphere(n),   &
                               Surface(n),      &
                               Atmosphere_TL(n),&
                               Surface_TL(n),   &
                               GeometryInfo(n), &
                               ChannelInfo(inst:inst),  &
                               RTSolution(:,n),   &
                               RTSolution_TL(:,n),&
                               Options(n))
         else
            RTSolution_TL(:,n)%brightness_temperature = 0.
            do l = 1, ChannelInfo(inst)%n_Channels
               do k = kts , kte
                  RTSolution_TL(l,n)%brightness_temperature = RTSolution_TL(l,n)%brightness_temperature + &
                                       iv%instid(inst)%t_jacobian(l,k,n) * Atmosphere_TL(n)%Temperature(k) +  &
                                       iv%instid(inst)%q_jacobian(l,k,n) * Atmosphere_TL(n)%absorber(k,1)
                  if (crtm_cloud) then
                     RTSolution_TL(l,n)%brightness_temperature = RTSolution_TL(l,n)%brightness_temperature + &
                        iv%instid(inst)%water_jacobian(l,k,n)   * Atmosphere_TL(n)%Cloud(1)%Water_Content(k) + &
                        iv%instid(inst)%ice_jacobian(l,k,n)     * Atmosphere_TL(n)%Cloud(2)%Water_Content(k) + &
                        iv%instid(inst)%rain_jacobian(l,k,n)    * Atmosphere_TL(n)%Cloud(3)%Water_Content(k) + &
                        iv%instid(inst)%snow_jacobian(l,k,n)    * Atmosphere_TL(n)%Cloud(4)%Water_Content(k) + &
                        iv%instid(inst)%graupel_jacobian(l,k,n) * Atmosphere_TL(n)%Cloud(5)%Water_Content(k) + &
                        iv%instid(inst)%hail_jacobian(l,k,n)    * Atmosphere_TL(n)%Cloud(6)%Water_Content(k)
                  end if
               end do
               RTSolution_TL(l,n)%brightness_temperature = RTSolution_TL(l,n)%brightness_temperature + &
                                       iv%instid(inst)%ps_jacobian(l,n) * Atmosphere_TL(n)%Level_Pressure(Atmosphere_TL(n)%n_Layers) 
            end do
         endif
      end do
     !$OMP END PARALLEL DO

      !-------------------------------------------------------------------
      ! [1.7] assign Hdx :
      !-------------------------------------------------------------------
      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

         y%instid(inst)%tb(:,n) = y%instid(inst)%tb(:,n) + RTSolution_TL(:,n)%brightness_temperature
	 
         call CRTM_Atmosphere_Destroy( Atmosphere_TL(n) )
         IF ( CRTM_Atmosphere_Associated(Atmosphere_TL(n)) ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Atmosphere_TL Structure"/))
         end if

         call CRTM_Surface_Destroy(Surface_TL(n))
         IF ( CRTM_Surface_Associated(Surface_TL(n)) ) THEN
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface_TL Structure"/))
         end if

      end do  ! end loop for pixels 

      deallocate (temperature)
      deallocate (absorber)
      deallocate (xb_q)
      deallocate (psfc)     

      if (crtm_cloud) then
         deallocate (qcw)
         deallocate (qci)
         deallocate (qrn)
         deallocate (qsn)
         deallocate (qgr)
      end if

      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      ! -------------------------------------------------------------------
         call CRTM_Options_Destroy(Options(n))
         IF ( CRTM_Options_Associated(Options(n)) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Options Structure"/))
         end if

         call CRTM_Surface_Destroy(Surface(n))
         IF ( CRTM_Surface_Associated(Surface(n)) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Surface Structure"/))
         end if
      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
         call CRTM_Atmosphere_Destroy( Atmosphere(n) )
         IF ( CRTM_Atmosphere_Associated(Atmosphere(n)) ) THEN
            call da_error(__FILE__,__LINE__, &
                         (/"Error in deallocatting CRTM Atmosphere Structure"/))
         end if

      end do 

      deallocate( RTSolution, RTSolution_TL, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in deallocatting RTSolution"/))
      END IF

      deallocate( Atmosphere, Atmosphere_TL, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in deallocatting Atmosphere"/))
      END IF

      deallocate( Surface, Surface_TL, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in deallocatting Surface"/))
      END IF

      deallocate( Options, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in deallocatting Options"/))
      END IF

      deallocate( GeometryInfo, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
                      (/"Error in deallocatting GeometryInfo"/))
      END IF

   end do sensor_loop     ! end loop for sensor

   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif

end subroutine da_transform_xtoy_crtm

