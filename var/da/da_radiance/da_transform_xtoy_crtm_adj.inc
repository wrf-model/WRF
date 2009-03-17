subroutine da_transform_xtoy_crtm_adj ( cv_size, cv, iv, jo_grad_y, jo_grad_x )

   !---------------------------------------------------------------------------
   ! PURPOSE: transform gradient from obs space to model grid space.
   !
   ! METHOD:  jo_grad_x = H^T jo_grad_y =  - H^T R^-1 ( d - H delta_x )
   !           1. input gradient in obs space and reference state of RTTOV
   !           2. call adjoint of RTM
   !           3. adjoint of interpolation from model grid to obs loc
   !
   !  HISTORY: 11/19/2006 - Creation                       Zhiquan Liu
   !           01/11/2008 - Add crtm_cloud                 Xiaoyan Zhang
   !           11/25/2008 - Zero Jacobian for top level    Tom Auligne
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: cv_size         ! Size of cv array.
   real, intent(inout)            :: cv(1:cv_size)   ! control variables.
   type (x_type), intent(inout)   :: jo_grad_x ! 
   type (y_type),  intent(in)     :: jo_grad_y ! H' delta_x
   type (iv_type), intent(in)     :: iv        ! O-B structure.

   integer, parameter             :: AIRS_Max_Channels = 281
   
#ifdef CRTM

   integer                        :: icld, jcld
   integer                        :: k  ! Index dimension.
   integer                        :: num_rad  ! Number of radiance obs
   integer                        :: inst, nchanl, n
   integer                        :: ipred, npred, gammapred, id
   real                           :: cv_local(1:cv_size)
   real, allocatable              :: q_ad(:,:)
   real, allocatable              :: t_ad(:,:)
   real, allocatable              :: p_ad(:)

!! for crtm_cloud
   real, allocatable              :: qcw(:,:)
   real, allocatable              :: qci(:,:)
   real, allocatable              :: qrn(:,:)
   real, allocatable              :: qsn(:,:)
   real, allocatable              :: qgr(:,:)

!   type(infa_type), pointer       :: info                
!   integer                        :: i,j
!   real                           :: dx,dy,dxm,dym


   ! CRTM local varaibles and types
   integer :: wmo_sensor_id, Error_Status, Allocate_Status
   type (CRTM_RTSolution_type ), allocatable :: RTSolution(:,:),RTSolution_AD(:,:)
   type (CRTM_Atmosphere_type )   :: Atmosphere(1), Atmosphere_AD(1)
   type (CRTM_Surface_type )      :: Surface(1), Surface_AD(1)
   type (CRTM_GeometryInfo_type ) :: GeometryInfo(1)
   type (CRTM_Options_type )      :: Options(1)
   
   integer                        :: ts_index
   integer                        :: nclouds, ncv
   real, allocatable              :: cc_ad(:)
   real                           :: rad_clr            ! RT clear/cloudy radiances
   real, allocatable              :: rad_ovc(:)         ! RT overcast radiances
   real                           :: rad_ad, tb_ad

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

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm_adj")
   
   cv_local(:) = 0.0
   
!----------------------------------------------------------------------------
! CRTM allocation
!
! Atmosphere structure
    Atmosphere(1)%n_Layers=(kte-kts)+1   ! number of vertical levels
    Atmosphere(1)%n_Absorbers=2
    Atmosphere(1)%n_Clouds=0
    Atmosphere(1)%n_Aerosols=0
    if (crtm_cloud) Atmosphere(1)%n_Clouds=6
 
    Error_Status = CRTM_Allocate_Atmosphere( Atmosphere(1)%n_Layers, &
                                             Atmosphere(1)%n_Absorbers, &
                                             Atmosphere(1)%n_Clouds, &
                                             Atmosphere(1)%n_Aerosols, &
                                             Atmosphere)
    if ( Error_Status /= 0 ) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocatting CRTM Atmosphere Structure"/))
    end if
    Atmosphere(1)%Absorber_ID(1)=H2O_ID
    Atmosphere(1)%Absorber_ID(2)=O3_ID
    Atmosphere(1)%Climatology=crtm_atmosphere
 
    if (crtm_cloud) then
       Atmosphere(1)%Cloud(1)%Type=WATER_CLOUD
       Atmosphere(1)%Cloud(2)%Type=ICE_CLOUD
       Atmosphere(1)%Cloud(3)%Type=RAIN_CLOUD
       Atmosphere(1)%Cloud(4)%Type=SNOW_CLOUD
       Atmosphere(1)%Cloud(5)%Type=GRAUPEL_CLOUD
       Atmosphere(1)%Cloud(6)%Type=HAIL_CLOUD
    end if

!-------------------------------------------------------------------------------

   do inst = 1, iv%num_inst                 ! loop for sensor
      num_rad = iv%instid(inst)%info%n2 - iv%instid(inst)%info%n1 + 1
      if ( num_rad < 1 ) cycle
      allocate (q_ad(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (t_ad(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (p_ad(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))

      if (crtm_cloud) then
         allocate (qcw(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qci(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qrn(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qsn(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qgr(kts:kte,iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      end if
      
      q_ad = 0.0
      t_ad = 0.0
      p_ad = 0.0

!      info => iv%instid(inst)%info

  ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if ( Error_Status /= 0) then
      !    call da_error(__FILE__,__LINE__, &
      !     (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! end if
      nchanl    = ChannelInfo(inst)%n_channels
 
  ! Allocate forward model solution RTSolution array to number of channels
      allocate( RTSolution( ChannelInfo(inst)%n_Channels, 1 )   , &
                RTSolution_AD( ChannelInfo(inst)%n_Channels, 1 ), &
               STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting RTSolution"/))
      END IF
 
  ! CRTM Surface Structure
      if (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsua') then
         wmo_sensor_id=WMO_AMSUA
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsub') then
         wmo_sensor_id=WMO_AMSUB
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsre') then
         wmo_sensor_id=WMO_AMSRE
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmi') then
         wmo_sensor_id=WMO_SSMI
#ifndef CRTM_1_1
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmis') then
         wmo_sensor_id=WMO_SSMIS   ! available since CRTM_1_2
#endif
      else
         wmo_sensor_id=INVALID_WMO_SENSOR_ID
      end if
 
      Error_Status = CRTM_Allocate_Surface( nchanl,     &  ! Input
                                   Surface)  ! Output
      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Surface Structure"/))
      end if
 
  ! CRTM Options structure
      Options(1)%n_channels = nchanl
      if ( use_antcorr(inst) ) Options(1)%Antenna_Correction = 1   ! SET = 1 in CRTM_Parameters.f90
      Error_Status = CRTM_Allocate_Options( nchanl,    &  ! Input
                                            Options)      ! Output
      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Options Structure"/))
      endif

        ! Gamma correction from VarBC
        !----------------------------
#ifdef CRTM_MODIF
      if (use_varbc) then
        gammapred = iv%instid(inst)%varbc_info%gammapred 
        do k = 1, nchanl
           npred = iv%instid(inst)%varbc(k)%npred
           if (npred <= 0) cycle                                               ! VarBC channels only
           if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle         ! Gamma channels only
           if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle         ! Gamma channels only
           do ipred = 1, npred
              if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
              RTSolution(k,1)%Gamma = iv%instid(inst)%varbc(k)%param(ipred)
           end do
        end do
      end if
      RTSolution_AD(:,1)%Gamma    = 0.0
#endif

      do n =  iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel

      ! [1.0] Extract base state Atmosphere variables
            Atmosphere(1)%level_pressure(0) = iv%instid(inst)%pf(0,n)
           do k=1,Atmosphere(1)%n_layers
            Atmosphere(1)%pressure(k) = iv%instid(inst)%pm(k,n)
            Atmosphere(1)%level_pressure(k) = iv%instid(inst)%pf(k,n)
            Atmosphere(1)%temperature(k) = iv%instid(inst)%tm(k,n)
            Atmosphere(1)%absorber(k,1) = iv%instid(inst)%qm(k,n)
           if (crtm_cloud) then
              Atmosphere(1)%cloud(1)%water_content(k)=iv%instid(inst)%qcw(k,n)
              Atmosphere(1)%cloud(2)%water_content(k)=iv%instid(inst)%qci(k,n)
              Atmosphere(1)%cloud(3)%water_content(k)=iv%instid(inst)%qrn(k,n)
              Atmosphere(1)%cloud(4)%water_content(k)=iv%instid(inst)%qsn(k,n)
              Atmosphere(1)%cloud(5)%water_content(k)=iv%instid(inst)%qgr(k,n)
              Atmosphere(1)%cloud(6)%water_content(k)=iv%instid(inst)%qhl(k,n)
              Atmosphere(1)%cloud(1)%effective_radius(k)=iv%instid(inst)%rcw(k,n)
              Atmosphere(1)%cloud(2)%effective_radius(k)=iv%instid(inst)%rci(k,n)
              Atmosphere(1)%cloud(3)%effective_radius(k)=iv%instid(inst)%rrn(k,n)
              Atmosphere(1)%cloud(4)%effective_radius(k)=iv%instid(inst)%rsn(k,n)
              Atmosphere(1)%cloud(5)%effective_radius(k)=iv%instid(inst)%rgr(k,n)
              Atmosphere(1)%cloud(6)%effective_radius(k)=iv%instid(inst)%rhl(k,n)
           end if
           end do

      ! [1.1] User-supplied emissivity
          ! Options%emissivity_switch = 1
          ! Options%emissivity(1:Options%n_channels) = &
          !     iv%instid(inst)%emiss(1:Options%n_channels,n)

      ! [1.1] CRTM Surface parameter data
     Surface(1)%Land_Coverage=iv%instid(inst)%land_coverage(n)
     Surface(1)%Water_Coverage=iv%instid(inst)%water_coverage(n)
     Surface(1)%Snow_Coverage=iv%instid(inst)%snow_coverage(n)
     Surface(1)%Ice_Coverage=iv%instid(inst)%ice_coverage(n)

     if (Surface(1)%Land_Coverage > 0.0) then
       Surface(1)%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
       Surface(1)%Land_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface(1)%Soil_Moisture_Content=iv%instid(inst)%smois(n)  !0.05    ! volumetric water content (g/cm**3)
       !Surface(1)%Canopy_Water_Content=0.05      ! gravimetric water content
       Surface(1)%Vegetation_Fraction=iv%instid(inst)%vegtyp(n)
       Surface(1)%Soil_Temperature=iv%instid(inst)%tslb(n)
     end if
     if (Surface(1)%Water_Coverage > 0.0) then
       !Surface(1)%Water_Type=SEA_WATER          ! (Currently NOT used)
       Surface(1)%Water_Temperature=iv%instid(inst)%ts(n)     ! K
       Surface(1)%Wind_Speed=sqrt((iv%instid(inst)%u10(n))**2+ &
                               (iv%instid(inst)%v10(n))**2)  ! m/sec
       !surface(1)%Wind_Direction=0.0            ! NOT used
       Surface(1)%Salinity=33.0                  ! ppmv
     end if
     if (Surface(1)%Snow_Coverage > 0.0) then
       Surface(1)%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface(1)%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface(1)%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
       !Surface(1)%Snow_Density=0.2               ! g/cm**3
       !Surface(1)%Snow_Grain_Size=2.0            ! mm
     end if
     if (Surface(1)%Ice_Coverage > 0.0) then
       !Surface(1)%Ice_Type=FRESH_ICE             ! NO Table offered, single example is FRESH_ICE
       Surface(1)%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
       Surface(1)%Ice_Thickness=10.0              ! mm
       !Surface(1)%Ice_Density=0.9                ! g/cm**3
       !Surface(1)%Ice_Roughness=0.0               ! NO Table offered, single example is ZERO
     end if
     Surface(1)%SensorData%n_channels = nchanl
#ifdef CRTM_1_1
     Surface(1)%SensorData%Sensor_Id  = wmo_sensor_id             ! CRTM_1.1
#else
     Surface(1)%SensorData%Select_WMO_Sensor_Id  = wmo_sensor_id  ! CRTM_1.2
#endif
     Surface(1)%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                          iv%instid(inst)%tb_xb(1:nchanl,n)
     
      ! -- Copy the adjoint atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_AD )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_AD structure"/))
      END IF

      ! -- Copy the adjoint surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_AD )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_AD structure"/))
      END IF

    ! -- Zero the Adjoint outputs 
    ! Important: adjoint variables must be initialized
       call CRTM_Zero_Atmosphere( Atmosphere_AD )
       call CRTM_Zero_Surface( Surface_AD )

      ! [1.2] CRTM GeometryInfo Structure
        GeometryInfo(1)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo(1)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
        GeometryInfo(1)%iFOV=iv%instid(inst)%scanpos(n)
   !     GeometryInfo(1)%Satellite_Height=830.0
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Sensor_Zenith_Angle=
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Source_Zenith_Angle=
	 
      ! [1.3] assign tb = R^-1 Re :

         RTSolution_AD(:,1)%brightness_temperature = jo_grad_y%instid(inst)%tb(:,n)
         RTSolution_AD(:,1)%radiance = 0.0 ! must assign zero, since each call of AD model will return non-zero value

      ! [1.4] Call CRTM_AD model

         call da_crtm_ad (1, nchanl, 1, Atmosphere,   &
                            Surface,      &
                            RTSolution_AD,&
                            GeometryInfo, &
                            ChannelInfo(inst:inst),  &
                            Atmosphere_AD,&
                            Surface_AD,   &
                            RTSolution,   &
                            Options)

      ! Skin Temperature
      !-----------------
	 if (use_satcv(1)) then
            ts_index = iv%instid(inst)%cv_index(n)%ts 	       
  	    if (Surface(1)%Land_Coverage  > 0.0) cv(ts_index) = Surface_AD(1)%Land_Temperature
            if (Surface(1)%Water_Coverage > 0.0) cv(ts_index) = Surface_AD(1)%Water_Temperature
            if (Surface(1)%Snow_Coverage  > 0.0) cv(ts_index) = Surface_AD(1)%Snow_Temperature
            if (Surface(1)%Ice_Coverage   > 0.0) cv(ts_index) = Surface_AD(1)%Ice_Temperature
	 end if 

      ! Cloud cover(s)
      !---------------
	 if (use_satcv(2)) then
	    nclouds = iv%instid(inst)%cv_index(n)%nclouds
	    ncv     = iv%instid(inst)%cv_index(n)%ncv
	    allocate (rad_ovc (nclouds))
	    allocate (cc_ad   (ncv))
	    do k = 1, nchanl
               if (ALL(iv%instid(inst)%ichan(k) /= Bands(:,1))) cycle   ! Only Channels in Band 1 	       
               rad_clr      = iv%instid(inst)%rad_xb(k,n)             
               rad_ovc(:)   = iv%instid(inst)%rad_ovc(k,kte-nclouds+1:kte,n)
	       rad_ad       = 0.0
	       tb_ad        = jo_grad_y%instid(inst)%tb(k,n)
	       call CRTM_Planck_Temperature_AD(inst,k,rad_clr,tb_ad,rad_ad)

	       cc_ad        = rad_ad * (rad_ovc(:)-rad_clr)
!	       !---------------------------------------------------------------
!              ! Change of variable (preconditioning) 
!              !---------------------------------------------------------------
!	       do icld = 1, ncv
!	          cc_ad(icld) = SUM(rad_ad * (rad_ovc(:)-rad_clr) * &
!		                iv%instid(inst)%cv_index(n)%vtox(icld,:) )
!	       end do

	       cv(iv%instid(inst)%cv_index(n)%cc) = cv(iv%instid(inst)%cv_index(n)%cc) + cc_ad
	    end do	
	    deallocate(rad_ovc, cc_ad)
	 end if 

      ! [1.5] Scale transformation and fill zero for no-control variable

         if (crtm_cloud) then
            do k=kts,kte
               do icld=1,Atmosphere(1)%n_Clouds
                  Atmosphere_AD(1)%Cloud(icld)%Water_Content(k) = &
		        Atmosphere_AD(1)%Cloud(icld)%Water_Content(k) * &
		        (Atmosphere(1)%Level_Pressure(k)- Atmosphere(1)%Level_Pressure(k-1)) / gravity
               enddo
            enddo
         end if

       ! [1.6] Adjoint of Interpolate horizontally from ob to grid:

            do k=kts,kte ! from bottom to top

               if (crtm_cloud) then
                  qcw(k,n)=Atmosphere_AD(1)%Cloud(1)%Water_Content(kte-k+1)
                  qci(k,n)=Atmosphere_AD(1)%Cloud(2)%Water_Content(kte-k+1)
                  qrn(k,n)=Atmosphere_AD(1)%Cloud(3)%Water_Content(kte-k+1)
                  qsn(k,n)=Atmosphere_AD(1)%Cloud(4)%Water_Content(kte-k+1)
                  qgr(k,n)=Atmosphere_AD(1)%Cloud(5)%Water_Content(kte-k+1)
               end if    

               if (atmosphere(1)%pressure(kte-k+1) >= 75.0) &        ! Zero Jacobian for top level(s)
                  q_ad(k,n) = Atmosphere_AD(1)%Absorber(kte-k+1,1) * 1000.0            ! in g/kg
               if (k < kte) &                                        ! Zero Jacobian for top level
                  t_ad(k,n)=Atmosphere_AD(1)%Temperature(kte-k+1)
            end do

            p_ad(n) = Atmosphere_AD(1)%Level_Pressure(atmosphere(1)%n_layers) * 0.01   ! in hPa

            Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_AD )
            if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Atmosphere_AD Structure"/))
            end if

            Error_Status = CRTM_Destroy_Surface(Surface_AD)
            if ( Error_Status /= 0 ) then
              call da_error(__FILE__,__LINE__, &
                 (/"Error in deallocatting CRTM Surface_AD Structure"/))
            end if

         end do       !  end loop for pixels
	 
         ! [1.7] Gamma correction to VarBC
#ifdef CRTM_MODIF
         if (use_varbc) then
           do k = 1, nchanl
              npred = iv%instid(inst)%varbc(k)%npred
              if (npred <= 0) cycle                                               ! VarBC channels only
              if (iv%instid(inst)%varbc_info%npredmax < gammapred)  cycle         ! Gamma channels only
              if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle         ! Gamma channels only
              do ipred = 1, npred
                 if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
                 id = iv%instid(inst)%varbc(k)%index(ipred)
                 cv_local(id) = cv_local(id) + &
                    SUM(RTSolution_AD(k,1)%Gamma * iv%instid(inst)%varbc(k)%vtox(ipred,1:npred))
              end do   
           end do	    
         end if

       ! Gathering Gamma correction across processors
       !--------------------------------------------- 
!!!        call wrf_dm_sum_reals(cv_local, cv)
#endif
      
        if (crtm_cloud) then
           call da_interp_lin_2d_adj_partial(jo_grad_x%qcw(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qcw)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qci(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qci)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qrn(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qrn)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qsn(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qsn)
           call da_interp_lin_2d_adj_partial(jo_grad_x%qgr(:,:,kts:kte),iv%instid(inst)%info, kts,kte, qgr)
        endif

         call da_interp_lin_2d_adj_partial(jo_grad_x%t(:,:,kts:kte),    iv%instid(inst)%info, kts,kte, t_ad)
         call da_interp_lin_2d_adj_partial(jo_grad_x%q(:,:,kts:kte),    iv%instid(inst)%info, kts,kte, q_ad)
         call da_interp_lin_2d_adj_partial(jo_grad_x%psfc, iv%instid(inst)%info, 1,1, p_ad)

         deallocate (q_ad)
         deallocate (t_ad)
         deallocate (p_ad)

         if (crtm_cloud) then
            deallocate (qcw)
            deallocate (qci)
            deallocate (qrn)
            deallocate (qsn)
            deallocate (qgr)
         end if

      !-------------------------------------------------------------------
      ! [2.0] Deallocating CRTM structures
      !-------------------------------------------------------------------
         deallocate( RTSolution, RTSolution_AD, STAT = Allocate_Status )
         if ( Allocate_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
              (/"Error in deallocatting RTSolution"/))
         END IF

         Error_Status = CRTM_Destroy_Options(Options)
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Options Structure"/))
         end if

         Error_Status = CRTM_Destroy_Surface(Surface)
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface Structure"/))
         end if
   end do        ! end loop for sensor

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    end if
       
   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm_adj")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif
 
end subroutine da_transform_xtoy_crtm_adj

