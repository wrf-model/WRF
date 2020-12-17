subroutine da_get_innov_vector_crtmk ( it, grid, ob, iv )

   !---------------------------------------------------------------------------
   !  PURPOSE: Calculate innovation vector for radiance data.
   !
   !  METHOD:  d = y - H(x)
   !       1. interpolate grid%xb to obs location
   !       2. call foreward RTM to get simulated bright temperature 
   !       3. obs BT - simulated BT
   !---------------------------------------------------------------------------

   implicit none
   
   integer,           intent(in)    :: it       ! External iteration.
   type (domain),     intent(in)    :: grid     ! first guess state.
   type (y_type),     intent(inout) :: ob       ! Observation structure.
   type (iv_type),    intent(inout) :: iv       ! O-B structure.

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

   real    :: v_p(kms:kme)  ! Model value p at ob hor. location.
   real    :: model_qcw(kms:kme)
   real    :: model_rho(kms:kme)
   real, allocatable :: model_snow(:)  ! snow water equivalent, different from model_snowh,
                                       ! used in calculating reff_water

   ! real    :: model_tm(kms:kme)

   integer :: inst, nchanl, n1,n2

   ! variables for computing clwp
   real    :: clw(kms:kme), dpf(kms:kme)
   real    :: clwp

   ! CRTM local varaibles and types
   integer :: wmo_sensor_id,Error_Status, Allocate_Status

   type (CRTM_RTSolution_type), allocatable :: RTSolution(:,:), RTSolution_K(:,:)
   type (CRTM_Atmosphere_type)   :: Atmosphere(1)
   type (CRTM_Surface_type)      :: Surface(1)
   type (CRTM_Atmosphere_type), allocatable :: Atmosphere_K(:,:)
   type (CRTM_Surface_type),    allocatable :: Surface_K(:,:)
   type (CRTM_GeometryInfo_type) :: GeometryInfo(1)

   real :: t(1), a(1)

   ! WHY? use argument it
   if (it==0) then; write(unit=stdout,fmt='(A)') "WHY? have argument it to"//__FILE__; end if

   alloc_status (:) = 0

   if (trace_use) call da_trace_entry("da_get_innov_vector_crtmk")

   ! CRTM allocation

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
   if (Error_Status /= 0) then 
       call da_error(__FILE__,__LINE__, &
         (/"Error in allocating CRTM Atmosphere Structure"/))
   end if

   Atmosphere(1)%Absorber_ID(1)=H2O_ID
   Atmosphere(1)%Absorber_ID(2)=O3_ID

   if (crtm_cloud) then
      Atmosphere(1)%Cloud(1)%Type=WATER_CLOUD
      Atmosphere(1)%Cloud(2)%Type=ICE_CLOUD
      Atmosphere(1)%Cloud(3)%Type=RAIN_CLOUD
      Atmosphere(1)%Cloud(4)%Type=SNOW_CLOUD
      Atmosphere(1)%Cloud(5)%Type=GRAUPEL_CLOUD
      Atmosphere(1)%Cloud(6)%Type=HAIL_CLOUD
   end if

   !------------------------------------------------------
   ! [1.0] calculate the background bright temperature
   !-------------------------------------------------------
   do inst = 1, iv%num_inst                 ! loop for sensor
      ! if ( iv%instid(inst)%num_rad < 1 ) cycle
      if (iv%instid(inst)%info%n2 < iv%instid(inst)%info%n1) cycle
      num_levs  = kte-kts+1 
      ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if (Error_Status /= 0) then
      !   call da_error(__FILE__,__LINE__, (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! end if
      nchanl    = ChannelInfo(inst)%n_channels

      ! Allocate forward model solution RTSolution array to number of channels
      allocate (RTSolution(ChannelInfo(inst)%n_Channels,1),   &
                RTSolution_K(ChannelInfo(inst)%n_Channels,1), &
                Atmosphere_K(ChannelInfo(inst)%n_Channels,1), &
                Surface_K(ChannelInfo(inst)%n_Channels,1),    &
                STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) then
         call da_error(__FILE__,__LINE__, (/"Error in allocating RTSolution"/))
      end if
             
      ! CRTM Surface Structure
      if (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsua') then
         wmo_sensor_id=WMO_AMSUA
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsub') then
         wmo_sensor_id=WMO_AMSUB
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='amsre') then
         wmo_sensor_id=WMO_AMSRE
      elseif (trim(crtm_sensor_name(rtminit_sensor(inst))) =='ssmi') then
         wmo_sensor_id=WMO_SSMI
      else
         wmo_sensor_id=INVALID_WMO_SENSOR_ID
      end if

      Error_Status = CRTM_Allocate_Surface( nchanl,     &  ! Input
                                   Surface)  ! Output
      if (Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in allocating CRTM Surface Structure Structure"/))
      end if
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

      do n=n1,n2
         ! if ( n > iv%instid(inst)%num_rad ) exit

         ! [1.1] Get horizontal interpolation weights:

         i = iv%instid(inst)%info%i(1,n)
         j = iv%instid(inst)%info%j(1,n)
         dx = iv%instid(inst)%info%dx(1,n)
         dy = iv%instid(inst)%info%dy(1,n)
         dxm = iv%instid(inst)%info%dxm(1,n)
         dym = iv%instid(inst)%info%dym(1,n)

         ! horizontal interpolate grid%xb pressure to ob position for every grid%xb layer
         ! get CRTM pressure Layers 
         do k=kts,kte ! from bottem to top
            v_p(k) = dym*(dxm*grid%xb%p(i,j  ,k) + dx*grid%xb%p(i+1,j  ,k)) &
                + dy *(dxm*grid%xb%p(i,j+1,k) + dx*grid%xb%p(i+1,j+1,k))
            v_p(k) = 0.01*v_p(k)  ! convert Pa to hPa
            Atmosphere(1)%Pressure(kte-k+1)=v_p(k) ! from top to bottom
         end do

         ! determine surface type of obs location
         !-----------------------------------------
         call da_detsurtyp ( grid%xb%snow, grid%xb%xice, grid%xb%landmask,  &
            grid%xb%ivgtyp, grid%xb%isltyp, &
            ims, ime, jms, jme, &
            i, j, dx, dy, dxm, dym, &
            model_isflg,model_ivgtyp, model_isltyp, &
            Surface(1)%Water_Coverage, Surface(1)%Ice_Coverage, &
            Surface(1)%Land_Coverage, Surface(1)%Snow_Coverage )

         call da_interp_lin_2d_partial (grid%xb%snow, iv%instid(inst)%info, 1, n, n, model_snow(n:n) )

         ! [1.2] Interpolate horizontally to ob:
         do k=kts,kte ! from bottom to top
            call da_interp_lin_2d_partial (grid%xb%t(:,:,k), iv%instid(inst)%info,k,n,n,t)
            call da_interp_lin_2d_partial (grid%xb%q(:,:,k), iv%instid(inst)%info,k,n,n,a) 
            Atmosphere(1)%Temperature(kte-k+1) = t(1)
            Atmosphere(1)%Absorber(kte-k+1,1)  = a(1)
            Atmosphere(1)%Absorber(kte-k+1,1) = 1000.0*Atmosphere(1)%Absorber(kte-k+1,1) ! in g/kg

            ! NOTE: WRF high-level q values seems too big, replaced by constants
            if (v_p(k) < 75.0) Atmosphere(1)%Absorber(kte-k+1,1) = 0.001

            call da_interp_lin_2d_partial (grid%xb%qcw(:,:,k), iv%instid(inst)%info,k,n,n, model_qcw(kte-k+1:kte-k+1))
            
            if (crtm_cloud) then
               Atmosphere(1)%Cloud(1)%Water_Content(kte-k+1) = model_qcw(kte-k+1)

               call da_interp_lin_2d_partial (grid%xb%qci(:,:,k), iv%instid(inst)%info,k,n,n, &
                  Atmosphere(1)%Cloud(2)%Water_Content(kte-k+1:kte-k+1) )

               call da_interp_lin_2d_partial (grid%xb%qrn(:,:,k), iv%instid(inst)%info,k,n,n, &
                  Atmosphere(1)%Cloud(3)%Water_Content(kte-k+1:kte-k+1) )

               call da_interp_lin_2d_partial (grid%xb%qsn(:,:,k), iv%instid(inst)%info, k,n,n, &
                  Atmosphere(1)%Cloud(4)%Water_Content(kte-k+1:kte-k+1) )

               call da_interp_lin_2d_partial (grid%xb%qgr(:,:,k), iv%instid(inst)%info, k,n,n, &
                  Atmosphere(1)%Cloud(5)%Water_Content(kte-k+1:kte-k+1) )

               Atmosphere(1)%Cloud(6)%Water_Content(kte-k+1) = 0.0

               call da_interp_lin_2d_partial (grid%xb%rho(:,:,k), iv%instid(inst)%info, k,n,n, &
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

               Atmosphere(1)%Cloud(1)%Effective_Radius(kte-k+1)=0.01  ! in mm
               Atmosphere(1)%Cloud(2)%Effective_Radius(kte-k+1)=0.03  ! in mm
               !Atmosphere(1)%Cloud(3)%Effective_Radius(kte-k+1)=0.3
               !Atmosphere(1)%Cloud(4)%Effective_Radius(kte-k+1)=0.6
               !Atmosphere(1)%Cloud(5)%Effective_Radius(kte-k+1)=0.6
               Atmosphere(1)%Cloud(6)%Effective_Radius(kte-k+1)=0.6

            end if
         end do

         call da_interp_lin_2d_partial (grid%xb%u10,  iv%instid(inst)%info, 1, n, n, model_u10(n:n))
         call da_interp_lin_2d_partial (grid%xb%v10,  iv%instid(inst)%info, 1, n, n, model_v10(n:n))
         call da_interp_lin_2d_partial (grid%xb%psfc, iv%instid(inst)%info, 1, n, n, model_psfc(n:n))

         model_psfc(n) = 0.01*model_psfc(n)           ! convert to hPa
         model_ptop = 0.01*grid%xb%ptop

         ! get CRTM levels (0.005hPa at top) /model full level
         Atmosphere(1)%Level_Pressure(0)=model_ptop   ! to sigma level 51-->sigmaf=0
         Atmosphere(1)%Level_Pressure(Atmosphere(1)%n_Layers)=model_psfc(n) ! to sigma level 1->sigmaf=1

         do k=kts+1,kte
            Atmosphere(1)%Level_Pressure(kte-k+1)= grid%xb%sigmaf(k)*(model_psfc(n)-model_ptop)+model_ptop
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
            call da_interp_lin_2d_partial (grid%xb % tgrn, iv%instid(inst)%info, 1, n, n, model_ts(n:n))
         else
            call da_interp_lin_2d_partial (grid%xb % tsk,  iv%instid(inst)%info, 1, n, n, model_ts(n:n))
         end if

         call da_interp_lin_2d_partial (grid%xb % terr, iv%instid(inst)%info, 1, n, n, model_elv(n:n))

         ! variables for emissivity calculations
         !---------------------------------------- 
         call da_interp_lin_2d_partial (grid%xb % smois,  iv%instid(inst)%info, 1, n, n, model_smois(n:n) )
         call da_interp_lin_2d_partial (grid%xb % tslb,   iv%instid(inst)%info, 1, n, n, model_tslb(n:n) )
         call da_interp_lin_2d_partial (grid%xb % snowh,  iv%instid(inst)%info, 1, n, n, model_snowh(n:n) )
         call da_interp_lin_2d_partial (grid%xb % vegfra, iv%instid(inst)%info, 1, n, n, model_vegfra(n:n) )

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
         ! GeometryInfo(1)%Satellite_Height=830.0
         ! GeometryInfo(1)%Sensor_Scan_Angle=
         ! GeometryInfo(1)%Sensor_Zenith_Angle=
         ! GeometryInfo(1)%Sensor_Scan_Angle=
         ! GeometryInfo(1)%Source_Zenith_Angle=

         ! CRTM Surface parameter data

         if (Surface(1)%Land_Coverage > 0.0) then
            Surface(1)%Land_Type=GRASS_SOIL           ! land type (User guide appendix 3)
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
            Surface(1)%Snow_Type=NEW_SNOW             ! User guide appendix 3
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
            Surface(1)%SensorData%n_channels = nchanl
            Surface(1)%SensorData%Sensor_ID  = wmo_sensor_id 
            Surface(1)%SensorData%Tb(1:nchanl) = ob%instid(inst)%tb(1:nchanl,n)
         end if

         ! CRTM surface/atmosphere K initialization
         do l = 1, ChannelInfo(inst)%n_Channels
            ! -- Copy the adjoint atmosphere structure
            Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_K(l,:) )

            if ( Error_Status /= 0 ) then
               call da_error(__FILE__,__LINE__,  &
                  (/"Error copying Atmosphere_K structure"/))
            end if

            ! -- Copy the adjoint surface structure
            Error_Status = CRTM_Assign_Surface( Surface, Surface_K(l,:) )
       
            if ( Error_Status /= 0 ) then
               call da_error(__FILE__,__LINE__, &
                  (/"Error copying Surface_K structure"/))
            end if
         end do

         ! -- Zero the Adjoint outputs
         ! Important: adjoint variables must be initialized
         call CRTM_Zero_Atmosphere( Atmosphere_K )
         call CRTM_Zero_Surface( Surface_K )

         ! [1.3] assign tb = R^-1 Re :

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
                            RTSolution) !,   &
                            !Options = Options)

         !----------------------------------------------------------------
         ! [2.0] calculate components of innovation vector:
         !----------------------------------------------------------------
         do k = 1, nchanl
            if ( iv%instid(inst)%tb_inv(k,n) > missing_r ) then 
               iv%instid(inst)%tb_xb(k,n)  = RTSolution(k,1)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n) = &
               ob%instid(inst)%tb(k,n) - RTSolution(k,1)%Brightness_Temperature
            else
               iv%instid(inst)%tb_xb(k,n)    = RTSolution(k,1)%Brightness_Temperature
               iv%instid(inst)%tb_inv(k,n)   = missing_r
            end if
            iv%instid(inst)%emiss(k,n) = RTSolution(k,1)%surface_emissivity
            ! surface Jacobian
            iv%instid(inst)%ts_jacobian(k,n) = Surface_k(k,1)%water_temperature
            iv%instid(inst)%windspeed_jacobian(k,n) = Surface_k(k,1)%wind_speed
            iv%instid(inst)%emiss_jacobian(k,n) = RTSolution_k(k,1)%surface_emissivity
         end do

         !----------------------------------------------------------------
         ! [3.0] store base state and Jacobian to innovation structure
         !----------------------------------------------------------------
         ! full level pressures
         iv%instid(inst)%pf(0,n)  = Atmosphere(1)%level_pressure(0)
         do k=1,Atmosphere(1)%n_layers
            iv%instid(inst)%pm(k,n)  = Atmosphere(1)%pressure(k)
            iv%instid(inst)%pf(k,n)  = Atmosphere(1)%level_pressure(k)
            iv%instid(inst)%tm(k,n)  = Atmosphere(1)%temperature(k)
            iv%instid(inst)%qm(k,n)  = Atmosphere(1)%absorber(k,1)
            ! T, Q Jacobian
            do l=1,nchanl
               iv%instid(inst)%t_jacobian(l,k,n) = Atmosphere_k(l,1)%temperature(k)
               iv%instid(inst)%q_jacobian(l,k,n) = Atmosphere_k(l,1)%absorber(k,1)
            end do
            if (crtm_cloud) then
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
               ! Cloud Jacobian
               do l=1,nchanl
                  iv%instid(inst)%water_jacobian(l,k,n) =   &
                     Atmosphere_k(l,1)%cloud(1)%water_content(k)
                  iv%instid(inst)%ice_jacobian(l,k,n) =     &
                     Atmosphere_k(l,1)%cloud(2)%water_content(k)
                  iv%instid(inst)%rain_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(3)%water_content(k)
                  iv%instid(inst)%snow_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(4)%water_content(k)
                  iv%instid(inst)%graupel_jacobian(l,k,n) = &
                     Atmosphere_k(l,1)%cloud(5)%water_content(k)
                  iv%instid(inst)%hail_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(6)%water_content(k)

                  iv%instid(inst)%water_r_jacobian(l,k,n) =   &
                     Atmosphere_k(l,1)%cloud(1)%effective_radius(k)
                  iv%instid(inst)%ice_r_jacobian(l,k,n) =     &
                     Atmosphere_k(l,1)%cloud(2)%effective_radius(k)
                  iv%instid(inst)%rain_r_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(3)%effective_radius(k)
                  iv%instid(inst)%snow_r_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(4)%effective_radius(k)
                  iv%instid(inst)%graupel_r_jacobian(l,k,n) = &
                     Atmosphere_k(l,1)%cloud(5)%effective_radius(k)
                  iv%instid(inst)%hail_r_jacobian(l,k,n) =    &
                     Atmosphere_k(l,1)%cloud(6)%effective_radius(k)
               end do
            end if
         end do

         !----------------------------------------------
         ! [5.0] store surface information to innovation structure
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
      end do       !  end loop for pixels

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

      deallocate( RTSolution, RTSolution_K, STAT = Allocate_Status )

      if (Allocate_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating RTSolution"/))
      end if

      Error_Status = CRTM_Destroy_Surface(Surface)
      if (Error_Status /= 0) then
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocating CRTM Surface Structure"/))
      end if

      Error_Status = CRTM_Destroy_Surface(Surface_K)
      if ( Error_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocatting CRTM Surface_K Structure"/))
      endif

      Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_K )
      if ( Error_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocatting CRTM Atmosphere_K Structure"/))
      endif

      deallocate( Atmosphere_K, Surface_K, STAT = Allocate_Status )
      if ( Allocate_Status /= 0 ) THEN
         call da_error(__FILE__,__LINE__, &
            (/"Error in deallocatting CRTM Surface Structure"/))
      endif

   end do        ! end loop for sensor

   Error_Status = CRTM_Destroy_Atmosphere (Atmosphere)
   if (Error_Status /= 0) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocating CRTM Atmosphere Structure"/))
   end if   

   if (trace_use) call da_trace_exit("da_get_innov_vector_crtmk")
 
end subroutine da_get_innov_vector_crtmk

