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
   !  HISTORY: 11/16/2006 - Creation            Zhiquan Liu
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)            :: cv_size         ! Size of cv array.
   real, intent(in)               :: cv(1:cv_size)   ! control variables.
   type (domain),  intent(in)     :: grid
   type (y_type),  intent(inout)  :: y        ! H' delta_x
   type (iv_type), intent(in)     :: iv       ! O-B structure.

#ifdef CRTM
   integer                        :: k  ! Index dimension.

   integer           :: inst, num_rad, nchanl, n, icld
   integer           :: ipred, npred, gammapred, id
   real, allocatable :: temperature(:,:)
   real, allocatable :: absorber(:,:)
   real, allocatable :: psfc(:)
!! for crtm_cloud
   real, allocatable :: qcw(:,:),qci(:,:),qrn(:,:),qsn(:,:),qgr(:,:)

   ! CRTM local variables and types
   integer :: wmo_sensor_id, Error_Status, Allocate_Status
   type( CRTM_RTSolution_type ), ALLOCATABLE :: RTSolution(:,:),RTSolution_TL(:,:)
   type( CRTM_Atmosphere_type )   :: Atmosphere(1), Atmosphere_TL(1)
   type( CRTM_Surface_type )      :: Surface(1), Surface_TL(1)
   type( CRTM_GeometryInfo_type ) :: GeometryInfo(1)
   type( CRTM_Options_type )      :: Options(1)
!---------------------------------------------------------

   if ( iv%num_inst < 1 ) return

   if (trace_use) call da_trace_entry("da_transform_xtoy_crtm")

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

  ! CRTM channel information structure
      ! Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptor(inst),ChannelInfo)
      ! if ( Error_Status /= 0) then
      !    call da_error(__FILE__,__LINE__, &
      !    (/"Error in calling CRTM_Set_ChannelInfo"/))
      ! end if
      nchanl    = ChannelInfo(inst)%n_channels
                                        
  ! Allocate forward model solution RTSolution array to number of channels
      allocate( RTSolution( ChannelInfo(inst)%n_Channels, 1 ), &
                RTSolution_TL( ChannelInfo(inst)%n_Channels, 1 ), &
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
      Error_Status = CRTM_Allocate_Options( nchanl,   &  ! Input
                                            Options)     ! InOut
      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error in allocatting CRTM Options Structure"/))
      endif

      allocate (temperature(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (absorber(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
      allocate (psfc(iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))

      temperature(:,:) = 0.0
      absorber(:,:)    = 0.0
      psfc(:)          = 0.0

      if (crtm_cloud) then

         allocate (qcw(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qci(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qrn(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qsn(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         allocate (qgr(Atmosphere(1)%n_layers, iv%instid(inst)%info%n1:iv%instid(inst)%info%n2))
         qcw(:,:) = 0.0
         qci(:,:) = 0.0
         qrn(:,:) = 0.0
         qsn(:,:) = 0.0
         qgr(:,:) = 0.0

      end if

      do k=kts,kte ! from bottom to top
         call da_interp_lin_2d_partial (grid%xa%t(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            temperature(kte-k+1,:))
         call da_interp_lin_2d_partial (grid%xa%q(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            absorber(kte-k+1,:))
         do n=iv%instid(inst)%info%n1, iv%instid(inst)%info%n2
            if (iv%instid(inst)%pm(kte-k+1,n) < 75.0) absorber(kte-k+1,n) = 0.0
         end do

      if (crtm_cloud) then

         call da_interp_lin_2d_partial (grid%xa%qcw(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            qcw(kte-k+1,:))
         call da_interp_lin_2d_partial (grid%xa%qci(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            qci(kte-k+1,:))
         call da_interp_lin_2d_partial (grid%xa%qrn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            qrn(kte-k+1,:))
         call da_interp_lin_2d_partial (grid%xa%qsn(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            qsn(kte-k+1,:))
         call da_interp_lin_2d_partial (grid%xa%qgr(:,:,k), iv%instid(inst)%info, k, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, &
            qgr(kte-k+1,:))
      end if

      end do

      call da_interp_lin_2d_partial (grid%xa%psfc, iv%instid(inst)%info, 1, iv%instid(inst)%info%n1, iv%instid(inst)%info%n2, psfc(:))

     if ( use_varbc .or. freeze_varbc ) then
        ! Gamma correction from VarBC
        !----------------------------
        gammapred = iv%instid(inst)%varbc_info%gammapred 
        do k = 1, nchanl
           npred = iv%instid(inst)%varbc(k)%npred
           if (npred <= 0) cycle                                               ! VarBC channels only
           if (iv%instid(inst)%varbc(k)%pred_use(gammapred) < 0) cycle         ! Gamma channels only
           do ipred = 1, npred
              if (iv%instid(inst)%varbc(k)%ipred(ipred) /= gammapred) cycle
              id = iv%instid(inst)%varbc(k)%index(ipred)
              RTSolution(k,1)%Gamma    = iv%instid(inst)%varbc(k)%param(ipred)
              RTSolution_TL(k,1)%Gamma = SUM(cv(id) * iv%instid(inst)%varbc(k)%vtox(ipred,1:npred))
           end do	  
        end do
        ! if (n <= 1 ) write(6,'(15f8.3)') 'CRTM_TL',RTSolution_TL(1:nchanl,1)%Gamma
     end if

      !do n= 1, num_rad           ! loop for pixel
      do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
            
      ! [1.1] Get horizontal interpolation weights:

      ! [1.3] Extract base state Atmosphere variables 
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

      ! [1.4] User-supplied emissivity
           !Options%emissivity_switch = 1
           !Options%emissivity(1:Options%n_channels) = &
           !    iv%instid(inst)%emiss(1:Options%n_channels,n)

      ! [1.4] CRTM Surface parameter data
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
       Surface(1)%Salinity=33.                   ! ppmv
     end if
     if (Surface(1)%Snow_Coverage > 0.0) then
       Surface(1)%Snow_Type=NEW_SNOW             ! User guide appendix 3
       Surface(1)%Snow_Temperature=iv%instid(inst)%ts(n)      ! K
       Surface(1)%Snow_Depth=iv%instid(inst)%snowh(n)         ! mm
       !Surface(1)%Snow_Density=0.2               ! g/cm**3
       !Surface(1)%Snow_Grain_Size=2.0            ! mm
     end if
     if (Surface(1)%Ice_Coverage > 0.0) then
       !Surface(1)%Ice_Type=FRESH_ICE             ! NO Table offered, single egrid%xample is FRESH_ICE
       Surface(1)%Ice_Temperature=iv%instid(inst)%ts(n)       ! K
       Surface(1)%Ice_Thickness=10.0              ! mm
       !Surface(1)%Ice_Density=0.9                ! g/cm**3
       !Surface(1)%Ice_Roughness=0.0               ! NO Table offered, single egrid%xample is ZERO
     end if
     Surface(1)%SensorData%n_channels = nchanl
     Surface(1)%SensorData%Sensor_ID  = wmo_sensor_id
     Surface(1)%SensorData%Tb(1:nchanl) = iv%instid(inst)%tb_inv(1:nchanl,n) + &
                                           iv%instid(inst)%tb_xb(1:nchanl,n)

      ! -- Copy the TL atmosphere structure
      Error_Status = CRTM_Assign_Atmosphere( Atmosphere, Atmosphere_TL )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Atmosphere_TL structure"/))
      END IF

      ! -- Copy the TL surface structure
      Error_Status = CRTM_Assign_Surface( Surface, Surface_TL )

      if ( Error_Status /= 0 ) then
        call da_error(__FILE__,__LINE__, &
          (/"Error copying Surface_TL structure"/))
      END IF

    ! -- Zero the TL outputs
    ! Important: adjoint variables must be initialized
    call CRTM_Zero_Atmosphere( Atmosphere_TL )
    call CRTM_Zero_Surface( Surface_TL )
       
    Atmosphere_TL(1)%Temperature(kts:kte)             = temperature(kts:kte,n)
    Atmosphere_TL(1)%Absorber(kts:kte,1)              = absorber(kts:kte,n)
    Atmosphere_TL(1)%Level_Pressure(Atmosphere_TL(1)%n_Layers) = psfc(n)

              Atmosphere_TL(1)%Level_Pressure(Atmosphere(1)%n_Layers)= &
                         0.01*Atmosphere_TL(1)%Level_Pressure(Atmosphere(1)%n_Layers)
              Atmosphere_TL(1)%Absorber(:,1) = 1000.0*Atmosphere_TL(1)%Absorber(:,1) ! in g/kg
     if (crtm_cloud) then
        Atmosphere_TL(1)%Cloud(1)%Water_Content(kts:kte)             = qcw(kts:kte,n)
        Atmosphere_TL(1)%Cloud(2)%Water_Content(kts:kte)             = qci(kts:kte,n)
        Atmosphere_TL(1)%Cloud(3)%Water_Content(kts:kte)             = qrn(kts:kte,n)
        Atmosphere_TL(1)%Cloud(4)%Water_Content(kts:kte)             = qsn(kts:kte,n)
        Atmosphere_TL(1)%Cloud(5)%Water_Content(kts:kte)             = qgr(kts:kte,n)
        Atmosphere_TL(1)%Cloud(6)%Water_Content(kts:kte)             = 0.
! convert cloud content unit from kg/kg to kg/m^2
         do k=kts,kte
         do icld=1,Atmosphere(1)%n_Clouds

            Atmosphere_TL(1)%Cloud(icld)%Water_Content(k)=  Atmosphere_TL(1)%Cloud(icld)%Water_Content(k)* &
                 (Atmosphere(1)%Level_Pressure(k)- Atmosphere(1)%Level_Pressure(k-1))*100./gravity
 
         enddo
         enddo
      end if


      ! [1.5] CRTM GeometryInfo Structure
        GeometryInfo(1)%Sensor_Zenith_Angle=iv%instid(inst)%satzen(n)
        GeometryInfo(1)%Source_Zenith_Angle=iv%instid(inst)%solzen(n)
        GeometryInfo(1)%iFOV=iv%instid(inst)%scanpos(n)
   !     GeometryInfo(1)%Satellite_Height=830.0
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Sensor_Zenith_Angle=
   !     GeometryInfo(1)%Sensor_Scan_Angle=
   !     GeometryInfo(1)%Source_Zenith_Angle=

      ! [1.6] Call CRTM_TL model

         call da_crtm_tl (1, nchanl, 1, Atmosphere,   &
                            Surface,      &
                            Atmosphere_TL,&
                            Surface_TL,   &
                            GeometryInfo, &
                            ChannelInfo(inst:inst),  &
                            RTSolution,   &
                            RTSolution_TL,&
                            Options)
      
         !Error_Status = CRTM_Tangent_Linear(Atmosphere,   &
         !                   Surface,      &
         !                   Atmosphere_TL,&
         !                   Surface_TL,   &
         !                   GeometryInfo, &
         !                   ChannelInfo,  &
         !                   RTSolution,   &
         !                   RTSolution_TL)  !,&
         !                   !Options = Options)
         !if ( Error_Status /= 0 ) then
         !     call da_error(__FILE__,__LINE__, &
         !        (/"Error in calling CRTM_Tangent_Linear"/))
         !endif

      !-------------------------------------------------------------------
      ! [1.7] assign Hdx :
      !-------------------------------------------------------------------
         y%instid(inst)%tb(:,n) = RTSolution_TL(:,1)%brightness_temperature
!         if (n <= 15 ) write(6,'(15f8.3)') RTSolution_TL(:,1)%brightness_temperature

         Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_TL )
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Atmosphere_TL Structure"/))
         end if

         Error_Status = CRTM_Destroy_Surface(Surface_TL)
         if ( Error_Status /= 0 ) then
            call da_error(__FILE__,__LINE__, &
               (/"Error in deallocatting CRTM Surface_TL Structure"/))
         end if

      end do  ! end loop for pixels 

      deallocate (temperature)
      deallocate (absorber)
      deallocate (psfc)     

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
         deallocate( RTSolution, RTSolution_TL, STAT = Allocate_Status )
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

   end do     ! end loop for sensor

      !-------------------------------------------------------------------
      ! [3.0] Deallocating CRTM Atmosphere structures
      !-------------------------------------------------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    if ( Error_Status /= 0 ) then
       call da_error(__FILE__,__LINE__, &
         (/"Error in deallocatting CRTM Atmosphere Structure"/))
    end if

   if (trace_use) call da_trace_exit("da_transform_xtoy_crtm")
#else
    call da_error(__FILE__,__LINE__, &
       (/"Must compile with $CRTM option for radiances"/))
#endif

end subroutine da_transform_xtoy_crtm

