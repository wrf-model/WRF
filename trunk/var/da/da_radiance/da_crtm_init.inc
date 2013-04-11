subroutine da_crtm_init(iv,ob, nsensor)
!------------------------------------------------------------------------------
!  PURPOSE: interface to the initialization subroutine of CRTM
!
!  METHOD:  read CRTM coefs files
!
!  HISTORY: 10/15/2006  added crtm initialization    Tomislava Vukicevic, ATOC, University of Colorado
!           11/09/2006  Updated                      Zhiquan Liu
!           10/24/2007  limit to CRTM init           Tom Auligne
!------------------------------------------------------------------------------

 implicit none 

 type (iv_type), intent (inout) :: iv
 type (y_type) , intent (inout) :: ob
 integer ,       intent (in)    :: nsensor

!
!  local arguments
!------------------- 
 integer   :: n, j, ichan

!
! CRTM local ---------------------------------------------------
!
  integer :: Error_Status
!  character( 256 ) :: SpcCoeff_File
!  character( 256 ) :: TauCoeff_File
  character( 256 ) :: AerosolCoeff_File
  character( 256 ) :: CloudCoeff_File
  character( 256 ) :: EmisCoeff_File 
  character( 256 ) :: File_Path
!  character( 80 ), pointer :: Sensor_Descriptor(:)
!
! end of CRTM local

  call da_trace_entry("da_crtm_init")

!---------------------------------------------------------------------
! 1.0 get CRTM sensor descriptor
!---------------------------------------------------------------------
  allocate(Sensor_Descriptor(nsensor))
  call da_crtm_sensor_descriptor(nsensor,Sensor_Descriptor)
  allocate(ChannelInfo(nsensor))

! CRTM load coefficients
!-----------------------------------------------------------
! 1.1 call CRTM_Init to load coefficients and fill ChannelInfo structure
!-----------------------------------------------------------
  ! input: 
     !SpcCoeff_File     = 'amsua_n15-n16.SpcCoeff.bin'
     !TauCoeff_File     = 'amsua_n15-n16.TauCoeff.bin'
     AerosolCoeff_File = 'AerosolCoeff.bin'
     CloudCoeff_File   = 'CloudCoeff.bin'
     EmisCoeff_File    = 'EmisCoeff.bin'
     File_Path         = 'crtm_coeffs/'
  !----------------------------------------------------------------
  ! ChannelInfo structure contains on output: 
  !
  ! n_channels - integer, total number of channels
  ! Sensor_Index - integer
  ! Channel_Index - integer pointer, index of the channels loaded during initialization
  ! Sensor_Channel - integer pointer, the sensor channel #
  ! Sensor_ID - character pointer, character string containing satellite and sensor descr
  !                                        example: amsre_aqua (Appendix B in User guide)
  ! WMO_Satellite_ID - integer pointer
  ! WMO_Sensor_ID - integer pointer
  !----------------------------------------------------------------- 

     Error_Status = CRTM_Init(Sensor_Descriptor, &
                              ChannelInfo, &
                              AerosolCoeff_File = AerosolCoeff_File, &
                              EmisCoeff_File = EmisCoeff_File , &
                              CloudCoeff_File = CloudCoeff_File, &
                              File_Path = File_Path) 

     if ( Error_Status /= 0 ) then 
       call da_error(__FILE__,__LINE__, &
         (/"Error in initializing CRTM"/))
     END IF

     iv%instid(1:nsensor)%nlevels = kme-kms+1

    if (print_detail_rad) then
       do n = 1, nsensor
          write (6,*) 'in da_crtm_init: ChannelInfo content'
          write (6,*) 'Sensor_Index ',ChannelInfo(n)%Sensor_Index
          write (6,*) 'n_channels ',ChannelInfo(n)%n_channels
          write (6,*) 'Channel_Index ',ChannelInfo(n)%Channel_Index(:)
          write (6,*) 'Sensor_Channel ',ChannelInfo(n)%Sensor_Channel(:)
          write (6,*) 'Sensor_ID ',ChannelInfo(n)%Sensor_ID
          write (6,*) 'WMO_Satellite_ID ',ChannelInfo(n)%WMO_Satellite_ID
          write (6,*) 'WMO_Sensor_ID ',ChannelInfo(n)%WMO_Sensor_ID
       end do
    end if

  call da_trace_exit("da_crtm_init")

end subroutine da_crtm_init
