!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module module_mapinfo

  implicit none

  type MapInfoStruct
     character(len=2) :: hproj         ! Two-character abbreviation identifying map projection
     integer          :: supmap_jproj  ! Integer as used in NCAR-Graphics subroutine SUPMAP to identify map projection
     integer          :: nx
     integer          :: ny
     real             :: dx            ! Grid spacing in km or degrees as appropriate
     real             :: dy            ! Grid spacing in km or degrees as appropriate
     real             :: lat1          ! We need to make this a reference latitude?
     real             :: lon1          ! We need to make this a reference longitude?
     real             :: lat2          ! For Cylindrical Equidistant grids
     real             :: lon2          ! For Cylindrical Equidistant grids
     real             :: xlonc
     real             :: truelat1
     real             :: truelat2
  end type MapInfoStruct

contains
  subroutine print_mapinfo(mapinfo)
    implicit none
    type (MapInfoStruct), intent(in) ::  mapinfo
    write(*,'("MAPINFO:  ")')
    write(*,'(4x, "       HPROJ = ", A2)') mapinfo%hproj
    write(*,'(4x, "SUPMAP_JPROJ = ", I2)') mapinfo%supmap_jproj
    write(*,'(4x, "          NX = ", I6)') mapinfo%nx
    write(*,'(4x, "          NY = ", I6)') mapinfo%ny
    write(*,'(4x, "          DX = ", F12.4)') mapinfo%dx
    write(*,'(4x, "          DY = ", F12.4)') mapinfo%dy
    write(*,'(4x, "        LAT1 = ", F12.4)') mapinfo%lat1
    write(*,'(4x, "        LON1 = ", F12.4)') mapinfo%lon1
    if ( mapinfo%lat2 > -1.E25)     write(*,'(4x, "        LAT2 = ", F12.4)') mapinfo%lat2
    if ( mapinfo%lon2 > -1.E25)     write(*,'(4x, "        LON2 = ", F12.4)') mapinfo%lon2
    if ( mapinfo%xlonc > -1.E25)    write(*,'(4x, "       XLONC = ", F12.4)') mapinfo%xlonc
    if ( mapinfo%truelat1 > -1.E25) write(*,'(4x, "    TRUELAT1 = ", F12.4)') mapinfo%truelat1
    if ( mapinfo%truelat2 > -1.E25) write(*,'(4x, "    TRUELAT2 = ", F12.4)') mapinfo%truelat2
  end subroutine print_mapinfo

end module module_mapinfo
