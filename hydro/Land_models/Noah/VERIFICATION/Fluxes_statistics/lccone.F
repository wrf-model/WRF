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

subroutine lccone (fsplat,ssplat,sign,confac)
  real, parameter :: conv=0.01745329251994
  integer :: sign
  real :: fsplat,ssplat,confac
  if (abs(fsplat-ssplat).lt.1.E-2) then
     confac = sin(fsplat*conv)
  else
     confac = log10(cos(fsplat*conv))-log10(cos(ssplat*conv))
     confac = confac/(log10(tan((45.-float(sign)*fsplat/2.)*conv))-&
          log10(tan((45.-float(sign)*ssplat/2.)*conv)))
  endif
end subroutine lccone
