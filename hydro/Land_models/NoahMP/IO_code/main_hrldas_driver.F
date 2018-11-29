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

program Noah_hrldas_driver
! this is the main program to drive HRLDAS-Noah, HRLDAS-NoahMP, and other Land models.

#ifdef Noah1d
! this is used to drive Noah1d
   use module_noah1d_hrldas_driver, only: land_driver_ini, land_driver_exe
#else
! this is used to drive NoahMP
   use module_noahmp_hrldas_driver, only: land_driver_ini, land_driver_exe
#endif


   implicit none
   integer :: ITIME, NTIME


   call land_driver_ini(NTIME)
#ifdef WRF_HYDRO   
   do ITIME = 1, NTIME
#else
   do ITIME = 0, NTIME
#endif
       call land_driver_exe(ITIME)
   end do
#ifdef WRF_HYDRO
   call hydro_finish() 
#endif


END 

