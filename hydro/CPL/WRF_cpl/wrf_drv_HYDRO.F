!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
!  <brief list of changes to this source file>
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

!2345678
!ywGW       subroutine wrf_drv_HYDRO(HYDRO_dt,grid, config_flags, its,ite,jts,jte)
       subroutine wrf_drv_HYDRO(HYDRO_dt,grid, its,ite,jts,jte)
          use module_wrf_HYDRO, only: wrf_cpl_HYDRO
          USE module_domain, ONLY : domain 
          USE module_configure, ONLY : grid_config_rec_type
       implicit none
          integer:: its,ite,jts,jte
          real :: HYDRO_dt
          TYPE ( domain ), INTENT(INOUT) :: grid
!ywGW          TYPE ( grid_config_rec_type ), INTENT(IN) :: config_flags
          TYPE ( grid_config_rec_type ) :: config_flags
!         return

          if(grid%num_nests .lt. 1) then

!ywGW             call wrf_cpl_HYDRO(HYDRO_dt, grid, config_flags, its,ite,jts,jte)  
             call wrf_cpl_HYDRO(HYDRO_dt, grid, its,ite,jts,jte)  

          endif
       end subroutine wrf_drv_HYDRO


       subroutine wrf_drv_HYDRO_ini(grid,its,ite,jts,jte)
          use module_wrf_HYDRO, only: wrf_cpl_HYDRO
          USE module_domain, ONLY : domain
          implicit none
           integer:: its,ite,jts,jte
          TYPE ( domain ), INTENT(INOUT) :: grid

          if(grid%num_nests .lt. 1) then
!            call wrf_cpl_HYDRO_ini(grid,its,ite,jts,jte)  
          endif

       end subroutine wrf_drv_HYDRO_ini

