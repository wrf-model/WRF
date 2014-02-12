!2345678
       subroutine wrf_drv_HYDRO(HYDRO_dt,grid,its,ite,jts,jte)
          use module_wrf_HYDRO, only: wrf_cpl_HYDRO
          USE module_domain, ONLY : domain 
          implicit none
           integer:: its,ite,jts,jte
          real :: HYDRO_dt
          TYPE ( domain ), INTENT(INOUT) :: grid
!         return

          if(grid%num_nests .lt. 1) then

             call wrf_cpl_HYDRO(HYDRO_dt, grid,its,ite,jts,jte)  

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

