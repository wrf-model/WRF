subroutine da_setup_firstguess_wrf_nmm_regional(xbx, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Define/allocate components of WRF model state.
   !---------------------------------------------------------------------------

   implicit none

   type (xbx_type), intent(out)         :: xbx    ! Header & non-gridded vars.

   type (domain), intent(inout)         :: grid

   integer           :: map_util_project
   real              :: x, y, xxc, yyc, lat_cen, lon_cen
  
   real              :: buf(2)

   character(len=24) :: xb_date, an_date
   integer           :: len, seconds, i_grid,  j_grid, m_expand
   real              :: latinc, loninc

   if (trace_use) call da_trace_entry("da_setup_firstguess_wrf_nmm_regional")

   !-----------------------------------------------------------------------
   ! [0.0] check the xb_date for 3DVAR
   !-----------------------------------------------------------------------

   write(unit=xb_date,fmt='(i4.4,2("-",i2.2),"_",i2.2,2(":",i2.2),".0000")')  &
        grid%start_year, grid%start_month, grid%start_day, &
        grid%start_hour, grid%start_minute,grid%start_second

   len = len_trim(ANALYSIS_DATE)

   write(unit=an_date(1:len), fmt='(a)') trim(ANALYSIS_DATE)

   seconds = int(da_diff_seconds(an_date, xb_date))

   if (seconds > ANALYSIS_ACCU) then
      write(unit=message(1),fmt='(A,A,A,A)') &
         "xb_date=",xb_date," an_date=", an_date
      write(unit=message(2),fmt='(A,I6,A,I6)') &
         "diff=",seconds,"   ANALYSIS_ACCU=",ANALYSIS_ACCU
      message(3)="=======> Wrong xb time found???"
      call da_warning(__FILE__,__LINE__,message(1:3))
   end if

   !------------------------------------------------------------------------
   ! [1.0] Read original WRF format first guess:
   !------------------------------------------------------------------------
   
   !------------------------------------------------------------------------
   ! [2.0] Copy header info:
   !------------------------------------------------------------------------

   if ((grid%xp%its == grid%xp%ids) .and. (grid%xp%jts == grid%xp%jds)) then
      buf(1) = grid%xlat(grid%xp%its, grid%xp%jts)
      buf(2) = grid%xlong(grid%xp%its, grid%xp%jts)
   end if
   
   call wrf_dm_bcast_real(buf, 2)
   start_lat=buf(1)
   start_lon=buf(2)

   !------------------------------------------------------------------------
   ! Setup map utility
   !------------------------------------------------------------------------

   call nl_get_map_proj     (grid%id , grid%map_proj)
   call nl_get_truelat1     (grid%id , grid%truelat1)
   call nl_get_truelat2     (grid%id , grid%truelat2)
   call nl_get_dx           (grid%id , grid%dx)
   call nl_get_cen_lat      (grid%id , grid%cen_lat)
   call nl_get_cen_lon      (grid%id , grid%cen_lon)
   call nl_get_moad_cen_lat (grid%id , grid%moad_cen_lat)
   call nl_get_stand_lon    (grid%id , grid%stand_lon)

   phic   = grid%moad_cen_lat
   xlonc  = grid%cen_lon   
   loninc = grid%cf1       
   latinc = grid%cf2       

   truelat1_3dv = grid%truelat1
   truelat2_3dv = grid%truelat2
   pole = 90.0
   dsm = 0.001 * grid%dx

   map_util_project = grid%map_proj
   ! Set map projection in WRFSI world.

   if (grid%map_proj == 0 .or. grid%map_proj == 6) then
      map_util_project = PROJ_LATLON
   else if (grid%map_proj == 1) then
      map_util_project = PROJ_LC
   else if (grid%map_proj == 2) then
      map_util_project = PROJ_PS
   else if (grid%map_proj == 3) then
      map_util_project = PROJ_MERC
   end if

   call da_map_set(map_util_project,grid%cen_lat,grid%cen_lon,   &
                real(grid%xp%ide-grid%xp%ids+2)/2.0, real(grid%xp%jde-grid%xp%jds+2)/2.0, &
                grid%dx,grid%stand_lon,grid%truelat1,grid%truelat2,latinc,loninc,map_info)
   ! Need to set map projection in WRF world.
   map_projection = grid%map_proj
   cone_factor = map_info%cone

   if (print_detail_map) then
      write(unit=stdout, fmt='(a, i6)') &
           'map_proj =', grid%map_proj

      write(unit=stdout, fmt='(a, e16.6)') &
           'cen_lat  =', grid%cen_lat,  &
           'cen_lon  =', grid%cen_lon,  &
           'truelat1 =', grid%truelat1, &
           'truelat2 =', grid%truelat2, &
           'start_lat =', start_lat, &
           'start_lon =', start_lon, &
           'latinc    =', latinc   , &
           'loninc    =', loninc   , &
           'cone_fact =', cone_factor, &
           'dsm      =', dsm
   end if


    mix = grid%xp%ide - grid%xp%ids  + 1
    mjy = grid%xp%jde - grid% xp%jds + 1
    mkz = grid%xp%kde - grid%xp%kds  + 1

   call da_transfer_wrf_nmm_regional_toxb(xbx, grid)

   if (trace_use) call da_trace_exit("da_setup_firstguess_wrf_nmm_regional")

end subroutine da_setup_firstguess_wrf_nmm_regional


