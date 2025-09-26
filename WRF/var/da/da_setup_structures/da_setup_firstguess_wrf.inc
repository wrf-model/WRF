subroutine da_setup_firstguess_wrf(xbx, grid, config_flags, ens)

   !---------------------------------------------------------------------------
   ! Purpose: Define/allocate components of WRF model state.
   !---------------------------------------------------------------------------

   implicit none

   type (xbx_type), intent(out)         :: xbx    ! Header & non-gridded vars.

   type (domain), intent(inout)         :: grid
   type(grid_config_rec_type), intent(in) :: config_flags
   logical, intent(in) :: ens

   integer           :: map_util_project
   real              :: x, y, lat_cen, lon_cen
  
   real              :: buf(2)

   character(len=24) :: xb_date, an_date
   integer           :: len, seconds, i_grid,  j_grid, m_expand


   if (trace_use) call da_trace_entry("da_setup_firstguess_wrf")

   !-----------------------------------------------------------------------
   ! [0.0] check the xb_date for 3DVAR
   !-----------------------------------------------------------------------

   if ( num_fgat_time == 1 ) then
      write(unit=xb_date,fmt='(i4.4,2("-",i2.2),"_",i2.2,2(":",i2.2),".0000")')  &
           grid%start_year, grid%start_month, grid%start_day, &
           grid%start_hour, grid%start_minute,grid%start_second

      len = len_trim(ANALYSIS_DATE)

      write(unit=an_date(1:len), fmt='(a)') trim(ANALYSIS_DATE)

      seconds = int(da_diff_seconds(an_date, xb_date))

      if (seconds > analysis_accu) then
         message(1)="The time of your first guess file is different from the analysis date"
         write(unit=message(2),fmt='(A,A)') &
             "xb_date    = ",xb_date
         write(unit=message(3),fmt='(A,A)') &
             "an_date    = ",an_date
         write(unit=message(4),fmt='(A,I10,A)') &
            "Difference between analysis_date and first guess date is ",seconds," seconds."
         message(5)="Correct your choice of fg file, or change analysis_date or analysis_accu in namelist.input"
         call da_error(__FILE__,__LINE__,message(1:5))
      end if
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
   call nl_get_pole_lat     (grid%id , grid%pole_lat)
   call nl_get_moad_cen_lat (grid%id , grid%moad_cen_lat)
   call nl_get_stand_lon    (grid%id , grid%stand_lon)

   phic = grid%moad_cen_lat
   xlonc = grid%stand_lon

   truelat1_3dv = grid%truelat1
   truelat2_3dv = grid%truelat2
   pole = 90.0
   dsm = 0.001 * grid%dx

   map_util_project = grid%map_proj

   ! Print mapping info to log file(s)
   write(unit=message(1), fmt='(a)') 'Domain mapping info:'
   write(unit=message(2), fmt='(a, i6)') &
        'map_proj =', grid%map_proj
   write(unit=message(3), fmt='(a, e16.6)') &
        'cen_lat   =', grid%cen_lat
   write(unit=message(4), fmt='(a, e16.6)') &
        'cen_lon   =', grid%cen_lon
   write(unit=message(5), fmt='(a, e16.6)') &
        'truelat1  =', grid%truelat1
   write(unit=message(6), fmt='(a, e16.6)') &
        'truelat2  =', grid%truelat2
   write(unit=message(7), fmt='(a, e16.6)') &
        'start_lat =', start_lat
   write(unit=message(8), fmt='(a, e16.6)') &
        'start_lon =', start_lon
   write(unit=message(9), fmt='(a, e16.6)') &
        'pole_lat  =', grid%pole_lat
   write(unit=message(10), fmt='(a, e16.6)') &
        'dsm       =', dsm
   call da_message(message(1:10))

   ! Set map projection in WRFSI world.
   map_util_project = PROJ_LC

   if (grid%map_proj == 0 .or. grid%map_proj == 6 ) then
      map_util_project = PROJ_LATLON

      if (grid%pole_lat < 89.9) then
         write(unit=message(1),fmt='(A,E10.6)')'POLE_LAT = ',grid%pole_lat
         write(unit=message(2),fmt='(A)')"WRFDA does not support rotated cylindrical equidistant projection"
         write(unit=message(3),fmt='(A)')"Choose a first guess file with a valid projection"
         call da_error(__FILE__,__LINE__,message(1:3))
      end if
   else if (grid%map_proj == 1) then
      map_util_project = PROJ_LC
   else if (grid%map_proj == 2) then
      map_util_project = PROJ_PS
   else if (grid%map_proj == 3) then
      map_util_project = PROJ_MERC
   else
      write(unit=message(1),fmt='(A,I6)')'map_proj = ',grid%map_proj
      write(unit=message(2),fmt='(A)')"WRFDA does not support the selected map projection"
      write(unit=message(3),fmt='(A)')"Choose a first guess file with a valid projection"
      call da_error(__FILE__,__LINE__,message(1:3))
   end if

   call da_map_set(map_util_project,grid%cen_lat,grid%cen_lon,   &
                real(grid%xp%ide-grid%xp%ids+2)/2.0, real(grid%xp%jde-grid%xp%jds+2)/2.0, &
                grid%dx,grid%stand_lon,grid%truelat1,grid%truelat2,grid%truelat1,grid%stand_lon,map_info)

   ! Need to set map projection in WRF world.
   map_projection = grid%map_proj

   cone_factor = map_info%cone

   if (.not. global .and. print_detail_map) then
     
      !----------------------------------------------------------------------
      ! Check the ll_to_ij:
      !----------------------------------------------------------------------

      message(1)="Check the map_set correctness::::::::::::::::::::::::"

      ! Domain center:
      call  da_llxy_wrf(map_info, grid%cen_lat, grid%cen_lon, start_x, start_y)
      write(unit=message(2),fmt='("Center: latc,lonc,x,y, Xc, Yc:",6f10.3)') &
                  grid%cen_lat, grid%cen_lon, start_x, start_y, &
                  real(grid%xp%ide-grid%xp%ids+2)/2.0, real(grid%xp%jde-grid%xp%jds+2)/2.0

      start_x = real(grid%xp%ide-grid%xp%ids+2)/2.0
      start_y = real(grid%xp%jde-grid%xp%jds+2)/2.0
      lat_cen = -999.9
      lon_cen = -999.9
      call  da_xyll(map_info, start_x, start_y, lat_cen, lon_cen)
      write(unit=message(3), &
         fmt='("Center: X, Y, latc, lonc, phic, xlonc:",6f10.3)') &
         start_x, start_y, lat_cen, lon_cen,   &
         grid%cen_lat, grid%cen_lon
      call da_message(message(1:3))
   end if

   ! Setup the domain definition for use of the GRAPH:

   coarse_ds = 0.001 * grid%dx
   coarse_ix = grid%e_we - grid%s_we + 1
   coarse_jy = grid%e_sn - grid%s_sn + 1
   start_x = 1.0
   start_y = 1.0

   if( fg_format==fg_format_kma_global) then
   delt_lat = 180.0/real(grid%e_sn - grid%s_sn - 1)
   delt_lon = 360.0/real(grid%e_we - grid%s_we)
   else if( fg_format==fg_format_wrf_arw_global) then
   delt_lat = 180.0/real(grid%e_sn - grid%s_sn)
   delt_lon = 360.0/real(grid%e_we - grid%s_we)
   end if

   !--------------------------------------------------------------------------
   ! [3.0] Interpolate WRF C-grid winds to p points of WRFVAR grid (interpolate 
   ! u to west, v to south?
   !---------------------------------------------------------------------------

   grid%xb % mix = grid%xp%ide - grid%xp%ids + 1
   grid%xb % mjy = grid%xp%jde - grid% xp%jds + 1
   grid%xb % mkz = grid%xp%kde - grid%xp%kds + 1

   grid%xb % ds  = 0.001 * grid%dx

   mix = grid%xb % mix
   mjy = grid%xb % mjy
   mkz = grid%xb % mkz
   
   if ( .not. ens ) then
      call da_transfer_wrftoxb(xbx, grid, config_flags)
   endif

   if (trace_use) call da_trace_exit("da_setup_firstguess_wrf")

end subroutine da_setup_firstguess_wrf


