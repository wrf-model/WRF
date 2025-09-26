module process_domain_module

   use mpas_mesh
   use target_mesh
   use remapper

   type (mpas_mesh_type), save :: mpas_source_mesh
   type (target_mesh_type), save :: wrf_target_mesh_m, wrf_target_mesh_u, wrf_target_mesh_v
   type (remap_info_type), save :: remap_info_m, remap_info_u, remap_info_v
   real, dimension(:,:), allocatable, target :: xlat_rad, xlon_rad, xlat_u_rad, xlon_u_rad, xlat_v_rad, xlon_v_rad

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_domain
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_domain(n, extra_row, extra_col)
   
      use date_pack
      use gridinfo_module
      use interp_option_module
      use misc_definitions_module
      use module_debug
      use storage_module
   
      implicit none
   
      ! Arguments
      integer, intent(in) :: n
      logical, intent(in) :: extra_row, extra_col
   
      ! Local variables
      integer :: istatus
      integer :: i, t, dyn_opt, &
                 we_dom_s, we_dom_e, sn_dom_s, sn_dom_e, &
                 we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                 sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                 we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                 sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                 idiff, n_times, &
                 west_east_dim, south_north_dim, bottom_top_dim, map_proj, &
                 is_water, is_lake, is_ice, is_urban, i_soilwater, &
                 grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio, sub_x, sub_y, num_land_cat, process_bdy_width
      real :: cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
              dom_dx, dom_dy, pole_lat, pole_lon
      real, dimension(16) :: corner_lats, corner_lons
      real, pointer, dimension(:,:) :: landmask
      real, pointer, dimension(:,:) :: xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v
      logical, allocatable, dimension(:) :: got_this_field, got_const_field
      character (len=19) :: valid_date, temp_date
      character (len=128) :: title, mminlu
      character (len=128), allocatable, dimension(:) :: output_flags, td_output_flags
      character (len=128), dimension(:), pointer :: geogrid_flags

      ! CWH Initialize local pointer variables
      nullify(landmask)
      nullify(xlat)
      nullify(xlon)
      nullify(xlat_u)
      nullify(xlon_u)
      nullify(xlat_v)
      nullify(xlon_v)
      nullify(geogrid_flags)

      ! Compute number of times that we will process
      call geth_idts(end_date(n), start_date(n), idiff)
      call mprintf((idiff < 0),ERROR,'Ending date is earlier than starting date in namelist for domain %i.', i1=n)
   
      n_times = idiff / interval_seconds
   
      ! Check that the interval evenly divides the range of times to process
      call mprintf((mod(idiff, interval_seconds) /= 0),WARN, &
                   'In namelist, interval_seconds does not evenly divide '// &
                   '(end_date - start_date) for domain %i. Only %i time periods '// &
                   'will be processed.', i1=n, i2=n_times)
   
      ! Initialize the storage module
      call mprintf(.true.,LOGFILE,'Initializing storage module')
      call storage_init()
   
      ! 
      ! Do time-independent processing
      ! 
      call get_static_fields(n, dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, map_proj, &
                    we_dom_s, we_dom_e, sn_dom_s, sn_dom_e, &
                    we_patch_s,      we_patch_e, &
                    we_patch_stag_s, we_patch_stag_e, &
                    sn_patch_s,      sn_patch_e, &
                    sn_patch_stag_s, sn_patch_stag_e, &
                    we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                    sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                    mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban, i_soilwater, &
                    grid_id, parent_id, &
                    i_parent_start, j_parent_start, i_parent_end, j_parent_end, &
                    parent_grid_ratio, sub_x, sub_y, &
                    cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                    pole_lat, pole_lon, dom_dx, dom_dy, landmask, xlat, xlon, xlat_u, xlon_u, &
                    xlat_v, xlon_v, corner_lats, corner_lons, title, geogrid_flags)


      allocate(output_flags(num_entries))
      allocate(got_const_field(num_entries))

      do i=1,num_entries
         output_flags(i)    = ' '
         got_const_field(i) = .false.
      end do
   
      ! This call is to process the constant met fields (SST or SEAICE, for example)
      ! That we process constant fields is indicated by the first argument
      call process_single_met_time(.true., temp_date, n, extra_row, extra_col, xlat, xlon, &
                          xlat_u, xlon_u, xlat_v, xlon_v, landmask, &
                          title, dyn_opt, &
                          west_east_dim, south_north_dim, &
                          we_dom_s, we_dom_e, sn_dom_s, sn_dom_e, &
                          we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                          sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                          we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                          sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                          got_const_field, &
                          map_proj, mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban, i_soilwater, &
                          grid_id, parent_id, i_parent_start, &
                          j_parent_start, i_parent_end, j_parent_end, dom_dx, dom_dy, &
                          cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                          pole_lat, pole_lon, parent_grid_ratio, sub_x, sub_y, &
                          corner_lats, corner_lons, output_flags, geogrid_flags, 0)

      !
      ! Begin time-dependent processing
      !

      allocate(td_output_flags(num_entries))
      allocate(got_this_field (num_entries))
   
      ! Loop over all times to be processed for this domain
      do t=0,n_times
   
         call geth_newdate(valid_date, trim(start_date(n)), t*interval_seconds)
         temp_date = ' '

         if (mod(interval_seconds,3600) == 0) then
            write(temp_date,'(a13)') valid_date(1:10)//'_'//valid_date(12:13)
         else if (mod(interval_seconds,60) == 0) then
            write(temp_date,'(a16)') valid_date(1:10)//'_'//valid_date(12:16)
         else
            write(temp_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)
         end if
   
         call mprintf(.true.,STDOUT, ' Processing %s', s1=trim(temp_date))
         call mprintf(.true.,LOGFILE, 'Preparing to process output time %s', s1=temp_date)
   
         do i=1,num_entries
            td_output_flags(i) = output_flags(i)
            got_this_field(i)  = got_const_field(i)
         end do

         if (t > 0) then
            process_bdy_width = process_only_bdy
         else
            process_bdy_width = 0
         end if
   
         call process_single_met_time(.false., temp_date, n, extra_row, extra_col, xlat, xlon, &
                             xlat_u, xlon_u, xlat_v, xlon_v, landmask, &
                             title, dyn_opt, &
                             west_east_dim, south_north_dim, &
                             we_dom_s, we_dom_e, sn_dom_s, sn_dom_e, &
                             we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                             sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                             we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                             sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                             got_this_field, &
                             map_proj, mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban, i_soilwater, &
                             grid_id, parent_id, i_parent_start, &
                             j_parent_start, i_parent_end, j_parent_end, dom_dx, dom_dy, &
                             cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                             pole_lat, pole_lon, parent_grid_ratio, sub_x, sub_y, &
                             corner_lats, corner_lons, td_output_flags, geogrid_flags, process_bdy_width)
   
      end do  ! Loop over n_times


      deallocate(td_output_flags)
      deallocate(got_this_field)

      deallocate(output_flags)
      deallocate(got_const_field)

      if (associated(geogrid_flags)) deallocate(geogrid_flags)
   
      call storage_delete_all()

      istatus = mpas_mesh_free(mpas_source_mesh)

      if (allocated(xlat_rad)) deallocate(xlat_rad)
      if (allocated(xlon_rad)) deallocate(xlon_rad)
      if (allocated(xlat_u_rad)) deallocate(xlat_u_rad)
      if (allocated(xlon_u_rad)) deallocate(xlon_u_rad)
      if (allocated(xlat_v_rad)) deallocate(xlat_v_rad)
      if (allocated(xlon_v_rad)) deallocate(xlon_v_rad)
      istatus = target_mesh_free(wrf_target_mesh_m)
      istatus = target_mesh_free(wrf_target_mesh_u)
      istatus = target_mesh_free(wrf_target_mesh_v)

      istatus = remap_info_free(remap_info_m)
      istatus = remap_info_free(remap_info_u)
      istatus = remap_info_free(remap_info_v)
   
   end subroutine process_domain


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_static_fields
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_static_fields(n, dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, &
                    map_proj,                                                               &
                    we_dom_s,   we_dom_e,   sn_dom_s,        sn_dom_e,                      &
                    we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e,               &
                    sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e,               &
                    we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e,                       &
                    sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e,                       &
                    mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban, i_soilwater, &
                    grid_id, parent_id,                                                     &
                    i_parent_start, j_parent_start, i_parent_end, j_parent_end,             &
                    parent_grid_ratio, sub_x, sub_y,                                        &
                    cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2,          &
                    pole_lat, pole_lon, dom_dx, dom_dy, landmask, xlat, xlon, xlat_u, xlon_u, &
                    xlat_v, xlon_v, corner_lats, corner_lons, title, geogrid_flags)

      use gridinfo_module
      use input_module
      use llxy_module
      use parallel_module
      use storage_module
      use module_debug
      use list_module

      implicit none

      ! Arguments
      integer, intent(in) :: n
      integer, intent(inout) :: dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, &
                                map_proj, &
                                we_dom_s, we_dom_e, sn_dom_s, sn_dom_e, &
                                we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                                sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                                we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                                sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                                is_water, is_lake, is_ice, is_urban, i_soilwater, grid_id, parent_id, &
                                i_parent_start, j_parent_start, i_parent_end, j_parent_end, &
                                parent_grid_ratio, sub_x, sub_y, num_land_cat
      real, pointer, dimension(:,:) :: landmask
      real, intent(inout) :: cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                             dom_dx, dom_dy, pole_lat, pole_lon
      real, pointer, dimension(:,:) :: xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v
      real, dimension(16), intent(out) :: corner_lats, corner_lons
      character (len=128), intent(inout) :: title, mminlu
      character (len=128), dimension(:), pointer :: geogrid_flags
    
      ! Local variables
      integer :: istatus, i, j, k, sp1, ep1, sp2, ep2, sp3, ep3, &
                 lh_mult, rh_mult, bh_mult, th_mult, subx, suby
      integer :: we_mem_subgrid_s, we_mem_subgrid_e, &
                 sn_mem_subgrid_s, sn_mem_subgrid_e
      integer :: we_patch_subgrid_s, we_patch_subgrid_e, &
                 sn_patch_subgrid_s, sn_patch_subgrid_e
      real, pointer, dimension(:,:,:) :: real_array
      character (len=3) :: memorder
      character (len=128) :: grid_type, datestr, cname, stagger, cunits, cdesc, ctemp
      character (len=128), dimension(3) :: dimnames
      type (fg_input) :: field
      type (list) :: static_list, flag_list
      type (list_item), dimension(:), pointer :: static_list_array      

      call list_init(static_list)

      ! CWH Initialize local pointer variables
      nullify(real_array)

      ! Initialize the input module to read static input data for this domain
      call mprintf(.true.,LOGFILE,'Opening static input file.')
      call input_init(n, istatus)
      call mprintf((istatus /= 0),ERROR, 'input_init(): Error opening input for domain %i.', i1=n)
   
      ! Read global attributes from the static data input file 
      call mprintf(.true.,LOGFILE,'Reading static global attributes.')
      call read_global_attrs(title, datestr, grid_type, dyn_opt, west_east_dim,          &
                             south_north_dim, bottom_top_dim,                            &
                             we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e,   &
                             sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e,   &
                             map_proj, mminlu, num_land_cat,                             &
                             is_water, is_lake, is_ice, is_urban, i_soilwater,           &
                             grid_id, parent_id, i_parent_start,                         &
                             j_parent_start, i_parent_end, j_parent_end, dom_dx, dom_dy, &
                             cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1,        &
                             truelat2, pole_lat, pole_lon, parent_grid_ratio,            &
                             corner_lats, corner_lons, sub_x, sub_y)

      we_dom_s = 1
      sn_dom_s = 1
      if (grid_type(1:1) == 'C') then
         we_dom_e = west_east_dim   - 1
         sn_dom_e = south_north_dim - 1
      else if (grid_type(1:1) == 'E') then
         we_dom_e = west_east_dim 
         sn_dom_e = south_north_dim
      end if
     
      ! Given the full dimensions of this domain, find out the range of indices 
      !   that will be worked on by this processor. This information is given by 
      !   my_minx, my_miny, my_maxx, my_maxy
      call parallel_get_tile_dims(west_east_dim, south_north_dim)

      ! Must figure out patch dimensions from info in parallel module
      if (nprocs > 1 .and. .not. do_tiled_input) then

         we_patch_s      = my_minx
         we_patch_stag_s = my_minx
         we_patch_e      = my_maxx - 1
         sn_patch_s      = my_miny
         sn_patch_stag_s = my_miny
         sn_patch_e      = my_maxy - 1

         if (gridtype == 'C') then
            if (my_x /= nproc_x - 1) then
               we_patch_e = we_patch_e + 1
               we_patch_stag_e = we_patch_e
            else
               we_patch_stag_e = we_patch_e + 1
            end if
            if (my_y /= nproc_y - 1) then
               sn_patch_e = sn_patch_e + 1
               sn_patch_stag_e = sn_patch_e
            else
               sn_patch_stag_e = sn_patch_e + 1
            end if
         else if (gridtype == 'E') then
            we_patch_e = we_patch_e + 1
            sn_patch_e = sn_patch_e + 1
            we_patch_stag_e = we_patch_e
            sn_patch_stag_e = sn_patch_e
         end if

      end if

      ! Compute multipliers for halo width; these must be 0/1
      if (my_x /= 0) then
        lh_mult = 1
      else
        lh_mult = 0
      end if
      if (my_x /= (nproc_x-1)) then
        rh_mult = 1
      else
        rh_mult = 0
      end if
      if (my_y /= 0) then
        bh_mult = 1
      else
        bh_mult = 0
      end if
      if (my_y /= (nproc_y-1)) then
        th_mult = 1
      else
        th_mult = 0
      end if

      we_mem_s = we_patch_s - HALO_WIDTH*lh_mult
      we_mem_e = we_patch_e + HALO_WIDTH*rh_mult
      sn_mem_s = sn_patch_s - HALO_WIDTH*bh_mult
      sn_mem_e = sn_patch_e + HALO_WIDTH*th_mult
      we_mem_stag_s = we_patch_stag_s - HALO_WIDTH*lh_mult
      we_mem_stag_e = we_patch_stag_e + HALO_WIDTH*rh_mult
      sn_mem_stag_s = sn_patch_stag_s - HALO_WIDTH*bh_mult
      sn_mem_stag_e = sn_patch_stag_e + HALO_WIDTH*th_mult

      ! Initialize a proj_info type for the destination grid projection. This will
      !   primarily be used for rotating Earth-relative winds to grid-relative winds
      call set_domain_projection(map_proj, stand_lon, truelat1, truelat2, &
                                 dom_dx, dom_dy, dom_dx, dom_dy, west_east_dim, &
                                 south_north_dim, real(west_east_dim)/2., &
                                 real(south_north_dim)/2.,cen_lat, cen_lon, &
                                 cen_lat, cen_lon)
   
      ! Read static fields using the input module; we know that there are no more
      !   fields to be read when read_next_field() returns a non-zero status.
      istatus = 0
      do while (istatus == 0)  
        call read_next_field(sp1, ep1, sp2, ep2, sp3, ep3, cname, cunits, cdesc, &
                             memorder, stagger, dimnames, subx, suby, &
                             real_array, istatus)
        if (istatus == 0) then

          call mprintf(.true.,LOGFILE, 'Read in static field %s.',s1=cname)
          call list_insert(static_list, ckey=cname, cvalue=cname)
   
          ! We will also keep copies in core of the lat/lon arrays, for use in 
          !    interpolation of the met fields to the model grid.
          ! For now, we assume that the lat/lon arrays will have known field names
          if (index(cname, 'XLAT_M') /= 0 .and. &
              len_trim(cname) == len_trim('XLAT_M')) then
             allocate(xlat(we_mem_s:we_mem_e,sn_mem_s:sn_mem_e))
             xlat(we_patch_s:we_patch_e,sn_patch_s:sn_patch_e) = real_array(:,:,1)
             call exchange_halo_r(xlat, & 
                                  we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, 1, 1, &
                                  we_patch_s, we_patch_e, sn_patch_s, sn_patch_e, 1, 1)

          else if (index(cname, 'XLONG_M') /= 0 .and. &
                   len_trim(cname) == len_trim('XLONG_M')) then
             allocate(xlon(we_mem_s:we_mem_e,sn_mem_s:sn_mem_e))
             xlon(we_patch_s:we_patch_e,sn_patch_s:sn_patch_e) = real_array(:,:,1)
             call exchange_halo_r(xlon, & 
                                  we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, 1, 1, &
                                  we_patch_s, we_patch_e, sn_patch_s, sn_patch_e, 1, 1)

          else if (index(cname, 'XLAT_U') /= 0 .and. &
                   len_trim(cname) == len_trim('XLAT_U')) then
             allocate(xlat_u(we_mem_stag_s:we_mem_stag_e,sn_mem_s:sn_mem_e))
             xlat_u(we_patch_stag_s:we_patch_stag_e,sn_patch_s:sn_patch_e) = real_array(:,:,1)
             call exchange_halo_r(xlat_u, & 
                                  we_mem_stag_s, we_mem_stag_e, sn_mem_s, sn_mem_e, 1, 1, &
                                  we_patch_stag_s, we_patch_stag_e, sn_patch_s, sn_patch_e, 1, 1)

          else if (index(cname, 'XLONG_U') /= 0 .and. &
                   len_trim(cname) == len_trim('XLONG_U')) then
             allocate(xlon_u(we_mem_stag_s:we_mem_stag_e,sn_mem_s:sn_mem_e))
             xlon_u(we_patch_stag_s:we_patch_stag_e,sn_patch_s:sn_patch_e) = real_array(:,:,1)
             call exchange_halo_r(xlon_u, & 
                                  we_mem_stag_s, we_mem_stag_e, sn_mem_s, sn_mem_e, 1, 1, &
                                  we_patch_stag_s, we_patch_stag_e, sn_patch_s, sn_patch_e, 1, 1)

          else if (index(cname, 'XLAT_V') /= 0 .and. &
                   len_trim(cname) == len_trim('XLAT_V')) then
             allocate(xlat_v(we_mem_s:we_mem_e,sn_mem_stag_s:sn_mem_stag_e))
             xlat_v(we_patch_s:we_patch_e,sn_patch_stag_s:sn_patch_stag_e) = real_array(:,:,1)
             call exchange_halo_r(xlat_v, & 
                                  we_mem_s, we_mem_e, sn_mem_stag_s, sn_mem_stag_e, 1, 1, &
                                  we_patch_s, we_patch_e, sn_patch_stag_s, sn_patch_stag_e, 1, 1)

          else if (index(cname, 'XLONG_V') /= 0 .and. &
                   len_trim(cname) == len_trim('XLONG_V')) then
             allocate(xlon_v(we_mem_s:we_mem_e,sn_mem_stag_s:sn_mem_stag_e))
             xlon_v(we_patch_s:we_patch_e,sn_patch_stag_s:sn_patch_stag_e) = real_array(:,:,1)
             call exchange_halo_r(xlon_v, & 
                                  we_mem_s, we_mem_e, sn_mem_stag_s, sn_mem_stag_e, 1, 1, &
                                  we_patch_s, we_patch_e, sn_patch_stag_s, sn_patch_stag_e, 1, 1)

          else if (index(cname, 'LANDMASK') /= 0 .and. &
                   len_trim(cname) == len_trim('LANDMASK')) then
             allocate(landmask(we_mem_s:we_mem_e,sn_mem_s:sn_mem_e))
             landmask(we_patch_s:we_patch_e,sn_patch_s:sn_patch_e) = real_array(:,:,1)
             call exchange_halo_r(landmask, & 
                                  we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, 1, 1, &
                                  we_patch_s, we_patch_e, sn_patch_s, sn_patch_e, 1, 1)

          end if

          if (subx > 1) then
             we_mem_subgrid_s   = (we_mem_s                 + HALO_WIDTH*lh_mult - 1)*subx - HALO_WIDTH*lh_mult + 1
             we_mem_subgrid_e   = (we_mem_e   + (1-rh_mult) - HALO_WIDTH*rh_mult    )*subx + HALO_WIDTH*rh_mult
             we_patch_subgrid_s = (we_patch_s                                    - 1)*subx                      + 1
             we_patch_subgrid_e = (we_patch_e + (1-rh_mult)                         )*subx
          end if
          if (suby > 1) then
             sn_mem_subgrid_s   = (sn_mem_s                 + HALO_WIDTH*bh_mult - 1)*suby - HALO_WIDTH*bh_mult + 1
             sn_mem_subgrid_e   = (sn_mem_e   + (1-th_mult) - HALO_WIDTH*th_mult    )*suby + HALO_WIDTH*th_mult
             sn_patch_subgrid_s = (sn_patch_s                                    - 1)*suby                      + 1
             sn_patch_subgrid_e = (sn_patch_e + (1-th_mult)                         )*suby
          end if
    
          ! Having read in a field, we write each level individually to the
          !   storage module; levels will be reassembled later on when they
          !   are written.
          do k=sp3,ep3
             field%header%sr_x=subx
             field%header%sr_y=suby
             field%header%version = 1
             field%header%date = start_date(n) 
             field%header%time_dependent = .false.
             field%header%mask_field = .false.
             field%header%constant_field = .false.
             field%header%forecast_hour = 0.0
             field%header%fg_source = 'geogrid_model'
             field%header%field = cname
             field%header%units = cunits
             field%header%description = cdesc
             field%header%vertical_coord = dimnames(3) 
             field%header%vertical_level = k
             field%header%array_order = memorder
             field%header%is_wind_grid_rel = .true.
             field%header%array_has_missing_values = .false.
             if (gridtype == 'C') then
                if (subx > 1 .or. suby > 1) then
                   field%map%stagger = M
                   field%header%dim1(1) = we_mem_subgrid_s
                   field%header%dim1(2) = we_mem_subgrid_e
                   field%header%dim2(1) = sn_mem_subgrid_s
                   field%header%dim2(2) = sn_mem_subgrid_e
                else if (trim(stagger) == 'M') then
                   field%map%stagger = M
                   field%header%dim1(1) = we_mem_s
                   field%header%dim1(2) = we_mem_e
                   field%header%dim2(1) = sn_mem_s
                   field%header%dim2(2) = sn_mem_e
                else if (trim(stagger) == 'U') then
                   field%map%stagger = U
                   field%header%dim1(1) = we_mem_stag_s
                   field%header%dim1(2) = we_mem_stag_e
                   field%header%dim2(1) = sn_mem_s
                   field%header%dim2(2) = sn_mem_e
                else if (trim(stagger) == 'V') then
                   field%map%stagger = V
                   field%header%dim1(1) = we_mem_s
                   field%header%dim1(2) = we_mem_e
                   field%header%dim2(1) = sn_mem_stag_s
                   field%header%dim2(2) = sn_mem_stag_e
                else if (trim(stagger) == 'CORNER') then
                   field%map%stagger = CORNER
                   field%header%dim1(1) = we_mem_stag_s
                   field%header%dim1(2) = we_mem_stag_e
                   field%header%dim2(1) = sn_mem_stag_s
                   field%header%dim2(2) = sn_mem_stag_e
                end if
             else if (gridtype == 'E') then
                if (trim(stagger) == 'M') then
                   field%map%stagger = HH
                else if (trim(stagger) == 'V') then
                   field%map%stagger = VV
                end if
                field%header%dim1(1) = we_mem_s
                field%header%dim1(2) = we_mem_e
                field%header%dim2(1) = sn_mem_s
                field%header%dim2(2) = sn_mem_e
             end if

             allocate(field%valid_mask)

             if (subx > 1 .or. suby > 1) then
                allocate(field%r_arr(we_mem_subgrid_s:we_mem_subgrid_e,&
                                     sn_mem_subgrid_s:sn_mem_subgrid_e))
                field%r_arr(we_patch_subgrid_s:we_patch_subgrid_e,sn_patch_subgrid_s:sn_patch_subgrid_e) = &
                           real_array(sp1:ep1,sp2:ep2,k)
                call exchange_halo_r(field%r_arr, &
                           we_mem_subgrid_s, we_mem_subgrid_e, sn_mem_subgrid_s, sn_mem_subgrid_e, 1, 1, &
                           we_patch_subgrid_s, we_patch_subgrid_e, sn_patch_subgrid_s, sn_patch_subgrid_e, 1, 1)
                call bitarray_create(field%valid_mask, &
                                     (we_mem_subgrid_e-we_mem_subgrid_s)+1, &
                                     (sn_mem_subgrid_e-sn_mem_subgrid_s)+1)
                do j=1,(sn_mem_subgrid_e-sn_mem_subgrid_s)+1
                   do i=1,(we_mem_subgrid_e-we_mem_subgrid_s)+1
                      call bitarray_set(field%valid_mask, i, j)     
                   end do
                end do

             else if (field%map%stagger == M  .or. & 
                 field%map%stagger == HH .or. &
                 field%map%stagger == VV) then
                allocate(field%r_arr(we_mem_s:we_mem_e,&
                                     sn_mem_s:sn_mem_e))
                field%r_arr(we_patch_s:we_patch_e,sn_patch_s:sn_patch_e) = real_array(sp1:ep1,sp2:ep2,k)
                call exchange_halo_r(field%r_arr, &
                           we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, 1, 1, &
                           we_patch_s, we_patch_e, sn_patch_s, sn_patch_e, 1, 1)
                call bitarray_create(field%valid_mask, &
                                     (we_mem_e-we_mem_s)+1, &
                                     (sn_mem_e-sn_mem_s)+1)
                do j=1,(sn_mem_e-sn_mem_s)+1
                   do i=1,(we_mem_e-we_mem_s)+1
                      call bitarray_set(field%valid_mask, i, j)     
                   end do
                end do
             else if (field%map%stagger == U) then
                allocate(field%r_arr(we_mem_stag_s:we_mem_stag_e,&
                                     sn_mem_s:sn_mem_e))
                field%r_arr(we_patch_stag_s:we_patch_stag_e,sn_patch_s:sn_patch_e) = real_array(sp1:ep1,sp2:ep2,k)
                call exchange_halo_r(field%r_arr, &
                           we_mem_stag_s, we_mem_stag_e, sn_mem_s, sn_mem_e, 1, 1, &
                           we_patch_stag_s, we_patch_stag_e, sn_patch_s, sn_patch_e, 1, 1)
                call bitarray_create(field%valid_mask, &
                                     (we_mem_stag_e-we_mem_stag_s)+1, &
                                     (sn_mem_e-sn_mem_s)+1)
                do j=1,(sn_mem_e-sn_mem_s)+1
                   do i=1,(we_mem_stag_e-we_mem_stag_s)+1
                      call bitarray_set(field%valid_mask, i, j)     
                   end do
                end do
             else if (field%map%stagger == V) then
                allocate(field%r_arr(we_mem_s:we_mem_e,&
                                     sn_mem_stag_s:sn_mem_stag_e))
                field%r_arr(we_patch_s:we_patch_e,sn_patch_stag_s:sn_patch_stag_e) = real_array(sp1:ep1,sp2:ep2,k)
                call exchange_halo_r(field%r_arr, &
                           we_mem_s, we_mem_e, sn_mem_stag_s, sn_mem_stag_e, 1, 1, &
                           we_patch_s, we_patch_e, sn_patch_stag_s, sn_patch_stag_e, 1, 1)
                call bitarray_create(field%valid_mask, &
                                     (we_mem_e-we_mem_s)+1, &
                                     (sn_mem_stag_e-sn_mem_stag_s)+1)
                do j=1,(sn_mem_stag_e-sn_mem_stag_s)+1
                   do i=1,(we_mem_e-we_mem_s)+1
                      call bitarray_set(field%valid_mask, i, j)     
                   end do
                end do
             else if (field%map%stagger == CORNER) then
                allocate(field%r_arr(we_mem_stag_s:we_mem_stag_e,&
                                     sn_mem_stag_s:sn_mem_stag_e))
                field%r_arr(we_patch_stag_s:we_patch_stag_e,sn_patch_stag_s:sn_patch_stag_e) = real_array(sp1:ep1,sp2:ep2,k)
                call exchange_halo_r(field%r_arr, &
                           we_mem_stag_s, we_mem_stag_e, sn_mem_stag_s, sn_mem_stag_e, 1, 1, &
                           we_patch_stag_s, we_patch_stag_e, sn_patch_stag_s, sn_patch_stag_e, 1, 1)
                call bitarray_create(field%valid_mask, &
                                     (we_mem_stag_e-we_mem_stag_s)+1, &
                                     (sn_mem_stag_e-sn_mem_stag_s)+1)
                do j=1,(sn_mem_stag_e-sn_mem_stag_s)+1
                   do i=1,(we_mem_stag_e-we_mem_stag_s)+1
                      call bitarray_set(field%valid_mask, i, j)     
                   end do
                end do
             end if

             nullify(field%modified_mask)
     
             call storage_put_field(field)
    
          end do
    
        end if   ! if (istatus == 0) 
      end do  ! do while (istatus == 0)

      static_list_array => list_get_keys(static_list)
      call list_init(flag_list)
      do i=1,size(static_list_array)
         istatus = 0
         ctemp = 'FLAG_'//trim(static_list_array(i)%ckey)
         call ext_get_dom_ti_integer_scalar(ctemp, istatus, suppress_errors=.true.)
         if (istatus == 1) call list_insert(flag_list, ckey=ctemp, cvalue=ctemp)
      end do
      deallocate(static_list_array)
      call list_destroy(static_list)
    
      ! Done reading all static fields for this domain
      call input_close()

      static_list_array => list_get_keys(flag_list)
      allocate(geogrid_flags(size(static_list_array)))
      do i=1,size(static_list_array)
         geogrid_flags(i) = static_list_array(i)%ckey
      end do
      deallocate(static_list_array)

      call list_destroy(flag_list)

   end subroutine get_static_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_single_met_time
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_single_met_time(do_const_processing, &
                             temp_date, n, extra_row, extra_col, xlat, xlon, &
                             xlat_u, xlon_u, xlat_v, xlon_v, landmask, &
                             title, dyn_opt, &
                             west_east_dim, south_north_dim, &
                             we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                             we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                             sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                             we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                             sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                             got_this_field, &
                             map_proj, mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban, i_soilwater, &
                             grid_id, parent_id, i_parent_start, &
                             j_parent_start, i_parent_end, j_parent_end, dom_dx, dom_dy, &
                             cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                             pole_lat, pole_lon, parent_grid_ratio, sub_x, sub_y, &
                             corner_lats, corner_lons, output_flags, geogrid_flags, process_bdy_width)
   
      use bitarray_module
      use gridinfo_module
      use interp_module
      use interp_option_module
      use llxy_module
      use misc_definitions_module
      use module_debug
      use output_module
      use parallel_module
      use read_met_module
      use rotate_winds_module
      use storage_module
   
      implicit none
   
      ! Arguments
      logical, intent(in) :: do_const_processing
      integer, intent(in) :: n, dyn_opt, west_east_dim, south_north_dim, map_proj, &
                 we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                 we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                 sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                 we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                 sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                 is_water, is_lake, is_ice, is_urban, i_soilwater, &
                 grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio, sub_x, sub_y, num_land_cat, &
                 process_bdy_width
! BUG: Should we be passing these around as pointers, or just declare them as arrays?
      real, pointer, dimension(:,:) :: landmask
      real, intent(in) :: dom_dx, dom_dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, &
                 truelat1, truelat2, pole_lat, pole_lon
      real, dimension(16), intent(in) :: corner_lats, corner_lons
      real, pointer, dimension(:,:) :: xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v
      logical, intent(in) :: extra_row, extra_col
      logical, dimension(:), intent(inout) :: got_this_field
      character (len=19), intent(in) :: temp_date
      character (len=128), intent(in) :: mminlu
      character (len=128), dimension(:), intent(inout) :: output_flags
      character (len=128), dimension(:), pointer :: geogrid_flags

! BUG: Move this constant to misc_definitions_module?
integer, parameter :: BDR_WIDTH = 3
   
      ! Local variables
      integer :: istatus, fg_idx, idx, idxt, i, j, bottom_top_dim, &
                 sm1, em1, sm2, em2, sm3, em3, &
                 sp1, ep1, sp2, ep2, sp3, ep3, &
                 sd1, ed1, sd2, ed2, sd3, ed3, &
                 u_idx
      integer :: nmet_flags
      integer :: num_metgrid_soil_levs
      integer, pointer, dimension(:) :: soil_levels
      real :: rx, ry
      integer, pointer, dimension(:) :: u_levels, v_levels
      real, pointer, dimension(:,:,:) :: real_array
      character (len=19) :: output_date
      character (len=128) :: cname, title
      character (len=MAX_FILENAME_LEN) :: input_name
      character (len=128), allocatable, dimension(:) :: met_flags
      type (fg_input) :: field, u_field, v_field
      type (met_data) :: fg_data

      ! CWH Initialize local pointer variables
      nullify(soil_levels)
      nullify(u_levels)
      nullify(v_levels)
      nullify(real_array)


      ! For this time, we need to process all first-guess filename roots. When we 
      !   hit a root containing a '*', we assume we have hit the end of the list
      fg_idx = 1
      if (do_const_processing) then
         input_name = constants_name(fg_idx)
      else
         input_name = fg_name(fg_idx)
      end if
      do while (input_name /= '*')
   
         call mprintf(.true.,STDOUT, '    %s', s1=input_name)
         call mprintf(.true.,LOGFILE, 'Getting input fields from %s', s1=input_name)

         if (index(input_name, 'mpas:') == 1) then
            call process_mpas_fields(input_name, do_const_processing, temp_date, fg_data, got_this_field, &
                                     landmask, process_bdy_width, &
                                     u_field, v_field, &
                                     dom_dx, dom_dy, &
                                     xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v, &
                                     output_flags, west_east_dim, south_north_dim, &
                                     we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                     we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                                     sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                                     we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                                     sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e )
         else
            call process_intermediate_fields(input_name, do_const_processing, temp_date, fg_data, got_this_field, &
                                             landmask, process_bdy_width, &
                                             u_field, v_field, &
                                             dom_dx, dom_dy, &
                                             xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v, &
                                             output_flags, west_east_dim, south_north_dim, &
                                             we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                             we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                                             sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                                             we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                                             sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e )
         end if
   
         fg_idx = fg_idx + 1
         if (do_const_processing) then
            input_name = constants_name(fg_idx)
         else
            input_name = fg_name(fg_idx)
         end if
      end do ! while (input_name /= '*')

      !
      ! Rotate winds from earth-relative to grid-relative
      !
   
      call storage_get_levels(u_field, u_levels)
      call storage_get_levels(v_field, v_levels)
   
      if (associated(u_levels) .and. associated(v_levels)) then 
         u_idx = 1
         do u_idx = 1, size(u_levels)
            u_field%header%vertical_level = u_levels(u_idx)
            call storage_get_field(u_field, istatus)
            v_field%header%vertical_level = v_levels(u_idx)
            call storage_get_field(v_field, istatus)
  
            if (gridtype == 'C') then
               call met_to_map(u_field%r_arr, u_field%valid_mask, &
                               v_field%r_arr, v_field%valid_mask, &
                               we_mem_stag_s, sn_mem_s, &
                               we_mem_stag_e, sn_mem_e, &
                               we_mem_s, sn_mem_stag_s, &
                               we_mem_e, sn_mem_stag_e, &
                               xlon_u, xlon_v, xlat_u, xlat_v)
            else if (gridtype == 'E') then
               call met_to_map_nmm(u_field%r_arr, u_field%valid_mask, &
                                   v_field%r_arr, v_field%valid_mask, &
                                   we_mem_s, sn_mem_s, &
                                   we_mem_e, sn_mem_e, &
                                   xlat_v, xlon_v)
            end if

         end do

         deallocate(u_levels)
         deallocate(v_levels)

      end if

      if (do_const_processing) return

      !
      ! Now that we have all degribbed fields, we build a 3-d pressure field, and fill in any 
      !   missing levels in the other 3-d fields 
      !
      call mprintf(.true.,LOGFILE,'Filling missing levels.')
      call fill_missing_levels(output_flags)

      call mprintf(.true.,LOGFILE,'Creating derived fields.')
      call create_derived_fields(gridtype, fg_data%hdate, fg_data%xfcst, &
                                 we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                                 we_mem_stag_s, we_mem_stag_e, sn_mem_stag_s, sn_mem_stag_e, &
                                 got_this_field, output_flags)

      !
      ! Derive some MPAS fields from others, e.g., TT from theta and pressure
      !
      call derive_mpas_fields()

      !
      ! Check that every mandatory field was found in input data
      !
      do i=1,num_entries
         if (is_mandatory(i) .and. .not. got_this_field(i)) then
            call mprintf(.true.,ERROR,'The mandatory field %s was not found in any input data.',s1=fieldname(i))
         end if
      end do
       
      !
      ! Before we begin to write fields, if debug_level is set high enough, we 
      !    write a table of which fields are available at which levels to the
      !    metgrid.log file, and then we check to see if any fields are not 
      !    completely covered with data.
      !
      call storage_print_fields()
      call find_missing_values()

      !
      ! All of the processing is now done for this time period for this domain;
      !   now we simply output every field from the storage module.
      !
    
      title = 'OUTPUT FROM METGRID V4.2' 
   
      ! Initialize the output module for this domain and time
      call mprintf(.true.,LOGFILE,'Initializing output module.')
      output_date = temp_date
      if ( .not. nocolons ) then
         if (len_trim(temp_date) == 13) then
            output_date(14:19) = ':00:00' 
         else if (len_trim(temp_date) == 16) then
            output_date(17:19) = ':00' 
         end if
      else
         if (len_trim(temp_date) == 13) then
            output_date(14:19) = '_00_00' 
         else if (len_trim(temp_date) == 16) then
            output_date(17:19) = '_00' 
         end if
      endif

      call output_init(n, title, output_date, gridtype, dyn_opt, &
                       corner_lats, corner_lons, &
                       we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                       we_patch_s,  we_patch_e,  sn_patch_s,  sn_patch_e, &
                       we_mem_s,    we_mem_e,    sn_mem_s,    sn_mem_e, &
                       extra_col, extra_row)
   
      call get_bottom_top_dim(bottom_top_dim)

      ! Add in a flag to tell real that we have seven new msf fields
      nmet_flags = num_entries + 1
      if (associated(geogrid_flags)) nmet_flags = nmet_flags + size(geogrid_flags)
      allocate(met_flags(nmet_flags))
      do i=1,num_entries
         met_flags(i) = output_flags(i)
      end do 
      if (gridtype == 'C') then
         met_flags(num_entries+1) = 'FLAG_MF_XY'
      else
         met_flags(num_entries+1) = ' '
      end if
      if (associated(geogrid_flags)) then
         do i=1,size(geogrid_flags)
            met_flags(num_entries+1+i) = geogrid_flags(i)
         end do
      end if

      ! Find out how many soil levels or layers we have; this assumes a field named ST
      field % header % field = 'ST'
      nullify(soil_levels)
      call storage_get_levels(field, soil_levels)

      if (.not. associated(soil_levels)) then
         field % header % field = 'SOILT'
         nullify(soil_levels)
         call storage_get_levels(field, soil_levels)
      end if

      if (.not. associated(soil_levels)) then
         field % header % field = 'STC_WPS'
         nullify(soil_levels)
         call storage_get_levels(field, soil_levels)
      end if

      if (associated(soil_levels)) then
         num_metgrid_soil_levs = size(soil_levels) 
         deallocate(soil_levels)
      else
         num_metgrid_soil_levs = 0
      end if
   
      ! First write out global attributes
      call mprintf(.true.,LOGFILE,'Writing global attributes to output.')
      call write_global_attrs(title, output_date, gridtype, dyn_opt, west_east_dim,          &
                              south_north_dim, bottom_top_dim,                               &
                              we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e,      &
                              sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e,      &
                              map_proj, mminlu, num_land_cat,                                &
                              is_water, is_lake, is_ice, is_urban, i_soilwater,              &
                              grid_id, parent_id, i_parent_start,                            &
                              j_parent_start, i_parent_end, j_parent_end, dom_dx, dom_dy,    &
                              cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2, &
                              pole_lat, pole_lon, parent_grid_ratio, sub_x, sub_y,           &
                              corner_lats, corner_lons, num_metgrid_soil_levs,               &
                              met_flags, nmet_flags, process_bdy_width)

      deallocate(met_flags)
    
      call reset_next_field()

      istatus = 0
    
      ! Now loop over all output fields, writing each to the output module
      do while (istatus == 0)
         call get_next_output_field(cname, real_array, &
                                    sm1, em1, sm2, em2, sm3, em3, istatus)
         if (istatus == 0) then

            call mprintf(.true.,LOGFILE,'Writing field %s to output.',s1=cname)
            call write_field(sm1, em1, sm2, em2, sm3, em3, &
                             cname, output_date, real_array)
            deallocate(real_array)

         end if
   
      end do

      call mprintf(.true.,LOGFILE,'Closing output file.')
      call output_close()

      ! Free up memory used by met fields for this valid time
      call storage_delete_all_td()
   
   end subroutine process_single_met_time


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_intermediate_fields
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_intermediate_fields(input_name, do_const_processing, temp_date, fg_data, got_this_field, &
                                          landmask, process_bdy_width, &
                                          u_field, v_field, &
                                          dom_dx, dom_dy, &
                                          xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v, &
                                          output_flags, west_east_dim, south_north_dim, &
                                          we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                          we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                                          sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                                          we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                                          sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e )

      use bitarray_module
      use gridinfo_module
      use interp_module
      use interp_option_module
      use llxy_module
      use misc_definitions_module
      use module_debug
      use output_module
      use parallel_module
      use read_met_module
      use rotate_winds_module
      use storage_module

      implicit none

! BUG: Move this constant to misc_definitions_module?
integer, parameter :: BDR_WIDTH = 3

      character (len=*), intent(inout) :: input_name
      logical, intent(in) :: do_const_processing
      character (len=*), intent(in) :: temp_date
      type (met_data), intent(inout) :: fg_data
      logical, dimension(:), intent(inout) :: got_this_field
      real, pointer, dimension(:,:) :: landmask
      integer, intent(in) :: process_bdy_width
      type (fg_input), intent(inout) :: u_field, v_field
      character (len=128), dimension(:), intent(inout) :: output_flags
      real, intent(in) :: dom_dx, dom_dy
      real, pointer, dimension(:,:) :: xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v
      integer, intent(in) :: west_east_dim, south_north_dim, &
                 we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                 we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                 sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                 we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                 sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e

      integer :: istatus
      integer :: idx, idxt, u_idx
      integer :: iqstatus
      real :: threshold
      real, pointer, dimension(:,:) :: halo_slab => null()
      integer, pointer, dimension(:) :: u_levels => null()
      integer, pointer, dimension(:) :: v_levels => null()
      integer :: bdr_wdth
      logical :: do_gcell_interp
      type (fg_input) :: field


      ! Do a first pass through this fg source to get all mask fields used
      !   during interpolation
      call get_interp_masks(trim(input_name), do_const_processing, temp_date)

      istatus = 0

      ! Initialize the module for reading in the met fields
      call read_met_init(trim(input_name), do_const_processing, temp_date, istatus)

      if (istatus == 0) then

         ! Process all fields and levels from the current file; read_next_met_field()
         !   will return a non-zero status when there are no more fields to be read.
         do while (istatus == 0) 
   
            call read_next_met_field(fg_data, istatus)
   
            if (istatus == 0) then
   
               ! Find index into fieldname, interp_method, masked, and fill_missing
               !   of the current field
               idxt = num_entries + 1
               do idx=1,num_entries
                  if ((index(fieldname(idx), trim(fg_data%field)) /= 0) .and. &
                      (len_trim(fieldname(idx)) == len_trim(fg_data%field))) then

                     got_this_field(idx) = .true.

                     if (index(input_name,trim(from_input(idx))) /= 0 .or. &
                        (from_input(idx) == '*' .and. idxt == num_entries + 1)) then
                        idxt = idx
                     end if

                  end if
               end do
               idx = idxt
               if (idx > num_entries) idx = num_entries ! The last entry is a default

               ! Do we need to rename this field?
               if (output_name(idx) /= ' ') then
                  fg_data%field = output_name(idx)(1:9)

                  idxt = num_entries + 1
                  do idx=1,num_entries
                     if ((index(fieldname(idx), trim(fg_data%field)) /= 0) .and. &
                         (len_trim(fieldname(idx)) == len_trim(fg_data%field))) then

                        got_this_field(idx) = .true.

                        if (index(input_name,trim(from_input(idx))) /= 0 .or. &
                           (from_input(idx) == '*' .and. idxt == num_entries + 1)) then
                           idxt = idx
                        end if

                     end if
                  end do
                  idx = idxt
                  if (idx > num_entries) idx = num_entries ! The last entry is a default
               end if

               ! Do a simple check to see whether this is a global dataset
               ! Note that we do not currently support regional Gaussian grids
               if ((fg_data%iproj == PROJ_LATLON .and. abs(fg_data%nx * fg_data%deltalon - 360.) < 0.0001) &
                    .or. (fg_data%iproj == PROJ_GAUSS)) then
                  bdr_wdth = BDR_WIDTH
                  allocate(halo_slab(1-BDR_WIDTH:fg_data%nx+BDR_WIDTH,1:fg_data%ny))

                  halo_slab(1:fg_data%nx,                      1:fg_data%ny) = &
                            fg_data%slab(1:fg_data%nx,              1:fg_data%ny)

                  halo_slab(1-BDR_WIDTH:0,                     1:fg_data%ny) = &
                            fg_data%slab(fg_data%nx-BDR_WIDTH+1:fg_data%nx, 1:fg_data%ny)

                  halo_slab(fg_data%nx+1:fg_data%nx+BDR_WIDTH, 1:fg_data%ny) = &
                            fg_data%slab(1:BDR_WIDTH,       1:fg_data%ny)

                  deallocate(fg_data%slab)
               else
                  bdr_wdth = 0
                  halo_slab => fg_data%slab
                  nullify(fg_data%slab)
               end if

               call mprintf(.true.,LOGFILE,'Processing %s at level %f.',s1=fg_data%field,f1=fg_data%xlvl)               

               call push_source_projection(fg_data%iproj, fg_data%xlonc, fg_data%truelat1, &
                                           fg_data%truelat2, fg_data%dx, fg_data%dy, fg_data%deltalat, &
                                           fg_data%deltalon, fg_data%starti, fg_data%startj, &
                                           fg_data%startlat, fg_data%startlon,  & 
                                           fg_data%pole_lat, fg_data%pole_lon,        &  
                                           fg_data%centerlat, fg_data%centerlon,      &  
                                           real(fg_data%nx+1)/2., real(fg_data%ny+1)/2.,  &  
                                           earth_radius=fg_data%earth_radius*1000.)
   
               ! Initialize fg_input structure to store the field
               field%header%version = 1
               field%header%date = fg_data%hdate//'        '
               if (do_const_processing) then
                  field%header%time_dependent = .false.
                  field%header%constant_field = .true.
               else
                  field%header%time_dependent = .true.
                  field%header%constant_field = .false.
               end if
               field%header%forecast_hour = fg_data%xfcst 
               field%header%fg_source = 'FG'
               field%header%field = ' '
               field%header%field(1:9) = fg_data%field
               field%header%units = ' '
               field%header%units(1:25) = fg_data%units
               field%header%description = ' '
               field%header%description(1:46) = fg_data%desc
               call get_z_dim_name(fg_data%field,field%header%vertical_coord)
               field%header%vertical_level = nint(fg_data%xlvl) 
               field%header%sr_x = 1
               field%header%sr_y = 1
               field%header%array_order = 'XY ' 
               field%header%is_wind_grid_rel = fg_data%is_wind_grid_rel 
               field%header%array_has_missing_values = .false.
               nullify(field%r_arr)
               nullify(field%valid_mask)
               nullify(field%modified_mask)

               if (output_this_field(idx) .and. flag_in_output(idx) /= ' ') then
                  output_flags(idx) = flag_in_output(idx)
               end if

               ! If we should not output this field, just list it as a mask field
               if (output_this_field(idx)) then
                  field%header%mask_field = .false.
               else
                  field%header%mask_field = .true.
               end if
   
               !
               ! Before actually doing any interpolation to the model grid, we must check
               !    whether we will be using the average_gcell interpolator that averages all 
               !    source points in each model grid cell
               !
               do_gcell_interp = .false.
               if (index(interp_method(idx),'average_gcell') /= 0) then
   
                  call get_gcell_threshold(interp_method(idx), threshold, istatus)
                  if (istatus == 0) then
                     if (fg_data%dx == 0. .and. fg_data%dy == 0. .and. &
                         fg_data%deltalat /= 0. .and. fg_data%deltalon /= 0.) then
                        fg_data%dx = abs(fg_data%deltalon)
                        fg_data%dy = abs(fg_data%deltalat)
                     else
! BUG: Need to more correctly handle dx/dy in meters.
                        fg_data%dx = fg_data%dx / 111000.  ! Convert meters to approximate degrees
                        fg_data%dy = fg_data%dy / 111000.
                     end if
                     if (gridtype == 'C') then
                        if (threshold*max(fg_data%dx,fg_data%dy)*111. <= max(dom_dx,dom_dy)/1000.) &
                           do_gcell_interp = .true. 
                     else if (gridtype == 'E') then
                        if (threshold*max(fg_data%dx,fg_data%dy) <= max(dom_dx,dom_dy)) &
                           do_gcell_interp = .true. 
                     end if
                  end if
               end if
   
               ! Interpolate to U staggering
               if (output_stagger(idx) == U) then

                  call storage_query_field(field, iqstatus)
                  if (iqstatus == 0) then
                     call storage_get_field(field, iqstatus)
                     call mprintf((iqstatus /= 0),ERROR,'Queried field %s at level %i and found it,'// &
                                  ' but could not get data.',s1=fg_data%field,i1=nint(fg_data%xlvl))
                     if (associated(field%modified_mask)) then
                        call bitarray_destroy(field%modified_mask)
                        nullify(field%modified_mask)
                     end if
                  else
                     allocate(field%valid_mask)
                     call bitarray_create(field%valid_mask, we_mem_stag_e-we_mem_stag_s+1, sn_mem_e-sn_mem_s+1)
                  end if
   
                  ! Save a copy of the fg_input structure for the U field so that we can find it later
                  if (is_u_field(idx)) call dup(field, u_field)

                  allocate(field%modified_mask)
                  call bitarray_create(field%modified_mask, we_mem_stag_e-we_mem_stag_s+1, sn_mem_e-sn_mem_s+1)

                  if (do_const_processing .or. field%header%time_dependent) then
                     call interp_met_field(input_name, fg_data%field, U, M, &
                                  field, xlat_u, xlon_u, we_mem_stag_s, we_mem_stag_e, sn_mem_s, sn_mem_e, &
                                  we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                  halo_slab, 1-bdr_wdth, fg_data%nx+bdr_wdth, 1, fg_data%ny, bdr_wdth, do_gcell_interp, &
                                  field%modified_mask, process_bdy_width)
                  else
                     call mprintf(.true.,INFORM,' - already processed this field from constant file.')
                  end if

               ! Interpolate to V staggering
               else if (output_stagger(idx) == V) then

                  call storage_query_field(field, iqstatus)
                  if (iqstatus == 0) then
                     call storage_get_field(field, iqstatus)
                     call mprintf((iqstatus /= 0),ERROR,'Queried field %s at level %i and found it,'// &
                                  ' but could not get data.',s1=fg_data%field,i1=nint(fg_data%xlvl))
                     if (associated(field%modified_mask)) then
                        call bitarray_destroy(field%modified_mask)
                        nullify(field%modified_mask)
                     end if
                  else
                     allocate(field%valid_mask)
                     call bitarray_create(field%valid_mask, we_mem_e-we_mem_s+1, sn_mem_stag_e-sn_mem_stag_s+1)
                  end if
   
                  ! Save a copy of the fg_input structure for the V field so that we can find it later
                  if (is_v_field(idx)) call dup(field, v_field)

                  allocate(field%modified_mask)
                  call bitarray_create(field%modified_mask, we_mem_e-we_mem_s+1, sn_mem_stag_e-sn_mem_stag_s+1)

                  if (do_const_processing .or. field%header%time_dependent) then
                     call interp_met_field(input_name, fg_data%field, V, M, &
                                  field, xlat_v, xlon_v, we_mem_s, we_mem_e, sn_mem_stag_s, sn_mem_stag_e, &
                                  we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                  halo_slab, 1-bdr_wdth, fg_data%nx+bdr_wdth, 1, fg_data%ny, bdr_wdth, do_gcell_interp, &
                                  field%modified_mask, process_bdy_width)
                  else
                     call mprintf(.true.,INFORM,' - already processed this field from constant file.')
                  end if
          
               ! Interpolate to VV staggering
               else if (output_stagger(idx) == VV) then

                  call storage_query_field(field, iqstatus)
                  if (iqstatus == 0) then
                     call storage_get_field(field, iqstatus)
                     call mprintf((iqstatus /= 0),ERROR,'Queried field %s at level %i and found it,'// &
                                  ' but could not get data.',s1=fg_data%field,i1=nint(fg_data%xlvl))
                     if (associated(field%modified_mask)) then
                        call bitarray_destroy(field%modified_mask)
                        nullify(field%modified_mask)
                     end if
                  else
                     allocate(field%valid_mask)
                     call bitarray_create(field%valid_mask, we_mem_e-we_mem_s+1, sn_mem_e-sn_mem_s+1)
                  end if
   
                  ! Save a copy of the fg_input structure for the U field so that we can find it later
                  if (is_u_field(idx)) call dup(field, u_field)

                  ! Save a copy of the fg_input structure for the V field so that we can find it later
                  if (is_v_field(idx)) call dup(field, v_field)

                  allocate(field%modified_mask)
                  call bitarray_create(field%modified_mask, we_mem_e-we_mem_s+1, sn_mem_e-sn_mem_s+1)

                  if (do_const_processing .or. field%header%time_dependent) then
                     call interp_met_field(input_name, fg_data%field, VV, M, &
                                  field, xlat_v, xlon_v, we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                                  we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                  halo_slab, 1-bdr_wdth, fg_data%nx+bdr_wdth, 1, fg_data%ny, bdr_wdth, do_gcell_interp, &
                                  field%modified_mask, process_bdy_width)
                  else
                     call mprintf(.true.,INFORM,' - already processed this field from constant file.')
                  end if

               ! All other fields interpolated to M staggering for C grid, H staggering for E grid
               else

                  call storage_query_field(field, iqstatus)
                  if (iqstatus == 0) then
                     call storage_get_field(field, iqstatus)
                     call mprintf((iqstatus /= 0),ERROR,'Queried field %s at level %i and found it,'// &
                                  ' but could not get data.',s1=fg_data%field,i1=nint(fg_data%xlvl))
                     if (associated(field%modified_mask)) then
                        call bitarray_destroy(field%modified_mask)
                        nullify(field%modified_mask)
                     end if
                  else
                     allocate(field%valid_mask)
                     call bitarray_create(field%valid_mask, we_mem_e-we_mem_s+1, sn_mem_e-sn_mem_s+1)
                  end if

                  allocate(field%modified_mask)
                  call bitarray_create(field%modified_mask, we_mem_e-we_mem_s+1, sn_mem_e-sn_mem_s+1)

                  if (do_const_processing .or. field%header%time_dependent) then
                     if (gridtype == 'C') then
                        call interp_met_field(input_name, fg_data%field, M, M, &
                                     field, xlat, xlon, we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                                     we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                     halo_slab, 1-bdr_wdth, fg_data%nx+bdr_wdth, 1, fg_data%ny, bdr_wdth, do_gcell_interp, &
                                     field%modified_mask, process_bdy_width, landmask)
   
                     else if (gridtype == 'E') then
                        call interp_met_field(input_name, fg_data%field, HH, M, &
                                     field, xlat, xlon, we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                                     we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                     halo_slab, 1-bdr_wdth, fg_data%nx+bdr_wdth, 1, fg_data%ny, bdr_wdth, do_gcell_interp, &
                                     field%modified_mask, process_bdy_width, landmask)
                     end if
                  else
                     call mprintf(.true.,INFORM,' - already processed this field from constant file.')
                  end if

               end if

               call bitarray_merge(field%valid_mask, field%modified_mask)

               deallocate(halo_slab)
                            
               ! Store the interpolated field
               call storage_put_field(field)

               call pop_source_projection()

            end if
         end do
   
         call read_met_close()

         call push_source_projection(fg_data%iproj, fg_data%xlonc, fg_data%truelat1, &
                                     fg_data%truelat2, fg_data%dx, fg_data%dy, fg_data%deltalat, &
                                     fg_data%deltalon, fg_data%starti, fg_data%startj, &
                                     fg_data%startlat, fg_data%startlon,  & 
                                     fg_data%pole_lat, fg_data%pole_lon,        &  
                                     fg_data%centerlat, fg_data%centerlon,      &  
                                     real(fg_data%nx+1)/2., real(fg_data%ny+1)/2.,  &  
                                     earth_radius=fg_data%earth_radius*1000.)
   
         !
         ! If necessary, rotate winds to earth-relative for this fg source
         !
   
         call storage_get_levels(u_field, u_levels)
         call storage_get_levels(v_field, v_levels)
   
         if (associated(u_levels) .and. associated(v_levels)) then 
            u_idx = 1
            do u_idx = 1, size(u_levels)
               u_field%header%vertical_level = u_levels(u_idx)
               call storage_get_field(u_field, istatus)
               v_field%header%vertical_level = v_levels(u_idx)
               call storage_get_field(v_field, istatus)

               if (associated(u_field%modified_mask) .and. &
                   associated(v_field%modified_mask)) then
  
                  if (u_field%header%is_wind_grid_rel) then
                     if (gridtype == 'C') then
                        call map_to_met(u_field%r_arr, u_field%modified_mask, &
                                        v_field%r_arr, v_field%modified_mask, &
                                        we_mem_stag_s, sn_mem_s, &
                                        we_mem_stag_e, sn_mem_e, &
                                        we_mem_s, sn_mem_stag_s, &
                                        we_mem_e, sn_mem_stag_e, &
                                        xlon_u, xlon_v, xlat_u, xlat_v)
                     else if (gridtype == 'E') then
                        call map_to_met_nmm(u_field%r_arr, u_field%modified_mask, &
                                            v_field%r_arr, v_field%modified_mask, &
                                            we_mem_s, sn_mem_s, &
                                            we_mem_e, sn_mem_e, &
                                            xlat_v, xlon_v)
                     end if
                  end if

                  call bitarray_destroy(u_field%modified_mask)
                  call bitarray_destroy(v_field%modified_mask)
                  nullify(u_field%modified_mask)
                  nullify(v_field%modified_mask)
                  call storage_put_field(u_field)
                  call storage_put_field(v_field)
               end if

            end do

            deallocate(u_levels)
            deallocate(v_levels)

         end if

         call pop_source_projection()
      
      else
         if (do_const_processing) then
            call mprintf(.true.,WARN,'Couldn''t open file %s for input.',s1=input_name)
         else
            call mprintf(.true.,WARN,'Couldn''t open file %s for input.',s1=trim(input_name)//':'//trim(temp_date))
         end if
      end if

   end subroutine process_intermediate_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_mpas_fields
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_mpas_fields(input_name, do_const_processing, temp_date, fg_data, got_this_field, &
                                          landmask, process_bdy_width, &
                                          u_field, v_field, &
                                          dom_dx, dom_dy, &
                                          xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v, &
                                          output_flags, west_east_dim, south_north_dim, &
                                          we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                                          we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                                          sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                                          we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                                          sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e )

      use bitarray_module
      use gridinfo_module
      use interp_module
      use interp_option_module
      use read_met_module
      use llxy_module
      use misc_definitions_module
      use module_debug
      use output_module
      use parallel_module
      use rotate_winds_module
      use storage_module
      use scan_input
      use mpas_mesh

      implicit none

! BUG: Move this constant to misc_definitions_module?
integer, parameter :: BDR_WIDTH = 3

      character (len=*), intent(inout) :: input_name
      logical, intent(in) :: do_const_processing
      character (len=*), intent(in) :: temp_date
      type (met_data), intent(inout) :: fg_data
      logical, dimension(:), intent(inout) :: got_this_field
      real, pointer, dimension(:,:) :: landmask
      integer, intent(in) :: process_bdy_width
      type (fg_input), intent(inout) :: u_field, v_field
      character (len=128), dimension(:), intent(inout) :: output_flags
      real, intent(in) :: dom_dx, dom_dy
      real, pointer, dimension(:,:) :: xlat, xlon, xlat_u, xlon_u, xlat_v, xlon_v
      integer, intent(in) :: west_east_dim, south_north_dim, &
                 we_domain_s, we_domain_e, sn_domain_s, sn_domain_e, &
                 we_patch_s, we_patch_e, we_patch_stag_s, we_patch_stag_e, &
                 sn_patch_s, sn_patch_e, sn_patch_stag_s, sn_patch_stag_e, &
                 we_mem_s, we_mem_e, we_mem_stag_s, we_mem_stag_e, &
                 sn_mem_s, sn_mem_e, sn_mem_stag_s, sn_mem_stag_e

      real, parameter :: deg2rad = asin(1.0) / 90.0

      integer :: i, j, k
      integer :: idx
      integer :: istat
      integer :: strlen
      character (len=MAX_FILENAME_LEN) :: mpas_filename
      integer :: nRecords
      type (input_handle_type) :: mpas_handle
      type (input_field_type) :: mpas_field
      type (target_field_type) :: wrf_field
      type (fg_input) :: field_to_store

      strlen = len_trim(input_name)
      if (do_const_processing) then
         write(mpas_filename,'(a)') input_name(6:strlen)
      else
         write(mpas_filename,'(a)') input_name(6:strlen)//'.'//trim(temp_date)//'.nc'
      end if
      call mprintf(.true.,LOGFILE,'Processing MPAS fields from file %s',s1=mpas_filename)

      !
      ! If we do not already have mesh information, get that now...
      !
      if (.not. mpas_source_mesh % valid) then
         if (mpas_mesh_setup(mpas_filename, mpas_source_mesh) /= 0) then
            call mprintf(.true.,ERROR, 'Error setting up MPAS mesh %s with scan_input_open', s1=mpas_filename)
         end if
      end if

      !
      ! If we have not already defined the WRF grid, do that now...
      !
      if (.not. wrf_target_mesh_m % valid) then
         allocate(xlat_rad(size(xlat,1), size(xlat,2)))
         allocate(xlon_rad(size(xlat,1), size(xlat,2)))
         xlat_rad(:,:) = xlat(:,:) * deg2rad
         xlon_rad(:,:) = xlon(:,:) * deg2rad
         call mprintf(.true.,LOGFILE,'Need to set up WRF target mass-grid')
         if (target_mesh_setup(wrf_target_mesh_m, lat2d=xlat_rad, lon2d=xlon_rad) /= 0) then
            call mprintf(.true.,ERROR, 'Error setting up WRF target grid')
         end if

         call mprintf(.true.,LOGFILE,'Also computing remapping weights...')
         if (remap_info_setup(mpas_source_mesh, wrf_target_mesh_m, remap_info_m) /= 0) then
            call mprintf(.true.,ERROR, 'Error computing remapping weights from MPAS to WRF grid')
         end if
      else
         call mprintf(.true.,LOGFILE,'Already set up WRF target mass-grid')
      end if

      if (.not. wrf_target_mesh_u % valid) then
         allocate(xlat_u_rad(size(xlat_u,1), size(xlat_u,2)))
         allocate(xlon_u_rad(size(xlat_u,1), size(xlat_u,2)))
         xlat_u_rad(:,:) = xlat_u(:,:) * deg2rad
         xlon_u_rad(:,:) = xlon_u(:,:) * deg2rad
         call mprintf(.true.,LOGFILE,'Need to set up WRF target U-grid')
         if (target_mesh_setup(wrf_target_mesh_u, lat2d=xlat_u_rad, lon2d=xlon_u_rad) /= 0) then
            call mprintf(.true.,ERROR, 'Error setting up WRF target grid')
         end if

         call mprintf(.true.,LOGFILE,'Also computing remapping weights...')
         if (remap_info_setup(mpas_source_mesh, wrf_target_mesh_u, remap_info_u) /= 0) then
            call mprintf(.true.,ERROR, 'Error computing remapping weights from MPAS to WRF grid')
         end if
      else
         call mprintf(.true.,LOGFILE,'Already set up WRF target U-grid')
      end if

      if (.not. wrf_target_mesh_v % valid) then
         allocate(xlat_v_rad(size(xlat_v,1), size(xlat_v,2)))
         allocate(xlon_v_rad(size(xlat_v,1), size(xlat_v,2)))
         xlat_v_rad(:,:) = xlat_v(:,:) * deg2rad
         xlon_v_rad(:,:) = xlon_v(:,:) * deg2rad
         call mprintf(.true.,LOGFILE,'Need to set up WRF target V-grid')
         if (target_mesh_setup(wrf_target_mesh_v, lat2d=xlat_v_rad, lon2d=xlon_v_rad) /= 0) then
            call mprintf(.true.,ERROR, 'Error setting up WRF target grid')
         end if

         call mprintf(.true.,LOGFILE,'Also computing remapping weights...')
         if (remap_info_setup(mpas_source_mesh, wrf_target_mesh_v, remap_info_v) /= 0) then
            call mprintf(.true.,ERROR, 'Error computing remapping weights from MPAS to WRF grid')
         end if
      else
         call mprintf(.true.,LOGFILE,'Already set up WRF target V-grid')
      end if


      if (scan_input_open(mpas_filename, mpas_handle, nRecords) /= 0) then
         call mprintf(.true.,ERROR, 'Error opening %s with scan_input_open', s1=mpas_filename)
      end if


      ! Initialize fg_input structure to store the field
      field_to_store%header%version = 1
      field_to_store%header%date = '?'
      if (do_const_processing) then
         field_to_store%header%time_dependent = .false.
         field_to_store%header%constant_field = .true.
      else
         field_to_store%header%time_dependent = .true.
         field_to_store%header%constant_field = .false.
      end if
      field_to_store%header%forecast_hour = 0.0
      field_to_store%header%fg_source = 'MPAS'
      field_to_store%header%field = ' '
      field_to_store%header%field(1:9) = '?'
      field_to_store%header%units = ' '
      field_to_store%header%units(1:25) = '?'
      field_to_store%header%description = ' '
      field_to_store%header%description(1:46) = '?'
      field_to_store%header%vertical_coord = 'z_dim_name'
      field_to_store%header%vertical_level = 0
      field_to_store%header%sr_x = 1
      field_to_store%header%sr_y = 1
      field_to_store%header%array_order = 'XY ' 
      field_to_store%header%is_wind_grid_rel = .false.
      field_to_store%header%array_has_missing_values = .false.
      nullify(field_to_store%r_arr)
      nullify(field_to_store%valid_mask)
      nullify(field_to_store%modified_mask)

      ! If we should not output this field, just list it as a mask field
!???      if (output_this_field(idx)) then
         field_to_store%header%mask_field = .false.
!???      else
!???         field%header%mask_field = .true.
!???      end if


      do while (scan_input_next_field(mpas_handle, mpas_field) == 0) 

         if (can_remap_field(mpas_field)) then

            ! Here, rename a few MPAS fields that would be difficult to treat
            ! with METGRID.TBL options; principally, these are surface fields
            ! that have different names from their upper-air counterparts.
            if (trim(mpas_field % name) == 'u10') then
               mpas_field % name = 'uReconstructZonal'
            else if (trim(mpas_field % name) == 'v10') then
               mpas_field % name = 'uReconstructMeridional'
            else if (trim(mpas_field % name) == 'q2') then
               mpas_field % name = 'qv'
            else if (trim(mpas_field % name) == 't2m') then
               mpas_field % name = 'theta'
            end if

            ! Mark this MPAS field as "gotten" for any later checks
            ! on mandatory fields
            idx = mpas_name_to_idx(trim(mpas_field % name))
            if (idx > 0) then
               got_this_field(idx) = .true.
               if (output_this_field(idx) .and. flag_in_output(idx) /= ' ') then
                  output_flags(idx) = flag_in_output(idx)
               end if
            else
               istat = scan_input_free_field(mpas_field)
               cycle
            end if
    
            istat = scan_input_read_field(mpas_field, frame=1)
 
            field_to_store%map%stagger = mpas_output_stagger(mpas_field % name)
            if (field_to_store%map%stagger == M) then
               field_to_store%header%dim1(1) = we_mem_s
               field_to_store%header%dim1(2) = we_mem_e
               field_to_store%header%dim2(1) = sn_mem_s
               field_to_store%header%dim2(2) = sn_mem_e
               if (idx > 0) then
                  if (masked(idx) == MASKED_WATER) then
                     istat = remap_field(remap_info_m, mpas_field, wrf_field, masked=.true.)
                  else
                     istat = remap_field(remap_info_m, mpas_field, wrf_field, masked=.false.)
                  end if
               else
                  istat = remap_field(remap_info_m, mpas_field, wrf_field, masked=.false.)
               end if
            else if (field_to_store%map%stagger == U) then
               field_to_store%header%dim1(1) = we_mem_stag_s
               field_to_store%header%dim1(2) = we_mem_stag_e
               field_to_store%header%dim2(1) = sn_mem_s
               field_to_store%header%dim2(2) = sn_mem_e
               istat = remap_field(remap_info_u, mpas_field, wrf_field)
            else if (field_to_store%map%stagger == V) then
               field_to_store%header%dim1(1) = we_mem_s
               field_to_store%header%dim1(2) = we_mem_e
               field_to_store%header%dim2(1) = sn_mem_stag_s
               field_to_store%header%dim2(2) = sn_mem_stag_e
               istat = remap_field(remap_info_v, mpas_field, wrf_field)
            else
               call mprintf(.true.,ERROR, 'Cannot handle requested output stagger %i for MPAS field %s ...', &
                            i1=field_to_store%map%stagger, s1=trim(mpas_field % name))
            end if

            if (wrf_field % ndims == 3 .and. trim(wrf_field % dimnames(1)) == 'nVertLevels') then   ! 3-d MPAS atmosphere field
               field_to_store % header % field = mpas_to_intermediate_name(mpas_field % name)

               ! If no match in the METGRID.TBL was found for this MPAS field, just use the MPAS name
               if (len_trim(field_to_store % header % field) == 0) then
                  field_to_store % header % field = trim(mpas_field % name)
               end if

               field_to_store % header % vertical_coord = 'num_mpas_levels'
               do k=1,wrf_field % dimlens(1)
                  allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
                  allocate(field_to_store % valid_mask)
                  call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
                  do j=1,wrf_field % dimlens(3)
                  do i=1,wrf_field % dimlens(2)
                     call bitarray_set(field_to_store % valid_mask, i, j)
                  end do
                  end do
                  field_to_store % header % vertical_level = k

                  if (wrf_field % xtype == FIELD_TYPE_REAL) then
                     field_to_store % r_arr(:,:) = wrf_field % array3r(k,:,:)
                  else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                     field_to_store % r_arr(:,:) = wrf_field % array3d(k,:,:)
                  end if

                  ! The u_field and v_field are used later by calling code to
                  ! determine which fields represent the x- and y-components of
                  ! horizonal wind velocity for the purposes of wind rotation
                  if (trim(mpas_field % name) == 'uReconstructZonal') then
                     call dup(field_to_store, u_field)
                  end if
                  if (trim(mpas_field % name) == 'uReconstructMeridional') then
                     call dup(field_to_store, v_field)
                  end if

                  call storage_put_field(field_to_store)
               end do

            else if (wrf_field % ndims == 3 .and. trim(wrf_field % dimnames(1)) == 'nVertLevelsP1') then   ! 3-d MPAS atmosphere field
               field_to_store % header % field = mpas_to_intermediate_name(mpas_field % name)

               ! If no match in the METGRID.TBL was found for this MPAS field, just use the MPAS name
               if (len_trim(field_to_store % header % field) == 0) then
                  field_to_store % header % field = trim(mpas_field % name)
               end if

               ! Handle surface level
               field_to_store % header % vertical_coord = 'none'
               allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
               allocate(field_to_store % valid_mask)
               call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
               do j=1,wrf_field % dimlens(3)
               do i=1,wrf_field % dimlens(2)
                  call bitarray_set(field_to_store % valid_mask, i, j)
               end do
               end do
               field_to_store % header % vertical_level = 200100.0

               if (wrf_field % xtype == FIELD_TYPE_REAL) then
                  field_to_store % r_arr(:,:) = wrf_field % array3r(1,:,:)
               else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                  field_to_store % r_arr(:,:) = wrf_field % array3d(1,:,:)
               end if

               call storage_put_field(field_to_store)

               ! Handle all other layers
               field_to_store % header % vertical_coord = 'num_mpas_levels'
               do k=1,wrf_field % dimlens(1) - 1
                  allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
                  allocate(field_to_store % valid_mask)
                  call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
                  do j=1,wrf_field % dimlens(3)
                  do i=1,wrf_field % dimlens(2)
                     call bitarray_set(field_to_store % valid_mask, i, j)
                  end do
                  end do
                  field_to_store % header % vertical_level = k

                  ! Average to layer midpoint
                  if (wrf_field % xtype == FIELD_TYPE_REAL) then
                     field_to_store % r_arr(:,:) = 0.5 * (wrf_field % array3r(k,:,:) + wrf_field % array3r(k+1,:,:))
                  else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                     field_to_store % r_arr(:,:) = 0.5 * (wrf_field % array3d(k,:,:) + wrf_field % array3d(k+1,:,:))
                  end if

                  call storage_put_field(field_to_store)
               end do

               ! Special case: zgrid field also provides SOILHGT
               if (trim(mpas_field % name) == 'zgrid') then
                  field_to_store % header % field = 'SOILHGT'

                  allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
                  allocate(field_to_store % valid_mask)
                  call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
                  do j=1,wrf_field % dimlens(3)
                  do i=1,wrf_field % dimlens(2)
                     call bitarray_set(field_to_store % valid_mask, i, j)
                  end do
                  end do
                  field_to_store % header % vertical_level = 200100.0

                  if (wrf_field % xtype == FIELD_TYPE_REAL) then
                     field_to_store % r_arr(:,:) = wrf_field % array3r(1,:,:)
                  else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                     field_to_store % r_arr(:,:) = wrf_field % array3d(1,:,:)
                  end if

                  call storage_put_field(field_to_store)

                  do idx=1,num_entries
                     if (trim(fieldname(idx)) == 'SOILHGT') then
                        got_this_field(idx) = .true.
                        if (output_this_field(idx) .and. flag_in_output(idx) /= ' ') then
                           output_flags(idx) = flag_in_output(idx)
                        end if
                        exit
                     end if
                  end do
               end if

            else if (wrf_field % ndims == 3 .and. trim(wrf_field % dimnames(1)) == 'nSoilLevels') then   ! 3-d MPAS soil field

               field_to_store % header % vertical_coord = 'none'
               if (trim(mpas_field % name) == 'tslb') then
                  do k=1,wrf_field % dimlens(1)
                     if (k == 1) then
                        field_to_store % header % field = 'ST000010'
                     else if (k == 2) then
                        field_to_store % header % field = 'ST010040'
                     else if (k == 3) then
                        field_to_store % header % field = 'ST040100'
                     else if (k == 4) then
                        field_to_store % header % field = 'ST100200'
                     else
                        call mprintf(.true.,ERROR, 'Too many soil layers in MPAS soil field %s ...', s1=trim(mpas_field % name))
                     end if
                     allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
                     allocate(field_to_store % valid_mask)
                     call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
                     do j=1,wrf_field % dimlens(3)
                     do i=1,wrf_field % dimlens(2)
                        call bitarray_set(field_to_store % valid_mask, i, j)
                     end do
                     end do
                     field_to_store % header % vertical_level = 200100.0

                     if (wrf_field % xtype == FIELD_TYPE_REAL) then
                        field_to_store % r_arr(:,:) = wrf_field % array3r(k,:,:)
                     else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                        field_to_store % r_arr(:,:) = wrf_field % array3d(k,:,:)
                     end if

                     if (idx > 0) then
                        if (masked(idx) == MASKED_WATER) then
                           where (landmask(:,:) == 0) field_to_store % r_arr(:,:) = fill_missing(idx)
                        end if
                     end if

                     call storage_put_field(field_to_store)
                  end do
               else if (trim(mpas_field % name) == 'smois') then
                  do k=1,wrf_field % dimlens(1)
                     if (k == 1) then
                        field_to_store % header % field = 'SM000010'
                     else if (k == 2) then
                        field_to_store % header % field = 'SM010040'
                     else if (k == 3) then
                        field_to_store % header % field = 'SM040100'
                     else if (k == 4) then
                        field_to_store % header % field = 'SM100200'
                     else
                        call mprintf(.true.,ERROR, 'Too many soil layers in MPAS soil field %s ...', s1=trim(mpas_field % name))
                     end if
                     allocate(field_to_store % r_arr(wrf_field % dimlens(2), wrf_field % dimlens(3)))
                     allocate(field_to_store % valid_mask)
                     call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(2), wrf_field % dimlens(3))
                     do j=1,wrf_field % dimlens(3)
                     do i=1,wrf_field % dimlens(2)
                        call bitarray_set(field_to_store % valid_mask, i, j)
                     end do
                     end do
                     field_to_store % header % vertical_level = 200100.0

                     if (wrf_field % xtype == FIELD_TYPE_REAL) then
                        field_to_store % r_arr(:,:) = wrf_field % array3r(k,:,:)
                     else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                        field_to_store % r_arr(:,:) = wrf_field % array3d(k,:,:)
                     end if

                     if (idx > 0) then
                        if (masked(idx) == MASKED_WATER) then
                           where (landmask(:,:) == 0) field_to_store % r_arr(:,:) = fill_missing(idx)
                        end if
                     end if

                     call storage_put_field(field_to_store)
                  end do
               else
                  call mprintf(.true.,WARN, 'Skipping unknown MPAS soil field %s ...', s1=trim(mpas_field % name))
               end if

            else if (wrf_field % ndims == 2) then   ! 2-d MPAS field
               field_to_store % header % field = mpas_to_intermediate_name(mpas_field % name)

               ! If no match in the METGRID.TBL was found for this MPAS field, just use the MPAS name
               if (len_trim(field_to_store % header % field) == 0) then
                  field_to_store % header % field = trim(mpas_field % name)
               end if

               field_to_store % header % vertical_coord = 'none'
               allocate(field_to_store % r_arr(wrf_field % dimlens(1), wrf_field % dimlens(2)))
               allocate(field_to_store % valid_mask)
               call bitarray_create(field_to_store % valid_mask, wrf_field % dimlens(1), wrf_field % dimlens(2))
               do j=1,wrf_field % dimlens(2)
               do i=1,wrf_field % dimlens(1)
                  call bitarray_set(field_to_store % valid_mask, i, j)
               end do
               end do
               field_to_store % header % vertical_level = 200100.0

               if (wrf_field % xtype == FIELD_TYPE_REAL) then
                  field_to_store % r_arr(:,:) = wrf_field % array2r(:,:)
               else if (wrf_field % xtype == FIELD_TYPE_DOUBLE) then
                  field_to_store % r_arr(:,:) = wrf_field % array2d(:,:)
               end if

               if (idx > 0) then
                  if (masked(idx) == MASKED_WATER) then
                     where (landmask(:,:) == 0) field_to_store % r_arr(:,:) = fill_missing(idx)
                  end if
               end if

               call storage_put_field(field_to_store)
            end if
    
            istat = free_target_field(wrf_field)
         end if

         istat = scan_input_free_field(mpas_field)
      end do
      
      if (scan_input_close(mpas_handle) /= 0) then
         call mprintf(.true.,ERROR, 'Error closing %s with scan_input_close', s1=mpas_filename)
      end if

   end subroutine process_mpas_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: derive_mpas_fields
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine derive_mpas_fields()

      use bitarray_module
      use gridinfo_module
      use interp_module
      use interp_option_module
      use read_met_module
      use llxy_module
      use misc_definitions_module
      use module_debug
      use output_module
      use parallel_module
      use rotate_winds_module
      use storage_module
      use scan_input
      use mpas_mesh
      use constants_module

      implicit none

      integer :: k
      integer :: istatus
      integer, pointer, dimension(:) :: theta_levels => null()
      integer, pointer, dimension(:) :: pressure_levels => null()
      real, pointer, dimension(:,:) :: exner => null()
      type (fg_input) :: theta_field
      type (fg_input) :: pressure_field

      !
      ! Derive TT from theta and pressure
      !
      theta_field%header%time_dependent = .true.
      theta_field%header%constant_field = .false.
      theta_field%header%field = 'TT'
      theta_field%header%vertical_level = 0
      nullify(theta_field%r_arr)
      nullify(theta_field%valid_mask)
      nullify(theta_field%modified_mask)

      pressure_field%header%time_dependent = .true.
      pressure_field%header%constant_field = .false.
      pressure_field%header%field = 'PRESSURE'
      pressure_field%header%vertical_level = 0
      nullify(pressure_field%r_arr)
      nullify(pressure_field%valid_mask)
      nullify(pressure_field%modified_mask)

      call storage_get_levels(theta_field, theta_levels)
      call storage_get_levels(pressure_field, pressure_levels)

      if (associated(theta_levels) .and. associated(pressure_levels)) then
!         call mprintf(.true.,LOGFILE, 'Computing MPAS TT field from theta and pressure...')

         if (size(theta_levels) == size(pressure_levels)) then
            do k = 1, size(theta_levels)
               theta_field % header % vertical_level = theta_levels(k)
               call storage_get_field(theta_field, istatus)
               if (trim(theta_field % header % fg_source) /= 'MPAS') then
                  cycle
               end if
               if (istatus /= 0) then
                  call mprintf(.true.,ERROR, 'Could not get MPAS theta field at level %i', i1=theta_levels(k))
                  return
               end if

               pressure_field % header % vertical_level = pressure_levels(k)
               call storage_get_field(pressure_field, istatus)
               if (trim(pressure_field % header % fg_source) /= 'MPAS') then
                  cycle
               end if
               if (istatus /= 0) then
                  call mprintf(.true.,ERROR, 'Could not get MPAS pressure field at level %i', i1=theta_levels(k))
                  return
               end if

               ! Compute temperature
               call mprintf(.true.,LOGFILE, 'Computing TT at level %i for MPAS dataset', i1=theta_levels(k))
               if (.not. associated(exner)) then
                  allocate(exner(size(theta_field % r_arr, 1), size(theta_field % r_arr, 2)))
               end if
               exner(:,:) = (pressure_field % r_arr(:,:) / P0)**(RD/CP)
               theta_field % r_arr(:,:) = theta_field % r_arr(:,:) * exner(:,:)

               call storage_put_field(theta_field)
            end do
!         else
!            call mprintf(.true.,ERROR, 'The MPAS theta and pressure fields do not have the same number of levels!')
         end if

         deallocate(theta_levels)
         deallocate(pressure_levels)
         if (associated(exner)) then
            deallocate(exner)
         end if
      end if

   end subroutine derive_mpas_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_interp_masks
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_interp_masks(fg_prefix, is_constants, fg_date)

      use interp_option_module
      use read_met_module
      use storage_module

      implicit none

      ! Arguments
      logical, intent(in) :: is_constants
      character (len=*), intent(in) :: fg_prefix, fg_date

! BUG: Move this constant to misc_definitions_module?
integer, parameter :: BDR_WIDTH = 3

      ! Local variables
      integer :: i, istatus, idx, idxt
      type (fg_input) :: mask_field
      type (met_data) :: fg_data

      istatus = 0

      call read_met_init(fg_prefix, is_constants, fg_date, istatus)

      do while (istatus == 0)
   
         call read_next_met_field(fg_data, istatus)

         if (istatus == 0) then

            ! Find out which METGRID.TBL entry goes with this field
            idxt = num_entries + 1
            do idx=1,num_entries
               if ((index(fieldname(idx), trim(fg_data%field)) /= 0) .and. &
                   (len_trim(fieldname(idx)) == len_trim(fg_data%field))) then

                  if (index(fg_prefix,trim(from_input(idx))) /= 0 .or. &
                     (from_input(idx) == '*' .and. idxt == num_entries + 1)) then
                     idxt = idx
                  end if

               end if
            end do
            idx = idxt
            if (idx > num_entries) idx = num_entries ! The last entry is a default

            ! Do we need to rename this field?
            if (output_name(idx) /= ' ') then
               fg_data%field = output_name(idx)(1:9)

               idxt = num_entries + 1
               do idx=1,num_entries
                  if ((index(fieldname(idx), trim(fg_data%field)) /= 0) .and. &
                      (len_trim(fieldname(idx)) == len_trim(fg_data%field))) then

                     if (index(fg_prefix,trim(from_input(idx))) /= 0 .or. &
                        (from_input(idx) == '*' .and. idxt == num_entries + 1)) then
                        idxt = idx
                     end if

                  end if
               end do
               idx = idxt
               if (idx > num_entries) idx = num_entries ! The last entry is a default
            end if

            do i=1,num_entries
               if (interp_mask(i) /= ' ' .and. (trim(interp_mask(i)) == trim(fg_data%field))) then

                  mask_field%header%version = 1
                  mask_field%header%date = ' '
                  mask_field%header%date = fg_date
                  if (is_constants) then
                     mask_field%header%time_dependent = .false.
                     mask_field%header%constant_field = .true.
                  else
                     mask_field%header%time_dependent = .true.
                     mask_field%header%constant_field = .false.
                  end if
                  mask_field%header%mask_field = .true.
                  mask_field%header%forecast_hour = 0.
                  mask_field%header%fg_source = 'degribbed met data'
                  mask_field%header%field = trim(fg_data%field)//'.mask'
                  mask_field%header%units = '-'
                  mask_field%header%description = '-'
                  mask_field%header%vertical_coord = 'none'
                  mask_field%header%vertical_level = 1
                  mask_field%header%sr_x = 1
                  mask_field%header%sr_y = 1
                  mask_field%header%array_order = 'XY'
                  mask_field%header%dim1(1) = 1
                  mask_field%header%dim1(2) = fg_data%nx
                  mask_field%header%dim2(1) = 1
                  mask_field%header%dim2(2) = fg_data%ny
                  mask_field%header%is_wind_grid_rel = .true.
                  mask_field%header%array_has_missing_values = .false.
                  mask_field%map%stagger = M

                  ! Do a simple check to see whether this is a global lat/lon dataset
                  ! Note that we do not currently support regional Gaussian grids
                  if ((fg_data%iproj == PROJ_LATLON .and. abs(fg_data%nx * fg_data%deltalon - 360.) < 0.0001) &
                       .or. (fg_data%iproj == PROJ_GAUSS)) then
                     allocate(mask_field%r_arr(1-BDR_WIDTH:fg_data%nx+BDR_WIDTH,1:fg_data%ny))

                     mask_field%r_arr(1:fg_data%nx,                      1:fg_data%ny) = &
                         fg_data%slab(1:fg_data%nx,              1:fg_data%ny)

                     mask_field%r_arr(1-BDR_WIDTH:0,                     1:fg_data%ny) = &
                         fg_data%slab(fg_data%nx-BDR_WIDTH+1:fg_data%nx, 1:fg_data%ny)

                     mask_field%r_arr(fg_data%nx+1:fg_data%nx+BDR_WIDTH, 1:fg_data%ny) = &
                         fg_data%slab(1:BDR_WIDTH,       1:fg_data%ny)

                  else
                     allocate(mask_field%r_arr(1:fg_data%nx,1:fg_data%ny))
                     mask_field%r_arr = fg_data%slab
                  end if

                  nullify(mask_field%valid_mask)
                  nullify(mask_field%modified_mask)
     
                  call storage_put_field(mask_field)

                  exit
                
               end if 
            end do

            if (associated(fg_data%slab)) deallocate(fg_data%slab)

         end if

      end do

      call read_met_close()

   end subroutine get_interp_masks

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_met_field
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine interp_met_field(input_name, short_fieldnm, ifieldstagger, istagger, &
                               field, xlat, xlon, sm1, em1, sm2, em2, &
                               sd1, ed1, sd2, ed2, &
                               slab, minx, maxx, miny, maxy, bdr, do_gcell_interp, &
                               new_pts, process_bdy_width, landmask)

      use bitarray_module
      use interp_module
      use interp_option_module
      use llxy_module
      use misc_definitions_module
      use storage_module

      implicit none 

      ! Arguments
      integer, intent(in) :: ifieldstagger, istagger, &
                             sm1, em1, sm2, em2, &
                             sd1, ed1, sd2, ed2, &
                             minx, maxx, miny, maxy, bdr, &
                             process_bdy_width
      real, dimension(minx:maxx,miny:maxy), intent(in) :: slab
      real, dimension(sm1:em1,sm2:em2), intent(in) :: xlat, xlon
      real, dimension(sm1:em1,sm2:em2), intent(in), optional :: landmask
      logical, intent(in) :: do_gcell_interp
      character (len=9), intent(in) :: short_fieldnm
      character (len=MAX_FILENAME_LEN), intent(in) :: input_name
      type (fg_input), intent(inout) :: field
      type (bitarray), intent(inout) :: new_pts

      ! Local variables
      integer :: i, j, idx, idxt, orig_selected_proj, interp_mask_status, &
                 interp_land_mask_status, interp_water_mask_status, process_width
      integer, pointer, dimension(:) :: interp_array, interp_opts
      real :: rx, ry, temp
      real, pointer, dimension(:,:) :: data_count
      type (fg_input) :: mask_field, mask_water_field, mask_land_field
      !BPR BEGIN
      real, dimension(sm1:em1,sm2:em2) :: r_arr_cur_source
      !BPR END

      ! CWH Initialize local pointer variables
      nullify(interp_array)
      nullify(interp_opts)
      nullify(data_count)

      ! Find index into fieldname, interp_method, masked, and fill_missing
      !   of the current field
      idxt = num_entries + 1
      do idx=1,num_entries
         if ((index(fieldname(idx), trim(short_fieldnm)) /= 0) .and. &
             (len_trim(fieldname(idx)) == len_trim(short_fieldnm))) then 
            if (index(input_name,trim(from_input(idx))) /= 0 .or. &
               (from_input(idx) == '*' .and. idxt == num_entries + 1)) then
               idxt = idx
            end if
         end if
      end do
      idx = idxt
      if (idx > num_entries) then
         call mprintf(.true.,WARN,'Entry in METGRID.TBL not found for field %s. '// &
                      'Default options will be used for this field!', s1=short_fieldnm)
         idx = num_entries ! The last entry is a default
      end if

      if (process_bdy_width == 0) then
         process_width = max(ed1-sd1+1, ed2-sd2+1)
      else
         process_width = process_bdy_width
        ! Add two extra rows/cols to accommodate staggered fields: one extra row/col for
        !    averaging to mass points in real, and one beyond that for averaging during 
        !    wind rotation 
        if (ifieldstagger /= M) process_width = process_width + 2
      end if

      field%header%dim1(1) = sm1 
      field%header%dim1(2) = em1
      field%header%dim2(1) = sm2
      field%header%dim2(2) = em2
      field%map%stagger = ifieldstagger
      if (.not. associated(field%r_arr)) then
         allocate(field%r_arr(sm1:em1,sm2:em2))
      end if

      interp_mask_status = 1
      interp_land_mask_status = 1
      interp_water_mask_status = 1

      if (interp_mask(idx) /= ' ') then
         mask_field%header%version = 1
         mask_field%header%forecast_hour = 0.
         mask_field%header%field = trim(interp_mask(idx))//'.mask'
         mask_field%header%vertical_coord = 'none'
         mask_field%header%vertical_level = 1

         call storage_get_field(mask_field, interp_mask_status)

      end if 
      if (interp_land_mask(idx) /= ' ') then
         mask_land_field%header%version = 1
         mask_land_field%header%forecast_hour = 0.
         mask_land_field%header%field = trim(interp_land_mask(idx))//'.mask'
         mask_land_field%header%vertical_coord = 'none'
         mask_land_field%header%vertical_level = 1

         call storage_get_field(mask_land_field, interp_land_mask_status)

      end if 
      if (interp_water_mask(idx) /= ' ') then
         mask_water_field%header%version = 1
         mask_water_field%header%forecast_hour = 0.
         mask_water_field%header%field = trim(interp_water_mask(idx))//'.mask'
         mask_water_field%header%vertical_coord = 'none'
         mask_water_field%header%vertical_level = 1

         call storage_get_field(mask_water_field, interp_water_mask_status)

      end if 

      interp_array => interp_array_from_string(interp_method(idx))
      interp_opts => interp_options_from_string(interp_method(idx))

   
      !
      ! Interpolate using average_gcell interpolation method
      !
      if (do_gcell_interp) then
         !BPR BEGIN
         !If a lower priority source of the current field has already been read
         !in, the results of that input are currently in field%r_arr 
         !Pass COPY of field%r_arr into accum_continous because in accum_continuous
         !it will set the input variable to zero over points covered by the
         !current source and then determine the appropriate value to place at
         !that point.  This will overwrite data already put in field%r_arr by 
         !lower priority sources.
         !This is problematic if the current source results in missing values 
         !over parts of the area covered by the current source where a lower
         !priority source has already provided a value. In this case, if one 
         !passes in field%r_arr, it will overwrite the value provided by the
         !lower priority source with zero.
         r_arr_cur_source = field%r_arr
         !BPR END
         allocate(data_count(sm1:em1,sm2:em2))
         data_count = 0.

         if (interp_mask_status == 0) then
            call accum_continuous(slab, &
                         minx, maxx, miny, maxy, 1, 1, bdr, &
                         !BPR BEGIN
                         !Pass COPY of field%r_arr instead of field%r_arr itself
                         !field%r_arr, data_count, &
                         r_arr_cur_source, data_count, &
                         !BPR END
                         sm1, em1, sm2, em2, 1, 1, &
                         istagger, &
                         new_pts, missing_value(idx), interp_mask_val(idx), interp_mask_relational(idx), mask_field%r_arr)
         else
            call accum_continuous(slab, &
                         minx, maxx, miny, maxy, 1, 1, bdr, &
                         !BPR BEGIN
                         !Pass COPY of field%r_arr instead of field%r_arr itself
                         !field%r_arr, data_count, &
                         r_arr_cur_source, data_count, &
                         !BPR END
                         sm1, em1, sm2, em2, 1, 1, &
                         istagger, &
                         new_pts, missing_value(idx), -1.) ! The -1 is the maskval, but since we
                                                           !   we do not give an optional mask, no
                                                           !   no need to worry about -1s in data
         end if

         orig_selected_proj = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         do j=sm2,em2
            do i=sm1,em1

               if (abs(i - sd1) >= process_width .and. (abs(i - ed1) >= process_width) .and. &
                   abs(j - sd2) >= process_width .and. (abs(j - ed2) >= process_width)) then
                  field%r_arr(i,j) = fill_missing(idx)
                  call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  cycle
               end if

               if (present(landmask)) then

                  if (landmask(i,j) /= masked(idx)) then
                     if (data_count(i,j) > 0.) then
                        !BPR BEGIN
                        !accum_continuous is now passed a copy of field%r_arr (r_arr_cur_source)
                        !instead of field%r_arr itself so that it does not set
                        !field%r_arr to zero where the input source is marked as missing
                        !field%r_arr(i,j) = field%r_arr(i,j) / data_count(i,j)
                        field%r_arr(i,j) = r_arr_cur_source(i,j) / data_count(i,j)
                        !BPR END
                        call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                     else

                        if (interp_mask_status == 0) then
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                                   mask_relational=interp_mask_relational(idx), &
                                                   mask_val=interp_mask_val(idx), mask_field=mask_field%r_arr)
                        else
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx))
                        end if
   
                        if (temp /= missing_value(idx)) then
                           field%r_arr(i,j) = temp
                           call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                        end if

                     end if
                  else
                     field%r_arr(i,j) = fill_missing(idx)
                     call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  end if

                  if (.not. bitarray_test(new_pts, i-sm1+1, j-sm2+1) .and. &
                      .not. bitarray_test(field%valid_mask, i-sm1+1, j-sm2+1)) then
                     field%r_arr(i,j) = fill_missing(idx)

                     ! Assume that if missing fill value is other than default, then user has asked
                     !    to fill in any missing values, and we can consider this point to have 
                     !    received a valid value
                     if (fill_missing(idx) /= NAN) call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  end if

               else

                  if (data_count(i,j) > 0.) then
                     !BPR BEGIN
                     !accum_continuous is now passed a copy of field%r_arr (r_arr_cur_source)
                     !instead of field%r_arr itself so that it does not set
                     !field%r_arr to zero where the input source is marked as missing
                     !field%r_arr(i,j) = field%r_arr(i,j) / data_count(i,j)
                     field%r_arr(i,j) = r_arr_cur_source(i,j) / data_count(i,j)
                     !BPR END
                     call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  else

                     if (interp_mask_status == 0) then
                        temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                                mask_relational=interp_mask_relational(idx), &
                                                mask_val=interp_mask_val(idx), mask_field=mask_field%r_arr)
                     else
                        temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                minx, maxx, miny, maxy, bdr, missing_value(idx))
                     end if

                     if (temp /= missing_value(idx)) then
                        field%r_arr(i,j) = temp
                        call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                     end if

                  end if

                  if (.not. bitarray_test(new_pts, i-sm1+1, j-sm2+1) .and. &
                      .not. bitarray_test(field%valid_mask, i-sm1+1, j-sm2+1)) then
                     field%r_arr(i,j) = fill_missing(idx)

                     ! Assume that if missing fill value is other than default, then user has asked
                     !    to fill in any missing values, and we can consider this point to have 
                     !    received a valid value
                     if (fill_missing(idx) /= NAN) call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  end if

               end if

            end do
         end do
         call select_domain(orig_selected_proj) 
         deallocate(data_count)

      !
      ! No average_gcell interpolation method
      !
      else

         orig_selected_proj = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         do j=sm2,em2
            do i=sm1,em1

               if (abs(i - sd1) >= process_width .and. (abs(i - ed1) >= process_width) .and. &
                   abs(j - sd2) >= process_width .and. (abs(j - ed2) >= process_width)) then
                  field%r_arr(i,j) = fill_missing(idx)
                  call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  cycle
               end if

               if (present(landmask)) then

                  if (masked(idx) == MASKED_BOTH) then

                     if (landmask(i,j) == 0) then  ! WATER POINT

                        if (interp_land_mask_status == 0) then
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                                   mask_relational=interp_land_mask_relational(idx), &
                                                   mask_val=interp_land_mask_val(idx), mask_field=mask_land_field%r_arr)
                        else
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx))
                        end if
   
                     else if (landmask(i,j) == 1) then  ! LAND POINT

                        if (interp_water_mask_status == 0) then
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                                   mask_relational=interp_water_mask_relational(idx), &
                                                   mask_val=interp_water_mask_val(idx), mask_field=mask_water_field%r_arr)
                        else
                           temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                   minx, maxx, miny, maxy, bdr, missing_value(idx))
                        end if
   
                     end if

                  else if (landmask(i,j) /= masked(idx)) then

                     if (interp_mask_status == 0) then
                        temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                                mask_relational=interp_mask_relational(idx), &
                                                mask_val=interp_mask_val(idx), mask_field=mask_field%r_arr)
                     else
                        temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                                minx, maxx, miny, maxy, bdr, missing_value(idx))
                     end if

                  else
                     temp = missing_value(idx)
                  end if

               ! No landmask for this field
               else

                  if (interp_mask_status == 0) then
                     temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                             minx, maxx, miny, maxy, bdr, missing_value(idx), &
                                             mask_relational=interp_mask_relational(idx), &
                                             mask_val=interp_mask_val(idx), mask_field=mask_field%r_arr)
                  else
                     temp = interp_to_latlon(xlat(i,j), xlon(i,j), istagger, interp_array, interp_opts, slab, &
                                             minx, maxx, miny, maxy, bdr, missing_value(idx))
                  end if

               end if

               if (temp /= missing_value(idx)) then
                  field%r_arr(i,j) = temp
                  call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
               else if (present(landmask)) then
                  if (landmask(i,j) == masked(idx)) then
                     field%r_arr(i,j) = fill_missing(idx)
                     call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
                  end if
               end if

               if (.not. bitarray_test(new_pts, i-sm1+1, j-sm2+1) .and. &
                   .not. bitarray_test(field%valid_mask, i-sm1+1, j-sm2+1)) then
                  field%r_arr(i,j) = fill_missing(idx)

                  ! Assume that if missing fill value is other than default, then user has asked
                  !    to fill in any missing values, and we can consider this point to have 
                  !    received a valid value
                  if (fill_missing(idx) /= NAN) call bitarray_set(new_pts, i-sm1+1, j-sm2+1)
               end if

            end do
         end do
         call select_domain(orig_selected_proj) 
      end if

      deallocate(interp_array)
      deallocate(interp_opts)

   end subroutine interp_met_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: interp_to_latlon
   ! 
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function interp_to_latlon(rlat, rlon, istagger, interp_method_list, interp_opt_list, slab, &
                             minx, maxx, miny, maxy, bdr, source_missing_value, &
                             mask_field, mask_relational, mask_val)

      use interp_module
      use llxy_module

      implicit none

      ! Arguments
      integer, intent(in) :: minx, maxx, miny, maxy, bdr, istagger
      integer, dimension(:), intent(in) :: interp_method_list
      integer, dimension(:), intent(in) :: interp_opt_list
      real, intent(in) :: rlat, rlon, source_missing_value
      real, dimension(minx:maxx,miny:maxy), intent(in) :: slab
      real, intent(in), optional :: mask_val
      real, dimension(minx:maxx,miny:maxy), intent(in), optional :: mask_field
      character(len=1), intent(in), optional :: mask_relational

      ! Return value
      real :: interp_to_latlon
     
      ! Local variables
      real :: rx, ry

      interp_to_latlon = source_missing_value
   
      call lltoxy(rlat, rlon, rx, ry, istagger) 
      if (rx >= minx+bdr-0.5 .and. rx <= maxx-bdr+0.5) then
         if (present(mask_field) .and. present(mask_val) .and. present (mask_relational)) then
            interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, 1, 1, source_missing_value, &
                                               interp_method_list, interp_opt_list, 1, mask_relational, mask_val, mask_field)
         else if (present(mask_field) .and. present(mask_val)) then
            interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, 1, 1, source_missing_value, &
                                               interp_method_list, interp_opt_list, 1, maskval=mask_val, mask_array=mask_field)
         else
            interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, 1, 1, source_missing_value, &
                                               interp_method_list, interp_opt_list, 1)
         end if
      else
         interp_to_latlon = source_missing_value 
      end if

      if (interp_to_latlon == source_missing_value) then

         ! Try a lon in the range 0. to 360.; all lons in the xlon 
         !    array should be in the range -180. to 180.
         if (rlon < 0.) then
            call lltoxy(rlat, rlon+360., rx, ry, istagger) 
            if (rx >= minx+bdr-0.5 .and. rx <= maxx-bdr+0.5) then
               if (present(mask_field) .and. present(mask_val) .and. present(mask_relational)) then
                  interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, &
                                                     1, 1, source_missing_value, &
                                                     interp_method_list, interp_opt_list, 1, &
                                                     mask_relational, mask_val, mask_field)
               else if (present(mask_field) .and. present(mask_val)) then
                  interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, &
                                                     1, 1, source_missing_value, &
                                                     interp_method_list, interp_opt_list, 1, &
                                                     maskval=mask_val, mask_array=mask_field)
               else
                  interp_to_latlon = interp_sequence(rx, ry, 1, slab, minx, maxx, miny, maxy, &
                                                     1, 1, source_missing_value, &
                                                     interp_method_list, interp_opt_list, 1)
               end if
            else
               interp_to_latlon = source_missing_value 
            end if

         end if

      end if

      return

   end function interp_to_latlon

  
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_bottom_top_dim
   ! 
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_bottom_top_dim(bottom_top_dim)

      use interp_option_module
      use list_module
      use storage_module

      implicit none

      ! Arguments
      integer, intent(out) :: bottom_top_dim

      ! Local variables
      integer :: i, j
      integer, pointer, dimension(:) :: field_levels
      character (len=32) :: z_dim
      type (fg_input), pointer, dimension(:) :: headers
      type (list) :: temp_levels
   
      !CWH Initialize local pointer variables
      nullify(field_levels)
      nullify(headers)

      ! Initialize a list to store levels that are found for 3-d fields 
      call list_init(temp_levels)
   
      ! Get a list of all time-dependent fields (given by their headers) from
      !   the storage module
      call storage_get_td_headers(headers)
   
      !
      ! Given headers of all fields, we first build a list of all possible levels
      !    for 3-d met fields (excluding sea-level, though).
      !
      do i=1,size(headers)
         call get_z_dim_name(headers(i)%header%field, z_dim)
   
         ! We only want to consider 3-d met fields
         if (z_dim(1:18) == 'num_metgrid_levels') then

            ! Find out what levels the current field has
            call storage_get_levels(headers(i), field_levels)
            do j=1,size(field_levels)
   
               ! If this level has not yet been encountered, add it to our list
               if (.not. list_search(temp_levels, ikey=field_levels(j), ivalue=field_levels(j))) then
                  if (field_levels(j) /= 201300) then
                     call list_insert(temp_levels, ikey=field_levels(j), ivalue=field_levels(j))
                  end if
               end if
   
            end do
   
            deallocate(field_levels)

         end if
   
      end do

      bottom_top_dim = list_length(temp_levels)

      call list_destroy(temp_levels)
      deallocate(headers)

   end subroutine get_bottom_top_dim

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: fill_missing_levels
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine fill_missing_levels(output_flags)
   
      use interp_option_module
      use list_module
      use module_debug
      use module_mergesort
      use storage_module
   
      implicit none

      ! Arguments
      character (len=128), dimension(:), intent(inout) :: output_flags
   
      ! Local variables
      integer :: i, ii, j, ix, jx, k, lower, upper, temp, istatus
      integer, pointer, dimension(:) :: union_levels, field_levels
      real, pointer, dimension(:) :: r_union_levels
      character (len=128) :: clevel
      type (fg_input) :: lower_field, upper_field, new_field, search_field
      type (fg_input), pointer, dimension(:) :: headers, all_headers
      type (list) :: temp_levels
      type (list_item), pointer, dimension(:) :: keys
   
      ! CWH Initialize local pointer variables
      nullify(union_levels)
      nullify(field_levels)
      nullify(r_union_levels)
      nullify(headers)
      nullify(all_headers)
      nullify(keys)

      ! Initialize a list to store levels that are found for 3-d fields 
      call list_init(temp_levels)
   
      ! Get a list of all fields (given by their headers) from the storage module
      call storage_get_td_headers(headers)
      call storage_get_all_headers(all_headers)
   
      !
      ! Given headers of all fields, we first build a list of all possible levels
      !    for 3-d met fields (excluding sea-level, though).
      !
      do i=1,size(headers)
   
         ! Find out what levels the current field has
         call storage_get_levels(headers(i), field_levels)
         do j=1,size(field_levels)
   
            ! If this level has not yet been encountered, add it to our list
            if (.not. list_search(temp_levels, ikey=field_levels(j), ivalue=field_levels(j))) then
               if (field_levels(j) /= 201300) then
                  call list_insert(temp_levels, ikey=field_levels(j), ivalue=field_levels(j))
               end if
            end if
   
         end do
   
         deallocate(field_levels)
   
      end do
   
      if (list_length(temp_levels) > 0) then
   
         ! 
         ! With all possible levels stored in a list, get an array of levels, sorted
         !    in decreasing order
         !
         i = 0
         allocate(union_levels(list_length(temp_levels)))
         do while (list_length(temp_levels) > 0)
            i = i + 1
            call list_get_first_item(temp_levels, ikey=union_levels(i), ivalue=temp)     
         end do
         call mergesort(union_levels, 1, size(union_levels))
   
         allocate(r_union_levels(size(union_levels)))
         do i=1,size(union_levels)
            r_union_levels(i) = real(union_levels(i))
         end do

         !
         ! With a sorted, complete list of levels, we need 
         !    to go back and fill in missing levels for each 3-d field 
         !
         do i=1,size(headers)

            !
            ! Find entry in METGRID.TBL for this field, if one exists; if it does, then the
            !    entry may tell us how to get values for the current field at the missing level
            !
            do ii=1,num_entries
               if (fieldname(ii) == headers(i)%header%field) exit 
            end do
            if (ii <= num_entries) then
               call dup(headers(i),new_field)
               nullify(new_field%valid_mask)
               nullify(new_field%modified_mask)
               call fill_field(new_field, ii, output_flags, r_union_levels)
            end if

         end do

         deallocate(union_levels)
         deallocate(r_union_levels)
         deallocate(headers)

         call storage_get_td_headers(headers)

         !
         ! Now we may need to vertically interpolate to missing values in 3-d fields
         !
         do i=1,size(headers)
   
            call storage_get_levels(headers(i), field_levels)
   
            ! If this isn't a 3-d array, nothing to do
            if (size(field_levels) > 1) then

               do k=1,size(field_levels)
                  call dup(headers(i),search_field)
                  search_field%header%vertical_level = field_levels(k)
                  call storage_get_field(search_field,istatus) 
                  if (istatus == 0) then
                     JLOOP: do jx=search_field%header%dim2(1),search_field%header%dim2(2)
                        ILOOP: do ix=search_field%header%dim1(1),search_field%header%dim1(2)
                           if (.not. bitarray_test(search_field%valid_mask, &
                                                   ix-search_field%header%dim1(1)+1, &
                                                   jx-search_field%header%dim2(1)+1)) then

                              call dup(search_field, lower_field)
                              do lower=k-1,1,-1
                                 lower_field%header%vertical_level = field_levels(lower)
                                 call storage_get_field(lower_field,istatus) 
                                 if (bitarray_test(lower_field%valid_mask, &
                                                   ix-search_field%header%dim1(1)+1, &
                                                   jx-search_field%header%dim2(1)+1)) &
                                     exit 
                                
                              end do                        

                              call dup(search_field, upper_field)
                              do upper=k+1,size(field_levels)
                                 upper_field%header%vertical_level = field_levels(upper)
                                 call storage_get_field(upper_field,istatus) 
                                 if (bitarray_test(upper_field%valid_mask, &
                                                   ix-search_field%header%dim1(1)+1, &
                                                   jx-search_field%header%dim2(1)+1)) &
                                     exit 
                                
                              end do                        
                              if (upper <= size(field_levels) .and. lower >= 1) then
                                 search_field%r_arr(ix,jx) = real(abs(field_levels(upper)-field_levels(k))) &
                                                           / real(abs(field_levels(upper)-field_levels(lower))) &
                                                           * lower_field%r_arr(ix,jx) &
                                                           + real(abs(field_levels(k)-field_levels(lower))) &
                                                           / real(abs(field_levels(upper)-field_levels(lower))) &
                                                           * upper_field%r_arr(ix,jx)
                                 call bitarray_set(search_field%valid_mask, &
                                                   ix-search_field%header%dim1(1)+1, &
                                                   jx-search_field%header%dim2(1)+1)
                              end if
                           end if
                        end do ILOOP
                     end do JLOOP
                  else
                     call mprintf(.true.,ERROR, &
                                  'This is bad, could not get %s at level %i.', &
                                  s1=trim(search_field%header%field), i1=field_levels(k))
                  end if
               end do

            end if

            deallocate(field_levels)

         end do

      end if
   
      call list_destroy(temp_levels)
      deallocate(all_headers)
      deallocate(headers)
   
   end subroutine fill_missing_levels


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: create_derived_fields
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine create_derived_fields(arg_gridtype, hdate, xfcst, &
                                 we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                                 we_mem_stag_s, we_mem_stag_e, sn_mem_stag_s, sn_mem_stag_e, &
                                 created_this_field, output_flags)

      use interp_option_module
      use list_module
      use module_mergesort
      use storage_module

      implicit none

      ! Arguments
      integer, intent(in) :: we_mem_s, we_mem_e, sn_mem_s, sn_mem_e, &
                             we_mem_stag_s, we_mem_stag_e, sn_mem_stag_s, sn_mem_stag_e
      real, intent(in) :: xfcst 
      logical, dimension(:), intent(inout) :: created_this_field 
      character (len=1), intent(in) :: arg_gridtype 
      character (len=24), intent(in) :: hdate 
      character (len=128), dimension(:), intent(inout) :: output_flags

      ! Local variables
      integer :: idx, i, j, istatus
      type (fg_input) :: field

      ! Initialize fg_input structure to store the field
      field%header%version = 1
      field%header%date = hdate//'        '
      field%header%time_dependent = .true.
      field%header%mask_field = .false.
      field%header%constant_field = .false.
      field%header%forecast_hour = xfcst 
      field%header%fg_source = 'Derived from FG'
      field%header%field = ' '
      field%header%units = ' '
      field%header%description = ' '
      field%header%vertical_level = 0
      field%header%sr_x = 1
      field%header%sr_y = 1
      field%header%array_order = 'XY ' 
      field%header%is_wind_grid_rel = .true.
      field%header%array_has_missing_values = .false.
      nullify(field%r_arr)
      nullify(field%valid_mask)
      nullify(field%modified_mask)

      !
      ! Check each entry in METGRID.TBL to see whether it is a derive field
      !
      do idx=1,num_entries
         if (is_derived_field(idx)) then

            created_this_field(idx) = .true.

            call mprintf(.true.,INFORM,'Going to create the field %s',s1=fieldname(idx))

            ! Intialize more fields in storage structure
            field%header%field = fieldname(idx)
            call get_z_dim_name(fieldname(idx),field%header%vertical_coord)
            field%map%stagger = output_stagger(idx)
            if (arg_gridtype == 'E') then
               field%header%dim1(1) = we_mem_s
               field%header%dim1(2) = we_mem_e
               field%header%dim2(1) = sn_mem_s
               field%header%dim2(2) = sn_mem_e
            else if (arg_gridtype == 'C') then
               if (output_stagger(idx) == M) then
                  field%header%dim1(1) = we_mem_s
                  field%header%dim1(2) = we_mem_e
                  field%header%dim2(1) = sn_mem_s
                  field%header%dim2(2) = sn_mem_e
               else if (output_stagger(idx) == U) then
                  field%header%dim1(1) = we_mem_stag_s
                  field%header%dim1(2) = we_mem_stag_e
                  field%header%dim2(1) = sn_mem_s
                  field%header%dim2(2) = sn_mem_e
               else if (output_stagger(idx) == V) then
                  field%header%dim1(1) = we_mem_s
                  field%header%dim1(2) = we_mem_e
                  field%header%dim2(1) = sn_mem_stag_s
                  field%header%dim2(2) = sn_mem_stag_e
               end if
            end if

            call fill_field(field, idx, output_flags)

         end if
      end do


   end subroutine create_derived_fields


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: fill_field
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine fill_field(field, idx, output_flags, all_level_list)

      use interp_option_module
      use list_module
      use module_mergesort
      use storage_module

      implicit none

      ! Arguments
      integer, intent(in) :: idx
      type (fg_input), intent(inout) :: field
      character (len=128), dimension(:), intent(inout) :: output_flags
      real, dimension(:), intent(in), optional :: all_level_list

      ! Local variables
      integer :: i, j, istatus, isrclevel
      integer, pointer, dimension(:) :: all_list
      real :: rfillconst, rlevel, rsrclevel
      type (fg_input) :: query_field
      type (list_item), pointer, dimension(:) :: keys
      character (len=128) :: asrcname
      logical :: filled_all_lev

      !CWH Initialize local pointer variables
      nullify(all_list)
      nullify(keys)

      filled_all_lev = .false.

      !
      ! Get a list of all levels to be filled for this field
      !
      keys => list_get_keys(fill_lev_list(idx))

      do i=1,list_length(fill_lev_list(idx))

         !
         ! First handle a specification for levels "all"
         !
         if (trim(keys(i)%ckey) == 'all') then
          
            ! We only want to fill all levels if we haven't already filled "all" of them
            if (.not. filled_all_lev) then

               filled_all_lev = .true.

               query_field%header%time_dependent = .true.
               query_field%header%field = ' '
               nullify(query_field%r_arr)
               nullify(query_field%valid_mask)
               nullify(query_field%modified_mask)

               ! See if we are filling this level with a constant
               call get_constant_fill_lev(keys(i)%cvalue, rfillconst, istatus)
               if (istatus == 0) then
                  if (present(all_level_list)) then
                     do j=1,size(all_level_list)
                        call create_level(field, real(all_level_list(j)), idx, output_flags, rfillconst=rfillconst)
                     end do
                  else
                     query_field%header%field = level_template(idx)
                     nullify(all_list)
                     call storage_get_levels(query_field, all_list)
                     if (associated(all_list)) then
                        do j=1,size(all_list)
                           call create_level(field, real(all_list(j)), idx, output_flags, rfillconst=rfillconst)
                        end do
                        deallocate(all_list)
                     end if
                  end if
         
               ! Else see if we are filling this level with a constant equal
               !   to the value of the level
               else if (trim(keys(i)%cvalue) == 'vertical_index') then
                  if (present(all_level_list)) then
                     do j=1,size(all_level_list)
                        call create_level(field, real(all_level_list(j)), idx, output_flags, &
                                          rfillconst=real(all_level_list(j)))
                     end do
                  else
                     query_field%header%field = level_template(idx)
                     nullify(all_list)
                     call storage_get_levels(query_field, all_list)
                     if (associated(all_list)) then
                        do j=1,size(all_list)
                           call create_level(field, real(all_list(j)), idx, output_flags, rfillconst=real(all_list(j)))
                        end do
                        deallocate(all_list)
                     end if
                  end if
        
               ! Else, we assume that it is a field from which we are copying levels
               else
                  if (present(all_level_list)) then
                     do j=1,size(all_level_list)
                        call create_level(field, real(all_level_list(j)), idx, output_flags, &
                                          asrcname=keys(i)%cvalue, rsrclevel=real(all_level_list(j)))
                     end do
                  else
                     query_field%header%field = keys(i)%cvalue  ! Use same levels as source field, not level_template
                     nullify(all_list)
                     call storage_get_levels(query_field, all_list)
                     if (associated(all_list)) then
                        do j=1,size(all_list)
                           call create_level(field, real(all_list(j)), idx, output_flags, &
                                             asrcname=keys(i)%cvalue, rsrclevel=real(all_list(j)))
                        end do
                        deallocate(all_list)
   
                     else
   
                        ! If the field doesn't have any levels (or does not exist) then we have not
                        !   really filled all levels at this point.
                        filled_all_lev = .false.
                     end if
                  end if
      
               end if
            end if
                  
         !
         ! Handle individually specified levels
         !
         else 

            read(keys(i)%ckey,*) rlevel

            ! See if we are filling this level with a constant
            call get_constant_fill_lev(keys(i)%cvalue, rfillconst, istatus)
            if (istatus == 0) then
               call create_level(field, rlevel, idx, output_flags, rfillconst=rfillconst)

            ! Otherwise, we are filling from another level
            else
               call get_fill_src_level(keys(i)%cvalue, asrcname, isrclevel)
               rsrclevel = real(isrclevel)
               call create_level(field, rlevel, idx, output_flags, &
                                 asrcname=asrcname, rsrclevel=rsrclevel)
               
            end if
         end if
      end do

      if (associated(keys)) deallocate(keys)

   end subroutine fill_field


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: create_level
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine create_level(field_template, rlevel, idx, output_flags, &
                           rfillconst, asrcname, rsrclevel)

      use storage_module
      use interp_option_module

      implicit none

      ! Arguments
      type (fg_input), intent(inout) :: field_template
      real, intent(in) :: rlevel
      integer, intent(in) :: idx
      character (len=128), dimension(:), intent(inout) :: output_flags
      real, intent(in), optional :: rfillconst, rsrclevel
      character (len=128), intent(in), optional :: asrcname
       
      ! Local variables
      integer :: i, j, istatus
      integer :: sm1,em1,sm2,em2
      type (fg_input) :: query_field

      !
      ! Check to make sure optional arguments are sane
      !
      if (present(rfillconst) .and. (present(asrcname) .or. present(rsrclevel))) then
         call mprintf(.true.,ERROR,'A call to create_level() cannot be given specifications '// &
                      'for both a constant fill value and a source level.')

      else if ((present(asrcname) .and. .not. present(rsrclevel)) .or. &
               (.not. present(asrcname) .and. present(rsrclevel))) then
         call mprintf(.true.,ERROR,'Neither or both of optional arguments asrcname and '// &
                      'rsrclevel must be specified to subroutine create_level().')

      else if (.not. present(rfillconst) .and. &
               .not. present(asrcname)   .and. &
               .not. present(rsrclevel)) then
         call mprintf(.true.,ERROR,'A call to create_level() must be given either a specification '// &
                      'for a constant fill value or a source level.')
      end if

      query_field%header%time_dependent = .true.
      query_field%header%field = field_template%header%field
      query_field%header%vertical_level = rlevel
      nullify(query_field%r_arr)
      nullify(query_field%valid_mask)
      nullify(query_field%modified_mask)

      call storage_query_field(query_field, istatus)
      if (istatus == 0) then
         call mprintf(.true.,INFORM,'%s at level %f already exists; leaving it alone.', &
                      s1=field_template%header%field, f1=rlevel)
         return 
      end if

      sm1 = field_template%header%dim1(1)
      em1 = field_template%header%dim1(2)
      sm2 = field_template%header%dim2(1)
      em2 = field_template%header%dim2(2)

      !
      ! Handle constant fill value case
      !
      if (present(rfillconst)) then

         field_template%header%vertical_level = rlevel
         allocate(field_template%r_arr(sm1:em1,sm2:em2))
         allocate(field_template%valid_mask)
         allocate(field_template%modified_mask)
         call bitarray_create(field_template%valid_mask, em1-sm1+1, em2-sm2+1)
         call bitarray_create(field_template%modified_mask, em1-sm1+1, em2-sm2+1)
 
         field_template%r_arr = rfillconst

         do j=sm2,em2
            do i=sm1,em1
               call bitarray_set(field_template%valid_mask, i-sm1+1, j-sm2+1)
            end do
         end do

         call storage_put_field(field_template)

         if (output_this_field(idx) .and. flag_in_output(idx) /= ' ') then
            output_flags(idx) = flag_in_output(idx)
         end if

      !
      ! Handle source field and source level case
      !
      else if (present(asrcname) .and. present(rsrclevel)) then

         query_field%header%field = ' '
         query_field%header%field = asrcname
         query_field%header%vertical_level = rsrclevel

         ! Check to see whether the requested source field exists at the requested level
         call storage_query_field(query_field, istatus)

         if (istatus == 0) then

            ! Read in requested field at requested level
            call storage_get_field(query_field, istatus)
            if ((query_field%header%dim1(1) /= field_template%header%dim1(1)) .or. &
                (query_field%header%dim1(2) /= field_template%header%dim1(2)) .or. &
                (query_field%header%dim2(1) /= field_template%header%dim2(1)) .or. &
                (query_field%header%dim2(2) /= field_template%header%dim2(2))) then
               call mprintf(.true.,ERROR,'Dimensions for %s do not match those of %s. This is '// &
                            'probably because the staggerings of the fields do not match.', &
                            s1=query_field%header%field, s2=field_template%header%field)
            end if

            field_template%header%vertical_level = rlevel
            allocate(field_template%r_arr(sm1:em1,sm2:em2))
            allocate(field_template%valid_mask)
            allocate(field_template%modified_mask)
            call bitarray_create(field_template%valid_mask, em1-sm1+1, em2-sm2+1)
            call bitarray_create(field_template%modified_mask, em1-sm1+1, em2-sm2+1)
 
            field_template%r_arr = query_field%r_arr

            ! We should retain information about which points in the field are valid
            do j=sm2,em2
               do i=sm1,em1
                  if (bitarray_test(query_field%valid_mask, i-sm1+1, j-sm2+1)) then
                     call bitarray_set(field_template%valid_mask, i-sm1+1, j-sm2+1)
                  end if
               end do
            end do

            call storage_put_field(field_template)

            if (output_this_field(idx) .and. flag_in_output(idx) /= ' ') then
               output_flags(idx) = flag_in_output(idx)
            end if

         else
            call mprintf(.true.,INFORM,'Couldn''t find %s at level %f to fill level %f of %s.', &
                         s1=asrcname,f1=rsrclevel,f2=rlevel,s2=field_template%header%field)
         end if

      end if

   end subroutine create_level
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: accum_continuous
   !
   ! Purpose: Sum up all of the source data points whose nearest neighbor in the
   !   model grid is the specified model grid point.
   !
   ! NOTE: When processing the source tile, those source points that are 
   !   closest to a different model grid point will be added to the totals for 
   !   such grid points; thus, an entire source tile will be processed at a time.
   !   This routine really processes for all model grid points that are 
   !   within a source tile, and not just for a single grid point.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine accum_continuous(src_array, &
                               src_min_x, src_max_x, src_min_y, src_max_y, src_min_z, src_max_z, bdr_width, &
                               dst_array, n, &
                               start_i, end_i, start_j, end_j, start_k, end_k, &
                               istagger, &
                               new_pts, msgval, maskval, mask_relational, mask_array, sr_x, sr_y)
   
      use bitarray_module
      use misc_definitions_module
   
      implicit none
   
      ! Arguments
      integer, intent(in) :: start_i, end_i, start_j, end_j, start_k, end_k, istagger, &
                             src_min_x, src_max_x, src_min_y, src_max_y, src_min_z, src_max_z, bdr_width
      real, intent(in) :: maskval, msgval
      real, dimension(src_min_x:src_max_x, src_min_y:src_max_y, src_min_z:src_max_z), intent(in) :: src_array
      real, dimension(start_i:end_i, start_j:end_j, start_k:end_k), intent(inout) :: dst_array, n
      real, dimension(src_min_x:src_max_x, src_min_y:src_max_y), intent(in), optional :: mask_array
      integer, intent(in), optional :: sr_x, sr_y
      type (bitarray), intent(inout) :: new_pts
      character(len=1), intent(in), optional :: mask_relational
   
      ! Local variables
      integer :: i, j
      integer, pointer, dimension(:,:,:) :: where_maps_to
      real :: rsr_x, rsr_y

      rsr_x = 1.0
      rsr_y = 1.0
      if (present(sr_x)) rsr_x = real(sr_x)
      if (present(sr_y)) rsr_y = real(sr_y)
   
      allocate(where_maps_to(src_min_x:src_max_x,src_min_y:src_max_y,2))
      do i=src_min_x,src_max_x
         do j=src_min_y,src_max_y
            where_maps_to(i,j,1) = NOT_PROCESSED 
         end do
      end do
   
      call process_continuous_block(src_array, where_maps_to, &
                               src_min_x, src_min_y, src_min_z, src_max_x, src_max_y, src_max_z, &
                               src_min_x+bdr_width, src_min_y, src_min_z, &
                               src_max_x-bdr_width, src_max_y, src_max_z, &
                               dst_array, n, start_i, end_i, start_j, end_j, start_k, end_k, &
                               istagger, &
                               new_pts, rsr_x, rsr_y, msgval, maskval, mask_relational, mask_array)
   
      deallocate(where_maps_to)
   
   end subroutine accum_continuous
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: process_continuous_block 
   !
   ! Purpose: To recursively process a subarray of continuous data, adding the 
   !   points in a block to the sum for their nearest grid point. The nearest 
   !   neighbor may be estimated in some cases; for example, if the four corners 
   !   of a subarray all have the same nearest grid point, all elements in the 
   !   subarray are added to that grid point.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   recursive subroutine process_continuous_block(tile_array, where_maps_to, &
                                   src_min_x, src_min_y, src_min_z, src_max_x, src_max_y, src_max_z, &
                                   min_i, min_j, min_k, max_i, max_j, max_k, &
                                   dst_array, n, &
                                   start_x, end_x, start_y, end_y, start_z, end_z, &
                                   istagger, &
                                   new_pts, sr_x, sr_y, msgval, maskval, mask_relational, mask_array)
   
      use bitarray_module
      use llxy_module
      use misc_definitions_module
   
      implicit none
   
      ! Arguments
      integer, intent(in) :: min_i, min_j, min_k, max_i, max_j, max_k, &
                             src_min_x, src_min_y, src_min_z, src_max_x, src_max_y, src_max_z, &
                             start_x, end_x, start_y, end_y, start_z, end_z, istagger
      integer, dimension(src_min_x:src_max_x,src_min_y:src_max_y,2), intent(inout) :: where_maps_to
      real, intent(in) :: sr_x, sr_y, maskval, msgval
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y,src_min_z:src_max_z), intent(in) :: tile_array
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y), intent(in), optional :: mask_array
      real, dimension(start_x:end_x,start_y:end_y,start_z:end_z), intent(inout) :: dst_array, n
      type (bitarray), intent(inout) :: new_pts
      character(len=1), intent(in), optional :: mask_relational
   
      ! Local variables
      integer :: orig_selected_domain, x_dest, y_dest, i, j, k, center_i, center_j
      real :: lat_corner, lon_corner, rx, ry
   
      ! Compute the model grid point that the corners of the rectangle to be 
      !   processed map to
      ! Lower-left corner
      if (where_maps_to(min_i,min_j,1) == NOT_PROCESSED) then
         orig_selected_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(min_j), lat_corner, lon_corner, istagger)
         call select_domain(1)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx - 1.0)*sr_x + 1.0
         ry = (ry - 1.0)*sr_y + 1.0
         call select_domain(orig_selected_domain)
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. &
             real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,min_j,1) = nint(rx)
            where_maps_to(min_i,min_j,2) = nint(ry)
         else
            where_maps_to(min_i,min_j,1) = OUTSIDE_DOMAIN
         end if
      end if
   
      ! Upper-left corner
      if (where_maps_to(min_i,max_j,1) == NOT_PROCESSED) then
         orig_selected_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(max_j), lat_corner, lon_corner, istagger)
         call select_domain(1)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx - 1.0)*sr_x + 1.0
         ry = (ry - 1.0)*sr_y + 1.0
         call select_domain(orig_selected_domain)
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. &
             real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,max_j,1) = nint(rx)
            where_maps_to(min_i,max_j,2) = nint(ry)
         else
            where_maps_to(min_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
   
      ! Upper-right corner
      if (where_maps_to(max_i,max_j,1) == NOT_PROCESSED) then
         orig_selected_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(max_j), lat_corner, lon_corner, istagger)
         call select_domain(1)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx - 1.0)*sr_x + 1.0
         ry = (ry - 1.0)*sr_y + 1.0
         call select_domain(orig_selected_domain)
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. &
             real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(max_i,max_j,1) = nint(rx)
            where_maps_to(max_i,max_j,2) = nint(ry)
         else
            where_maps_to(max_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
   
      ! Lower-right corner
      if (where_maps_to(max_i,min_j,1) == NOT_PROCESSED) then
         orig_selected_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(min_j), lat_corner, lon_corner, istagger)
         call select_domain(1)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx - 1.0)*sr_x + 1.0
         ry = (ry - 1.0)*sr_y + 1.0
         call select_domain(orig_selected_domain)
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. &
             real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(max_i,min_j,1) = nint(rx)
            where_maps_to(max_i,min_j,2) = nint(ry)
         else
            where_maps_to(max_i,min_j,1) = OUTSIDE_DOMAIN
         end if
      end if
   
      ! If all four corners map to same model grid point, accumulate the 
      !   entire rectangle
      if (where_maps_to(min_i,min_j,1) == where_maps_to(min_i,max_j,1) .and. &
          where_maps_to(min_i,min_j,1) == where_maps_to(max_i,max_j,1) .and. &
          where_maps_to(min_i,min_j,1) == where_maps_to(max_i,min_j,1) .and. &
          where_maps_to(min_i,min_j,2) == where_maps_to(min_i,max_j,2) .and. &
          where_maps_to(min_i,min_j,2) == where_maps_to(max_i,max_j,2) .and. &
          where_maps_to(min_i,min_j,2) == where_maps_to(max_i,min_j,2) .and. &
          where_maps_to(min_i,min_j,1) /= OUTSIDE_DOMAIN) then 
         x_dest = where_maps_to(min_i,min_j,1)
         y_dest = where_maps_to(min_i,min_j,2)
         
         ! If this grid point was already given a value from higher-priority source data, 
         !   there is nothing to do.
!         if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
   
            ! If this grid point has never been given a value by this level of source data,
            !   initialize the point
            if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
               do k=min_k,max_k
                  dst_array(x_dest,y_dest,k) = 0.
               end do
            end if
   
            ! Sum all the points whose nearest neighbor is this grid point
            if (present(mask_array) .and. present(mask_relational)) then
               do i=min_i,max_i
                  do j=min_j,max_j
                     do k=min_k,max_k
                        ! Ignore masked/missing values in the source data
                        if (tile_array(i,j,k) /= msgval) then
                           if (mask_relational == ' ' .and. mask_array(i,j) /= maskval) then
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           else if (mask_relational == '<' .and. mask_array(i,j) >= maskval) then
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           else if (mask_relational == '>' .and. mask_array(i,j) <= maskval) then
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           end if
                        end if
                     end do
                  end do
               end do
            else if (present(mask_array)) then
               do i=min_i,max_i
                  do j=min_j,max_j
                     do k=min_k,max_k
                        ! Ignore masked/missing values in the source data
                        if ((tile_array(i,j,k) /= msgval) .and. &
                            (mask_array(i,j) /= maskval)) then
                           dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                           n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                           call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                        end if
                     end do
                  end do
               end do
            else
               do i=min_i,max_i
                  do j=min_j,max_j
                     do k=min_k,max_k
                        ! Ignore masked/missing values in the source data
                        if ((tile_array(i,j,k) /= msgval)) then
                           dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                           n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                           call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                        end if
                     end do
                  end do
               end do
            end if
   
!         end if
   
      ! Rectangle is a square of four points, and we can simply deal with each of the points
      else if (((max_i - min_i + 1) <= 2) .and. ((max_j - min_j + 1) <= 2)) then
         do i=min_i,max_i
            do j=min_j,max_j
               x_dest = where_maps_to(i,j,1)
               y_dest = where_maps_to(i,j,2)
     
               if (x_dest /= OUTSIDE_DOMAIN) then 
   
!                  if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                     if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                        do k=min_k,max_k
                           dst_array(x_dest,y_dest,k) = 0.
                        end do
                     end if
                     
                     if (present(mask_array) .and. present(mask_relational)) then
                        do k=min_k,max_k
                           ! Ignore masked/missing values
                           if (tile_array(i,j,k) /= msgval) then
                              if (mask_relational == ' ' .and. mask_array(i,j) /= maskval) then
                                 dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                                 n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                                 call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                              else if (mask_relational == '<' .and. mask_array(i,j) >= maskval) then
                                 dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                                 n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                                 call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                              else if (mask_relational == '>' .and. mask_array(i,j) <= maskval) then
                                 dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                                 n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                                 call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                              end if
                           end if
                        end do
                     else if (present(mask_array)) then
                        do k=min_k,max_k
                           ! Ignore masked/missing values
                           if ((tile_array(i,j,k) /= msgval) .and. &
                                (mask_array(i,j) /= maskval)) then
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           end if
                        end do
                     else
                        do k=min_k,max_k
                           ! Ignore masked/missing values
                           if ((tile_array(i,j,k) /= msgval)) then 
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           end if
                        end do
                     end if
!                  end if
     
               end if
            end do
         end do
   
      ! Not all corners map to the same grid point, and the rectangle contains more than
      !   four points
      else
         center_i = (max_i + min_i)/2
         center_j = (max_j + min_j)/2
   
         ! Recursively process lower-left rectangle
         call process_continuous_block(tile_array, where_maps_to, &
                    src_min_x, src_min_y, src_min_z, &
                    src_max_x, src_max_y, src_max_z, &
                    min_i, min_j, min_k, &
                    center_i, center_j, max_k, &
                    dst_array, n, &
                    start_x, end_x, start_y, end_y, start_z, end_z, &
                    istagger, &
                    new_pts, sr_x, sr_y, msgval, maskval, mask_relational, mask_array) 
         
         if (center_i < max_i) then
            ! Recursively process lower-right rectangle
            call process_continuous_block(tile_array, where_maps_to, &
                       src_min_x, src_min_y, src_min_z, &
                       src_max_x, src_max_y, src_max_z, &
                       center_i+1, min_j, min_k, max_i, &
                       center_j, max_k, &
                       dst_array, n, &
                       start_x, end_x, start_y, &
                       end_y, start_z, end_z, &
                       istagger, &
                       new_pts, sr_x, sr_y, msgval, maskval, mask_relational, mask_array) 
         end if
   
         if (center_j < max_j) then
            ! Recursively process upper-left rectangle
            call process_continuous_block(tile_array, where_maps_to, &
                       src_min_x, src_min_y, src_min_z, &
                       src_max_x, src_max_y, src_max_z, &
                       min_i, center_j+1, min_k, center_i, &
                       max_j, max_k, &
                       dst_array, n, &
                       start_x, end_x, start_y, &
                       end_y, start_z, end_z, &
                       istagger, &
                       new_pts, sr_x, sr_y, msgval, maskval, mask_relational, mask_array) 
         end if
   
         if (center_i < max_i .and. center_j < max_j) then
            ! Recursively process upper-right rectangle
            call process_continuous_block(tile_array, where_maps_to, &
                       src_min_x, src_min_y, src_min_z, &
                       src_max_x, src_max_y, src_max_z, &
                       center_i+1, center_j+1, min_k, max_i, &
                       max_j, max_k, &
                       dst_array, n, &
                       start_x, end_x, start_y, &
                       end_y, start_z, end_z, &
                       istagger, &
                       new_pts, sr_x, sr_y, msgval, maskval, mask_relational, mask_array) 
         end if
      end if
   
   end subroutine process_continuous_block

end module process_domain_module
