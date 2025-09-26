module input_module

   use gridinfo_module
   use misc_definitions_module
   use module_debug
#ifdef IO_BINARY
   use module_internal_header_util
#endif
   use parallel_module
   use queue_module
 
   type (queue) :: unit_desc
 
   ! WRF I/O API related variables
   integer :: handle
 
   integer :: num_calls
 
   character (len=1) :: internal_gridtype
 
   contains
 
 
   subroutine input_init(nest_number, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: nest_number
      integer, intent(out) :: istatus
  
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  
      ! Local variables
      integer :: i
      integer :: comm_1, comm_2
      character (len=MAX_FILENAME_LEN) :: input_fname
  
      istatus = 0
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
#ifdef IO_BINARY
         if (io_form_input == BINARY) call ext_int_ioinit('sysdep info', istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) call ext_ncd_ioinit('sysdep info', istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) call ext_gr1_ioinit('sysdep info', istatus)
#endif
         call mprintf((istatus /= 0),ERROR,'Error in ext_pkg_ioinit')
     
         comm_1 = 1
         comm_2 = 1
         input_fname = ' '
         if (gridtype == 'C') then
#ifdef IO_BINARY
            if (io_form_input == BINARY) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .int'
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .nc'
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) input_fname = trim(opt_output_from_geogrid_path)//'geo_em.d  .grib'
#endif
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+9:i+10),'(i2.2)') nest_number
         else if (gridtype == 'E') then
#ifdef IO_BINARY
            if (io_form_input == BINARY) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .int'
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .nc'
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) input_fname = trim(opt_output_from_geogrid_path)//'geo_nmm.d  .grib'
#endif
            i = len_trim(opt_output_from_geogrid_path)
            write(input_fname(i+10:i+11),'(i2.2)') nest_number
         end if

         if (nprocs > 1 .and. do_tiled_input) then
            write(input_fname(len_trim(input_fname)+1:len_trim(input_fname)+5), '(a1,i4.4)') &
                            '_', my_proc_id
         end if
     
         istatus = 0
#ifdef IO_BINARY
         if (io_form_input == BINARY) &
            call ext_int_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) &
            call ext_ncd_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) &
            call ext_gr1_open_for_read(trim(input_fname), comm_1, comm_2, 'sysdep info', handle, istatus)
#endif
         call mprintf((istatus /= 0),ERROR,'Couldn''t open file %s for input.',s1=input_fname)
     
         call q_init(unit_desc)
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)
  
      num_calls = 0
 
   end subroutine input_init
 
 
   subroutine read_next_field(start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k, &
                              cname, cunits, cdesc, memorder, stagger, &
                              dimnames, sr_x, sr_y, real_array, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: start_patch_i, end_patch_i, &
                              start_patch_j, end_patch_j, &
                              start_patch_k, end_patch_k, &
                              sr_x, sr_y
      real, pointer, dimension(:,:,:) :: real_array
      character (len=*), intent(out) :: cname, memorder, stagger, cunits, cdesc
      character (len=128), dimension(3), intent(inout) :: dimnames
      integer, intent(inout) :: istatus
  
#include "wrf_io_flags.h"
#include "wrf_status_codes.h"
  
      ! Local variables
      integer :: ndim, wrftype
      integer :: sm1, em1, sm2, em2, sm3, em3, sp1, ep1, sp2, ep2, sp3, ep3
      integer, dimension(3) :: domain_start, domain_end, temp
      real, pointer, dimension(:,:,:) :: real_domain
      character (len=20) :: datestr
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         if (num_calls == 0) then
#ifdef IO_BINARY
            if (io_form_input == BINARY) call ext_int_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) call ext_ncd_get_next_time(handle, datestr, istatus)
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) call ext_gr1_get_next_time(handle, datestr, istatus)
#endif
         end if
     
         num_calls = num_calls + 1
   
#ifdef IO_BINARY
         if (io_form_input == BINARY) call ext_int_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) call ext_ncd_get_next_var(handle, cname, istatus) 
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) call ext_gr1_get_next_var(handle, cname, istatus) 
#endif
      end if
  
      if (nprocs > 1 .and. .not. do_tiled_input) call parallel_bcast_int(istatus)
      if (istatus /= 0) return
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
         istatus = 0
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            call ext_int_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
         end if
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) then
            call ext_ncd_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
            call ext_ncd_get_var_ti_integer(handle, 'sr_x', &
                                            trim(cname), temp(1), 1, temp(3), istatus)
            call ext_ncd_get_var_ti_integer(handle, 'sr_y', &
                                            trim(cname), temp(2), 1, temp(3), istatus)
         end if
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) then
            call ext_gr1_get_var_info(handle, cname, ndim, memorder, stagger, domain_start, domain_end, wrftype, istatus)
            call ext_gr1_get_var_ti_integer(handle, 'sr_x', &
                                            trim(cname), temp(1), 1, temp(3), istatus)
            call ext_gr1_get_var_ti_integer(handle, 'sr_y', &
                                            trim(cname), temp(2), 1, temp(3), istatus)
         end if
#endif
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), problems with ext_pkg_get_var_info()')

         start_patch_i = domain_start(1) 
         start_patch_j = domain_start(2) 
         end_patch_i = domain_end(1)
         end_patch_j = domain_end(2)
         if (ndim == 3) then
            start_patch_k = domain_start(3) 
            end_patch_k = domain_end(3) 
         else
            domain_start(3) = 1
            domain_end(3) = 1
            start_patch_k = 1
            end_patch_k = 1
         end if
     
         nullify(real_domain)
     
         allocate(real_domain(start_patch_i:end_patch_i, start_patch_j:end_patch_j, start_patch_k:end_patch_k))
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            call ext_int_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) then
            call ext_ncd_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) then
            call ext_gr1_read_field(handle, '0000-00-00_00:00:00', cname, real_domain, WRF_REAL, &
                          1, 1, 0, memorder, stagger, &
                          dimnames, domain_start, domain_end, domain_start, domain_end, &
                          domain_start, domain_end, istatus)
         end if
#endif
     
         call mprintf((istatus /= 0),ERROR,'In read_next_field(), got error code %i.', i1=istatus)

         if (io_form_input == BINARY) then
            qd = q_remove(unit_desc)
            cunits = qd%units
            cdesc = qd%description
            stagger = qd%stagger
            sr_x = qd%sr_x
            sr_y = qd%sr_y
         else
            cunits = ' '
            cdesc = ' '
            stagger = ' '
            sr_x = temp(1)
            sr_y = temp(2)
        
#ifdef IO_NETCDF
            if (io_form_input == NETCDF) then
               call ext_ncd_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_ncd_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_ncd_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
#endif
#ifdef IO_GRIB1
            if (io_form_input == GRIB1) then
               call ext_gr1_get_var_ti_char(handle, 'units', cname, cunits, istatus)
               call ext_gr1_get_var_ti_char(handle, 'description', cname, cdesc, istatus)
               call ext_gr1_get_var_ti_char(handle, 'stagger', cname, stagger, istatus)
            end if
#endif
         end if
  
      end if ! (my_proc_id == IO_NODE .or. do_tiled_input)

      if (nprocs > 1 .and. .not. do_tiled_input) then
         call parallel_bcast_char(cname, len(cname))
         call parallel_bcast_char(cunits, len(cunits))
         call parallel_bcast_char(cdesc, len(cdesc))
         call parallel_bcast_char(memorder, len(memorder))
         call parallel_bcast_char(stagger, len(stagger))
         call parallel_bcast_char(dimnames(1), 128)
         call parallel_bcast_char(dimnames(2), 128)
         call parallel_bcast_char(dimnames(3), 128)
         call parallel_bcast_int(domain_start(3))
         call parallel_bcast_int(domain_end(3))
         call parallel_bcast_int(sr_x)
         call parallel_bcast_int(sr_y)
   
         sp1 = my_minx
         ep1 = my_maxx - 1
         sp2 = my_miny
         ep2 = my_maxy - 1
         sp3 = domain_start(3)
         ep3 = domain_end(3)
   
         if (internal_gridtype == 'C') then
            if (my_x /= nproc_x - 1 .or. stagger == 'U' .or. stagger == 'CORNER' .or. sr_x > 1) then
               ep1 = ep1 + 1
            end if
            if (my_y /= nproc_y - 1 .or. stagger == 'V' .or. stagger == 'CORNER' .or. sr_y > 1) then
               ep2 = ep2 + 1
            end if
         else if (internal_gridtype == 'E') then
            ep1 = ep1 + 1
            ep2 = ep2 + 1
         end if
   
         if (sr_x > 1) then
            sp1 = (sp1-1)*sr_x+1
            ep1 =  ep1   *sr_x
         end if
         if (sr_y > 1) then
            sp2 = (sp2-1)*sr_y+1
            ep2 =  ep2   *sr_y
         end if

         sm1 = sp1
         em1 = ep1
         sm2 = sp2
         em2 = ep2
         sm3 = sp3
         em3 = ep3
   
         start_patch_i = sp1
         end_patch_i   = ep1
         start_patch_j = sp2
         end_patch_j   = ep2
         start_patch_k = sp3
         end_patch_k   = ep3
   
         allocate(real_array(sm1:em1,sm2:em2,sm3:em3))
         if (my_proc_id /= IO_NODE) then
            allocate(real_domain(1,1,1))
            domain_start(1) = 1
            domain_start(2) = 1
            domain_start(3) = 1
            domain_end(1) = 1
            domain_end(2) = 1
            domain_end(3) = 1
         end if
         call scatter_whole_field_r(real_array, &
                                   sm1, em1, sm2, em2, sm3, em3, &
                                   sp1, ep1, sp2, ep2, sp3, ep3, &
                                   real_domain, &
                                   domain_start(1), domain_end(1), &
                                   domain_start(2), domain_end(2), &
                                   domain_start(3), domain_end(3))
         deallocate(real_domain)

      else
  
         real_array => real_domain

      end if
 
   end subroutine read_next_field
 
   subroutine read_global_attrs(title, start_date, grid_type, dyn_opt,                                &
                                west_east_dim, south_north_dim, bottom_top_dim,                       &
                                we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag,             &
                                sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag,             &
                                map_proj, mminlu, num_land_cat, is_water, is_lake, is_ice, is_urban,  &
                                isoilwater, grid_id, parent_id, i_parent_start, j_parent_start,       &
                                i_parent_end, j_parent_end, dx, dy, cen_lat, moad_cen_lat, cen_lon,   &
                                stand_lon, truelat1, truelat2, pole_lat, pole_lon, parent_grid_ratio, &
                                corner_lats, corner_lons, sr_x, sr_y)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: dyn_opt, west_east_dim, south_north_dim, bottom_top_dim, map_proj,   &
                 is_water, is_lake, we_patch_s, we_patch_e, we_patch_s_stag, we_patch_e_stag,      &
                 sn_patch_s, sn_patch_e, sn_patch_s_stag, sn_patch_e_stag,                         &
                 is_ice, is_urban, isoilwater, grid_id, parent_id, i_parent_start, j_parent_start, &
                 i_parent_end, j_parent_end, parent_grid_ratio, sr_x, sr_y, num_land_cat
      real, intent(out) :: dx, dy, cen_lat, moad_cen_lat, cen_lon, stand_lon, truelat1, truelat2,  &
                 pole_lat, pole_lon
      real, dimension(16), intent(out) :: corner_lats, corner_lons
      character (len=128), intent(out) :: title, start_date, grid_type, mminlu
  
      ! Local variables
      integer :: istatus, i
      real :: wps_version
      character (len=128) :: cunits, cdesc, cstagger
      integer, dimension(3) :: sr
      type (q_data) :: qd
  
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
  
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            istatus = 0
            do while (istatus == 0) 
               cunits = ' '
               cdesc = ' '
               cstagger = ' '
               sr = 0
               call ext_int_get_var_ti_char(handle, 'units', 'VAR', cunits, istatus)
         
               if (istatus == 0) then
                  call ext_int_get_var_ti_char(handle, 'description', 'VAR', cdesc, istatus)
          
                  if (istatus == 0) then
                     call ext_int_get_var_ti_char(handle, 'stagger', 'VAR', cstagger, istatus)

                    if (istatus == 0) then
                    call ext_int_get_var_ti_integer(handle, 'sr_x', 'VAR', sr(1), 1, sr(3), istatus)

                        if (istatus == 0) then
                        call ext_int_get_var_ti_integer(handle, 'sr_y', 'VAR', sr(2), 1, sr(3), istatus)
         
                             qd%units = cunits
                             qd%description = cdesc
                             qd%stagger = cstagger
                             qd%sr_x = sr(1)
                             qd%sr_y = sr(2)
                             call q_insert(unit_desc, qd)
 
                        end if
                    end if
                  end if
               end if
            end do
         end if
#endif
     
         call ext_get_dom_ti_char          ('TITLE', title)
         if (index(title,'GEOGRID V4.2') /= 0) then
            wps_version = 4.2
         else if (index(title,'GEOGRID V4.1') /= 0) then
            wps_version = 4.1
         else if (index(title,'GEOGRID V4.0.3') /= 0) then
            wps_version = 4.03
         else if (index(title,'GEOGRID V4.0.2') /= 0) then
            wps_version = 4.02
         else if (index(title,'GEOGRID V4.0.1') /= 0) then
            wps_version = 4.01
         else if (index(title,'GEOGRID V4.0') /= 0) then
            wps_version = 4.0
         else if (index(title,'GEOGRID V3.9.1') /= 0) then
            wps_version = 3.91
         else if (index(title,'GEOGRID V3.9.0.1') /= 0) then
            wps_version = 3.901
         else if (index(title,'GEOGRID V3.9') /= 0) then
            wps_version = 3.9
         else if (index(title,'GEOGRID V3.8.1') /= 0) then
            wps_version = 3.81
         else if (index(title,'GEOGRID V3.8') /= 0) then
            wps_version = 3.8
         else if (index(title,'GEOGRID V3.7.1') /= 0) then
            wps_version = 3.71
         else if (index(title,'GEOGRID V3.7') /= 0) then
            wps_version = 3.7
         else if (index(title,'GEOGRID V3.6.1') /= 0) then
            wps_version = 3.61
         else if (index(title,'GEOGRID V3.6') /= 0) then
            wps_version = 3.6
         else if (index(title,'GEOGRID V3.5.1') /= 0) then
            wps_version = 3.51
         else if (index(title,'GEOGRID V3.5') /= 0) then
            wps_version = 3.5
         else if (index(title,'GEOGRID V3.4.1') /= 0) then
            wps_version = 3.41
         else if (index(title,'GEOGRID V3.4') /= 0) then
            wps_version = 3.4
         else if (index(title,'GEOGRID V3.3.1') /= 0) then
            wps_version = 3.31
         else if (index(title,'GEOGRID V3.3') /= 0) then
            wps_version = 3.3
         else if (index(title,'GEOGRID V3.2.1') /= 0) then
            wps_version = 3.21
         else if (index(title,'GEOGRID V3.2') /= 0) then
            wps_version = 3.2
         else if (index(title,'GEOGRID V3.1.1') /= 0) then
            wps_version = 3.11
         else if (index(title,'GEOGRID V3.1') /= 0) then
            wps_version = 3.1
         else if (index(title,'GEOGRID V3.0.1') /= 0) then
            wps_version = 3.01
         else
            wps_version = 3.0
         end if
         call mprintf(.true.,DEBUG,'Reading static data from WPS version %f', f1=wps_version)
         call ext_get_dom_ti_char          ('SIMULATION_START_DATE', start_date)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_GRID_DIMENSION', west_east_dim)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_GRID_DIMENSION', south_north_dim)
         call ext_get_dom_ti_integer_scalar('BOTTOM-TOP_GRID_DIMENSION', bottom_top_dim)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_UNSTAG', we_patch_s)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_UNSTAG', we_patch_e)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_START_STAG', we_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('WEST-EAST_PATCH_END_STAG', we_patch_e_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_UNSTAG', sn_patch_s)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_UNSTAG', sn_patch_e)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_START_STAG', sn_patch_s_stag)
         call ext_get_dom_ti_integer_scalar('SOUTH-NORTH_PATCH_END_STAG', sn_patch_e_stag)
         call ext_get_dom_ti_char          ('GRIDTYPE', grid_type)
         call ext_get_dom_ti_real_scalar   ('DX', dx)
         call ext_get_dom_ti_real_scalar   ('DY', dy)
         call ext_get_dom_ti_integer_scalar('DYN_OPT', dyn_opt)
         call ext_get_dom_ti_real_scalar   ('CEN_LAT', cen_lat)
         call ext_get_dom_ti_real_scalar   ('CEN_LON', cen_lon)
         call ext_get_dom_ti_real_scalar   ('TRUELAT1', truelat1)
         call ext_get_dom_ti_real_scalar   ('TRUELAT2', truelat2)
         call ext_get_dom_ti_real_scalar   ('MOAD_CEN_LAT', moad_cen_lat)
         call ext_get_dom_ti_real_scalar   ('STAND_LON', stand_lon)
         call ext_get_dom_ti_real_scalar   ('POLE_LAT', pole_lat)
         call ext_get_dom_ti_real_scalar   ('POLE_LON', pole_lon)
         call ext_get_dom_ti_real_vector   ('corner_lats', corner_lats, 16)
         call ext_get_dom_ti_real_vector   ('corner_lons', corner_lons, 16)
         call ext_get_dom_ti_integer_scalar('MAP_PROJ', map_proj)
         call ext_get_dom_ti_char          ('MMINLU', mminlu)
         if ( wps_version >= 3.01 ) then
            call ext_get_dom_ti_integer_scalar('NUM_LAND_CAT', num_land_cat)
         else
            num_land_cat = 24
         end if
         call ext_get_dom_ti_integer_scalar('ISWATER', is_water)
         if ( wps_version >= 3.01 ) then
            call ext_get_dom_ti_integer_scalar('ISLAKE', is_lake)
         else
            is_lake = -1
         end if
         call ext_get_dom_ti_integer_scalar('ISICE', is_ice)
         call ext_get_dom_ti_integer_scalar('ISURBAN', is_urban)
         call ext_get_dom_ti_integer_scalar('ISOILWATER', isoilwater)
         call ext_get_dom_ti_integer_scalar('grid_id', grid_id)
         call ext_get_dom_ti_integer_scalar('parent_id', parent_id)
         call ext_get_dom_ti_integer_scalar('i_parent_start', i_parent_start)
         call ext_get_dom_ti_integer_scalar('j_parent_start', j_parent_start)
         call ext_get_dom_ti_integer_scalar('i_parent_end', i_parent_end)
         call ext_get_dom_ti_integer_scalar('j_parent_end', j_parent_end)
         call ext_get_dom_ti_integer_scalar('parent_grid_ratio', parent_grid_ratio)
         call ext_get_dom_ti_integer_scalar('sr_x', sr_x)
         call ext_get_dom_ti_integer_scalar('sr_y', sr_y)
   
      end if

  
      if (nprocs > 1 .and. .not. do_tiled_input) then
  
         call parallel_bcast_char(title, len(title))
         call parallel_bcast_char(start_date, len(start_date))
         call parallel_bcast_char(grid_type, len(grid_type))
         call parallel_bcast_int(west_east_dim)
         call parallel_bcast_int(south_north_dim)
         call parallel_bcast_int(bottom_top_dim)
         call parallel_bcast_int(we_patch_s)
         call parallel_bcast_int(we_patch_e)
         call parallel_bcast_int(we_patch_s_stag)
         call parallel_bcast_int(we_patch_e_stag)
         call parallel_bcast_int(sn_patch_s)
         call parallel_bcast_int(sn_patch_e)
         call parallel_bcast_int(sn_patch_s_stag)
         call parallel_bcast_int(sn_patch_e_stag)
         call parallel_bcast_int(sr_x)
         call parallel_bcast_int(sr_y)

         ! Must figure out patch dimensions from info in parallel module
!         we_patch_s      = my_minx
!         we_patch_s_stag = my_minx
!         we_patch_e      = my_maxx - 1
!         sn_patch_s      = my_miny
!         sn_patch_s_stag = my_miny
!         sn_patch_e      = my_maxy - 1
!
!         if (trim(grid_type) == 'C') then
!            if (my_x /= nproc_x - 1) then
!               we_patch_e_stag = we_patch_e + 1
!            end if
!            if (my_y /= nproc_y - 1) then
!               sn_patch_e_stag = sn_patch_e + 1
!            end if
!         else if (trim(grid_type) == 'E') then
!            we_patch_e = we_patch_e + 1
!            sn_patch_e = sn_patch_e + 1
!            we_patch_e_stag = we_patch_e
!            sn_patch_e_stag = sn_patch_e
!         end if

         call parallel_bcast_real(dx)
         call parallel_bcast_real(dy)
         call parallel_bcast_int(dyn_opt)
         call parallel_bcast_real(cen_lat)
         call parallel_bcast_real(cen_lon)
         call parallel_bcast_real(truelat1)
         call parallel_bcast_real(truelat2)
         call parallel_bcast_real(pole_lat)
         call parallel_bcast_real(pole_lon)
         call parallel_bcast_real(moad_cen_lat)
         call parallel_bcast_real(stand_lon)
         do i=1,16
            call parallel_bcast_real(corner_lats(i))
            call parallel_bcast_real(corner_lons(i))
         end do
         call parallel_bcast_int(map_proj)
         call parallel_bcast_char(mminlu, len(mminlu))
         call parallel_bcast_int(is_water)
         call parallel_bcast_int(is_lake)
         call parallel_bcast_int(is_ice)
         call parallel_bcast_int(is_urban)
         call parallel_bcast_int(isoilwater)
         call parallel_bcast_int(grid_id)
         call parallel_bcast_int(parent_id)
         call parallel_bcast_int(i_parent_start)
         call parallel_bcast_int(i_parent_end)
         call parallel_bcast_int(j_parent_start)
         call parallel_bcast_int(j_parent_end)
         call parallel_bcast_int(parent_grid_ratio)
      end if
  
      internal_gridtype = grid_type
 
   end subroutine read_global_attrs


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_scalar(var_name, var_value, suppress_errors)

      implicit none

      ! Arguments
      integer, intent(out) :: var_value
      character (len=*), intent(in) :: var_name
      logical, intent(in), optional :: suppress_errors

      ! Local variables
      integer :: istatus, outcount

#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif

      if (present(suppress_errors)) then
         call mprintf((istatus /= 0 .and. .not.suppress_errors),ERROR,'Error while reading domain time-independent attribute.')
      else
         call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')
      end if

   end subroutine ext_get_dom_ti_integer_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_integer
   !
   ! Purpose: Read a domain time-independent integer attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_integer_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      integer, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_integer(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_integer_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_scalar(var_name, var_value)

      implicit none

      ! Arguments
      real, intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         1, outcount, istatus)
      end if
#endif

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_scalar


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_real
   !
   ! Purpose: Read a domain time-independent real attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_real_vector(var_name, var_value, n)

      implicit none

      ! Arguments
      integer, intent(in) :: n
      real, dimension(n), intent(out) :: var_value
      character (len=*), intent(in) :: var_name

      ! Local variables
      integer :: istatus, outcount

#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_real(handle, trim(var_name), &
                                         var_value, &
                                         n, outcount, istatus)
      end if
#endif

      call mprintf((istatus /= 0),ERROR,'Error while reading domain time-independent attribute.')

   end subroutine ext_get_dom_ti_real_vector


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: ext_get_dom_ti_char
   !
   ! Purpose: Read a domain time-independent character attribute from input.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine ext_get_dom_ti_char(var_name, var_value)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: var_name
      character (len=128), intent(out) :: var_value

      ! Local variables
      integer :: istatus

#ifdef IO_BINARY
      if (io_form_input == BINARY) then
         call ext_int_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if
#endif
#ifdef IO_NETCDF
      if (io_form_input == NETCDF) then
         call ext_ncd_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if
#endif
#ifdef IO_GRIB1
      if (io_form_input == GRIB1) then
         call ext_gr1_get_dom_ti_char(handle, trim(var_name), &
                                         var_value, &
                                         istatus)
      end if
#endif

      call mprintf((istatus /= 0),ERROR,'Error in reading domain time-independent attribute')

   end subroutine ext_get_dom_ti_char

 
   subroutine input_close()
 
      implicit none
  
      ! Local variables
      integer :: istatus
  
      istatus = 0
      if (my_proc_id == IO_NODE .or. do_tiled_input) then
#ifdef IO_BINARY
         if (io_form_input == BINARY) then
            call ext_int_ioclose(handle, istatus)
            call ext_int_ioexit(istatus)
         end if
#endif
#ifdef IO_NETCDF
         if (io_form_input == NETCDF) then
            call ext_ncd_ioclose(handle, istatus)
            call ext_ncd_ioexit(istatus)
         end if
#endif
#ifdef IO_GRIB1
         if (io_form_input == GRIB1) then
            call ext_gr1_ioclose(handle, istatus)
            call ext_gr1_ioexit(istatus)
         end if
#endif
      end if
 
      call q_destroy(unit_desc)
 
   end subroutine input_close
 
end module input_module
