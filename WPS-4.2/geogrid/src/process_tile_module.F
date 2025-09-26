!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: process_tile
!
! Description:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module process_tile_module

   use module_debug


   contains


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_tile
   !
   ! Purpose: To process a tile, whose lower-left corner is at 
   !       (tile_i_min, tile_j_min) and whose upper-right corner is at
   !       (tile_i_max, tile_j_max), of the model grid given by which_domain 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_tile(which_domain, grid_type, dynopt,        &
                           dummy_start_dom_i, dummy_end_dom_i,     &
                           dummy_start_dom_j, dummy_end_dom_j,     &
                           dummy_start_patch_i, dummy_end_patch_i, &
                           dummy_start_patch_j, dummy_end_patch_j, &
                           extra_col, extra_row)
   
      use bitarray_module
      use hash_module
      use llxy_module
      use misc_definitions_module
      use output_module
      use smooth_module
      use source_data_module
    
      implicit none

      ! Arguments
      integer, intent(in) :: which_domain, dynopt, &
                             dummy_start_dom_i, dummy_end_dom_i, dummy_start_dom_j, dummy_end_dom_j, &
                             dummy_start_patch_i, dummy_end_patch_i, dummy_start_patch_j, dummy_end_patch_j
      logical, intent(in) :: extra_col, extra_row
      character (len=1), intent(in) :: grid_type
    
      ! Local variables
      integer :: i, j, k, kk, istatus, ifieldstatus, idomcatstatus, field_count
      integer :: min_category, max_category, min_level, max_level, &
                 smth_opt, smth_passes, num_landmask_categories
      integer :: start_dom_i, end_dom_i, start_dom_j, end_dom_j, end_dom_stag_i, end_dom_stag_j
      integer :: start_patch_i, end_patch_i, start_patch_j, end_patch_j, end_patch_stag_i, end_patch_stag_j
      integer :: start_mem_i, end_mem_i, start_mem_j, end_mem_j, end_mem_stag_i, end_mem_stag_j
      integer :: sm1, em1, sm2, em2
      integer :: istagger
      integer, dimension(MAX_LANDMASK_CATEGORIES) :: landmask_value
      real :: sum, dominant, msg_fill_val, topo_flag_val, mass_flag, land_total, water_total
      real, dimension(16) :: corner_lats, corner_lons
      real, pointer, dimension(:,:) :: xlat_array,   xlon_array,   &
                                       xlat_array_u, xlon_array_u, &
                                       xlat_array_v, xlon_array_v, &
                                       xlat_array_corner, xlon_array_corner, &
                                       clat_array,   clon_array,   &
                                       xlat_array_subgrid, xlon_array_subgrid, &
                                       f_array, e_array, &
                                       mapfac_array_m_x, mapfac_array_u_x, mapfac_array_v_x, &
                                       mapfac_array_m_y, mapfac_array_u_y, mapfac_array_v_y, &
                                       mapfac_array_x_subgrid, mapfac_array_y_subgrid,       &
                                       sina_array, cosa_array
      real, pointer, dimension(:,:) :: xlat_ptr, xlon_ptr, mapfac_ptr_x, mapfac_ptr_y, landmask, dominant_field
      real, pointer, dimension(:,:,:) :: field, slp_field
      logical :: is_water_mask, only_save_dominant, halt_on_missing
      character (len=19) :: datestr
      character (len=128) :: fieldname, gradname, domname, landmask_name
      character (len=256) :: temp_string
      type (bitarray) :: processed_domain 
      type (hashtable) :: processed_fieldnames
      character (len=128), dimension(2) :: dimnames
      integer :: sub_x, sub_y
      integer :: opt_status
      real :: dom_dx,dom_dy
      ! Probably not all of these nullify statements are needed...
      nullify(xlat_array)
      nullify(xlon_array)
      nullify(xlat_array_u)
      nullify(xlon_array_u)
      nullify(xlat_array_v)
      nullify(xlon_array_v)
      nullify(xlat_array_corner)
      nullify(xlon_array_corner)
      nullify(clat_array)
      nullify(clon_array)
      nullify(xlat_array_subgrid)
      nullify(xlon_array_subgrid)
      nullify(f_array)
      nullify(e_array)
      nullify(mapfac_array_m_x)
      nullify(mapfac_array_u_x)
      nullify(mapfac_array_v_x)
      nullify(mapfac_array_m_y)
      nullify(mapfac_array_u_y)
      nullify(mapfac_array_v_y)
      nullify(mapfac_array_x_subgrid)
      nullify(mapfac_array_y_subgrid)
      nullify(sina_array)
      nullify(cosa_array)
      nullify(xlat_ptr)
      nullify(xlon_ptr)
      nullify(mapfac_ptr_x)
      nullify(mapfac_ptr_y)
      nullify(landmask)
      nullify(dominant_field)
      nullify(field)
      nullify(slp_field)
   
      datestr = '0000-00-00_00:00:00'
      field_count = 0
      mass_flag=1.0
    
      ! The following pertains primarily to the C grid
      ! Determine whether only (n-1)th rows/columns should be computed for variables
      !   on staggered grid. In a distributed memory situation, not every tile should
      !   have only (n-1)th rows/columns computed, or we end up with (n-k) 
      !   rows/columns when there are k patches in the y/x direction
      if (extra_col) then
         start_patch_i    = dummy_start_patch_i    ! The seemingly pointless renaming of start
         end_patch_i      = dummy_end_patch_i - 1  !   naming convention with modified end_patch variables, 
         end_patch_stag_i = dummy_end_patch_i      !   variables is so that we can maintain consistent
                                                   !   which are marked as intent(in)
         start_mem_i    = start_patch_i    - HALO_WIDTH
         end_mem_i      = end_patch_i      + HALO_WIDTH
         end_mem_stag_i = end_patch_stag_i + HALO_WIDTH
      else                                     
         start_patch_i    = dummy_start_patch_i
         end_patch_i      = dummy_end_patch_i
         end_patch_stag_i = dummy_end_patch_i

         start_mem_i    = start_patch_i    - HALO_WIDTH
         end_mem_i      = end_patch_i      + HALO_WIDTH
         end_mem_stag_i = end_patch_stag_i + HALO_WIDTH
      end if
    
      if (extra_row) then
         start_patch_j    = dummy_start_patch_j
         end_patch_j      = dummy_end_patch_j - 1
         end_patch_stag_j = dummy_end_patch_j

         start_mem_j    = start_patch_j    - HALO_WIDTH
         end_mem_j      = end_patch_j      + HALO_WIDTH
         end_mem_stag_j = end_patch_stag_j + HALO_WIDTH
      else
         start_patch_j    = dummy_start_patch_j
         end_patch_j      = dummy_end_patch_j
         end_patch_stag_j = dummy_end_patch_j

         start_mem_j    = start_patch_j    - HALO_WIDTH
         end_mem_j      = end_patch_j      + HALO_WIDTH
         end_mem_stag_j = end_patch_stag_j + HALO_WIDTH
      end if

      start_dom_i = dummy_start_dom_i
      if (grid_type == 'C') then
         end_dom_i      = dummy_end_dom_i - 1
         end_dom_stag_i = dummy_end_dom_i
      else if (grid_type == 'E') then
         end_dom_i      = dummy_end_dom_i
         end_dom_stag_i = dummy_end_dom_i
      end if

      start_dom_j = dummy_start_dom_j
      if (grid_type == 'C') then
         end_dom_j      = dummy_end_dom_j - 1
         end_dom_stag_j = dummy_end_dom_j
      else if (grid_type == 'E') then
         end_dom_j      = dummy_end_dom_j
         end_dom_stag_j = dummy_end_dom_j
      end if
    
      ! Allocate arrays to hold all lat/lon fields; these will persist for the duration of
      !   the process_tile routine
      ! For C grid, we have M, U, and V points
      ! For E grid, we have only M and V points
      allocate(xlat_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
      allocate(xlon_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
      allocate(xlat_array_v(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
      allocate(xlon_array_v(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
      if (grid_type == 'C') then
         allocate(xlat_array_u(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
         allocate(xlon_array_u(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
         allocate(clat_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         allocate(clon_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         allocate(xlat_array_corner(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_stag_j))
         allocate(xlon_array_corner(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_stag_j))
      end if
      nullify(xlat_array_subgrid)
      nullify(xlon_array_subgrid)
      nullify(mapfac_array_x_subgrid)
      nullify(mapfac_array_y_subgrid)
    
      ! Initialize hash table to track which fields have been processed
      call hash_init(processed_fieldnames)
    
      !
      ! Calculate lat/lon for every point in the tile (XLAT and XLON)
      ! The xlat_array and xlon_array arrays will be used in processing other fields
      !
      call mprintf(.true.,STDOUT,'  Processing XLAT and XLONG')
    
      if (grid_type == 'C') then
         call get_lat_lon_fields(xlat_array, xlon_array, start_mem_i, &
                               start_mem_j, end_mem_i, end_mem_j, M)
         call get_lat_lon_fields(xlat_array_v, xlon_array_v, start_mem_i, &
                               start_mem_j, end_mem_i, end_mem_stag_j, V)
         call get_lat_lon_fields(xlat_array_u, xlon_array_u, start_mem_i, &
                               start_mem_j, end_mem_stag_i, end_mem_j, U)
         call get_lat_lon_fields(xlat_array_corner, xlon_array_corner, start_mem_i, &
                               start_mem_j, end_mem_stag_i, end_mem_stag_j, CORNER)
         call get_lat_lon_fields(clat_array, clon_array, start_mem_i, &
                               start_mem_j, end_mem_i, end_mem_j, M, comp_ll=.true.)

         corner_lats(1) = xlat_array(start_patch_i,start_patch_j)
         corner_lats(2) = xlat_array(start_patch_i,end_patch_j)
         corner_lats(3) = xlat_array(end_patch_i,end_patch_j)
         corner_lats(4) = xlat_array(end_patch_i,start_patch_j)
     
         corner_lats(5) = xlat_array_u(start_patch_i,start_patch_j)
         corner_lats(6) = xlat_array_u(start_patch_i,end_patch_j)
         corner_lats(7) = xlat_array_u(end_patch_stag_i,end_patch_j)
         corner_lats(8) = xlat_array_u(end_patch_stag_i,start_patch_j)
     
         corner_lats(9)  = xlat_array_v(start_patch_i,start_patch_j)
         corner_lats(10) = xlat_array_v(start_patch_i,end_patch_stag_j)
         corner_lats(11) = xlat_array_v(end_patch_i,end_patch_stag_j)
         corner_lats(12) = xlat_array_v(end_patch_i,start_patch_j)
     
         call xytoll(real(start_patch_i)-0.5, real(start_patch_j)-0.5, corner_lats(13), corner_lons(13), M)
         call xytoll(real(start_patch_i)-0.5, real(end_patch_j)+0.5, corner_lats(14), corner_lons(14), M)
         call xytoll(real(end_patch_i)+0.5, real(end_patch_j)+0.5, corner_lats(15), corner_lons(15), M)
         call xytoll(real(end_patch_i)+0.5, real(start_patch_j)-0.5, corner_lats(16), corner_lons(16), M)

         corner_lons(1) = xlon_array(start_patch_i,start_patch_j)
         corner_lons(2) = xlon_array(start_patch_i,end_patch_j)
         corner_lons(3) = xlon_array(end_patch_i,end_patch_j)
         corner_lons(4) = xlon_array(end_patch_i,start_patch_j)
     
         corner_lons(5) = xlon_array_u(start_patch_i,start_patch_j)
         corner_lons(6) = xlon_array_u(start_patch_i,end_patch_j)
         corner_lons(7) = xlon_array_u(end_patch_stag_i,end_patch_j)
         corner_lons(8) = xlon_array_u(end_patch_stag_i,start_patch_j)
     
         corner_lons(9)  = xlon_array_v(start_patch_i,start_patch_j)
         corner_lons(10) = xlon_array_v(start_patch_i,end_patch_stag_j)
         corner_lons(11) = xlon_array_v(end_patch_i,end_patch_stag_j)
         corner_lons(12) = xlon_array_v(end_patch_i,start_patch_j)
     
      else if (grid_type == 'E') then
         call get_lat_lon_fields(xlat_array, xlon_array, start_mem_i, &
                               start_mem_j, end_mem_i, end_mem_j, HH)
         call get_lat_lon_fields(xlat_array_v, xlon_array_v, start_mem_i, &
                               start_mem_j, end_mem_i, end_mem_stag_j, VV)
   
         corner_lats(1) = xlat_array(start_patch_i,start_patch_j)
         corner_lats(2) = xlat_array(start_patch_i,end_patch_j)
         corner_lats(3) = xlat_array(end_patch_i,end_patch_j)
         corner_lats(4) = xlat_array(end_patch_i,start_patch_j)
     
         corner_lats(5) = xlat_array_v(start_patch_i,start_patch_j)
         corner_lats(6) = xlat_array_v(start_patch_i,end_patch_stag_j)
         corner_lats(7) = xlat_array_v(end_patch_i,end_patch_stag_j)
         corner_lats(8) = xlat_array_v(end_patch_i,start_patch_j)
     
         corner_lats(9)  = 0.0
         corner_lats(10) = 0.0
         corner_lats(11) = 0.0
         corner_lats(12) = 0.0
     
         corner_lats(13) = 0.0
         corner_lats(14) = 0.0
         corner_lats(15) = 0.0
         corner_lats(16) = 0.0
     
         corner_lons(1) = xlon_array(start_patch_i,start_patch_j)
         corner_lons(2) = xlon_array(start_patch_i,end_patch_j)
         corner_lons(3) = xlon_array(end_patch_i,end_patch_j)
         corner_lons(4) = xlon_array(end_patch_i,start_patch_j)
     
         corner_lons(5) = xlon_array_v(start_patch_i,start_patch_j)
         corner_lons(6) = xlon_array_v(start_patch_i,end_patch_stag_j)
         corner_lons(7) = xlon_array_v(end_patch_i,end_patch_stag_j)
         corner_lons(8) = xlon_array_v(end_patch_i,start_patch_j)
     
         corner_lons(9)  = 0.0
         corner_lons(10) = 0.0
         corner_lons(11) = 0.0
         corner_lons(12) = 0.0
     
         corner_lons(13) = 0.0
         corner_lons(14) = 0.0
         corner_lons(15) = 0.0
         corner_lons(16) = 0.0
    
      end if

      ! Initialize the output module now that we have the corner point lats/lons
      call output_init(which_domain, 'OUTPUT FROM GEOGRID V4.2', '0000-00-00_00:00:00', grid_type, dynopt, &
                       corner_lats, corner_lons, &
                       start_dom_i,   end_dom_i,   start_dom_j,   end_dom_j, &
                       start_patch_i, end_patch_i, start_patch_j, end_patch_j, &
                       start_mem_i,   end_mem_i,   start_mem_j,   end_mem_j, &
                       extra_col, extra_row)
    
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLAT_M', datestr, real_array = xlat_array)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                       'XLONG_M', datestr, real_array = xlon_array)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, &
                       'XLAT_V', datestr, real_array = xlat_array_v)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, &
                       'XLONG_V', datestr, real_array = xlon_array_v)
      if (grid_type == 'C') then
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, &
                        'XLAT_U', datestr, real_array = xlat_array_u)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, &
                        'XLONG_U', datestr, real_array = xlon_array_u)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_stag_j, 1, 1, &
                        'XLAT_C', datestr, real_array = xlat_array_corner)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_stag_j, 1, 1, &
                        'XLONG_C', datestr, real_array = xlon_array_corner)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                        'CLAT', datestr, real_array = clat_array)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, &
                        'CLONG', datestr, real_array = clon_array)

         if (associated(clat_array)) deallocate(clat_array)
         if (associated(clon_array)) deallocate(clon_array)

      end if
   

      !
      ! Calculate map factor for current domain
      !
      if (grid_type == 'C') then
         call mprintf(.true.,STDOUT,'  Processing MAPFAC')
    
         allocate(mapfac_array_m_x(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         allocate(mapfac_array_m_y(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         call get_map_factor(xlat_array, xlon_array, mapfac_array_m_x, mapfac_array_m_y, start_mem_i, &
                             start_mem_j, end_mem_i, end_mem_j)
! Global WRF uses map scale factors in X and Y directions, but "regular" WRF uses a single MSF
!    on each staggering. In the case of regular WRF, we can assume that MAPFAC_MX = MAPFAC_MY = MAPFAC_M,
!    and so we can simply write MAPFAC_MX as the MAPFAC_M field. Ultimately, when global WRF is
!    merged into the WRF trunk, we will need only two map scale factor fields for each staggering,
!    in the x and y directions, and these will be the same in the case of non-Cassini projections
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_M',  &
                          datestr, real_array = mapfac_array_m_x)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_MX', &
                          datestr, real_array = mapfac_array_m_x)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_MY', &
                         datestr, real_array = mapfac_array_m_y)
    
         allocate(mapfac_array_v_x(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
         allocate(mapfac_array_v_y(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
         call get_map_factor(xlat_array_v, xlon_array_v, mapfac_array_v_x, mapfac_array_v_y, start_mem_i, &
                             start_mem_j, end_mem_i, end_mem_stag_j)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, 'MAPFAC_V',  &
                          datestr, real_array = mapfac_array_v_x)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, 'MAPFAC_VX', &
                          datestr, real_array = mapfac_array_v_x)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, 'MAPFAC_VY', &
                          datestr, real_array = mapfac_array_v_y)
    
         allocate(mapfac_array_u_x(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
         allocate(mapfac_array_u_y(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
         call get_map_factor(xlat_array_u, xlon_array_u, mapfac_array_u_x, mapfac_array_u_y, start_mem_i, &
                             start_mem_j, end_mem_stag_i, end_mem_j)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_U',  &
                          datestr, real_array = mapfac_array_u_x)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_UX', &
                          datestr, real_array = mapfac_array_u_x)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, 'MAPFAC_UY', &
                          datestr, real_array = mapfac_array_u_y)

      end if
    
    
      !
      ! Coriolis parameters (E and F)
      !
      call mprintf(.true.,STDOUT,'  Processing F and E')
    
      allocate(f_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
      allocate(e_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
    
      call get_coriolis_parameters(xlat_array, f_array, e_array, &
                                   start_mem_i, start_mem_j, end_mem_i, end_mem_j)
    
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'E', &
                       datestr, real_array = e_array)
      call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'F', &
                       datestr, real_array = f_array)
    
      if (associated(f_array)) deallocate(f_array)
      if (associated(e_array)) deallocate(e_array)
    
    
      !
      ! Rotation angle (SINALPHA and COSALPHA)
      !
      if (grid_type == 'C') then
         call mprintf(.true.,STDOUT,'  Processing ROTANG')
    
         ! Mass-staggered points
         allocate(sina_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         allocate(cosa_array(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
    
         call get_rotang(xlat_array, xlon_array, cosa_array, sina_array, &
                         start_mem_i, start_mem_j, end_mem_i, end_mem_j)
    
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'SINALPHA', &
                          datestr, real_array = sina_array)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'COSALPHA', &
                          datestr, real_array = cosa_array)
    
         if (associated(sina_array)) deallocate(sina_array)
         if (associated(cosa_array)) deallocate(cosa_array)

         ! U-staggered points
         allocate(sina_array(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
         allocate(cosa_array(start_mem_i:end_mem_stag_i, start_mem_j:end_mem_j))
    
         call get_rotang(xlat_array_u, xlon_array_u, cosa_array, sina_array, &
                         start_mem_i, start_mem_j, end_mem_stag_i, end_mem_j)
    
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, 'SINALPHA_U', &
                          datestr, real_array = sina_array)
         call write_field(start_mem_i, end_mem_stag_i, start_mem_j, end_mem_j, 1, 1, 'COSALPHA_U', &
                          datestr, real_array = cosa_array)
    
         if (associated(sina_array)) deallocate(sina_array)
         if (associated(cosa_array)) deallocate(cosa_array)
    
         ! V-staggered points
         allocate(sina_array(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
         allocate(cosa_array(start_mem_i:end_mem_i, start_mem_j:end_mem_stag_j))
    
         call get_rotang(xlat_array_v, xlon_array_v, cosa_array, sina_array, &
                         start_mem_i, start_mem_j, end_mem_i, end_mem_stag_j)
    
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, 'SINALPHA_V', &
                          datestr, real_array = sina_array)
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_stag_j, 1, 1, 'COSALPHA_V', &
                          datestr, real_array = cosa_array)
    
         if (associated(sina_array)) deallocate(sina_array)
         if (associated(cosa_array)) deallocate(cosa_array)
      end if
    
      ! Every field up until now should probably just be processed regardless of what the user 
      !   has specified for fields to be processed.
      ! Hereafter, we process user-specified fields
    
      !
      ! First process the field that we will derive a landmask from
      !
      call get_landmask_field(geog_data_res(which_domain), landmask_name, is_water_mask, landmask_value, istatus)

      do kk=1,MAX_LANDMASK_CATEGORIES
         if (landmask_value(kk) == INVALID) then
            num_landmask_categories = kk-1
            exit
         end if
      end do
      if (kk > MAX_LANDMASK_CATEGORIES) num_landmask_categories = MAX_LANDMASK_CATEGORIES

      if (istatus /= 0) then
         call mprintf(.true.,WARN,'No field specified for landmask calculation. Will set landmask=1 at every grid point.')
     
         allocate(landmask(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         landmask = 1.
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'LANDMASK', &
                          datestr, landmask)
    
      else
    
         allocate(landmask(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
         landmask = 1.
     
         call mprintf(.true.,STDOUT,'  Processing %s', s1=trim(landmask_name))
     
         call get_missing_fill_value(landmask_name, msg_fill_val, istatus)
         if (istatus /= 0) msg_fill_val = NAN
     
         call get_halt_on_missing(landmask_name, halt_on_missing, istatus)
         if (istatus /= 0) halt_on_missing = .false.
     
         ! Do we calculate a dominant category for this field?
         call get_domcategory_name(landmask_name, domname, only_save_dominant, idomcatstatus)
     
         temp_string = ' '
         temp_string(1:128) = landmask_name
         call hash_insert(processed_fieldnames, temp_string)
     
         call get_max_categories(landmask_name, min_category, max_category, istatus)
         allocate(field(start_mem_i:end_mem_i, start_mem_j:end_mem_j, min_category:max_category))

         if (.not. only_save_dominant) then
            field_count = field_count + 1
            call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                         i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=landmask_name)
         else
            field_count = field_count + 1
            call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                         i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=domname)
         end if

         if (grid_type == 'C') then
            call calc_field(landmask_name, field, xlat_array, xlon_array, M, &
                            start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                            min_category, max_category, processed_domain, 1, landmask=landmask, sr_x=1, sr_y=1)
         else if (grid_type == 'E') then
            call calc_field(landmask_name, field, xlat_array, xlon_array, HH, &
                            start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                            min_category, max_category, processed_domain, 1, landmask=landmask, sr_x=1, sr_y=1)
         end if
     
         ! If user wants to halt when a missing value is found in output field, check now
         if (halt_on_missing) then
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  ! Only need to examine k=1
                  if (field(i,j,1) == msg_fill_val) then
                     call mprintf(.true.,ERROR,' Missing value encountered in output field. Quitting.')
                  end if
               end do
            end do
         end if
     
         ! Find fractions
         do i=start_mem_i, end_mem_i
            do j=start_mem_j, end_mem_j
               sum = 0.
               do k=min_category,max_category
                  sum = sum + field(i,j,k)
               end do
               if (sum > 0.0) then
                  do k=min_category,max_category
                     field(i,j,k) = field(i,j,k) / sum
                  end do
               else
                  do k=min_category,max_category
                     field(i,j,k) = msg_fill_val
                  end do
               end if
            end do
         end do
     
         if (is_water_mask) then
            call mprintf(.true.,STDOUT,'  Calculating landmask from %s ( WATER =', &
                         newline=.false.,s1=trim(landmask_name))
         else
            call mprintf(.true.,STDOUT,'  Calculating landmask from %s ( LAND =', &
                         newline=.false.,s1=trim(landmask_name))
         end if
         do k = 1, num_landmask_categories
            call mprintf(.true.,STDOUT,' %i',newline=.false.,i1=landmask_value(k))
            if (k == num_landmask_categories) call mprintf(.true.,STDOUT,')')
         end do
     
         ! Calculate landmask
         if (is_water_mask) then
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  water_total = -1.
                  do k=1,num_landmask_categories
                     if (landmask_value(k) >= min_category .and. landmask_value(k) <= max_category) then 
                        if (field(i,j,landmask_value(k)) /= msg_fill_val) then
                           if (water_total < 0.) water_total = 0.
                           water_total = water_total + field(i,j,landmask_value(k)) 
                        else
                           water_total = -1.
                           exit
                        end if
                     end if
                  end do
                  if (water_total >= 0.0) then
                     if (water_total < 0.50) then
                        landmask(i,j) = 1.
                     else
                        landmask(i,j) = 0.
                     end if
                  else
                     landmask(i,j) = -1.
                  end if
               end do
            end do
         else
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  land_total = -1.
                  do k=1,num_landmask_categories
                     if (landmask_value(k) >= min_category .and. landmask_value(k) <= max_category) then 
                        if (field(i,j,landmask_value(k)) /= msg_fill_val) then
                           if (land_total < 0.) land_total = 0.
                           land_total = land_total + field(i,j,landmask_value(k)) 
                        else
                           land_total = -1.
                           exit
                        end if
                     end if
                  end do
                  if (land_total >= 0.0) then
                     if (land_total > 0.50) then
                        landmask(i,j) = 1.
                     else
                        landmask(i,j) = 0.
                     end if
                  else
                     landmask(i,j) = -1.
                  end if
               end do
            end do
         end if
    
         call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, 'LANDMASK', &
                          datestr, landmask)
    

                  call get_domain_resolution(dom_dx, dom_dy)

 
         ! If we should only save the dominant category, then no need to write out fractional field
         if (.not.only_save_dominant .or. (idomcatstatus /= 0)) then
     
            ! Finally, we may be asked to smooth the fractional field
            call get_smooth_option(landmask_name, smth_opt, smth_passes, istatus)
            if (istatus == 0) then

               if (grid_type == 'C') then
                  if (smth_opt == ONETWOONE) then
                     call one_two_one(field,                      &
                                      start_patch_i, end_patch_i, &
                                      start_patch_j, end_patch_j, &
                                      start_mem_i, end_mem_i,     &
                                      start_mem_j, end_mem_j,     &
                                      min_category, max_category, &
                                      smth_passes, msg_fill_val,dom_dx,dom_dy)
                  else if (smth_opt == SMTHDESMTH) then
                     call smth_desmth(field,                      &
                                      start_patch_i, end_patch_i, &
                                      start_patch_j, end_patch_j, &
                                      start_mem_i, end_mem_i,     &
                                      start_mem_j, end_mem_j,     &
                                      min_category, max_category, &
                                      smth_passes, msg_fill_val)
                  else if (smth_opt == SMTHDESMTH_SPECIAL) then
                     call smth_desmth_special(field,              &
                                      start_patch_i, end_patch_i, &
                                      start_patch_j, end_patch_j, &
                                      start_mem_i, end_mem_i,     &
                                      start_mem_j, end_mem_j,     &
                                      min_category, max_category, &
                                      smth_passes, msg_fill_val)
                  end if
               else if (grid_type == 'E') then
                  if (smth_opt == ONETWOONE) then
                     call one_two_one_egrid(field,                &
                                      start_patch_i, end_patch_i, &
                                      start_patch_j, end_patch_j, &
                                      start_mem_i, end_mem_i,     &
                                      start_mem_j, end_mem_j,     &
                                      min_category, max_category, &
                                      smth_passes, msg_fill_val, 1.0)
                  else if (smth_opt == SMTHDESMTH) then
                     call smth_desmth_egrid(field,                &
                                      start_patch_i, end_patch_i, &
                                      start_patch_j, end_patch_j, &
                                      start_mem_i, end_mem_i,     &
                                      start_mem_j, end_mem_j,     &
                                      min_category, max_category, &
                                      smth_passes, msg_fill_val, 1.0)
                  else if (smth_opt == SMTHDESMTH_SPECIAL) then
                     call mprintf(.true.,WARN,'smth-desmth_special is not currently implemented for NMM. '// &
                                              'No smoothing will be done.')
                  end if
               end if

            end if
      
            call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, &
                             min_category, max_category, trim(landmask_name), &
                             datestr, real_array=field)
         end if
     
         if (idomcatstatus == 0) then
            allocate(dominant_field(start_mem_i:end_mem_i, start_mem_j:end_mem_j))
     
            if (.not. only_save_dominant) then
               field_count = field_count + 1
               call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                            i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=domname)
            end if

            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  if ((landmask(i,j) == 1. .and. is_water_mask) .or. &
                      (landmask(i,j) == 0. .and. .not.is_water_mask)) then
                     dominant = 0.
                     dominant_field(i,j) = real(min_category-1)
                     do k=min_category,max_category
                        do kk=1,num_landmask_categories
                           if (k == landmask_value(kk)) exit 
                        end do
                        if (field(i,j,k) > dominant .and. kk > num_landmask_categories) then
                           dominant_field(i,j) = real(k)
                           dominant = field(i,j,k)
                        end if
                     end do
                  else
                     dominant = 0.
                     dominant_field(i,j) = real(min_category-1)
                     do k=min_category,max_category
                        do kk=1,num_landmask_categories
                           if (field(i,j,k) > dominant .and. k == landmask_value(kk)) then
                              dominant_field(i,j) = real(k)
                              dominant = field(i,j,k)
                           end if
                        end do
                     end do
                  end if
               end do
            end do
     
            call write_field(start_mem_i, end_mem_i, start_mem_j, end_mem_j, 1, 1, trim(domname), &
                             datestr, dominant_field)
          
            deallocate(dominant_field)
         end if
     
         deallocate(field)
      end if
   
      !
      ! Now process all other fields specified by the user
      !
      call reset_next_field()
      ifieldstatus = 0
      do while (ifieldstatus == 0) 
         call get_next_fieldname(fieldname, ifieldstatus)
     
         ! There is another field in the GEOGRID.TBL file 
         if (ifieldstatus == 0) then
            temp_string(1:128) = fieldname

            call get_source_opt_status(fieldname, 0, opt_status)
      
            ! If this field is still to be processed
            if (.not. hash_search(processed_fieldnames, temp_string) .and. opt_status == 0) then
     
               call hash_insert(processed_fieldnames, temp_string)
               call mprintf(.true.,STDOUT,'  Processing %s', s1=trim(fieldname))
       
               call get_output_stagger(fieldname, istagger, istatus)
               dimnames(:) = 'null'
               call get_subgrid_dim_name(which_domain, fieldname, dimnames, &
                                         sub_x, sub_y, istatus)
       
               if (istagger == M .or. (sub_x > 1) .or. (sub_y > 1)) then
                  sm1 = start_mem_i
                  em1 = end_mem_i
                  sm2 = start_mem_j
                  em2 = end_mem_j
                  xlat_ptr => xlat_array 
                  xlon_ptr => xlon_array 
                  mapfac_ptr_x => mapfac_array_m_x
                  mapfac_ptr_y => mapfac_array_m_y
               else if (istagger == U) then ! In the case that extra_cols = .false.
                  sm1 = start_mem_i          ! we should have that end_mem_stag is
                  em1 = end_mem_stag_i       ! the same as end_mem, so we do not need
                  sm2 = start_mem_j          ! to check extra_cols or extra rows here
                  em2 = end_mem_j
                  xlat_ptr => xlat_array_u 
                  xlon_ptr => xlon_array_u 
                  mapfac_ptr_x => mapfac_array_u_x
                  mapfac_ptr_y => mapfac_array_u_y
               else if (istagger == V) then
                  sm1 = start_mem_i
                  em1 = end_mem_i
                  sm2 = start_mem_j
                  em2 = end_mem_stag_j
                  xlat_ptr => xlat_array_v 
                  xlon_ptr => xlon_array_v 
                  mapfac_ptr_x => mapfac_array_v_x
                  mapfac_ptr_y => mapfac_array_v_y
               else if (istagger == HH) then   ! E grid
                  sm1 = start_mem_i
                  em1 = end_mem_i
                  sm2 = start_mem_j
                  em2 = end_mem_j
                  xlat_ptr => xlat_array 
                  xlon_ptr => xlon_array 
                  mapfac_ptr_x => mapfac_array_m_x
                  mapfac_ptr_y => mapfac_array_m_y
               else if (istagger == VV) then   ! E grid 
                  sm1 = start_mem_i
                  em1 = end_mem_i
                  sm2 = start_mem_j
                  em2 = end_mem_stag_j
                  xlat_ptr => xlat_array_v 
                  xlon_ptr => xlon_array_v 
                  mapfac_ptr_x => mapfac_array_v_x
                  mapfac_ptr_y => mapfac_array_v_y
               end if

               if (sub_x > 1) then
                  sm1 = (start_mem_i - 1)*sub_x + 1
                  if (extra_col) then
                     em1 = (end_mem_i + 1)*sub_x
                  else
                     em1 = (end_mem_i    )*sub_x
                  end if
               end if
               if (sub_y > 1)then
                  sm2 = (start_mem_j - 1)*sub_y + 1
                  if (extra_row) then
                     em2 = (end_mem_j + 1)*sub_y
                  else
                     em2 = (end_mem_j    )*sub_y
                  end if
               end if

!BUG: This should probably be moved up to where other lat/lon fields are calculated, and we should
!     just determine whether we will have any subgrids or not at that point
               if ((sub_x > 1) .or. (sub_y > 1)) then
!                  if (associated(xlat_array_subgrid))     deallocate(xlat_array_subgrid)
!                  if (associated(xlon_array_subgrid))     deallocate(xlon_array_subgrid)
!                  if (associated(mapfac_array_x_subgrid)) deallocate(mapfac_array_x_subgrid)
!                  if (associated(mapfac_array_y_subgrid)) deallocate(mapfac_array_y_subgrid)
                  allocate(xlat_array_subgrid(sm1:em1,sm2:em2))
                  allocate(xlon_array_subgrid(sm1:em1,sm2:em2))
                  allocate(mapfac_array_x_subgrid(sm1:em1,sm2:em2))
                  allocate(mapfac_array_y_subgrid(sm1:em1,sm2:em2))
                  call get_lat_lon_fields(xlat_array_subgrid, xlon_array_subgrid, &
                                   sm1, sm2, em1, em2, M, sub_x=sub_x, sub_y=sub_y)
                  xlat_ptr => xlat_array_subgrid
                  xlon_ptr => xlon_array_subgrid
                  call get_map_factor(xlat_ptr, xlon_ptr, mapfac_array_x_subgrid, &
                                      mapfac_array_y_subgrid, sm1, sm2, em1, em2)
                  mapfac_ptr_x => mapfac_array_x_subgrid
                  mapfac_ptr_y => mapfac_array_y_subgrid
               end if
       
               call get_missing_fill_value(fieldname, msg_fill_val, istatus)
               if (istatus /= 0) msg_fill_val = NAN 
       
               call get_halt_on_missing(fieldname, halt_on_missing, istatus)
               if (istatus /= 0) halt_on_missing = .false.
       
               ! Destination field type is CONTINUOUS
               if (iget_fieldtype(fieldname,istatus) == CONTINUOUS) then
                  call get_max_levels(fieldname, min_level, max_level, istatus)
                  allocate(field(sm1:em1, sm2:em2, min_level:max_level))

                  field_count = field_count + 1
                  call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                               i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=fieldname)

                  if ((sub_x > 1) .or. (sub_y > 1)) then
                     call calc_field(fieldname, field, xlat_ptr, xlon_ptr, istagger, &
                                sm1, em1, sm2, em2, min_level, max_level, &
                                processed_domain, 1, sr_x=sub_x, sr_y=sub_y)
                  else
                     call calc_field(fieldname, field, xlat_ptr, xlon_ptr, istagger, &
                                sm1, em1, sm2, em2, min_level, max_level, &
                                processed_domain, 1, landmask=landmask, sr_x=sub_x, sr_y=sub_y)
                  end if
        
                  ! If user wants to halt when a missing value is found in output field, check now
                  if (halt_on_missing) then
                     do i=sm1, em1
                        do j=sm2, em2
                           ! Only need to examine k=1
                           if (field(i,j,1) == msg_fill_val) then
                              call mprintf(.true.,ERROR,' Missing value encountered in output field. Quitting.')
                           end if
                        end do
                     end do
                  end if
                          call get_domain_resolution(dom_dx, dom_dy)

                  ! We may be asked to smooth the fractional field
                  call get_smooth_option(fieldname, smth_opt, smth_passes, istatus)
                  if (istatus == 0) then

                     if (grid_type == 'C') then
                        if (smth_opt == ONETWOONE) then
                           call one_two_one(field,                      &
                                            start_patch_i, end_patch_i, &
                                            start_patch_j, end_patch_j, &
                                            sm1,         em1,           &
                                            sm2,         em2,           &
                                            min_level, max_level,       &
                                            smth_passes, msg_fill_val,dom_dx,dom_dy)
                        else if (smth_opt == SMTHDESMTH) then
                           call smth_desmth(field,                      &
                                            start_patch_i, end_patch_i, &
                                            start_patch_j, end_patch_j, &
                                            sm1,         em1,           &
                                            sm2,         em2,           &
                                            min_level, max_level,       &
                                            smth_passes, msg_fill_val)
                        else if (smth_opt == SMTHDESMTH_SPECIAL) then
                           call smth_desmth_special(field,              &
                                            start_patch_i, end_patch_i, &
                                            start_patch_j, end_patch_j, &
                                            sm1,         em1,           &
                                            sm2,         em2,           &
                                            min_level, max_level,       &
                                            smth_passes, msg_fill_val)
                       end if
  
                     else if (grid_type == 'E') then
 
                        if (trim(fieldname) == 'HGT_M' ) then
                           topo_flag_val=1.0
                           mass_flag=1.0
                        else if (trim(fieldname) == 'HGT_V') then
                           topo_flag_val=1.0
                           mass_flag=0.0
                        else
                           topo_flag_val=0.0
                        end if
  
                        if (smth_opt == ONETWOONE) then
                           call one_two_one_egrid(field,                &
                                            start_patch_i, end_patch_i, &
                                            start_patch_j, end_patch_j, &
                                            sm1,         em1,           &
                                            sm2,         em2,           &
                                            min_level, max_level,       &
                                            smth_passes, topo_flag_val, mass_flag)
                        else if (smth_opt == SMTHDESMTH) then
                           call smth_desmth_egrid(field,                &
                                            start_patch_i, end_patch_i, &
                                            start_patch_j, end_patch_j, &
                                            sm1,         em1,           &
                                            sm2,         em2,           &
                                            min_level, max_level,       &
                                            smth_passes, topo_flag_val, mass_flag)
                        else if (smth_opt == SMTHDESMTH_SPECIAL) then
                           call mprintf(.true.,WARN,'smth-desmth_special is not currently implemented for NMM. '// &
                                                    'No smoothing will be done.')
                        end if
  
                     end if

                  end if

                  call write_field(sm1, em1, sm2, em2, &
                                   min_level, max_level, trim(fieldname), datestr, real_array=field)
        
                  ! Do we calculate directional derivatives from this field?
                  call get_dfdx_name(fieldname, gradname, istatus)
                  if (istatus == 0) then
                     allocate(slp_field(sm1:em1,sm2:em2,min_level:max_level))

                     field_count = field_count + 1
                     call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                                  i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=gradname)

                     if (grid_type == 'C') then
                        call calc_dfdx(field, slp_field, sm1, sm2, min_level, em1, em2, max_level, mapfac_ptr_x)
                     else if (grid_type == 'E') then
                        call calc_dfdx(field, slp_field, sm1, sm2, min_level, em1, em2, max_level)
                     end if
                     call write_field(sm1, em1, sm2, em2, &
                                      min_level, max_level, trim(gradname), datestr, real_array=slp_field)
                     deallocate(slp_field)
                  end if
                  call get_dfdy_name(fieldname, gradname, istatus)
                  if (istatus == 0) then
                     allocate(slp_field(sm1:em1,sm2:em2,min_level:max_level))

                     field_count = field_count + 1
                     call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                                  i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=gradname)

                     if (grid_type == 'C') then
                        call calc_dfdy(field, slp_field, sm1, sm2, min_level, em1, em2, max_level, mapfac_ptr_y)
                     else if (grid_type == 'E') then
                        call calc_dfdy(field, slp_field, sm1, sm2, min_level, em1, em2, max_level)
                     end if
                     call write_field(sm1, em1, sm2, em2, &
                                      min_level, max_level, trim(gradname), datestr, real_array=slp_field)
                     deallocate(slp_field)
                  end if
        
                  deallocate(field)
      
               ! Destination field type is CATEGORICAL
               else
                  call get_max_categories(fieldname, min_category, max_category, istatus)
                  allocate(field(sm1:em1, sm2:em2, min_category:max_category))

                  ! Do we calculate a dominant category for this field?
                  call get_domcategory_name(fieldname, domname, only_save_dominant, idomcatstatus)
        
                  if (.not. only_save_dominant) then
                     field_count = field_count + 1
                     call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                                  i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=fieldname)
                  else
                     field_count = field_count + 1
                     call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                                  i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=domname)
                  end if

                  if ((sub_x > 1) .or. (sub_y > 1)) then
                     call calc_field(fieldname, field, xlat_ptr, xlon_ptr, istagger, &
                                     sm1, em1, sm2, em2, min_category, max_category, &
                                     processed_domain, 1, sr_x=sub_x, sr_y=sub_y)
                  else
                     call calc_field(fieldname, field, xlat_ptr, xlon_ptr, istagger, &
                                     sm1, em1, sm2, em2, min_category, max_category, &
                                     processed_domain, 1, landmask=landmask, sr_x=sub_x, sr_y=sub_y)
                  end if
        
                  ! If user wants to halt when a missing value is found in output field, check now
                  if (halt_on_missing) then
                     do i=sm1, em1
                        do j=sm2, em2
                           ! Only need to examine k=1
                           if (field(i,j,1) == msg_fill_val) then
                              call mprintf(.true.,ERROR,' Missing value encountered in output field. Quitting.')
                           end if
                        end do
                     end do
                  end if
       
                  ! Find fractions
                  do i=sm1, em1
                     do j=sm2, em2
                        sum = 0.
                        do k=min_category,max_category
                           sum = sum + field(i,j,k)
                        end do
                        if (sum > 0.0) then
                           do k=min_category,max_category
                              field(i,j,k) = field(i,j,k) / sum
                           end do
                        else
                           do k=min_category,max_category
                              field(i,j,k) = msg_fill_val
                           end do
                        end if
                     end do
                  end do
                          call get_domain_resolution(dom_dx, dom_dy)

                  ! If we should only save the dominant category, then no need to write out fractional field
                  if (.not.only_save_dominant .or. (idomcatstatus /= 0)) then
        
                     ! Finally, we may be asked to smooth the fractional field
                     call get_smooth_option(fieldname, smth_opt, smth_passes, istatus)
                     if (istatus == 0) then
                        if (grid_type == 'C') then
                           if (smth_opt == ONETWOONE) then
                              call one_two_one(field,                    &
                                             start_patch_i, end_patch_i, &
                                             start_patch_j, end_patch_j, &
                                             sm1,         em1,           &
                                             sm2,         em2,           &
                                             min_category, max_category, &
                                             smth_passes, msg_fill_val,dom_dx,dom_dy)
                           else if (smth_opt == SMTHDESMTH) then
                              call smth_desmth(field,                    &
                                             start_patch_i, end_patch_i, &
                                             start_patch_j, end_patch_j, &
                                             sm1,         em1,           &
                                             sm2,         em2,           &
                                             min_category, max_category, &
                                             smth_passes, msg_fill_val)
                           else if (smth_opt == SMTHDESMTH_SPECIAL) then
                              call smth_desmth_special(field,            &
                                             start_patch_i, end_patch_i, &
                                             start_patch_j, end_patch_j, &
                                             sm1,         em1,           &
                                             sm2,         em2,           &
                                             min_category, max_category, &
                                             smth_passes, msg_fill_val)
                           end if
                        else if (grid_type == 'E') then
                           if (smth_opt == ONETWOONE) then
                              call one_two_one_egrid(field,              &
                                             start_patch_i, end_patch_i, &
                                             start_patch_j, end_patch_j, &
                                             sm1,         em1,           &
                                             sm2,         em2,           &
                                             min_category, max_category, &
                                             smth_passes, msg_fill_val, 1.0)
                           else if (smth_opt == SMTHDESMTH) then
                              call smth_desmth_egrid(field,              &
                                             start_patch_i, end_patch_i, &
                                             start_patch_j, end_patch_j, &
                                             sm1,         em1,           &
                                             sm2,         em2,           &
                                             min_category, max_category, &
                                             smth_passes, msg_fill_val, 1.0)
                           else if (smth_opt == SMTHDESMTH_SPECIAL) then
                              call mprintf(.true.,WARN,'smth-desmth_special is not currently implemented for NMM. '// &
                                                       'No smoothing will be done.')
                           end if
                        end if
                     end if
         
                     call write_field(sm1, em1, sm2, em2, &
                                      min_category, max_category, trim(fieldname), datestr, real_array=field)
                  end if
        
                  if (idomcatstatus == 0) then
                     call mprintf(.true.,STDOUT,'  Processing %s', s1=trim(domname))
                     allocate(dominant_field(sm1:em1, sm2:em2))
 
                     if (.not. only_save_dominant) then
                        field_count = field_count + 1
                        call mprintf(.true.,LOGFILE,'Processing field %i of %i (%s)', &
                                     i1=field_count,i2=NUM_FIELDS-NUM_AUTOMATIC_FIELDS,s1=domname)
                     end if

                     do i=sm1, em1
                        do j=sm2, em2
                           dominant = 0.
                           dominant_field(i,j) = real(min_category-1)
                           do k=min_category,max_category
                              if (field(i,j,k) > dominant .and. field(i,j,k) /= msg_fill_val) then 
                                 dominant_field(i,j) = real(k)
                                 dominant = field(i,j,k)
             !                  else
             !                    dominant_field(i,j) = nint(msg_fill_val)
! Maybe we should put an else clause here to set the category equal to the missing fill value?
! BUG: The problem here seems to be that, when we set a fraction equal to the missing fill value
!   above, if the last fractional index we process here has been filled, we think that the dominant
!   category should be set to the missing fill value. Perhaps we could do some check to only
!   assign the msg_fill_val if no other valid category has been assigned? But this may still not
!   work if the missing fill value is something like 0.5. Somehow use bitarrays, perhaps, to remember
!   which points are missing and which just happen to have the missing fill value?
                              end if
                           end do
                           if (dominant_field(i,j) == real(min_category-1)) dominant_field(i,j) = msg_fill_val
                        end do
                     end do
                     call write_field(sm1, em1, sm2, em2, 1, 1, &
                                      trim(domname), datestr, dominant_field)
                     deallocate(dominant_field)
                  end if
       
                  deallocate(field)

                  if ((sub_x > 1) .or. (sub_y > 1)) then
                     if (associated(xlat_array_subgrid))     deallocate(xlat_array_subgrid)
                     if (associated(xlon_array_subgrid))     deallocate(xlon_array_subgrid)
                     if (associated(mapfac_array_x_subgrid)) deallocate(mapfac_array_x_subgrid)
                     if (associated(mapfac_array_y_subgrid)) deallocate(mapfac_array_y_subgrid)
                  end if
   
               end if
      
            end if
         end if
    
      end do

      ! Close output
      call output_close()
    
      call hash_destroy(processed_fieldnames)
    
      ! Free up memory
      if (associated(xlat_array)) deallocate(xlat_array)
      if (associated(xlon_array)) deallocate(xlon_array)
      if (grid_type == 'C') then
         if (associated(xlat_array_u)) deallocate(xlat_array_u)
         if (associated(xlon_array_u)) deallocate(xlon_array_u)
         if (associated(xlat_array_corner)) deallocate(xlat_array_corner)
         if (associated(xlon_array_corner)) deallocate(xlon_array_corner)
         if (associated(mapfac_array_u_x)) deallocate(mapfac_array_u_x)
         if (associated(mapfac_array_u_y)) deallocate(mapfac_array_u_y)
      end if
      if (associated(xlat_array_v)) deallocate(xlat_array_v)
      if (associated(xlon_array_v)) deallocate(xlon_array_v)
      if (associated(mapfac_array_m_x)) deallocate(mapfac_array_m_x)
      if (associated(mapfac_array_m_y)) deallocate(mapfac_array_m_y)
      if (associated(mapfac_array_v_x)) deallocate(mapfac_array_v_x)
      if (associated(mapfac_array_v_y)) deallocate(mapfac_array_v_y)
      if (associated(landmask)) deallocate(landmask)
      if (associated(xlat_array_subgrid))     deallocate(xlat_array_subgrid)
      if (associated(xlon_array_subgrid))     deallocate(xlon_array_subgrid)
      if (associated(mapfac_array_x_subgrid)) deallocate(mapfac_array_x_subgrid)
      if (associated(mapfac_array_y_subgrid)) deallocate(mapfac_array_y_subgrid)

      nullify(xlat_ptr)
      nullify(xlon_ptr)
   
   end subroutine process_tile
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: calc_field
   !
   ! Purpose: This routine fills in the "field" array with interpolated source 
   !   data. When multiple resolutions of source data are available, an appropriate
   !   resolution is chosen automatically. The specified field may either be a 
   !   continuous field or a categorical field.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive subroutine calc_field(fieldname, field, xlat_array, xlon_array, istagger, &
                                   start_i, end_i, start_j, end_j, start_k, end_k, &
                                   processed_domain, ilevel, landmask, sr_x, sr_y)
   
      use bitarray_module
      use interp_module
      use llxy_module
      use misc_definitions_module
      use proc_point_module
      use queue_module
      use source_data_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_i, end_i, start_j, end_j, start_k, end_k, ilevel, istagger
      real, dimension(start_i:end_i, start_j:end_j), intent(in) :: xlat_array, xlon_array
      real, dimension(start_i:end_i, start_j:end_j, start_k:end_k), intent(inout) :: field
      real, dimension(start_i:end_i, start_j:end_j), intent(in), optional :: landmask
      integer, intent(in), optional :: sr_x, sr_y
      character (len=128), intent(in) :: fieldname
      type (bitarray), intent(inout) :: processed_domain
    
      ! Local variables
      integer :: start_src_k, end_src_k
      integer :: i, j, k, ix, iy, itype
      integer :: user_iproj, istatus
      integer :: opt_status
      real :: mask_val
      real :: temp
      real :: scale_factor
      real :: msg_val, msg_fill_val, threshold, src_dx, src_dy, dom_dx, dom_dy
      real :: user_stand_lon, user_truelat1, user_truelat2, user_dxkm, user_dykm, &
              user_known_x, user_known_y, user_known_lat, user_known_lon
      real, pointer, dimension(:,:,:) :: data_count
      integer, pointer, dimension(:) :: interp_type
      integer, pointer, dimension(:) :: interp_opts
      character (len=128) :: interp_string
      type (bitarray) :: bit_domain, level_domain
      type (queue)    :: point_queue, tile_queue
      type (q_data)   :: current_pt

      nullify(data_count)
      nullify(interp_type)
      nullify(interp_opts)

      ! If this is the first trip through this routine, we need to allocate the bit array that
      !  will persist through all recursive calls, tracking which grid points have been assigned
      !  a value. 
      if (ilevel == 1) call bitarray_create(processed_domain, end_i-start_i+1, end_j-start_j+1)

      ! Find out if this "priority level" (given by ilevel) exists
      call check_priority_level(fieldname, ilevel, istatus)
    
      ! A bad status indicates that that no data for priority level ilevel is available, and thus, that
      !   no further levels will be specified. We are done processing for this level.
      if (istatus /= 0) then
         if (ilevel == 1) call bitarray_destroy(processed_domain)
         return 
      end if
    
      ! Before proceeding with processing for this level, though, process for the next highest priority level
      !   of source data
      call calc_field(fieldname, field, xlat_array, xlon_array, istagger, start_i, end_i, &
                      start_j, end_j, start_k, end_k, processed_domain, ilevel+1, landmask, sr_x, sr_y)

      ! At this point, all levels of source data with higher priority have been processed, and we can assign
      !   values to all grid points that have not already been given values from higher-priority data

      call get_source_opt_status(fieldname, ilevel, opt_status)
      if (opt_status == 0) then

         ! Find out the projection of the data for this "priority level" (given by ilevel)
         call get_data_projection(fieldname, user_iproj, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_known_x, user_known_y, user_known_lat, &
                                  user_known_lon, ilevel, istatus)
       
         ! A good status indicates that there is data for this priority level, so we store the projection
         !   of that data on a stack. The projection will be on the top of the stack (and hence will be 
         !   the "active" projection) once all higher-priority levels have been processed
         call push_source_projection(user_iproj, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_dykm, user_dxkm, user_known_x, user_known_y, &
                                  user_known_lat, user_known_lon)
       
         ! Initialize point processing module
         call proc_point_init()
       
         ! Initialize queues
         call q_init(point_queue)
         call q_init(tile_queue)
       
         ! Determine whether we will be processing categorical data or continuous data
         itype = iget_source_fieldtype(fieldname, ilevel, istatus)
         call get_interp_option(fieldname, ilevel, interp_string, istatus)
         interp_type => interp_array_from_string(interp_string)
         interp_opts => interp_options_from_string(interp_string)
   
         ! Also, check whether we will be using the cell averaging interpolator for continuous fields
         if (index(interp_string,'average_gcell') /= 0 .and. itype == CONTINUOUS) then
            call get_gcell_threshold(interp_string, threshold, istatus)
            if (istatus == 0) then
               call get_source_resolution(fieldname, ilevel, src_dx, src_dy, istatus)
               if (istatus == 0) then
                  call get_domain_resolution(dom_dx, dom_dy)
                  if (gridtype == 'C') then
                     if (threshold*max(src_dx,src_dy)*111. <= max(dom_dx,dom_dy)/1000.) then
                        itype = SP_CONTINUOUS
                        allocate(data_count(start_i:end_i,start_j:end_j,start_k:end_k))
                        data_count = 0.
                     end if
                  else if (gridtype == 'E') then
                     if (max(src_dx,src_dy) >= threshold*max(dom_dx,dom_dy)) then
                        itype = SP_CONTINUOUS
                        allocate(data_count(start_i:end_i,start_j:end_j,start_k:end_k))
                        data_count = 0.
                     end if
                  end if
               end if
            end if
         end if
   
         call get_missing_value(fieldname, ilevel, msg_val, istatus)
         if (istatus /= 0) msg_val = NAN
         call get_missing_fill_value(fieldname, msg_fill_val, istatus)
         if (istatus /= 0) msg_fill_val = NAN
       
         call get_masked_value(fieldname, ilevel, mask_val, istatus)
         if (istatus /= 0) mask_val = -1.
       
         if (itype == CONTINUOUS .or. itype == SP_CONTINUOUS) then
            call get_source_levels(fieldname, ilevel, start_src_k, end_src_k, istatus)
            if (istatus /= 0) return
         end if
       
         ! Initialize bitarray used to track which points have been visited and assigned values while 
         !   processing *this* priority level of data
         call bitarray_create(bit_domain, end_i-start_i+1, end_j-start_j+1)
         call bitarray_create(level_domain, end_i-start_i+1, end_j-start_j+1)
       
         ! Begin by placing a point in the tile_queue
         current_pt%lat = xlat_array(start_i,start_j)
         current_pt%lon = xlon_array(start_i,start_j)
         current_pt%x = start_i
         current_pt%y = start_j
         call q_insert(tile_queue, current_pt)
       
         ! While there are still grid points in tiles that have not yet been processed
         do while (q_isdata(tile_queue))
       
            ! Take a point from the outer queue and place it in the point_queue for processing 
            current_pt = q_remove(tile_queue)
        
            ! If this level of data is categorical (i.e., is given as an array of category indices),
            !   then first try to process the entire tile in one call to accum_categorical. Any grid
            !   points that are not given values by accum_categorical and that lie within the current
            !   tile of source data are individually assigned values in the inner loop
            if (itype == CATEGORICAL) then
        
               ! Have we already visited this point? If so, this tile has already been processed by 
               !   accum_categorical. 
               if (.not. bitarray_test(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)) then
                  call q_insert(point_queue, current_pt) 
                  if (.not. have_processed_tile(current_pt%lat, current_pt%lon, fieldname, ilevel)) then
                     call accum_categorical(current_pt%lat, current_pt%lon, istagger, field, &
                                            start_i, end_i, start_j, end_j, start_k, end_k, &
                                            fieldname, processed_domain, level_domain, &
                                            ilevel, msg_val, mask_val, sr_x, sr_y)
! BUG: Where do we mask out those points that are on land/water when masked=land/water is set? 
                  end if
                  call bitarray_set(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)
               end if
        
            else if (itype == SP_CONTINUOUS) then
   
               ! Have we already visited this point? If so, this tile has already been processed by 
               !   accum_continuous. 
               if (.not. bitarray_test(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)) then
                  call q_insert(point_queue, current_pt) 
                  if (.not. have_processed_tile(current_pt%lat, current_pt%lon, fieldname, ilevel)) then
                     call accum_continuous(current_pt%lat, current_pt%lon, istagger, field, data_count, &
                                            start_i, end_i, start_j, end_j, start_k, end_k, &
                                            fieldname, processed_domain, level_domain, &
                                            ilevel, msg_val, mask_val, sr_x, sr_y)
! BUG: Where do we mask out those points that are on land/water when masked=land/water is set? 
                  end if
                  call bitarray_set(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)
               end if
   
            else if (itype == CONTINUOUS) then
        
               ! Have we already visited this point? If so, the tile containing this point has already been
               !   processed in the inner loop.
               if (.not. bitarray_test(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)) then
                  call q_insert(point_queue, current_pt) 
                  call bitarray_set(bit_domain, current_pt%x-start_i+1, current_pt%y-start_j+1)
               end if
        
            end if
        
            ! This inner loop, where all grid points contained in the current source tile are processed
            do while (q_isdata(point_queue))
               current_pt = q_remove(point_queue)
               ix = current_pt%x
               iy = current_pt%y
         
               ! Process the current point
               if (itype == CONTINUOUS .or. itype == SP_CONTINUOUS) then
         
                  ! Have we already assigned this point a value from this priority level?
                  if (.not. bitarray_test(level_domain, ix-start_i+1, iy-start_j+1)) then
           
                     ! If the point was already assigned a value from a higher-priority level, no 
                     !   need to assign a new value
                     if (bitarray_test(processed_domain, ix-start_i+1, iy-start_j+1)) then
                        call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
            
                     ! Otherwise, need to assign values from this level of source data if we can
                     else
                        if (present(landmask) .and. (istagger == M .or. istagger == HH)) then
                           if (landmask(ix,iy) /= mask_val) then
                              do k=start_src_k,end_src_k
                                 temp = get_point(current_pt%lat, current_pt%lon, k, &
                                                  fieldname, ilevel, interp_type, interp_opts, msg_val)
                                 if (temp /= msg_val) then
                                    field(ix, iy, k) = temp
                                    call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
                                    if (itype == SP_CONTINUOUS) data_count(ix, iy, k) = 1.0
                                 else
                                    field(ix, iy, k) = msg_fill_val
                                 end if
                              end do
                           else
                              do k=start_k,end_k
                                 field(ix,iy,k) = msg_fill_val
                              end do
                           end if
                        else
                           do k=start_src_k,end_src_k
                              temp = get_point(current_pt%lat, current_pt%lon, k, &
                                               fieldname, ilevel, interp_type, interp_opts, msg_val)
                              if (temp /= msg_val) then
                                 field(ix, iy, k) = temp
                                 call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
                                 if (itype == SP_CONTINUOUS) data_count(ix, iy, k) = 1.0
                              else
                                 field(ix, iy, k) = msg_fill_val
                              end if
                           end do
                        end if
                     end if
                  end if
         
               else if (itype == CATEGORICAL) then
         
                  ! Have we already assigned this point a value from this priority level?
                  if (.not.bitarray_test(level_domain, ix-start_i+1, iy-start_j+1)) then
          
                     ! If the point was already assigned a value from a higher-priority level, no 
                     !   need to assign a new value
                     if (bitarray_test(processed_domain, ix-start_i+1, iy-start_j+1)) then
                        call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
            
                     ! Otherwise, the point was apparently not given a value when accum_categorical
                     !   was called for the current tile, and we need to assign values from this 
                     !   level of source data if we can
                     else
                        if (present(landmask) .and. (istagger == M .or. istagger == HH)) then
                           if (landmask(ix,iy) /= mask_val) then
                              temp = get_point(current_pt%lat, current_pt%lon, 1, &
                                               fieldname, ilevel, interp_type, interp_opts, msg_val)
         
                              do k=start_k,end_k
                                 field(ix,iy,k) = 0.
                              end do
         
                              if (temp /= msg_val) then
                                 if (int(temp) >= start_k .and. int(temp) <= end_k) then
                                    field(ix, iy, int(temp)) = field(ix, iy, int(temp)) + 1.
                                 else
                                    call mprintf(.true.,WARN,' Attempted to assign an invalid category '// &
                                                 '%i to grid point (%i, %i)', i1=int(temp), i2=ix, i3=iy)
                                 end if
                                 call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
                              end if
      
                           else
                              do k=start_k,end_k
                                 field(ix,iy,k) = 0.
                              end do
                           end if
                        else
                           temp = get_point(current_pt%lat, current_pt%lon, 1, &
                                            fieldname, ilevel, interp_type, interp_opts, msg_val)
         
                           do k=start_k,end_k
                              field(ix,iy,k) = 0.
                           end do
         
                           if (temp /= msg_val) then
                              if (int(temp) >= start_k .and. int(temp) <= end_k) then
                                 field(ix, iy, int(temp)) = field(ix, iy, int(temp)) + 1.
                              else
                                 call mprintf(.true.,WARN,' Attempted to assign an invalid category '// &
                                              '%i to grid point (%i, %i)', i1=int(temp), i2=ix, i3=iy)
                              end if
                              call bitarray_set(level_domain, ix-start_i+1, iy-start_j+1)
                           end if
                        end if
                     end if
                  end if
         
               end if
         
               ! Scan neighboring points, adding them to the appropriate queue based on whether they
               !   are in the current tile or not
               if (iy > start_j) then
                  if (ix > start_i) then
           
                     ! Neighbor with relative position (-1,-1)
                     call process_neighbor(ix-1, iy-1, bit_domain, point_queue, tile_queue, &
                                           xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
                  end if
          
                  ! Neighbor with relative position (0,-1) 
                  call process_neighbor(ix, iy-1, bit_domain, point_queue, tile_queue, &
                                        xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
          
                  if (ix < end_i) then
          
                     ! Neighbor with relative position (+1,-1)
                     call process_neighbor(ix+1, iy-1, bit_domain, point_queue, tile_queue, &
                                           xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
                  end if
               end if
         
               if (ix > start_i) then
         
                  ! Neighbor with relative position (-1,0)
                  call process_neighbor(ix-1, iy, bit_domain, point_queue, tile_queue, &
                                        xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
               end if
         
               if (ix < end_i) then
         
                  ! Neighbor with relative position (+1,0)
                  call process_neighbor(ix+1, iy, bit_domain, point_queue, tile_queue, &
                                        xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
               end if
         
               if (iy < end_j) then
                  if (ix > start_i) then
          
                     ! Neighbor with relative position (-1,+1)
                     call process_neighbor(ix-1, iy+1, bit_domain, point_queue, tile_queue, &
                                           xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
                  end if
          
                  ! Neighbor with relative position (0,+1)
                  call process_neighbor(ix, iy+1, bit_domain, point_queue, tile_queue, &
                                        xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
                  if (ix < end_i) then
          
                     ! Neighbor with relative position (+1,+1)
                     call process_neighbor(ix+1, iy+1, bit_domain, point_queue, tile_queue, &
                                           xlat_array, xlon_array, start_i, end_i, start_j, end_j, ilevel)
                  end if
               end if
         
            end do
        
         end do
   
         if (itype == SP_CONTINUOUS) then
            itype = CONTINUOUS
            if (present(landmask) .and. (istagger == M .or. istagger == HH)) then
               do j=start_j,end_j
                  do i=start_i,end_i
                     if (landmask(i,j) /= mask_val) then
                        do k=start_k,end_k
                           if (data_count(i,j,k) > 0.) then
                              field(i,j,k) = field(i,j,k) / data_count(i,j,k)
                           else
                              if (.not.bitarray_test(processed_domain, i-start_i+1, j-start_j+1)) then
                                 field(i,j,k) = msg_fill_val
                              end if
                           end if
                        end do
                     else
                        if (.not.bitarray_test(processed_domain, i-start_i+1, j-start_j+1)) then
                           do k=start_k,end_k
                              field(i,j,k) = msg_fill_val
                           end do
                        end if
                     end if
                  end do
               end do
            else
               do k=start_k,end_k
                  do j=start_j,end_j
                     do i=start_i,end_i
                        if (data_count(i,j,k) > 0.) then
                           field(i,j,k) = field(i,j,k) / data_count(i,j,k)
                        else
                           if (.not.bitarray_test(processed_domain, i-start_i+1, j-start_j+1)) then
                              field(i,j,k) = msg_fill_val
                           end if
                        end if
                     end do
                  end do
               end do
            end if
            deallocate(data_count)
   
         else if (itype == CATEGORICAL) then
            if (present(landmask) .and. (istagger == M .or. istagger == HH)) then
               do j=start_j,end_j
                  do i=start_i,end_i
                     if (landmask(i,j) == mask_val) then
                        do k=start_k,end_k
                           field(i,j,k) = 0.
                        end do
                     end if
                  end do
               end do
            end if
         end if
   
         deallocate(interp_type)
         deallocate(interp_opts)
   
   
         ! We may need to scale this field by a constant
         call get_field_scale_factor(fieldname, ilevel, scale_factor, istatus)
         if (istatus == 0) then
            do i=start_i, end_i
               do j=start_j, end_j
                  if (bitarray_test(level_domain,i-start_i+1,j-start_j+1) .and. &
                      .not. bitarray_test(processed_domain,i-start_i+1,j-start_j+1)) then
                     do k=start_k,end_k
                        if (field(i,j,k) /= msg_fill_val) then
                           field(i,j,k) = field(i,j,k) * scale_factor
                        end if
                     end do
                  end if
               end do
            end do
         end if
   
       
         ! Now add the points that were assigned values at this priority level to the complete array
         !   of points that have been assigned values
         call bitarray_merge(processed_domain, level_domain)
       
         call bitarray_destroy(bit_domain)
         call bitarray_destroy(level_domain)
         call q_destroy(point_queue)
         call q_destroy(tile_queue)
         call proc_point_shutdown()
       
         ! Remove the projection of the current level of source data from the stack, thus "activating" 
         !   the projection of the next highest level
         call pop_source_projection()

      else
         call mprintf(.true.,STDOUT,'   Important note: could not open input dataset for priority level %i, '// &
                                    'but this level is optional.', i1=ilevel)
         call mprintf(.true.,LOGFILE,'   Important note: could not open input dataset for priority level %i, '// &
                                    'but this level is optional.', i1=ilevel)
      end if

      ! If this is the last level of the recursion, we can also deallocate processed_domain
      if (ilevel == 1) call bitarray_destroy(processed_domain)
   
   end subroutine calc_field
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_lat_lon_fields
   !
   ! Purpose: To calculate the latitude and longitude for every gridpoint in the
   !   tile of the model domain. The caller may specify that the grid for which 
   !   values are computed is staggered or unstaggered using the "stagger" 
   !   argument.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_lat_lon_fields(xlat_arr, xlon_arr, start_mem_i, &
                                 start_mem_j, end_mem_i, end_mem_j, stagger, comp_ll, &
                                 sub_x, sub_y)
   
      use llxy_module
      use misc_definitions_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, end_mem_i, &
                             end_mem_j, stagger
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(out) :: xlat_arr, xlon_arr
      logical, optional, intent(in) :: comp_ll
      integer, optional, intent(in) :: sub_x, sub_y

      ! Local variables
      integer :: i, j
      real :: rx, ry
    
      rx = 1.0
      ry = 1.0
      if (present(sub_x)) rx = real(sub_x)
      if (present(sub_y)) ry = real(sub_y)

      do i=start_mem_i, end_mem_i
         do j=start_mem_j, end_mem_j
            call xytoll(real(i-1)/rx+1.0, real(j-1)/ry+1.0, &
                        xlat_arr(i,j), xlon_arr(i,j), stagger, comp_ll=comp_ll)
         end do
      end do

   end subroutine get_lat_lon_fields
   

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_map_factor
   !
   ! Purpose: Given the latitude field, this routine calculates map factors for 
   !   the grid points of the specified domain. For different grids (e.g., C grid, 
   !   E grid), the latitude array should provide the latitudes of the points for
   !   which map factors are to be calculated. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_map_factor(xlat_arr, xlon_arr, mapfac_arr_x, mapfac_arr_y, &
                             start_mem_i, start_mem_j, end_mem_i, end_mem_j)
   
      use constants_module
      use gridinfo_module
      use misc_definitions_module
      use map_utils
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, end_mem_i, end_mem_j
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(in) :: xlat_arr, xlon_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(out) :: mapfac_arr_x
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(out) :: mapfac_arr_y
    
      ! Local variables
      integer :: i, j
      real :: n, colat, colat0, colat1, colat2, comp_lat, comp_lon
    
      !
      ! Equations for map factor given in Principles of Meteorological Analysis,
      ! Walter J. Saucier, pp. 32-33 
      !
    
      ! Lambert conformal projection
      if (iproj_type == PROJ_LC) then
         if (truelat1 /= truelat2) then
            colat1 = rad_per_deg*(90.0 - truelat1)
            colat2 = rad_per_deg*(90.0 - truelat2)
            n = (log(sin(colat1)) - log(sin(colat2))) &
                / (log(tan(colat1/2.0)) - log(tan(colat2/2.0)))
      
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  colat = rad_per_deg*(90.0 - xlat_arr(i,j))
                  mapfac_arr_x(i,j) = sin(colat2)/sin(colat)*(tan(colat/2.0)/tan(colat2/2.0))**n
                  mapfac_arr_y(i,j) = mapfac_arr_x(i,j)
               end do
            end do
     
         else
            colat0 = rad_per_deg*(90.0 - truelat1)
      
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  colat = rad_per_deg*(90.0 - xlat_arr(i,j))
                  mapfac_arr_x(i,j) = sin(colat0)/sin(colat)*(tan(colat/2.0)/tan(colat0/2.0))**cos(colat0)
                  mapfac_arr_y(i,j) = mapfac_arr_x(i,j)
               end do
            end do
    
         end if
    
      ! Polar stereographic projection
      else if (iproj_type == PROJ_PS) then
    
         do i=start_mem_i, end_mem_i
            do j=start_mem_j, end_mem_j
               mapfac_arr_x(i,j) = (1.0 + sin(rad_per_deg*abs(truelat1)))/(1.0 + sin(rad_per_deg*sign(1.,truelat1)*xlat_arr(i,j)))
               mapfac_arr_y(i,j) = mapfac_arr_x(i,j)
            end do
         end do
    
      ! Mercator projection 
      else if (iproj_type == PROJ_MERC) then
         colat0 = rad_per_deg*(90.0 - truelat1)
     
         do i=start_mem_i, end_mem_i
            do j=start_mem_j, end_mem_j
               colat = rad_per_deg*(90.0 - xlat_arr(i,j))
               mapfac_arr_x(i,j) = sin(colat0) / sin(colat) 
               mapfac_arr_y(i,j) = mapfac_arr_x(i,j)
            end do
         end do
    
      ! Global cylindrical projection
      else if (iproj_type == PROJ_CYL) then
     
         do i=start_mem_i, end_mem_i
            do j=start_mem_j, end_mem_j
               if (abs(xlat_arr(i,j)) == 90.0) then
                  mapfac_arr_x(i,j) = 0.    ! MSF actually becomes infinite at poles, but 
                                            !   the values should never be used there; by
                                            !   setting to 0, we hope to induce a "divide
                                            !   by zero" error if they are
               else
                  mapfac_arr_x(i,j) = 1.0 / cos(xlat_arr(i,j)*rad_per_deg) 
               end if
               mapfac_arr_y(i,j) = 1.0
            end do
         end do
    
      ! Rotated global cylindrical projection
      else if (iproj_type == PROJ_CASSINI) then
     
         if (abs(pole_lat) == 90.) then
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  if (abs(xlat_arr(i,j)) >= 90.0) then
                     mapfac_arr_x(i,j) = 0.    ! MSF actually becomes infinite at poles, but 
                                               !   the values should never be used there; by
                                               !   setting to 0, we hope to induce a "divide
                                               !   by zero" error if they are
                  else
                     mapfac_arr_x(i,j) = 1.0 / cos(xlat_arr(i,j)*rad_per_deg) 
                  end if
                  mapfac_arr_y(i,j) = 1.0
               end do
            end do
         else
            do i=start_mem_i, end_mem_i
               do j=start_mem_j, end_mem_j
                  call rotate_coords(xlat_arr(i,j),xlon_arr(i,j), &
                                     comp_lat, comp_lon, &
                                     pole_lat, pole_lon, stand_lon, &
                                     -1)
                  if (abs(comp_lat) >= 90.0) then
                     mapfac_arr_x(i,j) = 0.    ! MSF actually becomes infinite at poles, but 
                                               !   the values should never be used there; by
                                               !   setting to 0, we hope to induce a "divide
                                               !   by zero" error if they are
                  else
                     mapfac_arr_x(i,j) = 1.0 / cos(comp_lat*rad_per_deg) 
                  end if
                  mapfac_arr_y(i,j) = 1.0
               end do
            end do
         end if
    
      else if (iproj_type == PROJ_ROTLL) then
    
         do i=start_mem_i, end_mem_i
            do j=start_mem_j, end_mem_j
               mapfac_arr_x(i,j) = 1.0
               mapfac_arr_y(i,j) = 1.0
            end do
         end do
    
      end if
   
   end subroutine get_map_factor
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_coriolis_parameters
   !
   ! Purpose: To calculate the Coriolis parameters f and e for every gridpoint in
   !   the tile of the model domain 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_coriolis_parameters(xlat_arr, f, e, &
                                      start_mem_i, start_mem_j, end_mem_i, end_mem_j)
     
      use constants_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, end_mem_i, end_mem_j
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(in) :: xlat_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(out) :: f, e
    
      ! Local variables
      integer :: i, j
    
      do i=start_mem_i, end_mem_i
         do j=start_mem_j, end_mem_j
     
            f(i,j) = 2.0*OMEGA_E*sin(rad_per_deg*xlat_arr(i,j))
            e(i,j) = 2.0*OMEGA_E*cos(rad_per_deg*xlat_arr(i,j))
     
         end do
      end do
      
   end subroutine get_coriolis_parameters
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_rotang
   !
   ! Purpose: To calculate the sine and cosine of rotation angle. 
   !
   ! NOTES: The formulas used in this routine come from those in the 
   !   vecrot_rotlat() routine of the original WRF SI.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_rotang(xlat_arr, xlon_arr, cosa, sina, &
                         start_mem_i, start_mem_j, end_mem_i, end_mem_j)
   
      use constants_module
      use gridinfo_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, end_mem_i, end_mem_j
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(in) :: xlat_arr, xlon_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(out) :: cosa, sina
    
      ! Local variables
      integer :: i, j
      real :: alpha, d_lon

      do i=start_mem_i, end_mem_i
         do j=start_mem_j+1, end_mem_j-1
            d_lon = xlon_arr(i,j+1)-xlon_arr(i,j-1)
            if (d_lon > 180.) then
               d_lon = d_lon - 360.
            else if (d_lon < -180.) then
               d_lon = d_lon + 360.
            end if

            alpha = atan2(-cos(xlat_arr(i,j)*RAD_PER_DEG) * (d_lon*RAD_PER_DEG), &
                            ((xlat_arr(i,j+1)-xlat_arr(i,j-1))*RAD_PER_DEG))
            sina(i,j) = sin(alpha)
            cosa(i,j) = cos(alpha)
         end do
      end do

      do i=start_mem_i, end_mem_i
         d_lon = xlon_arr(i,start_mem_j+1)-xlon_arr(i,start_mem_j)
         if (d_lon > 180.) then
            d_lon = d_lon - 360.
         else if (d_lon < -180.) then
            d_lon = d_lon + 360.
         end if

         alpha = atan2(-cos(xlat_arr(i,start_mem_j)*RAD_PER_DEG) * (d_lon*RAD_PER_DEG), &
                       ((xlat_arr(i,start_mem_j+1)-xlat_arr(i,start_mem_j))*RAD_PER_DEG))
         sina(i,start_mem_j) = sin(alpha)
         cosa(i,start_mem_j) = cos(alpha)
      end do

      do i=start_mem_i, end_mem_i
         d_lon = xlon_arr(i,end_mem_j)-xlon_arr(i,end_mem_j-1)
         if (d_lon > 180.) then
            d_lon = d_lon - 360.
         else if (d_lon < -180.) then
            d_lon = d_lon + 360.
         end if

         alpha = atan2(-cos(xlat_arr(i,end_mem_j)*RAD_PER_DEG) * (d_lon*RAD_PER_DEG), &
                       ((xlat_arr(i,end_mem_j)-xlat_arr(i,end_mem_j-1))*RAD_PER_DEG))
         sina(i,end_mem_j) = sin(alpha)
         cosa(i,end_mem_j) = cos(alpha)
      end do
    
   end subroutine get_rotang
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: process_neighbor
   !
   ! Purpose: This routine, give the x/y location of a point, determines whether
   !   the point has already been processed, and if not, which processing queue
   !   the point should be placed in.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine process_neighbor(ix, iy, bit_domain, point_queue, tile_queue, &
                               xlat_array, xlon_array, &
                               start_i, end_i, start_j, end_j, ilevel)
   
      use bitarray_module
      use misc_definitions_module
      use proc_point_module
      use queue_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: ix, iy, start_i, end_i, start_j, end_j, ilevel
      real, dimension(start_i:end_i, start_j:end_j), intent(in) :: xlat_array, xlon_array
      type (bitarray), intent(inout) :: bit_domain
      type (queue), intent(inout) :: point_queue, tile_queue
    
      ! Local variables
      type (q_data) :: process_pt
      logical :: is_in_tile
    
      ! If the point has already been visited, no need to do anything more.
      if (.not. bitarray_test(bit_domain, ix-start_i+1, iy-start_j+1)) then 
    
         ! Create a queue item for the current point
         process_pt%lat = xlat_array(ix,iy)            
         process_pt%lon = xlon_array(ix,iy)            
         process_pt%x = ix
         process_pt%y = iy
     
         is_in_tile = is_point_in_tile(process_pt%lat, process_pt%lon, ilevel)
     
         ! If the point is in the current tile, add it to the list of points
         !   to be processed in the inner loop
         if (is_in_tile) then
            call q_insert(point_queue, process_pt)
            call bitarray_set(bit_domain, ix-start_i+1, iy-start_j+1) 
     
         ! Otherwise, we will process this point later. Add it to the list for 
         !   the outer loop.
         else
            call q_insert(tile_queue, process_pt)
         end if
    
      end if
   
   end subroutine process_neighbor
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: calc_dfdy
   !
   ! Purpose: This routine calculates df/dy for the field in src_arr, and places
   !   the result in dst_array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine calc_dfdy(src_arr, dst_arr, start_mem_i, start_mem_j, start_mem_k, &
                        end_mem_i, end_mem_j, end_mem_k, mapfac)
   
      ! Modules
      use gridinfo_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, start_mem_k, end_mem_i, end_mem_j, end_mem_k
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j,start_mem_k:end_mem_k), intent(in) :: src_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j,start_mem_k:end_mem_k), intent(out) :: dst_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(in), optional :: mapfac
    
      ! Local variables
      integer :: i, j, k
    
      if (present(mapfac)) then
         do k=start_mem_k,end_mem_k
            do i=start_mem_i, end_mem_i
               do j=start_mem_j+1, end_mem_j-1
                  dst_arr(i,j,k) = (src_arr(i,j+1,k) - src_arr(i,j-1,k))/(2.*dykm*mapfac(i,j))
               end do
            end do
     
            do i=start_mem_i, end_mem_i
               dst_arr(i,start_mem_j,k) = (src_arr(i,start_mem_j+1,k) - src_arr(i,start_mem_j,k))/(dykm*mapfac(i,j))
            end do
     
            do i=start_mem_i, end_mem_i
               dst_arr(i,end_mem_j,k) = (src_arr(i,end_mem_j,k) - src_arr(i,end_mem_j-1,k))/(dykm*mapfac(i,j))
            end do
         end do
      else
         do k=start_mem_k,end_mem_k
            do i=start_mem_i, end_mem_i
               do j=start_mem_j+1, end_mem_j-1
                  dst_arr(i,j,k) = (src_arr(i,j+1,k) - src_arr(i,j-1,k))/(2.*dykm)
               end do
            end do
     
            do i=start_mem_i, end_mem_i
               dst_arr(i,start_mem_j,k) = (src_arr(i,start_mem_j+1,k) - src_arr(i,start_mem_j,k))/(dykm)
            end do
     
            do i=start_mem_i, end_mem_i
               dst_arr(i,end_mem_j,k) = (src_arr(i,end_mem_j,k) - src_arr(i,end_mem_j-1,k))/(dykm)
            end do
         end do
      end if
   
   end subroutine calc_dfdy
   
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: calc_dfdx
   !
   ! Purpose: This routine calculates df/dx for the field in src_arr, and places
   !   the result in dst_array.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine calc_dfdx(src_arr, dst_arr, start_mem_i, start_mem_j, &
                        start_mem_k, end_mem_i, end_mem_j, end_mem_k, mapfac)
   
      ! Modules
      use gridinfo_module
    
      implicit none
    
      ! Arguments
      integer, intent(in) :: start_mem_i, start_mem_j, start_mem_k, end_mem_i, end_mem_j, end_mem_k
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j, start_mem_k:end_mem_k), intent(in) :: src_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j, start_mem_k:end_mem_k), intent(out) :: dst_arr
      real, dimension(start_mem_i:end_mem_i, start_mem_j:end_mem_j), intent(in), optional :: mapfac
    
      ! Local variables
      integer :: i, j, k
    
      if (present(mapfac)) then
         do k=start_mem_k, end_mem_k
            do i=start_mem_i+1, end_mem_i-1
               do j=start_mem_j, end_mem_j
                  dst_arr(i,j,k) = (src_arr(i+1,j,k) - src_arr(i-1,j,k))/(2.*dxkm*mapfac(i,j))
               end do
            end do
     
            do j=start_mem_j, end_mem_j
               dst_arr(start_mem_i,j,k) = (src_arr(start_mem_i+1,j,k) - src_arr(start_mem_i,j,k))/(dxkm*mapfac(i,j))
            end do
     
            do j=start_mem_j, end_mem_j
               dst_arr(end_mem_i,j,k) = (src_arr(end_mem_i,j,k) - src_arr(end_mem_i-1,j,k))/(dxkm*mapfac(i,j))
            end do
         end do
      else
         do k=start_mem_k, end_mem_k
            do i=start_mem_i+1, end_mem_i-1
               do j=start_mem_j, end_mem_j
                  dst_arr(i,j,k) = (src_arr(i+1,j,k) - src_arr(i-1,j,k))/(2.*dxkm)
               end do
            end do
     
            do j=start_mem_j, end_mem_j
               dst_arr(start_mem_i,j,k) = (src_arr(start_mem_i+1,j,k) - src_arr(start_mem_i,j,k))/(dxkm)
            end do
     
            do j=start_mem_j, end_mem_j
               dst_arr(end_mem_i,j,k) = (src_arr(end_mem_i,j,k) - src_arr(end_mem_i-1,j,k))/(dxkm)
            end do
         end do
      end if
   
   end subroutine calc_dfdx

end module process_tile_module
