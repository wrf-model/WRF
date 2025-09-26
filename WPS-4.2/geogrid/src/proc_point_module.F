!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: proc_point_module
!
! Purpose: This module provides routines that produce a value for a model grid
!    point in two ways. If the field for which a value is being calculated is
!    a continuous field, this module provided functionality to interpolate 
!    from the source array to the specified point. If the field is a categorical
!    field, this module provided functionality to accumulate the values of all 
!    source points whose nearest model gridpoint is the specified point.
!    Routines are also provided that help the caller determine an optimized 
!    order in which to process the model grid points.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module proc_point_module

   ! Modules
   use bitarray_module
   use hash_module
   use misc_definitions_module
   use module_debug
   use source_data_module
 
   ! Information about which tile is in memory
   integer :: src_min_x, src_max_x, src_min_y, src_max_y, src_min_z, src_max_z, src_npts_bdr
   integer :: src_level
   character (len=128) :: src_fieldname
   character (len=256) :: src_fname
 
   ! Source tiles
   real, pointer, dimension(:,:,:) :: src_array
 
   ! Hash to track which tiles we have already processed
   type (hashtable) :: h_table
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: proc_point_init 
   !
   ! Purpose: Initialize the module.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine proc_point_init()
 
      implicit none
  
      ! Initialize module variables
      src_min_x = INVALID
      src_min_y = INVALID
      src_min_z = INVALID
      src_max_x = INVALID
      src_max_y = INVALID
      src_max_z = INVALID
      src_fieldname = ' '
      src_fname = ' '
      nullify(src_array)
  
      call hash_init(h_table)
 
   end subroutine proc_point_init 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: proc_point_shutdown 
   !
   ! Purpose: Do any cleanup work.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine proc_point_shutdown()
 
      implicit none
  
      ! Effectively reset the hash table that tracks which tiles have been processed
      !   by removing all entries
      call hash_destroy(h_table)
  
      if (associated(src_array)) deallocate(src_array)
  
      src_min_x = INVALID
      src_min_y = INVALID
      src_min_z = INVALID
      src_max_x = INVALID
      src_max_y = INVALID
      src_max_z = INVALID
      src_fieldname = ' '
      src_fname = ' '
 
   end subroutine proc_point_shutdown
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: accum_categorical
   !
   ! Purpose: Count the number of source points in each category whose nearest 
   !   neighbor is the specified model grid point.
   !
   ! NOTE: When processing the source tile, those source points that are 
   !   closest to a different model grid point will be added to the totals for 
   !   such grid points; thus, an entire source tile will be processed at a time.
   !   This routine really processes for all model grid points that are 
   !   within a source tile, and not just for a single grid point.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine accum_categorical(xlat, xlon, istagger, array, &
                                start_i, end_i, start_j, end_j, &
                                start_k, end_k, fieldname, processed_pts, &
                                new_pts, ilevel, msgval, maskval, sr_x, sr_y)

      use llxy_module
      use bitarray_module

      implicit none
  
      ! Arguments
      integer, intent(in) :: start_i, end_i, start_j, end_j, start_k, end_k, &
                             istagger, ilevel
      real, intent(in) :: xlat, xlon, msgval, maskval
      real, dimension(start_i:end_i, start_j:end_j, start_k:end_k), intent(inout) :: array
      character (len=128), intent(in) :: fieldname
      type (bitarray), intent(inout) :: processed_pts, new_pts
      integer, intent(in), optional :: sr_x, sr_y
  
      ! Local variables
      integer :: istatus, i, j
      integer :: current_domain, k
      integer, pointer, dimension(:,:,:) :: where_maps_to
      real :: rlat, rlon
      real :: rarea
      real :: rsr_x, rsr_y

      rlat = xlat
      if (xlon >= 180.) then
         rlon = xlon - 360.
      else
         rlon = xlon
      end if

      rsr_x = 1.0
      rsr_y = 1.0
      if (present(sr_x)) rsr_x = real(sr_x)
      if (present(sr_y)) rsr_y = real(sr_y)

      ! Assume source data is on unstaggered grid; specify M for istagger argument
      call get_data_tile(rlat, rlon, ilevel, fieldname, &
                         src_fname, src_array, src_min_x, src_max_x, src_min_y, &
                         src_max_y, src_min_z, src_max_z, src_npts_bdr, &
                         istatus)
  
      src_fieldname = fieldname
      src_level = ilevel

      call hash_insert(h_table, src_fname)
  
      if (istatus /= 0) return
  
      allocate(where_maps_to(src_min_x:src_max_x,src_min_y:src_max_y,2))
      do i=src_min_x,src_max_x
         do j=src_min_y,src_max_y
            where_maps_to(i,j,1) = NOT_PROCESSED 
         end do
      end do

      call process_categorical_block(src_array, istagger, where_maps_to, &
                              src_min_x+src_npts_bdr, src_min_y+src_npts_bdr, src_min_z, &
                              src_max_x-src_npts_bdr, src_max_y-src_npts_bdr, src_max_z, &
                              array, start_i, end_i, start_j, end_j, start_k, end_k, &
                              processed_pts, new_pts, ilevel, rsr_x, rsr_y, msgval, maskval)

      ! If a grid cell has less than half of its area covered by data from this source,
      !   then clear the cell and let another source fill in the cell
      if (ilevel > 1) then
         do i=start_i,end_i
            do j=start_j,end_j
               if (bitarray_test(new_pts, i-start_i+1, j-start_j+1) .and. &
                   .not. bitarray_test(processed_pts, i-start_i+1, j-start_j+1)) then
                  rarea = 0.
                  do k=start_k,end_k
                     rarea = rarea + array(i,j,k)
                  end do
                  current_domain = iget_selected_domain()
                  call select_domain(SOURCE_PROJ)
                  if (proj_stack(current_nest_number)%dx < 0.) then
                     rarea = rarea * (proj_stack(current_nest_number)%latinc*111000.)**2.0
                  else
                     rarea = rarea * proj_stack(current_nest_number)%dx**2.0
                  end if
                  call select_domain(current_domain)
                  if (proj_stack(current_nest_number)%dx < 0.) then
                     if ((proj_stack(current_nest_number)%latinc*111000.)**2.0 > 2.0*rarea) then
                        do k=start_k,end_k
                           array(i,j,k) = 0.
                        end do
                        call bitarray_clear(new_pts, i-start_i+1, j-start_j+1)
                     end if 
                  else
                     if (proj_stack(current_nest_number)%dx**2.0 > 2.0*rarea) then
                        do k=start_k,end_k
                           array(i,j,k) = 0.
                        end do
                        call bitarray_clear(new_pts, i-start_i+1, j-start_j+1)
                     end if 
                  end if
               end if
            end do
         end do
      end if
  
      deallocate(where_maps_to)
 
   end subroutine accum_categorical
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: process_categorical_block 
   !
   ! Purpose: To recursively process a subarray of categorical data, assigning
   !   the points in a block to their nearest grid point. The nearest neighbor
   !   may be estimated in some cases; for example, if the four corners of a 
   !   subarray all have the same nearest grid point, all elements in the 
   !   subarray are assigned to that grid point.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   recursive subroutine process_categorical_block(tile_array, istagger, where_maps_to, &
                                   min_i, min_j, min_k, max_i, max_j, max_k, dst_array, &
                                   start_x, end_x, start_y, end_y, start_z, end_z, &
                                   processed_pts, new_pts, ilevel, sr_x, sr_y, &
                                   msgval, maskval, mask_array)
 
      use llxy_module
  
      implicit none
  
      ! Arguments
      integer, intent(in) :: min_i, min_j, min_k, max_i, max_j, max_k, istagger, &
                             start_x, end_x, start_y, end_y, start_z, end_z, ilevel
      integer, dimension(src_min_x:src_max_x,src_min_y:src_max_y,2), intent(inout) :: where_maps_to
      real, intent(in) :: sr_x, sr_y, msgval, maskval
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y,src_min_z:src_max_z), intent(in) :: tile_array
      real, dimension(start_x:end_x,start_y:end_y,start_z:end_z), intent(inout) :: dst_array
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y), intent(in), optional :: mask_array
      type (bitarray), intent(inout) :: processed_pts, new_pts
  
      ! Local variables
      integer :: x_dest, y_dest, i, j, k, center_i, center_j, current_domain
      real :: lat_corner, lon_corner, rx, ry

      ! Compute the model grid point that the corners of the rectangle to be 
      !   processed map to
      ! Lower-left corner
      if (where_maps_to(min_i,min_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(min_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,min_j,1) = nint(rx)
            where_maps_to(min_i,min_j,2) = nint(ry)
         else
            where_maps_to(min_i,min_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Upper-left corner
      if (where_maps_to(min_i,max_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(max_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,max_j,1) = nint(rx)
            where_maps_to(min_i,max_j,2) = nint(ry)
         else
            where_maps_to(min_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Upper-right corner
      if (where_maps_to(max_i,max_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(max_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(max_i,max_j,1) = nint(rx)
            where_maps_to(max_i,max_j,2) = nint(ry)
         else
            where_maps_to(max_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Lower-right corner
      if (where_maps_to(max_i,min_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(min_j), lat_corner, lon_corner, M)
         call select_domain(current_domain) 
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
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
         if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
 
            ! If this grid point has never been given a value by this level of source data,
            !   initialize the point
            if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
               do k=start_z,end_z
                  dst_array(x_dest,y_dest,k) = 0.
               end do
            end if
  
            ! Count all the points whose nearest neighbor is this grid point
            if (present(mask_array)) then
               do i=min_i,max_i
                  do j=min_j,max_j
                     ! Ignore masked/missing values in the source data
                     if ((tile_array(i,j,min_k) /= msgval) .and. &
                         (mask_array(i,j) /= maskval)) then
                        if (int(tile_array(i,j,min_k)) >= start_z .and. int(tile_array(i,j,min_k)) <= end_z) then
                           dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) = &
                               dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) + 1.0 
                           call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                        else
                           call mprintf(.true., WARN, 'In source tile %s, point (%i, %i) has '// &
                                        'an invalid category of %i', &
                                        s1=trim(src_fname), i1=i, i2=j, i3=int(tile_array(i,j,min_k)))
                        end if
                     end if
                  end do
               end do
            else
               do i=min_i,max_i
                  do j=min_j,max_j
                     ! Ignore masked/missing values in the source data
                     if (tile_array(i,j,min_k) /= msgval) then
                        if (int(tile_array(i,j,min_k)) >= start_z .and. int(tile_array(i,j,min_k)) <= end_z) then
                           dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) = &
                               dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) + 1.0 
                           call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                        else
                           call mprintf(.true., WARN, 'In source tile %s, point (%i, %i) '// &
                                        'has an invalid category of %i', &
                                        s1=trim(src_fname), i1=i, i2=j, i3=int(tile_array(i,j,min_k)))
                        end if
                     end if
                  end do
               end do
            end if
 
         end if
  
      ! Rectangle is a square of four points, and we can simply deal with each of the points
      else if (((max_i - min_i + 1) <= 2) .and. ((max_j - min_j + 1) <= 2)) then
         do i=min_i,max_i
            do j=min_j,max_j
               x_dest = where_maps_to(i,j,1)
               y_dest = where_maps_to(i,j,2)
     
               if (x_dest /= OUTSIDE_DOMAIN) then 
      
                  if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                     if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                        do k=start_z,end_z
                           dst_array(x_dest,y_dest,k) = 0.
                        end do
                     end if
                     
                     ! Ignore masked/missing values
                     if (present(mask_array)) then
                        if ((tile_array(i,j,min_k) /= msgval) .and. &
                             (mask_array(i,j) /= maskval)) then
                           if (int(tile_array(i,j,min_k)) >= start_z .and. int(tile_array(i,j,min_k)) <= end_z) then
                              dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) = &
                                  dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) + 1.0 
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           else
                              call mprintf(.true., WARN, 'In source tile %s, point (%i, %i) has '// &
                                           'an invalid category of %i', &
                                           s1=trim(src_fname), i1=i, i2=j, i3=int(tile_array(i,j,min_k)))
                           end if
                        end if
                     else
                        if (tile_array(i,j,min_k) /= msgval) then 
                           if (int(tile_array(i,j,min_k)) >= start_z .and. int(tile_array(i,j,min_k)) <= end_z) then
                              dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) = &
                                  dst_array(x_dest,y_dest,int(tile_array(i,j,min_k))) + 1.0 
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           else
                              call mprintf(.true., WARN, 'In source tile %s, point (%i, %i) has '// &
                                           'an invalid category of %i', &
                                           s1=trim(src_fname), i1=i, i2=j, i3=int(tile_array(i,j,min_k)))
                           end if
                        end if
                     end if
                  end if
     
               end if
            end do
         end do
  
      ! Not all corners map to the same grid point, and the rectangle contains more than
      !   four points
      else
         center_i = (max_i + min_i)/2
         center_j = (max_j + min_j)/2
   
         ! Recursively process lower-left rectangle
         call process_categorical_block(tile_array, istagger, where_maps_to, min_i, min_j, min_k, center_i, &
                    center_j, max_k, dst_array, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                    new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array)
         
         ! Recursively process lower-right rectangle
         if (center_i < max_i) then
            call process_categorical_block(tile_array, istagger, where_maps_to, center_i+1, min_j, min_k, max_i, &
                       center_j, max_k, dst_array, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array)
         end if
   
         ! Recursively process upper-left rectangle
         if (center_j < max_j) then
            call process_categorical_block(tile_array, istagger, where_maps_to, min_i, center_j+1, min_k, center_i, &
                       max_j, max_k, dst_array, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array)
         end if
   
         ! Recursively process upper-right rectangle
         if (center_i < max_i .and. center_j < max_j) then
            call process_categorical_block(tile_array, istagger, where_maps_to, center_i+1, center_j+1, min_k, max_i, &
                       max_j, max_k, dst_array, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array)
         end if
      end if
  
   end subroutine process_categorical_block
 
 
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
   subroutine accum_continuous(xlat, xlon, istagger, array, n, &
                                start_i, end_i, start_j, end_j, &
                                start_k, end_k, fieldname, processed_pts, &
                                new_pts, ilevel, msgval, maskval, sr_x, sr_y)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: start_i, end_i, start_j, end_j, start_k, end_k, &
                             istagger, ilevel
      real, intent(in) :: xlat, xlon, msgval, maskval
      real, dimension(start_i:end_i, start_j:end_j, start_k:end_k), intent(inout) :: array, n
      character (len=128), intent(in) :: fieldname
      type (bitarray), intent(inout) :: processed_pts, new_pts
      integer, intent(in), optional :: sr_x, sr_y
  
      ! Local variables
      integer :: istatus, i, j
      integer, pointer, dimension(:,:,:) :: where_maps_to
      real :: rlat, rlon
      real :: rsr_x, rsr_y

      rlat = xlat
      if (xlon >= 180.) then
         rlon = xlon - 360.
      else
         rlon = xlon
      end if

      rsr_x = 1.0
      rsr_y = 1.0
      if (present(sr_x)) rsr_x = real(sr_x)
      if (present(sr_y)) rsr_y = real(sr_y)

      ! Assume source data is on unstaggered grid; specify M for istagger argument
      call get_data_tile(rlat, rlon, ilevel, fieldname, &
                         src_fname, src_array, src_min_x, src_max_x, src_min_y, &
                         src_max_y, src_min_z, src_max_z, src_npts_bdr, &
                         istatus)
  
      src_fieldname = fieldname
      src_level = ilevel
  
      call hash_insert(h_table, src_fname)
  
      if (istatus /= 0) then
         src_min_x = INVALID
         src_min_y = INVALID
         src_max_x = INVALID
         src_max_y = INVALID
         return
      end if
  
      allocate(where_maps_to(src_min_x:src_max_x,src_min_y:src_max_y,2))
      do i=src_min_x,src_max_x
         do j=src_min_y,src_max_y
            where_maps_to(i,j,1) = NOT_PROCESSED 
         end do
      end do
  
      call process_continuous_block(src_array, istagger, where_maps_to, &
                               src_min_x+src_npts_bdr, src_min_y+src_npts_bdr, src_min_z, &
                               src_max_x-src_npts_bdr, src_max_y-src_npts_bdr, src_max_z, &
                               array, n, start_i, end_i, start_j, end_j, start_k, end_k, &
                               processed_pts, new_pts, ilevel, rsr_x, rsr_y, msgval, maskval)
  
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
   recursive subroutine process_continuous_block(tile_array, istagger, where_maps_to, &
                                   min_i, min_j, min_k, max_i, max_j, max_k, dst_array, n, &
                                   start_x, end_x, start_y, end_y, start_z, end_z, &
                                   processed_pts, new_pts, ilevel, sr_x, sr_y, &
                                   msgval, maskval, mask_array)
 
      use llxy_module
  
      implicit none
  
      ! Arguments
      integer, intent(in) :: min_i, min_j, min_k, max_i, max_j, max_k, istagger, &
                             start_x, end_x, start_y, end_y, start_z, end_z, ilevel
      integer, dimension(src_min_x:src_max_x,src_min_y:src_max_y,2), intent(inout) :: where_maps_to
      real, intent(in) :: sr_x, sr_y, msgval, maskval
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y,src_min_z:src_max_z), intent(in) :: tile_array
      real, dimension(start_x:end_x,start_y:end_y,start_z:end_z), intent(inout) :: dst_array, n
      real, dimension(src_min_x:src_max_x,src_min_y:src_max_y), intent(in), optional :: mask_array
      type (bitarray), intent(inout) :: processed_pts, new_pts
  
      ! Local variables
      integer :: x_dest, y_dest, i, j, k, center_i, center_j, current_domain
      real :: lat_corner, lon_corner, rx, ry
  
      ! Compute the model grid point that the corners of the rectangle to be 
      !   processed map to
      ! Lower-left corner
      if (where_maps_to(min_i,min_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(min_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,min_j,1) = nint(rx)
            where_maps_to(min_i,min_j,2) = nint(ry)
         else
            where_maps_to(min_i,min_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Upper-left corner
      if (where_maps_to(min_i,max_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(min_i), real(max_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(min_i,max_j,1) = nint(rx)
            where_maps_to(min_i,max_j,2) = nint(ry)
         else
            where_maps_to(min_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Upper-right corner
      if (where_maps_to(max_i,max_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(max_j), lat_corner, lon_corner, M)
         call select_domain(current_domain)
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
            where_maps_to(max_i,max_j,1) = nint(rx)
            where_maps_to(max_i,max_j,2) = nint(ry)
         else
            where_maps_to(max_i,max_j,1) = OUTSIDE_DOMAIN
         end if
      end if
  
      ! Lower-right corner
      if (where_maps_to(max_i,min_j,1) == NOT_PROCESSED) then
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call xytoll(real(max_i), real(min_j), lat_corner, lon_corner, M)
         call select_domain(current_domain) 
         call lltoxy(lat_corner, lon_corner, rx, ry, istagger)
         rx = (rx-1.0) * sr_x + 1.0
         ry = (ry-1.0) * sr_y + 1.0
         if (real(start_x) <= rx .and. rx <= real(end_x) .and. real(start_y) <= ry .and. ry <= real(end_y)) then
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
         if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
 
            ! If this grid point has never been given a value by this level of source data,
            !   initialize the point
            if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
               do k=min_k,max_k
                  dst_array(x_dest,y_dest,k) = 0.
               end do
            end if
  
            ! Sum all the points whose nearest neighbor is this grid point
            if (present(mask_array)) then
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
                        if (tile_array(i,j,k) /= msgval) then 
                           dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k) 
                           n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                           call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                        end if
                     end do
                  end do
               end do
            end if
 
         end if
  
      ! Rectangle is a square of four points, and we can simply deal with each of the points
      else if (((max_i - min_i + 1) <= 2) .and. ((max_j - min_j + 1) <= 2)) then
         do i=min_i,max_i
            do j=min_j,max_j
               x_dest = where_maps_to(i,j,1)
               y_dest = where_maps_to(i,j,2)
     
               if (x_dest /= OUTSIDE_DOMAIN) then 
      
                  if (.not. bitarray_test(processed_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                     if (.not. bitarray_test(new_pts, x_dest-start_x+1, y_dest-start_y+1)) then
                        do k=min_k,max_k
                           dst_array(x_dest,y_dest,k) = 0.
                        end do
                     end if
                     
                     if (present(mask_array)) then
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
                           if (tile_array(i,j,k) /= msgval) then 
                              dst_array(x_dest,y_dest,k) = dst_array(x_dest,y_dest,k) + tile_array(i,j,k)
                              n(x_dest,y_dest,k) = n(x_dest,y_dest,k) + 1.0
                              call bitarray_set(new_pts, x_dest-start_x+1, y_dest-start_y+1)
                           end if
                        end do
                     end if
                  end if
     
               end if
            end do
         end do
  
      ! Not all corners map to the same grid point, and the rectangle contains more than
      !   four points
      else
         center_i = (max_i + min_i)/2
         center_j = (max_j + min_j)/2
   
         ! Recursively process lower-left rectangle
         call process_continuous_block(tile_array, istagger, where_maps_to, min_i, min_j, min_k, center_i, &
                    center_j, max_k, dst_array, n, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                    new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array) 
         
         ! Recursively process lower-right rectangle
         if (center_i < max_i) then
            call process_continuous_block(tile_array, istagger, where_maps_to, center_i+1, min_j, min_k, max_i, &
                       center_j, max_k, dst_array, n, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array) 
         end if
   
         ! Recursively process upper-left rectangle
         if (center_j < max_j) then
            call process_continuous_block(tile_array, istagger, where_maps_to, min_i, center_j+1, min_k, center_i, &
                       max_j, max_k, dst_array, n, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array) 
         end if
   
         ! Recursively process upper-right rectangle
         if (center_i < max_i .and. center_j < max_j) then
            call process_continuous_block(tile_array, istagger, where_maps_to, center_i+1, center_j+1, min_k, max_i, &
                       max_j, max_k, dst_array, n, start_x, end_x, start_y, end_y, start_z, end_z, processed_pts, &
                       new_pts, ilevel, sr_x, sr_y, msgval, maskval, mask_array) 
         end if

      end if
  
   end subroutine process_continuous_block
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: get_point 
   !
   ! Purpose: For a specified lat/lon and level, return the value of the field
   !   interpolated to or nearest the lat/lon.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function get_point(xlat, xlon, lvl, fieldname, &
                      ilevel, interp_type, interp_opts, msgval)
 
      ! Modules
      use interp_module
      use llxy_module
    
      implicit none
  
      ! Arguments
      integer, intent(in) :: lvl, ilevel
      real, intent(in) :: xlat, xlon, msgval
      character (len=128), intent(in) :: fieldname
      integer, dimension(:), intent(in) :: interp_type
      integer, dimension(:), intent(in) :: interp_opts
  
      ! Return value
      real :: get_point
  
      ! Local variables
      integer :: istatus, current_domain
      real :: rlat, rlon, rx, ry
  
      rlat = xlat
      if (xlon >= 180.) then
         rlon = xlon - 360.
      else
         rlon = xlon
      end if

      ! If tile is in memory, interpolate
      if (ilevel == src_level .and. is_point_in_tile(rlat, rlon, ilevel) .and. fieldname == src_fieldname) then
  
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call lltoxy(rlat, rlon, rx, ry, M)  ! Assume source data on unstaggered grid
         call select_domain(current_domain)
   
         get_point = interp_sequence(rx, ry, lvl, src_array, src_min_x, src_max_x, src_min_y, &
                                     src_max_y, src_min_z, src_max_z, msgval, interp_type, interp_opts, 1) 
  
      else
  
         call get_data_tile(rlat, rlon, ilevel, fieldname, &
                            src_fname, src_array, src_min_x, src_max_x, src_min_y, &
                            src_max_y, src_min_z, src_max_z, src_npts_bdr, &
                            istatus)
   
         src_fieldname = fieldname
         src_level = ilevel
   
         if (istatus /= 0) then
            get_point = msgval
            return
         end if
   
         current_domain = iget_selected_domain()
         call select_domain(SOURCE_PROJ)
         call lltoxy(rlat, rlon, rx, ry, M)  ! Assume source data on unstaggered grid
         call select_domain(current_domain)
   
         get_point = interp_sequence(rx, ry, lvl, src_array, src_min_x, src_max_x, src_min_y, &
                                     src_max_y, src_min_z, src_max_z, msgval, interp_type, interp_opts, 1) 
      end if
  
      return
 
   end function get_point
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: have_processed_tile
   !
   ! Purpose: This funtion returns .true. if the tile of data for 
   !   the specified field has already been processed, and .false. otherwise.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function have_processed_tile(xlat, xlon, fieldname, ilevel)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      real, intent(in) :: xlat, xlon
      character (len=128), intent(in) :: fieldname
  
      ! Return value
      logical :: have_processed_tile
  
      ! Local variables
      integer :: istatus
      character (len=256) :: test_fname
  
      call get_tile_fname(test_fname, xlat, xlon, ilevel, fieldname, istatus)
      have_processed_tile = hash_search(h_table, test_fname)
  
      return
 
   end function have_processed_tile
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: is_point_in_tile
   !
   ! Purpose: Returns whether the specified lat/lon could be processed
   !   without incurring a file access.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   function is_point_in_tile(xlat, xlon, ilevel)
 
      use llxy_module
    
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      real, intent(in) :: xlat, xlon
  
      ! Return value
      logical :: is_point_in_tile
  
      ! Local variables
      integer :: current_domain
      real :: rlat, rlon, rx, ry
  
      rlat = xlat
      if (xlon >= 180.) then
         rlon = xlon - 360.
      else
         rlon = xlon
      end if
  
      current_domain = iget_selected_domain()
      call select_domain(SOURCE_PROJ)
      call lltoxy(rlat, rlon, rx, ry, M)
      call select_domain(current_domain)
  
  !    if (real(src_min_x+src_npts_bdr) <= rx .and. rx <= real(src_max_x-src_npts_bdr) .and. &
  !        real(src_min_y+src_npts_bdr) <= ry .and. ry <= real(src_max_y-src_npts_bdr)) then
! BUG 2006-06-01
!      if (src_min_x+src_npts_bdr <= ceiling(rx) .and. floor(rx) <= src_max_x-src_npts_bdr .and. &
!          src_min_y+src_npts_bdr <= ceiling(ry) .and. floor(ry) <= src_max_y-src_npts_bdr) then
      if (src_min_x+src_npts_bdr <= floor(rx+0.5) .and. ceiling(rx-0.5) <= src_max_x-src_npts_bdr .and. &
          src_min_y+src_npts_bdr <= floor(ry+0.5) .and. ceiling(ry-0.5) <= src_max_y-src_npts_bdr) then
         is_point_in_tile = .true.
      else
         is_point_in_tile = .false.
      end if
   
      return
 
   end function is_point_in_tile

end module proc_point_module
