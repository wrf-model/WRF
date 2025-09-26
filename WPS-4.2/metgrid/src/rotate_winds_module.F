module rotate_winds_module

   use bitarray_module
   use constants_module
   use llxy_module
   use map_utils
   use misc_definitions_module
   use module_debug

   integer :: orig_selected_projection

   contains

   !
   ! ARW Wind Rotation Code
   !

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: map_to_met                                                             !
   !                                                                              !
   ! Purpose: Rotate grid-relative winds to Earth-relative winds                  !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine map_to_met(u, u_mask, v, v_mask, &
                         us1, us2, ue1, ue2, &
                         vs1, vs2, ve1, ve2, &
                         xlon_u, xlon_v, xlat_u, xlat_v)

      implicit none

      ! Arguments
      integer, intent(in) :: us1, us2, ue1, ue2, vs1, vs2, ve1, ve2
      real, pointer, dimension(:,:) :: u, v, xlon_u, xlon_v, xlat_u, xlat_v
      type (bitarray), intent(in) :: u_mask, v_mask

      orig_selected_projection = iget_selected_domain()
      call select_domain(SOURCE_PROJ)
      call metmap_xform(u, u_mask, v, v_mask, &
                        us1, us2, ue1, ue2, &
                        vs1, vs2, ve1, ve2, &
                        xlon_u, xlon_v, xlat_u, xlat_v, 1)
      call select_domain(orig_selected_projection)

   end subroutine map_to_met

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: met_to_map                                                             !
   !                                                                              !
   ! Purpose: Rotate Earth-relative winds to grid-relative winds                  !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine met_to_map(u, u_mask, v, v_mask, &
                         us1, us2, ue1, ue2, &
                         vs1, vs2, ve1, ve2, &
                         xlon_u, xlon_v, xlat_u, xlat_v)

      implicit none

      ! Arguments
      integer, intent(in) :: us1, us2, ue1, ue2, vs1, vs2, ve1, ve2
      real, pointer, dimension(:,:) :: u, v, xlon_u, xlon_v, xlat_u, xlat_v
      type (bitarray), intent(in) :: u_mask, v_mask

      orig_selected_projection = iget_selected_domain()
      call select_domain(1)
      call metmap_xform(u, u_mask, v, v_mask, &
                        us1, us2, ue1, ue2, &
                        vs1, vs2, ve1, ve2, &
                        xlon_u, xlon_v, xlat_u, xlat_v, -1)
      call select_domain(orig_selected_projection)

   end subroutine met_to_map

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: metmap_xform                                                           !
   !                                                                              !
   ! Purpose: Do the actual work of rotating winds for C grid.                    !
   !          If idir= 1, rotate grid-relative winds to Earth-relative winds      !
   !          If idir=-1, rotate Earth-relative winds to grid-relative winds      !
   !                                                                              !
   ! ASSUMPTIONS: 1) MEMORY ORDER IS XY.                                          !
   !              2) U ARRAY HAS ONE MORE COLUMN THAN THE V ARRAY, AND V ARRAY    !
   !                 HAS ONE MORE ROW THAN U ARRAY.                               !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine metmap_xform(u, u_mask, v, v_mask, &
                           us1, us2, ue1, ue2, vs1, vs2, ve1, ve2, &
                           xlon_u, xlon_v, xlat_u, xlat_v, idir)

      implicit none

      ! Arguments
      integer, intent(in) :: us1, us2, ue1, ue2, vs1, vs2, ve1, ve2, idir
      real, pointer, dimension(:,:) :: u, v, xlon_u, xlon_v, xlat_u, xlat_v
      type (bitarray), intent(in) :: u_mask, v_mask

      ! Local variables
      integer :: i, j
      real :: u_weight, v_weight
      real :: u_map, v_map, alpha, diff
      real, pointer, dimension(:,:) :: u_new, v_new, u_mult, v_mult
      logical :: do_last_col_u, do_last_row_u, do_last_col_v, do_last_row_v
      real :: x_u, y_u, x_v, y_v
      real :: xlat_u_p1 , xlon_u_p1, xlat_v_p1 , xlon_v_p1
      real :: xlat_u_m1 , xlon_u_m1, xlat_v_m1 , xlon_v_m1
      real :: diff_lon, diff_lat

      ! If the proj_info structure has not been initialized, we don't have
      !   information about the projection and standard longitude.
      if (proj_stack(current_nest_number)%init) then

         ! Only rotate winds for Lambert conformal, polar stereographic, or Cassini
         if ((proj_stack(current_nest_number)%code == PROJ_LC) .or. &
             (proj_stack(current_nest_number)%code == PROJ_PS) .or. &
             (proj_stack(current_nest_number)%code == PROJ_CASSINI)) then
            call mprintf((idir ==  1),LOGFILE,'Rotating map winds to earth winds.')
            call mprintf((idir == -1),LOGFILE,'Rotating earth winds to grid winds')

            allocate(u_mult(us1:ue1,us2:ue2))
            allocate(v_mult(vs1:ve1,vs2:ve2))

            do j=us2,ue2
               do i=us1,ue1
                  if (bitarray_test(u_mask, i-us1+1, j-us2+1)) then
                     u_mult(i,j) = 1.
                  else
                     u_mult(i,j) = 0.
                  end if
               end do
            end do

            do j=vs2,ve2
               do i=vs1,ve1
                  if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                     v_mult(i,j) = 1.
                  else
                     v_mult(i,j) = 0.
                  end if
               end do
            end do

            if (ue1-us1 == ve1-vs1) then
               do_last_col_u = .false.
               do_last_col_v = .true.
            else
               do_last_col_u = .true.
               do_last_col_v = .false.
            end if

            if (ue2-us2 == ve2-vs2) then
               do_last_row_u = .true.
               do_last_row_v = .false.
            else
               do_last_row_u = .false.
               do_last_row_v = .true.
            end if

            ! Create arrays to hold rotated winds
            allocate(u_new(us1:ue1, us2:ue2))
            allocate(v_new(vs1:ve1, vs2:ve2))

            ! Rotate U field
            do j=us2,ue2
               do i=us1,ue1
                  diff = idir * (xlon_u(i,j) - proj_stack(current_nest_number)%stdlon)
                  if (diff > 180.) then
                     diff = diff - 360.
                  else if (diff < -180.) then
                     diff = diff + 360.
                  end if

                  ! Calculate the rotation angle, alpha, in radians
                  if (proj_stack(current_nest_number)%code == PROJ_LC) then
                     alpha = diff * proj_stack(current_nest_number)%cone * rad_per_deg * proj_stack(current_nest_number)%hemi 
                  else if ( (proj_stack(SOURCE_PROJ)%code == PROJ_CASSINI) .and. ( idir == 1 ) ) then
                        call latlon_to_ij ( proj_stack(SOURCE_PROJ) , &
                                            xlat_u(i,j), xlon_u(i,j), x_u, y_u )
                        call ij_to_latlon ( proj_stack(SOURCE_PROJ) , &
                                            x_u, y_u + 0.1 , xlat_u_p1 , xlon_u_p1 )
                        call ij_to_latlon ( proj_stack(SOURCE_PROJ) , &
                                            x_u, y_u - 0.1 , xlat_u_m1 , xlon_u_m1 )
                        diff_lon = xlon_u_p1-xlon_u_m1
                        if (diff_lon > 180.) then
                           diff_lon = diff_lon - 360.
                        else if (diff_lon < -180.) then
                           diff_lon = diff_lon + 360.
                        end if
                        diff_lat = xlat_u_p1-xlat_u_m1
                        alpha =-atan2(   (-cos(xlat_u(i,j)*rad_per_deg) * diff_lon*rad_per_deg),   &
                                                        diff_lat*rad_per_deg    &
                                     )
                  else if ( (proj_stack(current_nest_number)%code == PROJ_CASSINI) .and. ( idir == -1 ) ) then
                        if (j == ue2) then
                           diff = xlon_u(i,j)-xlon_u(i,j-1)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_u(i,j)*rad_per_deg) * diff*rad_per_deg),   &   
                                                           (xlat_u(i,j)-xlat_u(i,j-1))*rad_per_deg    &   
                                        )   
                        else if (j == us2) then
                           diff = xlon_u(i,j+1)-xlon_u(i,j)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_u(i,j)*rad_per_deg) * diff*rad_per_deg),   &   
                                                           (xlat_u(i,j+1)-xlat_u(i,j))*rad_per_deg    &   
                                        )   
                        else
                           diff = xlon_u(i,j+1)-xlon_u(i,j-1)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_u(i,j)*rad_per_deg) * diff*rad_per_deg),   &   
                                                           (xlat_u(i,j+1)-xlat_u(i,j-1))*rad_per_deg    &   
                                        )   
                        end if
                  else
                     alpha = diff * rad_per_deg * proj_stack(current_nest_number)%hemi 
                  end if
                 
                  v_weight = 0.

                  ! On C grid, take U_ij, and get V value at the same lat/lon
                  !   by averaging the four surrounding V points
                  if (bitarray_test(u_mask, i-us1+1, j-us2+1)) then
                     u_map = u(i,j)
                     if (i == us1) then
                        if (j == ue2 .and. do_last_row_u) then
                           v_weight = v_mult(i,j)
                           v_map = v(i,j)*v_mult(i,j)
                        else
                           v_weight = v_mult(i,j) + v_mult(i,j+1)
                           v_map = v(i,j)*v_mult(i,j) + v(i,j+1)*v_mult(i,j+1)
                        end if 
                     else if (i == ue1 .and. do_last_col_u) then
                        if (j == ue2 .and. do_last_row_u) then
                           v_weight = v_mult(i-1,j)
                           v_map = v(i-1,j)
                        else
                           v_weight = v_mult(i-1,j) + v_mult(i-1,j+1) 
                           v_map = v(i-1,j)*v_mult(i-1,j) + v(i-1,j+1)*v_mult(i-1,j+1)
                        end if 
                     else if (j == ue2 .and. do_last_row_u) then
                        v_weight = v_mult(i-1,j-1) + v_mult(i,j-1)
                        v_map = v(i-1,j-1)*v_mult(i-1,j-1) + v(i,j-1)*v_mult(i,j-1)
                     else
                        v_weight = v_mult(i-1,j) + v_mult(i-1,j+1) + v_mult(i,j) + v_mult(i,j+1)
                        v_map = v(i-1,j)*v_mult(i-1,j) + v(i-1,j+1)*v_mult(i-1,j+1) + v(i,j)*v_mult(i,j) + v(i,j+1)*v_mult(i,j+1)
                     end if
                     if (v_weight > 0.) then
                        u_new(i,j) = cos(alpha)*u_map + sin(alpha)*v_map/v_weight
                     else
                        u_new(i,j) = u(i,j)
                     end if
                  else
                     u_new(i,j) = u(i,j)
                  end if

               end do
            end do

            ! Rotate V field
            do j=vs2,ve2
               do i=vs1,ve1

                  diff = idir * (xlon_v(i,j) - proj_stack(current_nest_number)%stdlon)
                  if (diff > 180.) then
                     diff = diff - 360.
                  else if (diff < -180.) then
                     diff = diff + 360.
                  end if

                  if (proj_stack(current_nest_number)%code == PROJ_LC) then
                     alpha = diff * proj_stack(current_nest_number)%cone * rad_per_deg * proj_stack(current_nest_number)%hemi 
                  else if ( (proj_stack(SOURCE_PROJ)%code == PROJ_CASSINI) .and. ( idir == 1 ) ) then
                        call latlon_to_ij ( proj_stack(SOURCE_PROJ) , &
                                            xlat_v(i,j), xlon_v(i,j), x_v, y_v )
                        call ij_to_latlon ( proj_stack(SOURCE_PROJ) , &
                                            x_v, y_v + 0.1 , xlat_v_p1 , xlon_v_p1 )
                        call ij_to_latlon ( proj_stack(SOURCE_PROJ) , &
                                            x_v, y_v - 0.1 , xlat_v_m1 , xlon_v_m1 )
                        diff_lon = xlon_v_p1-xlon_v_m1
                        if (diff_lon > 180.) then
                           diff_lon = diff_lon - 360.
                        else if (diff_lon < -180.) then
                           diff_lon = diff_lon + 360.
                        end if
                        diff_lat = xlat_v_p1-xlat_v_m1
                        alpha =-atan2(   (-cos(xlat_v(i,j)*rad_per_deg) * diff_lon*rad_per_deg),   &
                                                        diff_lat*rad_per_deg    &
                                     )
                  else if ( (proj_stack(current_nest_number)%code == PROJ_CASSINI) .and. ( idir == -1 ) ) then
                        if (j == ve2) then
                           diff = xlon_v(i,j)-xlon_v(i,j-1)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_v(i,j)*rad_per_deg) * diff*rad_per_deg),   &
                                                           (xlat_v(i,j)-xlat_v(i,j-1))*rad_per_deg    &
                                        )
                        else if (j == vs2) then
                           diff = xlon_v(i,j+1)-xlon_v(i,j)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_v(i,j)*rad_per_deg) * diff*rad_per_deg),   &
                                                           (xlat_v(i,j+1)-xlat_v(i,j))*rad_per_deg    &
                                        )
                        else
                           diff = xlon_v(i,j+1)-xlon_v(i,j-1)
                           if (diff > 180.) then
                              diff = diff - 360.
                           else if (diff < -180.) then
                              diff = diff + 360.
                           end if
                           alpha = atan2(   (-cos(xlat_v(i,j)*rad_per_deg) * diff*rad_per_deg),   &
                                                           (xlat_v(i,j+1)-xlat_v(i,j-1))*rad_per_deg    &
                                        )
                        end if
                  else
                     alpha = diff * rad_per_deg * proj_stack(current_nest_number)%hemi 
                  end if

                  u_weight = 0.

                  if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                     v_map = v(i,j)
                     if (j == vs2) then
                        if (i == ve1 .and. do_last_col_v) then
                           u_weight = u_mult(i,j)
                           u_map = u(i,j)*u_mult(i,j)
                        else
                           u_weight = u_mult(i,j) + u_mult(i+1,j)
                           u_map = u(i,j)*u_mult(i,j) + u(i+1,j)*u_mult(i+1,j)
                        end if 
                     else if (j == ve2 .and. do_last_row_v) then
                        if (i == ve1 .and. do_last_col_v) then
                           u_weight = u_mult(i,j-1)
                           u_map = u(i,j-1)*u_mult(i,j-1)
                        else
                           u_weight = u_mult(i,j-1) + u_mult(i+1,j-1)
                           u_map = u(i,j-1)*u_mult(i,j-1) + u(i+1,j-1)*u_mult(i+1,j-1)
                        end if 
                     else if (i == ve1 .and. do_last_col_v) then
                        u_weight = u_mult(i,j) + u_mult(i,j-1)
                        u_map = u(i,j)*u_mult(i,j) + u(i,j-1)*u_mult(i,j-1)
                     else
                        u_weight = u_mult(i,j-1) + u_mult(i,j) + u_mult(i+1,j-1) + u_mult(i+1,j)
                        u_map = u(i,j-1)*u_mult(i,j-1) + u(i,j)*u_mult(i,j) + u(i+1,j-1)*u_mult(i+1,j-1) + u(i+1,j)*u_mult(i+1,j)
                     end if
                     if (u_weight > 0.) then
                        v_new(i,j) = -sin(alpha)*u_map/u_weight + cos(alpha)*v_map
                     else
                        v_new(i,j) = v(i,j)
                     end if
                  else
                     v_new(i,j) = v(i,j)
                  end if

               end do
            end do

            ! Copy rotated winds back into argument arrays
            u = u_new 
            v = v_new 

            deallocate(u_new)
            deallocate(v_new)
            deallocate(u_mult)
            deallocate(v_mult)
         end if

      else
         call mprintf(.true.,ERROR,'In metmap_xform(), uninitialized proj_info structure.')
      end if
 
   end subroutine metmap_xform


   !
   ! NMM Wind Rotation Code
   !

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: map_to_met_nmm                                                         !
   !                                                                              !
   ! Purpose: Rotate grid-relative winds to Earth-relative winds                  !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine map_to_met_nmm(u, u_mask, v, v_mask, &
                             vs1, vs2, ve1, ve2, &
                             xlat_v, xlon_v)

      implicit none

      ! Arguments
      integer, intent(in) :: vs1, vs2, ve1, ve2
      real, pointer, dimension(:,:) :: u, v, xlat_v, xlon_v
      type (bitarray), intent(in) :: u_mask, v_mask

      orig_selected_projection = iget_selected_domain()
      call select_domain(SOURCE_PROJ)
      call metmap_xform_nmm(u, u_mask, v, v_mask, &
                            vs1, vs2, ve1, ve2, &
                            xlat_v, xlon_v, 1)
      call select_domain(orig_selected_projection)

   end subroutine map_to_met_nmm

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: met_to_map_nmm                                                         !
   !                                                                              !
   ! Purpose: Rotate Earth-relative winds to grid-relative winds                  !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine met_to_map_nmm(u, u_mask, v, v_mask, &
                             vs1, vs2, ve1, ve2, &
                             xlat_v, xlon_v)

      implicit none

      ! Arguments
      integer, intent(in) :: vs1, vs2, ve1, ve2
      real, pointer, dimension(:,:) :: u, v, xlat_v, xlon_v
      type (bitarray), intent(in) :: u_mask, v_mask

      orig_selected_projection = iget_selected_domain()
      call select_domain(1)
      call metmap_xform_nmm(u, u_mask, v, v_mask, &
                            vs1, vs2, ve1, ve2, &
                            xlat_v, xlon_v, -1)
      call select_domain(orig_selected_projection)

   end subroutine met_to_map_nmm


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: metmap_xform_nmm                                                       !
   !                                                                              !
   ! Purpose: Do the actual work of rotating winds for E grid.                    !
   !          If idir= 1, rotate grid-relative winds to Earth-relative winds      !
   !          If idir=-1, rotate Earth-relative winds to grid-relative winds      !
   !                                                                              !
   ! ASSUMPTIONS: 1) MEMORY ORDER IS XY.                                          !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine metmap_xform_nmm(u, u_mask, v, v_mask, &
                               vs1, vs2, ve1, ve2, &
                               xlat_v, xlon_v, idir)

      implicit none

      ! Arguments
      integer, intent(in) :: vs1, vs2, ve1, ve2, idir
      real, pointer, dimension(:,:) :: u, v, xlat_v, xlon_v
      type (bitarray), intent(in) :: u_mask, v_mask

      ! Local variables
      integer :: i, j
      real :: u_map, v_map, diff, alpha
      real :: phi0, lmbd0, big_denominator, relm, rlat_v,rlon_v, clontemp
      real :: sin_phi0, cos_phi0,  cos_alpha, sin_alpha
      real, pointer, dimension(:,:) :: u_new, v_new


      ! If the proj_info structure has not been initialized, we don't have
      !   information about the projection and standard longitude.
      if (proj_stack(current_nest_number)%init) then

         if (proj_stack(current_nest_number)%code == PROJ_ROTLL) then

            call mprintf((idir ==  1),LOGFILE,'Rotating map winds to earth winds.')
            call mprintf((idir == -1),LOGFILE,'Rotating earth winds to grid winds')
   
            ! Create arrays to hold rotated winds
            allocate(u_new(vs1:ve1, vs2:ve2))
            allocate(v_new(vs1:ve1, vs2:ve2))

            phi0  = proj_stack(current_nest_number)%lat1 * rad_per_deg

            clontemp= proj_stack(current_nest_number)%lon1

            if (clontemp .lt. 0.) then
               lmbd0 = (clontemp + 360.) * rad_per_deg
            else
               lmbd0 = (clontemp) * rad_per_deg
            endif

            sin_phi0  = sin(phi0)
            cos_phi0  = cos(phi0)

            do j=vs2,ve2
               do i=vs1,ve1

                  ! Calculate the sine and cosine of rotation angle
                  rlat_v = xlat_v(i,j) * rad_per_deg
                  rlon_v = xlon_v(i,j) * rad_per_deg
                  relm = rlon_v - lmbd0
                  big_denominator = cos(asin( &
                                       cos_phi0 * sin(rlat_v) - &
                                       sin_phi0 * cos(rlat_v) * cos(relm) &
                                        )   )

                  sin_alpha = sin_phi0 * sin(relm)  /  &
                                         big_denominator

                  cos_alpha = (cos_phi0 * cos(rlat_v) + &
                               sin_phi0 * sin(rlat_v) * cos(relm))  /  &
                                            big_denominator
   
                  ! Rotate U field
                  if (bitarray_test(u_mask, i-vs1+1, j-vs2+1)) then
                     u_map = u(i,j)
                     if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                        v_map = v(i,j)
                     else
                        v_map = 0.
                     end if
                     
                     u_new(i,j) = cos_alpha*u_map + idir*sin_alpha*v_map
                  else
                     u_new(i,j) = u(i,j)
                  end if
                       
                  ! Rotate V field
                  if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                     v_map = v(i,j)
                     if (bitarray_test(u_mask, i-vs1+1, j-vs2+1)) then
                        u_map = u(i,j)
                     else
                        u_map = 0.
                     end if
                     
                     v_new(i,j) = -idir*sin_alpha*u_map + cos_alpha*v_map
                  else
                     v_new(i,j) = v(i,j)
                  end if
   
               end do
            end do

            ! Copy rotated winds back into argument arrays
            u = u_new 
            v = v_new 
   
            deallocate(u_new)
            deallocate(v_new)
   
         ! Only rotate winds for Lambert conformal, polar stereographic, or Cassini
         else if ((proj_stack(current_nest_number)%code == PROJ_LC) .or. &
                  (proj_stack(current_nest_number)%code == PROJ_PS) .or. &
                  (proj_stack(current_nest_number)%code == PROJ_CASSINI)) then

            call mprintf((idir ==  1),LOGFILE,'Rotating map winds to earth winds.')
            call mprintf((idir == -1),LOGFILE,'Rotating earth winds to grid winds')

            ! Create arrays to hold rotated winds
            allocate(u_new(vs1:ve1, vs2:ve2))
            allocate(v_new(vs1:ve1, vs2:ve2))

            do j=vs2,ve2
               do i=vs1,ve1

                  diff = idir * (xlon_v(i,j) - proj_stack(current_nest_number)%stdlon)
                  if (diff > 180.) then
                     diff = diff - 360.
                  else if (diff < -180.) then
                     diff = diff + 360.
                  end if

                  ! Calculate the rotation angle, alpha, in radians
                  if (proj_stack(current_nest_number)%code == PROJ_LC) then
                     alpha = diff * proj_stack(current_nest_number)%cone * &
                             rad_per_deg * proj_stack(current_nest_number)%hemi
                  else
                     alpha = diff * rad_per_deg * proj_stack(current_nest_number)%hemi 
                  end if

                  ! Rotate U field
                  if (bitarray_test(u_mask, i-vs1+1, j-vs2+1)) then
                     u_map = u(i,j)
                     if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                        v_map = v(i,j)
                     else
                        v_map = 0.
                     end if
                     
                     u_new(i,j) = cos(alpha)*u_map + idir*sin(alpha)*v_map
                  else
                     u_new(i,j) = u(i,j)
                  end if
                       
                  ! Rotate V field
                  if (bitarray_test(v_mask, i-vs1+1, j-vs2+1)) then
                     v_map = v(i,j)
                     if (bitarray_test(u_mask, i-vs1+1, j-vs2+1)) then
                        u_map = u(i,j)
                     else
                        u_map = 0.
                     end if
                     
                     v_new(i,j) = -idir*sin(alpha)*u_map + cos(alpha)*v_map
                  else
                     v_new(i,j) = v(i,j)
                  end if

               end do
            end do

            ! Copy rotated winds back into argument arrays
            u = u_new 
            v = v_new 
   
            deallocate(u_new)
            deallocate(v_new)

         end if

      else
         call mprintf(.true.,ERROR,'In metmap_xform_nmm(), uninitialized proj_info structure.')
      end if

   end subroutine metmap_xform_nmm

end module rotate_winds_module
