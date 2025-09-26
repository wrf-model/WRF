!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE LLXY_MODULE
!
! This module handles transformations between model grid coordinates and 
!   latitude-longitude coordinates. The actual transformations are done through
!   the map_utils module. 
! 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module llxy_module

   use gridinfo_module
   use list_module
   use map_utils
   use module_debug
   use misc_definitions_module
 
   ! Parameters
   integer, parameter :: MAX_SOURCE_LEVELS = 20
 
   ! Variables
   integer :: current_nest_number
   integer :: SOURCE_PROJ = 0
   ! The following arrays hold values for all available domains 
   ! NOTE: The entries in the arrays for "domain 0" are used for projection
   !       information of user-specified source data
   type (proj_info), dimension(-MAX_SOURCE_LEVELS:MAX_DOMAINS) :: proj_stack
 
   ! The projection and domain that we have computed constants for
   integer :: computed_proj = INVALID
   integer :: computed_domain = INVALID
 
   contains
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: push_source_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine push_source_projection(iprojection, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_dlat, user_dlon, user_known_x, &
                                  user_known_y, user_known_lat, user_known_lon, &
                                  user_pole_lat, user_pole_lon, &   
                                  user_centerlat, user_centerlon, &   
                                  user_centeri, user_centerj, &   
                                  earth_radius)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: iprojection
      real, intent(in) :: user_stand_lon, user_truelat1, user_truelat2, user_dxkm, user_dykm, &
                          user_dlat, user_dlon, &
                          user_known_x, user_known_y, user_known_lat, user_known_lon
      real, intent(in), optional :: earth_radius
      real, intent(in), optional :: user_centerlon, user_centerlat, user_pole_lat, user_pole_lon 
      real, intent(in), optional :: user_centerj, user_centeri   
      

      SOURCE_PROJ = SOURCE_PROJ-1
      if (SOURCE_PROJ < -MAX_SOURCE_LEVELS) then
         call mprintf(.true.,ERROR,'In push_user_projection(), too many levels of user projections.')
      end if
  
      call map_init(proj_stack(SOURCE_PROJ))

      if (iprojection == PROJ_LATLON) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      nxmax=nint(360.0 / user_dlon), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_MERC) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CYL) then
         call mprintf(.true.,ERROR,'Should not have PROJ_CYL as projection for ' &
                          //'source data in push_source_projection()')
  
      else if (iprojection == PROJ_CASSINI) then
  
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      stdlon=user_stand_lon, &  
                      lat1=user_centerlat, &
                      lon1=user_centerlon, &
                      lat0=user_pole_lat, &
                      lon0=user_pole_lon, &
                      knowni=user_centeri, &
                      knownj=user_centerj, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_LC) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_ALBERS_NAD83) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_PS) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_PS_WGS84) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_GAUSS) then
         call map_set(iprojection, proj_stack(SOURCE_PROJ), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      nxmax=nint(360.0 / user_dlon), &
                      nlat=nint(user_dlat), &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ROTLL) then
  ! BUG: Implement this projection.
  
      end if
     
   end subroutine push_source_projection
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: pop_source_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine pop_source_projection()
 
      implicit none
  
      SOURCE_PROJ = SOURCE_PROJ+1
      
      call mprintf((SOURCE_PROJ > 0), ERROR, &
                   'In pop_user_projection(), projection stack has overflowed.')
 
   end subroutine pop_source_projection
 
 
#ifdef _METGRID
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: set_domain_projection
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine set_domain_projection(iprojection, user_stand_lon, user_truelat1, user_truelat2, &
                                  user_dxkm, user_dykm, user_dlat, user_dlon, &
                                  user_xdim, user_ydim, user_known_x, &
                                  user_known_y, user_known_lat, user_known_lon, &
                                  user_pole_lat, user_pole_lon, earth_radius)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: iprojection
      integer, intent(in) :: user_xdim, user_ydim
      real, intent(in) :: user_stand_lon, user_truelat1, user_truelat2, &
                          user_dxkm, user_dykm, user_dlat, user_dlon, &
                          user_known_x, user_known_y, user_known_lat, user_known_lon, &
                          user_pole_lat, user_pole_lon
      real, intent(in), optional :: earth_radius
  
      current_nest_number = 1

      call map_init(proj_stack(current_nest_number))
  
      if (iprojection == PROJ_LATLON) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_MERC) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CYL) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      stdlon=user_stand_lon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_CASSINI) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      latinc=user_dlat, &
                      loninc=user_dlon, &
                      dx=user_dxkm,        &
                      dy=user_dykm,        &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      lat0=user_pole_lat, &
                      lon0=user_pole_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_LC) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ALBERS_NAD83) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      truelat2=user_truelat2, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_PS) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm, &
                      r_earth=earth_radius)

      else if (iprojection == PROJ_PS_WGS84) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      truelat1=user_truelat1, &
                      stdlon=user_stand_lon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      knowni=user_known_x, &
                      knownj=user_known_y, &
                      dx=user_dxkm)
  
      else if (iprojection == PROJ_GAUSS) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      nlat=nint(user_dlat), &
                      loninc=user_dlon, &
                      r_earth=earth_radius)
  
      else if (iprojection == PROJ_ROTLL) then
         call map_set(iprojection, proj_stack(current_nest_number), &
                      ixdim=user_xdim, &
                      jydim=user_ydim, &
                      phi=user_dlat, &
                      lambda=user_dlon, &
                      lat1=user_known_lat, &
                      lon1=user_known_lon, &
                      stagger=HH, &
                      latinc=user_dykm, &
                      loninc=user_dxkm, &
                      r_earth=earth_radius)
  
      end if
     
   end subroutine set_domain_projection
#endif


#ifdef _GEOGRID
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: compute_nest_locations
   !
   ! Purpose: This routine computes the variables necessary in determining the 
   !   location of all nests without reference to the parent or coarse domains.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine compute_nest_locations()
 
      implicit none
  
      ! Local variables
      integer :: i
      real :: temp_known_x, temp_known_y, temp_known_lat, temp_known_lon, &
              temp_dxkm, temp_dykm, temp_dlat, temp_dlon
  
      ! Set location of coarse/mother domain
      call map_init(proj_stack(1))
  
      if (iproj_type == PROJ_LATLON) then
         call map_set(iproj_type, proj_stack(1), &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      latinc=dykm, &
                      loninc=dxkm)
   
      else if (iproj_type == PROJ_MERC) then
         call map_set(iproj_type, proj_stack(1), &
                      truelat1=truelat1, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      dx=dxkm)
  
      else if (iproj_type == PROJ_CYL) then
         call map_set(iproj_type, proj_stack(1), &
                      latinc=dlatdeg, &
                      loninc=dlondeg, &
                      stdlon=stand_lon)
  
      else if (iproj_type == PROJ_CASSINI) then
         call map_set(iproj_type, proj_stack(1), &
                      latinc=dlatdeg, &
                      loninc=dlondeg, &
                      dx=dxkm,       &
                      dy=dykm,       &
                      stdlon=stand_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      lat0=pole_lat, &
                      lon0=pole_lon, &
                      lat1=known_lat, &
                      lon1=known_lon)
  
      else if (iproj_type == PROJ_LC) then
         call map_set(iproj_type, proj_stack(1), &
                      truelat1=truelat1, &
                      truelat2=truelat2, &
                      stdlon=stand_lon, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      dx=dxkm)
  
      else if (iproj_type == PROJ_ALBERS_NAD83) then
         call map_set(iproj_type, proj_stack(1), &
                      truelat1=truelat1, &
                      truelat2=truelat2, &
                      stdlon=stand_lon, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      dx=dxkm)
  
      else if (iproj_type == PROJ_PS) then
         call map_set(iproj_type, proj_stack(1), &
                      truelat1=truelat1, &
                      stdlon=stand_lon, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      dx=dxkm)

      else if (iproj_type == PROJ_PS_WGS84) then
         call map_set(iproj_type, proj_stack(1), &
                      truelat1=truelat1, &
                      stdlon=stand_lon, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      knowni=known_x, &
                      knownj=known_y, &
                      dx=dxkm)
  
      else if (iproj_type == PROJ_GAUSS) then
         call map_set(iproj_type, proj_stack(current_nest_number), &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      nlat=nint(dykm), &
                      loninc=dxkm)
  
      else if (iproj_type == PROJ_ROTLL) then
         call map_set(iproj_type, proj_stack(1), &
                      ixdim=ixdim(1), &
                      jydim=jydim(1), &
                      phi=phi, &
                      lambda=lambda, &
                      lat1=known_lat, &
                      lon1=known_lon, &
                      latinc=dykm, &
                      loninc=dxkm, &
                      stagger=HH)
   
      end if
  
      ! Now we can compute lat/lon <-> x/y for coarse domain
      call select_domain(1)
  
      ! Call a recursive procedure to find the lat/lon of the centerpoint for 
      !   each domain
      do i=2,n_domains
  
         temp_known_x = real(ixdim(i))/2.
         temp_known_y = real(jydim(i))/2.

         call find_known_latlon(i, temp_known_x, temp_known_y, &
                                temp_known_lat, temp_known_lon, &
                                temp_dxkm, temp_dykm, temp_dlat, temp_dlon)
   
         if (iproj_type == PROJ_LATLON) then
            call map_set(iproj_type, proj_stack(i), &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         latinc=temp_dlat, &
                         loninc=temp_dlon)
   
         else if (iproj_type == PROJ_MERC) then
            call map_set(iproj_type, proj_stack(i), &
                         truelat1=truelat1, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         dx=temp_dxkm)
    
         else if (iproj_type == PROJ_CYL) then
            call mprintf(.true.,ERROR,'Don''t know how to do nesting with PROJ_CYL ' &
                                      //'in compute_nest_locations()')
  
         else if (iproj_type == PROJ_CASSINI) then
            call map_set(iproj_type, proj_stack(i), &
                         latinc=temp_dlat, &
                         loninc=temp_dlon, &
                         dx=temp_dxkm,  &
                         dy=temp_dykm,  &
                         stdlon=stand_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         lat0=pole_lat, &
                         lon0=pole_lon, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon)
    
         else if (iproj_type == PROJ_LC) then
            call map_set(iproj_type, proj_stack(i), &
                         truelat1=truelat1, &
                         truelat2=truelat2, &
                         stdlon=stand_lon, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         dx=temp_dxkm)
    
         else if (iproj_type == PROJ_ALBERS_NAD83) then
            call map_set(iproj_type, proj_stack(i), &
                         truelat1=truelat1, &
                         truelat2=truelat2, &
                         stdlon=stand_lon, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         dx=temp_dxkm)
   
         else if (iproj_type == PROJ_PS) then
            call map_set(iproj_type, proj_stack(i), &
                         truelat1=truelat1, &
                         stdlon=stand_lon, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         dx=temp_dxkm)

         else if (iproj_type == PROJ_PS_WGS84) then
            call map_set(iproj_type, proj_stack(i), &
                         truelat1=truelat1, &
                         stdlon=stand_lon, &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         knowni=temp_known_x, &
                         knownj=temp_known_y, &
                         dx=temp_dxkm)
   
         else if (iproj_type == PROJ_GAUSS) then
            call map_set(iproj_type, proj_stack(current_nest_number), &
                         lat1=temp_known_lat, &
                         lon1=temp_known_lon, &
                         nlat=nint(temp_dykm), &
                         loninc=temp_dxkm)
   
         else if (iproj_type == PROJ_ROTLL) then
   ! BUG: Implement this projection.
   
         end if
  
      end do
 
   end subroutine compute_nest_locations
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: find_known_latlon
   !
   ! Purpose: This recursive routine computes the latitude and longitude for a 
   !   specified x/y location in the given nest number, and also computes the
   !   grid spacing
   !
   ! NOTE: This routine assumes that xytoll will work correctly for the 
   !       coarse domain.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   recursive subroutine find_known_latlon(n, rx, ry, rlat, rlon, dx, dy, dlat, dlon)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: n
      real, intent(in) :: rx, ry
      real, intent(out) :: rlat, rlon, dx, dy, dlat, dlon
  
      ! Local variables
      real :: x_in_parent, y_in_parent
  
      if (n == 1) then   ! Stopping case for the recursion
  
         dx = dxkm 
         dy = dykm 
         dlat = dlatdeg 
         dlon = dlondeg 
         call ij_to_latlon(proj_stack(current_nest_number), rx, ry, rlat, rlon)
  
         return
  
      else               ! Recursive case
   
         x_in_parent = (rx - ((parent_grid_ratio(n)+1.)/2.)) &
                      / parent_grid_ratio(n) + parent_ll_x(n)
         y_in_parent = (ry - ((parent_grid_ratio(n)+1.)/2.)) &
                      / parent_grid_ratio(n) + parent_ll_y(n)
   
         call find_known_latlon(parent_id(n), x_in_parent, y_in_parent, rlat, rlon, dx, dy, dlat, dlon)
   
         dx = dx / parent_grid_ratio(n)
         dy = dy / parent_grid_ratio(n)
         dlat = dlat / parent_grid_ratio(n)
         dlon = dlon / parent_grid_ratio(n)
      end if 
 
   end subroutine find_known_latlon


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: compute_nest_level_info
   !
   ! Purpose: This routine computes the parameters describing a nesting level for 
   !          NMM grids.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine compute_nest_level_info()

      implicit none

      ! Local variables
      integer :: i, nest_level, temp
      type (list) :: level_list 

      call list_init(level_list)

      ! Set location of coarse/mother domain
      call map_init(proj_stack(1))

      call map_set(PROJ_ROTLL, proj_stack(1), &
                   ixdim=ixdim(1), &
                   jydim=jydim(1), &
                   phi=phi, &
                   lambda=lambda, &
                   lat1=known_lat, &
                   lon1=known_lon, &
                   latinc=dykm, &
                   loninc=dxkm, &
                   stagger=HH)

      parent_ur_x(1) = real(ixdim(1))
      parent_ur_y(1) = real(jydim(1))

      do i=2,n_domains

         nest_level = get_nest_level(i)

         if (.not. list_search(level_list, ikey=nest_level, ivalue=temp)) then

            call list_insert(level_list, ikey=nest_level, ivalue=nest_level)

            ixdim(nest_level) = ixdim(1)*(3**(nest_level-1))-(3**(nest_level-1)-1)
            jydim(nest_level) = jydim(1)*(3**(nest_level-1))-(3**(nest_level-1)-1)

            parent_ur_x(nest_level) = ixdim(nest_level)
            parent_ur_y(nest_level) = jydim(nest_level)

            call map_set(PROJ_ROTLL, proj_stack(nest_level), &
                         ixdim = ixdim(nest_level), &
                         jydim = jydim(nest_level), &
                         phi    = phi, &
                         lambda = lambda, &
                         lat1=known_lat, &
                         lon1=known_lon, &
                         latinc=(dykm/real((3**(nest_level-1)))), &
                         loninc=(dxkm/real((3**(nest_level-1)))), &
                         stagger=HH)
         end if

      end do

      call list_destroy(level_list)

   end subroutine compute_nest_level_info

   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_domain_resolution
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_domain_resolution(dom_dx, dom_dy)

      implicit none

      ! Arguments
      real, intent(out) :: dom_dx, dom_dy

      ! The proj_info structure only stores dx, so set both dom_dx and dom_dy to dx
      dom_dx = proj_stack(current_nest_number)%dx
      dom_dy = proj_stack(current_nest_number)%dx

   end subroutine get_domain_resolution


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_nest_level
   !
   ! Purpose: This function returns, given a grid ID number, the nesting level of
   !   that domain; the coarse domain is taken to have nesting level 1.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   function get_nest_level(i)
      
      implicit none

      ! Arguments
      integer, intent(in) :: i

      ! Local variables
      integer :: j

      ! Return value
      integer :: get_nest_level

      ! If argument is the coarse domain, return
      if (i == 1) then
         get_nest_level = 1
         return
      end if

      if (i > MAX_DOMAINS) then
         call mprintf(.true., ERROR, &
                      'get_nest_level() called with invalid grid ID of %i.',i1=i)
      end if

      ! If not the coarse domain, then nesting level is at least 2
      ! Yes, this looks silly. But we do not have a grid_id array, so
      !    we must check on parent_id
      get_nest_level = 2

      j = i
      do while (parent_id(j) /= 1)
         j = parent_id(j)
         get_nest_level = get_nest_level + 1
         
         ! Sanity check
         if (get_nest_level > MAX_DOMAINS) then
            call mprintf(.true., ERROR, &
                         'Spooky nesting setup encountered in get_nest_level().')
         end if
      end do

   end function get_nest_level
#endif


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: select_domain
   !
   ! Purpose: This routine is used to select which nest x/y <-> lat/lon 
   !   conversions will be with respect to. For example, selecting domain 2 will
   !   cause the llxy routine to compute x/y locations with respect to domain 2
   !   given a lat/lon.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine select_domain(domain_num)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: domain_num
  
#ifdef _GEOGRID
      if (domain_num > n_domains) then
         call mprintf(.true.,ERROR,'In select_domain(), selected domain is greater than n_domains.')
      end if
#endif
#ifdef _METGRID
      if (domain_num > 1) then
         call mprintf(.true.,ERROR,'In select_domain(), selected domain is greater than 1.')
      end if
#endif
  
      current_nest_number = domain_num
 
   end subroutine select_domain
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: iget_selected_domain
   !
   ! Purpose: This function returns the number of the currently selected nest. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   function iget_selected_domain()
 
      implicit none
  
      ! Return value
      integer :: iget_selected_domain
      
      iget_selected_domain = current_nest_number
 
   end function iget_selected_domain 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: lltoxy
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine lltoxy(xlat, xlon, x, y, stagger, comp_ll)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: stagger
      real, intent(in) :: xlat, xlon
      real, intent(out) :: x, y
      logical, optional, intent(in) :: comp_ll

      ! Local variables
      logical :: save_comp_ll
  
      ! Account for grid staggering
      if (stagger == HH) then
         proj_stack(current_nest_number)%stagger = HH
      else if (stagger == VV) then
         proj_stack(current_nest_number)%stagger = VV
      end if
  
      if (present(comp_ll)) then
         save_comp_ll = proj_stack(current_nest_number)%comp_ll
         proj_stack(current_nest_number)%comp_ll = comp_ll
      end if

      call latlon_to_ij(proj_stack(current_nest_number), xlat, xlon, x, y)

      if (present(comp_ll)) then
         proj_stack(current_nest_number)%comp_ll = save_comp_ll
      end if
  
      ! Account for grid staggering
      if (stagger == U) then
         x = x + 0.5
      else if (stagger == V) then
         y = y + 0.5
      else if (stagger == CORNER) then
         x = x + 0.5
         y = y + 0.5
      end if
 
   end subroutine lltoxy
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: lltoxy
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine xytoll(x, y, xlat, xlon, stagger, comp_ll)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: stagger
      real, intent(in) :: x, y
      real, intent(out) :: xlat, xlon
      logical, optional, intent(in) :: comp_ll

      ! Local variables
      real :: rx, ry
      logical :: save_comp_ll
  
      ! Account for grid staggering; we cannot modify x and y, so modify local
      !   copies of them
      if (stagger == U) then
         rx = x - 0.5
         ry = y
      else if (stagger == V) then
         rx = x
         ry = y - 0.5
      else if (stagger == HH) then
         proj_stack(current_nest_number)%stagger = HH
         rx = x
         ry = y
      else if (stagger == VV) then
         proj_stack(current_nest_number)%stagger = VV
         rx = x
         ry = y
      else if (stagger == CORNER) then
         proj_stack(current_nest_number)%stagger = CORNER
         rx = x - 0.5
         ry = y - 0.5
      else
         rx = x
         ry = y
      end if

      if (present(comp_ll)) then
         save_comp_ll = proj_stack(current_nest_number)%comp_ll
         proj_stack(current_nest_number)%comp_ll = comp_ll
      end if
  
      call ij_to_latlon(proj_stack(current_nest_number), rx, ry, xlat, xlon)

      if (present(comp_ll)) then
         proj_stack(current_nest_number)%comp_ll = save_comp_ll
      end if
 
   end subroutine xytoll

end module llxy_module
