subroutine da_map_set(proj_code,lat1,lon1,knowni,knownj,dx,stdlon,truelat1,truelat2,latinc,loninc,proj)
   ! Given a partially filled proj_info structure, this routine computes
   ! polei, polej, rsw, and cone (if LC projection) to complete the 
   ! structure.  This allows us to eliminate redundant calculations when
   ! calling the coordinate conversion routines multiple times for the
   ! same map.
   ! This will generally be the first routine called when a user wants
   ! to be able to use the coordinate conversion routines, and it
   ! will call the appropriate subroutines based on the 
   ! proj%code which indicates which projection type  this is.

   implicit none
   
   integer,         intent(in)  :: proj_code
   real,            intent(in)  :: lat1
   real,            intent(in)  :: lon1
   real,            intent(in)  :: dx
   real,            intent(in)  :: stdlon
   real,            intent(in)  :: truelat1
   real,            intent(in)  :: truelat2
   real,            intent(in)  :: knowni , knownj
   real,            intent(in)  :: latinc, loninc 
   type(proj_info), intent(out) :: proj

   if (trace_use_dull) call da_trace_entry("da_map_set")

   ! First, check for validity of mandatory variables in proj
   if (ABS(lat1) > 90.0) then
      call da_error(__FILE__,__LINE__, &
         (/"Latitude of origin corner required as follows: -90N <= lat1 < = 90N"/))
   end if
   if (ABS(lon1) > 180.0) then
      call da_error(__FILE__,__LINE__, &
         (/"Longitude of origin required as follows: -180E <= lon1 <= 180W"/))
   end if
   if ((dx .LE. 0.0).AND.(proj_code .NE. PROJ_LATLON)) then
      call da_error(__FILE__,__LINE__, &
         (/"Require grid spacing (dx) in meters be positive!"/))
   end if
   if ((ABS(stdlon) > 180.0).AND.(proj_code .NE. PROJ_MERC)) then
      call da_error(__FILE__,__LINE__, &
         (/"Need orientation longitude (stdlon) as: -180E <= lon1 <= 180W"/)) 
   end if
   if (proj%code .NE. PROJ_LATLON .and. ABS(truelat1)>90.0) then
      call da_error(__FILE__,__LINE__, &
         (/"Set true latitude 1 for all projections!"/))
   end if
   
   call da_map_init(proj) 
   proj%code  = proj_code
   proj%lat1 = lat1
   proj%lon1 = lon1
   proj%knowni = knowni
   proj%knownj = knownj
   proj%dx    = dx
   proj%stdlon = stdlon
   proj%truelat1 = truelat1
   proj%truelat2 = truelat2
   proj%latinc   = latinc  
   proj%loninc   = loninc  
   if (proj%code .NE. PROJ_LATLON) then
      proj%dx = dx
      if (truelat1 < 0.0) then
         proj%hemi = -1.0 
      else
         proj%hemi = 1.0
      end if
      proj%rebydx = earth_radius_m / dx
   end if
   pick_proj: select case(proj%code)

      case(PROJ_PS)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up POLAR STEREOGRAPHIC map...'
         end if
         call da_set_ps(proj)

      case(PROJ_LC)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up LAMBERT CONFORMAL map...'
         end if
         if (ABS(proj%truelat2) > 90.0) then
            if (print_detail_map) then
               write(unit=stdout,fmt='(A)') 'Second true latitude not set, assuming a tangent'
               write(unit=stdout,fmt='(A,F10.3)') 'projection at truelat1: ', proj%truelat1
            end if
            proj%truelat2=proj%truelat1
         end if
         call da_set_lc(proj)
   
      case (PROJ_MERC)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up MERCATOR map...'
         end if
         call da_set_merc(proj)
   
      case (PROJ_LATLON)
         if (print_detail_map) then
            write(unit=stdout,fmt='(A)') 'Setting up CYLINDRICAL EQUIDISTANT LATLON map...'
         end if
         ! Convert lon1 to 0->360 notation
         if (proj%lon1 < 0.0) proj%lon1 = proj%lon1 + 360.0
   
         proj%cone = 1.0                                  
      case default
         write(unit=message(1),fmt='(A,I2)') 'Unknown projection code: ', proj%code
         call da_error(__FILE__,__LINE__,message(1:1))
   end select pick_proj
   proj%init = .true.

   if (trace_use_dull) call da_trace_exit("da_map_set")

end subroutine da_map_set


