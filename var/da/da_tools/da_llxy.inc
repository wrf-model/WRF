subroutine da_llxy (info, loc, outside, outside_all)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   ! This routine converts (lat, lon) into (x,y) coordinates

   implicit none

   type(info_type),       intent(in)    :: info
   type(model_loc_type),  intent(inout) :: loc
   logical      ,         intent(out)   :: outside      !wrt local domain
   logical, optional,     intent(out)   :: outside_all  !wrt all domains

   ! too many return statments to trace
   ! if (trace_use_frequent) call da_trace_entry("da_llxy")

   outside = .false.
   loc % x   = -1.0
   loc % y   = -1.0
   
   ! get the (x, y) coordinates

   if (fg_format == fg_format_wrf_arw_regional) then
      call da_llxy_wrf(map_info, info%lat, info%lon, loc%x, loc%y)
   else if (fg_format == fg_format_wrf_nmm_regional) then
      call da_llxy_rotated_latlon(info%lat, info%lon, map_info, loc%x, loc%y)
   else if (global) then
      call da_llxy_global (info%lat, info%lon, loc%x, loc%y)
   else
      call da_llxy_default (info%lat, info%lon, loc%x, loc%y)
   end if

   call da_togrid (loc%x, its-2, ite+2, loc%i, loc%dx, loc%dxm)!

   call da_togrid (loc%y, jts-2, jte+2, loc%j, loc%dy, loc%dym)
   

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all = .false.
      ! Do not check for global options 
      if (.not. global) then 
         if ((int(loc%x) < ids) .or. (int(loc%x) >= ide) .or. &
            (int(loc%y) < jds) .or. (int(loc%y) >= jde)) then
            outside_all = .true. 
            outside = .true. 
            return
         end if
         if (def_sub_domain) then
            if (x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
                x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
               outside_all = .true.
            outside = .true. 
            return
            end if
         end if
      end if
   end if

   if (global) then
      if ((loc%j < jts-1) .or. (loc%j > jte)) then
         outside = .true.
         return
      end if

    if (fg_format == fg_format_kma_global) then
      if (loc%j == jde) then
         loc%j = loc%j - 1
         loc%dy  = 1.0
         loc%dym = 0.0
      end if
    end if
      return
   end if

   ! Check for edge of domain:

   if ((loc%i < ids) .or. (loc%i >= ide) .or. &
      (loc%j < jds) .or. (loc%j >= jde)) then
      outside     = .true. 
      return
   end if

   ! FIX? hack
   if ((loc%i < its-1) .or. (loc%i > ite) .or. &
      (loc%j < jts-1) .or. (loc%j > jte)) then
   ! if ((loc%i < its-1) .or. (loc%i >= ite) .or. &
   !     (loc%j < jts-1) .or. (loc%j >= jte)) then
      outside = .true.
      return

      if (def_sub_domain) then
         if (x_start_sub_domain > loc%x .or. y_start_sub_domain > loc%y .or. &
             x_end_sub_domain   < loc%x .or. y_end_sub_domain   < loc%y) then
             outside = .true.
         end if
      end if
   end if

   ! if (trace_use_frequent) call da_trace_exit("da_llxy")

end subroutine da_llxy


