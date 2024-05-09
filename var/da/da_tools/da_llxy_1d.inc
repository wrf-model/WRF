subroutine da_llxy_1d ( infos, locs, outside, outside_all, do_xy, do_outside)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Author: JJ Guerrette,  MMM/NCAR,  Date: 05/23/2018
   !    Modified from da_llxy, including child subroutines
   !-----------------------------------------------------------------------

   ! This routine converts (lat, lon) into (x,y) coordinates

   implicit none

   type(info_type), optional, intent(in)    :: infos(:)
   type(model_loc_type),      intent(inout) :: locs(:)
   logical      ,             intent(out)   :: outside(:)      !wrt local domain
   logical, optional,         intent(out)   :: outside_all(:)  !wrt all domains
   logical, optional,         intent(in)    :: do_xy, do_outside
   logical :: do_xy_, do_outside_

   if (trace_use) call da_trace_entry("da_llxy_1d")

   outside = .false.

   do_xy_ = .true.
   if ( present(do_xy) ) do_xy_ = do_xy
   if ( do_xy_ )  then
      if (present(infos)) then
         locs(:) % x   = -1.0
         locs(:) % y   = -1.0
   
         ! get the (x, y) coordinates
         if ( fg_format == fg_format_wrf_arw_regional ) then
            call da_llxy_wrf_1d(map_info, infos(:)%lat, infos(:)%lon, locs(:)%x, locs(:)%y)
         else if (fg_format == fg_format_wrf_nmm_regional) then
            call da_llxy_rotated_latlon_1d(infos(:)%lat, infos(:)%lon, map_info, locs(:)%x, locs(:)%y)
         else if (global) then
            call da_llxy_global_1d (infos(:)%lat, infos(:)%lon, locs(:)%x, locs(:)%y)
         else
            call da_llxy_default_1d (infos(:)%lat, infos(:)%lon, locs(:)%x, locs(:)%y)
         end if
      else
         message(1)='da_llxy_1d requires infos in order to determine x & y'
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

#ifdef A2C
   call da_togrid_1d (locs(:)%x, its-3, ite+3, locs(:)%i, locs(:)%dx, locs(:)%dxm)!

   call da_togrid_1d (locs(:)%y, jts-3, jte+3, locs(:)%j, locs(:)%dy, locs(:)%dym)
#else
   call da_togrid_1d (locs(:)%x, its-2, ite+2, locs(:)%i, locs(:)%dx, locs(:)%dxm)!

   call da_togrid_1d (locs(:)%y, jts-2, jte+2, locs(:)%j, locs(:)%dy, locs(:)%dym)
#endif

!   do_outside_ = .true.
!   if ( present(do_outside) ) do_outside_ = do_outside
!   if ( .not.do_outside_ )  return

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all(:) = .false.
      ! Do not check for global options 
      if (.not. global) then 
         outside_all = outside_all .or. &
            (int(locs(:)%x) < ids) .or. (int(locs(:)%x) >= ide) .or. &
            (int(locs(:)%y) < jds) .or. (int(locs(:)%y) >= jde)
         outside = outside .or. outside_all
         if (def_sub_domain) then
            outside_all = outside_all .or. &
               x_start_sub_domain > locs(:)%x .or. y_start_sub_domain > locs(:)%y .or. &
               x_end_sub_domain   < locs(:)%x .or. y_end_sub_domain   < locs(:)%y
            outside = outside .or. outside_all
         end if
      end if
   end if

   if (fg_format == fg_format_kma_global) then
      outside = outside .or. &
         (locs(:)%j < jts-1) .or. (locs(:)%j  > jte)

      where (locs(:)%j == jde)
         locs%j   = locs%j - 1
         locs%dy  = 1.0
         locs%dym = 0.0
      end where

      return
   end if

   ! Check for edge of domain:
   outside = outside .or. &
      (locs(:)%i < ids) .or. (locs(:)%i >= ide) .or. &
      (locs(:)%j < jds) .or. (locs(:)%j >= jde)

   ! FIX? hack
   outside = outside .or. &
#ifdef A2C
      (locs(:)%i < its-2) .or. (locs(:)%i > ite) .or. &
         (locs(:)%j < jts-2) .or. (locs(:)%j > jte)
#else
      (locs(:)%i < its-1) .or. (locs(:)%i > ite) .or. &
         (locs(:)%j < jts-1) .or. (locs(:)%j > jte)
#endif

   if (def_sub_domain) then
      outside = outside .or. &
         x_start_sub_domain > locs(:)%x .or. y_start_sub_domain > locs(:)%y .or. &
         x_end_sub_domain   < locs(:)%x .or. y_end_sub_domain   < locs(:)%y
   end if

   if (trace_use) call da_trace_exit("da_llxy_1d")

end subroutine da_llxy_1d
