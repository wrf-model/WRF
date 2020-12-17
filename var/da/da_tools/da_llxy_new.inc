subroutine da_llxy_new (info, outside, outside_all)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   ! This routine converts (lat, lon) into (x,y) coordinates

   implicit none

   type(infa_type),   intent(inout) :: info
   logical,           intent(inout) :: outside(:,:)      ! wrt local domain
   logical, optional, intent(out)   :: outside_all(:,:)  ! wrt all domains

   if (trace_use) call da_trace_entry("da_llxy_new")

   outside(:,:) = .false.
   info%x(:,:)   = -1.0
   info%y(:,:)   = -1.0
   
   ! get the (x, y) coordinates

   if (fg_format == fg_format_wrf_arw_regional) then
      call da_llxy_wrf_new(map_info, info)
   else if (fg_format == fg_format_wrf_nmm_regional) then
      write(unit=message(1),fmt='(A,I5)') &
         "Needs to be developed for fg_format_nmm_regional = ",fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
   else if (global) then
      call da_llxy_global_new (info)
   else
      call da_llxy_default_new (info)
   end if

   call da_togrid_new (info%x, its-2, ite+2, info%i, info%dx, info%dxm)
   call da_togrid_new (info%y, jts-2, jte+2, info%j, info%dy, info%dym)

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all(:,:) = .false.
      ! Do not check for global options 
      if (.not. global) then 
         where ((int(info%x(:,:)) < ids) .or. (int(info%x(:,:)) >= ide) .or. &
            (int(info%y(:,:)) < jds) .or. (int(info%y(:,:)) >= jde))
            outside_all(:,:) = .true. 
            outside(:,:) = .true. 
         end where
         if (def_sub_domain) then
            where (x_start_sub_domain > info%x(:,:) .or. y_start_sub_domain > info%y(:,:) .or. &
                x_end_sub_domain   < info%x(:,:) .or. y_end_sub_domain   < info%y(:,:))
               outside_all(:,:) = .true.
               outside(:,:) = .true. 
            end where
         end if
      end if
   end if

   if (fg_format == fg_format_kma_global) then
      where ((info%j(:,:) < jts-1) .or. (info%j(:,:)  > jte))
         outside(:,:) = .true.
      end where

      where (info%j(:,:) == jde)
         info%j(:,:) = info%j(:,:) - 1
         info%dy(:,:)  = 1.0
         info%dym(:,:) = 0.0
      end where

      return
   end if

   ! Check for edge of domain:

   where ((info%i(:,:) < ids) .or. (info%i(:,:) >= ide) .or. &
      (info%j(:,:) < jds) .or. (info%j(:,:) >= jde))
      outside     = .true. 
   end where

   ! FIX? hack
   where ((info%i(:,:) < its-1) .or. (info%i(:,:) > ite) .or. &
      (info%j(:,:) < jts-1) .or. (info%j(:,:) > jte))
      outside(:,:) = .true.
   end where

   if (def_sub_domain) then
      where (x_start_sub_domain > info%x(:,:) .or. y_start_sub_domain > info%y(:,:) .or. &
             x_end_sub_domain   < info%x(:,:) .or. y_end_sub_domain   < info%y(:,:))
         outside = .true.
      end where 
   end if

   if (trace_use) call da_trace_exit("da_llxy_new")

end subroutine da_llxy_new


