subroutine da_outside (locs, outside, outside_all)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   ! Vectorized determination of whether locs are on this domain/patch, copied from da_llxy_new

   implicit none

   type(model_loc_type),   intent(inout) :: locs(:,:)
   logical,           intent(inout)      :: outside(:,:)      ! wrt local domain
   logical, optional, intent(out)        :: outside_all(:,:)  ! wrt all domains

   if (trace_use) call da_trace_entry("da_outside")

   outside(:,:) = .false.

#ifdef A2C
   call da_togrid_new (locs(:,:)%x, its-3, ite+3, locs(:,:)%i, locs(:,:)%dx, locs(:,:)%dxm)!

   call da_togrid_new (locs(:,:)%y, jts-3, jte+3, locs(:,:)%j, locs(:,:)%dy, locs(:,:)%dym)
#else
   call da_togrid_new (locs(:,:)%x, its-2, ite+2, locs(:,:)%i, locs(:,:)%dx, locs(:,:)%dxm)!

   call da_togrid_new (locs(:,:)%y, jts-2, jte+2, locs(:,:)%j, locs(:,:)%dy, locs(:,:)%dym)
#endif

   ! refactor to remove this ugly duplication later
   if (present(outside_all)) then
      outside_all(:,:) = .false.
      ! Do not check for global options 
      if (.not. global) then 
         where ((int(locs(:,:)%x) < ids) .or. (int(locs(:,:)%x) >= ide) .or. &
            (int(locs(:,:)%y) < jds) .or. (int(locs(:,:)%y) >= jde))
            outside_all(:,:) = .true. 
            outside(:,:) = .true. 
         end where
         if (def_sub_domain) then
            where (x_start_sub_domain > locs(:,:)%x .or. y_start_sub_domain > locs(:,:)%y .or. &
                x_end_sub_domain   < locs(:,:)%x .or. y_end_sub_domain   < locs(:,:)%y)
               outside_all(:,:) = .true.
               outside(:,:) = .true. 
            end where
         end if
      end if
   end if

   if (fg_format == fg_format_kma_global) then
      where ((locs(:,:)%j < jts-1) .or. (locs(:,:)%j  > jte))
         outside(:,:) = .true.
      end where

      where (locs(:,:)%j == jde)
         locs(:,:)%j = locs(:,:)%j - 1
         locs(:,:)%dy  = 1.0
         locs(:,:)%dym = 0.0
      end where

      return
   end if

   ! Check for edge of domain:

   where ((locs(:,:)%i < ids) .or. (locs(:,:)%i >= ide) .or. &
      (locs(:,:)%j < jds) .or. (locs(:,:)%j >= jde))
      outside     = .true. 
   end where

   ! FIX? hack
#ifdef A2C
   where ((locs(:,:)%i < its-2) .or. (locs(:,:)%i > ite) .or. &
      (locs(:,:)%j < jts-2) .or. (locs(:,:)%j > jte))
#else
   where ((locs(:,:)%i < its-1) .or. (locs(:,:)%i > ite) .or. &
      (locs(:,:)%j < jts-1) .or. (locs(:,:)%j > jte))
#endif
      outside(:,:) = .true.
   end where

   if (def_sub_domain) then
      where (x_start_sub_domain > locs(:,:)%x .or. y_start_sub_domain > locs(:,:)%y .or. &
             x_end_sub_domain   < locs(:,:)%x .or. y_end_sub_domain   < locs(:,:)%y)
         outside(:,:) = .true.
      end where 
   end if

   if (trace_use) call da_trace_exit("da_outside")

end subroutine da_outside


