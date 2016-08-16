subroutine da_llxy_wrf_new(proj, info)

   !-----------------------------------------------------------------------
   ! Purpose: Converts input lat/lon values to the cartesian (x,y) value
   ! for the given projection. 
   !-----------------------------------------------------------------------

   implicit none

   type(proj_info), intent(in)    :: proj
   type(infa_type), intent(inout) :: info

   if (trace_use) call da_trace_entry("da_llxy_wrf_new")

   if (.NOT.proj%init) then
      call da_error(__FILE__,__LINE__, &
        (/"You have not called map_set for this projection!"/))
   end if

   select case(proj%code)
 
      case(PROJ_LATLON)
         call da_llxy_latlon_new(proj,info)

      case(PROJ_MERC)
         call da_llxy_merc_new(proj,info)
         info%x(:,:) = info%x(:,:) + proj%knowni - 1.0
         info%y(:,:) = info%y(:,:) + proj%knownj - 1.0

      case(PROJ_PS)
         call da_llxy_ps_new(proj,info)
      
      case(PROJ_LC)
         call da_llxy_lc_new(proj,info)
         info%x(:,:) = info%x(:,:) + proj%knowni - 1.0
         info%y(:,:) = info%y(:,:) + proj%knownj - 1.0

      case default
         write(unit=message(1),fmt='(A,I2)') &
            'Unrecognized map projection code: ', proj%code
         call da_error(__FILE__,__LINE__,message(1:1))
   end select

   if (trace_use) call da_trace_exit("da_llxy_wrf_new")

end subroutine da_llxy_wrf_new


