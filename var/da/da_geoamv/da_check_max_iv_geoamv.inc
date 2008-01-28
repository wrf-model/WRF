subroutine da_check_max_iv_geoamv(iv, it, itu,ituf,itvv,itvvf)

   !-------------------------------------------------------------------------
   ! Purpose: Innovation vector check for Geo. AMVs               
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: itu,ituf,itvv,itvvf

   integer :: k,n
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_geoamv")

   !---------------------------------------------------------------------------
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for GeoAMV_U   = ',max_error_uv, &
         'Error max test ratio for GeoAMV_V   = ',max_error_uv
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   do n=iv%info(geoamv)%n1,iv%info(geoamv)%n2
      do k = 1, iv%info(geoamv)%levels(n)
         call da_max_error_qc(it, iv%info(geoamv), n, iv%geoamv(n)%u(k), max_error_uv, itu, ituf, &
            check_max_iv_unit, 'U    ', failed, check_max_iv_print)
         call da_max_error_qc (it, iv%info(geoamv), n, iv%geoamv(n)%v(k), max_error_uv, itvv, itvvf, &
            check_max_iv_unit, 'V    ', failed, check_max_iv_print)                           
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_geoamv")

end subroutine da_check_max_iv_geoamv      


