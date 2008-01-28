subroutine da_check_max_iv_gpspw(iv, it, ittpw,ittpwf)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   !-------------------------------------------------------------------------

   implicit none


   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: ittpw,ittpwf

   logical :: failed 
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_gpspw")

   if (rootproc .and. check_max_iv_print) then
      write (check_max_iv_unit,'(A)')  &
         '----------------------------------------------------------------'
      write (unit = check_max_iv_unit, fmt = '(A,/)') 'MAX ERROR TEST QC:'
      write (unit = check_max_iv_unit, fmt = '(/,9(A,F3.0,/))')  &
         'Error max test ratio for gps_tpw = ',max_error_pw
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   failed = .false.

   ! TPW:

   do n=iv%info(gpspw)%n1,iv%info(gpspw)%n2
      call da_max_error_qc(it, iv%info(gpspw), n, iv%gpspw(n)%tpw, max_error_pw, ittpw, &
        ittpwf, check_max_iv_unit, 'TPW  ', failed, check_max_iv_print)
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_gpspw")

end subroutine da_check_max_iv_gpspw


