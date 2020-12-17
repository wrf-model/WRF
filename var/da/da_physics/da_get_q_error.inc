subroutine da_get_q_error( p, t, q, t_error, rh_error, q_error)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real, intent(in)  :: p        ! Observed pressure.
   real, intent(in)  :: t        ! Observed temperature.
   real, intent(in)  :: q        ! Observed specific humidity.
   real, intent(in)  :: t_error  ! Temperature observation error.
   real, intent(in)  :: rh_error ! RH observation error.
   real, intent(out) :: q_error  ! q observation error.

   real :: es    ! Saturation vapor pressure.
   real :: qs    ! Saturation specific humidity.
   real :: rh    ! Relative humidity.

   real :: f_qv_from_rh
   external f_qv_from_rh

   if (trace_use_frequent) call da_trace_entry("da_get_q_error")

   if (ABS(p - missing_r) > 1.0 .AND. ABS(t - missing_r) > 1.0 .AND. ABS(q - missing_r) > 1) then
      ! Calculate rh:
      call da_tp_to_qs( t, p, es, qs)

      rh = 100.0 * q / qs
      if (rh > 100.0) rh = 100.0

      ! Get observation error for qv. Is this right?
      q_error = f_qv_from_rh( rh_error, t_error, rh, t, p)
   else
      q_error = missing_r
   end if

   if (q_error == 0.0) q_error = missing_r

   if (trace_use_frequent) call da_trace_exit("da_get_q_error")

end subroutine da_get_q_error


