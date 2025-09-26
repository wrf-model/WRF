subroutine da_read_obs_ssmi_integer_array(aa,nn,i0,i_step)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: nn, i0, i_step
   integer, intent(out) :: aa(:)

   integer :: i, ii

   if (trace_use_frequent) call da_trace_entry("da_read_obs_ssmi_integer_array")

   do i = 1, nn
      ii = i_step*(i-1) + i0
      read (unit=char_ned (ii:ii+8),fmt='((I7,2X))') aa(i)
   end do

   if (trace_use_frequent) call da_trace_exit("da_read_obs_ssmi_integer_array")

end subroutine da_read_obs_ssmi_integer_array


