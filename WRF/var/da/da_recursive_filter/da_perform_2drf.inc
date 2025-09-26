subroutine da_perform_2drf(ni, nj, num_passes, rf_scale, field)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni               ! Array dimension 1.
   integer, intent(in)    :: nj               ! Array dimension 2.
   integer, intent(in)    :: num_passes       ! Number of passes of RF.
   real,    intent(in)    :: rf_scale         ! Recursive filter scaling parameter.
   real*8,  intent(inout) :: field(1:ni,1:nj) ! Field to be filtered.

   integer               :: i, j, pass       ! Loop counters.
   real*8                :: e, alpha         ! Recursive filter parameters.
   real                  :: mean_field       ! Mean field.

   if (trace_use) call da_trace_entry("da_perform_2drf")

   e = 0.25 * num_passes / (rf_scale * rf_scale)
   alpha = 1 + e - sqrt(e * (e + 2.0))

   mean_field = sum(field(1:ni,1:nj)) / real(ni*nj)

   do pass = 1, num_passes
      ! Perform filter in I-direction:
      do j = 1, nj
         call da_recursive_filter_1d(pass, alpha, field(1:ni,j), ni)
      end do

      ! Perform filter in J-direction:
      do i = 1, ni
         call da_recursive_filter_1d(pass, alpha, field(i,1:nj), nj)
      end do
   end do

   if (trace_use) call da_trace_exit("da_perform_2drf")

end subroutine da_perform_2drf


