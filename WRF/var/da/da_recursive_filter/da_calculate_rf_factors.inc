subroutine da_calculate_rf_factors (rf_lengthscale, rf_alpha, rf_scale_factor)

   !---------------------------------------------------------------------------
   ! Purpose: Calculate:
   !          1) Alpha value for recursive filter.
   !          2) Turning conditions appropriate for B=UU^T RF.
   !          3) Generic (depends only on rf_passes) rescaling factor.
   !          4) Grid-dependent (hopefully temporary) scaling factor - removed.
   !---------------------------------------------------------------------------

   implicit none
   
   real*8, intent(in) :: rf_lengthscale(:)      ! Non-dim. R.F. lengthscale.
   real*8,intent(out) :: rf_alpha(:)            ! RF alpha factor.
   real*8,intent(out) :: rf_scale_factor(:)     ! Variance scaling factor.
      
   integer, parameter :: n = 500                ! 2n +1 = # pts in delta func.
   integer            :: kz                     ! 3rd array dimension.
   integer            :: pass                   ! Pass of recursive filter.
   integer            :: k                      ! Loop counter
   ! integer          :: nn                       ! Loop counter
      
   real               :: rf_e                   ! RF E factor.      

   real*8, dimension(-n:n) :: field_in, field_out  ! Working field.
   ! real, dimension(-n:n) :: field_out1  ! Working field.

   if (trace_use_dull) call da_trace_entry("da_calculate_rf_factors")

   !-------------------------------------------------------------------------
   ! [1.0]: Initialise:
   !-------------------------------------------------------------------------  

   kz = size(rf_scale_factor)
   
   rf_scale_factor(:) = 0.0
   
   do k = 1, kz

      !-------------------------------------------------------------------------
      ! [2.0]: Calculate RF alpha:
      !-------------------------------------------------------------------------  

      rf_e = 0.25 * rf_passes / (rf_lengthscale(k) * rf_lengthscale(k))
      rf_alpha(k) = 1.0 + rf_e - sqrt(rf_e * (rf_e + 2.0))

      !-------------------------------------------------------------------------
      ! [3.0]: Calculate rescaling factor:
      !-------------------------------------------------------------------------

      ! [3.1]: Calculate generic rescaling (normalise zero distance to 1):
      ! For rf_passes=2 (SOAR) = 4*rf_lengthscale.
      ! For rf_passes=infinity (Gaussian) = sqrt(8*pi)*rf_lengthscale.

      field_in(-n:n) = 0.0
      field_in(0) = 1.0
      field_out(-n:n) = field_in(-n:n)

      do pass = 1, rf_passes / 2
         call da_recursive_filter_1d_adj(pass, rf_alpha(k), field_out, 2*n+1)
      end do

      do pass = 1, rf_passes / 2
         call da_recursive_filter_1d(pass, rf_alpha(k), field_out, 2*n+1)
      end do

      rf_scale_factor(k) = 1.0 / field_out(0)

      ! Uncomment the following to test equivalence of UU^T and RF:
      ! write(unit=stdout,fmt='(A,f15.5)') &
      !    ' RF Scaling Factor = ', 1.0 / field_out(0)
      ! field_out1(-n:n) = field_in(-n:n)
      ! do pass = 1, rf_passes
      !    call da_recursive_filter_1d(pass, rf_alpha(k), field_out1, 2*n+1)
      ! end do

      ! do nn = -n, n
      !    write(unit=stdout,fmt='(2i5,4f12.5)')k, nn, field_in(nn), &
      !                             field_out(nn) / field_out(0), &
      !                             field_out1(nn) / field_out1(0), &
      !                             exp(-0.125*(real(nn)/rf_lengthscale(k))**2)
      ! end do

   end do ! End loop over k

   if (trace_use_dull) call da_trace_exit("da_calculate_rf_factors")
   
end subroutine da_calculate_rf_factors
      

