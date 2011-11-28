subroutine da_filter_regcoeffs(ni, nj, nk, num_bins, num_bins2d, num_passes, &
   rf_scale, bin, regcoeff1, regcoeff2, regcoeff3)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: ni, nj, nk                  ! Grid dimensions.
   integer, intent(in)    :: num_bins                    ! Number of bins for 3D coeffs.
   integer, intent(in)    :: num_bins2d                  ! Number of bins for 2D coeffs.
   integer, intent(in)    :: num_passes                  ! Number of passes for RF.
   real,    intent(in)    :: rf_scale                    ! Smoothing scale of RF.
   integer, intent(in)    :: bin(1:ni,1:nj,1:nk)         ! Bin assigned to each point.
   real,    intent(inout) :: regcoeff1(1:num_bins)       ! psi/chi regression cooefficient.
   real,    intent(inout) :: regcoeff2(1:nk,1:num_bins2d)! psi/ps regression cooefficient.
   real,    intent(inout) :: regcoeff3(1:nk,1:nk,1:num_bins2d) ! psi/T regression cooefficient.

   integer :: i, j, k              ! Loop counters.
   integer :: b                    ! Bin index.
   real*8  :: field(1:ni,1:nj)     ! Field for recursive filter.

   if (trace_use) call da_trace_entry("da_filter_regcoeffs")

   !----------------------------------------------------------------------------
   ! [1] Filter psi/chi coefficient:
   !----------------------------------------------------------------------------

   do k = 1, nk
      ! Prepare field for filtering:
      do j = 1, nj
         do i = 1, ni
            b = bin(i,j,k)
            field(i,j) = regcoeff1(b)
         end do
      end do

      call da_perform_2drf(ni, nj, num_passes, rf_scale, field)

      do j = 1, nj
         do i = 1, ni
            b = bin(i,j,k)
            regcoeff1(b) = field(i,j)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_filter_regcoeffs")

end subroutine da_filter_regcoeffs


