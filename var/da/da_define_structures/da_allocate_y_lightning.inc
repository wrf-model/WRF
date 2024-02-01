subroutine da_allocate_y_lightning (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP)   
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.
   
   integer                       :: n       ! Loop counter.
   integer                       :: nlevels ! Number of levels.
   
   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y_lightning")

   y % nlocal(lightning) = iv%info(lightning)%nlocal
   y % ntotal(lightning) = iv%info(lightning)%ntotal

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(lightning) > 0) then
      allocate (y % lightning(1:y % nlocal(lightning)))
      do n = 1, y % nlocal(lightning)
         nlevels = iv%info(lightning)%levels(n)
         allocate (y % lightning(n) % w(1:nlevels))
         allocate (y % lightning(n) % div(1:nlevels))
         allocate (y % lightning(n) % qv(1:nlevels))
         y % lightning(n) % w(1:nlevels) = 0.0
         y % lightning(n) % div(1:nlevels) = 0.0
         y % lightning(n) % qv(1:nlevels) = 0.0
	   end do
   end if

   if (trace_use) call da_trace_exit("da_allocate_y_lightning")

end subroutine da_allocate_y_lightning
