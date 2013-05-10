subroutine da_allocate_y_rain (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y_rain")

   y % nlocal(rain) = iv%info(rain)%nlocal
   y % ntotal(rain) = iv%info(rain)%ntotal

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(rain) > 0) then
      allocate (y % rain(1:y % nlocal(rain)))
      y % rain(1:y % nlocal(rain)) % rain = 0.0
   end if

   if (trace_use) call da_trace_exit("da_allocate_y_rain")

end subroutine da_allocate_y_rain


