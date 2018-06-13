subroutine da_allocate_y_radar (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.

   integer                       :: n       ! Loop counter.
   integer                       :: nlevels ! Number of levels.

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y_radar")

   y % nlocal(radar) = iv%info(radar)%nlocal
   y % ntotal(radar) = iv%info(radar)%ntotal

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(radar) > 0) then
      allocate (y % radar(1:y % nlocal(radar)))
      do n = 1, y % nlocal(radar)
         nlevels = iv%info(radar)%levels(n)
         allocate (y % radar(n)%rv(1:nlevels))
         allocate (y % radar(n)%rf(1:nlevels))
         allocate (y % radar(n)%rrn(1:nlevels))
         allocate (y % radar(n)%rsn(1:nlevels))
         allocate (y % radar(n)%rgr(1:nlevels))
         allocate (y % radar(n)%rqv(1:nlevels))

         y % radar(n) % rv(1:nlevels)  = 0.0
         y % radar(n) % rf(1:nlevels)  = 0.0
         y % radar(n) % rrn(1:nlevels) = 0.0
         y % radar(n) % rsn(1:nlevels) = 0.0
         y % radar(n) % rgr(1:nlevels) = 0.0
         y % radar(n) % rqv(1:nlevels) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_allocate_y_radar")

end subroutine da_allocate_y_radar


