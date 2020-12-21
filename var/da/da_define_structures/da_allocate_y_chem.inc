subroutine da_allocate_y_chem (iv, y)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate arrays used in y and residual obs structures.
   !---------------------------------------------------------------------------

   implicit none
   
   type (iv_type), intent(in)    :: iv      ! Ob type input.
   type (y_type),  intent(inout) :: y       ! Residual type structure.
   integer                       :: ilocal

   !---------------------------------------------------------------------------
   !  [1.0] Copy number of observations:
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_allocate_y_chem")

   y % nlocal(chem_surf) = iv%info(chem_surf)%nlocal
   y % ntotal(chem_surf) = iv%info(chem_surf)%ntotal

   y % nlocal(chem_acft) = iv%info(chem_acft)%nlocal
   y % ntotal(chem_acft) = iv%info(chem_acft)%ntotal

  !---------------------------------------------------------------------------
  ! [2.0] Allocate:
  !---------------------------------------------------------------------------

   if (y % nlocal(chem_surf) > 0) then
      allocate (y % chem_surf(1:y % nlocal(chem_surf)))
      do ilocal = 1, y % nlocal(chem_surf)
         allocate (y % chem_surf(ilocal)%chem (num_chem_surf-1) )
         y % chem_surf(ilocal) % chem(:) = 0.0
      end do
   end if

   if (y % nlocal(chem_acft) > 0) then
      allocate (y % chem_acft(1:y % nlocal(chem_acft)))
      do ilocal = 1, y % nlocal(chem_acft)
         allocate (y % chem_acft(ilocal)%chem (num_chem_acft-1,num_platform) )
         y % chem_acft(ilocal) % chem(:,:) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_allocate_y_chem")

end subroutine da_allocate_y_chem


