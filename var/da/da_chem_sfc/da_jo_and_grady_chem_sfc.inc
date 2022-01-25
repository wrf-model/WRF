subroutine da_jo_and_grady_chem_sfc(iv, re, jo, jo_grad_y)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (iv_type), intent(in)   :: iv          ! Innovation vector.
   type (y_type),  intent(in)   :: re          ! Residual vector.
   type (y_type),  intent(inout):: jo_grad_y   ! Grad_y(Jo)
   type (jo_type), intent(inout):: jo          ! Obs cost function.

   integer        :: n,ichem

   ! the following "global" objects are used only when testing
   type (iv_type) :: iv_glob         ! Global Innovation vector (O-B).
   type (y_type)  :: re_glob         ! Global Residual vector (O-A).
   type (y_type)  :: jo_grad_y_glob  ! Global Grad_y(Jo)

   if (trace_use_dull) call da_trace_entry("da_jo_and_grady_chem_sfc")

   jo % chemic_surf = 0.0

   if (test_dm_exact) then
      if (iv%info(chemic_surf)%ntotal == 0) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_chem_sfc")
         return
      end if
   else
      if (iv%info(chemic_surf)%nlocal < 1) then
         if (trace_use_dull) call da_trace_exit("da_jo_and_grady_chem_sfc")
         return
      end if
   end if

   do ichem = PARAM_FIRST_SCALAR, num_chemic_surf
   do n=1, iv%info(chemic_surf)%nlocal
      jo_grad_y%chemic_surf(n)%chem(ichem) = -re%chemic_surf(n)%chem(ichem) / (iv%chemic_surf(n)%chem(ichem)%error * iv%chemic_surf(n)%chem(ichem)%error)
   end do
   end do

      call da_jo_chem_sfc(iv, re, jo_grad_y, jo)

   jo % chemic_surf = 0.5 * jo % chemic_surf

   if (trace_use_dull) call da_trace_exit("da_jo_and_grady_chem_sfc")

end subroutine da_jo_and_grady_chem_sfc


