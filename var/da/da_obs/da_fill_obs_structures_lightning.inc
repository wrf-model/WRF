subroutine da_fill_obs_structures_lightning(iv, ob)

   !----------------------------------------------------------------------------   
   ! Purpose: Allocates observation structure and fills it from iv.
   ! Authors: Z Chen (zchen@fjnu.edu.cn), Jenny Sun (NCAR), X Qie (IAP) 
   !----------------------------------------------------------------------------   

   implicit none

   type (iv_type), intent(inout) :: iv   ! Obs and header structure.
   type (y_type), intent(out)    :: ob   ! (Smaller) observation structure.

   integer :: n, k     ! Loop counters.
   integer :: i,j

   if (trace_use) call da_trace_entry("da_fill_obs_structures_lightning")

   !---------------------------------------------------------------------------
   ! Initialise obs error factors (which will be overwritten in use_obs_errfac)
   !---------------------------------------------------------------------------

   iv % lightning_ef_w   = 1.0
   iv % lightning_ef_div = 1.0
   iv % lightning_ef_qv  = 1.0
   !----------------------------------------------------------------------
   ! [1.0] Allocate innovation vector and observation structures:
   !----------------------------------------------------------------------
   call da_allocate_y_lightning(iv, ob)

   !----------------------------------------------------------------------
   ! [2.0] Transfer observations:
   !----------------------------------------------------------------------

   ! [2.20] Transfer lightning obs:

   if (iv%info(lightning)%nlocal > 0) then
      do n = 1, iv%info(lightning)%nlocal
	 do k = 1, iv%info(lightning)%levels(n)
            ob%lightning(n)%w(k)  = iv%lightning(n)%w(k)%inv
            ob%lightning(n)%div(k)  = iv%lightning(n)%div(k)%inv
            ob%lightning(n)%qv(k) = iv%lightning(n)%qv(k)%inv
        end do			
      end do
   end if

   if (trace_use) call da_trace_exit("da_fill_obs_structures_lightning")

end subroutine da_fill_obs_structures_lightning
