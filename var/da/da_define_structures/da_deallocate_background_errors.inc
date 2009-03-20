subroutine da_deallocate_background_errors (be)

   !---------------------------------------------------------------------------
   ! Purpose: Deallocate components of wrfvar background errors.
   !---------------------------------------------------------------------------

   implicit none

   type (be_type), intent(inout)        :: be     ! Background error structure.
   
   if (trace_use) call da_trace_entry("da_deallocate_background_errors")

   if (cv_options /= 3) then

      ! Deallocate gridpoint errors:

      if (be % v1 % mz > 0) deallocate (be % v1 % val)
      if (be % v2 % mz > 0) deallocate (be % v2 % val)
      if (be % v3 % mz > 0) deallocate (be % v3 % val)
      if (be % v4 % mz > 0) deallocate (be % v4 % val)
      if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % val) 
    
      if (be % v1 % mz > 0) deallocate (be % v1 % rf_alpha)
      if (be % v2 % mz > 0) deallocate (be % v2 % rf_alpha)
      if (be % v3 % mz > 0) deallocate (be % v3 % rf_alpha)
      if (be % v4 % mz > 0) deallocate (be % v4 % rf_alpha)
      if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % rf_alpha)

      if (global) then
         if (be % v1 % mz > 0) deallocate (be % v1 % power)
         if (be % v2 % mz > 0) deallocate (be % v2 % power)
         if (be % v3 % mz > 0) deallocate (be % v3 % power)
         if (be % v4 % mz > 0) deallocate (be % v4 % power)
         if (be % v5 % mz > 0) deallocate (be % v5 % power) 
      end if

      ! Deallocate eigenvectors of vertical error covariance:

      if (vert_corr == vert_corr_2) then
         if (be % v1 % mz > 0) deallocate (be % v1 % evec)
         if (be % v2 % mz > 0) deallocate (be % v2 % evec)
         if (be % v3 % mz > 0) deallocate (be % v3 % evec)
         if (be % v4 % mz > 0) deallocate (be % v4 % evec)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % evec)

         if (be % v1 % mz > 0) deallocate (be % v1 % evec_g)
         if (be % v2 % mz > 0) deallocate (be % v2 % evec_g)
         if (be % v3 % mz > 0) deallocate (be % v3 % evec_g)
         if (be % v4 % mz > 0) deallocate (be % v4 % evec_g)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % evec_g)

         if (be % v1 % mz > 0) deallocate (be % v1 % val_g)
         if (be % v2 % mz > 0) deallocate (be % v2 % val_g)
         if (be % v3 % mz > 0) deallocate (be % v3 % val_g)
         if (be % v4 % mz > 0) deallocate (be % v4 % val_g)
         if (be % v5 % mz > 0 .and. .not. global) deallocate (be % v5 % val_g)
      end if

      deallocate (be % reg_chi)
      deallocate (be % reg_ps)
      deallocate (be % reg_t)
   
      ! Deallocate control variable errors (in future uncomment use these to allow 
      ! eg NMC error correlations).

      ! deallocate (be % cv % val)

   else ! for cv_options = 3
    
      deallocate (be % corz)
      deallocate (be % corp)
      deallocate (be % vz)
      deallocate (be % agvz)
      deallocate (be % bvz)
      deallocate (be % wgvz)
      deallocate (be % be)
      deallocate (be % rate)
      deallocate (be % table)
      deallocate (be % slix)
      deallocate (be % slipx)
      deallocate (be % sljy)
      deallocate (be % sljpy)
   
   end if

   if (trace_use) call da_trace_exit("da_deallocate_background_errors")

end subroutine da_deallocate_background_errors


