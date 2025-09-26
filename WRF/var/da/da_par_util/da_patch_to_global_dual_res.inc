subroutine da_patch_to_global_dual_res(grid, vlocal, vglobal, mz)

   !----------------------------------------------------------------------
   ! Purpose: Gathers local 3D array vlocal into global array vglobal.  
   ! Assumes that "k" is not decomposed.  End indices in the "k" dimension 
   ! are inferred from mz, which can be less than kde.  
   !
   ! Must be called by all MPI tasks.  
   !----------------------------------------------------------------------

   implicit none

   type(domain),      intent(in)  :: grid
   real,              intent(in)  :: vlocal(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int)
   real,              intent(out) :: vglobal(ids_int:ide_int,jds_int:jde_int,kds_int:kde_int)
   integer, optional, intent(in)  :: mz

#ifdef DM_PARALLEL

   integer :: local_kde
   integer :: local_kme
   integer :: local_kpe
#endif

   if (trace_use_frequent) call da_trace_entry("da_patch_to_global_dual_res")

#ifdef DM_PARALLEL

   if (present(mz)) then
      local_kde = kds_int + mz - 1
      local_kme = local_kde
      local_kpe = local_kde
   else
      local_kde = kde_int
      local_kme = kme_int
      local_kpe = kpe_int
   end if

   if (local_kde > 0) then
      call wrf_patch_to_global_real (vlocal, vglobal, grid%xp%domdesc, &
         trim(grid_stagger), trim(grid_ordering), &
         ids_int, ide_int, jds_int, jde_int, kds_int, local_kde,  &
         ims_int, ime_int, jms_int, jme_int, kms_int, local_kme,  &
         ips_int, ipe_int, jps_int, jpe_int, kps_int, local_kpe)
   end if
#else
   vglobal(ids_int:ide_int,jds_int:jde_int,kds_int:kde_int) = vlocal(ids_int:ide_int,jds_int:jde_int,kds_int:kde_int)
#endif

   if (trace_use_frequent) call da_trace_exit("da_patch_to_global_dual_res")

end subroutine da_patch_to_global_dual_res


