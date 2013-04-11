subroutine da_patch_to_global_2d (grid, vlocal, vglobal)

   !---------------------------------------------------------------------
   ! Purpose: Gathers local 2D array vlocal into global array vglobal. 
   !
   ! Must be called by all MPI tasks.
   !---------------------------------------------------------------------  

   implicit none

   type(domain), intent(in)  :: grid
   real,         intent(in)  :: vlocal(:,:)
   real,         intent(out) :: vglobal(:,:)

   real, allocatable :: vlocal3d(:,:,:), vglobal3d(:,:,:)

   if (trace_use_frequent) call da_trace_entry("da_patch_to_global_2d")

   allocate(vlocal3d (ims:ime, jms:jme, 1:1))
   allocate(vglobal3d(ids:ide, jds:jde, 1:1))

   vlocal3d(:,:,1) = vlocal(:,:)
   call da_patch_to_global_3d(grid, vlocal3d, vglobal3d, 1)
   if (rootproc) then
      vglobal(:,:) = vglobal3d(:,:,1)
   end if

   deallocate(vlocal3d)
   deallocate(vglobal3d)

   if (trace_use_frequent) call da_trace_exit("da_patch_to_global_2d")

end subroutine da_patch_to_global_2d


