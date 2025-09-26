subroutine da_set_tst_trnsf_fld(grid, va, vb, typical_rms)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(domain), intent(in)  :: grid   ! model state
   real,         intent(out) :: va(ims:ime, jms:jme, kms:kme)
   real,         intent(in)  :: vb(ims:ime, jms:jme, kms:kme)
   real,         intent(in)  :: typical_rms

   real    :: gbuf(ids:ide, jds:jde, kds:kde) 
   integer :: size3d
   integer :: k             ! loop counter.
   real    :: scale         ! scale factor.
   real    :: xy_dim_inv    ! 1 / real(iy * jx)
   real    :: field_mean    ! mean field.
   real    :: field_rms     ! rms field.

   if (trace_use) call da_trace_entry("da_set_tst_trnsf_fld")

   size3d = (ide-ids+1)*(jde-jds+1)*(kde-kds+1)

   call da_patch_to_global(grid, vb, gbuf)
   call wrf_dm_bcast_real(gbuf, size3d)

   xy_dim_inv  = 1.0 / real((ide-ids+1)*(jde-jds+1))

   do k = kts,kte
      field_mean = sum(gbuf(ids:ide, jds:jde, k)) * xy_dim_inv

      gbuf(ids:ide, jds:jde, k) = gbuf(ids:ide, jds:jde, k) - field_mean

      field_rms = sqrt(1.0e-6+sum(gbuf(ids:ide, jds:jde, k)**2) * xy_dim_inv)

      scale = typical_rms / field_rms

      va(its:ite, jts:jte, k) = scale * gbuf(its:ite, jts:jte, k)
   end do

   if (trace_use) call da_trace_exit("da_set_tst_trnsf_fld")

end subroutine da_set_tst_trnsf_fld


