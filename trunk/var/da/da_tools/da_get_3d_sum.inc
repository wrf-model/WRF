subroutine da_get_3d_sum(var, name)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,             intent(in) :: var(ims:ime, jms:jme, kms:kme)
   character(len=*), intent(in) :: name

   real :: partial, total

   if (trace_use) call da_trace_entry("da_get_3d_sum")

   partial = sum(var(its:ite,jts:jte,kts:kte)*var(its:ite,jts:jte,kts:kte))

   total = wrf_dm_sum_real (partial)

   write(unit=stdout, fmt='(3a, e24.14)') 'Square sum of <', trim(name), '>=', total

   if (trace_use) call da_trace_exit("da_get_3d_sum")

end subroutine da_get_3d_sum


