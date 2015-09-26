subroutine da_get_spoles(x,              &
          ids, ide, jds, jde, &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte  )

   !---------------------------------------------------------------------------
   ! Purpose: Treatment for Scalar field at Poles
   !---------------------------------------------------------------------------

   implicit none
   

   integer, intent(in)    :: ids, ide, jds, jde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real,    intent(inout) :: x(ims:ime,jms:jme,kms:kme)   

   integer                :: k
   real                   :: tmpvar,tmps,tmp_s

   if (trace_use) call da_trace_entry("da_get_spoles")

   tmpvar      = 1.0/real(ide-ids+1)

   do k = kts, kte
      tmps =0.0  ; tmp_s =0.0
      if (jts == jds)  tmp_s = tmpvar*sum(x(its:ite,jts+1,k))

      tmps = wrf_dm_sum_real( tmp_s)
      if (jts == jds) x(its:ite,jts,k) = tmps
 
      tmps =0.0  ; tmp_s =0.0
      if (jte == jde) tmp_s = tmpvar*sum(x(its:ite,jte-1,k))

      tmps = wrf_dm_sum_real( tmp_s)
      if (jte == jde) x(its:ite,jte,k) = tmps
   end do

   if (trace_use) call da_trace_exit("da_get_spoles")

end subroutine da_get_spoles


