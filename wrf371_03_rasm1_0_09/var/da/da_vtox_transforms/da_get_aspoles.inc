subroutine da_get_aspoles(x,              &
          ids, ide, jds, jde, &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte  )

   !---------------------------------------------------------------------------
   ! Purpose: Treatment for Adjoint of Scalar field at Poles
   !---------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)    :: ids, ide, jds, jde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real,    intent(inout) :: x(ims:ime,jms:jme,kms:kme)   

   integer                :: k
   real                   :: tmpvar
   real                   :: tmps(kts:kte)
   real                   :: tmp_s(kts:kte)

   tmpvar      = 1.0/real(ide-ids+1)

   if (trace_use) call da_trace_entry("da_get_aspoles")

   tmp_s(:) = 0.0

   if (jts == jds) then
      do k = kts, kte
         tmp_s(k) = tmpvar*sum(x(its:ite,jts,k))
      end do
   end if

   call wrf_dm_sum_reals(tmp_s(:),tmps(:))

   if (jts == jds) then
      do k = kts, kte
         x(its:ite,jts+1,k) = x(its:ite,jts+1,k) + tmps(k)
         x(its:ite,jts,k) = 0.0
      end do
   end if

   tmp_s(:) = 0.0

   if (jte == jde) then
      do k = kts, kte
         tmp_s(k) = tmpvar*sum(x(its:ite,jte,k))
      end do
   end if

   call wrf_dm_sum_reals(tmp_s(:),tmps(:))

   if (jte == jde) then
      do k = kts, kte
         x(its:ite,jte-1,k) = x(its:ite,jte-1,k) + tmps(k)
         x(its:ite,jte,k) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_aspoles")

end subroutine da_get_aspoles


