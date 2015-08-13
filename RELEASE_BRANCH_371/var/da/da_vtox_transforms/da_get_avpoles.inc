subroutine da_get_avpoles(u,v,       &
          ids, ide, jds, jde, &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte  )

   !--------------------------------------------------------------------------- 
   ! Purpose: Treatment for Adjoint of Polar winds 
   !---------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)    :: ids, ide, jds, jde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real,    intent(inout) :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real,    intent(inout) :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
 
   real                   :: tmpvar                                         
   real                   :: tmpu(kts:kte)
   real                   :: tmp_u(kts:kte)
   real                   :: tmpv(kts:kte)
   real                   :: tmp_v(kts:kte)
   integer                :: k

   if (trace_use) call da_trace_entry("da_get_avpoles")

   tmpvar      = 1.0/real(ide-ids+1)

   ! cos_xls etc in da_control, calculated in da_setup_firstguess

   tmp_u(:) =0.0
   tmp_v(:) =0.0

   if (jts == jds) then 
      do k = kts,kte
         tmp_u(k) = tmpvar*sum(-u(its:ite,jts,k)*cos_xls(its:ite) & 
                            +v(its:ite,jts,k)*sin_xls(its:ite))
         tmp_v(k) = tmpvar*sum(-u(its:ite,jts,k)*sin_xls(its:ite) & 
                            -v(its:ite,jts,k)*cos_xls(its:ite))
      end do
   end if

   call wrf_dm_sum_reals(tmp_u(:), tmpu(:))
   call wrf_dm_sum_reals(tmp_v(:), tmpv(:))

   if (jts == jds) then 
      do k = kts,kte
         u(its:ite,jts+1,k) = u(its:ite,jts+1,k) -tmpu(k)*cos_xls(its:ite) &
                              - tmpv(k)*sin_xls(its:ite)
         v(its:ite,jts+1,k) = v(its:ite,jts+1,k) +tmpu(k)*sin_xls(its:ite) &
                              - tmpv(k)*cos_xls(its:ite)
         u(its:ite,jts,k) = 0.0
         v(its:ite,jts,k) = 0.0
      end do
   end if

   tmp_u(:) =0.0
   tmp_v(:) =0.0

   if (jte == jde) then 
      do k = kts,kte
         tmp_u(k) = tmpvar*sum(-u(its:ite,jte,k)*cos_xle(its:ite) & 
                            -v(its:ite,jte,k)*sin_xle(its:ite))
         tmp_v(k) = tmpvar*sum( u(its:ite,jte,k)*sin_xle(its:ite) & 
                            -v(its:ite,jte,k)*cos_xle(its:ite))
      end do
   end if

   call wrf_dm_sum_reals(tmp_u(:), tmpu(:))
   call wrf_dm_sum_reals(tmp_v(:), tmpv(:))

   if (jte == jde) then 
      do k = kts,kte
         u(its:ite,jte-1,k) = u(its:ite,jte-1,k) -tmpu(k)*cos_xle(its:ite) &
                              + tmpv(k)*sin_xle(its:ite)
         v(its:ite,jte-1,k) = v(its:ite,jte-1,k) -tmpu(k)*sin_xle(its:ite) &
                              - tmpv(k)*cos_xle(its:ite)
         u(its:ite,jte,k) = 0.0
         v(its:ite,jte,k) = 0.0
      end do
   end if

   if (trace_use) call da_trace_exit("da_get_avpoles")

end subroutine da_get_avpoles


