subroutine da_get_vpoles(u,v,        &
          ids, ide, jds, jde, &
          ims, ime, jms, jme, kms, kme,  &
          its, ite, jts, jte, kts, kte  )

   !---------------------------------------------------------------------------
   !  Purpose: Treatment for Polar winds  
   !---------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)    :: ids, ide, jds, jde
   integer, intent(in)    :: ims, ime, jms, jme, kms, kme
   integer, intent(in)    :: its, ite, jts, jte, kts, kte
   real, intent(inout)    :: u(ims:ime,jms:jme,kms:kme)   ! u wind comp.
   real, intent(inout)    :: v(ims:ime,jms:jme,kms:kme)   ! v wind comp.
 
   real                   :: tmpvar                                         
   real                   :: tmpu,tmp_u,tmpv,tmp_v
   integer                :: k

   if (trace_use) call da_trace_entry("da_get_vpoles")

   ! cos_xls etc in da_control, calculated in da_setup_firstguess

   tmpvar       = 1.0/real(ide-ids+1)

   do k = kts,kte
      tmp_u =0.0
      tmp_v =0.0
      tmpu = 0.0
      tmpv = 0.0

      if (jts == jds) then 
         tmp_u = tmpvar*sum(-u(its:ite,jts+1,k)*cos_xls(its:ite)& 
                            +v(its:ite,jts+1,k)*sin_xls(its:ite))
         tmp_v = tmpvar*sum(-u(its:ite,jts+1,k)*sin_xls(its:ite)& 
                            -v(its:ite,jts+1,k)*cos_xls(its:ite))
      end if

      tmpu = wrf_dm_sum_real( tmp_u)
      tmpv = wrf_dm_sum_real( tmp_v)

      if (jts == jds) then 
        u(its:ite,jts,k) = -tmpu*cos_xls(its:ite) -tmpv*sin_xls(its:ite)
        v(its:ite,jts,k) =  tmpu*sin_xls(its:ite) -tmpv*cos_xls(its:ite)
      end if
 
      tmp_u =0.0
      tmp_v =0.0
      tmpu = 0.0
      tmpv = 0.0

      if (jte == jde) then 
         tmp_u = tmpvar*sum(-u(its:ite,jte-1,k)*cos_xle(its:ite)& 
                            -v(its:ite,jte-1,k)*sin_xle(its:ite))
         tmp_v = tmpvar*sum( u(its:ite,jte-1,k)*sin_xle(its:ite)& 
                            -v(its:ite,jte-1,k)*cos_xle(its:ite))
      end if
      tmpu = wrf_dm_sum_real( tmp_u)
      tmpv = wrf_dm_sum_real( tmp_v)
      if (jte == jde) then 
         u(its:ite,jte,k) = -tmpu*cos_xle(its:ite) +tmpv*sin_xle(its:ite)
         v(its:ite,jte,k) = -tmpu*sin_xle(its:ite) -tmpv*cos_xle(its:ite)
      end if
   end do

   if (trace_use) call da_trace_exit("da_get_vpoles")

end subroutine da_get_vpoles


