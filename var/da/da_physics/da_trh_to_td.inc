subroutine da_trh_to_td (rh, t, td)

   !---------------------------------------------------------------------
   !
   !                       function f_td_from_rh
   !                     **************************
   !
   !  purpose:
   !  -------
   !     compute dew point from temperature and relative humidity
   !
   !   method:
   !   ------
   !     invert the relation
   !
   !     rh = 100.0 * exp (l_over_rv * (1.0/t - 1.0/td))
   !
   !   input:
   !   -----
   !      t_k:   temperature       in k
   !      rh:    relative humidity in %
   !
   !   output:
   !   ------
   !      td:    dew point in k
   !
   !   references:
   !   -----------
   !    R. R. Rogers and M. K. Yau, 1989: a short course in cloud physics,
   !                                   3nd edition, pergamon press, page 14-19.
   !
   !   verification set:
   !   -----------------
   !    t_k  = 268.15 k,  
   !    td_k = 262.55 k
   !    rh   = 65 %, 
   !    p_pa = 80000  pa, 
   !    qv   = 2.11e-03 kg/kg,
   !
   !  modifications:
   !   ------------
   !    parallel implementation. -al bourgeoits
   ! 
   !-------------------------------------------------------------------------

   implicit none

   real, dimension(ims:ime,jms:jme,kms:kme), &
      intent(inout) :: rh ! relative humidity.
   real, dimension(ims:ime,jms:jme,kms:kme), &
      intent(in)    ::  t ! temperature.
   real, dimension(ims:ime,jms:jme,kms:kme), &
      intent(out)   :: td ! dew point in k.

   integer :: i, j, k

   real    :: invdifftd, invtd

   if (trace_use_dull) call da_trace_entry("da_trh_to_td")

   do j=jts,jte
      do k=kts,kte
         do i=its,ite
            if (rh(i,j,k) < 10.0) then
               rh(i,j,k) = 10.0
            else if (rh(i,j,k) > 105.0) then
               rh(i,j,k) = 105.0
            end if

            invdifftd = log (rh(i,j,k)/100.0) / l_over_rv

            invtd = 1/t(i,j,k)  - invdifftd

            td(i,j,k)  = 1.0 / invtd

            if (td(i,j,k) > t(i,j,k)) &
               td(i,j,k) = t(i,j,k)
         end do
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_trh_to_td")

end subroutine da_trh_to_td


