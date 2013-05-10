subroutine da_trh_to_td (grid)

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

   type (domain), intent(inout) :: grid

   integer :: i, j, k, ij

   real    :: invdifftd, invtd

   if (trace_use_dull) call da_trace_entry("da_trh_to_td")

   !$OMP PARALLEL DO &
   !$OMP PRIVATE( ij, i, j, k, invdifftd, invtd )
   do ij = 1 , grid%num_tiles

   do k=kts,kte
      do j=grid%j_start(ij), grid%j_end(ij)
         do i=its,ite
            if (grid%xb%rh(i,j,k) < 10.0) then
               grid%xb%rh(i,j,k) = 10.0
            else if (grid%xb%rh(i,j,k) > 105.0) then
               grid%xb%rh(i,j,k) = 105.0
            end if

            invdifftd = log (grid%xb%rh(i,j,k)/100.0) / l_over_rv

            invtd = 1/grid%xb%t(i,j,k)  - invdifftd

            grid%xb%td(i,j,k)  = 1.0 / invtd

            if (grid%xb%td(i,j,k) > grid%xb%t(i,j,k)) &
               grid%xb%td(i,j,k) = grid%xb%t(i,j,k)
         end do
      end do
   end do

   end do
   !$OMP END PARALLEL DO

   if (trace_use_dull) call da_trace_exit("da_trh_to_td")

end subroutine da_trh_to_td


