subroutine da_couple_uv (u,  v,  mu, mub,  msfu, msfv, c1, c2, ids, ide, jds, jde, kds, kde)

   implicit none

   integer, intent(in)    :: ids, ide, jds, jde, kds, kde
   real,    intent(inout) :: u(ids:ide+1,jds:jde,kds:kde)
   real,    intent(inout) :: v(ids:ide,jds:jde+1,kds:kde)
   real,    intent(in)    :: msfu(ids:ide+1,jds:jde)
   real,    intent(in)    :: msfv(ids:ide,jds:jde+1)
   real,    intent(in)    :: mu(ids:ide,jds:jde)
   real,    intent(in)    :: mub(ids:ide,jds:jde)
   real,    intent(in)    :: c1(kds:kde)
   real,    intent(in)    :: c2(kds:kde)

   real, allocatable :: muu(:,:), muv(:,:)

   ! update_bc
   ! if (trace_use) call da_trace_entry("da_couple_uv")  

   allocate(muu(ids:ide+1, jds:jde ))
   allocate(muv(ids:ide  , jds:jde+1))

   ! couple variables u, v

   call da_calc_mu_uv (mu, mub, muu, muv, ids, ide, jds, jde)

   call da_couple (muu, u, msfu, c1, c2, ids, ide+1, jds, jde, kds, kde)

   call da_couple (muv, v, msfv, c1, c2, ids, ide, jds, jde+1, kds, kde)

   deallocate(muu)
   deallocate(muv)

   ! if (trace_use) call da_trace_exit("da_couple_uv")  

end subroutine da_couple_uv


