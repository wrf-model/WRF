subroutine da_set_boundary_xb(grid)

   !------------------------------------------------------------------------
   ! Purpose:  
   !
   ! Merge East-West boundary values for the desired grid%xb-type variables 
   !------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout) :: grid       ! first guess state.

   integer :: n, j, k

   if ((its /= ids) .or. (ite /= ide)) return

   if (trace_use) call da_trace_entry("da_set_boundary_xb")

   ! 2d
   k = kte + 1
   do j=jms, jme
      do n=1,bdyzone
         grid%xb%lat(ids-n,j)        = grid%xb%lat(ide+1-n,j)
         grid%xb%lon(ids-n,j)        = grid%xb%lon(ide+1-n,j)
         grid%xb%cori(ids-n,j)       = grid%xb%cori(ide+1-n,j)
         grid%xb%terr(ids-n,j)       = grid%xb%terr(ide+1-n,j)
         grid%xb%psfc(ids-n,j)       = grid%xb%psfc(ide+1-n,j)
         grid%xb%map_factor(ids-n,j) = grid%xb%map_factor(ide+1-n,j)
         grid%xb%coefx(ids-n,j)      = grid%xb%coefx(ide+1-n,j)
         grid%xb%coefy(ids-n,j)      = grid%xb%coefy(ide+1-n,j)
         grid%xb%w(ids-n,j,k)        = grid%xb%w(ide+1-n,j,k)
         ! grid%xb%h(ids-n,j,k)        = grid%xb%h(ide+1-n,j,k)

         grid%xb%lat(ide+n,j)        = grid%xb%lat(ids-1+n,j)
         grid%xb%lon(ide+n,j)        = grid%xb%lon(ids-1+n,j)
         grid%xb%cori(ide+n,j)       = grid%xb%cori(ids-1+n,j)
         grid%xb%terr(ide+n,j)       = grid%xb%terr(ids-1+n,j)
         grid%xb%psfc(ide+n,j)       = grid%xb%psfc(ids-1+n,j)
         grid%xb%map_factor(ide+n,j) = grid%xb%map_factor(ids-1+n,j)
         grid%xb%coefx(ide+n,j)      = grid%xb%coefx(ids-1+n,j)
         grid%xb%coefy(ide+n,j)      = grid%xb%coefy(ids-1+n,j)
         grid%xb%w(ide+n,j,k)        = grid%xb%w(ids-1+n,j,k)
         ! grid%xb%h(ide+n,j,k)        = grid%xb%h(ids-1+n,j,k)

         ! Zhiquan Liu add some RTTOV variables
         !--------------------------------------
         grid%xb%t2(ide+n,j) = grid%xb%t2(ids-1+n,j)
         grid%xb%q2(ide+n,j) = grid%xb%q2(ids-1+n,j)
         grid%xb%u10(ide+n,j) = grid%xb%u10(ids-1+n,j)
         grid%xb%v10(ide+n,j) = grid%xb%v10(ids-1+n,j)
         grid%xb%tsk(ide+n,j) = grid%xb%tsk(ids-1+n,j)
         grid%xb%tgrn(ide+n,j) = grid%xb%tgrn(ids-1+n,j)
         grid%xb%landmask(ide+n,j) = grid%xb%landmask(ids-1+n,j)
         grid%xb%snow(ide+n,j) = grid%xb%snow(ids-1+n,j)
         grid%xb%xland(ide+n,j) = grid%xb%xland(ids-1+n,j)

         grid%xb%smois(ide+n,j) = grid%xb%smois(ids-1+n,j)
         grid%xb%tslb(ide+n,j) = grid%xb%tslb(ids-1+n,j)
         grid%xb%xice(ide+n,j) = grid%xb%xice(ids-1+n,j)
         grid%xb%ivgtyp(ide+n,j) = grid%xb%ivgtyp(ids-1+n,j)
         grid%xb%isltyp(ide+n,j) = grid%xb%isltyp(ids-1+n,j)
         grid%xb%vegfra(ide+n,j) = grid%xb%vegfra(ids-1+n,j)
         grid%xb%snowh(ide+n,j) = grid%xb%snowh(ids-1+n,j)

      end do
   end do

   ! 3d
   do k=kts, kte
      do j=jms, jme
         do n=1,bdyzone
            grid%xb%h(ids-n,j,k) = grid%xb%h(ide+1-n,j,k)
            grid%xb%u(ids-n,j,k) = grid%xb%u(ide+1-n,j,k)
            grid%xb%v(ids-n,j,k) = grid%xb%v(ide+1-n,j,k)
            grid%xb%w(ids-n,j,k) = grid%xb%w(ide+1-n,j,k)
            grid%xb%t(ids-n,j,k) = grid%xb%t(ide+1-n,j,k)
            grid%xb%p(ids-n,j,k) = grid%xb%p(ide+1-n,j,k)
            grid%xb%q(ids-n,j,k) = grid%xb%q(ide+1-n,j,k)
            grid%xb%qs(ids-n,j,k) = grid%xb%qs(ide+1-n,j,k)
            grid%xb%es(ids-n,j,k) = grid%xb%es(ide+1-n,j,k)
            grid%xb%rh(ids-n,j,k) = grid%xb%rh(ide+1-n,j,k)
            grid%xb%td(ids-n,j,k) = grid%xb%td(ide+1-n,j,k)
            grid%xb%rho(ids-n,j,k)= grid%xb%rho(ide+1-n,j,k)

            grid%xb%h(ide+n,j,k) = grid%xb%h(ids-1+n,j,k)
            grid%xb%u(ide+n,j,k) = grid%xb%u(ids-1+n,j,k)
            grid%xb%v(ide+n,j,k) = grid%xb%v(ids-1+n,j,k)
            grid%xb%w(ide+n,j,k) = grid%xb%w(ids-1+n,j,k)
            grid%xb%t(ide+n,j,k) = grid%xb%t(ids-1+n,j,k)
            grid%xb%p(ide+n,j,k) = grid%xb%p(ids-1+n,j,k)
            grid%xb%q(ide+n,j,k) = grid%xb%q(ids-1+n,j,k)
            grid%xb%qs(ide+n,j,k) = grid%xb%qs(ids-1+n,j,k)
            grid%xb%es(ide+n,j,k) = grid%xb%es(ids-1+n,j,k)
            grid%xb%rh(ide+n,j,k) = grid%xb%rh(ids-1+n,j,k)
            grid%xb%td(ide+n,j,k) = grid%xb%td(ids-1+n,j,k)
            grid%xb%rho(ide+n,j,k) = grid%xb%rho(ids-1+n,j,k)
         end do
      end do
   end do

   if (trace_use) call da_trace_exit("da_set_boundary_xb")

end subroutine da_set_boundary_xb


