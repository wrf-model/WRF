subroutine da_transform_xtoy_qscat_adj(grid, iv, jo_grad_y, jo_grad_x)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !-----------------------------------------------------------------------

   implicit none
   type (domain),  intent(in)    :: grid          ! first guess state.
   type (iv_type), intent(in)    :: iv          ! obs. inc vector (o-b).
   type (y_type) , intent(in)    :: jo_grad_y   ! grad_y(jo)
   type (x_type) , intent(inout) :: jo_grad_x   ! grad_x(jo)
   integer :: i, j, k  ! Index dimension.
   real    :: dx, dxm  ! Interpolation weights.
   real    :: dy, dym  ! Interpolation weights.
   real    :: znt, rib, ht, za, bn, fm, bd

   integer :: n        ! Loop counter.

   real, allocatable :: u(:,:)
   real, allocatable :: v(:,:)
   real, allocatable :: ub(:,:)
   real, allocatable :: vb(:,:)
   real    :: v_h(kms:kme)      ! Model value h at ob hor. location.

   if (trace_use_dull) call da_trace_entry("da_transform_xtoy_qscat_adj")

   allocate (u(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (v(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

   allocate (ub(1,iv%info(qscat)%n1:iv%info(qscat)%n2))
   allocate (vb(1,iv%info(qscat)%n1:iv%info(qscat)%n2))

   call da_interp_lin_3d (grid%xb%u, iv%info(qscat), ub)
   call da_interp_lin_3d (grid%xb%v, iv%info(qscat), vb)

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      if (wind_sd_qscat) then
          call da_uv_to_sd_adj(jo_grad_y%qscat(n)%u, &
                               jo_grad_y%qscat(n)%v, u(1,n), v(1,n), ub(1,n), vb(1,n))
      else
          u(1,n) = jo_grad_y%qscat(n)%u
          v(1,n) = jo_grad_y%qscat(n)%v
      end if

      if (qscat_neutral) then
         i   = iv%info(qscat)%i(1,n)
         j   = iv%info(qscat)%j(1,n)
         dx  = iv%info(qscat)%dx(1,n)
         dy  = iv%info(qscat)%dy(1,n)
         dxm = iv%info(qscat)%dxm(1,n)
         dym = iv%info(qscat)%dym(1,n)
         do k=kts,kte
            v_h(k) = dym*(dxm*grid%xb%h(i,j,k)+dx*grid%xb%h(i+1,j,k)) + dy*(dxm*grid%xb%h(i,j+1,k)+dx*grid%xb%h(i+1,j+1,k))
         end do

         znt = dym*(dxm*grid%znt(i,j) + dx*grid%znt(i+1,j)) + dy*(dxm*grid%znt(i,j+1) + dx*grid%znt(i+1,j+1))
         rib = dym*(dxm*grid%br(i,j)  + dx*grid%br(i+1,j))  + dy*(dxm*grid%br(i,j+1)  + dx*grid%br(i+1,j+1))
         ht  = dym*(dxm*grid%ht(i,j)  + dx*grid%ht(i+1,j))  + dy*(dxm*grid%ht(i,j+1)  + dx*grid%ht(i+1,j+1))

         za  = v_h(kts) - ht 

         bn = log( 1 + za/znt )
         if (rib < 0) then
            fm=1-10 * rib / ( 1-75 * rib * bn * sqrt( 1+10/znt ) )
         else
            fm = 1 / (1 + 10 * rib / sqrt(1 + 5*rib ) )
         end if
         bd = bn/sqrt(fm)

         u(1,n) = u(1,n) * log( 1 + 10/znt ) / bd
         v(1,n) = v(1,n) * log( 1 + 10/znt ) / bd
      end if
   end do

#ifdef A2C
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(qscat), u,'u')
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(qscat), v,'v')
#else
   call da_interp_lin_3d_adj (jo_grad_x%u, iv%info(qscat), u)
   call da_interp_lin_3d_adj (jo_grad_x%v, iv%info(qscat), v)
#endif

   deallocate (u)
   deallocate (v)
   deallocate (ub)
   deallocate (vb)

   if (trace_use_dull) call da_trace_exit("da_transform_xtoy_qscat_adj")

end subroutine da_transform_xtoy_qscat_adj


