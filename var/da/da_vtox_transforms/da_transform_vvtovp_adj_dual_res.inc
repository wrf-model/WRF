subroutine da_transform_vvtovp_adj_dual_res(grid, evec, eval, vertical_wgt, vp, vv, mz, levels)

   !---------------------------------------------------------------------------
   ! Purpose: Adjoint of da_transform_vvtovp.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(in) :: grid
   integer, intent(in) :: mz                         ! # vertical modes.
   integer, intent(in) :: levels                     ! no. of vertical levels

   real*8, intent(in)  :: evec(jds_int:jde_int,kds_int:kde_int,1:mz) ! Eigenvectors.
   real*8, intent(in)  :: eval(jds_int:jde_int,1:mz)         ! Eigenvalues.

!  real*8, intent(in)  :: evec(1:jde_int-jds_int+1,kds_int:kde_int,1:mz) ! Eigenvectors.
!  real*8, intent(in)  :: eval(1:jde_int-jds_int+1,1:mz)         ! Eigenvalues.
   real, intent(in)    :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real, intent(inout) :: vp(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int)! CV in level space.
   real, intent(out)   :: vv(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int)! CV in EOF space.
 
   integer :: i, j, m, k, ij             ! Loop counters.
   real    :: temp

   if (trace_use_dull) call da_trace_entry("da_transform_vvtovp_adj")

   !-------------------------------------------------------------------
   ! [1.0] Apply inner-product weighting if vertical_ip /= vertical_ip_0:
   !------------------------------------------------------------------- 

   if (vertical_ip /= vertical_ip_0) then
      vp(its:ite,jts:jte,kts:levels) = vp(its:ite,jts:jte,kts:levels) / &
         vertical_wgt(its:ite,jts:jte,kts:levels)
   end if

   !-------------------------------------------------------------------
   ! [2.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
   !------------------------------------------------------------------- 

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, m, k, j, i, temp )
   do ij = 1 , grid%num_tiles
      vv(:,grid%j_start(ij):grid%j_end(ij),:) = 0.0
      do m = 1, mz
         do k = kts_int, levels
            do j = grid%j_start(ij), grid%j_end(ij)
               temp = evec(j,k,m) * eval(j,m)
   
               do i = its_int, ite_int
                  vv(i,j,m) = vv(i,j,m) + temp*vp(i,j,k)
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   if (trace_use_dull) call da_trace_exit("da_transform_vvtovp_adj")

end subroutine da_transform_vvtovp_adj_dual_res


