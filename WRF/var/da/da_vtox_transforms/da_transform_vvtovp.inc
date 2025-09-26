subroutine da_transform_vvtovp(grid, evec, eval, vertical_wgt, vv, vp, mz, levels)

   !---------------------------------------------------------------------------
   ! Purpose: Transform from fields on vertical EOFS to fields on vertical 
   ! levels.
   !
   ! Method:  Perform vp(i,j,k) = P E L^{1/2} vv(i,j,m) transform.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain), intent(in)  :: grid
   integer, intent(in)  :: mz                         ! # vertical modes.
   integer, intent(in)  :: levels                     ! # no. of levels  

   real*8,  intent(in)  :: evec(jds:jde,kds:kde,1:mz) ! Eigenvectors.
   real*8,  intent(in)  :: eval(jds:jde,1:mz)         ! Eigenvalues.
   real,    intent(in)  :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real,    intent(in)  :: vv(ims:ime,jms:jme,kms:kme)   ! CV in EOF space.
   real,    intent(out) :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   
   integer :: i, j, k, m, ij             ! Loop counters.
   real    :: temp

   if (trace_use_dull) call da_trace_entry("da_transform_vvtovp")

   !-------------------------------------------------------------------
   ! [1.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
   !------------------------------------------------------------------- 

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k, m, j, i, temp )
   do ij = 1 , grid%num_tiles
      vp(:,grid%j_start(ij):grid%j_end(ij),:) = 0.0
      do k = kts, levels
         do m = 1, mz
            do j = grid%j_start(ij), grid%j_end(ij)
               temp = evec(j,k,m) * eval(j,m)
   
               do i = its, ite
                  vp(i,j,k) = vp(i,j,k) + temp*vv(i,j,m)
               end do
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO
   
   !-------------------------------------------------------------------
   ! [2.0] Apply inner-product weighting if vertical_ip /= vertical_ip_0:
   !------------------------------------------------------------------- 

   if (vertical_ip /= vertical_ip_0) then
      vp(its:ite,jts:jte,kts:levels) = vp(its:ite,jts:jte,kts:levels) / &
         vertical_wgt(its:ite,jts:jte,kts:levels)                          
   end if

   if (trace_use_dull) call da_trace_exit("da_transform_vvtovp")

end subroutine da_transform_vvtovp


