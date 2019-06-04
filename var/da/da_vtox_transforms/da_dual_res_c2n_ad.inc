subroutine da_dual_res_c2n_ad( input_fld, output_fld,  &
                   nims, nime, njms, njme, nkms, nkme, &
                   nits, nite, njts, njte, nkts, nkte, &
                   cids, cide, cjds, cjde, ckds, ckde, &
                   cims, cime, cjms, cjme, ckms, ckme, &
                   cits, cite, cjts, cjte, ckts, ckte)

   ! adjoint of bilinear interpolation from low-res to high-res
   ! input_fld  is high-res on nest grid
   ! output_fld is low-res on coarse grid

   implicit none

   integer, intent(in) :: cids, cide, ckds, ckde, cjds, cjde, &
                          cims, cime, ckms, ckme, cjms, cjme, &
                          cits, cite, ckts, ckte, cjts, cjte
   integer, intent(in) :: nims, nime, nkms, nkme, njms, njme, &
                          nits, nite, nkts, nkte, njts, njte
   real, intent(in)    :: input_fld (nims:nime,njms:njme,nkms:nkme)
   real, intent(inout) :: output_fld(cims:cime,cjms:cjme,ckms:ckme)

   ! local variables
   real                :: output_loc(cids:cide,cjds:cjde,ckds:ckde)
   real                :: output_g  (cids:cide,cjds:cjde,ckds:ckde)
   integer             :: i, j, k, ijk, ierr

   if (trace_use) call da_trace_entry("da_dual_res_c2n_ad")

   output_loc = 0.0
   output_g   = 0.0

   do k = nkts, nkte
      do j = njts, njte
         do i = nits, nite
            output_loc(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)   =  &
               aens_locs(i,j)%dym * aens_locs(i,j)%dxm  * input_fld(i,j,k) + output_loc(aens_locs(i,j)%i  ,aens_locs(i,j)%j,k)
            output_loc(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)   =  &
               aens_locs(i,j)%dym * aens_locs(i,j)%dx   * input_fld(i,j,k) + output_loc(aens_locs(i,j)%i+1,aens_locs(i,j)%j,k)
            output_loc(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k) =  &
               aens_locs(i,j)%dy  * aens_locs(i,j)%dxm  * input_fld(i,j,k) + output_loc(aens_locs(i,j)%i  ,aens_locs(i,j)%j+1,k)
            output_loc(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k) =  &
               aens_locs(i,j)%dy  * aens_locs(i,j)%dx   * input_fld(i,j,k) + output_loc(aens_locs(i,j)%i+1,aens_locs(i,j)%j+1,k)
         end do
      end do
   end do

#ifdef DM_PARALLEL
   ijk = (cide-cids+1)*(cjde-cjds+1)*(ckde-ckds+1)
   call mpi_allreduce(output_loc(:,:,:), output_g(:,:,:), ijk, &
                      true_mpi_real, mpi_sum, comm, ierr)
   if ( ierr /= 0 ) then
      write(message(1), '(a, i3)') 'Error mpi_allreduce on proc', myproc
      call da_error(__FILE__,__LINE__,message(1:1))
   end if
#else
   output_g(:,:,:) = output_loc(:,:,:)
#endif

   output_fld(cits:cite,cjts:cjte,ckts:ckte) = output_g(cits:cite,cjts:cjte,ckts:ckte)

   if (trace_use) call da_trace_exit("da_dual_res_c2n_ad")

end subroutine da_dual_res_c2n_ad

