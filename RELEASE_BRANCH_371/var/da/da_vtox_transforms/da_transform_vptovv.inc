subroutine da_transform_vptovv (evec, eval, vertical_wgt, vp, vv, mz, &
   kds,kde, ims,ime, jms,jme, kms,kme, its,ite, jts,jte, kts,kte)

   !---------------------------------------------------------------------------
   ! Purpose: Transform from fields on vertical levels to fields on vertical 
   ! EOFS.
   !
   ! Method: Perform vv(i,j,m) = L^{-1/2} E^{T} vp(i,j,k) transform.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: mz                         ! # vertical modes.
   integer, intent(in)    :: kds,kde  ! domain dims.
   integer, intent(in)    :: ims,ime, jms,jme, kms,kme  ! memory dims.
   integer, intent(in)    :: its,ite, jts,jte, kts,kte  ! tile   dims
   real*8,  intent(in)    :: evec(jts:jte,kds:kde,1:mz) ! Eigenvectors.
   real*8,  intent(in)    :: eval(jts:jte,1:mz)         ! Eigenvalues.
   real,    intent(in)    :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   real,    intent(inout) :: vp(ims:ime,jms:jme,kms:kme)! CV in level space.
   real,    intent(out)   :: vv(ims:ime,jms:jme,1:mz)   ! CV in EOF space.
   
   integer :: i, j, m                    ! Loop counters.
   real    :: ETVp                       ! E(k,m)^{T}*vp(i,j,k)

   ! Do not add trace, as routine used by gen_be
   ! if (trace_use) call da_trace_entry("da_transform_vptovv")
   
   !-------------------------------------------------------------------
   ! [1.0] Apply inner-product weighting if vertical_ip /= vertical_ip_0
   !------------------------------------------------------------------- 

   if (vertical_ip /= vertical_ip_0) then
      vp(its:ite,jts:jte,kts:kte) = vp(its:ite,jts:jte,kts:kte) * &
                                    vertical_wgt(its:ite,jts:jte,kts:kte)
   end if

   !-------------------------------------------------------------------
   ! [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
   !-------------------------------------------------------------------

   do m = 1, mz
      do j = jts, jte
         do i = its, ite
            ETVp = sum(evec(j,kts:kte,m) * vp(i,j,kts:kte))
            vv(i,j,m) = ETVp / eval(j,m)
         end do
      end do
   end do

end subroutine da_transform_vptovv


