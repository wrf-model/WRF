subroutine da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)

   !--------------------------------------------------------------------------
   ! Purpose: Control variable transform x' = Uv. 
   !--------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   integer,        intent(in)    :: cv_size ! Size of cv array.
   type(xbx_type), intent(in)    :: xbx  ! For header & non-grid arrays.
   type(be_type),  intent(in)    :: be   ! background errors.
   type(ep_type),  intent(in)    :: ep   ! Ensemble perturbations.
   real,           intent(in)    :: cv(1:cv_size)   ! control variables.
   type(vp_type),  intent(out)   :: vv   ! grdipt/eof cv (local).
   type(vp_type),  intent(inout) :: vp   ! grdipt/level cv (local).

   integer :: i, j, k
   real    :: sdmd, s1md, mu
   real    :: p(kms:kme)
   real    :: mr_a(kms:kme)
   real    :: mr_b(kms:kme)
   real    :: PU, PD, coeff

   if (trace_use) call da_trace_entry("da_transform_vtox")

   call da_zero_x (grid%xa)

   !----------------------------------------------------------------------
   ! [1.0]: Perform vv = u_h cv transform:
   !----------------------------------------------------------------------

   if (global) then
      call da_transform_vtovv_global(cv_size, xbx, be, cv, vv) 
   else
      call da_transform_vtovv(grid, cv_size, be, cv, vv)
   end if

   !----------------------------------------------------------------------
   ! [2.0]: Perform vp = u_v vv transform:
   !----------------------------------------------------------------------

   if (vert_corr == vert_corr_2) then      
      call da_vertical_transform('u', be, grid%xb % vertical_inner_product, vv, vp)
   else
      vp % v1(its:ite,jts:jte,kts:kte) = vv % v1(its:ite,jts:jte,kts:kte)
      vp % v2(its:ite,jts:jte,kts:kte) = vv % v2(its:ite,jts:jte,kts:kte)
      vp % v3(its:ite,jts:jte,kts:kte) = vv % v3(its:ite,jts:jte,kts:kte)
      vp % v4(its:ite,jts:jte,kts:kte) = vv % v4(its:ite,jts:jte,kts:kte)
      vp % v5(its:ite,jts:jte,kts:kte) = vv % v5(its:ite,jts:jte,kts:kte)
   end if

   ! WHY?
   ! if (be % ne > 0) then
   !    vp % alpha(its:ite,jts:jte,1:be%ne) = vv%alpha(its:ite,jts:jte,1:be%ne)
   ! end if

   !----------------------------------------------------------------------  
   ! [3.0]: Perform x = u_p vp transform::
   !----------------------------------------------------------------------

   call da_transform_vptox(grid, vp, be, ep)

   !----------------------------------------------------------------------
   ! [4.0]: Move the following:
   !----------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         if (fg_format == fg_format_wrf) then
            sdmd=0.0
            s1md=0.0
            do k=kts,kte
               mr_a(k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
               mr_b(k) = grid%xb%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))

               sdmd=sdmd+mr_a(k)*grid%xb%dnw(k)
               s1md=s1md+(1.0+mr_b(k))*grid%xb%dnw(k)
            end do

            mu=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md

            p(kte+1)=0.0

            do k=kte,kts,-1
               p(k)=p(k+1)-(mu*(1.0+mr_b(k)) &
                       + grid%xb%psac(i,j)*mr_a(k))*grid%xb%dnw(k)

               grid%xa%p(i,j,k)=0.5*(p(k)+p(k+1))
            end do
         else if (fg_format == fg_format_kma_global) then
            do k=kts,kte
               if (k == kte) then
                  coeff=grid%xb%KMA_B(K)/(grid%xb%KMA_A(K)+grid%xb%KMA_B(K)*grid%xb%psfc(I,J)/100.0)
               else
                  PU = grid%xb%KMA_A(K+1) + grid%xb%KMA_B(K+1)*grid%xb%psfc(I,J)/100.0
                  PD = grid%xb%KMA_A(K ) + grid%xb%KMA_B(K )*grid%xb%psfc(I,J)/100.0
                  coeff=grid%xb%KMA_B(K)  *1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU)) &
                    + PD-PU)&
                    + grid%xb%KMA_B(K+1)*1.0/(PD-PU)**2*(PD*(LOG(PD)-LOG(PU))-PD+PU)
               end if
               ! Here since grid%xa%psfc holds value in Pa. dlnp -> dp
               grid%xa%p(i,j,k) =  grid%xb%p(i,j,k) * grid%xa%psfc(I,J)/100.0 * coeff

            end do
         end if
      end do
   end do

   call da_pt_to_rho_lin(grid)
 
   ! If test_transforms = .true., not "XToY" transform needed to do here:

   if (.not.test_transforms) then
      ! Exchange grid%xa halo region.
#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

      if (sfc_assi_options == 2) then
         call da_transform_xtowtq (grid)
         ! Exchange grid%xa (surface variable) halo region.
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs .or. use_gpsrefobs) then

         ! Now do something for PW
         call da_transform_xtotpw(grid)

         ! GPS Refractivity:
         if (use_gpsrefobs) &
            call da_transform_xtogpsref_lin(grid)

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
              use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
               call da_error(__FILE__,__LINE__, &
                  (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_lin(grid)
         end if
         if (use_ssmitbobs) call da_transform_xtotb_lin (grid)

         ! Exchange grid%xa halo region.
#ifdef DM_PARALLEL
#include "HALO_SSMI_XA.inc"
#endif
      end if
   end if

   ! Compute w increments using Richardson's eqn.

   if (use_radarobs) then
      call da_uvprho_to_w_lin(grid)

      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%wh(i,j,k)=0.5*(grid%xa%w(i,j,k)+grid%xa%w(i,j,k+1))
            end do
         end do
      end do

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
 
      if (use_radarobs .and. use_radar_rf) then
         ! Partition of hydrometeor increments via warm rain process
         call da_moist_phys_lin(grid)
      end if
   end if

   !---------------------------------------------------------------
   ! Polar treatment for Global 
   !---------------------------------------------------------------

   if (global)  then   
      call da_get_vpoles(grid%xa%u,grid%xa%v, &
         ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
      call da_get_spoles(grid%xa%t, &
         ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
      call da_get_spoles(grid%xa%p, &
         ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
      call da_get_spoles(grid%xa%q, &
         ids, ide, jds, jde, ims, ime, jms, jme, kms, kme, its, ite, jts, jte, kts, kte)
      call da_get_spoles(grid%xa%psfc, &
         ids, ide, jds, jde, ims, ime, jms, jme,   1,   1, its, ite, jts, jte,   1,   1)
      call da_set_boundary_xa(grid)
   end if   

   if (trace_use) call da_trace_exit("da_transform_vtox")

end subroutine da_transform_vtox


