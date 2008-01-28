subroutine da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   !--------------------------------------------------------------------------
   ! Purpose: Control variable transform Adjoint
   !--------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   integer,        intent(in)    :: cv_size ! Size of cv array.
   type(xbx_type), intent(in)    :: xbx  ! For header & non-grid arrays.
   type(be_type),  intent(in)    :: be   ! background errors.
   type(ep_type),  intent(in)    :: ep   ! ensemble perturbation structure.
   type(vp_type),  intent(out)   :: vp   ! grdipt/level cv (local).
   type(vp_type),  intent(out)   :: vv   ! grdipt/eof cv (local).
   real,           intent(inout) :: cv(1:cv_size) ! control variables.

   integer :: i, j, k
   real    :: sdmd, s1md, mu
   real    :: p(kms:kme), mr_a(kms:kme), mr_b(kms:kme)
   real    :: PU, PD, coeff

   if (trace_use) call da_trace_entry("da_transform_vtox_adj")

   !-------------------------------------------------------------------------
   ! Polar treatment for Global
   !-------------------------------------------------------------------------

   if (global) then     
      ! Poles treatment for global WRFVAR
      call da_get_avpoles(grid%xa%u,grid%xa%v, &
         ids,ide,jds,jde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
      call da_get_aspoles(grid%xa%t, &
         ids,ide,jds,jde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
      call da_get_aspoles(grid%xa%p, &
         ids,ide,jds,jde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
      call da_get_aspoles(grid%xa%q, &
         ids,ide,jds,jde,ims,ime,jms,jme,kms,kme,its,ite,jts,jte,kts,kte)
      call da_get_aspoles(grid%xa%psfc, &
         ids,ide,jds,jde,ims,ime,jms,jme,1,1,its,ite,jts,jte,1,1)
   end if     

   ! Compute w increments using Richardson's eqn.
   if (use_radarobs) then
      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%w(i,j,k)=grid%xa%w(i,j,k)+0.5*grid%xa%wh(i,j,k)
               grid%xa%w(i,j,k+1)=grid%xa%w(i,j,k+1)+0.5*grid%xa%wh(i,j,k)
               grid%xa%wh(i,j,k)=0.0
            end do
         end do
      end do

      call da_uvprho_to_w_adj(grid)
 
      if (use_radarobs .and. use_radar_rf) then
         ! Partition of hydrometeor increments via warm rain process
         call da_moist_phys_adj(grid)
      end if
   end if

   !-------------------------------------------------------------------------
   ! If test_transforms = .true., not "XToY" transform needed to do here (YRG):

   if (.not.test_transforms) then
      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs .or. use_gpsrefobs) then
         if (use_ssmitbobs) call da_transform_xtotb_adj(grid)
         if (use_ssmt1obs .or. use_ssmt2obs .or. &
             use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
               call da_error(__FILE__,__LINE__, &
                  (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_adj(grid)
         end if

         ! GPS Refractivity:
         if (use_gpsrefobs) call da_transform_xtogpsref_adj(grid)

         ! Now for PW.
         call da_transform_xtotpw_adj(grid)
      end if

      if (sfc_assi_options == 2) call da_transform_xtowtq_adj(grid)
   end if
 
   call da_pt_to_rho_adj(grid)
   do j=jts,jte
      do i=its,ite
         if (fg_format == fg_format_wrf) then
            mu=0.0
            s1md=0.0

            p(:)=0.0

            do k=kts,kte
               mr_b(k) = grid%xb%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))
               s1md=s1md+(1.0+mr_b(k))*grid%xb%dnw(k)

               p(k) = p(k) + 0.5*grid%xa%p(i,j,k)
               p(k+1) = p(k+1) + 0.5*grid%xa%p(i,j,k)

               mu = mu - p(k)*(1.0+mr_b(k))*grid%xb%dnw(k)

               mr_a(k) = - p(k)*grid%xb%psac(i,j)*grid%xb%dnw(k)
               p(k+1) = p(k+1) + p(k)
            end do

            grid%xa%psfc(i,j) = grid%xa%psfc(i,j) - mu/s1md
            sdmd=-mu*grid%xb%psac(i,j)/s1md

            do k=kts,kte
               mr_a(k) = mr_a(k) + sdmd*grid%xb%dnw(k)
               grid%xa%q(i,j,k) = grid%xa%q(i,j,k) + mr_a(k)/(1.0 - grid%xb%q(i,j,k))**2
            end do
         else if (fg_format == fg_format_kma_global)then
            do k=kts,kte
               if (k == kte) then
                  coeff = grid%xb%KMA_B(K)/(grid%xb%KMA_A(K)+grid%xb%KMA_B(K)* &
                     grid%xb%psfc(I,J)/100.0)
               else
                  PU = grid%xb%KMA_A(K+1) + grid%xb%KMA_B(K+1)*grid%xb%psfc(I,J)/100.0
                  PD = grid%xb%KMA_A(K ) + grid%xb%KMA_B(K )*grid%xb%psfc(I,J)/100.0
                  coeff=grid%xb%KMA_B(K)*1.0/(PD-PU)**2*(-PU*(LOG(PD)-LOG(PU))+PD-PU)&
                     + grid%xb%KMA_B(K+1)*1.0/(PD-PU)**2*(PD*(LOG(PD)-LOG(PU))-PD+PU)
               end if
      
               grid%xa%psfc(i,j) = grid%xa % psfc(i,j) + &
                  grid%xb%p(i,j,k) * grid%xa % p(i,j,k)/100.0 * coeff 
            end do
         end if
      end do
   end do

   if (global) then     
      call da_set_boundary_xa(grid)
   end if

   !-------------------------------------------------------------------------
   ! [3.0]: Perform x = u_p vp transform::
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vp)
   call da_transform_vptox_adj(grid, vp, be, ep)

   !-------------------------------------------------------------------------
   ! [2.0]: Perform vp = u_v vv transform:
   !-------------------------------------------------------------------------

   call da_zero_vp_type (vv)

   ! Uv for alpha is a null transform:
   ! if (be % ne > 0) then
   !    vv % alpha(its:ite,jts:jte,1:be%ne) = &
   !    vp % alpha(its:ite,jts:jte,1:be%ne) 
   ! end if

   if (vert_corr == 2) then      
      call da_vertical_transform('u_adj', be, &
         grid%xb % vertical_inner_product, vv, vp)
   else
      vv % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte)
      vv % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte)
      vv % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte)
      vv % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte)
      vv % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte)
   end if

   !-------------------------------------------------------------------------
   ! [1.0]: perform vv = u_h cv transform:
   !-------------------------------------------------------------------------

   if (global) then
      call da_transform_vtovv_global_adj(cv_size, xbx, be, cv, vv)
   else
      call da_transform_vtovv_adj(grid, cv_size, be, cv, vv)
   end if

   if (trace_use) call da_trace_exit("da_transform_vtox_adj")

end subroutine da_transform_vtox_adj


