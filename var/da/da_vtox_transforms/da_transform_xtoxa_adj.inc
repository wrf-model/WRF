subroutine da_transform_xtoxa_adj(grid)

   !--------------------------------------------------------------------------------------------------
   ! Purpose: Transfers fields from WRF TL fields to diagnostic fields needed by observational operators
   !          Adjoint   
   !--------------------------------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid

   integer :: i, j, k
   real    :: sdmd, s1md, mu
   real    :: p(kms:kme), mr_a(kms:kme), mr_b(kms:kme)
   real    :: PU, PD, coeff

   if (trace_use) call da_trace_entry("da_transform_xtoxa_adj")

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
   if ( Use_RadarObs)  then
      do k=kts,kte
         do j=jts,jte
            do i=its,ite
               grid%xa%w(i,j,k)=grid%xa%w(i,j,k)+0.5*grid%xa%wh(i,j,k)
               grid%xa%w(i,j,k+1)=grid%xa%w(i,j,k+1)+0.5*grid%xa%wh(i,j,k)
               grid%xa%wh(i,j,k)=0.0
            end do
         end do
      end do
      if ( .not. var4d .and. cloud_cv_options == 1 ) call da_uvprho_to_w_adj(grid)
   end if

   if ( (use_radarobs .and. use_radar_rf) .or. (use_rad .and. crtm_cloud) ) then
      if ( cloud_cv_options == 1 .and. use_3dvar_phy) then
         ! Partition of hydrometeor increments via warm rain process
         call da_moist_phys_adj(grid)
      end if
   end if

   !-------------------------------------------------------------------------
   ! If test_transforms = .true., not "XToY" transform needed to do here (YRG):

   if (.not.test_transforms) then
      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. use_gpsztdobs .or. &
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
         if ( use_gpsrefObs .or. use_gpsztdObs) then 
            if (use_gpsztdObs) call da_transform_xtoztd_adj(grid)  
            call da_transform_XTogpsref_adj(grid)  
         endif 

         ! Now for PW.
         call da_transform_xtotpw_adj(grid)
      end if

      if (sfc_assi_options == 2) call da_transform_xtowtq_adj(grid)
   end if
 
   call da_pt_to_rho_adj(grid)

   if ( .not. var4d ) then

   do j=jts,jte
      do i=its,ite
         if ((fg_format==fg_format_wrf_arw_regional) .or. &
             (fg_format==fg_format_wrf_arw_global  )  ) then
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

   endif ! only for 3dvar

   if (global) then     
      call da_set_boundary_xa(grid)
   end if

   if (trace_use) call da_trace_exit("da_transform_xtoxa_adj")

end subroutine da_transform_xtoxa_adj


