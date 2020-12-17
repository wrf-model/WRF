subroutine da_transform_xtoxa(grid, wpec_on)

   !--------------------------------------------------------------------------------------------------
   ! Purpose: Transfers fields from WRF TL fields to diagnostic fields needed by observational operators
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !--------------------------------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   logical, intent(in), optional :: wpec_on

   integer :: i, j, k
   real    :: sdmd, s1md, mu
   real    :: p(kms:kme)
   real    :: mr_a(kms:kme)
   real    :: mr_b(kms:kme)
   real    :: PU, PD, coeff
   real    :: geoh(kms:kme)
   logical :: calc_wpec_term

   if (trace_use) call da_trace_entry("da_transform_xtoxa")

   !----------------------------------------------------------------------
   ! [4.0]: Move the following:
   !----------------------------------------------------------------------

   calc_wpec_term = .false.
   if ( present(wpec_on) ) then
      if ( wpec_on ) calc_wpec_term =.true.
   end if

   if ( .not. var4d ) then ! for 4dvar, xa%p comes from the model

   do j=jts,jte
      do i=its,ite
        if ((fg_format==fg_format_wrf_arw_regional) .or. &
            (fg_format==fg_format_wrf_arw_global  ) ) then

            sdmd=0.0
            s1md=0.0
            do k=kts,kte
               mr_a(k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
               mr_b(k) = grid%xb%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))

               sdmd=sdmd+mr_a(k)*grid%xb%dnw(k)
               s1md=s1md+(1.0+mr_b(k))*grid%xb%dnw(k)
            end do

            mu=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
            if ( calc_wpec_term ) then
               grid%xa%mu(i,j) = mu
            end if

            p(kte+1)=0.0

            do k=kte,kts,-1
               p(k)=p(k+1)-(c1h(k)*mu*(1.0+mr_b(k)) &
                       + (c1h(k)*grid%xb%psac(i,j)+c2h(k))*mr_a(k))*grid%xb%dnw(k)

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

   endif ! only for 3dvar

   call da_pt_to_rho_lin(grid)

   if ( .not. var4d .and. calc_wpec_term ) then
      do j=jts,jte
         do i=its,ite
            if ((fg_format==fg_format_wrf_arw_regional) .or. &
               (fg_format==fg_format_wrf_arw_global  ) ) then
               geoh(kts)=0.0
               do k=kts,kte
                  geoh(k+1)=geoh(k)+(-c1h(k)*grid%xa%mu(i,j) &
                          + grid%xa%rho(i,j,k)*(c1h(k)*grid%xb%psac(i,j)+c2h(k)) &
                          /grid%xb%rho(i,j,k))* grid%xb%dnw(k)/grid%xb%rho(i,j,k)
                  grid%xa%geoh(i,j,k)=0.5*(geoh(k)+geoh(k+1))
               end do
            end if
         end do
      end do
   end if

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe + 1
     ide = ide + 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe + 1
     jde = jde + 1
  end if
#endif
#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

  if ( .not. var4d .and. calc_wpec_term ) then
#ifdef DM_PARALLEL
#include "HALO_XA_WPEC.inc"
#include "HALO_XB_WPEC.inc"
#endif
  end if

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe - 1
     ide = ide - 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe - 1
     jde = jde - 1
  end if
#endif

   ! If test_transforms = .true., not "XToY" transform needed to do here:

   if (.not.test_transforms) then
      ! Exchange grid%xa halo region.

      if (sfc_assi_options == 2) then
         call da_transform_xtowtq (grid)
         ! Exchange grid%xa (surface variable) halo region.
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. use_gpsztdobs .or. &
          use_ssmitbobs .or. use_ssmiretrievalobs .or. use_gpsrefobs .or. use_gpsephobs) then

         ! Now do something for PW
         call da_transform_xtotpw(grid)

         ! Space-based GPS Refractivity, GPS Excess Phase, and Ground-based GPS ZTD:
         if ( use_gpsrefObs .or. use_gpsephObs .or. use_gpsztdObs) then
            call da_transform_xtogpsref_lin(grid)  
            if (use_GpsztdObs) call da_transform_xtoztd_lin(grid) 
         endif 

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

   if ( Use_RadarObs ) then
      if ( .not. var4d .and. cloud_cv_options == 1) call da_uvprho_to_w_lin(grid)

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
   end if

   if ( cloud_cv_options == 1 )then
      ! Partition of hydrometeor increments via warm rain process
      call da_moist_phys_lin(grid)
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

   if (trace_use) call da_trace_exit("da_transform_xtoxa")

end subroutine da_transform_xtoxa


