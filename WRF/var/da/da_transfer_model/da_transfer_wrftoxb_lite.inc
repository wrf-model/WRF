subroutine da_transfer_wrftoxb_lite(xbx, grid, config_flags)

   !---------------------------------------------------------------------------
   ! Purpose: Transfers fields from WRF to first guess structure.
   !    Author: Xin Zhang,  MMM/NESL/NCAR,  Date: 10/10/2011
   !---------------------------------------------------------------------------

   implicit none
   
   type (xbx_type), intent(inout)     :: xbx        ! Header & non-gridded vars.

   type(domain), intent(inout)        :: grid
   type(grid_config_rec_type), intent(in) :: config_flags

   integer :: i, j, k, ij

   real    :: theta, tmpvar

   real, dimension(ims:ime,jms:jme) :: rgh_fac

   character(len=19) :: current_date

   real :: loc_psac_mean

   real, dimension(jds:jde) :: loc_latc_mean

   integer :: size2d

   real, dimension(kms:kme) :: DDT

   real   :: qvf1, cvpm, cpovcv, ppb, ttb, albn, aln, height, temp
   real, allocatable :: arrayglobal(:,:)

   logical:: no_ppb


   ! If grid%pb does not existed in FG (YRG, 08/26/2010):
     ppb = sum(grid%pb*grid%pb)
     no_ppb = ppb == 0.0

   !---------------------------------------------------------------------------
   ! Set xb array range indices for processor subdomain.
   !---------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_transfer_wrftoxb_lite")

   grid%xb % map  = grid%map_proj
   grid%xb % ds   = grid%dx

   grid%xb % mix = grid%xp % ide - grid%xp % ids + 1
   grid%xb % mjy = grid%xp % jde - grid%xp % jds + 1
   grid%xb % mkz = grid%xp % kde - grid%xp % kds + 1

   !---------------------------------------------------------------------------
   ! WRF-specific fitelds:
   !---------------------------------------------------------------------------

   ptop = grid%p_top

   grid%xb%sigmaf(kte+1) = grid%znw(kte+1)

   grid%xb%znw(kte+1) = grid%znw(kte+1)
   grid%xb%znu(kte+1) = 0.0
 
   do k=kts,kte
      grid%xb%sigmah(k) = grid%znu(k)
      grid%xb%sigmaf(k) = grid%znw(k)

      grid%xb%znu(k) = grid%znu(k)
      grid%xb%znw(k) = grid%znw(k)
      grid%xb%dn(k)  = grid%dn(k)
      grid%xb%dnw(k) = grid%dnw(k)
   end do

   grid%xb % ptop = ptop
      
   !---------------------------------------------------------------------------
   ! Convert WRF fitelds to xb:
   !---------------------------------------------------------------------------

   !---------------------------------------------------------------
   ! Need this to exchange values in the halo region.
   ! grid%xa%u and grid%xa%v are used as temporary arrays and so
   ! it is easy to use the existing exchange scheme.
   !
   ! Note, this is needed as u_2 and v_2 has no guarantee
   ! the most east column, and the most north row are
   ! properly initailized for each tile.
   !---------------------------------------------------------------

   ! Fill the halo region for u and v.

#ifdef DM_PARALLEL
#include "HALO_EM_C.inc"
#endif

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, i, j, k, cvpm, cpovcv, ppb, temp, ttb ) &
   !$OMP PRIVATE ( albn, qvf1, aln, theta )
   do ij = 1 , grid%num_tiles

   do j=grid%j_start(ij), grid%j_end(ij)
      k = kte+1

      do i=its,ite
         grid%p(i,j,k) = 0.0
         grid%xb%map_factor(i,j) = grid%msft(i,j)
         grid%xb%cori(i,j) = grid%f(i,j)
         grid%xb%tgrn(i,j) = grid%sst(i,j)
         if (grid%xb%tgrn(i,j) < 100.0) &
            grid%xb%tgrn(i,j) = grid%tmn(i,j)
         grid%xb%lat(i,j) = grid%xlat(i,j)
         grid%xb%lon(i,j) = grid%xlong(i,j)
         grid%xb%terr(i,j) = grid%ht(i,j)
         grid%xb%snow(i,j) = grid%snowc(i,j)
         grid%xb%lanu(i,j) = grid%lu_index(i,j)
         grid%xb%landmask(i,j) = grid%landmask(i,j)
         grid%xb%xland(i,j) = grid%xland(i,j)
         ! Z. Liu below are variables used by RTTOV
         grid%xb%tsk(i,j) = grid%tsk(i,j)
         grid%xb%smois(i,j) = grid%smois(i,j,1)
         grid%xb%tslb(i,j) = grid%tslb(i,j,1)
         grid%xb%xice(i,j) = grid%xice(i,j)
         grid%xb%ivgtyp(i,j) = grid%ivgtyp(i,j)
         grid%xb%isltyp(i,j) = grid%isltyp(i,j)
         grid%xb%vegfra(i,j) = grid%vegfra(i,j)
         grid%xb%snowh(i,j) = grid%snowh(i,j)*1000.0 ! meter to mm    
      end do

      cvpm =  - (1.0 - gas_constant/cp)
      cpovcv = cp / (cp - gas_constant)

      ! In case of var4d, pb etc. will be recalculated in start_em with realsize=8, 
      ! Howvwer, the originals are computed with realsize=4.
      if ( no_ppb ) then
         do k=kts,kte
            do i=its,ite
               ! The base specific volume (from real.init.code)
               ppb  = grid%znu(k) * grid%mub(i,j) + ptop
               grid%pb(i,j,k) = ppb
               temp = MAX ( iso_temp, base_temp + base_lapse*log(ppb/base_pres) )
               ttb  = temp * (base_pres/ppb)**kappa
               ! ttb  = (base_temp + base_lapse*log(ppb/base_pres)) * &
               !   (base_pres/ppb)**kappa
               albn = (gas_constant/base_pres) * ttb * (ppb/base_pres)**cvpm

               qvf1 = 1.0 + grid%moist(i,j,k,P_QV) / rd_over_rv
               aln  = -1.0 / (grid%mub(i,j)+grid%mu_2(i,j)) * &
                      (albn*grid%mu_2(i,j) + grid%rdnw(k) * &
                      (grid%ph_2(i,j,k+1) - grid%ph_2(i,j,k)))
               ! total pressure:
               grid%xb%p(i,j,k) = base_pres * &
                                 ((gas_constant*(t0+grid%t_2(i,j,k))*qvf1) / &
                                 (base_pres*(aln+albn)))**cpovcv
               ! total density
               grid%xb%rho(i,j,k)= 1.0 / (albn+aln)
               ! pressure purtubation:
               grid%p(i,j,k) = grid%xb%p(i,j,k) - ppb
            end do
         end do
      else
         do k=kts,kte
            do i=its,ite
               ppb = grid%pb(i,j,k)
               temp = MAX ( iso_temp, base_temp + base_lapse*log(ppb/base_pres) )
               ttb  = temp * (base_pres/ppb)**kappa
               ! ttb  = (base_temp + base_lapse*log(ppb/base_pres)) * &
               !   (base_pres/ppb)**kappa
               albn = (gas_constant/base_pres) * ttb * (ppb/base_pres)**cvpm

               qvf1 = 1.0 + grid%moist(i,j,k,P_QV) / rd_over_rv
               aln  = -1.0 / (grid%mub(i,j)+grid%mu_2(i,j)) * &
                      (albn*grid%mu_2(i,j) + grid%rdnw(k) * &
                      (grid%ph_2(i,j,k+1) - grid%ph_2(i,j,k)))
               grid%xb%p(i,j,k) = grid%pb(i,j,k) + grid%p(i,j,k)
               ! total density
               grid%xb%rho(i,j,k)= 1.0 / (albn+aln)
            end do
         end do
      endif

      do k=kts,kte+1
         do i=its,ite
            grid%xb%hf(i,j,k) = (grid%phb(i,j,k)+grid%ph_2(i,j,k))/gravity
            grid%xb%w (i,j,k) = grid%w_2(i,j,k)
         end do
      end do

      do k=kts,kte
         do i=its,ite
            grid%xb%u(i,j,k) = 0.5*(grid%u_2(i,j,k)+grid%u_2(i+1,j,k))
            grid%xb%v(i,j,k) = 0.5*(grid%v_2(i,j,k)+grid%v_2(i,j+1,k))
            grid%xb%wh(i,j,k)= 0.5*(grid%xb%w(i,j,k)+grid%xb%w(i,j,k+1))
            grid%xb%h(i,j,k) = 0.5*(grid%xb%hf(i,j,k)+grid%xb%hf(i,j,k+1))

            if ( num_pseudo == 0 ) then
               grid%moist(i,j,k,P_QV) = max(grid%moist(i,j,k,P_QV), qlimit)
            end if
            grid%xb%q(i,j,k) = grid%moist(i,j,k,P_QV)

            theta = t0 + grid%t_2(i,j,k)
            grid%xb%t(i,j,k) = theta*(grid%xb%p(i,j,k)/base_pres)**kappa

            ! Convert to specific humidity from mixing ratio of water vapor:
            grid%xb%q(i,j,k)=grid%xb%q(i,j,k)/(1.0+grid%xb%q(i,j,k))
   
            ! Background qrn needed for radar radial velocity assmiilation:

            if (size(grid%moist,dim=4) >= 4) then
               grid%xb%qcw(i,j,k) = max(grid%moist(i,j,k,p_qc), 0.0)
               grid%xb%qrn(i,j,k) = max(grid%moist(i,j,k,p_qr), 0.0)
               grid%xb%qt (i,j,k) = grid%xb%q(i,j,k) + grid%xb%qcw(i,j,k) + &
                  grid%xb%qrn(i,j,k)
            end if

            if (size(grid%moist,dim=4) >= 6) then
               grid%xb%qci(i,j,k) = max(grid%moist(i,j,k,p_qi), 0.0)
               grid%xb%qsn(i,j,k) = max(grid%moist(i,j,k,p_qs), 0.0)
            end if

            if (size(grid%moist,dim=4) >= 7) then
               grid%xb%qgr(i,j,k) = max(grid%moist(i,j,k,p_qg), 0.0)
            end if

            if ( config_flags%mp_physics == 3 ) then   ! WSM3-class scheme
               if ( grid%xb%t(i,j,k) <= t_kelvin ) then
                  grid%xb%qci(i,j,k) = grid%xb%qcw(i,j,k)
                  grid%xb%qcw(i,j,k) = 0.0
                  grid%xb%qsn(i,j,k) = grid%xb%qrn(i,j,k)
                  grid%xb%qrn(i,j,k) = 0.0
               end if
            end if

         end do
      end do

      do i=its,ite
         grid%xb%psac(i,j) = grid%mub(i,j)+grid%mu_2(i,j)
! To make the Psfc consistent with WRF (YRG, 04/06/2010):
         grid%xb%psfc(i,j) = grid%psfc(i,j)

      end do
   end do

   end do
   !$OMP END PARALLEL DO

   !---------------------------------------------------------------------------
   ! [3.0] Calculate vertical inner product for use in vertical transform:
   !---------------------------------------------------------------------------
      
   if (vertical_ip == vertical_ip_sqrt_delta_p) then
      ! Vertical inner product is sqrt(Delta p):
      do k=kts,kte
         grid%xb % vertical_inner_product(its:ite,jts:jte,k) = &
            sqrt(grid%xb % psac(its:ite,jts:jte) * grid%xb%sigmah(k))
      end do 
   else if (vertical_ip == vertical_ip_delta_p) then

      ! Vertical inner product is Delta p:
      do k=1,grid%xb%mkz
         grid % xb % vertical_inner_product(its:ite,jts:jte,k) = &
         grid % xb % psac(its:ite,jts:jte) * grid%xb%sigmah(k)
      end do
   end if

   !---------------------------------------------------------------------------
   ! Calculate saturation vapour pressure and relative humidity:
   !---------------------------------------------------------------------------

   !$OMP PARALLEL DO &
   !$OMP PRIVATE ( ij, k, j, i )
   do ij = 1 , grid%num_tiles
      do k=kts,kte
         do j=grid%j_start(ij),grid%j_end(ij)
            do i=its,ite
               call da_tpq_to_rh(grid%xb % t(i,j,k), grid%xb % p(i,j,k), &
                  grid%xb % q(i,j,k), grid%xb %es(i,j,k), grid%xb %qs(i,j,k), &
                  grid%xb %rh(i,j,k))
            end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO 

   ! Fill the halo region for xb        

#ifdef DM_PARALLEL
#include "HALO_XB.inc"
#endif

   ! Calculate time step from one dimensional cloud model parameterization

   if (dt_cloud_model) then
      do j = jts, jte
         do i = its, ite
            call da_cloud_model (grid%xb%t(I,J,:),  grid%xb%p(I,J,:), &
               grid%xb%q(I,J,:), grid%xb%qcw(I,J,:), grid%xb%qrn(I,J,:), &
               grid%xb%h(I,J,:), grid%xb%hf(I,J,:), ddt, kts, kte)

            do k = kts, kte
               grid%xb%delt(i,j,k) = DDT(k)
            end do
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_transfer_wrftoxb_lite")

end subroutine da_transfer_wrftoxb_lite

