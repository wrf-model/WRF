subroutine da_transfer_xatowrf(grid, config_flags)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRF increments
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !
   !  The following WRF fields are modified:  
   !    grid%u_2
   !    grid%v_2
   !    grid%w_2
   !    grid%mu_2
   !    grid%ph_2
   !    grid%t_2
   !    grid%moist
   !    grid%p
   !    grid%psfc
   !    grid%t2, grid%q2, grid%u10, grid%v10, grid%th2
   !
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)            :: grid
   type(grid_config_rec_type), intent(in) :: config_flags

   integer :: i, j, k

   real    :: sdmd, s1md

   ! arrays to hold wrf increments on the c-grid 

#ifdef A2C
   real, dimension(ims:ime,jms:jme, kms:kme) :: q_cgrid, ph_cgrid
#else
   real, dimension(ims:ime,jms:jme, kms:kme) :: &
      u_cgrid, v_cgrid, q_cgrid, ph_cgrid
#endif

   real, dimension(ims:ime,jms:jme) :: mu_cgrid

   real :: t_full, p_full, rho_dry, q_full, ph_full, ph_xb_hd, &
           qvf1, qvf2, qvf1_b, qvf2_b

   real :: uu, vv, ps1, ts1, qv1, height
   real :: zs1, zs2, pf2, theta, thetam, mu_full, pfu, pfd, phm, cvpm, p2m
   real, dimension(kms:kme) :: ald, ph
   logical :: has_lsm_info

   if (trace_use) call da_trace_entry("da_transfer_xatowrf")

   has_lsm_info = .false.
   if ( grid%sf_surface_physics == 2 ) then
       if ( sum(grid%hfx*grid%hfx)   > 0.0 .and. &
            sum(grid%qfx*grid%qfx)   > 0.0 ) then
          has_lsm_info = .true.
       end if
   end if

   ! To keep the background PH perturbation:

   do j=jts,jte
      do i=its,ite
         do k=kts, kte+1
            ph_cgrid(i,j,k) = grid%ph_2(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it its easy.
   !---------------------------------------------------------------------------

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            if ((grid%xb%q(i,j,k)+grid%xa%q(i,j,k)) < 0.0) then
               q_cgrid(i,j,k) =-grid%xb%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
            else
               q_cgrid(i,j,k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
            end if
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] compute increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         sdmd=0.0
         s1md=0.0
         do k=kts,kte
            sdmd=sdmd+q_cgrid(i,j,k)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do

         mu_cgrid(i,j)=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] compute pressure increments 
   !---------------------------------------------------------------------------

   if ( .not. var4d ) then ! for 4dvar, it is usefulless

   ! Tangent linear code for grid%xa%p (based on WRF "real.init.code") 
   ! developed by Y.-R. Guo 05/13/2004:

   do j=jts,jte
      do i=its,ite

         k = kte
         qvf1   = 0.5*(q_cgrid(i,j,k)+q_cgrid(i,j,k))
         qvf1_b = 0.5*(grid%moist(i,j,k,P_QV)+grid%moist(i,j,k,P_QV))
         qvf2   = - qvf1 / ((1.0+qvf1_b)*(1.0+qvf1_b))
         qvf2_b = 1.0/(1.0+qvf1_b)
         qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
         qvf1_b = qvf1_b*qvf2_b
         grid%xa%p(i,j,k) = (-0.5/grid%rdnw(k)) * &
                    ((c1f(k)*mu_cgrid(i,j)+qvf1*c1f(k)*grid%mub(i,j)) / qvf2_b &
                     -(c1f(k)*grid%mu_2(i,j)+qvf1_b*c1f(k)*grid%mub(i,j))*qvf2/(qvf2_b*qvf2_b))

         do k = kte-1,1,-1
            qvf1   = 0.5*(q_cgrid(i,j,k)+q_cgrid(i,j,k+1))
            qvf1_b = 0.5*(grid%moist(i,j,k,P_QV)+grid%moist(i,j,k+1,P_QV))
            qvf2   = - qvf1 / ((1.0+qvf1_b)*(1.0+qvf1_b))
            qvf2_b = 1.0/(1.0+qvf1_b)
            qvf1   = qvf1*qvf2_b + qvf1_b*qvf2
            qvf1_b = qvf1_b*qvf2_b
            grid%xa%p(i,j,k) = grid%xa%p(i,j,k+1)  &
                       - (1.0/grid%rdn(k+1)) * &
                       ((c1f(k)*mu_cgrid(i,j)+qvf1*c1f(k)*grid%mub(i,j)) / qvf2_b &
                        -(c1f(k)*grid%mu_2(i,j)+qvf1_b*c1f(k)*grid%mub(i,j))*qvf2/(qvf2_b*qvf2_b))
         end do

      end do
   end do

   else
    
      do k=kts, kte
        do j=jts,jte
           do i=its,ite
              grid%xa%p(i,j,k) = 0.
           end do
        end do
      end do

   endif 

   ! update perturbation pressure

   do k=kts, kte
     do j=jts,jte
        do i=its,ite
           grid%p(i,j,k) = grid%p(i,j,k) + grid%xa%p(i,j,k)
        end do
     end do
   end do

   ! Adjust grid%xa%q to make grid%xb%q + grid%xa%q > 0.0

   if (check_rh == check_rh_tpw) then
      ! Shu-Hua~s TPW conservation:
      call da_check_rh(grid)
   else if (check_rh == check_rh_simple) then
      ! Simple resetting to max/min values:
      call da_check_rh_simple(grid)
   end if

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            q_cgrid(i,j,k) = grid%xa%q(i,j,k)/(1.0 - grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] Convert temperature increments into theta increments 
   !       Evaluate also the increments of (1/rho) and geopotential
   !---------------------------------------------------------------------------

   if (print_detail_xa) then
      write(unit=stdout, fmt='(a, e24.12)') &
         'sum(abs(grid%xa%t(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%t(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xa%p(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xa%p(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xb%t(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xb%t(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%xb%p(its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%xb%p(its:ite,jts:jte,kts:kte))), &
         'sum(abs(grid%t_2 (its:ite,jts:jte,kts:kte)))=', &
         sum(abs(grid%t_2 (its:ite,jts:jte,kts:kte)))

       write(unit=stdout, fmt='(2(2x, a, e20.12))') &
          'maxval(grid%xa%u(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%u(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%u(its:ite,jts:jte,kts:kte))=', & 
          minval(grid%xa%u(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%v(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%v(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%v(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%v(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%t(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%t(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%t(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%t(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%q(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%q(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%q(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%q(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%p(its:ite,jts:jte,kts:kte))=', &
          maxval(grid%xa%p(its:ite,jts:jte,kts:kte)), &
          'minval(grid%xa%p(its:ite,jts:jte,kts:kte))=', &
          minval(grid%xa%p(its:ite,jts:jte,kts:kte)), &
          'maxval(grid%xa%psfc(its:ite,jts:jte))   =', &
          maxval(grid%xa%psfc(its:ite,jts:jte)), &
          'minval(grid%xa%psfc(its:ite,jts:jte))   =', &
          minval(grid%xa%psfc(its:ite,jts:jte))
   end if

   IF (grid%hypsometric_opt == 1) THEN

   do j=jts,jte
      do i=its,ite

         ph_full  = grid%ht(i,j) * gravity
         ph_xb_hd = grid%ht(i,j) * gravity
         do k = kts, kte
            ! To obtain all of the full fitelds: t, p, q(mixing ratio), rho
            t_full   = grid%xa%t(i,j,k) + grid%xb%t(i,j,k)
            p_full   = grid%xa%p(i,j,k) + grid%xb%p(i,j,k)
            q_full   = grid%moist(i,j,k,P_QV) + q_cgrid(i,j,k)

            ! Note: According to WRF, this is the dry air density used to
            !       compute the geopotential height: 
            rho_dry = p_full / (gas_constant*t_full*(1.0+q_full/rd_over_rv))

            ! To compute the theta increment with the full fields:
            grid%t_2(i,j,k) = t_full*(base_pres/p_full)**kappa - t0

            ! The full field of analysis ph:
            ph_full  = ph_full  &
                       - grid%xb%dnw(k) * &
                       ((c1h(k)*grid%xb%psac(i,j)+c2h(k))+c1h(k)*mu_cgrid(i,j)) &
                       / rho_dry

            ! background hydrostatic phi:
            ph_xb_hd  = ph_xb_hd  &
                       - grid%xb%dnw(k) * &
                       (c1h(k)*grid%xb%psac(i,j)+c2h(k)) / grid%xb%rho(i,j,k)

            ! The analysis perturbation = Hydro_ph - base_ph + nonhydro_xb_ph:
            grid%ph_2(i,j,k+1) = ph_full - grid%phb(i,j,k+1) &
                            + (grid%xb%hf(i,j,k+1)*gravity - ph_xb_hd)
         end do
      end do
   end do

   ELSE IF  (grid%hypsometric_opt == 2) THEN

   ! Geopotential increment reflecting hypsometric_opt: wee 11/29/2011

   cvpm =  - (1.0 - gas_constant/cp)

   DO j=jts,jte
   DO i=its,ite

      mu_full = grid%mub(i,j)+grid%mu_2(i,j)+mu_cgrid(i,j)

      ! Compute geopotential (using dry inverse density and dry pressure)
      ph      = 0.0
      ph(kts) = grid%ht(i,j) * gravity
      DO k = kts, kte
         p_full = grid%xb%p(i,j,k) + grid%xa%p(i,j,k)
         t_full = grid%xb%t(i,j,k) + grid%xa%t(i,j,k)
         q_full = grid%moist(i,j,k,P_QV) + q_cgrid(i,j,k)
         theta  = t_full*(base_pres/p_full)**kappa

         ! Update potential temperature
         grid%t_2(i,j,k) = theta - t0

         ! Compute dry inverse density using the equation of state
         thetam = theta*(1.0 + q_full/rd_over_rv)
         ald(k) = (gas_constant/base_pres)*thetam*(p_full/base_pres)**cvpm

      ! Dry mass is purely hydrostatic: Native approach in WRF

          pfu = mu_full*c3f(k+1) + c4f(k+1) + grid%p_top
          pfd = mu_full*c3f(k)   + c4f(k)   + grid%p_top
          phm = mu_full*c3h(k)   + c4h(k)   + grid%p_top
          ph(k+1) = ph(k) + ald(k)*phm*LOG(pfd/pfu) 
          grid%ph_2(i,j,k+1) = ph(k+1) - grid%phb(i,j,k+1)
       END DO

      ! Update geopotential perturbation

!      grid%ph_2(i,j,:) = 0.0
!      DO k= kts, kte+1
!         grid%ph_2(i,j,k) = ph(k) - grid%phb(i,j,k)
!      END DO

   END DO
   END DO

   ENDIF ! hypsometric_opt

   ! To compute the geopotential height increment:

   do k=kts, kte+1
     do j=jts,jte
        do i=its,ite
           ph_cgrid(i,j,k) = grid%ph_2(i,j,k) - ph_cgrid(i,j,k)
        end do
     end do
   end do

   ! ========================
   ! Write out the increment:
   ! ========================

   if (write_increments) then
      write(unit=stdout,fmt='(/"Write out increment for plotting......")')
      call da_write_increments (grid, q_cgrid, mu_cgrid, ph_cgrid)
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
#else
   ! CONVERT FROM A-GRID TO C-GRID

#endif

#ifdef DM_PARALLEL
#include "HALO_XA_A.inc"
#endif

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
#else
   ! Fill the boundary

   ! The southern boundary
   if (jts == jds) then
      grid%xa%v(its:ite,jts-1,kts:kte)=2.0*grid%xa%v(its:ite,jts  ,kts:kte) &
                            -    grid%xa%v(its:ite,jts+1,kts:kte)
   end if

   ! The northern boundary
   if (jte == jde) then
      grid%xa%v(its:ite,jte+1,kts:kte)=2.0*grid%xa%v(its:ite,jte  ,kts:kte) &
                            -    grid%xa%v(its:ite,jte-1,kts:kte)
   end if

   ! The western boundary
   if (its == ids) then
      grid%xa%u(its-1,jts:jte,kts:kte)=2.0*grid%xa%u(its  ,jts:jte,kts:kte) &
                            -    grid%xa%u(its+1,jts:jte,kts:kte)
   end if

   ! The eastern boundary
   if (ite == ide) then
      grid%xa%u(ite+1,jts:jte,kts:kte)=2.0*grid%xa%u(ite  ,jts:jte,kts:kte) &
                            -    grid%xa%u(ite-1,jts:jte,kts:kte)
   end if

   do k=kts,kte
      do j=jts,jte+1
         do i=its,ite+1
            u_cgrid(i,j,k)=0.5*(grid%xa%u(i-1,j  ,k)+grid%xa%u(i,j,k))
            v_cgrid(i,j,k)=0.5*(grid%xa%v(i  ,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   !------------------------------------------------------------------------
   ! For later plot and comparation Purpose only, zero out the unused var.
   !------------------------------------------------------------------------

   ! The northern boundary
   if (jte == jde) then
      u_cgrid(its:ite+1,jte+1,kts:kte)=0.0
   end if

   ! The eastern boundary
   if (ite == ide) then
      v_cgrid(ite+1,jts:jte+1,kts:kte)=0.0
   end if
#endif
   !---------------------------------------------------------------------------
   ! [5.0] add increment to the original guess and update "grid"
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         grid%mu_2(i,j) = grid%mu_2(i,j) + mu_cgrid(i,j)
         grid%w_2(i,j,kte+1)=  grid%w_2(i,j,kte+1) + grid%xa%w(i,j,kte+1)
         grid%psfc(i,j) = grid%psfc(i,j) + grid%xa%psfc(i,j)
      end do

      do k=kts,kte
         do i=its,ite
#ifndef A2C
            grid%u_2(i,j,k) = grid%u_2(i,j,k) + u_cgrid(i,j,k)
            grid%v_2(i,j,k) = grid%v_2(i,j,k) + v_cgrid(i,j,k)
#endif
            grid%w_2(i,j,k) = grid%w_2(i,j,k) + grid%xa%w(i,j,k)

            ! (xb%q+xa%q in specific humidity) >= 0.0
            ! does not guarantee that (qv+q_cgrid in mixing ratio) >= 0.0
            ! for example, when xa%q is negative, q_cgrid is a lager negative value.
            ! impose a minimum value to prevent negative final qv
            if ( num_pseudo == 0 ) then
               grid%moist(i,j,k,P_QV) = max(grid%moist(i,j,k,P_QV)+q_cgrid(i,j,k), qlimit)
            else
               grid%moist(i,j,k,P_QV) = grid%moist(i,j,k,P_QV)+q_cgrid(i,j,k)
            end if

            if (size(grid%moist,dim=4) >= 4) then
               grid%moist(i,j,k,p_qc) = max(grid%moist(i,j,k,p_qc) + grid%xa%qcw(i,j,k), 0.0)
               grid%moist(i,j,k,p_qr) = max(grid%moist(i,j,k,p_qr) + grid%xa%qrn(i,j,k), 0.0)
            end if

            if (size(grid%moist,dim=4) >= 6) then
               grid%moist(i,j,k,p_qi) = max(grid%moist(i,j,k,p_qi) + grid%xa%qci(i,j,k), 0.0)
               grid%moist(i,j,k,p_qs) = max(grid%moist(i,j,k,p_qs) + grid%xa%qsn(i,j,k), 0.0)
            end if

            if (size(grid%moist,dim=4) >= 7) then
               grid%moist(i,j,k,p_qg) = max(grid%moist(i,j,k,p_qg) + grid%xa%qgr(i,j,k), 0.0)
            end if
         end do
      end do
   end do

#ifdef A2C
   do j=jts,jte
      do k=kts,kte
         do i=its,ite+1
          grid%u_2(i,j,k) = grid%u_2(i,j,k) + grid%xa%u(i,j,k)
         end do
      end do
   end do

   do j=jts,jte+1
      do k=kts,kte
         do i=its,ite
          grid%v_2(i,j,k) = grid%v_2(i,j,k) + grid%xa%v(i,j,k)
         end do
      end do
   end do
#else
   ! The northern boundary
   if (jte == jde) then
      j=jte+1
      do k=kts,kte
         do i=its,ite
            grid%v_2(i,j,k) = grid%v_2(i,j,k) + v_cgrid(i,j,k)
         end do
      end do
   end if

   ! The eastern boundary
   if (ite == ide) then
      i=ite+1
      do k=kts,kte
         do j=jts,jte
            grid%u_2(i,j,k) = grid%u_2(i,j,k) + u_cgrid(i,j,k)
         end do
      end do
   end if
#endif

#ifdef DM_PARALLEL
#include "HALO_EM_C.inc"
#endif
! re-calculate T2, Q2, U10, V10, TH2 using updated fields

   do j=jts,jte
      do i=its,ite
         uu = 0.5*(grid%u_2(i,j,kts)+grid%u_2(i+1,j,kts) )
         vv = 0.5*(grid%v_2(i,j,kts)+grid%v_2(i,j+1,kts) )
         ps1 = grid%p(i,j,kts)   + grid%pb(i,j,kts)
         ts1 = (t0+grid%t_2(i,j,kts))*(ps1/base_pres)**kappa
         qv1 = grid%moist(i,j,kts, p_qv) !qv1, input to da_sfc_wtq, is mixing ratio
         !hcl-07/2015 comment out below
         !if (grid%hypsometric_opt == 1) then
         !   height = 0.5*(grid%phb(i,j,kts)+grid%ph_2(i,j,kts)+ &
         !                 grid%phb(i,j,kts+1)+grid%ph_2(i,j,kts+1))/gravity
         !   height = height - grid%ht(i,j)
         !elseif (grid%hypsometric_opt == 2) then
         ! ! Height is in proportion to log pressure: wee 11/22/2011
         !   zs1 = (grid%phb(i,j,kts)+grid%ph_2(i,j,kts))/gravity
         !   zs2 = (grid%phb(i,j,kts+1)+grid%ph_2(i,j,kts+1))/gravity
         !
         !   mu_full = grid%mub(i,j)+grid%mu_2(i,j)
         !   pfu = mu_full*grid%znw(kts+1) + grid%p_top
         !   pfd = mu_full*grid%znw(kts)   + grid%p_top
         !   phm = mu_full*grid%znu(kts)   + grid%p_top
         !   height = (zs2-zs1)*LOG(pfd/phm)/LOG(pfd/pfu)
         !endif
         !hcl-07/2015 to be consistent with the height calculation done in wrf
         height = 0.5*(grid%phb(i,j,kts)+grid%ph_2(i,j,kts)+ &
                       grid%phb(i,j,kts+1)+grid%ph_2(i,j,kts+1))/gravity
         height = height - grid%ht(i,j)
         if (height <= 0.0) then
            message(1) = "Negative height found"
            write (unit=message(2),FMT='(2I6,A,F10.2,A,F10.2)') &
               i,j,' ht = ',height ,' terr =  ',grid%ht(i,j)
            call da_error(__FILE__,__LINE__, message(1:2))
         end if
         if ( update_sfcdiags ) then
            if ( use_wrf_sfcinfo ) then
               call da_sfc_wtq(grid%psfc(i,j), grid%tsk(i,j),                 &
                  ps1, ts1, qv1, uu, vv,                                      &
                  height,  grid%xb%rough(i,j),grid%xb%xland(i,j), grid%xb%ds, &
                  grid%u10(i,j), grid%v10(i,j), grid%t2(i,j),                 &
                  grid%q2(i,j), grid%xb%regime(i,j),                          &
                  has_lsm_info, regime_wrf=grid%regime(i,j),                  &
                  qsfc_wrf=grid%qsfc(i,j), znt_wrf=grid%znt(i,j),             &
                  ust_wrf=grid%ust(i,j), mol_wrf=grid%mol(i,j),               &
                  hfx=grid%hfx(i,j), qfx=grid%qfx(i,j), pblh=grid%pblh(i,j) )
            else
               call da_sfc_wtq(grid%psfc(i,j), grid%tsk(i,j),                 &
                  ps1, ts1, qv1, uu, vv,                                      &
                  height,  grid%xb%rough(i,j),grid%xb%xland(i,j), grid%xb%ds, &
                  grid%u10(i,j), grid%v10(i,j), grid%t2(i,j),                 &
                  grid%q2(i,j), grid%xb%regime(i,j))
            end if

            ! 2-m pressure: Ground level and first half level are used
            !hcl p2m = grid%psfc(i,j)*EXP(-2.0/height*LOG(grid%psfc(i,j)/ps1))
            !hcl grid%th2(i,j) = grid%t2(i,j)*(base_pres/p2m)**kappa

            !hcl-07/2015 to be consistent with the th2 calculation done in wrf
            grid%th2(i,j) = grid%t2(i,j)*(base_pres/grid%psfc(i,j))**kappa
         end if ! if update_sfcdiags
      end do
   end do

   if (print_detail_xa) then
      write(unit=stdout, fmt=*) 'simple variables:'

      if (ite == ide) then
         write (unit=stdout,fmt=*)  ' '

         do k=kts+5,kte,10
            do j=jts,jte,10
               write(unit=stdout, fmt=*) &
                    '  grid%u_2(', ide+1, ',', j, ',', k, ')=', &
                       grid%u_2(ide+1,j,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

      if (jte == jde) then
         write(unit=stdout, fmt=*) ' '

         do k=kts+5,kte,10
            do i=its,ite,10
               write(unit=stdout, fmt=*) &
                    '  grid%v_2(', i, ',', jde+1, ',', k, ')=', &
                       grid%v_2(i, jde+1,k)
            end do
            write(unit=stdout, fmt=*) ' '
         end do
      end if

#ifdef A2C
      write(unit=stdout, fmt='(2(2x, a, e20.12))') &
         'maxval(mu_cgrid(its:ite,jts:jte))       =', &
         maxval(mu_cgrid(its:ite,jts:jte)), &
         'minval(mu_cgrid(its:ite,jts:jte))       =', &
         minval(mu_cgrid(its:ite,jts:jte)), &
         'maxval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(q_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(q_cgrid(its:ite,jts:jte,kts:kte))
#else
      write(unit=stdout, fmt='(2(2x, a, e20.12))') &
         'maxval(mu_cgrid(its:ite,jts:jte))       =', &
         maxval(mu_cgrid(its:ite,jts:jte)), &
         'minval(mu_cgrid(its:ite,jts:jte))       =', &
         minval(mu_cgrid(its:ite,jts:jte)), &
        'maxval(u_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(u_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(u_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(u_cgrid(its:ite,jts:jte,kts:kte)), &
         'maxval(v_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(v_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(v_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(v_cgrid(its:ite,jts:jte,kts:kte)), &
         'maxval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         maxval(q_cgrid(its:ite,jts:jte,kts:kte)), &
         'minval(q_cgrid(its:ite,jts:jte,kts:kte))  =', &
         minval(q_cgrid(its:ite,jts:jte,kts:kte))
#endif

      do k=kts,kte
         write(unit=stdout, fmt='(a, i3)') 'k=', k

#ifdef A2C
         write(unit=stdout, fmt='(2(2x, a, e20.12))') &
            'maxval(q_cgrid(its:ite,jts:jte,k))  =', maxval(q_cgrid(its:ite,jts:jte,k)), &
            'minval(q_cgrid(its:ite,jts:jte,k))  =', minval(q_cgrid(its:ite,jts:jte,k))
#else
         write(unit=stdout, fmt='(2(2x, a, e20.12))') &
            'maxval(u_cgrid(its:ite,jts:jte,k))  =', maxval(u_cgrid(its:ite,jts:jte,k)), &
            'minval(u_cgrid(its:ite,jts:jte,k))  =', minval(u_cgrid(its:ite,jts:jte,k)), &
            'maxval(v_cgrid(its:ite,jts:jte,k))  =', maxval(v_cgrid(its:ite,jts:jte,k)), &
            'minval(v_cgrid(its:ite,jts:jte,k))  =', minval(v_cgrid(its:ite,jts:jte,k)), &
            'maxval(q_cgrid(its:ite,jts:jte,k))  =', maxval(q_cgrid(its:ite,jts:jte,k)), &
            'minval(q_cgrid(its:ite,jts:jte,k))  =', minval(q_cgrid(its:ite,jts:jte,k))
#endif
      end do
   end if
#ifdef VAR4D
   IF ( var4d ) THEN

      ! We need to transfer LBC perturbation from model_grid to grid for output

      grid%u_bxs = grid%u_bxs + model_grid%g_u_bxs
      grid%u_bxe = grid%u_bxe + model_grid%g_u_bxe
      grid%u_bys = grid%u_bys + model_grid%g_u_bys
      grid%u_bye = grid%u_bye + model_grid%g_u_bye
      grid%v_bxs = grid%v_bxs + model_grid%g_v_bxs
      grid%v_bxe = grid%v_bxe + model_grid%g_v_bxe
      grid%v_bys = grid%v_bys + model_grid%g_v_bys
      grid%v_bye = grid%v_bye + model_grid%g_v_bye
!     grid%w_bxs = grid%w_bxs + model_grid%g_w_bxs
!     grid%w_bxe = grid%w_bxe + model_grid%g_w_bxe
!     grid%w_bys = grid%w_bys + model_grid%g_w_bys
!     grid%w_bye = grid%w_bye + model_grid%g_w_bye
      grid%t_bxs = grid%t_bxs + model_grid%g_t_bxs
      grid%t_bxe = grid%t_bxe + model_grid%g_t_bxe
      grid%t_bys = grid%t_bys + model_grid%g_t_bys
      grid%t_bye = grid%t_bye + model_grid%g_t_bye
      grid%mu_bxs = grid%mu_bxs + model_grid%g_mu_bxs
      grid%mu_bxe = grid%mu_bxe + model_grid%g_mu_bxe
      grid%mu_bys = grid%mu_bys + model_grid%g_mu_bys
      grid%mu_bye = grid%mu_bye + model_grid%g_mu_bye
      grid%ph_bxs = grid%ph_bxs + model_grid%g_ph_bxs
      grid%ph_bxe = grid%ph_bxe + model_grid%g_ph_bxe
      grid%ph_bys = grid%ph_bys + model_grid%g_ph_bys
      grid%ph_bye = grid%ph_bye + model_grid%g_ph_bye
      grid%moist_bxs = grid%moist_bxs + model_grid%g_moist_bxs
      grid%moist_bxe = grid%moist_bxe + model_grid%g_moist_bxe
      grid%moist_bys = grid%moist_bys + model_grid%g_moist_bys
      grid%moist_bye = grid%moist_bye + model_grid%g_moist_bye
   
      grid%u_btxs = grid%u_btxs + model_grid%g_u_btxs
      grid%u_btxe = grid%u_btxe + model_grid%g_u_btxe
      grid%u_btys = grid%u_btys + model_grid%g_u_btys
      grid%u_btye = grid%u_btye + model_grid%g_u_btye
      grid%v_btxs = grid%v_btxs + model_grid%g_v_btxs
      grid%v_btxe = grid%v_btxe + model_grid%g_v_btxe
      grid%v_btys = grid%v_btys + model_grid%g_v_btys
      grid%v_btye = grid%v_btye + model_grid%g_v_btye
!     grid%w_btxs = grid%w_btxs + model_grid%g_w_btxs
!     grid%w_btxe = grid%w_btxe + model_grid%g_w_btxe
!     grid%w_btys = grid%w_btys + model_grid%g_w_btys
!     grid%w_btye = grid%w_btye + model_grid%g_w_btye
      grid%t_btxs = grid%t_btxs + model_grid%g_t_btxs
      grid%t_btxe = grid%t_btxe + model_grid%g_t_btxe
      grid%t_btys = grid%t_btys + model_grid%g_t_btys
      grid%t_btye = grid%t_btye + model_grid%g_t_btye
      grid%mu_btxs = grid%mu_btxs + model_grid%g_mu_btxs
      grid%mu_btxe = grid%mu_btxe + model_grid%g_mu_btxe
      grid%mu_btys = grid%mu_btys + model_grid%g_mu_btys
      grid%mu_btye = grid%mu_btye + model_grid%g_mu_btye
      grid%ph_btxs = grid%ph_btxs + model_grid%g_ph_btxs
      grid%ph_btxe = grid%ph_btxe + model_grid%g_ph_btxe
      grid%ph_btys = grid%ph_btys + model_grid%g_ph_btys
      grid%ph_btye = grid%ph_btye + model_grid%g_ph_btye
      grid%moist_btxs = grid%moist_btxs + model_grid%g_moist_btxs
      grid%moist_btxe = grid%moist_btxe + model_grid%g_moist_btxe
      grid%moist_btys = grid%moist_btys + model_grid%g_moist_btys
      grid%moist_btye = grid%moist_btye + model_grid%g_moist_btye

   ENDIF
#endif
   if (trace_use) call da_trace_exit("da_transfer_xatowrf")

end subroutine da_transfer_xatowrf


