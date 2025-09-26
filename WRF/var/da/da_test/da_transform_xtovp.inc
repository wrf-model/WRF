subroutine da_transform_xtovp(grid, xb, xbx, xa, vp, be)

   !---------------------------------------------------------------------------
   ! Purpose: Transforms analysis to control variables (Vp-type)
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !
   ! Updates:
   !
   !       Implementation of multi-variate BE
   !       Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 02/01/2010
   !---------------------------------------------------------------------------

   implicit none

   type(domain),            intent(inout) :: grid
   type(xb_type),           intent(in)    :: xb         ! First guess structure.
   type(xbx_type),          intent(in)    :: xbx        ! Header/non-gridded vars.
   type(x_type),            intent(inout) :: xa         ! Analysis increments.
   type(vp_type),           intent(out)   :: vp         ! CV on grid structure.
   type(be_type), optional, intent(in)    :: be         ! Background errors.

   real    :: vor(ims:ime,jms:jme,kms:kme) ! Vorticity.
   real    :: div(ims:ime,jms:jme,kms:kme) ! Divergence.

   real    :: one_over_m2(ims:ime,jms:jme) !   Multiplicative coeff.

   integer :: i, j, k , k1                 ! Loop counters.

   if (trace_use) call da_trace_entry("da_transform_xtovp")

   if ( (cv_options == 3) .or. (cv_options == 7) ) then
      write(unit=message(1),fmt='(A,I3)') 'Cannot perform transform_xtovp for cv_options == ',cv_options
      call da_error(__FILE__,__LINE__,message(1:1))
   endif

   !----------------------------------------------------------------
   ! [1.0] Perform transform v = U^{-1} x
   !----------------------------------------------------------------      

   call da_zero_vp_type (vp)

   ! [2.2] Transform u, v to streamfunction via vorticity:

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
#include "HALO_PSICHI_UV_ADJ.inc"
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
#endif

   call da_uv_to_vorticity(xb, xa % u, xa % v, vor)

   ! Convert vorticity to Del**2 psi:
   if (.not. global) then               
      if (fg_format == fg_format_wrf_arw_regional) then
         one_over_m2(its:ite,jts:jte) = 1.0 / (xb % map_factor(its:ite,jts:jte) * &
                                        xb % map_factor(its:ite,jts:jte))
         do k = kts, kte
            vor(its:ite,jts:jte,k) = &
              one_over_m2(its:ite,jts:jte)*vor(its:ite,jts:jte,k)
         end do
      else if (fg_format == fg_format_wrf_nmm_regional) then
         write(unit=message(1),fmt='(A,I5)') &
         "Needs to be developed for fg_format_nmm_regional = ",fg_format
         call da_error(__FILE__,__LINE__,message(1:1))
      else
         write(unit=message(1),fmt='(A,I5,A,L10)') &
            ' Wrong choice of fg_format= ',fg_format,' with global = ',global
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   ! Calculate psi:
   write (unit=stdout,fmt=*) ' calling Solve_PoissonEquation for Psi'
   call da_solve_poissoneqn_fct(grid,xbx, vor, vp%v1)

   ! [2.3] Transform u, v to velocity potential via divergence:

   call da_message((/'calling UV_To_Divergence'/))
   call da_uv_to_divergence(xb, xa % u, xa % v, div)

   ! Convert divergence to Del**2 chi:
   if (.not. global)  then              
      if (fg_format == fg_format_wrf_arw_regional) then
         do k = kts, kte
            div(its:ite,jts:jte,k) = &
               one_over_m2(its:ite,jts:jte) * div(its:ite,jts:jte,k)
         end do
      else if (fg_format == fg_format_wrf_nmm_regional) then
         write(unit=message(1),fmt='(A,I5)') &
         "Needs to be developed for fg_format_nmm_regional = ",fg_format
         call da_error(__FILE__,__LINE__,message(1:1))
      else
         write(unit=message(1),fmt='(A,I5,A,L10)') &
            ' Wrong choice of fg_format= ',fg_format,' with global = ',global
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   ! Calculate chi:

   call da_message((/' calling Solve_PoissonEquation for Chi'/))
   call da_solve_poissoneqn_fct(grid,xbx, div, vp%v2)

   ! [2.4] Transform chi to chi_u:
   call da_message((/' calculating chi_u'/))
   do k=kts,kte
      do j=jts,jte
         vp%v2(its:ite,j,k) = vp%v2(its:ite,j,k) - &
            be%reg_psi_chi(j,k)*vp%v1(its:ite,j,k)
      end do
   end do

   ! [2.5] Compute t_u:
   call da_message((/' calculating t_u'/))
   do k1=kts,kte
    do k=kts,kte
      do j=jts,jte
            vp%v3(its:ite,j,k) = vp%v3(its:ite,j,k) - be%reg_psi_t(j,k,k1)*vp%v1(its:ite,j,k1)
      end do
    end do
   end do
   if ( cv_options == 6 ) then
      do k1=kts,kte
         do k=kts,kte
            do j=jts,jte
               vp%v3(its:ite,j,k) = vp%v3(its:ite,j,k) + be%reg_chi_u_t(j,k,k1)*vp%v2(its:ite,j,k1)
            end do
         end do
      end do
   end if

   ! [2.6] Choice of moisture control variable:
   call da_message((/' calculating psudo rh'/))
   vp % v4(its:ite,jts:jte,kts:kte) = xa % q  (its:ite,jts:jte,kts:kte) /   &
      xb % qs (its:ite,jts:jte,kts:kte)

   if ( cv_options == 6 ) then
      do k1 = kts, kte
         do k = kts, kte
            do j = jts, jte
               vp%v4(its:ite,j,k) = vp%v4(its:ite,j,k)    - &
                                    be%reg_psi_rh(j,k,k1)*vp%v1(its:ite,j,k1)    + &
                                    be%reg_chi_u_rh(j,k,k1)*vp%v2(its:ite,j,k1)  + &
                                    be%reg_t_u_rh(j,k,k1)*vp%v3(its:ite,j,k1) + &
                                    be%reg_ps_u_rh(j,k1)*vp%v5(its:ite,j,1)
            end do
         end do
      end do
   end if

   ! [2.7] compute psfc_u:
   call da_message((/' calculating psfc_u '/))
   do j=jts,jte
      do i=its,ite
         vp % v5(i,j,1) = xa%psfc(i,j) - be%reg_psi_ps(j,k)*vp%v1(i,j,k)
      end do
   end do
   if ( cv_options == 6 ) then
      do j=jts,jte
         do i=its,ite
            vp % v5(i,j,1) = xa%psfc(i,j) + be%reg_chi_u_ps(j,k)*vp%v2(i,j,k)
         end do
      end do
   end if

   if (trace_use) call da_trace_exit("da_transform_xtovp")

end subroutine da_transform_xtovp


