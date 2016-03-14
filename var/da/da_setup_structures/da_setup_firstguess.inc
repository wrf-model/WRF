subroutine da_setup_firstguess(xbx, grid, config_flags, ens )

   !---------------------------------------------------------------------------
   ! Purpose: Allocate and read in components of first guess state.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------------

   implicit none

   type (xbx_type),intent(out)  :: xbx   ! Header & non-gridded vars.

   type(domain),intent(inout)   :: grid
   type(grid_config_rec_type), intent(in) :: config_flags
   logical, intent(in) :: ens

   integer :: is, ie, js, je, ij, i, j
   real    :: ddx , ddy    

   if (trace_use) call da_trace_entry("da_setup_firstguess")

   is = grid%xp % its
   ie = grid%xp % ite
   js = grid%xp % jts
   je = grid%xp % jte

   ! Calculate sin and cosine values used in da_get_avpoles

   if (global) then
      if (grid%xp%jts == grid%xp%jds) then

         allocate(cos_xls(grid%xp%its:grid%xp%ite))
         allocate(sin_xls(grid%xp%its:grid%xp%ite))
         cos_xls(grid%xp%its:grid%xp%ite) = & 
            cos(deg_to_rad*grid%xlong(grid%xp%its:grid%xp%ite,grid%xp%jts))
         sin_xls(grid%xp%its:grid%xp%ite) = &
            sin(deg_to_rad*grid%xlong(grid%xp%its:grid%xp%ite,grid%xp%jts))
      end if

      if (grid%xp%jte == grid%xp%jde) then 
         allocate(cos_xle(grid%xp%its:grid%xp%ite))
         allocate(sin_xle(grid%xp%its:grid%xp%ite))
         cos_xle(grid%xp%its:grid%xp%ite) = &
            cos(deg_to_rad*grid%xlong(grid%xp%its:grid%xp%ite,grid%xp%jte))
         sin_xle(grid%xp%its:grid%xp%ite) = &
            sin(deg_to_rad*grid%xlong(grid%xp%its:grid%xp%ite,grid%xp%jte))
      end if
   end if

   !---------------------------------------------------------------------------      
   ! [1.0] Setup and read in fields from first guess:
   !---------------------------------------------------------------------------      

   if ((fg_format==fg_format_wrf_arw_regional) .or. &
      (fg_format==fg_format_wrf_arw_global  ) ) then
      call da_setup_firstguess_wrf(xbx, grid, config_flags,ens)
      ! when ens=.true., da_setup_firstguess(_wrf) is called solely for map_info,
      ! the rest of the code should be skipped
      if ( ens ) then
         if (trace_use) call da_trace_exit("da_setup_firstguess")
         return
      end if
   else if (fg_format == fg_format_wrf_nmm_regional ) then
      call da_setup_firstguess_wrf_nmm_regional(xbx, grid)
   else if (fg_format == fg_format_kma_global) then
      ! First guess is an KMA format file:
      call da_setup_firstguess_kma(xbx, grid)
   end if

   !---------------------------------------------------------------------------
   ! Exchange halo region for XB arrays.
   !---------------------------------------------------------------------------

   if ((fg_format==fg_format_wrf_arw_regional) .or. &
      (fg_format==fg_format_wrf_arw_global  ) ) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      !$OMP PARALLEL DO &
      !$OMP PRIVATE (ij, i, j)
      do ij = 1, grid%num_tiles
         do j = grid%j_start(ij), grid%j_end(ij)
            do i = is, ie
               grid%xb%coefx(i,j) = 0.5 * grid%xb%map_factor(i,j)/grid%xb%ds
               grid%xb%coefy(i,j) = grid%xb%coefx(i,j)
               grid%xb%coefz(i,j) = 0.5 / (grid%xb%map_factor(i,j)*grid%xb%ds)
            end do
         end do
      end do
      !$OMP END PARALLEL DO
   else if (fg_format == fg_format_wrf_nmm_regional) then
      grid%xb%coefx(is:ie,js:je) = 0.5/grid%mu0(is:ie,js:je)
      grid%xb%coefy(is:ie,js:je) = 0.5/grid%xb%ds         
   else if (fg_format == fg_format_kma_global) then
      ! Calculate multiplicative constants for PsiChi_TO_UV 
      ddx =  earth_radius*1000 * 2.0 * pi / (grid%xb%ide-grid%xb%ids+1)
      ddy =  earth_radius*1000       * pi / (grid%xb%jde-grid%xb%jds)
      grid%xb% coefx(is:ie,js:je) = 0.5 / (ddx * cos(grid%xlat(is:ie,js:je)*pi/180.))
      grid%xb% coefy(is:ie,js:je) = 0.5 /  ddy
   else
      write(unit=message(1),fmt='(A,I5)') &
         "Wrong choice for fg_format = ",fg_format
      call da_error(__FILE__,__LINE__,message(1:1))
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
#include "HALO_INIT.inc"
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

   periodic_x = grid%periodic_x

   if (global) then     
      ! Set East-West boundary for Xb-array 
      call da_set_boundary_xb(grid)
   end if

   !---------------------------------------------------------------------------      
   ! [2.0] Setup grid-dependent constants used:
   !---------------------------------------------------------------------------

   ! [2.1] Set up fast Fourier & Legendre transform constants:

   if (SIZE(xbx%fft_factors_x) /=num_fft_factors) &
      call da_setup_runconstants(grid, xbx)

   if (trace_use) call da_trace_exit("da_setup_firstguess")

end subroutine da_setup_firstguess


