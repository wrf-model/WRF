subroutine da_setup_testfield(grid)

   !----------------------------------------------------------------------------
   ! Purpose: produce test increment field based on grid%xb field.
   !
   ! Method:  pass through x=uv transfom to ensure satisfies boundary conditions
   !----------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)   :: grid

   integer :: i, j

   if (trace_use) call da_trace_entry("da_setup_testfield")

   !-------------------------------------------------------------------------
   ! [1.0]: initialise:
   !-------------------------------------------------------------------------

   write(unit=stdout, fmt='(/a/)') &
      'Starting da_setup_testfield ...'

   !-------------------------------------------------------------------------
   ! [2.0]: set up test increment field structure:
   !-------------------------------------------------------------------------

   ! [2.1] test wind, temperature, pressure, humidity and cloud parameters

   call da_set_tst_trnsf_fld(grid, grid%xa%u, grid%xb%u, typical_u_rms)
   call da_set_tst_trnsf_fld(grid, grid%xa%v, grid%xb%v, typical_v_rms)
   call da_set_tst_trnsf_fld(grid, grid%xa%w, grid%xb%w, typical_w_rms)
   call da_set_tst_trnsf_fld(grid, grid%xa%t, grid%xb%t, typical_t_rms)
   call da_set_tst_trnsf_fld(grid, grid%xa%p, grid%xb%p, typical_p_rms)
   call da_set_tst_trnsf_fld(grid, grid%xa%q, grid%xb%q, typical_q_rms)
   if ( ( use_rad .and. crtm_cloud ) .or. use_radar_rf .or. use_radar_rhv ) then
      call da_set_tst_trnsf_fld(grid, grid%xa%qcw, grid%xb%qcw, typical_qcw_rms)
      call da_set_tst_trnsf_fld(grid, grid%xa%qrn, grid%xb%qrn, typical_qrn_rms)
      call da_set_tst_trnsf_fld(grid, grid%xa%qci, grid%xb%qci, typical_qci_rms)
      call da_set_tst_trnsf_fld(grid, grid%xa%qsn, grid%xb%qsn, typical_qsn_rms)
      call da_set_tst_trnsf_fld(grid, grid%xa%qgr, grid%xb%qgr, typical_qgr_rms)
   end if

   ! [2.5] get test density increment from linearised ideal gas law:

   call da_pt_to_rho_lin(grid)

   grid%xa%psfc(grid%xp%its:grid%xp%ite, grid%xp%jts:grid%xp%jte) = &
   grid%xa%p   (grid%xp%its:grid%xp%ite, grid%xp%jts:grid%xp%jte, grid%xp%kts)

   if (print_detail_testing) then
      write(unit=stdout, fmt='(2a,4x,a,i8)') &
         'file:', __FILE__, 'line:', __LINE__

      write(unit=stdout, fmt=*) 'grid%xp%its, grid%xp%ite, grid%xp%jts, grid%xp%jte) =', &
         grid%xp%its, grid%xp%ite, grid%xp%jts, grid%xp%jte
   
      do j=grid%xp%jts, grid%xp%jte
         do i=grid%xp%its, grid%xp%ite
            if (i == j) then
               write(unit=stdout, fmt='(2(a,i4),a,f14.6)') &
                  'grid%xa%psfc(', i, ',', j, ') =', grid%xa%psfc(i, j)
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

   if (sfc_assi_options == 2) then
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
   end if


   if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
        use_ssmitbobs .or. use_ssmiretrievalobs) then

      ! Now do something for PW
      call da_transform_xtotpw(grid)

      ! GPS Refractivity:
      if (use_gpsrefobs) &
         call da_transform_xtogpsref_lin(grid)

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_ssmitbobs .or. use_ssmiretrievalobs) then
         call da_transform_xtoseasfcwind_lin(grid)
      end if

#ifdef DM_PARALLEL
#include "HALO_SSMI_XA.inc"
#endif
   end if

   write(unit=stdout, fmt='(/a/)') 'End of da_setup_testfield.'

   if (trace_use) call da_trace_exit("da_setup_testfield")

end subroutine da_setup_testfield


