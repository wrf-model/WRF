subroutine da_check_dynamics_adjoint(cv_size, cv, xbx, be, grid, config_flags)

   !--------------------------------------------------------------------------
   ! Purpose: Test observation operator transform and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < y, y > = < x, x_adj >.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Xiaowen Tang,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                       :: cv_size ! Size of cv array.
   type (be_type),             intent(in)    :: be    ! background error structure.
   real, intent(inout)                       :: cv(1:cv_size)   ! control variables.
   type (xbx_type),            intent(inout) :: xbx   ! Header & non-gridded vars.
   type (domain),              intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >

   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w
   real, dimension(ims:ime, jms:jme)          :: xa2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_qcw, xa2_qci, xa2_qrn, xa2_qsn, xa2_qgr
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_u, x6a2_v, x6a2_t, &
                                                 x6a2_p, x6a2_q, x6a2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_w
   real, dimension(ims:ime, jms:jme)          :: x6a2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: x6a2_qcw, x6a2_qci, x6a2_qrn, x6a2_qsn, x6a2_qgr
   real, dimension(ims:ime, jms:jme, kms:kme) :: tempv

   integer :: nobwin, i, j, k, fgat_rain
   character(len=4) :: filnam
   character(len=256) :: timestr
   integer :: time_step_seconds
   type(x_type) :: shuffle
   real             :: subarea, whole_area

   if (trace_use) call da_trace_entry("da_check_dynamics_adjoint")

   write (unit=stdout, fmt='(/a/)') 'da_check_dynamics_adjoint: Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

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

   call da_transform_vtox(grid, cv_size, xbx, be, grid%ep, cv, grid%vv, grid%vp)
   call da_transform_xtoxa(grid)

   xa2_u(ims:ime, jms:jme, kms:kme) = grid%xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = grid%xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = grid%xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = grid%xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = grid%xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = grid%xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= grid%xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = grid%xa%psfc(ims:ime, jms:jme)

   xa2_qcw = 0.0
   xa2_qrn = 0.0
   xa2_qci = 0.0
   xa2_qsn = 0.0
   xa2_qgr = 0.0
   if ( cloud_cv_options >= 1 ) then
      xa2_qcw(ims:ime, jms:jme, kms:kme) = grid%xa%qcw(ims:ime, jms:jme, kms:kme)
      xa2_qrn(ims:ime, jms:jme, kms:kme) = grid%xa%qrn(ims:ime, jms:jme, kms:kme)
      if ( cloud_cv_options >= 2 ) then
         xa2_qci(ims:ime, jms:jme, kms:kme) = grid%xa%qci(ims:ime, jms:jme, kms:kme)
         xa2_qsn(ims:ime, jms:jme, kms:kme) = grid%xa%qsn(ims:ime, jms:jme, kms:kme)
         xa2_qgr(ims:ime, jms:jme, kms:kme) = grid%xa%qgr(ims:ime, jms:jme, kms:kme)
      end if
   end if

   x6a2_u = 0.0
   x6a2_v = 0.0
   x6a2_t = 0.0
   x6a2_p = 0.0
   x6a2_q = 0.0
   x6a2_w = 0.0
   x6a2_rh = 0.0
   x6a2_psfc = 0.0

   x6a2_qcw = 0.0
   x6a2_qci = 0.0
   x6a2_qrn = 0.0
   x6a2_qsn = 0.0
   x6a2_qgr = 0.0

#ifdef A2C
    if( ite == ide ) &
print*,__FILE__,jte,' xa2_u.xa2_u for col= ',ite+1,sum(xa2_u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
    if( jte == jde ) &
print*,__FILE__,jte,' xa2_v.xa2_v for row= ',jte+1,sum(xa2_v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
#endif

   if ( num_fgat_time > 1 ) then
      call domain_clock_get (grid, stop_timestr=timestr)
      call domain_clock_set( grid, current_timestr=timestr )
      call domain_clock_set (grid, time_step_seconds=-1*var4d_bin)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   endif

   fgat_rain = num_fgat_time
   do nobwin= num_fgat_time, 1, -1
      !----------------------------------------------------------------------
      ! [1.0] Perform y = Hx transform:
      !----------------------------------------------------------------------
      ! call da_uv_to_divergence(grid%xb, grid%xa%u, grid%xa%v, tempv)
      call da_divergence_constraint(grid%xb, grid%xa%u, grid%xa%v, tempv)
      partial_lhs = partial_lhs + SUM( tempv(ims:ime, jms:jme, kms:kme) * tempv(ims:ime, jms:jme, kms:kme) )
      pertile_lhs = pertile_lhs + SUM( tempv(its:ite, jts:jte, kts:kte) * tempv(its:ite, jts:jte, kts:kte) )

      !----------------------------------------------------------------------
      ! [5.0] Perform adjoint operation:
      !----------------------------------------------------------------------
      call da_zero_x (grid%xa)
      ! call da_uv_to_divergence_adj(grid, grid%xa%u, grid%xa%v, tempv)
      call da_divergence_constraint_adj(grid, grid%xa%u, grid%xa%v, tempv)

      if ( nobwin > 1 ) call domain_clockadvance (grid)
      call domain_clockprint(150, grid, 'DEBUG Adjoint Check:  get CurrTime from clock,')

   end do

   if ( num_fgat_time > 1 ) then
      call nl_get_time_step ( grid%id, time_step_seconds)
      call domain_clock_set (grid, time_step_seconds=time_step_seconds)
      call domain_clockprint(150, grid, 'get CurrTime from clock,')
   endif


   pertile_rhs = sum (grid%xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme))          &
      + sum (grid%xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme))         &
      + sum (grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))
#ifdef VAR4D
   pertile_rhs = pertile_rhs &
      + sum (grid%x6a%u(ims:ime, jms:jme, kms:kme) * x6a2_u(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%v(ims:ime, jms:jme, kms:kme) * x6a2_v(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%w(ims:ime, jms:jme, kms:kme) * x6a2_w(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%t(ims:ime, jms:jme, kms:kme) * x6a2_t(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%p(ims:ime, jms:jme, kms:kme) * x6a2_p(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%q(ims:ime, jms:jme, kms:kme) * x6a2_q(ims:ime, jms:jme, kms:kme))        &
      + sum (grid%x6a%rh(ims:ime, jms:jme, kms:kme)* x6a2_rh(ims:ime, jms:jme, kms:kme))       &
      + sum (grid%x6a%psfc(ims:ime, jms:jme) * x6a2_psfc(ims:ime, jms:jme))
#endif
   if ( cloud_cv_options >= 1 ) then
      pertile_rhs = pertile_rhs &
         + sum (grid%xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme))   &
         + sum (grid%xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))
      if ( cloud_cv_options >= 2 ) then
         pertile_rhs = pertile_rhs &
            + sum (grid%xa%qci(ims:ime, jms:jme, kms:kme) * xa2_qci(ims:ime, jms:jme, kms:kme))&
            + sum (grid%xa%qsn(ims:ime, jms:jme, kms:kme) * xa2_qsn(ims:ime, jms:jme, kms:kme))&
            + sum (grid%xa%qgr(ims:ime, jms:jme, kms:kme) * xa2_qgr(ims:ime, jms:jme, kms:kme))
      end if
   end if
#ifdef VAR4D
   pertile_rhs = pertile_rhs &
      + sum (grid%x6a%qcw(ims:ime, jms:jme, kms:kme) * x6a2_qcw(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qci(ims:ime, jms:jme, kms:kme) * x6a2_qci(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qrn(ims:ime, jms:jme, kms:kme) * x6a2_qrn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qsn(ims:ime, jms:jme, kms:kme) * x6a2_qsn(ims:ime, jms:jme, kms:kme))    &
      + sum (grid%x6a%qgr(ims:ime, jms:jme, kms:kme) * x6a2_qgr(ims:ime, jms:jme, kms:kme))
#endif


   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------

   partial_rhs = sum (grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1))      &
      + sum (grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte))          &
      + sum (grid%xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte))         &
      + sum (grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte))
#ifdef VAR4D
   partial_rhs = partial_rhs &
      + sum (grid%x6a%u(its:ite, jts:jte, kts:kte) * x6a2_u(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%v(its:ite, jts:jte, kts:kte) * x6a2_v(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%w(its:ite, jts:jte, kts:kte+1) * x6a2_w(its:ite, jts:jte, kts:kte+1))    &
      + sum (grid%x6a%t(its:ite, jts:jte, kts:kte) * x6a2_t(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%p(its:ite, jts:jte, kts:kte) * x6a2_p(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%q(its:ite, jts:jte, kts:kte) * x6a2_q(its:ite, jts:jte, kts:kte))        &
      + sum (grid%x6a%rh(its:ite, jts:jte, kts:kte)* x6a2_rh(its:ite, jts:jte, kts:kte))       &
      + sum (grid%x6a%psfc(its:ite, jts:jte) * x6a2_psfc(its:ite, jts:jte))
#endif
   if ( cloud_cv_options >= 1 ) then
      partial_rhs = partial_rhs &
         + sum (grid%xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte))   &
         + sum (grid%xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))
      if ( cloud_cv_options >= 2 ) then
         partial_rhs = partial_rhs &
            + sum (grid%xa%qci(its:ite, jts:jte, kts:kte) * xa2_qci(its:ite, jts:jte, kts:kte)) &
            + sum (grid%xa%qsn(its:ite, jts:jte, kts:kte) * xa2_qsn(its:ite, jts:jte, kts:kte)) &
            + sum (grid%xa%qgr(its:ite, jts:jte, kts:kte) * xa2_qgr(its:ite, jts:jte, kts:kte))
      end if
   end if
#ifdef VAR4D
   partial_rhs = partial_rhs &
      + sum (grid%x6a%qcw(its:ite, jts:jte, kts:kte) * x6a2_qcw(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qci(its:ite, jts:jte, kts:kte) * x6a2_qci(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qrn(its:ite, jts:jte, kts:kte) * x6a2_qrn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qsn(its:ite, jts:jte, kts:kte) * x6a2_qsn(its:ite, jts:jte, kts:kte)) &
      + sum (grid%x6a%qgr(its:ite, jts:jte, kts:kte) * x6a2_qgr(its:ite, jts:jte, kts:kte))
#endif

#ifdef A2C
    if( ite == ide ) then
print*,__FILE__,' contribution from ',ite+1,' col for U : ',sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   end if
    if( jte == jde ) then
print*,__FILE__,' contribution from ',jte+1,' row for V : ',sum(grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   end if
#endif

   !----------------------------------------------------------------------
   !  [7.0] Print output:
   !----------------------------------------------------------------------
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < y, y     > = ', pertile_lhs
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real (partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real (partial_rhs)

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if

   write (unit=stdout, fmt='(/a/)') 'da_check_dynamics_adjoint: Test Finished:'
   if (trace_use) call da_trace_exit("da_check_dynamics_adjoint")

end subroutine da_check_dynamics_adjoint


