subroutine da_check_vchemtox_adjoint(grid, vchem, be, vchem2)

   !---------------------------------------------------------------------------
   ! Purpose: Test V to X routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < x, x > = < v_adj, v >.
   !---------------------------------------------------------------------------

   use module_state_description, only : num_chem, PARAM_FIRST_SCALAR
   use da_define_structures, only : da_zero_xchem_type
   use module_domain, only : xchem_type

   implicit none

   type(domain),  intent(inout) :: grid
   type(xchem_type), intent(in) :: vchem   ! Grid point/EOF equivalent.
   type(be_type), intent(in)    :: be   ! Background error structure.

   real                   :: adj_par_lhs ! < x, x >
   real                   :: adj_par_rhs ! < x, x >
   real                   :: adj_sum_lhs ! < x, x >
   real                   :: adj_sum_rhs ! < v_adj, v >
   integer                :: ic
   type(xchem_type), intent(out) :: vchem2   ! Grid point/EOF equivalent.

   if (trace_use) call da_trace_entry("da_check_vchemtox_adjoint")

   write(unit=stdout, fmt='(/a/)') &
      'da_check_vchemtox_adjoint: Adjoint Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------
   vchem2%chem_ic(:,:,:,:) = 0.D0
   adj_par_rhs = 0.0
   adj_par_lhs = 0.0   
   !----------------------------------------------------------------------
   ! [2.0] Perform x = U v transform:
   !----------------------------------------------------------------------

   call da_zero_xchem_type (grid%xachem)
   call da_transform_vchemtox(grid, vchem, be)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation: 
   !----------------------------------------------------------------------

   do ic = PARAM_FIRST_SCALAR ,num_chem
     adj_par_lhs = adj_par_lhs + sum(grid%xachem%chem_ic(its:ite,jts:jte,kts:kte,ic)**2) / typical_t_rms**2
   end do

   !-------------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !-------------------------------------------------------------------------

    do ic = PARAM_FIRST_SCALAR ,num_chem
!      grid%xachem%chem_ic(:,:,:,ic) = grid%xachem%chem_ic(:,:,:,ic) / typical_t_rms**2
      grid%xachem%chem_ic(its:ite,jts:jte,kts:kte,ic) = grid%xachem%chem_ic(its:ite,jts:jte,kts:kte,ic) / typical_t_rms**2
    end do

   !-------------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !-------------------------------------------------------------------------

   call da_transform_vchemtox_adj(grid, vchem2, be)

   !-------------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !-------------------------------------------------------------------------

    do ic = PARAM_FIRST_SCALAR ,num_chem
      adj_par_rhs = adj_par_rhs+sum(vchem%chem_ic(its:ite,jts:jte,1:be%v12(ic-1)%mz,ic) * vchem2%chem_ic(its:ite,jts:jte,1:be%v12(ic-1)%mz,ic))
    end do
   !-------------------------------------------------------------------------
   ! [7.0] Print output:
   !-------------------------------------------------------------------------

   if (.not. global ) then
    if( num_procs == 1) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Single Domain: < x, x >     = ', adj_par_lhs, &
         'Single Domain: < v_adj, v > = ', adj_par_rhs
    else
      write(unit=stdout, fmt='(/a/,a/)')&
        'It is Multi Processor Run: ',&
        'For Single Domain: da_check_vchemtox_adjoint Test: Not Performed'
    endif
   end if

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)

   if (global) then
      adj_sum_rhs = adj_par_rhs
   else
      adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)
   end if

   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < x, x >     = ', adj_sum_lhs, &
         'Whole  Domain: < vchem_adj, vchem > = ', adj_sum_rhs
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vchemtox_adjoint: Finished'

   if (trace_use) call da_trace_exit("da_check_vchemtox_adjoint")

end subroutine da_check_vchemtox_adjoint


