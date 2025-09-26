subroutine da_check_vtoy_adjoint(cv_size,grid, config_flags, vp, vv, xbx, be, ep, iv, y)

   !---------------------------------------------------------------------------
   !  Purpose: Perform V To Y Adjoint transform test                             
   !
   !  Method:  Perform adjoint test on complete transform: <y,y> = <v_adj,v>.!
   !---------------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: cv_size
   type(grid_config_rec_type), intent(inout) :: config_flags
   type(domain),               intent(inout) :: grid 
   type(vp_type),              intent(inout) :: vv    ! Grdipt/EOF CV.
   type(vp_type),              intent(inout) :: vp    ! Grdipt/level CV.
   type(xbx_type),             intent(inout) :: xbx   ! Header & non-gridded vars.
   type(be_type),              intent(in)    :: be    ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! ensemble perturbation structure.
   type(iv_type),              intent(inout) :: iv    ! ob. increment vector.
   type(y_type),               intent(inout) :: y     ! y = h (xa)

   real    :: cv(1:cv_size)          ! Test control variable.
   real    :: cv_2(1:cv_size)
   real    :: adj_sum_lhs               ! < y, y >
   real    :: adj_rhs,adj_sum_rhs       ! < v, v_adj >
   real    :: partial_lhs   ! < y, y >
   real    :: pertile_lhs   ! < y, y >  

   if (trace_use_dull) call da_trace_entry("da_check_vtoy_adjoint")

   write(unit=stdout, fmt='(/a/a)') &
        '       da_check_vtoy_adjoint:',&
        '---------------------------------------'

   call random_number(cv(:))
   cv(:) = cv(:) - 0.5

   cv_2(1:cv_size) = cv(1:cv_size)

   call da_zero_x(grid%xa)
   call da_zero_vp_type(vp)
   call da_zero_vp_type(vv)

   call da_transform_vtoy(cv_size, be, ep, cv, iv, vp, vv, xbx, y, grid, config_flags, vp, vv)

   !-------------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation and
   !        Rescale input to adjoint routine :
   !-------------------------------------------------------------------------

   call da_get_y_lhs_value(iv, y, partial_lhs, pertile_lhs, adj_sum_lhs)

   cv = 0.0

   ! WHY?
   ! call da_zero_vp_type(vp)
   ! call da_zero_vp_type(vv)
   ! call da_zero_x(grid%xa)      

   call da_transform_vtoy_adj(cv_size, be, ep, cv, iv, vp, vv, xbx, y, grid, &
      config_flags, .true., vp, vv)

   adj_rhs = sum(cv(1:cv_size) * cv_2(1:cv_size))

   !-------------------------------------------------------------------------
   !      Print output:
   !-------------------------------------------------------------------------

#ifdef DM_PARALLEL
   if (global) then
      adj_sum_rhs = adj_rhs
   else
      call mpi_allreduce(adj_rhs, adj_sum_rhs, 1, true_mpi_real, mpi_sum, &
                       comm, ierr)
   end if
#else
   adj_sum_rhs = adj_rhs
   adj_sum_lhs = partial_lhs
#endif

#ifdef DM_PARALLEL
   if (rootproc) then
      write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < y, y     > = ', adj_sum_lhs
      write(unit=stdout, fmt='(A,1pe22.14)') &
         'Whole Domain  < v, v_adj > = ', adj_sum_rhs
   end if
#else
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < y, y     > = ', adj_sum_lhs
   write(unit=stdout, fmt='(A,1pe22.14)') &
      'Whole Domain  < v, v_adj > = ', adj_sum_rhs
#endif

   if (trace_use_dull) call da_trace_exit("da_check_vtoy_adjoint")

end subroutine da_check_vtoy_adjoint


