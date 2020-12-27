subroutine da_check_cvtovv_adjoint(grid, cv_size, xbx, be, cv, vv)

   !---------------------------------------------------------------------------
   ! Purpose: Test vtovv routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < vv, vv > = < cv_adj, cv >.
   !---------------------------------------------------------------------------

   implicit none

   type(domain), intent(inout)               :: grid

   integer, intent(in)               :: cv_size ! Size of cv array.  
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be    ! background error structure.
   real, intent(in)                  :: cv(1:cv_size) ! control variable.
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).

   real                              :: adj_par_lhs ! < Vv, Vv >
   real                              :: adj_par_rhs ! < cv_adj, cv >
   real                              :: adj_sum_lhs ! < Vv, Vv >
   real                              :: adj_sum_rhs ! < cv_adj, cv >
   real                              :: cv2(1:cv_size)! control variable.
   integer                           :: cv_size_tmp

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   if (trace_use) call da_trace_entry("da_check_cvtovv_adjoint")

   if (cv_options == 3 ) then
      write(unit=stdout, fmt='(/a,i2,a/)') 'cv_options =',cv_options, &
                     '   no da_check_cvtovv_adjoint check...'
      goto 1234
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_cvtovv_adjoint: Test Results:'
      
   !-------------------------------------------------------------------------
   ! [2.0] Perform Vp = U_v Vv transform:
   !-------------------------------------------------------------------------

   if (global) then
      call da_transform_vtovv_global(cv_size, xbx, be, cv, vv)
   else
      call da_transform_vtovv(grid, cv_size, be, cv, vv)
   end if

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation:
   !----------------------------------------------------------------------
   adj_par_lhs = sum(vv % v1(its:ite,jts:jte,1:be%v1%mz)**2) &
               + sum(vv % v2(its:ite,jts:jte,1:be%v2%mz)**2) &
               + sum(vv % v3(its:ite,jts:jte,1:be%v3%mz)**2) &
               + sum(vv % v4(its:ite,jts:jte,1:be%v4%mz)**2) &
               + sum(vv % v5(its:ite,jts:jte,1:be%v5%mz)**2)

   if ( cloud_cv_options >= 2 ) then
      adj_par_lhs = adj_par_lhs                                   &
                  + sum(vv % v6(its:ite,jts:jte,1:be%v6%mz)**2)   &
                  + sum(vv % v7(its:ite,jts:jte,1:be%v7%mz)**2)   &
                  + sum(vv % v8(its:ite,jts:jte,1:be%v8%mz)**2)   &
                  + sum(vv % v9(its:ite,jts:jte,1:be%v9%mz)**2)   &
                  + sum(vv % v10(its:ite,jts:jte,1:be%v10%mz)**2)
   end if

   if ( use_cv_w ) then
      adj_par_lhs = adj_par_lhs                                   &
                  + sum(vv % v11(its:ite,jts:jte,1:be%v11%mz)**2)
   end if

   if (be % ne > 0) then
!     adj_par_lhs = adj_par_lhs + sum(vv % alpha(its:ite,jts:jte,1:be%alpha%mz,1:be%ne)**2)
      adj_par_lhs = adj_par_lhs + sum(vv % alpha(its_int:ite_int,jts_int:jte_int,1:be%alpha%mz,1:be%ne)**2)
   end if

   !----------------------------------------------------------------------
   ! [4.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------

   if (global) then
      call da_transform_vtovv_global_adj(cv_size, xbx, be, cv2, vv)
   else
      call da_transform_vtovv_adj(grid, cv_size, be, cv2, vv)
   end if

   cv_size_tmp = cv_size - be%cv%size_jp - be%cv%size_js - be%cv%size_jl
   adj_par_rhs = sum(cv(1:cv_size_tmp) * cv2(1:cv_size_tmp))

   !----------------------------------------------------------------------
   ! [5.0] Print output:
   !----------------------------------------------------------------------

   if (.not. global ) then
    if( num_procs == 1) then
      write(unit=stdout, fmt='(a,e22.14)') &
         'Single Domain: < Vv, Vv >     = ', adj_par_lhs, &
         'Single Domain: < cv_adj, cv > = ', adj_par_rhs
    else
      write(unit=stdout, fmt='(/a/,a/)')&
        'It is Multi Processor Run: ',&
        'For Single Domain: da_check_cvtovv_adjoint Test: Not Performed'
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
           'Whole  Domain: < Vv, Vv >     = ', adj_sum_lhs, &
           'Whole  Domain: < cv_adj, cv > = ', adj_sum_rhs
   end if
      
   write(unit=stdout, fmt='(/a/)') &
      'da_check_cvtovv_adjoint: Test Finished.'

1234 continue

   if (trace_use) call da_trace_exit("da_check_cvtovv_adjoint")

end subroutine da_check_cvtovv_adjoint


