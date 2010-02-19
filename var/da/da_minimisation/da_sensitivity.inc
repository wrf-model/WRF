subroutine da_sensitivity(iter, grid, config_flags, cv_size, xbx, be, iv, xhat, qhat, cv, y, &
                          eignvec, eignval, shat)

   !-------------------------------------------------------------------------
   ! Purpose:        Compute observation sensitivity and impact
   !
   ! Called either from da_minimise_lz or da_solve
   !
   ! History: 03/05/2009  Creation (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none

   integer,        intent(in)        :: iter                   ! Inner-loop iteration
   type(domain),   intent(inout)     :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   integer,        intent(in)        :: cv_size                ! Total cv size
   type (xbx_type),intent(in)        :: xbx                    ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                     ! background error structure.
   type (iv_type), intent(inout)     :: iv                     ! ob. increment vector.
   real,           intent(in)        :: xhat(1:cv_size)        ! control variable (local).
   real,           intent(in)        :: qhat(1:cv_size, 0:ntmax)  
   real,           intent(in)        :: cv(1:cv_size)          ! control variable (local).
   type (y_type),  intent(inout)     :: y                      ! y = H(x_inc) structure.
   real*8,         intent(in)        :: eignvec(ntmax, ntmax)
   real*8,         intent(in)        :: eignval(ntmax)
   real,           intent(out)       :: shat(1:cv_size)        ! control variable (local).
   
   type (y_type)                     :: ktr         
   integer                           :: i, j, mz(7)
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   real                              :: amat(1:cv_size)           ! cv copy.
   real                              :: ritz(ntmax, ntmax)

    mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be % ne /)
    jp_start = be % cv % size_jb + be % cv % size_je + 1
    jp_end   = be % cv % size_jb + be % cv % size_je + be % cv % size_jp

    ! Define Shat = dF/dx (e.g. F=1/2<x',B-1.x'> --> dF/dx=xhat)
    !-----------------------------------------------------------
    call da_adjoint_sensitivity(grid, config_flags, cv_size, xbx, be, iv, xhat, cv, y, shat)

    ! Apply Analysis Error Covariance Matrix estimation (A) to dF/dx 
    !---------------------------------------------------------------
      amat = 0.0
      do i = 1, iter
         do j = 1, iter
            ritz(i,j) = SUM( eignvec(i,1:iter) * (1.0/eignval(1:iter)) * eignvec(j,1:iter) )
	    amat      = amat + qhat(:,i) * ritz(i,j) * &
	                       da_dot_cv(cv_size, qhat(:,j), shat, grid, mz, jp_start, jp_end)
	 end do
      end do

    ! Calculate observation sensitivity: Kt = R-1.H.A
    !------------------------------------------------
      call da_allocate_y  (iv, ktr)

    ! Apply observation operator H 
      call da_transform_vtoy(cv_size, be, grid%ep, amat, iv, grid%vp, grid%vv, xbx, ktr, grid, config_flags)
				
    ! Apply R-1 (for Observation Sensitivity) and then Dot Product with initial innovations (for Observation Impact)
      call da_obs_sensitivity(ktr, iv)
             
      call da_deallocate_y(ktr)

    ! Adjoint test    
      write(stdout,*) 'ADJOINT_TEST2:', da_dot_cv(cv_size, xhat, xhat, grid, mz, jp_start, jp_end)

end subroutine da_sensitivity
