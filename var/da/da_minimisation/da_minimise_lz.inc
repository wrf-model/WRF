subroutine da_minimise_lz(grid, config_flags,            &
                           it, cv_size, xbx, be, iv,     &
                           j_grad_norm_target, xhat, cv, &
                           re, y, j_cost)

   !-------------------------------------------------------------------------
   ! Purpose:         Main Lanczos minimisation routine 
   !
   ! Here 
   !    cv   is updated in outer-loop.
   !    xhat is the control variable in inner-loop.
   !
   ! Called from da_solve
   !
   ! History: 07/30/2008  Creation (Tom Auligne)
   !
   ! Reference: Golub and Van Loan 1996 (p493)
   !
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)               :: it                     ! external iteration.
   integer, intent(in)               :: cv_size                ! Total cv size
   type (xbx_type),intent(in)        :: xbx                    ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                     ! background error structure.
   type (iv_type), intent(inout)     :: iv                     ! ob. increment vector.
   real, intent(inout)               :: j_grad_norm_target     ! Target norm.
   real, intent(out)                 :: xhat(1:cv_size)        ! control variable (local).
   real, intent(inout)               :: cv(1:cv_size)          ! control variable (local).
   type (y_type), intent(inout)      :: re                     ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y                      ! y = H(x_inc) structure.

   type (j_type), intent(out)        :: j_cost                 ! cost function

   type(domain), intent(inout)       :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   
   integer, parameter                :: nbvar = 6
   integer, parameter                :: npredmax = 8

   type (y_type)                     :: re0, ktr         
   integer                           :: iter, i, j, k, n, inst
   integer                           :: mz(6), info, nsstwrk
   integer                           :: npred, ipred
   real                              :: fhat(1:cv_size)           ! cv copy.
   real                              :: ghat(1:cv_size)           ! cv copy.
   real                              :: qhat(1:cv_size, 0:ntmax)  ! cv copy.
   real                              :: hessian(1:cv_size)        ! cv copy.
   real                              :: shat(1:cv_size)           ! cv copy.
   real                              :: ob_grad, gdot, rho, mu, bndlm
   real                              :: alpha(ntmax), beta(0:ntmax), subdiag(ntmax)
   real                              :: c(cv_size), d(ntmax), sstwrk(2*ntmax-2)
   real                              :: eignval(ntmax), eignvec(ntmax, ntmax)
   real                              :: ritz(ntmax, ntmax), q_tmp(ntmax), bnds(ntmax)
   real                              :: ktd_global(25,6), ktdrad_global(iv%num_inst) ! < Sensitivity, d >
   real                              :: ktbrad_global(iv%num_inst,npredmax) ! < Sensitivity, d >
   real                              :: adj_rhs, adj_sum_rhs ! < cv, cv >
   real                              :: j_init, j_estim
 
   if (trace_use) call da_trace_entry("da_minimise_lz")

   write(unit=stdout,fmt='(A)') 'Minimize cost function using Lanczos method'
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [1.0] Initialization:
   !-------------------------------------------------------------------------
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /)
   call da_calculate_j(it, 0, cv_size, be % cv % size_jb, be % cv % size_je, &
                        be % cv % size_jp, xbx, be, iv, xhat, cv, &
                        re, y, j_cost, fhat, grid, config_flags)
 
   re0       = re
   ghat      = - fhat
   beta(0)   = SQRT(da_dot_cv(cv_size, cv_size_domain, ghat, ghat, grid, mz, use_varbc))
   qhat(:,0) = 0.0
   j_init    = j_cost%total

   if (it == 1) j_grad_norm_target = beta(0)

   write(unit=stdout,fmt='("Starting outer iteration : ",i3)') it
   write(unit=stdout,fmt=11) sqrt(sum(ghat*ghat)),eps(it)*beta(0), &
              j_cost%total, beta(0)
11 format('Original gradient is ',1PD15.8,/,&
          'For this outer iteration gradient target is ',1PD15.8,/,&
          'Starting cost function: ' ,1PD15.8,' gradient= ',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)')'Iter      Beta             Alpha'

   !-------------------------------------------------------------------------
   ! [2.0] Iteratively solve for minimum of cost function:
   !-------------------------------------------------------------------------
   do iter=1, ntmax      
      qhat(:,iter) = ghat / beta(iter-1)
      
      call da_calculate_gradj(it,iter,cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                              xbx,be,iv,y,qhat(:,iter),cv,fhat,grid,config_flags)
       
    ! Apply Lanczos recurrence and orthonormalize new gradient (using modified Gramm-Schmidt)
    !----------------------------------------------------------------------------------------
      alpha(iter) = da_dot_cv(cv_size, cv_size_domain, qhat(:,iter), fhat, grid, mz, use_varbc)

      ghat        = fhat - alpha(iter)*qhat(:,iter) - beta(iter-1)*qhat(:,iter-1)
      do i = iter, 1, -1
         gdot = da_dot_cv(cv_size, cv_size_domain, ghat, qhat(:,i), grid, mz, use_varbc)
         ghat = ghat - gdot * qhat(:,i)
      end do

      beta(iter)  = SQRT(da_dot_cv (cv_size, cv_size_domain, ghat, ghat, grid, mz, use_varbc))
      
    ! Lanczos iteration  
    !------------------
      if (iter == 1) then
         d(1) = alpha(1)
	 c    = qhat(:,1)
	 rho  = beta(0) / alpha(1)
	 xhat = rho * qhat(:,1)
      else
         mu      = beta(iter-1) / d(iter-1)
	 d(iter) = alpha(iter) - beta(iter-1)*mu
	 c       = qhat(:,iter) - mu*c
	 rho     = - mu*d(iter-1)*rho / d(iter)
	 xhat    = xhat + rho*c
      end if
      
    ! Determine eigenvalues and eigenvectors of the Lanczos tri-diagonal matrix
    !--------------------------------------------------------------------------
      eignval(1:iter)   = alpha(1:iter)
      subdiag(1:iter-1) = beta(1:iter-1)
      nsstwrk           = MAX(2*iter-2,1)
      info              = 0
      call DSTEQR('I',iter,eignval(1:iter),subdiag(1:iter-1),eignvec(:,1:iter),ntmax,&
	           sstwrk(1:nsstwrk),info)
      if (info /=0) write(stdout,*) 'Error in Lanczos minimization: SSTEQR returned ',info 
      
      write(stdout,*) 'Ritz eigenvalues: ',eignval(iter:1:-1)     
      
    ! Count converged eigenpairs (using Arnoldi relation)
    !----------------------------------------------------
    bndlm        = eps(it) * eignval(iter)  
    bnds(1:iter) = abs(beta(iter) * eignvec(iter,1:iter))
    write(stdout,*) 'Number of converged eigenpairs: ', COUNT(bnds(1:iter) <= bndlm)
      
    ! Define Shat = dF/dx (e.g. F=1/2<x',B-1.x'> --> dF/dx=xhat)
    !-----------------------------------------------------------
    call da_adjoint_sensitivity(grid, config_flags, cv_size, xbx, be, iv, xhat, cv, y, shat)
    
    ! Apply Hessian estimation (A-1) to dF/dx 
    !----------------------------------------
      hessian = 0.0
      do i = 1, iter
         do j = 1, iter
            ritz(i,j) = SUM( eignvec(i,1:iter) * (1.0/eignval(1:iter)) * eignvec(j,1:iter) )
	    hessian = hessian +  qhat(:,i) * ritz(i,j) * &
	              da_dot_cv(cv_size, cv_size_domain, qhat(:,j), shat, grid, mz, use_varbc)
	 end do
      end do

    ! Calculate observation sensitivity: Kt = R-1.H.A-1
    !--------------------------------------------------
      call da_allocate_y  (iv, ktr)
      call da_zero_x      (grid%xa)
      call da_zero_vp_type(grid%vp)
      call da_zero_vp_type(grid%vv)

    ! Apply observation operator H 
      call da_transform_vtoy(cv_size, be, grid%ep, hessian, iv, grid%vp, grid%vv, xbx, &
	                     ktr, grid, config_flags)
				
    ! Apply R-1 (for Observation Sensitivity) and then Dot Product with initial innovations (for Observation Impact)
      call da_obs_sensitivity(nbvar, npredmax, ktr, iv, ktd_global, ktdrad_global, ktbrad_global)
             
      call da_deallocate_y(ktr)

	! Adjoint test    
!         adj_sum_rhs = da_dot_cv(cv_size, cv_size_domain, xhat, xhat, grid, mz, use_varbc)        
!	 print*, 'ADJOINT_TEST:', SUM(ktd_global)+SUM(ktdrad_global), adj_sum_rhs
!        print*, 'Conventional Observation Impact:',SUM(ktd_global,dim=1), '--->',SUM(ktd_global,dim=2)
!	 print*, 'Satellite Observation Impact:', ktdrad_global
!	 print*, 'Bias Correction Impact:', SUM(ktbrad_global,dim=1), '--->',SUM(ktbrad_global,dim=2)
      
      call da_calculate_gradj(it,iter,cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                              xbx,be,iv,y,xhat,cv,fhat,grid,config_flags)
      ob_grad = SQRT(da_dot_cv(cv_size, cv_size_domain, fhat, fhat, grid, mz, use_varbc))
			      
      if (ob_grad  < eps(it) * beta(0)) exit
      
      if (calculate_cg_cost_fn) then
         j_estim = j_init - 0.5 * da_dot_cv(cv_size, cv_size_domain, qhat(:,1)*beta(0), xhat, grid, mz, use_varbc)         
         write(unit=stdout,fmt=14)iter, j_estim, ob_grad	 
      else
         write(unit=stdout,fmt=14)iter, ob_grad	 
      end if	 
14    format(i3,5x,1PD15.8,5x,1PD15.8) 
   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = MIN(iter, ntmax)
   
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') " "
   write(unit=stdout, &
      fmt='("Inner iteration stopped after ",i4," iterations")') iter
   write(unit=stdout,fmt='(A)') " "

   call da_calculate_j(it, iter, cv_size, be % cv % size_jb, &
         be % cv % size_je, be % cv % size_jp, &
         xbx, be, iv, xhat, cv, &
         re, y, j_cost, fhat,   &
         grid, config_flags                       )

    write(unit=stdout,fmt=15) iter, j_cost%total , &
       SQRT(da_dot_cv(cv_size, cv_size_domain, fhat, fhat, grid, mz, use_varbc))
15  format('Final: ',I3,' iter, J=',1PD15.8,', g=',1PD15.8)
    write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
1004 continue

   cv = cv + xhat

   if (trace_use) call da_trace_exit("da_minimise_lz")

end subroutine da_minimise_lz


