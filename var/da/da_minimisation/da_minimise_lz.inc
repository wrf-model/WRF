subroutine da_minimise_lz(grid, config_flags,            &
                           it, cv_size, xbx, be, iv,     &
                           j_grad_norm_target, xhat, qhat, cv, &
                           re, y, j_cost, eignvec, eignval, neign)

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

   type(domain), intent(inout)       :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   integer, intent(in)               :: it                     ! external iteration.
   integer, intent(in)               :: cv_size                ! Total cv size
   type (xbx_type),intent(in)        :: xbx                    ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                     ! background error structure.
   type (iv_type), intent(inout)     :: iv                     ! ob. increment vector.
   real, intent(inout)               :: j_grad_norm_target     ! Target norm.
   real, intent(out)                 :: xhat(1:cv_size)        ! control variable (local).
   real, intent(out)                 :: qhat(1:cv_size, 0:ntmax)  ! cv copy.
   real, intent(inout)               :: cv(1:cv_size)          ! control variable (local).
   type (y_type), intent(inout)      :: re                     ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y                      ! y = H(x_inc) structure.
   type (j_type), intent(out)        :: j_cost                 ! cost function
   real*8, intent(out)               :: eignvec(ntmax, ntmax)
   real*8, intent(out)               :: eignval(ntmax)
   integer, intent(out)              :: neign
   
   integer                           :: i, j, k, n, inst, iter
   integer                           :: mz(7), info, nsstwrk
   integer                           :: npred, ipred
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   real                              :: fhat(1:cv_size)        ! cv copy.
   real                              :: ghat(1:cv_size)        ! cv copy.
   real                              :: ghat0(1:cv_size)       ! cv copy.
   real                              :: shat(1:cv_size)        ! cv copy.
   real                              :: gdot, rho, mu, bndlm
   real                              :: alpha(ntmax), beta(0:ntmax)
   real*8                            :: subdiag(ntmax)
   real                              :: c(cv_size), d(ntmax)
   real*8                            :: sstwrk(2*ntmax-2)
   real                              :: bnds(ntmax)
   real                              :: adj_rhs, adj_sum_rhs ! < cv, cv >
   real                              :: j_total, j0_total, rrmnew_norm
   
   if (trace_use) call da_trace_entry("da_minimise_lz")

   write(unit=stdout,fmt='(A)') 'Minimize cost function using Lanczos method'
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [1.0] Initialization:
   !-------------------------------------------------------------------------
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be % ne /)
   
   jp_start   = be % cv % size_jb + be % cv % size_je + 1
   jp_end     = be % cv % size_jb + be % cv % size_je + be % cv % size_jp
   
   call da_calculate_j(it, 0, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                       xbx, be, iv, xhat, cv, re, y, j_cost, ghat0, grid, config_flags)
 
   j0_total = j_cost%total
   if (j0_total == 0.0) return
   ghat      = - ghat0
   beta(0)   = SQRT(da_dot_cv(cv_size, ghat, ghat, grid, mz, jp_start, jp_end))
   qhat(:,0) = 0.0
   j_grad_norm_target = beta(0)

   write(unit=stdout,fmt='("Starting outer iteration : ",i3)') it
   write(unit=stdout,fmt=11) j0_total, beta(0),eps(it)*beta(0)
11 format('Starting cost function: ' ,1PD15.8,', Gradient: ',1PD15.8,/,&
          'For this outer iteration gradient target is:       ',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') 'Iter    Cost Function         Gradient'

   !-------------------------------------------------------------------------
   ! [2.0] Iteratively solve for minimum of cost function:
   !-------------------------------------------------------------------------
   do iter=1, ntmax      
      qhat(:,iter) = ghat / beta(iter-1)
      
      call da_calculate_gradj(it,iter,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                              xbx,be,iv,qhat(:,iter),y,fhat,grid,config_flags)
       
    ! Apply Lanczos recurrence and orthonormalize new gradient (using modified Gramm-Schmidt)
    !----------------------------------------------------------------------------------------
      alpha(iter) = da_dot_cv(cv_size, qhat(:,iter), fhat, grid, mz, jp_start, jp_end)

      ghat        = fhat - alpha(iter)*qhat(:,iter) - beta(iter-1)*qhat(:,iter-1)
      do i = iter, 1, -1
         gdot = da_dot_cv(cv_size, ghat, qhat(:,i), grid, mz, jp_start, jp_end)
         ghat = ghat - gdot * qhat(:,i)
      end do

      beta(iter)  = SQRT(da_dot_cv (cv_size, ghat, ghat, grid, mz, jp_start, jp_end))
      
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
            
    ! Count converged eigenpairs (using Arnoldi relation)
    !----------------------------------------------------
      bndlm        = eps(it) * eignval(iter)  
      bnds(1:iter) = abs(beta(iter) * eignvec(iter,1:iter))
      neign        = COUNT(bnds(1:iter) <= bndlm)
               
      rrmnew_norm = SQRT(da_dot_cv(cv_size, ghat, ghat, grid, mz, jp_start, jp_end))

    ! Print Gradient (and Cost Function)
    !-----------------------------------
      j_total = j0_total + 0.5 * da_dot_cv(cv_size,ghat0,xhat,grid,mz,jp_start,jp_end)
      write(unit=stdout,fmt=12)iter, j_total, rrmnew_norm      	 
12    format(i3,5x,1PD15.8,5x,1PD15.8)

      if (rrmnew_norm < eps(it) * j_grad_norm_target) exit      

      if (calculate_cg_cost_fn) &
         call da_calculate_j(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
	                     xbx, be, iv, xhat, cv, re, y, j_cost, fhat, grid, config_flags)
   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = MIN(iter, ntmax)
   
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') " "
   write(unit=stdout, fmt='("Inner iteration stopped after ",i4," iterations")') iter
   write(unit=stdout,fmt='(A)') " "

   call da_calculate_j(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                       xbx, be, iv, xhat, cv, re, y, j_cost, ghat, grid, config_flags)

   write(unit=stdout,fmt=15) iter, j_cost%total , &
                             SQRT(da_dot_cv(cv_size, ghat, ghat, grid, mz, jp_start, jp_end))
15  format('Final: ',I3,' iter, J=',1PD15.8,', g=',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'

   write(stdout,*) 'Ritz eigenvalues: ',eignval(iter:1:-1)     
   write(stdout,*) 'Number of converged eigenpairs: ', neign

   if (adj_sens) &
      call da_sensitivity(iter,grid,config_flags,cv_size,xbx,be,iv,xhat,qhat,cv,y,eignvec,eignval,shat)
   
   cv = cv + xhat

   if (trace_use) call da_trace_exit("da_minimise_lz")

end subroutine da_minimise_lz


