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
   integer, intent(in)               :: it                            ! external iteration.
   integer, intent(in)               :: cv_size                       ! Total cv size
   type (xbx_type),intent(inout)     :: xbx                           ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be                            ! background error structure.
   type (iv_type), intent(inout)     :: iv                            ! ob. increment vector.
   real, intent(inout)               :: j_grad_norm_target            ! Target norm.
   real, intent(out)                 :: xhat(1:cv_size)               ! control variable (local).
   real, intent(out)                 :: qhat(1:cv_size, 0:ntmax(it))  ! cv copy.
   real, intent(inout)               :: cv(1:cv_size)                 ! control variable (local).
   type (y_type), intent(inout)      :: re                            ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y                             ! y = H(x_inc) structure.
   type (j_type), intent(out)        :: j_cost                        ! cost function
   real*8, intent(out)               :: eignvec(ntmax(it), ntmax(it))
   real*8, intent(out)               :: eignval(ntmax(it))
   integer, intent(out)              :: neign
   
   integer                           :: i, j, k, n, inst, iter
   integer                           :: info, nsstwrk
   integer                           :: npred, ipred
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   real                              :: fhat(1:cv_size)        ! cv copy.
   real                              :: ghat(1:cv_size)        ! cv copy.
   real                              :: ghat0(1:cv_size)       ! cv copy.
   real                              :: shat(1:cv_size)        ! cv copy.
   real                              :: gdot, rho, mu, bndlm
   real                              :: alpha(ntmax(it)), beta(0:ntmax(it))
   real*8                            :: subdiag(ntmax(it))
   real                              :: c(cv_size), d(ntmax(it))
   real*8                            :: sstwrk(2*ntmax(it)-2)
   real                              :: bnds(ntmax(it))
   real                              :: adj_rhs, adj_sum_rhs ! < cv, cv >
   real                              :: j_total, j0_total
   real                              :: dfs, dfsmax
   
   if (trace_use) call da_trace_entry("da_minimise_lz")

   write(unit=stdout,fmt='(A)') 'Minimize cost function using Lanczos method'
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [1.0] Initialization:
   !-------------------------------------------------------------------------
   jp_start   = be % cv % size_jb + be % cv % size_je + 1
   jp_end     = be % cv % size_jb + be % cv % size_je + be % cv % size_jp
   
   call da_calculate_j(it, 0, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                       be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat, cv, re, y, j_cost, grid, config_flags)

   call da_calculate_gradj(it,0,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                           be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat+cv, y, ghat0, grid, config_flags, re)

   j0_total = j_cost%total
   if (j0_total == 0.0) then
      if (trace_use) call da_trace_exit("da_minimise_lz")
      return
   end if
   ghat      = - ghat0
   beta(0)   = SQRT(da_dot_cv(cv_size, ghat, ghat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end))
   qhat(:,0) = 0.0
   j_grad_norm_target = beta(0)

   write(unit=stdout,fmt='("Starting outer iteration : ",i3)') it
   write(unit=stdout,fmt=11) j0_total, beta(0),eps(it)
11 format('Starting cost function: ' ,1PD15.8,', Gradient: ',1PD15.8,/,&
          'For this outer iteration, the targeted reduction is:       ',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') 'Iter     Cost Function       Info Content'

   !-------------------------------------------------------------------------
   ! [2.0] Iteratively solve for minimum of cost function:
   !-------------------------------------------------------------------------
   do iter=1, ntmax(it)
      qhat(:,iter) = ghat / beta(iter-1)
      
      call da_calculate_gradj(it,iter,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                              be%cv%size_jl, be%cv%size_jt, xbx,be,iv,qhat(:,iter),y,fhat,grid,config_flags)
       
    ! Apply Lanczos recurrence and orthonormalize new gradient (using modified Gramm-Schmidt)
    !----------------------------------------------------------------------------------------
      alpha(iter) = da_dot_cv(cv_size, qhat(:,iter), fhat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end)

      ghat        = fhat - alpha(iter)*qhat(:,iter) - beta(iter-1)*qhat(:,iter-1)
      do i = iter, 1, -1
         gdot = da_dot_cv(cv_size, ghat, qhat(:,i), grid, be%cv_mz, be%ncv_mz, jp_start, jp_end)
         ghat = ghat - gdot * qhat(:,i)
      end do

      beta(iter)  = SQRT(da_dot_cv (cv_size, ghat, ghat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end))
      
    ! Lanczos iteration  
    !------------------
      if (iter == 1) then
         d(1) = alpha(1)
	 c    = qhat(:,1)
	 rho  = beta(0) / alpha(1)
	 xhat = rho * qhat(:,1)
         dfs  = da_dot_cv(cv_size, xhat, xhat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end)
         dfsmax = dfs
      else
         mu      = beta(iter-1) / d(iter-1)
	 d(iter) = alpha(iter) - beta(iter-1)*mu
	 c       = qhat(:,iter) - mu*c
	 rho     = - mu*d(iter-1)*rho / d(iter)
	 xhat    = xhat + rho*c
         dfs     = da_dot_cv(cv_size, rho*c, rho*c, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end)
         if (dfs > dfsmax) dfsmax = dfs
      end if
      
    ! Determine eigenvalues and eigenvectors of the Lanczos tri-diagonal matrix
    !--------------------------------------------------------------------------
      eignval(1:iter)   = alpha(1:iter)
      subdiag(1:iter-1) = beta(1:iter-1)
      nsstwrk           = MAX(2*iter-2,1)
      info              = 0
      call DSTEQR('I',iter,eignval(1:iter),subdiag(1:iter-1),eignvec(:,1:iter),ntmax(it),&
	           sstwrk(1:nsstwrk),info)
      if (info /=0) write(stdout,*) 'Error in Lanczos minimization: SSTEQR returned ',info 
            
    ! Count converged eigenpairs (using Arnoldi relation)
    !----------------------------------------------------
      bndlm        = eps(it) * eignval(iter)  
      bnds(1:iter) = abs(beta(iter) * eignvec(iter,1:iter))
      neign        = COUNT(bnds(1:iter) <= bndlm)
               
    ! Print Cost Function and Infomation Content
    !-----------------------------------
      j_total = j0_total + 0.5 * da_dot_cv(cv_size,ghat0,xhat,grid,be%cv_mz,be%ncv_mz,jp_start,jp_end)
      write(unit=stdout,fmt=12)iter, j_total, dfs      	 
12    format(i3,5x,1PD15.8,5x,1PD15.8)

    ! Stop minimization based on Information Content
    ! ----------------------------------------------
      if (sqrt(dfs) < eps(it) * sqrt(dfsmax)) exit

    ! Option to calculate J Cost Function explicitly
    ! ----------------------------------------------
      if (calculate_cg_cost_fn) &
         call da_calculate_j(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                             be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat, cv, re, y, j_cost, grid, config_flags)

      if ( write_detail_grad_fn ) then
         call da_calculate_gradj(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                                 be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat+cv, y, fhat, grid, config_flags, re)
      end if

   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = MIN(iter, ntmax(it))
   
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') " "
   write(unit=stdout, fmt='("Inner iteration stopped after ",i4," iterations")') iter
   write(unit=stdout,fmt='(A)') " "

   call da_calculate_j(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                       be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat, cv, re, y, j_cost, grid, config_flags)

   call da_calculate_gradj(it,iter,cv_size,be%cv%size_jb,be%cv%size_je,be%cv%size_jp, &
                           be%cv%size_jl, be%cv%size_jt, xbx, be, iv, xhat+cv, y, ghat, grid, config_flags, re)

   write(unit=stdout,fmt=15) iter, j_cost%total , &
                             SQRT(da_dot_cv(cv_size, ghat, ghat, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end))
15  format('Final: ',I3,' iter, J=',1PD15.8,', g=',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'

   write(stdout,*) 'Ritz eigenvalues: ',eignval(iter:1:-1)     
   write(stdout,*) 'Number of converged eigenpairs: ', neign
   neign = iter

   if (trace_use) call da_trace_exit("da_minimise_lz")

end subroutine da_minimise_lz


