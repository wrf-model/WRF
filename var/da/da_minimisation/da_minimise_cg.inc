subroutine da_minimise_cg(grid, config_flags,            &
                           it, cv_size, xbx, be, iv, &
                           j_grad_norm_target, xhat, cv, &
                           re, y, j_cost)

   !-------------------------------------------------------------------------
   ! Purpose:         Main Conjugate Gradient minimisation routine 
   !
   ! Here 
   !    cv   is updated in outer-loop.
   !    xhat is the control variable in inner-loop.
   !
   ! Called from da_solve
   !
   ! History: 12/12/08 - Split J and GradJ calculations (Tom Auligne)
   !          12/12/08 - Re-orthonormalization option   (Tom Auligne)
   !
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)               :: it    ! external iteration.
   integer, intent(in)               :: cv_size          ! Total cv size
   type (xbx_type),intent(in)        :: xbx   ! Header & non-gridded vars.
   type (be_type), intent(in)        :: be    ! background error structure.
   type (iv_type), intent(inout)     :: iv    ! ob. increment vector.
   real, intent(inout)               :: j_grad_norm_target ! Target norm.
   real, intent(out)                 :: xhat(1:cv_size)  ! control variable (local).
   real, intent(inout)               :: cv(1:cv_size)    ! control variable (local).
   type (y_type), intent(inout)      :: re    ! residual (o-a) structure.
   type (y_type), intent(inout)      :: y     ! y = H(x_inc) structure.

   type (j_type), intent(out)        :: j_cost                 ! cost function

   type(domain), intent(inout)       :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer                           :: iter            
   integer                           :: je_start, je_end       ! Start/end indices of Je.
   integer                           :: cv_size_jb             ! end indices of Jb.
   integer                           :: mz(6)
   real                              :: fhat(1:cv_size)        ! cv copy.
   real                              :: ghat(1:cv_size)        ! cv copy.
   real                              :: ghat0(1:cv_size)       ! cv copy.
   real                              :: phat(1:cv_size)        ! cv copy.
   real, allocatable                 :: qhat(:,:)              ! cv copy.
   real                              :: apdotp,step,rrmold,rrmnew,ratio 
   real                              :: ob_grad, rrmnew_norm, gdot
   real                              :: j_total    
 
   ! Variables for Conjugate Gradient preconditioning
   real                              :: precon(1:cv_size)      ! cv copy.
   real                              :: g_total, g_partial, jo_partial                          
   integer                           :: i, ii, nv, nn, istart, iend, sz(5)
      
   if (trace_use) call da_trace_entry("da_minimise_cg")

   write(unit=stdout,fmt='(A)') 'Minimize cost function using CG method'
   if (calculate_cg_cost_fn) then
      write(unit=stdout,fmt='(A)') &
         'For this run cost function diagnostics will be written'
   else
      write(unit=stdout,fmt='(A)') &
         'For this run cost function diagnostics will not be written'
   end if
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [1.0] Initialization:
   !-------------------------------------------------------------------------
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /)
   sz = (/ be%cv%size1, be%cv%size2, be%cv%size3, be%cv%size4, be%cv%size5 /)
   
   call da_calculate_j(it, 0, cv_size, be % cv % size_jb, be % cv % size_je, &
                        be % cv % size_jp, xbx, be, iv, xhat, cv, &
                        re, y, j_cost, grid, config_flags)

   call da_calculate_gradj(-it, 0, cv_size, be % cv % size_jb, be % cv % size_je, &
                        be % cv % size_jp, xbx, be, iv,  xhat, cv, re, y, ghat, grid, config_flags)
   ghat0 = ghat
   
   ! [1.1] Preconditioning:
   !-----------------------
   precon  = 1.0
   
   if (precondition_cg) then
      g_total = da_dot(cv_size,ghat,ghat)
      
      iend    = 0
      do nv = 1, 5
         nn = sz(nv) / mz(nv)
	 do ii = 1, mz(nv)
            istart     = iend + 1
            iend       = istart + nn - 1
	    g_partial  = da_dot(nn, ghat(istart:iend), ghat(istart:iend))
            jo_partial = j_cost%total / SUM(mz(1:5))

	    precon(istart:iend)=  1 / &
	       (1 + precondition_factor*(g_partial/g_total)/(jo_partial/j_cost%total)) 
	 end do
      end do
   end if
   
   phat  = - precon * ghat

   rrmold = da_dot_cv(cv_size, cv_size_domain, -phat, ghat, grid, mz, use_varbc)

   if (j_cost%total == 0.0) return

   if (it == 1) j_grad_norm_target = sqrt (rrmold)

   if (orthonorm_gradient) then
      allocate(qhat(1:cv_size, 0:ntmax))
      qhat(:,0) = ghat / rrmold
   end if

   write(unit=stdout,fmt='("Starting outer iteration : ",i3)') it
   write(unit=stdout,fmt=11) j_cost%total, sqrt(rrmold), eps(it)*j_grad_norm_target
11 format('Starting cost function: ' ,1PD15.8,', Gradient= ',1PD15.8,/,&
          'For this outer iteration gradient target is:       ',1PD15.8)
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   if (calculate_cg_cost_fn) then
      write(unit=stdout,fmt='(A)') &
         'Iter    Cost Function         Gradient             Step'
   else
      write(unit=stdout,fmt='(A)')'Iter      Gradient             Step'
   end if

   !-------------------------------------------------------------------------
   ! [2.0] iteratively solve for minimum of cost function:
   !-------------------------------------------------------------------------

   do iter=1, ntmax
      if (rrmold == 0.0) exit

      call da_calculate_gradj(it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                              xbx, be, iv, phat, cv, re, y, fhat, grid, config_flags  )				 
      
      apdotp = da_dot_cv(cv_size, cv_size_domain, fhat, phat, grid, mz, use_varbc)

      step = 0.0
      if (apdotp .gt. 0.0) step = rrmold/apdotp
      
      ghat = ghat + step * fhat
      xhat = xhat + step * phat
      
    ! Orthonormalize new gradient (using modified Gramm-Schmidt algorithm)
      if (orthonorm_gradient) then
         do i = iter-1, 0, -1
            gdot = da_dot_cv(cv_size, cv_size_domain, ghat, qhat(:,i), grid, mz, use_varbc)
            ghat = ghat - gdot * qhat(:,i)
         end do
      end if
      
      rrmnew = da_dot_cv (cv_size, cv_size_domain, precon*ghat, ghat, grid, &
                          mz, use_varbc)
			  
      rrmnew_norm = sqrt(rrmnew)

      if (rrmnew_norm  < eps(it) * j_grad_norm_target) exit
      ratio = 0.0
      if (rrmold .gt. 0.0) ratio = rrmnew/rrmold

      if (orthonorm_gradient) qhat(:,iter) = ghat / rrmnew_norm
      phat         = - precon * ghat       + ratio * phat

      rrmold=rrmnew

    ! Print Gradient (and Cost Function)
    !-----------------------------------
      if (print_detail_grad) then
         call da_calculate_j(it, iter, cv_size, be % cv % size_jb, be % cv % size_je, &
                             be % cv % size_jp, xbx, be, iv, xhat, cv, &
                             re, y, j_cost, grid, config_flags)
         call da_calculate_gradj(-it, iter, cv_size, be%cv%size_jb, be%cv%size_je, be%cv%size_jp, &
                                 xbx, be, iv, xhat, cv, re, y, fhat, grid, config_flags  )				 
         write(unit=stdout,fmt=12)iter, j_cost%total, rrmnew_norm, step
      elseif (calculate_cg_cost_fn) then      		 
         j_total = j_cost%total + 0.5 * da_dot_cv(cv_size,cv_size_domain,ghat0,xhat,grid,mz,use_varbc)
         write(unit=stdout,fmt=12)iter, j_total, rrmnew_norm, step         	 
      else
         write(unit=stdout,fmt=14)iter, rrmnew_norm , step
      end if

12    format(i3,5x,1PD15.8,5x,1PD15.8,5x,1PD15.8)
14    format(i3,5x,1PD15.8,5x,1PD15.8)
   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = MIN(iter, ntmax)
   if (orthonorm_gradient) deallocate(qhat)
   
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') " "
   write(unit=stdout, &
      fmt='("Inner iteration stopped after ",i4," iterations")') iter
   write(unit=stdout,fmt='(A)') " "

   call da_calculate_j(it, iter, cv_size, be % cv % size_jb, &
         be % cv % size_je, be % cv % size_jp, xbx, be, iv, xhat, cv, &
	 re, y, j_cost,grid, config_flags)

   call da_calculate_gradj(-it, iter, cv_size, be % cv % size_jb, be % cv % size_je, &
                           be % cv % size_jp, xbx, be, iv,  xhat, cv, &
			   re, y, ghat, grid, config_flags)
	
   rrmnew_norm = SQRT(da_dot_cv(cv_size,cv_size_domain,ghat,ghat,grid,mz,use_varbc))

    write(unit=stdout,fmt=15) iter, j_cost%total , rrmnew_norm
15  format('Final: ',I3,' iter, J=',1PD15.8,', g=',1PD15.8)
    write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'

   cv = cv + xhat

   if (trace_use) call da_trace_exit("da_minimise_cg")

end subroutine da_minimise_cg
