subroutine da_minimise_cg(grid, config_flags,            &
                           it, cv_size, xbx, be, iv, &
                           j_grad_norm_target, xhat, cv, &
                           re, y, j)

   !-------------------------------------------------------------------------
   ! Purpose:         Main Conjugate Gradient minimisation routine 
   !
   ! Here 
   !    cv   is updated in outer-loop.
   !    xhat is the control variable in inner-loop.
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

   type (j_type), intent(out)    :: j                         ! cost function

   type(domain), intent(inout)   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer                           :: iter            
   integer                           :: je_start, je_end       ! Start/end indices of Je.
   integer                           :: cv_size_jb             ! end indices of Jb.
   real                              :: j_grad(1:cv_size)      ! grad_v j (local-grid)
   real                              :: fhat(1:cv_size)        ! cv copy.
   real                              :: ghat(1:cv_size)        ! cv copy.
   real                              :: phat(1:cv_size)        ! cv copy.
   real                              :: apdotp,step,rrmold,rrmnew,ratio 
   real                              :: ob_grad,rrmnew_norm
   integer                           :: mz(6)

   type (y_type)                     :: jo_grad_y              ! Grad_y(Jo)
 
   ! Variables for Conjugate Gradient preconditioning
   real                              :: precon(1:cv_size)      ! cv copy.
   real                              :: j_total, g_total, g_partial, jo_partial                          
   integer                           :: ii, nv, nn, istart, iend, sz(5)
   
   ! Variables for VarBC background constraint
   real                              :: xhat_jp(be%cv%size_jp) ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr

   
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

   ! Initialize temporary cv structures:
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /)
   if ( .not. anal_type_randomcv ) xhat = 0.0
   j_grad = 0.0
   fhat = 0.0

   call da_allocate_y(iv, jo_grad_y)

   call da_calculate_j(it, 0, cv_size, be % cv % size_jb, be % cv % size_je, &
                        be % cv % size_jp, &
			xbx, be, iv, xhat, cv, &
                        re, y, j, j_grad,         &
                        grid, config_flags                       )

   ghat  = j_grad
   
   ! [1.1] Preconditioning:
   !-----------------------
   precon  = 1.0
   
   if (precondition_cg) then
      g_total = da_dot(cv_size,ghat,ghat)
      j_total = j% total
      
      iend    = 0
      sz      = (/be%cv%size1, be%cv%size2, be%cv%size3, be%cv%size4, be%cv%size5/)
      do nv = 1, 5
         nn = sz(nv) / mz(nv)
	 do ii = 1, mz(nv)
            istart = iend + 1
            iend   = istart + nn - 1
	    g_partial  = da_dot(nn, ghat(istart:iend), ghat(istart:iend))
            jo_partial = j_total / SUM(mz(1:5))

	    precon(istart:iend)=  1 / &
	       (1 + precondition_factor*(g_partial/g_total)/(jo_partial/j_total)) 
	 end do
      end do
   end if
   
   phat  = - precon * ghat

   rrmold = da_dot_cv(cv_size, cv_size_domain, -phat, ghat, grid, mz, use_varbc)

   if (j%total == 0.0) go to 1004

   if (it == 1) j_grad_norm_target = sqrt (rrmold)

   write(unit=stdout,fmt='("Starting outer iteration : ",i3)') it
   write(unit=stdout,fmt=11) j_grad_norm_target,eps(it)*j_grad_norm_target, &
              j%total, sqrt(rrmold)
11 format('Original gradient is ',1PD15.8,/,&
          'For this outer iteration gradient target is ',1PD15.8,/,&
          'Starting cost function: ' ,1PD15.8,' gradient= ',1PD15.8)
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
      if (rrmold == 0.0) go to 1002

      ! calculate grad_v (Jo):
      !-----------------------
      call da_transform_vtoy(cv_size, be, grid%ep, phat, iv, grid%vp, &
         grid%vv, xbx, y,         &
         grid, config_flags                               )

      call da_calculate_grady(iv, y , jo_grad_y)

      call da_transform_vtoy_adj(iter, cv_size, be, grid%ep, fhat, iv, &
         grid%vp, grid%vv, xbx, jo_grad_y, &
         grid, config_flags, .true.)

      fhat = -fhat    !! Compensate for sign in calculation of grad_v (Jo)

      ! calculate grad_v (Jb):
      !-----------------------
      if (be % cv % size_jb > 0) then
         cv_size_jb = be % cv % size_jb
         fhat(1:cv_size_jb) = fhat(1:cv_size_jb) + jb_factor*phat(1:cv_size_jb)
      end if 	 

      ! calculate grad_v (Je):
      !-----------------------
      if (be % cv % size_je > 0) then
       	 je_start = be % cv % size_jb + 1
         je_end = be % cv % size_jb + be % cv % size_je
         fhat(je_start:je_end) = fhat(je_start:je_end) + &
                                   je_factor*phat(je_start:je_end)
      end if
      	 
      ! calculate grad_v (Jp):
      !-----------------------
      if (use_varbc) then
         jp_start = be % cv % size_jb + be % cv % size_je + 1
         jp_end   = be % cv % size_jb + be % cv % size_je + be % cv % size_jp
	 xhat_jp  = 0.0
         do inst = 1, iv % num_inst   
            do ichan = 1, iv%instid(inst)%nchan
               npred    = iv%instid(inst)%varbc(ichan)%npred
               if (npred <= 0) cycle               !! VarBC channels only	 
               do ipred = 1, npred
                  id     = iv%instid(inst)%varbc(ichan)%index(ipred)
	          bgerr  = iv%instid(inst)%varbc(ichan)%bgerr(ipred)
	          if (bgerr > 0.0) &
    	             xhat_jp(id-jp_start+1) = (1/sqrt(bgerr)) * &
		     SUM(phat(id) * iv%instid(inst)%varbc(ichan)%vtox(ipred,1:npred))            
	       end do
            end do
         end do
         fhat(jp_start:jp_end) = fhat(jp_start:jp_end) + xhat_jp(1:be%cv%size_jp)
      end if
      
      apdotp = da_dot_cv(cv_size, cv_size_domain, fhat, phat, grid, mz, use_varbc)

      step = 0.0
      if (apdotp .gt. 0.0) step = rrmold/apdotp
      ghat = ghat + step * fhat
      xhat = xhat + step * phat
      if (calculate_cg_cost_fn) then
         j_grad = 0.0
         call da_calculate_j(it, iter, cv_size, be % cv % size_jb, &
            be % cv % size_je, be % cv % size_jp, &
            xbx, be, iv, xhat, cv, &
            re, y, j, j_grad,         &
            grid, config_flags                       )
            ob_grad = da_dot_cv(cv_size, cv_size_domain, j_grad, j_grad, grid, &
                                mz, use_varbc)
         ob_grad = sqrt(ob_grad)
      end if

      rrmnew = da_dot_cv (cv_size, cv_size_domain, precon*ghat, ghat, grid, &
                          mz, use_varbc)
			  
      rrmnew_norm = sqrt(rrmnew)

      if (rrmnew_norm  < eps(it) * j_grad_norm_target) go to 1002
      ratio = 0.0
      if (rrmold .gt. 0.0) ratio = rrmnew/rrmold

      phat = - precon * ghat       + ratio * phat

      rrmold=rrmnew
      if (calculate_cg_cost_fn) then
         write(unit=stdout,fmt=12)iter, j%total, ob_grad, step
      else
         write(unit=stdout,fmt=14)iter, rrmnew_norm , step
      end if
12    format(i3,5x,1PD15.8,5x,1PD15.8,5x,1PD15.8)
14    format(i3,5x,1PD15.8,5x,1PD15.8)
   end do

   !-------------------------------------------------------------------------
   ! End of the minimization of cost function
   !-------------------------------------------------------------------------
   iter = iter -1
   go to 1003
1002 continue
   if (calculate_cg_cost_fn) then
      write(unit=stdout,fmt=12)iter, j%total, ob_grad, step
   else
      write(unit=stdout,fmt=14)iter, rrmnew_norm , step
   end if
1003 continue
   write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
   write(unit=stdout,fmt='(A)') " "
   write(unit=stdout, &
      fmt='("Inner iteration stopped after ",i4," iterations")') iter
   write(unit=stdout,fmt='(A)') " "

   if (calculate_cg_cost_fn) then 
      rrmnew_norm = ob_grad
   else
      call da_calculate_j(it, iter, cv_size, be % cv % size_jb, &
         be % cv % size_je, be % cv % size_jp, &
         xbx, be, iv, xhat, cv, &
         re, y, j, j_grad,         &
         grid, config_flags                       )

      rrmnew_norm = da_dot_cv(cv_size, cv_size_domain, j_grad, j_grad, grid, &
                               mz, use_varbc)
      rrmnew_norm = sqrt(rrmnew_norm)
    end if

    write(unit=stdout,fmt=15) iter, j%total , rrmnew_norm
15  format('Final: ',I3,' iter, J=',1PD15.8,', g=',1PD15.8)
    write(unit=stdout,fmt='(A)') &
      '----------------------------------------------------------'
1004 continue

   cv = cv + xhat

   call da_deallocate_y(jo_grad_y)   

   if (trace_use) call da_trace_exit("da_minimise_cg")

end subroutine da_minimise_cg


