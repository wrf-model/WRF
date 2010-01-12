subroutine da_calculate_gradj(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                              xbx, be, iv, xhat, cv, re, y, grad, grid, config_flags  )

   !---------------------------------------------------------------------------
   ! Purpose: Calculates the gradient of the cost function w/r to cv
   !
   ! Called from da_minimise_cg (or da_minimise_lz)
   !
   ! History: 12/12/08 - Creation from da_calculate_j (Tom Auligne)
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: it     ! external iteration #.
   integer, intent(in)                :: iter   ! internal iteration #.
   integer, intent(in)                :: cv_size    ! Total cv size.
   integer, intent(in)                :: cv_size_jb, cv_size_je, cv_size_jp
   type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: xhat   (1:cv_size)   ! control variables.
   real, intent(in)                   :: cv     (1:cv_size)   ! control variables.
   type (y_type), intent(inout)       :: re     ! residual vector (o-a).
   type (y_type), intent(inout)       :: y
   real, intent(out)                  :: grad(cv_size)        ! gradient of cost function
   
   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   real             :: jo_partial                   ! jo for this processor
   type (y_type)    :: jo_grad_y                    ! Grad_y(jo)
   integer          :: mz(6)
   real             :: grad_jo(cv_size)
   real             :: grad_jb(cv_size)
   real             :: grad_je(cv_size)
   real             :: grad_jp(cv_size)
   real             :: grad_js(cv_size)
   real             :: gnorm_jb, gnorm_jo, gnorm_j, gnorm_je, gnorm_js, gnorm_jp

   ! Variables for VarBC background constraint
   real :: xhat_jp(cv_size_jp) ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr
   integer                           :: n

   if (trace_use) call da_trace_entry("da_calculate_gradj")

   !-------------------------------------------------------------------------
   ! [0.0] initialization:
   !-------------------------------------------------------------------------
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /)

   je_start   = cv_size_jb + 1
   je_end     = cv_size_jb + cv_size_je
   jp_start   = cv_size_jb + cv_size_je + 1
   jp_end     = cv_size_jb + cv_size_je + cv_size_jp
   
   grad_jo = 0.0
   grad_jb = 0.0
   grad_je = 0.0
   grad_jp = 0.0
   grad_js = 0.0

   !-------------------------------------------------------------------------
   ! [1.0] calculate grad_v (jo):
   !-------------------------------------------------------------------------
   call da_allocate_y(iv, jo_grad_y)
   
   if (it < 0) then
      call da_calculate_residual(iv, y, re)
      call da_calculate_grady(iv, re, jo_grad_y)   
      call da_transform_vtoy_adj(iter, cv_size, be, grid%ep, grad_jo, iv, &
         grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, .true.)
   else
      call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, &
           grid%vv, xbx, y, grid, config_flags)
      call da_calculate_grady(iv, y, jo_grad_y)   
      call da_transform_vtoy_adj(iter, cv_size, be, grid%ep, grad_jo, iv, &
           grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, .true.)
      grad_jo = - grad_jo    !! Compensate for sign in calculation of grad_v (Jo)
   end if
      
   call da_deallocate_y(jo_grad_y)

   !-------------------------------------------------------------------------
   ! [2.0] calculate grad_v (jb):
   !-------------------------------------------------------------------------
   if (cv_size_jb > 0) then
      if (it < 0) then
         grad_jb(1:cv_size_jb) = jb_factor * (xhat(1:cv_size_jb)+cv(1:cv_size_jb))
      else
         grad_jb(1:cv_size_jb) = jb_factor * (xhat(1:cv_size_jb))
      end if
   endif

   !-------------------------------------------------------------------------
   ! [3.0] calculate grad_v (je):
   !-------------------------------------------------------------------------
   if (cv_size_je > 0) then
      if (it < 0) then
         grad_je(je_start:je_end) = je_factor * (xhat(je_start:je_end)+cv(je_start:je_end))
      else
         grad_je(je_start:je_end) = je_factor * (xhat(je_start:je_end))
      end if
   end if
   
   !-------------------------------------------------------------------------
   ! [4.0] calculate grad_v (jp):
   !-------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc) then
      xhat_jp(1:cv_size_jp) = 0.0
      do inst = 1, iv % num_inst   
         do ichan = 1, iv%instid(inst)%nchan
            if (satinfo(inst)%iuse(ichan) <= 0) cycle
            npred    = iv%instid(inst)%varbc(ichan)%npred
            if (npred <= 0) cycle               !! VarBC channels only	 
            do ipred = 1, npred
               id     = iv%instid(inst)%varbc(ichan)%index(ipred)
	       bgerr  = iv%instid(inst)%varbc(ichan)%bgerr(ipred)
	       if (bgerr > 0.0) &
    	          xhat_jp(id-jp_start+1) = (1/bgerr) * &
                     SUM( (cv(id)+xhat(id)) * &
                     iv%instid(inst)%varbc(ichan)%vtox(ipred,1:npred))
	    end do
         end do
      end do
      if (it < 0) then
         grad_jp(jp_start:jp_end) = xhat_jp(1:cv_size_jp) + cv(jp_start:jp_end)
      else
         grad_jp(jp_start:jp_end) = xhat_jp(1:cv_size_jp)
      end if
   
   end if
#endif
      
   !-------------------------------------------------------------------------
   ! [5.0] calculate grad_v (js):
   !-------------------------------------------------------------------------
   if (ANY(use_satcv)) then
      do inst = 1, iv % num_inst   
         do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
         ! Skin Temperature
         !-----------------
	    if (use_satcv(1)) &
            grad_js(iv%instid(inst)%cv_index(n)%ts) = xhat(iv%instid(inst)%cv_index(n)%ts) + cv(iv%instid(inst)%cv_index(n)%ts)
	    
         ! Cloud cover(s)
         !---------------
	    if (use_satcv(2)) then
	    grad_js(iv%instid(inst)%cv_index(n)%cc) = xhat(iv%instid(inst)%cv_index(n)%cc) + cv(iv%instid(inst)%cv_index(n)%cc)

	    WHERE (xhat(iv%instid(inst)%cv_index(n)%cc) < 0.0 .or.                              &
	           xhat(iv%instid(inst)%cv_index(n)%cc) > 1.0 )                                 &
	    grad_js(iv%instid(inst)%cv_index(n)%cc) = grad_js(iv%instid(inst)%cv_index(n)%cc) + &
                                                       10.0 * (xhat(iv%instid(inst)%cv_index(n)%cc)+cv(iv%instid(inst)%cv_index(n)%cc))
            end if
	 end do
      end do	      
   end if

   !--------------------------------------------------------------------------------------------------
   ! [6.0] calculate grad_v (j) = grad_v (jb) + grad_v (jo) + grad_v (je) + grad_v (jp) + grad_v (js):
   !--------------------------------------------------------------------------------------------------   
   grad = grad_jo + grad_jb + grad_je + grad_jp + grad_js

   !-------------------------------------------------------------------------
   ! [7.0] write gradient:
   !-------------------------------------------------------------------------
   gnorm_jo = sqrt(da_dot_cv(cv_size, cv_size_domain, grad_jo, grad_jo, grid, mz, use_varbc))
   gnorm_jb = sqrt(da_dot_cv(cv_size, cv_size_domain, grad_jb, grad_jb, grid, mz, use_varbc))
   gnorm_je = sqrt(da_dot_cv(cv_size, cv_size_domain, grad_je, grad_je, grid, mz, use_varbc))
   gnorm_jp = sqrt(da_dot_cv(cv_size, cv_size_domain, grad_jp, grad_jp, grid, mz, use_varbc))
   gnorm_js = sqrt(da_dot_cv(cv_size, cv_size_domain, grad_js, grad_js, grid, mz, use_varbc))
   gnorm_j  = sqrt(da_dot_cv(cv_size, cv_size_domain, grad,    grad,    grid, mz, use_varbc))
 
   if (it == -1 .and. iter == 0 .and. rootproc) then
      write(unit=grad_unit,fmt='(a)')'Outer    EPS     Inner      G           Gb       Go           Ge           Gp           Gs'
      write(unit=grad_unit,fmt='(a)')'Iter             Iter                            '
   end if

   if (it <0 .and. rootproc) then
      write(grad_unit,fmt='(2x,i2,1x,e10.3,2x,i4,7(1x,f10.3))') & 
         -it, eps(-it), iter, gnorm_j, gnorm_jb, gnorm_jo, gnorm_je, gnorm_jp, gnorm_js
   end if
   
   if (trace_use) call da_trace_exit("da_calculate_gradj")

end subroutine da_calculate_gradj


