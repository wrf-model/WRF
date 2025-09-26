subroutine da_calculate_gradj(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                              cv_size_jl, cv_size_jt, xbx, be, iv, cv, y, grad, grid, config_flags, re )

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
   integer, intent(in)                :: cv_size_jb, cv_size_je, cv_size_jp, cv_size_jl, cv_size_jt
   type (xbx_type),intent(inout)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: cv     (1:cv_size)   ! control variables.
   type (y_type), intent(inout)       :: y
   real, intent(out)                  :: grad(cv_size)        ! gradient of cost function
   type (y_type), optional, intent(inout) :: re     ! residual vector (o-a).
   
   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   integer          :: jl_start, jl_end             ! Start/end indices of Jl.
   integer          :: jt_start, jt_end, iphase
   real             :: jo_partial                   ! jo for this processor
   type (y_type)    :: jo_grad_y                    ! Grad_y(jo)
   real             :: grad_jo(cv_size)
   real             :: grad_jb(cv_size)
   real             :: grad_je(cv_size)
   real             :: grad_jd(cv_size)
   real             :: grad_jp(cv_size)
   real             :: grad_js(cv_size)
   real             :: grad_jl(cv_size)
   real             :: grad_jm(cv_size)
   real             :: grad_jt(cv_size)
   real             :: gnorm_j, gnorm_jo, gnorm_jb, gnorm_je, gnorm_jd, gnorm_jp, gnorm_js, gnorm_jl, gnorm_jt
   real             :: gnorm_jm
   logical          :: jcdf_flag

   real             :: inc_div(ims:ime,jms:jme,kms:kme) ! Temp storage

   ! Variables for VarBC background constraint
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr
   integer                           :: n

   if (trace_use) call da_trace_entry("da_calculate_gradj")

   !-------------------------------------------------------------------------
   ! [0.0] initialization:
   !-------------------------------------------------------------------------
   je_start   = cv_size_jb + 1
   je_end     = cv_size_jb + cv_size_je
   jp_start   = cv_size_jb + cv_size_je + 1
   jp_end     = cv_size_jb + cv_size_je + cv_size_jp
   jl_start   = cv_size_jb + cv_size_je + cv_size_jp + 1
   jl_end     = cv_size_jb + cv_size_je + cv_size_jp + cv_size_jl
   jt_start   = jl_end + 1
   jt_end     = jl_end + cv_size_jt

   grad_jo = 0.0
   grad_jb = 0.0
   grad_je = 0.0
   grad_jd = 0.0
   grad_jp = 0.0
   grad_js = 0.0
   grad_jl = 0.0
   grad_jm = 0.0
   grad_jt = 0.0

   inc_div = 0.0

   jcdf_flag = .false.

   !-------------------------------------------------------------------------
   ! [1.0] calculate grad_v (jo):
   !-------------------------------------------------------------------------
   call da_allocate_y(iv, jo_grad_y)
   
   if (present(re)) then
      call da_calculate_grady(iv, re, jo_grad_y)   
      if ( iter > 0 .and. test_gradient ) jcdf_flag = .true.
#ifdef VAR4D
      call da_transform_vtoy_adj(cv_size, be, grid%ep, grad_jo, iv, &
               grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, jcdf_flag, grid%vp6, grid%vv6)
#else
      call da_transform_vtoy_adj(cv_size, be, grid%ep, grad_jo, iv, &
               grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, jcdf_flag)
#endif
   else
#ifdef VAR4D
      call da_transform_vtoy(cv_size, be, grid%ep, cv, iv, grid%vp, &
              grid%vv, xbx, y, grid, config_flags, grid%vp6, grid%vv6)
#else
      call da_transform_vtoy(cv_size, be, grid%ep, cv, iv, grid%vp, &
              grid%vv, xbx, y, grid, config_flags)
#endif
      call da_calculate_grady(iv, y, jo_grad_y)   
#ifdef VAR4D
      call da_transform_vtoy_adj(cv_size, be, grid%ep, grad_jo, iv, &
              grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, .true., grid%vp6, grid%vv6)
#else
      call da_transform_vtoy_adj(cv_size, be, grid%ep, grad_jo, iv, &
              grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, .true.)
#endif
      grad_jo = - grad_jo    !! Compensate for sign in calculation of grad_v (Jo)
   end if
      
   call da_deallocate_y(jo_grad_y)

   !-------------------------------------------------------------------------
   ! [2.0] calculate grad_v (jb):
   !-------------------------------------------------------------------------
   if (cv_size_jb > 0) grad_jb(1:cv_size_jb) = jb_factor * cv(1:cv_size_jb)

   !-------------------------------------------------------------------------
   ! [3.0] calculate grad_v (je):
   !-------------------------------------------------------------------------
   if (cv_size_je > 0) grad_je(je_start:je_end) = je_factor * cv(je_start:je_end)
   
   !----------------------------------------------------------------------
   ! [3.1] calculate grad_v (jd):
   !----------------------------------------------------------------------
   if (use_wpec) then

      if (var4d) call da_error(__FILE__,__LINE__,(/'Cannot use 4dvar with dynamic constraint'/))
      if (wpec_factor <= 0) call da_error(__FILE__,__LINE__,(/'"wpec_factor" for dynamic constraint must be greater than zero'/))

      grid%xa%grad_p_x(:,:,:)=0.0
      grid%xa%grad_p_y(:,:,:)=0.0

      call da_transform_vtod_wpec(cv_size, be, grid%ep, cv, grid%vp, grid%vv, xbx, grid)

      grid%xa%grad_p_x=(grid%xa%grad_p_x)/wpec_factor
      grid%xa%grad_p_y=(grid%xa%grad_p_y)/wpec_factor

      call da_transform_vtod_wpec_adj(cv_size, be, grid%ep, grad_jd, grid%vp, grid%vv, xbx, grid)

   end if

   !-------------------------------------------------------------------------
   ! [4.0] calculate grad_v (jp):
   !-------------------------------------------------------------------------
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc .and. cv_size_jp > 0) then
      do inst = 1, iv % num_inst   
         do ichan = 1, iv%instid(inst)%nchan
            npred    = iv%instid(inst)%varbc(ichan)%npred
            if (npred <= 0) cycle               !! VarBC channels only	 
            do ipred = 1, npred
               id     = iv%instid(inst)%varbc(ichan)%index(ipred)
	       bgerr  = iv%instid(inst)%varbc(ichan)%bgerr(ipred)
	       if (bgerr > 0.0) &
                  grad_jp(id) = (1/sqrt(bgerr)) * &
                     SUM(cv(id) * iv%instid(inst)%varbc(ichan)%vtox(ipred,1:npred))
	    end do
         end do
      end do
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
            grad_js(iv%instid(inst)%cv_index(n)%ts) = cv(iv%instid(inst)%cv_index(n)%ts)
	    
         ! Cloud cover(s)
         !---------------
	    if (use_satcv(2)) then
	    grad_js(iv%instid(inst)%cv_index(n)%cc) = cv(iv%instid(inst)%cv_index(n)%cc)

	    WHERE (cv(iv%instid(inst)%cv_index(n)%cc) < 0.0 .or.                                &
	           cv(iv%instid(inst)%cv_index(n)%cc) > 1.0 )                                   &
	    grad_js(iv%instid(inst)%cv_index(n)%cc) = grad_js(iv%instid(inst)%cv_index(n)%cc) + &
                                                       10.0 * cv(iv%instid(inst)%cv_index(n)%cc)
            end if
	 end do
      end do	      
   end if

   !-------------------------------------------------------------------------
   ! [6.0] calculate grad_v (jl):
   !-------------------------------------------------------------------------
   if (cv_size_jl > 0) grad_jl(jl_start:jl_end) = cv(jl_start:jl_end)

   !-------------------------------------------------------------------------
   ! [6.1] calculate grad_v (jm):
   !-------------------------------------------------------------------------
   if (use_divc) then
      call da_transform_vtox(grid, cv_size, xbx, be, grid%ep, cv, grid%vv, grid%vp)
      if ( be%ne > 0 .and. alphacv_method == alphacv_method_xa ) then
         call da_transform_vpatox(grid, be, grid%ep, grid%vp)
         call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
      end if
      call da_transform_xtoxa(grid)
      call da_divergence_constraint(grid%xb, grid%xa%u, grid%xa%v, inc_div)
      inc_div = inc_div/divc_factor
      call da_zero_x(grid%xa)
      call da_divergence_constraint_adj(grid, grid%xa%u, grid%xa%v, inc_div)
      call da_transform_xtoxa_adj(grid)
      if (be % ne > 0 .and. alphacv_method == alphacv_method_xa) then
         call da_transform_vpatox_adj(grid, be, grid%ep, grid%vp)
      end if
      call da_zero_vp_type(grid%vp)
      call da_transform_vtox_adj(grid, cv_size, xbx, be, grid%ep, grid%vp, grid%vv, grad_jm)
   end if

   !-------------------------------------------------------------------------
   ! [n.n] calculate grad_t (jt):
   !-------------------------------------------------------------------------

   if (cv_size_jt > 0) then
       do n = 1, iv%varbc_tamdar%nair
          do iphase = 1, iv%varbc_tamdar%nphase
             if (iv%varbc_tamdar%nobs(iphase,n) > 0 .and. iv%varbc_tamdar%ifuse(iphase,n) > 0) then
                 do ipred = 1, iv%varbc_tamdar%npred
                    id     = iv%varbc_tamdar%index(ipred,iphase,n)
                    bgerr  = iv%varbc_tamdar%bgerr(ipred,iphase,n)
                    if (bgerr > 0.0) &
                        grad_jt(id) = (1/sqrt(bgerr)) * SUM(cv(id) * &
                                iv%varbc_tamdar%vtox(ipred,1:iv%varbc_tamdar%npred,iphase,n))
                 end do
             end if
          end do
       end do
   end if

   !--------------------------------------------------------------------------------------------------
   ! [7.0] calculate grad_v (j) = grad_v (jb) + grad_v (jo) + grad_v (je) + grad_v (jd) + grad_v (jp) + grad_v (js) + grad_v (jl)
   !--------------------------------------------------------------------------------------------------   
   grad = grad_jo + grad_jb + grad_je + grad_jd + grad_jp + grad_js + grad_jl + grad_jm + grad_jt

   !-------------------------------------------------------------------------
   ! [8.0] write Gradient:
   !-------------------------------------------------------------------------
   if (rootproc) then
      if (it == 1 .and. iter == 0) then
         write(unit=grad_unit,fmt='(a)')'Outer    EPS     Inner      G           Gb       Go           Ge         Gd         Gp         Gs        Gl         Gm         Gt'
         write(unit=grad_unit,fmt='(a)')'Iter             Iter                            '
      end if
   end if

   if ( present(re) ) then
      gnorm_j  = sqrt(da_dot_cv(cv_size, grad,    grad,    grid, be%cv_mz, be%ncv_mz, jp_start, jp_end))
      gnorm_jo = sqrt(da_dot_cv(cv_size, grad_jo, grad_jo, grid, be%cv_mz, be%ncv_mz))
      gnorm_jb = sqrt(da_dot_cv(cv_size, grad_jb, grad_jb, grid, be%cv_mz, be%ncv_mz))
      gnorm_je = sqrt(da_dot_cv(cv_size, grad_je, grad_je, grid, be%cv_mz, be%ncv_mz))
      gnorm_jd = sqrt(da_dot_cv(cv_size, grad_jd, grad_jd, grid, be%cv_mz, be%ncv_mz))
      gnorm_jp = sqrt(da_dot_cv(cv_size, grad_jp, grad_jp, grid, be%cv_mz, be%ncv_mz, jp_start, jp_end))
      gnorm_js = sqrt(da_dot_cv(cv_size, grad_js, grad_js, grid, be%cv_mz, be%ncv_mz))
      gnorm_jl = sqrt(da_dot_cv(cv_size, grad_jl, grad_jl, grid, be%cv_mz, be%ncv_mz))
      gnorm_jm = sqrt(da_dot_cv(cv_size, grad_jm, grad_jm, grid, be%cv_mz, be%ncv_mz))
      gnorm_jt = sqrt(da_dot_cv(cv_size, grad_jt, grad_jt, grid, be%cv_mz, be%ncv_mz))

      if (rootproc) &
         write(grad_unit,fmt='(2x,i2,1x,e10.3,2x,i4,10(1x,f10.3))') &
               it, eps(it), iter, gnorm_j, gnorm_jb, gnorm_jo, gnorm_je, gnorm_jd, gnorm_jp, gnorm_js, gnorm_jl, gnorm_jm, gnorm_jt
   end if

   if (trace_use) call da_trace_exit("da_calculate_gradj")

end subroutine da_calculate_gradj
