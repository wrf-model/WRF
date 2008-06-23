subroutine da_calculate_j(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                           xbx, be, iv, xhat, cv, &
                           re, y, j, j_grad,            &
                           grid, config_flags                     )

   !---------------------------------------------------------------------------
   ! Purpose: Initialises the Y-array
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: it     ! external iteration #.
   integer, intent(in)                :: iter   ! internal iteration #.
   integer, intent(in)                :: cv_size    ! Total cv size.
   integer, intent(in)                :: cv_size_jb ! Jb cv size.
   integer, intent(in)                :: cv_size_je ! Je cv size.
   integer, intent(in)                :: cv_size_jp ! Jp cv size.
   type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   type (y_type) , intent(inout)      :: re     ! residual vector (o-a).
   type (y_type) , intent(inout)      :: y      ! y = H(x_inc).
   real, intent(out)                  :: j_grad(1:cv_size) ! control variables.
   type (j_type) , intent(out)        :: j      ! cost function j

   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   real             :: jo_partial                   ! jo for this processor
   real             :: gnorm_jb, gnorm_jo, gnorm_j, gnorm_je 
   type (y_type)    :: jo_grad_y ! Grad_y(jo)
   real             :: cv_jb(1:cv_size_jb)          ! Jb control variable.
   real             :: xhat_jb(1:cv_size_jb)        ! Jb control variable.
   real             :: cv_je(1:cv_size_je)          ! Je control variable.
   real             :: xhat_je(1:cv_size_je)        ! Je control variable.
   real, allocatable, dimension(:) :: cv_xhat_jb, cv_xhat_je
   integer          :: ndynopt, is, ie, js, je, ks, ke
   real             :: dtemp1x

   ! Variables for VarBC background constraint
   real , allocatable, dimension(:)  :: cv_xhat_jp ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr, gnorm_jp
 
   if (trace_use) call da_trace_entry("da_calculate_j")

   allocate(cv_xhat_jb(cv_size_jb))
   allocate(cv_xhat_je(cv_size_je))
   allocate(cv_xhat_jp(cv_size_jp))


   je_start = cv_size_jb + 1
   je_end = cv_size_jb + cv_size_je

   call da_allocate_y(iv, jo_grad_y)

   !-------------------------------------------------------------------------
   ! [1.0] calculate jo:
   !-------------------------------------------------------------------------

   ! [1.1] transform from control variable to model grid space:

   if (iter > 0) &
      call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, grid%vv,&
                              xbx, y, &
                              grid, config_flags                      )

   ! [1.2] compute residual (o-a) = (o-b) - h x~

   call da_calculate_residual(iv, y, re)

   ! [1.3] calculate jo:

   call da_jo_and_grady(iv, re, jo_partial, j % jo, jo_grad_y)

   if (test_dm_exact) then
      ! jo_partial has been already summed at lower level
      j % jo % total = jo_partial
   else
      j % jo % total = wrf_dm_sum_real(jo_partial)
   end if

   ! [1.4] calculate jc-dfi:

   j % jc = 0.0

   if ( var4d .and. grid%jcdfi_io .and. iter > 0 ) then

      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt (1 , DYN_EM_TL)

      call da_med_initialdata_input(grid , config_flags, 'tldf')

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt (1 , DYN_EM)

      is = grid%xp%ips
      ie = grid%xp%ipe
      if ( ie == grid%xp%ide ) ie = ie + 1
      js = grid%xp%jps
      je = grid%xp%jpe
      if ( je == grid%xp%jde ) je = je + 1
      ks = grid%xp%kps
      ke = grid%xp%kpe
      
      j % jc = j % jc + 0.5*grid%jcdfi_gama*da_dot( (ie-is+1)*(je-js+1)*(ke-ks+1), grid%g_u_2(is:ie,js:je,ks:ke), grid%g_u_2(is:ie,js:je,ks:ke) )
      j % jc = j % jc + 0.5*grid%jcdfi_gama*da_dot( (ie-is+1)*(je-js+1)*(ke-ks+1), grid%g_v_2(is:ie,js:je,ks:ke), grid%g_v_2(is:ie,js:je,ks:ke) )
      j % jc = j % jc + 0.5*grid%jcdfi_gama*da_dot( (ie-is+1)*(je-js+1)*(ke-ks+1), grid%g_t_2(is:ie,js:je,ks:ke), grid%g_t_2(is:ie,js:je,ks:ke) )
      j % jc = j % jc + 0.5*grid%jcdfi_gama*da_dot( (ie-is+1)*(je-js+1), grid%g_mu_2(is:ie,js:je), grid%g_mu_2(is:ie,js:je) )
      j % jc = j % jc + 0.5*grid%jcdfi_gama*da_dot( (ie-is+1)*(je-js+1)*(ke-ks+1), grid%g_moist(is:ie,js:je,ks:ke,P_G_QV), grid%g_moist(is:ie,js:je,ks:ke,P_G_QV) )

      dtemp1x = j % jc
      ! summation across processors:
      j % jc  = wrf_dm_sum_real(dtemp1x)

   end if

   !-------------------------------------------------------------------------
   ! [2.0] calculate jb:
   !-------------------------------------------------------------------------

   cv_jb(1:cv_size_jb) = cv (1:cv_size_jb)
   xhat_jb(1:cv_size_jb) = xhat (1:cv_size_jb)

   cv_xhat_jb(1:cv_size_jb) = cv_jb(1:cv_size_jb) + xhat_jb(1:cv_size_jb)

   j % jb = 0.5 * da_dot_cv(cv_size_jb, cv_size_domain_jb, &
       cv_xhat_jb, cv_xhat_jb, grid, &
       (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /))

   j % jb = jb_factor * j % jb

   !-------------------------------------------------------------------------
   ! [3.0] calculate je:
   !-------------------------------------------------------------------------

   j % je = 0.0
   if (be % ne > 0) then
      cv_je(1:cv_size_je) = cv(je_start:je_end)
      xhat_je(1:cv_size_je) = xhat(je_start:je_end)
      cv_xhat_je(1:cv_size_je) = cv_je(1:cv_size_je) + xhat_je(1:cv_size_je)
      j % je = 0.5 * da_dot_cv(cv_size_je, cv_size_domain_je, &
          cv_xhat_je, cv_xhat_je, grid, &
          (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /))
   end if
   j % je = je_factor * j % je

   !-------------------------------------------------------------------------
   ! [4.0] calculate jp:
   !-------------------------------------------------------------------------
   j % jp = 0.0
   if (use_varbc) then
      jp_start                 = cv_size_jb + cv_size_je + 1
      jp_end                   = cv_size_jb + cv_size_je + cv_size_jp
      cv_xhat_jp(1:cv_size_jp) = 0.0
      do inst = 1, iv % num_inst   
         do ichan = 1, iv%instid(inst)%nchan
            npred    = iv%instid(inst)%varbc(ichan)%npred
            if (npred <= 0) cycle               !! VarBC channels only	 
            do ipred = 1, npred
               id     = iv%instid(inst)%varbc(ichan)%index(ipred)
	       bgerr  = iv%instid(inst)%varbc(ichan)%bgerr(ipred)
	       if (bgerr > 0.0) &
    	          cv_xhat_jp(id-jp_start+1) = (1/sqrt(bgerr)) * &
	             SUM( (cv(id)+xhat(id)) * &
		     iv%instid(inst)%varbc(ichan)%vtox(ipred,1:npred))            
	    end do
         end do
      end do
      j % jp = 0.5 * da_dot(cv_size_jp, cv_xhat_jp, cv_xhat_jp)
   end if

   !-------------------------------------------------------------------------
   ! [5.0] calculate total cost function j = jo + jb + jc + je + jp:
   !-------------------------------------------------------------------------

   j % total = j % jb + j % jo % total + j % je + j % jp
   if (grid%jcdfi_use) j % total = j % total  + j % jc

   if (it == 1 .and. iter == 0 .and. rootproc) then
      write(unit=cost_unit,fmt='(a)')'Outer    EPS     Inner      J           Jb       Jo           Jc         Je         Jp'
      write(unit=cost_unit,fmt='(a)')'Iter             Iter                            '
      write(unit=grad_unit,fmt='(a)')'Outer    EPS     Inner      G           Gb       Go           Ge           Gp'
      write(unit=grad_unit,fmt='(a)')'Iter             Iter                            '
   end if

   if (rootproc) then
      write(unit=cost_unit,fmt='(2x,i2,1x,e10.3,2x,i4,6(1x,f10.3))') &
         it, EPS(it), iter, j % total, j % jb, j % jo % total, j % jc, j % je, j % jp
   end if

   !-------------------------------------------------------------------------
   ! [6.0] calculate grad_v (jo):
   !-------------------------------------------------------------------------

   call da_transform_vtoy_adj(iter, cv_size, be, grid%ep, j_grad, iv, &
      grid%vp, grid%vv, xbx, jo_grad_y, &
      grid, config_flags, .false.)

   call da_deallocate_y(jo_grad_y)

   if (print_detail_grad) then
      write(unit=stdout, fmt='(a,i6)') 'Calculate grad_v(jo) iter=', iter
      write(unit=stdout, fmt='(a, e24.14)') &
         '   cv_jb.cv_jb = ',sum(cv_jb(:) * cv_jb(:))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   cv_je.cv_je = ',sum(cv_je(:) * cv_je(:))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   xhat.xhat = ',sum(xhat(1:cv_size) * xhat(1:cv_size))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   j_grad.j_grad = ',sum(j_grad(1:cv_size) * j_grad(1:cv_size))
   end if

   !------------------------------------------------------------------------------------
   ! [7.0] calculate grad_v (j) = grad_v (jb) + grad_v (jo) + grad_v (je) + grad_v (jp):
   !------------------------------------------------------------------------------------

   gnorm_jo = da_dot_cv(cv_size, cv_size_domain, j_grad, j_grad, grid, &
      (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /), &
      use_varbc)

   ! Add Del_v (Jb)
   j_grad(1:cv_size_jb) =  jb_factor * &
                           (cv(1:cv_size_jb) + xhat(1:cv_size_jb)) + &
                           j_grad(1:cv_size_jb)

   ! Add Del_v (Je)
   if (cv_size_je > 0) then
      j_grad(je_start:je_end) = je_factor * &
                                (cv(je_start:je_end) + xhat(je_start:je_end)) + &
                                j_grad(je_start:je_end)
   end if
   
   ! Add Del_v (Jp)
   if (use_varbc) then
      j_grad(jp_start:jp_end) = j_grad(jp_start:jp_end) + cv_xhat_jp(1:cv_size_jp)
   end if
      
   cv_xhat_jb(1:cv_size_jb) = cv_jb(1:cv_size_jb) + xhat_jb(1:cv_size_jb)
   gnorm_jb = da_dot_cv(cv_size_jb, cv_size_domain_jb, &
                         cv_xhat_jb, cv_xhat_jb, grid, &
                         (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /))
   cv_xhat_je(1:cv_size_je) = cv_je(1:cv_size_je) + xhat_je(1:cv_size_je)
   gnorm_je = da_dot_cv(cv_size_je, cv_size_domain_je, &
                         cv_xhat_je, cv_xhat_je, grid, &
                         (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /))
   gnorm_jp = da_dot_cv(cv_size_jp, cv_size_domain_jp, &
                         cv_xhat_jp, cv_xhat_jp, grid, &
                         (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /))

   if (print_detail_grad) then
      write(unit=stdout, fmt='(a,i6)') 'Calculate grad_v(j) iter=', iter
      write(unit=stdout, fmt='(a, e24.14)') &
         '   cv_jb.cv_jb = ', sum(cv_jb(1:cv_size_jb) * cv_jb(1:cv_size_jb))
      write(unit=stdout, fmt='(a, e24.14)') &
         '    cv_je.cv_je = ', sum(cv_je(1:cv_size_je) * cv_je(1:cv_size_je))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   xhat_jb.xhat_jb = ', sum(xhat_jb(1:cv_size_jb) * xhat_jb(1:cv_size_jb))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   xhat_je.xhat_je = ', sum(xhat_je(1:cv_size_je) * xhat_je(1:cv_size_je))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   cv_xhat_jp.cv_xhat_jp = ', sum(cv_xhat_jp(1:cv_size_jp) * cv_xhat_jp(1:cv_size_jp))
      write(unit=stdout, fmt='(a, e24.14)') &
         '   j_grad.j_grad = ',sum(j_grad(1:cv_size) * j_grad(1:cv_size))
      write (unit=stdout,fmt='(A)') " " 
   end if

   gnorm_j = da_dot_cv(cv_size, cv_size_domain, j_grad, j_grad, grid, &
                        (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /),&
			use_varbc)

   gnorm_jo= sqrt(gnorm_jo)
   gnorm_jb= jb_factor * sqrt(gnorm_jb)
   gnorm_je= je_factor * sqrt(gnorm_je)
   gnorm_jp= sqrt(gnorm_jp)
   gnorm_j = sqrt(gnorm_j)

   if (rootproc) then
      write(grad_unit,fmt='(2x,i2,1x,e10.3,2x,i4,6(1x,f10.3))') & 
         it, eps(it), iter, gnorm_j, gnorm_jb, gnorm_jo, gnorm_je, gnorm_jp
   end if

   deallocate(cv_xhat_jb)
   deallocate(cv_xhat_je)
   deallocate(cv_xhat_jp)

   if (trace_use) call da_trace_exit("da_calculate_j")

end subroutine da_calculate_j


