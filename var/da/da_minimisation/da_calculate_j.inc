subroutine da_calculate_j(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                           cv_size_jl, cv_size_jt, xbx, be, iv, xhat, cv, &
                           re, y, j, grid, config_flags                     )

   !---------------------------------------------------------------------------
   ! Purpose: Calculate Cost Function
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: it     ! external iteration #.
   integer, intent(in)                :: iter   ! internal iteration #.
   integer, intent(in)                :: cv_size    ! Total cv size.
   integer, intent(in)                :: cv_size_jb ! Jb cv size.
   integer, intent(in)                :: cv_size_je ! Je cv size.
   integer, intent(in)                :: cv_size_jp ! Jp cv size.
   integer, intent(in)                :: cv_size_jl ! Jl cv size.
   integer, intent(in)                :: cv_size_jt ! Jt cv size.
   type (xbx_type),intent(inout)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   type (y_type) , intent(inout)      :: re     ! residual vector (o-a).
   type (y_type) , intent(inout)      :: y      ! y = H(x_inc).
   type (j_type) , intent(out)        :: j      ! cost function j

   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   integer          :: jl_start, jl_end             ! Start/end indices of Je.
   integer          :: jt_start, jt_end, iphase
   real             :: jo_partial                   ! jo for this processor
   type (y_type)    :: jo_grad_y ! Grad_y(jo)
   real             :: cv_xhat_jb(cv_size_jb), cv_xhat_je(cv_size_je), cv_xhat_jl(cv_size_jl), &
                       cv_xhat_jt(cv_size_jt)
   integer          :: ndynopt
   real             :: dtemp1x
   integer          :: i, jj, k
   real             :: subarea, whole_area

   ! Variables for VarBC background constraint
   real                              :: cv_xhat_jp(cv_size_jp) ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr, gnorm_jp  
   real                              :: gnorm_jt
    
   integer                           :: n, cldtoplevel(1), icld, nclouds, ncv, minlev_cld
   real                              :: jd_local
   real                              :: js_local
   real                              :: jm_local
   real                              :: jt_local
   real, allocatable                 :: cc(:)

   real                              :: inc_div(ims:ime,jms:jme,kms:kme)

   if (trace_use) call da_trace_entry("da_calculate_j")

   !-------------------------------------------------------------------------
   ! [0.0] initialization:
   !-------------------------------------------------------------------------
   je_start = cv_size_jb + 1
   je_end   = cv_size_jb + cv_size_je
   jp_start = cv_size_jb + cv_size_je + 1
   jp_end   = cv_size_jb + cv_size_je + cv_size_jp
   jl_start = cv_size_jb + cv_size_je + cv_size_jp + 1
   jl_end   = cv_size_jb + cv_size_je + cv_size_jp + cv_size_jl
   jt_start = jl_end + 1
   jt_end   = jl_end + cv_size_jt

   inc_div = 0.0

   call da_allocate_y(iv, jo_grad_y)

   !-------------------------------------------------------------------------
   ! [1.0] calculate jo:
   !-------------------------------------------------------------------------

   ! [1.1] transform from control variable to model grid space:

   if (iter > 0) then
#ifdef VAR4D
      call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, grid%vv,&
                              xbx, y,                                         &
                              grid, config_flags, grid%vp6, grid%vv6)
#else
      call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, grid%vv,&
                              xbx, y,                                         &
                              grid, config_flags)
#endif
   end if

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

   if ( var4d .and. (grid%jcdfi_use .or. grid%jcdfi_diag == 1) .and. iter > 0 ) then

#ifdef VAR4D

      subarea = SUM ( grid%xb%grid_box_area(its:ite,jts:jte) )
      whole_area = wrf_dm_sum_real(subarea)

      ! Multipled by -1.0 because the dnw is negative

      do jj = jms, jme
         do k = kms, kme
            do i = ims, ime
               j%jc = j%jc - 0.5 * config_flags%jcdfi_penalty * model_grid%jcdfi_u(i,k,jj)**2 * &
                     grid%xb%grid_box_area(i,jj)/whole_area*grid%xb%dnw(k)
               j%jc = j%jc - 0.5 * config_flags%jcdfi_penalty * model_grid%jcdfi_v(i,k,jj)**2 * &
                     grid%xb%grid_box_area(i,jj)/whole_area*grid%xb%dnw(k)
               j%jc = j%jc - 0.5 * config_flags%jcdfi_penalty * model_grid%jcdfi_t(i,k,jj)**2 * &
                     (9.81/3.0)**2*grid%xb%grid_box_area(i,jj)/whole_area*grid%xb%dnw(k)
               j%jc = j%jc - 0.5 * config_flags%jcdfi_penalty * model_grid%jcdfi_p(i,k,jj)**2 * &
                     (1.0/300.)**2*grid%xb%grid_box_area(i,jj)/whole_area*grid%xb%dnw(k)
            enddo
         enddo
      enddo

      dtemp1x = j % jc
      ! summation across processors:
      j % jc  = wrf_dm_sum_real(dtemp1x)

#endif
   end if

   !-------------------------------------------------------------------------
   ! [2.0] calculate jb:
   !-------------------------------------------------------------------------

   j % jb = 0.0
   if (cv_size_jb > 0) then
      cv_xhat_jb(1:cv_size_jb) = cv(1:cv_size_jb) + xhat(1:cv_size_jb)
      j % jb = jb_factor * 0.5 * da_dot_cv(cv_size_jb,  cv_xhat_jb, cv_xhat_jb, grid, be%cv_mz, be%ncv_mz)
   end if

   !-------------------------------------------------------------------------
   ! [3.0] calculate je:
   !-------------------------------------------------------------------------

   j % je = 0.0
   if (be % ne > 0) then
      cv_xhat_je(1:cv_size_je) = cv(je_start:je_end) + xhat(je_start:je_end)
      j % je = je_factor * 0.5 * da_dot_cv(cv_size_je, cv_xhat_je, cv_xhat_je, grid, be%cv_mz, be%ncv_mz)
   end if


   !----------------------------------------------------------------------
   ![1.0.1] calculate grad_v (jd):
   !----------------------------------------------------------------------

   j % jd = 0.0

   if (use_wpec) then

      if (var4d) call da_error(__FILE__,__LINE__,(/'Cannot use 4dvar with dynamic constraint'/))
      if (wpec_factor <= 0) call da_error(__FILE__,__LINE__,(/'"wpec_factor" for dynamic constraint must be greater than zero'/))

      grid%xa%grad_p_x(:,:,:)=0.0
      grid%xa%grad_p_y(:,:,:)=0.0

      !hcl why xhat+cv
      call da_transform_vtod_wpec(cv_size, be, grid%ep, xhat+cv, grid%vp, grid%vv, xbx, grid)

      do i=its,ite
         do jj=jts,jte
            do k=kts,kte
               j % jd = j % jd + 0.5*(grid%xa%grad_p_x(i,jj,k)**2+grid%xa%grad_p_y(i,jj,k)**2)/wpec_factor
            end do
         end do
      end do

      jd_local = j % jd
      ! summation across processors:
      j % jd  = wrf_dm_sum_real(jd_local)

   end if

   !-------------------------------------------------------------------------
   ! [4.0] calculate jl:
   !-------------------------------------------------------------------------
   j % jl = 0.0
   if ( var4d ) then
      cv_xhat_jl(1:cv_size_jl) = cv (jl_start:jl_end) + xhat(jl_start:jl_end)

      j % jl = 0.5 * da_dot_cv(cv_size_jl, cv_xhat_jl, cv_xhat_jl, grid, be%cv_mz, be%ncv_mz)

   endif

   !-------------------------------------------------------------------------
   ! [5.0] calculate jp:
   !-------------------------------------------------------------------------
   j % jp = 0.0
#if defined(RTTOV) || defined(CRTM)
   if (use_varbc .and. cv_size_jp > 0) then
      cv_xhat_jp = 0.0
      do inst = 1, iv % num_inst   
         do ichan = 1, iv%instid(inst)%nchan
            npred    = iv%instid(inst)%varbc(ichan)%npred
            if (npred <= 0) cycle               !! VarBC channels only	 
            do ipred = 1, npred
               id     = iv%instid(inst)%varbc(ichan)%index(ipred)
	       bgerr  = iv%instid(inst)%varbc(ichan)%bgerr(ipred)
	       if (bgerr > 0.0) &
    	          cv_xhat_jp(id-jp_start+1) = (1/sqrt(bgerr)) * &
	             SUM((cv(id)+xhat(id)) * iv%instid(inst)%varbc(ichan)%vtox(ipred,1:npred))            
	    end do
         end do
      end do
      j % jp = 0.5 * da_dot(cv_size_jp, cv_xhat_jp, cv_xhat_jp)
   end if
#endif

   !-------------------------------------------------------------------------
   ! [6.0] calculate js:
   !-------------------------------------------------------------------------
   j % js = 0.0
   if (ANY(use_satcv)) then
      do inst = 1, iv % num_inst   
         do n = iv%instid(inst)%info%n1, iv%instid(inst)%info%n2 ! loop for pixel
         ! Skin Temperature
         !-----------------
	    if (use_satcv(1)) then
               j % js = j % js + 0.5 * xhat(iv%instid(inst)%cv_index(n)%ts) **2
	       
!	       !!! Super-TMP dump of Tskin increment for plotting purposes
!               if (iter > 0) iv%instid(inst)%tb_xb(1,n)  = xhat(iv%instid(inst)%cv_index(n)%ts) 
	    end if	 
	    
         ! Cloud cover(s)
         !---------------
	    if (use_satcv(2)) then
	    j % js = j % js + 0.5 * SUM( xhat(iv%instid(inst)%cv_index(n)%cc) **2)

	    j % js = j % js + 0.5 * SUM( (10.0 * xhat(iv%instid(inst)%cv_index(n)%cc)) **2,      &
	                                  MASK = xhat(iv%instid(inst)%cv_index(n)%cc) < 0.0 .or. &
				                 xhat(iv%instid(inst)%cv_index(n)%cc) > 1.0 )

	       if (iter > 0) then
	          nclouds = iv%instid(inst)%cv_index(n)%nclouds
     	          ncv     = iv%instid(inst)%cv_index(n)%ncv
		  allocate(cc(nclouds))

		  cc = xhat(iv%instid(inst)%cv_index(n)%cc)
	       !---------------------------------------------------------------
               ! Change of variable (preconditioning) 
               !---------------------------------------------------------------
!		  do icld = 1, nclouds
!    	             cc(icld) = SUM( xhat(iv%instid(inst)%cv_index(n)%cc) * &
!	                                iv%instid(inst)%cv_index(n)%vtox(icld,1:ncv) )
!	          end do
		  
	          if (use_satcv(1)) then
		     write (*, '(i6,100F8.2)')n,xhat(iv%instid(inst)%cv_index(n)%ts), SUM(cc)*100, cc*100
		  else
		     write (*, '(i6,100F8.2)')n,SUM(cc)*100, cc*100						  
                  end if
		  
!		  !!! Super-TMP dump of Cloud Cover increment for plotting purposes	 
!                  iv%instid(inst)%tb_inv(1,n) = SUM(cc)*100.0 
!                  
!		  !!! Super-TMP dump of Cloud Top Pressure for plotting purposes
!		  minlev_cld = 5
!		  if (ANY(cc(minlev_cld:nclouds) > 0.01)) then
!		     cldtoplevel = MINLOC(cc(minlev_cld:nclouds), MASK = cc(minlev_cld:nclouds) > 0.01)
!		  else
!		     cldtoplevel = nclouds
!		  end if   
!		  cldtoplevel = cldtoplevel + kte - nclouds !!!+ minlev_cld
!!                  if (rtm_option == rtm_option_rttov) then
!!                     re%instid(inst)%tb(1,n) = coefs(inst)%ref_prfl_p(cldtoplevel(1))
!!                  elseif (rtm_option == rtm_option_crtm) then
!                     re%instid(inst)%tb(1,n) = iv%instid(inst)%pm(cldtoplevel(1),n)
!!                  end if  	    
		  
		  deallocate(cc)
	       end if    
	    end if
	 end do
      end do	      
      js_local = j % js
      ! summation across processors:
      j % js = wrf_dm_sum_real(js_local)
   end if

   !-------------------------------------------------------------------------
   ! [6.1] calculate jm:
   !-------------------------------------------------------------------------
   j % jm = 0.0
   if (use_divc) then
      call da_transform_vtox(grid, cv_size, xbx, be, grid%ep, xhat+cv, grid%vv, grid%vp)
      if ( be%ne > 0 .and. alphacv_method == alphacv_method_xa ) then
         call da_transform_vpatox(grid, be, grid%ep, grid%vp)
         call da_add_xa(grid%xa, grid%xa_ens) !grid%xa = grid%xa + xa_ens
      end if
      call da_transform_xtoxa(grid)
      call da_divergence_constraint(grid%xb, grid%xa%u, grid%xa%v, inc_div)
      j % jm   = 0.5* SUM(inc_div(its:ite, jts:jte, kts:kte)* &
                          inc_div(its:ite, jts:jte, kts:kte))/divc_factor
      jm_local = j % jm
      ! summation across processors:
      j % jm  = wrf_dm_sum_real(jm_local)
   end if

   !-------------------------------------------------------------------------
   ! [n.n] calculate jt:
   !-------------------------------------------------------------------------

   j % jt = 0.0
   jt_local = 0.0
   if (cv_size_jt > 0) then
       do n = 1, iv%varbc_tamdar%nair
          do iphase = 1, iv%varbc_tamdar%nphase
             if (iv%varbc_tamdar%nobs(iphase,n) > 0 .and. iv%varbc_tamdar%ifuse(iphase,n) > 0) then
                 do ipred = 1, iv%varbc_tamdar%npred
                    id     = iv%varbc_tamdar%index(ipred,iphase,n)
                    bgerr  = iv%varbc_tamdar%bgerr(ipred,iphase,n)
                    if (bgerr > 0.0) &
                        cv_xhat_jt(id-jt_start+1) = (1/sqrt(bgerr)) * &
                        SUM( (cv(id)+xhat(id)) * iv%varbc_tamdar%vtox(ipred,1:iv%varbc_tamdar%npred,iphase,n) )
                 end do
             end if
          end do
       end do
       jt_local = 0.5 * da_dot(cv_size_jt, cv_xhat_jt, cv_xhat_jt)
   end if
   j % jt  = wrf_dm_sum_real(jt_local)

   !-------------------------------------------------------------------------------
   ! [7.0] calculate total cost function j = jo + jb + jc + je + jd + jp + js + jt:
   !-------------------------------------------------------------------------------

   j % total = j % jb + j % jo % total + j % je + j % jd + j % jp + j % js + j % jm + j % jt
   if (grid%jcdfi_use) j % total = j % total  + j % jc
   if (var4d) j % total = j % total  + j % jl

   !-------------------------------------------------------------------------
   ! [8.0] write cost function:
   !-------------------------------------------------------------------------
   if (rootproc) then
      if (it == 1 .and. iter == 0) then
         write(unit=cost_unit,fmt='(a)')'Outer    EPS     Inner      J           Jb       Jo           Jc         Je         Jd         Jp         Js        Jl         Jm         Jt'
         write(unit=cost_unit,fmt='(a)')'Iter             Iter                            '
      end if

      write(unit=cost_unit,fmt='(2x,i2,1x,e10.3,2x,i4,11(1x,f10.3))') &
         it, EPS(it), iter, j % total, j % jb, j % jo % total, j % jc, j % je, j % jd, j % jp, j%js, j%jl, j%jm, j%jt
   end if

   call da_deallocate_y (jo_grad_y)
if (trace_use) call da_trace_exit("da_calculate_j")

end subroutine da_calculate_j

