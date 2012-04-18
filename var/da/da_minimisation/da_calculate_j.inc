subroutine da_calculate_j(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                           cv_size_jl, xbx, be, iv, xhat, cv, &
                           re, y, j, grad, grid, config_flags                     )

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
   integer, intent(in)                :: cv_size_jl ! Jl cv size.
   type (xbx_type),intent(inout)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(in)                   :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   type (y_type) , intent(inout)      :: re     ! residual vector (o-a).
   type (y_type) , intent(inout)      :: y      ! y = H(x_inc).
   type (j_type) , intent(out)        :: j      ! cost function j
   real, intent(out)                  :: grad(cv_size)        ! gradient of cost function

   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   integer          :: jl_start, jl_end             ! Start/end indices of Je.
   real             :: jo_partial                   ! jo for this processor
   type (y_type)    :: jo_grad_y ! Grad_y(jo)
   real             :: cv_xhat_jb(cv_size_jb), cv_xhat_je(cv_size_je), cv_xhat_jl(cv_size_jl)
#ifdef CLOUD_CV
   integer          :: mz(11)
#else
   integer          :: mz(7)
#endif
   integer          :: ndynopt
   real             :: dtemp1x
   integer          :: i, jj, k
   real             :: subarea, whole_area

   ! Variables for VarBC background constraint
   real                              :: cv_xhat_jp(cv_size_jp) ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr, gnorm_jp  
    
   integer                           :: n, cldtoplevel(1), icld, nclouds, ncv, minlev_cld
   real                              :: js_local
   real, allocatable                 :: cc(:)
   
   if (trace_use) call da_trace_entry("da_calculate_j")

   !-------------------------------------------------------------------------
   ! [0.0] initialization:
   !-------------------------------------------------------------------------
#ifdef CLOUD_CV
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%v6%mz, be%v7%mz,be%v8%mz, be%v9%mz, be%alpha%mz, be % ne /)
#else
   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz, be % ne /)
#endif
   je_start = cv_size_jb + 1
   je_end   = cv_size_jb + cv_size_je
   jp_start = cv_size_jb + cv_size_je + 1
   jp_end   = cv_size_jb + cv_size_je + cv_size_jp
   jl_start = cv_size_jb + cv_size_je + cv_size_jp + 1
   jl_end =   cv_size_jb + cv_size_je + cv_size_jp + cv_size_jl

   call da_allocate_y(iv, jo_grad_y)

   !-------------------------------------------------------------------------
   ! [1.0] calculate jo:
   !-------------------------------------------------------------------------

   ! [1.1] transform from control variable to model grid space:

   if (iter > 0) &
      call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, grid%vv,&
                              grid%vp6, grid%vv6, xbx, y, &
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
      j % jb = jb_factor * 0.5 * da_dot_cv(cv_size_jb,  cv_xhat_jb, cv_xhat_jb, grid, mz)
   end if

   !-------------------------------------------------------------------------
   ! [3.0] calculate je:
   !-------------------------------------------------------------------------

   j % je = 0.0
   if (be % ne > 0) then
      cv_xhat_je(1:cv_size_je) = cv(je_start:je_end) + xhat(je_start:je_end)
      j % je = je_factor * 0.5 * da_dot_cv(cv_size_je, cv_xhat_je, cv_xhat_je, grid, mz)
   end if

   !-------------------------------------------------------------------------
   ! [4.0] calculate jl:
   !-------------------------------------------------------------------------
   j % jl = 0.0
   if ( var4d ) then
      cv_xhat_jl(1:cv_size_jl) = cv (jl_start:jl_end) + xhat(jl_start:jl_end)

      j % jl = 0.5 * da_dot_cv(cv_size_jl, cv_xhat_jl, cv_xhat_jl, grid, mz)

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
   ! [7.0] calculate total cost function j = jo + jb + jc + je + jp + js:
   !-------------------------------------------------------------------------

   j % total = j % jb + j % jo % total + j % je + j % jp + j % js
   if (grid%jcdfi_use) j % total = j % total  + j % jc
   if (var4d) j % total = j % total  + j % jl

   !-------------------------------------------------------------------------
   ! [8.0] write cost function:
   !-------------------------------------------------------------------------
   if (rootproc) then
      if (it == 1 .and. iter == 0) then
         write(unit=cost_unit,fmt='(a)')'Outer    EPS     Inner      J           Jb       Jo           Jc         Je         Jp         Js        jl'
         write(unit=cost_unit,fmt='(a)')'Iter             Iter                            '
         write(unit=grad_unit,fmt='(a)')'Outer    EPS     Inner      G           Gb       Go           Ge           Gp           Gs        Gl'
         write(unit=grad_unit,fmt='(a)')'Iter             Iter                            '
      end if

      write(unit=cost_unit,fmt='(2x,i2,1x,e10.3,2x,i4,8(1x,f10.3))') &
         it, EPS(it), iter, j % total, j % jb, j % jo % total, j % jc, j % je, j % jp, j%js, j%jl
   end if
         
   !-------------------------------------------------------------------------
   ! [8.0] Calculate Gradient:
   !-------------------------------------------------------------------------
   call da_calculate_gradj(it,iter,cv_size,cv_size_jb,cv_size_je,cv_size_jp, &
                           cv_size_jl,xbx, be, iv, xhat+cv, y, grad, grid, config_flags, re)

   call da_deallocate_y (jo_grad_y)
if (trace_use) call da_trace_exit("da_calculate_j")

end subroutine da_calculate_j

