subroutine da_calculate_gradj(it, iter, cv_size, cv_size_jb, cv_size_je, cv_size_jp, &
                              xbx, be, iv, y, xhat, cv, j_grad, grid, config_flags  )

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
   type (y_type), intent(inout)       :: y
   real, intent(in)                   :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   real, intent(out)                  :: j_grad(1:cv_size) ! control variables.

   type(domain), intent(inout)  :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer          :: je_start, je_end             ! Start/end indices of Je.
   real             :: jo_partial                   ! jo for this processor
   real             :: gnorm_jb, gnorm_jo, gnorm_j, gnorm_je 
   type (y_type)    :: jo_grad_y                    ! Grad_y(jo)
   integer          :: mz(6)

   ! Variables for VarBC background constraint
   real , allocatable, dimension(:)  :: cv_xhat_jp ! Jp control variable.
   integer                           :: jp_start, jp_end       ! Start/end indices of Jp.
   integer                           :: inst, ichan, npred, ipred, id
   real                              :: bgerr
 
   if (trace_use) call da_trace_entry("da_calculate_j")

   mz = (/ be%v1%mz, be%v2%mz, be%v3%mz, be%v4%mz, be%v5%mz, be%alpha%mz /)

   je_start   = cv_size_jb + 1
   je_end     = cv_size_jb + cv_size_je
   jp_start   = cv_size_jb + cv_size_je + 1
   jp_end     = cv_size_jb + cv_size_je + cv_size_jp

   !-------------------------------------------------------------------------
   ! [1.0] calculate grad_v (jo):
   !-------------------------------------------------------------------------
   call da_allocate_y(iv, jo_grad_y)
   
   call da_transform_vtoy(cv_size, be, grid%ep, xhat, iv, grid%vp, &
      grid%vv, xbx, y, grid, config_flags)

   call da_calculate_grady(iv, y , jo_grad_y)

   call da_transform_vtoy_adj(iter, cv_size, be, grid%ep, j_grad, iv, &
      grid%vp, grid%vv, xbx, jo_grad_y, grid, config_flags, .true.)

   call da_deallocate_y(jo_grad_y)

   j_grad = - j_grad    !! Compensate for sign in calculation of grad_v (Jo)

   !-------------------------------------------------------------------------
   ! [2.0] calculate grad_v (jb):
   !-------------------------------------------------------------------------
   if (cv_size_jb > 0) then
      j_grad(1:cv_size_jb) =  j_grad(1:cv_size_jb) + jb_factor * &
                              (cv(1:cv_size_jb) + xhat(1:cv_size_jb))
   
   end if 	 

   !-------------------------------------------------------------------------
   ! [3.0] calculate grad_v (je):
   !-------------------------------------------------------------------------
   if (cv_size_je > 0) then
      j_grad(je_start:je_end) = j_grad(je_start:je_end) + je_factor * &
                                (cv(je_start:je_end) + xhat(je_start:je_end))
   end if
   
   !-------------------------------------------------------------------------
   ! [4.0] calculate grad_v (jp):
   !-------------------------------------------------------------------------
   if (use_varbc) then
      allocate(cv_xhat_jp(cv_size_jp))
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
      j_grad(jp_start:jp_end) = j_grad(jp_start:jp_end) + cv_xhat_jp(1:cv_size_jp)
   
      deallocate(cv_xhat_jp)
   end if
      
   if (trace_use) call da_trace_exit("da_calculate_j")

end subroutine da_calculate_gradj


