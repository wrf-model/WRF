  subroutine da_varbc_tl (cv_size, cv, iv, y)

   !---------------------------------------------------------------------------
   !  PURPOSE: Apply bias correction to radiance innovations.
   !
   !  METHOD: delta_d    = y - delta_bc
   !          y = H (delta_x)
   !          delta_bc   = SUM( delta_beta_i Pred_i )
   !                       i
   !          delta_beta = VarBC parameters
   !          Pred = Bias predictors
   ! 
   !  Called from da_transform_vtoy
   !
   !  HISTORY: 10/27/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)           :: cv_size         ! Size of cv array.
   real, intent(in)              :: cv(1:cv_size)   ! control variables.
   type (iv_type), intent(in)    :: iv              ! O-B structure.
   type (y_type), intent(inout)  :: y               ! y = h (xa)

   integer              :: inst, i, k, num_rad, n, npred
   real, allocatable    :: varbc_param_tl(:)
   
   if (trace_use) call da_trace_entry("da_varbc_tl")

      do inst = 1, iv%num_inst                 ! loop for sensor
	 num_rad = iv%instid(inst)%num_rad
         if (num_rad < 1) cycle

         allocate(varbc_param_tl(iv%instid(inst)%varbc_info%npredmax))
  
         do k=1,iv%instid(inst)%nchan          ! loop for channel
            npred = iv%instid(inst)%varbc(k)%npred
	    if (npred <= 0) cycle              ! VarBC channels only
	    	     
           !---------------------------------------------------------------
           ! Change of variable (preconditioning) 
           !---------------------------------------------------------------
            varbc_param_tl(:) = 0.0
	    do i = 1, npred
    	       varbc_param_tl(i) = SUM( cv(iv%instid(inst)%varbc(k)%index(1:npred)) * &
	                                iv%instid(inst)%varbc(k)%vtox(i,1:npred) )
	    end do	
		
           !---------------------------------------------------------------
           ! TL of bias correction through linear regression
           !---------------------------------------------------------------
	    do n = 1, num_rad                                         ! loop for pixel      
               if (iv%instid(inst)%tb_qc(k,n) <= qc_varbc_bad) cycle  ! good obs only

  	        y%instid(inst)%tb(k,n) = y%instid(inst)%tb(k,n) +   &
		   SUM( varbc_param_tl(1:npred) *                   &
		        iv%instid(inst)%varbc_info%pred(            &
		        iv%instid(inst)%varbc(k)%ipred(1:npred),n) )
            end do
         end do

         deallocate(varbc_param_tl)
      end do                                   ! end loop for sensor

   if (trace_use) call da_trace_exit("da_varbc_tl")

 end subroutine da_varbc_tl
