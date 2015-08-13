  subroutine da_varbc_adj (cv_size, cv, iv, y)

   !---------------------------------------------------------------------------
   !  PURPOSE: Apply bias correction to radiance innovations.
   !
   !  Called from da_transform_vtoy_adj
   !
   !  HISTORY: 10/27/2007 - Creation                     Tom Auligne
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)           :: cv_size         ! Size of cv array.
   real, intent(inout)           :: cv(1:cv_size)   ! control variables.
   type (iv_type), intent(inout) :: iv              ! O-B structure.
   type (y_type), intent(in)     :: y               ! y = h (xa)

   integer              :: inst, i, k, num_rad, n, npred
   real, allocatable    :: varbc_param_adj(:)
   real                 :: cv_local

   if (trace_use) call da_trace_entry("da_varbc_adj")

      do inst = 1, iv%num_inst                 ! loop for sensor
         num_rad = iv%instid(inst)%num_rad
   
         allocate(varbc_param_adj(iv%instid(inst)%varbc_info%npredmax))

         do k=1,iv%instid(inst)%nchan          ! loop for channel
            npred = iv%instid(inst)%varbc(k)%npred
	    if (npred <= 0) cycle              ! VarBC channels only
	   
           !---------------------------------------------------------------
           ! Adjoint to bias correction through linear regression
           !---------------------------------------------------------------
            varbc_param_adj(:) = 0.0
	    if (num_rad >0) then
	       do n = 1, num_rad                                     ! loop for pixel      
                  if (iv%instid(inst)%tb_qc(k,n) <= qc_varbc_bad) cycle  ! good obs only
                  if (.not. iv%instid(inst)%info%proc_domain(1,n))  cycle   ! do not sum up HALO data
	       
		  varbc_param_adj(1:npred) = varbc_param_adj(1:npred)      + &
	       		   		     y%instid(inst)%tb(k,n)        * &
		                            iv%instid(inst)%varbc_info%pred( &
					    iv%instid(inst)%varbc(k)%ipred(1:npred),n ) 
	       end do	        
	    end if
		       
           !---------------------------------------------------------------
           ! Change of variable (preconditioning) + sum across processors
           !---------------------------------------------------------------
	    do i = 1, npred
               cv_local = SUM(varbc_param_adj(1:npred) * iv%instid(inst)%varbc(k)%vtox(i,1:npred))
               cv(iv%instid(inst)%varbc(k)%index(i)) = cv(iv%instid(inst)%varbc(k)%index(i)) + &
	                                               wrf_dm_sum_real(cv_local)	
            end do
	 end do
	 
         deallocate(varbc_param_adj)	     
      end do                                   ! end loop for sensor

   if (trace_use) call da_trace_exit("da_varbc_adj")

 end subroutine da_varbc_adj
