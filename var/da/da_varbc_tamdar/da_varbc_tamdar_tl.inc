   subroutine da_varbc_tamdar_tl (cv_size, cv, iv, y)

   !---------------------------------------------------!
   !  METHOD: delta_d    = y - delta_bc                !
   !          y          = H (delta_x)                 !
   !          delta_bc   = SUM( delta_beta_i Pred_i )  !
   !---------------------------------------------------!

   implicit none

   integer, intent(in)           :: cv_size
   real, intent(in)              :: cv(1:cv_size)
   type (iv_type), intent(in)    :: iv   
   type (y_type), intent(inout)  :: y               ! y = h (xa)

   integer                       :: isn,ivar,iflt,ipred,iobs,iphase
   real                          :: delta_bc

   real, allocatable             :: varbc_param_tl(:)
   

   if (trace_use) call da_trace_entry("da_varbc_tamdar_tl")

   allocate( varbc_param_tl(iv%varbc_tamdar%npred) )

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase

         if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then

             varbc_param_tl(:) = 0.0

             do ipred = 1, iv%varbc_tamdar%npred
                varbc_param_tl(ipred) = &
	                   SUM( cv(iv%varbc_tamdar%index(ipred,iphase,iflt)) * &
                                iv%varbc_tamdar%vtox(ipred,1:iv%varbc_tamdar%npred,iphase,iflt) )
             end do	

             delta_bc = SUM( varbc_param_tl(1:iv%varbc_tamdar%npred)* &
                             iv%varbc_tamdar%pred(1:iv%varbc_tamdar%npred,iphase,iflt) )

             do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                if (iv%tamdar(isn)%t(1)%qc >= 0) &
	            y%tamdar(isn)%t(1) = y%tamdar(isn)%t(1) + delta_bc
             end do

         end if
      end do
   end do

   deallocate(varbc_param_tl)

   if (trace_use) call da_trace_exit("da_varbc_tamdar_tl")

   end subroutine da_varbc_tamdar_tl
