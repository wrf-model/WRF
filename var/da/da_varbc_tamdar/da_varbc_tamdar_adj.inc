   subroutine da_varbc_tamdar_adj (cv_size, cv, iv, y)

   implicit none

   integer, intent(in)           :: cv_size    
   real, intent(inout)           :: cv(1:cv_size)
   type (iv_type), intent(inout) :: iv        
   type (y_type), intent(in)     :: y             ! y = h (xa)

   integer                       :: isn,iflt,iobs,ipred,iphase,npred
   real, allocatable             :: varbc_param_adj(:)
   real                          :: yh, cv_local


   if (trace_use) call da_trace_entry("da_varbc_tamdar_adj")

   npred = iv%varbc_tamdar%npred

   allocate( varbc_param_adj(npred) )

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase
         if (iv%varbc_tamdar%nobs_sum(iphase,iflt) >= varbc_tamdar_nobsmin) then

             varbc_param_adj(:) = 0.0

             if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                 do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                    isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                    yh = 0
                    if( iv%tamdar(isn)%t(1)%qc >= 0 ) yh = y%tamdar(isn)%t(1)
                    varbc_param_adj(1:npred) = varbc_param_adj(1:npred) + &
                                               yh * iv%varbc_tamdar%pred(1:npred,iphase,iflt)
                 end do
             end if

             do ipred = 1, npred
                cv_local = SUM( varbc_param_adj(1:npred) * iv%varbc_tamdar%vtox(ipred,1:npred,iphase,iflt) )
                cv_local = wrf_dm_sum_real(cv_local)

                if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) &
                    cv(iv%varbc_tamdar%index(ipred,iphase,iflt)) = cv( iv%varbc_tamdar%index(ipred,iphase,iflt) ) + cv_local
             end do

         end if
      end do
   end do

   deallocate(varbc_param_adj)	     

   if (trace_use) call da_trace_exit("da_varbc_tamdar_adj")

   end subroutine da_varbc_tamdar_adj
