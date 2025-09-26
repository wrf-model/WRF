   subroutine da_varbc_tamdar_precond (iv)

   !----------------------------------------------------!
   !  Estimate Hessian for Preconditioning              !
   !                                                    !
   !  Preconditioning = inverse square root of Hessian  !
   !                                                    !
   !  Hermite real matrix:                              !
   !                 A = Q*D*Q^T                        !
   !           A^(1/k) = Q*D^(1/K)*Q^T                  !
   !----------------------------------------------------!

   implicit none

   type (iv_type), intent (inout) :: iv

   integer                        :: i,j,iflt,iobs,isn,ipred,iphase,npred
   real                           :: lhessian,lbgerr,predi,predj,verr

   real, allocatable 		  :: hessian(:,:)
   real*8, allocatable 		  :: eignvec(:,:), eignval(:)
   

   if (trace_use) call da_trace_entry("da_varbc_tamdar_precond")
   
   if (rootproc) &
       write(unit=varbc_tamdar_unit,fmt='(//5X,A)') 'Estimating hessian for preconditioning'

   npred = iv%varbc_tamdar%npred

   allocate ( hessian(npred, npred) )
   allocate ( eignvec(npred, npred) )
   allocate ( eignval(npred)        )

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase
         if (iv%varbc_tamdar%nobs_sum(iphase,iflt) >= varbc_tamdar_nobsmin) then

             hessian(:,:) = 0.
             eignvec(:,:) = 0.
             eignval(:)   = 0.

             lbgerr = 0.0
             if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                 do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                    isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                    verr = 0.0
                    if (iv%tamdar(isn)%t(1)%qc >= 0) verr = iv%tamdar(isn)%t(1)%error
                    lbgerr = lbgerr + verr**2.0/varbc_tamdar_nbgerr
                 end do
             end if
             lbgerr = wrf_dm_sum_real(lbgerr)

             do i = 1, npred
                do j = i, npred
                   lhessian = 0.0	
      
                   if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                       do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                          isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                          predi = iv%varbc_tamdar%pred(i,iphase,iflt)
                          predj = iv%varbc_tamdar%pred(j,iphase,iflt)

                          verr = 0.0
                          if (iv%tamdar(isn)%t(1)%qc >= 0) verr = iv%tamdar(isn)%t(1)%error
                          if (verr > 0.) lhessian = lhessian + predi*predj/verr**2.0
		       end do 
                   end if
                   hessian(i,j) = wrf_dm_sum_real(lhessian)
	           hessian(j,i) = hessian(i,j)  	       
	        end do
         
                iv%varbc_tamdar%bgerr(i,iphase,iflt) = lbgerr/iv%varbc_tamdar%nobs_sum(iphase,iflt)
                if (iv%varbc_tamdar%bgerr(i,iphase,iflt) > 0.) & 
                    hessian(i,i)=hessian(i,i)+1.0/iv%varbc_tamdar%bgerr(i,iphase,iflt)
             end do   

             if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then

                 call da_eof_decomposition(npred, hessian(1:npred,1:npred), &
                                           eignvec(1:npred,1:npred),eignval(1:npred))

                 if (ANY( eignval(1:npred) <= 0 )) then
                     write(unit=stdout,fmt='(A,I8,A,2F12.5)') &
                          'VARBC_TAMDAR: non-positive Hessian for tail_id ', iv%varbc_tamdar%tail_id(iflt), &
                          '. Eigenvalues =',eignval(1:npred) 

                     do i = 1, npred
                        if (hessian(i,i) > 0) iv%varbc_tamdar%vtox(i,i,iphase,iflt) = 1.0/sqrt(hessian(i,i))
                     end do
                 else
                     do i = 1, npred
                     do j = i, npred
                        iv%varbc_tamdar%vtox(i,j,iphase,iflt) = SUM( eignvec(i,1:npred)* &
                                                                     sqrt(1.0/eignval(1:npred)) * &
                                                                     eignvec(j,1:npred) )
                        iv%varbc_tamdar%vtox(j,i,iphase,iflt) = iv%varbc_tamdar%vtox(i,j,iphase,iflt)
                     end do
                     end do
                 end if
             end if
         end if
      end do   
   end do

   deallocate(hessian, eignvec, eignval)   

   if (rootproc) &
       write(unit=varbc_tamdar_unit,fmt='(/5X,A)') 'End estimating hessian for preconditioning'

   if (trace_use) call da_trace_exit("da_varbc_tamdar_precond")

   end subroutine da_varbc_tamdar_precond
