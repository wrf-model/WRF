   subroutine da_varbc_tamdar_direct (iv,ob)

   !-----------------------------------------------!
   !  Apply bias correction to TAMDAR innovations  !
   !-----------------------------------------------!

   implicit none

   type (iv_type), intent(inout)   :: iv
   type (y_type),  intent(inout)   :: ob

   real                            :: bc,bias
   real                            :: contri(5)
   integer                         :: i,isn,iflt,iobs,ipred,iphase,nobs
   character(len=3)                :: cphz(3)
   character(len=30)               :: stringn


   if (trace_use) call da_trace_entry("da_varbc_tamdar_direct")

   if (rootproc) &
       write(unit=varbc_tamdar_unit,fmt='(//5X,A/)')'Calculating corrected innovation'

   cphz = (/'des','asc','cru'/)

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase
         if (iv%varbc_tamdar%nobs_sum(iphase,iflt) >= varbc_tamdar_nobsmin) then

             bc = SUM( iv%varbc_tamdar%param(1:iv%varbc_tamdar%npred,iphase,iflt) * &
                       iv%varbc_tamdar%pred(1:iv%varbc_tamdar%npred,iphase,iflt) )

             nobs = 0
             bias = 0.

             if (iv%varbc_tamdar%nobs(iphase,iflt) > 0 .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0 ) then 
                 do iobs = 1, iv%varbc_tamdar%nobs(iphase,iflt)
                    isn = iv%varbc_tamdar%obs_sn(iobs,iphase,iflt)
                    if (iv%tamdar(isn)%t(1)%qc >= 0) then
                        bias = bias + iv%tamdar(isn)%t(1)%inv
                        nobs = nobs + 1

                        iv%tamdar(isn)%t(1)%inv = iv%tamdar(isn)%t(1)%inv - bc
                    end if
                 end do
             end if

             if (wrf_dm_sum_integer(nobs) > 0) &
                 bias = wrf_dm_sum_real(bias)/wrf_dm_sum_integer(nobs)

             if (rootproc .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then
                 write(unit=varbc_tamdar_unit,fmt='(10X,A,I5,A,2X,A,2X,A,2F10.4)') &
                      'bias corrected for ',iv%varbc_tamdar%tail_id(iflt),  &
                      '  at', cphz(iphase), ': (BC,OMB)', bc, bias
             end if

         end if
      end do
   end do

   if (rootproc) write(unit=varbc_tamdar_unit,fmt='(/10X,A)') &
                      " ID  Phase  Predictor/Parameter/Bias model (npred)  &  BC"

   ipred = 3*iv%varbc_tamdar%npred+1
   write(stringn,'(I3)') ipred
   stringn = '(10X,I4,2X,A,2X,'//trim(ADJUSTL(stringn))//'f8.3)'
   stringn = trim(adjustl(stringn))

   do iflt = 1, iv%varbc_tamdar%nair
      do iphase = 1, iv%varbc_tamdar%nphase
         if (rootproc .and. iv%varbc_tamdar%ifuse(iphase,iflt) > 0) then 

             contri(:) = 0.
             bc = 0.

             do ipred = 1, iv%varbc_tamdar%npred
                contri(ipred) = &
                       iv%varbc_tamdar%param(ipred,iphase,iflt) * &
                       iv%varbc_tamdar%pred(ipred,iphase,iflt)
                bc = bc + contri(ipred)
             end do

             write(unit=varbc_tamdar_unit,fmt=stringn) &
                   iv%varbc_tamdar%tail_id(iflt), cphz(iphase), &
                  (iv%varbc_tamdar%pred(ipred,iphase,iflt), &
                   iv%varbc_tamdar%param(ipred,iphase,iflt), &
                   contri(ipred), ipred=1,iv%varbc_tamdar%npred),bc
         end if
      end do
   end do

   if (trace_use) call da_trace_exit("da_varbc_tamdar_direct")

   end subroutine da_varbc_tamdar_direct
