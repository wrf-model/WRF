subroutine da_check_max_iv_polaramv(iv,it,num_qcstat_conv)   

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type),    intent(inout) :: iv
   integer,          intent(in)    :: it      ! Outer iteration
   integer,          intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer  :: k,n, ipr
   logical  :: failed,failed1,failed2
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_polaramv")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(polaramv)%n1,iv%info(polaramv)%n2
      do k = 1, iv%info(polaramv)%levels(n)
        call da_get_print_lvl(iv%polaramv(n)%p(k),ipr)
         if(.not. qc_rej_both)then
             if(wind_sd_polaramv)then
               failed=.false.
               if( iv%polaramv(n)%u(k)%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%u(k), max_error_spd,failed)
                   if( iv%info(polaramv)%proc_domain(k,n) ) then
                       num_qcstat_conv(1,polaramv,1,ipr) = num_qcstat_conv(1,polaramv,1,ipr) + 1
                       if(failed) then
                          num_qcstat_conv(2,polaramv,1,ipr) = num_qcstat_conv(2,polaramv,1,ipr) + 1
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'polaramv',ob_vars(1),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                       end if
                   end if
                end if

                failed=.false.
                if( iv%polaramv(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%v(k), max_error_dir,failed)
                    if( iv%info(polaramv)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,polaramv,2,ipr) = num_qcstat_conv(1,polaramv,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,polaramv,2,ipr) = num_qcstat_conv(2,polaramv,2,ipr) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'polaramv',ob_vars(2),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                        end if
                    end if
                end if

             else

                failed=.false.
                if( iv%polaramv(n)%u(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%u(k), max_error_uv,failed)
                    if( iv%info(polaramv)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,polaramv,1,ipr) = num_qcstat_conv(1,polaramv,1,ipr) + 1
                        if(failed) then
                           num_qcstat_conv(2,polaramv,1,ipr) = num_qcstat_conv(2,polaramv,1,ipr) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'polaramv',ob_vars(1),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                        end if
                    end if
                end if

                failed=.false.
                if( iv%polaramv(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%v(k), max_error_uv,failed)
                    if( iv%info(polaramv)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,polaramv,2,ipr) = num_qcstat_conv(1,polaramv,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,polaramv,2,ipr) = num_qcstat_conv(2,polaramv,2,ipr) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'polaramv',ob_vars(2),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                        end if
                    end if
                 end if
             end if

             if(wind_sd_polaramv)then
                if(iv%polaramv(n)%u(k)%qc == fails_error_max .or. abs(iv%polaramv(n)%u(k)%inv) >= max_omb_spd) then
                   iv%polaramv(n)%u(k)%qc = fails_error_max
                   iv%polaramv(n)%u(k)%inv = 0.0
                endif
                if(iv%polaramv(n)%v(k)%qc == fails_error_max .or. abs(iv%polaramv(n)%v(k)%inv) >= max_omb_dir) then
                   iv%polaramv(n)%v(k)%qc = fails_error_max
                   iv%polaramv(n)%v(k)%inv = 0.0
                endif
             endif

          else


         failed1=.false.
         failed2=.false.

         if( iv%polaramv(n)%v(k)%qc >= obs_qc_pointer .or. iv%polaramv(n)%u(k)%qc >= obs_qc_pointer )  then
             if(wind_sd_polaramv)then
                call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%u(k), max_error_spd,failed1)
                call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%v(k), max_error_dir,failed2)
             else
                call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%u(k), max_error_uv,failed1)
                call da_max_error_qc (it,iv%info(polaramv), n, iv%polaramv(n)%v(k), max_error_uv,failed2)
             endif

             if( iv%info(polaramv)%proc_domain(k,n) ) then
                 num_qcstat_conv(1,polaramv,1,ipr) = num_qcstat_conv(1,polaramv,1,ipr) + 1
                 num_qcstat_conv(1,polaramv,2,ipr) = num_qcstat_conv(1,polaramv,2,ipr) + 1

                 if(failed1 .or. failed2) then
                    num_qcstat_conv(2,polaramv,1,ipr) = num_qcstat_conv(2,polaramv,1,ipr) + 1
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'polaramv',ob_vars(1),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                    num_qcstat_conv(2,polaramv,2,ipr) = num_qcstat_conv(2,polaramv,2,ipr) + 1
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'polaramv',ob_vars(2),iv%info(polaramv)%lat(k,n),iv%info(polaramv)%lon(k,n),0.01*iv%polaramv(n)%p(k)
                 end if
             end if
          end if

	  if(wind_sd_polaramv)then
             if(iv%polaramv(n)%u(k)%qc == fails_error_max .or. iv%polaramv(n)%v(k)%qc == fails_error_max .or. &
                abs(iv%polaramv(n)%v(k)%inv) >= max_omb_dir .or. abs(iv%polaramv(n)%u(k)%inv) >= max_omb_spd )then
                iv%polaramv(n)%u(k)%qc = fails_error_max
                iv%polaramv(n)%v(k)%qc = fails_error_max
                iv%polaramv(n)%u(k)%inv = 0.0
                iv%polaramv(n)%v(k)%inv = 0.0
             endif
          else
             if(iv%polaramv(n)%u(k)%qc == fails_error_max .or. iv%polaramv(n)%v(k)%qc == fails_error_max ) then
                iv%polaramv(n)%u(k)%qc = fails_error_max
                iv%polaramv(n)%v(k)%qc = fails_error_max
                iv%polaramv(n)%u(k)%inv = 0.0
                iv%polaramv(n)%v(k)%inv = 0.0
              endif
          endif
       endif
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_polaramv")

end subroutine da_check_max_iv_polaramv      
