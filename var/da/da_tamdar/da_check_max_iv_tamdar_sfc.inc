subroutine da_check_max_iv_tamdar_sfc(iv,ob, it, num_qcstat_conv)                                 

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.


   logical :: failed,failed1,failed2
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_tamdar_sfc")


   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(tamdar_sfc)%n1,iv%info(tamdar_sfc)%n2
         if(.not. qc_rej_both)then
            if(wind_sd_tamdar)then
               failed=.false.
               if( iv%tamdar_sfc(n)%u%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%u, max_error_spd,failed)
                   if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
                       num_qcstat_conv(1,tamdar_sfc,1,1) = num_qcstat_conv(1,tamdar_sfc,1,1) + 1
                       if(failed) then
                          num_qcstat_conv(2,tamdar_sfc,1,1) = num_qcstat_conv(2,tamdar_sfc,1,1) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'tamdar_sfc',ob_vars(1),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                          end if
                       end if
                   end if
               end if

               failed=.false.
               if( iv%tamdar_sfc(n)%v%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%v, max_error_dir,failed)
                   if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
                       num_qcstat_conv(1,tamdar_sfc,2,1) = num_qcstat_conv(1,tamdar_sfc,2,1) + 1
                       if(failed)then
                          num_qcstat_conv(2,tamdar_sfc,2,1) = num_qcstat_conv(2,tamdar_sfc,2,1) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'tamdar_sfc',ob_vars(2),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                          end if
                       end if
                   end if
               end if
            else
               failed=.false.
               if( iv%tamdar_sfc(n)%u%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%u, max_error_uv,failed)
                   if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
                       num_qcstat_conv(1,tamdar_sfc,1,1) = num_qcstat_conv(1,tamdar_sfc,1,1) + 1
                       if(failed) then
                          num_qcstat_conv(2,tamdar_sfc,1,1) = num_qcstat_conv(2,tamdar_sfc,1,1) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'tamdar_sfc',ob_vars(1),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                          end if
                       end if
                   end if
               end if

               failed=.false.
               if( iv%tamdar_sfc(n)%v%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%v, max_error_uv,failed)
                   if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
                       num_qcstat_conv(1,tamdar_sfc,2,1) = num_qcstat_conv(1,tamdar_sfc,2,1) + 1
                       if(failed)then
                          num_qcstat_conv(2,tamdar_sfc,2,1) = num_qcstat_conv(2,tamdar_sfc,2,1) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'tamdar_sfc',ob_vars(2),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                          end if
                       end if
                   end if
               end if
            end if

            if(wind_sd_tamdar)then
               if(iv%tamdar_sfc(n)%u%qc == fails_error_max .or.  abs(iv%tamdar_sfc(n)%u%inv) >= max_omb_spd) then
                  iv%tamdar_sfc(n)%u%qc = fails_error_max
                  iv%tamdar_sfc(n)%u%inv = 0.0
               endif
               if(iv%tamdar_sfc(n)%v%qc == fails_error_max .or.  abs(iv%tamdar_sfc(n)%v%inv) >= max_omb_dir) then
                  iv%tamdar_sfc(n)%v%qc = fails_error_max
                  iv%tamdar_sfc(n)%v%inv = 0.0
               endif
            endif
         else
            failed1=.false.
            failed2=.false.

            if( iv%tamdar_sfc(n)%v%qc >= obs_qc_pointer .or. iv%tamdar_sfc(n)%u%qc >= obs_qc_pointer )  then
                if(wind_sd_tamdar)then
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%u, max_error_spd,failed1)
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%v, max_error_dir,failed2)
                else
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%u, max_error_uv,failed1)
                   call da_max_error_qc (it,iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%v, max_error_uv,failed2)
                endif
            endif

            if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
                num_qcstat_conv(1,tamdar_sfc,1,1) = num_qcstat_conv(1,tamdar_sfc,1,1) + 1
                num_qcstat_conv(1,tamdar_sfc,2,1) = num_qcstat_conv(1,tamdar_sfc,2,1) + 1

                if(failed1 .or. failed2) then
                   num_qcstat_conv(2,tamdar_sfc,1,1) = num_qcstat_conv(2,tamdar_sfc,1,1) + 1
                   if ( write_rej_obs_conv ) then
                   write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                         'tamdar_sfc',ob_vars(1),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                   end if
                   num_qcstat_conv(2,tamdar_sfc,2,1) = num_qcstat_conv(2,tamdar_sfc,2,1) + 1
                   if ( write_rej_obs_conv ) then
                   write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                         'tamdar_sfc',ob_vars(2),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
                   end if
                endif
            endif

            if(wind_sd_tamdar)then
               if(iv%tamdar_sfc(n)%u%qc == fails_error_max .or. iv%tamdar_sfc(n)%v%qc == fails_error_max .or. &
                  abs(iv%tamdar_sfc(n)%v%inv) >= max_omb_dir .or. abs(iv%tamdar_sfc(n)%u%inv) >= max_omb_spd )then
                  iv%tamdar_sfc(n)%u%qc = fails_error_max
                  iv%tamdar_sfc(n)%v%qc = fails_error_max
                  iv%tamdar_sfc(n)%u%inv = 0.0
                  iv%tamdar_sfc(n)%v%inv = 0.0
               endif
            else
               if(iv%tamdar_sfc(n)%u%qc == fails_error_max .or. iv%tamdar_sfc(n)%v%qc == fails_error_max ) then
                  iv%tamdar_sfc(n)%u%qc = fails_error_max
                  iv%tamdar_sfc(n)%v%qc = fails_error_max
                  iv%tamdar_sfc(n)%u%inv = 0.0
                  iv%tamdar_sfc(n)%v%inv = 0.0
               endif
            endif
         endif

      failed=.false.
      if( iv%tamdar_sfc(n)%t%qc >= obs_qc_pointer )  then 
      call da_max_error_qc (it, iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%t, max_error_t , failed)
      if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
      num_qcstat_conv(1,tamdar_sfc,3,1)= num_qcstat_conv(1,tamdar_sfc,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,tamdar_sfc,3,1)= num_qcstat_conv(2,tamdar_sfc,3,1) + 1
      if ( write_rej_obs_conv ) then
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'tamdar_sfc',ob_vars(3),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
      end if
      end if
      end if
      end if

      failed=.false.
      if( iv%tamdar_sfc(n)%p%qc >= obs_qc_pointer ) then
      call da_max_error_qc (it, iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%p, max_error_p , failed)         
      if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
      num_qcstat_conv(1,tamdar_sfc,5,1)= num_qcstat_conv(1,tamdar_sfc,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,tamdar_sfc,5,1)= num_qcstat_conv(2,tamdar_sfc,5,1) + 1
      if ( write_rej_obs_conv ) then
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'tamdar_sfc',ob_vars(5),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
      end if
      end if
      end if
      end if

      failed=.false.
      if( iv%tamdar_sfc(n)%q%qc >= obs_qc_pointer ) then
       if( iv%tamdar_sfc(n)%t%qc == fails_error_max .or. iv%tamdar_sfc(n)%p%qc == fails_error_max) then
       failed=.true.
       iv%tamdar_sfc(n)%q%qc  = fails_error_max
       iv%tamdar_sfc(n)%q%inv = 0.0
       else
       call da_max_error_qc (it, iv%info(tamdar_sfc), n, iv%tamdar_sfc(n)%q, max_error_q , failed)
       endif
      if( iv%info(tamdar_sfc)%proc_domain(1,n) ) then
      num_qcstat_conv(1,tamdar_sfc,4,1)= num_qcstat_conv(1,tamdar_sfc,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,tamdar_sfc,4,1)= num_qcstat_conv(2,tamdar_sfc,4,1) + 1
      if ( write_rej_obs_conv ) then
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'tamdar_sfc',ob_vars(4),iv%info(tamdar_sfc)%lat(1,n),iv%info(tamdar_sfc)%lon(1,n),0.01*ob%tamdar_sfc(n)%p
      end if
      end if
      end if
      end if

   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_tamdar_sfc")

end subroutine da_check_max_iv_tamdar_sfc
