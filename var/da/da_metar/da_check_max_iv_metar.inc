subroutine da_check_max_iv_metar(iv,ob, it, num_qcstat_conv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it    ! outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.


   logical :: failed,failed1,failed2
   integer :: n

  if (trace_use_dull) call da_trace_entry("da_check_max_iv_metar")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(metar)%n1,iv%info(metar)%n2
      if(.not. qc_rej_both)then
         if(wind_sd_metar)then
            failed=.false.
            if( iv%metar(n)%u%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%u, max_error_spd,failed)
                if( iv%info(metar)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,metar,1,1) = num_qcstat_conv(1,metar,1,1) + 1
                    if(failed) then
                       num_qcstat_conv(2,metar,1,1) = num_qcstat_conv(2,metar,1,1) + 1
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                       'metar',ob_vars(1),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
                    end if
                end if
            end if

            failed=.false.
            if( iv%metar(n)%v%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%v, max_error_dir,failed)
                if( iv%info(metar)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,metar,2,1) = num_qcstat_conv(1,metar,2,1) + 1
                    if(failed)then
                       num_qcstat_conv(2,metar,2,1) = num_qcstat_conv(2,metar,2,1) + 1
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                            'metar',ob_vars(2),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
                    end if
                end if
            end if
         else
            failed=.false.
            if( iv%metar(n)%u%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%u, max_error_uv,failed)
                if( iv%info(metar)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,metar,1,1) = num_qcstat_conv(1,metar,1,1) + 1
                    if(failed) then
                       num_qcstat_conv(2,metar,1,1) = num_qcstat_conv(2,metar,1,1) + 1
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                             'metar',ob_vars(1),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
                    end if
                end if
             end if
             failed=.false.
             if( iv%metar(n)%v%qc >= obs_qc_pointer ) then
                 call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%v, max_error_uv,failed)
                 if( iv%info(metar)%proc_domain(1,n) ) then
                     num_qcstat_conv(1,metar,2,1) = num_qcstat_conv(1,metar,2,1) + 1
                     if(failed)then
                        num_qcstat_conv(2,metar,2,1) = num_qcstat_conv(2,metar,2,1) + 1
                        write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                              'metar',ob_vars(2),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
                     end if
                 end if
             end if
          end if

          if(wind_sd_metar)then
             if(iv%metar(n)%u%qc == fails_error_max .or.  abs(iv%metar(n)%u%inv) >= max_omb_spd) then
                iv%metar(n)%u%qc = fails_error_max
                iv%metar(n)%u%inv = 0.0
             endif
             if(iv%metar(n)%v%qc == fails_error_max .or.  abs(iv%metar(n)%v%inv) >= max_omb_dir) then
                iv%metar(n)%v%qc = fails_error_max
                iv%metar(n)%v%inv = 0.0
             endif
          endif
       else
          failed1=.false.
          failed2=.false.
          if( iv%metar(n)%v%qc >= obs_qc_pointer .or. iv%metar(n)%u%qc >= obs_qc_pointer )  then
              if(wind_sd_metar)then
                 call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%u, max_error_spd,failed1)
                 call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%v, max_error_dir,failed2)
              else
                 call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%u, max_error_uv,failed1)
                 call da_max_error_qc (it,iv%info(metar), n, iv%metar(n)%v, max_error_uv,failed2)
              endif
          endif

          if( iv%info(metar)%proc_domain(1,n) ) then
              num_qcstat_conv(1,metar,1,1) = num_qcstat_conv(1,metar,1,1) + 1
              num_qcstat_conv(1,metar,2,1) = num_qcstat_conv(1,metar,2,1) + 1

              if(failed1 .or. failed2) then
                 num_qcstat_conv(2,metar,1,1) = num_qcstat_conv(2,metar,1,1) + 1
                 write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                       'metar',ob_vars(1),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
                 num_qcstat_conv(2,metar,2,1) = num_qcstat_conv(2,metar,2,1) + 1
                 write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                       'metar',ob_vars(2),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
              endif
           endif

           if(wind_sd_metar)then
              if(iv%metar(n)%u%qc == fails_error_max .or. iv%metar(n)%v%qc == fails_error_max .or. &
                 abs(iv%metar(n)%v%inv) >= max_omb_dir .or. abs(iv%metar(n)%u%inv) >= max_omb_spd )then
                 iv%metar(n)%u%qc = fails_error_max
                 iv%metar(n)%v%qc = fails_error_max
                 iv%metar(n)%u%inv = 0.0
                 iv%metar(n)%v%inv = 0.0
              endif
           else
              if(iv%metar(n)%u%qc == fails_error_max .or. iv%metar(n)%v%qc == fails_error_max ) then
                 iv%metar(n)%u%qc = fails_error_max
                 iv%metar(n)%v%qc = fails_error_max
                 iv%metar(n)%u%inv = 0.0
                 iv%metar(n)%v%inv = 0.0
              endif
           endif
        endif

      failed=.false.
      if( iv%metar(n)%t%qc >= obs_qc_pointer )  then
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%t, max_error_t , failed)
      if( iv%info(metar)%proc_domain(1,n) ) then
      num_qcstat_conv(1,metar,3,1)= num_qcstat_conv(1,metar,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,3,1)= num_qcstat_conv(2,metar,3,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(3),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%metar(n)%p%qc >= obs_qc_pointer )  then 
      call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%p, max_error_p , failed)         
      if( iv%info(metar)%proc_domain(1,n) ) then
      num_qcstat_conv(1,metar,5,1)= num_qcstat_conv(1,metar,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,5,1)= num_qcstat_conv(2,metar,5,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(5),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%metar(n)%q%qc >= obs_qc_pointer ) then
       if( iv%metar(n)%t%qc == fails_error_max .or. iv%metar(n)%p%qc == fails_error_max) then
       failed=.true.
       iv%metar(n)%q%qc  = fails_error_max
       iv%metar(n)%q%inv = 0.0
       else
       call da_max_error_qc (it, iv%info(metar), n, iv%metar(n)%q, max_error_q , failed)
       endif
      if( iv%info(metar)%proc_domain(1,n) ) then
      num_qcstat_conv(1,metar,4,1)= num_qcstat_conv(1,metar,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,metar,4,1)= num_qcstat_conv(2,metar,4,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
      'metar',ob_vars(4),iv%info(metar)%lat(1,n),iv%info(metar)%lon(1,n),0.01*ob%metar(n)%p
      end if
      end if
      end if
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_metar")

end subroutine da_check_max_iv_metar
