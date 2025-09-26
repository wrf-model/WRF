subroutine da_check_max_iv_profiler(iv, it, num_qcstat_conv)   

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

   integer :: k,n, ipr
   logical :: failed,failed1,failed2
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_profiler")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(profiler)%n1,iv%info(profiler)%n2
      do k = 1, iv%info(profiler)%levels(n)

       call da_get_print_lvl(iv%profiler(n)%p(k),ipr)

        if(.not. qc_rej_both)then
             if(wind_sd_profiler)then
               failed=.false.
               if( iv%profiler(n)%u(k)%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%u(k), max_error_spd,failed)
                   if( iv%info(profiler)%proc_domain(k,n) ) then
                       num_qcstat_conv(1,profiler,1,ipr) = num_qcstat_conv(1,profiler,1,ipr) + 1
                       if(failed) then
                          num_qcstat_conv(2,profiler,1,ipr) = num_qcstat_conv(2,profiler,1,ipr) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'profiler',ob_vars(1),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                          end if
                       end if
                   end if
                end if

                failed=.false.
                if( iv%profiler(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%v(k), max_error_dir,failed)
                    if( iv%info(profiler)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,profiler,2,ipr) = num_qcstat_conv(1,profiler,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,profiler,2,ipr) = num_qcstat_conv(2,profiler,2,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'profiler',ob_vars(2),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                           end if
                        end if
                    end if
                end if

             else

            failed=.false.
            if( iv%profiler(n)%u(k)%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%u(k), max_error_uv,failed)
                if( iv%info(profiler)%proc_domain(k,n) ) then
                    num_qcstat_conv(1,profiler,1,ipr) = num_qcstat_conv(1,profiler,1,ipr) + 1
                    if(failed) then
                       num_qcstat_conv(2,profiler,1,ipr) = num_qcstat_conv(2,profiler,1,ipr) + 1
                       if ( write_rej_obs_conv ) then
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                       'profiler',ob_vars(1),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                       end if
                    end if
                end if
             end if

             failed=.false.
             if( iv%profiler(n)%v(k)%qc >= obs_qc_pointer ) then
                 call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%v(k), max_error_uv,failed)
                 if( iv%info(profiler)%proc_domain(k,n) ) then
                     num_qcstat_conv(1,profiler,2,ipr) = num_qcstat_conv(1,profiler,2,ipr) + 1
                     if(failed)then
                        num_qcstat_conv(2,profiler,2,ipr) = num_qcstat_conv(2,profiler,2,ipr) + 1
                        if ( write_rej_obs_conv ) then
                        write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                        'profiler',ob_vars(2),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                        end if
                     end if
                 end if
             end if
          end if

             if(wind_sd_profiler)then
                if(iv%profiler(n)%u(k)%qc == fails_error_max .or.  abs(iv%profiler(n)%u(k)%inv) >= max_omb_spd) then
                   iv%profiler(n)%u(k)%qc = fails_error_max
                   iv%profiler(n)%u(k)%inv = 0.0
                endif
                if(iv%profiler(n)%v(k)%qc == fails_error_max .or.  abs(iv%profiler(n)%v(k)%inv) >= max_omb_dir) then
                   iv%profiler(n)%v(k)%qc = fails_error_max
                   iv%profiler(n)%v(k)%inv = 0.0
                endif
             endif

          else
             failed1=.false.
             failed2=.false.

             if( iv%profiler(n)%v(k)%qc >= obs_qc_pointer .or. iv%profiler(n)%u(k)%qc >= obs_qc_pointer )  then
                 if(wind_sd_profiler)then
                    call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%u(k), max_error_spd,failed1)
                    call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%v(k), max_error_dir,failed2)
                 else
                    call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%u(k), max_error_uv,failed1)
                    call da_max_error_qc (it,iv%info(profiler), n, iv%profiler(n)%v(k), max_error_uv,failed2)
                 endif
             endif

             if( iv%info(profiler)%proc_domain(k,n) ) then
                 num_qcstat_conv(1,profiler,1,ipr) = num_qcstat_conv(1,profiler,1,ipr) + 1
                 num_qcstat_conv(1,profiler,2,ipr) = num_qcstat_conv(1,profiler,2,ipr) + 1

                 if(failed1 .or. failed2) then
                    num_qcstat_conv(2,profiler,1,ipr) = num_qcstat_conv(2,profiler,1,ipr) + 1
                    if ( write_rej_obs_conv ) then
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'profiler',ob_vars(1),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                    end if
                    num_qcstat_conv(2,profiler,2,ipr) = num_qcstat_conv(2,profiler,2,ipr) + 1
                    if ( write_rej_obs_conv ) then
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'profiler',ob_vars(2),iv%info(profiler)%lat(k,n),iv%info(profiler)%lon(k,n),0.01*iv%profiler(n)%p(k)
                    end if
                 endif
             endif

             if(wind_sd_profiler)then
                if(iv%profiler(n)%u(k)%qc == fails_error_max .or. iv%profiler(n)%v(k)%qc == fails_error_max .or. &
                   abs(iv%profiler(n)%v(k)%inv) >= max_omb_dir .or. abs(iv%profiler(n)%u(k)%inv) >= max_omb_spd )then
                   iv%profiler(n)%u(k)%qc = fails_error_max
                   iv%profiler(n)%v(k)%qc = fails_error_max
                   iv%profiler(n)%u(k)%inv = 0.0
                   iv%profiler(n)%v(k)%inv = 0.0
                endif
             else
                if(iv%profiler(n)%u(k)%qc == fails_error_max .or. iv%profiler(n)%v(k)%qc == fails_error_max ) then
                   iv%profiler(n)%u(k)%qc = fails_error_max
                   iv%profiler(n)%v(k)%qc = fails_error_max
                   iv%profiler(n)%u(k)%inv = 0.0
                   iv%profiler(n)%v(k)%inv = 0.0
                endif
             endif
          end if

      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_profiler")

end subroutine da_check_max_iv_profiler
