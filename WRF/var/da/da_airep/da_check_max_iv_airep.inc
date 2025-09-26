subroutine da_check_max_iv_airep(iv, it, num_qcstat_conv)            

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: k,n,ipr
   logical :: failed,failed1,failed2
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_airep")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(airep)%n1,iv%info(airep)%n2
      do k = 1, iv%info(airep)%levels(n)
         call da_get_print_lvl(iv%airep(n)%p(k),ipr)

         if(.not. qc_rej_both)then
             if(wind_sd_airep)then
               failed=.false.
               if( iv%airep(n)%u(k)%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%u(k), max_error_spd,failed)
                   if( iv%info(airep)%proc_domain(k,n) ) then
                       num_qcstat_conv(1,airep,1,ipr) = num_qcstat_conv(1,airep,1,ipr) + 1
                       if(failed) then
                          num_qcstat_conv(2,airep,1,ipr) = num_qcstat_conv(2,airep,1,ipr) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'airep',ob_vars(1),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                          end if
                       end if
                   end if
                end if

                failed=.false.
                if( iv%airep(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%v(k), max_error_dir,failed)
                    if( iv%info(airep)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,airep,2,ipr) = num_qcstat_conv(1,airep,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,airep,2,ipr) = num_qcstat_conv(2,airep,2,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'airep',ob_vars(2),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                           end if
                        end if
                    end if
                end if

             else

                failed=.false.
                if( iv%airep(n)%u(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%u(k), max_error_uv,failed)
                    if( iv%info(airep)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,airep,1,ipr) = num_qcstat_conv(1,airep,1,ipr) + 1
                        if(failed) then
                           num_qcstat_conv(2,airep,1,ipr) = num_qcstat_conv(2,airep,1,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'airep',ob_vars(1),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                           end if
                        end if
                    end if
                end if

                failed=.false.
                if( iv%airep(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%v(k), max_error_uv,failed)
                    if( iv%info(airep)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,airep,2,ipr) = num_qcstat_conv(1,airep,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,airep,2,ipr) = num_qcstat_conv(2,airep,2,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'airep',ob_vars(2),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                           end if
                        end if
                    end if
                 end if
             end if

             if(wind_sd_airep)then
                if(iv%airep(n)%u(k)%qc == fails_error_max .or. abs(iv%airep(n)%u(k)%inv) >= max_omb_spd) then 
                   iv%airep(n)%u(k)%qc = fails_error_max
                   iv%airep(n)%u(k)%inv = 0.0
                endif
                if(iv%airep(n)%v(k)%qc == fails_error_max .or. abs(iv%airep(n)%v(k)%inv) >= max_omb_dir) then
                   iv%airep(n)%v(k)%qc = fails_error_max
                   iv%airep(n)%v(k)%inv = 0.0
                endif
             endif

          else

             failed1=.false.
             failed2=.false.

             if( iv%airep(n)%v(k)%qc >= obs_qc_pointer .or. iv%airep(n)%u(k)%qc >= obs_qc_pointer )  then
                 if(wind_sd_airep)then
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%u(k), max_error_spd,failed1)
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%v(k), max_error_dir,failed2)
                 else
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%u(k), max_error_uv,failed1)
                    call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%v(k), max_error_uv,failed2)
                 endif
             endif

             if( iv%info(airep)%proc_domain(k,n) ) then
                 num_qcstat_conv(1,airep,1,ipr) = num_qcstat_conv(1,airep,1,ipr) + 1
                 num_qcstat_conv(1,airep,2,ipr) = num_qcstat_conv(1,airep,2,ipr) + 1

                 if(failed1 .or. failed2) then
                    num_qcstat_conv(2,airep,1,ipr) = num_qcstat_conv(2,airep,1,ipr) + 1
                    if ( write_rej_obs_conv ) then
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'airep',ob_vars(1),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                    end if
                    num_qcstat_conv(2,airep,2,ipr) = num_qcstat_conv(2,airep,2,ipr) + 1
                    if ( write_rej_obs_conv ) then
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'airep',ob_vars(2),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
                    end if
                 endif
             endif

             if(wind_sd_airep)then
                if(iv%airep(n)%u(k)%qc == fails_error_max .or. iv%airep(n)%v(k)%qc == fails_error_max .or. &
                   abs(iv%airep(n)%v(k)%inv) >= max_omb_dir .or. abs(iv%airep(n)%u(k)%inv) >= max_omb_spd )then
                   iv%airep(n)%u(k)%qc = fails_error_max
                   iv%airep(n)%v(k)%qc = fails_error_max
                   iv%airep(n)%u(k)%inv = 0.0
                   iv%airep(n)%v(k)%inv = 0.0
                endif
             else
                if(iv%airep(n)%u(k)%qc == fails_error_max .or. iv%airep(n)%v(k)%qc == fails_error_max ) then
                   iv%airep(n)%u(k)%qc = fails_error_max
                   iv%airep(n)%v(k)%qc = fails_error_max
                   iv%airep(n)%u(k)%inv = 0.0
                   iv%airep(n)%v(k)%inv = 0.0
                endif
             endif
          endif

        failed=.false.
        if( iv%airep(n)%t(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%t(k), max_error_t,failed)
        if( iv%info(airep)%proc_domain(k,n) ) then
         num_qcstat_conv(1,airep,3,ipr) = num_qcstat_conv(1,airep,3,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,airep,3,ipr) = num_qcstat_conv(2,airep,3,ipr) + 1
           if ( write_rej_obs_conv ) then
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'airep',ob_vars(3),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
           end if
         end if
        end if
        end if

        failed=.false.
        if( iv%airep(n)%q(k)%qc >= obs_qc_pointer ) then
        if( iv%airep(n)%t(k)%qc == fails_error_max ) then
           failed=.true.
           iv%airep(n)%q(k)%qc  = fails_error_max
           iv%airep(n)%q(k)%inv = 0.0
        else
           call da_max_error_qc (it,iv%info(airep), n, iv%airep(n)%q(k), max_error_q ,failed)
        endif
        if( iv%info(airep)%proc_domain(k,n) ) then
           num_qcstat_conv(1,airep,4,ipr) = num_qcstat_conv(1,airep,4,ipr) + 1
           if(failed) then
           num_qcstat_conv(2,airep,4,ipr) = num_qcstat_conv(2,airep,4,ipr) + 1
           if ( write_rej_obs_conv ) then
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'airep',ob_vars(4),iv%info(airep)%lat(k,n),iv%info(airep)%lon(k,n),0.01*iv%airep(n)%p(k)
           end if
           end if
        end if
        end if
      end do
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_airep")

end subroutine da_check_max_iv_airep
