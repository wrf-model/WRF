subroutine da_check_max_iv_tamdar(iv, it,num_qcstat_conv)

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

   integer :: k,n,ipr
   logical :: failed,failed1,failed2

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_tamdar")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(tamdar)%n1,iv%info(tamdar)%n2
      do k = 1, iv%info(tamdar)%levels(n)
         call da_get_print_lvl(iv%tamdar(n)%p(k),ipr) 

         if(.not. qc_rej_both)then
            if(wind_sd_tamdar)then
               failed=.false.
               if( iv%tamdar(n)%u(k)%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%u(k), max_error_spd,failed)
                   if( iv%info(tamdar)%proc_domain(k,n) ) then
                       num_qcstat_conv(1,tamdar,1,ipr) = num_qcstat_conv(1,tamdar,1,ipr) + 1
                       if(failed) then
                          num_qcstat_conv(2,tamdar,1,ipr) = num_qcstat_conv(2,tamdar,1,ipr) + 1
                          if ( write_rej_obs_conv ) then
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'tamdar',ob_vars(1),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                          end if
                       end if
                   end if
                end if

                failed=.false.
                if( iv%tamdar(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%v(k), max_error_dir,failed)
                    if( iv%info(tamdar)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,tamdar,2,ipr) = num_qcstat_conv(1,tamdar,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,tamdar,2,ipr) = num_qcstat_conv(2,tamdar,2,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'tamdar',ob_vars(2),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                           end if
                        end if
                    end if
                end if
             else
                failed=.false.
                if( iv%tamdar(n)%u(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%u(k), max_error_uv,failed)
                    if( iv%info(tamdar)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,tamdar,1,ipr) = num_qcstat_conv(1,tamdar,1,ipr) + 1
                        if(failed) then
                           num_qcstat_conv(2,tamdar,1,ipr) = num_qcstat_conv(2,tamdar,1,ipr) + 1
                           if ( write_rej_obs_conv ) then
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'tamdar',ob_vars(1),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                           end if
                         end if
                    end if
                 end if

                 failed=.false.
                 if( iv%tamdar(n)%v(k)%qc >= obs_qc_pointer ) then
                     call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%v(k), max_error_uv,failed)
                     if( iv%info(tamdar)%proc_domain(k,n) ) then
                         num_qcstat_conv(1,tamdar,2,ipr) = num_qcstat_conv(1,tamdar,2,ipr) + 1
                         if(failed)then
                            num_qcstat_conv(2,tamdar,2,ipr) = num_qcstat_conv(2,tamdar,2,ipr) + 1
                            if ( write_rej_obs_conv ) then
                            write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                            'tamdar',ob_vars(2),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                            end if
                         end if
                     end if
                 end if
              end if

              if(wind_sd_tamdar)then
                 if(iv%tamdar(n)%u(k)%qc == fails_error_max .or. abs(iv%tamdar(n)%u(k)%inv) >= max_omb_spd) then
                    iv%tamdar(n)%u(k)%qc = fails_error_max
                    iv%tamdar(n)%u(k)%inv = 0.0
                 endif
                 if(iv%tamdar(n)%v(k)%qc == fails_error_max .or. abs(iv%tamdar(n)%v(k)%inv) >= max_omb_dir) then
                    iv%tamdar(n)%v(k)%qc = fails_error_max
                    iv%tamdar(n)%v(k)%inv = 0.0
                 endif
              endif
           else
              failed1=.false.
              failed2=.false.

              if( iv%tamdar(n)%v(k)%qc >= obs_qc_pointer .or. iv%tamdar(n)%u(k)%qc >= obs_qc_pointer )  then
                  if(wind_sd_tamdar)then
                     call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%u(k), max_error_spd,failed1)
                     call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%v(k), max_error_dir,failed2)
                  else
                     call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%u(k), max_error_uv,failed1)
                     call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%v(k), max_error_uv,failed2)
                  endif
              endif

              if( iv%info(tamdar)%proc_domain(k,n) ) then
                  num_qcstat_conv(1,tamdar,1,ipr) = num_qcstat_conv(1,tamdar,1,ipr) + 1
                  num_qcstat_conv(1,tamdar,2,ipr) = num_qcstat_conv(1,tamdar,2,ipr) + 1

                  if(failed1 .or. failed2) then
                     num_qcstat_conv(2,tamdar,1,ipr) = num_qcstat_conv(2,tamdar,1,ipr) + 1
                     if ( write_rej_obs_conv ) then
                     write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'tamdar',ob_vars(1),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                     end if
                     num_qcstat_conv(2,tamdar,2,ipr) = num_qcstat_conv(2,tamdar,2,ipr) + 1
                     if ( write_rej_obs_conv ) then
                     write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'tamdar',ob_vars(2),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
                     end if
                  endif
               endif

               if(wind_sd_tamdar)then
                  if(iv%tamdar(n)%u(k)%qc == fails_error_max .or. iv%tamdar(n)%v(k)%qc == fails_error_max .or. &
                     abs(iv%tamdar(n)%v(k)%inv) >= max_omb_dir .or. abs(iv%tamdar(n)%u(k)%inv) >= max_omb_spd )then
                     iv%tamdar(n)%u(k)%qc = fails_error_max
                     iv%tamdar(n)%v(k)%qc = fails_error_max
                     iv%tamdar(n)%u(k)%inv = 0.0
                     iv%tamdar(n)%v(k)%inv = 0.0
                  endif
               else
                  if(iv%tamdar(n)%u(k)%qc == fails_error_max .or. iv%tamdar(n)%v(k)%qc == fails_error_max ) then
                     iv%tamdar(n)%u(k)%qc = fails_error_max
                     iv%tamdar(n)%v(k)%qc = fails_error_max
                     iv%tamdar(n)%u(k)%inv = 0.0
                     iv%tamdar(n)%v(k)%inv = 0.0
                  endif
               endif
            endif

         failed=.false.
         if( iv%tamdar(n)%t(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%t(k), max_error_t ,failed)
         if( iv%info(tamdar)%proc_domain(k,n) ) then
             num_qcstat_conv(1,tamdar,3,ipr) = num_qcstat_conv(1,tamdar,3,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,tamdar,3,ipr) = num_qcstat_conv(2,tamdar,3,ipr) + 1
           if ( write_rej_obs_conv ) then
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'tamdar',ob_vars(3),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
           end if
         end if
         end if
         end if

         failed=.false.
         if( iv%tamdar(n)%q(k)%qc >= obs_qc_pointer ) then
          if( iv%tamdar(n)%t(k)%qc == fails_error_max ) then
          failed=.true.
          iv%tamdar(n)%q(k)%qc  = fails_error_max
          iv%tamdar(n)%q(k)%inv = 0.0
          else
          call da_max_error_qc (it,iv%info(tamdar), n, iv%tamdar(n)%q(k), max_error_q ,failed)
          endif
         if( iv%info(tamdar)%proc_domain(k,n) ) then
                    num_qcstat_conv(1,tamdar,4,ipr) = num_qcstat_conv(1,tamdar,4,ipr) + 1
         if(failed) then
         num_qcstat_conv(2,tamdar,4,ipr) = num_qcstat_conv(2,tamdar,4,ipr) + 1
           if ( write_rej_obs_conv ) then
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'tamdar',ob_vars(4),iv%info(tamdar)%lat(k,n),iv%info(tamdar)%lon(k,n),0.01*iv%tamdar(n)%p(k)
           end if
         end if
         end if
         end if

      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_tamdar")

end subroutine da_check_max_iv_tamdar
