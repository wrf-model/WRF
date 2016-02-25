subroutine da_check_max_iv_sound(iv, it,num_qcstat_conv)

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

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_sound")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(sound)%n1,iv%info(sound)%n2
      do k = 1, iv%info(sound)%levels(n)
         call da_get_print_lvl(iv%sound(n)%p(k),ipr) 

         if(.not. qc_rej_both)then
            if(wind_sd_sound)then
               failed=.false.
               if( iv%sound(n)%u(k)%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%u(k), max_error_spd,failed)
                   if( iv%info(sound)%proc_domain(k,n) ) then
                       num_qcstat_conv(1,sound,1,ipr) = num_qcstat_conv(1,sound,1,ipr) + 1
                       if(failed) then
                          num_qcstat_conv(2,sound,1,ipr) = num_qcstat_conv(2,sound,1,ipr) + 1
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'sound',ob_vars(1),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                       end if
                   end if
                end if

                failed=.false.
                if( iv%sound(n)%v(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%v(k), max_error_dir,failed)
                    if( iv%info(sound)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,sound,2,ipr) = num_qcstat_conv(1,sound,2,ipr) + 1
                        if(failed)then
                           num_qcstat_conv(2,sound,2,ipr) = num_qcstat_conv(2,sound,2,ipr) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'sound',ob_vars(2),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                        end if
                    end if
                end if
             else
                failed=.false.
                if( iv%sound(n)%u(k)%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%u(k), max_error_uv,failed)
                    if( iv%info(sound)%proc_domain(k,n) ) then
                        num_qcstat_conv(1,sound,1,ipr) = num_qcstat_conv(1,sound,1,ipr) + 1
                        if(failed) then
                           num_qcstat_conv(2,sound,1,ipr) = num_qcstat_conv(2,sound,1,ipr) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'sound',ob_vars(1),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                        end if
                    end if
                 end if

                 failed=.false.
                 if( iv%sound(n)%v(k)%qc >= obs_qc_pointer ) then
                     call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%v(k), max_error_uv,failed)
                     if( iv%info(sound)%proc_domain(k,n) ) then
                         num_qcstat_conv(1,sound,2,ipr) = num_qcstat_conv(1,sound,2,ipr) + 1
                         if(failed)then
                            num_qcstat_conv(2,sound,2,ipr) = num_qcstat_conv(2,sound,2,ipr) + 1
                            write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                            'sound',ob_vars(2),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                         end if
                     end if
                 end if
              end if

              if(wind_sd_sound)then
                 if(iv%sound(n)%u(k)%qc == fails_error_max .or. abs(iv%sound(n)%u(k)%inv) >= max_omb_spd) then
                    iv%sound(n)%u(k)%qc = fails_error_max
                    iv%sound(n)%u(k)%inv = 0.0
                 endif
                 if(iv%sound(n)%v(k)%qc == fails_error_max .or. abs(iv%sound(n)%v(k)%inv) >= max_omb_dir) then
                    iv%sound(n)%v(k)%qc = fails_error_max
                    iv%sound(n)%v(k)%inv = 0.0
                 endif
              endif
           else
              failed1=.false.
              failed2=.false.

              if( iv%sound(n)%v(k)%qc >= obs_qc_pointer .or. iv%sound(n)%u(k)%qc >= obs_qc_pointer )  then
                  if(wind_sd_sound)then
                     call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%u(k), max_error_spd,failed1)
                     call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%v(k), max_error_dir,failed2)
                  else
                     call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%u(k), max_error_uv,failed1)
                     call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%v(k), max_error_uv,failed2)
                  endif
              endif

              if( iv%info(sound)%proc_domain(k,n) ) then
                  num_qcstat_conv(1,sound,1,ipr) = num_qcstat_conv(1,sound,1,ipr) + 1
                  num_qcstat_conv(1,sound,2,ipr) = num_qcstat_conv(1,sound,2,ipr) + 1

                  if(failed1 .or. failed2) then
                     num_qcstat_conv(2,sound,1,ipr) = num_qcstat_conv(2,sound,1,ipr) + 1
                     write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'sound',ob_vars(1),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                    num_qcstat_conv(2,sound,2,ipr) = num_qcstat_conv(2,sound,2,ipr) + 1
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'sound',ob_vars(2),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
                 endif
              endif

              if(wind_sd_sound)then
                 if(iv%sound(n)%u(k)%qc == fails_error_max .or. iv%sound(n)%v(k)%qc == fails_error_max .or. &
                    abs(iv%sound(n)%v(k)%inv) >= max_omb_dir .or. abs(iv%sound(n)%u(k)%inv) >= max_omb_spd )then
                    iv%sound(n)%u(k)%qc = fails_error_max
                    iv%sound(n)%v(k)%qc = fails_error_max
                    iv%sound(n)%u(k)%inv = 0.0
                    iv%sound(n)%v(k)%inv = 0.0
                 endif
              else
                 if(iv%sound(n)%u(k)%qc == fails_error_max .or. iv%sound(n)%v(k)%qc == fails_error_max ) then
                    iv%sound(n)%u(k)%qc = fails_error_max
                    iv%sound(n)%v(k)%qc = fails_error_max
                    iv%sound(n)%u(k)%inv = 0.0
                    iv%sound(n)%v(k)%inv = 0.0
                 endif
              endif
           endif


         failed=.false.
         if( iv%sound(n)%t(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%t(k), max_error_t ,failed)
         if( iv%info(sound)%proc_domain(k,n) ) then
                    num_qcstat_conv(1,sound,3,ipr) = num_qcstat_conv(1,sound,3,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,sound,3,ipr) = num_qcstat_conv(2,sound,3,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'sound',ob_vars(3),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
         end if
         end if
         end if

         failed=.false.
         if( iv%sound(n)%q(k)%qc >= obs_qc_pointer ) then 
          if( iv%sound(n)%t(k)%qc == fails_error_max ) then
          failed=.true.
          iv%sound(n)%q(k)%qc  = fails_error_max
          iv%sound(n)%q(k)%inv = 0.0
          else
          call da_max_error_qc (it,iv%info(sound), n, iv%sound(n)%q(k), max_error_q ,failed)
          endif
         if( iv%info(sound)%proc_domain(k,n) ) then
                    num_qcstat_conv(1,sound,4,ipr) = num_qcstat_conv(1,sound,4,ipr) + 1
         if(failed) then
         num_qcstat_conv(2,sound,4,ipr) = num_qcstat_conv(2,sound,4,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'sound',ob_vars(4),iv%info(sound)%lat(k,n),iv%info(sound)%lon(k,n),0.01*iv%sound(n)%p(k)
         end if
         end if
         end if

      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_sound")

end subroutine da_check_max_iv_sound
