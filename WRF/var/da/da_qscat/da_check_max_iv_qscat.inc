subroutine da_check_max_iv_qscat(iv, it, num_qcstat_conv)        

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   logical :: failed,failed1,failed2
   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_qscat")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(qscat)%n1,iv%info(qscat)%n2
      if(.not. qc_rej_both)then
         if(wind_sd_qscat)then
            failed=.false.
            if( iv%qscat(n)%u%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%u, max_error_spd,failed)
                if( iv%info(qscat)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,qscat,1,1) = num_qcstat_conv(1,qscat,1,1) + 1
                    if(failed) then
                       num_qcstat_conv(2,qscat,1,1) = num_qcstat_conv(2,qscat,1,1) + 1
                       if ( write_rej_obs_conv ) then
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                       'qscat',ob_vars(1),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                       end if
                    end if
                end if
            end if

            failed=.false.
            if( iv%qscat(n)%v%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%v, max_error_dir,failed)
                if( iv%info(qscat)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,qscat,2,1) = num_qcstat_conv(1,qscat,2,1) + 1
                    if(failed)then
                       num_qcstat_conv(2,qscat,2,1) = num_qcstat_conv(2,qscat,2,1) + 1
                       if ( write_rej_obs_conv ) then
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                             'qscat',ob_vars(2),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                       end if
                    end if
                end if
            end if

         else
            failed=.false.
            if( iv%qscat(n)%u%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%u, max_error_uv,failed)
                if( iv%info(qscat)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,qscat,1,1) = num_qcstat_conv(1,qscat,1,1) + 1
                    if(failed) then
                       num_qcstat_conv(2,qscat,1,1) = num_qcstat_conv(2,qscat,1,1) + 1
                       if ( write_rej_obs_conv ) then
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                             'qscat',ob_vars(1),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                       end if
                    end if
                end if
            end if
            failed=.false.
            if( iv%qscat(n)%v%qc >= obs_qc_pointer ) then
                call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%v, max_error_uv,failed)
                if( iv%info(qscat)%proc_domain(1,n) ) then
                    num_qcstat_conv(1,qscat,2,1) = num_qcstat_conv(1,qscat,2,1) + 1
                    if(failed)then
                       num_qcstat_conv(2,qscat,2,1) = num_qcstat_conv(2,qscat,2,1) + 1
                       if ( write_rej_obs_conv ) then
                       write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                             'qscat',ob_vars(2),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                       end if
                    end if
                end if
             end if
          end if

          if(wind_sd_qscat)then
             if(iv%qscat(n)%u%qc == fails_error_max .or. abs(iv%qscat(n)%u%inv) >= max_omb_spd) then
                iv%qscat(n)%u%qc = fails_error_max
                iv%qscat(n)%u%inv = 0.0
             endif
             if(iv%qscat(n)%v%qc == fails_error_max .or. abs(iv%qscat(n)%v%inv) >= max_omb_dir) then
                iv%qscat(n)%v%qc = fails_error_max
                iv%qscat(n)%v%inv = 0.0
             endif
          endif

       else

          failed1=.false.
          failed2=.false.

          if( iv%qscat(n)%v%qc >= obs_qc_pointer .or. iv%qscat(n)%u%qc >= obs_qc_pointer )  then
              if(wind_sd_qscat)then
                 call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%u, max_error_spd,failed1)
                 call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%v, max_error_dir,failed2)
              else
                 call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%u, max_error_uv,failed1)
                 call da_max_error_qc (it,iv%info(qscat), n, iv%qscat(n)%v, max_error_uv,failed2)
              endif

              if( iv%info(qscat)%proc_domain(1,n) ) then
                  num_qcstat_conv(1,qscat,1,1) = num_qcstat_conv(1,qscat,1,1) + 1
                  num_qcstat_conv(1,qscat,2,1) = num_qcstat_conv(1,qscat,2,1) + 1
 
                  if(failed1 .or. failed2) then
                     num_qcstat_conv(2,qscat,1,1) = num_qcstat_conv(2,qscat,1,1) + 1
                     if ( write_rej_obs_conv ) then
                     write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                           'qscat',ob_vars(1),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                     end if
                     num_qcstat_conv(2,qscat,2,1) = num_qcstat_conv(2,qscat,2,1) + 1
                     if ( write_rej_obs_conv ) then
                     write(qcstat_conv_unit,'(2x,a10,2x,a4,2f12.2,a12)')&
                           'qscat',ob_vars(2),iv%info(qscat)%lat(1,n),iv%info(qscat)%lon(1,n),'1013.25'
                     end if
                  end if
              end if
          end if

	  if(wind_sd_qscat)then
             if(iv%qscat(n)%u%qc == fails_error_max .or. iv%qscat(n)%v%qc == fails_error_max .or. &
                abs(iv%qscat(n)%v%inv) >= max_omb_dir .or. abs(iv%qscat(n)%u%inv) >= max_omb_spd )then
                iv%qscat(n)%u%qc = fails_error_max
                iv%qscat(n)%v%qc = fails_error_max
                iv%qscat(n)%u%inv = 0.0
                iv%qscat(n)%v%inv = 0.0
             endif
          else
             if(iv%qscat(n)%u%qc == fails_error_max .or. iv%qscat(n)%v%qc == fails_error_max ) then
                iv%qscat(n)%u%qc = fails_error_max
                iv%qscat(n)%v%qc = fails_error_max
                iv%qscat(n)%u%inv = 0.0
                iv%qscat(n)%v%inv = 0.0
             endif
         endif
      endif

    end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_qscat")

end subroutine da_check_max_iv_qscat


