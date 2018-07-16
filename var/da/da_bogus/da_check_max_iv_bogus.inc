subroutine da_check_max_iv_bogus(iv,ob, it, num_qcstat_conv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.



   integer :: k,n, ipr
   logical :: failed
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_bogus")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(bogus)%n1,iv%info(bogus)%n2
      do k = 1, iv%info(bogus)%levels(n)
         call da_get_print_lvl(iv%bogus(n)%p(k),ipr)

        failed=.false.
        if( iv%bogus(n)%u(k)%qc >= obs_qc_pointer ) then
         call da_max_error_qc (it,iv%info(bogus), n, iv%bogus(n)%u(k), max_error_buv,failed)
        if( iv%info(bogus)%proc_domain(k,n) ) then
           num_qcstat_conv(1,bogus,1,ipr) = num_qcstat_conv(1,bogus,1,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,bogus,1,ipr) = num_qcstat_conv(2,bogus,1,ipr) + 1
          if ( write_rej_obs_conv ) then
          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
          'bogus',ob_vars(1),iv%info(bogus)%lat(k,n),iv%info(bogus)%lon(k,n),0.01*iv%bogus(n)%p(k)
          end if
         end if
        end if
        end if

        failed=.false.
        if( iv%bogus(n)%v(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(bogus), n, iv%bogus(n)%v(k), max_error_buv,failed)
        if( iv%info(bogus)%proc_domain(k,n) ) then
          num_qcstat_conv(1,bogus,2,ipr) = num_qcstat_conv(1,bogus,2,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,bogus,2,ipr) = num_qcstat_conv(2,bogus,2,ipr) + 1
          if ( write_rej_obs_conv ) then
          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
          'bogus',ob_vars(2),iv%info(bogus)%lat(k,n),iv%info(bogus)%lon(k,n),0.01*iv%bogus(n)%p(k)
          end if
         end if
        end if
        end if

        failed=.false.
        if( iv%bogus(n)%t(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(bogus), n, iv%bogus(n)%t(k), max_error_bt ,failed)
        if( iv%info(bogus)%proc_domain(k,n) ) then
           num_qcstat_conv(1,bogus,3,ipr) = num_qcstat_conv(1,bogus,3,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,bogus,3,ipr) = num_qcstat_conv(2,bogus,3,ipr) + 1
          if ( write_rej_obs_conv ) then
          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
          'bogus',ob_vars(3),iv%info(bogus)%lat(k,n),iv%info(bogus)%lon(k,n),0.01*iv%bogus(n)%p(k)
          end if
         end if
        end if
        end if

        failed=.false.
        if( iv%bogus(n)%q(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(bogus), n, iv%bogus(n)%q(k), max_error_bq ,failed)
        if( iv%info(bogus)%proc_domain(k,n) ) then
           num_qcstat_conv(1,bogus,4,ipr) = num_qcstat_conv(1,bogus,4,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,bogus,4,ipr) = num_qcstat_conv(2,bogus,4,ipr) + 1
          if ( write_rej_obs_conv ) then
          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
          'bogus',ob_vars(4),iv%info(bogus)%lat(k,n),iv%info(bogus)%lon(k,n),0.01*iv%bogus(n)%p(k)
          end if
         end if
        end if
        end if

      end do 
      ! Sea Level Pressure

      if( iv%info(bogus)%proc_domain(1,n) ) then
        failed=.false.
        if( iv%bogus(n)%slp%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(bogus), n, iv%bogus(n)%slp, max_error_slp ,failed)
           num_qcstat_conv(1,bogus,5,1) = num_qcstat_conv(1,bogus,5,1) + 1
         if(failed) then
          num_qcstat_conv(2,bogus,5,1) = num_qcstat_conv(2,bogus,5,1) + 1
          if ( write_rej_obs_conv ) then
          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
          'bogus',ob_vars(5),iv%info(bogus)%lat(1,n),iv%info(bogus)%lon(1,n),ob%bogus(n)%slp
          end if
        endif
      endif
      endif

   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_bogus")

end subroutine da_check_max_iv_bogus
