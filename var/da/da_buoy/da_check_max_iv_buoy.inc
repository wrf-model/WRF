subroutine da_check_max_iv_buoy(iv,ob, it, num_qcstat_conv)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration 
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.



   logical :: failed 
   integer :: n
   
   if (trace_use_dull) call da_trace_entry("da_check_max_iv_buoy")       


   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(buoy)%n1,iv%info(buoy)%n2
      if( iv%buoy(n)%u%qc == fails_error_max .and. it > 1 )iv%buoy(n)%u%qc =0
      if( iv%buoy(n)%u%qc >= obs_qc_pointer ) then
         failed=.false.
         if( check_max_iv) &
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%u, max_error_uv, failed)
      if( iv%info(buoy)%proc_domain(1,n) ) then
      num_qcstat_conv(1,buoy,1,1)= num_qcstat_conv(1,buoy,1,1) + 1
      if(failed) then
      num_qcstat_conv(2,buoy,1,1)= num_qcstat_conv(2,buoy,1,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'buoy',ob_vars(1),iv%info(buoy)%lat(1,n),iv%info(buoy)%lon(1,n),0.01*ob%buoy(n)%p
      end if
      end if
      end if

      if( iv%buoy(n)%v%qc == fails_error_max .and. it > 1 )iv%buoy(n)%v%qc =0
      if( iv%buoy(n)%v%qc >= obs_qc_pointer ) then
      failed=.false.
      if( check_max_iv) &
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%v, max_error_uv, failed)             
      if( iv%info(buoy)%proc_domain(1,n) ) then
      num_qcstat_conv(1,buoy,2,1)= num_qcstat_conv(1,buoy,2,1) + 1
      if(failed) then
      num_qcstat_conv(2,buoy,2,1)= num_qcstat_conv(2,buoy,2,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'buoy',ob_vars(2),iv%info(buoy)%lat(1,n),iv%info(buoy)%lon(1,n),0.01*ob%buoy(n)%p
      end if
      end if
      end if

      if( iv%buoy(n)%t%qc == fails_error_max .and. it > 1 )iv%buoy(n)%t%qc =0
      if( iv%buoy(n)%t%qc >= obs_qc_pointer ) then
      failed=.false.
      if( check_max_iv) &
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%t, max_error_t , failed)
      if( iv%info(buoy)%proc_domain(1,n) ) then
      num_qcstat_conv(1,buoy,3,1)= num_qcstat_conv(1,buoy,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,buoy,3,1)= num_qcstat_conv(2,buoy,3,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'buoy',ob_vars(3),iv%info(buoy)%lat(1,n),iv%info(buoy)%lon(1,n),0.01*ob%buoy(n)%p
      end if
      end if
      end if

      if( iv%buoy(n)%q%qc == fails_error_max  .and. it > 1)iv%buoy(n)%q%qc =0
      if( iv%buoy(n)%q%qc >= obs_qc_pointer ) then
      failed=.false.
      if( check_max_iv) &
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%q, max_error_q , failed)
      if( iv%info(buoy)%proc_domain(1,n) ) then
      num_qcstat_conv(1,buoy,4,1)= num_qcstat_conv(1,buoy,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,buoy,4,1)= num_qcstat_conv(2,buoy,4,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'buoy',ob_vars(4),iv%info(buoy)%lat(1,n),iv%info(buoy)%lon(1,n),0.01*ob%buoy(n)%p
      end if
      end if
      end if

      if( iv%buoy(n)%p%qc == fails_error_max .and. it > 1 )iv%buoy(n)%p%qc =0
      if( iv%buoy(n)%p%qc >= obs_qc_pointer ) then
      failed=.false.
      if( check_max_iv) &
      call da_max_error_qc (it, iv%info(buoy), n, iv%buoy(n)%p, max_error_p , failed)         
      if( iv%info(buoy)%proc_domain(1,n) ) then
      num_qcstat_conv(1,buoy,5,1)= num_qcstat_conv(1,buoy,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,buoy,5,1)= num_qcstat_conv(2,buoy,5,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'buoy',ob_vars(5),iv%info(buoy)%lat(1,n),iv%info(buoy)%lon(1,n),0.01*ob%buoy(n)%p
      end if
      end if
      end if
   end do
   
   if (trace_use_dull) call da_trace_exit("da_check_max_iv_buoy")       

end subroutine da_check_max_iv_buoy
