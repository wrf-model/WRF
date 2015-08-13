subroutine da_check_max_iv_ships(iv,ob, it, num_qcstat_conv)

   !-------------------------------------------------------------------------
   ! Purpose: TBD
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)
   type(y_type),  intent(in)    :: ob      ! Observation structure.


   logical :: failed,failed1,failed2
   integer :: n

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_ships")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(ships)%n1,iv%info(ships)%n2
         if(.not. qc_rej_both)then
            if(wind_sd_ships)then
               failed=.false.
               if( iv%ships(n)%u%qc >= obs_qc_pointer ) then
                   call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%u, max_error_spd,failed)
                   if( iv%info(ships)%proc_domain(1,n) ) then
                       num_qcstat_conv(1,ships,1,1) = num_qcstat_conv(1,ships,1,1) + 1
                       if(failed) then
                          num_qcstat_conv(2,ships,1,1) = num_qcstat_conv(2,ships,1,1) + 1
                          write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'ships',ob_vars(1),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                       end if
                   end if
                end if

                failed=.false.
                if( iv%ships(n)%v%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%v, max_error_dir,failed)
                    if( iv%info(ships)%proc_domain(1,n) ) then
                        num_qcstat_conv(1,ships,2,1) = num_qcstat_conv(1,ships,2,1) + 1
                        if(failed)then
                           num_qcstat_conv(2,ships,2,1) = num_qcstat_conv(2,ships,2,1) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'ships',ob_vars(2),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                        end if
                    end if
                end if

             else

                failed=.false.
                if( iv%ships(n)%u%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%u, max_error_uv,failed)
                    if( iv%info(ships)%proc_domain(1,n) ) then
                        num_qcstat_conv(1,ships,1,1) = num_qcstat_conv(1,ships,1,1) + 1
                        if(failed) then
                           num_qcstat_conv(2,ships,1,1) = num_qcstat_conv(2,ships,1,1) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'ships',ob_vars(1),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                        end if
                    end if
                end if
                failed=.false.
                if( iv%ships(n)%v%qc >= obs_qc_pointer ) then
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%v, max_error_uv,failed)
                    if( iv%info(ships)%proc_domain(1,n) ) then
                        num_qcstat_conv(1,ships,2,1) = num_qcstat_conv(1,ships,2,1) + 1
                        if(failed)then
                           num_qcstat_conv(2,ships,2,1) = num_qcstat_conv(2,ships,2,1) + 1
                           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                           'ships',ob_vars(2),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                        end if
                    end if
                end if
             end if

             if(wind_sd_ships)then
                if(iv%ships(n)%u%qc == fails_error_max .or.  abs(iv%ships(n)%u%inv) >= max_omb_spd) then
                   iv%ships(n)%u%qc = fails_error_max
                   iv%ships(n)%u%inv = 0.0
                endif
                if(iv%ships(n)%v%qc == fails_error_max .or.  abs(iv%ships(n)%v%inv) >= max_omb_dir) then
                   iv%ships(n)%v%qc = fails_error_max
                   iv%ships(n)%v%inv = 0.0
                endif
             endif
          else
             failed1=.false.
             failed2=.false.
             if( iv%ships(n)%v%qc >= obs_qc_pointer .or. iv%ships(n)%u%qc >= obs_qc_pointer )  then
                 if(wind_sd_ships)then
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%u, max_error_spd,failed1)
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%v, max_error_dir,failed2)
                 else
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%u, max_error_uv,failed1)
                    call da_max_error_qc (it,iv%info(ships), n, iv%ships(n)%v, max_error_uv,failed2)
                 endif
             endif

             if( iv%info(ships)%proc_domain(1,n) ) then
                 num_qcstat_conv(1,ships,1,1) = num_qcstat_conv(1,ships,1,1) + 1
                 num_qcstat_conv(1,ships,2,1) = num_qcstat_conv(1,ships,2,1) + 1

                 if(failed1 .or. failed2) then
                    num_qcstat_conv(2,ships,1,1) = num_qcstat_conv(2,ships,1,1) + 1
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'ships',ob_vars(1),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                    num_qcstat_conv(2,ships,2,1) = num_qcstat_conv(2,ships,2,1) + 1
                    write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
                          'ships',ob_vars(2),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
                 endif
             endif

             if(wind_sd_ships)then
                if(iv%ships(n)%u%qc == fails_error_max .or. iv%ships(n)%v%qc == fails_error_max .or. &
                   abs(iv%ships(n)%v%inv) >= max_omb_dir .or. abs(iv%ships(n)%u%inv) >= max_omb_spd )then
                   iv%ships(n)%u%qc = fails_error_max
                   iv%ships(n)%v%qc = fails_error_max
                   iv%ships(n)%u%inv = 0.0
                   iv%ships(n)%v%inv = 0.0
                endif
             else
                if(iv%ships(n)%u%qc == fails_error_max .or. iv%ships(n)%v%qc == fails_error_max ) then
                   iv%ships(n)%u%qc = fails_error_max
                   iv%ships(n)%v%qc = fails_error_max
                   iv%ships(n)%u%inv = 0.0
                   iv%ships(n)%v%inv = 0.0
                endif
             endif
          endif

      failed=.false.
      if( iv%ships(n)%t%qc >= obs_qc_pointer )  then
      call da_max_error_qc (it, iv%info(ships), n, iv%ships(n)%t, max_error_t , failed)
      if( iv%info(ships)%proc_domain(1,n) ) then
      num_qcstat_conv(1,ships,3,1)= num_qcstat_conv(1,ships,3,1) + 1
      if(failed) then
      num_qcstat_conv(2,ships,3,1)= num_qcstat_conv(2,ships,3,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'ships',ob_vars(3),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%ships(n)%p%qc >= obs_qc_pointer ) then 
      call da_max_error_qc (it, iv%info(ships), n, iv%ships(n)%p, max_error_p , failed)         
      if( iv%info(ships)%proc_domain(1,n) ) then
      num_qcstat_conv(1,ships,5,1)= num_qcstat_conv(1,ships,5,1) + 1
      if(failed) then
      num_qcstat_conv(2,ships,5,1)= num_qcstat_conv(2,ships,5,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'ships',ob_vars(5),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
      end if
      end if
      end if

      failed=.false.
      if( iv%ships(n)%q%qc >= obs_qc_pointer ) then
       if( iv%ships(n)%t%qc == fails_error_max .or. iv%ships(n)%p%qc == fails_error_max) then
       failed=.true.  
       iv%ships(n)%q%qc  = fails_error_max
       iv%ships(n)%q%inv = 0.0
       else
       call da_max_error_qc (it, iv%info(ships), n, iv%ships(n)%q, max_error_q , failed)
       endif
      if( iv%info(ships)%proc_domain(1,n) ) then
      num_qcstat_conv(1,ships,4,1)= num_qcstat_conv(1,ships,4,1) + 1
      if(failed) then
      num_qcstat_conv(2,ships,4,1)= num_qcstat_conv(2,ships,4,1) + 1
      write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'ships',ob_vars(4),iv%info(ships)%lat(1,n),iv%info(ships)%lon(1,n),0.01*ob%ships(n)%p
      end if
      end if
      end if 

   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_ships")

end subroutine da_check_max_iv_ships


