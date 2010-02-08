subroutine da_check_max_iv_mtgirs(iv, it, num_qcstat_conv)

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
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_mtgirs")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n = iv%info(mtgirs)%n1,iv%info(mtgirs)%n2
    do k = 1, iv%info(mtgirs)%levels(n)
         call da_get_print_lvl(iv%mtgirs(n)%p(k),ipr)
         failed=.false.
         if( iv%mtgirs(n)%u(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc (it,iv%info(mtgirs), n, iv%mtgirs(n)%u(k), max_error_uv,failed)
         if( iv%info(mtgirs)%proc_domain(k,n) ) then
             num_qcstat_conv(1,mtgirs,1,ipr) = num_qcstat_conv(1,mtgirs,1,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,mtgirs,1,ipr) = num_qcstat_conv(2,mtgirs,1,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'mtgirs',ob_vars(1),iv%info(mtgirs)%lat(k,n),iv%info(mtgirs)%lon(k,n),0.01*iv%mtgirs(n)%p(k)
         end if
         end if
         end if

         failed=.false.
         if( iv%mtgirs(n)%v(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc (it,iv%info(mtgirs), n, iv%mtgirs(n)%v(k), max_error_uv,failed)
         if( iv%info(mtgirs)%proc_domain(k,n) ) then
             num_qcstat_conv(1,mtgirs,2,ipr) = num_qcstat_conv(1,mtgirs,2,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,mtgirs,2,ipr) = num_qcstat_conv(2,mtgirs,2,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'mtgirs',ob_vars(2),iv%info(mtgirs)%lat(k,n),iv%info(mtgirs)%lon(k,n),0.01*iv%mtgirs(n)%p(k)
         end if
         end if
         end if

         failed=.false.
         if( iv%mtgirs(n)%t(k)%qc >= obs_qc_pointer )  then
         call da_max_error_qc (it,iv%info(mtgirs), n, iv%mtgirs(n)%t(k), max_error_t ,failed)
         if( iv%info(mtgirs)%proc_domain(k,n) ) then
             num_qcstat_conv(1,mtgirs,3,ipr) = num_qcstat_conv(1,mtgirs,3,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,mtgirs,3,ipr) = num_qcstat_conv(2,mtgirs,3,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'mtgirs',ob_vars(3),iv%info(mtgirs)%lat(k,n),iv%info(mtgirs)%lon(k,n),0.01*iv%mtgirs(n)%p(k)
         end if
         end if
         end if

         failed=.false.
         if( iv%mtgirs(n)%q(k)%qc >= obs_qc_pointer )  then 
          if( iv%mtgirs(n)%t(k)%qc == fails_error_max ) then
          failed=.true.
          iv%mtgirs(n)%q(k)%qc  = fails_error_max 
          iv%mtgirs(n)%q(k)%inv = 0.0
          else
          call da_max_error_qc (it,iv%info(mtgirs), n, iv%mtgirs(n)%q(k), max_error_q ,failed)
          endif
         if( iv%info(mtgirs)%proc_domain(k,n) ) then
            num_qcstat_conv(1,mtgirs,4,ipr) = num_qcstat_conv(1,mtgirs,4,ipr) + 1
         if(failed) then
         num_qcstat_conv(2,mtgirs,4,ipr) = num_qcstat_conv(2,mtgirs,4,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'mtgirs',ob_vars(4),iv%info(mtgirs)%lat(k,n),iv%info(mtgirs)%lon(k,n),0.01*iv%mtgirs(n)%p(k)
         end if
         end if
         end if

    end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_mtgirs")

end subroutine da_check_max_iv_mtgirs
