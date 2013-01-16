subroutine da_check_max_iv_geoamv(iv, it, num_qcstat_conv)    

   !-------------------------------------------------------------------------
   ! Purpose: Innovation vector check for Geo. AMVs               
   ! Update:
   !    Removed Outerloop check as it is done in da_get_innov
   !    Author: Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 07/12/2009
   !-------------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   integer,       intent(inout) :: num_qcstat_conv(:,:,:,:)

   integer :: k,n, ipr
   logical :: failed

   if (trace_use_dull) call da_trace_entry("da_check_max_iv_geoamv")

   !---------------------------------------------------------------------------
   ! [1.0] Perform maximum innovation vector check:
   !---------------------------------------------------------------------------

   do n=iv%info(geoamv)%n1,iv%info(geoamv)%n2
      do k = 1, iv%info(geoamv)%levels(n)
         call da_get_print_lvl(iv%geoamv(n)%p(k),ipr)

         failed=.false.
         if( iv%geoamv(n)%u(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc(it, iv%info(geoamv), n, iv%geoamv(n)%u(k), max_error_uv,failed)
         if( iv%info(geoamv)%proc_domain(k,n) ) then
             num_qcstat_conv(1,geoamv,1,ipr) = num_qcstat_conv(1,geoamv,1,ipr) + 1
         if(failed) then
          num_qcstat_conv(2,geoamv,1,ipr) = num_qcstat_conv(2,geoamv,1,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'geoamv',ob_vars(1),iv%info(geoamv)%lat(k,n),iv%info(geoamv)%lon(k,n),0.01*iv%geoamv(n)%p(k)
         end if
         end if
         end if

         failed=.false.
         if( iv%geoamv(n)%v(k)%qc >= obs_qc_pointer ) then 
         call da_max_error_qc(it, iv%info(geoamv), n, iv%geoamv(n)%v(k), max_error_uv,failed)
         if( iv%info(geoamv)%proc_domain(k,n) ) then
             num_qcstat_conv(1,geoamv,2,ipr) = num_qcstat_conv(1,geoamv,2,ipr) + 1
         if(failed)then
          num_qcstat_conv(2,geoamv,2,ipr) = num_qcstat_conv(2,geoamv,2,ipr) + 1
           write(qcstat_conv_unit,'(2x,a10,2x,a4,3f12.2)')&
           'geoamv',ob_vars(2),iv%info(geoamv)%lat(k,n),iv%info(geoamv)%lon(k,n),0.01*iv%geoamv(n)%p(k)
         end if
         end if
         end if
      end do
   end do

   if (trace_use_dull) call da_trace_exit("da_check_max_iv_geoamv")

end subroutine da_check_max_iv_geoamv      

