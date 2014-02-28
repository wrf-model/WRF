subroutine da_set_3d_obs_missing (iv, n)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: n    ! obs index

   integer :: i, k
   real    :: xmiss

   if (trace_use) call da_trace_entry("da_set_obs_missing")

   xmiss = -888.0

   select case (n)
   case (radar)
      do i = 1, iv%info(n)%nlocal
        do k=1,iv%info(n)%levels(i)
         if ( iv%info(n)%thinned(k,i) ) then
            iv%radar(i)%height(k) = missing_r
            iv%radar(i)%height_qc(k) = missing_data
            iv%radar(i)%rv(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%radar(i)%rf(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
        end do
      end do
   case (airep)
      do i = 1, iv%info(n)%nlocal
        do k=1,iv%info(n)%levels(i)
         if ( iv%info(n)%thinned(k,i) ) then
            iv%airep(i)%h(k) = missing_r
            iv%airep(i)%p(k) = missing_r
            iv%airep(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%t(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%airep(i)%q(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
        end do
      end do
   case (tamdar)
      do i = 1, iv%info(n)%nlocal
        do k=1,iv%info(n)%levels(i)
         if ( iv%info(n)%thinned(k,i) ) then
            iv%tamdar(i)%h(k) = missing_r
            iv%tamdar(i)%p(k) = missing_r
            iv%tamdar(i)%u(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar(i)%v(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar(i)%t(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar(i)%q(k) = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
        end do
      end do
   case (tamdar_sfc)
      do i = 1, iv%info(n)%nlocal
         if ( iv%info(n)%thinned(1,i) ) then
            iv%tamdar_sfc(i)%h = missing_r
            iv%tamdar_sfc(i)%u = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar_sfc(i)%v = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar_sfc(i)%t = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar_sfc(i)%p = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
            iv%tamdar_sfc(i)%q = field_type(missing_r, missing_data, xmiss, missing_r, missing_r)
         end if
      end do
   case default
      write(unit=message(1),fmt='(A,I4)') 'Wrong obs_index= ',n
      call da_error(__FILE__,__LINE__,message(1:1))
   end select

   if (trace_use) call da_trace_exit("da_set_3d_obs_missing")
 
end subroutine da_set_3d_obs_missing


