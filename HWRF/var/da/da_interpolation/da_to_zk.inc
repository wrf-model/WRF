subroutine da_to_zk(obs_v, mdl_v, v_interp_optn, zk)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,                  intent(in)  :: v_interp_optn
   real,                     intent(in)  :: obs_v
   real, dimension(kms:kme), intent(in)  :: mdl_v
   real,                     intent(out) :: zk

   integer                :: k

   if (trace_use_dull) call da_trace_entry("da_to_zk")

   zk = missing_r

   if (v_interp_optn == v_interp_p) then
      if (obs_v > mdl_v(kts) .or. obs_v < mdl_v(kte)) then
         if (anal_type_verify) then
            ! Guo (02/06/2006), for VERifY, allow the extrapolation to obtain the zk:
            if (obs_v > mdl_v(kts)) then
               ! below the lowest level:
               zk = real(kts+1) - &
                  (obs_v-mdl_v(kts+1))/(mdl_v(kts)-mdl_v(kts+1))
            else if (obs_v < mdl_v(kte)) then
               ! above the highest level:
               zk = real(kte-1) + &
                  (obs_v-mdl_v(kte-1))/(mdl_v(kte)-mdl_v(kte-1))
            end if
         else
            if (trace_use_dull) call da_trace_exit("da_to_zk")
            return
         end if
      else
         do k = kts,kte-1
            if(obs_v <= mdl_v(k) .and. obs_v >= mdl_v(k+1)) then
               zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
               exit
            end if
         end do
      end if
   else if(v_interp_optn == v_interp_h) then
      if (obs_v < mdl_v(kts) .or. obs_v > mdl_v(kte)) then
         if (anal_type_verify) then
            ! Guo (02/06/2006), for VERifY, allow the extrapolation to obtain the zk:
            if (obs_v < mdl_v(kts)) then
               ! below the lowest level:
               zk = real(kts+1) - &
                  (obs_v-mdl_v(kts+1))/(mdl_v(kts)-mdl_v(kts+1))
            else if (obs_v > mdl_v(kte)) then
               ! above the highest level:
               zk = real(kte-1) + &
                  (obs_v-mdl_v(kte-1))/(mdl_v(kte)-mdl_v(kte-1))
            end if
         else
            if (trace_use_dull) call da_trace_exit("da_to_zk")
            return
         end if
      else
         do k = kts,kte-1
            if(obs_v >= mdl_v(k) .and. obs_v <= mdl_v(k+1)) then
               zk = real(k) + (mdl_v(k) - obs_v)/(mdl_v(k) - mdl_v(k+1))
               exit
            end if
         end do
      end if
   end if

   if (trace_use_dull) call da_trace_exit("da_to_zk")

end subroutine da_to_zk


