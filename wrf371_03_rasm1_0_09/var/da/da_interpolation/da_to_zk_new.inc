subroutine da_to_zk_new(obs_v, mdl_v, v_interp_optn, num,nlevels,zk)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   real,             intent(in)  :: obs_v(nlevels)
   real,             intent(in)  :: mdl_v(kms:kme,num)
   integer,          intent(in)  :: v_interp_optn
   integer,          intent(in)  :: num
   integer,          intent(in)  :: nlevels
   real,             intent(out) :: zk(nlevels,num)

   integer                :: k,n,kk
   real    :: r_ktsplus, r_kteminus

   if (trace_use) call da_trace_entry("da_to_zk_new")

   zk(:,:) = missing_r

   r_ktsplus  = real(kts+1)
   r_kteminus = real(kte-1)

   if (v_interp_optn == v_interp_p) then
      if (anal_type_verify) then
         do n=1,num
            do k=1,nlevels
               if (obs_v(k) > mdl_v(kts,n)) then
                  ! below the lowest level:
                  zk(k,n) = r_ktsplus - &
                     (obs_v(k)-mdl_v(kts+1,n))/(mdl_v(kts,n)-mdl_v(kts+1,n))
               else if (obs_v(k) < mdl_v(kts,n)) then
                  ! above the highest level:
                  zk(k,n) = r_kteminus + &
                     (obs_v(k)-mdl_v(kte-1,n))/(mdl_v(kte,n)-mdl_v(kte-1,n))
               else
                  do kk = kts,kte-1
                     if (obs_v(k) <= mdl_v(kk,n) .and. obs_v(k) >= mdl_v(kk+1,n)) then
                        zk(k,n) = real(kk) + (mdl_v(kk,n) - obs_v(k))/(mdl_v(kk,n) - mdl_v(kk+1,n))
                        exit
                     end if
                  end do
               end if
            end do
         end do
      else
         do n=1,num
            do k=1,nlevels
               do kk = kts,kte-1
                  if (obs_v(k) <= mdl_v(kk,n) .and. obs_v(k) >= mdl_v(kk+1,n)) then
                     zk(k,n) = real(kk) + (mdl_v(kk,n) - obs_v(k))/(mdl_v(kk,n) - mdl_v(kk+1,n))
                     exit
                  end if
               end do
            end do
         end do
      end if
   else if(v_interp_optn == v_interp_h) then
      if (anal_type_verify) then
         do n=1,num
            do k=1,nlevels
               if (obs_v(k) < mdl_v(kts,n)) then
                   ! below the lowest level:
                   zk(k,n) = r_ktsplus - &
                     (obs_v(k)-mdl_v(kts+1,n))/(mdl_v(kts,n)-mdl_v(kts+1,n))
               else if (obs_v(k) > mdl_v(kts,n)) then
                  ! above the highest level:
                  zk(k,n) = r_kteminus + &
                     (obs_v(k)-mdl_v(kte-1,n))/(mdl_v(kte,n)-mdl_v(kte-1,n))
               else
                  do kk = kts,kte-1
                     if (obs_v(k) >= mdl_v(kk,n) .and. obs_v(k) <= mdl_v(kk+1,n)) then
                        zk(k,n) = real(kk) + (mdl_v(kk,n) - obs_v(k))/(mdl_v(kk,n) - mdl_v(kk+1,n))
                        exit
                     end if
                  end do
               end if
            end do
         end do
      else
         do n=1,num
            do k=1,nlevels
               do kk = kts,kte-1
                  if (obs_v(k) >= mdl_v(kk,n) .and. obs_v(k) <= mdl_v(kk+1,n)) then
                     zk(k,n) = real(kk) + (mdl_v(kk,n) - obs_v(k))/(mdl_v(kk,n) - mdl_v(kk+1,n))
                     exit
                  end if
               end do
            end do
         end do
      end if
   end if

   if (trace_use) call da_trace_exit("da_to_zk_new")

end subroutine da_to_zk_new


