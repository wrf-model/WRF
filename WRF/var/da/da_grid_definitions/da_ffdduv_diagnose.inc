subroutine da_ffdduv_diagnose(obs1, inv1, inc1, obs2, inv2, inc2, qc1, qc2, ID) 

   implicit none

   integer ,intent (in)    :: qc1, qc2, ID
   real    ,intent (inout) :: obs1, inv1, inc1, obs2, inv2, inc2

   real   :: uobs, uinv, uinc, uan, ufg, &
             vobs, vinv, vinc, van, vfg, & 
             spdobs, spdinv, spdinc, spdfg, spdan, &
             dirobs, dirinv, dirinc, dirfg, diran 

   if (trace_use) call da_trace_entry("da_ffdduv_diagnose")

   if (obs1 .gt. missing_r .and. obs2 .gt. missing_r) then

      select case (ID)

      case (convert_fd2uv);

         spdobs = obs1
         spdinv = inv1
         spdinc = inc1
         dirobs = obs2
         dirinv = inv2
         dirinc = inc2

         spdfg  = spdobs - spdinv
         spdan  = spdobs - spdinc
         dirfg  = dirobs - dirinv
         diran  = dirobs - dirinc

         call da_ffdduv_model(spdobs, dirobs, uobs, vobs, convert_fd2uv)
         call da_ffdduv_model(spdfg,  dirfg,  ufg,  vfg,  convert_fd2uv)
         call da_ffdduv_model(spdan,  diran,  uan,  van,  convert_fd2uv)

         uinv = uobs - ufg
         uinc = uobs - uan
         vinv = vobs - vfg
         vinc = vobs - van

         obs1 = uobs
         inv1 = uinv
         inc1 = uinc
         obs2 = vobs
         inv2 = vinv
         inc2 = vinc

      case (convert_uv2fd);

         uobs = obs1
         uinv = inv1
         uinc = inc1
         vobs = obs2
         vinv = inv2
         vinc = inc2

         ufg  = uobs - uinv
         uan  = uobs - uinc
         vfg  = vobs - vinv
         van  = vobs - vinc

         call da_ffdduv_model(spdobs, dirobs, uobs, vobs, convert_uv2fd)
         call da_ffdduv_model(spdfg,  dirfg,  ufg,  vfg,  convert_uv2fd)
         call da_ffdduv_model(spdan,  diran,  uan,  van,  convert_uv2fd)

         spdinv = spdobs - spdfg
         spdinc = spdobs - spdan
         dirinv = dirobs - dirfg
         dirinc = dirobs - diran
             
         if (dirinv > 180.0) dirinv = dirinv - 360.0
         if (dirinc > 180.0) dirinc = dirinc - 360.0
         if (dirinv < -180.0) dirinv = dirinv + 360.0
         if (dirinc < -180.0) dirinc = dirinc + 360.0

         obs1 = spdobs
         inv1 = spdinv
         inc1 = spdinc
         obs2 = dirobs
         inv2 = dirinv
         inc2 = dirinc

      case default
         write(unit=message(1),fmt='(A,I2)') ' UNKNOWN OPTION ',ID
         call da_error(__FILE__,__LINE__,message(1:1))

      end select

   else

      obs1 = missing_r
      inv1 = 0.0
      inc1 = 0.0
      obs2 = missing_r
      inv2 = 0.0
      inc2 = 0.0

   end if

   if (trace_use) call da_trace_exit("da_ffdduv_diagnose")

end subroutine da_ffdduv_diagnose
