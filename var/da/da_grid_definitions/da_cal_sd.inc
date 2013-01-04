subroutine da_cal_sd (spobs, dirobs, uinv, vinv, spinv, dirinv)

   implicit none
   real     :: uobs, vobs, uinv, vinv, &  
               spobs, dirobs, spinv, dirinv 
   real :: ufg, vfg, spfg, dirfg, spana, dirana

   uinv = 0
   vinv = 0

   spfg = spobs - spinv
   dirfg = dirobs - dirinv

   call da_ffdduv2(spfg, dirfg, ufg, vfg, -50.0, convert_fd2uv)
   call da_ffdduv2(spobs, dirobs, uobs, vobs, -50.0, convert_fd2uv)
   uinv = uobs - ufg
   vinv = vobs - vfg

end subroutine da_cal_sd
