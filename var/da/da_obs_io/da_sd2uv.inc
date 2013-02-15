subroutine da_sd2uv (spobs, spinv, spinc, dirobs, dirinv, dirinc, uqc, &
                     uobs, uinv, uinc, vobs, vinv, vinc)
   integer  :: uqc
   real     :: uobs, uinv, uinc, & 
               vobs, vinv, vinc, &
               spobs, spinv, spinc, & 
               dirobs, dirinv, dirinc
   real :: uana, vana, ufg, vfg, spfg, dirfg, spana, dirana

   if (uqc >= 0) then
       spfg = spobs - spinv
       dirfg = dirobs - dirinv
       spana = spobs - spinc
       dirana = dirobs - dirinc

       call da_ffdduv2(spobs, dirobs, uobs, vobs, -98.0, convert_fd2uv)
       call da_ffdduv2(spfg, dirfg, ufg, vfg, -98.0, convert_fd2uv)
       call da_ffdduv2(spana, dirana, uana, vana, -98.0, convert_fd2uv)

       uinv = uobs - ufg
       vinv = vobs - vfg

       uinc = uobs - uana
       vinc = vobs - vana
  else
       uobs = -888888
       vobs = -888888
       uinv = 0.0
       vinv = 0.0
       uinc = 0.0
       vinc = 0.0
  end if

end subroutine da_sd2uv
