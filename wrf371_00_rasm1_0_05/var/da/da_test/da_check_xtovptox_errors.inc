subroutine da_check_xtovptox_errors(xa, xa2_u, xa2_v, xa2_w, xa2_t, &
                                     xa2_p, xa2_q, xa2_rho, &
                                     xa2_qt, xa2_qcw, xa2_qrn)

   !---------------------------------------------------------------------------
   ! Purpose: Test invertibility of v = U^{-1} x followed by x = Uv.
   !
   !  Method:  Perform statistics on differences in initial and final x.
   !---------------------------------------------------------------------------

   implicit none
      
   type (x_type), intent(in)      :: xa          ! Test input

   real, dimension(ims:ime, jms:jme, kms:kme), &
                 intent(in)      :: xa2_u, xa2_v, xa2_t, &
                                    xa2_p, xa2_q, xa2_rho, &
                                    xa2_qt, xa2_qcw, xa2_qrn
   real, dimension(ims:ime, jms:jme, kms:kme), &
                 intent(in)      :: xa2_w    !xiao


   real                           :: rms_fild    ! RMS of field.
   real                           :: rms_diff    ! RMS of differnce.

   real, dimension(ims:ime, jms:jme, kms:kme) :: diff ! Difference

   if (trace_use) call da_trace_entry("da_check_xtovpx_errors")

   !----------------------------------------------------------------------
   ! [1.0]: Check u differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_u(its:ite, jts:jte, kts:kte) &
                                   - xa% u(its:ite, jts:jte, kts:kte)
   
   rms_fild = sqrt(sum(xa % u(its:ite, jts:jte, kts:kte) &
                       * xa % u(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' u is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' u RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' u RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' u RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if        
     
   !----------------------------------------------------------------------
   ! [2.0]: Check v differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_v(its:ite, jts:jte, kts:kte) &
                                   - xa% v(its:ite, jts:jte, kts:kte)
   
   rms_fild = sqrt(sum(xa % v(its:ite, jts:jte, kts:kte) &
                       * xa % v(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))
     
   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' v is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' v RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' v RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' v RMS error/RMS field = ', &
      rms_diff/rms_fild
   end if    
      
   !----------------------------------------------------------------------
   ! [3.0]: Check t differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_t(its:ite, jts:jte, kts:kte) &
                                   - xa% t(its:ite, jts:jte, kts:kte)

   rms_fild = sqrt(sum(xa % t(its:ite, jts:jte, kts:kte) &
                       * xa % t(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))

   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' t is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' t RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' t RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' t RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if         
        
   !----------------------------------------------------------------------
   ! [4.0]: Check p differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_p(its:ite, jts:jte, kts:kte) &
                                   - xa% p(its:ite, jts:jte, kts:kte)

   rms_fild = sqrt(sum(xa % p(its:ite, jts:jte, kts:kte) &
                       * xa % p(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))

   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' p is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' p RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' p RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' p RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if           

   !----------------------------------------------------------------------
   ! [5.0]: Check q differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_q(its:ite, jts:jte, kts:kte) &
                                   - xa% q(its:ite, jts:jte, kts:kte)

   rms_fild = sqrt(sum(xa % q(its:ite, jts:jte, kts:kte) &
                       * xa % q(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))

   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' q is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' q RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' q RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' q RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if        

   !----------------------------------------------------------------------
   ! [6.0]: Check rho differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte) = xa2_rho(its:ite, jts:jte, kts:kte) &
                                   - xa% rho(its:ite, jts:jte, kts:kte)

   rms_fild = sqrt(sum(xa % rho(its:ite, jts:jte, kts:kte) &
                       * xa % rho(its:ite, jts:jte, kts:kte)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte) &
                       * diff(its:ite, jts:jte, kts:kte)))

   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' rho is zero ' 
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' rho RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' rho RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' rho RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if        

   !----------------------------------------------------------------------
   ! [7.0]: Check w differences:
   !----------------------------------------------------------------------

   diff(its:ite, jts:jte, kts:kte+1) = xa2_w(its:ite, jts:jte, kts:kte+1) &
                                     - xa% w(its:ite, jts:jte, kts:kte+1)

   rms_fild = sqrt(sum(xa % w(its:ite, jts:jte, kts:kte+1) &
                       * xa % w(its:ite, jts:jte, kts:kte+1)))
   rms_diff = sqrt(sum(diff(its:ite, jts:jte, kts:kte+1) &
                       * diff(its:ite, jts:jte, kts:kte+1)))

   if (rms_fild == 0.0) then
      write(unit=stdout, fmt='(a)') ' w is zero '
   else
      write(unit=stdout, fmt='(a,1pe10.4)') ' w RMS error = ', rms_diff
      write(unit=stdout, fmt='(a,1pe10.4)') ' w RMS field = ', rms_fild
      write(unit=stdout, fmt='(a,1pe10.4)') ' w RMS error/RMS field = ', &
         rms_diff/rms_fild
   end if

   if (trace_use) call da_trace_exit("da_check_xtovpx_errors")
         
end subroutine da_check_xtovptox_errors


