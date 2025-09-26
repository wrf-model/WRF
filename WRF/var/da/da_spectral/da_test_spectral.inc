subroutine da_test_spectral (max_wave, sizec, xbx, field)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer,         intent(in) :: max_wave ! Maximum wavenumber.
   integer,         intent(in) :: sizec    ! Size of complex cv.
   type (xbx_type), intent(in) :: xbx  ! For header & non-grid arrays.
   real,            intent(in) :: field(its:ite,jts:jte)    ! Gridpoint field.

   real    :: field_out(its:ite,jts:jte)
   real*8  :: power(0:max_wave)   ! Power spectrum
   real    :: rcv(1:2*sizec)      ! Spectral modes.
   real    :: rcv_out(1:2*sizec)  ! Spectral modes.
   integer :: m,mm, index_start, index_end
   complex :: r_leg(1:jde)     
   complex :: ccv(1:sizec)     ! Spectral modes.
   complex :: ccv1(1:sizec)    ! Spectral modes.
   real    :: den, num, xx

   if (trace_use) call da_trace_entry("da_test_spectral")

   write (unit=stdout, fmt='(A)') &
      ' Test orthogonality of Associated Legendre Polynomials:'

   ! Initialise Power spectrum
   power  = 1.0
   call da_setlegpol_test( jde, max_wave, xbx%alp_size, xbx%int_wgts, xbx%alp )

   write(unit=stdout,fmt='(A)') &
      ' Test invertibility of spectral (Fourier, Legendre) transforms:'

   ! Gridpoint to spectral:
   rcv = 0.0
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, max_wave, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                             xbx%alp_size, 2*sizec, xbx%alp, xbx%wsave, xbx%int_wgts, rcv, field)

   field_out = 0.0
   ! Spectral to gridpoint:
   call da_vtovv_spectral( max_wave, sizec, &
                             xbx % lenr, xbx % lenwrk, xbx % lensav, &
                             xbx % inc, xbx % alp_size, xbx % alp, &
                             xbx % wsave, power, rcv, field_out)

   write(unit=stdout,fmt='(1x,a,e30.10)') &
      'Domain-Averaged (Grid->Spectral->Grid) Error = ', &
              sqrt( sum( ( field_out(1:xbx%ni,1:xbx%nj) - field(1:xbx%ni,1:xbx%nj) )**2 ) / &
                    sum( field(1:xbx%ni,1:xbx%nj)**2 ) )
   rcv_out = 0.0
   
   ! Gridpoint to spectral (again):
   call da_vv_to_v_spectral( xbx%ni, xbx%nj, max_wave, xbx%inc, xbx%lenr, xbx%lensav, xbx%lenwrk, &
                      xbx%alp_size, 2*sizec, xbx%alp, xbx%wsave, xbx%int_wgts, rcv_out, field_out)

   rcv_out(1:2*sizec) = rcv_out(1:2*sizec) - rcv(1:2*sizec) ! Create difference for test diags.
    
   write(unit=stdout,fmt='(1x,a,e30.10)') &
      ' Domain-Averaged (Spectral->Grid->Spectral) Error = ', &
                       sqrt( sum( rcv_out(1:2*sizec)**2 ) ) / sqrt( sum( rcv(1:2*sizec)**2 ) )

   ! Adjoint test for Spectral Transform
   rcv_out = 0.0
   call da_vtovv_spectral_adj( max_wave, sizec, &
                                 xbx % lenr, xbx % lenwrk, xbx % lensav, &
                                 xbx % inc, xbx % alp_size, xbx % alp, &
                                 xbx % wsave, power, rcv_out, field_out)

   write(unit=stdout,fmt='(A)') ' Adjoint test result for  Spectral -> Grid : '
   write(unit=stdout,fmt='(1x,a,e30.10)') &
      ' LHS  ( LX.LX)       = ',&
      sum( field_out(1:xbx%ni,1:xbx%nj)*field_out(1:xbx%ni,1:xbx%nj) ) 
   write(unit=stdout,fmt='(1x,a,e30.10)') &
      ' RHS  (  X.L^TLX )   = ', sum( rcv(1:2*sizec)*rcv_out(1:2*sizec) ) 

   ! Inverse test for Legendre Transform

   write(unit=stdout,fmt='(A)') '  Inverse and Adjoint Legendre test result:'

   do m = 0, max_wave
      index_start = m * ( max_wave + 1 - m ) + m * ( m + 1 ) / 2 + 1
      index_end   = index_start + max_wave - m

      do mm = index_start, index_end
         if (2*mm > 2*sizec) then
            call da_error(__FILE__,__LINE__, &
               (/"rcv_out index bounce"/))
         end if
         ccv(mm) = cmplx (rcv_out(2*mm-1), rcv_out(2*mm))
      end do
      r_leg = 0.0
      call da_legtra_inv( jds, jde, jts, jte, max_wave, xbx%alp_size, m, xbx%alp, &
                          ccv(index_start:index_end), r_leg )

      ccv1(index_start:index_end) = 0.0
      call da_legtra ( xbx%nj, max_wave, xbx%alp_size, m, xbx%int_wgts, xbx%alp, r_leg, &
                       ccv1(index_start:index_end) )
      ccv1(index_start:index_end) = ccv1(index_start:index_end) - &
                                    ccv(index_start:index_end) 
      num =  sum ( real(ccv1(index_start:index_end))*real(ccv1(index_start:index_end))+&
         aimag(ccv1(index_start:index_end))* aimag(ccv1(index_start:index_end)) )     
      den =  sum ( real(ccv(index_start:index_end))*real(ccv(index_start:index_end))+&
         aimag(ccv(index_start:index_end))* aimag(ccv(index_start:index_end)) )     
      write(unit=stdout,fmt='(A,I4,A,E30.20)') &
         'For zonal wave number',m,' difference ',sqrt(num)/sqrt(den)

      xx = sum( real(r_leg(1:xbx%nj))* real(r_leg(1:xbx%nj))+ &
               aimag(r_leg(1:xbx%nj))*aimag(r_leg(1:xbx%nj)) )
      write(unit=stdout,fmt='(a,i5,a,e30.20)') 'For Wave = ',m,' LX.LX    = ',xx

      ccv1(index_start:index_end) = 0.0
      call da_legtra_inv_adj( jds, jde, jts, jte, max_wave, xbx%alp_size, m, xbx%alp, &
                              ccv1(index_start:index_end), r_leg )
      xx = sum( real(ccv(index_start:index_end))*     &
                real(ccv1(index_start:index_end))     +&
               aimag(ccv(index_start:index_end))* &
               aimag(ccv1(index_start:index_end)) )
      write(unit=stdout,fmt='(a,i5,a,e30.20,/)') 'For Wave = ',m,' X.L^TLX  = ',xx
   end do

   if (trace_use) call da_trace_exit("da_test_spectral")

end subroutine da_test_spectral


