subroutine da_eof_decomposition (kz, bx, e, l)
   
   !---------------------------------------------------------------------------
   ! Purpose: Compute eigenvectors E and eigenvalues L of vertical covariance 
   !          matrix
   !          B_{x} defined by equation:  E^{T} B_{x} E = L, given input kz x kz 
   !          BE field.
   !---------------------------------------------------------------------------
   
   implicit none

   integer, intent(in)  :: kz               ! Dimension of error matrix. 
   real,    intent(in)  :: bx(1:kz,1:kz)    ! Vert. background error.
   real*8,  intent(out) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(out) :: l(1:kz)          ! Eigenvalues of Bx.

   integer :: work             ! Size of work array.
   integer :: m                ! Loop counters
   integer :: info             ! Info code.

   real*8  :: work_array(1:3*kz-1)
   real*8  :: ecopy(1:kz,1:kz)
   real*8  :: lcopy(1:kz)   

   if (trace_use) call da_trace_entry("da_eof_decomposition")    

   !-------------------------------------------------------------------------
   ! [5.0]: Perform global eigenvalue decomposition using LAPACK software:
   !-------------------------------------------------------------------------
   
   work = 3 * kz - 1   
   ecopy(1:kz,1:kz) = bx(1:kz,1:kz)
   lcopy(1:kz) = 0.0

   call dsyev( 'V', 'U', kz, ecopy, kz, lcopy, work_array, work, info )
   
   if ( info /= 0 ) then
      write(unit=message(1),fmt='(A,I4)') &
         "Error in decomposition, info = ", info
      call da_error(__FILE__,__LINE__,message(1:1))
   end if
   
   ! Swap order of eigenvalues, vectors so 1st is one with most variance:
   
   do m = 1, kz
      l(m) = lcopy(kz+1-m)
      e(1:kz,m) = ecopy(1:kz,kz+1-m)
   end do  

   if (trace_use) call da_trace_exit("da_eof_decomposition")    
   
end subroutine da_eof_decomposition


