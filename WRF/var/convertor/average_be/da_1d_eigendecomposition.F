subroutine da_1d_eigendecomposition( bx, e, l, k )

   !------------------------------------------------------------------------------
   !  Purpose: Compute eigenvectors E and eigenvalues L of vertical covariance matrix
   !           B_{x} defined by equation:  E^{T} B_{x} E = L, given input 3D field of
   !           errors (sum over all horizontal locations).
   !------------------------------------------------------------------------------

   implicit none
   
   integer, intent(in)               :: k

   real, dimension(k,k), intent(in)  :: bx      ! Global vert. background error.
   real, dimension(k,k), intent(out) :: e       ! Eigenvectors of Bx.
   real, dimension(k), intent(out)   :: l       ! Global eigenvalues of Bx.
   
   integer                  :: kz               ! Size of 3rd dimension.
   integer                  :: m                ! Loop counters
   integer                  :: work             ! Size of work array.
   integer                  :: info             ! Info code.
   
   real, allocatable        :: ecopy(:,:)
   real, allocatable        :: lcopy(:)
   real, allocatable        :: work_array(:)

!   if (trace_use_dull) call da_trace_entry("da_1d_eigendecomposition")

   !-------------------------------------------------------------------------
   ! [1.0]: Initialise:
   !-------------------------------------------------------------------------
   
   kz = size(bx, dim=1)
   
   !-------------------------------------------------------------------------
   ! [5.0]: Perform global eigenvalue decomposition using LAPACK software:
   !-------------------------------------------------------------------------
   
   work = 3 * kz - 1
   allocate( work_array(1:work) )
   
   allocate( ecopy(1:kz,1:kz) )
   allocate( lcopy(1:kz) )
   
   ecopy(:,:) = bx(:,:)

   lcopy = 0.0

   call dsyev( 'V', 'U', kz, ecopy, kz, lcopy, &
              work_array, work, info )
   
!   if ( info /= 0 ) then
!      write(unit=message(1),fmt='(A,I4,A)') &
!         ' da_1d_eigendecomposition: info = ', &
!         info,' - error in decomposition.'
!      call da_error(__FILE__,__LINE__,message(1:1))
!   end if
   
   !--Swap order of eigenvalues, vectors so 1st is one with most
   !  variance, etc:
   
   do m=1,kz
      l(m) = lcopy(kz+1-m)
      e(1:kz,m) = ecopy(1:kz,kz+1-m)
   end do
   
   deallocate (work_array)
   deallocate (ecopy)
   deallocate (lcopy)

!   if (trace_use_dull) call da_trace_exit("da_1d_eigendecomposition")
   
end subroutine da_1d_eigendecomposition
