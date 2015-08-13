subroutine da_eof_decomposition_test (kz, bx, e, l)
   
   !------------------------------------------------------------------------------
   ! Purpose: 
   ! [1] Print eigenvalues:
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   ! [4] Check B correctness: B = E*L*E^T
   !------------------------------------------------------------------------------
   
   implicit none

   integer, intent(in) :: kz               ! Dimension of BE matrix   
   real,    intent(in) :: bx(1:kz,1:kz)    ! Global vert. background error.
   real*8,  intent(in) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(in) :: l(1:kz)          ! Eigenvalues of Bx.
   
   integer                  :: k, k1, k2, m     ! Loop counters
   real                     :: tot_variance     ! Total variance.
   real                     :: cum_variance     ! Cumulative variance.
   real                     :: max_off_diag     ! Maximum off-diagonal.

   real                     :: work(1:kz,1:kz)  ! 2D Work matrix.
   real                     :: bc(1:kz,1:kz)    ! 2D Work matrix.
   logical                  :: array_mask(1:kz) ! Array mask for MAXVAL.

   if (trace_use) call da_trace_entry("da_eof_decomposition_test")

   !------------------------------------------------------------------------- 
   ! [1] Print eigenvalues:
   !-------------------------------------------------------------------------

   tot_variance = sum(l(1:kz))
   cum_variance = 0.0
   
   write(unit=stdout,fmt='(A)')'  Mode    Eigenvalue     Cumulative Variance      e(k,k)'

   do k = 1, kz
      cum_variance = cum_variance + l(k)
      write(unit=stdout,fmt='(I4,4x,e12.4,10x,f8.4,4x,e12.4)') &
            k, l(k), cum_variance / tot_variance, e(k,k)
   end do

   write(unit=stdout,fmt=*)
   
   call da_array_print( 1, e, 'Global Eigenvectors' )

   !-------------------------------------------------------------------------
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   !-------------------------------------------------------------------------
   
   write(unit=stdout,fmt='(A)')' Eigenvector orthogonality check:'
   write(unit=stdout,fmt='(A)')' Mode     Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(1:kz,k1) * e(1:kz,k2))
      end do
   
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------   
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   !-------------------------------------------------------------------------   
   
   write(unit=stdout,fmt='(A)')' Eigenvector completeness check:'
   write(unit=stdout,fmt='(A)')' Level    Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(k1,1:kz) * e(k2,1:kz))
      end do
   
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------
   ! [4]  check B correctness: B = E*L*E^T
   !-------------------------------------------------------------------------

   write(unit=stdout,fmt='(a/a)') &
        'real and Calculated B (diagonal)', &
        'lvl                 real-B                    Calculated-B'

   do k=1,kz
      do m=1,kz
         work(k,m)=l(k)*e(m,k)
         bc(k,m)=0.0
      end do
   end do
   
   do k1=1,kz
      do k2=1,kz
         do m=1,kz
            bc(k1,k2)=bc(k1,k2)+e(k1,m)*work(m,k2)
         end do
      end do

      write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k1), bc(k1,k1)
   end do

   do k2=1,kz
      write(unit=stdout, fmt='(a,i4/a)') &
           'real and Calculated B (off diagonal):', k2, &
           'lvl                 real-B                    Calculated-B'

      do k1=1,kz
        write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k2), bc(k1,k2)
      end do
   end do

   if (trace_use) call da_trace_exit("da_eof_decomposition_test")
   
end subroutine da_eof_decomposition_test


