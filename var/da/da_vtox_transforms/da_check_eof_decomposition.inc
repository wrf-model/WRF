subroutine da_check_eof_decomposition(be_eigenval, be_eigenvec, name)

   !---------------------------------------------------------------------------
   ! Purpose: Check eigenvectors E of vertical covariance matrix B_{x} which 
   ! have been read in from NMC-statistics file.
   !
   ! Method:  E and L (eigenvalues) are computed using LAPACK/BLAS software in 
   ! the NMC code using the definition E^{T} B_{x} E = L. This routine checks 
   ! for eigenvector orthogonality and completeness as defined below.
   !---------------------------------------------------------------------------

   implicit none
      
   real*8, intent(in)            :: be_eigenval(:)   ! Back. error eigenvalues.
   real*8, intent(in)            :: be_eigenvec(:,:) ! Back. error eigenvector
   character(len=*), intent(in)  :: name             ! Variable name.

   integer                       :: kz               ! Size of 3rd dimension.
   integer                       :: k, k1, k2, m     ! Loop counters
   real                          :: tot_variance     ! Total variance.
   real                          :: cum_variance     ! Cumulative variance.
   real                          :: max_off_diag     ! Maximum off-diagonal.
      
   real, allocatable             :: matrix2(:,:)     ! 2D Work matrix.
   logical, allocatable          :: array_mask(:)    ! Array mask for MAXVAL.

   if (trace_use) call da_trace_entry("da_check_eof_decomposition")

   !----------------------------------------------------------------------
   ! [1.0]: Initialise:
   !----------------------------------------------------------------------  

   kz = size(be_eigenval)
                         
   !----------------------------------------------------------------------
   ! [2.0]: Print out global eigenvalues (used for truncation):
   !----------------------------------------------------------------------  

   cum_variance = 0.0
   tot_variance = sum(be_eigenval(1:kz))

   write(unit=stdout,fmt='(A)') 'Name Mode  Eigenvalue Normalised Variance'
   do k = 1, kz
      cum_variance =  be_eigenval(k) + cum_variance
      write(unit=stdout,fmt='(A,I4,e14.4,f10.4)') &
         trim(name), k, be_eigenval(k), cum_variance / tot_variance
   end do
   write(unit=stdout,fmt=*) ' '
   write(unit=stdout,fmt='(A,e13.5)') 'Total variance = Tr(Bv) = ', tot_variance
   write(unit=stdout,fmt=*) ' '

   !--------------------------------------------------------------------------
   ! [2.0]: Test global eigenvectors:
   !--------------------------------------------------------------------------

   ! [2.1] Print out global eigenvectors:

   write(unit=stdout,fmt='(2A)') 'Domain eigenvectors for ', trim(name)

   write(unit=stdout,fmt='(50i13)')(m, m=1,kz)
   do k = 1, kz      
      write(unit=stdout,fmt='(I3,50e13.5)')k, (be_eigenvec(k,m), m=1,kz)
   end do
   write(unit=stdout,fmt='(A)') " "

   ! [2.2]: Test eigenvector orthogonality: sum_k (e_m(k) e_n(k)) = delta_mn:

   allocate(array_mask(1:kz))
   allocate(matrix2(1:kz,1:kz))
      
   write(unit=stdout,fmt='(2A)') &
      'Eigenvector orthogonality check for ', trim(name)
   write(unit=stdout,fmt='(A)')' Mode     Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = sum(be_eigenvec(:,k1) * be_eigenvec(:,k2))
      end do
         
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), &
         max_off_diag
   end do
   write(unit=stdout,fmt=*) ' '

   ! [2.3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2

   write(unit=stdout,fmt='(2A)') &
      'Eigenvector completeness check for ', trim(name)
   write(unit=stdout,fmt='(A)')' Level    Diagonal         Maximum off-diagonal'
   do k1 = 1, kz
      do k2 = 1, kz
         matrix2(k1,k2) = sum(be_eigenvec(k1,:) * be_eigenvec(k2,:))
      end do
         
      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = MAXVAL(ABS(matrix2(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, matrix2(k1,k1), &
         max_off_diag
   end do
   write(unit=stdout,fmt=*) ' '

   !-------------------------------------------------------------------------
   ! [3.0]: Tidy up:
   !-------------------------------------------------------------------------  

   deallocate(matrix2)
   deallocate(array_mask)

   if (trace_use) call da_trace_exit("da_check_eof_decomposition")
       
end subroutine da_check_eof_decomposition


