subroutine da_allocate_background_errors_chem  (jy, kz, l, e, be_sub)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of wrfvar background errors.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: jy                 ! i/y dimension.
   integer, intent(in)              :: kz                 ! k/z dimension.
   real*8, intent(in)               :: l(:)               ! Global eigenvalue.
   real*8, intent(in)               :: e(:,:)             ! Global eigenvectors.
!   real*8, intent(in)               :: be_eval_loc(:,:)   ! Std dev/local evalue.
!   real*8, intent(in)               :: be_evec_loc(:,:,:) ! Local eigenvectors.
   type (be_subtype), intent(inout) :: be_sub             ! Backgrd error struct.
    
   integer                          :: mz                 ! Vertical truncation.
   integer                          :: j, m, k            ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_allocate_background_errors_chem")

   !--------------------------------------------------------------------------
   ! [1.0] Initialise:
   !--------------------------------------------------------------------------

   mz = be_sub % mz
   
   !--------------------------------------------------------------------------
   ! [2.0] Allocate components of be_sub structure:
   !--------------------------------------------------------------------------

   if (mz > 0) then
      allocate  (be_sub % val(1:jy,1:mz))
      
         !--------------------------------------------------------------------
         ! [3.0] Allocate eigenvalues of temporal error covariance matrix:
         !--------------------------------------------------------------------

         ! use global eigenvalues:
         do m = 1, mz
            be_sub % val(1:jy,m) = sqrt (l(m))
         end do
 
         if (print_detail_be) then
            write(unit=message(1),fmt='(A,A)') 'j*k Eigenvalues for ', be_sub % name
            call da_array_print(2, be_sub % val(1:jy,1:mz), message(1))
         end if

         !----------------------------------------------------------------------- 
         ! [4.0] Allocate global eigenvectors of temporal error cov.:
         !-----------------------------------------------------------------------

         allocate  (be_sub % evec(1:jy,1:kz,1:mz))
         
         ! use global eigenvectors:
         do j = 1, jy
            be_sub % evec(j,1:kz,1:mz) = e(1:kz,1:mz)
         end do
         
         if (print_detail_be) then      
            write(unit=message(1),fmt='(A,A)') 'k*k Eigenvectors for j = 1 ', be_sub % name
            call da_array_print (2, be_sub % evec(1,1:kz,1:mz), message(1))
         
            write(unit=message(1),fmt='(A,A)') 'k*k Eigenvectors for j = jy ', be_sub % name
            call da_array_print (2, be_sub % evec(jy,1:kz,1:mz), message(1))
         end if

         allocate (be_sub%val_g(1:mz))
         allocate (be_sub%evec_g(1:kz,1:mz))
  
         be_sub % val_g(1:mz) = l(1:mz)
         be_sub % evec_g(1:kz,1:mz) = e(1:kz,1:mz)

      !-----------------------------------------------------------------------
      ! [2.2] Allocate recursive filter lengthscales and variance scaling factors:
      !-----------------------------------------------------------------------

      allocate (be_sub % rf_alpha(1:mz))

      be_sub % rf_alpha(1:mz) = 1.0    
   end if

   if (trace_use_dull) call da_trace_exit("da_allocate_background_errors_chem")

end subroutine da_allocate_background_errors_chem


