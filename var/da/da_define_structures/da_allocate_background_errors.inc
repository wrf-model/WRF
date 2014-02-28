subroutine da_allocate_background_errors  (jy, kz, l, e, be_eval_loc, &
                                           be_evec_loc, be_sub)

   !---------------------------------------------------------------------------
   ! Purpose: Allocate components of wrfvar background errors.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: jy                 ! i/y dimension.
   integer, intent(in)              :: kz                 ! k/z dimension.
   real*8, intent(in)               :: l(:)               ! Global eigenvalue.
   real*8, intent(in)               :: e(:,:)             ! Global eigenvectors.
   real*8, intent(in)               :: be_eval_loc(:,:)   ! Std dev/local evalue.
   real*8, intent(in)               :: be_evec_loc(:,:,:) ! Local eigenvectors.
   type (be_subtype), intent(inout) :: be_sub             ! Backgrd error struct.
    
   integer                          :: mz                 ! Vertical truncation.
   integer                          :: j, m, k            ! Loop counter.

   if (trace_use_dull) call da_trace_entry("da_allocate_background_errors")

   !--------------------------------------------------------------------------
   ! [1.0] Initialise:
   !--------------------------------------------------------------------------

   mz = be_sub % mz
   
   !--------------------------------------------------------------------------
   ! [2.0] Allocate components of be_sub structure:
   !--------------------------------------------------------------------------

   if (mz > 0) then
      allocate  (be_sub % val(1:jy,1:mz))
      
      if (vert_corr == vert_corr_2) then

         !--------------------------------------------------------------------
         ! [3.0] Allocate eigenvalues of vertical error covariance matrix:
         !--------------------------------------------------------------------

         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvalues:
            do m = 1, mz
               be_sub % val(1:jy,m) = sqrt (l(m))
            end do
         else
            ! use eigenvalues varying with j-direction.
            do j = 1, jy
               do k = 1, mz
                  if (be_eval_loc(j,k) <=0) then
                     write (unit=message(1),fmt='(A,I5,A,I5,A,F10.2)') &
                        "At lat= ",j," For mode = ",k," local eigen value= ",be_eval_loc(j,k)
                     call da_error(__FILE__,__LINE__,message(1:1))
                  end if
               end do
            end do
            be_sub % val(1:jy,1:mz) = sqrt (be_eval_loc(1:jy,1:mz))            
         end if
 
         if (print_detail_be) then
            write(unit=message(1),fmt='(A,A)') 'j*k Eigenvalues for ', be_sub % name
            call da_array_print(2, be_sub % val(1:jy,1:mz), message(1))
         end if

         !----------------------------------------------------------------------- 
         ! [4.0] Allocate global eigenvectors of vertical error cov.:
         !-----------------------------------------------------------------------

         allocate  (be_sub % evec(1:jy,1:kz,1:mz))
         
         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvectors:
            do j = 1, jy
               be_sub % evec(j,1:kz,1:mz) = e(1:kz,1:mz)
            end do
         else
            ! use eigenvectors varying with i-direction.
            be_sub % evec(1:jy,1:kz,1:mz) =  be_evec_loc(1:jy,1:kz,1:mz)
         end if
         
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
      else if (vert_corr == vert_corr_1) then
         if (print_detail_be) then
           write(unit=message(1),fmt='(A)') 'Change BE std dev to variance in NMC code'
           call da_message(message(1:1))
         end if
         if (vert_evalue == vert_evalue_global) then
            ! use global eigenvalues:
          do m = 1, mz
          be_sub % val(1:jy,m) = l(m)
          end do 
         else
          be_sub % val(1:jy,1:mz) = be_eval_loc(1:jy,1:mz)
         end if
      end if

      !-----------------------------------------------------------------------
      ! [2.2] Allocate recursive filter lengthscales and variance scaling factors:
      !-----------------------------------------------------------------------

      allocate (be_sub % rf_alpha(1:mz))

      be_sub % rf_alpha(1:mz) = 1.0    
   end if

   if (trace_use_dull) call da_trace_exit("da_allocate_background_errors")

end subroutine da_allocate_background_errors


