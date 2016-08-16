subroutine da_readwrite_be_stage3(outunit, nk, variable)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)      :: outunit                    ! Output unit number.
   integer, intent(in)      :: nk                         ! Number of vertical levels/modes.
   character*10, intent(in) :: variable                   ! Variable name

   character(len=filename_len) :: filename                   ! Input filename.
   character*10             :: vardum                     ! Dummy variable name.

   integer                  :: num_bins2d                 ! Number of Eigenvector bins.
   integer                  :: nkdum                      ! Dummy nk variable.
   real                     :: e_vec(1:nk,1:nk)           ! Domain-averaged eigenvectors.
   real                     :: e_val(1:nk)                ! Domain-averaged eigenvalues.  
   real, allocatable        :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable        :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.

   integer :: iunit
 
   call da_get_unit(iunit)
   filename = 'gen_be_stage3.'//trim(variable)//'.dat'
   open (iunit, file = filename, form='unformatted')
   read(iunit)vardum
   if (trim(vardum) /= trim(variable)) then
     call da_error(__FILE__,__LINE__, &
       (/"Inconsistent variable name"/))
   end if

   read(iunit)nkdum, num_bins2d
   if (nkdum /= nk) then
     call da_error(__FILE__,__LINE__, &
       (/"Inconsistent nk between regression coefficients and vertical modes"/))
   end if

   allocate(e_vec_loc(1:nk,1:nk,1:num_bins2d))
   allocate(e_val_loc(1:nk,1:num_bins2d))

   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   close(iunit)
   call da_free_unit(iunit)

   write(outunit)variable
   write(outunit)nk, num_bins2d
   write(outunit)e_vec
   write(outunit)e_val
   write(outunit)e_vec_loc
   write(outunit)e_val_loc

   deallocate(e_vec_loc)
   deallocate(e_val_loc)

end subroutine da_readwrite_be_stage3


