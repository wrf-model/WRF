subroutine da_get_vertical_truncation( max_vert_var, eigenval, be_sub)

   !---------------------------------------------------------------------------
   !  Purpose: Calculate vertical mode truncation from explained variance.
   !
   !  Method:  Calculate cumulative variance and compare with limit.
   !---------------------------------------------------------------------------

   implicit none

   real, intent(in)                 :: max_vert_var    ! Vertical variance limit.
   real*8, intent(in)               :: eigenval(:)     ! Global eigenvaluess.
   type(be_subtype), intent(inout) :: be_sub          ! Back. error sub type.
    
   integer                          :: kz              ! # vertical levels.
   integer                          :: k               ! Loop counter.
   real*8                           :: tot_variance    ! Total variance.
   real*8                           :: cum_variance    ! Cumulative variance.
   character(LEN = 6)              :: name            ! Variable name.

   if (trace_use_dull) call da_trace_entry("da_get_vertical_truncation")


   !---------------------------------------------------------------------------
   ! [1.0] Initialise:
   !---------------------------------------------------------------------------

   kz = size( eigenval(:))
   name = trim(be_sub % name)

   !---------------------------------------------------------------------------
   ! [2.0] Calculate vertical truncation:
   !---------------------------------------------------------------------------

   if (max_vert_var >= 100.0) then
   
      ! [2.1] No truncation: 
      be_sub % mz = kz

      ! Disregard zero/-ve eigenvalues(which should be very small and only 
      ! appear if statistics have been interpolated between domains):

      do k = 1, kz
         if (eigenval(k) <= 0.0) then
            be_sub % mz = k - 1
            exit
         end if
      end do      
   else
   
      ! [2.2] Calculate cumulative variance and truncate:

      tot_variance = sum( eigenval(1:kz))
      cum_variance = 0.0
      
      do k = 1, kz
         cum_variance = cum_variance + eigenval(k)
         
         if (eigenval(k) <= 0.0) then
            be_sub % mz = k - 1
            exit
         end if
         
         if (cum_variance/tot_variance >= 0.01 * max_vert_var) then
            be_sub % mz = k
            exit
         end if  
      end do
      
      if (max_vert_var == 0.0) be_sub % mz = 0 

   end if

   write(unit=stdout,fmt='(A,A6,A3,I3,A1,f7.2,A2)') &
      'Vertical truncation for ', name, &
      ' = ', be_sub % mz, '(', &
      max_vert_var, '%)'
   write (unit=stdout,fmt='(A)') " "

   if (trace_use_dull) call da_trace_exit("da_get_vertical_truncation")
                                       
end subroutine da_get_vertical_truncation


