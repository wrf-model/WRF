subroutine da_recursive_filter_1d(pass, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of recursive filter on 1D array.
   !
   ! Method:  Perform right-moving filter followed by left-moving filter.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: pass           ! Current pass of filter.
   real*8,  intent(in)    :: alpha          ! Alpha coefficient for RF.
   real*8,  intent(inout) :: field(:)       ! Array to be filtered.
   integer, intent(in)    :: n              ! Size of field array.

   integer :: j              ! Loop counter.
   real    :: one_alpha      ! 1 - alpha.
   real    :: a(1:n)         ! Input field.
   real    :: b(1:n)         ! Field after left-right pass.
   real    :: c(1:n)         ! Field after right-left pass.

   if (trace_use_dull) call da_trace_entry("da_recursive_filter_1d")
   
   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   one_alpha = 1.0 - alpha
   
   a(1:n) = field(1:n)

   !-------------------------------------------------------------------------
   ! [2.0] Perform right-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      b(1) = one_alpha * a(1)
   else if (pass == 2) then
      b(1) = a(1) / (1.0 + alpha)
   else
      b(1) = one_alpha * (a(1) - alpha**3 * a(2)) / (1.0 - alpha**2)**2
   end if

   ! [2.2] Perform pass left to right:

   do j = 2, n
      b(j) = alpha * b(j-1) + one_alpha * a(j)
   end do

   !-------------------------------------------------------------------------
   ! [3.0] Perform left-moving filter:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      c(n) = b(n) / (1.0 + alpha)
   else
      c(n) = one_alpha * (b(n) - alpha**3 * b(n-1)) / (1.0 - alpha**2)**2
   end if

   ! [3.2] Perform pass left to right:

   do j = n-1, 1, -1
      c(j) = alpha * c(j+1) + one_alpha * b(j)
   end do
        
   field(1:n) = c(1:n)

   if (trace_use_dull) call da_trace_exit("da_recursive_filter_1d")
   
end subroutine da_recursive_filter_1d


