subroutine da_recursive_filter_1d_adj(pass, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of recursive filter on 1D array - adjoint.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: pass           ! Current pass of filter.
   real*8, intent(in)     :: alpha          ! Alpha coefficient for RF.
   real*8, intent(inout)  :: field(:)       ! Array to be filtered.
   integer, intent(in)    :: n              ! Size of field array.
   
   integer                :: j              ! Loop counter.
   real                   :: one_alpha      ! 1 - alpha.
   real                   :: a(1:n)         ! Input field.
   real                   :: b(1:n)         ! Field after left-right pass.
   real                   :: c(1:n)         ! Field after right-left pass.

   if (trace_use_dull) call da_trace_entry("da_recursive_filter_1d_adj")

   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   one_alpha = 1.0 - alpha

   !-------------------------------------------------------------------------
   ! [4.0] Copy and tidy up:
   !-------------------------------------------------------------------------

   c(1:n) = field(1:n)
   
   !-------------------------------------------------------------------------
   ! [3.0] Perform left-moving filter:
   !-------------------------------------------------------------------------

   ! [3.2] Perform pass left to right:

   b(1:n) = 0.0   

   do j = 1, n-1
      c(j+1) = c(j+1) + alpha * c(j)
      b(j) = one_alpha * c(j)
   end do

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      b(n) = b(n) + c(n) / (1.0 + alpha)
   else
      b(n) = b(n) + one_alpha * c(n) / (1.0 - alpha**2)**2
      b(n-1) = b(n-1) - one_alpha * alpha**3 * c(n) / (1.0 - alpha**2)**2
   end if

   !-------------------------------------------------------------------------
   ! [2.0] Perform right-moving filter:
   !-------------------------------------------------------------------------

   a(1:n) = 0.0

   ! [2.2] Perform pass left to right:

   do j = n, 2, -1
      b(j-1) = b(j-1) + alpha * b(j)
      a(j) = a(j) + one_alpha * b(j)
   end do

   ! use turning conditions as in the appendix of Hayden & Purser (1995):

   if (pass == 1) then
      a(1) = a(1) + one_alpha * b(1)
   else if (pass == 2) then
      a(1) = a(1) + b(1) / (1.0 + alpha)
   else
      a(1) = a(1) + one_alpha * b(1) / (1.0 - alpha**2)**2
      a(2) = a(2) - one_alpha * alpha**3 * b(1) / (1.0 - alpha**2)**2
   end if

   field(1:n) = a(1:n)

   if (trace_use_dull) call da_trace_exit("da_recursive_filter_1d_adj")
   
end subroutine da_recursive_filter_1d_adj


