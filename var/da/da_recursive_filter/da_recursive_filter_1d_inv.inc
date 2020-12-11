subroutine da_recursive_filter_1d_inv(pass, alpha, field, n)

   !---------------------------------------------------------------------------
   ! Purpose: Perform one pass of inverse of recursive filter on 1D array.
   !
   ! Method:  Inverse filter is non-recursive. References:
   !
   ! Lorenc, A. (1992), Iterative Analysis Using Covariance Functions and Filters. 
   !                    Q.J.R. Meteorol. Soc., 118: 569-591. Equation (A2)
   !
   ! Christopher M. Hayden and R. James Purser, 1995: Recursive Filter Objective Analysis of 
   !                    Meteorological Fields: Applications to NESDIS Operational Processing. 
   !                    J. Appl. Meteor., 34, 3-15. 
   !
   ! Dale Barker etal., 2004, A 3DVAR data assimilation system for use with MM5, 
   !                          NCAR Tech Note 393.
   !
   ! Author: Zhiquan (Jake) Liu, NCAR/MMM, 2015-09 
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

   if (trace_use_dull) call da_trace_entry("da_recursive_filter_1d_inv")
   
   !-------------------------------------------------------------------------
   ! [1.0] Initialise:
   !-------------------------------------------------------------------------

   one_alpha = 1.0 - alpha
   
   c(1:n) = field(1:n)

   !-------------------------------------------------------------------------
   ! [2.0] Perform non-recursive inverse filter:
   !-------------------------------------------------------------------------

   ! Follow the appendix Eq. (A2) of Lorenc (1992):

   do j = 2, n-1
      a(j) = c(j) - (alpha/one_alpha**2) * (c(j-1)-2.0*c(j)+c(j+1))
   end do

   !-------------------------------------------------------------------------
   ! [3.0] Perform inverse filter at boundary points 1 & n:
   !-------------------------------------------------------------------------

   ! use turning conditions as in the appendix of Hayden & Purser (1995):
   ! also see Barker etal., 2004, chapter 5a.

   if (pass == 1) then
     b(1) = (c(1)-alpha*c(2))/one_alpha
     a(1) = b(1)/one_alpha

     b(n-1) = (c(n-1)-alpha*c(n))/one_alpha
     b(n) = c(n)*(1.0+alpha)
     a(n) = (b(n) - alpha*b(n-1))/one_alpha
   else if ( pass == 2) then
     b(1) = (c(1)-alpha*c(2))/one_alpha
     a(1) = b(1)*(1.0+alpha)

     b(n-1) = (c(n-1)-alpha*c(n))/one_alpha
     b(n) = c(n)*(1.0-alpha**2)**2/one_alpha+alpha**3*b(n-1)
     a(n) = (b(n) - alpha*b(n-1))/one_alpha
   else
     b(1) = (c(1)-alpha*c(2))/one_alpha
     a(1) = b(1)*(1.0-alpha**2)**2/one_alpha+alpha**3*a(2)

     b(n-1) = (c(n-1)-alpha*c(n))/one_alpha
     b(n) = c(n)*(1.0-alpha**2)**2/one_alpha+alpha**3*b(n-1)
     a(n) = (b(n) - alpha*b(n-1))/one_alpha
   end if

   field(1:n) = a(1:n)

   if (trace_use_dull) call da_trace_exit("da_recursive_filter_1d_inv")
   
end subroutine da_recursive_filter_1d_inv
