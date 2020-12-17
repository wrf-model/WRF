subroutine da_lubksb(n, np, indx, a, b)

   !-----------------------------------------------------------------------
   ! Purpose: Adapted Numerical Recipes routine to solve the set of n linear 
   ! equations A.X=B.
   ! Routine takes in to account possibility that B will begin with many zero elements, 
   ! so it is efficient for matrix inversion.
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: n              ! Logical size of array.
   integer, intent(in)    :: np             ! Physical size of array.
   integer, intent(in)    :: indx(1:n)      ! Permutation vector returned by LUDCMP. 
   real,    intent(in)    :: a(1:np,1:np)   ! LU decomposition of matrix A in A.x=B.
   real,    intent(inout) :: b(1:n)         ! On input = B, on output = x.

   integer :: i , ii , j , ll
   real    :: sum

   if (trace_use) call da_trace_entry("da_lubksb")

   ii = 0
   do i = 1 , n
      ll = indx(i)
      sum = b(ll)
      b(ll) = b(i)

      if (ii /= 0) then
         do j = ii , i - 1
            sum = sum - a(i,j) * b(j)
         end do
      else if (sum /= 0.0) then
         ii = i
      end if
      b(i) = sum
   end do

   do i = n , 1 , -1
      sum = b(i)
      if (i < n) then
         do j = i + 1 , n
            sum = sum - a(i,j) * b(j)
         end do
      end if
      b(i) = sum / a(i,i)
   end do

   if (trace_use) call da_trace_exit("da_lubksb")

end subroutine da_lubksb


