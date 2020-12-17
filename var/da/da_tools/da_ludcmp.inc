subroutine da_ludcmp(n, np, indx, a, d)

   !-----------------------------------------------------------------------
   ! Purpose: Adapted Numerical Recipes routine to solve the set of n linear 
   ! equations 
   ! A.X=B. Routine takes in to account possibility that B will begin with many 
   ! zero elements, so it is efficient for matrix inversion.
   !-----------------------------------------------------------------------

   implicit none

   integer, intent(in)    :: n           ! Logical size of array.
   integer, intent(in)    :: np          ! Physical size of array.
   integer, intent(out)   :: indx(1:n)   ! Permutation vector returned by LUDCMP 
   real,    intent(inout) :: a(1:np,1:np)! LU decomposition of matrix A in A.x=B
   real,    intent(out)   :: d           ! On input = B, on output = x.

   real, parameter      :: tiny = 1.0e-20
   real                 :: aamax , dum , sum
   integer              :: i , imax , j , k
   real                 :: vv(1:np)

   if (trace_use) call da_trace_entry("da_ludcmp")

   d = 1.0
   do i = 1 , n
      aamax = 0.0

      do j = 1 , n
         if (abs(a(i,j)) > aamax) aamax = abs(a(i,j))
      end do
      if (aamax == 0.0) then
         call da_error(__FILE__,__LINE__,(/"Singular matrix"/))
      end if
      vv(i) = 1.0 / aamax
   end do

   do j = 1 , n
      if (j > 1) then
         do i = 1 , j - 1
            sum = a(i,j)
            if (i > 1) then
               do k = 1 , i - 1
                  sum = sum - a(i,k) * a(k,j)
               end do
               a(i,j) = sum
            end if
         end do
      end if

      aamax = 0.0
      do i = j , n
         sum = a(i,j)
         if (j > 1) then
            do k = 1 , j - 1
               sum = sum - a(i,k) * a(k,j)
            end do
            a(i,j) = sum
         end if
         dum = vv(i) * abs(sum)
         if (dum >= aamax) then
            imax = i
            aamax = dum
         end if
      end do

      if (j /= imax) then
         do k = 1 , n
            dum = a(imax,k)
            a(imax,k) = a(j,k)
            a(j,k) = dum
         end do
         d = -d
         vv(imax) = vv(j)
      end if

      indx(j) = imax
      if (j /= n) then
         if (a(j,j) == 0.0) a(j,j) = tiny
         dum = 1.0 / a(j,j)
         do i = j + 1 , n
            a(i,j) = a(i,j) * dum
         end do
      end if
   end do

   if (a(n,n) == 0.0) a(n,n) = tiny

   if (trace_use) call da_trace_exit("da_ludcmp")

end subroutine da_ludcmp


