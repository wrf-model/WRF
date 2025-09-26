module module_mergesort

   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: mergesort
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive subroutine mergesort(array, n1, n2)
   
      implicit none
   
      ! Arguments
      integer, intent(in) :: n1, n2
      integer, dimension(n1:n2), intent(inout) :: array
   
      ! Local variables
      integer :: i, j, k
      real :: rtemp
      real, dimension(1:n2-n1+1) :: temp
   
      if (n1 >= n2) return
   
      if (n2 - n1 == 1) then
         if (array(n1) < array(n2)) then
            rtemp = array(n1)
            array(n1) = array(n2)
            array(n2) = rtemp
         end if
         return
      end if
   
      call mergesort(array(n1:n1+(n2-n1+1)/2), n1, n1+(n2-n1+1)/2)
      call mergesort(array(n1+((n2-n1+1)/2)+1:n2), n1+((n2-n1+1)/2)+1, n2)
   
      i = n1
      j = n1 + ((n2-n1+1)/2) + 1
      k = 1
      do while (i <= n1+(n2-n1+1)/2 .and. j <= n2)
         if (array(i) > array(j)) then
            temp(k) = array(i)
            k = k + 1
            i = i + 1
         else
            temp(k) = array(j)
            k = k + 1
            j = j + 1
         end if
      end do
   
      if (i <= n1+(n2-n1+1)/2) then
         do while (i <= n1+(n2-n1+1)/2)
            temp(k) = array(i)
            i = i + 1
            k = k + 1
         end do
      else
         do while (j <= n2)
            temp(k) = array(j)
            j = j + 1
            k = k + 1
         end do
      end if
   
      array(n1:n2) = temp(1:k-1)
   
   end subroutine mergesort
   
end module module_mergesort
