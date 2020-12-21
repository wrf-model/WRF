
subroutine kj_swap (source, target, is, ie, js, je, ks, ke)

   implicit none

   integer, intent(in) :: is, ie, js, je, ks, ke
   real, dimension(is:ie,js:je,ks:ke), intent(in) :: source
   real, dimension(is:ie,ks:ke,js:je), intent(out) :: target

   integer :: i, j, k

   if (trace_use_dull) call da_trace_entry("kj_swap")

   do j = js, je
      do k = ks, ke
         do i = is, ie
            target(i,k,j) = source(i,j,k)
         enddo
      enddo
   enddo

   if (trace_use_dull) call da_trace_entry("kj_swap")

end subroutine kj_swap
