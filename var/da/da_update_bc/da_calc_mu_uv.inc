subroutine da_calc_mu_uv (mu, mub, muu, muv, &
                        ids, ide, jds, jde)

   implicit none

   integer, intent(in) :: ids, ide, jds, jde

   real, dimension(ids:ide, jds:jde),     intent(in  ) :: mu,  mub

   real, dimension(ids:ide+1, jds:jde ), intent( out) :: muu
   real, dimension(ids:ide  , jds:jde+1), intent( out) :: muv

   real, dimension(ids-1:ide+1, jds-1:jde+1) :: mut

   integer :: i, j

   do j=jds,jde
      do i=ids,ide
         mut(i,j) = mu(i,j)+mub(i,j)
      end do

      mut(ids-1,j) = mut(ids,j)
      mut(ide+1,j) = mut(ide,j)
   end do

   do i=ids-1,ide+1
      mut(i,jds-1)=mut(i,jds)
      mut(i,jde+1)=mut(i,jde)
   end do

   do j=jds,jde
      do i=ids,ide+1
         muu(i,j) = 0.5*(mut(i,j)+mut(i-1,j))
      end do
   end do

   do j=jds,jde+1
      do i=ids,ide
         muv(i,j) = 0.5*(mut(i,j)+mut(i,j-1))
      end do
   end do

end subroutine da_calc_mu_uv


