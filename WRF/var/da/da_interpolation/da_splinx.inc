SUBROUTINE da_splinx ( n, x, y, nn, xx, c, yy )

!*************************************************************
! This subroutine provides natural cubic spline interpolation
! of a given grid function onto another given grid.
! Inputs:  n - number of input grid points
!          x - array of input argument data
!          y - array of input function data
!          nn - number of output data
!          xx - array of output argument data
! Outputs: yy - array of output function data
!*************************************************************
! If output grid is outside input grid
! then linear extrapolation is applied
!*************************************************************

      implicit none
      integer :: i, j, k, m
      integer,intent(in) :: n
      integer,intent(in) :: nn
      real, dimension(n), intent(in) :: x,y
      real, dimension(nn),intent(in) :: xx
      real, dimension(nn),intent(out):: yy
      real, dimension(3,n),intent(out):: c

      real :: dxr, dyr, dxl, dyl, dx, dy, del, der, d, dyx

      c(1,1)=0.
      c(2,1)=0.
      c(2,n)=0.
      dxr=x(2)-x(1)
      dyr=(y(2)-y(1))/dxr
      do 1 i=2,n-1
      dxl=dxr
      dxr=x(i+1)-x(i)
      dyl=dyr
      dyr=(y(i+1)-y(i))/dxr
      dx=dxr+dxl
      dy=(dyr-dyl)/dx
      c(1,i)=-dxr/(2.*dx+dxl*c(1,i-1))
      c(2,i)=(6.*dx*dy-dxl*c(2,i-1))/(2.*dx+dxl*c(1,i-1))
1     continue
      do 2 i=n-1,2,-1
      c(2,i)=c(1,i)*c(2,i+1)+c(2,i)
2     continue
      do 3 i=1,n-1
      dx=x(i+1)-x(i)
      dy=y(i+1)-y(i)
      dyx=dy/dx
      c(1,i)=dyx-dx*(c(2,i)/3.+c(2,i+1)/6.)
      c(2,i)=c(2,i)/2.
      c(3,i)=(dy-c(1,i)*dx-c(2,i)*dx**2)/dx**3
3     continue

      do 5 j=1,nn
      if (xx(j).le.x(1)) then
      yy(j)=y(1)+c(1,1)*(xx(j)-x(1))
      else if (xx(j).ge.x(n)) then
      del=x(n)-x(n-1)
      der=c(1,n-1)+2.d0*c(2,n-1)*del+3.d0*c(3,n-1)*del**2
      yy(j)=y(n)+der*(xx(j)-x(n))
      else
      do 4 i=1,n
4     if (x(i).le.xx(j)) m=i
      d=xx(j)-x(m)
      yy(j)=y(m)+d*(c(1,m)+d*(c(2,m)+d*c(3,m)))
      end if
5     continue

END SUBROUTINE da_splinx
