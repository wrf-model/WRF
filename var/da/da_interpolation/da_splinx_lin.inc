subroutine da_splinx_lin( n, x, y, g_y, c, g_c, nn, xx, yy, g_yy )

implicit none
integer n
integer nn
real(kind=8) c(3,n)
real(kind=8) g_c(3,n)
real(kind=8) g_y(n)
real(kind=8) g_yy(n)
real(kind=8) x(n)
real(kind=8) xx(n)
real(kind=8) y(n)
real(kind=8) yy(n)
real(kind=8) d
real(kind=8) del
real(kind=8) der
real(kind=8) dx
real(kind=8) dxl
real(kind=8) dxr
real(kind=8) dy
real(kind=8) dyl
real(kind=8) dyr
real(kind=8) dyx
real(kind=8) g_der
real(kind=8) g_dy
real(kind=8) g_dyl
real(kind=8) g_dyr
real(kind=8) g_dyx
integer i
integer j
integer m
g_c(1,1) = 0.
c(1,1) = 0.
g_c(2,1) = 0.
c(2,1) = 0.
g_c(2,n) = 0.
c(2,n) = 0.
dxr = x(2)-x(1)
g_dyr = g_y(2)/dxr-g_y(1)/dxr
dyr = (y(2)-y(1))/dxr
do i = 2, n-1
  dxl = dxr
  dxr = x(i+1)-x(i)
  g_dyl = g_dyr
  dyl = dyr
  g_dyr = g_y(i+1)/dxr-g_y(i)/dxr
  dyr = (y(i+1)-y(i))/dxr
  dx = dxr+dxl
  g_dy = (-(g_dyl/dx))+g_dyr/dx
  dy = (dyr-dyl)/dx
  g_c(1,i) = g_c(1,i-1)*(dxr*dxl/((2.*dx+dxl*c(1,i-1))*(2.*dx+dxl*c(1,i-1))))
  c(1,i) = -(dxr/(2.*dx+dxl*c(1,i-1)))
  g_c(2,i) = (-(g_c(2,i-1)*(dxl/(2.*dx+dxl*c(1,i-1)))+g_c(1,i-1)  &
            *((6.*dx*dy-dxl*c(2,i-1))*dxl/((2.*dx+dxl*c(1,i-1))   &
            *(2.*dx+dxl*c(1,i-1))))))+g_dy*(6*dx/(2.*dx+dxl*c(1,i-1)))
  c(2,i) = (6.*dx*dy-dxl*c(2,i-1))/(2.*dx+dxl*c(1,i-1))
end do
do i = n-1, 2, -1
  g_c(2,i) = g_c(2,i+1)*c(1,i)+g_c(2,i)+g_c(1,i)*c(2,i+1)
  c(2,i) = c(1,i)*c(2,i+1)+c(2,i)
end do
do i = 1, n-1
  dx = x(i+1)-x(i)
  g_dy = g_y(i+1)-g_y(i)
  dy = y(i+1)-y(i)
  g_dyx = g_dy/dx
  dyx = dy/dx
  g_c(1,i) = (-(0.16666667*g_c(2,i+1)*dx+0.33333333*g_c(2,i)*dx))+g_dyx
  c(1,i) = dyx-dx*(c(2,i)/3.+c(2,i+1)/6.)
  g_c(2,i) = 0.5*g_c(2,i)
  c(2,i) = c(2,i)/2.
  g_c(3,i) = (-(g_c(2,i)*(dx**2/dx**3)+g_c(1,i)*(dx/dx**3)))+g_dy/dx**3
  c(3,i) = (dy-c(1,i)*dx-c(2,i)*dx**2)/dx**3
end do
do j = 1, nn
  if (xx(j) .le. x(1)) then
    g_yy(j) = g_c(1,1)*(xx(j)-x(1))+g_y(1)
    yy(j) = y(1)+c(1,1)*(xx(j)-x(1))
  else if (xx(j) .ge. x(n)) then
    del = x(n)-x(n-1)
    g_der = 3*g_c(3,n-1)*del**2+2*g_c(2,n-1)*del+g_c(1,n-1)
    der = c(1,n-1)+2.d0*c(2,n-1)*del+3.d0*c(3,n-1)*del**2
    g_yy(j) = g_der*(xx(j)-x(n))+g_y(n)
    yy(j) = y(n)+der*(xx(j)-x(n))
  else
    do i = 1, n
      if (x(i) .le. xx(j)) then
        m = i
      endif
    end do
    d = xx(j)-x(m)
    g_yy(j) = g_c(3,m)*d*d*d+g_c(2,m)*d*d+g_c(1,m)*d+g_y(m)
    yy(j) = y(m)+d*(c(1,m)+d*(c(2,m)+d*c(3,m)))
  endif
end do

end subroutine da_splinx_lin
