subroutine da_splinx_adj ( n, x, y, a_y, nn, xx, c, a_c, a_yy )
implicit none
integer :: n, nn
integer :: i, j, m, i1
real :: a_c(3,n), a_y(n), a_yy(n), c(3,n), x(n), xx(n), y(n)
real :: a_der, a_dy, a_dyl, a_dyr, a_dyx, dyx
real :: ch(3,n), ch_1(3,n), ch_2(3,n), ch_3(3,n), ch_4(3,n), ch_5(3,n), ch_6(3,n)
real :: d, del, dx, dxl, dxr, dy, dyl, dyr

!----------------------------------------------
! RESET LOCAL ADJOINT VARIABLES
!----------------------------------------------
a_der = 0.
a_dy = 0.
a_dyl = 0.
a_dyr = 0.
a_dyx = 0.

  ch(:,:) = c(:,:)

c(1,1) = 0.
c(2,1) = 0.
c(2,n) = 0.
dxr = x(2)-x(1)
dyr = (y(2)-y(1))/dxr

do i = 2, n-1
  dxl = dxr
  dxr = x(i+1)-x(i)
  dyl = dyr
  dyr = (y(i+1)-y(i))/dxr
  dx = dxr+dxl
  dy = (dyr-dyl)/dx
  c(1,i) = -(dxr/(2.*dx+dxl*c(1,i-1)))

  ch_2(1,i-1) = c(1,i-1)
  ch_6(2,i-1) = c(2,i-1)

  c(2,i) = (6.*dx*dy-dxl*c(2,i-1))/(2.*dx+dxl*c(1,i-1))
end do

  do i=n-1,2,-1

   ch_4(2,i+1)=c(2,i+1)
   ch_5(1,i)=c(1,i)

   c(2,i)=c(1,i)*c(2,i+1)+c(2,i)
  end do

do j = 1, nn
  a_der = 0.
  if (xx(j) .le. x(1)) then
    a_c(1,1) = a_c(1,1)+a_yy(j)*(xx(j)-x(1))
    a_y(1) = a_y(1)+a_yy(j)
    a_yy(j) = 0.
  else if (xx(j) .ge. x(n)) then
    del = x(n)-x(n-1)
    a_der = a_der+a_yy(j)*(xx(j)-x(n))
    a_y(n) = a_y(n)+a_yy(j)
    a_yy(j) = 0.
    a_c(3,n-1) = a_c(3,n-1)+3*a_der*del**2
    a_c(2,n-1) = a_c(2,n-1)+2*a_der*del
    a_c(1,n-1) = a_c(1,n-1)+a_der
    a_der = 0.
  else
    do i = 1, n
      if (x(i) .le. xx(j)) then
        m = i
      endif
    end do
    d = xx(j)-x(m)
    a_c(3,m) = a_c(3,m)+a_yy(j)*d*d*d
    a_c(2,m) = a_c(2,m)+a_yy(j)*d*d
    a_c(1,m) = a_c(1,m)+a_yy(j)*d
    a_y(m) = a_y(m)+a_yy(j)
    a_yy(j) = 0.
  endif
end do

do i = n-1,1,-1
  dx = x(i+1)-x(i)
  a_c(2,i) = a_c(2,i)-a_c(3,i)*(dx**2/dx**3)
  a_c(1,i) = a_c(1,i)-a_c(3,i)*(dx/dx**3)
  a_dy = a_dy+a_c(3,i)/dx**3
  a_c(3,i) = 0.
  a_c(2,i) = 0.5*a_c(2,i)
  a_c(2,i+1) = a_c(2,i+1)-0.16666667*a_c(1,i)*dx
  a_c(2,i) = a_c(2,i)-0.33333333*a_c(1,i)*dx
  a_dyx = a_dyx+a_c(1,i)
  a_c(1,i) = 0.
  a_dy = a_dy+a_dyx/dx
  a_dyx = 0.
  a_y(i+1) = a_y(i+1)+a_dy
  a_y(i) = a_y(i)-a_dy
  a_dy = 0.
end do

do i = 2, n-1
!do i = n-1,2,-1
   c(2,i+1)=ch_4(2,i+1)
   c(1,i)=ch_5(1,i)

  a_c(1,i) = a_c(1,i)+a_c(2,i)*c(2,i+1)
  a_c(2,i+1) = a_c(2,i+1)+a_c(2,i)*c(1,i)
end do

do i = n-1, 2, -1

  c(:,:) = ch(:,:)

  c(1,1) = 0.
  c(2,1) = 0.
  c(2,n) = 0.
  dxr = x(2)-x(1)
  dyr = (y(2)-y(1))/dxr
  do i1 = 2, i-1
    dxl = dxr
    dxr = x(i1+1)-x(i1)
    dyl = dyr
    dyr = (y(i1+1)-y(i1))/dxr
    dx = dxr+dxl
    dy = (dyr-dyl)/dx
    c(1,i1) = -(dxr/(2.*dx+dxl*c(1,i1-1)))
    c(2,i1) = (6.*dx*dy-dxl*c(2,i1-1))/(2.*dx+dxl*c(1,i1-1))
  end do
  ch_1(:,:) = c(:,:)

  dxl = dxr
  dxr = x(i+1)-x(i)
  dyl = dyr
  dyr = (y(i+1)-y(i))/dxr
  dx = dxr+dxl
  dy = (dyr-dyl)/dx
  c(1,i) = -(dxr/(2.*dx+dxl*c(1,i-1)))


  a_c(2,i-1) = a_c(2,i-1)-a_c(2,i)*(dxl/(2.*dx+dxl*c(1,i-1)))
  a_c(1,i-1) = a_c(1,i-1)-a_c(2,i)*((6.*dx*dy-dxl*c(2,i-1))*dxl/((2.*dx+dxl*c(1,i-1))*(2.*dx+dxl*c(1,i-1))))
  a_dy = a_dy+a_c(2,i)*(6*dx/(2.*dx+dxl*c(1,i-1)))
  a_c(2,i) = 0.
  c(:,:) = ch_1(:,:)
  a_c(1,i-1) = a_c(1,i-1)+a_c(1,i)*(dxr*dxl/((2.*dx+dxl*c(1,i-1))*(2.*dx+dxl*c(1,i-1))))
  a_c(1,i) = 0.
  a_dyl = a_dyl-a_dy/dx
  a_dyr = a_dyr+a_dy/dx
  a_dy = 0.
  a_y(i+1) = a_y(i+1)+a_dyr/dxr
  a_y(i) = a_y(i)-a_dyr/dxr
  a_dyr = 0.
  a_dyr = a_dyr+a_dyl
  a_dyl = 0.
end do

dxr = x(2)-x(1)
a_y(2) = a_y(2)+a_dyr/dxr
a_y(1) = a_y(1)-a_dyr/dxr
a_dyr = 0.
a_c(2,n) = 0.
a_c(2,1) = 0.
a_c(1,1) = 0.


end subroutine da_splinx_adj
