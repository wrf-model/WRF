SUBROUTINE tred2(a,n,np,d,e)
!USE nrutil, ONLY : assert_eq,outerprod
IMPLICIT NONE
INTEGER, PARAMETER :: sp = 8
INTEGER n,np
REAL(SP), DIMENSION(np,np), INTENT(INOUT) :: a
REAL(SP), DIMENSION(np), INTENT(OUT) :: d,e
!LOGICAL(LGT), OPTIONAL, INTENT(IN) :: novectors
INTEGER :: i,j,l,k
REAL(SP) :: f,g,h,hh,scale
REAL(SP), DIMENSION(size(a,1)) :: gg
!LOGICAL(LGT) :: yesvec
!n=assert_eq(size(a,1),size(a,2),size(d),size(e),’tred2’)
!if (present(novectors)) then
!yesvec=.not. novectors
!else
!yesvec=.true.
!end if
do i=n,2,-1
l=i-1
h=0.0
if (l > 1) then
scale=sum(abs(a(i,1:l)))
if (scale == 0.0) then 
e(i)=a(i,l)
else
a(i,1:l)=a(i,1:l)/scale 
h=sum(a(i,1:l)**2) 
f=a(i,l)
g=-sign(sqrt(h),f)
e(i)=scale*g
h=h-f*g 
a(i,l)=f-g 
a(1:l,i)=a(i,1:l)/h 
do j=1,l 
e(j)=(dot_product(a(j,1:j),a(i,1:j)) &
+dot_product(a(j+1:l,j),a(i,j+1:l)))/h
end do
f=dot_product(e(1:l),a(i,1:l))
hh=f/(h+h) 
e(1:l)=e(1:l)-hh*a(i,1:l)

do j=1,l 
a(j,1:j)=a(j,1:j)-a(i,j)*e(1:j)-e(j)*a(i,1:j)
end do
end if
else
e(i)=a(i,l)
end if
d(i)=h
end do
d(1)=0.0
e(1)=0.0
do i=1,n 

l=i-1
if (d(i) /= 0.0) then
gg(1:l)=matmul(a(i,1:l),a(1:l,1:l))
!a(1:l,1:l)=a(1:l,1:l)-outerprod(a(1:l,i),gg(1:l))
do j=1,l
do k=1,l
   a(k,j)=a(k,j)-gg(j)*a(k,i)
end do
end do
end if
d(i)=a(i,i)
a(i,i)=1.0 
a(i,1:l)=0.0
a(1:l,i)=0.0
!else
!d(i)=a(i,i)
!end if
end do
END SUBROUTINE tred2
