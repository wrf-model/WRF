SUBROUTINE da_gpseph_rays ( gps_rays, gpseph )

!---------------------------------------------------------------------------------------
!  Purpose: calculate GPS ray path. (Sokolovskiy et al., 2005)
!---------------------------------------------------------------------------------------

   implicit none

   type (gpsrays_type), intent(inout) :: gps_rays
   type (gpseph_type),  intent(inout) :: gpseph

   real,dimension(kds:kde) :: mean_h

   integer :: i, j, k, l, m, i1, i2, i3, nbot, ntop
   integer :: is, ie, js, je, ks, ke
   integer :: i1l,i2l,i1r,i2r
   integer :: ip1,ip2,ip3,je2
   real :: dtr, rtd, step, dst, rox, glat, glon, h, rxabs
   real,dimension(ids:ide,jds:jde) :: x2, y2
   real,dimension(3):: rp, rn, rr, rx
   real,dimension(2) :: w1
   real,dimension(2,2) :: w2
   real :: ray_length
   real :: xlat, xlon
   real :: requator, rpolar, temp_lat

   mean_h(:) = global_h_mean(:) !km

   dtr  = pi/180.
   rtd  = 180./pi
   step = gps_ray_path_step

   is = ids
   ie = ide
   js = jds
   je = jde
   ks = kds
   ke = kde

   do j=js, je
      do i=is, ie
         x2(i,j)= global_lat(i,j)
         if (global_lon(i,j) >= 0.) then
            y2(i,j)= global_lon(i,j)
         else
            y2(i,j)= global_lon(i,j)+360.
         endif
      enddo
   enddo

   if ( gpseph%rfict < 0 ) then
      ! calculate rfict if it is not available from the obs input
      requator = 6378.1363
      rpolar   = 6356.7516
      temp_lat = dtr * gpseph%lat(1)
      gpseph%rfict = sqrt((requator**2*rpolar**2)/          &
                     (requator**2*(sin(abs(temp_lat)))**2+  &
                      rpolar**2*(cos(abs(temp_lat)))**2))
   end if

   nbot = gpseph%level1
   ntop = gpseph%level2
   gps_rays%nbot = nbot
   gps_rays%ntop = ntop

   if ( .not. allocated(gps_rays%je2) )    allocate ( gps_rays%je2 (nbot:ntop) )
   if ( .not. allocated(gps_rays%ip123) )  allocate ( gps_rays%ip123 (nbot:ntop) )
   if ( .not. allocated(gps_rays%ilocal) ) allocate ( gps_rays%ilocal (nbot:ntop) )

   do i=nbot,ntop

      !* rp is a vector along tangent point direction
      !* rn is a vector normal to rp along the meridian
      !* transform coordiante from sphere to cartesian coordinate
      !* A is an angle from vector to x-y plane,
      !* B is an angle from x-axis to the project of the vector on x-y plane
      !* x=r*cos(A)*cos(B)
      !* y=r*cos(A)*sin(B)
      !* z=r*sin(A)

      rp(1)=(gpseph%rfict+mean_h(i))*cos(dtr*gpseph%lat(i)) &
                                    *cos(dtr*gpseph%lon(i))
      rp(2)=(gpseph%rfict+mean_h(i))*cos(dtr*gpseph%lat(i)) &
                                    *sin(dtr*gpseph%lon(i))
      rp(3)=(gpseph%rfict+mean_h(i))*sin(dtr*gpseph%lat(i))
      rn(1)=-sin(dtr*gpseph%lat(i))*cos(dtr*gpseph%lon(i))
      rn(2)=-sin(dtr*gpseph%lat(i))*sin(dtr*gpseph%lon(i))
      rn(3)=+cos(dtr*gpseph%lat(i))
      CALL spin(rn,rp,-dtr*gpseph%azim(i),rr)
      CALL rnorm(rr)

      rox=sqrt(rp(1)**2+rp(2)**2)
      glat=rtd*atan(rp(3)/rox)
      glon=rtd*atan2(rp(2),rp(1))
      CALL absv(rp,rxabs)
      h = rxabs - gpseph%rfict
      ray_length=sqrt((mean_h(ke)-h)*(2*gpseph%rfict+mean_h(ke)+h))
      je2=int(ray_length/step)+1
      gps_rays%je2(i)=je2

      if ( .not. allocated(gps_rays%ip123(i)%i1) ) allocate ( gps_rays%ip123(i)%i1(    -je2:je2) )
      if ( .not. allocated(gps_rays%ip123(i)%i2) ) allocate ( gps_rays%ip123(i)%i2(    -je2:je2) )
      if ( .not. allocated(gps_rays%ip123(i)%i3) ) allocate ( gps_rays%ip123(i)%i3(    -je2:je2) )
      if ( .not. allocated(gps_rays%ip123(i)%h) )  allocate ( gps_rays%ip123(i)%h (    -je2:je2) )
      if ( .not. allocated(gps_rays%ip123(i)%w1) ) allocate ( gps_rays%ip123(i)%w1(2,  -je2:je2) )
      if ( .not. allocated(gps_rays%ip123(i)%w2) ) allocate ( gps_rays%ip123(i)%w2(2,2,-je2:je2) )

      gps_rays%ip123(i)%h(0) = h

      ip1=0
      ip2=0
      ip3=0
      if ( glon < 0.0 ) glon = glon + 360.0
      CALL find2(ie,je,rxabs,x2,y2,glat,glon,ip1,ip2,w2)
      gps_rays%ip123(i)%i1(0) = ip1
      gps_rays%ip123(i)%i2(0) = ip2
      gps_rays%ip123(i)%w2(1:2,1:2,0) = w2

      CALL find1(ke,mean_h,h,ip3,w1)
      gps_rays%ip123(i)%i3(0) = ip3
      gps_rays%ip123(i)%w1(1:2,0) = w1

      i1l=0
      i2l=0
      i1r=0
      i2r=0
      i1= 0
      i2= 0

      DO j=1,je2
         !* calculate S from TP point and integrate to different direction (WRF)
         !* S is asymmetric
         do k=-1,+1,2
            dst=step*j*k
            do l=1,3
               rx(l)=rp(l)+dst*rr(l)
            end do
            !* transform coordiante from cartesian to sphere coordinate
            rox=sqrt(rx(1)**2+rx(2)**2)
            glat=rtd*atan(rx(3)/rox)
            glon=rtd*atan2(rx(2),rx(1))
            CALL absv(rx,rxabs)
            h = rxabs - gpseph%rfict
            gps_rays%ip123(i)%h(k*j) = h

            IF (h <= mean_h(ke-1)) then
               if (k == -1) then
                  i1=i1l
                  i2=i2l
               endif
               if (k == 1) then
                  i1=i1r
                  i2=i2r
               endif
               if ( glon < 0.0 ) glon = glon + 360.0
               CALL find2(ie,je,rxabs,x2,y2,glat,glon,i1,i2,w2)
               gps_rays%ip123(i)%i1(k*j) = i1
               gps_rays%ip123(i)%i2(k*j) = i2
               gps_rays%ip123(i)%w2(1:2,1:2,k*j) = w2

               ! avoid boundary effect
               if (i1==1 .or. i1>=ie-1 .or. i2==1 .or. i2>=je-1) then
                  w2(:,:)=0.
                  gps_rays%ip123(i)%w2(1:2,1:2,k*j) = w2
               endif

               ! avoid mountain effect
               CALL find1(ke,mean_h,h,i3,w1)
               gps_rays%ip123(i)%i3(k*j) = i3
               gps_rays%ip123(i)%w1(1:2,k*j) = w1
               if (h < (global_terr(i1,i2)*0.001)) then
                  w1(:)=0.
                  gps_rays%ip123(i)%w1(1:2,k*j) = w1
               endif

               if (k == -1) then
                  i1l=i1
                  i2l=i2
               endif
               if (k == 1) then
                  i1r=i1
                  i2r=i2
               endif
            END IF
         end do ! from left to right
      END DO !from the begining to the end

   end do !nbot,ntop loop

END SUBROUTINE da_gpseph_rays

! ................................................................

SUBROUTINE spin ( v1, vs, a, v2 )
! *****************************************************
!  S.V.Sokolovskiy
! ***************************************************
!  This subroutine rotates vector V1 around vector VS
!  at angle A. V2 is the vector after the rotation.
! ***************************************************
       implicit  none
       integer :: i, j, k
       real ::  vsabs, a1, a2, a3
       real,intent(in)::a
       real, dimension(3),intent(in)::v1
       real, dimension(3),intent(in)::vs
       real, dimension(3)::vsn
       real, dimension(3),intent(out)::v2
       real, dimension(3,3) :: s

! Calculation of the unit vector around which
! the rotation should be done.

       vsabs=sqrt(vs(1)**2+vs(2)**2+vs(3)**2)
       do 1 i=1,3
1      vsn(i)=vs(i)/vsabs

!-------------------------------------------------------------------------------------
! Calculation of the rotation matrix.
! Any linear transformation of a vector A can be
! represented via a matrix M:
!    -           -
!   | a11 a12 a13 |
! A=| a21 a22 a23 |
!   | a31 a32 a33 |
!    -           -
!              -     -                 -                 -             -          -
!             | 1 0 0 |               | c1**2 c1*c2 c1*c3 |           |  0 -c3  c2 |
!  =cos(delta)| 0 1 0 |+(1-cos(delta))| c2*c1 c2**2 c2*c3 |+sin(delta)|  c3  0 -c1 |
!             | 0 0 1 |               | c3*c1 c3*c2 c3**2 |           | -c2  c1  0 |
!              -     -                 -                 -             -          -
!
!--------------------------------------------------------------------------------------
       a1=cos(a)
       a2=1.-a1
       a3=sin(a)
       s(1,1)=a2*vsn(1)*vsn(1)+a1
       s(1,2)=a2*vsn(1)*vsn(2)-a3*vsn(3)
       s(1,3)=a2*vsn(1)*vsn(3)+a3*vsn(2)
       s(2,1)=a2*vsn(2)*vsn(1)+a3*vsn(3)
       s(2,2)=a2*vsn(2)*vsn(2)+a1
       s(2,3)=a2*vsn(2)*vsn(3)-a3*vsn(1)
       s(3,1)=a2*vsn(3)*vsn(1)-a3*vsn(2)
       s(3,2)=a2*vsn(3)*vsn(2)+a3*vsn(1)
       s(3,3)=a2*vsn(3)*vsn(3)+a1

! Calculation of the rotated vector.

       do 2 i=1,3
2      v2(i)=s(i,1)*v1(1)+s(i,2)*v1(2)+s(i,3)*v1(3)

END SUBROUTINE spin

! .............................................................

SUBROUTINE rnorm ( v )
       implicit none
       integer :: i, j, k
       real :: s
       real, dimension(3),intent(inout):: v

       s=sqrt(v(1)**2+v(2)**2+v(3)**2)
       do i=1,3
       v(i)=v(i)/s
       end do
END SUBROUTINE rnorm

! ...............................................................

SUBROUTINE absv ( r, rabs )
!*****************************************************************
!* The absolute value of a vector
!*****************************************
       implicit none
       real,intent(out)::rabs
       real,dimension(3),intent(in):: r

       rabs=sqrt(r(1)**2+r(2)**2+r(3)**2)

END SUBROUTINE absv

! ..............................................................

SUBROUTINE find1 ( n, x, xx, n1, w )
       implicit none
       integer :: i, j, k, n2
       integer,intent(in)::n
       integer,intent(out):: n1
       real :: dst, dst1, ww
       real,intent(in)::xx
       real, dimension(n),intent(in)::x
       real, dimension(2),intent(out)::w
!  n:  the number of total model levels
!=====================================================================
!       dst=1.0e+38
!       do i=1,n-1
!
!       dst1=(x(i)-xx)**2+(x(i+1)-xx)**2
!       if (dst1.lt.dst) then
!       dst=dst1
!       n1=i
!       end if
!       end do
!
!       w(1)=abs(x(n1+1)-xx)
!       w(2)=abs(x(n1)-xx)
!       ww=w(1)+w(2)
!       w(1)=w(1)/ww
!       w(2)=w(2)/ww
!=====================================================================
      do i=1,n-1
      if ((xx.ge.x(i)).and.(xx.le.x(i+1))) then
      n1=i
      n2=i+1
      ww=x(n2)-x(n1)
      w(1)=(x(n2)-xx)/ww
      w(2)=(xx-x(n1))/ww
      goto 1
      end if
      end do
1     continue
      if (xx.lt.x(1)) then
!  for xx outside the model bottom
      n1= 1
      w(1)= 1.0
      w(2)= 0.0
      endif
      if (xx.gt.x(n)) then
!  for xx outside the model top
      n1= n-1
      w(1)= 0.0   ! weighting on n1 level
      w(2)= 1.0   ! weighting on n1+1 level (i.e., the top level)
      endif
!=====================================================================

END SUBROUTINE find1
! ...........................................................................

SUBROUTINE find2 ( ide, jde, rfict, x2, y2, xp, yp, i1, j1, w )
      IMPLICIT NONE
      integer :: i, j, k, l, m, is, ie, js, je
      integer,intent(inout)::i1, j1
      integer :: i2,j2   ! for test finding points
      integer :: ide, jde
      real :: dtr, dst, dst1, dst2, tmp, ww
      real :: xpd,ypd,xps,yps,zps,x2d,y2d,x2s,y2s,z2s,xdis,ydis,zdis
      real,intent(in)::xp,yp,rfict
      real, dimension(ide,jde),intent(in):: x2, y2
      real, dimension(2,2),intent(out) :: w
!
      dtr=pi/180.

!  transfer model xlat, xlon to spherical x and y
!

      if((i1 > 0) .and. (j1 > 0))then

       is= max0(i1-3,1)
       ie= min0(i1+3,ide-1)
       js= max0(j1-3,1)
       je= min0(j1+3,jde-1)

      else

       is=1
       ie=ide-1
       js=1
       je=jde-1

      end if

       dst=1.e29
       do i=is,ie
       do j=js,je

       dst1=0.0
       do l=i,i+1
       do m=j,j+1
       x2d= x2(l,m)-xp
       y2d= y2(l,m)-yp
       dst1=dst1+(x2d*x2d+y2d*y2d)
       end do
       end do
       if (dst1 < dst) then
       dst=dst1
       i1=i
       j1=j
       end if

       end do
       end do

!**********************************************************
! for test finding points
!
!       dst=1.e29
!       do i=1,xb%ide-1
!       do j=1,xb%jde-1
!
!       dst1=0.0
!       do l=i,i+1
!       do m=j,j+1
!       x2d= x2(l,m)-xp2
!       y2d= y2(l,m)-yp2
!       dst1=dst1+(x2d*x2d+y2d*y2d)
!       end do
!       end do
!       if (dst1 < dst) then
!       dst=dst1
!       i2=i
!       j2=j
!       end if
!
!       end do
!       end do
!
!       if ((i1 /= i2).or.(j1 /= j2))then
!       write(225,*) 'test for finding points'
!       write(225,*) 'i1,i2=',i1,i2
!       write(225,*) 'j1,j2=',j1,j2
!       endif
!**********************************************************
!  calculate weighting
       xpd= xp*dtr
       ypd= yp*dtr
       xps= rfict*cos(xpd)*cos(ypd)
       yps= rfict*cos(xpd)*sin(ypd)
       zps= rfict*sin(xpd)
       do i=1,2
       do j=1,2
       x2d= x2(i1+i-1,j1+j-1)*dtr
       y2d= y2(i1+i-1,j1+j-1)*dtr
       x2s= rfict*cos(x2d)*cos(y2d)
       y2s= rfict*cos(x2d)*sin(y2d)
       z2s= rfict*sin(x2d)
       xdis= x2s-xps
       ydis= y2s-yps
       zdis= z2s-zps
       tmp= xdis*xdis + ydis*ydis + zdis*zdis
       w(i,j)=1.0/sqrt(tmp)
       end do
       end do


       ww= 0.
       do i=1,2
       do j=1,2
       ww= ww+w(i,j)
       end do
       end do

       do i=1,2
       do j=1,2
       w(i,j)= w(i,j)/ww
       end do
       end do

END SUBROUTINE find2
