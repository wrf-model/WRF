SUBROUTINE da_transform_through_wavelet_adj(grid,mz,wsd,sd,lv,v,vv)
!
! Purpose: Horizontal control-var. transform-adjoint vv -> v through dwtai_w.c.
! Author: Aime' Fournier
!
! v(:nq,m) = MATMUL(TRANSPOSE(uh(:,:,m)),vv(:ni*nj,m)), m the vert. mode,
! uh(ij,q,m) = w(q,ij)*wsd(q,m)*sd(ij,m)/p_x(ij) the m'th horiz. CVT matrix,
! w(q,:ni*nj) the q'th of nq horizontal 2D wavelets,
! wsd(q,m) the standard deviation of the q'th wavelet coefficient,
! sd(ij,m) the standard deviation in grid-box ij (or 1), and
! p_x(ij)**2 the horizontal area in ij. 
!
! Method:
!  3) Diagonal multiply by sd(ij,m)/p_x(:ni*nj), cf. gen_be_stage4_regional.
!  2) MatMul by w(:nq,:ni*nj) using dwtai_w();
!  1) Diagonal multiply by wsd(:nq,m);
!
 IMPLICIT NONE
 TYPE(domain), INTENT(IN):: grid
 TYPE(vp_type),INTENT(IN):: vv					! Grid point/EOF equivalent.
 INTEGER,      INTENT(IN):: lv					! Total CV dimension.
 INTEGER,      INTENT(IN):: mz(6)				! Vertical truncations.
 REAL,         INTENT(IN):: sd( nij(0,1,0),nij(0,0,0),4*mz(6)+1)! Field   standard deviation.
 REAL,         INTENT(IN):: wsd(nij(0,1,2),nij(0,0,2),4*mz(6)+1)! Wavelet standard deviation.
 REAL,        INTENT(OUT):: v(lv)				! Field to be transformed.
 INTEGER                 :: dj(0:1),dk(0:1),dv(5),f,i,j,l,m,n,p
 LOGICAL, SAVE           :: call1=.TRUE.


#ifdef WAVELET

 v=0.
 dj = nij(0,1,0:2:2)			! pseudo-eastward lengths.
 dk(0) = (nij(0,0,0)-1)*dj(1)+dj(0)	! horizontal model-var stride.
 dk(1) = nij(0,0,2)*dj(1)		! horizontal control-var size.
!
! 1D indexing of 3D ("//" & "/" for model- & cv-space):
!             _______________________________________
!            /m+dk1-dj1/ ...  /       / ... /m+dk1-1/
!           /_________/______/_______/_____/_______/
!          /         /      /       /     /       /__
!         /_________/______/_______/_____/_______/  /
!        /m+dk0-dj0/ ...  /m+dk0-1/ ... /       /__/
!       //_______//______//_____//_____/_______/  /__
!      //...    //      //     //     /       /__/  /
!     //_______//______//_____//_____/_______/  /__/
!    //m+dj1  //      //     //     /       /__/  /
!   //_______//______//_____//_____/_______/  /__/
!  //m      // ...  /m+dj0-1/ ... /m+dj1-1/__/  /
! //_______//______//_____//_____/_______/  /__/
!   //_______//______//_____//_____/_______/  /
!  //m-dk1  //      //     //     /       /__/
! //_______//______//_____//_____/_______/  /
!   //_______//______//_____//_____/_______/
!  //...    //      //     //     /       /
! //_______//______//_____//_____/_______/
!
 dv = mz(1:5)*dk(1)			! stride between variables.
 l = 0					! pointer to wsd and sd.
 p = 1					! pointer to variable f.
 DO f = 1, 5				! Fields loop:
    n = 1				! n <= 1 + mz(f):
    DO m = p, p+dv(f)-dk(1), dk(1)	! mz(f) vertical-modes loop:
       j = m				! m <= j <= m+dk0-dj0:
       DO i = jts, jts+nij(0,0,0)-1	! Pseudo-northward loop:
          SELECT CASE(f);
             CASE(1); v(j:j+dj(0)-1) = vv%v1(its:ite,i,n)
             CASE(2); v(j:j+dj(0)-1) = vv%v2(its:ite,i,n)
             CASE(3); v(j:j+dj(0)-1) = vv%v3(its:ite,i,n)
             CASE(4); v(j:j+dj(0)-1) = vv%v4(its:ite,i,n)
             CASE(5); v(j:j+dj(0)-1) = vv%v5(its:ite,i,n)
          ENDSELECT
!         [3.0] Apply box-area and std.-dev. factors:
!         v(j:j+dj(0)-1) = v(j:j+dj(0)-1)/SQRT(grid%xb%grid_box_area(1:dj(0),i))
          IF( do_normalize )v(j:j+dj(0)-1) = v(j:j+dj(0)-1)*sd(:,i,l+n)
          j = j+dj(1)
       ENDDO

!      [2.1] Perform dwtai() in pseudo-northward directions:
       DO i = m, m+dj(0)-1		! Pseudo-eastward loop:
          CALL dwtai_w(namw//CHAR(0),lf,v(i:i+dk(1)-dj(1):dj(1)),ws,nij(:,0,0),nij(:,0,2),nb)
       ENDDO
!      [2.0] Perform dwtai() in pseudo-eastward directions:
       DO j = m, m+dk(1)-dj(1), dj(1)	! Pseudo-northward loop:
          CALL dwtai_w(namw//CHAR(0),lf,v(j:j+dj(1)-   1       ),ws,nij(:,1,0),nij(:,1,2),nb)
       ENDDO
!      [1.0] Multiply by wavelet standard deviations:
       v(m:m+dk(1)-1) = RESHAPE(wsd(:,:,l+n),(/dk(1)/))*v(m:m+dk(1)-1)
       n = n+1				! Increment mode.
    ENDDO				! m (& n) loop.
    l = l+mz(6)				! wsd & sd pointer has fixed increments.
    p = p+dv(f)				! point to next variable.
 ENDDO					! Fields loop

 IF( call1 )THEN
    PRINT'(a,": use p_x=1 until gen_be_stage4_regional uses p_x/=1")',__FILE__
    call1=.FALSE.
 ENDIF

#else
    write(6,*) "Must compile with $WAVELET option "
    stop
#endif

ENDSUBROUTINE da_transform_through_wavelet_adj
