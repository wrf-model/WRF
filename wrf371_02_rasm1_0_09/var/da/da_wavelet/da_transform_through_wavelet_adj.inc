SUBROUTINE da_transform_through_wavelet_adj(grid,mz,wsd,v,vv)
!
! Purpose: Horizontal control-var. transform-adjoint vv -> v through dwtai_w.c.
! Author: Aime' Fournier
!
! v(:nq,m) = MATMUL(TRANSPOSE(uh(:,:,m)),vv(:ni*nj,m)), m the vert. mode,
! uh(ij,q,m) = w(q,ij)*wsd(q,m)/p_x(ij) the m'th horiz. CVT matrix,
! w(q,:ni*nj) the q'th of nq horizontal 2D wavelets,
! wsd(q,m) the standard deviation of the q'th wavelet coefficient,
! p_x(ij)**2 the horizontal area in ij. 
!
! Method:
!  3) Diagonal divide by p_x(:ni*nj), cf. gen_be_stage4_regional.
!  2) MatMul by w(:nq,:ni*nj) using dwtai_w();
!  1) Diagonal multiply by wsd(:nq,m);
!
 IMPLICIT NONE
 TYPE(domain), INTENT( IN)::grid
 REAL,         INTENT( IN)::vv(ims:ime,jms:jme,kms:kme)		! Grid point/EOF equivalent.
 INTEGER,      INTENT( IN)::mz					! Vertical truncation.
 REAL,         INTENT( IN)::wsd(nij(0,1,2),nij(0,0,2),mz)	! Wavelet standard deviation.
 REAL,         INTENT(OUT)::v(nij(0,0,2)*nij(0,1,2)*mz)		! Field to be transformed.
 INTEGER                  ::dj(0:1),dk(0:1),dv,i,j,m,n
 LOGICAL, SAVE            ::call1=.TRUE.

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
 dv = mz*dk(1)				! variable 3D length.
 n = 1					! n <= 1 + mz:
 DO m = 1, 1+dv-dk(1), dk(1)		! mz vertical-modes loop:
    j = m				! m <= j <= m+dk0-dj0:
    DO i = jts, jts+nij(0,0,0)-1	! Pseudo-northward loop:
       v(j:j+dj(0)-1) = vv(its:ite,i,n)
!      [3.0] Apply box-area factor:
       v(j:j+dj(0)-1) = v(j:j+dj(0)-1)/SQRT(grid%xb%grid_box_area(1:dj(0),i))
       j = j+dj(1)
    ENDDO

!   [2.1] Perform dwtai() in pseudo-northward directions:
    DO i = m, m+dj(0)-1			! Pseudo-eastward loop:
       CALL dwtai_w(namw//CHAR(0),lf,v(i:i+dk(1)-dj(1):dj(1)),ws,nij(:,0,0),nij(:,0,2),nb)
    ENDDO

!   [2.0] Perform dwtai() in pseudo-eastward directions:
    DO j = m, m+dk(1)-dj(1), dj(1)	! Pseudo-northward loop:
       CALL dwtai_w(namw//CHAR(0),lf,v(j:j+dj(1)-   1       ),ws,nij(:,1,0),nij(:,1,2),nb)
    ENDDO
!   [1.0] Multiply by wavelet standard deviations:
    v(m:m+dk(1)-1) = RESHAPE(wsd(:,:,n),(/dk(1)/))*v(m:m+dk(1)-1)
    n = n+1				! Increment mode.
 ENDDO					! m (& n) loop.

 IF( call1 )THEN
    PRINT'(a,": ",ES7.1,"<grid_box_area~",ES7.1,"<",ES7.1)', &
       __FILE__,MINVAL(grid%xb%grid_box_area(1:dj(0),jts:jts+nij(0,0,0)-1)), &
              SQRT(SUM(grid%xb%grid_box_area(1:dj(0),jts:jts+nij(0,0,0)-1)**2)/(dj(0)*nij(0,0,0))), &
                MAXVAL(grid%xb%grid_box_area(1:dj(0),jts:jts+nij(0,0,0)-1))
    call1=.FALSE.
 ENDIF

#else
    PRINT'(a,": must compile after setenv WAVELET 1")',__FILE__
    CALL wrf_abort
#endif

ENDSUBROUTINE da_transform_through_wavelet_adj
