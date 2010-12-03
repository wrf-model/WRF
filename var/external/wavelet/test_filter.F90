! Author: Aime' Fournier
! E-mail: fournier@ucar.edu

#include "realt.h"

 SUBROUTINE test_filter(nam,ran,kinh,king,h,g)
 IMPLICIT NONE
 CHARACTER nam				! wavelet name
 INTEGER k,king,kinh,n,ng,nl,ran
 realt, DIMENSION(ran) :: g		! wavelet hpf
 realt, DIMENSION(ran) :: h		! wavelet lpf
 realt :: fg=-1E37,fl=1E37,fs=0.
 realt, DIMENSION(:), ALLOCATABLE :: p
!
 PRINT '("Filter-convolution ",A1,I2.2,"[",I1,"]*",A1,I2.2,"[",I1,"]:")',&
       nam,ran,kinh,nam,ran,king
 nl=CEILING(.5*(1-ran))			! least n
 ng=FLOOR(  .5*(ran-1))			! greatest n
 ALLOCATE(p(1:ng-nl+1))
 p=0.
 PRINT '(A,A38,99(I7,:))',CHAR(9),"n:",(n,n=nl,ng)
 DO n=nl,ng
    DO k=MAX(0,-2*n),MIN(ran-1,ran-1-2*n)
       p(n-nl+1)=p(n-nl+1)+h(k+1)*g(k+1+2*n)
    ENDDO
    IF( n==0 .AND. kinh==king ) p(n-nl+1)=p(n-nl+1)-1.
    fl=MIN(fl,p(n-nl+1))
    fg=MAX(fg,p(n-nl+1))
 ENDDO
 PRINT '(A,ES8.1,"<orthogonality error<",ES8.1,":",99(F7.1,:))',CHAR(9),fl,fg,p
 DEALLOCATE(p)
 END SUBROUTINE test_filter
