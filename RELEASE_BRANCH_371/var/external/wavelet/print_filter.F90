! Author: Aime' Fournier
! E-mail: fournier@ucar.edu

#include "realt.h"

 SUBROUTINE print_filter(nam,ran,kin,f)
 IMPLICIT NONE
 CHARACTER nam				! wavelet name
 INTEGER k,kin,ran
 realt, DIMENSION(ran) :: f		! wavelet filter
 realt fs,fse
!
 fs=0.
 IF( kin==0 ) THEN
    fse=SQRT(2.)
 ELSEIF( kin==1 ) THEN
    fse=0.
 ELSE
    STOP
 ENDIF
 WRITE(*,                                                              &
       '("Filter ",A1,I2.2,"[",I1,"] has alpha=0, omega=",I2,", f={",99(1X,F10.6,:))',&
       ADVANCE="NO")nam,ran,kin,ran-1,f
 DO k=1,ran
    fs=fs+f(k);				! f sum
 ENDDO
 PRINT '("} and sum-error ",ES9.2)',fs-fse
 END SUBROUTINE print_filter
