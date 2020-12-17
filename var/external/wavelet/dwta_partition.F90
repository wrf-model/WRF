! Author: Aime' Fournier
! E-mail: fournier@ucar.edu

 SUBROUTINE dwta_partition(iv,iw,ia,lm,lh,lg)
!
! create indexes for dwtai()
!
 IMPLICIT NONE
 INTEGER j,lg,lh,lm			! lg,lh=filter lengths
 INTEGER, DIMENSION(0:lm) :: iv		! in iv[0]: data length/out iv[1:lm]: v-space length
 INTEGER, DIMENSION(0:lm) :: iw		! out iw[0:lm]: w-space lengths
 INTEGER, DIMENSION(0:lm) :: ia		! out: ia[0] transform length; ia[1:lm] w starts
!
 DO j=1,lm				! loop to maximum level:
    iv(j)=FLOOR(.5*(iv(j-1)+lh))
    iw(j)=FLOOR(.5*(iv(j-1)+lg))
 ENDDO
 iw(0)=0				! no w space at level 0
 ia(lm)=iv(lm)				! start of largest-scale w
 DO j=lm,1,-1
    ia(j-1)=ia(j)+iw(j)
 ENDDO
 END SUBROUTINE dwta_partition
