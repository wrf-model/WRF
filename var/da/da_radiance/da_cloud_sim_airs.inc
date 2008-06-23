subroutine da_cloud_sim_airs(KINDIC,KDIM,PX,PF,PG,IZS,RZS,DZS)

! Purpose :
! -------
! Simulate the cloud as a linear combination of grey clouds at model levels

! Interface :
! ---------
! KINDIC
! KDIM           : Dimension of cloud fraction variable
! PX             : Cloud fraction variable             -> Input
! PF             : Fitness Error
! PG             : Gradient of cloud fraction variable -> Output

! Externals :
! ---------

! Method :
! ------

! Reference :
! ---------

! Author :
! ------
! 01/08/2005  Thomas Auligne         *ECMWF*

! Modifications :
! -------------

! ---------------------------------------------------------------------------------------------

IMPLICIT NONE

!! Parameters !!
INTEGER,INTENT(IN)      :: KINDIC 
INTEGER,INTENT(IN)      :: KDIM 
INTEGER,INTENT(IN)      :: IZS(2)
double precision   ,INTENT(INOUT)   :: PX(KDIM)
double precision   ,INTENT(OUT)     :: PF 
double precision   ,INTENT(OUT)     :: PG(KDIM)
real               ,INTENT(IN)      :: RZS(kdim*4)      ! Eigenvectors
DOUBLE PRECISION   ,INTENT(IN)      :: DZS(IZS(1)*KDIM) ! AMAT
!! Local arrays !!
INTEGER                 :: JCH, JLEV, nchan, neignvec
REAL                    :: ZNORM_PG, ZCLR, ZDCLR, eignvec(kdim,izs(2))
REAL                    :: AMAT(IZS(1),KDIM)

!IF (KINDIC == 1) RETURN
 PF       = 0.0
 PG       = 0.0
 nchan    = izs(1)
 neignvec = izs(2)
!eignvec  = RESHAPE(rzs,(/KDIM,neignvec/))

 AMAT     = RESHAPE(DZS(1:NCHAN*KDIM),(/NCHAN,KDIM/))
!PX       = MATMUL(eignvec,MATMUL(TRANSPOSE(eignvec),PX))
 PX(KDIM) = 1.0 - SUM(PX(1:kdim-1))
 where (PX < 0.0) PX = 0.0
 where (PX > 1.0) PX = 1.0
 
 
 DO JCH=1,NCHAN 
   PF = PF + 0.5 * (SUM(PX*AMAT(JCH,:)) - 1.0)**2
   DO JLEV=1,KDIM
   PG(JLEV) = PG(JLEV) + &
             (AMAT(JCH,JLEV)-AMAT(JCH,KDIM)) * (SUM(PX*AMAT(JCH,:)) - 1.0)
   ENDDO
 ENDDO

!write(*,'(a,2f10.2,50f6.1)') 'ACD_PX',PF,sqrt(sum(pg**2)),sum(px(1:kdim-1))*100.,PX*100.

end subroutine da_cloud_sim_airs
