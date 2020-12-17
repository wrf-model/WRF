subroutine da_cloud_sim(KINDIC,KDIM,PX,PF,PG,IZS,RZS,DZS)

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
double precision   ,INTENT(INOUT)   :: PX(KDIM) !izs(2)
double precision   ,INTENT(OUT)     :: PF 
double precision   ,INTENT(OUT)     :: PG(KDIM)
real               ,INTENT(IN)      :: RZS(kdim*izs(2))      ! Eigenvectors
DOUBLE PRECISION   ,INTENT(IN)      :: DZS(IZS(1)*KDIM) ! AMAT
!! Local arrays !!
INTEGER                 :: JCH, ilev, JLEV, nchan, neignvec
REAL                    :: ZNORM_PG, ZCLR, ZDCLR, eignvec(kdim,izs(2)), eignval(izs(2))
double precision        :: AMAT(IZS(1),KDIM)
double precision        :: alpha, beta
double precision        :: zx(KDIM), zgx(KDIM, KDIM), zx_eof(KDIM)

!IF (KINDIC == 1) RETURN
 PF       = 0.0
 PG       = 0.0
 nchan    = izs(1)
 neignvec = izs(2)
 !eignvec  = RESHAPE(rzs(1:KDIM*neignvec),(/KDIM,neignvec/))
 !eignval  = rzs(KDIM*neignvec+1:(KDIM+1)*neignvec)

 AMAT     = RESHAPE(DZS(1:NCHAN*KDIM),(/NCHAN,KDIM/))
 PX(KDIM) = 1.0 - SUM(PX(1:kdim-1))
! where (PX < 0.0) PX = 0.0
! where (PX > 1.0) PX = 1.0

 !ZX_EOF   = MATMUL(eignvec,eignval*PX)
!!! ZX_EOF   = MATMUL(eignvec,MATMUL(TRANSPOSE(eignvec),PX))
 zx_eof = PX
 zx     = zx_eof

 ! Softmax (= multiple-logistic) variable transform
 !beta = 100.0
  
 !zx = exp(beta*zx_eof) / SUM(exp(beta*zx_eof))
 !do ilev = 1, kdim
 !   do jlev = 1, kdim
 !      zgx(ilev,jlev) = - beta * zx(ilev) * zx(jlev)
 !      if (ilev == jlev) zgx(ilev,jlev) =  zgx(ilev,jlev) + zx(ilev) * beta
 !   end do
 !end do
 
 DO JCH=1,NCHAN 
   PF = PF + 0.5 * (SUM(ZX*AMAT(JCH,:)) - 1.0)**2
   DO JLEV=1,KDIM
      PG(JLEV) = PG(JLEV) + (AMAT(JCH,JLEV)-AMAT(JCH,KDIM)) * (SUM(ZX*AMAT(JCH,:)) - 1.0)
   ENDDO
 ENDDO
 !PG = MATMUL(PG, zgx)

 alpha = float(nchan)*100.0
 PF = PF + 0.5*alpha*SUM(ZX**2, MASK=ZX<0.0)
 WHERE (ZX<0.0) PG = PG + alpha*ZX

!write(*,'(a,2f10.2,50f6.1)') 'ACD_PX',PF,sqrt(sum(pg**2)),sum(px(1:kdim-1))*100.,PX*100.
!write(*,'(a,2f10.5,f10.2,50f7.2)') '888888 ',PF,sqrt(sum(pg**2)),sum(zx(1:kdim-1))*100.,ZX*100.

end subroutine da_cloud_sim
