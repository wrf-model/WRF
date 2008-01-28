      SUBROUTINE W2GCONV(IGRD, JGRD, KGRD, JCAP, KMAX, INTVL , 
     C  DPHI, LMAX, KLMAX, MEND1, NEND1, JEND1, IMAXG, JMAXG, 
     C  IMAX, IOUT, JMAX, JOUT, IMX, JOUTHF, JMXGHF, KMX2, LMX2, MNWAV, 
     C          ht, psfc, t, u, v, q, A, B,
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde, 
     C          its , jts, kts, ite , jte, kte  )
      
         INTEGER ims , jms, kms, ime , jme, kme,  
     C           ids , jds, kds, ide , jde, kde, 
     C           its , jts, kts, ite , jte, kte  
      
      INTEGER  kst 
      DIMENSION A(kms:kme+1), B(kms:kme+1)
      DIMENSION ht(ims:ime,jms:jme), psfc(ims:ime,jms:jme)
      DIMENSION t(ims:ime,jms:jme,kms:kme),q(ims:ime,jms:jme,kms:kme)
      DIMENSION u(ims:ime,jms:jme,kms:kme),v(ims:ime,jms:jme,kms:kme)
C
      DIMENSION ID(5)
      DIMENSION LAG(MEND1,NEND1)
      DIMENSION IFAX(10),TRIGS(IMAX)
      DIMENSION PNM (MNWAV,JOUTHF),DPNM(MNWAV,JOUTHF),
     1          ESINCL(JOUT),ECOSCL(JOUT),GCOSCL(JMAXG)
C
      DIMENSION QDAT1(KMX2,MNWAV),QDAT2(KMX2,MNWAV),
     1          QPHIS(   2,MNWAV)
C
CrizviDIMENSION       PS   (IOUT*JOUT),PSX (IOUT*JOUT),PSY (IOUT*JOUT)
      DIMENSION IDA (IOUT*JOUT)
      DIMENSION EDAT1(IMX *JOUT*KLMAX),EDAT2(IMX *JOUT*KLMAX)
      DIMENSION TMP(IOUT*JOUT)
      DIMENSION TMP1(IOUT*JOUT*KLMAX),TMP2(IOUT*JOUT*KLMAX)
C
Crizvi      REAL*8 GAUL(JMAXG),GAUW(JMAXG)
      REAL*8 PPNM(MNWAV),HHNM(MNWAV)
C
C
      DIMENSION KTOUT(82)
C
      DATA IT,JT,BS,FM/4,4,0.0,1.0E10/
      DATA ITSAV,IPSL/1,1/
      DATA RHMIN,TDMAX/1.0E-6,31.0/
      DATA KTOUT/82*999/
      DATA IWVHST,ITPGFL
     1    /     1,     3/
	  DATA NGH,NWU,NWV,NTM,NTD,NRR,NRV,NPV
     1    /  1,  2,  3,  4,  5,  6,  7,  8/
      DATA KTPART /-1/
      DATA KTOHK,IHKFL/42,99/

      NAMELIST /NAMOUT/ KTPART,KTOUT,ITSAV,KTOHK,ID_PROC
	  NAMELIST /NAMFIL/ IWVHST,ITPGFL
      NAMELIST /NAMDBG/ NOMTN,IDEBUG,IT,JT,IPSL
      NAMELIST /NAMSLP/ MINKTI
C------------------------------------------------------------------
C Check parameters
C------------------------------------------------------------------

      IF( IOUT /= (ide-ids+1) .or. JOUT /= (jde-jds+1) .or.
     C   KLMAX /= (kte-kts+1) ) then
         print*,' In W2GCONV IOUT,JOUT, KLMAX  = ',
     C                     IOUT,JOUT, KLMAX  
         print*,' Dims mismatch '                  
        stop
        end if 

	  IF (IGRD.NE.IOUT.OR.JGRD.NE.JOUT) THEN
		WRITE(*,*)'OUT GRID DOES NOT MATCH'
	  	WRITE(*,*)'IOUT=',IOUT,'JOUT=',JOUT
	  	WRITE(*,*)'IGRD=',IGRD,'JGRD=',JGRD
		STOP 9998
	  END IF
C#######################################################################
C  +++ COMPUTE AND READ IN CONSTANTS +++
C#######################################################################
      READ (95,NAMOUT)
      READ (95,NAMFIL)
      READ (95,NAMDBG)
      READ (95,NAMSLP)
      WRITE(96,NAMOUT)
      WRITE(96,NAMFIL)
      WRITE(96,NAMDBG)
      WRITE(96,NAMSLP)
C------------------------------------------------------------------
C Init variables
C------------------------------------------------------------------
	  NNMAX=12/MINKTI+1
C------------------------------------------------------------------
C CHECK DATE    SURFACE DATA FILE
C------------------------------------------------------------------
      IO=IOUT
      JO=JOUT
      IJOUT=IOUT*JOUT
      IJMAX=IMAX*JOUT
      IJKOUT=IOUT*JOUT*KLMAX
      GINV  =1./gravity
C-----------------------------------------------------------------------
      CALL SETARY(LAG,MEND1,NEND1,JEND1)
C-----------------------------------------------------------------------
C  +++ SET UP FFT COEFFICIENTS +++
C-----------------------------------------------------------------------
      TRIGS(:) = 0.
      CALL RFFTIM(IMAX,TRIGS,IFAX)
C-----------------------------------------------------------------------
C  +++ SETUP GAUSSIAN LATITUDES +++
C-----------------------------------------------------------------------
CLSW  CALL GAUSS(GAUL,GAUW,JMAXG)
CLSW  COSCL(1:JMAXG)=GAUL(1:JMAXG)
C-----------------------------------------------------------------------
C  +++ SETUP ZNM ETC +++
C-----------------------------------------------------------------------
      CALL ZNME2PXX
     I(MEND1,MNWAV,JOUTHF,
     O PNM  ,DPNM ,
     W PPNM ,HHNM)
      PAI=4.0*ATAN(1.0)
      DPAI=PAI/FLOAT(JOUT-1)
      DO 130 J=1,JOUT
      	ESINCL(J)=SIN(DPAI*FLOAT(J-1))
      	ECOSCL(J)=COS(DPAI*FLOAT(J-1))
 130  CONTINUE
C-----------------------------------------------------------------------
C  +++ READ IN TOPOGRAPHY FILE +++
C-----------------------------------------------------------------------
        IF(ITPGFL.NE.0) THEN
          READ(ITPGFL) MDIM,DPHIX,IDIM,JDIM,JHMSPH
          DPHIX=DPHIX
          IDIM=IDIM
          JDIM=JDIM
          IF(MDIM.NE.MEND1.OR.JHMSPH.NE.0) THEN
            WRITE(96,901) MDIM,MEND1,JHMSPH
  901       FORMAT(1H ,'*** ERROR IN TOPOGRPHY FILE ***'/
     1                ,1X,'(MDIM,MEND1)=(',I3,',',I3,')'/
     2                ,1X,'JHMSPH      = ',I3/)
            STOP 3002
          END IF
          READ(ITPGFL) ((QDAT1(KKK,NNN),KKK=1,2),NNN=1,MNWAV)
          CLOSE (ITPGFL)
C
          CALL REOWV
     1     (QDAT1,QPHIS,MEND1,NEND1,JEND1,MNWAV,KMX2,2,0,0,2,LAG)
!shc-rizvi start
          print*,'1. Doing Wave to grid transform for Topo'
          CALL W2G
     I     (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,   1,
     I      IFAX ,TRIGS,PNM  ,QPHIS,
     O      EDAT1,
     W      EDAT2)
!shc-rizvi end
C
        ELSE
          READ(IWVHST,END=1033) ID,KT,ISTP,FSEC,NREC,
     1                 KM,(A(K),K=1,KM),(B(K),K=1,KM)
         if( IOUT /= (ide-ids+1) .or. JOUT /= (jde-jds+1) .or.
     C   KLMAX /= (kte-kts+1) ) then
         print*,' In W2GCONV IOUT,JOUT, KLMAX  = ',
     C                     IOUT,JOUT, KLMAX  
         print*,' Dims mismatch '                  
         stop
         endif
          IF(KT.EQ.-1.OR.KT.EQ.0) THEN
            READ(IWVHST) (QDAT2(KKK,1),KKK=1,2*MNWAV),
     1                  (QDAT1(KKK,1),KKK=1,2*MNWAV)
Crizvi            CALL REOWV
Crizvi     1      (QDAT1,QPHIS,MEND1,NEND1,JEND1,MNWAV,KMX2,2,0,0,2,LAG)
!shc-rizvi start
            print*,'2. Doing Wave to grid transform for Topo'
            CALL W2G
     I       (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,   1,
     I        IFAX ,TRIGS,PNM  ,QDAT1,
     O        EDAT1,
     W        EDAT2)
!shc-rizvi end
            READ(IWVHST)
            READ(IWVHST)
            READ(IWVHST)
            READ(IWVHST)
            GO TO 1033
          ELSE
            READ(IWVHST)
            READ(IWVHST)
            READ(IWVHST)
            READ(IWVHST)
            READ(IWVHST)
          END IF
        END IF
1033      CONTINUE

        REWIND(IWVHST)
C
        CALL CUT(TMP, IOUT,JOUT,   1,EDAT1,IMAX,INTVL)
        DO 160 IJ=1,IJOUT
        TMP(IJ) =GINV*TMP(IJ) 
  160   CONTINUE
        IF(IDEBUG.GE.1) THEN
          CALL GOUT(TMP,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'PHIS',1)
        END IF
C        write(989,'(10f10.3)') (ETPG(IJ),IJ=1,IJOUT)
       call reorder_for_wrf(TMP,ht,IOUT,JOUT, 1,
     C          ims , jms, 1, ime , jme, 1, 
     C          ids , jds, 1, ide , jde, 1,
     C          its , jts, 1, ite , jte, 1)
C
C#######################################################################
C  +++ READ IN HISTORY FILE +++
C  +++ WAVE TO GRID CONVERSION START +++
C#######################################################################
C...IWVHST; WAVE  HISTORY      FILE
C...ISFCFL; GAUSS GRID SURFACE FILE
C...IGHIST; EQUAL LAT-LON GRID P-HISTORY FILE
C           IF IGHIST=0, NO FILE OUTPUT
C...IPGUES; EQUAL LAT-LON GRID   P-GUESS FILE
C           IF IPGUES=0, NO FILE OUTPUT
C...IXXXFL; GRID ETA-TEMPORARY FILE FOR XXX PASSED TO ETA-P CONVERSION
C           IF IXXXFL=0, NO XXX TEMPORARY FILE OUTPUT
C           XXX IS TMP,SPH,U,V,DIV,OMG
C
      ICOUNT=1
C
 2000 CONTINUE
C
      IF(KTOUT(ICOUNT).EQ.999.AND.KTOUT(1).NE.-999) GOTO 1001
      READ(IWVHST) ID,KT,ISTP,FSEC,NREC,                       
     1                      KM,(A(K),K=1,KM),(B(K),K=1,KM)              
      WRITE(96,666) ID,KT                                              
 666  FORMAT(1H ,'HISTORY FILE READ IN AT',6I5)                         
C     IF(KT.LE.0) THEN                                                  
C          WRITE(96,*)'KT=',KT
C     END IF                                                            
C                                                                       
      IF(KM.NE.KMAX) THEN                                               
          WRITE(96,902)                                                 
  902     FORMAT(1X ,'*** ERROR IN NO. OF ETALEV ***')                  
          STOP 3001                                                     
      END IF                                                           
!
      IF(KT.EQ.KTOUT(ICOUNT).OR.KTOUT(1).EQ.-999) THEN                  
        ICOUNT=ICOUNT+1                                                 
        WRITE(96,905) ID,KT                                             
  905   FORMAT(1H ,2X,'YEAR=',I4,4X,'MONTH=',I2,4X,'DAY=',I2,4X,'HOUR=',
     *          I2,4X,'WEEK=',I1,4X,'FCST=',I3)                         
C---------------------------------------------------------------------
C...SURFACE PRESSURE AND ITS GRADIENTS
C---------------------------------------------------------------------
        READ(IWVHST) (QDAT1(KMN,1),KMN=1,2*MNWAV)
      	WRITE(96,*) ID,KT,'qdat1'
C...IPXY=1;OUTPUT IS PS,D(PS)/DX,-D(PS)/DY
      	IPXY=1
       print*,' Doing Wave to grid transform for Psfc'
        CALL W2GPXY
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,
     I  IFAX ,TRIGS,PNM  ,DPNM ,IPXY,IPSL,QDAT1,
     O  EDAT1,
     W  EDAT2)
        CALL CUT(TMP ,IOUT,JOUT,1,EDAT1(        1),IMAX,INTVL)
Crizvi        CALL CUT(PSX ,IOUT,JOUT,1,EDAT1(  IJMAX+1),IMAX,INTVL)
Crizvi        CALL CUT(PSY ,IOUT,JOUT,1,EDAT1(2*IJMAX+1),IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
          CALL GOUT(TMP,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'PSFC',1)
!         CALL GOUT(PSX,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'DPSX',1)
!         CALL GOUT(PSY,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'DPSY',1)
        END IF
       call reorder_for_wrf(TMP, psfc,IOUT,JOUT, 1,
     C          ims , jms, 1, ime , jme, 1, 
     C          ids , jds, 1, ide , jde, 1,
     C          its , jts, 1, ite , jte, 1)
C---------------------------------------------------------------------
C...TEMPERATURE
C---------------------------------------------------------------------
        READ (IWVHST) QDAT1
       print*,' Doing Wave to grid transform for Temperature'
        CALL W2G
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,KMAX,
     I  IFAX ,TRIGS,PNM  ,QDAT1,
     O  EDAT1,
     W  EDAT2)
        CALL CUT(TMP1,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
           K=KMAX
          CALL GOUT(TMP1,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'ETMP',K)
        END IF
       call reorder_for_wrf(TMP1, t, IOUT,JOUT, KLMAX,
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde,
     C          its , jts, kts, ite , jte, kte )
C----------------------------------------------------------------------
C... VORTICITY & DIVERGENCE
C----------------------------------------------------------------------
C...VORTICITY
        READ(IWVHST)  QDAT1
C...DIVERGENCE
        READ(IWVHST)  QDAT2
       print*,' Doing Wave to grid transform for Wind'
        CALL W2GUV
     I (MEND1,NEND1,JEND1 ,MNWAV,IMAX,JOUT,IMX  ,JOUTHF,KMAX,
     I  IFAX ,TRIGS,ESINCL,ER   ,PNM ,DPNM,QDAT1,QDAT2 ,
     O  EDAT1,EDAT2,
     W  TMP2)
        CALL CUT(TMP1,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
        CALL CUT(TMP2,IOUT,JOUT,KMAX,EDAT2,IMAX,INTVL)
        IF(IDEBUG.GE.1) THEN
          K=KMAX
          CALL GOUT(TMP1,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'EU  ',K)
          CALL GOUT(TMP2,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'EV  ',K)
        END IF
C
       call reorder_for_wrf(TMP1, U ,IOUT,JOUT, KLMAX,
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde,
     C          its , jts, kts, ite , jte, kte )
       call reorder_for_wrf(TMP2, V ,IOUT,JOUT, KLMAX,
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde,
     C          its , jts, kts, ite , jte, kte )
C----------------------------------------------------------------------
C...SPECIFIC HUMIDITY
C----------------------------------------------------------------------
        READ(IWVHST)  QDAT1
       print*,' Doing Wave to grid transform for Moisture'
        CALL W2G
     I (MEND1,NEND1,JEND1,MNWAV,IMAX,JOUT,IMX,JOUTHF,KMAX,
     I  IFAX ,TRIGS,PNM  ,QDAT1,
     O  EDAT1,
     W  EDAT2)
       print*,' Done  Wave to grid transform for Moisture calling cut'
        CALL CUT(TMP1,IOUT,JOUT,KMAX,EDAT1,IMAX,INTVL)
C rizvi fix negative humidity to 0.000001
        CALL FIX_NEG_MOIST(TMP1,IOUT,JOUT,KMAX)
       print*,' Done  cut calling reorder_for_wrf'
c---------------------------------------------------------------
       call reorder_for_wrf(TMP1,  Q,IOUT,JOUT, KLMAX,
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde,
     C          its , jts, kts, ite , jte, kte )
       print*,' Done  calling reorder_for_wrf'
c---------------------------------------------------------------
        IF(IDEBUG.GE.1) THEN
          K=KMAX
          CALL GOUT(TMP1,IDA,IO,JO,1,1,IO,JO,IT,JT,BS,FM,'ESPH',K)
        END IF
      ELSE
        READ(IWVHST)                                                    
        READ(IWVHST)                                                   
        READ(IWVHST)                                                    
        READ(IWVHST)                                                    
        READ(IWVHST)                                                    
        GO TO 2000                                                      
      END IF
C
C
      GO TO 2000
C
 1001 CONTINUE
      RETURN
      END SUBROUTINE W2GCONV

      SUBROUTINE FIX_NEG_MOIST(X,N1,N2,N3)      
C---------------------------------------------------------
C This fixes less than 0.00000 input array to 0.000001            
C              Syed RH Rizvi    08/24/2004
C---------------------------------------------------------
      DIMENSION X(N1,N2,N3) 
      DO K = 1,N3
        DO J= 1,N2
         DO I = 1, N1
C          IF(X(I,J,K) < 0) X(I,J,K) = 0.
          IF(X(I,J,K) < 0.000001) X(I,J,K) = 0.000001
         END DO
        END DO
      END DO
      RETURN
      END SUBROUTINE FIX_NEG_MOIST     

      SUBROUTINE reorder_for_wrf_2d(kma, X, n1,n2 ,
     C          ims , jms,  ime , jme,  
     C          ids , jds,  ide , jde,  
     C          its , jts,  ite , jte   )

      IMPLICIT NONE
      INTEGER,intent(in) ::
     C           ims , jms,  ime , jme,   
     C           ids , jds,  ide , jde,  
     C           its , jts,  ite , jte  
      integer, intent(in)        :: n1,n2                    
      real, intent(in)           :: kma(n1*n2)
      real, intent(  out)        :: X(ims:ime,jms:jme)
      real, dimension(1:n1,1:n2) :: wrf
      real                            :: tmp(n1,n2)
      integer                         :: i,j,ist

C
        ist = 1
        do j=1,n2
        do i=1,n1
        tmp(i,j) = kma(ist)
        ist = ist + 1
        end do
        end do
C        
         do j= 1,n2
          wrf(1:n1,j) = tmp(1:n1,n2-j+1)
         end do 
C
         do j =  jts, jte
         X(its:ite,j) = wrf(its:ite,j)
         end do
       
      RETURN
      END SUBROUTINE reorder_for_wrf_2d
      SUBROUTINE reorder_for_wrf_3d(kma, X, n1,n2,n3, 
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde, 
     C          its , jts, kts, ite , jte, kte  )

      IMPLICIT NONE
      INTEGER,intent(in) ::
     C           ims , jms, kms, ime , jme, kme,  
     C           ids , jds, kds, ide , jde, kde, 
     C           its , jts, kts, ite , jte, kte  
      integer, intent(in)        :: n1,n2,n3                    
      real, intent(in)           :: kma(n1*n2*n3)
      real, intent(OUT)          :: X(ims:ime,jms:jme,kms:kme)
      real, dimension(1:n1,1:n2,1:n3) :: wrf
      real                            :: tmp(n1,n2,n3)
      integer                         :: i,j,k,ist

C
        ist = 1
        do k=1,n3
        do j=1,n2
        do i=1,n1
        tmp(i,j,k) = kma(ist)
        ist = ist + 1
        end do
        end do
        end do
C        
        do k =1,n3
         do j= 1,n2
          wrf(1:n1,j,k) = tmp(1:n1,n2-j+1,k)
         end do 
        end do 
C
        do k=kts,kte
         do j =  jts, jte
         X(its:ite,j,k) = wrf(its:ite,j,k)
         end do
        end do
C
      RETURN
      END SUBROUTINE reorder_for_wrf_3d

      SUBROUTINE reorder_for_wrf(kma, wrf, n1,n2,n3, 
     C          ims , jms, kms, ime , jme, kme, 
     C          ids , jds, kds, ide , jde, kde, 
     C          its , jts, kts, ite , jte, kte  )
      IMPLICIT NONE
      INTEGER,intent(in) ::
     C           ims , jms, kms, ime , jme, kme,  
     C           ids , jds, kds, ide , jde, kde, 
     C           its , jts, kts, ite , jte, kte  
      integer, intent(in)        :: n1,n2,n3                    
      real, intent(in)           :: kma(n1*n2*n3)
      real, intent(OUT)          :: wrf(ims:ime,jms:jme,kms:kme)
      real                       :: XWRF(ims:ime,jms:jme,kms:kme)
      real                       :: tmp(n1,n2,n3), xtmp(n1,n2,n3)
      integer                    :: i,j,k, half
C
       tmp(1:n1,1:n2,1:n3) = 
     & RESHAPE( kma(1:n1*n2*n3),(/n1,n2,n3/) )
C        
        do k =1,n3
         do j= 1,n2
          xtmp(1:n1,j,k) = tmp(1:n1,n2-j+1,k)
         end do 
        end do 
C
        do k=kts,kte
         do j =  jts, jte
         XWRF(its:ite,j,k) = xtmp(its:ite,j,k)
         end do
        end do
C
        half = (ite-its+1)/2
        do k=kts,kte
          do j= jts,jte
            do i=its,ite
             if( i <= half)then
              WRF(half+i,j,k) = XWRF(i,j,k)
             else
              WRF(i-half,j,k) = XWRF(i,j,k)
             end if
            end do
          end do
        end do
C
       RETURN
      END SUBROUTINE reorder_for_wrf
