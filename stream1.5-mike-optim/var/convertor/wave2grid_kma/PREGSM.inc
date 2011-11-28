      SUBROUTINE PREGSM(PSE,GTE,GUE,GVE,GQE,PSB,GTB,GUB,GVB,GQB,
     &          IMAXE,JMAXE,ISST,JSST,MAXJZ,IVAR, 
     &          IMAX,JMAX,KMAX,IDIM,JDIM,MEND1,NEMD1,JEND1,
     &          ISNW,JSNW,JMAXHF,MNWAV,IMX)
      INTEGER IDATE(5), IDGES(5), IDSST(5)
      CHARACTER*8 FILE, MODEL0, RESL0
      CHARACTER*80 CINF0(10)
      CHARACTER*4 TYPE, EXPR0, KTUNIT, NPROD, NPROM, VCODD, VCODM
      CHARACTER*4 LEVEL, ELEM
      CHARACTER*32 TITLE
      CHARACTER*16 UNIT
      CHARACTER*8 MDLINF(4)
! modified by shc p1 start
      CHARACTER*80 CINF0_X(10),CINF_temp
      CHARACTER*4 VCODD_X, VCODM_X
! modified by shc p1 end
      REAL        DTHPRO(7)
      INTEGER ITYP(2)
      CHARACTER*48 LABEL
      INTEGER JTINF(2)
	  CHARACTER*10	FROMUNPACK
	  INTEGER		IUNPACK
C
      DIMENSION A(KMAX+1), B(KMAX+1), AAM(KMAX+1), BBM(KMAX+1)
      DIMENSION AGD(KMAX+1), BGD(KMAX+1), AGM(KMAX+1), BGM(KMAX+1)
      DIMENSION GPHIS(IMAX*JMAX)
      REAL, DIMENSION(IMAX,JMAX)    :: GAU

      DIMENSION PSE (IMAXE,JMAXE),
     1       GZE (IMAXE,JMAXE,KMAX), GTE  (IMAXE,JMAXE,KMAX),
     2       GUE (IMAXE,JMAXE,KMAX), GVE  (IMAXE,JMAXE,KMAX),
     3       GQE (IMAXE,JMAXE,KMAX)
      DIMENSION  PS  (IMAX,JMAX),  GRH (IMAX,JMAX,KMAX),
     1       GZ  (IMAX,JMAX,KMAX), GT  (IMAX,JMAX,KMAX),
     2       GU  (IMAX,JMAX,KMAX), GV  (IMAX,JMAX,KMAX),
     3       GQ  (IMAX,JMAX,KMAX), AGT (IMAX,JMAX,KMAX),
     4       GCWC(IMAX,JMAX,KMAX), GCVR(IMAX,JMAX,KMAX),
     5       GUMB(IMAX,JMAX,KMAX),
     6       GSST(IMAX,JMAX)     , GSNW(IMAX,JMAX)
      DIMENSION  PSB (IMAX,JMAX),  GRHB(IMAX,JMAX,KMAX),
     1       GQB (IMAX,JMAX,KMAX), GTB (IMAX,JMAX,KMAX),
     2       GUB (IMAX,JMAX,KMAX), GVB (IMAX,JMAX,KMAX)
      real   rdum (imax,jmax,kmax)
!modified by shc AB start
      INTEGER MMM0(5)
!modified by shc AB end

      DIMENSION VLG(IMAX,JMAX,KMAX)                                             
C     DIMENSION WRK1(IMAX,JMAX,KMAX), WRK2(IMAX,JMAX,KMAX),                     
      REAL * 8  WRK1(IMAX,JMAX,KMAX), WRK2(IMAX,JMAX,KMAX)                      
      DIMENSION WRK3(IMAX,JMAX,KMAX), WRK4(IMAX,JMAX,KMAX),                     
     2          WRK5(IMAX,JMAX,KMAX), WRK6(IMAX,JMAX,KMAX)                      
      CHARACTER*4 ALVL                                                          
!modified by shc I2 start
c     INTEGER*2 I2(IDIM*JDIM)     !shc-rizvi
      INTEGER   I2(IDIM*JDIM/2)   !shc-rizvi
!modified by shc I2 end
      REAL*8    WRK(IDIM,JDIM)                                                  
      DIMENSION SSTA(ISST*JSST), SEWA(ISNW,JSNW)                                
      DIMENSION COLRAD(JMAX), DY(JMAX), LY(JMAX)                                
      DIMENSION WORK(362,182),DP(4,IMAX,JMAX)                                   
      INTEGER*2 IP(2,IMAX,JMAX)                                                 
      REAL*8    GAUL(JMAX),GAUW(JMAX),COCOT(JMAX)                               
      COMMON/CTETEN/TABLE(25000)                                                
      COMMON/DTETEN/DTABLE(25000)                                               
      REAL*8 TABLE,DTABLE,RGSA                                                       
      DIMENSION RLAT(MAXJZ), ZDAT(MAXJZ,KMAX)                                   
C                                                                               
      NAMELIST /NAMFIL0/NALFL0,NVPFL0,NGSFL0,NSSTFL0,NSNWFL0,NINFL0,
     1		    KTLAG0,IDCHCK0,NDIGFL0,NTPFL0,NALOT0,NRSFL0
C     NAMELIST /HEADIN/ TYPE,FILE,KTUNIT,IDTYPE,
C    1                  IBACK,NNSP
C------------------------------------------------------------------------
C  NALFL : 3DOI INPUT FILE
C  NVPFL : VERTIAL LEVEL DEF. FILE
C  NTPFL : TOPO FILE
C  NALOT : 3DOI INPUT SAVE FILE
C  NRSFL : UNPACK INPUT FILE
C------------------------------------------------------------------------
      NAMELIST /NAMVER0/ MODEL0, RESL0, EXPR0, CINF0                                 
                                                                                
      DATA RHMIN/1.0E-3/                             
      DATA TLAPS,QCONS,QMIN,KST,ITERMX/2.0E-3,2.5E-6,1.0E-10,10,3/              
C                                                                               
      DATA NALFL0,NGSFL0,NSSTFL0,NSNWFL0,NINFL0,NVPFL0,NALOT0,NRSFL0
     1    /     1,     2,    -1,    -1,    11,    21,   12,   -1/                           
      DATA KTLAG0 / 6/                                                           
      DATA IDCHCK0/ 1/                                                           
!modified by shc AB start
c  The definition A and B by 'DATA' was removed
!modified by shc AB end
!modified by shc AB start
      READ(115) MMM0,MMM1,MMM2,FFF1,MMM3,MMM4,
     1                      (A(K),K=1,MMM4),(B(K),K=1,MMM4)
      A(KMAX+1)=0.0; B(KMAX+1)=0.0
!modified by shc AB end
C                                                                               
C   =================================================================           
C   >>>   READ ANAL TIME                                          <<<         
C   =================================================================           
          READ(94,'(I4,3I2)') (IDATE(I),I=1,4)
          IDATE(5)=0
C   =================================================================           
C   >>>   NAMELIST (NAMFIL)                                       <<<           
C   =================================================================           
      READ(96,NAMFIL0)                                                            
c     READ(96,HEADIN)                                                            
      WRITE(6,NAMFIL0)                                                           
c     WRITE(6,HEADIN)                                                           
!modified by shc p1 start
      LARHM=20
!modified by shc p1 end
C   =================================================================
C   >>>   Select Input Source                                     <<<
C   =================================================================
!shc-wei start
c     CALL GETENV('FROMUNPACK',FROMUNPACK)
c     IF (LEN_TRIM(FROMUNPACK).EQ.0) THEN
c       IUNPACK=0
c     ELSE
c       READ(FROMUNPACK,'(I1)') IUNPACK
c     END IF
c     WRITE(6,*)'IUNPACK=',IUNPACK
!shc-wei end
C   =================================================================           
C   >>>   GENERATE GAUSSIAN LATITUDES                             <<<           
C   =================================================================           
      CALL GAUSS(GAUL,GAUW,JMAX)                                                
      DO 800 J=1,JMAX                                                           
      COLRAD(J)=ACOS(GAUL(J))                                                   
  800 CONTINUE                                                                  
      CALL ZMNLAT( RLAT, MAXJZ, COLRAD, JMAX )                                  
C                                                                               
C   =================================================================           
C   >>>   TETEN                                                   <<<           
C   =================================================================           
      ICE = 1                                                                   
      CALL TETEN(ICE)                                                           
C   =================================================================           
C   >>>  READ TOPO FILE
C   =================================================================           
          NTPFL0 = 3
c         go to 33333     !shc For T63 only
 	  IF (NTPFL0.GT.0) THEN
 	  	READ(NTPFL0)NWV,DUM,IGRD,JGRD
 	  	IF ((IGRD.NE.IMAX).OR.(JGRD.NE.JMAX)) THEN
 			WRITE(*,*)' TOPO DIM DOES NOT MATCH'
 			WRITE(*,*)'IMAX=',IMAX,' IGRD=',IGRD
 			WRITE(*,*)'JMAX=',JMAX,' JGRD=',JGRD
                        STOP 9988
 	  	END IF
 	  	READ(NTPFL0)
 	  	READ(NTPFL0)
 	  	READ(NTPFL0)GPHIS
 	  	WRITE(*,*)'GRID DISTANCE=',DUM
 	  END IF
C---------------------------------------------------------------------
C +++ CONVERT LAT/LON to GAUSS
C---------------------------------------------------------------------
33333        continue
c 	READ(NTPFL0,'(10f10.3)')GPHIS    !shc For T63 only
        CALL LT2GAU (PSE,IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,PS,DY,LY)
       DO K = 1, KMAX
        CALL LT2GAU (GTE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,GT(:,:,K),DY,LY)
        CALL LT2GAU (GUE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,GU(:,:,K),DY,LY)
        CALL LT2GAU (GVE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,GV(:,:,K),DY,LY)
        CALL LT2GAU (GQE(:,:,K),IMAXE,JMAXE,IMAX,JMAX,
     1                  COLRAD,GQ(:,:,K),DY,LY)
       ENDDO
9001   format(10e15.7)     
       rdum = 0.0       
       write(902,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
       write(902,9001) ((PS(i,j),i=1,imax),j=1,jmax)
       write(902,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
       do k=1,kmax
       write(902,9001) ((GT(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(902,9001) ((GU(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(902,9001) ((GV(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(902,9001) ((GQ(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(902,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(902,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)   
       enddo                                              
C                                                
C   =================================================================           
C   >>>   PS, TEMP, Q -> RH
C   =================================================================           
      CALL RELHUM                                 
     I  (GT ,GQ ,PS ,IMAX,JMAX,KMAX,A,B, 
     O   GRH)                                    
      CALL RELHUM                                 
     I  (GTB ,GQB ,PSB ,IMAX,JMAX,KMAX,A,B, 
     O   GRHB)                                   
      PIHF = pi*0.5                              
      DO 3739 K = 1,KMAX
      DO 3738 J = 1,JMAX
      DO 3737 I = 1,IMAX
        AANAL = GRH(I,J,K)-GRHB(I,J,K)                 
        AGES  = GRHB(I,J,K)                            
        IF    ( AGES.LE.0.0 .AND. AANAL.LE.0.0 ) THEN
           AANAL = 1.0E-6  
        ELSEIF( AGES.GE.1.0 .AND. AANAL.GE.0.0 ) THEN
           AANAL = 1.0    
        ELSE
          IF( AANAL.GT.0.0 ) THEN
            RES = 1.0-AGES  
          ELSE                                         
            RES =    -AGES                             
          ENDIF
          IF( ABS(AANAL).LE.ABS(RES*0.5) ) THEN 
            AANAL = AGES+AANAL   
          ELSE                                   
            AA = RES/PIHF*0.5                          
            XN = AANAL-RES*0.5                         
            AANAL        = AGES + 0.5*RES + AA*ATAN(XN/AA)
          ENDIF
        ENDIF
        AANAL        = MAX( AANAL, 1.0E-6 ) 
        AANAL        = MIN( AANAL, 1.0E0  )
        GRH(I,J,K) = AANAL
 3737 CONTINUE
 3738 CONTINUE
 3739 CONTINUE                                         

C   =================================================================           
C   >>>   PS, TEMP, Q -> Z
C   =================================================================           
      CALL GPLHGT
     I  (PS,GT,GQ,GPHIS,IMAX,JMAX,KMAX,gas_constant,gravity,A,B,
     I      1,JMAX,
     O   GZ)

CLSW   do k=1,22,3
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GZ(I,J,K),I=1,IMAX)
CLSW    enddo
CLSW   enddo
C  ==================================================================
C  >>> SAVE INPUT DATA
C  ==================================================================
        IF (NALOT0.GT.0) THEN
            WRITE(NALOT0)PS
            WRITE(NALOT0)GZ
            WRITE(NALOT0)GU
            WRITE(NALOT0)GV
            WRITE(NALOT0)GQ
            WRITE(NALOT0)GT
        END IF
Crizvi      ELSE          ! START WITH UNPACK FILE
Crizvi        LARHM=20
Crizvi        READ(NRSFL0)IDATE
Crizvi        READ(NRSFL0)PS
Crizvi        READ(NRSFL0)GZ
Crizvi        READ(NRSFL0)GU
Crizvi        READ(NRSFL0)GV
Crizvi        READ(NRSFL0)GQ
Crizvi        READ(NRSFL0)AGT
Crizvi
Crizvi      END IF ! READ ANAL FINISH
       print*,' Gaussian lats data size ',IMAX, JMAX, KMAX
        write(661,'(10f10.3)')PS
        write(661,'(10f10.3)')GU
        write(661,'(10f10.3)')GV
        write(661,'(10f10.3)')GT
        write(661,'(10f10.3)')GQ

CLSW      write(99,*) ' Gauss GT'
CLSW   do k=1,2
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GT(I,J,k),I=1,IMAX)
CLSW    enddo
CLSW   enddo
C---------------------------------------------------------------------
C      DO J = 1, JMAX
C        write(99,FMT='(10F12.5,1x)') (GAU(I,J),I=1,IMAX)
C      ENDDO

1000  CONTINUE

C   =================================================================           
C   >>>   NAMELIST (NAMVER)                                       <<<           
C   =================================================================           
      CINF0(1)=' ';CINF0(2)=' ';CINF0(3)=' ';CINF0(4)=' ';CINF0(5)=' '               
      CINF0(6)=' ';CINF0(7)=' ';CINF0(8)=' ';CINF0(9)=' ';CINF0(10)=' '              
      READ(96,NAMVER0)                                                            
      WRITE(6,NAMVER0)                                                           
C   =================================================================
      IF(NGSFL0.GE.0) THEN
      CALL REDGES
     I(NGSFL0 ,IMAX  ,JMAX  ,KMAX  ,KTLAG0 ,IDATE ,IDCHCK0,
     O IDGES ,AGD   ,BGD   ,AGM   ,BGM   ,GCWC  ,GCVR  ,GUMB  ,
     W I2    ,IDSST )
      ENDIF
!modified by shc ZT start
      goto 7700
!modified by shc ZT end
C   =================================================================           
C   >>>   Z -> TV                                                 <<<           
C   =================================================================           
C     CALL CTIME( 4, 'ZE2TVE              ' )
C   >>> GT IS TV (OUTPUT)
      IF (NTPFL0.LT.0) THEN
        CALL GH2TV(GZ, GT, PS, GPHIS, A, B,
     1          IMAX  , JMAX  , KMAX  ,WRK1  , WRK2  , WRK3  , WRK4)
      ELSE
        CALL ZE2TVE( GZ    , GT    , PS    , A     , B     ,
     I             IMAX  , JMAX  , KMAX  ,
     W             VLG   , WRK1  , WRK2  , WRK3  , WRK4  , WRK5  ,
     W             WRK6  )
C
CLSW      write(99,*) ' Z->TV'
CLSW   do k=1,2
CLSW    do j=1,jmax
CLSW      write(99,FMT='(10F12.5,1x)') (GT(I,J,K),I=1,IMAX)
CLSW    enddo
CLSW   enddo
      END IF
CLSW  CALL ZMNT( ZDAT, MAXJZ, KMAX, GT   , IMAX, JMAX )
CLSW  CALL OUTZ( ZDAT, MAXJZ, KMAX, 'TV  ',
CLSW 1            'TV                             ', 'K               ',
CLSW 2             0, RLAT, 'KMAX' )

C   =================================================================           
C   >>>   RH, TV -> Q, T                                          <<<           
C   =================================================================           
      IDX=1
      LARHM=20                 
      DO K=1,LARHM-1
      DO I=1,IMAX
      DO J=1,JMAX
       GQ(I,J,K)=GRH(I,J,K) 
      ENDDO
      ENDDO
      ENDDO                    
      CALL CRH2SHA
     I(IMAX*JMAX, KMAX, PS, A, B, gravity,gas_constant,
     I TLAPS,QCONS,QMIN,KST,ITERMX,
     I IDX, LARHM,
     O GQ, GT)
C          write(99,*) ' after  RH, TV -> Q, T'
C       do k=1,2
C        do j=1,jmax
C          write(99,FMT='(10F12.5,1x)') (GT(I,J,K),I=1,IMAX)
C        enddo
C       enddo
!modified by shc ZT start
7700   continue
!modified by shc ZT end

!modified by shc q0 start
      DO I=1,IMAX
      DO J=1,JMAX
      DO K=1,KMAX
        IF (GQ(I,J,K).LT.0.00) GQ(I,J,K)=1.E-06
      ENDDO
      ENDDO
      ENDDO
      print *, 'shcimsi q0=',1.E-06
!modified by shc q0 end

!modified by shc p1 start
      NANFL=151
      KT=0
      DO i=1,80
        CINF_temp(i:i)=' '
      ENDDO
      DO j=1,10
        CINF0_X(j)=CINF_temp
      ENDDO
      VCODD_X='    '
      VCODM_X='    '
      CALL WRTHED
     I(NANFL ,
     I 'GVS1',IDATE ,'ANALETA ','GANL9603','T213L30L',
     I 'R03 ','HOUR',1     ,0     ,0     ,
     I IMAX  ,JMAX  ,'GAUS', 0.0 , 0.0,
     I 0.0   ,0.0, 0.0   ,0.0   ,
     I VCODD_X,KMAX  ,A     ,B     ,
     I IMAX  ,JMAX  ,'GAUS', 0.0 , 0.0,
     I 0.0   ,0.0, 0.0   ,0.0   ,
     I VCODM_X,KMAX  ,A     ,B     ,
     I CINF0_X )

      GPHIS(:)=GPHIS(:)/G
      CALL MOVERD(GPHIS, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'TOPO',
     2 'GEOPOTENTIAL HEIGHT             ', 'M               ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      CALL MOVERD(PS, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'P   ',
     2 'SURFACE PRESSURE                ', 'HPA             ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      CALL MOVERD(GU(1,1,1), WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'U   ',
     2 'SURFACE U                       ', 'M/S             ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      CALL MOVERD(GV(1,1,1), WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'V   ',
     2 'SURFACE V                       ', 'M/S             ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      CALL MOVERD(GT(1,1,1), WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'T   ',
     2 'SURFACE T                       ', 'K               ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      CALL MOVERD(GRH(1,1,1), WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NANFL , IDATE , KT    , 'SURF', 'RH  ',
     2 'SURFACE RELATIVE HUMIDITY       ', '0-1             ',
     3 0     , 0     ,WRK  , IMAX  , JMAX  , I2    )

      DO K=1,KMAX
        CALL MOVERD(GU(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'U   ',
     2   'U                               ', 'M/S             ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO

      DO K=1,KMAX
        CALL MOVERD(GV(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'V   ',
     2   'V                               ', 'M/S             ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO

      DO K=1,KMAX
        CALL MOVERD(GZ(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'Z   ',
     2   'GEOPOTENTIAL HEIGHT             ', 'M               ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO

      DO K=1,KMAX
        CALL MOVERD(GT(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'T   ',
     2   'TEMPERATURE                     ', 'K               ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO

      DO K=1, LARHM-1
        CALL MOVERD(GRH(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'RH  ',
     2   'RELATIVE HUMIDITY               ', '0-1             ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO

      DO K=LARHM,KMAX
        CALL MOVERD(GQ(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NANFL , IDATE , KT    ,  ALVL, 'Q   ',
     2   'SPECIFIC HUMIDITY               ', 'KG/KG           ',
     3   0     , 0     ,WRK  , IMAX  , JMAX  , I2    )
      ENDDO
      WRITE(NANFL) IDATE,KT,0,'    ','    '
!modified by shc p1 end

       rdum = 0.0       
       write(903,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
       write(903,9001) ((PS(i,j),i=1,imax),j=1,jmax)
       write(903,9001) ((rdum(i,j,1),i=1,imax),j=1,jmax)
       do k=1,kmax
       write(903,9001) ((GT(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(903,9001) ((GU(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(903,9001) ((GV(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(903,9001) ((GQ(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(903,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)
       enddo
       do k=1,kmax
       write(903,9001) ((rdum(i,j,k),i=1,imax),j=1,jmax)   
       enddo                                            
C   *****************************************************************           
C   >>>   OUTPUT INITIAL VALUE                                    <<<           
C   *****************************************************************           
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL WRTHED                                                               
     I(NINFL0 ,                                                                  
     I 'GVS1',IDATE ,'INITETA ',MODEL0, RESL0,                                    
     I EXPR0 ,'HOUR',1     ,0     ,0     ,                                      
     I IMAX  ,JMAX  ,'GAUS',360.0/IMAX, REAL(JMAX),                             
     I 1.0   ,(JMAX+1)/2.0, 0.0   ,0.0   ,                                      
     I 'ETA ',KMAX  ,A     ,B     ,                                             
     I IMAX  ,JMAX  ,'GAUS',360.0/IMAX, REAL(JMAX),                             
     I 1.0   ,(JMAX+1)/2.0, 0.0   ,0.0   ,                                      
     I 'ETA ',KMAX  ,A     ,B     ,                                             
     I CINF0 )                                                                  
C                                                                               
C   =================================================================           
C   >>>   PS                                                      <<<           
C   =================================================================           
      CALL MOVERD(PS, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL0 , IDATE , -1    , 'SURF', 'P   ',
     2 'P                               ', 'HPA             ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
C                                                                               
C   =================================================================           
C   >>>   U, V                                                    <<<           
C   =================================================================           
        DO 9030 K=1,KMAX
        CALL MOVERD(GU(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'U   ',
     2   'U                               ', 'M/S             ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9030   CONTINUE
        DO 9040 K=1,KMAX
        CALL MOVERD(GV(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'V   ',
     2   'V                               ', 'M/S             ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9040   CONTINUE
C   =================================================================           
C   >>>   T, Q                                                    <<<           
C   =================================================================           
        DO 9010 K=1,KMAX
        CALL MOVERD(GT(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'T   ',
     2   'T                               ', 'K               ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9010   CONTINUE
        DO 9020 K=1,KMAX
        CALL MOVERD(GQ(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'Q   ',
     2   'Q                               ', 'KG/KG           ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9020   CONTINUE
C   =================================================================           
C   >>>  SAVE INPUT FIELD FOR DIAG.
C   =================================================================           
      IF (NDIGFL0.GT.0) THEN
        WRITE(NDIGFL0)GT
        WRITE(NDIGFL0)GQ
      END IF
C                                                                               
C   =================================================================           
C   >>>   CWC, CVR                                                <<<           
C   =================================================================           
      IF(NGSFL0.GT.0) THEN
        DO 9050 K=1,KMAX
        CALL MOVERD(GCWC(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'CWC ',
     2   'CLOUD WATER CONTENT             ', 'KG/KG           ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9050   CONTINUE
        DO 9060 K=1,KMAX
        CALL MOVERD(GCVR(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'CVR ',
     2   'CLOUD COVER                     ', '-               ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9060   CONTINUE
C                                                                               
C   =================================================================           
C   >>>   UMB                                                     <<<           
C   =================================================================           
        DO 9070 K=1,KMAX
        CALL MOVERD(GUMB(1,1,K), WRK, IMAX*JMAX)
        WRITE(ALVL(1:4), '(I4)') K
        CALL WRTDAT
     1  (NINFL0 , IDATE , -1    , ALVL  , 'UMB ',
     2   'UPWARD MASS FLUX AT CLOUD BASE  ', 'KG/S/M**2       ',
     3   0     , 0     , WRK   , IMAX  , JMAX  , I2    )
 9070   CONTINUE
      END IF   !NGSFL>0
C                                                                               
C   *****************************************************************           
C   >>>   SST ANOMALY                                             <<<           
C   *****************************************************************           
      IF( NSSTFL0.NE.-1 ) THEN
      CALL GETTYP(NSSTFL0,IOTYP)
C
      IF(IOTYP.EQ.1) THEN
C     CALL GVDFIR(NSSTFL0,
C    1            IDSST,IBACK,IM,JM,MDLINF,DTHPRO,CINF0,ITYP,IRTN)
C     WRITE(6,*) 'GVDFIR:IRTN=',IRTN
C     CALL GVDFNR(NSSTFL0,IDSST,0,'SURF','SSTA',
C    1            LABEL,JTINF,SSTA,IRTN)
C     WRITE(6,*) 'GVDFNR:IRTN=',IRTN
      WRITE(*,*)' UNKNOWN IOTYP:1'
      STOP 9999
      ELSE IF(IOTYP.EQ.3) THEN
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED
     I(NSSTFL0,
     O TYPE  ,IDSST ,FILE  ,MODEL0,RESL0 ,EXPR0 ,KTUNIT,IDTYPE,
     O IBACK ,NNSP  ,
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,
     O XID   ,XJD   ,XLATD ,XLOND ,
     O VCODD ,KMD   ,A     ,B     ,
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,
     O XIM   ,XJM   ,XLATM ,XLONM ,
     O VCODM ,KMM   ,AAM   ,BBM   ,
     O CINF0 )
C   =================================================================           
C   >>>   SST ANOMALLY                                            <<<           
C   =================================================================           
      DO 1 I=1,NNSP
        READ(NSSTFL0)
    1 CONTINUE
 3001 CALL REDDAT
     I(NSSTFL0,
     O IDSST , KT    ,
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,
     O SSTA  , IRTN  ,
     I ISST  , JSST  , 1     ,
     W BASE  , AMP   ,I2    )
      IF(ELEM.NE.'SSTA') GOTO 3001
      WRITE(6,*) '## ', TITLE, '(',UNIT,')'
      ENDIF
C
      WRITE(6,*) '## ', IDSST, KT
      IF( IDCHCK0.EQ.1 ) THEN
        CALL CVDATE( IDGES, IDSST, 24 )
        IF( IDATE(1).NE.IDGES(1).OR.IDATE(2).NE.IDGES(2).OR.
     1      IDATE(3).NE.IDGES(3) ) THEN
          WRITE(6,*) 'SSTA : DATE CHECK ERROR'
          STOP 999
        ENDIF
      ENDIF
C
      CALL LT2GAU(SSTA,ISST,JSST,IMAX,JMAX,COLRAD,GSST,DY,LY)
      CALL MOVERD(GSST, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL0 , IDATE , -1    , 'SURF', 'SSTA',
     2 'SST ANOMALLY                    ', 'K               ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
      WRITE(6,*) '## SST ANOMALLY WAS WRITTEN'
C
      ENDIF
C                                                                               
C   *****************************************************************           
C   >>>   SNOW ANALYSIS                                           <<<           
C   *****************************************************************           
      IF( NSNWFL0.NE.-1 ) THEN
C   =================================================================           
C   >>>   HEADER                                                  <<<           
C   =================================================================           
      CALL REDHED
     I(NSNWFL0,
     O TYPE  ,IDSST ,FILE  ,MODEL0,RESL0 ,EXPR0 ,KTUNIT,IDTYPE,
     O IBACK ,NNSP  ,
     O IMD   ,JMD   ,NPROD ,FLONID, FLATID,
     O XID   ,XJD   ,XLATD ,XLOND ,
     O VCODD ,KMD   ,A     ,B     ,
     O IMM   ,JMM   ,NPROM ,FLONIM, FLATIM,
     O XIM   ,XJM   ,XLATM ,XLONM ,
     O VCODM ,KMM   ,AAM   ,BBM   ,
     O CINF0 )
      DO 2 I=1,NNSP
        READ(NSNWFL0)
    2 CONTINUE
C   =================================================================           
C   >>>   SNOW ANALYSIS                                           <<<           
C   =================================================================           
      CALL REDDAT
     I(NSNWFL0,
     O IDSST , KT    ,
     O LEVEL , ELEM  , TITLE , UNIT  , KTSD  , KTSA  ,
     O SEWA  , IRTN  ,
     I ISNW  , JSNW  , 1     ,
     W BASE  , AMP   ,I2    )
      WRITE(6,*) '## ', TITLE, '(',UNIT,')'
      WRITE(6,*) '## ', IDSST, KT
      IF( IDCHCK0.EQ.1 ) THEN
        CALL CVDATE( IDGES, IDSST, 24 )
        IF( IDATE(1).NE.IDGES(1).OR.IDATE(2).NE.IDGES(2).OR.
     1      IDATE(3).NE.IDGES(3) ) THEN
          WRITE(6,*) 'SNOW : DATE CHECK ERROR'
          STOP 999
        ENDIF
      ENDIF
C   -----                                                                       
      CALL SETWHT (IMAX,JMAX,DP,IP,GAUL,GAUW,COCOT)
      DO 100 J=1,180
      DO 100 I=1,360
      WORK(I+1,J+1)=SEWA(I,J)
  100 CONTINUE
      DO 200 J=1,180
      WORK(  1,J+1)=WORK(361,J+1)
      WORK(362,J+1)=WORK(  2,J+1)
  200 CONTINUE
      DO 300 I=1,362
      WORK(I,  1)=WORK(I,  2)
      WORK(I,182)=WORK(I,181)
  300 CONTINUE
      CALL INTERP(WORK,GSNW,IMAX,JMAX,DP,IP)
C
      DO 400 J=1,JMAX
        DO 410 I=1,IMAX
          GSNW(I,J)=GSNW(I,J)/100.0
  410   CONTINUE
  400 CONTINUE
C   -----
      CALL MOVERD(GSNW, WRK, IMAX*JMAX)
      CALL WRTDAT
     1(NINFL0 , IDATE , -1    , 'SURF', 'SEW ',
     2 'SNOW EQUIVALENT WATER           ', 'M               ',
     3 0     , 0     , WRK   , IMAX  , JMAX  , I2    )
      WRITE(6,*) '## SNOW ANALYSIS WAS WRITTEN'
      ENDIF
C   =================================================================           
C   >>>   EOF                                                     <<<           
C   =================================================================           
      WRITE(6,*) '## PREGSM IS NORMAL ENDED'
C                                                                               
      END SUBROUTINE PREGSM
