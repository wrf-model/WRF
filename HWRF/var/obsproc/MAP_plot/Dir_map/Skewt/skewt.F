      SUBROUTINE SKEWT (PRES,TEMP,DWPT,SPD,DIR,NN,TITLINE,
     &   MDATE,XTIME,ISTAT,XLOC,YLOC,YLAT,YLON,
     &   ILW,ICT,ICD,ICW,LCF,WOFFS,BADVAL)
C
C     SKEWT- PLOTS SOUNDINGS ON A SKEWT, LOG P THERMODYNAMIC DIAGRAM
C
C     PRES- PRESSURE ARRAY FOR THERMODYNAMIC DATA (MB)
C     TEMP- TEMPERATURE ARRAY (CELSIUS)
C     DWPT- DEW POINT ARRAY (CELSIUS)
C     SPD- WIND SPEED ARRAY (M/S)
C     DIR- WIND DIRECTION ARRAY (DEGREES-0 IS NORTH)
C     NN- NUMBER OF DATA LEVELS
C     TITLINE - 80-character title.
C     MDATE - MODEL INITIAL TIME YYMMDDHH
C     XTIME - FORECAST INCREMENT
C     ISTAT- STRING CONTAINING STATION NAME (40 CHAR.)
C     XLOC, YLOC - X and Y locations
C     YLAT, YLON - Lat and Lon
C     ILW -  LINE WIDTH ( 2000 recommended for thin, 6000 recommended for thick)
C     LCF - LOGICAL CALL FRAME.  If .TRUE., FRAME is called.
C     BADVAL- VALUE ASSIGNED TO MISSING DATA  --TEMP,DWPT TRACES ARE
C                   TERMINATED AS SOON AS THIS NUMBER IS ENCOUNTERED.
C
C        OUTPUT PARAMETERS.....
C     ALL INPUT PARAMETERS REMAIN UNCHANGED BY THIS ROUTINE.
C
      DIMENSION PRES(NN),TEMP(NN),DWPT(NN),SPD(NN),DIR(NN)
      CHARACTER LAB*120, ISTAT*40
C
      CHARACTER *80    TITLINE
      CHARACTER *2     ISPOPT
      INTEGER          ITSTRT,
     *                 ITITLEN,
     *                 LCNTR
      COMMON /SKWDRW/ ISKDRW
C
      LOGICAL LCF
C
C
C  DEGREES TO RADIANS
      PARAMETER (DTR = 0.0174532925)
C  WIND BARB DATA
      PARAMETER (XM = 24.2)
C
C  LINE WIDTH VARIABLES
C
      DATA ISPOPT / 'LW' /
      DATA ISWIDE / 1000 /
C
C  STATEMENT FUNCTIONS:  MAPPINGS FROM (P,T) TO CM ON SKEWT
C
      FY(P) = 132.182 - 44.061 * ALOG10(P)
      FX(T,Y) = 0.54 * T + 0.90692 * Y
C
C      PRINT 99,ICT,ICD,ICW,LCF
 99   FORMAT('== SKEWT: ICTL,ICDL,ICWL,LCF:',3I2,2X,L2)
      ICTL = abs(ICT)
      ICDL = abs(ICD)
      ICWL = abs(ICW)
C
C  Initialization
C
      DO I = 1, 120
         LAB(I:I) = ' '
      ENDDO
C
C  Check Character Strings
C
      DO I = 1, 40
         if ( (ichar(istat(i:i)).lt.32) .or.
     &        (ichar(istat(i:i)).gt.127)) then
            print*, 'ISTAT PROBLEM.'
            istat(i:i) = '*'
         endif
      enddo
      DO I = 1, 80
         if ( (ichar(titline(i:i)).lt.32) .or.
     &        (ichar(titline(i:i)).gt.127)) then
            print*, 'TITLINE PROBLEM.'
            titline(i:i) = '*'
         endif
      enddo

C
C  TEST TO SEE IF A BACKGROUND HAS BEEN DRAWN, IF NOT CALL SKEWT_BACKGROUND
C
      CALL GSPLCI (9 )
      CALL GSPMCI (9 )
      CALL GSTXCI (9 )

      IF (ISKDRW .EQ. 0) THEN
        CALL SKEWT_BACKGROUND
        ISKDRW = 1
      END IF

      CALL GSPLCI (9 )
      CALL GSPMCI (9 )
      CALL GSTXCI (9 )
C
C  SKEWT BACKGROUND HAS BEEN GENERATED-- PLOT THE SOUNDING
C
      CALL GFLAS3 (1)
      CALL SET(.05,.95,.05,.95,-19.0,27.1,-.9346217,44.061,1)
C

C  PUT ON TITLE

      ITSTRT = INDEX(TITLINE,':')
      IF (ITSTRT .NE. 0) THEN
         DO LCNTR=1,80-ITSTRT
            TITLINE(LCNTR:LCNTR) = TITLINE(LCNTR+ITSTRT:LCNTR+ITSTRT)
         ENDDO
         DO LCNTR=80-ITSTRT+2,80
            TITLINE(LCNTR:LCNTR)=' '
         ENDDO
         ITITLEN = 80
         DO 100 LCNTR=80,1,-1
            IF (TITLINE(LCNTR:LCNTR) .NE. ' ') THEN
               ITITLEN = LCNTR
               GOTO 110
            END IF
 100     CONTINUE
 110     CONTINUE
      ELSE
         ITITLEN=LEN(TITLINE)
      END IF

      CALL WTSTR (4.05,-2.4,TITLINE(1:ITITLEN),12,0,0)
      IF (XTIME .LT. -50.) THEN
	WRITE(LAB,106) ISTAT, MDATE, XLOC, YLOC,
     &      YLAT, YLON
      ICHRLEN = 91
      ELSE IF (XTIME .GT. -50.) THEN
        WRITE(LAB,107) ISTAT, MDATE, NINT(XTIME), XLOC,
     &      YLOC, YLAT, YLON
      ICHRLEN = 98
      ENDIF
      CALL WTSTR (-19.5,-1.4,LAB(1:ICHRLEN),8,0,-1)
  106 FORMAT(1X,A40,2X,I8,'  X=',F6.2,' Y=',F6.2,
     &   2X,F7.2,2X,F7.2)
  107 FORMAT(1X,A40,2X,I8,' +',I3,'H  X=',F6.2,' Y=',F6.2,
     &   2X,F7.2,2X,F7.2)
            CALL GSPMCI (1 )
            CALL GSTXCI (1 )
            CALL GSPLCI (ICTL)   ! Make wind-barb line the color of the T line.
      IF(NN.GT.0) CALL LINE(XM+woffs,-.9346217,XM+woffs,44.061)
            CALL GSPLCI (1 )
C
C  SOLID DASH PATTERN, INCREASED SPOT SIZE (DEFAULT=8)
C
      CALL DASHDB (65535)
      CALL GETUSV (ISPOPT,ISNORM)
      CALL SETUSV (ISPOPT,ISWIDE)
C
            CALL SETUSV('LW',ilw)
            CALL GSPLCI (ictl)
            CALL GSPMCI (ictl)
            CALL GSTXCI (ictl)

      II = 0
      DO 60 I=1,NN
         IF (PRES(I).lt.100.0) goto 60
         IF (abs(TEMP(I)-BADVAL).gt.1.) THEN
            II =  II + 1
            Y=FY(PRES(I))
            X=FX(TEMP(I),Y)
            IF (II.EQ.1) CALL FRSTPT(X,Y)
            IF (ict.GE.0) CALL VECTOR(X,Y)
         ENDIF
 60   CONTINUE

      CALL SETUSV('LW',ilw)
      CALL GSPLCI (icdl)
      CALL GSPMCI (icdl)
      CALL GSTXCI (icdl)

      II = 0
      DO 70 I=1,NN
         IF (PRES(I).lt.100.0) goto 70
         IF (abs(DWPT(I)-BADVAL).gt.1.)THEN
            II = II + 1
            Y=FY(PRES(I))
            X=FX(DWPT(I),Y)
            IF(II.EQ.1)CALL FRSTPT(X,Y)
            IF (ICD.GE.0) CALL VECTOR(X,Y)
         ENDIF
 70   CONTINUE

            CALL SETUSV('LW',2000)
            CALL GSPLCI (8 )
            CALL GSPMCI (8 )
            CALL GSTXCI (8 )
C
C         PLOT WIND VECTORS
C
      IF (NN.LE.0) GO TO 76

      CALL SETUSV (ISPOPT,ISWIDE)
      DO 75 I=1,NN
         IF (PRES(I).lt.100.0) goto 75
         if ((abs(spd(i)-badval).lt.1).or.
     &       (abs(dir(i)-badval).lt.1)) go to 75
         IF (DIR(I) .GT. 360.) GO TO 75
          ANG=DIR(I)*DTR
          U = -SPD(I)*SIN(ANG)
          V = -SPD(I)*COS(ANG)
          Y1=FY(PRES(I))
         IF (ICW.GE.0) CALL SKEWT_WNDBARB (XM+WOFFS,Y1,U,V)
   75 CONTINUE
   76 CONTINUE
      CALL GSPLCI (1 )
            CALL GSPMCI (1 )
            CALL GSTXCI (1 )
C
C  RESET TO NORMAL SPOT SIZE AND EXIT
C
      CALL SETUSV (ISPOPT,ISNORM)
C
      IF (LCF) CALL FRAME
C
      RETURN
      END
