      SUBROUTINE LLXY(XLATi,XLONi,X,Y,IYY,JXX,DSKM)                                LLXY.1
C                                                                                LLXY.2
C   PURPOSE   : CALCULATES THE X AND Y GIVEN THE LATITUDE AND LONGITUDE.         LLXY.3
C   ON ENTRY  :                                                                  LLXY.4
C   XLAT      : THE LATITUDE.                                                    LLXY.5
C   XLON      : THE LONGITUDE.                                                   LLXY.6
C   IEND,JEND : THE NUMBER OF DOMAIN GRID POINTS IN THE I AND J                  LLXY.7
C               DIRECTIONS IN THE COARSE DOMAIN WITH THE GRID                    LLXY.8
C               DISTANCE DSKM.                                                   LLXY.9
C   DSKM      : THE DOMAIN GRID DISTANCE.                                        LLXY.10
C   ON EXIT   :                                                                  LLXY.11
C   X         : THE COORDINATE IN X (J)-DIRECTION.                               LLXY.12
C   Y         : THE COORDINATE IN Y (I)-DIRECTION.                               LLXY.13
C                                                                                LLXY.14
C   CALLS THE INTRINSIC FUNCTIONS SIN, COS AND TAN.                              LLXY.15
C   CALLED BY SUBROUTINE PLTTER                                                  LLXY.16
C                                                                                LLXY.17
C   COMMENTS :                                                                   LLXY.18
C IN ADDITION TO THE LIST OF ARGUMENTS, SUBROUTINE LLXY GETS THE MAP             LLXY.19
C INFORMATION (THE CENTRAL LATITUDE (PHIC) AND LONGITUDE (XLONC),                LLXY.20
C THE CONE CONSTANT XN, ETC.) FROM THE COMMON BLOCK /MAPS/.  SINCE               LLXY.21
C PHIC AND XLONC ARE SET WITH RESPECT TO THE COARSE MESH, THE X AND              LLXY.22
C Y VALUES THAT COME OUT OF THE SUBROUTINE LLXY ARE THE COARSE DOMAIN            LLXY.23
C GRID INDICES THAT DEFINE THE STATION LOCATION.  THE TERRAIN PROGRAM            LLXY.24
C THEN COVERTS THE X AND Y VALUES BACK TO THE NESTED DOMAIN INDICES.             LLXY.25
C                                                                                LLXY.26
# include "maps.incl" 
C
      CENTRI = FLOAT(IYY + 1)/2.0
      CENTRJ = FLOAT(JXX + 1)/2.0 

      xlon = xloni 
      xlat = xlati
      xlat = max (xlat, -89.95) 
      xlat = min (xlat, +89.95) 
   
      YLON = XLON - XLONC 
      IF(YLON.GT.180) YLON = YLON - 360.
      IF(YLON.LT.-180) YLON = YLON + 360.

      IF ( PROJECT.EQ.'CE') THEN
         XX = YLON * DSKM
         YY = (XLAT - PHIC ) * DSKM
         XC = 0.
         YC = 0.
      ELSE IF (PROJECT.EQ.'ME') THEN                                                  LLXY.29
         XC = XCNTR                                                              LLXY.30
         YC = YCNTR                                                              LLXY.31
         CELL = COS(XLAT/CONV)/(1.0+SIN(XLAT/CONV))
         YY = -C2*ALOG(CELL)
         XX = C2*YLON/CONV           

      ELSE                                                                       LLXY.37
      PSI0 = ( POLE - PHIC )/CONV                                                LLXY.38
      XC = 0.0
C                                                                                LLXY.41
C     CALCULATE X,Y COORDS. RELATIVE TO POLE                                     LLXY.42
C                                                                                LLXY.43
cc      YLON = XLON - XLONC                                                        LLXY.44
cc      IF(YLON.GT.180) YLON = YLON - 360.                                         LLXY.45
cc      IF(YLON.LT.-180) YLON = YLON + 360.                                        LLXY.46
      FLP = XN*YLON/CONV                                                         LLXY.47
      PSX = ( POLE - XLAT )/CONV                                                 LLXY.48
      IF (PROJECT.EQ.'ST') then
        bb = 2.0*(COS(PSI1/2.0)**2)
        YC = -A*bb*TAN(PSI0/2.0)
        R  = -A*bb*TAN(PSX/2.0)
      ELSE
        bb = -A/XN*SIN(PSI1)  
        YC = bb*(TAN(PSI0/2.0)/TAN(PSI1/2.0))**XN
        R  = bb*(TAN(PSX /2.0)/TAN(PSI1/2.0))**XN
      ENDIF
      IF ( PHIC.LT.0.0 ) THEN                                                    LLXY.50
         XX = R*SIN(FLP)                                                         LLXY.51
         YY = R*COS(FLP)                                                         LLXY.52
      ELSE                                                                       LLXY.53
         XX = -R*SIN(FLP)                                                        LLXY.54
         YY = R*COS(FLP)                                                         LLXY.55
      END IF                                                                     LLXY.56
C                                                                                LLXY.57
      ENDIF                                                                      LLXY.58
C                                                                                LLXY.59
C  TRANSFORM (1,1) TO THE ORIGIN                                                 LLXY.60
C                                                                                LLXY.61
      X = ( XX - XC )/DSKM + CENTRJ                                              LLXY.64
      Y = ( YY - YC )/DSKM + CENTRI                                              LLXY.65
c      IF (PROJECT.EQ.'CE' .and. X < 1.0) X = float(JXX) + X - 1.0
      RETURN                                                                     LLXY.66
      END                                                                        LLXY.67
