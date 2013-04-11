      SUBROUTINE FFDDUV(F,D,U,V,YLON,XLONC,PHIC,IPROJ,II,ID)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                 C
C    WIND SPEED AND DIRECTION <-------> WIND COMPONENTS U AND V   C
C                                                                 C
C    ID = 1,      F, D ------> U, V                               C
C    ID = 2,      U, V ------> F, D                               C
C                                                                 C
C    II = 0,      SAME UNIT IN BOTH INPUT AND OUTPUT              C
C    II = 1,      INPUT UNIT = KNOT,     OUTPUT UNIT = M/S        C
C    II = 2,      INPUT UNIT = M/S,      OUTPUT UNIT = KNOT       C
C                                                                 C
C  ** The definition of wind components at the South/North poles  C
C     is based on the YLON provided by the station information as C
C     usual:i.e. the D = 0 degree is the wind coming from NORTH   C
C     along the longitude YLON.                                   C
C                             Y.-R. Guo   02/14/2003              C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      CHARACTER*6 NPROJ(3),IPROJ 
      DATA NPROJ/'LAMCON','POLSTR','MERCAT'/    
C
c      print '("F,D,U,V,YLON,XLONC,PHIC,IPROJ,II,ID:")'
c      print '(7f10.2,1x,A,2I3)',F,D,U,V,YLON,XLONC,PHIC,IPROJ,II,ID

      XN = 0.716
      IF(IPROJ .EQ.  NPROJ(2)) XN = 1.0
      DEGRAN = 3.1415927/180.
C
      SKN = 0.5144 
C
      GO TO (10,20,40) ID
C
10    CONTINUE
C
C (1) TO CONVERT THE WIND FROM VELOCITY FORM TO COMPONENT FORM ON
C     EARTH, AND THEN TO LAMBERT CONFORMAL PROJECTION, OR POLAR 
C     STEREOGRAPHIC PROJECTION, U AND V COMPONENTS.  CHANGEING
C     PROJECTIONS REQUIRES ONLY A CHANGE OF THE CONE CONSTANT, XN 
C     -- THE EQUATIONS REMAIN THE SAME.
C
      AEARTH = D*DEGRAN
      UEARTH = -F*SIN(AEARTH)
      VEARTH = -F*COS(AEARTH)
C     FOR CONVERSION TO GRID COORDINATES,
C     SEE PROGRAM DATAMAP, SUBR VECT, AND
C     ANTHES' METEO. 597 NOTES, EQUA. 2.23, 2.25, 2.28.
C     ALSO, CONVERT FROM KNOTS TO M/S.
      XLONRT = XLONC-YLON
      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.
      ANG=XLONRT*XN*DEGRAN
C
C   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES
C
      IF(IPROJ.EQ.NPROJ(3)) ANG=0.
      IF(PHIC.LT.0.0) ANG=-ANG
      U = VEARTH*SIN(ANG) + UEARTH*COS(ANG)
      V = VEARTH*COS(ANG) - UEARTH*SIN(ANG)
C
      GO TO 30
C
20    CONTINUE
C
      XLONRT = XLONC-YLON
      IF(XLONRT .GT. 180.) XLONRT=XLONRT-360.
      IF(XLONRT .LT.-180.) XLONRT=XLONRT+360.
      ANG=XLONRT*XN*DEGRAN
C
C   FOR MERCATOR PROJECTION, THE WINDS ARE AS IN EARTH COORDINATES
C
      IF(IPROJ.EQ.NPROJ(3)) ANG=0.
      IF(PHIC.LT.0.0) ANG=-ANG
      UE = U*COS(ANG) - V*SIN(ANG)
      VE = U*SIN(ANG) + V*COS(ANG)
C
      F = SQRT(UE*UE + VE*VE)
      IF (F.EQ.0.0) THEN
         D = 99999.0
         GO TO 30
      ENDIF
C
      IF (VE.EQ.0.0) THEN
         IF (UE.GT.0.0) D = 270.
         IF (UE.LT.0.0) D =  90.
C
      ELSE
      AEARTH = ATAN(UE/VE)/DEGRAN
      IF (UE.LE.0.0 .AND. VE.LE.0.0 ) D = AEARTH
      IF (UE.LE.0.0 .AND. VE.GE.0.0 ) D = AEARTH + 180.0
      IF (UE.GE.0.0 .AND. VE.GE.0.0 ) D = AEARTH + 180.0
      IF (UE.GE.0.0 .AND. VE.LE.0.0 ) D = AEARTH + 360.0
      ENDIF
C
30    CONTINUE
C
      IF (II.EQ.0) GO TO 40
      IF (II.EQ.1) THEN
         IF (ID.EQ.1) THEN
            U = U*SKN
            V = V*SKN
         ELSE IF (ID.EQ.2) THEN
            F = F*SKN
         ENDIF
      ELSE IF (II.EQ.2) THEN
         IF (ID.EQ.1) THEN
            U = U/SKN
            V = V/SKN
         ELSE IF (ID.EQ.2) THEN
            F = F/SKN
         ENDIF
      ENDIF
C
40    RETURN
      END
