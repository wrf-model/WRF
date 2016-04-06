      SUBROUTINE TTDRHQ(T,TD,P,RH,Q,ID)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                 C
C    This subroutine calculates the T, TD, RH and Q.              C
C    T, TD ------ (C) and P ------- (MB)                          C
C                                                                 C
C    ID = 1;       T, TD ------> RH, Q                            C
C    ID = 2;       RH, Q ------> T, TD                            C
C    ID = 3;       T,  Q ------> TD,RH                            C
C    ID = 4;       T, RH ------> TD, Q                            C
C                                                                 C
CcccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccC
C
      EP2 = 0.622
      SVP1 = 0.611
      SVP2 = 19.84659
      SVP3 = 5418.12
      P = P/10.0
C
      GO TO (10,20,30,40,50), ID 
C
10    CONTINUE
C
      T = T + 273.16
      TD = TD + 273.16
C (1)   T, TD -----> RH, Q
C
C  mixing ratio .........
      ES = SVP1*EXP(SVP2-SVP3/T)
      QS = EP2*ES/(P-ES)
      EM = SVP1*EXP(SVP2-SVP3/TD)
      Q  = EP2*EM/(P-EM)
C  relative humidity.....
      RH = Q/QS
      IF(RH .LT. 0.01) RH = 0.01
      IF(RH .GT. 1.00) RH = 1.00
C
      GO TO 50
C
20    CONTINUE
C
C (2)  RH, Q -----> T, TD
C
      EM = Q*P/(EP2+Q)
      TD = SVP3/(SVP2-ALOG(EM/SVP1))
      QS = Q/RH
      ES = QS*P/(EP2+QS)
      T = SVP3/(SVP2-ALOG(ES/SVP1))
C
      GO TO 50
C
C (3)  T, Q -----> RH, TD
C
30    T = T + 273.16
      EM = Q*P/(EP2+Q)
      TD = SVP3/(SVP2-ALOG(EM/SVP1))
      ES = SVP1*EXP(SVP2-SVP3/T)
      QS = EP2*ES/(P-ES)
      RH = Q/QS
C
      GO TO 50
C
C (4)  T, RH ------> TD, Q
C 
40    T = T + 273.16
      ES = SVP1*EXP(SVP2-SVP3/T)
      QS = EP2*ES/(P-ES)
      Q = QS*RH
      EM = Q*P/(EP2+Q)
      TD = SVP3/(SVP2-ALOG(EM/SVP1))
C
50    CONTINUE
      TD = TD - 273.16
      T = T - 273.16
      IF (TD .GT. T) TD = T
      P = P*10.0
C
      RETURN
      END 
