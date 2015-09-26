      SUBROUTINE BINT(PP,XX,YY,LIST,III,JJJ,ICRS)
C
C --- BI-LINEAR INTERPOLATION AMONG FOUR GRID VALUES
C
C     INPUT : LIST, XX, YY
C     OUTPUT: PP
C
      REAL LIST(III,JJJ),STL(4,4)
      IB=III-ICRS
      JB=JJJ-ICRS
      PP=0.0
      N = 0
      I=INT(XX+0.00001)
      J=INT(YY+0.00001)
      X=XX-I
      Y=YY-J
      IF ((ABS(X).GT.0.00001).OR.(ABS(Y).GT.0.00001)) THEN
C
      DO 2 K=1,4
        KK=I+K-2
        DO 2 L=1,4
          STL(K,L)=0.
          LL=J+L-2
          IF ((KK.GE.1 ) .AND. (KK.LE.IB) .AND.
     -        (LL.LE.JB) .AND. (LL.GE.1 )) THEN
          STL(K,L) = LIST(KK,LL)
          N=N+1
C .. a zero value inside the domain being set to 1.E-12:
          IF (STL(K,L).EQ.0.) STL(K,L)=1.E-12
          ENDIF
2     CONTINUE
C
      CALL ONED(A,X,STL(1,1),STL(2,1),STL(3,1),STL(4,1))
      CALL ONED(B,X,STL(1,2),STL(2,2),STL(3,2),STL(4,2))
      CALL ONED(C,X,STL(1,3),STL(2,3),STL(3,3),STL(4,3))
      CALL ONED(D,X,STL(1,4),STL(2,4),STL(3,4),STL(4,4))
C
C .. CHECK TANGENT LINEAR OF ONED, SAVE BASIC STATE:
C      WRITE(20) XX,YY,Y,A,B,C,D
C
      CALL ONED(PP,Y,A,B,C,D)
        IF(N.NE.16) THEN
          CALL ONED(E,Y,STL(1,1),STL(1,2),STL(1,3),STL(1,4))
          CALL ONED(F,Y,STL(2,1),STL(2,2),STL(2,3),STL(2,4))
          CALL ONED(G,Y,STL(3,1),STL(3,2),STL(3,3),STL(3,4))
          CALL ONED(H,Y,STL(4,1),STL(4,2),STL(4,3),STL(4,4))
C .. CHECK TANGENT LINEAR OF ONED, SAVE BASIC STATE:
C      WRITE(20) XX,YY,X,E,F,G,H
C
          CALL ONED(QQ,X,E,F,G,H)
          PP = (PP+QQ)*0.5
        ENDIF
C
      ELSE
C
      PP = LIST(I,J)
      ENDIF
C
      RETURN
      END



