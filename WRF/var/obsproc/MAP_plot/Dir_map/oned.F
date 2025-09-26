      SUBROUTINE ONED(Y,X,A,B,C,D)
C
C ..  Input : X, A, B, C, D
C     Output: Y
C       1, 2, 3, and 4 points interpolation:
C       In this subroutine, the zero value of A, B, C, D means that
C       point outside the domain.      
C       
C .. 1-point:
C .. take the value at the second point:
      IF (X.EQ.0.) THEN
        ONE=B
C .. take the value at the third point:
      ELSE IF (X.EQ.1.) THEN
        ONE=C
C .. the point X outside the range:
      ELSE IF (B*C .EQ. 0.) THEN
        ONE=0.
      ELSE
        IF (A*D .EQ. 0.) THEN
C .. 3-point interpolation:
          IF (A.NE.0.) THEN
          ONE = B+X*(0.5*(C-A)+X*(0.5*(C+A)-B))
          ELSE IF (D.NE.0.) THEN
          ONE = C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C))
          ELSE
C .. 2-point interpolation:
          ONE = B*(1.0-X)+C*X
          ENDIF
        ELSE
C .. 4-point interpolation:
          ONE = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+
     >        X*(C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C)))
        ENDIF
      ENDIF
C
      Y = ONE
C
      RETURN

      END
