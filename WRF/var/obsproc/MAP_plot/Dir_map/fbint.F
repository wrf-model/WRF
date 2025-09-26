C                                                                               
      FUNCTION FBINT(XX,YY,LIST,III,JJJ,IBINT)                                   
C                                                                               
C-----BILINEAR INTERPOLATION AMONG FOUR GRID VALUES                             
C                                                                               
      REAL LIST(III,JJJ),STL(4,4)                                               
C                                                                               
      IB = III-IBINT                                                            
      JB = JJJ-IBINT                                                            
      FBINT = 0.0                                                                
      N    = 0                                                                  
      I    = INT(XX+0.00001)                                                    
      J    = INT(YY+0.00001)                                                    
      X    = XX-I                                                               
      Y    = YY-J                                                               
      IF (ABS(X).GT.0.0001.OR.ABS(Y).GT.0.0001) GOTO 10                         
      FBINT = LIST(I,J)                                                          
      RETURN                                                                    
   10 CONTINUE                                                                  
      DO 20 K=1,4                                                               
      KK = I+K-2                                                                
      DO 20 L=1,4                                                               
      STL(K,L) = 0.                                                             
      LL = J+L-2                                                                
      IF (KK.LT.1.OR.KK.GT.IB) GOTO 20                                          
      IF (LL.GT.JB.OR.LL.LT.1) GOTO 20                                          
      STL(K,L) = LIST(KK,LL)                                                    
      N = N+1                                                                   
      IF (STL(K,L).EQ.0.0) STL(K,L) = 1.E-25                                    
   20 CONTINUE                                                                  
C                                                                               
C-----FIND INDEX OF CLOSEST POINT TO XX,YY.                                     
C                                                                               
      A = FONED(X,STL(1,1),STL(2,1),STL(3,1),STL(4,1))                           
      B = FONED(X,STL(1,2),STL(2,2),STL(3,2),STL(4,2))                           
      C = FONED(X,STL(1,3),STL(2,3),STL(3,3),STL(4,3))                           
      D = FONED(X,STL(1,4),STL(2,4),STL(3,4),STL(4,4))                           
      FBINT = FONED(Y,A,B,C,D)                                                    
C                                                                               
      IF (N.EQ.16) RETURN                                                       
      E = FONED(Y,STL(1,1),STL(1,2),STL(1,3),STL(1,4))                           
      F = FONED(Y,STL(2,1),STL(2,2),STL(2,3),STL(2,4))                           
      G = FONED(Y,STL(3,1),STL(3,2),STL(3,3),STL(3,4))                           
      H = FONED(Y,STL(4,1),STL(4,2),STL(4,3),STL(4,4))                           
      FBINT = (FBINT+FONED(X,E,F,G,H))/2.                                          
      RETURN                                                                    
      END                                                                       
