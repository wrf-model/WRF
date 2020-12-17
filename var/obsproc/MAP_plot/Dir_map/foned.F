      FUNCTION FONED(X,A,B,C,D)                                                  
      FONED = 0.                                                                 
      IF (X.EQ.0.) FONED = B                                                     
      IF (X.EQ.1.) FONED = C                                                     
      IF (B*C.EQ.0.) RETURN                                                     
      IF (A*D.EQ.0.) GOTO 20                                                    
      FONED = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*            
     1   (0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C)))                                     
      RETURN                                                                    
   20 FONED = B*(1.0-X)+C*X                                                      
      IF (A.NE.0.0) FONED = B+X*(0.5*(C-A)+X*(0.5*(C+A)-B))                      
      IF (D.NE.0.0) FONED = C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C))          
      RETURN                                                                    
      END                                                                       
