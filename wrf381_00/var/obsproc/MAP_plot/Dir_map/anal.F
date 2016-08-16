      SUBROUTINE ANAL(A2,ASTA,XOBS,YOBS,IMAX,JMAX,NSTA,RID,SPVAL)               
Cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          
c                                                                    c          
c   The subroutin ANAL completes OBJECTIVE ANALYSIS with the         c          
c   dataset ASTA(observations), XOBS(x-direction coordinates),       c          
c   YOBS(y-direction coordinates) and the largest radius of          c          
c   influence for the 1st scan, RID(in unit of grid distance).       c          
c   The value of missing data is SPVAL. Cressman technique is        c          
c   used with 3 scans while radius of infuence was decreased         c          
c   by factor, 0.75. The first scan creates the first guess          c          
c   and the 2nd and 3rd scans calculates the corrections.            c          
c   The final result is field A2(imax,jmax) with the values of       c          
c   SPVAL at those grid points on which the observations did         c          
c   not influenced.                                                  c          
c                                                                    c          
c   To filter the short waves, the 5-points smoother was used        c          
c   as many as 8 times before returning.                             c          
c                                                                    c          
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc          
C
#     include "param1.incl"
C
      DIMENSION A2(IMAX,JMAX),XOBS(NSTA),YOBS(NSTA),ASTA(NSTA),                 
     1          COR(IIMAX,JJMAX),SUM(IIMAX,JJMAX),NS(IIMAX,JJMAX),                    
     2          NS1(IIMAX,JJMAX)                                                  
C                                                                               
C     OBJECTIVE ANALYSIS TO FILL A GRID BASED ON OBSERVATIONS                   
C     XOBS AND YOBS ARE X AND Y POSITIONS ON OBSERVATIONS, NOT                  
C     NECESSARILY GRID POINTS.                                                  
C                                                                               
      IE     = IMAX-1                                                           
      JE     = JMAX-1                                                           
      DO I = 1,IMAX                                                             
      DO J = 1,JMAX                                                             
        NS1(I,J) = 0                                                            
      END DO                                                                    
      END DO                                                                    
C                                                                               
      DO 100 NSCAN = 1,3                                                        
C                                                                               
      IF ( NSCAN.EQ.1) THEN                                                     
         RIN = RID                                                              
      ELSE                                                                      
         RIN = .75*RIN                                                          
      ENDIF                                                                     
C                                                                               
C-----GRID LENGTHS IN X AND Y DIRECTIONS ARE UNITY.                             
C                                                                               
      RIS   = RIN**2                                                            
C-----RIN IS RADIUS OF INFLUENCE IN GRID UNITS                                  
C                                                                               
      DO 30 I = 1,IMAX                                                          
      DO 30 J = 1,JMAX                                                          
      COR(I,J) = 0.0                                                            
      SUM(I,J) = 0.0                                                            
      NS(I,J)  = 0                                                              
   30 CONTINUE                                                                  
C                                                                               
C-----BEGIN TO PROCESS THE NSTA OBSERVATIONS:                                   
C                                                                               
      DO 80 KK = 1,NSTA                                                         
      IF (ABS(ASTA(KK)-SPVAL).LT.1.E-3) GO TO 80                                         
C                                                                               
C-----DEFINE MAX AND MIN I AND J VALUES TO LIMIT THE NUMBER OF POINTS           
C-----MUST BE CONSIDERED.                                                       
C                                                                               
      RIOBS = YOBS(KK)                                                          
      RJOBS = XOBS(KK)                                                          
C                                                                               
      if (RJOBS.GT.JE     .OR. RJOBS.LT.1.0 .OR.                                
     1    RIOBS.GT.IE     .OR. RIOBS.LT.1.0) GO TO 80                           
C                                                                               
      YMAXI = RIOBS + RIN                                                       
      MAXI  = IFIX(YMAXI + 0.99)                                                
      MAXI  = MIN0(MAXI,IE)                                                     
C                                                                               
      YMINI = RIOBS - RIN                                                       
      MINI  = IFIX(YMINI)                                                       
      MINI  = MAX0(MINI,1)                                                      
C                                                                               
      XMAXJ = RJOBS + RIN                                                       
      MAXJ  = IFIX(XMAXJ + 0.99)                                                
      MAXJ  = MIN0(MAXJ,JE)                                                     
C                                                                               
      XMINJ = RJOBS - RIN                                                       
      MINJ  = IFIX(XMINJ)                                                       
      MINJ  = MAX0(MINJ,1)                                                      
C                                                                               
      if (nscan.eq.1) go to 77                                                  
         AAST0 = FBINT(RIOBS,RJOBS,A2,IMAX,JMAX,1)                               
         AAST = (asta(kk)-AAST0)                                                
c                                                                               
c  If the difference between interpolated value and observation                 
c  exceeded 100.0, the execution stopped.                                       
c                                                                               
        if (ABS(AAST).GT.150.0) then                                            
        i0 = int(riobs)                                                         
        j0 = int(rjobs)                                                         
        print 75,riobs,rjobs,i0,j0,kk,asta(kk),AAST0,AAST                       
75      format(2x,'riobs=',f6.2,2x,'rjobs=',f6.2,'  i0,j0=',2i4,                
     1         '  kk=',i3,2x,'ASTA=',f8.2,2X,'AAST0=',F8.2,2X,                  
     2         'AAST=',F8.2)                                                    
        do i1 = i0+2,i0-1,-1                                                    
        print 76,i1,(a2(i1,j1),j1 = j0-1,j0+2)                                  
        end do                                                                  
76      format(2x,'i0=',i3,2x,4f8.2)                                            
        PRINT *,'*** THIS IS BAD DATA, THROW IT AWAY!! ***'                     
        AAST = 0.                                                               
        endif                                                                   
c                                                                               
77    continue                                                                  
      DO 70 I=MINI,MAXI                                                         
      DO 70 J=MINJ,MAXJ                                                         
C                                                                               
      RX = FLOAT(J) - RJOBS                                                     
      RY = FLOAT(I) - RIOBS                                                     
      RSQ = RX**2+RY**2                                                         
      IF (RSQ.GE.RIS) GOTO 70                                                   
C                                                                               
      WT = (RIS - RSQ)/(RIS + RSQ)                                              
C                                                                               
C-----SAVE MAX. WEIGHTING FACTOR AND TERRAIN HEIGHT TO CHECK IF GRID            
C-----POINT SHOULD BE TREATED AS A LAND OR SEA POINT.                           
C                                                                               
      IF (WT.GT.0.0) THEN                                                       
         IF (NSCAN.GT.1) THEN                                                   
         COR(I,J) = COR(I,J) + WT*AAST                                          
         ELSE                                                                   
         COR(I,J)   = COR(I,J) + WT*ASTA(KK)                                    
         ENDIF                                                                  
         SUM(I,J)   = SUM(I,J) + WT                                             
         NS(I,J)    = NS(I,J) + 1                                               
      ENDIF                                                                     
  70  CONTINUE                                                                  
  80  CONTINUE                                                                  
C                                                                               
C-----NOW APPLY SUMMED WEIGHTS AND WEIGHTED OBSERVATIONS TO DETERMINE           
C-----TERRAIN VALUE AT I,J POINTS                                               
C                                                                               
      DO 90 I = 1,IE                                                            
      DO 90 J = 1,JE                                                            
      IF (NS(I,J) .NE. 0) THEN                                                  
         COR(I,J) = COR(I,J)/SUM(I,J)                                           
         IF (NSCAN.GT.1) THEN                                                   
         A2(I,J) = A2(I,J) + COR(I,J)                                           
         ELSE                                                                   
         A2(I,J)  = COR(I,J)                                                    
         ENDIF                                                                  
      ELSE IF (NSCAN.EQ.1) THEN                                                 
        A2(I,J) = SPVAL                                                         
C        PRINT 26,RIN,I,J                                                       
      ENDIF                                                                     
   90 CONTINUE                                                                  
C                                                                               
C   to keep the NS(i,j) of the 1st scan:                                        
C                                                                               
      IF (NSCAN.EQ.1) THEN                                                      
      do i = 1,ie                                                               
      do j = 1,je                                                               
        ns1(i,j) = ns(i,j)                                                      
      end do                                                                    
      end do                                                                    
      ENDIF                                                                     
C                                                                               
100   CONTINUE                                                                  
C                                                                               
   26 FORMAT(' NO OBSERVATIONS ARE WITHIN RIN=',F7.2,                           
     1 ' GRID LENGTHS OF I=',I3,' J=',I3)                                       
C                                                                               
C   ---MAY WANT TO SMOOTH FINAL FIELD A2 HERE                                   
c                                                                               
      c1 = .125                                                                 
C                                                                               
C  .. 8 times of smoothing:                                                     
      DO KK = 1,8                                                               
C                                                                               
      do 200 i = 2,imax-2                                                       
      do 201 j = 2,jmax-2                                                       
        if (  a2(i,j).eq.spval .or. a2(i+1,j).eq.spval .or.                     
     1      a2(i-1,j).eq.spval .or. a2(i,j+1).eq.spval .or.                     
     2      a2(i,j-1).eq.spval) then                                            
        cor(i,j) = spval                                                        
        else                                                                    
        cor(i,j) = a2(i,j)*(1.-4.*c1) + c1*(a2(i+1,j) + a2(i-1,j)               
     1           + a2(i,j+1) + a2(i,j-1))                                       
        endif                                                                   
201   continue                                                                  
200   continue                                                                  
c                                                                               
      do 210 i = 2,imax-2                                                       
      do 211 j = 2,jmax-2                                                       
        if (cor(i,j).eq.spval) go to 211                                        
        a2(i,j) = cor(i,j)                                                      
211   continue                                                                  
c      print *,' ------------- i=',i,': a2'                                     
c      print 202,(a2(i,jj),jj=2,jmax-2)                                         
202   format(15f8.2)                                                            
210   continue                                                                  
c                                                                               
      END DO                                                                    
C                                                                               
c  to set the special value at the grid points                                  
c  on which the value was just affected by 1                                    
c  obs. data:                                                                   
c                                                                               
      do i = 1,ie                                                               
      do j = 1,je                                                               
        if (ns1(i,j).eq.1) a2(i,j) = spval                                      
      end do                                                                    
      end do                                                                    
c                                                                               
      do i = 1,ie                                                               
        a2(i,jmax) = a2(i,je)                                                   
      end do                                                                    
      do j = 1,jmax                                                             
        a2(imax,j) = a2(ie,j)                                                   
      end do                                                                    
c                                                                               
      RETURN                                                                    
      END                                                                       
