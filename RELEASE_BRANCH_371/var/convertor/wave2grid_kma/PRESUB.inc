      SUBROUTINE CRH2SH( WV, T, PS, A, B, IJMAX, LMAX, LARHM,                   
     \                   QMIN, ITERMX )                                         
                                                                                
      DIMENSION                                                                 
     1        WV(IJMAX,LMAX), T(IJMAX,LMAX), PS(IJMAX), A(LMAX), B(LMAX)        
                                                                                
C     INPUT  WV : RELATIVE HUMIDITY( 0-1 )   ( L=<LARHM ) : CHANGED             
C               : SPECIFIC HUMIDITY( KG/KG ) ( L >LARHM ) : UNCHANGED           
C            T  : VIRTUAL TEMPERATURE(K)                  : CHANGED             
C            PS : SURFACE PRESSURE(HPA)                                         
C     OUTPUT WV : SPECIFIC HUMIDITY( KG/KG ) ( FOR ALL LEVELS )                 
C            T  : REAL TEMPERATURE(K)                                           
                                                                                
      EE = 1.D0/(0.608D0*t_kelvin)                                               
      FF = 1.D0/(0.608D0*35.9D0)                                                
      GG = 6.11D0*0.622D0                                                       
      CC = t_kelvin/35.9D0*7.5D0*LOG(10.D0)                                      
                                                                                
      DO 100 IJ = 1, IJMAX                                                      
                                                                                
        DO 110 L = 1, LARHM                                                     
                                                                                
          VT = T (IJ,L)                                                         
          RH = WV(IJ,L)                                                         
          IF( L.LE.LMAX-1 ) THEN                                                
            PHF1 = A(L  )+B(L  )*PS(IJ)                                         
            PHF2 = A(L+1)+B(L+1)*PS(IJ)                                         
            PL   = EXP( ( PHF1*LOG(PHF1)-PHF2*LOG(PHF2) )                       
     \                    /( PHF1-PHF2 )-1.0 )                                  
          ELSE                                                                  
            PL   = 0.5*( A(L)+B(L)*PS(IJ) )                                     
          END IF                                                                
                                                                                
          AA = GG*RH/PL                                                         
          BB = EE*(VT-t_kelvin)                                                    
          DD = FF*(VT-35.9)                                                     
          HH = CC*(BB-DD)                                                       
                                                                                
C         繰り返し計算開始 : QQ = INITIAL VALUE OF SPECIFIC HUMIDITY        
                                                                                
          QQ = AA*EXP(CC*BB/DD)                                                 
                                                                                
          DO 120 ITER = 1, ITERMX                                               
            QV  = AA*EXP(CC*(QQ-BB)/(QQ-DD))                                    
            FQ  = QV-QQ/(1.0+0.608*QQ)                                          
            DFQ = HH*QV/((QQ-DD)**2)-1.0/(1.0+0.608*QQ)**2                      
            QQ  = QQ-FQ/DFQ                                                     
            IF( QQ.LT.QMIN ) QQ = QMIN                                          
  120     CONTINUE                                                              
                                                                                
C         繰り返し計算終了→実温度の計算                                    
                                                                                
          TT = VT/(1.0+0.608*QQ)                                                
          T (IJ,L) = TT                                                         
          WV(IJ,L) = QQ                                                         
                                                                                
  110   CONTINUE                                                                
                                                                                
        IF( LARHM.LT.LMAX ) THEN                                                
          DO 130 L = LARHM+1, LMAX                                              
            T(IJ,L) = T(IJ,L)/(1.0+0.608*WV(IJ,L))                              
  130     CONTINUE                                                              
        ENDIF                                                                   
                                                                                
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE CRH2SH
C**********************************************************************         
      SUBROUTINE CRH2SHA                                                        
     I(IJMAX,KMAX,PS,A,B,GRAV,GASR,TLAPS,QCONS,QMIN,KST,ITERMX,                 
*-->                                                                            
     I IDX, LARHM,                                                              
*<--                                                                            
     O WV,T)                                                                    
C                                                                               
C***  CALCULATE SPECIFIC HUMIDITY AND REAL TEMPERATURE                          
C   FROM RELATIVE HUMIDITY ,REAL TEMPERATURE AND VIRTUAL TEMPERATURE            
C   CORRECTION                                                                  
C     SPECIFIC HUMIDITY = CONSTANT IN STRATOSPHERE                              
C                                                                               
C*** (ARRAYS)    (INPUT) ******************* (OUTPUT) ******************        
C     WV         RELATIVE HUMIDITY(NOT %)    SPECIFIC HUMIDITY(KG/KG)           
C     T          VIRTUAL TEMPERATURE(K)      REAL TEMPERATURE(K)                
C     PS         SURFACE PRESSURE (MB)       (UNCHANGED)                        
C     A,B        A+B*PS DEFINES INTER-LAYER LEVEL                               
C***********************************************************************        
      DIMENSION WV(IJMAX,KMAX),T(IJMAX,KMAX),PS(IJMAX),A(KMAX),B(KMAX)          
      DATA Q90/2.7E-6/   ! SAME AS IN WV300M                                    
      COMMON/CTETEN/TABLE(25000)                                                
      COMMON/DTETEN/DTABLE(25000)                                               
      REAL*8 TABLE,DTABLE,X                                                     
      LOGICAL FIRST/.TRUE./                                                     
C                                                                               
      IF(FIRST) THEN                                                            
          GBYR=GRAV/GASR                                                        
      ENDIF                                                                     
C                                                                               
      DO 100 K=1,KMAX                                                           
      DO 100 I=  1,IJMAX                                                        
      VT=T  (I,K)                                                               
      IF( K.LT.LARHM ) THEN                                                     
      RH=WV (I,K)                                                               
      ENDIF                                                                     
C                                                                               
      IF(K.LE.KMAX-1) THEN                                                      
      PHF1=A(K  )+B(K  )*PS(I)                                                  
      PHF2=A(K+1)+B(K+1)*PS(I)                                                  
      PL  =EXP( (PHF1*LOG(PHF1)-PHF2*LOG(PHF2))/(PHF1-PHF2)-1.0 )               
      ELSE                                                                      
      PL  =0.5*(A(K)+B(K)*PS(I))                                                
      END IF                                                                    
C                                                                               
C                                                                               
          T0 = VT                                                               
       YI = (T0-123.2D0)*100.0D0                                                
       IY = YI                                                                  
       IY = MAX( IY, 1 )                                     ! 96/01/31         
       IY = MIN( IY, 24999 )                                 ! 96/01/31         
       X = YI - IY                                                              
       QSAT = ((1.0D0-X)*TABLE(IY)+X*TABLE(IY+1))/PL                            
CMM     IF((IDX.EQ.1).AND.(PL.LE.90.)) THEN                                     
CMM       QQ=MIN(Q90,QSAT*0.9)                                                  
CMM     ELSE                                                                    
      IF( K.LT.LARHM ) THEN                                                     
       DQSAT = ((1.0D0-X)*DTABLE(IY)+X*DTABLE(IY+1))/PL                         
       FT = VT-T0-0.608*T0*QSAT*RH                                              
       DFT = -1.0-0.608*QSAT*RH-0.608*T0*DQSAT*RH                               
       T0 = T0 - FT/DFT                                                         
C                                                                               
       YI = (T0-123.2D0)*100.0D0                                                
       IY = YI                                                                  
       IY = MAX( IY, 1 )                                    ! 96/01/31          
       IY = MIN( IY, 24999 )                                ! 96/01/31          
       X = YI - IY                                                              
       QSAT = ((1.0D0-X)*TABLE(IY)+X*TABLE(IY+1))/PL                            
       DQSAT = ((1.0D0-X)*DTABLE(IY)+X*DTABLE(IY+1))/PL                         
       FT = VT-T0-0.608*T0*QSAT*RH                                              
       DFT = -1.0-0.608*QSAT*RH-0.608*T0*DQSAT*RH                               
       T0 = T0 - FT/DFT                                                         
C                                                                               
       YI = (T0-123.2D0)*100.0D0                                                
       IY = YI                                                                  
       IY = MAX( IY, 1 )                                    ! 96/01/31          
       IY = MIN( IY, 24999 )                                 ! 96/01/31         
       X = YI - IY                                                              
       QQ = ((1.0D0-X)*TABLE(IY)+X*TABLE(IY+1))/PL*RH                           
      ELSE                                                                      
       QQ = WV(I,K)                                                             
      ENDIF                                                                     
C                                                                               
      IF((IDX.EQ.1).AND.(PL.LE.90.)) THEN                                       
        QQ=MIN(QQ,QSAT*0.9)                                                     
      ENDIF                                                                     
C                                                                               
C +++ CALCULATE REAL TEMPERATURE +++                                            
C                                                                               
          TT=VT/(1.+0.608*QQ)                                                   
C                                                                               
      T (I,K)=TT                                                                
      WV(I,K)=QQ                                                                
C                                                                               
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE CRH2SHA                                                        
C ---------------------------------------------------------------------         
      SUBROUTINE SPLDIF( ZOUT, POUT, LMXOUT, ZIN, PIN, LMXIN )                  
                                                                                
      DIMENSION ZOUT(LMXOUT), POUT(LMXOUT), ZIN(LMXIN), PIN(LMXIN)              
      DIMENSION SM(40), H(40), AL(40), AM(40), AP(40), C(40)                    
                                                                                
C     INPUT / ZIN (L), PIN (L), LMXIN  : INPUT.DATA, PRES(LOG), NUMBER          
C     OUTPUT/ ZOUT(L), POUT(L), LMXOUT : OUTPUT-VAL, PRES(LOG), NUMBER          
                                                                                
      LM1 = LMXIN-1                                                             
      GR = -gravity/gas_constant
                                                                                
      DO 110 L = 2, LMXIN                                                       
        H(L) = PIN(L)-PIN(L-1)                                                  
  110 CONTINUE                                                                  
                                                                                
      DO 120 L = 2, LM1                                                         
        AL(L) = 0.5*H(L+1)/(H(L)+H(L+1))                                        
        AM(L) = 0.5-AL(L)                                                       
  120 CONTINUE                                                                  
                                                                                
C     終端条件( LAPSE RATE IS CONSTANT )                                    
C     SM(1) = SM(2) ; SM(LMXIN-1) = SM(LMXIN) : SECOND DERIVATIVE               
                                                                                
      AL(1)     = -1.0                                                          
      AM(LMXIN) = -1.0                                                          
      AL(LMXIN) =  0.0                                                          
                                                                                
      DO 130 L = 2, LMXIN                                                       
        AP(L) = 1.0/(1.0-AL(L-1)*AM(L))                                         
        AL(L) = AL(L)*AP(L)                                                     
  130 CONTINUE                                                                  
                                                                                
      C(1)     = 0.0                                                            
      C(LMXIN) = 0.0                                                            
      DO 160 L = 2, LM1                                                         
        C(L) = 3.0*((ZIN(L+1)-ZIN(L))/H(L+1) - (ZIN(L)-ZIN(L-1))/H(L))          
     \            /(H(L)+H(L+1))                                                
  160 CONTINUE                                                                  
                                                                                
C     FORWARD SUBSTITUTION                                                      
                                                                                
      DO 200 L = 2, LMXIN                                                       
        C(L) = (C(L)-C(L-1)*AM(L))*AP(L)                                        
  200 CONTINUE                                                                  
      SM(LMXIN) = C(LMXIN)                                                      
                                                                                
C     BACKWARD SUBSTUTUTION                                                     
                                                                                
      DO 220 K = 1, LM1                                                         
        L = LMXIN-K                                                             
        SM(L) = C(L)-AL(L)*SM(L+1)                                              
  220 CONTINUE                                                                  
                                                                                
C     INTERPOLATION                                                             
                                                                                
      LB = 2                                                                    
                                                                                
      DO 500 LOUT = 1, LMXOUT                                                   
                                                                                
        X = POUT(LOUT)                                                          
        DO 300 L = LB, LMXIN                                                    
          IF( X.GE.PIN(L) ) GO TO 310                                           
  300   CONTINUE                                                                
        L = LMXIN                                                               
  310   LB = L                                                                  
                                                                                
C       ３次スプライン関数の微分                                            
                                                                                
        ZOUT(LOUT) = SM(L-1)*(  -(PIN(L)-X)**2 /(2.0*H(L))+H(L)/6.0 )           
     \             + SM(L)  *( (X-PIN(L-1))**2 /(2.0*H(L))-H(L)/6.0 )           
     \             + ( ZIN(L)-ZIN(L-1) )/H(L)                                   
        ZOUT(LOUT) = ZOUT(LOUT)*GR                                              
                                                                                
  500 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END SUBROUTINE SPLDIF
C ---------------------------------------------------------------------         
      SUBROUTINE SPLDIF3( ZOUT, POUT, LMXOUT, ZIN, PIN, LMXIN, IJMAX,           
     W SM, H, AL, AM, AP, C )                                                   
      DIMENSION ZOUT(IJMAX,LMXOUT), POUT(IJMAX,LMXOUT),                         
     1          ZIN (IJMAX,LMXIN),  PIN (IJMAX,LMXIN)                           
CMM   DIMENSION ZOUT(LMXOUT), POUT(LMXOUT), ZIN(LMXIN), PIN(LMXIN)              
      DIMENSION SM(IJMAX,LMXIN), H(IJMAX,LMXIN), AL(IJMAX,LMXIN),               
     1          AM(IJMAX,LMXIN), AP(IJMAX,LMXIN), C(IJMAX,LMXIN)                
CMM   DIMENSION SM(40), H(40), AL(40), AM(40), AP(40), C(40)                    
                                                                                
C     INPUT / ZIN (L), PIN (L), LMXIN  : INPUT.DATA, PRES(LOG), NUMBER          
C     OUTPUT/ ZOUT(L), POUT(L), LMXOUT : OUTPUT-VAL, PRES(LOG), NUMBER          
                                                                                
      LM1 = LMXIN-1                                                             
      GR = -gravity/gas_constant
                                                                                
      DO 110 L = 2, LMXIN                                                       
      DO 110 I = 1, IJMAX                                                       
        H(I,L) = PIN(I,L)-PIN(I,L-1)                                            
  110 CONTINUE                                                                  
                                                                                
      DO 120 L = 2, LM1                                                         
      DO 120 I = 1, IJMAX                                                       
        AL(I,L) = 0.5*H(I,L+1)/(H(I,L)+H(I,L+1))                                
        AM(I,L) = 0.5-AL(I,L)                                                   
  120 CONTINUE                                                                  
                                                                                
C     終端条件( LAPSE RATE IS CONSTANT )                                    
C     SM(1) = SM(2) ; SM(LMXIN-1) = SM(LMXIN) : SECOND DERIVATIVE               
      DO 125 I = 1, IJMAX                                                       
        AL(I,1)     = -1.0                                                      
        AM(I,LMXIN) = -1.0                                                      
        AL(I,LMXIN) =  0.0                                                      
  125 CONTINUE                                                                  
                                                                                
      DO 130 L = 2, LMXIN                                                       
      DO 130 I = 1, IJMAX                                                       
C                                                                               
        AP(I,L) = 1.0/(1.0-AL(I,L-1)*AM(I,L))                                   
        AL(I,L) = AL(I,L)*AP(I,L)                                               
  130 CONTINUE                                                                  
                                                                                
      DO 155 I = 1, IJMAX                                                       
      C(I,1)     = 0.0                                                          
      C(I,LMXIN) = 0.0                                                          
  155 CONTINUE                                                                  
C                                                                               
      DO 160 L = 2, LM1                                                         
      DO 160 I = 1, IJMAX                                                       
        C(I,L) = 3.0*((ZIN(I,L+1)-ZIN(I,L))/H(I,L+1)                            
     1         - (ZIN(I,L)-ZIN(I,L-1))/H(I,L))                                  
     2            /(H(I,L)+H(I,L+1))                                            
  160 CONTINUE                                                                  
                                                                                
C     FORWARD SUBSTITUTION                                                      
                                                                                
      DO 200 L = 2, LMXIN                                                       
      DO 200 I = 1, IJMAX                                                       
        C(I,L) = (C(I,L)-C(I,L-1)*AM(I,L))*AP(I,L)                              
  200 CONTINUE                                                                  
      DO 205 I = 1, IJMAX                                                       
      SM(I,LMXIN) = C(I,LMXIN)                                                  
  205 CONTINUE                                                                  
                                                                                
C     BACKWARD SUBSTUTUTION                                                     
                                                                                
      DO 220 K = 1, LM1                                                         
      DO 220 I = 1, IJMAX                                                       
        L = LMXIN-K                                                             
        SM(I,L) = C(I,L)-AL(I,L)*SM(I,L+1)                                      
  220 CONTINUE                                                                  
                                                                                
C     INTERPOLATION                                                             
                                                                                
C     LB = 2                                                                    
                                                                                
      DO 500 LOUT = 1, LMXOUT                                                   
      DO 500 I = 1, IJMAX                                                       
                                                                                
        X = POUT(I,LOUT)                                                        
        L = LOUT ! FOR ONLY PIN.EQ.POUT                                         
        IF( L.LT.2 ) L=2                                                        
CM      DO 300 L = LB, LMXIN                                                    
CM        IF( X.GE.PIN(L) ) GO TO 310                                           
CM300   CONTINUE                                                                
CM      L = LMXIN                                                               
CM310   LB = L                                                                  
                                                                                
        ZOUT(I,LOUT) = SM(I,L-1)*(  -(PIN(I,L)-X)**2                            
     1             /(2.0*H(I,L))+H(I,L)/6.0 )                                   
     2             + SM(I,L)  *( (X-PIN(I,L-1))**2                              
     3             /(2.0*H(I,L))-H(I,L)/6.0 )                                   
     \             + ( ZIN(I,L)-ZIN(I,L-1) )/H(I,L)                             
        ZOUT(I,LOUT) = ZOUT(I,LOUT)*GR                                          
                                                                                
  500 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
      END SUBROUTINE SPLDIF3
C======================================================================         
      SUBROUTINE SETWHT                                                         
     1(IMAX,JMAX,DP,IP,DCOSCL,DGW,COCLT)                                        
      DIMENSION DP(4,IMAX,JMAX)                                                 
      INTEGER*2 IP(2,IMAX,JMAX)                                                 
      REAL*8  DCOSCL(JMAX),DGW(JMAX),COCLT(JMAX)                                
C                                                                               
Crizvi     Already defined in module_wave2grid_kma
C      JMAXHF=JMAX/2                                                             
      DELX=360.0/FLOAT(IMAX)                                                    
      CALL GAUSS(DCOSCL,DGW,JMAX)                                               
C                                                                               
      R2D=180.0/pi
      DO 100 J=1,JMAXHF                                                         
      COCLT(       J)=      R2D*ACOS(DCOSCL(J))                                
      COCLT(JMAX+1-J)=180.0-R2D*ACOS(DCOSCL(J))                                
  100 CONTINUE                                                                  
C     WRITE(6,*) 'CHECK OF COLATTITUDE',COCLT                                   
C                                                                               
      DO 120 J=1,JMAX                                                           
      PX=COCLT(J)+1.5                                                           
      LAT=PX                                                                    
      DLAT=PX-FLOAT(LAT)                                                        
      DO 120 I=1,IMAX                                                           
      QX=DELX*FLOAT(I-1)+1.5                                                    
      LON=QX                                                                    
      DLON=QX-FLOAT(LON)                                                        
         IP(1,I,J)=LON                                                          
         IP(2,I,J)=LAT                                                          
         DP(1,I,J)=(1.0-DLAT)*(1.0-DLON)                                        
         DP(2,I,J)=(1.0-DLAT)*DLON                                              
         DP(3,I,J)=DLAT*DLON                                                    
         DP(4,I,J)=DLAT*(1.0-DLON)                                              
  120 CONTINUE                                                                  
C     DO 1000 J=1,JMAX                                                          
C     WRITE(6,1010) J,IP(1,1,J),IP(2,1,J),(DP(I,1,J),I=1,4)                     
C1000 CONTINUE                                                                  
C     DO 1100 J=1,JMAX                                                          
C     WRITE(6,1010) J,IP(1,IMAX,J),IP(2,IMAX,J),(DP(I,IMAX,J),I=1,4)            
C1100 CONTINUE                                                                  
C     DO 1200 J=1,IMAX                                                          
C     WRITE(6,1020) J,IP(1,J,1),IP(2,J,1),(DP(I,J,1),I=1,4)                     
C1200 CONTINUE                                                                  
C     DO 1300 J=1,IMAX                                                          
C     WRITE(6,1020) J,IP(1,J,JMAX),IP(2,J,JMAX),(DP(I,J,JMAX),I=1,4)            
C1300 CONTINUE                                                                  
C1010 FORMAT(1H ,'J=',I3,5X,2I5,5X,4F10.3)                                      
C1020 FORMAT(1H ,'I=',I3,5X,2I5,5X,4F10.3)                                      
C                                                                               
      RETURN                                                                    
      END SUBROUTINE SETWHT                                                         
      SUBROUTINE INTERP                                                         
     1(WORK,DATA,IMAX,JMAX,DP,IP)                                               
C                                                                               
      DIMENSION WORK(362,182),DATA(IMAX,JMAX),DP(4,IMAX,JMAX)                   
      INTEGER*2 IP(2,IMAX,JMAX)                                                 
C                                                                               
      DO 100 J=1,JMAX                                                           
      DO 100 I=1,IMAX                                                           
       II=IP(1,I,J)                                                             
       JJ=IP(2,I,J)                                                             
       DATA(I,J)=DP(1,I,J)*WORK(II,JJ  )+DP(2,I,J)*WORK(II+1,JJ  )              
     *          +DP(4,I,J)*WORK(II,JJ+1)+DP(3,I,J)*WORK(II+1,JJ+1)              
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE INTERP                                                         
