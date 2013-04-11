!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mradbg.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MRADBG (M,IDO,IP,L1,IDL1,CC,C1,C2,IM1,IN1,             &
     &          CH,CH2,IM2,IN2,WA)                                      
      REAL      CH(IN2,IDO,L1,IP)    ,CC(IN1,IDO,IP,L1) ,               &
     &          C1(IN1,IDO,L1,IP)    ,C2(IN1,IDL1,IP),                  &
     &          CH2(IN2,IDL1,IP)     ,WA(IDO)                           
!                                                                       
      M1D = (M-1)*IM1+1 
      M2S = 1-IM2 
      TPI=2.*4.*ATAN(1.0) 
      ARG = TPI/FLOAT(IP) 
      DCP = COS(ARG) 
      DSP = SIN(ARG) 
      IDP2 = IDO+2 
      NBD = (IDO-1)/2 
      IPP2 = IP+2 
      IPPH = (IP+1)/2 
      IF (IDO .LT. L1) GO TO 103 
      DO 102 K=1,L1 
         DO 101 I=1,IDO 
            M2 = M2S 
            DO 1001 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,I,K,1) = CC(M1,I,1,K) 
 1001       CONTINUE 
  101    CONTINUE 
  102 END DO 
      GO TO 106 
  103 DO 105 I=1,IDO 
         DO 104 K=1,L1 
            M2 = M2S 
            DO 1004 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,I,K,1) = CC(M1,I,1,K) 
 1004       CONTINUE 
  104    CONTINUE 
  105 END DO 
  106 DO 108 J=2,IPPH 
         JC = IPP2-J 
         J2 = J+J 
         DO 107 K=1,L1 
            M2 = M2S 
            DO 1007 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,1,K,J) = CC(M1,IDO,J2-2,K)+CC(M1,IDO,J2-2,K) 
            CH(M2,1,K,JC) = CC(M1,1,J2-1,K)+CC(M1,1,J2-1,K) 
 1007       CONTINUE 
  107    CONTINUE 
  108 END DO 
      IF (IDO .EQ. 1) GO TO 116 
      IF (NBD .LT. L1) GO TO 112 
      DO 111 J=2,IPPH 
         JC = IPP2-J 
         DO 110 K=1,L1 
            DO 109 I=3,IDO,2 
               IC = IDP2-I 
               M2 = M2S 
               DO 1009 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = CC(M1,I-1,2*J-1,K)+CC(M1,IC-1,2*J-2,K) 
               CH(M2,I-1,K,JC) = CC(M1,I-1,2*J-1,K)-CC(M1,IC-1,2*J-2,K) 
               CH(M2,I,K,J) = CC(M1,I,2*J-1,K)-CC(M1,IC,2*J-2,K) 
               CH(M2,I,K,JC) = CC(M1,I,2*J-1,K)+CC(M1,IC,2*J-2,K) 
 1009          CONTINUE 
  109       CONTINUE 
  110    CONTINUE 
  111 END DO 
      GO TO 116 
  112 DO 115 J=2,IPPH 
         JC = IPP2-J 
         DO 114 I=3,IDO,2 
            IC = IDP2-I 
            DO 113 K=1,L1 
               M2 = M2S 
               DO 1013 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = CC(M1,I-1,2*J-1,K)+CC(M1,IC-1,2*J-2,K) 
               CH(M2,I-1,K,JC) = CC(M1,I-1,2*J-1,K)-CC(M1,IC-1,2*J-2,K) 
               CH(M2,I,K,J) = CC(M1,I,2*J-1,K)-CC(M1,IC,2*J-2,K) 
               CH(M2,I,K,JC) = CC(M1,I,2*J-1,K)+CC(M1,IC,2*J-2,K) 
 1013          CONTINUE 
  113       CONTINUE 
  114    CONTINUE 
  115 END DO 
  116 AR1 = 1. 
      AI1 = 0. 
      DO 120 L=2,IPPH 
         LC = IPP2-L 
         AR1H = DCP*AR1-DSP*AI1 
         AI1 = DCP*AI1+DSP*AR1 
         AR1 = AR1H 
         DO 117 IK=1,IDL1 
            M2 = M2S 
            DO 1017 M1=1,M1D,IM1 
            M2 = M2+IM2 
            C2(M1,IK,L) = CH2(M2,IK,1)+AR1*CH2(M2,IK,2) 
            C2(M1,IK,LC) = AI1*CH2(M2,IK,IP) 
 1017       CONTINUE 
  117    CONTINUE 
         DC2 = AR1 
         DS2 = AI1 
         AR2 = AR1 
         AI2 = AI1 
         DO 119 J=3,IPPH 
            JC = IPP2-J 
            AR2H = DC2*AR2-DS2*AI2 
            AI2 = DC2*AI2+DS2*AR2 
            AR2 = AR2H 
            DO 118 IK=1,IDL1 
               M2 = M2S 
               DO 1018 M1=1,M1D,IM1 
               M2 = M2+IM2 
               C2(M1,IK,L) = C2(M1,IK,L)+AR2*CH2(M2,IK,J) 
               C2(M1,IK,LC) = C2(M1,IK,LC)+AI2*CH2(M2,IK,JC) 
 1018          CONTINUE 
  118       CONTINUE 
  119    CONTINUE 
  120 END DO 
      DO 122 J=2,IPPH 
         DO 121 IK=1,IDL1 
            M2 = M2S 
            DO 1021 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH2(M2,IK,1) = CH2(M2,IK,1)+CH2(M2,IK,J) 
 1021       CONTINUE 
  121    CONTINUE 
  122 END DO 
      DO 124 J=2,IPPH 
         JC = IPP2-J 
         DO 123 K=1,L1 
            M2 = M2S 
            DO 1023 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,1,K,J) = C1(M1,1,K,J)-C1(M1,1,K,JC) 
            CH(M2,1,K,JC) = C1(M1,1,K,J)+C1(M1,1,K,JC) 
 1023       CONTINUE 
  123    CONTINUE 
  124 END DO 
      IF (IDO .EQ. 1) GO TO 132 
      IF (NBD .LT. L1) GO TO 128 
      DO 127 J=2,IPPH 
         JC = IPP2-J 
         DO 126 K=1,L1 
            DO 125 I=3,IDO,2 
               M2 = M2S 
               DO 1025 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = C1(M1,I-1,K,J)-C1(M1,I,K,JC) 
               CH(M2,I-1,K,JC) = C1(M1,I-1,K,J)+C1(M1,I,K,JC) 
               CH(M2,I,K,J) = C1(M1,I,K,J)+C1(M1,I-1,K,JC) 
               CH(M2,I,K,JC) = C1(M1,I,K,J)-C1(M1,I-1,K,JC) 
 1025          CONTINUE 
  125       CONTINUE 
  126    CONTINUE 
  127 END DO 
      GO TO 132 
  128 DO 131 J=2,IPPH 
         JC = IPP2-J 
         DO 130 I=3,IDO,2 
            DO 129 K=1,L1 
               M2 = M2S 
               DO 1029 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = C1(M1,I-1,K,J)-C1(M1,I,K,JC) 
               CH(M2,I-1,K,JC) = C1(M1,I-1,K,J)+C1(M1,I,K,JC) 
               CH(M2,I,K,J) = C1(M1,I,K,J)+C1(M1,I-1,K,JC) 
               CH(M2,I,K,JC) = C1(M1,I,K,J)-C1(M1,I-1,K,JC) 
 1029          CONTINUE 
  129       CONTINUE 
  130    CONTINUE 
  131 END DO 
  132 CONTINUE 
      IF (IDO .EQ. 1) RETURN 
      DO 133 IK=1,IDL1 
         M2 = M2S 
         DO 1033 M1=1,M1D,IM1 
         M2 = M2+IM2 
         C2(M1,IK,1) = CH2(M2,IK,1) 
 1033    CONTINUE 
  133 END DO 
      DO 135 J=2,IP 
         DO 134 K=1,L1 
            M2 = M2S 
            DO 1034 M1=1,M1D,IM1 
            M2 = M2+IM2 
            C1(M1,1,K,J) = CH(M2,1,K,J) 
 1034       CONTINUE 
  134    CONTINUE 
  135 END DO 
      IF (NBD .GT. L1) GO TO 139 
      IS = -IDO 
      DO 138 J=2,IP 
         IS = IS+IDO 
         IDIJ = IS 
         DO 137 I=3,IDO,2 
            IDIJ = IDIJ+2 
            DO 136 K=1,L1 
               M2 = M2S 
               DO 1036 M1=1,M1D,IM1 
               M2 = M2+IM2 
               C1(M1,I-1,K,J) = WA(IDIJ-1)*CH(M2,I-1,K,J)-WA(IDIJ)*     &
     &          CH(M2,I,K,J)                                            
               C1(M1,I,K,J) = WA(IDIJ-1)*CH(M2,I,K,J)+WA(IDIJ)*         &
     &          CH(M2,I-1,K,J)                                          
 1036          CONTINUE 
  136       CONTINUE 
  137    CONTINUE 
  138 END DO 
      GO TO 143 
  139 IS = -IDO 
      DO 142 J=2,IP 
         IS = IS+IDO 
         DO 141 K=1,L1 
            IDIJ = IS 
            DO 140 I=3,IDO,2 
               IDIJ = IDIJ+2 
               M2 = M2S 
               DO 1040 M1=1,M1D,IM1 
               M2 = M2+IM2 
               C1(M1,I-1,K,J) = WA(IDIJ-1)*CH(M2,I-1,K,J)-WA(IDIJ)*     &
     &          CH(M2,I,K,J)                                            
               C1(M1,I,K,J) = WA(IDIJ-1)*CH(M2,I,K,J)+WA(IDIJ)*         &
     &          CH(M2,I-1,K,J)                                          
 1040          CONTINUE 
  140       CONTINUE 
  141    CONTINUE 
  142 END DO 
  143 RETURN 
      END                                           
