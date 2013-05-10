!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                       
!   FFTPACK 5.0                                                         
!                                                                       
!   Authors:  Paul N. Swarztrauber and Richard A. Valent                
!                                                                       
!   $Id: mradfg.f,v 1.2 2004/06/15 21:29:20 rodney Exp $                
!                                                                       
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                        
      SUBROUTINE MRADFG (M,IDO,IP,L1,IDL1,CC,C1,C2,IM1,IN1,             &
     &              CH,CH2,IM2,IN2,WA)                                  
      REAL          CH(IN2,IDO,L1,IP)   ,CC(IN1,IDO,IP,L1),             &
     &              C1(IN1,IDO,L1,IP)   ,C2(IN1,IDL1,IP),               &
     &              CH2(IN2,IDL1,IP)    ,WA(IDO)                        
!                                                                       
      M1D = (M-1)*IM1+1 
      M2S = 1-IM2 
      TPI=2.*4.*ATAN(1.0) 
      ARG = TPI/FLOAT(IP) 
      DCP = COS(ARG) 
      DSP = SIN(ARG) 
      IPPH = (IP+1)/2 
      IPP2 = IP+2 
      IDP2 = IDO+2 
      NBD = (IDO-1)/2 
      IF (IDO .EQ. 1) GO TO 119 
      DO 101 IK=1,IDL1 
         M2 = M2S 
         DO 1001 M1=1,M1D,IM1 
         M2 = M2+IM2 
         CH2(M2,IK,1) = C2(M1,IK,1) 
 1001    CONTINUE 
  101 END DO 
      DO 103 J=2,IP 
         DO 102 K=1,L1 
            M2 = M2S 
            DO 1002 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH(M2,1,K,J) = C1(M1,1,K,J) 
 1002       CONTINUE 
  102    CONTINUE 
  103 END DO 
      IF (NBD .GT. L1) GO TO 107 
      IS = -IDO 
      DO 106 J=2,IP 
         IS = IS+IDO 
         IDIJ = IS 
         DO 105 I=3,IDO,2 
            IDIJ = IDIJ+2 
            DO 104 K=1,L1 
               M2 = M2S 
               DO 1004 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = WA(IDIJ-1)*C1(M1,I-1,K,J)+WA(IDIJ)      &
     &           *C1(M1,I,K,J)                                          
               CH(M2,I,K,J) = WA(IDIJ-1)*C1(M1,I,K,J)-WA(IDIJ)          &
     &           *C1(M1,I-1,K,J)                                        
 1004          CONTINUE 
  104       CONTINUE 
  105    CONTINUE 
  106 END DO 
      GO TO 111 
  107 IS = -IDO 
      DO 110 J=2,IP 
         IS = IS+IDO 
         DO 109 K=1,L1 
            IDIJ = IS 
            DO 108 I=3,IDO,2 
               IDIJ = IDIJ+2 
               M2 = M2S 
               DO 1008 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH(M2,I-1,K,J) = WA(IDIJ-1)*C1(M1,I-1,K,J)+WA(IDIJ)      &
     &           *C1(M1,I,K,J)                                          
               CH(M2,I,K,J) = WA(IDIJ-1)*C1(M1,I,K,J)-WA(IDIJ)          &
     &           *C1(M1,I-1,K,J)                                        
 1008          CONTINUE 
  108       CONTINUE 
  109    CONTINUE 
  110 END DO 
  111 IF (NBD .LT. L1) GO TO 115 
      DO 114 J=2,IPPH 
         JC = IPP2-J 
         DO 113 K=1,L1 
            DO 112 I=3,IDO,2 
               M2 = M2S 
               DO 1012 M1=1,M1D,IM1 
               M2 = M2+IM2 
               C1(M1,I-1,K,J) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC) 
               C1(M1,I-1,K,JC) = CH(M2,I,K,J)-CH(M2,I,K,JC) 
               C1(M1,I,K,J) = CH(M2,I,K,J)+CH(M2,I,K,JC) 
               C1(M1,I,K,JC) = CH(M2,I-1,K,JC)-CH(M2,I-1,K,J) 
 1012          CONTINUE 
  112       CONTINUE 
  113    CONTINUE 
  114 END DO 
      GO TO 121 
  115 DO 118 J=2,IPPH 
         JC = IPP2-J 
         DO 117 I=3,IDO,2 
            DO 116 K=1,L1 
               M2 = M2S 
               DO 1016 M1=1,M1D,IM1 
               M2 = M2+IM2 
               C1(M1,I-1,K,J) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC) 
               C1(M1,I-1,K,JC) = CH(M2,I,K,J)-CH(M2,I,K,JC) 
               C1(M1,I,K,J) = CH(M2,I,K,J)+CH(M2,I,K,JC) 
               C1(M1,I,K,JC) = CH(M2,I-1,K,JC)-CH(M2,I-1,K,J) 
 1016          CONTINUE 
  116       CONTINUE 
  117    CONTINUE 
  118 END DO 
      GO TO 121 
  119 DO 120 IK=1,IDL1 
         M2 = M2S 
         DO 1020 M1=1,M1D,IM1 
         M2 = M2+IM2 
         C2(M1,IK,1) = CH2(M2,IK,1) 
 1020    CONTINUE 
  120 END DO 
  121 DO 123 J=2,IPPH 
         JC = IPP2-J 
         DO 122 K=1,L1 
            M2 = M2S 
            DO 1022 M1=1,M1D,IM1 
            M2 = M2+IM2 
            C1(M1,1,K,J) = CH(M2,1,K,J)+CH(M2,1,K,JC) 
            C1(M1,1,K,JC) = CH(M2,1,K,JC)-CH(M2,1,K,J) 
 1022       CONTINUE 
  122    CONTINUE 
  123 END DO 
!                                                                       
      AR1 = 1. 
      AI1 = 0. 
      DO 127 L=2,IPPH 
         LC = IPP2-L 
         AR1H = DCP*AR1-DSP*AI1 
         AI1 = DCP*AI1+DSP*AR1 
         AR1 = AR1H 
         DO 124 IK=1,IDL1 
            M2 = M2S 
            DO 1024 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH2(M2,IK,L) = C2(M1,IK,1)+AR1*C2(M1,IK,2) 
            CH2(M2,IK,LC) = AI1*C2(M1,IK,IP) 
 1024       CONTINUE 
  124    CONTINUE 
         DC2 = AR1 
         DS2 = AI1 
         AR2 = AR1 
         AI2 = AI1 
         DO 126 J=3,IPPH 
            JC = IPP2-J 
            AR2H = DC2*AR2-DS2*AI2 
            AI2 = DC2*AI2+DS2*AR2 
            AR2 = AR2H 
            DO 125 IK=1,IDL1 
               M2 = M2S 
               DO 1025 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CH2(M2,IK,L) = CH2(M2,IK,L)+AR2*C2(M1,IK,J) 
               CH2(M2,IK,LC) = CH2(M2,IK,LC)+AI2*C2(M1,IK,JC) 
 1025          CONTINUE 
  125       CONTINUE 
  126    CONTINUE 
  127 END DO 
      DO 129 J=2,IPPH 
         DO 128 IK=1,IDL1 
            M2 = M2S 
            DO 1028 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CH2(M2,IK,1) = CH2(M2,IK,1)+C2(M1,IK,J) 
 1028       CONTINUE 
  128    CONTINUE 
  129 END DO 
!                                                                       
      IF (IDO .LT. L1) GO TO 132 
      DO 131 K=1,L1 
         DO 130 I=1,IDO 
            M2 = M2S 
            DO 1030 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CC(M1,I,1,K) = CH(M2,I,K,1) 
 1030       CONTINUE 
  130    CONTINUE 
  131 END DO 
      GO TO 135 
  132 DO 134 I=1,IDO 
         DO 133 K=1,L1 
            M2 = M2S 
            DO 1033 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CC(M1,I,1,K) = CH(M2,I,K,1) 
 1033       CONTINUE 
  133    CONTINUE 
  134 END DO 
  135 DO 137 J=2,IPPH 
         JC = IPP2-J 
         J2 = J+J 
         DO 136 K=1,L1 
            M2 = M2S 
            DO 1036 M1=1,M1D,IM1 
            M2 = M2+IM2 
            CC(M1,IDO,J2-2,K) = CH(M2,1,K,J) 
            CC(M1,1,J2-1,K) = CH(M2,1,K,JC) 
 1036       CONTINUE 
  136    CONTINUE 
  137 END DO 
      IF (IDO .EQ. 1) RETURN 
      IF (NBD .LT. L1) GO TO 141 
      DO 140 J=2,IPPH 
         JC = IPP2-J 
         J2 = J+J 
         DO 139 K=1,L1 
            DO 138 I=3,IDO,2 
               IC = IDP2-I 
               M2 = M2S 
               DO 1038 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CC(M1,I-1,J2-1,K) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC) 
               CC(M1,IC-1,J2-2,K) = CH(M2,I-1,K,J)-CH(M2,I-1,K,JC) 
               CC(M1,I,J2-1,K) = CH(M2,I,K,J)+CH(M2,I,K,JC) 
               CC(M1,IC,J2-2,K) = CH(M2,I,K,JC)-CH(M2,I,K,J) 
 1038          CONTINUE 
  138       CONTINUE 
  139    CONTINUE 
  140 END DO 
      RETURN 
  141 DO 144 J=2,IPPH 
         JC = IPP2-J 
         J2 = J+J 
         DO 143 I=3,IDO,2 
            IC = IDP2-I 
            DO 142 K=1,L1 
               M2 = M2S 
               DO 1042 M1=1,M1D,IM1 
               M2 = M2+IM2 
               CC(M1,I-1,J2-1,K) = CH(M2,I-1,K,J)+CH(M2,I-1,K,JC) 
               CC(M1,IC-1,J2-2,K) = CH(M2,I-1,K,J)-CH(M2,I-1,K,JC) 
               CC(M1,I,J2-1,K) = CH(M2,I,K,J)+CH(M2,I,K,JC) 
               CC(M1,IC,J2-2,K) = CH(M2,I,K,JC)-CH(M2,I,K,J) 
 1042          CONTINUE 
  142       CONTINUE 
  143    CONTINUE 
  144 END DO 
      RETURN 
      END                                           
