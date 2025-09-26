C  User-defined Rate Law functions
C  Note: the default argument type for rate laws, as read from the equations file, is single precision
C        but all the internal calculations are performed in REAL*8

C Arrhenius
      KPP_REAL FUNCTION ARR( A0,B0,C0 )
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
                 
      REAL A0,B0,C0      
      ARR =  DBLE(A0) * EXP(-DBLE(B0)/TEMP) * (TEMP/300.0D0)**DBLE(C0)   
           
      RETURN
      END           


C Simplified Arrhenius, with two arguments
C Note: The argument B0 has a changed sign when compared to ARR
      KPP_REAL FUNCTION ARR2( A0,B0 )
      INCLUDE 'KPP_ROOT_Parameters.h'
      INCLUDE 'KPP_ROOT_Global.h'
                 
      REAL A0,B0     
      ARR2 =  DBLE(A0) * EXP( DBLE(B0)/TEMP )   
           
      RETURN
      END           

      KPP_REAL FUNCTION EP2(A0,C0,A2,C2,A3,C3)
      INCLUDE 'KPP_ROOT_Parameters.h'                       
      INCLUDE 'KPP_ROOT_Global.h'
      
      REAL A0,C0,A2,C2,A3,C3
      REAL*8 K0,K2,K3
      
      K0 = DBLE(A0) * EXP(-DBLE(C0)/TEMP)
      K2 = DBLE(A2) * EXP(-DBLE(C2)/TEMP)
      K3 = DBLE(A3) * EXP(-DBLE(C3)/TEMP)
      K3 = K3*CFACTOR*1.0d6
      EP2 = K0 + K3/(1.0d0+K3/K2 )
        
      RETURN
      END  


      KPP_REAL FUNCTION EP3(A1,C1,A2,C2) 
      INCLUDE 'KPP_ROOT_Parameters.h'               
      INCLUDE 'KPP_ROOT_Global.h'
      
      REAL A1, C1, A2, C2
      REAL*8 K1, K2
      
      K1 = DBLE(A1) * EXP(-DBLE(C1)/TEMP)
      K2 = DBLE(A2) * EXP(-DBLE(C2)/TEMP)
      EP3 = K1 + K2*(1.0d6*CFACTOR)
      
      RETURN
      END    


      KPP_REAL FUNCTION FALL ( A0,B0,C0,A1,B1,C1,CF)
      INCLUDE 'KPP_ROOT_Parameters.h'                      
      INCLUDE 'KPP_ROOT_Global.h'
      
      REAL A0,B0,C0,A1,B1,C1,CF
      REAL*8 K0, K1
      
      K0 = DBLE(A0) * EXP(-DBLE(B0)/TEMP)* (TEMP/300.0D0)**DBLE(C0)
      K1 = DBLE(A1) * EXP(-DBLE(B1)/TEMP)* (TEMP/300.0D0)**DBLE(C1)
      K0 = K0*CFACTOR*1.0D6
      K1 = K0/K1
      FALL = (K0/(1.0d0+K1))*
     *      DBLE(CF)**(1.0d0/(1.0d0+(DLOG10(K1))**2))
        
      RETURN
      END
