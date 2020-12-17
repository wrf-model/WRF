      SUBROUTINE REDDAT_BIN                                                         
     I(NFL   , IMAXE , JMAXE , KMAX  , PSE    ,                                               
     O GTE   , GUE   , GVE   , GQE   )                          
C                                                                               
      REAL*8 PSE(IMAXE,JMAXE)
      REAL*8 GUE(IMAXE,JMAXE,KMAX),GVE(IMAXE,JMAXE,KMAX),
     &     GTE(IMAXE,JMAXE,KMAX),GQE(IMAXE,JMAXE,KMAX)                                                
      REAL*8 DUM1(IMAXE,JMAXE),DUM2(IMAXE,JMAXE)
      REAL*8 DUM3(IMAXE,JMAXE,KMAX),DUM4(IMAXE,JMAXE,KMAX),
     &     DUM5(IMAXE,JMAXE,KMAX),DUM6(IMAXE,JMAXE,KMAX)                                                
      
C                                                                               
          READ(NFL) DUM1
          READ(NFL) DUM2
          PSE = DUM2

          READ(NFL) DUM3
          GTE= DUM3

          READ(NFL) DUM4
          GUE= DUM4

          READ(NFL) DUM5
          GVE= DUM5
       
          READ(NFL) DUM6
          GQE= DUM6

      RETURN                                                                    
C                                                                               
      END SUBROUTINE REDDAT_BIN                                                         
