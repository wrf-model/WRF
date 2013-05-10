      SUBROUTINE CR8I2V(RDATA,BASE,AMP,IDATA,LM)                                
************************************************************************        
*     ( IN ) RDATA   R*8(LM)     : 実数配列                                 
*     ( OUT) BASE    R*4         : 基準値                                   
*     ( OUT) AMP     R*4         : 係数                                     
*     ( OUT) IDATA   I*2(LM)     : 整数配列                                 
*     ( IN ) LM      I*4         : データ数                                 
*                                                                               
*                                                                               
************************************************************************        
*                                                                               
      REAL*8    RDATA(LM),RMAX,RMIN,DIST,DBASE,EXPV                             
      INTEGER*4 IDATA(*)     !shc-rizvi
c     INTEGER*2 IDATA(*)     !shc-rizvi
      REAL*8    DVAL/32760.D0/                                                  
      INTEGER*2 HZERO/0/,HWORK                                                  
*                                                                               
*   GET BASE,AMP                                                                
*                                                                               
      RMAX=RDATA(1)                                                             
      RMIN=RDATA(1)                                                             
      DO I=2,LM                                                              
         IF (RDATA(I).GT.RMAX) RMAX=RDATA(I)                                    
         IF (RDATA(I).LT.RMIN) RMIN=RDATA(I)                                    
      END DO                                                                  
      DIST=(RMAX-RMIN)/2                                                        
      DBASE=(RMIN+RMAX)/2                                                       
      BASE=DBASE                                                                
      AMP=DIST/DVAL                                                             
*                                                                               
*    PACK                                                                       
*                                                                               
      IF (DIST.EQ.0) THEN 
         DO 20 I=1,LM/2                                                         
            IDATA(I)=0                                                          
   20    CONTINUE                                                               
         IF ((LM/2)*2.NE.LM) THEN                    ! LM ODD                   
Crizvi            CALL MOVEC(IDATA(LM/2+1),1,HZERO,1,2)                               
         IDATA(LM/2+1) = HZERO * 65536
         END IF                                                                 
      ELSE                             ! NORMAL DATA                            
         EXPV=DVAL/DIST                                                         
*cdir nodep                                                                     
         DO 30 I=2,LM,2                                                         
            IWORK=NINT((RDATA(I)-DBASE)*EXPV)        ! EVEN PART                
            IF (IWORK.GE.0) THEN                                                
               IDATA(I/2)=NINT((RDATA(I-1)-DBASE)*EXPV)*65536+IWORK             
            ELSE                                                                
               IDATA(I/2)=(NINT((RDATA(I-1)-DBASE)*EXPV)+1)*65536+IWORK         
            END IF                                                              
   30    CONTINUE                                                               
         IF ((LM/2)*2.NE.LM) THEN                    ! LM ODD                   
            HWORK=NINT((RDATA(LM)-DBASE)*EXPV)                                  
Crizvi            CALL MOVEC(IDATA(LM/2+1),1,HWORK,1,2)                               
         IDATA(LM/2+1) = HWORK * 65536
         END IF                                                                 
      END IF                                                                    
*                                                                               
      RETURN                                                                    
      END SUBROUTINE CR8I2V
