      SUBROUTINE PACK (Z,IDA,STAND,AMP,NGRID)                                   
C                                                                               
      REAL*8    Z(NGRID), DMAX, DMIN                                            
Crizvi      INTEGER*2 IDA(NGRID)                                                      
      INTEGER IDA(NGRID)                                                      
                                                                                
      CMAX=32767.0                                                              
                                                                                
*    ******* SCALING PART *******                                               
                                                                                
Crizvi      DMAX = -1.0E75                                                            
Crizvi      DMIN =  1.0E75                                                            
      DMAX = -1.0E38                                                            
      DMIN =  1.0E38                                                            
      DO 20 I=1,NGRID                                                           
       DMAX = MAX(DMAX,Z(I))                                                    
       DMIN = MIN(DMIN,Z(I))                                                    
   20 CONTINUE                                                                  
      STAND=(DMAX+DMIN)*0.5                                                     
      AMP  =(DMAX-STAND)/CMAX                                                   
                                                                                
*      ***** PACKING PART  *******                                              
                                                                                
      IF(AMP.EQ.0.0) THEN                                                       
        RAMP=1.0                                                                
      ELSE                                                                      
        RAMP=1.0/AMP                                                            
      END IF                                                                    
C                                                                               
      DO 40 I=1,NGRID                                                           
       WORK=(Z(I)-STAND)*RAMP                                                   
       IF(WORK.GT.0.0) THEN                                                     
         IDA(I)=INT(WORK+0.5)                                                   
       ELSE                                                                     
         IDA(I)=INT(WORK-0.5)                                                   
       END IF                                                                   
   40 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE PACK

C*********************************************************************          
      SUBROUTINE CR4I2V(RDATA,BASE,AMP,IDATA,LM)                                
************************************************************************        
*     ( IN ) RDATA   R*4(LM)     : 実数配列                                 
*     ( OUT) BASE    R*4         : 基準値                                   
*     ( OUT) AMP     R*4         : 係数                                     
*     ( OUT) IDATA   I*2(LM)     : 整数配列                                 
*     ( IN ) LM      I*4         : データ数                                 
*                                                                               
************************************************************************        
*                                                                               
Crizvi      REAL*4    RDATA(LM),RMAX,RMIN,DIST,DBASE,EXPV                             
      REAL    RDATA(LM),RMAX,RMIN,DIST,DBASE,EXPV                             
      INTEGER*4 IDATA(*)                                                        
Crizvi      REAL*4    DVAL/32760.E0/                                                  
      REAL    DVAL/32760.E0/                                                  
Crizvi      INTEGER*2 HZERO/0/,HWORK                                                  
      INTEGER HZERO/0/,HWORK                                                  
*                                                                               
*   GET BASE,AMP                                                                
*                                                                               
      RMAX=RDATA(1)                                                             
      RMIN=RDATA(1)                                                             
      DO 10 I=2,LM                                                              
         IF (RDATA(I).GT.RMAX) RMAX=RDATA(I)                                    
         IF (RDATA(I).LT.RMIN) RMIN=RDATA(I)                                    
   10 CONTINUE                                                                  
      DIST=(RMAX-RMIN)/2                                                        
      DBASE=(RMIN+RMAX)/2                                                       
      BASE=DBASE                                                                
      AMP=DIST/DVAL                                                             
*                                                                               
*    PACK                                                                       
*                                                                               
      IF (DIST.EQ.0) THEN              ! ALL SAME                               
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
      END SUBROUTINE CR4I2V

C*********************************************************************          
C   >>>   データをアンパックする（ベクトル版）                  <<<         
C*********************************************************************          
      SUBROUTINE CI2R4V(RDATA,BASE,AMP,IDATA,LM)                                
************************************************************************        
*                                                                               
*   数値予報課流２バイト整数型データ配列を４バイト実数に変換する。          
*   （Ｓ３８００ベクトル処理用）                                            
*                                      1995.11.06   中野尚                  
*    引数                                                                   
*     ( OUT) RDATA   R*4(LM)     : 実数配列                                 
*     ( IN ) BASE    R*4         : 基準値                                   
*     ( IN ) AMP     R*4         : 係数                                     
*     ( IN ) IDATA   I*2(LM)     : 整数配列                                 
*     ( IN ) LM      I*4         : データ数                                 
*                                                                               
*     必要なサブルーチン 無し                                               
*     注意：ＩＤＡＴＡは語境界にあること。                                  
*                                                                               
************************************************************************        
*                                                                               
Crizvi      REAL*4    RDATA(LM)                                                       
      REAL    RDATA(LM)                                                       
cshc-rizvi start
      INTEGER*4 IDATA(*)                                                        
c     INTEGER*2 IDATA(*)                                                        
cshc-rizvi end
      INTEGER*2 HWORK                                                           
C      INTEGER HWORK                                                           
*                                                                               
      DO 10 I=2,LM,2                                                            
         IWRK=IDATA(I/2)/65536                                                  
         IRMN=IDATA(I/2)-IWRK*65536                                             
         IF (IRMN.EQ.0) THEN                     ! LOWER-HALF=0                 
            RDATA(I-1)=BASE+AMP*IWRK                                            
            RDATA(I)  =BASE                                                     
         ELSE IF (IRMN.GT.0) THEN                                               
            RDATA(I-1)=BASE+AMP*IWRK                                            
            IF (IRMN.LT.32768) THEN                                             
               RDATA(I)=BASE+AMP*IRMN                                           
            ELSE                                                                
               RDATA(I)=BASE+AMP*(IRMN-65536)                                   
            END IF                                                              
         ELSE                                                                   
            RDATA(I-1)=BASE+AMP*(IWRK-1)                                        
            IF (IRMN.LT.-32768) THEN                                            
               RDATA(I)=BASE+AMP*(IRMN+65536)                                   
            ELSE                                                                
               RDATA(I)=BASE+AMP*IRMN                                           
            END IF                                                              
         END IF                                                                 
   10 CONTINUE                                                                  
      IF ((LM/2)*2.NE.LM) THEN                    ! LM ODD                      
Crizvi         CALL MOVEC(HWORK,1,IDATA(LM/2+1),1,2)  
         HWORK=IDATA(LM/2+1)/65536
         RDATA(LM)=BASE+AMP*HWORK                                               
      END IF                                                                    
*                                                                               
      RETURN                                                                    
      END SUBROUTINE CI2R4V

C*********************************************************************          
C   >>>   データをアンパックする（ベクトル版）                  <<<         
C*********************************************************************          
      SUBROUTINE CI2R8V(RDATA,BASE,AMP,IDATA,LM)                                
************************************************************************        
*                                                                               
*   数値予報課流２バイト整数型データ配列を８バイト実数に変換する。          
*   （Ｓ３８００ベクトル処理用）                                            
*                                      1995.11.07   中野尚                  
*    引数                                                                   
*     ( OUT) RDATA   R*8(LM)     : 実数配列                                 
*     ( IN ) BASE    R*4         : 基準値                                   
*     ( IN ) AMP     R*4         : 係数                                     
*     ( IN ) IDATA   I*2(LM)     : 整数配列                                 
*     ( IN ) LM      I*4         : データ数                                 
*                                                                               
*     必要なサブルーチン 無し                                               
*     注意：ＲＤＡＴＡは２語境界、ＩＤＡＴＡは語境界にあること。            
*                                                                               
************************************************************************        
*                                                                               
      REAL*8    RDATA(LM)                                                       
      INTEGER*4 IDATA(*)                                                        
      INTEGER*2 HWORK                                                           
c     INTEGER HWORK                                                           
*                                                                               
      DO 10 I=2,LM,2                                                            
         IWRK=IDATA(I/2)/65536                                                  
         IRMN=IDATA(I/2)-IWRK*65536                                             
         IF (IRMN.EQ.0) THEN                     ! LOWER-HALF=0                 
            RDATA(I-1)=BASE+AMP*IWRK                                            
            RDATA(I)  =BASE                                                     
         ELSE IF (IRMN.GT.0) THEN                                               
            RDATA(I-1)=BASE+AMP*IWRK                                            
            IF (IRMN.LT.32768) THEN                                             
               RDATA(I)=BASE+AMP*IRMN                                           
            ELSE                                                                
               RDATA(I)=BASE+AMP*(IRMN-65536)                                   
            END IF                                                              
         ELSE                                                                   
            RDATA(I-1)=BASE+AMP*(IWRK-1)                                        
            IF (IRMN.LT.-32768) THEN                                            
               RDATA(I)=BASE+AMP*(IRMN+65536)                                   
            ELSE                                                                
               RDATA(I)=BASE+AMP*IRMN                                           
            END IF                                                              
         END IF                                                                 
   10 CONTINUE                                                                  
      IF ((LM/2)*2.NE.LM) THEN                    ! LM ODD                      
Crizvi         CALL MOVEC(HWORK,1,IDATA(LM/2+1),1,2)  
         HWORK=IDATA(LM/2+1)/65536
         RDATA(LM)=BASE+AMP*HWORK                                               
      END IF                                                                    
*                                                                               
      RETURN                                                                    
      END SUBROUTINE CI2R8V
*                                                                               
C*********************************************************************          
      SUBROUTINE MOVERD(DATIN,DATOUT,N)                                         
      DIMENSION DATIN(N)                                                        
      REAL*8    DATOUT(N)                                                       
C                                                                               
      DO 100 I=1,N                                                              
      DATOUT(I)=DATIN(I)                                                        
  100 CONTINUE                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE MOVERD

C*********************************************************************          
      SUBROUTINE GETTYP(NFL,IOTYP)                                              
      CHARACTER*4 GVSD                                                          
      REWIND NFL                                                                
C                                                                               
      READ(NFL,'(2A4)',ERR=10) GVSD, GVSD                                       
   10 REWIND NFL                                                                
      IF( GVSD.EQ.'GVD1' ) THEN                                                 
        IOTYP=1                                                                 
        RETURN                                                                  
      ENDIF                                                                     
C                                                                               
      READ(NFL,ERR=20) GVSD                                                     
   20 REWIND NFL                                                                
      IF( GVSD.EQ.'GVS1' ) THEN                                                 
        IOTYP=3                                                                 
        RETURN                                                                  
      ENDIF                                                                     
C                                                                               
      IOTYP=-1                                                                  
C                                                                               
      RETURN                                                                    
      END SUBROUTINE GETTYP

