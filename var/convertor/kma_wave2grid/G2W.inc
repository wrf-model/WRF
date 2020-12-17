      SUBROUTINE G2W                                                            
     I(MEND1,NEND1 ,JEND1,MNWAV,IMAX,JMAX  ,IMX,JMAXHF,KMAX ,                   
     I PNM  ,GDAT ,IFAX  ,TRIGS,GW  ,                                           
     O QDAT ,                                                                   
     W GWRK)                                                                    
C                                                                               
      DIMENSION QDAT(2,KMAX,MNWAV)  ,PNM(MNWAV,JMAXHF)                          
      DIMENSION GDAT(IMAX,JMAX,KMAX),GWRK(IMX,JMAX,KMAX)                       
      DIMENSION IFAX(10),TRIGS(IMAX),GW(JMAX)                                   
C                                                                               
      LOT =JMAX*KMAX                                                            
       print*,' rizvi in G2W input Gauss grid data '
       do j=100,JMAX,50
       print*,j,' Lat (Gauss) grid val ',(GDAT(i,j,KMAX),i=200,210)
       end do
      print*,' in G2W calling FFT991 with LOT= ',LOT
      CALL FFT991(GDAT,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
C     CALL FFT991(GDAT,GWRK,TRIGS,IFAX,1,IMX,IMAX,LOT,-1)                       
C                                                                               
      CALL RESET(QDAT,2*KMAX*MNWAV)                                             
C                                                                               
      CALL WEIHT2(IMAX,MEND1,JMAX/2,JMAX,KMAX,   0,   0,2*KMAX,                 
     1             GW,GDAT,GWRK)                                           

      CALL G2WPP (MEND1,NEND1,JEND1,1,2*KMAX,2*KMAX,MNWAV,                      
     1             JMAX/2,JMAX,PNM,QDAT,GWRK)                                   
C                                                                               
      RETURN                                                                    
      END SUBROUTINE G2W

