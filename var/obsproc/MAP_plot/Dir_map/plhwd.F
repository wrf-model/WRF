      SUBROUTINE PLHWD(IX,JX,WSP,WDR,Ylon,NS,SPVAL,N)                    
C                                                                               
C   WSP, WDR   ----- the wind speed and direction                               
C   XCS, YCS --- the horizontal coordinates.                                    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc       
C                                                                               
#     include    <maps.incl>                                            
      DIMENSION  WSP(NS), WDR(NS), Ylon(ns)                              
c                                                                               
      CALL GFLAS3(N)                                                            
C                                                                               
      call set(xsa,xsb,ysa,ysb,1.,float(JX),1.,float(IX),ltype)                 
c                                                                               
      number = 0
      DO 10 J = 1,NS                            

        if (xcs(j).lt.1.0 .or. xcs(j).gt.float(jx) .or.                         
     *      ycs(j).lt.1.0 .or. ycs(j).gt.float(ix)) go to 10                    
        IF (WSP(J).EQ.SPVAL) GO TO 10                                           
        Uu = 0.
        Vv = 0.
        call  FFDDUV(wsp(j),wdr(j),Uu,Vv,YLON(j),XLONC,PHIC,IPROJ,0,1)
c        PRINT '("J,wsp,wdr,Uu,Vv,YLON,XLONC,PHIC,IPROJ:")'
c        PRINT '(I2,7F10.2,2X,A)',
c     *           J,wsp(j),wdr(j),Uu,Vv,YLON(j),XLONC,PHIC,IPROJ
        IF (WSP(J).EQ.SPVAL .OR.WSP(J).GT.120.  ) GO TO 10                      
        number = number+1                                                       
c .. m/s --> knots:                                                             
            UU = Uu*1.94
            VV = Vv*1.94
        CALL WNDBARB(XCS(J),YCS(J),UU,VV)                                       
10    CONTINUE                                                                  
      print *,number,' OBS of WIND within the domain'                           
C
      call frame
                                                                               
      RETURN                                                                    
      END                                                                       
