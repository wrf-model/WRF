      SUBROUTINE PLHMP(L,IX,JX,IOFF,JOFF,ICS,INDEX,FINC,NDOT,LW,SPVAL,                   
     1                 NS,YLAT,YLON,ASTA,NAME,NUMBER,N,FF,A2)                        
c
#     include    <maps.incl>
#     include    <nestdmn.incl>                                            
C                                                                               
      DIMENSION   FF(JX,IX), YLAT(ns), YLON(ns), asta(ns), 
     >            a2(ix+2*ioff,jx+2*joff)
C              
      real  levels(100)                   ! levels for CONDRV                   
      character*80 title, TM              ! title string                        
      integer errsev                                                            
      character ssta*5                                                          
      character *(*) NAME                                                
      do ii = 1,3000
        xcs(ii) = -999.9
        ycs(ii) = -999.9
      end do
c                                                                               
      DX = DIS(N)
      IR = NRATIO(N)                                                             PLTTER.67
      XIR = FLOAT(IR)                                                            PLTTER.68
      JEND = (NESTJX(1)-1)*IR + 1                                                PLTTER.69
      IEND = (NESTIX(1)-1)*IR + 1                                                PLTTER.70
      ISTART = 1                                                                 PLTTER.71
      JSTART = 1                                                                 PLTTER.72
        JEX = 0                                                                  PLTTER.73
        IEX = 0                                                                  PLTTER.74
      IF (N.EQ.1) THEN                                                           PLTTER.75
        JEX = JOFFST                                                             PLTTER.76
        IEX = IOFFST                                                             PLTTER.77
      ENDIF                                                                      PLTTER.78
      IF (N.EQ.1) THEN                                                           PLTTER.79
      XA = 1.0                                                                   PLTTER.80
      YA = 1.0                                                                   PLTTER.81
      XB = FLOAT(JEND)                                                           PLTTER.82
      YB = FLOAT(IEND)                                                           PLTTER.83
      ELSE                                                                       PLTTER.84
      XA = (XWEST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.                              PLTTER.85
      XB = (XEAST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.                              PLTTER.86
      YA = (XSOUTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.                              PLTTER.87
      YB = (XNORTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.                              PLTTER.88
      ENDIF                                                                      PLTTER.89
C                                                                                PLTTER.90
C  AVOID THE XA, XB, YA, YB ARE OUTSIDE THE BIG DOMAIN:                          PLTTER.91
      IF (XA.LT.1.0) THEN                                                        PLTTER.92
          XA = 1.0                                                               PLTTER.93
          PRINT *,'XA < 1.0, XA=1.0'                                             PLTTER.94
          STOP                                                                   PLTTER.95
      ENDIF                                                                      PLTTER.96
      IF (XB.GT.FLOAT(JEND)) THEN                                                PLTTER.97
          XB = FLOAT(JEND)                                                       PLTTER.98
          PRINT *,'XB > JEND, XB = FLOAT(JEND)'                                  PLTTER.99
          STOP                                                                   PLTTER.100
      ENDIF                                                                      PLTTER.101
      IF (YA.LT.1.0) THEN                                                        PLTTER.102
          YA = 1.0                                                               PLTTER.103
          PRINT *,'YA < 1.0, YA = 1.0'                                           PLTTER.104
          STOP                                                                   PLTTER.105
      ENDIF                                                                      PLTTER.106
      IF (YB.GT.FLOAT(IEND)) THEN                                                PLTTER.107
          YB = FLOAT(IEND)
          PRINT *,'YB > IEND, YB = FLOAT(IEND)'                                  PLTTER.109
          STOP                                                                   PLTTER.110
      ENDIF                                                                      PLTTER.111
C                                                                                PLTTER.112
      PRINT 2001, N,ISTART,IEND,JSTART,JEND,XA,XB,YA,YB                          PLTTER.114
2001  FORMAT(/2X,'--- CALL PLHMP ..........',I3                                  PLTTER.115
     1    /2X,'ISTART,IEND,JSTART,JEND:',4I6,3X,'XA,XB,YA,YB:',4F8.1)            PLTTER.116
                                                                                 PLTTER.118
      WRITE(TITLE(1:9),22) N                                                     PLTTER.119
 22   FORMAT('DOMAIN ',I2)                                                       PLTTER.120
      WRITE(6,23) TITLE                                                          PLTTER.121
 23   FORMAT('TITLE= ',A9) 
c                                                                               
C  OPEN THE COLOR TABLE FILE                                                    
      MAPUNIT=17                                                                
C  OPEN THE CORRECT TABLE FILE                                                  
      OPEN (MAPUNIT,FILE='mapco.tbl',STATUS='OLD')                                
C  SET UP A COLOR TABLE                                                         
      CALL RDCOLT(MAPUNIT)                                                      
C  MAKE THE MAP                                                                 
c     ... turn of clipping                                                      
      CALL GSCLIP (0)                                                           
c     ... use whole domain                                                      
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)                              
c     ... errsev: <0 abort on warning, =0 abort on error, >0 no abort           
      errsev=1                                                                  
      CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                               
     *             DX,JEND,IEND,xa,xb,ya,yb,                                    
     * '                                                            ',          
     *             9,N,MAPUNIT,.FALSE.,errsev)                                 
C  CLOSE THE TABLE FILE                                                         
      CLOSE (MAPUNIT)                                                           
      call getset(xsa,xsb,ysa,ysb,xsc,xsd,ysc,ysd,ltype)                        
C     ... nice ordinary map                                                     
C                                                                               
      CALL GFLAS3(N)                                                            
C                                                                               
      PRINT *,' -----> MAP PLOTTED'                                             
C                                                                               
C  DEFINE TITLE                                                                 
                                                                                
      XINC=1./FLOAT(JX-1) * 0.5                                                 
      YINC=1./FLOAT(IX-1) * 0.5                                                 
      if (ics.eq.0) then                                                        
        xinc = 0.0                                                              
        yinc = 0.0                                                              
      endif                                                                     
c                                                                               
c        ... put on the observed values on the map:                             
C                                                                               
      print 51,xsa,xsb,ysa,ysb,ltype                                            
51    format(2x,'xsa,xsb,ysa,ysb,ltype:',4f6.2,i5)                              
c                                                                               
      call set(xsa,xsb,ysa,ysb,1.,float(Jx),1.,float(Ix),ltype)                 
      dist=amax1(float(Jx),float(Ix))                                           
      delt=dist/50.                                                            
      call gsclip(0)                                                            
c      call gspmci(8+n)                                                         
c      call gstxci(8+n)                                                         
c      call gsplci(8+n)                                                         
c               
      number = 0                                                                
      do 40 j = 1,ns
         CALL LLXY(YLAT(J),YLON(J),XX,YY,IEND,JEND,DX)
         XX=XX-XA+1.
         YY=YY-YA+1. 
         xcs(j) = xx+float(joff)
         ycs(j) = yy+float(ioff)
c
        if (xx.lt.1.0 .or. xx.gt.float(jx) .or.                         
     *      yy.lt.1.0 .or. yy.gt.float(ix)) go to 40                    
        if (abs(asta(j)-spval).lt.1.e-3) go to 40                                          
        number = number+1                                                       
c        print 41,j,ylat(j),ylon(j),xx,yy,asta(j)                                       
41      format('j=',i3,2x,'lat,lon:',2f8.2,
     *                    ' xx,yy:',2f8.2,2x,'asta=',A6)                      
c                                                                               
            TTEMP = asta(j)                                               
            write(ssta,30) ttemp                                                
30          format(f5.0)                                                          
            xxx = xx                                                   
            yyy = yy                                                   
          call pwritx(xxx,yyy,ssta,5,8,0,0)
c          CALL plchlq(xxx,yyy,ssta,0.010,0.,0.)
cc          call points(xx,yy,1,-4,0)                                     
40    continue                                                                  
c
        CALL SETUSV('LW',LW)                                                    
C                                                          
      print *,number,' OF ',NAME,' OBS within the domain'                       
C                                                                               
      if (INDEX.EQ.1) then                                                      
c
        if (L.gt.8) then
          xrid = 1.4*rid(n)
        else
          xrid = rid(n)
        endif
        call ANAL(A2,ASTA,Xcs,Ycs,IX+2*ioff,JX+2*joff,ns,xRID,SPVAL)                           
c                                                                               
        do i = 1,ix                                                             
        do j = 1,jx                                                             
        ff(j,i) = a2(i+ioff,j+joff)                                                       
        end do                                                                  
        end do                                                                  
c                                                                               
        CALL SETUSV('LW',LW)                                                    
        call set(xsa+XINC,xsb-XINC,ysa+XINC,ysb-XINC,                           
     *                    1.,float(Jx-1),1.,float(Ix-1), 1)                     
        CALL CONREC(FF,JX,JX,IX,0.,0.,FINC,-1, 0, NDOT)                         
C                                                                               
      endif                                                                     
C                                                                               
          write(title,202) NAME
 202      format(A16)
      PRINT *,' -----> PARAMETERS ',NAME,' TITLE=',TITLE(1:8),' PLOTTED'                             
C
          Xx1 = 40.
          CALL SET(0.0,1.0,0.0,1.0,1.,100.,1.,100.,1)
          CALL PWRIT(Xx1, 8.,TITLE(1:16),16,2,0,0)
          CALL PWRITX(xx1+8.,8.,'''KGU''',5,1,0,0)
          CALL PWRITX(xx1+8.,8.,Symbol,1,isize,0,0)           
          CALL PWRITX(Xx1+8.,8.,'''PRU''',5,1,0,0)
C
      CALL FRAME
C                                                                               
      CALL SETUSV('LW',1000)                                                  
C
      RETURN                                                                    
      END                                                                       
