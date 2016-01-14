      SUBROUTINE PLSTN1(IX,JX,NS,YLAT,YLON,ASTA,IC,Symbol,NAME,N,
     >                  deltx,delty,isize,Xx1,ifr,
     >                  time_obs, time_window_min, time_window_max)
c
#     include "nestdmn.incl"
#     include    "maps.incl"                                            
C                                                                  
      DIMENSION ylat(ns), ylon(ns), asta(ns), time_obs (ns)
      CHARACTER*5 ASTA                                    
      integer Symbol
C                                                                               
      real  levels(100)                   ! levels for CONDRV                   
      character*80 title, TM              ! title string                        
      integer errsev, num_time_out, num_domain_out
      character gridloc*3
      character*(*) NAME                                                
      character*19 time_obs,time_window_min,time_window_max
      logical inside
C                              
      num_time_out = 0
      num_domain_out = 0
                                                 
      DX = DIS(N)         
      IR = NRATIO(N)
      XIR = FLOAT(IR)  
      JEND = (NESTJX(1)-1)*IR + 1  
      IEND = (NESTIX(1)-1)*IR + 1  
      ISTART = 1  
      JSTART = 1  
        JEX = 0   
        IEX = 0   
      IF (N.EQ.1) THEN  
        JEX = JOFFST  
        IEX = IOFFST   
      ENDIF   
      IF (N.EQ.1) THEN  
      XA = 1.0   
      YA = 1.0    
      XB = FLOAT(JEND)  
      YB = FLOAT(IEND)
      ELSE  
      XA = (XWEST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.    
      XB = (XEAST(N) - FLOAT(JOFFST) - 1.)*XIR + 1.   
      YA = (XSOUTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1. 
      YB = (XNORTH(N) -FLOAT(IOFFST) - 1.)*XIR + 1.   
      ENDIF  
C
C  AVOID THE XA, XB, YA, YB ARE OUTSIDE THE BIG DOMAIN:  
      IF (XA.LT.1.0) THEN    
          XA = 1.0    
          PRINT *,'XA < 1.0, XA=1.0'     
          STOP  
      ENDIF    
      IF (XB.GT.FLOAT(JEND)) THEN   
          XB = FLOAT(JEND)  
          PRINT *,'XB > JEND, XB = FLOAT(JEND)'   
          STOP   
      ENDIF   
      IF (YA.LT.1.0) THEN    
          YA = 1.0     
          PRINT *,'YA < 1.0, YA = 1.0'   
          STOP   
      ENDIF  
C
      PRINT 2001, N,ISTART,IEND,JSTART,JEND,XA,XB,YA,YB   
2001  FORMAT(/2X,'--- CALL PLSTN1 ..........',I3  
     1    /2X,'ISTART,IEND,JSTART,JEND:',4I6,3X,'XA,XB,YA,YB:',4F8.1)  
C
      WRITE(TITLE(1:9),22) N     
 22   FORMAT('DOMAIN ',I2) 
      WRITE(6,23) TITLE  
 23   FORMAT('TITLE= ',A9)     
C
C  OPEN THE COLOR TABLE FILE                                                    
      MAPUNIT=17                                                                
C  OPEN THE CORRECT TABLE FILE                                                  
cc      OPEN (MAPUNIT,FILE='map.tbl',STATUS='OLD')                                
      OPEN (MAPUNIT,FILE='mapco.tbl',STATUS='OLD')                                
C  SET UP A COLOR TABLE                                                         
      CALL RDCOLT(MAPUNIT)                                                      
C  MAKE THE MAP                                                                 
c     ... turn of clipping                                                      
      CALL GSCLIP (0)                                                           
c     ... use whole domain                                                      
      CALL SET (0.05,0.95,0.05,0.95,0.0,1.0,0.0,1.0,1)                              
c     ... errsev: <0 abort on warning, =0 abort on error, >0 no abort           
      errsev=1                                                                  
      print '(/a)',"CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                               
     *              DX,JEND,IEND,xa,xb,ya,yb,                                    
     * '                                                            ',          
     *             9,n,MAPUNIT,.FALSE.,errsev)"
      print '(a,a,2x,a,5f10.3,2x,a,2i5/
     *        a,4f10.3,2x,a,i3,1x,a,i4,1x,a,i3/)',
     *        'PROJEC=',PROJECT,'TRUELAT1,TRUELAT2,PHIC,XLONC,DX:',
     *        TRUELAT1,TRUELAT2,PHIC,XLONC,DX,
     *        'JEND,IEND:',JEND,IEND,
     *        'xa,xb,ya,yb:',xa,xb,ya,yb,                                    
     *        'n=',n,'MAPUNIT=',MAPUNIT,'errsev=',errsev
                                 
      CALL MAPDRV (PROJECT,TRUELAT1,TRUELAT2,PHIC,XLONC,                               
     *             DX,JEND,IEND,xa,xb,ya,yb,                                    
     * '                                                            ',          
     *             9,n,MAPUNIT,.FALSE.,errsev)                                 
C  CLOSE THE TABLE FILE                                                         
      CLOSE (MAPUNIT)                                                           
      call getset(xsa,xsb,ysa,ysb,xsc,xsd,ysc,ysd,ltype)                        
C     ... nice ordinary map                                                     
C                                                                               
      CALL GFLAS3(N)                                                            
C                                                                               
      PRINT *,' -----> MAP PLOTTED' 
C                                            
c        ... put on the observed values on the map:                             
C                                                                               
      print 51,xsa,xsb,ysa,ysb,ltype                                            
51    format(2x,'xsa,xsb,ysa,ysb,ltype:',4f6.2,i5)                              
c                                                                               
      call set(xsa,xsb,ysa,ysb,1.,float(Jx),1.,float(Ix),ltype)                 
      dist=amax1(float(Jx),float(Ix))                                           
      delt=dist/100.                                                            
      call gsclip(0)                                                            
      call gspmci(IC)                                                          
      call gstxci(IC)                                                          
      call gsplci(IC)                                                          
c                                                                               
      print *,'NS=',NS,'  DX=',DX
      number = 0                                                                
      do 40 j = 1,ns

      CALL INSIDE_WINDOW (time_obs (j), 
     >                    time_window_min, time_window_max, 
     >                    inside, 0)
      if (.not.inside) num_time_out = num_time_out + 1

      IF (inside) THEN

      CALL LLXY(YLAT(j),YLON(j),XX,YY,IEND,JEND,DX)
C  GET BACK TO THE NEST DOMAIN COORDINATE
         XX=XX-XA+1. 
         YY=YY-YA+1.  
         xcs(j) = xx
         ycs(j) = yy
        if (xx.lt.1.0 .or. xx.gt.float(jx) .or.                         
     *      yy.lt.1.0 .or. yy.gt.float(ix)) then
            num_domain_out = num_domain_out + 1
        print 
     * '("n_out=",i5,1x,"xx,yy:",2f10.2,1x,"lat,lon:",2f10.2,2x,2I4)',
     *          num_domain_out, xx,yy,YLAT(j),YLON(j), jx, ix 
            go to 40
        endif
c
          xx = xx+deltx
          yy = yy+delty
          call points(xx,yy,1,Symbol,0) 
c          CALL PWRITX(xx,yy,'''KGU''',5,1,0,0)
c          CALL PWRITX(xx,yy,Symbol,1,isize,0,0)           
c          CALL PWRITX(XX,YY,'''PRU''',5,1,0,0)
C                        
cc          CALL plchlq(xx,yy+delt,asta(j),0.01,0.,0.)                   
        number = number+1
cc        print 41,j,number,ylat(j),ylon(j),xx,yy,asta(j)
41      format('j=',i6,2x,'num=',i6,'  lat,lon:',2f11.5,
     *                    ' xx,yy:',2f11.5,2x,'asta=',A5)                      

      ENDIF
40    continue                                                                  
C                                                                               

c         write(title,202) number, NAME
          write(title,203) number, NAME,
     > time_window_min ( 1: 4), time_window_min ( 6: 7),
     > time_window_min ( 9:10), time_window_min (12:13),
     > time_window_min (15:16), 
     > time_window_max ( 9:10), time_window_max (12:13),
     > time_window_max (15:16)

          print*
          print*, title
          print '(5x,"num_time_out =",i5)',num_time_out
          print '(5x,"num_domain_out =",i5)',num_domain_out

 202      format(I7,2X,A6,2X)
 203      format(I7,2X,A10,2X,
     >A4,'-',A2,1X,'[',A2,'_',A2,':',A2,',',A2,'_',A2,':',A2,']')

C1997/07 [16:00,16:23]
          CALL SET(0.0,1.0,0.0,1.0,1.,100.,1.,100.,1)
          CALL PWRIT(Xx1, 6.,TITLE(1:48),48,2,0,0)
          call points(xx1-40.,6.,1,Symbol,0) 
c          CALL PWRITX(xx1+15.,6.,'''KGU''',5,1,0,0)
c          CALL PWRITX(xx1+15.,6.,Symbol,1,isize,0,0)           
c          CALL PWRITX(Xx1+15.,6.,'''PRU''',5,1,0,0)
C
      IF (IFR.EQ.1) CALL FRAME                                                                
cc      CALL COLOR
C                                                                               
      RETURN                                                                    
      END                                                                       
