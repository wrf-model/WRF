      SUBROUTINE PLSTN2(IX,JX,NS,ylat,ylon,ASTA,
     >                  tb19v_inv,tb19h_inv,tb22v_inv,
     >                  tb37v_inv,tb37h_inv,tb85v_inv,tb85h_inv,
     >                  tb19v,tb19h,tb22v,tb37v,tb37h,tb85v,tb85h,
     >                  NAME,N,
     >                  deltx,delty,isize,Xx1,ifr,
     >                  time_obs, time_window_min, time_window_max)
c
#     include "nestdmn.incl"
#     include    "maps.incl"                                            
C                                                                  
      DIMENSION ylat(ns), ylon(ns), time_obs (ns),asta(ns)

      DIMENSION tb19v_inv(ns), tb19h_inv(ns), tb22v_inv(ns)
      DIMENSION tb37v_inv(ns)
      DIMENSION tb37h_inv(ns), tb85v_inv(ns), tb85h_inv(ns)
      
      DIMENSION tb19v(ns), tb19h(ns), tb22v(ns), tb37v(ns)
      DIMENSION tb37h(ns), tb85v(ns), tb85h(ns)
      INTEGER IC
      real t1,t2,t3,t4,t5
      CHARACTER*5 ASTA                                    
      character*1 Symbol
C                                                                               
      real  levels(100)                   ! levels for CONDRV                   
      character*80 title, TM              ! title string                        
      integer errsev                                                            
      character gridloc*3
      character*(*) NAME                                                
      character*19 time_obs,time_window_min,time_window_max
      logical inside
C                                                                               
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
2001  FORMAT(/2X,'--- CALL PLSTN2 ..........',I3  
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
      CALL SET (0.0,1.0,0.0,1.0,0.0,1.0,0.0,1.0,1)                              
c     ... errsev: <0 abort on warning, =0 abort on error, >0 no abort           
      errsev=1                                                                  
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

c                                                                               
      t0=30.
      t1=20.
      t2=10.
      t3=3.
      t4=-3.
      t5=-7.
      print *,'NS=',NS,'  DX=',DX
      number = 0                                                                
      do 40 j = 1,ns
         IC = 10
         Symbol = '-'
c        if (tb19v_inv(j) .gt. t0) then
c red
c           IC = 14
c           Symbol = '+'
c        endif

c        if (tb19v_inv(j) .gt. t1 .and. tb19v_inv(j) .le. t0 ) then
c blue
c           IC = 15 
c           Symbol = '+'
c        endif

c        if (tb19v_inv(j) .gt. t2 .and. tb19v_inv(j) .le. t1 ) then
c           IC = 16 
c           Symbol = '+'
c        endif

         if (tb19v_inv(j) .gt. t3 .and. tb19v_inv(j) .le. t2 ) then
            IC = 16
            Symbol = '$'
         endif

         if (tb19v_inv(j) .gt. t4 .and. tb19v_inv(j) .le. t3 ) then
c dark red 14
c blue     12
c yellow    9
c skin     13
c green    10
            IC = 9 
            Symbol = '+'
         endif

         if (tb19v_inv(j) .gt. t5 .and. tb19v_inv(j) .le. t4 ) then
            IC = 12
            Symbol = '+'
         endif

         if (tb19v_inv(j) .le. t5) then
            IC = 7
            Symbol = '*'
         endif

         call gspmci(IC)                                                          
         call gstxci(IC)                                                          
         call gsplci(IC)                                                          

      CALL INSIDE_WINDOW (time_obs (j), 
     >                    time_window_min, time_window_max, 
     >                    inside, 0)

      IF (inside) THEN

      CALL LLXY(YLAT(j),YLON(j),XX,YY,IEND,JEND,DX)
c        XX = ssmixj(j)
c        YY = ssmiyi(j)
C  GET BACK TO THE NEST DOMAIN COORDINATE
         XX=XX-XA+1. 
         YY=YY-YA+1.  
         xcs(j) = xx
         ycs(j) = yy
        if (xx.lt.1.0 .or. xx.gt.float(jx) .or.                         
     *      yy.lt.1.0 .or. yy.gt.float(ix)) go to 40
c                                                                               
          xx = xx+deltx
          yy = yy+delty
          CALL PWRITX(xx,yy,'''KGU''',5,1,0,0)
          CALL PWRITX(xx,yy,Symbol,1,isize,0,0)           
          CALL PWRITX(XX,YY,'''PRU''',5,1,0,0)
C                        
        number = number+1

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

 202      format(I7,2X,A6,2X)
 203      format(I7,2X,A6,2X,
     >A4,'-',A2,1X,'[',A2,'_',A2,':',A2,',',A2,'_',A2,':',A2,']')

C1997/07 [16:00,16:23]
          CALL SET(0.0,1.0,0.0,1.0,1.,100.,1.,100.,1)
          CALL PWRIT(Xx1, 6.,TITLE(1:44),44,2,0,0)
          CALL PWRITX(xx1+15.,6.,'''KGU''',5,1,0,0)
          CALL PWRITX(xx1+15.,6.,Symbol,1,isize,0,0)           
          CALL PWRITX(Xx1+15.,6.,'''PRU''',5,1,0,0)
C
      IF (IFR.EQ.1) CALL FRAME                                                                
cc      CALL COLOR
C                                                                               
      RETURN                                                                    
      END                                                                       
