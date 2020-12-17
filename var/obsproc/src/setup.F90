      SUBROUTINE SETUP (domain_check_h, IPROJ, PHIC, XLONC, &
                                        TRUELAT1, TRUELAT2, &
#ifdef BKG
                        DIS, XCNTR, YCNTR, XN, POLE, PSI1,  C2)
#else
                        MAXNES, NESTIX, NESTJX, DIS, NUMC, NESTI, NESTJ, &
                        IXC, JXC, XCNTR, YCNTR, XN, POLE, PSI1,  C2, &
                        XIM11, XJM11) 
#endif
!------------------------------------------------------------------------------!
!
! MAP PROJECTION FACTORS CALCULATION
!
! INPUT:
! -----
!  IPROJ:     PROJECTION TYPE
!  PHIC:      CENTRAL LATITUDE
!  XLONC:     CENTRAL LONGITUDE
!  TRUELAT1:  TRUE LATITUDE 1
!  TRUELAT2:  TRUE LATITUDE 2
!  MAXNES:    NUMBER OF DOMAINS < 10
!  NESTIX (): HORIZONTAL DOMAINS DIMENSIONS IN I DIRECTION
!  NESTJX (): HORIZONTAL DOMAINS DIMENSIONS IN J DIRECTION
!  DIS ():    HORIZONTAL DOMAINS GRID SPACES IN METERS
!  NUMC ():   MOTHER DOMAIN ID (1= COARSE DOMAIN)
!  NESTI ():  DOMAINS' I-INDECES OF LOW LEFT CORNER IN MOTHER DOMAIN
!  NESTJ ():  DOMAINS' J-INDECES OF LOW LEFT CORNER IN MOTHER DOMAIN
!
! OUTPUT:
! ------
!  IXC:         COARSE DOMAIN GRID I DIMENSION
!  JXC:         COARSE DOMAIN GRID J DIMENSION
!  XCNTR:       X-COORDINATE OF COARSE DOMAIN CENTER
!  YCNTR:       Y-COORDINATE OF COARSE DOMAIN CENTER
!  XN:          CONE PROJECTION
!  POLE:        POLE LATITUDE
!  PSI1:       
!  C2:
!  XIM11 (10):  DOMAINS' INDECES OF LOW LEFT CORNER OF DOMAIN IDD
!  XJM11 (10):  DOMAINS' INDECES OF LOW LEFT CORNER OF DOMAIN IDD
!
! 
! Y.-R. GUO,       September 2000
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

      IMPLICIT NONE

      INTEGER, INTENT (inout) :: iproj
      REAL,    INTENT (inout) :: phic,  xlonc, truelat1, truelat2
#ifdef BKG
      REAL,    INTENT (out)   :: pole, xn, psi1, c2, xcntr, ycntr
      REAL,    INTENT (inout), DIMENSION (3) :: dis
#else
      INTEGER, INTENT (in)    :: maxnes
      INTEGER, INTENT (in), DIMENSION (maxnes) :: nestix, nestjx, NUMC, &
                                              nesti, nestj
      REAL,    INTENT (inout), DIMENSION (maxnes) :: dis

      INTEGER, INTENT (out)   :: ixc, jxc

      REAL,    INTENT (out)   :: pole, xn
      REAL,    INTENT (out)   :: xim11 (maxnes), xjm11 (maxnes)
      REAL,    INTENT (out)   :: psi1, c2, xcntr, ycntr
#endif
!------------------------------------------------------------------------------!
#ifdef BKG
      REAL :: psx, cell, cell2, r, phictr, sign

      INTEGER, DIMENSION (3) :: nproj

      CHARACTER (LEN=80) :: project
#else
!     REAL :: conv, a
      REAL :: iexp, aexp
      REAL :: psx, cell, cell2, r, phictr, hdsize, sign
      REAL :: xjc, xdis, yjc, ydis

      REAL :: cntrj0, cntri0
      INTEGER :: ixcmax, jxcmax
      REAL, DIMENSION (maxnes) :: xsouth,xwest,xnorth,xeast

      INTEGER :: incr
      INTEGER :: m, nm, nmc, nd1, nd2, mismatch
      INTEGER :: ioffst, joffst, iimxn, jjmxn
      INTEGER :: iratio (maxnes), nratio (maxnes)

      INTEGER, DIMENSION (3) :: nproj

      CHARACTER (LEN=80) :: project
!------------------------------------------------------------------------------!
!     DATA conv  / 57.29578 /
!     DATA a     / 6370.    /
#endif
      include 'constants.inc'
      
      logical  domain_check_h
      DATA nproj / 1, 2, 3  /
!     DATA nproj / 'LAMCON', 'POLSTR', 'MERCAT' /
!------------------------------------------------------------------------------!
      if (iproj == 0 ) then
          PROJECT='CE'
          XCNTR = 0
          YCNTR = 0
!          DIS(1) = 2.0 * pi * a / (NESTJX (1)-1) 
          DIS(1) = 110.0
          if (domain_check_h) STOP 'domain_check_h'
          goto 1100
      endif

!     WRITE (0,'(A)') " SET UP MAP PARAMETERS"

#ifdef BKG
! DEFINE THE PARAMETERS OF MAP BASED ON THE IPROJ:                            
                                                                               
      XN = -1.0E08                                                           

      IF (PHIC.LT.0) THEN                                                        
         SIGN=-1.      ! SOUTH HEMESPHERE 

      ELSE                                                                      

         SIGN=1.       ! NORTH HEMESPHERE

      ENDIF                                                                     

      POLE = 90.                                                             

      IF (IPROJ .EQ. NPROJ(1)) THEN
          if (abs(TRUELAT1 - TRUELAT2) .gt. 0.1) then
            XN = ALOG10 (COS (TRUELAT1 / CONV)) - &
                 ALOG10 (COS (TRUELAT2 / CONV))                                         
 
            XN = XN/(ALOG10 (TAN ((45.0 - SIGN*TRUELAT1/2.0) / CONV)) - & 
                     ALOG10 (TAN ((45.0 - SIGN*TRUELAT2/2.0) / CONV)))          
          else
            XN = sign*sin(truelat1 / conv)
          endif

          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT='LC'

      ELSE IF (IPROJ .EQ. NPROJ(2)) THEN

          XN = 1.0


! PSI1 IS THE PSEUDO-LATITUDE                                                   

          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT = 'ST'

      ELSE IF (IPROJ .EQ. NPROJ(3)) THEN

          XN = 0.                                                                

          PSI1 = 0.
          PROJECT = 'ME'

      END IF                                                                    

      PSI1 = PSI1 / CONV

      IF (PHIC .LT. 0.) THEN                                                      
          PSI1 = -PSI1
          POLE = -POLE

      ENDIF                                                                     

!--------CALCULATE R                                                            

      IF (IPROJ .NE. NPROJ(3)) THEN

          PSX = (POLE-PHIC)/CONV

          IF (IPROJ .EQ. NPROJ(1)) THEN

              CELL  = A*SIN (PSI1)/XN
              CELL2 = (TAN (PSX/2.)) / (TAN (PSI1/2.))                               
         ENDIF                                                                  

         IF (IPROJ .EQ. NPROJ(2)) THEN
            CELL  = A*SIN (PSX)/XN
            CELL2 = (1. + COS (PSI1))/(1. + COS (PSX))
         ENDIF                                                                  

         R = CELL*(CELL2)**XN                                                   

         XCNTR = 0.0                                                            
         YCNTR = -R                                                             

      ENDIF                                                                     

!-----FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1            

      IF (IPROJ .EQ. NPROJ(3)) THEN

         C2     = A*COS (PSI1)
         XCNTR  = 0.0                                                           
         PHICTR = PHIC/CONV                                                     
         CELL   = COS (PHICTR)/(1.0+SIN (PHICTR)) 
         YCNTR  = - C2*ALOG (CELL)                                              

      ENDIF                                                                     
1100  continue

#else
                                                                              
! REMOVE DOMAIN EXTENSION IN MM5 INPUT/OUTPUT FILES

      IEXP = 0
      AEXP =  -1.001*DIS(1)

! REMOVE INITIALIZATION OF TRUE LAT 

!     TRUELAT1=91.0                                                             
!     TRUELAT2=91.0                                                             

! DEFINE THE PARAMETERS OF MAP BASED ON THE IPROJ:                            
                                                                               
      XN = -1.0E08                                                           

      IF (PHIC.LT.0) THEN                                                        
         SIGN=-1.      ! SOUTH HEMESPHERE 

      ELSE                                                                      

         SIGN=1.       ! NORTH HEMESPHERE

      ENDIF                                                                     

      POLE = 90.                                                             

      IF (IPROJ .EQ. NPROJ(1)) THEN

          IF (ABS (TRUELAT1) .GT. 90.) THEN
              TRUELAT1 = 60.
              TRUELAT2 = 30.
              TRUELAT1 = SIGN*TRUELAT1
              TRUELAT2 = SIGN*TRUELAT2
          ENDIF

          IF (ABS(TRUELAT1 - TRUELAT2) .GT. 0.1) then
            XN = ALOG10 (COS (TRUELAT1 / CONV)) - &
                 ALOG10 (COS (TRUELAT2 / CONV))

            XN = XN/(ALOG10 (TAN ((45.0 - SIGN*TRUELAT1/2.0) / CONV)) - &
                     ALOG10 (TAN ((45.0 - SIGN*TRUELAT2/2.0) / CONV)))
          ELSE
            XN = SIGN*SIN(TRUELAT1 / CONV)
          ENDIF

          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT='LC'

      ELSE IF (IPROJ .EQ. NPROJ(2)) THEN

          XN = 1.0

          IF (ABS(TRUELAT1) .GT. 90.) THEN

              TRUELAT1 = 60.
              TRUELAT2 = 0.

              TRUELAT1 = SIGN*TRUELAT1
              TRUELAT2 = SIGN*TRUELAT2

          ENDIF


! PSI1 IS THE PSEUDO-LATITUDE                                                   

          PSI1 = 90.-SIGN*TRUELAT1

          PROJECT = 'ST'

      ELSE IF (IPROJ .EQ. NPROJ(3)) THEN

          XN = 0.                                                                

          IF (ABS (TRUELAT1) .GT. 90.) THEN

              TRUELAT1 = 0.
              TRUELAT2 = 0.                                                               
          ENDIF

          IF (TRUELAT1 .NE. 0.) THEN                                                   
              WRITE (0,'(/,A)') &
             "MERCATOR PROJECTION IS ONLY SUPPORTED AT 0 DEGREE TRUE LATITUDE."
              WRITE (0,'(A,/)') &
             "TRUELAT1 IS RESET TO 0"

               TRUELAT1 = 0.                                                               
          ENDIF

          PSI1 = 0.
          PROJECT = 'ME'

      END IF                                                                    

      PSI1 = PSI1 / CONV

      IF (PHIC .LT. 0.) THEN                                                      
          PSI1 = -PSI1
          POLE = -POLE

      ENDIF                                                                     

!--------CALCULATE R                                                            

      IF (IPROJ .NE. NPROJ(3)) THEN

          PSX = (POLE-PHIC)/CONV

          IF (IPROJ .EQ. NPROJ(1)) THEN

              CELL  = A*SIN (PSI1)/XN
              CELL2 = (TAN (PSX/2.)) / (TAN (PSI1/2.))                               
         ENDIF                                                                  

         IF (IPROJ .EQ. NPROJ(2)) THEN
            CELL  = A*SIN (PSX)/XN
            CELL2 = (1. + COS (PSI1))/(1. + COS (PSX))
         ENDIF                                                                  

         R = CELL*(CELL2)**XN                                                   

         XCNTR = 0.0                                                            
         YCNTR = -R                                                             

      ENDIF                                                                     

!-----FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1            

      IF (IPROJ .EQ. NPROJ(3)) THEN

         C2     = A*COS (PSI1)
         XCNTR  = 0.0                                                           
         PHICTR = PHIC/CONV                                                     
         CELL   = COS (PHICTR)/(1.0+SIN (PHICTR)) 
         YCNTR  = - C2*ALOG (CELL)                                              

      ENDIF                                                                     

1100  continue

      WRITE (0,'(2(A,F8.1),A,2X,A,f10.3)') &
      " COARSE GRID CENTER IS AT X = ",XCNTR," KM AND Y = ",YCNTR," KM.", &
      " DIS(1)=", DIS(1) 


!   CHECK THE COMPATIBILITY OF NEST DOMAINS WITH THE COARSE DOMAINS             
!     AND CALCULATE THE IRATIOS, INORTHS, ISOUTHS, IWESTS AND IEASTS            

!     A) EXTENDING THE COARSE DOMAIN IF IEXP = 1                                

        IXC = NESTIX (1)
        JXC = NESTJX (1)

        IOFFST = 0                                                              
        JOFFST = 0                                                              

      IF (IEXP .EQ. 1) THEN

          INCR = INT (AEXP/DIS (1) + 1.001)

          IXC = NESTIX (1) + INCR*2
          JXC = NESTJX (1) + INCR*2

          IOFFST = INCR
          JOFFST = INCR

          WRITE (0,'(A,I3)') " GRID IS EXPANDED BY ",INCR, &
                             " GRID POINTS ON EACH EDGE."
!          WRITE (0,'(2(A,I3))') &
!         "EXTENDED IXC IS ",IXC," EXTENDED JXC IS ",JXC               

      ENDIF                                                                     

!-----CENTER OF GRID IN THE COARSE DOMAIN                                       

      CNTRJ0 = FLOAT (JXC+1)/2.
      CNTRI0 = FLOAT (IXC+1)/2.

!      WRITE (0,'(2(A,I5))') &
!     "COARSE DOMAIN SIZE IX = ",NESTIX(1)," JX = ", NESTJX(1)                                            
!  MIX, MJX ARE USED IN SUB. TFUDGE:                                            

      IXCMAX = IXC
      JXCMAX = JXC 

      DO M = 1, MAXNES                                                       
         IXCMAX = MAX0 (NESTIX(M),IXCMAX)
         JXCMAX = MAX0 (NESTJX(M),JXCMAX)
      ENDDO

!      PRINT 24, IXCMAX,JXCMAX
!24    FORMAT('   ++> THE MAXIMUM DIMENSION = (',2I5,') <++')

!  CHECK IF POLE IS INSIDE THE DOMAIN OR NOT FOR LAMBERT PROJECTION:            

      HDSIZE = (IXC-1)*DIS(1)/2.                                               

!     IF (HDSIZE.GT.ABS(YCNTR) .AND. IPROJ.EQ.NPROJ(1))THEN                     
!        PRINT *,'-------------------------------------------------'            
!        PRINT *,'HALF DOMAIN SIZE IN Y-DIRECTION = ',HDSIZE                    
!        PRINT *,'    DISTANCE FROM CNTER TO POLE = ',ABS(YCNTR)                
!        PRINT *,'NOT MAKE SENSE WITH THE POLE IS INSIDE THE DOMAIN '           
!        PRINT *,'    FOR LAMBERT CONFORMAL PROJECTION!'                        
!        PRINT *,'=== PLEASE RE-SPECIFY THE CENTER OR DOMAIN SIZE. ==='         
!        STOP                                                                   
!     ENDIF                                                                     

!     B) CALCULATING THE IRATIOS, INORTHS AND IEASTS:                           

      IRATIO (1) = 1
      NRATIO (1) = 1
      XSOUTH (1) = 1.
      XWEST  (1) = 1.
      XNORTH (1) = FLOAT (IXC)
      XEAST  (1) = FLOAT (JXC)

      XJC = (XEAST(1) + 1.0)/2.                                                 

!      PRINT 27,XSOUTH(1),XWEST(1),XNORTH(1),XEAST(1),DIS(1),& 
!               IRATIO(1),NRATIO(1)                                            
! 27   FORMAT(1X,'XSOUTH(1)= ',F6.1,2X,'XWEST(1)= ',F6.1,2X, &
!      'XNORTH(1)= ',F6.1,2X,'XEAST(1)= ',F6.1,2X,'DIS(1)= ',F6.1,&
!      2X,'IRATIO(1)= ',I3,2X,'NRATIO(1)= ',I3)                                  

      MISMATCH = 0                                                              
                                                                                
      DO 30 NM = 2, MAXNES                                                      

!  DOMAINS' CONSISTENCY CHECK:                                                  

      NMC = NUMC (NM)

      IF (AMOD ((DIS (NMC)+0.09),DIS (NM)) .GT. 0.1) THEN

         MISMATCH = MISMATCH + 1                                                

!        PRINT 29, NM,NMC                                                       
!        PRINT 31,NM,DIS(NM),NMC,DIS(NMC)                                       
!29      FORMAT(2X,'DOMAIN ',I2,' HAS INCORRECT GRID SIZE ', &
!      '  IT HAS TO BE THE MULTIPLE OF DOMAIN ',I2)                             
!31      FORMAT(2X,'DOMAIN ',I2,' GRID SIZE= ',F6.1,' KM', &
!               '  DOMAIN ',I2,' GRID SIZE= ',F6.1,' KM')                        

        GO TO 30                                                                

      ENDIF                                                                     

      IRATIO (NM) = NINT (DIS (NMC)/ DIS (NM))
      NRATIO (NM) = NINT (DIS (1)  / DIS (NM))                                         

!   MAKE SURE THE 4 CORNER POINTS OF NEST DOMAINS ARE ON THE                    
!   PREVIOUS DOMAIN GRID-POINTS                                                 

      IF (MOD((NESTIX(NM)-1),IRATIO(NM)).NE.0) THEN                             

        MISMATCH = MISMATCH + 1                                                 

        IIMXN = (INT(FLOAT(NESTIX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1          

!        PRINT 32,NM,NESTIX(NM),IRATIO(NM),IIMXN                                
!32      FORMAT(2X,'NESTIX(',I2,')=',I4,' AND IRATIO=',I2,&
!      ' DOES NOT MATCH, YOU MAY SET NESTIX TO ',I4)

      ENDIF                                                                     

      IF (MOD ((NESTJX (NM)-1),IRATIO (NM)).NE.0) THEN

         MISMATCH = MISMATCH + 1

        JJMXN = (INT(FLOAT(NESTJX(NM)-1)/IRATIO(NM))+1)*IRATIO(NM) + 1          

!        PRINT 33,NM,NESTJX(NM),IRATIO(NM),JJMXN                               
!33      FORMAT(2X,'NESTJX(',I2,')=',I4,' AND IRATIO=',I2,&
!      ' DOES NOT MATCH, YOU MAY SET NESTJX TO ',I4)                                         
      ENDIF                                                                    
!                                                                               
!-----REDEFINE LOCATION OF LOWER LEFT CORNER OF FINE MESH (IN TERMS          
!     OF EXTENDED COARSE MESH - DOMAIN 1 INDICES) IF USING EXTENDED G        
                                                                                
!      PRINT 34,NM,NESTIX(NM),NESTJX(NM),DIS(NM),NESTI(NM),NESTJ(NM),&
!      NUMC(NM),IRATIO(NM),NRATIO(NM),IEXP                                   
!34    FORMAT(/1X,'DOMAIN ',I2,2X,'IX=',I3,2X,'JX=',I3,2X, &
!      'DS= ',F6.1,2X,'ICNS=',I4,2X,'JCNS=',I4,2X,&
!      'NUMC= ',I2,2X,'IRATIO= ',I2,2X,'NRATIO= ',I2,2X,&
!      'IEXP= ',I2)                                                   

      XDIS = 0.0                                                                
      YDIS = 0.0                                                                
      ND1 = NM                                                                  
      ND2 = NMC                                                                 

40    CONTINUE                                                                  

      XDIS = (NESTI(ND1)-1)*DIS(ND2) + XDIS
      YDIS = (NESTJ(ND1)-1)*DIS(ND2) + YDIS

      IF (ND2 .GT. 1) THEN                                                      
        ND1 = ND2                                                               
        ND2 = NUMC(ND2)                                                        
        GO TO 40                                                                
      ENDIF                                                                     

      XSOUTH(NM) = XDIS/DIS(1) + FLOAT(IOFFST) + 1                             
      XWEST(NM)  = YDIS/DIS(1) + FLOAT(JOFFST) + 1                             
      XNORTH(NM) = XSOUTH(NM) + FLOAT(NESTIX(NM)-1)*DIS(NM)/DIS(1)             
      XEAST(NM)  = XWEST(NM) + FLOAT(NESTJX(NM)-1)*DIS(NM)/DIS(1)             
                                                                                
!      PRINT 35                                                                 
!      PRINT 36,XSOUTH(NM),XWEST(NM),XNORTH(NM),XEAST(NM)                       
!35    FORMAT(2X,'COARSE MESH INDICES FOR THE 4 CORNER POINTS ARE')             
!36    FORMAT(2X,'SOUTH=',F6.1,3X,'WEST=',F6.1,3X,'NORTH =',F6.1,3X, & 
!            'EAST = ',F6.1)                                                   

30    CONTINUE                                                                 

      IF (MISMATCH.GT.0) THEN                                                  

!       PRINT *, &
!      'TERRAIN STOP IN SUBROUTINE SETUP DUE TO INCORRECT NEST DOMAIN SET UP' 
!       STOP 1111

       ENDIF                                                                    

!      Copy output

       
       XIM11 = XSOUTH
       XJM11 = XWEST

#endif
       END SUBROUTINE SETUP
