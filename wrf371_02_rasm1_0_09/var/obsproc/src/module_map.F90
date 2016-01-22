MODULE MODULE_MAP

!------------------------------------------------------------------------------!
! MM5 Map projection parameters needed to compute MM5 coordinates (xj, yi)
! from latitude and longitude.
! get_map_params: Read the parameters from an MM5 V2/V3 input/output file
! set_map_param:  Set the map parameters from the input read in MM5 file
!
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!


! INTEGER  :: IDD, IPROJ, IXC,   JXC
! REAL     :: PHIC,  XLONC, XN,    TRUELAT1, TRUELAT2, POLE, &
!             DIS (10),   XIM11 (10), XJM11 (10), &
!             PSI1,  C2,    XCNTR, YCNTR

  USE MODULE_MM5

!------------------------------------------------------------------------------!
CONTAINS

      SUBROUTINE get_map_params (iunit)
!------------------------------------------------------------------------------!
! PURPOSE:
! -------
!   READ THE MAP PROJECTION PARAMETERS: IPROJ, IXC, JXC, PHIC, XLONC, XN, 
!   TRUELAT1, TRUELAT2, POLE, DSC, DSM, XIM11, XJM11 
!   FROM A MM5 V2/V3 INPUT/OUTPUT FILE.
!
! Input:
!   iunit:  Logical file unit (file must be preably opened
!
! Output:  
!   IXC:      COARSE DOMAIN GRID DIMENSION IN I (N-S) DIRECTION
!   JXC:      COARSE DOMAIN GRID DIMENSION IN J (E-W) DIRECTION
!   IPROJ:    MAP PROJECTION (1 = LAMBER, 2 = POLAR, 3 = MERCATOR)
!   DSC:      COARSE DOMAIN GRID DISTANCE IN KM
!   PHIC:     COARSE DOMAIN CENTER LATITUDE
!   XLONC:    COARSE DOMAIN CENTER LONGITUDE
!   XN:       CONE FACTOR
!   TRUELAT1: TRUE LATITUDE 1 (DEGREE)
!   TRUELAT2: TRUE LATITUDE 2 (DEGREE)
!   POLE:     POLE POSITION IN DEGREE LATIITUDE
!   DSM :     DOMAIN GRID DISTANCE IN KM
!   XIM11:    I LOCATION IN THE COARSE DOMAIN OF THE CURRENT DOMAIN POINT (1,1)
!   XJM11:    J LOCATION IN THE COARSE DOMAIN OF THE CURRENT DOMAIN POINT (1,1)
!
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      INTEGER, INTENT (in)  :: iunit
!     INTEGER, INTENT (out) :: iproj, ixc, jxc
!     REAL,    INTENT (out) :: phic, pole, truelat1, xn, &
!                              xlonc, truelat2, dsc, dsm, xim11, xjm11

      INTEGER               :: io_error
      CHARACTER (LEN= 8)    :: message

      INTEGER(kind=4)       :: flag
      integer(kind=4), dimension(50,20) :: bhi
      real(kind=4),    dimension(20,20) :: bhr
      character(80),   dimension(50,20) :: bhic
      character(80),   dimension(20,20) :: bhrc

!------------------------------------------------------------------------------!

      REWIND (iunit)

! 1.  READ V3 HEADER
! ==================

! 1.1 V3 very first record must be an integer
!     --------------------------------------

      flag = 2

      READ (iunit, IOSTAT = io_error) flag

! 1.2 Then come headers
!     -----------------

      IF ((io_error .EQ. 0) .AND. (flag .EQ. 0)) THEN

           READ (iunit, IOSTAT = io_error) bhi, bhr, bhic, bhrc

! 1.3 Integer map parameters
!     ----------------------

           IXC   = bhi ( 5,1)
           JXC   = bhi ( 6,1)
           IPROJ = bhi ( 7,1)
#ifdef BKG

           NUMC  = 1

           NESTIX= bhi ( 5,1)
           NESTJX= bhi ( 6,1)

           NESTI = 1
           NESTJ = 1

           IDD         = bhi(13,1)
           NUMC  (IDD) = bhi(14,1)

           NESTIX(IDD) = bhi(16,1)
           NESTJX(IDD) = bhi(17,1)

           NESTI (IDD) = bhi(18,1)
           NESTJ (IDD) = bhi(19,1)

! 1.4 Real map parameters
!     -------------------

           ptop  = bhr(2,2)
           ps0   = bhr(2,5)
           ts0   = bhr(3,5)
           tlp   = bhr(4,5)

           DIS (1)     = bhr (1, 1) * 0.001
           DIS (IDD)   = bhr (9, 1) * 0.001
#else
           IDD   = bhi (13,1)

! 1.4 Real map parameters
!     -------------------

           DIS (1)     = bhr (1, 1)
           DIS (IDD)   = bhr (9, 1)
#endif
           PHIC        = bhr (2, 1)
           XLONC       = bhr (3, 1)
           XN          = bhr (4, 1)
           TRUELAT1    = bhr (5, 1)
           TRUELAT2    = bhr (6, 1)
           POLE        = bhr ( 7, 1)
           XIM11 (IDD) = bhr (10, 1)
           XJM11 (IDD) = bhr (11, 1)


      ELSE

         write(unit=*, fmt='(a,i4)') &
              'Error reading big header of unit:', iunit
         call abort()

      ENDIF


! 3.  OTHER FILE FORMATS ARE UNKNOWN
! ==================================

      IF (io_error .NE. 0) THEN
          WRITE (message,'(I3)') iunit
          CALL error_handler ('get_mm5_params.F90',&
             ' Cannot read file unit ', TRIM  (message), .TRUE.)
      ENDIF


! 4. PRINT OUT
! ============

      WRITE (0,'(4(A,A,F7.2,/))',ADVANCE='no')  &
      "Map parameters: "," Phic = ",PHIC,    &
      "                "," Xlonc= ",XLONC,   &
      "                "," True1= ",TRUELAT1,&
      "                "," True2= ",TRUELAT2

      WRITE (0,'(A,A,I4)',ADVANCE='no') &
      "                "," Iproj= ",IPROJ

      SELECT CASE (IPROJ)
             CASE (1);      WRITE (0,'(A)') " (LAMBERT CONFORMAL)"
             CASE (2);      WRITE (0,'(A)') " (POLAR STEREO.)"
             CASE (3);      WRITE (0,'(A)') " (MERCATOR)"
             CASE DEFAULT ; WRITE (0,'(A)') " (UNKNOWN)"
      END SELECT

      END SUBROUTINE GET_MAP_PARAMS

!     SUBROUTINE SET_MAP_PARAMS (iproj, ixc,   jxc, &
!                                phic,  xlonc, xn,    truelat1, truelat2, pole,&
!                                dsc,   dsm,   xim11, xjm11, &
!                                c2, psi1, xcntr, ycntr)
      SUBROUTINE SET_MAP_PARAMS
!------------------------------------------------------------------------------!
! Purpose:
! -------
!   Set the map projection parameters: c2, psi1, xcntr, ycntr
!
! Input:
! -----
!  iproj:    Map projection (1 = lamber, 2 = polar, 3 = mercator)
!  phic:     Coarse domain center latitude (in degree)
!  pole:     Pole position latitude (in degree)
!  truelat1: True latitude for domain 1 (in degree)
!
! Output:
! ------
!  c2: 
!  psi1:
!  xcntr: 
!  ycntr:
!
!------------------------------------------------------------------------------!
      IMPLICIT NONE

!     INTEGER, INTENT (in)  :: iproj, ixc, jxc
!     REAL,    INTENT (in)  :: phic, pole, truelat1, xn, &
!                              xlonc, truelat2, dsc, dsm, xim11, xjm11
!     REAL,    INTENT (out) :: c2, psi1, xcntr, ycntr

      REAL :: psx, cell, cell2, r, phictr

      ! Pi and earth radius in km

!     REAL, PARAMETER :: PI   = 3.1415926535897932346
!     REAL, PARAMETER :: CONV = 180. / PI
!     REAL, PARAMETER :: A    = 6378.15

      INCLUDE 'constants.inc'
!------------------------------------------------------------------------------C

!     WRITE (0,'(A)') ' SET MAP PARAMETERS'
                                                                    
!   DEFINE PSI1:

        IF (IPROJ .EQ. 1 .OR. IPROJ.EQ.2) THEN

            IF(PHIC .LT. 0) THEN 
               PSI1 = 90.+TRUELAT1
               PSI1 = -PSI1
            ELSE
               PSI1 = 90.-TRUELAT1
            ENDIF          

        ELSE

            PSI1 = 0.

        ENDIF 

        PSI1 = PSI1/CONV
         
! --------CALCULATE R

        IF (IPROJ .NE. 3) THEN

            PSX = (POLE-PHIC)/CONV

            IF (IPROJ.EQ.1) THEN

                CELL  = A*SIN (PSI1)/XN  
                CELL2 = (TAN (PSX/2.))/(TAN (PSI1/2.))
                WRITE (0,*) ' CELL = ' ,A,PSI1,XN,CELL

           ENDIF 

           IF (IPROJ.EQ.2) THEN

               CELL  = A*SIN (PSX)/XN   
               CELL2 = (1. + COS (PSI1))/(1. + COS (PSX)) 

           ENDIF  

           R = CELL*(CELL2)**XN 

           XCNTR = 0.0 
           YCNTR = -R
            WRITE (0,*) ' YCNTR = ' ,CELL,CELL2,XN

        ENDIF  
!
! -----FOR MERCATOR PROJECTION, THE PROJECTION IS TRUE AT LAT AT PHI1

        IF (IPROJ .EQ. 3) THEN

            C2     = A*COS (PSI1) 
            XCNTR  = 0.0 
            PHICTR = PHIC/CONV 
            CELL   = COS (PHICTR)/(1.0+SIN (PHICTR)) 
            YCNTR  = - C2*ALOG (CELL)  

         ENDIF 

      WRITE (0,'(3(A,A,F7.2,/))',ADVANCE='no') &
      "                "," C2   = ",C2,   &
      "                "," Psi1 = ",Psi1, &
      "                "," Xcntr= ",Xcntr
      WRITE (0,'(1(A,A,F9.2))') &
      "                "," Ycntr= ",Ycntr

! 4.  END
! =======
      RETURN

      END SUBROUTINE SET_MAP_PARAMS

      SUBROUTINE LLXY (XLATI,XLONI,X,Y)
!------------------------------------------------------------------------------!
!
!                 ROUTINE LLXY
!                **************
!
!
! PURPOSE:  CALCULATES THE (X,Y) LOCATION (DOT) IN THE MESOSCALE GRIDS
! -------   FROM LATITUDES AND LONGITUDES
!
!
!  INPUT:   
!  -----
!   XLAT:    LATITUDES 
!   XLON:    LONGITUDES 
!
! OUTPUT:                      
! -----
!   X:        THE COORDINATE IN X (J)-DIRECTION.
!   Y:        THE COORDINATE IN Y (I)-DIRECTION.
!
!------------------------------------------------------------------------------!

       IMPLICIT NONE

       REAL, INTENT(IN)  :: XLATI, XLONI
!      REAL, INTENT(IN)  :: ID
       REAL, INTENT(OUT) :: X, Y
       REAL              :: DXLON
       REAL              :: XLAT, XLON
       REAL              :: XX, YY, XC, YC
       REAL              :: CELL, PSI0, PSX, R, FLP
       REAL              :: CENTRI, CENTRJ
       INTEGER           :: NRATIO 

       real              :: bb

       INCLUDE 'constants.inc'

!------------------------------------------------------------------------------!
!
       XLON = XLONI
       XLAT = XLATI
!
       XLAT = MAX (XLAT, -89.95)
       XLAT = MIN (XLAT, +89.95)
!
       DXLON = XLON - XLONC
       IF (DXLON.GT. 180) DXLON = DXLON - 360. 
       IF (DXLON.LT.-180) DXLON = DXLON + 360. 

       IF (IPROJ.EQ.3) THEN
!
           XC = XCNTR
           YC = YCNTR  
!
! For Mercator projection, high latitude has no definition:
           IF (abs(XLAT) .gt.89.95) THEN
           PRINT '(A,F8.3,A,F8.3)', &
             "**WARNING** MERCATOR PROJECTION, BUT abs(XLAT)= |", &
             XLAT,"| > 89.95 ??   XLON=",xlon
             Y = -9.9999e8
             X = -9.9999e8
             RETURN
           ENDIF
 
           CELL = COS(XLAT/CONV)/(1.0+SIN(XLAT/CONV)) 
           YY = -C2*ALOG(CELL)
           XX = C2*DXLON/CONV  

       ELSE                       
!
              PSI0 = ( POLE - PHIC )/CONV
              XC = 0.0
!
!------ CALCULATE X,Y COORDS. RELATIVE TO POLE
!
              FLP = XN*DXLON/CONV

              PSX = ( POLE - XLAT )/CONV

              if (IPROJ == 2) then
                 bb = 2.0*(COS(PSI1/2.0)**2)
                 YC = -A*bb*TAN(PSI0/2.0)
                  R = -A*bb*TAN(PSX/2.0)
              else
                 bb = -A/XN*SIN(PSI1)
                 YC = bb*(TAN(PSI0/2.0)/TAN(PSI1/2.0))**XN
                  R = bb*(TAN(PSX /2.0)/TAN(PSI1/2.0))**XN   
              endif

              IF (PHIC .LT. 0.0) THEN
                  XX = R*SIN(FLP)
                  YY = R*COS(FLP)
              ELSE
                  XX = -R*SIN(FLP)
                  YY = R*COS(FLP)
              END IF 

       ENDIF     
!
!  TRANSFORM (1,1) TO THE ORIGIN 
!
       CENTRI = FLOAT (IXC + 1)/2.0  ! the location of the center in the coarse
       CENTRJ = FLOAT (JXC + 1)/2.0  ! domain
!
       X = ( XX - XC )/DIS (1) + CENTRJ  ! the (X,Y) coordinates in the coarse 
       Y = ( YY - YC )/DIS (1) + CENTRI  ! domain
!
       NRATIO = NINT(DIS (1) / DIS (IDD))
!
       X = (X - XJM11 (IDD))*NRATIO + 1.0
       Y = (Y - XIM11 (IDD))*NRATIO + 1.0
!
      END SUBROUTINE LLXY

      SUBROUTINE XYLL (XX,YY,XLAT,XLON)
!--------------------------------------------------------------------------CCC
!
!                 ROUTINE XYLL
!                **************
!
!
! PURPOSE:  CALCULATES THE LATITUDES AND LONGITUDES FROM THE
! -------   (X,Y) LOCATION (DOT) IN THE MESOSCALE GRIDS.
!
!  INPUT:   
!  -----
!  X:         THE COORDINATE IN X (J)-DIRECTION.
!  Y:         THE COORDINATE IN Y (I)-DIRECTION.
!
! OUTPUT:                      
! -----
!
!   XLAT:     LATITUDES 
!   XLON:     LONGITUDES 
!----------------------------------------------------------------------------CC
!
        IMPLICIT NONE
!
! ARGUMENT
!
        REAL, INTENT(IN)  :: XX, YY
        REAL, INTENT(OUT) :: XLAT,XLON
!       INTEGER, INTENT(IN) :: ID
!
! LOCAL
!
        REAL :: CNTRI, CNTRJ
        REAL :: X, Y
        REAL :: FLP, FLPP, R, RXN, CELL, CEL1, CEL2, PSX

        INCLUDE 'constants.inc'
!------------------------------------------------------------------------------C

        CNTRI = float(IXC+1)/2.
        CNTRJ = float(JXC+1)/2. 
!
!-----CALCULATE X AND Y POSITIONS OF GRID AT DOT POINT
!
       X = XCNTR+(XX-1.0)*DIS(IDD)+(XJM11(IDD)-CNTRJ)*DIS(1)
       Y = YCNTR+(YY-1.0)*DIS(IDD)+(XIM11(IDD)-CNTRI)*DIS(1)

!-----NOW CALCULATE LAT AND LON OF THIS POINT
!
       IF (IPROJ.NE.3) THEN
!
           IF (Y.EQ.0.) THEN      
!
              IF (X.GE.0.0) FLP =  90.0/CONV 
              IF (X.LT.0.0) FLP = -90.0/CONV
!
           ELSE
!
              IF (PHIC.LT.0.0)THEN
                  FLP = ATAN2(X,Y)   
              ELSE
                 FLP = ATAN2(X,-Y) 
              ENDIF
!
           ENDIF 
!
           FLPP = (FLP/XN)*CONV+XLONC  
!
           IF (FLPP.LT.-180.) FLPP = FLPP + 360    
           IF (FLPP.GT.180.)  FLPP = FLPP - 360.  
!
           XLON = FLPP 
!
!--------NOW SOLVE FOR LATITUDE
!
         R = SQRT(X*X+Y*Y)  
!
         IF (PHIC.LT.0.0) R = -R  
!
         IF (IPROJ.EQ.1) THEN   
             CELL = (R*XN)/(A*SIN(PSI1))    
             RXN  = 1.0/XN   
             CEL1 = TAN(PSI1/2.)*(CELL)**RXN    
         ENDIF 
!
         IF (IPROJ.EQ.2) THEN
             CELL = R/A        
             CEL1 = CELL/(1.0+COS(PSI1))  
         ENDIF 
!
         CEL2 = ATAN(CEL1)    
         PSX  = 2.*CEL2*CONV
         XLAT = POLE-PSX 
!
       ENDIF   
!
!-----CALCULATIONS FOR MERCATOR LAT,LON    
!
       IF (IPROJ.EQ.3) THEN   
!
           XLON = XLONC + ((X-XCNTR)/C2)*CONV 
!
           IF (XLON.LT.-180.) XLON = XLON + 360
           IF (XLON.GT.180.)  XLON = XLON - 360.

           CELL = EXP(Y/C2)  
           XLAT = 2.*(CONV*ATAN(CELL))-90.0 
!
       ENDIF  
! 
       END SUBROUTINE XYLL
!==============================================================================!
END MODULE MODULE_MAP
