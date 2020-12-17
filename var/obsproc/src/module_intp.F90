MODULE MODULE_INTP

!------------------------------------------------------------------------------!
! Horizontal (2D) bilinear and vertical (1D) linear interpolation
!
! F. VANDENBERGHE, March 2001
!------------------------------------------------------------------------------!

CONTAINS

!==============================================================================!

      SUBROUTINE  INTPLIN_HORIZ (PVAL, PI, PJ, PFIELD2, KIX, KJX, KCRS)
!------------------------------------------------------------------------------!
!
!                       ROUTINE INTPLIN_HORIZ
!                     ***********************
!
! PURPOSE:
! -------
!      FAST 2D INTERPOLATION OF MM5 2D FIELD AT OBSERVATIONS MM5 INDECES
!
!  METHOD:
!  -------
!       PERFORM A BILINEAR INTERPOLATION OF MM5 GRIDDED 2D FIELD AT OBSERVATION
!       LOCATION DEFINED IN MM5 INDECES
!       ASSUME THAT OBSERVATION LATITUDE AND LONGITUDE HAVE ALREADY BEEN
!       CONVERTED INTO MM5 INDECES
!
!  BE CAREFULL WITH THE MM5 INDEXATION:
!  ------------------------------------
!
!     i (latitude)
!    /|\  /\ k (vertical)
!     |  /
!     | /
!     -----> j (longitude)
!
!
!   INPUT:
!   -----
!      CONST:  PI, PJ        INTERPOLATION LOCATION IN MM5 INDECES
!      ACTIVE: PFIELD2       2D MM5 ARRAY DEFINED ON LATITUDE AND LONGITUDE
!      CONST:  KIX, KJX      DIMENSION OF THE MM5 2D ARRAY TO INTERPOLATE
!      CONST:  KCRS          FLAG = 1 WHEN PFIELD2 IS DEFINED AT MM5 CROSS PT
!                            FLAG = 0 WHEN PFIELD2 IS DEFINED AT MM5 DOT PT
!                            (NOT USED FOR PLUG COMPATIBILITY WITH BINT)
!   OUTPUT:
!   ------
!      ACTIVE: PVAL          INTERPOLATED VALUE OF PFIELD2 AT (PI,PJ)
!
!
!   COMMON:                      NO
!   -------
!   EXTERNAL:                    NO
!   --------
!   REFERENCES:                  NO
!   ----------
!
!   MODIFICATIONS:
!   --------------
!       ORIGINAL :  98-07 (F. VANDENBERGHE)
!       ADDITIONS : 98-11 Norm DOCTOR (F. VANDENBERGHE)
!------------------------------------------------------------------------------!
      IMPLICIT NONE

      REAL    :: PVAL
      REAL    :: PI, PJ
      INTEGER :: KIX, KJX
      REAL, DIMENSION (KIX,KJX) :: PFIELD2
      INTEGER :: KCRS

      INTEGER :: II, IJ
      REAL    :: ZDI, ZDJ, ZDIM, ZDJM

!------------------------------------------------------------------------------C


! 1.  MODEL HORIZONTAL INDECES
! ============================

      II = IFIX (PI)
      IJ = IFIX (PJ)


      IF ((1 .LE. II) .AND. (II .LE. KIX-1)  .AND. &
          (1 .LE. IJ) .AND. (IJ .LE. KJX-1)) THEN


! 2.  HORIZONTAL INTERPOLATION COEFICIENTS
! ========================================
!
!     I+1,J |          | I+1,J+1 
!         --+----------+---          
!           |          | /\
!           |          | DIM
!           |          | \/
!           +PI  *     + --
!           |          | /\
!           |          | DI
!           |    PJ    | \/
!         --+----+-----+---          
!       I,J |<DJ>|<DJM>| I,J+1


      ZDI  = PI - FLOAT (II)
      ZDJ  = PJ - FLOAT (IJ)

      ZDIM = 1. - ZDI
      ZDJM = 1. - ZDJ


! 3. 2D FIELD HORIZONTAL BILINEAIRE INTERPOLATION
! ===============================================

         PVAL =   ZDJM*( ZDIM * PFIELD2 (II,  IJ  )  &
                        +ZDI  * PFIELD2 (II+1,IJ  )) &
                + ZDJ *( ZDIM * PFIELD2 (II,  IJ+1)  &
                        +ZDI  * PFIELD2 (II+1,IJ+1))



      ELSE

! 4.  ERROR PROCESSING
! ====================

         WRITE (0,'(/,A)')            ' Warning in sub. intplin_horiz:'
         WRITE (0,'(A,F7.3,A,I3,A)')  ' yi = ',pi,' outside [1 ',kix,'] or'
         WRITE (0,'(A,F7.3,A,I3,A)')  ' xj = ',pj,' outside [1 ',kjx,']'
         WRITE (0,'(A,/)')            ' No interpolation was performed'
!        STOP                         ' in intplin_horiz.F90'
         PVAL = 0. 
      ENDIF


! 5. INTERPOLATION IS DONE EXIT
! =============================

      RETURN
      END SUBROUTINE INTPLIN_HORIZ

      SUBROUTINE INTPLIN_VERTI (PVAL, PZK, PROFILE, KKX)

!------------------------------------------------------------------------------C
      IMPLICIT NONE

      REAL,                     INTENT (OUT)   :: PVAL
      REAL,                     INTENT (IN )   :: PZK
      INTEGER,                  INTENT (IN)    :: KKX
      REAL,    DIMENSION (KKX), INTENT (IN)    :: PROFILE
!
      REAL :: ZF_ABOVE, ZF_BELOW, ZDZ_ABOVE
      INTEGER IK
!------------------------------------------------------------------------------C
!
! 1. PRE-PROCESSING
! =================

! 1.1 VERTICAL INDEX
!     --------------
!
      IK = IFIX (PZK)
!
! 1.2 IF POINT IS ABOVE THE MODEL LID, TAKE THE VALUE AT LID AND EXIT
!     -------------------------------------------------------------
!
      IF (IK .LT. 1) THEN
          PVAL = PROFILE (1)
          WRITE (0,'(A,F6.3)') ' Point above model lid z = ',PZK
          RETURN
       ENDIF

! 1.3 IF POINT IS ON THE MODEL SURFACE, TAKE SURFACE VALUE AND EXIT
!     -------------------------------------------------------------
!
      IF (IK .GE. KKX) THEN
          PVAL = PROFILE (KKX)
          WRITE (0,'(A,F6.3)') ' Point below model surface z = ',PZK
          RETURN
      ENDIF
!
! 1.4 VALUE AT LEVEL JUST ABOVE AND BELOW INTERPOLATION POINT
!     -------------------------------------------------------
!
      ZF_ABOVE = PROFILE (IK)
      ZF_BELOW = PROFILE (IK+1)
!
!
! 2.  VERTICAL INTERPOLATION
! ==========================
!
! 2.1 DISTANCE BETWEEN POINT AND LEVEL ABOVE
!     --------------------------------------
!
      ZDZ_ABOVE  =   PZK  - REAL (IK)
!
!
! 2.2 3D FIELD VERTICAL LINEAR INTERPOLATION
!     --------------------------------------
!
!           (F (IK+1) - F (IK)) 
!     VAL = ------------------- * DZ + F (IK) 
!               IK+1 - IK
!
      PVAL = (ZF_BELOW - ZF_ABOVE) * ZDZ_ABOVE + ZF_ABOVE
!
!
!
! 3.  END   
! =======
      RETURN
      END SUBROUTINE  INTPLIN_VERTI
!==============================================================================!

FUNCTION intplin (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (UNIT = 0, FMT = '(A)' ) ' ERROR arrays must have same size'
        STOP 'in intplin.F'
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = (xx (jl+1) - x) * yy (jl) + (x - xx (jl)) * yy (jl+1)
        val = val / (xx (jl+1) - xx (jl))
    ENDIF

END FUNCTION intplin

FUNCTION intplog (x,xx,yy) RESULT (val)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx, yy
    REAL                :: x
    REAL                :: val

    INTEGER             :: n,m,jl
!------------------------------------------------------------------------------!

    n = size (xx)
    m = size (yy)

    IF (n .NE. m) THEN
        WRITE (UNIT = 0, FMT = '(A)' ) ' ERROR arrays must have same size'
        STOP 'in intplin.F'
    ENDIF

    jl = locate (x,xx)

    IF (jl .LE. 0) THEN    
        val = yy (1)
    ELSE IF (jl .GE. n) THEN    
        val = yy (n)
    ELSE
        val = log (xx (jl+1) / x) * yy (jl) + log (x / xx (jl)) * yy (jl+1)
        val = val / log (xx (jl+1) / xx (jl))
    ENDIF

END FUNCTION intplog

FUNCTION locate (x,xx) RESULT (index)
!------------------------------------------------------------------------------!
    IMPLICIT NONE

    REAL, DIMENSION (:) :: xx
    REAL                :: x
    INTEGER             :: index

    INTEGER             :: n,jl,jm,ju
    LOGICAL             :: ascnd
!------------------------------------------------------------------------------!

    n = size (xx)
    ascnd = (xx (n) >= xx (1))   ! True if ascending order, false otherwise
    jl = 0                       ! Initialize lower limit
    ju = n+1                     ! Initialize upper limit

    DO

       IF (ju-jl <= 1) EXIT      ! Repeat until this condition is satisfied

       jm = (ju+jl) / 2.         ! Compute a mid point

       IF (ascnd .EQV. (x >= xx (jm))) THEN
           jl = jm               ! Replace mid point by lower limit
       ELSE
           ju = jm               ! Replace mid point by upper limit
       ENDIF

    ENDDO

    IF (x .EQ. xx (1)) THEN      ! Set the output, being carefull with endpoints
        index = 1
    ELSE IF (x .EQ. xx (n)) THEN
        index = n-1 
    ELSE
        index = jl
    ENDIF
END FUNCTION LOCATE

END MODULE MODULE_INTP
