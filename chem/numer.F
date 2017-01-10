! This file contains the following subroutines, related to interpolations
! of input data, addition of points to arrays, and zeroing of arrays:
!     inter1
!     inter2
!     inter3
!     inter4
!     addpnt
!     zero1
!     zero2
!=============================================================================*

      SUBROUTINE inter1(ng,xg,yg, n,x,y)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Map input data given on single, discrete points, onto a discrete target  =*
!=  grid.                                                                    =*
!=  The original input data are given on single, discrete points of an       =*
!=  arbitrary grid and are being linearly interpolated onto a specified      =*
!=  discrete target grid.  A typical example would be the re-gridding of a   =*
!=  given data set for the vertical temperature profile to match the speci-  =*
!=  fied altitude grid.                                                      =*
!=  Some caution should be used near the end points of the grids.  If the    =*
!=  input data set does not span the range of the target grid, the remaining =*
!=  points will be set to zero, as extrapolation is not permitted.           =*
!=  If the input data does not encompass the target grid, use ADDPNT to      =*
!=  expand the input array.                                                  =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  NG  - INTEGER, number of points in the target grid                    (I)=*
!=  XG  - REAL, target grid (e.g. altitude grid)                          (I)=*
!=  YG  - REAL, y-data re-gridded onto XG                                 (O)=*
!=  N   - INTEGER, number of points in the input data set                 (I)=*
!=  X   - REAL, grid on which input data are defined                      (I)=*
!=  Y   - REAL, input y-data                                              (I)=*
!-----------------------------------------------------------------------------*

      IMPLICIT NONE

! input:
      INTEGER n, ng
      REAL xg(ng)
      REAL x(n), y(n)

! output:
      REAL yg(ng)

! local:
      REAL slope
      INTEGER jsave, i, j

      jsave = 1
      DO i = 1, ng
         yg(i) = 0.
         j = jsave
   10    CONTINUE
         IF ((x(j) .GT. xg(i)) .OR. (xg(i) .GE. x(j+1))) THEN
            j = j+1
            IF (j .LE. n-1) GOTO 10
!        ---- end of loop 10 ----
         ELSE
            slope = (y(j+1)-y(j)) / (x(j+1)-x(j))
            yg(i) = y(j) + slope * (xg(i) - x(j))
            jsave = j
         ENDIF
      ENDDO

      END SUBROUTINE inter1

!=============================================================================*

      SUBROUTINE inter2(ng,xg,yg,n,x,y,ierr)

!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Map input data given on single, discrete points onto a set of target     =*
!=  bins.                                                                    =*
!=  The original input data are given on single, discrete points of an       =*
!=  arbitrary grid and are being linearly interpolated onto a specified set  =*
!=  of target bins.  In general, this is the case for most of the weighting  =*
!=  functions (action spectra, molecular cross section, and quantum yield    =*
!=  data), which have to be matched onto the specified wavelength intervals. =*
!=  The average value in each target bin is found by averaging the trapezoi- =*
!=  dal area underneath the input data curve (constructed by linearly connec-=*
!=  ting the discrete input values).                                         =*
!=  Some caution should be used near the endpoints of the grids.  If the     =*
!=  input data set does not span the range of the target grid, an error      =*
!=  message is printed and the execution is stopped, as extrapolation of the =*
!=  data is not permitted.                                                   =*
!=  If the input data does not encompass the target grid, use ADDPNT to      =*
!=  expand the input array.                                                  =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  NG  - INTEGER, number of bins + 1 in the target grid                  (I)=*
!=  XG  - REAL, target grid (e.g., wavelength grid);  bin i is defined    (I)=*
!=        as [XG(i),XG(i+1)] (i = 1..NG-1)                                   =*
!=  YG  - REAL, y-data re-gridded onto XG, YG(i) specifies the value for  (O)=*
!=        bin i (i = 1..NG-1)                                                =*
!=  N   - INTEGER, number of points in input grid                         (I)=*
!=  X   - REAL, grid on which input data are defined                      (I)=*
!=  Y   - REAL, input y-data                                              (I)=*
!-----------------------------------------------------------------------------*

      IMPLICIT NONE

! input:
      INTEGER ng, n
      REAL x(n), y(n), xg(ng)

! output:
      REAL yg(ng)

! local:
      REAL area, xgl, xgu
      REAL darea, slope
      REAL a1, a2, b1, b2
      INTEGER ngintv
      INTEGER i, k, jstart
      INTEGER ierr

      ierr = 0

!  test for correct ordering of data, by increasing value of x

      DO i = 2, n
        IF (x(i) .LE. x(i-1)) THEN
          ierr = 1
          call wrf_debug( 0,'inter2: ERROR <<< x-grid not sorted' )
          RETURN
        ENDIF
      ENDDO

      DO i = 2, ng
        IF (xg(i) .LE. xg(i-1)) THEN
          ierr = 2
          call wrf_debug( 0,'inter2: ERROR <<< xg-grid not sorted!' )
          RETURN
        ENDIF
      ENDDO

! check for xg-values outside the x-range

      IF ( (x(1) .GT. xg(1)) .OR. (x(n) .LT. xg(ng)) ) THEN
        call wrf_error_fatal( 'inter2: <<<  Data do not span grid; Use ADDPNT to expand data and re-run.' )
      ENDIF

!  find the integral of each grid interval and use this to 
!  calculate the average y value for the interval      
!  xgl and xgu are the lower and upper limits of the grid interval

      jstart = 1
      ngintv = ng - 1
      DO i = 1,ngintv

! initalize:
            area = 0.0
            xgl = xg(i)
            xgu = xg(i+1)

!  discard data before the first grid interval and after the 
!  last grid interval
!  for internal grid intervals, start calculating area by interpolating
!  between the last point which lies in the previous interval and the
!  first point inside the current interval

            k = jstart
            IF (k .LE. n-1) THEN

!  if both points are before the first grid, go to the next point
   30         CONTINUE
              IF (x(k+1) .LE. xgl) THEN
                jstart = k - 1
                k = k+1
                IF (k .LE. n-1) GO TO 30
              ENDIF

!  if the last point is beyond the end of the grid, complete and go to the next
!  grid
   40         CONTINUE
              IF ((k .LE. n-1) .AND. (x(k) .LT. xgu)) THEN          
                jstart = k-1
! compute x-coordinates of increment
                a1 = MAX(x(k),xgl)
                a2 = MIN(x(k+1),xgu)
!  if points coincide, contribution is zero
                IF (x(k+1).EQ.x(k)) THEN
                  darea = 0.e0
                ELSE
                  slope = (y(k+1) - y(k))/(x(k+1) - x(k))
                  b1 = y(k) + slope*(a1 - x(k))
                  b2 = y(k) + slope*(a2 - x(k))
                  darea = (a2 - a1)*(b2 + b1)/2.
                ENDIF

!  find the area under the trapezoid from a1 to a2
                area = area + darea
! go to next point
                k = k+1
                GO TO 40
              ENDIF
            ENDIF

!  calculate the average y after summing the areas in the interval
            yg(i) = area/(xgu - xgl)

      ENDDO

      END SUBROUTINE inter2

!=============================================================================*

      SUBROUTINE inter3(ng,xg,yg, n,x,y, FoldIn)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Map input data given on a set of bins onto a different set of target     =*
!=  bins.                                                                    =*
!=  The input data are given on a set of bins (representing the integral     =*
!=  of the input quantity over the range of each bin) and are being matched  =*
!=  onto another set of bins (target grid).  A typical example would be an   =*
!=  input data set spcifying the extra-terrestrial flux on wavelength inter- =*
!=  vals, that has to be matched onto the working wavelength grid.           =*
!=  The resulting area in a given bin of the target grid is calculated by    =*
!=  simply adding all fractional areas of the input data that cover that     =*
!=  particular target bin.                                                   =*
!=  Some caution should be used near the endpoints of the grids.  If the     =*
!=  input data do not span the full range of the target grid, the area in    =*
!=  the "missing" bins will be assumed to be zero.  If the input data extend =*
!=  beyond the upper limit of the target grid, the user has the option to    =*
!=  integrate the "overhang" data and fold the remaining area back into the  =*
!=  last target bin.  Using this option is recommended when re-gridding      =*
!=  vertical profiles that directly affect the total optical depth of the    =*
!=  model atmosphere.                                                        =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  NG     - INTEGER, number of bins + 1 in the target grid               (I)=*
!=  XG     - REAL, target grid (e.g. working wavelength grid);  bin i     (I)=*
!=           is defined as [XG(i),XG(i+1)] (i = 1..NG-1)                     =*
!=  YG     - REAL, y-data re-gridded onto XG;  YG(i) specifies the        (O)=*
!=           y-value for bin i (i = 1..NG-1)                                 =*
!=  N      - INTEGER, number of bins + 1 in the input grid                (I)=*
!=  X      - REAL, input grid (e.g. data wavelength grid);  bin i is      (I)=*
!=           defined as [X(i),X(i+1)] (i = 1..N-1)                           =*
!=  Y      - REAL, input y-data on grid X;  Y(i) specifies the            (I)=*
!=           y-value for bin i (i = 1..N-1)                                  =*
!=  FoldIn - Switch for folding option of "overhang" data                 (I)=*
!=           FoldIn = 0 -> No folding of "overhang" data                     =*
!=           FoldIn = 1 -> Integerate "overhang" data and fold back into     =*
!=                         last target bin                                   =*
!-----------------------------------------------------------------------------*

      IMPLICIT NONE
      
! input:
      INTEGER n, ng
      REAL xg(ng)
      REAL x(n), y(n)

      INTEGER FoldIn

! output:
      REAL yg(ng)

! local:
      REAL a1, a2, sum
      REAL tail
      INTEGER jstart, i, j, k

! check whether flag given is legal
      IF ((FoldIn .NE. 0) .AND. (FoldIn .NE. 1)) THEN
         call wrf_error_fatal( 'inter3: ERROR <<<  Value for FOLDIN invalid. Must be 0 or 1' )
      ENDIF

! do interpolation

      jstart = 1
      DO i = 1, ng - 1
         yg(i) = 0.
         sum = 0.
         j = jstart
         IF (j .LE. n-1) THEN
   20      CONTINUE

           IF (x(j+1) .LT. xg(i)) THEN
              jstart = j
              j = j+1
              IF (j .LE. n-1) GO TO 20
           ENDIF               

   25      CONTINUE

           IF ((x(j) .LE. xg(i+1)) .AND. (j .LE. n-1)) THEN
              a1 = MAX(x(j),xg(i))
              a2 = MIN(x(j+1),xg(i+1))
              sum = sum + y(j) * (a2-a1)/(x(j+1)-x(j))
              j = j+1
              GO TO 25
           ENDIF
           yg(i) = sum 
         ENDIF
      ENDDO
      

! if wanted, integrate data "overhang" and fold back into last bin

      IF (FoldIn .EQ. 1) THEN
         j = j-1
         a1 = xg(ng)            ! upper limit of last interpolated bin
         a2 = x(j+1)            ! upper limit of last input bin considered
!        do folding only if grids don't match up and there is more input 
         IF ((a2 .GT. a1) .OR. (j+1 .LT. n)) THEN
            tail = y(j) * (a2-a1)/(x(j+1)-x(j))
            DO k = j+1, n-1
               tail = tail + y(k) * (x(k+1)-x(k))
            ENDDO
            yg(ng-1) = yg(ng-1) + tail
         ENDIF
      ENDIF

      END SUBROUTINE inter3

!=============================================================================*

      SUBROUTINE inter4(ng,xg,yg, n,x,y, FoldIn)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Map input data given on a set of bins onto a different set of target     =*
!=  bins.                                                                    =*
!=  The input data are given on a set of bins (representing the integral     =*
!=  of the input quantity over the range of each bin) and are being matched  =*
!=  onto another set of bins (target grid).  A typical example would be an   =*
!=  input data set spcifying the extra-terrestrial flux on wavelength inter- =*
!=  vals, that has to be matched onto the working wavelength grid.           =*
!=  The resulting area in a given bin of the target grid is calculated by    =*
!=  simply adding all fractional areas of the input data that cover that     =*
!=  particular target bin.                                                   =*
!=  Some caution should be used near the endpoints of the grids.  If the     =*
!=  input data do not span the full range of the target grid, the area in    =*
!=  the "missing" bins will be assumed to be zero.  If the input data extend =*
!=  beyond the upper limit of the target grid, the user has the option to    =*
!=  integrate the "overhang" data and fold the remaining area back into the  =*
!=  last target bin.  Using this option is recommended when re-gridding      =*
!=  vertical profiles that directly affect the total optical depth of the    =*
!=  model atmosphere.                                                        =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  NG     - INTEGER, number of bins + 1 in the target grid               (I)=*
!=  XG     - REAL, target grid (e.g. working wavelength grid);  bin i     (I)=*
!=           is defined as [XG(i),XG(i+1)] (i = 1..NG-1)                     =*
!=  YG     - REAL, y-data re-gridded onto XG;  YG(i) specifies the        (O)=*
!=           y-value for bin i (i = 1..NG-1)                                 =*
!=  N      - INTEGER, number of bins + 1 in the input grid                (I)=*
!=  X      - REAL, input grid (e.g. data wavelength grid);  bin i is      (I)=*
!=           defined as [X(i),X(i+1)] (i = 1..N-1)                           =*
!=  Y      - REAL, input y-data on grid X;  Y(i) specifies the            (I)=*
!=           y-value for bin i (i = 1..N-1)                                  =*
!=  FoldIn - Switch for folding option of "overhang" data                 (I)=*
!=           FoldIn = 0 -> No folding of "overhang" data                     =*
!=           FoldIn = 1 -> Integerate "overhang" data and fold back into     =*
!=                         last target bin                                   =*
!-----------------------------------------------------------------------------*

      IMPLICIT NONE
      
! input:
      INTEGER n, ng
      REAL xg(ng)
      REAL x(n), y(n)

      INTEGER FoldIn

! output:
      REAL yg(ng)

! local:
      REAL a1, a2, sum
      REAL tail
      INTEGER jstart, i, j, k

! check whether flag given is legal
      IF ((FoldIn .NE. 0) .AND. (FoldIn .NE. 1)) THEN
         call wrf_error_fatal( 'inter3: ERROR <<<  Value for FOLDIN invalid. Must be 0 or 1' )
      ENDIF

! do interpolation

      jstart = 1
      DO i = 1, ng - 1
         yg(i) = 0.
         sum = 0.
         j = jstart
         IF (j .LE. n-1) THEN
   20      CONTINUE
             IF (x(j+1) .LT. xg(i)) THEN
                jstart = j
                j = j+1
                IF (j .LE. n-1) GO TO 20
             ENDIF               
   25      CONTINUE
           IF ((x(j) .LE. xg(i+1)) .AND. (j .LE. n-1)) THEN
              a1 = MAX(x(j),xg(i))
              a2 = MIN(x(j+1),xg(i+1))
              sum = sum + y(j) * (a2-a1)
              j = j+1
              GO TO 25
           ENDIF
           yg(i) = sum /(xg(i+1)-xg(i))
        ENDIF
      ENDDO


! if wanted, integrate data "overhang" and fold back into last bin

      IF (FoldIn .EQ. 1) THEN
         j = j-1
         a1 = xg(ng)     ! upper limit of last interpolated bin
         a2 = x(j+1)     ! upper limit of last input bin considered
!        do folding only if grids don't match up and there is more input 
         IF ((a2 .GT. a1) .OR. (j+1 .LT. n)) THEN
           tail = y(j) * (a2-a1)/(x(j+1)-x(j))
           DO k = j+1, n-1
              tail = tail + y(k) * (x(k+1)-x(k))
           ENDDO
           yg(ng-1) = yg(ng-1) + tail
         ENDIF
      ENDIF

      END SUBROUTINE inter4

!=============================================================================*

      SUBROUTINE addpnt ( x, y, ld, n, xnew, ynew )
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Add a point <xnew,ynew> to a set of data pairs <x,y>.  x must be in      =*
!=  ascending order                                                          =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  X    - REAL vector of length LD, x-coordinates                       (IO)=*
!=  Y    - REAL vector of length LD, y-values                            (IO)=*
!=  LD   - INTEGER, dimension of X, Y exactly as declared in the calling  (I)=*
!=         program                                                           =*
!=  N    - INTEGER, number of elements in X, Y.  On entry, it must be:   (IO)=*
!=         N < LD.  On exit, N is incremented by 1.                          =*
!=  XNEW - REAL, x-coordinate at which point is to be added               (I)=*
!=  YNEW - REAL, y-value of point to be added                             (I)=*
!-----------------------------------------------------------------------------*

      IMPLICIT NONE

! calling parameters

      INTEGER, intent(in) :: ld
      INTEGER, intent(inout) :: n
      REAL, intent(inout)    :: x(ld), y(ld)
      REAL, intent(in) :: xnew, ynew

! local variables

      INTEGER insert
      INTEGER i
      CHARACTER(len=256) :: emsg

! check n<ld to make sure x will hold another point
      IF (n .GE. ld) THEN
         call wrf_error_fatal( 'addpnt: ERROR <<<  Cannot expand array All elements used.' )
      ENDIF

      insert = 1
      i = 2

! check whether x is already sorted.
! also, use this loop to find the point at which xnew needs to be inserted
! into vector x, if x is sorted.

 10   CONTINUE
      IF (i .LT. n) THEN
        IF (x(i) .LT. x(i-1)) THEN
           call wrf_error_fatal( 'addpnt: ERROR <<<  x-data must be in ascending order!' )
        ELSE
           IF (xnew .GT. x(i-1)) insert = i 
        ENDIF
        i = i+1
        GOTO 10
      ENDIF

! if <xnew,ynew> needs to be appended at the end, just do so,
! otherwise, insert <xnew,ynew> at position INSERT

      IF ( xnew .GT. x(n) ) THEN
         x(n+1) = xnew
         y(n+1) = ynew
      ELSE
! shift all existing points one index up
         DO i = n, insert, -1
           x(i+1) = x(i)
           y(i+1) = y(i)
         ENDDO
! insert new point
         x(insert) = xnew
         y(insert) = ynew
      ENDIF

! increase total number of elements in x, y

      n = n+1

      END SUBROUTINE addpnt
