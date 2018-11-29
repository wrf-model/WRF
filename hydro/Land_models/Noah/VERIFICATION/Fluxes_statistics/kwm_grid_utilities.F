!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

module kwm_grid_utilities
  private :: oned
contains

  real function bint(array, ix, jx, x, y) result(val)
    implicit none
!
!  Overlapping parabolic interpolation of array to point x, y.
!
    integer, intent(in)                   :: ix, jx
    real   , intent(in), dimension(ix,jx) :: array
    real   , intent(in)                   :: x, y

    real, dimension(4,4) :: STL
    integer :: I, J, K, KK, L, LL
    real    :: XX, YY, A, B, C, D, E, F, G, H

    if ((X < 2) .or. (Y < 2) .or. (X > IX-1) .or. (Y > JX-1)) then
       val = -1.E33
       return
    endif

    I = int(X)
    J = int(Y)
    XX = X-I
    YY = Y-J
!    IF(ABS(XX) <= 0.0001 .and. ABS(YY) <= 0.0001) THEN
!       val = array(i,j)
!       RETURN
!    ENDIF

    STL = ARRAY( I-1:I+2 , J-1:J+2 )

    A = ONED(XX,STL(1,1),STL(2,1),STL(3,1),STL(4,1))
    B = ONED(XX,STL(1,2),STL(2,2),STL(3,2),STL(4,2))
    C = ONED(XX,STL(1,3),STL(2,3),STL(3,3),STL(4,3))
    D = ONED(XX,STL(1,4),STL(2,4),STL(3,4),STL(4,4))
    val = ONED(YY,A,B,C,D)

  end function bint

  real function bint_p(array, ix, jx, x, y) result(val)
    implicit none
!
!  Overlapping parabolic interpolation of array to point x, y.
!
    integer, intent(in)                            :: ix, jx
    real   , pointer, dimension(:,:)               :: array
    real   , intent(in)                            :: x, y

    real, dimension(4,4) :: STL
    integer :: I, J, K, KK, L, LL
    real    :: XX, YY, A, B, C, D, E, F, G, H

    if ((X < 2) .or. (Y < 2) .or. (X > IX-1) .or. (Y > JX-1)) then
       val = -1.E33
       return
    endif

    I = int(X)
    J = int(Y)
    XX = X-I
    YY = Y-J
!    IF(ABS(XX) <= 0.0001 .and. ABS(YY) <= 0.0001) THEN
!       val = array(i,j)
!       RETURN
!    ENDIF

    STL = ARRAY( I-1:I+2 , J-1:J+2 )

    A = ONED(XX,STL(1,1),STL(2,1),STL(3,1),STL(4,1))
    B = ONED(XX,STL(1,2),STL(2,2),STL(3,2),STL(4,2))
    C = ONED(XX,STL(1,3),STL(2,3),STL(3,3),STL(4,3))
    D = ONED(XX,STL(1,4),STL(2,4),STL(3,4),STL(4,4))
    val = ONED(YY,A,B,C,D)

  end function bint_p

  REAL FUNCTION ONED(X,A,B,C,D) result(val)
    implicit none
    real, intent(in) :: X, A, B, C, D
    IF (abs(X) < 1.E-5) then
       val = B
    else if (abs(X-1.) < 1.E-5) then
       val = C
    else
       val = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*(0.5 &
            *(B-D)+(1.0-X)*(0.5*(B+D)-C)))
    endif
  END FUNCTION ONED

  real function four_point(array, ix, jx, x, y) result(val)

!*****************************************************************************!
! Performs a 4-point interpolation to a given (x,y) coordinate in an array.   !
! The X coordinate corresponds to the first dimension of array ARRAY.         !
! The Y coordinate corresponds to the second dimension of array ARRAY.        !
! For points outside the domain, the return value is -1.E33.                  !
!*****************************************************************************!

    implicit none
    integer, intent(in) :: ix, jx
    real, intent(in), dimension(ix,jx) :: array
    real, intent(in) :: x, y

    integer :: i, j, ip, jp
    real :: dx, dy, rx, ry

    if (((x-ix)>1.E-4) .or. ((y-jx)>1.E-4) .or. (x < 0.99999) .or. (y < 0.99999)) then
       if((x-ix)>1.E-5)  print*, 'x, ix = ', x, ix, ((x-ix)>1.E-5), (x-ix)
       if ( y < 0.99999) print*, 'y = ', y
       val = -1.E33
    else
       i = int(x+1.E-5)
       j = int(y+1.E-5)

       ! The following MAX test should be safe, since we've already checked
       ! that we're not too much less than 1.0
       i = max(i, 1)
       j = max(j, 1)

       dx = x-i
       dy = y-j
       if (dx < 1.E-4) dx = 0.0
       if (dx > 0.9999) dx = 1.0
       if (dy < 1.E-4) dy = 0.0
       if (dy > 0.9999) dy = 1.0
       rx = 1.0 - dx
       ry = 1.0 - dy

       ! The following MIN test should be safe, since we've already
       ip = min(i+1, ix)
       jp = min(j+1, jx)

!KWM       print*, 'ip, jp = ', ip, jp
!KWM       print*, 'i, j = ', i, j
!KWM       print*, 'rx, ry, dx, dy = ', rx, ry, dx, dy

       val= array(i ,j )*RY*RX + &
            array(ip,j )*RY*DX + &
            array(i ,jp)*DY*RX + &
            array(ip,jp)*DY*DX
    endif

  end function four_point



  real function four_point_p(array, ix, jx, x, y) result(val)

!*****************************************************************************!
! Performs a 4-point interpolation to a given (x,y) coordinate in an array.   !
! The X coordinate corresponds to the first dimension of array ARRAY.         !
! The Y coordinate corresponds to the second dimension of array ARRAY.        !
! For points outside the domain, the return value is -1.E33.                  !
!*****************************************************************************!

    implicit none
    integer, intent(in) :: ix, jx
    real, pointer, dimension(:,:) :: array
    real, intent(in) :: x, y

    integer :: i, j
    real :: dx, dy, rx, ry

    if ((x > ix) .or. (y > jx) .or. (x < 1) .or. (y < 1)) then
       val = -1.E33
    else
       i = int(x)
       j = int(y)
       dx = x-i
       dy = y-j
       rx = 1.0 - dx
       ry = 1.0 - dy

       val= array(i  ,j  )*RY*RX + &
            array(i+1,j  )*RY*DX + &
            array(i  ,j+1)*DY*RX + &
            array(i+1,j+1)*DY*DX
    endif

  end function four_point_p


  subroutine smdsm(FLD, ix, jx, npass)

!*****************************************************************************!
!                                                                             !
!  Purpose: Smooth the 2-d field FLD with the 2-pass smoother/desmoother      !
!           Watch it.  All values of FLD are assumed to be valid, that is,    !
!           no stagger is assumed.                                            !
!                                                                             !
!   On Entry:  FLD(IX,JX):  2-d field to be smoothed.                         !
!              IX, JX    :  Dimensions of 2-d field FLD.                      !
!              NPASS     :  Optional number of passes of the two-pass smoother!
!                           (default 1 pass of the two-pass smoother)         !
!                                                                             !
!   On Exit:   FLD(IX,JX):  The smoothed 2-d field.                           !
!                                                                             !
!*****************************************************************************!

    implicit none

    INTEGER, intent(in) :: IX, JX
    REAL, intent(inout), dimension(ix,jx) :: FLD
    integer, intent(in), optional :: npass

!  XNU(N) : Smoothing coefficient for pass N
    REAL, parameter, dimension(2) :: XNU = (/ 0.50, -0.52 /)
    INTEGER :: I, J, N, NP, IND
    REAL :: ASV, APLUS, CELL

    if (present(npass)) then
       np = npass
    else
       np = 1
    endif

    NUMPASS : do n = 1, np

       DO IND = 1, 2

!...  FIRST, SMOOTH IN THE IX DIRECTION
          DO I = 2,IX-1
             ASV = FLD(I,1)
             DO J = 2,JX-1
                APLUS = FLD(I,J+1)
                CELL = FLD(I,J)
                FLD(I,J) = FLD(I,J) + XNU(IND)*((ASV + APLUS)/2.0 - FLD(I,J))
                ASV = CELL
             ENDDO
          ENDDO

!...  NOW, SMOOTH IN THE JX DIRECTION
          DO J = 2,JX-1
             ASV = FLD(1,J)
             DO I = 2,IX-1
                APLUS = FLD(I+1,J)
                CELL = FLD(I,J)
                FLD(I,J) = FLD(I,J) + XNU(IND)*((ASV + APLUS)/2.0 - FLD(I,J))
                ASV = CELL
             ENDDO
          ENDDO

       ENDDO

    enddo NUMPASS

  END subroutine smdsm


  subroutine smt121(FLD, ix, jx, npass)

!*****************************************************************************!
!                                                                             !
!   Purpose :  Performs 1-2-1 smoothing on a 2-d field FLD.                   !
!              Watch it.  All values of FLD are assumed to be valid, that is, !
!              no stagger is assumed.                                         !
!                                                                             !
!   On entry : FLD(IX,JX): 2-D field to be smoothed.                          !
!                   IX,JX: The dimensions of the field FLD.                   !
!                   NPASS: Optional number of passes (default 1).             !
!                                                                             !
!   On exit :  FLD(IX,JX): The smoothed field.                                !
!                                                                             !
!   The final value at any particular grid point is a weighted sum            !
!   of that grid point and the eight immediately surrounding points           !
!   thus:                                                                     !
!                        1 2 1                                                !
!                        2 4 2                                                !
!                        1 2 1                                                !
!                                                                             !
!   At edges, we weight thus:                                                 !
!                        -----                                                !
!                        2 4 2                                                !
!                        1 2 1                                                !
!                                                                             !
!   At corners, we weight thus:                                               !
!                        +---                                                 !
!                        |4 2                                                 !
!                        |2 1                                                 !
!                                                                             !
!*****************************************************************************!

    implicit none

    integer, intent(in) :: ix, jx
    REAL, intent(inout), dimension(ix,jx) :: FLD
    integer, intent(in), optional :: npass

    integer :: i, j, n, np
    real :: cell, aplus, asv

    if (present(npass)) then
       np = npass
    else
       np = 1
    endif


    NUMPASS : do n = 1, np

       DO I = 1, IX
          ASV = FLD(I,1)
          FLD(I,1) = (2.*FLD(I,1) + FLD(I,2))/3.
          DO J = 2, JX-1
             APLUS = FLD(I,J+1)
             CELL = FLD(I,J)
             FLD(I,J) = 0.5*FLD(I,J) + 0.25*(ASV+APLUS)
             ASV = CELL
          ENDDO
          FLD(I,JX) = (2.*FLD(I,JX) + ASV)/3.
       ENDDO

       DO J=1,JX
          ASV = FLD(1,J)
          FLD(1,J) = (2.*FLD(1,J) + FLD(2,J))/3.
          DO I=2,IX-1
             APLUS = FLD(I+1,J)
             CELL = FLD(I,J)
             FLD(I,J) = 0.5*FLD(I,J) + 0.25*(ASV+APLUS)
             ASV = CELL
          ENDDO
          FLD(IX,J) = (2.*FLD(IX,J) + ASV)/3.
       ENDDO

    enddo NUMPASS

  END subroutine smt121

  subroutine gaussian_filter(fld, filt, ix, jx, dxkm, half_width)
!
! Implementation of a gaussian filter.
!
! Purpose:  Return a smoothed version of array FLD in array FILT.
!
    implicit none
    integer, intent(in) :: ix, jx ! Dimensions of input/output arrays
    real, intent(in), dimension(ix,jx) :: fld  ! Input array to be filtered.
    real, intent(out),dimension(ix,jx) :: filt ! Output filtered array.
    real, intent(in) :: dxkm       ! Grid spacing in km
    real, intent(in) :: half_width ! Half_Width in km

    real,    parameter :: pi = 3.14159265358979
    real,    parameter :: twopi = 2.*pi

    integer :: i, j, ii, jj, iii, jjj
    real :: x, y
    real :: sigma, sigsq
    integer :: fsz, hsz
    real :: cval, eval
    real, dimension(ix,jx) :: tmp
    real, allocatable, dimension(:) :: v
    real, allocatable, dimension(:,:) :: k

    sigma =  (half_width/dxkm)
    sigsq = sigma*sigma
    fsz = (nint(8*sigma)*2)-1
    hsz = (fsz+1)/2
    allocate(k(fsz,fsz))
    allocate(v(fsz))

    filt = 0.
    tmp = 0.

! Compute the kernel

! I'd rather do it in one step, going directly to my vector v, 
! but I don't see how to be able to do that cleanly and still
! correct for the error (how much the sum differs from 1.0).

! First I compute the two-dimensional kernel array.
    cval = 1./(twopi*sigsq) 
    do i = 1, fsz
       x = float(hsz-i)
       do j = 1, fsz
          y = float(hsz-j)
          eval = -((x*x)+(y*y))/(2.*sigsq)
          k(i,j) = exp(eval)
       enddo
    enddo
    k = k*cval
! Sum to see how far off of one we are, and divide k/sum
! to correct for that error (i.e., we want to sum to 1.0)
    k = k/sum(k)

! Now figure my vector from the corrected kernel array.
    v = k(1,:) / sqrt(k(1,1))

! Apply the kernel

    do j = 1, jx
       do i = 1, ix
          do ii = 1, fsz
             iii = (i-hsz)+ii
             if (iii < 1) iii = 1
             if (iii > ix) iii = ix
             tmp(i,j) = tmp(i,j) + v(ii)*fld(iii,j)
          enddo
       enddo
    enddo

    do i = 1, ix
       do j = 1, jx
          do jj = 1, fsz
             jjj = (j-hsz)+jj
             if (jjj < 1) jjj = 1
             if (jjj > jx) jjj = jx
             filt(i,j) = filt(i,j) + v(jj)*tmp(i,jjj)
          enddo
       enddo
    enddo

  end subroutine gaussian_filter

  subroutine crs2dot(slab1,slab2,ix,jx)

    ! PURPOSE: Interpolate in horizontal from cross to dot points

    implicit none
    integer, intent(in) :: ix,jx
    real, dimension(ix,jx), intent(in)  :: slab1
    real, dimension(ix,jx), intent(out) :: slab2
    integer :: ie,je,i,j
   
    IE=IX-1
    JE=JX-1
    DO I=2,IE
       DO J=2,JE
          SLAB2(I,J)=0.25*(SLAB1(I,J)+SLAB1(I-1,J)+SLAB1(I,J-1)+SLAB1(I-1,J-1))
       enddo
    enddo
    DO I=2,IE
       SLAB2(I,1)=0.5*(SLAB1(I,1)+SLAB1(I-1,1))
       SLAB2(I,JX)=0.5*(SLAB1(I,JE)+SLAB1(I-1,JE))
    enddo
    DO J=2,JE
       SLAB2(1,J)=0.5*(SLAB1(1,J)+SLAB1(1,J-1))
       SLAB2(IX,J)=0.5*(SLAB1(IE,J)+SLAB1(IE,J-1))
    enddo
    SLAB2(1,1)=SLAB1(1,1)
    SLAB2(1,JX)=SLAB1(1,JE)
    SLAB2(IX,JX)=SLAB1(IE,JE)
    SLAB2(IX,1)=SLAB1(IE,1)
   
  end subroutine crs2dot

!KWM  SUBROUTINE MQD (NOBS, XC, YC, DAT, IX, JX, A1)
!KWM    use kwm_matrix_utilities
!KWM!
!KWM! This routine does your basic, no-frills MQD analysis.  Adapted from
!KWM! the routines in RAWINS and LITTLE_R.
!KWM!
!KWM!
!KWM! MULTIQUADRIC INTERPOLATION BASED ON NUSS AND TITLEY (1994 MWR).
!KWM! THE FREE PARAMETERS ARE LAMBDA, THE SMOOTHING FACTOR AND C, THE
!KWM! MULTIQUADRIC PARAMETER. IF THE PROCEDURE BOMBS, THE MOST LIKELY
!KWM! PROBLEM IS THAT THE VALUE FOR C IS INCORRECT. FOR LONG, NARROW
!KWM! DOMAINS, IT MIGHT BE A PROBLEM.
!KWM!  ON ENTRY:
!KWM!         NOBS      :: Number of observations
!KWM!         DAT(nobs) :: Difference of first guess from obs. ( obs-FG)
!KWM!         XC (nobs) :: 1st coordinate of the observations
!KWM!         YC (nobs) :: 2nd coordinate of the observations
!KWM!         IX        :: 1st dimension of first-guess and analysis arrays
!KWM!         JX        :: 2nd dimension of first-guess and analysis arrays
!KWM!         A1(ix,jx) :: dimension(ix,jx):: first guess; 
!KWM!
!KWM! ON EXIT:
!KWM!         A1(ix,jx) :: Objective analysis
!KWM!
!KWM!  PROGRAMMED BY JIM BRESCH NCAR/UW    12/14/94
!KWM!  Adapted to use blas routines by Kevin W. Manning.
!KWM!  Further adapted, improved efficiency.  
!KWM!  Dropped into kwm modules package, October 2001.
!KWM
!KWM! As far as coordinate values:
!KWM!     XC is the coordinate in the IX dimension,
!KWM!     YC is the coordinate in the JX dimension.
!KWM! regardless of whether I is Y and J is X or I is X and J is Y.
!KWM!
!KWM    implicit none
!KWM
!KWM! First, the subroutine arguments:
!KWM    integer, intent(in)                      :: NOBS
!KWM    integer, intent(in)                      :: IX
!KWM    integer, intent(in)                      :: JX
!KWM    real   , intent(in)   , dimension(nobs)  :: XC
!KWM    real   , intent(in)   , dimension(nobs)  :: YC
!KWM    real   , intent(in)   , dimension(nobs)  :: DAT
!KWM    real   , intent(inout), dimension(ix,jx) :: A1
!KWM
!KWM! Now all the local names:
!KWM    real   , parameter            :: lambda = 0.0025 ! Smoothing Factor
!KWM
!KWM    real   , dimension(ix,nobs)   :: QGI             ! Work space
!KWM    real   , dimension(nobs,nobs) :: QI              ! Work space
!KWM    real   , dimension(ix,jx)     :: A2              ! Work space
!KWM    real,    dimension(ix*nobs)   :: XDUM            ! Work space
!KWM
!KWM    real                          :: C               ! MultiQuadric Parameter
!KWM    real                          :: ERRM
!KWM    integer                       :: I
!KWM    integer                       :: J
!KWM    integer                       :: IG
!KWM    integer                       :: JG
!KWM    IF (NOBS .LT. 3) THEN
!KWM       WRITE(6,*) 'WARNING: NOBS < 3. NO MODIFICATION TO THE FIRST '//&
!KWM            'GUESS FIELD OCCURRED.'
!KWM       RETURN
!KWM    ENDIF
!KWM    C = 0.0008 * (MAX0(IX,JX)) ! MultiQuadric Parameter
!KWM!
!KWM! SET ERRM, THE MEAN ERROR VALUE FOR THE VARIABLE BEING ANALYZED. THE
!KWM! RESULTS ARE NOT TOO SENSITIVE TO THIS PARAMETER, BUT THE VALUES MUST
!KWM! BE SANE.
!KWM    ERRM = 1.0
!KWM
!KWM    A2 = A1
!KWM
!KWM! FILL THE QI MATRIX
!KWM    DO J = 1, NOBS
!KWM       DO I = 1, NOBS
!KWM          QI(I,J) = -1.* SQRT(((XC(J)-XC(I))**2 +&
!KWM               (YC(J)-YC(I))**2)/(C*C)+1.)
!KWM       ENDDO
!KWM    ENDDO
!KWM
!KWM! ACCOUNT FOR OBSERVATIONAL UNCERTAINTY (NUSS AND TITLEY 1994)
!KWM    DO J = 1, NOBS
!KWM       I = J
!KWM       QI(I,J) = QI(I,J) + NOBS*LAMBDA*ERRM
!KWM    ENDDO
!KWM
!KWM! FIND THE INVERSE OF QI
!KWM    call invert(qi, nobs, nobs)
!KWM! QI NOW CONTAINS THE INVERSE OF ITSELF...
!KWM
!KWM    xdum = matmul(Qi, dat)
!KWM
!KWM! FILL THE QGI MATRIX
!KWM    DO JG = 1, JX
!KWM       DO IG = 1, IX
!KWM          DO I = 1, NOBS
!KWM             QGI(IG,I) = -1.* SQRT(((FLOAT(IG)-XC(I))**2 +&
!KWM                  (FLOAT(JG)-YC(I))**2)/(C*C)+1.)
!KWM          ENDDO
!KWM       ENDDO
!KWM! MULTIPLY QI INVERSE AND QGI
!KWM! MULTIPLY THE PRODUCT WITH THE DATA VECTOR
!KWM       A1(1:,JG) = matmul(Qgi, xdum)
!KWM    ENDDO
!KWM! ADD THE FIRST GUESS TO THE ARRAY
!KWM    A1 = A1 + A2
!KWM
!KWM  END SUBROUTINE MQD

end module kwm_grid_utilities
