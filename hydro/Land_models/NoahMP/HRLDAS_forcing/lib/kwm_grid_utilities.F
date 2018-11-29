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
       val = -1.E36
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
    integer :: I, J, II, JJ, L, LL
    real    :: XX, YY, A, B, C, D, E, F, G, H
    integer :: number_good  ! Barlage

!KWM    if ((X < 1) .or. (Y < 1) .or. (X > IX) .or. (Y > JX)) then
!KWM       val = -1.E36
!KWM       return
!KWM    endif

    I = int(X)
    J = int(Y)
    XX = X-I
    YY = Y-J

!    IF(ABS(XX) <= 0.0001 .and. ABS(YY) <= 0.0001) THEN
!       val = array(i,j)
!       RETURN
!    ENDIF

    do II = -1, 2
       do JJ = -1, 2
          if ((I+II<1) .or. (I+II>IX).or. (J+JJ<1) .or. (J+JJ>JX)) then
             STL(II+2,JJ+2) = -1.E36
          else
             STL(II+2,JJ+2) = ARRAY(I+II,J+JJ)
          endif
       enddo
    enddo
    
    ! STL = ARRAY( I-1:I+2 , J-1:J+2 )

    ! if (ANY(STL<-1.E25)) then
    !    val = -1.E36
    !    return
    ! endif
    
    ! Corner points missing are OK.  Any other points missing and we have a problem.
    if ((ANY(STL(:,2:3)<-1.E25)) .or. (ANY(STL(2:3,:)<-1.E25))) then
    
    ! Barlage 20150522: if this is violated, send back the average if any points are
    !                   valid, this could prevent problems if close to source boundary
    
      if (ANY(STL > -1.E25)) then
        val = sum(STL, MASK = STL > -1.E25)
        number_good = count(STL > -1.E25)
	val = val / number_good
      else
       val = -1.E36
      end if
      
      return
    endif

!KWM    do JJ = 4, 1, -1
!KWM       print*, (STL(II,JJ),II=1,4)
!KWM    enddo
!KWM    print*, '-----------------------------------'

    A = ONED(XX,STL(1,1),STL(2,1),STL(3,1),STL(4,1))
    B = ONED(XX,STL(1,2),STL(2,2),STL(3,2),STL(4,2))
    C = ONED(XX,STL(1,3),STL(2,3),STL(3,3),STL(4,3))
    D = ONED(XX,STL(1,4),STL(2,4),STL(3,4),STL(4,4))
    val = ONED(YY,A,B,C,D)

    if (ANY(STL<-1.E25)) then

       E = ONED(YY,STL(1,1),STL(1,2),STL(1,3),STL(1,4))
       F = ONED(YY,STL(2,1),STL(2,2),STL(2,3),STL(2,4))
       G = ONED(YY,STL(3,1),STL(3,2),STL(3,3),STL(3,4))
       H = ONED(YY,STL(4,1),STL(4,2),STL(4,3),STL(4,4))
       val = 0.5 * (val + ONED(XX,E,F,G,H))
    endif

    ! Constrain the results to fall between the original min and maximum of the 16 points
    val = min(val, maxval(STL))
    val = max(val, minval(STL))

  end function bint_p

  REAL FUNCTION ONED(X,A,B,C,D) result(val)
    ! Points B and C must have good data.  
    ! Points A and D may have the no-data indication (value less than -1.E25)
    implicit none
    real, intent(in) :: X, A, B, C, D

    if (B < -1.E25) stop "FATAL ERROR: In kwm_grid_utilities.F -- Point B in ONED"
    if (C < -1.E25) stop "FATAL ERROR: In kwm_grid_utilities.F -- Point C in ONED"

    IF (abs(X) < 1.E-5) then
       val = B
    else if (abs(X-1.) < 1.E-5) then
       val = C
    else

       if ((A<-1.E25).and.(D<-1.E25)) then
          ! Points A and D have the no-data flag.
          val = (B*(1.0-X)) + (C*X)
       else if (D<-1.E25) then
          ! No-data flag at point D.  Point A is good (or the previous If-test would have caught it).
          val = B+X*(0.5*(C-A)+X*(0.5*(C+A)-B))
       else if (A<-1.E25) then
          ! No-data flag at point A.  Point D is good (or the earlier If-test would have caught it).
          val = C+(1.0-X)*(0.5*(B-D)+(1.0-X)*(0.5*(B+D)-C))
       else
          ! All four points have good data
          val = (1.0-X)*(B+X*(0.5*(C-A)+X*(0.5*(C+A)-B)))+X*(C+(1.0-X)*(0.5 &
               *(B-D)+(1.0-X)*(0.5*(B+D)-C)))
       endif
       
    endif
  END FUNCTION ONED

  real function four_point(array, ix, jx, x, y) result(val)

!*****************************************************************************!
! Performs a 4-point interpolation to a given (x,y) coordinate in an array.   !
! The X coordinate corresponds to the first dimension of array ARRAY.         !
! The Y coordinate corresponds to the second dimension of array ARRAY.        !
! For points outside the domain, the return value is -1.E36.                  !
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
       val = -1.E36
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

       if (ANY(array(i:ip,j:jp)<-1.E25)) then
          val = -1.E36
          return
       endif


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
! For points outside the domain, the return value is -1.E36.                  !
!*****************************************************************************!

    implicit none
    integer, intent(in) :: ix, jx
    real, pointer, dimension(:,:) :: array
    real, intent(in) :: x, y

    integer :: i, j
    real :: dx, dy, rx, ry

    if ((x > ix) .or. (y > jx) .or. (x < 1) .or. (y < 1)) then
       val = -1.E36
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

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: wt_sixteen_pt_average
   !
   ! Purpose: Weighted average of sixteen surrounding grid point values
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   real function wt_sixteen_pt_average(array, nx, ny, x, y, msgvalin, msgvalout) result(val)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: nx, ny
      real, pointer, dimension(:,:) :: array
      real, intent(in) :: x, y               ! The location to interpolate to
      real, intent(in) :: msgvalin,msgvalout
    
      ! Local variables
      integer :: i, j, ifx, ify
      integer :: arrayistart,arrayiend,arrayjstart,arrayjend
      integer :: subistart,subiend,subjstart,subjend
      real :: sum, sum_weight, pcoef
      real, dimension(4,4) :: distance
      real, dimension(4,4) :: weights
      real, dimension(4,4) :: subset

      ifx = floor(x)
      ify = floor(y)
  
      ! First see whether the point is far enough within the array to 
      !   allow for a sixteen point average. If not, fill with missing values.
      if (ifx < 2 .or. ifx > nx-2 .or. ify < 2 .or. ify > ny-2) then
         subset = msgvalin
	 arrayistart = max(ifx-1,1)
	 arrayiend = min(ifx+2,nx)
	 arrayjstart = max(ify-1,1)
	 arrayjend = min(ify+2,ny)
	 subistart = 1
	 subiend = 4
	 subjstart = 1
	 subjend = 4
         if (ifx < 2)    subistart = 4 - (arrayiend - arrayistart)
         if (ifx > nx-2) subiend   = arrayiend - arrayistart + 1
         if (ify < 2)    subjstart = 4 - (arrayjend - arrayjstart)
         if (ify > ny-2) subjend   = arrayjend - arrayjstart + 1
	 subset(subistart:subiend,subjstart:subjend) = array(arrayistart:arrayiend,arrayjstart:arrayjend)
!         print *, arrayistart,arrayiend,arrayjstart,arrayjend
!	 print *, subistart,subiend,subjstart,subjend
!	 print *, ifx,ify,nx,ny,x,y
!	 stop
      else
         subset(:,:) = array(ifx-1:ifx+2,ify-1:ify+2)
      end if
!	 print *, ifx,ify,nx,ny,x,y
!	 print *, array(ifx-1:ifx+2,ify-1:ify+2)
!	 print *, subset
!	 stop
  
      sum_weight = 0.0
      pcoef = 2.0
      do i=1,4
         do j=1,4
   
            if (subset(i,j) == msgvalin) then
               weights(i,j) = 0.0
            else
               distance(i,j) = sqrt((x-real(ifx-2+i))**2+(y-real(ify-2+j))**2)
               weights(i,j) = max(0., 2.0 - distance(i,j))  ! from WPS
!               weights(i,j) = min(2.0,1.0/(max(0.1,distance(i,j)))**pcoef)  ! IDW looks strange
            end if
    
            sum_weight = sum_weight + weights(i,j)
   
         end do
      end do
      goto 1000
	 print *, ifx,ify,nx,ny,x,y
	 print *, distance(:,4)
	 print *, distance(:,3)
	 print *, distance(:,2)
	 print *, distance(:,1)
	 print *, weights(:,4)
	 print *, weights(:,3)
	 print *, weights(:,2)
	 print *, weights(:,1)
	 print *, subset(:,4)
	 print *, subset(:,3)
	 print *, subset(:,2)
	 print *, subset(:,1)
	 print *, sum_weight
!	 stop
1000 continue  
      if (sum_weight == 0.0) then
         val = msgvalout
      else
         sum = 0.0
         do i=1,4
            do j=1,4
               sum = sum + weights(i,j) * subset(i,j)
            end do
         end do
         val = sum / sum_weight
!	 print *, val, sum_weight
      end if
 
   end function wt_sixteen_pt_average
 

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

end module kwm_grid_utilities
