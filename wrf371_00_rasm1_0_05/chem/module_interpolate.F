module module_interpolate
! Description:
!   Routines for interpolation of data in latitude, longitude and time.
!
! Author: Gathered from a number of places and put into the current format by Jim Edwards
!   
! Modules Used:
! 
! use shr_kind_mod,   only: r8 => shr_kind_r8
! use abortutils,     only: endrun
! use scamMod,        only: single_column
! use cam_logfile,    only: iulog

  implicit none

  private

  integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
! integer,parameter :: r8 = selected_real_kind(8) ! 8 byte real
!
! Public Methods:
!
  public :: interp_type, lininterp, vertinterp, bilin, get_timeinterp_factors
  public :: lininterp_init, lininterp_finish
  type interp_type
     real(r8), pointer :: wgts(:)
     real(r8), pointer :: wgtn(:)
     integer, pointer  :: jjm(:)
     integer, pointer  :: jjp(:)
  end type interp_type
  interface lininterp
     module procedure lininterp_original
     module procedure lininterp_full1d
     module procedure lininterp1d
     module procedure lininterp2d2d
     module procedure lininterp2d1d
     module procedure lininterp3d2d
  end interface
  

contains
  subroutine lininterp_full1d (arrin, yin, nin, arrout, yout, nout)
    integer, intent(in) :: nin, nout
    real(r8), intent(in) :: arrin(nin), yin(nin), yout(nout)
    real(r8), intent(out) :: arrout(nout)
    type (interp_type) :: interp_wgts

    call lininterp_init(yin, nin, yout, nout, 1, interp_wgts)
    call lininterp1d(arrin, nin, arrout, nout, interp_wgts)
    call lininterp_finish(interp_wgts)

  end subroutine lininterp_full1d

  subroutine lininterp_init(yin, nin, yout, nout, extrap_method, interp_wgts, &
       cyclicmin, cyclicmax)
!
! Description:
!   Initialize a variable of type(interp_type) with weights for linear interpolation.
!       this variable can then be used in calls to lininterp1d and lininterp2d.
!   yin is a 1d array of length nin of locations to interpolate from - this array must 
!       be monotonic but can be increasing or decreasing
!   yout is a 1d array of length nout of locations to interpolate to, this array need
!       not be ordered
!   extrap_method determines how to handle yout points beyond the bounds of yin
!       if 0 set values outside output grid to 0 
!       if 1 set to boundary value
!       if 2 set to cyclic boundaries
!         optional values cyclicmin and cyclicmax can be used to set the bounds of the 
!         cyclic mapping - these default to 0 and 360.
!

    integer, intent(in) :: nin
    integer, intent(in) :: nout
    real(r8), intent(in) :: yin(:)           ! input mesh
    real(r8), intent(in) :: yout(:)         ! output mesh
    integer, intent(in) :: extrap_method       ! if 0 set values outside output grid to 0 
                                               ! if 1 set to boundary value
                                               ! if 2 set to cyclic boundaries
    real(r8), intent(in), optional :: cyclicmin, cyclicmax

    type (interp_type), intent(out) :: interp_wgts

    real(r8) :: cmin, cmax
    real(r8) :: extrap
    real(r8) :: dyinwrap
    real(r8) :: ratio
    real(r8) :: avgdyin
    integer :: i, j, icount
    integer :: jj
    real(r8), pointer :: wgts(:)
    real(r8), pointer :: wgtn(:)
    integer, pointer :: jjm(:)
    integer, pointer :: jjp(:)
    logical :: increasing
    character(len=132) :: message
    !
    ! Check validity of input coordinate arrays: must be monotonically increasing,
    ! and have a total of at least 2 elements
    !
    if (nin.lt.2) then
       call wrf_abort  !('LININTERP: Must have at least 2 input points for interpolation')
    end if
    if(present(cyclicmin)) then
       cmin=cyclicmin
    else
       cmin=0_r8
    end if
    if(present(cyclicmax)) then
       cmax=cyclicmax
    else
       cmax=360_r8
    end if
    if(cmax<=cmin) then
       call wrf_abort  !('LININTERP: cyclic min value must be < max value')
    end if
    increasing=.true.
    icount = 0
    do j=1,nin-1
       if (yin(j).gt.yin(j+1)) icount = icount + 1
    end do
    if(icount.eq.nin-1) then
       increasing = .false.
       icount=0
    endif
    if (icount.gt.0) then
       call wrf_abort  !('LININTERP: Non-monotonic input coordinate array found')
    end if
    allocate(interp_wgts%jjm(nout), &
         interp_wgts%jjp(nout), &
         interp_wgts%wgts(nout), &
         interp_wgts%wgtn(nout))

    jjm => interp_wgts%jjm
    jjp => interp_wgts%jjp
    wgts =>  interp_wgts%wgts
    wgtn =>  interp_wgts%wgtn

    !
    ! Initialize index arrays for later checking
    !
    jjm = 0
    jjp = 0

    extrap = 0.
    if(extrap_method.eq.0) then
       !
       ! For values which extend beyond boundaries, set weights
       ! such that values will be 0.
       !
       do j=1,nout
          if(increasing) then
             if (yout(j).lt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).gt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          else
             if (yout(j).gt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).lt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 0.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          end if
       end do
    else if(extrap_method.eq.1) then
       !
       ! For values which extend beyond boundaries, set weights
       ! such that values will just be copied.
       !
       do j=1,nout
          if(increasing) then
             if (yout(j).le.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).gt.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          else
             if (yout(j).gt.yin(1)) then
                jjm(j) = 1
                jjp(j) = 1
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             else if (yout(j).le.yin(nin)) then
                jjm(j) = nin
                jjp(j) = nin
                wgts(j) = 1.
                wgtn(j) = 0.
                extrap = extrap + 1.
             end if
          end if
       end do
    else if(extrap_method.eq.2) then
       !
       ! For values which extend beyond boundaries, set weights
       ! for circular boundaries 
       !
       dyinwrap = yin(1) + (cmax-cmin) - yin(nin)
       avgdyin = abs(yin(nin)-yin(1))/(nin-1.)
       ratio = dyinwrap/avgdyin
       if (ratio < 0.9 .or. ratio > 1.1) then
          write(message,*) 'Lininterp: Bad dyinwrap value =',dyinwrap,&
               ' avg=', avgdyin, yin(1),yin(nin)
          call wrf_message( trim(message) )
          call wrf_abort  !('interpolate_data')
       end if

       do j=1,nout
          if(increasing) then
             if (yout(j) <= yin(1)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin) - yin(nin))/dyinwrap
             else if (yout(j) > yin(nin)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)+(cmax-cmin)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)-yin(nin))/dyinwrap
             end if
          else
             if (yout(j) > yin(1)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin) - yin(nin))/dyinwrap
             else if (yout(j) <= yin(nin)) then
                jjm(j) = nin
                jjp(j) = 1
                wgts(j) = (yin(1)+(cmax-cmin)-yout(j))/dyinwrap
                wgtn(j) = (yout(j)+(cmax-cmin)-yin(nin))/dyinwrap
             end if

          endif
       end do
    end if

    !
    ! Loop though output indices finding input indices and weights
    !
    if(increasing) then
       do j=1,nout
          do jj=1,nin-1
             if (yout(j).gt.yin(jj) .and. yout(j).le.yin(jj+1)) then
                jjm(j) = jj
                jjp(j) = jj + 1
                wgts(j) = (yin(jj+1)-yout(j))/(yin(jj+1)-yin(jj))
                wgtn(j) = (yout(j)-yin(jj))/(yin(jj+1)-yin(jj))
                exit
             end if
          end do
       end do
    else
       do j=1,nout
          do jj=1,nin-1
             if (yout(j).le.yin(jj) .and. yout(j).gt.yin(jj+1)) then
                jjm(j) = jj
                jjp(j) = jj + 1
                wgts(j) = (yin(jj+1)-yout(j))/(yin(jj+1)-yin(jj))
                wgtn(j) = (yout(j)-yin(jj))/(yin(jj+1)-yin(jj))
                exit
             end if
          end do
       end do
    end if

#ifndef SPMD
    !
    ! Check grid overlap
    !
    extrap = 100.*extrap/real(nout,r8)
    if (extrap.gt.50. ) then
       write(message,*) 'interpolate_data:','yout=',minval(yout),maxval(yout),increasing,nout
       call wrf_message( trim(message) )
       write(message,*) 'interpolate_data:','yin=',yin(1),yin(nin)
       call wrf_message( trim(message) )
       write(message,*) 'interpolate_data:',extrap,' % of output grid will have to be extrapolated'
       call wrf_message( trim(message) )
       call wrf_abort  !('interpolate_data: ')
    end if
#endif

    !
    ! Check that interp/extrap points have been found for all outputs
    !
    icount = 0
    do j=1,nout
       if (jjm(j).eq.0 .or. jjp(j).eq.0) icount = icount + 1
       ratio=wgts(j)+wgtn(j)
       if((ratio<0.9.or.ratio>1.1).and.extrap_method.ne.0) then
          write(message,*) j, wgts(j),wgtn(j),jjm(j),jjp(j), increasing,extrap_method
          call wrf_message( trim(message) )
          call wrf_abort  !('Bad weight computed in LININTERP_init')
       end if
    end do
    if (icount.gt.0) then
       call wrf_abort  !('LININTERP: Point found without interp indices')
    end if

  end subroutine lininterp_init

  subroutine lininterp1d (arrin, n1, arrout, m1, interp_wgts)
    !-----------------------------------------------------------------------
    !
    ! Purpose: Do a linear interpolation from input mesh to output
    !          mesh with weights as set in lininterp_init.
    !
    !
    ! Author: Jim Edwards
    !
    !-----------------------------------------------------------------------
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: n1                 ! number of input latitudes
    integer, intent(in) :: m1                ! number of output latitudes
                                                                                                  
    real(r8), intent(in) :: arrin(n1)    ! input array of values to interpolate
    type(interp_type), intent(in) :: interp_wgts
    real(r8), intent(out) :: arrout(m1) ! interpolated array
                                                                                                  
    !
    ! Local workspace
    !
    integer j                ! latitude indices
    integer, pointer :: jjm(:)
    integer, pointer :: jjp(:)
                                                                                                  
    real(r8), pointer :: wgts(:)
    real(r8), pointer :: wgtn(:)
                                                                                                  
                                                                                                  
    jjm => interp_wgts%jjm
    jjp => interp_wgts%jjp
    wgts =>  interp_wgts%wgts
    wgtn =>  interp_wgts%wgtn
                                                                                                  
    !
    ! Do the interpolation
    !
    do j=1,m1
      arrout(j) = arrin(jjm(j))*wgts(j) + arrin(jjp(j))*wgtn(j)
    end do
                                                                                                  
    return
  end subroutine lininterp1d
                                                                                                  
  subroutine lininterp2d2d(arrin, n1, n2, arrout, m1, m2, wgt1, wgt2)
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: n1, n2, m1, m2
    real(r8), intent(in) :: arrin(n1,n2)    ! input array of values to interpolate
    type(interp_type), intent(in) :: wgt1, wgt2
    real(r8), intent(out) :: arrout(m1,m2) ! interpolated array
    !
    ! locals
    !
    integer i,j                ! indices
    integer, pointer :: iim(:), jjm(:)
    integer, pointer :: iip(:), jjp(:)
                                                                                                  
    real(r8), pointer :: wgts1(:), wgts2(:)
    real(r8), pointer :: wgtn1(:), wgtn2(:)
                                                                                                  
    real(r8) :: arrtmp(n1,m2)
                                                                                                  
                                                                                                  
    jjm => wgt2%jjm
    jjp => wgt2%jjp
    wgts2 => wgt2%wgts
    wgtn2 => wgt2%wgtn
                                                                                                  
    iim => wgt1%jjm
    iip => wgt1%jjp
    wgts1 => wgt1%wgts
    wgtn1 => wgt1%wgtn

    do j=1,m2
      do i=1,n1
        arrtmp(i,j) = arrin(i,jjm(j))*wgts2(j) + arrin(i,jjp(j))*wgtn2(j)
      end do
    end do

    do j=1,m2
      do i=1,m1
        arrout(i,j) = arrtmp(iim(i),j)*wgts1(i) + arrtmp(iip(i),j)*wgtn1(i)
      end do
    end do
                                                                                                  
  end subroutine lininterp2d2d
  subroutine lininterp2d1d(arrin, n1, n2, arrout, m1, wgt1, wgt2, fldname)
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: n1, n2, m1
    real(r8), intent(in) :: arrin(n1,n2)    ! input array of values to interpolate
    type(interp_type), intent(in) :: wgt1, wgt2
    real(r8), intent(out) :: arrout(m1) ! interpolated array
    character(len=*), intent(in), optional :: fldname(:)
    !
    ! locals
    !
    integer i                ! indices
    integer, pointer :: iim(:), jjm(:)
    integer, pointer :: iip(:), jjp(:)
                                                                                                  
    real(r8), pointer :: wgts(:), wgte(:)
    real(r8), pointer :: wgtn(:), wgtw(:)

    jjm => wgt2%jjm
    jjp => wgt2%jjp
    wgts => wgt2%wgts
    wgtn => wgt2%wgtn
                                                                                                  
    iim => wgt1%jjm
    iip => wgt1%jjp
    wgtw => wgt1%wgts
    wgte => wgt1%wgtn

    do i=1,m1
       arrout(i) = arrin(iim(i),jjm(i))*wgtw(i)*wgts(i)+arrin(iip(i),jjm(i))*wgte(i)*wgts(i) + &
                   arrin(iim(i),jjp(i))*wgtw(i)*wgtn(i)+arrin(iip(i),jjp(i))*wgte(i)*wgtn(i)
    end do

                                                                                                  
  end subroutine lininterp2d1d
  subroutine lininterp3d2d(arrin, n1, n2, n3, arrout, m1, len1, wgt1, wgt2)
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: n1, n2, n3, m1, len1   ! m1 is to len1 as ncols is to pcols 
    real(r8), intent(in) :: arrin(n1,n2,n3)    ! input array of values to interpolate
    type(interp_type), intent(in) :: wgt1, wgt2
    real(r8), intent(out) :: arrout(len1, n3) ! interpolated array

    !
    ! locals
    !
    integer i, k               ! indices
    integer, pointer :: iim(:), jjm(:)
    integer, pointer :: iip(:), jjp(:)
                                                                                                  
    real(r8), pointer :: wgts(:), wgte(:)
    real(r8), pointer :: wgtn(:), wgtw(:)

    jjm => wgt2%jjm
    jjp => wgt2%jjp
    wgts => wgt2%wgts
    wgtn => wgt2%wgtn
                                                                                                  
    iim => wgt1%jjm
    iip => wgt1%jjp
    wgtw => wgt1%wgts
    wgte => wgt1%wgtn

    do k=1,n3
       do i=1,m1
          arrout(i,k) = arrin(iim(i),jjm(i),k)*wgtw(i)*wgts(i)+arrin(iip(i),jjm(i),k)*wgte(i)*wgts(i) + &
               arrin(iim(i),jjp(i),k)*wgtw(i)*wgtn(i)+arrin(iip(i),jjp(i),k)*wgte(i)*wgtn(i)
       end do
    end do
                                                                                                  
  end subroutine lininterp3d2d
                                                                                                  



  subroutine lininterp_finish(interp_wgts)
    type(interp_type) :: interp_wgts

    deallocate(interp_wgts%jjm, &
         interp_wgts%jjp, &
         interp_wgts%wgts, &
         interp_wgts%wgtn)

    nullify(interp_wgts%jjm, &
         interp_wgts%jjp, &
         interp_wgts%wgts, &
         interp_wgts%wgtn)
  end subroutine lininterp_finish

  subroutine lininterp_original (arrin, yin, nlev, nlatin, arrout, &
       yout, nlatout)
    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: Do a linear interpolation from input mesh defined by yin to output
    !          mesh defined by yout.  Where extrapolation is necessary, values will
    !          be copied from the extreme edge of the input grid.  Vectorization is over
    !          the vertical (nlev) dimension.
    ! 
    ! Method: Check validity of input, then determine weights, then do the N-S interpolation.
    ! 
    ! Author: Jim Rosinski
    ! Modified: Jim Edwards so that there is no requirement of monotonacity on the yout array
    !
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    !
    ! Arguments
    !
    integer, intent(in) :: nlev                   ! number of vertical levels
    integer, intent(in) :: nlatin                 ! number of input latitudes
    integer, intent(in) :: nlatout                ! number of output latitudes

    real(r8), intent(in) :: arrin(nlev,nlatin)    ! input array of values to interpolate
    real(r8), intent(in) :: yin(nlatin)           ! input mesh
    real(r8), intent(in) :: yout(nlatout)         ! output mesh

    real(r8), intent(out) :: arrout(nlev,nlatout) ! interpolated array
    !
    ! Local workspace
    !
    integer j, jj              ! latitude indices
    integer jjprev             ! latitude indices
    integer k                  ! level index
    integer icount             ! number of values

    real(r8) extrap            ! percent grid non-overlap
    !
    ! Dynamic
    !
    integer :: jjm(nlatout)
    integer :: jjp(nlatout)

    real(r8) :: wgts(nlatout)
    real(r8) :: wgtn(nlatout)
    !
    ! Check validity of input coordinate arrays: must be monotonically increasing,
    ! and have a total of at least 2 elements
    !
    if (nlatin.lt.2) then
       call wrf_abort  !('LININTERP: Must have at least 2 input points for interpolation')
    end if

    icount = 0
    do j=1,nlatin-1
       if (yin(j).gt.yin(j+1)) icount = icount + 1
    end do


    if (icount.gt.0) then
       call wrf_abort  !('LININTERP: Non-monotonic coordinate array(s) found')
    end if
    !
    ! Initialize index arrays for later checking
    !
    do j=1,nlatout
       jjm(j) = 0
       jjp(j) = 0
    end do
    !
    ! For values which extend beyond N and S boundaries, set weights
    ! such that values will just be copied.
    !
    extrap = 0.

    do j=1,nlatout
       if (yout(j).le.yin(1)) then
          jjm(j) = 1
          jjp(j) = 1
          wgts(j) = 1.
          wgtn(j) = 0.
          extrap=extrap+1.
       else if (yout(j).gt.yin(nlatin)) then
          jjm(j) = nlatin
          jjp(j) = nlatin
          wgts(j) = 1.
          wgtn(j) = 0.
          extrap=extrap+1.
       endif
    end do

    !
    ! Loop though output indices finding input indices and weights
    !
    do j=1,nlatout
       do jj=1,nlatin-1
          if (yout(j).gt.yin(jj) .and. yout(j).le.yin(jj+1)) then
             jjm(j) = jj
             jjp(j) = jj + 1
             wgts(j) = (yin(jj+1)-yout(j))/(yin(jj+1)-yin(jj))
             wgtn(j) = (yout(j)-yin(jj))/(yin(jj+1)-yin(jj))
             exit
          end if
       end do
    end do
    !
    ! Check that interp/extrap points have been found for all outputs
    !
    icount = 0
    do j=1,nlatout
       if (jjm(j).eq.0 .or. jjp(j).eq.0) then
          icount = icount + 1
       end if
    end do
    if (icount.gt.0) then
       call wrf_abort  !('LININTERP: Point found without interp indices')
    end if
    !
    ! Do the interpolation
    !
    do j=1,nlatout
       do k=1,nlev
          arrout(k,j) = arrin(k,jjm(j))*wgts(j) + arrin(k,jjp(j))*wgtn(j)
       end do
    end do

    return
  end subroutine lininterp_original


  subroutine bilin (arrin, xin, yin, nlondin, nlonin, &
       nlevdin, nlev, nlatin, arrout, xout, &
       yout, nlondout, nlonout, nlevdout, nlatout)
    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    !
    ! Do a bilinear interpolation from input mesh defined by xin, yin to output
    ! mesh defined by xout, yout.  Circularity is assumed in the x-direction so
    ! input x-grid must be in degrees east and must start from Greenwich.  When
    ! extrapolation is necessary in the N-S direction, values will be copied 
    ! from the extreme edge of the input grid.  Vectorization is over the
    ! longitude dimension.  Input grid is assumed rectangular. Output grid
    ! is assumed ragged, i.e. xout is a 2-d mesh.
    ! 
    ! Method: Interpolate first in longitude, then in latitude.
    ! 
    ! Author: Jim Rosinski
    ! 
    !-----------------------------------------------------------------------
!   use shr_kind_mod, only: r8 => shr_kind_r8
!   use abortutils,   only: endrun
    !-----------------------------------------------------------------------
    implicit none
    !-----------------------------------------------------------------------
    integer,parameter :: r8 = selected_real_kind(12) ! 8 byte real
    !
    ! Input arguments
    !
    integer, intent(in) :: nlondin                        ! longitude dimension of input grid
    integer, intent(in) :: nlonin                         ! number of real longitudes (input)
    integer, intent(in) :: nlevdin                        ! vertical dimension of input grid
    integer, intent(in) :: nlev                           ! number of vertical levels
    integer, intent(in) :: nlatin                         ! number of input latitudes
    integer, intent(in) :: nlatout                        ! number of output latitudes
    integer, intent(in) :: nlondout                       ! longitude dimension of output grid
    integer, intent(in) :: nlonout(nlatout)               ! number of output longitudes per lat
    integer, intent(in) :: nlevdout                       ! vertical dimension of output grid

    real(r8), intent(in) :: arrin(nlondin,nlevdin,nlatin) ! input array of values to interpolate
    real(r8), intent(in) :: xin(nlondin)                  ! input x mesh
    real(r8), intent(in) :: yin(nlatin)                   ! input y mesh
    real(r8), intent(in) :: xout(nlondout,nlatout)        ! output x mesh
    real(r8), intent(in) :: yout(nlatout)                 ! output y mesh
    !
    ! Output arguments
    !
    real(r8), intent(out) :: arrout(nlondout,nlevdout,nlatout) ! interpolated array
    !
    ! Local workspace
    !
    integer :: i, ii, iw, ie, iiprev ! longitude indices
    integer :: j, jj, js, jn, jjprev ! latitude indices
    integer :: k                     ! level index
    integer :: icount                ! number of bad values

    real(r8) :: extrap               ! percent grid non-overlap
    real(r8) :: dxinwrap             ! delta-x on input grid for 2-pi
    real(r8) :: avgdxin              ! avg input delta-x
    real(r8) :: ratio                ! compare dxinwrap to avgdxin
    real(r8) :: sum                  ! sum of weights (used for testing)
    !
    ! Dynamic
    !
    integer :: iim(nlondout)         ! interpolation index to the left
    integer :: iip(nlondout)         ! interpolation index to the right
    integer :: jjm(nlatout)          ! interpolation index to the south
    integer :: jjp(nlatout)          ! interpolation index to the north

    real(r8) :: wgts(nlatout)        ! interpolation weight to the north
    real(r8) :: wgtn(nlatout)        ! interpolation weight to the north
    real(r8) :: wgte(nlondout)       ! interpolation weight to the north
    real(r8) :: wgtw(nlondout)       ! interpolation weight to the north
    real(r8) :: igrid(nlonin)        ! interpolation weight to the north

    character(len=132) :: message
    !
    ! Check validity of input coordinate arrays: must be monotonically increasing,
    ! and have a total of at least 2 elements
    !
    if (nlonin < 2 .or. nlatin < 2) then
       call wrf_abort  ! ('BILIN: Must have at least 2 input points for interpolation')
    end if

    if (xin(1) < 0._r8 .or. xin(nlonin) > 360._r8) then
       call wrf_abort  ! ('BILIN: Input x-grid must be between 0 and 360')
    end if

    icount = 0
    do i=1,nlonin-1
       if (xin(i) >= xin(i+1)) icount = icount + 1
    end do

    do j=1,nlatin-1
       if (yin(j) >= yin(j+1)) icount = icount + 1
    end do

    do j=1,nlatout-1
       if (yout(j) >= yout(j+1)) icount = icount + 1
    end do

    do j=1,nlatout
       do i=1,nlonout(j)-1
          if (xout(i,j) >= xout(i+1,j)) icount = icount + 1
       end do
    end do

    if (icount > 0) then
       call wrf_abort  ! ('BILIN: Non-monotonic coordinate array(s) found')
    end if

    if (yout(nlatout) <= yin(1) .or. yout(1) >= yin(nlatin)) then
       call wrf_abort  ! ('BILIN: No overlap in y-coordinates')
    end if

    do j=1,nlatout
       if (xout(1,j) < 0._r8 .or. xout(nlonout(j),j) > 360._r8) then
          call wrf_abort  ! ('BILIN: Output x-grid must be between 0 and 360')
       end if

       if (xout(nlonout(j),j) <= xin(1) .or.  &
            xout(1,j)          >= xin(nlonin)) then
          call wrf_abort  ! ('BILIN: No overlap in x-coordinates')
       end if
    end do
    !
    ! Initialize index arrays for later checking
    !
    do j=1,nlatout
       jjm(j) = 0
       jjp(j) = 0
    end do
    !
    ! For values which extend beyond N and S boundaries, set weights
    ! such that values will just be copied.
    !
    do js=1,nlatout
       if (yout(js) > yin(1)) exit
       jjm(js) = 1
       jjp(js) = 1
       wgts(js) = 1._r8
       wgtn(js) = 0._r8
    end do

    do jn=nlatout,1,-1
       if (yout(jn) <= yin(nlatin)) exit
       jjm(jn) = nlatin
       jjp(jn) = nlatin
       wgts(jn) = 1._r8
       wgtn(jn) = 0._r8
    end do
    !
    ! Loop though output indices finding input indices and weights
    !
    jjprev = 1
    do j=js,jn
       do jj=jjprev,nlatin-1
          if (yout(j) > yin(jj) .and. yout(j) <= yin(jj+1)) then
             jjm(j) = jj
             jjp(j) = jj + 1
             wgts(j) = (yin(jj+1) - yout(j)) / (yin(jj+1) - yin(jj))
             wgtn(j) = (yout(j)   - yin(jj)) / (yin(jj+1) - yin(jj))
             goto 30
          end if
       end do
       call wrf_abort  ! ('BILIN: Failed to find interp values')
30     jjprev = jj
    end do

    dxinwrap = xin(1) + 360._r8 - xin(nlonin)
    !
    ! Check for sane dxinwrap values.  Allow to differ no more than 10% from avg
    !
    avgdxin = (xin(nlonin)-xin(1))/(nlonin-1._r8)
    ratio = dxinwrap/avgdxin
    if (ratio < 0.9_r8 .or. ratio > 1.1_r8) then
       write(message,*)'BILIN: Insane dxinwrap value =',dxinwrap,' avg=', avgdxin
       call wrf_message( trim(message) )
       call wrf_abort  !
    end if
    !
    ! Check grid overlap
    ! Do not do on spmd since distributed output grid may be expected to fail this test
#ifndef SPMD
    extrap = 100._r8*((js - 1._r8) + real(nlatout - jn,r8))/nlatout
    if (extrap > 20._r8) then
       write(message,*)'BILIN:',extrap,' % of N/S output grid will have to be extrapolated'
       call wrf_message( trim(message) )
    end if
#endif
    !
    ! Check that interp/extrap points have been found for all outputs, and that
    ! they are reasonable (i.e. within 32-bit roundoff)
    !
    icount = 0
    do j=1,nlatout
       if (jjm(j) == 0 .or. jjp(j) == 0) icount = icount + 1
       sum = wgts(j) + wgtn(j)
       if (sum < 0.99999_r8 .or. sum > 1.00001_r8) icount = icount + 1
       if (wgts(j) < 0._r8 .or. wgts(j) > 1._r8) icount = icount + 1
       if (wgtn(j) < 0._r8 .or. wgtn(j) > 1._r8) icount = icount + 1
    end do

    if (icount > 0) then
       call wrf_abort  ! ('BILIN: something bad in latitude indices or weights')
    end if
    !
    ! Do the bilinear interpolation
    !
    do j=1,nlatout
       !
       ! Initialize index arrays for later checking
       !
       do i=1,nlondout
          iim(i) = 0
          iip(i) = 0
       end do
       !
       ! For values which extend beyond E and W boundaries, set weights such that
       ! values will be interpolated between E and W edges of input grid.
       !
       do iw=1,nlonout(j)
          if (xout(iw,j) > xin(1)) exit
          iim(iw) = nlonin
          iip(iw) = 1
          wgtw(iw) = (xin(1)        - xout(iw,j))   /dxinwrap
          wgte(iw) = (xout(iw,j)+360._r8 - xin(nlonin))/dxinwrap
       end do

       do ie=nlonout(j),1,-1
          if (xout(ie,j) <= xin(nlonin)) exit
          iim(ie) = nlonin
          iip(ie) = 1
          wgtw(ie) = (xin(1)+360._r8 - xout(ie,j))   /dxinwrap
          wgte(ie) = (xout(ie,j)    - xin(nlonin))/dxinwrap
       end do
       !
       ! Loop though output indices finding input indices and weights
       !
       iiprev = 1
       do i=iw,ie
          do ii=iiprev,nlonin-1
             if (xout(i,j) > xin(ii) .and. xout(i,j) <= xin(ii+1)) then
                iim(i) = ii
                iip(i) = ii + 1
                wgtw(i) = (xin(ii+1) - xout(i,j)) / (xin(ii+1) - xin(ii))
                wgte(i) = (xout(i,j) - xin(ii))   / (xin(ii+1) - xin(ii))
                goto 60
             end if
          end do
          call wrf_abort  ! ('BILIN: Failed to find interp values')
60        iiprev = ii
       end do

       icount = 0
       do i=1,nlonout(j)
          if (iim(i) == 0 .or. iip(i) == 0) icount = icount + 1
          sum = wgtw(i) + wgte(i)
          if (sum < 0.99999_r8 .or. sum > 1.00001_r8) icount = icount + 1
          if (wgtw(i) < 0._r8 .or. wgtw(i) > 1._r8) icount = icount + 1
          if (wgte(i) < 0._r8 .or. wgte(i) > 1._r8) icount = icount + 1
       end do

       if (icount > 0) then
          write(message,*)'BILIN: j=',j,' Something bad in longitude indices or weights'
          call wrf_message( trim(message) )
          call wrf_abort  !
       end if
       !
       ! Do the interpolation, 1st in longitude then latitude
       !
       do k=1,nlev
          do i=1,nlonin
             igrid(i) = arrin(i,k,jjm(j))*wgts(j) + arrin(i,k,jjp(j))*wgtn(j)
          end do

          do i=1,nlonout(j)
             arrout(i,k,j) = igrid(iim(i))*wgtw(i) + igrid(iip(i))*wgte(i)
          end do
       end do
    end do


    return
  end subroutine bilin

  subroutine vertinterp(ncol, ncold, nlev, pmid, pout, arrin, arrout)

    !----------------------------------------------------------------------- 
    ! 
    ! Purpose: 
    ! Vertically interpolate input array to output pressure level
    ! Copy values at boundaries.  
    ! 
    ! Method: 
    ! 
    ! Author: 
    ! 
    !-----------------------------------------------------------------------

    implicit none

    !------------------------------Arguments--------------------------------
    integer , intent(in)  :: ncol              ! column dimension
    integer , intent(in)  :: ncold             ! declared column dimension
    integer , intent(in)  :: nlev              ! vertical dimension
    real(r8), intent(in)  :: pmid(ncold,nlev)  ! input level pressure levels 
    real(r8), intent(in)  :: pout              ! output pressure level 
    real(r8), intent(in)  :: arrin(ncold,nlev) ! input  array
    real(r8), intent(out) :: arrout(ncold)     ! output array (interpolated)
    !--------------------------------------------------------------------------

    !---------------------------Local variables-----------------------------
    integer i,k               ! indices
    integer kupper(ncold)     ! Level indices for interpolation
    real(r8) dpu              ! upper level pressure difference
    real(r8) dpl              ! lower level pressure difference
    logical found(ncold)      ! true if input levels found
    logical error             ! error flag 
    !-----------------------------------------------------------------
    !
    ! Initialize index array and logical flags
    !
    do i=1,ncol
       found(i)  = .false.
       kupper(i) = 1
    end do
    error = .false.
    !     
    ! Store level indices for interpolation. 
    ! If all indices for this level have been found, 
    ! do the interpolation 
    !     
    do k=1,nlev-1
       do i=1,ncol
          if ((.not. found(i)) .and. pmid(i,k)<pout .and. pout<=pmid(i,k+1)) then
             found(i) = .true.
             kupper(i) = k
          end if
       end do
    end do
    !
    ! If we've fallen through the k=1,nlev-1 loop, we cannot interpolate and
    ! must extrapolate from the bottom or top data level for at least some
    ! of the longitude points.
    !
    do i=1,ncol
       if (pout <= pmid(i,1)) then
          arrout(i) = arrin(i,1)
       else if (pout >= pmid(i,nlev)) then
          arrout(i) = arrin(i,nlev)
       else if (found(i)) then
          dpu = pout - pmid(i,kupper(i))
          dpl = pmid(i,kupper(i)+1) - pout
          arrout(i) = (arrin(i,kupper(i)  )*dpl + arrin(i,kupper(i)+1)*dpu)/(dpl + dpu)
       else
          error = .true.
       end if
    end do
    !     
    ! Error check
    !
    if (error) then
       call wrf_abort  ! ('VERTINTERP: ERROR FLAG')
    end if

    return
  end subroutine vertinterp

  subroutine get_timeinterp_factors (cycflag, np1, cdayminus, cdayplus, cday, &
       fact1, fact2, str)
    !---------------------------------------------------------------------------
    !
    ! Purpose: Determine time interpolation factors (normally for a boundary dataset)
    !          for linear interpolation.
    !
    ! Method:  Assume 365 days per year.  Output variable fact1 will be the weight to
    !          apply to data at calendar time "cdayminus", and fact2 the weight to apply
    !          to data at time "cdayplus".  Combining these values will produce a result 
    !          valid at time "cday".  Output arguments fact1 and fact2 will be between 
    !          0 and 1, and fact1 + fact2 = 1 to roundoff.
    !
    ! Author:  Jim Rosinski
    !
    !--------------------------------------------------------------------------- 
    implicit none
    !
    ! Arguments
    !
    logical, intent(in) :: cycflag             ! flag indicates whether dataset is being cycled yearly

    integer, intent(in) :: np1                 ! index points to forward time slice matching cdayplus

    real(r8), intent(in) :: cdayminus          ! calendar day of rearward time slice
    real(r8), intent(in) :: cdayplus           ! calendar day of forward time slice
    real(r8), intent(in) :: cday               ! calenar day to be interpolated to
    real(r8), intent(out) :: fact1             ! time interpolation factor to apply to rearward time slice
    real(r8), intent(out) :: fact2             ! time interpolation factor to apply to forward time slice

    character(len=*), intent(in) :: str        ! string to be added to print in case of error (normally the callers name)
    !
    ! Local workspace
    !
    real(r8) :: deltat                         ! time difference (days) between cdayminus and cdayplus
    real(r8), parameter :: daysperyear = 365.  ! number of days in a year

    character(len=132) :: message
    !
    ! Initial sanity checks
    !
    if (np1 == 1 .and. .not. cycflag) then
       call wrf_abort  ! ('GETFACTORS:'//str//' cycflag false and forward month index = Jan. not allowed')
    end if

    if (np1 < 1) then
       call wrf_abort  ! ('GETFACTORS:'//str//' input arg np1 must be > 0')
    end if

    if (cycflag) then
       if ((cday < 1.) .or. (cday > (daysperyear+1.))) then
          write(message,*) 'GETFACTORS:', str, ' bad cday=',cday
          call wrf_message( trim(message) )
          call wrf_abort  ! ()
       end if
    else
       if (cday < 1.) then
          write(message,*) 'GETFACTORS:', str, ' bad cday=',cday
          call wrf_message( trim(message) )
          call wrf_abort  ! ()
       end if
    end if
    !
    ! Determine time interpolation factors.  Account for December-January 
    ! interpolation if dataset is being cycled yearly.
    !
    if (cycflag .and. np1 == 1) then                     ! Dec-Jan interpolation
       deltat = cdayplus + daysperyear - cdayminus
       if (cday > cdayplus) then                         ! We are in December
          fact1 = (cdayplus + daysperyear - cday)/deltat
          fact2 = (cday - cdayminus)/deltat
       else                                              ! We are in January
          fact1 = (cdayplus - cday)/deltat
          fact2 = (cday + daysperyear - cdayminus)/deltat
       end if
    else
       deltat = cdayplus - cdayminus
       fact1 = (cdayplus - cday)/deltat
       fact2 = (cday - cdayminus)/deltat
    end if

    if (.not. valid_timeinterp_factors (fact1, fact2)) then
       write(message,*) 'GETFACTORS: ', str, ' bad fact1 and/or fact2=', fact1, fact2
       call wrf_message( trim(message) )
       call wrf_abort  ! ()
    end if

    return
  end subroutine get_timeinterp_factors

  logical function valid_timeinterp_factors (fact1, fact2)
    !---------------------------------------------------------------------------
    !
    ! Purpose: check sanity of time interpolation factors to within 32-bit roundoff
    !
    !---------------------------------------------------------------------------
    implicit none

    real(r8), intent(in) :: fact1, fact2           ! time interpolation factors

    valid_timeinterp_factors = .true.

    ! The fact1 .ne. fact1 and fact2 .ne. fact2 comparisons are to detect NaNs.
    if (abs(fact1+fact2-1.) > 1.e-6 .or. &
         fact1 > 1.000001 .or. fact1 < -1.e-6 .or. &
         fact2 > 1.000001 .or. fact2 < -1.e-6 .or. &
         fact1 .ne. fact1 .or. fact2 .ne. fact2) then

       valid_timeinterp_factors = .false.
    end if

    return
  end function valid_timeinterp_factors

end module module_interpolate
