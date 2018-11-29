!
! Module with some handy graphics routines.
!
! This is very much a work in progress.
!
! A lot of this has been adapted from Mark Stoelinga's RIP program, 
! which is highly recommended.
!
module kwm_plot_utilities
  integer                           :: nsq
  real,              dimension(256) :: rsq, usq
  character(len=20), dimension(256) :: cosq


  ! NCO:  number of already-defined and named colors
  integer :: nco 

  type named_color
     character(len=20) :: name
     real :: r
     real :: g
     real :: b
  end type named_color

  type(named_color), dimension(0:255) :: co

  type plotconfig_type
     integer :: nicecon
     logical :: nolb
     logical :: cbar
     logical :: nohl
  end type plotconfig_type
  type(plotconfig_type) :: plotconfig = plotconfig_type(30, .FALSE., .TRUE., .FALSE.)

  integer :: nconarea

  integer, parameter :: maxcon = 80
  real, dimension(maxcon) :: valcon
  integer, dimension(maxcon) :: majcon
  integer, dimension(maxcon) :: icoindcp
  real :: valmin, valmax
  logical :: larng

  real :: nsqmax, nsqmin
  character(len=4) :: method = "cont"
  real :: frame_left = 0.0
  real :: frame_right = 1.0
  real :: frame_bottom = 0.0
  real :: frame_top = 1.0

  interface line_in_color
     module procedure line_in_color_integer, line_in_color_string
  end interface

contains

!****************************************************************************************

  subroutine kwm_start_ncargks(new_cgm_name)
    implicit none
    ! I can never remember what I called the subroutine.
    character(len=*), optional, intent(in) :: new_cgm_name
    if ( present (new_cgm_name) ) then
       call kwm_init_ncargks(new_cgm_name)
    else
       call kwm_init_ncargks()
    endif
  end subroutine kwm_start_ncargks

  subroutine kwm_open_ncargks(new_cgm_name)
    implicit none
    ! I can never remember what I called the subroutine.
    character(len=*), optional, intent(in) :: new_cgm_name
    if ( present (new_cgm_name) ) then
       call kwm_init_ncargks(new_cgm_name)
    else
       call kwm_init_ncargks()
    endif
  end subroutine kwm_open_ncargks

!****************************************************************************************

  subroutine kwm_init_ncargks(new_cgm_name)
! Starts off the NCAR Graphics package.
! Opens workstation 1 as a CGM workstation.
! Opens workstation 2 as a WISS.
! Defines a couple of colors, and sets a couple of NCAR Graphics parameters
! 
! If you want the gmeta file to be called something other than "gmeta",
! give this subroutine the optional argument.
    implicit none
    character(len=*), optional, intent(in) :: new_cgm_name
    character(len=80) :: newname
    character(len=1) :: hdum

    call gopks(6,0)

    if (present(new_cgm_name)) then
       newname = trim(new_cgm_name)
       call gesc(-1391, 1, newname, 1, 1, hdum)
    endif

    call gopwk(1,119,1)
    call gopwk(2,120,3)
    call gacwk(1)
    call gacwk(2)

    call define_color("white")
    call define_color("black")
    call pcseti("FN", 21)
    call pcsetc("FC", "~")
  end subroutine kwm_init_ncargks

!****************************************************************************************

  subroutine kwm_close_ncargks
    implicit none
    call gdawk(1)
    call gdawk(2)
    call gclwk(1)
    call gclwk(2)
    call gclks

    ! reset some accounting arrays
    nsq = 0
    rsq = 0.
    cosq = ""
    nco = 0
    co = named_color("",0.,0.,0.)

  end subroutine kwm_close_ncargks

!****************************************************************************************

  subroutine gset_named_color(NAME)
    ! Calls NCAR-Graphics subroutine GSCR to set the next color index 
    ! to the rgb values indicated by the color named NAME.

    ! Side effects:  Associate RGB values with a color index NCO.
    !                Increment module variable NCO.
    implicit none
    character(len=*), intent(in) :: NAME
    type(named_color) :: color

    print*
    print*, ' ***** '
    print*, ' ***** You probably want "define_color(NAME)" instead.'
    print*, ' ***** '
    print*

    color = get_x11_color(name)
    call gscr(1, nco, color%r, color%g, color%b)
    nco = nco+1

  end subroutine gset_named_color

!****************************************************************************************

  subroutine set_pval(nicecon, cmth, frml, frmr, frmb, frmt, cbar, nolb, nohl)
    implicit none
    integer, optional, intent(in) :: nicecon   ! Approximate number of contours
    character(len=4), optional, intent(in) :: cmth ! "fill", "cont", or "both"
    real, optional, intent(in) :: frml, frmr, frmb, frmt
    logical, optional, intent(in) :: cbar, nolb, nohl
    if (present(nicecon)) plotconfig%nicecon = nicecon
    if (present(cmth)   ) method = cmth
    if (present(frml)   ) frame_left  = frml
    if (present(frmr)   ) frame_right = frmr
    if (present(frmb)   ) frame_bottom = frmb
    if (present(frmt)   ) frame_top = frmt
    if (present(cbar)   ) plotconfig%cbar = cbar
    if (present(nolb)   ) plotconfig%nolb = nolb
    if (present(nohl)   ) plotconfig%nohl = nohl

  end subroutine set_pval

!****************************************************************************************

  subroutine fillcell(array, ix, jx, mask)
    implicit none
    integer, intent(in) :: ix, jx
    real, intent(in), dimension(ix,jx) :: array
    logical, dimension(ix,jx), optional, intent(in) :: mask

    logical, dimension(ix,jx) :: bmask
    integer :: i, j, n, mjsk, numcon
    real :: xmn, xmx, cintuse
    type(named_color) :: cotmp
    integer :: ico, icomax

    type(named_color), dimension(maxcon) :: usit

    mjsk = 0

    if (present(mask)) then
       bmask = mask
    else
       bmask = .TRUE.
    endif

!   Get the contour values
    if (larng) then

       numcon = plotconfig%nicecon
       xmn = minval(array, mask=bmask)
       xmx = maxval(array, mask=bmask)
       do i = 1, numcon
          valcon(i) = xmn + (xmx-xmn)*float(i-1)/float(numcon-1)
       enddo
       usq = xmn + (xmx-xmn)*rsq*0.01 ! *float(nsq-1)/float(nsq)

    else

       call getconvals(plotconfig%nicecon, mjsk, &
            array, ix, jx, maxcon, valcon, majcon, &
            cintuse, numcon, &
            rbeg = rsq(1), rend=rsq(nsq))
       usq = rsq
       xmn = usq(1)
       xmx = usq(nsq)
    endif

    if (numcon == 0) then
       ! No coloring levels, so just return.
       return
    endif

    nconarea = numcon+1
! Set up colors.
!   Assign the red, green, and blue fractions to the color sequence
!
    ILOOP : do i=1,nsq
       ! nco is the number of colors already assigned by name
       do j=0,nco-1
          if (cosq(i) == co(j)%name) then
             usit(i) = co(j)
             cycle ILOOP
          endif
       enddo
       ! If the color name is not found, look for it, add it to the list
       ! of named colors, and increment NCO, the number of named colors.
       usit(i) = get_x11_color(cosq(i))
       co(nco) = usit(i)
       nco = nco + 1
    enddo ILOOP

    nco = nco - 1
    icomax = nco

! Now see if new colors need to be defined.
    COLORLOOP : do i=1,nconarea
       icoindcp(i) = -1
       cotmp = color_fraction( i, usit, usq, nsq, numcon, valcon )
!
!   Assign color index to icoindcp
!      First check if color already exists
!
       do ico=0,icomax
          if ( (abs(cotmp%r - co(ico)%r)<=0.001) .and. &
               (abs(cotmp%g - co(ico)%g)<=0.001) .and. &
               (abs(cotmp%b - co(ico)%b)<=0.001) ) then
             icoindcp(i)=ico
             call gscr(1,ico,co(ico)%r,co(ico)%g,co(ico)%b)
             cycle COLORLOOP
          endif
       enddo
!
!      If not, set the new color and assign its index to icoincdcp
!
       icomax=icomax+1
       if (icomax == 256) then
          write(*,'("pltcon is trying to define color number 256, but 255",/,&
               &"is the limit.  Try increasing the contour interval, or",/,&
               &"reduce the number of colors defined in the color table.")')
          stop
       endif
       call gscr(1,icomax,cotmp%r,cotmp%g,cotmp%b)
       co(icomax) = cotmp
       icoindcp(i)=icomax
    enddo COLORLOOP

    do i = 1, ix
       JLOOP : do j = 1, jx
          if (.not. bmask(i,j)) cycle JLOOP
          if (array(i,j) <= xmn) cycle JLOOP
          do n = numcon, 1, -1
             if ( array(i,j) >= valcon(n)) then
                call ngsquares((/float(i)/), (/float(j)/), 1, 1., icoindcp(n+1))
                exit
             endif
          enddo
       enddo JLOOP
    enddo

    if (plotconfig%cbar) then
       call labelbar(array, ix, jx, numcon)
    endif

  end subroutine fillcell

!****************************************************************************************

  subroutine labelbar(array, ix, jx, numcon, iskip)
! Put on a color-scale and label the box
    implicit none
    integer, intent(in) :: ix, jx, numcon
    integer, optional, intent(in) :: iskip
    real, dimension(ix,jx) , intent(in) :: array
    real :: xmx, xmn
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml

    real :: xlmn, xlmx, ylmn, ylmx

    integer :: n, ierr

    integer :: ipx, ipn, ip
    character(len=40) :: fmt, hh
    integer, dimension(2) :: mxlc, mnlc

    real :: txtsz
    real :: mmsz

    txtsz = 0.009 * min((frame_right-frame_left),(frame_top-frame_bottom))
    mmsz = 0.015 * min((frame_right-frame_left),(frame_top-frame_bottom))

    call gsplci(1)
    call pcseti("CC", 1)

    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)

    call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
!KWM    call line(frame_left, frame_bottom, frame_right, frame_bottom)
!KWM    call line(frame_left, frame_bottom, frame_left, frame_top)
!KWM    call line(frame_left, frame_top, frame_right, frame_top)
!KWM    call line(frame_right, frame_bottom, frame_right, frame_top)
!    call set(xl, xr, xb, xt, wl, wr, wb, wt, ml)

!    call set(frame_left, frame_right, frame_bottom, frame_top, &
!         0., 1., 0., 1., ml)

    mxlc = maxloc(array)
    mnlc = minloc(array)
    xmx = array(mxlc(1), mxlc(2))
    xmn = array(mnlc(1), mnlc(2))

    xlmn = xr + 0.005
    ! xlmx = xlmn + (frame_right-xr)*0.25
    xlmx = xlmn + 0.050

    do n = 1, numcon
       ylmn = xb + float(n-1)/float(numcon) * (xt-xb)
       ylmx = xb + float(n  )/float(numcon) * (xt-xb)
!       call fillbox(xlmn, xlmx, ylmn, ylmx, icoindcp(n+1))
       call fillbox(xlmn, xlmx, ylmn, ylmx, icoindcp(n))
    enddo
    call gslwsc(1.)
    call line(xlmn,xb,xlmn,xt)
    call line(xlmx,xb,xlmx,xt)
    do n = 1, numcon!nsq
       ylmn = xb + float(n-1)/float(numcon) * (xt-xb)
       ylmx = xb + float(n  )/float(numcon) * (xt-xb)
       call line(xlmn, ylmn, xlmx, ylmn)
       call line(xlmn, ylmx, xlmx, ylmx)
    enddo
    call gslwsc(1.)

    ! Determine the scaling for the numbers to be printed:

    if (abs(xmx) > 1.E-12) then
       ipx = alog10(abs(xmx))
    else
       ipx = 0
    endif
    if (abs(xmn) > 1.E-12) then
       ipn = alog10(abs(xmn))
    else
       ipn = 0
    endif
    if (ipx - ipn > 6) then
       ip = -(3*ipx+ipn)/4
    else
       ip = -min(ipx, ipn)
    endif
    if (ip > 0) then
       write(fmt,'("(G14.5E2)")')
    else
       write(fmt,'("(",I3,"PF12.4)")') ip
    endif

!    do n = numcon+1, 1, -1
    do n = numcon, 2, -1
       ylmn = xb + float(n-1)/float(numcon) * (xt-xb)
       if (n == numcon+1) then
          write(hh, fmt=fmt) xmx
!          print*, 'xmx, valcon(n) = ', xmx, valcon(n)
          if (xmx == valcon(n-1)) then
             ! Don't repeat the max value
             hh = ""
          endif
       else
          write(hh, fmt=fmt) valcon(n-1)
       endif
       if (present(iskip)) then
          if (mod(n-2,iskip) == 0) then
             call pchiqu(xlmx-0.030, ylmn, hh, txtsz * iskip * 0.66, 0., -1.)
          endif
       else
          call pchiqu(xlmx+0.005, ylmn, hh, txtsz, 0., -1.)
       endif
    enddo

    if (ip < 0) then
       !call pchiqu(xlmn, xb-0.03, "scaled by 1.E"//fmt(2:4), txtsz, 0., -1.)
       call pchiqu(xlmn, xb, "~V-1Q~scaled by 1.E"//fmt(2:4), txtsz, 0., -1.)
    endif

    write(fmt, '(" at ",I4, 1x, I4)') mxlc(1), mxlc(2)

    write(hh, '(F14.5)') xmx
!    call pchiqu(xr, (xb-0.06), "(x) Max value = "//hh//trim(fmt),&
!         mmsz, 0., 1.)

!    call pchiqu(xr, xb-2*mmsz, "(x) Max value = "//hh//trim(fmt),&
!         mmsz, 0., 1.)

    write(fmt, '(" at ",I4, 1x, I4)') mnlc(1), mnlc(2)
    write(hh, '(F14.6)') xmn

!    call pchiqu(xr, (xb-0.08), "(+) Min value = "//hh//trim(fmt), &
!         mmsz, 0., 1.)

!    call pchiqu(xr, xb-4*mmsz, "(+) Min value = "//hh//trim(fmt), &
!         mmsz, 0., 1.)

    call set(xl, xr, xb, xt, wl, wr, wb, wt, ml)

  end subroutine labelbar

!****************************************************************************************

  subroutine pltcon(array, ix, jx)
    implicit none
    integer, intent(in) :: ix, jx
    real, dimension(ix,jx), intent(in) :: array

    integer, parameter :: lwrk = 800000, liwk = 2000000
    real, dimension(lwrk) :: rwrk
    integer, dimension(liwk) :: iwrk

    integer, parameter :: niam = 1000000
    integer, parameter :: ncs = 1000000
    integer, dimension(niam) :: iam
    real,    dimension(niam) :: xcs, ycs
    integer, dimension(50000)  :: iaia, igia

    real :: cintuse, cr, cg, cb
    integer :: numcon
    integer :: mjsk
    integer :: ico, i, j, icomax, ierr
    type(named_color) cotmp
    type(named_color), dimension(maxcon) :: usit
    real :: xmn, xmx

    mjsk = 0

!   Get the contour values
    if (larng) then

       numcon = plotconfig%nicecon
       xmn = minval(array)
       xmx = maxval(array)
       do i = 1, numcon
          valcon(i) = xmn + (xmx-xmn)*float(i-1)/float(numcon-1)
       enddo
       usq = xmn + (xmx-xmn)*rsq*0.01 ! *float(nsq-1)/float(nsq)

    else

       call getconvals(plotconfig%nicecon, mjsk, &
            array, ix, jx, maxcon, valcon, majcon, &
            cintuse, numcon, &
            rbeg = rsq(1), rend=rsq(nsq))
       usq = rsq

    endif

!KWM    print*, 'usq = ', usq(1:numcon)

! Number of areas for filling is one plus the number of contours, 
! i.e., the contours define the boundaries between areas.
    nconarea = numcon+1

    call cpseti('SET',0)   ! we'll use our own set call
    call cpseti('CLS',0)   ! we'll use our own cont. values
!
!   Some settings for hi/lo and contour labels
!
    call cpseti('NSD',-4)
    call cpseti('NOF',7)
    call cpsetr('PC6',.6)

! Set up colors.
!   Assign the red, green, and blue fractions to the color sequence
!
    ILOOP : do i=1,nsq
       ! nco is the number of colors already assigned by name
       do j=0,nco-1
          if (cosq(i) == co(j)%name) then
             usit(i) = co(j)
             cycle ILOOP
          endif
       enddo
       ! If the color name is not found, look for it, add it to the list
       ! of named colors, and increment NCO, the number of named colors.
       usit(i) = get_x11_color(cosq(i))
       co(nco) = usit(i)
       nco = nco + 1
    enddo ILOOP

    nco = nco - 1
    icomax = nco
!
! USIT is now our list of color names and associated rgb values in 
! our color sequence.  Color values in USIT correspond to USQ

! Note that we haven't actually called GSCR to assign RGB values to
! color indices yet.  This will come later

    COLORLOOP : do i=1,nconarea
       icoindcp(i) = -1
       cotmp = color_fraction( i, usit, usq, nsq, numcon, valcon )
!
!   Assign color index to icoindcp
!      First check if color already exists
!
       do ico=0,icomax
          if ( (abs(cotmp%r - co(ico)%r)<=0.001) .and. &
               (abs(cotmp%g - co(ico)%g)<=0.001) .and. &
               (abs(cotmp%b - co(ico)%b)<=0.001) ) then
             icoindcp(i)=ico
             call gscr(1,ico,co(ico)%r,co(ico)%g,co(ico)%b)
             cycle COLORLOOP
          endif
       enddo
!
!      If not, set the new color and assign its index to icoincdcp
!
       icomax=icomax+1
       if (icomax == 256) then
          write(*,'("pltcon is trying to define color number 256, but 255",/,&
               &"is the limit.  Try increasing the contour interval, or",/,&
               &"reduce the number of colors defined in the color table.")')
          stop
       endif
       call gscr(1,icomax,cotmp%r,cotmp%g,cotmp%b)
       co(icomax) = cotmp
       icoindcp(i)=icomax
    enddo COLORLOOP

    call cpseti('NVS', 4)
    call cpseti('NCL',numcon)
    if (plotconfig%nohl) then
       call cpsetc("HIT", ' ')
       call cpsetc("LOT", ' ')
    endif
    do i=1,numcon
       call cpseti('PAI',i)
       call cpsetr('CLV',valcon(i))
       if (plotconfig%nolb) then
          call cpseti('CLU',1)
       else
          call cpseti('CLU',3)
       endif
       call cpseti('AIB',i)
       call cpseti('AIA',i+1)
    enddo

    call cprect(array,ix,ix,jx,rwrk,lwrk,iwrk,liwk)
    call arinam(iam,niam) ! Initialize the area map

    call cpclam(array,rwrk,iwrk,iam)  ! put cont. lines in area map
    if ((method == "both") .or. (method == "fill")) then
       call arscam(iam,xcs,ycs,ncs,iaia,igia,5000,cpcolr)
       if (plotconfig%cbar) then
          call labelbar(array, ix, jx, numcon)
       endif
    endif

!KWM! Add a color bar.
!KWM!    call color_bar(nsq, 0., rsq(nsq))
!KWM    if (larng) then
!KWM       call color_bar(numcon-1, valmin, valmax)
!KWM    else
!KWM       call color_bar(numcon, valmin, valmax)
!KWM    endif

    if ((method == "cont") .or. (method == "both")) then
       ! Contour lines:
       call cplbdr(array, rwrk, iwrk)
       call cpcldr(array, rwrk, iwrk)
    endif

    call gsplci(1)
    call pcseti("CC", 1)
  end subroutine pltcon

!****************************************************************************************

  subroutine color_bar(nthresh, cmin, cmax)
    implicit none
    integer, intent(in) :: nthresh
    real, intent(in) :: cmin, cmax
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml

    real :: xlmn, xlmx, ylmn, ylmx
    integer :: n, ipx, ipn, ip

    character(len=14) :: hh, fmt
    integer :: i, j, jj
    integer :: ierr

    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
    call set(frame_left,frame_right,frame_bottom,frame_top,0.,1.,0.,1.,ml)

    xlmn = xr + 0.005
    ! xlmx = xlmn + (1.-xr)*0.25
    xlmn = xlmn + .050

    do n = 1, nthresh+1
       ylmn = xb + float(n-1)/float(nthresh+1) * (xt-xb)
       ylmx = xb + float(n  )/float(nthresh+1) * (xt-xb)
       if (n <= nthresh) then
          call cpseti('PAI',n)
          call cpgeti('AIB',j)
          call fillbox(xlmn, xlmx, ylmn, ylmx, icoindcp(j))
!          call fillbox(xlmn, xlmx, ylmn, ylmx, color_index(cosq(n)))
       else
          call cpseti('PAI',n-1)
          call cpgeti('AIA',jj)
          call cpgeti('AIB',j)
          call fillbox(xlmn, xlmx, ylmn, ylmx, icoindcp(jj))
!          call fillbox(xlmn, xlmx, ylmn, ylmx, color_index(cosq(n)))
       endif
    enddo

    call gslwsc(1.)
    call line(xlmn,xb,xlmn,xt)
    call line(xlmx,xb,xlmx,xt)
    do n = 1, nthresh+1
       ylmn = xb + float(n-1)/float(nthresh+1) * (xt-xb)
       ylmx = xb + float(n  )/float(nthresh+1) * (xt-xb)
       call line(xlmn, ylmn, xlmx, ylmn)
       call line(xlmn, ylmx, xlmx, ylmx)
    enddo
    call gslwsc(1.)

! Determine the scaling for the numbers to be printed:

    if (abs(cmax) > 1.E-12) then
       ipx = alog10(abs(cmax))
    else
       ipx = 0
    endif
    if (abs(cmin) > 1.E-12) then
       ipn = alog10(abs(cmin))
    else
       ipn = 0
    endif
    if (ipx - ipn > 6) then
       ip = -(3*ipx+ipn)/4
    else
       ip = -min(ipx, ipn)
    endif
    if (ip > 0) then
       write(fmt,'("(G14.5E2)")')
    else
       write(fmt,'("(",I3,"PF12.4)")') ip
       if (ierr /= 0) fmt="(G20.5E2)"
    endif

    do n = nthresh, 1, -1
       ylmn = xb + float(n)/float(nthresh+1) * (xt-xb)
       if (n == nthresh+1) then
!        write(hh, '(G14.5E2)') xmx
          write(hh, fmt=fmt) cmax
       else
!        write(hh, '(G14.5E2)') thresh(n)
          write(hh, fmt=fmt) valcon(n) !thresh(n)
       endif
       call pchiqu(xlmx+0.005, ylmn, hh, 0.009, 0., -1.)
    enddo

    if (ip < 0) then
       !call pchiqu(xlmn, xb-0.03, "scaled by 1.E"//fmt(2:4), 0.009, 0., -1.)
       call pchiqu(xlmn, xb, "~V-1Q~scaled by 1.E"//fmt(2:4), 0.009, 0., -1.)
    endif

!    write(fmt, '(" at ",I4, 1x, I4)') mxlcx, mxlcy

!    write(hh, '(F14.5)') xmx
!    call pchiqu(xlmn-0.17, (xb-0.06), "(x) Max value = "//hh//fmt,&
!         0.011, 0., 1.)

!    write(fmt, '(" at ",I4, 1x, I4)') mnlcx, mnlcy
!    write(hh, '(F14.6)') xmn
!    call pchiqu(xlmn-0.17, (xb-0.10), "(+) Min value = "//hh//fmt, &
!         0.011, 0., 1.)

    call set(xl, xr, xb, xt, wl, wr, wb, wt, ml)
  end subroutine color_bar

!==============================================================================

  type(named_color) function get_x11_color(name) result (c)
    use kwm_string_utilities
    implicit none
    character(len=*) :: name
    integer :: r, g, b, i
    character(len=20) :: cname

    logical, save :: already_read = .FALSE.
    integer :: ierr, idx
    character(len=80) :: string, stringname
    integer, external :: get_unused_unit
    integer :: iunit

    real, dimension(2500), save :: readlist_r, readlist_g, readlist_b
    character(len=64), dimension(2500), save :: readlist_name
    integer, save :: readlist_count = 0
    logical :: lthere

    if (.not. already_read) then
       already_read = .TRUE.
       iunit = get_unused_unit()

       inquire(file="/usr/lib/X11/rgb.txt", exist=lthere)
       if (lthere) then
          open(iunit, file="/usr/lib/X11/rgb.txt", form='formatted', &
               status='old', action='read', iostat=ierr)
       else
          inquire(file="/usr/share/X11/rgb.txt", exist=lthere)
          if (lthere) then
             open(iunit, file="/usr/share/X11/rgb.txt", form='formatted', &
                  status='old', action='read', iostat=ierr)
          else
             stop 'KWM_PLOT_UTILITIES:  Cannot find X11 "rgb.txt" file'
          endif
       endif
       if (ierr /= 0) stop "color table"
       do

          read(iunit, '(A)', iostat=ierr) string
          if (ierr /= 0) exit
          if (string(1:1) == "!") cycle

          stringname = " "
          read(string, *) r, g, b
          ! Find the index of the first alphabetical character
          idx = verify(string, " 0123456789"//char(9)) ! char(9) is tab
          stringname = trim(string(idx:))
          stringname = downcase(trim(stringname))
          stringname = unblank(trim(stringname))

          readlist_count = readlist_count + 1
          readlist_r(readlist_count) = float(r)/255.
          readlist_g(readlist_count) = float(g)/255.
          readlist_b(readlist_count) = float(b)/255.
          readlist_name(readlist_count) = stringname

       enddo

       close(iunit)
    endif

    ! Search through readlist for name

    if (name(1:1) == "#") then
       read(name(2:7), '(Z2,Z2,Z2)') r, g, b
       cname = name
       c = named_color(cname, float(r)/255., float(g)/255., float(b)/255.)
       return
    else if ((name(1:2) == "0x") .or. (name(1:2) == "0X")) then
       read(name(3:8), '(Z2,Z2,Z2)') r, g, b
       cname = name
       c = named_color(cname, float(r)/255., float(g)/255., float(b)/255.)
       return
    else

       do i = 1, readlist_count
          if (name == readlist_name(i)) then
             cname = name
             c = named_color(cname, readlist_r(i), readlist_g(i), readlist_b(i))
             return
          endif
       enddo

       write(*, '("color not found: ", A)') trim(name)
    endif

  end function get_x11_color

  type(named_color) function color_fraction(i, usit, rsq, count, numcon, valcon) &
       result (c)
! Returns c(name,r,g,b) that is to be used for index i

    implicit none
    integer, intent(in)                             :: i
    integer, intent(in)                             :: count
    type(named_color), intent(in), dimension(count) :: usit
    real,              intent(in), dimension(count) :: rsq
    integer, intent(in)                             :: numcon
    real,    intent(in),          dimension(numcon) :: valcon

    real :: val
    real :: red, green, blue, fac
    integer :: isq

    if (numcon == 0) then
       print*, 'numcon = ', numcon
       c = named_color("white", 1., 1., 1.)
       return
    endif
    if (numcon == 1) then
       print*, 'numcon = ', numcon
       c = named_color("white", 1., 1., 1.)
       return
    endif

    valmin=valcon(1)-.5*(valcon(2)-valcon(1))
    valmax=valcon(numcon)+.5*(valcon(numcon)-valcon(numcon-1))

    if (i == 1) then
       val=valmin
    elseif (i == numcon+1) then
       val=valmax
    else
       val=.5*(valcon(i-1)+valcon(i))
    endif

    c = named_color("transparent", -1., -1., -1.)

    if (val.le.valcon(1)) then
       if (usit(1)%name /= "transparent" ) c = usit(1)
    elseif (val.ge.valcon(numcon)) then
       if (usit(nsq)%name /= "transparent" ) c = usit(nsq)
    else
       ! Interpolate from USIT rgb values (associated with RSQ 
       ! floating-point values) to a new color c.

       do isq=1,nsq-1
          if (val.ge.rsq(isq).and.val.le.rsq(isq+1)) then
             if ( usit(isq  )%name /= "transparent" .and. &
                  usit(isq+1)%name /= "transparent" ) then
                fac=(val-rsq(isq))/(rsq(isq+1)-rsq(isq))
                red  =fac*usit(isq+1)%r+(1.-fac)*usit(isq)%r
                green=fac*usit(isq+1)%g+(1.-fac)*usit(isq)%g
                blue =fac*usit(isq+1)%b+(1.-fac)*usit(isq)%b
                c = named_color("", red, green, blue)
             endif
             return
          endif
       enddo
!KWM       print*, 'usit%name = ', usit%name
!KWM       print*, 'rsq = ', rsq
!KWM       print*, 'valcon = ', valcon
!KWM       print*, 'valmin, valmax = ', valmin, valmax
!KWM       print*, 'val = ', val
!KWM       stop "Not Found."
    endif
  end function color_fraction

!==============================================================================

  subroutine getconvals(ncon,mjsk, &
       array,ix,jx,maxcon,valcon,majcon, &
       cintuse, numcon, rbeg, rend, rint, fmsg)
! Swiped from Mark Stoelinga's RIP program and adapted.
    implicit none
    real,            optional, intent(in) :: rbeg ! cbeg
    real,            optional, intent(in) :: rend ! cend
    real,            optional, intent(in) :: rint ! cint
    real,            optional, intent(in) :: fmsg ! rmsg
    integer,                   intent(in) :: ncon
    integer,                   intent(in) :: mjsk
    integer,                   intent(in) :: ix, jx
    integer,                   intent(in) :: maxcon
    real,    dimension(ix,jx), intent(in) :: array

    real,    dimension(maxcon), intent(out) :: valcon
    integer, dimension(maxcon), intent(out) :: majcon
    real,                       intent(out) :: cintuse
    integer,                    intent(out) :: numcon

    integer :: ii, i, j, itmp, imaj, imajrel
!
!
    real :: cbeg, cend, cint, rmsg
    real :: cb, ce, ci, tmp, fac, diff, fv, rmax, rmin, crange
    real :: p
    logical :: mult
!
!   mjsk is the number of minor (unlabeled) contours desired
!   between major (labeled) contours.
!
    logical :: choosecb, chooseci

    if (present(fmsg)) then
       rmsg = fmsg
    else
       rmsg = -1.E30
    endif

!
!   cbeg is the beginning contour value.  rmsg means pick a
!   nice value close to the max value in the field. -rmsg means
!   pick a nice value close to the min value in the field.
!
    if (present(rbeg)) then
       cbeg = rbeg
    else
       cbeg = rmsg
    endif
!
!   cend is the ending contour value.  rmsg means pick a nice
!   value close to the max value in the field. -rmsg means pick
!   a nice value close to the min value in the field.
!
    if (present(rend)) then
       cend = rend
    else
       cend = -rmsg
    endif
!
!   cint is the contour interval. rmsg means pick a nice value
!   that generates close to ncon contours.
!
    if (present(rint)) then
       cint = rint
    else
       cint = rmsg
    endif
!
!   mult means use a multiplicative contour interval instead of
!   an additive one.  If mult is true, cbeg and cend must be of
!   the same sign (otherwise cend will be changed to a value
!   that is of the same sign as cbeg), and all contour values
!   will also be the same sign.
!
!KWM  mult = lmult(ipl)
    mult = .FALSE.
!
!   First get max and min in field.
!
    if (all(array==rmsg)) then
       write(*,*)'   In getconvals: not generating any contours'
       write(*,*)'    because all values are special value.'
       numcon=0
       return
    endif
!
    rmax = maxval(array, mask=(array/=rmsg))
    rmin = minval(array, mask=(array/=rmsg))

    if (rmax == rmin) then
       write(*,*)'   In getconvals: not generating any contours'
       write(*,*)'    because field is constant.  Value = ',rmax
       numcon=0
       return
    endif
!
    choosecb=.false.
    if (cbeg == rmsg) then
       cb=rmax
       choosecb=.true.
    elseif (cbeg == -rmsg) then
       cb=rmin
       choosecb=.true.
    else
       cb=cbeg
    endif
!
    if (cend == rmsg) then
       ce=rmax
    elseif (cend == -rmsg) then
       ce=rmin
    else
       ce=cend
    endif
!
    chooseci=.false.
    if (cint == rmsg .or. cint == 0.0) then
       chooseci=.true.
    elseif (cint < 0.0) then
       ci=-cint
    else
       ci=cint
    endif
!
!KWM  if (mult) then
!KWM     if (cb == 0.0.or.ce == 0.0) then
!KWM        write(iup,*)'In getconvals: not generating any contours'
!KWM        write(iup,*)'   because mult is true and cbeg or cend'
!KWM        write(iup,*)'   are zero. cbeg,cend=',cbeg,cend
!KWM        numcon=0
!KWM        return
!KWM     endif
!KWM     if (cb*ce.lt.0.0) then
!KWM        write(iup,*)'In getconvals: not generating any contours'
!KWM        write(iup,*)'   because mult is true and cbeg and cend are'
!KWM        write(iup,*)'   of opposite sign.  cbeg,cend=',cbeg,cend
!KWM        numcon=0
!KWM        return
!KWM     endif
!KWM     if (ci == 1.0) chooseci=.true.
!KWM     if (cb.lt.0.0) then
!KWM        iswitch=1
!KWM        cb=-cb
!KWM        ce=-ce
!KWM        temp=rmax
!KWM        rmax=-rmin
!KWM        rmin=-temp
!KWM     else
!KWM        iswitch=0
!KWM     endif
!KWM     valm=1e-8*cb
!KWM     rmax=max(rmax,valm)
!KWM     rmin=max(rmin,valm)
!KWM     ce=max(ce,valm)
!KWM  endif
!
!   Set up start, end, and interval.
!
!KWM  if (.not.mult) then
    if (chooseci) then
       crange=max(abs(ce-cb),1.e-10)
       ci = crange/ncon
       p = 10.**(int(alog10(ci)+50000.)-50000)
       ci=max(ci,p)
       ii=int(ci/p)
       if (ii.ge.7) then
          ci = 10.*p
       else
          ci = ii*p
       endif
    endif
    if (choosecb) then
       cb=nint(cb/ci)*ci
       if (cb.ge.rmax) cb=cb-ci
       if (cb.le.rmin) cb=cb+ci
    endif
!KWM  else
!KWM     if (choosecb) then
!KWM        cmax=10.**(int(alog10(rmax)+50000.)-50000)
!KWM        cmin=10.**(int(alog10(rmin)+50000.)-50000+1)
!KWM        cb=10.**(nint(alog10(cb)+50000.)-50000)
!KWM        cb=min(max(cb,cmin),cmax)
!KWM     endif
!KWM     if (chooseci) then
!KWM        ci=(max(ce,cb)/min(ce,cb))**(1./(max(ncon,2)-1))
!KWM        if (ci.le.1.6) then
!KWM           ci=sqrt(2.)
!KWM        elseif (ci.le.3.5) then
!KWM           ci=2.
!KWM        elseif (ci.le.7.5) then
!KWM           ci=5.
!KWM        else
!KWM           ci=10.**nint(alog10(ci))
!KWM        endif
!KWM     endif
!KWM  endif
    cintuse=ci
!
!   Generate contour levels.
!
!KWM  if (.not.mult) then
    if (ce.lt.cb) then
       ci=-abs(ci)
    else
       ci=abs(ci)
    endif
!KWM  else
!KWM     alci=alog(ci)
!KWM     if (abs(ce).lt.abs(cb)) then
!KWM        alci=-abs(alci)
!KWM     else
!KWM        alci=abs(alci)
!KWM     endif
!KWM     ci=exp(alci)
!KWM  endif
    numcon=1
    valcon(numcon)=cb
50  numcon=numcon+1
    if (numcon.gt.maxcon) goto 100
!KWM  if (.not.mult) then
    valcon(numcon)=valcon(numcon-1)+ci
!KWM  else
!KWM     valcon(numcon)=valcon(numcon-1)*ci
!KWM  endif
    if ((ce.ge.cb.and.valcon(numcon).gt.ce).or.&
         (ce.lt.cb.and.valcon(numcon).lt.ce)) goto 100
    goto 50
100 numcon=numcon-1
!
!   Return mult contours to opposite sign, if iswitch=1
!
!KWM  if (mult) then
!KWM     if (iswitch == 1) then
!KWM        do i=1,numcon
!KWM           valcon(i)=-valcon(i)
!KWM        enddo
!KWM     endif
!KWM  endif
!
!   Determine major contours
!
    do i=1,numcon
       if (.not.mult) then
          fac=valcon(i)/((mjsk+1)*abs(ci))
       else
          fac=alog10(abs(valcon(i)))
       endif
       diff=abs(fac-float(nint(fac)))
       if (diff.lt..001) then
          imaj=i
          goto 200
       endif
    enddo
    imaj=1
200 continue
    imajrel=mod(imaj,mjsk+1)-(mjsk+1)
    do i=1,numcon
       if (.not.mult) then
          fac=valcon(i)/((mjsk+1)*abs(ci))
       else
          fac=1.
       endif
       if (abs(fac).lt.1e-4) then    ! zero contour
          valcon(i)=0.0
          majcon(i)=0
       elseif (mod(i-imajrel,mjsk+1) == 0) then
          if (valcon(i).gt.0) then ! positive major (labeled) contour
             majcon(i)=2
          else                     ! negative major (labeled) contour
             majcon(i)=-2
          endif
       else
          if (valcon(i).gt.0) then   ! positive unlabeled contour
             majcon(i)=1
          else                       ! negative unlabeled contour
             majcon(i)=-1
          endif
       endif
    enddo
!
!   Reorder if valcon(1) > valcon(numcon)
!
    if (valcon(1).gt.valcon(numcon)) then
       do i=1,numcon/2
          ii=numcon+1-i
          tmp=valcon(i)
          valcon(i)=valcon(ii)
          valcon(ii)=tmp
          itmp=majcon(i)
          majcon(i)=majcon(ii)
          majcon(ii)=itmp
       enddo
    endif
!
  end subroutine getconvals

!****************************************************************************************

  subroutine cpcolr (xcra,ycra,ncra,iaia,igia,naia)
!
!   This is a user-supplied areas subroutine which allows the user
!   to color areas in a particular way.
!
    implicit none
    integer, intent(in) :: naia, ncra
    real   , intent(in), dimension(ncra) :: xcra, ycra
    integer, intent(in), dimension(naia) :: iaia, igia

    integer :: ifll, i
!
!   Assume polygon will be filled until we find otherwise.
!
    ifll=1
!
!   If any area identifier is negative, don't fill the polygon
!
    do i=1,naia
       if (iaia(i).lt.0.) then
          ifll=0
       endif
    enddo
!
!   Otherwise, fill the polygon in the color implied by its area
!   identifier relative to edge group 3 (the contour-line group).
!
    if (ifll.ne.0) then
       ifll=0
       do i=1,naia
          if (igia(i) == 3) then
             ifll=iaia(i)
          endif
       enddo
!
!      Note that if icoindcp(ifll) is negative, that means this
!      polygon should remain transparent (i.e. not filled).
!
       if (ifll.ge.1) then
          if (ifll.le.nconarea.and.icoindcp(ifll).ge.0) then
             call gsfaci(icoindcp(ifll))
             call gfa (ncra-1,xcra,ycra)
          else
             write(*, '("ifll = ",I12)') ifll
             write(*, '("icoindcp(ifll) = ",I2)') icoindcp(ifll)
             stop "Transparent problem.  Fix it?"
          endif
       endif
    endif

  end subroutine cpcolr

!****************************************************************************************

  subroutine dfclrs
    ! A couple of predefined colors, namely, white and black.
    implicit none
    integer :: i

    print*
    print*, ' ***** '
    print*, ' ***** Do not use DFCLRS.'
    print*, ' ***** '
    print*

    nco = 0
    co%name = " "

    co(nco)= named_color("white", 1., 1., 1.)
    nco = nco + 1

    co(nco) = named_color("black", 0., 0., 0.)
    nco = nco + 1

    do i = 0, nco-1
       call gscr(1, i, co(i)%r, co(i)%g, co(i)%b)
    enddo

  end subroutine dfclrs

!****************************************************************************************

  subroutine define_color(name)
    implicit none
    character(len=*), intent(in) :: name
    integer :: i

    ! print*, 'Function DEFINE_COLOR:  name = ', trim(name)

    do i = 0, nco-1
       if (co(i)%name == name) return
    enddo

    co(nco) = get_x11_color(name)
    call gscr(1, nco, co(nco)%r, co(nco)%g, co(nco)%b)
    ! print*,' nco, r,g,b = ', nco, co(nco)%r, co(nco)%g, co(nco)%b
    nco = nco + 1

  end subroutine define_color

!****************************************************************************************

  subroutine new_color(name, r, g, b)
    implicit none
    character(len=*), intent(in) :: name
    real, intent(in) :: r, g, b
    integer :: i

    do i = 0, nco-1
       if (co(i)%name == name) then
          write(*,'("Subroutine NEW_COLOR:  color ",A," already defined.")') name
          return
       endif
    enddo

    co(nco)= named_color(name, r, g, b)
    call gscr(1, nco, co(nco)%r, co(nco)%g, co(nco)%b)
    nco = nco + 1

  end subroutine new_color

!****************************************************************************************

  integer function color_index(name) result(ci)
    implicit none
    character(len=*), intent(in) :: name
    integer :: i

    do i = 0, nco-1
       ! print*, 'i, co(i)%name, name = ', i, trim(co(i)%name), trim(name)
       if (co(i)%name == name) then
          ci = i
          return
       endif
    enddo

    ! If the color has not already been defined, define it.
    call define_color(name)
    do i = 0, nco-1
       if (co(i)%name == name) then
          ci = i
          return
       endif
    enddo

    ! If the color is STILL not defined, we've got more problems than that.
    print*, 'function COLOR_INDEX:  Color not defined: ', trim(name)
    stop

  end function color_index

!****************************************************************************************

  subroutine named_color_sequence(seqname, cbeg, cend)
    implicit none
    character(len=*) :: seqname
    real :: cbeg, cend
    integer :: i, nnco
    real :: r

    nsqmin = cbeg
    nsqmax = cend

    select case (seqname)
    case ("precip")
       call initcosq
       nnco = 16
       do i = 1, nnco
          r = (cend-cbeg)*float(i-1)/float(nnco-1) + cbeg
          select case (i)
          case (1)
             call addcosq(r, "white")
          case (2)
             call addcosq(r, "gray70")
          case (3)
             call addcosq(r, "darkgreen")
          case (4)
             call addcosq(r, "green3")
          case (5)
             call addcosq(r, "green")
          case (6)
             call addcosq(r, "gold")
          case (7)
             call addcosq(r, "goldenrod")
          case (8)
             call addcosq(r, "darkgoldenrod")
          case (9)
             call addcosq(r, "orange3")
          case (10)
             call addcosq(r, "tomato3")
          case (11)
             call addcosq(r, "red4")
          case (12)
             call addcosq(r, "darkorchid4")
          case (13)
             call addcosq(r, "darkorchid3")
          case (14)
             call addcosq(r, "slateblue2")
          case (15)
             call addcosq(r, "dodgerblue3")
          case (16)
             call addcosq(r, "cyan")
          end select

       enddo
    case ("temperature")

       call initcosq
       nnco = 6
       do i = 1, nnco
          r = (cend-cbeg)*float(i-1)/float(nnco-1) + cbeg
          select case (i)
          case (1)
             call addcosq(r, "cyan")
          case (2)
             call addcosq(r, "blue")
          case (3)
             call addcosq(r, "white")
          case (4)
             call addcosq(r, "red")
          case (5)
             call addcosq(r, "magenta")
          case (6)
             call addcosq(r, "green")
          end select
       enddo
    case default
       print*, 'NAMED_COLOR_SEQUENCE: unknown sequence name: ', trim(seqname)
    end select

  end subroutine named_color_sequence

!****************************************************************************************

  subroutine set_color_sequence(cosqstr)
    implicit none
    character(len=*), intent(in) :: cosqstr
    integer :: i, j, ierr

    call initcosq

    i = 1
    j = index(cosqstr(i:),',')
    parse : do

       nsq = nsq + 1
       read(cosqstr(i:j-1),*, iostat=ierr) rsq(nsq)
       if (ierr /= 0) then
          write(*,'("Problem reading float from:  #", A,"#" )') cosqstr(i:j)
          stop
       endif
       i = j+1
       j = index(cosqstr(i:),',')-1+i

       if (j > i) then
          cosq(nsq) = cosqstr(i:j-1)
          i = j+1
          j = index(cosqstr(i:),',')-1+i
       else
          cosq(nsq) = cosqstr(i:)
          exit
       endif
    enddo parse

    ! Make sure that the color sequence values are ascending:
    do i = 1, nsq-1
       if (rsq(i+1) < rsq(i)) then
          write(*,'("Color sequence out of order:")')
          do j = 1, nsq
             if (j == i) then
                write(*,*) "------>", rsq(j), cosq(j) , "<-------"
             else
                write(*,*) "       ", rsq(j), cosq(j)
             endif
          enddo
          stop
       endif
    enddo

  end subroutine set_color_sequence

!****************************************************************************************

  subroutine initcosq()
    implicit none
    nsq = 0
    rsq = 0
    cosq = ""
  end subroutine initcosq

!****************************************************************************************

  subroutine addcosq(rval, coname)
    implicit none
    real :: rval
    character(len=*) :: coname

    nsq = nsq + 1
    rsq(nsq)  = rval
    cosq(nsq) = coname
  end subroutine addcosq

!****************************************************************************************

  subroutine set_gscr_color_indices(is,ie,seqstr)
    ! Set all the ncargks color indices between IS and IE to the 
    ! colors listed in the SEQSTR

    implicit none

    integer, intent(in) :: is ! Index Start
    integer, intent(in) :: ie ! Index End

    character(len=*), intent(in) :: seqstr ! Color Sequence String,
    ! of the form "cval,color,cval,color,cval,color..."


    type copair_type
       real :: xseq
       character(len=64) :: hseq
       real :: r, g, b
    end type copair_type

    type(copair_type), dimension(256) :: copair

    type(named_color) :: c


    integer :: cocount

    integer :: ii, jj, ir, ig, ib
    character(len=4096) :: current
    real :: xseq
    character(len=64) :: hseq

    real :: x, minseq, maxseq, f, r, g, b, h

    ! Parse the seqstr
    current = trim(seqstr)//","

    ii = index(current,",");
    cocount = 0
    do while (ii > 0)
       read(current(1:ii-1),*) xseq
       current = current(ii+1:)
       ii = index(current,",");
       hseq = current(1:ii-1)
       current = current(ii+1:)
       ii = index(current,",")
       cocount = cocount + 1

       copair(cocount)%xseq = xseq
       copair(cocount)%hseq = hseq

       c = get_x11_color(trim(hseq))
       copair(cocount)%r = c%r
       copair(cocount)%g = c%g
       copair(cocount)%b = c%b
    enddo

    minseq = copair(1)%xseq
    maxseq = copair(cocount)%xseq

    do ii = is, ie
       ! Where are we between min and max.
       x = minseq + float(ii-is)/float(ie-is)*(maxseq-minseq)

       do jj = 1, cocount-1
          ! Where is X bounded in our color sequence list.
          if (x == copair(jj)%xseq) then
             r = copair(jj)%r
             g = copair(jj)%g
             b = copair(jj)%b
          else
             if ((x >= copair(jj)%xseq) .and. (x <= copair(jj+1)%xseq)) then
                f = (x - copair(jj)%xseq)/(copair(jj+1)%xseq-copair(jj)%xseq)
                h = 1.0-f
                r = (copair(jj)%r*h + copair(jj+1)%r*f)
                g = (copair(jj)%g*h + copair(jj+1)%g*f)
                b = (copair(jj)%b*h + copair(jj+1)%b*f)
                exit
             endif
          endif
       enddo
       call gscr(1, ii, r, g, b)
    enddo

  end subroutine set_gscr_color_indices

!****************************************************************************************

  subroutine fillbox(xmin,xmax,ymin,ymax,icol)
!
! Draw filled box
!
    implicit none

! Input:

    real, intent(in) :: xmin, xmax, ymin, ymax
    integer, intent(in) :: icol   ! Color index of the color to use.

! Local:
    integer, parameter :: nra = 4
    integer, parameter :: nim = 2
    integer, parameter :: nnd = nra+2*nim
    integer, parameter :: nst = nra+nim

    real, dimension(nst) :: dst
    real, dimension(nnd) :: ind
    real, dimension(nra) :: x4, y4
    real :: sz
    integer :: i

    x4(1:2) = xmin
    x4(3:4) = xmax
    y4(1) = ymin
    y4(2:3) = ymax
    y4(4) = ymin

    call sfsgfa(x4, y4, nra, dst, nst, ind, nnd, icol)
  end subroutine fillbox

!****************************************************************************************

  subroutine ngsquares(x,y,ndim,xs,icol)
!
! Draw filled squares at specified x-y coordinates
!
    implicit none

! Input:
    integer, intent(in) :: ndim   ! Number of filled squares to draw.

    real, intent(in), dimension(ndim) :: x, y ! x-y coordinates of the centers
    !  of the squares

    integer, intent(in) :: icol   ! Color index of the color to use.
    real, intent(in) :: xs        ! Size to draw the squares.

! Local:
    integer, parameter :: nra = 4
    integer, parameter :: nim = 2
    integer, parameter :: nnd = nra+2*nim
    integer, parameter :: nst = nra+nim

    real, dimension(nst) :: dst
    real, dimension(nnd) :: ind
    real, dimension(nra) :: x4, y4
    real :: sz
    integer :: i

    sz = xs * 0.5 ! Each side is (1/2)* xs away from the center.

    do i = 1, ndim
       x4(1) = x(i) - sz
       x4(2) = x4(1)
       x4(3) = x(i) + sz
       x4(4) = x4(3)

       y4(1) = y(i) - sz
       y4(2) = y(i) + sz
       y4(3) = y4(2)
       y4(4) = y4(1)

       call sfsgfa(x4, y4, nra, dst, nst, ind, nnd, icol)
    enddo
  end subroutine ngsquares

!****************************************************************************************

  subroutine kwm_color_table(input_string)
    use kwm_string_utilities
    implicit none
    character(len=*), intent(in) :: input_string
    integer :: istart, iend, iidx
    integer :: icount, ierr
    real :: xval
    character(len=64) :: hcol
    character(len=1024) :: string

    call initcosq


    string = unblank(trim(input_string))

    istart = 1
    icount = 0
    iidx = 1
    do while ( iidx > 0 )
       iidx = index(string(istart:),',')
       if (iidx == 0) then
          iend = len(string) + 1
       else
          iend = iidx + istart - 1
       endif

       if (mod(icount,2) == 0) then
          read(string(istart:iend-1),*, iostat=ierr) xval
          if (ierr /= 0) then
             write(*,'("Cannot read float from string: #",A,"#")') string(istart:iend-1)
             stop
          endif
       else
          hcol = string(istart:iend-1)
          call addcosq(xval, trim(hcol))
       endif

       istart = iend+1
       icount = icount + 1

    enddo
  end subroutine kwm_color_table

!****************************************************************************************

  subroutine find_good_range(ymin, ymax, expn, irange)
    !
    ! Given an initial <ymin> and <ymax> (minimum and maximum of some field), 
    ! return "good round numbers" for <ymin> and <ymax>.
    ! <irange> is the number of steps between our good round <ymin> and good round <ymax>
    ! <expn> is the power-of-ten scaling applied to get our round numbers.
    !
    ! There are <irange> steps of size 10.0**<expn> between output values of <ymin> and <ymax>
    !
    implicit none
    real, intent(inout)  :: ymin
    real, intent(inout)  :: ymax
    integer, intent(out) :: expn
    integer, intent(out) :: irange

    integer :: iymin
    integer :: iymax

    expn = floor(log10(ymax-ymin)) - 1

    iymin = floor   ( (10.0**(real(-expn))) * ymin )
    iymax = ceiling ( (10.0**(real(-expn))) * ymax )
    irange = (iymax-iymin)
  
    ! If we've got too many steps between iymin and iymax, 
    ! then shift to the next power-of-ten for our scaling.
    if (irange > 20) then
       iymin = floor  (real(iymin)*0.1)
       iymax = ceiling(real(iymax)*0.1)
       expn = expn + 1
       irange = (iymax-iymin)
    endif

    ymin = real(iymin) * 10.0**(real(expn))
    ymax = real(iymax) * 10.0**(real(expn))

  end subroutine find_good_range

!****************************************************************************************

  subroutine line_in_color_integer(x1, y1, x2, y2, color, line_width)
    !
    ! Draw a straight line from point (x1,y1) to point (x2,y2).
    ! The optional <color> can adjust the line color.
    ! The optional <line_width> can adjust the line width.
    !
    ! At the end of the routine, the preset color and line width
    ! revert back to the values in place before the subroutine was called.
    implicit none
    real,              intent(in) :: x1, y1, x2, y2
    integer,           intent(in) :: color
    real,    optional, intent(in) :: line_width

    integer :: ierr, color_save
    real    :: line_width_save

    if (present(line_width)) then
       call gqlwsc(ierr, line_width_save)
       call gslwsc(line_width)
    endif

    call gqplci(ierr, color_save)
    call gsplci(color)

    call frstpt(x1,y1)
    call vector(x2,y2)
    call plotif(0., 0., 2)

    call gsplci(color_save)

    if (present(line_width)) then
       call gslwsc(line_width_save)
    endif
    
  end subroutine line_in_color_integer

  subroutine line_in_color_string(x1, y1, x2, y2, color, line_width)
    !
    ! Draw a straight line from point (x1,y1) to point (x2,y2).
    ! The optional <color> can adjust the line color.
    ! The optional <line_width> can adjust the line width.
    !
    ! At the end of the routine, the preset color and line width
    ! revert back to the values in place before the subroutine was called.
    implicit none
    real,                       intent(in) :: x1, y1, x2, y2
    character(len=*),           intent(in) :: color
    real,             optional, intent(in) :: line_width

    integer :: ierr, color_save
    real    :: line_width_save

    if (present(line_width)) then
       call gqlwsc(ierr, line_width_save)
       call gslwsc(line_width)
    endif

    call gqplci(ierr, color_save)
    call gsplci(color_index(color))

    call frstpt(x1,y1)
    call vector(x2,y2)
    call plotif(0., 0., 2)

    call gsplci(color_save)

    if (present(line_width)) then
       call gslwsc(line_width_save)
    endif
    
  end subroutine line_in_color_string

!****************************************************************************************

  subroutine connect_the_dots(x, y, n, color, line_width, on_missing)
    !
    ! Given paired arrays X and Y, each of size N, connect the dots in order.
    ! Optional color and line_width settings will override the original settings.
    ! At the end of the routine, the original color and line_width settings are
    ! restored.
    !
    ! Values more negative than -1.E25 are considered bad data.  
    !    if (on_missing == "SKIP") then
    !        Skip over bad data, connecting all good data points in order.
    !    else if (on_missing == "BREAK") then
    !        Break the line at a bad data point, restarting at the next good point.
    !    endif
    !
    implicit none
    integer,                    intent(in) :: n
    real,         dimension(n), intent(in) :: x
    real,         dimension(n), intent(in) :: y
    character(len=*), optional, intent(in) :: color
    real,             optional, intent(in) :: line_width
    character(len=*), optional, intent(in) :: on_missing

    integer :: i, ierr, color_save
    real    :: line_width_save
    logical :: pd
    logical :: skip

    if (present(color)) then
       call gqplci(ierr, color_save)
       call gsplci(color_index(color))
    endif

    if (present(line_width)) then
       call gqlwsc(ierr, line_width_save)
       call gslwsc(line_width)
    endif

    if (present(on_missing)) then
       if (on_missing == "SKIP") then
          skip = .TRUE.
       else if (on_missing == "BREAK") then
          skip = .FALSE.
       else
          write(*,'("CONNECT_THE_DOTS:")')
          write(*,'("   Optional ON_MISSING should be ''SKIP'' or ''BREAK'', not ''",A,"''")') on_missing
          stop
       endif
    else
       skip = .TRUE.
    endif

    pd = .FALSE.

    if (skip) then
       do i = 1, n
          if (y(i) > -1.E25) then
             if (pd) then
                call vector(x(i), y(i))
             else
                call frstpt(x(i), y(i))
                pd = .TRUE.
             endif
          endif
       enddo
    else
       do i = 1, n
          if (y(i) > -1.E25) then
             if (pd) then
                call vector(x(i), y(i))
             else
                call frstpt(x(i), y(i))
                pd = .TRUE.
             endif
          else
             if (pd) then
                call plotif(0., 0., 2)
                pd = .FALSE.
             endif
          endif
       enddo
    endif
    call plotif(0., 0., 2)

    if (present(color)) then
       call gsplci(color_save)
    endif

    if (present(line_width)) then
       call gslwsc(line_width_save)
    endif

  end subroutine connect_the_dots
!****************************************************************************************

end module kwm_plot_utilities
