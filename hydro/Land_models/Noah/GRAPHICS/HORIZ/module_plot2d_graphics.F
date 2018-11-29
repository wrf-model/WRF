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

module module_plot2d_graphics
  use module_2ddata

  type plt2d_parameters_type
     real :: contour_minimum
     real :: contour_maximum
     real :: contour_interval
     character(len=1024) :: color_table
     character(len=10) :: plot_type
     logical :: diff
  end type plt2d_parameters_type

  type(plt2d_parameters_type) :: plt2d_parameter

  type named_color
     character(len=20) :: name
     real :: r
     real :: g
     real :: b
  end type named_color

  logical :: mapped = .FALSE.


contains

  subroutine draw_field(fld, idim, jdim, gridinfo)
    implicit none

    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(in) :: fld
    type(gridinfo_type), intent(in) :: gridinfo

    if (plt2d_parameter%plot_type == "fillcell") then
       call fillcell(fld, idim, jdim, gridinfo)
    else if (plt2d_parameter%plot_type == "contour") then
       call plotfld(fld, idim, jdim, gridinfo)
    else if (plt2d_parameter%plot_type == "confill") then
       call fillfld(fld, idim, jdim, gridinfo)
    else
       call plotfld(fld, idim, jdim, gridinfo)
    endif

    call annotate()

    call frame()

  end subroutine draw_field

  subroutine annotate
    implicit none
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml
    character(len=256) :: txt
    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
    call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
    ! call gscr(1, 1, 0., 0., 0.)
    call pcseti("CC", 1)
   
    if ( (fieldinfo%name == "SOIL_M") .or. (fieldinfo%name == "SOIL_T") .or. &
         (fieldinfo%name == "SOIL_W")) then
       write(txt, '(A, " -- Level ", I2)') trim(fieldinfo%name), fieldinfo%level
    else
       txt = trim(fieldinfo%name)
    endif
    write(*,'(3x,"Field = ", A)') trim(txt)
    call pchiqu(0.5*(xl+xr), 0.5*(1+xt), trim(txt), 0.015, 0., 0.)
    txt = trim(fieldinfo%desc)//"   ("//trim(fieldinfo%units)//")"
    call pchiqu(0.5*(xl+xr), 0.5*(1+xt)-0.03, trim(txt), 0.013, 0., 0.)

    call pchiqu(xl, 0.5*(1+xt), fieldinfo%date, 0.015, 0., -1.)

    call set(xl, xr, xb, xt, wl, wr, wb, wt, ml)
  end subroutine annotate

!=============================================================================================

  subroutine plotfld(fld, idim, jdim, gridinfo)
    implicit none

    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(in) :: fld
    type(gridinfo_type), intent(in) :: gridinfo

    integer, parameter :: lwrk = 800000, liwk = 2000000
    real, dimension(lwrk) :: rwrk
    integer, dimension(liwk) :: iwrk

    if (.not. mapped) then
       mapped = .TRUE.
       call gflas1(0)
       call plotmap(gridinfo)
       call gflas2
    endif

    call cpseti("SET", 0)
    call cpseti("CLS", 1)
    call cpsetr("CIS", plt2d_parameter%contour_interval)
    call cpsetr("CMN", plt2d_parameter%contour_minimum)
    call cpsetr("CMX", plt2d_parameter%contour_maximum)

    call cpsetr("SPV", -1.E33)
    call cprect(fld,idim,idim,jdim,rwrk,lwrk,iwrk,liwk)
    call cplbdr(fld, rwrk, iwrk)
    call cpcldr(fld, rwrk, iwrk)

    call gflas3(0)

  end subroutine plotfld

!=============================================================================================

  subroutine fillfld(fld, idim, jdim, gridinfo)
    implicit none

    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(in) :: fld
    type(gridinfo_type), intent(in) :: gridinfo

    integer, parameter :: lwrk = 800000, liwk = 2000000
    real, dimension(lwrk) :: rwrk
    integer, dimension(liwk) :: iwrk

    integer, parameter :: niam = 10000000
    integer, parameter :: ncs = 10000000
    integer, dimension(niam) :: iam
    real,    dimension(niam) :: xcs, ycs
    integer, dimension(5000)  :: iaia, igia

    integer :: ncl
    real :: cint, cmin, cmax
    integer :: ncon, i, j, icol, labelint

    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml

    cmin = plt2d_parameter%contour_minimum
    cmax = plt2d_parameter%contour_maximum
    cint = plt2d_parameter%contour_interval

    if (.not. mapped) then
       mapped = .TRUE.
       call gflas1(0)
       call plotmap(gridinfo)
       call gflas2
    endif

    call cpseti("SET", 0)
    call cpseti("CLS", 1)
    call cpsetr("CIS", plt2d_parameter%contour_interval)
    call cpsetr("CMN", plt2d_parameter%contour_minimum)
    call cpsetr("CMX", plt2d_parameter%contour_maximum)

    call cpsetr("SPV", -1.E33)
    call cprect(fld,idim,idim,jdim,rwrk,lwrk,iwrk,liwk)

    call arinam(iam,niam) ! Initialize the area map
    call cpclam(fld,rwrk,iwrk,iam)  ! put cont. lines in area map
    call cpgeti("NCL", ncl)
    ! print*, 'ncl = ', ncl
    call dfclrsa(ncl-1)
    call arscam(iam,xcs,ycs,ncs,iaia,igia,5000,cpcolr)

    !call cplbdr(fld, rwrk, iwrk)
    !call cpcldr(fld, rwrk, iwrk)


    call gflas3(0)

    ! Add a color bar

    call colorbar(ncl-1, cmin, cmax)
  end subroutine fillfld

  subroutine colorbar(ncon, cmin, cmax)
    implicit none
    integer, intent(in) :: ncon
    real, intent(in) :: cmin, cmax
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml, labelint, i

    real :: barxmin
    real :: barxmax
    real :: barymin
    real :: barymax
    real :: yvalmn, yvalmx
    character(len=64) :: txt

    call pcseti("CC", 1)

    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
    call set(0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 1)

    barxmin = xr+0.02
    barxmax = xr+0.06
    barymin = xb;
    barymax = xt;
    ! print*, 'cmin, cmax, cint = ', cmin, cmax, cint
    ! print*,' ncon = ', ncon
    labelint = (ncon/30)+1

    do i = 0, ncon-1
       yvalmn = barymin + (barymax-barymin)*float(i)/float(ncon)
       yvalmx = barymin + (barymax-barymin)*float(i+1)/float(ncon)
       call fillbox(barxmin, barxmax, yvalmn, yvalmx, i+2)
       if (mod(i,labelint)==0) then
          write(txt, '(G12.4)') cmin + (cmax-cmin)*float(i)/float(ncon)
          call pchiqu(barxmax, yvalmn, trim(txt), 0.010, 0., -1.)
       endif
    enddo
    write(txt, '(G12.4)') cmax
    call pchiqu(barxmax, barymax, trim(txt), 0.010, 0., -1.)

    call line(barxmin, barymin, barxmin, barymax)
    call line(barxmin, barymax, barxmax, barymax)
    call line(barxmax, barymax, barxmax, barymin)
    call line(barxmax, barymin, barxmin, barymin)

    call set(xl, xr, xb, xt, wl, wr, wb, wt, ml)
  end subroutine colorbar

!=============================================================================================

  subroutine fillcell(fld, idim, jdim, gridinfo)
    implicit none

    integer, intent(in) :: idim, jdim
    real, dimension(idim,jdim), intent(in) :: fld
    type(gridinfo_type), intent(in) :: gridinfo

    real :: cint, cmin, cmax
    integer :: ncon, i, j, icol

    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml

    real :: barxmin
    real :: barxmax
    real :: barymin
    real :: barymax
    real :: yvalmn, yvalmx
    character(len=64) :: txt
    integer :: labelint

    if (.not. mapped) then
       mapped = .TRUE.
       call gflas1(0)
       call plotmap(gridinfo)
       call gflas2
    endif

    cint = plt2d_parameter%contour_interval
    cmin = plt2d_parameter%contour_minimum
    cmax = plt2d_parameter%contour_maximum
    ! maxf = maxval(fld, mask=(fld/=-1.E33))
    ! minf = minval(fld, mask=(fld/=-1.E33))

    ! Find contour minimum, contour maximum as an integer number of 
    ! contour_intervals from zero

    ncon = (cmax-cmin)/cint
    call dfclrsa(ncon)

    do i = 1, idim
       do j = 1, jdim
          if (fld(i,j) > -1.E32) then
             if (fld(i,j) >= cmax) then
                icol = ncon+1
             elseif (fld(i,j) <= cmin) then
                icol = 2
             else
                icol = (fld(i,j)-cmin)/cint + 2
             endif
             call ngsquares((/float(i)/),(/float(j)/), 1, 1., icol)
          endif
       enddo
    enddo

    call gflas3(0)

    call colorbar(ncon, cmin, cmax)

  end subroutine fillcell

!=============================================================================================

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
#ifdef dead          
          if (ifll.le.nconarea.and.icoindcp(ifll).ge.0) then
             call gsfaci(icoindcp(ifll))
#else
!             print*,' fill color ', ifll
             call gsfaci(ifll)
#endif
             call gfa (ncra-1,xcra,ycra)
#ifdef dead
          else
             write(*, '("ifll = ",I)') ifll
             write(*, '("icoindcp(ifll) = ",I)') icoindcp(ifll)
             stop "Transparent problem.  Fix it?"
          endif
#endif
       endif
    endif

  end subroutine cpcolr

!=============================================================================================

  subroutine dfclrsa(n)
    implicit none
    integer, intent(in) :: n
    integer :: i, indx, m
    real, dimension(0:255) :: xval, yval
    character(len=24), dimension(0:255) :: colname
    type(named_color) :: cb, ct
    integer :: ncols
    real :: z, fr

    xval = -99999.
    i = 1
    m = 0
    do
       indx = index(plt2d_parameter%color_table(i:), ",")
       if (indx <= 0) then
          colname((m-1)/2) = trim(plt2d_parameter%color_table(i:))
          exit
       endif
       indx = indx + i
       if (mod(m,2)==0) then
          read(plt2d_parameter%color_table(i:indx-2),*) xval(m/2)
       else
          colname((m-1)/2) = plt2d_parameter%color_table(i:indx-2)
       endif
       i = indx
       m = m + 1
    enddo
    ncols = (m/2)+1

    ! Define <n> color levels from our set of <ncols> colors.
    ! Map our <ncols> xval values to our <n> integer values.

    do i = 0, ncols-1
       ! yval(i) = (xval(i)/xval(ncols-1))*(n-1)
       yval(i) = (xval(i)-xval(0)) / (xval(ncols-1)-xval(0)) * (n-1)
       ! print*, xval(i), trim(colname(i)), yval(i)
    enddo

    do i = 0, n-1
       ! Search for the bracketing colors
       do m = 1, ncols-1
          if ( (i >= yval(m-1)) .and. (i <= yval(m)) ) then
             ! print*, i, m, yval(m-1), yval(m)
             cb = get_x11_color(colname(m-1))
             ct = get_x11_color(colname(m))
             !z = (float(i)/float(n-1))*xval(ncols-1)
             !fr = (z-(xval(m-1)))/(xval(m)-xval(m-1))
             z = xval(0) + (float(i)/float(n-1))*(xval(ncols-1)-xval(0))
             fr = (z-(xval(m-1)))/(xval(m)-xval(m-1))
             ! print*, i, z, (m-1), fr

             call gscr(1, i+2, &
                  (ct%r*fr)+(cb%r*(1-fr)), &
                  (ct%g*fr)+(cb%g*(1-fr)), &
                  (ct%b*fr)+(cb%b*(1-fr)) )
             if (m==ncols-1) then
                call gscr(1, i+3, &
                     (ct%r*fr)+(cb%r*(1-fr)), &
                     (ct%g*fr)+(cb%g*(1-fr)), &
                     (ct%b*fr)+(cb%b*(1-fr)) )
             endif
             exit
          endif
       enddo
       
       !call gscr(1, i+2, float(i)/float(n-1), float(i)/float(n-1), float(i)/float(n-1))
       !call gscr(1, i+2, float(i)/float(n-1), float(i)/float(n-1), float(i)/float(n-1))
    enddo

  end subroutine dfclrsa

!=============================================================================================

  type(named_color) function get_x11_color(name) result (c)
    use kwm_string_utilities
    implicit none
    character(len=*) :: name
    integer :: r, g, b, i
    character(len=20) :: cname

    logical, save :: already_read = .FALSE.
    integer :: ierr, idx
    character(len=80) :: string, stringname
    integer, parameter :: iunit = 22

    real, dimension(2500), save :: readlist_r, readlist_g, readlist_b
    character(len=64), dimension(2500), save :: readlist_name
    integer, save :: readlist_count = 0

    if (name(1:1) == "#") then
       read(name(2:7), '(Z2,Z2,Z2)') r,g,b
       c = named_color(trim(name), float(r)/255., float(g)/255., float(b)/255.)
       return
    else if ((name(1:2) == "Ox") .or. (name(1:2) == "0x")) then
       read(name(3:8), '(Z2,Z2,Z2)') r,g,b
       c = named_color(trim(name), float(r)/255., float(g)/255., float(b)/255.)
       return
    endif

    if (.not. already_read) then
       already_read = .TRUE.
       open(iunit, file="/usr/lib/X11/rgb.txt", form='formatted', &
            status='old', action='read', iostat=ierr)
       if (ierr /= 0) then
          open(iunit, file="/usr/share/X11/rgb.txt", form='formatted', &
               status='old', action='read', iostat=ierr)
          if (ierr /= 0) then
             stop "color table not found in /usr/lib/X11/rgb.txt or /usr/share/X11/rgb.txt"
          endif
       endif
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
    do i = 1, readlist_count
       if (name == readlist_name(i)) then
          cname = name
          c = named_color(cname, readlist_r(i), readlist_g(i), readlist_b(i))
          return
       endif
    enddo

    write(*, '("color not found: ", A)') trim(name)

  end function get_x11_color

!=============================================================================================

  subroutine init_ncargks(new_cgm_name)
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
    
    ! call define_color("white")
    ! call define_color("black")
    call gscr(1, 0, 1., 1., 1.) ! White
    call gscr(1, 1, 0., 0., 0.) ! Black
    call pcseti("FN", 21)
    call pcsetc("FC", "~")
  end subroutine init_ncargks

  subroutine close_ncargks
    implicit none
    call gdawk(1)
    call gdawk(2)
    call gclwk(1)
    call gclwk(2)
    call gclks
  end subroutine close_ncargks

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


  subroutine plotmap(gridinfo)
    implicit none
    type(gridinfo_type), intent(in) :: gridinfo
    real :: plm1, plm2, plm3, plm4
    character(len=2), parameter, dimension(3) :: project = (/"LC", "ST", "ME"/)
    integer :: ierr
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml

    call xytoll_generic(1.0, 1.0, plm1, plm2,  &
         project(gridinfo%iproj), gridinfo%dxm*0.001, gridinfo%lat1, gridinfo%lon1, 1.5, 1.5, &
         gridinfo%xlonc, gridinfo%truelat1, gridinfo%truelat2)

    call xytoll_generic(real(gridinfo%idim), real(gridinfo%jdim), plm3, plm4,  &
         project(gridinfo%iproj), gridinfo%dxm*0.001, gridinfo%lat1, gridinfo%lon1, 1.5, 1.5, &
         gridinfo%xlonc, gridinfo%truelat1, gridinfo%truelat2)

    call mappos(0.05, 0.85, 0.05, 0.85)

    if (project(gridinfo%iproj) == "LC") then
       ! Lambert Conformal
       call supmap(3, gridinfo%truelat1, gridinfo%xlonc, gridinfo%truelat2, &
            plm1, plm2, plm3, plm4, 2, 0, 4, 0, ierr)
    else
       write(*,'("Subroutine plotmap:  Unrecognized map projection: ",A)') project(gridinfo%iproj)
       stop
    endif

    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
    call set(xl, xr, xb, xt, 1.0, float(gridinfo%idim), 1.0, float(gridinfo%jdim), ml)


  end subroutine plotmap




end module module_plot2d_graphics

