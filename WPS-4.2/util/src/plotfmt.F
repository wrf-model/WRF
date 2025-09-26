program plotfmt

  use read_met_module

  implicit none
!
! Utility program to plot up files created by pregrid / SI / ungrib.
! Uses NCAR graphics routines.  If you don't have NCAR Graphics, you're 
! out of luck.
!
   INTEGER :: istatus
   integer :: idum, ilev

   CHARACTER ( LEN =132 )            :: flnm

   TYPE (met_data)                   :: fg_data

!
!   Set up the graceful stop (Sun, SGI, DEC).
!
   integer, external :: graceful_stop
#if (defined(_DOUBLEUNDERSCORE) && defined(MACOS)) || defined(NO_SIGNAL)
   ! we do not do any signaling
#else
   integer, external :: signal
#endif
   integer :: iii

#if (defined(_DOUBLEUNDERSCORE) && defined(MACOS)) || defined(NO_SIGNAL)
  ! still more no signaling
#else
  iii = signal(2, graceful_stop, -1)
#endif

  call getarg(1,flnm)

   IF ( flnm(1:1) == ' ' ) THEN
      print *,'USAGE: plotfmt.exe <filename>'
      print *,'       where <filename> is the name of an intermediate-format file'
      STOP
   END IF

  call gopks(6,idum)
  call gopwk(1,55,1)
  call gopwk(2,56,3)
  call gacwk(1)
  call gacwk(2)
  call pcseti('FN', 21)
  call pcsetc('FC', '~')

  call gscr(1,0, 1.000, 1.000, 1.000)
  call gscr(1,1, 0.000, 0.000, 0.000)
  call gscr(1,2, 0.900, 0.600, 0.600)

   CALL read_met_init(TRIM(flnm), .true., '0000-00-00_00', istatus)

   IF ( istatus == 0 ) THEN

      CALL  read_next_met_field(fg_data, istatus)

      DO WHILE (istatus == 0)

         ilev = nint(fg_data%xlvl)

         if (fg_data%iproj == PROJ_LATLON) then
            call plt2d(fg_data%slab, fg_data%nx, fg_data%ny, fg_data%iproj, &
                       fg_data%startlat, fg_data%startlon, fg_data%deltalon, &
                       fg_data%deltalat, fg_data%xlonc, fg_data%truelat1, fg_data%truelat2, &
                       fg_data%field, ilev, fg_data%units, fg_data%version, fg_data%desc, &
		       fg_data%map_source, TRIM(flnm))
         else
            call plt2d(fg_data%slab, fg_data%nx, fg_data%ny, fg_data%iproj, &
                       fg_data%startlat, fg_data%startlon, fg_data%dx, fg_data%dy, fg_data%xlonc, &
                       fg_data%truelat1, fg_data%truelat2, fg_data%field, ilev, fg_data%units, &
                       fg_data%version, fg_data%desc, fg_data%map_source, TRIM(flnm))
         end if


         IF (ASSOCIATED(fg_data%slab)) DEALLOCATE(fg_data%slab)

         CALL  read_next_met_field(fg_data, istatus)
      END DO

      CALL read_met_close()

   ELSE

      print *, 'File = ',TRIM(flnm)
      print *, 'Problem with input file, I can''t open it'
      STOP

   END IF

  call stopit

end program plotfmt

subroutine plt2d(tcr2d, iz, jz, llflag, &
     lat1, lon1, dx, dy, lov, truelat1, truelat2, &
     field, ilev, units, ifv, Desc, source, flnm)

  use misc_definitions_module

  implicit none

  integer :: llflag
  integer :: iz, jz, ifv
  real, dimension(iz,jz) :: tcr2d(iz,jz)
  real :: lat1, lon1, lov, truelat1, truelat2
  real :: dx, dy
  character(len=*) :: field
  character(len=*) ::  units
  character(len=*) :: Desc
  character(len=*) :: source
  character(len=30) :: hunit
  character(len=32) :: tmp32
  character (len=*) :: flnm

  integer :: iproj, ierr
  real :: pl1, pl2, pl3, pl4, plon, plat, rota, phic
  real :: xl, xr, xb, xt, wl, wr, wb, wt, yb
  integer :: ml, ih, i, j

  integer, parameter :: lwrk = 20000, liwk = 50000
  real, dimension(lwrk) :: rwrk
  integer, dimension(liwk) :: iwrk

  integer :: ilev
  integer :: found_it
  character(len=8) :: hlev

! declarations for windowing
  integer :: ioff, joff, i1, j1, ix, jx, funit
  real, allocatable,dimension(:,:)  :: scr2d
  logical :: is_used, lexist
  namelist /plotfmt/ ix, jx, ioff, joff
  
! This version allows the plotting of subsets of a lat/lon grid (i.e. NCEP GFS).
! ix,jx are the dimensions of the subset. ioff,joff are the offsets from 1,1

  ix = iz
  jx = jz
  ioff = 0
  joff = 0

! Read parameters from Fortran namelist
  do funit=10,100
    inquire(unit=funit, opened=is_used)
    if (.not. is_used) exit
  end do
  inquire(file='namelist.wps', exist = lexist)
  if (lexist) then
    open(funit,file='namelist.wps',status='old',form='formatted',err=1000)
    read(funit,plotfmt,iostat=found_it)
    close(funit)
    if(found_it .gt. 0 ) then
       print *,'error reading the plotfmt namelist record in namelist.wps'
       print *,'only ix, jx, ioff, joff are recognized.'
       stop 'namelist problem'
    end if
  end if


! ioff = 250     ! e.g. east of the Philippines from 0.5 degree GFS
! joff = 140
! ix = 20
! jx = 20

  if (ix+ioff .gt. iz .or. jx+joff .gt. jz) then
!   print *,'map subset is too large. Setting to full domain'
    ix = iz
    jx = jz
    ioff = 0
    joff = 0
  endif
! compute upper left point for the map   (works for NCEP GFS and godas)
  pl1 = lat1 + (joff*dy)  
  pl2 = lon1 + (ioff*dx)

  allocate (scr2d(ix,jx))

  do i = 1, ix
  do j = 1, jx
    i1 = i + ioff
    j1 = j + joff
    scr2d(i,j) = tcr2d(i1,j1)
  enddo
  enddo

  select case (llflag)
  case (PROJ_LATLON)
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'CE', pl1, pl2, &
          plon, truelat1, truelat2, dx, dy)
     plon = (pl2 + pl4) / 2.
     plat = 0.
     rota = 0.
     iproj=8
  case (PROJ_MERC)
     pl1 = lat1
     pl2 = lon1
     plon = 0.
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'ME', pl1, pl2, &
          plon, truelat1, truelat2, dx, dy)
     plat = 0.
     rota = 0
     iproj = 9
  case (PROJ_LC)
     pl1 = lat1
     pl2 = lon1
     plon = lov
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'LC', pl1, pl2,&
          plon, truelat1, truelat2, dx, dy)
     plat = truelat1
     rota = truelat2
     iproj=3
! This never used to be a problem, but currently we seem to need
! truelat1 (in plat) differ from truelat2 (in rota) for the 
! NCAR-Graphics map routines to work.  Maybe it's just a compiler
! thing.  So if the truelats are the same, we add an epsilon:
     if (abs(plat - rota) < 1.E-8) then
        plat = plat + 1.E-8
        rota = rota - 1.E-8
     endif
  case (PROJ_PS)
     print*, 'ix, jx = ', ix, jx
     print*, 'lat1, lon1 = ', lat1, lon1
     pl1 = lat1
     pl2 = lon1
     plon = lov
     if (truelat1 .lt. 0. ) then
       plat = -90.
     else
       plat = 90.
     endif
     print*, 'plon, plat = ', plon, plat
     rota = 0.
     call fmtxyll(float(ix), float(jx), pl3, pl4, 'ST', pl1, pl2,&
          plon, truelat1, truelat2, dx, dy)
     iproj=1
     print*, pl1, pl2, pl3, pl4
  case default
     print*,'Unsupported map projection ',llflag,' in input'
     stop
  end select

  call gsplci(2)   ! Use a different color for the map
! jlts = 2 means corner points are provided in p1,p2,pl3,pl4
  call supmap(iproj,plat,plon,rota,pl1,pl2,pl3,pl4,2,30,4,0,ierr)
  call gsplci(1)
! call supmap(iproj,plat+0.001,plon,rota-0.001,pl1,pl2,pl3,pl4,2,30,4,0,ierr)
  if (ierr.ne.0) then
     print*, 'supmap ierr = ', ierr
         stop
!    stop
  endif
  call getset(xl,xr,xb,xt,wl,wr,wb,wt,ml)

  write(hlev,'(I8)') ilev

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  if ( xb .lt. .16 ) then
    yb = .16    ! xb depends on the projection, so fix yb and use it for labels
  else
    yb = xb
  endif
  call pchiqu(0.1, yb-0.05, hlev//'  '//field, .020, 0.0, -1.0)
  print*, field//'#'//units//'#'//trim(Desc)
! call pchiqu(0.1, xb-0.12, Desc, .012, 0.0, -1.0)
  hunit = '                                      '
  ih = 0
  do i = 1, len(units)
     if (units(i:i).eq.'{') then
        hunit(ih+1:ih+3) = '~S~'
        ih = ih + 3
        elseif (units(i:i).eq.'}') then
        hunit(ih+1:ih+3) = '~N~'
        ih = ih + 3
     else
        ih = ih + 1
        hunit(ih:ih) = units(i:i)
     endif
  enddo
  if ( ifv .le. 3 ) then
    tmp32 = 'MM5 intermediate format'
  else if ( ifv .eq. 4 ) then
    tmp32 = 'SI intermediate format'
  else if ( ifv .eq. 5 ) then
    tmp32 = 'WPS intermediate format'
  endif
  call pchiqu(0.1, yb-0.09, hunit, .015, 0.0, -1.0)
  call pchiqu(0.1, yb-0.12, Desc, .013, 0.0, -1.0)
  call pchiqu(0.6, yb-0.12, source, .013, 0.0, -1.0)
  call pchiqu(0.1, yb-0.15, tmp32, .013, 0.0, -1.0)
  call pchiqu(0.6, yb-0.15, flnm, .013, 0.0, -1.0)

  call set(xl,xr,xb,xt,1.,float(ix),1.,float(jx),ml)

  call CPSETI ('SET - Do-SET-Call Flag', 0)
  call CPSETR ('SPV - Special Value', -1.E30)

  call cpseti('LLP', 3)

  if (dy.lt.0.) then
     call array_flip(scr2d, ix, jx)
  endif

  call cprect(scr2d,ix,ix,jx,rwrk,lwrk,iwrk,liwk)
  call cpcldr(scr2d,rwrk,iwrk)
  call cplbdr(scr2d,rwrk,iwrk)

  deallocate (scr2d)
  call frame
  return
1000 write(0,*) 'Error opening file namelist.wps, Stopping'
  stop 'namelist missing'

end subroutine plt2d

subroutine stopit
  call graceful_stop
end

subroutine graceful_stop
  call gdawk(2)
  call gdawk(1)
  call gclwk(2)
  call gclwk(1)
  call gclks
  print*, 'Graceful Stop.'
  stop
end subroutine graceful_stop

subroutine fmtxyll(x, y, xlat, xlon, project, glat1, glon1, gclon,&
     gtrue1, gtrue2, gdx, gdy)
  implicit none

  real , intent(in) :: x, y, glat1, glon1, gtrue1, gtrue2, gdx, gdy, gclon
  character(len=2), intent(in) :: project
  real , intent(out) :: xlat, xlon

  real :: gx1, gy1, gkappa
  real :: grrth = 6370., phist, phemi

  real :: r, y1
  integer :: iscan, jscan
  real, parameter :: pi = 3.1415926534
  real, parameter :: degran = pi/180.
  real, parameter :: raddeg = 180./pi
  real :: gt

  if (project.eq.'CE') then  ! Cylindrical Equidistant grid

     xlat = glat1 + gdy*(y-1.)
     xlon = glon1 + gdx*(x-1.)
     
  elseif (project == "ME") then

     gt = grrth * cos(gtrue1*degran)
     xlon = glon1 + (gdx*(x-1.)/gt)*raddeg
     y1 = gt*alog((1.+sin(glat1*degran))/cos(glat1*degran))/gdy
     xlat = 90. - 2. * atan(exp(-gdy*(y+y1-1.)/gt))*raddeg

  elseif (project.eq.'ST') then  ! Polar Stereographic grid

     if (gtrue1 .lt. 0.)then
       phemi = -1.
     else
       phemi = 1.
     endif
     r = grrth/gdx * tand((phemi*90. - glat1)/2.) * (1.+ phemi * sind(gtrue1))
     gx1 = r * sind(glon1-gclon)
     gy1 = - r * cosd(glon1-gclon)

     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = phemi*90. - 2.*atan2d((r*gdx),(grrth*(1.+ phemi*sind(gtrue1))))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1)) + gclon

  elseif (project.eq.'LC') then  ! Lambert-conformal grid

     call glccone(gtrue1, gtrue2, 1, gkappa)

     r = grrth/(gdx*gkappa)*sind(90.-gtrue1) * &
          (tand(45.-glat1/2.)/tand(45.-gtrue1/2.)) ** gkappa
     gx1 =  r*sind(gkappa*(glon1-gclon))
     gy1 = -r*cosd(gkappa*(glon1-gclon))

     r = sqrt((x-1.+gx1)**2 + (y-1+gy1)**2)
     xlat = 90. - 2.*atand(tand(45.-gtrue1/2.)* &
          ((r*gkappa*gdx)/(grrth*sind(90.-gtrue1)))**(1./gkappa))
     xlon = atan2d((x-1.+gx1),-(y-1.+gy1))/gkappa + gclon

  else

     write(*,'("Unrecoginzed projection: ", A)') project
     write(*,'("Abort in FMTXYLL",/)')
     stop

  endif
contains
  real function sind(theta)
    real :: theta
    sind = sin(theta*degran)
  end function sind
  real function cosd(theta)
    real :: theta
    cosd = cos(theta*degran)
  end function cosd
  real function tand(theta)
    real :: theta
    tand = tan(theta*degran)
  end function tand
  real function atand(x)
    real :: x
    atand = atan(x)*raddeg
  end function atand
  real function atan2d(x,y)
    real :: x,y
    atan2d = atan2(x,y)*raddeg
  end function atan2d

  subroutine glccone (fsplat,ssplat,sign,confac)
    implicit none
    real, intent(in) :: fsplat,ssplat
    integer, intent(in) :: sign
    real, intent(out) :: confac
    if (abs(fsplat-ssplat).lt.1.E-3) then
       confac = sind(fsplat)
    else
       confac = log10(cosd(fsplat))-log10(cosd(ssplat))
       confac = confac/(log10(tand(45.-float(sign)*fsplat/2.))- &
            log10(tand(45.-float(sign)*ssplat/2.)))
    endif
  end subroutine glccone

end subroutine fmtxyll

subroutine array_flip(array, ix, jx)
  implicit none
  integer :: ix, jx
  real , dimension(ix,jx) :: array

  real, dimension(ix) :: hold
  integer :: i, j, jj

  do j = 1, jx/2
     jj = jx+1-j
     hold = array(1:ix, j)
     array(1:ix,j) = array(1:ix,jj)
     array(1:ix,jj) = hold
  enddo
end subroutine array_flip

