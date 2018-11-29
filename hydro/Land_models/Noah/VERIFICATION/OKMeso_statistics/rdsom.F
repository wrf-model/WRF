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

module module_ver
  implicit none
  type mapinfo_type
     integer :: ix
     integer :: jx

     character(len=40) :: projection
     real    :: dx
     real    :: dy
     real    :: xlonc
     real    :: reflat
     real    :: reflon
     real    :: refx
     real    :: refy
     real    :: truelat1
     real    :: truelat2
  end type mapinfo_type

  type(mapinfo_type) :: map

end module module_ver

program ver
  use kwm_date_utilities
  use kwm_plot_utilities
  use module_ver
  implicit none

  integer, parameter :: maxstn = 200
  integer, parameter :: maxtime = 2500
  integer :: nstation
  character(len=4), dimension(maxstn) :: station
  real, dimension(maxstn) :: stnlat, stnlon, stnx, stny
  integer :: i, idt, indx

!  character(len=10), parameter :: startdate = "2002051300"
  character(len=10), parameter :: startdate = "2002030100"
  character(len=10), parameter :: enddate   = "2002063000"
  character(len=10) :: nowdate

  real, dimension(4, maxstn, maxtime) :: wc, sm

  character(len=256) :: wrfstatic ! wrfstatic filename

  call getarg(1, wrfstatic)

  print*, 'loc(station) = ', loc(station)


  ! Read the wrfstatic file to get map information
  call rdwrfstatic(wrfstatic)

  ! Find the OK Mesonet stations.
  call rdgeomeso(station, nstation, stnlat, stnlon, maxstn, 0)

  ! Find x/y
  do i = 1, nstation
     call lltoxy_generic(stnlat(i), stnlon(i), stnx(i), stny(i), &
          map%projection, 1.E-3*map%dx, map%reflat, map%reflon, map%refx, map%refy,&
          map%xlonc, map%truelat1, map%truelat2)
  enddo

  ! Loop over time
  call geth_newdate(nowdate, startdate, -1)
  DATELOOP: do while ( nowdate < enddate)
     call geth_newdate(nowdate, nowdate, 1)
     call geth_idts(nowdate, startdate, idt)
     indx = idt + 1
     print*, 'nowdate = ', nowdate, indx

     ! Get station data from the OK Mesonet files
     do i = 1, nstation
        call rdsom(station(i), nowdate//"00", wc(1:4,i,indx))
     enddo

     ! Get corresponding HRLDAS results from HRLDAS files
     call rdhrldas(station, stnlat, stnlon, nstation, nowdate, sm(1:4,1:nstation,indx))
     
  enddo DATELOOP

  ! Make some plots
  call kwm_init_ncargks("sm.cgm")
  do i = 1, nstation
     call pltsm(station(i), wc(1:2,i,1:indx), sm(1:2,i,1:indx), 2, indx, startdate, &
          stnlat(i), stnlon(i), stnx(i), stny(i))
  enddo
  call kwm_close_ncargks

end program ver

subroutine pltsm(stn, wc, sm, nlev, ndim, startdate, stnlat, stnlon, stnx, stny)
  use kwm_plot_utilities
  use kwm_date_utilities
  implicit none
  character(len=*),           intent(in) :: stn
  integer,                    intent(in) :: nlev
  integer,                    intent(in) :: ndim
  real, dimension(nlev,ndim), intent(in) :: wc
  real, dimension(nlev,ndim), intent(in) :: sm
  character(len=*),           intent(in) :: startdate
  real,                       intent(in) :: stnlat
  real,                       intent(in) :: stnlon
  real,                       intent(in) :: stnx
  real,                       intent(in) :: stny

  integer :: i,n
  character(len=256) :: text
  character(len=10)  :: enddate

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.14, 0.85, stn, 0.020, 0., -1.)
  write(text,'("Lat/Lon = ", F6.2, 1x, F7.2)') stnlat, stnlon
  call pchiqu(0.10, 0.81, trim(text), 0.012, 0., -1.)
  write(text,'("X/Y = ", F6.2, 1x, F7.2)') stnx, stny
  call pchiqu(0.10, 0.78, trim(text), 0.012, 0., -1.)
  text = "from "//startdate(1:4)//"-"//startdate(5:6)//"-"//startdate(7:8)//" "//startdate(9:10)//" Z"
  call pchiqu(0.44, 0.85, trim(text), 0.012, 0., -1.)

  call geth_newdate(enddate(1:10), startdate(1:10), ndim-1)
  text = "to   "//enddate(1:4)//"-"//enddate(5:6)//"-"//enddate(7:8)//" "//enddate(9:10)//" Z"
  call pchiqu(0.44, 0.82, trim(text), 0.012, 0., -1.)

  call gasetc("YLF", '(F5.2)')
  call gasetc("XLF", '(I4)')

  do n = 1, nlev

     if (n == 1) then
        call set(.1, .9, .5, .75, 1., float(ndim), 0., 0.5, 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 1., float(ndim), 0., 0.5, 1)
     endif
        
     call periml(0,0,5,1)

     call gsplci(color_index("blue"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i), wc(n,i))
        else
           call vector(float(i), wc(n,i))
        endif
     enddo
     call plotif(0., 0., 2)

     call gsplci(color_index("red"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i), sm(n,i))
        else
           call vector(float(i), sm(n,i))
        endif
     enddo
     call plotif(0., 0., 2)
     call gsplci(color_index("black"))
  enddo
  call frame()
end subroutine pltsm


subroutine rdgeomeso(station, nstation, stnlat, stnlon, maxstn, iverbose)
  implicit none
  integer,                             intent(in)  :: maxstn
  character(len=4), dimension(maxstn), intent(out) :: station
  real, dimension(maxstn),             intent(out) :: stnlat, stnlon
  integer,                             intent(out) :: nstation
  integer,                             intent(in) :: iverbose

  character(len=256) :: string
  integer :: ierr

  integer, parameter :: iunit = 10

  station = "    "
  stnlat = -999.0
  stnlon = -999.0
  nstation = 0

  open(iunit, file="/scholar/kmanning/more_hrldas/OKMESO/geomeso.tbl", &
       status='old', form='formatted', action='read')

  PRELOOP : do
     read(iunit, '(A256)', iostat=ierr) string
     if (ierr /= 0) stop "PRELOOP"
     if (string(1:6) == "[STOP]") exit PRELOOP
  enddo PRELOOP

  RDLOOP : do
     read(iunit, '(A256)', iostat=ierr) string
     if (ierr /= 0) exit RDLOOP

     nstation = nstation + 1
     read(string(6:9), '(A4)', iostat=ierr) station(nstation)
     if (ierr /= 0) stop "RDGEOMESO:  problem extracting station name"
     read(string(64:81), '(F8.4,1x,F9.4)', iostat=ierr) stnlat(nstation), stnlon(nstation)
     if (ierr /= 0) stop "RDGEOMESO:  problem extracting lat/lon"
     if (iverbose > 0) print*, station(nstation), stnlat(nstation), stnlon(nstation)

  enddo RDLOOP
  close(iunit)
end subroutine rdgeomeso
  

subroutine rdsom(stn, idate, wc)
  implicit none
  character(len=*), intent(in) :: stn, idate
  real, dimension(4), intent(out) :: wc
  integer :: ierr
  character(len=256) :: string, flnm
  character(len=12) :: rddate ! yyyymmddhhmm
  character(len=4)  :: rdstn

  integer :: i

  real, dimension(4) :: st, tr, mp
  integer, dimension(4) :: stq, trq, mpq, wcq
  real :: tref
  integer, parameter :: iunit = 10
  wc = -99999.


!  write(flnm, &
!   '("/wig/kmanning/HRLDAS_WRFDRIVER/VERIFICATION/OKMeso_Soil/",A8,".som")') &
!   idate(1:8)

  write(flnm, &
       '("/wig/kmanning/HRLDAS_WRFDRIVER/VERIFICATION/OKMeso_SoilMoisture_SOM/",A8,".som")') &
   idate(1:8)
  
  open(iunit, file=trim(flnm), status='old', form='formatted', action='read')
  
  RDLOOP : do
     read(iunit, '(A256)', iostat=ierr) string
     if (ierr /= 0) exit RDLOOP
     read(string(6:17), '(A12)', iostat=ierr) rddate
     if (ierr /= 0) stop "Problem extracting date"
     if (rddate == idate) then
        read(string(1:4), '(A4)', iostat=ierr) rdstn
        if (ierr /= 0) stop "Problem extracting station name"
        if (rdstn == stn) then
!KWM           print*, trim(string)
           read(string(19:),*, iostat=ierr) ((st(i), stq(i), tr(i), trq(i), mp(i), mpq(i), wc(i), wcq(i)), i=1,4), &
                tref
           if (ierr /= 0) stop "Problem extracting data"
!KWM           print*, 'st = ', st
!KWM           print*, 'tr = ', tr
!KWM           print*, 'mp = ', mp
!KWM           print*, 'wc = ', wc
!KWM           print*, 'tref = ', tref
           exit RDLOOP
        endif
     endif
  enddo RDLOOP

  close(iunit)

end subroutine rdsom

subroutine rdwrfstatic(wrfstatic)
  use module_ver
  implicit none
  character(len=*), intent(in) :: wrfstatic

#include <netcdf.inc>
  integer :: ncid, iret, dimid
  real, dimension(16) :: corner_lat, corner_lon

  ! Open the NetCDF file, and get mapping information.
  write(*,'("wrfsi_static_flnm: ''", A, "''")') trim(wrfstatic)
    iret = nf_open(wrfstatic, NF_WRITE, ncid)
    if (iret /= 0) then
       write(*,'("Problem opening wrfsi_static file: ''", A, "''")') &
            trim(wrfstatic)
       stop
    endif

    iret = nf_inq_dimid(ncid, "west_east", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  west_east"
    endif

    iret = nf_inq_dimlen(ncid, dimid, map%ix)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  west_east"
    endif

    iret = nf_inq_dimid(ncid, "south_north", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  south_north"
    endif

    iret = nf_inq_dimlen(ncid, dimid, map%jx)
    if (iret /= 0) then
       stop "nf_inq_dimlen:  south_north"
    endif

    iret = nf_inq_dimid(ncid, "land_cat", dimid)
    if (iret /= 0) then
       stop "nf_inq_dimid:  land_cat"
    endif

!KWM    iret = nf_inq_dimlen(ncid, dimid, land_cat)
!KWM    if (iret /= 0) then
!KWM       stop "nf_inq_dimlen:  land_cat"
!KWM    endif
!KWM
!KWM    iret = nf_inq_dimid(ncid, "soil_cat", dimid)
!KWM    if (iret /= 0) then
!KWM       stop "nf_inq_dimid:  soil_cat"
!KWM    endif
!KWM
!KWM    iret = nf_inq_dimlen(ncid, dimid, soil_cat)
!KWM    if (iret /= 0) then
!KWM       stop "nf_inq_dimlen:  soil_cat"
!KWM    endif
!KWM

! Various map attributes

    iret = nf_get_att_text(ncid, NF_GLOBAL, "map_projection", map%projection)
    if (iret /= 0) then
       stop "nf_get_att_float:  projection"
    endif

    if (map%projection(1:17) == "LAMBERT CONFORMAL") map%projection = "LC"

    iret = nf_get_att_real(ncid, NF_GLOBAL, "TRUELAT1", map%truelat1)
    if (iret /= 0) then
       stop "nf_get_att_float:  truelat1"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "TRUELAT2", map%truelat2)
    if (iret /= 0) then
       stop "nf_get_att_float:  truelat2"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "DX", map%dx)
    if (iret /= 0) then
       stop "nf_get_att_float:  dx"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "DY", map%dy)
    if (iret /= 0) then
       stop "nf_get_att_float:  dy"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "STAND_LON", map%xlonc)
    if (iret /= 0) then
       stop "nf_get_att_float:  xlonc"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "corner_lats", corner_lat)
    if (iret /= 0) then
       stop "nf_get_att_float:  corner_lat"
    endif

    iret = nf_get_att_real(ncid, NF_GLOBAL, "corner_lons", corner_lon)
    if (iret /= 0) then
       stop "nf_get_att_float:  corner_lon"
    endif

    map%reflat = corner_lat(1)
    map%reflon = corner_lon(1)
    map%refx = 1.0
    map%refy = 1.0

    iret = nf_close(ncid)

end subroutine rdwrfstatic

subroutine rdhrldas(stn, stnlat, stnlon, nstation, idate, sm)
  use module_ver
  implicit none
  integer,                               intent(in)  :: nstation
  character(len=*), dimension(nstation), intent(in)  :: stn
  real, dimension(nstation),             intent(in)  :: stnlat
  real, dimension(nstation),             intent(in)  :: stnlon
  character(len=*),                      intent(in)  :: idate
  real, dimension(4,nstation),           intent(out) :: sm

  character(len=256) :: flnm
#include <netcdf.inc>
  integer :: iret, ncid
  integer :: dimid, varid, idim, jdim
  real, allocatable, dimension(:,:) :: var
  real :: x, y, i, j
  integer :: ista, ilvl

  sm = -99999.

  write(flnm, &
   '("/scholar2/kmanning/VERY_TEMPORARY_HRLDAS/Run_3.2/",A10,".LDASOUT_DOMAIN1")') idate(1:10)

  
  iret = nf_open(flnm, NF_WRITE, ncid)
  if (iret /= 0) then
     write(*,'("Problem opening flnm: ''", A, "''")') &
          trim(flnm)
     stop
  endif

!KWM  iret = nf_inq_dimid(ncid, "idim", dimid)
!KWM  if (iret /= 0) then
!KWM     stop "nf_inq_dimid:  idim"
!KWM  endif
!KWM
!KWM  iret = nf_inq_dimlen(ncid, dimid, idim)
!KWM  if (iret /= 0) then
!KWM     stop "nf_inq_dimlen:  idim"
!KWM  endif
!KWM
!KWM  iret = nf_inq_dimid(ncid, "jdim", dimid)
!KWM  if (iret /= 0) then
!KWM     stop "nf_inq_dimid:  jdim"
!KWM  endif
!KWM
!KWM  iret = nf_inq_dimlen(ncid, dimid, jdim)
!KWM  if (iret /= 0) then
!KWM     stop "nf_inq_dimlen:  jdim"
!KWM  endif

!KWM  print*, 'ix, jx = ', map%ix, map%jx
!KWM  print*, 'map = ', map

  ! Read a soil-moisture field

  do ilvl = 1, 4

     iret = nf_inq_varid(ncid, "SOIL_M_"//char(ilvl+ichar("0")), varid)
     if (iret /= 0) then
        stop "nf_inq_varid:  SOIL_M_"
     endif
!KWM     print*, "SOIL_M_"//char(ilvl+ichar("0")), ' varid = ', varid

     allocate(var(map%ix, map%jx))

     iret = nf_get_var_real(ncid, varid, var)
     if (iret /= 0) then
        stop "nf_get_var_real: SOIL_M_1"
     endif

     do ista = 1, nstation
        call lltoxy_generic(stnlat(ista), stnlon(ista), x, y, &
             map%projection, 1.E-3*map%dx, map%reflat, map%reflon, map%refx, map%refy,&
             map%xlonc, map%truelat1, map%truelat2)

!        print*, 'lat, lon, x, y = ', stnlat(ista), stnlon(ista), x, y
        sm(ilvl, ista) = var(nint(x), nint(y))

     enddo

     deallocate(var)
  enddo

end subroutine rdhrldas


