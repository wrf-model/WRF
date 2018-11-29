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

subroutine rdmet(okmeso_met, nstation, station, idate, pcp, Tair_out, TS05_out, TS10_out, TS30_out)
  implicit none
  character(len=*),                      intent(in)  :: okmeso_met
  integer,                               intent(in)  :: nstation
  character(len=4), dimension(nstation), intent(in)  :: station
  character(len=*),                      intent(in)  :: idate
  real,             dimension(nstation), intent(out) :: pcp
  real,             dimension(nstation), intent(out) :: Tair_out
  real,             dimension(nstation), intent(out) :: TS05_out
  real,             dimension(nstation), intent(out) :: TS10_out
  real,             dimension(nstation), intent(out) :: TS30_out

  integer, save :: iunit
  integer :: ierr, i
  character(len=256) :: flnm
  character(len=256) :: string

  character(len=4) :: stid
  character(len=16) :: hdate

  real :: relh, Tair, wspd, wvec, wdir, wdsd, wssd, wmax, rain, pres, &
       srad, ta9m, ws2m, ts10, tb10, ts05, tb05, ts30, batv

  integer :: qrelh, qtair, qwspd, qwvec, qwdir, qwdsd, qwssd, qwmax, qrain, &
       qpres, qsrad, qta9m, qws2m, qts10, qtb10, qts05, qtb05, qts30
  integer :: iwdir

  real :: U,V,Qv

  character(len=256) :: fileopen = "NULL"

  ! Default values to return
  pcp = -99999.
  Tair_out = -99999.
  TS05_out = -99999.
  TS10_out = -99999.
  TS30_out = -99999.

  write(flnm, '(A, "/", A8, ".mwq.bz2")') okmeso_met, idate(1:8)

  if (trim(fileopen) /= trim(flnm)) then
     ! When we first open a file, we read past four header lines:

     if (trim(fileopen) /= "NULL") call bz_close(iunit, ierr, 1)

     call bz_open(trim(flnm)//char(0), iunit, ierr, 0)
     if (ierr /= 0) stop "bz_open returns non-zero"
     fileopen = trim(flnm)

     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) stop "bz_getline returns non-zero"
     print*, trim(string)

     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) stop "bz_getline returns non-zero"
     print*, trim(string)

     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) stop "bz_getline returns non-zero"
     print*, trim(string)

     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) stop "bz_getline returns non-zero"
     print*, trim(string)


  else
       call bz_restart(-1000)
       call bz_advance_line()
       call bz_advance_line()
       call bz_advance_line()
       call bz_advance_line()
  endif

  ! Scan forward through the file until we hit the time/date
  ! in which we are interested.
  FindDateLoop : do

     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) then
        stop "bz_getline returns non-zero"
     endif

     if ( string(8:19) == idate(1:10)//"00") then
        exit FindDateLoop
     endif
     if (string(8:19) > idate) then
        return
     endif
  enddo FindDateLoop

  ! Now scan through the records, pulling out station data, until we hit the
  ! end of file or end of this date/time.

  ! The things we want to do stuff with are:
  !    RELH:  Relative Humidity at 1.5 m      (%)
  !    Tair:  Air Temperature at 2 m          (C)
  !    WSPD:  Wind speed at 10 m              (m/s)
  !    WDIR:  Wind direction at 10 m          (degrees)
  !    PRES:  Pressure at 10 m                (hPa)
  !    RAIN:  Rainfall at 10 m,               (mm)
  !           accumulation since 00Z
  !    SRAD:  Solar Radiation at 10 m         (W/m2)
  !    WS2M:  Wind speed at 2 m               (m/s)
  !    TS05:  Soil Temperature at 5 cm        (C)
  !    TS10:  Soil Temperature at 10 cm       (C)
  !    TS30:  Soil Temperature at 30 cm       (C)
  !    TB05:  Bare Soil Temperature at 5 cm   (C)
  !    TB10:  Bare Soil Temperature at 10 cm  (C)


  RDLOOP : do

     if ( string(8:19) /= idate(1:10)//"00") exit RDLOOP

     read(string(1:208), 101, iostat=ierr) stid, &
          hdate(1:4), hdate(6:7), hdate(9:10), hdate(12:13), hdate(15:16), &
          relh, qrelh, Tair, qtair, wspd, qwspd, wvec, qwvec, iwdir, qwdir, &
          wdsd, qwdsd, wssd, qwssd, wmax, qwmax, rain, qrain, pres, qpres, &
          srad, qsrad, ta9m, qta9m, ws2m, qws2m, ts10, qts10, tb10, qtb10, &
          ts05, qts05, tb05, qtb05, ts30, qts30, batv

     if (ierr /= 0) then
        print*, 'string = "'//string//'"'
        stop
     endif

     ! Match up the station name we just read with our station table.

     StnLoop : do i = 1, nstation
        if (stid == station(i)) then

           ! Floating point wind direction
           wdir = float(iwdir)

           ! Convert temperatures to K
           if (Tair > -100) Tair = Tair + 273.15
           if (TS05 > -100) TS05 = TS05 + 273.15
           if (TS10 > -100) TS10 = TS10 + 273.15
           if (TS30 > -100) TS30 = TS30 + 273.15
           if (TB05 > -100) TB05 = TB05 + 273.15
           if (TB10 > -100) TB10 = TB10 + 273.15

           ! Convert pressure to Pa

           if (PRES > 0) PRES = PRES * 1.E2

           ! Convert RELH to Qv:
           
           call RHtoQV( RELH, Tair, PRES, Qv )

           ! Convert WSPD and WDIR to U and V

           call SDtoUV(WSPD, WDIR, U, V)

           ! Don't allow negative Short-Wave radiation.

           SRAD = max(SRAD,0.0) 

           pcp(i) = rain

           Tair_out(i) = Tair
           TS05_out(i) = TS05
           TS10_out(i) = TS10
           TS30_out(i) = TS30
           
           exit StnLoop
        endif

        if (i == nstation) print*, "station not matched:  ", stid
     enddo StnLoop

     ! Get another string for the next pass through the loop:
     call bz_getline(iunit, string, len(string), ierr)
     if (ierr /= 0) then
        stop "bz_getline returns non-zero"
     endif

  enddo RDLOOP

101 format(1x,A4,2x,A4,4A2,4(F7.1,I3),1(I6,I3),3(F7.1,I3),1(F8.2,I3),1(F9.2,I3),&
       9(F7.1,I3),F7.1)

end subroutine rdmet

subroutine RHtoQV( RH, T, P, Q )
  implicit none
  real, intent(in)  :: RH ! %
  real, intent(in)  :: T  ! K
  real, intent(in)  :: P  ! Pa
  real, intent(out) :: Q  ! kg/kg

  real, parameter :: E0    = 611.2    ! Pa
  real, parameter :: SVP2  = 17.67
  real, parameter :: SVP3  = 29.65
  real, parameter :: T00   = 273.15
  real, parameter :: EPS   = 0.622

  real :: ES, QS

  if ((T > 0) .and. ( P > 0) .and. ( RH > 0 )) then

     ES=E0*EXP(SVP2*(T-T00)/(T-SVP3))
     QS=EPS*ES/(P-ES)
     Q = RH*1.E-2*QS
  else
     Q = -999.
  endif

end subroutine RHtoQV

subroutine SDtoUV(WSPD, WDIR, U, V)
  implicit none
  real, intent(inout)  :: WSPD, WDIR
  real, intent(out) :: U, V

  real, parameter :: pi = 3.14159265
  real, parameter :: degrad = pi/180.

  if ((wspd > -400) .and. (wdir > -400)) then
     u = - wspd * sin(wdir*degrad)
     v = - wspd * cos(wdir*degrad)
  else
     u = -999
     v = -999
  endif
  
end subroutine SDtoUV
