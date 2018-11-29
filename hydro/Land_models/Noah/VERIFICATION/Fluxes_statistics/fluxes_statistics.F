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

program fluxes_statistics
  use kwm_date_utilities
  use kwm_grid_utilities
  use kwm_plot_utilities, only : kwm_init_ncargks, color_index, connect_the_dots, line_in_color, kwm_close_ncargks
  use module_hd
!  use v3_module
  implicit none

  character(len=256) :: flnm
  character(len=16) :: startdate = "2002-06-01_00:00"
  character(len=16) :: enddate =   "2002-06-24_00:00"

  integer :: ntim
  integer :: ista

  integer, parameter :: MAXSTA = 9
  ! integer, parameter :: MAXTIM = 9500
  integer, parameter :: MAXTIM = 4500
  real, dimension(MAXSTA) :: lat, lon, x, y
  real, dimension(MAXSTA, MAXTIM) :: hfx = -1.E33
  real, dimension(MAXSTA, MAXTIM) :: qfx_old = -1.E33
  real, dimension(MAXSTA, MAXTIM) :: qfx_corrected = -1.E33
  real, dimension(MAXSTA, MAXTIM) :: hrldas_hfx = -1.E33
  real, dimension(MAXSTA, MAXTIM) :: hrldas_qfx = -1.E33
  real, pointer, dimension(:,:) :: hfxptr, qfxptr, vegtyp, soltyp

  integer :: ierr, ibf, iaf, ihr
  character(len=16) :: nowdate
  integer, parameter :: icode = 10, iunit = 10

  character(len=16), dimension(MAXSTA,MAXTIM) :: hdate, hrldas_hdate
  integer :: itime
  logical :: lexist
  integer, dimension(0:23) :: qfxcount = 0, station_qfxcount, qfxcount_old = 0, station_qfxcount_old
  integer, dimension(0:23) :: hfxcount = 0, station_hfxcount

  real, dimension(0:23) :: qfxrmse = 0, qfxbias = 0, qfxoavg = 0, qfxmavg = 0 
  real, dimension(0:23) :: qfxrmse_old = 0, qfxbias_old = 0, qfxoavg_old = 0, qfxmavg_old = 0 
  real, dimension(0:23) :: hfxrmse = 0, hfxbias = 0, hfxoavg = 0, hfxmavg = 0

  real, dimension(0:23) :: station_qfxrmse, station_qfxbias, station_qfxoavg, station_qfxmavg
  real, dimension(0:23) :: station_qfxrmse_old, station_qfxbias_old, station_qfxoavg_old, station_qfxmavg_old

  real, dimension(0:23) :: station_hfxrmse, station_hfxbias, station_hfxoavg, station_hfxmavg

  real :: station_qfx_bias, station_qfxcorr_bias, station_hfx_bias
  real :: station_qfx_rmse, station_qfxcorr_rmse, station_hfx_rmse
  integer :: station_hfx_count, station_qfx_count, station_qfxcorr_count

  type(nf_structure) :: nfstruct
  real :: xl, xr, xb, xt, wl, wr, wb, wt
  integer :: ml
  real, dimension(0:24) :: xarray_diurnal = (/ ( itime, itime=0,24 ) /)
  integer :: narray
  real, dimension(10000) :: xarray
  real, dimension(10000) :: yarray
  real, dimension(10000) :: zarray
  real, dimension(10000) :: xxarray
  real, dimension(10000) :: yyarray
  real, dimension(10000) :: zzarray
  real, dimension(10000) :: xxxarray
  real, dimension(10000) :: yyyarray

  real, dimension(10000) :: qfxcorrobs_allsta, qfxcorrmdl_allsta
  integer                :: qfxcorr_ndim_allsta
  real                   :: qfxcorr_bias_allsta, qfxcorr_rmse_allsta, qfxcorr_correlation_allsta, qfxcorr_lsra_allsta, qfxcorr_lsrb_allsta

  real, dimension(10000) :: hfxobs_allsta, hfxmdl_allsta
  integer                :: hfx_ndim_allsta
  real                   :: hfx_bias_allsta, hfx_rmse_allsta, hfx_correlation_allsta, hfx_lsra_allsta, hfx_lsrb_allsta

  real, dimension(10000) :: qfxobs_allsta, qfxmdl_allsta
  integer                :: qfx_ndim_allsta
  real                   :: qfx_bias_allsta, qfx_rmse_allsta, qfx_correlation_allsta, qfx_lsra_allsta, qfx_lsrb_allsta

  character(len=256) :: fluxobs_directory = ""
  character(len=256) :: ldasout_directory = ""

  real :: bias
  real :: rmse
  real :: correlation
  real :: lsra
  real :: lsrb
  character(len=1) :: textsta

  real :: frame_left
  real :: frame_right
  real :: frame_bottom
  real :: frame_top
  real :: textsize

  call read_namelist(fluxobs_directory, ldasout_directory)

!KWM  open(iunit, file="mem.dump", form='unformatted')
!KWM  close(iunit, status="delete")
!KWM  inquire (file="mem.dump", exist=lexist)
!KWM
!KWM  if (.not. lexist) then

     call geth_idts(enddate, startdate, ntim)
     ntim = (ntim / 15) + 1
     print*, 'ntim = ', ntim

     ! Read the IHOP site data into arrays

     do ista = 1, maxsta
        call read_ihop(fluxobs_directory, startdate, ista, lat(ista), lon(ista),  &
             hdate(ista,:), hfx(ista,:), qfx_old(ista,:), qfx_corrected(ista,:), maxtim)
     enddo

     ! Read the HRLDAS (NetCDF-format) data into arrays
     call geth_newdate(nowdate(1:16), startdate(1:16), -60)

     do while ( nowdate <= enddate )
        call geth_newdate(nowdate, nowdate, 60)
        print*, 'nowdate = ', nowdate
!        flnm = "/avn2/kmanning/T1_TEST/Run/"&
!        flnm = "/avn2/kmanning/STATS/LDASOUT/"&
        flnm = trim(ldasout_directory)//"/" &
             //nowdate(1:4)//nowdate(6:7)// &
             nowdate(9:10)//nowdate(12:13)//".LDASOUT_DOMAIN1"
        call netcdf_open(trim(flnm), nfstruct)


!KWM  call kwm_init_ncargks("locations.cgm")
!KWM        call wrfmap(nfstruct)
!KWM        call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
!KWM        call set(xl, xr, xb, xt, 1., float(nfstruct%idim), 1., float(nfstruct%jdim), 1)

        if (nowdate == startdate) then
           do ista = 1, maxsta
              call lltoxy_hd(lat(ista), lon(ista), x(ista), y(ista), nfstruct)
!KWM CORRECT OFFSET              ! x and y are in dot-point coordinates.  
!KWM CORRECT OFFSET              ! Convert to cross-point coordinates
!KWM CORRECT OFFSET              x(ista) = x(ista) - 0.5
!KWM CORRECT OFFSET              y(ista) = y(ista) - 0.5
              print*, 'nfstruct%dskm = ', nfstruct%dskm
              print*, 'reflat, reflon = ', nfstruct%reflat, nfstruct%reflon
              print*, 'lat, lon, x, y = ', lat(ista), lon(ista), x(ista), y(ista)
!KWM              call ngdots(x(ista), y(ista), 1, 1.0, color_index("red"))
           enddo
        endif

!KWM  call kwm_close_ncargks

        call netcdf_get_field(nfstruct, "HFX", 1, hfxptr)
        call netcdf_get_field(nfstruct, "QFX", 1, qfxptr)
        ! call netcdf_get_field(nfstruct, "IVGTYP", 1, vegtyp)
        ! call netcdf_get_field(nfstruct, "ISLTYP", 1, soltyp)

        ! qfxptr = qfxptr * 2.5E6 ! For some of the older data I have that is in different units

        call geth_idts(nowdate, startdate, itime)
        itime = (itime / 15) + 1
        if (itime > maxtim) stop "Increase maxtim"
        ! print*, 'itime = ', itime, nowdate

        do ista = 1, maxsta
           ! print*, 'x y = ', x(ista), y(ista)
           hrldas_hdate(ista, itime) = nowdate
!KWM           hrldas_hfx(ista, itime) = hfxptr(nint(x(ista)),nint(y(ista)))
!KWM           hrldas_qfx(ista, itime) = qfxptr(nint(x(ista)),nint(y(ista)))
           hrldas_hfx(ista,itime) = &
                four_point(hfxptr, nfstruct%idim, nfstruct%jdim, x(ista), y(ista))
           hrldas_qfx(ista,itime) = &
                four_point(qfxptr, nfstruct%idim, nfstruct%jdim, x(ista), y(ista))
           ! print*, 'Station, vegetype, soiltype = ', ista,  &
           ! vegtyp(nint(x(ista)), nint(y(ista))), soltyp(nint(x(ista)), nint(y(ista)))
        enddo
        ! print*, hrldas_hfx(:,itime)

        ierr = nf90_close(nfstruct%ncid)

        deallocate(hfxptr, qfxptr)
        nullify(hfxptr, qfxptr)
!KWM        deallocate(vegtyp)
!KWM        nullify(vegtyp)
!KWM        deallocate(soltyp)
!KWM        nullify(soltyp)

     enddo



     ! Fill in observed flux data on the hour
     do itime = 2, maxtim-1
        do ista = 1, maxsta
           if (qfx_corrected(ista,itime) < -1.E25) then
              if ((qfx_corrected(ista,itime-1) > -1.E25) .and. &
                   (qfx_corrected(ista,itime+1) > -1.E25)) then
                 qfx_corrected(ista,itime) = &
                      0.5*(qfx_corrected(ista,itime-1)+qfx_corrected(ista,itime+1))
              endif
           endif
           if (qfx_old(ista,itime) < -1.E25) then
              if ((qfx_old(ista,itime-1) > -1.E25) .and. &
                   (qfx_old(ista,itime+1) > -1.E25)) then
                 qfx_old(ista,itime) = &
                      0.5*(qfx_old(ista,itime-1)+qfx_old(ista,itime+1))
              endif
           endif
           if (hfx(ista,itime) < -1.E25) then
              if ((hfx(ista,itime-1) > -1.E25) .and. (hfx(ista,itime+1) > -1.E25)) then
                 hfx(ista,itime) = 0.5*(hfx(ista,itime-1)+hfx(ista,itime+1))
              endif
           endif
        enddo
     enddo

     ! Fill in hrldas flux data on the off times
     TIMELOOP : do itime = 2, maxtim-1
        do ista = 1, maxsta
           if (hrldas_hdate(ista,itime) > enddate) exit TIMELOOP
           if (hdate(ista,itime) > enddate) exit TIMELOOP
!KWM           print '(A, i6, i6, f12.8, "(",A16,")" 1x, "(",A16, ")")', 'itime, ista = ', &
!KWM                itime, ista, hrldas_qfx(ista, itime), hdate(ista, itime), hrldas_hdate(ista, itime)
           if ( hrldas_qfx(ista, itime) < -1.E25 ) then
              ibf = itime
              do while ( hrldas_qfx(ista, ibf) < -1.E25)
                 ibf = ibf - 1
              enddo
              iaf = itime
              do while ( hrldas_qfx(ista, iaf) < -1.E25)
                 iaf = iaf + 1
              enddo
              hrldas_qfx(ista,itime) = (hrldas_qfx(ista,ibf)*(iaf-itime) + &
                   hrldas_qfx(ista,iaf)*(itime-ibf))/float(iaf-ibf)
           endif

           if ( hrldas_hfx(ista, itime) < -1.E25 ) then
              ibf = itime
              do while ( hrldas_hfx(ista, ibf) < -1.E25)
                 ibf = ibf - 1
              enddo
              iaf = itime
              do while ( hrldas_hfx(ista, iaf) < -1.E25)
                 iaf = iaf + 1
              enddo
              hrldas_hfx(ista,itime) = (hrldas_hfx(ista,ibf)*(iaf-itime) + &
                   hrldas_hfx(ista,iaf)*(itime-ibf))/float(iaf-ibf)
           endif

        enddo
     enddo TIMELOOP

!KWM     ! Mem Dump
!KWM     open(114, file='mem.dump', form='unformatted', action='write')
!KWM     write(114) lat, lon, x, y, hrldas_hdate, hrldas_hfx, hrldas_qfx, &
!KWM          hdate, hfx, qfx_old, qfx_corrected, ntim
!KWM     close(114)
!KWM  else
!KWM     open(114, file='mem.dump', form='unformatted', action='read')
!KWM     read(114) lat, lon, x, y, hrldas_hdate, hrldas_hfx, hrldas_qfx, &
!KWM          hdate, hfx, qfx_old, qfx_corrected, ntim
!KWM     close(114)
!KWM  endif

  ! Now that I've got everything, do some plotting:

  call kwm_init_ncargks("fluxes.cgm")
  call gaseti("LTY", 2)

  qfxcorr_ndim_allsta = 0
  hfx_ndim_allsta = 0
  qfx_ndim_allsta = 0

  STATIONLOOP : do ista = 1, maxsta

     write(textsta, '(I1)') ista
     station_qfxoavg = 0.
     station_qfxmavg = 0.
     station_qfxbias = 0.
     station_qfxrmse = 0.
     station_qfxcount = 0

     station_hfxoavg = 0.
     station_hfxmavg = 0.
     station_hfxbias = 0.
     station_hfxrmse = 0.
     station_hfxcount = 0

     station_qfxoavg_old = 0.
     station_qfxmavg_old = 0.
     station_qfxbias_old = 0.
     station_qfxrmse_old = 0.
     station_qfxcount_old = 0

     station_hfx_bias = 0.
     station_qfx_bias = 0.
     station_qfxcorr_bias = 0.

     station_hfx_count = 0.
     station_qfx_count = 0.
     station_qfxcorr_count = 0.

     station_hfx_rmse = 0.
     station_qfx_rmse = 0.
     station_qfxcorr_rmse = 0.

     call gsln(1)
     nowdate = startdate
     
     ! QFX_Corrected
     do while ( nowdate(1:13) < enddate(1:13) )
        call geth_newdate(nowdate(1:13), nowdate(1:13), 1)
        call geth_idts(nowdate(1:13), startdate(1:13), itime)
        itime = (itime * 4) + 1
        ! if (qfx_corrected(ista,itime) > -1.E25) then
        if (qfx_corrected(ista,itime) > -2000) then
           read(nowdate(12:13),*) ihr
           qfxoavg(ihr) = qfxoavg(ihr) + qfx_corrected(ista,itime+1)
           qfxmavg(ihr) = qfxmavg(ihr) + hrldas_qfx(ista,itime)
           qfxbias(ihr) = qfxbias(ihr) + (hrldas_qfx(ista,itime)-qfx_corrected(ista,itime+1))
           qfxrmse(ihr) = qfxrmse(ihr) + &
                ((hrldas_qfx(ista,itime) - qfx_corrected(ista,itime+1))**2)
           qfxcount(ihr) = qfxcount(ihr) + 1

           station_qfxoavg(ihr) = station_qfxoavg(ihr) + qfx_corrected(ista,itime+1)
           station_qfxmavg(ihr) = station_qfxmavg(ihr) + hrldas_qfx(ista,itime)
           station_qfxbias(ihr) = station_qfxbias(ihr) + (hrldas_qfx(ista,itime)-qfx_corrected(ista,itime+1))
           station_qfxrmse(ihr) = station_qfxrmse(ihr) + ((hrldas_qfx(ista,itime) - qfx_corrected(ista,itime+1))**2)
           station_qfxcount(ihr) = station_qfxcount(ihr) + 1

           station_qfxcorr_bias = station_qfxcorr_bias + (hrldas_qfx(ista,itime)-qfx_corrected(ista, itime+1))
           station_qfxcorr_rmse = station_qfxcorr_rmse + (hrldas_qfx(ista,itime)-qfx_corrected(ista, itime+1))**2

           station_qfxcorr_count = station_qfxcorr_count + 1

           xarray(station_qfxcorr_count) = float(itime)
           yarray(station_qfxcorr_count) = qfx_corrected(ista,itime+1)
           zarray(station_qfxcorr_count) = hrldas_qfx(ista,itime)

           qfxcorr_ndim_allsta = qfxcorr_ndim_allsta + 1
           qfxcorrobs_allsta(qfxcorr_ndim_allsta) = qfx_corrected(ista,itime+1)
           qfxcorrmdl_allsta(qfxcorr_ndim_allsta) = hrldas_qfx(ista,itime)

        endif
     enddo

     frame_left   = 0.0
     frame_right  = 0.5
     frame_bottom = 0.5
     frame_top    = 1.0
     xl = frame_left
     xr = frame_right
     xb = frame_bottom
     xt = frame_top
     call set ( xl , xr , xb , xt , 0.0 , 1.0 , 0.0 , 1.0 , 1 )
     call perim(0,0,0,0)
     call pchiqu(0.525, 0.95, "QFX:  Station "//textsta, 0.0075, 0.0, 0.0)
     
     xl = frame_left   + (frame_right-frame_left  )*0.15
     xr = frame_left   + (frame_right-frame_left  )*0.95
     xb = frame_bottom + (frame_top  -frame_bottom)*0.10
     xt = frame_bottom + (frame_top  -frame_bottom)*0.90
     call set ( xl , xr , xb , xt , xarray(1) , xarray(station_qfxcorr_count) , -100.0 , 500.0 , 1 )
     call gasetc("XLF", '(F5.0)')
     call gasetc("YLF", '(F5.0)')
     call gasetr("XLS", 0.015  * min(xr-xl,xt-xb))
     call gasetr("YLS", 0.015  * min(xr-xl,xt-xb))
     call gasetr("XLO", 0.008 * min(xr-xl,xt-xb))
     call gasetr("YLO", 0.008 * min(xr-xl,xt-xb))
     call gasetr("XMJ", 0.014 * min(xr-xl,xt-xb))
     call gasetr("YMJ", 0.014 * min(xr-xl,xt-xb))
     call gasetr("XMN", 0.007 * min(xr-xl,xt-xb))
     call gasetr("YMN", 0.007 * min(xr-xl,xt-xb))
     call periml(0, 0, 6, 0)
     call connect_the_dots(xarray, yarray, station_qfxcorr_count, color="blue")

     nowdate = startdate
     ! QFX_old
     do while ( nowdate(1:13) < enddate(1:13) )
        call geth_newdate(nowdate(1:13), nowdate(1:13), 1)
        call geth_idts(nowdate(1:13), startdate(1:13), itime)
        itime = (itime * 4) + 1
        if (qfx_old(ista,itime) > -2000) then
           read(nowdate(12:13),*) ihr
           qfxoavg_old(ihr) = qfxoavg_old(ihr) + qfx_old(ista,itime+1)
           qfxmavg_old(ihr) = qfxmavg_old(ihr) + hrldas_qfx(ista,itime)
           qfxbias_old(ihr) = qfxbias_old(ihr) + (hrldas_qfx(ista,itime)-qfx_old(ista,itime+1))
           qfxrmse_old(ihr) = qfxrmse_old(ihr) + &
                ((hrldas_qfx(ista,itime) - qfx_old(ista,itime+1))**2)
           qfxcount_old(ihr) = qfxcount_old(ihr) + 1

           station_qfxoavg_old(ihr) = station_qfxoavg_old(ihr) + qfx_old(ista,itime+1)
           station_qfxmavg_old(ihr) = station_qfxmavg_old(ihr) + hrldas_qfx(ista,itime)
           station_qfxbias_old(ihr) = station_qfxbias_old(ihr) + (hrldas_qfx(ista,itime)-qfx_old(ista,itime+1))
           station_qfxrmse_old(ihr) = station_qfxrmse_old(ihr) + ((hrldas_qfx(ista,itime) - qfx_old(ista,itime+1))**2)
           station_qfxcount_old(ihr) = station_qfxcount_old(ihr) + 1

           station_qfx_bias = station_qfx_bias + (hrldas_qfx(ista,itime) - qfx_old(ista, itime+1))
           station_qfx_rmse = station_qfx_rmse + (hrldas_qfx(ista,itime) - qfx_old(ista, itime+1))**2
           station_qfx_count = station_qfx_count + 1
           
           xxarray(station_qfx_count) = float(itime)
           yyarray(station_qfx_count) = qfx_old(ista,itime+1)
           zzarray(station_qfx_count) = hrldas_qfx(ista,itime)

           qfx_ndim_allsta = qfx_ndim_allsta + 1
           qfxobs_allsta(qfx_ndim_allsta) = qfx_old(ista,itime+1)
           qfxmdl_allsta(qfx_ndim_allsta) = hrldas_qfx(ista,itime)

        endif
     enddo

     ! HRLDAS qfx
     narray = 0
     nowdate = startdate
     do while ( nowdate(1:13) < enddate(1:13) )
        call geth_newdate(nowdate(1:13), nowdate(1:13), 1)
        call geth_idts(nowdate(1:13), startdate(1:13), itime)
        itime = (itime * 4) + 1
        if (hrldas_qfx(ista,itime) > -2000) then
           narray = narray + 1
           xxxarray(narray) = float(itime)
           yyyarray(narray) = hrldas_qfx(ista,itime)
        endif
     enddo

     call connect_the_dots(xxarray, yyarray, station_qfx_count, color="green")
     call connect_the_dots(xxxarray, yyyarray, narray, color="red")

     call gasetc("YLF", '(F5.0)')
     call gasetc("XLF", '(F5.0)')

     call flux_stats(yarray,  zarray,  station_qfxcorr_count, bias, rmse, correlation, lsra, lsrb)
     call draw_scatter("Obs QFX Corrected", "HRLDAS QFX", yarray, zarray, station_qfxcorr_count, &
          &            bias, rmse, correlation, lsra, lsrb, 0.5, 1.0, 0.5, 1.0)

     call flux_stats(yyarray, zzarray, station_qfx_count,     bias, rmse, correlation, lsra, lsrb)
     call draw_scatter("Obs QFX", "HRLDAS QFX", yyarray, zzarray, station_qfx_count, &
          &            bias, rmse, correlation, lsra, lsrb, 0.5, 1.0, 0., 0.5)

     station_qfxoavg = station_qfxoavg / float(station_qfxcount)
     station_qfxmavg = station_qfxmavg / float(station_qfxcount)
     station_qfxbias = station_qfxbias / float(station_qfxcount)
     station_qfxrmse = sqrt(station_qfxrmse/float(station_qfxcount))

     station_qfxoavg_old = station_qfxoavg_old / float(station_qfxcount_old)
     station_qfxmavg_old = station_qfxmavg_old / float(station_qfxcount_old)
     station_qfxbias_old = station_qfxbias_old / float(station_qfxcount_old)
     station_qfxrmse_old = sqrt(station_qfxrmse_old/float(station_qfxcount_old))

     frame_left   = 0.0
     frame_right  = 0.5
     frame_bottom = 0.0
     frame_top    = 0.5
     xl = frame_left
     xr = frame_right
     xb = frame_bottom
     xt = frame_top
     call set ( xl , xr , xb , xt , 0.0 , 1.0 , 0.0 , 1.0 , 1 )
     call perim(0,0,0,0)
     textsize = min((xr-xl),(xt-xb))*0.015
     call pchiqu(0.5, 0.95, "QFX -- Average diurnal cycle:  Station "//textsta, textsize, 0.0, 0.0)
     xl = frame_left   + (frame_right-frame_left  )*0.15
     xr = frame_left   + (frame_right-frame_left  )*0.95
     xb = frame_bottom + (frame_top  -frame_bottom)*0.10
     xt = frame_bottom + (frame_top  -frame_bottom)*0.90

     call pchiqu(0.18, 0.87, "Obs:", textsize, 0., -1.)
     call pchiqu(0.18, 0.84, "Obs Corr", textsize, 0., -1.)
     call pchiqu(0.18, 0.81, "HRLDAS:", textsize, 0., -1.)
     call line_in_color(.3, .87, .40, .87, color_index("green"))
     call line_in_color(.3, .84, .40, .84, color_index("blue"))
     call line_in_color(.3, .81, .40, .81, color_index("red"))

     call set ( xl , xr , xb , xt , xarray_diurnal(0) , xarray_diurnal(24) , -100.0 , 500.0 , 1 )
     call line_in_color(xarray_diurnal(0), 0.0, xarray_diurnal(24), 0.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 100.0, xarray_diurnal(24), 100.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 200.0, xarray_diurnal(24), 200.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 300.0, xarray_diurnal(24), 300.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 400.0, xarray_diurnal(24), 400.0, color_index("gray90"))
     call gasetc("XLF", "(I2.2)")
     call periml(24, 0, 6, 0)
  
     call connect_the_dots(xarray_diurnal, (/station_qfxoavg,station_qfxoavg(0)/), 25, color="blue")
     call connect_the_dots(xarray_diurnal, (/station_qfxoavg_old,station_qfxoavg_old(0)/), 25, color="green")
     call connect_the_dots(xarray_diurnal, (/station_qfxmavg,station_qfxmavg(0)/), 25, color="red")

     call frame()

     ! HFX

     nowdate = startdate
     do while ( nowdate(1:13) < enddate(1:13) )
        call geth_newdate(nowdate(1:13), nowdate(1:13), 1)
        call geth_idts(nowdate(1:13), startdate(1:13), itime)
        itime = (itime * 4) + 1
        if (hfx(ista,itime) > -2000) then
           read(nowdate(12:13),*) ihr
           hfxoavg(ihr) = hfxoavg(ihr) + hfx(ista,itime+1)
           hfxmavg(ihr) = hfxmavg(ihr) + hrldas_hfx(ista,itime)
           hfxbias(ihr) = hfxbias(ihr) + (hrldas_hfx(ista,itime)-hfx(ista,itime+1))
           hfxrmse(ihr) = hfxrmse(ihr) + &
                ((hrldas_hfx(ista,itime) - hfx(ista,itime+1))**2)
           hfxcount(ihr) = hfxcount(ihr) + 1

           station_hfxoavg(ihr) = station_hfxoavg(ihr) + hfx(ista,itime+1)
           station_hfxmavg(ihr) = station_hfxmavg(ihr) + hrldas_hfx(ista,itime)
           station_hfxbias(ihr) = station_hfxbias(ihr) + (hrldas_hfx(ista,itime)-hfx(ista,itime+1))
           station_hfxrmse(ihr) = station_hfxrmse(ihr) + ((hrldas_hfx(ista,itime) - hfx(ista,itime+1))**2)
           station_hfxcount(ihr) = station_hfxcount(ihr) + 1

           station_hfx_bias = station_hfx_bias + (hrldas_hfx(ista,itime) - hfx(ista, itime+1))
           station_hfx_rmse = station_hfx_rmse + (hrldas_hfx(ista,itime) - hfx(ista, itime+1))**2
           station_hfx_count = station_hfx_count + 1

           xarray(station_hfx_count) = float(itime)
           yarray(station_hfx_count) = hfx(ista,itime+1)
           zarray(station_hfx_count) = hrldas_hfx(ista,itime)

           hfx_ndim_allsta = hfx_ndim_allsta + 1
           hfxobs_allsta(hfx_ndim_allsta) = hfx(ista, itime+1)
           hfxmdl_allsta(hfx_ndim_allsta) = hrldas_hfx(ista,itime)

        endif
     enddo

     frame_left   = 0.0
     frame_right  = 0.5
     frame_bottom = 0.5
     frame_top    = 1.0
     xl = frame_left
     xr = frame_right
     xb = frame_bottom
     xt = frame_top
     call set ( xl , xr , xb , xt , 0.0 , 1.0 , 0.0 , 1.0 , 1 )
     call perim(0,0,0,0)
     textsize = min(xr-xl,xt-xb)*0.015
     call pchiqu(0.5, 0.95, "HFX:  Station "//textsta, textsize, 0.0, 0.0)
     xl = frame_left   + (frame_right-frame_left  )*0.15
     xr = frame_left   + (frame_right-frame_left  )*0.95
     xb = frame_bottom + (frame_top  -frame_bottom)*0.10
     xt = frame_bottom + (frame_top  -frame_bottom)*0.90
     wl = xarray(1)
     wr = xarray(station_hfx_count)
     wb = -100.0
     wt = 500.0
     call set (xl, xr, xb, xt, wl, wr, wb, wt, 1)
     call gasetc("XLF", '(F5.0)')
     call periml(0, 0, 0, 0)
     call connect_the_dots(xarray,yarray,station_hfx_count,color="blue")

     narray = 0
     ! HRLDAS hfx
     nowdate = startdate
     do while ( nowdate(1:13) < enddate(1:13) )
        call geth_newdate(nowdate(1:13), nowdate(1:13), 1)
        call geth_idts(nowdate(1:13), startdate(1:13), itime)
        itime = (itime * 4) + 1
        if (hrldas_hfx(ista,itime) > -1.E25) then
           narray = narray + 1
           xxxarray(narray) = float(itime)
           yyyarray(narray) = hrldas_hfx(ista,itime)
        endif
     enddo

     call connect_the_dots(xxxarray,yyyarray,narray,color="red")

     call flux_stats(yarray, zarray, station_hfx_count,     bias, rmse, correlation, lsra, lsrb)
     call draw_scatter("Obs HFX", "HRLDAS HFX", yarray, zarray, station_qfx_count, &
          &            bias, rmse, correlation, lsra, lsrb, 0.5, 1.0, 0.5, 1.0)

     station_hfxoavg = station_hfxoavg / float(station_hfxcount)
     station_hfxmavg = station_hfxmavg / float(station_hfxcount)
     station_hfxbias = station_hfxbias / float(station_hfxcount)
     station_hfxrmse = sqrt(station_hfxrmse/float(station_hfxcount))

     call gsplci(color_index("black"))
     frame_left   = 0.0
     frame_right  = 0.5
     frame_bottom = 0.0
     frame_top    = 0.5
     xl = frame_left
     xr = frame_right
     xb = frame_bottom
     xt = frame_top
     call set ( xl , xr , xb , xt , 0.0 , 1.0 , 0.0 , 1.0 , 1 )
     call perim(0,0,0,0)

     textsize = min(xr-xl,xt-xb)*0.015
     call pchiqu(0.5, 0.95, "HFX -- Average diurnal cycle: Station "//textsta, textsize, 0.0, 0.0)

     call pchiqu(0.18, 0.87, "Obs:", textsize, 0., -1.)
     call pchiqu(0.18, 0.84, "HRLDAS:", textsize, 0., -1.)
     call line_in_color(.3, .87, .40, .87, color_index("blue"))
     call line_in_color(.3, .84, .40, .84, color_index("red"))

     xl = frame_left   + (frame_right-frame_left  )*0.15
     xr = frame_left   + (frame_right-frame_left  )*0.95
     xb = frame_bottom + (frame_top  -frame_bottom)*0.10
     xt = frame_bottom + (frame_top  -frame_bottom)*0.90
     call set ( xl , xr , xb , xt , xarray_diurnal(0) , xarray_diurnal(24) , -100.0 , 500.0 , 1 )
     call line_in_color(xarray_diurnal(0), 0.0, xarray_diurnal(24), 0.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 100.0, xarray_diurnal(24), 100.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 200.0, xarray_diurnal(24), 200.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 300.0, xarray_diurnal(24), 300.0, color_index("gray90"))
     call line_in_color(xarray_diurnal(0), 400.0, xarray_diurnal(24), 400.0, color_index("gray90"))
     call periml(24,0,6,0)
  
     call connect_the_dots(xarray_diurnal, (/station_hfxoavg,station_hfxoavg(0)/), 25, color="blue")
     call connect_the_dots(xarray_diurnal, (/station_hfxmavg,station_hfxmavg(0)/), 25, color="red")

     call frame()

     write(*,'(I2,2x,8F10.4)') ista, sqrt(station_hfx_rmse/float(station_hfx_count)), &
          sqrt(station_qfx_rmse/float(station_qfx_count)), &
          sqrt(station_qfxcorr_rmse/float(station_qfxcorr_count)), &
          station_hfx_bias/float(station_hfx_count), &
          station_qfx_bias/float(station_qfx_count), &
          station_qfxcorr_bias/float(station_qfxcorr_count)

  enddo STATIONLOOP

  qfxoavg = qfxoavg / float(qfxcount)
  qfxmavg = qfxmavg / float(qfxcount)
  qfxbias = qfxbias / float(qfxcount)
  qfxrmse = sqrt(qfxrmse/float(qfxcount))

  qfxoavg_old = qfxoavg_old / float(qfxcount_old)
  qfxmavg_old = qfxmavg_old / float(qfxcount_old)
  qfxbias_old = qfxbias_old / float(qfxcount_old)
  qfxrmse_old = sqrt(qfxrmse_old/float(qfxcount_old))

  hfxoavg = hfxoavg / float(hfxcount)
  hfxmavg = hfxmavg / float(hfxcount)
  hfxbias = hfxbias / float(hfxcount)
  hfxrmse = sqrt(hfxrmse/float(hfxcount))

  ! Plot our average diurnal cycle.
  
  call gsplci(color_index("black"))
  call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.5*(.15+.9), 0.93, "Observed QFX (solid), Corrected Observed QFX (dot), HRLDAS QFX (dash)", 0.013, 0., 0.)
  call set (.15, .9, .55, .9, 0., 24., -100., 500., 1)
  call gasetc("XLF", '(I2.2)')
  call gasetc("YLF", '(F5.0)')
  call periml(8, 3, 6, 0)
  call line_in_color(0., 0., 24., 0., color_index("gray90"))
  
  call gsln(1)
  call connect_the_dots(xarray_diurnal, (/qfxoavg_old,qfxoavg_old(0)/), 25)

  call gsln(3)
  call connect_the_dots(xarray_diurnal, (/qfxoavg,qfxoavg(0)/), 25)

  call gsln(2)
  call connect_the_dots(xarray_diurnal, (/qfxmavg,qfxmavg(0)/), 25)
  call gsln(1)

  call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.5*(.15+.9), 0.46, "QFX bias w.r.t original obs (solid) and corrected obs (dot)", 0.013, 0., 0.)
  call pchiqu(0.5*(.15+.9), 0.42, "QFX rmse w.r.t original obs (dash) and corrected obs (dot-dash)", 0.013, 0., 0.)
  call set (.15, .9, .05, .40, 0., 24., -100., 200., 1)
  call gasetc("XLF", '(I2.2)')
  call gasetc("YLF", '(F5.0)')

  call line_in_color(0., -50., 24., -50., color_index("gray90"))
  call line_in_color(0., 0., 24., 0., color_index("blue2"))
  call line_in_color(0., 50., 24., 50., color_index("gray90"))
  call line_in_color(0., 100., 24., 100., color_index("gray90"))
  call line_in_color(0., 150., 24., 150., color_index("gray90"))
  call periml(8, 3, 6, 0)

  call gsln(1)
  call connect_the_dots(xarray_diurnal, (/qfxbias_old,qfxbias_old(0)/), 25)

  call gsln(3)
  call connect_the_dots(xarray_diurnal, (/qfxbias,qfxbias(0)/), 25)

  call gsln(2)
  call connect_the_dots(xarray_diurnal, (/qfxrmse_old,qfxrmse_old(0)/), 25)

  call gsln(4)
  call connect_the_dots(xarray_diurnal, (/qfxrmse,qfxrmse(0)/), 25)
  call gsln(1)

  call frame()

  call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.5*(.15+.9), 0.93, "Observed HFX (solid) and HRLDAS HFX (dash)", 0.013, 0., 0.)
  call gasetc("XLF", '(I2.2)')
  call gasetc("YLF", '(F5.0)')
  call set (.15, .9, .55, .9, 0., 24., -100., 500., 1)
  call periml(8, 3, 6, 0)

  call line_in_color(0., 0., 24., 0., color_index("gray90"))
  
  call gsln(1)
  call connect_the_dots(xarray_diurnal, (/hfxoavg,hfxoavg(0)/), 25)

  call gsln(2)
  call connect_the_dots(xarray_diurnal, (/hfxmavg,hfxmavg(0)/), 25)
  call gsln(1)

  call set (0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.5*(.15+.9), 0.43, "HFX bias (solid) and RMSE (dash)", 0.013, 0., 0.)
  call gasetc("XLF", '(I2.2)')
  call gasetc("YLF", '(F5.0)')
  call set (.15, .9, .05, .4, 0., 24., -100., 200., 1)

  call line_in_color(0., -50., 24., -50., color_index("gray90"))
  call line_in_color(0., 0., 24., 0., color_index("blue2"))
  call line_in_color(0.,  50., 24.,  50., color_index("gray90"))
  call line_in_color(0., 100., 24., 100., color_index("gray90"))
  call line_in_color(0., 150., 24., 150., color_index("gray90"))

  call periml(8, 3, 6, 0)

  
  call gsln(1)
  call connect_the_dots(xarray_diurnal, (/hfxbias, hfxbias(0)/), 25)

  call gsln(2)
  call connect_the_dots(xarray_diurnal, (/hfxrmse, hfxrmse(0)/), 25)
  call gsln(1)
  call frame()

  ! Compute our statistics over all stations
  call flux_stats(qfxcorrobs_allsta, qfxcorrmdl_allsta, qfxcorr_ndim_allsta, &
       &          qfxcorr_bias_allsta, qfxcorr_rmse_allsta, qfxcorr_correlation_allsta, &
       &          qfxcorr_lsra_allsta, qfxcorr_lsrb_allsta)

  call draw_scatter("Obs QFX Corrected", "HRLDAS QFX", qfxcorrobs_allsta, qfxcorrmdl_allsta, qfxcorr_ndim_allsta, &
       &            qfxcorr_bias_allsta, qfxcorr_rmse_allsta, qfxcorr_correlation_allsta, qfxcorr_lsra_allsta, qfxcorr_lsrb_allsta,&
       &            0.0, 1.0, 0.0, 1.0)

  call frame()

  call flux_stats(qfxobs_allsta, qfxmdl_allsta, qfx_ndim_allsta, qfx_bias_allsta, qfx_rmse_allsta, qfx_correlation_allsta, qfx_lsra_allsta, qfx_lsrb_allsta)
  call draw_scatter("Obs QFX", "HRLDAS QFX", qfxobs_allsta, qfxmdl_allsta, qfx_ndim_allsta, &
       &            qfx_bias_allsta, qfx_rmse_allsta, qfx_correlation_allsta, qfx_lsra_allsta, qfx_lsrb_allsta, &
       &            0.0, 1.0, 0.0, 1.0)
  call frame()

  call flux_stats(hfxobs_allsta, hfxmdl_allsta, hfx_ndim_allsta, hfx_bias_allsta, hfx_rmse_allsta, hfx_correlation_allsta, hfx_lsra_allsta, hfx_lsrb_allsta)
  call draw_scatter("Obs HFX", "HRLDAS HFX", hfxobs_allsta, hfxmdl_allsta, hfx_ndim_allsta, &
       &            hfx_bias_allsta, hfx_rmse_allsta, hfx_correlation_allsta, hfx_lsra_allsta, hfx_lsrb_allsta, &
       &            0.0, 1.0, 0.0, 1.0)
  call frame()

  call kwm_close_ncargks()
end program fluxes_statistics

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

subroutine read_ihop(fluxobs_directory, &
     startdate, ista, lat, lon, hdate, hfx, qfx_old, qfx_corrected, maxtim)
  use kwm_date_utilities
  use kwm_string_utilities
  implicit none
  character(len=*),                     intent(in)  :: fluxobs_directory
  character(len=16),                    intent(in)  :: startdate
  integer,                              intent(in)  :: ista
  integer,                              intent(in)  :: maxtim
  real,                                 intent(out) :: lat
  real,                                 intent(out) :: lon
  character(len=16), dimension(maxtim), intent(out) :: hdate
  real,              dimension(maxtim), intent(out) :: hfx
  real,              dimension(maxtim), intent(out) :: qfx_old
  real,              dimension(maxtim), intent(out) :: qfx_corrected

  character(len=256) :: flnm
  integer, parameter :: iunit = 26
  character(len=16) :: rddate

  integer :: rdyyyy, rdmm, rddd, rdhhmmss
  integer :: ierr, idum, itime
  real :: rd_rlw, rd_rsw

  real :: rt_T, rd_P, rd_MR, rd_SPD, rd_U, rd_V, rd_rainacc, rd_rainr
  real :: rd_Rnet, rd_Rpar, rd_Tsfc, rd_LE, rd_H, rd_Gsfc, rd_LEc
  integer :: indx
  character(len=8) :: Hrd_LEc

  character(len=256) :: string

  if (ista < 10) then
     write(flnm, fmt='(A,"/IHOPUDS",I1,"r.txt")') trim(fluxobs_directory), ista
     ! write(flnm,'("/avn2/kmanning/STATS/IHOP_DATA/Sites/LE_corrected-fromSEB/&
     ! &IHOPUDS",I1,"r.txt")') ista
  else if (ista == 10) then
     write(flnm, fmt='(A,"/IHOPUDCUS",I2,"r.txt")') trim(fluxobs_directory), ista
     ! write(flnm,'("/avn2/kmanning/STATS/IHOP_DATA/Sites/LE_corrected-fromSEB/&
     ! &IHOPUDCUS10r.txt")')
  endif

  open(iunit, file=trim(flnm), status='old', form='formatted', action='read', iostat=ierr)
  if (ierr /= 0) then
     write(*,'("***** ERROR EXIT *****",/)')
     write(*,'(3x, "Problem opening flux observations file:", /, 6x, A,/)') "'"//trim(flnm)//"'"
     write(*,'("***** ERROR EXIT *****")')
     stop
  endif

  select case (ista)
  case (1)
     lat = 36.0 + 28.370/60.
     lon = -(100.0 + 37.075/60.)
  case (2)
     lat = 36.0 + 37.327/60.
     lon = -(100.0 + 37.619/60.)
  case (3)
     lat = 36.0 + 51.662/60.
     lon = -(100.0 + 35.670/60.)
  case (4)
     lat = 37.0 + 21.474/60.
     lon = -(98.0 + 14.679/60.)
  case (5)
     lat = 37.0 + 22.684/60.
     lon = -(98.0 + 9.816/60.)
  case (6)
     lat = 37.0 + 21.269/60.
     lon = -(97.0 + 39.200/60.)
  case (7)
     lat = 37.0 + 18.792/60.
     lon = -(96.0 + 56.323/60.)
  case (8)
     lat = 37.0 + 24.418/60.
     lon = -(96.0 + 45.937/60.)
  case(9)
     lat = 37.0 + 24.618/60.
     lon = -(96.0 + 34.028/60.)
  case (10)
     ! From Joe Alfieri
     lat =  36.8764333333
     lon = -100.60890111
  end select


  ! Skip 4 header lines
  read(iunit,'(///)')
  READLOOP : do 

!
! Fore some reason, I'm having a hard time reading this file with
! the Intel compiler.  Mabye it's fixed in a later version.  For now,
! stick with pgi.
!
! Anyway, the read into character string Hrd_LEc is done because some bad data
! values in the file are encoded as the string "NaN".  Arrgh!
!
        

     if (ista == 10) then
        read(iunit,*, iostat=ierr) idum, idum, idum, idum, idum, rdyyyy, &
             rdmm, rddd, rdhhmmss, idum, idum, rd_rsw, rd_rlw, rt_T, rd_P, rd_MR, &
             rd_SPD, rd_U, rd_V, rd_rainacc, rd_rainr, rd_Rnet, rd_Rpar, &
             rd_Tsfc, rd_LE, Hrd_LEc, rd_H, rd_Gsfc 
     else
        read(iunit,*, iostat=ierr) idum, idum, idum, idum, idum, rdyyyy, &
              rdmm, rddd, rdhhmmss, idum, rd_rsw, rd_rlw, rt_T, rd_P, rd_MR, &
              rd_SPD, rd_U, rd_V, rd_rainacc, rd_rainr, rd_Rnet, rd_Rpar, &
              rd_Tsfc, rd_LE, Hrd_LEc, rd_H, rd_Gsfc
     endif
     if (ierr == -1) then
        print*, 'Hit end of file.'
        exit
     endif
     if (ierr /= 0) then
        print*, 'Hit read error.', ierr
        stop
     endif
     write(rddate, '(I4.4,2("-",I2.2),"_",I2.2,":",I2.2)') rdyyyy, rdmm, rddd, &
          rdhhmmss/10000, mod(rdhhmmss/100, 100)
     print '(I2, 2x, A, 3F20.10)', ista, rddate, rd_H, rd_le, rd_lec
     if (rddate(1:16) >= startdate(1:16)) then
        call geth_idts(rddate(1:16), startdate(1:16), itime)
        print*, 'rddate, startdate = ', rddate, '  ', startdate
        itime = (itime / 15) + 1
        if (itime > maxtim) stop "Increase maxtim."
!     if (rd_rlw < 9998) lw(itime) = rd_rlw
!     if (rd_rsw < 9998) sw(itime) = rd_rsw

        if (rd_H < 9998) HFX(itime) = rd_H
        if (rd_LE < 9998) QFX_OLD(itime) = rd_LE
        if (Hrd_LEc(1:3) /= "NaN") then
           read(Hrd_LEc, '(F7.0)', iostat=ierr) rd_lec
           if (ierr == 0) then
              if (rd_LEc < 9998) QFX_CORRECTED(itime) = rd_LEc
           else
              stop "Problem converting rd_LEc"
           endif
        endif

        hdate(itime) = rddate
!KWM     print*, rdyyyy, rdmm, rddd, rdhhmmss, hdate(icount)
     endif
  enddo READLOOP
  close(iunit)
  print*, 'finish routine read_ihop.'

end subroutine read_ihop

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

subroutine read_namelist(fluxobs_directory, ldasout_directory)
  implicit none
  character(len=256), intent(out) :: fluxobs_directory
  character(len=256), intent(out) :: ldasout_directory
  namelist/fluxstats/ fluxobs_directory, ldasout_directory

  integer :: ierr

  open(11, file="namelist.fluxstats", status='old', form='formatted', action='read', iostat=ierr)
  if (ierr /= 0) then
     write(*,'("***** ERROR EXIT *****",/)')
     write(*,'(3x, "Problem opening namelist file:", /, 6x, A,/)') "'"//"namelist.fluxstats"//"'"
     write(*,'("***** ERROR EXIT *****")')
     stop
  endif

  read(11, nml=fluxstats, iostat=ierr) 
  if (ierr /= 0) then
     write(*,'("***** ERROR EXIT *****",/)')
     write(*,'(3x, "Problem reading namelist file:", /, 6x, A,/)') "'"//"namelist.fluxstats"//"'"
     write(*,'("***** ERROR EXIT *****")')
     stop
  endif

  close(11)

  print*, 'fluxobs_directory = "'//trim(fluxobs_directory)//'"'
  
end subroutine read_namelist

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

subroutine draw_scatter(abbr1, abbr2, obs, mdl, ndim, bias, rmse, correlation, lsra, lsrb, &
     & frame_left, frame_right, frame_bottom, frame_top)
  use kwm_plot_utilities, only: line_in_color, find_good_range, color_index
  implicit none
  character(len=*),      intent(in)  :: abbr1
  character(len=*),      intent(in)  :: abbr2
  integer,               intent(in)  :: ndim
  real, dimension(ndim), intent(in)  :: obs
  real, dimension(ndim), intent(in)  :: mdl
  real,                  intent(in)  :: bias
  real,                  intent(in)  :: rmse
  real,                  intent(in)  :: correlation
  real,                  intent(in)  :: lsra
  real,                  intent(in)  :: lsrb
  real,                  intent(in)  :: frame_left
  real,                  intent(in)  :: frame_right
  real,                  intent(in)  :: frame_bottom
  real,                  intent(in)  :: frame_top

  real    :: hold_xl
  real    :: hold_xr
  real    :: hold_xb
  real    :: hold_xt
  real    :: hold_wl
  real    :: hold_wr
  real    :: hold_wb
  real    :: hold_wt
  integer :: hold_ml
  real    :: hold_xls
  real    :: hold_yls
  real    :: hold_xlo
  real    :: hold_ylo
  real    :: hold_xmj
  real    :: hold_ymj
  real    :: hold_xmn
  real    :: hold_ymn
  character(len=80) :: hold_xlf
  character(len=80) :: hold_ylf

  real :: xl
  real :: xr
  real :: xb
  real :: xt

  character(len=256) :: text
  real    :: minrange, maxrange
  integer :: expn, irange
  integer :: n
  integer :: iblue
  integer :: ired
  integer :: iblack

  real    :: text_scale1
  real    :: text_scale2

  call getset(hold_xl, hold_xr, hold_xb, hold_xt, hold_wl, hold_wr, hold_wb, hold_wt, hold_ml)
  call gagetc("XLF", hold_xlf)
  call gagetc("YLF", hold_ylf)
  call gagetr("XLS", hold_xls)
  call gagetr("YLS", hold_yls)
  call gagetr("XLO", hold_xlo)
  call gagetr("YLO", hold_ylo)
  call gagetr("XMJ", hold_xmj)
  call gagetr("YMJ", hold_ymj)
  call gagetr("XMN", hold_xmn)
  call gagetr("YMN", hold_ymn)

  
  iblue  = color_index("blue")
  ired   = color_index("red")
  iblack = color_index("black")

  xl = frame_left
  xr = frame_right
  xb = frame_bottom
  xt = frame_top

  text_scale1 = 0.014 * min((xr-xl),(xt-xb))
  text_scale2 = 0.012 * min((xr-xl),(xt-xb))

  call set(xl, xr, xb, xt, 0., 1., 0., 1., 1)
  call perim(0,0,0,0)

  write(text, '(A," / ",A, ":  Scatter Plot")') abbr1, abbr2
  call pchiqu(.55, .95, trim(text), text_scale1, 0.0, 0.0)

  write(text, '(A, " ( W m~S2~-2   )")') abbr1
  call pchiqu(.55, .05, trim(text), text_scale1, 0.0, 0.0)

  write(text, '(A, " ( W m~S2~-2   )")') abbr2
  call pchiqu(.06, .5, trim(text), text_scale1, 90.0, 0.0)

  write(text,'("Bias:  ", F10.4)') bias
  call pchiqu(0.17, 0.876, trim(text), text_scale2, 0.0, -1.0)

  write(text,'("RMSE:  ", F10.4)') rmse
  call pchiqu(0.17, 0.846, trim(text), text_scale2, 0.0, -1.0)

  write(text,'("Corr:  ", F10.4)') correlation
  call pchiqu(0.17, 0.816, trim(text), text_scale2, 0.0, -1.0)
  
  minrange = min(minval(mdl,mask=(mdl>-1.E25)), minval(obs,mask=(obs>-1.E25)))
  maxrange = max(maxval(mdl,mask=(mdl>-1.E25)), maxval(obs,mask=(obs>-1.E25)))
  call find_good_range(minrange, maxrange, expn, irange)

  xl = frame_left   + (frame_right-frame_left  )*0.15
  xr = frame_left   + (frame_right-frame_left  )*0.95
  xb = frame_bottom + (frame_top  -frame_bottom)*0.10
  xt = frame_bottom + (frame_top  -frame_bottom)*0.90

  call set(xl, xr, xb, xt, minrange, maxrange, minrange, maxrange, 1)
  call line_in_color(minrange, minrange, maxrange, maxrange, iblack)

  call line_in_color(minrange, lsra+lsrb*minrange, maxrange, lsra+lsrb*maxrange, ired)

  do n = 1, ndim
     if ( (obs(n) > -1.E25) .and. (mdl(n) > -1.E25) ) then
        call ngdots(obs(n), mdl(n), 1, (maxrange-minrange)*0.005, iblue)
     endif
  enddo
  call gasetc("XLF", "(F5.0)")
  call gasetc("YLF", "(F5.0)")
  call gasetr("XLS", 0.015  * min(xr-xl,xt-xb))
  call gasetr("YLS", 0.015  * min(xr-xl,xt-xb))
  call gasetr("XLO", 0.008 * min(xr-xl,xt-xb))
  call gasetr("YLO", 0.008 * min(xr-xl,xt-xb))
  call gasetr("XMJ", 0.014 * min(xr-xl,xt-xb))
  call gasetr("YMJ", 0.014 * min(xr-xl,xt-xb))
  call gasetr("XMN", 0.007 * min(xr-xl,xt-xb))
  call gasetr("YMN", 0.007 * min(xr-xl,xt-xb))
  call periml(irange,2,irange,2)

  ! Restore original condition
  call gasetc("XLF", trim(hold_xlf))
  call gasetc("YLF", trim(hold_ylf))
  call gasetr("XLS", hold_xls)
  call gasetr("YLS", hold_yls)
  call gasetr("XLO", hold_xlo)
  call gasetr("YLO", hold_ylo)
  call gasetr("XMJ", hold_xmj)
  call gasetr("YMJ", hold_ymj)
  call gasetr("XMN", hold_xmn)
  call gasetr("YMN", hold_ymn)
  call set(hold_xl, hold_xr, hold_xb, hold_xt, hold_wl, hold_wr, hold_wb, hold_wt, hold_ml)
end subroutine draw_scatter

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------

subroutine flux_stats(obs, mdl, ndim, bias, rmse, correlation, lsra, lsrb)
  implicit none
  integer, intent(in) :: ndim
  real, dimension(ndim), intent(in)  :: obs
  real, dimension(ndim), intent(in)  :: mdl
  real,                  intent(out) :: bias
  real,                  intent(out) :: rmse
  real,                  intent(out) :: correlation
  real,                  intent(out) :: lsra
  real,                  intent(out) :: lsrb

  integer :: n
  integer :: ndat
  
  real (kind=8) :: sumx, sumy, sumxx, sumxy, sumyy, sxx, sxy, syy

  bias = 0.0
  rmse = 0.0
  ndat = 0.0
  sumx   = 0.0
  sumy   = 0.0
  sumxx  = 0.0
  sumxy  = 0.0
  sumyy  = 0.0

  do n = 1, ndim
     if (( obs(n) > -1.E25 ) .and. ( mdl(n) > -1.E25 )) then
        bias  = bias + (mdl(n)-obs(n))
        rmse  = rmse + (mdl(n)-obs(n))*(mdl(n)-obs(n))
        sumx  = sumx + obs(n)
        sumy  = sumy + mdl(n)
        sumxx = sumxx  + ( obs(n)*obs(n) )
        sumyy = sumyy  + ( mdl(n)*mdl(n) )
        sumxy = sumxy  + ( obs(n)*mdl(n) )
        ndat = ndat + 1
     endif
  enddo

  bias = bias / real(ndat)
  rmse = sqrt ( rmse / real(ndat) )

  sxx = sumxx - sumx*sumx/real(ndat)
  syy = sumyy - sumy*sumy/real(ndat)
  sxy = sumxy - sumx*sumy/real(ndat)

  correlation = sxy/sqrt(sxx*syy)

  lsrb = ( (ndat*sumxy) - (sumx*sumy) ) / ( (ndat*sumxx) - (sumx*sumx) )
  lsra = ( sumy - (lsrb*sumx) ) / real(ndat)

end subroutine flux_stats

!--------------------------------------------------------------------------------
!--------------------------------------------------------------------------------
