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

  integer, parameter :: maxstn = 200
  integer, parameter :: maxtime = 5000

  type mapinfo_type
     sequence
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

  type memory_type
     sequence
     integer :: indx
     integer :: nstation
     character(len=4), dimension(maxstn) :: station
     real, dimension(maxstn) :: stnlat
     real, dimension(maxstn) :: stnlon
     real, dimension(maxstn) :: stnx
     real, dimension(maxstn) :: stny
     real, dimension(4, maxstn, maxtime) :: wc
     real, dimension(4, maxstn, maxtime) :: sm
     real, dimension(4, maxstn, maxtime) :: st

     real, dimension(maxstn, maxtime) :: okpcp
     real, dimension(maxstn, maxtime) :: okTair
     real, dimension(maxstn, maxtime) :: okTs05
     real, dimension(maxstn, maxtime) :: okTs10
     real, dimension(maxstn, maxtime) :: okTs30

  end type memory_type

  type(memory_type), target :: mem

end module module_ver

program ver
  use kwm_date_utilities
  use kwm_plot_utilities
  use module_ver
  use module_hd
  implicit none

  integer :: i, n, idt

  character(len=10) :: startdate
  character(len=10) :: enddate
  character(len=10) :: nowdate

  character(len=256) :: okmeso_soil_dir  ! Directory where OKMeso soil data reside
  character(len=256) :: okmeso_met_dir   ! Directory where OKMeso meteorological data reside
  character(len=256) :: ldasout_dir      ! Directory where the LDASOUT files reside

  logical :: lexist

  real :: hold, tmp
  integer, pointer :: nstation, indx
  real, pointer, dimension(:) :: stnlat, stnlon, stnx, stny
  real, pointer, dimension(:,:) :: okpcp, okTair, okTs05, okTs10, okTs30, okts
  real, pointer, dimension(:, :, :) :: wc, sm, st

  integer, dimension(2,maxstn,maxtime) :: tflag, mflag
  real, dimension(2, maxtime) :: wa, sa, smcount, stcount, sta, okta
  real, dimension(2) :: avsmbias, avsmrmse, avwcmean, avsmmean
  real, dimension(2) :: avstbias, avstrmse, avstamean, avoktmean

  real, allocatable, dimension(:,:) :: diurnal_obs, diurnal_mdl

  type(nf_structure) :: nfstruct_for_map
  integer :: iret


  nstation => mem%nstation
  indx     => mem%indx
  stnlat   => mem%stnlat
  stnlon   => mem%stnlon
  stnx     => mem%stnx
  stny     => mem%stny
  okpcp    => mem%okpcp
  okTair   => mem%okTair
  okTs05   => mem%okTs05
  okTs10   => mem%okTs10
  okTs30   => mem%okTs30
  sm       => mem%sm
  wc       => mem%wc
  st       => mem%st

  call read_namelist(okmeso_soil_dir, okmeso_met_dir, ldasout_dir, startdate, enddate)

!KWM  inquire(file="rdsom.mem", exist=lexist)
!KWM
!KWM  if (.not. lexist) then

     call netcdf_open(trim(ldasout_dir)//"/"//startdate//".LDASOUT_DOMAIN1", nfstruct_for_map)
     iret = nf90_close(nfstruct_for_map%ncid)

     ! Find the OK Mesonet stations.
     call rdgeomeso(trim(okmeso_soil_dir), mem%station, nstation, stnlat, stnlon, maxstn, 0)

     ! Find x/y
     do i = 1, nstation
        call lltoxy_hd(stnlat(i), stnlon(i), stnx(i), stny(i), nfstruct_for_map)
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
           call rdsom(trim(okmeso_soil_dir), mem%station(i), nowdate//"00", wc(1:4,i,indx))
        enddo

        call rdmet(trim(okmeso_met_dir), nstation, mem%station(1:nstation), nowdate//"00", &
             okpcp(1:nstation,indx), okTair(1:nstation,indx), &
             okTs05(1:nstation,indx), okTs10(1:nstation, indx), okTs30(1:nstation,indx))

        ! Get corresponding HRLDAS results from HRLDAS files
        call rdhrldas(trim(ldasout_dir), mem%station, stnlat, stnlon, nstation, nowdate, &
             sm(1:4,1:nstation,indx), st(1:4, 1:nstation, indx))

     enddo DATELOOP

!KWM     open(12, file="rdsom.mem", form='unformatted', action='write')
!KWM     write(12) map
!KWM     write(12) mem
!KWM     close(12)
!KWM
!KWM  else
!KWM
!KWM     print*, 'indx = ', indx
!KWM
!KWM     open(12, file="rdsom.mem", form='unformatted', action='read')
!KWM     read(12) map
!KWM     read(12) mem
!KWM     close(12)
!KWM
!KWM     print*, 'indx = ', indx
!KWM
!KWM  endif

  ! Convert accumulated rain from 00Z to hourly rain.
  do i = 1, nstation
     do n = 1, indx
        if (n == 1) then
           hold = 0.
        endif
        if (mod(n-1,24) == 1) then
           hold = 0
        endif
        tmp = okpcp(i,n) - hold
        hold = okpcp(i,n)
        okpcp(i,n) = tmp
     enddo
  enddo


  ! Make some plots
  call kwm_init_ncargks("sm.cgm")
  do i = 1, nstation

     do n = 1, 2
        call qc_sm(wc(n,i,1:indx), mflag(n,i,1:indx), indx)
        if (mem%station(i) == "HASK") mflag(n,i,1:indx) = 1
        if (mem%station(i) == "KING") mflag(n,i,1:indx) = 1
        if (mem%station(i) == "SKIA") mflag(n,i,1:indx) = 1

        ! If 65% of the station is suspect, throw out everything
        if (count(mflag(n,i,1:indx)==1) > 0.65*indx) then
           mflag(n,i,1:indx) = 1
        endif

     enddo

     call pltsm(mem%station(i), wc(1:2,i,1:indx), sm(1:2,i,1:indx), &
          st(1:2,i,1:indx), 2, indx, startdate, &
          stnlat(i), stnlon(i), stnx(i), stny(i), okpcp(i,1:indx), &
          okTair(i,1:indx), okTs05(i,1:indx), okTs30(i,1:indx), &
          mflag(:,i,1:indx))

  enddo


  ! Now construct some averages of all the stations

  allocate(diurnal_obs(2,0:23))
  allocate(diurnal_mdl(2,0:23))
  diurnal_obs = 0.
  diurnal_mdl = 0.

  do n = 1, 2
     allocate(okts(nstation,indx))
     okts = -1.E32
     select case (n)
     case (1)
        okts(1:nstation,1:indx) = okts05(1:nstation,1:indx) ! okts10?
     case (2)
        okts(1:nstation,1:indx) = okts30(1:nstation,1:indx)
     end select

     call construct_averages_temperature(startdate, enddate, nstation, indx, &
          st(n,1:nstation,1:indx), &
          okts(1:nstation,1:indx), &
          tflag(n,1:nstation,1:indx), &
          sta(n,1:indx), okta(n,1:indx), &
          avstamean(n), avoktmean(n), avstbias(n), avstrmse(n), stcount(n,1:indx),&
          diurnal_obs(n,0:23), diurnal_mdl(n,0:23))

     deallocate(okts)

     call construct_averages_moisture(nstation, indx, &
          wc(n,1:nstation,1:indx), &
          sm(n,1:nstation,1:indx), &
          mflag(n,1:nstation,1:indx), &
          wa(n,1:indx), sa(n,1:indx), &
          avwcmean(n), avsmmean(n), avsmbias(n), avsmrmse(n), smcount(n,1:indx))
  enddo

  ! And plot the averages

  call pltav_sm(2, indx, wa(1:2, 1:indx), sa(1:2, 1:indx), smcount(1:2,1:indx), &
       startdate, &
       avwcmean, avsmmean, avsmbias, avsmrmse)

  call pltav_st(2, indx, sta(1:2, 1:indx), okta(1:2, 1:indx), stcount(1:2,1:indx), &
       startdate, &
       avstamean, avoktmean, avstbias, avstrmse)

  call pltav_diurnal(diurnal_obs, diurnal_mdl)


  call kwm_close_ncargks

end program ver

subroutine pltav_diurnal(obs, mdl)
  use kwm_plot_utilities
  implicit none
  real, dimension(2,0:23), intent(in) :: obs, mdl
  integer :: n, i

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.16, 0.85, "Soil T Average Diurnal Cycle: ", 0.012, 0., -1.)
  call pchiqu(0.16, 0.82, "Soil T Average Diurnal Cycle: ", 0.012, 0., -1.)

  call pchiqu(0.44, 0.85, "OK Mesonet: ", 0.012, 0., -1.)
  call pchiqu(0.44, 0.82, "HRLDAS: ",     0.012, 0., -1.)
  call gsplci(color_index("blue"))
  call line(.69, 0.85, .80, 0.85)
  call gsplci(color_index("red"))
  call line(.69, 0.82, .80, 0.82)
  call gsplci(color_index("black"))

  call gasetc("YLF", '(F5.1)')
  call gasetc("XLF", '(I4)')


  do n = 1, 2
     call gsplci(color_index("black"))
     if (n == 1) then
        call set(.1, .9, .5, .75, 0., 24., 287., 294., 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 0., 24., 288., 290., 1)
     endif
     call periml(24,0,5,1)

     call gsplci(color_index("blue"))
     call frstpt(0., obs(n,0))
     do i = 1, 24
        call vector(float(i), obs(n,mod(i,24)))
     enddo
     call plotif(0., 0., 2)


     call gsplci(color_index("red"))
     call frstpt(0., mdl(n,0))
     do i = 1, 24
        call vector(float(i), mdl(n,mod(i,24)))
     enddo
     call plotif(0., 0., 2)
  enddo

end subroutine pltav_diurnal

subroutine pltav_st(nlev, ndim, wa, sa, scount, startdate, wcmean, smmean, bias, rmse)
  use kwm_plot_utilities
  use kwm_date_utilities
  implicit none
  integer, intent(in) :: nlev, ndim
  real, dimension(nlev,ndim), intent(in) :: wa, sa, scount
  character(len=*), intent(in) :: startdate
  real, dimension(2), intent(in) :: wcmean, smmean, bias, rmse

  character(len=19) :: enddate
  integer :: n, i
  character(len=256) :: text
  logical :: pd

  real :: xl, xr, xb, xt, wl, wr, wb, wt
  integer :: ml

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.56, 0.85, "Soil T: ", 0.012, 0., -1.)
  call pchiqu(0.56, 0.82, "Soil T: ", 0.012, 0., -1.)

  call gflas1(4)
  call pchiqu(0.14, 0.95, "Averages", 0.020, 0., -1.)
  text = "from "//startdate(1:4)//"-"//startdate(5:6)//"-"//startdate(7:8)//" "//startdate(9:10)//" Z"
  call pchiqu(0.44, 0.95, trim(text), 0.012, 0., -1.)

  call geth_newdate(enddate(1:10), startdate(1:10), ndim-1)
  text = "to   "//enddate(1:4)//"-"//enddate(5:6)//"-"//enddate(7:8)//" "//enddate(9:10)//" Z"
  call pchiqu(0.44, 0.92, trim(text), 0.012, 0., -1.)

  call pchiqu(0.44, 0.85, "OK Mesonet: ", 0.012, 0., -1.)
  call pchiqu(0.44, 0.82, "HRLDAS: ",     0.012, 0., -1.)
  call gsplci(color_index("blue"))
  call line(.69, 0.85, .80, 0.85)
  call gsplci(color_index("red"))
  call line(.69, 0.82, .80, 0.82)
  call gsplci(color_index("black"))

  call pchiqu(0.10, 0.77, "Level 1", 0.012, 0., -1.)
  call pchiqu(0.10, 0.42, "Level 2", 0.012, 0., -1.)
  call gflas2


  call gasetc("YLF", '(F5.2)')
  call gasetc("XLF", '(I4)')

  do n = 1, nlev
     call gsplci(color_index("black"))

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 0., 125., 1)
        call plotif(0., 0., 2)

        call gasetc("YLF", '(F4.1)')
        call gaseti("YLO", -835)
        call periml((ndim-1)/24,0,5,1)
        call gaseti("YLO", 20)
        call gasetc("YLF", '(F5.2)')


     endif

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 270., 310., 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 0., float(ndim-1)/24., 270., 310., 1)
     endif

     call periml((ndim-1)/24,0,5,1)

     call gsplci(color_index("blue"))
     pd = .FALSE.
     do i = 1, ndim
        if (.not. pd) then
           call frstpt(float(i-1)/24., wa(n,i))
           pd = .TRUE.
        else
           call vector(float(i-1)/24., wa(n,i))
        endif
     enddo
     call plotif(0., 0., 2)


     call gsplci(color_index("red"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., sa(n,i))
        else
           call vector(float(i-1)/24., sa(n,i))
        endif
     enddo
     call plotif(0., 0., 2)

     call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
     call set   (xl, xr, xb, xt, wl, wr, 0., 125., ml)

     call gsplci(color_index("grey75"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., scount(n,i))
        else
           call vector(float(i-1)/24., scount(n,i))
        endif
     enddo
     call plotif(0., 0., 2)


     call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
     if (n == 1) then
        write(text, '("OKMESO mean Soil T:  ", F6.2)') wcmean(n)
        call pchiqu(0.12, .725, trim(text), 0.013, 0., -1.)
        write(text, '("HRLDAS mean Soil T:  ", F6.2)') smmean(n)
        call pchiqu(0.55, .725, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') bias(n), rmse(n)
        call pchiqu(0.55, .695, trim(text), 0.013, 0., -1.)
     else if (n == 2) then
        write(text, '("OKMESO mean Soil T:  ", F6.2)') wcmean(n)
        call pchiqu(0.12, .375, trim(text), 0.013, 0., -1.)
        write(text, '("HRLDAS mean Soil T:  ", F6.2)') smmean(n)
        call pchiqu(0.55, .375, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') bias(n), rmse(n)
        call pchiqu(0.55, .345, trim(text), 0.013, 0., -1.)
     endif


  enddo

  call gflas3(4)

  call frame()

end subroutine pltav_st

subroutine pltav_sm(nlev, ndim, wa, sa, scount, startdate, wcmean, smmean, bias, rmse)
  use kwm_plot_utilities
  use kwm_date_utilities
  implicit none
  integer, intent(in) :: nlev, ndim
  real, dimension(nlev,ndim), intent(in) :: wa, sa, scount
  character(len=*), intent(in) :: startdate
  real, dimension(2), intent(in) :: wcmean, smmean, bias, rmse

  character(len=19) :: enddate
  integer :: n, i
  character(len=256) :: text
  logical :: pd

  real :: xl, xr, xb, xt, wl, wr, wb, wt
  integer :: ml

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.56, 0.85, "Soil Moisture: ", 0.012, 0., -1.)
  call pchiqu(0.56, 0.82, "Soil Moisture: ", 0.012, 0., -1.)

  call gflas1(4)
  call pchiqu(0.14, 0.95, "Averages", 0.020, 0., -1.)
  text = "from "//startdate(1:4)//"-"//startdate(5:6)//"-"//startdate(7:8)//" "//startdate(9:10)//" Z"
  call pchiqu(0.44, 0.95, trim(text), 0.012, 0., -1.)

  call geth_newdate(enddate(1:10), startdate(1:10), ndim-1)
  text = "to   "//enddate(1:4)//"-"//enddate(5:6)//"-"//enddate(7:8)//" "//enddate(9:10)//" Z"
  call pchiqu(0.44, 0.92, trim(text), 0.012, 0., -1.)

  call pchiqu(0.44, 0.85, "OK Mesonet: ", 0.012, 0., -1.)
  call pchiqu(0.44, 0.82, "HRLDAS: ",     0.012, 0., -1.)
  call gsplci(color_index("blue"))
  call line(.69, 0.85, .80, 0.85)
  call gsplci(color_index("red"))
  call line(.69, 0.82, .80, 0.82)
  call gsplci(color_index("black"))

  call pchiqu(0.10, 0.77, "Level 1", 0.012, 0., -1.)
  call pchiqu(0.10, 0.42, "Level 2", 0.012, 0., -1.)
  call gflas2


  call gasetc("YLF", '(F5.2)')
  call gasetc("XLF", '(I4)')

  do n = 1, nlev
     call gsplci(color_index("black"))

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 0., 125., 1)
        call plotif(0., 0., 2)

        call gasetc("YLF", '(F4.1)')
        call gaseti("YLO", -835)
        call periml((ndim-1)/24,0,5,1)
        call gaseti("YLO", 20)
        call gasetc("YLF", '(F5.2)')


     endif

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 0., 0.5, 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 0., float(ndim-1)/24., 0., 0.5, 1)
     endif

     call periml((ndim-1)/24,0,5,1)

     call gsplci(color_index("blue"))
     pd = .FALSE.
     do i = 1, ndim
        if (.not. pd) then
           call frstpt(float(i-1)/24., wa(n,i))
           pd = .TRUE.
        else
           call vector(float(i-1)/24., wa(n,i))
        endif
     enddo
     call plotif(0., 0., 2)


     call gsplci(color_index("red"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., sa(n,i))
        else
           call vector(float(i-1)/24., sa(n,i))
        endif
     enddo
     call plotif(0., 0., 2)

     call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
     call set   (xl, xr, xb, xt, wl, wr, 0., 125., ml)

     call gsplci(color_index("grey75"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., scount(n,i))
        else
           call vector(float(i-1)/24., scount(n,i))
        endif
     enddo
     call plotif(0., 0., 2)


     call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
     if (n == 1) then
        write(text, '("OKMESO mean Soil Moisture:  ", F5.3)') wcmean(n)
        call pchiqu(0.12, .725, trim(text), 0.013, 0., -1.)
        write(text, '("HRLDAS mean Soil Moisture:  ", F5.3)') smmean(n)
        call pchiqu(0.55, .725, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') bias(n), rmse(n)
        call pchiqu(0.55, .695, trim(text), 0.013, 0., -1.)
     else if (n == 2) then
        write(text, '("OKMESO mean Soil Moisture:  ", F5.3)') wcmean(n)
        call pchiqu(0.12, .375, trim(text), 0.013, 0., -1.)
        write(text, '("HRLDAS mean Soil Moisture:  ", F5.3)') smmean(n)
        call pchiqu(0.55, .375, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') bias(n), rmse(n)
        call pchiqu(0.55, .345, trim(text), 0.013, 0., -1.)
     endif


  enddo

  call gflas3(4)

  call frame()

end subroutine pltav_sm

subroutine construct_averages_moisture(nstation, indx, obs, mdl, flag, wa, sa, &
     obsmean, mdlmean, bias, rmse, scount)
  implicit none
  integer, intent(in) :: nstation, indx
  real,    dimension(nstation,indx), intent(in)  :: obs, mdl
  integer, dimension(nstation,indx), intent(in)  :: flag
  real,    dimension(indx),          intent(out) :: wa, sa
  real,                              intent(out) :: bias, rmse, obsmean, mdlmean
  real,    dimension(indx),          intent(out) :: scount

  integer :: n, i, count, bcount

  bcount = 0
  rmse = 0.0
  bias = 0.0
  obsmean = 0.0
  mdlmean = 0.0

  do i = 1, indx
     wa(i) = 0
     sa(i) = 0
     count = 0
     do n = 1, nstation
        if ( (flag(n,i)==0) .and. (mdl(n,i)>0) ) then
           wa(i) = wa(i) + obs(n,i)
           sa(i) = sa(i) + mdl(n,i)
           count = count + 1
           bias = bias + (mdl(n,i)-obs(n,i))
           rmse = rmse + (mdl(n,i)-obs(n,i))**2
           obsmean = obsmean + obs(n,i)
           mdlmean = mdlmean + mdl(n,i)
           bcount = bcount + 1
        endif
     enddo

     if (count > 0) then
        wa(i) = wa(i) / real(count)
        sa(i) = sa(i) / real(count)
     else
        wa(i) = 0.
        sa(i) = 0.
     endif
     scount(i) = count

  enddo

  bias = bias / real(bcount)
  rmse = sqrt(rmse/real(bcount))
  obsmean = obsmean / real(bcount)
  mdlmean = mdlmean / real(bcount)


end subroutine construct_averages_moisture

subroutine construct_averages_temperature(startdate, enddate, nstation, indx, obs, mdl, flag, wa, sa, &
     obsmean, mdlmean, bias, rmse, scount, diurnal_obs, diurnal_mdl)
  use kwm_date_utilities
  implicit none
  character(len=*), intent(in) :: startdate, enddate
  integer, intent(in) :: nstation, indx
  real,    dimension(nstation,indx), intent(in)  :: obs, mdl
  integer, dimension(nstation,indx), intent(in)  :: flag
  real,    dimension(indx),          intent(out) :: wa, sa
  real,                              intent(out) :: bias, rmse, obsmean, mdlmean
  real,    dimension(indx),          intent(out) :: scount
  real,    dimension(0:23),          intent(out) :: diurnal_obs, diurnal_mdl

  integer, dimension(0:23) :: ocount, mcount
  integer :: n, i, count, bcount, ihr
  character(len=10) :: nowdate

  bcount = 0
  rmse = 0.0
  bias = 0.0
  obsmean = 0.0
  mdlmean = 0.0

  do i = 1, indx
     wa(i) = 0
     sa(i) = 0
     count = 0
     do n = 1, nstation
!        if ( (flag(n,i)==0) .and. (mdl(n,i)>0) ) then
        if ( (obs(n,i)>0) .and. (mdl(n,i)>0) ) then
           wa(i) = wa(i) + obs(n,i)
           sa(i) = sa(i) + mdl(n,i)
           count = count + 1
           bias = bias + (mdl(n,i)-obs(n,i))
           rmse = rmse + (mdl(n,i)-obs(n,i))**2
           obsmean = obsmean + obs(n,i)
           mdlmean = mdlmean + mdl(n,i)
           bcount = bcount + 1
        endif
     enddo

     if (count > 0) then
        wa(i) = wa(i) / real(count)
        sa(i) = sa(i) / real(count)
     else
        wa(i) = 0.
        sa(i) = 0.
     endif
     scount(i) = count

  enddo

  bias = bias / real(bcount)
  rmse = sqrt(rmse/real(bcount))
  obsmean = obsmean / real(bcount)
  mdlmean = mdlmean / real(bcount)

  ! And construct an average diurnal cycle
  diurnal_obs(0:23) = 0.0
  diurnal_mdl(0:23) = 0.0
  ocount(0:23) = 0
  mcount(0:23) = 0
  print*, 'startdate = "', startdate, '"'
  do i = 1, indx
     call geth_newdate(nowdate, startdate(1:10), (i-1))
     read(nowdate(9:10), *) ihr
     do n = 1, nstation
        if (obs(n,i) > 0) then
           diurnal_obs(ihr) = diurnal_obs(ihr) + obs(n,i)
           ocount(ihr) = ocount(ihr) + 1
        endif
        if (mdl(n,i) > 0) then
           diurnal_mdl(ihr) = diurnal_mdl(ihr) + mdl(n,i)
           mcount(ihr) = mcount(ihr) + 1
        endif
     enddo
  enddo
  diurnal_obs(0:23) = diurnal_obs(0:23) / float(ocount(0:23))
  diurnal_mdl(0:23) = diurnal_mdl(0:23) / float(mcount(0:23))

  print*, 'diurnal_obs = ', diurnal_obs
  print*, 'diurnal_mdl = ', diurnal_mdl


end subroutine construct_averages_temperature

subroutine pltsm(stn, wc, sm, st, nlev, ndim, startdate, stnlat, stnlon, stnx, stny, &
     okpcp, okTair, okTs05, okTs30, flag)
  use kwm_plot_utilities
  use kwm_date_utilities
  implicit none
  character(len=*),           intent(in) :: stn
  integer,                    intent(in) :: nlev
  integer,                    intent(in) :: ndim
  real, dimension(nlev,ndim), intent(in) :: wc           ! Observations
  real, dimension(nlev,ndim), intent(in) :: sm           ! HRLDAS
  real, dimension(nlev,ndim), intent(in) :: st
  character(len=*),           intent(in) :: startdate
  real,                       intent(in) :: stnlat
  real,                       intent(in) :: stnlon
  real,                       intent(in) :: stnx
  real,                       intent(in) :: stny
  real, dimension(ndim),      intent(in) :: okpcp
  real, dimension(ndim),      intent(in) :: okTair
  real, dimension(ndim),      intent(in) :: okTs05
  real, dimension(ndim),      intent(in) :: okTs30
  integer, dimension(2,ndim),   intent(in) :: flag

  integer :: i,n
  character(len=256) :: text
  character(len=10)  :: enddate
  logical :: pd
  
  real :: wcmean, smmean
  integer :: wccount, smcount

  integer :: smbcount
  real    :: smbias
  real    :: smrmse


  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.56, 0.85, "Soil Moisture: ", 0.012, 0., -1.)
  call pchiqu(0.56, 0.82, "Soil Moisture: ", 0.012, 0., -1.)
  call pchiqu(0.44, 0.79, "OK Mesonet hourly precip: ", 0.012, 0., -1.)
  call gsplci(color_index("green3"))
  call line(.69, 0.79, .80, 0.79)
  call gsplci(color_index("black"))

  call gflas1(2)
  call pchiqu(0.14, 0.95, stn, 0.020, 0., -1.)
  write(text,'("Lat/Lon = ", F6.2, 1x, F7.2)') stnlat, stnlon
  call pchiqu(0.10, 0.91, trim(text), 0.012, 0., -1.)
  write(text,'("X/Y = ", F6.2, 1x, F7.2)') stnx, stny
  call pchiqu(0.10, 0.88, trim(text), 0.012, 0., -1.)
  text = "from "//startdate(1:4)//"-"//startdate(5:6)//"-"//startdate(7:8)//" "//startdate(9:10)//" Z"
  call pchiqu(0.44, 0.95, trim(text), 0.012, 0., -1.)

  call geth_newdate(enddate(1:10), startdate(1:10), ndim-1)
  text = "to   "//enddate(1:4)//"-"//enddate(5:6)//"-"//enddate(7:8)//" "//enddate(9:10)//" Z"
  call pchiqu(0.44, 0.92, trim(text), 0.012, 0., -1.)

  call pchiqu(0.44, 0.85, "OK Mesonet: ", 0.012, 0., -1.)
  call pchiqu(0.44, 0.82, "HRLDAS: ",     0.012, 0., -1.)
  call gsplci(color_index("blue"))
  call line(.69, 0.85, .80, 0.85)
  call gsplci(color_index("red"))
  call line(.69, 0.82, .80, 0.82)
  call gsplci(color_index("black"))

  call pchiqu(0.10, 0.77, "Level 1", 0.012, 0., -1.)
  call pchiqu(0.10, 0.42, "Level 2", 0.012, 0., -1.)
  call gflas2


  call gasetc("YLF", '(F5.2)')
  call gasetc("XLF", '(I4)')

  do n = 1, nlev

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 0., 25., 1)
        do i = 1, ndim
           call fillbox((float(i)-1.0)/24., (float(i)+1.0)/24., 0., okpcp(i), color_index("green3"))
        enddo
        call plotif(0., 0., 2)

        call gasetc("YLF", '(F4.1)')
        call gaseti("YLO", -835)
        call periml((ndim-1)/24,0,5,1)
        call gaseti("YLO", 20)
        call gasetc("YLF", '(F5.2)')


     endif

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 0., 0.5, 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 0., float(ndim-1)/24., 0., 0.5, 1)
     endif
        
     call periml((ndim-1)/24,0,5,1)

     if (flag(n,1) == 1) then
        call gsplci(color_index("gray90"))
     else
        call gsplci(color_index("blue"))
     endif

     pd = .FALSE.
     wcmean = 0.
     wccount = 0
     do i = 1, ndim
        if (.not. pd) then
           call frstpt(float(i-1)/24., wc(n,i))
           if (flag(n,i) == 0) then
              wcmean = wcmean + wc(n,i)
              wccount = wccount + 1
              if (stn == "ANTL") print*, 'wcmean, wc = ', wcmean, wc(n,i)
           endif
           pd = .TRUE.
        else
           call vector(float(i-1)/24., wc(n,i))
           if (flag(n,i) == 0) then
              wcmean = wcmean + wc(n,i)
              wccount = wccount + 1
              if (stn == "ANTL") print*, 'wcmean, wc = ', wcmean, wc(n,i)
           endif
        endif

        if (i==ndim)then
           call plotif(0., 0., 2)
           pd = .FALSE.
        else if (flag(n,i) /= flag(n,i+1)) then
           call plotif(0., 0., 2)
           pd = .FALSE.
           if (flag(n,i+1) == 0) then
              call gsplci(color_index("blue"))
           else
              call gsplci(color_index("gray90"))
           endif
           
        endif
     enddo
     call plotif(0., 0., 2)
     
     call gsplci(color_index("red"))
     smmean = 0.
     smcount = 0
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., sm(n,i))
           smmean = smmean + sm(n,i)
           smcount = smcount + 1
        else
           call vector(float(i-1)/24., sm(n,i))
           smmean = smmean + sm(n,i)
           smcount = smcount + 1
        endif
     enddo
     call plotif(0., 0., 2)

     smbcount = 0
     smbias = 0.0
     smrmse = 0.0
     do i = 1, ndim
        if (flag(n,i) == 0) then ! ((sm(n,i)>0).and.(wc(n,i)>0)) then
           smbias = smbias + (sm(n,i)-wc(n,i))
           smrmse = smrmse + ((sm(n,i)-wc(n,i))**2)
           smbcount = smbcount + 1
        endif
     enddo
     if (smbcount > 0) then
        smbias = smbias / float(smbcount)
        smrmse = sqrt(smrmse / float(smbcount))
     endif

     call gsplci(color_index("black"))

     if (smcount > 0) then
        smmean = smmean / float(smcount)
     endif
     wcmean = wcmean / float(wccount)

     if (smbcount > 0) then
        if (n == 1) then
           write(*,'(A4, 3(3x,F6.3))', advance="no") stn, smmean, smbias, smrmse
        else
           write(*,'("   ", 3(3x,F6.3))', advance="no") smmean, smbias, smrmse
        endif
     endif
     if (n==2)  write(*,*)

     call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
     if (n == 1) then
        write(text, '("HRLDAS mean Soil Moisture:  ", F5.3)') smmean
        call pchiqu(0.55, .725, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') smbias, smrmse
        call pchiqu(0.55, .695, trim(text), 0.013, 0., -1.)
     else if (n == 2) then
        write(text, '("HRLDAS mean Soil Moisture:  ", F5.3)') smmean
        call pchiqu(0.55, .375, trim(text), 0.013, 0., -1.)
        write(text, '("bias: ", F6.3, "    RMSE: ", F5.3)') smbias, smrmse
        call pchiqu(0.55, .345, trim(text), 0.013, 0., -1.)
     endif

  enddo


  call gflas3(2)

  call frame()

  ! Now plot up temperature information
  call gasetc("YLF", '(F5.1)')

  call set(0., 1., 0., 1., 0., 1., 0., 1., 1)
  call pchiqu(0.56, 0.85, "Soil T: ", 0.012, 0., -1.)
  call pchiqu(0.56, 0.82, "Soil T: ", 0.012, 0., -1.)
  



  do n = 1, nlev

     if (n == 1) then
        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 275., 305., 1)
     else if (n == 2) then
        call set(.1, .9, .15, .4, 0., float(ndim-1)/24., 275., 305., 1)
     endif
        
     call periml((ndim-1)/24,0,5,1)

  call gflas3(2)


     call gsplci(color_index("blue"))
     pd = .FALSE.
     if (n == 1) then
        do i = 1, ndim
           if (.not. pd) then
              if (okTs05(i) >= 0) then
                 call frstpt(float(i-1)/24., okTs05(i))
                 pd = .TRUE.
              endif
           else
              if (okTs05(i) < 10) then
                 call plotif(0., 0., 2)
                 pd = .FALSE.
              else
                 call vector(float(i-1)/24., okTs05(i))
              endif
           endif
        enddo
     else if (n == 2) then
        do i = 1, ndim
           if (.not. pd) then
              if (okTs30(i) >= 0) then
                 call frstpt(float(i-1)/24., okTs30(i))
                 pd = .TRUE.
              endif
           else
              if (okTs30(i) < 10) then
                 call plotif(0., 0., 2)
                 pd = .FALSE.
              else
                 call vector(float(i-1)/24., okTs30(i))
              endif
           endif
        enddo
     endif
!KWM        if (i == 1) then
!KWM           if (n == 1) then
!KWM              call frstpt(float(i-1)/24., okTs05(i))
!KWM           else
!KWM              call frstpt(float(i-1)/24., okTs30(i))
!KWM           endif
!KWM        else
!KWM           if (n == 1) then
!KWM              call vector(float(i-1)/24., okTs05(i))
!KWM           else
!KWM              call vector(float(i-1)/24., okTs30(i))
!KWM           endif
!KWM        endif
!KWM     enddo
     call plotif(0., 0., 2)

     call gsplci(color_index("red"))
     do i = 1, ndim
        if (i == 1) then
           call frstpt(float(i-1)/24., st(n,i))
        else
          call vector(float(i-1)/24., st(n,i))
        endif
     enddo
     call plotif(0., 0., 2)

     if (n == 1) then

!KWM        call gsplci(color_index("magenta"))
!KWM        call set(.1, .9, .5, .75, 0., float(ndim-1)/24., 270., 310., 1)
!KWM        do i = 1, ndim
!KWM           if (i == 1) then
!KWM              call frstpt(float(i-1)/24., okTair(i))
!KWM           else
!KWM              call vector(float(i-1)/24., okTair(i))
!KWM           endif
!KWM        enddo
!KWM        call plotif(0., 0., 2)

     endif

     call gsplci(color_index("black"))
  enddo



  call frame()
end subroutine pltsm


subroutine rdgeomeso(okmeso_soil_dir, station, nstation, stnlat, stnlon, maxstn, iverbose)
  implicit none
  character(len=*),                    intent(in)  :: okmeso_soil_dir
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

  open(iunit, file=trim(okmeso_soil_dir)//"/geomeso.tbl", status='old', form='formatted', action='read')

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
  

subroutine rdsom(okmeso_soil_dir, stn, idate, wc)
  implicit none
  character(len=*),   intent(in)  :: okmeso_soil_dir
  character(len=*),   intent(in)  :: stn, idate
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


  write(flnm, '(A,"/",A8,".som")') okmeso_soil_dir, idate(1:8)
  
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

subroutine rdhrldas(ldasout_dir, stn, stnlat, stnlon, nstation, idate, sm, st)
  use module_ver
  use module_hd
  implicit none
  character(len=*),                      intent(in)  :: ldasout_dir
  integer,                               intent(in)  :: nstation
  character(len=*), dimension(nstation), intent(in)  :: stn
  real, dimension(nstation),             intent(in)  :: stnlat
  real, dimension(nstation),             intent(in)  :: stnlon
  character(len=*),                      intent(in)  :: idate
  real, dimension(4,nstation),           intent(out) :: sm
  real, dimension(4,nstation),           intent(out) :: st

  character(len=256) :: flnm, tmpflnm
  real, pointer, dimension(:,:,:) :: smptr, stptr
  real :: x, y, i, j
  integer :: ista, ilvl
  integer :: iret
  type(nf_structure) :: nfstruct
  logical :: bzipped, lexist

  sm = -99999.
  st = -99999.

  bzipped = .FALSE.
  write(flnm, '(A,"/",A10,".LDASOUT_DOMAIN1")') ldasout_dir, idate(1:10)

  inquire(file=trim(flnm), exist=lexist)
  if (lexist) then
     tmpflnm = flnm
  else
     flnm = trim(flnm)//".bz2"
     inquire(file=trim(flnm), exist=lexist)
     if (lexist) then
        print*, 'bunzipping.'
        bzipped = .TRUE.
        tmpflnm = "/tmp/bbn3.bz2"
        call system("rm -f "//trim(tmpflnm))
        call system("bzip2 -cd "//trim(flnm)//" > "//trim(tmpflnm))
     endif
  endif

  call netcdf_open(trim(tmpflnm), nfstruct)
  
  ! Read a soil-moisture field

  call netcdf_get_field_3d(nfstruct, "SOIL_M", 1, smptr)
  call netcdf_get_field_3d(nfstruct, "SOIL_T", 1, stptr)

  do ilvl = 1, 4

     do ista = 1, nstation
        call lltoxy_hd(stnlat(ista), stnlon(ista), x, y, nfstruct)

!KWM        print*, 'lat, lon, x, y = ', stnlat(ista), stnlon(ista), x, y
!KWM        print*, 'shape(smptr) = ', shape(smptr)
!KWM        print*, 'shape(sm) = ', shape(sm)
        sm(ilvl, ista) = smptr(nint(x), nint(y), ilvl)
        st(ilvl, ista) = stptr(nint(x), nint(y), ilvl)

     enddo

  enddo

  iret = nf90_close(nfstruct%ncid)
  deallocate(stptr, smptr)
  nullify(stptr, smptr)
  if (bzipped) then
     call system("rm "//trim(tmpflnm))
  endif

end subroutine rdhrldas

subroutine qc_sm(wc, flag, indx)
  implicit none
  integer,                  intent(in)  :: indx
  real,    dimension(indx), intent(in)  :: wc
  integer, dimension(indx), intent(out) :: flag

  integer :: i, n, istart, ihold1, ihold2, npass
  real :: val1, val2
  integer :: samestart, samecount, samecount_maximum, sameend, jm, jp
  real :: ymax, ymin
  real, dimension(indx) :: tmp, tmp2

  ! Flag 0 == OK
  ! Flag 1 == Suspect
  flag = 0

!  samecount_maximum = 1440
  samecount_maximum = 288

  where (wc <= 0) flag = 1

  ymax = maxval(wc, mask=(flag==0))
  ymin = minval(wc, mask=(flag==0))
!KWM  print*, stid//' ymin, ymax = ', ymin, ymax

  samestart = 0
  samecount = 0
  sameend = 0
  val1 = -999.
  val2 = -999.

  ! Find the first unflagged value.
  jm = 1
  do while (flag(jm) == 1) 
     jm = jm + 1
     if (jm > indx) exit
  enddo
  if (jm <= indx) then
     val2 = wc(jm)
  endif

  TIMELOOP : do

     ! Find the next unflagged value.
     jp = jm + 1
     if (jp > indx) exit TIMELOOP 
     do while (flag(jp) == 1) 
        jp = jp + 1
        if (jp > indx) exit TIMELOOP 
     enddo

     if ((wc(jp) == val1) .or. (wc(jp) == val2)) then
        samecount = samecount + 1
        sameend = jp
        if (samestart == 0) samestart = jm
     else

        if (samecount > samecount_maximum) then
           flag(samestart:sameend) = 1
        else if ( (max(val1,val2) > ymin + (ymax-ymin)*0.80) .and. &
             ( samecount > samecount_maximum*0.15)) then
           flag(samestart:sameend) = 1
        else if ( (min(val1,val2) < ymin + (ymax-ymin)*0.20) .and. &
             ( samecount > samecount_maximum*0.15)) then
           flag(samestart:sameend) = 1
        endif

        val1 = val2
        val2 = wc(jp)

        samecount = 0
        samestart = 0
        sameend = 0
     endif

     ! Call what used to be the "next" unflagged value, the latest unflagged value:
     jm = jp
  enddo TIMELOOP

  if (samecount > samecount_maximum) then
     flag(samestart:sameend) = 1
  endif


#ifdef _BLAHH_
     ! Flag regions with near-constant wc for a long time

     istart = -1
     do i = 2, indx

        if ((wc(n,i) >= 0).and.(wc(n,i-1)>=0) ) then
           
           if (abs(wc(n,i)-wc(n,i-1)) < 5.E-3) then
           ! if there's no (little) change:

              if (istart == -1) istart = i

              if (i == indx) then

                 if (istart > 0) then
                    if ((i-istart) > 50) then
                       flag(n,istart:i-1) = 1
                    endif
                    istart = -1
                 endif

              endif

           else
           ! if there has been some change:

               if (istart > 0) then
                 if ((i-istart) > 50) then
                    flag(n,istart:i-1) = 1
                 endif
                 istart = -1
              endif

           endif
        endif
     enddo

#endif

end subroutine qc_sm

subroutine read_namelist(okmeso_soil_dir, okmeso_met_dir, ldasout_dir, startdate, enddate)
  implicit none
  character(len=256) :: okmeso_soil_dir
  character(len=256) :: okmeso_met_dir
  character(len=256), intent(out) :: ldasout_dir
  character(len=10),  intent(out) :: startdate
  character(len=10),  intent(out) :: enddate
  integer ::  ierr
  character(len=10)  :: statistics_startdate
  character(len=10)  :: statistics_enddate

  okmeso_soil_dir = "UNSPECIFIED"
  okmeso_met_dir  = "UNSPECIFIED"
  ldasout_dir     = "UNSPECIFIED"

  
  namelist /okmeso/ okmeso_soil_dir, okmeso_met_dir, ldasout_dir, &
       statistics_startdate, statistics_enddate

  open(10, file="okmeso.namelist", status='old', form='formatted', action='read', iostat=ierr)
  if (ierr /= 0) then
     write(*,'("Namelist file not found:  okmeso.namelist")')
     stop
  endif
  read(10, okmeso, iostat=ierr)
  if (ierr /= 0) then
     write(*,'("Problem reading namelist file:  okmeso.namelist")')
     stop
  endif
  close(10)

  startdate = statistics_startdate
  enddate   = statistics_enddate

end subroutine read_namelist

