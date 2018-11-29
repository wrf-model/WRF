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

program ldasout_timeseries
  use kwm_date_utilities
  use module_plotts_graphics
  use pt_data
  implicit none

  integer, parameter :: namelist_fortran_unit = 11

  character(len=256) :: ldasout_flnm_template
  character(len=256) :: diff_flnm_template
  character(len=10) :: nowdate

  real, allocatable, dimension(:)   :: array
  real, allocatable, dimension(:)   :: array1
  integer :: idt, i, ierr
  integer :: ival, jval
  logical, dimension(1000) :: Metadata = .FALSE.
  integer :: loopcount

  call read_fileinfo_namelist(namelist_fortran_unit, ldasout_flnm_template, & 
       diff_flnm_template)

  call init_ncargks("test.cgm")

  loopcount = 0
  do 
     call read_plotinfo_namelist(namelist_fortran_unit, ierr)
     if (ierr /= 0) exit
     loopcount = loopcount + 1
     print*, 'Fldname = "'//trim(pltts_parameter%fldname)//'"'

     call geth_idts(pltts_parameter%enddate, pltts_parameter%startdate, idt)
     allocate(array (0:idt/pltts_parameter%time_interval))
     allocate(array1(0:idt/pltts_parameter%time_interval))

     nowdate = pltts_parameter%startdate

     ! print*, 'pltts_parameter = ', pltts_parameter
     do while (nowdate <= pltts_parameter%enddate)

        call geth_idts(nowdate, pltts_parameter%startdate, i)
        i = i / pltts_parameter%time_interval

        call get_point_data(trim(pltts_parameter%fldname), pltts_parameter%latitude, pltts_parameter%longitude, &
             ldasout_flnm_template, nowdate, array(i), pltts_parameter%soil_level, ival, jval)

        if (diff_flnm_template /= "") then
           call get_point_data(trim(pltts_parameter%fldname),pltts_parameter%latitude, pltts_parameter%longitude, & 
                diff_flnm_template, nowdate, array1(i), &
                pltts_parameter%soil_level, ival, jval)
           array(i) = array(i) - array1(i)
        endif

        if (.not. metadata(loopcount)) then
           metadata(loopcount) = .TRUE.
           write(100+loopcount,'("Info: ", A)')       trim(pltts_parameter%info)
           write(100+loopcount,'("Var:  ", A)')       trim(pltts_parameter%fldname)
           write(100+loopcount,'("Level: ", G20.12)') pltts_parameter%soil_level
           write(100+loopcount,'("Lat: ", G20.12)')   pltts_parameter%latitude
           write(100+loopcount,'("Lon: ", G20.12)')   pltts_parameter%longitude
           write(100+loopcount,'("X: ", G20.12)')     ival
           write(100+loopcount,'("Y: ", G20.12)')     jval
           write(100+loopcount,'("#")')
        endif
        write(100+loopcount,'(A10, G20.12)') nowdate, array(i)

        call geth_newdate(nowdate, nowdate, pltts_parameter%time_interval)
     enddo

     call pltts(array, 0, idt/pltts_parameter%time_interval)

     deallocate(array)
     deallocate(array1)

  enddo

  call close_ncargks

contains

  subroutine pltts(data, lowdim, highdim)
    integer, intent(in) :: lowdim, highdim
    real, dimension(lowdim:highdim), intent(in) :: data
    integer :: intv
    character(len=10) :: sd, ed
    character(len=256) :: text
    real :: xl, xr, xb, xt, wl, wr, wb, wt
    integer :: ml
    real :: x1
    integer :: markint, ndays,  nmarks
    sd = pltts_parameter%startdate
    ed = pltts_parameter%enddate

    call set(.15, .95, .2, .6, float(lowdim), float(highdim), &
         pltts_parameter%y_minimum, pltts_parameter%y_maximum, 1)
    intv = (pltts_parameter%y_maximum-pltts_parameter%y_minimum)/pltts_parameter%y_interval
    call gasetc("XLF", "(I0)")
    call periml(1,0,intv,0)

    ! print*, 'color = ', trim(pltts_parameter%color), data
    call gsplci(color_index(trim(pltts_parameter%color)))
    call frstpt(float(lowdim), data(lowdim))
    do i = lowdim+1, highdim
       call vector(float(i), data(i))
    enddo
    call plotif(0., 0., 2)
    call gsplci(color_index("black"))

    !
    ! And annotate
    !
    call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
    call set(0., 1., 0., 1., 0., 1., 0., 1., 1)

    call line(0.,0.,0.,1.)
    call line(0.,1.,1.,1.)
    call line(1.,1.,1.,0.)
    call line(1.,0.,0.,0.)

    call pcseti("CC", color_index(trim(pltts_parameter%color)))
    write (text, '(A, 3x, "Soil level ", I2)') trim(pltts_parameter%fldname), pltts_parameter%soil_level
    call pchiqu(0.55, 0.90 - (0.04*pltts_parameter%count), trim(text), 0.018, 0.0, 0.0)
    call pcseti("CC", color_index("black"))

    call pchiqu(0.55, 0.12, "From "//&
         &sd(1:4)//"-"//sd(5:6)//"-"//sd(7:8)//" / "//sd(9:10)//" Z  to  "//&
         &ed(1:4)//"-"//ed(5:6)//"-"//ed(7:8)//" / "//ed(9:10)//" Z", &
         0.015, 0.0, 0.0)

    ! How many marks do we add?
    markint = 0
    nmarks = 1000
    ndays = (highdim-lowdim) * pltts_parameter%time_interval / 24
    print*, 'ndays = ', ndays
    do while (nmarks > 30)
       markint = markint + 1
       nmarks = ndays / markint
    enddo

    print*, 'markint = ', markint

    nmarks = 0
    do i = lowdim, highdim, 1
       call geth_newdate(nowdate, pltts_parameter%startdate, i*pltts_parameter%time_interval)
       if (nowdate(9:10) == "00") then
          if (mod(nmarks,markint) == 0) then
             x1 = xl + real(i-lowdim)/real(highdim-lowdim)*(xr-xl)
             call line(x1,xb,x1,xb+0.01)
          endif
          if (mod(nmarks,markint*5) == 0) then
             call line(x1,xb,x1,xb+0.02)
             if (nmarks == 0) then
                call pchiqu(x1, xb-0.0225, nowdate(1:4)//"/~C~"//nowdate(5:6)//"-"//nowdate(7:8), 0.010, 0., 0.)
             else
                call pchiqu(x1, xb-0.015, nowdate(5:6)//"-"//nowdate(7:8), 0.010, 0., 0.)
             endif
          endif
          nmarks = nmarks + 1
       endif
    enddo

    nowdate = pltts_parameter%enddate
    call pchiqu(xr, xb-0.0225, nowdate(1:4)//"/~C~"//nowdate(5:6)//"-"//nowdate(7:8), 0.010, 0., 0.)


    if (pltts_parameter%callframe) then
       call frame()
    endif

  end subroutine pltts


end program ldasout_timeseries

!
! ############################################################################################################
!

subroutine read_fileinfo_namelist(namelist_fortran_unit, ldasout_flnm_template, &
   diff_flnm_template)
  use module_plotts_graphics
  implicit none
  integer, intent(in) :: namelist_fortran_unit
  character(len=256)  :: ldasout_flnm_template
  character(len=256)  :: diff_flnm_template
  character(len=10)   :: plot_startdate
  character(len=10)   :: plot_enddate
  integer             :: plot_interval

  namelist/file_info/ ldasout_flnm_template, plot_startdate, plot_enddate, plot_interval, diff_flnm_template

  plot_interval = 1
  diff_flnm_template = ""

  open(namelist_fortran_unit, file="namelist.pltts", status='old', form='formatted', action='read')

  read(namelist_fortran_unit, file_info) 

  pltts_parameter%startdate = plot_startdate
  pltts_parameter%enddate = plot_enddate
  pltts_parameter%time_interval = plot_interval

end subroutine read_fileinfo_namelist

!
! ############################################################################################################
!

subroutine read_plotinfo_namelist(namelist_fortran_unit, ierr)
  use module_plotts_graphics
  implicit none
  integer, intent(in) :: namelist_fortran_unit
  character(len=256)  :: fldname
  character(len=256)  :: info
  real :: y_interval
  real :: y_minimum
  real :: y_maximum
  character(len=1024) :: color
  integer :: soil_level
  real    :: ts_lat, ts_lon
  logical :: lopen
  logical :: callframe
  integer, intent(out)  :: ierr

  namelist/plot_info/ fldname, soil_level, &
       y_interval, y_minimum, y_maximum, color, ts_lat, ts_lon, callframe, info

! Default values:
  soil_level = 1
  ts_lat = -1.E33
  ts_lon = -1.E33
  if (pltts_parameter%callframe) then
     pltts_parameter%count = 0
  endif

  inquire(namelist_fortran_unit,opened=lopen)
  if (.not. lopen) then
     open(namelist_fortran_unit, file="namelist.pltts", status='old', form='formatted', action='read')
  endif

  read(namelist_fortran_unit, plot_info, iostat=ierr) 
  if (ierr/=0) then
     print*, 'Read ierr = ', ierr
     close(namelist_fortran_unit)
     return
  endif
  
  if (ts_lat > -1.E25) then
     pltts_parameter%latitude = ts_lat
  endif
  if (ts_lon > -1.E25) then
     pltts_parameter%longitude = ts_lon
  endif
  pltts_parameter%info = info
  pltts_parameter%fldname = fldname
  pltts_parameter%soil_level = soil_level
  pltts_parameter%y_interval = y_interval
  pltts_parameter%y_minimum = y_minimum
  pltts_parameter%y_maximum = y_maximum
  pltts_parameter%color = trim(color)
  pltts_parameter%callframe = callframe
  pltts_parameter%count = pltts_parameter%count + 1
  print*, 'lat/lon = ', pltts_parameter%latitude, pltts_parameter%longitude

end subroutine read_plotinfo_namelist
