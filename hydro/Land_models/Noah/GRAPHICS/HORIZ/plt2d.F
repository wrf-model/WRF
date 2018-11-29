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

!
! ############################################################################################################
!

program plt2d
  use module_2ddata
  use kwm_date_utilities
  use module_plot2d_graphics
  implicit none

  character(len=256) :: ldasout_flnm_template
  character(len=256) :: diff_flnm_template
  character(len=10) :: plot_startdate
  character(len=10) :: plot_enddate
  integer           :: plot_interval
  character(len=10) :: nowdate
  character(len=10) :: fldname
  real, pointer, dimension(:,:) :: fldptr, fldptr2
  integer :: soil_level
  type(gridinfo_type) :: gridinfo
  integer :: ierr

  integer, parameter :: namelist_fortran_unit = 10
  character(len=256) :: namelist_filename
  integer, external :: iargc

  print*, 'iargc() = ', iargc()
  if (iargc() == 1) then
     call getarg(1, namelist_filename)
  else
     namelist_filename = "namelist.plt2d"
  endif

  call read_fileinfo_namelist(namelist_filename, namelist_fortran_unit, &
       ldasout_flnm_template, diff_flnm_template, plot_startdate, plot_enddate, plot_interval)

  nowdate = plot_startdate

  call init_ncargks()

  do while (nowdate <= plot_enddate)
     print*, 'Date = ', nowdate

     do 
        call read_plotinfo_namelist(namelist_filename, namelist_fortran_unit, ierr, fldname, soil_level)
        if (ierr /= 0) exit

        call get_field(trim(ldasout_flnm_template), nowdate, trim(fldname), fldptr, gridinfo, &
             level=soil_level)

        if (diff_flnm_template /= "") then
           call get_field(trim(diff_flnm_template), nowdate, trim(fldname), fldptr2, gridinfo, &
                level=soil_level)
           fldptr = fldptr - fldptr2
           nullify(fldptr2)
        endif
     
        call draw_field(fldptr, gridinfo%idim, gridinfo%jdim, gridinfo)
        nullify(fldptr)

     enddo

     call geth_newdate(nowdate, nowdate, plot_interval)
  enddo

  call close_ncargks()
  
end program plt2d

!
! ############################################################################################################
!

subroutine read_fileinfo_namelist(namelist_filename, namelist_fortran_unit, &
     ldasout_flnm_template, diff_flnm_template, plot_startdate, plot_enddate, plot_interval)
  use module_plot2d_graphics
  implicit none
  character(len=*), intent(in) :: namelist_filename
  integer, intent(in) :: namelist_fortran_unit
  character(len=256)  :: ldasout_flnm_template
  character(len=256)  :: diff_flnm_template
  character(len=10)   :: plot_startdate
  character(len=10)   :: plot_enddate
  integer             :: plot_interval

  namelist/file_info/ ldasout_flnm_template, plot_startdate, plot_enddate, plot_interval, diff_flnm_template

  plot_interval = 1
  diff_flnm_template = ""

  open(namelist_fortran_unit, file=namelist_filename, status='old', form='formatted', action='read')

  read(namelist_fortran_unit, file_info) 

end subroutine read_fileinfo_namelist

!
! ############################################################################################################
!

subroutine read_plotinfo_namelist(namelist_filename, namelist_fortran_unit, ierr, fldname, soil_level)
  use module_plot2d_graphics
  implicit none
  character(len=*), intent(in) :: namelist_filename
  integer, intent(in) :: namelist_fortran_unit
  character(len=10)  :: fldname
  real :: contour_interval
  real :: contour_minimum
  real :: contour_maximum
  character(len=1024) :: color_table
  character(len=10)   :: plot_type
  integer :: soil_level
  logical :: lopen
  integer, intent(out)  :: ierr


  namelist/plot_info/ fldname, soil_level, &
       contour_interval, contour_minimum, contour_maximum, color_table, plot_type


! Default values:
  soil_level = 1
  plot_type = "contour"

  inquire(namelist_fortran_unit,opened=lopen)
  if (.not. lopen) then
     open(namelist_fortran_unit, file=namelist_filename, status='old', form='formatted', action='read')
  endif

  read(namelist_fortran_unit, plot_info, iostat=ierr) 
  if (ierr/=0) then
     close(namelist_fortran_unit)
     return
  endif
  ! ldasout_flnm_template = "/d5/kmanning/HRLDAS/HRLDAS/Run_USWRP_EXP7/<yyyymmddhh>.LDASOUT_DOMAIN2"
  ! plot_startdate = "2002060900"
  ! plot_enddate   = "2002060906"
  ! fldname = "SOIL_M"
  plt2d_parameter%contour_interval = contour_interval
  plt2d_parameter%contour_minimum = contour_minimum
  plt2d_parameter%contour_maximum = contour_maximum
  plt2d_parameter%color_table = trim(color_table)
  plt2d_parameter%plot_type = trim(plot_type)

end subroutine read_plotinfo_namelist

!
! ############################################################################################################
!
!KWM
!KWMsubroutine plotmap(gridinfo)
!KWM  use module_2ddata
!KWM  implicit none
!KWM  type(gridinfo_type), intent(in) :: gridinfo
!KWM  real :: plm1, plm2, plm3, plm4
!KWM  character(len=2), parameter, dimension(3) :: project = (/"LC", "ST", "ME"/)
!KWM  integer :: ierr
!KWM  real :: xl, xr, xb, xt, wl, wr, wb, wt
!KWM  integer :: ml
!KWM
!KWM  call xytoll_generic(1.0, 1.0, plm1, plm2,  &
!KWM       project(gridinfo%iproj), gridinfo%dxm*0.001, gridinfo%lat1, gridinfo%lon1, 1.5, 1.5, &
!KWM       gridinfo%xlonc, gridinfo%truelat1, gridinfo%truelat2)
!KWM
!KWM  call xytoll_generic(real(gridinfo%idim), real(gridinfo%jdim), plm3, plm4,  &
!KWM       project(gridinfo%iproj), gridinfo%dxm*0.001, gridinfo%lat1, gridinfo%lon1, 1.5, 1.5, &
!KWM       gridinfo%xlonc, gridinfo%truelat1, gridinfo%truelat2)
!KWM
!KWM  if (project(gridinfo%iproj) == "LC") then
!KWM     ! Lambert Conformal
!KWM     call supmap(3, gridinfo%truelat1, gridinfo%xlonc, gridinfo%truelat2, &
!KWM          plm1, plm2, plm3, plm4, 2, 0, 4, 0, ierr)
!KWM  else
!KWM     write(*,'("Unrecognized map projection: ",A)') project(gridinfo%iproj)
!KWM     stop
!KWM  endif
!KWM
!KWM  call getset(xl, xr, xb, xt, wl, wr, wb, wt, ml)
!KWM  call set(xl, xr, xb, xt, 1.0, float(gridinfo%idim), 1.0, float(gridinfo%jdim), ml)
!KWM  
!KWM
!KWMend subroutine plotmap
!KWM
!
! ############################################################################################################
!

