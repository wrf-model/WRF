program da_bilin

!----------------------------------------------------------------------
! Purpose: Regridding increment from low-resolution to high-resolution
!                     by using bilinear interpolation
!
! Input     : fg                   -- low resolution first guess file
!             wrfvar_output        -- low resolution analysis file
!             wrfinput_hires       -- high resolution first guess file
!
! Output    : wrfvar_output_hires  -- regridded high resolution analysis
!
! Increment           = an_lores  - fg_lores
! wrfvar_output_hires = Increment + wrfinput_hires
!
! In order to keep the domain size, it needs to match ( n - 1 )*ns + 1 
!
! where n  is the grid number in x or y
!       ns is the refinement ratio between two resulotions
!
! Compile:
!
!   pgf90 -o da_bilin.exe -I$NETCDF/include -L$NETCDF/lib -lnetcdf da_bilin.f90
!
! Usage:
!
!   da_bilin.exe  [-h] [-fg_lores filename] [-an_lores filename] 
!                 [-fg_hires filename] [-ns n ] [-o outputfile]
!
!     -fg_lores    Optional, low resulotion first guess file,                 default - fg"
!     -an_lores    Optional, low resulotion analysis file comes from wrfvar,  default - wrfvar_output"
!     -fg_hires    Optional, high resultion first guess file,                 default - wrfinput_hires"
!     -ns          Optional, the refinement ratio between two resulotions,    default - 3"
!     -o           Optional, output high resulotion analysis file,            default - wrfvar_output_hires"
!     -h           Show this help"
!
! jliu@ucar.edu , 2011-12-15
!----------------------------------------------------------------------

  use netcdf

  implicit none

  !These variables' incremental will be regridded by default
  character (len=6), dimension(1:19) :: vNam 

  integer :: i, j, k, n, status
  integer :: nLat, nLon, oLat, oLon
  integer :: sLat, eLat, sLon, eLon
  integer :: rLat, rLon

  integer :: ncidfg, ncidan, ncidout
  integer :: varid, nDims, dLen, varid_fg, varid_an, dimid
  integer :: regridsize, domainsize_out

  real, dimension(:,:,:,:), allocatable :: fg, an, increment, var_out
  real, dimension(:,:),     allocatable :: iVar, oVar

  integer, dimension(nf90_max_var_dims) :: vDimIDs
  integer, dimension(4)                 :: vdimsizes

  character (len = 19), dimension(:),        allocatable :: times
  character (len = 255)  :: appname   = ""
  character (len = 255)  :: arg       = ""
  character (len = 255)  :: fg_lores  = "fg"
  character (len = 255)  :: an_lores  = "wrfvar_output"
  character (len = 255)  :: fg_hires  = "wrfinput_hires"
  character (len = 255)  :: f_out     = "wrfvar_output_hires"
  character (len = 255)  :: errmsg    = ""
  character (len = 8)    :: i_char    = ""

  integer                :: ns        = 3
  !integer                :: cloud_cv_options = 0
  !integer                :: cv_w      = 0 

  LOGICAL :: file_exists

  integer iargc

  !These variables' incremental will be regridded by default

  vNam(1)="U"
  vNam(2)="V"
  vNam(3)="T"
  vNam(4)="QVAPOR"
  vNam(5)="PH"
  vNam(6)="P"
  vNam(7)="MU"
  vNam(8)="U10"
  vNam(9)="V10"
  vNam(10)="T2"
  vNam(11)="Q2"
  vNam(12)="PSFC"
  vNam(13)="TH2"

  vNam(14)="QCLOUD"
  vNam(15)="QRAIN"
  vNam(16)="QICE"
  vNam(17)="QSNOW"
  vNam(18)="QGRAUP"
  vNam(19)="W"

  call getarg(0, appname)
  n=index(appname, '/', BACK=.true.)
  appname = trim(appname(n+1:))

  DO i = 1, iargc(), 2
    arg=""
    call getarg(i, arg)
    select case ( trim(arg) )
      case ("-fg_lores")
        call getarg(i+1, arg)
        fg_lores=trim(arg)
      case ("-an_lores")
        call getarg(i+1, arg)
        an_lores=trim(arg)
      case ("-fg_hires")
        call getarg(i+1, arg)
        fg_hires=trim(arg)
      case ("-ns")
        call getarg(i+1, arg)
        read(arg, '(i3)') ns 
      case ("-o")
        call getarg(i+1, arg)
        f_out=trim(arg)
      !case ("-cloud_cv_options")
      !  call getarg(i+1, arg)
      !  read(arg, '(i3)') cloud_cv_options
      !case ("-cv_w")
      !  call getarg(i+1, arg)
      !  read(arg, '(i3)') cv_w
      case default
        call show_usage()
        call exit(0)
    end select
  END DO

  write (i_char, '(i8)') ns

  inquire(FILE=trim(fg_hires), EXIST=file_exists)

  if ( .not. file_exists ) then
    Write(*,*) "\nError: "//trim(fg_hires)//" not exists\n"
    call show_usage()
    call exit(-1)
  endif
  
  call system("cp "//fg_hires//" "//f_out)

  status = nf90_open(fg_lores, NF90_NOWRITE, ncidfg)
  errmsg = trim(fg_lores)
  if ( status /= nf90_noerr ) call nf90_handle_err(status, errmsg)

  status = nf90_open(an_lores, NF90_NOWRITE, ncidan)
  errmsg = trim(an_lores)
  if ( status /= nf90_noerr ) call nf90_handle_err(status, errmsg)

  status = nf90_open(f_out, NF90_WRITE, ncidout)
  errmsg= trim(f_out)
  if ( status /= nf90_noerr ) call nf90_handle_err(status, errmsg)

  status = nf90_inq_dimid(ncidout, "west_east_stag", dimid)
  status = nf90_inquire_dimension(ncidout, dimid, len=dLen)
  domainsize_out = dLen
  
  status = nf90_inq_dimid(ncidout, "south_north_stag", dimid)
  status = nf90_inquire_dimension(ncidout, dimid, len=dLen)
  domainsize_out = domainsize_out * dLen

  status = nf90_inq_dimid(ncidfg, "west_east_stag", dimid)
  status = nf90_inquire_dimension(ncidfg, dimid, len=dLen)
  regridsize = (dLen-1)*ns+1

  status = nf90_inq_dimid(ncidfg, "south_north_stag", dimid)
  status = nf90_inquire_dimension(ncidfg, dimid, len=dLen)
  regridsize = regridsize * ( (dLen-1)*ns+1 )

  if ( regridsize /= domainsize_out ) then
    Write(*,'(a,i2,a)') "Error : It needs to match m = (n-1)*",ns, &
                        "+1 where n is coarse grid number in x or y, "// &
                        "m is fine grid number in x or y."
    call exit(-1) 
  end if

  write (i_char, '(i8)') ns

  Write(*,*) " Input :"
  Write(*,*) "   Low resolution first guess  : "//trim(fg_lores)
  Write(*,*) "   Low resolution analysis     : "//trim(an_lores)
  Write(*,*) "   High resolution first guess : "//trim(fg_hires)
  Write(*,*) "   ns                          : "//adjustl(i_char)
  Write(*,*) "Output :"
  Write(*,*) "   High resolution analysis    : "//trim(f_out)

  errmsg = ""

  n = ubound(vNam,1)
  do i=1,n 

    Write (*,*) "Regridding increment for "//trim(vNam(i))

    status = nf90_inq_varid(ncidout, trim(vNam(i)), varid)
    status = nf90_inquire_variable(ncidout, varid, ndims=nDims,dimids=vDimIDs)

    vdimsizes = 1
    do j=1, nDims
      status = nf90_inquire_dimension(ncidout, vDimIDs(j), len = dLen )
      vdimsizes(j) = dLen
    end do

    allocate(var_out(vdimsizes(1), vdimsizes(2), vdimsizes(3), vdimsizes(4)), stat=status)

    status = nf90_get_var(ncidout, varid, var_out)

    status = nf90_inq_varid(ncidfg, trim(vNam(i)), varid_fg)
    status = nf90_inq_varid(ncidan, trim(vNam(i)), varid_an)

    status = nf90_inquire_variable(ncidfg, varid_fg, ndims=nDims,dimids=vDimIDs)

    vdimsizes = 1
    do j=1, nDims
      status = nf90_inquire_dimension(ncidfg, vDimIDs(j), len = dLen )
      vdimsizes(j) = dLen
    end do

    allocate(fg(vdimsizes(1), vdimsizes(2), vdimsizes(3), vdimsizes(4)), stat=status)
    allocate(an(vdimsizes(1), vdimsizes(2), vdimsizes(3), vdimsizes(4)), stat=status)
    allocate(increment(vdimsizes(1), vdimsizes(2), vdimsizes(3), vdimsizes(4)), stat=status)

    status = nf90_get_var(ncidfg, varid_fg, fg)
    status = nf90_get_var(ncidan, varid_an, an)

    increment = an - fg

    nLon = vdimsizes(1)
    nLat = vdimsizes(2)

    if ( trim(vNam(i) ) == "U" ) then
      rLat = nLat * ns
      rLon = (nLon-1) * ns + 1
      nLat = nLat + 2
    else
      rLon = nLon * ns
      rLat = (nLat-1) * ns + 1
      nLon = nLon + 2
      if ( trim(vNam(i)) /= "V" ) then
        rLat = nLat * ns
        nLat =  nLat + 2
      endif
    endif

    oLon = ( nLon - 1 ) * ns + 1
    oLat = ( nLat - 1 ) * ns + 1

    elat = (oLat - rLat) / 2
    slat =  oLat - rLat - elat + 1

    elon = (oLon - rLon) / 2
    slon =  oLon - rLon - elon + 1

    allocate(iVar(nLon, nLat), stat=status)
    allocate(oVar(oLon, oLat), stat=status)

    do j=1, vdimsizes(4)
      do k=1, vdimsizes(3)

        iVar = 0
        oVar = 0

        select case ( trim(vNam(i)) ) 
          case ("U")
            iVar(:,2:nlat-1) = increment(:,:,k,j)
            iVar(:,1)        = iVar(:,2)
            iVar(:,nlat)     = iVar(:,nlat-1)
          case ("V")
            iVar(2:nlon-1,:) = increment(:,:,k,j)
            iVar(1,:)        = iVar(2,:)
            iVar(nlon,:)     = iVar(nlon-1,:)
          case default
            iVar(2:nlon-1,2:nlat-1) = increment(:,:,k,j)
            iVar(1,:)               = iVar(2,:)
            iVar(nlon,:)            = iVar(nlon-1,:)
            iVar(:,1)               = iVar(:,2)
            iVar(:,nlat)            = iVar(:,nlat-1)
        end select

        call bilin(iVar, nLon, nLat, ns, oVar, oLon, oLat)

        select case ( trim(vNam(i)) )
          case ("U")
            var_out(:,:,k,j) = var_out(:,:,k,j) + oVar(:,slat:olat-elat)
          case ("V")
            var_out(:,:,k,j) = var_out(:,:,k,j) + oVar(slon:olon-elon,:)
          case default
            var_out(:,:,k,j) = var_out(:,:,k,j) + oVar(slon:olon-elon,slat:olat-elat)
        end select

      end do
    end do

    status = nf90_put_var(ncidout, varid, var_out)    

    deallocate(var_out, stat=status)
    deallocate(iVar, stat=status)
    deallocate(oVar, stat=status)
    deallocate(fg, stat=status)
    deallocate(an, stat=status)
    deallocate(increment, stat=status)

  end do

  status = nf90_close(ncidfg)
  status = nf90_close(ncidan)
  status = nf90_close(ncidout)

  Write(*,*) "Regridding increment completed successfully"

contains
  subroutine show_usage()
     Write(*,*) 'Usage :'//trim(appname)// &
     '[-h] [-fg_lores filename] [-an_lores filename] [-fg_hires filename] [-ns n ] [-o outputfile]'
     Write(*,*) "  -fg_lores    Optional, low resulotion first guess file,                 default - fg"
     Write(*,*) "  -an_lores    Optional, low resulotion analysis file comes from wrfvar,  default - wrfvar_output"
     Write(*,*) "  -fg_hires    Optional, high resultion first guess file,                 default - wrfinput_hires"
     Write(*,*) "  -ns          Optional, the refinement ratio between two resulotions,    default - 3"
     Write(*,*) "  -o           Optional, output high resulotion analysis file,            default - wrfvar_output_hires"
     Write(*,*) "  -h           Show this help"
  end subroutine show_usage

  subroutine nf90_handle_err(status, errmsg)
    integer,          intent(in) :: status
    character(len=*), intent(in) :: errmsg

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))//" : "//trim(errmsg)
      Stop
    end if
  end subroutine nf90_handle_err

  subroutine bilin(old,xi,yi,ns,new,xo,yo)

    implicit none

    integer,                 intent(in) :: xi,yi,xo,yo
    real, dimension(xi,yi),  intent(in) :: old
    integer,                 intent(in) :: ns
    real, dimension(xo,yo),  intent(out):: new

    real                                :: im(1:ns+1,2)
    integer:: i,j,jm1,im1,ix1,ix2,iy1,iy2

    forall(i=1:ns+1) im(i,2) = real(i-1)/ns
      im(:,1) = 1 - im(:,2)

    do j=2,yi
      jm1 = j - 1
      iy2 = jm1 * ns + 1
      iy1 = iy2 - ns
      do i=2,xi
        im1 = i - 1
        ix2 = im1 * ns + 1
        ix1 = ix2 - ns
        new(ix1:ix2,iy1:iy2) = matmul(im,matmul(old(im1:i,jm1:j),transpose(im)))
      end do
    end do

  end subroutine bilin

end program da_bilin
