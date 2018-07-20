program da_thin
!----------------------------------------------------------------------
! Purpose: Thinning wrfinput by using decimation
!
! Input  : wrfinput_hires -- High resolution wrfinput
!
! Output : wrfinput_lores -- Thinned wrfinput
!
! jliu@ucar.edu, 2011-12-15
!----------------------------------------------------------------------

  use netcdf

  implicit none

  integer :: i, n

  integer :: ncidin, ncidout, varid, varid_out, status
  integer :: nDims, nVars, nGlobalAtts, numsAtts, nTimes
  integer :: dLen, attLen, xtype, dID, unlimDimID, TID 
  integer :: divided_exactly, dimid

  integer :: dsizes(4), start(4), stride(4)

  integer, dimension(nf90_max_var_dims) :: vDimIDs

  integer, dimension(:),    allocatable :: vdimsizes

  real :: fVal

  real,                 dimension(:,:,:,:),  allocatable :: fVar
  integer,              dimension(:,:,:,:),  allocatable :: iVar
  character (len = 19), dimension(:),        allocatable :: times

  character (len = 14 )           :: coordinates
  character (len = NF90_MAX_NAME) :: vNam, dNam, attNam

  integer             :: decimation_factor = 3
  integer             ::            offset = 2
  character (len=255) ::            filin  = "wrfinput_hires"
  character (len=255) ::            filout = "wrfinput_lores"
  character (len=255) ::               arg = ""
  character (len=255) ::           appname = ""
  character(len=8)    ::            i_char =""

  integer iargc

  call getarg(0, appname)
  n=index(appname, '/', BACK=.true.)
  appname = trim(appname(n+1:))
    
  DO i = 1, iargc(), 2
    call getarg(i, arg)
    select case ( trim(arg) )
      case ("-i")
        call getarg(i+1, arg)
        filin=trim(arg)
      case ("-o")
        call getarg(i+1, arg)
        filout=trim(arg)
      case ("-thin")
        call getarg(i+1, arg)
        read(arg, '(i3)') decimation_factor
      case default 
        Write(*,*) "Usage : "//trim(appname)//" [-i inputfile] [-o outputfile] [-thin decimation_factor] [-h]"
        Write(*,*) "  -i     Optional, input filename,    default - wrfinput_hires"
        Write(*,*) "  -o     Optional, output filename,   default - wrfinput_lores"
        Write(*,*) "  -thin  Optional, decimation factor, default - 3"
        Write(*,*) "  -h     Shwo this usage"
        call exit(0)
    end select
  END DO

  if ( mod(decimation_factor,2) == 0 ) then
    Write(*,*) "\nError : decimation factor must be odd number\n"
    call exit(-1)
  endif

  status = nf90_open(filin, NF90_NOWRITE, ncidin)
  if ( status /= nf90_noerr ) then
    Write (*,*) "File open error. Please link the input file to "//trim(filin)
    call exit(-1)
  endif

  status = nf90_inq_dimid(ncidin, "west_east_stag", dimid)
  status = nf90_inquire_dimension(ncidin, dimid, len=dLen)
  divided_exactly = mod((dLen-1),decimation_factor)

  status = nf90_inq_dimid(ncidin, "south_north_stag", dimid)
  status = nf90_inquire_dimension(ncidin, dimid, len=dLen)
  divided_exactly = divided_exactly + mod((dLen-1),decimation_factor)

  if ( divided_exactly /= 0 ) then
    Write (*,fmt='(a,i2,a)') "Failed to thinning. Grids need to match : ( n - 1 ) mod ",decimation_factor," = 0"
    call exit(-1)
  endif

  status = nf90_create(filout, NF90_CLOBBER, ncidout)
  if ( status /= nf90_noerr) call nf90_handle_err(status)

  status = nf90_inquire(ncidin, nDims, nVars, nGlobalAtts, unlimDimID)
  if ( status /= nf90_noerr ) call nf90_handle_err(status)

  write (i_char, '(i8)') decimation_factor

  Write (*,*) "       Input file : "//trim(filin)
  Write (*,*) "      Output file : "//trim(filout)
  Write (*,*) "decimation factor : "//adjustl(i_char)

  do i=1, nGlobalAtts
    status = nf90_inq_attname(ncidin, NF90_GLOBAL, i, attNam)
    select case (trim(attNam))
      case ( "WEST-EAST_GRID_DIMENSION", "SOUTH-NORTH_GRID_DIMENSION", &
             "WEST-EAST_PATCH_END_UNSTAG", "WEST-EAST_PATCH_END_STAG", &
             "SOUTH-NORTH_PATCH_END_UNSTAG", "SOUTH-NORTH_PATCH_END_STAG" )
        status = nf90_get_att(ncidin, NF90_GLOBAL, attNam, fVal)
        status = nf90_put_att(ncidout, NF90_GLOBAL, attNam, int(( fVal - 1 ) / decimation_factor + 1) )
      case ("DX","DY", "DT" )
        status = nf90_get_att(ncidin, NF90_GLOBAL, attNam, fVal)
        status = nf90_put_att(ncidout, NF90_GLOBAL, attNam, fVal * decimation_factor )
      case default
        status = nf90_copy_att(ncidin, NF90_GLOBAL, attNam, ncidout, NF90_GLOBAL)
      end select
  end do

  allocate (vdimsizes(nDims), stat=status)

  do i=1, nDims

    status = nf90_inquire_dimension(ncidin, i, name=dNam, len = dLen)

    vdimsizes(i) = dLen
    select case (trim(dNam))
      case ("south_north_stag", "west_east_stag")
        vdimsizes(i) = (dLen - 1 ) / decimation_factor + 1
      case ("west_east", "south_north")
        vdimsizes(i) = dLen / decimation_factor
      case ("Time")
        allocate(times(dLen), stat=status)
        vdimsizes(i) = NF90_UNLIMITED
        nTimes = dLen
        TID = i
    end select

    status = nf90_def_dim(ncidout, dNam, vdimsizes(i), dID)

  end do

  vdimsizes(TID) = nTimes

  do varid=1, nVars
    status = nf90_inquire_variable(ncidin,varid,name=vNam,xtype=xtype,ndims=nDims,dimids=vDimIDs,natts=numsAtts)
    status = nf90_def_var(ncidout, trim(vNam), xtype, vDimIDs(1:nDims), varid_out)
    if(status /= nf90_NoErr) call nf90_handle_err(status)
    do i=1, numsAtts
      status = nf90_inq_attname(ncidin, varid, i, attNam)
      status = nf90_copy_att(ncidin, varid, trim(attNam), ncidout, varid_out)
      if(status /= nf90_NoErr) call nf90_handle_err(status)
    end do
  end do

  status = nf90_enddef(ncidout)

  offset = (decimation_factor + 1) / 2

  do varid=1, nVars

    status = nf90_inquire_variable(ncidin,varid,name=vNam,xtype=xtype,ndims=nDims,dimids=vDimIDs)

    dsizes = 1
    do i = 1 , nDims
      dsizes(i) = vdimsizes(vDimIDs(i))
    end do

    status = nf90_inquire_attribute(ncidin,varid,"coordinates")

    if ( status == nf90_noerr ) then

      Write(*,*) "Thinning for "//trim(vNam)

      coordinates=char(0)
      status = nf90_get_att(ncidin, varid, "coordinates" , coordinates)
      !print *, coordinates

      stride=(/decimation_factor,decimation_factor,1,1/)

      n = index(coordinates, char(0)) - 1
      if ( n < 0 ) n = len(coordinates)

      select case (trim(coordinates(1:n)))
        case ("XLONG_V XLAT_V")
          start=(/offset,1,1,1/)
        case ("XLONG_U XLAT_U")
          start=(/1,offset,1,1/)
        case ("XLONG XLAT")
          start=(/offset,offset,1,1/)
        case ("XLONG XLAT XTI")
          start=(/offset,offset,1,1/)
        case default
          print *, "Unkown coordinates : "//coordinates
          call exit(-1)
      end select

    else

      stride = 1
      start  = 1 
      
      if ( trim(vNam) == 'XLONG' .or. trim(vNam) == 'XLAT' ) then
          stride = (/decimation_factor,decimation_factor,1,1/)
          start  = (/offset,offset,1,1/)
      endif 

    endif

    select case (xtype)
      case (nf90_float)
        allocate(fVar( dsizes(1), dsizes(2), dsizes(3), dsizes(4) ), stat=status)
        status = nf90_get_var(ncidin, varid, fVar, start=start, stride=stride)
        if ( status == nf90_noerr ) then
          if ( vNam == "RDX" .or. vNam == "RDY" ) then
            status = nf90_put_var(ncidout, varid, fVar / decimation_factor)
          else
            status = nf90_put_var(ncidout, varid, fVar)
          endif
          if ( status /= nf90_noerr) call nf90_handle_err(status)
        else
          call nf90_handle_err(status)
        endif
        deallocate(fVar, stat=status)
      case (nf90_int)
        allocate(iVar( dsizes(1), dsizes(2), dsizes(3), dsizes(4) ), stat=status)
        status = nf90_get_var(ncidin, varid, iVar, start=start, stride=stride)
        if ( status == nf90_noerr ) then
          status = nf90_put_var(ncidout, varid, iVar)
          if ( status /= nf90_noerr) call nf90_handle_err(status)
        else
          call nf90_handle_err(status)
        endif
        deallocate(iVar, stat=status)
      case (nf90_char)
        if ( trim(vNam) == "Times") then
          status = nf90_get_var(ncidin, varid, times)
          if ( status == nf90_noerr ) then
            status = nf90_put_var(ncidout, varid, times)
            if ( status /= nf90_noerr) call nf90_handle_err(status)
          else
            call nf90_handle_err(status)
          endif
          deallocate(times, stat=status)
        else
          print *, "Unkown character variable :"//trim(vNam)
          call exit(-1)  
        endif
      case default
        print *, "Unkown xtype : ", xtype
        call exit(-1)
    end select
  end do

  status = nf90_close(ncidin)
  status = nf90_close(ncidout)

  Write(*,*) "Completed thinning successfully"

contains

  subroutine nf90_handle_err(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      call exit(-1) 
    end if
  end subroutine nf90_handle_err

end program da_thin
