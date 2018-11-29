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

module module_modify_wrfinput
  use netcdf

contains

  subroutine find_dimension(ncid, name, idim)
    !
    ! Given an NCID and a dimension name, return the dimension value.
    !
    implicit none
    ! Input:
    integer, intent(in)          :: ncid
    character(len=*), intent(in) :: name
    ! Output:
    integer, intent(out)         :: idim

    ! Local:
    integer :: iret, dimid

    iret = nf90_inq_dimid(ncid, trim(name), dimid)
    call error_handler(iret, failure="Problem finding dimension id for '"//trim(name)//"'")

    iret = nf90_inquire_dimension(ncid, dimid, len=idim)
    call error_handler(iret, failure="Problem finding '"//trim(name)//"' dimension value")
  end subroutine find_dimension

  subroutine error_handler(status, failure, success)
    !
    ! Check the error flag from a NetCDF function call, and print appropriate
    ! error message.
    !
    implicit none
    integer,                    intent(in) :: status
    character(len=*), optional, intent(in) :: failure
    character(len=*), optional, intent(in) :: success
    if (status .ne. NF90_NOERR) then
       write(*,'(/,A)') nf90_strerror(status)
       if (present(failure)) then
          write(*,'(/," ***** ", A,/)') failure
       endif
       stop 'Stopped'
    endif
    if (present(success)) then
       write(*,'(A)') success
    endif
  end subroutine error_handler


end module module_modify_wrfinput


program modify_wrfinput
  use module_modify_wrfinput
  implicit none
  character(len=256) :: wrfinput = " "
  character(len=256) :: ldasout = " "
  integer, external :: iargc
  integer :: numarg

  integer :: i
  integer :: iret, wrf_ncid, ldas_ncid, copy_ncid
  integer :: ix, jx,lx
  logical :: do_urban = .FALSE.
  character(len=256) :: arg

  numarg = iargc()

  if ((numarg /= 2) .and. (numarg /= 3)) then
     print*, 'Usage:  modify_wrfinput [-urban] [-help] <wrfinput> <ldasout>'
     print*, '           Where <wrfinput> is the destination file'
     print*, '             and <ldasout> is the source file.'
     stop
  else
     ARGLOOP : do i = 1, numarg
        call getarg (i, arg)
        if ((arg == "-urban") .or. (arg == "--urban")) then
           do_urban = .TRUE.
           cycle ARGLOOP
        endif
        if (( arg == "-h") .or. (arg == "-help") .or. (arg == "--help") ) then
           print*, 'Usage:  modify_wrfinput [-urban] [-help] <wrfinput> <ldasout>'
           print*, '           Where <wrfinput> is the destination file'
           print*, '             and <ldasout> is the source file.'
           stop
        endif
        if ( arg(1:1) == "-") then
           ! All arguments beginning with "-" should have been processed by now.
           print*, 'Unrecognized argument:  ', trim(arg)
           print*, 'Usage:  modify_wrfinput [-urban] [-help] <wrfinput> <ldasout>'
           print*, '           Where <wrfinput> is the destination file'
           print*, '             and <ldasout> is the source file.'
           stop
        endif
        if (wrfinput == " ") then
           wrfinput = arg
           cycle ARGLOOP
        endif
        if (ldasout == " ") then
           ldasout = arg
           cycle ARGLOOP
        endif
     enddo ARGLOOP
  endif

  iret = nf90_open(trim(wrfinput), NF90_NOWRITE, wrf_ncid)
  call error_handler(iret, failure="Problem opening file '"//trim(wrfinput)//"'")
  write(*, '(A, I5)') "Opening NetCDF wrfinput file '"//trim(wrfinput)//"' with wrf_ncid = ", &
       wrf_ncid

  iret = nf90_open(trim(ldasout), NF90_NOWRITE, ldas_ncid)
  call error_handler(iret, failure="Problem opening file '"//trim(ldasout)//"'")
  write(*, '(A, I5)') "Opening NetCDF ldasout file '"//trim(ldasout)//"' with ldas_ncid = ", &
       ldas_ncid

  iret = nf90_create("new.nc", NF90_CLOBBER, copy_ncid)
  call error_handler(iret, failure="Problem creating file 'new.nc'")

! Copy a dataset   Causes mysterious crashes!
  call copy_dataset(wrf_ncid, copy_ncid)

  ! Find dimensions
  call find_dimension(wrf_ncid, "west_east", ix)
  call find_dimension(wrf_ncid, "south_north", jx)
  call find_dimension(wrf_ncid, "soil_layers_stag", lx)

  iret = nf90_close(wrf_ncid)
  call error_handler(iret, failure="Problem closing wrf_ncid")

  iret = nf90_open("new.nc", NF90_WRITE, copy_ncid)
  call error_handler(iret, failure="Problem opening file 'new.nc'")
  wrf_ncid = copy_ncid

  !
  ! Replace existing fields in our copy.
  !
  call copy_replace(ldas_ncid, "CANWAT", wrf_ncid, "CANWAT")
  call copy_replace(ldas_ncid, "SOIL_M", wrf_ncid, "SMOIS")
  call copy_replace(ldas_ncid, "SOIL_W", wrf_ncid, "SH2O")
  call copy_replace(ldas_ncid, "SOIL_T", wrf_ncid, "TSLB")
  call copy_replace(ldas_ncid, "VEGFRA", wrf_ncid, "VEGFRA")
  !
  ! Add new fields in our copy.
  !
  if (do_urban) then
     call copy_new(ldas_ncid, "TC", wrf_ncid, "TC_URB")
     call copy_new(ldas_ncid, "TR", wrf_ncid, "TR_URB")
     call copy_new(ldas_ncid, "TG", wrf_ncid, "TG_URB")
     call copy_new(ldas_ncid, "TB", wrf_ncid, "TB_URB")
     call copy_new(ldas_ncid, "TRL", wrf_ncid, "TRL_URB")
     call copy_new(ldas_ncid, "TGL", wrf_ncid, "TGL_URB")
     call copy_new(ldas_ncid, "TBL", wrf_ncid, "TBL_URB")
  endif

  iret = nf90_close(wrf_ncid)
  call error_handler(iret, failure="Problem closing wrf_ncid")
  print*, 'Normal termination of modify_wrfinput'

end program modify_wrfinput

subroutine copy_replace(source_ncid, source_name, dest_ncid, dest_name)
  !
  ! Copy the variable named <source_name> from dataset <source_ncid> to
  ! variable named <dest_name> in dataset <dest_ncid>.
  !
  use module_modify_wrfinput
  implicit none
  integer, intent(in) :: source_ncid, dest_ncid
  character(len=*), intent(in) :: source_name, dest_name

  integer :: source_varid, dest_varid, source_nvdims, dest_nvdims

  real, allocatable, dimension(:,:,:,:) :: xdum
  real, allocatable, dimension(:,:,:,:) :: dest_gone
  integer :: iret
  integer :: xtype, dest_xtype

  integer, dimension(NF90_MAX_VAR_DIMS) :: vstart
  integer, dimension(NF90_MAX_VAR_DIMS) :: vcount
  integer, dimension(NF90_MAX_VAR_DIMS) :: source_dimids
  integer, dimension(NF90_MAX_VAR_DIMS) :: dest_dimids
  
  integer :: j, destdim


  iret = nf90_inq_varid(source_ncid, source_name, source_varid)
  call error_handler(iret, failure="Problem returning varid of field '" & 
       //trim(source_name)//"' for source")

  iret = nf90_inq_varid(dest_ncid, dest_name, dest_varid)
  call error_handler(iret, failure="Problem returning varid of field '" & 
       //trim(dest_name)//"' for destination")

  iret = nf90_inquire_variable(source_ncid, source_varid, &
       xtype=xtype, ndims=source_nvdims, dimids=source_dimids)
  call error_handler(iret, failure="Problem inquiring variable.")

  iret = nf90_inquire_variable(dest_ncid, dest_varid, &
       xtype=dest_xtype, ndims=dest_nvdims, dimids=dest_dimids)
  call error_handler(iret, failure="Problem inquiring variable.")

  print*, 'name = ', trim(source_name), source_nvdims

  if (xtype /= dest_xtype) then
     print*, "Field types do not match."
     stop
  endif

  if (source_nvdims /= dest_nvdims) then
     print*, 'source_nvdims = ', source_nvdims
     print*, 'dest_nvdims = ', dest_nvdims
     stop "field dimensions do not match"
  endif

  ! The numbers of dimensions match.  Continue.

  if (source_nvdims > 4) stop "nvdims problem"

  ! We have an appropriate number of dimensions.  Continue

  vstart = 1
  vcount = 1

  do j = 1, source_nvdims
     iret = nf90_inquire_dimension(source_ncid, source_dimids(j), len=vcount(j))
     call error_handler(iret, failure="Problem inquiring dimension")
     iret = nf90_inquire_dimension(dest_ncid, dest_dimids(j), len=destdim)
     call error_handler(iret, failure="Problem inquiring dimension")

     if (vcount(j) /= destdim) then
        print*, 'source_dims = ', vcount(1:source_nvdims)
        print*, 'source_dim  = ', vcount(j)
        print*, 'dest_dims   = ', destdim
        stop "field dimensions do not match"
     endif
  enddo

  ! The sizes of dimension match.  Continue

  if (xtype == NF90_FLOAT) then

     allocate(xdum(vcount(1),vcount(2),vcount(3),vcount(4)))
     allocate(dest_gone(vcount(1),vcount(2),vcount(3),vcount(4)))

     iret = nf90_get_var(source_ncid, source_varid, xdum, start=vstart, count=vcount)
     call error_handler(iret, failure="Problem getting variable")

     if (source_name == "VEGFRA") then
        ! Vegetation fraction in wrfinput file is in percent, not in fraction.
        write(*,'(/," ***** Scaling VEGFRA by 100.0 for transport to wrfinput file.",/)')
        xdum = xdum * 100.0
     endif

     iret = nf90_get_var(dest_ncid, dest_varid, dest_gone, start=vstart, count=vcount)
     call error_handler(iret, failure="Problem getting variable")
     
     where (xdum < -1.E25) xdum = dest_gone

     iret = nf90_put_var(dest_ncid, dest_varid, xdum, start=vstart, count=vcount)
     call error_handler(iret, failure="Problem putting variable")

     deallocate(xdum)
     deallocate(dest_gone)

  else

     stop "xtype"

  endif

end subroutine copy_replace
  
subroutine copy_new(source_ncid, source_name, dest_ncid, dest_name)
  !
  ! Copy the variable named <source_name> from dataset <source_ncid> to
  ! variable named <dest_name> in dataset <dest_ncid>.
  !
  use module_modify_wrfinput
  implicit none
  integer, intent(in) :: source_ncid, dest_ncid
  character(len=*), intent(in) :: source_name, dest_name

  integer :: source_varid, dest_varid, source_nvdims

  real, allocatable, dimension(:,:,:,:) :: xdum
  integer :: iret
  integer :: xtype

  integer, dimension(NF90_MAX_VAR_DIMS) :: vstart
  integer, dimension(NF90_MAX_VAR_DIMS) :: vcount
  integer, dimension(NF90_MAX_VAR_DIMS) :: source_dimids
  integer, dimension(NF90_MAX_VAR_DIMS) :: dest_dimids

  integer :: j
  character(len=NF90_MAX_NAME) :: dimname

  iret = nf90_inq_varid(source_ncid, source_name, source_varid)
  call error_handler(iret, failure="Problem returning varid of field '" & 
       //trim(source_name)//"' for source")

  iret = nf90_inquire_variable(source_ncid, source_varid, &
       xtype=xtype, ndims=source_nvdims, dimids=source_dimids)
  call error_handler(iret, failure="Problem inquiring variable.")

  print*, 'name = ', trim(source_name), source_nvdims

  if (source_nvdims > 4) stop "nvdims problem"

  vstart = 1
  vcount = 1

  do j = 1, source_nvdims

     iret = nf90_inquire_dimension(source_ncid, source_dimids(j), name=dimname, len=vcount(j))
     call error_handler(iret, failure="Problem inquiring dimension")

     ! For each of <source_dimids>, find the corresponding <dest_dimids>:
     iret = nf90_inq_dimid(dest_ncid, trim(dimname), dest_dimids(j))
     if ((trim(dimname)=="Times") .and. (iret /= NF90_NOERR)) then
        ! Grrrr.  Dimension name might be "Time" instead of "Times".  Corrected in more recent HRLDAS.
        iret = nf90_inq_dimid(dest_ncid, "Time", dest_dimids(j))
     endif
     call error_handler(iret, failure="Problem inquiring dimid for dimension '"//trim(dimname)//"'")

  enddo

  if (xtype == NF90_FLOAT) then

     allocate(xdum(vcount(1),vcount(2),vcount(3),vcount(4)))

     iret = nf90_get_var(source_ncid, source_varid, xdum, start=vstart, count=vcount)
     call error_handler(iret, failure="Problem getting variable")
     
     ! Put our destination dataset into define mode.
     iret = nf90_redef(dest_ncid)
     call error_handler(iret, failure="Problem putting dataset in define mode (redef)")

     ! Define our new variable.

     print*, 'dest_dimids = ', dest_dimids(1:source_nvdims)
     iret = nf90_def_var(dest_ncid, trim(dest_name), xtype, dest_dimids(1:source_nvdims), dest_varid)
     call error_handler(iret, failure="Problem defining new variable in destination file.")

     ! Define attributes for our new variable.

     iret = nf90_put_att(dest_ncid, dest_varid, "FieldType", 104)
     call error_handler(iret, failure="Problem defining attribute 'FieldType'")

     iret = nf90_copy_att(source_ncid, source_varid, "MemoryOrder", dest_ncid, dest_varid)
     call error_handler(iret, failure="Problem copying attribute 'MemoryOrder'")

     iret = nf90_copy_att(source_ncid, source_varid, "description", dest_ncid, dest_varid)
     call error_handler(iret, failure="Problem copying attribute 'description'")

     iret = nf90_copy_att(source_ncid, source_varid, "units", dest_ncid, dest_varid)
     call error_handler(iret, failure="Problem copying attribute 'units'")

     iret = nf90_copy_att(source_ncid, source_varid, "stagger", dest_ncid, dest_varid)
     call error_handler(iret, failure="Problem copying attribute 'stagger'")

     iret = nf90_put_att(dest_ncid, dest_varid, "coordinates", "XLONG XLAT")
     call error_handler(iret, failure="Problem defining attribute 'coordinates'")

     ! Take us out of define mode.

     iret = nf90_enddef(dest_ncid)
     call error_handler(iret, failure="Problem exiting define mode.")

     ! Write the new variable.

     iret = nf90_put_var(dest_ncid, dest_varid, xdum, start=vstart, count=vcount)
     call error_handler(iret, failure="Problem putting variable")

     deallocate(xdum)

  else

     stop "xtype"

  endif

end subroutine copy_new
  
subroutine copy_dataset(wrf_ncid, copy_ncid)
  use module_modify_wrfinput
  implicit none
  integer, intent(in) :: wrf_ncid, copy_ncid
  integer :: iret, i, j

  integer :: ndims, nvars, ngatts, unlimdimid, natts
  character(len=NF90_MAX_NAME) :: name
  integer :: lendd,  xtype, nvdims
  integer, dimension(NF90_MAX_VAR_DIMS) :: dimids

  real,             allocatable, dimension(:,:,:,:) :: xdum
  integer,          allocatable, dimension(:,:,:,:) :: idum
  character(len=1), allocatable, dimension(:,:,:,:) :: char_array
  integer, dimension(NF90_MAX_VAR_DIMS) :: vstart
  integer, dimension(NF90_MAX_VAR_DIMS) :: vcount

  iret = nf90_inquire(wrf_ncid, ndims, nvars, ngatts, unlimdimId)
  call error_handler(iret, failure="Problem inquiring on wrf_ncid")

  ! Copy the dimensions
  do i = 1, ndims

     iret = nf90_inquire_dimension(wrf_ncid, i, name, lendd)
     call error_handler(iret, failure="Problem inquiring dimension for wrf_ncid")

!KWM     if (i /= unlimdimid) then
     iret = nf90_def_dim (copy_ncid, trim(name), lendd, i)
     call error_handler(iret, failure="Problem defining dimension for copy_ncid")
!KWM     else
!KWM        iret = nf_def_dim (copy_ncid, trim(name), NF_UNLIMITED, i)
!KWM        call error_handler(iret)
!KWM     endif

  enddo

  ! Copy the global attributes
  do i = 1, ngatts

     iret = nf90_inq_attname(wrf_ncid, NF90_GLOBAL, i, name)
     call error_handler(iret, failure="Problem inquiring attname for wrf_ncid")

     iret = nf90_copy_att(wrf_ncid, NF90_GLOBAL, trim(name), copy_ncid, NF90_GLOBAL)
     call error_handler(iret, failure="Problem copying attribute")

  enddo

  ! Copy the variable definitions

  do i = 1, nvars

     iret = nf90_inquire_variable(wrf_ncid, i, name, xtype, nvdims, dimids, natts)
     call error_handler(iret, failure="Problem inquiring variable for wrf_ncid")

     iret = nf90_def_var(copy_ncid, trim(name), xtype, dimids(1:nvdims), i)
     call error_handler(iret, failure="Problem defining variable.")

     ! Copy the variable's attributes
     do j = 1, natts

        iret = nf90_inq_attname(wrf_ncid, i, j, name)
        call error_handler(iret, failure="Problem inquiring attribute name")

        iret = nf90_copy_att(wrf_ncid, i, trim(name), copy_ncid, i)
        call error_handler(iret, failure="Problem copying attribute")

     enddo

  enddo

  ! Take the new netcdf dataset out of define mode, so it's ready to receive data.
  iret = nf90_enddef(copy_ncid)
  call error_handler(iret, failure="Problem getting us out of define mode")

  ! Copy all the data 

  do i = 1, nvars

     iret = nf90_inquire_variable(wrf_ncid, i, name, xtype, nvdims, dimids)
     call error_handler(iret, failure="Problem inquiring variable.")
     print*, 'name = ', trim(name), nvdims
     if (nvdims > 4) stop "nvdims problem"

     vstart = 1
     vcount = 1

     do j = 1, nvdims
        iret = nf90_inquire_dimension(wrf_ncid, dimids(j), name, vcount(j))
        call error_handler(iret, failure="Problem inquiring dimension")
     enddo

     if (xtype == NF90_FLOAT) then

        allocate(xdum(vcount(1),vcount(2),vcount(3),vcount(4)))
        iret = nf90_get_var(wrf_ncid, i, xdum, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem getting float variable")

        iret = nf90_put_var(copy_ncid, i, xdum, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem putting float variable")
        deallocate(xdum)


     elseif (xtype == NF90_CHAR) then

        allocate(char_array(vcount(1),vcount(2),vcount(3),vcount(4)))
        iret = nf90_get_var(wrf_ncid, i, char_array, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem getting char variable")

        iret = nf90_put_var(copy_ncid, i, char_array, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem putting char variable")
        deallocate(char_array)

     elseif (xtype == NF90_INT) then

        allocate(idum(vcount(1),vcount(2),vcount(3),vcount(4)))
        iret = nf90_get_var(wrf_ncid, i, idum, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem getting int variable")

        iret = nf90_put_var(copy_ncid, i, idum, start=vstart, count=vcount)
        call error_handler(iret, failure="Problem putting int variable")
        deallocate(idum)

     else

        stop "xtype"

     endif

  enddo

  iret = nf90_close(copy_ncid)
  call error_handler(iret, failure="Problem closing copy_ncid.")

end subroutine copy_dataset

