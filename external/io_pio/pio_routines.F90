!---------------------------------------------------------------------------
!
! WRF Parallel I/O
! Author:  Wei Huang huangwei@ucar.edu
! Date:    June 01, 2014
!
!---------------------------------------------------------------------------
!$Id$
!---------------------------------------------------------------------------

module pio_routines

  use pio_kinds
  use pio

  use module_domain

  use wrf_data_pio

  implicit none

  include 'mpif.h'

  integer(i4) :: nprocs, myrank
  integer :: ids, ide, jds, jde, kds, kde, &
             ims, ime, jms, jme, kms, kme, &
             its, ite, jts, jte, kts, kte

CONTAINS

subroutine allocHandle(DataHandle,DH,Comm,Status)
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(out) :: DataHandle
  type(wrf_data_handle),pointer     :: DH
  integer              ,intent(IN)  :: Comm
  integer              ,intent(out) :: Status
  integer                           :: i, n
  integer                           :: stat

  do i=1,WrfDataHandleMax
    if(WrfDataHandles(i)%Free) then
      DH => WrfDataHandles(i)
      DataHandle = i
      do n = 1, MaxVars
         DH%vartype(n) = NOT_LAND_SOIL_VAR
      end do
      exit
    endif
    if(i==WrfDataHandleMax) then
      Status = WRF_WARN_TOO_MANY_FILES
      write(msg,*) 'Warning TOO MANY FILES in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      write(msg,*) 'Did you call ext_pio_ioinit?'
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  enddo
  DH%Free      =.false.
  DH%Comm      = Comm
  DH%Write     =.false.
  DH%first_operation  = .TRUE.
  DH%Collective = .TRUE.
  DH%CurrentVariable = 0
  Status = WRF_NO_ERR
 !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
 !write(unit=0, fmt='(a,i6)') 'Status = ', Status
end subroutine allocHandle

subroutine deallocHandle(DataHandle, Status)
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in) :: DataHandle
  integer              ,intent(out) :: Status
  type(wrf_data_handle),pointer     :: DH
  integer                           :: i
  integer                           :: stat

  IF ( DataHandle .GE. 1 .AND. DataHandle .LE. WrfDataHandleMax ) THEN
    if(.NOT. WrfDataHandles(DataHandle)%Free) then
      DH => WrfDataHandles(DataHandle)
      DH%Free      =.TRUE.
    endif

    deallocate(DH%iosystem)
  ENDIF
  Status = WRF_NO_ERR
end subroutine deallocHandle

subroutine GetDH(DataHandle,DH,Status)
  implicit none
  include 'wrf_status_codes.h'
  integer               ,intent(in)     :: DataHandle
  type(wrf_data_handle) ,pointer        :: DH
  integer               ,intent(out)    :: Status

  if(DataHandle < 1 .or. DataHandle > WrfDataHandleMax) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  DH => WrfDataHandles(DataHandle)
  if(DH%Free) then
    Status = WRF_WARN_BAD_DATA_HANDLE
    return
  endif
  Status = WRF_NO_ERR
  return
end subroutine GetDH

subroutine DateCheck(Date,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)      :: Date
  integer       ,intent(out)     :: Status
  
  if(len(Date) /= DateStrLen) then
    Status = WRF_WARN_DATESTR_BAD_LENGTH
  else  
    Status = WRF_NO_ERR
  endif
  return
end subroutine DateCheck

subroutine GetName(Element,Var,Name,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)     :: Element
  character*(*) ,intent(in)     :: Var
  character*(*) ,intent(out)    :: Name
  integer       ,intent(out)    :: Status
  character (VarNameLen)        :: VarName
  character (1)                 :: c
  integer                       :: i
  integer, parameter            ::  upper_to_lower =IACHAR('a')-IACHAR('A')

  VarName = Var
  Name = 'MD___'//trim(Element)//VarName
  do i=1,len(Name)
    c=Name(i:i)
    if('A'<=c .and. c <='Z') Name(i:i)=achar(iachar(c)+upper_to_lower)
    if(c=='-'.or.c==':') Name(i:i)='_'
  enddo
  Status = WRF_NO_ERR
  return
end subroutine GetName

subroutine GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  implicit none
  include 'wrf_status_codes.h'
  character (*)         ,intent(in)     :: IO
  integer               ,intent(in)     :: DataHandle
  character*(*)         ,intent(in)     :: DateStr
  integer               ,intent(out)    :: TimeIndex
  integer               ,intent(out)    :: Status
  type(wrf_data_handle) ,pointer        :: DH
  integer(KIND=PIO_OFFSET)              :: VStart(2)
  integer(KIND=PIO_OFFSET)              :: VCount(2)
  integer                               :: stat
  integer                               :: i

  DH => WrfDataHandles(DataHandle)
  call DateCheck(DateStr,Status)
  if(Status /= WRF_NO_ERR) then
    Status =  WRF_WARN_DATESTR_ERROR
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  if(IO == 'write') then
    TimeIndex = DH%TimeIndex
    if(TimeIndex <= 0) then
      TimeIndex = 1
    elseif(DateStr == DH%Times(TimeIndex)) then
      Status = WRF_NO_ERR
      return
    else
      TimeIndex = TimeIndex +1
      if(TimeIndex > MaxTimes) then
        Status = WRF_WARN_TIME_EOF
        write(msg,*) 'Warning TIME EOF in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    endif
    DH%TimeIndex        = TimeIndex
    DH%Times(TimeIndex) = DateStr
    VStart(1) = 1
    VStart(2) = TimeIndex
    VCount(1) = DateStrLen
    VCount(2) = 1
    stat = pio_put_var(DH%file_handle, DH%TimesVarID, DateStr)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
  else
    do i=1,MaxTimes
      if(DH%Times(i)==DateStr) then
        Status = WRF_NO_ERR
        TimeIndex = i
        exit
      endif
      if(i==MaxTimes) then
        Status = WRF_WARN_TIME_NF
        write(msg,*) 'Warning TIME ',DateStr,' NOT FOUND in ',__FILE__,', line', __LINE__ 
        call wrf_debug ( WARN , TRIM(msg))
        return
      endif
    enddo
  endif
  return
end subroutine GetTimeIndex

subroutine GetDim(MemoryOrder,NDim,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)  :: MemoryOrder
  integer       ,intent(out) :: NDim
  integer       ,intent(out) :: Status
  character*3                :: MemOrd

  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)
    case ('xyz','xzy','yxz','yzx','zxy','zyx','xsz','xez','ysz','yez')
      NDim = 3
    case ('xy','yx','xs','xe','ys','ye')
      NDim = 2
    case ('z','c')
      NDim = 1
    case ('0')  ! NDim=0 for scalars.  TBH:  20060502
      NDim = 0
    case default
      print *, 'memory order = ',MemOrd,'  ',MemoryOrder
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine GetDim

subroutine GetIndices(NDim,Start,End,i1,i2,j1,j2,k1,k2)
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(in)  :: NDim
  integer ,dimension(*),intent(in)  :: Start,End
  integer              ,intent(out) :: i1,i2,j1,j2,k1,k2

  i1=1
  i2=1
  j1=1
  j2=1
  k1=1
  k2=1
  if(NDim == 0) return  ! NDim=0 for scalars.  TBH:  20060502
  i1 = Start(1)
  i2 = End  (1)
  if(NDim == 1) return
  j1 = Start(2)
  j2 = End  (2)
  if(NDim == 2) return
  k1 = Start(3)
  k2 = End  (3)
  return
end subroutine GetIndices

logical function ZeroLengthHorzDim(MemoryOrder,Vector,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*)              ,intent(in)    :: MemoryOrder
  integer,dimension(*)       ,intent(in)    :: Vector
  integer                    ,intent(out)   :: Status
  integer                                   :: NDim
  integer,dimension(NVarDims)               :: temp
  character*3                               :: MemOrd
  logical zero_length

  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  zero_length = .false.
  select case (MemOrd)
    case ('xsz','xez','ysz','yez','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy','yzx')
      zero_length = temp(1) .lt. 1 .or. temp(3) .lt. 1
    case ('xy','yx','xyz','yxz')
      zero_length = temp(1) .lt. 1 .or. temp(2) .lt. 1
    case ('zxy','zyx')
      zero_length = temp(2) .lt. 1 .or. temp(3) .lt. 1
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      ZeroLengthHorzDim = .true.
      return
  end select
  Status = WRF_NO_ERR
  ZeroLengthHorzDim = zero_length
  return
end function ZeroLengthHorzDim

subroutine ExtOrder(MemoryOrder,Vector,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*)              ,intent(in)    :: MemoryOrder
  integer,dimension(*)       ,intent(inout) :: Vector
  integer                    ,intent(out)   :: Status
  integer                                   :: NDim
  integer,dimension(NVarDims)               :: temp
  character*3                               :: MemOrd

  call GetDim(MemoryOrder,NDim,Status)
  temp(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)

    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy')
      Vector(2) = temp(3)
      Vector(3) = temp(2)
    case ('yxz')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case ('yzx')
      Vector(1) = temp(3)
      Vector(2) = temp(1)
      Vector(3) = temp(2)
    case ('zxy')
      Vector(1) = temp(2)
      Vector(2) = temp(3)
      Vector(3) = temp(1)
    case ('zyx')
      Vector(1) = temp(3)
      Vector(3) = temp(1)
    case ('yx')
      Vector(1) = temp(2)
      Vector(2) = temp(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrder

subroutine ExtOrderStr(MemoryOrder,Vector,ROVector,Status)
  implicit none
  include 'wrf_status_codes.h'
  character*(*)                    ,intent(in)    :: MemoryOrder
  character*(*),dimension(*)       ,intent(in)    :: Vector
  character(80),dimension(NVarDims),intent(out)   :: ROVector
  integer                          ,intent(out)   :: Status
  integer                                         :: NDim
  character*3                                     :: MemOrd

  call GetDim(MemoryOrder,NDim,Status)
  ROVector(1:NDim) = Vector(1:NDim)
  call LowerCase(MemoryOrder,MemOrd)
  select case (MemOrd)

    case ('xyz','xsz','xez','ysz','yez','xy','xs','xe','ys','ye','z','c')
      continue
    case ('0')
      continue  ! NDim=0 for scalars.  TBH:  20060502
    case ('xzy')
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(2)
    case ('yxz')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case ('yzx')
      ROVector(1) = Vector(3)
      ROVector(2) = Vector(1)
      ROVector(3) = Vector(2)
    case ('zxy')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(3)
      ROVector(3) = Vector(1)
    case ('zyx')
      ROVector(1) = Vector(3)
      ROVector(3) = Vector(1)
    case ('yx')
      ROVector(1) = Vector(2)
      ROVector(2) = Vector(1)
    case default
      Status = WRF_WARN_BAD_MEMORYORDER
      return
  end select
  Status = WRF_NO_ERR
  return
end subroutine ExtOrderStr


subroutine LowerCase(MemoryOrder,MemOrd)
  implicit none
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)  :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1                :: c
  integer       ,parameter   :: upper_to_lower =IACHAR('a')-IACHAR('A')
  integer                    :: i,N

  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('A'<=c .and. c <='Z') MemOrd(i:i)=achar(iachar(c)+upper_to_lower)
  enddo
  return
end subroutine LowerCase

subroutine UpperCase(MemoryOrder,MemOrd)
  implicit none
  include 'wrf_status_codes.h'
  character*(*) ,intent(in)  :: MemoryOrder
  character*(*) ,intent(out) :: MemOrd
  character*1                :: c
  integer     ,parameter     :: lower_to_upper =IACHAR('A')-IACHAR('a')
  integer                    :: i,N

  MemOrd = ' '
  N = len(MemoryOrder)
  MemOrd(1:N) = MemoryOrder(1:N)
  do i=1,N
    c = MemoryOrder(i:i)
    if('a'<=c .and. c <='z') MemOrd(i:i)=achar(iachar(c)+lower_to_upper)
  enddo
  return
end subroutine UpperCase

subroutine netcdf_err(err,Status)
  implicit none
  include 'wrf_status_codes.h'
  integer  ,intent(in)  :: err
  integer  ,intent(out) :: Status
  character(len=80)     :: errmsg
  integer               :: stat

  if(err == PIO_NOERR)then
    Status = WRF_NO_ERR
  else
   !errmsg = NFMPI_STRERROR(err) 
   !write(msg,*) 'NetCDF error: ',errmsg
    write(msg,*) 'NetCDF error: ', 'from PIO'
    call wrf_debug ( WARN , TRIM(msg))
    Status = WRF_WARN_NETCDF
  endif
  return
end subroutine netcdf_err

subroutine find_iodesc(DH,MemoryOrder,Stagger,FieldTYpe,whole)
  implicit none
  type(wrf_data_handle), pointer   :: DH
  character*(*),     intent(in)    :: MemoryOrder
  character*(*),     intent(in)    :: Stagger
  integer,           intent(in)    :: FieldType
  logical,           intent(out)   :: whole
  character*3                      :: MemOrd
  character*1                      :: Stag
  integer           ,parameter     :: MaxUpperCase=IACHAR('Z')

  whole = .false.

  call LowerCase(MemoryOrder,MemOrd)
  call LowerCase(Stagger,Stag)

  select case (MemOrd)
    case ('xs','xe','ys','ye','z','c')
      whole = .true.
    case ('ysz','yez','xsz', 'xez')
      write(msg,*) 'PIO DOES NOT support memord: <', MemOrd, '>, in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
    case ('xzy')
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_v_real
            case ('z')
                 if(LAND_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_land_real
                   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                   !write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                   !write(unit=0, fmt='(a)') 'Select DH%iodesc3d_land_real'
                 else if(SOIL_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_real
                   !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                   !write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                   !write(unit=0, fmt='(a)') 'Select DH%iodesc3d_soil_real'
                 else if(SOIL_LAYERS_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_layers_real
                    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                    write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                    write(unit=0, fmt='(a)') 'Select DH%iodesc3d_soil_layers_real'
                 else if(MDL_CPL_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_mdl_cpl_real
                    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                    write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                    write(unit=0, fmt='(a)') 'Select DH%iodesc3d_mdl_cpl_real'
                 else
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_w_real
                 endif
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_real
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_v_double
            case ('z')
                 if(LAND_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_land_double
                 else if(SOIL_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_double
                 else if(SOIL_LAYERS_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_layers_double
                 else if(MDL_CPL_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_mdl_cpl_double
                 else
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_w_double
                 endif
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_v_int
            case ('z')
                 if(LAND_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_land_int
                 else if(SOIL_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_int
                 else if(SOIL_LAYERS_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_layers_int
                 else if(MDL_CPL_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_mdl_cpl_int
                 else
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_w_int
                 endif
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_int
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_v_int
            case ('z')
                 if(LAND_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_land_int
                 else if(SOIL_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_int
                 else if(SOIL_LAYERS_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_layers_int
                 else if(MDL_CPL_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_mdl_cpl_int
                 else
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_w_int
                 endif
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('xy')
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_real
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_real
                !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                !write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                !write(unit=0, fmt='(a)') 'Select DH%iodesc2d_m_real'
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_double
                 write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                 write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                 write(unit=0, fmt='(a)') 'Select DH%iodesc2d_m_double'
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_int
                 write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
                 write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
                 write(unit=0, fmt='(a)') 'Select DH%iodesc2d_m_int'
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case default
      whole = .true.
#if 0
      select case (FieldType)
        case (WRF_REAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc1d_real
        case (WRF_DOUBLE)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc1d_double 
        case (WRF_INTEGER)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc1d_int
        case (WRF_LOGICAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc1d_int
        case default
             write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
             call wrf_debug ( WARN , TRIM(msg))
             return
      end select
#endif
  end select
end subroutine find_iodesc

subroutine FieldIO(IO,DataHandle,DateStr,Starts,Length,MemoryOrder, &
                   Stagger,FieldType,Field,Status)
  implicit none
  include 'wrf_status_codes.h'
  character (*)              ,intent(in)    :: IO
  integer                    ,intent(in)    :: DataHandle
  character*(*)              ,intent(in)    :: DateStr
  integer,dimension(NVarDims),intent(in)    :: Starts
  integer,dimension(NVarDims),intent(in)    :: Length
  character*(*)              ,intent(in)    :: MemoryOrder
  character*(*)              ,intent(in)    :: Stagger
  integer                    ,intent(in)    :: FieldType
  integer,dimension(*)       ,intent(inout) :: Field
  integer                    ,intent(out)   :: Status
  integer                                   :: TimeIndex
  logical                                   :: whole
  integer                                   :: NDim
  integer                                   :: fldsize
  integer                                   :: n
  integer,dimension(NVarDims)               :: VStart
  integer,dimension(NVarDims)               :: VCount
  type(wrf_data_handle)      ,pointer       :: DH

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(3a)') 'MemoryOrder: <', trim(MemoryOrder), '>'
  write(unit=0, fmt='(3a)') 'Stagger: <', trim(Stagger), '>'

  DH => WrfDataHandles(DataHandle)
  call GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    write(msg,*) '  Bad time index for DateStr = ',DateStr
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif
  call GetDim(MemoryOrder,NDim,Status)
  VStart(:) = 1
  VCount(:) = 1
  VStart(1:NDim) = Starts(1:NDim)
  VCount(1:NDim) = Length(1:NDim)
  VStart(NDim+1) = TimeIndex
  VCount(NDim+1) = 1

  fldsize = 1
  do n = 1, NDim + 1
     write(unit=0, fmt='(2(a,i2,a,i4))') &
          'VStart(', n, ')=', VStart(n), ', VCount(', n, ')=', VCount(n)
     fldsize = fldsize * VCount(n)
  end do
     
  call find_iodesc(DH,MemoryOrder,Stagger,FieldTYpe,whole)

  write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  write(unit=0, fmt='(a,i6)') 'DH%CurrentVariable = ', DH%CurrentVariable
  write(unit=0, fmt='(a,i6)') 'fldsize =', fldsize
  write(unit=0, fmt='(a, i8)') 'FieldType: ', FieldType, 'WRF_REAL: ', WRF_REAL
  write(unit=0, fmt='(a, i8)') 'WRF_INTEGER: ', WRF_INTEGER, 'WRF_DOUBLE: ', WRF_DOUBLE
  write(unit=0, fmt='(a, l8)') 'whole: ', whole

  select case (FieldType)
    case (WRF_REAL)
      call ext_pio_RealFieldIO(whole,IO,DH,fldsize,Field,Status)
    case (WRF_DOUBLE)
      call ext_pio_DoubleFieldIO(whole,IO,DH,fldsize,Field,Status)
    case (WRF_INTEGER)
      call ext_pio_IntFieldIO(whole,IO,DH,fldsize,Field,Status)
    case (WRF_LOGICAL)
      call ext_pio_LogicalFieldIO(whole,IO,DH,fldsize,Field,Status)
    case default
      Status = WRF_WARN_DATA_TYPE_NOT_FOUND
      write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
  end select

  return
end subroutine FieldIO

! Returns .TRUE. iff it is OK to write time-independent domain metadata to the 
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is 
! returned.  
LOGICAL FUNCTION ncd_ok_to_put_dom_ti( DataHandle )
    implicit none
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, first_output, retval
    call ext_pio_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      first_output = ncd_is_first_operation( DataHandle )
!      retval = .NOT. dryrun .AND. first_output
      retval = dryrun
    ENDIF
    ncd_ok_to_put_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_put_dom_ti

! Returns .TRUE. iff it is OK to read time-independent domain metadata from the 
! file referenced by DataHandle.  If DataHandle is invalid, .FALSE. is 
! returned.  
LOGICAL FUNCTION ncd_ok_to_get_dom_ti( DataHandle )
    implicit none
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    CHARACTER*80 :: fname
    INTEGER :: filestate
    INTEGER :: Status
    LOGICAL :: dryrun, retval
    call ext_pio_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun       = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      retval = .NOT. dryrun
    ENDIF
    ncd_ok_to_get_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_get_dom_ti

! Returns .TRUE. iff nothing has been read from or written to the file 
! referenced by DataHandle.  If DataHandle is invalid, .FALSE. is returned.  
LOGICAL FUNCTION ncd_is_first_operation( DataHandle )
    implicit none
    include 'wrf_status_codes.h'
    INTEGER, INTENT(IN) :: DataHandle 
    TYPE(wrf_data_handle) ,POINTER :: DH
    INTEGER :: Status
    LOGICAL :: retval
    CALL GetDH( DataHandle, DH, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      retval = DH%first_operation
    ENDIF
    ncd_is_first_operation = retval
    RETURN
END FUNCTION ncd_is_first_operation

subroutine initialize_pio(grid, DH)
   implicit none

   type(domain)                   :: grid
   type(wrf_data_handle), pointer :: DH

   integer     :: ierr
   integer(i4) :: communicator, pioprocs, piostart, piostride, pioshift

   communicator = grid%communicator
   allocate(DH%iosystem)

  !call pio_setdebuglevel(1)

   call mpi_comm_size(communicator, nprocs, ierr)
   call mpi_comm_rank(communicator, myrank, ierr)
  !call mpi_comm_size(MPI_COMM_WORLD, nprocs, ierr)
  !call mpi_comm_rank(MPI_COMM_WORLD, myrank, ierr)
  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a,i6)') 'nprocs = ', nprocs, &
  !                            'myrank = ', myrank

   if(grid%pioprocs > nprocs) then
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'nprocs = ', nprocs, &
                                  'grid%pioprocs = ', grid%pioprocs
     !Force pioprocs to be nprocs.
      pioprocs = nprocs
   else if(grid%pioprocs < 1) then
     !Force pioprocs to be 1.
      pioprocs = 1
   else
      pioprocs = grid%pioprocs
   endif

   piostride = nprocs / grid%pioprocs

   if((grid%pioprocs * piostride) < nprocs) then
     !We expect that: nprocs = piostride * grid%pioprocs
      piostride = piostride + 1
   endif

   if(piostride /= grid%piostride) then
     !We expect that user's piostride equals what we calculated here.
     !If not, override it.
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'Calculated piostride = ', piostride, &
                                  'User provided piostride = ', grid%piostride
   endif

   if(grid%pioshift < 0) then
     !pioshift can from 0, but can not less than 0, usually, we 
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'User provided a pioshift of: ', grid%pioshift
      if(grid%piostride > 1) then
         pioshift = 1
      else
         pioshift = 0
      endif
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'PIO has forced pioshift to: ', pioshift
   else if(grid%pioshift >= grid%piostride) then
     !pioshift can not large then piostride
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'User provided a pioshift of: ', grid%pioshift
      if(grid%piostride > 1) then
         pioshift = 1
      else
         pioshift = 0
      endif
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'PIO has forced pioshift to: ', pioshift
   else
      pioshift = grid%pioshift
   endif

   if(grid%piostart < 0) then
     !Force piostart from 0
      write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
      write(unit=0, fmt='(a,i6)') 'User provided a piostart of: ', grid%piostart
      write(unit=0, fmt='(a,i6)') 'PIO has forced piosstart to: ', 0
      piostart = 0
   else
      piostart = grid%piostart
   endif

   write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
   write(unit=0, fmt='(2(a,i6))') 'nprocs = ', nprocs, ', myrank = ', myrank
   write(unit=0, fmt='(4(a,i6))') 'pioprocs = ', pioprocs, &
                                ', piostride = ', piostride, &
                                ', piostart = ', piostart, &
                                ', pioshift = ', pioshift

  !call PIO_init to initiate iosystem
  !call PIO_init(my_rank, MPI_COMM_WORLD, 4, 0, 4, PIO_rearr_box, iosystem, 1)
  !call PIO_init(myrank, MPI_COMM_WORLD, pioprocs, &
  
   call PIO_init(myrank, communicator, pioprocs, &
                 piostart, piostride, &
                 PIO_rearr_box, DH%iosystem, pioshift)

   DH%nprocs = nprocs
   DH%myrank = myrank

   DH%piostart = piostart
   DH%pioshift = pioshift
   DH%pioprocs = pioprocs
   DH%piostride = piostride

   call get_ijk_from_grid(grid,                         &
                          ids, ide, jds, jde, kds, kde, &
                          ims, ime, jms, jme, kms, kme, &
                          its, ite, jts, jte, kts, kte)

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(3(a,i6))') 'ids = ', ids, ', jds = ', jds, ', kds = ', kds
  !write(unit=0, fmt='(3(a,i6))') 'ide = ', ide, ', jde = ', jde, ', kde = ', kde
  !write(unit=0, fmt='(3(a,i6))') 'ims = ', ims, ', jms = ', jms, ', kms = ', kms
  !write(unit=0, fmt='(3(a,i6))') 'ime = ', ime, ', jme = ', jme, ', kme = ', kme
  !write(unit=0, fmt='(3(a,i6))') 'its = ', its, ', jts = ', jts, ', kts = ', kts
  !write(unit=0, fmt='(3(a,i6))') 'ite = ', ite, ', jte = ', jte, ', kte = ', kte

end subroutine initialize_pio

subroutine define_pio_iodesc(grid, DH)
   implicit none

   type(domain)                   :: grid
   type(wrf_data_handle), pointer :: DH

   integer(i4) :: communicator, myrank
   integer(i4) :: iostat

   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1) * (kme - kms + 1)) &
           :: compdof_3d
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1) * grid%num_land_cat) &
           :: compdof_3d_land
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1) * grid%num_soil_cat) &
           :: compdof_3d_soil
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1) * grid%num_soil_layers) &
           :: compdof_3d_soil_layers
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1) * grid%num_ext_model_couple_dom) &
           :: compdof_3d_mdl_cpl
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1)) &
           :: compdof_2d
   integer :: dims3d(4), dims2d(3), dims1d(2), dims0d(1)
   integer :: dims3d_land(4), dims3d_soil(4), dims3d_soil_layers(4)
   integer :: dims3d_mdl_cpl(4)
   integer :: lite, ljte, lkte
   integer :: i, j, k, npos

   communicator = grid%communicator
   myrank = DH%myrank

!--For MASS variables
   dims3d(1) = ide - 1
   dims3d(2) = jde - 1
   dims3d(3) = kde - 1
   dims3d(4) = 1

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

   dims3d_land(1) = dims3d(1)
   dims3d_land(2) = dims3d(2)
   dims3d_land(3) = grid%num_land_cat
   dims3d_land(4) = dims3d(4)

   dims3d_soil(1) = dims3d(1)
   dims3d_soil(2) = dims3d(2)
   dims3d_soil(3) = grid%num_soil_cat
   dims3d_soil(4) = dims3d(4)

   dims3d_soil_layers(1) = dims3d(1)
   dims3d_soil_layers(2) = dims3d(2)
   dims3d_soil_layers(3) = grid%num_soil_layers
   dims3d_soil_layers(4) = dims3d(4)

   dims3d_mdl_cpl(1) = dims3d(1)
   dims3d_mdl_cpl(2) = dims3d(2)
   dims3d_mdl_cpl(3) = grid%num_ext_model_couple_dom
   dims3d_mdl_cpl(4) = dims3d(4)

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)
   dims2d(3) = dims3d(4)

   dims1d(1) = dims3d(3)
   dims1d(2) = dims3d(4)

   dims0d(1) = dims3d(4)

   write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
   write(unit=0, fmt='(a, 6i6)') 'dims1d = ', dims1d
   write(unit=0, fmt='(a, 6i6)') 'dims2d = ', dims2d
   write(unit=0, fmt='(a, 6i6)') 'dims3d = ', dims3d
   write(unit=0, fmt='(a, 6i6)') 'dims3d_land = ', dims3d_land
   write(unit=0, fmt='(a, 6i6)') 'dims3d_soil = ', dims3d_soil
   write(unit=0, fmt='(a, 6i6)') 'grid%num_land_cat = ', grid%num_land_cat
   write(unit=0, fmt='(a, 6i6)') 'grid%num_soil_cat = ', grid%num_soil_cat
   write(unit=0, fmt='(a, 6i6)') 'grid%num_soil_layers = ', grid%num_soil_layers
   write(unit=0, fmt='(a, 6i6)') 'grid%num_ext_model_couple_dom = ', grid%num_ext_model_couple_dom

   do j = jms, jme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = 0
      enddo

      do k = kms, kme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = 0
      enddo
      enddo

      do k = 1, dims3d_land(3)
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_land(3) * (j - jms))
         compdof_3d_land(npos) = 0
      enddo
      enddo

      do k = 1, dims3d_soil(3)
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil(3) * (j - jms))
         compdof_3d_soil(npos) = 0
      enddo
      enddo

      do k = 1, dims3d_soil_layers(3)
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil_layers(3) * (j - jms))
         compdof_3d_soil_layers(npos) = 0
      enddo
      enddo

      do k = 1, dims3d_mdl_cpl(3)
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_mdl_cpl(3) * (j - jms))
         compdof_3d_mdl_cpl(npos) = 0
      enddo
      enddo
   enddo

   do j = jts, ljte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = i + dims2d(1) * (j - 1)
      end do

      do k = kts, lkte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = i + dims3d(1) * (k - 1 + dims3d(3) * (j - 1))
      enddo
      enddo

      do k = 1, dims3d_land(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_land(3) * (j - jms))
         compdof_3d_land(npos) = i + dims3d_land(1) * (k - 1 + dims3d_land(3) * (j - 1))
      enddo
      enddo

      do k = 1, dims3d_soil(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil(3) * (j - jms))
         compdof_3d_soil(npos) = i + dims3d_soil(1) * (k - 1 + dims3d_soil(3) * (j - 1))
      enddo
      enddo

      do k = 1, dims3d_soil_layers(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil_layers(3) * (j - jms))
         compdof_3d_soil_layers(npos) = i + dims3d_soil_layers(1) * (k - 1 + dims3d_soil_layers(3) * (j - 1))
      enddo
      enddo

      do k = 1, dims3d_mdl_cpl(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_mdl_cpl(3) * (j - jms))
         compdof_3d_mdl_cpl(npos) = i + dims3d_mdl_cpl(1) * (k - 1 + dims3d_mdl_cpl(3) * (j - 1))
      enddo
      enddo
   enddo

!--call init_decomp in order to setup the IO decomposition with PIO
   call pio_setdebuglevel(1)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_land, compdof_3d_land, DH%iodesc3d_land_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_land, compdof_3d_land, DH%iodesc3d_land_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_land, compdof_3d_land, DH%iodesc3d_land_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_soil, compdof_3d_soil, DH%iodesc3d_soil_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_soil, compdof_3d_soil, DH%iodesc3d_soil_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_soil, compdof_3d_soil, DH%iodesc3d_soil_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_soil_layers, compdof_3d_soil_layers, DH%iodesc3d_soil_layers_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_soil_layers, compdof_3d_soil_layers, DH%iodesc3d_soil_layers_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_soil_layers, compdof_3d_soil_layers, DH%iodesc3d_soil_layers_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_mdl_cpl, compdof_3d_mdl_cpl, DH%iodesc3d_mdl_cpl_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_mdl_cpl, compdof_3d_mdl_cpl, DH%iodesc3d_mdl_cpl_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_mdl_cpl, compdof_3d_mdl_cpl, DH%iodesc3d_mdl_cpl_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_m_double)

!--For X-STAG variables
   dims3d(1) = ide
   dims3d(2) = jde - 1
   dims3d(3) = kde - 1

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)

   dims1d(1) = dims3d(3)

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a, 6i6)') 'dims3d = ', dims3d
  !write(unit=0, fmt='(a, 6i6)') 'dims2d = ', dims2d
  !write(unit=0, fmt='(a, 6i6)') 'dims1d = ', dims1d

  !compdof_3d =  0
  !compdof_2d =  0

   do j = jms, jme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = 0
      enddo

      do k = kms, kme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = 0
      enddo
      enddo
   enddo

   do j = jts, ljte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = i + dims2d(1) * (j - 1)
      end do

      do k = kts, lkte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = i + dims3d(1) * (k - 1 + dims3d(3) * (j - 1))
      enddo
      enddo
   enddo

!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_u_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_u_int)

   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_u_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_u_int)

!--For Y-STAG variables
   dims3d(1) = ide - 1
   dims3d(2) = jde
   dims3d(3) = kde - 1

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)

   dims1d(1) = dims3d(3)

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a, 6i6)') 'dims3d = ', dims3d
  !write(unit=0, fmt='(a, 6i6)') 'dims2d = ', dims2d
  !write(unit=0, fmt='(a, 6i6)') 'dims1d = ', dims1d

  !compdof_3d =  0
  !compdof_2d =  0

   do j = jms, jme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = 0
      enddo

      do k = kms, kme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = 0
      enddo
      enddo
   enddo

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a,i6)') 'npos = ', npos, &
  !                            '(ime - ims + 1) * (jme - jms + 1) * (kme - kms + 1) = ', &
  !                             (ime - ims + 1) * (jme - jms + 1) * (kme - kms + 1)

   do j = jts, ljte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms)
         compdof_2d(npos) = i + dims2d(1) * (j - 1)
      end do

      do k = kts, lkte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = i + dims3d(1) * (k - 1 + dims3d(3) * (j - 1))
      enddo
      enddo
   enddo


!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_v_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_v_int)

   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_v_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_v_int)

!--For Z-STAG variables
   dims3d(1) = ide - 1
   dims3d(2) = jde - 1
   dims3d(3) = kde

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)

   dims1d(1) = dims3d(3)

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a, 6i6)') 'dims3d = ', dims3d
  !write(unit=0, fmt='(a, 6i6)') 'dims2d = ', dims2d
  !write(unit=0, fmt='(a, 6i6)') 'dims1d = ', dims1d

  !compdof_3d =  0

   do j = jms, jme
      do k = kms, kme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = 0
      enddo
      enddo
   enddo

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a,i6)') 'npos = ', npos, &
  !                            '(ime - ims + 1) * (jme - jms + 1) * (kme - kms + 1) = ', &
  !                             (ime - ims + 1) * (jme - jms + 1) * (kme - kms + 1)

   do j = jts, ljte
   do k = kts, lkte
   do i = its, lite
      npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
      compdof_3d(npos) = i + dims3d(1) * (k - 1 + dims3d(3) * (j - 1))
   enddo
   enddo
   enddo


!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_w_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_w_int)

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a)') 'finished: define_pio_iodesc'

end subroutine define_pio_iodesc

subroutine reorder (MemoryOrder,MemO)
  implicit none
  include 'wrf_status_codes.h'
  character*(*)     ,intent(in)    :: MemoryOrder
  character*3       ,intent(out)   :: MemO
  character*3                      :: MemOrd
  integer                          :: N,i,i1,i2,i3

  MemO = MemoryOrder
  N = len_trim(MemoryOrder)
  if(N == 1) return
  call lowercase(MemoryOrder,MemOrd)
! never invert the boundary codes
  select case ( MemOrd )
     case ( 'xsz','xez','ysz','yez' )
       return
     case default
       continue
  end select
  i1 = 1
  i3 = 1
  do i=2,N
    if(ichar(MemOrd(i:i)) < ichar(MemOrd(i1:i1))) I1 = i
    if(ichar(MemOrd(i:i)) > ichar(MemOrd(i3:i3))) I3 = i
  enddo
  if(N == 2) then
    i2=i3
  else
    i2 = 6-i1-i3
  endif
  MemO(1:1) = MemoryOrder(i1:i1)
  MemO(2:2) = MemoryOrder(i2:i2)
  if(N == 3) MemO(3:3) = MemoryOrder(i3:i3)
  if(MemOrd(i1:i1) == 's' .or. MemOrd(i1:i1) == 'e') then
    MemO(1:N-1) = MemO(2:N)
    MemO(N:N  ) = MemoryOrder(i1:i1)
  endif
  return
end subroutine reorder

end module pio_routines

