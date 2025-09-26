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

subroutine allocHandle(DataHandle,DH,Status)
  implicit none
  include 'wrf_status_codes.h'
  integer              ,intent(out) :: DataHandle
  type(wrf_data_handle),pointer     :: DH
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
  DH%Write     =.false.
  DH%first_operation  = .TRUE.
  DH%CurrentVariable = 0
  Status = WRF_NO_ERR
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

   !deallocate(DH%iosystem)
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
  integer                               :: VStart(2)
  integer                               :: VCount(2)
  integer                               :: stat
  integer                               :: i
  character(len=DateStrLen)             :: tmpdatestr(1)

  if(len(Datestr) == DateStrLen) then
    tmpdatestr = DateStr
  else
    write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
    write(unit=0, fmt='(3a)') 'IO: <', trim(IO), '>'
    write(unit=0, fmt='(a,i3)') 'DataHandle = ', DataHandle
    write(unit=0, fmt='(3a)') 'DateStr: <', trim(DateStr), '>'
    write(unit=0, fmt='(a,i6,a,i6)') 'DateStrLen = ', DateStrLen, &
                                     ' did not equal len(DateStr): ', len(DateStr)
    Status =  WRF_WARN_DATESTR_ERROR
    write(msg,*) 'Warning DATE STRING ERROR in ',__FILE__,', line', __LINE__ 
    call wrf_debug ( WARN , TRIM(msg))
    return
  end if

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
      TimeIndex = TimeIndex + 1
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
   !write(unit=0, fmt='(3a,i6)') 'DateStr: <', trim(DateStr), '>, TimeIndex =', TimeIndex
    stat = pio_put_var(DH%file_handle, DH%vtime, VStart, VCount, tmpdatestr)
    call netcdf_err(stat,Status)
    if(Status /= WRF_NO_ERR) then
      write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__ 
      call wrf_debug ( WARN , TRIM(msg))
      return
    endif
   !call pio_advanceframe(DH%vtime)
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
                 else if(SOIL_CAT_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_real
                 else if(SOIL_LAYERS_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_soil_layers_real
                 else if(MDL_CPL_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_mdl_cpl_real
                 else if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_real
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
                 else if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_double
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
                 else if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_int
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
                 else if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
                    DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_int
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
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_v_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_m_int
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
    case ('xsz')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_v_real
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_w_real
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_m_real
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_v_double
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_w_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_m_int
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xsz_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('xez')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_v_real
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_w_real
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_m_real
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_v_double
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_w_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_m_int
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_xez_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('ysz')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_v_real
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_w_real
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_m_real
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_v_double
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_w_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_m_int
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ysz_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('yez')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_u_real
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_v_real
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_w_real
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_m_real
          end select
        case (WRF_DOUBLE)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_u_double
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_v_double
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_w_double
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_m_double
          end select
        case (WRF_INTEGER)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_m_int
          end select
        case (WRF_LOGICAL)
          select case (Stag)
            case ('x')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_u_int
            case ('y')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_v_int
            case ('z')
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_w_int
            case default
                 DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_yez_m_int
          end select
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('xs')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xs_m_real
        case (WRF_DOUBLE)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xs_m_double
        case (WRF_INTEGER)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xs_m_int
        case (WRF_LOGICAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xs_m_int
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('xe')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xe_m_real
        case (WRF_DOUBLE)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xe_m_double
        case (WRF_INTEGER)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xe_m_int
        case (WRF_LOGICAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_xe_m_int
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('ys')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ys_m_real
        case (WRF_DOUBLE)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ys_m_double
        case (WRF_INTEGER)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ys_m_int
        case (WRF_LOGICAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ys_m_int
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('ye')
      DH%vartype(DH%CurrentVariable) = BDY_VAR
      select case (FieldType)
        case (WRF_REAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ye_m_real
        case (WRF_DOUBLE)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ye_m_double
        case (WRF_INTEGER)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ye_m_int
        case (WRF_LOGICAL)
             DH%ioVar(DH%CurrentVariable) = DH%iodesc2d_ye_m_int
        case default
          write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
          call wrf_debug ( WARN , TRIM(msg))
          whole = .true.
          return
      end select
    case ('xyz')
      select case (Stag)
        case ('z')
             if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
                select case (FieldType)
                  case (WRF_REAL)
                       DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_real
                  case (WRF_DOUBLE)
                       DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_double
                  case (WRF_INTEGER)
                       DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_int
                  case (WRF_LOGICAL)
                       DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_ensemble_int
                  case default
                       write(msg,*) 'Warning DO NOT KNOW HOW TO HANDLE this FieldType in ',__FILE__,', line', __LINE__
                       call wrf_debug ( WARN , TRIM(msg))
                       DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_real
                end select
             else
                write(msg,*) 'Warning DO NOT KNOW HOW TO HANDLE THIS VAR KIND in ',__FILE__,', line', __LINE__
                call wrf_debug ( WARN , TRIM(msg))
                DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_w_double
             end if
        case default
             write(msg,*) 'Warning DO NOT KNOW HOW TO HANDLE THIS STAG in ',__FILE__,', line', __LINE__
             call wrf_debug ( WARN , TRIM(msg))
             DH%ioVar(DH%CurrentVariable) = DH%iodesc3d_m_real
      end select
   !case ('z','c')
   !  whole = .true.
    case default
      whole = .true.
      if(ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable)) then
         whole = .false.
      end if
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

logical function is_boundary(MemoryOrder)

  implicit none

  character*(*), intent(in) :: MemoryOrder

  logical     :: isbdy
  character*3 :: MemOrd

  isbdy = .false.

  call LowerCase(MemoryOrder,MemOrd)

  select case (MemOrd)
    case ('xsz', 'xez', 'ysz', 'yez')
      isbdy = .true.
    case ('xs', 'xe', 'ys', 'ye')
      isbdy = .true.
    case default
      isbdy = .false.
  end select

  is_boundary = isbdy
end function is_boundary

subroutine FieldIO(IO,DataHandle,DateStr,Dimens,Starts,Counts,Length,MemoryOrder, &
                   Stagger,FieldType,Field,Status)
  implicit none
  include 'wrf_status_codes.h'
  character (*)              ,intent(in)    :: IO
  integer                    ,intent(in)    :: DataHandle
  character*(*)              ,intent(in)    :: DateStr
  integer,dimension(NVarDims),intent(inout) :: Dimens
  integer,dimension(NVarDims),intent(inout) :: Starts
  integer,dimension(NVarDims),intent(inout) :: Counts
  integer,dimension(NVarDims),intent(in)    :: Length
  character*(*)              ,intent(in)    :: MemoryOrder
  character*(*)              ,intent(in)    :: Stagger
  integer                    ,intent(in)    :: FieldType
  integer,dimension(*)       ,intent(inout) :: Field
  integer                    ,intent(out)   :: Status
  integer                                   :: TimeIndex
  logical                                   :: whole, isbdy
  integer                                   :: NDim
  integer                                   :: fldsize, datasize
  integer                                   :: n
  type(wrf_data_handle)      ,pointer       :: DH
  integer(KIND=PIO_OFFSET)                  :: pioidx

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

  fldsize = 1
  datasize = 1
  do n = 1, NDim
     fldsize = fldsize * Length(n)
     datasize = datasize * Counts(n)
  end do

  Starts(NDim+1) = TimeIndex
  Counts(NDim+1) = 1

  call find_iodesc(DH,MemoryOrder,Stagger,FieldTYpe,whole)
  isbdy = is_boundary(MemoryOrder)
 !isbdy = BDY_VAR == DH%vartype(DH%CurrentVariable)

  pioidx = TimeIndex
  call pio_setframe(DH%descVar(DH%CurrentVariable), pioidx)
 !DH%descVar(DH%CurrentVariable)%rec = TimeIndex

 !write(unit=0, fmt='(3a,i6)') 'File: ', __FILE__, ', line: ', __LINE__
 !write(unit=0, fmt='(3a,l8)') 'IO = ', trim(IO), ', whole = ', whole
 !write(unit=0, fmt='(4a)') 'MemoryOrder = ', trim(MemoryOrder), ', Stagger = ', trim(Stagger)
 !write(unit=0, fmt='(a,i4,a,i3)') 'DH%vartype(', DH%CurrentVariable, ') = ', DH%vartype(DH%CurrentVariable)

 !if(whole .and. (ENSEMBLE_VAR == DH%vartype(DH%CurrentVariable))) then
 !   whole = .false.
 !end if

  select case (FieldType)
    case (WRF_REAL)
      if(isbdy .and. (IO == 'read')) then
        Dimens(NDim+1) = TimeIndex
        call read_bdy_RealFieldIO(DH,NDim,Dimens,Starts,Counts,Field,Status)
      else
        call ext_pio_RealFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Field,Status)
      endif
    case (WRF_DOUBLE)
      if(isbdy .and. (IO == 'read')) then
        Dimens(NDim+1) = TimeIndex
        call read_bdy_DoubleFieldIO(DH,NDim,Dimens,Starts,Counts,Field,Status)
      else
        call ext_pio_DoubleFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Field,Status)
      endif
    case (WRF_INTEGER)
      call ext_pio_IntFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Field,Status)
    case (WRF_LOGICAL)
      call ext_pio_LogicalFieldIO(whole,IO,DH,Starts,Counts,fldsize,datasize,Field,Status)
    case default
      Status = WRF_WARN_DATA_TYPE_NOT_FOUND
      write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg))
      return
  end select

  return
end subroutine FieldIO

subroutine FieldBDY(IO,DataHandle,DateStr,NDim,Domains, &
                    MemoryStart,MemoryEnd,PatchStart,PatchEnd, &
                    FieldType,Field,Status)
  implicit none
  include 'wrf_status_codes.h'
  character (*)              ,intent(in)    :: IO
  integer                    ,intent(in)    :: DataHandle,NDim
  character*(*)              ,intent(in)    :: DateStr
  integer,dimension(*)       ,intent(inout) :: Domains
  integer,dimension(*)       ,intent(in)    :: MemoryStart, MemoryEnd
  integer,dimension(*)       ,intent(in)    :: PatchStart,  PatchEnd
  integer                    ,intent(in)    :: FieldType
  integer,dimension(*)       ,intent(inout) :: Field
  integer                    ,intent(out)   :: Status
  integer                                   :: TimeIndex
  type(wrf_data_handle)      ,pointer       :: DH
  integer(KIND=PIO_OFFSET)                  :: pioidx

  DH => WrfDataHandles(DataHandle)
  call GetTimeIndex(IO,DataHandle,DateStr,TimeIndex,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'Warning in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , TRIM(msg))
    write(msg,*) '  Bad time index for DateStr = ',DateStr
    call wrf_debug ( WARN , TRIM(msg))
    return
  endif

  pioidx = TimeIndex
  call pio_setframe(DH%descVar(DH%CurrentVariable), pioidx)
 !DH%descVar(DH%CurrentVariable)%rec = TimeIndex
  Domains(NDim+1) = TimeIndex

  select case (FieldType)
    case (WRF_REAL)
         call read_bdy_RealFieldIO(DH,NDim,Domains,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Field,Status)
    case (WRF_DOUBLE)
         call read_bdy_DoubleFieldIO(DH,NDim,Domains,MemoryStart,MemoryEnd,PatchStart,PatchEnd,Field,Status)
    case default
         Status = WRF_WARN_DATA_TYPE_NOT_FOUND
         write(msg,*) 'Warning DATA TYPE NOT FOUND in ',__FILE__,', line', __LINE__
         call wrf_debug ( WARN , TRIM(msg))
         return
  end select

  return
end subroutine FieldBDY

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
    LOGICAL :: dryrun, retval
    call ext_pio_inquire_filename( DataHandle, fname, filestate, Status )
    IF ( Status /= WRF_NO_ERR ) THEN
      write(msg,*) 'Warning Status = ',Status,' in ',__FILE__, &
                   ', line', __LINE__
      call wrf_debug ( WARN , TRIM(msg) )
      retval = .FALSE.
    ELSE
      dryrun = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
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
      dryrun = ( filestate .EQ. WRF_FILE_OPENED_NOT_COMMITTED )
      retval = .NOT. dryrun
    ENDIF
    ncd_ok_to_get_dom_ti = retval
    RETURN
END FUNCTION ncd_ok_to_get_dom_ti

subroutine initialize_pio(grid, DH)
   implicit none

   type(domain)                   :: grid
   type(wrf_data_handle), pointer :: DH

   integer     :: ierr
   integer(i4) :: communicator, pioprocs, piostart, piostride, pioshift

   communicator = grid%communicator

   if(.not. associated(DH%iosystem)) then
      allocate(DH%iosystem)
   end if

   DH%Write = 0

  !call pio_setdebuglevel(1)

   call mpi_comm_size(communicator, nprocs, ierr)
   call mpi_comm_rank(communicator, myrank, ierr)

   if(grid%pioprocs > nprocs) then
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

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(2(a,i6))') 'nprocs = ', nprocs, ', myrank = ', myrank
  !write(unit=0, fmt='(4(a,i6))') 'pioprocs = ', pioprocs, &
  !                             ', piostride = ', piostride, &
  !                             ', piostart = ', piostart, &
  !                             ', pioshift = ', pioshift

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
           dimension((ime - ims + 1) * (jme - jms + 1) * grid%ensdim) &
           :: compdof_3d_ensemble
   integer(kind=PIO_Offset), &
           dimension((jme - jms + 1) * (kme - kms + 1) * grid%spec_bdy_width ) &
           :: compdof_3d_xsz, compdof_3d_xez
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (kme - kms + 1) * grid%spec_bdy_width ) &
           :: compdof_3d_ysz, compdof_3d_yez
   integer(kind=PIO_Offset), &
           dimension((jme - jms + 1) * grid%spec_bdy_width ) &
           :: compdof_2d_xs, compdof_2d_xe
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * grid%spec_bdy_width ) &
           :: compdof_2d_ys, compdof_2d_ye
   integer(kind=PIO_Offset), &
           dimension((ime - ims + 1) * (jme - jms + 1)) &
           :: compdof_2d
   integer :: dims3d(3), dims2d(2), dims2di(3)
   integer :: dims3d_xb(3), dims2d_xb(2)
   integer :: dims3d_yb(3), dims2d_yb(2)
   integer :: dims3d_land(3), dims3d_soil(3), dims3d_soil_layers(3)
   integer :: dims3d_mdl_cpl(3)
   integer :: dims3d_ensemble(3)
   integer :: lite, ljte, lkte
   integer :: i, j, k, n, npos

   DH%first_operation = .false.
   communicator = grid%communicator
   myrank = DH%myrank

!--For MASS variables
   dims3d(1) = ide - 1
   dims3d(2) = jde - 1
   dims3d(3) = kde - 1

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

   dims3d_land(1) = dims3d(1)
   dims3d_land(2) = dims3d(2)
   dims3d_land(3) = grid%num_land_cat

   dims3d_soil(1) = dims3d(1)
   dims3d_soil(2) = dims3d(2)
   dims3d_soil(3) = grid%num_soil_cat

   dims3d_soil_layers(1) = dims3d(1)
   dims3d_soil_layers(2) = dims3d(2)
   dims3d_soil_layers(3) = grid%num_soil_layers

   dims3d_mdl_cpl(1) = dims3d(1)
   dims3d_mdl_cpl(2) = dims3d(2)
   dims3d_mdl_cpl(3) = grid%num_ext_model_couple_dom

   dims3d_ensemble(1) = dims3d(1)
   dims3d_ensemble(2) = dims3d(2)
   dims3d_ensemble(3) = grid%ensdim

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)

   dims2di(1) = dims3d(1)
   dims2di(2) = dims3d(2)
   dims2di(3) = 1

   dims3d_xb(1) = dims3d(2)
   dims3d_xb(2) = dims3d(3)
   dims3d_xb(3) = grid%spec_bdy_width

   dims3d_yb(1) = dims3d(1)
   dims3d_yb(2) = dims3d(3)
   dims3d_yb(3) = grid%spec_bdy_width

   dims2d_xb(1) = dims2d(2)
   dims2d_xb(2) = grid%spec_bdy_width

   dims2d_yb(1) = dims2d(1)
   dims2d_yb(2) = grid%spec_bdy_width

  !write(unit=0, fmt='(3a,i6)') 'file: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(a, 6i6)') 'dims2d = ', dims2d
  !write(unit=0, fmt='(a, 6i6)') 'dims3d = ', dims3d
  !write(unit=0, fmt='(a, 6i6)') 'dims3d_land = ', dims3d_land
  !write(unit=0, fmt='(a, 6i6)') 'dims3d_soil = ', dims3d_soil
  !write(unit=0, fmt='(a, 6i6)') 'grid%num_land_cat = ', grid%num_land_cat
  !write(unit=0, fmt='(a, 6i6)') 'grid%num_soil_cat = ', grid%num_soil_cat
  !write(unit=0, fmt='(a, 6i6)') 'grid%num_soil_layers = ', grid%num_soil_layers
  !write(unit=0, fmt='(a, 6i6)') 'grid%num_ext_model_couple_dom = ', grid%num_ext_model_couple_dom
  !write(unit=0, fmt='(a, 6i6)') 'grid%spec_bdy_width = ', grid%spec_bdy_width

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

   do n = 1, grid%spec_bdy_width
      do i = ims, ime
         npos = i - ims + 1 + (ime - ims + 1) * (n - 1)
         compdof_2d_ys(npos) = 0
         compdof_2d_ye(npos) = 0
      enddo

      do j = jms, jme
         npos = j - jms + 1 + (jme - jms + 1) * (n - 1)
         compdof_2d_xs(npos) = 0
         compdof_2d_xe(npos) = 0
      enddo

      do k = kms, kme
      do i = ims, ime
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = 0
         compdof_3d_yez(npos) = 0
      enddo
      enddo

      do k = kms, kme
      do j = jms, jme
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = 0
         compdof_3d_xez(npos) = 0
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
         compdof_3d(npos) = i + dims3d(1) * (j - 1 + dims3d(2) * (k - 1))
      enddo
      enddo

      do k = 1, dims3d_land(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_land(3) * (j - jms))
         compdof_3d_land(npos) = i + dims3d_land(1) * (j - 1 + dims3d_land(2) * (k - 1))
      enddo
      enddo

      do k = 1, dims3d_soil(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil(3) * (j - jms))
         compdof_3d_soil(npos) = i + dims3d_soil(1) * (j - 1 + dims3d_soil(2) * (k - 1))
      enddo
      enddo

      do k = 1, dims3d_soil_layers(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_soil_layers(3) * (j - jms))
         compdof_3d_soil_layers(npos) = i + dims3d_soil_layers(1) * (j - 1 + dims3d_soil_layers(2) * (k - 1))
      enddo
      enddo

      do k = 1, dims3d_mdl_cpl(3)
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (k - 1 + dims3d_mdl_cpl(3) * (j - jms))
         compdof_3d_mdl_cpl(npos) = i + dims3d_mdl_cpl(1) * (j - 1 + dims3d_mdl_cpl(2) * (k - 1))
      enddo
      enddo
   enddo

   do k = 1, dims3d_ensemble(3)
      do j = jms, jme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms + (jme - jms + 1) * (k - 1))
         compdof_3d_ensemble(npos) = 0
      enddo
      enddo

      do j = jts, ljte
      do i = its, lite
         npos = (i - ims + 1) + (ime - ims + 1) * (j - jms + (jme - jms + 1) * (k - 1))
         compdof_3d_ensemble(npos) = i + dims3d_ensemble(1) * (j - 1 + dims3d_ensemble(2) * (k - 1))
      enddo
      enddo
   enddo

  !write(unit=0, fmt='(3a,i6)') 'File: ', __FILE__, ', line: ', __LINE__
  !write(unit=0, fmt='(4x,a,i6)') 'npos = ', npos
  !write(unit=0, fmt='(4x,a,i16)') 'compdof_3d_ensemble(npos) = ', compdof_3d_ensemble(npos)

   if(1 == its) then
      do n = 1, grid%spec_bdy_width
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (n - 1)
         compdof_2d_xs(npos) = j + dims2d_xb(1) * (n - 1)
      enddo
      enddo
   endif

   if(1 == jts) then
      do n = 1, grid%spec_bdy_width
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (n - 1)
         compdof_2d_ys(npos) = i + dims2d_yb(1) * (n - 1)
      enddo
      enddo
   endif

   if(dims2d(1) == lite) then
      do n = 1, grid%spec_bdy_width
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (n - 1)
         compdof_2d_xe(npos) = j + dims2d_xb(1) * (n - 1)
      enddo
      enddo
   endif

   if(dims2d(2) == ljte) then
      do n = 1, grid%spec_bdy_width
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (n - 1)
         compdof_2d_ye(npos) = i + dims2d_yb(1) * (n - 1)
      enddo
      enddo
   endif

   if(1 == its) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(1 == jts) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims2d(1) == lite) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xez(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims2d(2) == ljte) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_yez(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

!--call init_decomp in order to setup the IO decomposition with PIO
  !call pio_setdebuglevel(1)

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

  !call pio_setdebuglevel(1)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_ensemble, compdof_3d_ensemble, DH%iodesc3d_ensemble_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_ensemble, compdof_3d_ensemble, DH%iodesc3d_ensemble_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_ensemble, compdof_3d_ensemble, DH%iodesc3d_ensemble_double)
  !call pio_setdebuglevel(0)

#ifndef INTSPECIAL
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_m_int)
#else
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2di, compdof_2d, DH%iodesc2d_m_int)
#endif
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d_xb, compdof_2d_xs, DH%iodesc2d_xs_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d_xb, compdof_2d_xs, DH%iodesc2d_xs_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d_xb, compdof_2d_xs, DH%iodesc2d_xs_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d_xb, compdof_2d_xe, DH%iodesc2d_xe_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d_xb, compdof_2d_xe, DH%iodesc2d_xe_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d_xb, compdof_2d_xe, DH%iodesc2d_xe_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d_yb, compdof_2d_ys, DH%iodesc2d_ys_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d_yb, compdof_2d_ys, DH%iodesc2d_ys_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d_yb, compdof_2d_ys, DH%iodesc2d_ys_m_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d_yb, compdof_2d_ye, DH%iodesc2d_ye_m_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d_yb, compdof_2d_ye, DH%iodesc2d_ye_m_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d_yb, compdof_2d_ye, DH%iodesc2d_ye_m_double)

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

   dims3d_xb(1) = dims3d(2)
   dims3d_xb(2) = dims3d(3)
   dims3d_xb(3) = grid%spec_bdy_width

   dims3d_yb(1) = dims3d(1)
   dims3d_yb(2) = dims3d(3)
   dims3d_yb(3) = grid%spec_bdy_width

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

   do n = 1, grid%spec_bdy_width
      do k = kms, kme
      do i = ims, ime
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = 0
         compdof_3d_yez(npos) = 0
      enddo
      enddo

      do k = kms, kme
      do j = jms, jme
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = 0
         compdof_3d_xez(npos) = 0
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
         compdof_3d(npos) = i + dims3d(1) * (j - 1 + dims3d(2) * (k - 1))
      enddo
      enddo
   enddo

   if(1 == its) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(1 == jts) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(1) == lite) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xez(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(2) == ljte) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_yez(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_u_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_u_int)

   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_u_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_u_int)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_u_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_u_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_u_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_u_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_u_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_u_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_u_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_u_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_u_double)

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

   dims3d_xb(1) = dims3d(2)
   dims3d_xb(2) = dims3d(3)
   dims3d_xb(3) = grid%spec_bdy_width

   dims3d_yb(1) = dims3d(1)
   dims3d_yb(2) = dims3d(3)
   dims3d_yb(3) = grid%spec_bdy_width

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

   do n = 1, grid%spec_bdy_width
      do k = kms, kme
      do i = ims, ime
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = 0
         compdof_3d_yez(npos) = 0
      enddo
      enddo

      do k = kms, kme
      do j = jms, jme
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = 0
         compdof_3d_xez(npos) = 0
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
         compdof_3d(npos) = i + dims3d(1) * (j - 1 + dims3d(2) * (k - 1))
      enddo
      enddo
   enddo

   if(1 == its) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(1 == jts) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(1) == lite) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xez(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(2) == ljte) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_yez(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_v_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_v_int)

   call PIO_initdecomp(DH%iosystem, PIO_double, dims2d, compdof_2d, DH%iodesc2d_v_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims2d, compdof_2d, DH%iodesc2d_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims2d, compdof_2d, DH%iodesc2d_v_int)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_v_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_v_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_v_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_v_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_v_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_v_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_v_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_v_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_v_double)

!--For Z-STAG variables
   dims3d(1) = ide - 1
   dims3d(2) = jde - 1
   dims3d(3) = kde

   dims2d(1) = dims3d(1)
   dims2d(2) = dims3d(2)

   dims3d_xb(1) = dims3d(2)
   dims3d_xb(2) = dims3d(3)
   dims3d_xb(3) = grid%spec_bdy_width

   dims3d_yb(1) = dims3d(1)
   dims3d_yb(2) = dims3d(3)
   dims3d_yb(3) = grid%spec_bdy_width

   lite = ite
   ljte = jte
   lkte = kte

   if(lite > dims3d(1)) lite = dims3d(1)
   if(ljte > dims3d(2)) ljte = dims3d(2)
   if(lkte > dims3d(3)) lkte = dims3d(3)

  !compdof_3d =  0

   do j = jms, jme
      do k = kms, kme
      do i = ims, ime
         npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
         compdof_3d(npos) = 0
      enddo
      enddo
   enddo

   do n = 1, grid%spec_bdy_width
      do k = kms, kme
      do i = ims, ime
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = 0
         compdof_3d_yez(npos) = 0
      enddo
      enddo

      do k = kms, kme
      do j = jms, jme
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = 0
         compdof_3d_xez(npos) = 0
      enddo
      enddo
   enddo

   do j = jts, ljte
   do k = kts, lkte
   do i = its, lite
      npos = (i - ims + 1) + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (j - jms))
      compdof_3d(npos) = i + dims3d(1) * (j - 1 + dims3d(2) * (k - 1))
   enddo
   enddo
   enddo

   if(1 == its) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xsz(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(1 == jts) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_ysz(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(1) == lite) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do j = jts, ljte
         npos = j - jms + 1 + (jme - jms + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_xez(npos) = j + dims3d_xb(1) * (k - 1 + dims3d_xb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

   if(dims3d(2) == ljte) then
      do n = 1, grid%spec_bdy_width
      do k = kts, lkte
      do i = its, lite
         npos = i - ims + 1 + (ime - ims + 1) * (k - kms + (kme - kms + 1) * (n - 1))
         compdof_3d_yez(npos) = i + dims3d_yb(1) * (k - 1 + dims3d_yb(2) * (n - 1))
      enddo
      enddo
      enddo
   endif

!--call init_decomp in order to setup the IO decomposition with PIO
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d, compdof_3d, DH%iodesc3d_w_double)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d, compdof_3d, DH%iodesc3d_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d, compdof_3d, DH%iodesc3d_w_int)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_w_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xsz, DH%iodesc3d_xsz_w_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_w_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_xb, compdof_3d_xez, DH%iodesc3d_xez_w_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_w_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_ysz, DH%iodesc3d_ysz_w_double)

   call PIO_initdecomp(DH%iosystem, PIO_int,    dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_w_int)
   call PIO_initdecomp(DH%iosystem, PIO_real,   dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_w_real)
   call PIO_initdecomp(DH%iosystem, PIO_double, dims3d_yb, compdof_3d_yez, DH%iodesc3d_yez_w_double)

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

