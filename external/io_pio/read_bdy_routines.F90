!------------------------------------------------------------------
!$Id$
!------------------------------------------------------------------

subroutine read_bdy_RealFieldIO(DH,onbdy,NDim,Dimens,Starts,Counts,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  type(wrf_data_handle)                    :: DH
  logical                     ,intent(in)  :: onbdy
  integer                     ,intent(in)  :: NDim
  integer,dimension(NVarDims) ,intent(in)  :: Dimens
  integer,dimension(NVarDims) ,intent(in)  :: Starts
  integer,dimension(NVarDims) ,intent(in)  :: Counts
  real, dimension(*)          ,intent(out) :: Data
  integer                     ,intent(out) :: Status
  integer,dimension(4)                     :: Ones
  integer                                  :: stat
  integer                                  :: i,k,n,nloc,nglb
  real,dimension(Dimens(1)*Dimens(2)*Dimens(3)) :: buffer

  write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
  write(unit=0, fmt='(4x,a,i3,3a,i2,a,l8)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
        trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim, ', obbdy = ', onbdy
  write(unit=0, fmt='(4x,5(a,i6))') &
       'Dimens(1)=', Dimens(1), &
       ', Starts(1)=', Starts(1), &
       ', Counts(1)=', Counts(1), &
       ', Starts(5)=', Starts(NVarDims), &
       ', Counts(5)=', Counts(NVarDims)

 !call pio_setdebuglevel(10)

  Ones = 1

  if(3 == NDim) then
     stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Ones,Dimens(1:4),buffer)
  else if(2 == NDim) then
     stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Ones(1:3),Dimens(1:3),buffer)
  else
     stat = -1
  end if
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif

  if(onbdy) then
    if(3 == Ndim) then
      do n = 1, Counts(3)
      do k = 1, Counts(2)
      do i = 1, Counts(1)
         nglb = i + Starts(1) - 1 + Dimens(1) * (k - 1 + Dimens(2) * (n - 1))
         nloc = i + Starts(NVarDims) - 1 + Counts(NVarDims) * (k - 1 + Counts(2) * (n - 1))
         Data(nloc) = buffer(nglb)
      end do
      end do
      end do
    else if(2 == Ndim) then
      do n = 1, Counts(2)
      do i = 1, Counts(1)
         nglb = i + Starts(1) - 1 + Dimens(1) * (n - 1)
         nloc = i + Starts(NVarDims) - 1 + Counts(NVarDims) * (n - 1)
         Data(nloc) = buffer(nglb)

         write(unit=0, fmt='(6x,2(2x,a,i4),2(2x,a,i6),2(2x,a,e12.6))') &
              'n=', n, ', i=', i, ', nglb=', nglb, ', nloc=', nloc, &
              ', Data(nloc)=', Data(nloc), ', buffer(nglb)=', buffer(nglb)
      end do
      end do
    else 
      write(unit=0, fmt='(/3a,i6)') 'file: ',__FILE__,', line', __LINE__
      write(unit=0, fmt='(a,i6)') 'Do not know how handle NDim = ', NDim
      write(unit=0, fmt='(4x,a,i4,a,i3,4x,a,i3)') &
           'DH%vartype(', DH%CurrentVariable, ') =', DH%vartype(DH%CurrentVariable), &
           'BDY_VAR =', BDY_VAR
      write(unit=0, fmt='(4x,a,i3,3a,i2)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
            trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim
    end if
  end if
end subroutine read_bdy_RealFieldIO

subroutine read_bdy_DoubleFieldIO(DH,onbdy,NDim,Dimens,Starts,Counts,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  type(wrf_data_handle)       ,pointer     :: DH
  logical                     ,intent(in)  :: onbdy
  integer                     ,intent(in)  :: NDim
  integer,dimension(NVarDims) ,intent(in)  :: Dimens
  integer,dimension(NVarDims) ,intent(in)  :: Starts
  integer,dimension(NVarDims) ,intent(in)  :: Counts
  real*8,dimension(*)         ,intent(out) :: Data
  integer                     ,intent(out) :: Status
  integer                                  :: stat
  integer                                  :: i,k,n,nloc,nglb
  real*8,dimension(Dimens(1)*Dimens(2)*Dimens(3)) :: buffer

  stat = pio_get_var(DH%file_handle,DH%descVar(DH%CurrentVariable),Starts,Counts,buffer)
  call netcdf_err(stat,Status)
  if(Status /= WRF_NO_ERR) then
    write(msg,*) 'NetCDF error in ',__FILE__,', line', __LINE__
    call wrf_debug ( WARN , msg)
  endif

  if(onbdy) then
    if(3 == Ndim) then
      do n = 1, Counts(3)
      do k = 1, Counts(2)
      do i = 1, Counts(1)
         nglb = i + Starts(1) - 1 + Dimens(1) * (k - 1 + Dimens(2) * (n - 1))
         nloc = i + Starts(NVarDims) - 1 + Counts(NVarDims) * (k - 1 + Counts(2) * (n - 1))
         Data(nloc) = buffer(nglb)
      end do
      end do
      end do
    else if(2 == Ndim) then
      do n = 1, Counts(2)
      do i = 1, Counts(1)
         nglb = i + Starts(1) - 1 + Dimens(1) * (n - 1)
         nloc = i + Starts(NVarDims) - 1 + Counts(NVarDims) * (n - 1)
         Data(nloc) = buffer(nglb)
      end do
      end do
    else 
      write(unit=0, fmt='(/3a,i6)') 'file: ',__FILE__,', line', __LINE__
      write(unit=0, fmt='(a,i6)') 'Do not know how handle NDim = ', NDim
      write(unit=0, fmt='(4x,a,i4,a,i3,4x,a,i3)') &
           'DH%vartype(', DH%CurrentVariable, ') =', DH%vartype(DH%CurrentVariable), &
           'BDY_VAR =', BDY_VAR
      write(unit=0, fmt='(4x,a,i3,2a)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
            trim(DH%VarNames(DH%CurrentVariable))
    end if
  end if
end subroutine read_bdy_DoubleFieldIO

