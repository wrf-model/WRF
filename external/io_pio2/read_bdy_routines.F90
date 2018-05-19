!------------------------------------------------------------------
!$Id: read_bdy_routines.F90 7621 2014-08-14 20:28:51Z huangwei@ucar.edu $
!------------------------------------------------------------------

subroutine transRg2l(ds1,de1,ds2,de2,ds3,de3, &
                     ms1,me1,ms2,me2,ms3,me3, &
                     ps1,pe1,ps2,pe2,ps3,pe3, &
                     dlocal, dglobal)
  integer           ,intent(in)  :: ds1,de1,ds2,de2,ds3,de3
  integer           ,intent(in)  :: ms1,me1,ms2,me2,ms3,me3
  integer           ,intent(in)  :: ps1,pe1,ps2,pe2,ps3,pe3
  real              ,intent(out) ::  dlocal(ms1:me1,ms2:me2,ms3:me3)
  real              ,intent(in)  :: dglobal(ds1:de1,ds2:de2,ds3:de3)
  integer                        :: i,j,k

!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=ps3,pe3
    do j=ps2,pe2
      do i=ps1,pe1
          dlocal(i,j,k) = dglobal(i,j,k)
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
  return
end subroutine transRg2l

subroutine transDg2l(ds1,de1,ds2,de2,ds3,de3, &
                     ms1,me1,ms2,me2,ms3,me3, &
                     ps1,pe1,ps2,pe2,ps3,pe3, &
                     dlocal, dglobal)
  integer           ,intent(in)  :: ds1,de1,ds2,de2,ds3,de3
  integer           ,intent(in)  :: ms1,me1,ms2,me2,ms3,me3
  integer           ,intent(in)  :: ps1,pe1,ps2,pe2,ps3,pe3
  real*8            ,intent(out) ::  dlocal(ms1:me1,ms2:me2,ms3:me3)
  real*8            ,intent(in)  :: dglobal(ds1:de1,ds2:de2,ds3:de3)
  integer                        :: i,j,k

!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=ps3,pe3
    do j=ps2,pe2
      do i=ps1,pe1
          dlocal(i,j,k) = dglobal(i,j,k)
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
  return
end subroutine transDg2l

subroutine read_bdy_RealFieldIO(DH,NDim,Dimens,MemoryStart,MemoryEnd, &
                                PatchStart,PatchEnd,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  type(wrf_data_handle)                    :: DH
  integer                     ,intent(in)  :: NDim
  integer, dimension(*)       ,intent(in)  :: Dimens
  integer, dimension(*)       ,intent(in)  :: MemoryStart, MemoryEnd
  integer, dimension(*)       ,intent(in)  :: PatchStart,  PatchEnd
  real, dimension(*)          ,intent(out) :: Data
  integer                     ,intent(out) :: Status
  integer,dimension(4)                     :: Ones
  integer                                  :: stat
  integer                                  :: i,k,n,nloc,nglb
  real,dimension(Dimens(1)*Dimens(2)*Dimens(3)) :: buffer

 !write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
 !write(unit=0, fmt='(4x,a,i3,3a,i2)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
 !      trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim
 !write(unit=0, fmt='(4x,5(a,i6))') &
 !     'Dimens(1)=', Dimens(1), &
 !     ', MemoryStart(1)=', MemoryStart(1), &
 !     ', MemoryEnd(1)=', MemoryEnd(1), &
 !     ', PatchStart(1)=', PatchStart(1), &
 !     ', PatchEnd(1)=', PatchEnd(1)

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

  if(3 == Ndim) then
    call transRg2l(1,Dimens(1),1,Dimens(2),1,Dimens(3), &
                   MemoryStart(1),MemoryEnd(1),MemoryStart(2),MemoryEnd(2),MemoryStart(3),MemoryEnd(3), &
                    PatchStart(1), PatchEnd(1), PatchStart(2), PatchEnd(2), PatchStart(3), PatchEnd(3), &
                   Data, buffer)
  else if(2 == Ndim) then
    call transRg2l(1,Dimens(1),1,Dimens(2),1,Dimens(3), &
                   MemoryStart(1),MemoryEnd(1),MemoryStart(2),MemoryEnd(2),1,1, &
                    PatchStart(1), PatchEnd(1), PatchStart(2), PatchEnd(2),1,1, &
                   Data, buffer)
  else 
    write(unit=0, fmt='(/3a,i6)') 'file: ',__FILE__,', line', __LINE__
    write(unit=0, fmt='(a,i6)') 'Do not know how handle NDim = ', NDim
    write(unit=0, fmt='(4x,a,i4,a,i3,4x,a,i3)') &
         'DH%vartype(', DH%CurrentVariable, ') =', DH%vartype(DH%CurrentVariable), &
         'BDY_VAR =', BDY_VAR
    write(unit=0, fmt='(4x,a,i3,3a,i2)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
          trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim
  end if
end subroutine read_bdy_RealFieldIO

subroutine read_bdy_DoubleFieldIO(DH,NDim,Dimens,MemoryStart,MemoryEnd, &
                                  PatchStart,PatchEnd,Data,Status)
  use pio
  use pio_kinds
  use wrf_data_pio
  use pio_routines
  implicit none
  include 'wrf_status_codes.h'
  type(wrf_data_handle)                    :: DH
  integer                     ,intent(in)  :: NDim
  integer, dimension(*)       ,intent(in)  :: Dimens
  integer, dimension(*)       ,intent(in)  :: MemoryStart, MemoryEnd
  integer, dimension(*)       ,intent(in)  :: PatchStart,  PatchEnd
  real*8,  dimension(*)       ,intent(out) :: Data
  integer                     ,intent(out) :: Status
  integer,dimension(4)                     :: Ones
  integer                                  :: stat
  integer                                  :: i,k,n,nloc,nglb
  real*8,dimension(Dimens(1)*Dimens(2)*Dimens(3)) :: buffer

 !write(unit=0, fmt='(//3a,i6)') 'file: ',__FILE__,', line', __LINE__
 !write(unit=0, fmt='(4x,a,i3,3a,i2)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
 !      trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim
 !write(unit=0, fmt='(4x,5(a,i6))') &
 !     'Dimens(1)=', Dimens(1), &
 !     ', MemoryStart(1)=', MemoryStart(1), &
 !     ', MemoryEnd(1)=', MemoryEnd(1), &
 !     ', PatchStart(1)=', PatchStart(1), &
 !     ', PatchEnd(1)=', PatchEnd(1)

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

  if(3 == Ndim) then
    call transDg2l(1,Dimens(1),1,Dimens(2),1,Dimens(3), &
                   MemoryStart(1),MemoryEnd(1),MemoryStart(2),MemoryEnd(2),MemoryStart(3),MemoryEnd(3), &
                    PatchStart(1), PatchEnd(1), PatchStart(2), PatchEnd(2), PatchStart(3), PatchEnd(3), &
                   Data, buffer)
  else if(2 == Ndim) then
    call transDg2l(1,Dimens(1),1,Dimens(2),1,Dimens(3), &
                   MemoryStart(1),MemoryEnd(1),MemoryStart(2),MemoryEnd(2),1,1, &
                    PatchStart(1), PatchEnd(1), PatchStart(2), PatchEnd(2),1,1, &
                   Data, buffer)
  else 
    write(unit=0, fmt='(/3a,i6)') 'file: ',__FILE__,', line', __LINE__
    write(unit=0, fmt='(a,i6)') 'Do not know how handle NDim = ', NDim
    write(unit=0, fmt='(4x,a,i4,a,i3,4x,a,i3)') &
         'DH%vartype(', DH%CurrentVariable, ') =', DH%vartype(DH%CurrentVariable), &
         'BDY_VAR =', BDY_VAR
    write(unit=0, fmt='(4x,a,i3,3a,i2)') 'DH%VarNames(', DH%CurrentVariable, ') = ', &
          trim(DH%VarNames(DH%CurrentVariable)), ', NDim = ', NDim
  end if
end subroutine read_bdy_DoubleFieldIO

