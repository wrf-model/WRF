! on linux, compile wrf then compile as:
! pgf90 -Mfree -I ../../main -I ../../inc -I /usr/local/netcdf-pgi/include vort.F90 libwrfio_nf.a /usr/local/netcdf-pgi/lib/libnetcdf.a ../../main/libwrflib.a
! on AIX, compile wrf then compile as:
! /lib/cpp -C -P vort.F90 > vort.f
! mpxlf -qfree=f90 -I ../../share -I ../../main -I ../../inc -I /usr/local/netcdf/include vort.f libwrfio_nf.a /usr/local/netcdf/lib/libnetcdf.a ../../main/libwrflib.a

module read_util_module

contains

   subroutine arguments(v2file, lmore)
     implicit none
     character(len=*) :: v2file
     character(len=120) :: harg
     logical :: lmore
   
     integer :: ierr, i, numarg
   
     numarg = command_argument_count()
   
     i = 1
     lmore = .false.
   
     do while ( i < numarg) 
        call get_command_argument(number=i, value=harg)
        print*, 'harg = ', trim(harg)
   
        if (harg == "-v") then
           i = i + 1
           lmore = .true.
        elseif (harg == "-h") then
           call help
        endif
   
     enddo
   
     call get_command_argument(number=i, value=harg)
     v2file = harg
   end subroutine arguments
   
   subroutine help
     implicit none
     character(len=120) :: cmd
     call get_command_argument(number=0, value=cmd)
   
     write(*,'(/,"Usage: ", A, " [-v] v2file ")') trim(cmd)
     write(*,'(8x, "-v     : Print extra info")')
     write(*,'(8x, "v3file : MM5v3 file name to read.")')
     write(*,'(8x, "-h     : print this help message and exit.",/)')
     stop
   end subroutine help
end module read_util_module



 program readv3
  use wrf_data
  use read_util_module
  use module_compute_geop


  implicit none
#include "wrf_status_codes.h"
#include <netcdf.inc>
  character(len=120) :: flnm
  character(len=120) :: flnm2
  character(len=120) :: arg3
  character(len=19) :: DateStr
  character(len=19) :: DateStr2
  character(len=31) :: VarName
  character(len=31) :: VarName2
  integer dh1, dh2

  integer :: flag, flag2
  integer :: iunit, iunit2

  integer :: i,j,k
  integer :: levlim
  integer :: cross
  integer :: ndim, ndim2
  integer :: WrfType, WrfType2
  real :: time, time2
  real*8 :: a, b
  real*8 :: sum1, sum2, diff1, diff2, serr, perr, rms
  integer, dimension(4) :: start_index, end_index, start_index2, end_index2, end_index_u, end_index_uz
  integer , Dimension(3) :: MemS,MemE,PatS,PatE
  character (len= 4) :: staggering,   staggering2
  character (len= 3) :: ordering,     ordering2, ord
  character (len=24) :: start_date,   start_date2
  character (len=24) :: current_date, current_date2
  character (len=31) :: name,         name2,  tmpname
  character (len=25) :: units,        units2
  character (len=46) :: description,  description2

  real, allocatable, dimension(:,:,:) :: ph, phb, p, pb
  real, allocatable, dimension(:,:)   :: height

  integer ::  ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              its, ite, jts, jte, kts, kte
  integer outcount


  character (len=80), dimension(3)  ::  dimnames
  character (len=80) :: SysDepInfo

  integer :: l, n
  integer :: ikdiffs, ifdiffs

  real, allocatable, dimension(:,:,:,:) :: data,data2

  integer :: ierr, ierr2, ier, ier2, Status, Status_next_time, Status_next_time2, Status_next_var, Status_next_var_2

  logical :: newtime = .TRUE.
  logical :: justplot, efound

  integer, external :: iargc
  logical, external :: iveceq

  levlim = -1

  call ext_ncd_ioinit(SysDepInfo,Status)
  call set_wrf_debug_level ( 1 )


  Justplot = .true.

! get arguments
!  if ( iargc() .ge. 2 ) then
    call get_command_argument(number=1, value=flnm)
!    call get_command_argument(number=2, value=flnm2)
    ierr = 0
    call ext_ncd_open_for_read( trim(flnm), 0, 0, "", dh1, Status)
    if ( Status /= 0 ) then 
      print*,'error opening ',flnm, ' Status = ', Status ; stop 
    endif
!    call ext_ncd_open_for_read( trim(flnm2), 0, 0, "", dh2, Status)
!    if ( Status /= 0 ) go to 923
!    goto 924
!923    continue
!
!! bounce here if second name is not openable -- this would mean that
!! it is a field name instead.
!
!    print*,'could not open ',flnm2
!    name = flnm2
!    Justplot = .true.
!924    continue
!  if ( iargc() .eq. 3 ) then
!    call get_command_argument(number=3, value=arg3)
!    read(arg3,*)levlim
!    print*,'LEVLIM = ',LEVLIM
!  endif
!  else
!     print*,'Usage: command file1 file2'
!     stop
!  endif

!print*,'Just plot ',Justplot

start_index = 1
end_index = 0

CALL ext_ncd_get_dom_ti_integer(dh1,'WEST-EAST_GRID_DIMENSION',end_index(1),1,OutCount,Status)
CALL ext_ncd_get_dom_ti_integer(dh1,'BOTTOM-TOP_GRID_DIMENSION',end_index(2),1,OutCount,Status)
CALL ext_ncd_get_dom_ti_integer(dh1,'SOUTH-NORTH_GRID_DIMENSION',end_index(3),1,OutCount,Status)
ord = 'XZY'
staggering = ' '



allocate(ph(end_index(1),end_index(2),end_index(3)))
allocate(phb(end_index(1),end_index(2),end_index(3)))
allocate(p(end_index(1),end_index(2),end_index(3)))
allocate(pb(end_index(1),end_index(2),end_index(3)))
allocate(height(end_index(1),end_index(3)))

ids=start_index(1); ide=end_index(1); jds=start_index(3); jde=end_index(3); kds=start_index(2); kde=end_index(2)
ims=start_index(1); ime=end_index(1);   jms=start_index(3); jme=end_index(3);   kms=start_index(2); kme=end_index(2)
its=start_index(1); ite=end_index(1)-1; jts=start_index(3); jte=end_index(3)-1; kts=start_index(2); kte=end_index(2)-1

end_index_u  = end_index - 1
end_index_uz = end_index - 1
end_index_uz(2) = end_index_uz(2) + 1



if ( Justplot ) then
  print*, 'flnm = ', trim(flnm)

  call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)

  DO WHILE ( Status_next_time .eq. 0 )
    write(*,*)'Next Time ',TRIM(Datestr)

    staggering = 'Z'
    name = 'PH'
    call ext_ncd_read_field(dh1,DateStr,TRIM(name),ph,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index_uz,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index_uz,                      & !pat
                            ierr)
    name = 'PHB'
    call ext_ncd_read_field(dh1,DateStr,TRIM(name),phb,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index_uz,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index_uz,                      & !pat
                            ierr)
    staggering = ' '
    name = 'P'
    call ext_ncd_read_field(dh1,DateStr,TRIM(name),p,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index_u,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index_u,                      & !pat
                            ierr)
    name = 'PB'
    call ext_ncd_read_field(dh1,DateStr,TRIM(name),pb,WRF_REAL,0,0,0,ord, &
                            staggering, dimnames ,                      &
                            start_index,end_index_u,                      & !dom
                            start_index,end_index,                      & !mem
                            start_index,end_index_u,                      & !pat
                            ierr)

    CALL compute_500mb_height  ( ph, phb, p, pb,                  &
                                   height,                          &
                                   ids, ide, jds, jde, kds, kde,    &
                                   ims, ime, jms, jme, kms, kme,    &
                                   its, ite, jts, jte, kts, kte    )

    write(88,*)end_index_u(1),end_index_u(3),' height ',trim(Datestr)
    do j = 1, end_index_u(3)
      do i = 1, end_index_u(1)
        write(88,*) height(i,j)
      enddo
    enddo

    call ext_ncd_get_next_time(dh1, DateStr, Status_next_time)
  enddo
endif

end program readv3

! stub for routine called by module_wrf_error (used by netcdf implementation of IO api)
SUBROUTINE wrf_abort
  STOP
END SUBROUTINE wrf_abort
