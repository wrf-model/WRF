program testwrite_john
  use wrf_data_pnc
  implicit none
  include 'wrf_status_codes.h'
  include 'netcdf.inc'
  character (80) FileName
  integer Comm
  character (80) SysDepInfo
  integer     :: DataHandle
  integer Status
  integer NCID
  real data(200)
  integer idata(200)
  real*8 ddata(200)
  logical ldata(200)
  character (80) cdata
  integer OutCount
  integer i,j,k

  integer, parameter ::    pad = 3  
  integer, parameter ::    jds=1       , jde=6      , &
                           ids=1       , ide=9      , &
                           kds=1       , kde=5         
  integer, parameter ::    jms=jds-pad , jme=jde+pad , &
                           ims=ids-pad , ime=ide+pad , &
                           kms=kds     , kme=kde       
  integer, parameter ::    jps=jds     , jpe=jde    , &
                           ips=ids     , ipe=ide    , &
                           kps=kds     , kpe=kde       

  real u( ims:ime , kms:kme , jms:jme )
  real v( ims:ime , kms:kme , jms:jme )
  real rho( ims:ime , kms:kme , jms:jme )
  real u2( ims:ime , jms:jme )
  real u1( ims:ime )

  integer int( ims:ime , kms:kme , jms:jme )
  real*8  r8 ( ims:ime , kms:kme , jms:jme )

  integer Dom
  character*3 MemOrd
  character (19) Date
  character (19) Date2
  integer , Dimension(3) :: DomS,DomE,MemS,MemE,PatS,PatE
  integer , Dimension(2) :: Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E
  integer , Dimension(1) :: Dom1S,Dom1E,Mem1S,Mem1E,Pat1S,Pat1E
  print *, 'Testing wrf write'
  print *, ims,ime , kms,kme , jms,jme
  Date = '2000-09-18_16:42:01'
  Date2 = '2000-09-18_16:52:01'
  call ext_init(Status)
  print *,'After call ext_init, Status =',Status
  FileName = 'foo.nc'
  Comm = 1
  SysDepInfo = 'sys info'

print*,'!!!!!!!!!!!!!!!!!!!!!!! ext_open_for_write_begin'

  call ext_open_for_write_begin( FileName, Comm, SysDepInfo, DataHandle, Status)
  print *, ' ext_open_for_write_begin Status = ',Status,DataHandle

  MemOrd = "XZY"

  DomS(1) = ids
  DomE(1) = ide
  DomS(2) = kds
  DomE(2) = kde
  DomS(3) = jds
  DomE(3) = jde

  PatS(1) = ips
  PatE(1) = ipe
  PatS(2) = kps
  PatE(2) = kpe
  PatS(3) = jps
  PatE(3) = jpe

  MemS(1) = ims
  MemE(1) = ime
  MemS(2) = kms
  MemE(2) = kme
  MemS(3) = jms
  MemE(3) = jme

  Dom2S(1) = ids
  Dom2S(2) = jds
  Dom2E(1) = ide
  Dom2E(2) = jde
  Mem2S(1) = ims
  Mem2S(2) = jms
  Mem2E(1) = ime
  Mem2E(2) = jme
  Pat2S(1) = ips
  Pat2S(2) = jps
  Pat2E(1) = ipe
  Pat2E(2) = jpe

  Dom1S = ids
  Dom1E = ide
  Mem1S = ims
  Mem1E = ime
  Pat1S = ips
  Pat1E = ipe

  call ext_write_field(DataHandle,Date,'u',u,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'v',v,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'rho',rho,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'u2',u2,WRF_REAL,Comm,Dom,'XY',Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'ud2',u,WRF_REAL,Comm,Dom,"XzY",DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'u1',u1,WRF_REAL,Comm,Dom,'Z',Dom1S,Dom1E,Mem1S,Mem1E,Pat1S,Pat1E,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'int',int,WRF_INTEGER,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'double',r8,WRF_DOUBLE,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status

  call ext_open_for_write_commit(DataHandle, Status)
  print *, '             ext_open_for_write_commit Status = ', Status,DataHandle

  do j=jds,jde
    do k=kds,kde
      do i=ids,ide
        u  (i,k,j) = 100*i+j+10*k
        v  (i,k,j) = 100*i+j+10*k
        rho(i,k,j) = 100*i+j+10*k
        int(i,k,j) = 100*i+j+10*k
        r8 (i,k,j) = 100*i+j+10*k
      enddo
    enddo
  enddo
  do j=jds,jde
    do i=ids,ide
      u2(i,j) = 10*i+j
    enddo
  enddo
  do i=ids,ide
    u1(i) = i
  enddo

  print *,'testWRFWrite u  (2,3,4) = ',u(2,3,4)
  print *,'testWRFWrite v  (4,3,2) = ',v(4,3,2)
  print *,'testWRFWrite rho(3,4,5) = ',rho(3,4,5)
  print *,'testWRFWrite u2 (6,5)   = ',u2(6,5)
  print *,'testWRFWrite u1 (9)     = ',u1(9)
  print *,'testWRFWrite int(8,5,6) = ',int(8,5,6)
  print *,'testWRFWrite r8 (7,4,5) = ',r8(7,4,5)
  call ext_write_field(DataHandle,Date,'u',u,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'v',v,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'rho',rho,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'u2',u2,WRF_REAL,Comm,Dom,'XY',Dom2S,Dom2E,Mem2S,Mem2E,Pat2S,Pat2E,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'ud2',u,WRF_REAL,Comm,Dom,"XzY",DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'u1',u1,WRF_REAL,Comm,Dom,'Z',Dom1S,Dom1E,Mem1S,Mem1E,Pat1S,Pat1E,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'int',int,WRF_INTEGER,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              first write: ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date,'double',r8,WRF_DOUBLE,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'             dry run : ext_write_field Status = ',Status

  print *,'2nd : testWRFWrite u(3,3,3) = ',u(3,3,3)
  print *,'2nd : testWRFWrite v(4,4,4) = ',v(4,4,4)
  print *,'2nd : testWRFWrite rho(3,4,5) = ',rho(3,4,5)
  call ext_write_field(DataHandle,Date2,'u',u,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              2nd write : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date2,'v',v,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              2nd write : ext_write_field Status = ',Status
  call ext_write_field(DataHandle,Date2,'rho',rho,WRF_REAL,Comm,Dom,MemOrd,DomS,DomE,MemS,MemE,PatS,PatE,Status)
  print *,'              2nd write : ext_write_field Status = ',Status

  call ext_close( DataHandle, Status)
  print *, '             After ext_close, Status = ',Status
  call ext_exit(Status)
  print *,'              End of test program',Status
  stop
  end program testwrite_john
