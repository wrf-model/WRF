  ix=0
  jx=0
  kx=0
  call reorder(MemoryOrder,MemO)
  if(IACHAR(MemO(1:1)) > MaxUpperCase) ix=i2+i1
  if(IACHAR(MemO(2:2)) > MaxUpperCase) jx=j2+j1
  if(IACHAR(MemO(3:3)) > MaxUpperCase) kx=k2+k1

! pjj/cray
  if(IO == 'write') then
!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!dir$ prefervector
!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          DFIELD = Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k))
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
else
!dir$ concurrent
!$OMP PARALLEL DO SCHEDULE(RUNTIME) PRIVATE(i,j,k)
  do k=k1,k2
    do j=j1,j2
!dir$ prefervector
!dir$ concurrent
!cdir select(vector)
      do i=i1,i2
          Field(1:di,abs(ix-i),abs(jx-j),abs(kx-k)) = DFIELD
      enddo
    enddo
  enddo
!$OMP END PARALLEL DO
endif

  return
