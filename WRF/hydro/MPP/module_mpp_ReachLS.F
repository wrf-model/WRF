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

!   This is used as a coupler with the WRF model.
MODULE MODULE_mpp_ReachLS

  use module_mpp_land, only:  io_id, my_id, mpp_status, mpp_land_max_int1, mpp_land_sync, HYDRO_COMM_WORLD
  implicit none


  TYPE Grid2ReachMap
      real,allocatable, dimension(:) :: sv
      real,allocatable, dimension(:) :: rv
      real,allocatable, dimension(:) :: rvId
      real,allocatable, dimension(:) :: snId
  end TYPE Grid2ReachMap

  interface ReachLS_decomp
     module procedure ReachLS_decompReal
     module procedure ReachLS_decompReal8
     module procedure ReachLS_decompInt 
     module procedure ReachLS_decompChar
  end interface

  interface ReachLS_write_io
     module procedure ReachLS_wReal
     module procedure ReachLS_wReal2
     module procedure ReachLS_wReal8
     module procedure ReachLS_wInt
     module procedure ReachLS_wInt2
     module procedure ReachLS_wChar
  end interface

  interface gBcastValue
     module procedure gbcastReal
     module procedure gbcastInt 
     module procedure gbcastReal2
  end interface

  interface updateLinkV
     module procedure updateLinkV8_mem
     module procedure updateLinkV4_mem
  end interface
  


  include "mpif.h"

  integer,allocatable,dimension(:) :: sDataRec  ! sending data size
  integer,allocatable,dimension(:) :: rDataRec  ! receiving data size
  integer,allocatable,dimension(:) :: linkls_s  ! receiving data size
  integer,allocatable,dimension(:) :: linkls_e  ! receiving data size
  integer,allocatable,dimension(:) :: ToInd  ! size of toInd

  integer ::  numprocs
  integer, allocatable, dimension(:) :: LLINKIDINDX, aLinksl
  integer :: LLINKLEN, gNlinksl, tmpnlinksl, l_nlinksl, max_nlinkSL

  contains


  subroutine updateLinkV8_mem(LinkV, outV)
! for big memory data
     implicit none
     real, dimension(:) :: outV
     real*8, dimension(:) :: LinkV
     real, allocatable, dimension(:) :: gLinkV_r4
     real*8, allocatable,dimension(:) ::  tmpBuf, gLinkV_r8
     integer :: ierr, i, tag, k,m,lsize
     integer, allocatable,dimension(:) :: lindex
     if(my_id .eq. io_id) then
           allocate(gLinkV_r4(gnlinksl))
           allocate(gLinkV_r8(gnlinksl))
           gLinkV_r4 = 0.0
           gLinkV_r8 = 0.0
           do i = 1, LLINKLEN
               gLinkV_r8(LLINKIDINDX(i)) = LinkV(i)
           end do
     endif

     if(my_id .ne. IO_id) then
        
          tag = 101
          call mpi_send(LLINKLEN,1,MPI_INTEGER, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
          if(LLINKLEN .gt. 0) then
              tag = 102
              call mpi_send(LLINKIDINDX,LLINKLEN,MPI_INTEGER, IO_id,     &
                    tag,HYDRO_COMM_WORLD,ierr)
              tag = 103
              call mpi_send(LinkV,LLINKLEN,MPI_DOUBLE_PRECISION, IO_id,     &
                   tag,HYDRO_COMM_WORLD,ierr)
          endif
      else   
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
                tag = 101
                call mpi_recv(lsize,1,MPI_INTEGER, i,     &
                            tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                if(lsize .gt. 0) then
                      allocate(lindex(lsize) )
                      allocate(tmpBuf(lsize) )
                      tag = 102
                      call mpi_recv(lindex,lsize,MPI_INTEGER, i,     &
                            tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      tag = 103
                      call mpi_recv(tmpBuf,lsize,&
                            MPI_DOUBLE_PRECISION,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      do k = 1, lsize
                          gLinkV_r8(lindex(k)) = gLinkV_r8(lindex(k)) + tmpBuf(k)
                      end do
                      if(allocated(lindex)) deallocate(lindex)
                      if(allocated(tmpBuf)) deallocate(tmpBuf)
               endif
            end if
          end do
          gLinkV_r4 = gLinkV_r8
          if(allocated(gLinkV_r8)) deallocate(gLinkV_r8)
      end if 

      call ReachLS_decompReal(gLinkV_r4,outV)

      if(my_id .eq. io_id) then
         if(allocated(gLinkV_r4))  deallocate(gLinkV_r4)
      endif
  end subroutine updateLinkV8_mem

  subroutine updateLinkV4_mem(LinkV, outV)
! for big memory data
     implicit none
     real, dimension(:) :: outV
     real, dimension(:) :: LinkV
     real, allocatable, dimension(:) :: gLinkV_r4
     real, allocatable,dimension(:) ::  tmpBuf
     integer :: ierr, i, tag, k,m,lsize
     integer, allocatable,dimension(:) :: lindex
     if(my_id .eq. io_id) then
           allocate(gLinkV_r4(gnlinksl))
           gLinkV_r4 = 0.0
           do i = 1, LLINKLEN
               gLinkV_r4(LLINKIDINDX(i)) = LinkV(i)
           end do
     endif

     if(my_id .ne. IO_id) then
        
          tag = 101
          call mpi_send(LLINKLEN,1,MPI_INTEGER, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
          if(LLINKLEN .gt. 0) then
              tag = 102
              call mpi_send(LLINKIDINDX,LLINKLEN,MPI_INTEGER, IO_id,     &
                    tag,HYDRO_COMM_WORLD,ierr)
              tag = 103
              call mpi_send(LinkV,LLINKLEN,MPI_REAL, IO_id,     &
                   tag,HYDRO_COMM_WORLD,ierr)
          endif
      else   
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
                tag = 101
                call mpi_recv(lsize,1,MPI_INTEGER, i,     &
                            tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                if(lsize .gt. 0) then
                      allocate(lindex(lsize) )
                      allocate(tmpBuf(lsize) )
                      tag = 102
                      call mpi_recv(lindex,lsize,MPI_INTEGER, i,     &
                            tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      tag = 103
                      call mpi_recv(tmpBuf,lsize,&
                            MPI_REAL,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      do k = 1, lsize
                          gLinkV_r4(lindex(k)) = gLinkV_r4(lindex(k)) + tmpBuf(k)
                      end do
                      if(allocated(lindex)) deallocate(lindex)
                      if(allocated(tmpBuf)) deallocate(tmpBuf)
               endif
            end if
          end do
      end if 

      call ReachLS_decompReal(gLinkV_r4,outV)

      if(my_id .eq. io_id) then
          if(allocated(gLinkV_r4)) deallocate(gLinkV_r4)
      endif
  end subroutine updateLinkV4_mem


  subroutine updateLinkV8(LinkV, outV)
     implicit none
     real, dimension(:) :: outV 
     real*8, dimension(:) :: LinkV
     real*8, dimension(gNlinksl) :: gLinkV,gLinkV_r
     real, dimension(gNlinksl) :: gLinkV_r4
     integer :: ierr, i, tag
     gLinkV = 0.0
     gLinkV_r = 0.0
     do i = 1, LLINKLEN 
         gLinkV(LLINKIDINDX(i)) = LinkV(i)
     end do

     if(my_id .ne. IO_id) then
          tag = 102
          call mpi_send(gLinkV,gnlinksl,MPI_DOUBLE_PRECISION, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
      else
          gLinkV_r = gLinkV 
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               tag = 102
               call mpi_recv(gLinkV,gnlinksl,&
                   MPI_DOUBLE_PRECISION,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               gLinkV_r = gLinkV_r + gLinkV
            end if
          end do
      end if
      gLinkV_r4 = gLinkV_r

      call ReachLS_decompReal(gLinkV_r4,outV)      
  end subroutine updateLinkV8

  subroutine updateLinkV4(LinkV, outV)
     implicit none
     real, dimension(:) :: outV  
     real, dimension(:) :: LinkV
     real, dimension(gNlinksl) :: gLinkV,gLinkV_r
     real, dimension(gNlinksl) :: gLinkV_r4
     integer :: ierr, i, tag
     gLinkV = 0.0 
     gLinkV_r = 0.0
     do i = 1, LLINKLEN
         gLinkV(LLINKIDINDX(i)) = LinkV(i)
     end do

     if(my_id .ne. IO_id) then
          tag = 102
          call mpi_send(gLinkV,gnlinksl,MPI_REAL, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
      else
          gLinkV_r = gLinkV    
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               tag = 102
               call mpi_recv(gLinkV,gnlinksl,&
                   MPI_REAL,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               gLinkV_r = gLinkV_r + gLinkV
            end if
          end do
      end if
      gLinkV_r4 = gLinkV_r
      call ReachLS_decompReal(gLinkV_r4,outV)
  end subroutine updateLinkV4

  subroutine gbcastReal(inV, outV)
     implicit none
     real, dimension(:) :: outV
     real, dimension(:) :: inV  
     integer :: ierr
     call ReachLS_write_io(inV,outV)
     call mpi_bcast(outV(1:gnlinksl),gnlinksl,MPI_REAL,   &
            IO_id,HYDRO_COMM_WORLD,ierr) 
  end subroutine gbcastReal

  subroutine gbcastReal2_old(index,size1,inV, insize, outV)
     implicit none
     integer :: size1, insize
     integer,dimension(:) :: index
     real, dimension(:) :: outV
     real, dimension(:) :: inV  
     real, dimension(max_nlinkSL) :: tmpV  
     integer :: ierr, k, i, m, j, bsize
     outV = 0
     do i = 0, numprocs -1
            bsize = linkls_e(i+1) - linkls_s(i+1) + 1
         if(linkls_e(i+1) .gt. 0) then
            if(my_id .eq. i) tmpV(1:bsize) = inV(1:bsize)
            call mpi_bcast(tmpV(1:bsize),bsize,MPI_REAL,   &
                i,HYDRO_COMM_WORLD,ierr) 
            do j = 1, size1
                do k = 1, bsize
                   if(index(j) .eq. (linkls_s(i+1) + k -1) ) then
                      outV(j) = tmpV(k)
                      goto  100
                   endif
                end do
 100            continue
            end do

         endif
     end do
  end subroutine gbcastReal2_old

  subroutine gbcastReal2(index,size1,inV, insize, outV)
     implicit none
     integer :: size1, insize
     integer,dimension(:) :: index
     real, dimension(:) :: outV
     real, dimension(:) :: inV
!     real, dimension(max_nlinkSL) :: tmpV
     real, dimension(gnlinksl) :: gbuf
     integer :: ierr, k, i, m, j, bsize
     outV = 0
     call ReachLS_write_io(inV,gbuf)
     call mpi_bcast(gbuf,gnlinksl,MPI_REAL,   &
            IO_id,HYDRO_COMM_WORLD,ierr) 
     do j = 1, size1
        outV(j) = gbuf(index(j))
     end do
  end subroutine gbcastReal2




  subroutine gbcastInt(inV, outV)
     implicit none
     integer, dimension(:) :: outV
     integer, dimension(:) :: inV  
     integer :: ierr
     call ReachLS_write_io(inV,outV)
     call mpi_bcast(outV(1:gnlinksl),gnlinksl,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr) 
  end subroutine gbcastInt 


  subroutine getLocalIndx(glinksl,LINKID, LLINKID)
       implicit none
       integer, dimension(:) :: LINKID, LLINKID
       integer :: i,k, glinksl, ierr
       integer :: gLinkId(glinksl)
       integer :: cache_idx, cache_block
       integer :: cache_block_begin, cache_block_end

       LLINKLEN = size(LLINKID,1)
       allocate(LLINKIDINDX(LLINKLEN))
       LLINKIDINDX = 0
       gNlinksl = glinksl

       call ReachLS_write_io(LINKID,gLinkId)

       call mpi_bcast(gLinkId(1:glinksl),glinksl,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)

       cache_block = 256
       do cache_idx = 0, glinksl - 1, cache_block
          cache_block_begin = cache_idx + 1
          cache_block_end = min(cache_idx + cache_block, glinksl)
          do i = 1, LLINKLEN
             if(LLINKIDINDX(i) /= 0) cycle
             do k = cache_block_begin, cache_block_end
                if(LLINKID(i) .eq. gLinkId(k)) then
                   LLINKIDINDX(i) = k
                endif
             end do
          end do
       end do
       
       call mpp_land_sync()
  end subroutine getLocalIndx

  subroutine ReachLS_ini(glinksl,nlinksl,linklsS, linklsE)
     implicit none
     integer, intent(in) :: glinksl
     integer, intent(out) :: nlinksl, linklsS, linklsE
     integer :: i, ii, ierr

! get my_id and numprocs 
     call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_id, ierr )
     call MPI_COMM_SIZE( HYDRO_COMM_WORLD, numprocs, ierr )


     nlinksl = glinksl / numprocs 
     allocate(linkls_s(numprocs))
     allocate(linkls_e(numprocs))
     allocate(aLinksl(numprocs))
     allocate(ToInd(numprocs))
     
     ToInd = -1

     linkls_s(1) = 1
     linkls_e(1) = nlinksl
     aLinksl = nlinksl

     do i = 2, mod(glinksl, numprocs)+1
         aLinksl(i) = aLinksl(i) + 1
     end do
     do i = 2, numprocs
        linkls_s(i) = linkls_e(i-1)+1
        linkls_e(i) = linkls_s(i) + aLinksl(i)-1
     end do

     nlinksl = aLinksl(my_id+1) 
     
     linklsS = linkls_s(my_id+1)
     linklsE = linkls_e(my_id+1)
     tmpnlinksl = aLinksl(my_id+1) 
     l_nlinksl = nlinksl

     max_nlinksl = l_nlinksl
     call mpp_land_max_int1(max_nlinksl)
     
     gNlinksl = glinksl
  end subroutine ReachLS_ini
  
  subroutine MapGrid2ReachIni(in2d)
     implicit none
     integer, intent(in),dimension(:,:) :: in2d 
     integer :: ix, jx, i,j,n,ntotal, ierr
     integer, dimension(numprocs) :: tmpS

     allocate(sDataRec(numprocs))
     allocate(rDataRec(numprocs))
     
     ntotal = 0
     sDataRec = 0
     rDataRec = 0
     ix = size(in2d,1)
     jx = size(in2d,2)
     do j = 1, jx
        do i = 1, ix
           if(in2d(i,j) .gt. 0) then
              do n = 1, numprocs
                  if((in2d(i,j) .ge. linkls_s(n)) .and. (in2d(i,j) .le. linkls_e(n)) ) then
                              sDataRec(n) = sDataRec(n) + 1
                  endif
              end do
           endif
        enddo
     enddo   

     do n = 1, numprocs
         if(my_id .eq. n-1) then
             tmpS = sDataRec
         endif
         call mpi_bcast(tmpS,numprocs,MPI_INTEGER,   &
            n-1,HYDRO_COMM_WORLD,ierr)
         rDataRec(n) = tmpS(n)
     enddo
    
  end subroutine MapGrid2ReachIni


  subroutine ReachLS_decompReal(inV,outV)
      implicit none
      real,INTENT(in),dimension(:) :: inV
      real,INTENT(out),dimension(:) :: outV
      integer ::  i, ierr, tag
      tag = 11
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(1:(linkls_e(i)-linkls_s(i)+1) ) = inV(linkls_s(i):linkls_e(i))
                endif
            else
                if(aLinksl(i) .gt. 0) then
                    call mpi_send(inV(linkls_s(i):linkls_e(i)), &
                        aLinksl(i), &
                        MPI_REAL, i-1 ,tag,HYDRO_COMM_WORLD,ierr)
                endif
            endif
         end do
      else
         if(aLinksl(my_id+1) .gt. 0) then
             call mpi_recv(outV(1:(linkls_e(my_id+1)-linkls_s(my_id+1)+1) ), &  !! this one has +1!
              aLinksl(my_id+1),                                        &
              MPI_REAL, io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
         endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_decompReal

  subroutine ReachLS_decompReal8(inV,outV)
      implicit none
      real*8,intent(in),dimension(:) :: inV
      real*8,intent(out),dimension(:) :: outV
      integer ::  i, ierr, tag
      tag = 11
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(1:(linkls_e(i)-linkls_s(i)+1) ) = inV(linkls_s(i):linkls_e(i))
                endif
            else
                if(aLinksl(i) .gt. 0) then
                    call mpi_send(inV(linkls_s(i):linkls_e(i)), &
                        aLinksl(i), &
                        MPI_REAL8, i-1 ,tag,HYDRO_COMM_WORLD,ierr)
                endif
            endif
         end do
      else
         if(aLinksl(my_id+1) .gt. 0) then
             call mpi_recv(outV(1:(linkls_e(my_id+1)-linkls_s(my_id+1)+1) ), &  !! this one has +1!
              aLinksl(my_id+1),                                        &
              MPI_REAL8, io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
         endif
      endif
      call mpp_land_sync()
      end subroutine ReachLS_decompReal8

  subroutine ReachLS_decompInt(inV,outV)
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,INTENT(out),dimension(:) :: outV
      integer ::  i, ierr, tag
      tag = 11
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                    outV(1:linkls_e(i)-linkls_s(i)+1) = inV(linkls_s(i):linkls_e(i))
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  call mpi_send(inV(linkls_s(i):linkls_e(i)), &
                      aLinksl(i),                &
                      MPI_INTEGER, i-1,tag,HYDRO_COMM_WORLD,ierr)
               endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               call mpi_recv(outV(1:linkls_e(my_id+1)-linkls_s(my_id+1)+1), &
                    alinksl(my_id+1),                           &
                    MPI_INTEGER, io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
          endif
     endif

      call mpp_land_sync()

  END subroutine ReachLS_decompInt


  subroutine ReachLS_decompChar(inV,outV)
     implicit none
     character(len=*),intent(in), dimension(:) :: inV
     character(len=*),intent(out),dimension(:) :: outV
     integer ::  i, ierr, tag
     integer :: strLen
     strLen = len(inV(1))
     tag = 11
     if(my_id .eq. io_id) then
        do i = 1, numprocs
           if(i-1 .eq. io_id) then
              if(alinksl(i) .gt. 0) then
                 outV(1:(linkls_e(i)-linkls_s(i)+1)) = inV(linkls_s(i):linkls_e(i))
              endif
           else
              if(aLinksl(i) .gt. 0) then
                 ! The mpi_send takes what you give it and THEN treats each caracter as an array element.
                 call mpi_send(inV(linkls_s(i):linkls_e(i)),       &
                      strLen*aLinksl(i),                           &
                      MPI_CHARACTER, i-1, tag, HYDRO_COMM_WORLD, ierr)
              endif
           endif
        end do
     else
        if(aLinksl(my_id+1) .gt. 0) then
           ! The mpi_recv treats each caracter as an array element.
           call mpi_recv(outV(1 : (linkls_e(my_id+1)-linkls_s(my_id+1)+1) ), &  !jlm should have +1
                strLen*alinksl(my_id+1),                                              &
                MPI_CHARACTER, io_id, tag, HYDRO_COMM_WORLD, mpp_status,ierr          )
        endif
     endif
     call mpp_land_sync()
  end subroutine ReachLS_decompChar
     
     
  subroutine ReachLS_wReal(inV,outV)
      implicit none
      real,INTENT(in),dimension(:) :: inV
      real,INTENT(out),dimension(:) :: outV
      integer :: i, ierr, tag, ss  , mm
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            tag = 12
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
                if(aLinksl(i) .gt. 0) then

                    call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                         aLinksl(i),                            &
                         MPI_REAL,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               tag = 12
               ss = size(inv,1)
               call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_REAL,io_id,tag,HYDRO_COMM_WORLD,ierr)
          endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_wReal

  subroutine ReachLS_wReal8(inV,outV)
      implicit none
      real*8,intent(in),dimension(:)  :: inV
      real*8,intent(out),dimension(:) :: outV
      integer :: i, ierr, tag, ss  , mm
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            tag = 12
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
                if(aLinksl(i) .gt. 0) then

                    call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                         aLinksl(i),                            &
                         MPI_REAL8,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               tag = 12
               ss = size(inv,1)
               call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_REAL8,io_id,tag,HYDRO_COMM_WORLD,ierr)
          endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_wReal8


  subroutine ReachLS_wInt(inV,outV)
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,INTENT(out),dimension(:) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  tag = 12
                  call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                       aLinksl(i),                             &
                       MPI_INTEGER,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               endif
            endif
         end do
      else
           if(aLinksl(my_id+1) .gt. 0) then
                tag = 12
                call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,ierr)
           endif
      endif
      call mpp_land_sync()
  END subroutine ReachLS_wInt 


  subroutine ReachLS_wInt2(inV,outV,len,glen)
      implicit none
      integer  :: len, glen
      integer,INTENT(in),dimension(len) :: inV
      integer,INTENT(out),dimension(glen) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
               if(aLinksl(i) .gt. 0) then
                  tag = 12
                  call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                       aLinksl(i),                             &
                       MPI_INTEGER,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               endif
            endif
         end do
      else
           if(aLinksl(my_id+1) .gt. 0) then
                tag = 12
                call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,ierr)
           endif
      endif  
      call mpp_land_sync()
  END subroutine ReachLS_wInt2

  subroutine ReachLS_wReal2(inV,outV,len,glen)
      implicit none
      integer :: len, glen
      real,INTENT(in),dimension(len) :: inV
      real,INTENT(out),dimension(glen) :: outV
      integer :: i, ierr, tag
      outV = 0
      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                if(alinksl(i) .gt. 0) then
                   outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
                endif
            else
                if(aLinksl(i) .gt. 0) then
                    tag = 12
                    call mpi_recv(outV(linkls_s(i):linkls_e(i)), &
                         aLinksl(i),                            &
                         MPI_REAL,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else
          if(aLinksl(my_id+1) .gt. 0) then
               tag = 12
               call mpi_send(inV(1:aLinksl(my_id+1) ), &
                      aLinksl(my_id+1),                      &
                      MPI_REAL,io_id,tag,HYDRO_COMM_WORLD,ierr)
          endif
      endif
      call mpp_land_sync()   
  END subroutine ReachLS_wReal2

  subroutine ReachLS_wChar(inV,outV)
     implicit none
     character(len=*), intent(in), dimension(:)  :: inV
     character(len=*) ,intent(out),dimension(:) :: outV
     integer :: i, ierr, tag
     integer :: strLen
     strLen = len(inV(1))
     if(my_id .eq. io_id) then
        do i = 1, numprocs
           if(i-1 .eq. io_id) then
              if(alinksl(i) .gt. 0) then
                 outV(linkls_s(i):linkls_e(i)) = inV(1:linkls_e(i)-linkls_s(i)+1)
              endif
           else
              if(aLinksl(i) .gt. 0) then
                 tag = 12
                 ! ? seems asymmetric with ReachLS_decompChar
                 call mpi_recv(outV( linkls_s(i) : linkls_e(i) ), &
!                 call mpi_recv(outV( ((linkls_s(i)-1)+1) : (linkls_e(i)) ), &
                      aLinksl(i),                                                  &
                      MPI_CHARACTER, i-1, tag, HYDRO_COMM_WORLD, mpp_status, ierr           )
              endif
           endif
        end do
     else
        if(aLinksl(my_id+1) .gt. 0) then
           tag = 12
           ! The mpi_send takes what you give it and THEN treats each caracter as an array element.
           call mpi_send(inV(1:aLinksl(my_id+1)),              &
                aLinksl(my_id+1),                       &
                MPI_CHARACTER, io_id, tag, HYDRO_COMM_WORLD, ierr)
        endif
     endif
     call mpp_land_sync()   
  end subroutine ReachLS_wChar


  subroutine getFromInd(linkid,to,ind,indLen)
      integer,dimension(:) :: linkid, to
      integer, allocatable, dimension(:) ::ind
      integer :: k, m, kk, mm,indLen
      integer, dimension(gnlinksl) :: glinkid   
      call ReachLS_write_io(linkid,glinkid)
      mm = size(to,1)
      kk = 0
      do k = 1, gnlinksl
          do m = 1, mm
             if(glinkid(k) .eq. to(m) ) then
                 kk = kk +1
                 goto 2001
             endif
          end do
2001      continue
      end do
      allocate(ind(kk))
      kk = 0
      do k = 1, gnlinksl
          do m = 1, mm
             if(glinkid(k) .eq. to(m) ) then
                 kk = kk +1
                 ind(kk) = glinkid(k)
                 goto 2002
             endif
          end do
2002      continue
      end do
      indLen = kk      

  end subroutine getFromInd

  subroutine getToInd(from,to,ind,indLen,gToNodeOut)
      integer,dimension(:) :: from, to
      integer, allocatable, dimension(:) ::ind
      integer, allocatable, dimension(:,:) :: gToNodeOut
      integer :: k, m, kk, mm,indLen, i, ierr
      integer, dimension(gnlinksl) :: gto
      integer :: maxNum, num, tmp_num
      integer :: cache_block_begin, cache_block_end, cache_idx, cache_block
      integer, allocatable :: num_a(:)

      call gBcastValue(to,gto)

!      mm = size(from,1)
       mm = l_nlinksl

      allocate(num_a(mm))
      cache_block = 256
      num_a = 0
      tmp_num = 0
      kk = 0
      maxNum = 0
      do cache_idx = 0, gnlinksl-1, cache_block
         cache_block_begin = cache_idx + 1
         cache_block_end = min(cache_idx + cache_block, gnlinksl)
         do m = 1, mm
            tmp_num = num_a(m)
            do k = cache_block_begin, cache_block_end
               if(gto(k) .eq. from(m) ) then
                  kk = kk + 1
                  tmp_num = tmp_num + 1
               endif
            end do
            num_a(m) = tmp_num
            num = tmp_num
            if(num .gt. maxNum) maxNum = num
         end do
      enddo

      allocate(ind(kk))
      allocate(gToNodeOut(mm,maxNum+1))
      gToNodeOut = -99

      indLen = kk

      kk = 0
      num_a = 1
      tmp_num = 1
      do cache_idx = 0, gnlinksl-1, cache_block
         cache_block_begin = cache_idx + 1
         cache_block_end = min(cache_idx + cache_block, gnlinksl)
         do m = 1, mm
            tmp_num = num_a(m)
            do k = cache_block_begin, cache_block_end
               if(gto(k) .eq. from(m) ) then
                  kk = kk +1
                  !yw ind(kk) = gto(k)
                  ind(kk) = k
                  !! gToNodeOut(m,num+1) = gto(k)
                  gToNodeOut(m,tmp_num+1) = kk
                  gToNodeOut(m,1) = tmp_num
                  tmp_num = tmp_num + 1
               endif
            end do
            num_a(m) = tmp_num
         end do
      enddo
      deallocate(num_a)

      ToInd(my_id+1) = kk
      do i = 0, numprocs - 1
         call mpi_bcast(ToInd(i+1),1,MPI_INTEGER,   &
            i,HYDRO_COMM_WORLD,ierr)
      end do

  end subroutine getToInd

  subroutine com_decomp1dInt(inV,gsize,outV,lsize)
!     output outV and lsize
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,allocatable,dimension(:) :: outV
      integer ::  i, ierr, tag, imod, ncomsize
      integer :: lsize, ssize,start, gsize, end
      tag = 19
      ncomsize = gsize/numprocs
      imod = mod(gsize,numprocs)


      if(my_id .eq. io_id) then
         start = 0
         end = 0
         do i = 1, numprocs
            if(i-1 .lt. imod) then
                  ssize = ncomsize + 1
            else
                  ssize = ncomsize 
            endif
 
            start = end + 1 
            end = start + ssize - 1

            if(i-1 .eq. io_id) then
                if(ssize .gt. 0) then
                    allocate(outV(ssize) )
                    outV(1:ssize) = inV(1:ssize)
                    lsize = ssize
                else
                    lsize = 0    
                endif
            else
                if(ssize .gt. 0 ) then  
                   call mpi_send(inV(start:start+ssize-1), ssize,       &
                      MPI_INTEGER, i-1,tag,HYDRO_COMM_WORLD,ierr)
                endif
            endif
         end do
      else   
              if(my_id .lt. imod) then
                   lsize = ncomsize + 1
              else
                   lsize = ncomsize 
              endif
              if( lsize .gt. 0) then
                  allocate(outV(lsize) )
                  call mpi_recv(outV,lsize,                           &
                        MPI_INTEGER, io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
              endif
      endif
      call mpp_land_sync()


  END subroutine com_decomp1dInt

  subroutine com_write1dInt(inV,lsize,outV,gsize)
!     output outV and lsize
      implicit none
      integer,INTENT(in),dimension(:) :: inV
      integer,dimension(:) :: outV
      integer ::  i, ierr, tag, imod, ncomsize
      integer :: lsize, rsize,start, gsize, end
      tag = 18
      ncomsize = gsize/numprocs
      imod = mod(gsize,numprocs)

      if(my_id .eq. io_id) then
            start = 0
            end = 0
         do i = 1, numprocs
            if(i-1 .lt. imod) then
                  rsize = ncomsize + 1
            else
                  rsize = ncomsize 
            endif

            start = end + 1 
            end = start + rsize - 1

            if(i-1 .eq. io_id) then
                if(rsize .gt. 0) then
                    outV(1:rsize) = inV(1:rsize)
                endif
            else
                if(rsize .gt. 0 ) then  
                  call mpi_recv(outV(start:start+rsize-1), rsize,          &
                        MPI_INTEGER, i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                endif
            endif
         end do
      else   
              if(my_id .lt. imod) then
                   lsize = ncomsize + 1
              else
                   lsize = ncomsize 
              endif
              if( lsize .gt. 0) then
                   call mpi_send(inV, lsize,       &
                      MPI_INTEGER, io_id,tag,HYDRO_COMM_WORLD,ierr)
              endif
      endif

      call mpp_land_sync()

  END subroutine com_write1dInt 

  subroutine pack_decomp_int(g1bufid, ndata, nprocs_map, lnsizes, istart,bufid)
     implicit none
     integer :: ndata
     integer, dimension(:) :: g1bufid,  nprocs_map, lnsizes, bufid
     integer :: i,j,k, tag, ierr
     integer, allocatable,dimension(:) :: buf
     integer, dimension(:) :: istart
     integer, dimension(numprocs) :: count
     ! pack data


     if(my_id .eq. io_id) then
         allocate(buf(ndata))
         count = 0
         do i = 1, ndata
            k = nprocs_map(i)
            if( k .gt. 0) then
               buf(istart(k) + count(k)) = g1bufid(i) 
               count(k) = count(k) + 1
            end if
         end do
!         write(6,*) " count = ", count
!         write(6,*) " istart = ", istart
!         write(6,*) " lnsizes = ", lnsizes
      end if
      !finish packing 

      call mpp_land_sync()
!      call hydro_finish()

      if(my_id .ne. IO_id) then
          tag = 72
          if(lnsizes(my_id + 1) .gt. 0) then
             call mpi_recv(bufid,lnsizes(my_id + 1),&
                   MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
          endif
      else
          do i = 0, numprocs - 1
            if(i .ne. my_id) then
               tag = 72
               if(lnsizes(i+1) .gt. 0) then
                  call mpi_send(buf(istart(i+1):istart(i+1)+lnsizes(i+1)-1),  &
                      lnsizes(i+1),MPI_INTEGER,i, tag,HYDRO_COMM_WORLD,ierr)
               endif
            else
                if(lnsizes(i+1) .gt. 0) then
                   bufid = buf(istart(i+1):istart(i+1)+lnsizes(i+1)-1)
                endif
            end if
          end do
       end if
       if(my_id .eq. io_id) then 
          if(allocated(buf)) deallocate(buf)
       endif
  end subroutine pack_decomp_int

  subroutine pack_decomp_real8(g1bufid, ndata, nprocs_map, lnsizes, istart,bufid)
     implicit none
     integer :: ndata
     real*8, dimension(:) :: g1bufid, bufid
     integer,dimension(:) ::  nprocs_map, lnsizes 
     integer :: i,j,k, tag, ierr
     real*8, allocatable,dimension(:) :: buf
     integer, dimension(:) :: istart
     integer, dimension(numprocs) :: count
     ! pack data
     if(my_id .eq. io_id) then
         allocate(buf(ndata))
         count = 0
         do i = 1, ndata
            k = nprocs_map(i)
            if( k .gt. 0) then
              buf(istart(k) + count(k)) = g1bufid(i) 
              count(k) = count(k) + 1
            endif
         end do
      end if
       call mpp_land_sync()
      if(my_id .ne. IO_id) then
          tag = 72
          if(lnsizes(my_id + 1) .gt. 0) then
             call mpi_recv(bufid,lnsizes(my_id + 1),&
                   MPI_DOUBLE_PRECISION,io_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
          endif
      else
          do i = 0, numprocs - 1
            if(i .ne. my_id) then
               tag = 72
               if(lnsizes(i+1) .gt. 0) then
                  call mpi_send(buf(istart(i+1):istart(i+1)+lnsizes(i+1)-1),  &
                      lnsizes(i+1),MPI_DOUBLE_PRECISION,i, tag,HYDRO_COMM_WORLD,ierr)
               endif
            else
                if(lnsizes(my_id + 1) .gt. 0) then
                   bufid = buf(istart(i + 1):istart(i+1)+lnsizes(i+1)-1)
                endif
            end if
          end do
       end if
     if(my_id .eq. io_id) then 
         if(allocated(buf))  deallocate(buf)
     endif
  end subroutine pack_decomp_real8

! this is used for nhdPlus with Lake. 
! resolve the data from TO_NODE grids, and update back to NLINKSL grids.
  subroutine TONODE2RSL (ind,inVar,size,gNLINKSL,NLINKSL,ioVar,flag)
    implicit none
    integer,intent(in) :: size,gNLINKSL,NLINKSL  ! 
    integer,intent(in) , dimension(size) :: ind, inVar
    integer,intent(inout), dimension(nlinksl) :: ioVar
    integer, allocatable, dimension(:) :: gvar, buf, tmpInd
    integer :: i,j,k, tag, ierr, tmpSize, flag

    if(gNLINKSL .le. 0) return

    if(my_id .eq. io_id) then
       allocate(gvar(gNLINKSL))
    else
       allocate(gvar(1))
    endif
    call ReachLS_wInt(ioVar,gvar)

      if(my_id .eq. io_id) then
         do i = 1, numprocs
            if(i-1 .eq. io_id) then
                do k = 1, size
                   if(inVar(k) .ne. flag) then
                      gvar(ind(k)) = inVar(k)
                   endif
                end do
            else
                  tag = 82
                  call mpi_recv(tmpSize,1,MPI_INTEGER,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                  if(tmpSize .gt. 0) then
                      allocate(buf(tmpSize))
                      allocate(tmpInd(tmpSize))
                      tag = 83
                      call mpi_recv(tmpInd, tmpSize , &
                           MPI_INTEGER,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      tag = 84
                      call mpi_recv(buf, tmpSize , &
                           MPI_INTEGER,i-1,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                      do k = 1, tmpSize
                         if(buf(k) .ne. flag) then
                             gvar(tmpInd(k)) = buf(k)
                         endif
                      end do
                      if(allocated(buf))  deallocate(buf)
                      if(allocated(tmpInd)) deallocate(tmpInd)
                  endif
            endif
         end do
      else   
          tag = 82
          call mpi_send(size,1,MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,ierr)
          if(size .gt. 0) then
             tag = 83
             call mpi_send(ind(1:size),size, &
                 MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,ierr)
             tag = 84
             call mpi_send(inVar(1:size),size, &
                 MPI_INTEGER,io_id,tag,HYDRO_COMM_WORLD,ierr)
          endif
      endif 
      call ReachLS_decomp(gvar, ioVar)
      if(allocated(gvar)) deallocate(gvar)
  end subroutine TONODE2RSL 


END MODULE MODULE_mpp_ReachLS


