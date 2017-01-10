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
MODULE MODULE_mpp_GWBUCKET

  use module_mpp_land, only:  io_id, my_id, mpp_status, mpp_land_max_int1, numprocs, &
                 mpp_land_bcast_real, sum_real8,  mpp_land_sync
  implicit none

  

  include "mpif.h"

  integer,allocatable,dimension(:) :: sizeInd  ! size of Basins for each tile
  integer ::  maxSizeInd

  integer :: gw_ini

  contains

  subroutine gwbucket_ini()
     allocate(sizeInd(numprocs))
     sizeInd = 0
     gw_ini = 99
     maxSizeInd = 0
  end subroutine gwbucket_ini

 
  subroutine collectSizeInd(numbasns)
     implicit none
     integer, intent(in) :: numbasns
     integer :: i, ierr, tag, rcv 

      call mpp_land_sync()

     if(gw_ini .ne. 99) call gwbucket_ini()

     if(my_id .ne. IO_id) then
          tag = 66 
          call mpi_send(numbasns,1,MPI_INTEGER, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
     else
          do i = 0, numprocs - 1
              if(i .eq. IO_id) then
                 sizeInd(i+1) = numbasns 
              else
                 tag = 66
                 call mpi_recv(rcv,1,&
                     MPI_INTEGER,i,tag,MPI_COMM_WORLD,mpp_status,ierr)

                 sizeInd(i+1) = rcv
              end if
              if(sizeInd(i+1) .gt. maxSizeInd) maxSizeInd = sizeInd(i+1)
          end do
      end if
  end subroutine collectSizeInd

  subroutine gw_write_io_real(numbasns,inV,ind,outV)
     implicit none
     integer, intent(in) :: numbasns
     integer :: i, ierr, tag, tag2,k
     real,intent(in), dimension(numbasns) :: inV
     integer,intent(in), dimension(numbasns) :: ind
     real, dimension(:) :: outV
     real, allocatable,dimension(:) :: vbuff
     integer, allocatable,dimension(:) :: ibuff

     if(gw_ini .ne. 99) then
        stop "FATAL ERROR: mpp_GWBUCKET not initialized."        
     endif 

     if(my_id .eq. IO_id) then
         outV = 0.0
         allocate(vbuff(maxSizeInd))
         allocate(ibuff(maxSizeInd))
     else
         allocate(vbuff(1))
         allocate(ibuff(1))
     endif

     if(my_id .ne. IO_id) then
        if(numbasns .gt. 0) then
          tag = 62
          call mpi_send(inV,numbasns,MPI_REAL, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
          tag2 = 63
          call mpi_send(ind,numbasns,MPI_INTEGER, IO_id,     &
                tag2,MPI_COMM_WORLD,ierr)
        endif
      else

          do k = 1, numbasns
              outV(ind(k)) = inV(k)  
          end do

          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               if(sizeInd(i+1) .gt. 0) then
                  tag = 62
                  call mpi_recv(vbuff(1:sizeInd(i+1)),sizeInd(i+1),&
                      MPI_REAL,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
                  tag2 = 63
                  call mpi_recv(ibuff(1:sizeInd(i+1)),sizeInd(i+1),&
                      MPI_INTEGER,i,tag2,MPI_COMM_WORLD,mpp_status,ierr)
   
                  do k = 1, sizeInd(i+1)
                     outV(ibuff(k)) = vbuff(k) 
                  end do
               endif 
             end if
           end do
      end if
      if(allocated(ibuff)) deallocate(ibuff)
      if(allocated(vbuff)) deallocate(vbuff)
  end subroutine gw_write_io_real

  subroutine gw_write_io_int(numbasns,inV,ind,outV)
      implicit none
      integer, intent(in) :: numbasns
      integer :: i, ierr, tag, tag2,k
      integer,intent(in), dimension(numbasns) :: inV
      integer,intent(in), dimension(numbasns) :: ind
      integer, dimension(:) :: outV
      integer, allocatable,dimension(:) :: vbuff
      integer, allocatable,dimension(:) :: ibuff
 
      if(gw_ini .ne. 99) then
         stop "FATAL ERROR: mpp_GWBUCKET not initialized."        
      endif 
 
      if(my_id .eq. IO_id) then
          outV = 0.0
          allocate(vbuff(maxSizeInd))
          allocate(ibuff(maxSizeInd))
      else
          allocate(vbuff(1))
          allocate(ibuff(1))
      endif
 
      if(my_id .ne. IO_id) then
         if(numbasns .gt. 0) then
           tag = 62
           call mpi_send(inV,numbasns,MPI_INTEGER, IO_id,     &
                 tag,MPI_COMM_WORLD,ierr)
           tag2 = 63
           call mpi_send(ind,numbasns,MPI_INTEGER, IO_id,     &
                 tag2,MPI_COMM_WORLD,ierr)
         endif
       else
 
           do k = 1, numbasns
               outV(ind(k)) = inV(k)  
           end do
 
           do i = 0, numprocs - 1
             if(i .ne. IO_id) then
                if(sizeInd(i+1) .gt. 0) then
                   tag = 62
                   call mpi_recv(vbuff(1:sizeInd(i+1)),sizeInd(i+1),&
                       MPI_INTEGER,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
                   tag2 = 63
                   call mpi_recv(ibuff(1:sizeInd(i+1)),sizeInd(i+1),&
                       MPI_INTEGER,i,tag2,MPI_COMM_WORLD,mpp_status,ierr)
    
                   do k = 1, sizeInd(i+1)
                      outV(ibuff(k)) = vbuff(k) 
                   end do
                endif 
              end if
            end do
       end if
       deallocate(ibuff)
       deallocate(vbuff)
   end subroutine gw_write_io_int

  subroutine gw_decompose_real(gnumbasns,numbasns,ind,inV,outV)
     implicit none
     integer, intent(in) :: numbasns, gnumbasns
     integer :: i, ierr, tag, bas
     real,intent(in), dimension(:) :: inV
     integer,intent(in), dimension(:) :: ind
     real, dimension(:) :: outV
     real, dimension(gnumbasns) :: buff

     outV = 0
     if(gnumbasns .lt. 0) return

     if(my_id .eq. io_id) buff = inV
     call mpp_land_bcast_real(gnumbasns,buff)

     do i = 1, numbasns
        bas = ind(i)
        outV(i) = buff(bas)
     end do
  end subroutine gw_decompose_real

   subroutine gw_sum_real(vinout,nsize,gsize,ind)
       implicit none
       integer nsize,i,j,tag,ierr,gsize, k
       real*8, dimension(nsize):: vinout
       integer, dimension(nsize) :: ind
       real*8, dimension(gsize) :: vbuff

       vbuff = 0
       do k = 1, nsize
          vbuff(ind(k)) = vinout(k) 
       end do
       call sum_real8(vbuff,gsize)
       do k = 1, nsize
          vinout(k) = vbuff(ind(k)) 
       end do
    end subroutine gw_sum_real
  


end MODULE MODULE_mpp_GWBUCKET


