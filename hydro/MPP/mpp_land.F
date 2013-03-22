!#### This is a module for parallel Land model.
MODULE MODULE_MPP_LAND

  use MODULE_CPL_LAND 

  IMPLICIT NONE
  include "mpif.h"
  integer, public :: left_id,right_id,up_id,down_id,my_id
  integer, public :: left_right_np,up_down_np ! define total process in two dimensions.
  integer, public :: left_right_p ,up_down_p ! the position of the current process in the logical topography.
  integer, public :: IO_id   ! the number for IO. (Last processor for IO)
  integer, public :: global_nx, global_ny, local_nx,local_ny
  integer, public :: global_rt_nx, global_rt_ny
  integer, public :: local_rt_nx,local_rt_ny,rt_AGGFACTRT
  integer, public :: numprocs   ! total process, get by mpi initialization.

  integer mpp_status(MPI_STATUS_SIZE)

  integer  overlap_n
  integer, allocatable, DIMENSION(:), public :: local_nx_size,local_ny_size
  integer, allocatable, DIMENSION(:), public :: local_rt_nx_size,local_rt_ny_size
  integer, allocatable, DIMENSION(:), public :: startx,starty
 
  contains

  subroutine LOG_MAP2d()
    implicit none
    integer :: ierr
      call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr ) 
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr ) 

      call getNX_NY(numprocs, left_right_np,up_down_np)
      if(my_id.eq.IO_id) then 
#ifdef HYDRO_D
            write(6,*) ""
            write(6,*) "total process:",numprocs
            write(6,*) "left_right_np =", left_right_np,&
                 "up_down_np=",up_down_np
#endif
      end if

!   ### get the row and column of the current process in the logical topography.
!   ### left --> right, 0 -->left_right_np -1
!   ### up --> down, 0 --> up_down_np -1
        left_right_p = mod(my_id , left_right_np)
        up_down_p = my_id / left_right_np

!   ### get the neighbors.  -1 means no neighbor.
        down_id = my_id - left_right_np
        up_id =   my_id + left_right_np 
        if( up_down_p .eq. 0) down_id = -1
        if( up_down_p .eq. (up_down_np-1) ) up_id = -1

        left_id = my_id - 1 
        right_id = my_id + 1 
        if( left_right_p .eq. 0) left_id = -1
        if( left_right_p .eq. (left_right_np-1) ) right_id =-1
    
!    ### the IO node is the last processor.
!yw        IO_id = numprocs - 1
         IO_id = 0

! print the information for debug.

        call mpp_land_sync()

  return 
  end  subroutine log_map2d
!old subroutine MPP_LAND_INIT(flag, ew_numprocs, sn_numprocs)
  subroutine MPP_LAND_INIT()
!    ### initialize the land model logically based on the two D method. 
!    ### Call this function directly if it is nested with WRF.
    implicit none
    integer :: ierr
    integer :: ew_numprocs, sn_numprocs  ! input the processors in x and y direction.
    logical mpi_inited
     
!     left_right_np = ew_numprocs
!     up_down_np  = sn_numprocs

      CALL mpi_initialized( mpi_inited, ierr )
      if ( .NOT. mpi_inited ) then
           call MPI_INIT( ierr )  ! stand alone land model.
      else 
           call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr ) 
           call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr ) 
           return
      endif
!     create 2d logical mapping of the CPU.
      call log_map2d()

      return
  end   subroutine MPP_LAND_INIT


     subroutine MPP_LAND_PAR_INI(over_lap,in_global_nx,in_global_ny,AGGFACTRT)
        integer in_global_nx,in_global_ny, AGGFACTRT
        integer :: over_lap   ! the overlaped grid number. (default is 1)
        global_nx = in_global_nx
        global_ny = in_global_ny 
        rt_AGGFACTRT = AGGFACTRT
        global_rt_nx = in_global_nx*AGGFACTRT
        global_rt_ny = in_global_ny *AGGFACTRT
        !overlap_n = 1
        local_nx = global_nx / left_right_np 
        if(left_right_p .eq. (left_right_np-1) ) then
              local_nx = global_nx   &
                    -int(global_nx/left_right_np)*(left_right_np-1)
        end if
        local_ny = global_ny / up_down_np 
        if(  up_down_p .eq. (up_down_np-1) ) then
           local_ny = global_ny  &
                 -int(global_ny/up_down_np)*(up_down_np -1)
        end if

        local_rt_nx=local_nx*AGGFACTRT+2
        local_rt_ny=local_ny*AGGFACTRT+2
        if(left_id.lt.0) local_rt_nx = local_rt_nx -1
        if(right_id.lt.0) local_rt_nx = local_rt_nx -1
        if(up_id.lt.0) local_rt_ny = local_rt_ny -1
        if(down_id.lt.0) local_rt_ny = local_rt_ny -1

        call get_local_size(local_nx, local_ny,local_rt_nx,local_rt_ny)
        call calculate_start_p()
        
        in_global_nx = local_nx
        in_global_ny = local_ny
#ifdef HYDRO_D
        write(6,*) "my_id=",my_id,"global_rt_nx=",global_rt_nx
        write(6,*) "my_id=",my_id,"global_rt_nx=",global_rt_ny
        write(6,*) "my_id=",my_id,"global_nx=",global_nx
        write(6,*) "my_id=",my_id,"global_nx=",global_ny
#endif
        return 
        end  subroutine MPP_LAND_PAR_INI

  subroutine MPP_LAND_LR_COM(in_out_data,NX,NY,flag)
!   ### Communicate message on left right direction.
    integer NX,NY
    real in_out_data(nx,ny),data_r(2,ny)
    integer count,size,tag,  ierr
    integer flag   ! 99 replace the boundary, else get the sum.

    if(flag .eq. 99) then ! replace the data  
       if(right_id .ge. 0) then  !   ### send to right first.
           tag = 11 
           size = ny
           call mpi_send(in_out_data(nx-1,:),size,MPI_REAL,   &
             right_id,tag,MPI_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
           tag = 11
           size = ny
           call mpi_recv(in_out_data(1,:),size,MPI_REAL,  &
              left_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = ny 
          tag = 21
          call mpi_send(in_out_data(2,:),size,MPI_REAL,   &
             left_id,tag,MPI_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = ny 
          call mpi_recv(in_out_data(nx,:),size,MPI_REAL,&
             right_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
      endif

    else   ! get the sum

       if(right_id .ge. 0) then !   ### send to right first.
         tag = 11
         size = 2*ny 
         call mpi_send(in_out_data(nx-1:nx,:),size,MPI_REAL,   &
             right_id,tag,MPI_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
          tag = 11
          size = 2*ny
          call mpi_recv(data_r,size,MPI_REAL,left_id,tag, &
               MPI_COMM_WORLD,mpp_status,ierr)
          in_out_data(1,:) = in_out_data(1,:) + data_r(1,:)
          in_out_data(2,:) = in_out_data(2,:) + data_r(2,:)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = 2*ny
          tag = 21
          call mpi_send(in_out_data(1:2,:),size,MPI_REAL,   &
             left_id,tag,MPI_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = 2*ny
          call mpi_recv(in_out_data(nx-1:nx,:),size,MPI_REAL,&
             right_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
      endif
    endif   ! end if black for flag.

    return
  end subroutine MPP_LAND_LR_COM

  subroutine MPP_LAND_LR_COM8(in_out_data,NX,NY,flag)
!   ### Communicate message on left right direction.
    integer NX,NY
    real*8 in_out_data(nx,ny),data_r(2,ny)
    integer count,size,tag,  ierr
    integer flag   ! 99 replace the boundary, else get the sum.

    if(flag .eq. 99) then ! replace the data  
       if(right_id .ge. 0) then  !   ### send to right first.
           tag = 11 
           size = ny
           call mpi_send(in_out_data(nx-1,:),size,MPI_DOUBLE_PRECISION,   &
             right_id,tag,MPI_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
           tag = 11
           size = ny
           call mpi_recv(in_out_data(1,:),size,MPI_DOUBLE_PRECISION,  &
              left_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = ny 
          tag = 21
          call mpi_send(in_out_data(2,:),size,MPI_DOUBLE_PRECISION,   &
             left_id,tag,MPI_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = ny 
          call mpi_recv(in_out_data(nx,:),size,MPI_DOUBLE_PRECISION,&
             right_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
      endif

    else   ! get the sum

       if(right_id .ge. 0) then !   ### send to right first.
         tag = 11
         size = 2*ny 
         call mpi_send(in_out_data(nx-1:nx,:),size,MPI_DOUBLE_PRECISION,   &
             right_id,tag,MPI_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
          tag = 11
          size = 2*ny
          call mpi_recv(data_r,size,MPI_DOUBLE_PRECISION,left_id,tag, &
               MPI_COMM_WORLD,mpp_status,ierr)
          in_out_data(1,:) = in_out_data(1,:) + data_r(1,:)
          in_out_data(2,:) = in_out_data(2,:) + data_r(2,:)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = 2*ny
          tag = 21
          call mpi_send(in_out_data(1:2,:),size,MPI_DOUBLE_PRECISION,   &
             left_id,tag,MPI_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = 2*ny
          call mpi_recv(in_out_data(nx-1:nx,:),size,MPI_DOUBLE_PRECISION,&
             right_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
      endif
    endif   ! end if black for flag.

    return
  end subroutine MPP_LAND_LR_COM8
  
  
  subroutine get_local_size(local_nx, local_ny,rt_nx,rt_ny)
    integer local_nx, local_ny, rt_nx,rt_ny
    integer i,status,ierr, tag
    integer tmp_nx,tmp_ny
!   ### if it is IO node, get the local_size of the x and y direction 
!   ### for all other tasks.
    integer s_r(2)

!   if(my_id .eq. IO_id) then 
       allocate(local_nx_size(numprocs),stat = status) 
       allocate(local_ny_size(numprocs),stat = status) 
       allocate(local_rt_nx_size(numprocs),stat = status) 
       allocate(local_rt_ny_size(numprocs),stat = status) 
!   end if

       call mpp_land_sync()

       if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1 
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 1
                 call mpi_recv(s_r,2,MPI_INTEGER,i, & 
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                 local_nx_size(i+1) = s_r(1)
                 local_ny_size(i+1) = s_r(2)
               else
                   local_nx_size(i+1) = local_nx
                   local_ny_size(i+1) = local_ny
               end if
           end do
       else 
           tag =  1  
           s_r(1) = local_nx
           s_r(2) = local_ny
           call mpi_send(s_r,2,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
       end if

 
       if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1 
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 2
                 call mpi_recv(s_r,2,MPI_INTEGER,i, & 
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                 local_rt_nx_size(i+1) = s_r(1)
                 local_rt_ny_size(i+1) = s_r(2)
               else
                   local_rt_nx_size(i+1) = rt_nx
                   local_rt_ny_size(i+1) = rt_ny
               end if
           end do
       else 
           tag =  2  
           s_r(1) = rt_nx
           s_r(2) = rt_ny
           call mpi_send(s_r,2,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
       end if
       call mpp_land_sync()
       return 
  end  subroutine get_local_size


  subroutine MPP_LAND_UB_COM(in_out_data,NX,NY,flag)
!   ### Communicate message on up down direction.
    integer NX,NY
    real in_out_data(nx,ny),data_r(nx,2)
    integer count,size,tag, status, ierr
    integer flag  ! 99 replace the boundary , else get the sum of the boundary


    if(flag .eq. 99) then  ! replace the boundary data.

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx
           call mpi_send(in_out_data(:,ny-1),size,MPI_REAL,   &
               up_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down 
           tag = 31 
           size = nx
           call mpi_recv(in_out_data(:,1),size,MPI_REAL, &
              down_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
       endif
   
       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx
           call mpi_send(in_out_data(:,2),size,MPI_REAL,      &
                down_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper 
           tag = 41 
           size = nx
           call mpi_recv(in_out_data(:,ny),size,MPI_REAL, &
               up_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif
     
    else  ! flag = 1 

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx*2
           call mpi_send(in_out_data(:,ny-1:ny),size,MPI_REAL,   &
               up_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down
           tag = 31
           size = nx*2
           call mpi_recv(data_r,size,MPI_REAL, &
              down_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
           in_out_data(:,1) = in_out_data(:,1) + data_r(:,1)
           in_out_data(:,2) = in_out_data(:,2) + data_r(:,2)
       endif

       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx*2
           call mpi_send(in_out_data(:,1:2),size,MPI_REAL,      &
                down_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper
           tag = 41
           size = nx * 2
           call mpi_recv(in_out_data(:,ny-1:ny),size,MPI_REAL, &
               up_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif
    endif  ! end of block  flag
    return
  end  subroutine MPP_LAND_UB_COM

  subroutine MPP_LAND_UB_COM8(in_out_data,NX,NY,flag)
!   ### Communicate message on up down direction.
    integer NX,NY
    real*8 in_out_data(nx,ny),data_r(nx,2)
    integer count,size,tag, status, ierr
    integer flag  ! 99 replace the boundary , else get the sum of the boundary


    if(flag .eq. 99) then  ! replace the boundary data.

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx
           call mpi_send(in_out_data(:,ny-1),size,MPI_DOUBLE_PRECISION,   &
               up_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down 
           tag = 31 
           size = nx
           call mpi_recv(in_out_data(:,1),size,MPI_DOUBLE_PRECISION, &
              down_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
       endif
   
       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx
           call mpi_send(in_out_data(:,2),size,MPI_DOUBLE_PRECISION,      &
                down_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper 
           tag = 41 
           size = nx
           call mpi_recv(in_out_data(:,ny),size,MPI_DOUBLE_PRECISION, &
               up_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif
     
    else  ! flag = 1 

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx*2
           call mpi_send(in_out_data(:,ny-1:ny),size,MPI_DOUBLE_PRECISION,   &
               up_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down
           tag = 31
           size = nx*2
           call mpi_recv(data_r,size,MPI_DOUBLE_PRECISION, &
              down_id,tag,MPI_COMM_WORLD, mpp_status,ierr)
           in_out_data(:,1) = in_out_data(:,1) + data_r(:,1)
           in_out_data(:,2) = in_out_data(:,2) + data_r(:,2)
       endif

       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx*2
           call mpi_send(in_out_data(:,1:2),size,MPI_DOUBLE_PRECISION,      &
                down_id,tag,MPI_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper
           tag = 41
           size = nx * 2
           call mpi_recv(in_out_data(:,ny-1:ny),size,MPI_DOUBLE_PRECISION, &
               up_id,tag,MPI_COMM_WORLD,mpp_status,ierr)
       endif
    endif  ! end of block  flag
    return
  end  subroutine MPP_LAND_UB_COM8
  
  subroutine calculate_start_p()
! calculate startx and starty
    integer :: local_startx, local_starty
    integer :: i,status, ierr, tag
    integer :: r_s(2)

    allocate(starty(numprocs),stat = ierr) 
    allocate(startx(numprocs),stat = ierr)

    local_startx = int(global_nx/left_right_np) * left_right_p+1 
    local_starty = int(global_ny/up_down_np) * up_down_p+1 
    if(left_id .lt. 0) local_startx = 1
    if(down_id .lt. 0) local_starty = 1

    if(my_id .eq. IO_id) then
         startx(my_id+1) = local_startx
         starty(my_id+1) = local_starty
    end if

    r_s(1) = local_startx
    r_s(2) = local_starty
    call mpp_land_sync()

    if(my_id .eq. IO_id) then
        do i = 0, numprocs - 1
           ! block receive  from other node.
           if(i.ne.my_id) then
              tag = 1
              call mpi_recv(r_s,2,MPI_INTEGER,i, &
                   tag,MPI_COMM_WORLD,mpp_status,ierr)
              startx(i+1) = r_s(1)
              starty(i+1) = r_s(2)
           end if
        end do
     else
           tag =  1
           call mpi_send(r_s,2,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
     end if

     call mpp_land_sync()

     return
  end subroutine calculate_start_p

  subroutine decompose_data_real (in_buff,out_buff)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      real in_buff(global_nx,global_ny),out_buff(local_nx,local_ny) 
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend

      tag = 2
      if(my_id .eq. IO_id) then
         do i = 0, numprocs - 1
            ibegin = startx(i+1)
            iend   = startx(i+1)+local_nx_size(i+1) -1
            jbegin = starty(i+1)
            jend   = starty(i+1)+local_ny_size(i+1) -1
          
            if(my_id .eq. i) then
               out_buff=in_buff(ibegin:iend,jbegin:jend)
            else
               ! send data to the rest process.
               size = local_nx_size(i+1)*local_ny_size(i+1)
               call mpi_send(in_buff(ibegin:iend,jbegin:jend),size,&
                  MPI_REAL, i,tag,MPI_COMM_WORLD,ierr)
            end if
         end do
      else 
         size = local_nx*local_ny
         call mpi_recv(out_buff,size,MPI_REAL,IO_id, &
                tag,MPI_COMM_WORLD,mpp_status,ierr) 
      end if
      return
  end subroutine decompose_data_real

  subroutine decompose_data_int (in_buff,out_buff)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      integer in_buff(global_nx,global_ny),out_buff(local_nx,local_ny) 
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
 
      tag = 2
      if(my_id .eq. IO_id) then
         do i = 0, numprocs - 1
            ibegin = startx(i+1)
            iend   = startx(i+1)+local_nx_size(i+1) -1
            jbegin = starty(i+1)
            jend   = starty(i+1)+local_ny_size(i+1) -1
            if(my_id .eq. i) then
               out_buff=in_buff(ibegin:iend,jbegin:jend)
            else
               ! send data to the rest process.
               size = local_nx_size(i+1)*local_ny_size(i+1)
               call mpi_send(in_buff(ibegin:iend,jbegin:jend),size,&
                  MPI_INTEGER, i,tag,MPI_COMM_WORLD,ierr)
            end if
         end do
      else 
         size = local_nx*local_ny
         call mpi_recv(out_buff,size,MPI_INTEGER,IO_id, &
                tag,MPI_COMM_WORLD,mpp_status,ierr) 
      end if
      return
  end subroutine decompose_data_int

  subroutine write_IO_int(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      integer in_buff(1:local_nx,1:local_ny),    &
          out_buff(global_nx,global_ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_nx*local_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_INTEGER, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          do i = 0, numprocs - 1
            ibegin = startx(i+1)
            iend   = startx(i+1)+local_nx_size(i+1) -1
            jbegin = starty(i+1)
            jend   = starty(i+1)+local_ny_size(i+1) -1
            if(i .eq. IO_id) then
               out_buff(ibegin:iend,jbegin:jend) = in_buff 
            else 
               size = local_nx_size(i+1)*local_ny_size(i+1)
               tag = 2
               call mpi_recv(out_buff(ibegin:iend,jbegin:jend),size,&
                   MPI_INTEGER,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_int

  subroutine write_IO_real(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      real in_buff(1:local_nx,1:local_ny),    &
          out_buff(global_nx,global_ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_nx*local_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_REAL, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          do i = 0, numprocs - 1
            ibegin = startx(i+1)
            iend   = startx(i+1)+local_nx_size(i+1) -1
            jbegin = starty(i+1)
            jend   = starty(i+1)+local_ny_size(i+1) -1
            if(i .eq. IO_id) then
               out_buff(ibegin:iend,jbegin:jend) = in_buff 
            else 
               size = local_nx_size(i+1)*local_ny_size(i+1)
               tag = 2
               call mpi_recv(out_buff(ibegin:iend,jbegin:jend),size,&
                   MPI_REAL,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_real

  subroutine write_IO_RT_real(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      real in_buff(1:local_rt_nx,1:local_rt_ny),    &
          out_buff(global_rt_nx,global_rt_ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_rt_nx*local_rt_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_REAL, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          do i = 0, numprocs - 1
            ibegin = startx(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1) 
            if(ibegin.gt.1) ibegin=ibegin - 1
            iend   = ibegin + local_rt_nx_size(i+1) -1
            jbegin = starty(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1)
            if(jbegin.gt.1) jbegin=jbegin - 1
            jend   = jbegin + local_rt_ny_size(i+1) -1
            if(i .eq. IO_id) then
               out_buff(ibegin:iend,jbegin:jend) = in_buff 
            else 
               size = local_rt_nx_size(i+1)*local_rt_ny_size(i+1)
               tag = 2
               call mpi_recv(out_buff(ibegin:iend,jbegin:jend),size,&
                   MPI_REAL,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_RT_real


  subroutine write_IO_RT_int (in_buff,out_buff)
! the IO node will receive the data from the rest process.
      integer :: in_buff(1:local_rt_nx,1:local_rt_ny),    &
          out_buff(global_rt_nx,global_rt_ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_rt_nx*local_rt_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_INTEGER, IO_id,     &
                tag,MPI_COMM_WORLD,ierr)
      else
          do i = 0, numprocs - 1
            ibegin = startx(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1) 
            if(ibegin.gt.1) ibegin=ibegin - 1
            iend   = ibegin + local_rt_nx_size(i+1) -1
            jbegin = starty(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1)
            if(jbegin.gt.1) jbegin=jbegin - 1
            jend   = jbegin + local_rt_ny_size(i+1) -1
            if(i .eq. IO_id) then
               out_buff(ibegin:iend,jbegin:jend) = in_buff 
            else 
               size = local_rt_nx_size(i+1)*local_rt_ny_size(i+1)
               tag = 2
               call mpi_recv(out_buff(ibegin:iend,jbegin:jend),size,&
                   MPI_INTEGER,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_RT_int

  subroutine mpp_land_sync()
      integer ierr
       call MPI_barrier( MPI_COMM_WORLD ,ierr)
      return
  end subroutine mpp_land_sync

! subroutine mpp_land_sync()
!    integer tag, i, status, ierr,size
!    integer buff(2)
!    
!     size =2 
!     buff = 3 
!     if(my_id .ne. IO_id) then
!         tag = 2
!         call mpi_send(buff,size,MPI_INTEGER, IO_id,     &
!               tag,MPI_COMM_WORLD,ierr)
!     else
!         do i = 0, numprocs - 1
!              tag = 2
!            if(i .ne. IO_id) then
!              call mpi_recv(buff,size,&
!                  MPI_INTEGER,i,tag,MPI_COMM_WORLD,mpp_status,ierr)
!           end if
!         end do
!     end if

!     return
! end subroutine mpp_land_sync

  subroutine mpp_land_bcast_int(size,inout)
      integer size
      integer inout(size)
      integer ierr
        call mpi_bcast(inout,size,MPI_INTEGER,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int

  subroutine mpp_land_bcast_int1(inout)
      integer inout
      integer ierr
        call mpi_bcast(inout,1,MPI_INTEGER,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int1

  subroutine mpp_land_bcast_real1(inout)
      real inout
      integer ierr
        call mpi_bcast(inout,1,MPI_REAL,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_real1

  subroutine mpp_land_bcast_real(size,inout)
      integer size
      real inout(size)
      integer ierr
        call mpi_bcast(inout,size,MPI_real,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real

  subroutine mpp_land_bcast_rd(size,inout)
      integer size
      real*8 inout(size)
      integer ierr
        call mpi_bcast(inout,size,MPI_REAL8,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_rd

  subroutine mpp_land_bcast_char(size,inout)
      integer size
      character inout(*)
      integer ierr
        call mpi_bcast(inout,size,MPI_CHARACTER,   &
            IO_id,MPI_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char

 
  subroutine MPP_LAND_COM_REAL(in_out_data,NX,NY,flag)
!   ### Communicate message on left right and up bottom directions.
    integer NX,NY
    integer flag != 99  test only for land model. (replace the boundary).
                 != 1   get the sum of the boundary value.
    real in_out_data(nx,ny)

    call MPP_LAND_LR_COM(in_out_data,NX,NY,flag)
    call MPP_LAND_UB_COM(in_out_data,NX,NY,flag)

    return
  end subroutine MPP_LAND_COM_REAL

  subroutine MPP_LAND_COM_REAL8(in_out_data,NX,NY,flag)
!   ### Communicate message on left right and up bottom directions.
    integer NX,NY
    integer flag != 99  test only for land model. (replace the boundary).
                 != 1   get the sum of the boundary value.
    real*8 in_out_data(nx,ny)

    call MPP_LAND_LR_COM8(in_out_data,NX,NY,flag)
    call MPP_LAND_UB_COM8(in_out_data,NX,NY,flag)

    return
  end subroutine MPP_LAND_COM_REAL8

  subroutine MPP_LAND_COM_INTEGER(data,NX,NY,flag)
!   ### Communicate message on left right and up bottom directions.
    integer NX,NY
    integer flag != 99  test only for land model. (replace the boundary).
                 != 1   get the sum of the boundary value.
    integer data(nx,ny)
    real in_out_data(nx,ny)

    in_out_data = data + 0.0
    call MPP_LAND_LR_COM(in_out_data,NX,NY,flag)
    call MPP_LAND_UB_COM(in_out_data,NX,NY,flag)
    data = in_out_data + 0

    return
  end subroutine MPP_LAND_COM_INTEGER
 
     subroutine read_restart_3(unit,nz,out)
        integer unit,nz,i
        real buf3(global_nx,global_ny,nz),&
          out(local_nx,local_ny,3)
        if(my_id.eq.IO_id) read(unit) buf3
        do i = 1,nz
          call decompose_data_real (buf3(:,:,i),out(:,:,i))
        end do
     return
     end subroutine read_restart_3

     subroutine read_restart_2(unit,out)
        integer unit,ierr2
        real  buf2(global_nx,global_ny),&
          out(local_nx,local_ny)

       if(my_id.eq.IO_id) read(unit,IOSTAT=ierr2) buf2
        call mpp_land_bcast_int1(ierr2)
        if(ierr2 .ne. 0) return

        call decompose_data_real (buf2,out)
     return
     end subroutine read_restart_2

     subroutine read_restart_rt_2(unit,out)
        integer unit,ierr2
        real  buf2(global_rt_nx,global_rt_ny),&
          out(local_rt_nx,local_rt_ny)

       if(my_id.eq.IO_id) read(unit,IOSTAT=ierr2) buf2
        call mpp_land_bcast_int1(ierr2)
        if(ierr2.ne.0) return

        call decompose_RT_real(buf2,out, &
          global_rt_nx,global_rt_ny,local_rt_nx,local_rt_ny)
     return
     end subroutine read_restart_rt_2

     subroutine read_restart_rt_3(unit,nz,out)
        integer unit,nz,i,ierr2
        real buf3(global_rt_nx,global_rt_ny,nz),&
          out(local_rt_nx,local_rt_ny,3)

        if(my_id.eq.IO_id) read(unit,IOSTAT=ierr2) buf3
        call mpp_land_bcast_int1(ierr2)
        if(ierr2.ne.0) return

        do i = 1,nz
          call decompose_RT_real (buf3(:,:,i),out(:,:,i),&
          global_rt_nx,global_rt_ny,local_rt_nx,local_rt_ny)
        end do
     return
     end subroutine read_restart_rt_3

     subroutine write_restart_3(unit,nz,in)
        integer unit,nz,i
        real buf3(global_nx,global_ny,nz),&
          in(local_nx,local_ny,nz)
        do i = 1,nz
          call write_IO_real(in(:,:,i),buf3(:,:,i))
        end do
        if(my_id.eq.IO_id) write(unit) buf3
     return
     end subroutine write_restart_3

     subroutine write_restart_2(unit,in)
        integer unit
        real  buf2(global_nx,global_ny),&
           in(local_nx,local_ny)
        call write_IO_real(in,buf2)
        if(my_id.eq.IO_id) write(unit) buf2
     return
     end subroutine write_restart_2

     subroutine write_restart_rt_2(unit,in)
        integer unit
        real  buf2(global_rt_nx,global_rt_ny), &
           in(local_rt_nx,local_rt_ny)
        call write_IO_RT_real(in,buf2)
        if(my_id.eq.IO_id) write(unit) buf2
     return
     end subroutine write_restart_rt_2

     subroutine write_restart_rt_3(unit,nz,in)
        integer unit,nz,i
        real buf3(global_rt_nx,global_rt_ny,nz),&
          in(local_rt_nx,local_rt_ny,nz)
        do i = 1,nz
          call write_IO_RT_real(in(:,:,i),buf3(:,:,i))
        end do
        if(my_id.eq.IO_id) write(unit) buf3
     return
     end subroutine write_restart_rt_3

   subroutine decompose_RT_real (in_buff,out_buff,g_nx,g_ny,nx,ny)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      integer g_nx,g_ny,nx,ny
      real in_buff(g_nx,g_ny),out_buff(nx,ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend

      tag = 2
      if(my_id .eq. IO_id) then
         do i = 0, numprocs - 1
            ibegin = startx(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1) 
            if(ibegin.gt.1) ibegin=ibegin - 1
            iend   = ibegin + local_rt_nx_size(i+1) -1
            jbegin = starty(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1)
            if(jbegin.gt.1) jbegin=jbegin - 1
            jend   = jbegin + local_rt_ny_size(i+1) -1

            if(my_id .eq. i) then
               out_buff=in_buff(ibegin:iend,jbegin:jend)
            else
               ! send data to the rest process.
               size = (iend-ibegin+1)*(jend-jbegin+1)
               call mpi_send(in_buff(ibegin:iend,jbegin:jend),size,&
                  MPI_REAL, i,tag,MPI_COMM_WORLD,ierr)
            end if
         end do
      else
         size = nx*ny
         call mpi_recv(out_buff,size,MPI_REAL,IO_id, &
                tag,MPI_COMM_WORLD,mpp_status,ierr)
      end if
      return
  end subroutine decompose_RT_real

   subroutine decompose_RT_int (in_buff,out_buff,g_nx,g_ny,nx,ny)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      integer g_nx,g_ny,nx,ny
      integer in_buff(g_nx,g_ny),out_buff(nx,ny)
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend

      tag = 2
        call mpp_land_sync()
      if(my_id .eq. IO_id) then
         do i = 0, numprocs - 1
            ibegin = startx(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1)
            if(ibegin.gt.1) ibegin=ibegin - 1
            iend   = ibegin + local_rt_nx_size(i+1) -1
            jbegin = starty(i+1)*rt_AGGFACTRT - (rt_AGGFACTRT-1)
            if(jbegin.gt.1) jbegin=jbegin - 1
            jend   = jbegin + local_rt_ny_size(i+1) -1

            if(my_id .eq. i) then
               out_buff=in_buff(ibegin:iend,jbegin:jend)
            else
               ! send data to the rest process.
               size = (iend-ibegin+1)*(jend-jbegin+1)
               call mpi_send(in_buff(ibegin:iend,jbegin:jend),size,&
                  MPI_INTEGER, i,tag,MPI_COMM_WORLD,ierr)
            end if
         end do
      else
         size = nx*ny
         call mpi_recv(out_buff,size,MPI_INTEGER,IO_id, &
                tag,MPI_COMM_WORLD,mpp_status,ierr)
      end if
      return
  end subroutine decompose_RT_int

  subroutine getNX_NY(nprocs, nx,ny)
  ! calculate the nx and ny based on the total nprocs.
    integer nprocs, nx, ny
    integer i,j, max
    max = nprocs
    do j = 1, nprocs
       if( mod(nprocs,j) .eq. 0 ) then
           i = nprocs/j
           if( abs(i-j) .lt. max) then
               max = abs(i-j)
               nx = i 
               ny = j 
           end if
       end if
    end do
  return 
  end subroutine getNX_NY

     subroutine pack_global_22(in,   &
        out,k)
        integer ix,jx,k,i
        real out(global_nx,global_ny,k)
        real  in(local_nx,local_ny,k)
        do i = 1, k
          call write_IO_real(in(:,:,i),out(:,:,i))
        enddo
     return 
     end subroutine pack_global_22


  subroutine wrf_LAND_set_INIT(info,total_pe,AGGFACTRT)
    implicit none
    integer total_pe
    integer info(9,total_pe),AGGFACTRT
    integer :: ierr, status
    integer i

      call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr ) 
      call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr ) 

!   ### get the neighbors.  -1 means no neighbor.
      left_id = info(2,my_id+1)
      right_id = info(3,my_id+1)
      up_id =   info(4,my_id+1)
      down_id = info(5,my_id+1)
      IO_id = 0

       allocate(local_nx_size(numprocs),stat = status) 
       allocate(local_ny_size(numprocs),stat = status) 
       allocate(local_rt_nx_size(numprocs),stat = status) 
       allocate(local_rt_ny_size(numprocs),stat = status) 
       allocate(starty(numprocs),stat = ierr) 
       allocate(startx(numprocs),stat = ierr)

       i = my_id + 1
       local_nx = info(7,i) - info(6,i) + 1
       local_ny = info(9,i) - info(8,i) + 1
 
       global_nx = 0
       global_ny = 0
       do i = 1, numprocs
          global_nx = max(global_nx,info(7,i))
          global_ny = max(global_ny,info(9,i))
       enddo

       local_rt_nx = local_nx*AGGFACTRT+2
       local_rt_ny = local_ny*AGGFACTRT+2
       if(left_id.lt.0) local_rt_nx = local_rt_nx -1
       if(right_id.lt.0) local_rt_nx = local_rt_nx -1
       if(up_id.lt.0) local_rt_ny = local_rt_ny -1
       if(down_id.lt.0) local_rt_ny = local_rt_ny -1

       global_rt_nx = global_nx*AGGFACTRT
       global_rt_ny = global_ny*AGGFACTRT
       rt_AGGFACTRT = AGGFACTRT

       do i =1,numprocs 
          local_nx_size(i) = info(7,i) - info(6,i) + 1
          local_ny_size(i) = info(9,i) - info(8,i) + 1
          startx(i)        = info(6,i) 
          starty(i)        = info(8,i) 

          local_rt_nx_size(i) = (info(7,i) - info(6,i) + 1)*AGGFACTRT+2
          local_rt_ny_size(i) = (info(9,i) - info(8,i) + 1 )*AGGFACTRT+2
          if(info(2,i).lt.0) local_rt_nx_size(i) = local_rt_nx_size(i) -1
          if(info(3,i).lt.0) local_rt_nx_size(i) = local_rt_nx_size(i) -1
          if(info(4,i).lt.0) local_rt_ny_size(i) = local_rt_ny_size(i) -1
          if(info(5,i).lt.0) local_rt_ny_size(i) = local_rt_ny_size(i) -1
       enddo
      return 
      end   subroutine wrf_LAND_set_INIT

      subroutine getMy_global_id()
          integer ierr
          call MPI_COMM_RANK( MPI_COMM_WORLD, my_id, ierr ) 
      return
      end subroutine getMy_global_id

  subroutine MPP_CHANNEL_COM_REAL(Link_location,ix,jy,Link_V,size,flag)
  ! communicate the data for channel routine.
      implicit none
      integer ix,jy,size
      integer Link_location(ix,jy)
      integer i,j, flag
      real Link_V(size), tmp_inout(ix,jy)

       if(size .eq. 0) return

!     map the Link_V data to tmp_inout(ix,jy)
      do i = 1,ix 
            if(Link_location(i,1) .gt. 0) &
               tmp_inout(i,1) = Link_V(Link_location(i,1))
            if(Link_location(i,2) .gt. 0) &
               tmp_inout(i,2) = Link_V(Link_location(i,2))
            if(Link_location(i,jy-1) .gt. 0) &
               tmp_inout(i,jy-1) = Link_V(Link_location(i,jy-1))
            if(Link_location(i,jy) .gt. 0) &
               tmp_inout(i,jy) = Link_V(Link_location(i,jy))
        enddo
      do j = 1,jy 
            if(Link_location(1,j) .gt. 0) &
               tmp_inout(1,j) = Link_V(Link_location(1,j))
            if(Link_location(2,j) .gt. 0) &
               tmp_inout(2,j) = Link_V(Link_location(2,j))
            if(Link_location(ix-1,j) .gt. 0) &
               tmp_inout(ix-1,j) = Link_V(Link_location(ix-1,j))
            if(Link_location(ix,j) .gt. 0) &
               tmp_inout(ix,j) = Link_V(Link_location(ix,j))
      enddo

!   commu nicate tmp_inout
    call MPP_LAND_COM_REAL(tmp_inout, ix,jy,flag)

!map the data back to Link_V
      do j = 1,jy 
            if(Link_location(1,j) .gt. 0) &
               Link_V(Link_location(1,j)) = tmp_inout(1,j)
            if(Link_location(2,j) .gt. 0) &
               Link_V(Link_location(2,j)) = tmp_inout(2,j)
            if(Link_location(ix-1,j) .gt. 0) &
               Link_V(Link_location(ix-1,j)) = tmp_inout(ix-1,j)
            if(Link_location(ix,j) .gt. 0) &
               Link_V(Link_location(ix,j)) = tmp_inout(ix,j)
      enddo
      do i = 1,ix 
            if(Link_location(i,1) .gt. 0) &
               Link_V(Link_location(i,1)) = tmp_inout(i,1)
            if(Link_location(i,2) .gt. 0) &
               Link_V(Link_location(i,2)) = tmp_inout(i,2)
            if(Link_location(i,jy-1) .gt. 0) &
               Link_V(Link_location(i,jy-1)) = tmp_inout(i,jy-1)
            if(Link_location(i,jy) .gt. 0) &
               Link_V(Link_location(i,jy)) = tmp_inout(i,jy)
      enddo
  end subroutine MPP_CHANNEL_COM_REAL

     subroutine print_2(unit,in,fm)
        integer unit
        character(len=*) fm
        real  buf2(global_nx,global_ny),&
           in(local_nx,local_ny)
        call write_IO_real(in,buf2)
        if(my_id.eq.IO_id) write(unit,*) buf2
     return
     end subroutine print_2

     subroutine print_rt_2(unit,in)
        integer unit
        real  buf2(global_nx,global_ny),&
           in(local_nx,local_ny)
        call write_IO_real(in,buf2)
        if(my_id.eq.IO_id) write(unit,*) buf2
     return
     end subroutine print_rt_2

     subroutine mpp_land_max_int1(v)
        implicit none
        integer v, r1, max
        integer i, ierr, tag
        if(my_id .eq. IO_id) then
           max = v
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 101
                 call mpi_recv(r1,1,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                 if(max <= r1) max = r1 
              end if
           end do
       else
           tag =  101
           call mpi_send(v,1,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
       end if
       call mpp_land_bcast_int1(max)
       v = max
       return
     end subroutine mpp_land_max_int1
     
     subroutine mpp_land_max_real1(v)
        implicit none
        real v, r1, max
        integer i, ierr, tag
        if(my_id .eq. IO_id) then
           max = v
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 101
                 call mpi_recv(r1,1,MPI_REAL,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                 if(max <= r1) max = r1 
              end if
           end do
       else
           tag =  101
           call mpi_send(v,1,MPI_REAL, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
       end if
       call mpp_land_bcast_real1(max)
       v = max
       return
     end subroutine mpp_land_max_real1

     subroutine mpp_same_int1(v)   
        implicit none
        integer v,r1
        integer i, ierr, tag
        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 109
                 call mpi_recv(r1,1,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                 if(v .ne. r1) v = -99  
              end if
           end do
       else
           tag =  109
           call mpi_send(v,1,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
       end if
       call mpp_land_bcast_int1(v)
     end subroutine mpp_same_int1

     subroutine write_chanel_real8(v,nodelist_in,mpp_nlinks,nlinks)   
        implicit none
        real*8 recv(nlinks), v(nlinks)
        integer nodelist(nlinks), mpp_nlinks,nlinks, nodelist_in(nlinks)
        integer i, ierr, tag, k
        integer length, node

        nodelist = nodelist_in
        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 109
                 call mpi_recv(nodelist,nlinks,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                   tag = 119
                 call mpi_recv(recv(:),nlinks,MPI_DOUBLE_PRECISION,i,  &
                   tag,MPI_COMM_WORLD,mpp_status,ierr)

                 do k = 1,nodelist(nlinks)
                    if(nodelist(k) .gt. -99) then
                      node = nodelist(k) 
                      v(node) = recv(node)
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  109
           nodelist(nlinks) = mpp_nlinks 
           call mpi_send(nodelist,nlinks,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
           tag = 119
           call mpi_send(v,nlinks,MPI_DOUBLE_PRECISION,IO_id,   &
               tag,MPI_COMM_WORLD,ierr)
        end if
     end subroutine write_chanel_real8


     subroutine write_chanel_real(v,nodelist_in,mpp_nlinks,nlinks)   
        implicit none
        real recv(nlinks), v(nlinks)
        integer nodelist(nlinks), mpp_nlinks,nlinks, nodelist_in(nlinks)
        integer i, ierr, tag, k
        integer length, node

        nodelist = nodelist_in
        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 109
                 call mpi_recv(nodelist,nlinks,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                   tag = 119
                 call mpi_recv(recv(:),nlinks,MPI_REAL,i,  &
                   tag,MPI_COMM_WORLD,mpp_status,ierr)

                 do k = 1,nodelist(nlinks)
                    if(nodelist(k) .gt. -99) then
                      node = nodelist(k) 
                      v(node) = recv(node)
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  109
           nodelist(nlinks) = mpp_nlinks 
           call mpi_send(nodelist,nlinks,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
           tag = 119
           call mpi_send(v,nlinks,MPI_REAL,IO_id,   &
               tag,MPI_COMM_WORLD,ierr)
        end if
     end subroutine write_chanel_real

     subroutine write_chanel_int(v,nodelist_in,mpp_nlinks,nlinks)   
        implicit none
        integer :: recv(nlinks), v(nlinks)
        integer nodelist(nlinks), mpp_nlinks,nlinks, nodelist_in(nlinks)
        integer i, ierr, tag, k
        integer length, node

        nodelist = nodelist_in
        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 109
                 call mpi_recv(nodelist,nlinks,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                   tag = 119
                 call mpi_recv(recv(:),nlinks,MPI_INTEGER,i,  &
                   tag,MPI_COMM_WORLD,mpp_status,ierr)

                 do k = 1,nodelist(nlinks)
                    if(nodelist(k) .gt. -99) then
                      node = nodelist(k) 
                      v(node) = recv(node)
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  109
           nodelist(nlinks) = mpp_nlinks 
           call mpi_send(nodelist,nlinks,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
           tag = 119
           call mpi_send(v,nlinks,MPI_INTEGER,IO_id,   &
               tag,MPI_COMM_WORLD,ierr)
        end if
     end subroutine write_chanel_int


     subroutine write_lake_real(v,nodelist_in,nlakes)   
        implicit none
        real recv(nlakes), v(nlakes)
        integer nodelist(nlakes), nlakes, nodelist_in(nlakes)
        integer i, ierr, tag, k
        integer length, node

        nodelist = nodelist_in
        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 129
                 call mpi_recv(nodelist,nlakes,MPI_INTEGER,i, &
                      tag,MPI_COMM_WORLD,mpp_status,ierr)
                   tag = 139
                 call mpi_recv(recv(:),nlakes,MPI_REAL,i,  &
                   tag,MPI_COMM_WORLD,mpp_status,ierr)

                 do k = 1,nlakes
                    if(nodelist(k) .gt. -99) then
                       node = nodelist(k) 
                       v(node) = recv(node)
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  129
           call mpi_send(nodelist,nlakes,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)
           tag = 139
           call mpi_send(v,nlakes,MPI_REAL,IO_id,   &
               tag,MPI_COMM_WORLD,ierr)
        end if
     end subroutine write_lake_real

     subroutine read_rst_crt_r(unit,out,size)
         implicit none
        integer unit, size, ierr,ierr2
        real  out(size),out1(size)
        if(my_id.eq.IO_id) then
          read(unit,IOSTAT=ierr2,end=99) out1
          if(ierr2.eq.0) out=out1
        endif
99      continue
        call mpp_land_bcast_int1(ierr2)
        if(ierr2 .ne. 0) return
        call mpi_bcast(out,size,MPI_REAL,   &
            IO_id,MPI_COMM_WORLD,ierr)
     return
     end subroutine read_rst_crt_r  

         subroutine write_rst_crt_r(unit,cd,nlinks_index,mpp_nlinks,nlinks)
         integer :: unit,mpp_nlinks,nlinks,nlinks_index(nlinks)
         real cd(nlinks)
         call write_chanel_real(cd,nlinks_index,mpp_nlinks,nlinks)
         write(unit) cd
         return
         end subroutine write_rst_crt_r

    subroutine sum_real8(vin,nsize)
       implicit none
       integer nsize,i,j,tag,ierr
       real*8, dimension(nsize):: vin,recv
       real, dimension(nsize):: v 
       tag = 319
       if(my_id .eq. IO_id) then
          do i = 0, numprocs - 1
             if(i .ne. my_id) then
               call mpi_recv(recv,nsize,MPI_DOUBLE_PRECISION,i,  &
                    tag,MPI_COMM_WORLD,mpp_status,ierr)
               vin(:) = vin(:) + recv(:)
             endif
          end do
          v = vin
       else
             call mpi_send(vin,nsize,MPI_DOUBLE_PRECISION,IO_id,   &
                  tag,MPI_COMM_WORLD,ierr)
       endif
       call mpp_land_bcast_real(nsize,v) 
       vin = v
       return
    end subroutine sum_real8

  subroutine get_globalDim(ix,g_ix)
     implicit none
     integer ix,g_ix, ierr
     include "mpif.h"

     if ( my_id .eq. IO_id ) then
           g_ix = ix
        call mpi_reduce( MPI_IN_PLACE, g_ix, 4, MPI_INTEGER, &
             MPI_SUM, 0, MPI_COMM_WORLD, ierr )
     else
        call mpi_reduce( ix,       0,      4, MPI_INTEGER, &
             MPI_SUM,  0, MPI_COMM_WORLD, ierr )
     endif
     call mpp_land_bcast_int1(g_ix)

     return

  end subroutine get_globalDim

  subroutine gather_1d_real_tmp(vl,s_in,e_in,vg,sg)
    integer sg, s,e, size, s_in, e_in
    integer index_s(2)
    integer tag, ierr,i
!   s: start index, e: end index
    real  vl(e_in-s_in+1), vg(sg)
    s = s_in
    e = e_in

    if(my_id .eq. IO_id) then 
        vg(s:e) = vl
    end if

     index_s(1) = s
     index_s(2) = e
     size = e - s + 1 

    if(my_id .eq. IO_id) then
         do i = 0, numprocs - 1 
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 202
                 call mpi_recv(index_s,2,MPI_INTEGER,i, & 
                      tag,MPI_COMM_WORLD,mpp_status,ierr)

                 tag = 203
                 e = index_s(2)
                 s = index_s(1)
                 size = e - s + 1 
                 call mpi_recv(vg(s:e),size,MPI_REAL,  &
                    i,tag,MPI_COMM_WORLD,mpp_status,ierr)
              endif
         end do
     else 
           tag =  202
           call mpi_send(index_s,2,MPI_INTEGER, IO_id,     &
               tag,MPI_COMM_WORLD,ierr)

           tag =  203  
           call mpi_send(vl,size,MPI_REAL,IO_id,   &
               tag,MPI_COMM_WORLD,ierr)
     end if

     return 
  end  subroutine gather_1d_real_tmp

  subroutine sum_double(inout)
      implicit none
      real*8:: inout, send
      integer :: ierr
      send = inout
      CALL MPI_ALLREDUCE(send,inout,1,MPI_DOUBLE,MPI_SUM,MPI_COMM_WORLD,ierr)
  end subroutine sum_double


END MODULE MODULE_MPP_LAND

        subroutine mpp_land_abort()
            implicit none
  include "mpif.h"
            integer ierr
            CALL MPI_ABORT(MPI_COMM_WORLD,1,IERR)
        end ! mpp_land_abort

  subroutine mpp_land_finish()
      implicit none
      include "mpif.h"
      integer ierr
#ifdef HYDRO_D
      write(6,*) "mpp_land_finish called "
#endif
      call MPI_barrier( MPI_COMM_WORLD ,ierr)
      if(ierr .ne. 0) call mpp_land_abort()
      call MPI_finalize(ierr)
      if(ierr .ne. 0) call mpp_land_abort()
#ifdef HYDRO_D
      stop 999
#endif
      return
  end ! mpp_land_finish 

  subroutine mpp_land_sync()
      implicit none
      include "mpif.h"
      integer ierr
      call MPI_barrier( MPI_COMM_WORLD ,ierr)
      if(ierr .ne. 0) call mpp_land_abort()
      return
  end ! mpp_land_sync




