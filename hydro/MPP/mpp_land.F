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

!#### This is a module for parallel Land model.
MODULE MODULE_MPP_LAND

  use MODULE_CPL_LAND

  IMPLICIT NONE
  include "mpif.h"
  !integer, public :: HYDRO_COMM_WORLD ! communicator for WRF-Hydro - moved to MODULE_CPL_LAND
  integer, public :: left_id,right_id,up_id,down_id,my_id
  integer, public :: left_right_np,up_down_np ! define total process in two dimensions.
  integer, public :: left_right_p ,up_down_p ! the position of the current process in the logical topography.
  integer, public :: IO_id   ! the number for IO. (Last processor for IO)
  integer, public :: global_nx, global_ny, local_nx,local_ny
  integer, public :: global_rt_nx, global_rt_ny
  integer, public :: local_rt_nx,local_rt_ny,rt_AGGFACTRT
  integer, public :: numprocs   ! total process, get by mpi initialization.
  integer :: local_startx, local_starty
  integer :: local_startx_rt, local_starty_rt, local_endx_rt, local_endy_rt

  integer mpp_status(MPI_STATUS_SIZE)

  integer  overlap_n
  integer, allocatable, DIMENSION(:), public :: local_nx_size,local_ny_size
  integer, allocatable, DIMENSION(:), public :: local_rt_nx_size,local_rt_ny_size
  integer, allocatable, DIMENSION(:), public :: startx,starty
  integer, allocatable, DIMENSION(:), public :: mpp_nlinks

  interface check_land
     module procedure check_landreal1
     module procedure check_landreal1d
     module procedure check_landreal2d
     module procedure check_landreal3d
  end interface
  interface write_io_land
     module procedure write_io_real3d
  end interface
  interface mpp_land_bcast
     module procedure mpp_land_bcast_real2
     module procedure mpp_land_bcast_real_1d
     module procedure mpp_land_bcast_real8_1d
     module procedure mpp_land_bcast_real1
     module procedure mpp_land_bcast_char1d 
     module procedure mpp_land_bcast_char1
     module procedure mpp_land_bcast_int1 
     module procedure mpp_land_bcast_int1d 
     module procedure mpp_land_bcast_int2d 
     module procedure mpp_land_bcast_logical
     
  end interface
 
  contains

  subroutine LOG_MAP2d()
    implicit none
    integer :: ndim, ierr
    integer, dimension(0:1) :: dims, coords
    
    logical cyclic(0:1), reorder
    data cyclic/.false.,.false./  ! not cyclic
    data reorder/.false./
    
      call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_id, ierr )
      call MPI_COMM_SIZE( HYDRO_COMM_WORLD, numprocs, ierr )

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

! BF  setup virtual cartesian grid topology
      ndim = 2

      dims(0) = up_down_np      ! rows
      dims(1) = left_right_np   ! columns
!
     call MPI_Cart_create(HYDRO_COMM_WORLD, ndim, dims, &
                          cyclic, reorder, cartGridComm, ierr)
     
     call MPI_CART_GET(cartGridComm, 2, dims, cyclic, coords, ierr)
     
     p_up_down = coords(0)
     p_left_right = coords(1)
     np_up_down = up_down_np 
     np_left_right = left_right_np
 
     
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
           HYDRO_COMM_WORLD = MPI_COMM_WORLD
      else 
           call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_id, ierr )
           call MPI_COMM_SIZE( HYDRO_COMM_WORLD, numprocs, ierr )
           return
      endif
!     create 2d logical mapping of the CPU.
      call log_map2d()
      return
  end   subroutine MPP_LAND_INIT


     subroutine MPP_LAND_PAR_INI(over_lap,in_global_nx,in_global_ny,AGGFACTRT)
        integer in_global_nx,in_global_ny, AGGFACTRT
        integer :: over_lap   ! the overlaped grid number. (default is 1)
        integer :: i

        global_nx = in_global_nx
        global_ny = in_global_ny 
        rt_AGGFACTRT = AGGFACTRT
        global_rt_nx = in_global_nx*AGGFACTRT
        global_rt_ny = in_global_ny *AGGFACTRT
        !overlap_n = 1
!ywold        local_nx = global_nx / left_right_np 
!ywold        if(left_right_p .eq. (left_right_np-1) ) then
!ywold              local_nx = global_nx   &
!ywold                    -int(global_nx/left_right_np)*(left_right_np-1)
!ywold        end if
!ywold        local_ny = global_ny / up_down_np 
!ywold        if(  up_down_p .eq. (up_down_np-1) ) then
!ywold           local_ny = global_ny  &
!ywold                 -int(global_ny/up_down_np)*(up_down_np -1)
!ywold       end if

        local_nx = int(global_nx / left_right_np)
        !if(global_nx .ne. (local_nx*left_right_np) ) then
        if(mod(global_nx, left_right_np) .ne. 0) then
            do i = 1, mod(global_nx, left_right_np)
               if(left_right_p .eq. i ) then
                   local_nx = local_nx + 1
               end if
            end do
        end if

        local_ny = int(global_ny / up_down_np)
        !if(global_ny .ne. (local_ny * up_down_np) ) then
        if(mod(global_ny,up_down_np) .ne. 0 ) then
            do i = 1, mod(global_ny,up_down_np)
                 if( up_down_p .eq. i) then
                     local_ny = local_ny + 1
                 end if
            end do
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
             right_id,tag,HYDRO_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
           tag = 11
           size = ny
           call mpi_recv(in_out_data(1,:),size,MPI_REAL,  &
              left_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = ny 
          tag = 21
          call mpi_send(in_out_data(2,:),size,MPI_REAL,   &
             left_id,tag,HYDRO_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = ny 
          call mpi_recv(in_out_data(nx,:),size,MPI_REAL,&
             right_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
      endif

    else   ! get the sum

       if(right_id .ge. 0) then !   ### send to right first.
         tag = 11
         size = 2*ny 
         call mpi_send(in_out_data(nx-1:nx,:),size,MPI_REAL,   &
             right_id,tag,HYDRO_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
          tag = 11
          size = 2*ny
          call mpi_recv(data_r,size,MPI_REAL,left_id,tag, &
               HYDRO_COMM_WORLD,mpp_status,ierr)
          in_out_data(1,:) = in_out_data(1,:) + data_r(1,:)
          in_out_data(2,:) = in_out_data(2,:) + data_r(2,:)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = 2*ny
          tag = 21
          call mpi_send(in_out_data(1:2,:),size,MPI_REAL,   &
             left_id,tag,HYDRO_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = 2*ny
          call mpi_recv(in_out_data(nx-1:nx,:),size,MPI_REAL,&
             right_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
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
             right_id,tag,HYDRO_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
           tag = 11
           size = ny
           call mpi_recv(in_out_data(1,:),size,MPI_DOUBLE_PRECISION,  &
              left_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = ny 
          tag = 21
          call mpi_send(in_out_data(2,:),size,MPI_DOUBLE_PRECISION,   &
             left_id,tag,HYDRO_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = ny 
          call mpi_recv(in_out_data(nx,:),size,MPI_DOUBLE_PRECISION,&
             right_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
      endif

    else   ! get the sum

       if(right_id .ge. 0) then !   ### send to right first.
         tag = 11
         size = 2*ny 
         call mpi_send(in_out_data(nx-1:nx,:),size,MPI_DOUBLE_PRECISION,   &
             right_id,tag,HYDRO_COMM_WORLD,ierr)
       end if
       if(left_id .ge. 0) then !   receive from left
          tag = 11
          size = 2*ny
          call mpi_recv(data_r,size,MPI_DOUBLE_PRECISION,left_id,tag, &
               HYDRO_COMM_WORLD,mpp_status,ierr)
          in_out_data(1,:) = in_out_data(1,:) + data_r(1,:)
          in_out_data(2,:) = in_out_data(2,:) + data_r(2,:)
       endif 

      if(left_id .ge. 0 ) then !   ### send to left second.
          size = 2*ny
          tag = 21
          call mpi_send(in_out_data(1:2,:),size,MPI_DOUBLE_PRECISION,   &
             left_id,tag,HYDRO_COMM_WORLD,ierr)
      endif
      if(right_id .ge. 0) then !   receive from  right
          tag = 21
          size = 2*ny
          call mpi_recv(in_out_data(nx-1:nx,:),size,MPI_DOUBLE_PRECISION,&
             right_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
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
               tag,HYDRO_COMM_WORLD,ierr)
       end if

 
       if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1 
              if(i .ne. my_id) then
                 !block receive  from other node.
                 tag = 2
                 call mpi_recv(s_r,2,MPI_INTEGER,i, & 
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
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
               tag,HYDRO_COMM_WORLD,ierr)
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
               up_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down 
           tag = 31 
           size = nx
           call mpi_recv(in_out_data(:,1),size,MPI_REAL, &
              down_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
       endif
   
       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx
           call mpi_send(in_out_data(:,2),size,MPI_REAL,      &
                down_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper 
           tag = 41 
           size = nx
           call mpi_recv(in_out_data(:,ny),size,MPI_REAL, &
               up_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
       endif
     
    else  ! flag = 1 

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx*2
           call mpi_send(in_out_data(:,ny-1:ny),size,MPI_REAL,   &
               up_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down
           tag = 31
           size = nx*2
           call mpi_recv(data_r,size,MPI_REAL, &
              down_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
           in_out_data(:,1) = in_out_data(:,1) + data_r(:,1)
           in_out_data(:,2) = in_out_data(:,2) + data_r(:,2)
       endif

       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx*2
           call mpi_send(in_out_data(:,1:2),size,MPI_REAL,      &
                down_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper
           tag = 41
           size = nx * 2
           call mpi_recv(in_out_data(:,ny-1:ny),size,MPI_REAL, &
               up_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
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
               up_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down 
           tag = 31 
           size = nx
           call mpi_recv(in_out_data(:,1),size,MPI_DOUBLE_PRECISION, &
              down_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
       endif
   
       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx
           call mpi_send(in_out_data(:,2),size,MPI_DOUBLE_PRECISION,      &
                down_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper 
           tag = 41 
           size = nx
           call mpi_recv(in_out_data(:,ny),size,MPI_DOUBLE_PRECISION, &
               up_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
       endif
     
    else  ! flag = 1 

       if(up_id .ge. 0 ) then !   ### send to up first.
           tag = 31
           size = nx*2
           call mpi_send(in_out_data(:,ny-1:ny),size,MPI_DOUBLE_PRECISION,   &
               up_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(down_id .ge. 0 ) then !   receive from down
           tag = 31
           size = nx*2
           call mpi_recv(data_r,size,MPI_DOUBLE_PRECISION, &
              down_id,tag,HYDRO_COMM_WORLD, mpp_status,ierr)
           in_out_data(:,1) = in_out_data(:,1) + data_r(:,1)
           in_out_data(:,2) = in_out_data(:,2) + data_r(:,2)
       endif

       if(down_id .ge. 0 ) then !   send down.
           tag = 41
           size = nx*2
           call mpi_send(in_out_data(:,1:2),size,MPI_DOUBLE_PRECISION,      &
                down_id,tag,HYDRO_COMM_WORLD,ierr)
       endif
       if(up_id .ge. 0 ) then !   receive from upper
           tag = 41
           size = nx * 2
           call mpi_recv(in_out_data(:,ny-1:ny),size,MPI_DOUBLE_PRECISION, &
               up_id,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
       endif
    endif  ! end of block  flag
    return
  end  subroutine MPP_LAND_UB_COM8
  
  subroutine calculate_start_p()
! calculate startx and starty
    integer :: i,status, ierr, tag
    integer :: r_s(2)
    integer ::  t_nx, t_ny

    allocate(starty(numprocs),stat = ierr) 
    allocate(startx(numprocs),stat = ierr)

    local_startx = int(global_nx/left_right_np) * left_right_p+1 
    local_starty = int(global_ny/up_down_np) * up_down_p+1 

!ywold
    t_nx = 0
    do i = 1, mod(global_nx,left_right_np)
       if(left_right_p .gt. i ) then
           t_nx = t_nx + 1
       end if
    end do
    local_startx = local_startx + t_nx

    t_ny = 0
    do i = 1, mod(global_ny,up_down_np)
       if( up_down_p .gt. i) then
           t_ny = t_ny + 1
       end if
    end do
    local_starty = local_starty + t_ny


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
                   tag,HYDRO_COMM_WORLD,mpp_status,ierr)
              startx(i+1) = r_s(1)
              starty(i+1) = r_s(2)
           end if
        end do
     else
           tag =  1
           call mpi_send(r_s,2,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
     end if

     call mpp_land_sync()

! calculate the routing land start x and y
     local_startx_rt = local_startx*rt_AGGFACTRT - (rt_AGGFACTRT-1)
     if(local_startx_rt.gt.1) local_startx_rt=local_startx_rt - 1
     local_starty_rt = local_starty*rt_AGGFACTRT - (rt_AGGFACTRT-1)
     if(local_starty_rt.gt.1) local_starty_rt=local_starty_rt - 1

     local_endx_rt   = local_startx_rt + local_rt_nx -1
     local_endy_rt   = local_starty_rt + local_rt_ny -1

     return
  end subroutine calculate_start_p

  subroutine decompose_data_real3d (in_buff,out_buff,klevel)
      implicit none
      integer:: klevel, k
      real,dimension(:,:,:) ::  in_buff,out_buff
      do k = 1, klevel
          call decompose_data_real(in_buff(:,k,:),out_buff(:,k,:))
      end do
  end subroutine decompose_data_real3d


  subroutine decompose_data_real (in_buff,out_buff)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      real,intent(in), dimension(:,:) :: in_buff
      real,intent(out), dimension(local_nx,local_ny) :: out_buff
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
                  MPI_REAL, i,tag,HYDRO_COMM_WORLD,ierr)
            end if
         end do
      else 
         size = local_nx*local_ny
         call mpi_recv(out_buff,size,MPI_REAL,IO_id, &
                tag,HYDRO_COMM_WORLD,mpp_status,ierr)
      end if
      return
  end subroutine decompose_data_real


  subroutine decompose_data_int (in_buff,out_buff)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      integer,dimension(:,:) ::  in_buff,out_buff
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
                  MPI_INTEGER, i,tag,HYDRO_COMM_WORLD,ierr)
            end if
         end do
      else 
         size = local_nx*local_ny
         call mpi_recv(out_buff,size,MPI_INTEGER,IO_id, &
                tag,HYDRO_COMM_WORLD,mpp_status,ierr)
      end if
      return
  end subroutine decompose_data_int

  subroutine write_IO_int(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      integer,dimension(:,:):: in_buff,  out_buff
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_nx*local_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_INTEGER, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
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
                   MPI_INTEGER,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_int

  subroutine write_IO_char_head(in, out, imageHead)
  !! JLM 2015-11-30
  !! for i is image number (starting from 0), 
  !! this routine writes 
  !! in(1:imageHead(i+1)) 
  !! to 
  !! out( (sum(imageHead(i+1-1))+1) : ((sum(imageHead(i+1-1))+1)+imageHead(i+1)) )
  !! where out is on the IO node.
      character(len=*), intent(in),  dimension(:) :: in 
      character(len=*), intent(out), dimension(:) :: out
      integer,   intent(in),  dimension(:) :: imageHead
      integer :: tag, i, status, ierr, size
      integer :: ibegin,iend,jbegin,jend
      integer :: lenSize, theStart, theEnd
      tag = 2 
      if(my_id .ne. IO_id) then
         lenSize = imageHead(my_id+1)*len(in(1))  !! some times necessary for character arrays?
         if(lenSize .eq. 0) return
         call mpi_send(in,lenSize,MPI_CHARACTER,IO_id,tag,HYDRO_COMM_WORLD,ierr)
      else
         do i = 0, numprocs-1
            lenSize  = imageHead(i+1)*len(in(1))  !! necessary?
            if(lenSize .eq. 0) cycle
            if(i .eq. 0) then
               theStart = 1
            else 
               theStart = sum(imageHead(1:(i+1-1))) +1
            end if
            theEnd   = theStart + imageHead(i+1) -1
            if(i .eq. IO_id) then
               out(theStart:theEnd) = in(1:imageHead(i+1))
            else 
               call mpi_recv(out(theStart:theEnd),lenSize,&
                    MPI_CHARACTER,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            end if
         end do
      end if
  end subroutine write_IO_char_head


  subroutine write_IO_real3d(in_buff,out_buff,klevel)
     implicit none
! the IO node will receive the data from the rest process.
      integer klevel, k
      real,dimension(:,:,:):: in_buff, out_buff
      do k = 1, klevel
         call write_IO_real(in_buff(:,k,:),out_buff(:,k,:))
      end do
  end subroutine write_IO_real3d

  subroutine write_IO_real(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      real,dimension(:,:):: in_buff, out_buff
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_nx*local_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_REAL, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
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
                   MPI_REAL,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_real

  subroutine write_IO_RT_real(in_buff,out_buff)
! the IO node will receive the data from the rest process.
      real,dimension(:,:) ::  in_buff, out_buff
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_rt_nx*local_rt_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_REAL, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
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
                   MPI_REAL,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_RT_real


  subroutine write_IO_RT_int (in_buff,out_buff)
! the IO node will receive the data from the rest process.
      integer,intent(in),dimension(:,:) :: in_buff
      integer,intent(out),dimension(:,:) ::  out_buff
      integer tag, i, status, ierr,size
      integer ibegin,iend,jbegin,jend
      if(my_id .ne. IO_id) then
          size = local_rt_nx*local_rt_ny
          tag = 2
          call mpi_send(in_buff,size,MPI_INTEGER, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
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
                   MPI_INTEGER,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            end if
          end do
      end if
      return
  end subroutine write_IO_RT_int

  subroutine mpp_land_bcast_log1(inout)
      logical inout
      integer ierr
        call mpi_bcast(inout,1,MPI_LOGICAL,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_log1


  subroutine mpp_land_bcast_int(size,inout)
      integer size
      integer inout(size)
      integer ierr
        call mpi_bcast(inout,size,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int

  subroutine mpp_land_bcast_int1d(inout)
      integer len 
      integer inout(:)
     integer ierr
      len = size(inout,1)
        call mpi_bcast(inout,len,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int1d

  subroutine mpp_land_bcast_int1d_root(inout, rootId)
     integer len 
     integer inout(:)
     integer, intent(in) :: rootId
     integer ierr
      len = size(inout,1)
        call mpi_bcast(inout,len,MPI_INTEGER,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int1d_root

  subroutine mpp_land_bcast_int1(inout)
      integer inout
      integer ierr
        call mpi_bcast(inout,1,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int1

  subroutine mpp_land_bcast_int1_root(inout, rootId)
      integer inout
      integer ierr
      integer, intent(in) :: rootId
        call mpi_bcast(inout,1,MPI_INTEGER,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_int1_root

  subroutine mpp_land_bcast_logical(inout)
      logical ::  inout
      integer ierr
        call mpi_bcast(inout,1,MPI_LOGICAL,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_logical

  subroutine mpp_land_bcast_logical_root(inout, rootId)
      logical ::  inout
      integer, intent(in) :: rootId
      integer ierr
        call mpi_bcast(inout,1,MPI_LOGICAL,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_logical_root


  subroutine mpp_land_bcast_real1(inout)
      real inout
      integer ierr
        call mpi_bcast(inout,1,MPI_REAL,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return 
  end subroutine mpp_land_bcast_real1

  subroutine mpp_land_bcast_real_1d(inout)
      integer len
      real inout(:)
      integer ierr
      len = size(inout,1) 
        call mpi_bcast(inout,len,MPI_real,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real_1d


  subroutine mpp_land_bcast_real_1d_root(inout, rootId)
      integer len
      real inout(:)
      integer, intent(in) :: rootId
      integer ierr
      len = size(inout,1)
        call mpi_bcast(inout,len,MPI_real,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
    end subroutine mpp_land_bcast_real_1d_root


  subroutine mpp_land_bcast_real8_1d(inout)
      integer len
      real*8 inout(:)
      integer ierr
      len = size(inout,1)
        call mpi_bcast(inout,len,MPI_double,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real8_1d


  subroutine mpp_land_bcast_real(size1,inout)
      integer size1
      ! real inout(size1)
      real , dimension(:) :: inout
      integer ierr, len
        call mpi_bcast(inout,size1,MPI_real,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
        call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real

  subroutine mpp_land_bcast_int2d(inout)
      integer length1, k,length2
      integer inout(:,:)
      integer ierr
      length1 = size(inout,1)
      length2 = size(inout,2)
      do k = 1, length2
        call mpi_bcast(inout(:,k),length1,MPI_INTEGER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      end do
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_int2d

  subroutine mpp_land_bcast_real2(inout)
      integer length1, k,length2
      real inout(:,:)
      integer ierr
      length1 = size(inout,1)
      length2 = size(inout,2)
      do k = 1, length2
        call mpi_bcast(inout(:,k),length1,MPI_real,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      end do
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real2

  subroutine mpp_land_bcast_real3d(inout)
      integer j, k, length1, length2, length3
      real inout(:,:,:)
      integer ierr
      length1 = size(inout,1)
      length2 = size(inout,2)
      length3 = size(inout,3)
      do k = 1, length3
         do j = 1, length2
            call mpi_bcast(inout(:,j,k), length1, MPI_real, &
                 IO_id, HYDRO_COMM_WORLD, ierr)
         end do
      end do
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_real3d
  
  subroutine mpp_land_bcast_rd(size,inout)
      integer size
      real*8 inout(size)
      integer ierr
        call mpi_bcast(inout,size,MPI_REAL8,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_rd

  subroutine mpp_land_bcast_char(size,inout)
      integer size
      character inout(*)
      integer ierr
        call mpi_bcast(inout,size,MPI_CHARACTER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char

  subroutine mpp_land_bcast_char_root(size,inout,rootId)
      integer size
      character inout(*)
      integer, intent(in) :: rootId
      integer ierr
        call mpi_bcast(inout,size,MPI_CHARACTER,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char_root


  subroutine mpp_land_bcast_char1d(inout)
      character(len=*) :: inout(:)
      integer :: lenSize
      integer :: ierr
      lenSize = size(inout,1)*len(inout)
      call mpi_bcast(inout,lenSize,MPI_CHARACTER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char1d

  subroutine mpp_land_bcast_char1d_root(inout,rootId)
      character(len=*) :: inout(:)
      integer, intent(in) :: rootId
      integer :: lenSize
      integer :: ierr
      lenSize = size(inout,1)*len(inout)
      call mpi_bcast(inout,lenSize,MPI_CHARACTER,rootId,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char1d_root

  subroutine mpp_land_bcast_char1(inout)
      integer len
      character(len=*) inout
      integer ierr
      len = LEN_TRIM(inout)
      call mpi_bcast(inout,len,MPI_CHARACTER,   &
            IO_id,HYDRO_COMM_WORLD,ierr)
      call mpp_land_sync()
    return
  end subroutine mpp_land_bcast_char1

 
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
      real,intent(in),dimension(:,:) :: in_buff
      real,intent(out),dimension(:,:) :: out_buff
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
                  MPI_REAL, i,tag,HYDRO_COMM_WORLD,ierr)
            end if
         end do
      else
         size = nx*ny
         call mpi_recv(out_buff,size,MPI_REAL,IO_id, &
                tag,HYDRO_COMM_WORLD,mpp_status,ierr)
      end if
      return
  end subroutine decompose_RT_real

   subroutine decompose_RT_int (in_buff,out_buff,g_nx,g_ny,nx,ny)
! usage: all of the cpu call this subroutine.
! the IO node will distribute the data to rest of the node.
      integer g_nx,g_ny,nx,ny
      integer,intent(in),dimension(:,:) ::  in_buff
      integer,intent(out),dimension(:,:) :: out_buff
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
                  MPI_INTEGER, i,tag,HYDRO_COMM_WORLD,ierr)
            end if
         end do
      else
         size = nx*ny
         call mpi_recv(out_buff,size,MPI_INTEGER,IO_id, &
                tag,HYDRO_COMM_WORLD,mpp_status,ierr)
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

      call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_id, ierr )
      call MPI_COMM_SIZE( HYDRO_COMM_WORLD, numprocs, ierr )

      if(numprocs .ne. total_pe) then
         write(6,*) "FATAL ERROR: In wrf_LAND_set_INIT() - numprocs .ne. total_pe ",numprocs, total_pe 
         call mpp_land_abort()
      endif


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
          call MPI_COMM_RANK( HYDRO_COMM_WORLD, my_id, ierr )
      return
      end subroutine getMy_global_id

  subroutine MPP_CHANNEL_COM_REAL(Link_location,ix,jy,Link_V,size,flag)
  ! communicate the data for channel routine.
      implicit none
      integer ix,jy,size
      integer Link_location(ix,jy)
      integer i,j, flag
      real Link_V(size), tmp_inout(ix,jy)

      tmp_inout = -999

      if(size .eq. 0) then  
            tmp_inout = -999
      else

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
    endif

!   commu nicate tmp_inout
    call MPP_LAND_COM_REAL(tmp_inout, ix,jy,flag)

!map the data back to Link_V
    if(size .eq. 0) return
      do j = 1,jy 
            if( (Link_location(1,j) .gt. 0) .and. (tmp_inout(1,j) .ne. -999) ) &
               Link_V(Link_location(1,j)) = tmp_inout(1,j)
            if((Link_location(2,j) .gt. 0) .and. (tmp_inout(2,j) .ne. -999) ) &
               Link_V(Link_location(2,j)) = tmp_inout(2,j)
            if((Link_location(ix-1,j) .gt. 0) .and. (tmp_inout(ix-1,j) .ne. -999)) &
               Link_V(Link_location(ix-1,j)) = tmp_inout(ix-1,j)
            if((Link_location(ix,j) .gt. 0) .and. (tmp_inout(ix,j) .ne. -999) )&
               Link_V(Link_location(ix,j)) = tmp_inout(ix,j)
      enddo
      do i = 1,ix 
            if((Link_location(i,1) .gt. 0) .and. (tmp_inout(i,1) .ne. -999) )&
               Link_V(Link_location(i,1)) = tmp_inout(i,1)
            if( (Link_location(i,2) .gt. 0) .and. (tmp_inout(i,2) .ne. -999) )&
               Link_V(Link_location(i,2)) = tmp_inout(i,2)
            if((Link_location(i,jy-1) .gt. 0) .and. (tmp_inout(i,jy-1) .ne. -999) ) &
               Link_V(Link_location(i,jy-1)) = tmp_inout(i,jy-1)
            if((Link_location(i,jy) .gt. 0) .and. (tmp_inout(i,jy) .ne. -999) ) &
               Link_V(Link_location(i,jy)) = tmp_inout(i,jy)
      enddo
  end subroutine MPP_CHANNEL_COM_REAL

  subroutine MPP_CHANNEL_COM_INT(Link_location,ix,jy,Link_V,size,flag)
  ! communicate the data for channel routine.
      implicit none
      integer ix,jy,size
      integer Link_location(ix,jy)
      integer i,j, flag
      integer Link_V(size), tmp_inout(ix,jy)

      if(size .eq. 0) then  
           tmp_inout = -999
      else

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
    endif

!   commu nicate tmp_inout
    call MPP_LAND_COM_INTEGER(tmp_inout, ix,jy,flag)

!map the data back to Link_V
    if(size .eq. 0) return
      do j = 1,jy 
            if( (Link_location(1,j) .gt. 0) .and. (tmp_inout(1,j) .ne. -999) ) &
               Link_V(Link_location(1,j)) = tmp_inout(1,j)
            if((Link_location(2,j) .gt. 0) .and. (tmp_inout(2,j) .ne. -999) ) &
               Link_V(Link_location(2,j)) = tmp_inout(2,j)
            if((Link_location(ix-1,j) .gt. 0) .and. (tmp_inout(ix-1,j) .ne. -999)) &
               Link_V(Link_location(ix-1,j)) = tmp_inout(ix-1,j)
            if((Link_location(ix,j) .gt. 0) .and. (tmp_inout(ix,j) .ne. -999) )&
               Link_V(Link_location(ix,j)) = tmp_inout(ix,j)
      enddo
      do i = 1,ix 
            if((Link_location(i,1) .gt. 0) .and. (tmp_inout(i,1) .ne. -999) )&
               Link_V(Link_location(i,1)) = tmp_inout(i,1)
            if( (Link_location(i,2) .gt. 0) .and. (tmp_inout(i,2) .ne. -999) )&
               Link_V(Link_location(i,2)) = tmp_inout(i,2)
            if((Link_location(i,jy-1) .gt. 0) .and. (tmp_inout(i,jy-1) .ne. -999) ) &
               Link_V(Link_location(i,jy-1)) = tmp_inout(i,jy-1)
            if((Link_location(i,jy) .gt. 0) .and. (tmp_inout(i,jy) .ne. -999) ) &
               Link_V(Link_location(i,jy)) = tmp_inout(i,jy)
      enddo
  end subroutine MPP_CHANNEL_COM_INT
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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                 if(max <= r1) max = r1 
              end if
           end do
       else
           tag =  101
           call mpi_send(v,1,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                 if(max <= r1) max = r1 
              end if
           end do
       else
           tag =  101
           call mpi_send(v,1,MPI_REAL, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                 if(v .ne. r1) v = -99  
              end if
           end do
       else
           tag =  109
           call mpi_send(v,1,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
       end if
       call mpp_land_bcast_int1(v)
     end subroutine mpp_same_int1



     subroutine write_chanel_real(v,map_l2g,gnlinks,nlinks,g_v)   
        implicit none
        integer  gnlinks,nlinks, map_l2g(nlinks)
        real recv(nlinks), v(nlinks)
        ! real g_v(gnlinks), tmp_v(gnlinks)
        integer i, ierr, tag, k
        integer length, node, message_len
        integer,allocatable,dimension(:) :: tmp_map
        real, allocatable, dimension(:) :: tmp_v
        real, dimension(:) :: g_v

        if(my_id .eq. io_id) then
           allocate(tmp_map(gnlinks))
           allocate(tmp_v(gnlinks))
           if(nlinks .le. 0) then
               tmp_map = -999
           else
               tmp_map(1:nlinks) = map_l2g(1:nlinks)
           endif
        else
           allocate(tmp_map(1))
           allocate(tmp_v(1))
        endif

        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              message_len = mpp_nlinks(i+1)
              if(i .ne. my_id) then
                 !block receive  from other node.

                 tag = 109
                 call mpi_recv(tmp_map(1:message_len),message_len,MPI_INTEGER,i, &
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                   tag = 119

                 call mpi_recv(tmp_v(1:message_len),message_len,MPI_REAL,i,  &
                   tag,HYDRO_COMM_WORLD,mpp_status,ierr)

                 do k = 1,message_len
                    node = tmp_map(k) 
                    if(node .gt. 0) then
                      g_v(node) = tmp_v(k)
                    else
#ifdef HYDRO_D
                      write(6,*) "Maping infor k=",k," node=", node
#endif
                    endif
                 enddo
              else
                 do k = 1,nlinks
                    node = map_l2g(k) 
                    if(node .gt. 0) then
                      g_v(node) = v(k)
                    else
#ifdef HYDRO_D
                      write(6,*) "local Maping infor k=",k," node=",node 
#endif
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  109
           call mpi_send(map_l2g,nlinks,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
           tag = 119
           call mpi_send(v,nlinks,MPI_REAL,IO_id,   &
               tag,HYDRO_COMM_WORLD,ierr)

        end if
           if(allocated(tmp_map)) deallocate(tmp_map)
           if(allocated(tmp_v)) deallocate(tmp_v)
     end subroutine write_chanel_real

     subroutine write_chanel_int(v,map_l2g,gnlinks,nlinks,g_v)   
        implicit none
        integer gnlinks,nlinks, map_l2g(nlinks)
        integer ::  recv(nlinks), v(nlinks)
        integer, allocatable, dimension(:) :: tmp_map , tmp_v
        integer, dimension(:) :: g_v
        integer i, ierr, tag, k
        integer length, node, message_len

        if(my_id .eq. io_id) then
           allocate(tmp_map(gnlinks))
           allocate(tmp_v(gnlinks))
           if(nlinks .le. 0) then
               tmp_map = -999
           else
               tmp_map(1:nlinks) = map_l2g(1:nlinks)
           endif
        else
           allocate(tmp_map(1))
           allocate(tmp_v(1))
        endif


        if(my_id .eq. IO_id) then
           do i = 0, numprocs - 1
              message_len = mpp_nlinks(i+1)
              if(i .ne. my_id) then
                 !block receive  from other node.

                 tag = 109
                 call mpi_recv(tmp_map(1:message_len),message_len,MPI_INTEGER,i, &
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                   tag = 119

                 call mpi_recv(tmp_v(1:message_len),message_len,MPI_INTEGER,i,  &
                   tag,HYDRO_COMM_WORLD,mpp_status,ierr)

                 do k = 1,message_len
                    if(tmp_map(k) .gt. 0) then
                      node = tmp_map(k) 
                      g_v(node) = tmp_v(k)
                    else 
#ifdef HYDRO_D
                      write(6,*) "Maping infor k=",k," node=",tmp_v(k)
#endif
                    endif
                 enddo
              else
                 do k = 1,nlinks
                    if(map_l2g(k) .gt. 0) then
                      node = map_l2g(k) 
                      g_v(node) = v(k)
                    else
#ifdef HYDRO_D
                      write(6,*) "Maping infor k=",k," node=",map_l2g(k)
#endif
                    endif
                 enddo
              end if
            
           end do
        else
           tag =  109
           call mpi_send(map_l2g,nlinks,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
           tag = 119
           call mpi_send(v,nlinks,MPI_INTEGER,IO_id,   &
               tag,HYDRO_COMM_WORLD,ierr)
        end if
           if(allocated(tmp_map)) deallocate(tmp_map)
           if(allocated(tmp_v)) deallocate(tmp_v)
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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                   tag = 139
                 call mpi_recv(recv(:),nlakes,MPI_REAL,i,  &
                   tag,HYDRO_COMM_WORLD,mpp_status,ierr)

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
               tag,HYDRO_COMM_WORLD,ierr)
           tag = 139
           call mpi_send(v,nlakes,MPI_REAL,IO_id,   &
               tag,HYDRO_COMM_WORLD,ierr)
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
            IO_id,HYDRO_COMM_WORLD,ierr)
     return
     end subroutine read_rst_crt_r  

         subroutine write_rst_crt_r(unit,cd,map_l2g,gnlinks,nlinks)
         integer :: unit,gnlinks,nlinks,map_l2g(nlinks)
         real cd(nlinks)
         real g_cd (gnlinks)
         call write_chanel_real(cd,map_l2g,gnlinks,nlinks, g_cd)
         write(unit) g_cd
         return
         end subroutine write_rst_crt_r

    subroutine sum_int1d(vin,nsize)
       implicit none
       integer nsize,i,j,tag,ierr
       integer, dimension(nsize):: vin,recv
       tag = 319
       if(nsize .le. 0) return
       if(my_id .eq. IO_id) then
          do i = 0, numprocs - 1
             if(i .ne. my_id) then
               call mpi_recv(recv,nsize,MPI_INTEGER,i,  &
                    tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               vin(:) = vin(:) + recv(:)
             endif
          end do
       else
             call mpi_send(vin,nsize,MPI_INTEGER,IO_id,   &
                  tag,HYDRO_COMM_WORLD,ierr)
       endif
       call mpp_land_bcast_int1d(vin) 
       return
    end subroutine sum_int1d

    subroutine combine_int1d(vin,nsize, flag)
       implicit none
       integer nsize,i,j,tag,ierr, flag, k
       integer, dimension(nsize):: vin,recv
       tag = 319
       if(nsize .le. 0) return
       if(my_id .eq. IO_id) then
          do i = 0, numprocs - 1
             if(i .ne. my_id) then
               call mpi_recv(recv,nsize,MPI_INTEGER,i,  &
                    tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               do k = 1, nsize
                  if(recv(k) .ne. flag) then
                     vin(k) = recv(k)
                  endif
               enddo
             endif
          end do
       else
             call mpi_send(vin,nsize,MPI_INTEGER,IO_id,   &
                  tag,HYDRO_COMM_WORLD,ierr)
       endif
       call mpp_land_bcast_int1d(vin)
       return
    end subroutine combine_int1d



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
                    tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               vin(:) = vin(:) + recv(:)
             endif
          end do
          v = vin
       else
             call mpi_send(vin,nsize,MPI_DOUBLE_PRECISION,IO_id,   &
                  tag,HYDRO_COMM_WORLD,ierr)
       endif
       call mpp_land_bcast_real(nsize,v) 
       vin = v
       return
    end subroutine sum_real8

!  subroutine get_globalDim(ix,g_ix)
!     implicit none
!     integer ix,g_ix, ierr
!     include "mpif.h"
!
!     if ( my_id .eq. IO_id ) then
!           g_ix = ix
!        call mpi_reduce( MPI_IN_PLACE, g_ix, 4, MPI_INTEGER, &
!             MPI_SUM, 0, HYDRO_COMM_WORLD, ierr )
!     else
!        call mpi_reduce( ix,       0,      4, MPI_INTEGER, &
!             MPI_SUM,  0, HYDRO_COMM_WORLD, ierr )
!     endif
!      call mpp_land_bcast_int1(g_ix)
!
!     return
!
!  end subroutine get_globalDim

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
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)

                 tag = 203
                 e = index_s(2)
                 s = index_s(1)
                 size = e - s + 1 
                 call mpi_recv(vg(s:e),size,MPI_REAL,  &
                    i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
              endif
         end do
     else 
           tag =  202
           call mpi_send(index_s,2,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)

           tag =  203  
           call mpi_send(vl,size,MPI_REAL,IO_id,   &
               tag,HYDRO_COMM_WORLD,ierr)
     end if

     return 
  end  subroutine gather_1d_real_tmp

  subroutine sum_real1(inout)
      implicit none
      real:: inout, send
      integer :: ierr
      send = inout
      CALL MPI_ALLREDUCE(send,inout,1,MPI_REAL,MPI_SUM,HYDRO_COMM_WORLD,ierr)
  end subroutine sum_real1 

  subroutine sum_double(inout)
      implicit none
      real*8:: inout, send
      integer :: ierr
      send = inout
      !yw CALL MPI_ALLREDUCE(send,inout,1,MPI_DOUBLE,MPI_SUM,HYDRO_COMM_WORLD,ierr)
      CALL MPI_ALLREDUCE(send,inout,1,MPI_DOUBLE_PRECISION,MPI_SUM,HYDRO_COMM_WORLD,ierr)
  end subroutine sum_double

  subroutine mpp_chrt_nlinks_collect(nlinks)
  ! collect the nlinks
       implicit none
       integer :: nlinks
       integer :: i, ierr, status, tag
       allocate(mpp_nlinks(numprocs),stat = status) 
                 tag = 138
       mpp_nlinks = 0
       if(my_id .eq. IO_id) then
          do i = 0,numprocs -1
            if(i .ne. my_id) then
               call mpi_recv(mpp_nlinks(i+1),1,MPI_INTEGER,i, &
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
            else
               mpp_nlinks(i+1) = 0
            end if
          end do
       else
           call mpi_send(nlinks,1,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
       endif

     
  end subroutine mpp_chrt_nlinks_collect

     subroutine  getLocalXY(ix,jx,startx,starty,endx,endy)
!!! this is for NoahMP only
        implicit none
        integer:: ix,jx,startx,starty,endx,endy
        startx = local_startx
        starty = local_starty
        endx = startx + ix -1
        endy = starty + jx -1
     end subroutine getLocalXY

     subroutine check_landreal1(unit, inVar)
        implicit none
        integer :: unit
        real :: inVar
        if(my_id .eq. IO_id) then
           write(unit,*) inVar
           call flush(unit)
        endif
     end subroutine check_landreal1

     subroutine check_landreal1d(unit, inVar)
        implicit none
        integer :: unit
        real :: inVar(:)
        if(my_id .eq. IO_id) then
           write(unit,*) inVar
           call flush(unit)
        endif
     end subroutine check_landreal1d
     subroutine check_landreal2d(unit, inVar)
        implicit none
        integer :: unit
        real :: inVar(:,:)
        real :: g_var(global_nx,global_ny)
        call write_io_real(inVar,g_var) 
        if(my_id .eq. IO_id) then
           write(unit,*) g_var 
           call flush(unit)
        endif
     end subroutine check_landreal2d

     subroutine check_landreal3d(unit, inVar)
        implicit none
        integer :: unit, k, klevel
        real :: inVar(:,:,:)
        real :: g_var(global_nx,global_ny)
        klevel = size(inVar,2)
        do k = 1, klevel
           call write_io_real(inVar(:,k,:),g_var) 
           if(my_id .eq. IO_id) then
              write(unit,*) g_var
              call flush(unit)
           endif
        end do
     end subroutine check_landreal3d

     subroutine mpp_collect_1d_int(nlinks,vinout)
  ! collect the nlinks
       implicit none
       integer :: nlinks
       integer :: i, ierr, status, tag
       integer, dimension(nlinks) :: vinout
       integer, dimension(nlinks) :: buf
       tag = 139
       call mpp_land_sync()
       if(my_id .eq. IO_id) then
          do i = 0,numprocs -1
            if(i .ne. my_id) then
               call mpi_recv(buf,nlinks,MPI_INTEGER,i, &
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               vinout = vinout + buf
            end if
          end do
       else
           call mpi_send(vinout,nlinks,MPI_INTEGER, IO_id,     &
               tag,HYDRO_COMM_WORLD,ierr)
       endif
       call mpp_land_sync()
       call mpp_land_bcast_int1d(vinout)
    
  end subroutine mpp_collect_1d_int

  subroutine mpp_collect_1d_int_mem(nlinks,vinout)
  ! consider the memory and big size data transport
  ! collect the nlinks
       implicit none
       integer :: nlinks
       integer :: i, ierr, status, tag
       integer, dimension(nlinks) :: vinout, tmpIn
       integer, dimension(nlinks) :: buf
       integer :: lsize, k,m
       integer, allocatable, dimension(:) :: tmpBuf

       call mpp_land_sync()
       if(my_id .eq. IO_id) then
          allocate (tmpBuf(nlinks))
          do i = 0,numprocs -1
            if(i .ne. my_id) then
               tag = 120
               call mpi_recv(lsize,1,MPI_INTEGER,i, &
                      tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               if(lsize .gt. 0) then
                   tag = 121
                   call mpi_recv(tmpBuf(1:lsize),lsize,MPI_INTEGER,i, &
                        tag,HYDRO_COMM_WORLD,mpp_status,ierr)
                   do k = 1, lsize
                      m = tmpBuf(k)
                      vinout(m) = 1
                   end do
               endif
            end if
          end do
          if(allocated(tmpBuf)) deallocate(tmpBuf)
       else 
           lsize = 0
           do k = 1, nlinks
               if(vinout(k) .gt. 0) then
                  lsize = lsize + 1
                  tmpIn(lsize) = k        
               end if
           end do 
           tag = 120
           call mpi_send(lsize,1,MPI_INTEGER, IO_id,     &
                 tag,HYDRO_COMM_WORLD,ierr)
           if(lsize .gt. 0) then
              tag = 121
              call mpi_send(tmpIn(1:lsize),lsize,MPI_INTEGER, IO_id,     &
                 tag,HYDRO_COMM_WORLD,ierr)
           endif
       endif
       call mpp_land_sync()
       call mpp_land_bcast_int1d(vinout)
   
  end subroutine mpp_collect_1d_int_mem

! stop the job due to the fatal error.
      subroutine fatal_error_stop(msg)
        character(len=*) :: msg
        integer :: ierr
      write(6,*) "The job is stoped due to the fatal error. ", trim(msg)
      call flush(6)
      call mpp_land_abort()
      call MPI_finalize(ierr)
     return
     end  subroutine fatal_error_stop

     subroutine updateLake_seqInt(in,nsize,in0)
       implicit none
       integer :: nsize
       integer, dimension(nsize) :: in
       integer, dimension(nsize) :: tmp
       integer, dimension(:) :: in0
       integer tag, i, status, ierr, k
       if(nsize .le. 0) return

       tag = 29
       if(my_id .ne. IO_id) then
          call mpi_send(in,nsize,MPI_INTEGER, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
       else
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               call mpi_recv(tmp,nsize,&
                   MPI_INTEGER,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               do k = 1, nsize 
                  if(in0(k) .ne. tmp(k)) in(k) = tmp(k)
               end do
            end if
          end do
       end if
       call mpp_land_bcast_int1d(in)
     
     end subroutine updateLake_seqInt

     subroutine updateLake_seq(in,nsize,in0)
       implicit none
       integer :: nsize
       real, dimension(nsize) :: in
       real, dimension(nsize) :: tmp
       real, dimension(:) :: in0
       integer tag, i, status, ierr, k
       if(nsize .le. 0) return

       tag = 29
       if(my_id .ne. IO_id) then
          call mpi_send(in,nsize,MPI_REAL, IO_id,     &
                tag,HYDRO_COMM_WORLD,ierr)
       else
          do i = 0, numprocs - 1
            if(i .ne. IO_id) then
               call mpi_recv(tmp,nsize,&
                   MPI_REAL,i,tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               do k = 1, nsize 
                  if(in0(k) .ne. tmp(k)) in(k) = tmp(k)
               end do
            end if
          end do
       end if
       call mpp_land_bcast_real_1d(in)
     
     end subroutine updateLake_seq

!subroutine match1dLake:
!global lake. Find the same lake and mark as flag
! default of win is 0
    subroutine match1dLake(vin,nsize,flag)
       implicit none
       integer nsize,i,j,tag,ierr, flag, k
       integer, dimension(nsize):: vin,recv
       tag = 319
       if(nsize .le. 0) return
       if(my_id .eq. IO_id) then
          do i = 0, numprocs - 1
             if(i .ne. my_id) then
               call mpi_recv(recv,nsize,MPI_INTEGER,i,  &
                    tag,HYDRO_COMM_WORLD,mpp_status,ierr)
               do k = 1, nsize 
                 if(recv(k) .eq. flag) vin(k) = flag
                 if(vin(k) .ne. flag) then
                   if(vin(k) .gt. 0 .and. recv(k) .gt. 0) then
                       vin(k) = flag
                   else
                       if(recv(k) .gt. 0) vin(k) = recv(k)
                   endif 
                 endif
               end do
             endif
          end do
       else
             call mpi_send(vin,nsize,MPI_INTEGER,IO_id,   &
                  tag,HYDRO_COMM_WORLD,ierr)
       endif   
       call mpp_land_bcast_int1d(vin)
       return  
    end subroutine match1dLake

        subroutine mpp_land_abort()
            implicit none
            integer ierr
            CALL MPI_ABORT(HYDRO_COMM_WORLD,1,IERR)
        end subroutine mpp_land_abort ! mpp_land_abort

  subroutine mpp_land_sync()
      implicit none
      integer ierr
      call MPI_barrier( HYDRO_COMM_WORLD ,ierr)
      if(ierr .ne. 0) call mpp_land_abort()
      return
  end subroutine mpp_land_sync ! mpp_land_sync


    subroutine mpp_comm_scalar_real(scalar, fromImage, toImage)
    implicit none
    real,    intent(inout) :: scalar
    integer, intent(in)    :: fromImage, toImage
    integer:: ierr, tag
    tag=2   
    if(my_id .eq. fromImage) &
         call mpi_send(scalar, 1, MPI_REAL, &
                       toImage, tag, HYDRO_COMM_WORLD, ierr)
    if(my_id .eq. toImage) &
         call mpi_recv(scalar, 1, MPI_REAL, &
                       fromImage, tag, HYDRO_COMM_WORLD, mpp_status, ierr)
    end subroutine mpp_comm_scalar_real

    subroutine mpp_comm_scalar_char(scalar, fromImage, toImage)
    implicit none
    character(len=*), intent(inout) :: scalar
    integer,          intent(in)    :: fromImage, toImage
    integer:: ierr, tag, length
    tag=2
    length=len(scalar)
    if(my_id .eq. fromImage) &
         call mpi_send(scalar, length, MPI_CHARACTER, &
                       toImage, tag, HYDRO_COMM_WORLD, ierr)
    if(my_id .eq. toImage) &
         call mpi_recv(scalar, length, MPI_CHARACTER, &
                       fromImage, tag, HYDRO_COMM_WORLD, mpp_status, ierr)
    end subroutine mpp_comm_scalar_char

    
    
END MODULE MODULE_MPP_LAND



