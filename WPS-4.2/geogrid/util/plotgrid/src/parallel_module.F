!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MODULE PARALLEL_MODULE
!
! This module provides routines for parallelizing.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module parallel_module

#ifdef _MPI
include 'mpif.h'
#endif

   integer, parameter :: IO_NODE = 0

   integer, parameter :: HALO_WIDTH = 3
 
   integer, pointer, dimension(:,:) :: processors, &
                                       proc_minx, proc_maxx, &
                                       proc_miny, proc_maxy
   integer :: nprocs, &
              my_proc_id, &
              nproc_x, nproc_y, &
              my_x, my_y, &
              my_minx, my_miny, my_maxx, my_maxy, &
              comm
 

   contains
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_start
   !
   ! Purpose: For MPI, the purpose of this routine is to basically set up
   !   a communicator for a rectangular mesh, and determine how many processors
   !   in the x and y directions there will be. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_start()
 
      implicit none
  
      ! Arguments
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_rank, mpi_size
      integer :: mpi_ierr
      integer, dimension(2) :: dims, coords
      integer :: rectangle, myleft, myright, mytop, mybottom
      integer :: mini, m, n
      logical, dimension(2) :: periods
  
      ! Find out our rank and the total number of processors
      call MPI_Init(mpi_ierr)
      call MPI_Comm_rank(MPI_COMM_WORLD, mpi_rank, mpi_ierr)
      call MPI_Comm_size(MPI_COMM_WORLD, mpi_size, mpi_ierr)

      comm = MPI_COMM_WORLD

      nprocs = mpi_size
      my_proc_id = mpi_rank

      ! Code from RSL to get number of procs in m and n directions
      mini = 2*nprocs
      nproc_x = 1
      nproc_y = nprocs
      do m = 1, nprocs
        if ( mod( nprocs, m ) == 0 ) then
          n = nprocs / m
          if ( abs(m-n) < mini  ) then
            mini = abs(m-n)
            nproc_x = m
            nproc_y = n
          end if
        end if
      end do

      ! Calculate which patch current processor will work on
      my_x = mod(mpi_rank,nproc_x) 
      my_y = mpi_rank / nproc_x

#else
      comm = 0
      my_proc_id = IO_NODE
      nprocs = 1
      my_x = 0
      my_y = 0
      nproc_x = 1
      nproc_y = 1
#endif
  
      nullify(processors)
      nullify(proc_minx)
      nullify(proc_maxx)
      nullify(proc_miny)
      nullify(proc_maxy)
  
   end subroutine parallel_start 
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_get_tile_dims
   !
   ! Purpose: To compute the starting and ending indices of the patch that the
   !   calling processor is to work on. When there are multiple processors, 
   !   appropriate data structures describing the range of indices being 
   !   worked on by other processors are also allocated and filled
   !   (processors, proc_minx, proc_maxx, proc_miny, proc_maxy).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_get_tile_dims(idim, jdim)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: idim, jdim
  
      ! Local variables
#ifdef _MPI
      integer :: i, j, ix, iy, px, py
      integer, dimension(2) :: buffer
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
  
      !
      ! Determine starting and ending grid points in x and y direction that we will work on
      !
      ! NOTE:
      ! For now, copy code from RSL_LITE's module_dm.F until build mechanism to link 
      ! WRF and WPS code is worked out more.
      ! Eventually, it would probably be best to use module_dm code without copying
      !
      my_minx = -1
      j = 1
      do i = 1, idim
         call task_for_point(i, j, 1, idim, 1, jdim, nproc_x, nproc_y, px, py)
         if ( px == my_x ) then
            my_maxx = i
            if ( my_minx == -1 ) my_minx = i
         end if
      end do

      my_miny = -1
      i = 1
      do j = 1, jdim
         call task_for_point(i, j, 1, idim, 1, jdim, nproc_x, nproc_y, px, py)
         if ( py == my_y ) then
            my_maxy = j
            if ( my_miny == -1 ) my_miny = j
         end if
      end do

      ! Create space to hold information about which other processors are 
      !   working on which parts of the domain
      allocate(processors(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_minx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_miny(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxy(0:nproc_x-1, 0:nproc_y-1))

      ! Exchange information with other processors
      if (my_proc_id == IO_NODE) then
         processors(my_x, my_y) = my_proc_id
         do i=1,nprocs-1
            call MPI_Recv(buffer, 2, MPI_INTEGER, i, MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            processors(buffer(1), buffer(2)) = mpi_stat(MPI_SOURCE)
         end do
      else
         buffer(1) = my_x
         buffer(2) = my_y
         call MPI_Send(buffer, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
      end if

      do ix=0,nproc_x-1
         do iy=0,nproc_y-1
            call parallel_bcast_int(processors(ix,iy), IO_NODE)
         end do
      end do
     
      proc_minx(my_x, my_y) = my_minx
      proc_maxx(my_x, my_y) = my_maxx
      proc_miny(my_x, my_y) = my_miny
      proc_maxy(my_x, my_y) = my_maxy

      do ix=0,nproc_x-1
         do iy=0,nproc_y-1
            call parallel_bcast_int(proc_minx(ix,iy), processors(ix,iy))
            call parallel_bcast_int(proc_maxx(ix,iy), processors(ix,iy))
            call parallel_bcast_int(proc_miny(ix,iy), processors(ix,iy))
            call parallel_bcast_int(proc_maxy(ix,iy), processors(ix,iy))
         end do
      end do
  
#else
      allocate(processors(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_minx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_miny(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxx(0:nproc_x-1, 0:nproc_y-1))
      allocate(proc_maxy(0:nproc_x-1, 0:nproc_y-1))
  
      processors(0,0) = IO_NODE
      proc_minx(0,0) = 1
      proc_miny(0,0) = 1
      proc_maxx(0,0) = idim
      proc_maxy(0,0) = jdim
      my_minx = 1
      my_maxx = idim
      my_miny = 1
      my_maxy = jdim
  
#endif
 
   end subroutine parallel_get_tile_dims


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Copied from RSL_LITE's task_for_point.c until a good way can be found to
   !    get the build mechanism to use the original code in RSL_LITE.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine task_for_point(i_p, j_p, ids_p, ide_p, jds_p, jde_p, npx, npy, px, py)

      implicit none

      ! Arguments
      integer, intent(in) :: i_p, j_p, ids_p, ide_p, jds_p, jde_p, npx, npy
      integer, intent(out) :: px, py

      ! Local variables
      integer :: a, b, rem, idim, jdim, i, j, ids, jds, ide, jde

      i = i_p - 1
      j = j_p - 1
      ids = ids_p - 1
      jds = jds_p - 1
      ide = ide_p - 1
      jde = jde_p - 1

      idim = ide-ids+1
      jdim = jde-jds+1

      i = max(i,ids)
      i = min(i,ide)
      rem = mod(idim, npx)
      a = ( rem / 2 ) * ( (idim / npx) + 1 )
      b = a + ( npx - rem ) * ( idim / npx )
      if ( i-ids < a ) then
         px = (i-ids) / ( (idim / npx) + 1 )
      else if ( i-ids < b ) then
         px = ( a / ( (idim / npx) + 1 ) ) + (i-a-ids) / ( ( b - a ) / ( npx - rem ) ) 
      else 
         px = ( a / ( (idim / npx) + 1 ) ) + (b-a-ids) / ( ( b - a ) / ( npx - rem ) ) + &
                                             (i-b-ids) / ( ( idim / npx ) + 1 ) 
      end if

      j = max(j,jds)
      j = min(j,jde)
      rem = mod(jdim, npy)
      a = ( rem / 2 ) * ( (jdim / npy) + 1 )
      b = a + ( npy - rem ) * ( jdim / npy )
      if ( j-jds < a ) then
         py = (j-jds) / ( (jdim / npy) + 1 )
      else if ( j-jds < b ) then
         py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-jds) / ( ( b - a ) / ( npy - rem ) )
      else
         py = ( a / ( (jdim / npy) + 1 ) ) + (b-a-jds) / ( ( b - a ) / ( npy - rem ) ) + &
                                             (j-b-jds) / ( ( jdim / npy ) + 1 )
      end if

   end subroutine task_for_point
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: gather_whole_field_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine gather_whole_field_i(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(in) :: patch_array
      integer, dimension(ds1:de1,ds2:de2,ds3:de3), intent(inout) :: domain_array
  
      ! Local variables
#ifdef _MPI
      integer :: i, ii, j, jj, kk
      integer, dimension(2) :: idims, jdims
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat

      if (my_proc_id == IO_NODE) then
  
         do j=0,nproc_y-1
            do i=0,nproc_x-1
               if (processors(i,j) /= IO_NODE) then
                  call MPI_Recv(jdims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  call MPI_Recv(idims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  do kk=ds3,de3
! BUG: Check on mpi_stat and mpi_ierr
                     call MPI_Recv(domain_array(idims(1):idims(2),jdims(1):jdims(2),kk), &
                                               (idims(2)-idims(1)+1)*(jdims(2)-jdims(1)+1), &
                                   MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  end do
               else
                  domain_array(ps1:pe1,ps2:pe2,ps3:pe3) = patch_array(ps1:pe1,ps2:pe2,ps3:pe3)
               end if
            end do
         end do
  
      else
  
         jdims(1) = ps2
         jdims(2) = pe2
         call MPI_Send(jdims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         idims(1) = ps1
         idims(2) = pe1
         call MPI_Send(idims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         do kk=ps3,pe3
            call MPI_Send(patch_array(ps1:pe1,ps2:pe2,kk), (pe1-ps1+1)*(pe2-ps2+1), MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
! BUG: Check on mpi_ierr
         end do
     end if
#endif
 
   end subroutine gather_whole_field_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: gather_whole_field_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine gather_whole_field_r(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(in) :: patch_array
      real, dimension(ds1:de1,ds2:de2,ds3:de3), intent(inout) :: domain_array
  
      ! Local variables
#ifdef _MPI
      integer :: i, ii, j, jj, kk
      integer, dimension(2) :: idims, jdims
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
  
      if (my_proc_id == IO_NODE) then
  
         do j=0,nproc_y-1
            do i=0,nproc_x-1
               if (processors(i,j) /= IO_NODE) then
                  call MPI_Recv(jdims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  call MPI_Recv(idims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  do kk=ds3,de3
! BUG: Check on mpi_stat and mpi_ierr
                     call MPI_Recv(domain_array(idims(1):idims(2),jdims(1):jdims(2),kk), &
                                   (idims(2)-idims(1)+1)*(jdims(2)-jdims(1)+1), &
                                   MPI_REAL, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  end do
               else
                  domain_array(ps1:pe1,ps2:pe2,ps3:pe3) = patch_array(ps1:pe1,ps2:pe2,ps3:pe3)
               end if
            end do
         end do
  
      else
  
         jdims(1) = ps2
         jdims(2) = pe2
         call MPI_Send(jdims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         idims(1) = ps1
         idims(2) = pe1
         call MPI_Send(idims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         do kk=ps3,pe3
            call MPI_Send(patch_array(ps1:pe1,ps2:pe2,kk), (pe1-ps1+1)*(pe2-ps2+1), MPI_REAL, 0, my_proc_id, comm, mpi_ierr)
! BUG: Check on mpi_ierr
         end do
      end if
#endif
 
   end subroutine gather_whole_field_r
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: scatter_whole_field_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine scatter_whole_field_i(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array
      integer, dimension(ds1:de1,ds2:de2,ds3:de3), intent(in) :: domain_array
  
      ! Local variables
#ifdef _MPI
      integer :: i, ii, j, jj, kk
      integer, dimension(2) :: idims, jdims
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
  
      if (my_proc_id == IO_NODE) then
  
         do j=0,nproc_y-1
            do i=0,nproc_x-1
               if (processors(i,j) /= IO_NODE) then
                  call MPI_Recv(jdims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  call MPI_Recv(idims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  do kk=ds3,de3
! BUG: Check on mpi_stat and mpi_ierr
                     call MPI_Send(domain_array(idims(1):idims(2),jdims(1):jdims(2),kk), &
                                               (idims(2)-idims(1)+1)*(jdims(2)-jdims(1)+1), &
                                   MPI_INTEGER, processors(i,j), my_proc_id, comm, mpi_ierr)
                  end do
               else
                  patch_array(ps1:pe1,ps2:pe2,ps3:pe3) = domain_array(ps1:pe1,ps2:pe2,ps3:pe3)
               end if
            end do
         end do
  
      else
  
         jdims(1) = ps2
         jdims(2) = pe2
         call MPI_Send(jdims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         idims(1) = ps1
         idims(2) = pe1
         call MPI_Send(idims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         do kk=ps3,pe3
            call MPI_Recv(patch_array(ps1:pe1,ps2:pe2,kk), (pe1-ps1+1)*(pe2-ps2+1), &
                          MPI_INTEGER, 0, MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
! BUG: Check on mpi_ierr
         end do
     end if
#endif
 
   end subroutine scatter_whole_field_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: scatter_whole_field_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine scatter_whole_field_r(patch_array, ms1, me1, ms2, me2, ms3, me3, &
                                 ps1, pe1, ps2, pe2, ps3, pe3, &
                                 domain_array, ds1, de1, ds2, de2, ds3, de3)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3, &
                             ds1, de1, ds2, de2, ds3, de3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array
      real, dimension(ds1:de1,ds2:de2,ds3:de3), intent(in) :: domain_array
  
      ! Local variables
#ifdef _MPI
      integer :: i, ii, j, jj, kk
      integer, dimension(2) :: idims, jdims
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat
  
      if (my_proc_id == IO_NODE) then
  
         do j=0,nproc_y-1
            do i=0,nproc_x-1
               if (processors(i,j) /= IO_NODE) then
                  call MPI_Recv(jdims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  call MPI_Recv(idims, 2, MPI_INTEGER, processors(i,j), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
                  do kk=ds3,de3
! BUG: Check on mpi_stat and mpi_ierr
                     call MPI_Send(domain_array(idims(1):idims(2),jdims(1):jdims(2),kk), &
                                               (idims(2)-idims(1)+1)*(jdims(2)-jdims(1)+1), &
                                   MPI_REAL, processors(i,j), my_proc_id, comm, mpi_ierr)
                  end do
               else
                  patch_array(ps1:pe1,ps2:pe2,ps3:pe3) = domain_array(ps1:pe1,ps2:pe2,ps3:pe3)
               end if
            end do
         end do
  
      else
  
         jdims(1) = ps2
         jdims(2) = pe2
         call MPI_Send(jdims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         idims(1) = ps1
         idims(2) = pe1
         call MPI_Send(idims, 2, MPI_INTEGER, 0, my_proc_id, comm, mpi_ierr)
         do kk=ps3,pe3
            call MPI_Recv(patch_array(ps1:pe1,ps2:pe2,kk), (pe1-ps1+1)*(pe2-ps2+1), &
                          MPI_REAL, 0, MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
! BUG: Check on mpi_ierr
         end do
     end if
#endif
 
   end subroutine scatter_whole_field_r


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: exchange_halo_r
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine exchange_halo_r(patch_array, &
                              ms1, me1, ms2, me2, ms3, me3, &
                              ps1, pe1, ps2, pe2, ps3, pe3)
 
      implicit none

      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3
      real, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array

      ! Local variables
#ifdef _MPI
      integer :: jj, kk
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat

      !
      ! Get left edge of halo
      !
      if (my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get right edge of halo
      !
      if (my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get bottom edge of halo
      !
      if (my_y /= (nproc_y - 1)) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_REAL, &
                             processors(my_x,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_REAL, &
                             processors(my_x,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get top edge of halo
      !
      if (my_y /= 0) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_REAL, &
                             processors(my_x,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1)) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_REAL, &
                             processors(my_x,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get lower-right corner of halo
      !
      if (my_y /= (nproc_y - 1) .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0 .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get upper-left corner of halo
      !
      if (my_y /= 0 .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1) .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get upper-right corner of halo
      !
      if (my_y /= 0 .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1) .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get lower-left corner of halo
      !
      if (my_y /= (nproc_y - 1) .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x+1,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0 .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_REAL, &
                             processors(my_x-1,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if
#endif
  
   end subroutine exchange_halo_r


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: exchange_halo_i
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine exchange_halo_i(patch_array, &
                              ms1, me1, ms2, me2, ms3, me3, &
                              ps1, pe1, ps2, pe2, ps3, pe3)
 
      implicit none

      ! Arguments
      integer, intent(in) :: ps1, pe1, ps2, pe2, ps3, pe3, &
                             ms1, me1, ms2, me2, ms3, me3
      integer, dimension(ms1:me1,ms2:me2,ms3:me3), intent(inout) :: patch_array

      ! Local variables
#ifdef _MPI
      integer :: jj, kk
      integer :: mpi_ierr
      integer, dimension(MPI_STATUS_SIZE) :: mpi_stat

      !
      ! Get left edge of halo
      !
      if (my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get right edge of halo
      !
      if (my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,me2
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get bottom edge of halo
      !
      if (my_y /= (nproc_y - 1)) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_INTEGER, &
                             processors(my_x,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_INTEGER, &
                             processors(my_x,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get top edge of halo
      !
      if (my_y /= 0) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_INTEGER, &
                             processors(my_x,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1)) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(ms1:me1,jj,kk), (me1-ms1+1), MPI_INTEGER, &
                             processors(my_x,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get lower-right corner of halo
      !
      if (my_y /= (nproc_y - 1) .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0 .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get upper-left corner of halo
      !
      if (my_y /= 0 .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1) .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get upper-right corner of halo
      !
      if (my_y /= 0 .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=ps2,ps2+HALO_WIDTH-1
               call MPI_Send(patch_array(ps1:ps1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y-1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= (nproc_y - 1) .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=me2-HALO_WIDTH+1,me2
               call MPI_Recv(patch_array(me1-HALO_WIDTH+1:me1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y+1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if

      !
      ! Get lower-left corner of halo
      !
      if (my_y /= (nproc_y - 1) .and. my_x /= (nproc_x - 1)) then
         do kk=ps3,pe3
            do jj=pe2-HALO_WIDTH+1,pe2
               call MPI_Send(patch_array(pe1-HALO_WIDTH+1:pe1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x+1,my_y+1), my_proc_id, comm, mpi_ierr)
            end do
         end do
      end if
      if (my_y /= 0 .and. my_x /= 0) then
         do kk=ps3,pe3
            do jj=ms2,ms2+HALO_WIDTH-1
               call MPI_Recv(patch_array(ms1:ms1+HALO_WIDTH-1,jj,kk), HALO_WIDTH, MPI_INTEGER, &
                             processors(my_x-1,my_y-1), MPI_ANY_TAG, comm, mpi_stat, mpi_ierr)
            end do
         end do
      end if
#endif
  
   end subroutine exchange_halo_i
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_logical
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_logical(lval)
 
      implicit none
  
      ! Argument
      logical, intent(inout) :: lval
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr
  
      call MPI_Bcast(lval, 1, MPI_LOGICAL, IO_NODE, comm, mpi_ierr)
#endif
 
   end subroutine parallel_bcast_logical
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_int
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_int(ival, from_whom)
 
      implicit none
  
      ! Argument
      integer, intent(inout) :: ival
      integer, intent(in), optional :: from_whom
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr
  
      if (present(from_whom)) then
         call MPI_Bcast(ival, 1, MPI_INTEGER, from_whom, comm, mpi_ierr)
      else
         call MPI_Bcast(ival, 1, MPI_INTEGER, IO_NODE, comm, mpi_ierr)
      end if
#endif
 
   end subroutine parallel_bcast_int
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_real
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_real(rval, from_whom)
 
      implicit none
  
      ! Argument
      real, intent(inout) :: rval
      integer, intent(in), optional :: from_whom
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr
  
      if (present(from_whom)) then
         call MPI_Bcast(rval, 1, MPI_REAL, from_whom, comm, mpi_ierr)
      else
         call MPI_Bcast(rval, 1, MPI_REAL, IO_NODE, comm, mpi_ierr)
      end if
#endif
 
   end subroutine parallel_bcast_real
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_bcast_char
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_bcast_char(cval, n, from_whom)
 
      implicit none
  
      ! Argument
      integer, intent(in) :: n
      character (len=n), intent(inout) :: cval
      integer, intent(in), optional :: from_whom
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr
  
      if (present(from_whom)) then
         call MPI_Bcast(cval, n, MPI_CHARACTER, from_whom, comm, mpi_ierr)
      else
         call MPI_Bcast(cval, n, MPI_CHARACTER, IO_NODE, comm, mpi_ierr)
      end if
#endif
 
   end subroutine parallel_bcast_char
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_finish
   !
   ! Purpose: Free up, deallocate, and for MPI, finalize.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_finish()
 
      implicit none
  
      ! Arguments
  
      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr
  
      call MPI_Finalize(mpi_ierr)
#endif
 
      if (associated(processors)) deallocate(processors)
      if (associated(proc_minx)) deallocate(proc_minx)
      if (associated(proc_maxx)) deallocate(proc_maxx)
      if (associated(proc_miny)) deallocate(proc_miny)
      if (associated(proc_maxy)) deallocate(proc_maxy)
 
   end subroutine parallel_finish


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: parallel_abort
   !
   ! Purpose: Terminate everything
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine parallel_abort()

      implicit none

      ! Arguments

      ! Local variables
#ifdef _MPI
      integer :: mpi_ierr, mpi_errcode

      call MPI_Abort(MPI_COMM_WORLD, mpi_errcode, mpi_ierr)
#endif

      stop

   end subroutine parallel_abort
 
end module parallel_module
