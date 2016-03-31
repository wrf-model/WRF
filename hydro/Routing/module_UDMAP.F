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

! This subrouting includs the data structure and tools used for NHDPlus network mapping.
module module_UDMAP

use module_namelist, only: nlst_rt
#ifdef MPP_LAND
use module_mpp_land, only: my_id, local_startx_rt, local_starty_rt,  &
         local_endx_rt,local_endy_rt, left_id, right_id, down_id, up_id, mpp_collect_1d_int_mem, &
         IO_id , numprocs
use module_mpp_land, only: mpp_land_bcast_int, mpp_land_bcast_real8_1d, mpp_land_bcast_int1

use module_mpp_land, only: sum_int1d, global_rt_nx, global_rt_ny, write_IO_rt_int, MPP_LAND_COM_INTEGER

use MODULE_mpp_ReachLS, only : updatelinkv, ReachLS_write_io, com_write1dInt, &
               com_decomp1dInt, pack_decomp_int, pack_decomp_real8

#endif

implicit none

#ifndef MPP_LAND
    integer, parameter :: numprocs=1
#endif

#include <netcdf.inc>

type userDefineMapping
    integer, allocatable, dimension(:) :: grid_i, grid_j
    real, allocatable, dimension(:) :: weight, nodeArea, cellArea
    integer :: ngrids
    integer :: myid
!   for bucket model definition
    real, allocatable, dimension(:) :: cellWeight
    integer, allocatable, dimension(:) :: cell_i, cell_j
    integer :: ncell
end type userDefineMapping

TYPE ( userDefineMapping ), allocatable, DIMENSION (:) :: LUDRSL

integer, allocatable, dimension(:) :: bufid
real*8 , allocatable, dimension(:) :: bufw
integer :: LNUMRSL  ! number of local links
integer :: ter_rt_flag
real*8, allocatable, dimension(:) :: basns_area
integer :: gnpid, lnsize
integer, allocatable, dimension(:) :: bufi,bufj

contains
    subroutine UDMP_ini(nlinksl,ixrt,jxrt,rtmask, OVRTSWCRT, SUBRTSWCRT,cell_area)
!This is the driver for user defined mapping file funciton application.
        integer :: ixrt, jxrt, OVRTSWCRT, SUBRTSWCRT, nlinksl
        integer, intent(in), dimension(ixrt,jxrt):: rtmask
        integer :: npid    !local variable.
        real,dimension(:,:) :: cell_area
        ter_rt_flag = 0
        if(OVRTSWCRT .eq. 1 .or. SUBRTSWCRT .eq. 1) then
            ter_rt_flag = 1
        endif
        call readUDMP(ixrt,jxrt,npid,nlinksl)
        call UDMP2LOCAL(npid,ixrt,jxrt,rtmask, ter_rt_flag)
        call getUDMP_area(cell_area)
    end subroutine UDMP_ini

    subroutine readUDMP(ixrt,jxrt,npid, nlinksl)
        implicit none
        integer :: i,j,Ndata, did, Npid, nlinksl, k, m, kk
        integer,allocatable,dimension(:) :: g1bufid, gbufid, linkid ,bufidflag, &
               bufid_tmp, nprocs_map, lnsizes, istart
        integer :: ix_bufid, ii, ixrt,jxrt
        integer, allocatable, dimension(:) :: gbufi,gbufj,bufsize
        real*8 , allocatable, dimension(:) :: gbufw
        
        did = 1
        call get_dimension(trim(nlst_rt(did)%UDMAP_FILE), ndata, npid) 

#ifdef MPP_LAND
        gnpid = npid
        allocate (lnsizes(numprocs))
        if(my_id .eq. io_id) then
           allocate (istart(numprocs))
           allocate (nprocs_map(ndata))
           allocate(gbufi(ndata))
           allocate(gbufj(ndata))
           call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"i_index",gbufi)
           call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"j_index",gbufj)
        endif
           call get_nprocs_map(ixrt,jxrt,gbufi,gbufj,nprocs_map,ndata)

        if(my_id .eq. io_id) then
           lnsizes = 0 
           do i =1 , ndata
               if(nprocs_map(i) .gt. 0) then
                  lnsizes(nprocs_map(i)) = lnsizes(nprocs_map(i)) + 1 
               endif
           enddo
        endif
        call mpp_land_bcast_int(numprocs,lnsizes)

     if(my_id .eq. io_id ) then
        kk = 0
        do i = 1, numprocs 
           kk = kk + lnsizes(i) 
        end do
     end if

      if(my_id .eq. IO_id) then
          ii = 1
          do i = 1, numprocs
             istart(i) = ii
             if(lnsizes(i) .gt. 0) then
                ii = lnsizes(i) + ii
             else
                istart(i) = -999
             endif
          end do
      endif

      if(lnsizes(my_id+1) .gt. 0)  allocate(bufi(lnsizes(my_id+1) ))
      call pack_decomp_int(gbufi, ndata, nprocs_map, lnsizes, istart,bufi)
      if(my_id .eq. io_id) then 
           if(allocated(gbufi))  deallocate(gbufi)
      endif

      
      if(lnsizes(my_id+1) .gt. 0) allocate(bufj(lnsizes(my_id+1) ))
      call pack_decomp_int(gbufj, ndata, nprocs_map, lnsizes, istart,bufj)
      if(my_id .eq. io_id)  then 
         if(allocated(gbufj)) deallocate(gbufj)
      endif


! check bufid
!      check  polyid and linkid
        allocate(linkid(nlinksl))
        if(my_id .eq. io_id) then
            call get1d_int(trim(nlst_rt(did)%route_link_f),"link",linkid)
            allocate(gbufid(npid))
            call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"polyid",gbufid)
        endif
#ifdef MPP_LAND
       if(nlinksl .gt. 0) then
          call mpp_land_bcast_int(nlinksl,linkid)
       endif
       call com_decomp1dInt(gbufid,npid,bufid_tmp,ix_bufid)
#endif
       if(ix_bufid .gt. 0) then
          allocate(bufidflag(ix_bufid))
          bufidflag = -999
       endif

       do i = 1, ix_bufid
          do j = 1, nlinksl
               if(bufid_tmp(i) .eq. linkid(j)) then
                  bufidflag(i) = bufid_tmp(i)
                  goto 102
               endif
          end do
102       continue
       end do

#ifdef MPP_LAND
      call com_write1dInt(bufidflag,ix_bufid,gbufid,npid)
#endif
      if(ix_bufid .gt. 0) then
          if(allocated(bufidflag)) deallocate(bufidflag)
          if(allocated(bufid_tmp)) deallocate(bufid_tmp)
      endif
      if(allocated(linkid)) deallocate(linkid)
      if(my_id .eq. io_id) then
          allocate(bufsize(npid))
          allocate(g1bufid(ndata))
          call get1d_int(trim(nlst_rt(did)%UDMAP_FILE),"overlaps",bufsize)
          g1bufid = -999
          i = 1
          do k = 1, npid
               do j = 1, bufsize(k)
                 g1bufid(i) = gbufid(k)
                 i = i + 1 
               end do
          enddo
          if(allocated(bufsize))  deallocate(bufsize)
      endif


      if(my_id .eq. io_id) then 
           if(allocated(gbufid)) deallocate(gbufid)
      endif


      if(lnsizes(my_id+1) .gt. 0) allocate(bufid(lnsizes(my_id+1) ))
      call pack_decomp_int(g1bufid, ndata, nprocs_map, lnsizes, istart,bufid)
      if(my_id .eq. io_id) then 
            if(allocated(g1bufid)) deallocate(g1bufid)
      endif


      if(my_id .eq. io_id) then
          allocate(gbufw(ndata))
          call get1d_real8(trim(nlst_rt(did)%UDMAP_FILE),"regridweight",gbufw)
      endif
      if(lnsizes(my_id+1) .gt. 0) allocate(bufw(lnsizes(my_id+1) ))
      call pack_decomp_real8(gbufw, ndata, nprocs_map, lnsizes, istart,bufw)
      if(my_id .eq. io_id) then 
          if(allocated(gbufw))     deallocate(gbufw)
      endif


        if(my_id .eq. io_id) then
           if(allocated(nprocs_map)) deallocate (nprocs_map)
           if(allocated(istart)) deallocate (istart)
        endif
        lnsize = lnsizes(my_id + 1)
        if(allocated(lnsizes)) deallocate(lnsizes)
#else
       call hydro_stop("FATAL ERROR in UDMP : sequential not defined.")
#endif

    end subroutine readUDMP

    subroutine UDMP2LOCAL(npid,ix,jx,rtmask, ter_rt_flag)
        implicit none
        integer :: i,j,k, ngrids, ix,jx, starti,startj, endi,endj, ii,jj, npid, kk
        integer, intent(in), dimension(ix,jx) :: rtmask
        integer, dimension(lnsize) :: lndflag,gridflag , tmpgridflag
        integer :: ter_rt_flag, m, c


!   find ngrids is 0 so that we need to mapping from subsurface runoff.
#ifdef MPP_LAND
        if(left_id .ge. 0) then
           starti = local_startx_rt  + 1
        else
           starti = local_startx_rt 
        endif
        if(down_id .ge. 0) then
           startj = local_starty_rt  + 1
        else
           startj = local_starty_rt 
        endif
        if(right_id .ge. 0) then
           endi = local_startx_rt + ix -2
        else
           endi = local_startx_rt + ix -1
        endif
        if(up_id .ge. 0) then
           endj = local_starty_rt + jx -2
        else
           endj = local_starty_rt + jx -1
        endif
#else
        starti = 1
        startj = 1
        endi = ix
        endj = jx
#endif
        gridflag = 0
        lndflag = 0
      
#ifdef MPP_LAND
        k = 0
        do i = 1, lnsize
           if(bufid(i) .gt. 0) then
                if(bufi(i) .ge. starti .and. bufj(i) .ge. startj .and. &
                    bufi(i) .le. endi   .and. bufj(i) .le. endj) then
                    if(k .eq. 0) then
                       k = 1
                    else
                       if(bufid(i) .ne. bufid(i-1)) k = k + 1
                    endif
                    lndflag(k) = lndflag(k) + 1
                    if(ter_rt_flag .eq. 1) then
                        if(rtmask(bufi(i)-local_startx_rt+1,bufj(i)-local_starty_rt+1) .ge. 0) then
                             gridflag(k) = gridflag(k) + 1 
                        endif
                    endif
                 endif
           endif
        end do

! decide how many mapping land grids on current domain
!       tmpgridflag = gridflag
#ifdef MPP_LAND
!       call mpp_collect_1d_int_mem(npid,tmpgridflag)
#endif

! decide how many user defined links on current domain
        kk = k
        LNUMRSL = 0 
        do k = 1, lnsize
           if(lndflag(k) .gt. 0) LNUMRSL = LNUMRSL + 1
        enddo


        if(LNUMRSL .gt. 0) then 
               allocate(LUDRSL(LNUMRSL))
               allocate( basns_area(LNUMRSL) )
        else
               write(6,*) "Warning: no routing links found."
               call cleanBuf()
               return
        endif

        kk = 0
        do k = 1, lnsize 
           if( bufid(k) .ge. 0 ) then
             if (bufi(k) .ge. starti .and. bufj(k) .ge. startj .and. &
                    bufi(k) .le. endi   .and. bufj(k) .le. endj ) then
                 if(kk .eq. 0) then
                       kk = 1
                 else
                       if(bufid(k) .ne. bufid(k-1)) kk = kk + 1
                 endif
                 LUDRSL(kk)%myid = bufid(k) 
                 LUDRSL(kk)%ngrids = -999
                 if(gridflag(kk) .gt. 0) then
                   LUDRSL(kk)%ngrids = gridflag(kk)
                   if(.not. allocated(LUDRSL(kk)%weight) ) then
                         allocate( LUDRSL(kk)%weight(LUDRSL(kk)%ngrids ))
                         allocate( LUDRSL(kk)%grid_i(LUDRSL(kk)%ngrids ))
                         allocate( LUDRSL(kk)%grid_j(LUDRSL(kk)%ngrids ))
                         allocate( LUDRSL(kk)%nodeArea(LUDRSL(kk)%ngrids ))
                   endif
                 endif
!  define bucket variables
                 LUDRSL(kk)%ncell = lndflag(kk) 
                 if(.not. allocated(LUDRSL(kk)%cellweight) ) then
                     allocate( LUDRSL(kk)%cellweight(LUDRSL(kk)%ncell))
                     allocate( LUDRSL(kk)%cell_i(LUDRSL(kk)%ncell))
                     allocate( LUDRSL(kk)%cell_j(LUDRSL(kk)%ncell))
                     allocate( LUDRSL(kk)%cellArea(LUDRSL(kk)%ncell))
                 endif
             endif
           endif
        enddo


! maping grid_i, grid_j and weight
        kk = 0
        m  = 1
        c  = 1
        do i = 1, lnsize 
               if( (bufid(i) .ge. 0)  ) then 
                   if(bufi(i) .ge. starti .and. bufj(i) .ge. startj .and. &
                      bufi(i) .le. endi   .and. bufj(i) .le. endj) then
                      if(kk .eq. 0) then
                         kk = 1
                      else
                         if(bufid(i) .ne. bufid(i-1)) then 
                             kk = kk + 1
                             m  = 1
                             c  = 1
                         endif
                      endif

                      if(LUDRSL(kk)%ngrids .gt. 0) then 
                          if(rtmask(bufi(i)-local_startx_rt+1,bufj(i)-local_starty_rt+1) .ge. 0) then
                             LUDRSL(kk)%grid_i(m) = bufi(i) - local_startx_rt+1
                             LUDRSL(kk)%grid_j(m) = bufj(i) - local_starty_rt+1
                             LUDRSL(kk)%weight(m) = bufw(i) 
                             m  = m  + 1
                          endif
                      endif
!! begin define bucket variables
                          LUDRSL(kk)%cell_i(c) = bufi(i) - local_startx_rt+1
                          LUDRSL(kk)%cell_j(c) = bufj(i) - local_starty_rt+1
                          LUDRSL(kk)%cellWeight(c) = bufw(i)
                          c  = c  + 1
!! end define bucket variables 
                   endif
                endif
        end do

        call cleanBuf()

#else
        call hydro_stop("FATAL ERROR in UDMP: Sequential not work.")
#endif
   
    end subroutine UDMP2LOCAL

    subroutine cleanBuf()
        if(allocated(bufi))  deallocate(bufi)
        if(allocated(bufj))  deallocate(bufj)
        if(allocated(bufw))  deallocate(bufw)
        if(allocated(bufid))  deallocate(bufid)
    end subroutine cleanBuf

     subroutine get_dimension(fileName, ndata,npid)
            implicit none
            character(len=*) fileName
            integer ncid , iret, ndata,npid, dimid
#ifdef MPP_LAND
            if(my_id .eq. IO_id) then
#endif
               iret = nf_open(fileName, NF_NOWRITE, ncid)
               if (iret /= 0) then
                  write(*,'("FATAL ERROR: Problem opening mapping file: ''", A, "''")') &
                       trim(fileName)
                  call hydro_stop("In get_dimension() - Problem opening mapping file.")
               endif
        
               iret = nf_inq_dimid(ncid, "polyid", dimid)
        
               if (iret /= 0) then
                  print*, "nf_inq_dimid:  polyid"
                  call hydro_stop("In get_dimension() - nf_inq_dimid:  polyid")
               endif
           
               iret = nf_inq_dimlen(ncid, dimid, npid)
           
               iret = nf_inq_dimid(ncid, "data", dimid)
               if (iret /= 0) then
                          print*, "nf_inq_dimid:  data"
                          call hydro_stop("In get_file_dimension() - nf_inq_dimid:  data")
               endif
        
               iret = nf_inq_dimlen(ncid, dimid, ndata)
               iret = nf_close(ncid)
#ifdef MPP_LAND
            endif
            call mpp_land_bcast_int1(ndata)
            call mpp_land_bcast_int1(npid)
#endif
            return
     end subroutine get_dimension

       subroutine get1d_real8(fileName,var_name,out_buff)
          implicit none
          integer :: ivar, iret,varid,ncid
          real*8 out_buff(:)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"failed to open the netcdf file: ",trim(fileName)
            call hydro_stop("In get1d_real8() - failed to open the netcdf file.")
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
               write(6,*) "Read Variable Error file: ",trim(fileName)
               write(6,*) "Read Error: could not find ",trim(var_name)
               call hydro_stop("In get1d_real8() - failed to read netcdf varialbe name. ")
          end if
          iret = nf_get_var_double(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_real8

       subroutine get1d_int(fileName,var_name,out_buff)
          implicit none
          integer :: ivar, iret,varid,ncid
          integer out_buff(:)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
            print*,"FATAL ERROR: Failed to open the netcdf file: ",trim(fileName)
            call hydro_stop("In get1d_int() -  Failed to open the netcdf file")
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
               write(6,*) "Read Variable Error file: ",trim(fileName)
               write(6,*) "Read Error: could not find ",trim(var_name)
               call hydro_stop("In get1d_int() - failed to read netcdf variable name.")
          end if
          iret = nf_get_var_int(ncid, varid, out_buff)
          iret = nf_close(ncid)
      end subroutine get1d_int

      subroutine getUDMP_area(cell_area)
         implicit none
         integer i,j,k, m
         real, dimension(:,:) :: cell_area
         do k  = 1, LNUMRSL
            if(LUDRSL(k)%ngrids .gt. 0) then
                do m = 1, LUDRSL(k)%ngrids
                    LUDRSL(k)%nodeArea(m) = cell_area(LUDRSL(k)%grid_i(m),LUDRSL(k)%grid_j(m)) 
                enddo
            endif
                do m = 1, LUDRSL(k)%ncell
                    LUDRSL(k)%cellArea(m) = cell_area(LUDRSL(k)%cell_i(m),LUDRSL(k)%cell_j(m)) 
                enddo
           
            basns_area(k) = 0 
            do m = 1, LUDRSL(k)%ncell
                    basns_area(k) = basns_area(k) + &
                          cell_area(LUDRSL(k)%cell_i(m),LUDRSL(k)%cell_j(m)) * LUDRSL(k)%cellWeight(m) 
            enddo
            
         end do
      end subroutine getUDMP_area
    
      subroutine get_basn_area_nhd(inOut)
         implicit none
         real, dimension(:) :: inOut
         real, dimension(gnpid) :: buf
#ifdef MPP_LAND
         call updateLinkV(basns_area, inOut)
#else
         inOut = basns_area
#endif

      
      end subroutine get_basn_area_nhd

      subroutine get_nprocs_map(ix,jx,bufi,bufj,nprocs_map,ndata)
          implicit none
          integer,dimension(:)  :: bufi, bufj,nprocs_map 
!          integer, allocatable, dimension(:) ::  lbufi,lbufj, lmap
          integer  :: ndata, lsize, ix,jx
          integer, dimension(ix,jx) :: mask
          integer, allocatable,dimension(:,:) :: gmask

        integer :: i,j,k, starti,startj, endi,endj, ii,jj, npid, kk
#ifdef MPP_LAND
           
          mask = my_id + 1
          if(my_id .eq. IO_id) allocate(gmask(global_rt_nx, global_rt_ny))

          call MPP_LAND_COM_INTEGER(mask,IX,JX,99)
          call write_IO_rt_int(mask, gmask) 

          if(my_id .eq. io_id ) then
             nprocs_map = -999
             do i = 1, ndata
                  if( (bufi(i) .gt. 0 .and. bufi(i) .le. global_rt_nx) .and.  &
                     (bufj(i) .gt. 0 .and. bufj(i) .le. global_rt_ny) ) then
                     nprocs_map(i) = gmask(bufi(i), bufj(i))
                     if( gmask(bufi(i), bufj(i)) .lt. 0) then
                         write(6,*) "mapping error in gmask : ", bufi(i) ,bufj(i)
                     endif
                  else
                      write(6,*) "no mapping for i,j : ", bufi(i) ,bufj(i)
                  endif
             end do

             if(allocated(gmask)) deallocate(gmask)
          endif
#else
        call hydro_stop("FATAL ERROR in UDMP: Sequential not work.")
#endif


      end subroutine get_nprocs_map


end module module_UDMAP
