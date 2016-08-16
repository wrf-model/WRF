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

module module_HYDRO_utils
  use module_RT_data, only: rt_domain
  use module_namelist, only: nlst_rt
#ifdef MPP_LAND
     use module_mpp_land, only: global_nx, global_ny, my_id, IO_id, &
           decompose_data_real, write_io_real, MPP_LAND_COM_REAL, &
           write_io_int, mpp_land_bcast_real, global_rt_nx, global_rt_ny, &
           decompose_rt_real, write_io_rt_real
     use MODULE_mpp_GWBUCKET, only: gw_decompose_real
#endif


  implicit none
  logical lr_dist_flag    !land routing distance calculated or not. 
  
contains

        integer function get2d_real(var_name,out_buff,ix,jx,fileName)
          implicit none
#         include "netcdf.inc"
          integer :: ivar, iret,varid,ncid,ix,jx
          real out_buff(ix,jx)
          character(len=*), intent(in) :: var_name
          character(len=*), intent(in) :: fileName
          get2d_real = -1

          iret = nf_open(trim(fileName), NF_NOWRITE, ncid)
          if (iret .ne. 0) then
#ifdef HYDRO_D
            print*,"Failed to open the netcdf file: ",trim(fileName)
#endif
            out_buff = -9999.
            return
          endif
          ivar = nf_inq_varid(ncid,trim(var_name),  varid)
          if(ivar .ne. 0) then
            ivar = nf_inq_varid(ncid,trim(var_name//"_M"),  varid)
            if(ivar .ne. 0) then
#ifdef HYDRO_D
               write(6,*) "Read Error: could not find ",var_name
#endif
                 return
            endif
          end if
          iret = nf_get_var_real(ncid, varid, out_buff)
          iret = nf_close(ncid)
          get2d_real =  ivar
      end function get2d_real

 
! this module create the distance dx, dy and diagnoal for routing
! 8 direction as the slop:
! 1: i,j+1  
! 2: i+1, j+1
! 3: i+1, j
! 4: i+1, j-1
! 5: i, j-1
! 6: i-1, j-1
! 7: i-1, j
! 8: i-1, j+1
   real function get_dy(i,j,v,ix,jx)  
      ! south north
       integer :: i,j,ix,jx
       real,dimension(ix,jx,9) :: v 
       if( v(i,j,1) .le. 0) then
          get_dy = v(i,j,5)
       else if( v(i,j,5) .le. 0) then
          get_dy = v(i,j,1)
       else
          get_dy = (v(i,j,1) + v(i,j,5) ) / 2
       endif
       return
   end function get_dy

   real function get_dx(i,j,v,ix,jx)   
      ! east-west
       integer :: i,j, ix,jx
       real,dimension(ix,jx,9) :: v 
       if( v(i,j,3) .le. 0) then
          get_dx = v(i,j,7)
       else if( v(i,j,7) .le. 0) then
          get_dx = v(i,j,3)
       else
          get_dx = (v(i,j,3) + v(i,j,7) ) / 2
       endif
       return
   end function get_dx

   real function get_ll_d(lat1_in, lat2_in, lon1_in, lon2_in)
     implicit none
     real:: lat1, lat2, lon1, lon2
     real:: lat1_in, lat2_in, lon1_in, lon2_in
     real::  r, pai, a,c, dlat, dlon, b1,b2
     pai = 3.14159
     lat1 = lat1_in * pai/180
     lat2 = lat2_in * pai/180
     lon1 = lon1_in * pai/180
     lon2 = lon2_in * pai/180
     r = 6378.1*1000
     dlat = lat2 -lat1
     dlon = lon2 -lon1
     a = sin(dlat/2)*sin(dlat/2) + cos(lat1)*cos(lat2)*sin(dlon/2)*sin(dlon/2)
     b1 = sqrt(a) 
     b2 = sqrt(1-a)  
     c = 2.0*atan2(b1,b2)
     get_ll_d = R*c
     return 

   end function get_ll_d

   real function get_ll_d_tmp(lat1_in, lat2_in, lon1_in, lon2_in)
     implicit none
     real:: lat1, lat2, lon1, lon2
     real:: lat1_in, lat2_in, lon1_in, lon2_in
     real::  r, pai
     pai = 3.14159
     lat1 = lat1_in * pai/180
     lat2 = lat2_in * pai/180
     lon1 = lon1_in * pai/180
     lon2 = lon2_in * pai/180
     r = 6371*1000
     get_ll_d_tmp = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*r
     return 

   end function get_ll_d_tmp

   subroutine get_rt_dxdy_ll(did)
!   use the land lat and lon to derive the routing distrt
      implicit none
      integer:: did, k
      integer iret
!     external get2d_real
!     real get2d_real
#ifdef MPP_LAND
      real, dimension(global_rt_nx,global_rt_ny):: latrt, lonrt
      real, dimension(global_rt_nx,global_rt_ny,9):: dist
      if(my_id .eq. IO_id) then
 ! read the lat and lon. 
         iret =  get2d_real("LONGITUDE",lonrt,global_rt_nx,global_rt_ny,&
                     trim(nlst_rt(did)%GEO_FINEGRID_FLNM ))
         iret =  get2d_real("LATITUDE",latrt,global_rt_nx,global_rt_ny,&
                     trim(nlst_rt(did)%GEO_FINEGRID_FLNM ))
         call get_dist_ll(dist,latrt,lonrt,global_rt_nx,global_rt_ny)
      end if
     do k = 1 , 9
        call decompose_RT_real(dist(:,:,k),rt_domain(did)%dist(:,:,k), &
                global_rt_nx,global_rt_ny,rt_domain(did)%ixrt,rt_domain(did)%jxrt)
     end do
#else
      real, dimension(rt_domain(did)%ixrt,rt_domain(did)%jxrt):: latrt, lonrt
 ! read the lat and lon. 
         iret =  get2d_real("LONGITUDE",lonrt,rt_domain(did)%ixrt,rt_domain(did)%jxrt,&
                     trim(nlst_rt(did)%GEO_FINEGRID_FLNM ))
         iret =  get2d_real("LATITUDE",latrt,rt_domain(did)%ixrt,rt_domain(did)%jxrt,&
                     trim(nlst_rt(did)%GEO_FINEGRID_FLNM ))
         call get_dist_ll(rt_domain(did)%dist,latrt,lonrt,rt_domain(did)%ixrt,rt_domain(did)%jxrt)
#endif

   end subroutine get_rt_dxdy_ll

!  get dx and dy of lat and lon   
   subroutine get_dist_ll(dist,lat,lon,ix,jx)
      implicit none
      integer:: ix,jx 
      real, dimension(ix,jx,9):: dist
      real, dimension(ix,jx):: lat, lon
      integer:: i,j 
      real x,y 
      dist = -1
      do j = 1, jx
        do i = 1, ix
          if(j .lt. jx) dist(i,j,1) = &
             get_ll_d(lat(i,j), lat(i,j+1), lon(i,j), lon(i,j+1))
          if(j .lt. jx .and. i .lt. ix) dist(i,j,2) =  &
             get_ll_d(lat(i,j), lat(i+1,j+1), lon(i,j), lon(i+1,j+1))
          if(i .lt. ix) dist(i,j,3) = &    
             get_ll_d(lat(i,j), lat(i+1,j), lon(i,j), lon(i+1,j))
          if(j .gt. 1 .and. i .lt. ix) dist(i,j,4) = &    
             get_ll_d(lat(i,j), lat(i+1,j-1), lon(i,j), lon(i+1,j-1))
          if(j .gt. 1 ) dist(i,j,5) = &   
             get_ll_d(lat(i,j), lat(i,j-1), lon(i,j), lon(i,j-1))
          if(j .gt. 1 .and. i .gt. 1) dist(i,j,6) = &   
             get_ll_d(lat(i,j), lat(i-1,j-1), lon(i,j), lon(i-1,j-1))
          if(i .gt. 1) dist(i,j,7) = &   
             get_ll_d(lat(i,j), lat(i-1,j), lon(i,j), lon(i-1,j))
          if(j .lt. jx .and. i .gt. 1) dist(i,j,8) = &   
             get_ll_d(lat(i,j), lat(i-1,j+1), lon(i,j), lon(i-1,j+1))
        end do
      end do
      do j = 1, jx 
        do i = 1, ix
            if(j.eq.1) then
               y =  get_ll_d(lat(i,j), lat(i,j+1), lon(i,j), lon(i,j+1))
            else if(j.eq.jx) then 
               y =  get_ll_d(lat(i,j-1), lat(i,j), lon(i,j-1), lon(i,j))
            else
               y =  get_ll_d(lat(i,j-1), lat(i,j+1), lon(i,j-1), lon(i,j+1))/2.0
            endif

            if(i.eq.ix) then
                x =  get_ll_d(lat(i,j), lat(i-1,j), lon(i,j), lon(i-1,j))
            else if(i.eq.1) then
                x =  get_ll_d(lat(i,j), lat(i+1,j), lon(i,j), lon(i+1,j))
            else
                x =  get_ll_d(lat(i-1,j), lat(i+1,j), lon(i-1,j), lon(i+1,j))/2.0
            endif
            dist(i,j,9) = x * y 
        end do
      end do
#ifdef HYDRO_D
      write(6,*) "finished get_dist_ll"
#endif
   end subroutine get_dist_ll

!  get dx and dy of map projected
   subroutine get_dxdy_mp(dist,ix,jx,dx,dy)
      implicit none
      integer:: ix,jx 
      real :: dx,dy
      integer:: i,j 
      real :: v1
      ! out variable
      real, dimension(ix,jx,9)::dist
      dist = -1
      v1 = sqrt(dx*dx + dy*dy)
      do j = 1, jx
        do i = 1, ix
          if(j .lt. jx) dist(i,j,1) = dy 
          if(j .lt. jx .and. i .lt. ix) dist(i,j,2) = v1 
          if(i .lt. ix) dist(i,j,3) = dx 
          if(j .gt. 1 .and. i .lt. ix) dist(i,j,4) = v1 
          if(j .gt. 1 ) dist(i,j,5) = dy 
          if(j .gt. 1 .and. i .gt. 1) dist(i,j,6) = v1 
          if(i .gt. 1) dist(i,j,7) = dx 
          if(j .lt. jx .and. i .gt. 1) dist(i,j,8) = v1 
          dist(i,j,9) = dx * dy
        end do
      end do
#ifdef HYDRO_D
      write(6,*) "finished get_dxdy_mp "
#endif
   end subroutine get_dxdy_mp

   subroutine get_dist_lsm(did)
     integer did
#ifdef MPP_LAND
     integer ix,jx,ixrt,jxrt, k
     real , dimension(global_nx,global_ny):: latitude,longitude
     real, dimension(global_nx,global_ny,9):: dist 
     if(nlst_rt(did)%dxrt0 .lt. 0) then
           ! lat and lon grid
          call write_io_real(rt_domain(did)%lat_lsm,latitude) 
          call write_io_real(rt_domain(did)%lon_lsm,longitude) 
          if(my_id.eq.IO_id) then
               call get_dist_ll(dist,latitude,longitude,  &
                         global_nx,global_ny)
          endif
       
     else
           ! mapp projected grid.
          if(my_id.eq.IO_id) then
              call get_dxdy_mp(dist,global_nx,global_ny, &
                 nlst_rt(did)%dxrt0*nlst_rt(did)%AGGFACTRT,nlst_rt(did)%dxrt0*nlst_rt(did)%AGGFACTRT)
          endif
     endif
     do k = 1 , 9
        call decompose_data_real(dist(:,:,k),rt_domain(did)%dist_lsm(:,:,k))
     end do
#else
     if(nlst_rt(did)%dxrt0 .lt. 0) then
        ! lat and lon grid
        call get_dist_ll(rt_domain(did)%dist_lsm,rt_domain(did)%lat_lsm,rt_domain(did)%lon_lsm,  &
                      rt_domain(did)%ix,rt_domain(did)%jx)
     else
        ! mapp projected grid.
        call get_dxdy_mp(rt_domain(did)%dist_lsm,rt_domain(did)%ix,rt_domain(did)%jx, &
              nlst_rt(did)%dxrt0*nlst_rt(did)%AGGFACTRT,nlst_rt(did)%dxrt0*nlst_rt(did)%AGGFACTRT)
     endif
#endif


   end subroutine get_dist_lsm

   subroutine get_dist_lrt(did)
     integer did, k

!     real :: tmp_dist(global_rt_nx, global_rt_ny,9)

! calculate the distance for land routing from the lat /lon of land surface model
     if(nlst_rt(did)%dxrt0 .lt. 0) then
        ! using lat and lon grid when channel routing is off
        call get_rt_dxdy_ll(did)
     else
        ! mapp projected grid.
         call get_dxdy_mp(rt_domain(did)%dist,rt_domain(did)%ixrt,rt_domain(did)%jxrt, &
              nlst_rt(did)%dxrt0,nlst_rt(did)%dxrt0)
#ifdef MPP_LAND
        do k = 1, 9
           call MPP_LAND_COM_REAL(rt_domain(did)%dist(:,:,k),rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
        end do
#endif
     endif


   end subroutine get_dist_lrt

!   subroutine get_dist_crt(did)
!      integer did, k
! calculate the distance from channel routing
!     if(nlst_rt(did)%dxrt0 .lt. 0) then
!        ! lat and lon grid
!        if(rt_domain(did)%dist(1,1,9) .eq. -999)   &
!           call get_dist_ll(rt_domain(did)%dist,rt_domain(did)%latval,rt_domain(did)%lonval,  &
!                      rt_domain(did)%ixrt,rt_domain(did)%jxrt)
!     else
!        ! mapp projected grid.
!        if(rt_domain(did)%dist(1,1,9) .eq. -999)   &
!           call get_dxdy_mp(rt_domain(did)%dist,rt_domain(did)%ixrt,rt_domain(did)%jxrt, &
!              nlst_rt(did)%dxrt0,nlst_rt(did)%dxrt0)
!     endif
!#ifdef MPP_LAND
!     do k = 1, 9
!       call MPP_LAND_COM_REAL(rt_domain(did)%dist(:,:,k),rt_domain(did)%IXRT,rt_domain(did)%JXRT,99)
!     end do
!#endif
!   end subroutine get_dist_crt
   
   subroutine get_basn_area(did)
      implicit none
      integer :: did, ix,jx, k
      real :: basns_area(rt_domain(did)%gnumbasns)
#ifdef MPP_LAND
      integer :: mask(global_nx, global_ny) 
      real :: dist_lsm(global_nx, global_ny,9) 
#else
      integer :: mask(rt_domain(did)%ix, rt_domain(did)%jx)
      real :: dist_lsm(rt_domain(did)%ix, rt_domain(did)%jx,9) 
#endif
#ifdef MPP_LAND
      ix = global_nx
      jx = global_ny
      call write_IO_int(rt_domain(did)%GWSUBBASMSK,mask) 
      do k = 1,  9
         call write_IO_real(rt_domain(did)%dist_lsm(:,:,k),dist_lsm(:,:,k)) 
      end do
#else
      ix = rt_domain(did)%ix
      jx = rt_domain(did)%jx
      mask = rt_domain(did)%GWSUBBASMSK
      dist_lsm = rt_domain(did)%dist_lsm
#endif

#ifdef MPP_LAND
      if(my_id .eq. IO_id) then
         call get_area_g(basns_area,mask, rt_domain(did)%gnumbasns,ix,jx,dist_lsm)
      end if
!      call mpp_land_bcast_real(rt_domain(did)%numbasns,rt_domain(did)%basns_area)

      call gw_decompose_real(rt_domain(did)%gnumbasns, rt_domain(did)%numbasns, &
           rt_domain(did)%basnsInd, basns_area,rt_domain(did)%basns_area)
#else
      call get_area_g(rt_domain(did)%basns_area,mask, rt_domain(did)%gnumbasns,ix,jx,dist_lsm)
#endif
   end subroutine get_basn_area

   subroutine get_area_g(basns_area,GWSUBBASMSK, numbasns,ix,jx,dist)
      integer :: i,j, n, ix,jx, numbasns
      integer :: count(numbasns)
      real :: basns_area(numbasns) , dist(ix,jx,9)
      integer :: GWSUBBASMSK(ix,jx)
      basns_area = 0
      count = 0
      do  j = 1, jx
        do  i = 1, ix
           n = GWSUBBASMSK(i,j)
           if(n .gt. 0) then
              basns_area(n) = basns_area(n)+dist(i,j,9)
              count(n) = count(n) + 1
           endif
        end do
      end do
      do i = 1, numbasns
         if(count(i) .gt. 0) then
             basns_area(i) = basns_area(i) / count(i) 
         end if
      end do
   end subroutine get_area_g


   subroutine get_node_area(did)
       integer :: did
       call get_area_g(rt_domain(did)%node_area,rt_domain(did)%CH_NETLNK, &
         rt_domain(did)%NLINKS,rt_domain(did)%ixrt,rt_domain(did)%jxrt,rt_domain(did)%dist)
   end subroutine get_node_area
    

end module module_HYDRO_utils
