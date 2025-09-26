program elev_angle

   implicit none

   integer, external :: iargc

   integer :: istatus
   integer :: i, j, n_bins, we_dim, sn_dim
   real, pointer, dimension(:,:) :: hgt
   real, allocatable, dimension(:,:,:) :: topo_angle
   character (len=1024) :: filename

   if (iargc() /= 1) then
      write(6,*) ' '
      write(6,*) 'Usage: elev_angle.exe <geogrid output file>'
      write(6,*) ' '
      stop
   end if

   call getarg(1,filename)

   !
   ! Read in topography field from geogrid output file
   !
   call read_topo_field(filename, hgt, we_dim, sn_dim, istatus)
   if (istatus /= 0) stop

   write(6,*) 'Read HGT_M field dimensioned ',we_dim,sn_dim
   do j=1,sn_dim,10
   do i=1,we_dim,10
      write(6,'(a6,i3,a1,i3,a2,f13.5)') 'HGT_M(',i,',',j,')=',hgt(i,j)
   end do
   end do

   n_bins = 180
   allocate(topo_angle(we_dim, sn_dim, n_bins))

   ! 
   ! Compute elevation angles for each azimuth angle bin
   !
   topo_angle = 10.0 

   !
   ! Write elevation angle data to intermediate file
   !
   call write_elev_angles(topo_angle, we_dim, sn_dim, n_bins, istatus)


   deallocate(topo_angle)
   deallocate(hgt)

   stop

end program elev_angle


subroutine read_topo_field(filename, hgt, we_dim, sn_dim, istatus)

   use netcdf

   implicit none

   ! Arguments
   character (len=*), intent(in) :: filename
   real, pointer, dimension(:,:) :: hgt
   integer, intent(out)          :: we_dim, sn_dim
   integer, intent(out)          :: istatus

   ! Local variables
   integer :: ncid, topo_varid, we_dimid, sn_dimid
   character (len=NF90_MAX_NAME) :: toponame, we_name, sn_name


   istatus = nf90_open(trim(filename), 0, ncid) 
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not open file '//trim(filename)
      write(6,*) ' '
      istatus = 1
      return
   end if

   sn_name = 'south_north'
   istatus = nf90_inq_dimid(ncid, sn_name, sn_dimid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of dimension south_north'
      write(6,*) ' '
      istatus = 1
      return
   end if

   istatus = nf90_inquire_dimension(ncid, sn_dimid, sn_name, sn_dim)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get south_north dimension'
      write(6,*) ' '
      istatus = 1
      return
   end if

   we_name = 'west_east'
   istatus = nf90_inq_dimid(ncid, we_name, we_dimid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of dimension west_east'
      write(6,*) ' '
      istatus = 1
      return
   end if

   istatus = nf90_inquire_dimension(ncid, we_dimid, we_name, we_dim)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get west_east dimension'
      write(6,*) ' '
      istatus = 1
      return
   end if

   toponame = 'HGT_M'
   istatus = nf90_inq_varid(ncid, toponame, topo_varid)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not get ID of variable HGT_M'
      write(6,*) ' '
      istatus = 1
      return
   end if

   allocate(hgt(we_dim,sn_dim))

   istatus = nf90_get_var(ncid, topo_varid, hgt)
   if (istatus /= NF90_NOERR) then
      write(6,*) ' '
      write(6,*) 'Error: Could not read HGT_M field'
      write(6,*) ' '
      istatus = 1
      deallocate(hgt)
      return
   end if

   istatus = nf90_close(ncid)
  
   istatus = 0

end subroutine read_topo_field


subroutine write_elev_angles(field, we_dim, sn_dim, kdim, istatus)

   use write_met_module

   implicit none

   ! Arguments
   real, dimension(we_dim, sn_dim, kdim), intent(in) :: field
   integer, intent(in)                               :: we_dim, sn_dim, kdim 
   integer, intent(out)                              :: istatus

   ! Local variables
   integer :: i
   type (met_data) :: met_angle

   call write_met_init('ELEVANGLES', .true., '0000-00-00_00:00:00', istatus)

   if (istatus /= 0) then
      write(6,*) ' '
      write(6,*) 'Error opening output file ELEVANGLES'
      write(6,*) ' '
      istatus = 1
      return
   end if

   met_angle%version = 5
   met_angle%iproj = PROJ_MERC
   met_angle%field = 'TOPO_ELEV'
   met_angle%units = 'degrees'
   met_angle%desc  = 'Topography elevation'
!   met_angle%iproj = ...
!   met_angle%truelat1 = ...
!   met_angle%blah = ...
!   met_angle%blah2 = ...

   allocate(met_angle%slab(we_dim, sn_dim))

   do i=1,kdim

      met_angle%xlvl = real(i)
      met_angle%slab(:,:) = field(:,:,i)
      call write_next_met_field(met_angle, istatus)

      if (istatus /= 0) then
         write(6,*) 'Error writing data to output file ELEVANGLES'
         istatus = 1
         deallocate(met_angle%slab)
         return
      end if

   end do

   call write_met_close()

   istatus = 0

end subroutine write_elev_angles
