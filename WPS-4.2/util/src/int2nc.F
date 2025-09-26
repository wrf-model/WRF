program int2nc

!  use netcdf
   use module_debug
   use misc_definitions_module
   use read_met_module

   implicit none

  include "netcdf.inc"

   integer, parameter :: NDIMS = 2
   integer :: istatus, dim, i, varid, ablevel, proj, nproj
   real :: fcst, slat, slon, dlat, dlon, nlat, dxn, dyn
   real :: xloncen, tlat1, tlat2, radius, si, sj
   logical :: windrot 
   character (len=132) field, name, cablevel, date, source, units, desc, flnm, nfile
   real, allocatable, dimension(:,:) :: data
   integer :: ncid
   integer, dimension(20) :: dval
   integer :: dimids(NDIMS)
   integer :: tmp_dims(NDIMS)

   type (met_data)             :: fg_data

   character (len=*),dimension(10),parameter :: dname = (/"i1","j1","i2","j2","i3","j3","i4","j4","i5","j5"/)
   character (len=*),parameter :: DATEV = "date"
   character (len=*),parameter :: FCSTV = "forecast"
   character (len=*),parameter :: SOURCEV = "map_source"
   character (len=*),parameter :: LEVELV = "level"
   character (len=*),parameter :: FIELDV = "field"
   character (len=*),parameter :: UNITSV = "units"
   character (len=*),parameter :: DESCV = "description"
   character (len=*),parameter :: NX = "nx"
   character (len=*),parameter :: NY = "ny"
   character (len=*),parameter :: IPROJ = "projection"
   character (len=*),parameter :: STARTI = "starti"
   character (len=*),parameter :: STARTJ = "startj"
   character (len=*),parameter :: STARTLAT = "startlat"
   character (len=*),parameter :: STARTLON = "startlon"
   character (len=*),parameter :: DELTALAT = "deltalat"
   character (len=*),parameter :: DELTALON = "deltalon"
   character (len=*),parameter :: NLATS = "nlats"
   character (len=*),parameter :: DX = "dx"
   character (len=*),parameter :: DY = "dy"
   character (len=*),parameter :: XLONC = "xlonc"
   character (len=*),parameter :: TRUELAT1 = "truelat1"
   character (len=*),parameter :: TRUELAT2 = "truelat2"
   character (len=*),parameter :: EARTH_RADIUS = "earth_radius"
   character (len=*),parameter :: IS_WIND_GRID_REL = "is_wind_grid_rel"
   character (len=*),parameter :: FILLVALUE = "_FillValue"

   dval = 0

   !  Get the input file name from the command line.
   call getarg (1,flnm)

   if (flnm(1:1) == ' ') then
      print *,'USAGE: int2nc.exe <filename>'
      print *,'       where <filename> is the name of an intermediate-format file'
      stop
   end if
   nfile = trim(adjustl(flnm))//".nc"

   call set_debug_level(WARN)

   call read_met_init(trim(flnm), .true., '0000-00-00_00', istatus)
   call check(nf_create(trim(nfile),nf_clobber,ncid))

   print*, 'OPENING FILE: ',trim(adjustl(flnm))

   if(istatus == 0) then
     call read_next_met_field(fg_data,istatus)
     do while (istatus == 0)
       tmp_dims(1) = fg_data%nx
       tmp_dims(2) = fg_data%ny
         do dim = 1,2
           i = 1
           CHECKDIMS : DO
             if(dval(i) == 0) then
               dval(i) = tmp_dims(dim)
               call check(nf_def_dim(ncid,dname(i),dval(i),i)) 
               EXIT CHECKDIMS
             else
               if (dval(i) == tmp_dims(dim) ) then
                 EXIT CHECKDIMS
               end if
             end if
             i = i+1
             CYCLE CHECKDIMS
           END DO CHECKDIMS
         end do
       call read_next_met_field(fg_data,istatus)
     end do
   else
     print *, 'File = ',trim(flnm)
     print *, 'Problem with input file, I can''t open it'
     stop
   end if


   call read_met_close()
   call check(nf_close(ncid))

   call read_met_init(trim(flnm), .true., '0000-00-00_00', istatus)
   call check(nf_open(trim(nfile),nf_write,ncid))

   if (istatus == 0) then
      call read_next_met_field(fg_data, istatus)
      do while (istatus == 0)

         date = trim(adjustl(fg_data%hdate))
         fcst = fg_data%xfcst
         source = fg_data%map_source
         field = fg_data%field
         units = fg_data%units
         desc = fg_data%desc
         ablevel = fg_data%xlvl
         write(cablevel,'(I6)') ablevel
         name = trim(adjustl(field))//"__0"//trim(adjustl(cablevel))
         print *,"Reading Field, Level: ",trim(adjustl(field)),", ",trim(adjustl(cablevel))

         nproj = fg_data%iproj
         proj = nproj
         if(nproj == 1) proj = 3
         if(nproj == 3) proj = 1
         if(proj == 0) then         ! Cylindrical Equidistand
           si = fg_data%starti
           sj = fg_data%startj
           slat = fg_data%startlat
           slon = fg_data%startlon
           dlat = fg_data%deltalat
           dlon = fg_data%deltalon
           radius = fg_data%earth_radius
         else if(proj == 1) then    ! Mercator
           si = fg_data%starti
           sj = fg_data%startj
           slat = fg_data%startlat
           slon = fg_data%startlon
           dxn = fg_data%dx
           dyn = fg_data%dy
           tlat1 = fg_data%truelat1
           radius = fg_data%earth_radius
         else if(proj == 3) then    ! Lambert Conformal
           si = fg_data%starti
           sj = fg_data%startj
           slat = fg_data%startlat
           slon = fg_data%startlon
           dxn = fg_data%dx
           dyn = fg_data%dy
           xloncen = fg_data%xlonc
           tlat1 = fg_data%truelat1
           tlat2 = fg_data%truelat2
           radius = fg_data%earth_radius
         else if(proj == 4) then    ! Gaussian
           si = fg_data%starti
           sj = fg_data%startj
           slat = fg_data%startlat
           slon = fg_data%startlon
           nlat = fg_data%deltalat
           dlon = fg_data%deltalon
           radius = fg_data%earth_radius
         else if(proj == 5) then    ! Polar Stereographic
           si = fg_data%starti
           sj = fg_data%startj
           slat = fg_data%startlat
           slon = fg_data%startlon
           dxn = fg_data%dx
           dyn = fg_data%dy
           xloncen = fg_data%xlonc
           tlat1 = fg_data%truelat1
           radius = fg_data%earth_radius
         end if
         windrot = fg_data%is_wind_grid_rel

         if(allocated(data)) deallocate(data)
         allocate(data(fg_data%nx,fg_data%ny))
         data = fg_data%slab

         tmp_dims(1) = fg_data%nx
         tmp_dims(2) = fg_data%ny
         do dim = 1,2
           i = 1
           CHECKDIMS2 : DO
             if (dval(i) == tmp_dims(dim) ) then
               dimids(dim) = i
               EXIT CHECKDIMS2
             end if
             i = i+1
             CYCLE CHECKDIMS2
           END DO CHECKDIMS2
         end do

         call check(nf_redef(ncid))
         call check(nf_def_var(ncid,name,NF_REAL,NDIMS,dimids,varid))
         call check(nf_put_att_text(ncid,varid,DATEV,132,date))
         call check(nf_put_att_real(ncid,varid,FCSTV,nf_float,1,fcst))
         call check(nf_put_att_text(ncid,varid,SOURCEV,132,source))
         call check(nf_put_att_text(ncid,varid,FIELDV,132,field))
         call check(nf_put_att_text(ncid,varid,UNITSV,132,units))
         call check(nf_put_att_text(ncid,varid,DESCV,132,desc))
         call check(nf_put_att_int (ncid,varid,LEVELV,nf_int,1,ablevel))
         call check(nf_put_att_int (ncid,varid,NX,nf_int,1,fg_data%nx))
         call check(nf_put_att_int (ncid,varid,NY,nf_int,1,fg_data%ny))
         call check(nf_put_att_int (ncid,varid,IPROJ,nf_int,1,proj))
         call check(nf_put_att_real(ncid,varid,FILLVALUE,nf_real,1,-1e30))


         if(proj == 0) then
           call check(nf_put_att_real(ncid,varid,STARTI,nf_float,1,si))
           call check(nf_put_att_real(ncid,varid,STARTJ,nf_float,1,sj))
           call check(nf_put_att_real(ncid,varid,STARTLAT,nf_float,1,slat))
           call check(nf_put_att_real(ncid,varid,STARTLON,nf_float,1,slon))
           call check(nf_put_att_real(ncid,varid,DELTALAT,nf_float,1,dlat))
           call check(nf_put_att_real(ncid,varid,DELTALON,nf_float,1,dlon))
           call check(nf_put_att_real(ncid,varid,EARTH_RADIUS,nf_float,1,radius))
         else if(proj == 1) then
           call check(nf_put_att_real(ncid,varid,STARTI,nf_float,1,si))
           call check(nf_put_att_real(ncid,varid,STARTJ,nf_float,1,sj))
           call check(nf_put_att_real(ncid,varid,STARTLAT,nf_float,1,slat))
           call check(nf_put_att_real(ncid,varid,STARTLON,nf_float,1,slon))
           call check(nf_put_att_real(ncid,varid,DX,nf_float,1,dxn))
           call check(nf_put_att_real(ncid,varid,DY,nf_float,1,dyn))
           call check(nf_put_att_real(ncid,varid,TRUELAT1,nf_float,1,tlat1))
           call check(nf_put_att_real(ncid,varid,EARTH_RADIUS,nf_float,1,radius))
         else if(proj == 3) then
           call check(nf_put_att_real(ncid,varid,STARTI,nf_float,1,si))
           call check(nf_put_att_real(ncid,varid,STARTJ,nf_float,1,sj))
           call check(nf_put_att_real(ncid,varid,STARTLAT,nf_float,1,slat))
           call check(nf_put_att_real(ncid,varid,STARTLON,nf_float,1,slon))
           call check(nf_put_att_real(ncid,varid,DX,nf_float,1,dxn))
           call check(nf_put_att_real(ncid,varid,DY,nf_float,1,dyn))
           call check(nf_put_att_real(ncid,varid,XLONC,nf_float,1,xloncen))
           call check(nf_put_att_real(ncid,varid,TRUELAT1,nf_float,1,tlat1))
           call check(nf_put_att_real(ncid,varid,TRUELAT2,nf_float,1,tlat2))
           call check(nf_put_att_real(ncid,varid,EARTH_RADIUS,nf_float,1,radius))
         else if(proj == 4) then
           call check(nf_put_att_real(ncid,varid,STARTI,nf_float,1,si))
           call check(nf_put_att_real(ncid,varid,STARTJ,nf_float,1,sj))
           call check(nf_put_att_real(ncid,varid,STARTLAT,nf_float,1,slat))
           call check(nf_put_att_real(ncid,varid,STARTLON,nf_float,1,slon))
           call check(nf_put_att_real(ncid,varid,NLATS,nf_float,1,nlat))
           call check(nf_put_att_real(ncid,varid,DELTALON,nf_float,1,dlon))
           call check(nf_put_att_real(ncid,varid,EARTH_RADIUS,nf_float,1,radius))
         else if(proj == 5) then
           call check(nf_put_att_real(ncid,varid,STARTI,nf_float,1,si))
           call check(nf_put_att_real(ncid,varid,STARTJ,nf_float,1,sj))
           call check(nf_put_att_real(ncid,varid,STARTLAT,nf_float,1,slat))
           call check(nf_put_att_real(ncid,varid,STARTLON,nf_float,1,slon))
           call check(nf_put_att_real(ncid,varid,DX,nf_float,1,dxn))
           call check(nf_put_att_real(ncid,varid,DY,nf_float,1,dyn))
           call check(nf_put_att_real(ncid,varid,XLONC,nf_float,1,xloncen))
           call check(nf_put_att_real(ncid,varid,TRUELAT1,nf_float,1,tlat1))
           call check(nf_put_att_real(ncid,varid,EARTH_RADIUS,nf_float,1,radius))
         end if

         call check(nf_enddef(ncid))
         call check(nf_put_var_real(ncid,varid,data))

         call read_next_met_field(fg_data,istatus)

      end do

      call read_met_close()

   end if

   call check(nf_close(ncid))

   print *,'SUCCESSFUL COMPLETION OF PROGRAM INT2NC, ',trim(nfile),' WRITTEN.'

contains
  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf_noerr) then
      print *, trim(nf_strerror(status))
      stop "Stopped"
    end if
  end subroutine check
end program int2nc
