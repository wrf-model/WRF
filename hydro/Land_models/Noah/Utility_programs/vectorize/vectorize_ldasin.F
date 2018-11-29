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

program vectorize_ldasin
  use netcdf
  implicit none

  integer :: ncid_in, ncid_out
  integer :: iret,iloc,ilat,ilon
  integer :: dim_date, dim_we, dim_sn, dim_time
  integer, dimension(23) :: varid_in,  &
       &                    varid_out

  character(len=256) :: attstring
  real               :: attfloat
  integer            :: attint

  integer :: south_north_old
  integer :: west_east_old
  integer :: south_north_new
  integer :: west_east_new
  integer :: k
  integer, dimension(100) :: keep_lu_list
  integer :: landuse_count
  logical :: FOUND_WEASD
  logical :: FOUND_VEGFRA
  logical :: FOUND_CANWAT
  logical :: FOUND_SKINTEMP
  logical :: FOUND_STEMP1
  logical :: FOUND_STEMP2
  logical :: FOUND_STEMP3
  logical :: FOUND_STEMP4
  logical :: FOUND_SMOIS1
  logical :: FOUND_SMOIS2
  logical :: FOUND_SMOIS3
  logical :: FOUND_SMOIS4
  logical :: FOUND_GVFMIN
  logical :: FOUND_GVFMAX
  logical :: FOUND_Z2D
  logical, allocatable, dimension(:,:) :: lumask
  real, allocatable, dimension(:,:,:) :: lu_index
  real, allocatable, dimension(:,:,:) :: t2d_in,       &
       &                                 q2d_in,       &
       &                                 u2d_in,       &
       &                                 v2d_in,       &
       &                                 psfc_in,      &
       &                                 rainrate_in,  &
       &                                 swdown_in,    &
       &                                 lwdown_in,    &
       &                                 weasd_in,     &
       &                                 vegfra_in,    &
       &                                 canwat_in,    &
       &                                 skintemp_in,  &
       &                                 stemp1_in,    &
       &                                 stemp2_in,    &
       &                                 stemp3_in,    &
       &                                 stemp4_in,    &
       &                                 smois1_in,    &
       &                                 smois2_in,    &
       &                                 smois3_in,    &
       &                                 smois4_in,    &
       &                                 gvfmin_in,    &
       &                                 gvfmax_in,    &
       &                                 z2d_in
  real, allocatable, dimension(:,:,:) :: t2d_out,      &
       &                                 q2d_out,      &
       &                                 u2d_out,      &
       &                                 v2d_out,      &
       &                                 psfc_out,     &
       &                                 rainrate_out, &
       &                                 swdown_out,   &
       &                                 lwdown_out,   &
       &                                 weasd_out,    &
       &                                 vegfra_out,   &
       &                                 canwat_out,   &
       &                                 skintemp_out, &
       &                                 stemp1_out,   &
       &                                 stemp2_out,   &
       &                                 stemp3_out,   &
       &                                 stemp4_out,   &
       &                                 smois1_out,   &
       &                                 smois2_out,   &
       &                                 smois3_out,   &
       &                                 smois4_out,   &
       &                                 gvfmin_out,   &
       &                                 gvfmax_out,   &
       &                                 z2d_out

  character(len=256) :: infile
  character(len=256) :: full_geo_em
  character(len=256) :: vector_directory
  character(len=256) :: lustring
  character(len=256) :: dumstr

  namelist/vectorize_namelist/ keep_lu_list, full_geo_em, vector_directory
  
  ! Default values
  keep_lu_list = -99999
  landuse_count = 0
  full_geo_em = " "
  vector_directory = " "
  open(12, file="namelist.vectorize", status='old', form='formatted', action='read')
  read(12, vectorize_namelist)
  close(12)
  do while (keep_lu_list(landuse_count+1) > 0)
     landuse_count = landuse_count + 1
  enddo
  write(*,'("Keep the following ", I4, " land-use categories:")') landuse_count
  write(*,'("         ", I4)') keep_lu_list(1:landuse_count)

  call getarg(1,infile)
  print *, 'Processing file: ',trim(infile)

!!!!!
! READ GEO_EM FILE TO GET THE VEGTYPE
!!!!!

  iret = nf90_open(trim(full_geo_em), NF90_NOWRITE, ncid_in)
  call error_handler(iret, "Problem opening geo_em file ''"//trim(full_geo_em)//"''")

  call get_dimlen(ncid_in, "west_east", west_east_old)
  call get_dimlen(ncid_in, "south_north", south_north_old)

  allocate(lu_index(west_east_old, south_north_old, 1))

  iret = nf90_inq_varid(ncid_in,"LU_INDEX",varid_in(1))
  call error_handler(iret, "Problem getting varid for LU_INDEX from geo_em file")

  iret = nf90_get_var(ncid_in, varid_in(1), lu_index)
  call error_handler(iret, "Problem getting variable LU_INDEX from geo_em file")

  iret = nf90_close(ncid_in)
  call error_handler(iret, "Problem closing geo_em file")

  allocate(t2d_in     (west_east_old, south_north_old, 1))
  allocate(q2d_in     (west_east_old, south_north_old, 1))
  allocate(u2d_in     (west_east_old, south_north_old, 1))
  allocate(v2d_in     (west_east_old, south_north_old, 1))
  allocate(psfc_in    (west_east_old, south_north_old, 1))
  allocate(rainrate_in(west_east_old, south_north_old, 1))
  allocate(swdown_in  (west_east_old, south_north_old, 1))
  allocate(lwdown_in  (west_east_old, south_north_old, 1))
  allocate(weasd_in   (west_east_old, south_north_old, 1))
  allocate(vegfra_in  (west_east_old, south_north_old, 1))
  allocate(canwat_in  (west_east_old, south_north_old, 1))
  allocate(skintemp_in(west_east_old, south_north_old, 1))
  allocate(stemp1_in  (west_east_old, south_north_old, 1))
  allocate(stemp2_in  (west_east_old, south_north_old, 1))
  allocate(stemp3_in  (west_east_old, south_north_old, 1))
  allocate(stemp4_in  (west_east_old, south_north_old, 1))
  allocate(smois1_in  (west_east_old, south_north_old, 1))
  allocate(smois2_in  (west_east_old, south_north_old, 1))
  allocate(smois3_in  (west_east_old, south_north_old, 1))
  allocate(smois4_in  (west_east_old, south_north_old, 1))
  allocate(gvfmin_in  (west_east_old, south_north_old, 1))
  allocate(gvfmax_in  (west_east_old, south_north_old, 1))
  allocate(z2d_in     (west_east_old, south_north_old, 1))

  allocate(lumask(west_east_old, south_north_old))
  lumask = .FALSE.
  do k = 1, landuse_count
     where (nint(lu_index(:,:,1)) == keep_lu_list(k)) lumask = .TRUE.
  enddo
  west_east_new = count(lumask)
  south_north_new = 1
  write(*, '("Count of points to keep = ", I20)') west_east_new

  allocate(t2d_out     (west_east_new, south_north_new, 1))
  allocate(q2d_out     (west_east_new, south_north_new, 1))
  allocate(u2d_out     (west_east_new, south_north_new, 1))
  allocate(v2d_out     (west_east_new, south_north_new, 1))
  allocate(psfc_out    (west_east_new, south_north_new, 1))
  allocate(rainrate_out(west_east_new, south_north_new, 1))
  allocate(swdown_out  (west_east_new, south_north_new, 1))
  allocate(lwdown_out  (west_east_new, south_north_new, 1))
  allocate(weasd_out   (west_east_new, south_north_new, 1))
  allocate(vegfra_out  (west_east_new, south_north_new, 1))
  allocate(canwat_out  (west_east_new, south_north_new, 1))
  allocate(skintemp_out(west_east_new, south_north_new, 1))
  allocate(stemp1_out  (west_east_new, south_north_new, 1))
  allocate(stemp2_out  (west_east_new, south_north_new, 1))
  allocate(stemp3_out  (west_east_new, south_north_new, 1))
  allocate(stemp4_out  (west_east_new, south_north_new, 1))
  allocate(smois1_out  (west_east_new, south_north_new, 1))
  allocate(smois2_out  (west_east_new, south_north_new, 1))
  allocate(smois3_out  (west_east_new, south_north_new, 1))
  allocate(smois4_out  (west_east_new, south_north_new, 1))
  allocate(gvfmin_out  (west_east_new, south_north_new, 1))
  allocate(gvfmax_out  (west_east_new, south_north_new, 1))
  allocate(z2d_out     (west_east_new, south_north_new, 1))

!!!!!
! READ IN VARIABLES
!!!!!

  iret = nf90_open(trim(infile), NF90_NOWRITE, ncid_in)
  call error_handler(iret, "Problem opening input file")

  call read_var_2d(ncid_in, "T2D",      varid_in(1), t2d_in(:,:,1),      west_east_old, south_north_old)
  call read_var_2d(ncid_in, "Q2D",      varid_in(2), q2d_in(:,:,1),      west_east_old, south_north_old)
  call read_var_2d(ncid_in, "U2D",      varid_in(3), u2d_in(:,:,1),      west_east_old, south_north_old)
  call read_var_2d(ncid_in, "V2D",      varid_in(4), v2d_in(:,:,1),      west_east_old, south_north_old)
  call read_var_2d(ncid_in, "PSFC",     varid_in(5), psfc_in(:,:,1),     west_east_old, south_north_old)
  call read_var_2d(ncid_in, "RAINRATE", varid_in(6), rainrate_in(:,:,1), west_east_old, south_north_old)
  call read_var_2d(ncid_in, "SWDOWN",   varid_in(7), swdown_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d(ncid_in, "LWDOWN",   varid_in(8), lwdown_in(:,:,1),   west_east_old, south_north_old)

  ! Optional variables:
  
  call read_var_2d_optional(ncid_in, "WEASD",    FOUND_WEASD,    varid_in(9),  weasd_in(:,:,1),    west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "VEGFRA",   FOUND_VEGFRA,   varid_in(10), vegfra_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "CANWAT",   FOUND_CANWAT,   varid_in(11), canwat_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "SKINTEMP", FOUND_SKINTEMP, varid_in(12), skintemp_in(:,:,1), west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "STEMP_1",  FOUND_STEMP1,   varid_in(13), stemp1_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "STEMP_2",  FOUND_STEMP2,   varid_in(14), stemp2_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "STEMP_3",  FOUND_STEMP3,   varid_in(15), stemp3_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "STEMP_4",  FOUND_STEMP4,   varid_in(16), stemp4_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "SMOIS_1",  FOUND_SMOIS1,   varid_in(17), smois1_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "SMOIS_2",  FOUND_SMOIS2,   varid_in(18), smois2_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "SMOIS_3",  FOUND_SMOIS3,   varid_in(19), smois3_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "SMOIS_4",  FOUND_SMOIS4,   varid_in(20), smois4_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "GVFMIN",   FOUND_GVFMIN,   varid_in(21), gvfmin_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "GVFMAX",   FOUND_GVFMAX,   varid_in(22), gvfmax_in(:,:,1),   west_east_old, south_north_old)
  call read_var_2d_optional(ncid_in, "Z2D",      FOUND_Z2D,      varid_in(23), z2d_in(:,:,1),      west_east_old, south_north_old)

!!!!!
! CREATE A NEW FILE IN VECTOR FORMAT
!!!!!

  iret = nf90_create(trim(vector_directory)//trim(infile(index(infile,"/",.TRUE.):)), NF90_CLOBBER, ncid_out)
  call error_handler(iret, "Problem creating file '"//trim(vector_directory)//trim(infile(index(infile,"/",.TRUE.):))//"'")

  iret = nf90_def_dim(ncid_out,"Time",NF90_UNLIMITED,dim_time)
  call error_handler(iret, "Problem defining dimension 'Time'")

  iret = nf90_def_dim(ncid_out,"DateStrLen",19,dim_date)
  call error_handler(iret, "Problem defining dimension 'DateStrLen'")

  iret = nf90_def_dim(ncid_out,"west_east",west_east_new,dim_we)
  call error_handler(iret, "Problem defining dimension 'west_east'")

  iret = nf90_def_dim(ncid_out,"south_north",south_north_new,dim_sn)
  call error_handler(iret, "Problem defining dimension 'south_north'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "TITLE", attstring)
  call error_handler(iret, "Problem getting attribute 'TITLE'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"TITLE",trim(attstring))
  call error_handler(iret, "Problem putting attribute 'TITLE'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "missing_value", attfloat)
  call error_handler(iret, "Problem getting attribute 'missing_value'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"missing_value", attfloat)
  call error_handler(iret, "Problem putting attribute 'missing_value'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "_FillValue", attfloat)
  call error_handler(iret, "Problem getting attribute '_FillValue'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"_FillValue", attfloat)
  call error_handler(iret, "Problem putting attribute '_FillValue'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"WEST-EAST_GRID_DIMENSION", west_east_new+1)
  call error_handler(iret, "Problem putting attribute 'WEST-EAST_GRID_DIMENSION'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"SOUTH-NORTH_GRID_DIMENSION", south_north_new+1)
  call error_handler(iret, "Problem putting attribute 'SOUTH-NORTH_GRID_DIMENSION'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "DX", attfloat)
  call error_handler(iret, "Problem getting attribute 'DX'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"DX", attfloat)
  call error_handler(iret, "Problem putting attribute 'DX'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "DY", attfloat)
  call error_handler(iret, "Problem getting attribute 'DY'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"DY", attfloat)
  call error_handler(iret, "Problem putting attribute 'DY'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "TRUELAT1", attfloat)
  call error_handler(iret, "Problem getting attribute 'TRUELAT1'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"TRUELAT1", attfloat)
  call error_handler(iret, "Problem putting attribute 'TRUELAT1'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "TRUELAT2", attfloat)
  call error_handler(iret, "Problem getting attribute 'TRUELAT2'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"TRUELAT2", attfloat)
  call error_handler(iret, "Problem putting attribute 'TRUELAT2'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "LA1", attfloat)
  call error_handler(iret, "Problem getting attribute 'LA1'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"LA1", attfloat)
  call error_handler(iret, "Problem putting attribute 'LA1'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "LO1", attfloat)
  call error_handler(iret, "Problem getting attribute 'LO1'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"LO1", attfloat)
  call error_handler(iret, "Problem putting attribute 'LO1'")

!KWM  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"LA2", attfloat)
!KWM  call error_handler(iret, "Problem putting attribute 'LA2'")
!KWM
!KWM  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"LA2", attfloat)
!KWM  call error_handler(iret, "Problem putting attribute 'LA2'")
!KWM
!KWM  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "LO2", attfloat)
!KWM  call error_handler(iret, "Problem getting attribute 'LO2'")
!KWM
!KWM  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"LO2", attfloat)
!KWM  call error_handler(iret, "Problem putting attribute 'LO2'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "STAND_LON", attfloat)
  call error_handler(iret, "Problem getting attribute 'STAND_LON'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"STAND_LON", attfloat)
  call error_handler(iret, "Problem putting attribute 'STAND_LON'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "MAP_PROJ", attint)
  call error_handler(iret, "Problem getting attribute 'MAP_PROJ'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"MAP_PROJ", attint)
  call error_handler(iret, "Problem putting attribute 'MAP_PROJ'")

  iret = nf90_get_att(ncid_in, NF90_GLOBAL, "MMINLU", attstring)
  call error_handler(iret, "Problem getting attribute 'MMINLU'")

  iret = nf90_put_att(ncid_out,NF90_GLOBAL,"MMINLU", attstring)
  call error_handler(iret, "Problem putting attribute 'MMINLU'")

  lustring = " "
  do k = 1, landuse_count
     write(dumstr, *) keep_lu_list(k)
     if (k>1) lustring = trim(lustring)//","
     lustring = trim(lustring) // trim(adjustl(dumstr))
  enddo
  iret = nf90_put_att(ncid_out, NF90_GLOBAL, "vector_landuse_indices", trim(lustring))
  call error_handler(iret,"Problem putting attribute 'vector_landuse_indices'")

!!!!!
! Fill our vectors
!!!!!

  iloc = 0
  do ilat = 1,south_north_old
     do ilon = 1,west_east_old
        if (lumask(ilon,ilat)) then
           iloc = iloc + 1
           if (iloc > west_east_new) then
              print*, 'up to ', iloc
              stop "Too many!"
           endif
           t2d_out(iloc,1,1) = t2d_in(ilon,ilat,1)
           q2d_out(iloc,1,1) = q2d_in(ilon,ilat,1)
           u2d_out(iloc,1,1) = u2d_in(ilon,ilat,1)
           v2d_out(iloc,1,1) = v2d_in(ilon,ilat,1)
           psfc_out(iloc,1,1) = psfc_in(ilon,ilat,1)
           rainrate_out(iloc,1,1) = rainrate_in(ilon,ilat,1)
           swdown_out(iloc,1,1) = swdown_in(ilon,ilat,1)
           lwdown_out(iloc,1,1) = lwdown_in(ilon,ilat,1)
           if (FOUND_WEASD)    weasd_out   (iloc,1,1) = weasd_in   (ilon,ilat,1)
           if (FOUND_VEGFRA)   vegfra_out  (iloc,1,1) = vegfra_in  (ilon,ilat,1) 
           if (FOUND_CANWAT)   canwat_out  (iloc,1,1) = canwat_in  (ilon,ilat,1)
           if (FOUND_SKINTEMP) skintemp_out(iloc,1,1) = skintemp_in(ilon,ilat,1) 
           if (FOUND_STEMP1)   stemp1_out  (iloc,1,1) = stemp1_in  (ilon,ilat,1) 
           if (FOUND_STEMP1)   stemp2_out  (iloc,1,1) = stemp2_in  (ilon,ilat,1) 
           if (FOUND_STEMP1)   stemp3_out  (iloc,1,1) = stemp3_in  (ilon,ilat,1) 
           if (FOUND_STEMP1)   stemp4_out  (iloc,1,1) = stemp4_in  (ilon,ilat,1) 
           if (FOUND_SMOIS1)   smois1_out  (iloc,1,1) = smois1_in  (ilon,ilat,1) 
           if (FOUND_SMOIS1)   smois2_out  (iloc,1,1) = smois2_in  (ilon,ilat,1) 
           if (FOUND_SMOIS1)   smois3_out  (iloc,1,1) = smois3_in  (ilon,ilat,1) 
           if (FOUND_SMOIS1)   smois4_out  (iloc,1,1) = smois4_in  (ilon,ilat,1) 
           if (FOUND_GVFMIN)   gvfmin_out  (iloc,1,1) = gvfmin_in  (ilon,ilat,1) 
           if (FOUND_GVFMAX)   gvfmax_out  (iloc,1,1) = gvfmax_in  (ilon,ilat,1) 
           if (FOUND_Z2D)      z2d_out     (iloc,1,1) = z2d_in     (ilon,ilat,1) 
        endif
     enddo
  enddo

!!!!!
! Define the new variables in our output file
!!!!!

  call define_var(ncid_out, "T2D",      dim_we, dim_sn, dim_time, varid_out(1), "K")
  call define_var(ncid_out, "Q2D",      dim_we, dim_sn, dim_time, varid_out(2), "kg kg{-1}")
  call define_var(ncid_out, "U2D",      dim_we, dim_sn, dim_time, varid_out(3), "m s{-1}")
  call define_var(ncid_out, "V2D",      dim_we, dim_sn, dim_time, varid_out(4), "m s{-1}")
  call define_var(ncid_out, "PSFC",     dim_we, dim_sn, dim_time, varid_out(5), "Pa")
  call define_var(ncid_out, "RAINRATE", dim_we, dim_sn, dim_time, varid_out(6), "mm s{-1}")
  call define_var(ncid_out, "SWDOWN",   dim_we, dim_sn, dim_time, varid_out(7), "W m{-2}")
  call define_var(ncid_out, "LWDOWN",   dim_we, dim_sn, dim_time, varid_out(8), "W m{-2}")

  if ( FOUND_WEASD ) then
     call define_var(ncid_out, "WEASD", dim_we, dim_sn, dim_time, varid_out(9), "kg m{-2}")
  endif

  if ( FOUND_VEGFRA ) then
     call define_var(ncid_out, "VEGFRA", dim_we, dim_sn, dim_time, varid_out(10), "%")
  endif

  if ( FOUND_CANWAT ) then
     call define_var(ncid_out, "CANWAT", dim_we, dim_sn, dim_time, varid_out(11), "kg m{-2}")
  endif

  if ( FOUND_SKINTEMP ) then
     call define_var(ncid_out, "SKINTEMP", dim_we, dim_sn, dim_time, varid_out(12), "K")
  endif

  if ( FOUND_STEMP1 ) then
     call define_var(ncid_out, "STEMP_1", dim_we, dim_sn, dim_time, varid_out(13), "K")
  endif

  if ( FOUND_STEMP2 ) then
     call define_var(ncid_out, "STEMP_2", dim_we, dim_sn, dim_time, varid_out(14), "K")
  endif

  if ( FOUND_STEMP3 ) then
     call define_var(ncid_out, "STEMP_3", dim_we, dim_sn, dim_time, varid_out(15), "K")
  endif

  if ( FOUND_STEMP4 ) then
     call define_var(ncid_out, "STEMP_4", dim_we, dim_sn, dim_time, varid_out(16), "K")
  endif

  if ( FOUND_SMOIS1 ) then
     call define_var(ncid_out, "SMOIS_1", dim_we, dim_sn, dim_time, varid_out(17), "fraction")
  endif

  if ( FOUND_SMOIS2 ) then
     call define_var(ncid_out, "SMOIS_2", dim_we, dim_sn, dim_time, varid_out(18), "fraction")
  endif

  if ( FOUND_SMOIS3 ) then
     call define_var(ncid_out, "SMOIS_3", dim_we, dim_sn, dim_time, varid_out(19), "fraction")
  endif

  if ( FOUND_SMOIS4 ) then
     call define_var(ncid_out, "SMOIS_4", dim_we, dim_sn, dim_time, varid_out(20), "fraction")
  endif

  if ( FOUND_GVFMIN ) then
     call define_var(ncid_out, "GVFMIN", dim_we, dim_sn, dim_time, varid_out(21), "%")
  endif

  if ( FOUND_GVFMAX ) then
     call define_var(ncid_out, "GVFMAX", dim_we, dim_sn, dim_time, varid_out(22), "%")
  endif

  if ( FOUND_Z2D ) then
     call define_var(ncid_out, "Z2D",    dim_we, dim_sn, dim_time, varid_out(23), "m")
  endif

!!!!!
! Get out of define mode
!!!!!

  iret = nf90_enddef(ncid_out)
  call error_handler(iret, "Problem exiting define mode")

!!!!!
! WRITE VARIABLES
!!!!!

  iret = nf90_put_var(ncid_out,varid_out(1),t2d_out)
  call error_handler(iret, "Problem writing variable 'T2D'")

  iret = nf90_put_var(ncid_out,varid_out(2),q2d_out)
  call error_handler(iret, "Problem writing variable 'Q2D'")

  iret = nf90_put_var(ncid_out,varid_out(3),u2d_out)
  call error_handler(iret, "Problem writing variable 'U2D'")

  iret = nf90_put_var(ncid_out,varid_out(4),v2d_out)
  call error_handler(iret, "Problem writing variable 'V2D'")

  iret = nf90_put_var(ncid_out,varid_out(5),psfc_out)
  call error_handler(iret, "Problem writing variable 'PSFC'")

  iret = nf90_put_var(ncid_out,varid_out(6),rainrate_out)
  call error_handler(iret, "Problem writing variable 'RAINRATE'")

  iret = nf90_put_var(ncid_out,varid_out(7),swdown_out)
  call error_handler(iret, "Problem writing variable 'SWDOWN'")

  iret = nf90_put_var(ncid_out,varid_out(8),lwdown_out)
  call error_handler(iret, "Problem writing variable 'LWDOWN'")

  if ( FOUND_WEASD ) then
     iret = nf90_put_var(ncid_out,varid_out(9),weasd_out)
     call error_handler(iret, "Problem writing variable 'WEASD'")
  endif

  if ( FOUND_VEGFRA ) then
     iret = nf90_put_var(ncid_out,varid_out(10),vegfra_out)
     call error_handler(iret, "Problem writing variable 'VEGFRA'")
  endif

  if ( FOUND_CANWAT ) then
     iret = nf90_put_var(ncid_out,varid_out(11),canwat_out)
     call error_handler(iret, "Problem writing variable 'CANWAT'")
  endif

  if ( FOUND_SKINTEMP ) then
     iret = nf90_put_var(ncid_out,varid_out(12),skintemp_out)
     call error_handler(iret, "Problem writing variable 'SKINTEMP'")
  endif

  if ( FOUND_STEMP1 ) then
     iret = nf90_put_var(ncid_out,varid_out(13),stemp1_out)
     call error_handler(iret, "Problem writing variable 'STEMP_1'")
  endif

  if ( FOUND_STEMP2 ) then
     iret = nf90_put_var(ncid_out,varid_out(14),stemp2_out)
     call error_handler(iret, "Problem writing variable 'STEMP_2'")
  endif

  if ( FOUND_STEMP3 ) then
     iret = nf90_put_var(ncid_out,varid_out(15),stemp3_out)
     call error_handler(iret, "Problem writing variable 'STEMP_3'")
  endif

  if ( FOUND_STEMP4 ) then
     iret = nf90_put_var(ncid_out,varid_out(16),stemp4_out)
     call error_handler(iret, "Problem writing variable 'STEMP_4'")
  endif

  if ( FOUND_SMOIS1 ) then
     iret = nf90_put_var(ncid_out,varid_out(17),smois1_out)
     call error_handler(iret, "Problem writing variable 'SMOIS_1'")
  endif

  if ( FOUND_SMOIS2 ) then
     iret = nf90_put_var(ncid_out,varid_out(18),smois2_out)
     call error_handler(iret, "Problem writing variable 'SMOIS_2'")
  endif

  if ( FOUND_SMOIS3 ) then
     iret = nf90_put_var(ncid_out,varid_out(19),smois3_out)
     call error_handler(iret, "Problem writing variable 'SMOIS_3'")
  endif

  if ( FOUND_SMOIS4 ) then
     iret = nf90_put_var(ncid_out,varid_out(20),smois4_out)
     call error_handler(iret, "Problem writing variable 'SMOIS_4'")
  endif

  if ( FOUND_GVFMIN ) then
     iret = nf90_put_var(ncid_out,varid_out(21),gvfmin_out)
     call error_handler(iret, "Problem writing variable 'GVFMIN'")
  endif

  if ( FOUND_GVFMAX ) then
     iret = nf90_put_var(ncid_out,varid_out(22),gvfmax_out)
     call error_handler(iret, "Problem writing variable 'GVFMAX'")
  endif

  if ( FOUND_Z2D ) then
     iret = nf90_put_var(ncid_out,varid_out(23),z2d_out)
     call error_handler(iret, "Problem writing variable 'Z2D'")
  endif

  iret = nf90_close(ncid_out)
  call error_handler(iret, "Problem closing output file")

  iret = nf90_close(ncid_in)
  call error_handler(iret, "Problem closing input file")

end program vectorize_ldasin

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine error_handler(ierr, failure)
  use netcdf
  implicit none
  integer, intent(in) :: ierr
  character(len=*), intent(in) :: failure

  if (ierr == NF90_NOERR) return

  write(*, '("----ERROR---------------------------------------------------")')
  write(*, '(5X,A)') trim(nf90_strerror(ierr))
  if (failure /= "") write(*, '(5X,A)') trim(failure)
  write(*, '("------------------------------------------------------------")')
  stop

end subroutine error_handler

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine get_dimlen(ncid, name, dimlen)
  use netcdf
  implicit none
  integer, intent(in) :: ncid
  character(len=*), intent(in) :: name
  integer, intent(out) :: dimlen

  integer :: dimid
  integer :: ierr

  ierr = nf90_inq_dimid(ncid, name, dimid)
  call error_handler(ierr, "GET_DIMLEN : Problem getting dimension id for '"//name//"'")

  ierr = nf90_inquire_dimension(ncid, dimid, len=dimlen)
  call error_handler(ierr, "GET_DIMLEN : Problem getting dimension length for '"//name//"'")

end subroutine get_dimlen

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine read_var_2d(ncid, name, varid, array, idim, jdim)
  use netcdf
  implicit none
  integer,                    intent(in)  :: ncid
  character(len=*),           intent(in)  :: name
  integer,                    intent(in)  :: idim
  integer,                    intent(in)  :: jdim
  integer,                    intent(out) :: varid
  real, dimension(idim,jdim), intent(out) :: array   ! Assumed-shape array
  
  integer :: iret

  iret = nf90_inq_varid ( ncid, name, varid )
  call error_handler(iret, "Problem getting varid for '"//name//"'")

  iret = nf90_get_var ( ncid, varid, array)
  call error_handler(iret, "Problem getting variable '"//name//"'")

end subroutine read_var_2d

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine read_var_2d_optional(ncid, name, found, varid, array, idim, jdim)
  use netcdf
  implicit none
  integer,                    intent(in)  :: ncid
  character(len=*),           intent(in)  :: name
  integer,                    intent(in)  :: idim
  integer,                    intent(in)  :: jdim
  logical,                    intent(out) :: found
  integer,                    intent(out) :: varid
  real, dimension(idim,jdim), intent(out) :: array   ! Assumed-shape array
  
  integer :: iret

  iret = nf90_inq_varid ( ncid, name, varid )
  if ( iret /= NF90_NOERR ) then
     found = .FALSE.
     write(*,'("Problem getting varid for variable ''",A,"''")') name
     write(*,'("Continuing to process this time without ''",A,"''")') name
  else
     found = .TRUE.
     iret = nf90_get_var(ncid, varid, array)
     call error_handler(iret, "Problem getting variable '"//name//"'")
  endif

end subroutine read_var_2d_optional

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

subroutine define_var(ncid, name, dim_we, dim_sn, dim_time, varid, units)
  use netcdf
  implicit none
  integer,          intent(in)  :: ncid
  character(len=*), intent(in)  :: name
  integer,          intent(in)  :: dim_we
  integer,          intent(in)  :: dim_sn
  integer,          intent(in)  :: dim_time
  integer,          intent(out) :: varid
  character(len=*), intent(in)  :: units
  integer :: iret

  iret = nf90_def_var(ncid, name, NF90_FLOAT, (/dim_we,dim_sn,dim_time/), varid)
  call error_handler(iret, "Problem defining variable '"//name//"'")
  iret = nf90_put_att(ncid,varid,"units",units)
  call error_handler(iret, "Problem putting attribute 'units' for variable '"//name//"'")
  iret = nf90_put_att(ncid,varid,"_FillValue",-1.E36)
  call error_handler(iret, "Problem putting attribute '_FillValue' for variable '"//name//"'")
end subroutine define_var

!-----------------------------------------------------------------------------
!-----------------------------------------------------------------------------

