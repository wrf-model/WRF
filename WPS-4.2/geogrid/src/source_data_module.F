!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Module: source_data_module
!
! Description: 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module source_data_module

   use hash_module
   use list_module
   use module_debug
   use misc_definitions_module
 
   ! Parameters
   integer, parameter :: RETURN_LANDMASK = 0, &
                         RETURN_DOMCAT_LM = 1, &
                         RETURN_DFDX_LM = 2, &
                         RETURN_DFDY_LM = 3, &
                         RETURN_FIELDNAME = 4, &
                         RETURN_DOMCAT = 5, &
                         RETURN_DFDX = 6, &
                         RETURN_DFDY = 7
   integer, parameter :: MAX_LANDMASK_CATEGORIES = 100
 
   ! Module variables
   integer :: num_entries
   integer :: next_field = 1
   integer :: output_field_state = RETURN_LANDMASK 
   character (len=128) :: last_output_fieldname = ''
   integer, pointer, dimension(:) :: source_proj, source_wordsize, source_endian, source_fieldtype, &
                  source_dest_fieldtype, source_priority, source_tile_x, source_tile_y, &
                  source_tile_z, source_tile_z_start, source_tile_z_end, source_tile_bdr, &
                  source_category_min, source_category_max, source_smooth_option, &
                  source_smooth_passes, source_output_stagger, source_row_order, &
                  source_filename_digits
   integer :: source_iswater, source_islake, source_isice, source_isurban, source_isoilwater
   real, pointer, dimension(:) :: source_dx, source_dy, source_known_x, source_known_y, &
                  source_known_lat, source_known_lon, source_masked, source_truelat1, source_truelat2, &
                  source_stdlon, source_scale_factor, source_missing_value, source_fill_missing
   character (len=128), pointer, dimension(:) :: source_fieldname, source_path, source_interp_string, &
                  source_dominant_category, source_dominant_only, source_dfdx, source_dfdy, &
                  source_z_dim_name, source_units, source_descr, source_output_flag, source_res
   character (len=128) :: source_mminlu
   logical, pointer, dimension(:) :: is_proj, is_wordsize, is_endian, is_fieldtype, &
                  is_dest_fieldtype, is_priority, is_tile_x, is_tile_y, is_tile_z, &
                  is_tile_z_start, is_tile_z_end, is_tile_bdr, is_category_min, &
                  is_category_max, is_masked, &
                  is_dx, is_dy, is_known_x, is_known_y, &
                  is_known_lat, is_known_lon, is_truelat1, is_truelat2, is_stdlon, &
                  is_scale_factor, is_fieldname, is_path, is_dominant_category, &
                  is_dominant_only, is_dfdx, is_dfdy, is_z_dim_name, &
                  is_smooth_option, is_smooth_passes, is_signed, is_missing_value, &
                  is_fill_missing, is_halt_missing, is_output_stagger, is_row_order, &
                  is_units, is_descr, is_subgrid, is_output_flag, is_optional, is_not_found

   type (list), pointer, dimension(:) :: source_res_path, source_interp_option, source_landmask_land, &
                                         source_landmask_water
   type (hashtable) :: bad_files   ! Track which files produce errors when we try to open them
   type (hashtable) :: duplicate_fnames  ! Remember which output fields we have returned 

 
   contains
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_datalist
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_datalist()
 
      use gridinfo_module
      use stringutil
  
      implicit none
  
      ! Parameters
      integer, parameter :: BUFSIZE = 256
  
      ! Local variables
      integer :: nparams, idx, eos, ispace, comma, i, j, funit
      logical :: have_specification, is_used
      character (len=128) :: res_string, path_string, interp_string, landmask_string
      character (len=BUFSIZE) :: buffer
  
      nparams = 0
      num_entries = 0
  
      do funit=10,100
         inquire(unit=funit, opened=is_used)
         if (.not. is_used) exit
      end do
      open(funit,file=trim(opt_geogrid_tbl_path)//'GEOGRID.TBL',form='formatted',status='old',err=1000)
  
      !
      ! We will first go through the file to determine how many field
      !   specifications there are
      !
    10 read(funit,'(a)',end=20,err=1001) buffer
      call despace(buffer)
  
      ! Is this line a comment?
      if (buffer(1:1) == '#') then
  
      ! Are we beginning a new field specification?
      else if (index(buffer,'=====') /= 0) then
         if (nparams > 0) num_entries = num_entries + 1  
         nparams = 0
  
      else
         eos = index(buffer,'#')
         if (eos /= 0) buffer(eos:BUFSIZE) = ' ' 
   
         ! Does this line contain at least one parameter specification?
         if (index(buffer,'=') /= 0) then
            nparams = nparams + 1
         end if
      end if
      go to 10
  
    20 rewind(funit)
  
      ! In case the last entry ended without a ======== line
      if (nparams > 0) num_entries = num_entries + 1  
  
      call mprintf(.true.,STDOUT,'Parsed %i entries in GEOGRID.TBL', i1=num_entries)
  
      !
      ! Now that we know how many fields the user has specified, allocate
      !   the properly sized arrays
      !
      allocate(source_wordsize(num_entries))
      allocate(source_endian(num_entries))
      allocate(source_fieldtype(num_entries))
      allocate(source_dest_fieldtype(num_entries))
      allocate(source_proj(num_entries))
      allocate(source_priority(num_entries))
      allocate(source_dx(num_entries))
      allocate(source_dy(num_entries))
      allocate(source_known_x(num_entries))
      allocate(source_known_y(num_entries))
      allocate(source_known_lat(num_entries))
      allocate(source_known_lon(num_entries))
      allocate(source_truelat1(num_entries))
      allocate(source_truelat2(num_entries))
      allocate(source_stdlon(num_entries))
      allocate(source_fieldname(num_entries))
      allocate(source_path(num_entries))
      allocate(source_interp_string(num_entries))
      allocate(source_tile_x(num_entries))
      allocate(source_tile_y(num_entries))
      allocate(source_tile_z(num_entries))
      allocate(source_tile_z_start(num_entries))
      allocate(source_tile_z_end(num_entries))
      allocate(source_filename_digits(num_entries))
      allocate(source_category_min(num_entries))
      allocate(source_category_max(num_entries))
      allocate(source_tile_bdr(num_entries))
      allocate(source_masked(num_entries))
      allocate(source_output_stagger(num_entries))
      allocate(source_row_order(num_entries))
      allocate(source_dominant_category(num_entries))
      allocate(source_dominant_only(num_entries))
      allocate(source_dfdx(num_entries))
      allocate(source_dfdy(num_entries))
      allocate(source_scale_factor(num_entries))
      allocate(source_z_dim_name(num_entries))
      allocate(source_units(num_entries))
      allocate(source_descr(num_entries))
      allocate(source_res(num_entries))
      allocate(source_smooth_option(num_entries))
      allocate(source_smooth_passes(num_entries))
      allocate(source_missing_value(num_entries))
      allocate(source_fill_missing(num_entries))
      allocate(source_res_path(num_entries))
      allocate(source_interp_option(num_entries))
      allocate(source_landmask_land(num_entries))
      allocate(source_landmask_water(num_entries))
      allocate(source_output_flag(num_entries))
      do i=1,num_entries
         call list_init(source_res_path(i))
         call list_init(source_interp_option(i))
         call list_init(source_landmask_land(i))
         call list_init(source_landmask_water(i))
      end do
  
      allocate(is_wordsize(num_entries))
      allocate(is_endian(num_entries))
      allocate(is_fieldtype(num_entries))
      allocate(is_dest_fieldtype(num_entries))
      allocate(is_proj(num_entries))
      allocate(is_priority(num_entries))
      allocate(is_dx(num_entries))
      allocate(is_dy(num_entries))
      allocate(is_known_x(num_entries))
      allocate(is_known_y(num_entries))
      allocate(is_known_lat(num_entries))
      allocate(is_known_lon(num_entries))
      allocate(is_truelat1(num_entries))
      allocate(is_truelat2(num_entries))
      allocate(is_stdlon(num_entries))
      allocate(is_fieldname(num_entries))
      allocate(is_path(num_entries))
      allocate(is_tile_x(num_entries))
      allocate(is_tile_y(num_entries))
      allocate(is_tile_z(num_entries))
      allocate(is_tile_z_start(num_entries))
      allocate(is_tile_z_end(num_entries))
      allocate(is_category_min(num_entries))
      allocate(is_category_max(num_entries))
      allocate(is_tile_bdr(num_entries))
      allocate(is_masked(num_entries))
      allocate(is_halt_missing(num_entries))
      allocate(is_output_stagger(num_entries))
      allocate(is_row_order(num_entries))
      allocate(is_dominant_category(num_entries))
      allocate(is_dominant_only(num_entries))
      allocate(is_dfdx(num_entries))
      allocate(is_dfdy(num_entries))
      allocate(is_scale_factor(num_entries))
      allocate(is_z_dim_name(num_entries))
      allocate(is_units(num_entries))
      allocate(is_descr(num_entries))
      allocate(is_smooth_option(num_entries))
      allocate(is_smooth_passes(num_entries))
      allocate(is_signed(num_entries))
      allocate(is_missing_value(num_entries))
      allocate(is_fill_missing(num_entries))
      allocate(is_subgrid(num_entries))
      allocate(is_output_flag(num_entries))
      allocate(is_optional(num_entries))
      allocate(is_not_found(num_entries))

      source_wordsize=0
      source_endian=0
      source_fieldtype=0
      source_dest_fieldtype=0
      source_proj=0
      source_priority=0
      source_dx=0
      source_dy=0
      source_known_x=0
      source_known_y=0
      source_known_lat=0
      source_known_lon=0
      source_truelat1=0
      source_truelat2=0
      source_stdlon=0
      source_fieldname=' '
      source_path=' '
      source_interp_string=' '
      source_tile_x=0
      source_tile_y=0
      source_tile_z=0
      source_tile_z_start=0
      source_tile_z_end=0
      source_filename_digits=5
      source_category_min=0
      source_category_max=0
      source_tile_bdr=0
      source_masked=0
      source_output_stagger=0
      source_row_order=0
      source_dominant_category=' '
      source_dominant_only=' '
      source_dfdx=' '
      source_dfdy=' '
      source_scale_factor=0
      source_z_dim_name=' '
      source_units=' '
      source_descr=' '
      source_res=' '
      source_smooth_option=0
      source_smooth_passes=0
      source_missing_value=0
      source_fill_missing=0
      source_output_flag=' '

      is_wordsize=.false.
      is_endian=.false.
      is_fieldtype=.false.
      is_dest_fieldtype=.false.
      is_proj=.false.
      is_priority=.false.
      is_dx=.false.
      is_dy=.false.
      is_known_x=.false.
      is_known_y=.false.
      is_known_lat=.false.
      is_known_lon=.false.
      is_truelat1=.false.
      is_truelat2=.false.
      is_stdlon=.false.
      is_fieldname=.false.
      is_path=.false.
      is_tile_x=.false.
      is_tile_y=.false.
      is_tile_z=.false.
      is_tile_z_start=.false.
      is_tile_z_end=.false.
      is_category_min=.false.
      is_category_max=.false.
      is_tile_bdr=.false.
      is_masked=.false.
      is_halt_missing=.false.
      is_output_stagger=.false.
      is_row_order=.false.
      is_dominant_category=.false.
      is_dominant_only=.false.
      is_dfdx=.false.
      is_dfdy=.false.
      is_scale_factor=.false.
      is_z_dim_name=.false.
      is_units=.false.
      is_descr=.false.
      is_smooth_option=.false.
      is_smooth_passes=.false.
      is_signed=.false.
      is_missing_value=.false.
      is_fill_missing=.false.
      is_subgrid=.false.
      is_output_flag=.false.
      is_optional=.false.
      is_not_found=.false.

      ! This is the default value of source_mminlu
      ! If this default is changed then you must also modify the variable
      ! default_mminlu in get_source_params
      write(source_mminlu,'(a4)') 'USGS'
      source_iswater = 16
      source_islake = -1
      source_isice = 24
      source_isurban = 1
      source_isoilwater = 14
  
      ! 
      ! Actually read and save the specifications
      !
      nparams = 0
      i = 1
    30 buffer = ' '
      read(funit,'(a)',end=40,err=1001) buffer
      call despace(buffer)
  
      ! Is this line a comment?
      if (buffer(1:1) == '#') then
         ! Do nothing.
  
      ! Are we beginning a new field specification?
      else if (index(buffer,'=====') /= 0) then   !{
         if (nparams > 0) i = i + 1  
         nparams = 0
         if (i <= num_entries) then
!BUG: Are these initializations needed now that we've added initializations for
!     all options after their initial allocation above?
            is_path(i) = .false.
            is_masked(i) = .false.
            is_halt_missing(i) = .false.
            is_output_stagger(i) = .false.
            is_dominant_category(i) = .false.
            is_dominant_only(i) = .false.
            is_dfdx(i) = .false.
            is_dfdy(i) = .false.
            is_dest_fieldtype(i) = .false.
            is_priority(i) = .false.
            is_z_dim_name(i) = .false.
            is_smooth_option(i) = .false.
            is_smooth_passes(i) = .false.
            is_fill_missing(i) = .false.
            is_subgrid(i) = .false.
            is_output_flag(i) = .false.
            is_optional(i) = .false.
         end if
  
      else
         ! Check whether the current line is a comment
         if (buffer(1:1) /= '#') then
            have_specification = .true. 
         else
            have_specification = .false.
         end if

         ! If only part of the line is a comment, just turn the comment into spaces 
         eos = index(buffer,'#')
         if (eos /= 0) buffer(eos:BUFSIZE) = ' ' 
   
         do while (have_specification)   !{
   
            ! If this line has no semicolon, it may contain a single specification,
            !   so we set have_specification = .false. to prevent the line from being 
            !   processed again and "pretend" that the last character was a semicolon
            eos = index(buffer,';')
            if (eos == 0) then
               have_specification = .false.
               eos = BUFSIZE
            end if
    
            idx = index(buffer(1:eos-1),'=')
    
            if (idx /= 0) then   !{
               nparams = nparams + 1
     
               if (index('name',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_fieldname(i) = .true.
                  source_fieldname(i) = ' '
                  source_fieldname(i)(1:ispace-idx) = buffer(idx+1:ispace-1)
     
               else if (index('priority',trim(buffer(1:idx-1))) /= 0) then
                  is_priority(i) = .true.
                  read(buffer(idx+1:eos-1),'(i10)') source_priority(i)
     
               else if (index('dest_type',trim(buffer(1:idx-1))) /= 0) then
                  if (index('continuous',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_dest_fieldtype(i) = .true.
                     source_dest_fieldtype(i) = CONTINUOUS
                  else if (index('categorical',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_dest_fieldtype(i) = .true.
                     source_dest_fieldtype(i) = CATEGORICAL
                  end if
     
               else if (index('interp_option',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  interp_string = ' '
                  interp_string(1:ispace-idx-1) = buffer(idx+1:ispace-1)
                  ispace = index(interp_string,':')
                  if (ispace /= 0) then
                     write(res_string,'(a)') interp_string(1:ispace-1)
                  else
                     res_string = 'default'
                  end if
                  write(interp_string,'(a)') trim(interp_string(ispace+1:128))
                  if (list_search(source_interp_option(i), ckey=res_string, cvalue=interp_string)) then
                     call mprintf(.true., WARN, &
                                  'In GEOGRID.TBL entry %i, multiple interpolation methods are '// &
                                  'given for resolution %s. %s will be used.', &
                                  i1=i, s1=trim(res_string), s2=trim(interp_string))
                  else
                     call list_insert(source_interp_option(i), ckey=res_string, cvalue=interp_string)
                  end if
     
               else if (index('smooth_option',trim(buffer(1:idx-1))) /= 0) then
                  if ((index('1-2-1',trim(buffer(idx+1:eos-1))) /= 0) .and. &
                      (len_trim(buffer(idx+1:eos-1)) == len('1-2-1'))) then
                     is_smooth_option(i) = .true.
                     source_smooth_option(i) = ONETWOONE
                  else if ((index('smth-desmth',trim(buffer(idx+1:eos-1))) /= 0) .and. & 
                      (len_trim(buffer(idx+1:eos-1)) == len('smth-desmth'))) then
                     is_smooth_option(i) = .true.
                     source_smooth_option(i) = SMTHDESMTH
                  else if ((index('smth-desmth_special',trim(buffer(idx+1:eos-1))) /= 0) .and. &
                      (len_trim(buffer(idx+1:eos-1)) == len('smth-desmth_special'))) then
                     is_smooth_option(i) = .true.
                     source_smooth_option(i) = SMTHDESMTH_SPECIAL
                  end if
     
               else if (index('smooth_passes',trim(buffer(1:idx-1))) /= 0) then
                  is_smooth_passes(i) = .true.
                  read(buffer(idx+1:eos-1),'(i10)') source_smooth_passes(i)
       
               else if (index('rel_path',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  path_string = ' '
                  path_string(1:ispace-idx-1) = buffer(idx+1:ispace-1)
                  if (path_string(ispace-idx-1:ispace-idx-1) /= '/') &
                     path_string(ispace-idx:ispace-idx) = '/'
                  if (path_string(1:1) == '/') then
                     path_string(1:127) = path_string(2:128)
                     path_string(128:128) = ' '
                  end if
                  ispace = index(path_string,':')
                  if (ispace /= 0) then
                     write(res_string,'(a)') path_string(1:ispace-1)
                  else
                     res_string = 'default'
                  end if
                  write(path_string,'(a)') trim(geog_data_path)//trim(path_string(ispace+1:128))
                  if (list_search(source_res_path(i), ckey=res_string, cvalue=path_string)) then
                     call mprintf(.true., WARN, &
                                  'In GEOGRID.TBL entry %i, multiple paths are given for '// &
                                  'resolution %s. %s will be used.', &
                                  i1=i, s1=trim(res_string), s2=trim(path_string))
                  else
                     call list_insert(source_res_path(i), ckey=res_string, cvalue=path_string)
                  end if
     
               else if (index('abs_path',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  path_string = ' '
                  path_string(1:ispace-idx-1) = buffer(idx+1:ispace-1)
                  if (path_string(ispace-idx-1:ispace-idx-1) /= '/') &
                     path_string(ispace-idx:ispace-idx) = '/'
                  ispace = index(path_string,':')
                  if (ispace /= 0) then
                     write(res_string,'(a)') path_string(1:ispace-1)
                  else
                     res_string = 'default'
                  end if
                  write(path_string,'(a)') trim(path_string(ispace+1:128))
                  if (list_search(source_res_path(i), ckey=res_string, cvalue=path_string)) then
                     call mprintf(.true., WARN, &
                                  'In GEOGRID.TBL entry %i, multiple paths are given for '// &
                                  'resolution %s. %s will be used.', &
                                  i1=i, s1=trim(res_string), s2=trim(path_string))
                  else
                     call list_insert(source_res_path(i), ckey=res_string, cvalue=path_string)
                  end if
    
               else if (index('output_stagger',trim(buffer(1:idx-1))) /= 0) then
                  if (index('M',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_output_stagger(i) = .true.
                     source_output_stagger(i) = M
                  else if (index('U',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_output_stagger(i) = .true.
                     source_output_stagger(i) = U
                  else if (index('V',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_output_stagger(i) = .true.
                     source_output_stagger(i) = V
                  else if (index('HH',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_output_stagger(i) = .true.
                     source_output_stagger(i) = HH
                  else if (index('VV',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_output_stagger(i) = .true.
                     source_output_stagger(i) = VV
                  end if
     
               else if ((index('landmask_water',trim(buffer(1:idx-1))) /= 0) .and. &
                        (len_trim(buffer(1:idx-1)) == 14)) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  landmask_string = ' '
                  landmask_string(1:ispace-idx-1) = buffer(idx+1:ispace-1)
                  ispace = index(landmask_string,':')
                  if (ispace /= 0) then
                     write(res_string,'(a)') landmask_string(1:ispace-1)
                  else
                     res_string = 'default'
                  end if
                  write(landmask_string,'(a)') trim(landmask_string(ispace+1:128))
                  if (list_search(source_landmask_water(i), ckey=res_string, cvalue=landmask_string)) then
                     call mprintf(.true., WARN, &
                                  'In GEOGRID.TBL entry %i, multiple landmask category specifications are given for '// &
                                  'resolution %s. %s will be used.', &
                                  i1=i, s1=trim(res_string), s2=trim(landmask_string))
                  else
                     call list_insert(source_landmask_water(i), ckey=res_string, cvalue=landmask_string)
                  end if

               else if ((index('landmask_land',trim(buffer(1:idx-1))) /= 0) .and. &
                        (len_trim(buffer(1:idx-1)) == 13)) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  landmask_string = ' '
                  landmask_string(1:ispace-idx-1) = buffer(idx+1:ispace-1)
                  ispace = index(landmask_string,':')
                  if (ispace /= 0) then
                     write(res_string,'(a)') landmask_string(1:ispace-1)
                  else
                     res_string = 'default'
                  end if
                  write(landmask_string,'(a)') trim(landmask_string(ispace+1:128))
                  if (list_search(source_landmask_land(i), ckey=res_string, cvalue=landmask_string)) then
                     call mprintf(.true., WARN, &
                                  'In GEOGRID.TBL entry %i, multiple landmask category specifications are given for '// &
                                  'resolution %s. %s will be used.', &
                                  i1=i, s1=trim(res_string), s2=trim(landmask_string))
                  else
                     call list_insert(source_landmask_land(i), ckey=res_string, cvalue=landmask_string)
                  end if
     
               else if ((index('masked',trim(buffer(1:idx-1))) /= 0) .and. &
                        (len_trim(buffer(1:idx-1)) == 6)) then
                  if (index('water',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_masked(i) = .true.
                     source_masked(i) = 0.
                  else if (index('land',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_masked(i) = .true.
                     source_masked(i) = 1.
                  end if
     
               else if (index('fill_missing',trim(buffer(1:idx-1))) /= 0) then
                  is_fill_missing(i) = .true.
                  read(buffer(idx+1:eos-1),*) source_fill_missing(i)
       
               else if (index('halt_on_missing',trim(buffer(1:idx-1))) /= 0) then
                  if (index('yes',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_halt_missing(i) = .true.
                  else if (index('no',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_halt_missing(i) = .false.
                  end if
     
               else if (index('dominant_category',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_dominant_category(i) = .true.
                  source_dominant_category(i) = ' '
                  source_dominant_category(i)(1:ispace-idx) = buffer(idx+1:ispace-1)
      
               else if (index('dominant_only',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_dominant_only(i) = .true.
                  source_dominant_only(i) = ' '
                  source_dominant_only(i)(1:ispace-idx) = buffer(idx+1:ispace-1)
     
               else if (index('df_dx',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_dfdx(i) = .true.
                  source_dfdx(i) = ' '
                  source_dfdx(i)(1:ispace-idx) = buffer(idx+1:ispace-1)
     
               else if (index('df_dy',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_dfdy(i) = .true.
                  source_dfdy(i) = ' '
                  source_dfdy(i)(1:ispace-idx) = buffer(idx+1:ispace-1)
     
               else if (index('z_dim_name',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_z_dim_name(i) = .true.
                  source_z_dim_name(i) = ' '
                  source_z_dim_name(i)(1:ispace-idx) = buffer(idx+1:ispace-1)

               else if (index('subgrid',trim(buffer(1:idx-1))) /= 0) then
                  if (index('yes',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_subgrid(i) = .true.
                  else if (index('no',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_subgrid(i) = .false.
                  end if

               else if (index('flag_in_output',trim(buffer(1:idx-1))) /= 0) then
                  ispace = idx+1
                  do while ((ispace < eos) .and. (buffer(ispace:ispace) /= ' '))
                     ispace = ispace + 1
                  end do 
                  is_output_flag(i) = .true.
                  source_output_flag(i)(1:ispace-idx) = buffer(idx+1:ispace-1)

               else if (index('optional',trim(buffer(1:idx-1))) /= 0) then
                  if (index('yes',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_optional(i) = .true.
                  else if (index('no',trim(buffer(idx+1:eos-1))) /= 0) then
                     is_subgrid(i) = .false.
                  end if
     
               else
                   call mprintf(.true., WARN, 'In GEOGRID.TBL, unrecognized option %s in '// &
                                'entry %i.',i1=idx, s1=buffer(i:idx-1))
       
               end if
    
            end if   !} index(buffer(1:eos-1),'=') /= 0
    
            buffer = buffer(eos+1:BUFSIZE)
         end do   ! while eos /= 0 }
  
      end if   !} index(buffer, '=====') /= 0
      go to 30
  
    40 close(funit)
  
      ! Check the user specifications for gross errors
      if ( .not. check_data_specification() ) then
         call datalist_destroy()
         call mprintf(.true.,ERROR,'Errors were found in either index files or GEOGRID.TBL.')
      end if
  
      call hash_init(bad_files)
  
      return
  
    1000 call mprintf(.true.,ERROR,'Could not open GEOGRID.TBL')
  
    1001 call mprintf(.true.,ERROR,'Encountered error while reading GEOGRID.TBL')
 
   end subroutine get_datalist
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_source_params
   !
   ! Purpose: For each field, this routine reads in the metadata in the index file 
   !   for the first available resolution of data specified by res_string. Also 
   !   based on res_string, this routine sets the interpolation sequence for the
   !   field. This routine should be called prior to processing a field for each
   !   domain.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_source_params(res_string)
 
      use stringutil
 
      implicit none
  
      ! Parameters
      integer, parameter :: BUFSIZE = 256
  
      ! Arguments
      character (len=128), intent(in) :: res_string
  
      ! Local variables
      integer :: idx, i, is, ie, ispace, eos, iquoted, funit, iostatus
      character (len=128) :: temp_data, temp_interp
      character (len=256) :: test_fname
      character (len=BUFSIZE) :: buffer
      logical :: have_specification, is_used
      logical :: landusef_already_found
      character (len=128) :: cur_mminlu, previous_mminlu 
      character (len=128), parameter :: default_mminlu = 'USGS' 
      logical :: mminlu_found_in_this_entry
      landusef_already_found = .FALSE.
      previous_mminlu = 'NOT_YET_SET'

      ! For each entry in the GEOGRID.TBL file
      ENTRY_LOOP: do idx=1,num_entries
         mminlu_found_in_this_entry = .FALSE.

         ! Initialize metadata
         is_wordsize(idx)  = .false.
         is_endian(idx)    = .false.
         is_row_order(idx) = .false.
         is_fieldtype(idx) = .false.
         is_proj(idx)      = .false.
         is_dx(idx)        = .false.
         is_dy(idx)        = .false.
         is_known_x(idx)   = .false.
         is_known_y(idx)   = .false.
         is_known_lat(idx) = .false.
         is_known_lon(idx) = .false.
         is_truelat1(idx)  = .false.
         is_truelat2(idx)  = .false.
         is_stdlon(idx)    = .false.
         is_tile_x(idx)    = .false.
         is_tile_y(idx)    = .false.
         is_tile_z(idx)    = .false.
         is_tile_z_start(idx) = .false.
         is_tile_z_end(idx)   = .false.
         is_category_min(idx) = .false.
         is_category_max(idx) = .false.
         is_tile_bdr(idx)     = .false.
         is_fieldname(idx)    = .false.
         is_scale_factor(idx) = .false.
         is_units(idx)        = .false.
         is_descr(idx)        = .false.
         is_signed(idx)       = .false.
         is_missing_value(idx) = .false.
         is_not_found(idx)     = .false.
   
         
         ! Set the interpolator sequence for the field to be the first value in res_string that matches
         !   the resolution keyword for an interp_sequence specification
         is = 1
         ie = index(res_string(is:128),'+') - 1
         if (ie <= 0) ie = 128
         temp_interp = res_string(is:ie)
         do while (.not. list_search(source_interp_option(idx), ckey=temp_interp, cvalue=source_interp_string(idx)) &
                   .and. is <= 128)
            call mprintf(.true., INFORM, 'For %s, couldn''t find interpolator sequence for '// &
                         'resolution %s.', &
                         s1=trim(source_fieldname(idx)), s2=trim(temp_interp))
            is = ie+2
            ie = is + index(res_string(is:128),'+') - 2
            if (ie - is <= 0) ie = 128
            temp_interp = res_string(is:ie)
         end do

         if (is > 128) then
            temp_interp = 'default'
            if (list_search(source_interp_option(idx), ckey=temp_interp, cvalue=source_interp_string(idx))) then
               call mprintf(.true., INFORM, 'Using default interpolator sequence for %s.', &
                            s1=trim(source_fieldname(idx)))
            else
               if (is_optional(idx)) then
                  is_not_found(idx) = .true.
                  call mprintf(.true., INFORM, 'Could not find interpolator sequence for requested resolution '// &
                               'for this entry of %s in GEOGRID.TBL and no default interpolator was specified.',&
                               s1=trim(source_fieldname(idx)))
                  call mprintf(.true., INFORM, 'This entry in GEOGRID.TBL of %s is optional and will not be '// &
                               'processed.  However, there may be other entries for %s in GEOGRID.TBL and ' // &
                               'thus this field may yet be processed.', s1=trim(source_fieldname(idx)), &
                               s2=trim(source_fieldname(idx)))
                  cycle ENTRY_LOOP
               else 
                  call mprintf(.true., ERROR, 'Could not find interpolator sequence for requested resolution for %s.'// &
                               ' The sources specified in namelist.wps is not listed in GEOGRID.TBL.', &
                               s1=trim(source_fieldname(idx)))
               end if
            end if
         else
            call mprintf(.true., INFORM, 'Using %s interpolator sequence for %s.', &
                         s1=temp_interp, s2=trim(source_fieldname(idx)))
         end if
   
         ! Set the data source for the field to be the first value in res_string that matches
         !   the resolution keyword for an abs_path or rel_path specification
         is = 1
         ie = index(res_string(is:128),'+') - 1
         if (ie <= 0) ie = 128
         temp_data = res_string(is:ie)
         do while (.not. list_search(source_res_path(idx), ckey=temp_data, cvalue=source_path(idx)) &
                   .and. is <= 128)
            call mprintf(.true., INFORM, 'For %s, couldn''t find %s data source.', &
                         s1=trim(source_fieldname(idx)), s2=trim(temp_data))
            is = ie+2
            ie = is + index(res_string(is:128),'+') - 2
            if (ie - is <= 0) ie = 128
            temp_data = res_string(is:ie)
         end do

         if (is > 128) then
            temp_data = 'default'
            if (list_search(source_res_path(idx), ckey=temp_data, cvalue=source_path(idx))) then
               call mprintf(.true., INFORM, 'Using default data source for %s.', &
                            s1=trim(source_fieldname(idx)))
            else
               call mprintf(.true., ERROR, 'Could not find data resolution for requested resolution for %s. '// &
                            'The source specified in namelist.wps is not listed in GEOGRID.TBL.', &
                            s1=trim(source_fieldname(idx)))
            end if
         else
            call mprintf(.true., INFORM, 'Using %s data source for %s.', &
                         s1=temp_data, s2=trim(source_fieldname(idx)))
         end if
         source_res(idx) = temp_data

         call mprintf(trim(temp_data) /= trim(temp_interp),WARN,'For %s, using %s data source with %s interpolation sequence.', &
                      s1=source_fieldname(idx), s2=temp_data, s3=temp_interp)

         write(test_fname, '(a)') trim(source_path(idx))//'index'
     
         !
         ! Open the index file for the data source for this field, and read in metadata specs
         !
         do funit=10,100
            inquire(unit=funit, opened=is_used)
            if (.not. is_used) exit
         end do
         open(funit,file=trim(test_fname),form='formatted',status='old',iostat=iostatus)
         if (iostatus /= 0) then
            if (is_optional(idx)) then
               is_not_found(idx) = .true.
               call mprintf(.true.,INFORM,'Could not read ''index'' file %s for field %s', s1=trim(test_fname), &
                            s2=trim(source_fieldname(idx)))
               call mprintf(.true.,INFORM,'This field is optional and will not be processed.')
            else
               call mprintf(.true.,ERROR,'Could not open %s', s1=trim(test_fname))
            end if

            cycle ENTRY_LOOP

         end if
   
      30 buffer = ' '
         read(funit,'(a)',end=40, err=1001) buffer
         call despace(buffer)
     
         ! Is this line a comment?
         if (buffer(1:1) == '#') then
            ! Do nothing.
     
         else
            have_specification = .true. 
      
            ! If only part of the line is a comment, just turn the comment into spaces 
            eos = index(buffer,'#')
            if (eos /= 0) buffer(eos:BUFSIZE) = ' ' 
      
            do while (have_specification)   !{
      
               ! If this line has no semicolon, it may contain a single specification,
               !   so we set have_specification = .false. to prevent the line from being 
               !   processed again and pretend that the last character was a semicolon
               eos = index(buffer,';')
               if (eos == 0) then
                  have_specification = .false.
                  eos = BUFSIZE
               end if
       
               i = index(buffer(1:eos-1),'=')
       
               if (i /= 0) then   !{
       
                  if (index('projection',trim(buffer(1:i-1))) /= 0) then
                     if (index('lambert',trim(buffer(i+1:eos-1))) /= 0) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_LC
                     else if (index('polar_wgs84',trim(buffer(i+1:eos-1))) /= 0 .and. &
                              len_trim('polar_wgs84') == len_trim(buffer(i+1:eos-1))) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_PS_WGS84
                     else if (index('albers_nad83',trim(buffer(i+1:eos-1))) /= 0 .and. &
                              len_trim('albers_nad83') == len_trim(buffer(i+1:eos-1))) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_ALBERS_NAD83
                     else if (index('polar',trim(buffer(i+1:eos-1))) /= 0 .and. &
                              len_trim('polar') == len_trim(buffer(i+1:eos-1))) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_PS
                     else if (index('mercator',trim(buffer(i+1:eos-1))) /= 0) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_MERC
                     else if (index('regular_ll',trim(buffer(i+1:eos-1))) /= 0) then
                        is_proj(idx) = .true.
                        source_proj(idx) = PROJ_LATLON
                     end if
        
                  else if (index('type',trim(buffer(1:i-1))) /= 0) then
                     if (index('continuous',trim(buffer(i+1:eos-1))) /= 0) then
                        is_fieldtype(idx) = .true.
                        source_fieldtype(idx) = CONTINUOUS
                     else if (index('categorical',trim(buffer(i+1:eos-1))) /= 0) then
                        is_fieldtype(idx) = .true.
                        source_fieldtype(idx) = CATEGORICAL
                     end if
        
                  else if (index('signed',trim(buffer(1:i-1))) /= 0) then
                     if (index('yes',trim(buffer(i+1:eos-1))) /= 0) then
                        is_signed(idx) = .true.
                     else if (index('no',trim(buffer(i+1:eos-1))) /= 0) then
                        is_signed(idx) = .false.
                     end if
        
                  else if (index('units',trim(buffer(1:i-1))) /= 0) then
                     ispace = i+1
                     iquoted = 0
                     do while (((ispace < eos) .and. (buffer(ispace:ispace) /= ' ')) .or. (iquoted == 1))
                        if (buffer(ispace:ispace) == '"' .or. buffer(ispace:ispace) == '''') iquoted = mod(iquoted+1,2)
                        ispace = ispace + 1
                     end do 
                     is_units(idx) = .true.
                     source_units(idx) = ' '
                     if (buffer(i+1:i+1) == '"' .or. buffer(i+1:i+1) == '''') i = i + 1
                     if (buffer(ispace-1:ispace-1) == '"' .or. buffer(ispace-1:ispace-1) == '''') ispace = ispace - 1
                     source_units(idx)(1:ispace-i) = buffer(i+1:ispace-1)
        
                  else if (index('description',trim(buffer(1:i-1))) /= 0) then
                     ispace = i+1
                     iquoted = 0
                     do while (((ispace < eos) .and. (buffer(ispace:ispace) /= ' ')) .or. (iquoted == 1))
                        if (buffer(ispace:ispace) == '"' .or. buffer(ispace:ispace) == '''') iquoted = mod(iquoted+1,2)
                        ispace = ispace + 1
                     end do 
                     is_descr(idx) = .true.
                     source_descr(idx) = ' '
                     if (buffer(i+1:i+1) == '"' .or. buffer(i+1:i+1) == '''') i = i + 1
                     if (buffer(ispace-1:ispace-1) == '"' .or. buffer(ispace-1:ispace-1) == '''') ispace = ispace - 1
                     source_descr(idx)(1:ispace-i) = buffer(i+1:ispace-1)
        
                  else if (index('mminlu',trim(buffer(1:i-1))) /= 0) then
                     ispace = i+1
                     iquoted = 0
                     do while (((ispace < eos) .and. (buffer(ispace:ispace) /= ' ')) .or. (iquoted == 1))
                        if (buffer(ispace:ispace) == '"' .or. buffer(ispace:ispace) == '''') iquoted = mod(iquoted+1,2)
                        ispace = ispace + 1
                     end do 
                     if (buffer(i+1:i+1) == '"' .or. buffer(i+1:i+1) == '''') i = i + 1
                     if (buffer(ispace-1:ispace-1) == '"' .or. buffer(ispace-1:ispace-1) == '''') ispace = ispace - 1
                     source_mminlu(1:ispace-i) = buffer(i+1:ispace-1)
                     mminlu_found_in_this_entry = .TRUE.
        
                  else if (index('iswater',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),*) source_iswater
          
                  else if (index('islake',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),*) source_islake
          
                  else if (index('isice',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),*) source_isice
          
                  else if (index('isurban',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),*) source_isurban
          
                  else if (index('isoilwater',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),*) source_isoilwater
          
                  else if (index('dx',trim(buffer(1:i-1))) /= 0) then
                     is_dx(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_dx(idx)
          
                  else if (index('dy',trim(buffer(1:i-1))) /= 0) then
                     is_dy(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_dy(idx)
          
                  else if (index('known_x',trim(buffer(1:i-1))) /= 0) then
                     is_known_x(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_known_x(idx)
          
                  else if (index('known_y',trim(buffer(1:i-1))) /= 0) then
                     is_known_y(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_known_y(idx)
          
                  else if (index('known_lat',trim(buffer(1:i-1))) /= 0) then
                     is_known_lat(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_known_lat(idx)
          
                  else if (index('known_lon',trim(buffer(1:i-1))) /= 0) then
                     is_known_lon(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_known_lon(idx)
          
                  else if (index('stdlon',trim(buffer(1:i-1))) /= 0) then
                     is_stdlon(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_stdlon(idx)
          
                  else if (index('truelat1',trim(buffer(1:i-1))) /= 0) then
                     is_truelat1(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_truelat1(idx)
          
                  else if (index('truelat2',trim(buffer(1:i-1))) /= 0) then
                     is_truelat2(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_truelat2(idx)
          
                  else if (index('wordsize',trim(buffer(1:i-1))) /= 0) then
                     is_wordsize(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_wordsize(idx)
        
                  else if (index('endian',trim(buffer(1:i-1))) /= 0) then
                     if (index('big',trim(buffer(i+1:eos-1))) /= 0) then
                        is_endian(idx) = .true.
                        source_endian(idx) = BIG_ENDIAN
                     else if (index('little',trim(buffer(i+1:eos-1))) /= 0) then
                        is_endian(idx) = .true.
                        source_endian(idx) = LITTLE_ENDIAN
                     else
                        call mprintf(.true.,WARN,'Invalid value for keyword ''endian'' '// &
                                     'specified in index file. BIG_ENDIAN will be used.')
                     end if
          
                  else if (index('row_order',trim(buffer(1:i-1))) /= 0) then
                     if (index('bottom_top',trim(buffer(i+1:eos-1))) /= 0) then
                        is_row_order(idx) = .true.
                        source_row_order(idx) = BOTTOM_TOP
                     else if (index('top_bottom',trim(buffer(i+1:eos-1))) /= 0) then
                        is_row_order(idx) = .true.
                        source_row_order(idx) = TOP_BOTTOM
                     end if
          
                  else if (index('tile_x',trim(buffer(1:i-1))) /= 0) then
                     is_tile_x(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_x(idx)
        
                  else if (index('tile_y',trim(buffer(1:i-1))) /= 0) then
                     is_tile_y(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_y(idx)
        
                  else if (index('tile_z',trim(buffer(1:i-1))) /= 0) then
                     is_tile_z(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_z(idx)
        
                  else if (index('tile_z_start',trim(buffer(1:i-1))) /= 0) then
                     is_tile_z_start(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_z_start(idx)
        
                  else if (index('tile_z_end',trim(buffer(1:i-1))) /= 0) then
                     is_tile_z_end(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_z_end(idx)
        
                  else if (index('filename_digits',trim(buffer(1:i-1))) /= 0) then
                     read(buffer(i+1:eos-1),'(i10)') source_filename_digits(idx)
        
                  else if (index('category_min',trim(buffer(1:i-1))) /= 0) then
                     is_category_min(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_category_min(idx)
        
                  else if (index('category_max',trim(buffer(1:i-1))) /= 0) then
                     is_category_max(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_category_max(idx)
        
                  else if (index('tile_bdr',trim(buffer(1:i-1))) /= 0) then
                     is_tile_bdr(idx) = .true.
                     read(buffer(i+1:eos-1),'(i10)') source_tile_bdr(idx)
        
                  else if (index('missing_value',trim(buffer(1:i-1))) /= 0) then
                     is_missing_value(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_missing_value(idx)
        
                  else if (index('scale_factor',trim(buffer(1:i-1))) /= 0) then
                     is_scale_factor(idx) = .true.
                     read(buffer(i+1:eos-1),*) source_scale_factor(idx)
          
                  else
                     call mprintf(.true., WARN, 'In %s, unrecognized option %s in entry %i.', &
                                  s1=trim(test_fname), s2=buffer(i:i-1), i1=i)
          
                  end if
       
               end if   !} index(buffer(1:eos-1),'=') /= 0
       
               buffer = buffer(eos+1:BUFSIZE)
            end do   ! while eos /= 0 }
     
         end if   !} index(buffer, '=====') /= 0
     
         go to 30
    
    40 close(funit)
 
       !Make sure we do not use two sources of land use data that use different
       !categories (e.g., USGS categories for some and NLCD2006 for others)
       if( trim(source_fieldname(idx)) .eq. 'LANDUSEF' ) then
        !If mminlu was defined for the current LANDUSEF entry then store that as the
        !current value, otherwise use the default value
        if( mminlu_found_in_this_entry ) then
         cur_mminlu = source_mminlu
        else
         cur_mminlu = default_mminlu
        end if
        !If a previous entry in GEOGRID.TBL already read in LANDUSEF 
        if ( landusef_already_found ) then
         !Check mminlu string found for previous LANDUSEF entry to see if it matches the current entry
         if ( previous_mminlu .ne. cur_mminlu ) then
          if( mminlu_found_in_this_entry ) then !Current entry DID have an MMINLU setting
           !Previous LANDUSEF entry used default mminlu (either explicitly specified mminlu that is the
           !default or did not specify anything and defaulted to this value)
           if ( previous_mminlu .ne. default_mminlu ) then 
            call mprintf(.true., ERROR, 'MMINLU values differ among input fields.  Based on reading '// &
                         '%s using entry %i from GEOGRID.TBL mminlu = %s but a previous GEOGRID.TBL ' // &
                         'entry resulted in mminlu being set to to %s', &
                         s1=trim(source_fieldname(idx)),i1=idx,s2=trim(cur_mminlu), &
                         s3=trim(previous_mminlu))
           !Previous LANDUSEF entry did NOT use default mminlu
           else
            call mprintf(.true., ERROR, 'MMINLU values differ among input fields.  Based on reading '// &
                         '%s using entry %i from GEOGRID.TBL mminlu = %s but a previous GEOGRID.TBL '// &
                         'entry resulted in mminlu being set to (or defaulting to) %s.', &
                         s1=trim(source_fieldname(idx)),i1=idx, &
                         s2=trim(cur_mminlu), s3=trim(previous_mminlu))
           end if !If previous LANDUSEF entry used default MMINLU
          !Current entry did not have an MMINLU setting
          else 
            call mprintf(.true., ERROR, 'MMINLU values differ among input fields.  Reading %s using ' // &
                         'entry %i from GEOGRID.TBL did not set mminlu, and thus the default value ' // &
                         'of %s is assumed.  However, mminlu was previously set to %s', &
                         s1=trim(source_fieldname(idx)),i1=idx,s2=trim(cur_mminlu), &
                         s3=trim(previous_mminlu))
          end if !If the current entry explicitly set MMINLU
         end if !If MMINLU associated with previous LANDUSEF entry matches current LANDUSEF entry
        end if !If previous GEOGRID.TBL entry already read in LANDUSEF

        landusef_already_found = .TRUE.
        previous_mminlu = cur_mminlu
       end if

      end do ENTRY_LOOP

      return
  
    1001 call mprintf(.true.,ERROR,'Encountered error while reading %s', s1=trim(test_fname))
 
   end subroutine get_source_params
 
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: datalist_destroy()
   !
   ! Purpose: This routine deallocates any memory that was allocated by the 
   !   get_datalist() subroutine.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine datalist_destroy()
 
      implicit none
  
      ! Local variables
      integer :: i
  
      if (associated(source_wordsize)) then
         deallocate(source_wordsize)
         deallocate(source_endian)
         deallocate(source_fieldtype)
         deallocate(source_dest_fieldtype)
         deallocate(source_proj)
         deallocate(source_priority)
         deallocate(source_dx)
         deallocate(source_dy)
         deallocate(source_known_x)
         deallocate(source_known_y)
         deallocate(source_known_lat)
         deallocate(source_known_lon)
         deallocate(source_truelat1)
         deallocate(source_truelat2)
         deallocate(source_stdlon)
         deallocate(source_fieldname)
         deallocate(source_path)
         deallocate(source_interp_string)
         deallocate(source_tile_x)
         deallocate(source_tile_y)
         deallocate(source_tile_z)
         deallocate(source_tile_z_start)
         deallocate(source_tile_z_end)
         deallocate(source_filename_digits)
         deallocate(source_tile_bdr)
         deallocate(source_category_min)
         deallocate(source_category_max)
         deallocate(source_masked)
         deallocate(source_output_stagger)
         deallocate(source_row_order)
         deallocate(source_dominant_category)
         deallocate(source_dominant_only)
         deallocate(source_dfdx)
         deallocate(source_dfdy)
         deallocate(source_scale_factor)
         deallocate(source_z_dim_name)
         deallocate(source_smooth_option)
         deallocate(source_smooth_passes)
         deallocate(source_units)
         deallocate(source_descr)
         deallocate(source_res)
         deallocate(source_missing_value)
         deallocate(source_fill_missing)
         do i=1,num_entries
            call list_destroy(source_res_path(i))
            call list_destroy(source_interp_option(i))
            call list_destroy(source_landmask_land(i))
            call list_destroy(source_landmask_water(i))
         end do
         deallocate(source_res_path)
         deallocate(source_interp_option)
         deallocate(source_landmask_land)
         deallocate(source_landmask_water)
         deallocate(source_output_flag)
     
         deallocate(is_wordsize)
         deallocate(is_endian)
         deallocate(is_fieldtype)
         deallocate(is_dest_fieldtype)
         deallocate(is_proj)
         deallocate(is_priority)
         deallocate(is_dx)
         deallocate(is_dy)
         deallocate(is_known_x)
         deallocate(is_known_y)
         deallocate(is_known_lat)
         deallocate(is_known_lon)
         deallocate(is_truelat1)
         deallocate(is_truelat2)
         deallocate(is_stdlon)
         deallocate(is_fieldname)
         deallocate(is_path)
         deallocate(is_tile_x)
         deallocate(is_tile_y)
         deallocate(is_tile_z)
         deallocate(is_tile_z_start)
         deallocate(is_tile_z_end)
         deallocate(is_tile_bdr)
         deallocate(is_category_min)
         deallocate(is_category_max)
         deallocate(is_masked)
         deallocate(is_halt_missing)
         deallocate(is_output_stagger)
         deallocate(is_row_order)
         deallocate(is_dominant_category)
         deallocate(is_dominant_only)
         deallocate(is_dfdx)
         deallocate(is_dfdy)
         deallocate(is_scale_factor)
         deallocate(is_z_dim_name)
         deallocate(is_smooth_option)
         deallocate(is_smooth_passes)
         deallocate(is_signed)
         deallocate(is_units)
         deallocate(is_descr)
         deallocate(is_missing_value)
         deallocate(is_fill_missing)
         deallocate(is_subgrid)
         deallocate(is_output_flag)
         deallocate(is_optional)
         deallocate(is_not_found)
      end if
  
      call hash_destroy(bad_files)
 
   end subroutine datalist_destroy
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: reset_next_field
   !
   ! Purpose: To reset the pointer to the next field in the list of fields 
   !   specified by the user.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine reset_next_field()
 
      implicit none
  
      next_field = 1
 
   end subroutine reset_next_field
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_next_fieldname
   !
   ! Purpose: Calling this routine results in field_name being set to the name of
   !   the field currently pointed to. If istatus /= 0 upon return, an error 
   !   occurred, and the value of field_name is undefined.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_next_fieldname(field_name, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      character (len=128), intent(out) :: field_name
  
      istatus = 1
  
      if (next_field <= num_entries) then
  
         field_name = source_fieldname(next_field)
         next_field = next_field + 1
         istatus = 0
  
      end if
     
   end subroutine get_next_fieldname
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_next_output_fieldname
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   recursive subroutine get_next_output_fieldname(nest_num, field_name, ndims, &
                                            min_cat, max_cat, istagger, memorder, &
                                            dimnames, units, description, sr_x, sr_y, &
                                            derived_from, istatus)

      use gridinfo_module
 
      implicit none
  
#include "wrf_io_flags.h"
  
      ! Arguments
      integer, intent(in) :: nest_num
      integer, intent(out) :: istatus, ndims, istagger, min_cat, max_cat
      integer, intent(out) :: sr_x, sr_y
      character (len=128), intent(out) :: memorder, field_name, units, description, derived_from
      character (len=128), dimension(3), intent(out) :: dimnames
  
      ! Local variables
      integer :: temp_fieldtype
      integer, dimension(MAX_LANDMASK_CATEGORIES) :: landmask
      logical :: is_water_mask, is_dom_only
      character (len=128) :: domcat_name, dfdx_name, dfdy_name
      character (len=256) :: temphash
  
      istatus = 1
  
      if (output_field_state == RETURN_LANDMASK) then
         call hash_init(duplicate_fnames)
         call get_landmask_field(geog_data_res(nest_num), field_name, is_water_mask, landmask, istatus)
         derived_from = ''
         last_output_fieldname(1:128) = field_name(1:128)
         if (istatus == 0) then
            temphash(129:256) = ' '
            temphash(1:128) = field_name(1:128)
            call hash_insert(duplicate_fnames, temphash)
            call get_domcategory_name(field_name, domcat_name, is_dom_only, istatus)
            ! We will only save the dominant category
            if (is_dom_only .and. (istatus == 0)) then
               output_field_state = RETURN_DOMCAT_LM
               call get_next_output_fieldname(nest_num, field_name, ndims, &
                                              min_cat, max_cat, istagger, &
                                              memorder, dimnames, units, description, &
                                              sr_x, sr_y, derived_from, istatus)
               return
            else
               ndims = 2
               min_cat = 1
               max_cat = 1
               temp_fieldtype = iget_fieldtype(field_name, istatus)
               if (istatus == 0) then
                  if (temp_fieldtype == CONTINUOUS) then
                     call get_max_levels(field_name, min_cat, max_cat, istatus)
                  else if (temp_fieldtype == CATEGORICAL) then
                     call get_max_categories(field_name, min_cat, max_cat, istatus)
                  end if
                  if (max_cat - min_cat > 0) ndims = 3
               end if
               call get_output_stagger(field_name, istagger, istatus)
               if (istagger == M) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == U) then
                  dimnames(1) = 'west_east_stag' 
                  dimnames(2) = 'south_north' 
               else if (istagger == V) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north_stag' 
               else if (istagger == HH) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == VV) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               end if
               if (ndims == 2) then
                  memorder = 'XY ' 
                  dimnames(3) = ' '
               else if (ndims == 3) then
                  memorder = 'XYZ' 
                  call get_z_dim_name(field_name, dimnames(3), istatus)
                  istatus = 0 
               else
                  memorder = '   ' 
                  dimnames(3) = ' '
               end if
               call get_subgrid_dim_name(nest_num, field_name, dimnames(1:2), sr_x, sr_y, istatus)
               call get_source_units(field_name, 1, units, istatus)
               if (istatus /= 0) units = '-'
               call get_source_descr(field_name, 1, description, istatus)
               if (istatus /= 0) description = '-'
               istatus = 0
               output_field_state = RETURN_DOMCAT_LM
            end if
         else
            output_field_state = RETURN_FIELDNAME
            call get_next_output_fieldname(nest_num, field_name, ndims, &
                                           min_cat, max_cat, istagger, &
                                           memorder, dimnames, units, description, &
                                           sr_x, sr_y, derived_from, istatus)
            return
         end if
  
      else if (output_field_state == RETURN_FIELDNAME) then
         call get_next_fieldname(field_name, istatus)
         derived_from = ''
         last_output_fieldname(1:128) = field_name(1:128)
         temphash(129:256) = ' '
         temphash(1:128) = field_name(1:128)
         if (istatus == 0 .and. (.not. hash_search(duplicate_fnames, temphash))) then
            call hash_insert(duplicate_fnames, temphash)
            call get_domcategory_name(field_name, domcat_name, is_dom_only, istatus)
            ! We will only save the dominant category
            if (is_dom_only .and. (istatus == 0)) then
               output_field_state = RETURN_DOMCAT
               call get_next_output_fieldname(nest_num, field_name, ndims, &
                                              min_cat, max_cat, istagger, &
                                              memorder, dimnames, units, description, &
                                              sr_x, sr_y, derived_from, istatus)
               return
     
            ! Return the fractional field
            else
               ndims = 2
               min_cat = 1
               max_cat = 1
               temp_fieldtype = iget_fieldtype(field_name, istatus)
               if (istatus == 0) then
                  if (temp_fieldtype == CONTINUOUS) then
                     call get_max_levels(field_name, min_cat, max_cat, istatus)
                  else if (temp_fieldtype == CATEGORICAL) then
                     call get_max_categories(field_name, min_cat, max_cat, istatus)
                  end if
                  if (max_cat - min_cat > 0) ndims = 3
               end if
               call get_output_stagger(field_name, istagger, istatus)
               if (istagger == M) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == U) then
                  dimnames(1) = 'west_east_stag' 
                  dimnames(2) = 'south_north' 
               else if (istagger == V) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north_stag' 
               else if (istagger == HH) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == VV) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               end if
               if (ndims == 2) then
                  memorder = 'XY ' 
                  dimnames(3) = ' '
               else if (ndims == 3) then
                  memorder = 'XYZ' 
                  call get_z_dim_name(field_name, dimnames(3), istatus)
                  istatus = 0 
               else
                  memorder = '   ' 
                  dimnames(3) = ' '
               end if
               call get_subgrid_dim_name(nest_num, field_name, dimnames(1:2), sr_x, sr_y, istatus)
               call get_source_units(field_name, 1, units, istatus)
               if (istatus /= 0) units = '-'
               call get_source_descr(field_name, 1, description, istatus)
               if (istatus /= 0) description = '-'
               istatus = 0
               output_field_state = RETURN_DOMCAT  
            end if 
         else if (istatus /= 0) then
            output_field_state = RETURN_LANDMASK
            call hash_destroy(duplicate_fnames)
            return 
         else if (hash_search(duplicate_fnames, temphash)) then
            call get_next_output_fieldname(nest_num, field_name, ndims, &
                                           min_cat, max_cat, istagger, &
                                           memorder, dimnames, units, description, &
                                           sr_x, sr_y, derived_from, istatus)
            return
         end if
  
      else if (output_field_state == RETURN_DOMCAT .or. &
               output_field_state == RETURN_DOMCAT_LM ) then
         derived_from = last_output_fieldname
         if (output_field_state == RETURN_DOMCAT) then
            next_field = next_field - 1
            call get_next_fieldname(field_name, istatus)
         else
            call get_landmask_field(geog_data_res(nest_num), field_name, is_water_mask, landmask, istatus)
         end if
         if (istatus == 0) then
            call get_domcategory_name(field_name, domcat_name, is_dom_only, istatus)
            if (istatus == 0) then
               ndims = 2
               min_cat = 1
               max_cat = 1
               call get_output_stagger(field_name, istagger, istatus)
               if (istagger == M) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == U) then
                  dimnames(1) = 'west_east_stag' 
                  dimnames(2) = 'south_north' 
               else if (istagger == V) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north_stag' 
               else if (istagger == HH) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == VV) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               end if
               dimnames(3) = ' ' 
               memorder = 'XY ' 

               call get_subgrid_dim_name(nest_num, field_name, dimnames(1:2), sr_x, sr_y, istatus)
               field_name = domcat_name
               units = 'category'
               description = 'Dominant category'
               if (output_field_state == RETURN_DOMCAT) then
                  output_field_state = RETURN_DFDX
               else
                  output_field_state = RETURN_DFDX_LM
               end if
            else
               if (output_field_state == RETURN_DOMCAT) then
                  output_field_state = RETURN_DFDX
               else
                  output_field_state = RETURN_DFDX_LM
               end if
               call get_next_output_fieldname(nest_num, field_name, ndims, &
                                              min_cat, max_cat, istagger, &
                                              memorder, dimnames, units, description, &
                                              sr_x, sr_y, derived_from, istatus)
              return
            end if 
         else
            call mprintf(.true., ERROR, 'get_next_output_fieldname(): In state DOMCAT, '// &
                         'but no field name is found.')
         end if
  
      else if (output_field_state == RETURN_DFDX .or. &
               output_field_state == RETURN_DFDX_LM) then
         derived_from = last_output_fieldname
         if (output_field_state == RETURN_DFDX) then
            next_field = next_field - 1
            call get_next_fieldname(field_name, istatus)
         else
            call get_landmask_field(geog_data_res(nest_num), field_name, is_water_mask, landmask, istatus)
         end if
         if (istatus == 0) then
            call get_dfdx_name(field_name, dfdx_name, istatus)
            if (istatus == 0) then
               ndims = 2
               min_cat = 1
               max_cat = 1
               temp_fieldtype = iget_fieldtype(field_name, istatus)
               if (istatus == 0) then
                  if (temp_fieldtype == CONTINUOUS) then
                     call get_max_levels(field_name, min_cat, max_cat, istatus)
                  else if (temp_fieldtype == CATEGORICAL) then
                     call get_max_categories(field_name, min_cat, max_cat, istatus)
                  end if
                  if (max_cat - min_cat > 0) ndims = 3
               end if
               call get_output_stagger(field_name, istagger, istatus)
               if (istagger == M) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == U) then
                  dimnames(1) = 'west_east_stag' 
                  dimnames(2) = 'south_north' 
               else if (istagger == V) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north_stag' 
               else if (istagger == HH) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == VV) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               end if
               if (ndims == 2) then
                  memorder = 'XY ' 
                  dimnames(3) = ' '
               else if (ndims == 3) then
                  memorder = 'XYZ' 
                  call get_z_dim_name(field_name, dimnames(3), istatus)
                  istatus = 0 
               else
                  memorder = '   ' 
                  dimnames(3) = ' '
               end if
!---------- B. Kosovic 2016-10-28 ---- Changes begin
! The following two lines of code are in the wrong place resulting in
! incorrect size of the dfdx array on the nested domain(s)
!               field_name = dfdx_name
!               units = '-'

               call get_subgrid_dim_name(nest_num, field_name, dimnames(1:2), sr_x, sr_y, istatus)
! Correct location of the two lines of code is below (see also dfdy)
               field_name = dfdx_name
               units = '-'
!---------- B. Kosovic 2016-10-28 ----- End of changes 
               description = 'df/dx'
               if (output_field_state == RETURN_DFDX) then
                  output_field_state = RETURN_DFDY
               else
                  output_field_state = RETURN_DFDY_LM
               end if
            else 
               if (output_field_state == RETURN_DFDX) then
                  output_field_state = RETURN_DFDY
               else
                  output_field_state = RETURN_DFDY_LM
               end if
               call get_next_output_fieldname(nest_num, field_name, ndims, &
                                              min_cat, max_cat, istagger, &
                                              memorder, dimnames, units, description, &
                                              sr_x, sr_y, derived_from, istatus)
               return
            end if 
         else
            call mprintf(.true., ERROR, 'get_next_output_fieldname(): In state DFDX, '// &
                         'but no field name is found.')
         end if
  
      else if (output_field_state == RETURN_DFDY .or. &
               output_field_state == RETURN_DFDY_LM) then
         derived_from = last_output_fieldname
         if (output_field_state == RETURN_DFDY) then
            next_field = next_field - 1
            call get_next_fieldname(field_name, istatus)
         else
            call get_landmask_field(geog_data_res(nest_num), field_name, is_water_mask, landmask, istatus)
         end if
         if (istatus == 0) then
            call get_dfdy_name(field_name, dfdy_name, istatus)
            if (istatus == 0) then
               ndims = 2
               min_cat = 1
               max_cat = 1
               temp_fieldtype = iget_fieldtype(field_name, istatus)
               if (istatus == 0) then
                  if (temp_fieldtype == CONTINUOUS) then
                     call get_max_levels(field_name, min_cat, max_cat, istatus)
                  else if (temp_fieldtype == CATEGORICAL) then
                     call get_max_categories(field_name, min_cat, max_cat, istatus)
                  end if
                  if (max_cat - min_cat > 0) ndims = 3
               end if
               call get_output_stagger(field_name, istagger, istatus)
               if (istagger == M) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == U) then
                  dimnames(1) = 'west_east_stag' 
                  dimnames(2) = 'south_north' 
               else if (istagger == V) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north_stag' 
               else if (istagger == HH) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               else if (istagger == VV) then
                  dimnames(1) = 'west_east' 
                  dimnames(2) = 'south_north' 
               end if
               if (ndims == 2) then
                  memorder = 'XY ' 
                  dimnames(3) = ' '
               else if (ndims == 3) then
                  memorder = 'XYZ' 
                  call get_z_dim_name(field_name, dimnames(3), istatus)
                  istatus = 0 
               else
                  memorder = '   ' 
                  dimnames(3) = ' '
               end if
               
               call get_subgrid_dim_name(nest_num, field_name, dimnames(1:2), sr_x, sr_y, istatus)
               field_name = dfdy_name
               units = '-'
               description = 'df/dy'
               output_field_state = RETURN_FIELDNAME
            else
               output_field_state = RETURN_FIELDNAME
               call get_next_output_fieldname(nest_num, field_name, ndims, &
                                              min_cat, max_cat, istagger, &
                                              memorder, dimnames, units, description, &
                                              sr_x, sr_y, derived_from, istatus)
               return
            end if 
         else
            call mprintf(.true., ERROR, 'get_next_output_fieldname(): In state DFDY, but no '// &
                         'field name is found.')
         end if
  
      end if
  
   end subroutine get_next_output_fieldname
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_landmask_field
   !
   ! Purpose: To return the name of the field from which the landmask is to be 
   !   computed. If no error occurs, is_water_mask is .true. if the landmask 
   !   value specifies the value of water, and .false. if the landmask value 
   !   specifies the value of land.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_landmask_field(res_string, landmask_name, is_water_mask, landmask, istatus)
 
      implicit none
  
      ! Arguments
      character (len=128), intent(in) :: res_string
      integer, dimension(:), intent(out) :: landmask
      integer, intent(out) :: istatus
      logical, intent(out) :: is_water_mask
      character (len=128), intent(out) :: landmask_name
  
      ! Local variables
      integer :: j
      integer :: ilen
      integer :: idx
      integer :: is, ie, sos, eos, comma
      character (len=128) :: temp_res, mask_cat_string
  
      istatus = 1
  
      do idx=1,num_entries

         if (list_length(source_landmask_land(idx)) > 0) then
            is = 1
            ie = index(res_string(is:128),'+') - 1
            if (ie <= 0) ie = 128
            temp_res = res_string(is:ie)
            do while (.not. list_search(source_landmask_land(idx), ckey=temp_res, cvalue=mask_cat_string) &
                      .and. is <= 128)
               is = ie+2
               ie = is + index(res_string(is:128),'+') - 2
               if (ie - is <= 0) ie = 128
               temp_res = res_string(is:ie)
            end do

            if (is > 128) then
               temp_res = 'default'
               if (list_search(source_landmask_land(idx), ckey=temp_res, cvalue=mask_cat_string)) then
                  is_water_mask = .false.
                  landmask_name = source_fieldname(idx)
                  istatus = 0
               end if
            else
               is_water_mask = .false.
               landmask_name = source_fieldname(idx)
               istatus = 0
            end if

         end if

         ! Note: The following cannot be an else-if, since different resolutions of data may
         ! specify, alternately, a land or a water mask, and in general we need to search 
         ! both lists

         if (list_length(source_landmask_water(idx)) > 0) then
            is = 1
            ie = index(res_string(is:128),'+') - 1
            if (ie <= 0) ie = 128
            temp_res = res_string(is:ie)
            do while (.not. list_search(source_landmask_water(idx), ckey=temp_res, cvalue=mask_cat_string) &
                      .and. is <= 128)
               is = ie+2
               ie = is + index(res_string(is:128),'+') - 2
               if (ie - is <= 0) ie = 128
               temp_res = res_string(is:ie)
            end do

            if (is > 128) then
               temp_res = 'default'
               if (list_search(source_landmask_water(idx), ckey=temp_res, cvalue=mask_cat_string)) then
                  is_water_mask = .true.
                  landmask_name = source_fieldname(idx)
                  istatus = 0
               end if
            else
               is_water_mask = .true.
               landmask_name = source_fieldname(idx)
               istatus = 0
            end if
         end if

         if (istatus == 0) then
            j = 1
            sos = 0
            eos = 128
            comma = index(mask_cat_string(sos+1:eos-1),',')
            do while (comma > 0 .and. j < MAX_LANDMASK_CATEGORIES)
               read(mask_cat_string(sos+1:sos+comma-1),'(i10)') landmask(j)
               sos = sos + comma
               comma = index(mask_cat_string(sos+1:eos-1),',')
               j = j + 1
            end do
            read(mask_cat_string(sos+1:eos-1),'(i10)') landmask(j)
            j = j + 1
            if (j <= MAX_LANDMASK_CATEGORIES) then   ! Terminate list with a flag value
               landmask(j) = INVALID 
            end if
            exit
         end if
  
      end do
 
   end subroutine get_landmask_field
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_missing_value
   !
   ! Pupose: Return the value used in the source data to indicate missing data.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_missing_value(fieldnm, ilevel, rmissing, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      real, intent(out) :: rmissing
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) .and. &
             (source_priority(idx) == ilevel)) then
   
            if (is_missing_value(idx)) then
               rmissing = source_missing_value(idx)
               istatus = 0
               exit
            end if
   
         end if
      end do
  
   end subroutine get_missing_value
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_source_units
   !
   ! Pupose: Return a string giving the units of the specified source data.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_source_units(fieldnm, ilevel, cunits, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: cunits
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) .and. &
             (source_priority(idx) == ilevel)) then
   
            if (is_units(idx)) then
               cunits = source_units(idx)
               istatus = 0
               exit
            end if
   
         end if
      end do
  
   end subroutine get_source_units
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_source_descr
   !
   ! Pupose: Return a string giving a description of the specified source data.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_source_descr(fieldnm, ilevel, descr, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: descr
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) .and. &
             (source_priority(idx) == ilevel)) then
   
            if (is_units(idx)) then
               descr = source_descr(idx)
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_source_descr
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_missing_fill_value
   !
   ! Pupose: Return the value to fill missing points with.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_missing_fill_value(fieldnm, rmissing, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      real, intent(out) :: rmissing
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) ) then 
   
            if (is_fill_missing(idx)) then
               rmissing = source_fill_missing(idx)
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_missing_fill_value
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_halt_on_missing
   !
   ! Pupose: Returns 1 if the program should halt upon encountering a missing 
   !   value in the final output field, and 0 otherwise.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_halt_on_missing(fieldnm, halt, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      logical, intent(out) :: halt
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 0
      halt = .false.
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) ) then 
   
            if (is_halt_missing(idx)) halt = .true.
   
         end if
      end do
 
   end subroutine get_halt_on_missing
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_masked_value
   !
   ! Pupose: If the field is to be masked by the landmask, returns 0 if the field
   !   is masked over water and 1 if the field is masked over land. If no mask is
   !   to be applied, -1 is returned.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_masked_value(fieldnm, ilevel, masked, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      real, intent(out) :: masked
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 0
      masked = -1.
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) .and. &
             (source_priority(idx) == ilevel)) then
   
            if (is_masked(idx)) then
               masked = source_masked(idx)
               exit
            end if
   
         end if
      end do
 
   end subroutine get_masked_value
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_max_levels
   !
   ! Purpose: Returns the number of levels for the field given by fieldnm. 
   !   The number of levels will generally be specified for continuous fields, 
   !   whereas min/max category will generally be specified for categorical ones.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_max_levels(fieldnm, min_level, max_level, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: min_level, max_level, istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
      logical :: have_found_field
  
      have_found_field = .false.
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dest_fieldtype(idx) .and. (source_dest_fieldtype(idx) /= CONTINUOUS)) then
               call mprintf(.true., WARN, 'In GEOGRID.TBL, destination field type for %s is '// &
                            'not continuous and min/max levels specified.', s1=trim(fieldnm))
            end if
            if (.not. have_found_field) then
               if (is_tile_z_start(idx) .and. is_tile_z_end(idx)) then
                  have_found_field = .true.
                  istatus = 0
                  min_level = source_tile_z_start(idx)
                  max_level = source_tile_z_end(idx)
               else if (is_tile_z(idx)) then
                  have_found_field = .true.
                  istatus = 0
                  min_level = 1
                  max_level = source_tile_z(idx)
               end if
     
               if (.not. (is_tile_z_start(idx) .and. is_tile_z_end(idx))) then
                  if (is_tile_z_start(idx) .or. is_tile_z_end(idx)) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of tile_z_start '// &
                                  'and tile_z_end specified for entry %i.',i1=idx)
                  end if
               end if
            else
               if (is_tile_z_start(idx) .and. is_tile_z_end(idx)) then
                  if (source_tile_z_start(idx) < min_level) min_level = source_tile_z_start(idx)
                  if (source_tile_z_end(idx) > max_level) max_level = source_tile_z_end(idx)
               else if (is_tile_z(idx)) then
                  if (min_level > 1) min_level = 1
                  if (source_tile_z(idx) > max_level) max_level = source_tile_z(idx)
               end if
     
               if (.not. (is_tile_z_start(idx) .and. is_tile_z_end(idx))) then
                  if (is_tile_z_start(idx) .or. is_tile_z_end(idx)) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of tile_z_start '// &
                                  'and tile_z_end specified for entry %i.',i1=idx)
                  end if
               end if
            end if
   
         end if
      end do
 
   end subroutine get_max_levels
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_source_levels
   !
   ! Purpose: Return the min and max z-index for the source data for fieldname
   !   at a specified priority level (compared with the min/max level over
   !   all priority levels, as given by get_max_levels).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_source_levels(fieldnm, ilevel, min_level, max_level, istatus)
     
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: min_level, max_level, istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
    
               if (is_tile_z_start(idx) .and. is_tile_z_end(idx)) then
                  istatus = 0
                  min_level = source_tile_z_start(idx)
                  max_level = source_tile_z_end(idx)
               else if (is_tile_z(idx)) then
                  istatus = 0
                  min_level = 1
                  max_level = source_tile_z(idx)
               end if
     
               if (.not. (is_tile_z_start(idx) .and. is_tile_z_end(idx))) then
                  if (is_tile_z_start(idx) .or. is_tile_z_end(idx)) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of tile_z_start '// &
                                  'and tile_z_end specified for entry %i.',i1=idx)
                  end if 
               end if
    
            end if
         end if
      end do
 
   end subroutine get_source_levels
 
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_max_categories
   !
   ! Purpose: Returns the minimum category and the maximum category for the field
   !   given by fieldnm.
   !   Min/max category will generally be specified for categorical fields, 
   !   whereas the number of levels will generally be specified for continuous 
   !   fields. 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_max_categories(fieldnm, min_category, max_category, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: min_category, max_category, istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
      logical :: have_found_field
  
      have_found_field = .false.
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dest_fieldtype(idx) .and. (source_dest_fieldtype(idx) /= CATEGORICAL)) then
               call mprintf(.true., WARN, &
                            'In GEOGRID.TBL, cannot get min/max categories for continuous '// &
                            'field %s at entry %i. Perhaps the user has requested to '// &
                            'perform a strange operation on the field.', s1=trim(fieldnm), i1=idx)
            end if
            if (.not. have_found_field) then
               if (is_category_min(idx) .and. is_category_max(idx)) then
                  have_found_field = .true.
                  istatus = 0
                  min_category = source_category_min(idx)
                  max_category = source_category_max(idx)
               else if (is_category_min(idx) .or. is_category_max(idx)) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of min_category and '// &
                               'max_category specified for entry %i.',i1=idx)
               end if
            else
               if (is_category_min(idx) .and. is_category_max(idx)) then
                  if (source_category_min(idx) < min_category) min_category = source_category_min(idx)
                  if (source_category_max(idx) > max_category) max_category = source_category_max(idx)
               else if (is_category_min(idx) .or. is_category_max(idx)) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of min_category and '// &
                               'max_category specified for entry %i.',i1=idx)
               end if
            end if
   
         end if
      end do
  
   end subroutine get_max_categories
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_source_categories
   !
   ! Purpose: Return the min and max category for the source data for fieldname
   !   at a specified priority level (compared with the min/max category over
   !   all priority levels, as given by get_max_categories).
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_source_categories(fieldnm, ilevel, min_category, max_category, istatus)
     
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: min_category, max_category, istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
    
               if (is_category_min(idx) .and. is_category_max(idx)) then
                  istatus = 0
                  min_category = source_category_min(idx)
                  max_category = source_category_max(idx)
               else if (is_category_min(idx) .or. is_category_max(idx)) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of min_category '// &
                               'and max_category specified for entry %i.',i1=idx)
               end if 
    
            end if
         end if
      end do
 
   end subroutine get_source_categories
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_domcategory_name
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_domcategory_name(fieldnm, domcat_name, ldominant_only, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      logical, intent(out) :: ldominant_only
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: domcat_name
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      ldominant_only = .false.
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dominant_category(idx)) then
               domcat_name = source_dominant_category(idx) 
               istatus = 0
               if (is_dominant_only(idx)) then
                  call mprintf(.true., WARN, 'In GEOGRID.TBL, both dominant_category and '// &
                               'dominant_only are specified in entry %i. Using specification '// &
                               'for dominant_category.',i1=idx)
                  is_dominant_only(idx) = .false.
               end if
               exit
    
            else if (is_dominant_only(idx)) then
               domcat_name = source_dominant_only(idx) 
               ldominant_only = .true.
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_domcategory_name
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_dfdx_name
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_dfdx_name(fieldnm, dfdx_name, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: dfdx_name
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dfdx(idx)) then
               dfdx_name = source_dfdx(idx) 
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_dfdx_name
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_dfdy_name
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_dfdy_name(fieldnm, dfdy_name, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: dfdy_name
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dfdy(idx)) then
               dfdy_name = source_dfdy(idx) 
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_dfdy_name
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_z_dim_name
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_z_dim_name(fieldnm, z_dim, istatus)
   
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: z_dim
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries 
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (is_z_dim_name(idx)) then
               z_dim = source_z_dim_name(idx)
               istatus = 0
               exit
            end if
         end if
      end do
     
   end subroutine get_z_dim_name
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_field_scale_factor
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_field_scale_factor(fieldnm, ilevel, scale_factor, istatus)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      real, intent(out) :: scale_factor
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm)) .and. &
             (ilevel == source_priority(idx))) then
   
            if (is_scale_factor(idx)) then
               scale_factor = source_scale_factor(idx) 
               istatus = 0
            end if
   
         end if
      end do
 
   end subroutine get_field_scale_factor
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_output_stagger
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_output_stagger(fieldnm, istagger, istatus)
 
      use gridinfo_module
    
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus, istagger
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_output_stagger(idx)) then
               istatus = 0
               istagger = source_output_stagger(idx)
               exit
            else
               if (gridtype == 'C') then
                  istatus = 0
                  istagger = M
                  exit
               else if (gridtype == 'E') then
                  istatus = 0
                  istagger = HH
                  exit
               end if
            end if
   
         end if
      end do
 
   end subroutine get_output_stagger
 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_subgrid_dim_name
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_subgrid_dim_name(nest_num, field_name, dimnames, &
                                   sub_x, sub_y, istatus)

      use gridinfo_module

      implicit none
      integer, intent(in) :: nest_num
      integer, intent(out) :: sub_x, sub_y, istatus
      character(len=128), intent(in) :: field_name
      character(len=128), dimension(2), intent(inout) :: dimnames
      integer :: idx, nlen

      sub_x = 1
      sub_y = 1

      istatus = 0
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(field_name)) /= 0) .and. &
            (len_trim(source_fieldname(idx)) == len_trim(field_name))) then
            if (is_subgrid(idx)) then
               istatus = 0
               if (is_output_stagger(idx)) then
                  call mprintf(.true.,ERROR,'Cannot use subgrids on variables with staggered grids')
               end if
               dimnames(1) = trim(dimnames(1))//"_subgrid"
               dimnames(2) = trim(dimnames(2))//"_subgrid"
               sub_x = subgrid_ratio_x(nest_num)
               sub_y = subgrid_ratio_y(nest_num)
            end if
         end if
      end do

   end subroutine get_subgrid_dim_name
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_output_flag
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_output_flag(fieldnm, output_flag, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      character (len=*), intent(in) :: fieldnm
      character (len=128), intent(out) :: output_flag
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_output_flag(idx)) then
               output_flag = source_output_flag(idx) 
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end subroutine get_output_flag


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_interp_option
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_interp_option(fieldnm, ilevel, interp_opt, istatus)
   
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
      character (len=128), intent(out) :: interp_opt
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
    
               interp_opt = source_interp_string(idx)
               istatus = 0
               exit
    
            end if
         end if
      end do
  
   end subroutine get_interp_option


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_gcel_threshold
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_gcell_threshold(interp_opt, threshold, istatus)
   
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      real, intent(out) :: threshold
      character (len=128), intent(in) :: interp_opt

      ! Local variables
      integer :: i, p1, p2

      istatus = 1
      threshold = 1.0
    
      i = index(interp_opt,'average_gcell')
      if (i /= 0) then
         ! Move the "average_gcell" option to the beginning
!         if (i /= 1) then
!            p1 =  
!         end if

         ! Check for a threshold 
         p1 = index(interp_opt(i:128),'(')
         p2 = index(interp_opt(i:128),')')
         if (p1 /= 0 .and. p2 /= 0) then
            read(interp_opt(p1+1:p2-1),*,err=1000) threshold
         else
            call mprintf(.true., WARN, 'Problem with specified threshold '// &
                         'for average_gcell interp option. Setting threshold to 0.0.')
            threshold = 0.0
         end if
      end if
      istatus = 0
    
      return

      1000 call mprintf(.true., ERROR, 'Threshold option to average_gcell interpolator '// &
                        'must be a real number, enclosed in parentheses immediately '// &
                        'after keyword "average_gcell"')
  
   end subroutine get_gcell_threshold
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_smooth_option
   !
   ! Pupose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_smooth_option(fieldnm, smooth_opt, smooth_passes, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus, smooth_opt, smooth_passes
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_smooth_option(idx)) then
               istatus = 0
               smooth_opt = source_smooth_option(idx)
     
               if (is_smooth_passes(idx)) then
                  smooth_passes = source_smooth_passes(idx)
               else
                  smooth_passes = 1
               end if
     
               exit
            end if
   
         end if
      end do
 
   end subroutine get_smooth_option
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: iget_fieldtype
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function iget_fieldtype(fieldnm, istatus)
 
      implicit none
    
      ! Arguments
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      ! Return value
      integer :: iget_fieldtype
     
      istatus = 1
  
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
   
            if (is_dest_fieldtype(idx)) then
               iget_fieldtype = source_dest_fieldtype(idx) 
               istatus = 0
               exit
            end if
   
         end if
      end do
 
   end function iget_fieldtype
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: iget_source_fieldtype
   !
   ! Purpose: Given a resolution, in degrees, and the name of a field, this 
   !   function returns the type (categorical, continuous, etc.) of the source
   !   data that will be used. This may, in general, depend on the field name
   !   and the resolution; for example, near 30 second resolution, land use data
   !   may come from a categorical field, whereas for lower resolutions, it may
   !   come from arrays of land use fractions for each category.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function iget_source_fieldtype(fieldnm, ilevel, istatus)
 
      implicit none
    
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      character (len=128), intent(in) :: fieldnm
  
      ! Return value
      integer :: iget_source_fieldtype
  
      ! Local variables
      integer :: idx
  
      ! Find information about the source tiles for the specified fieldname
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               istatus = 0
               iget_source_fieldtype = source_fieldtype(idx)
               exit
            end if
         end if
      end do
 
   end function iget_source_fieldtype
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_data_tile
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_data_tile(xlat, xlon, ilevel, field_name, &
                            file_name, array, start_x_dim, end_x_dim, start_y_dim, &
                            end_y_dim, start_z_dim, end_z_dim, npts_bdr, &
                            istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      integer, intent(out) :: start_x_dim, end_x_dim, &
                              start_y_dim, end_y_dim, &
                              start_z_dim, end_z_dim, &
                              npts_bdr
      real, intent(in) :: xlat, xlon         ! Location that tile should contain
      real, pointer, dimension(:,:,:) :: array  ! The array to be allocated by this routine
      character (len=128), intent(in) :: field_name
      character (len=256), intent(out) :: file_name
  
      ! Local variables
      integer :: j, k
      integer :: local_wordsize, local_endian, sign_convention, irow_order, strlen
      integer :: xdim,ydim,zdim
      real :: scalefac
      real, allocatable, dimension(:) :: temprow
  
      call get_tile_fname(file_name, xlat, xlon, ilevel, field_name, istatus)

      if (index(file_name, 'OUTSIDE') /= 0) then
         istatus = 1
         return
      else if (hash_search(bad_files, file_name)) then
         istatus = 1
         return
      end if
  
      call get_tile_dimensions(xlat, xlon, start_x_dim, end_x_dim, start_y_dim, end_y_dim, &
                      start_z_dim, end_z_dim, npts_bdr, local_wordsize, local_endian, &
                      sign_convention, ilevel, field_name, istatus)
  
      xdim = (end_x_dim-start_x_dim+1)
      ydim = (end_y_dim-start_y_dim+1)
      zdim = (end_z_dim-start_z_dim+1)

      if (associated(array)) deallocate(array)
      allocate(array(xdim,ydim,zdim))
  
      call get_row_order(field_name, ilevel, irow_order, istatus)
      if (istatus /= 0) irow_order = BOTTOM_TOP
  
      call s_len(file_name,strlen)

      scalefac = 1.0

      call read_geogrid(file_name, strlen, array, xdim, ydim, zdim, &
                        sign_convention, local_endian, scalefac, local_wordsize, istatus)

      if (irow_order == TOP_BOTTOM) then
         allocate(temprow(xdim))
         do k=1,zdim
            do j=1,ydim
               if (ydim-j+1 <= j) exit               
               temprow(1:xdim)          = array(1:xdim,j,k)
               array(1:xdim,j,k)        = array(1:xdim,ydim-j+1,k)
               array(1:xdim,ydim-j+1,k) = temprow(1:xdim)
            end do
         end do
         deallocate(temprow)
      end if
  
      if (istatus /= 0) then
         start_x_dim = INVALID
         start_y_dim = INVALID
         end_x_dim   = INVALID
         end_y_dim   = INVALID

         call hash_insert(bad_files, file_name)
      end if
 
   end subroutine get_data_tile


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_row_order
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_row_order(fieldnm, ilevel, irow_order, istatus)

      implicit none

      ! Arguments
      integer, intent(in) :: ilevel
      character (len=128), intent(in) :: fieldnm
      integer, intent(out) :: irow_order, istatus

      ! Local variables
      integer :: idx

      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               if (is_row_order(idx)) then
                  irow_order = source_row_order(idx)
                  istatus = 0
                  exit
               end if
            end if
         end if
      end do

   end subroutine get_row_order
  
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: get_tile_dimensions
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   subroutine get_tile_dimensions(xlat, xlon, start_x_dim, end_x_dim, start_y_dim, end_y_dim, &
                        start_z_dim, end_z_dim, npts_bdr, bytes_per_datum, endianness, &
                        sign_convention, ilevel, fieldnm, istatus)
 
      use llxy_module
  
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: start_x_dim, end_x_dim, &
                              start_y_dim, end_y_dim, &
                              start_z_dim, end_z_dim, &
                              npts_bdr, &
                              bytes_per_datum, endianness, &
                              sign_convention, istatus
      real, intent(in) :: xlat, xlon
      character (len=128), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx, current_domain
      real :: rx, ry
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               istatus = 0
               exit
            end if
         end if
      end do
  
      if (istatus /= 0) then
         start_x_dim = 1
         start_y_dim = 1
         start_z_dim = 1
         end_x_dim = 1
         end_y_dim = 1
         end_z_dim = 1
         bytes_per_datum = 0
         return
      end if
  
      current_domain = iget_selected_domain()
      call select_domain(SOURCE_PROJ)
      call lltoxy(xlat, xlon, rx, ry, M) 
      call select_domain(current_domain)
  
      start_x_dim = source_tile_x(idx) * nint(real(floor((rx-0.5) / real(source_tile_x(idx))))) + 1
      end_x_dim = start_x_dim + source_tile_x(idx) - 1
  
      start_y_dim = source_tile_y(idx) * nint(real(floor((ry-0.5) / real(source_tile_y(idx))))) + 1
      end_y_dim = start_y_dim + source_tile_y(idx) - 1
  
      if (is_tile_z_start(idx) .and. is_tile_z_end(idx)) then
         start_z_dim = source_tile_z_start(idx)
         end_z_dim = source_tile_z_end(idx)
      else if (is_tile_z(idx)) then
         start_z_dim = 1
         end_z_dim = source_tile_z(idx)
      end if
  
      if (.not. (is_tile_z_start(idx) .and. is_tile_z_end(idx))) then
         if (is_tile_z_start(idx) .or. is_tile_z_end(idx)) then
            call mprintf(.true., ERROR, 'In GEOGRID.TBL, only one of tile_z_start and '// &
                         'tile_z_end specified for entry %i.',i1=idx)
         end if 
      end if
  
      if (is_tile_bdr(idx)) then
         npts_bdr = source_tile_bdr(idx)
      else
         npts_bdr = 0
      end if
  
      start_x_dim = start_x_dim - npts_bdr
      end_x_dim = end_x_dim + npts_bdr
      start_y_dim = start_y_dim - npts_bdr
      end_y_dim = end_y_dim + npts_bdr
  
      if (is_wordsize(idx)) then
         bytes_per_datum = source_wordsize(idx)
      else
         bytes_per_datum = 1
         call mprintf(.true.,ERROR,'In GEOGRID.TBL, no wordsize specified for data in entry %i.',i1=idx)
      end if

      if (is_endian(idx)) then
         endianness = source_endian(idx)
      else
         endianness = BIG_ENDIAN
      end if
  
      if (is_signed(idx)) then
         sign_convention = 1
      else
         sign_convention = 0
      end if
 
   end subroutine get_tile_dimensions
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_tile_fname
   !
   ! Purpose: 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_tile_fname(test_fname, xlat, xlon, ilevel, fieldname, istatus)
 
      use llxy_module
      use gridinfo_module
  
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      real, intent(in) :: xlat, xlon
      character (len=*), intent(in) :: fieldname
      character (len=256), intent(out) :: test_fname
  
      ! Local variables
      integer :: current_domain, idx
      real :: rx, ry
  
      istatus = 1
      write(test_fname, '(a)') 'TILE.OUTSIDE.DOMAIN'

      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldname)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldname))) then
            if (ilevel == source_priority(idx)) then
               istatus = 0
               exit
            end if
         end if
      end do
  
      if (istatus /= 0) return
  
      current_domain = iget_selected_domain()
      call select_domain(SOURCE_PROJ)
      call lltoxy(xlat, xlon, rx, ry, M) 
      call select_domain(current_domain)

!      rx = real(source_tile_x(idx)) * real(floor((rx-0.5*source_dx(idx))/ real(source_tile_x(idx)))) + 1.0
!      ry = real(source_tile_y(idx)) * real(floor((ry-0.5*source_dy(idx))/ real(source_tile_y(idx)))) + 1.0
      rx = real(source_tile_x(idx)) * real(floor((rx-0.5) / real(source_tile_x(idx)))) + 1.0
      ry = real(source_tile_y(idx)) * real(floor((ry-0.5) / real(source_tile_y(idx)))) + 1.0

      if (rx > 0. .and. ry > 0.) then
         select case(source_filename_digits(idx))
            case (5)
               write(test_fname, '(a,i5.5,a1,i5.5,a1,i5.5,a1,i5.5)') trim(source_path(idx)), &
                        nint(rx),'-',nint(rx)+source_tile_x(idx)-1,'.',nint(ry),'-',nint(ry)+source_tile_y(idx)-1
            case (6)
               write(test_fname, '(a,i6.6,a1,i6.6,a1,i6.6,a1,i6.6)') trim(source_path(idx)), &
                        nint(rx),'-',nint(rx)+source_tile_x(idx)-1,'.',nint(ry),'-',nint(ry)+source_tile_y(idx)-1
            case default
               call mprintf(.true., ERROR, 'In GEOGRID.TBL, for index file of data at '// &
                            'entry %i, filename_digits must be either 5 or 6.', i1=idx)
               istatus = 1
               return
         end select
      end if

   end subroutine get_tile_fname
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_source_resolution
   !
   ! Purpose:
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_source_resolution(fieldnm, ilevel, src_dx, src_dy, istatus)

      implicit none

      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
      real, intent(out) :: src_dx, src_dy
      character (len=*), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               if (is_dx(idx) .and. is_dy(idx)) then
                  src_dx = source_dx(idx)
                  src_dy = source_dy(idx)
                  if (source_proj(idx) /= PROJ_LATLON) then
                     src_dx = src_dx / 111000.
                     src_dy = src_dy / 111000.
                  end if
                  istatus = 0
                  exit
               end if 
            end if
         end if
      end do

   end subroutine get_source_resolution


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: check_priority_level
   !
   ! Purpose: Determines whether there exists the specified priority level for
   !   the specified field.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine check_priority_level(fieldnm, ilevel, istatus)
 
      implicit none
  
      ! Arguments
      character (len=*), intent(in) :: fieldnm
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               istatus = 0
            end if
         end if
      end do

   end subroutine check_priority_level


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_data_projection
   !
   ! Purpose: To acquire the parameters necessary in defining the grid on which
   !   the user-specified data for field 'fieldnm' are given.
   ! 
   ! NOTES: If the routine successfully acquires values for all necessary 
   !        parameters, istatus is set to 0. In case of an error, 
   !        OR IF THE USER HAS NOT SPECIFIED A TILE OF DATA FOR FIELDNM,
   !        istatus is set to 1.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_data_projection(fieldnm, iproj, stand_lon, truelat1, truelat2, &
                       dxkm, dykm, known_x, known_y, known_lat, known_lon, ilevel, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(in) :: ilevel
      integer, intent(out) :: iproj, istatus
      real, intent(out) :: stand_lon, truelat1, truelat2, dxkm, dykm, &
                           known_x, known_y, known_lat, known_lon
      character (len=*), intent(in) :: fieldnm
  
      ! Local variables
      integer :: idx
  
      istatus = 1
      do idx=1,num_entries
         if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
             (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
            if (ilevel == source_priority(idx)) then
               istatus = 0
               if (is_proj(idx)) then
                  iproj = source_proj(idx) 
               else
                  iproj = 1 
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for projection in entry %i.',i1=idx)
               end if
               if (is_known_x(idx)) then
                  known_x = source_known_x(idx) 
               else
                  known_x = 1. 
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for known_x in entry %i.',i1=idx)
               end if
               if (is_known_y(idx)) then
                  known_y =  source_known_y(idx)
               else
                  known_y = 1. 
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for known_y in entry %i.',i1=idx)
               end if
               if (is_known_lat(idx)) then
                  known_lat = source_known_lat(idx)
               else
                  known_lat = 1.
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for known_lat in entry %i.',i1=idx)
               end if
               if (is_known_lon(idx)) then
                  known_lon = source_known_lon(idx)
               else
                  known_lon = 1.
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for known_lon in entry %i.',i1=idx)
               end if
               if (is_truelat1(idx)) then
                  truelat1 = source_truelat1(idx)
               else if (is_proj(idx) .and. source_proj(idx) /= PROJ_LATLON) then
                  truelat1 = 1.
                  call mprintf(.true., WARN, &
                               'In GEOGRID.TBL, no specification for truelat1 in entry %i.',i1=idx)
               end if
               if (is_truelat2(idx)) then
                  truelat2 = source_truelat2(idx)
               else if (is_proj(idx) .and. source_proj(idx) /= PROJ_LATLON) then
                  truelat2 = 1.
                  call mprintf(.true., WARN, &
                               'In GEOGRID.TBL, no specification for truelat2 in entry %i.',i1=idx)
               end if
               if (is_stdlon(idx)) then
                  stand_lon = source_stdlon(idx)
               else if (is_proj(idx) .and. source_proj(idx) /= PROJ_LATLON) then
                  stand_lon = 1.
                  call mprintf(.true., WARN, &
                               'In GEOGRID.TBL, no specification for stdlon in entry %i.',i1=idx)
               end if
               if (is_dx(idx)) then
                  dxkm = source_dx(idx)
               else
                  dxkm = 1. 
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for dx in entry %i.',i1=idx)
               end if
               if (is_dy(idx)) then
                  dykm = source_dy(idx)
               else
                  dykm = 1. 
                  call mprintf(.true., ERROR, &
                               'In GEOGRID.TBL, no specification for dy in entry %i.',i1=idx)
               end if
               exit
            end if
         end if
      end do
 
   end subroutine get_data_projection
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   ! Name: get_source_opt_status
   !
   ! Purpose: Determines whether a field is optional and can be skipped
   !
   ! If ilevel is specified as 0, the return value, istatus, will be 0 if
   ! any priority level is available for the field, and 1 otherwise. Otherwise,
   ! istatus will be set to 0 only if the specified priority level for 
   ! the field is available.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
   subroutine get_source_opt_status(fieldnm, ilevel, istatus)

      implicit none

      ! Arguments
      character (len=*), intent(in) :: fieldnm
      integer, intent(in) :: ilevel
      integer, intent(out) :: istatus
  
      ! Local variables
      integer :: idx

  
      ! Any priority level is available
      if (ilevel == 0) then
         istatus = 1
         do idx=1,num_entries
            if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
                (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
               if (.not. is_not_found(idx)) then
                  istatus = 0
                  exit
               end if 
            end if
         end do

      ! Only the specified level is to be checked
      else
         istatus = 0
         do idx=1,num_entries
            if ((index(source_fieldname(idx),trim(fieldnm)) /= 0) .and. &
                (len_trim(source_fieldname(idx)) == len_trim(fieldnm))) then
               if (ilevel == source_priority(idx)) then
                  if (is_not_found(idx)) then
                     istatus = 1
                     exit
                  end if 
               end if
            end if
         end do
      end if

   end subroutine get_source_opt_status
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Name: check_data_specification
   !
   ! Purpose: To check for obvious errors in the user source data specifications.
   !   Returns .true. if specification passes all checks, and .false. otherwise.
   !   For failing checks, diagnostic messages are printed.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   function check_data_specification( )
  
      implicit none
  
      ! Return value
      logical :: check_data_specification
  
      ! Local variables
      integer :: i, j, istatus
      integer, pointer, dimension(:) :: priorities
      real :: rmissing
      logical :: begin_priority, halt
      character (len=128) :: cur_name
  
      check_data_specification = .false.
  
      ! Check that each specification has a name, priority level, and path
      do i=1,num_entries
         if (.not. is_fieldname(i)) then
            call mprintf(.true., ERROR, &
                         'In GEOGRID.TBL, specification %i does not have a name.',i1=i)
         end if
         if (.not. is_priority(i)) then
            call mprintf(.true., ERROR, &
                         'In GEOGRID.TBL, specification %i does not have a priority.',i1=i)
         end if
         if (list_length(source_res_path(i)) == 0) then
            call mprintf(.true., ERROR, &
                         'In GEOGRID.TBL, no path (relative or absolute) is specified '// &
                         'for entry %i.',i1=i)
         end if
      end do
  
      ! The fill_missing and halt_on_missing options are mutually exclusive
      do i=1,num_entries
         call get_halt_on_missing(source_fieldname(i), halt, istatus) 
         call get_missing_fill_value(source_fieldname(i), rmissing, istatus)
         if (halt .and. (istatus == 0)) then
            call mprintf(.true., ERROR, 'In GEOGRID.TBL, the halt_on_missing and fill_missing '// &
                         'options are mutually exclusive, but both are given for field %s', &
                         s1=trim(source_fieldname(i)))
         end if
      end do
  
      ! Check that the field from which landmask is calculated is not output on a staggering
      do i=1,num_entries
         if (list_length(source_landmask_land(i)) > 0 .or. list_length(source_landmask_water(i)) > 0) then
            if (is_output_stagger(i)) then
               if (source_output_stagger(i) /= M) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, landmask cannot be derived from '// &
                               'a field that is computed on a staggered grid at entry %i.',i1=i)
               end if
            end if
         end if
      end do
  
      ! Also check that any field that is to be masked by the landmask is not output on a staggering
      do i=1,num_entries
         if (is_masked(i) .and. is_output_stagger(i)) then
            if (source_output_stagger(i) /= M) then
               call mprintf(.true., ERROR, 'In GEOGRID.TBL, landmask cannot be used with '// &
                            'a field that is computed on a staggered grid at entry %i.',i1=i)
            end if
         end if
      end do
  
      allocate(priorities(num_entries))
  
      ! Now check that priorities for each source are unique and in the interval [1,n], n <= num_entries 
      do i=1,num_entries
         priorities = 0
         cur_name = source_fieldname(i)
         do j=1,num_entries
            if (source_fieldname(j) == cur_name) then
    
               if (source_priority(j) > num_entries .or. source_priority(j) < 1) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, priorities for %s do not '// &
                               'form a sequence 1,2,...,n.',  s1=trim(cur_name))
     
               else 
                  if (priorities(source_priority(j)) == 1) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, more than one entry for %s '// &
                                  'has priority %i.', s1=trim(cur_name), i1=source_priority(j))
                
                  else
                     priorities(source_priority(j)) = 1
                  end if
               end if
    
            end if
         end do
   
         begin_priority = .false.
         do j=num_entries,1,-1
            if (.not.begin_priority .and. priorities(j) == 1) then
               begin_priority = .true. 
            else if (begin_priority .and. priorities(j) == 0) then
               call mprintf(.true., ERROR, 'In GEOGRID.TBL, no entry for %s has '// &
                            'priority %i, but an entry has priority %i.', &
                            s1=trim(cur_name), i1=j, i2=j+1)
            end if
         end do
      end do
  
      deallocate(priorities)
  
      ! Units must match for all priority levels of a field
      do i=1,num_entries
         if (source_priority(i) == 1) then
            do j=1,num_entries
               if ((source_fieldname(i) == source_fieldname(j)) .and. &
                   (source_units(i) /= source_units(j))) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, units for %s at entry %i '// &
                               'do not match units at entry %i (%s)', &
                               s1=trim(source_fieldname(i)), i1=j, i2=i, s2=trim(source_units(i)))
               end if
            end do
         end if
      end do
  
      ! Make sure that user has not asked to calculate landmask from a continuous field
      do i=1,num_entries
         if (is_dest_fieldtype(i)) then
            if (source_dest_fieldtype(i) == CONTINUOUS) then
               if (list_length(source_landmask_water(i)) > 0 .or. list_length(source_landmask_land(i)) > 0) then
                  call mprintf(.true., ERROR, 'In GEOGRID.TBL, landmask cannot be '// &
                               'calculated from a continuous destination field at entry %i.',i1=i)
               end if
            end if
         end if
      end do
  
      ! If either min_category or max_category is specified, then both must be specified
      do i=1,num_entries
         if (is_category_min(i) .or. is_category_max(i)) then
            if (.not. is_category_min(i)) then
               call mprintf(.true., ERROR, 'In GEOGRID.TBL, for index file of data at '// &
                            'entry %i, category_max is specified, but category_min is '// &
                            'not. Both must be specified.',i1=i)
            else if (.not. is_category_max(i)) then
               call mprintf(.true., ERROR, 'In GEOGRID.TBL, for index file of data at '// &
                            'entry %i, category_min is specified, but category_max is '// &
                            'not. Both must be specified.',i1=i)
            end if
         end if
      end do
  
      ! For continuous data, (category_max - category_min + 1) should equal tile_z
      do i=1,num_entries
         if (is_fieldtype(i)) then
            if (source_fieldtype(i) == CONTINUOUS) then
               if (is_category_max(i) .and. is_category_min(i) .and. is_tile_z(i)) then
                  if (source_tile_z(i) /= (source_category_max(i) - source_category_min(i) + 1)) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, tile_z must equal '// &
                                  '(category_max - category_min + 1) at entry %i.',i1=i)
                  end if
               else if (is_category_max(i) .and. is_category_min(i) .and. &
                        is_tile_z_start(i) .and. is_tile_z_end(i)) then
                  if (source_tile_z_end(i) /= source_category_max(i) .or. &
                      source_tile_z_start(i) /= source_category_min(i)) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, tile_z_end must equal '// &
                                  'category_max, and tile_z_start must equal category_min '// &
                                  'at entry %i.',i1=i)
                  end if
               end if
            end if
         end if
      end do
  
      ! Make sure that user has not named a dominant category or computed slope field 
      !    the same as a fractional field
      do i=1,num_entries
         if (source_dominant_category(i) == source_fieldname(i)) then
           call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant category cannot have '// &
                        'the same name as the field at entry %i.',i1=i)
         end if
   
         do j=1,num_entries
            if (.not. is_dominant_only(i)) then
               if (is_dfdx(j)) then
                  if (source_dfdx(j) == source_fieldname(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, field name at entry %i '// &
                                  'cannot have the same name as the slope field df_dx at entry %i.', &
                                  i1=i, i2=j)
                  end if
               end if
               if (is_dfdy(j)) then
                  if (source_dfdy(j) == source_fieldname(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, field name at entry %i '// &
                                  'cannot have the same name as the slope field df_dy at entry %i.', &
                                  i1=i, i2=j)
                  end if
               end if
               if (is_dfdx(j) .and. is_dominant_category(i)) then
                  if (source_dfdx(j) == source_dominant_category(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant field name at '// &
                                  'entry %i cannot have the same name as the slope field df_dx '// &
                                  'at entry %i.',i1=i, i2=j)
                  end if
               end if
               if (is_dfdy(j) .and. is_dominant_category(i)) then
                  if (source_dfdy(j) == source_dominant_category(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant field name at '// &
                                  'entry %i cannot have the same name as the slope field '// &
                                  'df_dy at entry %i.',i1=i, i2=j)
                  end if
               end if
            else
               if (is_dfdx(j)) then
                  if (source_dfdx(j) == source_dominant_only(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant field name at '// &
                                  'entry %i cannot have the same name as the slope field '// &
                                  'df_dx at entry %i.',i1=i, i2=j)
                  end if
               end if
               if (is_dfdy(j)) then
                  if (source_dfdy(j) == source_dominant_only(i)) then 
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant field name at '// &
                                  'entry %i cannot have the same name as the slope field '// &
                                  'df_dy at entry %i.',i1=i, i2=j)
                  end if
               end if
            end if
            if (i /= j) then
               if (is_dfdx(i)) then
                  if (is_dfdx(j)) then
                     if (source_dfdx(j) == source_dfdx(i)) then 
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, slope field df_dx at '// &
                                     'entry %i cannot have the same name as the slope '// &
                                     'field df_dx at entry %i.',i1=i, i2=j)
                     end if
                  end if
                  if (is_dfdy(j)) then
                     if (source_dfdy(j) == source_dfdx(i)) then 
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, slope field df_dx at '// &
                                     'entry %i cannot have the same name as the slope field '// &
                                     'df_dy at entry %i.',i1=i, i2=j)
                     end if
                  end if
               end if
               if (is_dfdy(i)) then
                  if (is_dfdx(j)) then
                     if (source_dfdx(j) == source_dfdy(i)) then 
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, slope field df_dy at '// &
                                     'entry %i cannot have the same name as the slope field '// &
                                     'df_dx at entry %i.',i1=i, i2=j)
                     end if
                  end if
                  if (is_dfdy(j)) then
                     if (source_dfdy(j) == source_dfdy(i)) then 
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, slope field df_dy at '// &
                                     'entry %i cannot have the same name as the slope field '// &
                                     'df_dy at entry %i.',i1=i, i2=j)
                     end if
                  end if
               end if
               if (is_dominant_category(i)) then
                  if (source_dominant_category(i) == source_fieldname(j)) then ! Possible exception
                     if (.not. (is_dominant_only(j) .and. (source_dominant_category(i) /= source_dominant_only(j)))) then
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant category at '// &
                                     'entry %i cannot have the same name as the field at '// &
                                     'entry %i.',i1=i, i2=j)
                     end if
                  else if (is_dominant_category(j) .and. &
                           (source_dominant_category(i) == source_dominant_category(j))) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant category at entry '// &
                                  '%i cannot have the same name as dominant category at '// &
                                  'entry %i.',i1=i, i2=j)
                  else if (is_dominant_only(j) .and. &
                           (source_dominant_category(i) == source_dominant_only(j))) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant category at '// &
                                  'entry %i cannot have the same name as dominant_only '// &
                                  'category at entry %i.',i1=i, i2=j)
                  end if
               else if (is_dominant_only(i)) then
                  if (source_dominant_only(i) == source_fieldname(j)) then ! Possible exception
                     if (.not. (is_dominant_only(j) .and. (source_dominant_only(i) /= source_dominant_only(j)))) then
                        call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant_only category '// &
                                     'at entry %i cannot have the same name as the field at '// &
                                     'entry %i.',i1=i, i2=j)
                     end if
                  else if (is_dominant_category(j) .and. &
                           (source_dominant_only(i) == source_dominant_category(j))) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant_only category '// &
                                  'at entry %i cannot have the same name as dominant '// &
                                  'category at entry %i.',i1=i, i2=j)
                  else if (is_dominant_only(j) .and. &
                           (source_dominant_only(i) == source_dominant_only(j))) then
                     call mprintf(.true., ERROR, 'In GEOGRID.TBL, dominant_only category '// &
                                  'at entry %i cannot have the same name as dominant_only '// &
                                  'category at entry %i.',i1=i, i2=j)
                  end if
               end if
            end if
         end do
      end do
  
      check_data_specification = .true.

   end function check_data_specification
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: s_len
   !
   ! Purpose: This routine receives a fortran string, and returns the number of 
   !    characters in the string before the first "space" is encountered. It 
   !    considers ascii characters 33 to 126 to be valid characters, and ascii 
   !    0 to 32, and 127 to be "space" characters.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine s_len(string, s_length)
 
      implicit none
  
      ! Arguments
      character (len=*), intent(in) :: string
      integer, intent(out) :: s_length
  
      ! Local variables
      integer :: i, len_str, aval
      logical :: space
  
      space = .false.
      i = 1
      len_str = len(string)
      s_length = len_str     
      do while ((i .le. len_str) .and. (.not. space))
         aval = ichar(string(i:i))
         if ((aval .lt. 33) .or. (aval .gt. 126)) then
            s_length = i - 1
            space = .true.
         endif
         i = i + 1
      enddo
 
   end subroutine s_len


   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: display_optional_field_msgs
   !
   ! Purpose: This routine prints out information regarding any optional fields
   !    in the GEOGRID.TBL file that were not found at run-time and were
   !    skipped.
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine display_optional_field_msgs()

      implicit none

      integer :: idx
      logical :: do_prints


      do_prints = .false.

      do idx=1,num_entries
         if (is_optional(idx) .and. is_not_found(idx)) then
            do_prints = .true.
            exit
         end if 
      end do

      if (do_prints) then
         call mprintf(.true., STDOUT, ' ')
         call mprintf(.true., STDOUT, '  Optional fields not processed by geogrid:')
         call mprintf(.true., LOGFILE, 'Optional fields not processed by geogrid:')

         do idx=1,num_entries
            if (is_optional(idx) .and. is_not_found(idx)) then
               call mprintf(.true., STDOUT, '    %s (priority=%i, resolution=''%s'', path=''%s'')', s1=source_fieldname(idx), &
                            i1=source_priority(idx), s2=source_res(idx), s3=source_path(idx))
               call mprintf(.true., LOGFILE, '    %s (priority=%i, resolution=''%s'', path=''%s'')', s1=source_fieldname(idx), &
                            i1=source_priority(idx), s2=source_res(idx), s3=source_path(idx))
            end if 
         end do

         call mprintf(.true., STDOUT, ' ')
      end if

   end subroutine display_optional_field_msgs
 
end module source_data_module
