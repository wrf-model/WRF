module write_met_module

   use module_debug
   use misc_definitions_module
   use met_data_module

   ! State variables?
   integer :: output_unit
   character (len=MAX_FILENAME_LEN) :: met_out_filename
 
   contains

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: write_met_init
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine write_met_init(fg_source, source_is_constant, datestr, istatus)
 
      implicit none
  
      ! Arguments
      integer, intent(out) :: istatus
      logical, intent(in) :: source_is_constant
      character (len=*), intent(in) :: fg_source
      character (len=*), intent(in) :: datestr
  
      ! Local variables
      integer :: io_status
      logical :: is_used

      istatus = 0
    
      !  1) BUILD FILENAME BASED ON TIME 
      met_out_filename = ' '
      if (.not. source_is_constant) then 
         write(met_out_filename, '(a)') trim(fg_source)//':'//trim(datestr)
      else
         write(met_out_filename, '(a)') trim(fg_source)
      end if
  
      !  2) OPEN FILE
      do output_unit=10,100
         inquire(unit=output_unit, opened=is_used)
         if (.not. is_used) exit
      end do 
      call mprintf((output_unit > 100),ERROR,'In write_met_init(), couldn''t find an available Fortran unit.')
      open(unit=output_unit, file=trim(met_out_filename), status='unknown', form='unformatted', iostat=io_status)

      if (io_status > 0) istatus = 1

      return
  
 
   end subroutine write_met_init
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: write_next_met_field
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine write_next_met_field(fg_data, istatus)
 
      implicit none
  
      ! Arguments
      type (met_data), intent(in) :: fg_data
      integer, intent(out) :: istatus
  
      ! Local variables
      character (len=8) :: startloc
      character (len=9) :: local_field
  
      istatus = 1
  
      !  1) WRITE FORMAT VERSION
      write(unit=output_unit) fg_data % version

      local_field = fg_data % field
      if (local_field == 'GHT      ') local_field = 'HGT      '

      ! PREGRID
      if (fg_data % version == 3) then

         ! Cylindrical equidistant
         if (fg_data % iproj == PROJ_LATLON) then
            write(unit=output_unit) fg_data % hdate, &
                                    fg_data % xfcst, &
                                    local_field,     &
                                    fg_data % units, &
                                    fg_data % desc,  &
                                    fg_data % xlvl,  &
                                    fg_data % nx,    &
                                    fg_data % ny,    &
                                    0
            write(unit=output_unit) fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % deltalat, &
                                    fg_data % deltalon
     
         ! Mercator
         else if (fg_data % iproj == PROJ_MERC) then
            write(unit=output_unit) fg_data % hdate, &
                                    fg_data % xfcst, &
                                    local_field,     &
                                    fg_data % units, &
                                    fg_data % desc,  &
                                    fg_data % xlvl,  &
                                    fg_data % nx,    &
                                    fg_data % ny,    &
                                    1
            write(unit=output_unit) fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % truelat1
     
         ! Lambert conformal
         else if (fg_data % iproj == PROJ_LC) then
            write(unit=output_unit) fg_data % hdate, &
                                    fg_data % xfcst, &
                                    local_field,     &
                                    fg_data % units, &
                                    fg_data % desc,  &
                                    fg_data % xlvl,  &
                                    fg_data % nx,    &
                                    fg_data % ny,    &
                                    3
            write(unit=output_unit) fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1, &
                                    fg_data % truelat2
     
         ! Polar stereographic
         else if (fg_data % iproj == PROJ_PS) then
            write(unit=output_unit) fg_data % hdate, &
                                    fg_data % xfcst, &
                                    local_field,     &
                                    fg_data % units, &
                                    fg_data % desc,  &
                                    fg_data % xlvl,  &
                                    fg_data % nx,    &
                                    fg_data % ny,    &
                                    5
            write(unit=output_unit) fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1

         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', &
                         i1=fg_data % iproj,s1=met_out_filename)
     
         end if
     
         write(unit=output_unit) fg_data % slab
     
         istatus = 0 
    
      ! GRIB_PREP
      else if (fg_data % version == 4) then

         if (fg_data % starti == 1.0 .and. fg_data % startj == 1.0) then
            startloc='SWCORNER'
         else
            startloc='CENTER  '
         end if

         ! Cylindrical equidistant
         if (fg_data % iproj == PROJ_LATLON) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    0
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % deltalat, &
                                    fg_data % deltalon

         ! Mercator
         else if (fg_data % iproj == PROJ_MERC) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    1
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % truelat1

         ! Lambert conformal
         else if (fg_data % iproj == PROJ_LC) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    3
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1, &
                                    fg_data % truelat2

         ! Polar stereographic
         else if (fg_data % iproj == PROJ_PS) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    5
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', &
                         i1=fg_data % iproj,s1=met_out_filename)
     
         end if
  
         write(unit=output_unit) fg_data % slab
      
         istatus = 0

      ! WPS
      else if (fg_data % version == 5) then

         if (fg_data % starti == 1.0 .and. fg_data % startj == 1.0) then
            startloc='SWCORNER'
         else
            startloc='CENTER  '
         end if

         ! Cylindrical equidistant
         if (fg_data % iproj == PROJ_LATLON) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    0
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % deltalat, &
                                    fg_data % deltalon, &
                                    fg_data % earth_radius

         ! Mercator
         else if (fg_data % iproj == PROJ_MERC) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    1
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % truelat1, &
                                    fg_data % earth_radius

         ! Lambert conformal
         else if (fg_data % iproj == PROJ_LC) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    3
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1, &
                                    fg_data % truelat2, &
                                    fg_data % earth_radius

         ! Gaussian
         else if (fg_data % iproj == PROJ_GAUSS) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    4
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % deltalat, &
                                    fg_data % deltalon, &
                                    fg_data % earth_radius

         ! Polar stereographic
         else if (fg_data % iproj == PROJ_PS) then
            write(unit=output_unit) fg_data % hdate,      &
                                    fg_data % xfcst,      &
                                    fg_data % map_source, &
                                    local_field,          &
                                    fg_data % units,      &
                                    fg_data % desc,       &
                                    fg_data % xlvl,       &
                                    fg_data % nx,         &
                                    fg_data % ny,         &
                                    5
            write(unit=output_unit) startloc, &
                                    fg_data % startlat, &
                                    fg_data % startlon, &
                                    fg_data % dx,       &
                                    fg_data % dy,       &
                                    fg_data % xlonc,    &
                                    fg_data % truelat1, &
                                    fg_data % earth_radius
     
         ! ?????????
         else
            call mprintf(.true.,ERROR,'Unrecognized projection code %i when reading from %s.', &
                         i1=fg_data % iproj,s1=met_out_filename)
     
         end if
  
         write(unit=output_unit) fg_data % is_wind_grid_rel

         write(unit=output_unit) fg_data % slab
      
         istatus = 0

      else
         call mprintf(.true.,ERROR,'Didn''t recognize format number %i.', i1=fg_data % version)
      end if
  
      return
 
   end subroutine write_next_met_field
 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Name: write_met_close
   !
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   subroutine write_met_close()
 
      implicit none
  
      close(unit=output_unit)
      met_out_filename = 'UNINITIALIZED_FILENAME'
  
   end subroutine write_met_close

end module write_met_module
