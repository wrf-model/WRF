program height_ukmo

   !  This program computes the 3d height field for the UKMO data.  The heights
   !  are constant in time, and are a function only of the topography and a few
   !  constants.  There are several UKMO data sets available, denoted by the
   !  different numbers of vertical levels.  An input file defines the required
   !  information for each of these data sets.

   use date_pack
   use gridinfo_module
   use read_met_module
   use write_met_module
   use misc_definitions_module
   use module_debug

   implicit none

   ! Local variables
   integer :: t, istatus
   character (len=19) :: valid_date, temp_date, output_date
   character (len=128) :: input_name
   type (met_data) :: temp_data, soilhgt_data, height_data

   integer :: model_levels , first_constant_r_rho_level , k_loop , j_loop , i_loop , temp_count , date_loop
   real :: z_top_of_model , etac
   real, dimension(:) , allocatable :: eta
   real , dimension(:,:) , allocatable :: soil_hold
   logical :: still_more_dates_to_process

   call get_namelist_params()

   call set_debug_level(WARN)

   input_name = fg_name(1)

   call geth_newdate(valid_date, trim(start_date(1)), 0 )
   temp_date = ' '
   output_date = ' '
   write(temp_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)
   write(output_date,'(a19)') valid_date(1:10)//'_'//valid_date(12:19)

   !  Do this twice.  The first time is to find out how many levels of
   !  3d fields there are.  Why?  The data from UKMO has *some* of the 
   !  levels, just not all.  So we can compute the height field, but we
   !  are not allowed to just spit out 70 levels.

   ! Initialize the module for reading in the met fields
   call read_met_init(trim(input_name), .false., temp_date(1:13), istatus)

   if (istatus == 0) then
      call mprintf(.true.,STDOUT,'Reading from %s at time %s', s1=input_name, s2=temp_date(1:13))

      ! Process all fields and levels from the current file; read_next_met_field()
      !   will return a non-zero status when there are no more fields to be read.
      temp_count = 0
      just_temp : do while (istatus == 0)

         call read_next_met_field(temp_data, istatus)

         if (istatus == 0) then

            if (trim(temp_data%field) == 'TT' .and. temp_data%xlvl /= 200100.) then
               temp_count = temp_count + 1
            else if (trim(temp_data%field) == 'SOILHGT' .and. temp_data%xlvl == 200100.) then
               allocate ( soil_hold(temp_data%nx,temp_data%ny) )
               do j_loop = 1 , temp_data%ny
                  do i_loop = 1 , temp_data%nx
                     soil_hold(i_loop,j_loop) = temp_data%slab(i_loop,j_loop)
                  end do
               end do
            end if
   
            deallocate ( temp_data%slab )
         end if
   
      end do just_temp
      call mprintf(.true.,STDOUT,'Total number of TT levels, excluding 200100 = %i' , i1=temp_count)
   
      call read_met_close()

   else
      call mprintf(.true.,ERROR,'Problem opening %s at time %s', s1=input_name, s2=temp_date(1:13))
   end if

   !  Now we are doing this the second time.  We know how many temp levels there are, temp_count,
   !  so we will use the same number for height.

   ! Initialize the module for reading in the met fields - again!
   call read_met_init(trim(input_name), .false., temp_date(1:13), istatus)

   if (istatus == 0) then
      call mprintf(.true.,STDOUT,'Reading from %s at time %s', s1=input_name, s2=temp_date(1:13))

      ! Process all fields and levels from the current file; read_next_met_field()
      !   will return a non-zero status when there are no more fields to be read.
      all_fields : do while (istatus == 0)


         if ( associated ( soilhgt_data%slab ) ) then
            deallocate ( soilhgt_data%slab )
         end if
         call read_next_met_field(soilhgt_data, istatus)

         if (istatus == 0) then

            if (trim(soilhgt_data%field) == 'SOILHGT  ' .and. soilhgt_data%xlvl == 200100.) then
               if (.not. associated(height_data%slab)) then
                  allocate(height_data%slab(soilhgt_data%nx,soilhgt_data%ny))
               end if
               height_data = soilhgt_data
               height_data%hdate = '0000-00-00_00:00:00     '
               height_data%xfcst = 0.
               height_data%field = 'HGT      '
               height_data%slab = 0.

!print *,'height_data%nx ,soilhgt_data%nx = ',height_data%nx ,soilhgt_data%nx
!print *,'height_data%ny ,soilhgt_data%ny = ',height_data%ny ,soilhgt_data%ny

               if (height_data%nx    /= soilhgt_data%nx .or. &
                   height_data%ny    /= soilhgt_data%ny .or. &
                   height_data%iproj /= soilhgt_data%iproj) then
                  call mprintf(.true.,ERROR,'Mismatch in height field dimensions in file %s', &
                               s1=trim(input_name)//':'//temp_date(1:13))
               end if
                
               open ( 10 , &
                      file = 'util/vertical_grid_70_20m_80km.txt' , &
                      form = 'formatted' , &
                      access = 'sequential' , &
                      status = 'old' , &
                      iostat = istatus ) 

               if ( istatus /= 0 ) then
                  call mprintf(.true.,ERROR,'Cannot open the UKMO file util/vertical_grid_70_20m_80km.txt')
               end if

               read (10 , * ) 
               read (10 , * ) 
               read (10 , * ) 
               read (10 , fmt='(30x,i2)' ) model_levels
               read (10 , fmt='(30x,i2)' ) first_constant_r_rho_level
               read (10 , fmt='(30x,f17.11)' ) z_top_of_model
print *,'model_levels, first_constant_r_rho_level, z_top_of_model = ', model_levels, first_constant_r_rho_level, z_top_of_model
               read (10 , * ) 
               read (10 , * ) 
 
               allocate ( eta ( model_levels ) )
               eta = 0.
               do k_loop = 1 , first_constant_r_rho_level-1
                  read ( 10 , fmt='(65x,f10.7)' ) eta(k_loop)
!print *,k_loop,' eta = ',eta(k_loop)
               end do
               k_loop = first_constant_r_rho_level
               read ( 10 , fmt='(29x,f10.7,26x,f10.7)' ) etac , eta(k_loop)
               do k_loop = first_constant_r_rho_level+1 , model_levels
                  read ( 10 , fmt='(65x,f10.7)' ) eta(k_loop)
!print *,k_loop,' eta = ',eta(k_loop)
               end do

               exit all_fields
            end if
   
   
         end if
   
      end do all_fields
   
      call read_met_close()

   else
      call mprintf(.true.,ERROR,'Problem opening %s at time %s', s1=input_name, s2=temp_date(1:13))
   end if

   !  Now we have to write the height out for each time.

   if (associated(height_data%slab)) then

      still_more_dates_to_process = .true.
      soilhgt_data%xfcst=  0.
      soilhgt_data%hdate=  output_date // '.0000'
      height_data%xfcst=  0.
      height_data%hdate=  output_date // '.0000'
      date_loop = 1
    
      all_dates : do while ( still_more_dates_to_process )

print *,'Generating ',temp_count,' height levels for ',output_date,'.'

         call write_met_init('HGT', .false., output_date(1:13), istatus)
         soilhgt_data%xlvl = 200100
         soilhgt_data%field = 'SOILHGT  '
         soilhgt_data%desc = 'Computed Geopotential Height, UKMO diagnostic '

         !  Corrections for UKMO

!     soilhgt_data%deltalon=  360. / real(soilhgt_data%nx)

         do j_loop = 1 , soilhgt_data%ny
            do i_loop = 1 , soilhgt_data%nx
               soilhgt_data%slab(i_loop,j_loop) = soil_hold(i_loop,j_loop)
            end do
         end do

         call write_next_met_field(soilhgt_data, istatus) 

         height_data%version=  soilhgt_data%version
         height_data%nx=  soilhgt_data%nx
         height_data%ny=  soilhgt_data%ny
         height_data%iproj=  soilhgt_data%iproj
!        height_data%xfcst=  soilhgt_data%xfcst
!        height_data%xlvl=  soilhgt_data%xlvl
         height_data%startlat=  soilhgt_data%startlat
         height_data%startlon=  soilhgt_data%startlon
         height_data%starti=  soilhgt_data%starti
         height_data%startj=  soilhgt_data%startj
         height_data%deltalat=  soilhgt_data%deltalat
         height_data%deltalon=  soilhgt_data%deltalon
         height_data%dx=  soilhgt_data%dx
         height_data%dy=  soilhgt_data%dy
         height_data%xlonc=  soilhgt_data%xlonc
         height_data%truelat1=  soilhgt_data%truelat1
         height_data%truelat2=  soilhgt_data%truelat2
         height_data%earth_radius=  soilhgt_data%earth_radius
         height_data%is_wind_grid_rel=  soilhgt_data%is_wind_grid_rel
         height_data%field=  'HGT      '
!        height_data%hdate=  soilhgt_data%hdate
         height_data%units=  soilhgt_data%units
         height_data%map_source=  soilhgt_data%map_source
         height_data%desc=  soilhgt_data%desc

         do k_loop = MIN(model_levels,temp_count) , first_constant_r_rho_level+1 , -1
!        height_data%xlvl = temp_count + 1 - k_loop
            height_data%xlvl =                  k_loop
            do j_loop = 1 , height_data%ny
               do i_loop = 1 , height_data%nx
                  height_data%slab(i_loop,j_loop) = eta(k_loop) * z_top_of_model
               end do
            end do

            call write_next_met_field(height_data, istatus) 
         end do

         do k_loop = first_constant_r_rho_level , 1 , -1
!        height_data%xlvl = temp_count + 1 - k_loop
            height_data%xlvl =                  k_loop
            do j_loop = 1 , height_data%ny
               do i_loop = 1 , height_data%nx
                  height_data%slab(i_loop,j_loop) = eta(k_loop) * z_top_of_model + &
                                          soil_hold(i_loop,j_loop) * ( 1. - eta(k_loop) / etac ) **2
               end do
            end do

            call write_next_met_field(height_data, istatus) 
         end do
     
         call write_met_close()

         call geth_newdate ( output_date , valid_date , interval_seconds * date_loop )
         date_loop = date_loop + 1

         if ( TRIM(output_date) > TRIM(end_date(1)) ) then
            still_more_dates_to_process = .false.
         end if

      end do all_dates

   end if


   call mprintf(.true.,STDOUT,' *** Successful completion of program height_ukmo.exe *** ')

   stop

end program height_ukmo
