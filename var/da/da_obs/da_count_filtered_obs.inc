subroutine da_count_filtered_obs (ptop, map, ds, phic, xlonc, truelat1, truelat2, &
   coarse_ix, coarse_jy, start_x, start_y)

   !---------------------------------------------------------------------------
   ! Purpose: Scans intermediate Filtered Obs file, 
   !          counts various obs type and writes on filtered_obs_unit
   !---------------------------------------------------------------------------

   implicit none

   real,              intent(in) :: ptop, ds
   real,              intent(in) :: phic, xlonc, truelat1, truelat2
   integer,           intent(in) :: coarse_ix, coarse_jy
   real,              intent(in) :: start_x, start_y
   integer,           intent(in) :: map

   integer                      :: i, iost, fm
   type (multi_level_type)      :: platform
   real                         :: height_error
   integer                      :: nlocal(num_ob_indexes)
   integer                      :: num_others

   integer                        :: iunit, files, total_obs     
   integer                        :: maxnes, numc, idd
   character(len=filename_len)    :: filename


   if (trace_use) call da_trace_entry("da_count_filtered_obs")

   nlocal(:) = 0
   num_others = 0

   call da_get_unit(iunit)

   ! Loop over all data files
   do files = 0, num_procs-1
      write(unit=filename, fmt='(a,i4.4)') 'filtered_obs.',files  
      open(unit=iunit, file= trim(filename), form='formatted',iostat=iost)
      if (iost /= 0) call da_error(__FILE__,__LINE__, (/"Cannot open "//filename/))

      !  loop over records

      reports: do
         ! read station general info

         read (iunit, fmt = fmt_info, iostat = iost) &
            platform%info%platform,    &
            platform%info%date_char,   &
            platform%info%name,        &
            platform%info%levels,      &
            platform%info%lat,         &
            platform%info%lon,         &
            platform%info%elv,         &
            platform%info%id

         if (iost /= 0) then
            !write (0,'(/A,I9)') ' end OF OBS unit: ',iunit
            !write (0,'(/A,I9)') ' iostat:          ',iost
            exit reports
         end if

         ! Read surface Info

         read (iunit, fmt = fmt_srfc)  &
            platform%loc%slp%inv, platform%loc%slp%qc, &
            platform%loc%slp%error,                    &
            platform%loc%pw%inv, platform%loc%pw%qc,   &
            platform%loc%pw%error
         ! levels < 1 and not GPSPW, go back to reports

         if ((platform%info%levels < 1) .AND.            &
             (index(platform%info%platform, 'GPSPW') <= 0)) then
              cycle reports
         end if
         read(platform%info%platform(4:6), '(I3)') fm

         ! read each level
         do i = 1, platform%info%levels
            platform%each (i) = each_level_type(missing_r, missing, -1.0, & ! height
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! u
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! v
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! p
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! t
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! q
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! rh
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r), & ! td
               field_type(missing_r, missing_data, missing_r, missing_r, missing_r))  ! speed 

            read (unit = iunit, fmt = trim (fmt_each)) &
               platform%each(i)%p%inv, platform%each(i)%p%qc, platform%each(i)%p%error, &
               platform%each(i)%speed%inv, platform%each(i)%speed%qc,                   &
               platform%each(i)%speed%error,                                            &
               platform%each(i)%v%inv, platform%each(i)%v%qc, platform%each(i)%v%error, &
               platform%each(i)%height,                                                 &
               platform%each(i)%height_qc,                                              &
               height_error,                                                            &
               platform%each(i)%t%inv, platform%each(i)%t%qc, platform%each(i)%t%error, &
               platform%each(i)%td%inv, platform%each(i)%td%qc, platform%each(i)%td%error, &
               platform%each(i)%rh%inv, platform%each(i)%rh%qc, platform%each(i)%rh%error
         end do

         if (platform%info%levels < 1) then
            if (fm /= 111) then
               cycle reports
            end if
         end if
         select case(fm)

         case (12) ;

            if (.not.use_synopobs) cycle reports
            nlocal(synop) = nlocal(synop) + 1

         case (13, 17) ;                  ! ships          

            if (.not.use_shipsobs) cycle reports
            nlocal(ships)  = nlocal(ships)  + 1

         case (15:16) ;

            if (.not.use_metarobs) cycle reports
            nlocal(metar) = nlocal(metar) + 1

         case (32:34) ;

            if (.not.use_pilotobs) cycle reports
            nlocal(pilot) = nlocal(pilot) + 1

         case (35:38) ;
            if (.not.use_soundobs) cycle reports
            nlocal(sound) = nlocal(sound) + 1
            nlocal(sonde_sfc) = nlocal(sonde_sfc) + 1

         case (161) ;
            if (.not.use_mtgirsobs) cycle reports
            nlocal(mtgirs) = nlocal(mtgirs) + 1

         case (86) ;

            if (.not.use_satemobs) cycle reports

            ! Reject cloudy satem obs.
 
            if (platform%loc%pw%inv > 10.0) then
               cycle reports
            end if

            nlocal(satem) = nlocal(satem) + 1

         case (88)    ;
            ! Geostationary or Polar orbitting Satellite AMVs:

            if (index(platform%info%name, 'MODIS') > 0 .or. &
                index(platform%info%name, 'modis') > 0 .or. &
                index(platform%info%id, 'AVHRR') > 0)  then
               if (.not.use_polaramvobs) cycle reports
               nlocal(polaramv) = nlocal(polaramv) + 1
            else
               if (.not.use_geoamvobs) cycle reports 
               nlocal(geoamv) = nlocal(geoamv) + 1
            end if

         case (42,96:97) ;

            if (.not.use_airepobs) cycle reports
            nlocal(airep) = nlocal(airep) + 1

         case (101) ;
            if (.not.use_tamdarobs) cycle reports
            nlocal(tamdar) = nlocal(tamdar) + 1
            nlocal(tamdar_sfc) = nlocal(tamdar_sfc) + 1

         case (111) ;
         
            if (.not.use_gpspwobs) cycle reports
            nlocal(gpspw) = nlocal(gpspw) + 1

         case (116) ;
         
            if (.not.use_gpsrefobs) cycle reports
            nlocal(gpsref) = nlocal(gpsref) + 1

         case (121) ;
            ! SSM/T1 temperatures:

            if (.not.use_ssmt1obs) cycle reports
            nlocal(ssmt1) = nlocal(ssmt1) + 1

         case (122) ;
            ! SSM/T2 relative humidities:

            if (.not.use_ssmt2obs) cycle reports
            nlocal(ssmt2) = nlocal(ssmt2) + 1

         case (281)    ;
            ! Scatterometer:

            if (.not.use_qscatobs) cycle reports
            nlocal(qscat)  = nlocal(qscat)  + 1

         case (132) ;

            if (.not.use_profilerobs) cycle reports
            nlocal(profiler) = nlocal(profiler) + 1

         case (135) ;

            if (.not.use_bogusobs) cycle reports
            nlocal(bogus) = nlocal(bogus) + 1

         case (18,19) ;             ! bouy

            if (.not.use_buoyobs) cycle reports
            nlocal(buoy)  = nlocal(buoy)  + 1

         case (133) ;      !  AIRS retrievals
            if (.not.use_airsretobs) cycle reports
            nlocal(airsr) = nlocal(airsr) + 1

         case default;
            num_others = num_others + 1
            write(unit=message(1), fmt='(a)') 'unsaved obs found:'
            write(unit=message(2), fmt='(2a)') &
               'platform%info%platform=', platform%info%platform
            write(unit=message(3), fmt='(a, i3)') &
               'platform%info%levels=', platform%info%levels
            call da_warning(__FILE__,__LINE__,message(1:3))
         end select
      end do reports                  !  Loop over reports              
      close (iunit)
   end do               !  Loop over all data files
   call da_free_unit (iunit)

   ! write counts in the header 

   total_obs = nlocal(synop) + nlocal(metar) + nlocal(ships) + &
      nlocal(buoy) + nlocal(sound) + nlocal(sonde_sfc) + nlocal(airep) + nlocal(pilot) + &
      nlocal(geoamv) + nlocal(polaramv) + nlocal(gpspw) + nlocal(gpsref) + &
      nlocal(profiler) + nlocal(qscat) + nlocal(ssmt1) + nlocal(ssmt2) +  &
      nlocal(satem)  + nlocal(bogus) +  nlocal(airsr) + nlocal(mtgirs) + nlocal(tamdar) + nlocal(tamdar_sfc) + num_others

   write  (unit = filtered_obs_unit, fmt = '(A,I7,A,F8.0,A)')    &
      "TOTAL =",total_obs, ", MISS. =",missing_r,","  

   ! Write other counts       

   write  (unit = filtered_obs_unit, fmt = '(6(A,I7,A))')    &
      "SYNOP =",nlocal(synop),", ", &
      "METAR =",nlocal(metar),", ", &
      "SHIP  =",nlocal(ships),", ", &
      "BUOY  =",nlocal(buoy),", ", &
      "TEMP  =",nlocal(sound),", ", &
      "TEMP_SFC  =",nlocal(sonde_sfc),", ", &
      "AIREP =",nlocal(airep),", ", &
      "PILOT =",nlocal(pilot),", ", &
      "GeAMV =",nlocal(geoamv),", ", &
      "PoAMV =",nlocal(polaramv),", ", &
      "GPSPW =",nlocal(gpspw),", ", &
      "GPSRF =",nlocal(gpsref),", ", &
      "PROFL =",nlocal(profiler),", ", &
      "QSCAT =",nlocal(qscat),", ", &
      "SSMT1 =",nlocal(ssmt1),", ", &
      "SSMT2 =",nlocal(ssmt2),", ", &
      "SATEM =",nlocal(satem), ", ", &
      "BOGUS =",nlocal(bogus),", ", &
      "AIRSR =",nlocal(airsr),", ", &
      "MTGIRS=",nlocal(mtgirs),", ", &
      "TAMDAR=",nlocal(tamdar),", ", &
      "TAMDAR_SFC=",nlocal(tamdar_sfc),", ", &
      "OTHER =",num_others,", "

   ! write reference state info

   write (unit = filtered_obs_unit, fmt = '(A,F7.2,A,F7.2,4(A,F7.2),A)') &
      "PHIC  =", phic,", XLONC =", xlonc,", TRUE1 =", truelat1,&
      ", TRUE2 =",truelat2, ", XIM11 =", start_y, ", XJM11 =", start_x, ","
   write (unit = filtered_obs_unit, fmt = '(2(A,F7.2),A,F7.0,A,F7.0,A)') &
      "TS0   =",  base_temp, ", TLP   =", base_lapse, &
      ", PTOP  =",  ptop,", PS0   =",  base_pres,"," 

   ! write domain info 

   !  hardwire following variables
      maxnes = 2; numc = 1
   if ( coarse_ix == ide+1 .and. coarse_jy == jde+1 ) then
      idd = 2
   else
      idd = 1
   end if
   write (unit = filtered_obs_unit, fmt = '(5(A,I7),A)') &
      "IXC   =", coarse_jy   ,", JXC   =", coarse_ix   ,", IPROJ =", map,&
      ", IDD   =", idd,", MAXNES=",maxnes,","
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTIX=", coarse_jy, ", ", jde+1, ", "
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTJX=", coarse_ix, ", ", ide+1, ", "
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NUMC  =",(numc ,", ", i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,F7.2,A))')&
      "DIS   =",(ds/1000.0,", ",i = 1, maxnes)
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTI=", 1, ", ", int(start_y), ", "
   write (unit = filtered_obs_unit, fmt = '(A,10(:,I7,A))')  &
      "NESTJ=", 1, ", ", int(start_x), ", "

   ! write variable names and unit

   write (unit = filtered_obs_unit, fmt = '(A)') &
      "INFO  = PLATFORM, DATE, NAME, LEVELS, LATITUDE, LONGITUDE, ELEVATION, ID."
   write (unit = filtered_obs_unit, fmt = '(A)') &
      "SRFC  = SLP, PW (DATA,QC,ERROR)."
   write (unit = filtered_obs_unit, fmt = '(A)') &
      "EACH  = PRES, SPEED, DIR, HEIGHT, TEMP, DEW PT, HUMID (DATA,QC,ERROR)*LEVELS."

   ! write format info
 
   write (unit = filtered_obs_unit, fmt = '(2A)') 'INFO_fmt = ', trim (fmt_info)
   write (unit = filtered_obs_unit, fmt = '(2A)') 'SRFC_fmt = ', trim (fmt_srfc)
   write (unit = filtered_obs_unit, fmt = '(2A)') 'EACH_fmt = ', trim (fmt_each)

   ! write end of header record

   if (write_mod_filtered_obs) then
      write (unit = filtered_obs_unit, fmt = '(A)') &
         "MODIFIED FILTERED OBS #--------------------------------------------------------#"
   else
      write (unit = filtered_obs_unit, fmt = '(A)') &
         "FILTERED OBS #-----------------------------------------------------------------#"
   end if     

   if (trace_use) call da_trace_exit("da_count_filtered_obs")

end subroutine da_count_filtered_obs 


