subroutine da_read_iv_for_multi_inc(file_index, iv)

   !-----------------------------------------------------------------------
   ! Purpose: Read for Multi-incremental 
   !-----------------------------------------------------------------------

   !-------------------------------------------------------------------------
   ! read iv=O-B structure written by WRFVAR
   !-------------------------------------------------------------------------

   implicit none

   type (iv_type), intent(inout)    :: iv      ! O-B structure.
   integer,  intent(in)             :: file_index
   integer                      :: unit_in
   character(len=filename_len)   :: filename

   integer      :: num_obs, ios 
   character*20 :: ob_type_string               
   
   integer      :: n, gn
   logical      :: found_flag

   if (trace_use) call da_trace_entry("da_read_iv_for_multi_inc")

   !-------------------------------------------------------------------------
   ! Fix input unit
   !-------------------------------------------------------------------------

   call da_get_unit(unit_in)

   write(unit=filename, fmt='(a,i3.3)') 'gts_omb.', file_index

   ! [1] surface obs:

   if (iv%info(synop)%plocal(iv%time)-iv%info(synop)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.synop',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'synop' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find synop marker. "/))
       gn = 0
       do n = iv%info(synop)%plocal(iv%time-1) + 1, &
              iv%info(synop)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find synop obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(synop)%plocal(iv%time)-iv%info(synop)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [2] metar obs:

   if (iv%info(metar)%plocal(iv%time)-iv%info(metar)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.metar',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'metar' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find metar marker. "/))
       gn = 0
       do n = iv%info(metar)%plocal(iv%time-1) + 1, &
              iv%info(metar)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find metar obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(metar)%plocal(iv%time)-iv%info(metar)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [3] ships obs:

   if (iv%info(ships)%plocal(iv%time)-iv%info(ships)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.ships',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ships' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ships marker. "/))
       gn = 0
       do n = iv%info(ships)%plocal(iv%time-1) + 1, &
              iv%info(ships)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ships obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(ships)%plocal(iv%time)-iv%info(ships)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [4] sonde_sfc obs:

   if (iv%info(sonde_sfc)%plocal(iv%time)-iv%info(sonde_sfc)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.sonde_sfc',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'sonde_sfc' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sonde_sfc marker. "/))
       gn = 0
       do n = iv%info(sonde_sfc)%plocal(iv%time-1) + 1, &
              iv%info(sonde_sfc)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sonde_sfc obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(sonde_sfc)%plocal(iv%time)-iv%info(sonde_sfc)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [5] sound obs:

   if (iv%info(sound)%plocal(iv%time)-iv%info(sound)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.sound',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'sound' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sound marker. "/))
       gn = 0
       do n = iv%info(sound)%plocal(iv%time-1) + 1, &
              iv%info(sound)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find sound obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(sound)%plocal(iv%time)-iv%info(sound)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [6] mtgirs obs:

   if (iv%info(mtgirs)%plocal(iv%time)-iv%info(mtgirs)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.mtgirs',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'mtgirs' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find mtgirs marker. "/))
       gn = 0
       do n = iv%info(mtgirs)%plocal(iv%time-1) + 1, &
              iv%info(mtgirs)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find mtgirs obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(mtgirs)%plocal(iv%time)-iv%info(mtgirs)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [7] tamdar obs:

   if (iv%info(tamdar)%plocal(iv%time)-iv%info(tamdar)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.tamdar',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'tamdar' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find tamdar marker. "/))
       gn = 0
       do n = iv%info(tamdar)%plocal(iv%time-1) + 1, &
              iv%info(tamdar)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find tamdar obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(tamdar)%plocal(iv%time)-iv%info(tamdar)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [8] tamdar_sfc obs:

   if (iv%info(tamdar_sfc)%plocal(iv%time)-iv%info(tamdar_sfc)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.tamdar_sfc',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'tamdar_sfc' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find tamdar_sfc marker. "/))
       gn = 0
       do n = iv%info(tamdar_sfc)%plocal(iv%time-1) + 1, &
              iv%info(tamdar_sfc)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find tamdar_sfc obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(tamdar_sfc)%plocal(iv%time)-iv%info(tamdar_sfc)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [9] buoy obs:

   if (iv%info(buoy)%plocal(iv%time)-iv%info(buoy)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.buoy',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'buoy' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find buoy marker. "/))
       gn = 0
       do n = iv%info(buoy)%plocal(iv%time-1) + 1, &
              iv%info(buoy)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find buoy obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(buoy)%plocal(iv%time)-iv%info(buoy)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [10] Geo AMV obs:

   if (iv%info(geoamv)%plocal(iv%time)-iv%info(geoamv)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.geoamv',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'geoamv' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find geoamv marker. "/))
       gn = 0
       do n = iv%info(geoamv)%plocal(iv%time-1) + 1, &
              iv%info(geoamv)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find geoamv obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(geoamv)%plocal(iv%time)-iv%info(geoamv)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [11] gpspw obs:

   if (iv%info(gpspw)%plocal(iv%time)-iv%info(gpspw)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.gpspw',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'gpspw' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpspw marker. "/))
       gn = 0
       do n = iv%info(gpspw)%plocal(iv%time-1) + 1, &
              iv%info(gpspw)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpspw obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(gpspw)%plocal(iv%time)-iv%info(gpspw)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [12] SSM/I obs:

   if (iv%info(ssmi_rv)%plocal(iv%time)-iv%info(ssmi_rv)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmir',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmir' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmir marker. "/))
       gn = 0
       do n = iv%info(ssmi_rv)%plocal(iv%time-1) + 1, &
              iv%info(ssmi_rv)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmir obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(ssmi_rv)%plocal(iv%time)-iv%info(ssmi_rv)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [13] airep obs:

   if (iv%info(airep)%plocal(iv%time)-iv%info(airep)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.airep',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'airep' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airep marker. "/))
       gn = 0
       do n = iv%info(airep)%plocal(iv%time-1) + 1, &
              iv%info(airep)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airep obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(airep)%plocal(iv%time)-iv%info(airep)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [14] polaramv obs:

   if (iv%info(polaramv)%plocal(iv%time)-iv%info(polaramv)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.polaramv',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'polaramv' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find polaramv marker. "/))
       gn = 0
       do n = iv%info(polaramv)%plocal(iv%time-1) + 1, &
              iv%info(polaramv)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find polaramv obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(polaramv)%plocal(iv%time)-iv%info(polaramv)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [15] pilot obs:

   if (iv%info(pilot)%plocal(iv%time)-iv%info(pilot)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.pilot',form='formatted',status='old',iostat=ios)

       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'pilot' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find pilot marker. "/))
       gn = 0
       do n = iv%info(pilot)%plocal(iv%time-1) + 1, &
              iv%info(pilot)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find pilot obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(pilot)%plocal(iv%time)-iv%info(pilot)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [16] ssmi_tb obs:

   if (iv%info(ssmi_tb)%plocal(iv%time)-iv%info(ssmi_tb)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmi_tb',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmi_tb' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmi_tb marker. "/))
       gn = 0
       do n = iv%info(ssmi_tb)%plocal(iv%time-1) + 1, &
              iv%info(ssmi_tb)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmi_tb obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(ssmi_tb)%plocal(iv%time)-iv%info(ssmi_tb)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [17] satem obs:

   if (iv%info(satem)%plocal(iv%time)-iv%info(satem)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.satem',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'satem' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find satem marker. "/))
       gn = 0
       do n = iv%info(satem)%plocal(iv%time-1) + 1, &
              iv%info(satem)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find satem obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(satem)%plocal(iv%time)-iv%info(satem)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [18] ssmt1 obs:

   if (iv%info(ssmt1)%plocal(iv%time)-iv%info(ssmt1)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmt1',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmt1' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt1 marker. "/))
       gn = 0
       do n = iv%info(ssmt1)%plocal(iv%time-1) + 1, &
              iv%info(ssmt1)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt1 obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(ssmt1)%plocal(iv%time)-iv%info(ssmt1)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [19] ssmt2 obs:

   if (iv%info(ssmt2)%plocal(iv%time)-iv%info(ssmt2)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.ssmt2',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'ssmt2' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt2 marker. "/))
       gn = 0
       do n = iv%info(ssmt2)%plocal(iv%time-1) + 1, &
              iv%info(ssmt2)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find ssmt2 obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(ssmt2)%plocal(iv%time)-iv%info(ssmt2)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [20] scatterometer obs:

   if (iv%info(qscat)%plocal(iv%time)-iv%info(qscat)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.qscat',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'qscat' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find qscat marker. "/))
       gn = 0
       do n = iv%info(qscat)%plocal(iv%time-1) + 1, &
              iv%info(qscat)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find qscat obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(qscat)%plocal(iv%time)-iv%info(qscat)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [21] profiler obs:

   if (iv%info(profiler)%plocal(iv%time)-iv%info(profiler)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.profiler',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'profiler' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find profiler marker. "/))
       gn = 0
       do n = iv%info(profiler)%plocal(iv%time-1) + 1, &
              iv%info(profiler)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find profiler obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(profiler)%plocal(iv%time)-iv%info(profiler)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [22] TC bogus obs:

   if (iv%info(bogus)%plocal(iv%time)-iv%info(bogus)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.bogus',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'bogus' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find bogus marker. "/))
       gn = 0
       do n = iv%info(bogus)%plocal(iv%time-1) + 1, &
              iv%info(bogus)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find bogus obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(bogus)%plocal(iv%time)-iv%info(bogus)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [23] AIRS retrievals:

   if (iv%info(airsr)%plocal(iv%time)-iv%info(airsr)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.airsr',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'airsr' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airsr marker. "/))
       gn = 0
       do n = iv%info(airsr)%plocal(iv%time-1) + 1, &
              iv%info(airsr)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find airsr obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(airsr)%plocal(iv%time)-iv%info(airsr)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if

   ! [24] gpsref obs:

   if (iv%info(gpsref)%plocal(iv%time)-iv%info(gpsref)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.gpsref',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'gpsref' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpsref marker. "/))
       gn = 0
       do n = iv%info(gpsref)%plocal(iv%time-1) + 1, &
              iv%info(gpsref)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find gpsref obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(gpsref)%plocal(iv%time)-iv%info(gpsref)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if


   ! [25] radar obs:

   if (iv%info(radar)%plocal(iv%time)-iv%info(radar)%plocal(iv%time-1) > 0) then

       open(unit=unit_in,file=trim(filename)//'.radar',form='formatted',status='old',iostat=ios)
       if (ios /= 0) Then
          call da_error(__FILE__,__LINE__, &
             (/"Cannot open file"//filename/))
       end if

       read(unit_in,'(a20,i8)', end = 999, err = 1000) ob_type_string,num_obs
       if ( trim(adjustl(ob_type_string)) .ne. 'radar' ) &
           call da_error(__FILE__,__LINE__, &
                           (/"Cannot find radar marker. "/))
       gn = 0
       do n = iv%info(radar)%plocal(iv%time-1) + 1, &
              iv%info(radar)%plocal(iv%time)
          call da_search_obs (ob_type_string, unit_in, num_obs, n, iv, found_flag)
          if (found_flag .eqv. .false.) &
              call da_error(__FILE__,__LINE__, &
                           (/"Cannot find radar obs. "/))
          gn = gn + 1
       end do
       if (gn /= iv%info(radar)%plocal(iv%time)-iv%info(radar)%plocal(iv%time-1)) &
           call da_error(__FILE__,__LINE__, &
                        (/"Unequal obs. found "/))
       close (unit_in)
   end if


999 continue
   close (unit_in)
   call da_free_unit(unit_in)

   if (trace_use) call da_trace_exit("da_read_iv_for_multi_inc")
   return

1000 continue
   write(unit=message(1), fmt='(a,i3)') &
      'read error on unit: ',unit_in
   call da_warning(__FILE__,__LINE__,message(1:1))

end subroutine da_read_iv_for_multi_inc
