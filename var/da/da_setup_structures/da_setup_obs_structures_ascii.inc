subroutine da_setup_obs_structures_ascii( ob, iv, grid )

   !-------------------------------------------------------------------------
   ! Purpose: Define, allocate and read of observation structure.
   ! Updates:
   !          Syed RH Rizvi NCAR/NESL/MMM/DAS Date:  02/21/2013 
   !          Updated with thinning option
   !-------------------------------------------------------------------------

   implicit none

   type (y_type),  intent(out)   :: ob  ! Observation structure.
   type (iv_type), intent(inout) :: iv  ! O-B structure.
   type (domain),  intent(inout) :: grid  ! First guess structure

   character(len=filename_len)  :: filename
   integer                      :: n, i, j, k
   logical                      :: outside, thin_3d
   logical                      :: uvq_direct=.false.

   if (trace_use) call da_trace_entry("da_setup_obs_structures_ascii")
   !-------------------------------
   ! 0.0 Make thinning grids
   !------------------------------
   thin_3d=.false.
   if ( thin_conv_ascii ) then
      allocate(thinning_grid_conv(num_ob_indexes))
      do n = 1, num_ob_indexes
         if ( thin_conv_opt(n) <= no_thin ) cycle
         if( n == airep .or. n == tamdar ) then
            thin_3d=.true.
            call make3grids (n,thin_mesh_conv(n), thin_3d)
         else
            call make3grids (n,thin_mesh_conv(n))
         end if
      end do
   end if

   !--------------------------------------------------------------------------
   ! [1.0] Scan GTS observation header and get idea of number of obs:
   !--------------------------------------------------------------------------
  
   if (num_fgat_time > 1) then
!      filename = ' '

      do n=1, num_fgat_time

         iv%time = n
         filename = ' '

        write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.ascii'

         ! scan main body of gts observation file
         call da_scan_obs_ascii (iv, filename,grid)

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! scan SSMI observation file
            write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.ssmi'
            call da_scan_obs_ssmi (iv, filename)
         end if

         if (use_lsac) then
            call da_scan_obs_lsac(iv)
         endif

         iv%info(:)%plocal(n) = iv%info(:)%nlocal
         iv%info(:)%ptotal(n) = iv%info(:)%ntotal
      end do
   else
      iv%time = 1
      call da_scan_obs_ascii(iv, 'ob.ascii', grid)
      !-----------------------------------------------------------------------
      ! read header of ssmi observation file
      !-----------------------------------------------------------------------
      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         call da_scan_obs_ssmi(iv, 'ob.ssmi')
      end if

      if (use_lsac) then
         call da_scan_obs_lsac(iv)
      endif

      do i=1,num_ob_indexes
         if (i == radar .or. i == lightning) cycle
         iv%info(i)%plocal(iv%time) = iv%info(i)%nlocal
         iv%info(i)%ptotal(iv%time) = iv%info(i)%ntotal
      end do
   end if

   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------
   call da_allocate_observations (iv)

   if (num_fgat_time > 1) then

      do n=1, num_fgat_time
         iv%time = n
         filename = ' '  

         write(filename(1:10), fmt='(a, i2.2, a)') 'ob', n,'.ascii'

         ! Read gts observation file
         call da_read_obs_ascii (iv, filename, uvq_direct, grid)

         if (use_ssmiretrievalobs .or. use_ssmitbobs) then
            ! read ssmi observation file
            write(filename(1:9), fmt='(a, i2.2, a)') 'ob', n,'.ssmi'
            call da_read_obs_ssmi (iv, filename)
         end if

         if (use_lsac) then
            call da_read_obs_lsac(iv)
         end if

         do i=1,num_ob_indexes
            if (i == radar .or. i==lightning) cycle
            iv%info(i)%thin_ptotal(n) = iv%info(i)%thin_ntotal
            iv%info(i)%thin_plocal(n) = iv%info(i)%thin_nlocal
         end do
      end do
   else
      iv%time = 1

      call da_read_obs_ascii(iv, 'ob.ascii', uvq_direct, grid)

      if (use_ssmiretrievalobs .or. use_ssmitbobs) then
         ! read ssmi observation file
         call da_read_obs_ssmi (iv, 'ob.ssmi')
      end if

      if (use_lsac) then
         call da_read_obs_lsac(iv)
      end if

      do i=1,num_ob_indexes
         if (i == radar .or. i == lightning) cycle
         iv%info(i)%thin_ptotal(iv%time) = iv%info(i)%thin_ntotal
         iv%info(i)%thin_plocal(iv%time) = iv%info(i)%thin_nlocal
      end do
   end if

   if ( use_gpsrefobs .and. (ob_format_gpsro == ob_format_bufr) ) then
      call da_read_obs_bufrgpsro(iv)
   end if

   if ( use_gpsephobs .and. (ob_format_gpsro == ob_format_bufr) ) then
      call da_read_obs_bufrgpsro_eph(iv)
   end if

   !--------------------------------------------------------------------------
   ! [2.5] Set all thinned obs missing  
   !--------------------------------------------------------------------------
    if ( thin_conv_ascii ) then
       do i = 1, num_ob_indexes
          if ( thin_conv_opt(i) <= no_thin ) cycle
          if (i == radar .or. i == lightning) cycle
          if ( iv%info(i)%ntotal > 0 ) then
             if ( iv%info(i)%nlocal > 0 ) then
                if ( ANY(iv%info(i)%thinned(:,:)) ) then
                   if( i == airep .or. i==tamdar ) then
                      call da_set_3d_obs_missing(iv,i)  ! assign missing values level-by-level if thinned=true data
                   else
                      call da_set_obs_missing(iv,i)  ! assign missing values to those thinned=true data
                   end if
                end if
             end if
          end if
       end do
    end if ! thin_conv_ascii
  
   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   if (uvq_direct) then
      call da_fill_obs_structures(iv, ob, uvq_direct)
   else
      call da_fill_obs_structures(iv, ob)
   endif



   iv%time = 1

   if ( thin_conv_ascii ) then
      do n = 1, num_ob_indexes
         if ( thin_conv_opt(n) <= no_thin ) cycle
         if( n == airep .or. n==tamdar ) then
            thin_3d=.true.
            call destroygrids_conv (n, thin_3d)
         else
            call destroygrids_conv (n)
         end if
      end do
      deallocate(thinning_grid_conv)
   end if

   if (trace_use) call da_trace_exit("da_setup_obs_structures_ascii")
end subroutine da_setup_obs_structures_ascii


