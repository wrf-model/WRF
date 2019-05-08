subroutine da_read_obs_chem(grid, iv) 
   !-------------------------------------------------------------------------
   ! Purpose:        Read surface and aircraft chem observation files
   !
   ! History: 06/2015  Creation (Jonathan Guerrette)
   !
   !-------------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)     :: grid
   type (iv_type),    intent(inout) :: iv
!   character(len=*),  intent(in)    :: surf_file, acft_file

   integer                                    :: iobs, jobs, itime, ichem, iplt, iunit, dummyi, &
                                                 nsurf, nacft, iostat, nrepeat, &
                                                 ilocal
   integer, dimension(num_surf_obs)  :: repeat_obs
   real, dimension(num_surf_obs)     :: tis, tfs, iis, jjs, kks
   real, dimension(:,:), allocatable :: chem_s
   real, dimension(:,:), allocatable :: chem_VARs
   real, dimension(num_surf_obs)     :: dummy_outs

   real, dimension(num_acft_obs)     :: tia, tfa, iia, jja, kka, ppa
   real, dimension(:,:), allocatable :: chem_a
   real, dimension(:,:), allocatable :: chem_a_avg

   real, dimension(num_acft_obs)     :: dummy_outa
   real, dimension(num_ts)           :: dummy_outaa
   real, dimension(:,:), allocatable :: chem_VARa

   logical                           :: eliminate_colocated = .TRUE.
   character*3                       :: dummychar=""

   ! Need to modify file naming conventions and related code
   !   when multiple aircraft platforms are used.  Surface
   !   files can be concatenated. -JJG
   character*256         :: surf_file = "SURFACE_DATA.txt"
   character*256         :: acft_file = "AIRCRAFT_DATA.txt"  
   character*256         :: acft_var_file = "AIRCRAFT_MODEL_VARIANCE.txt"  

   if (trace_use) call da_trace_entry("da_read_obs_chem")

   nsurf = num_surf_obs
   nacft = num_acft_obs

   if (nsurf .gt. 0 .and. use_chem_surfobs) then
      call da_message((/"Reading in chem SURFACE observations"/))

!      do iobs=1,nsurf
!         allocate ( iv%chem_surf(iobs)%chem (num_chem_surf-1) )
!         allocate ( iv%chem_surf(iobs)%weight (num_chem_surf-1) )
!
!         do ichem = 1,num_chem_surf-1
!            iv%chem_surf(iobs)%chem(ichem)%inv = -5.D0
!            iv%chem_surf(iobs)%chem(ichem)%error = -5.D0
!            iv%chem_surf(iobs)%weight(ichem) = -5.D0
!         end do
!      end do

      tis(:) = 0.D0
      tfs(:) = 0.D0
      iis(:) = 0.D0
      jjs(:) = 0.D0
      kks(:) = 0.D0

      allocate( chem_s (num_surf_obs,num_chem_surf-1) )
      allocate( chem_VARs (num_surf_obs,num_chem_surf-1) )

      chem_s(:,:) = 0.D0
      chem_VARs(:,:) = 0.D0

      repeat_obs(:) = 0
      nrepeat = 0

      if(rootproc) then
      ! =========
      ! open file
      ! =========
         call da_get_unit(iunit)  
         open(unit   = iunit,     &
            FILE   = trim(surf_file), &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            iostat =  iostat,     &
            STATUS = 'OLD')
   
         if (iostat /= 0) then
            write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
            call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!            call da_wrf_error_fatal('Error opening surface chem obs file')
            call wrf_message('Error opening surface chem obs file')  !!! add !!!
         end if   

         write(dummychar,fmt='(I0)') num_chem_surf-1
         do iobs=1,nsurf
            read (iunit, fmt = '(I5,2(F10.3),3(F17.12),' // trim(dummychar) // '(D10.3),' // trim(dummychar) // '(D10.3))', iostat = iostat) &
              dummyi, &
              tis(iobs), &
              tfs(iobs), &
              iis(iobs), &
              jjs(iobs), &
              kks(iobs), &
              chem_s(iobs,:), &
              chem_VARs(iobs,:)

            if ( eliminate_colocated) then
               do jobs=1,iobs-1
                  if ( repeat_obs(jobs) .gt. 0 ) cycle
                  if ( tis(iobs) .eq. tis(jobs) .and. &
                       tfs(iobs) .eq. tfs(jobs) .and. &
                       iis(iobs) .eq. iis(jobs) .and. &
                       jjs(iobs) .eq. jjs(jobs) .and. &
                       kks(iobs) .eq. kks(jobs) ) then
                     repeat_obs(iobs) = jobs
                     nrepeat = nrepeat + 1
                  end if
               end do
            end if

            if (iostat /= 0 .OR. dummyi < 0) then
               write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
               call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!               call da_wrf_error_fatal('Error reading surface chem obs')
               call wrf_message('frror reading surface chem obs') !!! add !!!
            end if
         end do

         close(iunit)
         call da_free_unit(iunit)

         if ( eliminate_colocated) then
            do iobs=1,nsurf
               if (repeat_obs(iobs) .gt. 0) then
                  jobs = repeat_obs(iobs)
                  repeat_obs(jobs) = repeat_obs(jobs) + 1
                  do ichem = 1, num_chem_surf-1
                     chem_s(jobs,ichem) = ( chem_s(jobs,ichem) * repeat_obs(jobs) + chem_s(iobs,ichem) )/ (repeat_obs(jobs) + 1)
                     chem_s(iobs,ichem) = missing_r
                  end do

                  tis(iobs) = missing_r
                  tfs(iobs) = missing_r
                  iis(iobs) = missing_r
                  jjs(iobs) = missing_r
                  kks(iobs) = missing_r
                  repeat_obs(iobs) = 0
               end if
            end do
            if ( nrepeat .ne. sum(repeat_obs) ) then
               write(unit=message(1),fmt='(A,2I5)') "Error finding co-located surface obs.: ",nrepeat,sum(repeat_obs)
               call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!               call da_wrf_error_fatal('Error reading surface chem obs')
               call wrf_message('Error reading surface chem obs')  !!! add !!!
            end if
         end if
      end if

#ifdef DM_PARALLEL
      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(tis,  dummy_outs)
      tis = dummy_outs
!      grid%ti_surf = dummy_outs

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(tfs,  dummy_outs)
      tfs = dummy_outs
!      grid%tf_surf = dummy_outs

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(iis,  dummy_outs)
      iis = dummy_outs
!      grid%i_surf = dummy_outs

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(jjs,  dummy_outs)
      jjs = dummy_outs
!      grid%j_surf = dummy_outs

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(kks,  dummy_outs)
      kks = dummy_outs
!      grid%k_surf = dummy_outs

      do ichem = 1, num_chem_surf-1
         call mpi_barrier(comm, ierr)
         call wrf_dm_sum_reals(chem_s(:,ichem),  dummy_outs)
         chem_s(:,ichem) = dummy_outs

         call mpi_barrier(comm, ierr)
         call wrf_dm_sum_reals(chem_VARs(:,ichem),  dummy_outs)
         chem_VARs(:,ichem) = dummy_outs
      end do
#endif
      grid%ti_surf = tis
      grid%tf_surf = tfs
      grid%i_surf = iis
      grid%j_surf = jjs
      grid%k_surf = kks


!#else
!      grid%ti_surf = tis
!      grid%tf_surf = tfs
!      grid%i_surf = iis
!      grid%j_surf = jjs
!      grid%k_surf = kks
!
!#endif

      ilocal = 0
      do iobs=1,nsurf
         allocate ( iv%chem_surf(iobs)%chem (num_chem_surf-1) )
         allocate ( iv%chem_surf(iobs)%weight (num_chem_surf-1) )
         if (floor(tis(iobs)) .GE. 1 .AND. &
             ceiling(tfs(iobs)) .LE. num_ts) THEN
            ilocal = ilocal + 1
            iv%info(chem_surf)%i(:,iobs) = INT( iis(iobs) + 0.5D0 )
            iv%info(chem_surf)%j(:,iobs) = INT( jjs(iobs) + 0.5D0 )
            iv%info(chem_surf)%k(:,iobs) = INT( kks(iobs) + 0.5D0 )
            iv%chem_surf(iobs)%ti = tis(iobs) * REAL(time_step, 8) / 3600.0
            iv%chem_surf(iobs)%tf = tfs(iobs) * REAL(time_step, 8) / 3600.0
            do ichem = 1, num_chem_surf-1
               iv%chem_surf(iobs)%chem(ichem)%inv = chem_s(iobs,ichem)
               iv%chem_surf(iobs)%chem(ichem)%error = sqrt( chem_VARs(iobs,ichem) )
               iv%chem_surf(iobs)%chem(ichem)%qc = 0
               iv%chem_surf(iobs)%weight(ichem) = 1.D0
            end do
         else
            iv%info(chem_surf)%i(:,iobs) = missing
            iv%info(chem_surf)%j(:,iobs) = missing
            iv%info(chem_surf)%k(:,iobs) = missing
            iv%chem_surf(iobs)%ti = missing_r
            iv%chem_surf(iobs)%tf = missing_r
            do ichem = 1, num_chem_surf-1
               iv%chem_surf(iobs)%chem(ichem)%inv = missing_r
               iv%chem_surf(iobs)%chem(ichem)%error = missing_r 
               iv%chem_surf(iobs)%chem(ichem)%qc = -77
               iv%chem_surf(iobs)%weight(ichem) = missing_r
            end do

         end if
      end do
      iv%info(chem_surf)%n1 = 1
      iv%info(chem_surf)%n2 = nsurf
!      iv%info(chem_surf)%nlocal = ilocal
!      iv%info(chem_surf)%ntotal = ilocal

      deallocate( chem_s )
      deallocate( chem_VARs )

   end if

   if (nacft .gt. 0 .and. use_chem_acftobs) then
      call da_message((/"Reading in chem AIRCRAFT observations"/))

      do itime=1,num_ts
         allocate ( iv%chem_acft(itime)%chem (num_chem_acft-1,num_platform) )
         allocate ( iv%chem_acft(itime)%weight (num_chem_acft-1,num_platform) )
      end do

      iplt = 1
!   do iplt = 1, num_platform


      tia(:) = 0.D0
      tfa(:) = 0.D0
      iia(:) = 0.D0
      jja(:) = 0.D0
      kka(:) = 0.D0
      ppa(:) = 0.D0

      allocate( chem_a (num_acft_obs,num_chem_acft-1) )
      chem_a(:,:) = 0.D0

      if(rootproc) then
      ! =========   
      ! open file
      ! =========   
         call da_get_unit(iunit)  
         open(unit   = iunit,     &
            FILE   = trim(acft_file), &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            iostat =  iostat,     &
            STATUS = 'OLD')
   
         if (iostat /= 0) then
            write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
            call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!            call da_wrf_error_fatal('Error opening aircraft chem obs file')
                       call wrf_message('Error opening aircraft chem obs file')  !!! add !!!
         end if   

         write(dummychar,fmt='(I0)') num_chem_acft-1
         do iobs=1,nacft
            read (iunit, fmt = '(I5,2(F10.3),3(F17.12),F20.12,' // trim(dummychar) // 'D10.3)', iostat = iostat) &
              dummyi, &
              tia(iobs), &
              tfa(iobs), &
              iia(iobs), &
              jja(iobs), &
              kka(iobs), &
              ppa(iobs), &
              chem_a(iobs,:)
            if (iostat /= 0 .OR. dummyi < 0) then
               write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
               call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!               call da_wrf_error_fatal('Error reading aircraft chem obs')
               call wrf_message('Error reading aircraft chem obs')  !!! add !!!
            end if 
         end do
         close(iunit)
         call da_free_unit(iunit)
      end if

#ifdef DM_PARALLEL
      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(tia,  dummy_outa)
      tia = dummy_outa
!      grid%ti_acft = dummy_outa

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(tfa,  dummy_outa)
      tfa = dummy_outa
!      grid%tf_acft = dummy_outa

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(iia,  dummy_outa)
      iia = dummy_outa
!      grid%i_acft = dummy_outa

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(jja,  dummy_outa)
      jja = dummy_outa
!      grid%j_acft = dummy_outa

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(kka,  dummy_outa)
      kka = dummy_outa
!      grid%k_acft = dummy_outa

      call mpi_barrier(comm, ierr)
      call wrf_dm_sum_reals(ppa,  dummy_outa)
      ppa = dummy_outa
!      grid%p_acft = dummy_outa

      do ichem = 1, num_chem_acft-1
         call mpi_barrier(comm, ierr)
         call wrf_dm_sum_reals(chem_a(:,ichem),  dummy_outa)
         chem_a(:,ichem) = dummy_outa
      end do

#endif


      grid%i_acft(:,iplt) = 0.D0
      grid%j_acft(:,iplt) = 0.D0
      grid%k_acft(:,iplt) = 0.D0
      grid%p_acft(:,iplt) = 0.D0

      allocate( chem_a_avg (num_ts,num_chem_acft-1) )
      chem_a_avg(:,:) = 0.D0

      grid%acft_obs_count(:,iplt) = 0

      do iobs = 1,nacft
         itime = INT( tia(iobs) + 0.5D0 )
         if (itime >= 1 .AND. itime <=num_ts) THEN
            grid%i_acft(itime,iplt) = grid%i_acft(itime,iplt) + iia(iobs)
            grid%j_acft(itime,iplt) = grid%j_acft(itime,iplt) + jja(iobs)
            grid%k_acft(itime,iplt) = grid%k_acft(itime,iplt) + kka(iobs)
            grid%p_acft(itime,iplt) = grid%p_acft(itime,iplt) + ppa(iobs)
            do ichem = 1, num_chem_acft-1
               chem_a_avg(itime,ichem) = chem_a_avg(itime,ichem) + chem_a(iobs,ichem)
            end do
            grid%acft_obs_count(itime,iplt) = grid%acft_obs_count(itime,iplt) + 1
         end if
      end do

      do itime = 1,num_ts
         if( grid%acft_obs_count(itime,iplt) > 0 ) then
            grid%i_acft(itime,iplt) = grid%i_acft(itime,iplt) / REAL(grid%acft_obs_count(itime,iplt),8)
            grid%j_acft(itime,iplt) = grid%j_acft(itime,iplt) / REAL(grid%acft_obs_count(itime,iplt),8)
            grid%k_acft(itime,iplt) = grid%k_acft(itime,iplt) / REAL(grid%acft_obs_count(itime,iplt),8)
            grid%p_acft(itime,iplt) = grid%p_acft(itime,iplt) / REAL(grid%acft_obs_count(itime,iplt),8)
            do ichem = 1, num_chem_acft-1
               chem_a_avg(itime,ichem) = chem_a_avg(itime,ichem) / REAL(grid%acft_obs_count(itime,iplt),8)
            end do
         end if
      end do

      call da_message((/"Reading in chem AIRCRAFT obs. variance"/))

      allocate( chem_VARa (num_ts,num_chem_acft-1) )
      chem_VARa(:,:) = 0.D0

      if(rootproc) then
      ! =========   
      ! open file
      ! =========   
         call da_get_unit(iunit)  
         open(unit   = iunit,     &
            FILE   = trim(acft_var_file), &
            FORM   = 'FORMATTED',  &
            ACCESS = 'SEQUENTIAL', &
            iostat =  iostat,     &
            STATUS = 'OLD')
   
         if (iostat /= 0) then
            write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
            call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!            call da_wrf_error_fatal('Error opening aircraft chem obs variance file')
            call wrf_message('Error opening aircraft chem obs variance file') !!! add !!!
         end if   

         write(dummychar,fmt='(I0)') num_chem_acft-1
         do itime=1,num_ts
            read (iunit, fmt = '(I5,' // trim(dummychar) // 'D10.3)', iostat = iostat) &
              dummyi, &
              chem_VARa(itime,:)
            if (iostat /= 0 .OR. dummyi < 0) then
               write(unit=message(1),fmt='(A,I5)') "Error message: ",iostat
               call da_warning(__FILE__,__LINE__,message(1:1))
!!! add !!!               call da_wrf_error_fatal('Error reading aircraft chem obs variance')
               call wrf_message('Error reading aircraft chem obs variance')  !!! add !!!
            end if 
         end do
         close(iunit)
         call da_free_unit(iunit)
      end if

#ifdef DM_PARALLEL

      do ichem = 1, num_chem_acft-1
         call mpi_barrier(comm, ierr)
         call wrf_dm_sum_reals(chem_VARa(:,ichem),  dummy_outaa)
         chem_VARa(:,ichem) = dummy_outaa
      end do
#endif

      ilocal = 0
      do itime=1,num_ts
         if ( grid%acft_obs_count(itime,iplt) > 0 ) THEN
            ilocal = ilocal + 1
            iv%info(chem_acft)%i(iplt,itime) = INT( grid%i_acft(itime,iplt) + 0.5D0 )
            iv%info(chem_acft)%j(iplt,itime) = INT( grid%j_acft(itime,iplt) + 0.5D0 )
            iv%chem_acft(itime)%ti = REAL((itime-1) * time_step, 8) / 3600.0
            iv%chem_acft(itime)%tf = REAL(itime * time_step, 8) / 3600.0
            do ichem = 1, num_chem_acft-1
               iv%chem_acft(itime)%chem(ichem,iplt)%inv = chem_a_avg(itime,ichem)
               iv%chem_acft(itime)%chem(ichem,iplt)%error = sqrt( chem_VARa(itime,ichem) )
               iv%chem_acft(itime)%chem(ichem,iplt)%qc = 0
               iv%chem_acft(itime)%weight(ichem,iplt) = 1.D0
            end do
         else
            iv%info(chem_acft)%i(iplt,itime) = missing
            iv%info(chem_acft)%j(iplt,itime) = missing
            iv%chem_acft(itime)%ti = missing_r
            iv%chem_acft(itime)%tf = missing_r
            do ichem = 1, num_chem_acft-1
               iv%chem_acft(itime)%chem(ichem,iplt)%inv = missing_r
               iv%chem_acft(itime)%chem(ichem,iplt)%error = missing_r
               iv%chem_acft(itime)%chem(ichem,iplt)%qc = -77
               iv%chem_acft(itime)%weight(ichem,iplt) = missing_r
            end do           
         end if
      end do
      iv%info(chem_acft)%n1 = 1
      iv%info(chem_acft)%n2 = num_ts

      deallocate ( chem_a )
      deallocate ( chem_a_avg )
      deallocate ( chem_VARa )

   end if

   if (trace_use) call da_trace_exit("da_read_obs_chem")

end subroutine da_read_obs_chem

