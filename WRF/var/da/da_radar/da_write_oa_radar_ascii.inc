subroutine da_write_oa_radar_ascii ( ob, iv, re, it )

   !---------------------------------------------------------------------------
   ! Purpose: write out OMB and OMA vector structure for radar data.
   !---------------------------------------------------------------------------

   implicit none

   type (y_type),     intent(in)  :: ob       ! Observation structure.
   type (iv_type),    intent(in)  :: iv       ! O-B structure.
   type (y_type),     intent(in)  :: re       ! O-A structure.
   integer,           intent(in)  :: it       ! external iteration counter

   character(len=filename_len)               :: filename , outfname
   character(len=filename_len), allocatable  :: filename1(:)
   integer :: num_obs, num, levels, ios
   integer :: n, i, k, m, kk, l
   integer :: oma_radar_unit, omb_radar_unit, omb_radar_iter_unit
   real    :: lat, lon, height, zk
   real    :: rv_obs, rv_inv, rv_error, rv_inc
   real    :: rf_obs, rf_inv, rf_error, rf_inc
   real    :: rn_obs, rn_inv, rn_error, rn_inc
   real    :: sn_obs, sn_inv, sn_error, sn_inc
   real    :: gr_obs, gr_inv, gr_error, gr_inc
   real    :: qv_obs, qv_inv, qv_error, qv_inc
   integer :: rv_qc, rf_qc
   integer :: rn_qc, sn_qc, gr_qc, qv_qc
   character(len=5) :: stn_id

   if (trace_use) call da_trace_entry("da_write_oa_radar_ascii")

   write(unit=message(1),fmt='(A)') 'Writing radar OMA ascii file'
   call da_message(message(1:1))

   ! myproc is set in var/da/da_main/da_wrfvar_init1.inc
   ! it is 0 for serial mode
   write(unit=filename, fmt='(a,i2.2,a,i4.4)') 'radar_omb_oma_',it,'.', myproc

   call da_get_unit(oma_radar_unit)
   open(unit=oma_radar_unit,file=trim(filename),form='formatted',iostat=ios)
   if (ios /= 0) Then
       call da_error(__FILE__,__LINE__, &
         (/"Cannot open oma radar file "//trim(filename)/))
   endif

   if (iv % info(radar)%nlocal  >0 ) then
      num_obs = 0
      do n = 1, iv% info(radar)%nlocal
         if (iv%info(radar)%proc_domain(1,n)) num_obs=num_obs+1
      end do
      if (num_obs > 0) then
         write(oma_radar_unit,'(a20,i8)')'radar', num_obs
         num_obs = 0
         do n = 1, iv % info(radar)%nlocal
            if (iv%info(radar)%proc_domain(1,n)) then
               num_obs = num_obs + 1
               write(oma_radar_unit,'(i8)') iv % info(radar) % levels(n)
               do k = 1, iv % info(radar) % levels(n)
                  if ( .not. use_radar_rhv ) then
                     write(oma_radar_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                        num_obs , k, 'RADAR', &
                        iv % info (radar) % lat(1,n), &  ! Latitude
                        iv % info (radar) % lon(1,n), &  ! Longitude
                        iv % radar(n) % height(k),    &  ! Obs height in m
                        ob%radar(n)%rv(k), &
                        iv%radar(n)%rv(k)%inv,iv%radar(n)%rv(k)%qc,iv%radar(n)%rv(k)%error,&
                        re%radar(n)%rv(k), & ! O, O-B, qc, err, O-A rv
                        ob%radar(n)%rf(k), &
                        iv%radar(n)%rf(k)%inv,iv%radar(n)%rf(k)%qc,iv%radar(n)%rf(k)%error,&
                        re%radar(n)%rf(k),iv%info(radar)%zk(k,n)   ! O, O-B, qc, err, O-A rf
                  else ! use_radar_rhv
                     if ( use_radar_rqv ) then
                        write(oma_radar_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                           num_obs , k, 'RADAR', &
                           iv % info (radar) % lat(1,n), &  ! Latitude
                           iv % info (radar) % lon(1,n), &  ! Longitude
                           iv % radar(n) % height(k),    &  ! Obs height in m
                           ob%radar(n)%rv(k),   &  ! radial velocity
                           iv%radar(n)%rv(k)%inv,iv%radar(n)%rv(k)%qc,iv%radar(n)%rv(k)%error,&
                           re%radar(n)%rv(k),   &  ! O, O-B, qc, err, O-A rv
                           iv%radar(n)%rrno(k), &  ! retrieved rain water
                           iv%radar(n)%rrn(k)%inv,iv%radar(n)%rrn(k)%qc,iv%radar(n)%rrn(k)%error,&
                           re%radar(n)%rrn(k),  &  ! O, O-B, qc, err, O-A rrn
                           iv%radar(n)%rsno(k), &  ! retrieved snow
                           iv%radar(n)%rsn(k)%inv,iv%radar(n)%rsn(k)%qc,iv%radar(n)%rsn(k)%error,&
                           re%radar(n)%rsn(k),  &  ! O, O-B, qc, err, O-A rsn
                           iv%radar(n)%rgro(k), &  ! retrieved graupel
                           iv%radar(n)%rgr(k)%inv,iv%radar(n)%rgr(k)%qc,iv%radar(n)%rgr(k)%error,&
                           re%radar(n)%rgr(k),  &  ! O, O-B, qc, err, O-A rgr
                           iv%radar(n)%rqvo(k), &  ! retrieved water vapor
                           iv%radar(n)%rqv(k)%inv,iv%radar(n)%rqv(k)%qc,iv%radar(n)%rqv(k)%error,&
                           re%radar(n)%rqv(k)      ! O, O-B, qc, err, O-A rqv
                     else ! not use_radar_rqv
                        write(oma_radar_unit,'(2i8,a5,2f9.2,f17.7,4(2f17.7,i8,2f17.7))')&
                           num_obs , k, 'RADAR', &
                           iv % info (radar) % lat(1,n), &  ! Latitude
                           iv % info (radar) % lon(1,n), &  ! Longitude
                           iv % radar(n) % height(k),    &  ! Obs height in m
                           ob%radar(n)%rv(k),   &  ! radial velocity
                           iv%radar(n)%rv(k)%inv,iv%radar(n)%rv(k)%qc,iv%radar(n)%rv(k)%error,&
                           re%radar(n)%rv(k),   &  ! O, O-B, qc, err, O-A rv
                           iv%radar(n)%rrno(k), &  ! retrieved rain water
                           iv%radar(n)%rrn(k)%inv,iv%radar(n)%rrn(k)%qc,iv%radar(n)%rrn(k)%error,&
                           re%radar(n)%rrn(k),  &  ! O, O-B, qc, err, O-A rrn
                           iv%radar(n)%rsno(k), &  ! retrieved snow
                           iv%radar(n)%rsn(k)%inv,iv%radar(n)%rsn(k)%qc,iv%radar(n)%rsn(k)%error,&
                           re%radar(n)%rsn(k),  &  ! O, O-B, qc, err, O-A rsn
                           iv%radar(n)%rgro(k), &  ! retrieved graupel
                           iv%radar(n)%rgr(k)%inv,iv%radar(n)%rgr(k)%qc,iv%radar(n)%rgr(k)%error,&
                           re%radar(n)%rgr(k)      ! O, O-B, qc, err, O-A rgr
                     end if ! check use_radar_rqv
                  end if ! check use_radar_rhv
               end do ! leve loop
            end if ! check proc_domain
         end do ! nlocal loop
      end if ! check num_obs
   end if ! check nlocal

#ifdef DM_PARALLEL
   ! Wait to ensure all temporary files have been written
   call mpi_barrier(comm, ierr)
#endif

   close (oma_radar_unit)
   call da_free_unit(oma_radar_unit)

   IF (rootproc) THEN
      call da_get_unit(omb_radar_unit)
      allocate (filename1(0:num_procs-1))
      do k = 0,num_procs-1
         write(unit=filename1(k),fmt ='(a,i2.2,a,i4.4)')'radar_omb_oma_',it,'.',k
      end do
      call da_get_unit(omb_radar_iter_unit)
      write(unit=outfname,fmt ='(a,i2.2)')'radar_omb_oma_',it
      open(omb_radar_iter_unit,file=trim(outfname),form='formatted', status='replace', iostat=ios)
      if (ios /= 0) call da_error(__FILE__,__LINE__, &
          (/"Cannot open file "//trim(outfname)/))
   ENDIF

   num_obs = 0
   IF (iv % info(radar)%nlocal  >0 ) then
      do n = 1, iv% info(radar)%nlocal
         if (iv%info(radar)%proc_domain(1,n)) num_obs=num_obs+1
      end do
   ENDIF
   call da_proc_sum_int(num_obs)
   IF (num_obs > 0 .and. rootproc) then
      write(omb_radar_iter_unit,'(a20,i8)')'radar', num_obs
      num_obs = 0
      num = 0
      do k = 0,num_procs-1
         open(omb_radar_unit,file=trim(filename1(k)),status='old',iostat=ios)
         if (ios /= 0) call da_error(__FILE__,__LINE__, &
                                       (/"Cannot open file "//trim(filename1(k))/))
         read(omb_radar_unit, '(20x,i8)', iostat=ios)num_obs
         IF(ios /= 0)THEN
            write(unit=message(1),fmt='(A,A)') 'Nothing to read from ',filename1(k)
            call da_message(message(1:1))
            cycle
         ENDIF
         if (num_obs > 0) then
            do n = 1, num_obs
               read(omb_radar_unit,'(i8)') levels
               write(omb_radar_iter_unit,'(i8)') levels
               num = num + 1
               do m = 1, levels
                  if ( .not. use_radar_rhv ) then
                     read(omb_radar_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                         kk,l, stn_id,     &  ! Station
                         lat, lon, height, &  ! Lat/lon, height
                         rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                         rf_obs, rf_inv, rf_qc, rf_error, rf_inc, zk
                     write(omb_radar_iter_unit,'(2i8,a5,2f9.2,f17.7,2(2f17.7,i8,2f17.7),f17.7)')&
                         num,m,stn_id,     &  ! Station
                         lat, lon, height, &  ! Lat/lon, height
                         rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                         rf_obs, rf_inv, rf_qc, rf_error, rf_inc, zk
                  else ! use_radar_rhv
                     if ( use_radar_rqv ) then
                        read(omb_radar_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                            kk,l, stn_id,     &  ! Station
                            lat, lon, height, &  ! Lat/lon, height
                            rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                            rn_obs, rn_inv, rn_qc, rn_error, rn_inc, &
                            sn_obs, sn_inv, sn_qc, sn_error, sn_inc, &
                            gr_obs, gr_inv, gr_qc, gr_error, gr_inc, &
                            qv_obs, qv_inv, qv_qc, qv_error, qv_inc
                        write(omb_radar_iter_unit,'(2i8,a5,2f9.2,f17.7,5(2f17.7,i8,2f17.7))')&
                            num,m,stn_id,     &  ! Station
                            lat, lon, height, &  ! Lat/lon, height
                            rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                            rn_obs, rn_inv, rn_qc, rn_error, rn_inc, &
                            sn_obs, sn_inv, sn_qc, sn_error, sn_inc, &
                            gr_obs, gr_inv, gr_qc, gr_error, gr_inc, &
                            qv_obs, qv_inv, qv_qc, qv_error, qv_inc
                     else ! not use_radar_rqv
                        read(omb_radar_unit,'(2i8,a5,2f9.2,f17.7,4(2f17.7,i8,2f17.7))')&
                            kk,l, stn_id,     &  ! Station
                            lat, lon, height, &  ! Lat/lon, height
                            rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                            rn_obs, rn_inv, rn_qc, rn_error, rn_inc, &
                            sn_obs, sn_inv, sn_qc, sn_error, sn_inc, &
                            gr_obs, gr_inv, gr_qc, gr_error, gr_inc
                        write(omb_radar_iter_unit,'(2i8,a5,2f9.2,f17.7,4(2f17.7,i8,2f17.7))')&
                            num,m,stn_id,     &  ! Station
                            lat, lon, height, &  ! Lat/lon, height
                            rv_obs, rv_inv, rv_qc, rv_error, rv_inc, &
                            rn_obs, rn_inv, rn_qc, rn_error, rn_inc, &
                            sn_obs, sn_inv, sn_qc, sn_error, sn_inc, &
                            gr_obs, gr_inv, gr_qc, gr_error, gr_inc
                     end if ! check use_radar_rqv
                  end if ! check use_radar_rhv
               end do ! level loop
            end do ! num_obs loop
         end if ! check num_obs
      end do ! num_proc loop
   END IF

   if (rootproc) then
      close(omb_radar_unit)
      close(omb_radar_iter_unit)
      call da_free_unit(omb_radar_unit)
      call da_free_unit(omb_radar_iter_unit)
      deallocate (filename1)
   end if

   if (trace_use) call da_trace_exit("da_write_oa_radar_ascii")

end subroutine da_write_oa_radar_ascii
