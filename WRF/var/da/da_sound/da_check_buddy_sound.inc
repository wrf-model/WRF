subroutine da_check_buddy_sound(iv, dx, it)

   !-----------------------------------------------------------------------
   ! Purpose: Buddy check for SOUND observations.
   !
   ! For SOUND, the binning procedure should be completed before going
   ! into the da_buddy_qc. 
   !
   !                       Yong-Run Guo, 10/10/2008
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   integer,       intent(in)    :: it      ! Outer iteration
   real,          intent(in)    :: dx

   integer :: k, n, bin, i, j, m_max, m_max_g, kk, nn, numobs, ierr
   real    :: dx_km, Press_mb

   integer, parameter               :: num_bins_p = 13
   real, parameter                  :: bin_width_p = 10000.0
   real, parameter                  :: bottom_pressure = 100000.0
   real   , dimension(0:num_bins_p) :: bin_start_p, pressure, bin_width
   integer, dimension(0:num_bins_p) :: num
!
   integer, allocatable, dimension(:,:) :: n_iv, k_iv

   integer,          allocatable, dimension(:,:,:) :: qc_flag_small
   real,             allocatable, dimension(:,:) :: xob, yob
   real,             allocatable, dimension(:,:,:) :: obs
   character(len=5), allocatable, dimension(:,:) :: station_id
#ifdef DM_PARALLEL
   integer,          allocatable, dimension(:,:,:,:) :: qc_flag_small_g
   real,             allocatable, dimension(:,:,:) :: xob_g, yob_g
   real,             allocatable, dimension(:,:,:,:) :: obs_g
   integer,                       dimension(0:num_bins_p,0:num_procs-1) :: num_recv
#endif
!-----------------------------------------------------------------------------
   
   if (trace_use_dull) call da_trace_entry("da_check_buddy_sound")

   !--------------------------------------------------------------------------- 
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_buddy_print) then
      write (check_buddy_unit,'(/A)')  &
         '================================================================'
      write (unit = check_buddy_unit, fmt = '(A,i4,A,i4/)') &
            'SOUND BUDDY TEST QC:  no_buddies_qc=',no_buddies,&
            '  fails_buddy_check_qc=',fails_buddy_check
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Bin the data vertically based on the obs p::
   !---------------------------------------------------------------------------

!   print*,'==> Sound Buddy check: num_bins_p = ',num_bins_p
   dx_km = dx / 1000.0
!  
   bin_start_p(0) = bottom_pressure
   pressure   (0) = bin_start_p(0)
   bin_width      (0) = 0.0     
   do n = 1, num_bins_p
      bin_start_p(n) = bin_start_p(n-1) - bin_width(n-1)
      if (bin_start_p(n) > 30000.0) then
         bin_width(n) = bin_width_p
      else
         bin_width(n) = bin_width_p / 2.0
      endif
      pressure(n) = bin_start_p(n) - bin_width(n)/2.0
   enddo
   bin_start_p(0) = bottom_pressure + 10.0

!   print '(I3,2x,"start_p=",f10.1," mid-pressure=",f10.1," width=",f10.1)', &
!        (n, bin_start_p(n), pressure(n), bin_width(n), n=0, num_bins_p)
!
! 2.1 Get the maximum dimension for all the bins:
!
   num = 0
   do n = iv%info(sound)%n1,iv%info(sound)%n2
      do k =  1, iv%info(sound)%levels(n)
         if (iv%sound(n)%p(k) > missing_r) then
           do i = 0, num_bins_p - 1
              if (iv%sound(n)%p(k) <= bin_start_p(i) .and. &
                  iv%sound(n)%p(k) >  bin_start_p(i+1) ) then
                 bin = i
                 exit
              endif
           enddo
!           bin = int( (bottom_pressure - iv%sound(n)%p(k))/bin_width(n) ) + 1
           if (iv%sound(n)%p(k) > bottom_pressure) bin = 0
           if (iv%sound(n)%p(k) <=  bin_start_p(num_bins_p)) bin = num_bins_p
           num(bin) = num(bin) + 1
         endif
      enddo
   enddo
   m_max = maxval(num)
!   print *,(i,num(i),i=0,num_bins_p)
!   print *,"m_max=", m_max
!
! 2.2 Save the location indices (n,k) for each of bins:
!
!   print '("Sound n1=",i5,"  n2=",i5)',iv%info(sound)%n1,iv%info(sound)%n2
   allocate ( n_iv( 0: num_bins_p,1:m_max+10 ) )
   allocate ( k_iv( 0: num_bins_p,1:m_max+10 ) )

   num = 0
   do n = iv%info(sound)%n1,iv%info(sound)%n2
      do k =  1, iv%info(sound)%levels(n)
         if (iv%sound(n)%p(k) > missing_r) then
           do i = 0, num_bins_p - 1
              if (iv%sound(n)%p(k) <= bin_start_p(i) .and. &
                  iv%sound(n)%p(k) >  bin_start_p(i+1) ) then
                 bin = i
                 exit
              endif
           enddo
!           bin = int( (bottom_pressure - iv%sound(n)%p(k))/bin_width(n) ) + 1
           if (iv%sound(n)%p(k) > bottom_pressure) bin = 0
           if (iv%sound(n)%p(k) <=  bin_start_p(num_bins_p)) bin = num_bins_p

           num(bin) = num(bin) + 1
           n_iv(bin,num(bin)) = n
           k_iv(bin,num(bin)) = k

         endif
      enddo
   end do
!
! 2.3 Print out the binned results:
!
!   do i = 0, num_bins_p
!      print '("bin:",I2,"  start_p=",f8.1," num=",i5)', &
!                      i, bin_start_p(i), num(i)
!      do j = 1, num(i)
!         n = n_iv(i,j)
!         k = k_iv(i,j)
!         print '("j, n, k:",3i5,2x,"p=",f10.1)', &
!                  j, n, k, iv%sound(n)%p(k)
!      enddo
!   enddo
   !---------------------------------------------------------------------------
   ! [3.0] Buddy check for each of the pressure-bins::
   !---------------------------------------------------------------------------

#ifdef DM_PARALLEL
   call mpi_allgather(num, num_bins_p+1, mpi_integer, num_recv, num_bins_p+1, mpi_integer, comm, ierr)
   call mpi_allreduce(m_max, m_max_g, 1, mpi_integer, mpi_max, comm, ierr)
   m_max = m_max_g
   allocate(xob_g(1:m_max,0:num_bins_p,0:num_procs-1))
   allocate(yob_g(1:m_max,0:num_bins_p,0:num_procs-1))
   allocate(obs_g(1:m_max,4,0:num_bins_p,0:num_procs-1))
   allocate(qc_flag_small_g(1:m_max,4,0:num_bins_p,0:num_procs-1))
#endif

   allocate(xob(1:m_max,0:num_bins_p))
   allocate(yob(1:m_max,0:num_bins_p))
   allocate(obs(1:m_max,4,0:num_bins_p))
   allocate(qc_flag_small(1:m_max,4,0:num_bins_p))
   allocate(station_id   (1:m_max,0:num_bins_p))

   obs = 0.0
   qc_flag_small = missing
#ifdef DM_PARALLEL
   obs_g = 0.0
   qc_flag_small_g = missing
#endif

   do i = 0, num_bins_p
      
      numobs = num(i)
 
      do n = 1, numobs
         nn = n_iv(i,n)
         kk = k_iv(i,n)
 
         station_id(n,i)          = iv%info(sound)%id(nn)
         xob(n,i)                 = iv%info(sound)%x(1,nn)
         yob(n,i)                 = iv%info(sound)%y(1,nn)

         obs(n,1,i)               = iv%sound(nn)%u(kk)%inv
         qc_flag_small(n,1,i)     = iv%sound(nn)%u(kk)%qc
         obs(n,2,i)               = iv%sound(nn)%v(kk)%inv
         qc_flag_small(n,2,i)     = iv%sound(nn)%v(kk)%qc
         obs(n,3,i)               = iv%sound(nn)%t(kk)%inv
         qc_flag_small(n,3,i)     = iv%sound(nn)%t(kk)%qc
         obs(n,4,i)               = iv%sound(nn)%q(kk)%inv
         qc_flag_small(n,4,i)     = iv%sound(nn)%q(kk)%qc
      enddo

   enddo

#ifdef DM_PARALLEL
   call mpi_allgather (xob, m_max*(num_bins_p+1), mpi_real8, xob_g, m_max*(num_bins_p+1), mpi_real8, comm, ierr)
   call mpi_allgather (yob, m_max*(num_bins_p+1), mpi_real8, yob_g, m_max*(num_bins_p+1), mpi_real8, comm, ierr)
   call mpi_allgather (obs, 4*m_max*(num_bins_p+1), mpi_real8, obs_g, 4*m_max*(num_bins_p+1), mpi_real8, comm, ierr)
   call mpi_allgather (qc_flag_small, 4*m_max*(num_bins_p+1), mpi_integer, qc_flag_small_g, 4*m_max*(num_bins_p+1), &
                        mpi_integer, comm, ierr)
#endif

   do i = 0, num_bins_p

     if ( num(i) <= 1 ) cycle


! 3.1 Get the Station locations:
   
     ! Pressure level:
     Press_mb = pressure(i) / 100.0
     numobs = num(i)

     if (rootproc .and. check_buddy_print) then
         write (check_buddy_unit,'(5X,A)')  &
         '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      write (unit = check_buddy_unit, fmt = '(5X,A,I3,2X,A,I6)') &
             'BIN =', i, 'NUMOBS =', numobs
     end if


! 3.2 U-component buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'UU      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_uv=', max_buddy_uv 

#ifdef DM_PARALLEL
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,1,i), qc_flag_small(:,1,i), &
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print,  xob_g(:,i,:), &
                       yob_g(:,i,:), obs_g(:,1,i,:), qc_flag_small_g(:,1,i,:),num_recv(i,:))
#else
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,1,i), qc_flag_small(:,1,i),&
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print )
#endif

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        kk = k_iv(i,n)
        iv%sound(nn)%u(kk)%qc = qc_flag_small(n,1,i)
     enddo

! 3.2 V-component buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'VV      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_uv=', max_buddy_uv
 
#ifdef DM_PARALLEL
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,2,i), qc_flag_small(:,2,i), &
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print,  xob_g(:,i,:), &
                       yob_g(:,i,:), obs_g(:,2,i,:), qc_flag_small_g(:,2,i,:),num_recv(i,:))
#else
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,2,i), qc_flag_small(:,2,i),&
                       'VV      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print )
#endif

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        kk = k_iv(i,n)
        iv%sound(nn)%v(kk)%qc = qc_flag_small(n,2,i)
     enddo

! 3.3 Temperature buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'TT      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_t=', max_buddy_t 

#ifdef DM_PARALLEL
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,3,i), qc_flag_small(:,3,i), &
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print,  xob_g(:,i,:), &
                       yob_g(:,i,:), obs_g(:,3,i,:), qc_flag_small_g(:,3,i,:),num_recv(i,:))
#else
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,3,i), qc_flag_small(:,3,i),&
                       'TT      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_t , check_buddy_unit, check_buddy_print )
#endif

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        kk = k_iv(i,n)
        iv%sound(nn)%t(kk)%qc = qc_flag_small(n,3,i)
     enddo

! 3.3 Specific humidity buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'QQ      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_rh=', max_buddy_rh 

#ifdef DM_PARALLEL
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,4,i), qc_flag_small(:,4,i), &
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print,  xob_g(:,i,:), &
                       yob_g(:,i,:), obs_g(:,4,i,:), qc_flag_small_g(:,4,i,:),num_recv(i,:))
#else
     call da_buddy_qc (numobs, m_max, station_id(:,i), xob(:,i), yob(:,i), obs(:,4,i), qc_flag_small(:,4,i),&
                       'QQ      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_rh , check_buddy_unit, check_buddy_print )
#endif

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        kk = k_iv(i,n)
        iv%sound(nn)%q(kk)%qc = qc_flag_small(n,4,i)
     enddo

! 3.4 Deallocate arrays

   enddo

#ifdef DM_PARALLEL
   deallocate(xob_g)
   deallocate(yob_g)
   deallocate(obs_g)
   deallocate(qc_flag_small_g)
#endif
   deallocate(xob)
   deallocate(yob)
   deallocate(obs)
   deallocate(qc_flag_small)
   deallocate(station_id   )

   deallocate ( n_iv )
   deallocate ( k_iv )

   if (trace_use_dull) call da_trace_exit("da_check_buddy_sound")

end subroutine da_check_buddy_sound
