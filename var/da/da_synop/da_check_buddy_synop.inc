subroutine da_check_buddy_synop(iv, ob, dx, it)

   !-----------------------------------------------------------------------
   ! Purpose: Buddy check for SYNOP observations.
   !
   ! For SYNOP, there may not need the binning procedure before going
   ! into the da_buddy_qc. So  bottom_pressure = 30000.0 nad num_bins_p = 1.
   ! If you want to do binning, minor modifications needed.
   !
   !                       Yong-Run Guo, 10/10/2008
   !-----------------------------------------------------------------------

   implicit none

   type(iv_type), intent(inout) :: iv
   type(y_type),  intent(in)    :: ob      ! Observation structure
   integer,       intent(in)    :: it      ! Outer iteration
   real,          intent(in)    :: dx

   integer :: k, n, bin, i, j, m_max, kk, nn, numobs
   real    :: dx_km, Press_mb

! All data in one bin:
   integer, parameter               :: num_bins_p = 1
   real, parameter                  :: bottom_pressure = 30000.0
! 
! Total of 13 bins used:
!   integer, parameter               :: num_bins_p = 13
!   real, parameter                  :: bottom_pressure = 100000.0

   real, parameter                  :: bin_width_p = 10000.0
   real   , dimension(0:num_bins_p) :: bin_start_p, pressure, bin_width
   integer, dimension(0:num_bins_p) :: num
!
   integer, allocatable, dimension(:,:) :: n_iv

   integer,          allocatable, dimension(:) :: qc_flag_small
   real,             allocatable, dimension(:) :: xob, yob, obs
   character(len=5), allocatable, dimension(:) :: station_id
!-----------------------------------------------------------------------------
   
!   if (trace_use_dull) call da_trace_entry("da_check_buddy_synop")

   !--------------------------------------------------------------------------- 
   ! [1.0] Open diagnostic file:
   !---------------------------------------------------------------------------

   if (rootproc .and. check_buddy_print) then
      write (check_buddy_unit,'(/A)')  &
         '================================================================'
      write (unit = check_buddy_unit, fmt = '(A,i4,A,i4/)') &
            'SYNOP BUDDY TEST QC:  no_buddies_qc=',no_buddies,&
            '  fails_buddy_check_qc=',fails_buddy_check
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Bin the data vertically based on the obs p::
   !---------------------------------------------------------------------------

!   print*,'==> Synop Buddy check: num_bins_p = ',num_bins_p
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
!
! Only 1 bin=0 used, if you want to do the normal binning, comment out 
! the line below:
   pressure   (0) = 100000.0

!   print '(I3,2x,"start_p=",f10.1," mid-pressure=",f10.1," width=",f10.1)', &
!        (n, bin_start_p(n), pressure(n), bin_width(n), n=0, num_bins_p)
!
! 2.1 Get the maximum dimension for all the bins:
!
   num = 0
   do n = iv%info(synop)%n1,iv%info(synop)%n2
         if (ob%synop(n)%p > missing_r) then
           do i = 0, num_bins_p - 1
              if (iv%synop(n)%p%qc >=0 .and.             &
                  (ob%synop(n)%p <= bin_start_p(i) .and. &
                   ob%synop(n)%p >  bin_start_p(i+1)) ) then
                 bin = i
                 exit
              endif
           enddo
!           bin = int( (bottom_pressure - ob%synop(n)%p)/bin_width(n) ) + 1
           if (ob%synop(n)%p > bottom_pressure) bin = 0
           if (ob%synop(n)%p <=  bin_start_p(num_bins_p)) bin = num_bins_p
           num(bin) = num(bin) + 1
         endif
   enddo
   m_max = maxval(num)
!   print *,(i,num(i),i=0,num_bins_p)
!   print *,"m_max=", m_max
!
! 2.2 Save the location indices (n,k) for each of bins:
!
!   print '("Synop n1=",i5,"  n2=",i5)',iv%info(synop)%n1,iv%info(synop)%n2
   allocate ( n_iv( 0: num_bins_p,1:m_max+10 ) )

   num = 0
   do n = iv%info(synop)%n1,iv%info(synop)%n2
         if (ob%synop(n)%p > missing_r) then
           do i = 0, num_bins_p - 1
              if (iv%synop(n)%p%qc >=0 .and.             &
                  (ob%synop(n)%p <= bin_start_p(i) .and. &
                   ob%synop(n)%p >  bin_start_p(i+1)) ) then
                 bin = i
                 exit
              endif
           enddo
!           bin = int( (bottom_pressure - ob%synop(n)%p)/bin_width(n) ) + 1
           if (ob%synop(n)%p > bottom_pressure) bin = 0
           if (ob%synop(n)%p <=  bin_start_p(num_bins_p)) bin = num_bins_p

           num(bin) = num(bin) + 1
           n_iv(bin,num(bin)) = n

         endif
   end do
!
! 2.3 Print out the binned results:
!
!   do i = 0, num_bins_p
!      print '("bin:",I2,"  start_p=",f8.1," num=",i5)', &
!                      i, bin_start_p(i), num(i)
!      do j = 1, num(i)
!         n = n_iv(i,j)
!         print '("j, n:",2i5,2x,"p=",f10.1)', &
!                  j, n, ob%synop(n)%p
!      enddo
!   enddo
   !---------------------------------------------------------------------------
   ! [3.0] Buddy check for each of the pressure-bins::
   !---------------------------------------------------------------------------

   do i = 0, num_bins_p

     if (num(i) <= 1) cycle

! 3.1 Get the Station locations:
   
     ! Pressure level:
     Press_mb = pressure(i) / 100.0
     numobs = num(i)

     allocate(xob(1:numobs))
     allocate(yob(1:numobs))
     allocate(obs(1:numobs))
     allocate(qc_flag_small(1:numobs))
     allocate(station_id   (1:numobs))

     if (rootproc .and. check_buddy_print) then
         write (check_buddy_unit,'(5X,A)')  &
         '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
      write (unit = check_buddy_unit, fmt = '(5X,A,I3,2X,A,I6)') &
             'BIN =', i, 'NUMOBS =', numobs
     end if
!     print *,'SYNOP: BIN=', i, '  numobs=',num(i)

     ! Station locations

     do n = 1, numobs
        nn = n_iv(i,n)
!
        station_id(n)        = iv%info(synop)%id(nn)
        xob(n)               = iv%info(synop)%x(1,nn)
        yob(n)               = iv%info(synop)%y(1,nn)
     enddo
 
! 3.2 U-component buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'UU      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_uv=', max_buddy_uv 

     obs = 0.0
     qc_flag_small(n) = missing
     do n = 1, numobs
        nn = n_iv(i,n)
        obs(n)               = iv%synop(nn)%u%inv
        qc_flag_small(n)     = iv%synop(nn)%u%qc
     enddo

     call da_buddy_qc (numobs, m_max, station_id, xob, yob, obs, qc_flag_small,&
                       'UU      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print )

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        iv%synop(nn)%u%qc = qc_flag_small(n)
     enddo

! 3.2 V-component buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'VV      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_uv=', max_buddy_uv
 

     obs = 0.0
     qc_flag_small(n) = missing
     do n = 1, numobs
        nn = n_iv(i,n)
        obs(n)               = iv%synop(nn)%v%inv
        qc_flag_small(n)     = iv%synop(nn)%v%qc
     enddo

     call da_buddy_qc (numobs, m_max, station_id, xob, yob, obs, qc_flag_small,&
                       'VV      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_uv , check_buddy_unit, check_buddy_print )

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        iv%synop(nn)%v%qc = qc_flag_small(n)
     enddo

! 3.3 Temperature buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'TT      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_t=', max_buddy_t 

     obs = 0.0
     qc_flag_small(n) = missing
     do n = 1, numobs
        nn = n_iv(i,n)
        obs(n)               = iv%synop(nn)%t%inv
        qc_flag_small(n)     = iv%synop(nn)%t%qc
     enddo

     call da_buddy_qc (numobs, m_max, station_id, xob, yob, obs, qc_flag_small,&
                       'TT      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_t , check_buddy_unit, check_buddy_print )

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        iv%synop(nn)%t%qc = qc_flag_small(n)
     enddo

! 3.3 Specific humidity buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'QQ      ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_rh=', max_buddy_rh 

     obs = 0.0
     qc_flag_small(n) = missing
     do n = 1, numobs
        nn = n_iv(i,n)
        obs(n)               = iv%synop(nn)%q%inv
        qc_flag_small(n)     = iv%synop(nn)%q%qc
     enddo

     call da_buddy_qc (numobs, m_max, station_id, xob, yob, obs, qc_flag_small,&
                       'QQ      ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_rh , check_buddy_unit, check_buddy_print )

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        iv%synop(nn)%q%qc = qc_flag_small(n)
     enddo

! 3.4 Pressure buddy check:

     if (rootproc .and. check_buddy_print) &
         write (check_buddy_unit,'(8X,A,A,f10.1,3(A,f6.1))')  &
                'PMSL    ', ' Pressure(mb)=',Press_mb, ' ds(km)=',dx_km,&
                '  buddy_weight=', buddy_weight , &
                '  max_buddy_p=', max_buddy_p 

     obs = 0.0
     qc_flag_small(n) = missing
     do n = 1, numobs
        nn = n_iv(i,n)
        obs(n)               = iv%synop(nn)%p%inv
        qc_flag_small(n)     = iv%synop(nn)%p%qc
     enddo

     call da_buddy_qc (numobs, m_max, station_id, xob, yob, obs, qc_flag_small,&
                       'PMSL    ', Press_mb, dx_km, buddy_weight , &
                       max_buddy_p , check_buddy_unit, check_buddy_print )

   !  Put the qc_flag back into the permanent space.
   
     do n = 1, numobs
        nn = n_iv(i,n)
        iv%synop(nn)%p%qc = qc_flag_small(n)
     enddo

! 3.5 Deallocate arrays

1234 continue

     deallocate(xob)
     deallocate(yob)
     deallocate(obs)
     deallocate(qc_flag_small)
     deallocate(station_id   )

   enddo

   deallocate ( n_iv )

   if (trace_use_dull) call da_trace_exit("da_check_buddy_synop")

end subroutine da_check_buddy_synop
