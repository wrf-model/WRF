program gen_be_ep1

   use da_control, only : stderr,stdout, filename_len
   use da_gen_be, only : da_filter_regcoeffs
   use da_tools_serial, only : da_get_unit, da_free_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk, nkdum          ! Grid dimensions.
   integer             :: i, j, k, member            ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (2D fields).
   integer             :: num_passes                 ! Recursive filter passes.
   integer             :: count                      ! Counter.
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: rf_scale                   ! Recursive filter scale.
   real                :: count_inv                  ! 1 / count.

   real, allocatable   :: psi(:,:,:)                 ! psi.
   real, allocatable   :: chi(:,:,:)                 ! chi.
   real, allocatable   :: temp(:,:,:)                ! Temperature.
   real, allocatable   :: rh(:,:,:)                  ! Relative humidity.
   real, allocatable   :: ps(:,:)                    ! Surface pressure.
   real, allocatable   :: chi_u(:,:,:)               ! chi.
   real, allocatable   :: temp_u(:,:,:)              ! Temperature.
   real, allocatable   :: ps_u(:,:)                  ! Surface pressure.

   real, allocatable   :: psi_mnsq(:,:,:)            ! psi.
   real, allocatable   :: chi_mnsq(:,:,:)    ! chi.
   real, allocatable   :: temp_mnsq(:,:,:)   ! Temperature.
   real, allocatable   :: rh_mnsq(:,:,:)     ! Relative humidity.
   real, allocatable   :: ps_mnsq(:,:)       ! Surface pressure.
   real, allocatable   :: chi_u_mnsq(:,:,:)  ! chi.
   real, allocatable   :: temp_u_mnsq(:,:,:) ! Temperature.
   real, allocatable   :: ps_u_mnsq(:,:)     ! Surface pressure.

   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point.

   real, allocatable   :: regcoeff1(:)       ! psi/chi regression cooefficient.
   real, allocatable   :: regcoeff2(:,:)     ! psi/ps regression cooefficient.
   real, allocatable   :: regcoeff3(:,:,:)   ! psi/T regression cooefficient.

   namelist / gen_be_stage2a_nl / start_date, end_date, interval, &
                                  ne, num_passes, rf_scale 

   integer :: ounit, gen_be_ounit, namelist_unit, iunit

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(gen_be_ounit)
   call da_get_unit(namelist_unit)

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   ne = 1
   num_passes = 0
   rf_scale = 1.0

   open(unit=namelist_unit, file='gen_be_stage2a_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage2a_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing control variable fields'
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read regression coefficients and bin information:'
!--------------------------------------------------------------------------------------------- 

   filename = 'be.dat'
   open (iunit, file = filename, form='unformatted')

   read(iunit)ni, nj, nk
   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )

   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)
   read(iunit)regcoeff1
   read(iunit)regcoeff2
   read(iunit)regcoeff3

   close(iunit)

   allocate( psi(1:ni,1:nj,1:nk) )
   allocate( chi(1:ni,1:nj,1:nk) )
   allocate( temp(1:ni,1:nj,1:nk) )
   allocate( rh(1:ni,1:nj,1:nk) )
   allocate( ps(1:ni,1:nj) )
   allocate( chi_u(1:ni,1:nj,1:nk) )
   allocate( temp_u(1:ni,1:nj,1:nk) )
   allocate( ps_u(1:ni,1:nj) )

   if ( num_passes > 0 ) then

      write(6,'(a,i4,a)')' [3] Apply ', num_passes, ' pass recursive filter to regression coefficients:'
      call da_filter_regcoeffs( ni, nj, nk, num_bins, num_bins2d, num_passes, rf_scale, bin, &
                                regcoeff1, regcoeff2, regcoeff3 )
   else
      write(6,'(a)')' [3] num_passes = 0. Bypassing recursive filtering.'
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [4] Read standard fields, and compute unbalanced control variable fields:'
!---------------------------------------------------------------------------------------------

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      write(6,'(a,a)')'    Calculating unbalanced fields for date ', date

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read psi predictor:
         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

!        Calculate unbalanced chi:
         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi
         close(iunit)

         do k = 1, nk
            do j = 1, nj
               do i = 1, ni
                  b = bin(i,j,k)
                  chi_u(i,j,k) = chi(i,j,k) - regcoeff1(b) * psi(i,j,k)
               end do
            end do
         end do

         variable = 'chi_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)chi_u
         close(ounit)

!        Calculate unbalanced T:
         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp
         close(iunit)

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               do k = 1, nk
                  temp_u(i,j,k) = temp(i,j,k) - SUM(regcoeff3(k,1:nk,b) * psi(i,j,1:nk))
               end do
            end do
         end do

         variable = 't_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, nk
         write(ounit)temp_u
         close(ounit)

!        Calculate unbalanced ps:
         variable = 'ps'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ps
         close(iunit)

         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               ps_u(i,j) = ps(i,j) - SUM(regcoeff2(1:nk,b) * psi(i,j,1:nk))
            end do
         end do

         variable = 'ps_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (ounit, file = filename, form='unformatted')
         write(ounit)ni, nj, 1
         write(ounit)ps_u
         close(ounit)

      end do  ! End loop over ensemble members.

!--------------------------------------------------------------------------------------
!     Calculate mean control variables (diagnostics): 
!--------------------------------------------------------------------------------------

!     Read psi predictor:
      variable = 'psi'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (iunit, file = filename, form='unformatted')
      read(iunit)ni, nj, nk
      read(iunit)psi
      close(iunit)

!     Calculate unbalanced chi:
      variable = 'chi'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (iunit, file = filename, form='unformatted')
      read(iunit)ni, nj, nk
      read(iunit)chi
      close(iunit)

      do k = 1, nk
         do j = 1, nj
            do i = 1, ni
               b = bin(i,j,k)
               chi_u(i,j,k) = chi(i,j,k) - regcoeff1(b) * psi(i,j,k)
            end do
         end do
      end do

      variable = 'chi_u'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (ounit, file = filename, form='unformatted')
      write(ounit)ni, nj, nk
      write(ounit)chi_u
      close(ounit)

!     Calculate unbalanced T:
      variable = 't'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (iunit, file = filename, form='unformatted')
      read(iunit)ni, nj, nk
      read(iunit)temp
      close(iunit)

      do j = 1, nj
         do i = 1, ni
            b = bin2d(i,j)
            do k = 1, nk
               temp_u(i,j,k) = temp(i,j,k) - SUM(regcoeff3(k,1:nk,b) * psi(i,j,1:nk))
            end do
         end do
      end do

      variable = 't_u'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (ounit, file = filename, form='unformatted')
      write(ounit)ni, nj, nk
      write(ounit)temp_u
      close(ounit)

!     Calculate unbalanced ps:
      variable = 'ps'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (iunit, file = filename, form='unformatted')
      read(iunit)ni, nj, nkdum
      read(iunit)ps
      close(iunit)

      do j = 1, nj
         do i = 1, ni
            b = bin2d(i,j)
            ps_u(i,j) = ps(i,j) - SUM(regcoeff2(1:nk,b) * psi(i,j,1:nk))
         end do
      end do

      variable = 'ps_u'
      filename = trim(variable)//'/'//date(1:10)
      filename = trim(filename)//'.'//trim(variable)//'.mean'
      open (ounit, file = filename, form='unformatted')
      write(ounit)ni, nj, 1
      write(ounit)ps_u
      close(ounit)

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   deallocate( bin )
   deallocate( bin2d )
   deallocate( regcoeff1 )
   deallocate( regcoeff2 )
   deallocate( regcoeff3 )

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [5] Compute mean square statistics:'
!---------------------------------------------------------------------------------------------

   allocate( psi_mnsq(1:ni,1:nj,1:nk) )
   allocate( chi_mnsq(1:ni,1:nj,1:nk) )
   allocate( temp_mnsq(1:ni,1:nj,1:nk) )
   allocate( rh_mnsq(1:ni,1:nj,1:nk) )
   allocate( ps_mnsq(1:ni,1:nj) )
   allocate( chi_u_mnsq(1:ni,1:nj,1:nk) )
   allocate( temp_u_mnsq(1:ni,1:nj,1:nk) )
   allocate( ps_u_mnsq(1:ni,1:nj) )

   date = start_date
   cdate = sdate

   do while ( cdate <= edate )
      count = 0

      psi_mnsq = 0.0
      chi_mnsq = 0.0
      temp_mnsq = 0.0
      rh_mnsq = 0.0
      ps_mnsq = 0.0
      chi_u_mnsq = 0.0
      temp_u_mnsq = 0.0
      ps_u_mnsq = 0.0

      do member = 1, ne
         write(ce,'(i3.3)')member
         count = count + 1
         count_inv = 1.0 / real(count)

         variable = 'psi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)psi
         close(iunit)

         variable = 'chi'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi
         close(iunit)

         variable = 't'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp
         close(iunit)

         variable = 'rh'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)rh
         close(iunit)

         variable = 'ps'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ps
         close(iunit)

         variable = 'chi_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)chi_u
         close(iunit)

         variable = 't_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)temp_u
         close(iunit)

         variable = 'ps_u'
         filename = trim(variable)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)ps
         close(iunit)

!        Calculate accumulating mean:
         psi_mnsq = ( real( count-1 ) * psi_mnsq + psi * psi ) * count_inv
         chi_mnsq = ( real( count-1 ) * chi_mnsq + chi * chi ) * count_inv
         temp_mnsq = ( real( count-1 ) * temp_mnsq + temp * temp ) * count_inv
         rh_mnsq = ( real( count-1 ) * rh_mnsq + rh * rh ) * count_inv
         ps_mnsq = ( real( count-1 ) * ps_mnsq + ps * ps ) * count_inv
         chi_u_mnsq = ( real( count-1 ) * chi_u_mnsq + chi_u * chi_u ) * count_inv
         temp_u_mnsq = ( real( count-1 ) * temp_u_mnsq + temp_u * temp_u ) * count_inv
         ps_u_mnsq = ( real( count-1 ) * ps_u_mnsq + ps_u * ps_u ) * count_inv
      end do  ! End loop over ensemble members.

      psi_mnsq = sqrt(psi_mnsq) ! Convert mnsq to stdv (mean=0)
      chi_mnsq = sqrt(chi_mnsq) ! Convert mnsq to stdv (mean=0)
      temp_mnsq = sqrt(temp_mnsq) ! Convert mnsq to stdv (mean=0)
      rh_mnsq = sqrt(rh_mnsq) ! Convert mnsq to stdv (mean=0)
      ps_mnsq = sqrt(ps_mnsq) ! Convert mnsq to stdv (mean=0)
      chi_u_mnsq = sqrt(chi_u_mnsq) ! Convert mnsq to stdv (mean=0)
      temp_u_mnsq = sqrt(temp_u_mnsq) ! Convert mnsq to stdv (mean=0)
      ps_u_mnsq = sqrt(ps_u_mnsq) ! Convert mnsq to stdv (mean=0)

!     Write mean square statistics:
      filename = 'psi/'//date(1:10)//'.psi.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)psi_mnsq
      close(gen_be_ounit)

      filename = 'chi/'//date(1:10)//'.chi.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)chi_mnsq
      close(gen_be_ounit)

      filename = 't/'//date(1:10)//'.t.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)temp_mnsq
      close(gen_be_ounit)

      filename = 'rh/'//date(1:10)//'.rh.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)rh_mnsq
      close(gen_be_ounit)

      filename = 'ps/'//date(1:10)//'.ps.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)ps_mnsq
      close(gen_be_ounit)

      filename = 'chi_u/'//date(1:10)//'.chi_u.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)chi_u_mnsq
      close(gen_be_ounit)

      filename = 't_u/'//date(1:10)//'.t_u.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)temp_u_mnsq
      close(gen_be_ounit)

      filename = 'ps_u/'//date(1:10)//'.ps_u.stdv'
      open (gen_be_ounit, file = filename, form='unformatted')
      write(gen_be_ounit)ni, nj, nk
      write(gen_be_ounit)ps_u_mnsq
      close(gen_be_ounit)

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do     ! End loop over times.

   deallocate( psi )
   deallocate( chi )
   deallocate( temp )
   deallocate( rh )
   deallocate( ps )
   deallocate( chi_u )
   deallocate( temp_u )
   deallocate( ps_u )
   deallocate( psi_mnsq )
   deallocate( chi_mnsq )
   deallocate( temp_mnsq )
   deallocate( rh_mnsq )
   deallocate( ps_mnsq )
   deallocate( chi_u_mnsq )
   deallocate( temp_u_mnsq )
   deallocate( ps_u_mnsq )

   call da_free_unit(ounit)
   call da_free_unit(iunit)
   call da_free_unit(gen_be_ounit)
   call da_free_unit(namelist_unit)

end program gen_be_ep1

