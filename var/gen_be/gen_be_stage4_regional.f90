program gen_be_stage4_regional

   use da_control, only : stderr,stdout, filename_len
   use da_tools_serial, only : da_get_unit, da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)       :: run_dir                    ! Run directory.
   character(len=filename_len)       :: filename                   ! Input filename.
   character*1         :: k_label                    ! = 'k' if model level, 'm' if mode output.
   character*2         :: ck                         ! Level index -> character.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj                     ! Dimensions read in.
   integer             :: k, kdum                    ! Vertical index.
   integer             :: stride                     ! Calculate correlation with every stride point.
   integer             :: nbins                      ! Number of latitude bins
   integer             :: ibin                       ! Which latitude bin to process (1:nbins)
   integer             :: nn                         ! Dimension of radii bins.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: member                     ! Loop counters.
   integer             :: jstart, jend               ! Starting and ending j indices
   real                :: count                      ! Counter for times/members.
   integer, allocatable:: nr(:)                      ! Number of points in each bin.
   real, allocatable   :: field(:,:)                 ! Input 2D field.
   real, allocatable   :: cov(:)                     ! Covariance as a function of distance.

   namelist / gen_be_stage4_regional_nl / start_date, end_date, interval, variable, &
                                          ne, k, nbins, ibin, stride, run_dir

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   start_date = '2004030312'
   end_date = '2004033112'
   interval = 24
   variable = 'psi'
   ne = 1
   k = 1
   stride = 1
   nbins = 1
   ibin = 1
   run_dir = '/mmmtmp/dmbarker'
   open(unit=namelist_unit, file='gen_be_stage4_regional_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_regional_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(UNIT=6,FMT='(4a)')' Computing error correlation scales for dates ', start_date, ' to ', end_date
   write(UNIT=6,FMT='(a,i3,a,i3)') '                                    for bin ', ibin, ' of ', nbins
   write(UNIT=6,FMT='(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(UNIT=6,FMT='(a,i8)')' Number of ensemble members at each time = ', ne
   write(UNIT=6,FMT='(a,i8)')' Stride over which to jump points in correlation calculation = ', stride

   date = start_date
   cdate = sdate

   write(UNIT=ck,FMT='(i2)') k
   if ( k < 10 ) ck = '0'//ck(2:2)
   k_label = 'm'

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [2] Input fields and calculate correlation as a function of distance between points.'
!---------------------------------------------------------------------------------------------

   count = 1.0

   do while ( cdate <= edate )
      do member = 1, ne

         write(UNIT=6,FMT='(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member

         write(UNIT=ce,FMT='(i3.3)')member

!        Assumes variable directory has been created by script:
         filename = trim(run_dir)//'/'//trim(variable)//'/'//date(1:10)//'.'//trim(variable)
         filename = trim(filename)//'.e'//ce//'.'//ck
         open (UNIT=iunit, file = filename, form='unformatted')
         read(UNIT=iunit) ni, nj, kdum

         jstart = floor(real(ibin-1)*real(nj)/real(nbins))+1
         jend   = floor(real(ibin)  *real(nj)/real(nbins))
         write(UNIT=6,FMT='(a,i4,a,i4)') 'Computing length scale over j range ', jstart, ' to ', jend

         if ( count == 1.0 ) then
            nn = ni * ni + nj * nj  ! Maximum radius across grid (squared).
            allocate(field(1:ni,1:nj))
            allocate(nr(0:nn))
            allocate(cov(0:nn))
            cov(0:nn) = 0.0

            call get_grid_info( ni, nj, nn, stride, nr, jstart, jend )
         end if

         read(UNIT=iunit)field(1:ni,1:nj)
         close(UNIT=iunit)

!        Calculate spatial correlation:
         call get_covariance( ni, nj, nn, stride, count, nr, jstart, jend, field, cov )

         count = count + 1.0

      end do ! End loop over members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate
   end do

!---------------------------------------------------------------------------------------------
   write(UNIT=6,FMT='(a)')' [3] Compute fit of correlation to a straight line.'
!---------------------------------------------------------------------------------------------

   filename = 'sl_print.'//trim(variable)//'.'//ck
   open( unit=ounit, file=trim(filename), form='formatted', &
         action='write', access='sequential', status='replace')

   call make_scale_length( variable, ck, ounit, nn, nr, cov )

   close(unit=ounit, status='keep')

   deallocate (field) 
   deallocate (cov)

contains

subroutine get_grid_info( ni, nj, nn, stride, nr, jstart, jend )

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
   integer, intent(out)   :: nr(0:nn)                ! Number of points in each bin.

   integer                :: i, j, m, n              ! Indices.
   integer                :: d2                      ! Distance squared.

   nr(0:nn) = 0

!  [1] Get points distribution nr(0:nn):

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:NR)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate points distribution (i,j) for points that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            nr(d2) = nr(d2) + 1                        ! Add this point to number at this distance.
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
               nr(d2) = nr(d2) + 1                     ! Add this point to number at this distance.
            end do ! m
         end do ! n
      end do ! i
   end do ! j
!$OMP END PARALLEL DO

end subroutine get_grid_info

subroutine get_covariance( ni, nj, nn, stride, count, nr, jstart, jend, field, cov )
   
!  Calculate the number of points, distance matrx between points on grid.
!  Dale Barker: May 16th 2005.

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   real, intent(in)       :: count                   ! Number of times/ensemble members so far.
   integer, intent(in)    :: nr(0:nn)                ! Number of points in each bin.
   integer, intent(in)    :: jstart, jend            ! Starting and ending j indices
   real, intent(in)       :: field(1:ni,1:nj)        ! 2D field.
   real, intent(inout)    :: cov(0:nn)               ! Covariance of each bin.

   integer                :: i, j, m, n              ! Loop counters.
   integer                :: d2                      ! Distance squared.
   real                   :: count_inv               ! 1 / count.
   real                   :: bb(0:nn)                ! Covariance for this field.

   count_inv = 1.0 / count

   bb(0:nn) = 0.0

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:BB)
   do j = jstart, jend, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate spatial correlations with point (i,j) that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            bb(d2) = bb(d2) + field(i,j) * field(m,n)
         end do

!        Now complete remaining rows:
         do n = j+1, jend
            do m = 1, ni
               d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
               bb(d2) = bb(d2) + field(i,j) * field(m,n)
            end do ! m
         end do ! n

      end do ! i
   end do ! j
!$OMP END PARALLEL DO

!  Calculate average values for each bin at this time:
!$OMP PARALLEL DO PRIVATE(D2)
   do d2 = 0, nn
      if ( nr(d2) /= 0 ) then
         bb(d2) = bb(d2) / real(nr(d2))

!        Calculate accumulating average over members/times:
         cov(d2) = ( (count - 1.0) * cov(d2) + bb(d2) ) * count_inv

      end if
   end do
!$OMP END PARALLEL DO

end subroutine get_covariance

subroutine make_scale_length( variable, ck, ounit, nn, nr, cov )

!  Purpose: Calculate fit of input covariance data to Gaussian correlation function
!  cov(r) = cov(0) * exp(-r**2 / 8)
!  Dale Barker: 17th May 2005.

   implicit none

   character*10, intent(in):: variable               ! Variable name
   character*2, intent(in):: ck                      ! Level index -> character.
   integer, intent(in)    :: ounit                   ! Output unit.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in )   :: nr(0:nn)                ! Number of points in each bin.
   real, intent(in)       :: cov(0:nn)               ! Covariance of each bin.

   real(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
   real(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
   real(kind=8)           :: d(0:nn)                 ! Distance for each bin.

   integer                :: n, d2                   ! Loop counters.
   integer                :: nmax                    ! Number of points available for curve fitting.
   real                   :: yr_cutoff               ! Noise cut-off criterion.
   real                   :: corr_min                ! Corresponding correlation value.
   real(kind=8)           :: coeff1, coeff2          ! Curve-fitting coefficients.
   real(kind=8)           :: ml, sl                  ! Gradient, scalelength.

   yr(0:nn) = 0.0
   nrr(0:nn) = 0.0
   d(0:nn) = 0.0

   yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
   corr_min = exp( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)
   write(UNIT=6,FMT='(a,1pe15.5)')' Fit Gaussian curve to data for correlations >= ', corr_min

   write(UNIT=6,FMT='(5a)')'  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '
   n = 0
   nrr(n) = real(nr(n))
   write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(0), cov(n), yr(n)

   do d2 = 1, nn

      if ( nr(d2) > 0 .and. cov(d2) < cov(0) ) then ! Omit bins with no data and negative logs.

         if ( cov(d2) / cov(0) < corr_min ) exit ! Yong-Run's noise cut-off criterion.
         n = n + 1
         yr(n) = sqrt( 8.0 * log(cov(0) / cov(d2)) )
         nrr(n) = real(nr(d2))
         d(n) = sqrt(real(d2))                  ! Distance
         write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(n), cov(d2), yr(n)
       end if
   end do
   nmax = n

!  Now perform curve-fitting when more than 2 points available:

!-----Steps of fitting Gaussian Distribution:

!     B(r) = B(0) exp(-d**2/(8*s**2)      (1)

!     Log at both side of (1):

!        ln[B(0)/B(d)] = d**2/(8*s**2)   (2)

!        {8*ln[B(0)/B(d)]}**0.5 = d/s = m * d

!     Let:

!        y(d) = {8*ln[B(0)/B(d)]}**0.5

!        m = sum[d * y(d)]/sum[d*d]

   coeff1 = 0.0
   coeff2 = 0.0

   do n = 1, nmax
      WRITE(UNIT=6,FMT='("n, nrr, d, yr:",i3,3e15.6)') n, nrr(n), d(n), yr(n)
      coeff1 = coeff1 + nrr(n) * d(n) * yr(n)
      coeff2 = coeff2 + nrr(n) * d(n) * d(n)
   end do

   if (coeff2 > 0.0) then

     ml = coeff1 / coeff2

     sl = 1.0 / ml

   else

! When no fitting could be completed, set the missing value = 0.0 (YRG 06/30/2005):

     ml = 0.0
     sl = 0.0

   endif

   write(UNIT=6,FMT='(/3a,3e30.8/)') trim(variable), ' scale-length at mode:', ck, ml, sl

   write(UNIT=ounit,FMT='(a,2e20.8)') ck, ml, sl

end subroutine make_scale_length


end program gen_be_stage4_regional
