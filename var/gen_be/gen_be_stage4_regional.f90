program gen_be_stage4_regional

   use da_control, only : filename_len,stderr,stdout,do_normalize,use_rf
   use da_reporting, only : da_error
   use da_tools_serial, only : da_get_unit, da_advance_cymdh
   use da_wavelet, only: lf,namw,nb,nij,ws,wsd

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*10        :: variable                   ! Variable name
   character(len=filename_len)       :: run_dir      ! Run directory.
   character(len=filename_len)       :: filename     ! I/O filename.
   character*1         :: k_label                    ! = 'k' if model level, 'm' if mode output.
   character*2         :: ck                         ! Level index -> character.
   integer             :: ni, nj                     ! Dimensions read in.
   integer             :: k                          ! Vertical index.
   integer             :: stride                     ! Calculate correlation with every stride point.
   integer             :: nbins                      ! Number of latitude bins
   integer             :: ibin                       ! Which latitude bin to process (1:nbins)
   integer             :: nn                         ! Dimension of radii bins.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: member                     ! Loop counters.
   integer             :: jstart, jend               ! Starting and ending j indices
   integer             :: i                          ! Dummy index
   logical             :: print_wavelets
   logical             :: allow_missing_dates        ! If data from stage 1 is not contiguous, attempt to continue
   logical             :: missing                    ! Flag if subroutine found missing date
   real                :: rcount                     ! Counter for times/members ("count" is intrinsic).
   real                :: fnorm                      ! field norm
   integer, allocatable:: nr(:)                      ! Number of points in each bin.
   real, allocatable   :: field(:,:)                 ! Input 2D field.
   real, allocatable   :: grid_box_area(:,:)         ! cf. da_transfer_wrftoxb
   real, allocatable   :: sd(:,:)                    ! Field standard deviations.
   real, allocatable   :: cov(:)                     ! Covariance as a function of distance.

   real, allocatable   :: wav(:,:)                   ! Wavelet-coefficient averages.

   namelist / gen_be_stage4_regional_nl / start_date, end_date, interval, variable, &
                                          ne, k, nbins, ibin, stride, run_dir, &
                                          do_normalize, print_wavelets, lf, namw, nb, &
                                          use_rf, allow_missing_dates

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
   if( use_rf )then
      nbins = 1
      ibin = 1
      stride = 1
   endif
   run_dir = '/mmmtmp/dmbarker'
   allow_missing_dates = .false.
   open(unit=namelist_unit, file='gen_be_stage4_regional_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_stage4_regional_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   if( use_rf )then
      write(UNIT=6,FMT='(4a)')' Computing error correlation scales for dates ', start_date, ' to ', end_date
      write(UNIT=6,FMT='(a,i3,a,i3)') '                                    for bin ', ibin, ' of ', nbins
      write(UNIT=6,FMT='(a,i8)')' Stride over which to jump points in correlation calculation = ', stride
   else
#ifdef WAVELET
      call qftest_w(namw//char(0),lf)
#else
      call da_error(__FILE__,__LINE__,(/"needs qftest_w, compile after setenv WAVELET 1"/))
#endif
      write(6,'(" Computing field and wavelet std. devs. for dates ",a," to ",a)')start_date,end_date
      write(6,'("    using ",i8," bands of ",a,i0.2," wavelet.")')nb,namw,lf
!     allocate(fw(lf,0:1))			! Wavelet filters.
!     do i=0,1					! Assign wavelet filters:
!        call qf_w(namw//char(0),lf,i,fw(:,i))
!        call print_filter(namw,lf,i,fw(:,i))
!     enddo
      allocate(nij(0:nb,0:1,0:2))
   endif
   write(UNIT=6,FMT='(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(UNIT=6,FMT='(a,i8)')' Number of ensemble members at each time = ', ne

   write(UNIT=ck,FMT='(i2)') k
   if ( k < 10 ) ck = '0'//ck(2:2)
   k_label = 'm'

   call initialize_dates(date,start_date,cdate,sdate,rcount)

!---------------------------------------------------------------------------------------------
   if( use_rf )then
      write(6,'(" [2.0] Input fields and calculate correlation as a function of distance between points.")')
   else
      write(6,'(" [2.0] Input fields and calculate wavelet mean and mean-square values.")')
      filename=trim(run_dir)//'/grid_box_area'
      open(iunit,file=filename,form='unformatted')
      read(iunit)ni,nj
      allocate(grid_box_area(ni,nj))
      read(iunit)grid_box_area
      close(iunit)
   endif
!---------------------------------------------------------------------------------------------

   if( do_normalize )then
      do while( cdate<=edate )			! Do part of later cdate loop now:
         do member = 1, ne			! Loop over members:
            call read_ni_nj(iunit,member,run_dir,variable,date(1:10),ck,ni,nj,missing)
            if (missing) then
               write(6,'(a,i10)')' WARNING: MISSING DATA FOR DATE ',cdate
               if (allow_missing_dates) then
                  write(6,'(a)')' Attempting to continue since allow_missing_dates = .true.'
                  cycle
               else
                  call da_error(__FILE__,__LINE__,(/"ERROR: MISSING DATA"/))
               endif
            endif
            if( rcount==1.0 )call allocate_field_moments(field,wav,sd,ni,nj)
            read(iunit)field
            close(iunit)
            call update_moments(field,wav,sd,ni,nj,rcount,1.)
         enddo					! End loop over members.
         call calculate_next_date(date,interval,new_date,cdate)
      enddo					! do while ( cdate<=edate )
!---------------------------------------------------------------------------------------------
      write(6,'(" [2.1] Compute field standard deviations.")')
!---------------------------------------------------------------------------------------------
      call update_moments(field,wav,sd,ni,nj,rcount,0.)
!     print'(es9.2,"< sd[",a,",k=",i0,"]~",es9.2,"<",es9.2)', &
!        minval(sd),trim(variable),k,sqrt(sum(sd**2)/product(nij(0,:,0))),maxval(sd)
      deallocate(field,wav)			! Need different SIZEs later.
      call initialize_dates(date,start_date,cdate,sdate,rcount)
   endif					! if( do_normalize )

   do while ( cdate <= edate )
      do member = 1, ne

         write(UNIT=6,FMT='(5a,i4)')'    Date = ', date, ', variable ', trim(variable), &
                           ' and member ', member
         call read_ni_nj(iunit,member,run_dir,variable,date(1:10),ck,ni,nj,missing)
         if (missing) then
            write(6,'(a,i10)')' WARNING: MISSING DATA FOR DATE ',cdate
            if (allow_missing_dates) then
               write(6,'(a)')' Attempting to continue since allow_missing_dates = .true.'
               cycle
            else
               call da_error(__FILE__,__LINE__,(/"ERROR: MISSING DATA"/))
            endif
         endif
         if( use_rf )then
            jstart = floor(real(ibin-1)*real(nj)/real(nbins))+1
            jend   = floor(real(ibin)  *real(nj)/real(nbins))
            write(UNIT=6,FMT='(a,i4,a,i4)') 'Computing length scale over j range ', jstart, ' to ', jend
         else
            write(6,'("Computing wavelet coefficients")')
         endif
         if ( rcount == 1.0 ) then
            if( use_rf )then
               nn = ni * ni + nj * nj		! Maximum radius across grid (squared).
               allocate(field(1:ni,1:nj))
               allocate(nr(0:nn))
               allocate(cov(0:nn))
               cov(0:nn) = 0.0
               call get_grid_info( ni, nj, nn, stride, nr, jstart, jend )
            else				! Use wavelets:
               nij(0,0,0)=nj			! Largest low-pass dimension along j.
               nij(0,1,0)=ni			! Largest low-pass dimension along i.
               do i=1,0,-1			! loop over i & j directions:
#ifdef WAVELET
                  call dwta_partition(nij(:,i,0),nij(:,i,1),nij(:,i,2),nb,lf,lf)
#else
                  call da_error(__FILE__,__LINE__,(/"needs dwta_partition, compile after setenv WAVELET 1"/))
#endif
                  write(6,&
                        '("Direction ",a," has partition nij(0:",i1,",",i1,",0:2)={",99("{",i3,",",i3,",",i3,"}",:))',&
                        ADVANCE="NO")char(106-i),nb,i,transpose(nij(:,i,:))
                  write(6,'("}.")')
               enddo
               call allocate_field_moments(field,wav,wsd,nij(0,1,2),nij(0,0,2))
               allocate(ws(maxval(nij(0,:,2))))
            endif
         endif					! rcount == 1.0

         read(UNIT=iunit)field(1:ni,1:nj)
         close(UNIT=iunit)

         if( do_normalize )field(:ni,:nj)=field(:ni,:nj)/sd
         if( use_rf )then
!           Calculate spatial correlation:
            call get_covariance( ni, nj, nn, stride, rcount, nr, jstart, jend, field(:ni,:nj), cov )
            rcount=rcount+1.0
         else
            field(:ni,:nj)=field(:ni,:nj)*sqrt(grid_box_area)
            fnorm=sqrt(sum(field(:ni,:nj)**2))
            print'("r=||    ",a,"(",a,",",i0,",:",i0,",:",i0,") ||  : ",ES15.9)', &
                  trim(variable),date,member,ni,nj,fnorm
!           Calculate wavelet transform:
#ifdef WAVELET
            call dwtai2_w(namw//char(0),lf,field,ws,nij(0,0,0),nij(0,0,2),nb)
#else
            call da_error(__FILE__,__LINE__,(/"needs dwtai2_w, compile after setenv WAVELET 1"/))
#endif
            print'("1-||  W[",a,"(",a,",",i0,",:",i0,",:",i0,")]||/r: ",ES15.9)', &
                  trim(variable),date,member,nij(0,1,2),nij(0,0,2),1.-sqrt(sum(field**2))/fnorm
            call update_moments(field,wav,wsd,nij(0,1,2),nij(0,0,2),rcount,1.)
         endif

      enddo					! End loop over members.
      call calculate_next_date(date,interval,new_date,cdate)
   end do					! do while ( cdate <= edate )

   if( use_rf )then
!---------------------------------------------------------------------------------------------
      write(UNIT=6,FMT='(a)')' [3] Compute fit of correlation to a straight line.'
!---------------------------------------------------------------------------------------------
      filename = 'sl_print.'//trim(variable)//'.'//ck
      open( unit=ounit, file=trim(filename), form='formatted', iostat=i, &
            action='write', access='sequential', status='replace')
      if( i/=0 )print'("OPEN(FILE=",a,",IOSTAT=",i0,")")',trim(filename),i
      call make_scale_length( variable, ck, ounit, nn, nr, cov )
      close(unit=ounit, status='keep')
      deallocate(cov)
      if( do_normalize )then
         write(ck,'(i0)')k
         open(ounit,action="write",file="mom",form="unformatted",status="replace")
         write(ounit)do_normalize
         write(ounit)nj,ni
         write(ounit)sd
         close(ounit)
      endif
   else 
!---------------------------------------------------------------------------------------------
      write(6,'(" [3] Compute wavelet standard deviations.")')
!---------------------------------------------------------------------------------------------
      call update_moments(field,wav,wsd,nij(0,1,2),nij(0,0,2),rcount,0.)
      print'(es9.2,"<wsd[",a,",k=",i0,"]~",es9.2,"<",es9.2)', &
         minval(wsd),trim(variable),k,sqrt(sum(wsd**2)/product(nij(0,:,2))),maxval(wsd)
      open(ounit,action="write",file="momw",form="unformatted",status="replace")
!     Note that namw is just 1 byte!
      write(ounit)do_normalize,namw,lf,nb
      write(ounit)nij
      write(ounit)wsd
      if( do_normalize )write(ounit)sd
      close(ounit)
      if( print_wavelets )then
         do i=1,0,-1			! loop over i & j directions:
            deallocate(ws)
            allocate(cov(nij(0,i,2)),ws(2*floor(.5*(nij(0,i,0)+lf))+lf-2))
            open(ounit,action="write",file="w_"//char(106-i)//".dat",form="unformatted",status="replace")
            write(ounit)namw,lf,nb,nij(:,i,:)
            do k=1,nij(0,i,2)		! loop over wavelet indexes:
               cov=0.			! compute wavelet basis functions:
               cov(k)=1.		! k'th wavelet basis function
#ifdef WAVELET
               call idwtai_w(namw//char(0),lf,cov,ws,nij(0,i,0),nij(0,i,1),nij(0,i,2),nb)
#else
               write(6,*) "Needs idwtai_w, please compile code with WAVELET setting"
               stop
#endif
               write(ounit)real(cov(:nij(0,i,0)),4)
            enddo			! wrote wavelet basis function to file
            close(ounit)
            deallocate(cov)
         enddo
      endif 
!     deallocate(fw)
      deallocate(grid_box_area,nij,wav,ws,wsd)
      if( do_normalize )deallocate(sd)
   endif
   deallocate(field) 

contains

 SUBROUTINE allocate_field_moments(field,av,sd,ni,nj)
 IMPLICIT NONE
 INTEGER,         INTENT(IN   )::ni,nj
 REAL,ALLOCATABLE,INTENT(INOUT)::av(:,:),field(:,:),sd(:,:)
 ALLOCATE(av(ni,nj),field(ni,nj),sd(ni,nj))
 av=0.
 sd=0.
 ENDSUBROUTINE allocate_field_moments

 SUBROUTINE calculate_next_date(date,interval,new_date,cdate)
 IMPLICIT NONE
 CHARACTER*10,INTENT(INOUT)::date
 CHARACTER*10,INTENT(  OUT)::new_date
 INTEGER,     INTENT(IN   )::interval
 INTEGER,     INTENT(  OUT)::cdate
 CALL da_advance_cymdh(date,interval,new_date)
 date=new_date
 READ(date,'(I10)')cdate
 ENDSUBROUTINE calculate_next_date

 SUBROUTINE initialize_dates(date,start_date,cdate,sdate,rcount)
 IMPLICIT NONE
 CHARACTER*10,INTENT(IN )::start_date
 CHARACTER*10,INTENT(OUT)::date
 INTEGER,INTENT(IN )::sdate
 INTEGER,INTENT(OUT)::cdate
 REAL,   INTENT(OUT)::rcount
 date=start_date
 cdate=sdate
 rcount=1.0
 ENDSUBROUTINE initialize_dates

 SUBROUTINE update_moments(field,av,sd,ni,nj,rcount,irc)
 IMPLICIT NONE
 INTEGER,INTENT(IN   ):: ni,nj
 REAL,   INTENT(IN   ):: field(ni,nj),irc
 REAL,   INTENT(INOUT):: av(ni,nj),rcount,sd(ni,nj)
 REAL                 :: c
 IF( irc==1. )THEN
    av=av+field
    sd=sd+field**2			! sd>=av**2/rcount in exact arithmetic.
    rcount=rcount+irc
 ELSE
    rcount=rcount-1.			! Last increment was bogus.
    av=av/rcount
    sd=(sd-rcount*av**2)/(rcount-1.)
    c=COUNT(sd<0.)
    IF(c>0.) &
       PRINT'(a,": rcount=",F2.0,", zeroing 0 > sd**2 > ",ES9.2," (",ES8.2,")%.")', &
          __FILE__,rcount,MINVAL(sd),100*c/(ni*nj)
!   Correct for small negative differences:
    sd=SQRT(MAX(0.,sd))
 ENDIF
 ENDSUBROUTINE update_moments

 SUBROUTINE read_ni_nj(iunit,member,run_dir,variable,date,ck,ni,nj,missing)
 IMPLICIT NONE 
 CHARACTER(LEN=filename_len),INTENT(IN):: run_dir       ! Run directory.
 CHARACTER*2,                INTENT(IN):: ck            ! Level index -> character.
 CHARACTER*10,               INTENT(IN):: date,variable ! Date, variable name
 INTEGER,                    INTENT(IN):: iunit,member
 INTEGER,                   INTENT(OUT):: ni,nj
 LOGICAL,                   INTENT(OUT):: missing       ! missing date flag, set true if file open fails
 INTEGER                               :: readstat
 CHARACTER*3                           :: ce            ! Member index -> character.
 CHARACTER(LEN=filename_len)           :: filename      ! I/O filename.
 INTEGER                               :: kdum
 WRITE(ce,'(i3.3)')member
 missing = .false.
! Assumes variable directory has been created by script:
 filename=TRIM(run_dir)//'/'//TRIM(variable)//'/'//date//'.'//TRIM(variable) &
                                    //'.e'//ce//'.'//ck
 OPEN(iunit,FILE=filename,FORM="UNFORMATTED")
 READ(iunit,IOSTAT=readstat)ni,nj,kdum
 if (readstat /= 0) then
       ni = 0
       nj = 0
       missing = .true.
 endif
 ENDSUBROUTINE read_ni_nj

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
