program da_tune_obs_hollingsworth2

   !-----------------------------------------------------------
   ! Abstract:
   !    Purpose: Program for tuning observation errors
   !             (Hollingsworth method)
   !        Ref: Tellus (1986) 38, pp.111-161 (Part I & II)
   !-----------------------------------------------------------

   use da_control, only : filename_len, earth_radius, pi

   implicit none
   
   character*5, parameter    :: missing_c = '*****'
   integer, parameter        :: ounit = 10
   integer, parameter        :: num_bins_p = 10
   integer, parameter        :: obs_qc_pointer = 0
   integer, parameter        :: max_stations = 2526
   integer, parameter        :: max_times = 200  
   integer, parameter        :: inunit = 35
   integer, parameter        :: min_obs = 30
   integer, parameter        :: num_bins = 100
   integer, parameter        :: missing_i = -88
   real, parameter           :: bottom_pressure = 1000.0
   real, parameter           :: bin_width_p = 100.0
   real, parameter           :: missing_r = -888888.0
   real, parameter           :: max_distance = 5000      ! km

   type sub_type
      integer                :: qcflag(1:max_times)
      real                   :: ominusb(1:max_times)
      real                   :: sigmao(1:max_times)
      real                   :: pressure(1:max_times)
   end type sub_type

   type obs_type
      character*5            :: id(1:max_stations)
      integer                :: num_reject
      integer                :: num_keep
      integer                :: num_stations
      integer                :: num_obs(1:max_stations)
      real                   :: mean_omb
      real                   :: stdv_omb
      real                   :: lat(1:max_stations)
      real                   :: lon(1:max_stations)
      real                   :: mean_ominusb(1:max_stations)
      real                   :: dis(1:max_stations,1:max_stations)
      real                   :: cov(1:num_bins)
      type (sub_type)        :: data(1:max_stations)
   end type obs_type

   character(len=filename_len) :: filename
   ! character*5               :: station_chosen
   character*5               :: station_id
   integer                   :: times, n, b
   integer                   :: bin
   integer                   :: num_times
   integer                   :: qc
   integer                   :: percent_reject
   real                      :: p_ob
   real                      :: lati, long, iv, error
   ! type (obs_type)           :: obs(1:num_bins_p)
   type (obs_type),allocatable           :: obs(:)


   real                      :: bin_start_p(1:num_bins_p)    ! Number of pressure bins
   real                      :: obs_err_used(1:num_bins_p)   ! Obs error currently used 


   ! for temperature it is 1.0
   ! data obs_err_used/ 1.0,  1.0,  1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/ 
   data obs_err_used/ 1.1,  1.1,  1.1, 1.4, 1.8, 2.3, 2.8, 3.3, 3.3, 2.7/ 
   !                      1000  900   800  700  600  500  400  300  200  100 
   print*,' num_bins_p = ',num_bins_p 
   allocate ( obs( 1: num_bins_p) )
   do b = 1, num_bins_p
      bin_start_p(b) = bottom_pressure - real(b-1) * bin_width_p

      ! Initialize obs structure:
      call da_init_obs( obs(b) )
   end do

   !-----------------------------------------------------------------------
   ! [1.0] Read in O-B data and fill structure:
   !-----------------------------------------------------------------------

   times = 0
   ! station_chosen = "MOWV3"
   ! filename = "da_tune_obs_hollingsworth2_poamvu_"//station_chosen(1:5)//"_omb.out"
   ! station_chosen = "91652"
   ! filename = "da_tune_obs_hollingsworth2_soundt_"//station_chosen(1:5)//"_omb.out"
   filename = "hollingsworth2.out"
   open(ounit, file = filename, status = "unknown" )

   do ! Loop over time:
      times = times + 1

      do ! Loop over obs in file:
      
         read(inunit,'(a5,2f9.3,3f17.7,i8)')station_id, lati, long, &
                                            p_ob, iv, error, qc

         ! [1.1] Exit when come to end of file markers:
         ! if ( station_id(1:5) == '*****' .or. station_id == '*end*' ) exit
         if ( station_id(1:5) == '*****' .or. station_id == '*end*' ) then
            print*,' Hit station_id as ',station_id
            exit
         end if
         
         ! [1.2] Store data:

         ! if ( station_id(1:5) == station_chosen ) then

         p_ob = 0.01 * p_ob   ! Convert Pa to hPa

         bin = int( (bottom_pressure - p_ob)/bin_width_p ) + 1
         if (bin <= 0) bin = 1
         if (bin > num_bins_p) bin = num_bins_p

         call da_store_ominusb( station_id, times, qc, lati, long, &
                                   p_ob, iv, error, obs(bin) )
         ! end if
               
      end do

      ! [1.5] A station_id = '*end*' indicates the complete end of the file:
      if ( station_id == '*end*' ) then
         exit
      end if

   end do
   
   num_times = times
   write(0,'(A,i8)')' Number of analysis times read in = ', num_times
   write(0,*)

   !-----------------------------------------------------------------------
   ! [2.0]: Calculate O-B statistics over all stations:
   !-----------------------------------------------------------------------

   write(0,'(A)')' P(hPA)  Stations Obs-Used Obs-Reject     Mean(O-B)    STDV(O-B)'
   write(ounit,'(A)')' P(hPA)  Stations Obs-Used Obs-Reject     Mean(O-B)    STDV(O-B)'
   ! write(0,'(a,a)')' Station chosen = ', station_chosen
   do b = 1, num_bins_p
   call da_obs_stats( num_times, obs(b) )

      percent_reject = 0
      if ( obs(b) % num_stations > 0  ) then
           percent_reject = nint( 100.0 * real( obs(b) % num_reject) / &
                            real( obs(b) % num_keep + obs(b) % num_reject ) )
      end if
   
      write(ounit,'(i6,i8,2x,2i8,i3,2f13.6)')int(bin_start_p(b)), &
                                             obs(b) % num_stations, obs(b) % num_keep, &
                                             obs(b) % num_reject, percent_reject, &
                                             obs(b) % mean_omb, obs(b) % stdv_omb
   end do
   write(0,*)

   !-----------------------------------------------------------------------
   ! [3.0]: Calculate matrix of distances between points:
   !-----------------------------------------------------------------------

   do b = 1, num_bins_p
      n = obs(b) % num_stations
      call da_get_distance( n, obs(b) % lat(1:n), obs(b) % lon(1:n), obs(b) % dis(1:n,1:n) )
      write(0,'(4i8,2f13.6)')int(bin_start_p(b)), obs(b) % num_stations, &
                             obs(b) % num_keep, obs(b) % num_reject, &
                             obs(b) % mean_omb, obs(b) % stdv_omb
   end do

   !-----------------------------------------------------------------------
   ! [4.0]: Calculate O-B covariances:
   !-----------------------------------------------------------------------

   do b = 1, num_bins_p
      ! call da_bin_covariance( obs(b) % num_stations, num_times, max_distance, obs(b) )
      call da_bin_covariance( obs(b) % num_stations, num_times, max_distance, obs(b), obs_err_used(b),b)
   end do

contains

subroutine da_init_obs( obs )

   implicit none

   type (obs_type), intent(out) :: obs
   
   integer                      :: n

   obs % num_reject = 0
   obs % num_keep = 0
   obs % num_stations = 0
   obs % num_obs(:) = 0
   obs % id(:) = missing_c
   obs % lat(:) = missing_r
   obs % lon(:) = missing_r
   do n = 1, max_stations
      obs % data(n) % qcflag(:) = missing_i
      obs % data(n) % ominusb(:) = missing_r
      obs % data(n) % sigmao(:) = missing_r
      obs % data(n) % pressure(:) = missing_r
   end do

end subroutine da_init_obs

subroutine da_store_ominusb( station_id, times, qc, lati, long, p_ob, &
                             iv, error, obs )

   implicit none

   character*5, intent(in)       :: station_id
   integer, intent(in)           :: times
   integer, intent(in)           :: qc
   real, intent(in)              :: lati
   real, intent(in)              :: long
   real, intent(in)              :: p_ob
   real, intent(in)              :: iv
   real, intent(in)              :: error
   type(obs_type), intent(inout) :: obs

   integer                       :: this_station
   integer                       :: n
   real                          :: pi_over_180

   pi_over_180 = pi / 180.0

   ! [1.0] Count total number of obs rejected by QC:

   if ( qc < obs_qc_pointer ) then
      obs % num_reject = obs % num_reject + 1
   else
      obs % num_keep = obs % num_keep + 1
   end if

   ! [2.0] Check if this station seen before:

   if ( ANY(obs % id(:) == station_id) ) then
      do n = 1, obs % num_stations
         if ( obs % id(n) == station_id ) then
            this_station = n
         end if
      end do
   else ! New station:
      obs % num_stations = obs % num_stations + 1
      this_station = obs % num_stations
      obs % id(this_station) = station_id
      
      if ( long >= 0.0 ) then
         obs % lon(this_station) = pi_over_180 * long
      else
         obs % lon(this_station) = pi_over_180 * ( 180.0 - long )
      end if
      obs % lat(this_station) = pi_over_180 * lati

   end if

   ! [3.0] Check if getting too many stations or times for array sizes:

   if ( obs % num_stations > max_stations ) then
      write(0,'(A)')' Need to increase max_stations. Stopping'
      stop
   end if

   ! [4.0] Read in qc, O-B, and sigma_o:

   obs % data(this_station) % qcflag(times) = qc
   obs % data(this_station) % ominusb(times) = iv
   obs % data(this_station) % sigmao(times) = error
   obs % data(this_station) % pressure(times) = p_ob
   
end subroutine da_store_ominusb

subroutine da_obs_stats( num_times, obs )

   integer, intent(in)            :: num_times
   type (obs_type), intent(inout) :: obs
   
   integer, parameter             :: num_bins = 101
   
   integer                        :: n, nobs, nobs_station, times, b, count
   integer                        :: bin_count(1:num_bins), maxcount
   real                           :: sumobs1, sumobs2, sum_station
   real                           :: min_bin, max_bin, bin_width, omb, x, z
   real                           :: bin_start(1:num_bins)
   real                           :: dommean(1:num_times)

   obs % mean_omb = 0.0
   obs % stdv_omb = 0.0

   nobs = 0
   sumobs1 = 0.0
   sumobs2 = 0.0

   do n = 1, obs % num_stations
      nobs_station = 0
      sum_station = 0.0
      obs % mean_ominusb(n) = 0.0
      do times = 1, num_times
         if ( obs % data(n) % qcflag(times) >= obs_qc_pointer ) then
            nobs = nobs + 1
            sumobs1 = sumobs1 + obs % data(n) % ominusb(times)
            sumobs2 = sumobs2 + obs % data(n) % ominusb(times) **2

            nobs_station = nobs_station + 1
            sum_station = sum_station + obs % data(n) % ominusb(times)
         end if
      end do
      obs % num_obs(n) = nobs_station
      if ( nobs_station > 0 ) then
         obs % mean_ominusb(n) = sum_station / real(nobs_station)
      end if
   end do

  
   ! Calculate basic statistics:
   if ( nobs > 0 ) then
      obs % mean_omb = sumobs1 / real(nobs)
      obs % stdv_omb = sumobs2 / real(nobs) ! Actually mean square here.
      obs % stdv_omb = sqrt( obs % stdv_omb - obs % mean_omb**2 )
   end if

   ! Get data distribution:
   max_bin = 5.0 * obs % stdv_omb
   min_bin = -max_bin
   if (nobs > 1) then
      bin_width = 2.0 * max_bin / real(num_bins)
   else
      write (0,*) "Cannot work with a single observation"
      stop
   end if
         
   bin_count(:) = 0   
   do b = 1, num_bins
      bin_start(b) = min_bin + real(b-1) * bin_width
   end do

   do n = 1, obs % num_stations
      do times = 1, num_times
         omb = obs % data(n) % ominusb(times)
     
         if ( obs % data(n) % qcflag(times) >= obs_qc_pointer ) then

           b = int( (omb-min_bin) / bin_width) + 1

           if (b >= 1 .and. b <= num_bins) bin_count(b) = bin_count(b) + 1
         end if
      end do
   end do
  
   maxcount = maxval(bin_count(:))
   ! write(0,'(a,i8)')' Max count = ', maxcount
   ! write(0,'(a)')' Bin      x=O-B   z=(x-xm)/sd     Count   exp(-0.5*z*z)'
   do b = 1, num_bins
      x = bin_start(b) + 0.5 * bin_width
      z = ( x - obs % mean_omb ) / obs % stdv_omb
      ! write(0,'(i4,4f12.5)')b, x, z, bin_count(b)/real(maxcount), exp(-0.5*z*z)
   end do
   ! write(0,*)
   
   ! Get time series of mean error:
   dommean(:) = 0.0
   ! write(0,'(a)')' Time  NumObs Domain Mean'
   do times = 1, num_times
      count = 0
      do n = 1, obs % num_stations
         if ( obs % data(n) % qcflag(times) >= obs_qc_pointer ) then
            dommean(times) = dommean(times) + obs % data(n) % ominusb(times)
            count = count + 1
         end if
      end do
      if ( count > 0 ) dommean(times) = dommean(times) / real(count)
      ! write(0,'(i4,i8,f12.5)')times, count, dommean(times)
   end do
   ! write(0,*)

   ! Remove station mean from O-B values (removes instrumental/model bias):

   do n = 1, obs % num_stations
      do times = 1, num_times
         if ( obs % data(n) % qcflag(times) >= obs_qc_pointer ) then
            obs % data(n) % ominusb(times) = obs % data(n) % ominusb(times) - &
                                             obs % mean_ominusb(n)
            ! obs % mean_omb
         end if
      end do
   end do

end subroutine da_obs_stats

subroutine da_get_distance( num_stations, lat, lon, dis )

   implicit none

   integer, intent(in)       :: num_stations
   real, intent(in)          :: lat(1:num_stations)
   real, intent(in)          :: lon(1:num_stations)
   real, intent(out)         :: dis(1:num_stations,1:num_stations)

   integer                   :: n1, n2
   real                      :: pi_over_2, colat1, colat2, londiff
   real                      :: dist

   pi_over_2 = 0.5 * pi

   dis(1:num_stations,1:num_stations) = 0.0
   do n1 = 1, num_stations
      colat1 = pi_over_2 - lat(n1)

      do n2 = n1, num_stations
                  if ( n1 /= n2 ) then
            colat2 = pi_over_2 - lat(n2)
            londiff = abs(lon(n2) - lon(n1))

            dist = acos( cos(colat1) * cos(colat2) + &
                         sin(colat1) * sin(colat2) * cos(londiff) ) !in radians
            dis(n1,n2) = earth_radius * dist                ! in km
         end if
            
      end do
   end do

end subroutine da_get_distance

subroutine da_bin_covariance( num_stations, num_times, max_distance, obs, obs_err_used, lvl)

   implicit none

   integer, intent(in)       :: num_stations
   integer, intent(in)       :: num_times
   real, intent(in)          :: max_distance
   type (obs_type), intent(inout) :: obs
   real, intent(in)          :: obs_err_used
   integer, intent(in)       :: lvl           

   integer                   :: b, n1, n2, times, sum_b
   real                      :: bin_width, bin_dis
   real                      :: sum_dis, sum_covar, sum2covar
   real                      :: covar, gaussian
   real                      :: x, y, sum_x, sum_x2, sum_xy, sum_y
   real                      :: gradient, lengthscale, intercept
   real                      :: bk_err_var, bk_err_stdv, ob_err_stdv
   real                      :: avg_dis(1:num_bins)
   real                      :: coverr(1:num_bins)
   integer                   :: sum_obs(1:num_bins)
   logical                   :: bin_pass(1:num_bins)

   avg_dis(:) = 0.0
   coverr(:) = 0.0
   obs % cov(:) = 0.0
   sum_obs(:) = 0

   bin_width = max_distance / real(num_bins)

   ! Calculate covariance and sum for all good obs:

   do b = 1, num_bins
      bin_dis = real(b-1) * bin_width
      sum_dis = 0.0
      sum_covar = 0.0
      sum2covar = 0.0

      do n1 = 1, num_stations
         do n2 = n1, num_stations
            if ( obs % dis(n1,n2) >= bin_dis .and. &
                 obs % dis(n1,n2) < bin_dis + bin_width .and. &
                 obs % dis(n1,n2) /= 0.0 ) then 

               do times = 1, num_times
                  if ( obs % data(n2) % qcflag(times) >= obs_qc_pointer .and. &
                     obs % data(n1) % qcflag(times) >= obs_qc_pointer ) then 

                     covar = obs % data(n2) % ominusb(times) * &
                             obs % data(n1) % ominusb(times)

                     sum_obs(b) = sum_obs(b) + 1
                     sum_dis = sum_dis + obs % dis(n1,n2)
                     sum_covar = covar + sum_covar
                     sum2covar = covar**2 + sum2covar

                  end if
               end do

            end if
         end do
      end do
      
      ! write(6,'(2i8,2f15.5)')b, sum_obs(b), sum_covar, obs % cov(b)

      ! Calculate average separation and covariance of obs:
      if ( sum_obs(b) > 0 ) then
         avg_dis(b) = sum_dis / real(sum_obs(b))
         obs % cov(b) = sum_covar / real(sum_obs(b))
         sum2covar = sum2covar / real(sum_obs(b))
      end if

      ! Calculate 95% error bar for obs % cov estimate = 1.96 s/sqrt(n-1):
      ! Needs min_obs observations in bin to be considered in Gaussian calc.
      bin_pass(b) = .false.

      if ( sum_obs(b) > min_obs ) then
         coverr(b) = 1.96 * sqrt( sum2covar - obs % cov(b)**2 ) / &
                     sqrt( real(sum_obs(b) - 1) )

         if ( coverr(b) > 0.0 .and. coverr(b) < obs % cov(b) ) then
            bin_pass(b) = .true.
         end if
      end if

   end do

   ! Fit good data to a Gaussian to get lengthscale:
   ! B = B0 exp (-r**2 / 8s**2) => ln B = ln B0 - r**2 / 8s**2
   ! y = gradient * x + intercept => gradient = -1/8s**2, x = r**2, intercept = lnB0

   sum_b = 0
   lengthscale = 0.0
   gradient = 0.0
   
   if ( any(bin_pass) ) then
      sum_x = 0.0
      sum_x2= 0.0
      sum_xy= 0.0
      sum_y = 0.0

      do b = 1, num_bins
         if ( bin_pass(b) ) then
            x = avg_dis(b) * avg_dis(b)
            y = log( obs % cov(b) )
            sum_b = sum_b + 1
            sum_x  = sum_x + x
            sum_x2 = sum_x2 + x * x
            sum_xy = sum_xy + x * y
            sum_y = sum_y + y
         end if
      end do

      if ( sum_b > 1 ) then
         gradient = ( real(sum_b) * sum_xy - sum_x * sum_y ) / &
                    ( real(sum_b) * sum_x2 - sum_x * sum_x )
         intercept = ( sum_x2 * sum_y - sum_x * sum_xy ) / &
                    ( real(sum_b) * sum_x2 - sum_x * sum_x )
         if ( gradient < 0.0 ) then
            lengthscale = sqrt( -1.0 / ( 8.0 * gradient ) )
            bk_err_var = exp(intercept)
         end if
      end if
   end if

   ! Print out covariance data:

   gaussian = 0.0
   do b = 1, num_bins
      if (bin_pass(b)) &
         write(0,'(a)')' Bin  NumObs  Separation(km) Covariance      CovErr        GaussFit'
      if ( gradient < 0.0 ) then
         gaussian = bk_err_var*exp(-0.125*(avg_dis(b)/lengthscale)**2)
      end if

      if (obs % cov(b) /= 0.0 ) then
         write(0,'(i5,i7,3e14.6,l1,e13.6)')b, sum_obs(b), avg_dis(b), &
                                        obs % cov(b), &
                                        coverr(b), bin_pass(b), gaussian
         if (obs % cov(b) /= 0.0 .and. bin_pass(b) ) & 
            write(70+lvl,'(2i7,4e14.6)')b, sum_obs(b), avg_dis(b), obs % cov(b),& 
                                        coverr(b), gaussian
      end if
   end do
   write(0,*)

   if ( sum_b > 1 ) then
      ob_err_stdv = sqrt( obs % stdv_omb**2 - bk_err_var  )
      bk_err_stdv = sqrt( bk_err_var )
      write(0,'(a)')' The following are derived from Gaussian fit (warning):'
      write(0,'(3(a,e14.6),2(a,i3),a)')' Scale =', lengthscale, &
                                       ' km, ob_err_stdv = ', ob_err_stdv, &
                                       ' , bk_err_stdv = ', bk_err_stdv, &
                                       ' using', sum_b, ' /', num_bins, ' bins.'
      write(30,'(4f15.3)') ob_err_stdv, bk_err_stdv, obs_err_used, lengthscale
   end if

end subroutine da_bin_covariance

end program da_tune_obs_hollingsworth2
