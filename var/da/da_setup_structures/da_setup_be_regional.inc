subroutine da_setup_be_regional(xb, be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors
   !
   ! Updates:
   !
   !       Implementation of multi-variate BE
   !       Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 02/01/2010
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                    ! First guess structure.
   type (be_type), intent(inout) :: be                    ! Back. errors structure.

   logical                do_normalize1      ! Test do_normalize.
   integer             :: i, j, k, m         ! Loop counters.
   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point
   integer             :: bin_type           ! Type of bin to average over.
   integer             :: num_bins           ! Number of bins (3D fields).
   integer             :: num_bins2d         ! Number of bins (3D fields).
   real*8  :: junk                           ! For reading truncated sd, wsd
   real*8  :: lat_min, lat_max, binwidth_lat ! Used if bin_type = 2 (degrees)..
   real*8  :: hgt_min, hgt_max, binwidth_hgt ! Used if bin_type = 2 (m). .

   real*8, allocatable           :: be1_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be2_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be3_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be4_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be5_eval_loc(:,:)     ! Temp arrays.
#ifdef CLOUD_CV
   real*8, allocatable           :: be6_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be7_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be8_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be9_eval_loc(:,:)     ! Temp arrays.
#endif
   real*8, allocatable           :: be1_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be2_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be3_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be4_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be5_eval_glo(:)       ! Global Eigenvalues.
#ifdef CLOUD_CV
   real*8, allocatable           :: be6_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be7_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be8_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be9_eval_glo(:)       ! Global Eigenvalues.
#endif
   real*8, allocatable           :: alpha_val(:)          ! Global Eigenvalues.

   real*8, allocatable           :: be1_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be2_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be3_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be4_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be5_evec_loc(:,:,:)   ! Local Eigenvectors.
#ifdef CLOUD_CV
   real*8, allocatable           :: be6_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be7_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be8_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be9_evec_loc(:,:,:)   ! Local Eigenvectors.
#endif

   real*8, allocatable           :: be1_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be2_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be3_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be4_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be5_evec_glo(:,:)     ! Global Eigenvectors.
#ifdef CLOUD_CV
   real*8, allocatable           :: be6_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be7_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be8_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be9_evec_glo(:,:)     ! Global Eigenvectors.
#endif

   real*8, allocatable           :: alpha_evec(:,:)       ! Global Eigenvectors.

   real*8, allocatable           :: be1_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be2_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be3_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be4_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be5_rf_lengthscale(:)
#ifdef CLOUD_CV
   real*8, allocatable           :: be6_rf_lengthscale(:)
   real*8, allocatable           :: be7_rf_lengthscale(:)
   real*8, allocatable           :: be8_rf_lengthscale(:)
   real*8, allocatable           :: be9_rf_lengthscale(:)
#endif
   real*8, allocatable           :: alpha_rf_lengthscale(:)
   real*8, allocatable           :: alpha_rf_scale_factor(:)

   real*8, allocatable           :: evec_loc(:,:,:)        ! Latitudinally varying eigenvectors.
   real*8, allocatable           :: eval_loc(:,:)          ! Latitudinally varying eigenvalues.

   character*5                 :: namv(5)=(/"psi  ","chi_u","t_u  ","rh   ","ps_u "/)
   character*10                :: variable
   character*80                :: var80c
   integer                     :: ni, nj, nk, nk_2d, b
   integer                     :: ix, jy, kz, kzs(5), mz, mzs(5)
   real, allocatable   :: regcoeff_psi_chi(:)        ! psi/chi    regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)      ! psi/t      regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! psi/ps     regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)     ! psi/rh     regression cooefficient.
   real, allocatable   :: regcoeff_chi_u_t(:,:,:)    ! chi_u/t    regression coefficient
   real, allocatable   :: regcoeff_chi_u_ps(:,:)     ! chi_u/ps   regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)   ! chi_u/rh   regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)     ! t_u/rh     regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)      ! ps_u/rh    regression coefficient
   real                :: qrain_th_low, qrain_th_high

   integer                     :: be_unit, ier, be_rf_unit, be_print_unit, it

   if (trace_use) call da_trace_entry("da_setup_be_regional")

   if( cv_options == 5 .or. cv_options == 6) then
   write (unit=message(1),fmt='(A,i3)') &
      'Set up background errors for regional application for cv_options = ',cv_options
   write (unit=message(2),fmt='(a)') ' '

   else 
      call wrf_shutdown
      stop "Stopped, due to wrong choice of cv_option. Expects 5 or 6"
   end if

   ix = xb % mix
   jy = xb % mjy
   kz = xb % mkz

   be_rf_unit    = unit_end + 1
   be_print_unit = unit_end + 2

   call da_get_unit(be_unit)
   open(unit=be_unit,file="be.dat", status="old",form="unformatted")

   rewind (be_unit)
   read (be_unit, iostat=ier) ni, nj, nk
   if (ier /= 0) then
      write(unit=*, fmt='(a, i2, a, i3)') &
           'cv_options:', cv_options,' Reading error in unit=',be_unit
      call wrf_shutdown
      stop "Stopped, Please check if proper BE file s provided"
   else
      if ( nk /= kz ) then
         write(message(1),'("{ni,nj,nk}={",3(1x,i0),"} in be.dat but {ix,jy,kz}={",3(1x,i0),"}!")') &
            ni,nj,nk,ix,jy,kz
         call da_error(__FILE__,__LINE__,  &
            message(1:1))
!           (/"Vertical levels in fg and be.dat do not match."/))
      end if
   endif

   allocate (bin(1:ni,1:nj,1:nk))
   allocate (bin2d(1:ni,1:nj))

#ifdef CLOUD_CV
   read (be_unit)bin_type
   read (be_unit)num_bins, num_bins2d
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)qrain_th_low, qrain_th_high
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)
#else
   read (be_unit)bin_type
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)num_bins, num_bins2d
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)
#endif
   ! 1.1 Read in regression coefficients
   allocate  (regcoeff_psi_chi(1:num_bins))
   allocate  (regcoeff_psi_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_ps_u_rh(1:nk,1:num_bins2d))

   regcoeff_psi_chi    = 0.       
   regcoeff_psi_t      = 0.
   regcoeff_psi_ps     = 0.
   regcoeff_psi_rh     = 0.
   regcoeff_chi_u_t    = 0.
   regcoeff_chi_u_ps   = 0.
   regcoeff_chi_u_rh   = 0.
   regcoeff_t_u_rh     = 0.
   regcoeff_ps_u_rh    = 0.

   if ( cv_options == 5 ) then
   read (be_unit) regcoeff_psi_chi
   read (be_unit) regcoeff_psi_ps  
   read (be_unit) regcoeff_psi_t
   else
   do i = 1 , 9
   read (be_unit) var80c   
   select case( trim(adjustl(var80c)) )
   case ('regcoeff_psi_chi')
   read (be_unit) regcoeff_psi_chi
   case ('regcoeff_psi_t')
   read (be_unit) regcoeff_psi_t
   case ('regcoeff_psi_ps')
   read (be_unit) regcoeff_psi_ps
   case ('regcoeff_psi_rh')
   read (be_unit) regcoeff_psi_rh
   case ('regcoeff_chi_u_t')
   read (be_unit) regcoeff_chi_u_t
   case ('regcoeff_chi_u_ps')
   read (be_unit) regcoeff_chi_u_ps
   case ('regcoeff_chi_u_rh')
   read (be_unit) regcoeff_chi_u_rh
   case ('regcoeff_t_u_rh')
   read (be_unit) regcoeff_t_u_rh
   case ('regcoeff_ps_u_rh')
   read (be_unit) regcoeff_ps_u_rh
   case default;
      message(1)=' Read problem in regression coefficients in BE file '
      write (unit=message(2),fmt='(A,A)') ' Trying to read regression coefficients for variable: ',trim(adjustl(var80c))
      call da_error(__FILE__,__LINE__,message(1:2))
   end select
   end do
   end if

   ! 1.2 Fill regression coeff. array
   allocate (be%reg_psi_chi  (1:jy,1:nk))
   allocate (be%reg_psi_t    (1:jy,1:nk,1:nk))
   allocate (be%reg_psi_ps   (1:jy,1:nk))
   allocate (be%reg_psi_rh   (1:jy,1:nk,1:nk))
   allocate (be%reg_chi_u_t  (1:jy,1:nk,1:nk))
   allocate (be%reg_chi_u_ps (1:jy,1:nk))
   allocate (be%reg_chi_u_rh (1:jy,1:nk,1:nk))
   allocate (be%reg_t_u_rh   (1:jy,1:nk,1:nk))
   allocate (be%reg_ps_u_rh  (1:jy,1:nk))

   be%reg_psi_chi    = 0.
   be%reg_psi_t      = 0.
   be%reg_psi_ps     = 0.
   be%reg_psi_rh     = 0.
   be%reg_chi_u_t    = 0.
   be%reg_chi_u_ps   = 0.
   be%reg_chi_u_rh   = 0.
   be%reg_t_u_rh     = 0.

   do k=1,nk
      do j =1, jy
         b = bin(1,j,k)
         be%reg_psi_chi(j,k) = psi_chi_factor * regcoeff_psi_chi(b)
      end do
   end do

   do j=1,jy
      b = bin2d(1,j)
      do k=1,nk
         be%reg_psi_ps(j,k)   = psi_ps_factor   * regcoeff_psi_ps(k,b)
         be%reg_ps_u_rh(j,k)  = ps_u_rh_factor  * regcoeff_ps_u_rh(k,b)
         be%reg_chi_u_ps(j,k) = chi_u_ps_factor * regcoeff_chi_u_ps(k,b)
      end do
   end do

   do j=1,jy
      b = bin2d(1,j)
      do i=1,nk
         do k=1,nk
            be%reg_psi_t(j,i,k)   = psi_t_factor   *  regcoeff_psi_t(i,k,b)
            be%reg_psi_rh(j,i,k)     = psi_rh_factor     *  regcoeff_psi_rh(i,k,b)
            be%reg_chi_u_t(j,i,k) = chi_u_t_factor *  regcoeff_chi_u_t(i,k,b)
            be%reg_chi_u_rh(j,i,k)   = chi_u_rh_factor   *  regcoeff_chi_u_rh(i,k,b)
            be%reg_t_u_rh(j,i,k)  = t_u_rh_factor  *  regcoeff_t_u_rh(i,k,b)
         end do
      end do
   end do

   deallocate (regcoeff_psi_chi)
   deallocate (regcoeff_psi_t)
   deallocate (regcoeff_psi_ps)
   deallocate (regcoeff_psi_rh)
   deallocate (regcoeff_chi_u_t)
   deallocate (regcoeff_chi_u_ps)
   deallocate (regcoeff_chi_u_rh)
   deallocate (regcoeff_t_u_rh)
   deallocate (regcoeff_ps_u_rh)

   ! 1.3 Domain_averaged regression coefficients

  if (.not.lat_stats_option) then
      write (unit=message(3), fmt='(3x, a)') &
         'Using the averaged regression coefficients for unbalanced part'

      do k=1,nk
         be%reg_psi_ps  (1:num_bins2d,k) = sum(be%reg_psi_ps  (1:num_bins2d,k))/dble(num_bins2d)
         be%reg_ps_u_rh (1:num_bins2d,k) = sum(be%reg_ps_u_rh (1:num_bins2d,k))/dble(num_bins2d)
         be%reg_chi_u_ps(1:num_bins2d,k) = sum(be%reg_chi_u_ps(1:num_bins2d,k))/dble(num_bins2d)
      end do

      do m=1,nk
         do k=1,nk

           be%reg_psi_t  (1:num_bins2d,k,m)= sum(be%reg_psi_t(1:num_bins2d,k,m))/dble(num_bins2d)
           be%reg_psi_rh    (1:num_bins2d,k,m)= sum(be%reg_psi_rh  (1:num_bins2d,k,m))/dble(num_bins2d)
           be%reg_chi_u_t(1:num_bins2d,k,m)= sum(be%reg_chi_u_t(1:num_bins2d,k,m))/dble(num_bins2d)
           be%reg_chi_u_rh  (1:num_bins2d,k,m)= sum(be%reg_chi_u_rh  (1:num_bins2d,k,m))/dble(num_bins2d)
           be%reg_t_u_rh (1:num_bins2d,k,m)= sum(be%reg_t_u_rh (1:num_bins2d,k,m))/dble(num_bins2d)

         end do
      end do
   else
      write (unit=message(3), fmt='(3x, a)') &
         'Using the latitude-dependent regression coefficients for unbalanced part'
   end if

   call da_message(message(1:3))

   ! 2.0 Load the eigenvector and eigenvalue

   allocate (be1_eval_loc (1:jy,1:kz))
   allocate (be2_eval_loc (1:jy,1:kz))
   allocate (be3_eval_loc (1:jy,1:kz))
   allocate (be4_eval_loc (1:jy,1:kz))
   allocate (be5_eval_loc (1:jy,1:1))
#ifdef CLOUD_CV
   allocate (be6_eval_loc (1:jy,1:kz))
   allocate (be7_eval_loc (1:jy,1:kz))
   allocate (be8_eval_loc (1:jy,1:kz))
   allocate (be9_eval_loc (1:jy,1:kz))
#endif

   if (vert_corr == vert_corr_2) then

      allocate (be1_eval_glo(1:kz))
      allocate (be2_eval_glo(1:kz))
      allocate (be3_eval_glo(1:kz))
      allocate (be4_eval_glo(1:kz))
      allocate (be5_eval_glo(1:1))
#ifdef CLOUD_CV
      allocate (be6_eval_glo(1:kz))
      allocate (be7_eval_glo(1:kz))
      allocate (be8_eval_glo(1:kz))
      allocate (be9_eval_glo(1:kz))
#endif
      allocate (be1_evec_loc(1:jy,1:kz,1:kz))
      allocate (be2_evec_loc(1:jy,1:kz,1:kz))
      allocate (be3_evec_loc(1:jy,1:kz,1:kz))
      allocate (be4_evec_loc(1:jy,1:kz,1:kz))
      allocate (be5_evec_loc(1:jy,1: 1,1: 1))
#ifdef CLOUD_CV
      allocate (be6_evec_loc(1:jy,1:kz,1:kz))
      allocate (be7_evec_loc(1:jy,1:kz,1:kz))
      allocate (be8_evec_loc(1:jy,1:kz,1:kz))
      allocate (be9_evec_loc(1:jy,1:kz,1:kz))
#endif
      allocate (be1_evec_glo(1:kz,1:kz))
      allocate (be2_evec_glo(1:kz,1:kz))
      allocate (be3_evec_glo(1:kz,1:kz))
      allocate (be4_evec_glo(1:kz,1:kz))
      allocate (be5_evec_glo(1:1,1:1))
#ifdef CLOUD_CV
      allocate (be6_evec_glo(1:kz,1:kz))
      allocate (be7_evec_glo(1:kz,1:kz))
      allocate (be8_evec_glo(1:kz,1:kz))
      allocate (be9_evec_glo(1:kz,1:kz))
#endif
   end if

   ! 2.2 Read in the eigenvector and eigenvalue 

#ifdef CLOUD_CV
   do i = 1 , 8
#else
   do i = 1 , 4
#endif
    read (be_unit) variable
   select case( trim(adjustl(variable)) )
   case ('psi')
   be % v1 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be1_evec_glo
   read (be_unit)  be1_eval_glo
   if( i == 1) then
   allocate (evec_loc(1:nk,1:nk,1:num_bins2d))
   allocate (eval_loc(1:nk,     1:num_bins2d))
   end if
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,jy
      b = bin2d(1,j)
      be1_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be1_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do

   case ('chi_u')
   be % v2 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be2_evec_glo
   read (be_unit)  be2_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,jy
      b = bin2d(1,j)
      be2_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be2_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do

   case ('t_u')
   be % v3 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be3_evec_glo
   read (be_unit)  be3_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,jy
      b = bin2d(1,j)
      be3_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be3_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do

   case ('rh_u' , 'rh' )
   be % v4 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be4_evec_glo
   read (be_unit)  be4_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,jy
      b = bin2d(1,j)
      be4_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be4_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do

#ifdef CLOUD_CV

! qcloud  
   case ('qcloud' )      
   be % v6 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be6_evec_glo
   read (be_unit)  be6_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
  if(cloud_cv_options.eq.2)then
   do j=1,jy
      b = bin2d(1,j)
      be6_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be6_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  else
   be6_eval_glo=1.0e-6
  end if 
! qrain   
   case ('qrain' )      
   be % v7 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be7_evec_glo
   read (be_unit)  be7_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
  if(cloud_cv_options.eq.2)then
   do j=1,jy
      b = bin2d(1,j)
      be7_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be7_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  else
   be7_eval_glo=1.0e-6
  end if 
! qice    
   case ('qice' )      
   be % v8 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be8_evec_glo
   read (be_unit)  be8_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
  if(cloud_cv_options.eq.2)then
   do j=1,jy
      b = bin2d(1,j)
      be8_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be8_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  else
   be8_eval_glo=1.0e-6
  end if 
! qsnow   
   case ('qsnow' )      
   be % v9 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be9_evec_glo
   read (be_unit)  be9_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
  if(cloud_cv_options.eq.2)then
   do j=1,jy
      b = bin2d(1,j)
      be9_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be9_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  else
   be9_eval_glo=1.0e-6
  end if 

#endif

   case default;
      message(1)=' Read problem in eigen vaectors/values in BE file '
      write (unit=message(2),fmt='(A,A)') ' Trying to read Eigenvectors for variable: ',trim(adjustl(variable))
      call da_error(__FILE__,__LINE__,message(1:2))
   end select
   end do

   deallocate (evec_loc)
   deallocate (eval_loc)

   ! 2.2.5 Control variable ps_u
   read (be_unit) variable
   read (be_unit) nk_2d, num_bins2d
   allocate (evec_loc(1:nk_2d,1:nk_2d,1:num_bins2d))
   allocate (eval_loc(1:nk_2d,        1:num_bins2d))
   read (be_unit)  be5_evec_glo
   read (be_unit)  be5_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   if( trim(adjustl(variable)) /= 'ps_u' ) then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected ps_u but got ',trim(adjustl(variable))
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v5 % name = trim(adjustl(variable))
   do j=1,jy
      b = bin2d(1,j)
      be5_evec_loc(j,1:1,1:1) = evec_loc(1:1,1:1,b)
      be5_eval_loc(j,1:1 ) = eval_loc(1:1,b)
   end do

   deallocate (evec_loc)
   deallocate (eval_loc)

!
   if(use_radarobs .and. use_radar_rf .or. use_rad .and. crtm_cloud) then  
      if ( cloud_cv_options == 1 ) be % v4 % name = 'qt   '
   end if

   write (unit=message(1),fmt='(3x,A,7A)') 'WRF-Var dry control variables are:', &
      trim(be % v1 % name), ', ', trim(be % v2 % name), ', ', &
      trim(be % v3 % name), ' and ', trim(be % v5 % name)
#ifdef CLOUD_CV
   write (unit=message(1),fmt='(3x,A,7A)') 'WRF-Var wet control variables are:', &
      trim(be % v6 % name), ', ', trim(be % v7 % name), ', ', &
      trim(be % v8 % name), ' and ', trim(be % v9 % name)
#endif
   write (unit=message(2),fmt='(3x,A,A)') &
      'Humidity control variable is ', trim(be % v4 % name)

   call da_message(message(1:2))
!
   ! 3.0 Check and get the truncated number of the vertical modes
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if (vert_corr == vert_corr_2) then

      ! 3.1 Perform checks on eigenvectors:

      if (test_statistics) then
         call da_check_eof_decomposition(be1_eval_glo(:), be1_evec_glo(:,:), be % v1 % name)
         call da_check_eof_decomposition(be2_eval_glo(:), be2_evec_glo(:,:), be % v2 % name)
         call da_check_eof_decomposition(be3_eval_glo(:), be3_evec_glo(:,:), be % v3 % name)
         call da_check_eof_decomposition(be4_eval_glo(:), be4_evec_glo(:,:), be % v4 % name)
#ifdef CLOUD_CV
       if(cloud_cv_options.eq.2)then
         call da_check_eof_decomposition(be6_eval_glo(:), be6_evec_glo(:,:), be % v6 % name)
         call da_check_eof_decomposition(be7_eval_glo(:), be7_evec_glo(:,:), be % v7 % name)
         call da_check_eof_decomposition(be8_eval_glo(:), be8_evec_glo(:,:), be % v8 % name)
         call da_check_eof_decomposition(be9_eval_glo(:), be9_evec_glo(:,:), be % v9 % name)
       end if
#endif
      end if

      ! 3.2 Truncate in vertical:

      call da_get_vertical_truncation(max_vert_var1, be1_eval_glo(:), be % v1)
      call da_get_vertical_truncation(max_vert_var2, be2_eval_glo(:), be % v2)
      call da_get_vertical_truncation(max_vert_var3, be3_eval_glo(:), be % v3)
      call da_get_vertical_truncation(max_vert_var4, be4_eval_glo(:), be % v4)
#ifdef CLOUD_CV
      if (cloud_cv_options.eq.2) then
         call da_get_vertical_truncation(max_vert_var6, be6_eval_glo(:), be % v6)
         call da_get_vertical_truncation(max_vert_var7, be7_eval_glo(:), be % v7)
         call da_get_vertical_truncation(max_vert_var8, be8_eval_glo(:), be % v8)
         call da_get_vertical_truncation(max_vert_var9, be9_eval_glo(:), be % v9)
      else
         be % v6 % mz = xb % mkz
         be % v7 % mz = xb % mkz
         be % v8 % mz = xb % mkz
         be % v9 % mz = xb % mkz
      endif
#endif
      if (max_vert_var5 == 0.0) then
         be % v5 % mz = 0
      else
         be % v5 % mz = 1
      end if

      write (unit=stdout,fmt=*) ' '

   else

      ! 3.3 no truncated

      be % v1 % mz = xb % mkz
      be % v2 % mz = xb % mkz
      be % v3 % mz = xb % mkz
      be % v4 % mz = xb % mkz
      be % v5 % mz = xb % mkz
#ifdef CLOUD_CV
      be % v6 % mz = xb % mkz
      be % v7 % mz = xb % mkz
      be % v8 % mz = xb % mkz
      be % v9 % mz = xb % mkz
#endif
   end if
   kzs=(/kz,kz,kz,kz,1/)
   mzs=(/be%v1%mz,be%v2%mz,be%v3%mz,be%v4%mz,be%v5%mz/)

   ! 4.0 Initialise control variable space components of header:
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! 4.1 Compute the size of the control variables

   be % mix = ix
   be % mjy = jy

   ! 4.2 Transfer errors to error structure:

   call da_allocate_background_errors(jy, kz, be1_eval_glo, be1_evec_glo, be1_eval_loc, &
                                       be1_evec_loc, be % v1)
   call da_allocate_background_errors(jy, kz, be2_eval_glo, be2_evec_glo, be2_eval_loc, &
                                       be2_evec_loc, be % v2)
   call da_allocate_background_errors(jy, kz, be3_eval_glo, be3_evec_glo, be3_eval_loc, &
                                       be3_evec_loc, be % v3)
   call da_allocate_background_errors(jy, kz, be4_eval_glo, be4_evec_glo, be4_eval_loc, &
                                       be4_evec_loc, be % v4)

   ! 4.2.1 transfer the ps_u variance to be % v5:

   call da_allocate_background_errors(jy,  1, be5_eval_glo, be5_evec_glo, be5_eval_loc, &
                                       be5_evec_loc, be % v5)
#ifdef CLOUD_CV
   call da_allocate_background_errors(jy, kz, be6_eval_glo, be6_evec_glo, be6_eval_loc, &
                                       be6_evec_loc, be % v6)
   call da_allocate_background_errors(jy, kz, be7_eval_glo, be7_evec_glo, be7_eval_loc, &
                                       be7_evec_loc, be % v7)
   call da_allocate_background_errors(jy, kz, be8_eval_glo, be8_evec_glo, be8_eval_loc, &
                                       be8_evec_loc, be % v8)
   call da_allocate_background_errors(jy, kz, be9_eval_glo, be9_evec_glo, be9_eval_loc, &
                                       be9_evec_loc, be % v9)
#endif
   if (print_detail_be) then
      write (unit=stdout,fmt='(3x,a,i10)') "b) Finished eigenvector processing!"
   end if

   if( use_rf )then		! use recursive filters:
      ! 5.0 Load the scale lengths
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~

      ! 5.1 allocate the array for scale lengths

      allocate (be1_rf_lengthscale(1:nk))
      allocate (be2_rf_lengthscale(1:nk))
      allocate (be3_rf_lengthscale(1:nk))
      allocate (be4_rf_lengthscale(1:nk))
      allocate (be5_rf_lengthscale(1:nk))
#ifdef CLOUD_CV
      allocate (be6_rf_lengthscale(1:nk))
      allocate (be7_rf_lengthscale(1:nk))
      allocate (be8_rf_lengthscale(1:nk))
      allocate (be9_rf_lengthscale(1:nk))
#endif
      b=0			! pointer to variable*mode
      allocate(nij(0:0,0:1,0:0))
      ! 5.2 read in the scale lengths
#ifdef CLOUD_CV
      do i = 1 , 9		! variables loop:
#else
      do i = 1 , 5              ! variables loop:
#endif
         read (be_unit,IOSTAT=k) variable
         if (print_detail_be)&
          write(unit=stdout,fmt='(2a)')' Reading lengthscale for: ',trim(adjustl(variable))
         if( k/=0 ) &
            print'("Error ",I0," reading namv(",I0,")=",A,"/=",A,", do_normalize=",L2)', &
               k,i,namv(i),variable,do_normalize
         select case( trim(adjustl(variable)) )
            case ('psi')
               read(be_unit) be1_rf_lengthscale
            case ('chi_u')
               read(be_unit) be2_rf_lengthscale
            case ('t_u')
               read(be_unit) be3_rf_lengthscale
            case ('rh_u' , 'rh')
               read(be_unit) be4_rf_lengthscale
#ifdef CLOUD_CV
            case ('qcloud')
             if(cloud_cv_options.eq.2)then
               read(be_unit) be6_rf_lengthscale
             else
               read(be_unit) 
              be6_rf_lengthscale = 5.0
             end if
            case ('qrain')
             if(cloud_cv_options.eq.2)then
               read(be_unit) be7_rf_lengthscale
             else
               read(be_unit) 
              be7_rf_lengthscale = 5.0
             end if
            case ('qice')
             if(cloud_cv_options.eq.2)then
               read(be_unit) be8_rf_lengthscale
             else
               read(be_unit) 
              be8_rf_lengthscale = 5.0
             end if
            case ('qsnow')
             if(cloud_cv_options.eq.2)then
               read(be_unit) be9_rf_lengthscale
             else
               read(be_unit) 
              be9_rf_lengthscale = 5.0
             end if
#endif
            case ('ps_u')
               read(be_unit) be5_rf_lengthscale(1:1)
            case default;
               message(1)='Read problem in lengthscales in be.dat'
               write(message(2),'("Trying to read lengthscales for variable ",I0,": ",A)')i,trim(adjustl(variable))
               call da_error(__FILE__,__LINE__,message(1:2))
         endselect
         if( do_normalize )then
            read(be_unit)do_normalize1
            if( do_normalize.neqv.do_normalize1 ) &
               call da_error(__FILE__,__LINE__,(/namv(i)//": do_normalize.neqv.do_normalize1"/))
            read(be_unit)nij
            if( i==1 )allocate(be%sd(nij(0,1,0),nij(0,0,0),sum(mzs)))
            do k=1,mzs(i)
               read(be_unit)be%sd(:,:,b+k)
            enddo
            do k=mzs(i)+1,kzs(i)! read and discard truncated modes:
               read(be_unit)(junk,j=1,nij(0,1,0)*nij(0,0,0))
            enddo
            write(*,'(A,": |sd[",A,"]|=",es9.3)')__FILE__,namv(i),sqrt(sum(be%sd(:,:,b+1:b+mzs(i))**2))
            b=b+mzs(i)		! point to next variable.
         endif
      enddo
#ifdef CLOUD_CV
      if (print_detail_be) then
      print*,'length scale'
      print*,'be1= ',be1_rf_lengthscale
      print*,'be2= ',be2_rf_lengthscale
      print*,'be3= ',be3_rf_lengthscale
      print*,'be4= ',be4_rf_lengthscale
      print*,'be5= ',be5_rf_lengthscale
      print*,'be6= ',be6_rf_lengthscale
      print*,'be7= ',be7_rf_lengthscale
      print*,'be8= ',be8_rf_lengthscale
      print*,'be9= ',be9_rf_lengthscale
      end if
#endif

      ! 5.3 Convert the scale lengths in the real distance (meter)

      be1_rf_lengthscale(1:nk) = be1_rf_lengthscale(1:nk) * xb%ds
      be2_rf_lengthscale(1:nk) = be2_rf_lengthscale(1:nk) * xb%ds
      be3_rf_lengthscale(1:nk) = be3_rf_lengthscale(1:nk) * xb%ds
      be4_rf_lengthscale(1:nk) = be4_rf_lengthscale(1:nk) * xb%ds
      be5_rf_lengthscale(1:1)  = be5_rf_lengthscale(1:1)  * xb%ds
#ifdef CLOUD_CV
      be6_rf_lengthscale(1:nk) = be6_rf_lengthscale(1:nk) * xb%ds
      be7_rf_lengthscale(1:nk) = be7_rf_lengthscale(1:nk) * xb%ds
      be8_rf_lengthscale(1:nk) = be8_rf_lengthscale(1:nk) * xb%ds
      be9_rf_lengthscale(1:nk) = be9_rf_lengthscale(1:nk) * xb%ds
#endif
   else				! use wavelets:
      read(be_unit)k		! trimmed string length
      if( k>len(variable) )then
         write(message(1),'(i0,">",i0," for use_rf=.false.")')k,len(variable)
         call da_error(__FILE__,__LINE__,message(1:1))
      endif
      read(be_unit)variable(1:k)
      if( variable(1:k) /= 'wavelet' )then
         write(message(1),'(A,"/=''wavelet''")')variable(1:k)
         call da_error(__FILE__,__LINE__,message(1:1))
      endif
      b = 0			! pointer to variable*mode
      do m=1,5			! variables loop:
         read(be_unit)k,nk
         if( k>len(variable) )then
            write(message(1),'(i0,">",i0," for ",A)')k,len(variable),namv(m)
            call da_error(__FILE__,__LINE__,message(1:1))
         elseif( nk /= kzs(m) )then
            write(message(1),'(i0,"=nk/=kzs(",i0,")=",i0," for ",A)')nk,m,kzs(m),namv(m)
            call da_error(__FILE__,__LINE__,message(1:1))
         endif
         read(be_unit)variable(1:k)
         if( variable(1:k) /= trim(namv(m)) )then
            write(message(1),'(A,"/=",A)')variable(1:k),trim(namv(m))
            call da_error(__FILE__,__LINE__,message(1:1))
         endif
         if( m==1 )then
!           Possibly reassign namelist do_normalize value:
            read(be_unit)do_normalize,namw,lf,nb
!           print*,namv(m),nb,namw,lf
            allocate(nij(0:nb,0:1,0:2))
            read(be_unit)nij	! wavelet indexes
            write(*,'(A,": ")',advance="no")__FILE__
            do i=0,nb		! wavelet-band loop:
               write(*,'(i2,"{",2(i3,","),i3,";",2(i3,","),i3,"}")',advance="no")&
                  i,transpose(nij(i,:,:))
            enddo
            print*
            allocate(be%wsd(nij(0,1,2),nij(0,0,2),sum(mzs)), &
!____________________max of {,i}dwtai wavelet scratch workspace sizes:
                     ws(max(maxval(nij(0,:,2)), &
                            2*floor(.5*(real(maxval(nij(0,:,0)))+lf))+lf-2)))
            if( do_normalize )allocate(be%sd(nij(0,1,0),nij(0,0,0),sum(mzs)))
         endif			! if( m==1 )
         do k=1,mzs(m)
!___________mode-k field & wavelet-coefficient std. devs.:
            read(be_unit)be%wsd(:,:,b+k)
            if( do_normalize )read(be_unit)be%sd(:,:,b+k)
         enddo			! mode-k loop.
         do k=mzs(m)+1,kzs(m)	! read and discard truncated modes:
            read(be_unit)(junk,j=1,nij(0,1,2)*nij(0,0,2))
            if( do_normalize )read(be_unit)(junk,j=1,nij(0,1,0)*nij(0,0,0))
         enddo			! mode-k loop.
         write(*,'(A,": |wsd[",A,"]|=",es9.3)')__FILE__,namv(m),sqrt(sum(be%wsd(:,:,b+1:b+mzs(m))**2))
         if( do_normalize )write(*,'(A,": | sd[",A,"]|=",es9.3)')__FILE__,namv(m),sqrt(sum(be%sd(:,:,b+1:b+mzs(m))**2))
         b = b+mzs(m)		! point to next variable.
      enddo			! variables loop
   endif			! if( use_rf )
   ! 6.0 Perform checks on eigenvectors with be data structure:
   if (test_statistics) then
      call da_check_eof_decomposition(be%v1%val_g(:), be%v1%evec_g(:,:),&
                                     be%v1%name)
      call da_check_eof_decomposition(be%v2%val_g(:), be%v2%evec_g(:,:),&
                                     be%v2%name)
      call da_check_eof_decomposition(be%v3%val_g(:), be%v3%evec_g(:,:),&
                                     be%v3%name)
      call da_check_eof_decomposition(be%v4%val_g(:), be%v4%evec_g(:,:),&
                                     be%v4%name)
#ifdef CLOUD_CV
      call da_check_eof_decomposition(be%v6%val_g(:), be%v6%evec_g(:,:),&
                                     be%v6%name)
      call da_check_eof_decomposition(be%v7%val_g(:), be%v7%evec_g(:,:),&
                                     be%v7%name)
      call da_check_eof_decomposition(be%v8%val_g(:), be%v8%evec_g(:,:),&
                                     be%v8%name)
      call da_check_eof_decomposition(be%v9%val_g(:), be%v9%evec_g(:,:),&
                                     be%v9%name)
#endif
   end if

   ! 6.2 Close the be unit

   close(be_unit)
   call da_free_unit(be_unit)

   if( use_rf )then
      ! 6.3 Keep the original be % v1, be % v2,...., and lengthscale in the first loop
      !     for the rescaling in the later loops:

      it = 1

      if (max_ext_its > 1) then

          print '(/5x,">>> Save the variances and scale-lengths in outer-loop",i2/)', it
          write(be_rf_unit)  kz, jy, ix, be % v1 % mz, be % v2 % mz, be% v3 % mz, &
                                     be % v4 % mz, be % v5 % mz, xb % ds
          write(be_rf_unit) be % v1 % val, be % v2 % val, be% v3 % val, &
                                     be % v4 % val, be % v5 % val, &
             be1_rf_lengthscale, be2_rf_lengthscale, be3_rf_lengthscale, &
             be4_rf_lengthscale, be5_rf_lengthscale
     
          if (print_detail_be ) then
             write(be_print_unit,'("it=",i2,2x,"kz=",i3,2x,"jy=",i4,2x,"ix=",i4,2x,"ds=",e12.5)') &
                                               it, kz, jy, ix, xb % ds
             write(be_print_unit,'("Original val and rf, and mz:",5i5)') &
                      be % v1 % mz, be % v2 % mz, be% v3 % mz, be % v4 % mz, be % v5 % mz
             write(be_print_unit,'("mz=",i3,2x,"be%v1%val:"/(10e12.5))') be%v1%mz, be%v1%val(1,:)
             write(be_print_unit,'("mz=",i3,2x,"be%v2%val:"/(10e12.5))') be%v2%mz, be%v2%val(1,:)
             write(be_print_unit,'("mz=",i3,2x,"be%v3%val:"/(10e12.5))') be%v3%mz, be%v3%val(1,:)
             write(be_print_unit,'("mz=",i3,2x,"be%v4%val:"/(10e12.5))') be%v4%mz, be%v4%val(1,:)
             write(be_print_unit,'("mz=",i3,2x,"be%v5%val:"/(10e12.5))') be%v5%mz, be%v5%val(1,:)
             write(be_print_unit,'(/"scale-length: kz=",i3)') kz
             do i = 1,kz 
               if (i == 1) then
                 write(be_print_unit,'(i3,2x,5e15.5)') i,be1_rf_lengthscale(i), &
                   be2_rf_lengthscale(i), be3_rf_lengthscale(i), be4_rf_lengthscale(i), &
                   be5_rf_lengthscale(i)
               else
                 write(be_print_unit,'(i3,2x,5e15.5)') i,be1_rf_lengthscale(i), &
                   be2_rf_lengthscale(i), be3_rf_lengthscale(i), be4_rf_lengthscale(i)
               endif
             enddo
    
          endif

      endif

      ! 7.0 Apply empirical and recursive filter rescaling factor:
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      call da_rescale_background_errors(var_scaling1(1), len_scaling1(1), &
                                         xb % ds, be1_rf_lengthscale, be % v1)
      call da_rescale_background_errors(var_scaling2(1), len_scaling2(1), &
                                         xb % ds, be2_rf_lengthscale, be % v2)
      call da_rescale_background_errors(var_scaling3(1), len_scaling3(1), &
                                         xb % ds, be3_rf_lengthscale, be % v3)
      call da_rescale_background_errors(var_scaling4(1), len_scaling4(1), &
                                         xb % ds, be4_rf_lengthscale, be % v4)
      call da_rescale_background_errors(var_scaling5(1), len_scaling5(1), &
                                         xb % ds, be5_rf_lengthscale, be % v5)
#ifdef CLOUD_CV
      call da_rescale_background_errors(var_scaling6(1), len_scaling6(1), &
                                         xb % ds, be6_rf_lengthscale, be % v6)
      call da_rescale_background_errors(var_scaling7(1), len_scaling7(1), &
                                         xb % ds, be7_rf_lengthscale, be % v7)
      call da_rescale_background_errors(var_scaling8(1), len_scaling8(1), &
                                         xb % ds, be8_rf_lengthscale, be % v8)
      call da_rescale_background_errors(var_scaling9(1), len_scaling9(1), &
                                         xb % ds, be9_rf_lengthscale, be % v9)
#endif
   endif
   if (print_detail_be ) then

       write(be_print_unit,'("it=",i2,2x,"kz=",i3,2x,"jy=",i4,2x,"ix=",i4,2x,"ds=",e12.5)') &
                                                      it, kz, jy, ix, xb % ds
       write(be_print_unit,'("Loop it=",i2," val and rf, and mz:",5i5)') &
                  it, be % v1 % mz, be % v2 % mz, be% v3 % mz, be % v4 % mz, be % v5 % mz
       write(be_print_unit,'("mz=",i3,2x,"be%v1%val:"/(10e12.5))') be%v1%mz, be%v1%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v2%val:"/(10e12.5))') be%v2%mz, be%v2%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v3%val:"/(10e12.5))') be%v3%mz, be%v3%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v4%val:"/(10e12.5))') be%v4%mz, be%v4%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v5%val:"/(10e12.5))') be%v5%mz, be%v5%val(1,:)
#ifdef CLOUD_CV
       write(be_print_unit,'("mz=",i3,2x,"be%v6%val:"/(10e12.5))') be%v6%mz, be%v6%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v7%val:"/(10e12.5))') be%v7%mz, be%v7%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v8%val:"/(10e12.5))') be%v8%mz, be%v8%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v9%val:"/(10e12.5))') be%v9%mz, be%v9%val(1,:)
#endif
       write(be_print_unit,'(/"scale-length: kz=",i3)') kz
       do i = 1,kz 
         if (i == 1) then
           write(be_print_unit,'(i3,2x,5e15.5)') i, be % v1 % rf_alpha(i), &
                be % v2 % rf_alpha(i), be % v3 % rf_alpha(i), be % v4 % rf_alpha(i), &
                be % v5 % rf_alpha(i)
         else
           write(be_print_unit,'(i3,2x,5e15.5)') i, be % v1 % rf_alpha(i), &
                be % v2 % rf_alpha(i), be % v3 % rf_alpha(i), be % v4 % rf_alpha(i)
       endif
       enddo

   endif

   ! 8.0 deallocate input model state:
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   deallocate (be1_eval_loc)
   deallocate (be2_eval_loc)
   deallocate (be3_eval_loc)
   deallocate (be4_eval_loc)
   deallocate (be5_eval_loc)
#ifdef CLOUD_CV
   deallocate (be6_eval_loc)
   deallocate (be7_eval_loc)
   deallocate (be8_eval_loc)
   deallocate (be9_eval_loc)
#endif
   if( use_rf )then
      deallocate (be1_rf_lengthscale)
      deallocate (be2_rf_lengthscale)
      deallocate (be3_rf_lengthscale)
      deallocate (be4_rf_lengthscale)
      deallocate (be5_rf_lengthscale)
#ifdef CLOUD_CV
      deallocate (be6_rf_lengthscale)
      deallocate (be7_rf_lengthscale)
      deallocate (be8_rf_lengthscale)
      deallocate (be9_rf_lengthscale)
#endif
   endif
   if (vert_corr == vert_corr_2) then
      deallocate (be1_eval_glo)
      deallocate (be2_eval_glo)
      deallocate (be3_eval_glo)
      deallocate (be4_eval_glo)
      deallocate (be5_eval_glo)
#ifdef CLOUD_CV
      deallocate (be6_eval_glo)
      deallocate (be7_eval_glo)
      deallocate (be8_eval_glo)
      deallocate (be9_eval_glo)
#endif
      deallocate (be1_evec_loc)
      deallocate (be2_evec_loc)
      deallocate (be3_evec_loc)
      deallocate (be4_evec_loc)
      deallocate (be5_evec_loc)
#ifdef CLOUD_CV
      deallocate (be6_evec_loc)
      deallocate (be7_evec_loc)
      deallocate (be8_evec_loc)
      deallocate (be9_evec_loc)
#endif
      deallocate (be1_evec_glo)
      deallocate (be2_evec_glo)
      deallocate (be3_evec_glo)
      deallocate (be4_evec_glo)
      deallocate (be5_evec_glo)
#ifdef CLOUD_CV
      deallocate (be6_evec_glo)
      deallocate (be7_evec_glo)
      deallocate (be8_evec_glo)
      deallocate (be9_evec_glo)
#endif
   end if

   deallocate (bin)
   deallocate (bin2d)

   if (be % ne > 0) then
      be % alpha % name = 'alpha'
      allocate (alpha_val(1:kz)) ! Not using jy dimension yet.
      allocate (alpha_evec(1:kz,1:kz)) ! Not using jy dimension yet.

      if ( alpha_vertloc ) then ! Use vertical localization:
         call da_get_unit(be_unit)
         open(unit=be_unit,file='be.vertloc.dat', status='old', form='unformatted')

         read (be_unit)nk
         read (be_unit)alpha_val(1:nk)
         read (be_unit)alpha_evec(1:nk,1:nk)
         close(be_unit)
         call da_free_unit(be_unit)
         be % alpha % mz = nk

         call da_get_vertical_truncation(max_vert_var_alpha, alpha_val, be % alpha)
      else
         be % alpha % mz = 1 ! No vertical localization.
         alpha_val(1) = 1.0
         alpha_val(2:kz) = 0.0
         alpha_evec(:,:) = 1.0
      end if
      mz = be % alpha % mz

!     Alpha eigenvalues and eigenvectors:
      allocate (be % alpha % val(1:jy,1:mz)) ! Not using jy dimension but here for consistency.
      allocate (be % alpha % evec(1:jy,1:kz,1:mz))
      do m = 1, mz
         be % alpha % val(:,m) = sigma_alpha * alpha_val(m)
         do k = 1, nk
            be % alpha % evec(:,k,m) = alpha_evec(k,m)
         end do
      end do

!     Alpha RF lengthscales and variance scaling factors:
      allocate (alpha_rf_lengthscale(1:mz))
      allocate (be % alpha % rf_alpha(1:mz))
      allocate (alpha_rf_scale_factor(1:mz))

      alpha_rf_lengthscale(1:mz) = 1000.0 * alpha_corr_scale / xb % ds ! Convert km to grid spacings.

      call da_calculate_rf_factors( alpha_rf_lengthscale(:), be % alpha % rf_alpha(:), &
                                    alpha_rf_scale_factor(:) )
      do m = 1, mz
         be % alpha % val(:,m) = alpha_rf_scale_factor(m) * be % alpha % val(:,m)
      end do

      if( .not. use_rf ) then
         allocate(be%alpha%wsd(nij(0,1,2),nij(0,0,2),mz))
         call random_number(be%alpha%wsd)			! need to parallelize
         if( do_normalize )then
            allocate(be%alpha%sd(nij(0,1,0),nij(0,0,0),mz))
            call random_number(be%alpha%sd)			! need to parallelize
         endif
      endif

      deallocate(alpha_val)
      deallocate(alpha_evec)
      deallocate(alpha_rf_lengthscale)
      deallocate(alpha_rf_scale_factor)

   else
      be % alpha % mz = 0
   end if

   if (trace_use) call da_trace_exit("da_setup_be_regional")

end subroutine da_setup_be_regional


