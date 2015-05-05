subroutine da_setup_be_regional(xb, be, grid)

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
   type (domain),  intent(in)    :: grid

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
   real*8, allocatable           :: be10_eval_loc(:,:)    ! Temp arrays.
   real*8, allocatable           :: be11_eval_loc(:,:)    ! Temp arrays.
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
   real*8, allocatable           :: be10_eval_glo(:)      ! Global Eigenvalues.
   real*8, allocatable           :: be11_eval_glo(:)      ! Global Eigenvalues.
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
   real*8, allocatable           :: be10_evec_loc(:,:,:)  ! Local Eigenvectors.
   real*8, allocatable           :: be11_evec_loc(:,:,:)  ! Local Eigenvectors.
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
   real*8, allocatable           :: be10_evec_glo(:,:)    ! Global Eigenvectors.
   real*8, allocatable           :: be11_evec_glo(:,:)    ! Global Eigenvectors.
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
   real*8, allocatable           :: be10_rf_lengthscale(:)
   real*8, allocatable           :: be11_rf_lengthscale(:)
#endif
   real*8, allocatable           :: alpha_rf_lengthscale(:)
   real*8, allocatable           :: alpha_rf_scale_factor(:)

   real*8, allocatable           :: evec_loc(:,:,:)        ! Latitudinally varying eigenvectors.
   real*8, allocatable           :: eval_loc(:,:)          ! Latitudinally varying eigenvalues.

   character*5                 :: namv(5)=(/"psi  ","chi_u","t_u  ","rh   ","ps_u "/)
   character*10                :: variable
   character*80                :: var80c
   character(len=15)           :: write_fmt
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

   integer                     :: be_unit, ier, be_rf_unit, be_print_unit, it, idummy

!-----------for interpolating CV5--------------------------------------------------------------
   REAL, ALLOCATABLE           :: reg_psi_ps0(:,:), reg_psi_chi0(:,:), reg_psi_t0(:,:,:), &
                                  reg_psi_ps   (:), reg_psi_chi   (:), reg_psi_t   (:,:), &
                                  reg_psi_ps_be(:), reg_psi_chi_be(:), reg_psi_t_be(:,:)
   REAL, DIMENSION(:,:), ALLOCATABLE:: covm1_be, covm2_be, covm3_be, covm4_be,&
                                       covm1   , covm2   , covm3   , covm4
   REAL, DIMENSION(:),   ALLOCATABLE:: rfls1_be, rfls2_be, rfls3_be, rfls4_be
#ifdef CLOUD_CV
   REAL, DIMENSION(:),   ALLOCATABLE:: rfls6_be, rfls7_be, rfls8_be, rfls9_be, rfls10_be, rfls11_be
#endif
   REAL, DIMENSION(:),   ALLOCATABLE:: eta_be
! For write out the Interpolated CV5 BE (bin_type=5) file:
   integer              :: bin_type_out, num_bins_out, num_bins2d_out
   integer, allocatable :: bin_out(:,:,:), bin2d_out(:,:)
   integer, parameter  :: be_out_unit   = 135

!===========for interpolating CV5==============================================================

   if (trace_use) call da_trace_entry("da_setup_be_regional")
   if( (cv_options == 5) .or. (cv_options == 6) .or. (cv_options == 7) ) then
      write (unit=message(1),fmt='(A,I3)') &
         'Set up background errors for regional application for cv_options = ',cv_options
      write (unit=message(2),fmt='(a)') ' '
      call da_message(message(1:2))
      if ( (cv_options == 7 .or. cv_options == 6) .and. interpolate_stats ) then
         write (unit=message(1),fmt='(A,I3)') 'interpolate_stats is not implemented for cv_options=',cv_options
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   else 
      write (unit=message(1),fmt='(a)') 'cv_options should be 5, 6, or 7'
      call da_error(__FILE__,__LINE__,message(1:1))
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
      write (unit=message(1),fmt='(a,i3)') 'Error in reading be.dat, unit= ',be_unit
      call da_error(__FILE__,__LINE__,message(1:1))
   end if
   read (be_unit) bin_type

!-----------for interpolating CV5--------------------------------------------------------------
   if ( .not. interpolate_stats ) then 
      if (ni /= ix .or. nj /= jy .or. nk /= kz) then
         write(unit=message(1),fmt='(a)')  &
            'Dimensions of the assimilation domain do not match those in the BE file.'
         write(unit=message(2),fmt='(3x,a,3i4)') "in be.dat: ni, nj, nk = ",ni, nj, nk
         write(unit=message(3),fmt='(3x,a,3i4)') "in fg:     ix, jy, kz = ",ix, jy, kz
         write(message(4),'(a)') 'be.dat need to be generated using data consistent with the assimilation domain.'
         call da_error(__FILE__,__LINE__,message(1:4))
      end if
   else  ! when interpolate_stats = true
      ! Must use the domain averaged Reg. Coeff.:
      lat_stats_option = .FALSE.
      ! Must use the global Eigenvector/eigenvalue
      vert_evalue = 1
      write (unit=message(1),fmt='(A,I3,A)') 'interpolate_stats is .true., BE for cv_options = ',cv_options,' will be interpolated.'
      call da_message(message(1:1))
      if (rootproc) write(be_out_unit) ix, jy, kz
      write(6,'(a/("k=",i3,2x,f10.4))') 'xb%sigmah=',(k,xb%sigmah(k),k=1,kz)
      allocate (eta_be(1:nk))
      read(be_unit,iostat=ier) eta_be
      !  If not available from BE file, get it from namelist
      if (ier /= 0) then
         write(message(1),'("ier=",i5,2x,a)') ier, " NO Eta values existed in BE file."
         write(message(2),'(a)') "Will use BE Eta values from the namelist be_eta."
         call da_message(message(1:2))
         rewind(be_unit)
         read(be_unit) idummy, idummy, idummy
         read(be_unit) idummy
         eta_be = be_eta(1:nk)
      endif
      write(6,'(a/("k=",i3,2x,f10.4))') 'eta_be=',(k,eta_be(k),k=1,nk)
      !  If eta values not defined correctly, stop
      if (eta_be(1) == eta_be(nk)) then
         write(message(1),'(a)') 'Wrong eta values found.'
         write(message(2),'(a)') 'The eta levels used in generating be.dat should be specified in namelist &wrfvar10 be_eta.'
         call da_error(__FILE__,__LINE__,message(1:2))
      endif
   end if

!===========for interpolating CV5==============================================================

   allocate (bin(1:ni,1:nj,1:nk))
   allocate (bin2d(1:ni,1:nj))

#ifdef CLOUD_CV
  if ( interpolate_stats ) then
     write(unit=message(1),fmt='(A)') &
          '!!  Option "interpolate_stats" may not work for CV5 with CLOUD_CV options  !!'
     call da_warning(__FILE__,__LINE__,message(1:1))
  end if
  if(cloud_cv_options.eq.2)then
   read (be_unit)num_bins, num_bins2d
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)qrain_th_low, qrain_th_high
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)
  else 
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)num_bins, num_bins2d
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)
  end if
#else
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)num_bins, num_bins2d
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)
#endif
!
!-----------for interpolating CV5--------------------------------------------------------------
! Write out the interpolated CV5 BE:
    if (Interpolate_stats) then
 
! No matter which bin_type BE inputed, after Interpolated, the BE always
! became bin_type = 1 BE, i.e. the global BE.
     bin_type_out   = 5
     num_bins_out   = kz
     num_bins2d_out = 1
     if (rootproc) then 
     write(be_out_unit)bin_type_out
     write(be_out_unit)lat_min, lat_max, binwidth_lat
     write(be_out_unit)hgt_min, hgt_max, binwidth_hgt
     write(be_out_unit)num_bins_out, num_bins2d_out
     endif
      allocate (bin_out(1:ix,1:jy,1:kz))
      allocate (bin2d_out(1:ix,1:jy))
        do k = 1, kz
           bin_out (:,:,k) = k
        enddo
        bin2d_out = 1
      if (rootproc) then
      write(be_out_unit)bin_out
      write(be_out_unit)bin2d_out
      endif
      deallocate (bin_out)
      deallocate (bin2d_out)
    endif
!===========for interpolating CV5==============================================================
!
   ! 1.1 Read in regression coefficients
   allocate  (regcoeff_psi_chi(1:num_bins))
   allocate  (regcoeff_psi_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_ps(1:nk,1:num_bins2d))
   if ( cv_options == 6 ) then
      allocate  (regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d))
      allocate  (regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d))
      allocate  (regcoeff_chi_u_ps(1:nk,1:num_bins2d))
      allocate  (regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d))
      allocate  (regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d))
      allocate  (regcoeff_ps_u_rh(1:nk,1:num_bins2d))
   end if

   if ( (cv_options == 5) .or. (cv_options == 7) ) then
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
   ! 1.2 Fill regression coeff. array for model BE, vertical dimension is kz:

   allocate (be%reg_psi_chi  (1:jy,1:kz))
   allocate (be%reg_psi_t    (1:jy,1:kz,1:kz))
   allocate (be%reg_psi_ps   (1:jy,1:kz))
   if ( cv_options == 6 ) then
      allocate (be%reg_psi_rh   (1:jy,1:kz,1:kz))
      allocate (be%reg_chi_u_t  (1:jy,1:kz,1:kz))
      allocate (be%reg_chi_u_ps (1:jy,1:kz))
      allocate (be%reg_chi_u_rh (1:jy,1:kz,1:kz))
      allocate (be%reg_t_u_rh   (1:jy,1:kz,1:kz))
      allocate (be%reg_ps_u_rh  (1:jy,1:kz))
   end if
!
!-----------for interpolating CV5--------------------------------------------------------------
   if ( interpolate_stats ) then
      allocate (reg_psi_chi0(1:nj,1:nk))
      allocate (reg_psi_ps0 (1:nj,1:nk))
      allocate (reg_psi_t0  (1:nj,1:nk,1:nk))
   end if
!===========for interpolating CV5==============================================================

   be%reg_psi_chi    = 0.
   be%reg_psi_t      = 0.
   be%reg_psi_ps     = 0.
   if ( cv_options == 6 ) then
      be%reg_psi_rh     = 0.
      be%reg_chi_u_t    = 0.
      be%reg_chi_u_ps   = 0.
      be%reg_chi_u_rh   = 0.
      be%reg_t_u_rh     = 0.
   end if

   if ( interpolate_stats ) then
      do k=1,nk
         do j=1,nj
            reg_psi_chi0(j,k) = regcoeff_psi_chi(bin(1,j,k))
            b = bin2d(1,j)
            reg_psi_ps0(j,k)  = regcoeff_psi_ps(k,b)
         end do
      end do
      do k=1,nk
         do i=1,nk
            do j=1,nj
               b = bin2d(1,j)
               reg_psi_t0(j,i,k) = regcoeff_psi_t(i,k,b)
            end do
         end do
      end do
   end if   ! if interpolate_stats

   if ( .not. interpolate_stats ) then
      do k=1,nk
         do j=1,nj
            be%reg_psi_chi(j,k) = psi_chi_factor * regcoeff_psi_chi(bin(1,j,k))
            b = bin2d(1,j)
            be%reg_psi_ps(j,k)  = psi_ps_factor  * regcoeff_psi_ps(k,b)
         end do
      end do
      do k=1,nk
         do i=1,nk
            do j=1,nj
               b = bin2d(1,j)
               be%reg_psi_t(j,i,k) = psi_t_factor * regcoeff_psi_t(i,k,b)
            end do
         end do
      end do

      if ( cv_options == 6 ) then
         do k=1,nk
            do j=1,nj
               b = bin2d(1,j)
               be%reg_ps_u_rh(j,k)  = ps_u_rh_factor  * regcoeff_ps_u_rh(k,b)
               be%reg_chi_u_ps(j,k) = chi_u_ps_factor * regcoeff_chi_u_ps(k,b)
            end do
         end do
         do k=1,nk
            do i=1,nk
               do j=1,nj
                  b = bin2d(1,j)
                  be%reg_psi_rh(j,i,k)   = psi_rh_factor   * regcoeff_psi_rh(i,k,b)
                  be%reg_chi_u_t(j,i,k)  = chi_u_t_factor  * regcoeff_chi_u_t(i,k,b)
                  be%reg_chi_u_rh(j,i,k) = chi_u_rh_factor * regcoeff_chi_u_rh(i,k,b)
                  be%reg_t_u_rh(j,i,k)   = t_u_rh_factor   * regcoeff_t_u_rh(i,k,b)
               end do
            end do
         end do
      end if   ! if cv_options 6
   end if   ! if not interpolate_stats

   deallocate (regcoeff_psi_chi)
   deallocate (regcoeff_psi_t)
   deallocate (regcoeff_psi_ps)
   if ( cv_options == 6 ) then
      deallocate (regcoeff_psi_rh)
      deallocate (regcoeff_chi_u_t)
      deallocate (regcoeff_chi_u_ps)
      deallocate (regcoeff_chi_u_rh)
      deallocate (regcoeff_t_u_rh)
      deallocate (regcoeff_ps_u_rh)
   end if
   ! 1.3 Domain_averaged regression coefficients

  if (.not.lat_stats_option) then
      write (unit=message(1), fmt='(a)') ' '
      write (unit=message(2), fmt='(3x, a)') &
         'Using the averaged regression coefficients for unbalanced part'

!-----------for interpolating CV5--------------------------------------------------------------
      if ( interpolate_stats ) then
         allocate (reg_psi_ps_be (1:nk))
         allocate (reg_psi_chi_be(1:nk))
         allocate (reg_psi_t_be  (1:nk,1:nk))
         do k=1,nk
            reg_psi_ps_be(k)  = sum(reg_psi_ps0 (:,k))/dble(nj)
            reg_psi_chi_be(k) = sum(reg_psi_chi0(:,k))/dble(nj)
         end do
         do m=1,nk
            do k=1,nk
               reg_psi_t_be(k,m)=sum(reg_psi_t0(:,k,m))/dble(nj)
            end do
         end do
         deallocate (reg_psi_chi0)
         deallocate (reg_psi_ps0 )
         deallocate (reg_psi_t0  )
      end if   ! if interpolate_stats
!===========for interpolating CV5==============================================================

      if ( (.not. interpolate_stats) .and. (bin_type /= 5) ) then
         do k=1,kz
             be%reg_psi_ps  (:,k) = sum(be%reg_psi_ps (:,k))/dble(jy)
             be%reg_psi_chi (:,k) = sum(be%reg_psi_chi(:,k))/dble(jy)
         end do
         do m=1,kz
            do k=1,kz
                be%reg_psi_t (:,k,m) = sum(be%reg_psi_t(:,k,m))/dble(jy)
             end do
         end do
         if ( cv_options == 6 ) then
            do k=1,kz
               be%reg_ps_u_rh (:,k) = sum(be%reg_ps_u_rh (:,k))/dble(jy)
               be%reg_chi_u_ps(:,k) = sum(be%reg_chi_u_ps(:,k))/dble(jy)
            end do
            do m=1,kz
               do k=1,kz
                  be%reg_psi_rh  (:,k,m) = sum(be%reg_psi_rh  (:,k,m))/dble(jy)
                  be%reg_chi_u_t (:,k,m) = sum(be%reg_chi_u_t (:,k,m))/dble(jy)
                  be%reg_chi_u_rh(:,k,m) = sum(be%reg_chi_u_rh(:,k,m))/dble(jy)
                  be%reg_t_u_rh  (:,k,m) = sum(be%reg_t_u_rh  (:,k,m))/dble(jy)
               end do
            end do
         end if   ! if cv_options 6
      end if   ! if not interpolate_stats

   else
      write (unit=message(1), fmt='(a)') ' '
      write (unit=message(2), fmt='(3x, a)') &
         'Using the geographically-dependent regression coefficients for unbalanced part'
   end if

   call da_message(message(1:2))
   ! 2.0 Load the eigenvector and eigenvalue

   allocate (be1_eval_loc (1:nj,1:nk))
   allocate (be2_eval_loc (1:nj,1:nk))
   allocate (be3_eval_loc (1:nj,1:nk))
   allocate (be4_eval_loc (1:nj,1:nk))
   allocate (be5_eval_loc (1:nj,1:1))
#ifdef CLOUD_CV
   allocate (be6_eval_loc (1:nj,1:nk))
   allocate (be7_eval_loc (1:nj,1:nk))
   allocate (be8_eval_loc (1:nj,1:nk))
   allocate (be9_eval_loc (1:nj,1:nk))
   allocate (be10_eval_loc (1:nj,1:nk))
   allocate (be11_eval_loc (1:nj,1:nk))
#endif

   if (vert_corr == vert_corr_2) then

      allocate (be1_eval_glo(1:nk))
      allocate (be2_eval_glo(1:nk))
      allocate (be3_eval_glo(1:nk))
      allocate (be4_eval_glo(1:nk))
      allocate (be5_eval_glo(1:1))
#ifdef CLOUD_CV
      allocate (be6_eval_glo(1:nk))
      allocate (be7_eval_glo(1:nk))
      allocate (be8_eval_glo(1:nk))
      allocate (be9_eval_glo(1:nk))
      allocate (be10_eval_glo(1:nk))
      allocate (be11_eval_glo(1:nk))
#endif
      allocate (be1_evec_loc(1:nj,1:nk,1:nk))
      allocate (be2_evec_loc(1:nj,1:nk,1:nk))
      allocate (be3_evec_loc(1:nj,1:nk,1:nk))
      allocate (be4_evec_loc(1:nj,1:nk,1:nk))
      allocate (be5_evec_loc(1:nj,1: 1,1: 1))
#ifdef CLOUD_CV
      allocate (be6_evec_loc(1:nj,1:nk,1:nk))
      allocate (be7_evec_loc(1:nj,1:nk,1:nk))
      allocate (be8_evec_loc(1:nj,1:nk,1:nk))
      allocate (be9_evec_loc(1:nj,1:nk,1:nk))
      allocate (be10_evec_loc(1:nj,1:nk,1:nk))
      allocate (be11_evec_loc(1:nj,1:nk,1:nk))
#endif
      allocate (be1_evec_glo(1:nk,1:nk))
      allocate (be2_evec_glo(1:nk,1:nk))
      allocate (be3_evec_glo(1:nk,1:nk))
      allocate (be4_evec_glo(1:nk,1:nk))
      allocate (be5_evec_glo(1:1,1:1))
#ifdef CLOUD_CV
      allocate (be6_evec_glo(1:nk,1:nk))
      allocate (be7_evec_glo(1:nk,1:nk))
      allocate (be8_evec_glo(1:nk,1:nk))
      allocate (be9_evec_glo(1:nk,1:nk))
      allocate (be10_evec_glo(1:nk,1:nk))
      allocate (be11_evec_glo(1:nk,1:nk))
#endif
   end if
   ! 2.2 Read in the eigenvector and eigenvalue 

#ifdef CLOUD_CV
   do i = 1 , 10 
   if (cloud_cv_options.eq.2 )then
      read (be_unit) variable
   else
      if (i.le.4) then
         read (be_unit) variable
      elseif (i.eq.5) then
         variable = "qcloud"
      elseif (i.eq.6) then
         variable = "qrain"
      elseif (i.eq.7) then
         variable = "qice"
      elseif (i.eq.8) then
         variable = "qsnow"
      elseif (i.eq.9) then
         variable = "qgraupel"
      elseif (i.eq.10) then
         variable = "z-wind"
      end if
   end if
#else
   do i = 1 , 4
   read (be_unit) variable
#endif
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
   do j=1,nj
      b = bin2d(1,j)
      be1_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be1_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
!   write(6,*)'be1_evec_loc(10,10,10)=',be1_evec_loc(10,10,10)
!   write(6,*)'be1_eval_loc(10,10)=',be1_eval_loc(10,10)

   case ('chi_u')
   be % v2 % name = trim(adjustl(variable))
   read (be_unit) nk, num_bins2d
   read (be_unit)  be2_evec_glo
   read (be_unit)  be2_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
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
   do j=1,nj
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
   do j=1,nj
      b = bin2d(1,j)
      be4_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be4_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
!   write(6,*)'be4_evec_loc(10,10,10)=',be4_evec_loc(10,10,10)
!   write(6,*)'be4_eval_loc(10,10)=',be4_eval_loc(10,10)

#ifdef CLOUD_CV

! qcloud  
   case ('qcloud' )      
   be % v6 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be6_evec_glo
   read (be_unit)  be6_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be6_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be6_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be6_eval_glo=1.0e-6
  end if 
! qrain   
   case ('qrain' )      
   be % v7 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be7_evec_glo
   read (be_unit)  be7_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be7_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be7_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be7_eval_glo=1.0e-6
  end if 
! qice    
   case ('qice' )      
   be % v8 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be8_evec_glo
   read (be_unit)  be8_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be8_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be8_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be8_eval_glo=1.0e-6
  end if 
! qsnow   
   case ('qsnow' )      
   be % v9 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be9_evec_glo
   read (be_unit)  be9_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be9_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be9_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be9_eval_glo=1.0e-6
  end if 
  ! qgraupel
   case ('qgraupel' )
   be % v10 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be10_evec_glo
   read (be_unit)  be10_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be10_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be10_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be10_eval_glo=1.0e-6
  end if
   case ('z-wind' )
   be % v11 % name = trim(adjustl(variable))
  if(cloud_cv_options.eq.2)then
   read (be_unit) nk, num_bins2d
   read (be_unit)  be11_evec_glo
   read (be_unit)  be11_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   do j=1,nj
      b = bin2d(1,j)
      be11_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be11_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
   end do
  elseif (cloud_cv_options.eq.3)then
   be11_eval_glo=1.0
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
#ifdef CLOUD_CV
   nk_2d=1
#endif
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
   do j=1,nj
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

   write (unit=message(1),fmt='(3x,A,7A)') 'WRFDA dry control variables are: ', &
      trim(be % v1 % name), ', ', trim(be % v2 % name), ', ', &
      trim(be % v3 % name), ' and ', trim(be % v5 % name)
#ifdef CLOUD_CV
   write (unit=message(1),fmt='(3x,A,11A)') 'WRFDA hydrometeor control variables are: ', &
      trim(be % v6 % name), ', ', trim(be % v7 % name), ', ', &
      trim(be % v8 % name), ' and ', trim(be % v9 % name), ', ', &
      trim(be % v10 % name), ' and ', trim(be % v11 % name) 
#endif
   write (unit=message(2),fmt='(3x,A,A)') &
      'Humidity control variable is ', trim(be % v4 % name)

   call da_message(message(1:2))
!
  if (use_rf) then

! 3.0 Load the scale lengths
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! 3.1 allocate the array for scale lengths
   allocate (rfls1_be(1:nk))
   allocate (rfls2_be(1:nk))
   allocate (rfls3_be(1:nk))
   allocate (rfls4_be(1:nk))
#ifdef CLOUD_CV
      allocate (rfls6_be(1:nk))
      allocate (rfls7_be(1:nk))
      allocate (rfls8_be(1:nk))
      allocate (rfls9_be(1:nk))
      allocate (rfls10_be(1:nk))
      allocate (rfls11_be(1:nk))
#endif
   allocate (be1_rf_lengthscale(1:kz))
   allocate (be2_rf_lengthscale(1:kz))
   allocate (be3_rf_lengthscale(1:kz))
   allocate (be4_rf_lengthscale(1:kz))
   allocate (be5_rf_lengthscale(1:1)) ! YRG, 01/09/2012
#ifdef CLOUD_CV
      allocate (be6_rf_lengthscale(1:kz))
      allocate (be7_rf_lengthscale(1:kz))
      allocate (be8_rf_lengthscale(1:kz))
      allocate (be9_rf_lengthscale(1:kz))
      allocate (be10_rf_lengthscale(1:kz))
      allocate (be11_rf_lengthscale(1:kz))
#endif

! ===================================================================
! This is used for do_normalize = .T., I am not sure if it is working 
! properly (???), more testing needed. (YRG, 01/12/2012)
!
      b=0			! pointer to variable*mode
      allocate(nij(0:0,0:1,0:0))

      kzs=(/kz,kz,kz,kz,1/)
      be%v1%mz=1; be%v2%mz=1;be%v3%mz=1;be%v4%mz=1;be%v5%mz=1; 
      mzs=(/be%v1%mz,be%v2%mz,be%v3%mz,be%v4%mz,be%v5%mz/)
! ====================================================================
   ! 3.2 read in the scale lengths
#ifdef CLOUD_CV
      do i = 1 , 11		! variables loop:
   if (cloud_cv_options.eq.2 )then
      read (be_unit, IOSTAT=k) variable
   else
      if (i.le.5) then
         read (be_unit, IOSTAT=k) variable
      elseif (i.eq.6) then
         variable = "qcloud"
      elseif (i.eq.7) then
         variable = "qrain"
      elseif (i.eq.8) then
         variable = "qice"
      elseif (i.eq.9) then
         variable = "qsnow"
      elseif (i.eq.10) then
         variable = "qgraupel"
      elseif (i.eq.11) then
         variable = "z-wind"
      end if
   end if

#else
   do i = 1 , 5              ! variables loop:
      read (be_unit, IOSTAT=k) variable
#endif
      if (print_detail_be)&
           write(unit=stdout,fmt='(2a)')' Reading lengthscale for: ',trim(adjustl(variable))
      if( k/=0 ) then
         write(unit=message(1),fmt='(A,I0,A,I0,A,A)') 'Error ',k,' reading namv(',i,')=',namv(i)
         call da_warning(__FILE__,__LINE__,message(1:1))
      end if
      select case( trim(adjustl(variable)) )
         case ('psi')
            read(be_unit) rfls1_be
            if (.not.interpolate_stats) be1_rf_lengthscale = rfls1_be
         case ('chi_u')
            read(be_unit) rfls2_be
            if (.not.interpolate_stats) be2_rf_lengthscale = rfls2_be
         case ('t_u')
            read(be_unit) rfls3_be
            if (.not.interpolate_stats) be3_rf_lengthscale = rfls3_be
         case ('rh_u' , 'rh')
            read(be_unit) rfls4_be
            if (.not.interpolate_stats) be4_rf_lengthscale = rfls4_be

#ifdef CLOUD_CV
         case ('qcloud')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls6_be
            else
               ! read(be_unit) 
               !be6_rf_lengthscale = 1.0
               rfls6_be = 1.0
            end if
            if (.not.interpolate_stats) be6_rf_lengthscale = rfls6_be
         case ('qrain')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls7_be
            else
               ! read(be_unit) 
               !be7_rf_lengthscale = 1.0
               rfls7_be = 1.0
            end if
            if (.not.interpolate_stats) be7_rf_lengthscale = rfls7_be
         case ('qice')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls8_be
            else
               ! read(be_unit) 
               !be8_rf_lengthscale = 1.0
               rfls8_be = 1.0
            end if
            if (.not.interpolate_stats) be8_rf_lengthscale = rfls8_be
         case ('qsnow')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls9_be
            else
               ! read(be_unit) 
               !be9_rf_lengthscale = 1.0
               rfls9_be = 1.0
            end if
            if (.not.interpolate_stats) be9_rf_lengthscale = rfls9_be
         case ('qgraupel')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls10_be
            else
               ! read(be_unit)
               rfls10_be = 1.0
            end if
            if (.not.interpolate_stats) be10_rf_lengthscale = rfls10_be
         case ('z-wind')
            if(cloud_cv_options.eq.2)then
               read(be_unit) rfls11_be
            else
               ! read(be_unit)
               rfls11_be = 1.0
            end if
            if (.not.interpolate_stats) be11_rf_lengthscale = rfls11_be
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
      if (print_detail_be) then
! Lengthscale variables (except 5!) are arrays of length "kz", so need to account for this in fmt statement
         write(write_fmt,'(A,I0,A)')'(A,',kz,'F10.5)'
         write(unit=message(1),fmt='(A)') 'BE Length scales:'
         write(unit=message(2),fmt=write_fmt) 'be1= ',be1_rf_lengthscale
         write(unit=message(3),fmt=write_fmt) 'be2= ',be2_rf_lengthscale
         write(unit=message(4),fmt=write_fmt) 'be3= ',be3_rf_lengthscale
         write(unit=message(5),fmt=write_fmt) 'be4= ',be4_rf_lengthscale
         write(unit=message(6),fmt='(A,F10.5)') 'be5= ',be5_rf_lengthscale
#ifdef CLOUD_CV
         write(unit=message(7),fmt=write_fmt) 'be6= ',be6_rf_lengthscale
         write(unit=message(8),fmt=write_fmt) 'be7= ',be7_rf_lengthscale
         write(unit=message(9),fmt=write_fmt) 'be8= ',be8_rf_lengthscale
         write(unit=message(10),fmt=write_fmt) 'be9= ',be9_rf_lengthscale
         write(unit=message(11),fmt=write_fmt) 'be10= ',be10_rf_lengthscale
         write(unit=message(12),fmt=write_fmt) 'be11= ',be11_rf_lengthscale
         call da_message(message(1:12))
#else
         call da_message(message(1:6))
#endif
      end if

  endif
!================== for interpolating CV5 =======================================
   ! 3.3 Interpolate the CV5 BE to the model resolution
   if (Interpolate_Stats) then
 
   ! 3.3.1 Compute the Vertical covariance matrix arrays for input CV5 BE:
 
   ! 3.3.1.1 Allocate the covariance arrays for input CV5 BE
 
      allocate (covm1_be(1:nk,1:nk))
      allocate (covm2_be(1:nk,1:nk))
      allocate (covm3_be(1:nk,1:nk))
      allocate (covm4_be(1:nk,1:nk))
   ! 3.3.1.2 Compute the vertical covariance matrix from Input CV5 BE
   !       eigenvector/eigenvalue:
      call da_Eigen_to_covmatrix(nk, be1_evec_glo, be1_eval_glo, covm1_be)
      call da_Eigen_to_covmatrix(nk, be2_evec_glo, be2_eval_glo, covm2_be)
      call da_Eigen_to_covmatrix(nk, be3_evec_glo, be3_eval_glo, covm3_be)
      call da_Eigen_to_covmatrix(nk, be4_evec_glo, be4_eval_glo, covm4_be)
   ! Deallocate the input CV5 BE Global eigenvector/eigenvalue:
      DEALLOCATE ( be1_eval_glo )
      DEALLOCATE ( be2_eval_glo )
      DEALLOCATE ( be3_eval_glo )
      DEALLOCATE ( be4_eval_glo )
 
      DEALLOCATE ( be1_evec_glo )
      DEALLOCATE ( be2_evec_glo )
      DEALLOCATE ( be3_evec_glo )
      DEALLOCATE ( be4_evec_glo )

   ! Allocate the model CV5 BE Global eigenvector/eigenvalue:
      ALLOCATE ( be1_eval_glo(1:kz) )
      ALLOCATE ( be2_eval_glo(1:kz) )
      ALLOCATE ( be3_eval_glo(1:kz) )
      ALLOCATE ( be4_eval_glo(1:kz) )
 
      ALLOCATE ( be1_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be2_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be3_evec_glo(1:kz,1:kz) )
      ALLOCATE ( be4_evec_glo(1:kz,1:kz) )
 
   ! 3.3.2 Convert the Vertical resolution from input CV5 BE to Model:

   ! 3.3.2.1 Allocate the Covariance matrix arrays for model
 
      allocate (covm1(1:kz,1:kz))
      allocate (covm2(1:kz,1:kz))
      allocate (covm3(1:kz,1:kz))
      allocate (covm4(1:kz,1:kz))
 
   ! 3.3.2.2 Allocate the regression coeff. arrays for model BE
 
      allocate (reg_psi_ps (1:kz))
      allocate (reg_psi_chi(1:kz))
      allocate (reg_psi_t  (1:kz,1:kz))
   ! 3.3.2 Vertical resolution conversion from input BE(nk) to model(kz) BE:
      call da_chg_be_Vres(kz, nk, xb%sigmah, eta_be,&
                        reg_psi_chi_be,reg_psi_ps_be,reg_psi_t_be, &
                        reg_psi_chi   ,reg_psi_ps   ,reg_psi_t   , &
                        covm1_be, covm2_be, covm3_be, covm4_be, &
                        covm1   , covm2   , covm3   , covm4   , &
                        rfls1_be, rfls2_be, rfls3_be, rfls4_be, &
                        be1_rf_lengthscale, be2_rf_lengthscale, &
                        be3_rf_lengthscale, be4_rf_lengthscale)
       
   ! 3.3.3 Deallocate the arrays for input CV5 BE:
      deallocate (reg_psi_ps_be )
      deallocate (reg_psi_chi_be)
      deallocate (reg_psi_t_be  )
 
      deallocate ( rfls1_be )
      deallocate ( rfls2_be )
      deallocate ( rfls3_be )
      deallocate ( rfls4_be )
#ifdef CLOUD_CV
      deallocate ( rfls6_be )
      deallocate ( rfls7_be )
      deallocate ( rfls8_be )
      deallocate ( rfls9_be )
      deallocate ( rfls10_be )
      deallocate ( rfls11_be )
#endif
   ! 3.3.4 Assign the Regression coefficients for model BE:
 
   ! 3.3.4.1 Ps and Chi regression coefficients
      do k=1,kz
        do j=1,jy
          be%reg_psi_ps(j,k)  = reg_psi_ps(k)
          be%reg_psi_chi (j,k)= reg_psi_chi(k)
        enddo
      enddo
!          write(6,*)'be%reg_psi_ps(10,10)=',be%reg_psi_ps(10,10)
!          write(6,*)'be%reg_psi_chi(10,10)=',be%reg_psi_chi(10,10)
   ! 3.3.4.2 Temp. regression coefficients
      do m=1,kz
      do k=1,kz
        do j=1,jy
          be%reg_psi_t(j,k,m) = reg_psi_t(k,m)
        enddo
      enddo
      enddo
!          write(6,*)'be%reg_psi_t(10,10,10)=',be%reg_psi_t(10,10,10)
   ! 3.3.4.3 RWite out the Interpolated BE: Regression coefficients
        if (rootproc) then 
        write(be_out_unit) reg_psi_chi
        write(be_out_unit) reg_psi_ps
        write(be_out_unit) reg_psi_t
        endif
   ! 3.3.4.4 Deallocate the domain-averaged Reg. Coeff. arrays:
 
      deallocate (reg_psi_ps )
      deallocate (reg_psi_chi)
      deallocate (reg_psi_t  )
   ! 3.3.5 Re-compute the Eigenvector/eigenvalue from the model covariance matrix
      call da_gen_eigen(kz, covm1, be1_evec_glo, be1_eval_glo)
      call da_gen_eigen(kz, covm2, be2_evec_glo, be2_eval_glo)
      call da_gen_eigen(kz, covm3, be3_evec_glo, be3_eval_glo)
      call da_gen_eigen(kz, covm4, be4_evec_glo, be4_eval_glo)
   ! 3.3.6 Write out the interpolated CV5 BE
 
   ! 3.3.6.1 Eigenvector/eigenvalue:
      variable = be % v1 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) kz, num_bins2d_out
         write(be_out_unit) be1_evec_glo
         write(be_out_unit) be1_eval_glo
      ! - Because num_bins2d = 1, be1_evec_loc = be1_evec_glo:
         write(be_out_unit) be1_evec_glo
         write(be_out_unit) be1_eval_glo
      endif
      variable = be % v2 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) kz, num_bins2d_out
         write(be_out_unit) be2_evec_glo
         write(be_out_unit) be2_eval_glo
      ! - Because num_bins2d = 1, be2_evec_loc = be2_evec_glo:
         write(be_out_unit) be2_evec_glo
         write(be_out_unit) be2_eval_glo
      endif
      variable = be % v3 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) kz, num_bins2d_out
         write(be_out_unit) be3_evec_glo
         write(be_out_unit) be3_eval_glo
      ! - Because num_bins2d = 1, be3_evec_loc = be3_evec_glo:
         write(be_out_unit) be3_evec_glo
         write(be_out_unit) be3_eval_glo
      endif
      variable = be % v4 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) kz, num_bins2d_out
         write(be_out_unit) be4_evec_glo
         write(be_out_unit) be4_eval_glo
      ! - Because num_bins2d = 1, be4_evec_loc = be4_evec_glo:
         write(be_out_unit) be4_evec_glo
         write(be_out_unit) be4_eval_glo
      endif
      variable = be % v5 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) nk_2d, num_bins2d_out
         write(be_out_unit) be5_evec_glo
         write(be_out_unit) be5_eval_glo
      ! - Because num_bins2d = 1, be5_evec_loc = be5_evec_glo:
         write(be_out_unit) be5_evec_glo
         write(be_out_unit) be5_eval_glo
      endif

   ! 3.3.6.2 Scale-length
      variable = be % v1 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) be1_rf_lengthscale
      endif
      variable = be % v2 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) be2_rf_lengthscale
      endif
      variable = be % v3 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) be3_rf_lengthscale
      endif
      variable = be % v4 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) be4_rf_lengthscale
      endif
      variable = be % v5 % name
      if (rootproc) then
         write(be_out_unit) variable
         write(be_out_unit) be5_rf_lengthscale
      endif 
   ! - Close the output unit:
 
      close(be_out_unit)
   ! 3.3.7 Deallocate the arrays for reading the input CV5 BE:
 
      DEALLOCATE ( be1_eval_loc )
      DEALLOCATE ( be2_eval_loc )
      DEALLOCATE ( be3_eval_loc )
      DEALLOCATE ( be4_eval_loc )
      DEALLOCATE ( be5_eval_loc )
      DEALLOCATE ( be1_evec_loc )
      DEALLOCATE ( be2_evec_loc )
      DEALLOCATE ( be3_evec_loc )
      DEALLOCATE ( be4_evec_loc )
      DEALLOCATE ( be5_evec_loc )
 
   ! Allocate the arrays for Model local BE:
      ALLOCATE ( be1_eval_loc (1:jy,1:kz) )
      ALLOCATE ( be2_eval_loc (1:jy,1:kz) )
      ALLOCATE ( be3_eval_loc (1:jy,1:kz) )
      ALLOCATE ( be4_eval_loc (1:jy,1:kz) )
      ALLOCATE ( be5_eval_loc (1:jy,1:1) )
      ALLOCATE ( be1_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be2_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be3_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be4_evec_loc(1:jy,1:kz,1:kz) )
      ALLOCATE ( be5_evec_loc(1:jy,1:1,1:1) )
 
   endif

   ! 4.0 Check and get the truncated number of the vertical modes
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if (vert_corr == vert_corr_2) then

      ! 4.1 Perform checks on eigenvectors:

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
         call da_check_eof_decomposition(be10_eval_glo(:), be10_evec_glo(:,:), be % v10 % name)
         call da_check_eof_decomposition(be11_eval_glo(:), be11_evec_glo(:,:), be % v11 % name)
       end if
#endif
      end if

      ! 4.2 Truncate in vertical:

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
         call da_get_vertical_truncation(max_vert_var10,be10_eval_glo(:),be % v10)
         call da_get_vertical_truncation(max_vert_var11,be11_eval_glo(:),be % v11)
      else
         be % v6 % mz = xb % mkz
         be % v7 % mz = xb % mkz
         be % v8 % mz = xb % mkz
         be % v9 % mz = xb % mkz
         be % v10% mz = xb % mkz
         be % v11% mz = xb % mkz
      endif
#endif
      if (max_vert_var5 == 0.0) then
         be % v5 % mz = 0
      else
         be % v5 % mz = 1
      end if

      write (unit=stdout,fmt=*) ' '

   else

      ! 4.3 no truncated

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
      be % v10% mz = xb % mkz
      be % v11% mz = xb % mkz
#endif
   end if

   kzs=(/kz,kz,kz,kz,1/)
   mzs=(/be%v1%mz,be%v2%mz,be%v3%mz,be%v4%mz,be%v5%mz/)

   ! 5.0 Initialise control variable space components of header:
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! 5.1 Compute the size of the control variables

   be % mix = ix
   be % mjy = jy

   ! 5.2 Transfer errors to error structure:

   call da_allocate_background_errors(jy, kz, be1_eval_glo, be1_evec_glo, be1_eval_loc, &
                                       be1_evec_loc, be % v1)
   call da_allocate_background_errors(jy, kz, be2_eval_glo, be2_evec_glo, be2_eval_loc, &
                                       be2_evec_loc, be % v2)
   call da_allocate_background_errors(jy, kz, be3_eval_glo, be3_evec_glo, be3_eval_loc, &
                                       be3_evec_loc, be % v3)
   call da_allocate_background_errors(jy, kz, be4_eval_glo, be4_evec_glo, be4_eval_loc, &
                                       be4_evec_loc, be % v4)

   ! 5.2.1 transfer the ps_u variance to be % v5:

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
   call da_allocate_background_errors(jy, kz, be10_eval_glo, be10_evec_glo, be10_eval_loc, &
                                       be10_evec_loc, be % v10)
   call da_allocate_background_errors(jy, kz, be11_eval_glo, be11_evec_glo, be11_eval_loc, &
                                       be11_evec_loc, be % v11)
#endif
   if (print_detail_be) then
      write (unit=stdout,fmt='(3x,a,i10)') "b) Finished eigenvector processing!"
   end if

   if( use_rf )then		! use recursive filters:

   ! 5.3 Convert the scale lengths in the real distance (meter)

      be1_rf_lengthscale(1:kz) = be1_rf_lengthscale(1:kz) * xb%ds
      be2_rf_lengthscale(1:kz) = be2_rf_lengthscale(1:kz) * xb%ds
      be3_rf_lengthscale(1:kz) = be3_rf_lengthscale(1:kz) * xb%ds
      be4_rf_lengthscale(1:kz) = be4_rf_lengthscale(1:kz) * xb%ds
      be5_rf_lengthscale(1:1)  = be5_rf_lengthscale(1:1)  * xb%ds
#ifdef CLOUD_CV
      be6_rf_lengthscale(1:kz) = be6_rf_lengthscale(1:kz) * xb%ds
      be7_rf_lengthscale(1:kz) = be7_rf_lengthscale(1:kz) * xb%ds
      be8_rf_lengthscale(1:kz) = be8_rf_lengthscale(1:kz) * xb%ds
      be9_rf_lengthscale(1:kz) = be9_rf_lengthscale(1:kz) * xb%ds
      be10_rf_lengthscale(1:kz)= be10_rf_lengthscale(1:kz) * xb%ds
      be11_rf_lengthscale(1:kz)= be11_rf_lengthscale(1:kz) * xb%ds
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
            allocate(nij(0:nb,0:1,0:2))
            read(be_unit)nij	! wavelet indexes
            write(*,'(A,": ")',advance="no")__FILE__
            do i=0,nb		! wavelet-band loop:
               write(*,'(i2,"{",2(i3,","),i3,";",2(i3,","),i3,"}")',advance="no")&
                  i,transpose(nij(i,:,:))
            enddo
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
      call da_check_eof_decomposition(be%v10%val_g(:), be%v10%evec_g(:,:),&
                                     be%v10%name)
      call da_check_eof_decomposition(be%v11%val_g(:), be%v11%evec_g(:,:),&
                                     be%v11%name)
#endif
   end if

   ! 6.1 Close the be unit

   close(be_unit)
   call da_free_unit(be_unit)

   if( use_rf )then
   ! 6.2 Keep the original be % v1, be % v2,...., and lengthscale in the first loop
   !     for the rescaling in the later loops:

      it = 1

      if (max_ext_its > 1) then

          write(unit=message(1),fmt='(A,I4)') '>>> Save the variances and scale-lengths in outer-loop', it
          call da_message(message(1:1))
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
      call da_rescale_background_errors(var_scaling10(1), len_scaling10(1), &
                                         xb % ds, be10_rf_lengthscale, be % v10)
      call da_rescale_background_errors(var_scaling11(1), len_scaling11(1), &
                                         xb % ds, be11_rf_lengthscale, be % v11)
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
       write(be_print_unit,'("mz=",i3,2x,"be%v10%val:"/(10e12.5))') be%v10%mz, be%v10%val(1,:)
       write(be_print_unit,'("mz=",i3,2x,"be%v11%val:"/(10e12.5))') be%v11%mz, be%v11%val(1,:)
#endif
       write(be_print_unit,'(/"scale-length: kz=",i3)') kz
       if (be % ne > 0) then
          write(be_print_unit,'(/"Hybrid assimilation: number of members=",i3)') be % ne
          write(be_print_unit,'("be%v1%rf_alpha:"/(10e12.5))') be % v1 % rf_alpha(:)
          write(be_print_unit,'("be%v2%rf_alpha:"/(10e12.5))') be % v2 % rf_alpha(:)
          write(be_print_unit,'("be%v3%rf_alpha:"/(10e12.5))') be % v3 % rf_alpha(:)
          write(be_print_unit,'("be%v4%rf_alpha:"/(10e12.5))') be % v4 % rf_alpha(:)
          write(be_print_unit,'("be%v5%rf_alpha:"/(10e12.5))') be % v5 % rf_alpha(:)
       endif

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
   deallocate (be10_eval_loc)
   deallocate (be11_eval_loc)
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
      deallocate (be10_rf_lengthscale)
      deallocate (be11_rf_lengthscale)
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
      deallocate (be10_eval_glo)
      deallocate (be11_eval_glo)
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
      deallocate (be10_evec_loc)
      deallocate (be11_evec_loc)
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
      deallocate (be10_evec_glo)
      deallocate (be11_evec_glo)
#endif
   end if

   deallocate (bin)
   deallocate (bin2d)

   if (be % ne > 0) then
      be % alpha % name = 'alpha'
      allocate (alpha_val(1:nk)) ! Not using jy dimension yet.
      allocate (alpha_evec(1:nk,1:nk)) ! Not using jy dimension yet.

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
      allocate (be % alpha % evec(1:nj,1:nk,1:mz))

      if ( anal_type_hybrid_dual_res ) then
         deallocate(be % alpha % val, be % alpha % evec)
         allocate (be % alpha % val(jds_int:jde_int,1:mz))
         allocate (be % alpha % evec(jds_int:jde_int,1:nk,1:mz))
      endif


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


      if ( anal_type_hybrid_dual_res ) then
         alpha_rf_lengthscale(1:mz) = 1000.0 * alpha_corr_scale / (grid%intermediate_grid % dx )
      else
         alpha_rf_lengthscale(1:mz) = 1000.0 * alpha_corr_scale / xb % ds ! Convert km to grid spacings.
      endif


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


