subroutine da_setup_be_nmm_regional(xb, be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)  :: xb                    ! First guess structure.
   type (be_type), intent(out) :: be                    ! Back. errors structure.

   integer                     :: i, j, k, m       ! Loop counters.
   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point
   integer             :: bin_type           ! Type of bin to average over.
   integer             :: num_bins           ! Number of bins (3D fields).
   integer             :: num_bins2d         ! Number of bins (3D fields).
   real*8    :: lat_min, lat_max, binwidth_lat ! Used if bin_type = 2 (degrees)..
   real*8    :: hgt_min, hgt_max, binwidth_hgt ! Used if bin_type = 2 (m). .

   real*8, allocatable           :: be1_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be2_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be3_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be4_eval_loc(:,:)     ! Temp arrays.
   real*8, allocatable           :: be5_eval_loc(:,:)     ! Temp arrays.

   real*8, allocatable           :: be1_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be2_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be3_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be4_eval_glo(:)       ! Global Eigenvalues.
   real*8, allocatable           :: be5_eval_glo(:)       ! Global Eigenvalues.

   real*8, allocatable           :: be1_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be2_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be3_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be4_evec_loc(:,:,:)   ! Local Eigenvectors.
   real*8, allocatable           :: be5_evec_loc(:,:,:)   ! Local Eigenvectors.

   real*8, allocatable           :: be1_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be2_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be3_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be4_evec_glo(:,:)     ! Global Eigenvectors.
   real*8, allocatable           :: be5_evec_glo(:,:)     ! Global Eigenvectors.

   real*8, allocatable           :: be1_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be2_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be3_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be4_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be5_rf_lengthscale(:)
   real*8, allocatable           :: alpha_rf_lengthscale(:)

   real*8, allocatable           :: evec_loc(:,:,:)        ! Latitudinally varying eigenvectors.
   real*8, allocatable           :: eval_loc(:,:)          ! Latitudinally varying eigenvalues.

   character*10                :: variable
   integer                     :: ni, nj, nk, nk_2d, b         
   integer                     :: ix, jy, kz
   real*8, allocatable           :: regcoeff1(:)
   real*8, allocatable           :: regcoeff2(:,:)
   real*8, allocatable           :: regcoeff3(:,:,:)
   real                        :: avg,avg2,avg3
   integer                     :: be_unit

   if (trace_use) call da_trace_entry("da_setup_be_nmm_regional")

   be % v1 % name = 'psi  '           ! Streamfunction
   be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
   be % v3 % name = 't_u'             ! Unbalanced temperature.
   be % v4 % name = 'q/qsg'
   be % v5 % name = 'psfc'            ! surface pressure
!clq
   if(use_radarobs .and. use_radar_rf) then
   be % v4 % name = 'qt   '
   end if
!clq end

   write (unit=message(2),fmt='(3x,A,7A)') 'WRF-Var dry control variables are:', &
      trim(be % v1 % name), ', ', trim(be % v2 % name), ', ', &
      trim(be % v3 % name), ' and ', trim(be % v5 % name)

   write (unit=message(3),fmt='(3x,A,A)') &
      'Humidity control variable is ', trim(be % v4 % name)

   write (unit=message(3),fmt='(3x,A,A)') &
      'Humidity control variable is ', trim(be % v4 % name)

   ix = xb % mix
   jy = xb % mjy
   kz = xb % mkz
   call da_get_unit(be_unit)
   open(unit=be_unit,file="be.dat", status="old",form="unformatted")

   rewind (be_unit)
   read (be_unit) ni, nj, nk

   allocate (bin(1:ni,1:nj,1:nk))
   allocate (bin2d(1:ni,1:nj))

   read (be_unit)bin_type
   read (be_unit)lat_min, lat_max, binwidth_lat
   read (be_unit)hgt_min, hgt_max, binwidth_hgt
   read (be_unit)num_bins, num_bins2d
   read (be_unit)bin(1:ni,1:nj,1:nk)
   read (be_unit)bin2d(1:ni,1:nj)

   ! 1.1 Read in regression coefficients


   allocate (regcoeff1(1:num_bins))
   allocate (regcoeff2(1:nk,1:num_bins2d))
   allocate (regcoeff3(1:nk,1:nk,1:num_bins2d))

   read (be_unit) regcoeff1  
   read (be_unit) regcoeff2 
   read (be_unit) regcoeff3  

   ! 1.2 Fill regression coeff. array

!                                47 25 50
   allocate (be%reg_chi(1:jy,1:kz))
   allocate (be%reg_ps (1:jy,1:kz))
   allocate (be%reg_t  (1:jy,1:kz,1:kz))

   do k=1,kz
      do j =1, jy
         b = bin(1,1,k)
         be%reg_chi(j,k) = regcoeff1(b)
      end do
   end do
   do j=1,jy
      b = bin2d(1,1)
      do k=1,kz
         be%reg_ps(j,k) = regcoeff2(k,b)
      end do
   end do

   do j=1,jy
      b = bin2d(1,1)
      do i=1,kz
         do k=1,kz
            be%reg_t(j,i,k) = regcoeff3(i,k,b)
         end do
      end do
   end do

   deallocate (regcoeff1)
   deallocate (regcoeff2)
   deallocate (regcoeff3)


   ! 1.3 Domain_averaged regression coefficients

   if (.not.lat_stats_option) then
      write (unit=message(4), fmt='(3x, a)') &
         'Using the averaged regression coefficients for unbalanced part'

      do k=1,nk
         avg= 0.0
         avg2=0.0
         do j=1,num_bins2d
            avg= avg + be%reg_ps (j,k)/float(num_bins2d)
            avg2= avg2 + be%reg_chi (j,k)/float(num_bins2d)
         end do

         do j=1,num_bins2d
            be%reg_ps(j,k)=avg
            be%reg_chi (j,k)=avg2
         end do
      end do

      do m=1,nk
         do k=1,nk
            avg3= 0.0

            do j=1,num_bins2d
               avg3= avg3 + be%reg_t (j,k,m)/float(num_bins2d)
            end do

            do j=1,num_bins2d
               be%reg_t(j,k,m)=avg3
            end do
         end do
      end do
   else
      write (unit=message(4), fmt='(3x, a)') &
         'Using the latitude-dependent regression coefficients for unbalanced part'
   end if

   call da_message(message(1:4))

   ! 2.0 Load the eigenvector and eigenvalue
   allocate (be1_eval_loc (1:jy,1:nk))
   allocate (be2_eval_loc (1:jy,1:nk))
   allocate (be3_eval_loc (1:jy,1:nk))
   allocate (be4_eval_loc (1:jy,1:nk))
   allocate (be5_eval_loc (1:jy,1:1))

   if (vert_corr == vert_corr_2) then

      allocate (be1_eval_glo(1:nk))
      allocate (be2_eval_glo(1:nk))
      allocate (be3_eval_glo(1:nk))
      allocate (be4_eval_glo(1:nk))
      allocate (be5_eval_glo(1:1))

      allocate (be1_evec_loc(1:jy,1:nk,1:nk))
      allocate (be2_evec_loc(1:jy,1:nk,1:nk))
      allocate (be3_evec_loc(1:jy,1:nk,1:nk))
      allocate (be4_evec_loc(1:jy,1:nk,1:nk))
      allocate (be5_evec_loc(1:jy,1: 1,1: 1))

      allocate (be1_evec_glo(1:nk,1:nk))
      allocate (be2_evec_glo(1:nk,1:nk))
      allocate (be3_evec_glo(1:nk,1:nk))
      allocate (be4_evec_glo(1:nk,1:nk))
      allocate (be5_evec_glo(1:1,1:1))
   end if

   ! 2.2 Read in the eigenvector and eigenvalue 

   ! 2.2.1 Control variable 1 (psi)

   read (be_unit) variable
   read (be_unit) nk, num_bins2d
   read (be_unit)  be1_evec_glo
   read (be_unit)  be1_eval_glo
   allocate (evec_loc(1:nk,1:nk,1:num_bins2d))
   allocate (eval_loc(1:nk,     1:num_bins2d))
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   if (variable(1:3) /= 'psi') then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected psi but got ',variable
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v1 % name = variable
   do j=1,jy
      b = bin2d(1,1)
      be1_evec_loc(j,1:kz,1:kz) = evec_loc(1:kz,1:kz,b)
      be1_eval_loc(j,1:kz  ) = eval_loc(1:kz,b)
   end do

   ! 2.2.2 Control variable 2 (chi_u)

   read (be_unit) variable
   read (be_unit) nk, num_bins2d
   read (be_unit)  be2_evec_glo
   read (be_unit)  be2_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   if (variable(1:5) /= 'chi_u') then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected chi_u but got ',variable
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v2 % name = variable
   do j=1,jy
      b = bin2d(1,1)
      be2_evec_loc(j,1:kz,1:kz) = evec_loc(1:kz,1:kz,b)
      be2_eval_loc(j,1:kz  ) = eval_loc(1:kz,b)
   end do

   ! 2.2.3 Control variable 3 (t_u)
   read (be_unit) variable
   read (be_unit) nk, num_bins2d
   read (be_unit)  be3_evec_glo
   read (be_unit)  be3_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   if (variable(1:3) /= 't_u') then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected t_u but got ',variable
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v3 % name = variable
   do j=1,jy
      b = bin2d(1,1)
      be3_evec_loc(j,1:kz,1:kz) = evec_loc(1:kz,1:kz,b)
      be3_eval_loc(j,1:kz  ) = eval_loc(1:kz,b)
   end do

   ! 2.2.4 Control variable 4 (q/qsg)

   read (be_unit) variable
   read (be_unit) nk, num_bins2d
   read (be_unit)  be4_evec_glo
   read (be_unit)  be4_eval_glo
   read (be_unit)  evec_loc
   read (be_unit)  eval_loc
   if (variable(1:3) /= 'rh') then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected rh but got ',variable
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v4 % name = variable
   do j=1,jy
      b = bin2d(1,1)
      be4_evec_loc(j,1:kz,1:kz) = evec_loc(1:kz,1:kz,b)
      be4_eval_loc(j,1:kz  ) = eval_loc(1:kz,b)
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
   if (variable(1:4) /= 'ps_u') then
      message(1)=' Variable mismatch while transfering eigen values from BE file '
      write (unit=message(2),fmt='(A,A)') ' Expected ps_u but got ',variable
      call da_error(__FILE__,__LINE__,message(1:2))
   end if
   be % v5 % name = variable
   do j=1,jy
      b = bin2d(1,1)
      be5_evec_loc(j,1:1,1:1) = evec_loc(1:1,1:1,b)
      be5_eval_loc(j,1:1 ) = eval_loc(1:1,b)
   end do

   deallocate (evec_loc)
   deallocate (eval_loc)

   ! 3.0 Check and get the truncated number of the vertical modes
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if (vert_corr == vert_corr_2) then

      ! 3.1 Perform checks on eigenvectors:

      if (test_statistics) then
         call da_check_eof_decomposition(be1_eval_glo(:), be1_evec_glo(:,:), be % v1 % name)
         call da_check_eof_decomposition(be2_eval_glo(:), be2_evec_glo(:,:), be % v2 % name)
         call da_check_eof_decomposition(be3_eval_glo(:), be3_evec_glo(:,:), be % v3 % name)
         call da_check_eof_decomposition(be4_eval_glo(:), be4_evec_glo(:,:), be % v4 % name)
      end if

      ! 3.2 Truncate in vertical:

      call da_get_vertical_truncation(max_vert_var1, be1_eval_glo(:), be % v1)
      call da_get_vertical_truncation(max_vert_var2, be2_eval_glo(:), be % v2)
      call da_get_vertical_truncation(max_vert_var3, be3_eval_glo(:), be % v3)
      call da_get_vertical_truncation(max_vert_var4, be4_eval_glo(:), be % v4)

      be % v5 % mz = 1

      write (unit=stdout,fmt=*) ' '

   else

      ! 3.3 no truncated

      be % v1 % mz = xb % mkz
      be % v2 % mz = xb % mkz
      be % v3 % mz = xb % mkz
      be % v4 % mz = xb % mkz
      be % v5 % mz = xb % mkz

   end if

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

   if (print_detail_be) then
      write (unit=stdout,fmt='(3x,a,i10)') "b) Finished eigenvector processing!"
   end if

   ! 5.0 Load the scale lengths
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~

   ! 5.1 allocate the array for scale lengths

   allocate (be1_rf_lengthscale(1:nk))
   allocate (be2_rf_lengthscale(1:nk))
   allocate (be3_rf_lengthscale(1:nk))
   allocate (be4_rf_lengthscale(1:nk))
   allocate (be5_rf_lengthscale(1:nk))

   ! 5.2 read in the scale lengths

   read (be_unit) variable
   read (be_unit) be1_rf_lengthscale

   read (be_unit) variable
   read (be_unit) be2_rf_lengthscale

   read (be_unit) variable
   read (be_unit) be3_rf_lengthscale

   read (be_unit) variable
   read (be_unit) be4_rf_lengthscale

   read (be_unit) variable
   read (be_unit) be5_rf_lengthscale(1:1)
   be%v5%name = variable

   ! 5.3 Convert the scale lengths in the real distance (meter)

   be1_rf_lengthscale(1:nk) = be1_rf_lengthscale(1:nk) * xb%ds
   be2_rf_lengthscale(1:nk) = be2_rf_lengthscale(1:nk) * xb%ds
   be3_rf_lengthscale(1:nk) = be3_rf_lengthscale(1:nk) * xb%ds
   be4_rf_lengthscale(1:nk) = be4_rf_lengthscale(1:nk) * xb%ds
   be5_rf_lengthscale(1:1)  = be5_rf_lengthscale(1:1)  * xb%ds

   ! 6.0 Perform checks on eigenvectors with be data structure:
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   if (test_statistics) then
      call da_check_eof_decomposition(be%v1%val_g(:), be%v1%evec_g(:,:),&
                                     be%v1%name)
      call da_check_eof_decomposition(be%v2%val_g(:), be%v2%evec_g(:,:),&
                                     be%v2%name)
      call da_check_eof_decomposition(be%v3%val_g(:), be%v3%evec_g(:,:),&
                                     be%v3%name)
      call da_check_eof_decomposition(be%v4%val_g(:), be%v4%evec_g(:,:),&
                                     be%v4%name)
   end if

   ! 6.2 Close the be unit

   close(be_unit)
   call da_free_unit(be_unit)

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

   ! 8.0 deallocate input model state:
   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   deallocate (be1_eval_loc)
   deallocate (be2_eval_loc)
   deallocate (be3_eval_loc)
   deallocate (be4_eval_loc)
   deallocate (be5_eval_loc)

   if (vert_corr == vert_corr_2) then
      deallocate (be1_eval_glo)
      deallocate (be2_eval_glo)
      deallocate (be3_eval_glo)
      deallocate (be4_eval_glo)
      deallocate (be5_eval_glo)

      deallocate (be1_evec_loc)
      deallocate (be2_evec_loc)
      deallocate (be3_evec_loc)
      deallocate (be4_evec_loc)
      deallocate (be5_evec_loc)

      deallocate (be1_evec_glo)
      deallocate (be2_evec_glo)
      deallocate (be3_evec_glo)
      deallocate (be4_evec_glo)
      deallocate (be5_evec_glo)

   end if

   deallocate (bin)
   deallocate (bin2d)

   if (be % ne > 0) then
      be % alpha % mz = be % ne
      be % alpha % name = 'alpha'

      allocate (be5_eval_loc (1:1,1:num_bins2d))
      allocate (be5_eval_glo(1:1))
      allocate (be5_evec_loc(1:1,1:1,1:num_bins2d))
      allocate (be5_evec_glo(1:1,1:1))

      ! Transfer the alpha standard deviation to be % alpha:
      allocate (be % alpha % val(1:jy,1:be % ne))
      allocate (be % alpha % evec(1:jy,1:kz,1:be % ne))
      allocate (be % alpha % rf_alpha(1:be % ne))

      be % alpha % val(1:jy,1:be % ne) = alpha_std_dev
      be % alpha % evec(1:jy,1:kz,1:be % ne) = 1.0
      be % alpha % rf_alpha(1:be % ne) = 1.0 

      ! Include alpha lengthscale info:
      allocate (alpha_rf_lengthscale(1:be % ne))
      alpha_rf_lengthscale(:) = 1000.0 * alpha_corr_scale ! Convert km to m.

      call da_rescale_background_errors(1.0, 1.0, &
         xb % ds, alpha_rf_lengthscale, be % alpha)
   else
      be % alpha % mz = 0
   end if

   if (trace_use) call da_trace_exit("da_setup_be_nmm_regional")

end subroutine da_setup_be_nmm_regional


