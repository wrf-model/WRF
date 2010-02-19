subroutine da_setup_be_regional(xb, be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors
   !---------------------------------------------------------------------------

   implicit none

   type (xb_type), intent(in)    :: xb                    ! First guess structure.
   type (be_type), intent(inout) :: be                    ! Back. errors structure.

   integer                     :: i, j, k, m       ! Loop counters.
   integer, allocatable:: bin(:,:,:)         ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)         ! Bin assigned to each 2D point
   integer             :: bin_type           ! Type of bin to average over.
   integer             :: num_bins           ! Number of bins (3D fields).
   integer             :: num_bins2d         ! Number of bins (3D fields).
   real*8  :: lat_min, lat_max, binwidth_lat ! Used if bin_type = 2 (degrees)..
   real*8  :: hgt_min, hgt_max, binwidth_hgt ! Used if bin_type = 2 (m). .

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
   real*8, allocatable           :: alpha_val(:)          ! Global Eigenvalues.

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
   real*8, allocatable           :: alpha_evec(:,:)       ! Global Eigenvectors.

   real*8, allocatable           :: be1_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be2_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be3_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be4_rf_lengthscale(:) ! RF lengthscale.
   real*8, allocatable           :: be5_rf_lengthscale(:)
   real*8, allocatable           :: alpha_rf_lengthscale(:)
   real*8, allocatable           :: alpha_rf_scale_factor(:)

   real*8, allocatable           :: evec_loc(:,:,:)        ! Latitudinally varying eigenvectors.
   real*8, allocatable           :: eval_loc(:,:)          ! Latitudinally varying eigenvalues.

   character*10                :: variable
   integer                     :: ni, nj, nk, nk_2d, b         
   integer                     :: ix, jy, kz, mz
   real*8, allocatable           :: regcoeff1(:)
   real*8, allocatable           :: regcoeff2(:,:)
   real*8, allocatable           :: regcoeff3(:,:,:)
   real                        :: avg,avg2,avg3
   integer                     :: be_unit, ier, be_rf_unit, be_print_unit, it

   if (trace_use) call da_trace_entry("da_setup_be_regional")

   write (unit=message(1),fmt='(A)') &
      'Set up background errors for regional application'

   be % v1 % name = 'psi  '           ! Streamfunction
   be % v2 % name = 'chi_u'           ! Uncorrelated velocity potential.
   be % v3 % name = 't_u'             ! Unbalanced temperature.
   be % v4 % name = 'q/qsg'
   be % v5 % name = 'psfc'            ! surface pressure
!
   if(use_radarobs .and. use_radar_rf .or. use_rad .and. crtm_cloud) then  
   be % v4 % name = 'qt   '
   end if
!

   write (unit=message(2),fmt='(3x,A,7A)') 'WRF-Var dry control variables are:', &
      trim(be % v1 % name), ', ', trim(be % v2 % name), ', ', &
      trim(be % v3 % name), ' and ', trim(be % v5 % name)

   write (unit=message(3),fmt='(3x,A,A)') &
      'Humidity control variable is ', trim(be % v4 % name)

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
      stop "Stopped, Please check if CV5 BE file provided correctly."
   else
      if ( nk /= kz ) then
         call da_error(__FILE__,__LINE__,  &
            (/"Vertial levels in fg and be.dat do not match."/))
      end if
   endif

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
   allocate (be%reg_chi(1:jy,1:nk))
   allocate (be%reg_ps (1:jy,1:nk))
   allocate (be%reg_t  (1:jy,1:nk,1:nk))

   do k=1,nk
      do j =1, jy
         b = bin(1,j,k)
         be%reg_chi(j,k) = regcoeff1(b)
      end do
   end do

   do j=1,jy
      b = bin2d(1,j)
      do k=1,nk
         be%reg_ps(j,k) = regcoeff2(k,b)
      end do
   end do

   do j=1,jy
      b = bin2d(1,j)
      do i=1,nk
         do k=1,nk
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

   allocate (be1_eval_loc (1:jy,1:kz))
   allocate (be2_eval_loc (1:jy,1:kz))
   allocate (be3_eval_loc (1:jy,1:kz))
   allocate (be4_eval_loc (1:jy,1:kz))
   allocate (be5_eval_loc (1:jy,1:1))

   if (vert_corr == vert_corr_2) then

      allocate (be1_eval_glo(1:kz))
      allocate (be2_eval_glo(1:kz))
      allocate (be3_eval_glo(1:kz))
      allocate (be4_eval_glo(1:kz))
      allocate (be5_eval_glo(1:1))

      allocate (be1_evec_loc(1:jy,1:kz,1:kz))
      allocate (be2_evec_loc(1:jy,1:kz,1:kz))
      allocate (be3_evec_loc(1:jy,1:kz,1:kz))
      allocate (be4_evec_loc(1:jy,1:kz,1:kz))
      allocate (be5_evec_loc(1:jy,1: 1,1: 1))

      allocate (be1_evec_glo(1:kz,1:kz))
      allocate (be2_evec_glo(1:kz,1:kz))
      allocate (be3_evec_glo(1:kz,1:kz))
      allocate (be4_evec_glo(1:kz,1:kz))
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
      b = bin2d(1,j)
      be1_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be1_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
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
      b = bin2d(1,j)
      be2_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be2_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
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
      b = bin2d(1,j)
      be3_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be3_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
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
      b = bin2d(1,j)
      be4_evec_loc(j,1:nk,1:nk) = evec_loc(1:nk,1:nk,b)
      be4_eval_loc(j,1:nk  ) = eval_loc(1:nk,b)
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
      b = bin2d(1,j)
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

   deallocate (be1_rf_lengthscale)
   deallocate (be2_rf_lengthscale)
   deallocate (be3_rf_lengthscale)
   deallocate (be4_rf_lengthscale)
   deallocate (be5_rf_lengthscale)

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

   else
      be % alpha % mz = 0
   end if

   if (trace_use) call da_trace_exit("da_setup_be_regional")

end subroutine da_setup_be_regional


