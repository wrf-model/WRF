subroutine da_setup_be_global (be)

   !---------------------------------------------------------------------------
   ! Purpose: Define and allocate components of background errors
   ! 
   ! Updates: 
   !
   !       Implementation of multi-variate BE  
   !       Syed RH Rizvi,  MMM/NESL/NCAR,  Date: 02/01/2010
   !---------------------------------------------------------------------------

   implicit none

   type (be_type), intent(out) :: be                 ! Back. errors structure

   integer                     :: nrec
   integer                     :: max_wave_in        ! Dimension of input power
   integer                     :: i, j, k, n ! Loop counters
   ! real, allocatable   :: height(:,:,:)      ! Height field.
   integer, allocatable:: bin(:,:,:)      ! Bin assigned to each 3D point
   integer, allocatable:: bin2d(:,:)      ! Bin assigned to each 2D point
   integer             :: bin_type        ! Type of bin to average over.
   integer             :: num_bins        ! Number of bins (3D fields).
   integer             :: num_bins2d      ! Number of bins (3D fields).
   logical             :: dummy

   real                :: binwidth_lat       ! Used if bin_type = 2 (degrees) 
   real                :: binwidth_hgt       ! Used if bin_type = 2 (m)
   real                :: hgt_min, hgt_max   ! Used if bin_type = 2 (m)
   real                :: lat_min, lat_max   ! Used if bin_type = 2 (degrees)

   character*10         :: variable
   integer              :: ni, nj, nk, b, be_unit
   integer, allocatable :: max_wave(:)
   real*8, allocatable  :: evec_g(:,:)
   real*8, allocatable  :: eval_g(:)  
   real*8, allocatable  :: evec_loc(:,:,:)
   real*8, allocatable  :: eval_loc(:,:)  
   real, allocatable   :: regcoeff_psi_chi(:)        ! psi/chi    regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)      ! psi/t   regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! psi/ps     regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)     ! psi/rh     regression cooefficient.
   real, allocatable   :: regcoeff_chi_u_t(:,:,:)    ! chi_u/t regression coefficient
   real, allocatable   :: regcoeff_chi_u_ps(:,:)     ! chi_u/ps   regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)   ! chi_u/rh   regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)     ! t_u/rh  regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)      ! ps_u/rh    regression coefficient

   real*8, allocatable :: power(:)               ! Temporary power spectrum.
   real, allocatable    :: power2d(:,:)           ! Temporary power spectrum.

   if (trace_use) call da_trace_entry("da_setup_be_global")

   call da_message((/"[3.0] Set up background errors (be) for global WRF-Var"/))

   be % max_wave = ide / 2 - 1

   !---------------------------------------------------------------------
   ! [1] Read in be metadata:
   !---------------------------------------------------------------------

   call da_get_unit(be_unit)
   open (unit=be_unit,file="be.dat",status="old",form="unformatted")

   read (be_unit, end= 99, err = 100) ni, nj, nk 
   read (be_unit, err = 100) bin_type
   read (be_unit, err = 100) lat_min, lat_max, binwidth_lat
   read (be_unit, err = 100) hgt_min, hgt_max, binwidth_hgt
   read (be_unit, err = 100) num_bins, num_bins2d

   allocate (bin(1:ni,1:nj,1:nk))
   allocate (bin2d(1:ni,1:nj))
   read (be_unit, err = 100) bin(1:ni,1:nj,1:nk)
   read (be_unit, err = 100) bin2d(1:ni,1:nj)

   if (ni /= ide .or. nj /= jde .or. nk /= kde) then
      call da_error(__FILE__,__LINE__, &
         (/"Cannot generate BE at this resolution"/))
   end if

   !---------------------------------------------------------------------
   ! [2] Read in be regression coefficients:
   !---------------------------------------------------------------------


   allocate  (regcoeff_psi_chi(1:num_bins))
   allocate  (regcoeff_psi_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_ps_u_rh(1:nk,1:num_bins2d))

   regcoeff_psi_chi = 0.
   regcoeff_psi_t   = 0.
   regcoeff_psi_ps  = 0.
   regcoeff_psi_rh  = 0.
   regcoeff_chi_u_t = 0.
   regcoeff_chi_u_ps= 0.
   regcoeff_chi_u_rh= 0.
   regcoeff_t_u_rh  = 0.
   regcoeff_ps_u_rh = 0.

   read (be_unit,end= 99,  err = 100) regcoeff_psi_chi
   read (be_unit,end= 99,  err = 100) regcoeff_psi_ps 
   read (be_unit,end= 99,  err = 100) regcoeff_psi_t

   ! Fill regression coeff. array
   allocate (be%reg_psi_chi(1:jde,1:nk))
   allocate (be%reg_psi_t  (1:jde,1:nk,1:nk))
   allocate (be%reg_psi_ps (1:jde,1:nk))
   allocate (be%reg_psi_rh (1:jde,1:nk,1:nk))
   allocate (be%reg_chi_u_t(1:jde,1:nk,1:nk))
   allocate (be%reg_chi_u_ps(1:jde,1:nk))
   allocate (be%reg_chi_u_rh(1:jde,1:nk,1:nk))
   allocate (be%reg_t_u_rh (1:jde,1:nk,1:nk))
   allocate (be%reg_ps_u_rh(1:jde,1:nk))


   be%reg_psi_chi = 0.
   be%reg_psi_t   = 0.
   be%reg_psi_ps  = 0.
   be%reg_psi_rh  = 0.
   be%reg_chi_u_t = 0.
   be%reg_chi_u_ps= 0.
   be%reg_chi_u_rh= 0.
   be%reg_t_u_rh  = 0.


   do k=1,nk
      do j =1, jde
         b = bin(1,j,k)
         be%reg_psi_chi(j,k) = psi_chi_factor * regcoeff_psi_chi(b)
      end do
   end do

   do j=1,jde
      b = bin2d(1,j)
      do k=1,nk
         be%reg_psi_ps(j,k)   = psi_ps_factor   * regcoeff_psi_ps(k,b)
         be%reg_ps_u_rh(j,k)  = ps_u_rh_factor  * regcoeff_ps_u_rh(k,b)
         be%reg_chi_u_ps(j,k) = chi_u_ps_factor * regcoeff_chi_u_ps(k,b)
      end do
   end do

   do j=1,jde
      b = bin2d(1,j)
      do i=1,nk
         do k=1,nk
            be%reg_psi_t(j,i,k)   = psi_t_factor      *  regcoeff_psi_t(i,k,b)
            be%reg_psi_rh(j,i,k)  = psi_rh_factor     *  regcoeff_psi_rh(i,k,b)
            be%reg_chi_u_t(j,i,k) = chi_u_t_factor    *  regcoeff_chi_u_t(i,k,b)
            be%reg_chi_u_rh(j,i,k)= chi_u_rh_factor   *  regcoeff_chi_u_rh(i,k,b)
            be%reg_t_u_rh(j,i,k)  = t_u_rh_factor     *  regcoeff_t_u_rh(i,k,b)
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

   !---------------------------------------------------------------------
   ! [3] Read in be vertical eigenmodes:
   !---------------------------------------------------------------------

   do nrec = 1, 4
      read (be_unit,end= 99,  err = 100) variable   
      read (be_unit,end= 99,  err = 100) nk, num_bins2d 

      allocate (evec_g(1:nk,1:nk))
      allocate (eval_g(1:nk))
      allocate (evec_loc(1:nk,1:nk,num_bins2d))
      allocate (eval_loc(1:nk,num_bins2d))

      read (be_unit,end= 99, err = 100) evec_g     
      read (be_unit,end= 99, err = 100) eval_g     
      read (be_unit,end= 99, err = 100) evec_loc     
      read (be_unit,end= 99, err = 100) eval_loc    

      if (nrec == 1) then
         be % v1 % name = variable               
         call da_get_bins_info(nj, nk, bin2d, evec_g, eval_g, &
            evec_loc, eval_loc, max_vert_var1, var_scaling1(1), be%v1)

      else if (nrec == 2) then
         be % v2 % name = variable               
         call da_get_bins_info(nj, nk, bin2d, evec_g, eval_g, &
            evec_loc, eval_loc, max_vert_var2, var_scaling2(1), be%v2)

      else if (nrec == 3) then
         be % v3 % name = variable               
         call da_get_bins_info(nj, nk, bin2d, evec_g, eval_g, &
            evec_loc, eval_loc, max_vert_var3, var_scaling3(1), be%v3)

      else if (nrec == 4) then
         be % v4 % name = variable               
         call da_get_bins_info(nj, nk, bin2d, evec_g, eval_g, &
            evec_loc, eval_loc, max_vert_var4, var_scaling4(1), be%v4)
      end if 

      deallocate (evec_g)     
      deallocate (eval_g)     
      deallocate (evec_loc)     
      deallocate (eval_loc)     

   end do ! loop nrec

   deallocate (bin)
   deallocate (bin2d)

   !---------------------------------------------------------------------
   ! [4] Read in be power spectra:
   !---------------------------------------------------------------------

   allocate(max_wave(1:nk))

   do k = 1, nk
      read (be_unit) variable
      read (be_unit) max_wave_in, nrec
      read (be_unit) dummy ! use to preserve file format 
      if (k == 1) then
          allocate (power(0:max_wave_in))                      ! Temporary.
          allocate (power2d(0:max_wave_in,1:nk))               ! Temporary.
      end if
      read (be_unit) power(0:max_wave_in) 
      power2d(:,k) = power(:) 

      ! Truncate power spectra:
      call da_truncate_spectra(be % max_wave, max_wave_in, power_truncation, &
                                power, max_wave(k))
   end do

   be % v1 % max_wave = maxval(max_wave(1:nk))
   write (unit=stdout,fmt='(/3x,3a,i6)') &
      'Horizontal truncation for ', be % v1 % name, ' = ', be % v1 % max_wave
   allocate (be % v1 % power(0:be % v1 % max_wave,1:nk))
   be % v1 % power(0:be % v1 % max_wave,1:nk) = power2d(0:be % v1 % max_wave,1:nk)
   be % v1 % power(0,1:nk) = len_scaling1(1) * be % v1 % power(0,1:nk) 

   do k = 1, nk
      read (be_unit) variable
      read (be_unit) max_wave_in, nrec
      read (be_unit) dummy ! use to preserve file format    
      read (be_unit) power(0:max_wave_in) 
      power2d(:,k) = power(:) 

      ! Truncate power spectra:
      call da_truncate_spectra (be % max_wave, max_wave_in, power_truncation, &
                                power, max_wave(k))
   end do

   be % v2 % max_wave = maxval(max_wave(1:nk))
   write (unit=stdout,fmt='(3x,3a,i6)') &
      'Horizontal truncation for ', be % v2 % name, ' = ', be % v2 % max_wave
   allocate (be % v2 % power(0:be % v2 % max_wave,1:nk))
   be % v2 % power(0:be % v2 % max_wave,1:nk) = power2d(0:be % v2 % max_wave,1:nk)
   be % v2 % power(0,1:nk) = len_scaling2(1) * be % v2 % power(0,1:nk) 

   do k = 1, nk
      read (be_unit) variable
      read (be_unit) max_wave_in, nrec
      read (be_unit) dummy ! use to preserve file format    
      read (be_unit) power(0:max_wave_in) 
      power2d(:,k) = power(:) 

      ! Truncate power spectra:
      call da_truncate_spectra (be % max_wave, max_wave_in, power_truncation, &
                                power, max_wave(k))
   end do

   be % v3 % max_wave = maxval(max_wave(1:nk))
   write(unit=stdout,fmt='(3x,3a,i6)') &
      'Horizontal truncation for ', be % v3 % name, ' = ', be % v3 % max_wave
   allocate (be % v3 % power(0:be % v3 % max_wave,1:nk))
   be % v3 % power(0:be % v3 % max_wave,1:nk) = power2d(0:be % v3 % max_wave,1:nk)
   be % v3 % power(0,1:nk) = len_scaling3(1) * be % v3 % power(0,1:nk) 

   do k = 1, nk
      read (be_unit) variable
      read (be_unit) max_wave_in, nrec
      read (be_unit) dummy ! use to preserve file format
      read (be_unit) power(0:max_wave_in) 
      power2d(:,k) = power(:) 

      ! Truncate power spectra:
      call da_truncate_spectra (be % max_wave, max_wave_in, power_truncation, &
                                power, max_wave(k))
   end do

   be % v4 % max_wave = maxval(max_wave(1:nk))
   write (unit=stdout,fmt='(3x,3a,i6)') &
      'Horizontal truncation for ', be % v4 % name, ' = ', be % v4 % max_wave
   allocate (be % v4 % power(0:be % v4 % max_wave,1:nk))
   be % v4 % power(0:be % v4 % max_wave,1:nk) = power2d(0:be % v4 % max_wave,1:nk)
   be % v4 % power(0,1:nk) = len_scaling4(1) * be % v4 % power(0,1:nk) 

   ! ps_u:
   read (be_unit) variable
   be % v5 % name = variable
   be % v5 % mz = 1
   if (max_vert_var5 <=  0.0) be % v5 % mz = 0                         
   read (be_unit) max_wave_in, nrec
   read (be_unit) dummy ! use to preserve file format
   read (be_unit) power(0:max_wave_in) 

   ! Truncate power spectra:
   call da_truncate_spectra (be % max_wave, max_wave_in, power_truncation, &
                             power, be % v5 % max_wave)

   write (unit=stdout,fmt='(3x,3a,i6)') &
      'Horizontal truncation for ', be % v5 % name, ' = ', be % v5 % max_wave
   allocate (be % v5 % power(0:be % v5 % max_wave,1))
   be % v5 % power(0:be % v5 % max_wave,1) = power(0:be % v5 % max_wave)
   be % v5 % power(0,1) = len_scaling5(1) * be%v5%power(0,1) 

   deallocate(power)

   !--------------------------------------------------------------
   ! [5] Perform checks on eigenvectors:
   !--------------------------------------------------------------

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
 
   !--------------------------------------------------------------
   ! [6] Set up alpha (flow-dependent) control variable "errors": 
   !--------------------------------------------------------------

   if (be % ne > 0) then
      be % alpha % mz = be % ne
      be % alpha % name = 'alpha'
      be % alpha % max_wave = alpha_truncation
      if (print_detail_be) then
         write(unit=stdout,fmt='(3x,3a,i6)') &
            'Horizontal truncation for ', be % alpha % name, ' = ', &
            be % alpha % max_wave
      end if
      allocate (power(0:be % alpha % max_wave))
      call da_calc_power_spectrum(be % alpha % max_wave, power)

      allocate (be % alpha % power(0:be % alpha % max_wave, be % ne))
      do n = 1, be % ne
         be % alpha % power(0:be % alpha % max_wave,n) = power(0:be % alpha % max_wave)
      end do
   end if

   deallocate (max_wave)

   write (unit=stdout,fmt='(A)') " "

   close (be_unit)
   call da_free_unit(be_unit)

   if (trace_use) call da_trace_exit("da_setup_be_global")

   return

99 write (unit=message(1),fmt='(a, i5)')' Unexpected end on BE-unit = ',be_unit
   call da_error(__FILE__,__LINE__,message(1:1))

100 write (unit=message(1),fmt='(a, i5)')' Read error on BE-unit = ',be_unit
   call da_error(__FILE__,__LINE__,message(1:1))

   ! FIX? daft having these here

   deallocate(power)
   deallocate(power2d)

end subroutine da_setup_be_global


