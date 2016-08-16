subroutine da_setup_runconstants(grid, xbx)

   !---------------------------------------------------------------------------
   !  Purpose: Define constants used.
   !---------------------------------------------------------------------------

   implicit none

   type (domain),   intent(inout) :: grid                
   type (xbx_type), intent(inout) :: xbx     ! Header & non-gridded vars.

   integer                  :: n             ! Loop counter.

   integer                  :: fft_size_i    ! Efficient FFT 1st dimension.
   integer                  :: fft_size_j    ! Efficient FFT 2nd dimension.
   logical                  :: found_magic   ! true when efficient FFT found
   logical                  :: need_pad      ! True when need pad_i > 0

   integer                  :: i, j
   integer                  :: nx            ! nx + 1 = ix + pad_i
   integer                  :: ny            ! ny + 1 = jy + pad_j
   real                     :: const         ! Multiplicative constants.
   real                     :: coeff_nx      ! Multiplicative coefficients.
   real                     :: coeff_ny      ! Multiplicative coefficients.
   real                     :: cos_coeff_nx  ! Multiplicative coefficients.
   real                     :: cos_coeff_ny  ! Multiplicative coefficients.

   if (trace_use) call da_trace_entry("da_setup_runconstants")

   !---------------------------------------------------------------------------
   ! [1.0] Calculate padded grid-size required for efficient FFTs and factors:
   !---------------------------------------------------------------------------

   ! [1.1] In x direction:

   nx = ide - ids

   allocate(xbx % fft_factors_x(num_fft_factors))

   do n = nx, nx+nrange
      call da_find_fft_factors(n, found_magic, xbx % fft_factors_x)

      if (found_magic) then
         if (mod(n, 2) == 0) then
            fft_size_i = n
            xbx % fft_pad_i = n - nx
            exit
         end if
      end if
   end do

   if (.NOT. found_magic) then
     call da_error(__FILE__,__LINE__, &
       (/"No FFT factor found: invalid e_we value"/))
   end if

   allocate(xbx % trig_functs_x(1:3*fft_size_i))  

   call da_find_fft_trig_funcs(fft_size_i, xbx % trig_functs_x(:))

   ! [1.2] In y direction:

   ny = jde - jds

   allocate(xbx % fft_factors_y(num_fft_factors))

   do n = ny, ny+nrange
      call da_find_fft_factors(n, found_magic, xbx % fft_factors_y)

      if (found_magic) then
         if (mod(n, 2) == 0) then
            fft_size_j = n
            xbx % fft_pad_j = n - ny
            exit
         end if
      end if
   end do
      
   if (.NOT. found_magic) then
     call da_error(__FILE__,__LINE__, &
       (/"No FFT factor found: invalid e_sn value"/))
   end if

   allocate(xbx % trig_functs_y(1:3*fft_size_j))  

   call da_find_fft_trig_funcs(fft_size_j, xbx % trig_functs_y(:))

   !-----Multiplicative coefficent for solution of spectral Poission eqn:

   !mslee.Bgrid
   ! const = -0.5 * grid%xb%ds * grid%xb%ds
   const = -2.0 * grid%xb%ds * grid%xb%ds

   nx = ide - ids + xbx%fft_pad_i
   ny = jde - jds + xbx%fft_pad_j
   ! YRG: A-grid:
   coeff_nx = 2.0 * pi / real(nx)
   coeff_ny = 2.0 * pi / real(ny)

   ! YRG: B-grid::
   ! coeff_nx = pi / real(nx)
   ! coeff_ny = pi / real(ny)

   xbx%fft_ix = nx + 1
   xbx%fft_jy = ny + 1

   allocate(xbx%fft_coeffs(1:xbx%fft_ix,1:xbx%fft_jy))

   do j = 2, ny
      cos_coeff_ny = COS(coeff_ny * real(j-1))
      do i = 2, nx
         cos_coeff_nx = COS(coeff_nx * real(i-1))
         !mslee.Bgrid
         ! xbx%fft_coeffs(i,j) = const / (1.0 - cos_coeff_nx * cos_coeff_ny)
         if (cos_coeff_nx.eq.1.and.cos_coeff_ny.eq.1) then
            xbx%fft_coeffs(i,j) = 0.0
         else
            xbx%fft_coeffs(i,j) = const / (2.0 - cos_coeff_nx - cos_coeff_ny)
         end if
      end do
   end do

   ! Set to zero all coefficients which are multiplied by sin(0.0) in FST:

   !mslee      xbx%fft_coeffs(1,1:xbx%fft_jy)  = 0.0
   !mslee      xbx%fft_coeffs(xbx%fft_ix,1:xbx%fft_jy) = 0.0
   !mslee      xbx%fft_coeffs(1:xbx%fft_ix,1)  = 0.0
   !mslee      xbx%fft_coeffs(1:xbx%fft_ix,xbx%fft_jy) = 0.0
   !mslee. we need to check the following

   xbx%fft_adjoint_factor = 4.0 / real(nx * ny)

   !---------------------------------------------------------------------------

   ! Calculate i increment for distributing pad region across processors.

   need_pad = .true.
   if (xbx%fft_pad_i <= 0) then
      need_pad = .false.
   else if (xbx%fft_pad_i > (ide-ids+1)) then
      write(unit=message(1), fmt='(a)') &
       'FFT xbx%fft_pad_i is too large!'
      write(unit=message(2), fmt='(2x,a,i4)') &
         '(ide-ids+1) = ', (ide-ids+1), &
         'xbx%fft_pad_i = ', xbx%fft_pad_i
      call da_error (__FILE__,__LINE__,message(1:2))
   else
      xbx%pad_inc = (ide-ids+1)/xbx%fft_pad_i
   end if

   ! Calculate number of pad vectors in x to be done on this processor.
   ! Need to save xbx%pad_num, xbx%pad_inc, and xbx%pad_loc

   xbx%pad_num = 0
   if (need_pad) then
      do n=1, xbx%fft_pad_i
         i = (n-1)*xbx%pad_inc + 1
         if ((i >= grid%xp%itsy) .and. (i <= grid%xp%itey)) then
            xbx%pad_num = xbx%pad_num + 1
         end if
      end do

      if (xbx%pad_num > 0) then
         allocate(xbx%pad_loc(1:xbx%pad_num))
         allocate(xbx%pad_pos(1:xbx%pad_num))
      end if

      xbx%pad_num = 0
      do n=1, xbx%fft_pad_i
         i = (n-1)*xbx%pad_inc + 1
         if ((i >= grid%xp%itsy) .and. (i <= grid%xp%itey)) then
            xbx%pad_num = xbx%pad_num + 1
            xbx%pad_loc(xbx%pad_num) = i
            xbx%pad_pos(xbx%pad_num) = grid%xp%ide + n
         end if
      end do
   end if
   
   !---------------------------------------------------------------------------

   if (global) then
      ! Set up Spectral transform constants:
      xbx%inc    = 1
      xbx%ni     = ide-ids+1
      xbx%nj     = jde-jds+1
      xbx%lenr   = xbx%inc * (xbx%ni - 1) + 1
      xbx%lensav = xbx%ni + int(log(real(xbx%ni))) + 4
      xbx%lenwrk = xbx%ni
      xbx%max_wavenumber = xbx%ni/2 -1
      xbx%alp_size = (xbx%nj+ 1)*(xbx%max_wavenumber+1)*(xbx%max_wavenumber+2)/4

      if (print_detail_spectral) then
         write (unit=stdout,fmt='(a)') "Spectral transform constants"
         write (unit=stdout, fmt='(a, i8)') &
            '   inc            =', xbx%inc   , &
            '   ni             =', xbx%ni    , &
            '   nj             =', xbx%nj    , &
            '   lenr           =', xbx%lenr  , &
            '   lensav         =', xbx%lensav, &
            '   lenwrk         =', xbx%lenwrk, &
            '   max_wavenumber =', xbx%max_wavenumber, &
            '   alp_size       =', xbx%alp_size
         write (unit=stdout,fmt='(a)') " "
      end if

      allocate(xbx%coslat(1:xbx%nj))
      allocate(xbx%sinlat(1:xbx%nj))
      allocate(xbx%coslon(1:xbx%ni))
      allocate(xbx%sinlon(1:xbx%ni))
      allocate(xbx%int_wgts(1:xbx%nj))      ! Interpolation weights
      allocate(xbx%alp(1:xbx%alp_size))
      allocate(xbx%wsave(1:xbx%lensav))

      call da_initialize_h(xbx%ni, xbx%nj, xbx%max_wavenumber, &
                           xbx%lensav, xbx%alp_size,           &
                           xbx%wsave, grid%xb%lon, xbx%sinlon,      &
                           xbx%coslon, grid%xb%lat, xbx%sinlat,     &
                           xbx%coslat, xbx%int_wgts, xbx%alp)


   end if

   if (trace_use) call da_trace_exit("da_setup_runconstants")
   
end subroutine da_setup_runconstants


