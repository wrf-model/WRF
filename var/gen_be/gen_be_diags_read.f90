program gen_be_diags_read
!------------------------------------------------------------------------
!  Purpose: Process background error statistics for display of various 
!           diagnostics using WRFDA/graphics/NCL graphics utilities
!
!  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
!           & Monika Krysta, (CTBTO, Vienna, Austria)
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------


   use da_control, only : do_normalize,filename_len,stderr,stdout,use_rf
   use da_tools_serial, only : da_get_unit
   use da_gen_be, only : da_print_be_stats_h_regional, &
      da_print_be_stats_h_global, da_print_be_stats_p, da_print_be_stats_v

   implicit none

   character*10        :: variable          ! Variable name
   character*80        :: var80c

   character*8         :: uh_method         ! Uh_method (power, scale)
   integer             :: n_smth_sl                  ! Number of smoothing for scale-length
   integer             :: cv_options                 ! WRFDA CV_OPTIONS
   character(len=filename_len)        :: filename          ! Input filename.
   integer             :: outunit           ! Output unit for diagnostics.
   integer             :: ni, nj, nk, nk_3d ! Dimensions read in.
   integer             :: bin_type          ! Type of bin to average over. !!!DALE ADD.
   integer             :: num_bins          ! Number of 3D bins.
   integer             :: num_bins2d        ! Number of 2D bins.
   integer             :: k                 ! Loop counter.
   integer             :: kdum              ! Dummy vertical index.
   integer             :: max_wavenumber    ! Smallest scale required (ni/2 - 1).
   real                :: lat_min, lat_max  ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat      ! Used if bin_type = 2 (degrees). !!!DALE ADD..
   real                :: binwidth_hgt      ! Used if bin_type = 2 (m). !!!DALE ADD..
   real                :: hgt_min, hgt_max  ! Used if bin_type = 2 (m).
   real                :: scale_length_ps   ! Scale length for scalar ps or ps_u
   logical             :: dummy

   integer, allocatable:: bin(:,:,:)        ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)        ! Bin assigned to each 2D point.
   real, allocatable   :: regcoeff_psi_chi(:)        ! psi/chi    regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)   ! psi/t   regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)    ! psi/ps     regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)  ! psi/rh     regression cooefficient.
   real, allocatable   :: regcoeff_chi_u_t(:,:,:) ! chi_u/t regression coefficient
   real, allocatable   :: regcoeff_chi_u_ps(:,:)  ! chi_u/ps   regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)! chi_u/rh   regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)  ! t_u/rh  regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)   ! ps_u/rh    regression coefficient

   real, allocatable   :: e_vec(:,:)        ! Domain-averaged eigenvectors.
   real, allocatable   :: e_val(:)          ! Domain-averaged eigenvalues.  
   real, allocatable   :: e_vec_loc(:,:,:)  ! Latitudinally varying eigenvectors.
   real, allocatable   :: e_val_loc(:,:)    ! Latitudinally varying eigenvalues.
   real, allocatable   :: total_power(:)    ! Total Power spectrum.
   real, allocatable   :: scale_length(:)   ! Scale length for regional application.


   namelist / gen_be_diags_nl / cv_options, do_normalize, n_smth_sl, uh_method, use_rf


   integer :: iunit,namelist_unit

   ! Hardwire because of complicated incrementing of unit numbers writing a file
   ! each time 
   integer, parameter :: ounit = 71

   stderr = 0
   stdout = 6

   uh_method = 'scale'
   n_smth_sl = 0
   cv_options = 5

   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)

   open(unit=namelist_unit, file='gen_be_diags_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_diags_nl)
   close(namelist_unit)

   filename = 'be.dat'
   print '("*** Unit=",i3,3X,"filename=",a40)',iunit, filename
   open (iunit, file = filename, form='unformatted')

   !----------------------------------------------------------------------------
   ! [1] Gather regression coefficients.
   !----------------------------------------------------------------------------

   ! Read the dimensions:
   read(iunit)ni, nj, nk
   nk_3d = nk

   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )

   ! Read bin info:

   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d
   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)


   if ( cv_options /= 7 ) then
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

      regcoeff_psi_chi = 0.
      regcoeff_psi_t   = 0.
      regcoeff_psi_ps  = 0.
      regcoeff_psi_rh  = 0.
      regcoeff_chi_u_t = 0.
      regcoeff_chi_u_ps= 0.
      regcoeff_chi_u_rh= 0.
      regcoeff_t_u_rh  = 0.
      regcoeff_ps_u_rh = 0.

      if ( cv_options == 5 ) then
         read (iunit) regcoeff_psi_chi
         read (iunit) regcoeff_psi_ps
         read (iunit) regcoeff_psi_t
      else
         do k = 1 , 9
            read (iunit) var80c
            select case( trim(adjustl(var80c)) )
            case ('regcoeff_psi_chi')
               read (iunit) regcoeff_psi_chi
            case ('regcoeff_psi_t')
               read (iunit) regcoeff_psi_t
            case ('regcoeff_psi_ps')
               read (iunit) regcoeff_psi_ps
            case ('regcoeff_psi_rh')
               read (iunit) regcoeff_psi_rh
            case ('regcoeff_chi_u_t')
               read (iunit) regcoeff_chi_u_t
            case ('regcoeff_chi_u_ps')
               read (iunit) regcoeff_chi_u_ps
            case ('regcoeff_chi_u_rh')
               read (iunit) regcoeff_chi_u_rh
            case ('regcoeff_t_u_rh')
               read (iunit) regcoeff_t_u_rh
            case ('regcoeff_ps_u_rh')
               read (iunit) regcoeff_ps_u_rh
            case default;
               write(6,fmt='(A,A)')'Error in reading regression coefficients for: ',trim(adjustl(var80c))
               stop
            end select
         end do
      end if

      outunit = ounit + 100
      call da_print_be_stats_p( outunit, ni, nj, nk, num_bins, num_bins2d, &
                    bin, bin2d, regcoeff_psi_chi,regcoeff_psi_ps,regcoeff_psi_t)     
   else
      !outunit is advanced by 3 within da_print_be_stats_p; since we don't call
      !that for cv_options=7 we need to account for it
      outunit = ounit + 103 
   end if

   !----------------------------------------------------------------------------
   ! [2] Gather vertical error eigenvectors, eigenvalues.
   !----------------------------------------------------------------------------

   read(iunit)variable
   read(iunit)nk, num_bins2d

   allocate( e_vec(1:nk,1:nk) )
   allocate( e_val(1:nk) )
   allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
   allocate( e_val_loc(1:nk,1:num_bins2d) )

   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )
   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

   read(iunit)variable
   read(iunit)nk, num_bins2d
   read(iunit)e_vec
   read(iunit)e_val
   read(iunit)e_vec_loc
   read(iunit)e_val_loc
   call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                             e_vec, e_val, e_vec_loc, e_val_loc )

   deallocate( e_vec )
   deallocate( e_val )
   deallocate( e_vec_loc )
   deallocate( e_val_loc )

   if (uh_method /= 'power') then
      ! 2d fields: ps_u, ps:

      read(iunit)variable
      read(iunit)nk, num_bins2d

      allocate( e_vec(1:nk,1:nk) )
      allocate( e_val(1:nk) )
      allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
      allocate( e_val_loc(1:nk,1:num_bins2d) )

      read(iunit)e_vec
      read(iunit)e_val
      read(iunit)e_vec_loc
      read(iunit)e_val_loc
      call da_print_be_stats_v( outunit, variable, nk, num_bins2d, &
                                e_vec, e_val, e_vec_loc, e_val_loc )
      deallocate( e_vec )
      deallocate( e_val )
      deallocate( e_vec_loc )
      deallocate( e_val_loc )
   end if
   ! To assign the dimension nk for 3d fields:
   nk = nk_3d

   if (uh_method == 'power') then
       write(6,'(/a)') '[3] Gather horizontal error power spectra.'

      do k = 1, nk
         read(iunit)variable
         read(iunit)max_wavenumber, kdum
         if ( k == 1 ) allocate( total_power(0:max_wavenumber) )
         read(iunit) dummy ! to preserve namelist format
         read(iunit)total_power(:)
         call da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )
      end do

      do k = 1, nk
         read(iunit)variable
         read(iunit)max_wavenumber, kdum
         read(iunit) dummy ! to preserve namelist format
         read(iunit)total_power(:)
         call da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )
      end do

      do k = 1, nk
         read(iunit)variable
         read(iunit)max_wavenumber, kdum
         read(iunit) dummy ! to preserve namelist format
         read(iunit)total_power(:)
         call da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )
      end do

      do k = 1, nk
         read(iunit)variable
         read(iunit)max_wavenumber, kdum
         read(iunit) dummy ! to preserve namelist format
         read(iunit)total_power(:)
         call da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )
      end do

      read(iunit)variable
      read(iunit)max_wavenumber, kdum
      read(iunit) dummy ! to preserve namelist format
      read(iunit)total_power(:)
      call da_print_be_stats_h_global( outunit, variable, k, max_wavenumber, total_power )

   else if (uh_method == 'scale   ') then

      allocate (scale_length(1:nk))

      ! psi/u:
      read(iunit) variable
      read(iunit) scale_length
      call da_print_be_stats_h_regional( outunit, variable, nk, scale_length )

      ! chi_u/v:
      read(iunit) variable
      read(iunit) scale_length
      call da_print_be_stats_h_regional( outunit, variable, nk, scale_length )

      ! t_u/t:
      read(iunit) variable
      read(iunit) scale_length
      call da_print_be_stats_h_regional( outunit, variable, nk, scale_length )

      ! rh:
      read(iunit) variable
      read(iunit) scale_length
      call da_print_be_stats_h_regional( outunit, variable, nk, scale_length )

      ! ps_u/ps:
      read(iunit) variable
      read(iunit) scale_length_ps ! This is a scalar, can be ps_u or ps depending on cv_options
      write(6,'(3a,i5)')' Scale length for variable ', trim(variable), ' in unit ', outunit
      write(outunit,'(a,i4,1pe15.5)')trim(variable), 1, scale_length_ps
      outunit = outunit + 1
      write(6,*)

      deallocate (scale_length)

   else
      write(6,'(a,": uh_method=",a," not implemented.")')__FILE__,uh_method
   endif

   close(iunit)

end program gen_be_diags_read
