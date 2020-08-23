program gen_be_v3
!
! Author: Jamie Bresch (NCAR/MMM)
! Some subroutines are adapted from the existing WRFDA gen_be and WRF code.
!
! Required libraries: netCDF and WRFIO (libwrfio_nf.a).
!                     LAPACK lib is required only if the code will be used for
!                            do_slen_calc=true and do_eof_transform=true.
! Required compiler flag: big_endian
!
! For be generation application and when do_eof_transform=true, LAPACK_LIB is required:
!    set NETCDF = your_netcdf_build_dir
!    set WRF_DIR = your_wrf_build_dir
!    set LAPACK_LIB = your_system_lapack_a (for example: /usr/lib/liblapack.dylib or /glade/u/apps/opt/intel/2017u1/mkl/lib/intel64_lin/libmkl_lapack95_lp64.a)
!    cpp -P -traditional gen_be.F90 > be.f90
!    ifort be.f90 -o gen_be.exe -qopenmp -convert big_endian ${WRF_DIR}/external/io_netcdf/libwrfio_nf.a \
!       -L${NETCDF}/lib -lnetcdf -lnetcdff -lm -I${NETCDF}/include ${LAPACK_LIB}
!
! For ep generation application:
!    set NETCDF = your_netcdf_build_dir
!    set WRF_DIR = your_wrf_build_dir
!    cpp -P -traditional -DNO_LAPACK_LIB gen_be.F90 > be.f90
!    ifort be.f90 -o gen_be.exe -qopenmp -convert big_endian ${WRF_DIR}/external/io_netcdf/libwrfio_nf.a \
!       -L${NETCDF}/lib -lnetcdf -lnetcdff -lm -I${NETCDF}/include

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

   integer, parameter  :: r_single = selected_real_kind(6)  ! single precision
   integer, parameter  :: r_double = selected_real_kind(15) ! double precision
   integer, parameter  :: i_byte   = selected_int_kind(1)   ! byte integer
   integer, parameter  :: i_short  = selected_int_kind(4)   ! short integer
   integer, parameter  :: i_long   = selected_int_kind(8)   ! long integer
   integer, parameter  :: i_kind   = i_long                 ! default integer

   integer, parameter :: filename_len = 200
   integer, parameter :: DateStrLen = 19
   integer, parameter :: VarNameLen = 10
   integer, parameter :: stdout = 6
   integer, parameter :: root = 0
   integer, parameter :: nvar_max = 20
   integer, parameter :: imiss = -99
   real,    parameter :: t00 = 300.0
   real,    parameter :: p00 = 100000.0
   real,    parameter :: gas_constant = 287.0
   real,    parameter :: cp = 7.0*gas_constant/2.0
   real,    parameter :: kappa = gas_constant/cp
   real,    parameter :: t_kelvin = 273.15
   real,    parameter :: t_triple = 273.16 ! triple point of water
   real,    parameter :: gas_constant_v = 461.6
   real,    parameter :: rd_over_rv = gas_constant/gas_constant_v
   real,    parameter :: rd_over_rv1 = 1.0 - rd_over_rv

   namelist /gen_be_nml/ nc_list_file, be_method, varnames, outnames, &
      do_pert_calc, do_eof_transform, do_slen_calc, slen_opt, stride, yr_cutoff, &
      verbose, level_start, level_end, aux_file, write_be_dat_r8, pert1_read_opt, &
      vertloc_opt, es_ice
   namelist /ens_nml/ nens, write_ep, write_ens_stdv, ep_format, nproc_out, time_lag_ens

   character(len=filename_len) :: nc_list_file  ! the text file that contains a list of netcdf files to process
   character (len=3)           :: be_method     ! "NMC" or "ENS"
   character(len=VarNameLen)   :: varnames(nvar_max)
   character(len=VarNameLen)   :: outnames(nvar_max)
   character(len=filename_len) :: aux_file      ! the file to read MAPFAC_MX/MAPFAC_MY when they are not available in wrfout
   logical :: do_pert_calc           ! if calculating perturbations
   logical :: do_eof_transform       ! true: in EOF space; false: in physical space
   logical :: do_slen_calc           ! if calculating scale-length
   integer :: slen_opt               ! 1: curve-fitting method; 2: laplacian method
   integer :: stride                 ! for slen_opt=1 to skip every stride grid point
   real    :: yr_cutoff(nvar_max)    ! for slen_opt=1
   logical :: verbose
   logical :: es_ice                 ! whether to consider ice effect or not for saturation vapor pressure
   integer :: nens                   ! set in namelist for ENS method. hard-coded to 2 for NMC method.
   logical :: write_ep               ! if writing out ensemble perturbations
   logical :: write_ens_stdv         ! if writing out stdv of ensemble perturbations
   integer :: ep_format              !  1: each member in separate file (real*8)
                                     ! 11: each member in separate file (real*4)
                                     !  2: all members in one file (full-domain) (real*4)
                                     !  3: all members in one file (sub-domain), need to specify nproc_out (real*4)
   integer :: nproc_out              ! number of processors to decompose for ep_format=3
   logical :: time_lag_ens           ! if calculating statistics for time lagged ensembles

   integer :: level_start, level_end ! for do_slen_calc=true. Can be set to be other than 1 and nvert for quick debugging/testing
   integer :: pert1_read_opt         ! how to access pert1 data for compute_bv_sl
                                     ! 1: read and store all cases in memory at once
                                     ! 2: read from pert1 file when need it
   integer :: vertloc_opt            ! 0: no vertical localization
                                     ! 1: with vertical localization using log-P algorithm
   logical :: write_pert0
   logical :: write_pert1
   logical :: write_be_dat_r8        ! if writing out be.dat in real*8

   integer :: num_procs, myproc
   integer :: ncase
   integer :: ni, nj, nk
   integer :: ni1, nj1, nk1
   integer :: nvar, nvar_avail
   integer :: mp_physics
   real    :: ds
   character(len=VarNameLen) :: vartmp
   integer :: nvar_all

   integer, allocatable :: istart_ens(:), iend_ens(:)
   integer, allocatable :: istart_case(:), iend_case(:)
   integer :: ens_istart, ens_iend
   integer :: case_istart, case_iend

   logical, allocatable :: do_this_var(:)
   integer, allocatable :: var_dim(:)

   real, allocatable :: vertloc_rho(:,:)
   real, allocatable :: vertloc_kcutoff(:)

   logical :: read_t
   logical :: read_qv
   logical :: read_prs
   logical :: read_psfc
   logical :: read_ght = .false.
   logical :: read_u
   logical :: read_v
   logical :: calc_psi
   logical :: calc_chi
   logical :: calc_regcoeff
   integer :: indx_psi

   integer              :: nk_2d = 1
   integer              :: sl_unit = 77
   integer              :: be_unit = 88
   integer              :: nml_unit = 81
   integer              :: bin_type = 5
   integer              :: num_bins
   integer              :: num_bins2d
   integer, allocatable :: bin(:,:,:)
   integer, allocatable :: bin2d(:,:)

   integer :: ii, i, j, n, k, ierr, iv
   integer :: icase, icode
   integer :: istat
   logical :: isfile

   character(len=filename_len), allocatable :: filenames(:,:)
   character(len=DateStrLen),   allocatable :: filedates(:,:)
   character(len=filename_len)              :: output_file

   logical :: remove_ens_mean     = .true.  ! for ENS method
   logical :: remove_time_mean    = .true.  ! for NMC method
   logical :: write_stage3        = .false.

   type xdata_type
      real, allocatable :: value(:,:,:)
   end type xdata_type
   type (xdata_type), allocatable :: xdata(:,:,:)

   type be_type
      real, allocatable :: e_vec(:,:)
      real, allocatable :: e_val(:)
      real, allocatable :: e_vec_loc(:,:,:)
      real, allocatable :: e_val_loc(:,:)
      real, allocatable :: scale_length(:)
   end type be_type
   type (be_type), allocatable :: be_data(:)
   real(r_double), allocatable :: r8tmp1d(:)
   real(r_double), allocatable :: r8tmp2d(:,:)
   real(r_double), allocatable :: r8tmp2d2(:,:)
   real(r_double), allocatable :: r8tmp3d(:,:,:)

   ! for slen_opt=2
   real, allocatable :: mapfac_x(:,:)
   real, allocatable :: mapfac_y(:,:)

   ! for psi/chi calculation
   real, allocatable :: mapfac_m(:,:)
   real, allocatable :: mapfac_u(:,:)
   real, allocatable :: mapfac_v(:,:)

   real :: mean_scale
   real :: spike_tolerance = 1.5
   integer :: n_smth_sl = 2
   real, allocatable :: sl_smth(:) ! smoothed scale length

   integer :: ntasks_x, ntasks_y
   integer :: ids, ide, jds, jde, kds, kde
   integer, allocatable :: ips(:), ipe(:), jps(:), jpe(:), kps(:), kpe(:)
   integer :: ipe_out, jpe_out, kpe_out
   character(len=6) :: pe_name

   character(len=10) :: this_time
   character(len=8)  :: this_date

   continue ! end of declaration

#ifdef DM_PARALLEL
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world,num_procs,ierr)
   call mpi_comm_rank(mpi_comm_world,myproc,ierr)
#else
   num_procs = 1
   myproc = 0
#endif
 
   ! initialize namelist variables
   nc_list_file   = 'flist.txt'
   be_method = 'NMC'
   nens = 0
   varnames = 'none'
   outnames = 'none'
   yr_cutoff = 3.0   ! the same value as in wrfda/var/gen_be/gen_be_stage4_regional.f90
   do_eof_transform = .true.
   stride = 2
   do_pert_calc = .true.
   do_slen_calc = .true.
   slen_opt = 2
   write_ep = .false.
   write_ens_stdv = .false.
   ep_format = 2
   nproc_out = 1
   verbose = .false.
   es_ice = .false.
   aux_file = 'none'
   write_be_dat_r8 = .true.
   pert1_read_opt = 1
   vertloc_opt = 0
   time_lag_ens = .false.

   ! initialize internal options
   write_pert0 = .false.
   write_pert1 = .false.
   level_start = 1
   level_end = 100

   ! read namelist
   open(unit=nml_unit, file='namelist.gen_be_v3', status='old', form='formatted')
   read(unit=nml_unit, nml=gen_be_nml, iostat=istat)
   call error_handler(istat, 'reading namelist gen_be_nml')
   if ( myproc == root ) write(stdout,nml=gen_be_nml)
   read(unit=nml_unit, nml=ens_nml, iostat=istat)
   call error_handler(istat, 'reading namelist ens_nml')
   if ( myproc == root ) write(stdout,nml=ens_nml)

   ! find out how many variables to process
   nvar = 0
   do i = 1, nvar_max
      if ( trim(varnames(i)) /= 'none' ) then
         nvar = nvar + 1
      end if
   end do
   if ( myproc == root ) write(stdout,*) 'num_procs = ', num_procs
   if ( myproc == root ) write(stdout,*) 'nvar = ', nvar
   if ( nvar > 0 ) then
      allocate (do_this_var(nvar))
      allocate (var_dim(nvar))
   else
      call error_handler(-1, 'varnames not set in namelist.gen_be_v3')
   end if

   ! make outnames the same as varnames if not specified
   do i = 1, nvar
      if ( trim(outnames(i)) == 'none' ) then
         outnames(i) = varnames(i)
      end if
   end do

   ! convert varnames to be in upper case
   do iv = 1, nvar
      vartmp = varnames(iv)
      do i = 1, len_trim(vartmp)
         icode = ichar(vartmp(i:i))
         if (icode>=97 .and. icode<=122) then
            vartmp(i:i) = char(icode - 97 + 65)
         end if
      end do
      varnames(iv) = vartmp
      ! allow ps_u in user namelist, but set it to psfc_u internally
      if ( trim(varnames(iv)) == 'PS_U' ) then
         varnames(iv) = 'PSFC_U'
      end if
   end do

   ! convert be_method to be in upper case
   do i = 1, len_trim(be_method)
      icode = ichar(be_method(i:i))
      if (icode>=97 .and. icode<=122) then
         be_method(i:i) = char(icode - 97 + 65)
      end if
   end do

   if ( trim(be_method) == 'NMC' ) then
      ! hard-coded nens=2 for NMC application
      nens = 2
      ! make sure time_lag_ens is false for NMC method
      time_lag_ens = .false.
   else if ( trim(be_method) == 'ENS' ) then
      if ( nens == 0 ) then
         call error_handler(-1, &
            'nens (number of ensemble) is not set in the namelist.gen_be_v3 for be_method=ENS')
      end if
   else
      call error_handler(-1, &
         'be_method in namelist.gen_be_v3 should be either NMC or ENS')
   end if

   ! ext_ncd calls are called in subroutines get_info, read_fixed_fields, and compute_pert
   call ext_ncd_ioinit("",ierr)
   call error_handler(ierr, 'ext_ncd_ioinit')

   ! get number of cases, file names, and file dates from the input list
   ! as well as dimensions (ni, nj, nk), ds, and mp_physics option.
   call get_info(nc_list_file)

   nvar_avail = count(do_this_var(:))
   if ( nvar_avail == 0 ) then
      call error_handler(-1, 'no valid variables found from the varnames in namelist.gen_be_v3')
   end if

   if ( myproc == root ) then
      write(stdout,*) 'ncase, nens = ', ncase, nens
      if ( trim(be_method) == 'ENS' ) then
         if ( num_procs > nens ) then
            call error_handler(-1, 'num_procs must be smaller or equal to nens')
         end if
      end if
      if ( trim(be_method) == 'NMC' ) then
         if ( num_procs > ncase ) then
            call error_handler(-1, 'num_procs must be smaller or equal to ncase')
         end if
      end if
      if ( verbose ) then
         if ( ncase > 0 .and. nens > 0 ) then
            do i = 1, ncase
               do n = 1, nens
                  write(stdout,'(a,i4,a,i4,2x,a)') ' case: ', i, ' ens: ', n, trim(filenames(n,i))
               end do
            end do
         end if
      end if
   end if

   if ( myproc == root ) write(stdout, '(a,3i5,f10.2)') ' ni, nj, nk, ds = ', ni, nj, nk, ds

   ni1 = ni + 1
   nj1 = nj + 1

   ! divide nens among available processors
   allocate (istart_ens(0:num_procs-1))
   allocate (iend_ens  (0:num_procs-1))
   do i = 0, num_procs - 1
      call para_range(1, nens, num_procs, i, istart_ens(i), iend_ens(i))
   end do

   ! divide ncase among available processors
   allocate (istart_case(0:num_procs-1))
   allocate (iend_case  (0:num_procs-1))
   do i = 0, num_procs - 1
      call para_range(1, ncase, num_procs, i, istart_case(i), iend_case(i))
   end do

   ! loop indices for distributing pert calulcation among processors
   if ( trim(be_method) == 'ENS' ) then
      case_istart = 1
      case_iend   = ncase
      ens_istart  = istart_ens(myproc)
      ens_iend    = iend_ens(myproc)
   else if ( trim(be_method) == 'NMC' ) then
      case_istart = istart_case(myproc)
      case_iend   = iend_case(myproc)
      ens_istart  = 1
      ens_iend    = 2
   end if

   if ( calc_psi .or. calc_chi ) then
      allocate(mapfac_m(ni,nj))
      allocate(mapfac_u(ni1,nj))
      allocate(mapfac_v(ni,nj1))
      if ( trim(aux_file) == 'none' ) then
         ! read MAPFAC_M, MAPFAC_U and MAPFAC_V from file filenames(1,1)
         aux_file = filenames(1,1)
      end if
      call read_fixed_fields_mapfac(trim(aux_file))
   end if

   if ( do_slen_calc .and. slen_opt==2 ) then
      allocate(mapfac_x(ni,nj))
      allocate(mapfac_y(ni,nj))
      if ( trim(aux_file) == 'none' ) then
         ! read MAPFAC_MX and MAPFAC_MY from file filenames(1,1)
         aux_file = filenames(1,1)
      end if
      call read_fixed_fields(trim(aux_file))
   end if

   if ( vertloc_opt > 0 ) then
      allocate(vertloc_rho(nk,nk))
      allocate(vertloc_kcutoff(nk))
      if ( trim(aux_file) == 'none' ) then
         ! read vertical level info from file filenames(1,1)
         aux_file = filenames(1,1)
      end if
      call get_vertloc(trim(aux_file), nk, vertloc_rho, vertloc_kcutoff)
      if ( myproc == root ) then
         write(stdout,*)
         do k = 1,nk
            write(stdout, '(a,i4,2x,f4.1)')   ' vertloc: k, kcutoff   = ', k, vertloc_kcutoff(k)
            write(stdout, '(a,i4,100(f6.3))') ' vertloc: k, rho(k:nk) = ', k, vertloc_rho(k,k:nk)
         end do
         write(stdout,*)
      end if
   end if

   if ( do_pert_calc ) then
      call compute_pert
   end if

   call ext_ncd_ioexit(ierr)

   if ( calc_regcoeff .or. do_slen_calc ) then
      ! hard-code bin_type to be 5
      ! set num_bins, num_bins2d, bin, bin2d with bin_type=5 values
      allocate(bin(ni,nj,nk))
      allocate(bin2d(ni,nj))
      num_bins = nk
      num_bins2d = 1
      do k = 1, nk
         bin(:,:,k) = k
      end do
      bin2d(:,:) = 1
   end if

   if ( calc_regcoeff ) then
#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif
      call compute_regcoeff_unbalanced
      ! force pert1_read_opt=2 in order to read in unbalanced fields that are in separate files
      pert1_read_opt = 2
      !if ( myproc == root ) write(stdout, '(a,3i5,f10.2)') ' --- Using pert1_read_opt=2 for unbalanced fields ---'
   end if

   if ( do_slen_calc ) then
      allocate( be_data(nvar) )
      do i = 1, nvar
         allocate( be_data(i)%e_vec(1:nk,1:nk) )
         allocate( be_data(i)%e_val(1:nk) )
         allocate( be_data(i)%e_vec_loc(1:nk,1:nk,1:num_bins2d) )
         allocate( be_data(i)%e_val_loc(1:nk,1:num_bins2d) )
         allocate( be_data(i)%scale_length(1:nk) )
         be_data(i)%e_vec        = 0.0
         be_data(i)%e_val        = 0.0
         be_data(i)%e_vec_loc    = 0.0
         be_data(i)%e_val_loc    = 0.0
         be_data(i)%scale_length = 0.0
      end do
   end if

   if ( do_slen_calc ) then
#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif
      call compute_bv_sl
   end if

   if ( do_slen_calc ) then
#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif
      allocate(sl_smth(nk))
      if ( write_be_dat_r8 ) then
         allocate(r8tmp1d(1:nk))
         allocate(r8tmp2d(1:nk,1:nk))
         allocate(r8tmp2d2(1:nk,1:num_bins2d))
         allocate(r8tmp3d(1:nk,1:nk,1:num_bins2d))
      end if
      do i = 1, nvar
         if ( .not. do_this_var(i) ) cycle
         if ( myproc /= MOD((i-1), num_procs) ) cycle

         ! deal with zero scale_length
         do k = 1, nk
            if ( be_data(i)%scale_length(k) <= 0.0 ) then
               be_data(i)%scale_length(k) = be_data(i)%scale_length(k-1)
            end if
         end do

         ! Remove spikes in lengthscales (extrapolate if spike detected):
         do k = 2, nk-1
            mean_scale = 0.5 * ( be_data(i)%scale_length(k-1) + be_data(i)%scale_length(k+1) )
            if ( be_data(i)%scale_length(k) > spike_tolerance * mean_scale ) then
               be_data(i)%scale_length(k) = mean_scale
            end if
         end do

         ! Smooth the scale_length
         sl_smth(:) =  be_data(i)%scale_length(:)
         do n = 1, n_smth_sl
            do k = 2, nk-1
               sl_smth(k) = be_data(i)%scale_length(k)  &
                  + 0.25*(be_data(i)%scale_length(k-1)+ &
                          be_data(i)%scale_length(k+1)- &
                          2.0*be_data(i)%scale_length(k))
            end do
            be_data(i)%scale_length(:) = sl_smth(:)
         end do

         output_file = 'be_'//trim(varnames(i))//'.dat'
         write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
         open (be_unit, file=trim(output_file), form='unformatted', status='unknown')
         write(be_unit) ni, nj, nk, ds
         write(be_unit) bin_type
         write(be_unit) num_bins, num_bins2d
         write(be_unit) bin
         write(be_unit) bin2d
         !write(be_unit) nvar
         write(be_unit) varnames(i)
         write(be_unit) var_dim(i)
         if ( var_dim(i) == 3 ) then
            if ( write_be_dat_r8 ) then
               r8tmp2d  = be_data(i)%e_vec
               write(be_unit) r8tmp2d
               r8tmp1d  = be_data(i)%e_val
               write(be_unit) r8tmp1d
               r8tmp3d  = be_data(i)%e_vec_loc
               write(be_unit) r8tmp3d
               r8tmp2d2 = be_data(i)%e_val_loc
               write(be_unit) r8tmp2d2
               r8tmp1d  = be_data(i)%scale_length
               write(be_unit) r8tmp1d
            else
               write(be_unit) be_data(i)%e_vec
               write(be_unit) be_data(i)%e_val
               write(be_unit) be_data(i)%e_vec_loc
               write(be_unit) be_data(i)%e_val_loc
               write(be_unit) be_data(i)%scale_length
            end if
         else if ( var_dim(i) == 2 ) then
            if ( write_be_dat_r8 ) then
               r8tmp2d(1:1,1:1)   = be_data(i)%e_vec(1:1,1:1)
               write(be_unit) r8tmp2d(1:1,1:1)
               r8tmp1d(1:1)       = be_data(i)%e_val(1:1)
               write(be_unit) r8tmp1d(1:1)
               r8tmp3d(1:1,1:1,:) = be_data(i)%e_vec_loc(1:1,1:1,:)
               write(be_unit) r8tmp3d(1:1,1:1,:)
               r8tmp2d2(1:1,:)    = be_data(i)%e_val_loc(1:1,:)
               write(be_unit) r8tmp2d2(1:1,:)
              r8tmp1d(1:1)       = be_data(i)%scale_length(1:1)
               write(be_unit) r8tmp1d(1:1)
            else
               write(be_unit) be_data(i)%e_vec(1:1,1:1)
               write(be_unit) be_data(i)%e_val(1:1)
               write(be_unit) be_data(i)%e_vec_loc(1:1,1:1,:)
               write(be_unit) be_data(i)%e_val_loc(1:1,:)
               write(be_unit) be_data(i)%scale_length(1:1)
            end if
         end if
         close(be_unit)
         output_file = 'sl_'//trim(varnames(i))//'.txt'
         open (sl_unit, file=trim(output_file), form='formatted', status='unknown')
         if ( var_dim(i) == 3 ) then
            do k = 1, nk
               write(sl_unit,'(a,i3,f10.3)') varnames(i), k, be_data(i)%scale_length(k)
            end do
         else if ( var_dim(i) == 2 ) then
            write(sl_unit,'(a,i3,f10.3)') varnames(i), 1, be_data(i)%scale_length(1)
         end if
         close(sl_unit)
      end do
      deallocate(sl_smth)
      if ( write_be_dat_r8 ) then
         deallocate(r8tmp1d)
         deallocate(r8tmp2d)
         deallocate(r8tmp2d2)
         deallocate(r8tmp3d)
      end if
   end if

   if ( do_slen_calc ) then
      do i = 1, nvar
         deallocate( be_data(i)%e_vec )
         deallocate( be_data(i)%e_val )
         deallocate( be_data(i)%e_vec_loc )
         deallocate( be_data(i)%e_val_loc )
         deallocate( be_data(i)%scale_length )
      end do
      deallocate(be_data)
      if ( allocated(bin) ) deallocate(bin)
      if ( allocated(bin2d) ) deallocate(bin2d)
   end if

   if ( do_slen_calc .and. slen_opt==2 ) then
      if ( allocated(mapfac_x) ) deallocate (mapfac_x)
      if ( allocated(mapfac_y) ) deallocate (mapfac_y)
   end if

   if ( calc_psi .or. calc_chi ) then
      if ( allocated(mapfac_m) ) deallocate (mapfac_m)
      if ( allocated(mapfac_u) ) deallocate (mapfac_u)
      if ( allocated(mapfac_v) ) deallocate (mapfac_v)
   end if

   if ( allocated(filenames) ) deallocate(filenames)
   if ( allocated(filedates) ) deallocate(filedates)
   if ( allocated(do_this_var) ) deallocate(do_this_var)
   if ( allocated(var_dim) ) deallocate(var_dim)

   if ( vertloc_opt > 0 ) then
      if ( allocated(vertloc_rho) ) deallocate(vertloc_rho)
      if ( allocated(vertloc_kcutoff) ) deallocate(vertloc_kcutoff)
   end if

   deallocate (istart_ens)
   deallocate (iend_ens  )
   deallocate (istart_case)
   deallocate (iend_case  )

if ( myproc == root ) write(stdout,'(a)')' All Done!'

#ifdef DM_PARALLEL
   call mpi_finalize(ierr)
#endif

contains

subroutine get_info(nc_list_file)

   implicit none

   character(len=*), intent(in) :: nc_list_file

   integer :: iunit, ierr, fid, icase, iens
   integer :: nfile, ifile, i, n, icount
   integer :: icnt
   character(len=DateStrLen), allocatable :: file_init_dates(:)
   character(len=DateStrLen), allocatable :: file_valid_dates(:)
   integer, allocatable :: ilist_file(:,:)
   logical, allocatable :: valid(:)
   character(len=filename_len), allocatable :: nc_fnames(:)
   character(len=filename_len)              :: fname
   character(len=filename_len)              :: txtbuf
   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering = ' N/A' !dummy
   character(len=3)                :: ordering
   integer, dimension(4)           :: start_index, end_index
   integer                         :: ndim, wrftype
   character(len=DateStrLen)       :: DateStr
   real(r_single)                  :: dx

   iunit = 21
   ncase = 0

   ! get file names from nc_list_file
   nfile  = 0  ! initialize the number of netcdf files to read
   inquire(file=trim(nc_list_file), exist=isfile)
   if ( .not. isfile ) then
      call error_handler(-1, 'File not found: nc_list_file '//trim(nc_list_file) )
   else
      open(unit=iunit, file=trim(nc_list_file), status='old', form='formatted')
      !first find out the number of netcdf files to read
      istat = 0
      do while ( istat == 0 )
         read(unit=iunit, fmt='(a)', iostat=istat) txtbuf
         if ( istat /= 0 ) then
            exit
         else
            nfile = nfile + 1
         end if
      end do
      if ( nfile == 0 ) then
         call error_handler(-1, 'No valid files as specified in nc_list_file '//trim(nc_list_file) )
      end if
      allocate (nc_fnames(nfile))
      !read the nc_list_file again to get the netcdf file names
      rewind(iunit)
      do ifile = 1, nfile
         read(unit=iunit, fmt='(a)', iostat=istat) nc_fnames(ifile)
      end do
      close(iunit)
      allocate (file_init_dates(nfile))
      allocate (file_valid_dates(nfile))
      if ( .not. time_lag_ens ) then
         allocate (ilist_file(nens,nfile))
      else
         allocate (ilist_file(nfile,nfile))
      end if
      allocate (valid(nfile))
      file_init_dates(:)  = '0000-00-00_00:00:00'
      file_valid_dates(:) = '0000-00-00_00:00:00'
      ilist_file(:,:) = imiss
      valid(:) = .false.
   end if !nc_list_file

   icount = 0
   file_loop1: do ifile = 1, nfile

      fname = trim(nc_fnames(ifile))
      inquire(file=trim(fname), exist=isfile)
      if ( .not. isfile ) then
         if ( myproc == root ) write(stdout,'(a,a,a)') ' Warning: File not found: '//trim(fname)//'  Skipping it'
         cycle file_loop1
      end if

      call ext_ncd_open_for_read(trim(fname), 0, 0, "", fid, ierr)
      if ( ierr /= 0 ) then
         if ( myproc == root ) write(stdout, '(a,a,i8,a)') ' Warning: error opening ', trim(fname), ierr, '  Skipping it'
         cycle file_loop1
      end if

      call ext_ncd_get_next_time(fid, DateStr, ierr)
      if ( ierr == 0 ) then
         file_valid_dates(ifile) = DateStr
      else
         cycle file_loop1
      end if
      call ext_ncd_get_dom_ti_char(fid, 'SIMULATION_START_DATE', DateStr, ierr)
      if ( ierr == 0 ) then
         file_init_dates(ifile) = DateStr
      else
         cycle file_loop1
      end if

      valid(ifile) = .true.
      icount = icount + 1

      if ( icount == 1 ) then
         call ext_ncd_get_dom_ti_integer(fid, 'WEST-EAST_PATCH_END_UNSTAG',   ni, 1, icnt, ierr)
         call error_handler(ierr, 'reading attribute WEST-EAST_PATCH_END_UNSTAG')
         call ext_ncd_get_dom_ti_integer(fid, 'SOUTH-NORTH_PATCH_END_UNSTAG', nj, 1, icnt, ierr)
         call error_handler(ierr, 'reading attribute SOUTH-NORTH_PATCH_END_UNSTAG')
         call ext_ncd_get_dom_ti_integer(fid, 'BOTTOM-TOP_PATCH_END_UNSTAG',  nk, 1, icnt, ierr)
         call error_handler(ierr, 'reading attribute BOTTOM-TOP_PATCH_END_UNSTAG')
         call ext_ncd_get_dom_ti_integer(fid, 'MP_PHYSICS', mp_physics, 1, icnt, ierr)
         call error_handler(ierr, 'reading attribute MP_PHYSICS')
         call ext_ncd_get_dom_ti_real   (fid, 'DX', dx, 1, icnt, ierr)
         call error_handler(ierr, 'reading attribute DX')
         ds = dx
         read_t   = .false.
         read_qv  = .false.
         read_prs = .false.
         read_psfc= .false.
         read_u   = .false.
         read_v   = .false.
         calc_psi = .false.
         calc_chi = .false.
         calc_regcoeff = .false.
         indx_psi = -1
         do i = 1, nvar
            call ext_ncd_get_var_info (fid, trim(varnames(i)), ndim, ordering, staggering, &
                                       start_index, end_index, wrftype, ierr)
            var_dim(i) = ndim
            if ( ierr == 0 ) then ! direct variables
               do_this_var(i) = .true.
               if ( trim(varnames(i)) == 'T' ) then
                  read_prs = .true.
               end if
            else ! possible derived variables
               if ( trim(varnames(i)) == 'RH' ) then
                  do_this_var(i) = .true.
                  read_t   = .true.
                  read_qv  = .true.
                  read_prs = .true.
                  var_dim(i) = 3
               else if ( trim(varnames(i)) == 'PSI' ) then
                  do_this_var(i) = .true.
                  read_u = .true.
                  read_v = .true.
                  var_dim(i) = 3
                  calc_psi = .true.
                  indx_psi = i
               else if ( trim(varnames(i)) == 'CHI' ) then
                  do_this_var(i) = .true.
                  read_u = .true.
                  read_v = .true.
                  var_dim(i) = 3
                  calc_chi = .true.
               else if ( trim(varnames(i)) == 'CHI_U' ) then
                  do_this_var(i) = .true.
                  read_u = .true.
                  read_v = .true.
                  var_dim(i) = 3
                  calc_psi = .true.
                  calc_chi = .true.
                  calc_regcoeff = .true.
               else if ( trim(varnames(i)) == 'T_U' ) then
                  do_this_var(i) = .true.
                  read_prs = .true.
                  read_t = .true.
                  read_u = .true.
                  read_v = .true.
                  var_dim(i) = 3
                  calc_psi = .true.
                  calc_regcoeff = .true.
               else if ( trim(varnames(i)) == 'PSFC_U' ) then
                  do_this_var(i) = .true.
                  read_psfc = .true.
                  read_u = .true.
                  read_v = .true.
                  var_dim(i) = 2
                  calc_psi = .true.
                  calc_regcoeff = .true.
               else
                  do_this_var(i) = .false.
                  if ( myproc == root ) write(stdout,'(3a)') &
                     ' Warning: ', trim(varnames(i)), ' not available in the input file or not implemented, skipping it'
               end if
            end if ! ext_ncd_get_var_info check
         end do ! nvar loop
      end if ! one-time check

      call ext_ncd_ioclose(fid, ierr)

      if ( trim(be_method) == 'NMC' ) then
         if ( icount == 1 ) then
            icase = 0
         else if ( icount > 1 ) then
            find_match_loop_nmc: do ii = ifile-1, 1, -1
               if ( valid(ii) ) then
                  if ( file_valid_dates(ifile) == file_valid_dates(ii) ) then
                     icase = icase + 1
                     if ( file_init_dates(ifile) < file_init_dates(ii) ) then
                        ilist_file(1,icase) = ifile
                        ilist_file(2,icase) = ii
                     else
                        ilist_file(1,icase) = ii
                        ilist_file(2,icase) = ifile
                     end if
                     exit find_match_loop_nmc
                  end if
               end if
            end do find_match_loop_nmc
         end if

      else if ( trim(be_method) == 'ENS' ) then
         if ( icount == 1 ) then
            iens = 1
            icase = 1
            ilist_file(iens,icase) = ifile
         else if ( icount > 1 ) then
            if ( .not. time_lag_ens ) then
               find_match_loop_ens: do ii = ifile-1, 1, -1
                  if ( valid(ii) ) then
                     if ( file_init_dates(ifile) == file_init_dates(ii) ) then
                        if ( file_valid_dates(ifile) == file_valid_dates(ii) ) then
                           iens = iens + 1
                           ilist_file(iens,icase) = ifile
                           exit find_match_loop_ens
                        end if
                     else
                        icase = icase + 1
                        iens  = 1
                        ilist_file(iens,icase) = ifile
                        exit find_match_loop_ens
                     end if ! if same init_date
                  end if ! valid file
               end do find_match_loop_ens
            else ! time_lag_ens
               ! only check_valid_date for time_lag_ens
               if ( file_valid_dates(ifile) == file_valid_dates(ilist_file(iens,icase)) ) then
                  iens = iens + 1
                  ilist_file(iens,icase) = ifile
               end if
            end if ! time_lag_ens
         end if
      end if
   end do file_loop1

   ncase = icase
   if ( time_lag_ens ) then
      nens = iens
      if ( myproc == root ) then
         write(stdout,'(a,i4,a)') ' --- Resetting nens = ', nens, ' for time_lag_ens ---'
      end if
   end if

   if ( ncase > 0 .and. nens > 0 ) then
      if ( .not. allocated(filenames) ) allocate(filenames(1:nens,1:ncase))
      if ( .not. allocated(filedates) ) allocate(filedates(1:nens,1:ncase))
      filenames(:,:) = 'missing'
      filedates(:,:) = '0000-00-00_00:00:00'
      do i = 1, ncase
         do n = 1, nens
            if ( ilist_file(n,i) /= imiss ) then
               filenames(n,i) = nc_fnames(ilist_file(n,i))
               filedates(n,i) = file_valid_dates(ilist_file(n,i))
            end if
         end do
      end do
   end if

   deallocate (ilist_file)
   deallocate (file_init_dates)
   deallocate (file_valid_dates)
   deallocate (valid)

end subroutine get_info

subroutine read_fixed_fields(input_file)

   implicit none

   integer :: fid, ierr
   character(len=*),intent(in) :: input_file
   character(len=DateStrLen)   :: DateStr
   real(r_single), allocatable :: xfield2d(:,:)
   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering = ' N/A' !dummy
   character(len=3)                :: ordering
   integer, dimension(4)           :: start_index, end_index
   integer                         :: ndim, wrftype
   character(len=512)              :: message
   character(len=VarNameLen)       :: varname

   !input_file = trim(filenames(1,1))

   call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
   write(message,'(a,i3,a,a)') ' Proc ', myproc, ' opening ', trim(input_file)
   call error_handler(ierr, trim(message))

   call ext_ncd_get_next_time(fid, DateStr, ierr)
   call error_handler(ierr, 'ext_ncd_get_next_time')

   if ( .not. allocated(xfield2d) ) allocate (xfield2d(ni, nj))
   !if ( .not. allocated(lat) ) allocate (lat(ni, nj))
   !varname = 'XLAT'
   !call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
   !                           start_index, end_index, wrftype, ierr)
   !call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   !call ext_ncd_read_field(fid, DateStr, trim(varname),  &
   !                        xfield2d(:,:), wrftype,       &
   !                        0, 0, 0, ordering,            &
   !                        staggering, dimnames,         & !dummy
   !                        start_index, end_index,       & !dom
   !                        start_index, end_index,       & !mem
   !                        start_index, end_index,       & !pat
   !                        ierr                   )
   !call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   !lat(:,:) = xfield2d(:,:)

   if ( .not. allocated(mapfac_x) ) allocate (mapfac_x(ni, nj))
   varname = 'MAPFAC_MX'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield2d(:,:), wrftype,       &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   mapfac_x(:,:) = xfield2d(:,:)

   if ( .not. allocated(mapfac_y) ) allocate (mapfac_y(ni, nj))
   varname = 'MAPFAC_MY'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield2d(:,:), wrftype,       &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   mapfac_y(:,:) = xfield2d(:,:)

   call ext_ncd_ioclose(fid, ierr)
   if ( allocated(xfield2d) ) deallocate (xfield2d)

end subroutine read_fixed_fields

subroutine read_fixed_fields_mapfac(input_file)

   implicit none

   integer :: fid, ierr
   character(len=*),intent(in)     :: input_file
   character(len=DateStrLen)       :: DateStr
   real(r_single), allocatable     :: xfield2d(:,:)
   real(r_single), allocatable     :: xfield2d_u(:,:)
   real(r_single), allocatable     :: xfield2d_v(:,:)
   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering = ' N/A' !dummy
   character(len=3)                :: ordering
   integer, dimension(4)           :: start_index, end_index
   integer                         :: ndim, wrftype
   character(len=512)              :: message
   character(len=VarNameLen)       :: varname

   !input_file = trim(filenames(1,1))

   call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
   write(message,'(a,i3,a,a)') ' Proc ', myproc, ' opening ', trim(input_file)
   call error_handler(ierr, trim(message))

   call ext_ncd_get_next_time(fid, DateStr, ierr)
   call error_handler(ierr, 'ext_ncd_get_next_time')

   if ( .not. allocated(xfield2d) ) allocate (xfield2d(ni, nj))

   if ( .not. allocated(mapfac_m) ) allocate (mapfac_m(ni, nj))
   varname = 'MAPFAC_M'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield2d(:,:), wrftype,       &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   mapfac_m(:,:) = xfield2d(:,:)
   deallocate (xfield2d)

   if ( .not. allocated(xfield2d_u) ) allocate (xfield2d_u(ni1, nj))

   if ( .not. allocated(mapfac_u) ) allocate (mapfac_u(ni1, nj))
   varname = 'MAPFAC_U'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield2d_u(:,:), wrftype,     &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   mapfac_u(:,:) = xfield2d_u(:,:)
   deallocate (xfield2d_u)

   if ( .not. allocated(xfield2d_v) ) allocate (xfield2d_v(ni, nj1))

   if ( .not. allocated(mapfac_v) ) allocate (mapfac_v(ni, nj1))
   varname = 'MAPFAC_V'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield2d_v(:,:), wrftype,     &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   mapfac_v(:,:) = xfield2d_v(:,:)
   deallocate (xfield2d_v)

   call ext_ncd_ioclose(fid, ierr)

end subroutine read_fixed_fields_mapfac

subroutine get_vertloc(input_file, kz, rho, kcutoff)

   implicit none

   character(len=*),intent(in) :: input_file
   integer, intent(in)         :: kz
   real, intent(inout)         :: rho(kz,kz)
   real, intent(inout)         :: kcutoff(kz)

   integer                         :: fid, ierr
   character(len=DateStrLen)       :: DateStr
   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering = ' N/A' !dummy
   character(len=3)                :: ordering
   integer, dimension(4)           :: start_index, end_index
   integer                         :: ndim, wrftype
   character(len=512)              :: message
   character(len=VarNameLen)       :: varname
   real(r_single), allocatable     :: xfield(:)

   integer           :: k, k1, i
   real              :: kscale, kscale_invsq, kdist
   real              :: cutoff
   real              :: p_top, base_pres
   real, allocatable :: znw(:)
   real, allocatable :: p_w(:)  ! pressure at full levels
   real, allocatable :: ln_p_w(:)
   real, allocatable :: dlnp(:,:)
   real, allocatable :: kcutoff_tmp(:)
   logical           :: smooth_kcutoff

   call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
   write(message,'(a,i3,a,a)') ' Proc ', myproc, ' opening ', trim(input_file)
   call error_handler(ierr, trim(message))

   call ext_ncd_get_next_time(fid, DateStr, ierr)
   call error_handler(ierr, 'ext_ncd_get_next_time')

   if ( .not. allocated(xfield) ) allocate (xfield(kz+1))
   varname = 'P_TOP'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield(1), wrftype,           &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   p_top = xfield(1)

   varname = 'P00'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield(1), wrftype,           &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   base_pres = xfield(1)

   if ( .not. allocated(znw) ) allocate(znw(kz+1))
   varname = 'ZNW'
   call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                              start_index, end_index, wrftype, ierr)
   call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
   call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                           xfield(:), wrftype,           &
                           0, 0, 0, ordering,            &
                           staggering, dimnames,         & !dummy
                           start_index, end_index,       & !dom
                           start_index, end_index,       & !mem
                           start_index, end_index,       & !pat
                           ierr                   )
   call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
   znw(:) = xfield(:)

   call ext_ncd_ioclose(fid, ierr)
   if ( allocated(xfield) ) deallocate (xfield)

   allocate(p_w(1:kz+1))
   allocate(ln_p_w(1:kz+1))
   allocate(dlnp(1:kz,1:kz))

   ! empirical settings
   cutoff   = 1.0/2.71828 !1/e

   do k = 1, kz+1
      p_w(k) = znw(k)*(base_pres - p_top) + p_top
      ln_p_w(k) = log(max(p_w(k),0.0001))
   end do
   kcutoff(:) = 1.0  ! initialize
   do k = 1, kz
      do k1 = k+1, kz
         dlnp(k,k1) = abs(ln_p_w(k)-ln_p_w(k1))
         if ( dlnp(k,k1) <= cutoff ) then
            kcutoff(k) = k1-k+1
            !if ( 0.5*(p_w(k)+p_w(k+1)) <= 15000.0 ) then ! 15000.0Pa
            !   kcutoff(k) = min(kcutoff(k), 1.0)
            !end if
         end if
      end do
   end do

   ! smoothing
   !smooth_kcutoff = .false.
   smooth_kcutoff = .true.
   if ( smooth_kcutoff ) then
      allocate(kcutoff_tmp(1:kz))
      kcutoff_tmp(:) = kcutoff(:)
      do i = 1, 2 ! two passes
         do k = 2, kz-1
            kcutoff_tmp(k)  = kcutoff(k)+ 0.25 * ( kcutoff(k-1) + kcutoff(k+1) -2.0*kcutoff(k) )
         end do
         kcutoff(:) = kcutoff_tmp(:)
      end do
      deallocate(kcutoff_tmp)
   end if

   deallocate(dlnp)
   deallocate(ln_p_w)
   deallocate(p_w)

   ! specify probability densities:
   rho(:,:) = 1.0  ! initialize
   do k = 1, kz
      kscale = kcutoff(k)
      kscale_invsq = 1.0 / ( kscale * kscale )
      do k1 = k, kz
         kdist = k1 - k
         rho(k,k1) = exp ( -1.0 * real(kdist * kdist) * kscale_invsq )
         rho(k1,k) = rho(k,k1)
      end do
   end do

end subroutine get_vertloc

subroutine compute_pert

   implicit none

   integer :: ounit
   integer :: i, j, k
   integer(i_kind) :: ijk
   integer :: iv, ie, ic, n, icnt
   integer :: fid, ierr

   character(len=filename_len) :: input_file, output_file
   character(len=3)   :: ce
   character(len=VarNameLen)       :: varname

   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering = ' N/A' !dummy
   character(len=3)                :: ordering
   integer, dimension(4)           :: start_index, end_index
   integer                         :: ndim, wrftype
   character(len=DateStrLen)       :: DateStr
   character(len=512)              :: message

   integer, allocatable :: iproc_var(:)

   integer :: ivs, ive, ie_indx
   integer :: my_nvar, dest_proc
   integer :: this_mpi_real

   real(r_single), allocatable :: xfield(:,:,:)
   real(r_single), allocatable :: xfield_u(:,:,:)
   real(r_single), allocatable :: xfield_v(:,:,:)
   real(r_single), allocatable :: xfield_w(:,:,:)

   real, allocatable :: vor(:,:,:)    ! vorticity
   real, allocatable :: div(:,:,:)    ! divergence
   real, allocatable :: psi_s(:,:,:)  ! stream function
   real, allocatable :: psi(:,:,:)    ! stream function on mass grid
   real, allocatable :: chi(:,:,:)    ! velocity potential
   real, allocatable :: ustag(:,:,:)  ! u on staggered grid
   real, allocatable :: vstag(:,:,:)  ! v on staggered grid
   real, allocatable :: psfc(:,:)     ! surface pressure
   real, allocatable :: prs(:,:,:)    ! prs on half levels
   real, allocatable :: prs_w(:,:,:)  ! prs on full levels
   real, allocatable :: ght(:,:,:)
   real, allocatable :: theta(:,:,:)  ! potential temperature
   real, allocatable :: q(:,:,:)      ! qv mixing ratio
   real, allocatable :: mu(:,:)
   real, allocatable :: mub(:,:)
   real, allocatable :: znw(:)
   real    :: p_top
   real    :: tk, es, qs
   logical :: got_prs, got_th, got_qv, got_u, got_v, got_psfc

   real, allocatable :: time_mean(:,:,:,:)
   real(r_double), allocatable :: xsum_loc(:,:,:,:), xsum(:,:,:,:)

   real(r_single), allocatable :: ens_mean(:,:,:,:)
   real(r_single), allocatable :: ens_stdv(:,:,:,:)
   real(r_double), allocatable :: ens_sum_loc(:,:,:)
   real(r_double), allocatable :: ens_sum(:,:,:)
   real(r_double), allocatable :: r8tmp(:,:,:)
   real, allocatable :: xtmp4d(:,:,:,:)
   real, allocatable :: globuf(:,:,:,:)
   real, allocatable :: locbuf(:,:,:,:)

   if ( myproc == root ) then
      if ( trim(be_method) == 'ENS' ) then
         if ( remove_ens_mean ) then
            write(stdout,'(a)')' ====== Computing gen_be ensemble perturbation ======'
         else
            write(stdout,'(a)')' ====== Computing gen_be ensemble forecast files ======'
         end if
         write(stdout,'(a)')' Perturbations are in MODEL SPACE'
         write(stdout,'(a,i4)')' Ensemble Size = ', nens
      else
         write(stdout,'(a)')' ====== Computing gen_be NMC-method forecast differences ======'
      end if
   end if

   ounit = 61

   ni1 = ni + 1
   nj1 = nj + 1
   nk1 = nk + 1
   ijk = ni * nj * nk

   write(stdout,'(a,i4,a,i4,a,i4)') &
      ' Proc ', myproc, ' will read  ens files ', ens_istart, ' - ', ens_iend
   write(stdout,'(a,i4,a,i4,a,i4)') &
      ' Proc ', myproc, ' will read case files ', case_istart, ' - ', case_iend

   if ( ens_iend >= ens_istart .and. case_iend >= case_istart ) then
      if ( calc_psi .and. indx_psi < 0 ) then
         ! when psi is not in the var list but is needed for unbalanced fields
         ! allocate index nvar+1 for psi
         allocate(xdata(nvar+1,ens_istart:ens_iend,case_istart:case_iend), stat=ierr)
         indx_psi = nvar + 1
         nvar_all = nvar + 1
      else
         allocate(xdata(nvar,ens_istart:ens_iend,case_istart:case_iend), stat=ierr)
         nvar_all = nvar
      end if
      call error_handler(ierr, 'allocating xdata in compute_pert')
   end if

   do ic = case_istart, case_iend
      do ie = ens_istart, ens_iend
         do iv = 1, nvar_all
            allocate(xdata(iv,ie,ic)%value(ni,nj,nk), stat=ierr)
            call error_handler(ierr, 'allocating xdata%value in compute_pert')
            xdata(iv,ie,ic)%value(:,:,:) = 0.0
         end do
      end do
   end do

   if ( myproc == root ) then
      write(stdout,'(a)')' ====== Reading data from netCDF files ======'
   end if

   case_loop1: do ic = case_istart, case_iend
      ens_loop1: do ie = ens_istart, ens_iend

         input_file = trim(filenames(ie,ic))

         got_qv = .false.
         got_th = .false.
         got_u  = .false.
         got_v  = .false.
         got_psfc = .false.

         call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
         write(message,'(a,i3,a,a)') ' Proc ', myproc, ' opening ', trim(input_file)
         call error_handler(ierr, trim(message))

         call ext_ncd_get_next_time(fid, DateStr, ierr)
         call error_handler(ierr, 'ext_ncd_get_next_time')

         if ( .not. allocated(xfield) ) allocate (xfield(ni, nj, nk))

         ! read some basic fields
         if ( read_prs ) then
            ! read P and PB to get prs for converting T (theta) to temperature
            varname = 'P'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            !call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            if ( ierr == 0 ) then
               if ( .not. allocated(prs) ) allocate (prs(ni, nj, nk))
               call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                       xfield, wrftype,              &
                                       0, 0, 0, ordering,            &
                                       staggering, dimnames,         & !dummy
                                       start_index, end_index,       & !dom
                                       start_index, end_index,       & !mem
                                       start_index, end_index,       & !pat
                                       ierr                   )
               call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               prs(:,:,:) = xfield(:,:,:)
               varname = 'PB'
               call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering, &
                                          start_index, end_index, wrftype, ierr)
               call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
               call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                       xfield, wrftype,              &
                                       0, 0, 0, ordering,            &
                                       staggering, dimnames,         & !dummy
                                       start_index, end_index,       & !dom
                                       start_index, end_index,       & !mem
                                       start_index, end_index,       & !pat
                                       ierr                   )
               call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               prs(:,:,:) = prs(:,:,:) + xfield(:,:,:) !pp + pb
               got_prs = .true.
            else
               got_prs = .false.
            end if ! p, pb
         end if ! read_prs

         if ( read_psfc ) then
            if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
            varname = 'PSFC'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield(:,:,1), wrftype,    &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            if ( .not. allocated(psfc) ) allocate (psfc(ni, nj))
            psfc(:,:) = xfield(:,:,1)
            got_psfc = .true.
         end if

         if ( read_ght ) then
            if ( .not. allocated(ght) )      allocate (ght(ni, nj, nk))
            if ( .not. allocated(xfield_w) ) allocate (xfield_w(ni, nj ,nk1))
            varname = 'PH'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield_w, wrftype,            &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     xfield(i,j,k) = 0.5*(xfield_w(i,j,k)+xfield_w(i,j,k+1))
                  end do
               end do
            end do
            varname = 'PHB'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering, &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield_w, wrftype,            &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     ght(i,j,k) = xfield(i,j,k) +  &
                                  0.5*(xfield_w(i,j,k)+xfield_w(i,j,k+1) )
                  end do
               end do
            end do
         end if ! read_ght

         if ( read_t ) then
            ! t is needed for rh calculation
            if ( .not. allocated(theta) ) allocate (theta(ni, nj, nk))
            varname = 'T'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield, wrftype,              &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            theta(:,:,:) = xfield(:,:,:) + t00
            got_th = .true.
         end if

         if ( read_qv .or. (read_prs .and. (.not. got_prs)) ) then
            ! qv is needed for rh and prs calculation
            if ( .not. allocated(q) )  allocate (q(ni, nj, nk))
            varname = 'QVAPOR'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield, wrftype,              &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            ! from mixing ratio to specific humidity
            !q(:,:,:) = xfield(:,:,:) / ( 1.0 + xfield(:,:,:) )
            q(:,:,:) = xfield(:,:,:) ! stay in mixing ratio for now
            got_qv = .true.
         end if ! calc_rh

         if ( read_prs .and. (.not. got_prs) ) then
            !if ( myproc == root ) write(stdout,'(a)') '    --- calculating hydrostatic pressure ---'
            ! need p_top, mu, mub, znw, and qvapor to calculate hydrostatic pressure
            varname = 'P_TOP'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield(1,1,1), wrftype,       &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            p_top = xfield(1,1,1)

            if ( .not. allocated(mu) ) allocate(mu(ni,nj))
            varname = 'MU'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield(:,:,1), wrftype,       &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            mu(:,:) = xfield(:,:,1)

            if ( .not. allocated(mub) ) allocate(mub(ni,nj))
            varname = 'MUB'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield(:,:,1), wrftype,       &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            mub(:,:) = xfield(:,:,1)

            if ( .not. allocated(znw) ) allocate(znw(nk1))
            if ( .not. allocated(xfield_w) ) allocate (xfield_w(ni, nj ,nk1))
            varname = 'ZNW'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, trim(varname),  &
                                    xfield_w(1,1,:), wrftype,     &
                                    0, 0, 0, ordering,            &
                                    staggering, dimnames,         & !dummy
                                    start_index, end_index,       & !dom
                                    start_index, end_index,       & !mem
                                    start_index, end_index,       & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            znw(:) = xfield_w(1,1,:)
            deallocate (xfield_w)

            if ( .not. allocated(prs  ) )    allocate (prs  (ni, nj, nk))
            if ( .not. allocated(prs_w) )    allocate (prs_w(ni, nj, nk+1))

            ! the following is adapted from p_hyd calculation in WRF subroutine phy_prep
            do j = 1, nj
               do i = 1, ni
                  prs_w(i,j,nk+1) = p_top
               end do
            end do
            do k = nk, 1, -1
               do j = 1, nj
                  do i = 1, ni
                     prs_w(i,j,k) = prs_w(i,j,k+1) - (1.0+q(i,j,k))*(mu(i,j)+mub(i,j))*(znw(k+1)-znw(k))
                  end do
               end do
            end do
            do k = nk, 1, -1
               do j = 1, nj
                  do i = 1, ni
                     prs(i,j,k) = 0.5*(prs_w(i,j,k)+prs_w(i,j,k+1))
                  end do
               end do
            end do
            deallocate (prs_w)
            deallocate (mu)
            deallocate (mub)
            deallocate (znw)
         end if ! read_prs, got_prs

         if ( read_u ) then
            if ( .not. allocated(xfield_u) ) allocate (xfield_u(ni1,nj, nk))
            if ( .not. allocated(ustag) ) allocate (ustag(ni1, nj, nk))
            varname = 'U'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield_u(:,:,:), wrftype,  &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            ustag(:,:,:) = xfield_u(:,:,:)
            got_u = .true.
         end if

         if ( read_v ) then
            if ( .not. allocated(xfield_v) ) allocate (xfield_v(ni, nj1,nk))
            if ( .not. allocated(vstag) ) allocate (vstag(ni, nj1, nk))
            varname = 'V'
            call ext_ncd_get_var_info (fid, trim(varname), ndim, ordering, staggering,  &
                                       start_index, end_index, wrftype, ierr)
            call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield_v(:,:,:), wrftype,  &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
            vstag(:,:,:) = xfield_v(:,:,:)
            got_v = .true.
         end if

         if ( calc_psi .and. got_u .and. got_v ) then
            if ( .not. allocated(vor) ) allocate (vor(ni+1, nj+1, nk))
            if ( .not. allocated(psi_s) ) allocate (psi_s(ni+1, nj+1, nk))
            if ( .not. allocated(psi) )   allocate (psi(ni, nj, nk))
            ! Calculate vorticity (in center of mass grid on WRF's Arakawa C-grid)
            call da_uv_to_vor_c(ni, nj, nk, ds, &
                                mapfac_m, mapfac_u, mapfac_v, ustag, vstag, vor)
            ! Calculate streamfunction
            ! Assumes vor converted to Del**2 psi
            call da_del2a_to_a(ni+1, nj+1, nk, ds, vor, psi_s)

            ! interpolate psi to mass points
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     psi(i,j,k) = 0.25 * ( psi_s(i,j,k) + psi_s(i+1,j,k) +     &
                                           psi_s(i,j+1,k) + psi_s(i+1,j+1,k) )
                  end do
               end do
            end do
            if ( allocated(psi_s) ) deallocate (psi_s)
         end if ! calc_psi

         if ( calc_chi .and. got_u .and. got_v ) then
            if ( .not. allocated(div) ) allocate (div(ni, nj, nk))
            if ( .not. allocated(chi) ) allocate (chi(ni, nj, nk))
            ! Calculate divergence (at mass pts. on WRF's Arakawa C-grid)
            call da_uv_to_div_c(ni, nj, nk, ds, &
                                 mapfac_m, mapfac_u, mapfac_v, ustag, vstag, div)
            ! Calculate velocity potential
            ! Assumes div converted to Del**2 chi
            call da_del2a_to_a(ni, nj, nk, ds, div, chi)
         end if ! calc_chi

         var_loop1: do iv = 1, nvar

            if ( .not. do_this_var(iv) ) cycle var_loop1

            varname = trim(varnames(iv))
            call ext_ncd_get_var_info (fid, varname, ndim, ordering, staggering, &
                                       start_index, end_index, wrftype, ierr)
            !call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))

            !write(stdout, '(a,i3,a,a8,a,a)') ' Proc ', myproc, ' Reading ', &
            !   trim(varname), ' from ', trim(input_file)

            if ( varname == 'PSFC' .or. varname == 'PSFC_U' ) then
               if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
               if ( .not. got_psfc ) then
                  call ext_ncd_read_field(fid, DateStr, varname,  &
                                       xfield(:,:,1), wrftype,    &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
                  call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               else
                  xfield(:,:,1) = psfc(:,:)
               end if
               xdata(iv,ie,ic)%value(:,:,1) = xfield(:,:,1)
            else if ( varname == 'U' ) then
               if ( .not. allocated(xfield_u) ) allocate (xfield_u(ni1,nj, nk))
               if ( .not. got_u ) then
                  call ext_ncd_read_field(fid, DateStr, varname,  &
                                       xfield_u(:,:,:), wrftype,  &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
                  call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               else
                  xfield_u(:,:,:) = ustag(:,:,:)
               end if
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        xdata(iv,ie,ic)%value(i,j,k) = &
                           0.5 * ( xfield_u(i,j,k) + xfield_u(i+1,j,k) )
                     end do
                  end do
               end do
            else if ( varname == 'V' ) then
               if ( .not. allocated(xfield_v) ) allocate (xfield_v(ni, nj1,nk))
               if ( .not. got_v ) then
                  call ext_ncd_read_field(fid, DateStr, varname,  &
                                       xfield_v(:,:,:), wrftype,  &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
                  call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               else
                  xfield_v(:,:,:) = vstag(:,:,:)
               end if
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        xdata(iv,ie,ic)%value(i,j,k) = &
                           0.5 * ( xfield_v(i,j,k) + xfield_v(i,j+1,k) )
                     end do
                  end do
               end do
            else if ( varname == 'W' ) then
               if ( .not. allocated(xfield_w) ) allocate (xfield_w(ni, nj ,nk1))
               call ext_ncd_read_field(fid, DateStr, varname,     &
                                       xfield_w(:,:,:), wrftype,  &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
               call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        xdata(iv,ie,ic)%value(i,j,k) = &
                           0.5 * ( xfield_w(i,j,k) + xfield_w(i,j,k+1) )
                     end do
                  end do
               end do
            else if ( varname == 'RH' ) then
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        !tc = (theta(i,j,k)*(prs(i,j,k)/p00)**kappa) - t_kelvin
                        !es = 611.2 * exp( 17.67 * tc / (tc + 243.5) )
                        !qs = rd_over_rv * es /( prs(i,j,k) - rd_over_rv1 * es)
                        tk = theta(i,j,k)*(prs(i,j,k)/p00)**kappa
                        call calc_es_qs(tk, prs(i,j,k), es, qs, es_ice)
                        ! get RH is ratio, not percentage
                        xdata(iv,ie,ic)%value(i,j,k) = (q(i,j,k)/(1.0+q(i,j,k))) / qs
                     end do
                  end do
               end do
            else if ( varname == 'QVAPOR' ) then
               if ( got_qv ) then
                  ! from mixing ratio to specific humidity
                  xdata(iv,ie,ic)%value(:,:,:) = q(:,:,:)/(1.0+q(:,:,:))
               else
                  if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
                  call ext_ncd_read_field(fid, DateStr, varname,     &
                                          xfield(:,:,:), wrftype,    &
                                          0, 0, 0, ordering,         &
                                          staggering, dimnames,      & !dummy
                                          start_index, end_index,    & !dom
                                          start_index, end_index,    & !mem
                                          start_index, end_index,    & !pat
                                          ierr                   )
                  call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
                  ! from mixing ratio to specific humidity
                  xdata(iv,ie,ic)%value(:,:,:) = &
                     xfield(:,:,:) / ( 1.0 + xfield(:,:,:) )
               end if ! got_qv
            else if ( varname == 'T' .or. varname == 'T_U' ) then
               if ( got_th ) then
                  ! potential temperature to temperature
                  xdata(iv,ie,ic)%value(:,:,:) = &
                     theta(:,:,:)*(prs(:,:,:)/p00)**kappa
               else
                  if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
                  call ext_ncd_read_field(fid, DateStr, varname,     &
                                          xfield(:,:,:), wrftype,    &
                                          0, 0, 0, ordering,         &
                                          staggering, dimnames,      & !dummy
                                          start_index, end_index,    & !dom
                                          start_index, end_index,    & !mem
                                          start_index, end_index,    & !pat
                                          ierr                   )
                  call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
                  ! perturbation potential temperature to temperature
                  xdata(iv,ie,ic)%value(:,:,:) = &
                     (t00+xfield(:,:,:))*(prs(:,:,:)/p00)**kappa
               end if ! got_th
            else if ( varname == 'PSI' ) then
               xdata(iv,ie,ic)%value(:,:,:) = psi(:,:,:)
            else if ( varname == 'CHI' .or. varname == 'CHI_U' ) then
               xdata(iv,ie,ic)%value(:,:,:) = chi(:,:,:)
            else
               if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
               call ext_ncd_read_field(fid, DateStr, varname,     &
                                       xfield(:,:,:), wrftype,    &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
               call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               xdata(iv,ie,ic)%value(:,:,:) = xfield
            end if

            !if ( varname(1:1) == 'Q' ) then
            !    ! from kg/kg to g/kg
            !    xdata(iv,ie,ic)%value(:,:,:) = xdata(iv,ie,ic)%value(:,:,:)*1000.0
            !end if

         end do var_loop1 ! nvar loop

         if ( indx_psi > nvar ) then
            xdata(indx_psi,ie,ic)%value(:,:,:) = psi(:,:,:)
         end if

         call ext_ncd_ioclose(fid, ierr)

      end do ens_loop1 ! ensemble member loop

      write(stdout,'(a,i3,a,a)') ' Proc', myproc, &
         ' Computing perturbations for date ', filedates(1,ic)

      if ( trim(be_method) == 'NMC' ) then

         if ( remove_time_mean ) then
            ! allocate for summing up the cases each processor sees
            if ( .not. allocated(xsum_loc) )  then
               allocate(xsum_loc(ni,nj,nk,nvar_all), stat=ierr)
               call error_handler(ierr, 'allocating xsum_loc')
               xsum_loc = 0.0
            end if
         end if

         do iv = 1, nvar_all
            ! store forecast difference in index 1 of ie
            xdata(iv,1,ic)%value(:,:,:) = xdata(iv,1,ic)%value(:,:,:) - xdata(iv,2,ic)%value(:,:,:)
            if ( remove_time_mean ) then
               ! accumulating sums for calculating case/time mean later
               xsum_loc(:,:,:,iv) = xsum_loc(:,:,:,iv)+xdata(iv,1,ic)%value(:,:,:)
            end if
         end do

         if ( write_pert0 ) then
            output_file = 'pert0.'//filedates(1,ic)//'.e001'
            !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
            open (ounit, file = output_file, form='unformatted')
            write(ounit) nvar, ni, nj, nk
            do iv = 1, nvar
               write(ounit) var_dim(iv)
               write(ounit) filedates(1,ic), varnames(iv)
               if ( var_dim(iv) == 2 ) then
                  write(ounit) xdata(iv,1,ic)%value(:,:,1)
               else if ( var_dim(iv) == 3 ) then
                  write(ounit) xdata(iv,1,ic)%value(:,:,1:nk)
               end if
            end do
            close(ounit)
         end if

      end if ! be_method=NMC

      if ( trim(be_method) == 'ENS' ) then

         if ( .not. allocated(ens_sum_loc) ) allocate(ens_sum_loc(ni,nj,nk))
         if ( .not. allocated(ens_sum) )     allocate(ens_sum(ni,nj,nk))
         if ( .not. allocated(ens_mean) )    allocate(ens_mean(ni,nj,nk,nvar_all))

         if ( myproc == root ) write(stdout,'(a)') ' ====== Computing ensemble mean ======'

         var_loop2: do iv = 1, nvar_all
            if ( iv <= nvar ) then
               if ( .not. do_this_var(iv) ) cycle var_loop2
            end if
            ens_sum_loc(:,:,:) = 0.0 ! initialize for each variable
            ens_sum    (:,:,:) = 0.0 ! initialize for each variable
            do n = istart_ens(myproc),iend_ens(myproc)
               ens_sum_loc(:,:,:) = ens_sum_loc(:,:,:)+xdata(iv,n,ic)%value(:,:,:)
            end do
#ifdef DM_PARALLEL
            call mpi_allreduce(ens_sum_loc(:,:,:),ens_sum(:,:,:),ijk, &
                               mpi_real8,mpi_sum,mpi_comm_world,ierr)
            if ( ierr /= 0 ) then
               write(stdout, '(a, i3)') 'Error mpi_allreduce on proc', myproc
               call mpi_abort(mpi_comm_world,1,ierr)
            end if
#else
            ens_sum(:,:,:) = ens_sum_loc(:,:,:)
#endif
            ! The order of summation may be different for different number
            ! of processors and that may lead to slightly different results
            ! (10**(-13) or smaller).
            ! Store the mean in real*4 to get around the tiny differences
            ! in order to get identical results when different numbers of
            ! processors are used.
            ens_mean(:,:,:,iv) = ens_sum(:,:,:)/real(nens)
         end do var_loop2

         if ( .not. write_ens_stdv ) then
            deallocate(ens_sum_loc)
            deallocate(ens_sum)
         end if

         if ( myproc == root ) write(stdout,'(a)') ' ====== Computing ensemble perturbations ======'
         do iv = 1, nvar_all
            if ( iv <= nvar ) then
               if ( .not. do_this_var(iv) ) cycle
            end if
            do ie = ens_istart, ens_iend ! each proc loops over a subset of ens
               if ( remove_ens_mean ) then
                  xdata(iv,ie,ic)%value(:,:,:) = xdata(iv,ie,ic)%value(:,:,:) - ens_mean(:,:,:,iv)
               end if
            end do
         end do

         if ( write_ens_stdv ) then
            if ( myproc == root ) write(stdout,'(a)') ' ====== Computing ensemble stdv ======'
            allocate(ens_stdv(ni,nj,nk,nvar))
            do iv = 1, nvar
               if ( .not. do_this_var(iv) ) cycle
               ens_sum_loc(:,:,:) = 0.0 ! initialize for each variable
               ens_sum    (:,:,:) = 0.0 ! initialize for each variable
               do n = istart_ens(myproc),iend_ens(myproc)
                  ! local sum of (x-mean)**2
                  ens_sum_loc(:,:,:) = ens_sum_loc(:,:,:) + xdata(iv,n,ic)%value(:,:,:)*xdata(iv,n,ic)%value(:,:,:)
               end do
#ifdef DM_PARALLEL
               call mpi_allreduce(ens_sum_loc(:,:,:),ens_sum(:,:,:),ijk, &
                                  mpi_real8,mpi_sum,mpi_comm_world,ierr)
               if ( ierr /= 0 ) then
                  write(stdout, '(a, i3)') 'Error mpi_allreduce on proc', myproc
                  call mpi_abort(mpi_comm_world,1,ierr)
               end if
#else
               ens_sum(:,:,:) = ens_sum_loc(:,:,:)
#endif
               ens_stdv(:,:,:,iv) = sqrt(ens_sum(:,:,:)/real(nens))
            end do
            deallocate(ens_sum_loc)
            deallocate(ens_sum)
         end if

         if ( calc_regcoeff ) then
            ! write out psi
            do ie = ens_istart, ens_iend ! each proc loops over a subset of ens
               write(ce,'(i3.3)') ie
               output_file = 'psi.'//filedates(ie,ic)//'.e'//trim(ce)
               !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
               open (ounit, file = output_file, form='unformatted')
               write(ounit) 1, ni, nj, nk
               write(ounit) 3
               write(ounit) filedates(ie,ic), 'PSI       '
               write(ounit) xdata(indx_psi,ie,ic)%value(:,:,:)
               close(ounit)
            end do
         end if

         if ( write_pert1 .or. do_slen_calc ) then
            do ie = ens_istart, ens_iend ! each proc loops over a subset of ens
               write(ce,'(i3.3)') ie
               output_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
               write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
               open (ounit, file = output_file, form='unformatted')
               write(ounit) nvar, ni, nj, nk
               do iv = 1, nvar
                  write(ounit) var_dim(iv)
                  write(ounit) filedates(ie,ic), varnames(iv)
                  if ( var_dim(iv) == 2 ) then
                     write(ounit) xdata(iv,ie,ic)%value(:,:,1)
                  else if ( var_dim(iv) == 3 ) then
                     write(ounit) xdata(iv,ie,ic)%value(:,:,:)
                  end if
               end do
               close(ounit)
            end do
         end if

         if ( write_ep .and. (ep_format==1 .or. ep_format==11) ) then
            if ( ep_format==1 ) then
               allocate(r8tmp(ni,nj,nk))
            end if
            do iv = 1, nvar
               if ( .not. do_this_var(iv) ) cycle
               do ie = ens_istart, ens_iend ! each proc loops over a subset of ens
                  write(ce,'(i3.3)') ie
                  !output_file = trim(varnames(iv))//'.e'//trim(ce)
                  output_file = trim(outnames(iv))//'.e'//trim(ce)
                  open (ounit, file = output_file, form='unformatted')
                  if ( ep_format==1 ) then
                      r8tmp(:,:,:) = dble(xdata(iv,ie,ic)%value(:,:,:))
                  end if
                  if ( var_dim(iv) == 2 ) then
                     write(ounit) ni, nj, nk_2d
                     if ( ep_format==11 ) then
                        write(ounit) xdata(iv,ie,ic)%value(:,:,1)
                     else
                        write(ounit) r8tmp(:,:,1)
                     end if
                  else if ( var_dim(iv) == 3 ) then
                     write(ounit) ni, nj, nk
                     if ( ep_format==11 ) then
                        write(ounit) xdata(iv,ie,ic)%value(:,:,1:nk)
                     else
                        write(ounit) r8tmp(:,:,1:nk)
                     end if
                  end if
                  close(ounit)
               end do
            end do
            if ( ep_format==1 ) then
               deallocate(r8tmp)
            end if
         end if ! write_ep, ep_format=1, ep_format=11

         if ( write_ep .and. (ep_format==2 .or. ep_format==3) ) then

            if ( ep_format==3 ) then
               ! find out the grid decomposition info
               allocate(ips(0:nproc_out-1))
               allocate(ipe(0:nproc_out-1))
               allocate(jps(0:nproc_out-1))
               allocate(jpe(0:nproc_out-1))
               allocate(kps(0:nproc_out-1))
               allocate(kpe(0:nproc_out-1))
               ids = 1
               ide = ni + 1
               jds = 1
               jde = nj + 1
               kds = 1
               kde = nk + 1
               call wrfda_decompose_xy (nproc_out, ids, ide, jds, jde, kds, kde, &
                                        ips, ipe, jps, jpe, kps, kpe, ntasks_x, ntasks_y)
               if ( myproc == root ) then
                  write(stdout,'(a)') '------------------------------------------------------------------'
                  write(stdout,'(i4,a,i4,a,i4)')  &
                     nproc_out, ' processors decomposed to ntasks_x = ', ntasks_x, ' and ntasks_y = ', ntasks_y
                  write(stdout,'(a)') '------------------------------------------------------------------'
                  write(stdout,'(a)') '             ids   ide   jds   jde   kds   kde'
                  write(stdout,'(10x,6i6)') ids, ide, jds, jde, kds, kde
                  write(stdout,'(a)') '------------------------------------------------------------------'
                  write(stdout,'(a)') '             ips   ipe   jps   jpe   kps   kpe'
                  do i = 0, nproc_out-1
                     write(stdout,'(a,i4,a,6i6)')'proc ',i, ':', &
                        ips(i),ipe(i),jps(i),jpe(i),kps(i),kpe(i)
                  end do
               end if
            end if ! ep_format=3

            ! gather ep of one variable from all ensemble members and output all members to one file
            if ( myproc == root ) then
               allocate (globuf(ni, nj, nk, nens))
            end if
            allocate (locbuf(ni, nj, nk, nens))

#ifdef DM_PARALLEL
            if ( kind(locbuf) == 4 ) then
               this_mpi_real = mpi_real
            else
               this_mpi_real = mpi_real8
            end if
#endif

            my_nvar = 0
            var_loop3: do iv = 1, nvar
               if ( .not. do_this_var(iv) ) cycle var_loop3

               ! initialize to zero for each variable
               if ( myproc == root ) globuf(:,:,:,:) = 0.0
               locbuf(:,:,:,:) = 0.0

               do n = istart_ens(myproc),iend_ens(myproc)
                  locbuf(:,:,:,n) = xdata(iv,n,ic)%value(:,:,:)
               end do

#ifdef DM_PARALLEL
               call mpi_reduce(locbuf,globuf,ijk*nens, &
                               this_mpi_real,mpi_sum,root,mpi_comm_world,ierr)
               if ( ierr /= 0 ) then
                  write(stdout, '(a, i3)') 'Error mpi_reduce on proc', myproc
                  call mpi_abort(mpi_comm_world,1,ierr)
               end if
#else
               if ( myproc == root ) globuf(:,:,:,:) = locbuf(:,:,:,:)
#endif
               if ( myproc == root ) then
                  ! root processor writes out globuf inside the variable loop
                  write(stdout,'(a,a)') ' Outputing ensemble perturbations for ', trim(varnames(iv))

                  if ( ep_format == 2 ) then
                     output_file = 'ep.'//trim(outnames(iv))
                     open (ounit, file = trim(output_file), form='unformatted', status='replace')

                     write(ounit) outnames(iv)     !len=10
                     write(ounit) filedates(1,ic)  !len=19 0000-00-00_00:00:00
                     write(ounit) var_dim(iv)
                     if ( var_dim(iv) == 2 ) then
                        write(ounit) ni, nj, nk_2d, nens
                        do  n = 1, nens
                           write(ounit) globuf(:,:,1,n)
                        end do
                        write(ounit) ens_mean(:,:,1,iv)
                        if ( write_ens_stdv ) then
                           write(ounit) ens_stdv(:,:,1,iv)
                        end if
                     else if ( var_dim(iv) == 3 ) then
                        write(ounit) ni, nj, nk, nens
                        do  n = 1, nens
                           write(ounit) globuf(:,:,:,n)
                        end do
                        write(ounit) ens_mean(:,:,:,iv)
                        if ( write_ens_stdv ) then
                           write(ounit) ens_stdv(:,:,:,iv)
                        end if
                     end if
                     close(ounit)
                  else if ( ep_format == 3 ) then
                     do i = 0, nproc_out-1
                        write(pe_name,'(a2,i4.4)') 'pe', i
                        output_file = 'ep.'//trim(outnames(iv))//'.'//trim(pe_name)
                        open (ounit, file = trim(output_file), form='unformatted', status='replace')

                        ipe_out = min(ni,ipe(i))
                        jpe_out = min(nj,jpe(i))
                        kpe_out = min(nk,kpe(i))

                        write(ounit) outnames(iv)     !len=10
                        write(ounit) filedates(1,ic)  !len=19 0000-00-00_00:00:00
                        write(ounit) var_dim(iv)
                        if ( var_dim(iv) == 2 ) then
                           !write(ounit) ni, nj, nk_2d, nens
                           write(ounit) ips(i),ipe_out,jps(i),jpe_out,nk_2d,nk_2d,nens
                           do  n = 1, nens
                              write(ounit) globuf(ips(i):ipe_out,jps(i):jpe_out,1,n)
                           end do
                           write(ounit) ens_mean(ips(i):ipe_out,jps(i):jpe_out,1,iv)
                           if ( write_ens_stdv ) then
                              write(ounit) ens_stdv(ips(i):ipe_out,jps(i):jpe_out,1,iv)
                           end if
                        else if ( var_dim(iv) == 3 ) then
                           !write(ounit) ni, nj, nk, nens
                           write(ounit) ips(i),ipe_out,jps(i),jpe_out,kps(i),kpe_out,nens
                           do  n = 1, nens
                              write(ounit) globuf(ips(i):ipe_out,jps(i):jpe_out,kps(i):kpe_out,n)
                           end do
                           write(ounit) ens_mean(ips(i):ipe_out,jps(i):jpe_out,kps(i):kpe_out,iv)
                           if ( write_ens_stdv ) then
                              write(ounit) ens_stdv(ips(i):ipe_out,jps(i):jpe_out,kps(i):kpe_out,iv)
                           end if
                        end if
                        close(ounit)
                     end do
                  end if

               end if ! root

            end do var_loop3
            deallocate (locbuf)

            if ( allocated(globuf) ) deallocate (globuf)
            if ( ep_format==3 ) then
               deallocate(ips)
               deallocate(ipe)
               deallocate(jps)
               deallocate(jpe)
               deallocate(kps)
               deallocate(kpe)
            end if
         end if ! write_ep, ep_format=2 or 3

         deallocate(ens_mean)
         if ( write_ens_stdv ) deallocate(ens_stdv)

      end if ! be_method=ENS

   end do case_loop1 ! case loop

   if ( allocated(vor) )   deallocate (vor)
   if ( allocated(div) )   deallocate (div)
   if ( allocated(psi) )   deallocate (psi)
   if ( allocated(chi) )   deallocate (chi)
   if ( allocated(ustag) ) deallocate (ustag)
   if ( allocated(vstag) ) deallocate (vstag)
   if ( allocated(psfc) )  deallocate (psfc)
   if ( allocated(prs) )   deallocate (prs)
   if ( allocated(ght) )   deallocate (ght)
   if ( allocated(theta) ) deallocate (theta)
   if ( allocated(q) )     deallocate (q)
   if ( allocated(xfield) )   deallocate (xfield)
   if ( allocated(xfield_u) ) deallocate (xfield_u)
   if ( allocated(xfield_v) ) deallocate (xfield_v)
   if ( allocated(xfield_w) ) deallocate (xfield_w)

   if ( trim(be_method) == 'NMC' ) then
      if ( remove_time_mean ) then
         if ( .not. allocated(time_mean) ) allocate(time_mean(ni,nj,nk,nvar_all))
#ifdef DM_PARALLEL
         if ( .not. allocated(xsum) )      allocate(xsum(ni,nj,nk,nvar_all))
         do iv = 1, nvar_all
            call mpi_allreduce(xsum_loc(:,:,:,iv),xsum(:,:,:,iv), ijk, &
                               mpi_real8, mpi_sum, mpi_comm_world,ierr)
            time_mean(:,:,:,iv) = xsum(:,:,:,iv)/real(ncase)
         end do
         deallocate(xsum)
#else
         do iv = 1, nvar_all
            time_mean(:,:,:,iv) = xsum_loc(:,:,:,iv)/real(ncase)
         end do
#endif
         do ic = case_istart, case_iend
            do iv = 1, nvar_all
               xdata(iv,1,ic)%value(:,:,:) = xdata(iv,1,ic)%value(:,:,:) - time_mean(:,:,:,iv)
            end do
         end do
         deallocate(time_mean)
         deallocate(xsum_loc)
      end if ! remove_time_mean

      if ( calc_regcoeff ) then
         ! write out psi
         do ic = case_istart, case_iend
            output_file = 'psi.'//filedates(1,ic)//'.e001'
            !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
            open (ounit, file = output_file, form='unformatted')
            write(ounit) 1, ni, nj, nk
            write(ounit) 3
            write(ounit) filedates(1,ic), 'PSI       '
            write(ounit) xdata(indx_psi,1,ic)%value(:,:,:)
            close(ounit)
         end do
      end if

      if ( write_pert1 .or. do_slen_calc ) then
         do ic = case_istart, case_iend
            output_file = 'pert1.'//filedates(1,ic)//'.e001'
            !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
            open (ounit, file = output_file, form='unformatted')
            write(ounit) nvar, ni, nj, nk
            do iv = 1, nvar
               write(ounit) var_dim(iv)
               write(ounit) filedates(1,ic), varnames(iv)
               if ( var_dim(iv) == 2 ) then
                  write(ounit) xdata(iv,1,ic)%value(:,:,1)
               else if ( var_dim(iv) == 3 ) then
                  write(ounit) xdata(iv,1,ic)%value(:,:,1:nk)
               end if
            end do
            close(ounit)
         end do
      end if
   end if ! be_method=NMC

   do ic = case_istart, case_iend
      do ie = ens_istart, ens_iend
         do iv = 1, nvar_all
            if ( allocated(xdata(iv,ie,ic)%value) ) then
               deallocate(xdata(iv,ie,ic)%value)
            end if
         end do
      end do
   end do
   if ( allocated(xdata)) deallocate(xdata)

end subroutine compute_pert

subroutine compute_regcoeff_unbalanced
! adapted from var/gen_be/gen_be_stage2.f90 and var/gen_be/gen_be_stage2a.f90

   implicit none

   real, parameter     :: variance_threshold = 1e-6  ! Percentage of <psi psi> variance discarded.

   character(len=filename_len) :: input_file, output_file
   character*3         :: ce                         ! Member index -> character
   integer             :: i, j, k, member, k2, k3, m ! Loop counters
   integer             :: b                          ! Bin marker
   integer             :: mmax                       ! Maximum mode (after variance truncation)
   real                :: coeffa, coeffb             ! Accumulating mean coefficients
   real                :: total_variance             ! Total variance of <psi psi> matrix
   real                :: cumul_variance             ! Cumulative variance of <psi psi> matrix
   real                :: summ                       ! Summation dummy

   real, allocatable   :: psi(:,:,:)                 ! psi
   real, allocatable   :: chi(:,:,:)                 ! chi
   real, allocatable   :: temp(:,:,:)                ! Temperature
   real, allocatable   :: ps(:,:,:)                  ! Surface pressure

   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields)
   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields)
   real, allocatable   :: covar1(:)                  ! Covariance between input fields
   real, allocatable   :: covar2(:,:)                ! Covariance between input fields
   real, allocatable   :: covar3(:,:,:)              ! Covariance between input fields
   real, allocatable   :: var1(:)                    ! Autocovariance of field
   real, allocatable   :: var2(:,:,:)                ! Autocovariance of field
   real, allocatable   :: var2_inv(:,:,:)            ! Inverse Autocovariance of field

   real, allocatable   :: work(:,:)                  ! EOF work array
   real*8, allocatable :: evec(:,:)                  ! Gridpoint eigenvectors
   real*8, allocatable :: eval(:)                    ! Gridpoint sqrt(eigenvalues)
   real, allocatable   :: LamInvET(:,:)              ! ET/sqrt(Eigenvalue)
   real, allocatable   :: regcoeff1(:)               ! psi/chi regression cooefficient
   real, allocatable   :: regcoeff2(:,:)             ! psi/ps regression cooefficient
   real, allocatable   :: regcoeff3(:,:,:)           ! psi/T regression cooefficient

   integer :: ic, ie, iv
   integer :: istart_member4cov, iend_member4cov  ! index for covariance loop
   integer :: istart_member4bal, iend_member4bal  ! index for unbalanced loop
   integer :: ounit
   integer :: this_mpi_real
   integer :: proc_send

   logical :: got_var2_inv

   if ( myproc == root ) then
      write(stdout,'(a)')' ====== Computing regcoeff and unbalanced fields ======'
   end if

   ounit = 24

   allocate( psi(1:ni,1:nj,1:nk) )
   allocate( bin_pts(1:num_bins) )
   allocate( bin_pts2d(1:num_bins2d) )

   if ( trim(be_method) == 'NMC' ) then
      istart_member4cov = 1
      iend_member4cov   = 1
      istart_member4bal = 1
      iend_member4bal   = 1
   else if ( trim(be_method) == 'ENS' ) then
      istart_member4cov = 1
      iend_member4cov   = nens
      istart_member4bal = istart_ens(myproc)
      iend_member4bal   = iend_ens(myproc)
   end if

   allocate( regcoeff1(1:num_bins) )
   allocate( regcoeff2(1:nk,1:num_bins2d) )
   allocate( regcoeff3(1:nk,1:nk,1:num_bins2d) )
   regcoeff1 = 0.0
   regcoeff2 = 0.0
   regcoeff3 = 0.0

#ifdef DM_PARALLEL
   if ( kind(regcoeff1) == 4 ) then
      this_mpi_real = mpi_real
   else
      this_mpi_real = mpi_real8
   end if
#endif

   ! nvar distributed among processors
   var_loop_regcoef: do iv = 1, nvar

      if ( .not. do_this_var(iv) ) cycle var_loop_regcoef
      if ( myproc /= MOD((iv-1), num_procs) ) cycle var_loop_regcoef
      if ( trim(varnames(iv)) /= 'CHI_U'  .and. &
           trim(varnames(iv)) /= 'PSFC_U' .and. &
           trim(varnames(iv)) /= 'T_U'         ) cycle var_loop_regcoef

      !write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Computing regcoeff for variable ', trim(varnames(iv))

      bin_pts(:) = 0
      bin_pts2d(:) = 0

      if ( trim(varnames(iv)) == 'CHI_U' ) then
         if ( .not. allocated(chi) )    allocate( chi(1:ni,1:nj,1:nk) )
         if ( .not. allocated(covar1) ) allocate( covar1(1:num_bins) )
         if ( .not. allocated(var1) )   allocate( var1(1:num_bins) )
         chi(:,:,:) = 0.0
         covar1(:) = 0.0
         var1(:) = 0.0
      end if

      if ( trim(varnames(iv)) == 'PSFC_U' .or. trim(varnames(iv)) == 'T_U' ) then
         if ( .not. allocated(var2) ) allocate( var2(1:nk,1:nk,1:num_bins2d) )
         var2(:,:,:) = 0.0
         if ( trim(varnames(iv)) == 'PSFC_U' ) then
            ! still allocate 1:nk for ps even though only 1 will be used
            ! this is needed to make get_pert1_data work for ps
            if ( .not. allocated(ps) )     allocate( ps(1:ni,1:nj,1:nk) )
            if ( .not. allocated(covar2) ) allocate( covar2(1:nk,1:num_bins2d) )
            ps(:,:,:) = 0.0
            covar2(:,:) = 0.0
         end if
         if ( trim(varnames(iv)) == 'T_U' ) then
            if ( .not. allocated(temp) )   allocate( temp(1:ni,1:nj,1:nk) )
            if ( .not. allocated(covar3) ) allocate( covar3(1:nk,1:nk,1:num_bins2d) )
            temp(:,:,:) = 0.0
            covar3(:,:,:) = 0.0
         end if
      end if

      write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Computing psi covariances for variable ', trim(varnames(iv))

      do ic = 1, ncase
         do ie = istart_member4cov, iend_member4cov
            !write(stdout,'(5a,i4)')'    Processing data for date ', filedates(ie,ic), ', variable ', trim(varnames(iv)), &
            !                  ', member ', ie

            ! psi is needed for all variables
            write(ce,'(i3.3)') ie
            input_file = 'psi.'//filedates(ie,ic)//'.e'//trim(ce)
            call get_pert1_data(trim(input_file), 'PSI', ni, nj, nk, psi)

            input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
            if ( trim(varnames(iv)) == 'CHI_U' ) then
               call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, chi)
               ! Calculate psi/chi covariances:
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        b = bin(i,j,k)
                        bin_pts(b) = bin_pts(b) + 1
                        coeffa = 1.0 / real(bin_pts(b))
                        coeffb = real(bin_pts(b)-1) * coeffa
                        covar1(b) = coeffb * covar1(b) + coeffa * psi(i,j,k) * chi(i,j,k)
                        var1(b) = coeffb * var1(b) + coeffa * psi(i,j,k) * psi(i,j,k)
                     end do
                  end do
               end do
            end if ! chi

            if ( trim(varnames(iv)) == 'PSFC_U' .or. trim(varnames(iv)) == 'T_U' ) then

               if ( trim(varnames(iv)) == 'PSFC_U' ) then
                  call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, ps)
               end if ! ps
               if ( trim(varnames(iv)) == 'T_U' ) then
                  call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, temp)
               end if ! t

               ! Calculate psi/ps, and psi/T, and psi/psi covariances:
               do j = 1, nj
                  do i = 1, ni
                     b = bin2d(i,j)
                     bin_pts2d(b) = bin_pts2d(b) + 1
                     coeffa = 1.0 / real(bin_pts2d(b))
                     coeffb = real(bin_pts2d(b)-1) * coeffa
                     do k = 1, nk

                        if ( trim(varnames(iv)) == 'PSFC_U' ) then
                           ! psi/ps:
                           covar2(k,b) = coeffb * covar2(k,b) + coeffa * ps(i,j,1) * psi(i,j,k)
                        end if

                        if ( trim(varnames(iv)) == 'T_U' ) then
                           ! psi/T:
                           do k2 = 1, nk
                              covar3(k,k2,b) = coeffb * covar3(k,k2,b) + &
                                               coeffa * temp(i,j,k) * psi(i,j,k2)
                           end do
                        end if

                        ! psi/psi (symmetric):
                        do k2 = 1, k
                           var2(k,k2,b) = coeffb * var2(k,k2,b) + &
                                          coeffa * psi(i,j,k) * psi(i,j,k2)
                        end do

                     end do ! k loop
                  end do ! i loop
               end do ! j loop

            end if ! ps_u or t_u

         end do ! ens member loop

      end do ! ncase loop

      got_var2_inv = .false. ! initialize

      if ( trim(varnames(iv)) == 'PSFC_U' .or. trim(varnames(iv)) == 'T_U' ) then

         ! Fill in psi/psi covariance by symmetry:
         do b = 1, num_bins2d
            do k = 1, nk
               do k2 = k+1, nk ! Symmetry.
                  var2(k,k2,b) = var2(k2,k,b)
               end do
            end do
         end do

         if ( .not. got_var2_inv ) then

            write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Computing eigenvectors, eigenvalues and inverse for psi/psi covariance'

            if ( .not. allocated(work) )     allocate( work(1:nk,1:nk) )
            if ( .not. allocated(evec) )     allocate( evec(1:nk,1:nk) )
            if ( .not. allocated(eval) )     allocate( eval(1:nk) )
            if ( .not. allocated(LamInvET) ) allocate( LamInvET(1:nk,1:nk) )
            if ( .not. allocated(var2_inv) ) allocate( var2_inv(1:nk,1:nk,1:num_bins2d) )

            do b = 1, num_bins2d
               LamInvET(:,:) = 0.0
               work(1:nk,1:nk) = var2(1:nk,1:nk,b)
               call da_eof_decomposition( nk, work, evec, eval )

               ! Truncate eigenvalues to ensure inverse is not dominated by rounding error:
               summ = 0.0
               do m = 1, nk
                  summ = summ + eval(m)
               end do
               total_variance = summ

               cumul_variance = 0.0
               mmax = nk
               do m = 1, nk
                  cumul_variance = cumul_variance + eval(m) / total_variance
                  if ( cumul_variance > 1.0 - variance_threshold ) then
                     mmax = m - 1
                     exit
                  end if
               end do
               !write(6,'(2(a,i6),2(a,1pe11.5))') ' Bin = ', b, ', <psipsi> truncation = ', mmax, &
               !                                  ', Total Variance = ', total_variance, &
               !                                  ', Condition number = ', eval(1) / eval(nk-1)

               ! Lam{-1} . E^T:
               do k = 1, nk
                  do m = 1, mmax
                     LamInvET(m,k) = evec(k,m) / eval(m)
                  end do
               end do

               ! <psi psi>^{-1} = E . Lam{-1} . E^T:
               do k = 1, nk
                  do k2 = 1, k
                     summ = 0.0
                     do m = 1, nk
                        summ = summ + evec(k,m) * LamInvET(m,k2)
                     end do
                     var2_inv(k,k2,b) = summ
                  end do
               end do

               do k = 1, nk
                  do k2 = k+1, nk ! Symmetry.
                     var2_inv(k,k2,b) = var2_inv(k2,k,b)
                  end do
               end do
            end do
            got_var2_inv = .true.
         end if ! got_var2_inv
      end if ! ps_u or t_u

      write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Computing regression coefficients for variable ', trim(varnames(iv))

      if ( trim(varnames(iv)) == 'CHI_U' ) then
         ! psi/chi:
         do b = 1, num_bins
            regcoeff1(b) = covar1(b) / var1(b)
         end do
         ! Output regression coefficients
         output_file = 'regcoeff1.dat'
         open (ounit, file=trim(output_file), form='unformatted', status='unknown')
         write(ounit)ni, nj, nk
         write(ounit)num_bins, num_bins2d
         write(ounit)regcoeff1
         close(ounit)
      end if ! chi_u

      if ( trim(varnames(iv)) == 'PSFC_U' ) then
         ! psi/ps:
         do b = 1, num_bins2d
            do k = 1, nk
               summ = 0.0
               do k2 = 1, nk
                  summ = summ + covar2(k2,b) * var2_inv(k2,k,b)
               end do
               regcoeff2(k,b) = summ
            end do
         end do
         ! Output regression coefficients
         output_file = 'regcoeff2.dat'
         open (ounit, file=trim(output_file), form='unformatted', status='unknown')
         write(ounit)ni, nj, nk
         write(ounit)num_bins, num_bins2d
         write(ounit)regcoeff2
         close(ounit)
      end if ! ps_u

      if ( trim(varnames(iv)) == 'T_U' ) then
         ! psi/T:
         do b = 1, num_bins2d
            do k = 1, nk
               do k2 = 1, nk
                  summ = 0.0
                  do k3 = 1, nk
                     summ = summ + covar3(k,k3,b) * var2_inv(k3,k2,b)
                  end do
                  regcoeff3(k,k2,b) = summ
               end do
            end do
         end do
         ! Output regression coefficients
         output_file = 'regcoeff3.dat'
         open (ounit, file=trim(output_file), form='unformatted', status='unknown')
         write(ounit)ni, nj, nk
         write(ounit)num_bins, num_bins2d
         write(ounit)regcoeff3
         close(ounit)
      end if ! t_u

      if ( trim(varnames(iv)) == 'PSFC_U' .or. trim(varnames(iv)) == 'T_U' ) then
         if ( allocated(var2_inv) ) deallocate( var2_inv )
         if ( allocated(LamInvET) ) deallocate( LamInvET )
         if ( allocated(eval) )     deallocate( eval )
         if ( allocated(evec) )     deallocate( evec )
         if ( allocated(work) )     deallocate( work )
         if ( allocated(var2) )     deallocate( var2 )
      end if

      if ( trim(varnames(iv)) == 'CHI_U' ) then
         deallocate( covar1 )
         deallocate( var1 )
      end if
      if ( trim(varnames(iv)) == 'PSFC_U' ) then
         deallocate( covar2 )
      end if
      if ( trim(varnames(iv)) == 'T_U' ) then
         deallocate( covar3 )
      end if

   end do var_loop_regcoef

#ifdef DM_PARALLEL
      do iv = 1, nvar
         if ( .not. do_this_var(iv) ) cycle
         proc_send = MOD((iv-1), num_procs)
         if ( trim(varnames(iv)) == 'CHI_U' ) then
            call mpi_bcast(regcoeff1(:), num_bins, this_mpi_real, proc_send, mpi_comm_world, ierr )
         end if
         if ( trim(varnames(iv)) == 'PSFC_U' ) then
            call mpi_bcast(regcoeff2(:,:), nk*num_bins2d, this_mpi_real, proc_send, mpi_comm_world, ierr )
         end if
         if ( trim(varnames(iv)) == 'T_U' ) then
            call mpi_bcast(regcoeff3(:,:,:), nk*nk*num_bins2d, this_mpi_real, proc_send, mpi_comm_world, ierr )
         end if
      end do
#endif

   var_loop_unbal: do iv = 1, nvar

      if ( .not. do_this_var(iv) ) cycle var_loop_unbal
      if ( trim(varnames(iv)) /= 'CHI_U'  .and. &
           trim(varnames(iv)) /= 'PSFC_U' .and. &
           trim(varnames(iv)) /= 'T_U'         ) cycle var_loop_unbal

      if ( myproc == root ) write(stdout,'(2a)') ' Computing unbalanced field for variable ', trim(varnames(iv))

      if ( trim(varnames(iv)) == 'CHI_U' ) then
         if ( .not. allocated(chi) )   allocate( chi(1:ni,1:nj,1:nk) )
         chi(:,:,:) = 0.0
      end if
      if ( trim(varnames(iv)) == 'PSFC_U' ) then
         if ( .not. allocated(ps) )    allocate( ps(1:ni,1:nj,1:nk) )
         ps(:,:,:) = 0.0
      end if
      if ( trim(varnames(iv)) == 'T_U' ) then
         if ( .not. allocated(temp) )  allocate( temp(1:ni,1:nj,1:nk) )
         temp(:,:,:) = 0.0
      end if

      ! ncase/nens distributed among processors
      do ic = case_istart, case_iend
         do ie = istart_member4bal, iend_member4bal

            !write(stdout,'(a,i3,4a)') ' Proc', myproc, ' Computing unbalanced field for variable ', trim(varnames(iv)), &
            !                          ' date ', filedates(ie,ic)

            ! psi is needed for all variables
            write(ce,'(i3.3)') ie
            input_file = 'psi.'//filedates(ie,ic)//'.e'//trim(ce)
            call get_pert1_data(trim(input_file), 'PSI', ni, nj, nk, psi)

            input_file  = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
            output_file = 'pert1_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)

            if ( trim(varnames(iv)) == 'CHI_U' ) then
               call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, chi)
               do k = 1, nk
                  do j = 1, nj
                     do i = 1, ni
                        b = bin(i,j,k)
                        chi(i,j,k) = chi(i,j,k) - regcoeff1(b) * psi(i,j,k)
                     end do
                  end do
               end do
               open (ounit, file=output_file, form='unformatted')
               write(ounit) 1, ni, nj, nk
               write(ounit) 3
               write(ounit) filedates(ie,ic), varnames(iv)
               write(ounit) chi(:,:,:)
               close(ounit)
            end if ! chi_u

            if ( trim(varnames(iv)) == 'PSFC_U' ) then
               call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, ps)
               do j = 1, nj
                  do i = 1, ni
                     b = bin2d(i,j)
                     ps(i,j,1) = ps(i,j,1) - SUM(regcoeff2(1:nk,b) * psi(i,j,1:nk))
                  end do
               end do
               open (ounit, file=output_file, form='unformatted')
               write(ounit) 1, ni, nj, nk
               write(ounit) 2
               write(ounit) filedates(ie,ic), varnames(iv)
               write(ounit) ps(:,:,1)
               close(ounit)
            end if ! ps_u

            if ( trim(varnames(iv)) == 'T_U' ) then
               call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, temp)
               do j = 1, nj
                  do i = 1, ni
                     b = bin2d(i,j)
                     do k = 1, nk
                        temp(i,j,k) = temp(i,j,k) - SUM(regcoeff3(k,1:nk,b) * psi(i,j,1:nk))
                     end do
                  end do
               end do
               open (ounit, file=output_file, form='unformatted')
               write(ounit) 1, ni, nj, nk
               write(ounit) 3
               write(ounit) filedates(ie,ic), varnames(iv)
               write(ounit) temp(:,:,:)
               close(ounit)
            end if ! t_u

         end do ! ens member loop
      end do ! ncase loop

#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif

      if ( trim(varnames(iv)) == 'CHI_U' ) then
         if ( allocated(chi) ) deallocate( chi )
      end if
      if ( trim(varnames(iv)) == 'PSFC_U' ) then
         if ( allocated(ps) ) deallocate( ps )
      end if
      if ( trim(varnames(iv)) == 'T_U' ) then
         if ( allocated(temp) ) deallocate( temp )
      end if

   end do var_loop_unbal

   deallocate( regcoeff1 )
   deallocate( regcoeff2 )
   deallocate( regcoeff3 )
   deallocate( psi )
   deallocate( bin_pts )
   deallocate( bin_pts2d )

end subroutine compute_regcoeff_unbalanced

subroutine compute_bv_sl

   implicit none

   character(len=filename_len)  :: filename                   ! Input filename.
   character(len=filename_len)  :: output_file
   integer             :: i, j, k, k1, k2, b         ! Loop counters.
   integer             :: ic, ie, iv, m
   integer :: istart_member, iend_member             ! loop index for nens
   integer :: istart_member_p, iend_member_p         ! loop index for projecting to modes
   real                :: inv_nij                    ! 1 / (ni*nj).
   real                :: mean_field                 ! Mean field.
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.

   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: field(:,:,:)               ! Input field.
   real, allocatable   :: fieldv(:,:,:)              ! projected field
   real, allocatable   :: field2d(:,:)               ! Input field.
   real, allocatable   :: bv(:,:,:)                  ! Vertical BE for this time.
   real, allocatable   :: work(:,:)                  ! EOF work array.
   real, allocatable   :: e_vec_loc(:,:,:)           ! Latitudinally varying eigenvectors.
   real, allocatable   :: e_val_loc(:,:)             ! Latitudinally varying eigenvalues.
   real*8, allocatable :: e_vec(:,:)                 ! Domain-averaged eigenvectors.
   real*8, allocatable :: e_val(:)                   ! Domain-averaged eigenvalues.
   real*8, allocatable :: evec(:,:,:)                ! Gridpoint eigenvectors.
   real*8, allocatable :: eval(:,:)                  ! Gridpoint sqrt(eigenvalues).
   real :: ETVp
   character(len=DateStrLen)       :: DateStr_tmp
   character(len=VarNameLen)       :: var_tmp
   character(len=filename_len)     :: input_file
   character(len=3)     :: ce

   integer :: ndim
   integer :: ijk
   integer :: proc_send
   integer :: nn
   integer :: nvar_read, ni_read, nj_read, nk_read, nkk
   integer :: k_start, k_end
   integer :: this_mpi_real
   integer, allocatable:: nr(:), icount(:)
   real, allocatable   :: cov(:,:)
   real, allocatable   :: ml(:), sl(:)
   real, allocatable   :: sl_g(:)
   real, allocatable   :: hl(:), hlt(:)

   integer :: bv_unit = 22
   integer :: iunit = 23
   integer :: ounit = 24
   testing_eofs = .false.
   use_global_eofs = .true.

   inv_nij = 1.0 / real(ni*nj)
   allocate( field(1:ni,1:nj,1:nk) )

#ifdef DM_PARALLEL
   if ( kind(field) == 4 ) then
      this_mpi_real = mpi_real
   else
      this_mpi_real = mpi_real8
   end if
#endif

   if ( trim(be_method) == 'NMC' ) then
      istart_member   = 1
      iend_member     = 1
      istart_member_p = 1
      iend_member_p   = 1
   else if ( trim(be_method) == 'ENS' ) then
      istart_member   = 1
      iend_member     = nens
      istart_member_p = istart_ens(myproc)
      iend_member_p   = iend_ens(myproc)
   end if

   if ( pert1_read_opt == 1 ) then

   allocate(xdata(1:nvar,istart_member:iend_member,1:ncase), stat=ierr)
   call error_handler(ierr, 'allocating xdata in compute_bv_sl')

   do ic = 1, ncase
      do ie = istart_member, iend_member
         write(ce,'(i3.3)') ie
         input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
         inquire(file=trim(input_file), exist=isfile)
         if ( .not. isfile ) then
            call error_handler(-1, trim(input_file)//' not found for getting perturbation')
         end if
         !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' Reading from ', trim(input_file)
         open (iunit, file=trim(input_file), form='unformatted', status='old')
         read(iunit) nvar_read, ni_read, nj_read, nk_read
         read_loop: do iv = 1, nvar_read
            read(iunit) ndim
            read(iunit) DateStr_tmp, var_tmp
            if ( ndim == 2 ) then
               read(iunit) field(:,:,1)
            else if ( ndim == 3 ) then
               read(iunit) field(:,:,:)
            end if
            allocate(xdata(iv,ie,ic)%value(ni,nj,nk), stat=ierr)
            call error_handler(ierr, 'allocating xdata%value in compute_bv_sl')
            xdata(iv,ie,ic)%value(:,:,:) = field(:,:,:)
         end do read_loop
         close(iunit)
      end do
   end do

   end if ! pert1_read_opt=1, store all pert1 data in memory

   if ( do_eof_transform ) then

      if ( myproc == root ) write(stdout,'(4a)')' ====== Computing vertical error eigenvalues, eigenvectors ======'

      allocate( bv(1:nk,1:nk,1:num_bins2d) )
      allocate( bin_pts2d(1:num_bins2d) )
      allocate( work(1:nk,1:nk) )
      allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
      allocate( e_val_loc(1:nk,1:num_bins2d) )
      allocate( e_vec(1:nk,1:nk) )
      allocate( e_val(1:nk) )
      allocate( evec(1:nj,1:nk,1:nk) )
      allocate( eval(1:nj,1:nk) )

      if ( pert1_read_opt == 2 ) then
         allocate( fieldv(1:ni,1:nj,1:nk) )
      end if

      ! nvar distributed among processors
      var_loop_bv: do iv = 1, nvar

         if ( .not. do_this_var(iv) ) cycle var_loop_bv
         if ( myproc /= MOD((iv-1), num_procs) ) cycle var_loop_bv

         write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Processing vertical error stats for variable ', trim(varnames(iv))

         bv(:,:,:) = 0.0
         bin_pts2d(:) = 0

         if ( var_dim(iv) == 2 ) then
            nkk = 1
         else if ( var_dim(iv) == 3 ) then
            nkk = nk
         end if

         do ic = 1, ncase
            do ie = istart_member, iend_member
               !write(stdout,'(5a,i4)')'    Processing data for date ', filedates(ie,ic), ', variable ', trim(varnames(iv)), &
               !                  ', member ', ie

               if ( pert1_read_opt == 1 ) then

                  field(:,:,:) = xdata(iv,ie,ic)%value(:,:,:)

               else if ( pert1_read_opt == 2 ) then

                  write(ce,'(i3.3)') ie
                  if ( trim(varnames(iv)) == 'CHI_U' .or. &
                       trim(varnames(iv)) == 'T_U'   .or. &
                       trim(varnames(iv)) == 'PSFC_U'    ) then
                     input_file = 'pert1_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                  else
                     input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
                  end if

                  call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, field)

               end if

               ! Remove area mean
               do k = 1, nkk
                  mean_field = sum(field(1:ni,1:nj,k)) * inv_nij
                  field(1:ni,1:nj,k) = field(1:ni,1:nj,k) - mean_field
               end do

               do j = 1, nj
                  do i = 1, ni
                     b = bin2d(i,j)
                     bin_pts2d(b) = bin_pts2d(b) + 1
                     coeffa = 1.0 / real(bin_pts2d(b))
                     coeffb = real(bin_pts2d(b)-1) * coeffa
                     do k1 = 1, nkk
                        do k2 = 1, k1
                           bv(k1,k2,b) = coeffb * bv(k1,k2,b) + coeffa * field(i,j,k1) * field(i,j,k2)
                        end do
                     end do
                  end do
               end do
            end do  ! End loop over ensemble members.
         end do  ! End loop over times.

         !  Fill in upper-right part of BE matrix by symmetry:

         do b = 1, num_bins2d
            do k1 = 1, nkk
               do k2 = k1+1, nkk ! Symmetry.
                  if ( vertloc_opt > 0 ) then
                     bv(k2,k1,b) = bv(k2,k1,b) * vertloc_rho(k2,k1)
                  end if
                  !if ( k1 > 40 ) then ! arbitrarily remove negative values near top
                  !   bv(k2,k1,b) = max(0.0, bv(k2,k1,b))
                  !end if
                  bv(k1,k2,b) = bv(k2,k1,b)
               end do
            end do
         end do

         !write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Calculating eigenvectors and eigenvalues for variable ', trim(varnames(iv))

         ! BE decomposition:
         do b = 1, num_bins2d
            !write(stdout,'(2(a,i6))')' Calculate eigenvectors and eigenvalues for bin ', b, &
            !                    ' of ', num_bins2d

            work(1:nkk,1:nkk) = bv(1:nkk,1:nkk,b)
            call da_eof_decomposition( nkk, work, e_vec, e_val )
            e_vec_loc(1:nkk,1:nkk,b) = e_vec(1:nkk,1:nkk)
            e_val_loc(1:nkk,b) = e_val(1:nkk)
         end do

         !  Domain-averaged BE decomposition:
         work(1:nkk,1:nkk) = 0.0
         do b = 1, num_bins2d
            work(1:nkk,1:nkk) = work(1:nkk,1:nkk) + bv(1:nkk,1:nkk,b)
         end do
         work(1:nkk,1:nkk) = work(1:nkk,1:nkk) / real( num_bins2d )

         output_file = 'Bv_'//trim(varnames(iv))//'.txt'
         open(bv_unit, file=trim(output_file), form='formatted', status='unknown')
         do k = 1, nkk
            write(bv_unit,*) varnames(iv), k, work(k,k)
         end do
         close(bv_unit)

         call da_eof_decomposition( nkk, work, e_vec, e_val )

         if ( testing_eofs ) then
            call da_eof_decomposition_test( nkk, work, e_vec, e_val )
         end if

         be_data(iv)%e_vec(1:nkk,1:nkk)                  = e_vec(1:nkk,1:nkk)
         be_data(iv)%e_val(1:nkk)                        = e_val(1:nkk)
         be_data(iv)%e_vec_loc(1:nkk,1:nkk,1:num_bins2d) = e_vec_loc(1:nkk,1:nkk,1:num_bins2d)
         be_data(iv)%e_val_loc(1:nkk,1:num_bins2d)       = e_val_loc(1:nkk,1:num_bins2d)

         if ( write_stage3 ) then
            !  Output eigenvectors, eigenvalues
            filename = 'gen_be_stage3.'//trim(varnames(iv))//'.dat'
            open (ounit, file = filename, form='unformatted')
            write(ounit)varnames(iv)
            write(ounit)nkk, num_bins2d
            write(ounit)e_vec(1:nkk,1:nkk)
            write(ounit)e_val(1:nkk)
            write(ounit)e_vec_loc(1:nkk,1:nkk,1:num_bins2d)
            write(ounit)e_val_loc(1:nkk,1:num_bins2d)
            close(ounit)
         end if

         !  Decide on local or domain-averaged EOFs for horizontal correlations:
         if ( use_global_eofs ) then
            do b = 1, num_bins2d
               e_vec_loc(1:nkk,1:nkk,b) = e_vec(1:nkk,1:nkk)
               e_val_loc(1:nkk,b) = e_val(1:nkk)
            end do
         end if

         !  Map binned eigenvectors to x, y grid, and take sqrt(this is used in WRF-Var):
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               evec(j,1:nkk,1:nkk) = e_vec_loc(1:nkk,1:nkk,b)
               eval(j,1:nkk) = sqrt(e_val_loc(1:nkk,b))
            end do
         end do

         !  Output eigenvectors, eigenvalues
         filename = 'EV_'//trim(varnames(iv))//'.dat'
         open (ounit, file = filename, form='unformatted')
         write(ounit)varnames(iv)
         write(ounit)ni, nj, nkk
         write(ounit)evec(1:nj,1:nkk,1:nkk)
         write(ounit)eval(1:nj,1:nkk)
         close(ounit)
      end do var_loop_bv

#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif

      var_loop_proj: do iv = 1, nvar

         if ( .not. do_this_var(iv) ) cycle var_loop_proj

         if ( var_dim(iv) == 3 ) then

            ! raed eigenvectors, eigenvalues
            filename = 'EV_'//trim(varnames(iv))//'.dat'
            open (iunit, file = filename, form='unformatted')
            read(iunit) !varnames(iv)
            read(iunit) !ni, nj, nkk
            read(iunit) evec(1:nj,1:nk,1:nk)
            read(iunit) eval(1:nj,1:nk)
            close(iunit)

            if ( myproc == root ) write(stdout,'(a)')' Projecting fields onto vertical modes'

            ! ncase/nens distributed among processors
            do ic = case_istart, case_iend
               do ie = istart_member_p, iend_member_p
                  !write(stdout,'(5a,i4)')'    Date = ', filedates(ie,ic), ', variable ', trim(varnames(iv)), &
                  !                  ', member ', ie
                  ! Project fields onto vertical modes
                  ! Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform
                if ( pert1_read_opt == 1 ) then
                  do m = 1, nk
                     do j = 1, nj
                        do i = 1, ni
                           ETVp = sum(evec(j,1:nk,m) * xdata(iv,ie,ic)%value(i,j,1:nk))
                           field(i,j,m) = ETVp / eval(j,m)
                        end do
                     end do
                  end do
                  xdata(iv,ie,ic)%value(:,:,:) = field(:,:,:)
                else if ( pert1_read_opt == 2 ) then
                  write(ce,'(i3.3)') ie
                  if ( trim(varnames(iv)) == 'CHI_U' .or. &
                       trim(varnames(iv)) == 'T_U'   .or. &
                       trim(varnames(iv)) == 'PSFC_U'    ) then
                     input_file = 'pert1_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                  else
                     input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
                  end if
                  call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, field)
                  do m = 1, nk
                     do j = 1, nj
                        do i = 1, ni
                           ETVp = sum(evec(j,1:nk,m) * field(i,j,1:nk))
                           fieldv(i,j,m) = ETVp / eval(j,m)
                        end do
                     end do
                  end do
                  output_file = 'pertv_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                  !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' writing to ', trim(output_file)
                  open (ounit, file=output_file, form='unformatted')
                  write(ounit) 1, ni, nj, nk
                  write(ounit) var_dim(iv)
                  write(ounit) filedates(ie,ic), varnames(iv)
                  write(ounit) fieldv(:,:,:)
                  close(ounit)
                end if ! pert1_read_opt
               end do ! End loop over members.
            end do ! case loop
         end if ! var_dim=3
      end do var_loop_proj

#ifdef DM_PARALLEL
      call mpi_barrier(mpi_comm_world,ierr)
#endif

      if ( pert1_read_opt == 1 ) then
#ifdef DM_PARALLEL
      ijk = ni*nj*nk
      do iv = 1, nvar
         if ( .not. do_this_var(iv) ) cycle
         proc_send = MOD((iv-1), num_procs)
         do ic = 1, ncase
            do ie = istart_member, iend_member
               if ( myproc == MOD((iv-1), num_procs) ) then
                  field(:,:,:) = xdata(iv,ie,ic)%value(:,:,:)
               end if
               call mpi_bcast(field(:,:,:), ijk, this_mpi_real, proc_send, mpi_comm_world, ierr )
               if ( myproc /= MOD((iv-1), num_procs) ) then
                  xdata(iv,ie,ic)%value(:,:,:) = field(:,:,:)
               end if
            end do
         end do
      end do
#endif
      end if ! pert1_read_opt=1

      if ( pert1_read_opt == 2 ) then
         deallocate( fieldv )
#ifdef DM_PARALLEL
         call mpi_barrier(mpi_comm_world,ierr)
#endif
      end if
      deallocate( eval )
      deallocate( evec )
      deallocate( e_val )
      deallocate( e_vec )
      deallocate( e_val_loc )
      deallocate( e_vec_loc )
      deallocate( work )
      deallocate( bin_pts2d )
      deallocate( bv )

   end if ! do_eof_transform

   if ( do_slen_calc ) then
      if ( myproc == root ) write(stdout,'(4a)')' ====== Computing horizontal length scales ======'
      allocate(field2d(ni,nj))
      allocate(icount(nk))
      allocate(sl(nk))
      allocate(sl_g(nk))
      if ( slen_opt == 1 ) then
         nn = ni * ni + nj * nj  ! Maximum radius across grid (squared).
         allocate(nr(0:nn))
         call get_grid_info( ni, nj, nn, stride, nr )

         allocate(cov(0:nn,nk))
         allocate(ml(nk))

         do iv = 1, nvar
            if ( .not. do_this_var(iv) ) cycle
            sl(:) = 0.0
            ml(:) = 0.0
            if ( var_dim(iv) == 2 ) then
               k_start = 1
               k_end   = 1
            else if ( var_dim(iv) == 3 ) then
               k_start = level_start
               k_end   = min(nk, level_end)
            end if
            k_loop_opt1: do k = k_start, k_end
               if ( myproc == root ) then
                  write(stdout,'(a,i3,a,a)') ' Computing sl for level ', k, ' of ', trim(varnames(iv))
                  if ( verbose ) then
                     call date_and_time(date=this_date, time=this_time)
                     write(stdout,fmt='(a,a8,1x,a12)') &
                        '  Begin at ', this_date, &
                        this_time(1:2)//':'//this_time(3:4)//' '//this_time(5:10)
                  end if
               end if
               icount(k) = 0
               cov(0:nn,k) = 0.0
               do ic = 1, ncase
                  do ie = istart_member, iend_member

                     if ( pert1_read_opt == 1 ) then
                        field2d(:,:) = xdata(iv,ie,ic)%value(:,:,k)
                     else if ( pert1_read_opt == 2 ) then
                        write(ce,'(i3.3)') ie
                        if ( do_eof_transform .and. (var_dim(iv) == 3) ) then
                           input_file = 'pertv_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                        else
                           if ( trim(varnames(iv)) == 'CHI_U' .or. &
                                trim(varnames(iv)) == 'T_U'   .or. &
                                trim(varnames(iv)) == 'PSFC_U'    ) then
                              input_file = 'pert1_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                           else
                              input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
                           end if ! unbalanced variables
                        end if
                        call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, field)
                        field2d(:,:) = field(:,:,k)
                     end if

                     icount(k) = icount(k) + 1
                     ! Calculate spatial correlation
                     call get_covariance( ni, nj, nn, stride, icount(k), nr, &
                                          field2d(:,:), cov(0:nn,k) )
                  end do
               end do
               call make_scale_length(nn, nr, cov(0:nn,k), yr_cutoff(iv), ml(k), sl(k))
               if ( myproc == root ) then
                  if ( verbose ) then
                     call date_and_time(date=this_date, time=this_time)
                     write(stdout,fmt='(a,a8,1x,a12)') &
                        ' Finish at ', this_date, &
                        this_time(1:2)//':'//this_time(3:4)//' '//this_time(5:10)
                  end if
                  write(stdout,'(a,a,i3,f10.3,a)') ' ScaleLength: ', varnames(iv), k, sl(k)*ds*0.001, ' km'
               end if
               be_data(iv)%scale_length(k) = sl(k)
            end do k_loop_opt1
         end do

         deallocate(nr)
         deallocate(cov)
         deallocate(ml)
      else if ( slen_opt == 2 ) then

         allocate( hl(1:num_bins2d) )
         allocate( hlt(1:num_bins2d) )
         do iv = 1, nvar
            if ( .not. do_this_var(iv) ) cycle
            sl(:) = 0.0
            if ( var_dim(iv) == 2 ) then
               k_start = 1
               k_end   = 1
            else if ( var_dim(iv) == 3 ) then
               k_start = level_start
               k_end   = min(nk, level_end)
            end if
            k_loop_opt2: do k = k_start, k_end
               if ( myproc /= MOD((k-1), num_procs) ) cycle k_loop_opt2
               write(stdout,'(a,i3,a,i3,a,a)') ' Proc', myproc, ' computing sl for level ', k, ' of ', trim(varnames(iv))
               icount(k) = 0
               hlt(:) = 0.0
               do ic = 1, ncase
                  do ie = istart_member, iend_member

                     if ( pert1_read_opt == 1 ) then
                        field2d(:,:) = xdata(iv,ie,ic)%value(:,:,k)
                     else if ( pert1_read_opt == 2 ) then
                        write(ce,'(i3.3)') ie
                        if ( do_eof_transform .and. (var_dim(iv) == 3) ) then
                           input_file = 'pertv_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                        else
                           if ( trim(varnames(iv)) == 'CHI_U' .or. &
                                trim(varnames(iv)) == 'T_U'   .or. &
                                trim(varnames(iv)) == 'PSFC_U'    ) then
                              input_file = 'pert1_'//trim(varnames(iv))//'.'//filedates(ie,ic)//'.e'//trim(ce)
                           else
                              input_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
                           end if ! unbalanced
                        end if
                        call get_pert1_data(trim(input_file), trim(varnames(iv)), ni, nj, nk, field)
                        field2d(:,:) = field(:,:,k)
                     end if

                    call horz_lenscale(ni,nj,field2d,ds,mapfac_x,mapfac_y,hl,bin2d,num_bins2d)
                    icount(k) = icount(k) + 1
                    hlt(:) = hlt(:) + hl(:)
                  end do
               end do
               if ( icount(k) > 0 ) then
                  hl(:) = sum(hlt(:))/real(icount(k))
               end if
               sl(k) = hl(1) !hcl-num_bins2d=1
               sl(k) = sl(k) / ds ! convert to grid unit
               write(stdout,'(a,a,i3,f10.3,a)') 'ScaleLength: ', varnames(iv), k, sl(k)*ds*0.001, ' km'
               be_data(iv)%scale_length(k) = sl(k)
            end do k_loop_opt2
#ifdef DM_PARALLEL
            call mpi_allreduce(sl(:),sl_g(:), nk, &
                               this_mpi_real, mpi_sum, mpi_comm_world, ierr)
#else
            sl_g(:) = sl(:)
#endif
            be_data(iv)%scale_length(:) = sl_g(:)
         end do
         deallocate( hl )
         deallocate( hlt )
      end if
      deallocate(field2d)
      deallocate(icount)
      deallocate(sl)
      deallocate(sl_g)
   end if ! do_slen_calc

   deallocate(field)

   if ( pert1_read_opt == 1 ) then
   do ic = 1, ncase
      do ie = istart_member, iend_member
         do iv = 1, nvar
            deallocate(xdata(iv,ie,ic)%value)
         end do
      end do
   end do
   if ( allocated(xdata)) deallocate(xdata)
   end if ! pert1_read_opt=1

end subroutine compute_bv_sl

   subroutine horz_lenscale(nx,ny,x,ds,mapfac_x,mapfac_y,tlength,nlat,ncat)
!---------------------------------------------------------------------
! Purpose: Computes horizontal scalelength of a 2D field
!
! Method : L=[8{Del**2(Var(field)/Del**2(Var(Del**2(field))}]**0.25
!
! Reference: Wan-Shu et al, MWR 2002, Wan-Shu et al
!
! Author: Syed RH Rizvi,  NCAR/ESSL/MMM/DAG  08/06/2009
! Please acknowledge author/institute in work that uses this code.
!-----------------------------------------------------------------------

   implicit none
   integer, intent(in)       :: nx, ny   
   real,    intent(in)       :: x(nx,ny) 
   real,    intent(in)       :: ds
   real,    intent(in)       :: mapfac_x(nx,ny) 
   real,    intent(in)       :: mapfac_y(nx,ny) 
   real,    intent(out)      :: tlength(1:ncat)
   integer, intent(in)       :: nlat(nx,ny) 
   integer, intent(in)       :: ncat 

   real    :: lx(nx,ny) 
   real    :: xx(1:nx+2,1:ny+2) 
   real    :: corr(ncat),lcorr(ncat)
   real    :: dx, dy, sumx, sumlx
   integer :: i,j , n 

! Fill xx array
! Middle 
    xx(2:nx+1,2:ny+1)=x(1:nx,1:ny)
! Left Col 
    xx(1,2:ny+1)=x(1,1:ny)     
! Right Col
    xx(nx+2,2:ny+1)=x(nx,1:ny)     
! Bottom Row
    xx(2:nx+1,1) = x(1:nx,1) 
! Top Row
    xx(2:nx+1,ny+2) = x(1:nx,ny)
! Corners
    xx(1,1) = x(1,1) ; xx(nx+2,1) = x(nx,1) ; xx(nx+2,ny+2)=x(nx,ny) ; xx(1,ny+2)=x(1,ny)

! Now compute Laplacian of x  
    lx = 0

    do i=2,nx+1
    do j=2,ny+1
    dx=ds/mapfac_x(i-1,j-1)
    dx = dx* dx
    dy=ds/mapfac_y(i-1,j-1)
    dy = dy* dy
    lx(i-1,j-1) = ( (xx(i-1,j)+xx(i+1,j)-2.*xx(i,j))/dx + & 
                     (xx(i,j-1)+xx(i,j+1)-2.*xx(i,j))/dy   )  
    enddo
    enddo
! compute variance of field & its laplacian
    corr=0.
    lcorr=0.
    do j=1,ny
    do i=1,nx
    n=nlat(i,j)
    corr(n)=corr(n)+x(i,j)*x(i,j)
    lcorr(n)=lcorr(n)+lx(i,j)*lx(i,j)
    enddo
    enddo

    tlength(:) = 0.0 !initialize
    do n=1,ncat
    if( lcorr(n) > 0 ) tlength(n)=(8.*corr(n)/lcorr(n))**.25 
    enddo
!  
!
!! Average out      
!    sumx=0.   
!    sumlx=0.
!    do n=4,ncat-3
!    if( corr(n) > 0 .and. lcorr(n) > 0) then
!    sumx  = sumx  + corr(n)
!    sumlx = sumlx + lcorr(n)
!    else
!    end if
!    enddo
!    if( sumx == 0 .or. sumlx == 0.0 ) Then
!    tlength(1) = 1.
!    else
!    tlength(1) = (8.*sumx/sumlx)**.25 
!    end if
!    tlength(2:ncat)=tlength(1)

  end subroutine horz_lenscale

subroutine get_grid_info( ni, nj, nn, stride, nr )

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   integer, intent(out)   :: nr(0:nn)                ! Number of points in each bin.

   integer                :: i, j, m, n              ! Indices.
   integer                :: d2                      ! Distance squared.

   nr(0:nn) = 0

!  [1] Get points distribution nr(0:nn):

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:NR)
   do j = 1, nj, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate points distribution (i,j) for points that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            nr(d2) = nr(d2) + 1                        ! Add this point to number at this distance.
         end do

!        Now complete remaining rows:
         do n = j+1, nj
            do m = 1, ni
               d2 = (m-i)*(m-i) + (n-j)*(n-j)          ! Calculate distance (squared)
               nr(d2) = nr(d2) + 1                     ! Add this point to number at this distance.
            end do ! m
         end do ! n
      end do ! i
   end do ! j
!$OMP END PARALLEL DO

end subroutine get_grid_info

subroutine get_covariance( ni, nj, nn, stride, count, nr, field, cov )
   
!  Calculate the number of points, distance matrx between points on grid.
!  Dale Barker: May 16th 2005.

   implicit none

   integer, intent(in)    :: ni, nj                  ! Grid dimensions.
   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in)    :: stride                  ! Calculate correlation with every stride point.
   integer, intent(in)    :: count                   ! Number of times/ensemble members so far.
   integer, intent(in)    :: nr(0:nn)                ! Number of points in each bin.
   real, intent(in)       :: field(1:ni,1:nj)        ! 2D field.
   real, intent(inout)    :: cov(0:nn)               ! Covariance of each bin.

   integer                :: i, j, m, n              ! Loop counters.
   integer                :: d2                      ! Distance squared.
   real                   :: count_inv               ! 1 / count.
   real                   :: bb(0:nn)                ! Covariance for this field.

   count_inv = 1.0 / real(count)

   bb(0:nn) = 0.0

!$OMP PARALLEL DO PRIVATE(I,J,N,D2) REDUCTION(+:BB)
   do j = 1, nj, stride                         ! Pick every stride point to save cost.
      do i = 1, ni, stride                             ! Pick every stride point to save cost.

!        Calculate spatial correlations with point (i,j) that haven't already been computed:

!        Finish off current row first:
         n = j
         do m = i, ni
            d2 = (m-i) * (m-i)                         ! Calculate distance (squared).
            bb(d2) = bb(d2) + field(i,j) * field(m,n)
         end do

!        Now complete remaining rows:
         do n = j+1, nj
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

subroutine make_scale_length(nn, nr, cov, yr_cutoff, ml, sl)

!  Purpose: Calculate fit of input covariance data to Gaussian correlation function
!  cov(r) = cov(0) * exp(-r**2 / 8)
!  Dale Barker: 17th May 2005.

   implicit none

   integer, intent(in)    :: nn                      ! Dimension of radii bins.
   integer, intent(in )   :: nr(0:nn)                ! Number of points in each bin.
   real, intent(in)       :: cov(0:nn)               ! Covariance of each bin.
   real, intent(in)       :: yr_cutoff               ! Noise cut-off criterion.
   real, intent(out)      :: ml, sl            ! Gradient, scalelength.

   real(kind=8)           :: yr(0:nn)                ! sqrt(8 ln(cov(0)/cov(r)))
   real(kind=8)           :: nrr(0:nn)               ! Reduced nr array.
   real(kind=8)           :: d(0:nn)                 ! Distance for each bin.

   integer                :: n, d2                   ! Loop counters.
   integer                :: nmax                    ! Number of points available for curve fitting.
   real                   :: corr_min                ! Corresponding correlation value.
   real(kind=8)           :: coeff1, coeff2          ! Curve-fitting coefficients.

   yr(0:nn) = 0.0
   nrr(0:nn) = 0.0
   d(0:nn) = 0.0

   !hcl yr_cutoff = 3.0                                   ! Value taken from Yong-Run's experiments!
   !hcl make yr_cutoff a run-time namelist variable with default = 3.0
   corr_min = exp( -0.125 * yr_cutoff * yr_cutoff )  ! yr_cutoff = sqrt(8 ln corr_min)
   !write(UNIT=6,FMT='(a,1pe15.5)')' Fit Gaussian curve to data for correlations >= ', corr_min
   !write(UNIT=6,FMT='(5a)')'  n  ', ' nr(n) ', ' d(n) ', ' cov(n) ', ' yr(n)  '
   n = 0
   nrr(n) = real(nr(n))
   !write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(0), cov(n), yr(n)

   do d2 = 1, nn

      if ( nr(d2) > 0 .and. cov(d2) < cov(0) ) then ! Omit bins with no data and negative logs.

         if ( cov(d2) / cov(0) < corr_min ) exit ! Yong-Run's noise cut-off criterion.
         n = n + 1
         yr(n) = sqrt( 8.0 * log(cov(0) / cov(d2)) )
         nrr(n) = real(nr(d2))
         d(n) = sqrt(real(d2))                  ! Distance
         !write(UNIT=6,FMT='(i6,4e13.5)') n, nrr(n), d(n), cov(d2), yr(n)
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
      !WRITE(UNIT=6,FMT='("n, nrr, d, yr:",i3,3e15.6)') n, nrr(n), d(n), yr(n)
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

end subroutine make_scale_length

subroutine da_eof_decomposition (kz, bx, e, l)
! taken from WRFDA/var/da/da_tools/da_eof_decomposition.inc
   !---------------------------------------------------------------------------
   ! Purpose: Compute eigenvectors E and eigenvalues L of vertical covariance
   !          matrix
   !          B_{x} defined by equation:  E^{T} B_{x} E = L, given input kz x kz
   !          BE field.
   !---------------------------------------------------------------------------

#ifdef USE_WRFDA
   use da_lapack, only : dsyev
#endif
   implicit none

   integer, intent(in)  :: kz               ! Dimension of error matrix.
   real,    intent(in)  :: bx(1:kz,1:kz)    ! Vert. background error.
   real*8,  intent(out) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(out) :: l(1:kz)          ! Eigenvalues of Bx.

   integer :: work             ! Size of work array.
   integer :: m                ! Loop counters
   integer :: info             ! Info code.

   real*8  :: work_array(1:3*kz-1)
   real*8  :: ecopy(1:kz,1:kz)
   real*8  :: lcopy(1:kz)

   !-------------------------------------------------------------------------
   ! [5.0]: Perform global eigenvalue decomposition using LAPACK software:
   !-------------------------------------------------------------------------

   work = 3 * kz - 1
   ecopy(1:kz,1:kz) = bx(1:kz,1:kz)
   lcopy(1:kz) = 0.0

   call dsyev( 'V', 'U', kz, ecopy, kz, lcopy, work_array, work, info )

   if ( info /= 0 ) then
      if ( info == -99 ) then
         call error_handler(-1, 'The code must be compiled with LAPACK_LIB' )
      else
         write(unit=stdout,fmt='(A,I4)') "Error in decomposition, info = ", info
      end if
   end if

   ! Swap order of eigenvalues, vectors so 1st is one with most variance:

   do m = 1, kz
      l(m) = lcopy(kz+1-m)
      e(1:kz,m) = ecopy(1:kz,kz+1-m)
   end do

end subroutine da_eof_decomposition

subroutine da_eof_decomposition_test (kz, bx, e, l)
! taken from WRFDA/var/da/da_tools/da_eof_decomposition_test.inc
   !------------------------------------------------------------------------------
   ! Purpose:
   ! [1] Print eigenvalues:
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   ! [4] Check B correctness: B = E*L*E^T
   !------------------------------------------------------------------------------

   implicit none

   integer, intent(in) :: kz               ! Dimension of BE matrix
   real,    intent(in) :: bx(1:kz,1:kz)    ! Global vert. background error.
   real*8,  intent(in) :: e(1:kz,1:kz)     ! Eigenvectors of Bx.
   real*8,  intent(in) :: l(1:kz)          ! Eigenvalues of Bx.

   integer                  :: k, k1, k2, m     ! Loop counters
   real                     :: tot_variance     ! Total variance.
   real                     :: cum_variance     ! Cumulative variance.
   real                     :: max_off_diag     ! Maximum off-diagonal.

   real                     :: work(1:kz,1:kz)  ! 2D Work matrix.
   real                     :: bc(1:kz,1:kz)    ! 2D Work matrix.
   logical                  :: array_mask(1:kz) ! Array mask for MAXVAL.

   !------------------------------------------------------------------------- 
   ! [1] Print eigenvalues:
   !-------------------------------------------------------------------------

   tot_variance = sum(l(1:kz))
   cum_variance = 0.0

   write(unit=stdout,fmt='(A)')'  Mode    Eigenvalue     Cumulative Variance      e(k,k)'

   do k = 1, kz
      cum_variance = cum_variance + l(k)
      write(unit=stdout,fmt='(I4,4x,e12.4,10x,f8.4,4x,e12.4)') &
            k, l(k), cum_variance / tot_variance, e(k,k)
   end do

   write(unit=stdout,fmt=*)

   call da_array_print( 1, e, 'Global Eigenvectors' )

   !-------------------------------------------------------------------------
   ! [2] Test orthogonality of eigenvectors - sum_k (e_m(k) e_n(k)) = delta_mn:
   !-------------------------------------------------------------------------

   write(unit=stdout,fmt='(A)')' Eigenvector orthogonality check:'
   write(unit=stdout,fmt='(A)')' Mode     Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(1:kz,k1) * e(1:kz,k2))
      end do

      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------
   ! [3] Test eigenvectors completeness - sum_m (e_m(k1) e_m(k2)) = delta_k1k2:
   !-------------------------------------------------------------------------

   write(unit=stdout,fmt='(A)')' Eigenvector completeness check:'
   write(unit=stdout,fmt='(A)')' Level    Diagonal         Maximum off-diagonal'

   do k1 = 1, kz
      do k2 = 1, kz
         work(k1,k2) = sum(e(k1,1:kz) * e(k2,1:kz))
      end do

      array_mask(1:kz) =.true.
      array_mask(k1) = .false.
      max_off_diag = maxval(abs(work(k1,:)),mask=array_mask(:))
      write(unit=stdout,fmt='(I4,4x,1pe12.4,10x,1pe12.4)')k1, work(k1,k1), max_off_diag
   end do
   write(unit=stdout,fmt=*)

   !-------------------------------------------------------------------------
   ! [4]  check B correctness: B = E*L*E^T
   !-------------------------------------------------------------------------

   write(unit=stdout,fmt='(a/a)') &
        'real and Calculated B (diagonal)', &
        'lvl                 real-B                    Calculated-B'

   do k=1,kz
      do m=1,kz
         work(k,m)=l(k)*e(m,k)
         bc(k,m)=0.0
      end do
   end do

   do k1=1,kz
      do k2=1,kz
         do m=1,kz
            bc(k1,k2)=bc(k1,k2)+e(k1,m)*work(m,k2)
         end do
      end do

      write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k1), bc(k1,k1)
   end do

   do k2=1,kz
      write(unit=stdout, fmt='(a,i4/a)') &
           'real and Calculated B (off diagonal):', k2, &
           'lvl                 real-B                    Calculated-B'

      do k1=1,kz
        write(unit=stdout,fmt='(I5,2F20.5)') k1, bx(k1,k2), bc(k1,k2)
      end do
   end do

end subroutine da_eof_decomposition_test

subroutine da_array_print(direction, a, ch)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------
   implicit none

   integer, intent(in)            :: direction
   real*8, intent(in)             :: a(:,:)
   character (len=*), intent(in)  :: ch

   real                           :: amax
   integer                        :: i, j
   integer                        :: len1, len2
   integer                        :: jstart
   integer                        :: jend
   integer                        :: jump

   len1 = size(a(:,:),dim=1)
   len2 = size(a(:,:),dim=2)

   ! Writes the scalar field a

   write(unit=stdout,fmt='(A)') trim(ch)

   amax = MAXVAL(abs(a))
   write(unit=stdout, fmt='(a, 1pe15.8, 4i8)') &
        '   max(a)=', amax, shape(a)

   write(unit=stdout,fmt='(a, 1pe15.8, a)') &
        '   max(a)=', amax, ', i down, j horiz.'

   write(unit=stdout,fmt='(6x,288i3)') (i,i=1,len2)

   ! Direction indicates the order of the rows of the print out:

   if (direction == 1) then
      jstart = 1
      jend = len1
      jump = 1
   else
      jstart = len1
      jend = 1
      jump = -1
   end if

   if (amax.ne.0.0)then
      do j=jstart,jend,jump
         write(unit=stdout,fmt='(1x,i5,288i3)') &
            j, (inT(a(j,i)/amax*99.0),i=1,len2)
      end do
   end if

   write (unit=stdout,fmt='(A)') " "

end subroutine da_array_print

subroutine error_handler(ierr, err_message)

   implicit none
   integer, intent(in) :: ierr
   character(len=*), intent(in) :: err_message

   if ( ierr /= 0 ) then
      if ( myproc == root ) write(stdout,*) 'Error: ', trim(err_message)
#ifdef DM_PARALLEL
      call mpi_abort(mpi_comm_world,1,ierr)
#else
      stop
#endif
   end if

end subroutine error_handler

subroutine get_pert1_data(pert_file, vname, nx, ny, nz, vdata)

   implicit none

   character(len=*), intent(in)    :: pert_file
   character(len=*), intent(in)    :: vname
   integer,          intent(in)    :: nx, ny, nz
   real,             intent(inout) :: vdata(nx,ny,nz)

   integer :: nvar_read, nx_read, ny_read, nz_read
   integer :: iv, iunit, ndim
   logical :: isfile
   real, allocatable :: field(:,:,:)
   character(len=DateStrLen) :: DateStr_tmp
   character(len=VarNameLen) :: var_tmp

   iunit = 21

   !pert_file = 'pert1.'//filedates(ie,ic)//'.e'//trim(ce)
   inquire(file=trim(pert_file), exist=isfile)
   if ( .not. isfile ) then
      call error_handler(-1, trim(pert_file)//' not found for getting perturbation')
   end if
   !write(stdout,'(a,i3,a,a)') ' Proc', myproc, ' Reading from ', trim(pert_file)
   open (iunit, file=trim(pert_file), form='unformatted', status='old')
   read(iunit) nvar_read, nx_read, ny_read, nz_read
   if ( nx/=nx_read .or. ny/=ny_read .or. nz/=nz_read ) then
      call error_handler(-1, 'dimensions mismatch in get_pert1_data')
   end if
   allocate(field(nx,ny,nz))
   field(:,:,:) = 0.0
   read_loop: do iv = 1, nvar_read
      read(iunit) ndim
      read(iunit) DateStr_tmp, var_tmp
      if ( ndim == 2 ) then
         read(iunit) field(:,:,1)
      else if ( ndim == 3 ) then
         read(iunit) field(:,:,:)
      end if
      if ( trim(vname) == trim(var_tmp) ) then
         exit read_loop
      else
         if ( iv < nvar_read ) then
            cycle read_loop
         else
            close(iunit)
            call error_handler(-1, 'error reading pert1 for variable '//trim(vname))
         end if
      end if
   end do read_loop
   vdata(:,:,:) = field(:,:,:)
   close(iunit)
   deallocate(field)

end subroutine get_pert1_data

subroutine calc_es_qs (t, p, es, qs, wrt_ice)

! Purpose: calculate saturation vapor pressure and saturation specific humidity
!          given temperature and pressure

   implicit none

   real, intent(in)  :: t, p
   real, intent(out) :: es, qs
   logical, intent(in), optional :: wrt_ice

   real, parameter   :: es_alpha = 611.2
   real, parameter   :: es_beta  = 17.67
   real, parameter   :: es_gamma = 243.5
   real, parameter   :: t_c_ref1 =   0.0   ! C
   real, parameter   :: t_c_ref2 = -20.0   ! C
   real, parameter   :: a0 = 6.107799961
   real, parameter   :: a1 = 4.436518521e-01
   real, parameter   :: a2 = 1.428945805e-02
   real, parameter   :: a3 = 2.650648471e-04
   real, parameter   :: a4 = 3.031240396e-06
   real, parameter   :: a5 = 2.034080948e-08
   real, parameter   :: a6 = 6.136820929e-11
   real, parameter   :: c1 = 9.09718
   real, parameter   :: c2 = 3.56654
   real, parameter   :: c3 = 0.876793
   real, parameter   :: c4 = 6.1071
   real              :: t_c, t1_c          ! T in degree C
   logical           :: ice

   ice = .false.
   if ( present(wrt_ice) ) then
      if ( wrt_ice ) ice = .true.
   end if

   t_c  = t - t_kelvin
   t1_c = t - t_triple

   ! Calculate saturation vapor pressure es

   if ( .not. ice ) then   ! over water only

      es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )

   else   ! consider ice-water and ice effects

      if ( t1_c > t_c_ref1 ) then   ! vapor pressure over water
         es = es_alpha * exp( es_beta * t_c / ( t_c + es_gamma ) )
      else if ( (t1_c <= t_c_ref1) .and. (t1_c >= t_c_ref2) ) then   ! vapor pressure over water below 0C
         es = a0 + t1_c * (a1 + t1_c * (a2 + t1_c * (a3 + t1_c * (a4 + t1_c * (a5 + t1_c * a6)))))
         es = es * 100.0  ! to Pa
      else   ! vapor pressure over ice
         es = 10.0 ** ( -c1 * (t_triple / t - 1.0) - c2 * alog10(t_triple / t) +  &
                         c3 * (1.0 - t / t_triple) +      alog10(c4))
         es = es * 100.0  ! to Pa
      end if

   end if

   ! Calculate saturation specific humidity qs

   qs = rd_over_rv * es / ( p - rd_over_rv1 * es )

end subroutine calc_es_qs

end program gen_be_v3

subroutine para_range(n1, n2, nprocs, myrank, ista, iend)
!
! Purpose: determines the start and end index for each PE
!          given the loop range.
!
   implicit none

   integer, intent(in)  :: n1, n2, nprocs, myrank
   integer, intent(out) :: ista, iend

   integer :: iwork1, iwork2

   iwork1 = (n2 - n1 + 1) / nprocs
   iwork2 = mod(n2 - n1 + 1, nprocs)
   ista = myrank * iwork1 + n1 + min(myrank, iwork2)
   iend = ista + iwork1 - 1
   if (iwork2 > myrank) iend = iend + 1
   return
end subroutine para_range

! wrf_debug is called by ext_ncd_ subroutines
! add dummy subroutine wrf_debug here to avoid WRF dependency
SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  RETURN
END SUBROUTINE wrf_debug

subroutine wrfda_decompose_xy (nproc, ids, ide, jds, jde, kds, kde, &
                         ips, ipe, jps, jpe, kps, kpe, ntasks_x, ntasks_y)

   implicit none

   integer, intent(in) :: nproc
   integer, intent(in) :: ids, ide, jds, jde, kds, kde
   integer, intent(inout) :: ips(0:nproc-1), ipe(0:nproc-1)
   integer, intent(inout) :: jps(0:nproc-1), jpe(0:nproc-1)
   integer, intent(inout) :: kps(0:nproc-1), kpe(0:nproc-1)
   integer, intent(out)   :: ntasks_x, ntasks_y

   integer, allocatable :: itask_x(:), itask_y(:)
   integer :: i, j, iproc
   integer :: stdout
   logical :: verbose

   verbose = .false.
   stdout = 6

   call mpaspect(nproc, ntasks_x, ntasks_y, 1, 1)
   if ( verbose ) then
      write(stdout,'(i4,a,i4,a,i4)')  &
         nproc, ' decomposes to ', ntasks_x, ' x ', ntasks_y
   end if

   allocate(itask_x(0:nproc-1))
   allocate(itask_y(0:nproc-1))

   iproc = 0
   do j = 0, ntasks_y - 1
      do i = 0, ntasks_x -1
         itask_x(iproc) = i
         itask_y(iproc) = j
         iproc = iproc + 1
      end do
   end do

   if ( verbose ) then
      do i = 0, nproc-1
         write(stdout,'(a,i4,a,i4,a,i4,a)')  &
            'proc ', i, ' (', itask_x(i), ', ', itask_y(i), ')'
      end do
   end if

   do iproc = 0, nproc-1
      call compute_patch_dims  ( ntasks_x, ntasks_y,      &
                   iproc, itask_x(iproc), itask_y(iproc), &
                   ids, ide, jds, jde, kds, kde,          &
                   ips(iproc), ipe(iproc),                &
                   jps(iproc), jpe(iproc),                &
                   kps(iproc),  kpe(iproc) )

      !if ( ipe(iproc) == ide ) ipe(iproc) = ipe(iproc) - 1
      !if ( jpe(iproc) == jde ) jpe(iproc) = jpe(iproc) - 1
      !if ( kpe(iproc) == kde ) kpe(iproc) = kpe(iproc) - 1
      if ( verbose ) then
         write(stdout,'(a,i4,a,6i6)')'proc ',iproc, ':', &
            ips(iproc),ipe(iproc),jps(iproc),jpe(iproc), &
            kps(iproc),kpe(iproc)
      end if
   end do

   deallocate(itask_x)
   deallocate(itask_y)

contains

   SUBROUTINE MPASPECT( P, MINM, MINN, PROCMIN_M, PROCMIN_N )
   ! adapted from subroutine mpaspect in external/RSL_LITE/module_dm.F
      IMPLICIT NONE
      INTEGER P, M, N, MINI, MINM, MINN, PROCMIN_M, PROCMIN_N, ierror
      MINI = 2*P
      MINM = 1
      MINN = P
      DO M = 1, P
        IF ( MOD( P, M ) .EQ. 0 ) THEN
          N = P / M
          IF ( ABS(M-N) .LT. MINI                &
               .AND. M .GE. PROCMIN_M            &
               .AND. N .GE. PROCMIN_N            &
             ) THEN
            MINI = ABS(M-N)
            MINM = M
            MINN = N
          ENDIF
        ENDIF
      ENDDO
      IF ( MINM .LT. PROCMIN_M .OR. MINN .LT. PROCMIN_N ) THEN
        WRITE( * , * )'MPASPECT: UNABLE TO GENERATE PROCESSOR MESH.  STOPPING.'
        WRITE( * , * )' PROCMIN_M ', PROCMIN_M
        WRITE( * , * )' PROCMIN_N ', PROCMIN_N
        WRITE( * , * )' P         ', P
        WRITE( * , * )' MINM      ', MINM
        WRITE( * , * )' MINN      ', MINN
      ENDIF
   RETURN
   END SUBROUTINE MPASPECT

  SUBROUTINE compute_patch_dims  ( ntasks_x, ntasks_y, &
                   mytask, mytask_x, mytask_y,         &
                   ids,  ide,  jds,  jde,  kds,  kde,  &
                   ips,  ipe,  jps,  jpe,  kps,  kpe )

    IMPLICIT NONE
    INTEGER, INTENT(IN)     ::  ntasks_x, ntasks_y
    INTEGER, INTENT(IN)     ::  mytask, mytask_x, mytask_y
    INTEGER, INTENT(IN)     ::  ids, ide, jds, jde, kds, kde
    INTEGER, INTENT(OUT)    ::  ips, ipe, jps, jpe, kps, kpe

    integer :: shw
    INTEGER Px, Py, P, i, j, k, ierr
    integer :: minx, miny

    shw = 0
    minx = 1
    miny = 1

! xy decomposition

    ips = -1
    j = jds
    ierr = 0
    DO i = ids, ide
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, ierr )
       IF ( ierr .NE. 0 ) stop 'error code returned by task_for_point '
       IF ( Px .EQ. mytask_x ) THEN
          ipe = i
          IF ( ips .EQ. -1 ) ips = i
       ENDIF
    ENDDO
    ! handle setting the memory dimensions where there are no X elements assigned to this proc
    IF (ips .EQ. -1 ) THEN
       ipe = -1
       ips = 0
    ENDIF
    jps = -1
    i = ids
    ierr = 0
    DO j = jds, jde
       CALL task_for_point ( i, j, ids, ide, jds, jde, ntasks_x, ntasks_y, Px, Py, ierr )
       IF ( ierr .NE. 0 ) stop 'error code returned by task_for_point '
       IF ( Py .EQ. mytask_y ) THEN
          jpe = j
          IF ( jps .EQ. -1 ) jps = j
       ENDIF
    ENDDO
    ! handle setting the memory dimensions where there are no Y elements assigned to this proc
    IF (jps .EQ. -1 ) THEN
       jpe = -1
       jps = 0
    ENDIF

! This appears redundant with the conditionals above, but we get cases with only
! one of the directions being set to "missing" when turning off extra processors.
! This may break the handling of setting only one of nproc_x or nproc_y via the namelist.
    IF (ipe .EQ. -1 .or. jpe .EQ. -1) THEN
       ipe = -1
       ips = 0
       jpe = -1
       jps = 0
    ENDIF

! extend the patch dimensions out shw along edges of domain
    IF ( ips < ipe .and. jps < jpe ) THEN
       IF ( mytask_x .EQ. 0 ) THEN
          ips = ips - shw
       ENDIF
       IF ( mytask_x .EQ. ntasks_x-1 ) THEN
          ipe = ipe + shw
       ENDIF
       IF ( mytask_y .EQ. 0 ) THEN
          jps = jps - shw
       ENDIF
       IF ( mytask_y .EQ. ntasks_y-1 ) THEN
          jpe = jpe + shw
       ENDIF
    ENDIF

    kps = 1
    kpe = kde-kds+1

  END SUBROUTINE compute_patch_dims

  subroutine task_for_point(i_p, j_p, ids_p, ide_p, jds_p, jde_p, npx_p, npy_p, Px, Py, ierr)
  ! adapted from external/RSL_LITE/task_for_point.c

    implicit none

    integer :: i_p , j_p , ids_p, ide_p , jds_p, jde_p , npx_p , npy_p, ierr
    integer :: i , j , ids, ide, jds, jde, npx, npy, minx, miny
    integer :: Px, Py
    integer :: idim, jdim
    integer :: rem, a, b

    i = i_p - 1
    j = j_p - 1
    npx = npx_p
    npy = npy_p
    minx = 1
    miny = 1

    ids = ids_p - 1
    ide = ide_p - 1
    jds = jds_p - 1
    jde = jde_p - 1
    idim = ide - ids + 1
    jdim = jde - jds + 1

    ierr = 0

    if ( npx > idim ) npx = idim
    if ( npy > jdim ) npy = jdim

    if ( idim / npx < minx ) then
       npx = idim/minx
       if (npx < 1) npx = 1
       if (npx /= npx_p) then
          write(stdout,'(a,2i4)')  &
             "TASK_FOR_POINT LIMITING PROCESSOR COUNT IN X-DIRECTION TO ", &
             npx, npx_p
          ierr = 1
       end if
    end if
    if ( jdim / npy < miny ) then
       npy = jdim/miny
       if (npy < 1) npy = 1
       if (npy /= npy_p) then
          write(stdout,'(a,2i4)')  &
             "TASK_FOR_POINT LIMITING PROCESSOR COUNT IN Y-DIRECTION TO ", &
             npy, npy_p
          ierr = 1
       end if
    end if

    !i = i >= ids ? i : ids ; i = i <= ide ? i : ide ;
    i = max(i, ids)
    i = min(i, ide)
    rem = mod(idim, npx)
    a = ( rem / 2 ) * ( (idim / npx) + 1 )
    b = a + ( npx - rem ) * ( idim / npx )
    if ( i-ids < a ) then
       Px = (i-ids) / ( (idim / npx) + 1 )
    else if ( i-ids < b ) then
       Px = ( a / ( (idim / npx) + 1 ) ) + (i-a-ids) / ( ( b - a ) / ( npx - rem ) )
    else
       Px = ( a / ( (idim / npx) + 1 ) ) + (b-a-ids) / ( ( b - a ) / ( npx - rem ) ) + &
                                           (i-b-ids) / ( ( idim / npx ) + 1 )
    end if

    !j = j >= jds ? j : jds ; j = j <= jde ? j : jde ;
    j = max(j, jds)
    j = min(j, jde)
    rem = mod(jdim, npy)
    a = ( rem / 2 ) * ( (jdim / npy) + 1 )
    b = a + ( npy - rem ) * ( jdim / npy )
    if ( j-jds < a ) then
       Py = (j-jds) / ( (jdim / npy) + 1 )
    else if ( j-jds < b ) then
       Py = ( a / ( (jdim / npy) + 1 ) ) + (j-a-jds) / ( ( b - a ) / ( npy - rem ) )
    else
       Py = ( a / ( (jdim / npy) + 1 ) ) + (b-a-jds) / ( ( b - a ) / ( npy - rem ) ) + &
                                           (j-b-jds) / ( ( jdim / npy ) + 1 )
    end if

  end subroutine task_for_point

end subroutine wrfda_decompose_xy

#ifdef NO_LAPACK_LIB
subroutine dsyev(JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )

   implicit none
   character(len=*) :: JOBZ, UPLO
   integer          :: INFO, LDA, LWORK, N
   real*8           :: A( LDA, * ), W( * ), WORK( * )

   ! This is a dummy subroutine for the code to compile for ep application without lapack lib.
   ! If this subroutine gets called (do_slen_calc=true and do_eof_transform=true),
   ! return a special info code to indicate that the code must be compiled with LAPACK_LIB.
   info = -99

end subroutine dsyev
#endif

subroutine da_uv_to_div_c( dim1, dim2, dim3, ds, &
                           mapfac_m, mapfac_u, mapfac_v, &
                           u, v, div )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate divergence on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!
!  NOTE: No boundary conditions required on the WRF Arakawa C-grid as
!        divergence (mass) points are all within the outer u/v pts.
!
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!                        d   U      d   V
!           Div = m^2 *[---(---) + ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2, dim3             ! Dimensions
   real, intent(in)   :: ds                           ! Resolution
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points
   real, intent(in)   :: u(1:dim1+1,1:dim2,1:dim3)    ! v wind
   real, intent(in)   :: v(1:dim1,1:dim2+1,1:dim3)    ! v wind
   real, intent(out)  :: div(1:dim1,1:dim2,1:dim3)    ! Divergence

   integer            :: i, j, k                      ! Loop counters
   real               :: ds_inv                       ! 1/ds
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient
   real               :: um(1:dim1+1,1:dim2,1:dim3)   ! u-wind copy
   real               :: vm(1:dim1,1:dim2+1,1:dim3)   ! v-wind copy

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   div(:,:,:) = 0.0

   ds_inv = 1.0 / ds
   coeff(:,:) = ds_inv

!------------------------------------------------------------------------------
!  [2] Calculate scaled u/v field:
!------------------------------------------------------------------------------

   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1+1
            um(i,j,k) = u(i,j,k) / mapfac_u(i,j)
         end do
      end do
   end do

   do k = 1, dim3
      do j = 1, dim2+1
         do i = 1, dim1
            if (mapfac_v(i,j) > 0.000001) then
               vm(i,j,k) = v(i,j,k) / mapfac_v(i,j)
            else
               vm(i,j,k)=0.0
            end if
         end do
      end do
   end do

!------------------------------------------------------------------------------
!  [3] Calculate divergence field:
!------------------------------------------------------------------------------

   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1
            div(i,j,k) = coeff(i,j) * ( um(i+1,j,k) - um(i,j,k) + vm(i,j+1,k) - vm(i,j,k) )
         end do
      end do
   end do

end subroutine da_uv_to_div_c

subroutine da_uv_to_vor_c( dim1, dim2, dim3, ds, &
                           mapfac_m, mapfac_u, mapfac_v, &
                           u, v, vor )

!------------------------------------------------------------------------------
!  PURPOSE: Calculate vorticity on a co-ordinate surface, given an input
!           wind field on an Arakawa C-grid.
!
!  NOTE: Zero vorticity boundary conditions.
!
!  HISTORY: 08/09/2005 - Creation of F90 version.           Dale Barker
!                        d   V      d   U
!           Vor = m^2 *[---(---) - ---(---) ]
!                        dx  m      dy  M
!------------------------------------------------------------------------------

   implicit none

   integer, intent(in):: dim1, dim2, dim3             ! Dimensions
   real, intent(in)   :: ds                           ! Resolution
   real, intent(in)   :: mapfac_m(1:dim1,1:dim2)      ! Map factor - mass pts
   real, intent(in)   :: mapfac_u(1:dim1+1,1:dim2)    ! Map factor - u points
   real, intent(in)   :: mapfac_v(1:dim1,1:dim2+1)    ! Map factor - u points
   real, intent(in)   :: u(1:dim1+1,1:dim2,1:dim3)    ! v wind
   real, intent(in)   :: v(1:dim1,1:dim2+1,1:dim3)    ! v wind
   real, intent(out)  :: vor(1:dim1+1,1:dim2+1,1:dim3)! Vorticity

   integer            :: i, j, k                      ! Loop counters
   real               :: ds_inv                       ! 1/ds
   real               :: coeff(1:dim1,1:dim2)         ! Coefficient
   real               :: um(1:dim1+1,1:dim2,1:dim3)   ! u-wind copy
   real               :: vm(1:dim1,1:dim2+1,1:dim3)   ! v-wind copy

!------------------------------------------------------------------------------
!  [1.0] Initialise:
!------------------------------------------------------------------------------

   vor(:,:,:) = 0.0

   ds_inv = 1.0 / ds
   coeff(:,:) = ds_inv

!------------------------------------------------------------------------------
!  [2] Calculate scaled u/v field:
!------------------------------------------------------------------------------

   do k = 1, dim3
      do j = 1, dim2
         do i = 1, dim1+1
            um(i,j,k) = u(i,j,k) / mapfac_u(i,j)
         end do
      end do
   end do

   do k = 1, dim3
      do j = 1, dim2+1
         do i = 1, dim1
            if (mapfac_v(i,j) > 0.000001) then
               vm(i,j,k) = v(i,j,k) / mapfac_v(i,j)
            else
               vm(i,j,k)=0.0
            end if
         end do
      end do
   end do

!------------------------------------------------------------------------------
!  [4] Calculate vorticity field:
!------------------------------------------------------------------------------

   do k = 1, dim3
      do j = 2, dim2
         do i = 2, dim1
            vor(i,j,k) = coeff(i,j) * ( vm(i,j,k) - vm(i-1,j,k) - um(i,j,k) + um(i,j-1,k) )
         end do
      end do
   end do

!  Boundary values (extrapolation):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
!   vor(1,1:dim2+1)      = 2.0 * vor(2,1:dim2+1) - vor(3,1:dim2+1)         ! West
!   vor(dim1+1,1:dim2+1) = 2.0 * vor(dim1,1:dim2+1) - vor(dim1-1,1:dim2+1) ! East
!   vor(1:dim1+1,1)      = 2.0 * vor(1:dim1+1,2) - vor(1:dim1+1,3)         ! South
!   vor(1:dim1+1,dim2+1) = 2.0 * vor(1:dim1+1,dim2) - vor(1:dim1+1,dim2-1) ! North

!  Boundary values (zero gradient):
!  Note - not used in Del**2 calculation if overwritten with bcs there).
   vor(1,2:dim2,:)        = vor(2,2:dim2,:)      ! West
   vor(dim1+1,2:dim2,:)   = vor(dim1,2:dim2,:)   ! East
   vor(1:dim1+1,1,:)      = vor(1:dim1+1,2,:)    ! South
   vor(1:dim1+1,dim2+1,:) = vor(1:dim1+1,dim2,:) ! North

end subroutine da_uv_to_vor_c

subroutine da_del2a_to_a( dim1, dim2, dim3, ds, del2a, a )
! adapted from var/gen_be/gen_be_stage0_wrf.f90

   implicit none

   integer, intent(in):: dim1, dim2, dim3
   real, intent(in)   :: ds                             ! grid distance
   real, intent(in)   :: del2a(1:dim1,1:dim2,1:dim3)    ! Del**2 a
   real, intent(out)  :: a(1:dim1,1:dim2,1:dim3)        ! Field a

   integer, parameter :: num_fft_factors = 10
   integer, parameter :: nrange = 1000
   real,    parameter :: pi = 3.1415926

   integer            :: n1, n2                         ! Padded dimensions (n=dim-1+pad)
   integer            :: fft_method                     ! 1=Cosine, 2=Sine transform
   integer            :: ifax1(1:num_fft_factors)       ! FFT factors
   integer            :: ifax2(1:num_fft_factors)       ! FFT factors
   real, allocatable  :: trigs1(:)                      ! FFT trig functions
   real, allocatable  :: trigs2(:)                      ! FFT trig functions
   real, allocatable  :: fft_coeffs(:,:)                ! FFT coefficients

   integer            :: fft_pad1, fft_pad2             ! Range to search for efficient FFT
   logical            :: found_magic                    ! True if 2**p 3**p 5**r dimension found
   integer            :: fft_factors(1:num_fft_factors) ! FFT factors
   real               :: const                          ! Multiplicative constant
   real               :: coeff_nx                       ! Multiplicative constant
   real               :: coeff_ny                       ! Multiplicative constant
   real               :: cos_coeff_nx                   ! Multiplicative constant
   real               :: cos_coeff_ny                   ! Multiplicative constant

   integer            :: i, j, k                        ! Loop counters
   integer            :: ij                             ! 1D array counter
   integer            :: isign                          ! -1=Grid>spec, 1=Spec>Grid
   integer            :: inc                            ! Stride between data points
   integer            :: jump                           ! Increment between start of data vectors
   integer            :: lot                            ! Number of data vectors
   integer            :: n                              ! n+1 is the length of the data
   integer            :: work_area                      ! Dimension of workspace
   !real               :: a2d(1:n1+1,1:n2+1)             ! 2D data array
   !real               :: a1d(1:(n1+1)*(n2+1))           ! 1D data array
   real, allocatable  :: a2d(:,:)                       ! 2D data array
   real, allocatable  :: a1d(:)                         ! 1D data array

!  Ensure efficient FFT dimensions by padding if necessary:
   n1 = dim1 - 1
   do n = n1, n1 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors )
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad1 = n - n1
         ifax1 = fft_factors
         exit
      end if
   end do
   n1 = n1 + fft_pad1

   n2 = dim2 - 1
   do n = n2, n2 + nrange
      call da_find_fft_factors( n, found_magic, fft_factors )
      if ( found_magic .and. mod(n,2) == 0 ) then ! Even magic number found.
         fft_pad2 = n - n2
         ifax2 = fft_factors
         exit
      end if
   end do
   n2 = n2 + fft_pad2

   allocate( trigs1(1:3*n1) )
   allocate( trigs2(1:3*n2) )
   allocate( fft_coeffs(1:n1+1,1:n2+1) )

   const = -0.5 * ds * ds
   coeff_nx = pi / real(n1)
   coeff_ny = pi / real(n2)

!  Calculate spectral Del**2 coefficients for C-grid (all pts. except i=j=1):
   fft_coeffs(1,1) = 0.0 ! Not used?
   do j = 2, n2+1
      cos_coeff_ny = cos(coeff_ny * real(j - 1))
      do i = 1, n1+1
         cos_coeff_nx = cos(coeff_nx * real(i - 1))
         fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
      end do
   end do
   j = 1
   cos_coeff_ny = cos(coeff_ny * real(j - 1))
   do i = 2, n1+1
      cos_coeff_nx = cos(coeff_nx * real(i - 1))
      fft_coeffs(i,j) = const / ( 2.0 - cos_coeff_nx - cos_coeff_ny)
   end do

   call da_find_fft_trig_funcs( n1, trigs1 )
   call da_find_fft_trig_funcs( n2, trigs2 )

   fft_method = 2

   work_area = ( n1 + 1 ) * ( n2 + 1 )

   allocate ( a2d(1:n1+1,1:n2+1) )
   allocate ( a1d(1:(n1+1)*(n2+1)) )

do k = 1, dim3

!  Fill 2D array structure
   do j = 1, dim2
      do i = 1, dim1
         a2d(i,j) = del2a(i,j,k)
      end do

!     Fill pad zone (and force b.c.s to satisfy solution type):
      if ( fft_method == 1 ) then ! Cosine transform.
         a2d(1,j) = a2d(2,j)
         do i = dim1, n1+1
            a2d(i,j) = a2d(dim1-1,j)
         end do
      else if ( fft_method == 2 ) then ! Sine transform:
         a2d(1,j) = 0.0
         do i = dim1, n1+1
            a2d(i,j) = 0.0
         end do
      end if
   end do

   if ( fft_method == 1 ) then ! Cosine transform.
      do i = 1, n1+1
         a2d(i,1) = a2d(i,2)
         do j = dim2, n2+1
            a2d(i,j) = a2d(i,dim2-1)
         end do
      end do
   else if ( fft_method == 2 ) then ! Sine transform:
      do i = 1, n1+1
         a2d(i,1) = 0.0
         do j = dim2, n2+1
            a2d(i,j) = 0.0
         end do
      end do
   end if

!  Transfer to data array:
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = a2d(i,j)
      end do
   end do

!------------------------------------------------------------------------------
!     Perform double fast sine/cosine transform to get spectral del2a:
!------------------------------------------------------------------------------

   isign = -1 ! Grid to spectral

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.
   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                   ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                   ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                   ifax2, trigs2, a1d, work_area )
   end if

!------------------------------------------------------------------------------
!  Perform conversion from del2a to a in spectral space:
!------------------------------------------------------------------------------

!  Note fft_coeffs(1,1)=0 so a(k=0,l=0) is also 0.
   do j = 1, n2+1
      do i = 1, n1+1
         ij = (j-1) * (n1+1) + i
         a1d(ij) = fft_coeffs(i,j) * a1d(ij)
      end do
   end do

!------------------------------------------------------------------------------
!  Perform double fast sine/cosine transform to get gridpoint a:
!------------------------------------------------------------------------------

   isign = 1 ! Spectral to grid.

!  1st dimension:
   inc = 1    ! Stride between data points.
   jump = n1+1! Increment between start of data vectors.
   lot = n2+1 ! Number of data vectors.
   n = n1     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                   ifax1, trigs1, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                   ifax1, trigs1, a1d, work_area )
   end if

!  2nd dimension:
   inc = n1+1 ! Stride between data points.
   jump = 1   ! Increment between start of data vectors.
   lot = n1+1 ! Number of data vectors.
   n = n2     ! n+1 is the length of the data.

   if ( fft_method == 1 ) then
      call fft551( isign, inc, jump, lot, n, &
                   ifax2, trigs2, a1d, work_area )
   else if ( fft_method == 2 ) then
      call fft661( isign, inc, jump, lot, n, &
                   ifax2, trigs2, a1d, work_area )
   end if

!  Transfer grid-point chi to 2D-array (throwing away pad):
   do j = 1, dim2
      do i = 1, dim1
         ij = (j-1) * (n1+1) + i
         a(i,j,k) = a1d(ij)
      end do
   end do

end do ! k loop

   deallocate (a2d)
   deallocate (a1d)
   deallocate (fft_coeffs)
   deallocate (trigs2)
   deallocate (trigs1)

contains

subroutine da_find_fft_factors(n, n_ok, fft_factors)
! taken from var/da/da_tools/da_find_fft_factors.inc

   !---------------------------------------------------------------------------
   ! Purpose: Calculates prime factors of input number.
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n
   logical, intent(out) :: n_ok
   integer, intent(out) :: fft_factors(:)

   integer :: i, k, l
   integer :: nfax, nu, ifac
   integer :: jfax(num_fft_factors)
   integer :: lfax(7)

   data lfax /6,8,5,4,3,2,1/

   ! in da_control
   !if (trace_use) call da_trace_entry("da_find_fft_factors")

   !---------------------------------------------------------------------------
   ! [1.0] Find factors of vector size (8,6,5,4,3,2; only one 8 allowed):
   !---------------------------------------------------------------------------

   n_ok = .false.
   fft_factors(:) = 0

   ! look for sixes first, store factors in descending order
   nu=n
   ifac=6
   k=0
   l=1

20 continue

   if (mod(nu,ifac).ne.0) goto 30

   ! 6 is a factor:
   k=k+1
   jfax(k)=ifac
   if (ifac.ne.8) goto 25
   if (k.eq.1) goto 25
   jfax(1)=8
   jfax(k)=6

25 continue
   nu=nu/ifac
   if (nu.eq.1) goto 50
   if (ifac.ne.8) goto 20

30 continue
   l=l+1
   ifac=lfax(l)
   if (ifac .gt. 1) goto 20

   ! illegal factors:
   ! write (unit=message(1),fmt='(a,i4,a)') 'n = ', n, ' contains illegal factors.'
   ! call da_warning(__file__,__line__,message(1:1))

   goto 9

   ! now reverse order of factors
50 continue
   nfax=k
   fft_factors(1)=nfax
   do i=1,nfax
      fft_factors(nfax+2-i)=jfax(i)
   end do

   n_ok = .true.

9  continue

   !if (trace_use) call da_trace_exit("da_find_fft_factors")

end subroutine da_find_fft_factors

subroutine da_find_fft_trig_funcs(n, trig_functs)
! taken from var/da/da_tools/da_find_fft_trig_funcs.inc

   !---------------------------------------------------------------------------
   ! Purpose: Set up constants required for Fourier, sine and cosine transforms
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)  :: n
   real,    intent(out) :: trig_functs(:)

   integer :: k, nil, nhl
   real    :: del, angle

   ! in da_control
   ! if (trace_use) call da_trace_entry("da_find_fft_trig_funcs")

   !---------------------------------------------------------------------------
   ! [1.0] Trig functions for real periodic transform:
   !---------------------------------------------------------------------------

   trig_functs(:) = 0.0

   del=4.0*(pi/2.0)/float(n)
   nil=0
   nhl=(n/2)-1

   do k=nil,nhl
      angle=float(k)*del
      trig_functs(2*k+1)=cos(angle)
      trig_functs(2*k+2)=sin(angle)
   end do

   ! [1.1] extra trig functions for cosine transform:

   del=0.5*del
   do k=1,nhl
      angle=float(k)*del
      trig_functs(2*n+k)=sin(angle)
   end do

   ! [1.2] extra trig functions for shifted cosine transform:

   del=0.5*del
   do k=1,n
      angle=float(k)*del
      trig_functs(n+k)=sin(angle)
   end do

   !if (trace_use) call da_trace_exit("da_find_fft_trig_funcs")

end subroutine da_find_fft_trig_funcs

end subroutine da_del2a_to_a

!DECK FFT551
!     SUBROUTINE 'FFT551' - MULTIPLE FAST COSINE TRANSFORM
!
!     AUTHOR: CLIVE TEMPERTON, MAY 1988
!     [ALL-FORTRAN VERSION: C.T., OCTOBER 1995]
!
!     COSINE TRANSFORM OF LENGTH N IS CONVERTED TO
!     REAL PERIODIC TRANSFORM OF LENGTH N BY PRE- AND POST-
!     PROCESSING. REAL PERIODIC TRANSFORM IS PERFORMED BY
!     PRUNING REDUNDANT OPERATIONS FROM COMPLEX TRANSFORM.
!
!     SEE FOR EXAMPLE PAUL SWARZTRAUBER, "SYMMETRIC FFT'S",
!     MATH. COMP. 47 (1986), 323-346.
!
!     A IS THE ARRAY CONTAINING INPUT & OUTPUT DATA
!     WORK IS AN AREA OF SIZE (N+1)*MIN(LOT,64)
!     TRIGS IS A PREVIOUSLY PREPARED LIST OF TRIG FUNCTION VALUES
!     IFAX IS A PREVIOUSLY PREPARED LIST OF FACTORS OF N
!     INC IS THE INCREMENT WITHIN EACH DATA 'VECTOR'
!         (E.G. INC=1 FOR CONSECUTIVELY STORED DATA)
!     JUMP IS THE INCREMENT BETWEEN THE START OF EACH DATA VECTOR
!     N+1 IS THE LENGTH OF THE DATA VECTORS
!        (WHICH INCLUDE NONZERO VALUES AT BOTH ENDPOINTS)
!     LOT IS THE NUMBER OF DATA VECTORS
!     ISIGN = +1 FOR TRANSFORM FROM SPECTRAL TO GRIDPOINT
!           = -1 FOR TRANSFORM FROM GRIDPOINT TO SPECTRAL
!
!     ORDERING OF COEFFICIENTS:   Z(0) , Z(1) , Z(2) , ... , Z(N)
!
!     ORDERING OF DATA:           X(0) , X(1) , X(2) , ... , X(N)
!
!     VECTORIZATION IS ACHIEVED ON CRAY BY DOING THE TRANSFORMS
!     IN PARALLEL
!
!     N MUST BE COMPOSED OF FACTORS 2,3 & 5 AND MUST BE EVEN
!
!     DEFINITION OF TRANSFORMS:
!     -------------------------
!
!     ISIGN=+1: X(I)=SUM(J=0,...,N)(E(J)*Z(J)*COS(I*J*PI/N))
!                    WHERE E(J)=0.5 FOR J=0,N --- ELSE E(J)=1
!
!     ISIGN=-1: Z(J)=(2/N)*SUM(I=0,...,N)(E(I)*X(I)*COS(I*J*PI/N))
!
! N.B.  FFT551 has an unusual definition of the FFTs,
!       such that the the coeff of wave0 is NOT the mean.
!
!---------------------------------------------------------------------
Subroutine FFT551 & ! in
 ( ISIGN,                & ! in
   INC,                  & ! in
   JUMP,                 & ! in
   LOT,                  & ! in
   N,                    & ! in
   IFAX,                 & ! in
   TRIGS,                & ! in
   A,                    & ! inout
   IDIM )                   ! in

! Code Description:  ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for 
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data 

 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array                                       !  vectors  (which include zeros 
                                           ! at the endpoints)
 Integer , intent (in)    :: IDIM           ! dimension workspace 

 Real :: WORK(IDIM)                      ! size (n+1)*min(lot,VectorLength)
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K, IC, J, LA, IGO, JA,JB,IA,IB
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: CO,S, t1,t2,si,scale, vectorlength

CHARACTER (LEN=*), PARAMETER :: RoutineName = "Var_FFT551"

      VectorLength = LOT
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      DO 200 NB=1,NBLOX
!
!     PREPROCESSING
!     -------------
      IA=ISTART
      IB=IA+NH*INC
      IC=IA+N*INC
      JA=1
      JB=NH+1
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 105 J=1,NVEX
         T1=0.5*(A(IA)+A(IC))
         T2=0.5*(A(IA)-A(IC))
         A(IA)=T1
         A(IC)=T2
         IA=IA+JUMP
         IC=IC+JUMP
  105    CONTINUE
      ELSE  
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA)=0.5*(A(IA)+A(IC))
         WORK(JB)=A(IB)
         A(IC)=0.5*(A(IA)-A(IC))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ENDIF
!
      DO 130 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      IC=ISTART+N*INC
      SI=TRIGS(2*N+K)
      CO=TRIGS(2*N+NH-K)
      IF (MOD(NFAX,2).EQ.1) THEN
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         T2 = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
  115    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 120 J=1,NVEX
         WORK(JA) = 0.5*(A(IA)+A(IB)) - SI*(A(IA)-A(IB))
         WORK(JB) = 0.5*(A(IA)+A(IB)) + SI*(A(IA)-A(IB))
         A(IC) = A(IC) + CO*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         IC=IC+JUMP
         JA=JA+NX
         JB=JB+NX
  120    CONTINUE
      ENDIF
  130 CONTINUE
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
      IGO=1-2*MOD(NFAX,2)
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL qpassm(WORK,WORK(IFAC*LA+1),A(IA),A(IA+LA*INC), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL qpassm(A(IA),A(IA+IFAC*LA*INC),WORK,WORK(LA+1), &
                    TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      ENDIF
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
  140 CONTINUE
!
!     POSTPROCESSING
!     --------------
      SCALE=2.0
      IF (ISIGN.EQ.+1) SCALE = FLOAT(N)
      S=1.0
      IF (ISIGN.EQ.-1) S = 2.0/FLOAT(N)
      JA=ISTART
      JB=JA+N*INC
      IA=1
      IB=N
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=S*A(JB)
      A(JB)=SCALE*WORK(IB)
      IA=IA+NX
      IB=IB+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=SCALE*WORK(IA)
      A(JA+INC)=-SCALE*WORK(IA+1)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE
      GO TO 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(UNIT=0,FMT='(A,I4,A)') &
        'VECTOR LENGTH =',NVEX,', GREATER THAN VectorLength'
      GO TO 570
  530 CONTINUE
      WRITE(UNIT=0,FMT='(A,I3,A)') &
        'FACTOR =',IFAC,', NOT CATERED FOR'
      GO TO 570
  550 CONTINUE
      WRITE(UNIT=0,FMT='(A,I3,A)') &
        'FACTOR =',IFAC,', ONLY CATERED FOR IF LA*IFAC=N'
  570 CONTINUE

      RETURN
      END SUBROUTINE FFT551


Subroutine FFT661  & ! in
 ( ISIGN,              & ! in
   INC,                & ! in
   JUMP,               & ! in
   LOT,                & ! in
   N,                  & ! in
   IFAX,               & ! in
   TRIGS,              & ! in
   A,                  & ! inout
   DIM )                 ! in
!
!
! Description:
!     MULTIPLE FAST SINE TRANSFORM
!     (Originally called FFT661, then Var_SinTrans)
!      author: clive temperton, may 1988 
!       (slightly modified for all-fortran version)
!
!     Sine transform of length n is converted to
!     Real periodic transform of length n by pre- and post-
!     processing. Real periodic transform is performed by
!     pruning redundant operations from complex transform.
!
!     see for example paul swarztrauber, "symmetric fft's",
!     math. comp. 47 (1986), 323-346.
!
! Method:
!
!     ordering of coefficients:   z(0) , z(1) , z(2) , ... , z(n)
!     ordering of data:           x(0) , x(1) , x(2) , ... , x(n)
!
!    vectorization is achieved on cray by doing the transforms
!    in parallel
!
!    N must be composed of factors 2,3 & 5 and must be even
!
!    definition of transforms:
!     -------------------------
!
!     isign=+1: x(i)=sum(j=1,...,n-1)(z(j)*sin(i*j*pi/n))
!
!     isign=-1: z(j)=(2/n)*sum(i=1,...,n-1)(x(i)*sin(i*j*pi/n))
!
! Current Code Owner: Andrew Lorenc
!
!   History:
! Version   Date     Comment
! -------   ----     -------
! 0.1       14/12/93 Original code. Phil Andrews
! 0.2       16/09/94 Small Modifications for the
!                    incorporation in the VAR project. HB
! 1.1       21/04/95 placed under control. JB
! 1.2       01/06/95 Tracing added. JB
!
! Code Description:
!    NB   BECAUSE OF THE TRICKY NESTED LOOPS 
!         ORIGINAL CODE F77 IS HARDLY TOUCHED !!!

Implicit none

! Subroutine arguments
 Integer , intent (in)    :: ISIGN         ! Switch forward (-1) or inverse (+1)
 Integer , intent (in)    :: INC           ! increment within each data
                                           ! vector  (e.g. INC=1 for 
                                           ! consecutively stored data)
 Integer , intent (in)    :: Jump          ! increment between start of
                                           ! data vectors
 Integer , intent (in)    :: LOT           ! Number of data vectors
 Integer , intent (in)    :: N             ! N+1 is the length of the data 
                                           !  vectors  (which include zeros 
                                           ! at the endpoints)
 Integer , intent (in)    :: DIM           ! dimension workspace 
 Integer , intent (in)    :: IFAX(10)      ! previously prepared list of 
                                           ! factors of N
 
 Real    , intent (in)    :: TRIGS(3*N)    ! previously prepared list of 
                                           ! trigonometric function values
 Real    , intent (inout) :: A( INC*(N+1) + JUMP*(LOT-1) ) ! data array

                                                    ! No descriptions given
 Integer                  :: NFAX,NX,NH
 Integer                  :: NBLOX,NVEX,NB
 Integer                  :: K,JA,JB,IA,IB,IGO,LA,J
 Integer                  :: IFAC,IERR,ISTART

 Real                     :: SI,T1,T2,SCALE, vectorlength
 Real                     :: WORK(DIM)     ! size (n+1)*min(lot,VectorLength)
     
      VectorLength = LOT
      NFAX=IFAX(1)
      NX=N+1
      NH=N/2
      NBLOX=1+(LOT-1)/VectorLength
      NVEX=LOT-(NBLOX-1)*VectorLength
      ISTART=1
!
      DO 200 NB=1,NBLOX
!
!     PREPROCESSING
!     -------------
      DO 120 K=1,NH-1
      JA=K+1
      JB=N+1-K
      IA=ISTART+K*INC
      IB=ISTART+(JB-1)*INC
      SI=TRIGS(2*N+K)
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 110 J=1,NVEX
         WORK(JA) = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         WORK(JB) = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         IA=IA+JUMP
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  110    CONTINUE
      ELSE
!DIR$ IVDEP
         DO 115 J=1,NVEX
         T1 = SI*(A(IA)+A(IB)) + 0.5*(A(IA)-A(IB))
         T2 = SI*(A(IA)+A(IB)) - 0.5*(A(IA)-A(IB))
         A(IA) = T1
         A(IB) = T2
         IA=IA+JUMP
         IB=IB+JUMP
  115    CONTINUE
      ENDIF
  120 CONTINUE

      JA=1
      JB=NH+1
      IA=ISTART
      IB=ISTART+NH*INC
      IF (MOD(NFAX,2).EQ.0) THEN
!DIR$ IVDEP
         DO 130 J=1,NVEX
         WORK(JA)=0.0
         WORK(JB)=2.0*A(IB)
         IB=IB+JUMP
         JA=JA+NX
         JB=JB+NX
  130    CONTINUE
         IGO = +1
      ELSE
!DIR$ IVDEP
         DO 135 J=1,NVEX
         A(IA)=0.0
         A(IB)=2.0*A(IB)
         IA=IA+JUMP
         IB=IB+JUMP
  135    CONTINUE
         IGO = -1
      ENDIF
!
!     PERIODIC FOURIER ANALYSIS
!     -------------------------
      IA=ISTART
      LA=N
!
      DO 140 K=1,NFAX
      IFAC=IFAX(NFAX+2-K)
      LA=LA/IFAC
      IERR=-1
      IF (IGO.EQ.+1) THEN
        CALL qpassm(WORK,WORK(IFAC*LA+1),A(IA),A(LA*INC+IA), &
                    TRIGS,1,INC,NX,JUMP,NVEX,N,IFAC,LA,IERR)
      ELSE IF (IGO.EQ.-1) THEN
        CALL qpassm(A(IA),A(IFAC*LA*INC+IA),WORK,WORK(LA+1), &
                    TRIGS,INC,1,JUMP,NX,NVEX,N,IFAC,LA,IERR)
      ENDIF
      IF (IERR.NE.0) GO TO 500
      IGO=-IGO
  140 CONTINUE
!
!     POSTPROCESSING
!     --------------
      SCALE=2.0
      IF (ISIGN.EQ.+1) SCALE = FLOAT(N)
      JA=ISTART
      JB=JA+N*INC
      IA=1
!DIR$ IVDEP
      DO 150 J=1,NVEX
      A(JA)=0.0
      A(JA+INC)=0.5*SCALE*WORK(IA)
      A(JB)=0.0
      IA=IA+NX
      JA=JA+JUMP
      JB=JB+JUMP
  150 CONTINUE
!
      DO 170 K=2,N-2,2
      JA=ISTART+K*INC
      IA=K
!DIR$ IVDEP
      DO 160 J=1,NVEX
      A(JA)=-SCALE*WORK(IA+1)
      A(JA+INC)=SCALE*WORK(IA)+A(JA-INC)
      IA=IA+NX
      JA=JA+JUMP
  160 CONTINUE
  170 CONTINUE
!
      ISTART=ISTART+NVEX*JUMP
      NVEX=VectorLength
  200 CONTINUE

      Go To 570
!
!     ERROR MESSAGES
!     --------------
  500 CONTINUE
      GO TO (510,530,550) IERR
  510 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'NVEX=', NVEX ,' GREATER THAN VectorLength'
      GO TO 570
  530 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'IFAC=', IFAC, 'NOT CATERED FOR'
      GO TO 570
  550 CONTINUE
      WRITE(UNIT=0,FMT='(A,I5,A)') 'IFAC=', IFAC, ' ONLY CATERED FOR IF LA*IFAC=N'
  570 CONTINUE


      RETURN


End subroutine FFT661


!C     SUBROUTINE 'QPASSM' - PERFORMS ONE PASS THROUGH DATA AS PART!C     OF MULTIPLE REAL FFT (FOURIER ANALYSIS) ROUTINE
!C
!C     A IS FIRST REAL INPUT VECTOR
!C         EQUIVALENCE B(1) WITH A(IFAC*LA*INC1+1)
!C     C IS FIRST REAL OUTPUT VECTOR
!C         EQUIVALENCE D(1) WITH C(LA*INC2+1)
!C     TRIGS IS A PRECALCULATED LIST OF SINES & COSINES
!C     INC1 IS THE ADDRESSING INCREMENT FOR A
!C     INC2 IS THE ADDRESSING INCREMENT FOR C
!C     INC3 IS THE INCREMENT BETWEEN INPUT VECTORS A
!C     INC4 IS THE INCREMENT BETWEEN OUTPUT VECTORS C
!C     LOT IS THE NUMBER OF VECTORS
!C     N IS THE LENGTH OF THE VECTORS
!C     IFAC IS THE CURRENT FACTOR OF N
!C     LA = N/(PRODUCT OF FACTORS USED SO FAR)
!C     IERR IS AN ERROR INDICATOR:
!C              0 - PASS COMPLETED WITHOUT ERROR
!C              1 - LOT GREATER THAN VectorLength
!C              2 - IFAC NOT CATERED FOR
!C              3 - IFAC ONLY CATERED FOR IF LA=N/IFAC
!C
!C-----------------------------------------------------------------------
!C
     SUBROUTINE QPASSM(A,B,C,D,TRIGS,INC1,INC2,INC3,INC4,LOT,N,IFAC,LA,IERR)

      INTEGER :: inc1, inc2, inc3, inc4, lot, n, ifac, la, ierr

      REAL  :: a, b, c, d, trigs
      DIMENSION A(*),B(*),C(*),D(*),TRIGS(N)
! Local named constants
 CHARACTER (LEN=*), PARAMETER :: RoutineName = "QPASSM"
!
      REAL  :: SIN36, SIN72, QRT5, SIN60
      DATA SIN36/0.587785252292473/,SIN72/0.951056516295154/, &
          QRT5/0.559016994374947/,SIN60/0.866025403784437/

      REAL    :: s1, s2, s3, s4, s5
      REAL    :: sin45, zsin36, zsin72, zqrt5, zsin60, zsin45, z
      REAL    :: a0, a1, a2, a3, a4, a5, a6, a10, a11, a20, a21
      REAL    :: b0, b1, b2, b3, b4, b5, b6, b10, b11, b20, b21
!      REAL    :: c0, c1, c2, c3, c4, c5
      REAL    :: c1, c2, c3, c4, c5
      INTEGER :: i, ijk, l, k, kb, m, iink, jink, ijump, kstop
      INTEGER :: ibad, igo, ia, ie, je, ibase, jbase, ja, jb, j, ic
      INTEGER :: if, jf, kf, ib, jc, kc, id, jd, kd, ke, ig, ih
      INTEGER :: vectorlength
!
!- End of header ---------------------------------------------------------------


      M=N/IFAC
      IINK=LA*INC1
      JINK=LA*INC2
      IJUMP=(IFAC-1)*IINK
      KSTOP=(N-IFAC)/(2*IFAC)
!
      IBAD=1
      VectorLength = lot
      IF (LOT.GT.VectorLength) GO TO 910
      IBASE=0
      JBASE=0
      IGO=IFAC-1
      IF (IGO.EQ.7) IGO=6
      IBAD=2
      IF (IGO.LT.1.OR.IGO.GT.6) GO TO 910
      GO TO (200,300,400,500,600,800),IGO
!
!     CODING FOR FACTOR 2
!     -------------------
  200 CONTINUE
      IA=1
      IB=IA+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
!
      IF (LA.EQ.M) GO TO 290
!
      DO 220 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 210 IJK=1,LOT
      C(JA+J)=A(IA+I)+A(IB+I)
      C(JB+J)=A(IA+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  210 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  220 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JB) GO TO 260
      DO 250 K=LA,KSTOP,LA
      KB=K+K
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      JBASE=0
      DO 240 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 230 IJK=1,LOT
      C(JA+J)=A(IA+I)+(C1*A(IB+I)+S1*B(IB+I))
      C(JB+J)=A(IA+I)-(C1*A(IB+I)+S1*B(IB+I))
      D(JA+J)=(C1*B(IB+I)-S1*A(IB+I))+B(IA+I)
      D(JB+J)=(C1*B(IB+I)-S1*A(IB+I))-B(IA+I)
      I=I+INC3
      J=J+INC4
  230 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  240 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB-JINK
  250 CONTINUE
      IF (JA.GT.JB) GO TO 900
  260 CONTINUE
      JBASE=0
      DO 280 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 270 IJK=1,LOT
      C(JA+J)=A(IA+I)
      D(JA+J)=-A(IB+I)
      I=I+INC3
      J=J+INC4
  270 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  280 CONTINUE
      GO TO 900
!
  290 CONTINUE
      Z=1.0/FLOAT(N)
      DO 294 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 292 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+A(IB+I))
      C(JB+J)=Z*(A(IA+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  292 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  294 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 3
!     -------------------
  300 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB
!
      IF (LA.EQ.M) GO TO 390
!
      DO 320 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 310 IJK=1,LOT
      C(JA+J)=A(IA+I)+(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-0.5*(A(IB+I)+A(IC+I))
      D(JB+J)=SIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  310 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  320 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JA.EQ.JC) GO TO 360
      DO 350 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      JBASE=0
      DO 340 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 330 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C2*A(IC+I)+S2*B(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C2*B(IC+I)-S2*A(IC+I))
      A2=A(IA+I)-0.5*A1
      B2=B(IA+I)-0.5*B1
      A3=SIN60*((C1*A(IB+I)+S1*B(IB+I))-(C2*A(IC+I)+S2*B(IC+I)))
      B3=SIN60*((C1*B(IB+I)-S1*A(IB+I))-(C2*B(IC+I)-S2*A(IC+I)))
      C(JA+J)=A(IA+I)+A1
      D(JA+J)=B(IA+I)+B1
      C(JB+J)=A2+B3
      D(JB+J)=B2-A3
      C(JC+J)=A2-B3
      D(JC+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  330 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  340 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
  350 CONTINUE
      IF (JA.GT.JC) GO TO 900
  360 CONTINUE
      JBASE=0
      DO 380 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 370 IJK=1,LOT
      C(JA+J)=A(IA+I)+0.5*(A(IB+I)-A(IC+I))
      D(JA+J)=-SIN60*(A(IB+I)+A(IC+I))
      C(JB+J)=A(IA+I)-(A(IB+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  370 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  380 CONTINUE
      GO TO 900
!
  390 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 394 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 392 IJK=1,LOT
      C(JA+J)=Z*(A(IA+I)+(A(IB+I)+A(IC+I)))
      C(JB+J)=Z*(A(IA+I)-0.5*(A(IB+I)+A(IC+I)))
      D(JB+J)=ZSIN60*(A(IC+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  392 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  394 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 4
!     -------------------
  400 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JB
!
      IF (LA.EQ.M) GO TO 490
!
      DO 420 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 410 IJK=1,LOT
      C(JA+J)=(A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I))
      C(JC+J)=(A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I))
      C(JB+J)=A(IA+I)-A(IC+I)
      D(JB+J)=A(ID+I)-A(IB+I)
      I=I+INC3
      J=J+INC4
  410 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  420 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JC) GO TO 460
      DO 450 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      JBASE=0
      DO 440 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 430 IJK=1,LOT
      A0=A(IA+I)+(C2*A(IC+I)+S2*B(IC+I))
      A2=A(IA+I)-(C2*A(IC+I)+S2*B(IC+I))
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C3*A(ID+I)+S3*B(ID+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C3*A(ID+I)+S3*B(ID+I))
      B0=B(IA+I)+(C2*B(IC+I)-S2*A(IC+I))
      B2=B(IA+I)-(C2*B(IC+I)-S2*A(IC+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C3*B(ID+I)-S3*A(ID+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C3*B(ID+I)-S3*A(ID+I))
      C(JA+J)=A0+A1
      C(JC+J)=A0-A1
      D(JA+J)=B0+B1
      D(JC+J)=B1-B0
      C(JB+J)=A2+B3
      C(JD+J)=A2-B3
      D(JB+J)=B2-A3
      D(JD+J)=-(B2+A3)
      I=I+INC3
      J=J+INC4
  430 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  440 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC-JINK
      JD=JD-JINK
  450 CONTINUE
      IF (JB.GT.JC) GO TO 900
  460 CONTINUE
      SIN45=SQRT(0.5)
      JBASE=0
      DO 480 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 470 IJK=1,LOT
      C(JA+J)=A(IA+I)+SIN45*(A(IB+I)-A(ID+I))
      C(JB+J)=A(IA+I)-SIN45*(A(IB+I)-A(ID+I))
      D(JA+J)=-A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      D(JB+J)=A(IC+I)-SIN45*(A(IB+I)+A(ID+I))
      I=I+INC3
      J=J+INC4
  470 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  480 CONTINUE
      GO TO 900
!
  490 CONTINUE
      Z=1.0/FLOAT(N)
      DO 494 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 492 IJK=1,LOT
      C(JA+J)=Z*((A(IA+I)+A(IC+I))+(A(IB+I)+A(ID+I)))
      C(JC+J)=Z*((A(IA+I)+A(IC+I))-(A(IB+I)+A(ID+I)))
      C(JB+J)=Z*(A(IA+I)-A(IC+I))
      D(JB+J)=Z*(A(ID+I)-A(IB+I))
      I=I+INC3
      J=J+INC4
  492 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  494 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 5
!     -------------------
  500 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC
      JE=JB
!
      IF (LA.EQ.M) GO TO 590
!
      DO 520 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 510 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-SIN72*A3-SIN36*A4
      D(JC+J)=-SIN36*A3+SIN72*A4
      I=I+INC3
      J=J+INC4
  510 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  520 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JB.EQ.JD) GO TO 560
      DO 550 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      JBASE=0
      DO 540 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 530 IJK=1,LOT
      A1=(C1*A(IB+I)+S1*B(IB+I))+(C4*A(IE+I)+S4*B(IE+I))
      A3=(C1*A(IB+I)+S1*B(IB+I))-(C4*A(IE+I)+S4*B(IE+I))
      A2=(C2*A(IC+I)+S2*B(IC+I))+(C3*A(ID+I)+S3*B(ID+I))
      A4=(C2*A(IC+I)+S2*B(IC+I))-(C3*A(ID+I)+S3*B(ID+I))
      B1=(C1*B(IB+I)-S1*A(IB+I))+(C4*B(IE+I)-S4*A(IE+I))
      B3=(C1*B(IB+I)-S1*A(IB+I))-(C4*B(IE+I)-S4*A(IE+I))
      B2=(C2*B(IC+I)-S2*A(IC+I))+(C3*B(ID+I)-S3*A(ID+I))
      B4=(C2*B(IC+I)-S2*A(IC+I))-(C3*B(ID+I)-S3*A(ID+I))
      A5=A(IA+I)-0.25*(A1+A2)
      A6=QRT5*(A1-A2)
      B5=B(IA+I)-0.25*(B1+B2)
      B6=QRT5*(B1-B2)
      A10=A5+A6
      A20=A5-A6
      B10=B5+B6
      B20=B5-B6
      A11=SIN72*B3+SIN36*B4
      A21=SIN36*B3-SIN72*B4
      B11=SIN72*A3+SIN36*A4
      B21=SIN36*A3-SIN72*A4
      C(JA+J)=A(IA+I)+(A1+A2)
      C(JB+J)=A10+A11
      C(JE+J)=A10-A11
      C(JC+J)=A20+A21
      C(JD+J)=A20-A21
      D(JA+J)=B(IA+I)+(B1+B2)
      D(JB+J)=B10-B11
      D(JE+J)=-(B10+B11)
      D(JC+J)=B20-B21
      D(JD+J)=-(B20+B21)
      I=I+INC3
      J=J+INC4
  530 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  540 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
  550 CONTINUE
      IF (JB.GT.JD) GO TO 900
  560 CONTINUE
      JBASE=0
      DO 580 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 570 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=A(IA+I)+0.25*(A3-A4)
      A6=QRT5*(A3+A4)
      C(JA+J)=A5+A6
      C(JB+J)=A5-A6
      C(JC+J)=A(IA+I)-(A3-A4)
      D(JA+J)=-SIN36*A1-SIN72*A2
      D(JB+J)=-SIN72*A1+SIN36*A2
      I=I+INC3
      J=J+INC4
  570 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  580 CONTINUE
      GO TO 900
!
  590 CONTINUE
      Z=1.0/FLOAT(N)
      ZQRT5=Z*QRT5
      ZSIN36=Z*SIN36
      ZSIN72=Z*SIN72
      DO 594 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 592 IJK=1,LOT
      A1=A(IB+I)+A(IE+I)
      A3=A(IB+I)-A(IE+I)
      A2=A(IC+I)+A(ID+I)
      A4=A(IC+I)-A(ID+I)
      A5=Z*(A(IA+I)-0.25*(A1+A2))
      A6=ZQRT5*(A1-A2)
      C(JA+J)=Z*(A(IA+I)+(A1+A2))
      C(JB+J)=A5+A6
      C(JC+J)=A5-A6
      D(JB+J)=-ZSIN72*A3-ZSIN36*A4
      D(JC+J)=-ZSIN36*A3+ZSIN72*A4
      I=I+INC3
      J=J+INC4
  592 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  594 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 6
!     -------------------
  600 CONTINUE
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      JA=1
      JB=JA+(2*M-LA)*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JC
      JF=JB
!
      IF (LA.EQ.M) GO TO 690
!
      DO 620 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 610 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=(A(IA+I)+A(ID+I))+A11
      C(JC+J)=(A(IA+I)+A(ID+I)-0.5*A11)
      D(JC+J)=SIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=(A(IA+I)-A(ID+I))-0.5*A11
      D(JB+J)=SIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=(A(IA+I)-A(ID+I))+A11
      I=I+INC3
      J=J+INC4
  610 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  620 CONTINUE
      JA=JA+JINK
      JINK=2*JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
      IBASE=IBASE+IJUMP
      IJUMP=2*IJUMP+IINK
      IF (JC.EQ.JD) GO TO 660
      DO 650 K=LA,KSTOP,LA
      KB=K+K
      KC=KB+KB
      KD=KC+KB
      KE=KD+KB
      KF=KE+KB
      C1=TRIGS(KB+1)
      S1=TRIGS(KB+2)
      C2=TRIGS(KC+1)
      S2=TRIGS(KC+2)
      C3=TRIGS(KD+1)
      S3=TRIGS(KD+2)
      C4=TRIGS(KE+1)
      S4=TRIGS(KE+2)
      C5=TRIGS(KF+1)
      S5=TRIGS(KF+2)
      JBASE=0
      DO 640 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 630 IJK=1,LOT
      A1=C1*A(IB+I)+S1*B(IB+I)
      B1=C1*B(IB+I)-S1*A(IB+I)
      A2=C2*A(IC+I)+S2*B(IC+I)
      B2=C2*B(IC+I)-S2*A(IC+I)
      A3=C3*A(ID+I)+S3*B(ID+I)
      B3=C3*B(ID+I)-S3*A(ID+I)
      A4=C4*A(IE+I)+S4*B(IE+I)
      B4=C4*B(IE+I)-S4*A(IE+I)
      A5=C5*A(IF+I)+S5*B(IF+I)
      B5=C5*B(IF+I)-S5*A(IF+I)
      A11=(A2+A5)+(A1+A4)
      A20=(A(IA+I)+A3)-0.5*A11
      A21=SIN60*((A2+A5)-(A1+A4))
      B11=(B2+B5)+(B1+B4)
      B20=(B(IA+I)+B3)-0.5*B11
      B21=SIN60*((B2+B5)-(B1+B4))
      C(JA+J)=(A(IA+I)+A3)+A11
      D(JA+J)=(B(IA+I)+B3)+B11
      C(JC+J)=A20-B21
      D(JC+J)=A21+B20
      C(JE+J)=A20+B21
      D(JE+J)=A21-B20
      A11=(A2-A5)+(A4-A1)
      A20=(A(IA+I)-A3)-0.5*A11
      A21=SIN60*((A4-A1)-(A2-A5))
      B11=(B5-B2)-(B4-B1)
      B20=(B3-B(IA+I))-0.5*B11
      B21=SIN60*((B5-B2)+(B4-B1))
      C(JB+J)=A20-B21
      D(JB+J)=A21-B20
      C(JD+J)=A11+(A(IA+I)-A3)
      D(JD+J)=B11+(B3-B(IA+I))
      C(JF+J)=A20+B21
      D(JF+J)=A21+B20
      I=I+INC3
      J=J+INC4
  630 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  640 CONTINUE
      IBASE=IBASE+IJUMP
      JA=JA+JINK
      JB=JB+JINK
      JC=JC+JINK
      JD=JD-JINK
      JE=JE-JINK
      JF=JF-JINK
  650 CONTINUE
      IF (JC.GT.JD) GO TO 900
  660 CONTINUE
      JBASE=0
      DO 680 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 670 IJK=1,LOT
      C(JA+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))+ SIN60*(A(IB+I)-A(IF+I))
      D(JA+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))-SIN60*(A(IC+I)+A(IE+I))
      C(JB+J)=A(IA+I)-(A(IC+I)-A(IE+I))
      D(JB+J)=A(ID+I)-(A(IB+I)+A(IF+I))
      C(JC+J)=(A(IA+I)+0.5*(A(IC+I)-A(IE+I)))-SIN60*(A(IB+I)-A(IF+I))
      D(JC+J)=-(A(ID+I)+0.5*(A(IB+I)+A(IF+I)))+SIN60*(A(IC+I)+A(IE+I))
      I=I+INC3
      J=J+INC4
  670 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  680 CONTINUE
      GO TO 900
!
  690 CONTINUE
      Z=1.0/FLOAT(N)
      ZSIN60=Z*SIN60
      DO 694 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 692 IJK=1,LOT
      A11=(A(IC+I)+A(IF+I))+(A(IB+I)+A(IE+I))
      C(JA+J)=Z*((A(IA+I)+A(ID+I))+A11)
      C(JC+J)=Z*((A(IA+I)+A(ID+I))-0.5*A11)
      D(JC+J)=ZSIN60*((A(IC+I)+A(IF+I))-(A(IB+I)+A(IE+I)))
      A11=(A(IC+I)-A(IF+I))+(A(IE+I)-A(IB+I))
      C(JB+J)=Z*((A(IA+I)-A(ID+I))-0.5*A11)
      D(JB+J)=ZSIN60*((A(IE+I)-A(IB+I))-(A(IC+I)-A(IF+I)))
      C(JD+J)=Z*((A(IA+I)-A(ID+I))+A11)
      I=I+INC3
      J=J+INC4
  692 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  694 CONTINUE
      GO TO 900
!
!     CODING FOR FACTOR 8
!     -------------------
  800 CONTINUE
      IBAD=3
      IF (LA.NE.M) GO TO 910
      IA=1
      IB=IA+IINK
      IC=IB+IINK
      ID=IC+IINK
      IE=ID+IINK
      IF=IE+IINK
      IG=IF+IINK
      IH=IG+IINK
      JA=1
      JB=JA+LA*INC2
      JC=JB+2*M*INC2
      JD=JC+2*M*INC2
      JE=JD+2*M*INC2
      Z=1.0/FLOAT(N)
      ZSIN45=Z*SQRT(0.5)
!
      DO 820 L=1,LA
      I=IBASE
      J=JBASE
!DIR$ IVDEP
      DO 810 IJK=1,LOT
      C(JA+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))+ &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JE+J)=Z*(((A(IA+I)+A(IE+I))+(A(IC+I)+A(IG+I)))- &
         ((A(ID+I)+A(IH+I))+(A(IB+I)+A(IF+I))))
      C(JC+J)=Z*((A(IA+I)+A(IE+I))-(A(IC+I)+A(IG+I)))
      D(JC+J)=Z*((A(ID+I)+A(IH+I))-(A(IB+I)+A(IF+I)))
      C(JB+J)=Z*(A(IA+I)-A(IE+I))+ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      C(JD+J)=Z*(A(IA+I)-A(IE+I))-ZSIN45*((A(IH+I)-A(ID+I))-(A(IF+I)-A(IB+I)))
      D(JB+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))+Z*(A(IG+I)-A(IC+I))
      D(JD+J)=ZSIN45*((A(IH+I)-A(ID+I))+(A(IF+I)-A(IB+I)))-Z*(A(IG+I)-A(IC+I))
      I=I+INC3
      J=J+INC4
  810 CONTINUE
      IBASE=IBASE+INC1
      JBASE=JBASE+INC2
  820 CONTINUE
!

  900 CONTINUE
      IBAD=0
  910 CONTINUE
      IERR=IBAD

      RETURN
      END SUBROUTINE QPASSM

