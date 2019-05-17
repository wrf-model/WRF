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
   real,    parameter :: gas_constant_v = 461.6
   real,    parameter :: rd_over_rv = gas_constant/gas_constant_v
   real,    parameter :: rd_over_rv1 = 1.0 - rd_over_rv

   namelist /gen_be_nml/ nc_list_file, be_method, varnames, outnames, &
      do_pert_calc, do_eof_transform, do_slen_calc, slen_opt, stride, yr_cutoff, &
      verbose, level_start, level_end, aux_file, write_be_dat_r8
   namelist /ens_nml/ nens, write_ep, write_ens_stdv, ep_format, nproc_out

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
   integer :: nens                   ! set in namelist for ENS method. hard-coded to 2 for NMC method.
   logical :: write_ep               ! if writing out ensemble perturbations
   logical :: write_ens_stdv         ! if writing out stdv of ensemble perturbations
   integer :: ep_format              !  1: each member in separate file (real*8)
                                     ! 11: each member in separate file (real*4)
                                     !  2: all members in one file (full-domain) (real*4)
                                     !  3: all members in one file (sub-domain), need to specify nproc_out (real*4)
   integer :: nproc_out              ! number of processors to decompose for ep_format=3

   integer :: level_start, level_end ! for do_slen_calc=true. Can be set to be other than 1 and nvert for quick debugging/testing
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

   logical, allocatable :: read_it(:)
   integer, allocatable :: var_dim(:)

   logical :: read_t
   logical :: read_qv
   logical :: read_prs
   logical :: read_ght = .false.

   integer              :: nk_2d = 1
   integer              :: sl_unit = 77
   integer              :: be_unit = 88
   integer              :: nml_unit = 81
   integer              :: bin_type = 5
   integer              :: num_bins
   integer              :: num_bins2d
   integer, allocatable :: bin(:,:,:)
   integer, allocatable :: bin2d(:,:)

   integer :: ii, i, j, n, k, ierr
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

   real, allocatable :: mapfac_x(:,:)
   real, allocatable :: mapfac_y(:,:)

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
   aux_file = 'none'
   write_be_dat_r8 = .true.

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
      allocate (read_it(nvar))
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

   nvar_avail = count(read_it(:))
   if ( nvar_avail == 0 ) then
      call error_handler(-1, 'no valid variables found from the varnames in namelist.gen_be_v3')
   end if

   if ( myproc == root ) then
      write(stdout,*) 'ncase, nens = ', ncase, nens
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

   if ( do_slen_calc .and. slen_opt==2 ) then
      allocate(mapfac_x(ni,nj))
      allocate(mapfac_y(ni,nj))
      if ( trim(aux_file) == 'none' ) then
         ! read MAPFAC_MX and MAPFAC_MY from file filenames(1,1)
         aux_file = filenames(1,1)
      end if
      call read_fixed_fields(trim(aux_file))
   end if

   if ( do_pert_calc ) then
      call compute_pert
   end if

   call ext_ncd_ioexit(ierr)

   if ( do_slen_calc ) then
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
      call compute_bv_sl
   end if

   if ( do_slen_calc ) then
      allocate(sl_smth(nk))
      if ( write_be_dat_r8 ) then
         allocate(r8tmp1d(1:nk))
         allocate(r8tmp2d(1:nk,1:nk))
         allocate(r8tmp2d2(1:nk,1:num_bins2d))
         allocate(r8tmp3d(1:nk,1:nk,1:num_bins2d))
      end if
      do i = 1, nvar
         if ( .not. read_it(i) ) cycle
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

   if ( allocated(filenames) ) deallocate(filenames)
   if ( allocated(filedates) ) deallocate(filedates)
   if ( allocated(read_it) ) deallocate(read_it)
   if ( allocated(var_dim) ) deallocate(var_dim)

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
      allocate (ilist_file(nens,nfile))
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
         do i = 1, nvar
            call ext_ncd_get_var_info (fid, trim(varnames(i)), ndim, ordering, staggering, &
                                       start_index, end_index, wrftype, ierr)
            var_dim(i) = ndim
            if ( ierr == 0 ) then
               read_it(i) = .true.
               if ( trim(varnames(i)) == 'T' ) then
                  read_prs = .true.
               end if
            else
               if ( trim(varnames(i)) == 'RH' ) then
                  read_t   = .true.
                  read_qv  = .true.
                  read_prs = .true.
                  read_it(i) = .true.
                  var_dim(i) = 3
               else
                  read_it(i) = .false.
                  if ( myproc == root ) write(stdout,'(3a)') &
                     ' Warning: ', trim(varnames(i)), ' not available in the input file, skipping it'
               end if
            end if
         end do
      end if

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
         else if ( icount > 1 ) then
            find_match_loop_ens: do ii = ifile-1, 1, -1
               if ( valid(ii) ) then
                  if ( file_init_dates(ifile) == file_init_dates(ii) ) then
                     if ( file_valid_dates(ifile) == file_valid_dates(ii) ) then
                        iens = iens + 1
                        ilist_file(iens,icase)   = ifile
                        if ( ii == 1 ) ilist_file(iens-1,icase) = ii
                        exit find_match_loop_ens
                     end if
                  else
                     icase = icase + 1
                  end if
               end if
            end do find_match_loop_ens
         end if
      end if
   end do file_loop1

   ncase = icase

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

   integer, allocatable :: istart_ens(:), iend_ens(:)
   integer, allocatable :: istart_case(:), iend_case(:)
   integer, allocatable :: istart_var(:), iend_var(:)
   integer, allocatable :: iproc_var(:)

   integer :: ens_istart, ens_iend
   integer :: case_istart, case_iend
   integer :: ivs, ive, ie_indx
   integer :: my_nvar, dest_proc

   real(r_single), allocatable :: xfield(:,:,:)
   real(r_single), allocatable :: xfield_u(:,:,:)
   real(r_single), allocatable :: xfield_v(:,:,:)
   real(r_single), allocatable :: xfield_w(:,:,:)

   real, allocatable :: prs(:,:,:)    ! prs on half levels
   real, allocatable :: prs_w(:,:,:)  ! prs on full levels
   real, allocatable :: ght(:,:,:)
   real, allocatable :: theta(:,:,:)  ! potential temperature
   real, allocatable :: q(:,:,:)      ! qv mixing ratio
   real, allocatable :: mu(:,:)
   real, allocatable :: mub(:,:)
   real, allocatable :: znw(:)
   real    :: p_top
   real    :: tc, es, qs
   logical :: got_prs, got_th, got_qv

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

   if ( trim(be_method) == 'ENS' ) then
      case_istart = 1
      case_iend   = ncase
      ens_istart  = istart_ens(myproc)
      ens_iend    = iend_ens(myproc)
   else if ( trim(be_method) == 'NMC' ) then
      case_istart = istart_case(myproc)
      case_iend   = iend_case(myproc)
      ens_istart  = 1
      ens_iend    = nens
   end if

   write(stdout,'(a,i4,a,i4,a,i4)') &
      ' Proc ', myproc, ' will read  ens files ', ens_istart, ' - ', ens_iend
   write(stdout,'(a,i4,a,i4,a,i4)') &
      ' Proc ', myproc, ' will read case files ', case_istart, ' - ', case_iend

   if ( ens_iend >= ens_istart .and. case_iend >= case_istart ) then
      allocate(xdata(nvar,ens_istart:ens_iend,case_istart:case_iend), stat=ierr)
      call error_handler(ierr, 'allocating xdata in compute_pert')
   end if

   do ic = case_istart, case_iend
      do ie = ens_istart, ens_iend
         do iv = 1, nvar
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
                                    xfield_w(1,1,:), wrftype,       &
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

         var_loop1: do iv = 1, nvar

            if ( .not. read_it(iv) ) cycle var_loop1

            varname = trim(varnames(iv))
            call ext_ncd_get_var_info (fid, varname, ndim, ordering, staggering, &
                                       start_index, end_index, wrftype, ierr)
            !call error_handler(ierr, 'ext_ncd_get_var_info '//trim(varname))

            !write(stdout, '(a,i3,a,a8,a,a)') ' Proc ', myproc, ' Reading ', &
            !   trim(varname), ' from ', trim(input_file)

            if ( varname == 'PSFC' ) then
               if ( .not. allocated(xfield) ) allocate (xfield  (ni, nj, nk))
               call ext_ncd_read_field(fid, DateStr, varname,     &
                                       xfield(:,:,1), wrftype,    &
                                       0, 0, 0, ordering,         &
                                       staggering, dimnames,      & !dummy
                                       start_index, end_index,    & !dom
                                       start_index, end_index,    & !mem
                                       start_index, end_index,    & !pat
                                       ierr                   )
               call error_handler(ierr, 'ext_ncd_read_field '//trim(varname))
               xdata(iv,ie,ic)%value(:,:,1) = xfield(:,:,1)
            else if ( varname == 'U' ) then
               if ( .not. allocated(xfield_u) ) allocate (xfield_u(ni1,nj, nk))
               call ext_ncd_read_field(fid, DateStr, varname,     &
                                       xfield_u(:,:,:), wrftype,  &
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
                           0.5 * ( xfield_u(i,j,k) + xfield_u(i+1,j,k) )
                     end do
                  end do
               end do
            else if ( varname == 'V' ) then
               if ( .not. allocated(xfield_v) ) allocate (xfield_v(ni, nj1,nk))
               call ext_ncd_read_field(fid, DateStr, varname,     &
                                       xfield_v(:,:,:), wrftype,  &
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
                        tc = (theta(i,j,k)*(prs(i,j,k)/p00)**kappa) - t_kelvin
                        es = 611.2 * exp( 17.67 * tc / (tc + 243.5) )
                        qs = rd_over_rv * es /( prs(i,j,k) - rd_over_rv1 * es)
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
            else if ( varname == 'T' ) then
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

         call ext_ncd_ioclose(fid, ierr)

      end do ens_loop1 ! ensemble member loop

      write(stdout,'(a,i3,a,a)') ' Proc', myproc, &
         ' Computing perturbations for date ', filedates(1,ic)

      if ( trim(be_method) == 'NMC' ) then

         if ( remove_time_mean ) then
            ! allocate for summing up the cases each processor sees
            if ( .not. allocated(xsum_loc) )  then
               allocate(xsum_loc(ni,nj,nk,nvar), stat=ierr)
               call error_handler(ierr, 'allocating xsum_loc')
               xsum_loc = 0.0
            end if
         end if

         do iv = 1, nvar
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
         if ( .not. allocated(ens_mean) )    allocate(ens_mean(ni,nj,nk,nvar))

         if ( myproc == root ) write(stdout,'(a)') ' ====== Computing ensemble mean ======'

         var_loop2: do iv = 1, nvar
            if ( .not. read_it(iv) ) cycle var_loop2
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
         do iv = 1, nvar
            if ( .not. read_it(iv) ) cycle
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
               if ( .not. read_it(iv) ) cycle
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
               if ( .not. read_it(iv) ) cycle
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

            my_nvar = 0
            var_loop3: do iv = 1, nvar
               if ( .not. read_it(iv) ) cycle var_loop3

               ! initialize to zero for each variable
               if ( myproc == root ) globuf(:,:,:,:) = 0.0
               locbuf(:,:,:,:) = 0.0

               do n = istart_ens(myproc),iend_ens(myproc)
                  locbuf(:,:,:,n) = xdata(iv,n,ic)%value(:,:,:)
               end do

#ifdef DM_PARALLEL
               call mpi_reduce(locbuf,globuf,ijk*nens, &
                               mpi_real,mpi_sum,root,mpi_comm_world,ierr)
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
         if ( .not. allocated(time_mean) ) allocate(time_mean(ni,nj,nk,nvar))
#ifdef DM_PARALLEL
         if ( .not. allocated(xsum) )      allocate(xsum(ni,nj,nk,nvar))
         do iv = 1, nvar
            call mpi_allreduce(xsum_loc(:,:,:,iv),xsum(:,:,:,iv), ijk, &
                               mpi_real8, mpi_sum, mpi_comm_world,ierr)
            time_mean(:,:,:,iv) = xsum(:,:,:,iv)/real(ncase)
         end do
         deallocate(xsum)
#else
         do iv = 1, nvar
            time_mean(:,:,:,iv) = xsum_loc(:,:,:,iv)/real(ncase)
         end do
#endif
         do ic = case_istart, case_iend
            do iv = 1, nvar
               xdata(iv,1,ic)%value(:,:,:) = xdata(iv,1,ic)%value(:,:,:) - time_mean(:,:,:,iv)
            end do
         end do
         deallocate(time_mean)
         deallocate(xsum_loc)
      end if ! remove_time_mean

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

   deallocate (istart_ens)
   deallocate (iend_ens  )
   deallocate (istart_case)
   deallocate (iend_case  )

   do ic = case_istart, case_iend
      do ie = ens_istart, ens_iend
         do iv = 1, nvar
            deallocate(xdata(iv,ie,ic)%value)
         end do
      end do
   end do
   if ( allocated(xdata)) deallocate(xdata)

end subroutine compute_pert

subroutine compute_bv_sl

   !use da_lapack, only : dsyev
   implicit none

   character(len=filename_len)  :: filename                   ! Input filename.
   character(len=filename_len)  :: output_file
   integer             :: i, j, k, k1, k2, b         ! Loop counters.
   integer             :: ic, ie, iv, m
   integer :: istart_member, iend_member
   real                :: inv_nij                    ! 1 / (ni*nj).
   real                :: mean_field                 ! Mean field.
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: testing_eofs               ! True if testing EOF decomposition.
   logical             :: use_global_eofs            ! True if projected data uses global EOFs.

   integer, allocatable:: bin_pts2d(:)               ! Number of points in bin (2D fields).
   real, allocatable   :: field(:,:,:)               ! Input field.
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
   allocate( bv(1:nk,1:nk,1:num_bins2d) )
   field(:,:,:) = 0.0
   bv(:,:,:) = 0.0

   allocate( bin_pts2d(1:num_bins2d) )
   bin_pts2d(:) = 0

      allocate( work(1:nk,1:nk) )
      allocate( e_vec_loc(1:nk,1:nk,1:num_bins2d) )
      allocate( e_val_loc(1:nk,1:num_bins2d) )
      allocate( e_vec(1:nk,1:nk) )
      allocate( e_val(1:nk) )
      allocate( evec(1:nj,1:nk,1:nk) )
      allocate( eval(1:nj,1:nk) )

   if ( trim(be_method) == 'NMC' ) then
      istart_member = 1
      iend_member   = 1
   else if ( trim(be_method) == 'ENS' ) then
      istart_member = 1
      iend_member   = nens
   end if

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

   if ( do_eof_transform ) then

      if ( myproc == root ) write(stdout,'(4a)')'====== Computing vertical error eigenvalues, eigenvectors ======'

      var_loop: do iv = 1, nvar

         if ( .not. read_it(iv) ) cycle var_loop
         if ( myproc /= MOD((iv-1), num_procs) ) cycle var_loop

         write(stdout,'(a,i3,2a)') ' Proc', myproc, ' Processing vertical error stats for variable ', trim(varnames(iv))

         if ( var_dim(iv) == 2 ) then
            nkk = 1
         else if ( var_dim(iv) == 3 ) then
            nkk = nk
         end if

         do ic = 1, ncase
            do ie = istart_member, iend_member
               !write(stdout,'(5a,i4)')'    Processing data for date ', filedates(ie,ic), ', variable ', trim(varnames(iv)), &
               !                  ', member ', ie

               field(:,:,:) = xdata(iv,ie,ic)%value(:,:,:)

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

         !if ( myproc == root ) write(stdout,'(a)')' Projecting fields onto vertical modes'

         if ( var_dim(iv) == 3 ) then
            do ic = 1, ncase
               do ie = istart_member, iend_member
                  !write(stdout,'(5a,i4)')'    Date = ', filedates(ie,ic), ', variable ', trim(varnames(iv)), &
                  !                  ', member ', ie
                  ! Project fields onto vertical modes
                  ! Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform
                  do m = 1, nk
                     do j = 1, nj
                        do i = 1, ni
                           ETVp = sum(evec(j,1:nk,m) * xdata(iv,ie,ic)%value(i,j,1:nk))
                           field(i,j,m) = ETVp / eval(j,m)
                        end do
                     end do
                  end do
                  xdata(iv,ie,ic)%value(:,:,:) = field(:,:,:)
               end do ! End loop over members.
            end do
         end if
      end do var_loop

#ifdef DM_PARALLEL
      ijk = ni*nj*nk
      do iv = 1, nvar
         if ( .not. read_it(iv) ) cycle
         proc_send = MOD((iv-1), num_procs)
         do ic = 1, ncase
            do ie = istart_member, iend_member
               if ( myproc == MOD((iv-1), num_procs) ) then
                  field(:,:,:) = xdata(iv,ie,ic)%value(:,:,:)
               end if
               call mpi_bcast(field(:,:,:), ijk, mpi_real, proc_send, mpi_comm_world, ierr )
               if ( myproc /= MOD((iv-1), num_procs) ) then
                  xdata(iv,ie,ic)%value(:,:,:) = field(:,:,:)
               end if
            end do
         end do
      end do
#endif

   end if ! do_eof_transform

   deallocate(field)

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
            if ( .not. read_it(iv) ) cycle
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
                    field2d(:,:) = xdata(iv,ie,ic)%value(:,:,k)
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
            if ( .not. read_it(iv) ) cycle
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
                    field2d(:,:) = xdata(iv,ie,ic)%value(:,:,k)
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
               write(stdout,'(a,a,i3,f10.3)') 'ScaleLength: ', varnames(iv), k, sl(k)*ds*0.001
               be_data(iv)%scale_length(k) = sl(k)
            end do k_loop_opt2
#ifdef DM_PARALLEL
            call mpi_allreduce(sl(:),sl_g(:), nk, &
                               mpi_real, mpi_sum, mpi_comm_world, ierr)
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

   do ic = 1, ncase
      do ie = istart_member, iend_member
         do iv = 1, nvar
            deallocate(xdata(iv,ie,ic)%value)
         end do
      end do
   end do
   if ( allocated(xdata)) deallocate(xdata)

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

