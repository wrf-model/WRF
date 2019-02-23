program gen_be_ep2

!-----------------------------------------------------------------------
! Purpose: To convert WRF ensemble to format required for use as
!          flow-dependent perturbations in WRFDA (alpha control variable,
!          alphacv_method = 2).
! History:
!     March 2017 - Creation   Jamie Bresch
!                  new parallelized code to replace the previous gen_be_ep2
!                  (now named gen_be_ep2_serial.f90)
!-----------------------------------------------------------------------

   implicit none

#ifdef DM_PARALLEL
   include 'mpif.h'
#if ( DWORDSIZE != RWORDSIZE )
   integer, parameter :: true_mpi_real    = mpi_real
#else
   integer, parameter :: true_mpi_real    = mpi_real8
#endif
#endif

   integer, parameter :: DateStrLen = 19 !as in wrf_io.F
   integer, parameter :: VarNameLen = 31 !as in wrf_io.F
   integer, parameter :: stdout = 6
   integer, parameter :: root = 0
   integer, parameter :: nvar_max = 10
   real,    parameter :: t00 = 300.0
   real,    parameter :: p00 = 100000.0
   real,    parameter :: gas_constant = 287.0
   real,    parameter :: cp = 7.0*gas_constant/2.0
   real,    parameter :: kappa = gas_constant/cp

   logical :: remove_mean        = .true.
   logical :: alpha_hydrometeors = .true.
   logical :: write_mean_stdv    = .true.

   type xdata_type
      character(len=VarNameLen) :: name
      real, allocatable :: value(:,:,:,:)
      real, allocatable :: mean(:,:,:)
      real, allocatable :: mnsq(:,:,:) !mean square
      real, allocatable :: stdv(:,:,:)
   end type xdata_type
   type (xdata_type), allocatable :: xdata(:)

   character(len=VarNameLen) :: varnames(nvar_max)
   character(len=VarNameLen) :: fnames(nvar_max)

   ! argument variables
   character(len=512)        :: directory, filename
   character(len=VarNameLen) :: cvar
   character(len=10)         :: cdate10
   character(len=3)          :: cne
   integer                   :: numarg
   integer                   :: icode

   integer :: num_procs, myproc
   integer :: ounit
   integer :: nvar, nens, iv, ivar, ie
   integer :: i, j, k, ijk
   integer :: ni, ni1, nj, nj1, nk
   integer :: mp_physics
   real    :: ens_inv

   character(len=512) :: input_file, output_file
   character(len=3)   :: ce

   character(len=80), dimension(3) :: dimnames
   character(len=4)                :: staggering=' N/A' !dummy
   character(len=3)                :: ordering
   character(len=DateStrLen)       :: DateStr
   character(len=VarNameLen)       :: varname
   integer, dimension(4)           :: start_index, end_index
   integer                         :: fid, ierr, ndim, wrftype
   integer                         :: icnt

   integer :: avail(nvar_max)
   integer :: readit(nvar_max)
   integer, allocatable :: istart(:), iend(:)
   integer, allocatable :: ncount(:), displs(:)

   real*4, allocatable :: pp(:,:,:) ! WRF perturbation P
   real*4, allocatable :: pb(:,:,:) ! WRF base P
   real*4, allocatable :: xfield(:,:,:)
   real*4, allocatable :: xfield_u(:,:,:)
   real*4, allocatable :: xfield_v(:,:,:)

   real,   allocatable :: globuf(:,:,:,:)
   real,   allocatable :: globuf1d(:)
   real,   allocatable :: tmp1d(:)

#ifdef DM_PARALLEL
   call mpi_init(ierr)
   call mpi_comm_size(mpi_comm_world,num_procs,ierr)
   call mpi_comm_rank(mpi_comm_world,myproc,ierr)
#else
   num_procs = 1
   myproc = 0
#endif

   ! variable names in wrfout files
   varnames = (/ 'U     ', 'V     ', 'T     ', 'QVAPOR', 'PSFC  ', &
                 'QCLOUD', 'QRAIN ', 'QICE  ', 'QSNOW ', 'QGRAUP' /)

   ! variable names for output
   fnames   = (/ 'u     ', 'v     ', 't     ', 'q     ', 'ps    ',  &
                 'qcloud', 'qrain ', 'qice  ', 'qsnow ', 'qgraup' /)

   numarg = command_argument_count()
   if ( numarg /= 4 .and. numarg /= 5 )then
      write(stdout,FMT='(a)') &
        "Usage: gen_be_ep2.exe  date  ne  directory  filename  [varname]"
#ifdef DM_PARALLEL
      call mpi_abort(mpi_comm_world,1,ierr)
#else
      stop
#endif
   end if

   ! initialze argument variables
   cdate10   = ""
   cne       = ""
   directory = ""
   filename  = ""
   cvar      = ""

   call get_command_argument(number=1, value=cdate10)
   call get_command_argument(number=2, value=cne)
   read(cne,'(i3)') nens
   call get_command_argument(number=3, value=directory)
   call get_command_argument(number=4, value=filename)
   if ( numarg == 5 ) then
      call get_command_argument(number=5, value=cvar)
      ! convert cvar to be in lowercase
      do i = 1, len_trim(cvar)
         icode = ichar(cvar(i:i))
         if (icode>=65 .and. icode<=90) then
            cvar(i:i) = char(icode + 97 - 65)
         end if
      end do
   else
      cvar = 'all'
   end if

   if ( myproc == root ) then
      if ( remove_mean ) then
         write(stdout,'(a,a)')' Computing gen_be ensemble perturbation files for date ', cdate10
      else
         write(stdout,'(a,a)')' Computing gen_be ensemble forecast files for date ', cdate10
      end if
      write(stdout,'(a)')' Perturbations are in MODEL SPACE'
      write(stdout,'(a,i4)')' Ensemble Size = ', nens
      write(stdout,'(a,a)')' Directory = ', trim(directory)
      write(stdout,'(a,a)')' Filename = ', trim(filename)
   end if

   ounit = 61

   call ext_ncd_ioinit("",ierr)

   ! open file e001 for retrieving general information

   input_file = trim(directory)//'/'//trim(filename)//'.e001'
   call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
   if ( ierr /= 0 ) then
      write(stdout, '(a,a,i8)') 'Error opening ', trim(input_file), ierr
#ifdef DM_PARALLEL
      call mpi_abort(mpi_comm_world,1,ierr)
#else
      stop
#endif
   end if

   ! retrieve dimensions from variable T

   varname = "T"
   call ext_ncd_get_var_info (fid, varname, ndim, ordering, staggering, &
                              start_index, end_index, wrftype, ierr)
   ni = end_index(1)
   nj = end_index(2)
   nk = end_index(3)
   ni1 = ni + 1
   nj1 = nj + 1
   ijk = ni * nj * nk
   if ( myproc == root ) write(stdout, '(a,3i5)') ' ni, nj, nk = ', ni, nj, nk

   ! retrieve information for cloud variables

   mp_physics = 0 !initialize
   call ext_ncd_get_dom_ti_integer (fid, 'MP_PHYSICS', mp_physics, 1, icnt, ierr)

   avail(1:5)  = 1 ! initialize as available for 5 basic variables
   avail(6:10) = 0 ! initialize as not available for cloud variables
   if ( alpha_hydrometeors ) then
      if ( mp_physics > 0 ) then
         avail(6) = 1  ! qcloud
         avail(7) = 1  ! qrain
         if ( mp_physics == 2 .or. mp_physics == 4 .or.  &
              mp_physics >= 6 ) then
            avail(8) = 1 ! qice
         end if
         if ( mp_physics == 2 .or. mp_physics >= 4 ) then
            avail(9) = 1 ! qsnow
         end if
         if ( mp_physics == 2 .or. mp_physics >= 6 ) then
            if ( mp_physics /= 11 .and. mp_physics /= 13 .and. &
                 mp_physics /= 14 ) then
               avail(10) = 1 ! qgraup
            end if
         end if
      end if
   end if

   ! done retrieving information from file e001
   call ext_ncd_ioclose(fid, ierr)

   allocate (xfield  (ni, nj, nk))
   allocate (xfield_u(ni1,nj, nk))
   allocate (xfield_v(ni, nj1,nk))

   ! number of variables to read
   readit(1:nvar_max) = 0 ! initilaze as not read
   if ( trim(cvar) == 'all' ) then
      readit(:) = 1
   else
      do i = 1, nvar_max
         if ( fnames(i) == trim(cvar) ) then
            readit(i) = 1
            exit
         end if
      end do
   end if
   nvar = 0
   do i = 1, nvar_max
      if ( avail(i) == 1 .and. readit(i) == 1 ) then
         nvar = nvar + 1
      end if
   end do

   if ( nvar < 1 ) then
      write(stdout, '(a,i3)') 'invalid number of variables to process ', nvar
#ifdef DM_PARALLEL
      call mpi_abort(mpi_comm_world,1,ierr)
#else
      stop
#endif
   end if

   ! divide nens among available processors
   allocate (istart(0:num_procs-1))
   allocate (iend  (0:num_procs-1))
   allocate (ncount(0:num_procs-1))
   allocate (displs(0:num_procs-1))
   do i = 0, num_procs - 1
      call para_range(1, nens, num_procs, i, istart(i), iend(i))
      ncount(i) = iend(i) - istart(i) + 1
   end do
   ! get displs to be used later in mpi gather
   displs(0) = 0
   do i = 1, num_procs-1
      displs(i) = displs(i-1) + ncount(i-1)
   end do
   write(stdout,'(a,i4,a,i4,a,i4)') &
      'Processor ', myproc, ' will read files ', istart(myproc), ' - ', iend(myproc)

   allocate(xdata(nvar))
   do ivar = 1, nvar
      allocate(xdata(ivar)%value(ni,nj,nk,istart(myproc):iend(myproc)))
      allocate(xdata(ivar)%mean(ni,nj,nk))
      xdata(ivar)%value = 0.0
      xdata(ivar)%mean  = 0.0
   end do

   allocate (pp(ni, nj, nk))
   allocate (pb(ni, nj, nk))

   !do ie = 1, nens
   do ie = istart(myproc), iend(myproc) ! each proc reads a subset of nens

      write(ce,'(i3.3)') ie
      input_file = trim(directory)//'/'//trim(filename)//'.e'//trim(ce)

      call ext_ncd_open_for_read(trim(input_file), 0, 0, "", fid, ierr)
      if ( ierr /= 0 ) then
         write(stdout, '(a,a)') 'Error opening ', trim(input_file)
#ifdef DM_PARALLEL
         call mpi_abort(mpi_comm_world,1,ierr)
#else
         stop
#endif
      end if

      call ext_ncd_get_next_time(fid, DateStr, ierr)

      ! read P and PB for converting T (theta) to temperature
      call ext_ncd_get_var_info (fid, 'P', ndim, ordering, staggering,  &
                                 start_index, end_index, wrftype, ierr)
      call ext_ncd_read_field(fid, DateStr, 'P',         &
                              pp, wrftype,               &
                              0, 0, 0, ordering,         &
                              staggering, dimnames,      & !dummy
                              start_index, end_index,    & !dom
                              start_index, end_index,    & !mem
                              start_index, end_index,    & !pat
                              ierr                   )
      call ext_ncd_get_var_info (fid, 'PB', ndim, ordering, staggering, &
                                 start_index, end_index, wrftype, ierr)
      call ext_ncd_read_field(fid, DateStr, 'PB',        &
                              pb, wrftype,               &
                              0, 0, 0, ordering,         &
                              staggering, dimnames,      & !dummy
                              start_index, end_index,    & !dom
                              start_index, end_index,    & !mem
                              start_index, end_index,    & !pat
                              ierr                   )

      ivar = 0
      var_loop: do iv = 1, nvar_max

         if ( avail(iv)==0 .or. readit(iv)==0 ) cycle var_loop

         varname = trim(varnames(iv))
         call ext_ncd_get_var_info (fid, varname, ndim, ordering, staggering, &
                                    start_index, end_index, wrftype, ierr)

         ivar = ivar + 1
         xdata(ivar)%name = fnames(iv)

         write(stdout, '(a,a8,a,a)') ' Reading ', trim(varname), ' from ', trim(input_file)

         if ( varname == 'PSFC' ) then
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield(:,:,1), wrftype,    &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            xdata(ivar)%value(:,:,1,ie) = xfield(:,:,1)
         else if ( varname == 'U' ) then
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield_u(:,:,:), wrftype,  &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     xdata(ivar)%value(i,j,k,ie) = &
                        0.5 * ( dble(xfield_u(i,j,k)) + dble(xfield_u(i+1,j,k)) )
                  end do
               end do
            end do
         else if ( varname == 'V' ) then
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield_v(:,:,:), wrftype,  &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            do k = 1, nk
               do j = 1, nj
                  do i = 1, ni
                     xdata(ivar)%value(i,j,k,ie) = &
                        0.5 * ( dble(xfield_v(i,j,k)) + dble(xfield_v(i,j+1,k)) )
                  end do
               end do
            end do
         else
            call ext_ncd_read_field(fid, DateStr, varname,     &
                                    xfield, wrftype,           &
                                    0, 0, 0, ordering,         &
                                    staggering, dimnames,      & !dummy
                                    start_index, end_index,    & !dom
                                    start_index, end_index,    & !mem
                                    start_index, end_index,    & !pat
                                    ierr                   )
            if ( varname == 'QVAPOR' ) then
               ! from mixing ratio to specific humidity
               xdata(ivar)%value(:,:,:,ie) = xfield(:,:,:) / ( 1.0 + xfield(:,:,:) )
            else if ( varname == 'T' ) then
               xdata(ivar)%value(:,:,:,ie) = &
                  (t00+xfield(:,:,:))*((pp(:,:,:)+pb(:,:,:))/p00)**kappa
            else
               xdata(ivar)%value(:,:,:,ie) = xfield
            end if
         end if

      end do var_loop ! nvar loop

      call ext_ncd_ioclose(fid, ierr)

   end do ! nens loop

   deallocate (pp)
   deallocate (pb)
   deallocate (xfield)
   deallocate (xfield_u)
   deallocate (xfield_v)

   if ( myproc == root ) write(stdout,'(a)') ' Computing mean'
   if ( myproc == root ) then
      allocate (globuf  (ni, nj, nk, nens))
   end if
#ifdef DM_PARALLEL
   if ( myproc == root ) then
      allocate (globuf1d(ijk*nens))
   end if
   allocate (tmp1d (ijk*ncount(myproc)))
#endif

   do ivar = 1, nvar
#ifdef DM_PARALLEL
      tmp1d = reshape(xdata(ivar)%value(:,:,:,istart(myproc):iend(myproc)), &
                      (/ ijk*ncount(myproc) /))
      ! gather all ens members to root
      call mpi_gatherv( tmp1d,                                 &
                        ijk*ncount(myproc), true_mpi_real,     &
                        globuf1d,                              &
                        ijk*ncount, ijk*displs, true_mpi_real, &
                        root, mpi_comm_world, ierr )
      if ( ierr /= 0 ) then
         write(stdout, '(a, i2)') 'Error mpi_gatherv on proc ', myproc
         call mpi_abort(mpi_comm_world,1,ierr)
      end if
      if ( myproc == root ) then
         globuf = reshape(globuf1d, (/ ni, nj, nk, nens /))
      end if
#else
      globuf(:,:,:,:) = xdata(ivar)%value(:,:,:,:)
#endif
      if ( myproc == root ) then

         allocate(xdata(ivar)%mnsq(ni,nj,nk))
         allocate(xdata(ivar)%stdv(ni,nj,nk))
         xdata(ivar)%mnsq  = 0.0
         xdata(ivar)%stdv  = 0.0

         do ie = 1, nens ! loop over all ens member
            ens_inv = 1.0/real(ie)
            ! calculate accumulating mean and mean square
           xdata(ivar)%mean(:,:,:) = (real(ie-1)*xdata(ivar)%mean(:,:,:)+globuf(:,:,:,ie))*ens_inv
           xdata(ivar)%mnsq(:,:,:) = (real(ie-1)*xdata(ivar)%mnsq(:,:,:)+globuf(:,:,:,ie)*globuf(:,:,:,ie))*ens_inv
         end do

         if ( write_mean_stdv ) then
            write(stdout,'(a,a)') ' Computing standard deviation and writing out for ', trim(xdata(ivar)%name)
            xdata(ivar)%stdv(:,:,:) = sqrt(xdata(ivar)%mnsq(:,:,:)-xdata(ivar)%mean(:,:,:)*xdata(ivar)%mean(:,:,:))

            ! output mean
            output_file = trim(xdata(ivar)%name)//'.mean'
            open (ounit, file = output_file, form='unformatted')
            write(ounit) ni, nj, nk
            if ( trim(xdata(ivar)%name) == 'ps' ) then
               write(ounit) xdata(ivar)%mean(:,:,1)
            else
               write(ounit) xdata(ivar)%mean(:,:,1:nk)
            end if
            close(ounit)

            ! output stdv
            output_file = trim(xdata(ivar)%name)//'.stdv'
            open (ounit, file = output_file, form='unformatted')
            write(ounit) ni, nj, nk
            if ( trim(xdata(ivar)%name) == 'ps' ) then
               write(ounit) xdata(ivar)%stdv(:,:,1)
            else
               write(ounit) xdata(ivar)%stdv(:,:,1:nk)
            end if
            close(ounit)
         end if ! write_mean_stdv
         deallocate(xdata(ivar)%mnsq)
         deallocate(xdata(ivar)%stdv)

      end if ! root

#ifdef DM_PARALLEL
      if ( remove_mean ) then
         call mpi_bcast(xdata(ivar)%mean, ijk , true_mpi_real , root , mpi_comm_world, ierr )
      end if
#endif

   end do

#ifdef DM_PARALLEL
   call mpi_barrier (mpi_comm_world,ierr)
#endif

   if ( myproc == root ) then
      deallocate (globuf)
   end if
#ifdef DM_PARALLEL
   if ( myproc == root ) then
      deallocate (globuf1d)
   end if
   deallocate (tmp1d)
#endif

   if ( myproc == root ) write(stdout,'(a)') ' Computing perturbations and writing out'
   do ivar = 1, nvar
      do ie = istart(myproc), iend(myproc) ! each proc loops over a subset of ens
         if ( remove_mean ) then
            xdata(ivar)%value(:,:,:,ie) = xdata(ivar)%value(:,:,:,ie) - xdata(ivar)%mean(:,:,:)
         end if
         write(ce,'(i3.3)') ie
         output_file = trim(xdata(ivar)%name)//'.e'//trim(ce)
         open (ounit, file = output_file, form='unformatted')
         write(ounit) ni, nj, nk
         if ( trim(xdata(ivar)%name) == 'ps' ) then
            write(ounit) xdata(ivar)%value(:,:,1,ie)
         else
            write(ounit) xdata(ivar)%value(:,:,1:nk,ie)
         end if
         close(ounit)
      end do
   end do

#ifdef DM_PARALLEL
   call mpi_barrier (mpi_comm_world,ierr)
#endif

   deallocate (istart)
   deallocate (iend  )
   deallocate (ncount)
   deallocate (displs)

   do ivar = 1, nvar
      deallocate(xdata(ivar)%value)
      deallocate(xdata(ivar)%mean)
   end do
   deallocate(xdata)

   if ( myproc == root ) write(stdout,'(a)')' All Done!'

#ifdef DM_PARALLEL
   call mpi_finalize(ierr)
#endif

contains

subroutine para_range(n1, n2, nprocs, myrank, ista, iend)
!
! Purpose: determines the start and end index for each PE
!          given the loop range.
! History: 2014-02-24  Xin Zhang
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

end program gen_be_ep2

! wrf_debug is called by ext_ncd_ subroutines
! add dummy subroutine wrf_debug here to avoid WRF dependency
SUBROUTINE wrf_debug( level , str )
  IMPLICIT NONE
  CHARACTER*(*) str
  INTEGER , INTENT (IN) :: level
  RETURN
END SUBROUTINE wrf_debug
