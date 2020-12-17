program gen_be_cov3d2d_contrib
!------------------------------------------------------------------------
!  Purpose: Computes contribution of 3D Field on the balanced part
!           of 2D Field for WRFDA cv_options=6
!
!  Auothor: Syed RH Rizvi (MMM/NESL/NCAR)   Date: 02/01/2010
!           & Monika Krysta, (CTBTO, Vienna, Austria)
!
!  Note: Please acknowledge author/institute in work that uses this code.
!------------------------------------------------------------------------

   use da_control, only : stderr, stdout, filename_len
   use da_tools_serial, only : da_get_unit,da_advance_cymdh

   implicit none

   character*10        :: start_date, end_date       ! Starting and ending dates.
   character*10        :: date, new_date             ! Current date (ccyymmddhh).
   character*80        :: variable
   character*80        :: regcoeff_var
   character*10        :: variable1                  ! Variable name
   character*10        :: variable2                  ! Variable name

   real, allocatable   :: regcoeff_psi_chi(:)        ! psi/chi    regression cooefficient.
   real, allocatable   :: regcoeff_psi_t(:,:,:)      ! psi/t      regression cooefficient.
   real, allocatable   :: regcoeff_psi_ps(:,:)       ! psi/ps     regression cooefficient.
   real, allocatable   :: regcoeff_psi_rh(:,:,:)     ! psi/rh     regression cooefficient.
   real, allocatable   :: regcoeff_chi_u_t(:,:,:)    ! chi_u/t    regression coefficient
   real, allocatable   :: regcoeff_chi_u_ps(:,:)     ! chi_u/ps   regression coefficient
   real, allocatable   :: regcoeff_chi_u_rh(:,:,:)   ! chi_u/rh   regression coefficient
   real, allocatable   :: regcoeff_t_u_rh(:,:,:)     ! t_u/rh     regression coefficient
   real, allocatable   :: regcoeff_ps_u_rh(:,:)      ! ps_u/rh    regression coefficient
   
   character(len=filename_len)        :: filename                   ! Input filename.
   character*3         :: ce                         ! Member index -> character.
   integer             :: ni, nj, nk, nkdum                 ! Grid dimensions.
   integer             :: i, j, k, member            ! Loop counters.
   integer             :: b                          ! Bin marker.
   integer             :: sdate, cdate, edate        ! Starting, current ending dates.
   integer             :: interval                   ! Period between dates (hours).
   integer             :: ne                         ! Number of ensemble members.
   integer             :: bin_type                   ! Type of bin to average over.
   integer             :: num_bins                   ! Number of bins (3D fields).
   integer             :: num_bins2d                 ! Number of bins (3D fields).
   real                :: lat_min, lat_max           ! Used if bin_type = 2 (degrees).
   real                :: binwidth_lat               ! Used if bin_type = 2 (degrees).
   real                :: hgt_min, hgt_max           ! Used if bin_type = 2 (m).
   real                :: binwidth_hgt               ! Used if bin_type = 2 (m).
   real                :: coeffa, coeffb             ! Accumulating mean coefficients.
   logical             :: first_time                 ! True if first file.

   real, allocatable   :: field1(:,:,:)              ! Field 1.
   real, allocatable   :: field2(:,:)              ! Field 2 (full)
   real, allocatable   :: field2_balanced(:,:)   ! Field 2 (unbalanced)
   real*4, allocatable   :: field(:,:)   ! Field 2 (unbalanced)
   integer, allocatable:: bin(:,:,:)                 ! Bin assigned to each 3D point.
   integer, allocatable:: bin2d(:,:)                 ! Bin assigned to each 2D point.
   integer, allocatable:: bin_pts(:)                 ! Number of points in bin (3D fields).
   integer, allocatable:: bin_pts2d(:)                 ! Number of points in bin (3D fields).
   real, allocatable   :: covar(:)                   ! Covariance between input fields.
   real, allocatable   :: var(:)                     ! Autocovariance of field.

   real, allocatable   :: regcoeff_field1_field2(:,:) ! Regression coefficient

   namelist / gen_be_cov3d_nl / start_date, end_date, interval, &
                                ne, variable1, variable2

   integer :: ounit,iunit,namelist_unit

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(a)')' [1] Initialize namelist variables and other scalars.'
!---------------------------------------------------------------------------------------------

   call da_get_unit(ounit)
   call da_get_unit(iunit)
   call da_get_unit(namelist_unit)


   start_date = '2007070100'
   end_date = '2007070500'
   interval = 24
   ne = 1
   variable1 = 'psi'
   variable2 = 'ps'

   open(unit=namelist_unit, file='gen_be_cov_contrib_nl.nl', &
        form='formatted', status='old', action='read')
   read(namelist_unit, gen_be_cov3d_nl)
   close(namelist_unit)

   read(start_date(1:10), fmt='(i10)')sdate
   read(end_date(1:10), fmt='(i10)')edate
   write(6,'(4a)')' Computing covariance for fields ', variable1 , ' and ', variable2
   write(6,'(4a)') ' Time period is ', start_date, ' to ', end_date
   write(6,'(a,i8,a)')' Interval between dates = ', interval, 'hours.'
   write(6,'(a,i8)')' Number of ensemble members at each time = ', ne

   date = start_date
   cdate = sdate

   member = 1
   write(ce,'(i3.3)')member

!  Read grid dimensions from first field file
   filename = trim(variable1)//'/'//date(1:10)
   filename = trim(filename)//'.'//trim(variable1)//'.e'//ce
   open (iunit, file = filename, form='unformatted')
   read(iunit)ni, nj, nk
   close(iunit)

!  Allocate
   write(6,'(a,3i8)')'    i, j, k dimensions are ', ni, nj, nk
   allocate( bin(1:ni,1:nj,1:nk) )
   allocate( bin2d(1:ni,1:nj) )
   allocate( field1(1:ni,1:nj,1:nk) )
   allocate( field2(1:ni,1:nj) )
   allocate( field2_balanced(1:ni,1:nj) )
   allocate( field(1:ni,1:nj) )

!  Read bin info:
   filename = 'bin.data'
   open (iunit, file = filename, form='unformatted')
   read(iunit)bin_type
   read(iunit)lat_min, lat_max, binwidth_lat
   read(iunit)hgt_min, hgt_max, binwidth_hgt
   read(iunit)num_bins, num_bins2d
   read(iunit)bin(1:ni,1:nj,1:nk)
   read(iunit)bin2d(1:ni,1:nj)
   close(iunit)

   allocate  (regcoeff_psi_chi(1:num_bins))
   allocate  (regcoeff_psi_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_psi_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_t(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_ps(1:nk,1:num_bins2d))
   allocate  (regcoeff_chi_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_t_u_rh(1:nk,1:nk,1:num_bins2d))
   allocate  (regcoeff_ps_u_rh(1:nk,1:num_bins2d))

   allocate( regcoeff_field1_field2(1:nk,1:num_bins2d) )

   allocate( bin_pts(1:num_bins) )
   allocate( bin_pts2d(1:num_bins2d) )
   allocate( covar(1:num_bins) )
   allocate( var(1:num_bins) )
   bin_pts(:) = 0
   bin_pts2d(:) = 0
   covar(:) = 0.0
   var(:) = 0.0

!---------------------------------------------------------------------------------------------
   write(6,'(2a)')' [2] Read fields, and calculate correlations'
!--------------------------------------------------------------------------------------------- 
   
!  Read regression coefficients
   filename = 'gen_be_stage2.dat'
   open(iunit, file = filename, form='unformatted',status='old')

   do k = 1, 2
   read (iunit)
   end do

   regcoeff_var='regcoeff_'//trim(variable1)//'_'//trim(variable2)
   do k = 1 , 9
   read (iunit) variable
   select case( trim(adjustl(variable)) )

   case ('regcoeff_psi_chi')
   read (iunit) regcoeff_psi_chi
   case ('regcoeff_psi_t')
   read (iunit) regcoeff_psi_t
   case ('regcoeff_psi_ps')
   read (iunit) regcoeff_psi_ps
   if(trim(adjustl(variable)) == trim(adjustl(regcoeff_var)) )then
   regcoeff_field1_field2 = regcoeff_psi_ps
   exit
   end if
   case ('regcoeff_psi_rh')
   read (iunit) regcoeff_psi_rh
   case ('regcoeff_chi_u_t')
   read (iunit) regcoeff_chi_u_t
   case ('regcoeff_chi_u_ps')
   read (iunit) regcoeff_chi_u_ps
   if(trim(adjustl(variable)) == trim(adjustl(regcoeff_var)) )then
   regcoeff_field1_field2 = regcoeff_chi_u_ps
   exit
   end if
   case ('regcoeff_chi_u_rh')
   read (iunit) regcoeff_chi_u_rh
   case ('regcoeff_t_u_rh')
   read (iunit) regcoeff_t_u_rh
   case ('regcoeff_ps_u_rh')
   read (iunit) regcoeff_ps_u_rh
   if(trim(adjustl(variable)) == trim(adjustl(regcoeff_var)) )then
   regcoeff_field1_field2 = regcoeff_ps_u_rh
   exit
   end if
   case default;
      write(6,fmt='(a)')'Read problem gen_be_stage2.dat'
      write(6,'(a,a)')' Trying to read regression coefficients:',trim(adjustl(variable))
      stop
   end select
   end do

   close(iunit)
   deallocate (regcoeff_psi_chi)
   deallocate (regcoeff_psi_t)
   deallocate (regcoeff_psi_ps)
   deallocate (regcoeff_psi_rh)
   deallocate (regcoeff_chi_u_t)
   deallocate (regcoeff_chi_u_ps)
   deallocate (regcoeff_chi_u_rh)
   deallocate (regcoeff_t_u_rh)
   deallocate (regcoeff_ps_u_rh)


   do while ( cdate <= edate )

      do member = 1, ne

         write(ce,'(i3.3)')member

!        Read first field:
         filename = trim(variable1)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable1)//'.e'//ce
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nk
         read(iunit)field1
         close(iunit)

!        Read second field:
         filename = trim(variable2)//'/'//date(1:10)
         filename = trim(filename)//'.'//trim(variable2)//'.e'//ce//'.01'
         open (iunit, file = filename, form='unformatted')
         read(iunit)ni, nj, nkdum
         read(iunit)field2
         close(iunit)

!        Calculate balanced contribution to field2
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
                  field2_balanced(i,j) = SUM(regcoeff_field1_field2(1:nk,b) * field1(i,j,1:nk))
            end do
         end do
         
!        Calculate covariances:
         do j = 1, nj
            do i = 1, ni
               b = bin2d(i,j)
               bin_pts2d(b) = bin_pts2d(b) + 1
               coeffa = 1.0 / real(bin_pts2d(b))
               coeffb = real(bin_pts2d(b)-1) * coeffa
               covar(b) = coeffb * covar(b) + coeffa * field2_balanced(i,j) * field2(i,j)
               var(b) = coeffb * var(b) + coeffa * field2(i,j) * field2(i,j)
            end do
         end do

      end do  ! End loop over ensemble members.

!     Calculate next date:
      call da_advance_cymdh( date, interval, new_date )
      date = new_date
      read(date(1:10), fmt='(i10)')cdate

   end do     ! End loop over times.

   filename = trim(variable1)//'.'//trim(variable2)//'.dat'
   open (ounit, file = filename, status='unknown')
   do j = 1, nj
      do i = 1, ni
         b = bin2d(i,j)
         if ( var(b) /= 0.0 ) then
            field(i,j) = covar(b) / var(b)
            write(ounit,'(f16.8)')covar(b)/var(b)
         else
            write(ounit,'(f16.8)')0.0
         end if
      end do
   end do
   close(ounit)

   filename = trim(variable1)//'.'//trim(variable2)//'.bin'
   open (ounit, file = filename, form = 'unformatted')
   do j = 1, nj
      do i = 1, ni
         b = bin2d(i,j)
         if ( var(b) /= 0.0 ) then
            field(i,j) = covar(b) / var(b)
         else
            field(i,j) = 0.
         end if
      end do
    write(ounit)field(:,j)
   end do
   close(ounit)

end program gen_be_cov3d2d_contrib

