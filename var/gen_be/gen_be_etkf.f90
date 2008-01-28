program gen_be_etkf

!---------------------------------------------------------------------- 
!  Purpose : Perform an Ensemble Transform Kalman Filter (ETKF) rescaling
!  of WRF ensemble forecast data.
!
!  Owner: Dale Barker (NCAR/MMM) - WRF wrappper, Xuguang Wang (NOAA) - ETKF algorithm.
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : trace_use, stdout,filename_len
!   use da_gen_be
   use da_etkf, only : da_solve_etkf
   use da_reporting, only : da_error, message

   implicit none

#include "netcdf.inc"

   integer, parameter    :: max_num_vars = 50         ! Maximum number of variables.
   integer, parameter    :: max_num_dims = 20         ! Maximum number of dimensions.
   integer, parameter    :: unit = 100                ! Unit number.

   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=filename_len)   :: output_file               ! Output file. 
   character (len=3)     :: ce                        ! Member index -> character.

   integer               :: num_members               ! Ensemble size.
   integer               :: nv                        ! Number of variables.
   integer               :: num_obs                   ! Number of observations.
   integer               :: naccumt1                  ! Number of previous cycles.
   integer               :: naccumt2                  ! Number of previous cycles.
   integer               :: nstartaccum1              ! Cycle from which naccumt1 cycle starts.
   integer               :: nstartaccum2              ! Cycle from which naccumt2 cycle starts.
   integer               :: nout                      ! Output record for inn. vec./ob. error var.
   integer               :: length                    ! Filename length.
   integer               :: rcode                     ! NETCDF return code.
   integer               :: cdfid                     ! NETCDF file IDs.
   integer               :: member, v, o, i, j, k, ijkv ! Loop counters.
   integer               :: ivtype                    ! 4=integer, 5=real, 6=d.p.
   integer               :: natts                     ! Number of field attributes.

   integer               :: index                     ! Array index.
   integer               :: nijkv                     ! Array counter.
   integer               :: iend                      ! End of array 
   real                  :: num_members_inv           ! 1 / ensemble size.
   real                  :: tainflatinput             ! Pre-specified inflation, if not using adaptive inflation.
   real                  :: rhoinput                  ! Pre-specified inflation, if not using adaptive rho factor.
   real                  :: ds                        ! Grid resolution (m).

   character(len=10)     :: cv(1:max_num_vars)        ! Default array of variable names.
   integer               :: id_var(1:max_num_vars)    ! NETCDF variable ID.
   integer               :: ndims(1:max_num_vars)     ! Number of field dimensions.
   integer               :: istart(1:max_num_vars)    ! Start index.
   integer               :: dimids(1:max_num_dims)    ! Array of dimension IDs.
   integer               :: one(1:max_num_dims)       ! Array of dimension starts.
   integer               :: dims(1:max_num_vars,1:max_num_dims)      ! Array of dimensions.
   integer               :: dim_prod(1:max_num_dims)  ! Product of array dimensions.

   real (kind=4), allocatable     :: data_r(:,:,:)             ! Data array.

   real, pointer         :: xf(:,:)                   ! Ensemble perturbations.
   real, pointer         :: xf_mean(:)                ! Ensemble perturbation mean.
   real, pointer         :: xf_vari(:)                ! Ensemble perturbation variance.
   real, pointer         :: y(:,:)                    ! H(xf).
   real, pointer         :: sigma_o2(:)               ! Ob error variance.
   real, pointer         :: yo(:)                     ! Observation.
   real, pointer         :: ens_mean(:)               ! Variable ensemble mean.
   real, pointer         :: ens_stdv_pert_prior(:)    ! Variable prior perturbation std. dev.
   real, pointer         :: ens_stdv_pert_poste(:)    ! Variable posterior perturbation std. dev.

   namelist / gen_be_etkf_nl / num_members, nv, cv, &
                               naccumt1, naccumt2, nstartaccum1, nstartaccum2, &
                               nout, tainflatinput, rhoinput

!---------------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [1] Initialize information.'
!-----------------------------------------------------------------------------------------

   num_members = 56
   nv = 1
   cv = "U"

   naccumt1 = 0
   naccumt2 = 0
   nstartaccum1 = 0
   nstartaccum2 = 0
   nout = 1 
   tainflatinput = 0.0
   rhoinput = 0.0

   open(unit=unit, file='gen_be_etkf_nl.nl', &
        form='formatted', status='old', action='read')
   read(unit, gen_be_etkf_nl)
   close(unit)

   write(stdout,'(a,i4)')'   Number of ensemble members = ', num_members
   write(stdout,'(a,i4)')'   Number of prognostic variables = ', nv
   write(stdout,'(50a)')'   List of prognostic variables = ', cv(1:nv)
   write(stdout,'(a,i4)')'   naccumt1 = ', naccumt1
   write(stdout,'(a,i4)')'   naccumt2 = ', naccumt2
   write(stdout,'(a,i4)')'   nstartaccum1 = ', nstartaccum1
   write(stdout,'(a,i4)')'   nstartaccum2 = ', nstartaccum2
   write(stdout,'(a,i4)')'   nout = ', nout
   write(stdout,'(a,f15.5)')'   tainflatinput = ', tainflatinput
   write(stdout,'(a,f15.5)')'   rhoinput = ', rhoinput

   num_members_inv = 1.0 / real(num_members)

   allocate( ens_mean(1:nv) )
   allocate( ens_stdv_pert_prior(1:nv) )
   allocate( ens_stdv_pert_poste(1:nv) )

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [2] Read observation information.'
!-----------------------------------------------------------------------------------------

   do member = 1, num_members

      write(unit=ce,FMT='(i3.3)')member
      input_file = 'ob.etkf.e'//ce  
      open(unit, file = input_file, status='old')
      read(unit,*)num_obs

      if ( member == 1 ) then
         write(stdout,'(a,i10)')'   Number of observations = ', num_obs
         allocate( y(1:num_obs,1:num_members) )
         allocate( sigma_o2(1:num_obs) )
         allocate( yo(1:num_obs) )
      end if

      do o = 1, num_obs
         read(unit,'(3f17.7)')yo(o), y(o,member), sigma_o2(o)
!        Convert yo-H(xb) to H(xb):
         y(o,member) = yo(o) - y(o,member)
!        Convert ob error standard deviation to variance:
         sigma_o2(o) = sigma_o2(o) * sigma_o2(o)
      end do
   end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [3] Set up arrays using input ensemble mean forecast'
!-----------------------------------------------------------------------------------------

!  Open mean:
   input_file = 'etkf_input'
   length = len_trim(input_file)
   rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )

   do v = 1, nv

!     Check variable is in file, and get variable id:
      rcode = nf_inq_varid ( cdfid, cv(v), id_var(v) )
      if ( rcode /= 0 ) then
         write(UNIT=message(1),FMT='(A,A)') &
            cv(v), ' variable is not in input file'
         call da_error(__FILE__,__LINE__,message(1:1)) 
      end if 

!     Get number of dimensions, and check of real type:
      dimids = 0
      rcode = nf_inq_var( cdfid, id_var(v), cv(v), ivtype, ndims(v), dimids, natts )
      if ( ivtype /= 5 ) then
         write(UNIT=message(1),FMT='(A,A)') cv(v), ' variable is not real type'
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

!     Get dimensions of field:
      dims(v,:) = 0
      do i = 1, ndims(v)
         rcode = nf_inq_dimlen( cdfid, dimids(i), dims(v,i) ) 
      end do
   end do

   one = 1
   istart(1) = 1
   do v = 2, nv
      istart(v) = istart(v-1) + product(dims(v-1,1:ndims(v-1)-1))
   end do
   nijkv = istart(nv) + product(dims(nv,1:ndims(nv)-1)) - 1

   allocate( xf_mean(1:nijkv) )
   do v = 1, nv
      allocate( data_r(dims(v,1),dims(v,2),dims(v,3)))

      call ncvgt( cdfid, id_var(v), one, dims(v,:), data_r, rcode)

!     Fill 4D array:
      index = istart(v)
      do k = 1, dims(v,3)
         do j = 1, dims(v,2)
            do i = 1, dims(v,1)
               xf_mean(index) = data_r(i,j,k)
               index = index + 1
            end do
         end do
      end do
      deallocate( data_r )
   end do ! v

   rcode = nf_close( cdfid )

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [4] Extract necessary fields from WRF ensemble forecasts.'
!-----------------------------------------------------------------------------------------

   allocate( xf(1:nijkv,1:num_members) )
   allocate( xf_vari(1:nijkv) )

   do member = 1, num_members
      write(UNIT=ce,FMT='(i3.3)')member

!     Open file:
      input_file = 'etkf_input.e'//ce
      length = len_trim(input_file)
      rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )

      do v = 1, nv
         allocate( data_r(dims(v,1),dims(v,2),dims(v,3)))

         call ncvgt( cdfid, id_var(v), one, dims(v,:), data_r, rcode)

!        Fill 4D array:
         index = istart(v)
         do k = 1, dims(v,3)
            do j = 1, dims(v,2)
               do i = 1, dims(v,1)
                  xf(index,member) = data_r(i,j,k)
                  index = index + 1
               end do
            end do
         end do
         deallocate( data_r )
      end do ! v
      rcode = nf_close( cdfid )
   end do !member

!  Convert ensemble forecasts to perturbations:
   do ijkv = 1, nijkv
      xf(ijkv,1:num_members) = xf(ijkv,1:num_members) - xf_mean(ijkv)
      xf_vari(ijkv) = sum(xf(ijkv,1:num_members)**2) * num_members_inv
   end do

!  Print prior mean, ensemble standard deviation:
   do v = 1, nv
      iend = istart(v) + product(dims(v,1:ndims(v)-1)) - 1
      ens_mean(v) = sum(xf_mean(istart(v):iend)) / real(iend - istart(v) + 1)
      ens_stdv_pert_prior(v) =sqrt( sum(xf_vari(istart(v):iend)) / &
                                    real(iend - istart(v) + 1) )
   end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [5] Call ETKF:'
!-----------------------------------------------------------------------------------------

   call da_solve_etkf( nijkv, num_members, num_obs, xf, y, sigma_o2, yo, nout, &
                       naccumt1, naccumt2, nstartaccum1, nstartaccum2, tainflatinput, &
                       rhoinput )

!  Calculate posterior ensemble standard deviation and output:
   do ijkv = 1, nijkv
      xf_vari(ijkv) = sum(xf(ijkv,1:num_members)**2) * num_members_inv
   end do

!  Print posterior mean, ensemble standard deviation:
   write(stdout,'(5a)')'   v', ' Variable  ', '    Ensemble Mean', &
                       '  Prior Pert StDv', ' Post. Pert. StDv'
   do v = 1, nv
      iend = istart(v) + product(dims(v,1:ndims(v)-1)) - 1
      ens_stdv_pert_poste(v) =sqrt( sum(xf_vari(istart(v):iend)) / &
                                    real(iend - istart(v) + 1) )

      write(stdout,'(i4,1x,a10,3f17.7)')v, cv(v), &
      ens_mean(v), ens_stdv_pert_prior(v), ens_stdv_pert_poste(v)
   end do

!-----------------------------------------------------------------------------------------
   write(stdout,'(/a)')' [6] Output ETKF analysis ensemble:'
!-----------------------------------------------------------------------------------------

   do member = 1, num_members
      write(UNIT=ce,FMT='(i3.3)')member

!     Open file:
      input_file = 'etkf_output.e'//ce
      length = len_trim(input_file)
      rcode = nf_open( input_file(1:length), NF_WRITE, cdfid )
      if ( rcode /= 0 ) then
         write(UNIT=message(1),FMT='(A,A)') &
            ' Error opening netcdf file ', input_file(1:length)
         call da_error(__FILE__,__LINE__,message(1:1))
      end if

      do v = 1, nv
         allocate( data_r(dims(v,1),dims(v,2),dims(v,3)))
   
!        Add updated perturbations back to ensemble mean and output
         index = istart(v)
         do k = 1, dims(v,3)
            do j = 1, dims(v,2)
               do i = 1, dims(v,1)
                  data_r(i,j,k) = xf_mean(index) + xf(index,member)
                  index = index + 1
               end do
            end do
         end do
         call ncvpt( cdfid, id_var(v), one, dims(v,:), data_r, rcode)
         deallocate( data_r )
      end do ! v
      rcode = nf_close( cdfid )
   end do !member

   deallocate( ens_mean )
   deallocate( ens_stdv_pert_prior )
   deallocate( ens_stdv_pert_poste )

end program gen_be_etkf

