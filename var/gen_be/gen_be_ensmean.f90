program gen_be_ensmean
!
!---------------------------------------------------------------------- 
!  Purpose: Calculate ensemble mean file from input WRF NETCDF input
!  ensemble members.
!
!  Owner: Dale Barker (NCAR/MMM) - WRF wrappper. Thanks to Cindy Bruyere.
!  Please acknowledge author/institute in work that uses this code.
!
!----------------------------------------------------------------------

#ifdef crayx1
#define iargc ipxfargc
#endif

   use da_control, only : stdout, stderr,filename_len
   use da_reporting, only : da_error,message

   implicit none

#include "netcdf.inc"

   integer, parameter    :: max_num_dims = 4          ! Maximum number of dimensions.
   integer, parameter    :: max_num_vars = 50         ! Maximum number of variables.
   integer, parameter    :: unit = 100                ! Unit number.

   character (len=filename_len)   :: directory                 ! General filename stub.
   character (len=filename_len)   :: filename                  ! General filename stub.
   character (len=filename_len)   :: input_file                ! Input file. 
   character (len=10)    :: var                       ! Variable to search for.
   character (len=3)     :: ce                        ! Member index -> character.

   integer               :: num_members               ! Ensemble size.
   integer               :: nv                        ! Number of variables.
   integer               :: v, member, i              ! Loop counters.
   integer               :: length                    ! Filename length.
   integer               :: rcode                     ! NETCDF return code.
   integer               :: cdfid                     ! NETCDF file IDs.
   integer               :: cdfid_mean                ! NETCDF file IDs.
   integer               :: cdfid_vari                ! NETCDF file IDs.
   integer               :: id_var                    ! NETCDF variable ID.
   integer               :: ivtype                    ! 4=integer, 5=real, 6=d.p.
   integer               :: ndims                     ! Number of field dimensions.
   integer               :: natts                     ! Number of field attributes.
   real                  :: member_inv                ! 1 / ensemble size.

   character(len=10)     :: cv(1:max_num_vars)        ! Default array of variable names.
   integer               :: dimids(1:10)              ! Array of dimension IDs.
   integer               :: dims(1:max_num_dims)      ! Array of dimensions.
   integer               :: istart(1:max_num_dims)    ! Array of dimension starts.
   integer               :: iend(1:max_num_dims)      ! Array of dimension ends.

   real (kind=4), allocatable     :: data_r(:,:,:)             ! Data array.
   real (kind=4), allocatable     :: data_r_mean(:,:,:)        ! Data array mean.
   real (kind=4), allocatable     :: data_r_vari(:,:,:)        ! Data array variance.
 
   namelist / gen_be_ensmean_nl / directory, filename, num_members, nv, cv

   stderr = 0
   stdout = 6

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [1] Initialize information.'
!---------------------------------------------------------------------------------------------

   directory = './'
   filename = 'test'
   num_members = 56
   nv = 1
   cv = "U"

   open(unit=unit, file='gen_be_ensmean_nl.nl', &
        form='formatted', status='old', action='read')
   read(unit, gen_be_ensmean_nl)
   close(unit)

   write(6,'(a,a)')'   Directory = ', trim(directory)
   write(6,'(a,a)')'   filename = ', trim(filename)
   write(6,'(a,i4)')'   Number of ensemble members = ', num_members
   write(6,'(a,i4)')'   Number of variables to average = ', nv
   write(6,'(50a)')'   List of variables to average = ', cv(1:nv)

!  Open template ensemble mean with write access:
   input_file = trim(directory)//'/'//trim(filename)//'.mean'
   length = len_trim(input_file)
   rcode = nf_open(input_file(1:length), NF_WRITE, cdfid_mean )
   if ( rcode /= 0) then
      write(UNIT=message(1),FMT='(A,A)') &
         ' Error opening netcdf file ', input_file(1:length)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

!  Open template ensemble variance with write access:
   input_file = trim(directory)//'/'//trim(filename)//'.vari'
   length = len_trim(input_file)
   rcode = nf_open(input_file(1:length), NF_WRITE, cdfid_vari )
   if ( rcode /= 0) then
      write(UNIT=message(1),FMT='(A,A)') &
         ' Error opening netcdf file ', input_file(1:length)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

!---------------------------------------------------------------------------------------------
   write(6,'(/a)')' [2] Extract necessary fields from WRF ensemble forecasts.'
!---------------------------------------------------------------------------------------------

   do v = 1, nv ! Loop over variables to average:
      var = cv(v)
      write(6,'(2a)')' Computing ensemble mean for variable ', var

      do member = 1, num_members
         write(UNIT=ce,FMT='(i3.3)')member

!        Open file:
         input_file = trim(directory)//'/'//trim(filename)//'.e'//trim(ce)
         print *, 'APM open ',input_file
         length = len_trim(input_file)
         rcode = nf_open( input_file(1:length), NF_NOWRITE, cdfid )

         if ( member == 1 ) then
!           Get variable ID:
            rcode = nf_inq_varid ( cdfid, var, id_var )

!           Check variable is in file:
            if ( rcode /= 0 ) then
               write(UNIT=message(1),FMT='(A,A)') &
                  var, ' variable is not in input file'
               call da_error(__FILE__,__LINE__,message(1:1))
             end if

!            Get dimension information for this field:
             dimids = 0
             dims = 0
             rcode = nf_inq_var( cdfid, id_var, var, ivtype, ndims, dimids, natts )
             do i = 1, ndims
                rcode = nf_inq_dimlen( cdfid, dimids(i), dims(i) )
             end do
             istart = 1
             iend = 1
             do i = 1, ndims-1
                iend(i) = dims(i)
             end do

!            Allocate and initialize data:
             if ( ivtype == 5 ) then
                allocate( data_r(iend(1),iend(2),iend(3)))
                allocate( data_r_mean(iend(1),iend(2),iend(3)))
                allocate( data_r_vari(iend(1),iend(2),iend(3)))
                data_r_mean = 0.0
                data_r_vari = 0.0
             else
                write(UNIT=message(1),FMT='(A,A)') &
                   var, ' variable is not real type'
                call da_error(__FILE__,__LINE__,message(1:1))
             end if
print *, var, ivtype, id_var
print *, istart, iend(1), iend(2), iend(3)
         end if

!        Calculate accumulating mean and variance:
         member_inv = 1.0 / real(member)   
         call ncvgt( cdfid, id_var, istart, iend, data_r, rcode)
         data_r_mean = ( real( member-1 ) * data_r_mean + data_r ) * member_inv
         data_r_vari = ( real( member-1 ) * data_r_vari + data_r * data_r ) * member_inv

         rcode = nf_close( cdfid )

      end do ! member

      data_r_vari = ( real(num_members)/real(num_members-1) * (data_r_vari - &
                      data_r_mean*data_r_mean) ) 

      data_r_vari = sqrt(data_r_vari)

      call ncvpt( cdfid_mean, id_var, istart, iend, data_r_mean, rcode)
      call ncvpt( cdfid_vari, id_var, istart, iend, data_r_vari, rcode)
      deallocate( data_r )
      deallocate( data_r_mean )
      deallocate( data_r_vari )

   end do ! variable

   rcode = nf_close( cdfid_mean )
   rcode = nf_close( cdfid_vari )

end program gen_be_ensmean

