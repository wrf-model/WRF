subroutine da_get_field( input_file, var, field_dims, dim1, dim2, dim3,k,field)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"
   
   character(len=200), intent(in)  :: input_file       ! NETCDF file nane.
   character(len=10),  intent(in)  :: var              ! Variable to search for.
   integer,            intent(in)  :: field_dims       ! # Dimensions of field. 
   integer,            intent(in)  :: dim1             ! Dimension 1 of field. 
   integer,            intent(in)  :: dim2             ! Dimension 2 of field. 
   integer,            intent(in)  :: dim3             ! Dimension 3 of field. 
   integer,            intent(in)  :: k                ! Vertical index.
   real,               intent(out) :: field(1:dim1,1:dim2) ! Output field

   integer                   :: cdfid            ! NETCDF file id.
   integer                   :: rcode            ! Return code(0=ok).
   integer                   :: length           ! Length of filename.
   integer                   :: id_var           ! NETCDF variable ID. 

   integer                   :: istart(4)        ! Start value of arrays.
   integer                   :: iend(4)          ! End value of arrays.
   real(kind=4), allocatable :: field1d(:)       ! Used if 1D field read. 
   real(kind=4), allocatable :: field2d(:,:)     ! Used if 2D field read. 
   real(kind=4), allocatable :: field3d(:,:,:)   ! Used if 3D field read. 

   if (trace_use_dull) call da_trace_entry("da_get_field")

   length = len_trim(input_file)
   rcode = nf_open( input_file(1:length), NF_NOwrite, cdfid)
   if (rcode /= 0) then
      write(message(1),'(3a,i0)')' nf_open(',input_file(1:length),') returned ',rcode
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   !  Check variable is in file:
   rcode = nf_inq_varid( cdfid, var, id_var)
   if (rcode /= 0) then
      write(message(1),'(3a)')var,' variable is not in input file ',input_file(1:length)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   istart = 1
   iend(1) = dim1
   iend(2) = dim2
   iend(4) = 1          ! Single time assumed.

   if (field_dims == 1) then
      iend(2) = 1
      iend(3) = 1
      allocate( field1d(1:dim1))
      call ncvgt( cdfid, id_var, istart, iend, field1d, rcode)
      field(:,1) = field1d(:)
      rcode = nf_close( cdfid)
      deallocate( field1d)
   else if (field_dims == 2) then
      iend(3) = 1
      allocate( field2d(1:dim1,1:dim2))
      call ncvgt( cdfid, id_var, istart, iend, field2d, rcode)
      field(:,:) = field2d(:,:)
      rcode = nf_close( cdfid)
      deallocate( field2d)
   else if (field_dims == 3) then
      iend(3) = dim3
      allocate( field3d(1:dim1,1:dim2,1:dim3))
      call ncvgt( cdfid, id_var, istart, iend, field3d, rcode)
      field(:,:) = field3d(:,:,k)
      deallocate( field3d)
   end if

   rcode = nf_close( cdfid)

   if (trace_use_dull) call da_trace_exit("da_get_field")

end subroutine da_get_field


