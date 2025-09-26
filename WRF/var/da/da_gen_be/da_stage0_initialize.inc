subroutine da_stage0_initialize(input_file, var, dim1, dim2, dim3, ds, mp_physics)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

#include "netcdf.inc"

   character (len=200), intent(in):: input_file       ! NETCDF file name.
   character (len=10), intent(in) :: var              ! Variable to search for.
   integer, intent(out)           :: dim1             ! Dimensions of field. 
   integer, intent(out)           :: dim2             ! Dimensions of field. 
   integer, intent(out)           :: dim3             ! Dimensions of field. 
   real, intent(out)              :: ds               ! Grid resolution.
   integer, intent(out), optional :: mp_physics       ! microphysics option

   character (len=80)             :: att_name         ! Attribute name.
   integer                        :: i                ! Loop counters.
   integer                        :: attlen           ! Length of attribute.
   integer                        :: cdfid            ! NETCDF file id.
   integer                        :: rcode            ! Return code (0=ok).
   integer                        :: length           ! Length of filename.
   integer                        :: id_var           ! NETCDF variable ID. 
   integer                        :: ivtype           ! 4=integer, 5=real, 6=d.p.
   integer                        :: ndims            !
   integer                        :: natts            ! Number of attributes.

   integer                        :: dimids(10)       !
   integer                        :: dims(1:4)        ! Dimensions of field. 
   real (kind=4), allocatable     :: value_real(:)    ! real attribute value. 

   ! if (trace_use) call da_trace_entry("da_stage0_initialize")

   ! Check file exists:
   length = len_trim(input_file)
   rcode = nf_open(input_file(1:length), NF_NOwrite, cdfid)
   if (rcode /= 0) then
      write(unit=message(1),fmt='(A,A)') &
         ' Error opening netcdf file ', input_file(1:length)
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ! Check variable is in file:
   rcode = nf_inq_varid (cdfid, var, id_var)
   if (rcode /= 0) then 
      write(unit=message(1),fmt='(A,A)') &
         var, ' variable is not in input file'
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   ! Get metadata (time, dimensions, global attributes):
   rcode = nf_inq_var(cdfid, id_var, var, ivtype, ndims, dimids, natts)
   do i = 1, ndims
     rcode = nf_inq_dimlen(cdfid, dimids(i), dims(i))
   end do
   dim1 = dims(1)
   dim2 = dims(2)
   dim3 = dims(3)

   ! Get resolution:
   att_name = 'DX'  ! Assume dx= dy.
   rcode = nf_inq_att(cdfid, nf_global, att_name, ivtype, attlen)
   allocate(value_real(1:attlen))
   rcode = NF_GET_ATT_real(cdfid, nf_global, att_name, value_real)
   ds = value_real(1)
   deallocate(value_real)

   ! get MP_PHYSICS option
   if ( present(mp_physics) ) then
      mp_physics = 0 !initialize
      ! get mp_physics from the global attribute
      rcode = NF_GET_ATT_int(cdfid, nf_global, 'MP_PHYSICS', mp_physics)
   end if

   rcode = nf_close(cdfid)

   ! if (trace_use) call da_trace_exit("da_stage0_initialize")

end subroutine da_stage0_initialize


