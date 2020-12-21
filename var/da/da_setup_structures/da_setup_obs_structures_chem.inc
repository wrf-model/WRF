subroutine da_setup_obs_structures_chem(grid, ob, iv)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (domain) , intent(inout) :: grid        ! model data
   type (y_type),  intent(out)   :: ob          ! Observation structure.
   type (iv_type), intent(inout) :: iv          ! O-B structure.

   character(len=filename_len)  :: filename
   integer                     :: n,i,j,k, itime, ichem, iplt
   
   ! thinning variables
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)
   integer  :: ntest

   if (trace_use) call da_trace_entry("da_setup_obs_structures_chem")

   !--------------------------------------------------------------------------
   ! [1.0] Scan data - only necessary if an alternative format is implemented
   !--------------------------------------------------------------------------

   iv%info(chem_surf)%ntotal = num_surf_obs
   iv%info(chem_surf)%nlocal = num_surf_obs

!   if (num_acft_obs <= 0) then
!      num_ts = 0
!   end if

   if (num_acft_obs <= 0) then
      iv%info(chem_acft)%ntotal = 0
      iv%info(chem_acft)%nlocal = 0
   else
      iv%info(chem_acft)%ntotal = num_ts
      iv%info(chem_acft)%nlocal = num_ts
   end if

   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------

   call da_allocate_observations_chem (iv)

   !--------------------------------------------------------------------------
   ! [2.0] Read data
   !--------------------------------------------------------------------------
   call da_read_obs_chem(grid, iv)

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------

   call da_fill_obs_structures_chem(iv, ob)

   if (trace_use) call da_trace_exit("da_setup_obs_structures_chem")

end subroutine da_setup_obs_structures_chem


