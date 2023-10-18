subroutine da_setup_obs_structures_chem_sfc(ob, iv, grid)

   !---------------------------------------------------------------------------
   ! Purpose: Define, allocate and read observation structure.
   !---------------------------------------------------------------------------

   implicit none
   
   type (y_type),  intent(out)   :: ob          ! Observation structure.
   type (iv_type), intent(inout) :: iv          ! O-B structure.
   type (domain) , intent(inout) :: grid        ! model data

   character(len=filename_len)  :: filename
   integer                     :: n,i,j,k, itime, ichem, iplt
   
   ! thinning variables
   integer  :: istart,iend,jstart,jend
   real     :: rlonlat(4)
   integer  :: ntest

   if (trace_use) call da_trace_entry("da_setup_obs_structures_chem_sfc")
   
   filename = "ob_chemsfc.ascii"

   !--------------------------------------------------------------------------
   ! [1.0] Scan data - only necessary if an alternative format is implemented
   !--------------------------------------------------------------------------
   call da_scan_obs_chem_sfc(iv, filename, grid)

   iv%time = 1      !!! Fixed by Soyoung Ha (June-2021)
   iv%info(chemic_surf)%plocal(iv%time) = iv%info(chemic_surf)%nlocal
   iv%info(chemic_surf)%ptotal(iv%time) = iv%info(chemic_surf)%ntotal

   !--------------------------------------------------------------------------
   ! Allocate the ob based on input number of obs:
   !--------------------------------------------------------------------------
   call da_allocate_observations_chem_sfc (iv)

   !--------------------------------------------------------------------------
   ! [2.0] Read data
   !--------------------------------------------------------------------------
   call da_read_obs_chem_sfc(iv, filename, grid)

   !--------------------------------------------------------------------------
   ! [3.0] Calculate innovation vector (O-B) and create (smaller) ob structure:
   !--------------------------------------------------------------------------
   call da_fill_obs_structures_chem_sfc(iv, ob)

   if (trace_use) call da_trace_exit("da_setup_obs_structures_chem_sfc")

end subroutine da_setup_obs_structures_chem_sfc


