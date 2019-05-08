subroutine da_copy_chem_to_model( wrfplus_grid, it )
   !-------------------------------------------------------------------------
   ! Purpose:        Copy the following chem data from 
   !                 WRFDA head_grid to WRFPLUS wrfplus_grid:
   !                 - Aircraft and surface observation meta data and 
   !                 - Emission scaling factors
   !
   ! Called from da_nl_model
   !
   ! History: 06/10/2014  Creation (Jonathan Guerrette)
   !
   !-------------------------------------------------------------------------

   type(domain),                   intent(inout) :: wrfplus_grid
   integer,                        intent(in)    :: it
   integer                                    :: iobs, iplt, ic, i, j, k

   if (num_surf_obs > 0) then
      do iobs = 1, num_surf_obs
         wrfplus_grid%ti_surf(iobs) = head_grid%ti_surf(iobs)
         wrfplus_grid%tf_surf(iobs) = head_grid%tf_surf(iobs)
         wrfplus_grid%i_surf(iobs) = head_grid%i_surf(iobs)
         wrfplus_grid%j_surf(iobs) = head_grid%j_surf(iobs)
         wrfplus_grid%k_surf(iobs) = head_grid%k_surf(iobs)
      end do
   end if

   if (num_acft_obs > 0) then
      wrfplus_grid%k_acft(:,:) = 0.D0
      do iplt = 1, num_platform
      do iobs = 1, num_ts
         wrfplus_grid%i_acft(iobs,iplt) = head_grid%i_acft(iobs,iplt)
         wrfplus_grid%j_acft(iobs,iplt) = head_grid%j_acft(iobs,iplt)
         if( pinterp_option .EQ. 0 ) then
            wrfplus_grid%k_acft(iobs,iplt) = head_grid%k_acft(iobs,iplt)
         end if
         wrfplus_grid%p_acft(iobs,iplt) = head_grid%p_acft(iobs,iplt)
         wrfplus_grid%acft_obs_count(iobs,iplt) = head_grid%acft_obs_count(iobs,iplt)
      end do
      end do
   end if

   head_grid%surf_hx(:,:) = 0.D0
   wrfplus_grid%surf_hx(:,:) = 0.D0
   head_grid%acft_hx(:,:,:) = 0.D0
   wrfplus_grid%acft_hx(:,:,:) = 0.D0

   head_grid%J = 0.D0
   wrfplus_grid%J = 0.D0

   if ( it .lt. 2 ) then
      do ic = PARAM_FIRST_SCALAR, num_scaleant
         do j = jms, jme
            do k = 1, num_ant_steps
               do i = ims, ime
                  wrfplus_grid%scaleant(i,k,j,ic) = head_grid%scaleant(i,j,k,ic)
               end do
            end do
         end do
      enddo
      do ic = PARAM_FIRST_SCALAR, num_scalebb
         do j = jms, jme
            do k = 1, num_bb_steps
               do i = ims, ime
                  wrfplus_grid%scalebb(i,k,j,ic) = head_grid%scalebb(i,j,k,ic)
               end do
            end do
         end do
      enddo
   end if


end subroutine da_copy_chem_to_model

