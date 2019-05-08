subroutine da_retrieve_chem_hx( model_grid, iv )
   !-------------------------------------------------------------------------
   ! Purpose:        Retrieve local modeled observations from WRFPLUS 
   !                 model_grid and distribute to all processes in
   !                 both head_grid and model_grid
   !
   ! Called from da_nl_model
   !
   ! History: 06/2015  Creation (Jonathan Guerrette)
   !
   !-------------------------------------------------------------------------

   type(domain),      intent(inout) :: model_grid
   type (iv_type),    intent(inout) :: iv
   integer                                    :: iobs, ichem, itime, iplt, &
                                                 iloc, jloc, kloc, ij, ilocal
   real, dimension(num_surf_obs) :: dummy_ins, dummy_outs
   real, dimension(num_ts) :: dummy_ina, dummy_outa
   real :: k_at_p

   if (num_surf_obs > 0) then
      do ichem = PARAM_FIRST_SCALAR, num_surf_hx
#ifdef DM_PARALLEL
         dummy_ins(:) = 0.D0
         dummy_outs(:) = 0.D0

         do iobs = 1, num_surf_obs
             dummy_ins(iobs) = model_grid%surf_hx(iobs,ichem)
         end do

         call mpi_barrier(comm, ierr)
         call wrf_dm_sum_reals(dummy_ins,  dummy_outs)

         model_grid%surf_hx(:,ichem) = dummy_outs
         head_grid%surf_hx(:,ichem) = dummy_outs
#else
         head_grid%surf_hx(:,ichem) = model_grid%surf_hx(:,ichem)
#endif
      end do
   end if

   do iplt = 1, num_platform
      if (num_acft_obs > 0) then
         do ichem = PARAM_FIRST_SCALAR, num_acft_hx
#ifdef DM_PARALLEL
            dummy_ina(:) = 0.D0
            dummy_outa(:) = 0.D0
   
            do itime = 1, num_ts
                dummy_ina(itime) = model_grid%acft_hx(itime,iplt,ichem)
            end do
            call mpi_barrier(comm, ierr)
            call wrf_dm_sum_reals(dummy_ina,  dummy_outa)
   
            model_grid%acft_hx(:,iplt,ichem) = dummy_outa
            head_grid%acft_hx(:,iplt,ichem) = dummy_outa
#else
            head_grid%acft_hx(:,iplt,ichem) = model_grid%acft_hx(:,iplt,ichem)
#endif
         end do
   
         ilocal = 0 
         do itime = 1, num_ts
            if( .NOT. model_grid%acft_obs_count(itime,iplt) > 0 ) cycle
            iloc = INT(model_grid%i_acft(itime,iplt) + 0.5D0)
            jloc = INT(model_grid%j_acft(itime,iplt) + 0.5D0)
            k_at_p=0.D0
            if( pinterp_option .NE. 0 ) then
               !$OMP PARALLEL DO &
               !$OMP PRIVATE( ij )
               do ij = 1 , model_grid%num_tiles
                  if ( iloc .ge. model_grid%i_start(ij) .AND. &
                     iloc .le. model_grid%i_end(ij) .AND. &
                     jloc .ge. model_grid%j_start(ij) .AND. &
                     jloc .le. model_grid%j_end(ij) ) then
                     k_at_p = model_grid%k_acft(itime,iplt)
                  end if
               end do
               !$OMP END PARALLEL DO
#ifdef DM_PARALLEL
               call mpi_barrier(comm, ierr)
               k_at_p = wrf_dm_sum_real(k_at_p)
#endif
               model_grid%k_acft(itime,iplt) = k_at_p
               head_grid%k_acft(itime,iplt) = k_at_p
            end if

            ilocal = ilocal + 1
            iv%info(chem_acft)%k(iplt,ilocal) = head_grid%k_acft(itime,iplt)
          
         end do
      end if
   end do

end subroutine da_retrieve_chem_hx

