subroutine da_calculate_chem_forcing_ad( it, model_grid, iv )
   !-------------------------------------------------------------------------
   ! Purpose:        Calculate forcing for chemical observations 
   !
   ! Called from da_solve
   !
   ! History: 10/23/2014  Creation (Jonathan Guerrette)
   !
   !-------------------------------------------------------------------------

   integer,                        intent(in) :: it
   type(domain),                   intent(inout) :: model_grid
   type (iv_type),    intent(inout) :: iv

   integer                                    :: iobs, ichem, jchem, itime, iloc, jloc, kloc, &
                                                 ij, ilocal, iplt
   real                                       :: resi, sigi

   if (num_surf_obs > 0) then
      model_grid%a_surf_hx(:,:) = 0.D0
      ilocal = 0
      do iobs = 1, num_surf_obs
         if ( .not. model_grid%ti_surf(iobs) .GE. 1 .OR. &
              .not. model_grid%tf_surf(iobs) .LE. num_ts ) cycle
         ilocal = ilocal + 1

         do ichem = PARAM_FIRST_SCALAR, num_chem_surf
            jchem = ichem - 1
            resi = model_grid%surf_hx(iobs,ichem) - iv%chem_surf(ilocal)%chem(jchem)%inv 
            sigi = iv%chem_surf(ilocal)%chem(jchem)%error
   
            if ( it .eq. 1 ) then
                  iv%chem_surf(ilocal)%weight(jchem) = 1.D0
            end if
   
            model_grid%a_surf_hx(iobs,ichem) = &
               iv%chem_surf(ilocal)%weight(jchem) * resi / sigi**2  
   
            iloc = INT(model_grid%i_surf(iobs) + 0.5D0)
            jloc = INT(model_grid%j_surf(iobs) + 0.5D0)
            kloc = INT(model_grid%k_surf(iobs) + 0.5D0)
   
            !$OMP PARALLEL DO &
            !$OMP PRIVATE( ij )
            do ij = 1 , model_grid%num_tiles
            if ( iloc .ge. model_grid%i_start(ij) .AND. &
                 iloc .le. model_grid%i_end(ij) .AND. &
                 jloc .ge. model_grid%j_start(ij) .AND. &
                 jloc .le. model_grid%j_end(ij) ) then
  
               model_grid%J(iloc,kloc,jloc) = model_grid%J(iloc,kloc,jloc) + &
                     iv%chem_surf(ilocal)%weight(jchem) * resi**2 / sigi**2
   
               model_grid%dJ(iloc,kloc,jloc) = model_grid%dJ(iloc,kloc,jloc) + &
                     iv%chem_surf(ilocal)%weight(jchem) * resi / sigi**2
   
            end if
            end do
            !$OMP END PARALLEL DO 
         end do
      end do
   end if

   do iplt = 1, num_platform
      if (num_acft_obs > 0) then
         model_grid%a_acft_hx(:,iplt,:) = 0.D0
         ilocal = 0
         do itime = 1, num_ts
            if( .NOT. model_grid%acft_obs_count(itime,iplt) > 0 ) cycle
            ilocal = ilocal + 1
   
            do ichem = PARAM_FIRST_SCALAR, num_chem_acft
               jchem = ichem - 1
               resi = model_grid%acft_hx(itime,iplt,ichem) - iv%chem_acft(ilocal)%chem(jchem,iplt)%inv 
               sigi = iv%chem_acft(ilocal)%chem(jchem,iplt)%error
      
               if ( it .eq. 1 ) then
                     iv%chem_acft(ilocal)%weight(jchem,iplt) = 1.D0
               end if
   
               model_grid%a_acft_hx(itime,iplt,ichem) = &
                     iv%chem_acft(ilocal)%weight(jchem,iplt) * resi / sigi**2
  
               iloc = INT(model_grid%i_acft(itime,iplt) + 0.5D0)
               jloc = INT(model_grid%j_acft(itime,iplt) + 0.5D0)
               kloc = INT(model_grid%k_acft(itime,iplt) + 0.5D0)
   
               !$OMP PARALLEL DO &
               !$OMP PRIVATE( ij )
               do ij = 1 , model_grid%num_tiles
               if ( iloc .ge. model_grid%i_start(ij) .AND. &
                    iloc .le. model_grid%i_end(ij) .AND. &
                    jloc .ge. model_grid%j_start(ij) .AND. &
                    jloc .le. model_grid%j_end(ij) ) then

                  model_grid%J(iloc,kloc,jloc) = model_grid%J(iloc,kloc,jloc) + &
                        iv%chem_acft(ilocal)%weight(jchem,iplt) * resi**2 / sigi**2
   
                  model_grid%dJ(iloc,kloc,jloc) = model_grid%dJ(iloc,kloc,jloc) + &
                        iv%chem_acft(ilocal)%weight(jchem,iplt) * resi / sigi**2
   
               end if
               end do
               !$OMP END PARALLEL DO
   
            end do
         end do
      end if
   end do

end subroutine da_calculate_chem_forcing_ad
   
   
