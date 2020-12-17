subroutine da_swap_xtraj ( grid )

   !-------------------------------------------------------------------------
   ! Purpose:        Swap KJ dimensions of fields from WRF to fit the fields of WRFDA
   !
   ! History: 07/16/2010  Creation (Xin Zhang )
   !
   !-------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout)     :: grid

#ifdef VAR4D

   integer                           :: i

   call kj_swap_reverse (model_grid%u_2, grid%u_2, &
           grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%v_2, grid%v_2, &
           grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%w_2, grid%w_2, &
           grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%t_2, grid%t_2, &
           grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%ph_2, grid%ph_2, &
          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%tslb, grid%tslb, &
          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, 1, grid%num_soil_layers)
   call kj_swap_reverse (model_grid%p, grid%p, &
          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!  call kj_swap_reverse (model_grid%pb, grid%pb, &
!         grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
!  call kj_swap_reverse (model_grid%phb, grid%phb, &
!         grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%smois, grid%smois, &
          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, 1, grid%num_soil_layers)
   do i = PARAM_FIRST_SCALAR, num_moist
      call kj_swap_reverse (model_grid%moist(:,:,:,i), grid%moist(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   enddo
   grid%mu_2 = model_grid%mu_2
   grid%psfc = model_grid%psfc
   grid%tsk = model_grid%tsk
   grid%u10 = model_grid%u10
   grid%v10 = model_grid%v10
   grid%t2 = model_grid%t2
   grid%q2 = model_grid%q2
!  grid%tmn = model_grid%tmn

   grid%snowc = model_grid%snowc
   grid%snowh = model_grid%snowh
   grid%rainnc = model_grid%rainnc
   grid%rainncv = model_grid%rainncv
   grid%rainc = model_grid%rainc
   grid%raincv = model_grid%raincv

!  grid%mub = model_grid%mub
!  grid%dx = model_grid%dx
!  grid%znw = model_grid%znw
!  grid%znu = model_grid%znu
!  grid%dn = model_grid%dn
!  grid%dnw = model_grid%dnw
!  grid%msft = model_grid%msft
!  grid%sst = model_grid%sst
!  grid%ht = model_grid%ht
!  grid%f = model_grid%f
!  grid%xlat = model_grid%xlat
!  grid%xlong = model_grid%xlong
!  grid%map_proj = model_grid%map_proj
!  grid%lu_index = model_grid%lu_index
!  grid%landmask = model_grid%landmask
!  grid%xland = model_grid%xland
!  grid%xice = model_grid%xice
!  grid%ivgtyp = model_grid%ivgtyp
!  grid%isltyp = model_grid%isltyp
!  grid%vegfra = model_grid%vegfra
!  grid%snowh = model_grid%snowh

#endif
end subroutine da_swap_xtraj

