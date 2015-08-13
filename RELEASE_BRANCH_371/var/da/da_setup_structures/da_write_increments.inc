subroutine da_write_increments (grid, q_cgrid, mu_cgrid, ph_cgrid)

   !----------------------------------------------------------------------
   ! Purpose: Write analysis increments
   !----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)                         :: grid
   real,intent(in) :: q_cgrid(ims:ime,jms:jme,kms:kme)
   real,intent(in) :: ph_cgrid(ims:ime,jms:jme,kms:kme)
   real,intent(in) :: mu_cgrid(ims:ime,jms:jme)

   ! Arrays for write out increments:
   integer                                          :: ix, jy, kz
#ifdef DM_PARALLEL
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy)               ::     gbuf_2d
   real, dimension(1:grid%xb%mix+1,1:grid%xb%mjy+1)           ::     gbuf_2dd
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz)      ::     gbuf

   real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz+1)    ::    wgbuf
   real, dimension(:,:,:), allocatable :: u_global, v_global, w_global, &
                                          p_global, t_global, q_global, &
                                         ph_global
   real, dimension(:,:)  , allocatable :: mu_global, psfc_global, &
                       psac_global, tgrn_global, terr_global, snow_global,&
                        lat_global,  lon_global, lanu_global,             &
                 map_factor_global, cori_global, landmask_global
#endif

   integer :: anl_inc_unit

   if (trace_use) call da_trace_entry("da_write_increments")


   ! Dimension of the domain:
   ix = grid%xb%mix
   jy = grid%xb%mjy
   kz = grid%xb%mkz

#ifdef DM_PARALLEL
 
   ! 3-d and 2-d increments:

   allocate (   p_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   t_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   q_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   u_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   v_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (   w_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (  ph_global (1:ix+1,1:jy+1,1:kz+1))
   allocate (psfc_global (1:ix+1,1:jy+1))
   allocate (  mu_global (1:ix+1,1:jy+1))
   call da_patch_to_global(grid, grid%xa % p, gbuf) 
   if (rootproc) then 
      p_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_patch_to_global(grid, grid%xa % t, gbuf) 
   if (rootproc) then 
      t_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_patch_to_global(grid, q_cgrid, gbuf) 
   if (rootproc) then 
      q_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_patch_to_global(grid, grid%xa % u, gbuf) 
   if (rootproc) then 
      u_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
   call da_patch_to_global(grid, grid%xa % v, gbuf) 
   if (rootproc) then 
      v_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if

   ! One more level for w and ph:
   grid%xp%kde=grid%xp%kde+1
   kde=kde+1
   call da_patch_to_global(grid, grid%xa % w, wgbuf) 
   if (rootproc) then 
      w_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
   end if 
   call da_patch_to_global(grid, ph_cgrid, wgbuf) 
   if (rootproc) then 
      ph_global(1:ix,1:jy,1:kz+1) = wgbuf(1:ix,1:jy,1:kz+1) 
   end if 
   kde=kde-1
   grid%xp%kde=grid%xp%kde-1
 
   call da_patch_to_global(grid, grid%xa % psfc, gbuf_2d) 
   if (rootproc) then 
      psfc_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 
   call da_patch_to_global(grid, mu_cgrid, gbuf_2d) 
   if (rootproc) then 
      mu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 

   ! 2d constant fields:

   allocate (      psac_global (1:ix+1,1:jy+1))
   allocate (      tgrn_global (1:ix+1,1:jy+1))
   allocate (      terr_global (1:ix+1,1:jy+1))
   allocate (      snow_global (1:ix+1,1:jy+1))
   allocate (       lat_global (1:ix+1,1:jy+1))
   allocate (       lon_global (1:ix+1,1:jy+1))
   allocate (      lanu_global (1:ix+1,1:jy+1))
   allocate (map_factor_global (1:ix+1,1:jy+1))
   allocate (      cori_global (1:ix+1,1:jy+1))
   allocate (  landmask_global (1:ix+1,1:jy+1))

   call da_patch_to_global(grid, grid%xb%psac, gbuf_2d) 
   if (rootproc) then 
      psac_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%tgrn, gbuf_2d) 
   if (rootproc) then 
      tgrn_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%terr, gbuf_2d) 
   if (rootproc) then 
      terr_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%snow, gbuf_2d) 
   if (rootproc) then 
      snow_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%lat , gbuf_2d) 
   if (rootproc) then 
      lat_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%lon , gbuf_2d) 
   if (rootproc) then 
      lon_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%lanu, gbuf_2d) 
   if (rootproc) then 
      lanu_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if
   call da_patch_to_global(grid, grid%xb%map_factor, gbuf_2d) 
   if (rootproc) then 
      map_factor_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if

   ! temporary increase to dimensions for cori
   ide=ide+1
   jde=jde+1
   grid%xp%ide=grid%xp%ide+1
   grid%xp%jde=grid%xp%jde+1
   call da_patch_to_global(grid, grid%xb%cori, gbuf_2dd) 
   if (rootproc) then
      cori_global(1:ix+1,1:jy+1) = gbuf_2dd(1:ix+1,1:jy+1) 
   end if
   ide=ide-1
   jde=jde-1
   grid%xp%ide=grid%xp%ide-1
   grid%xp%jde=grid%xp%jde-1

   call da_patch_to_global(grid, grid%xb%landmask, gbuf_2d)
   if (rootproc) then 
      landmask_global(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if

#endif

   if (rootproc) then
      call da_get_unit(anl_inc_unit)
      open(unit=anl_inc_unit, file='analysis_increments', form='unformatted')

      write (unit=anl_inc_unit) ANALYSIS_DATE

      write (unit=anl_inc_unit) 1, ix, 1, jy, 1, kz 

      ! Map projection information:
      write (unit=anl_inc_unit) map_projection, coarse_ix, coarse_jy
      write (unit=anl_inc_unit) &
         coarse_ds, start_x, start_y, &
         phic, xlonc, cone_factor, truelat1_3dv, truelat2_3dv, pole, dsm,   &
         psi1, c2, ptop, base_pres, t0, base_lapse, base_temp

      ! 1d constant fields:

      write (unit=anl_inc_unit) grid%xb%sigmah, grid%xb%sigmaf

#ifdef DM_PARALLEL

      ! 3d- and 2d-increments:
      write (unit=anl_inc_unit) u_global, v_global, w_global, p_global, &
         t_global, q_global, ph_global, mu_global, psfc_global

      ! 2d-constant fields:
      write (unit=anl_inc_unit) psac_global, tgrn_global, terr_global, &
         snow_global, lat_global, lon_global, lanu_global, map_factor_global, &
         cori_global, landmask_global
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)
#else

      ! 3d- and 2d-increments:
      write (unit=anl_inc_unit) grid%xa%u(1:ix+1,1:jy+1,1:kz+1), &
                    grid%xa%v(1:ix+1,1:jy+1,1:kz+1), &
                    grid%xa%w(1:ix+1,1:jy+1,1:kz+1), &
                    grid%xa%p(1:ix+1,1:jy+1,1:kz+1), &
                    grid%xa%t(1:ix+1,1:jy+1,1:kz+1), &
                    q_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    ph_cgrid(1:ix+1,1:jy+1,1:kz+1), &
                    mu_cgrid(1:ix+1,1:jy+1), &
                    grid%xa%psfc(1:ix+1,1:jy+1)

      !    .. 2d-constant fields:
      write (unit=anl_inc_unit) grid%xb%psac(1:ix+1,1:jy+1), &
                    grid%xb%tgrn(1:ix+1,1:jy+1), &
                    grid%xb%terr(1:ix+1,1:jy+1), &
                    grid%xb%snow(1:ix+1,1:jy+1), &
                    grid%xb%lat(1:ix+1,1:jy+1), &
                    grid%xb%lon(1:ix+1,1:jy+1), &
                    grid%xb%lanu(1:ix+1,1:jy+1), &
                    grid%xb%map_factor(1:ix+1,1:jy+1), &
                    grid%xb%cori(1:ix+1,1:jy+1), &
                    grid%xb%landmask(1:ix+1,1:jy+1)
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)
#endif

   end if

   if (trace_use) call da_trace_exit("da_write_increments")

end subroutine da_write_increments


