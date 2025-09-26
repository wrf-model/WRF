subroutine da_write_kma_increments(grid)

   !---------------------------------------------------------------------------
   ! Purpose: Gathers KMA analysis increments and writes 
   !           on "anl_inc_unit" unit 
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in) :: grid

   ! Arrays for write out increments:
   integer                                     :: ix, jy, kz

#ifdef DM_PARALLEL
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy)          :: gbuf_2d
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz) :: gbuf
   real, dimension(:,:)  , allocatable :: psfc_g
   real, dimension(:,:,:), allocatable :: u_g, v_g, t_g, q_g, p_g
#endif

   integer                                     :: i, j, k,anl_inc_unit

   if (trace_use) call da_trace_entry("da_write_kma_increments")

   ! Dimension of the domain:
   ix = grid%xb%mix
   jy = grid%xb%mjy
   kz = grid%xb%mkz

#ifdef DM_PARALLEL
 
   ! 3-d and 2-d increments:

   allocate (psfc_g (1:ix,1:jy))
   allocate (   u_g (1:ix,1:jy,1:kz))
   allocate (   v_g (1:ix,1:jy,1:kz))
   allocate (   t_g (1:ix,1:jy,1:kz))
   allocate (   q_g (1:ix,1:jy,1:kz))
   allocate (   p_g (1:ix,1:jy,1:kz))

   call da_patch_to_global(grid, grid%xa%psfc, gbuf_2d) 
   if (rootproc) then 
      psfc_g(1:ix,1:jy) = gbuf_2d(1:ix,1:jy) 
   end if 

   call da_patch_to_global(grid, grid%xa%u, gbuf) 
   if (rootproc) then 
      u_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, grid%xa%v, gbuf) 
   if (rootproc) then 
      v_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, grid%xa%t, gbuf) 
   if (rootproc) then 
      t_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, grid%xa%q, gbuf) 
   if (rootproc) then 
      q_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, grid%xa%p, gbuf) 
   if (rootproc) then 
      p_g(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 
#endif

   if (rootproc) then
      ! 3d- and 2d-increments:

      call da_get_unit(anl_inc_unit)
      open(unit=anl_inc_unit,file="analysis_increments_kma",status="replace", &
         form="unformatted")
#ifdef DM_PARALLEL
      write(anl_inc_unit) ((psfc_g(i,j),i=ids,ide),j=jds,jde)
      write(anl_inc_unit) (((u_g(i,j,k),i=ids,ide),j=ids,jde),k=kds,kde)
      write(anl_inc_unit) (((v_g(i,j,k),i=ids,ide),j=ids,jde),k=kds,kde)
      write(anl_inc_unit) (((t_g(i,j,k),i=ids,ide),j=ids,jde),k=kds,kde)
      write(anl_inc_unit) (((q_g(i,j,k),i=ids,ide),j=ids,jde),k=kds,kde)
      write(anl_inc_unit) (((p_g(i,j,k),i=ids,ide),j=ids,jde),k=kds,kde)
#else
      write(anl_inc_unit) ((grid%xa%psfc(i,j),i=ids,ide),j=jds,jde)
      write(anl_inc_unit) (((grid%xa%u(i,j,k),i=ids,ide),j=jds,jde),k=kds,kde)
      write(anl_inc_unit) (((grid%xa%v(i,j,k),i=ids,ide),j=jds,jde),k=kds,kde)
      write(anl_inc_unit) (((grid%xa%t(i,j,k),i=ids,ide),j=jds,jde),k=kds,kde)
      write(anl_inc_unit) (((grid%xa%q(i,j,k),i=ids,ide),j=jds,jde),k=kds,kde)
      write(anl_inc_unit) (((grid%xa%p(i,j,k),i=ids,ide),j=jds,jde),k=kds,kde)
#endif
      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)
   end if

   if (trace_use) call da_trace_exit("da_write_kma_increments")

end subroutine da_write_kma_increments 


