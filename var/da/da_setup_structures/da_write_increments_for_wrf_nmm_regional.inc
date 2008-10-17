subroutine da_write_increments_for_wrf_nmm_regional (grid)                                 

   !----------------------------------------------------------------------
   ! Purpose: Write analysis increments
   !----------------------------------------------------------------------

   implicit none

   type (domain), intent(inout)                                    :: grid
   integer                                                         :: ix, jy, kz
#ifdef DM_PARALLEL
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy)                    :: gbuf_2d
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz)      :: gbuf
#endif

   integer :: anl_inc_unit

   if (trace_use) call da_trace_entry("da_write_increments_for_wrf_nmm_regional")


   ! Dimension of the domain:
   ix = grid%xb%mix
   jy = grid%xb%mjy
   kz = grid%xb%mkz

   if (rootproc) then
      call da_get_unit(anl_inc_unit)
      open(unit=anl_inc_unit, file='analysis_increments_for_wrf-nmm', form='unformatted')

      write (unit=anl_inc_unit) analysis_date 

      write (unit=anl_inc_unit) ix,jy, kz 
   end if


#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xa % u, gbuf) 
#endif
   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf(1:ix,1:jy,1:kz) 
#else
      write (unit=anl_inc_unit) grid%xa%u(1:ix,1:jy,1:kz) 
#endif
   end if 

#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xa % v, gbuf) 
#endif
   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf(1:ix,1:jy,1:kz) 
#else
      write (unit=anl_inc_unit) grid%xa%v(1:ix,1:jy,1:kz) 
#endif
   end if 

#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xa % t, gbuf) 
#endif
   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf(1:ix,1:jy,1:kz) 
#else
      write (unit=anl_inc_unit) grid%xa%t(1:ix,1:jy,1:kz) 
#endif
   end if 

#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xa % q, gbuf) 
#endif
   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf(1:ix,1:jy,1:kz) 
#else
      write (unit=anl_inc_unit) grid%xa%q(1:ix,1:jy,1:kz) 
#endif
   end if 


#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xa % psfc, gbuf_2d) 
#endif

   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf_2d(1:ix,1:jy) 
#else
      write (unit=anl_inc_unit) grid%xa%psfc(1:ix,1:jy)
#endif
   end if 

#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xb % lat, gbuf_2d) 
#endif

   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf_2d(1:ix,1:jy) 
#else
      write (unit=anl_inc_unit) grid%xb%lat(1:ix,1:jy)
#endif
   end if 

#ifdef DM_PARALLEL
   call da_patch_to_global(grid, grid%xb % lon, gbuf_2d) 
#endif

   if (rootproc) then 
#ifdef DM_PARALLEL
      write (unit=anl_inc_unit) gbuf_2d(1:ix,1:jy) 
#else
      write (unit=anl_inc_unit) grid%xb%lon(1:ix,1:jy)
#endif
   end if 

      close(anl_inc_unit)
      call da_free_unit(anl_inc_unit)

   if (trace_use) call da_trace_exit("da_write_increments_for_wrf_nmm_regional")

end subroutine da_write_increments_for_wrf_nmm_regional
