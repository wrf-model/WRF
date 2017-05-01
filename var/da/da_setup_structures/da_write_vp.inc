subroutine da_write_vp (grid,vp,filename)

   !----------------------------------------------------------------------
   ! Purpose: Write vp, full varibles after balance transform Up
   !          will be interpolated into higher resolution by offline program
   ! Method:  based on da_write_increments.inc
   ! Author:  Zhiquan (Jake) Liu, NCAR/MMM, 2015-09
   !----------------------------------------------------------------------

   implicit none

   type (domain), intent(in)      :: grid
   type(vp_type), intent(in)      :: vp
   character(len=16), intent(in) :: filename

   ! Arrays for write out increments:
   integer                                          :: ix, jy, kz
#ifdef DM_PARALLEL
   !real, dimension(1:grid%xb%mix,1:grid%xb%mjy)               ::     gbuf_2d
   !real, dimension(1:grid%xb%mix+1,1:grid%xb%mjy+1)           ::     gbuf_2dd
   real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz) ::     gbuf

   !real, dimension(1:grid%xb%mix,1:grid%xb%mjy,1:grid%xb%mkz+1)    ::    wgbuf
   real, dimension(:,:,:), allocatable :: v1_global, v2_global, &
                                          v3_global, v4_global
   real, dimension(:,:,:)  , allocatable :: v5_global
#endif

   integer :: vp_unit, vp_local_unit
   character(len=7) :: vpfile

   if (trace_use) call da_trace_entry("da_write_vp")


   ! Dimension of the domain (unstagered):
   ix = grid%xb%mix
   jy = grid%xb%mjy
   kz = grid%xb%mkz

#ifdef DM_PARALLEL
 
   ! 3-d and 2-d increments:

   allocate (   v1_global (1:ix,1:jy,1:kz))
   allocate (   v2_global (1:ix,1:jy,1:kz))
   allocate (   v3_global (1:ix,1:jy,1:kz))
   allocate (   v4_global (1:ix,1:jy,1:kz))
   allocate (   v5_global (1:ix,1:jy,1:kz))

   call da_patch_to_global(grid, vp % v1, gbuf)  ! psi or u
   if (rootproc) then 
      v1_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, vp % v2, gbuf)  ! chi_u or v
   if (rootproc) then 
      v2_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   !call da_patch_to_global(grid, grid%xa % t, gbuf)  ! t_u or t
   call da_patch_to_global(grid, vp % v3, gbuf)  ! t_u or t
   if (rootproc) then 
      v3_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

   call da_patch_to_global(grid, vp % v4, gbuf)  ! q/qs
   if (rootproc) then 
      v4_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if

   !print *, "local size v5: ", size(vp % v5,1),size(vp % v5,2),size(vp % v5,3)
   call da_patch_to_global(grid, vp % v5, gbuf) ! Ps (:,:,1)
   if (rootproc) then 
      v5_global(1:ix,1:jy,1:kz) = gbuf(1:ix,1:jy,1:kz) 
   end if 

      !write(unit=vpfile,fmt='(a,i4.4)') 'vp_',myproc
      !call da_get_unit(vp_local_unit)
      !open(unit=vp_local_unit, file=trim(vpfile), form='unformatted')

      !print *, "local: ips,ipe,jps,jpe,kps,kpe=", ips,ipe,jps,jpe,kps,kpe
      !print *, "local: ims,ime,jms,jme,kms,kme=", ims,ime,jms,jme,kms,kme
      !print *, "local: dimx, dimy, dimz=", size(vp%v5,1),size(vp%v5,2),size(vp%v5,3)

      !write (unit=vp_local_unit) ips,ipe,jps,jpe,kps,kpe, &
      !                           ims,ime,jms,jme,kms,kme, &
      !              size(vp%v5,1),size(vp%v5,2),size(vp%v5,3)

      !write (unit=vp_local_unit) vp%v1, vp%v2, &
      !                     vp%v3, vp%v4, vp%v5

      !close(vp_local_unit)
      !call da_free_unit(vp_local_unit)


#endif

   if (rootproc) then
      call da_get_unit(vp_unit)
      open(unit=vp_unit, file=trim(filename), form='unformatted')

      !print *, "ANALYSIS_DATE= ", ANALYSIS_DATE
      !write (unit=vp_unit) ANALYSIS_DATE

      print *, "write_vp: Global ix, jy, kz=", ix, jy, kz
      write (unit=vp_unit) ix, jy, kz 

#ifdef DM_PARALLEL

      ! 3d- and 2d-increments in vp space:
      write (unit=vp_unit) v1_global, v2_global, &
                           v3_global, v4_global, v5_global

      close(vp_unit)
      call da_free_unit(vp_unit)

#else

      ! 3d- and 2d-increments:
      write (unit=vp_unit) vp%v1(1:ix,1:jy,1:kz), &
                           vp%v2(1:ix,1:jy,1:kz), &
                           vp%v3(1:ix,1:jy,1:kz), &
                           vp%v4(1:ix,1:jy,1:kz), &
                           vp%v5(1:ix,1:jy,1)

      close(vp_unit)
      call da_free_unit(vp_unit)
#endif

   end if

   if (trace_use) call da_trace_exit("da_write_vp")

end subroutine da_write_vp


