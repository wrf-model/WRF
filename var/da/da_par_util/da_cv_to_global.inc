subroutine da_cv_to_global(cv_size, cv_size_global, x, grid, mzs, nmzs, xg )


   !-----------------------------------------------------------------------
   ! Purpose: Gathers local cv-array x into domain cv-array xg(:).  
   ! Global cv-array xg will only be valid on the "monitor" task.  
   !
   ! Must be called by all MPI tasks.  
   !-----------------------------------------------------------------------

   implicit none

   integer,          intent(in)    :: cv_size        ! Size of local cv-array
   integer,          intent(in)    :: cv_size_global ! Size of domain cv-array
   real,             intent(in)    :: x(1:cv_size)   ! local cv-array
   type(domain),     intent(in)    :: grid
   integer,          intent(in)    :: nmzs           ! number of mz
   integer,          intent(in)    :: mzs(nmzs)      ! mz for each variable
                                         ! (to identify empty or 2D arrays)
   real,             intent(inout) :: xg(1:cv_size_global) ! global cv-array

#ifdef DM_PARALLEL

   ! Local declarations
   type (vp_type) :: vv_x    ! Grdipt/EOF cv_array (local)
   type (vp_type) :: vv_xg   ! Grdipt/EOF cv_array (global)
   type (xpose_type) :: xpg  ! global dimensions
   integer :: ids, ide, jds, jde, kds, &
              ims, ime, jms, jme, kms, &
              ips, ipe, jps, jpe, kps 

   integer :: n

   if (trace_use) call da_trace_entry("da_cv_to_global")      

   !
   ! Gather to mimic serial summation order.  
   !

   ! k?e varies with variable v1 - v5
   ids = ids; ide = ide; jds = jds; jde = jde; kds = kds
   ims = ims; ime = ime; jms = jms; jme = jme; kms = grid%xp%kms
   ips = grid%xp%ips; ipe = grid%xp%ipe; jps = grid%xp%jps; jpe = grid%xp%jpe; kps = grid%xp%kps

   ! TOdo:  encapsulate this crap!  
   ! allocate vv_x
   allocate(vv_x%v1(ims:ime,jms:jme,mzs(1)))
   allocate(vv_x%v2(ims:ime,jms:jme,mzs(2)))
   allocate(vv_x%v3(ims:ime,jms:jme,mzs(3)))
   allocate(vv_x%v4(ims:ime,jms:jme,mzs(4)))
   allocate(vv_x%v5(ims:ime,jms:jme,mzs(5)))
   if ( nmzs > 10 ) then
      allocate(vv_x%v6(ims:ime,jms:jme,mzs(6)))
      allocate(vv_x%v7(ims:ime,jms:jme,mzs(7)))
      allocate(vv_x%v8(ims:ime,jms:jme,mzs(8)))
      allocate(vv_x%v9(ims:ime,jms:jme,mzs(9)))
      allocate(vv_x%v10(ims:ime,jms:jme,mzs(10)))
   end if
   if ( nmzs == 8 .or. nmzs == 13 ) then
      allocate(vv_x%v11(ims:ime,jms:jme,mzs(nmzs-2)))
   end if
   ! the 4th dimension of vv_x%alpha is the number of ensembles
   allocate(vv_x%alpha(ims_int:ime_int,jms_int:jme_int,kms_int:kme_int,mzs(nmzs)))

   call da_cv_to_vv (cv_size, x, mzs, nmzs, vv_x )

   if (rootproc) then
      ! allocate vv_xg
      allocate(vv_xg%v1(ids:ide,jds:jde,mzs(1)))
      allocate(vv_xg%v2(ids:ide,jds:jde,mzs(2)))
      allocate(vv_xg%v3(ids:ide,jds:jde,mzs(3)))
      allocate(vv_xg%v4(ids:ide,jds:jde,mzs(4)))
      allocate(vv_xg%v5(ids:ide,jds:jde,mzs(5)))
      if ( nmzs > 10 ) then
         allocate(vv_xg%v6(ids:ide,jds:jde,mzs(6)))
         allocate(vv_xg%v7(ids:ide,jds:jde,mzs(7)))
         allocate(vv_xg%v8(ids:ide,jds:jde,mzs(8)))
         allocate(vv_xg%v9(ids:ide,jds:jde,mzs(9)))
         allocate(vv_xg%v10(ids:ide,jds:jde,mzs(10)))
      end if
      if ( nmzs == 8 .or. nmzs == 13 ) then
         allocate(vv_xg%v11(ids:ide,jds:jde,mzs(nmzs-2)))
      end if
      ! the 4th dimension of vv_xg%alpha is the number of ensembles
      allocate(vv_xg%alpha(ids_int:ide_int,jds_int:jde_int,kds_int:kde_int,mzs(nmzs)))
   else
      ! Allocate dummy array for non-monitor process to keep Fortran
      ! CICO happy...
      allocate(vv_xg%v1(1,1,1))
      allocate(vv_xg%v2(1,1,1))
      allocate(vv_xg%v3(1,1,1))
      allocate(vv_xg%v4(1,1,1))
      allocate(vv_xg%v5(1,1,1))
      if ( nmzs > 10 ) then
         allocate(vv_xg%v6(1,1,1))
         allocate(vv_xg%v7(1,1,1))
         allocate(vv_xg%v8(1,1,1))
         allocate(vv_xg%v9(1,1,1))
         allocate(vv_xg%v10(1,1,1))
      end if
      if ( nmzs == 8 .or. nmzs == 13 ) then
         allocate(vv_xg%v11(1,1,1))
      end if
      allocate(vv_xg%alpha(1,1,1,1))
   end if

   ! TOdo:  encapsulate this crap!  
   ! gather to global data structures
   ! it is possible to gather straight into a globally-sized cv-array
   ! TOdo:  add this optimization later
   call da_patch_to_global(grid, vv_x%v1,    vv_xg%v1,    mzs(1))
   call da_patch_to_global(grid, vv_x%v2,    vv_xg%v2,    mzs(2))
   call da_patch_to_global(grid, vv_x%v3,    vv_xg%v3,    mzs(3))
   call da_patch_to_global(grid, vv_x%v4,    vv_xg%v4,    mzs(4))
   call da_patch_to_global(grid, vv_x%v5,    vv_xg%v5,    mzs(5))
   if ( nmzs > 10 ) then
      call da_patch_to_global(grid, vv_x%v6,  vv_xg%v6,  mzs(6))
      call da_patch_to_global(grid, vv_x%v7,  vv_xg%v7,  mzs(7))
      call da_patch_to_global(grid, vv_x%v8,  vv_xg%v8,  mzs(8))
      call da_patch_to_global(grid, vv_x%v9,  vv_xg%v9,  mzs(9))
      call da_patch_to_global(grid, vv_x%v10, vv_xg%v10, mzs(10))
   end if
   if ( nmzs == 8 .or. nmzs == 13 ) then
      call da_patch_to_global(grid, vv_x%v11, vv_xg%v11, mzs(nmzs-2))
   end if
   if ( mzs(nmzs) > 0 ) then
      do n = 1, mzs(nmzs) ! Ensemble size
         if ( anal_type_hybrid_dual_res ) then
            call da_patch_to_global_dual_res(grid%intermediate_grid, vv_x%alpha(:,:,:,n), vv_xg%alpha(:,:,:,n), mzs(nmzs-1))
         else
            call da_patch_to_global(grid, vv_x%alpha(:,:,:,n), vv_xg%alpha(:,:,:,n), mzs(nmzs-1))
         endif
      end do
   end if

   ! deallocate vv_x
   deallocate (vv_x%v1, vv_x%v2, vv_x%v3, vv_x%v4, vv_x%v5, vv_x%alpha)
   if ( nmzs > 10 ) then
      deallocate (vv_x%v6, vv_x%v7, vv_x%v8, vv_x%v9, vv_x%v10)
   end if
   if ( nmzs == 8 .or. nmzs == 13 ) then
      deallocate (vv_x%v11)
   end if
   if (rootproc) then
      ! finally, collapse data back into a globally-sized cv-array
      xpg%ids = ids; xpg%ide = ide
      xpg%ims = ids; xpg%ime = ide
      xpg%its = ids; xpg%ite = ide
      xpg%jds = jds; xpg%jde = jde
      xpg%jms = jds; xpg%jme = jde
      xpg%jts = jds; xpg%jte = jde
      xpg%kds = kds; xpg%kde = kde
      xpg%kms = kds; xpg%kme = kde
      xpg%kts = kds; xpg%kte = kde
      call da_vv_to_cv(vv_xg, xpg, mzs, nmzs, cv_size_global, xg)
   end if
   ! deallocate vv_xg
   deallocate (vv_xg%v1, vv_xg%v2, vv_xg%v3, vv_xg%v4, vv_xg%v5, vv_xg%alpha)
   if ( nmzs > 10 ) then
      deallocate (vv_xg%v6, vv_xg%v7, vv_xg%v8, vv_xg%v9, vv_xg%v10)
   end if
   if ( nmzs == 8 .or. nmzs == 13 ) then
      deallocate (vv_xg%v11)
   end if

   if (trace_use) call da_trace_exit("da_cv_to_global")

#endif

end subroutine da_cv_to_global


