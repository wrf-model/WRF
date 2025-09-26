subroutine da_vertical_transform(grid, string, be, vertical_wgt, vv, vp)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none   

   type (domain),    intent(in)    :: grid
   character(len=*), intent(in)    :: string      ! Character operation
   type (be_type),   intent(in)    :: be          ! Background error structure.
   real,             intent(in)    :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   type (vp_type),   intent(inout) :: vv          ! CV in gridpt/EOF space.
   type (vp_type),   intent(inout) :: vp          ! CV in gridpt/level space.

   integer                         :: j, m, n     ! Loop counters.
   real                            :: alpha_stddev_inv ! 1/ sigma_alpha
   real                            :: size_inv    ! 1 / size.
   real                            :: alpha_me, alpha_ms, alpha_sd ! Alpha statistics.

   if (trace_use) call da_trace_entry("da_vertical_transform")

   select case(string)
      
   case ('u');
      
      !-------------------------------------------------------------------
      ! [1.0] Perform vp(i,j,k) = E L^{1/2} vv(i,j,m) transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vvtovp (grid, be % v1 % evec, be % v1 % val, vertical_wgt, &
            vv % v1, vp % v1, be % v1 % mz, kte)
      else
         vp % v1(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp (grid, be % v2 % evec, be % v2 % val, vertical_wgt, &
            vv % v2, vp % v2, be % v2 % mz, kte)
      else
         vp % v2(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp (grid, be % v3 % evec, be % v3 % val, vertical_wgt, &
            vv % v3, vp % v3, be % v3 % mz, kte)
      else
         vp % v3(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp (grid, be % v4 % evec, be % v4 % val, vertical_wgt, &
            vv % v4, vp % v4, be % v4 % mz, kte)
      else
         vp % v4(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vp % v5(its:ite,jts:jte,1) = vv % v5(its:ite,jts:jte,1)
         else 
         call da_transform_vvtovp (grid, be % v5 % evec, be % v5 % val, vertical_wgt, & 
            vv % v5, vp % v5, be % v5 % mz, kts)    
         end if
      else
         vp % v5(its:ite,jts:jte,kts:kts) = 0.0
      end if

      ! for cloud_cv_options<=1 and not use_cv_w
      vp % v6  = 0.0
      vp % v7  = 0.0
      vp % v8  = 0.0
      vp % v9  = 0.0
      vp % v10 = 0.0
      vp % v11 = 0.0

      if ( cloud_cv_options == 2 ) then
         if (be % v6 % mz > 0) then
            call da_transform_vvtovp (grid, be % v6 % evec, be % v6 % val, vertical_wgt, &
               vv % v6, vp % v6, be % v6 % mz, kte)
         end if

         if (be % v7 % mz > 0) then
            call da_transform_vvtovp (grid, be % v7 % evec, be % v7 % val, vertical_wgt, &
               vv % v7, vp % v7, be % v7 % mz, kte)
         end if

         if (be % v8 % mz > 0) then
            call da_transform_vvtovp (grid, be % v8 % evec, be % v8 % val, vertical_wgt, &
               vv % v8, vp % v8, be % v8 % mz, kte)
         end if

         if (be % v9 % mz > 0) then
            call da_transform_vvtovp (grid, be % v9 % evec, be % v9 % val, vertical_wgt, &
               vv % v9, vp % v9, be % v9 % mz, kte)
         end if

         if (be % v10% mz > 0) then
            call da_transform_vvtovp (grid, be % v10 % evec, be % v10 % val, vertical_wgt, &
               vv % v10, vp % v10, be % v10 % mz, kte)
         end if
      else if ( cloud_cv_options == 3 ) then
         if ( be % v6  % mz > 0) vp % v6  = vv % v6
         if ( be % v7  % mz > 0) vp % v7  = vv % v7
         if ( be % v8  % mz > 0) vp % v8  = vv % v8
         if ( be % v9  % mz > 0) vp % v9  = vv % v9
         if ( be % v10 % mz > 0) vp % v10 = vv % v10
      end if

      if ( use_cv_w ) then
         if (be % v11 % mz > 0) then
            if ( cloud_cv_options == 2 ) then
               call da_transform_vvtovp (grid, be % v11 % evec, be % v11 % val, vertical_wgt, &
                  vv % v11, vp % v11, be % v11 % mz, kte)
            else if ( cloud_cv_options == 3 ) then
               vp % v11 = vv % v11
            end if
         end if
      end if

      if ( be % ne > 0 .and. be % alpha % mz > 0 ) then
         do n = 1, be % ne
            if ( anal_type_hybrid_dual_res ) then
               call da_transform_vvtovp_dual_res (grid%intermediate_grid, be % alpha % evec, be % alpha % val, vertical_wgt, &
                                       vv % alpha(:,:,:,n), vp % alpha(:,:,:,n), be % alpha % mz, kte_int)
            else
               call da_transform_vvtovp (grid, be % alpha % evec, be % alpha % val, vertical_wgt, &
                                       vv % alpha(:,:,:,n), vp % alpha(:,:,:,n), be % alpha % mz, kte_int)
            endif
         end do

!        Calculate alpha standard deviation diagnostic:
!         size_inv = 1.0 / ( (ite-its+1) * ( jte-jts+1) * be % ne * be % alpha % mz )
!         alpha_me = sum(vp % alpha(its:ite,jts:jte,:)) * size_inv
!         alpha_ms = sum(vp % alpha(its:ite,jts:jte,:) * vp % alpha(its:ite,jts:jte,:)) * &
!                    size_inv
!         alpha_sd = sqrt( alpha_ms - alpha_me * alpha_me )
!         write(6,'(a,f15.5)')' Alpha std. dev = ', alpha_sd
      end if

   case ('u_inv');
     
      !------------------------------------------------------------------- 
      ! [2.0] Perform vv(i,j,m) = L^{-1/2} E^T vp(i,j,k) transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vptovv (be % v1 % evec, be % v1 % val, vertical_wgt, &
            vp % v1, vv % v1, be % v1 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte)
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vptovv (be % v2 % evec, be % v2 % val, vertical_wgt, &
            vp % v2, vv % v2, be % v2 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte)
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vptovv (be % v3 % evec, be % v3 % val, vertical_wgt, &
            vp % v3, vv % v3, be % v3 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte)
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vptovv (be % v4 % evec, be % v4 % val, vertical_wgt, &
            vp % v4, vv % v4, be % v4 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte)
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
         else
            call da_transform_vptovv (be % v5 % evec, be % v5 % val, vertical_wgt, &
               vp % v5, vv % v5, be % v5 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if
      end if

      if ( cloud_cv_options == 2 ) then
         if (be % v6 % mz > 0) then
            call da_transform_vptovv (be % v6 % evec, be % v6 % val, vertical_wgt, &
               vp % v6, vv % v6, be % v6 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if

         if (be % v7 % mz > 0) then
            call da_transform_vptovv (be % v7 % evec, be % v7 % val, vertical_wgt, &
               vp % v7, vv % v7, be % v7 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if

         if (be % v8 % mz > 0) then
            call da_transform_vptovv (be % v8 % evec, be % v8 % val, vertical_wgt, &
               vp % v8, vv % v8, be % v8 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if

         if (be % v9 % mz > 0) then
            call da_transform_vptovv (be % v9 % evec, be % v9 % val, vertical_wgt, &
               vp % v9, vv % v9, be % v9 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if

         if (be % v10 % mz > 0) then
            call da_transform_vptovv (be % v10 % evec, be % v10 % val, vertical_wgt, &
               vp % v10, vv % v10, be % v10 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
               its,ite, jts,jte, kts,kte)
         end if

      else if ( cloud_cv_options == 3 ) then
         if (be % v6 % mz > 0) then
            vv % v6 = vp % v6
         end if

         if (be % v7 % mz > 0) then
            vv % v7 = vp % v7
         end if

         if (be % v8 % mz > 0) then
            vv % v8 = vp % v8
         end if

         if (be % v9 % mz > 0) then
            vv % v9 = vp % v9
         end if

         if (be % v10 % mz > 0) then
            vv % v10 = vp % v10
         end if

      end if

      if ( use_cv_w ) then
         if (be % v11 % mz > 0) then
             if ( cloud_cv_options == 2 ) then
                call da_transform_vptovv (be % v11 % evec, be % v11 % val, vertical_wgt, &
                   vp % v11, vv % v11, be % v11 % mz, kds,kde, ims,ime, jms,jme, kms,kme, &
                   its,ite, jts,jte, kts,kte)
             else if ( cloud_cv_options == 3 ) then
                vv % v11 = vp % v11
             end if
         end if
      end if

      if ( be % ne > 0 .and. be % alpha % mz > 0 ) then
         do n = 1, be % ne
!           call da_transform_vptovv (be % alpha % evec, be % alpha % val, vertical_wgt, &
!                                     vp % alpha(:,:,:,n), vv % alpha(:,:,:,n), be % alpha % mz, kds,kde, &
!                                     ims,ime, jms,jme, kms,kme, its,ite, jts,jte, kts,kte)
            call da_transform_vptovv (be % alpha % evec, be % alpha % val, vertical_wgt, &
                                      vp % alpha(:,:,:,n), vv % alpha(:,:,:,n), be % alpha % mz, kds_int,kde_int, &
                                      ims_int,ime_int, jms_int,jme_int, kms_int,kme_int, its_int,ite_int, &
                                      jts_int,jte_int, kts_int,kte_int)
         end do
      end if

   case ('u_adj');
    
      !------------------------------------------------------------------- 
      ! [3.0] Perform vv_adj = U_{v}^{T} vp_adj transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vvtovp_adj (grid, be % v1 % evec, be % v1 % val, vertical_wgt, &
            vp % v1, vv % v1, be % v1 % mz, kte)
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp_adj (grid, be % v2 % evec, be % v2 % val, vertical_wgt, &
            vp % v2, vv % v2, be % v2 % mz, kte)
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp_adj (grid, be % v3 % evec, be % v3 % val, vertical_wgt, &
            vp % v3, vv % v3, be % v3 % mz, kte)
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp_adj (grid, be % v4 % evec, be % v4 % val, vertical_wgt, &
            vp % v4, vv % v4, be % v4 % mz, kte)
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
         else
            call da_transform_vvtovp_adj (grid, be % v5 % evec, be % v5 % val, vertical_wgt, &
               vp % v5, vv % v5, be % v5 % mz, kts)
         end if
      end if

      if ( cloud_cv_options == 2 ) then
         if (be % v6 % mz > 0) then
            call da_transform_vvtovp_adj (grid, be % v6 % evec, be % v6 % val, vertical_wgt, &
               vp % v6, vv % v6, be % v6 % mz, kte)
         end if

         if (be % v7 % mz > 0) then
            call da_transform_vvtovp_adj (grid, be % v7 % evec, be % v7 % val, vertical_wgt, &
               vp % v7, vv % v7, be % v7 % mz, kte)
         end if

         if (be % v8 % mz > 0) then
            call da_transform_vvtovp_adj (grid, be % v8 % evec, be % v8 % val, vertical_wgt, &
               vp % v8, vv % v8, be % v8 % mz, kte)
         end if

         if (be % v9 % mz > 0) then
            call da_transform_vvtovp_adj (grid, be % v9 % evec, be % v9 % val, vertical_wgt, &
               vp % v9, vv % v9, be % v9 % mz, kte)
         end if

         if (be % v10 % mz > 0) then
            call da_transform_vvtovp_adj (grid, be % v10 % evec, be % v10 % val, vertical_wgt, &
               vp % v10, vv % v10, be % v10 % mz, kte)
         end if

      else if ( cloud_cv_options == 3 ) then
         if (be % v6 % mz > 0) then
            vv % v6 = vp % v6
         end if

         if (be % v7 % mz > 0) then
            vv % v7 = vp % v7
         end if

         if (be % v8 % mz > 0) then
            vv % v8 = vp % v8
         end if

         if (be % v9 % mz > 0) then
            vv % v9 = vp % v9
         end if

         if (be % v10 % mz > 0) then
            vv % v10 = vp % v10
         end if

      end if

      if ( use_cv_w ) then
         if (be % v11 % mz > 0) then
            if ( cloud_cv_options == 2 ) then
               call da_transform_vvtovp_adj (grid, be % v11 % evec, be % v11 % val, vertical_wgt, &
                  vp % v11, vv % v11, be % v11 % mz, kte)
            else if ( cloud_cv_options == 3 ) then
               vv % v11 = vp % v11
            end if
         end if
      end if

      if ( be % ne > 0 .and. be % alpha % mz > 0 ) then
         do n = 1, be % ne
            if ( anal_type_hybrid_dual_res ) then
               call da_transform_vvtovp_adj_dual_res (grid%intermediate_grid, be % alpha % evec, be % alpha % val, vertical_wgt, &
                                           vp % alpha(:,:,:,n), vv % alpha(:,:,:,n), be % alpha % mz, kte_int)
            else
               call da_transform_vvtovp_adj (grid, be % alpha % evec, be % alpha % val, vertical_wgt, &
                                           vp % alpha(:,:,:,n), vv % alpha(:,:,:,n), be % alpha % mz, kte_int)
            endif
         end do
      end if

   case default;
   
      call da_error(__FILE__,__LINE__, &
         (/"Invalid da_vertical_transform option "//string/))

   end select

   if (trace_use) call da_trace_exit("da_vertical_transform")

end subroutine da_vertical_transform


