subroutine da_vertical_transform(string, be, vertical_wgt, vv, vp)

   !---------------------------------------------------------------------
   ! Purpose: TBD
   !---------------------------------------------------------------------

   implicit none   

   character(len=*), intent(in)    :: string      ! Character operation
   type (be_type),   intent(in)    :: be          ! Background error structure.
   real,             intent(in)    :: vertical_wgt(ims:ime,jms:jme,kms:kme) ! Weighting.
   type (vp_type),   intent(inout) :: vv          ! CV in gridpt/EOF space.
   type (vp_type),   intent(inout) :: vp          ! CV in gridpt/level space.

   integer                         :: j, m        ! Loop counters.
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
         call da_transform_vvtovp (be % v1 % evec, be % v1 % val, vertical_wgt, &
            vv % v1, vp % v1, be % v1 % mz, kte)
      else
         vp % v1(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp (be % v2 % evec, be % v2 % val, vertical_wgt, &
            vv % v2, vp % v2, be % v2 % mz, kte)
      else
         vp % v2(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp (be % v3 % evec, be % v3 % val, vertical_wgt, &
            vv % v3, vp % v3, be % v3 % mz, kte)
      else
         vp % v3(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp (be % v4 % evec, be % v4 % val, vertical_wgt, &
            vv % v4, vp % v4, be % v4 % mz, kte)
      else
         vp % v4(its:ite,jts:jte,kts:kte) = 0.0
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vp % v5(its:ite,jts:jte,1) = vv % v5(its:ite,jts:jte,1)
         else 
            call da_transform_vvtovp (be % v5 % evec, be % v5 % val, vertical_wgt, & 
               vv % v5, vp % v5, be % v5 % mz, kts)    
         end if
      else
         vp % v5(its:ite,jts:jte,kts:kts) = 0.0
      end if

      if (be % ne > 0) then
         size_inv = 1.0 / ( (ite-its+1) * ( jte-jts+1) * be % ne )
         do m = 1, be % ne
            do j = jts, jte
               vp % alpha(its:ite,j,m) = vv % alpha(its:ite,j,m) * be % alpha % val(j,m)
            end do
         end do

!        Calculate alpha standard deviation diagnostic:
         alpha_me = sum(vp % alpha(its:ite,jts:jte,:)) * size_inv
         alpha_ms = sum(vp % alpha(its:ite,jts:jte,:) * vp % alpha(its:ite,jts:jte,:)) * &
                    size_inv
         alpha_sd = sqrt( alpha_ms - alpha_me * alpha_me )
         write(6,'(a,f15.5)')' Alpha std. dev = ', alpha_sd
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

      if (be % ne > 0) then
         do m = 1, be % ne
            do j = jts, jte
               alpha_stddev_inv = 1.0 / be % alpha % val(j,m)
               vv % alpha(its:ite,j,m) = vp % alpha(its:ite,j,m) * alpha_stddev_inv
            end do
         end do
      end if

   case ('u_adj');
    
      !------------------------------------------------------------------- 
      ! [3.0] Perform vv_adj = U_{v}^{T} vp_adj transform:
      !------------------------------------------------------------------- 

      if (be % v1 % mz > 0) then
         call da_transform_vvtovp_adj (be % v1 % evec, be % v1 % val, vertical_wgt, &
            vp % v1, vv % v1, be % v1 % mz, kte)
      end if

      if (be % v2 % mz > 0) then
         call da_transform_vvtovp_adj (be % v2 % evec, be % v2 % val, vertical_wgt, &
            vp % v2, vv % v2, be % v2 % mz, kte)
      end if

      if (be % v3 % mz > 0) then
         call da_transform_vvtovp_adj (be % v3 % evec, be % v3 % val, vertical_wgt, &
            vp % v3, vv % v3, be % v3 % mz, kte)
      end if

      if (be % v4 % mz > 0) then
         call da_transform_vvtovp_adj (be % v4 % evec, be % v4 % val, vertical_wgt, &
            vp % v4, vv % v4, be % v4 % mz, kte)
      end if

      if (be % v5 % mz > 0) then
         if (global) then
            vv % v5(its:ite,jts:jte,1) = vp % v5(its:ite,jts:jte,1)
         else
            call da_transform_vvtovp_adj (be % v5 % evec, be % v5 % val, vertical_wgt, &
               vp % v5, vv % v5, be % v5 % mz, kts)
         end if
      end if

      if (be % ne > 0) then
         do m = 1, be % ne
            do j = jts, jte
               vv % alpha(its:ite,j,m) = vp % alpha(its:ite,j,m) * be % alpha % val(j,m)
            end do
         end do
      end if

   case default;
   
      call da_error(__FILE__,__LINE__, &
         (/"Invalid da_vertical_transform option "//string/))

   end select

   if (trace_use) call da_trace_exit("da_vertical_transform")

end subroutine da_vertical_transform


