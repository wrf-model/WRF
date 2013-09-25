subroutine da_test_vxtransform( grid, xbx, be, vv, vp)

   !---------------------------------------------------------------------------
   ! Purpose: Perform inverse/adjoint tests on control variable transform.
   !
   ! Method:  1) Test inverse and adjoint of physical variable transform.
   !          2) Test inverse and adjoint of vertical transform.
   !          3) Perform adjoint test on complete transform: <x,x> = <v_adj,v>.
   !
   !---------------------------------------------------------------------------

   implicit none

   type(domain),   intent(inout) :: grid
   type(xbx_type), intent(in)    :: xbx   ! Header & non-gridded vars.
   type(be_type),  intent(in)    :: be    ! background error structure.
   type(vp_type),  intent(out)   :: vp    ! Test CV structure.
   type(vp_type),  intent(out)   :: vv    ! Test CV structure.

   real          :: cv(1:cv_size) ! Test control variable.
   integer       :: i   
   type(vp_type) :: vp1, vv1     ! Test CV structure.
   real          :: field(ims:ime, jms:jme)
   real          :: xa2_u  (ims:ime, jms:jme, kms:kme)
   real          :: xa2_v  (ims:ime, jms:jme, kms:kme)
   real          :: xa2_t  (ims:ime, jms:jme, kms:kme)
   real          :: xa2_p  (ims:ime, jms:jme, kms:kme)
   real          :: xa2_q  (ims:ime, jms:jme, kms:kme)
   real          :: xa2_rh (ims:ime, jms:jme, kms:kme)
   real          :: xa2_rho(ims:ime, jms:jme, kms:kme)
   real          :: xa2_qt (ims:ime, jms:jme, kms:kme)
   real          :: xa2_qcw(ims:ime, jms:jme, kms:kme)
   real          :: xa2_qrn(ims:ime, jms:jme, kms:kme)
   real          :: xa2_w  (ims:ime, jms:jme, kms:kme)

   if (trace_use) call da_trace_entry("da_test_vxtransform")

   allocate (vp1 % v1(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v2(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v3(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v4(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v5(ims:ime,jms:jme,kms:kme))
#ifdef CLOUD_CV
   allocate (vp1 % v6(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v7(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v8(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v9(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v10(ims:ime,jms:jme,kms:kme))
   allocate (vp1 % v11(ims:ime,jms:jme,kms:kme))
#endif

   allocate (vv1 % v1(ims:ime,jms:jme,1:kz_vv(1)))
   allocate (vv1 % v2(ims:ime,jms:jte,1:kz_vv(2)))
   allocate (vv1 % v3(ims:ime,jms:jme,1:kz_vv(3)))
   allocate (vv1 % v4(ims:ime,jms:jme,1:kz_vv(4)))
   allocate (vv1 % v5(ims:ime,jms:jme,1:kz_vv(5)))
#ifdef CLOUD_CV
   allocate (vv1 % v6(ims:ime,jms:jme,1:kz_vv(6)))
   allocate (vv1 % v7(ims:ime,jms:jme,1:kz_vv(7)))
   allocate (vv1 % v8(ims:ime,jms:jme,1:kz_vv(8)))
   allocate (vv1 % v9(ims:ime,jms:jme,1:kz_vv(9)))
   allocate (vv1 % v10(ims:ime,jms:jme,1:kz_vv(10)))
   allocate (vv1 % v11(ims:ime,jms:jme,1:kz_vv(11)))
#endif 
   call da_zero_vp_type(vp1)
   call da_zero_vp_type(vv1)

   write(unit=stdout, fmt='(/a/)') 'da_testvxtransform:'

   write(unit=stdout, fmt='(/a/)') '---------------------------------------'

   ! Make cv all constant value 1.0

   call random_number(cv(:))
   cv(:) = cv(:) - 0.5
   call da_zero_x(grid%xa)
   
   if ( global ) then
      write(unit=stdout, fmt='(/a/)') ' Inverse tests not performed for global application'
      go to 1111
   end if

   if ( cv_options /= 5 ) then
      write(unit=stdout, fmt='(/a/)') ' Inverse tests not performed for cv_options=',cv_options
      go to 1111
   end if

#ifdef DM_PARALLEL
   write(unit=stdout, fmt='(/a/)') ' Inverse tests will not be done as it is MPP run'
#else
   write(unit=stdout, fmt='(/a/)') &
         ' Inverse tests follows:'
   call da_transform_vtox( grid, xbx, be, cv, vv, vp)
   call da_transform_xtoxa( grid )

   ! Store grid%xa, Vv & Vp for inverse test

   xa2_u(ims:ime,jms:jme,:) = grid%xa % u(ims:ime,jms:jme,:)
   xa2_v(ims:ime,jms:jme,:) = grid%xa % v(ims:ime,jms:jme,:)
   xa2_w(ims:ime,jms:jme,:) = grid%xa % w(ims:ime,jms:jme,:)
   xa2_t(ims:ime,jms:jme,:) = grid%xa % t(ims:ime,jms:jme,:)
   xa2_p(ims:ime,jms:jme,:) = grid%xa % p(ims:ime,jms:jme,:)
   xa2_q(ims:ime,jms:jme,:) = grid%xa % q(ims:ime,jms:jme,:)
   xa2_rho(ims:ime,jms:jme,:) = grid%xa % rho(ims:ime,jms:jme,:)

   if ( cv_options_hum == 2 ) then
      xa2_rh(ims:ime,jms:jme,:) = grid%xa % rh(ims:ime,jms:jme,:)
   end if

   vv1 % v1(its:ite,jts:jte,1:kz_vv(1)) = vv % v1(its:ite,jts:jte,1:kz_vv(1))
   vv1 % v2(its:ite,jts:jte,1:kz_vv(2)) = vv % v2(its:ite,jts:jte,1:kz_vv(2))
   vv1 % v3(its:ite,jts:jte,1:kz_vv(3)) = vv % v3(its:ite,jts:jte,1:kz_vv(3))
   vv1 % v4(its:ite,jts:jte,1:kz_vv(4)) = vv % v4(its:ite,jts:jte,1:kz_vv(4))
   vv1 % v5(its:ite,jts:jte,1:kz_vv(5)) = vv % v5(its:ite,jts:jte,1:kz_vv(5))
#ifdef CLOUD_CV
   vv1 % v6(its:ite,jts:jte,1:kz_vv(6)) = vv % v6(its:ite,jts:jte,1:kz_vv(6))
   vv1 % v7(its:ite,jts:jte,1:kz_vv(7)) = vv % v7(its:ite,jts:jte,1:kz_vv(7))
   vv1 % v8(its:ite,jts:jte,1:kz_vv(8)) = vv % v8(its:ite,jts:jte,1:kz_vv(8))
   vv1 % v9(its:ite,jts:jte,1:kz_vv(9)) = vv % v9(its:ite,jts:jte,1:kz_vv(9))
   vv1 % v10(its:ite,jts:jte,1:kz_vv(10)) = vv % v10(its:ite,jts:jte,1:kz_vv(10))
   vv1 % v11(its:ite,jts:jte,1:kz_vv(11)) = vv % v11(its:ite,jts:jte,1:kz_vv(11))
#endif

   vp1 % v1(its:ite,jts:jte,1:kz_vp(1)) = vp % v1(its:ite,jts:jte,1:kz_vp(1))
   vp1 % v2(its:ite,jts:jte,1:kz_vp(2)) = vp % v2(its:ite,jts:jte,1:kz_vp(2))
   vp1 % v3(its:ite,jts:jte,1:kz_vp(3)) = vp % v3(its:ite,jts:jte,1:kz_vp(3))
   vp1 % v4(its:ite,jts:jte,1:kz_vp(4)) = vp % v4(its:ite,jts:jte,1:kz_vp(4))
   vp1 % v5(its:ite,jts:jte,1:kz_vp(5)) = vp % v5(its:ite,jts:jte,1:kz_vp(5))
#ifdef CLOUD_CV
   vp1 % v6(its:ite,jts:jte,1:kz_vp(6)) = vp % v6(its:ite,jts:jte,1:kz_vp(6))
   vp1 % v7(its:ite,jts:jte,1:kz_vp(7)) = vp % v7(its:ite,jts:jte,1:kz_vp(7))
   vp1 % v8(its:ite,jts:jte,1:kz_vp(8)) = vp % v8(its:ite,jts:jte,1:kz_vp(8))
   vp1 % v9(its:ite,jts:jte,1:kz_vp(9)) = vp % v9(its:ite,jts:jte,1:kz_vp(9))
   vp1 % v10(its:ite,jts:jte,1:kz_vp(10)) = vp % v10(its:ite,jts:jte,1:kz_vp(10))
   vp1 % v11(its:ite,jts:jte,1:kz_vp(11)) = vp % v11(its:ite,jts:jte,1:kz_vp(11))
#endif
   !----------------------------------------------------------------------
   ! [1.0]: Perform VvToVpToVv test:                        
   !----------------------------------------------------------------------
   ! call da_transform_xtovp( grid, xbx, vp, be)

   if ( vert_corr == vert_corr_2 ) then
      ! perform vv = u_v^{-1} vp transform:
      call da_vertical_transform (grid, 'u_inv', be, grid%xb % vertical_inner_product, vv, vp)
   else
      vv % v1(its:ite,jts:jte,1:kz_vv(1)) = vp % v1(its:ite,jts:jte,1:kz_vp(1))
      vv % v2(its:ite,jts:jte,1:kz_vv(2)) = vp % v2(its:ite,jts:jte,1:kz_vp(2))
      vv % v3(its:ite,jts:jte,1:kz_vv(3)) = vp % v3(its:ite,jts:jte,1:kz_vp(3))
      vv % v4(its:ite,jts:jte,1:kz_vv(4)) = vp % v4(its:ite,jts:jte,1:kz_vp(4))
      vv % v5(its:ite,jts:jte,1:kz_vv(5)) = vp % v5(its:ite,jts:jte,1:kz_vp(5))
#ifdef CLOUD_CV 
      vv % v6(its:ite,jts:jte,1:kz_vv(6)) = vp % v6(its:ite,jts:jte,1:kz_vp(6))
      vv % v7(its:ite,jts:jte,1:kz_vv(7)) = vp % v7(its:ite,jts:jte,1:kz_vp(7))
      vv % v8(its:ite,jts:jte,1:kz_vv(8)) = vp % v8(its:ite,jts:jte,1:kz_vp(8))
      vv % v9(its:ite,jts:jte,1:kz_vv(9)) = vp % v9(its:ite,jts:jte,1:kz_vp(9))
      vv % v10(its:ite,jts:jte,1:kz_vv(10)) = vp % v10(its:ite,jts:jte,1:kz_vp(10))
      vv % v11(its:ite,jts:jte,1:kz_vv(11)) = vp % v11(its:ite,jts:jte,1:kz_vp(11))
#endif
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vvtovptovv_errors'

   call da_check_vp_errors (vv1, vv, kz_vv, its,ite, jts,jte, kts,kte)

   !----------------------------------------------------------------------
   ! [2.0]: Perform VpToVvToVp test:                        
   !----------------------------------------------------------------------
   if ( vert_corr == vert_corr_2 ) then
      ! perform vp = u_v (vv) transform:
      call da_vertical_transform (grid, 'u', be, grid%xb % vertical_inner_product, vv, vp)
   else
      vp % v1(its:ite,jts:jte,1:kz_vv(1)) = vv % v1(its:ite,jts:jte,1:kz_vv(1))
      vp % v2(its:ite,jts:jte,1:kz_vv(2)) = vv % v2(its:ite,jts:jte,1:kz_vv(2))
      vp % v3(its:ite,jts:jte,1:kz_vv(3)) = vv % v3(its:ite,jts:jte,1:kz_vv(3))
      vp % v4(its:ite,jts:jte,1:kz_vv(4)) = vv % v4(its:ite,jts:jte,1:kz_vv(4))
      vp % v5(its:ite,jts:jte,1:kz_vv(5)) = vv % v5(its:ite,jts:jte,1:kz_vv(5))
#ifdef CLOUD_CV
      vp % v6(its:ite,jts:jte,1:kz_vv(6)) = vv % v6(its:ite,jts:jte,1:kz_vv(6))
      vp % v7(its:ite,jts:jte,1:kz_vv(7)) = vv % v7(its:ite,jts:jte,1:kz_vv(7))
      vp % v8(its:ite,jts:jte,1:kz_vv(8)) = vv % v8(its:ite,jts:jte,1:kz_vv(8))
      vp % v9(its:ite,jts:jte,1:kz_vv(9)) = vv % v9(its:ite,jts:jte,1:kz_vv(9))
      vp % v10(its:ite,jts:jte,1:kz_vv(10)) = vv % v10(its:ite,jts:jte,1:kz_vv(10))
      vp % v11(its:ite,jts:jte,1:kz_vv(11)) = vv % v11(its:ite,jts:jte,1:kz_vv(11))
#endif
   end if

   ! Check inverse errors:

   write(unit=stdout, fmt='(/a/)') 'da_check_vptovvtovp_errors'

   call da_check_vp_errors( vp1, vp, kz_vv, its,ite, jts,jte, kts,kte )

   !----------------------------------------------------------------------
   ! [3.0] Check_CvToVv_Adjoint:
   !----------------------------------------------------------------------
   
   call da_check_cvtovv_adjoint( grid, xbx, be, cv, vv)
  
   !----------------------------------------------------------------------
   ! [4.0] Test inverse physical variable transform:
   ! Note: Currently these test are developed for regional only
   !----------------------------------------------------------------------

   if (.not. global) then
      call da_zero_x(grid%xa)
      call da_transform_vptox( grid, vp, be)
      ! [4.1] Test XToVpToX differences:

      write(unit=stdout, fmt='(/a/)') &
         'da_check_xtovptox_errors'
   
      call da_check_xtovptox_errors( grid%xa, xa2_u, xa2_v, xa2_w, xa2_t, &
         xa2_p, xa2_q,  xa2_rho, xa2_qt, xa2_qcw, xa2_qrn)

      ! [4.2] Perform v_{p} = U_{p}^{-1} x transform (again):

      call da_transform_xtovp( grid, xbx, vv, be)
      
      ! [2.4] Check inverse errors:

      write(unit=stdout, fmt='(/a/)') 'da_check_vptoxtovp_errors'

      call da_check_vp_errors( vp, vv, kz_vp, its,ite, jts,jte, kts,kte )
   end if

   !----------------------------------------------------------------------
   ! [5.0] Perform Vv -> Vp adjoint test: 
   !----------------------------------------------------------------------

   call da_check_vvtovp_adjoint( grid, be, vp, vv)

   !----------------------------------------------------------------------
   ! [6.0] Perform Vp -> X  adjoint tests: 
   !----------------------------------------------------------------------

   call da_check_vptox_adjoint( grid, be, vp)

#endif
1111 continue

   !----------------------------------------------------------------------
   ! [7.0]: Perform adjoint test on complete transform: <x,x> = <v_adj,v>
   !----------------------------------------------------------------------

   call da_check_vtox_adjoint( grid, xbx, be, cv, vv, vp)

   !----------------------------------------------------------------------
   ! [8.0]: Perform Spectral transform tests for Global                    
   !----------------------------------------------------------------------

   if (global) then   
#ifdef DM_PARALLEL
      write (unit=stdout,fmt=*)  &
         ' Inverse tests for spectral transforms will not be done as it is MPP run'
#else
      call random_number(field(:,:) )
      field(:,:) = field(:,:) - 0.5
      call da_test_spectral(xbx, field)
#endif
   end if

   deallocate (vp1 % v1)
   deallocate (vp1 % v2)
   deallocate (vp1 % v3)
   deallocate (vp1 % v4)
   deallocate (vp1 % v5)
#ifdef CLOUD_CV
   deallocate (vp1 % v6)
   deallocate (vp1 % v7)
   deallocate (vp1 % v8)
   deallocate (vp1 % v9)
   deallocate (vp1 % v10)
   deallocate (vp1 % v11)
#endif

   deallocate (vv1 % v1)
   deallocate (vv1 % v2)
   deallocate (vv1 % v3)
   deallocate (vv1 % v4)
   deallocate (vv1 % v5)
#ifdef CLOUD_CV
   deallocate (vv1 % v6)
   deallocate (vv1 % v7)
   deallocate (vv1 % v8)
   deallocate (vv1 % v9)
   deallocate (vv1 % v10)
   deallocate (vv1 % v11)
#endif
   if (trace_use) call da_trace_exit("da_test_vxtransform") 

end subroutine da_test_vxtransform


