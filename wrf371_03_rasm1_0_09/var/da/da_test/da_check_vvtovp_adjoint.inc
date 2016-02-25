subroutine da_check_vvtovp_adjoint(grid, ne, xb, be, vv, vp)

   !---------------------------------------------------------------------------
   ! Purpose: Test Vv to Vp routine and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < Vp, Vp > = < Vv_adj, Vv >.
   !---------------------------------------------------------------------------

   implicit none

   type (domain), intent(in)         :: grid
   integer, intent(in)               :: ne    ! Ensemble size.
   type (xb_type), intent(in)        :: xb    ! first guess (local).
   type (be_type), intent(in)        :: be    ! background error structure.
   type (vp_type), intent(inout)     :: vv    ! CV(i,j,m).
   type (vp_type), intent(inout)     :: vp    ! CV(i,j,k)

   real                              :: adj_par_lhs ! < x, x >
   real                              :: adj_par_rhs ! < v_adj, v >
   real                              :: adj_sum_lhs ! < x, x >
   real                              :: adj_sum_rhs ! < v_adj, v >

   real                              :: vv2_v1(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v2(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v3(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v4(ims:ime,jms:jme,kms:kme)
   real                              :: vv2_v5(ims:ime,jms:jme,kms:kme)
!  real                              :: vv2_alpha(ims:ime,jms:jme,kts:kte,1:ne)
   real                              :: vv2_alpha(ims_int:ime_int,jms_int:jme_int,kts_int:kte_int,1:ne)

   if (trace_use) call da_trace_entry("da_check_vvtovp_adjoint")

   if (cv_options == 3 ) then
      write(unit=stdout, fmt='(/a,i2,a/)') 'cv_options =',cv_options, &
                     '   no da_check_vvtovp_adjoint check...'
      goto 1235
   end if

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   write(unit=stdout, fmt='(/a/)') 'da_check_vvtovp_adjoint: Test Results:'
      
   !----------------------------------------------------------------------
   ! [2.0] Perform Vp = U_v Vv transform:
   !----------------------------------------------------------------------

   call da_vertical_transform(grid, 'u', be, &
                               xb % vertical_inner_product, &
                               vv, vp)

   !----------------------------------------------------------------------
   ! [3.0] Calculate LHS of adjoint test equation:
   !----------------------------------------------------------------------

   adj_par_lhs = sum(vp % v1(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp1_sumsq &
               + sum(vp % v2(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp2_sumsq &
               + sum(vp % v3(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp3_sumsq &
               + sum(vp % v4(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp4_sumsq &
               + sum(vp % v5(its:ite,jts:jte,kts:kte)**2) * inv_typ_vp5_sumsq

   if (be % ne > 0) then
      adj_par_lhs = adj_par_lhs + &
!        sum(vp % alpha(its:ite,jts:jte,kts:kte,1:be%ne)**2) * inv_typ_vpalpha_sumsq
         sum(vp % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne)**2) * inv_typ_vpalpha_sumsq
   end if

   !----------------------------------------------------------------------
   ! [4.0] Rescale input to adjoint routine:
   !----------------------------------------------------------------------

   vp % v1(its:ite,jts:jte,kts:kte) = vp % v1(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp1_sumsq
   vp % v2(its:ite,jts:jte,kts:kte) = vp % v2(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp2_sumsq
   vp % v3(its:ite,jts:jte,kts:kte) = vp % v3(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp3_sumsq
   vp % v4(its:ite,jts:jte,kts:kte) = vp % v4(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp4_sumsq
   vp % v5(its:ite,jts:jte,kts:kte) = vp % v5(its:ite,jts:jte,kts:kte) * &
      inv_typ_vp5_sumsq

   if (be % ne > 0) then
!     vp % alpha(its:ite,jts:jte,kts:kte,1:be%ne) = &
!        vp % alpha(its:ite,jts:jte,kts:kte,1:be%ne) * inv_typ_vpalpha_sumsq
      vp % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) = &
         vp % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) * inv_typ_vpalpha_sumsq
   end if

   !----------------------------------------------------------------------
   ! [5.0] Perform adjoint operation:
   !----------------------------------------------------------------------

   vv2_v1(its:ite,jts:jte,1:be%v1%mz) = vv % v1(its:ite,jts:jte,1:be%v1%mz)
   vv2_v2(its:ite,jts:jte,1:be%v2%mz) = vv % v2(its:ite,jts:jte,1:be%v2%mz)
   vv2_v3(its:ite,jts:jte,1:be%v3%mz) = vv % v3(its:ite,jts:jte,1:be%v3%mz)
   vv2_v4(its:ite,jts:jte,1:be%v4%mz) = vv % v4(its:ite,jts:jte,1:be%v4%mz)
   vv2_v5(its:ite,jts:jte,1:be%v5%mz) = vv % v5(its:ite,jts:jte,1:be%v5%mz)

   if (be % ne > 0) then
!     vv2_alpha(its:ite,jts:jte,kts:kte,1:be%ne) = vv % alpha(its:ite,jts:jte,kts:kte,1:be%ne)
      vv2_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) = &
           vv % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne)
   end if      

   call da_vertical_transform(grid, 'u_adj', be, &
                               xb % vertical_inner_product, &
                               vv, vp)

   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------

   adj_par_rhs = 0.0
   if (be % v1 % mz > 0) &
      adj_par_rhs = sum(vv % v1(its:ite,jts:jte,1:be%v1%mz) * &
         vv2_v1(its:ite,jts:jte,1:be%v1%mz)) + adj_par_rhs
   if (be % v2 % mz > 0) &
      adj_par_rhs = sum(vv % v2(its:ite,jts:jte,1:be%v2%mz) * &
         vv2_v2(its:ite,jts:jte,1:be%v2%mz)) + adj_par_rhs
   if (be % v3 % mz > 0) &
      adj_par_rhs = sum(vv % v3(its:ite,jts:jte,1:be%v3%mz) * &
         vv2_v3(its:ite,jts:jte,1:be%v3%mz)) + adj_par_rhs
   if (be % v4 % mz > 0) &
      adj_par_rhs = sum(vv % v4(its:ite,jts:jte,1:be%v4%mz) * &
         vv2_v4(its:ite,jts:jte,1:be%v4%mz)) + adj_par_rhs
   if (be % v5 % mz == 1) &
      adj_par_rhs = sum(vv % v5(its:ite,jts:jte,1:be%v5%mz) * &
         vv2_v5(its:ite,jts:jte,1:be%v5%mz)) + adj_par_rhs
!  if (be % ne > 0) &
!     adj_par_rhs = sum(vv % alpha(its:ite,jts:jte,kts:kte,1:be%ne) * &
!        vv2_alpha(its:ite,jts:jte,kts:kte,1:be%ne)) + adj_par_rhs
   if (be % ne > 0) &
      adj_par_rhs = sum(vv % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) * &
         vv2_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne)) + adj_par_rhs

   !----------------------------------------------------------------------
   ! [7.0] Print output:
   !----------------------------------------------------------------------

   write(unit=stdout, fmt='(a,1pe22.14)') &
        'Single domain < vp,     vp > = ', adj_par_lhs, &
        'Single domain < Vv_adj, Vv > = ', adj_par_rhs

   adj_sum_lhs = wrf_dm_sum_real(adj_par_lhs)
   adj_sum_rhs = wrf_dm_sum_real(adj_par_rhs)


   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write(unit=stdout, fmt='(a,1pe22.14)') &
         'Whole  Domain: < Vp, Vp >     = ', adj_sum_lhs, &
         'Whole  Domain: < Vv_adj, Vv > = ', adj_sum_rhs
   end if
      
   vv % v1(its:ite,jts:jte,1:be%v1%mz) = vv2_v1(its:ite,jts:jte,1:be%v1%mz)
   vv % v2(its:ite,jts:jte,1:be%v2%mz) = vv2_v2(its:ite,jts:jte,1:be%v2%mz)
   vv % v3(its:ite,jts:jte,1:be%v3%mz) = vv2_v3(its:ite,jts:jte,1:be%v3%mz)
   vv % v4(its:ite,jts:jte,1:be%v4%mz) = vv2_v4(its:ite,jts:jte,1:be%v4%mz)
   vv % v5(its:ite,jts:jte,1:be%v5%mz) = vv2_v5(its:ite,jts:jte,1:be%v5%mz)

   if (be % ne > 0) then
!     vv % alpha(its:ite,jts:jte,kts:kte,1:be%ne) = vv2_alpha(its:ite,jts:jte,kts:kte,1:be%ne)
      vv % alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne) = &
                 vv2_alpha(its_int:ite_int,jts_int:jte_int,kts_int:kte_int,1:be%ne)
   end if

   write(unit=stdout, fmt='(/a/)') 'da_check_vvtovp_adjoint: Test Finished.'

1235 continue

   if (trace_use) call da_trace_exit("da_check_vvtovp_adjoint")

end subroutine da_check_vvtovp_adjoint


