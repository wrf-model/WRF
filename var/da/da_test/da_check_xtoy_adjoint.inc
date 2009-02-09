subroutine da_check_xtoy_adjoint(cv_size, cv, grid, config_flags, iv, y)
   
   !--------------------------------------------------------------------------
   ! Purpose: Test observation operator transform and adjoint for compatibility.
   !
   ! Method:  Standard adjoint test: < y, y > = < x, x_adj >.
   !    Updated for Analysis on Arakawa-C grid
   !    Author: Syed RH Rizvi,  MMM/ESSL/NCAR,  Date: 10/22/2008
   !---------------------------------------------------------------------------
   
   implicit none
   
   integer, intent(in)                       :: cv_size ! Size of cv array.
   real, intent(inout)                       :: cv(1:cv_size)   ! control variables.
   type (domain),              intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   type (iv_type),             intent(inout) :: iv    ! ob. increment vector.
   type (y_type),              intent(inout) :: y     ! y = h (grid%xa)

   real                           :: adj_ttl_lhs   ! < y, y >
   real                           :: adj_ttl_rhs   ! < x, x_adj >

   real                           :: partial_lhs   ! < y, y >
   real                           :: partial_rhs   ! < x, x_adj >

   real                           :: pertile_lhs   ! < y, y >
   real                           :: pertile_rhs   ! < x, x_adj >
 
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_u, xa2_v, xa2_t, &
                                                 xa2_p, xa2_q, xa2_rh
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_w
   real, dimension(ims:ime, jms:jme)          :: xa2_psfc
   real, dimension(ims:ime, jms:jme, kms:kme) :: xa2_qcw, xa2_qrn
   integer :: nobwin, ndynopt
   character(len=4) :: filnam
   integer :: wrf_done_unit

   if (trace_use) call da_trace_entry("da_check_xtoy_adjoint")

   write (unit=stdout, fmt='(/a/)') 'da_check_xtoy_adjoint: Test Results:'

   !----------------------------------------------------------------------
   ! [1.0] Initialise:
   !----------------------------------------------------------------------

   partial_lhs = 0.0
   pertile_lhs = 0.0

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe + 1
     ide = ide + 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe + 1
     jde = jde + 1
  end if
#endif

#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

#ifdef A2C
  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. ide == ipe ) then
     ipe = ipe - 1
     ide = ide - 1
  end if

  if ((fg_format==fg_format_wrf_arw_regional  .or. &
       fg_format==fg_format_wrf_arw_global  ) .and. jde == jpe ) then
     jpe = jpe - 1
     jde = jde - 1
  end if
#endif

   xa2_u(ims:ime, jms:jme, kms:kme) = grid%xa%u(ims:ime, jms:jme, kms:kme)
   xa2_v(ims:ime, jms:jme, kms:kme) = grid%xa%v(ims:ime, jms:jme, kms:kme)
   xa2_t(ims:ime, jms:jme, kms:kme) = grid%xa%t(ims:ime, jms:jme, kms:kme)
   xa2_p(ims:ime, jms:jme, kms:kme) = grid%xa%p(ims:ime, jms:jme, kms:kme)
   xa2_q(ims:ime, jms:jme, kms:kme) = grid%xa%q(ims:ime, jms:jme, kms:kme)
   xa2_w(ims:ime, jms:jme, kms:kme) = grid%xa%w(ims:ime, jms:jme, kms:kme)
   xa2_rh(ims:ime, jms:jme, kms:kme)= grid%xa%rh(ims:ime, jms:jme, kms:kme)
   xa2_psfc(ims:ime, jms:jme)       = grid%xa%psfc(ims:ime, jms:jme)

   xa2_qcw(ims:ime, jms:jme, kms:kme) = grid%xa%qcw(ims:ime, jms:jme, kms:kme)
   xa2_qrn(ims:ime, jms:jme, kms:kme) = grid%xa%qrn(ims:ime, jms:jme, kms:kme)

#ifdef A2C
    if( ite == ide ) &    
print*,__FILE__,jte,' xa2_u.xa2_u for col= ',ite+1,sum(xa2_u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
    if( jte == jde ) &    
print*,__FILE__,jte,' xa2_v.xa2_v for row= ',jte+1,sum(xa2_v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
#endif
   if (var4d) then
      call da_transfer_xatowrftl(grid, config_flags, 'tl01')

#ifdef DM_PARALLEL
      call da_system("da_run_wrfplus_tl.ksh pre")
      if (rootproc) then
         call da_system("rm -rf wrf_done")
         call da_system("touch wrf_go_ahead")
         call da_get_unit(wrf_done_unit)
         do while (.true.)
            open(wrf_done_unit,file="wrf_done",status="old",err=303)
            close(wrf_done_unit)
            exit
303         continue
            call da_system("sleep 1")
         end do
         call da_free_unit(wrf_done_unit)
      end if
      call wrf_get_dm_communicator ( comm )
      call mpi_barrier(comm, ierr)
      call da_system("da_run_wrfplus_tl.ksh post")
#else
      call da_system("da_run_wrfplus_tl.ksh")
#endif
   end if

   do nobwin=1, num_fgat_time

      if (var4d) then
         write(filnam,'(a2,i2.2)') 'tl',nobwin
         call da_transfer_wrftltoxa(grid, config_flags, filnam)
       end if

      call da_pt_to_rho_lin(grid)

      if (sfc_assi_options == 2) then
         call da_transform_xtowtq (grid)
#ifdef DM_PARALLEL
#include "HALO_SFC_XA.inc"
#endif
      end if

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_gpsztdobs .or. use_gpsrefobs .or.                 &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         ! Now do something for PW
         call da_transform_xtotpw(grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs) then
            call da_transform_xtogpsref_lin(grid)
            if (use_gpsztdobs) call da_transform_xtoztd_lin(grid)
         end if

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
             use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
              call da_error(__FILE__,__LINE__, &
                (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_lin(grid)
         end if

         if (use_ssmitbobs) call da_transform_xtotb_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_SSMI_XA.inc"
#endif
      end if

      !----------------------------------------------------------------------
      ! [2.0] Perform y = Hx transform:
      !----------------------------------------------------------------------
   
      call da_transform_xtoy (cv_size, cv, grid, iv, y)

      !----------------------------------------------------------------------
      ! [3.0] Calculate LHS of adjoint test equation and
      !       Rescale input to adjoint routine :
      !----------------------------------------------------------------------

      if (iv%info(sound)%nlocal > 0) then
         call da_check_xtoy_adjoint_sound     (iv, y, partial_lhs, pertile_lhs)
         call da_check_xtoy_adjoint_sonde_sfc (iv, y, partial_lhs, pertile_lhs)
      end if
      if (iv%info(mtgirs)%nlocal   > 0) call da_check_xtoy_adjoint_mtgirs   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(tamdar)%nlocal   > 0) call da_check_xtoy_adjoint_tamdar_sfc(iv, y, partial_lhs, pertile_lhs)
      if (iv%info(synop)%nlocal    > 0) call da_check_xtoy_adjoint_synop    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(geoamv)%nlocal   > 0) call da_check_xtoy_adjoint_geoamv   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(polaramv)%nlocal > 0) call da_check_xtoy_adjoint_polaramv (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(airep)%nlocal    > 0) call da_check_xtoy_adjoint_airep    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(pilot)%nlocal    > 0) call da_check_xtoy_adjoint_pilot    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(radar)%nlocal    > 0) call da_check_xtoy_adjoint_radar    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(satem)%nlocal    > 0) call da_check_xtoy_adjoint_satem    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(metar)%nlocal    > 0) call da_check_xtoy_adjoint_metar    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ships)%nlocal    > 0) call da_check_xtoy_adjoint_ships    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(gpspw)%nlocal    > 0) call da_check_xtoy_adjoint_gpspw    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(gpsref)%nlocal   > 0) call da_check_xtoy_adjoint_gpsref   (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmi_tb)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_tb  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmi_rv)%nlocal  > 0) call da_check_xtoy_adjoint_ssmi_rv  (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt1    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(ssmt2)%nlocal    > 0) call da_check_xtoy_adjoint_ssmt2    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(qscat)%nlocal    > 0) call da_check_xtoy_adjoint_qscat    (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(profiler)%nlocal > 0) call da_check_xtoy_adjoint_profiler (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(buoy)%nlocal     > 0) call da_check_xtoy_adjoint_buoy     (iv, y, partial_lhs, pertile_lhs)
      if (iv%info(bogus)%nlocal    > 0) call da_check_xtoy_adjoint_bogus    (iv, y, partial_lhs, pertile_lhs)
      if (iv%num_inst     > 0) call da_check_xtoy_adjoint_rad      (iv, y, partial_lhs, pertile_lhs)

      !----------------------------------------------------------------------
      ! [5.0] Perform adjoint operation:
      !----------------------------------------------------------------------

      call da_zero_x (grid%xa)

      call da_transform_xtoy_adj (cv_size, cv, grid, iv, y, grid%xa)

#ifdef A2C
    if( ite == ide ) &    
print*,__FILE__,jte,' grid%xa%u.grid%xa%u for col= ',ite+1,sum(grid%xa%u(ite+1, jts:jte, kts:kte) * grid%xa%u(ite+1, jts:jte, kts:kte))
    if( jte == jde ) &    
print*,__FILE__,jte,' grid%xa%v.grid%x%%v for row= ',jte+1,sum(grid%xa%v(its:ite, jte+1, kts:kte) * grid%xa%v(its:ite, jte+1, kts:kte))
#endif

      if (use_ssmt1obs .or. use_ssmt2obs .or. use_gpspwobs .or. &
          use_gpsztdobs .or. use_gpsrefobs .or.                 &
          use_ssmitbobs .or. use_ssmiretrievalobs) then

         if (use_ssmitbobs) call da_transform_xtotb_adj (grid)

         ! for PW
         call da_transform_xtotpw_adj (grid)

         ! GPS Refractivity:
         if (use_gpsrefobs .or. use_gpsztdobs) then
            if (use_gpsztdobs) call da_transform_xtoztd_adj(grid)
            call da_transform_xtogpsref_adj (grid)
         end if

         if (use_ssmt1obs .or. use_ssmt2obs .or. &
             use_ssmitbobs .or. use_ssmiretrievalobs) then
            if (global) then
               call da_error(__FILE__,__LINE__, &
                  (/"grid%xb%speed is not available, see da_transfer_kmatoxb.inc"/))
            end if
            call da_transform_xtoseasfcwind_adj (grid)
         end if
      end if

      ! Now do something for surface variables
      if (sfc_assi_options == 2) then
         call da_transform_xtowtq_adj (grid)
      end if

      call da_pt_to_rho_adj (grid)

      if (var4d) then

         grid%g_u_2 = 0.0
         grid%g_v_2 = 0.0
         grid%g_w_2 = 0.0
         grid%g_t_2 = 0.0
         grid%g_ph_2 = 0.0
         grid%g_mu_2 = 0.0
         grid%g_moist = 0.0

         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam)

!!!!!! exclude TL&AD integration!!!!!!!
#ifdef MYOK
         ndynopt      = grid%dyn_opt
         grid%dyn_opt = DYN_EM_TL
         call nl_set_dyn_opt (1 , DYN_EM_TL)
         call da_med_initialdata_input(grid , config_flags, filnam)
         grid%a_u_2 =   grid%g_u_2
         grid%a_v_2 =   grid%g_v_2
         grid%a_w_2 =   grid%g_w_2
         grid%a_t_2 =   grid%g_t_2
         grid%a_ph_2 =   grid%g_ph_2
         grid%a_mu_2 =   grid%g_mu_2
         grid%a_moist =   grid%g_moist

         grid%dyn_opt = DYN_EM_AD
         call nl_set_dyn_opt (1 , DYN_EM_AD)
         call da_med_initialdata_output(grid , config_flags, 'gr01')               

         grid%dyn_opt = ndynopt
         call nl_set_dyn_opt (1 , DYN_EM)
         grid%g_u_2 = 0.0
         grid%g_v_2 = 0.0
         grid%g_w_2 = 0.0
         grid%g_t_2 = 0.0
         grid%g_ph_2 = 0.0
         grid%g_mu_2 = 0.0
         grid%g_moist = 0.0
         grid%a_u_2 = 0.0
         grid%a_v_2 = 0.0
         grid%a_w_2 = 0.0
         grid%a_t_2 = 0.0
         grid%a_ph_2 = 0.0
         grid%a_mu_2 = 0.0
         grid%a_moist = 0.0
#endif
         !!!!!!
      end if
   end do

   if (var4d) then

#ifndef TLDF
      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt (1 , DYN_EM_TL)

      call da_med_initialdata_input(grid , config_flags, 'tldf')
      grid%g_u_2 = 0.0
      grid%g_v_2 = 0.0
      grid%g_w_2 = 0.0
      grid%g_t_2 = 0.0
      grid%g_ph_2 = 0.0
      grid%g_mu_2 = 0.0
      grid%g_moist = 0.0

      call med_hist_out(grid , 3 , config_flags)

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt (1 , DYN_EM)
#endif

#ifdef DM_PARALLEL
      call da_system("da_run_wrfplus_ad.ksh pre")
      if (rootproc) then
         call da_system("rm -rf wrf_done")
         call da_system("touch wrf_go_ahead")
         call da_get_unit(wrf_done_unit)
         do while (.true.)
            open(wrf_done_unit,file="wrf_done",status="old",err=304)
            close(wrf_done_unit)
            exit
304         continue
            call da_system("sleep 1")
         end do
         call da_free_unit(wrf_done_unit)
      end if
      call wrf_get_dm_communicator ( comm )
      call mpi_barrier(comm, ierr)
      call da_system("da_run_wrfplus_ad.ksh post")
#else 
      call da_system("da_run_wrfplus_ad.ksh")
#endif

      grid%xa%u(its:ite,    jts:jte, kts:kte) = 0.0
      grid%xa%v(its:ite,    jts:jte, kts:kte) = 0.0
      grid%xa%w(its:ite,    jts:jte, kts:kte+1) = 0.0
      grid%xa%q(its:ite,    jts:jte, kts:kte) = 0.0
      grid%xa%t(its:ite,    jts:jte, kts:kte) = 0.0
      grid%xa%psfc(its:ite, jts:jte) = 0.0

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
      grid%a_u_2 =   grid%xa%u
      grid%a_v_2 =   grid%xa%v
      grid%a_w_2 =   grid%xa%w
      grid%a_t_2 =   grid%xa%t
      grid%a_ph_2 =   grid%xa%p
      grid%a_mu_2 =   grid%xa%psfc
      grid%a_moist(:,:,:,p_a_qv) =   grid%xa%q(:,:,:)

      grid%dyn_opt = DYN_EM_AD
      call nl_set_dyn_opt (1 , DYN_EM_AD)
      call da_med_initialdata_output(grid , config_flags, 'gr01')

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt (1 , DYN_EM)

   end if

   pertile_rhs = sum (grid%xa%u(ims:ime, jms:jme, kms:kme) * xa2_u(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%v(ims:ime, jms:jme, kms:kme) * xa2_v(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%w(ims:ime, jms:jme, kms:kme) * xa2_w(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%t(ims:ime, jms:jme, kms:kme) * xa2_t(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%p(ims:ime, jms:jme, kms:kme) * xa2_p(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%q(ims:ime, jms:jme, kms:kme) * xa2_q(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%rh(ims:ime, jms:jme, kms:kme)* xa2_rh(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%psfc(ims:ime, jms:jme) * xa2_psfc(ims:ime, jms:jme))
   pertile_rhs = pertile_rhs &
      + sum (grid%xa%qcw(ims:ime, jms:jme, kms:kme) * xa2_qcw(ims:ime, jms:jme, kms:kme)) &
      + sum (grid%xa%qrn(ims:ime, jms:jme, kms:kme) * xa2_qrn(ims:ime, jms:jme, kms:kme))

   !----------------------------------------------------------------------
   ! [6.0] Calculate RHS of adjoint test equation:
   !----------------------------------------------------------------------
   
   partial_rhs = sum (grid%xa%u(its:ite, jts:jte, kts:kte) * xa2_u(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%v(its:ite, jts:jte, kts:kte) * xa2_v(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%w(its:ite, jts:jte, kts:kte+1) * xa2_w(its:ite, jts:jte, kts:kte+1)) &
      + sum (grid%xa%t(its:ite, jts:jte, kts:kte) * xa2_t(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%p(its:ite, jts:jte, kts:kte) * xa2_p(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%q(its:ite, jts:jte, kts:kte) * xa2_q(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%rh(its:ite, jts:jte, kts:kte)* xa2_rh(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%psfc(its:ite, jts:jte) * xa2_psfc(its:ite, jts:jte)) 

   partial_rhs = partial_rhs &
      + sum (grid%xa%qcw(its:ite, jts:jte, kts:kte) * xa2_qcw(its:ite, jts:jte, kts:kte)) &
      + sum (grid%xa%qrn(its:ite, jts:jte, kts:kte) * xa2_qrn(its:ite, jts:jte, kts:kte))

#ifdef A2C
    if( ite == ide ) then
print*,__FILE__,' contribution from ',ite+1,' col for U : ',sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%u(ite+1, jts:jte, kts:kte) * xa2_u(ite+1, jts:jte, kts:kte))
   end if
    if( jte == jde ) then
print*,__FILE__,' contribution from ',jte+1,' row for V : ',sum(grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   partial_rhs = partial_rhs &
      + sum (grid%xa%v(its:ite, jte+1, kts:kte) * xa2_v(its:ite, jte+1, kts:kte))
   end if
#endif

   !----------------------------------------------------------------------
   !  [7.0] Print output:
   !----------------------------------------------------------------------
   
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < y, y     > = ', pertile_lhs
   write (unit=stdout, fmt='(A,1pe22.14)') ' Single Domain < x, x_adj > = ', pertile_rhs

   adj_ttl_lhs = wrf_dm_sum_real (partial_lhs)
   adj_ttl_rhs = wrf_dm_sum_real (partial_rhs)
   
   if (rootproc) then
      write(unit=stdout, fmt='(/)')
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < y, y     > = ', adj_ttl_lhs
      write (unit=stdout, fmt='(A,1pe22.14)') ' Whole Domain < x, x_adj > = ', adj_ttl_rhs
   end if

   write (unit=stdout, fmt='(/a/)') 'da_check_xtoy_adjoint: Test Finished:'
   if (trace_use) call da_trace_exit("da_check_xtoy_adjoint")
   
end subroutine da_check_xtoy_adjoint


