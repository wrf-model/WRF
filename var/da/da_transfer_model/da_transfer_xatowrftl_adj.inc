subroutine da_transfer_xatowrftl_adj(grid, config_flags, filnam)

   !---------------------------------------------------------------------------
   ! Purpose: Convert WRFTL variables to analysis increments
   !           (inverse of the incremental part of xatowrf)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam

   ! Local variables

   integer :: i, j, k, bid, ierr, ii, jj, spec_bdy_width
   real    :: sdmd, s1md
   real    :: rho_cgrid

   real, dimension(ims:ime,jms:jme,kms:kme) :: a_press
#ifndef A2C
   real, dimension(ims:ime,jms:jme,kms:kme) :: utmp
   real, dimension(ims:ime,jms:jme,kms:kme) :: vtmp
   real, dimension(ims:ime,jms:jme) :: mut
   real, dimension(ims:ime,jms:jme) :: muu
   real, dimension(ims:ime,jms:jme) :: muv
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl_adj")

   !---------------------------------------------------------------------------
   ! [7.0] Adjoint of outPUT (inPUT)
   !---------------------------------------------------------------------------

   if ( .not. adj_sens ) then

#ifdef VAR4D

   call kj_swap_reverse (model_grid%a_u_2, grid%a_u_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%a_v_2, grid%a_v_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%a_w_2, grid%a_w_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%a_t_2, grid%a_t_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%a_ph_2, grid%a_ph_2, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   call kj_swap_reverse (model_grid%a_p, grid%a_p, &
                 grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)

   grid%a_mu_2 = model_grid%a_mu_2
   grid%a_rainnc  = model_grid%a_rainnc
   grid%a_rainncv = model_grid%a_rainncv
   grid%a_rainc  = model_grid%a_rainc
   grid%a_raincv = model_grid%a_raincv

   do i = PARAM_FIRST_SCALAR, num_a_moist
      call kj_swap_reverse (model_grid%a_moist(:,:,:,i), grid%a_moist(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   enddo

   call da_transfer_wrftl_lbc_t0_adj ( grid )

   ! output gradient here, without LBC control 
   if ( .not. trajectory_io .or. var4d_detail_out ) then
      config_flags%auxhist7_outname = "gradient_d<domain>_<date>"
      call  med_hist_out ( grid , AUXHIST7_ALARM , config_flags )
      WRITE(message(1), FMT='(A)') 'Output gradient at 0h (including LBC gradient)'
      CALL da_message ( message(1:1) )
      config_flags%auxhist7_outname = "auxhist7_d<domain>_<date>"
   endif

#endif

   else

   if ( grid%auxinput17_oid .NE. 0 ) then
      call close_dataset ( grid%auxinput17_oid , config_flags , "DATASET=AUXINPUT17" )
   endif

   call open_r_dataset ( grid%auxinput17_oid, TRIM(filnam) , grid , config_flags, &
                              "DATASET=AUXINPUT17", ierr )
   if ( ierr .NE. 0 ) then
      WRITE( message(1) , * ) 'Error opening ', TRIM( filnam )
      CALL wrf_error_fatal( TRIM( message(1) ) )
   endif

   call wrf_debug (00 , 'Calling input_auxinput17' )
   call input_auxinput17 ( grid%auxinput17_oid, grid , config_flags , ierr )

   call close_dataset ( grid%auxinput17_oid , config_flags , "DATASET=AUXINPUT17" )

   endif

   !---------------------------------------------------------------------------
   ! [6.0] Adjoint of save OTHERinCREMENT
   !---------------------------------------------------------------------------

   do k=kts,kte+1
      do j=jts,jte
         do i=its,ite
            grid%xa%w(i,j,k)=grid%xa%w(i,j,k)+grid%a_w_2(i,j,k)
         end do
      end do
   end do

   grid%a_w_2 = 0.0

#ifndef A2C
   !---------------------------------------------------------------------------
   ! [5.0] Adjoint of CONVERT FROM A-GRID TO C-GRID
   !---------------------------------------------------------------------------

   ! Fill the halo region for a_u and a_v.
   utmp=grid%xa%u
   vtmp=grid%xa%v
#endif
   grid%xa%u=grid%a_u_2
   grid%xa%v=grid%a_v_2

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
#include "HALO_XA_A.inc"
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
#else
   grid%a_u_2=grid%xa%u
   grid%a_v_2=grid%xa%v
   grid%xa%u=utmp
   grid%xa%v=vtmp
   utmp=0.0
   vtmp=0.0

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            utmp(i,j,k)=utmp(i,j,k)+0.5*(grid%a_u_2(i+1,j  ,k)+grid%a_u_2(i,j,k))
            vtmp(i,j,k)=vtmp(i,j,k)+0.5*(grid%a_v_2(i  ,j+1,k)+grid%a_v_2(i,j,k))
         end do
      end do
   end do

   utmp(its-1,jts:jte,kts:kte)=utmp(its-1,jts:jte,kts:kte)+0.5*grid%a_u_2(its,jts:jte,kts:kte)
   utmp(ite+1,jts:jte,kts:kte)=utmp(ite+1,jts:jte,kts:kte)+0.5*grid%a_u_2(ite+1,jts:jte,kts:kte)
   vtmp(its:ite,jts-1,kts:kte)=vtmp(its:ite,jts-1,kts:kte)+0.5*grid%a_v_2(its:ite,jts,kts:kte)
   vtmp(its:ite,jte+1,kts:kte)=vtmp(its:ite,jte+1,kts:kte)+0.5*grid%a_v_2(its:ite,jte+1,kts:kte)

   ! The western boundary
   if (its == grid%xp%ids) then
      grid%xa%u(its  ,jts:jte,kts:kte)=grid%xa%u(its  ,jts:jte,kts:kte)+2.0*utmp(its-1,jts:jte,kts:kte)
      grid%xa%u(its+1,jts:jte,kts:kte)=grid%xa%u(its+1,jts:jte,kts:kte)-utmp(its-1,jts:jte,kts:kte)
   end if

   ! The eastern boundary
   if (ite == grid%xp%ide) then
      grid%xa%u(ite  ,jts:jte,kts:kte)=grid%xa%u(ite  ,jts:jte,kts:kte)+2.0*utmp(ite+1,jts:jte,kts:kte)
      grid%xa%u(ite-1,jts:jte,kts:kte)=grid%xa%u(ite-1,jts:jte,kts:kte)-utmp(ite+1,jts:jte,kts:kte)
   end if

   grid%xa%u=grid%xa%u+utmp

   ! The southern boundary
   if (jts == grid%xp%jds) then
      grid%xa%v(its:ite,jts  ,kts:kte)=grid%xa%v(its:ite,jts  ,kts:kte)+2.0*vtmp(its:ite,jts-1,kts:kte)
      grid%xa%v(its:ite,jts+1,kts:kte)=grid%xa%v(its:ite,jts+1,kts:kte)-vtmp(its:ite,jts-1,kts:kte)
   end if

   ! The northern boundary
   if (jte == grid%xp%jde) then
      grid%xa%v(its:ite,jte  ,kts:kte)=grid%xa%v(its:ite,jte  ,kts:kte)+2.0*vtmp(its:ite,jte+1,kts:kte)
      grid%xa%v(its:ite,jte-1,kts:kte)=grid%xa%v(its:ite,jte-1,kts:kte)-vtmp(its:ite,jte+1,kts:kte)
   end if

   grid%xa%v=grid%xa%v+vtmp
#endif

   grid%a_u_2 = 0.0
   grid%a_v_2 = 0.0

   !---------------------------------------------------------------------------
   ! [4.0] Adjoint of CONVERT TEMPERATURE inCREMENTS inTO THETA inCREMENTS
   !       EVALUATE ALSO THE inCREMENTS OF (1/rho) AND GEOPOTENTIAL
   !---------------------------------------------------------------------------

   a_press(its:ite,jts:jte,kts:kte+1)=0.0

   do k=kte,kts,-1
      do j=jts,jte
         do i=its,ite
            grid%xa%p(i,j,k)= grid%xa%p(i,j,k)+grid%a_p(i,j,k)
         end do
      end do
   end do

   grid%a_p = 0.0

!  do k=kte,kts,-1
!     do j=jts,jte
!        do i=its,ite
!           rho_cgrid=-(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           a_press(i,j,k )=a_press(i,j,k )+grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           a_press(i,j,k+1)=a_press(i,j,k+1)-grid%a_ph_2(i,j,k+1)/grid%xb%rho(i,j,k)
!           grid%a_ph_2(i,j,k ) =grid%a_ph_2(i,j,k)   +grid%a_ph_2(i,j,k+1)
!           grid%xa%q(i,j,k)=grid%xa%q(i,j,k)-grid%xb%rho(i,j,k)*0.61*rho_cgrid/(1.0+0.61*grid%xb%q(i,j,k))
!           grid%xa%t(i,j,k)=grid%xa%t(i,j,k)-grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%t(i,j,k)
!           grid%xa%p(i,j,k)= grid%xa%p(i,j,k)+grid%xb%rho(i,j,k)*rho_cgrid/grid%xb%p(i,j,k)
!        end do
!     end do
!  end do

   do k=kts,kte
      do j=jts,jte
         do i=its,ite 
            grid%xa%p(i,j,k)=grid%xa%p(i,j,k)-(t0+grid%t_2(i,j,k))*kappa*grid%a_t_2(i,j,k)/grid%xb%p(i,j,k)
            grid%xa%t(i,j,k)=grid%xa%t(i,j,k)+(t0+grid%t_2(i,j,k))*grid%a_t_2(i,j,k)/grid%xb%t(i,j,k)
         end do
      end do
   end do

   grid%a_t_2 = 0.0
   grid%a_ph_2 = 0.0
 
   !---------------------------------------------------------------------------
   ! [3.0] Adjoint of COMPUTE pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            a_press(i,j,k+1)=a_press(i,j,k+1)+0.5*grid%xa%p(i,j,k)
            a_press(i,j,k )=a_press(i,j,k )+0.5*grid%xa%p(i,j,k)
            grid%xa%p(i,j,k)=0.0
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)-(grid%mu_2(i,j)+grid%mub(i,j))*a_press(i,j,k)*grid%dn(k)
            grid%a_mu_2(i,j)=grid%a_mu_2(i,j)-a_press(i,j,k)*(1.0+grid%moist(i,j,k,P_QV))*grid%dn(k)
            a_press(i,j,k+1)=a_press(i,j,k+1)+a_press(i,j,k)
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] Adjoint of COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=jts,jte
      do i=its,ite
         sdmd=0.0
         s1md=0.0
         do k=kts,kte
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         sdmd=sdmd-grid%xb%psac(i,j)*grid%a_mu_2(i,j)/s1md
         grid%xa%psfc(i,j)=grid%xa%psfc(i,j)-grid%a_mu_2(i,j)/s1md
         do k=kts,kte
            grid%a_moist(i,j,k,P_A_QV)=grid%a_moist(i,j,k,P_A_QV)+sdmd*grid%dnw(k)
         end do
      end do
   end do

   grid%a_mu_2 = 0.0
   !---------------------------------------------------------------------------
   ! [1.0] Adjoint of Get the mixing ratio of moisture 
   !---------------------------------------------------------------------------
   do k=kts,kte
      do j=jts,jte
         do i=its,ite
            grid%xa%q(i,j,k)=grid%xa%q(i,j,k)+grid%a_moist(i,j,k,P_A_QV)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   if ( config_flags%mp_physics_ad == warmrain_ad ) then
      if ( f_a_qc .and. cloud_cv_options >= 1 ) then
         grid%xa%qcw(its:ite,jts:jte,kts:kte)=grid%a_moist(its:ite,jts:jte,kts:kte,p_a_qc)
      end if
      if ( f_a_qr .and. cloud_cv_options >= 1 ) then
         grid%xa%qrn(its:ite,jts:jte,kts:kte)=grid%a_moist(its:ite,jts:jte,kts:kte,p_a_qr)
      end if
      !placeholder
      !if ( config_flags%mp_physics_ad == icecld_ad ) then
      !   if ( f_a_qi .and. cloud_cv_options >= 2 ) then
      !      grid%xa%qci(its:ite,jts:jte,kts:kte)=grid%a_moist(its:ite,jts:jte,kts:kte,p_a_qi)
      !   end if
      !   if ( f_a_qs .and. cloud_cv_options >= 2 ) then
      !      grid%xa%qsn(its:ite,jts:jte,kts:kte)=grid%a_moist(its:ite,jts:jte,kts:kte,p_a_qs)
      !   end if
      !   if ( f_a_qg .and. cloud_cv_options >= 2 ) then
      !      grid%xa%qgr(its:ite,jts:jte,kts:kte)=grid%a_moist(its:ite,jts:jte,kts:kte,p_a_qg)
      !   end if
      !end if
   end if

   grid%a_moist = 0.0

   grid%a_rainnc = 0.0
   grid%a_rainncv = 0.0
   grid%a_rainc = 0.0
   grid%a_raincv = 0.0

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl_adj")

end subroutine da_transfer_xatowrftl_adj


