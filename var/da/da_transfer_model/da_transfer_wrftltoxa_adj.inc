subroutine da_transfer_wrftltoxa_adj(grid, config_flags, filnam, timestr)

   !---------------------------------------------------------------------------
   ! Purpose: Convert analysis increments into WRFDA increments 
   !          (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   character*4, intent(in) :: filnam
   character*256, intent(in) :: timestr

   integer :: i, j, k, ndynopt
   integer :: is, ie, js, je, ks, ke, ierr

   integer :: julyr, julday
   real    :: gmt

   real    :: sdmd, s1md
   real :: g_press(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, &
      grid%xp%kms:grid%xp%kme)
   real :: utmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, &
      grid%xp%kms:grid%xp%kme)
   real :: vtmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, &
      grid%xp%kms:grid%xp%kme)

   if (trace_use) call da_trace_entry("da_transfer_wrftltoxa_adj")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   grid%g_u_2  = 0.0
   grid%g_v_2  = 0.0
   grid%g_w_2  = 0.0
   grid%g_t_2  = 0.0
   grid%g_moist   = 0.0
   grid%g_mu_2 = 0.0
   grid%g_ph_2 = 0.0
   grid%g_p = 0.0
   grid%g_rainncv = 0.0
   grid%g_raincv = 0.0
   !---------------------------------------------------------------------------
   ! [6.0] ALL THE SIMPLE ONES
   !---------------------------------------------------------------------------

#ifdef DM_PARALLEL
#include "HALO_XA.inc"
#endif

   do k=ks,ke+1
      do j=js,je
         do i=is,ie
            grid%g_w_2(i,j,k)=grid%g_w_2(i,j,k)+grid%xa%w(i,j,k)
         end do
      end do
   end do
   grid%xa%w(is:ie,js:je,ks:ke+1) = 0.0

   if ( var4d ) then
      if ( config_flags%mp_physics_ad == warmrain_ad ) then
         if ( f_g_qc .and. cloud_cv_options >= 1 ) then
            grid%g_moist(is:ie,js:je,ks:ke,p_g_qc)=grid%xa%qcw(is:ie,js:je,ks:ke)
         end if
         if ( f_g_qr .and. cloud_cv_options >= 1 ) then
            grid%g_moist(is:ie,js:je,ks:ke,p_g_qr)=grid%xa%qrn(is:ie,js:je,ks:ke)
         end if
      end if
      !placeholder
      !if ( config_flags%mp_physics_ad == icecld_ad ) then
      !   if ( f_g_qi .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qi)=grid%xa%qci(is:ie,js:je,ks:ke)
      !   end if
      !   if ( f_g_qs .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qs)=grid%xa%qsn(is:ie,js:je,ks:ke)
      !   end if
      !   if ( f_g_qg .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qg)=grid%xa%qgr(is:ie,js:je,ks:ke)
      !   end if
      !end if
   end if

   !----------------------------------------------------------------------------
   ! [5.0] convert from c-grid to a-grid
   ! ----------------------------------------------------------------------------

#ifdef DM_PARALLEL
   utmp=grid%xa%u
   vtmp=grid%xa%v

   ! The western boundary
   if (is == grid%xp%ids) utmp(is-1,js:je,ks:ke)=0.0

   ! The southern boundary
   if (js == grid%xp%jds) vtmp(is:ie,js-1,ks:ke)=0.0

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%g_u_2(i,j,k)=grid%g_u_2(i,j,k)+0.5*(utmp(i-1,j  ,k)+utmp(i,j,k))
            grid%g_v_2(i,j,k)=grid%g_v_2(i,j,k)+0.5*(vtmp(i  ,j-1,k)+vtmp(i,j,k))
         end do
      end do
   end do

   ! The eastern boundary
   if (ie == grid%xp%ide)  &
       grid%g_u_2(ie+1,js:je,ks:ke)=grid%g_u_2(ie+1,js:je,ks:ke)+grid%xa%u(ie,js:je,ks:ke)/2.0

   ! The northern boundary
   if (je == grid%xp%jde)  &
       grid%g_v_2(is:ie,je+1,ks:ke)=grid%g_v_2(is:ie,je+1,ks:ke)+grid%xa%v(is:ie,je,ks:ke)/2.0

#else

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            grid%g_u_2(i,j,k)=grid%g_u_2(i,j,k)+0.5*(grid%xa%u(i-1,j,k)+grid%xa%u(i,j,k))
         end do
      end do
      do j=js+1,je
         do i=is,ie
            grid%g_v_2(i,j,k)=grid%g_v_2(i,j,k)+0.5*(grid%xa%v(i,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   ! The western boundary
   grid%g_u_2(is,js:je,ks:ke)=grid%g_u_2(is,js:je,ks:ke)+grid%xa%u(is,js:je,ks:ke)/2.0

   ! The eastern boundary
   grid%g_u_2(ie+1,js:je,ks:ke)=grid%g_u_2(ie+1,js:je,ks:ke)+grid%xa%u(ie,js:je,ks:ke)/2.0

   ! The southern boundary
   grid%g_v_2(is:ie,js,ks:ke)=grid%g_v_2(is:ie,js,ks:ke)+grid%xa%v(is:ie,js,ks:ke)/2.0

   ! The northern boundary
   grid%g_v_2(is:ie,je+1,ks:ke)=grid%g_v_2(is:ie,je+1,ks:ke)+grid%xa%v(is:ie,je,ks:ke)/2.0


#endif
   grid%xa%u(is:ie, js:je, ks:ke) = 0.0
   grid%xa%v(is:ie, js:je, ks:ke) = 0.0

   !---------------------------------------------------------------------------
   ! [4.0] CONVERT THETA INCREMENTS TO T INCREMENTS
   !---------------------------------------------------------------------------

   ! In the inverse, g_ph information is lost. This should be investigated later!!!
   ! However, in this adjoint problem, a_ph should be set to 0.0 Otherwise, a_ph 
   ! will be initialized randomly!

   grid%g_ph_2=0.0

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%xa%p(i,j,k)=grid%xa%p(i,j,k)+grid%xb%t(i,j,k)*kappa*grid%xa%t(i,j,k)/grid%xb%p(i,j,k)
            grid%g_t_2(i,j,k)=grid%g_t_2(i,j,k)+grid%xb%t(i,j,k)*grid%xa%t(i,j,k)/(t0+grid%t_2(i,j,k))
         end do
      end do
   end do

   grid%xa%t(is:ie, js:je, ks:ke) = 0.0
   !---------------------------------------------------------------------------
   ! [3.0] COMPUTE pressure increments 
   !---------------------------------------------------------------------------

!  g_press(is:ie,js:je,ks:ke+1)=0.0
!  do k=ks,ke
!     do j=js,je
!        do i=is,ie
!           g_press(i,j,k+1)=g_press(i,j,k+1)+0.5*grid%xa%p(i,j,k)
!           g_press(i,j,k )=g_press(i,j,k )+0.5*grid%xa%p(i,j,k)
!           grid%g_moist(i,j,k,P_G_QV)=grid%g_moist(i,j,k,P_G_QV)-(grid%mu_2(i,j)+grid%mub(i,j))*g_press(i,j,k)*grid%dn(k)
!           grid%g_mu_2(i,j)=grid%g_mu_2(i,j)-g_press(i,j,k)*(1.0+grid%moist(i,j,k,P_QV))*grid%dn(k)
!           g_press(i,j,k+1)=g_press(i,j,k+1)+g_press(i,j,k)
!        end do
!     end do
!  end do

   grid%g_p(is:ie, js:je, ks:ke) = grid%g_p(is:ie, js:je, ks:ke) + grid%xa%p(is:ie, js:je, ks:ke)

   grid%xa%p(is:ie, js:je, ks:ke) = 0.0

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE psfc increments from mu-increments
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         grid%g_mu_2(i,j)=grid%g_mu_2(i,j)-grid%xa%psfc(i,j)*s1md
         sdmd=sdmd-grid%xb%psac(i,j)*grid%xa%psfc(i,j)
         do k=ks,ke
            grid%g_moist(i,j,k,P_G_QV)=grid%g_moist(i,j,k,P_G_QV)+sdmd*grid%dnw(k)
         end do
      end do
   end do

   grid%xa%psfc(is:ie, js:je) = 0.0

   !---------------------------------------------------------------------------
   ! [1.0] Get the specific humidity increments from mixing ratio increments
   !---------------------------------------------------------------------------
   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%g_moist(i,j,k,P_G_QV)=grid%g_moist(i,j,k,P_G_QV)+grid%xa%q(i,j,k)* &
               (1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   grid%xa%q(is:ie, js:je, ks:ke) = 0.0

   if ( .not. adj_sens ) then
#ifdef VAR4D
      if ( trajectory_io ) then
         call kj_swap (grid%g_u_2, model_grid%g_u_2, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         call kj_swap (grid%g_v_2, model_grid%g_v_2, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         call kj_swap (grid%g_w_2, model_grid%g_w_2, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         call kj_swap (grid%g_t_2, model_grid%g_t_2, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         call kj_swap (grid%g_ph_2, model_grid%g_ph_2, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         call kj_swap (grid%g_p, model_grid%g_p, &
                       grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         model_grid%g_mu_2 = grid%g_mu_2
         model_grid%g_rainnc = grid%g_rainnc
         model_grid%g_rainncv = grid%g_rainncv
         model_grid%g_rainc = grid%g_rainc
         model_grid%g_raincv = grid%g_raincv

         do i = PARAM_FIRST_SCALAR, num_g_moist
            call kj_swap (grid%g_moist(:,:,:,i), model_grid%g_moist(:,:,:,i), &
                          grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
         enddo

         call push_ad_forcing (timestr)

      else
         config_flags%auxhist8_outname = "af_d<domain>_<date>"
         call  med_hist_out ( grid , AUXHIST8_ALARM , config_flags )
         WRITE(message(1), FMT='(A,A)') 'Save  ad. forcing time_stamp:', TRIM(timestr)
         CALL da_message ( message(1:1) )
         config_flags%auxhist8_outname = "auxhist8_d<domain>_<date>"
      endif

      if ( var4d_detail_out ) then
         config_flags%auxhist8_outname = "af_d<domain>_<date>"
         call  med_hist_out ( grid , AUXHIST8_ALARM , config_flags )
         WRITE(message(1), FMT='(A,A)') 'Save  ad. forcing time_stamp:', TRIM(timestr)
         CALL da_message ( message(1:1) )
         config_flags%auxhist8_outname = "auxhist8_d<domain>_<date>"
      endif
#endif
   else
      if ( grid%auxinput7_oid .NE. 0 ) then
         call close_dataset ( grid%auxinput7_oid , config_flags , "DATASET=AUXINPUT7" )
      endif

      call open_w_dataset (grid%auxinput7_oid, trim(filnam), grid, config_flags, &
                            output_auxinput7, "DATASET=AUXINPUT7", ierr)
      if ( ierr .NE. 0 ) CALL wrf_error_fatal('Error opening '//trim(filnam))

      start_date=current_date

      call geth_julgmt(julyr, julday, gmt)
      config_flags%gmt = gmt
      config_flags%julyr = julyr
      config_flags%julday = julday

      call output_auxinput7 (grid%auxinput7_oid, grid , config_flags , ierr)
      if ( ierr .NE. 0 ) CALL wrf_error_fatal('Error writing Gradient in auxinput7')
      call close_dataset (grid%auxinput7_oid, config_flags, "DATASET=AUXINPUT7")
   endif

   if (trace_use) call da_trace_exit("da_transfer_wrftltoxa_adj")

end subroutine da_transfer_wrftltoxa_adj


