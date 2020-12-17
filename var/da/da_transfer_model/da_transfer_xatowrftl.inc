subroutine da_transfer_xatowrftl(grid, config_flags, filnam, timestr)

   !---------------------------------------------------------------------------
   !  Purpose: Convert analysis increments into WRFTL increments 
   !           (following xatowrf, but only keep the increments)
   !---------------------------------------------------------------------------

   implicit none
   
   type(domain), intent(inout)               :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   character*4, intent(in) :: filnam
   character(len=256), intent(in) :: timestr

#ifdef VAR4D

   integer :: i, j, k
   integer :: is, ie, js, je, ks, ke
   real    :: sdmd, s1md
   real    :: rho_cgrid

#ifdef A2C
   real, allocatable, dimension(:,:,:) :: g_press
#else
   real, allocatable, dimension(:,:,:) :: utmp, vtmp, g_press
#endif

   integer ndynopt

   if (trace_use) call da_trace_entry("da_transfer_xatowrftl")

   is=grid%xp%its
   ie=grid%xp%ite
   js=grid%xp%jts
   je=grid%xp%jte
   ks=grid%xp%kts
   ke=grid%xp%kte

   allocate (g_press(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme,grid%xp%kms:grid%xp%kme))

   !---------------------------------------------------------------------------
   ! [1.0] Get the mixing ratio of moisture first, as it is easy.
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%g_moist(i,j,k,P_G_QV)=grid%xa%q(i,j,k)/(1.0-grid%xb%q(i,j,k))**2
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [2.0] COMPUTE increments of dry-column air mass per unit area
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         sdmd=0.0
         s1md=0.0
         do k=ks,ke
            sdmd=sdmd+grid%g_moist(i,j,k,P_G_QV)*grid%dnw(k)
            s1md=s1md+(1.0+grid%moist(i,j,k,P_QV))*grid%dnw(k)
         end do
         grid%g_mu_2(i,j)=-(grid%xa%psfc(i,j)+grid%xb%psac(i,j)*sdmd)/s1md
      end do
   end do

   !---------------------------------------------------------------------------
   ! [3.0] compute pressure increments (for computing theta increments)
   !---------------------------------------------------------------------------

   do j=js,je
      do i=is,ie
         g_press(i,j,ke+1)=0.0
         do k=ke,ks,-1
            g_press(i,j,k)=g_press(i,j,k+1) &
               -(grid%g_mu_2(i,j)*(1.0+grid%moist(i,j,k,P_QV)) &
               +(grid%mu_2(i,j)+grid%mub(i,j))*grid%g_moist(i,j,k,P_G_QV))* &
               grid%dn(k)
            grid%xa%p(i,j,k)=0.5*(g_press(i,j,k)+g_press(i,j,k+1))
         end do
      end do
   end do

   !---------------------------------------------------------------------------
   ! [4.0] convert temperature increments into theta increments
   !       evaluate also the increments of (1/rho) and geopotential
   !---------------------------------------------------------------------------

   do k=ks,ke
      do j=js,je
         do i=is,ie
            grid%g_t_2(i,j,k)=(t0+grid%t_2(i,j,k))* &
               (grid%xa%t(i,j,k)/grid%xb%t(i,j,k) &
                        -kappa*grid%xa%p(i,j,k)/grid%xb%p(i,j,k))
         end do
      end do
   end do
!  do j=js,je
!     do i=is,ie
!        grid%g_ph_2(i,j,ks)=0.0
!        do k=ks,ke
!           rho_cgrid=grid%xb%rho(i,j,k) &
!                     *(grid%xa%p(i,j,k)/grid%xb%p(i,j,k) &
!                     -grid%xa%t(i,j,k)/grid%xb%t(i,j,k) &
!                     -0.61*grid%xa%q(i,j,k)/(1.0+0.61*grid%xb%q(i,j,k)))
!           grid%g_ph_2(i,j,k+1)=grid%g_ph_2(i,j,k) &
!              -(g_press(i,j,k+1)-g_press(i,j,k) &
!              +(grid%ph_2(i,j,k+1)-grid%ph_2(i,j,k))*rho_cgrid) &
!              /grid%xb%rho(i,j,k)
!        end do
!     end do
!  end do

   grid%g_ph_2 = 0.0

!  grid%xa%p = 0.0
   grid%g_p = grid%xa%p

   deallocate (g_press)

   !---------------------------------------------------------------------------
   ! [5.0] convert from a-grid to c-grid
   !---------------------------------------------------------------------------

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

#ifdef DM_PARALLEL
#include "HALO_XA_A.inc"
#endif

!rizvi's update
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
!rizvi's update

   grid%g_u_2 = grid%xa%u
   grid%g_v_2 = grid%xa%v

#else
#ifdef DM_PARALLEL
#include "HALO_XA_A.inc"

   allocate ( utmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )
   allocate ( vtmp(grid%xp%ims:grid%xp%ime,grid%xp%jms:grid%xp%jme, grid%xp%kms:grid%xp%kme) )

   utmp = grid%xa%u
   vtmp = grid%xa%v

   ! The southern boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (js == grid%xp%jds) then
      vtmp(is:ie,js-1,ks:ke)=2.0*grid%xa%v(is:ie,js  ,ks:ke) &
                            -    grid%xa%v(is:ie,js+1,ks:ke)
   end if

   ! The northern boundary
   if (je == grid%xp%jde) then
      vtmp(is:ie,je+1,ks:ke)=2.0*grid%xa%v(is:ie,je  ,ks:ke) &
                            -    grid%xa%v(is:ie,je-1,ks:ke)
   end if

   ! The western boundary (fill A-GRID boundaries)
   ! To keep the gradient, A(0) = 2A(1)-A(2)
   if (is == grid%xp%ids) then
      utmp(is-1,js:je,ks:ke)=2.0*grid%xa%u(is  ,js:je,ks:ke) &
                            -    grid%xa%u(is+1,js:je,ks:ke)
   end if

   ! The eastern boundary
   if (ie == grid%xp%ide) then
      utmp(ie+1,js:je,ks:ke)=2.0*grid%xa%u(ie  ,js:je,ks:ke) &
                            -    grid%xa%u(ie-1,js:je,ks:ke)
   end if

   do k=ks,ke
      do j=js,je
         do i=is,ie+1
            grid%g_u_2(i,j,k)=0.5*(utmp(i-1,j  ,k)+utmp(i,j,k))
         end do
      end do
      do j=js,je+1
         do i=is,ie
            grid%g_v_2(i,j,k)=0.5*(vtmp(i  ,j-1,k)+vtmp(i,j,k))
         end do
      end do
   end do


   deallocate (utmp)
   deallocate (vtmp)
#else

   do k=ks,ke
      do j=js,je
         do i=is+1,ie
            grid%g_u_2(i,j,k)=0.5*(grid%xa%u(i-1,j,k)+grid%xa%u(i,j,k))
         end do
      end do
      do j=js+1,je
         do i=is,ie
            grid%g_v_2(i,j,k)=0.5*(grid%xa%v(i,j-1,k)+grid%xa%v(i,j,k))
         end do
      end do
   end do

   ! To keep the gradient, A(N+1) = 2A(N)-A(N-1)
   ! and on C-Grid, this will lead to C(N+1)=(A(N)+A(N+1))/2=(3A(N)-A(N-1))/2

   ! The eastern boundary
   grid%g_u_2(ie+1,js:je,ks:ke)=(3.0*grid%xa%u(ie,js:je,ks:ke)-grid%xa%u(ie-1,js:je,ks:ke))/2.0

   ! The northern boundary
   grid%g_v_2(is:ie,je+1,ks:ke)=(3.0*grid%xa%v(is:ie,je,ks:ke)-grid%xa%v(is:ie,je-1,ks:ke))/2.0

   ! To keep the gradient, A(0) = 2A(1)-A(2)
   ! and on C-Grid, this will lead to C(1)=(A(0)+A(1))/2=(3A(1)-A(2))/2

   ! The western boundary
   grid%g_u_2(is,js:je,ks:ke)=(3.0*grid%xa%u(is,js:je,ks:ke)-grid%xa%u(is+1,js:je,ks:ke))/2.0

   ! The southern boundary
   grid%g_v_2(is:ie,js,ks:ke)=(3.0*grid%xa%v(is:ie,js,ks:ke)-grid%xa%v(is:ie,js+1,ks:ke))/2.0

#endif

#endif
   !---------------------------------------------------------------------------
   ! [6.0] save OTHERinCREMENT 
   !---------------------------------------------------------------------------

   do j=js,je
      do k=ks,ke+1
         do i=is,ie
            grid%g_w_2(i,j,k)=grid%xa%w(i,j,k)
         end do
      end do
   end do

   if ( config_flags%mp_physics_ad == warmrain_ad ) then
      if ( f_g_qc .and. cloud_cv_options >= 1 ) then
         grid%g_moist(is:ie,js:je,ks:ke,p_g_qc) = grid%xa%qcw(is:ie,js:je,ks:ke)
      end if
      if ( f_g_qr .and. cloud_cv_options >= 1 ) then
         grid%g_moist(is:ie,js:je,ks:ke,p_g_qr) = grid%xa%qrn(is:ie,js:je,ks:ke)
      end if
      !placeholder
      !if ( config_flags%mp_physics_ad == icecld_ad ) then
      !   if ( f_g_qi .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qi) = grid%xa%qci(is:ie,js:je,ks:ke)
      !   end if
      !   if ( f_g_qs .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qs) = grid%xa%qsn(is:ie,js:je,ks:ke)
      !   end if
      !   if ( f_g_qg .and. cloud_cv_options >= 2 ) then
      !      grid%g_moist(is:ie,js:je,ks:ke,p_g_qg) = grid%xa%qgr(is:ie,js:je,ks:ke)
      !   end if
      !end if
   end if

   call da_transfer_wrftl_lbc_t0 ( grid )

   !---------------------------------------------------------------------------
   ! [7.0] output
   !---------------------------------------------------------------------------

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
   model_grid%g_rainnc  = 0.0
   model_grid%g_rainncv = 0.0
   model_grid%g_rainc  = 0.0
   model_grid%g_raincv = 0.0

   do i = PARAM_FIRST_SCALAR, num_g_moist
      call kj_swap (grid%g_moist(:,:,:,i), model_grid%g_moist(:,:,:,i), &
                    grid%xp%ims, grid%xp%ime, grid%xp%jms, grid%xp%jme, grid%xp%kms, grid%xp%kme)
   enddo

   if ( .not. trajectory_io .or. var4d_detail_out ) &
      call  med_hist_out ( grid , AUXHIST8_ALARM , config_flags )

   if (trace_use) call da_trace_exit("da_transfer_xatowrftl")

#endif
end subroutine da_transfer_xatowrftl


