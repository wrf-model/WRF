Module module_plumerise1
! use module_zero_plumegen_coms
  integer, parameter :: nveg_agreg      = 4
! integer, parameter :: tropical_forest = 1
! integer, parameter :: boreal_forest   = 2
! integer, parameter :: savannah        = 3

! integer, parameter :: grassland       = 4
  real, dimension(nveg_agreg) :: firesize,mean_fct
! character(len=20), parameter :: veg_name(nveg_agreg) = (/ &
!                              'Tropical-Forest', &
!                              'Boreal-Forest  ', &
!                              'Savanna        ', &
!                              'Grassland      ' /)
! character(len=20), parameter :: spc_suf(nveg_agreg) = (/ &
!                              'agtf' , &  ! trop forest
!                              'agef' , &  ! extratrop forest
!                              'agsv' , &  ! savanna
!                              'aggr'   /) ! grassland


CONTAINS
subroutine plumerise_driver (id,ktau,dtstep,                           &
           ebu,ebu_in,mean_fct_agtf,mean_fct_agef,mean_fct_agsv,mean_fct_aggr,              &
           firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr,              &
           config_flags, t_phy,moist,                                     &
           rho_phy,vvel,u_phy,v_phy,p_phy,                              &
           emis_ant,z_at_w,z,                                                       &
         ids,ide, jds,jde, kds,kde,                                        &
         ims,ime, jms,jme, kms,kme,                                        &
         its,ite, jts,jte, kts,kte                                         )

  USE module_configure
  USE module_model_constants
  USE module_state_description
  USE module_zero_plumegen_coms
  USE module_chem_plumerise_scalar
  IMPLICIT NONE
! integer, parameter :: nveg_agreg      = 4
! integer, parameter :: nveg_agreg      = 4
! integer, parameter :: tropical_forest = 1
! integer, parameter :: boreal_forest   = 2
! integer, parameter :: savannah        = 3

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   INTEGER,      INTENT(IN   ) :: id,ktau,                      &
                                  ids,ide, jds,jde, kds,kde,               &
                                  ims,ime, jms,jme, kms,kme,               &
                                  its,ite, jts,jte, kts,kte
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),                &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_ebu ),                 &
         INTENT(INOUT ) ::                                   ebu
   REAL, DIMENSION( ims:ime, 1, jms:jme, num_ebu_in ),                 &
         INTENT(INOUT ) ::                                   ebu_in
   REAL, DIMENSION( ims:ime, jms:jme ),                 &
         INTENT(IN ) ::                                                &
           mean_fct_agtf,mean_fct_agef,&
           mean_fct_agsv,mean_fct_aggr,firesize_agtf,firesize_agef,       &
           firesize_agsv,firesize_aggr

   REAL, DIMENSION( ims:ime, kms:config_flags%kemit, jms:jme,num_emis_ant ),            &
         INTENT(IN ) ::                                                    &
                     emis_ant
!
!
!
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,               &
          INTENT(IN   ) ::                                                 &
                                                      t_phy,               &
                 z,z_at_w,vvel,u_phy,v_phy,rho_phy,p_phy
      REAL,      INTENT(IN   ) ::                                          &
                             dtstep
!
! Local variables...
!
      INTEGER :: nv, i, j, k, ksub, nspecies


!     integer, parameter :: nspecies=num_ebu
      real, dimension (num_ebu) :: eburn_in 
      real, dimension (kte,num_ebu) :: eburn_out
      real, dimension (kte) :: u_in ,v_in ,w_in ,theta_in ,pi_in  &
                              ,rho_phyin ,qv_in ,zmid    &
                              ,z_lev
! real, dimension(nveg_agreg) :: firesize,mean_fct
      real :: sum, ffirs
!     real,save,dimension(its:ite,jts:jte) :: ffirs
      ffirs=0.
      nspecies=num_ebu
!     write(0,*)'plumerise'

       do j=jts,jte
          do i=its,ite
               ebu(i,kts,j,p_ebu_no)=ebu_in(i,1,j,p_ebu_in_no)
               ebu(i,kts,j,p_ebu_co)=ebu_in(i,1,j,p_ebu_in_co)
               ebu(i,kts,j,p_ebu_co2)=ebu_in(i,1,j,p_ebu_in_co2)
               ebu(i,kts,j,p_ebu_eth)=ebu_in(i,1,j,p_ebu_in_eth)
               ebu(i,kts,j,p_ebu_hc3)=ebu_in(i,1,j,p_ebu_in_hc3)
               ebu(i,kts,j,p_ebu_hc5)=ebu_in(i,1,j,p_ebu_in_hc5)
               ebu(i,kts,j,p_ebu_hc8)=ebu_in(i,1,j,p_ebu_in_hc8)
               ebu(i,kts,j,p_ebu_ete)=ebu_in(i,1,j,p_ebu_in_ete)
               ebu(i,kts,j,p_ebu_olt)=ebu_in(i,1,j,p_ebu_in_olt)
               ebu(i,kts,j,p_ebu_oli)=ebu_in(i,1,j,p_ebu_in_oli)
               ebu(i,kts,j,p_ebu_pm25)=ebu_in(i,1,j,p_ebu_in_pm25)
               ebu(i,kts,j,p_ebu_pm10)=ebu_in(i,1,j,p_ebu_in_pm10)
               ebu(i,kts,j,p_ebu_dien)=ebu_in(i,1,j,p_ebu_in_dien)
               ebu(i,kts,j,p_ebu_iso)=ebu_in(i,1,j,p_ebu_in_iso)
               ebu(i,kts,j,p_ebu_api)=ebu_in(i,1,j,p_ebu_in_api)
               ebu(i,kts,j,p_ebu_lim)=ebu_in(i,1,j,p_ebu_in_lim)
               ebu(i,kts,j,p_ebu_tol)=ebu_in(i,1,j,p_ebu_in_tol)
               ebu(i,kts,j,p_ebu_xyl)=ebu_in(i,1,j,p_ebu_in_xyl)
               ebu(i,kts,j,p_ebu_csl)=ebu_in(i,1,j,p_ebu_in_csl)
               ebu(i,kts,j,p_ebu_hcho)=ebu_in(i,1,j,p_ebu_in_hcho)
               ebu(i,kts,j,p_ebu_ald)=ebu_in(i,1,j,p_ebu_in_ald)
               ebu(i,kts,j,p_ebu_ket)=ebu_in(i,1,j,p_ebu_in_ket)
               ebu(i,kts,j,p_ebu_macr)=ebu_in(i,1,j,p_ebu_in_macr)
               ebu(i,kts,j,p_ebu_ora1)=ebu_in(i,1,j,p_ebu_in_ora1)
               ebu(i,kts,j,p_ebu_ora2)=ebu_in(i,1,j,p_ebu_in_ora2)
               ebu(i,kts,j,p_ebu_sulf)=ebu_in(i,1,j,p_ebu_in_sulf)
               ebu(i,kts,j,p_ebu_bc)=ebu_in(i,1,j,p_ebu_in_bc)
               ebu(i,kts,j,p_ebu_oc)=ebu_in(i,1,j,p_ebu_in_oc)
               ebu(i,kts,j,p_ebu_so2)=ebu_in(i,1,j,p_ebu_in_so2)
               ebu(i,kts,j,p_ebu_dms)=ebu_in(i,1,j,p_ebu_in_dms)
             do nv=1,num_ebu
                do k=kts+1,kte
                  ebu(i,k,j,nv)=0.
                enddo
             enddo
          enddo
       enddo
       do j=jts,jte
          do i=its,ite
            sum=mean_fct_agtf(i,j)+mean_fct_agef(i,j)+mean_fct_agsv(i,j)    &
                    +mean_fct_aggr(i,j)
            if(sum.lt.1.e-6)Cycle
!           write(0,*)'before',i,j,ebu_co(i,1,j),sum
!           ffirs=ffirs+1
            eburn_out=0.
            mean_fct(1)=mean_fct_agtf(i,j)
            mean_fct(2)=mean_fct_agef(i,j)
            mean_fct(3)=mean_fct_agsv(i,j)
            mean_fct(4)=mean_fct_aggr(i,j)
            firesize(1)=firesize_agtf(i,j)
            firesize(2)=firesize_agef(i,j)
            firesize(3)=firesize_agsv(i,j)
            firesize(4)=firesize_aggr(i,j)
            do nv=1,num_ebu
            eburn_in(nv)=ebu(i,kts,j,nv)
            enddo
            do k=kts,kte
              u_in(k)=u_phy(i,k,j)
              v_in(k)=v_phy(i,k,j)
              w_in(k)=vvel(i,k,j)
              qv_in(k)=moist(i,k,j,p_qv)
              pi_in(k)=cp*(p_phy(i,k,j)/p1000mb)**rcp
              zmid(k)=z(i,k,j)-z_at_w(i,kts,j)
              z_lev(k)=z_at_w(i,k,j)-z_at_w(i,kts,j)
              rho_phyin(k)=rho_phy(i,k,j)
              theta_in(k)=t_phy(i,k,j)/pi_in(k)*cp
!             if(ffirs.le.5)then
!               write(0,*)k,u_in(k),w_in(k),qv_in(k),pi_in(k)
!             endif
            enddo
!!$              pi_in(kte)=pi_in(kte-1)  !wig: These are no longer needed after changing definition
!!$              u_in(kte)=u_in(kte-1)    !     of kte in chem_driver (12-Oct-2007)
!!$              v_in(kte)=v_in(kte-1)
!!$              w_in(kte)=w_in(kte-1)
!!$              qv_in(kte)=qv_in(kte-1)
!!$              zmid(kte)=z(i,kte,j)-z_at_w(i,kts,j)
!!$              z_lev(kte)=z_at_w(i,kte,j)-z_at_w(i,kts,j)
!!$              rho_phyin(kte)=rho_phyin(kte-1)
!!$              theta_in(kte)=theta_in(kte-1)
!             if(ffirs.le.5)then
!           do k=kts,kte
!               write(0,*)k,z_lev(k),zmid(k),rho_phyin(k),theta_in(k)
!           enddo
!               write(0,*)'eburn',eburn_in(1),mean_fct,firesize
!             endif

            call plumerise(kte,1,1,1,1,1,1,firesize,mean_fct  &
                    ,nspecies,eburn_in,eburn_out &
                    ,u_in ,v_in ,w_in ,theta_in ,pi_in  &
                    ,rho_phyin ,qv_in ,zmid    &
                    ,z_lev         )
!             if(ffirs.le.5)then
!           do k=kts,kte
!               write(0,*)'eburn_out ',k,i,j,eburn_out(k,1)
!           enddo
!             endif
            do nv=1,num_ebu
            do k=kts+1,kte
            ebu(i,k,j,nv)=eburn_out(k,nv)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            enddo
            enddo

          enddo
          enddo
end subroutine plumerise_driver

END Module module_plumerise1
