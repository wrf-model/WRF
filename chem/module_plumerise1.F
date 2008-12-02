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
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,         &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,    &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,      &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,      &
           ebu_dms,ebu_sulf,                                              &
           ebu_in_no,ebu_in_co,ebu_in_co2,ebu_in_eth,ebu_in_hc3,ebu_in_hc5,      &
           ebu_in_hc8,ebu_in_ete,ebu_in_olt,ebu_in_oli,ebu_in_pm25,ebu_in_pm10,  &
           ebu_in_dien,ebu_in_iso,ebu_in_api,ebu_in_lim,ebu_in_tol,ebu_in_xyl,   &
           ebu_in_csl,ebu_in_hcho,ebu_in_ald,ebu_in_ket,ebu_in_macr,ebu_in_ora1, &
           ebu_in_ora2,ebu_in_bc,ebu_in_oc,ebu_in_so2,ebu_in_dms,ebu_in_sulf,    &
           mean_fct_agtf,mean_fct_agef,mean_fct_agsv,mean_fct_aggr,              &
           firesize_agtf,firesize_agef,firesize_agsv,firesize_aggr,              &
           config_flags, t_phy,moist,                                     &
           chem,rho_phy,vvel,u_phy,v_phy,p_phy,                              &
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
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),                 &
         INTENT(INOUT ) ::                                   chem
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                 &
         INTENT(INOUT ) ::                                                &
           ebu_no,ebu_co,ebu_co2,ebu_eth,ebu_hc3,ebu_hc5,ebu_hc8,         &
           ebu_ete,ebu_olt,ebu_oli,ebu_pm25,ebu_pm10,ebu_dien,ebu_iso,    &
           ebu_api,ebu_lim,ebu_tol,ebu_xyl,ebu_csl,ebu_hcho,ebu_ald,      &
           ebu_ket,ebu_macr,ebu_ora1,ebu_ora2,ebu_bc,ebu_oc,ebu_so2,       &
           ebu_dms,ebu_sulf

   REAL, DIMENSION( ims:ime, jms:jme ),                                        &
         OPTIONAL,                                                             &
         INTENT(IN ) ::                                                        &
         ebu_in_no,ebu_in_co,ebu_in_co2,ebu_in_eth,ebu_in_hc3,ebu_in_hc5,      &
         ebu_in_hc8,ebu_in_ete,ebu_in_olt,ebu_in_oli,ebu_in_pm25,ebu_in_pm10,  &
         ebu_in_dien,ebu_in_iso,ebu_in_api,ebu_in_lim,ebu_in_tol,ebu_in_xyl,   &
         ebu_in_csl,ebu_in_hcho,ebu_in_ald,ebu_in_ket,ebu_in_macr,ebu_in_ora1, &
         ebu_in_ora2,ebu_in_bc,ebu_in_oc,ebu_in_so2,ebu_in_dms,ebu_in_sulf

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
      INTEGER :: i, j, k, ksub


      integer, parameter :: nspecies=30
      real, dimension (nspecies) :: eburn_in 
      real, dimension (kte,nspecies) :: eburn_out
      real, dimension (kte) :: u_in ,v_in ,w_in ,theta_in ,pi_in  &
                              ,rho_phyin ,qv_in ,zmid    &
                              ,z_lev
! real, dimension(nveg_agreg) :: firesize,mean_fct
      real :: sum, ffirs
!     real,save,dimension(its:ite,jts:jte) :: ffirs
      ffirs=0.
!     write(0,*)'plumerise'

       do j=jts,jte
          do i=its,ite
               ebu_no(i,kts,j)=ebu_in_no(i,j)
               ebu_co(i,kts,j)=ebu_in_co(i,j)
               ebu_co2(i,kts,j)=ebu_in_co2(i,j)
               ebu_eth(i,kts,j)=ebu_in_eth(i,j)
               ebu_hc3(i,kts,j)=ebu_in_hc3(i,j)
               ebu_hc5(i,kts,j)=ebu_in_hc5(i,j)
               ebu_hc8(i,kts,j)=ebu_in_hc8(i,j)
               ebu_ete(i,kts,j)=ebu_in_ete(i,j)
               ebu_olt(i,kts,j)=ebu_in_olt(i,j)
               ebu_oli(i,kts,j)=ebu_in_oli(i,j)
               ebu_pm25(i,kts,j)=ebu_in_pm25(i,j)
               ebu_pm10(i,kts,j)=ebu_in_pm10(i,j)
               ebu_dien(i,kts,j)=ebu_in_dien(i,j)
               ebu_iso(i,kts,j)=ebu_in_iso(i,j)
               ebu_api(i,kts,j)=ebu_in_api(i,j)
               ebu_lim(i,kts,j)=ebu_in_lim(i,j)
               ebu_tol(i,kts,j)=ebu_in_tol(i,j)
               ebu_xyl(i,kts,j)=ebu_in_xyl(i,j)
               ebu_csl(i,kts,j)=ebu_in_csl(i,j)
               ebu_hcho(i,kts,j)=ebu_in_hcho(i,j)
               ebu_ald(i,kts,j)=ebu_in_ald(i,j)
               ebu_ket(i,kts,j)=ebu_in_ket(i,j)
               ebu_macr(i,kts,j)=ebu_in_macr(i,j)
               ebu_ora1(i,kts,j)=ebu_in_ora1(i,j)
               ebu_ora2(i,kts,j)=ebu_in_ora2(i,j)
               ebu_sulf(i,kts,j)=ebu_in_sulf(i,j)
               ebu_bc(i,kts,j)=ebu_in_bc(i,j)
               ebu_oc(i,kts,j)=ebu_in_oc(i,j)
               ebu_so2(i,kts,j)=ebu_in_so2(i,j)
               ebu_dms(i,kts,j)=ebu_in_dms(i,j)
             do k=kts+1,kte
               ebu_co(i,k,j)=0.
               ebu_co2(i,k,j)=0.
               ebu_eth(i,k,j)=0.
               ebu_hc3(i,k,j)=0.
               ebu_hc5(i,k,j)=0.
               ebu_hc8(i,k,j)=0.
               ebu_ete(i,k,j)=0.
               ebu_olt(i,k,j)=0.
               ebu_oli(i,k,j)=0.
               ebu_pm25(i,k,j)=0.
               ebu_pm10(i,k,j)=0.
               ebu_dien(i,k,j)=0.
               ebu_iso(i,k,j)=0.
               ebu_api(i,k,j)=0.
               ebu_lim(i,k,j)=0.
               ebu_tol(i,k,j)=0.
               ebu_xyl(i,k,j)=0.
               ebu_csl(i,k,j)=0.
               ebu_hcho(i,k,j)=0.
               ebu_ald(i,k,j)=0.
               ebu_ket(i,k,j)=0.
               ebu_macr(i,k,j)=0.
               ebu_ora1(i,k,j)=0.
               ebu_ora2(i,k,j)=0.
               ebu_sulf(i,k,j)=0.
               ebu_bc(i,k,j)=0.
               ebu_oc(i,k,j)=0.
               ebu_so2(i,k,j)=0.
               ebu_dms(i,k,j)=0.
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
            eburn_in(1)=ebu_no(i,kts,j)
            eburn_in(2)=ebu_co(i,kts,j)
            eburn_in(3)=ebu_co2(i,kts,j)
            eburn_in(4)=ebu_eth(i,kts,j)
            eburn_in(5)=ebu_hc3(i,kts,j)
            eburn_in(6)=ebu_hc5(i,kts,j)
            eburn_in(7)=ebu_hc8(i,kts,j)
            eburn_in(8)=ebu_ete(i,kts,j)
            eburn_in(9)=ebu_olt(i,kts,j)
            eburn_in(10)=ebu_oli(i,kts,j)
            eburn_in(11)=ebu_pm25(i,kts,j)
            eburn_in(12)=ebu_pm10(i,kts,j)
            eburn_in(13)=ebu_dien(i,kts,j)
            eburn_in(14)=ebu_iso(i,kts,j)
            eburn_in(15)=ebu_api(i,kts,j)
            eburn_in(16)=ebu_lim(i,kts,j)
            eburn_in(17)=ebu_tol(i,kts,j)
            eburn_in(18)=ebu_xyl(i,kts,j)
            eburn_in(19)=ebu_csl(i,kts,j)
            eburn_in(20)=ebu_hcho(i,kts,j)
            eburn_in(21)=ebu_ald(i,kts,j)
            eburn_in(22)=ebu_ket(i,kts,j)
            eburn_in(23)=ebu_macr(i,kts,j)
            eburn_in(24)=ebu_ora1(i,kts,j)
            eburn_in(25)=ebu_ora2(i,kts,j)
            eburn_in(26)=ebu_sulf(i,kts,j)
            eburn_in(27)=ebu_oc(i,kts,j)
            eburn_in(28)=ebu_bc(i,kts,j)
            eburn_in(29)=ebu_so2(i,kts,j)
            eburn_in(30)=ebu_dms(i,kts,j)
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
            do k=kts+1,kte
            ebu_no(i,k,j)=eburn_out(k,1)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_co(i,k,j)=eburn_out(k,2)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
!           write(0,*)'after',k,ebu_co(i,k,j)
            ebu_co2(i,k,j)=eburn_out(k,3)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_eth(i,k,j)=eburn_out(k,4)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_hc3(i,k,j)=eburn_out(k,5)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_hc5(i,k,j)=eburn_out(k,6)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_hc8(i,k,j)=eburn_out(k,7)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_ete(i,k,j)=eburn_out(k,8)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_olt(i,k,j)=eburn_out(k,9)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_oli(i,k,j)=eburn_out(k,10)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_pm25(i,k,j)=eburn_out(k,11)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_pm10(i,k,j)=eburn_out(k,12)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_dien(i,k,j)=eburn_out(k,13)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_iso(i,k,j)=eburn_out(k,14)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_api(i,k,j)=eburn_out(k,15)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_lim(i,k,j)=eburn_out(k,16)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_tol(i,k,j)=eburn_out(k,17)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_xyl(i,k,j)=eburn_out(k,18)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_csl(i,k,j)=eburn_out(k,19)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_hcho(i,k,j)=eburn_out(k,20)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_ald(i,k,j)=eburn_out(k,21)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_ket(i,k,j)=eburn_out(k,22)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_macr(i,k,j)=eburn_out(k,23)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_ora1(i,k,j)=eburn_out(k,24)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_ora2(i,k,j)=eburn_out(k,25)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_sulf(i,k,j)=eburn_out(k,26)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_oc(i,k,j)=eburn_out(k,27)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_bc(i,k,j)=eburn_out(k,28)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_so2(i,k,j)=eburn_out(k,29)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            ebu_dms(i,k,j)=eburn_out(k,30)*(z_at_w(i,k+1,j)-z_at_w(i,k,j))
            enddo

          enddo
          enddo
end subroutine plumerise_driver

END Module module_plumerise1
