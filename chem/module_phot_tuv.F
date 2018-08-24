!#define SW_DEBUG

   module module_phot_tuv

   use params_mod, only : dp, m2km, ppm2vmr, o2vmr, km2cm, m2s

   IMPLICIT NONE

   private
   public :: tuv_driver, tuv_init, tuv_timestep_init

   real, parameter :: conv1 = km2cm*.5
   real, parameter :: conv2 = conv1*ppm2vmr
   real, parameter :: bext340 = 5.E-6
   real, parameter :: bexth2o = 5.E-6

   integer :: nj
   integer :: nlambda_start = 1
   integer :: nlambda_af_start = 1
   integer :: nlambda_af_end   = 1
   integer :: nconc, ntemp, nwave
   integer :: n_temp_data, n_o3_data, n_air_dens_data
   integer :: j_o2_ndx = -1
   integer :: last, next 
   integer :: curjulday = 0
   integer, allocatable :: rxn_ndx(:)
   real    :: dels
   real    :: esfact
   logical :: has_exo_coldens
   logical :: tuv_is_initialized = .false.

   real, allocatable :: temp_tab(:)
   real, allocatable :: conc_tab(:)
   real, allocatable :: del_temp_tab(:)
   real, allocatable :: del_conc_tab(:)
   real, allocatable :: wl(:)
   real, allocatable :: wc(:)
   real, allocatable :: dw(:)
   real, allocatable :: w_fac(:)
   real, allocatable :: etfl(:)
   real, allocatable :: albedo(:)
   real, allocatable :: o2_xs(:)
   real, allocatable :: so2_xs(:)
   real, allocatable :: par_wght(:)
   real, allocatable :: ery_wght(:)
   real, allocatable :: z_temp_data(:), temp_data(:)
   real, allocatable :: z_o3_data(:), o3_data(:)
   real, allocatable :: z_air_dens_data(:), air_dens_data(:)
   real, allocatable :: o3_xs_tab(:,:)
   real, allocatable :: no2_xs_tab(:,:)
   real, allocatable :: xsqy_tab(:,:,:,:)
   character(len=32), allocatable :: tuv_jname(:)
   logical :: has_so2, has_no2
   logical :: is_full_tuv = .true.
!  logical :: is_full_tuv = .false.
   logical :: rxn_initialized
   logical, allocatable :: xsqy_is_zdep(:)

   type column_density
     integer       :: ncoldens_levs
     integer       :: ndays_of_year
     real(8), pointer :: col_levs(:)
     real(8), pointer :: day_of_year(:)
     real(8), pointer :: o3_col_dens(:,:,:,:)
     real(8), pointer :: o2_col_dens(:,:,:,:)
     logical       :: is_allocated
   end type column_density

   type(column_density), allocatable :: col_dens(:)

   CONTAINS

   subroutine tuv_driver(                                                 &
               dm, curr_secs, ktau, config_flags, haveaer,                &
               gmt, julday, t_phy, moist, aerwrf,                         &
               p8w, t8w, p_phy, chem, rho_phy,                            &
               dz8w, xlat, xlong, z, z_at_w, gd_cloud, gd_cloud2,         &
               tauaer1,tauaer2,tauaer3,tauaer4,                           &
               gaer1,gaer2,gaer3,gaer4,                                   &
               waer1,waer2,waer3,waer4,                                   &
               ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,   &
               ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,       &
               ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,            &
               ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,   &
               ph_n2o5,ph_o2,ph_n2o,ph_pooh,ph_pan,ph_mvk,ph_hyac,        &
               ph_glyald,ph_mek,ph_gly,                                   &
               pm2_5_dry,pm2_5_water,uvrad,                               &
               dt_cld,af_dir,af_dn,af_up,par,erythema,                    &
               ids,ide, jds,jde, kds,kde,                                 &
               ims,ime, jms,jme, kms,kme,                                 &
               its,ite, jts,jte, kts,kte )

   USE module_configure
   USE module_state_description
   USE module_model_constants
   USE module_data_radm2
   USE tuv_subs,   only : tuv_radfld, sundis, calc_zenith
   USE module_params, only : kz
   USE srb,        only : sjo2
   USE module_rxn, only : xsqy_table => xsqy_tab, the_subs, set_initialization
   USE module_rxn, only : get_initialization
   USE module_xsections, only : o3xs, no2xs_jpl06a

!---------------------------------------------------------------------
!  ... dummy arguments
!---------------------------------------------------------------------
   INTEGER,      INTENT(IN   ) :: dm,julday,                    &
                                  ids,ide, jds,jde, kds,kde,    &
                                  ims,ime, jms,jme, kms,kme,    &
                                  its,ite, jts,jte, kts,kte
   INTEGER,      INTENT(IN   ) :: ktau
   REAL(KIND=8), INTENT(IN   ) :: curr_secs
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_moist ),        &
         INTENT(IN ) ::                                   moist
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                   &
         INTENT(INOUT ) ::                                         &
               pm2_5_dry,pm2_5_water
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                   &
         INTENT(INOUT ) ::                                         &
               gd_cloud,gd_cloud2
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),              &
         INTENT(IN   ) :: tauaer1, tauaer2, tauaer3, tauaer4, &
                          waer1, waer2, waer3, waer4,         &
                          gaer1, gaer2, gaer3, gaer4
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ),                   &
         INTENT(INOUT ) ::                                         &
           ph_macr,ph_o31d,ph_o33p,ph_no2,ph_no3o2,ph_no3o,ph_hno2,&
           ph_hno3,ph_hno4,ph_h2o2,ph_ch2or,ph_ch2om,ph_ch3cho,    &
           ph_ch3coch3,ph_ch3coc2h5,ph_hcocho,ph_ch3cocho,         &
           ph_hcochest,ph_ch3o2h,ph_ch3coo2h,ph_ch3ono2,ph_hcochob,&
           ph_n2o5,ph_o2,ph_n2o,ph_pooh,ph_pan,ph_mvk,ph_hyac,     &
           ph_glyald,ph_mek,ph_gly
   REAL, INTENT(INOUT) :: dt_cld(ims:ime,kms:kme,jms:jme),         &
                          af_dir(ims:ime,kms:kme,jms:jme),         &
                          af_dn(ims:ime,kms:kme,jms:jme),          &
                          af_up(ims:ime,kms:kme,jms:jme),          &
                          par(ims:ime,kms:kme,jms:jme),            &
                          erythema(ims:ime,kms:kme,jms:jme)
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme, num_chem ),      &
         INTENT(IN ) ::                               chem
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,    &
          INTENT(IN   ) ::                                      &
                                                      z,        &
                                                      t_phy,    &
                                                      p_phy,    &
                                                      dz8w,     &
                                              t8w,p8w,z_at_w ,  &     
                                                      aerwrf ,  &     
                                                      rho_phy           
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &     
          INTENT(IN   ) ::                                      &     
                                                     xlat,      &
                                                     xlong
   REAL,  DIMENSION( ims:ime , jms:jme )                   ,    &     
          INTENT(INOUT   ) ::                        uvrad
   REAL,     INTENT(IN   ) ::                        gmt

   TYPE(grid_config_rec_type),  INTENT(IN   )    :: config_flags

   LOGICAL, INTENT(IN) :: haveaer
         
!---------------------------------------------------------------------
!  ... local variables
!---------------------------------------------------------------------
   real, parameter     :: boltz = 1.38065e-23                  ! J/K/molecule
   real, parameter     :: xair_kg = 1.e3/28.97*6.023e23*1.e-6
   real(dp), parameter :: Pa2hPa = .01_dp

   integer :: astat, ierr
   integer :: k, ki, kp1, i, j, n, wn
   integer :: ktsm1, ktem1
   integer :: nlev, nlyr
   integer :: jndx
   integer :: n_tuv_z, n_exo_z, last_n_tuv_z
   integer :: min_ndx(2), max_ndx(2)
   integer :: myproc
   integer(kind=8) :: ixhour
   integer :: minndx(2)

   real :: xmin, gmtp, uvb_dd1, uvb_du1, uvb_dir1
   real :: zenith, dobsi
   real :: delz_top
   real(kind=8) :: xtime, xhour
   real :: max_tauaer
   real :: Dobson(2)
   real :: xsect(nwave)
   real :: wrk(nwave), wrk_lam(nwave)
!  real :: tlev(kts-1:kte)
!  real :: tlyr(kts-1:kte)
!  real :: o33(kts-1:kte)
   real :: rhoa(kts-1:kte)
!  real :: dens_air(kts-1:kte)
   real :: aerext(kts-1:kte)
!  real :: qll(kts-1:kte)
!  real :: aircol(kts-1:kte)
!  real :: o3col(kts-1:kte)
!  real :: o2col(kts-1:kte)
!  real :: so2col(kts-1:kte)
!  real :: no2col(kts-1:kte)
   real :: dtcld_col(kts:kte)
!  real :: cldfrac(kts-1:kte)
   real :: par_col(kts:kte)
!  real :: tauaer300(kts-1:kte), tauaer400(kts-1:kte),        &
!          tauaer600(kts-1:kte), tauaer999(kts-1:kte)
!  real :: waer300(kts-1:kte), waer400(kts-1:kte),            &
!          waer600(kts-1:kte), waer999(kts-1:kte)
!  real :: gaer300(kts-1:kte), gaer400(kts-1:kte),            &
!          gaer600(kts-1:kte), gaer999(kts-1:kte)
   real :: zen_angle(ims:ime,jms:jme)
   real :: z_top(its:ite,jts:jte)
   real :: z_exo(1), dens_exo(1), temp_exo(1)
   real :: dummy(kz)
   real(dp) :: p_top(its:ite,jts:jte)
   real(dp) :: o2_exo_col(its:ite,jts:jte)
   real(dp) :: o3_exo_col(its:ite,jts:jte)
   real(dp) :: o3_exo_col_at_grnd(its:ite,jts:jte)

   logical  :: do_alloc
   logical  :: has_aer_ra_feedback

   real, allocatable :: tuv_z(:)
   real, allocatable :: dtuv_z(:)
   real, allocatable :: cldfrac(:)
   real, allocatable :: qll(:)
   real, allocatable :: tlev(:)
   real, allocatable :: tlyr(:)
   real, allocatable :: dens_air(:)
   real, allocatable :: o33(:)
   real, allocatable :: aircol(:)
   real, allocatable :: o3col(:)
   real, allocatable :: o2col(:)
   real, allocatable :: so2col(:)
   real, allocatable :: no2col(:)
   real, allocatable :: tauaer300(:), tauaer400(:), tauaer600(:), tauaer999(:)
   real, allocatable :: waer300(:), waer400(:), waer600(:), waer999(:)
   real, allocatable :: gaer300(:), gaer400(:), gaer600(:), gaer999(:)

   real, allocatable :: dtcld(:,:), dtaer(:,:)
   real, allocatable :: omcld(:,:), omaer(:,:)
   real, allocatable :: gcld(:,:), gaer(:,:)

   real, allocatable :: rad_fld(:,:)
   real, allocatable :: e_fld(:,:)
   real, allocatable :: tuv_prate(:,:)
   real, allocatable :: xsqy(:,:)
   real, allocatable :: srb_o2_xs(:,:)
   real, allocatable :: o3_xs(:,:), o3_xs_tpose(:,:)
   real, allocatable :: no2_xs(:,:), no2_xs_tpose(:,:)
   real, allocatable :: rad_fld_tpose(:,:)
   real, allocatable :: dir_fld(:,:), dwn_fld(:,:), up_fld(:,:)
   real, allocatable :: e_dir(:,:), e_dn(:,:), e_up(:,:)

   REAL               :: zexo_grd(2*kte)
   CHARACTER(len=256) :: msg

#ifdef SW_DEBUG
   logical :: tuv_diags
#endif

   call wrf_get_myproc( myproc )
   has_aer_ra_feedback = config_flags%aer_ra_feedback == 1

   xtime  = curr_secs/60._8   ! min since simulation start
   ixhour = int( gmt+.01,8 ) + int( xtime/60._8,8 )
   xhour  = real( ixhour,8 )
   xmin   = 60.*gmt + real( xtime-xhour*60._8,4 )
   gmtp   = real( mod(xhour,24._8),4 )
   gmtp   = gmtp + xmin/60.

   call calc_zenith( xlat, -xlong, &
                     julday, real(gmtp,8), zen_angle, &
                     its, ite, jts, jte, &
                     ims, ime, jms, jme )

#ifdef SW_DEBUG
   if( myproc == 6 ) then
     write(*,'(''tuv_drvr: its,jts = '',2i5)') its,jts
     minndx(:) = minloc( zen_angle(its:ite,jts:jte) )
     write(*,'(''tuv_drvr: min zen_angle ndx = '',2i5)') minndx(:)
     write(*,'(''tuv_drvr: min zen_angle = '',1p,g15.8)') minval( zen_angle(its:ite,jts:jte) )
     minndx(1) = minndx(1) + its - 1
     minndx(2) = minndx(2) + jts - 1
     write(*,'(''tuv_drvr: minndx = '',2i5)') minndx(:)
     write(*,'(''tuv_drvr: min zen_angle = '',1p,g15.8)') zen_angle(minndx(1),minndx(2))
   endif
#endif
   

   do j = jts,jte
     where( zen_angle(its:ite,j) == 90. )
       zen_angle(its:ite,j) = 89.9
     endwhere
   end do

   ktsm1 = kts - 1 ; ktem1 = kte - 1

   allocate( tuv_prate(kts:kte,nj),stat=ierr )
   if( ierr /= 0 ) then
     call wrf_error_fatal( 'tuv_driver: failed to allocate tuv_prate' )
   endif

any_daylight: &
   if( any( zen_angle(its:ite,jts:jte) < 90. ) ) then
     if( config_flags%has_o3_exo_coldens ) then
!      p_top(its:ite,jts:jte) = Pa2hPa * real( p_phy(its:ite,kte,jts:jte),dp )
       z_exo(1) = 50.
       call z_interp( z_air_dens_data, air_dens_data, n_air_dens_data, &
                      z_exo, dens_exo )
       call z_interp( z_temp_data, temp_data, n_temp_data, z_exo, temp_exo )
       p_top(its:ite,jts:jte) = temp_exo(1)*boltz*dens_exo(1)*1.e6*Pa2hPa
       call tuv_timestep_init( dm, julday )
       call p_interp( o2_exo_col, o3_exo_col, o3_exo_col_at_grnd, p_top, &
                      dm, its, ite, jts, jte )
     endif
     allocate( rad_fld(nwave,kts:kte),e_fld(kts:kte,nwave),stat=astat )
     ierr = ierr + astat
     allocate( dir_fld(kts:kte,nwave), dwn_fld(kts:kte,nwave), up_fld(kts:kte,nwave),stat=astat )
     ierr = ierr + astat
     allocate( e_dir(kts:kte,nwave), e_dn(kts:kte,nwave), e_up(kts:kte,nwave),stat=astat )
     ierr = ierr + astat
     if( .not. is_full_tuv ) then
       if( any( .not. xsqy_is_zdep(:) ) ) then
         allocate( rad_fld_tpose(kts:kte,nwave),stat=astat )
       endif
     elseif( any( xsqy_table(2:nj)%tpflag == 0 ) ) then
       allocate( rad_fld_tpose(kts:kte,nwave),stat=astat )
     endif
     ierr = ierr + astat
     allocate( srb_o2_xs(nwave,kts:kte) )
     ierr = ierr + astat
     allocate( xsqy(nwave,kts:kte) )
     ierr = ierr + astat
     if( ierr /= 0 ) then
       call wrf_error_fatal( 'tuv_driver: failed to allocate rad_fld ... xsqy' )
     endif
!-----------------------------------------------------------------------------
!     set solar distance factor
!-----------------------------------------------------------------------------
     if( curjulday /= julday ) then
       curjulday = julday
       esfact = sundis( julday )
     endif
     if( .not. config_flags%scale_o3_to_grnd_exo_coldens ) then
       if( config_flags%scale_o3_to_du_at_grnd ) then
         dobsi = max( 0.,config_flags%du_at_grnd )
       else
         dobsi = 0.
       endif
     endif
   endif any_daylight

   if( ierr /= 0 ) then
     call wrf_error_fatal( 'tuv_driver: failed to allocate module variables' )
   endif

   z_top(:,:) = 0.
   do j = jts,jte
     do i = its,ite
       if( zen_angle(i,j) < 90. ) then
         wrk(1) = m2km*(z(i,kte,j) - z_at_w(i,kts,j))
         z_top(i,j) = real( ceiling(wrk(1)) )
         delz_top = z_top(i,j) - wrk(1)
         if( delz_top < .3 ) then
           z_top(i,j) = z_top(i,j) + 1.
         endif
       endif
     end do
   end do

   if( is_full_tuv ) then
     rxn_initialized = .not. get_initialization()
     if( .not. rxn_initialized ) then
       do n = 1,nj
         jndx = rxn_ndx(n)
         if( jndx /= -1 ) then
           call the_subs(jndx)%xsqy_sub(nwave+1,wl,wc,kz,dummy,dummy,jndx)
         endif
       enddo
       call set_initialization( status=.false. )
     endif
   endif

   last_n_tuv_z = -1
lat_loop : &
   do j = jts,jte
long_loop : &
     do i = its,ite
#ifdef SW_DEBUG
      tuv_diags = i == minndx(1) .and. j == minndx(2)
#endif
       do n = 1,nj
         tuv_prate(:,n) = 0.
       end do
       zenith = zen_angle(i,j)
!---------------------------------------------------------------------
! if night, skip radiative field calculation
!---------------------------------------------------------------------
has_daylight : &
       if( zenith < 90. ) then
         do k = kts,kte
           rad_fld(:,k) = 0.
         end do
         do wn = 1,nwave
           e_fld(:,wn) = 0.
         end do
         wrk(1) = m2km*(z(i,kte,j) - z_at_w(i,kts,j))
         call setzgrid( wrk(1), n_exo_z, zexo_grd )
         n_tuv_z = kte + n_exo_z
         nlev = n_tuv_z - ktsm1 + 1
         nlyr = nlev - 1
         do_alloc = n_tuv_z /= last_n_tuv_z
         last_n_tuv_z = n_tuv_z
!---------------------------------------------------------------------
! allocate column variables
!---------------------------------------------------------------------
         if( do_alloc ) then
           call tuv_deallocate
           call tuv_allocate
         endif

!---------------------------------------------------------------------
! column vertical grid including exo-model top
!---------------------------------------------------------------------
         tuv_z(ktsm1)   = 0.
         tuv_z(kts:kte) = (z(i,kts:kte,j) - z_at_w(i,kts,j))*m2km
         tuv_z(kte+1:kte+n_exo_z) = zexo_grd(1:n_exo_z)
!        tuv_z(kte+1:kte+n_exo_z) = real( (/ (n,n=k,k+n_exo_z-1) /) )
!        tuv_z(kte+n_exo_z+1:n_tuv_z) = real( (/ (25+5*n,n=1,5) /) )
!---------------------------------------------------------------------
! cloud fraction
!---------------------------------------------------------------------
         cldfrac(:) = 0.
         if( config_flags%cld_od_opt > 1 ) then
           if( config_flags%pht_cldfrc_opt == 1 ) then
             call cldfrac_binary( cldfrac(kts:kte), &
                                  moist(i,kts:kte,j,p_qc), moist(i,kts:kte,j,p_qi), &
                                  moist(i,kts:kte,j,p_qs), kts, kte )
           elseif( config_flags%pht_cldfrc_opt == 2 ) then
             if( config_flags%cu_diag == 1 ) then
               call cldfrac_fractional( cldfrac(kts:kte), &
                                        moist(i,kts:kte,j,p_qv), &
                                        moist(i,kts:kte,j,p_qc) + gd_cloud(i,kts:kte,j),  &
                                        moist(i,kts:kte,j,p_qi) + gd_cloud2(i,kts:kte,j), &
                                        moist(i,kts:kte,j,p_qs), &
                                        p_phy(i,kts:kte,j), t_phy(i,kts:kte,j), kts, kte )
             else
               call cldfrac_fractional( cldfrac(kts:kte), &
                                        moist(i,kts:kte,j,p_qv), moist(i,kts:kte,j,p_qc), &
                                        moist(i,kts:kte,j,p_qi), moist(i,kts:kte,j,p_qs), &
                                        p_phy(i,kts:kte,j), t_phy(i,kts:kte,j), kts, kte )
             endif
           endif
         endif
!---------------------------------------------------------------------
! aerosols
!---------------------------------------------------------------------
         tauaer300(:) = 0.
         tauaer400(:) = 0.
         tauaer600(:) = 0.
         tauaer999(:) = 0.
         waer300(:) = 0.
         waer400(:) = 0.
         waer600(:) = 0.
         waer999(:) = 0.
         gaer300(:) = 0.
         gaer400(:) = 0.
         gaer600(:) = 0.
         gaer999(:) = 0.
         if( has_aer_ra_feedback ) then
           tauaer300(kts:kte) = tauaer1(i,kts:kte,j)
           tauaer400(kts:kte) = tauaer2(i,kts:kte,j)
           tauaer600(kts:kte) = tauaer3(i,kts:kte,j)
           tauaer999(kts:kte) = tauaer4(i,kts:kte,j)
           waer300(kts:kte) = waer1(i,kts:kte,j)
           waer400(kts:kte) = waer2(i,kts:kte,j)
           waer600(kts:kte) = waer3(i,kts:kte,j)
           waer999(kts:kte) = waer4(i,kts:kte,j)
           gaer300(kts:kte) = gaer1(i,kts:kte,j)
           gaer400(kts:kte) = gaer2(i,kts:kte,j)
           gaer600(kts:kte) = gaer3(i,kts:kte,j)
           gaer999(kts:kte) = gaer4(i,kts:kte,j)
           tauaer300(ktsm1) = tauaer300(kts)
           tauaer400(ktsm1) = tauaer400(kts)
           tauaer600(ktsm1) = tauaer600(kts)
           tauaer999(ktsm1) = tauaer999(kts)
           waer300(ktsm1) = waer300(kts)
           waer400(ktsm1) = waer400(kts)
           waer600(ktsm1) = waer600(kts)
           waer999(ktsm1) = waer999(kts)
           gaer300(ktsm1) = gaer300(kts)
           gaer400(ktsm1) = gaer400(kts)
           gaer600(ktsm1) = gaer600(kts)
           gaer999(ktsm1) = gaer999(kts)
         endif

         tlev(ktsm1)       = t8w(i,kts,j)
         tlev(kts:kte)     = t_phy(i,kts:kte,j)
         call z_interp( z_temp_data, temp_data, n_temp_data, &
                        tuv_z(kte+1:n_tuv_z), tlev(kte+1:n_tuv_z) )
         tlyr(ktsm1:n_tuv_z-1) = .5*(tlev(ktsm1:n_tuv_z-1) + tlev(kts:n_tuv_z))

         rhoa(ktsm1)   = p8w(i,kts,j)/(t8w(i,kts,j)*r_d)
         rhoa(kts:kte) = rho_phy(i,kts:kte,j)
         dens_air(ktsm1:kte) = xair_kg*rhoa(ktsm1:kte)                  ! air num.(molecules/cm^3)
         call z_interp( z_air_dens_data, air_dens_data, n_air_dens_data, &
                        tuv_z(kte+1:n_tuv_z), dens_air(kte+1:n_tuv_z) )

         o33(kts:kte) = ppm2vmr*chem(i,kts:kte,j,p_o3)*dens_air(kts:kte)
         o33(ktsm1)   = o33(kts)
         call z_interp( z_o3_data, o3_data, n_o3_data, &
                        tuv_z(kte+1:n_tuv_z), o33(kte+1:n_tuv_z) )

         qll(:)            = 0.
         qll(kts:kte)      = moist(i,kts:kte,j,p_qc) + moist(i,kts:kte,j,p_qi)
         if( config_flags%cu_diag == 1 ) then
           qll(kts:kte) = qll(kts:kte) + gd_cloud(i,kts:kte,j) + gd_cloud2(i,kts:kte,j)
         endif
         qll(kts:kte) = 1.e3*rhoa(kts:kte)*qll(kts:kte)
         where( qll(kts:kte) < 1.e-5 )
           qll(kts:kte) = 0.
         endwhere

         if( haveaer .and. ktau > 1 )then
           aerext(ktsm1) = aerext(kts)
         else
           aerext(ktsm1) = aerwrf(i,kts,j)
         endif

         if( .not. is_full_tuv ) then
           call xs_int( o3_xs, tlyr, o3_xs_tab )
           call xs_int( no2_xs, tlyr, no2_xs_tab )
         else
           call o3xs( nlyr,tlyr,nwave+1,wl,o3_xs_tpose )
           call no2xs_jpl06a( nlyr,tlyr,nwave+1,wl,no2_xs_tpose )
           o3_xs  = transpose( o3_xs_tpose )
           no2_xs = transpose( no2_xs_tpose )
         endif

         dtuv_z(ktsm1:n_tuv_z-1) = tuv_z(kts:n_tuv_z) - tuv_z(ktsm1:n_tuv_z-1)
         aircol(ktsm1:n_tuv_z-1) = &
                    km2cm*dtuv_z(ktsm1:n_tuv_z-1) &
                         *real(sqrt(real(dens_air(kts:n_tuv_z),kind=8)*real(dens_air(ktsm1:n_tuv_z-1),kind=8)),kind=4)
!                   km2cm*dtuv_z(ktsm1:n_tuv_z-1)*(dens_air(kts:n_tuv_z) - dens_air(ktsm1:n_tuv_z-1)) &
!                 / log( dens_air(kts:n_tuv_z)/dens_air(ktsm1:n_tuv_z-1) )
         o3col(ktsm1:n_tuv_z-1) = conv1*dtuv_z(ktsm1:n_tuv_z-1)*(o33(kts:n_tuv_z) + o33(ktsm1:n_tuv_z-1))
         o2col(ktsm1:n_tuv_z-1) = o2vmr*aircol(ktsm1:n_tuv_z-1)
         if( config_flags%has_o3_exo_coldens ) then
           o3col(n_tuv_z-1)  = o3col(n_tuv_z-1) + real( o3_exo_col(i,j),4 )
           o2col(n_tuv_z-1)  = o2col(n_tuv_z-1) + real( o2_exo_col(i,j),4 )
           aircol(n_tuv_z-1) = aircol(n_tuv_z-1) + o2col(n_tuv_z-1)/o2vmr
         endif
         if( config_flags%scale_o3_to_grnd_exo_coldens ) then
           dobsi = real( o3_exo_col_at_grnd(i,j),4 )/2.687e16
         endif

         so2col(:) = 0.
         if( has_so2 ) then
           so2col(ktsm1) = conv2*dtuv_z(ktsm1) &
                           *chem(i,kts,j,p_so2)*(dens_air(ktsm1) + dens_air(kts))
           so2col(kts:ktem1) = conv2*dtuv_z(kts:ktem1) &
                               *(chem(i,kts:ktem1,j,p_so2)*dens_air(kts:ktem1) &
                                 + chem(i,kts+1:kte,j,p_so2)*dens_air(kts+1:kte))
         endif
         no2col(:) = 0.
         if( has_no2 ) then
           no2col(ktsm1) = conv2*dtuv_z(ktsm1) &
                           *chem(i,kts,j,p_no2)*(dens_air(ktsm1) + dens_air(kts))
           no2col(kts:ktem1) = conv2*dtuv_z(kts:ktem1) &
                               *(chem(i,kts:ktem1,j,p_no2)*dens_air(kts:ktem1) &
                                 + chem(i,kts+1:kte,j,p_no2)*dens_air(kts+1:kte))
         endif

         call tuv_radfld( nlambda_start, config_flags%cld_od_opt, cldfrac, &
              nlyr, nwave, zenith, tuv_z, albedo, &
              aircol, o2col, o3col, so2col, no2col, &
              tauaer300, tauaer400, tauaer600, tauaer999, &
              waer300, waer400, waer600, waer999, &
              gaer300, gaer400, gaer600, gaer999, &
              dtaer, omaer, gaer, dtcld, omcld, gcld, &
              has_aer_ra_feedback, &
              qll, dobsi, o3_xs, no2_xs, o2_xs, &
              so2_xs, wl(1), wc, tlev, srb_o2_xs, rad_fld, e_fld, &
              e_dir, e_dn, e_up, &
              dir_fld, dwn_fld, up_fld, dtcld_col )

         do k = kts,kte
           af_dir(i,k,j)   = dot_product( dir_fld(k,nlambda_af_start:nlambda_af_end),dw(nlambda_af_start:nlambda_af_end) )
           af_dn(i,k,j)    = dot_product( dwn_fld(k,nlambda_af_start:nlambda_af_end),dw(nlambda_af_start:nlambda_af_end) )
           af_up(i,k,j)    = dot_product( up_fld(k,nlambda_af_start:nlambda_af_end),dw(nlambda_af_start:nlambda_af_end) )
         end do

         dt_cld(i,kts:kte,j)   = dtcld_col(kts:kte)
         wrk(nlambda_start:nwave) = dw(nlambda_start:nwave)*etfl(nlambda_start:nwave)
         wrk_lam(nlambda_start:nwave) = wrk(nlambda_start:nwave)*par_wght(nlambda_start:nwave)
         par(i,kts:kte,j)      =  matmul( e_fld(kts:kte,nlambda_start:nwave), wrk_lam(nlambda_start:nwave) )
         wrk_lam(nlambda_start:nwave) = wrk(nlambda_start:nwave)*ery_wght(nlambda_start:nwave)
         erythema(i,kts:kte,j) =  matmul( e_fld(kts:kte,nlambda_start:nwave), wrk_lam(nlambda_start:nwave) )

         if( .not. is_full_tuv ) then
           if( any( .not. xsqy_is_zdep(:) ) ) then
             rad_fld_tpose = transpose( rad_fld )
           endif
         elseif( any( xsqy_table(1:nj)%tpflag == 0 ) ) then
           rad_fld_tpose = transpose( rad_fld )
         endif

#ifdef SW_DEBUG
         if( tuv_diags ) then
           Dobson(1) = sum( o3col(ktsm1:ktem1) )/2.687e16
           Dobson(2) = o3col(kte)/2.687e16
           write(*,'(''tuv_drvr: o3col = '',1p,2g15.8)') Dobson(:)
           open(unit=33,file='wrfchm_inp')
           write(33,*) nlev
           write(33,*) kte
           write(33,*) tuv_z(ktsm1:n_tuv_z)
           write(33,*) tlev(ktsm1:n_tuv_z)
           write(33,*) o3col(ktsm1:n_tuv_z-1)
           write(33,*) so2col(ktsm1:n_tuv_z-1)
           write(33,*) no2col(ktsm1:n_tuv_z-1)
           write(33,*) dens_air(ktsm1:n_tuv_z)
           write(33,*) aircol(ktsm1:n_tuv_z-1)
           do wn = 1,nwave
             write(33,*) dtaer(ktsm1:n_tuv_z-1,wn)
           end do
           do wn = 1,nwave
             write(33,*) omaer(ktsm1:n_tuv_z-1,wn)
           end do
           do wn = 1,nwave
             write(33,*) gaer(ktsm1:n_tuv_z-1,wn)
           end do
           do wn = 1,nwave
             write(33,*) dtcld(ktsm1:n_tuv_z-1,wn)
           end do
           do wn = 1,nwave
             write(33,*) omcld(ktsm1:n_tuv_z-1,wn)
           end do
           do wn = 1,nwave
             write(33,*) gcld(ktsm1:n_tuv_z-1,wn)
           end do
           write(33,*) zen_angle(i,j)
           write(33,*) esfact
           write(33,*) dobsi
           close( 33 )

           open(unit=33,file='WRF-TUV.flx.out' )
           do n = nlambda_start,nwave
             write(33,*) dir_fld(kts:kte,n)
           end do
           write(33,*) ' '
           do n = nlambda_start,nwave
             write(33,*) dwn_fld(kts:kte,n)
           end do
           write(33,*) ' '
           do n = nlambda_start,nwave
             write(33,*) up_fld(kts:kte,n)
           end do
           write(33,*) ' '
           do n = nlambda_start,nwave
             write(33,*) rad_fld_tpose(kts:kte,n)
           end do
           close( 33 )
         endif
#endif

rate_loop: &
         do n = 1,nj
!---------------------------------------------------------------------
! set cross-section x quantum yields
!---------------------------------------------------------------------
           if( n /= j_o2_ndx ) then
             if( .not. is_full_tuv ) then
               if( xsqy_is_zdep(n) ) then
                 call xsqy_int( n, xsqy, tlev(kts:kte), dens_air(kts:kte) )
               endif
             else
               jndx = rxn_ndx(n)
               if( jndx /= -1 ) then
                 if( xsqy_table(jndx)%tpflag /= 0 ) then
                   call the_subs(jndx)%xsqy_sub(nwave+1,wl,wc,nlev,tlev,dens_air,jndx)
                 endif
               endif
             endif
           elseif( .not. is_full_tuv ) then
             call sjo2( kte, nwave, srb_o2_xs, xsqy )
           endif
!---------------------------------------------------------------------
! compute tuv photorates
!---------------------------------------------------------------------
           if( .not. is_full_tuv ) then
             if( xsqy_is_zdep(n) ) then
               do k = kts,kte
                 xsect(nlambda_start:nwave) = xsqy(nlambda_start:nwave,k)*w_fac(nlambda_start:nwave)*esfact
                 tuv_prate(k,n) = m2s*dot_product( rad_fld(nlambda_start:nwave,k),xsect(nlambda_start:nwave) )
               end do
             else
               xsect(nlambda_start:nwave) = xsqy_tab(nlambda_start:nwave,1,1,n)*w_fac(nlambda_start:nwave)*esfact
               tuv_prate(:,n) = m2s*matmul( rad_fld_tpose(:,nlambda_start:nwave),xsect(nlambda_start:nwave) )
             endif
           else
             if( n /= j_o2_ndx ) then
               if( xsqy_table(jndx)%tpflag > 0 ) then
                 do k = kts,kte
                   xsect(nlambda_start:nwave) = xsqy_table(jndx)%sq(k+1,nlambda_start:nwave)*w_fac(nlambda_start:nwave)*esfact
                   tuv_prate(k,n) = m2s*dot_product( rad_fld(nlambda_start:nwave,k),xsect(nlambda_start:nwave) )
                 end do
               else
                 xsect(nlambda_start:nwave) = xsqy_table(jndx)%sq(nlambda_start:nwave,1)*w_fac(nlambda_start:nwave)*esfact
                 tuv_prate(:,n) = m2s*matmul( rad_fld_tpose(:,nlambda_start:nwave),xsect(nlambda_start:nwave) )
               endif
             else
               do k = kts,kte
                 xsect(nlambda_start:nwave) = srb_o2_xs(nlambda_start:nwave,k)*w_fac(nlambda_start:nwave)*esfact
                 tuv_prate(k,n) = m2s*dot_product( rad_fld(nlambda_start:nwave,k),xsect(nlambda_start:nwave) )
               end do
             endif
           endif
         end do rate_loop

#ifdef SW_DEBUG
         if( myproc == 6 ) then
           if( tuv_diags ) then
             open(unit=33,file='WRF-TUV.j.out' )
             do n = 1,nj
               select case( n )
                 case( 2:4,7:10,12:13 )
                   do k = kts,kte,5
                     write(33,'(1p,5g15.7)') tuv_prate(k:min(k+4,kte),n)/m2s
                   end do
                   if( n < 13 ) then
                     write(33,*) ' '
                   endif
               end select
             end do
             close(33)
             call wrf_abort
           endif
         endif
#endif
       endif has_daylight
#if ( WRF_KPP == 1 )
#include "tuv2wrf_jvals.inc"
#endif

     end do long_loop
   end do lat_loop

#ifdef SW_DEBUG
   if( any( zen_angle(its:ite,jts:jte) < 90. ) ) then
     min_ndx(:) = minloc( z_top,mask=z_top>0. )
     min_ndx(:) = (/ min_ndx(1) + its - 1,min_ndx(2) + jts - 1 /)
     max_ndx(:) = maxloc( z_top,mask=z_top>0. )
     max_ndx(:) = (/ max_ndx(1) + its - 1,max_ndx(2) + jts - 1 /)
     write(msg,'(''tuv_driver: z_top min indices = '',2i6)') min_ndx(:)
     call wrf_debug( 0,trim(msg) )
     write(msg,'(''tuv_driver: z_top max indices = '',2i6)') max_ndx(:)
     call wrf_debug( 0,trim(msg) )
     write(msg,'(''tuv_driver: z_top min,max = '',1p2g15.7)') z_top(min_ndx(1),min_ndx(2)),z_top(max_ndx(1),max_ndx(2))
     call wrf_debug( 0,trim(msg) )
   endif
#endif

   call tuv_deallocate

   ierr = 0
   if( allocated( rad_fld ) ) then
     deallocate( rad_fld,stat=astat )
     ierr = ierr + astat
     deallocate( dir_fld, dwn_fld, up_fld,stat=astat )
     ierr = ierr + astat
     deallocate( e_dir, e_dn, e_up,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( rad_fld_tpose ) ) then
     deallocate( rad_fld_tpose,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( e_fld ) ) then
     deallocate( e_fld,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( xsqy ) ) then
     deallocate( xsqy,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( srb_o2_xs ) ) then
     deallocate( srb_o2_xs,stat=astat )
     ierr = ierr + astat
   endif

   if( allocated( tuv_prate ) ) then
     deallocate( tuv_prate,stat=astat )
     ierr = ierr + astat
   endif

   if( ierr /= 0 ) then
     call wrf_error_fatal( 'tuv_driver: failed to deallocate local variables' )
   endif

   CONTAINS

   subroutine setzgrid( ztop, nexo, zexo_grd )
!---------------------------------------------------------------------
!  set the vertical grid above model top up to 50 km for photorate
!  calculation
!---------------------------------------------------------------------

   integer, intent(out) :: nexo
   real, intent(in)     :: ztop
   real, intent(out)    :: zexo_grd(:)

   real, parameter :: ztrop  = 25.
   real, parameter :: dztrop = 1.
   real, parameter :: zlid   = 50.
   real, parameter :: dzlid  = 5.

   real    :: z
   real    :: zBase

   nexo = 0
   if( ztop >= zlid ) then
     return
   elseif( ztop <= ztrop ) then
     zBase = ztrop
   else
     zBase = dzlid*real( int(ztop/dzlid) )
   endif

   z = int( ztop )
   do while( z < ztrop )
     z = z + dztrop
     nexo = nexo + 1
     zexo_grd(nexo) = z
   enddo
     
   z = zBase
   do while( z < zlid )
     z = z + dzlid
     nexo = nexo + 1
     zexo_grd(nexo) = z
   end do 

   end subroutine setzgrid

   subroutine tuv_allocate
!---------------------------------------------------------------------
! allocate column variables
!---------------------------------------------------------------------

   allocate( tuv_z(ktsm1:n_tuv_z),dtuv_z(ktsm1:n_tuv_z-1),stat=ierr )
   allocate( cldfrac(ktsm1:n_tuv_z),stat=astat )
   ierr = ierr + astat
   allocate( qll(ktsm1:n_tuv_z),stat=astat )
   ierr = ierr + astat
   allocate( tlev(ktsm1:n_tuv_z),tlyr(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( dens_air(ktsm1:n_tuv_z),stat=astat )
   ierr = ierr + astat
   allocate( o33(ktsm1:n_tuv_z),stat=astat )
   ierr = ierr + astat
   allocate( o3_xs(nwave,ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   if( is_full_tuv ) then
     allocate( o3_xs_tpose(ktsm1:n_tuv_z-1,nwave),stat=astat )
     ierr = ierr + astat
     allocate( no2_xs_tpose(ktsm1:n_tuv_z-1,nwave),stat=astat )
     ierr = ierr + astat
   endif
   allocate( no2_xs(nwave,ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( aircol(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( o2col(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( o3col(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( no2col(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( so2col(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( tauaer300(ktsm1:n_tuv_z-1),tauaer400(ktsm1:n_tuv_z-1), &
             tauaer600(ktsm1:n_tuv_z-1),tauaer999(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( waer300(ktsm1:n_tuv_z-1),waer400(ktsm1:n_tuv_z-1), &
             waer600(ktsm1:n_tuv_z-1),waer999(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( gaer300(ktsm1:n_tuv_z-1),gaer400(ktsm1:n_tuv_z-1), &
             gaer600(ktsm1:n_tuv_z-1),gaer999(ktsm1:n_tuv_z-1),stat=astat )
   ierr = ierr + astat
   allocate( dtaer(ktsm1:n_tuv_z-1,nwave),omaer(ktsm1:n_tuv_z-1,nwave), &
             gaer(ktsm1:n_tuv_z-1,nwave),stat=astat )
   ierr = ierr + astat
   allocate( dtcld(ktsm1:n_tuv_z-1,nwave),omcld(ktsm1:n_tuv_z-1,nwave), &
             gcld(ktsm1:n_tuv_z-1,nwave),stat=astat )
   ierr = ierr + astat

   if( ierr /= 0 ) then
     call wrf_error_fatal( 'tuv_driver: failed to allocate column variables' )
   endif

   end subroutine tuv_allocate

   subroutine tuv_deallocate
!---------------------------------------------------------------------
! deallocate column variables
!---------------------------------------------------------------------

   integer :: astat, ierr

   ierr = 0
   if( allocated( tuv_z ) ) then
     deallocate( tuv_z,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( dtuv_z ) ) then
     deallocate( dtuv_z,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( cldfrac ) ) then
     deallocate( cldfrac,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( qll ) ) then
     deallocate( qll,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tlev ) ) then
     deallocate( tlev,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tlyr ) ) then
     deallocate( tlyr,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( dens_air ) ) then
     deallocate( dens_air,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( o33 ) ) then
     deallocate( o33,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( o3_xs ) ) then
     deallocate( o3_xs,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( o3_xs_tpose ) ) then
     deallocate( o3_xs_tpose,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( no2_xs ) ) then
     deallocate( no2_xs,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( no2_xs_tpose ) ) then
     deallocate( no2_xs_tpose,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( aircol ) ) then
     deallocate( aircol,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( o2col ) ) then
     deallocate( o2col,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( o3col ) ) then
     deallocate( o3col,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( no2col ) ) then
     deallocate( no2col,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( so2col ) ) then
     deallocate( so2col,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tauaer300 ) ) then
     deallocate( tauaer300, waer300, gaer300,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tauaer400 ) ) then
     deallocate( tauaer400, waer400, gaer400,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tauaer600 ) ) then
     deallocate( tauaer600, waer600, gaer600,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( tauaer999 ) ) then
     deallocate( tauaer999, waer999, gaer999,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( dtcld ) ) then
     deallocate( dtcld, omcld, gcld,stat=astat )
     ierr = ierr + astat
   endif
   if( allocated( dtaer ) ) then
     deallocate( dtaer, omaer, gaer,stat=astat )
     ierr = ierr + astat
   endif

   if( ierr /= 0 ) then
     call wrf_error_fatal( 'tuv_deallocate: failed to deallocate all variables' )
   endif

   end subroutine tuv_deallocate

   end subroutine tuv_driver

   subroutine tuv_init(  &
            domain, config_flags, z_at_w, aerwrf, g, &
            af_lambda_start, af_lambda_end, start_lambda, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte )

      USE module_configure
      USE params_mod, only : lambda_cutoff
      USE srb, only : init_srb
      USE module_state_description, only : p_so2, p_no2, param_first_scalar
      USE module_rxn, only : rxn_init, xsqy_table => xsqy_tab, npht_tab
      USE module_rxn, only : get_initialization

        ! .. Scalar Arguments ..
      INTEGER, INTENT (IN) :: domain
      INTEGER, INTENT (IN) :: ide, ids, ime, ims, ite, its, jde, jds, &
                              jme, jms, jte, jts, kde, kds, kme, kms, kte, kts
      REAL, INTENT (IN)    :: g
      REAL, INTENT (IN)    :: af_lambda_start, af_lambda_end, start_lambda
        ! .. Array Arguments ..
      REAL, INTENT (INOUT) :: aerwrf(ims:ime,kms:kme,jms:jme)
      REAL, INTENT (IN)    :: z_at_w(ims:ime,kms:kme,jms:jme)

      TYPE(grid_config_rec_type),  INTENT(IN) :: config_flags

        ! .. Local Scalars ..
      INTEGER :: astat
      INTEGER :: i, j, k, n, wn
        ! .. Local Arrays ..
      CHARACTER(len=256) :: msg

#ifndef NETCDF
      call wrf_error_fatal( 'tuv_init: requires netcdf' )
#endif

      DO j = jts, min(jte,jde-1)
        DO k = kts, min(kte,kde-1)
          DO i = its, min(ite,ide-1)
            aerwrf(i,k,j) = 0.
          END DO
        END DO
      END DO


is_initialized: &
      if( .not. tuv_is_initialized ) then
        has_exo_coldens = config_flags%has_o3_exo_coldens .or. config_flags%scale_o3_to_grnd_exo_coldens
        is_full_tuv = config_flags%is_full_tuv
#if ( WRF_KPP == 1 )
#include "tuvdef_jvals.inc"
#endif
        call get_xsqy_tab
        if( .not. is_full_tuv ) then
          allocate( xsqy_is_zdep(nj),stat=astat )
          if( astat /= 0 ) then
            call wrf_error_fatal( 'tuv_init: failed to allocate xsqy_is_zdep' )
          endif
          xsqy_is_zdep(:) = .false.
          if( j_o2_ndx > 0 ) then
            xsqy_is_zdep(j_o2_ndx) = .true.
          endif
          do n = 1,nj
            if( n /= j_o2_ndx ) then
t_loop :      do j = 1,nconc
                do i = 1,ntemp
                  if( any( xsqy_tab(:,i,j,n) /= xsqy_tab(:,1,1,n) ) ) then
                    xsqy_is_zdep(n) = .true.
                    exit t_loop
                  endif
                end do
              end do t_loop
            endif
          end do
        endif
        has_so2 = p_so2 >= param_first_scalar
        has_no2 = p_no2 >= param_first_scalar
        call init_srb
!---------------------------------------------------------------------
!     ... locate starting wave bin
!---------------------------------------------------------------------
        lambda_cutoff = start_lambda
        do nlambda_start = 1,nwave
          if( wc(nlambda_start) >= lambda_cutoff ) then
            exit
          endif
        end do
        if( nlambda_start > nwave ) then
          write(msg,'(''tuv_init: '',1pg15.7,'' is not in photo wavelength interval ('',g15.7,'','',g15.7,'')'')') &
                      lambda_cutoff,wl(1),wl(nwave+1)
          call wrf_error_fatal( trim(msg) )
        endif
!---------------------------------------------------------------------
!     ... locate af output wave bins
!---------------------------------------------------------------------
        do nlambda_af_start = nlambda_start,nwave
          if( wl(nlambda_af_start)   <= af_lambda_start .and. &
              wl(nlambda_af_start+1) >= af_lambda_start ) then
            exit
          endif
        end do
        do nlambda_af_end = nlambda_start,nwave
          if( wl(nlambda_af_end)   <= af_lambda_end .and. &
              wl(nlambda_af_end+1) >= af_lambda_end ) then
            exit
          endif
        end do
        allocate( par_wght(nwave), ery_wght(nwave), stat=astat )
        if( astat /= 0 ) then
          call wrf_error_fatal( 'tuv_init: failed to allocate par_wght,ery_wght' )
        endif
!---------------------------------------------------------------------
!     ... set PAR weight
!---------------------------------------------------------------------
        where (wc(:) > 400. .AND. wc(:) < 700.)
          par_wght(:) = 8.36e-3 * wc(:)
        elsewhere
          par_wght(:) = 0.
        end where
!---------------------------------------------------------------------
!     ... set erythema weight
!---------------------------------------------------------------------
        call fery( nwave, wc, ery_wght )
!---------------------------------------------------------------------
!     ... set surface albedo
!---------------------------------------------------------------------
        DO wn = 1,nwave
          IF (wl(wn)<400.) then
            albedo(wn) = 0.05
          elseIF (wl(wn)>=400. .AND. wl(wn)<450.) then
            albedo(wn) = 0.06
          elseIF (wl(wn)>=450. .AND. wl(wn)<500.) then
            albedo(wn) = 0.08
          elseIF (wl(wn)>=500. .AND. wl(wn)<550.) then
            albedo(wn) = 0.10
          elseIF (wl(wn)>=550. .AND. wl(wn)<600.) then
            albedo(wn) = 0.11
          elseIF (wl(wn)>=600. .AND. wl(wn)<640.)  then
            albedo(wn) = 0.12
          elseIF (wl(wn)>=640. .AND. wl(wn)<660.) then
            albedo(wn) = 0.135
          elseIF (wl(wn)>=660.) then
            albedo(wn) = 0.15
          endIF
        END DO
!---------------------------------------------------------------------
!     ... if full TUV then initialize
!---------------------------------------------------------------------
        if( is_full_tuv ) then
          call rxn_init( nwave+1,wl )
          allocate( rxn_ndx(nj),stat=astat )
          if( astat /= 0 ) then
            call wrf_error_fatal( 'tuv_init: failed to allocate rxn_ndx' )
          endif
          rxn_ndx(1:nj) = -1
          do j = 1,nj
            if( j /= j_o2_ndx ) then
              do n = 2,npht_tab
                if( trim(xsqy_table(n)%wrf_label) == trim(tuv_jname(j)) ) then
                  rxn_ndx(j) = n
                  exit
                endif
              enddo
            endif
          enddo
          rxn_initialized = .not. get_initialization()
        endif
        tuv_is_initialized = .true.
      endif is_initialized

!---------------------------------------------------------------------
!     ... get exo model column densities
!---------------------------------------------------------------------
      if( has_exo_coldens ) then
        call get_exo_coldens( domain, config_flags%exo_coldens_inname, &
             ids,ide, jds,jde, kds,kde, &
             ims,ime, jms,jme, kms,kme, &
             its,ite, jts,jte, kts,kte )
      endif

      end subroutine tuv_init

      subroutine get_xsqy_tab
!---------------------------------------------------------------------
!	... read in the cross section,quantum yield tables
!---------------------------------------------------------------------
      
      use params_mod, only : hc
      use srb,        only : ila, isrb
      use srb,        only : nchebev_term, nchebev_wave
      use srb,        only : chebev_ac, chebev_bc

!---------------------------------------------------------------------
!	... local variables
!---------------------------------------------------------------------
      integer :: astat, ierr
      integer :: m
      integer :: ncid, dimid, varid
      character(len=132) :: filename
      character(len=132) :: err_msg
      character(len=64)  :: varname

#ifdef NETCDF
include 'netcdf.inc'

!---------------------------------------------------------------------
!	... function declarations
!---------------------------------------------------------------------
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor

master_proc_a: &
      if( wrf_dm_on_monitor() ) then
        filename = 'wrf_tuv_xsqy.nc' 
        err_msg = 'get_xsqy_tab: failed to open file ' // trim(filename)
        call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
!---------------------------------------------------------------------
!	... get dimensions
!---------------------------------------------------------------------
        err_msg = 'get_xsqy_tab: failed to get nwave id'
        call handle_ncerr( nf_inq_dimid( ncid, 'nwave', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get nwave'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, nwave ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get ntemp id'
        call handle_ncerr( nf_inq_dimid( ncid, 'ntemp', dimid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get ntemp'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, ntemp ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get nconc id'
        call handle_ncerr( nf_inq_dimid( ncid, 'nconc', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get nconc'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, nconc ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get nchebev_term id'
        call handle_ncerr( nf_inq_dimid( ncid, 'nchebev_term', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get nchebev'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, nchebev_term ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get nchebev_wave id'
        call handle_ncerr( nf_inq_dimid( ncid, 'nchebev_wave', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get nchebev'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, nchebev_wave ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get n_temp_data id'
        call handle_ncerr( nf_inq_dimid( ncid, 'n_temp_data', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get n_temp_data'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, n_temp_data ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get n_o3_data id'
        call handle_ncerr( nf_inq_dimid( ncid, 'n_o3_data', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get n_o3_data'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, n_o3_data ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get n_air_dens_data id'
        call handle_ncerr( nf_inq_dimid( ncid, 'n_air_dens_data', dimid ), trim(err_msg) ) 
        err_msg = 'get_xsqy_tab: failed to get n_air_dens_data'
        call handle_ncerr( nf_inq_dimlen( ncid, dimid, n_air_dens_data ), trim(err_msg) )
      endif master_proc_a
#ifdef DM_PARALLEL
!---------------------------------------------------------------------
!	... bcast the dimensions
!---------------------------------------------------------------------
      CALL wrf_dm_bcast_bytes ( nwave, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( ntemp, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( nconc, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( n_temp_data, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( n_o3_data, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( n_air_dens_data, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( nchebev_term, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( nchebev_wave, IWORDSIZE )
#endif
!---------------------------------------------------------------------
!	... allocate module arrays
!---------------------------------------------------------------------
      ierr = 0
      allocate( z_temp_data(n_temp_data), z_o3_data(n_o3_data), &
                z_air_dens_data(n_air_dens_data),stat=astat )
      ierr = astat + ierr
      allocate( temp_data(n_temp_data), o3_data(n_o3_data), &
                air_dens_data(n_air_dens_data),stat=astat )
      ierr = astat + ierr
      allocate( wl(nwave+1), wc(nwave), dw(nwave), w_fac(nwave), &
                etfl(nwave), albedo(nwave), stat=astat )
      ierr = astat + ierr
      if( .not. is_full_tuv ) then
        allocate( temp_tab(ntemp), conc_tab(nconc), stat=astat )
        ierr = astat + ierr
        allocate( del_temp_tab(ntemp-1), del_conc_tab(nconc-1), stat=astat )
        ierr = astat + ierr
      endif
      allocate( chebev_ac(nchebev_term,nchebev_wave), stat=astat )
      ierr = astat + ierr
      allocate( chebev_bc(nchebev_term,nchebev_wave), stat=astat )
      ierr = astat + ierr
      allocate( o2_xs(nwave), so2_xs(nwave), stat=astat )
      ierr = astat + ierr
      allocate( o3_xs_tab(nwave,ntemp), no2_xs_tab(nwave,ntemp), stat=astat )
      ierr = astat + ierr
      if( .not. is_full_tuv ) then
        allocate( xsqy_tab(nwave,ntemp,nconc,nj), stat=astat )
        ierr = astat + ierr
      endif
      if( ierr /= 0 ) then
        call wrf_error_fatal( 'tuv_init: failed to allocate z_temp_data ...  xsqy_tab' )
      endif
!---------------------------------------------------------------------
!	... read arrays
!---------------------------------------------------------------------
master_proc_b: &      
      if( wrf_dm_on_monitor() ) then
        err_msg = 'get_xsqy_tab: failed to get z_temp_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'z_temp_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read z_temp_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, z_temp_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get z_o3_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'z_o3_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read z_o3_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, z_o3_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get z_air_dens_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'z_air_dens_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read z_air_dens_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, z_air_dens_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get temp_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'temp_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read temp_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, temp_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get o3_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'o3_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read o3_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, o3_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get air_dens_data variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'air_dens_data', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read air_dens_data variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, air_dens_data ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get wl variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'wl', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read wl variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, wl ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get wc variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'wc', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read wc variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, wc ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get etfl variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'etf', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read etfl variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, etfl ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get chebev_ac variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'chebev_ac', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read chebev_ac variable'
        call handle_ncerr( nf_get_var_double( ncid, varid, chebev_ac ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to get chebev_bc variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'chebev_bc', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read chebev_bc variable'
        call handle_ncerr( nf_get_var_double( ncid, varid, chebev_bc ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get ila variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'ila', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read ila variable'
        call handle_ncerr( nf_get_var_int( ncid, varid, ila ), trim(err_msg) )

        err_msg = 'get_xsqy_tab: failed to get isrb variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'isrb', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read isrb variable'
        call handle_ncerr( nf_get_var_int( ncid, varid, isrb ), trim(err_msg) )

        if( .not. is_full_tuv ) then
          err_msg = 'get_xsqy_tab: failed to temp_tab variable id'
          call handle_ncerr( nf_inq_varid( ncid, 'temps', varid ), trim(err_msg) )
          err_msg = 'get_xsqy_tab: failed to read temp_tab variable'
          call handle_ncerr( nf_get_var_real( ncid, varid, temp_tab ), trim(err_msg) )
          err_msg = 'get_xsqy_tab: failed to conc_tab variable id'
          call handle_ncerr( nf_inq_varid( ncid, 'concs', varid ), trim(err_msg) )
          err_msg = 'get_xsqy_tab: failed to read conc_tab variable'
          call handle_ncerr( nf_get_var_real( ncid, varid, conc_tab ), trim(err_msg) )
        endif
        err_msg = 'get_xsqy_tab: failed to o2_xs variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'o2_xs', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read o2_xs variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, o2_xs ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to so2_xs variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'so2_xs', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read so2_xs variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, so2_xs ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to o3_xs_tab variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'o3_xs', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read o3_xs_tab variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, o3_xs_tab ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to no2_xs_tab variable id'
        call handle_ncerr( nf_inq_varid( ncid, 'no2_xs', varid ), trim(err_msg) )
        err_msg = 'get_xsqy_tab: failed to read no2_xs_tab variable'
        call handle_ncerr( nf_get_var_real( ncid, varid, no2_xs_tab ), trim(err_msg) )
        if( .not. is_full_tuv ) then
          do m = 1,nj
            varname = trim(tuv_jname(m)) // '_xsqy'
            err_msg = 'get_xsqy_tab: failed to ' // trim(varname) //' variable id'
            call handle_ncerr( nf_inq_varid( ncid, trim(varname), varid ), trim(err_msg) )
            err_msg = 'get_xsqy_tab: failed to read ' // trim(varname) // ' variable'
            call handle_ncerr( nf_get_var_real( ncid, varid, xsqy_tab(:,:,:,m) ), trim(err_msg) )
          end do
        endif
      endif master_proc_b

#ifdef DM_PARALLEL
!---------------------------------------------------------------------
!	... bcast the arrays
!---------------------------------------------------------------------
      CALL wrf_dm_bcast_bytes ( ila, IWORDSIZE )
      CALL wrf_dm_bcast_bytes ( isrb, IWORDSIZE )
      CALL wrf_dm_bcast_bytes( wl, (nwave+1)*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( wc, nwave*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( etfl, nwave*RWORDSIZE )
      if( .not. is_full_tuv ) then
        CALL wrf_dm_bcast_bytes( temp_tab, ntemp*RWORDSIZE )
        CALL wrf_dm_bcast_bytes( conc_tab, nconc*RWORDSIZE )
      endif
      CALL wrf_dm_bcast_bytes( o2_xs, nwave*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( so2_xs, nwave*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( z_temp_data, n_temp_data*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( z_o3_data, n_o3_data*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( z_air_dens_data, n_air_dens_data*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( temp_data, n_temp_data*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( o3_data, n_o3_data*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( air_dens_data, n_air_dens_data*RWORDSIZE )
#if RWORDSIZE == 4
      CALL wrf_dm_bcast_bytes( chebev_ac, nchebev_term*nchebev_wave*2*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( chebev_bc, nchebev_term*nchebev_wave*2*RWORDSIZE )
#endif
#if RWORDSIZE == 8
      CALL wrf_dm_bcast_bytes( chebev_ac, nchebev_term*nchebev_wave*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( chebev_bc, nchebev_term*nchebev_wave*RWORDSIZE )
#endif
      CALL wrf_dm_bcast_bytes( o3_xs_tab, nwave*ntemp*RWORDSIZE )
      CALL wrf_dm_bcast_bytes( no2_xs_tab, nwave*ntemp*RWORDSIZE )
      if( .not. is_full_tuv ) then
        CALL wrf_dm_bcast_bytes( xsqy_tab, nwave*ntemp*nconc*nj*RWORDSIZE )
      endif
#endif

      if( .not. is_full_tuv ) then
        del_temp_tab(:ntemp-1) = 1./(temp_tab(2:ntemp) - temp_tab(1:ntemp-1))
        del_conc_tab(:nconc-1) = 1./(conc_tab(2:nconc) - conc_tab(1:nconc-1))
      endif
      dw(:nwave)    = wl(2:nwave+1) - wl(1:nwave)
      w_fac(:nwave) = dw(:nwave)*etfl(:nwave)*1.e-13*wc(:nwave)/hc

      if( wrf_dm_on_monitor() ) then
        err_msg = 'get_xsqy_tab: failed to close file ' // trim(filename)
        call handle_ncerr( nf_close( ncid ),trim(err_msg) )
      endif
#endif

      end subroutine get_xsqy_tab

      subroutine get_exo_coldens( dm, exo_coldens_filename, &
            ids,ide, jds,jde, kds,kde, &
            ims,ime, jms,jme, kms,kme, &
            its,ite, jts,jte, kts,kte )
!---------------------------------------------------------------------
!	... read in the exo column o2,o3 densities
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in)          :: dm
      integer, intent(in)  :: ide, ids, ime, ims, ite, its, jde, jds, &
                              jme, jms, jte, jts, kde, kds, kme, kms, kte, kts
      character(len=*), intent(in) :: exo_coldens_filename

!---------------------------------------------------------------------
!	... local variables
!---------------------------------------------------------------------
      INTEGER :: i, j, k
      integer :: astat
      integer :: ncid
      integer :: dimid
      integer :: varid
      integer :: max_dom
      integer :: cpos
      integer :: iend, jend
      integer :: lon_e, lat_e
      integer :: ncoldens_levs
      integer :: ndays_of_year
!     real, allocatable :: coldens(:,:,:,:)
      character(len=128) :: err_msg
      character(len=64)  :: filename
      character(len=2)   :: id_num

!---------------------------------------------------------------------
!	... function declarations
!---------------------------------------------------------------------
      LOGICAL, EXTERNAL  :: wrf_dm_on_monitor

#ifdef NETCDF
include 'netcdf.inc'
#define DM_BCAST_MACRO(A) CALL wrf_dm_bcast_bytes ( A , size ( A ) * DWORDSIZE )

!---------------------------------------------------------------------
!	... allocate column_density type
!---------------------------------------------------------------------
      if( .not. allocated(col_dens) ) then
        CALL nl_get_max_dom( 1,max_dom )
        allocate( col_dens(max_dom),stat=astat )
        if( astat /= 0 ) then
          call wrf_error_fatal( 'get_exo_coldens: failed to allocate col_dens' )
        endif
        write(err_msg,'(''get_exo_coldens: intializing '',i2,'' domains'')') max_dom
        call wrf_message( trim(err_msg) )
        col_dens(:)%is_allocated = .false.
      endif
!---------------------------------------------------------------------
!	... open column density netcdf file
!---------------------------------------------------------------------
col_dens_allocated : &
        if( .not. col_dens(dm)%is_allocated ) then
!         if( wrf_dm_on_monitor() ) then
            cpos = index( exo_coldens_filename, '<domain>' )
            if( cpos > 0 ) then
              write(id_num,'(i2.2)') dm
              filename = exo_coldens_filename(:cpos-1) // 'd' // id_num
            else
              filename = trim( exo_coldens_filename )
            endif
            err_msg = 'get_exo_coldens: intializing domain ' // id_num
            call wrf_message( trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to open file ' // trim(filename)
            call handle_ncerr( nf_open( trim(filename), nf_noclobber, ncid ), trim(err_msg) )
!---------------------------------------------------------------------
!	... get dimensions
!---------------------------------------------------------------------
             err_msg = 'get_exo_coldens: failed to get col_dens levels id'
             call handle_ncerr( nf_inq_dimid( ncid, 'coldens_levs', dimid ), trim(err_msg) ) 
             err_msg = 'get_exo_coldens: failed to get col_dens levels'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, ncoldens_levs ), trim(err_msg) )
             err_msg = 'get_exo_coldens: failed to get number of days in year id'
             call handle_ncerr( nf_inq_dimid( ncid, 'ndays_of_year', dimid ), trim(err_msg) )
             err_msg = 'get_exo_coldens: failed to get number of days in year'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, ndays_of_year ), trim(err_msg) )
             err_msg = 'get_exo_coldens: failed to get west_east id'
             call handle_ncerr( nf_inq_dimid( ncid, 'west_east', dimid ), trim(err_msg) ) 
             err_msg = 'get_exo_coldens: failed to get west_east'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, lon_e ), trim(err_msg) )
             err_msg = 'get_exo_coldens: failed to get south_north id'
             call handle_ncerr( nf_inq_dimid( ncid, 'south_north', dimid ), trim(err_msg) ) 
             err_msg = 'get_exo_coldens: failed to get south_north'
             call handle_ncerr( nf_inq_dimlen( ncid, dimid, lat_e ), trim(err_msg) )
!         end IF
#ifdef DM_PARALLEL
!---------------------------------------------------------------------
!	... bcast the dimensions
!---------------------------------------------------------------------
!         CALL wrf_dm_bcast_bytes ( ncoldens_levs , IWORDSIZE )
!         CALL wrf_dm_bcast_bytes ( ndays_of_year , IWORDSIZE )
!         CALL wrf_dm_bcast_bytes ( lon_e , IWORDSIZE )
!         CALL wrf_dm_bcast_bytes ( lat_e , IWORDSIZE )
#endif
!---------------------------------------------------------------------
!	... allocate local arrays
!---------------------------------------------------------------------
          iend = min( ite,ide-1 )
          jend = min( jte,jde-1 )
!         allocate( coldens(lon_e,lat_e,ncoldens_levs,ndays_of_year), stat=astat )
!         if( astat /= 0 ) then
!           call wrf_error_fatal( 'get_exo_coldens: failed to allocate coldens' )
!         end if
!---------------------------------------------------------------------
!	... allocate column_density type component arrays
!---------------------------------------------------------------------
          col_dens(dm)%ncoldens_levs = ncoldens_levs
          col_dens(dm)%ndays_of_year = ndays_of_year
          allocate( col_dens(dm)%col_levs(ncoldens_levs), &
                    col_dens(dm)%day_of_year(ndays_of_year), stat=astat )
          if( astat /= 0 ) then
            call wrf_error_fatal( 'get_exo_coldens: failed to allocate col_levs,day_of_year' )
          end if
          allocate( col_dens(dm)%o3_col_dens(its:iend,jts:jend,ncoldens_levs,ndays_of_year), &
                    col_dens(dm)%o2_col_dens(its:iend,jts:jend,ncoldens_levs,ndays_of_year), stat=astat )
          if( astat /= 0 ) then
            call wrf_error_fatal( 'get_exo_coldens: failed to allocate o3_col_dens,o2_col_dens' )
          end if
          col_dens(dm)%is_allocated = .true.
!---------------------------------------------------------------------
!	... read arrays
!---------------------------------------------------------------------
!         IF ( wrf_dm_on_monitor() ) THEN
            err_msg = 'get_exo_coldens: failed to get col_levs variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'coldens_levs', varid ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to read col_levs variable'
            call handle_ncerr( nf_get_var_double( ncid, varid, col_dens(dm)%col_levs ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to get days_of_year variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'days_of_year', varid ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to read days_of_year variable'
            call handle_ncerr( nf_get_var_double( ncid, varid, col_dens(dm)%day_of_year ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to get o3 col_dens variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'o3_column_density', varid ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to read o3 col_dens variable'
!           call handle_ncerr( nf_get_var_real( ncid, varid, coldens ), trim(err_msg) )
            call handle_ncerr( nf_get_vara_double( ncid, varid, (/its,jts,1,1/), &
                               (/iend-its+1,jend-jts+1,ncoldens_levs,ndays_of_year/), &
                               col_dens(dm)%o3_col_dens(its:iend,jts:jend,1:ncoldens_levs,1:ndays_of_year) ), trim(err_msg) )
!         ENDIF

#ifdef DM_PARALLEL
!         call wrf_debug( 0,'get_exo_coldens: bcast col_levs' )
!         DM_BCAST_MACRO(col_dens(dm)%col_levs)
!         call wrf_debug( 0,'get_exo_coldens: bcast day_of_year' )
!         DM_BCAST_MACRO(col_dens(dm)%day_of_year)
!         call wrf_debug( 0,'get_exo_coldens: bcast o3_col_dens' )
!         CALL wrf_dm_bcast_bytes ( coldens, size(coldens)*RWORDSIZE )
#endif

!         col_dens(dm)%o3_col_dens(its:iend,jts:jend,:ncoldens_levs,:ndays_of_year) = &
!                       coldens(its:iend,jts:jend,:ncoldens_levs,:ndays_of_year)

!         IF ( wrf_dm_on_monitor() ) THEN
            err_msg = 'get_exo_coldens: failed to get o2 col_dens variable id'
            call handle_ncerr( nf_inq_varid( ncid, 'o2_column_density', varid ), trim(err_msg) )
            err_msg = 'get_exo_coldens: failed to read o2 col_dens variable'
!           call handle_ncerr( nf_get_var_real( ncid, varid, coldens ), trim(err_msg) )
            call handle_ncerr( nf_get_vara_double( ncid, varid, (/its,jts,1,1/), &
                               (/iend-its+1,jend-jts+1,ncoldens_levs,ndays_of_year/), &
                               col_dens(dm)%o2_col_dens(its:iend,jts:jend,1:ncoldens_levs,1:ndays_of_year) ), trim(err_msg) )
!---------------------------------------------------------------------
!	... close column density netcdf file
!---------------------------------------------------------------------
            err_msg = 'get_exo_coldens: failed to close file ' // trim(filename)
            call handle_ncerr( nf_close( ncid ), trim(err_msg) )
!         end if

#ifdef DM_PARALLEL
!         call wrf_debug( 0,'get_exo_coldens: bcast o2_col_dens' )
!         CALL wrf_dm_bcast_bytes ( coldens, size(coldens)*RWORDSIZE )
#endif

!         col_dens(dm)%o2_col_dens(its:iend,jts:jend,:ncoldens_levs,:ndays_of_year) = &
!                       coldens(its:iend,jts:jend,:ncoldens_levs,:ndays_of_year)

!         deallocate( coldens )

!---------------------------------------------------------------------
!	... some diagnostics
!---------------------------------------------------------------------
          call wrf_debug( 100,' ' )
          write(err_msg,'(''get_exo_coldens: coldens variables for domain '',i2)') dm
          call wrf_debug( 100,trim(err_msg) )
          call wrf_debug( 100,'get_exo_coldens: days_of_year' )
          do k = 1,ndays_of_year,5
            write(err_msg,'(1p,5g15.7)') col_dens(dm)%day_of_year(k:min(k+4,ndays_of_year))
            call wrf_debug( 100,trim(err_msg) )
          end do
          call wrf_debug( 100,'get_exo_coldens: coldens levels' )
          do k = 1,ncoldens_levs,5
            write(err_msg,'(1p,5g15.7)') col_dens(dm)%col_levs(k:min(k+4,ncoldens_levs))
            call wrf_debug( 100,trim(err_msg) )
          end do
          call wrf_debug( 100,' ' )
        endif col_dens_allocated
#endif

      end subroutine get_exo_coldens

      subroutine tuv_timestep_init( dm, julday )
!-----------------------------------------------------------------------------
!	... setup the exo column time interpolation
!-----------------------------------------------------------------------------

      implicit none

!-----------------------------------------------------------------------------
!	... dummy arguments
!-----------------------------------------------------------------------------
      integer, intent(in)  ::  dm             ! domain number
      integer, intent(in)  ::  julday         ! day of year at present time step

!-----------------------------------------------------------------------------
!	... local variables
!-----------------------------------------------------------------------------
      integer  :: m
      real(dp) :: calday

      calday = real( julday,kind=dp)
      if( calday < col_dens(dm)%day_of_year(1) ) then
        next = 1
        last = 12
        dels = (365._dp + calday - col_dens(dm)%day_of_year(12)) &
                / (365._dp + col_dens(dm)%day_of_year(1) - col_dens(dm)%day_of_year(12))
      else if( calday >= col_dens(dm)%day_of_year(12) ) then
        next = 1
        last = 12
        dels = (calday - col_dens(dm)%day_of_year(12)) &
                / (365. + col_dens(dm)%day_of_year(1) - col_dens(dm)%day_of_year(12))
      else
        do m = 11,1,-1
          if( calday >= col_dens(dm)%day_of_year(m) ) then
            exit
          end if
        end do
        last = m
        next = m + 1
        dels = (calday - col_dens(dm)%day_of_year(m)) / (col_dens(dm)%day_of_year(m+1) - col_dens(dm)%day_of_year(m))
      end if

      end subroutine tuv_timestep_init

      subroutine z_interp( z_tab, tab, ntab, z_out, out )

      integer, intent(in) :: ntab
      real, intent(in)    :: z_tab(ntab)
      real, intent(in)    :: tab(ntab)
      real, intent(in)    :: z_out(:)
      real, intent(out)   :: out(:)
!---------------------------------------------------------------
!     	... altitude interpolation
!---------------------------------------------------------------

      integer :: k, kt, ktm1, n
      real    :: delz

      n = size(out)
      do k = 1,n
        if( z_out(k) <= z_tab(1) ) then
          out(k) = z_tab(1)
        elseif( z_out(k) >= z_tab(ntab) ) then
          out(k) = z_tab(ntab)
        else
          do kt = 1,ntab
            if( z_tab(kt) >= z_out(k) ) then
              ktm1 = kt - 1
              delz = (z_out(k) - z_tab(ktm1))/(z_tab(kt) - z_tab(ktm1)) 
              out(k) = tab(ktm1) + delz*(tab(kt) - tab(ktm1))
              exit
            endif
          end do
        endif
      end do

      end subroutine z_interp

      subroutine p_interp( o2_exo_col, o3_exo_col, o3_exo_col_at_grnd, ptop, &
                           dm, its, ite, jts, jte )
!---------------------------------------------------------------
!     	... pressure interpolation for exo col density
!---------------------------------------------------------------

      implicit none

!---------------------------------------------------------------
!     	... dummy arguments
!---------------------------------------------------------------
      integer, intent(in)   :: dm
      integer, intent(in)   :: its, ite
      integer, intent(in)   :: jts, jte
      real(dp), intent(in)  :: ptop(its:ite,jts:jte)             ! pressure at photolysis top (hPa)
      real(dp), intent(out) :: o2_exo_col(its:ite,jts:jte)       ! exo model o2 column density (molecules/cm^2)
      real(dp), intent(out) :: o3_exo_col(its:ite,jts:jte)       ! exo model o3 column density (molecules/cm^2)
      real(dp), intent(out) :: o3_exo_col_at_grnd(its:ite,jts:jte) ! exo model o3 column density at grnd (molecules/cm^2)

!---------------------------------------------------------------
!     	... local variables
!---------------------------------------------------------------
      integer  :: i, j, k, ku, kl
      integer  :: Kgrnd
      real(dp) :: pinterp
      real(dp) :: delp
      real(dp) :: tint_vals(2)

      Kgrnd = col_dens(dm)%ncoldens_levs

lat_loop : &
      do j = jts,jte
long_loop : &
         do i = its,ite
            pinterp = ptop(i,j)
            if( pinterp < col_dens(dm)%col_levs(1) ) then
               ku = 1
               kl = 1
               delp = 0._dp
            else
               do ku = 2,col_dens(dm)%ncoldens_levs
                  if( pinterp <= col_dens(dm)%col_levs(ku) ) then
                     kl = ku - 1
                     delp = log( pinterp/col_dens(dm)%col_levs(kl) )/log( col_dens(dm)%col_levs(ku)/col_dens(dm)%col_levs(kl) )
                     exit
                  end if
               end do
            end if
            tint_vals(1) = col_dens(dm)%o2_col_dens(i,j,kl,last) &
                           + delp * (col_dens(dm)%o2_col_dens(i,j,ku,last) &
                                     - col_dens(dm)%o2_col_dens(i,j,kl,last))
            tint_vals(2) = col_dens(dm)%o2_col_dens(i,j,kl,next) &
                           + delp * (col_dens(dm)%o2_col_dens(i,j,ku,next) &
                                     - col_dens(dm)%o2_col_dens(i,j,kl,next))
            o2_exo_col(i,j) = tint_vals(1) + dels * (tint_vals(2) - tint_vals(1))
            tint_vals(1) = col_dens(dm)%o3_col_dens(i,j,kl,last) &
                           + delp * (col_dens(dm)%o3_col_dens(i,j,ku,last) &
                                     - col_dens(dm)%o3_col_dens(i,j,kl,last))
            tint_vals(2) = col_dens(dm)%o3_col_dens(i,j,kl,next) &
                           + delp * (col_dens(dm)%o3_col_dens(i,j,ku,next) &
                                     - col_dens(dm)%o3_col_dens(i,j,kl,next))
            o3_exo_col(i,j) = tint_vals(1) + dels * (tint_vals(2) - tint_vals(1))
            tint_vals(1) = col_dens(dm)%o3_col_dens(i,j,Kgrnd,last)
            tint_vals(2) = col_dens(dm)%o3_col_dens(i,j,Kgrnd,next)
            o3_exo_col_at_grnd(i,j) = tint_vals(1) + dels * (tint_vals(2) - tint_vals(1))
         end do long_loop
      end do lat_loop

      end subroutine p_interp

      subroutine xsqy_int( n, xsqy, tlev, dens_air )
!---------------------------------------------------------------------
!	... interpolate m,t tables for xs * qy
!---------------------------------------------------------------------

      integer, intent(in)  :: n 
      real,    intent(in)  :: tlev(:)
      real,    intent(in)  :: dens_air(:)
      real,    intent(out) :: xsqy(:,:)

      real, parameter :: m0 = 2.45e19
      integer :: tndx, mndx, tndxp1, mndxp1
      integer :: k, ku
      real    :: temp, dens
      real    :: w(4)
      real    :: del_t, del_d

      ku = size( tlev )
      do k = 1,ku
        temp = tlev(k)
        do tndx = 1,ntemp
          if( temp_tab(tndx) > temp ) then
            exit
          endif
        end do
        tndx = max( min( tndx,ntemp ) - 1,1 )
        tndxp1 = tndx + 1
        del_t = max( 0.,min( 1.,(temp - temp_tab(tndx))*del_temp_tab(tndx) ) )

!       dens = dens_air(k)
        dens = dens_air(k)/m0
        do mndx = 1,nconc
          if( conc_tab(mndx) > dens ) then
            exit
          endif
        end do
        mndx = max( min( mndx,nconc ) - 1,1 )
        mndxp1 = mndx + 1
        del_d = max( 0.,min( 1.,(dens - conc_tab(mndx))*del_conc_tab(mndx) ) )

        w(1) = (1. - del_t)*(1. - del_d)
        w(2) = del_t*(1. - del_d)
        w(3) = (1. - del_t)*del_d
        w(4) = del_t*del_d

        xsqy(1:nwave,k) = w(1)*xsqy_tab(1:nwave,tndx,mndx,n) &
                        + w(2)*xsqy_tab(1:nwave,tndxp1,mndx,n) &
                        + w(3)*xsqy_tab(1:nwave,tndx,mndxp1,n) &
                        + w(4)*xsqy_tab(1:nwave,tndxp1,mndxp1,n)
      end do

      end subroutine xsqy_int

      subroutine xs_int( xs, tlev, xs_tab )
!---------------------------------------------------------------------
!	... interpolate tables for xs
!---------------------------------------------------------------------

      real,    intent(in)  :: tlev(:)
      real,    intent(in)  :: xs_tab(:,:)
      real,    intent(out) :: xs(:,:)

      integer :: tndx, tndxp1
      integer :: k, ku
      real    :: temp
      real    :: w(2)
      real    :: del_t

      ku = size( tlev )
      do k = 1,ku
        temp = tlev(k)
        do tndx = 1,ntemp
          if( temp_tab(tndx) > temp ) then
            exit
          endif
        end do
        tndx = max( min( tndx,ntemp ) - 1,1 )
        tndxp1 = tndx + 1
        del_t = max( 0.,min( 1.,(temp - temp_tab(tndx))*del_temp_tab(tndx) ) )

        w(1) = (1. - del_t)
        w(2) = del_t

        xs(1:nwave,k) = w(1)*xs_tab(1:nwave,tndx) &
                      + w(2)*xs_tab(1:nwave,tndxp1)
      end do

      end subroutine xs_int

      FUNCTION chap(zeta)
        ! chapman function is used when the solar zenith angle exceeds
        ! 75 deg.
        ! interpolates between values given in, e.g., mccartney (1976).
        ! .. Scalar Arguments ..
        REAL, intent(in) :: zeta
        ! .. Local Scalars ..
        REAL    :: rm
        INTEGER :: i
        LOGICAL :: fnd
        ! .. Local Arrays ..
        REAL :: y(75:96)
        ! .. Function Return Value ..
        REAL :: chap
        ! .. Data Statements ..
        DATA (y(i),i=75,96) &
         /3.800, 4.055, 4.348, 4.687, 5.083, 5.551, 6.113, &
          6.799, 7.650, 8.732, 10.144, 12.051, 14.730, 18.686, 24.905, 35.466, &
          55.211, 96.753, 197., 485., 1476., 9999./

        fnd = .false.
        DO i = 75, 96
          rm = real(i)
          IF (zeta < rm) then
            chap = y(i) + (y(i+1) - y(i))*(zeta - (rm - 1.))
            fnd = .true.
            exit
          ENDIF
        END DO

        IF( .not. fnd ) then
          chap = y(96)
        ENDIF

      END FUNCTION chap

      SUBROUTINE fery( nwave, w, wght )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Calculate the action spectrum value for erythema at a given wavelength
!=  according to: McKinlay, A.F and B.L.Diffey, A reference action spectrum
!=  for ultraviolet induced erythema in human skin, CIE Journal, vol 6,
!=  pp 17-22, 1987.
!=  Value at 300 nm = 0.6486
!-----------------------------------------------------------------------------

      INTEGER, intent(in) :: nwave
      REAL, intent(in)    :: w(:) 
      REAL, intent(out)   :: wght(:) 

      WHERE( w(1:nwave) < 298. )
          wght(1:nwave) = 1.
      ELSEWHERE( w(1:nwave) >= 298. .AND. w(1:nwave) < 328. )
          wght(1:nwave) = 10.**(0.094*(298. - w(1:nwave)))
      ELSEWHERE( w(1:nwave) >= 328. .AND. w(1:nwave) < 400. )
          wght(1:nwave) = 10.**(0.015*(139. - w(1:nwave)))
      ELSEWHERE( w(1:nwave) >= 400. )
          wght(1:nwave) = 1.e-36
      ENDWHERE

      END SUBROUTINE fery

      SUBROUTINE cldfrac_binary( CLDFRA,QC,QI, QS, kts, kte )
!---------------------------------------------------------------------
! !DESCRIPTION:
! Compute cloud fraction from input ice, snow, and cloud water fields
! if provided.
!
! Whether QI or QC is active or not is determined from the indices of
! the fields into the 4D scalar arrays in WRF. These indices are
! P_QI and P_QC, respectively, and they are passed in to the routine
! to enable testing to see if QI and QC represent active fields in
! the moisture 4D scalar array carried by WRF.
!
! If a field is active its index will have a value greater than or
! equal to PARAM_FIRST_SCALAR, which is also an input argument to
! this routine.
!EOP
!---------------------------------------------------------------------

   IMPLICIT NONE

   INTEGER,  INTENT(IN) :: kts, kte

   REAL,  INTENT(OUT  ) :: CLDFRA(kts:kte)

   REAL, INTENT(IN)     ::    QI(kts:kte), &
                              QC(kts:kte), &
                              QS(kts:kte)
!----------------------------------------------------------------------
!  local variables
!----------------------------------------------------------------------
   REAL, parameter :: thresh = 1.e-9

   INTEGER :: j

   where( (qc(kts:kte) + qi(kts:kte) + qs(kts:kte)) > thresh )
     cldfra(kts:kte) = 1.
   elsewhere
     cldfra(kts:kte) = 0.
   endwhere

   END SUBROUTINE cldfrac_binary

   SUBROUTINE cldfrac_fractional( CLDFRA, QV, QC, QI, QS, &
                                  p_phy, t_phy,           &
                                  kts, kte )

!----------------------------------------------------------------------
!  dummy arguments
!----------------------------------------------------------------------
   INTEGER,  INTENT(IN) :: kts,kte

   REAL, INTENT(OUT)    ::    CLDFRA(kts:kte)

   REAL, INTENT(IN)     ::    QV(kts:kte), &
                              QI(kts:kte), &
                              QC(kts:kte), &
                              QS(kts:kte), &
                              t_phy(kts:kte), &
                              p_phy(kts:kte)


!----------------------------------------------------------------------
!  local variables
!----------------------------------------------------------------------
   REAL    , PARAMETER :: ALPHA0 = 100.
   REAL    , PARAMETER :: GAMMA = 0.49
   REAL    , PARAMETER :: QCLDMIN = 1.E-12
   REAL    , PARAMETER :: PEXP = 0.25
   REAL    , PARAMETER :: RHGRID =1.0
   REAL    , PARAMETER :: SVP1 = 0.61078
   REAL    , PARAMETER :: SVP2 = 17.2693882
   REAL    , PARAMETER :: SVPI2 = 21.8745584
   REAL    , PARAMETER :: SVP3 = 35.86
   REAL    , PARAMETER :: SVPI3 = 7.66
   REAL    , PARAMETER :: SVPT0 = 273.15
   REAL    , PARAMETER :: r_d = 287.
   REAL    , PARAMETER :: r_v = 461.6
   REAL    , PARAMETER :: ep_2 = r_d/r_v

   INTEGER :: i,j,k
   INTEGER :: imax, jmax, kmax
   REAL    :: RHUM, tc, esw, esi, weight, qvsw, qvsi, qvs_weight
   REAL    :: QCLD, DENOM, ARG, SUBSAT, wrk
   REAL    :: relhum_max, wrk_max


! !DESCRIPTION:
!----------------------------------------------------------------------
! Compute cloud fraction from input ice and cloud water fields
! if provided.
!
! Whether QI or QC is active or not is determined from the indices of
! the fields into the 4D scalar arrays in WRF. These indices are 
! P_QI and P_QC, respectively, and they are passed in to the routine
! to enable testing to see if QI and QC represent active fields in
! the moisture 4D scalar array carried by WRF.
! 
! If a field is active its index will have a value greater than or
! equal to PARAM_FIRST_SCALAR, which is also an input argument to 
! this routine.
!----------------------------------------------------------------------
!EOP


!-----------------------------------------------------------------------
!     COMPUTE GRID-SCALE CLOUD COVER FOR RADIATION
!     (modified by Ferrier, Feb '02)
!
!     Cloud fraction parameterization follows Randall, 1994
!     (see Hong et al., 1998)
!-----------------------------------------------------------------------
! Note: ep_2=287./461.6 Rd/Rv
! Note: R_D=287.

! Alternative calculation for critical RH for grid saturation
!     RHGRID=0.90+.08*((100.-DX)/95.)**.5

! Calculate saturation mixing ratio weighted according to the fractions of
! water and ice.
! Following:
! Murray, F.W. 1966. ``On the computation of Saturation Vapor Pressure''  J. Appl. Meteor.  6 p.204
!    es (in mb) = 6.1078 . exp[ a . (T-273.16)/ (T-b) ]
!
!       over ice        over water
! a =   21.8745584      17.2693882
! b =   7.66            35.86
!---------------------------------------------------------------------

    relhum_max = -100.
    wrk_max    = -10000.
    imax = 0; kmax = 0; jmax = 0

vert_loop: &
    DO k = kts,kte
!---------------------------------------------------------------------
!    Determine cloud fraction (modified from original algorithm)
!---------------------------------------------------------------------
      QCLD = QI(k) + QC(k) + QS(k)
has_cloud : &
      IF( QCLD >= QCLDMIN ) THEN
        tc   = t_phy(k) - SVPT0
        esw  = 1000.0 * SVP1 * EXP( SVP2  * tc / ( t_phy(k) - SVP3  ) )
        esi  = 1000.0 * SVP1 * EXP( SVPI2 * tc / ( t_phy(k) - SVPI3 ) )
        QVSW = EP_2 * esw / ( p_phy(k) - esw )
        QVSI = EP_2 * esi / ( p_phy(k) - esi )

        weight     = (QI(k) + QS(k)) / QCLD
        QVS_WEIGHT = (1. - weight)*QVSW + weight*QVSI
        RHUM       = QV(k)/QVS_WEIGHT              !--- Relative humidity
!---------------------------------------------------------------------
!    Assume zero cloud fraction if there is no cloud mixing ratio
!---------------------------------------------------------------------
        IF( RHUM >= RHGRID )THEN
!---------------------------------------------------------------------
!    Assume cloud fraction of unity if near saturation and the cloud
!    mixing ratio is at or above the minimum threshold
!---------------------------------------------------------------------
          CLDFRA(k) = 1.
        ELSE
!---------------------------------------------------------------------
!    Adaptation of original algorithm (Randall, 1994; Zhao, 1995)
!    modified based on assumed grid-scale saturation at RH=RHgrid.
!---------------------------------------------------------------------
          SUBSAT = MAX( 1.E-10,RHGRID*QVS_WEIGHT - QV(k) )
          DENOM  = SUBSAT**GAMMA
          ARG    = MAX( -6.9,-ALPHA0*QCLD/DENOM )    ! <-- EXP(-6.9)=.001
!---------------------------------------------------------------------
!   prevent negative values  (new)
!---------------------------------------------------------------------
          RHUM = MAX( 1.E-10, RHUM )
          wrk  = (RHUM/RHGRID)**PEXP*(1. - EXP( ARG ))
          IF( wrk >= .01 ) then
            CLDFRA(k) = wrk
          ENDIF
        ENDIF
      ENDIF has_cloud
    END DO vert_loop

   END SUBROUTINE cldfrac_fractional

#ifdef NETCDF
      subroutine handle_ncerr( ret, mes )
!---------------------------------------------------------------------
!	... netcdf error handling routine
!---------------------------------------------------------------------

      implicit none

!---------------------------------------------------------------------
!	... dummy arguments
!---------------------------------------------------------------------
      integer, intent(in) :: ret
      character(len=*), intent(in) :: mes

include 'netcdf.inc'

      if( ret /= nf_noerr ) then
         call wrf_message( trim(mes) )
         call wrf_message( trim(nf_strerror(ret)) )
         call wrf_abort
      end if

      end subroutine handle_ncerr
#endif

    end module module_phot_tuv
