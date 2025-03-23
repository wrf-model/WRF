!WRF:MODEL_LAYER:PHYSICS

MODULE module_wind_mav
!
! Represents kinetic energy extracted by wind turbines and turbulence
! (TKE) they produce at model levels within the rotor area.
! This module is based on module_wind_fitch but uses the Jensen, XA and Gm wake
! loss models instead of the Fitch parameterization

! Code by Yulong MA (Guangdong-Hong kong-Macau Greater Bay Area Weather
! Research Center for Monitoring Warning and Forecasting;UDEL) and Cristina L. Archer (UDEL)

! --- NOTICE ---
! The following papers should be cited whenever presenting results using this scheme:
! Ma, Yulong, Cristina L. Archer, and Ahmadreza Vasel-Be-Hagh. "The Jensen wind
! farm parameterization." Wind Energy Science 7.6 (2022): 2407-2431.
! Ma, Yulong, Cristina L. Archer, and Ahmad Vasel‐Be‐Hagh. "Comparison of
! individual versus ensemble wind farm parameterizations inclusive of sub‐grid
! wakes for the WRF model." Wind Energy 25.9 (2022): 1573-1595.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!

#if defined(mpas)
  use mpas_dmpar
  use mpas_derived_types

  IMPLICIT NONE
  INTEGER, PARAMETER :: max_domains  = 1
  REAL, PARAMETER :: piconst = 3.141593
  logical, save :: windfarm_initialized = .false.  ! MPAS
#else 

  USE module_driver_constants, ONLY : max_domains
  USE module_model_constants, ONLY :  piconst

  USE module_llxy
  USE module_dm, ONLY : wrf_dm_min_real, wrf_dm_sum_reals
  USE module_configure, ONLY : grid_config_rec_type


  IMPLICIT NONE
#endif

  INTEGER, PARAMETER :: MAXVALS  = 100
  INTEGER :: nt
  INTEGER, DIMENSION(:), ALLOCATABLE :: NKIND, NVAL
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: ival,jval ! grid number in WRF
  REAL, DIMENSION(:), ALLOCATABLE :: hubheight, radius, radius2, diameter, area,&
                                     stc, stc2, cutin, cutout, npower
  REAL, DIMENSION(:,:), ALLOCATABLE :: xturb, yturb  ! (nt, maxdomain)
  REAL, DIMENSION(:,:), ALLOCATABLE :: turbws, turbtc, turbpw, turbpwcof  !  (nt,maxvals)

  REAL :: correction_factor

  CONTAINS

  !======================================================================

  subroutine  dragforce_mav(itimestep         &
       &,id                                      &
       &,z_at_w,z_at_m,u,v                       &
       &,dx,dz,dt,tke                            &
       &,du,dv                                   &
       &,windfarm_opt,power                      &
       &,windfarm_wake_model, windfarm_overlap_method &
       &,xland                                   &
#if defined(mpas)
       &,dminfo                                  &
       &,windfarm_ij, windfarm_deg               &
       &,xcell, ycell                            &
#else 
       &,cosa,sina                               &
#endif
       &,ids,ide,jds,jde,kds,kde                 &
       &,ims,ime,jms,jme,kms,kme                 &
       &,its,ite,jts,jte,kts,kte                 &
       &)

  implicit none

  integer, intent(in) :: id,windfarm_opt, windfarm_wake_model, windfarm_overlap_method
  integer, intent(in) :: its,ite,jts,jte,kts,kte
  integer, intent(in) :: ims,ime,jms,jme,kms,kme
  integer, intent(in) :: ids,ide,jds,jde,kds,kde
  real, intent(in) :: dx, dt
  real, dimension(ims:ime,kms:kme,jms:jme), intent(in) :: dz, u, v, z_at_w, z_at_m
  real, dimension(ims:ime,kms:kme,jms:jme), intent(inout) :: du, dv, tke
  real, dimension(ims:ime,jms:jme), intent(in) :: xland
  real, dimension(ims:ime,jms:jme), intent(inout) :: power
  integer, intent(in) :: itimestep

  real, dimension(ims:ime,kms:kme,jms:jme) :: Uearth, Vearth ! earth-relative u and v

#if defined(mpas)
  type(dm_info),intent(in) :: dminfo
  integer, intent(in) :: windfarm_ij
  real, intent(in) :: windfarm_deg
  real, dimension(ims:ime, jms:jme), intent(in) :: xcell, ycell  !hexgon cell center
#else 
  real, dimension(ims:ime,jms:jme), intent(in) :: cosa,sina
#endif

  ! Local
  real :: wfdensity
  integer :: itf, jtf, i, j, k
  integer :: wake_model, num_models, overlap_method
  integer :: wake_model_en(5), overlap_method_en(5)
  real, dimension(kms:kme) :: z_tmp
  real, dimension(ims:ime,kms:kme,jms:jme) :: tke_wk, du_wk, dv_wk

  real :: kw_nt(nt)
  real :: search_angle, search_dis
  integer :: ii, tt, kt
  integer :: num_ups_pot(nt), ups_indx_pot(nt,nt) ! potential ups turbines
  real :: avg_angle_tb(nt,nt) ! potential ups turbines

  integer :: tbindx(nt), num_ups(nt), ups_index(nt,nt)
  real :: ao_ups(nt,nt), ax_dist(nt,nt), ay_dist(nt,nt), az_dist(nt,nt)
  real :: blockfrac(nt), blockdist(nt), rblockdist(nt), ytb_rot_gm(nt,nt) ! GM
  logical :: find_tb
  real :: u_hub_nt(nt), v_hub_nt(nt), Uinf(nt), ulocal(nt), xland_nt(nt), terrain_nt(nt)
  real :: power_nt(nt), power_nt_md(5,nt)

  ! dir avg
  integer, parameter :: dir_num = 7
  real, parameter :: dir_avg_window = 5.0  ! +- 2.5 unit [degree]
  integer :: dir_ii
  real :: dtheta
  real :: dtheta_list(7)  ! [-2.5, -1.5, -0.5, 0., 0.5, 1.5, 2.5]
  real :: dtheta_avg_cof(7)  !gaussian distribution
  real :: dtheta_std  !gaussian distribution std

  ! parallel computing
  real :: dm_local_u_hub_nt(nt), dm_global_u_hub_nt(nt)
  real :: dm_local_v_hub_nt(nt), dm_global_v_hub_nt(nt)
  real :: dm_local_xland_nt(nt), dm_global_xland_nt(nt)
  real :: dm_local_terrain_nt(nt), dm_global_terrain_nt(nt)
  integer :: ic_tb

  integer,save :: n_valid_cur = 0
  integer :: tb_valid_cur(nt)


#if defined(mpas)
  wfdensity = 1.0/(dx*dx*sqrt(3.)/2.)
#else 
  wfdensity = 1.0/(dx*dx)
#endif

  tb_valid_cur(:) = 1 ! set all tbs in operation

  !---------------------------------------------
  ! Gaussion distribution direction avg
  dtheta_list(1) = -2.5; dtheta_list(7) = 2.5;
  dtheta_list(2) = -1.5; dtheta_list(6) = 1.5;
  dtheta_list(3) = -0.5; dtheta_list(5) = 0.5;
  dtheta_list(4) = 0.

  dtheta_std = 2.0  ! std [deg]
  dtheta_avg_cof(1) = exp(-dtheta_list(1)**2/(2.*dtheta_std**2))
  dtheta_avg_cof(2) = exp(-dtheta_list(2)**2/(2.*dtheta_std**2))
  dtheta_avg_cof(3) = exp(-dtheta_list(3)**2/(2.*dtheta_std**2))
  dtheta_avg_cof(4) = 1.
  dtheta_avg_cof(7) = dtheta_avg_cof(1)
  dtheta_avg_cof(6) = dtheta_avg_cof(2)
  dtheta_avg_cof(5) = dtheta_avg_cof(3)

  dtheta_avg_cof(:) = dtheta_avg_cof(:)/sum(dtheta_avg_cof)
  !---------------------------------------------

  !
  ! for parallel computing
  !
  itf = MIN0(ite,ide-1)
  jtf = MIN0(jte,jde-1)

  dm_local_u_hub_nt(:) = 0.
  dm_local_v_hub_nt(:) = 0.
  dm_local_xland_nt(:) = 0.
  dm_local_terrain_nt(:) = 0.
  dm_global_u_hub_nt(:) = 0.
  dm_global_v_hub_nt(:) = 0.
  dm_global_xland_nt(:) = 0.
  dm_global_terrain_nt(:) = 0.
  ic_tb = 0

#if defined(mpas)
  do kt = 1, nt
      i = ival(kt,id)
      j = jval(kt,id)
      if (i >= its .and. i <= itf .and. j >= jts .and. j <= jtf) then
          ic_tb = ic_tb + 1
          z_tmp = z_at_m(i,:,j) - z_at_w(i,1,j)  ! mass point height
          call to_zk2(hubheight(kt), z_tmp(1:kme-1), u(i,1:kme-1,j), kme-1, dm_local_u_hub_nt(kt))
          call to_zk2(hubheight(kt), z_tmp(1:kme-1), v(i,1:kme-1,j), kme-1, dm_local_v_hub_nt(kt))
      end if
  end do
  call mpas_dmpar_sum_real_array(dminfo, nt, dm_local_u_hub_nt, dm_global_u_hub_nt)
  call mpas_dmpar_sum_real_array(dminfo, nt, dm_local_v_hub_nt, dm_global_v_hub_nt)

#else


  ! ---- WRF grid related wind direction to earth related direction ---
  ! for Non Mercator projection, the wind direction should be rotated to earth
  ! coordinates (where U would be west-east and V would be north-south)
  ! https://www2.mmm.ucar.edu/wrf/users/FAQ_files/Miscellaneous.html
  DO j = jts, min(jte,jde-1)
  DO k = kts, kte-1
  DO i = its, min(ite,ide-1)
      Uearth(i,k,j) = U(i,k,j)*cosa(i,j) - V(i,k,j)*sina(i,j)
      Vearth(i,k,j) = V(i,k,j)*cosa(i,j) + U(i,k,j)*sina(i,j)
  ENDDO
  ENDDO
  ENDDO

  do kt = 1, nt
      i = ival(kt,id)
      j = jval(kt,id)
      if (i >= its .and. i <= itf .and. j >= jts .and. j <= jtf) then
          ic_tb = ic_tb + 1
          z_tmp = z_at_m(i,:,j) - z_at_w(i,1,j)  ! mass point height
          call to_zk2(hubheight(kt), z_tmp(1:kme-1), Uearth(i,1:kme-1,j), kme-1, dm_local_u_hub_nt(kt))
          call to_zk2(hubheight(kt), z_tmp(1:kme-1), Vearth(i,1:kme-1,j), kme-1, dm_local_v_hub_nt(kt))
          
          dm_local_xland_nt(kt) = xland(i,j)
          dm_local_terrain_nt(kt) = z_at_w(i,1,j)
      end if

      ! if turbine kt is out of the whole domain (i or j == -9999), assume it is not at 
      ! upstream of any turbines (distance >= 20D), set xturb, yturb to a large value
      ! and set uhub, vhub to a small value. It should have no effects on the rest of turbines.
      if (i == -9999 .or. j == -9999) then
          tb_valid_cur(kt) = 0
          dm_local_u_hub_nt(kt) = 1.e-3
          dm_local_v_hub_nt(kt) = 1.e-3
      endif
  end do

  call wrf_dm_sum_reals(dm_local_u_hub_nt, dm_global_u_hub_nt)
  call wrf_dm_sum_reals(dm_local_v_hub_nt, dm_global_v_hub_nt)
  call wrf_dm_sum_reals(dm_local_xland_nt, dm_global_xland_nt)
  call wrf_dm_sum_reals(dm_local_terrain_nt, dm_global_terrain_nt)
#endif

  u_hub_nt(:) = dm_global_u_hub_nt(:)
  v_hub_nt(:) = dm_global_v_hub_nt(:)
  xland_nt(:) = dm_global_xland_nt(:)
  terrain_nt(:) = dm_global_terrain_nt(:)

  !if (ic_tb == 0) return  ! no turbine in this tile, no need to do the rest part


  !
  ! potential ups turbines in a fan-shaped region
  !
  Uinf(:) = sqrt(u_hub_nt(:)**2 + v_hub_nt(:)**2) ! hub height speed

  search_angle = 30.*piconst/180.  ! +-30 deg, a wider region because of wind dir avg
  search_dis = 20.*diameter(1)     ! 20D
  num_ups_pot(:) = 0
  do kt = 1, nt
      if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain
      ii = 0
      do tt = 1, nt
          if (tt == kt) cycle
          find_tb = find_turb(xturb(kt,id), yturb(kt,id), xturb(tt,id), yturb(tt,id), &
                              u_hub_nt(kt), v_hub_nt(kt), search_angle, search_dis)
          if (find_tb) then
              ii = ii + 1
              ups_indx_pot(kt, ii) = tt
              avg_angle_tb(kt, tt) = atan2(v_hub_nt(kt)+v_hub_nt(tt), u_hub_nt(kt)+u_hub_nt(tt))
          end if
      end do
      num_ups_pot(kt) = ii
  end do


  ! 
  ! dir avg start 
  !
  tke_wk(:,:,:) = 0.
  du_wk(:,:,:) = 0.
  dv_wk(:,:,:) = 0.
  power(:,:) = 0.
  power_nt(:) = 0.  ! output to a txt file
  power_nt_md(:,:) = 0.  ! output to a txt file

  !------------------- Ensemble ---------------------
  if (windfarm_wake_model <= 3) then
     num_models = 1
     wake_model_en(1) = windfarm_wake_model
     overlap_method_en(1) = windfarm_overlap_method

  ! 1=JS, 2=XA, 3=GM
  else if (windfarm_wake_model == 4) then  ! JS-M4 + XA-M3
     num_models = 2 
     wake_model_en(1) = 1;   overlap_method_en(1) = 4
     wake_model_en(2) = 2;   overlap_method_en(2) = 3

  else if (windfarm_wake_model == 5) then ! JS-M4 + XA-M3 + GM
     num_models = 3 
     wake_model_en(1) = 1;   overlap_method_en(1) = 4
     wake_model_en(2) = 2;   overlap_method_en(2) = 3
     wake_model_en(3) = 3;   overlap_method_en(3) = 2

  else if (windfarm_wake_model == 6) then ! JS-M3 + JS-M4 + XA-M3 + GM, single-cell
     num_models = 4
     wake_model_en(1) = 1;   overlap_method_en(1) = 3
     wake_model_en(2) = 1;   overlap_method_en(2) = 4
     wake_model_en(3) = 2;   overlap_method_en(3) = 3
     wake_model_en(4) = 3;   overlap_method_en(4) = 2

  else if (windfarm_wake_model == 7) then ! JS-M4 + XA-M3 + XA-M4 + GM, multi-cell
     num_models = 4
     wake_model_en(1) = 1;   overlap_method_en(1) = 4
     wake_model_en(2) = 2;   overlap_method_en(2) = 3
     wake_model_en(3) = 2;   overlap_method_en(3) = 4
     wake_model_en(4) = 3;   overlap_method_en(4) = 2
  end if
  !------------------- Ensemble ---------------------

  do dir_ii = 1, dir_num ! dir avg loop
      if (dir_num > 1) then
          !dtheta = -(0.5*dir_avg_window - (dir_ii-1.)/(dir_num-1.)*dir_avg_window)/180.*piconst
          dtheta = dtheta_list(dir_ii)/180.*piconst
      else
          dtheta = 0.
      end if

      do ii = 1, num_models
        wake_model = wake_model_en(ii)
        overlap_method = overlap_method_en(ii)

        ! actual upstream turbines (overlap area > 0)
        call ups_turbs(kw_nt, ao_ups, ax_dist, ay_dist, az_dist, ytb_rot_gm, ups_index, num_ups, &
                       num_ups_pot, ups_indx_pot, avg_angle_tb, xturb(:,id), yturb(:,id), &
                       radius, area, hubheight, xland_nt, terrain_nt, nt, dtheta, wake_model)

        ! sort all turbines from the most upstream turbine
        ! NOT BASED on ax_dist because they are not at the same diretion. 
        ! (a directed graph problem)
        call sort_turb(nt, num_ups, ups_index, tbindx)

        ! cal. def and local speed
        if (wake_model == 1) then
            call cal_tb_ulocal_JS(ulocal, uinf, tbindx, num_ups, ups_index,      &
                                  ax_dist, Ao_ups, kw_nt, nt, radius, tb_valid_cur, overlap_method)

        else if (wake_model == 2) then
            call cal_tb_ulocal_XA(ulocal, uinf, tbindx, num_ups, ups_index,  &
                                  ax_dist, ay_dist, az_dist, Ao_ups,         &
                                  nt, radius, radius2, tb_valid_cur, overlap_method)

        else if (wake_model == 3) then
            call cal_tb_ulocal_GM(ulocal, blockfrac, blockdist, rblockdist, &
                                  uinf, tbindx, num_ups, ups_index,         &
                                  ax_dist, ay_dist, az_dist, ytb_rot_gm,    &
                                  nt, radius, tb_valid_cur)
        end if

        ! cal power and WRF tendencies
        call cal_power_wrf_tend(ulocal, uinf, tb_valid_cur, blockfrac, blockdist, u, v, dz, z_at_w, &
                                ival(:,id), jval(:,id), nt, radius, diameter, hubheight, area, &
                                wake_model, wfdensity, dt,             &
                                power_nt_md(ii,:), power, tke_wk, du_wk, dv_wk, dtheta_avg_cof(dir_ii), & 
                                ims,ime,jms,jme,kms,kme,its,itf,jts,jtf)
      end do
  end do

  tke_wk = tke_wk/num_models
  du_wk = du_wk/num_models
  dv_wk = dv_wk/num_models
  power = power/num_models

  tke = tke_wk  ! turbine generated TKE
  du = du + du_wk
  dv = dv + dv_wk
 
  do ii = 1, num_models
      power_nt(:) = power_nt(:) + power_nt_md(ii,:)
  enddo
  power_nt = power_nt/num_models

  ! write fraction power of each turbine to a txt at 4 hr
  !call write_power_txt(windfarm_wake_model, windfarm_overlap_method, itimestep, dt, its, jts, &
  !                    dx, power_nt, power_nt_md, ulocal, nt, num_models)

  end subroutine dragforce_mav


!==============================================================================
!==============================================================================


  subroutine write_power_txt(windfarm_model, windfarm_method, itimestep, dt, its, jts, &
                             dx, power_nt, power_nt_md, ulocal, nt, num_models)
  ! this function might be improved later.
  implicit none
  integer :: nt, windfarm_model, windfarm_method, itimestep, its, jts, num_models
  real :: dx, power_nt(nt), ulocal(nt), power_nt_md(5,nt), dt
  integer :: it_out, ii, i, j, kt
  integer,save :: it_init = 0, write_out = 0
  character(len=1024) :: fmt_my, str_my, fn_my
  real :: out_hr, max_power

  out_hr = 4. ! hr

  if (it_init == 0) it_init = itimestep

  write (str_my, "(I1)") windfarm_method

  IF (windfarm_model == 1) THEN
     fn_my = 'power_nt_JS_M'//trim(str_my)//'.txt_5.0d_0.25'
  ELSEIF (windfarm_model == 2) THEN
     fn_my = 'power_nt_XA_M'//trim(str_my)//'.txt_5.0d_0.25'
  ELSEIF (windfarm_model == 3) THEN
     IF (windfarm_method == 2) fn_my = 'power_nt_GM_MC.txt_5.0d_0.25'
     IF (windfarm_method == 3) fn_my = 'power_nt_GM_AN.txt_5.0d'
  ENDIF

  IF (windfarm_model == 4) fn_my = 'power_nt_EN2.txt_5.0d_0.25'
  IF (windfarm_model == 5) fn_my = 'power_nt_EN3.txt_5.0d_0.25'

  IF (windfarm_model == 6) fn_my = 'power_nt_EN6.txt_2.5d'
  IF (windfarm_model == 7) fn_my = 'power_nt_EN7.txt_2.5d'


  !if (itimestep == it_out .and. its == 1 .and. jts == 1) then 
  if ((itimestep-it_init)*dt >= 4.*3600. .and. write_out == 0 .and. its == 1 .and. jts == 1) then 
     write_out = 1

     write(*,*) 'output relative power', (itimestep-it_init)*dt
     OPEN ( FILE = fn_my, UNIT = 923)
     write (str_my, "(I6)") nt
     fmt_my = '('//trim(str_my)//'F12.2)'
 
     write(923,FMT=fmt_my) power_nt(1:nt)

     do ii = 1, num_models
         write(923,FMT=fmt_my) power_nt_md(ii,1:nt)
     end do

     write(923,FMT=fmt_my) ulocal(1:nt)
     CLOSE(923)

  endif
  end subroutine write_power_txt

!---------------------------------------------------------------

  subroutine ups_turbs( kw_nt, ao_ups, ax_dist, ay_dist, az_dist, ytb_rot_gm, ups_index, num_ups, &
                        num_ups_pot, ups_indx_pot, avg_angle_tb, xturb, yturb, &
                        radius, area, hubheight, xland_nt, terrain_nt, nt, dtheta, windfarm_model)
  implicit none
  integer, intent(in) :: nt, num_ups_pot(nt), ups_indx_pot(nt,nt), windfarm_model
  real, intent(in) :: avg_angle_tb(nt,nt), xturb(nt), yturb(nt), &
                      radius(nt), area(nt), hubheight(nt), xland_nt(nt), terrain_nt(nt)
  real, intent(out) :: ao_ups(nt,nt), ax_dist(nt,nt), ay_dist(nt,nt), az_dist(nt,nt), &
                       ytb_rot_gm(nt,nt), kw_nt(nt)
  integer, intent(out) :: ups_index(nt,nt), num_ups(nt)
  real :: dtheta
  
  integer :: num_ups_turb, tt, jt, kt, ii
  real :: cur_tb_ang, ax_GM(nt), x_ups_tmp, y_ups_tmp, x_cur, y_cur, &
           axialdist, Ao, wakewidth
  real :: kw_tmp, kw_test(nt), kw

  !-----------------------
  do kt = 1, nt
      if (xland_nt(kt) > 1.5) then  ! water = 2
          kw = 0.04    ! offshore
      else if (xland_nt(kt) < 1.5) then  ! land = 1
          kw = 0.0075  ! onshore
      end if

      if (windfarm_model == 1) then
          kw_test(kt) = kw
          kw_nt(kt) = kw
      else if (windfarm_model == 2) then
          kw_test(kt) = 5.*kw  ! choose a larger search region for XA
      end if
  end do

  if (windfarm_model == 3) then
      kw_test(:) = 0.     ! no wake expandation for GM
  end if
  !-----------------------


  do kt = 1, nt
      num_ups_turb = 0
      do tt = 1, num_ups_pot(kt)

          jt = ups_indx_pot(kt,tt)

          cur_tb_ang = avg_angle_tb(kt,jt) + dtheta
          call coordinate_rotation(x_cur, y_cur, xturb(kt), yturb(kt), cur_tb_ang)
          call coordinate_rotation(x_ups_tmp, y_ups_tmp, xturb(jt), yturb(jt), cur_tb_ang)

          axialdist = x_cur - x_ups_tmp
          if (axialdist <= 0.) then
              Ao = 0.
          else
              kw_tmp = kw_test(jt)
              wakewidth = radius(jt) + kw_tmp*axialdist
              Ao = AreaOverlap(y_cur, y_ups_tmp, hubheight(kt)+terrain_nt(kt), &
                               hubheight(jt)+terrain_nt(jt), radius(kt), wakewidth)
          end if
 
          !if (Ao/area(kt) > 0.) then
          if (Ao/area(kt) > 0.01) then
              num_ups_turb = num_ups_turb + 1
              ups_index(kt,num_ups_turb) = jt
              Ao_ups(kt,jt) = Ao/area(kt)
              ax_dist(kt,jt) = axialdist
              ay_dist(kt,jt) = y_cur - y_ups_tmp
              az_dist(kt,jt) = (hubheight(kt) + terrain_nt(kt)) - &
                               (hubheight(jt) + terrain_nt(jt)) 

              ax_gm(num_ups_turb) = axialdist  ! for GM to sort ups turbines
              ytb_rot_gm(kt,jt) =  y_ups_tmp
          end if

          ! used in analytical GM, it changes if ups turbines are 
          ! in different grid cells, just approximate value here. TO BE IMPROVED!
          ytb_rot_gm(kt,kt) = y_cur  

      end do
      num_ups(kt) = num_ups_turb

      if (windfarm_model == 3 .and. num_ups(kt) > 1) then  ! GM model
          call sort_gm(num_ups(kt), ups_index(kt,1:num_ups(kt)), ax_gm(1:num_ups(kt)))
      end if

  end do

  end subroutine ups_turbs

!---------------------------------------------------------------

  subroutine cal_tb_ulocal_JS(ulocal, uinf, tbindx, num_ups, ups_index, &
                              ax_dist, Ao_ups, kw_nt, nt, radius, tb_valid_cur, overlap_method)
  implicit none
  real, intent(out) :: ulocal(nt)
  real, intent(in ) :: uinf(nt), Ao_ups(nt,nt), ax_dist(nt,nt), radius(nt), kw_nt(nt)
  integer, intent(in) :: nt, tbindx(nt), num_ups(nt), ups_index(nt,nt), overlap_method
  integer,intent(in) :: tb_valid_cur(nt)

  ! turbws, turbtc, turbpwcof, stc, stc2, nval are global varibles, not defined here
  
  integer :: kt, it, jt, tt, nv
  real :: Udef_nt(nt), def_ij, tmp, thrcof

  ulocal(:) = uinf(:)

  do kt = 1, nt
  
      if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain

      it = tbindx(kt) ! tb of interet

      if (num_ups(it) == 0) cycle

      Udef_nt(:) = 0.
      do tt = 1, num_ups(it)
          jt = ups_index(it,tt)
          nv = nval(jt)
          call dragcof(tmp, tmp, thrcof, ulocal(jt), turbws(jt,1:nv), &
                       turbtc(jt,1:nv), turbpwcof(jt,1:nv), stc(jt), stc2(jt), nv)

          def_ij = (1. - sqrt(1. - thrcof))/(1. + kw_nt(jt)*ax_dist(it,jt)/radius(jt))**2*Ao_ups(it,jt)

          ! wake overlapping methods M1 - M4
          if (overlap_method == 1 .or. overlap_method == 2) then
              Udef_nt(jt) = uinf(jt)*def_ij*Ao_ups(it,jt)

          else if (overlap_method == 3) then
              Udef_nt(jt) = ulocal(jt)*def_ij*Ao_ups(it,jt)

          ! Here Udef_nt is actually a local U, not a DeltaU
          else if (overlap_method == 4) then
              Udef_nt(jt) = uinf(it)*(1. - Ao_ups(it,jt)) + uinf(jt)*(1. - def_ij)*Ao_ups(it,jt)
          end if

      end do

      if (overlap_method == 1) then
          ulocal(it) = Uinf(it) - sum(Udef_nt)
      else if (overlap_method == 2 .or. overlap_method == 3) then
          ulocal(it) = Uinf(it) - sqrt(sum(Udef_nt**2))
      else if (overlap_method == 4) then 
          ulocal(it) = sqrt(sum(Udef_nt**2)/num_ups(it))
      end if

  enddo

  end subroutine cal_tb_ulocal_JS

!---------------------------------------------------------------

  subroutine cal_tb_ulocal_XA(ulocal, uinf, tbindx, num_ups, ups_index, &
                              ax_dist, ay_dist, az_dist, Ao_ups, &
                              nt, radius, radius2, tb_valid_cur, overlap_method)
  implicit none
  real, intent(out) :: ulocal(nt)
  real, intent(in ) :: uinf(nt), Ao_ups(nt,nt), ax_dist(nt,nt), ay_dist(nt,nt), &
                       az_dist(nt,nt),  radius(nt), radius2(nt)
  integer, intent(in) :: nt, tbindx(nt), num_ups(nt), ups_index(nt,nt), overlap_method
  integer,intent(in) :: tb_valid_cur(nt)

  ! turbws, turbtc, turbpwcof, stc, stc2, nval are global varibles, not defined here
  
  real :: ky, kz
  integer :: kt, it, jt, tt, nv
  real :: Udef_nt(nt), def_ij, tmp, thrcof
  real :: beta, eps, sigmay, sigmaz, def_avg

  ! --- Are ky and kz the same over land?
  ky = 0.025
  kz = 0.0175
  
  ulocal(:) = uinf(:)

  do kt = 1, nt

      if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain

      it = tbindx(kt) ! tb of interet

      if (num_ups(it) == 0) cycle

      Udef_nt(:) = 0.
      do tt = 1, num_ups(it)
          jt = ups_index(it,tt)
          nv = nval(jt)
          call dragcof(tmp, tmp, thrcof, ulocal(jt), turbws(jt,1:nv), &
                       turbtc(jt,1:nv), turbpwcof(jt,1:nv), stc(jt), stc2(jt), nv)

          beta = 0.5*(1. + sqrt(1. - thrcof))/sqrt(1. - thrcof)
          eps = 0.25*sqrt(beta)
          sigmay = ky*ax_dist(it,jt) + eps*2*radius(jt)
          sigmaz = kz*ax_dist(it,jt) + eps*2*radius(jt)
          call Gaussian_integral(ay_dist(it,jt), az_dist(it,jt), radius(it), sigmay, sigmaz, def_avg)
          def_ij = (1. - sqrt(1.-radius2(jt)*thrcof/sigmay/sigmaz/2.))*def_avg

          ! wake overlapping methods M1 - M4
          if (overlap_method == 1 .or. overlap_method == 2) then
              Udef_nt(jt) = Uinf(jt)*def_ij

          else if (overlap_method == 3) then
              Udef_nt(jt) = ulocal(jt)*def_ij

          ! Here Udef_nt is actually a local U, not a DeltaU
          else if (overlap_method == 4) then
              Udef_nt(jt) = Uinf(jt)*(1. - def_ij)
          end if
      end do

      if (overlap_method == 1) then
          ulocal(it) = Uinf(it) - sum(Udef_nt)
      else if (overlap_method == 2 .or. overlap_method == 3) then
          ulocal(it) = Uinf(it) - sqrt(sum(Udef_nt**2))
      else if (overlap_method == 4) then 
          ulocal(it) = sqrt(sum(Udef_nt**2)/num_ups(it))
      end if
  end do

  end subroutine cal_tb_ulocal_XA

!---------------------------------------------------------------

  subroutine cal_tb_ulocal_GM(ulocal, blockfrac, blockdist, rblockdist, &
                              uinf, tbindx, num_ups, ups_index, &
                              ax_dist, ay_dist, az_dist, ytb_rot_gm, &
                              nt, radius, tb_valid_cur)
  implicit none
  real, intent(out) :: ulocal(nt), blockfrac(nt), blockdist(nt), rblockdist(nt)
  integer, intent(in) :: nt, tbindx(nt), num_ups(nt), ups_index(nt,nt)
  real, intent(in) :: uinf(nt), ax_dist(nt,nt), ay_dist(nt,nt), az_dist(nt,nt), &
                      ytb_rot_gm(nt,nt), radius(nt)
  integer,intent(in) :: tb_valid_cur(nt)
  integer :: kt, it
  real :: gfun_GM

  integer, parameter :: ndisk = 50 ! 50x50 samples for montecarlo
  real, parameter :: MAXD = 20. ! upsteam within 20d
  integer :: ii, jd, kd, jt, tt, nblock
  integer :: ndiskpt 
  real :: diskpt(ndisk)
  real :: distblk(ndisk,ndisk), rdistblk(ndisk,ndisk)
  real :: scaled_axdist(nt), raxdist(nt)
  integer :: on_disk(ndisk,ndisk)
  real :: on_disk_1d(ndisk*ndisk)
  real :: on_disk_1d_y(ndisk*ndisk), on_disk_1d_z(ndisk*ndisk)
  real :: on_disk_1d_yr(ndisk*ndisk), on_disk_1d_zr(ndisk*ndisk)
  real :: distblk_1d(ndisk*ndisk), rdistblk_1d(ndisk*ndisk)

  integer, parameter :: cal_method = 2 ! 1 : analytical, 2 = montecarlo

  ulocal(:) = uinf(:)

  if (cal_method == 2) then 

      do ii = 1, ndisk
          diskpt(ii) = -1. +  (ii-0.5)/ndisk*2.
      end do

      !on_disk(:,:) = 0
      on_disk_1d(:) = 0.
      on_disk_1d_y(:) = 0.
      on_disk_1d_z(:) = 0.
      ndiskpt = 0
      do jd = 1, ndisk
      do kd = 1, ndisk
          if (diskpt(jd)**2 + diskpt(kd)**2 < 1.) then
              ndiskpt = ndiskpt + 1
              !on_disk(jd,kd) = 1
              on_disk_1d(ndiskpt) = 1.
              on_disk_1d_y(ndiskpt) = diskpt(jd)
              on_disk_1d_z(ndiskpt) = diskpt(kd)
          endif
      end do
      end do

      do kt = 1, nt

          if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain
 
          it = tbindx(kt) ! tb of interest

          if (num_ups(it) == 0) then
              blockfrac(it) = 0.
          else
              do tt = 1, num_ups(it)
                  jt = ups_index(it,tt)
                  scaled_axdist(jt) = ax_dist(it,jt)/(MAXD*2.*radius(jt)) ! scaled by 20*diameter 
                  raxdist(jt) = 1./ax_dist(it,jt)
              end do
        
              nblock = 0

              on_disk_1d_yr(1:ndiskpt) = on_disk_1d_y(1:ndiskpt)*radius(it)
              on_disk_1d_zr(1:ndiskpt) = on_disk_1d_z(1:ndiskpt)*radius(it)

              !--- montecarlo 1 ---
              distblk_1d(1:ndiskpt) = on_disk_1d(1:ndiskpt) 
              rdistblk_1d(1:ndiskpt) = 0.
              do ii = 1, ndiskpt ! on tb it
                  do tt = num_ups(it), 1, -1  ! starting from the closest turbine
                      jt = ups_index(it,tt)
                      if ((on_disk_1d_yr(ii) - ay_dist(it,jt))**2 + &  ! on tb jt
                          (on_disk_1d_zr(ii) - az_dist(it,jt))**2 < radius2(jt)) then
                          nblock = nblock + 1
                          distblk_1d(nblock) = scaled_axdist(jt) ! ax_dist(jt)/(20*diameter(it))
                          rdistblk_1d(nblock) = raxdist(jt)      ! 1./ax_dist(jt)
                          exit
                      end if
                  end do
              end do
              blockdist(it) = sum(distblk_1d(1:ndiskpt))/ndiskpt
              rblockdist(it) = sum(rdistblk_1d(1:ndiskpt))/ndiskpt
              !--- montecarlo 1 ---

        
              !--- montecarlo 2 ---
              !!on_disk and ndiskpt are the same for all turbines, already calculated
              !!set distblk(jd,kd) = 1. on turbine (= 0 out of turbine)
              !distblk(:,:) = on_disk(:,:)*1.0
              !rdistblk(:,:) = 0.
              !do jd = 1, ndisk
              !do kd = 1, ndisk
              !    if (on_disk(jd,kd) == 1) then ! on turbine it
              !        do tt = num_ups(it), 1, -1  ! starting from the closest turbine
              !            jt = ups_index(it,tt)
              !            if ((diskpt(jd)*radius(it) - ay_dist(it,jt))**2 + &  ! on tb jt
              !                (diskpt(kd)*radius(it) - az_dist(it,jt))**2 < radius2(jt)) then
              !                nblock = nblock + 1
              !                distblk(jd,kd) = scaled_axdist(jt) ! ax_dist(jt)/(20*diameter(it))
              !                rdistblk(jd,kd) = raxdist(jt)      ! 1./ax_dist(jt)
              !                exit
              !            end if
              !        end do
              !    end if
              !end do
              !end do
              !blockdist(it) = sum(distblk)/ndiskpt
              !rblockdist(it) = sum(rdistblk)/ndiskpt
              !--- montecarlo 2 ---


              blockfrac(it) = float(nblock)/ndiskpt
              if (blockdist(it) > 1.) blockfrac(it) = 0.
          end if ! num_ups(it) > 0

          !---
          if (blockfrac(it) == 0.) then
              gfun_GM = 1.
          else
              gfun_GM = 0.9615 - 0.1549*blockfrac(it) - 0.0114*rblockdist(it)*20.*2*radius(it)
          end if
          ulocal(it) = Uinf(it)*gfun_GM
      enddo
  endif


  if (cal_method == 1) then 
      do kt = 1, nt

          if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain

          it = tbindx(kt) ! tb of interet
          call gm_BD_BR_analytical(blockfrac(it), blockdist(it), rblockdist(it), &
                                   radius(it), num_ups(it), ups_index(it,1:nt), nt, it, &
                                   ax_dist(it,1:nt), ytb_rot_gm(it,1:nt))
          if (blockfrac(it) == 0.) then
              gfun_GM = 1.
          else
              gfun_GM = 0.9615 - 0.1549*blockfrac(it) - 0.0114*rblockdist(it)*20.*2*radius(it)
          end if
          ulocal(it) = Uinf(it)*gfun_GM
      enddo
  endif
  end subroutine cal_tb_ulocal_GM

!---------------------------------------------------------------

  subroutine cal_power_wrf_tend(ulocal, uinf, tb_valid_cur, blockfrac, blockdist, u, v, dz, z_at_w, &
                                ival, jval, nt, radius, diameter, hubheight, area, &
                                windfarm_model, wfdensity, dt, &
                                power_nt, power, tke_wk, du_wk, dv_wk, dtheta_avg_cof_i, &
                                ims,ime,jms,jme,kms,kme,its,itf,jts,jtf)
  implicit none
  integer :: ims, ime, jms, jme, kms, kme, its, itf, jts, jtf
  real, dimension(ims:ime,kms:kme,jms:jme), intent(in) :: u, v, dz, z_at_w
  real, dimension(ims:ime,kms:kme,jms:jme) :: tke_wk, du_wk, dv_wk ! wrf output
  real, dimension(ims:ime,jms:jme) :: power  ! wrf output
  real :: power_nt(nt) ! output
  real :: dtheta_avg_cof_i  !gaussian distribution

  integer :: nt, ival(nt), jval(nt), windfarm_model
  real :: ulocal(nt), Uinf(nt), blockfrac(nt), blockdist(nt)
  real :: radius(nt), diameter(nt), hubheight(nt), area(nt), wfdensity, dt
  integer :: tb_valid_cur(nt)
  
  integer :: kt, nv, i, j, k
  real, dimension(kms:kme) :: speed_z, tarea_z, power2_z, z_tmp
  real :: power_GM, power1, power2, ec, tkecof, powcof, thrcof 
  real :: blade_l_point,blade_u_point,z1,z2
  integer :: k_turbine_bot, k_turbine_top
  real :: tmp_spd

  ! turbws, turbtc, turbpwcof, stc, stc2, nval are global varibles, not defined here

  do kt = 1, nt

      if (tb_valid_cur(kt) == 0) cycle ! turbine is turned off or outside of the domain

      ! power for each tb
      !IF (windfarm_model == 3) THEN  ! GM model
      !! YL: For multi-grid cases, I don't have a solution for actual power by GM.
      !! It might be scale with the maximun power for the wind farm.
      !    IF (blockfrac(kt) == 0.) THEN
      !        power_GM = 1.
      !    ELSE
      !        power_GM = 0.6824 - 0.3405*blockfrac(kt) + 0.2131*blockdist(kt)
      !    ENDIF
      !ENDIF

      nv = nval(kt)
      call dragcof(tkecof, powcof, thrcof, &
                   ulocal(kt), turbws(kt,1:nv), turbtc(kt,1:nv), &
                   turbpwcof(kt,1:nv), stc(kt), stc2(kt), nv)

      power1 = 0.5*1.23*ulocal(kt)**3*area(kt)*powcof ! 1.23 density

      power_nt(kt) = power_nt(kt) + power1*dtheta_avg_cof_i
      !!------- end power for each tb --------


      !----------- WRF tendencies ------------
      ! only considering turbines in the current tile
      ! the follwoing code is based on Fitch parameterization

      i = ival(kt)
      j = jval(kt)
      if (i > itf .or. i < its .or. j > jtf .or. j < jts ) cycle

      ! vertical layers cut by turbine blades
      blade_l_point = hubheight(kt) - radius(kt)
      blade_u_point = hubheight(kt) + radius(kt)
      k_turbine_bot = 0      !bottom level
      k_turbine_top = -1     !top level
      z_tmp = z_at_w(i,:,j) - z_at_w(i,1,j)
      do k = kms, kme-1
          if (blade_l_point >= z_tmp(k) .and. blade_l_point < z_tmp(k+1)) then
              k_turbine_bot = k
          end if
          if (blade_u_point >= z_tmp(k) .and. blade_u_point < z_tmp(k+1)) then
              k_turbine_top = k
          end if
      end do

      ! adjust coef. according to disk averaged power
      power2_z(:) = 0.
      do k = k_turbine_bot, k_turbine_top ! loop over turbine blade levels
          z1 = max(z_tmp(k) - blade_l_point, 0.)
          z2 = min(z_tmp(k+1) - blade_l_point, diameter(kt))
          CALL turbine_area(z1, z2, diameter(kt), tarea_z(k))

          speed_z(k) = ulocal(kt)/Uinf(kt)*sqrt(u(i,k,j)**2 + v(i,k,j)**2)
          power2_z(k) = 0.5*1.23*speed_z(k)**3*tarea_z(k)*powcof
      end do
      power2 = sum(power2_z)
      if (power1 == 0. .or. power2 == 0.) then
          ec = 1.
      else
          ec = power1/power2
      end if
      !ec = ec*wfdensity
      ec = ec*wfdensity*dtheta_avg_cof_i

      power(i,j) = power(i,j) + power2*dtheta_avg_cof_i  ! WRF output

      do k = k_turbine_bot, k_turbine_top ! loop over turbine blade levels
          !qke_wk(i,k,j) = qke_wk(i,k,j) + speed_z(k)**3*tarea_z(k)*tkecof*dt/dz(i,k,j)*ec
          tke_wk(i,k,j) = tke_wk(i,k,j) + 0.5*speed_z(k)**3*tkecof*tarea_z(k)/dz(i,k,j)*dt*ec
          du_wk(i,k,j)  = du_wk(i,k,j)  - 0.5*u(i,k,j)*thrcof*speed_z(k)*tarea_z(k)/dz(i,k,j)*ec
          dv_wk(i,k,j)  = dv_wk(i,k,j)  - 0.5*v(i,k,j)*thrcof*speed_z(k)*tarea_z(k)/dz(i,k,j)*ec
      end do

  end do

  end subroutine cal_power_wrf_tend

!---------------------------------------------------------------

  subroutine sort_turb(nt, num_ups, ups_index, tbindx)
  implicit none
  integer, intent(in) :: nt
  integer, intent(in) :: num_ups(nt), ups_index(nt,nt)
  integer, intent(inout) :: tbindx(nt)
  integer :: ic_tb, indx, kt, tt, flag(nt)
  
  flag(:) = 0
  ic_tb = 0
  
  do kt = 1, nt
      if (num_ups(kt) == 0) then
          ic_tb = ic_tb + 1
          flag(kt) = 1
          tbindx(ic_tb) = kt  ! sorted turb starting from upstream
      end if
  end do

  do while (ic_tb < nt)
      do kt = 1, nt
          if (flag(kt) == 1) cycle

          do tt = 1, num_ups(kt)
              indx = ups_index(kt,tt)
              if (flag(indx) == 0) exit

              if (tt == num_ups(kt)) then
                  ic_tb = ic_tb + 1
                  flag(kt) = 1
                  tbindx(ic_tb) = kt 
              end if
          end do
      end do
  enddo

  if (sum(flag) < nt) then
      write(*,*) 'something wrong in sorting turbine, wind_jensen/sort_turb'
      write(*,*) tbindx
      stop
  end if 

  endsubroutine sort_turb

!---------------------------------------------------------------

  subroutine sort_gm(nturb, tbindx, ax_dist)
  implicit none
  integer, intent(in) :: nturb
  integer, intent(out), dimension(nturb) :: tbindx
  real, intent(inout), dimension(nturb) :: ax_dist
  real, dimension(nturb) :: xloc
  integer :: i, a(1)
  real :: xmin
  integer :: tbindx_cp(nturb)

  xloc = ax_dist
  tbindx_cp = tbindx
  xmin = minval(xloc) - 1.

  do i = 1, nturb
      a = maxloc(xloc)
      tbindx(i) = tbindx_cp(a(1))
      xloc(a(1)) = xmin
  end do

  end subroutine sort_gm

!---------------------------------------------------------------

!---------------------------------------------------------------

  subroutine gm_BD_BR_analytical(blockfrac, blockdist, rblockdist, &
                                 radius, num_ups, ups_index, nt, it, ax_dist, y)
  implicit none
  integer :: nt, num_ups, it
  integer :: ups_index(nt)
  real :: ax_dist(nt), y(nt)
  real :: scaled_axdist(nt), raxdist(nt)
  real :: radius
  real, intent(out) :: blockfrac, blockdist, rblockdist
 
  real, parameter :: MAXD = 20. ! upsteam within 20d
  integer, parameter :: ndisk = 80
  real :: diameter, radius2, d, BR, BD, mindr, mindl
  integer :: tt, jt, numuptl, numuptr, jmindisl, jmindisr
  real :: blockdist_ups(nt), blockfrac_ups(nt), rblockdist_ups(nt)

  if (num_ups == 0) then
      blockfrac = 0.
      return
  endif

  diameter = radius*2
  radius2 = radius**2

  blockfrac_ups(:) = 0.
  blockdist_ups(:) = 0.
  rblockdist_ups(:) = 0.

  mindr = diameter
  mindl = diameter
  numuptl = 0
  numuptr = 0
  jmindisl = 0
  jmindisr = 0

  ! only look for 4 upstream turbines ??? YL

  do tt = num_ups, 1, -1  ! starting from the closest turbine
      jt = ups_index(tt)
      if (ax_dist(jt) > maxd*diameter) exit ! only consider ups tbs within 20d

      !--------------------
      d = y(jt) - y(it)
      
      if (d <= 0.) then !upstream turbine on the left side of (or on) the centerline
          numuptl = numuptl + 1
          if (abs(d) > mindl) then
              blockfrac_ups(jt) = 0.
          else
              if (numuptl == 1) then
                  if (numuptr == 0) then
                      blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius)
                  else
                      if ( abs(d) + mindr < diameter ) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(jt), y(jmindisr), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius)
                      end if
                  end if
              else
                  if (numuptr > 0 .and. abs(d) + mindr < diameter) then
                      if (mindr + mindl < diameter) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisl), radius) - &
                                              Ao_GM(y(jt), y(jmindisr), radius) + &
                                              Ao_GM(y(jmindisl), y(jmindisr), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisl), radius) - &
                                              Ao_GM(y(jt), y(jmindisr), radius)
                      end if
                  else
                      if (mindr + mindl < diameter) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisl), radius) + &
                                              Ao_GM(y(jmindisl), y(jmindisr), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisl), radius)
                      end if
                  end if
              end if
              mindl = abs(d)
              jmindisl = jt

              ! don't need to look for further ups tbs
              if (d == 0.) then
                  blockdist_ups(jt) = blockfrac_ups(jt)*ax_dist(jt)/(MAXD*diameter)
                  rblockdist_ups(jt) = blockfrac_ups(jt)/ax_dist(jt)
                  exit
              end if

          end if

      else !upstream turbine on the right side of the centerline
          numuptr = numuptr + 1
          if (abs(d) > mindr) then
              blockfrac_ups(jt) = 0.
          else
              if (numuptr == 1) then
                  if (numuptl == 0) then
                      blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius)
                  else
                      if ( abs(d) + mindl < diameter ) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(jt), y(jmindisl), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius)
                      end if
                  end if
              else
                  if (numuptl > 0 .and. abs(d) + mindl < diameter) then
                      if (mindr + mindl < diameter) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisr), radius) - &
                                              Ao_GM(y(jt), y(jmindisl), radius) + &
                                              Ao_GM(y(jmindisl), y(jmindisr), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisr), radius) - &
                                              Ao_GM(y(jt), y(jmindisl), radius)
                      end if
                  else
                      if (mindr + mindl < diameter) then
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisr), radius) + &
                                              Ao_GM(y(jmindisl), y(jmindisr), radius)
                      else
                          blockfrac_ups(jt) = Ao_GM(y(it), y(jt), radius) - &
                                              Ao_GM(y(it), y(jmindisr), radius)
                      end if
                  end if
              end if
              mindr = abs(d)
              jmindisr = jt
          end if
      end if !center, left and right are all done for blockfrac_ups(jt)

      blockdist_ups(jt) = blockfrac_ups(jt)*ax_dist(jt)/(MAXD*diameter)
      rblockdist_ups(jt) = blockfrac_ups(jt)/ax_dist(jt)

  end do

  BR = sum(blockfrac_ups)
  BD = sum(blockdist_ups) + 1.*(1.-BR)  ! normalized dist for non-blocked part is 1.

  blockfrac = BR 
  blockdist = BD
  rblockdist = sum(rblockdist_ups)

  if (blockdist > 1.) blockfrac = 0.

  end subroutine gm_BD_BR_analytical

!---------------------------------------------------------------

  function Ao_GM(x1, x2, Radius) result(Ao)
  implicit none
  real,intent(in) :: x1,x2,Radius
  real :: Ao
  real :: d, l, theta, Asector, Atriangle
  
  Ao = 0.
  d = sqrt((x1-x2)**2)
  if (d<2*Radius) then
      l = d/2.  !Improve later in case hubs are not at same height
      theta = 2 * acos(l/Radius)
      Asector = theta/2.*Radius**2
      Atriangle = l*Radius*sin(theta/2.)
      Ao = 2*(Asector - Atriangle)/(piconst*radius**2)
  end if

  end function Ao_GM

!---------------------------------------------------------------

  function AreaOverlap(y1, y2, z1, z2, r1, r2) result(AOverlap)
  implicit none
  real,intent(in) :: y1, y2, z1, z2, r1, r2
  real :: AOverlap
  real :: c, CBD, CAD

  c = sqrt((z1-z2)**2 + (y1-y2)**2)

  if ((c + min(r2,r1)) <= max(r2,r1)) then
     AOverlap = piconst*min(r2,r1)**2
  else if ((r1 + r2) <= c) then
     AOverlap = 0.
  else
     CBD = acos((r2**2 + c**2 - r1**2)/(2*r2*c))
     CAD = acos((r1**2 + c**2 - r2**2)/(2*r1*c))
     AOverlap = CBD*r2**2 + CAD*r1**2 - 0.5*r2**2*sin(2*CBD) - 0.5*r1**2*sin(2*CAD)
     !AOverlap = CBD*r2**2 + CAD*r1**2 - r1*c*sin(CAD)
  end if

  end function AreaOverlap

!---------------------------------------------------------------

  function find_turb(xc, yc, xt, yt, u, v, sr_angle, sr_dis)  result(ft)
  implicit none
  logical :: ft 
  real :: xc, yc, xt, yt, sr_angle, sr_dis, u, v
  real :: posi_angle, posi_dis, spd, xp, yp, angle
  real ( kind = 8 ) :: tmp1, tmp2

  ft = .false.

  xp = xt - xc
  yp = yt - yc
  posi_dis = sqrt(yp**2 + xp**2) 

  if (posi_dis <= sr_dis) then
      posi_angle = atan2(-yp, -xp) 
      spd = sqrt(u**2 + v**2)
      !tmp1 = -(u*xp + v*yp) ! negative means ups diretion
      tmp1 = real( -(u*xp + v*yp), kind = 8 )
      tmp2 = real( sqrt( (u**2 + v**2) * (xp**2 + yp**2) ), kind = 8)

      if (abs(tmp2) < abs(tmp1)) then
          tmp2 = sign(tmp1,tmp2)
      end if

      tmp1 = tmp1 / tmp2
      if (tmp1 < -1.0_8 .or. tmp1 > 1.0_8) then
          angle = 0.
      else
          angle = real(acos(tmp1), kind = 4)
      end if
 
      if (abs(angle) <= sr_angle) then
          ft = .true.
      end if
  end if

  end function find_turb

!---------------------------------------------------------------

  subroutine coordinate_rotation(xr, yr, x, y, theta)
  implicit none
  real :: xr, yr, x, y, theta
  xr = x*cos(theta) + y*sin(theta)
  yr = -x*sin(theta) + y*cos(theta)
  end subroutine coordinate_rotation
          
!---------------------------------------------------------------

  subroutine Gaussian_integral(ch, ck, R, sigma_x, sigma_y, avg_val) 
  ! integration of Gaussian distribution over an offset circle:
  ! (x-ch)**2 + (y-ck)**2 <= R**2
  ! DiDonato and Jarnagin, 1961
  implicit none
  real, intent(in) :: ch, ck, R, sigma_x, sigma_y 
  real :: d01, d11, t, A, P, avg_val, sum_val
  real :: WW(24), XX(24) ! 24 point gaussian quadrature integral for 1D function
  integer :: i
  
  ! https://pomax.github.io/bezierinfo/legendre-gauss.html
  WW( 1)=0.1279381953467522;  XX( 1)= -0.0640568928626056
  WW( 2)=0.1279381953467522;  XX( 2)= 0.0640568928626056
  WW( 3)=0.1258374563468283;  XX( 3)= -0.1911188674736163
  WW( 4)=0.1258374563468283;  XX( 4)= 0.1911188674736163
  WW( 5)=0.1216704729278034;  XX( 5)= -0.3150426796961634
  WW( 6)=0.1216704729278034;  XX( 6)= 0.3150426796961634
  WW( 7)=0.1155056680537256;  XX( 7)= -0.4337935076260451
  WW( 8)=0.1155056680537256;  XX( 8)= 0.4337935076260451
  WW( 9)=0.1074442701159656;  XX( 9)= -0.5454214713888396
  WW(10)=0.1074442701159656;  XX(10)= 0.5454214713888396
  WW(11)=0.0976186521041139;  XX(11)= -0.6480936519369755
  WW(12)=0.0976186521041139;  XX(12)= 0.6480936519369755
  WW(13)=0.0861901615319533;  XX(13)= -0.7401241915785544
  WW(14)=0.0861901615319533;  XX(14)= 0.7401241915785544
  WW(15)=0.0733464814110803;  XX(15)= -0.8200019859739029
  WW(16)=0.0733464814110803;  XX(16)= 0.8200019859739029
  WW(17)=0.0592985849154368;  XX(17)= -0.8864155270044011
  WW(18)=0.0592985849154368;  XX(18)= 0.8864155270044011
  WW(19)=0.0442774388174198;  XX(19)= -0.9382745520027328
  WW(20)=0.0442774388174198;  XX(20)= 0.9382745520027328
  WW(21)=0.0285313886289337;  XX(21)= -0.9747285559713095
  WW(22)=0.0285313886289337;  XX(22)= 0.9747285559713095
  WW(23)=0.0123412297999872;  XX(23)= -0.9951872199970213
  WW(24)=0.0123412297999872;  XX(24)= 0.9951872199970213
  
  sum_val = 0.
  do i = 1, 24      ! 24 point gaussian quadrature integral
      t = 0.5*XX(i) + 0.5 
      d01 = (ck - R*t*sqrt(2.-t**2))/(sqrt(2.)*sigma_y)
      d11 = (ck + R*t*sqrt(2.-t**2))/(sqrt(2.)*sigma_y)
      P = (exp(-0.5*( (ch - R*(1.-t**2))/sigma_x )**2) +   &
           exp(-0.5*( (ch + R*(1.-t**2))/sigma_x )**2)) *  &
          (erf(d11) - erf(d01))*t
      sum_val = sum_val + 0.5*WW(i)*P
  end do
  !A = R/sigma_y/np.sqrt(2*np.pi)  ! normalized gaussian distribution
  A = (2*piconst*sigma_x*sigma_y) * R/sigma_x/sqrt(2*piconst)
  avg_val = A*sum_val/(piconst*R**2)
   
  end subroutine Gaussian_integral

!---------------------------------------------------------------

  subroutine to_zk2(obs_v, mdl_v, mdl_data, iz, interp_out )
  ! 1D interp function
  implicit none
  integer :: k, iz, k1
  real, intent(in) :: obs_v
  real, dimension(1:iz), intent(in) :: mdl_v, mdl_data
  real, intent(out) :: interp_out
  real :: dz, dzm, zk

  if (obs_v < mdl_v(1) ) then
      interp_out = mdl_data(1)
      return
  else if (obs_v >= mdl_v(iz)) then
      interp_out = mdl_data(iz)
      return
  else
      do k = 1,iz-1
          if(obs_v >= mdl_v(k) .and. obs_v < mdl_v(k+1)) then
              zk = real(k) + (obs_v - mdl_v(k))/(mdl_v(k+1) - mdl_v(k))
              exit
          end if
      end do
      k1 = int( zk )
      dz = zk - float( k1 )
      dzm = float( k1+1 ) - zk

      interp_out = dzm*mdl_data(k1) + dz*mdl_data(k1+1)
      return
  end if

  end subroutine to_zk2

!---------------------------------------------------------------

  subroutine turbine_area(z1, z2, tdiameter, tarea)
  ! This subroutine calculates area of turbine between two vertical levels
  ! Input variables :
  !            z1 = distance between k level and lower blade tip
  !            z2 = distance between k+1 level and lower blade tip
  !            wfdensity = wind farm density in m^-2
  !     tdiameter = turbine diameter
  ! Output variable :
  !         tarea = area of turbine between two levels
  implicit none
  real, intent(in) :: tdiameter
  real, intent(inout) :: z1, z2
  real, intent(out):: tarea
  real r, zc1, zc2

  r = 0.5*tdiameter       !r = turbine radius
  z1 = r - z1             !distance of kth level from turbine center
  z2 = r - z2             !distance of k+1 th level from turbine center
  zc1 = abs(z1)
  zc2 = abs(z2)

  ! turbine area between z1 and z2
  if(z1 > 0. .and. z2 > 0.) then
      tarea = zc1*sqrt(r*r - zc1*zc1) + r*r*asin(zc1/r) - &
              zc2*sqrt(r*r - zc2*zc2) - r*r*asin(zc2/r)
  else if(z1 < 0. .and. z2 < 0.) then
      tarea = zc2*sqrt(r*r - zc2*zc2) + r*r*asin(zc2/r) - &
              zc1*sqrt(r*r - zc1*zc1) - r*r*asin(zc1/r)
  else
      tarea = zc2*sqrt(r*r - zc2*zc2) + r*r*asin(zc2/r) + &
              zc1*sqrt(r*r - zc1*zc1) + r*r*asin(zc1/r)
  end if

  end subroutine turbine_area

!---------------------------------------------------------------

  subroutine dragcof(tkecof, powcof, thrcof, speed, &
                     turb_ws, turb_tc, turb_pwcof, stdthrcoef, stdthrcoef2, nv)
  implicit none
  real, intent(in):: speed, stdthrcoef, stdthrcoef2
  integer :: nv
  real, dimension(1:nv) :: turb_ws, turb_tc, turb_pwcof
  real, intent(out):: tkecof,powcof,thrcof
  real :: cispeed, cospeed

  cispeed = turb_ws(1)
  cospeed = turb_ws(nv)

  if (speed < cispeed) then
      thrcof = stdthrcoef
      powcof = 0.
  else if (speed > cospeed) then
      thrcof = stdthrcoef2
      powcof = 0.
  else
      call to_zk2(speed, turb_ws(1:nv), turb_tc(1:nv), nv, thrcof)
      call to_zk2(speed, turb_ws(1:nv), turb_pwcof(1:nv), nv, powcof)
  endif

  ! tke coefficient calculation
  tkecof = max(0., thrcof-powcof)   !Cri: consider multiplying by 0.5 or so
  tkecof = correction_factor * tkecof
  !tkecof = 0.25*tkecof  ! Yulong

  end subroutine dragcof

!---------------------------------------------------------------

#if defined(mpas)
  SUBROUTINE point_in_polyogon(find, px, py, xcell, ycell, dv)
  implicit none

  ! dv: side length of hexgon
  real, intent(in) :: px, py, xcell, ycell, dv
  real :: xx, yy
  logical :: find

  xx = abs(px - xcell)
  yy = abs(py - ycell)

  find = .false.
  if (xx <= dv .and. yy <= sqrt(3.)/2.*dv) then  ! in the outer rectangle
      if (dv - xx >= yy/sqrt(3.) ) find = .true.
  endif

  END SUBROUTINE point_in_polyogon

!---------------------------------------------------------------

  ! called in core_atmosphere/physics/mpas_atmphys_init.F
  subroutine init_module_wind_jensen_MPAS(dminfo, windfarm_ij, windfarm_deg, &
                                          xcell, ycell, ncells, dc)
  implicit none
  type(dm_info),intent(in) :: dminfo
  integer :: ncells
  integer, parameter :: id = 1
  integer :: windfarm_ij
  real :: windfarm_deg
  real :: dc, dv
  real, dimension(ncells), intent(in) :: xcell, ycell  !hexgon cell center
! 
  real :: lat,lon,ts_rx,ts_ry
  real :: known_lat, known_lon
  integer :: i,j,k,ios, igs, jgs

  real :: x_rot, y_rot, theta, deg, xtb_center, ytb_center, xp, yp
  logical :: find
  character*256 num,input

  if (windfarm_initialized) return

  windfarm_initialized = .true.

  dv = sqrt(3.)/3.*dc

  if (windfarm_ij == 1) then
      open(71,file='windturbines-xy.txt',form='formatted',status='old',iostat=ios)
  else if (windfarm_ij == 2) then
      open(71,file='windturbines-ll.txt',form='formatted',status='old',iostat=ios)
  end if
 
  nt = 0
  do
      read(71, *, iostat=ios)
      if (ios /= 0) exit
      nt = nt + 1
  end do
  close(71)

  allocate (nkind(nt),nval(nt),ival(nt,max_domains),jval(nt,max_domains))
  allocate (xturb(nt,max_domains),yturb(nt,max_domains))
  allocate (hubheight(nt),stc(nt),stc2(nt),area(nt),radius(nt),radius2(nt),diameter(nt),npower(nt))
  allocate(turbws(nt,MAXVALS),turbtc(nt,MAXVALS),turbpw(nt,MAXVALS),turbpwcof(nt,MAXVALS))

  xturb = -9999.
  yturb = -9999.
  ival = -9999
  jval = -9999
  turbws = 0.
  turbtc = 0.
  turbpw = 0.
  turbpwcof = 0.

  if (windfarm_ij == 1) then
      open(71,file='windturbines-xy.txt',form='formatted',status='old',iostat=ios)
      do k = 1, nt
          nkind(k) = 1
          read(71,*) xturb(k,id), yturb(k,id)
      enddo
      close(71)

      !------- set wind farm center coordinate to (0,0) ---
      xtb_center = sum(xturb(1:nt,id))/nt
      ytb_center = sum(yturb(1:nt,id))/nt
      do k = 1, nt
          xturb(k,id) = xturb(k,id) - xtb_center
          yturb(k,id) = yturb(k,id) - ytb_center
      end do
      !-----------------------------------------------------

      !------- rotate wind farm -------
      deg = windfarm_deg
      do k = 1, nt
          !theta = -30./180.*piconst  ! d255: 225 - 255 = -30 

          theta = deg/180.*piconst
          call coordinate_rotation(x_rot, y_rot, xturb(k,id), yturb(k,id), theta)
          xturb(k,id) = x_rot
          yturb(k,id) = y_rot
      end do
      !-------------------------------

      !!-------------- find ix, iy -----------------

      igs = 10
      jgs = 12
      ival(:,id) = -9999
      jval(:,id) = 1
      DO k = 1, nt
          xp = xturb(k,id) + igs*sqrt(3.)/2.*dc
          yp = yturb(k,id) + jgs*sqrt(3.)/2.*dc
          DO i = 1, ncells
              call point_in_polyogon(find, xp, yp, xcell(i), ycell(i), dv)
              IF (find) THEN
                  ival(k,id) = i
                  EXIT
              ENDIF
          ENDDO
      ENDDO

      !write(*,*) 'MPAS loc0:', ival(:,id)
      !call mpas_dmpar_bcast_ints(dminfo, nt, ival(:,id))

      ! ---- test in one cell ---
      !ival(:,id) = ival(1,id)
      !write(*,*) 'MPAS loc:', ival(1,id)
      write(*,*) 'MPAS loc:'
      do k = 1, nt
          write(*,*) k, ival(k,id)
      end do
      !write(*,*) 'xcell:', xcell(1), xcell(ncells)
      !write(*,*) 'ycell:', ycell(1), ycell(ncells)
      ! ---- test in one cell ---
     !!-------------- end find ix, iy -----------

      do k = 1, nt
          write(num,*) nkind(k)
          num = adjustl(num)
          input = "wind-turbine-"//trim(num)//".tbl"
          open(file=trim(input),unit=19,form='formatted',status='old')
          read(19,*) nval(k)
          read(19,*) hubheight(k), diameter(k), stc(k), npower(k)

          area(k)=piconst/4.*diameter(k)**2

          do i = 1, nval(k)
              read(19,*) turbws(k,i), turbtc(k,i), turbpw(k,i)
              turbpwcof(k,i) = turbpw(k,i)*1000./(0.5*1.23*turbws(k,i)**3*area(k))
          end do

          radius(k) = 0.5*diameter(k)
          radius2(k) = radius(k)**2
          stc2(k) = turbtc(k,nval(k))
          close (19)
      end do
  endif

  end subroutine init_module_wind_jensen_MPAS

!---------------------------------------------------------------
#else
  
subroutine cal_xturb_yturb(lat_nt, lon_nt, wf_id_nt, nt, xturb_nt, yturb_nt)
  implicit none
  integer :: nt
  real(kind=8) :: lat_nt(nt), lon_nt(nt)
  integer :: wf_id_nt(nt)
  real(kind=8) :: xturb_nt(nt), yturb_nt(nt)

  integer :: ic, wf_id, k, kk, ik, mid_ic, nn
  real(kind=8) :: lon_tmp(nt), lat_wf(nt), lon_wf(nt)
  real(kind=8) :: lon_center, x, y
  real(kind=8) :: x_center, y_center
  real(kind=8) :: off_dist = 600000. ! used to seprate wind farms
  integer :: num_wf

  num_wf = 1

  ik = 1
  ic = 1
  wf_id = wf_id_nt(1)
  lon_tmp(ic) = lon_nt(1)
  lat_wf(ic) = lat_nt(1)
  lon_wf(ic) = lon_nt(1)
  do k = 2, nt
      if (wf_id_nt(k) == wf_id) then
         ic = ic + 1
         lon_tmp(ic) = lon_nt(k)
         lat_wf(ic) = lat_nt(k)
         lon_wf(ic) = lon_nt(k)
      else if (wf_id_nt(k) /= wf_id) then
         call shell_sort_1D(lon_tmp(1:ic),ic)
         mid_ic = ceiling(ic*0.5)
         lon_center = lon_tmp(mid_ic)

         x_center = 0.
         y_center = 0.
         do kk = 1, ic
             call latlon_to_xy(lat_wf(kk), lon_wf(kk), lon_center, x, y)
             !call latlon_to_xy(lat_wf(kk), lon_wf(kk), real(9.,kind=8), x, y) !Anholt test 
             xturb_nt(ik) = x
             yturb_nt(ik) = y
             x_center = x_center + x
             y_center = y_center + y
             ik = ik + 1
         enddo

         x_center = x_center/ic
         y_center = y_center/ic
         do kk = ik-ic, ik-1
             xturb_nt(kk) = xturb_nt(kk) - x_center
             yturb_nt(kk) = yturb_nt(kk) - y_center + num_wf*off_dist! off set distance for wind farm [m]
         enddo

         num_wf = num_wf + 1
         ic = 1
         wf_id = wf_id_nt(k)
         lon_tmp(ic) = lon_nt(k)
         lat_wf(ic) = lat_nt(k)
         lon_wf(ic) = lon_nt(k)
      endif
  enddo

  call shell_sort_1D(lon_tmp(1:ic),ic)
  mid_ic = ceiling(ic*0.5)
  lon_center = lon_tmp(mid_ic)

  x_center = 0.
  y_center = 0.
  do kk = 1, ic
      call latlon_to_xy(lat_wf(kk), lon_wf(kk), lon_center, x, y)
      !call latlon_to_xy(lat_wf(kk), lon_wf(kk), real(9.,kind=8), x, y) !Anholt test
      xturb_nt(ik) = x
      yturb_nt(ik) = y
      x_center = x_center + x
      y_center = y_center + y
      ik = ik + 1
  enddo

  x_center = x_center/ic
  y_center = y_center/ic
  do kk = ik-ic, ik-1
      xturb_nt(kk) = xturb_nt(kk) - x_center
      yturb_nt(kk) = yturb_nt(kk) - y_center +  num_wf*off_dist ! off set distance for wind farm [m]
  enddo

end subroutine cal_xturb_yturb

!------------------------------

subroutine latlon_to_xy(latitude, longitude, central_lon, easting, northing)
! from https://github.com/Turbo87/utm/blob/master/utm/conversion.py
  implicit none
  real(kind=8), intent(in) :: latitude, longitude, central_lon
  real(kind=8), intent(out) :: easting, northing

  real(kind=8), PARAMETER :: pi = 3.141592653589793
  real(kind=8) :: lat_rad, lat_sin, lat_cos, lat_tan, lat_tan2, lat_tan4
  real(kind=8) :: lon_rad
  real(kind=8) :: central_lon_rad, dlon_rad

  real(kind=8), PARAMETER :: K0 = 0.9996
  real(kind=8), PARAMETER :: E = 0.00669438
  real(kind=8), PARAMETER :: R = 6378137.
  real(kind=8) :: E2, E3, E_P2, SQRT_E
  real(kind=8) :: XE, XE2, XE3, XE4, XE5
  real(kind=8) :: M1, M2, M3, M4, P2, P3, P4, P5
  real(kind=8) :: n, c, a, a2, a3, a4, a5, a6, m

  lat_rad = latitude*pi/180.
  lat_sin = sin(lat_rad)
  lat_cos = cos(lat_rad)

  lat_tan = lat_sin / lat_cos
  lat_tan2 = lat_tan * lat_tan
  lat_tan4 = lat_tan2 * lat_tan2

  lon_rad = longitude*pi/180.

  ! differenct from UTM, set center lon at the wind farm center
  central_lon_rad = central_lon*pi/180. 

  ! -pi to pi
  dlon_rad = mod(lon_rad - central_lon_rad + pi, 2*pi) - pi

  E2 = E * E
  E3 = E2 * E
  E_P2 = E / (1. - E)

  SQRT_E = sqrt(1. - E)

  XE = (1. - SQRT_E) / (1. + SQRT_E)
  XE2 = XE * XE
  XE3 = XE2 * XE
  XE4 = XE3 * XE
  XE5 = XE4 * XE

  M1 = (1. - E / 4. - 3. * E2 / 64. - 5. * E3 / 256.)
  M2 = (3. * E / 8. + 3. * E2 / 32. + 45. * E3 / 1024.)
  M3 = (15. * E2 / 256. + 45. * E3 / 1024.)
  M4 = (35. * E3 / 3072.)

  P2 = (3. / 2. * XE - 27. / 32. * XE3 + 269. / 512. * XE5)
  P3 = (21. / 16. * XE2 - 55. / 32. * XE4)
  P4 = (151. / 96. * XE3 - 417. / 128. * XE5)
  P5 = (1097. / 512. * XE4)


  n = R / sqrt(1. - E * lat_sin**2)
  c = E_P2 * lat_cos**2

  a = lat_cos * dlon_rad
  a2 = a * a
  a3 = a2 * a
  a4 = a3 * a
  a5 = a4 * a
  a6 = a5 * a

  m = R * (M1 * lat_rad -    &
           M2 * sin(2. * lat_rad) +  &
           M3 * sin(4. * lat_rad) -  &
           M4 * sin(6. * lat_rad))

  easting = K0 * n * (a +  &
                      a3 / 6. * (1. - lat_tan2 + c) +  &
                      a5 / 120. * (5. - 18. * lat_tan2 + lat_tan4 + 72. * c - 58. * E_P2)) + 500000.

  northing = K0 * (m + n * lat_tan * &
                     (a2 / 2. +      &
                      a4 / 24. * (5. - lat_tan2 + 9. * c + 4. * c**2) +   &
                      a6 / 720. * (61. - 58. * lat_tan2 + lat_tan4 + 600. * c - 330. * E_P2)))

!  if (latitude < 0.) northing = northing + 10000000.

end subroutine latlon_to_xy

!------------------------------

subroutine shell_sort_1D(AA, n)
  implicit none
  integer :: n, k
  real(kind=8), dimension(1:n) :: AA
  integer :: i,j
  real(kind=8) :: A_tmp
  integer :: B_tmp
  k=n/2
  do while( k>0 )
    do i=k+1,n
       j=i-k
       do while( j>0 )
          if ( AA(j) .gt. AA(j+k) ) then
              A_tmp = AA(j)
              AA(j) = AA(j+k)
              AA(j+k) = A_tmp

              j=j-k
          else
              exit
          end if
      end do
    end do
    k=k/2
  end do

end subroutine shell_sort_1D

  subroutine init_module_wind_mav(id,config_flags,xlong,xlat,windfarm_initialized,dx,&
                                     ims,ime,jms,jme,its,ite,jts,jte,ids,ide,jds,jde)
  USE module_date_time ! must within subroutine, module_date_time.F ../share/
  implicit none
  integer :: ims,ime,jms,jme,ids,ide,jds,jde
  integer :: its,ite,jts,jte
  real :: dx
  real, dimension(ims:ime, jms:jme), intent(in) :: xlong,xlat
 
  type (grid_config_rec_type) :: config_flags
  type (proj_info) :: ts_proj
  logical :: windfarm_initialized ! WRF 
  character*256 num,input,message_wind
  real :: lat,lon,ts_rx,ts_ry
  real :: known_lat, known_lon
  integer :: i,j,k,id,ios, igs, jgs

  real :: xgrid(ide), ygrid(jde), tmp
  real :: x_rot, y_rot, theta, deg, xtb_center, ytb_center

  logical, external :: wrf_dm_on_monitor


  !--- local ---
  real(kind=8), dimension(:), allocatable :: lat_nt, lon_nt, xturb_nt, yturb_nt
  integer, dimension(:), allocatable :: wf_id_nt
  !--- local ---

  !---------
  logical :: lexist
  CHARACTER (LEN=24) :: date_str
  INTEGER:: julyr
  INTEGER:: julday
  REAL   :: gmt
  real(kind=8) :: calday
 
  !IF ( windfarm_initialized) RETURN 

  correction_factor = config_flags%windfarm_tke_factor

  ! get turbine number nt
  if ( wrf_dm_on_monitor() ) then
      if (config_flags%windfarm_ij == 1) then
          open(71,file='windturbines-xy.txt',form='formatted',status='old',iostat=ios)
      else if (config_flags%windfarm_ij == 2) then
          open(71,file='windturbines-ll.txt',form='formatted',status='old',iostat=ios)
      end if
 
      nt = 0
      do
          read(71, *, iostat=ios)
          if (ios /= 0) exit
          nt = nt + 1
      end do
      close(71)
  end if

  call wrf_dm_bcast_integer(nt,1)

  if (.not. windfarm_initialized) then
      allocate (nkind(nt),nval(nt),ival(nt,max_domains),jval(nt,max_domains))
      allocate (xturb(nt,max_domains),yturb(nt,max_domains))
      allocate (hubheight(nt),stc(nt),stc2(nt),area(nt),radius(nt),radius2(nt),diameter(nt),npower(nt))
      allocate(turbws(nt,MAXVALS),turbtc(nt,MAXVALS),turbpw(nt,MAXVALS),turbpwcof(nt,MAXVALS))

      allocate (xturb_nt(nt),yturb_nt(nt))
      allocate (lat_nt(nt),lon_nt(nt))
      allocate (wf_id_nt(nt))

      turbws = 0.
      turbtc = 0.
      turbpw = 0.
      turbpwcof = 0.
      nkind(:) = 1

      windfarm_initialized = .true.
  end if

  if (.not. allocated(nkind)) allocate(nkind(nt))
  if (.not. allocated(nval)) allocate(nval(nt))
  if (.not. allocated(ival)) allocate(ival(nt,max_domains))
  if (.not. allocated(jval)) allocate(jval(nt,max_domains))
  if (.not. allocated(xturb)) allocate(xturb(nt,max_domains))
  if (.not. allocated(yturb)) allocate(yturb(nt,max_domains))
  if (.not. allocated(hubheight)) allocate(hubheight(nt))
  if (.not. allocated(stc)) allocate(stc(nt))
  if (.not. allocated(stc2)) allocate(stc2(nt))
  if (.not. allocated(area)) allocate(area(nt))
  if (.not. allocated(radius)) allocate(radius(nt))
  if (.not. allocated(radius2)) allocate(radius2(nt))
  if (.not. allocated(diameter)) allocate(diameter(nt))
  if (.not. allocated(npower)) allocate(npower(nt))
  if (.not. allocated(turbws)) allocate(turbws(nt,maxvals))
  if (.not. allocated(turbtc)) allocate(turbtc(nt,maxvals))
  if (.not. allocated(turbpw)) allocate(turbpw(nt,maxvals))
  if (.not. allocated(turbpwcof)) allocate(turbpwcof(nt,maxvals))

  if (.not. allocated(xturb_nt)) allocate(xturb_nt(nt))
  if (.not. allocated(yturb_nt)) allocate(yturb_nt(nt))
  if (.not. allocated(lat_nt)) allocate(lat_nt(nt))
  if (.not. allocated(lon_nt)) allocate(lon_nt(nt))
  if (.not. allocated(wf_id_nt)) allocate(wf_id_nt(nt))

  xturb(:,id) = -9999.
  yturb(:,id) = -9999.
  ival(:,id) = -9999
  jval(:,id) = -9999

  !
  ! --- find turbine location ---
  !
  if ( wrf_dm_on_monitor() ) then

      ! real case, based on lat, lon
      if (config_flags%windfarm_ij == 2) then
          CALL map_init(ts_proj)
          open(71,file='windturbines-ll.txt',form='formatted',status='old',iostat=ios)

          do k = 1, nt
              !read(71,*) lat, lon
              read(71,*)  lat_nt(k), lon_nt(k), wf_id_nt(k), nkind(k)
              lat = lat_nt(k)
              lon = lon_nt(k)
              known_lat = xlat(its,jts) 
              known_lon = xlong(its,jts)

              ! Mercator
              if (config_flags%map_proj == PROJ_MERC) then
                 call map_set(PROJ_MERC, ts_proj,               &
                              truelat1 = config_flags%truelat1, &
                              lat1     = known_lat,             &
                              lon1     = known_lon,             &
                              knowni   = REAL(its),             &
                              knownj   = REAL(jts),             &
                              dx       = config_flags%dx)
        
              ! Lambert conformal
              else if (config_flags%map_proj == PROJ_LC) then
                 call map_set(PROJ_LC, ts_proj,                  &
                              truelat1 = config_flags%truelat1,  &
                              truelat2 = config_flags%truelat2,  &
                              stdlon   = config_flags%stand_lon, &
                              lat1     = known_lat,              &
                              lon1     = known_lon,              &
                              knowni   = REAL(its),              &
                              knownj   = REAL(jts),              &
                              dx       = config_flags%dx)
    
              ! Polar stereographic
              else if (config_flags%map_proj == PROJ_PS) then
                 call map_set(PROJ_PS, ts_proj,                  &
                              truelat1 = config_flags%truelat1,  &
                              stdlon   = config_flags%stand_lon, &
                              lat1     = known_lat,              &
                              lon1     = known_lon,              &
                              knowni   = REAL(its),              &
                              knownj   = REAL(jts),              &
                              dx       = config_flags%dx)
              end if

              call latlon_to_ij(ts_proj, lat, lon, ts_rx, ts_ry)

              ival(k,id)=nint(ts_rx)
              jval(k,id)=nint(ts_ry)
!              write(*,*) 'sss', id, k, ts_rx
              if (ival(k,id).lt.ids.and.ival(k,id).gt.ide) then
                  ival(k,id) = -9999
                  jval(k,id) = -9999
              end if  

          end do
          close(71)

          !--- cal turbine locations (x,y in [m]) based on (lat, lon)
          call cal_xturb_yturb(lat_nt, lon_nt, wf_id_nt, nt, xturb_nt, yturb_nt)
          do k = 1, nt
              xturb(k,id) = xturb_nt(k)
              yturb(k,id) = yturb_nt(k)
              !write(*,*) xturb(k,id), yturb(k,id)
          end do

      end if  ! windfarm_ij == 2

      ! ideal case, based on x, y (m)
      if (config_flags%windfarm_ij == 1) then
          open(71,file='windturbines-xy.txt',form='formatted',status='old',iostat=ios)
          do k = 1, nt
              read(71,*) xturb(k,id), yturb(k,id), wf_id_nt(k), nkind(k)
              !read(71,*) xturb(k,id), yturb(k,id)
              ! wf_id_nt(k) = 1 
              ! nkind(k) = 1
          enddo
          close(71)

          ! reset wind farm center coordinate to (0,0)
          xtb_center = sum(xturb(1:nt,id))/nt
          ytb_center = sum(yturb(1:nt,id))/nt
          do k = 1, nt
              xturb(k,id) = xturb(k,id) - xtb_center
              yturb(k,id) = yturb(k,id) - ytb_center
          end do

          ! rotate wind farm
          deg = config_flags%windfarm_deg
          do k = 1, nt
              !theta = -30./180.*piconst  ! d255: 225 - 255 = -30 
              theta = deg/180.*piconst
              call coordinate_rotation(x_rot, y_rot, xturb(k,id), yturb(k,id), theta)
              xturb(k,id) = x_rot
              yturb(k,id) = y_rot
          end do

          !!-------------- find ix, iy -----------------
          !igs = int(ide/2.5);   jgs = int(jde/2.5)  ! set wind farm center grid
          igs = int(ide/3);   jgs = int(jde/3)  ! set wind farm right lower coner

          do i = 1, ide
              xgrid(i) = (i-1)*dx
          end do
          do j = 1, jde
              ygrid(j) = (j-1)*dx
          end do

          do k = 1, nt
              tmp = (igs-1)*dx + xturb(k,id)
              do i = 1, ide-1
                  if (xgrid(i) <= tmp .and. xgrid(i+1) > tmp) then
                      ival(k,id) = i
                      exit
                  end if
              end do

              tmp = (jgs-1)*dx + yturb(k,id)
              do j = 1, jde-1
                  if (ygrid(j) <= tmp .and. ygrid(j+1) > tmp) then
                      jval(k,id) = j
                      exit
                  end if
              end do

              ! ---- test in one cell ---
              !ival(k,id) = igs
              !jval(k,id) = jgs
              !ival(k,id) = 12
              !jval(k,id) = 12
              ! ---- test in one cell ---
          end do
          !!-------------- end find ix, iy -----------
          write(*,*) 'WRF loc:'
          do k = 1, nt
              write(*,*) k, ival(k,id), jval(k,id)
          end do
      end if
  end if

  ! 
  ! read turbine info
  !
  if ( wrf_dm_on_monitor() ) then
      do k = 1, nt
          write(num,*) nkind(k)
          num = adjustl(num)
          input = "wind-turbine-"//trim(num)//".tbl"
          open(file=trim(input),unit=19,form='formatted',status='old')
          read(19,*) nval(k)
          read(19,*) hubheight(k), diameter(k), stc(k), npower(k)

          area(k)=piconst/4.*diameter(k)**2

          do i = 1, nval(k)
              read(19,*) turbws(k,i), turbtc(k,i), turbpw(k,i)
              turbpwcof(k,i) = turbpw(k,i)*1000./(0.5*1.23*turbws(k,i)**3*area(k))
          end do

          radius(k) = 0.5*diameter(k)
          radius2(k) = radius(k)**2
          stc2(k) = turbtc(k,nval(k))
          close (19)
      end do
  end if

  call wrf_dm_bcast_integer(nval,nt)
  call wrf_dm_bcast_integer(ival,nt*max_domains)
  call wrf_dm_bcast_integer(jval,nt*max_domains)
  call wrf_dm_bcast_real(xturb,nt*max_domains)
  call wrf_dm_bcast_real(yturb,nt*max_domains)
  call wrf_dm_bcast_real(hubheight,nt)
  call wrf_dm_bcast_real(area,nt)
  call wrf_dm_bcast_real(radius,nt)
  call wrf_dm_bcast_real(radius2,nt)
  call wrf_dm_bcast_real(diameter,nt)
  call wrf_dm_bcast_real(stc,nt)
  call wrf_dm_bcast_real(stc2,nt)
  call wrf_dm_bcast_real(npower,nt)
  call wrf_dm_bcast_integer(nkind,nt) 
  call wrf_dm_bcast_real(turbws,nt*maxvals) 
  call wrf_dm_bcast_real(turbtc,nt*maxvals) 
  call wrf_dm_bcast_real(turbpw,nt*maxvals) 
  call wrf_dm_bcast_real(turbpwcof,nt*maxvals) 

  end subroutine init_module_wind_mav

#endif
END MODULE module_wind_mav
