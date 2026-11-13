! Standalone full-module regression for the WRF SLUCM+BEM exchange repair.
! The runner compiles the actual WRF modules and tables. Synthetic forcing is
! deliberately used: this is not a Noah/Noah-MP integration or WRF restart test.
program urban_exchange_regression
  use, intrinsic :: iso_fortran_env, only: real64, output_unit, error_unit
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
  use module_sf_urban, only: urban, urban_param_init, &
    CH_SCHEME_DATA, TS_SCHEME_DATA, AHOPTION, SBEMOPTION, COPOPTION, &
    ALHOPTION, GROPTION, TREEOPTION, IMP_SCHEME, IRI_SCHEME, FGR, &
    distributed_aerodynamics_option, LCZ_SWITCH_kike, &
    PERFLO_TBL, HSESF_TBL, AC_FLOOR_RATIO_TBL, AC_USAGE_RATIO_CL_TBL, &
    AC_USAGE_RATIO_HT_TBL, AB_BUILD_RATIO_TBL, TARGTEMP_TBL, GAPTEMP_TBL, &
    TARGHUM_TBL, GAPHUM_TBL, TAUHUM_TBL, COP_TBL, VENT_FREQ_TBL, ALH_TBL, &
    AH_TRAFFIC_TBL, ROOF_WIDTH_kike, ROAD_WIDTH_kike, ZR_TBL, R_TBL
#ifndef BASELINE_ONLY
  use module_sf_urban, only: urban_step
#endif
  implicit none
  real, parameter :: cp_air=1004.5, lv_air=2.442e6, room_capacity=20000.0
  type :: urban_state
    logical :: lsolar=.false.
    integer :: num_roof_layers=4
    integer :: num_wall_layers=4
    integer :: num_road_layers=4
    real :: dzr(4)=0.0
    real :: dzb(4)=0.0
    real :: dzg(4)=0.0
    integer :: utype=1
    real :: ta=0.0
    real :: qa=0.0
    real :: ua=0.0
    real :: u1=0.0
    real :: v1=0.0
    real :: ssg=0.0
    real :: ssgd=0.0
    real :: ssgq=0.0
    real :: llg=0.0
    real :: rain=0.0
    real :: rhoo=0.0
    real :: za=0.0
    real :: declin=0.0
    real :: cosz=0.0
    real :: omg=0.0
    real :: xlat=0.0
    real :: delt=0.0
    real :: znt=0.0
    real :: chs=0.0
    real :: chs2=0.0
    real :: tr=0.0
    real :: tb=0.0
    real :: tg=0.0
    real :: tc=0.0
    real :: qc=0.0
    real :: uc=0.0
    real :: trl(4)=0.0
    real :: tbl(4)=0.0
    real :: tgl(4)=0.0
    real :: xxxr=0.0
    real :: xxxb=0.0
    real :: xxxg=0.0
    real :: xxxc=0.0
    real :: ts=0.0
    real :: qs=0.0
    real :: sh=0.0
    real :: lh=0.0
    real :: lh_kinematic=0.0
    real :: sw=0.0
    real :: alb=0.0
    real :: lw=0.0
    real :: g=0.0
    real :: rn=0.0
    real :: psim=0.0
    real :: psih=0.0
    real :: gz1oz0=0.0
    real :: cmr_urb=0.0
    real :: chr_urb=0.0
    real :: cmc_urb=0.0
    real :: chc_urb=0.0
    real :: u10=0.0
    real :: v10=0.0
    real :: th2=0.0
    real :: q2=0.0
    real :: ust=0.0
    real :: mh_urb=0.0
    real :: stdh_urb=0.0
    real :: lf_urb(4)=0.0
    real :: lp_urb=0.0
    real :: hgt_urb=0.0
    real :: frc_urb=0.0
    real :: lb_urb=0.0
    real :: zo_check=0.0
    real :: cmcr=0.0
    real :: tgr=0.0
    real :: tgrl(4)=0.0
    real :: smr(4)=0.0
    real :: cmgr_urb=0.0
    real :: chgr_urb=0.0
    integer :: jmonth=7
    real :: drelr=0.0
    real :: drelb=0.0
    real :: drelg=0.0
    real :: flxhumr=0.0
    real :: flxhumb=0.0
    real :: flxhumg=0.0
    real :: tvg=0.0
    real :: tt=0.0
    real :: xxxvg=0.0
    real :: tvgl(4)=0.0
    real :: smg(4)=0.0
    real :: cmcg=0.0
    real :: flxhumvg=0.0
    real :: flxhumt=0.0
    real :: lf_urb_s=0.0
    real :: z0_urb=0.0
    real :: vegfrac_in=0.0
    real :: qf_urb=0.0
    real :: qfb_urb=0.0
    real :: qfb_cl_urb=0.0
    real :: qfb_ht_urb=0.0
    real :: qfl_urb=0.0
    real :: qfbl_urb=0.0
    real :: ecac_urb=0.0
    real :: ec_urb=0.0
    real :: cop_urb=0.0
    real :: tin_urb=0.0
    real :: hin_urb=0.0
    real :: hout_urb=0.0
    real :: twin_urb=0.0
    real :: tgin_urb=0.0
    real :: wbgtin_urb=0.0
    real :: mrtin_urb=0.0
    real :: qbcht=0.0
    real :: qbrht=0.0
    real :: qrcht=0.0
    real :: qrrht=0.0
    real :: qvent=0.0
    real :: qin_urb=0.0
    real :: event=0.0
    real :: ein_urb=0.0
    real :: eout_urb=0.0
    integer :: itimestep=1
  end type urban_state
  integer, parameter :: state_size=147
  type(urban_state) :: a,b,c,d,initial,checkpoint
  integer :: cases=0, niter, i, j, k, ncategory, lcz, unit, nrecords, signcase
  real :: residual(2), source(2), steps(4)=[6.0,30.0,60.0,180.0]
  real :: humidity_values(3)=[0.0,0.00005,0.0001]
  real :: net(2), old_source(2), delta, widths(2), height
  character(len=80) :: mode, table_mode, label

  call get_command_argument(1,mode)
  call get_command_argument(2,table_mode)
  lcz=0
  if(trim(table_mode)=='lcz') lcz=1
  call fixture(a)
  call urban_param_init(a%dzr,a%dzb,a%dzg,4,1,lcz,.false.)
  LCZ_SWITCH_kike=lcz
  ncategory=3
  if(lcz==1) ncategory=10

  if(trim(mode)=='sbem0'.or.trim(mode)=='zeroexchange') then
    open(newunit=unit,file='regression_state.bin',access='stream',form='unformatted',status='replace')
    nrecords=0
    do i=1,ncategory
      do j=1,2
        call reset_parameters()
        if(trim(mode)=='sbem0') then
          SBEMOPTION=0
          TS_SCHEME_DATA=j
        else
          VENT_FREQ_TBL=0.0
          if(j==2) IMP_SCHEME=2
        endif
        call fixture(a)
        a%utype=i
        if(j==2) then
          a%rain=2.0
          a%drelr=0.0002; a%drelb=0.0001; a%drelg=0.0005
        endif
        call evaluate(a)
        call finite_state('reference_snapshot',a)
        write(unit) state_vector(a)
        nrecords=nrecords+1
      enddo
    enddo
    call reset_parameters()
    if(trim(mode)=='sbem0') SBEMOPTION=0
    VENT_FREQ_TBL=0.0
    distributed_aerodynamics_option=.true.
    call fixture(a)
    call distributed_geometry(a)
    call evaluate(a)
    call finite_state('distributed_reference_snapshot',a)
    write(unit) state_vector(a)
    nrecords=nrecords+1
    close(unit)
    write(output_unit,'(A,I0,A,I0)') 'WRF_URBAN_SNAPSHOT_PASS records=',nrecords,' values=',state_size
    stop
  endif
#ifdef BASELINE_ONLY
  error stop 'baseline executable only supports snapshot modes'
#else
  call reset_parameters()
  call fixture(a)
  if(trim(mode)=='invalid_negative_qin') then
    a%qin_urb=-0.001
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  else if(trim(mode)=='invalid_nonfinite_qin') then
    a%qin_urb=ieee_value(0.0,ieee_quiet_nan)
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  else if(trim(mode)=='invalid_allroof_canyon') then
    distributed_aerodynamics_option=.true.
    call distributed_geometry(a)
    a%lp_urb=1.0
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  else if(trim(mode)=='invalid_predicted_qin') then
    VENT_FREQ_TBL=0.0; PERFLO_TBL=1.0
    a%qin_urb=0.09999; a%delt=180.0
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  else if(trim(mode)=='invalid_predicted_tin') then
    VENT_FREQ_TBL=0.0; HSESF_TBL=100.0
    a%tin_urb=349.99; a%trl(4)=a%tin_urb; a%tbl(4)=a%tin_urb; a%delt=180.0
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  else if(len_trim(mode)>0.and.trim(mode)/='suite') then
    error stop 'unknown test mode'
  endif

  ! Time steps span the nested-domain values used in WRF, without ILS hourly
  ! subcycling. Each accepted trial must reconstruct exactly from its entry state.
  do i=1,ncategory
    do j=1,size(steps)
      call reset_parameters()
      call fixture(a)
      a%utype=i; a%delt=steps(j)
      initial=a
      call evaluate(a,niter,residual,source)
      write(label,'(A,I0,A,I0)') 'canopy_type_',i,'_dt_',int(steps(j))
      call check_coupled(trim(label),initial,a,niter,residual,source)
      call replay_trial(trim(label),initial,a,source)
      call dry_flux_closure(trim(label),a)
      call mark(trim(label))
    enddo
  enddo

  ! HVAC-off ventilation cools/dries or warms/moistens the outdoor canyon
  ! according to the sign of the actual indoor-outdoor difference.
  do signcase=1,2
    call reset_parameters()
    call fixture(initial)
    if(signcase==2) then
      initial%ta=295.0; initial%qa=0.005
      initial%tc=296.0; initial%qc=0.006
      initial%tin_urb=315.0; initial%qin_urb=0.025
    endif
    a=initial; VENT_FREQ_TBL=0.0
    call evaluate(a)
    b=initial; VENT_FREQ_TBL=3.0
    call evaluate(b,niter,residual,source)
    write(label,'(A,I0)') 'exchange_sign_',signcase
    call check_coupled(trim(label),initial,b,niter,residual,source)
    call replay_trial(trim(label),initial,b,source)
    call dry_flux_closure(trim(label),b)
    if(signcase==1) then
      if(b%qvent<=0.0.or.b%event<=0.0.or.b%tc>=a%tc.or.b%qc>=a%qc) &
        call fail(trim(label),'missing outdoor cooling/drying response')
    else
      if(b%qvent>=0.0.or.b%event>=0.0.or.b%tc<=a%tc.or.b%qc<=a%qc) &
        call fail(trim(label),'missing outdoor warming/moistening response')
    endif
    if(b%qfb_urb/=0.0.or.b%qfbl_urb/=0.0.or.b%ecac_urb/=0.0) &
      call fail(trim(label),'HVAC-off equipment diagnostics must be zero')
    call mark(trim(label))
  enddo

  do i=1,size(humidity_values)
    call reset_parameters()
    VENT_FREQ_TBL=0.0
    call fixture(initial)
    initial%qin_urb=humidity_values(i)
    a=initial
    call evaluate(a,niter,residual,source)
    write(label,'(A,I0)') 'valid_dry_indoor_state_',i
    if(a%qin_urb/=initial%qin_urb) call fail(trim(label),'artificial humidity source')
    call check_coupled(trim(label),initial,a,niter,residual,source)
    call replay_trial(trim(label),initial,a,source)
    call mark(trim(label))
  enddo

  ! Partial operating area and cooling-tower latent rejection retain gross
  ! machine diagnostics. No extra ACA or tile-fraction multiplier is applied.
  call reset_parameters()
  AC_FLOOR_RATIO_TBL=0.4
  PERFLO_TBL=0.05; HSESF_TBL=1.0
  call fixture(initial)
  initial%utype=3
  if(lcz==1) initial%utype=1
  initial%tin_urb=305.0; initial%qin_urb=0.020
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('partial_ac_latent_equipment',initial,a,niter,residual,source)
  call replay_trial('partial_ac_latent_equipment',initial,a,source)
  call dry_flux_closure('partial_ac_latent_equipment',a)
  if(a%qfbl_urb<=0.0.or.a%ecac_urb<=0.0) call fail('partial_ac_latent_equipment','inactive test fixture')
  call near('partial_ac_latent_equipment','gross sensible diagnostic',a%qf_urb,a%qfb_urb,1.0e-5)
  call near('partial_ac_latent_equipment','gross latent diagnostic',a%qfl_urb,a%qfbl_urb,1.0e-5)
  call mark('partial_ac_latent_equipment')

  call reset_parameters()
  call fixture(initial)
  a=initial; a%frc_urb=0.3
  b=initial; b%frc_urb=0.9
  call evaluate(a)
  call evaluate(b)
  call near('no_repeated_tile_fraction','SH',a%sh,b%sh,1.0e-6)
  call near('no_repeated_tile_fraction','LH',a%lh,b%lh,1.0e-6)
  call near('no_repeated_tile_fraction','TC',a%tc,b%tc,1.0e-6)
  call near('no_repeated_tile_fraction','QC',a%qc,b%qc,1.0e-10)
  call mark('no_repeated_tile_fraction')

  ! Restoring the full entry state must prevent repeatedly applying one step
  ! of rain storage, moisture extraction, diffusion and indoor evolution.
  call reset_parameters(); IMP_SCHEME=2
  call fixture(initial)
  initial%rain=4.0
  initial%drelr=0.0002; initial%drelb=0.0001; initial%drelg=0.0004
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('wet_trial_state_restore',initial,a,niter,residual,source)
  call replay_trial('wet_trial_state_restore',initial,a,source)
  call mark('wet_trial_state_restore')

  call reset_parameters()
  call fixture(initial)
  initial%qf_urb=100000.0; initial%qfl_urb=-100000.0
  a=initial
  call evaluate(a)
  initial%qf_urb=-100000.0; initial%qfl_urb=100000.0
  b=initial
  call evaluate(b)
  call near('no_old_qf_forcing','TC',a%tc,b%tc,0.002)
  call near('no_old_qf_forcing','QC',a%qc,b%qc,2.0e-7)
  call near('no_old_qf_forcing','SH',a%sh,b%sh,0.03)
  call near('no_old_qf_forcing','LH',a%lh,b%lh,0.03)
  call mark('no_old_qf_forcing')

  call reset_parameters()
  distributed_aerodynamics_option=.true.
  AC_FLOOR_RATIO_TBL=0.35
  call fixture(initial)
  call distributed_geometry(initial)
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('distributed_local_geometry',initial,a,niter,residual,source)
  call replay_trial('distributed_local_geometry',initial,a,source)
  call mark('distributed_local_geometry')

  call reset_parameters(); GROPTION=1; FGR=0.25; IMP_SCHEME=2
  call fixture(initial)
  initial%rain=2.0; initial%cmcr=0.0001
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('greenroof_water_state_restore',initial,a,niter,residual,source)
  call replay_trial('greenroof_water_state_restore',initial,a,source)
  call mark('greenroof_water_state_restore')

  call reset_parameters(); CH_SCHEME_DATA=1
  call fixture(initial)
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('monin_obukhov_canyon_exchange',initial,a,niter,residual,source)
  call replay_trial('monin_obukhov_canyon_exchange',initial,a,source)
  call dry_flux_closure('monin_obukhov_canyon_exchange',a)
  call mark('monin_obukhov_canyon_exchange')

  call reset_parameters()
  call fixture(initial)
  initial%jmonth=1; initial%ta=280.0; initial%qa=0.003
  initial%tc=282.0; initial%qc=0.004
  initial%tin_urb=293.0; initial%qin_urb=0.009
  initial%tr=285.0; initial%tb=286.0; initial%tg=284.0
  initial%trl=289.0; initial%tbl=289.0; initial%tgl=284.0
  initial%ssg=150.0; initial%llg=340.0
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('winter_hvac_off_exchange',initial,a,niter,residual,source)
  call replay_trial('winter_hvac_off_exchange',initial,a,source)
  call dry_flux_closure('winter_hvac_off_exchange',a)
  if(a%qvent>=0.0.or.a%event>=0.0) call fail('winter_hvac_off_exchange','wrong exchange direction')
  if(a%qfb_urb/=0.0.or.a%qfbl_urb/=0.0) call fail('winter_hvac_off_exchange','HVAC must remain off')
  call mark('winter_hvac_off_exchange')

  call reset_parameters()
  call fixture(initial)
  AC_FLOOR_RATIO_TBL=0.5; HSESF_TBL=20.0
  initial%delt=180.0
  initial%tin_urb=TARGTEMP_TBL(1)+GAPTEMP_TBL(1)-0.001
  initial%qin_urb=TARGHUM_TBL(1)+GAPHUM_TBL(1)-1.0e-6
  a=initial
  call evaluate(a,niter,residual,source)
  call check_coupled('coupled_control_threshold_crossing',initial,a,niter,residual,source)
  call replay_trial('coupled_control_threshold_crossing',initial,a,source)
  call dry_flux_closure('coupled_control_threshold_crossing',a)
  if(a%hout_urb<=0.0.or.a%eout_urb<=0.0) call fail('coupled_control_threshold_crossing','threshold not crossed')
  call mark('coupled_control_threshold_crossing')

  ! External traffic/ALH keep their pre-existing direct atmospheric routing;
  ! inserting the building exchange must not count those sources twice.
  call reset_parameters()
  call fixture(initial)
  a=initial
  call evaluate(a)
  AH_TRAFFIC_TBL=10.0; ALH_TBL=20.0; ALHOPTION=1
  b=initial
  call evaluate(b)
  call near('external_source_routing','TC',a%tc,b%tc,1.0e-6)
  call near('external_source_routing','QC',a%qc,b%qc,1.0e-10)
  call near('external_source_routing','SH source',b%sh-a%sh,b%qf_urb-a%qf_urb,0.001)
  call near('external_source_routing','LH source',b%lh-a%lh,b%qfl_urb-a%qfl_urb,0.001)
  call mark('external_source_routing')

  ! Serialization/replay of this module's call state, including an intervening
  ! unrelated cell. This does not exercise WRF restart I/O or MPI halo updates.
  call reset_parameters()
  call fixture(a)
  call evaluate(a)
  checkpoint=a
  open(newunit=unit,file='urban_state_checkpoint.bin',access='stream',form='unformatted',status='replace')
  write(unit) checkpoint
  close(unit)
  a%itimestep=2
  call evaluate(a)
  call fixture(c)
  c%tin_urb=310.0; c%qin_urb=0.025; c%rain=1.0
  call evaluate(c)
  open(newunit=unit,file='urban_state_checkpoint.bin',access='stream',form='unformatted',status='old')
  read(unit) b
  close(unit)
  b%itimestep=1
  call evaluate(b)
  if(any(state_vector(a)/=state_vector(b))) call fail('state_serialization_replay','call state depends on history or step counter')
  call mark('state_serialization_replay')

  write(output_unit,'(A,I0)') 'WRF_URBAN_EXCHANGE_TEST_PASS cases=',cases
#endif
contains
  subroutine fixture(s)
    type(urban_state), intent(out) :: s
    s=urban_state()
    s%dzr=[5.0,5.0,10.0,10.0]
    s%dzb=[5.0,5.0,10.0,10.0]
    s%dzg=[5.0,25.0,70.0,150.0]
    s%ta=303.0; s%qa=0.014; s%ua=3.0; s%u1=3.0; s%v1=0.0
    s%ssg=400.0; s%llg=420.0; s%rhoo=1.2; s%za=100.0
    s%declin=0.35; s%cosz=0.8; s%omg=0.0; s%xlat=35.0
    s%delt=60.0; s%znt=0.5; s%chs=0.02; s%chs2=0.02
    s%tr=305.0; s%tb=303.0; s%tg=307.0; s%tc=304.0; s%qc=0.014; s%uc=1.0
    s%trl=[305.0,303.0,300.0,297.0]
    s%tbl=[303.0,302.0,299.0,297.0]; s%tgl=[307.0,303.0,300.0,298.0]
    s%cmr_urb=0.01; s%chr_urb=0.01; s%cmc_urb=0.01; s%chc_urb=0.01
    s%u10=3.0; s%v10=0.0
    s%tgr=303.0; s%tgrl=300.0; s%smr=0.25
    s%cmgr_urb=0.01; s%chgr_urb=0.01
    s%tvg=303.0; s%tt=303.0; s%tvgl=300.0; s%smg=0.25
    s%tin_urb=295.0; s%qin_urb=0.007
    s%frc_urb=0.7; s%lf_urb_s=0.25; s%z0_urb=0.5; s%vegfrac_in=0.2
  end subroutine fixture

  subroutine distributed_geometry(s)
    type(urban_state), intent(inout) :: s
    s%mh_urb=10.0; s%hgt_urb=10.0; s%stdh_urb=3.0
    s%lp_urb=0.4; s%lb_urb=1.0; s%lf_urb=0.25; s%lf_urb_s=0.25
    s%z0_urb=0.5; s%vegfrac_in=0.2
  end subroutine distributed_geometry

  subroutine reset_parameters()
    CH_SCHEME_DATA=2; TS_SCHEME_DATA=1; AHOPTION=2; SBEMOPTION=1
    COPOPTION=0; ALHOPTION=0; GROPTION=0; TREEOPTION=0
    IMP_SCHEME=1; IRI_SCHEME=0; FGR=0.0
    distributed_aerodynamics_option=.false.
    PERFLO_TBL=0.0; HSESF_TBL=0.0; AC_FLOOR_RATIO_TBL=0.0
    AC_USAGE_RATIO_CL_TBL=1.0; AC_USAGE_RATIO_HT_TBL=1.0; AB_BUILD_RATIO_TBL=0.0
    TARGTEMP_TBL=298.0; GAPTEMP_TBL=1.0
    TARGHUM_TBL=0.008; GAPHUM_TBL=0.001; TAUHUM_TBL=3600.0
    COP_TBL=3.0; VENT_FREQ_TBL=1.0; ALH_TBL=0.0; AH_TRAFFIC_TBL=0.0
  end subroutine reset_parameters

  subroutine evaluate(s,niter,residual,sources)
    type(urban_state), intent(inout) :: s
    integer, optional, intent(out) :: niter
    real, optional, intent(out) :: residual(2),sources(2)
    integer :: local_iterations
    real :: local_residual(2), local_source(2)
    local_iterations=1; local_residual=0.0; local_source=0.0
    call urban( &
      lsolar=s%lsolar, num_roof_layers=s%num_roof_layers, num_wall_layers=s%num_wall_layers, &
      num_road_layers=s%num_road_layers, dzr=s%dzr, dzb=s%dzb, &
      dzg=s%dzg, utype=s%utype, ta=s%ta, &
      qa=s%qa, ua=s%ua, u1=s%u1, &
      v1=s%v1, ssg=s%ssg, ssgd=s%ssgd, &
      ssgq=s%ssgq, llg=s%llg, rain=s%rain, &
      rhoo=s%rhoo, za=s%za, declin=s%declin, &
      cosz=s%cosz, omg=s%omg, xlat=s%xlat, &
      delt=s%delt, znt=s%znt, chs=s%chs, &
      chs2=s%chs2, tr=s%tr, tb=s%tb, &
      tg=s%tg, tc=s%tc, qc=s%qc, &
      uc=s%uc, trl=s%trl, tbl=s%tbl, &
      tgl=s%tgl, xxxr=s%xxxr, xxxb=s%xxxb, &
      xxxg=s%xxxg, xxxc=s%xxxc, ts=s%ts, &
      qs=s%qs, sh=s%sh, lh=s%lh, &
      lh_kinematic=s%lh_kinematic, sw=s%sw, alb=s%alb, &
      lw=s%lw, g=s%g, rn=s%rn, &
      psim=s%psim, psih=s%psih, gz1oz0=s%gz1oz0, &
      cmr_urb=s%cmr_urb, chr_urb=s%chr_urb, cmc_urb=s%cmc_urb, &
      chc_urb=s%chc_urb, u10=s%u10, v10=s%v10, &
      th2=s%th2, q2=s%q2, ust=s%ust, &
      mh_urb=s%mh_urb, stdh_urb=s%stdh_urb, lf_urb=s%lf_urb, &
      lp_urb=s%lp_urb, hgt_urb=s%hgt_urb, frc_urb=s%frc_urb, &
      lb_urb=s%lb_urb, zo_check=s%zo_check, cmcr=s%cmcr, &
      tgr=s%tgr, tgrl=s%tgrl, smr=s%smr, &
      cmgr_urb=s%cmgr_urb, chgr_urb=s%chgr_urb, jmonth=s%jmonth, &
      drelr=s%drelr, drelb=s%drelb, drelg=s%drelg, &
      flxhumr=s%flxhumr, flxhumb=s%flxhumb, flxhumg=s%flxhumg, &
      tvg=s%tvg, tt=s%tt, xxxvg=s%xxxvg, &
      tvgl=s%tvgl, smg=s%smg, cmcg=s%cmcg, &
      flxhumvg=s%flxhumvg, flxhumt=s%flxhumt, lf_urb_s=s%lf_urb_s, &
      z0_urb=s%z0_urb, vegfrac_in=s%vegfrac_in, qf_urb=s%qf_urb, &
      qfb_urb=s%qfb_urb, qfb_cl_urb=s%qfb_cl_urb, qfb_ht_urb=s%qfb_ht_urb, &
      qfl_urb=s%qfl_urb, qfbl_urb=s%qfbl_urb, ecac_urb=s%ecac_urb, &
      ec_urb=s%ec_urb, cop_urb=s%cop_urb, tin_urb=s%tin_urb, &
      hin_urb=s%hin_urb, hout_urb=s%hout_urb, twin_urb=s%twin_urb, &
      tgin_urb=s%tgin_urb, wbgtin_urb=s%wbgtin_urb, mrtin_urb=s%mrtin_urb, &
      qbcht=s%qbcht, qbrht=s%qbrht, qrcht=s%qrcht, &
      qrrht=s%qrrht, qvent=s%qvent, qin_urb=s%qin_urb, &
      event=s%event, ein_urb=s%ein_urb, eout_urb=s%eout_urb, &
      itimestep=s%itimestep &
#ifndef BASELINE_ONLY
      , coupling_iterations=local_iterations, coupling_residual=local_residual, coupling_source=local_source &
#endif
      )
    if(present(niter)) niter=local_iterations
    if(present(residual)) residual=local_residual
    if(present(sources)) sources=local_source
  end subroutine evaluate

  function state_vector(s) result(values)
    type(urban_state), intent(in) :: s
    real :: values(state_size)
    values=[ &
      s%dzr, s%dzb, s%dzg, s%ta, s%qa, s%ua, &
      s%u1, s%v1, s%ssg, s%ssgd, s%ssgq, s%llg, &
      s%rain, s%rhoo, s%za, s%declin, s%cosz, s%omg, &
      s%xlat, s%delt, s%znt, s%chs, s%chs2, s%tr, &
      s%tb, s%tg, s%tc, s%qc, s%uc, s%trl, &
      s%tbl, s%tgl, s%xxxr, s%xxxb, s%xxxg, s%xxxc, &
      s%ts, s%qs, s%sh, s%lh, s%lh_kinematic, s%sw, &
      s%alb, s%lw, s%g, s%rn, s%psim, s%psih, &
      s%gz1oz0, s%cmr_urb, s%chr_urb, s%cmc_urb, s%chc_urb, s%u10, &
      s%v10, s%th2, s%q2, s%ust, s%mh_urb, s%stdh_urb, &
      s%lf_urb, s%lp_urb, s%hgt_urb, s%frc_urb, s%lb_urb, s%zo_check, &
      s%cmcr, s%tgr, s%tgrl, s%smr, s%cmgr_urb, s%chgr_urb, &
      s%drelr, s%drelb, s%drelg, s%flxhumr, s%flxhumb, s%flxhumg, &
      s%tvg, s%tt, s%xxxvg, s%tvgl, s%smg, s%cmcg, &
      s%flxhumvg, s%flxhumt, s%lf_urb_s, s%z0_urb, s%vegfrac_in, s%qf_urb, &
      s%qfb_urb, s%qfb_cl_urb, s%qfb_ht_urb, s%qfl_urb, s%qfbl_urb, s%ecac_urb, &
      s%ec_urb, s%cop_urb, s%tin_urb, s%hin_urb, s%hout_urb, s%twin_urb, &
      s%tgin_urb, s%wbgtin_urb, s%mrtin_urb, s%qbcht, s%qbrht, s%qrcht, &
      s%qrrht, s%qvent, s%qin_urb, s%event, s%ein_urb, s%eout_urb]
  end function state_vector

  subroutine finite_state(label,s)
    character(len=*), intent(in) :: label
    type(urban_state), intent(in) :: s
    if(.not.all(ieee_is_finite(state_vector(s)))) call fail(label,'nonfinite physics field or continuing state')
  end subroutine finite_state

  subroutine near(label,quantity,actual,expected,tolerance)
    character(len=*), intent(in) :: label,quantity
    real, intent(in) :: actual,expected,tolerance
    if(.not.ieee_is_finite(actual).or.abs(actual-expected)>tolerance) then
      write(error_unit,'(A,1X,A,1X,A,3ES24.15)') 'DETAIL',trim(label),trim(quantity),actual,expected,tolerance
      call fail(label,quantity)
    endif
  end subroutine near

  subroutine fail(label,reason)
    character(len=*), intent(in) :: label,reason
    write(error_unit,'(A)') 'WRF_URBAN_TEST_FAIL '//trim(label)//': '//trim(reason)
    error stop 1
  end subroutine fail

  subroutine mark(label)
    character(len=*), intent(in) :: label
    cases=cases+1
    write(output_unit,'(A)') 'WRF_URBAN_CASE_PASS '//trim(label)
  end subroutine mark

#ifndef BASELINE_ONLY
  subroutine geometry(s,roof,road,height)
    type(urban_state), intent(in) :: s
    real, intent(out) :: roof,road,height
    real :: width, lp, lf
    height=ZR_TBL(s%utype)
    roof=ROOF_WIDTH_kike(s%utype); road=ROAD_WIDTH_kike(s%utype)
    if(distributed_aerodynamics_option) then
      height=max(s%mh_urb,3.5)
      lp=max(0.05,min(1.0,s%lp_urb)); lf=max(0.05,min(1.0,s%lf_urb_s))
      width=height/lf
      roof=lp*width; road=(1.0-lp)*width
    endif
  end subroutine geometry

  subroutine check_coupled(label,previous,current,niter,residual,sources)
    character(len=*), intent(in) :: label
    type(urban_state), intent(in) :: previous,current
    integer, intent(in) :: niter
    real, intent(in) :: residual(2),sources(2)
    real :: roof,road,height,net(2),budget,expected,tolerance,vent,capacity
    call finite_state(label,current)
    call geometry(previous,roof,road,height)
    if(niter<1.or.niter>80) call fail(label,'iteration count outside solver contract')
    net=[current%qfb_urb-current%qvent/(roof+road),current%qfbl_urb-current%event/(roof+road)]
    if(any(abs(net-sources)>0.01+1.0e-5*max(abs(net),abs(sources)))) call fail(label,'returned/source mismatch')
    call near(label,'reported H residual',residual(1),net(1)-sources(1),1.0e-5)
    call near(label,'reported LE residual',residual(2),net(2)-sources(2),1.0e-5)
    capacity=(cp_air*previous%rhoo+room_capacity)*height*roof
    budget=capacity*(current%tin_urb-previous%tin_urb)/previous%delt
    expected=current%hin_urb-current%hout_urb
    tolerance=3.0*epsilon(1.0)*max(abs(previous%tin_urb),abs(current%tin_urb))*capacity/previous%delt
    call near(label,'indoor sensible budget',budget,expected,tolerance)
    capacity=lv_air*previous%rhoo*height*roof
    budget=capacity*(current%qin_urb-previous%qin_urb)/previous%delt
    expected=current%ein_urb-current%eout_urb
    tolerance=max(1.0e-5,3.0*epsilon(1.0)*max(abs(previous%qin_urb),abs(current%qin_urb))*capacity/previous%delt)
    call near(label,'indoor latent budget',budget,expected,tolerance)
    vent=VENT_FREQ_TBL(previous%utype)*height*roof/3600.0
    expected=cp_air*previous%rhoo*vent*(current%tc-previous%tin_urb)
    call near(label,'current canopy sensible exchange',current%qvent,expected,max(1.0e-4,abs(expected)*3.0e-6))
    expected=lv_air*previous%rhoo*vent*(current%qc-previous%qin_urb)
    call near(label,'current canopy latent exchange',current%event,expected,max(1.0e-4,abs(expected)*3.0e-6))
    write(output_unit,'(A,1X,A,1X,I0,8ES18.9)') 'COUPLING',trim(label),niter,residual,sources,current%tc,current%qc,current%sh,current%lh
  end subroutine check_coupled

  subroutine dry_flux_closure(label,s)
    character(len=*), intent(in) :: label
    type(urban_state), intent(in) :: s
    real :: roof,road,height,expected,r,rw,tolerance
    call geometry(s,roof,road,height)
    expected=s%qfbl_urb-s%event/(roof+road)
    call near(label,'dry urban latent flux (one source)',s%lh,expected,max(0.003,abs(expected)*1.0e-5))
    if(.not.distributed_aerodynamics_option) then
      r=R_TBL(s%utype); rw=1.0-r
      expected=s%rhoo*cp_air*(r*s%chr_urb*(s%tr-s%ta)+rw*s%chc_urb*(s%tc-s%ta))
      ! Small single-precision residuals from the wall/road Newton solution
      ! are bounded here; this is an independent top-of-canyon flux balance.
      tolerance=max(0.08,abs(expected)*2.0e-4)
      call near(label,'roof plus top-of-canyon sensible flux',s%sh,expected,tolerance)
    endif
  end subroutine dry_flux_closure

  subroutine replay_trial(label,previous,accepted,sources)
    character(len=*), intent(in) :: label
    type(urban_state), intent(in) :: previous,accepted
    real, intent(in) :: sources(2)
    type(urban_state) :: s
    real :: returned(2)
    logical :: valid
    s=previous
    call urban_step( &
      lsolar=s%lsolar, num_roof_layers=s%num_roof_layers, num_wall_layers=s%num_wall_layers, &
      num_road_layers=s%num_road_layers, dzr=s%dzr, dzb=s%dzb, &
      dzg=s%dzg, utype=s%utype, ta=s%ta, &
      qa=s%qa, ua=s%ua, u1=s%u1, &
      v1=s%v1, ssg=s%ssg, ssgd=s%ssgd, &
      ssgq=s%ssgq, llg=s%llg, rain=s%rain, &
      rhoo=s%rhoo, za=s%za, declin=s%declin, &
      cosz=s%cosz, omg=s%omg, xlat=s%xlat, &
      delt=s%delt, znt=s%znt, chs=s%chs, &
      chs2=s%chs2, tr=s%tr, tb=s%tb, &
      tg=s%tg, tc=s%tc, qc=s%qc, &
      uc=s%uc, trl=s%trl, tbl=s%tbl, &
      tgl=s%tgl, xxxr=s%xxxr, xxxb=s%xxxb, &
      xxxg=s%xxxg, xxxc=s%xxxc, ts=s%ts, &
      qs=s%qs, sh=s%sh, lh=s%lh, &
      lh_kinematic=s%lh_kinematic, sw=s%sw, alb=s%alb, &
      lw=s%lw, g=s%g, rn=s%rn, &
      psim=s%psim, psih=s%psih, gz1oz0=s%gz1oz0, &
      cmr_urb=s%cmr_urb, chr_urb=s%chr_urb, cmc_urb=s%cmc_urb, &
      chc_urb=s%chc_urb, u10=s%u10, v10=s%v10, &
      th2=s%th2, q2=s%q2, ust=s%ust, &
      mh_urb=s%mh_urb, stdh_urb=s%stdh_urb, lf_urb=s%lf_urb, &
      lp_urb=s%lp_urb, hgt_urb=s%hgt_urb, frc_urb=s%frc_urb, &
      lb_urb=s%lb_urb, zo_check=s%zo_check, cmcr=s%cmcr, &
      tgr=s%tgr, tgrl=s%tgrl, smr=s%smr, &
      cmgr_urb=s%cmgr_urb, chgr_urb=s%chgr_urb, jmonth=s%jmonth, &
      drelr=s%drelr, drelb=s%drelb, drelg=s%drelg, &
      flxhumr=s%flxhumr, flxhumb=s%flxhumb, flxhumg=s%flxhumg, &
      tvg=s%tvg, tt=s%tt, xxxvg=s%xxxvg, &
      tvgl=s%tvgl, smg=s%smg, cmcg=s%cmcg, &
      flxhumvg=s%flxhumvg, flxhumt=s%flxhumt, lf_urb_s=s%lf_urb_s, &
      z0_urb=s%z0_urb, vegfrac_in=s%vegfrac_in, qf_urb=s%qf_urb, &
      qfb_urb=s%qfb_urb, qfb_cl_urb=s%qfb_cl_urb, qfb_ht_urb=s%qfb_ht_urb, &
      qfl_urb=s%qfl_urb, qfbl_urb=s%qfbl_urb, ecac_urb=s%ecac_urb, &
      ec_urb=s%ec_urb, cop_urb=s%cop_urb, tin_urb=s%tin_urb, &
      hin_urb=s%hin_urb, hout_urb=s%hout_urb, twin_urb=s%twin_urb, &
      tgin_urb=s%tgin_urb, wbgtin_urb=s%wbgtin_urb, mrtin_urb=s%mrtin_urb, &
      qbcht=s%qbcht, qbrht=s%qbrht, qrcht=s%qrcht, &
      qrrht=s%qrrht, qvent=s%qvent, qin_urb=s%qin_urb, &
      event=s%event, ein_urb=s%ein_urb, eout_urb=s%eout_urb, &
      itimestep=s%itimestep &
      ,NET_H_SOURCE=sources(1),NET_LE_SOURCE=sources(2),COUPLING_VALID=valid,NET_EXCHANGE=returned)
    if(.not.valid) call fail(label,'accepted trial cannot be reevaluated')
    if(any(state_vector(s)/=state_vector(accepted))) then
      write(error_unit,'(A,ES18.9)') 'MAX_REPLAY_DIFFERENCE ',maxval(abs(state_vector(s)-state_vector(accepted)))
      call fail(label,'accepted trial advanced a continuing state more than once')
    endif
  end subroutine replay_trial
#endif
end program urban_exchange_regression
