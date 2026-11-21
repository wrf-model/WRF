! Standalone full-module regression for WRF green-roof/BEM heat transfer.
! The runner compiles the actual WRF modules and tables. Synthetic forcing is
! deliberately used: this is not a Noah/Noah-MP integration or WRF restart test.
program urban_greenroof_regression
  use, intrinsic :: iso_fortran_env, only: real64, output_unit, error_unit
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
  use module_sf_urban, only: urban, urban_param_init, &
    CH_SCHEME_DATA, TS_SCHEME_DATA, AHOPTION, SBEMOPTION, COPOPTION, &
    ALHOPTION, GROPTION, TREEOPTION, IMP_SCHEME, IRI_SCHEME, FGR, &
    distributed_aerodynamics_option, LCZ_SWITCH_kike, DZGR, &
    PERFLO_TBL, HSESF_TBL, AC_FLOOR_RATIO_TBL, AC_USAGE_RATIO_CL_TBL, &
    AC_USAGE_RATIO_HT_TBL, AB_BUILD_RATIO_TBL, TARGTEMP_TBL, GAPTEMP_TBL, &
    TARGHUM_TBL, GAPHUM_TBL, TAUHUM_TBL, COP_TBL, VENT_FREQ_TBL, ALH_TBL, &
    AH_TRAFFIC_TBL, ROOF_WIDTH_kike, ROAD_WIDTH_kike, ZR_TBL, R_TBL
#ifndef BASELINE_ONLY
  use module_sf_greenroof_bem, only: greenroof_bem_diagnostics
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

  type(urban_state) :: a,b,old,checkpoint
#ifndef BASELINE_ONLY
  type(greenroof_bem_diagnostics) :: gr_diag
#endif
  integer :: cases=0,niter,i,j,k,l,step,lcz,ncategory,unit,nrecords
  real :: residual(2),source(2),steps(4)=[6.,30.,60.,180.],fractions(3)=[0.25,0.5,1.]
  real :: time_seconds,expected,roof,road,height,old_green(4),onevector(state_size),twovector(state_size)
  real(real64) :: max_solver,max_surface,max_storage,max_cell,roundbound,stored_storage,cell_resid(4),qin(4),qout(4),rate(4)
  real(real64) :: qmixc,qmixr,indoor_flux_split,roundk(4),max_bem_flux_mismatch,max_roundbound,max_roundratio
  character(len=80) :: mode,table_mode,label
  call get_command_argument(1,mode)
  call get_command_argument(2,table_mode)
  lcz=0
  if(trim(table_mode)=='lcz') lcz=1
  call fixture(a)
  call urban_param_init(a%dzr,a%dzb,a%dzg,4,1,lcz,.false.)
  LCZ_SWITCH_kike=lcz
  ncategory=3
  if(lcz==1) ncategory=10
  max_solver=0;max_surface=0;max_storage=0;max_cell=0;max_bem_flux_mismatch=0;max_roundbound=0;max_roundratio=0

  if(index(trim(mode),'snapshot_')==1) then
    open(newunit=unit,file='regression_state.bin',access='stream',form='unformatted',status='replace')
    nrecords=0
    do i=1,ncategory
      do j=0,1
        call reset_parameters()
        call fixture(a)
        a%utype=i
        a%rain=real(j)
        if(trim(mode)=='snapshot_disabled') then
          GROPTION=0; SBEMOPTION=j
        else if(trim(mode)=='snapshot_nonbem_green') then
          GROPTION=1;SBEMOPTION=0;FGR=0.5
        else if(trim(mode)=='snapshot_fgr0') then
          GROPTION=1;SBEMOPTION=1;FGR=0.0
        else
          error stop 'unknown snapshot mode'
        endif
        do step=1,12
          a%itimestep=step
          call evaluate(a)
          call finite_state('reference_snapshot',a)
          write(unit) state_vector(a)
          nrecords=nrecords+1
        enddo
      enddo
    enddo
    close(unit)
    write(output_unit,'(A,I0,A,I0)') 'WRF_GREENROOF_SNAPSHOT_PASS records=',nrecords,' values=',state_size
    stop
  endif
#ifdef BASELINE_ONLY
  error stop 'baseline executable only supports snapshot modes'
#else
  call reset_parameters()
  call fixture(a)
  GROPTION=1;FGR=0.5
  if(trim(mode)=='invalid_fgr') then
    FGR=1.01
  else if(trim(mode)=='invalid_dz') then
    DZGR(4)=0.0
  else if(trim(mode)=='invalid_nonfinite_fgr') then
    FGR=ieee_value(0.0,ieee_quiet_nan)
  else if(trim(mode)=='invalid_distributed') then
    distributed_aerodynamics_option=.true.
    call distributed_geometry(a)
  else if(trim(mode)/='suite') then
    error stop 'unknown mode'
  endif
  if(trim(mode)/='suite') then
    call evaluate(a)
    error stop 'UNEXPECTED_INVALID_ACCEPTANCE'
  endif

  ! Existing zero-area roof tile must not alter active building/atmosphere
  ! results. Ignore only auxiliary states/transfer coefficients of that tile.
  call reset_parameters()
  call fixture(a)
  b=a
  call evaluate(a)
  if(gr_diag%active) call fail('disabled_diag','unexpected active diagnostic')
  GROPTION=1;FGR=0.0
  call evaluate(b)
  if(gr_diag%active) call fail('zero_area_diag','unexpected active diagnostic')
  onevector=state_vector(a);twovector=state_vector(b)
  ! Zero only the known green-roof auxiliary state entries, not BEM outputs.
  a%cmcr=b%cmcr;a%tgr=b%tgr;a%tgrl=b%tgrl;a%smr=b%smr
  a%cmgr_urb=b%cmgr_urb;a%chgr_urb=b%chgr_urb
  if(any(state_vector(a)/=state_vector(b))) call fail('zero_area_bypass','non-green state changed')
  call mark('zero_area_bypass')

  ! Selected host timesteps and all table categories, including both thermal
  ! directions at the room interface and periods with precipitation/dew.
  do i=1,ncategory
    do j=1,size(steps)
      do k=1,size(fractions)
        call reset_parameters()
        call fixture(a)
        GROPTION=1;FGR=fractions(k);a%delt=steps(j);a%utype=i
        AC_FLOOR_RATIO_TBL=0.6
        if(mod(i+k,2)==0) a%tin_urb=299.0
        if(k==3) then
          a%qa=0.025;a%ta=298.0;a%tgr=294.0;a%tgrl=294.0
        endif
        write(label,'(A,I0,A,I0,A,I0)') 'category',i,'_dt',nint(steps(j)),'_f',nint(100*fractions(k))
        do step=1,24
          a%rain=0.0
          if(mod(step,4)==0) a%rain=1.0
          old=a
          call evaluate(a,niter,residual,source)
          call green_checks(label,old,a)
        enddo
        call mark(label)
      enddo
    enddo
  enddo

  ! 72-hour prognostic continuation with smooth synthetic diurnal forcing.
  call reset_parameters()
  call fixture(a)
  GROPTION=1;FGR=0.5;AC_FLOOR_RATIO_TBL=0.6
  do step=1,4320
    time_seconds=real(step-1)*60.0
    a%omg=2.0*acos(-1.0)*(modulo(time_seconds,86400.0)/86400.0-0.5)
    a%ssg=max(0.0,750.0*cos(a%omg))
    a%ssgd=0.7*a%ssg;a%ssgq=0.3*a%ssg
    a%cosz=max(0.0,cos(a%omg))
    a%ta=299.0+5.0*cos(a%omg-0.5)
    a%qa=0.012+0.002*cos(a%omg)
    a%rain=0.0
    if(step>=1400.and.step<1460) a%rain=2.0
    a%itimestep=step
    old=a
    call evaluate(a,niter,residual,source)
    call green_checks('72hour',old,a)
    if(step==2160) then
      open(newunit=unit,file='greenroof_checkpoint.bin',access='stream',form='unformatted',status='replace')
      write(unit) a
      close(unit)
      checkpoint=a
    endif
  enddo
  call mark('72hour_evolution')

  ! Check no hidden state is introduced: same one-step continuation after
  ! state serialization, another cell, and an unrelated timestep counter.
  a=checkpoint
  call evaluate(a)
  call fixture(b);b%tin_urb=306.;b%tgrl=299.
  call evaluate(b)
  open(newunit=unit,file='greenroof_checkpoint.bin',access='stream',form='unformatted',status='old')
  read(unit) b
  close(unit)
  b%itimestep=1
  call evaluate(b)
  if(any(state_vector(a)/=state_vector(b))) call fail('greenroof_replay','serialized continuation differs')
  call mark('greenroof_state_serialization_replay')
  write(output_unit,'(A,7ES24.15)') 'GREENROOF_METRICS ',max_solver,max_surface,max_storage,max_cell,max_bem_flux_mismatch,max_roundbound,max_roundratio
  write(output_unit,'(A,I0)') 'WRF_GREENROOF_NATIVE_PASS cases=',cases
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
      , GREENROOF_DIAG=gr_diag &
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
  subroutine green_checks(label,previous,current)
    character(len=*),intent(in) :: label
    type(urban_state),intent(in) :: previous,current
    real(real64) :: expected_flux,actual_flux,tolerance,expected_load,actual_load
    integer :: cell
    call finite_state(label,current)
    if(.not.gr_diag%active) call fail(label,'green diagnostic inactive')
    if(gr_diag%fraction/=real(FGR,real64)) call fail(label,'wrong green fraction')
    rate=gr_diag%layer_capacity_jm3k*real(DZGR,real64)/real(previous%delt,real64)
    stored_storage=sum(rate*(real(current%tgrl,real64)-real(previous%tgrl,real64)))
    ! The saved old temperatures are exactly represented float32 inputs.
    ! Only rounding the new double solution back to float32 contributes here.
    roundk=0.5_real64*real(spacing(current%tgrl),real64)
    roundbound=sum(rate*roundk)+1.e-6_real64
    actual_flux=gr_diag%surface_to_column_wm2-gr_diag%roof_to_indoor_wm2-stored_storage
    if(abs(actual_flux)>roundbound) call fail(label,'column heat storage outside rounding bound')
    if(abs(actual_flux-gr_diag%column_residual_wm2)>1.e-7_real64) call fail(label,'wrong reported storage residual')
    if(abs(gr_diag%solver_residual_wm2)>1.e-6_real64) call fail(label,'double column solver does not conserve energy')
    if(abs(gr_diag%surface_residual_wm2)>0.01001_real64) call fail(label,'surface energy iteration not converged')
    qin=[gr_diag%surface_to_column_wm2,gr_diag%layer_interface_wm2]
    qout=[gr_diag%layer_interface_wm2,gr_diag%roof_to_indoor_wm2]
    cell_resid=qin-qout-rate*(real(current%tgrl,real64)-real(previous%tgrl,real64))
    do cell=1,4
      if(abs(cell_resid(cell))>rate(cell)*roundk(cell)+1.e-6_real64) call fail(label,'individual cell energy imbalance')
    enddo
    indoor_flux_split=gr_diag%roof_conv_wm2+gr_diag%roof_rad_wm2
    if(abs(indoor_flux_split-gr_diag%roof_to_indoor_wm2)>1.e-9_real64) call fail(label,'room flux counted inconsistently')
    qmixc=(1.-real(FGR,real64))*gr_diag%bare_roof_conv_wm2+real(FGR,real64)*gr_diag%roof_conv_wm2
    qmixr=(1.-real(FGR,real64))*gr_diag%bare_roof_rad_wm2+real(FGR,real64)*gr_diag%roof_rad_wm2
    tolerance=3.e-6_real64*max(1._real64,abs(qmixc),abs(qmixr))
    if(abs(real(current%qrcht,real64)-qmixc)>tolerance.or.abs(real(current%qrrht,real64)-qmixr)>tolerance) &
      call fail(label,'actual BEM argument lacks correct roof area weighting')
    ! No people/appliance baseload in this fixture: independent BEM RHS.
    roof=ROOF_WIDTH_kike(current%utype);height=ZR_TBL(current%utype)
    expected_load=real(current%qvent,real64)+real(roof,real64)*(qmixc+qmixr) &
      +2._real64*real(height,real64)*(real(current%qbcht,real64)+real(current%qbrht,real64))
    actual_load=real(current%hin_urb,real64)
    tolerance=1.e-3_real64+5.e-6_real64*abs(expected_load)
    if(abs(actual_load-expected_load)>tolerance) call fail(label,'mixed roof heat did not reach indoor load')
    max_solver=max(max_solver,abs(gr_diag%solver_residual_wm2))
    max_surface=max(max_surface,abs(gr_diag%surface_residual_wm2))
    max_storage=max(max_storage,abs(actual_flux))
    max_roundbound=max(max_roundbound,roundbound)
    max_roundratio=max(max_roundratio,abs(actual_flux)/roundbound)
    max_cell=max(max_cell,maxval(abs(cell_resid)))
    max_bem_flux_mismatch=max(max_bem_flux_mismatch,abs(actual_load-expected_load))
  end subroutine green_checks
#endif
end program urban_greenroof_regression
