! Native tests of the actual WRF BEM, with a separately compiled PR reference.
! Changed paths use independent storage/HVAC identities and threshold oracles;
! unchanged paths require exact equality of every returned field.
program bem_controls_regression
  use, intrinsic :: iso_fortran_env, only: real64, error_unit
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
  use module_sf_slucm_bem, only: slucm_bem_calculate
  use module_sf_slucm_bem_reference, only: reference_calculate => slucm_bem_calculate
  implicit none

  type :: inputs
    integer :: urban_type=2, month=7, hour=14, cop_option=0, lcz=1
    real :: perflo=0.0, heat_person=0.0, latent_person=0.0
    real :: appliance=0.0, latent_appliance=0.0
    real :: height=12.0, floor_height=3.0, cp=1004.5, ell=2.5e6
    real :: rho=1.2, rmf=20000.0, ventilation=0.0
    real :: canyon_temp=300.0, canyon_q=0.010
    real :: indoor_temp=300.0, indoor_q=0.010, pressure=101325.0
    real :: roof_conv=0.0, roof_rad=0.0, wall_conv=0.0, wall_rad=0.0
    real :: cop=3.0, latitude=35.0, abandoned=0.0
    real :: heating_usage=1.0, cooling_usage=1.0
    real :: target_temp=300.0, gap_temp=0.0, target_q=0.010, gap_q=0.0
    real :: tau_q=3600.0, dt=60.0, roof_width=10.0, road_width=15.0
    real :: ac_floor(24)=1.0, roof_temp=300.0, wall_temp=300.0
  end type inputs

  type :: outputs
    real :: qvent, event, hin, ein, cop, qfb, qfb_ht, qfb_cl, qfbl
    real :: hout, eout, tin, qin, mrt, globe, wetbulb, wbgt, ecac, ec
  end type outputs

  type(inputs) :: a, b
  type(outputs) :: x, y
  real(real64) :: heat_capacity, moisture_capacity, volume, area, floor_area
  real(real64) :: free_end, expected, onset_time, enabled_time, fraction
  real :: humidities(4), fractions(3), eps_t, eps_q
  integer :: cases=0, k, j
  logical :: valid
  character(len=64) :: mode
  character(len=80) :: label

  call get_command_argument(1,mode)
  if (len_trim(mode)>0) then
    call invalid_case(trim(mode),a)
    ! Omitting the optional flag must stop, so old standalone callers cannot
    ! inadvertently accept an invalid state. The harness expects nonzero exit.
    call candidate(a,x)
    write(error_unit,'(A)') 'UNEXPECTED_INVALID_ACCEPTANCE '//trim(mode)
    error stop 2
  end if

  ! Original active-control, off, sign and LCZ behavior must remain identical.
  a=inputs(); a%indoor_temp=305.0; a%indoor_q=0.020
  call exact_reference('active_cooling_and_dehumidification',a)
  a%cop_option=1
  call exact_reference('active_variable_cop',a)
  a=inputs(); a%indoor_temp=295.0; a%indoor_q=0.005; a%appliance=10.0
  call exact_reference('off_without_threshold_crossing',a)
  a=inputs(); a%indoor_temp=305.0; a%roof_conv=-2000.0
  call exact_reference('cooling_cannot_heat',a)
  a=inputs(); a%month=1; a%indoor_temp=290.0; a%indoor_q=0.030
  a%roof_conv=-10.0
  call exact_reference('active_north_heating',a)
  a%latitude=-35.0; a%month=7
  call exact_reference('active_south_heating',a)
  do k=1,10
    a=inputs(); a%urban_type=k; a%indoor_temp=305.0; a%indoor_q=0.020
    write(label,'(A,I0)') 'active_lcz_',k
    call exact_reference(trim(label),a)
  end do
  a=inputs(); a%lcz=0; a%urban_type=3; a%indoor_temp=305.0; a%indoor_q=0.020
  call exact_reference('active_three_urban_commercial',a)

  ! A valid dry room must retain its water exactly when all fluxes are zero.
  humidities=[0.0,5.0e-5,1.0e-4,0.0392]
  do k=1,size(humidities)
    a=inputs(); a%indoor_q=humidities(k); a%canyon_q=humidities(k)
    a%ac_floor=0.0
    call checked_candidate('dry_no_flux',a,x)
    if (x%qin/=a%indoor_q) call fail('dry_no_flux','unforced water was created or removed')
    call inactive_hvac('dry_no_flux',x)
    call budgets('dry_no_flux',a,x)
    write(label,'(A,I0)') 'dry_no_flux_',k
    call mark(trim(label))
  end do

  ! Pure ventilation remains active when HVAC is off, including reverse flow.
  do k=1,2
    a=inputs(); a%ac_floor=0.0; a%ventilation=1.0
    if (k==1) then
      a%indoor_temp=300.0; a%canyon_temp=310.0
      a%indoor_q=0.005; a%canyon_q=0.020
    else
      a%month=1; a%indoor_temp=290.0; a%canyon_temp=280.0
      a%indoor_q=0.020; a%canyon_q=0.005
    end if
    call checked_candidate('pure_ventilation',a,x)
    call inactive_hvac('pure_ventilation',x)
    expected=real(a%indoor_q,real64)+(real(a%dt,real64)*real(a%ventilation,real64)/3600.0_real64)* &
             (real(a%canyon_q,real64)-real(a%indoor_q,real64))
    call near('pure_ventilation','humidity mixing',real(x%qin,real64),expected,2.0e-9_real64)
    if (k==1.and.(x%qvent<=0.0.or.x%event<=0.0)) call fail('pure_ventilation','inward sign')
    if (k==2.and.(x%qvent>=0.0.or.x%event>=0.0)) call fail('pure_ventilation','outward sign')
    call budgets('pure_ventilation',a,x)
    write(label,'(A,I0)') 'pure_ventilation_',k
    call mark(trim(label))
  end do

  ! Set an unforced-state trajectory that crosses a threshold halfway through
  ! this finite WRF step. The independent oracle accounts for active duration
  ! and active floor fraction; no ILS substeps or tuning constants are used.
  fractions=[0.0,0.25,1.0]
  do j=1,size(fractions)
    do k=1,2
      a=inputs(); a%ac_floor=fractions(j); a%indoor_q=0.005
      call capacities(a,heat_capacity,moisture_capacity,volume,area,floor_area)
      if (k==1) then
        a%indoor_temp=299.5
        a%roof_conv=real(heat_capacity/(real(a%dt,real64)*real(a%roof_width,real64)))
      else
        a%month=1; a%indoor_temp=300.5
        a%roof_conv=-real(heat_capacity/(real(a%dt,real64)*real(a%roof_width,real64)))
      end if
      call checked_candidate('temperature_onset',a,x)
      onset_time=0.5_real64*real(a%dt,real64)
      enabled_time=real(a%dt,real64)-onset_time
      expected=real(fractions(j),real64)*real(x%hin,real64)*enabled_time/real(a%dt,real64)
      call near('temperature_onset','processed mean load',real(x%hout,real64),expected, &
                3.0e-6_real64*max(1.0_real64,abs(expected)))
      if (k==1) then
        expected=300.5_real64-0.5_real64*real(fractions(j),real64)
      else
        expected=299.5_real64+0.5_real64*real(fractions(j),real64)
      end if
      call near('temperature_onset','final temperature',real(x%tin,real64),expected,4.0e-5_real64)
      call budgets('temperature_onset',a,x)
      call hvac_identity('temperature_onset',a,x)
      write(label,'(A,I0,A,I0)') 'temperature_onset_',k,'_fraction_',j
      call mark(trim(label))
    end do
  end do

  ! Latent threshold arrival is independent of the sensible cooling decision.
  do j=1,size(fractions)
    a=inputs(); a%indoor_temp=295.0; a%indoor_q=0.009
    a%ac_floor=fractions(j)
    call capacities(a,heat_capacity,moisture_capacity,volume,area,floor_area)
    a%latent_appliance=real(moisture_capacity*0.002_real64/(real(a%dt,real64)*floor_area))
    call checked_candidate('humidity_onset',a,x)
    free_end=real(a%indoor_q,real64)+real(a%dt,real64)*real(x%ein,real64)/moisture_capacity
    expected=real(a%target_q,real64)+(1.0_real64-real(fractions(j),real64))* &
             (free_end-real(a%target_q,real64))
    call near('humidity_onset','final humidity',real(x%qin,real64),expected,2.0e-9_real64)
    if (x%hout/=0.0) call fail('humidity_onset','latent control activated sensible cooling')
    call budgets('humidity_onset',a,x)
    call hvac_identity('humidity_onset',a,x)
    write(label,'(A,I0)') 'humidity_onset_fraction_',j
    call mark(trim(label))
  end do

  ! Arriving exactly at the threshold at step end requires no earlier HVAC.
  a=inputs(); a%indoor_temp=299.5; a%indoor_q=0.005
  call capacities(a,heat_capacity,moisture_capacity,volume,area,floor_area)
  a%roof_conv=real(0.5_real64*heat_capacity/(real(a%dt,real64)*real(a%roof_width,real64)))
  call checked_candidate('end_step_threshold',a,x)
  call near('end_step_threshold','Hout',real(x%hout,real64),0.0_real64,0.02_real64)
  call near('end_step_threshold','Tin',real(x%tin,real64),300.0_real64,4.0e-5_real64)
  call budgets('end_step_threshold',a,x); call mark('end_step_threshold')

  ! Start at the threshold: positive load is processed from time zero.
  a=inputs(); a%indoor_temp=a%target_temp; a%indoor_q=a%target_q
  a%appliance=10.0; a%latent_appliance=0.1
  call checked_candidate('start_at_threshold',a,x)
  call near('start_at_threshold','Hout equals Hin',real(x%hout,real64),real(x%hin,real64),0.0_real64)
  call near('start_at_threshold','Eout equals Ein',real(x%eout,real64),real(x%ein,real64),0.0_real64)
  if (x%tin/=a%indoor_temp.or.x%qin/=a%indoor_q) call fail('start_at_threshold','state drifts at full control')
  call budgets('start_at_threshold',a,x); call hvac_identity('start_at_threshold',a,x)
  call mark('start_at_threshold')

  ! Check both sides of the activation boundary at representable offsets.
  a=inputs(); a%appliance=10.0; a%latent_appliance=0.1
  eps_t=spacing(a%target_temp); eps_q=spacing(a%target_q)
  a%indoor_temp=a%target_temp-eps_t; a%indoor_q=a%target_q-eps_q
  b=a; b%indoor_temp=b%target_temp+eps_t; b%indoor_q=b%target_q+eps_q
  call checked_candidate('threshold_continuity_below',a,x)
  call checked_candidate('threshold_continuity_above',b,y)
  call capacities(a,heat_capacity,moisture_capacity,volume,area,floor_area)
  expected=(heat_capacity+real(a%cp,real64)*real(a%rho,real64)*volume)*real(eps_t,real64)/real(a%dt,real64)
  call near('threshold_continuity','sensible jump',real(y%hout-x%hout,real64),expected,0.001_real64)
  expected=moisture_capacity*real(eps_q,real64)*(1.0_real64/real(a%dt,real64)+1.0_real64/real(a%tau_q,real64))
  call near('threshold_continuity','latent jump',real(y%eout-x%eout,real64),expected,2.0e-5_real64)
  call budgets('threshold_continuity_below',a,x); call budgets('threshold_continuity_above',b,y)
  call mark('threshold_continuity')

  ! Aggressive dehumidification is limited by available water through Eout,
  ! with the very same Eout used in storage and electricity/heat rejection.
  a=inputs(); a%indoor_temp=295.0; a%indoor_q=0.020
  a%target_q=1.0e-6; a%tau_q=0.01
  call checked_candidate('removal_capacity_bound',a,x)
  call near('removal_capacity_bound','qin floor',real(x%qin,real64),1.0e-4_real64,3.0e-9_real64)
  call budgets('removal_capacity_bound',a,x); call hvac_identity('removal_capacity_bound',a,x)
  call mark('removal_capacity_bound')

  ! Invalid trial return is available to WRF/MPAS callers without a serial
  ! STOP. Failure does not silently clip the conserved humidity update.
  do k=1,6
    select case(k)
    case(1); mode='invalid_dt'
    case(2); mode='invalid_tau'
    case(3); mode='invalid_geometry'
    case(4); mode='invalid_humidity'
    case(5); mode='nonfinite_input'
    case(6); mode='negative_update'
    end select
    call invalid_case(trim(mode),a)
    call candidate(a,x,valid)
    if (valid) call fail(trim(mode),'invalid trial reported valid')
    if (k==6.and.x%qin>=0.0) call fail(trim(mode),'negative budget was clipped')
    call mark(trim(mode)//'_flag')
  end do

  write(*,'(A,I0)') 'WRF_BEM_CONTROLS_TEST_PASS cases=',cases

contains

  subroutine candidate(a,x,valid)
    type(inputs),intent(in)::a
    type(outputs),intent(out)::x
    logical,intent(out),optional::valid
    call slucm_bem_calculate( &
      a%urban_type,a%month,a%hour,a%cop_option,a%lcz, &
      a%perflo,a%heat_person,a%latent_person,a%appliance,a%latent_appliance, &
      a%height,a%floor_height,a%cp,a%ell,a%rho,a%rmf,a%ventilation, &
      a%canyon_temp,a%canyon_q,a%indoor_temp,a%indoor_q,a%pressure, &
      a%roof_conv,a%roof_rad,a%wall_conv,a%wall_rad,a%cop,a%latitude,a%abandoned, &
      a%heating_usage,a%cooling_usage,a%target_temp,a%gap_temp,a%target_q,a%gap_q, &
      a%tau_q,a%dt,a%roof_width,a%road_width,a%ac_floor,a%roof_temp,a%wall_temp, &
      x%qvent,x%event,x%hin,x%ein,x%cop,x%qfb,x%qfb_ht,x%qfb_cl,x%qfbl, &
      x%hout,x%eout,x%tin,x%qin,x%mrt,x%globe,x%wetbulb,x%wbgt,x%ecac,x%ec,state_valid=valid)
  end subroutine candidate

  subroutine reference(a,x)
    type(inputs),intent(in)::a
    type(outputs),intent(out)::x
    call reference_calculate( &
      a%urban_type,a%month,a%hour,a%cop_option,a%lcz, &
      a%perflo,a%heat_person,a%latent_person,a%appliance,a%latent_appliance, &
      a%height,a%floor_height,a%cp,a%ell,a%rho,a%rmf,a%ventilation, &
      a%canyon_temp,a%canyon_q,a%indoor_temp,a%indoor_q,a%pressure, &
      a%roof_conv,a%roof_rad,a%wall_conv,a%wall_rad,a%cop,a%latitude,a%abandoned, &
      a%heating_usage,a%cooling_usage,a%target_temp,a%gap_temp,a%target_q,a%gap_q, &
      a%tau_q,a%dt,a%roof_width,a%road_width,a%ac_floor,a%roof_temp,a%wall_temp, &
      x%qvent,x%event,x%hin,x%ein,x%cop,x%qfb,x%qfb_ht,x%qfb_cl,x%qfbl, &
      x%hout,x%eout,x%tin,x%qin,x%mrt,x%globe,x%wetbulb,x%wbgt,x%ecac,x%ec)
  end subroutine reference

  subroutine checked_candidate(label,a,x)
    character(*),intent(in)::label
    type(inputs),intent(in)::a
    type(outputs),intent(out)::x
    logical::ok
    call candidate(a,x,ok)
    if (.not.ok) call fail(label,'valid case rejected')
    if (.not.all(ieee_is_finite(vector(x)))) call fail(label,'nonfinite returned field')
  end subroutine checked_candidate

  subroutine exact_reference(label,a)
    character(*),intent(in)::label
    type(inputs),intent(in)::a
    type(outputs)::current,original
    call checked_candidate(label,a,current)
    call reference(a,original)
    if (any(vector(current)/=vector(original))) call fail(label,'unchanged path differs from PR reference')
    call budgets(label,a,current)
    call hvac_identity(label,a,current)
    call mark(label)
  end subroutine exact_reference

  function vector(x) result(v)
    type(outputs),intent(in)::x
    real::v(19)
    v=[x%qvent,x%event,x%hin,x%ein,x%cop,x%qfb,x%qfb_ht,x%qfb_cl,x%qfbl, &
       x%hout,x%eout,x%tin,x%qin,x%mrt,x%globe,x%wetbulb,x%wbgt,x%ecac,x%ec]
  end function vector

  subroutine capacities(a,ch,cq,volume,land_area,floor_area)
    type(inputs),intent(in)::a
    real(real64),intent(out)::ch,cq,volume,land_area,floor_area
    volume=real(a%height,real64)*real(a%roof_width,real64)
    ch=(real(a%rho,real64)*real(a%cp,real64)+real(a%rmf,real64))*volume
    cq=real(a%ell,real64)*real(a%rho,real64)*volume
    land_area=real(a%roof_width,real64)+real(a%road_width,real64)
    floor_area=volume/real(a%floor_height,real64)
  end subroutine capacities

  subroutine budgets(label,a,x)
    character(*),intent(in)::label
    type(inputs),intent(in)::a
    type(outputs),intent(in)::x
    real(real64)::ch,cq,volume,land_area,floor_area,storage,flux_energy,rounding
    call capacities(a,ch,cq,volume,land_area,floor_area)
    storage=ch*(real(x%tin,real64)-real(a%indoor_temp,real64))
    flux_energy=real(a%dt,real64)*(real(x%hin,real64)-real(x%hout,real64))
    ! Temperature/humidity are single precision states. Their one-rounding
    ! storage uncertainty is derived from spacing, not a loosened flux test.
    rounding=1.1_real64*ch*real(spacing(x%tin),real64)+2.0e-6_real64*abs(flux_energy)
    call near(label,'sensible storage',storage,flux_energy,max(1.0e-8_real64,rounding))
    storage=cq*(real(x%qin,real64)-real(a%indoor_q,real64))
    flux_energy=real(a%dt,real64)*(real(x%ein,real64)-real(x%eout,real64))
    rounding=1.1_real64*cq*real(max(spacing(x%qin),spacing(a%indoor_q)),real64) &
             +2.0e-6_real64*abs(flux_energy)
    call near(label,'latent storage',storage,flux_energy,max(1.0e-8_real64,rounding))
  end subroutine budgets

  subroutine hvac_identity(label,a,x)
    character(*),intent(in)::label
    type(inputs),intent(in)::a
    type(outputs),intent(in)::x
    real(real64)::ch,cq,volume,land_area,floor_area,power,net_machine,expected
    call capacities(a,ch,cq,volume,land_area,floor_area)
    power=real(x%ecac,real64)*floor_area
    net_machine=(real(x%qfb,real64)+real(x%qfbl,real64))*land_area
    ! Heating Hout is negative; both heating and cooling obey this identity.
    expected=real(x%hout,real64)+real(x%eout,real64)+power
    call near(label,'machine first law',net_machine,expected, &
              5.0e-6_real64*max(1.0_real64,abs(expected)))
    expected=(abs(real(x%hout,real64))+real(x%eout,real64))/real(x%cop,real64)
    call near(label,'electricity from processed load',power,expected, &
              5.0e-6_real64*max(1.0_real64,abs(expected)))
  end subroutine hvac_identity

  subroutine inactive_hvac(label,x)
    character(*),intent(in)::label
    type(outputs),intent(in)::x
    if (any([x%hout,x%eout,x%qfb,x%qfb_ht,x%qfb_cl,x%qfbl,x%ecac]/=0.0)) &
      call fail(label,'HVAC-off load, heat rejection or electricity is nonzero')
  end subroutine inactive_hvac

  subroutine invalid_case(mode,a)
    character(*),intent(in)::mode
    type(inputs),intent(out)::a
    a=inputs()
    select case(mode)
    case('invalid_dt'); a%dt=0.0
    case('invalid_tau'); a%tau_q=0.0
    case('invalid_geometry'); a%floor_height=0.0
    case('invalid_humidity'); a%indoor_q=-0.001
    case('nonfinite_input'); a%indoor_q=ieee_value(0.0,ieee_quiet_nan)
    case('negative_update')
      a%ac_floor=0.0; a%ventilation=120.0; a%canyon_q=0.0; a%indoor_q=0.010
    case default
      call fail('test_mode','unknown negative test mode '//mode)
    end select
  end subroutine invalid_case

  subroutine near(label,quantity,value,expected,tolerance)
    character(*),intent(in)::label,quantity
    real(real64),intent(in)::value,expected,tolerance
    if (.not.ieee_is_finite(value).or.abs(value-expected)>tolerance) then
      write(error_unit,'(A,3ES22.12)') trim(label)//' '//trim(quantity)//' value/expected/tol=', &
        value,expected,tolerance
      call fail(label,quantity)
    end if
  end subroutine near

  subroutine mark(label)
    character(*),intent(in)::label
    cases=cases+1
    write(*,'(A)') 'WRF_BEM_CASE_PASS '//trim(label)
  end subroutine mark

  subroutine fail(label,reason)
    character(*),intent(in)::label,reason
    write(error_unit,'(A)') 'WRF_BEM_CASE_FAIL '//trim(label)//' '//trim(reason)
    error stop 1
  end subroutine fail
end program bem_controls_regression
