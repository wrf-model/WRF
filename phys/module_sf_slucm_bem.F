MODULE module_sf_slucm_bem

  IMPLICIT NONE
  PRIVATE

  PUBLIC :: slucm_bem_calculate
  PUBLIC :: slucm_bem_update_inner_layer

CONTAINS

!===============================================================================
! Update the innermost roof or wall layer using the indoor-side convective and
! radiative boundary condition used by SLUCM+BEM.
!
! The ordinary SLUCM multi_layer solver remains in module_sf_urban and is
! called first. This routine then replaces only the innermost-layer value.
! For the four-layer configuration used by the original SLUCM+BEM code, this
! reproduces its roof/wall update while keeping the standard SLUCM solver intact.
!===============================================================================
  SUBROUTINE slucm_bem_update_inner_layer(                         &
       num_layers, cap_cgs, conductivity_cgs, layer_depth_cm, dt, &
       emissivity, sigma_si, characteristic_height,               &
       indoor_temp_prev, layer_temp_prev, layer_temp,              &
       q_conv, q_rad)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: num_layers
    REAL, INTENT(IN) :: cap_cgs
    REAL, INTENT(IN) :: conductivity_cgs
    REAL, DIMENSION(num_layers), INTENT(IN) :: layer_depth_cm
    REAL, INTENT(IN) :: dt
    REAL, INTENT(IN) :: emissivity
    REAL, INTENT(IN) :: sigma_si
    REAL, INTENT(IN) :: characteristic_height
    REAL, INTENT(IN) :: indoor_temp_prev
    REAL, DIMENSION(num_layers), INTENT(IN) :: layer_temp_prev
    REAL, DIMENSION(num_layers), INTENT(INOUT) :: layer_temp
    REAL, INTENT(OUT) :: q_conv
    REAL, INTENT(OUT) :: q_rad

    INTEGER :: inner_layer
    INTEGER :: adjacent_layer
    REAL :: conv_coeff
    REAL :: rad_coeff
    REAL :: grashof_number
    REAL :: prandtl_number
    REAL :: cap_si
    REAL :: conductivity_si
    REAL :: depth_m
    REAL :: inner_temp_prev
    REAL :: adjacent_temp_prev

    inner_layer = num_layers
    adjacent_layer = num_layers - 1

    inner_temp_prev = layer_temp_prev(inner_layer)
    adjacent_temp_prev = layer_temp_prev(adjacent_layer)

    CALL cal_chtc(characteristic_height,                          &
                  inner_temp_prev - 273.15,                       &
                  indoor_temp_prev - 273.15,                      &
                  grashof_number, prandtl_number, conv_coeff)

    rad_coeff = 4.0 * emissivity * sigma_si * indoor_temp_prev**3

    q_conv = conv_coeff * (inner_temp_prev - indoor_temp_prev)
    q_rad  = rad_coeff  * (inner_temp_prev - indoor_temp_prev)

    ! Convert heat capacity from cal cm-3 K-1 to J m-3 K-1.
    cap_si = cap_cgs / ((1.0 / 4.1868) * 1.0E-6)

    ! Convert conductivity from cal cm-1 s-1 K-1 to W m-1 K-1.
    conductivity_si = conductivity_cgs / ((1.0 / 4.1868) * 1.0E-2)

    depth_m = layer_depth_cm(inner_layer) / 100.0

    layer_temp(inner_layer) = inner_temp_prev                      &
         + dt * (conductivity_si *                                &
         (adjacent_temp_prev - inner_temp_prev)                    &
         - depth_m * (conv_coeff + rad_coeff) *                   &
         (inner_temp_prev - indoor_temp_prev))                    &
         / (cap_si * depth_m**2)

  END SUBROUTINE slucm_bem_update_inner_layer

!===============================================================================
! SLUCM+BEM computational routine.
!
! This routine advances the indoor sensible/latent energy balance for one
! urban category and one model time step; it is not an initialization routine.
! It has no WRF- or MPAS-specific module dependency. The caller supplies
! scalar parameters selected from URBPARM.TBL or URBPARM_LCZ.TBL; when
! distributed aerodynamics is active, the caller may instead derive the BEM
! canyon geometry from grid-resolved morphology. module_sf_urban therefore
! remains the only external urban-physics interface.
!===============================================================================
  SUBROUTINE slucm_bem_calculate(                                      &
       urban_type, month, local_hour, cop_option, lcz_switch,     &
       perflo, heat_person, latent_heat_person,                   &
       appliance_baseload, latent_appliance_baseload,            &
       building_height, floor_height, cp_air, latent_heat_vapor, &
       air_density, effective_heat_capacity,                     &
       ventilation_frequency, canyon_temp,                       &
       canyon_specific_humidity, indoor_temp_prev,               &
       indoor_specific_humidity_prev, pressure_pa,               &
       roof_conv_flux, roof_rad_flux, wall_conv_flux,            &
       wall_rad_flux, rated_cop, latitude, abandoned_ratio,      &
       heating_usage_ratio, cooling_usage_ratio,                 &
       target_temp, temperature_deadband,                        &
       target_specific_humidity, humidity_deadband,              &
       humidity_relaxation_time, dt,                             &
       roof_width, road_width, ac_floor_ratio,                   &
       roof_inner_temp_prev, wall_inner_temp_prev,               &
       qvent, event, indoor_load, indoor_latent_load,            &
       simulated_cop, qfb, qfb_heating, qfb_cooling, qfbl,      &
       processed_load, processed_latent_load,                    &
       indoor_temp, indoor_specific_humidity, indoor_mrt,        &
       indoor_globe_temp, indoor_wetbulb_temp, indoor_wbgt,     &
       ecac, ec)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: urban_type
    INTEGER, INTENT(IN) :: month
    INTEGER, INTENT(IN) :: local_hour
    INTEGER, INTENT(IN) :: cop_option
    INTEGER, INTENT(IN) :: lcz_switch

    REAL, INTENT(IN) :: perflo
    REAL, INTENT(IN) :: heat_person
    REAL, INTENT(IN) :: latent_heat_person
    REAL, INTENT(IN) :: appliance_baseload
    REAL, INTENT(IN) :: latent_appliance_baseload
    REAL, INTENT(IN) :: building_height
    REAL, INTENT(IN) :: floor_height
    REAL, INTENT(IN) :: cp_air
    REAL, INTENT(IN) :: latent_heat_vapor
    REAL, INTENT(IN) :: air_density
    REAL, INTENT(IN) :: effective_heat_capacity
    REAL, INTENT(IN) :: ventilation_frequency
    REAL, INTENT(IN) :: canyon_temp
    REAL, INTENT(IN) :: canyon_specific_humidity
    REAL, INTENT(IN) :: indoor_temp_prev
    REAL, INTENT(IN) :: indoor_specific_humidity_prev
    REAL, INTENT(IN) :: pressure_pa
    REAL, INTENT(IN) :: roof_conv_flux
    REAL, INTENT(IN) :: roof_rad_flux
    REAL, INTENT(IN) :: wall_conv_flux
    REAL, INTENT(IN) :: wall_rad_flux
    REAL, INTENT(IN) :: rated_cop
    REAL, INTENT(IN) :: latitude
    REAL, INTENT(IN) :: abandoned_ratio
    REAL, INTENT(IN) :: heating_usage_ratio
    REAL, INTENT(IN) :: cooling_usage_ratio
    REAL, INTENT(IN) :: target_temp
    REAL, INTENT(IN) :: temperature_deadband
    REAL, INTENT(IN) :: target_specific_humidity
    REAL, INTENT(IN) :: humidity_deadband
    REAL, INTENT(IN) :: humidity_relaxation_time
    REAL, INTENT(IN) :: dt
    REAL, INTENT(IN) :: roof_width
    REAL, INTENT(IN) :: road_width
    REAL, DIMENSION(24), INTENT(IN) :: ac_floor_ratio
    REAL, INTENT(IN) :: roof_inner_temp_prev
    REAL, INTENT(IN) :: wall_inner_temp_prev

    REAL, INTENT(OUT) :: qvent
    REAL, INTENT(OUT) :: event
    REAL, INTENT(OUT) :: indoor_load
    REAL, INTENT(OUT) :: indoor_latent_load
    REAL, INTENT(OUT) :: simulated_cop
    REAL, INTENT(OUT) :: qfb
    REAL, INTENT(OUT) :: qfb_heating
    REAL, INTENT(OUT) :: qfb_cooling
    REAL, INTENT(OUT) :: qfbl
    REAL, INTENT(OUT) :: processed_load
    REAL, INTENT(OUT) :: processed_latent_load
    REAL, INTENT(OUT) :: indoor_temp
    REAL, INTENT(OUT) :: indoor_specific_humidity
    REAL, INTENT(OUT) :: indoor_mrt
    REAL, INTENT(OUT) :: indoor_globe_temp
    REAL, INTENT(OUT) :: indoor_wetbulb_temp
    REAL, INTENT(OUT) :: indoor_wbgt
    REAL, INTENT(OUT) :: ecac
    REAL, INTENT(OUT) :: ec

    LOGICAL :: heating_mode
    REAL, PARAMETER :: minimum_specific_humidity = 1.0E-4
    REAL :: occupant_baseload
    REAL :: total_baseload
    REAL :: latent_occupant_baseload
    REAL :: latent_total_baseload
    REAL :: indoor_heat_capacity
    REAL :: indoor_moisture_capacity
    REAL :: air_volume_flow
    REAL :: active_floor_fraction
    REAL :: floor_area
    REAL :: land_area
    REAL :: temp_lower_limit
    REAL :: temp_upper_limit
    REAL :: humidity_upper_limit
    REAL :: sensible_control_load
    REAL :: latent_control_load
    REAL :: max_latent_removal
    REAL :: cooling_load
    REAL :: heating_load
    REAL :: compressor_power
    REAL :: heat_rejection
    REAL :: safe_cop

    ! Explicit initialization prevents option-dependent undefined outputs.
    qvent = 0.0
    event = 0.0
    indoor_load = 0.0
    indoor_latent_load = 0.0
    simulated_cop = rated_cop
    qfb = 0.0
    qfb_heating = 0.0
    qfb_cooling = 0.0
    qfbl = 0.0
    processed_load = 0.0
    processed_latent_load = 0.0
    indoor_temp = indoor_temp_prev
    indoor_specific_humidity = indoor_specific_humidity_prev
    indoor_mrt = 0.0
    indoor_globe_temp = 0.0
    indoor_wetbulb_temp = 0.0
    indoor_wbgt = 0.0
    ecac = 0.0
    ec = 0.0

    occupant_baseload = perflo * heat_person
    total_baseload = appliance_baseload + occupant_baseload

    latent_occupant_baseload = perflo * latent_heat_person
    latent_total_baseload = latent_appliance_baseload             &
         + latent_occupant_baseload

    land_area = roof_width + road_width
    floor_area = (building_height / floor_height) * roof_width

    indoor_heat_capacity =                                        &
         (cp_air * air_density + effective_heat_capacity)          &
         * (building_height * roof_width)

    indoor_moisture_capacity = latent_heat_vapor * air_density    &
         * building_height * roof_width

    air_volume_flow = ventilation_frequency                       &
         * (building_height * roof_width) / 3600.0

    qvent = (cp_air * air_density) * air_volume_flow               &
         * (canyon_temp - indoor_temp_prev)

    event = (latent_heat_vapor * air_density) * air_volume_flow   &
         * (canyon_specific_humidity                              &
         - indoor_specific_humidity_prev)

    indoor_load = total_baseload * floor_area                     &
         + qvent                                                   &
         + roof_width * (roof_conv_flux + roof_rad_flux)           &
         + 2.0 * building_height * (wall_conv_flux + wall_rad_flux)

    indoor_latent_load = latent_total_baseload * floor_area       &
         + event

    IF (cop_option == 1) THEN
      CALL variable_cop(rated_cop, canyon_temp, indoor_temp_prev, &
                        urban_type, simulated_cop)
    END IF
    safe_cop = MAX(simulated_cop, 1.0E-6)

    temp_lower_limit = target_temp - MAX(temperature_deadband, 0.0)
    temp_upper_limit = target_temp + MAX(temperature_deadband, 0.0)
    humidity_upper_limit = target_specific_humidity               &
         + MAX(humidity_deadband, 0.0)

    ! Northern Hemisphere: heating in Oct-Mar; Southern Hemisphere: reverse.
    heating_mode = ((latitude >= 0.0) .AND.                       &
                    (month <= 3 .OR. month >= 10)) .OR.           &
                   ((latitude < 0.0) .AND.                        &
                    (month > 3 .AND. month < 10))

    IF (heating_mode) THEN
      active_floor_fraction = (1.0 - abandoned_ratio)            &
           * ac_floor_ratio(local_hour) * heating_usage_ratio
    ELSE
      active_floor_fraction = (1.0 - abandoned_ratio)            &
           * ac_floor_ratio(local_hour) * cooling_usage_ratio
    END IF
    active_floor_fraction = MIN(1.0, MAX(0.0, active_floor_fraction))

    ! Sensible heating and cooling are controlled independently of humidity.
    ! Sign constraints prevent a cooling call from producing negative Hout,
    ! or a heating call from producing positive Hout.
    IF (heating_mode) THEN
      IF (active_floor_fraction > 0.0 .AND.                       &
          indoor_temp_prev < temp_lower_limit) THEN
        sensible_control_load = indoor_load * active_floor_fraction &
             + active_floor_fraction * cp_air * air_density       &
             * building_height * roof_width                       &
             * (indoor_temp_prev - temp_lower_limit) / dt
        processed_load = MIN(0.0, sensible_control_load)
      END IF
    ELSE
      IF (active_floor_fraction > 0.0 .AND.                       &
          indoor_temp_prev > temp_upper_limit) THEN
        sensible_control_load = indoor_load * active_floor_fraction &
             + active_floor_fraction * cp_air * air_density       &
             * building_height * roof_width                       &
             * (indoor_temp_prev - temp_upper_limit) / dt
        processed_load = MAX(0.0, sensible_control_load)
      END IF

      ! Dehumidification is activated independently of the sensible cooling
      ! condition. The excess humidity is relaxed toward the upper comfort
      ! limit over humidity_relaxation_time, not over the WRF model time step.
      IF (active_floor_fraction > 0.0 .AND.                       &
          indoor_specific_humidity_prev > humidity_upper_limit) THEN
        latent_control_load = indoor_latent_load * active_floor_fraction &
             + active_floor_fraction * indoor_moisture_capacity   &
             * (indoor_specific_humidity_prev                     &
             - humidity_upper_limit) / humidity_relaxation_time
        processed_latent_load = MAX(0.0, latent_control_load)

        ! Prevent the dehumidification term from driving qin below its lower
        ! bound in one WRF time step, even if an extreme parameter is supplied.
        max_latent_removal = indoor_latent_load                   &
             + indoor_moisture_capacity                           &
             * (indoor_specific_humidity_prev                     &
             - minimum_specific_humidity) / dt
        processed_latent_load = MIN(processed_latent_load,        &
                                     MAX(0.0, max_latent_removal))
      END IF
    END IF

    indoor_temp = indoor_temp_prev                                &
         + (dt / indoor_heat_capacity) * (indoor_load - processed_load)

    indoor_specific_humidity = indoor_specific_humidity_prev      &
         + (dt / indoor_moisture_capacity)                        &
         * (indoor_latent_load - processed_latent_load)
    indoor_specific_humidity = MAX(minimum_specific_humidity,     &
                                   indoor_specific_humidity)

    ! Compute electricity and outdoor heat exchange from the load actually
    ! processed by HVAC. Therefore QFB/QFBL and ECAC are exactly zero when
    ! the HVAC system is inactive.
    IF (heating_mode) THEN
      heating_load = MAX(0.0, -processed_load)
      IF (heating_load > 0.0) THEN
        compressor_power = heating_load / safe_cop
        qfb_heating = -(heating_load - compressor_power) / land_area
        qfb = qfb_heating
        ecac = compressor_power / floor_area
      END IF
    ELSE
      cooling_load = MAX(0.0, processed_load + processed_latent_load)
      IF (cooling_load > 0.0) THEN
        compressor_power = cooling_load / safe_cop
        heat_rejection = cooling_load + compressor_power
        qfb_cooling = heat_rejection / land_area
        CALL split_cooling_heat_rejection(urban_type, lcz_switch, &
                                          qfb_cooling, qfb, qfbl)
        ecac = compressor_power / floor_area
      END IF
    END IF

    CALL indoor_comfort_diagnostics(indoor_temp,                  &
         indoor_specific_humidity, pressure_pa,                   &
         roof_inner_temp_prev, wall_inner_temp_prev,              &
         indoor_mrt, indoor_globe_temp, indoor_wetbulb_temp,      &
         indoor_wbgt)

    ec = ecac + appliance_baseload + latent_appliance_baseload

  END SUBROUTINE slucm_bem_calculate

!===============================================================================
  SUBROUTINE split_cooling_heat_rejection(urban_type, lcz_switch, &
                                          qfb_cooling, qfb, qfbl)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: urban_type
    INTEGER, INTENT(IN) :: lcz_switch
    REAL, INTENT(IN) :: qfb_cooling
    REAL, INTENT(OUT) :: qfb
    REAL, INTENT(OUT) :: qfbl

    LOGICAL :: cooling_tower_type

    IF (lcz_switch == 0) THEN
      cooling_tower_type = (urban_type == 3)
    ELSE
      cooling_tower_type = (urban_type == 1 .OR.                  &
                            urban_type == 4 .OR.                  &
                            urban_type == 10)
    END IF

    IF (cooling_tower_type) THEN
      qfb = qfb_cooling * 0.722
      qfbl = qfb_cooling * (1.0 - 0.722)
    ELSE
      qfb = qfb_cooling
      qfbl = 0.0
    END IF

  END SUBROUTINE split_cooling_heat_rejection

!===============================================================================
  SUBROUTINE indoor_comfort_diagnostics(indoor_temp,          &
       indoor_specific_humidity, pressure_pa,                       &
       roof_inner_temp_prev, wall_inner_temp_prev,                  &
       indoor_mrt, indoor_globe_temp, indoor_wetbulb_temp,          &
       indoor_wbgt)

    IMPLICIT NONE

    REAL, INTENT(IN) :: indoor_temp
    REAL, INTENT(IN) :: indoor_specific_humidity
    REAL, INTENT(IN) :: pressure_pa
    REAL, INTENT(IN) :: roof_inner_temp_prev
    REAL, INTENT(IN) :: wall_inner_temp_prev
    REAL, INTENT(OUT) :: indoor_mrt
    REAL, INTENT(OUT) :: indoor_globe_temp
    REAL, INTENT(OUT) :: indoor_wetbulb_temp
    REAL, INTENT(OUT) :: indoor_wbgt

    indoor_mrt = 0.5 * (roof_inner_temp_prev + wall_inner_temp_prev)
    indoor_globe_temp = 0.5 * indoor_temp + 0.5 * indoor_mrt

    CALL calc_wetbulb_urb(indoor_temp, indoor_specific_humidity,  &
                          pressure_pa, indoor_wetbulb_temp)

    indoor_wbgt = 0.7 * indoor_wetbulb_temp                       &
         + 0.3 * indoor_globe_temp

  END SUBROUTINE indoor_comfort_diagnostics

!===============================================================================
! Saturation vapor pressure [Pa].
!===============================================================================
  REAL FUNCTION esat_pa_wb(temp_k) RESULT(esat)

    IMPLICIT NONE

    REAL, INTENT(IN) :: temp_k
    REAL :: temp_c

    temp_c = temp_k - 273.15
    esat = 611.2 * EXP(17.67 * temp_c / (temp_c + 243.5))

  END FUNCTION esat_pa_wb

!===============================================================================
! Saturation specific humidity [kg kg-1].
!===============================================================================
  REAL FUNCTION qsat_kgkg_wb(temp_k, pressure_pa) RESULT(qsat)

    IMPLICIT NONE

    REAL, INTENT(IN) :: temp_k
    REAL, INTENT(IN) :: pressure_pa
    REAL, PARAMETER :: epsilon = 0.622
    REAL :: vapor_pressure

    vapor_pressure = esat_pa_wb(temp_k)
    qsat = epsilon * vapor_pressure                                &
         / (pressure_pa - (1.0 - epsilon) * vapor_pressure)

  END FUNCTION qsat_kgkg_wb

!===============================================================================
! Robust fallback wet-bulb solution based on moist-air enthalpy conservation.
!===============================================================================
  SUBROUTINE wetbulb_bisection(indoor_temp, indoor_q, pressure_pa, &
                               wetbulb_temp, error_code)

    IMPLICIT NONE

    REAL, INTENT(IN) :: indoor_temp
    REAL, INTENT(IN) :: indoor_q
    REAL, INTENT(IN) :: pressure_pa
    REAL, INTENT(OUT) :: wetbulb_temp
    INTEGER, INTENT(OUT) :: error_code

    REAL, PARAMETER :: cp_moist = 1004.0
    REAL, PARAMETER :: latent_heat = 2.5E6
    INTEGER, PARAMETER :: max_iterations = 60
    REAL, PARAMETER :: tolerance = 1.0E-4

    INTEGER :: iteration
    REAL :: enthalpy
    REAL :: residual
    REAL :: lower_temp
    REAL :: upper_temp
    REAL :: middle_temp
    REAL :: temp_local
    REAL :: q_local
    REAL :: pressure_local

    error_code = 0

    temp_local = indoor_temp
    q_local = MAX(0.0, indoor_q)
    pressure_local = MAX(1.0, pressure_pa)

    IF (pressure_pa <= 0.0 .OR. indoor_temp <= 0.0) THEN
      wetbulb_temp = -9999.0
      error_code = 1
      RETURN
    END IF

    enthalpy = cp_moist * temp_local + latent_heat * q_local

    upper_temp = temp_local
    lower_temp = MAX(150.0, temp_local - 80.0)

    DO iteration = 1, 20
      residual = cp_moist * lower_temp                            &
           + latent_heat * qsat_kgkg_wb(lower_temp, pressure_local) &
           - enthalpy
      IF (residual <= 0.0) EXIT
      lower_temp = MAX(100.0, lower_temp - 50.0)
    END DO

    residual = cp_moist * lower_temp                              &
         + latent_heat * qsat_kgkg_wb(lower_temp, pressure_local) &
         - enthalpy
    IF (residual > 0.0) THEN
      wetbulb_temp = temp_local
      error_code = 2
      RETURN
    END IF

    DO iteration = 1, max_iterations
      middle_temp = 0.5 * (lower_temp + upper_temp)
      residual = cp_moist * middle_temp                           &
           + latent_heat * qsat_kgkg_wb(middle_temp, pressure_local) &
           - enthalpy

      IF (ABS(upper_temp - lower_temp) < tolerance) THEN
        wetbulb_temp = middle_temp
        RETURN
      END IF

      IF (residual > 0.0) THEN
        upper_temp = middle_temp
      ELSE
        lower_temp = middle_temp
      END IF
    END DO

    wetbulb_temp = 0.5 * (lower_temp + upper_temp)
    error_code = 3

  END SUBROUTINE wetbulb_bisection

!===============================================================================
! Hybrid indoor wet-bulb calculation copied from the supplied latent-load code:
! Stull approximation in its normal range, with a bisection fallback.
!===============================================================================
  SUBROUTINE calc_wetbulb_urb(indoor_temp, indoor_q, pressure_pa, &
                              wetbulb_temp)

    IMPLICIT NONE

    REAL, INTENT(IN) :: indoor_temp
    REAL, INTENT(IN) :: indoor_q
    REAL, INTENT(IN) :: pressure_pa
    REAL, INTENT(OUT) :: wetbulb_temp

    REAL, PARAMETER :: epsilon = 0.622
    REAL, PARAMETER :: temp_zero = 273.15
    REAL, PARAMETER :: min_temp_fast = 233.15
    REAL, PARAMETER :: max_temp_fast = 333.15
    REAL, PARAMETER :: min_pressure_fast = 50000.0
    REAL, PARAMETER :: max_pressure_fast = 110000.0

    REAL :: temp_local
    REAL :: q_local
    REAL :: pressure_local
    REAL :: vapor_pressure
    REAL :: saturation_pressure
    REAL :: relative_humidity_percent
    REAL :: temp_c
    REAL :: wetbulb_c
    REAL :: wetbulb_fast
    INTEGER :: bisection_error
    LOGICAL :: use_fallback

    temp_local = indoor_temp
    q_local = MAX(0.0, indoor_q)
    pressure_local = MAX(1.0, pressure_pa)

    vapor_pressure = q_local * pressure_local                     &
         / (epsilon + (1.0 - epsilon) * q_local)
    saturation_pressure = esat_pa_wb(temp_local)
    relative_humidity_percent = 100.0 * vapor_pressure            &
         / MAX(1.0, saturation_pressure)

    use_fallback = .FALSE.
    IF (temp_local < min_temp_fast .OR. temp_local > max_temp_fast) &
         use_fallback = .TRUE.
    IF (pressure_local < min_pressure_fast .OR.                   &
        pressure_local > max_pressure_fast) use_fallback = .TRUE.
    IF (relative_humidity_percent < -1.0 .OR.                     &
        relative_humidity_percent > 101.0) use_fallback = .TRUE.

    relative_humidity_percent = MAX(0.0,                          &
         MIN(100.0, relative_humidity_percent))
    temp_c = temp_local - temp_zero

    wetbulb_c = temp_c * ATAN(0.151977 *                         &
         SQRT(relative_humidity_percent + 8.313659))              &
         + ATAN(temp_c + relative_humidity_percent)               &
         - ATAN(relative_humidity_percent - 1.676331)             &
         + 0.00391838 * relative_humidity_percent**1.5            &
         * ATAN(0.023101 * relative_humidity_percent)             &
         - 4.686035

    wetbulb_fast = wetbulb_c + temp_zero
    IF (wetbulb_fast > temp_local) wetbulb_fast = temp_local
    IF (wetbulb_fast < 100.0 .OR. wetbulb_fast > temp_local)     &
         use_fallback = .TRUE.

    IF (.NOT. use_fallback) THEN
      wetbulb_temp = wetbulb_fast
      RETURN
    END IF

    CALL wetbulb_bisection(temp_local, q_local, pressure_local,   &
                           wetbulb_temp, bisection_error)

    IF (bisection_error /= 0) THEN
      wetbulb_temp = wetbulb_fast
      IF (wetbulb_temp > temp_local) wetbulb_temp = temp_local
      IF (wetbulb_temp < 100.0) wetbulb_temp = 100.0
    END IF

  END SUBROUTINE calc_wetbulb_urb

!===============================================================================
  SUBROUTINE variable_cop(rated_cop, outdoor_temp, indoor_temp,   &
                          urban_type, simulated_cop)

    IMPLICIT NONE

    REAL, INTENT(IN) :: rated_cop
    REAL, INTENT(IN) :: outdoor_temp
    REAL, INTENT(IN) :: indoor_temp
    INTEGER, INTENT(IN) :: urban_type
    REAL, INTENT(OUT) :: simulated_cop

    REAL :: x
    REAL :: y
    REAL :: z
    REAL :: g
    REAL :: fq
    REAL :: fp
    REAL :: fx

    x = outdoor_temp - 273.15
    y = indoor_temp - 273.15
    z = 1.0

    g = 52.8 - 1.65 * x - y

    IF (g >= 0.0) THEN
      fq = 3.50E-1 + 3.82E-2 * y
    ELSE
      fq = 3.49E-1 + 2.93E-3 * x + 4.35E-2 * y - 4.20E-4 * x * y
    END IF

    IF (g >= 0.0) THEN
      fp = 5.68E-1 + 2.80E-3 * x + 9.19E-3 * y - 3.10E-5 * x * y
    ELSE
      fp = 1.66E-1 + 1.71E-2 * x + 1.80E-2 * y - 1.64E-4 * x * y
    END IF

    IF (urban_type == 1) THEN
      fx = 0.852618 * z**2 - 0.033970 * z + 0.183530
    ELSE
      fx = 0.788958 * z**2 + 0.099231 * z + 0.113276
    END IF

    simulated_cop = rated_cop * fq * z / (fp * fx)

  END SUBROUTINE variable_cop

!===============================================================================
  SUBROUTINE cal_chtc(fh, tlp, indoor_temp, gr, pr, chtc)

    IMPLICIT NONE

    REAL, INTENT(IN) :: fh
    REAL, INTENT(IN) :: tlp
    REAL, INTENT(IN) :: indoor_temp
    REAL, INTENT(OUT) :: gr
    REAL, INTENT(OUT) :: pr
    REAL, INTENT(OUT) :: chtc

    REAL, PARAMETER :: gravity = 9.8
    REAL :: tm
    REAL :: xny
    REAL :: thermal_diffusivity
    REAL :: thermal_conductivity
    REAL :: gp
    REAL :: xnu

    tm = 0.5 * (tlp + indoor_temp)

    xny = (0.138 + tm * (8.91667E-4 + tm * (-5.09317E-11 + tm * &
         (2.08324E-8 + tm * 3.55271E-15)))) * 1.0E-4

    thermal_diffusivity = (0.0689 + tm * (4.87083E-4 + tm *      &
         (9.27068E-7 + tm * (-1.77091E-8 + tm * 1.82306E-10)))) &
         / 3600.0

    thermal_conductivity = (0.0207 + tm * (7.16666E-5 + tm *    &
         (-2.08381E-8 + tm * (-4.16685E-9 + tm * 5.20841E-11)))) &
         * 1.163

    gr = gravity / (tm + 273.16) * ABS(tlp - indoor_temp)        &
         * fh**3 / xny**2
    pr = xny / thermal_diffusivity
    gp = gr * pr

    IF (gp < 1.0E9) THEN
      xnu = 0.56 * gp**0.25
    ELSE
      xnu = 0.13 * gp**0.33
    END IF

    chtc = xnu * thermal_conductivity / fh

  END SUBROUTINE cal_chtc

END MODULE module_sf_slucm_bem
