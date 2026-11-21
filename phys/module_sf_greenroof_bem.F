! SLUCM+BEM green-roof thermal connection, 2026-11-21.
! A parallel host passes state_valid and handles failure with its own fatal
! error routine. Callers that omit it stop on invalid input/solution.
! Four finite-volume cells: three substrate cells and one concrete roof cell.
! All inputs/outputs in SI units; heat flux is positive downward / into the room.
MODULE module_sf_greenroof_bem
  USE, INTRINSIC :: ieee_arithmetic, ONLY: ieee_is_finite
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: greenroof_column_step, greenroof_bem_diagnostics

  TYPE :: greenroof_bem_diagnostics
    LOGICAL :: active=.FALSE.
    REAL(KIND=8) :: fraction=0.0_8
    REAL(KIND=8) :: surface_to_column_wm2=0.0_8
    REAL(KIND=8) :: soil_to_roof_wm2=0.0_8
    REAL(KIND=8) :: roof_to_indoor_wm2=0.0_8
    REAL(KIND=8) :: roof_conv_wm2=0.0_8
    REAL(KIND=8) :: roof_rad_wm2=0.0_8
    REAL(KIND=8) :: storage_wm2=0.0_8
    REAL(KIND=8) :: column_residual_wm2=0.0_8
    REAL(KIND=8) :: solver_residual_wm2=0.0_8
    REAL(KIND=8) :: surface_residual_wm2=0.0_8
    REAL(KIND=8) :: inner_surface_k=0.0_8
    REAL(KIND=8) :: bare_roof_conv_wm2=0.0_8
    REAL(KIND=8) :: bare_roof_rad_wm2=0.0_8
    REAL(KIND=8) :: mixed_roof_conv_wm2=0.0_8
    REAL(KIND=8) :: mixed_roof_rad_wm2=0.0_8
    REAL(KIND=8) :: layer_capacity_jm3k(4)=0.0_8
    REAL(KIND=8) :: layer_conductivity_wmk(4)=0.0_8
    REAL(KIND=8) :: layer_interface_wm2(3)=0.0_8
  END TYPE greenroof_bem_diagnostics

CONTAINS

  SUBROUTINE greenroof_column_step(dt,dz,capacity,conductivity,told, &
                                  tsurface,troom,hconv,hrad,tnew,diag,dg_dtsurface,state_valid)
    REAL(KIND=8), INTENT(IN) :: dt,dz(4),capacity(4),conductivity(4),told(4)
    REAL(KIND=8), INTENT(IN) :: tsurface,troom,hconv,hrad
    REAL(KIND=8), INTENT(OUT) :: tnew(4),dg_dtsurface
    TYPE(greenroof_bem_diagnostics), INTENT(OUT) :: diag
    REAL(KIND=8) :: lower(4),diagonal(4),upper(4),rhs(4),response_rhs(4),response(4)
    REAL(KIND=8) :: conductance(3),g_top,g_room,h_room,rate(4),q_room,t_inner
    INTEGER :: k
    LOGICAL, OPTIONAL, INTENT(OUT) :: state_valid
    LOGICAL :: solve_valid

    IF (PRESENT(state_valid)) state_valid=.FALSE.
    tnew=told
    dg_dtsurface=0.0_8
    diag=greenroof_bem_diagnostics()
    IF (.NOT. ALL(ieee_is_finite([dt,tsurface,troom,hconv,hrad]))) THEN
      CALL greenroof_failure('greenroof_column_step: nonfinite boundary input',state_valid)
      RETURN
    END IF
    IF (.NOT. ALL(ieee_is_finite(dz)) .OR. .NOT. ALL(ieee_is_finite(capacity)) .OR. &
        .NOT. ALL(ieee_is_finite(conductivity)) .OR. .NOT. ALL(ieee_is_finite(told))) THEN
      CALL greenroof_failure('greenroof_column_step: nonfinite layer input',state_valid)
      RETURN
    END IF
    IF (dt <= 0.0_8 .OR. ANY(dz <= 0.0_8) .OR. ANY(capacity <= 0.0_8) .OR. &
        ANY(conductivity <= 0.0_8) .OR. hconv < 0.0_8 .OR. hrad < 0.0_8) THEN
      CALL greenroof_failure('greenroof_column_step: invalid thermal parameter',state_valid)
      RETURN
    END IF

    ! Resistances between cell centres are the two adjacent half layers.
    DO k=1,3
      conductance(k)=1.0_8/(0.5_8*dz(k)/conductivity(k)+ &
                           0.5_8*dz(k+1)/conductivity(k+1))
    END DO
    g_top=2.0_8*conductivity(1)/dz(1)
    h_room=hconv+hrad
    g_room=0.0_8
    IF (h_room > 0.0_8) &
      g_room=1.0_8/(0.5_8*dz(4)/conductivity(4)+1.0_8/h_room)

    ! Backward Euler; each interface flux appears with opposite signs in
    ! its two adjacent cells. No fixed deep-soil boundary remains in this tile.
    rate=capacity*dz/dt
    lower=0.0_8
    upper=0.0_8
    diagonal=rate
    rhs=rate*told
    DO k=1,3
      diagonal(k)=diagonal(k)+conductance(k)
      diagonal(k+1)=diagonal(k+1)+conductance(k)
      upper(k)=-conductance(k)
      lower(k+1)=-conductance(k)
    END DO
    diagonal(1)=diagonal(1)+g_top
    rhs(1)=rhs(1)+g_top*tsurface
    diagonal(4)=diagonal(4)+g_room
    rhs(4)=rhs(4)+g_room*troom
    CALL solve_tridiagonal(lower,diagonal,upper,rhs,tnew,solve_valid)
    IF (.NOT. solve_valid) THEN
      CALL greenroof_failure('greenroof_column_step: invalid temperature pivot',state_valid)
      RETURN
    END IF

    ! Analytic surface derivative used by the coupled surface-energy solve.
    response_rhs=0.0_8
    response_rhs(1)=g_top
    CALL solve_tridiagonal(lower,diagonal,upper,response_rhs,response,solve_valid)
    IF (.NOT. solve_valid) THEN
      CALL greenroof_failure('greenroof_column_step: invalid derivative pivot',state_valid)
      RETURN
    END IF
    dg_dtsurface=g_top*(1.0_8-response(1))
    q_room=g_room*(tnew(4)-troom)
    IF (h_room > 0.0_8) THEN
      t_inner=troom+q_room/h_room
    ELSE
      t_inner=tnew(4)
    END IF

    diag%active=.TRUE.
    diag%surface_to_column_wm2=g_top*(tsurface-tnew(1))
    DO k=1,3
      diag%layer_interface_wm2(k)=conductance(k)*(tnew(k)-tnew(k+1))
    END DO
    diag%soil_to_roof_wm2=diag%layer_interface_wm2(3)
    diag%roof_to_indoor_wm2=q_room
    ! Split exactly the same room flux that leaves the column, not a second
    ! calculation using the concrete cell-centre temperature as a surface.
    IF (h_room > 0.0_8) THEN
      diag%roof_conv_wm2=q_room*hconv/h_room
      diag%roof_rad_wm2=q_room*hrad/h_room
    END IF
    diag%inner_surface_k=t_inner
    diag%storage_wm2=SUM(rate*(tnew-told))
    diag%solver_residual_wm2=diag%surface_to_column_wm2-q_room-diag%storage_wm2
    diag%column_residual_wm2=diag%solver_residual_wm2
    diag%layer_capacity_jm3k=capacity
    diag%layer_conductivity_wmk=conductivity
    IF (.NOT. ALL(ieee_is_finite(tnew)) .OR. .NOT. ieee_is_finite(dg_dtsurface)) THEN
      CALL greenroof_failure('greenroof_column_step: nonfinite solution',state_valid)
      RETURN
    END IF
    IF (PRESENT(state_valid)) state_valid=.TRUE.
  END SUBROUTINE greenroof_column_step

  SUBROUTINE solve_tridiagonal(lower,diagonal,upper,rhs,solution,valid)
    REAL(KIND=8), INTENT(IN) :: lower(4),diagonal(4),upper(4),rhs(4)
    REAL(KIND=8), INTENT(OUT) :: solution(4)
    REAL(KIND=8) :: u(4),r(4),pivot
    INTEGER :: k
    LOGICAL, INTENT(OUT) :: valid
    valid=.FALSE.
    pivot=diagonal(1)
    IF (.NOT. ieee_is_finite(pivot)) RETURN
    IF (pivot <= 0.0_8) RETURN
    u(1)=upper(1)/pivot
    r(1)=rhs(1)/pivot
    DO k=2,4
      pivot=diagonal(k)-lower(k)*u(k-1)
      IF (.NOT. ieee_is_finite(pivot)) RETURN
    IF (pivot <= 0.0_8) RETURN
      u(k)=upper(k)/pivot
      r(k)=(rhs(k)-lower(k)*r(k-1))/pivot
    END DO
    solution(4)=r(4)
    DO k=3,1,-1
      solution(k)=r(k)-u(k)*solution(k+1)
    END DO
    valid=ALL(ieee_is_finite(solution))
  END SUBROUTINE solve_tridiagonal
  SUBROUTINE greenroof_failure(reason,state_valid)
    CHARACTER(LEN=*), INTENT(IN) :: reason
    LOGICAL, OPTIONAL, INTENT(OUT) :: state_valid
    IF (PRESENT(state_valid)) THEN
      state_valid=.FALSE.
    ELSE
      WRITE(*,'(A)') TRIM(reason)
      ERROR STOP 1
    END IF
  END SUBROUTINE greenroof_failure
END MODULE module_sf_greenroof_bem
