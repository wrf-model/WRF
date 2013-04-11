! Rosenbrock integrator with manual time step control
! Solve: d/dt c = f(t,c)

! written by Edwin Spee, CWI, Amsterdam. Last update: July 28, 1997
! email: Edwin.Spee@cwi.nl (http://edwin-spee.mypage.org/)
! adapted to KPP-2.1 by Rolf Sander, Max-Planck Institute, Mainz, Germany, 2005
!
! Integration method for Ros2:
! C_{n+1} = C_n + 3/2 dt k_1 + 1/2 dt k_2
! k_1 = S f(t_n, C_n)
! k_2 = S [f(t_{n+1},C_n + dt k_1) - 2 k_1]
!
! where g = 1.0 + sqrt(0.5_dp),
! S = (I - g dt J ) ^ {-1}
! with J the Jacobian

MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Precision, ONLY: dp

  IMPLICIT NONE
  PUBLIC
  SAVE

  ! description of the error numbers IERR
  CHARACTER(LEN=50), PARAMETER, DIMENSION(1) :: IERR_NAMES = (/ &
    'dummy value                                       ' /)

CONTAINS

  ! **************************************************************************

  SUBROUTINE INTEGRATE( TIN, TOUT, &
    ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )

    IMPLICIT NONE

    KPP_REAL, INTENT(IN) :: TIN  ! TIN - Start Time
    KPP_REAL, INTENT(IN) :: TOUT ! TOUT - End Time
    ! Optional input parameters and statistics
    INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
    KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
    KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
    INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

    CALL ROS2(TIN, TOUT)

    ! if optional parameters are given for output
    ! use them to store information in them
    IF (PRESENT(ISTATUS_U)) ISTATUS_U(:) = 0
    IF (PRESENT(RSTATUS_U)) THEN
      RSTATUS_U(:) = 0._dp
      RSTATUS_U(1)=TOUT ! put final time into RSTATUS_U
    ENDIF
    IF (PRESENT(IERR_U)) IERR_U = 1 ! dummy value

  END SUBROUTINE INTEGRATE

  ! **************************************************************************

  SUBROUTINE ROS2(Tstart,Tend)

    USE KPP_ROOT_Jacobian,         ONLY: Jac_SP
    USE KPP_ROOT_Global,           ONLY: VAR,   & ! VARiable species
                                         FIX,   & ! FIXed species
                                         RCONST   ! rate coefficients
    USE KPP_ROOT_JacobianSP,       ONLY: LU_DIAG
    USE KPP_ROOT_Function,         ONLY: Fun
    USE KPP_ROOT_LinearAlgebra,    ONLY: KppDecomp, KppSolve
    USE KPP_ROOT_Parameters,       ONLY: NVAR, LU_NONZERO

    IMPLICIT NONE

    KPP_REAL, INTENT(IN) :: Tstart, Tend

    KPP_REAL :: dt ! time step                                          
    KPP_REAL :: k1(NVAR), k2(NVAR), w1(NVAR), g, jvs(LU_NONZERO)
    INTEGER ising, i

    dt = Tend - Tstart
    g = 1.0 + SQRT(0.5_dp)
    CALL JAC_sp(VAR, FIX, RCONST, jvs)
    jvs(1:LU_NONZERO) = -g*dt*jvs(1:LU_NONZERO)

    ! Rolf von Kuhlmann:
    ! optionally cut this out and replace it by directly addressed statements
    DO i=1,NVAR
      jvs(LU_DIAG(i)) = jvs(LU_DIAG(i)) + 1.0_dp
    END DO

    CALL KppDecomp (jvs, ising)

    IF (ising /= 0) THEN
      PRINT *,'ising <> 0, dt=',dt
      STOP
    END IF

    CALL Fun(VAR, FIX, RCONST, k1 )

    CALL KppSolve (jvs,k1)

    DO i = 1,NVAR
      w1(i) = MAX(0.0_dp, VAR(i) + dt * k1(i) )
    END DO

    CALL Fun(w1, FIX, RCONST, k2 )

    k2(1:NVAR) = k2(1:NVAR) - 2.0_dp*k1(1:NVAR)
    CALL KppSolve (jvs,k2)
    DO i = 1,NVAR
      VAR(i) = MAX( 0.0_dp, VAR(i)+1.5_dp*dt*k1(i)+0.5_dp*dt*k2(i) )
    END DO

  END SUBROUTINE ROS2

  ! **************************************************************************

END MODULE KPP_ROOT_Integrator
