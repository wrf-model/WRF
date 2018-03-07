 SUBROUTINE jcdfi_init_coef 
 
    USE module_domain, ONLY : domain, head_grid
    USE module_utility

    IMPLICIT NONE

    INTEGER :: nstep2, rundfi, rc, i
    TYPE(WRFU_TimeInterval) :: run_interval
    REAL :: timestep

    INTERFACE
       SUBROUTINE optfil_driver(grid)
          USE module_domain, ONLY : domain
          TYPE (domain), POINTER :: grid
       END SUBROUTINE optfil_driver
    END INTERFACE
 
    head_grid%itimestep=0

    CALL optfil_driver(head_grid)

    timestep=abs(head_grid%dt)
    run_interval = head_grid%stop_subtime - head_grid%start_subtime

    CALL WRFU_TimeIntervalGet( run_interval, S=rundfi, rc=rc )
    rundfi = abs(rundfi)

    nstep2= ceiling((1.0 + real(rundfi)/timestep) / 2.0)

    IF ( MOD(int(1.0 + real(rundfi)/timestep),2) /= 0 ) THEN
       DO i=1,2*nstep2-1
          head_grid%hcoeff(i) = -1.0 * head_grid%hcoeff(i)
       END DO
    ELSE
       DO i=1,2*nstep2
          head_grid%hcoeff(i) = -1.0 * head_grid%hcoeff(i)
       END DO
    END IF

    head_grid%hcoeff(nstep2) = 1.0 + head_grid%hcoeff(nstep2) 

 END SUBROUTINE jcdfi_init_coef

 SUBROUTINE jcdfi_zero_forcing ( grid )

    USE module_domain, ONLY : domain

    IMPLICIT NONE

    !  Input data.

    TYPE(domain) , INTENT(INOUT) :: grid
 
    grid%itimestep = 0

    grid%jcdfi_u  = 0.0
    grid%jcdfi_v  = 0.0
    grid%jcdfi_t  = 0.0
    grid%jcdfi_p  = 0.0

 END SUBROUTINE jcdfi_zero_forcing

 SUBROUTINE jcdfi_tl (grid)

    USE module_domain, ONLY : domain

    IMPLICIT NONE

    !  Input data.

    TYPE(domain) , INTENT(INOUT) :: grid

    REAL :: hn
 
    hn = grid%hcoeff(grid%itimestep+1)

    grid%jcdfi_u  = grid%jcdfi_u  + hn * grid%g_u_2
    grid%jcdfi_v  = grid%jcdfi_v  + hn * grid%g_v_2
    grid%jcdfi_t  = grid%jcdfi_t  + hn * grid%g_t_2
    grid%jcdfi_p  = grid%jcdfi_p  + hn * grid%g_p

 END SUBROUTINE jcdfi_tl

 SUBROUTINE jcdfi_add_forcing ( grid )

    USE module_domain, ONLY : domain

    IMPLICIT NONE

    TYPE ( domain ) , INTENT(INOUT) :: grid

    REAL :: hn
 
    hn = grid%hcoeff(grid%itimestep+1)

    grid%a_u_2  = grid%a_u_2  + hn * grid%jcdfi_u 
    grid%a_v_2  = grid%a_v_2  + hn * grid%jcdfi_v
    grid%a_t_2  = grid%a_t_2  + hn * grid%jcdfi_t 
    grid%a_p    = grid%a_p    + hn * grid%jcdfi_p
 END SUBROUTINE jcdfi_add_forcing

