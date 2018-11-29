!  Program Name:
!  Author(s)/Contact(s):
!  Abstract:
!  History Log:
! 
!  Usage:
!  Parameters: <Specify typical arguments passed>
!  Input Files:
!        <list file names and briefly describe the data they include>
!  Output Files:
!        <list file names and briefly describe the information they include>
! 
!  Condition codes:
!        <list exit condition or error codes returned >
!        If appropriate, descriptive troubleshooting instructions or
!        likely causes for failures could be mentioned here with the
!        appropriate error code
! 
!  User controllable options: <if applicable>

MODULE module_Noahlsm_utility 
 
  REAL, PARAMETER      :: CP = 1004.5, RD = 287.04, SIGMA = 5.67E-8,    &
                          CPH2O = 4.218E+3,CPICE = 2.106E+3,            &
                          LSUBF = 3.335E+5

CONTAINS

  SUBROUTINE CALTMP(T1, SFCTMP, SFCPRS, ZLVL, Q2, TH2, T1V, TH2V, RHO ) 

    IMPLICIT NONE

    ! Input:
    REAL, INTENT(IN)       :: T1     ! Skin temperature (K)
    REAL, INTENT(IN)       :: SFCTMP ! Air temperature (K) at level ZLVL
    REAL, INTENT(IN)       :: Q2     ! Specific Humidity (kg/kg) at level ZLVL
    REAL, INTENT(IN)       :: SFCPRS ! Atmospheric pressure (Pa) at level ZLVL
    REAL, INTENT(IN)       :: ZLVL   ! Height (m AGL) where atmospheric fields are valid

    ! Output:
    REAL, INTENT(OUT)      :: TH2    ! Potential temperature (considering the reference pressure to be at the surface)
    REAL, INTENT(OUT)      :: T1V    ! Virtual skin temperature (K)
    REAL, INTENT(OUT)      :: TH2V   ! Virtual potential temperature at ZLVL
    REAL, INTENT(OUT)      :: RHO    ! Density

    ! Local:
    REAL                   :: T2V

    TH2 = SFCTMP + ( 0.0098 * ZLVL)
    T1V= T1 * (1.0+ 0.61 * Q2) 
    TH2V = TH2 * (1.0+ 0.61 * Q2)
    T2V = SFCTMP * ( 1.0 + 0.61 * Q2 )
    RHO = SFCPRS/(RD * T2V)

  END SUBROUTINE CALTMP

  SUBROUTINE CALHUM(SFCTMP, SFCPRS, Q2SAT, DQSDT2)

    IMPLICIT NONE
  
    ! Input:
    REAL, INTENT(IN)       :: SFCTMP
    REAL, INTENT(IN)       :: SFCPRS
    
    ! Output:
    REAL, INTENT(OUT)      :: Q2SAT   ! Saturated specific humidity
    REAL, INTENT(OUT)      :: DQSDT2 
    
    ! Local
    REAL, PARAMETER        :: A2=17.67,A3=273.15,A4=29.65, ELWV=2.501E6,        &
                              A23M4=A2*(A3-A4), E0=611.0, RV=461.0,             &
                              EPSILON=0.622 
    REAL                   :: ES

    ! ES:  e.g. Dutton chapter 8, eq 11
        ES = E0 * EXP ( ELWV/RV*(1./A3 - 1./SFCTMP) )

    ! Q2SAT:  
        Q2SAT = EPSILON * ES / (SFCPRS-(1-EPSILON)*ES)

! DQSDT2 is calculated assuming Q2SAT is a specific humidity
        !KWM DQSDT2=(Q2SAT/(1+Q2SAT))*A23M4/(SFCTMP-A4)**2 
        DQSDT2=Q2SAT*A23M4/(SFCTMP-A4)**2 

        END SUBROUTINE CALHUM
 

END MODULE module_Noahlsm_utility

subroutine wrf_error_fatal(string)
  implicit none
  character(len=*), intent(in) :: string
  print*, string
  stop
end subroutine wrf_error_fatal

subroutine wrf_dm_bcast_real(rval, ival)
  implicit none
  real,    intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_real

subroutine wrf_dm_bcast_integer(rval, ival)
  implicit none
  integer, intent(in) :: rval
  integer, intent(in) :: ival
end subroutine wrf_dm_bcast_integer

subroutine wrf_dm_bcast_string(rval, ival)
  implicit none
  character(len=*), intent(in) :: rval
  integer,          intent(in) :: ival
end subroutine wrf_dm_bcast_string
