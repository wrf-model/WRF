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

MODULE module_noah_chan_param_init_rt


CONTAINS
!
!-----------------------------------------------------------------
  SUBROUTINE CHAN_PARM_INIT (BOTWID,HLINK_INIT,CHAN_SS,CHMann)
!-----------------------------------------------------------------

    IMPLICIT NONE

    integer :: IINDEX, CHANCATS
    integer :: ORDER, IUNIT
    integer, PARAMETER :: NCHANTYPES=50 
    real,dimension(NCHANTYPES)    :: BOTWID,HLINK_INIT,CHAN_SS,CHMann
    character(LEN=11) :: DATATYPE

!-----SPECIFY CHANNEL RELATED CHARACTERISTICS :
!             ORDER: Strahler Stream Order
!            BOTWID: Channel Bottom Width (meters)
!        HLINK_INIT: Initial depth of flow in channel (meters)
!           CHAN_SS: Channel side slope (assuming trapezoidal channel geom)
!            CHMann: Channel Manning's N roughness coefficient 


!-----READ IN CHANNEL PROPERTIES FROM CHANPARM.TBL :
    IUNIT = 23
    OPEN(IUNIT, &
#ifndef NCEP_WCOSS
    FILE='CHANPARM.TBL', &
#endif
    FORM='FORMATTED',STATUS='OLD')
    READ (IUNIT,*)
    READ (IUNIT,2000,END=2002) DATATYPE
#ifdef HYDRO_D
    PRINT *, DATATYPE
#endif
    READ (IUNIT,*)CHANCATS,IINDEX
2000 FORMAT (A11)

!-----Read in Channel Parameters as functions of stream order...

    IF(DATATYPE.EQ.'StreamOrder')THEN
#ifdef HYDRO_D
       PRINT *, 'CHANNEL DATA SOURCE TYPE = ',DATATYPE,' FOUND',           &
            CHANCATS,' CATEGORIES'
#endif
       DO ORDER=1,CHANCATS
          READ (IUNIT,*)IINDEX,BOTWID(ORDER),HLINK_INIT(ORDER),CHAN_SS(ORDER),   &
               &     CHMann(ORDER)
#ifdef HYDRO_D
          PRINT *, IINDEX,BOTWID(ORDER),HLINK_INIT(ORDER),CHAN_SS(ORDER),   &
               &     CHMann(ORDER)
#endif
       ENDDO
    ENDIF


!-----Read in Channel Parameters as functions of ???other method??? (TBC)...


2002 CONTINUE

    CLOSE (IUNIT)
  END SUBROUTINE CHAN_PARM_INIT



#ifdef MPP_LAND
  SUBROUTINE mpp_CHAN_PARM_INIT (BOTWID,HLINK_INIT,CHAN_SS,CHMann)
    use module_mpp_land, only:  my_id, IO_id,mpp_land_bcast_int1, &
       mpp_land_bcast_real,mpp_land_bcast_int,mpp_land_bcast_real1
    implicit none
    integer :: IINDEX, CHANCATS
    integer :: ORDER
    integer, PARAMETER :: NCHANTYPES=50 
    real,dimension(NCHANTYPES)    :: BOTWID,HLINK_INIT,CHAN_SS,CHMann
    character(LEN=11) :: DATATYPE

    if(my_id.eq.io_id) then
       call CHAN_PARM_INIT(BOTWID,HLINK_INIT,CHAN_SS,CHMann)
    end if
       call mpp_land_bcast_real(NCHANTYPES,BOTWID)
       call mpp_land_bcast_real(NCHANTYPES,HLINK_INIT)
       call mpp_land_bcast_real(NCHANTYPES,CHAN_SS)
       call mpp_land_bcast_real(NCHANTYPES,CHMann)
    return 
    END SUBROUTINE mpp_CHAN_PARM_INIT
#endif
!-----------------------------------------------------------------
!-----------------------------------------------------------------


END MODULE module_Noah_chan_param_init_rt
