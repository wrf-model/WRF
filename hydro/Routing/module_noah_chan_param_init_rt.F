MODULE module_noah_chan_param_init_rt


CONTAINS
!
!-----------------------------------------------------------------
  SUBROUTINE CHAN_PARM_INIT (BOTWID,HLINK_INIT,CHAN_SS,CHMann)
!-----------------------------------------------------------------

    IMPLICIT NONE

    integer :: IINDEX, CHANCATS
    integer :: ORDER
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
    OPEN(19, FILE='CHANPARM.TBL',FORM='FORMATTED',STATUS='OLD')
    READ (19,*)
    READ (19,2000,END=2002) DATATYPE
#ifdef HYDRO_D
    PRINT *, DATATYPE
#endif
    READ (19,*)CHANCATS,IINDEX
2000 FORMAT (A11)

!-----Read in Channel Parameters as functions of stream order...

    IF(DATATYPE.EQ.'StreamOrder')THEN
#ifdef HYDRO_D
       PRINT *, 'CHANNEL DATA SOURCE TYPE = ',DATATYPE,' FOUND',           &
            CHANCATS,' CATEGORIES'
#endif
       DO ORDER=1,CHANCATS
          READ (19,*)IINDEX,BOTWID(ORDER),HLINK_INIT(ORDER),CHAN_SS(ORDER),   &
               &     CHMann(ORDER)
          PRINT *, IINDEX,BOTWID(ORDER),HLINK_INIT(ORDER),CHAN_SS(ORDER),   &
               &     CHMann(ORDER)
       ENDDO
    ENDIF


!-----Read in Channel Parameters as functions of ???other method??? (TBC)...


2002 CONTINUE

    CLOSE (19)
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
