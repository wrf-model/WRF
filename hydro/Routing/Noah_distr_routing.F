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

!DJG ------------------------------------------------
!DJG   SUBROUTINE RT_PARM
!DJG ------------------------------------------------

SUBROUTINE RT_PARM(IX,JY,IXRT,JXRT,VEGTYP,RETDP,OVRGH,  &
        AGGFACTR)
#ifdef MPP_LAND
    use module_mpp_land, only: left_id,down_id,right_id,&
        up_id,mpp_land_com_real,MPP_LAND_UB_COM, &
        MPP_LAND_LR_COM,mpp_land_com_integer
#endif

    IMPLICIT NONE

    !DJG -------- DECLARATIONS -----------------------

    INTEGER, INTENT(IN) :: IX,JY,IXRT,JXRT,AGGFACTR

    INTEGER, INTENT(IN), DIMENSION(IX,JY)	:: VEGTYP
    REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)	:: RETDP
    REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)	:: OVRGH


    !DJG Local Variables

    INTEGER	:: I,J,IXXRT,JYYRT
    INTEGER :: AGGFACYRT,AGGFACXRT


    !DJG Assign RETDP and OVRGH based on VEGTYP...

    do J=1,JY
        do I=1,IX

            do AGGFACYRT=AGGFACTR-1,0,-1
                do AGGFACXRT=AGGFACTR-1,0,-1

                    IXXRT=I*AGGFACTR-AGGFACXRT
                    JYYRT=J*AGGFACTR-AGGFACYRT
#ifdef MPP_LAND
                    if(left_id.ge.0) IXXRT=IXXRT+1
                    if(down_id.ge.0) JYYRT=JYYRT+1
#else
                    !yw ????
                    !       IXXRT=IXXRT+1
                    !       JYYRT=JYYRT+1
#endif

                    !        if(AGGFACTR .eq. 1) then
                    !            IXXRT=I
                    !            JYYRT=J
                    !        endif



                    !DJG Urban, rock, playa, snow/ice...
                    IF (VEGTYP(I,J).EQ.1.OR.VEGTYP(I,J).EQ.26.OR.   &
                            VEGTYP(I,J).EQ.26.OR.VEGTYP(I,J).EQ.24) THEN
                        RETDP(IXXRT,JYYRT)=1.3
                        OVRGH(IXXRT,JYYRT)=0.1
                        !DJG Wetlands and water bodies...
                    ELSE IF (VEGTYP(I,J).EQ.17.OR.VEGTYP(I,J).EQ.18.OR.  &
                            VEGTYP(I,J).EQ.19.OR.VEGTYP(I,J).EQ.16) THEN
                        RETDP(IXXRT,JYYRT)=10.0
                        OVRGH(IXXRT,JYYRT)=0.2
                        !DJG All other natural covers...
                    ELSE
                        RETDP(IXXRT,JYYRT)=5.0
                        OVRGH(IXXRT,JYYRT)=0.2
                    END IF

                end do
            end do

        end do
    end do
#ifdef MPP_LAND
    call MPP_LAND_COM_REAL(RETDP,IXRT,JXRT,99)
    call MPP_LAND_COM_REAL(OVRGH,IXRT,JXRT,99)
#endif

    !DJG ----------------------------------------------------------------
END SUBROUTINE RT_PARM
!DJG ----------------------------------------------------------------


!DJG ----------------------------------------------------------------
SUBROUTINE GETMAX8DIR(IXX0,JYY0,I,J,H,RETENT_DEP,sox,tmp_gsize,max,XX,YY)
    implicit none
    INTEGER:: IXX0,JYY0,IXX8,JYY8, XX, YY
    INTEGER, INTENT(IN) :: I,J

    REAL,INTENT(IN) :: H(XX,YY),RETENT_DEP(XX,YY),sox(XX,YY,8),tmp_gsize(9)
    REAL  max
    IXX0 = -1
    max = 0
    if (h(I,J).LE.retent_dep(I,J)) return

    IXX8 = I
    JYY8 = J+1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,1),IXX0,JYY0,max,tmp_gsize(1),XX,YY)

    IXX8 = I+1
    JYY8 = J+1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,2),IXX0,JYY0,max,tmp_gsize(2),XX,YY)

    IXX8 = I+1
    JYY8 = J
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,3),IXX0,JYY0,max,tmp_gsize(3),XX,YY)

    IXX8 = I+1
    JYY8 = J-1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,4),IXX0,JYY0,max,tmp_gsize(4),XX,YY)

    IXX8 = I
    JYY8 = J-1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,5),IXX0,JYY0,max,tmp_gsize(5),XX,YY)

    IXX8 = I-1
    JYY8 = J-1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,6),IXX0,JYY0,max,tmp_gsize(6),XX,YY)

    IXX8 = I-1
    JYY8 = J
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,7),IXX0,JYY0,max,tmp_gsize(7),XX,YY)

    IXX8 = I-1
    JYY8 = J+1
    call GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox(:,:,8),IXX0,JYY0,max,tmp_gsize(8),XX,YY)
    RETURN
END SUBROUTINE GETMAX8DIR

SUBROUTINE GET8DIR(IXX8,JYY8,I,J,H,RETENT_DEP,sox   &
        ,IXX0,JYY0,max,tmp_gsize,XX,YY)
    implicit none
    integer,INTENT(INOUT) ::IXX0,JYY0
    INTEGER, INTENT(IN) :: I,J,IXX8,JYY8,XX,YY
    REAL,INTENT(IN) :: H(XX,YY),RETENT_DEP(XX,YY),sox(XX,YY)
    REAL, INTENT(INOUT) ::max
    real, INTENT(IN) :: tmp_gsize
    real :: sfx

    sfx = sox(i,j)-(h(IXX8,JYY8)-h(i,j))*0.001/tmp_gsize
    if(sfx .le. 0 ) return
    if(max < sfx ) then
        IXX0 = IXX8
        JYY0 = JYY8
        max = sfx
    end if

END SUBROUTINE GET8DIR

!DJG------------------------------------------------------------


SUBROUTINE GETSUB8(I, J, XX, YY, wattbl, terrslpNeighbors, distNeighbors, &
                   maxneighI, maxneighJ, maxneighIndx, maxneighSlp)
   implicit none
   integer, intent(in)    :: I, J ! i, j indices for cell of interest
   integer, intent(in)    :: XX, YY ! rt domain dimensions
   real, intent(in)       :: wattbl(XX, YY) ! water table depth domain array (m)
   real, intent(in)       :: terrslpNeighbors(XX, YY, 8)
                             ! terrain slope 2d domain array for all 8 neighbors (m/m)
   real, intent(in)       :: distNeighbors(9)
                             ! lateral distance array for all 8 neighbors of I,J cell (m)
   integer, intent(inout) :: maxneighI, maxneighJ ! i, j for max terrain+head slope neighbor
   integer, intent(inout) :: maxneighIndx
                             ! neighbor cell direction index for max terrain+head slope neighbor
   real, intent(inout)    :: maxneighSlp ! terrain+head slope for max slope neighbor (m/m)

   ! Local variables
   integer                :: neighIndx ! local neighbor direction index (1-8)
   integer                :: neighI(8), neighJ(8) ! arrays of neighbor i, j indices

   ! Initialize maxslp vars to negative in case max cannot be found
   maxneighSlp = -1

   ! Setup i, j arrays for neighbors, starting north and rotating clockwise
   neighI = (/ I,   I+1, I+1, I+1, I,   I-1, I-1, I-1 /)
   neighJ = (/ J+1, J+1, J,   J-1, J-1, J-1, J,   J+1 /)

   ! Loop through all neighbors to update max elev+head slope neighbor
   do neighIndx = 1, 8
      call GETSUB8DIR(I, J, wattbl(I, J), &
                      neighI(neighIndx), neighJ(neighIndx), neighIndx, &
                      wattbl(neighI(neighIndx), neighJ(neighIndx)), &
                      terrslpNeighbors(I,J,neighIndx), distNeighbors(neighIndx), &
                      maxneighI, maxneighJ, maxneighIndx, maxneighSlp)
   enddo
   RETURN
END SUBROUTINE GETSUB8

SUBROUTINE GETSUB8DIR(I, J, selfWattbl, &
                      neighI, neighJ, neighIndx, neighWattbl, &
                      neighTerrslp, neighDist, &
                      maxneighI, maxneighJ, maxneighIndx, maxneighSlp)
   implicit none
   integer, intent(in)    :: I, J ! i, j indices for cell of interest
   real, intent(in)       :: selfWattbl ! water table depth for cell of interest (m)
   integer, intent(in)    :: neighI, neighJ ! neighbor cell i, j indices
   integer, intent(in)    :: neighIndx ! neighbor cell direction index (1-8)
   real, intent(in)       :: neighWattbl ! water table depth for specified neighbor index (m)
   real, intent(in)       :: neighTerrslp
                             ! terrain slope for specified neighbor index (m/m)
   real, intent(in)       :: neighDist
                             ! lateral distance for specified neighbor index (m)
   integer, intent(inout) :: maxneighI, maxneighJ ! i, j for max terrain+head slope neighbor
   integer, intent(inout) :: maxneighIndx
                             ! neighbor cell direction index for max terrain+head slope neighbor
   real, intent(inout)    :: maxneighSlp
                             ! terrain+head slope for max terrain+head slope neighbor (m/m)

   ! Local variables
   real                   :: dzdx ! subsurface head slope (m/m)
   real                   :: beta ! total terrain+head slope (m/m)

   ! Calculate total head slope
   ! NOTE: wattbl is depth of water table from surface (e.g., 0m if saturated column,
   ! 2m if no water table present). So total head slope should be:
   !    beta = ( (elev - wattbl) - (elev_neigh - wattbl_neigh) ) / distance
   !         = neighTerrslp - dzdx
   dzdx = ( selfWattbl - neighWattbl ) / neighDist
   beta = neighTerrslp - dzdx
   ! Check if this is max and update tracking variables
   if ( maxneighSlp < beta ) then
      maxneighI = neighI
      maxneighJ = neighJ
      maxneighSlp = beta
      maxneighIndx = neighIndx
   end if
END SUBROUTINE GETSUB8DIR


!DJG-----------------------------------------------------------------------
!DJG SUBROUTINE TER_ADJ_SOL    - Terrain adjustment of incoming solar radiation
!DJG-----------------------------------------------------------------------
SUBROUTINE TER_ADJ_SOL(IX,JX,SO8LD_D,TSLP,SHORT,XLAT,XLONG,olddate,DT)

#ifdef MPP_LAND
    use module_mpp_land, only:  my_id, io_id, &
        mpp_land_bcast_int1
#endif
    implicit none
    integer,INTENT(IN)     :: IX,JX
    INTEGER,INTENT(in), DIMENSION(IX,JX,3)   :: SO8LD_D
    real,INTENT(IN), DIMENSION(IX,JX)  :: XLAT,XLONG
    real,INTENT(IN) :: DT
    real,INTENT(INOUT), DIMENSION(IX,JX)  :: SHORT
    character(len=19) :: olddate

    ! Local Variables...
    real, dimension(IX,JX) ::TSLP,TAZI
    real, dimension(IX,JX) ::SOLDN
    real :: SOLDEC,DGRD,ITIME2,HRANGLE
    real :: BINSH,SOLZANG,SOLAZI,INCADJ
    real :: TAZIR,TSLPR,LATR,LONR,SOLDNADJ
    integer :: JULDAY0,HHTIME0,MMTIME0,YYYY0,MM0,DD0
    integer :: JULDAY,HHTIME,MMTIME,YYYY,MM,DD
    integer :: I,J


    !----------------------------------------------------------------------
    !  SPECIFY PARAMETERS and VARIABLES
    !----------------------------------------------------------------------

    JULDAY = 0
    SOLDN = SHORT
    DGRD = 3.14159/180.

    ! Set up time variables...
#ifdef MPP_LAND
    if(my_id .eq. IO_id) then
#endif
        read(olddate(1:4),"(I4)") YYYY0 ! real-time year (GMT)
        read(olddate(6:7),"(I2.2)") MM0 ! real-time month (GMT)
        read(olddate(9:10),"(I2.2)") DD0 ! real-time day (GMT)
        read(olddate(12:13),"(I2.2)") HHTIME0 ! real-time hour (GMT)
        read(olddate(15:16),"(I2.2)") MMTIME0 ! real-time minutes (GMT)
#ifdef MPP_LAND
    endif
    call mpp_land_bcast_int1(YYYY0)
    call mpp_land_bcast_int1(MM0)
    call mpp_land_bcast_int1(DD0)
    call mpp_land_bcast_int1(HHTIME0)
    call mpp_land_bcast_int1(MMTIME0)
#endif


    ! Set up terrain variables...(returns TSLP&TAZI in radians)
    call SLOPE_ASPECT(IX,JX,SO8LD_D,TAZI)

    !----------------------------------------------------------------------
    !  BEGIN LOOP THROUGH GRID
    !----------------------------------------------------------------------
    DO J=1,JX
        DO I=1,IX
            YYYY = YYYY0
            MM  = MM0
            DD  = DD0
            HHTIME = HHTIME0
            MMTIME = MMTIME0
            call GMT2LOCAL(1,1,XLONG(i,j),YYYY,MM,DD,HHTIME,MMTIME,DT)
            call JULDAY_CALC(YYYY,MM,DD,JULDAY)

            ! Convert to radians...
            LATR = XLAT(I,J)   !send solsub local lat in deg
            LONR = XLONG(I,J)   !send solsub local lon in deg
            TSLPR = TSLP(I,J)/DGRD !send solsub local slp in deg
            TAZIR = TAZI(I,J)/DGRD !send solsub local azim in deg

            !Call SOLSUB to return terrain adjusted incoming solar radiation...
            ! SOLSUB taken from Whiteman and Allwine, 1986, Environ. Software.

            call SOLSUB(LONR,LATR,TAZIR,TSLPR,SOLDN(I,J),YYYY,MM,         &
                DD,HHTIME,MMTIME,SOLDNADJ,SOLZANG,SOLAZI,INCADJ)

            SOLDN(I,J)=SOLDNADJ

        ENDDO
    ENDDO

    SHORT = SOLDN

    return
end SUBROUTINE TER_ADJ_SOL
!DJG-----------------------------------------------------------------------
!DJG END SUBROUTINE TER_ADJ_SOL
!DJG-----------------------------------------------------------------------


!DJG-----------------------------------------------------------------------
!DJG SUBROUTINE GMT2LOCAL
!DJG-----------------------------------------------------------------------
subroutine GMT2LOCAL(IX,JX,XLONG,YY,MM,DD,HH,MIN,DT)

    implicit none

    !!! Declare Passed Args.

    INTEGER, INTENT(INOUT) :: yy,mm,dd,hh,min
    INTEGER, INTENT(IN) :: IX,JX
    REAL,INTENT(IN), DIMENSION(IX,JX)  :: XLONG
    REAL,INTENT(IN) :: DT

    !!! Declare local variables

    integer :: i,j,minflag,hhflag,ddflag,mmflag,yyflag
    integer :: adj_min,lst_adj_min,lst_adj_hh,adj_hh
    real, dimension(IX,JX) :: TDIFF
    real :: tmp
    integer :: yyinit,mminit,ddinit,hhinit,mininit

    !!! Initialize flags
    hhflag=0
    ddflag=0
    mmflag=0
    yyflag=0

    !!! Set up constants...
    yyinit = yy
    mminit = mm
    ddinit = dd
    hhinit = hh
    mininit = min


    ! Loop through data...
    do j=1,JX
        do i=1,IX

            ! Reset yy,mm,dd...
            yy = yyinit
            mm = mminit
            dd = ddinit
            hh = hhinit
            min = mininit

            !!! Set up adjustments...
            !   - assumes +E , -W  longitude and 0.06667 hr/deg (=24/360)
            TDIFF(I,J) = XLONG(I,J)*0.06667   ! time offset in hr
            tmp = TDIFF(I,J)
            lst_adj_hh = INT(tmp)
            lst_adj_min = NINT(MOD(int(tmp),1)*60.) + int(DT/2./60.)  ! w/ 1/2 timestep adjustment...

            !!! Process Minutes...
            adj_min = min+lst_adj_min
            if (adj_min.lt.0) then
                min=60+adj_min
                lst_adj_hh = lst_adj_hh - 1
            else if (adj_min.ge.0.AND.adj_min.lt.60) then
                min=adj_min
            else if (adj_min.ge.60) then
                min=adj_min-60
                lst_adj_hh = lst_adj_hh + 1
            end if

            !!! Process Hours
            adj_hh = hh+lst_adj_hh
            if (adj_hh.lt.0) then
                hh = 24+adj_hh
                ddflag=1
            else if (adj_hh.ge.0.AND.adj_hh.lt.24) then
                hh=adj_hh
            else if (adj_hh.ge.24) then
                hh=adj_hh-24
                ddflag = 2
            end if



            !!! Process Days, Months, Years
            ! Subtract a day
            if (ddflag.eq.1) then
                if (dd.gt.1) then
                    dd=dd-1
                else
                    if (mm.eq.1) then
                        mm=12
                        yy=yy-1
                    else
                        mm=mm-1
                    end if
                    if ((mm.eq.1).or.(mm.eq.3).or.(mm.eq.5).or. &
                            (mm.eq.7).or.(mm.eq.8).or.(mm.eq.10).or. &
                            (mm.eq.12)) then
                        dd=31
                    else

                        !!! Adjustment for leap years!!!
                        if(mm.eq.2) then
                            if(MOD(yy,4).eq.0) then
                                dd=29
                            else
                                dd=28
                            end if
                        end if
                        if(mm.ne.2) dd=30
                    end if
                end if
            end if

            ! Add a day
            if (ddflag.eq.2) then
                if ((mm.eq.1).or.(mm.eq.3).or.(mm.eq.5).or. &
                        (mm.eq.7).or.(mm.eq.8).or.(mm.eq.10).or. &
                        (mm.eq.12)) then
                    if (dd.eq.31) then
                        dd=1
                        if (mm.eq.12) then
                            mm=1
                            yy=yy+1
                        else
                            mm=mm+1
                        end if
                    else
                        dd=dd+1
                    end if

                    !!! Adjustment for leap years!!!
                else if (mm.eq.2) then
                    if(MOD(yy,4).eq.0) then
                        if (dd.eq.29) then
                            dd=1
                            mm=3
                        else
                            dd=dd+1
                        end if
                    else
                        if (dd.eq.28) then
                            dd=1
                            mm=3
                        else
                            dd=dd+1
                        end if
                    end if
                else
                    if (dd.eq.30) then
                        dd=1
                        mm=mm+1
                    else
                        dd=dd+1
                    end if
                end if

            end if

        end do   !i-loop
    end do   !j-loop

    return
end subroutine

!DJG-----------------------------------------------------------------------
!DJG END SUBROUTINE GMT2LOCAL
!DJG-----------------------------------------------------------------------



!DJG-----------------------------------------------------------------------
!DJG SUBROUTINE JULDAY_CALC
!DJG-----------------------------------------------------------------------
subroutine JULDAY_CALC(YYYY,MM,DD,JULDAY)

    implicit none
    integer,intent(in) :: YYYY,MM,DD
    integer,intent(out) :: JULDAY

    integer :: resid
    integer julm(13)
    DATA JULM/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, &
        304, 334, 365 /

    integer LPjulm(13)
    DATA LPJULM/0, 31, 60, 91, 121, 152, 182, 213, 244, 274, &
        305, 335, 366 /

    resid = MOD(YYYY,4) !Set up leap year check...

    if (resid.ne.0) then    !If not a leap year....
        JULDAY = JULM(MM) + DD
    else                    !If a leap year...
        JULDAY = LPJULM(MM) + DD
    end if

    RETURN
END subroutine JULDAY_CALC
!DJG-----------------------------------------------------------------------
!DJG END SUBROUTINE JULDAY
!DJG-----------------------------------------------------------------------

!DJG-----------------------------------------------------------------------
!DJG SUBROUTINE SLOPE_ASPECT
!DJG-----------------------------------------------------------------------
subroutine SLOPE_ASPECT(IX,JX,SO8LD_D,TAZI)

    implicit none
    integer, INTENT(IN)		   :: IX,JX
    !	real,INTENT(in),DIMENSION(IX,JX)   :: TSLP  !terrain slope (m/m)
    real,INTENT(OUT),DIMENSION(IX,JX)   :: TAZI  !terrain aspect (deg)

    INTEGER, DIMENSION(IX,JX,3)   :: SO8LD_D
    real :: DGRD
    integer :: i,j

    !	TSLP = 0.  !Initialize as flat
    TAZI = 0.  !Initialize as north facing

    ! Find steepest descent slope and direction...
    do j=1,JX
        do i=1,IX
            !	TSLP(I,J) = TANH(Vmax(i,j)) ! calculate slope in radians...

            ! Convert steepest slope and aspect to radians...
            IF (SO8LD_D(i,j,3).eq.1) then
                TAZI(I,J) = 0.0
            ELSEIF (SO8LD_D(i,j,3).eq.2) then
                TAZI(I,J) = 45.0
            ELSEIF (SO8LD_D(i,j,3).eq.3) then
                TAZI(I,J) = 90.0
            ELSEIF (SO8LD_D(i,j,3).eq.4) then
                TAZI(I,J) = 135.0
            ELSEIF (SO8LD_D(i,j,3).eq.5) then
                TAZI(I,J) = 180.0
            ELSEIF (SO8LD_D(i,j,3).eq.6) then
                TAZI(I,J) = 225.0
            ELSEIF (SO8LD_D(i,j,3).eq.7) then
                TAZI(I,J) = 270.0
            ELSEIF (SO8LD_D(i,j,3).eq.8) then
                TAZI(I,J) = 315.0
            END IF

            DGRD = 3.141593/180.
            TAZI(I,J) = TAZI(I,J)*DGRD ! convert azimuth to radians...

        END DO
    END DO

    RETURN
END  subroutine SLOPE_ASPECT
!DJG-----------------------------------------------------------------------
!DJG END SUBROUTINE SLOPE_ASPECT
!DJG-----------------------------------------------------------------------

!DJG----------------------------------------------------------------
!DJG    SUBROUTINE SOLSUB
!DJG----------------------------------------------------------------
SUBROUTINE SOLSUB(LONG,LAT,AZ,IN,SC,YY,MO,IDA,IHR,MM,OUT1, &
        OUT2,OUT3,INCADJ)


    ! Notes....

    implicit none
    logical               :: daily, first
    integer               :: yy,mo,ida,ihr,mm,d
    integer,dimension(12) :: nday
    real                  :: lat,long,longcor,longsun,in,inslo
    real :: az,sc,out1,out2,out3,cosbeta,dzero,eccent,pi,calint
    real :: rtod,decmax,omega,onehr,omd,omdzero,rdvecsq,sdec
    real :: declin,cdec,arg,declon,sr,stdmrdn,b,em,timnoon,azslo
    real :: slat,clat,caz,saz,sinc,cinc,hinc,h,cosz,extra,extslo
    real :: t1,z,cosa,a,cosbeta_flat,INCADJ
    integer :: HHTIME,MMTIME,i,ik
    real, dimension(4) :: ACOF,BCOF

    ! Constants
    daily=.FALSE.
    ACOF(1) = 0.00839
    ACOF(2) = -0.05391
    ACOF(3) = -0.00154
    ACOF(4) = -0.0022
    BCOF(1) = -0.12193
    BCOF(2) = -0.15699
    BCOF(3) = -0.00657
    BCOF(4) = -0.00370
    DZERO = 80.
    ECCENT = 0.0167
    PI = 3.14159
    CALINT = 1.
    RTOD = PI / 180.
    DECMAX=(23.+26./60.)*RTOD
    OMEGA=2*PI/365.
    ONEHR=15.*RTOD

    ! Calculate Julian Day...
    D = 0
    call JULDAY_CALC(YY,MO,IDA,D)

    ! Ratio of radius vectors squared...
    OMD=OMEGA*D
    OMDZERO=OMEGA*DZERO
    !       RDVECSQ=1./(1.-ECCENT*COS(OMD))**2
    RDVECSQ = 1.    ! no adjustment for orbital changes when coupled to HRLDAS...

    ! Declination of sun...
    LONGSUN=OMEGA*(D-Dzero)+2.*ECCENT*(SIN(OMD)-SIN(OMDZERO))
    DECLIN=ASIN(SIN(DECMAX)*SIN(LONGSUN))
    SDEC=SIN(DECLIN)
    CDEC=COS(DECLIN)

    ! Check for Polar Day/night...
    ARG=((PI/2.)-ABS(DECLIN))/RTOD
    IF(ABS(LAT).GT.ARG) THEN
        IF((LAT.GT.0..AND.DECLIN.LT.0) .OR.       &
                (LAT.LT.0..AND.DECLON.GT.0.)) THEN
            OUT1 = 0.
            OUT2 = 0.
            OUT3 = 0.
            RETURN
        ENDIF
        SR=-1.*PI
    ELSE

        ! Calculate sunrise hour angle...
        SR=-1.*ABS(ACOS(-1.*TAN(LAT*RTOD)*TAN(DECLIN)))
    END IF

    ! Find standard meridian for site
    STDMRDN=NINT(LONG/15.)*15.
    LONGCOR=(LONG-STDMRDN)/15.

    ! Compute time correction from equation of time...
    B=2.*PI*(D-.4)/365
    EM=0.
    DO I=1,4
        EM=EM+(BCOF(I)*SIN(I*B)+ACOF(I)*COS(I*B))
    END DO

    ! Compute time of solar noon...
    TIMNOON=12.-EM-LONGCOR

    ! Set up a few more terms...
    AZSLO=AZ*RTOD
    INSLO=IN*RTOD
    SLAT=SIN(LAT*RTOD)
    CLAT=COS(LAT*RTOD)
    CAZ=COS(AZSLO)
    SAZ=SIN(AZSLO)
    SINC=SIN(INSLO)
    CINC=COS(INSLO)

    ! Begin solar radiation calculations...daily first, else instantaneous...
    IF (DAILY) THEN   ! compute daily integrated values...(Not used in HRLDAS!)
        IHR=0
        MM=0
        HINC=CALINT*ONEHR/60.
        IK=(2.*ABS(SR)/HINC)+2.
        FIRST=.TRUE.
        OUT1=0.
        DO I=1,IK
            H=SR+HINC*FLOAT(I-1)
            COSZ=SLAT*SDEC+CLAT*CDEC*COS(H)
            COSBETA=CDEC*((SLAT*COS(H))*(-1.*CAZ*SINC)- &
                SIN(H)*(SAZ*SINC)+(CLAT*COS(H))*CINC)+ &
                SDEC*(CLAT*(CAZ*SINC)+SLAT*CINC)
            EXTRA=SC*RDVECSQ*COSZ
            IF(EXTRA.LE.0.) EXTRA=0.
            EXTSLO=SC*RDVECSQ*COSBETA
            IF(EXTRA.LE.0. .OR. EXTSLO.LT.0.) EXTSLO=0.
            IF(FIRST .AND. EXTSLO.GT.0.) THEN
                OUT2=(H-HINC)/ONEHR+TIMNOON
                FIRST = .FALSE.
            END IF
            IF(.NOT.FIRST .AND. EXTSLO.LE.0.) OUT3=H/ONEHR+TIMNOON
            OUT1=EXTSLO+OUT1
        END DO
        OUT1=OUT1*CALINT*60./1000000.

    ELSE   ! Compute instantaneous values...(Is used in HRLDAS!)

        T1=FLOAT(IHR)+FLOAT(MM)/60.
        H=ONEHR*(T1-TIMNOON)
        COSZ=SLAT*SDEC+CLAT*CDEC*COS(H)

        ! Assuming HRLDAS forcing already accounts for season, time of day etc,
        ! subtract out the component of adjustment that would occur for
        ! a flat surface, this should leave only the sloped component remaining

        COSBETA=CDEC*((SLAT*COS(H))*(-1.*CAZ*SINC)-  &
            SIN(H)*(SAZ*SINC)+(CLAT*COS(H))*CINC)+ &
            SDEC*(CLAT*(CAZ*SINC)+SLAT*CINC)

        COSBETA_FLAT=CDEC*CLAT*COS(H)+SDEC*SLAT

        INCADJ = COSBETA+(1-COSBETA_FLAT)

        EXTRA=SC*RDVECSQ*COSZ
        IF(EXTRA.LE.0.) EXTRA=0.
        EXTSLO=SC*RDVECSQ*INCADJ
        !         IF(EXTRA.LE.0. .OR. EXTSLO.LT.0.) EXTSLO=0.  !remove check for HRLDAS.
        OUT1=EXTSLO
        Z=ACOS(COSZ)
        COSA=(SLAT*COSZ-SDEC)/(CLAT*SIN(Z))
        IF(COSA.LT.-1.) COSA=-1.
        IF(COSA.GT.1.) COSA=1.
        A=ABS(ACOS(COSA))
        IF(H.LT.0.) A=-A
        OUT2=Z/RTOD
        OUT3=A/RTOD+180

    END IF    ! End if for daily vs instantaneous values...

    !DJG-----------------------------------------------------------------------
    RETURN
END SUBROUTINE SOLSUB
!DJG-----------------------------------------------------------------------

subroutine seq_land_SO8(SO8LD_D,Vmax,TERR,dx,ix,jx)
    implicit none
    integer :: ix,jx,i,j
    REAL, DIMENSION(IX,JX,8)      :: SO8LD
    INTEGER, DIMENSION(IX,JX,3)   :: SO8LD_D
    real,DIMENSION(IX,JX)      :: TERR
    real                       :: dx(ix,jx,9),Vmax(ix,jx)
    SO8LD_D = -1
    do j = 2, jx -1
        do i = 2, ix -1
            SO8LD(i,j,1) = (TERR(i,j)-TERR(i,j+1))/dx(i,j,1)
            SO8LD_D(i,j,1) = i
            SO8LD_D(i,j,2) = j + 1
            SO8LD_D(i,j,3) = 1
            Vmax(i,j) = SO8LD(i,j,1)

            SO8LD(i,j,2) = (TERR(i,j)-TERR(i+1,j+1))/DX(i,j,2)
            if(SO8LD(i,j,2) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i + 1
                SO8LD_D(i,j,2) = j + 1
                SO8LD_D(i,j,3) = 2
                Vmax(i,j) = SO8LD(i,j,2)
            end if
            SO8LD(i,j,3) = (TERR(i,j)-TERR(i+1,j))/DX(i,j,3)
            if(SO8LD(i,j,3) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i + 1
                SO8LD_D(i,j,2) = j
                SO8LD_D(i,j,3) = 3
                Vmax(i,j) = SO8LD(i,j,3)
            end if
            SO8LD(i,j,4) = (TERR(i,j)-TERR(i+1,j-1))/DX(i,j,4)
            if(SO8LD(i,j,4) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i + 1
                SO8LD_D(i,j,2) = j - 1
                SO8LD_D(i,j,3) = 4
                Vmax(i,j) = SO8LD(i,j,4)
            end if
            SO8LD(i,j,5) = (TERR(i,j)-TERR(i,j-1))/DX(i,j,5)
            if(SO8LD(i,j,5) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i
                SO8LD_D(i,j,2) = j - 1
                SO8LD_D(i,j,3) = 5
                Vmax(i,j) = SO8LD(i,j,5)
            end if
            SO8LD(i,j,6) = (TERR(i,j)-TERR(i-1,j-1))/DX(i,j,6)
            if(SO8LD(i,j,6) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i - 1
                SO8LD_D(i,j,2) = j - 1
                SO8LD_D(i,j,3) = 6
                Vmax(i,j) = SO8LD(i,j,6)
            end if
            SO8LD(i,j,7) = (TERR(i,j)-TERR(i-1,j))/DX(i,j,7)
            if(SO8LD(i,j,7) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i - 1
                SO8LD_D(i,j,2) = j
                SO8LD_D(i,j,3) = 7
                Vmax(i,j) = SO8LD(i,j,7)
            end if
            SO8LD(i,j,8) = (TERR(i,j)-TERR(i-1,j+1))/DX(i,j,8)
            if(SO8LD(i,j,8) .gt. Vmax(i,j) ) then
                SO8LD_D(i,j,1) = i - 1
                SO8LD_D(i,j,2) = j + 1
                SO8LD_D(i,j,3) = 8
                Vmax(i,j) = SO8LD(i,j,8)
            end if
        enddo
    enddo
    Vmax = TANH(Vmax)
    return
end  subroutine seq_land_SO8

#ifdef MPP_LAND
subroutine MPP_seq_land_SO8(SO8LD_D,Vmax,TERRAIN,dx,ix,jx,&
        global_nx,global_ny)

    use module_mpp_land, only:  my_id, io_id, &
        write_io_real,decompose_data_int,decompose_data_real

    implicit none
    integer,intent(in) :: ix,jx,global_nx,global_ny
    INTEGER, intent(inout),DIMENSION(IX,JX,3)   :: SO8LD_D
    !         real,intent(in), DIMENSION(IX,JX)   :: TERRAIN
    real,DIMENSION(IX,JX)   :: TERRAIN
    real,intent(out),dimension(ix,jx) ::  Vmax
    real,intent(in)                     :: dx(ix,jx,9)
    real                     :: g_dx(ix,jx,9)

    real,DIMENSION(global_nx,global_ny)      :: g_TERRAIN
    real,DIMENSION(global_nx,global_ny)      :: g_Vmax
    integer,DIMENSION(global_nx,global_ny,3)      :: g_SO8LD_D
    integer :: k

    g_SO8LD_D = 0
    g_Vmax    = 0

    do k = 1, 9
        call write_IO_real(dx(:,:,k),g_dx(:,:,k))
    end do

    call write_IO_real(TERRAIN,g_TERRAIN)
    if(my_id .eq. IO_id) then
        call seq_land_SO8(g_SO8LD_D,g_Vmax,g_TERRAIN,g_dx,global_nx,global_ny)
    endif
    call decompose_data_int(g_SO8LD_D(:,:,3),SO8LD_D(:,:,3))
    call decompose_data_real(g_Vmax,Vmax)
    return
end subroutine MPP_seq_land_SO8

#endif


subroutine disaggregateDomain_drv(did)
   use module_RT_data, only: rt_domain
   use config_base, only: nlst, noah_lsm
   integer :: did
   call disaggregateDomain( RT_DOMAIN(did)%IX, RT_DOMAIN(did)%JX, nlst(did)%NSOIL, &
                            RT_DOMAIN(did)%IXRT, RT_DOMAIN(did)%JXRT, nlst(did)%AGGFACTRT, &
                            RT_DOMAIN(did)%SICE, RT_DOMAIN(did)%SMC, RT_DOMAIN(did)%SH2OX, &
                            RT_DOMAIN(did)%INFXSRT, rt_domain(did)%dist_lsm(:,:,9), &
                            RT_DOMAIN(did)%SMCMAX1, RT_DOMAIN(did)%SMCREF1, &
                            RT_DOMAIN(did)%SMCWLT1, RT_DOMAIN(did)%VEGTYP, RT_DOMAIN(did)%LKSAT, &
                            RT_DOMAIN(did)%NEXP, &
                            rt_domain(did)%overland%properties%distance_to_neighbor, &
                            RT_DOMAIN(did)%INFXSWGT, &
                            RT_DOMAIN(did)%LKSATFAC, &
                            rt_domain(did)%overland%streams_and_lakes%ch_netrt, &
                            RT_DOMAIN(did)%SH2OWGT, &
                            RT_DOMAIN(did)%subsurface%grid_transform%smcrefrt, &
                            rt_domain(did)%overland%control%infiltration_excess, &
                            RT_DOMAIN(did)%subsurface%grid_transform%smcmaxrt, &
                            RT_DOMAIN(did)%subsurface%grid_transform%smcwltrt, &
                            rt_domain(did)%subsurface%grid_transform%smcrt, &
                            rt_domain(did)%overland%streams_and_lakes%lake_mask, &
                            rt_domain(did)%subsurface%properties%lksatrt, &
                            rt_domain(did)%subsurface%properties%nexprt, &
                            RT_DOMAIN(did)%subsurface%properties%sldpth, &
                            RT_DOMAIN(did)%soiltypRT, RT_DOMAIN(did)%soiltyp, &
                            rt_domain(did)%ELRT, RT_DOMAIN(did)%iswater, &
                            rt_domain(did)%IMPERVFRAC, nlst(did)%imperv_adj)
end subroutine disaggregateDomain_drv

!===================================================================================================
! Subroutine Name:
!   subroutine disaggregateDomain
! Author(s)/Contact(s):
!   D. Gochis and W. Yu <gochis><ucar><edu>
! Abstract:
!   Disaggregates states and parameters from coarse grid to fine grid
! History Log:
! Usage:
! Parameters:
! Input Files:
! Output Files:
! Condition codes:
! User controllable options: None.
! Notes:
!===================================================================================================
subroutine disaggregateDomain(IX, JX, NSOIL, IXRT, JXRT, AGGFACTRT, &
                              SICE, SMC, SH2OX, INFXSRT, area_lsm, SMCMAX1, SMCREF1, &
                              SMCWLT1, VEGTYP, LKSAT, NEXP, dist, INFXSWGT, &
                              LKSATFAC, CH_NETRT, SH2OWGT, SMCREFRT, INFXSUBRT, SMCMAXRT, &
                              SMCWLTRT, SMCRT, LAKE_MSKRT, LKSATRT, NEXPRT,  &
                              SLDPTH, soiltypRT, soiltyp, elrt, iswater, impervfrac, imperv_adj)
#ifdef MPP_LAND
   use module_mpp_land, only: left_id,down_id,right_id, &
                              up_id,mpp_land_com_real, my_id, io_id, numprocs, &
                              mpp_land_sync,mpp_land_com_integer,mpp_land_max_int1, &
                              sum_real1
   use module_hydro_stop, only:HYDRO_stop
#endif

   implicit none

   ! Input Variables ------------------------------------------------------------------------
   ! General geometry/domain info:
   integer, intent(in)                           :: IX,JX ! coarse grid i,j dims
   integer, intent(in)                           :: IXRT,JXRT ! fine grid i,j dims
   integer, intent(in)                           :: AGGFACTRT ! aggregation factor between grids
   integer, intent(in)                           :: NSOIL ! number of soil layers
   integer, intent(in)                           :: iswater ! water veg class (from geogrid attrib)
   real, intent(in), dimension(NSOIL)            :: SLDPTH ! array soil layer depth intervals (m)
   integer, intent(in)                           :: imperv_adj ! impervious configuration option from hydro.namelist
   ! LSM grid parameters:
   real, intent(in),  dimension(IX,JX)           :: area_lsm ! cell area on the coarse grid (m2)
   integer, intent(in), dimension(IX,JX)         :: VEGTYP, soiltyp ! coarse grid veg and soil types
   real, intent(in),  dimension(IX,JX)           :: SMCMAX1 ! coarse grid porosity
   real, intent(in),  dimension(IX,JX)           :: SMCREF1 ! coarse grid field capacity
   real, intent(in),  dimension(IX,JX)           :: SMCWLT1 ! coarse grid wilting point
   real, intent(in),  dimension(IX,JX)           :: LKSAT ! coarse grid lateral ksat (m/s)
   real, intent(in),  dimension(IX,JX)           :: NEXP ! coarse grid n exponent
   ! LSM states:
   real, intent(in),  dimension(IX,JX,NSOIL)     :: SMC ! total soil moisture (m3/m3)
   real, intent(in),  dimension(IX,JX,NSOIL)     :: SH2OX ! liquid soil moisture (m3/m3)
   real, intent(in),  dimension(IX,JX)           :: INFXSRT
                                                    ! infiltration excess on coarse grid (mm)
   ! Routing grid parameters:
   real, intent(in), dimension(IXRT,JXRT,9)      :: dist
                                                    ! routing grid cell distances (m) and area (m2)
                                                    ! TODO: can we just pass in area since we don't need other distances?
   real, intent(in), dimension(IXRT,JXRT)        :: LKSATFAC ! lateral ksat adj factor
   real, intent(in), dimension(IXRT,JXRT)        :: elrt ! elevation grid (m)
   integer, intent(in), dimension(IXRT,JXRT)     :: CH_NETRT ! channel network routing grid
   real, intent(in), dimension(IXRT,JXRT)        :: impervfrac ! impervious fraction
   ! Routing states:
   real, intent(in), dimension(IXRT,JXRT)        :: INFXSWGT ! infiltration excess weighting grid
   real, intent(in), dimension(IXRT,JXRT,NSOIL)  :: SH2OWGT ! soil moisture weighting grid

   ! Update Variables ------------------------------------------------------------------------
   integer, intent(inout), dimension(IXRT,JXRT)  :: LAKE_MSKRT ! lake mask on the routing grid

   ! Output Variables ------------------------------------------------------------------------
   ! Parameters:
   integer, intent(out), dimension(IXRT,JXRT)    :: soiltypRT ! soil type on the routing grid
   real, intent(out), dimension(IXRT,JXRT,NSOIL) :: SMCMAXRT ! porosity on routing grid
   real, intent(out), dimension(IXRT,JXRT,NSOIL) :: SMCREFRT ! field capacity on routing grid
   real, intent(out), dimension(IXRT,JXRT,NSOIL) :: SMCWLTRT ! wilting point on routing grid
   real, intent(out), dimension(IXRT,JXRT)       :: LKSATRT ! lateral ksat on the routing grid (m/s)
   real, intent(out), dimension(IXRT,JXRT)       :: NEXPRT ! n exponent on the routing grid
   ! States:
   real, intent(out), dimension(IX,JX,NSOIL)     :: SICE ! soil ice content on coarse grid (m3/m3)
   real, intent(out), dimension(IXRT,JXRT,NSOIL) :: SMCRT
                                                    ! soil moisture contant on routing grid (m3/m3)
   real, intent(out), dimension(IXRT,JXRT)       :: INFXSUBRT
                                                    ! infiltration excess on routing grid (mm)

   ! Local Variables ------------------------------------------------------------------------
   integer                    :: i, j ! coarse grid loop indices
   integer                    :: AGGFACYRT, AGGFACXRT ! fine grid aggregation factors
   integer                    :: IXXRT, JYYRT ! fine grid i,j coordinates
   integer                    :: KRT, KF ! soil layer loop indixes
   real                       :: LSMVOL ! total infiltration excess volume per LSM cell (mm * m2)
   real                       :: SMCEXCS ! excess soil moisture (m3/m3)
   real                       :: WATHOLDCAP ! water holding capacity, smcmax - smcwlt (m3/m3)
   real                       :: smScaleFact ! soil moisture scaling factor for ksat (0-1)
   real, dimension(IXRT,JXRT) :: OCEAN_INFXSUBRT
                                 ! dummy variable to dump infiltration excess over ocean cells
#ifdef HYDRO_D
   ! For water budget calcs
   integer :: ii, jj, kk ! local loop indices
   real    :: smctot1,smcrttot2 ! total soil moisture, start and finish
   real    :: sicetot1 ! total ice, start and finish
   real    :: suminfxs1,suminfxsrt2 ! total infiltration excess, start and finish
#endif

   ! Initialize variables
   SICE = SMC - SH2OX
   SMCREFRT = 0.0
   ! ADCHANGE: Initialize ocean infxsubrt var to 0. Currently just a dump
   ! variable but could be used for future ocean model coupling
   OCEAN_INFXSUBRT = 0.0

   !DJG First, Disaggregate a few key fields for routing...
   !DJG Debug...
#ifdef HYDRO_D
   print *, "Beginning Disaggregation..."
#endif

   !DJG Mass balance check for disagg...
#ifdef HYDRO_D
   ! ADCHANGE: START Initial water balance variables
   ! ALL VARS in MM
   suminfxs1 = 0.
   smctot1 = 0.
   sicetot1 = 0.
   do ii=1,IX
      do jj=1,JX
         suminfxs1 = suminfxs1 + INFXSRT(ii,jj) / float(IX*JX)
         do kk=1,NSOIL
            smctot1 = smctot1 + SMC(ii,jj,kk)*SLDPTH(kk)*1000. / float(IX*JX)
            sicetot1 = sicetot1 + SICE(ii,jj,kk)*SLDPTH(kk)*1000. / float(IX*JX)
         end do
      end do
   end do
#ifdef MPP_LAND
   ! not tested
   CALL sum_real1(suminfxs1)
   CALL sum_real1(smctot1)
   CALL sum_real1(sicetot1)
   suminfxs1 = suminfxs1/float(numprocs)
   smctot1 = smctot1/float(numprocs)
   sicetot1 = sicetot1/float(numprocs)
#endif
   ! END Initial water balance variables
#endif

   !DJG Weighting alg. alteration...(prescribe wghts if time = 1)

   do J=1,JX ! Start coarse grid j loop
      do I=1,IX ! Start coarse grid i loop

         !DJG Weighting alg. alteration...
         LSMVOL = INFXSRT(I,J) * area_lsm(I,J) ! mm * m2

         do AGGFACYRT=AGGFACTRT-1,0,-1 ! Start disagg fine grid j loop
            do AGGFACXRT=AGGFACTRT-1,0,-1 ! Start disagg fine grid i loop

               IXXRT = I * AGGFACTRT - AGGFACXRT ! Define fine grid i
               JYYRT = J * AGGFACTRT - AGGFACYRT ! Define fine grid j
#ifdef MPP_LAND
               if(left_id.ge.0) IXXRT = IXXRT+1
               if(down_id.ge.0) JYYRT = JYYRT+1
#endif
               ! Initial redistribution of total coarse grid cell infiltration excess
               ! based on subgrid weight factor and current volume of water (mm * m2 / m2 = mm)
               INFXSUBRT(IXXRT,JYYRT) = LSMVOL *     &
                                        INFXSWGT(IXXRT,JYYRT) / dist(IXXRT,JYYRT,9)

               do KRT=1,NSOIL ! Soil layer loop

                  ! Adjustments for soil ice
                  IF (SICE(I,J,KRT) .gt. 0) then
                     !DJG Adjust SMCMAX for SICE when subsfc routing...make 3d variable
                     SMCMAXRT(IXXRT,JYYRT,KRT) = SMCMAX1(I,J) - SICE(I,J,KRT)
                     SMCREFRT(IXXRT,JYYRT,KRT) = SMCREF1(I,J) - SICE(I,J,KRT) !TODO: This can be negative! e.g., when almost saturated and fully frozen
                     WATHOLDCAP = SMCMAX1(I,J) - SMCWLT1(I,J)
                     IF (SICE(I,J,KRT) .le. WATHOLDCAP)    then
                        SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J)
                     else
                        if (SICE(I,J,KRT) .lt. SMCMAX1(I,J)) then
                           SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J) - &
                                                       (SICE(I,J,KRT) - WATHOLDCAP)
                        endif
                        if (SICE(I,J,KRT) .ge. SMCMAX1(I,J)) then
                           SMCWLTRT(IXXRT,JYYRT,KRT) = 0.
                        endif
                     endif
                  ELSE ! no ice
                     SMCMAXRT(IXXRT,JYYRT,KRT) = SMCMAX1(I,J)
                     SMCREFRT(IXXRT,JYYRT,KRT) = SMCREF1(I,J)
                     WATHOLDCAP = SMCMAX1(I,J) - SMCWLT1(I,J) !TODO: Not used again so can delete
                     SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J)
                  ENDIF   ! endif adjust for soil ice

                  !Now Adjust soil moisture
                  !DJG Use SH2O instead of SMC for 'liquid' water...
                  IF (SMCMAXRT(IXXRT,JYYRT,KRT) .GT. 0) THEN !Check for smcmax data (=0 over water)
                     SMCRT(IXXRT,JYYRT,KRT) = SH2OX(I,J,KRT) * SH2OWGT(IXXRT,JYYRT,KRT)
                     !old SMCRT(IXXRT,JYYRT,KRT)=SMC(I,J,KRT)
                  ELSE
                     !TODO: Double-check the values for water cells
                     SMCRT(IXXRT,JYYRT,KRT) = 0.001  !will be skipped w/ landmask
                     SMCMAXRT(IXXRT,JYYRT,KRT) = 0.001
                  END IF

                  !DJG Check/Adjust so that subgrid cells do not exceed saturation...
                  IF (SMCRT(IXXRT,JYYRT,KRT) .GT. SMCMAXRT(IXXRT,JYYRT,KRT)) THEN
                     SMCEXCS = (SMCRT(IXXRT,JYYRT,KRT) - SMCMAXRT(IXXRT,JYYRT,KRT)) &
                               * SLDPTH(KRT) * 1000.  !Excess soil water in units of (mm)
                     SMCRT(IXXRT,JYYRT,KRT) = SMCMAXRT(IXXRT,JYYRT,KRT)
                     DO KF = KRT-1,1,-1  !loop back upward to redistribute excess water from disagg.
                        SMCRT(IXXRT,JYYRT,KF) = SMCRT(IXXRT,JYYRT,KF) + SMCEXCS/(SLDPTH(KF)*1000.)
                        !Recheck new lyr sat.
                        IF (SMCRT(IXXRT,JYYRT,KF) .GT. SMCMAXRT(IXXRT,JYYRT,KF)) THEN
                           SMCEXCS = (SMCRT(IXXRT,JYYRT,KF) - SMCMAXRT(IXXRT,JYYRT,KF)) &
                                     * SLDPTH(KF) * 1000.  !Excess soil water in units of (mm)
                           SMCRT(IXXRT,JYYRT,KF) = SMCMAXRT(IXXRT,JYYRT,KF)
                        ELSE  ! Excess soil water expired
                           SMCEXCS = 0.
                           EXIT
                        END IF
                     END DO
                     IF (SMCEXCS .GT. 0) THEN  !If not expired by sfc then add to Infil. Excess
                        INFXSUBRT(IXXRT,JYYRT) = INFXSUBRT(IXXRT,JYYRT) + SMCEXCS
                        SMCEXCS = 0.
                     END IF
                  END IF  !End if for soil moisture saturation excess

               end do !End do for soil profile loop

               ! Debug loop
               do KRT=1,NSOIL
                  IF (SMCRT(IXXRT,JYYRT,KRT) .GT. SMCMAXRT(IXXRT,JYYRT,KRT)) THEN
                     print *, "FATAL ERROR: SMCMAX exceeded upon disaggregation3...", &
                              ixxrt, jyyrt, krt, &
                              SMCRT(IXXRT,JYYRT,KRT),SMCMAXRT(IXXRT,JYYRT,KRT)
                     call hydro_stop("In disaggregateDomain() - SMCMAX exceeded upon disaggregation3")
                  ELSE IF (SMCRT(IXXRT,JYYRT,KRT).LE.0.) THEN
                     print *, "SMCRT fully depleted upon disaggregation...", &
                         ixxrt,jyyrt,krt,&
                         "SMCRT=",SMCRT(IXXRT,JYYRT,KRT),"SH2OWGT=",SH2OWGT(IXXRT,JYYRT,KRT),&
                         "SH2O=",SH2OX(I,J,KRT)
                     print*, "SMC=", SMC(i,j,KRT), "SICE =", sice(i,j,KRT)
                     print *, "VEGTYP = ", VEGTYP(I,J)
                     print *, "i,j,krt, nsoil",i,j,krt,nsoil
                     ! ADCHANGE: If values are close but not exact, end up with a crash.
                     ! Force values to match.
                     !IF (SMC(i,j,KRT).EQ.sice(i,j,KRT)) THEN
                     IF (ABS(SMC(i,j,KRT) - sice(i,j,KRT)) .LE. 0.00001) THEN
                        print *, "SMC = SICE, soil layer totally frozen, proceeding..."
                        SMCRT(IXXRT,JYYRT,KRT) = 0.001
                        sice(i,j,KRT) = SMC(i,j,KRT)
                     ELSE
                        call hydro_stop("In disaggregateDomain() - SMCRT depleted")
                     END IF
                  END IF
               end do !debug loop

               ! Now do simple grid remapping tasks

               ! Lateral ksat
               !DJG 6.12.08 Map lateral hydraulic conductivity and apply distributed scaling
               ! ---        factor that will be read in from hires terrain file
               !              LKSATRT(IXXRT,JYYRT) = LKSAT(I,J) * LKSATFAC(IXXRT,JYYRT) * &  !Apply scaling factor...
               ! ...and scale from max to 0 when SMC decreases from SMCMAX to SMCREF...
               !!DJG error found from KIT,improper scaling       ((SMCMAXRT(IXXRT,JYYRT,NSOIL) - SMCRT(IXXRT,JYYRT,NSOIL)) / &
               !                                    (max(0.,(SMCMAXRT(IXXRT,JYYRT,NSOIL) - SMCRT(IXXRT,JYYRT,NSOIL))) / &
               !                                    (SMCMAXRT(IXXRT,JYYRT,NSOIL)-SMCREFRT(IXXRT,JYYRT,NSOIL)) )
               ! AD_CHANGE:
               ! Corrected to scale from 0 at SMCREF to full LKSAT*LKSATFAC at SMCMAX.
               ! This is a linear simplification of the true k to smc relationship to cover the
               ! range of conditions between smcmax and smcref. The DHSVM lateral routing
               ! scheme assumes saturated conditions (and therfore ksat), but we are extending
               ! that slightly to route water up until smcref.
               smScaleFact = (SMCRT(IXXRT,JYYRT,NSOIL) - SMCREFRT(IXXRT,JYYRT,NSOIL)) / &
                                (SMCMAXRT(IXXRT,JYYRT,NSOIL) - SMCREFRT(IXXRT,JYYRT, NSOIL))
               smScaleFact = max(0., smScaleFact) !becomes 0 if less than SMCREF
               smScaleFact = min(1., smScaleFact) !make sure scale factor doesn't go over 1
               LKSATRT(IXXRT,JYYRT) = LKSAT(I,J) * LKSATFAC(IXXRT,JYYRT) * smScaleFact

               ! n exponent for subsurface routing
               nexprt(ixxrt,jyyrt) = nexp(i,j)

               ! Lake mask
               !DJG set up lake mask...
               !--- modify to make lake mask large here, but not one of the routed lakes!!!
               !--            IF (VEGTYP(I,J).eq.16) then
               IF (VEGTYP(I,J) .eq. iswater .and. CH_NETRT(IXXRT,JYYRT).le.0) then
                  !--LAKE_MSKRT(IXXRT,JYYRT) = 1
                  !yw  LAKE_MSKRT(IXXRT,JYYRT) = 9999
                  LAKE_MSKRT(IXXRT,JYYRT) = -9999
               end if

               ! Soil type
               ! BF disaggregate soiltype information for gw-soil-coupling
               ! TODO: move this disaggregation code line to lsm_init section because soiltype is time-invariant
               soiltypRT(ixxrt,jyyrt) = soiltyp(i,j)

            end do ! end disagg fine grid i loop
         end do ! end disagg fine grid j loop

      end do ! end coarse grid i loop
   end do ! end coarse fine grid j loop

   ! AD: Add new zeroing out of -9999 elevation cells which are ocean
   where (ELRT .lt. -9998)
      OCEAN_INFXSUBRT = INFXSUBRT
      INFXSUBRT = 0.0
   endwhere

#ifdef HYDRO_D
   ! ADCHANGE: START Final water balance variables
   ! ALL VARS in MM
   suminfxsrt2 = 0.
   smcrttot2 = 0.
   do ii=1,IXRT
      do jj=1,JXRT
         suminfxsrt2 = suminfxsrt2 + INFXSUBRT(ii,jj) / float(IXRT*JXRT)
         do kk=1,NSOIL
            smcrttot2 = smcrttot2 + SMCRT(ii,jj,kk)*SLDPTH(kk)*1000. / float(IXRT*JXRT)
         end do
      end do
   end do
#ifdef MPP_LAND
   ! not tested
   CALL sum_real1(suminfxsrt2)
   CALL sum_real1(smcrttot2)
   suminfxsrt2 = suminfxsrt2/float(numprocs)
   smcrttot2 = smcrttot2/float(numprocs)
#endif
#ifdef MPP_LAND
   if (my_id .eq. IO_id) then
#endif
      print *, "Disagg Mass Bal: "
      print *, "WB_DISAG!InfxsDiff", suminfxsrt2-suminfxs1
      print *, "WB_DISAG!Infxs1", suminfxs1
      print *, "WB_DISAG!Infxs2", suminfxsrt2
      print *, "WB_DISAG!SMCDIff", smcrttot2-(smctot1-sicetot1)
      print *, "WB_DISAG!SMC1", smctot1
      print *, "WB_DISAG!SICE1", sicetot1
      print *, "WB_DISAG!SMC2", smcrttot2
      print *, "WB_DISAG!Residual", (suminfxsrt2-suminfxs1) + (smcrttot2-(smctot1-sicetot1))
#ifdef MPP_LAND
   endif
#endif
   ! END Final water balance variables
#endif

#ifdef HYDRO_D
   print *, "After Disaggregation..."
#endif
#ifdef MPP_LAND
   call MPP_LAND_COM_REAL(INFXSUBRT,IXRT,JXRT,99)
   call MPP_LAND_COM_REAL(LKSATRT,IXRT,JXRT,99)
   call MPP_LAND_COM_REAL(NEXPRT,IXRT,JXRT,99)
   call MPP_LAND_COM_INTEGER(LAKE_MSKRT,IXRT,JXRT,99)
   do i = 1, NSOIL
      call MPP_LAND_COM_REAL(SMCMAXRT(:,:,i),IXRT,JXRT,99)
      call MPP_LAND_COM_REAL(SMCRT(:,:,i),IXRT,JXRT,99)
      call MPP_LAND_COM_REAL(SMCWLTRT(:,:,i),IXRT,JXRT,99)
   end DO
#endif

end subroutine disaggregateDomain
!===================================================================================================


subroutine SubsurfaceRouting_drv(did)

    use module_RT_data, only: rt_domain
    use config_base, only: nlst

    implicit none
    integer :: did
    IF (nlst(did)%SUBRTSWCRT.EQ.1) THEN
        call subsurfaceRouting ( rt_domain(did)%subsurface, &
            rt_domain(did)%subsurface_static, &
            rt_domain(did)%subsurface_input, &
            rt_domain(did)%subsurface_output, &
            rt_domain(did)%overland%control%infiltration_excess)
    endif

end subroutine SubsurfaceRouting_drv



subroutine OverlandRouting_drv(did)
    use module_RT_data, only: rt_domain
    use config_base, only: nlst

    implicit none
    integer :: did
    if(nlst(did)%OVRTSWCRT .eq. 1) then
        !call OverlandRouting (nlst_rt(did)%DT, nlst_rt(did)%DTRT_TER, nlst_rt(did)%rt_option, &
            !         rt_domain(did)%ixrt, rt_domain(did)%jxrt,rt_domain(did)%overland%streams_and_lakes%lake_mask, &
            !         rt_domain(did)%overland%control%infiltration_excess, rt_domain(did)%overland%properties%retention_depth,rt_domain(did)%overland%properties%roughness, &
            !         rt_domain(did)%overland%properties%surface_slope_x, rt_domain(did)%overland%properties%surface_slope_y, rt_domain(did)%overland%control%surface_water_head_routing,  &
            !         rt_domain(did)%overland%control%dhrt, rt_domain(did)%overland%streams_and_lakes%ch_netrt, rt_domain(did)%overland%streams_and_lakes%surface_water_to_channel,&
            !         rt_domain(did)%overland%streams_and_lakes%surface_water_to_lake,rt_domain(did)%overland%control%boundary_flux, &
            !         rt_domain(did)%overland%streams_and_lakes%accumulated_surface_water_to_channel,rt_domain(did)%overland%control%boundary_flux_total, rt_domain(did)%overland%streams_and_lakes%accumulated_surface_water_to_lake,&
            !         rt_domain(did)%q_sfcflx_x,rt_domain(did)%q_sfcflx_y, &
            !         rt_domain(did)%overland%properties%distance_to_neighbor, rt_domain(did)%overland%properties%surface_slope, rt_domain(did)%overland%properties%max_surface_slope_index , &
            !         rt_domain(did)%overland%mass_balance%post_soil_moisture_content,rt_domain(did)%overland%mass_balance%pre_infiltration_excess,rt_domain(did)%overland%mass_balance%post_infiltration_excess, &
            !         rt_domain(did)%overland%mass_balance%pre_soil_moisture_content,rt_domain(did)%overland%mass_balance%accumulated_change_in_soil_moisture )
        call OverlandRouting( &
            rt_domain(did)%overland, &
            nlst(did)%DT, &
            nlst(did)%DTRT_TER, &
            nlst(did)%rt_option, &
            rt_domain(did)%ixrt, &
            rt_domain(did)%jxrt, &
            rt_domain(did)%q_sfcflx_x, &
            rt_domain(did)%q_sfcflx_y &
            )
        ! ADCHANGE: If overland routing is called, INFXSUBRT is moved to SFCHEADSUBRT, so
        !           zeroing out just in case
        rt_domain(did)%overland%control%infiltration_excess = 0.0
    endif
end subroutine OverlandRouting_drv






subroutine time_seconds(i3)
    integer time_array(8)
    real*8 i3
    call date_and_time(values=time_array)
    i3 = time_array(4)*24*3600+time_array(5) * 3600 + time_array(6) * 60 + &
        time_array(7) + 0.001 * time_array(8)
    return
end subroutine time_seconds

