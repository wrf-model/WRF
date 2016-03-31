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





!DJG ------------------------------------------------
!DJG   SUBROUTINE SUBSFC_RTNG
!DJG ------------------------------------------------

	SUBROUTINE SUBSFC_RTNG(dist,ZWATTABLRT,QSUBRT,SOXRT,    &
          SOYRT,LATKSATRT,SOLDEPRT,QSUBBDRYRT,QSUBBDRYTRT,      &    
          NSOIL,SMCRT,INFXSUBRT,SMCMAXRT,SMCREFRT,ZSOIL,IXRT,JXRT,DT,    &
          SMCWLTRT,SO8RT,SO8RT_D, rt_option,SLDPTH,junk4,CWATAVAIL, &
          SATLYRCHK)

!       use module_mpp_land, only: write_restart_rt_3, write_restart_rt_2, &
!            my_id
#ifdef MPP_LAND
        use module_mpp_land, only: MPP_LAND_COM_REAL, sum_real1, &
		my_id, io_id, numprocs
#endif
	IMPLICIT NONE

!DJG -------- DECLARATIONS ------------------------

	INTEGER, INTENT(IN) :: IXRT,JXRT,NSOIL

	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: SOXRT,junk4
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: SOYRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: LATKSATRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)    :: SOLDEPRT

	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)   :: ZWATTABLRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)   :: CWATAVAIL
        INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: SATLYRCHK


	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)   :: QSUBRT
	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)   :: QSUBBDRYRT

	REAL, INTENT(IN)                          :: dist(ixrt,jxrt,9)
	REAL, INTENT(IN)                          :: DT
	REAL, INTENT(IN), DIMENSION(NSOIL)        :: ZSOIL
	REAL, INTENT(IN), DIMENSION(NSOIL) 	  :: SLDPTH
	REAL, INTENT(INOUT)                       :: QSUBBDRYTRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: INFXSUBRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT,NSOIL) :: SMCMAXRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT,NSOIL) :: SMCREFRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT,NSOIL) :: SMCRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT,NSOIL) :: SMCWLTRT

	REAL, DIMENSION(IXRT,JXRT)	:: ywtmp
!DJG Local Variables

	INTEGER	:: I,J,KK
!djg        INTEGER, DIMENSION(IXRT,JXRT) :: SATLYRCHK

	REAL 	:: GRDAREA
	REAL	:: SUBFLO
	REAL	:: WATAVAIL

        INTEGER :: SO8RT_D(IXRT,JXRT,3)
        REAL :: SO8RT(IXRT,JXRT,8)
        integer ::  rt_option, index

        INTEGER :: DT_STEPS             !-- number of timestep in routing
        REAL :: SUBDT                !-- subsurface routing timestep
        INTEGER :: KRT                  !-- routing counter
        REAL, DIMENSION(IXRT,JXRT,NSOIL) :: SMCTMP  !--temp store of SMC
        REAL, DIMENSION(IXRT,JXRT) :: ZWATTABLRTTMP ! temp store of ZWAT
        REAL, DIMENSION(IXRT,JXRT) :: INFXSUBRTTMP ! temp store of infilx
!djg        REAL, DIMENSION(IXRT,JXRT) :: CWATAVAIL ! temp specif. of wat avial
        
        

!DJG Debug Variables...
        REAL :: qsubchk,qsubbdrytmp
        REAL :: junk1,junk2,junk3,junk5,junk6,junk7
        INTEGER, PARAMETER :: double=8
        REAL (KIND=double) :: smctot1a,smctot2a
	INTEGER :: kx,count

#ifdef HYDRO_D
! ADCHANGE: Water balance variables
       real   :: smctot1,smctot2
       real   :: suminfxsrt1,suminfxsrt2
       real   :: qbdry1,qbdry2
       real   :: sumqsubrt1, sumqsubrt2
#endif
        
!DJG -----------------------------------------------------------------
!DJG  SUBSURFACE ROUTING LOOP
!DJG    - SUBSURFACE ROUTING RUN ON NOAH TIMESTEP
!DJG    - SUBSURFACE ROUITNG ONLY PERFORMED ON SATURATED LAYERS
!DJG -----------------------------------------------------------------

#ifdef HYDRO_D
! ADCHANGE: START Initial water balance variables 
! ALL VARS in MM
       suminfxsrt1 = 0.
       qbdry1 = 0.
       smctot1 = 0.
       sumqsubrt1 = 0.
       do i=1,IXRT
         do j=1,JXRT
           suminfxsrt1 = suminfxsrt1 + INFXSUBRT(I,J) / float(IXRT*JXRT)
           qbdry1 = qbdry1 + QSUBBDRYRT(I,J)/dist(i,j,9)*SUBDT*1000. / float(IXRT*JXRT)
           sumqsubrt1 = sumqsubrt1 + QSUBRT(I,J)/dist(i,j,9)*SUBDT*1000. / float(IXRT*JXRT)
           do kk=1,NSOIL
               smctot1 = smctot1 + SMCRT(I,J,KK)*SLDPTH(KK)*1000. / float(IXRT*JXRT)
           end do
         end do
       end do

#ifdef MPP_LAND
! not tested
       CALL sum_real1(suminfxsrt1)
       CALL sum_real1(qbdry1)
       CALL sum_real1(sumqsubrt1)
       CALL sum_real1(smctot1)
       suminfxsrt1 = suminfxsrt1/float(numprocs)
       qbdry1 = qbdry1/float(numprocs)
       sumqsubrt1 = sumqsubrt1/float(numprocs)
       smctot1 = smctot1/float(numprocs)
#endif
! END Initial water balance variables
#endif


        !yw GRDAREA=DXRT*DXRT
        ! GRDAREA=dist(i,j,9)


!DJG debug subsfc...
         subflo = 0.0

!DJG Set up mass balance checks...
!         CWATAVAIL = 0.            !-- initialize subsurface watavail
         SUBDT = DT                !-- initialize the routing timestep to DT


!!!! Find saturated layer depth...
! Loop through domain to determine sat. layers and assign wat tbl depth...
!    and water available for subsfc routing (CWATAVAIL)...
!
!         CALL FINDZWAT(IXRT,JXRT,NSOIL,SMCRT,SMCMAXRT,SMCREFRT, &
!                             SMCWLTRT,ZSOIL,SATLYRCHK,ZWATTABLRT, &
!                             CWATAVAIL,SLDPTH)
         



!DJG debug variable...

!DJG Courant check temp variable setup...
         ZWATTABLRTTMP = ZWATTABLRT !-- temporary storage of water table level




!!!! Call subsurface routing subroutine...
#ifdef HYDRO_D
     print *, "calling subsurface routing subroutine...Opt. ",rt_option
#endif


     if(rt_option .eq. 1) then
        CALL ROUTE_SUBSURFACE1(dist,ZWATTABLRT,QSUBRT,SOXRT,SOYRT,  &   
               LATKSATRT,SOLDEPRT,IXRT,JXRT,QSUBBDRYRT,QSUBBDRYTRT, &   
               SO8RT,SO8RT_D,CWATAVAIL,SUBDT)
     else 
        CALL ROUTE_SUBSURFACE2(dist,ZWATTABLRT,QSUBRT,SOXRT,SOYRT,      &
               LATKSATRT,SOLDEPRT,IXRT,JXRT,QSUBBDRYRT,QSUBBDRYTRT,     &
               CWATAVAIL,SUBDT)
     end if

#ifdef HYDRO_D
     write(6,*) "finish calling ROUTE_SUBSURFACE ", rt_option
#endif


!!!! Update soil moisture fields with subsurface flow...

!!!! Loop through subsurface routing domain...
	DO I=1,IXRT
          DO J=1,JXRT

!!DJG Check for courant condition violation...put limit on qsub
!!DJG QSUB HAS units of m^3/s SUBFLO has units of m
          
! ADCHANGE: Moved this constraint to the ROUTE_SUBSURFACE routines
           !IF (CWATAVAIL(i,j).le.ABS(qsubrt(i,j))/dist(i,j,9)*SUBDT) THEN
           !  QSUBRT(i,j) = -1.0*CWATAVAIL(i,j)
           !  SUBFLO = QSUBRT(i,j)  !Units of qsubrt converted via CWATAVAIL
           !ELSE
             SUBFLO=QSUBRT(I,J)/dist(i,j,9)*SUBDT !Convert qsubrt from m^3/s to m
           !END IF

           WATAVAIL=0.  !Initialize to 0. for every cell...


!!DJG Begin loop through soil profile to adjust soil water content
!!DJG based on subsfc flow (SUBFLO)...

            IF (SUBFLO.GT.0) THEN ! Increase soil moist for +SUBFLO (Inflow)

! Loop through soil layers from bottom to top
              DO KK=NSOIL,1,-1


! Check for saturated layers
                IF (SMCRT(I,J,KK).GE.SMCMAXRT(I,J,KK)) THEN
                  IF (SMCRT(I,J,KK).GT.SMCMAXRT(I,J,KK)) THEN
                   print *, "FATAL ERROR: Subsfc acct. SMCMAX exceeded...", &
                       SMCRT(I,J,KK), SMCMAXRT(I,J,KK),KK,i,j
                   call hydro_stop("In SUBSFC_RTNG() - SMCMAX exceeded")
                  ELSE
                  END IF
                ELSE
                  WATAVAIL = (SMCMAXRT(I,J,KK)-SMCRT(I,J,KK))*SLDPTH(KK)
                  IF (WATAVAIL.GE.SUBFLO) THEN
                    SMCRT(I,J,KK) = SMCRT(I,J,KK) + SUBFLO/SLDPTH(KK)
                    SUBFLO = 0.
                  ELSE
                    SUBFLO = SUBFLO - WATAVAIL
                    SMCRT(I,J,KK) = SMCMAXRT(I,J,KK)
                  END IF
                END IF

                 IF (SUBFLO.EQ.0.) EXIT
!                IF (SUBFLO.EQ.0.) goto 669

              END DO      ! END DO FOR SOIL LAYERS

669           continue

! If all layers sat. add remaining subflo to infilt. excess...                  
              IF (KK.eq.0.AND.SUBFLO.gt.0.) then
                 INFXSUBRT(I,J) = INFXSUBRT(I,J) + SUBFLO*1000.    !Units = mm
                 SUBFLO=0.
              END IF

!DJG Error trap...
	       if (subflo.ne.0.) then
#ifdef HYDRO_D
                  print *, "Subflo (+) not expired...:",subflo,i,j,kk,SMCRT(i,j,1), &
                           SMCRT(i,j,2),SMCRT(i,j,3),SMCRT(i,j,4),SMCRT(i,j,5),  &
                           SMCRT(i,j,6),SMCRT(i,j,7),SMCRT(i,j,8),"SMCMAX",SMCMAXRT(i,j,1)
#endif
               end if

 
            ELSE IF (SUBFLO.LT.0) THEN    ! Decrease soil moist for -SUBFLO (Drainage)


!DJG loop from satlyr back down and subtract out subflo as necess...
!    now set to SMCREF, 8/24/07
!DJG and then using unsat cond as opposed to Ksat...

	      DO KK=SATLYRCHK(I,J),NSOIL
                 WATAVAIL = (SMCRT(I,J,KK)-SMCREFRT(I,J,KK))*SLDPTH(KK)
                 IF (WATAVAIL.GE.ABS(SUBFLO)) THEN
!?yw mod                 IF (WATAVAIL.GE.(ABS(SUBFLO)+0.000001) ) THEN
                   SMCRT(I,J,KK) = SMCRT(I,J,KK) + SUBFLO/SLDPTH(KK)
                   SUBFLO=0.
                 ELSE     ! Since subflo is small on a time-step following is unlikely...
                   SMCRT(I,J,KK)=SMCREFRT(I,J,KK)
                   SUBFLO=SUBFLO+WATAVAIL
                 END IF
                 IF (SUBFLO.EQ.0.) EXIT
!                IF (SUBFLO.EQ.0.) goto 668

              END DO  ! END DO FOR SOIL LAYERS
668        continue


!DJG Error trap...
              if(abs(subflo) .le. 1.E-7 )  subflo = 0.0  !truncate residual to 1E-7 prec.

	       if (subflo.ne.0.) then
#ifdef HYDRO_D
                  print *, "Subflo (-) not expired:",i,j,subflo,CWATAVAIL(i,j)
                  print *, "zwatabl = ", ZWATTABLRT(I,J)
                  print *, "QSUBRT(I,J)=",QSUBRT(I,J)
                  print *, "WATAVAIL = ",WATAVAIL, "kk=",kk
                  print *
#endif
               end if



            END IF  ! end if for +/- SUBFLO soil moisture accounting...




          END DO        ! END DO X dim
        END DO          ! END DO Y dim
!!!! End loop through subsurface routing domain...

#ifdef MPP_LAND
     do i = 1, NSOIL
        call MPP_LAND_COM_REAL(SMCRT(:,:,i),IXRT,JXRT,99)
     end DO
#endif

#ifdef HYDRO_D
! ADCHANGE: START Final water balance variables
! ALL VARS in MM
        suminfxsrt2 = 0.
        qbdry2 = 0.
        smctot2 = 0.
        sumqsubrt2 = 0.
        do i=1,IXRT
         do j=1,JXRT
            suminfxsrt2 = suminfxsrt2 + INFXSUBRT(I,J) / float(IXRT*JXRT)
            qbdry2 = qbdry2 + QSUBBDRYRT(I,J)/dist(i,j,9)*SUBDT*1000. / float(IXRT*JXRT)
            sumqsubrt2 = sumqsubrt2 + QSUBRT(I,J)/dist(i,j,9)*SUBDT*1000. / float(IXRT*JXRT)
            do kk=1,NSOIL
                smctot2 = smctot2 + SMCRT(I,J,KK)*SLDPTH(KK)*1000. / float(IXRT*JXRT)
            end do
         end do
        end do

#ifdef MPP_LAND
! not tested
        CALL sum_real1(suminfxsrt2)
        CALL sum_real1(qbdry2)
        CALL sum_real1(sumqsubrt2)
        CALL sum_real1(smctot2)
        suminfxsrt2 = suminfxsrt2/float(numprocs)
        qbdry2 = qbdry2/float(numprocs)
        sumqsubrt2 = sumqsubrt2/float(numprocs)
        smctot2 = smctot2/float(numprocs)
#endif

#ifdef MPP_LAND   
       if (my_id .eq. IO_id) then
#endif
       print *, "SUBSFC Routing Mass Bal: "
       print *, "WB_SUB!QsubDiff", sumqsubrt2-sumqsubrt1
       print *, "WB_SUB!Qsub1", sumqsubrt1
       print *, "WB_SUB!Qsub2", sumqsubrt2
       print *, "WB_SUB!InfxsDiff", suminfxsrt2-suminfxsrt1
       print *, "WB_SUB!Infxs1", suminfxsrt1
       print *, "WB_SUB!Infxs2", suminfxsrt2
       print *, "WB_SUB!QbdryDiff", qbdry2-qbdry1
       print *, "WB_SUB!Qbdry1", qbdry1
       print *, "WB_SUB!Qbdry2", qbdry2
       print *, "WB_SUB!SMCDiff", smctot2-smctot1
       print *, "WB_SUB!SMC1", smctot1
       print *, "WB_SUB!SMC2", smctot2
       print *, "WB_SUB!Residual", sumqsubrt1 - ( (suminfxsrt2-suminfxsrt1) &
                       + (smctot2-smctot1) )
#ifdef MPP_LAND
       endif
#endif
! END Final water balance variables
#endif


!DJG ----------------------------------------------------------------
  END SUBROUTINE SUBSFC_RTNG 
!DJG ----------------------------------------------------------------


!DJG ------------------------------------------------------------------------
!DJG  SUBSURFACE FINDZWAT
!DJG ------------------------------------------------------------------------
         SUBROUTINE FINDZWAT(IXRT,JXRT,NSOIL,SMCRT,SMCMAXRT,SMCREFRT, &
                             SMCWLTRT,ZSOIL,SATLYRCHK,ZWATTABLRT,CWATAVAIL,&
                             SLDPTH)

	IMPLICIT NONE

!DJG -------- DECLARATIONS ------------------------

	INTEGER, INTENT(IN) :: IXRT,JXRT,NSOIL
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT,NSOIL) :: SMCMAXRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT,NSOIL) :: SMCREFRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT,NSOIL) :: SMCRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT,NSOIL) :: SMCWLTRT
	REAL, INTENT(IN), DIMENSION(NSOIL)        :: ZSOIL
	REAL, INTENT(IN), DIMENSION(NSOIL)        :: SLDPTH
	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)   :: ZWATTABLRT
	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)   :: CWATAVAIL
        INTEGER, INTENT(OUT), DIMENSION(IXRT,JXRT) :: SATLYRCHK
       
!DJG Local Variables
        INTEGER :: KK,i,j


!!!! Find saturated layer depth...
! Loop through domain to determine sat. layers and assign wat tbl depth...


        SATLYRCHK = 0  !set flag for sat. layers
        CWATAVAIL = 0.  !set wat avail for subsfc rtng = 0.

        DO J=1,JXRT
          DO I=1,IXRT

! Loop through soil layers from bottom to top
              DO KK=NSOIL,1,-1

! Check for saturated layers
! Add additional logical check to ensure water is 'available' for routing,
!  (i.e. not 'frozen' or otherwise immobile)
!                IF (SMCRT(I,J,KK).GE.SMCMAXRT(I,J,KK).AND.SMCMAXRT(I,J,KK) &
!                  .GT.SMCWLTRT(I,J,KK)) THEN
                IF ( (SMCRT(I,J,KK).GE.SMCREFRT(I,J,KK)).AND.(SMCREFRT(I,J,KK) &
                  .GT.SMCWLTRT(I,J,KK)) ) THEN
! Add additional check to ensure saturation from bottom up only...8/8/05
                  IF((SATLYRCHK(I,J).EQ.KK+1) .OR. (KK.EQ.NSOIL) ) SATLYRCHK(I,J) = KK
                END IF

              END DO


! Designate ZWATTABLRT based on highest sat. layer and
! Define amount of water avail for subsfc routing on each gridcell (CWATAVAIL)
!  note: using a 'field capacity' value of SMCREF as lower limit...

              IF (SATLYRCHK(I,J).ne.0) then
                IF (SATLYRCHK(I,J).ne.1) then  ! soil column is partially sat.
                  ZWATTABLRT(I,J) = -ZSOIL(SATLYRCHK(I,J)-1)
!DJG 2/16/2016 fix                  DO KK=SATLYRCHK(I,J),NSOIL
!old                   CWATAVAIL(I,J) = (SMCRT(I,J,SATLYRCHK(I,J))-&
!old                                    SMCREFRT(I,J,SATLYRCHK(I,J))) * &
!old                                    (ZSOIL(SATLYRCHK(I,J)-1)-ZSOIL(NSOIL))
!DJG 2/16/2016 fix                    CWATAVAIL(I,J) = CWATAVAIL(I,J)+(SMCRT(I,J,KK)- &
!DJG 2/16/2016 fix                                     SMCREFRT(I,J,KK))*SLDPTH(KK)
!DJG 2/16/2016 fix                  END DO


                ELSE  ! soil column is fully saturated to sfc.
                  ZWATTABLRT(I,J) = 0.
!DJG 2/16/2016 fix                  DO KK=SATLYRCHK(I,J),NSOIL
!DJG 2/16/2016 fix                    CWATAVAIL(I,J) = (SMCRT(I,J,KK)-SMCREFRT(I,J,KK))*SLDPTH(KK)
!DJG 2/16/2016 fix                  END DO
                END IF
!DJG 2/16/2016 fix accumulation of CWATAVAIL...
                  DO KK=SATLYRCHK(I,J),NSOIL
                    CWATAVAIL(I,J) = CWATAVAIL(I,J)+(SMCRT(I,J,KK)- &
                                     SMCREFRT(I,J,KK))*SLDPTH(KK)
                  END DO
              ELSE  ! no saturated layers...
                ZWATTABLRT(I,J) = -ZSOIL(NSOIL)
                SATLYRCHK(I,J) = NSOIL + 1
              END IF


	   END DO
         END DO


!DJG ----------------------------------------------------------------
  END SUBROUTINE FINDZWAT 
!DJG ----------------------------------------------------------------


!DJG ----------------------------------------------------------------
!DJG ----------------------------------------------------------------
!DJG     SUBROUTINE ROUTE_SUBSURFACE2
!DJG ----------------------------------------------------------------

          SUBROUTINE ROUTE_SUBSURFACE2(                                 &
                dist,z,qsub,sox,soy,                                   &
                latksat,soldep,XX,YY,QSUBDRY,QSUBDRYT,CWATAVAIL,   &
                SUBDT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Subroutine to route subsurface flow through the watershed
!DJG ----------------------------------------------------------------
!
!  Called from: main.f (Noah_router_driver)
!
!  Returns: qsub=DQSUB   which in turn becomes SUBFLO in head calc.
!
!  Created:    D. Gochis                           3/27/03
!              Adaptded from Wigmosta, 1994
!
!  Modified:   D. Gochis                           1/05/04
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id,&
               up_id,mpp_land_com_real,MPP_LAND_UB_COM, &
               MPP_LAND_LR_COM,mpp_land_com_integer
#endif

        IMPLICIT NONE


!! Declare Passed variables

        INTEGER, INTENT(IN) :: XX,YY

!! Declare passed arrays

        REAL, INTENT(IN), DIMENSION(XX,YY) :: z
        REAL, INTENT(IN), DIMENSION(XX,YY) :: sox
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soy
        REAL, INTENT(IN), DIMENSION(XX,YY) :: latksat
        REAL, INTENT(IN), DIMENSION(XX,YY) :: CWATAVAIL
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soldep
        REAL, INTENT(OUT), DIMENSION(XX,YY) :: qsub
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: QSUBDRY
        REAL, INTENT(INOUT) :: QSUBDRYT
        REAL, INTENT(IN) :: SUBDT
        real, intent(in), dimension(xx,yy,9) :: dist 

!!! Declare Local Variables

        REAL :: dzdx,dzdy,beta,gamma
        REAL :: qqsub,hh,ksat, gsize

        INTEGER :: i,j
!!! Initialize variables
        REAL, PARAMETER :: nexp=1.0      ! local power law exponent
        qsub = 0.                        ! initialize flux = 0. !DJG 5 May 2014

!yw        soldep = 2.
        

! Begin Subsurface routing

!!! Loop to route water in x-direction
        do j=1,YY
          do i=1,XX
! check for boundary grid point?
          if (i.eq.XX) GOTO 998
          gsize = dist(i,j,3)

          dzdx= (z(i,j) - z(i+1,j))/gsize
          beta=sox(i,j) + dzdx + 1E-30
          if (abs(beta) .lt. 1E-20) beta=1E-20
          if (beta.lt.0) then
!yw            hh=(1-(z(i+1,j)/soldep(i,j)))**nexp
            hh=(1-(z(i+1,j)/soldep(i+1,j)))**nexp
! Change later to use mean Ksat of two cells
            ksat=latksat(i+1,j)
          else
            hh=(1-(z(i,j)/soldep(i,j)))**nexp
            ksat=latksat(i,j)
          end if

          if (hh .lt. 0.) then
            print *, "hsub<0 at gridcell...", i,j,hh,z(i+1,j),z(i,j), &
                      soldep(i,j),nexp
            call hydro_stop("In ROUTE_SUBSURFACE2() - hsub<0 at gridcell")
          end if

!Err. tan slope          gamma=-1.*((gsize*ksat*soldep(i,j))/nexp)*tan(beta)
!AD_CHANGE: beta is already a slope so no tan (consistent with ROUTE_SUBSURFACE1) 
          gamma=-1.*((gsize*ksat*soldep(i,j))/nexp)*beta
!DJG lacks tan(beta) of original Wigmosta version          gamma=-1.*((gsize*ksat*soldep(i,j))/nexp)*beta

          qqsub = gamma * hh
          qsub(i,j) = qsub(i,j) + qqsub
          qsub(i+1,j) = qsub(i+1,j) - qqsub

! Boundary adjustments
#ifdef MPP_LAND
          if ((i.eq.1).AND.(beta.lt.0.).and.(left_id.lt.0)) then
#else
          if ((i.eq.1).AND.(beta.lt.0.)) then
#endif
            qsub(i,j) = qsub(i,j) - qqsub
            QSUBDRY(i,j) = QSUBDRY(i,j) - qqsub
            QSUBDRYT = QSUBDRYT - qqsub
#ifdef MPP_LAND
          else if ((i.eq.(xx-1)).AND.(beta.gt.0.) &
              .and.(right_id.lt.0) ) then
#else
          else if ((i.eq.(xx-1)).AND.(beta.gt.0.)) then
#endif
            qsub(i+1,j) = qsub(i+1,j) + qqsub
            QSUBDRY(i+1,j) = QSUBDRY(i+1,j) + qqsub
            QSUBDRYT = QSUBDRYT + qqsub
          end if

998       continue

!! End loop to route sfc water in x-direction
          end do
        end do

#ifdef MPP_LAND
       call MPP_LAND_LR_COM(qsub,XX,YY,99)
       call MPP_LAND_LR_COM(QSUBDRY,XX,YY,99)
#endif


!!! Loop to route water in y-direction
        do j=1,YY
          do i=1,XX
! check for boundary grid point?
          if (j.eq.YY) GOTO 999
          gsize = dist(i,j,1)

          dzdy= (z(i,j) - z(i,j+1))/gsize
          beta=soy(i,j) + dzdy + 1E-30
          if (abs(beta) .lt. 1E-20) beta=1E-20
          if (beta.lt.0) then
!yw            hh=(1-(z(i,j+1)/soldep(i,j)))**nexp
            hh=(1-(z(i,j+1)/soldep(i,j+1)))**nexp
            ksat=latksat(i,j+1)
          else
            hh=(1-(z(i,j)/soldep(i,j)))**nexp
            ksat=latksat(i,j)
          end if

          if (hh .lt. 0.) GOTO 999

!Err. tan slope          gamma=-1.*((gsize*ksat*soldep(i,j))/nexp)*tan(beta)
          gamma=-1.*((gsize*ksat*soldep(i,j))/nexp)*beta

          qqsub = gamma * hh
          qsub(i,j) = qsub(i,j) + qqsub
          qsub(i,j+1) = qsub(i,j+1) - qqsub

! Boundary adjustments

#ifdef MPP_LAND
          if ((j.eq.1).AND.(beta.lt.0.).and.(down_id.lt.0)) then
#else
          if ((j.eq.1).AND.(beta.lt.0.)) then
#endif
            qsub(i,j) = qsub(i,j) - qqsub
            QSUBDRY(i,j) = QSUBDRY(i,j) - qqsub
            QSUBDRYT = QSUBDRYT - qqsub
#ifdef MPP_LAND
          else if ((j.eq.(yy-1)).AND.(beta.gt.0.)  &
                .and. (up_id.lt.0) ) then
#else
          else if ((j.eq.(yy-1)).AND.(beta.gt.0.)) then
#endif
            qsub(i,j+1) = qsub(i,j+1) + qqsub
            QSUBDRY(i,j+1) = QSUBDRY(i,j+1) + qqsub
            QSUBDRYT = QSUBDRYT + qqsub
          end if

999       continue

!! End loop to route sfc water in y-direction
          end do
        end do

#ifdef MPP_LAND
       call MPP_LAND_UB_COM(qsub,XX,YY,99)
       call MPP_LAND_UB_COM(QSUBDRY,XX,YY,99)
#endif

        return
!DJG------------------------------------------------------------
        end subroutine ROUTE_SUBSURFACE2
!DJG------------------------------------------------------------



!DJG ------------------------------------------------
!DJG   SUBROUTINE OV_RTNG
!DJG ------------------------------------------------

	SUBROUTINE OV_RTNG(DT,DTRT_TER,IXRT,JXRT,INFXSUBRT,      &
          SFCHEADSUBRT,DHRT,CH_NETRT,RETDEPRT,OVROUGHRT,      &
          QSTRMVOLRT,QBDRYRT,QSTRMVOLTRT,QBDRYTRT,SOXRT,     &
          SOYRT,dist,LAKE_MSKRT,LAKE_INFLORT,LAKE_INFLOTRT,  &
          SO8RT,SO8RT_D,rt_option,q_sfcflx_x,q_sfcflx_y)

!yyww 
#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id, &
              up_id,mpp_land_com_real, my_id, &
             mpp_land_sync
#endif

	IMPLICIT NONE

!DJG --------DECLARATIONS----------------------------

	INTEGER, INTENT(IN)			:: IXRT,JXRT
	REAL, INTENT(IN)			:: DT,DTRT_TER

	INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETRT
	INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)	:: INFXSUBRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)	:: SOXRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)	:: SOYRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT,9):: dist 
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)	:: RETDEPRT
	REAL, INTENT(IN), DIMENSION(IXRT,JXRT)	:: OVROUGHRT

	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)	:: SFCHEADSUBRT
	REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)	:: DHRT

	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: QSTRMVOLRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_INFLORT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: QBDRYRT
	REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: q_sfcflx_x,q_sfcflx_y
	REAL, INTENT(INOUT)     :: QSTRMVOLTRT,QBDRYTRT,LAKE_INFLOTRT
        REAL, INTENT(IN), DIMENSION(IXRT,JXRT,8)  :: SO8RT

!DJG Local Variables

	INTEGER :: KRT,I,J,ct

	REAL, DIMENSION(IXRT,JXRT)	:: INFXS_FRAC
	REAL	:: DT_FRAC,SUM_INFXS,sum_head
        INTEGER SO8RT_D(IXRT,JXRT,3), rt_option
	
	


!DJG ----------------------------------------------------------------------
! DJG BEGIN 1-D or 2-D OVERLAND FLOW ROUTING LOOP
!DJG ---------------------------------------------------------------------
!DJG  Loop over 'routing time step'
!DJG  Compute the number of time steps based on NOAH DT and routing DTRT_TER

       DT_FRAC=INT(DT/DTRT_TER)

#ifdef HYDRO_D
       write(6,*) "OV_RTNG  DT_FRAC, DT, DTRT_TER",DT_FRAC, DT, DTRT_TER
       write(6,*) "IXRT, JXRT = ",ixrt,jxrt
#endif

!DJG NOTE: Applying all infiltration excess water at once then routing
!DJG       Pre-existing SFHEAD gets combined with Precip. in the
!DJG       calculation of INFXS1 during subroutine SRT.f.
!DJG debug


!DJG Assign all infiltration excess to surface head...
            SFCHEADSUBRT=INFXSUBRT

!DJG Divide infiltration excess over all routing time-steps
!	     INFXS_FRAC=INFXSUBRT/(DT/DTRT_TER)

!DJG Set flux accumulation fields to 0. before each loop...
      q_sfcflx_x = 0.
      q_sfcflx_y = 0.
      ct =0


!DJG Execute routing time-step loop...


      DO KRT=1,DT_FRAC

        DO J=1,JXRT
          DO I=1,IXRT

!DJG Removed 4_29_05, sfhead now updated in route_overland subroutine...
!           SFCHEADSUBRT(I,J)=SFCHEADSUBRT(I,J)+DHRT(I,J)
!!           SFCHEADSUBRT(I,J)=SFCHEADSUBRT(I,J)+DHRT(I,J)+INFXS_FRAC(I,J)
!           DHRT(I,J)=0.

!DJG ERROR Check...

	   IF (SFCHEADSUBRT(I,J).lt.0.) THEN 
#ifdef HYDRO_D
		print *, "ywcheck 2 ERROR!!!: Neg. Surface Head Value at (i,j):",    &
                    i,j,SFCHEADSUBRT(I,J)
                print *, "RETDEPRT(I,J) = ",RETDEPRT(I,J), "KRT=",KRT
                print *, "INFXSUBRT(i,j)=",INFXSUBRT(i,j)
                print *, "jxrt=",jxrt," ixrt=",ixrt
#endif
           END IF

!DJG Remove surface water from channel cells
!DJG Channel inflo cells specified as nonzeros from CH_NET
!DJG 9/16/04  Channel Extractions Removed until stream model implemented...



!yw            IF (CH_NETRT(I,J).ne.-9999) THEN
           IF (CH_NETRT(I,J).ge.0) THEN
             ct = ct +1

!DJG Temporary test to up the retention depth of channel grid cells to 'soak' 
!more water into valleys....set retdep = retdep*100 (=5 mm)

!	     RETDEPRT(I,J) = RETDEPRT(I,J) * 100.0    !DJG TEMP HARDWIRE!!!!
!	     RETDEPRT(I,J) = 10.0    !DJG TEMP HARDWIRE!!!!

             IF (SFCHEADSUBRT(I,J).GT.RETDEPRT(I,J)) THEN
!!               QINFLO(CH_NET(I,J)=QINFLO(CH_NET(I,J)+SFCHEAD(I,J) - RETDEPRT(I,J)
               QSTRMVOLTRT = QSTRMVOLTRT + (SFCHEADSUBRT(I,J) - RETDEPRT(I,J))
               QSTRMVOLRT(I,J) = QSTRMVOLRT(I,J)+SFCHEADSUBRT(I,J)-RETDEPRT(I,J)

             ! if(QSTRMVOLRT(I,J) .gt. 0) then 
             !     print *, "QSTRVOL GT 0", QSTRMVOLRT(I,J),I,J 
             !  endif

               SFCHEADSUBRT(I,J) = RETDEPRT(I,J)
             END IF
           END IF

!DJG Lake inflow withdrawl from surface head...(4/29/05)
           

           IF (LAKE_MSKRT(I,J).gt.0) THEN
             IF (SFCHEADSUBRT(I,J).GT.RETDEPRT(I,J)) THEN
               LAKE_INFLOTRT = LAKE_INFLOTRT + (SFCHEADSUBRT(I,J) - RETDEPRT(I,J))
               LAKE_INFLORT(I,J) = LAKE_INFLORT(I,J)+SFCHEADSUBRT(I,J)-RETDEPRT(I,J)
               SFCHEADSUBRT(I,J) = RETDEPRT(I,J)
              
             END IF
           END IF



         END DO
        END DO

!yw check         call MPP_LAND_COM_REAL(QSTRMVOLRT,IXRT,JXRT,99)
!DJG----------------------------------------------------------------------
!DJG CALL OVERLAND FLOW ROUTING SUBROUTINE
!DJG----------------------------------------------------------------------

!DJG Debug...


           if(rt_option .eq. 1) then
              CALL ROUTE_OVERLAND1(DTRT_TER,dist,SFCHEADSUBRT,DHRT,SOXRT,   &
		SOYRT,RETDEPRT,OVROUGHRT,IXRT,JXRT,QBDRYRT,QBDRYTRT,    & 
                SO8RT,SO8RT_D,q_sfcflx_x,q_sfcflx_y)
            else
              CALL ROUTE_OVERLAND2(DTRT_TER,dist,SFCHEADSUBRT,DHRT,SOXRT,   &
                  SOYRT,RETDEPRT,OVROUGHRT,IXRT,JXRT,QBDRYRT,QBDRYTRT,  &
                  q_sfcflx_x,q_sfcflx_y)    
            end if
             
        END DO          ! END routing time steps

#ifdef HYDRO_D
 	print *, "End of OV_routing call..."
#endif

!----------------------------------------------------------------------
! END OVERLAND FLOW ROUTING LOOP
!     CHANNEL ROUTING TO FOLLOW 
!----------------------------------------------------------------------

!DJG ----------------------------------------------------------------
  END SUBROUTINE OV_RTNG 
!DJG ----------------------------------------------------------------

!DJG     SUBROUTINE ROUTE_OVERLAND1
!DJG ----------------------------------------------------------------

          SUBROUTINE ROUTE_OVERLAND1(dt,                                &
     &          gsize,h,qsfc,sox,soy,                                   &
     &     retent_dep,dist_rough,XX,YY,QBDRY,QBDRYT,SO8RT,SO8RT_D,      &
     &     q_sfcflx_x,q_sfcflx_y)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Subroutine to route excess rainfall over the watershed
!     using a 2d diffusion routing scheme.
!
!  Called from: main.f
!
!      Will try to formulate this to be called from NOAH
!
!  Returns: qsfc=DQOV   which in turn becomes DH in head calc.
!
!  Created:  Adaptded from CASC2D source code
!  NOTE: dh from original code has been replaced by qsfc
!        dhh replaced by qqsfc
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id, &
              up_id,mpp_land_com_real, my_id, mpp_land_com_real8,&
             mpp_land_sync
#endif

        IMPLICIT NONE


!! Declare Passed variables

        INTEGER, INTENT(IN) :: XX,YY
        REAL, INTENT(IN) :: dt, gsize(xx,yy,9)

!! Declare passed arrays

        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: h
        REAL, INTENT(IN), DIMENSION(XX,YY) :: qsfc
        REAL, INTENT(IN), DIMENSION(XX,YY) :: sox
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soy
        REAL, INTENT(IN), DIMENSION(XX,YY) :: retent_dep
        REAL, INTENT(IN), DIMENSION(XX,YY) :: dist_rough
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: QBDRY
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: q_sfcflx_x, q_sfcflx_y
        REAL, INTENT(INOUT) :: QBDRYT
        REAL, INTENT(IN), DIMENSION(XX,YY,8) :: SO8RT
        REAL*8, DIMENSION(XX,YY) :: QBDRY_tmp, DH
        REAL*8, DIMENSION(XX,YY) :: DH_tmp

!!! Declare Local Variables

        REAL :: dhdx,dhdy,alfax,alfay
        REAL :: hh53,qqsfc,hh,dt_new,hmax
        REAL :: sfx,sfy
        REAL :: tmp_adjust

        INTEGER :: i,j
        REAL IXX8,IYY8
        INTEGER  IXX0,JYY0,index, SO8RT_D(XX,YY,3)
        REAL  tmp_gsize,hsum

!!! Initialize variables



!!! Begin Routing of Excess Rainfall over the Watershed

        DH=0.
        DH_tmp=0.
        QBDRY_tmp =0.

!!! Loop to route water
        do j=2,YY-1
          do i=2,XX-1
          if (h(I,J).GT.retent_dep(I,J)) then 
             IXX0 = SO8RT_D(i,j,1)
             JYY0 = SO8RT_D(i,j,2)
             index = SO8RT_D(i,j,3)
             tmp_gsize = 1.0/gsize(i,j,index)
             sfx = so8RT(i,j,index)-(h(IXX0,JYY0)-h(i,j))*0.001*tmp_gsize
             hmax = h(i,j)*0.001  !Specify max head for mass flux limit...
             if(sfx .lt. 1E-20) then
               call GETMAX8DIR(IXX0,JYY0,I,J,H,RETENT_DEP,so8rt,gsize(i,j,:),sfx,XX,YY)
             end if
             if(IXX0 > 0) then  ! do the rest if the lowest grid can be found.
                 if(sfx .lt. 1E-20) then
#ifdef HYDRO_D
                      print*, "Message: sfx reset to 1E-20. sfx =",sfx
                      print*, "i,j,index,IXX0,JYY0",i,j,index,IXX0,JYY0
                      print*, "so8RT(i,j,index), h(IXX0,JYY0), h(i,j), gsize(i,j,index) ", &
                         so8RT(i,j,index), h(IXX0,JYY0), h(i,j), gsize(i,j,index)
#endif
                      sfx = 1E-20
                 end if
                 alfax = sqrt(sfx) / dist_rough(i,j) 
                 hh=(h(i,j)-retent_dep(i,j)) * 0.001
                 hh53=hh**(5./3.)

! Calculate q-flux...
                 qqsfc = alfax*hh53*dt * tmp_gsize

!Courant check (simple mass limit on overland flow)...
                 if (qqsfc.ge.(hmax*dt*tmp_gsize)) qqsfc = hmax*dt*tmp_gsize

! Accumulate directional fluxes on routing subgrid...
                 if (IXX0.gt.i) then
                   q_sfcflx_x(I,J) = q_sfcflx_x(I,J) + qqsfc * &
                         (1.0 - 0.5 * (ABS(j-JYY0)))
                 else if (IXX0.lt.i) then
                   q_sfcflx_x(I,J) = q_sfcflx_x(I,J) - 1.0 * &
                         qqsfc * (1.0 - 0.5 * (ABS(j-JYY0)))
                 else
                   q_sfcflx_x(I,J) = q_sfcflx_x(I,J) + 0.
                 end if
                 if (JYY0.gt.j) then
                   q_sfcflx_y(I,J) = q_sfcflx_y(I,J) + qqsfc * &
                          (1.0 - 0.5 * (ABS(i-IXX0)))
                 elseif (JYY0.lt.j) then
                   q_sfcflx_y(I,J) = q_sfcflx_y(I,J) - 1.0 * &
                          qqsfc * (1.0 - 0.5 * (ABS(i-IXX0)))
                 else
                   q_sfcflx_y(I,J) = q_sfcflx_y(I,J) + 0.
                 end if


!DJG put adjustment in for (h) due to qqsfc

!yw changed as following:
                 tmp_adjust=qqsfc*1000
                 if((h(i,j) - tmp_adjust) <0 )  then
#ifdef HYDRO_D
                   print*, "Error Warning: surface head is negative:  ",i,j,ixx0,jyy0, &
                       h(i,j) - tmp_adjust
#endif
                     tmp_adjust = h(i,j)
                 end if
 	         DH(i,j) = DH(i,j)-tmp_adjust
                 DH_tmp(ixx0,jyy0) = DH_tmp(ixx0,jyy0) + tmp_adjust
      !yw end change
                  
      !DG Boundary adjustments here
            !DG Constant Flux Condition
#ifdef MPP_LAND
      if( ((ixx0.eq.XX).and.(right_id .lt. 0)) .or. &
          ((ixx0.eq.1) .and.(left_id  .lt. 0)) .or. &
          ((jyy0.eq.1) .and.(down_id  .lt. 0)) .or. &
          ((JYY0.eq.YY).and.(up_id    .lt. 0)) ) then 
!              QBDRY_tmp(IXX0,JYY0)=QBDRY_tmp(IXX0,JYY0) - qqsfc*1000.
#else
                if ((ixx0.eq.XX).or.(ixx0.eq.1).or.(jyy0.eq.1)   &
                     .or.(JYY0.eq.YY )) then
!                     QBDRY(IXX0,JYY0)=QBDRY(IXX0,JYY0) - qqsfc*1000.
#endif
                     QBDRY_tmp(IXX0,JYY0)=QBDRY_tmp(IXX0,JYY0) - qqsfc*1000.
                     QBDRYT=QBDRYT - qqsfc
                     DH_tmp(IXX0,JYY0)= DH_tmp(IXX0,JYY0)-tmp_adjust
                end if
             end if
!! End loop to route sfc water 
          end if
          end do
        end do

#ifdef MPP_LAND
! use double precision to solve the underflow problem.
       call MPP_LAND_COM_REAL8(DH_tmp,XX,YY,1)
       call MPP_LAND_COM_REAL8(QBDRY_tmp,XX,YY,1)
#endif
       QBDRY = QBDRY + QBDRY_tmp
       DH = DH+DH_tmp 

#ifdef MPP_LAND
       call MPP_LAND_COM_REAL8(DH,XX,YY,99)
       call MPP_LAND_COM_REAL(QBDRY,XX,YY,99)
#endif

        H = H + DH

        return

!DJG ----------------------------------------------------------------------
        end subroutine ROUTE_OVERLAND1


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
!DJG ----------------------------------------------------------------
!DJG     SUBROUTINE ROUTE_SUBSURFACE1
!DJG ----------------------------------------------------------------

          SUBROUTINE ROUTE_SUBSURFACE1(                                 &
                dist,z,qsub,sox,soy,                                   &
                latksat,soldep,XX,YY,QSUBDRY,QSUBDRYT,SO8RT,SO8RT_D,    &
                CWATAVAIL,SUBDT)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Subroutine to route subsurface flow through the watershed
!
!  Called from: main.f (Noah_router_driver)
!
!  Returns: qsub=DQSUB   which in turn becomes SUBFLO in head calc.
!
!  Created:    D. Gochis                           3/27/03
!              Adaptded from Wigmosta, 1994
!
!  Modified:   D. Gochis                           1/05/04
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id,&
           up_id,mpp_land_com_real8,my_id,mpp_land_com_real
#endif

        IMPLICIT NONE


!! Declare Passed variables

        INTEGER, INTENT(IN) :: XX,YY

!! Declare passed arrays

        REAL, INTENT(IN), DIMENSION(XX,YY) :: z
        REAL, INTENT(IN), DIMENSION(XX,YY) :: sox
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soy
        REAL, INTENT(IN), DIMENSION(XX,YY) :: latksat
        REAL, INTENT(IN), DIMENSION(XX,YY) :: CWATAVAIL
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soldep
        REAL, INTENT(OUT), DIMENSION(XX,YY) :: qsub
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: QSUBDRY
        REAL, INTENT(INOUT) :: QSUBDRYT
        REAL*8, DIMENSION(XX,YY) :: qsub_tmp,QSUBDRY_tmp
!yw        INTEGER, INTENT(OUT) :: flag
        REAL, INTENT(IN) :: dist(xx,yy,9),SUBDT

!!! Declare Local Variables

        REAL :: dzdx,dzdy,beta,gamma
        REAL :: qqsub,hh,ksat

        REAL :: SO8RT(XX,YY,8)
        INTEGER :: SO8RT_D(XX,YY,3), rt_option
        

!!! Initialize variables

        REAL, PARAMETER :: nexp=1.0      ! local power law exponent
        integer IXX0,JYY0,index,i,j
        real tmp_gsize

!     temporary set it to be 2. Should be passed in.
!yw        soldep = 2.
! Begin Subsurface routing



!!! Loop to route water in x-direction
        qsub_tmp = 0.
        QSUBDRY_tmp = 0.

#ifdef HYDRO_D
        write(6,*) "call subsurface routing xx= , yy =", yy, xx
#endif

        do j=2,YY-1
          do i=2,XX-1


        if(i.ge.2.AND.i.le.XX-1.AND.j.ge.2.AND.j.le.YY-1) then !if grdcl chk
! check for boundary grid point?
          IXX0 = SO8RT_D(i,j,1)
          JYY0 = SO8RT_D(i,j,2)

          index = SO8RT_D(i,j,3)

            if(dist(i,j,index) .le. 0) then
               write(6,*) "FATAL ERROR: dist(i,j,index) is <= zero "   
               call hydro_stop("In ROUTE_SUBSURFACE1() - dist(i,j,index) is <= zero ")
            endif
            if(soldep(i,j) .eq. 0) then
               call hydro_stop("In ROUTE_SUBSURFACE1() - soldep is = zero")
            endif

          tmp_gsize = 1.0/dist(i,j,index)

       
          dzdx= (z(i,j) - z(IXX0,JYY0) )* tmp_gsize
          beta=so8RT(i,j,index) + dzdx 

          if(beta .lt. 1E-20 ) then   !if-then for direction...
            call GETSUB8(IXX0,JYY0,I,J,Z,so8rt,dist(i,j,:),beta,XX,YY)
          end if
          if(beta .gt. 0) then            !if-then for flux calc 
              if(beta .lt. 1E-20 ) then
#ifdef HYDRO_D
                   print*, "Message: beta need to be reset to 1E-20. beta = ",beta
#endif
                   beta = 1E-20
              end if

! do the rest if the lowest grid can be found.
              hh=(1-(z(i,j)/soldep(i,j)))**nexp
              ksat=latksat(i,j)

              if (hh .lt. 0.) then
                 print *, "hsub<0 at gridcell...", i,j,hh,z(i+1,j),z(i,j), &
                      soldep(i,j)
                 call hydro_stop("In ROUTE_SUBSURFACE1() - hsub<0 at gridcell ") 
              end if

!err. tan slope     gamma=-1.0*((gsize*ksat*soldep(i,j))/nexp)*tan(beta)
              gamma=-1.0*((dist(i,j,index)*ksat*soldep(i,j))/nexp)*beta
              qqsub = gamma * hh

! ADCHANGE: Moved this water available constraint from outside qsub calc loop to inside
!           to better account for adjustments to adjacent cells
              if( qqsub .le. 0 .and. CWATAVAIL(i,j).lt.ABS(qqsub)/dist(i,j,9)*SUBDT) THEN
                  qqsub = -1.0*CWATAVAIL(i,j)*dist(i,j,9)/SUBDT
              end if

              qsub(i,j) = qsub(i,j) + qqsub
              qsub_tmp(ixx0,jyy0) = qsub_tmp(ixx0,jyy0) - qqsub

!!DJG Error Checks...
              if(qqsub .gt. 0) then
                    print*, "FATAL ERROR: qqsub should be negative, qqsub =",qqsub,&
                       "gamma=",gamma,"hh=",hh,"beta=",beta,"dzdx=",dzdx,&
                       "so8RT=",so8RT(i,j,index),"latksat=",ksat, &
                       "tan(beta)=",tan(beta),i,j,z(i,j),z(IXX0,JYY0)
                    print*, "ixx0=",ixx0, "jyy0=",jyy0
                    print*, "soldep =", soldep(i,j), "nexp=",nexp
                 call hydro_stop("In ROUTE_SUBSURFACE1() - qqsub should be negative") 
              end if




! Boundary adjustments
#ifdef MPP_LAND
      if( ((ixx0.eq.XX).and.(right_id .lt. 0)) .or. &
          ((ixx0.eq.1) .and.(left_id  .lt. 0)) .or. &
          ((jyy0.eq.1) .and.(down_id  .lt. 0)) .or. &
          ((JYY0.eq.YY).and.(up_id    .lt. 0)) ) then 
#else
              if ((ixx0.eq.1).or.(ixx0.eq.xx).or.(jyy0.eq.1).or.(jyy0.eq.yy)) then
#endif
                qsub_tmp(ixx0,jyy0) = qsub_tmp(ixx0,jyy0) + qqsub
                QSUBDRY_tmp(ixx0,jyy0) = QSUBDRY_tmp(ixx0,jyy0) + qqsub

                QSUBDRYT = QSUBDRYT + qqsub
              end if

998           continue

!! End loop to route sfc water in x-direction
      end if  !endif for flux calc

          endif   !! Endif for gridcell check...


          end do  !endif for i-dim
!CRNT debug          if(flag.eq.-99) exit !exit loop for courant violation...
        end do   !endif for j-dim

#ifdef MPP_LAND

       call MPP_LAND_COM_REAL8(qsub_tmp,XX,YY,1)
       call MPP_LAND_COM_REAL8(QSUBDRY_tmp,XX,YY,1)
#endif
       qsub = qsub + qsub_tmp
       QSUBDRY= QSUBDRY + QSUBDRY_tmp 

!ADNOTE: Moved this check to inside qsub calc loop, so no need for additional loop
!        do j=2,YY-1
!          do i=2,XX-1
!            if(dist(i,j,9) .le. 0) then
!               call hydro_stop("In ROUTE_SUBSURFACE1() - dist(i,j,9) is <= zero")
!            endif
!!DJG Feb 16, 2016...comment out to debug...line is identical to line 255
!!            if(CWATAVAIL(i,j).lt.ABS(qsub(i,j))/dist(i,j,9)*SUBDT) THEN
!!              qsub(i,j) = -1.0*CWATAVAIL(i,j)
!!            end if
!          end do
!        end do

#ifdef MPP_LAND
       call MPP_LAND_COM_REAL(qsub,XX,YY,99)
       call MPP_LAND_COM_REAL(QSUBDRY,XX,YY,99)
#endif


        return
!DJG------------------------------------------------------------
        end subroutine ROUTE_SUBSURFACE1
!DJG------------------------------------------------------------

!DJG------------------------------------------------------------


      SUBROUTINE GETSUB8(IXX0,JYY0,I,J,Z,sox,tmp_gsize,max,XX,YY)
          implicit none
          INTEGER:: IXX0,JYY0,IXX8,JYY8, XX, YY
          INTEGER, INTENT(IN) :: I,J

          REAL,INTENT(IN) :: Z(XX,YY),sox(XX,YY,8),tmp_gsize(9)
          REAL  max
          max = -1

          IXX8 = I
          JYY8 = J+1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,1),IXX0,JYY0,max,tmp_gsize(1),XX,YY)

          IXX8 = I+1
          JYY8 = J+1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,2),IXX0,JYY0,max,tmp_gsize(2),XX,YY)

          IXX8 = I+1
          JYY8 = J
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,3),IXX0,JYY0,max,tmp_gsize(3),XX,YY)

          IXX8 = I+1
          JYY8 = J-1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,4),IXX0,JYY0,max,tmp_gsize(4),XX,YY)

          IXX8 = I
          JYY8 = J-1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,5),IXX0,JYY0,max,tmp_gsize(5),XX,YY)

          IXX8 = I-1
          JYY8 = J-1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,6),IXX0,JYY0,max,tmp_gsize(6),XX,YY)

          IXX8 = I-1
          JYY8 = J
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,7),IXX0,JYY0,max,tmp_gsize(7),XX,YY)

          IXX8 = I-1
          JYY8 = J+1
          call GETSUB8DIR(IXX8,JYY8,I,J,Z,sox(:,:,8),IXX0,JYY0,max,tmp_gsize(8),XX,YY)
        RETURN
        END SUBROUTINE GETSUB8

        SUBROUTINE GETSUB8DIR(IXX8,JYY8,I,J,Z,sox,IXX0,JYY0,max,tmp_gsize,XX,YY)
        implicit none
        integer,INTENT(INOUT) ::IXX0,JYY0
        INTEGER, INTENT(IN) :: I,J,IXX8,JYY8,XX,YY
        REAL,INTENT(IN) :: Z(XX,YY),sox(XX,YY)
        REAL, INTENT(INOUT) ::max
        real, INTENT(IN) :: tmp_gsize
        real :: beta , dzdx

          dzdx= (z(i,j) - z(IXX0,JYY0) )/tmp_gsize
          beta=sox(i,j) + dzdx 
          if(max < beta ) then
                   IXX0 = IXX8
                   JYY0 = JYY8
                   max = beta 
          end if

        END SUBROUTINE GETSUB8DIR
!DJG ----------------------------------------------------------------------

!DJG     SUBROUTINE ROUTE_OVERLAND2
!DJG ----------------------------------------------------------------

          SUBROUTINE ROUTE_OVERLAND2 (dt,                               &
     &          dist,h,qsfc,sox,soy,                                   &
     &          retent_dep,dist_rough,XX,YY,QBDRY,QBDRYT,               &
     &          q_sfcflx_x,q_sfcflx_y)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  Subroutine to route excess rainfall over the watershed
!     using a 2d diffusion routing scheme.
!
!  Called from: main.f
!
!      Will try to formulate this to be called from NOAH
!
!  Returns: qsfc=DQOV   which in turn becomes DH in head calc.
!
!  Created:  Adaptded from CASC2D source code
!  NOTE: dh from original code has been replaced by qsfc
!        dhh replaced by qqsfc
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id,&
               up_id,mpp_land_com_real,MPP_LAND_UB_COM, &
               MPP_LAND_LR_COM,mpp_land_com_integer
#endif

        IMPLICIT NONE


!! Declare Passed variables

        real :: gsize
        INTEGER, INTENT(IN) :: XX,YY
        REAL, INTENT(IN) :: dt , dist(XX,YY,9)

!! Declare passed arrays

        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: h
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: qsfc
        REAL, INTENT(IN), DIMENSION(XX,YY) :: sox
        REAL, INTENT(IN), DIMENSION(XX,YY) :: soy
        REAL, INTENT(IN), DIMENSION(XX,YY) :: retent_dep
        REAL, INTENT(IN), DIMENSION(XX,YY) :: dist_rough
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: QBDRY
        REAL, INTENT(INOUT), DIMENSION(XX,YY) :: q_sfcflx_x,q_sfcflx_y
        REAL, INTENT(INOUT) :: QBDRYT
        REAL  :: DH(XX,YY)

!!! Declare Local Variables

        REAL :: dhdx,dhdy,alfax,alfay
        REAL :: hh53,qqsfc,hh,dt_new
        REAL :: sfx,sfy
        REAL :: tmp_adjust

        INTEGER :: i,j

!!! Initialize variables




!!! Begin Routing of Excess Rainfall over the Watershed


        DH = 0
!!! Loop to route water in x-direction
        do j=1,YY
          do i=1,XX


! check for boundary gridpoint?
          if (i.eq.XX) GOTO 998
           gsize = dist(i,j,3)


! check for detention storage?
          if (h(i,j).lt.retent_dep(i,j).AND.     &
              h(i+1,j).lt.retent_dep(i+1,j)) GOTO 998

          dhdx = (h(i+1,j)/1000. - h(i,j)/1000.) / gsize  ! gisze-(m),h-(mm)

          sfx = (sox(i,j)-dhdx+1E-30)
          if (abs(sfx).lt.1E-20) sfx=1E-20
          alfax = ((abs(sfx))**0.5)/dist_rough(i,j)
          if (sfx.lt.0.) then
              hh=(h(i+1,j)-retent_dep(i+1,j))/1000.
          else
              hh=(h(i,j)-retent_dep(i,j))/1000.
          end if

          if ((retent_dep(i,j).gt.0.).AND.(hh.le.0.)) GOTO 998
          if (hh.lt.0.) then
          GOTO 998
          end if

          hh53=hh**(5./3.)


! Calculate q-flux... (units (m))
          qqsfc = (sfx/abs(sfx))*alfax*hh53*dt/gsize
          q_sfcflx_x(I,J) = q_sfcflx_x(I,J) + qqsfc

!DJG put adjustment in for (h) due to qqsfc

!yw changed as following:
           tmp_adjust=qqsfc*1000
          if(tmp_adjust .le. 0 ) GOTO 998
           if((h(i,j) - tmp_adjust) <0 )  then
#ifdef HYDRO_D
               print*, "WARNING: surface head is negative:  ",i,j
#endif
               tmp_adjust = h(i,j)
           end if
           if((h(i+1,j) + tmp_adjust) <0) then 
#ifdef HYDRO_D
               print*, "WARNING: surface head is negative: ",i+1,j
#endif
               tmp_adjust = -1*h(i+1,j)
           end if
 	   Dh(i,j) = Dh(i,j)-tmp_adjust
           Dh(i+1,j) = Dh(i+1,j) + tmp_adjust
!yw end change



!DG Boundary adjustments here
!DG Constant Flux Condition
#ifdef MPP_LAND
          if ((i.eq.1).AND.(sfx.lt.0).and. & 
                (left_id .lt. 0) ) then
#else
          if ((i.eq.1).AND.(sfx.lt.0)) then
#endif
             Dh(i,j) = Dh(i,j) + qqsfc*1000.
            QBDRY(I,J)=QBDRY(I,J) + qqsfc*1000.
            QBDRYT=QBDRYT + qqsfc*1000.
#ifdef MPP_LAND
          else if ( (i.eq.(XX-1)).AND.(sfx.gt.0) &
             .and. (right_id .lt. 0) ) then
#else
          else if ((i.eq.(XX-1)).AND.(sfx.gt.0)) then
#endif
             tmp_adjust = qqsfc*1000.
             if(h(i+1,j).lt.tmp_adjust) tmp_adjust = h(i+1,j)
             Dh(i+1,j) = Dh(i+1,j) - tmp_adjust
!DJG Re-assign h(i+1) = 0.0 when <0.0 (from rounding/truncation error)
            QBDRY(I+1,J)=QBDRY(I+1,J) - tmp_adjust
            QBDRYT=QBDRYT - tmp_adjust
          end if


998     continue

!! End loop to route sfc water in x-direction
          end do
        end do

        H = H + DH
#ifdef MPP_LAND
       call MPP_LAND_LR_COM(H,XX,YY,99)
       call MPP_LAND_LR_COM(QBDRY,XX,YY,99)
#endif


        DH = 0
!!!! Loop to route water in y-direction
        do j=1,YY
          do i=1,XX

!! check for boundary grid point?
          if (j.eq.YY) GOTO 999
           gsize = dist(i,j,1)


!! check for detention storage?
          if (h(i,j).lt.retent_dep(i,j).AND.     & 
              h(i,j+1).lt.retent_dep(i,j+1)) GOTO 999

          dhdy = (h(i,j+1)/1000. - h(i,j)/1000.) / gsize

          sfy = (soy(i,j)-dhdy+1E-30)
          if (abs(sfy).lt.1E-20) sfy=1E-20
          alfay = ((abs(sfy))**0.5)/dist_rough(i,j)
          if (sfy.lt.0.) then
              hh=(h(i,j+1)-retent_dep(i,j+1))/1000.
          else
              hh=(h(i,j)-retent_dep(i,j))/1000.
          end if

          if ((retent_dep(i,j).gt.0.).AND.(hh.le.0.)) GOTO 999
          if (hh.lt.0.) then
            GOTO 999
          end if

         hh53=hh**(5./3.)

! Calculate q-flux...
          qqsfc = (sfy/abs(sfy))*alfay*hh53*dt / gsize
          q_sfcflx_y(I,J) = q_sfcflx_y(I,J) + qqsfc


!DJG put adjustment in for (h) due to qqsfc
!yw	  h(i,j) = h(i,j)-qqsfc*1000.
!yw          h(i,j+1) = h(i,j+1) + qqsfc*1000.
!yw changed as following:
           tmp_adjust=qqsfc*1000
          if(tmp_adjust .le. 0 ) GOTO 999

           if((h(i,j) - tmp_adjust) <0 )  then
#ifdef HYDRO_D
               print *, "WARNING: surface head is negative:  ",i,j
#endif
               tmp_adjust = h(i,j)
           end if
           if((h(i,j+1) + tmp_adjust) <0) then
#ifdef HYDRO_D
               print *, "WARNING: surface head is negative: ",i,j+1
#endif
               tmp_adjust = -1*h(i,j+1)
           end if
	  Dh(i,j) = Dh(i,j)-tmp_adjust
          Dh(i,j+1) = Dh(i,j+1) + tmp_adjust
!yw end change

!          qsfc(i,j) = qsfc(i,j)-qqsfc
!          qsfc(i,j+1) = qsfc(i,j+1) + qqsfc
!!DG Boundary adjustments here
!!DG Constant Flux Condition
#ifdef MPP_LAND
          if ((j.eq.1).AND.(sfy.lt.0)   &
             .and. (down_id .lt. 0) ) then
#else
          if ((j.eq.1).AND.(sfy.lt.0)) then
#endif
            Dh(i,j) = Dh(i,j) + qqsfc*1000.
            QBDRY(I,J)=QBDRY(I,J) + qqsfc*1000.
            QBDRYT=QBDRYT + qqsfc*1000.
#ifdef MPP_LAND
          else if ((j.eq.(YY-1)).AND.(sfy.gt.0) &
             .and. (up_id .lt. 0) ) then
#else
          else if ((j.eq.(YY-1)).AND.(sfy.gt.0)) then
#endif
             tmp_adjust = qqsfc*1000.
             if(h(i,j+1).lt.tmp_adjust) tmp_adjust = h(i,j+1)
             Dh(i,j+1) = Dh(i,j+1) - tmp_adjust
!DJG Re-assign h(j+1) = 0.0 when <0.0 (from rounding/truncation error)
            QBDRY(I,J+1)=QBDRY(I,J+1) - tmp_adjust
            QBDRYT=QBDRYT - tmp_adjust
          end if

999     continue

!!!! End loop to route sfc water in y-direction
          end do
        end do

        H = H +DH
#ifdef MPP_LAND
       call MPP_LAND_UB_COM(H,XX,YY,99)
       call MPP_LAND_UB_COM(QBDRY,XX,YY,99)
#endif
        return

!DJG ----------------------------------------------------------------------
        end subroutine ROUTE_OVERLAND2




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
           use module_namelist, only: nlst_rt
           integer :: did
           call disaggregateDomain( RT_DOMAIN(did)%IX,RT_DOMAIN(did)%JX,nlst_rt(did)%NSOIL,&
             RT_DOMAIN(did)%IXRT,RT_DOMAIN(did)%JXRT,nlst_rt(did)%AGGFACTRT,RT_DOMAIN(did)%SICE, &
             RT_DOMAIN(did)%SMC,RT_DOMAIN(did)%SH2OX,RT_DOMAIN(did)%INFXSRT, &
             rt_domain(did)%dist_lsm(:,:,9),RT_DOMAIN(did)%SMCMAX1,RT_DOMAIN(did)%SMCREF1, &
             RT_DOMAIN(did)%SMCWLT1,RT_DOMAIN(did)%VEGTYP,RT_DOMAIN(did)%LKSAT,RT_DOMAIN(did)%dist, &
             RT_DOMAIN(did)%INFXSWGT, RT_DOMAIN(did)%OVROUGHRTFAC,RT_DOMAIN(did)%LKSATFAC, &
             RT_DOMAIN(did)%CH_NETRT,RT_DOMAIN(did)%SH2OWGT,RT_DOMAIN(did)%SMCREFRT,       &
             RT_DOMAIN(did)%INFXSUBRT,RT_DOMAIN(did)%SMCMAXRT, RT_DOMAIN(did)%SMCWLTRT,    &
             RT_DOMAIN(did)%SMCRT, &
             RT_DOMAIN(did)%OVROUGHRT, RT_DOMAIN(did)%LAKE_MSKRT, &
             RT_DOMAIN(did)%LKSATRT, RT_DOMAIN(did)%OV_ROUGH,RT_DOMAIN(did)%SLDPTH, &
	     RT_DOMAIN(did)%soiltypRT, RT_DOMAIN(did)%soiltyp)

      end subroutine disaggregateDomain_drv

      subroutine disaggregateDomain(IX,JX,NSOIL,IXRT,JXRT,AGGFACTRT, &
                     SICE, SMC,SH2OX, INFXSRT, area_lsm, SMCMAX1,SMCREF1, &
               SMCWLT1, VEGTYP, LKSAT, dist,INFXSWGT,OVROUGHRTFAC, &
               LKSATFAC, CH_NETRT,SH2OWGT,SMCREFRT, INFXSUBRT,SMCMAXRT, &
               SMCWLTRT,SMCRT, OVROUGHRT, LAKE_MSKRT, LKSATRT,OV_ROUGH,  &
               SLDPTH, soiltypRT, soiltyp                               &
            )
#ifdef MPP_LAND
        use module_mpp_land, only: left_id,down_id,right_id, &
              up_id,mpp_land_com_real, my_id, io_id, numprocs, &
             mpp_land_sync,mpp_land_com_integer,mpp_land_max_int1, &
             sum_real1
#endif
     implicit none
        integer,INTENT(IN) :: IX,JX,NSOIL,IXRT,JXRT,AGGFACTRT
        real, INTENT(OUT), DIMENSION(IX,JX,NSOIL) :: SICE
        real, INTENT(IN),  DIMENSION(IX,JX,NSOIL) :: SMC,SH2OX
        real, INTENT(IN),  DIMENSION(IX,JX) :: INFXSRT, area_lsm, SMCMAX1,SMCREF1, &
                                               SMCWLT1,  LKSAT
        integer, INTENT(IN), DIMENSION(IX,JX)      :: VEGTYP, soiltyp

        real,INTENT(IN),DIMENSION(IXRT,JXRT,9)::dist
        real,INTENT(IN),DIMENSION(IXRT,JXRT)::INFXSWGT,OVROUGHRTFAC, &
               LKSATFAC
        integer,INTENT(INOUT), DIMENSION(IXRT,JXRT)     ::CH_NETRT, soiltypRT
        real,INTENT(IN),DIMENSION(IXRT,JXRT,NSOIL)::SH2OWGT
        real,INTENT(OUT),DIMENSION(IXRT,JXRT,NSOIL)::SMCREFRT, SMCMAXRT, &
               SMCWLTRT,SMCRT
        real,INTENT(OUT),DIMENSION(IXRT,JXRT)::INFXSUBRT
        real,INTENT(INOUT),DIMENSION(IXRT,JXRT)::OVROUGHRT, LKSATRT
        integer,INTENT(INOUT), DIMENSION(IXRT,JXRT)  ::LAKE_MSKRT
                  

        real,INTENT(IN), DIMENSION(NSOIL)      :: SLDPTH
        REAL    OV_ROUGH(*)



        integer :: i, j, AGGFACYRT, AGGFACXRT, IXXRT, JYYRT,KRT, KF
        REAL  ::  LSMVOL,SMCEXCS, WATHOLDCAP

#ifdef HYDRO_D
! ADCHANGE: Water balance variables
       integer :: kk
       real    :: smctot1,smcrttot2
       real    :: sicetot1
       real    :: suminfxs1,suminfxsrt2
#endif

!-------------------------------------



	SICE=SMC-SH2OX
        SMCREFRT = 0

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
        do i=1,IX
         do j=1,JX
            suminfxs1 = suminfxs1 + INFXSRT(I,J) / float(IX*JX)
            do kk=1,NSOIL
                smctot1 = smctot1 + SMC(I,J,KK)*SLDPTH(KK)*1000. / float(IX*JX)
                sicetot1 = sicetot1 + SICE(I,J,KK)*SLDPTH(KK)*1000. / float(IX*JX)
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


        do J=1,JX
          do I=1,IX

!DJG Weighting alg. alteration...
              LSMVOL=INFXSRT(I,J)*area_lsm(I,J)


             do AGGFACYRT=AGGFACTRT-1,0,-1
              do AGGFACXRT=AGGFACTRT-1,0,-1

               IXXRT=I*AGGFACTRT-AGGFACXRT
               JYYRT=J*AGGFACTRT-AGGFACYRT
#ifdef MPP_LAND
       if(left_id.ge.0) IXXRT=IXXRT+1
       if(down_id.ge.0) JYYRT=JYYRT+1
#else
!yw ????
!       IXXRT=IXXRT+1
!       JYYRT=JYYRT+1
#endif
!        if(AGGFACTRT .eq. 1) then
!            IXXRT=I
!            JYYRT=J
!        endif


!DJG Implement subgrid weighting routine...
               INFXSUBRT(IXXRT,JYYRT)=LSMVOL*     &
                   INFXSWGT(IXXRT,JYYRT)/dist(IXXRT,JYYRT,9)
  

            do KRT=1,NSOIL  !Do for soil profile loop
               IF(SICE(I,J,KRT).gt.0) then  !...adjust for soil ice
!DJG Adjust SMCMAX for SICE when subsfc routing...make 3d variable
                 SMCMAXRT(IXXRT,JYYRT,KRT)=SMCMAX1(I,J)-SICE(I,J,KRT)
                 SMCREFRT(IXXRT,JYYRT,KRT)=SMCREF1(I,J)-SICE(I,J,KRT)
                 WATHOLDCAP = SMCMAX1(I,J) - SMCWLT1(I,J)
                 IF (SICE(I,J,KRT).le.WATHOLDCAP)    then
                        SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J)      
                 else
                    if(SICE(I,J,KRT).lt.SMCMAX1(I,J)) &
                          SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J) - &
                          (SICE(I,J,KRT)-WATHOLDCAP)
                    if(SICE(I,J,KRT).ge.SMCMAX1(I,J)) SMCWLTRT(IXXRT,JYYRT,KRT) = 0.
                 end if
               ELSE
                 SMCMAXRT(IXXRT,JYYRT,KRT)=SMCMAX1(I,J)
                 SMCREFRT(IXXRT,JYYRT,KRT)=SMCREF1(I,J)
                 WATHOLDCAP = SMCMAX1(I,J) - SMCWLT1(I,J)
                 SMCWLTRT(IXXRT,JYYRT,KRT) = SMCWLT1(I,J) 
               END IF   !endif adjust for soil ice...


!Now Adjust soil moisture
!DJG Use SH2O instead of SMC for 'liquid' water...
                 IF(SMCMAXRT(IXXRT,JYYRT,KRT).GT.0) THEN !Check for smcmax data (=0 over water)
                   SMCRT(IXXRT,JYYRT,KRT)=SH2OX(I,J,KRT)*SH2OWGT(IXXRT,JYYRT,KRT)
!old                   SMCRT(IXXRT,JYYRT,KRT)=SMC(I,J,KRT)
                 ELSE
                   SMCRT(IXXRT,JYYRT,KRT) = 0.001  !will be skipped w/ landmask
                   SMCMAXRT(IXXRT,JYYRT,KRT) = 0.001
                 END IF
!DJG Check/Adjust so that subgrid cells do not exceed saturation...
                 IF (SMCRT(IXXRT,JYYRT,KRT).GT.SMCMAXRT(IXXRT,JYYRT,KRT)) THEN
                   SMCEXCS = (SMCRT(IXXRT,JYYRT,KRT) - SMCMAXRT(IXXRT,JYYRT,KRT)) &
                             * SLDPTH(KRT)*1000.  !Excess soil water in units of (mm)
                   SMCRT(IXXRT,JYYRT,KRT) = SMCMAXRT(IXXRT,JYYRT,KRT)
                   DO KF = KRT-1,1, -1  !loop back upward to redistribute excess water from disagg.
                     SMCRT(IXXRT,JYYRT,KF) = SMCRT(IXXRT,JYYRT,KF) + SMCEXCS/(SLDPTH(KF)*1000.) 
                     IF (SMCRT(IXXRT,JYYRT,KF).GT.SMCMAXRT(IXXRT,JYYRT,KF)) THEN  !Recheck new lyr sat.
                       SMCEXCS = (SMCRT(IXXRT,JYYRT,KF) - SMCMAXRT(IXXRT,JYYRT,KF)) &
                           * SLDPTH(KF)*1000.  !Excess soil water in units of (mm)
                       SMCRT(IXXRT,JYYRT,KF) = SMCMAXRT(IXXRT,JYYRT,KF)
                     ELSE  ! Excess soil water expired
                       SMCEXCS = 0.
                       EXIT
                     END IF
                   END DO
                   IF (SMCEXCS.GT.0) THEN  !If not expired by sfc then add to Infil. Excess
                     INFXSUBRT(IXXRT,JYYRT) = INFXSUBRT(IXXRT,JYYRT) + SMCEXCS
                     SMCEXCS = 0.
                   END IF
                 END IF  !End if for soil moisture saturation excess


             end do !End do for soil profile loop



             do KRT=1,NSOIL  !debug loop
               IF (SMCRT(IXXRT,JYYRT,KRT).GT.SMCMAXRT(IXXRT,JYYRT,KRT)) THEN
                      print *, "FATAL ERROR: SMCMAX exceeded upon disaggregation3...", ixxrt,jyyrt,krt,&
                       SMCRT(IXXRT,JYYRT,KRT),SMCMAXRT(IXXRT,JYYRT,KRT)
                      call hydro_stop("In disaggregateDomain() - SMCMAX exceeded upon disaggregation3")
               ELSE IF (SMCRT(IXXRT,JYYRT,KRT).LE.0.) THEN
                       print *, "FATAL ERROR: SMCRT fully depleted upon disaggregation...", ixxrt,jyyrt,krt,&
                       "SMCRT=",SMCRT(IXXRT,JYYRT,KRT),"SH2OWGT=",SH2OWGT(IXXRT,JYYRT,KRT),&
                       "SH2O=",SH2OX(I,J,KRT)
                       print*, "SMC=", SMC(i,j,KRT), "SICE =", sice(i,j,KRT)
                       print *, "VEGTYP = ", VEGTYP(I,J)
                       print *, "i,j,krt, nsoil",i,j,krt,nsoil
! ADCHANGE: If values are close but not exact, end up with a crash. Force values to match.
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



!DJG map ov roughness as function of land use provided in VEGPARM.TBL...
! --- added extra check for VEGTYP for 'masked-out' locations...
! --- out of basin locations (VEGTYP=0) have OVROUGH hardwired to 0.1
            IF (VEGTYP(I,J).LE.0) then
              OVROUGHRT(IXXRT,JYYRT) = 0.1     !COWS mask test
            ELSE
               OVROUGHRT(IXXRT,JYYRT)=OV_ROUGH(VEGTYP(I,J))*OVROUGHRTFAC(IXXRT,JYYRT)  ! Distributed calibration...1/17/2012
            END IF



!DJG 6.12.08 Map lateral hydraulic conductivity and apply distributed scaling
! ---        factor that will be read in from hires terrain file
!              LKSATRT(IXXRT,JYYRT) = LKSAT(I,J) 
!              LKSATRT(IXXRT,JYYRT) = LKSAT(I,J) * LKSATFAC(IXXRT,JYYRT) * &  !Apply scaling factor...
! ...and scale from max to 0 when SMC decreases from SMCMAX to SMCREF...
!!DJG error found from KIT,improper scaling       ((SMCMAXRT(IXXRT,JYYRT,NSOIL) - SMCRT(IXXRT,JYYRT,NSOIL)) / &
!                                    (max(0.,(SMCMAXRT(IXXRT,JYYRT,NSOIL) - SMCRT(IXXRT,JYYRT,NSOIL))) / &
!                                    (SMCMAXRT(IXXRT,JYYRT,NSOIL)-SMCREFRT(IXXRT,JYYRT,NSOIL)) )

!AD_CHANGE: 
!New model corrected to scale from 0 at SMCREF to full LKSAT*LKSATFAC at SMCMAX:
		LKSATRT(IXXRT,JYYRT) = LKSAT(I,J) * LKSATFAC(IXXRT,JYYRT) * &
				min (1., &     !just in case, make sure scale factor doesn't go over 1
					( max(0.,(SMCRT(IXXRT,JYYRT,NSOIL) - SMCREFRT(IXXRT,JYYRT,NSOIL))) / &     !becomes 0 if less than SMCREF
				(SMCMAXRT(IXXRT,JYYRT,NSOIL)-SMCREFRT(IXXRT,JYYRT,NSOIL)) ) )

!DJG set up lake mask...
!--- modify to make lake mask large here, but not one of the routed lakes!!!
!--            IF (VEGTYP(I,J).eq.16) then
               IF (VEGTYP(I,J).eq.16 .and. &
                        CH_NETRT(IXXRT,JYYRT).le.0) then
                 !--LAKE_MSKRT(IXXRT,JYYRT) = 1
!yw                 LAKE_MSKRT(IXXRT,JYYRT) = 9999
                 LAKE_MSKRT(IXXRT,JYYRT) = -9999
               end if
               ! BF disaggregate soiltype information for gw-soil-coupling
	       ! TODO: move this disaggregation code line to lsm_init section because soiltype is time-invariant
               soiltypRT(ixxrt,jyyrt) = soiltyp(i,j)


              end do
             end do

          end do
        end do

#ifdef HYDRO_D
! ADCHANGE: START Final water balance variables
! ALL VARS in MM
        suminfxsrt2 = 0.
        smcrttot2 = 0.
        do i=1,IXRT
          do j=1,JXRT
            suminfxsrt2 = suminfxsrt2 + INFXSUBRT(I,J) / float(IXRT*JXRT)
            do kk=1,NSOIL
                smcrttot2 = smcrttot2 + SMCRT(I,J,KK)*SLDPTH(KK)*1000. / float(IXRT*JXRT)
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
       if(my_id .eq. IO_id) then
#endif
        print *, "Disagg Mass Bal: "
        print *, "WB_DISAG!InfxsDiff", suminfxsrt2-suminfxs1
        print *, "WB_DISAG!Infxs1", suminfxs1
        print *, "WB_DISAG!Infxs2", suminfxsrt2
        print *, "WB_DISAG!SMCDIff", smcrttot2-(smctot1-sicetot1)
        print *, "WB_DISAG!SMC1", smctot1
        print *, "WB_DISAG!SICE1", sicetot1
        print *, "WB_DISAG!SMC2", smcrttot2
        print *, "WB_DISAG!Residual", (suminfxsrt2-suminfxs1) + &
                         (smcrttot2-(smctot1-sicetot1))
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
        call MPP_LAND_COM_REAL(OVROUGHRT,IXRT,JXRT,99)
        call MPP_LAND_COM_INTEGER(LAKE_MSKRT,IXRT,JXRT,99)
     do i = 1, NSOIL
        call MPP_LAND_COM_REAL(SMCMAXRT(:,:,i),IXRT,JXRT,99)
        call MPP_LAND_COM_REAL(SMCRT(:,:,i),IXRT,JXRT,99)
        call MPP_LAND_COM_REAL(SMCWLTRT(:,:,i),IXRT,JXRT,99)
     end DO
#endif

     end subroutine disaggregateDomain

         subroutine SubsurfaceRouting_drv(did)

             use module_RT_data, only: rt_domain
             use module_namelist, only: nlst_rt
             implicit none
             integer :: did
             IF (nlst_rt(did)%SUBRTSWCRT.EQ.1) THEN
                call subsurfaceRouting (RT_DOMAIN(did)%ixrt, RT_DOMAIN(did)%jxrt , nlst_rt(did)%nsoil, &
                     RT_DOMAIN(did)%SMCRT,RT_DOMAIN(did)%SMCMAXRT,RT_DOMAIN(did)%SMCREFRT,&
                     RT_DOMAIN(did)%SMCWLTRT, RT_DOMAIN(did)%ZSOIL, RT_DOMAIN(did)%SLDPTH, & 
                     nlst_rt(did)%DT,RT_DOMAIN(did)%ZWATTABLRT,RT_DOMAIN(did)%SOXRT,       &
                     RT_DOMAIN(did)%SOYRT,RT_DOMAIN(did)%LKSATRT, RT_DOMAIN(did)%SOLDEPRT, &
                     RT_DOMAIN(did)%INFXSUBRT,RT_DOMAIN(did)%QSUBBDRYTRT, RT_DOMAIN(did)%QSUBBDRYRT,&
                     RT_DOMAIN(did)%QSUBRT ,nlst_rt(did)%rt_option, RT_DOMAIN(did)%dist, &
                     RT_DOMAIN(did)%sub_resid,RT_DOMAIN(did)%SO8RT_D, RT_DOMAIN(did)%SO8RT)
             endif

         end subroutine SubsurfaceRouting_drv
     
         subroutine subsurfaceRouting (ixrt, jxrt , nsoil, &
                  SMCRT,SMCMAXRT,SMCREFRT,SMCWLTRT, &
                  ZSOIL, SLDPTH, &
                  DT,ZWATTABLRT,SOXRT,SOYRT,LKSATRT,&
                  SOLDEPRT,INFXSUBRT,QSUBBDRYTRT, QSUBBDRYRT,&
                  QSUBRT ,rt_option, dist,sub_resid,SO8RT_D, SO8RT)
#ifdef MPP_LAND
        use module_mpp_land, only:  mpp_land_com_real, mpp_land_com_integer
#endif
         implicit none
         integer, INTENT(IN) :: ixrt, jxrt , nsoil, rt_option
         REAL, INTENT(IN)                          :: DT
         real,INTENT(IN), DIMENSION(NSOIL)      :: ZSOIL, SLDPTH
         REAL, INTENT(IN), DIMENSION(IXRT,JXRT)   :: SOXRT,SOYRT,LKSATRT, SOLDEPRT , sub_resid
         real,INTENT(INOUT), DIMENSION(IXRT,JXRT)::INFXSUBRT
         real,INTENT(INOUT) :: QSUBBDRYTRT
         REAL, INTENT(OUT), DIMENSION(IXRT,JXRT)   :: QSUBBDRYRT, QSUBRT
         REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT,NSOIL) :: SMCRT, SMCWLTRT, SMCMAXRT,SMCREFRT


         INTEGER :: SO8RT_D(IXRT,JXRT,3)
         REAL :: SO8RT(IXRT,JXRT,8)
         REAL, INTENT(IN)                          :: dist(ixrt,jxrt,9)
!  -----local array ----------
         REAL, DIMENSION(IXRT,JXRT)   :: ZWATTABLRT
         REAL, DIMENSION(IXRT,JXRT)   :: CWATAVAIL
         INTEGER, DIMENSION(IXRT,JXRT) :: SATLYRCHK
 



         CWATAVAIL = 0.
         CALL FINDZWAT(IXRT,JXRT,NSOIL,SMCRT,SMCMAXRT,SMCREFRT, &
                             SMCWLTRT,ZSOIL,SATLYRCHK,ZWATTABLRT, &
                             CWATAVAIL,SLDPTH)
#ifdef MPP_LAND
        call MPP_LAND_COM_REAL(ZWATTABLRT,IXRT,JXRT,99)
        call MPP_LAND_COM_REAL(CWATAVAIL,IXRT,JXRT,99)
        call MPP_LAND_COM_INTEGER(SATLYRCHK,IXRT,JXRT,99)
#endif


!DJG Second, Call subsurface routing routine...
#ifdef HYDRO_D
	print *, "Beginning SUB_routing..."
        print *, "Routing method is ",rt_option, " direction."
#endif

!!!! Find saturated layer depth...
! Loop through domain to determine sat. layers and assign wat tbl depth...
!    and water available for subsfc routing (CWATAVAIL)...
! This subroutine returns: ZWATTABLRT, CWATAVAIL and SATLYRCHK


    CALL SUBSFC_RTNG(dist,ZWATTABLRT,QSUBRT,SOXRT,SOYRT,  &
          LKSATRT,SOLDEPRT,QSUBBDRYRT,QSUBBDRYTRT,NSOIL,SMCRT,     &
          INFXSUBRT,SMCMAXRT,SMCREFRT,ZSOIL,IXRT,JXRT,DT,SMCWLTRT,SO8RT,    &
          SO8RT_D, rt_option,SLDPTH,SUB_RESID,CWATAVAIL,SATLYRCHK)

#ifdef HYDRO_D
    print *, "SUBROUTE routing called and returned..."
#endif

    end subroutine subsurfaceRouting 

   
       subroutine OverlandRouting_drv(did)
             use module_RT_data, only: rt_domain
             use module_namelist, only: nlst_rt
             implicit none
             integer :: did
             if(nlst_rt(did)%OVRTSWCRT .eq. 1) then
                 call OverlandRouting (nlst_rt(did)%DT, nlst_rt(did)%DTRT_TER, nlst_rt(did)%rt_option, &
                          rt_domain(did)%ixrt, rt_domain(did)%jxrt,rt_domain(did)%LAKE_MSKRT, &
                          rt_domain(did)%INFXSUBRT, rt_domain(did)%RETDEPRT,rt_domain(did)%OVROUGHRT, &
                          rt_domain(did)%SOXRT, rt_domain(did)%SOYRT, rt_domain(did)%SFCHEADSUBRT,  &
                          rt_domain(did)%DHRT, rt_domain(did)%CH_NETRT, rt_domain(did)%QSTRMVOLRT, &
                          rt_domain(did)%LAKE_INFLORT,rt_domain(did)%QBDRYRT, &
                          rt_domain(did)%QSTRMVOLTRT,rt_domain(did)%QBDRYTRT, rt_domain(did)%LAKE_INFLOTRT,&
                          rt_domain(did)%q_sfcflx_x,rt_domain(did)%q_sfcflx_y, &
                          rt_domain(did)%dist, rt_domain(did)%SO8RT, rt_domain(did)%SO8RT_D , &
                          rt_domain(did)%SMCTOT2,rt_domain(did)%suminfxs1,rt_domain(did)%suminfxsrt, &
                          rt_domain(did)%smctot1,rt_domain(did)%dsmctot )
! ADCHANGE: If overland routing is called, INFXSUBRT is moved to SFCHEADSUBRT, so 
!           zeroing out just in case
             rt_domain(did)%INFXSUBRT = 0.0
             endif
       end subroutine OverlandRouting_drv



       subroutine OverlandRouting (DT, DTRT_TER, rt_option, ixrt, jxrt,LAKE_MSKRT, &
                  INFXSUBRT, RETDEPRT,OVROUGHRT,SOXRT, SOYRT, SFCHEADSUBRT,DHRT, &
                  CH_NETRT, QSTRMVOLRT,LAKE_INFLORT,QBDRYRT, &
                  QSTRMVOLTRT,QBDRYTRT, LAKE_INFLOTRT, q_sfcflx_x,q_sfcflx_y, &
                  dist, SO8RT, SO8RT_D, &
                  SMCTOT2,suminfxs1,suminfxsrt,smctot1,dsmctot )
#ifdef MPP_LAND
        use module_mpp_land, only:  mpp_land_max_int1,  sum_real1, my_id, io_id, numprocs
#endif
       implicit none

       REAL, INTENT(IN) :: DT, DTRT_TER
       integer, INTENT(IN) :: ixrt, jxrt, rt_option
       INTEGER, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: LAKE_MSKRT

       REAL, INTENT(IN), DIMENSION(IXRT,JXRT)   :: INFXSUBRT,  &
                 RETDEPRT,OVROUGHRT,SOXRT, SOYRT
       REAL, INTENT(OUT), DIMENSION(IXRT,JXRT) :: SFCHEADSUBRT,DHRT
       INTEGER, INTENT(IN), DIMENSION(IXRT,JXRT) :: CH_NETRT
       REAL, INTENT(INOUT), DIMENSION(IXRT,JXRT) :: QSTRMVOLRT,LAKE_INFLORT,QBDRYRT, &
                QSTRMVOLTRT,QBDRYTRT, LAKE_INFLOTRT, q_sfcflx_x,q_sfcflx_y

       REAL, INTENT(IN), DIMENSION(IXRT,JXRT,9):: dist
       REAL, INTENT(IN), DIMENSION(IXRT,JXRT,8)  :: SO8RT
       INTEGER SO8RT_D(IXRT,JXRT,3)

       integer  :: i,j
      

       real            :: smctot2,smctot1,dsmctot
       real            :: suminfxsrt,suminfxs1
! local variable
       real            :: chan_in1,chan_in2
       real            :: lake_in1,lake_in2
       real            :: qbdry1,qbdry2
       integer :: sfcrt_flag



!DJG Third, Call Overland Flow Routing Routine...
#ifdef HYDRO_D
	print *, "Beginning OV_routing..."
        print *, "Routing method is ",rt_option, " direction."
#endif

!DJG debug...OV Routing...
	suminfxs1=0.
        chan_in1=0.
        lake_in1=0.
        qbdry1=0.
        do i=1,IXRT
         do j=1,JXRT
            suminfxs1=suminfxs1+INFXSUBRT(I,J)/float(IXRT*JXRT)
            chan_in1=chan_in1+QSTRMVOLRT(I,J)/float(IXRT*JXRT)
            lake_in1=lake_in1+LAKE_INFLORT(I,J)/float(IXRT*JXRT)
            qbdry1=qbdry1+QBDRYRT(I,J)/float(IXRT*JXRT)
         end do
        end do

#ifdef MPP_LAND
! not tested
        CALL sum_real1(suminfxs1)
        CALL sum_real1(chan_in1)
        CALL sum_real1(lake_in1)
        CALL sum_real1(qbdry1)
        suminfxs1 = suminfxs1/float(numprocs)
        chan_in1 = chan_in1/float(numprocs)
        lake_in1 = lake_in1/float(numprocs)
        qbdry1 = qbdry1/float(numprocs)
#endif


!DJG.7.20.2007 - Global check for infxs>retdep & skip if not...(set sfcrt_flag)
!DJG.7.20.2007 - this check will skip ov rtng when no flow is present...
        
        sfcrt_flag = 0
        
        do j=1,jxrt
          do i=1,ixrt
            if(INFXSUBRT(i,j).gt.RETDEPRT(i,j)) then
              sfcrt_flag = 1
              exit
            end if
          end do
          if(sfcrt_flag.eq.1) exit
        end do   

#ifdef MPP_LAND
       call mpp_land_max_int1(sfcrt_flag)            
#endif
!DJG.7.20.2007 - Global check for infxs>retdep & skip if not...(IF)

    if (sfcrt_flag.eq.1) then  !If/then for sfc_rt check...
#ifdef HYDRO_D
      write(6,*) "calling OV_RTNG "
#endif
      CALL OV_RTNG(DT,DTRT_TER,IXRT,JXRT,INFXSUBRT,SFCHEADSUBRT,DHRT,      &
        CH_NETRT,RETDEPRT,OVROUGHRT,QSTRMVOLRT,QBDRYRT,              &
        QSTRMVOLTRT,QBDRYTRT,SOXRT,SOYRT,dist,                       &
        LAKE_MSKRT,LAKE_INFLORT,LAKE_INFLOTRT,SO8RT,SO8RT_D,rt_option,&
        q_sfcflx_x,q_sfcflx_y) 
    else
      SFCHEADSUBRT = INFXSUBRT
#ifdef HYDRO_D
      print *, "No water to route overland..."
#endif
    end if  !Endif for sfc_rt check...

!DJG.7.20.2007 - Global check for infxs>retdep & skip if not...(ENDIF)

#ifdef HYDRO_D
    print *, "OV routing called and returned..."
#endif

!DJG Debug...OV Routing...
	suminfxsrt=0.
        chan_in2=0.
        lake_in2=0.
        qbdry2=0.
        do i=1,IXRT
         do j=1,JXRT
            suminfxsrt=suminfxsrt+SFCHEADSUBRT(I,J)/float(IXRT*JXRT)
            chan_in2=chan_in2+QSTRMVOLRT(I,J)/float(IXRT*JXRT)
            lake_in2=lake_in2+LAKE_INFLORT(I,J)/float(IXRT*JXRT)
            qbdry2=qbdry2+QBDRYRT(I,J)/float(IXRT*JXRT)
         end do
        end do
#ifdef MPP_LAND
! not tested
        CALL sum_real1(suminfxsrt)
        CALL sum_real1(chan_in2)
        CALL sum_real1(lake_in2)
        CALL sum_real1(qbdry2)
        suminfxsrt = suminfxsrt/float(numprocs)
        chan_in2 = chan_in2/float(numprocs)
        lake_in2 = lake_in2/float(numprocs)
        qbdry2 = qbdry2/float(numprocs)
#endif

#ifdef HYDRO_D
#ifdef MPP_LAND   
       if(my_id .eq. IO_id) then
#endif
	print *, "OV Routing Mass Bal: "
        print *, "WB_OV!InfxsDiff", suminfxsrt-suminfxs1
        print *, "WB_OV!Infxs1", suminfxs1
        print *, "WB_OV!Infxs2", suminfxsrt
        print *, "WB_OV!ChaninDiff", chan_in2-chan_in1
        print *, "WB_OV!Chanin1", chan_in1
        print *, "WB_OV!Chanin2", chan_in2
        print *, "WB_OV!LakeinDiff", lake_in2-lake_in1
        print *, "WB_OV!Lakein1", lake_in1
        print *, "WB_OV!Lakein2", lake_in2
        print *, "WB_OV!QbdryDiff", qbdry2-qbdry1
        print *, "WB_OV!Qbdry1", qbdry1
        print *, "WB_OV!Qbdry2", qbdry2
        print *, "WB_OV!Residual", (suminfxs1-suminfxsrt)-(chan_in2-chan_in1) &
                      -(lake_in2-lake_in1)-(qbdry2-qbdry1)
#ifdef MPP_LAND
       endif
#endif
#endif


       end subroutine OverlandRouting


      subroutine time_seconds(i3)
          integer time_array(8)
          real*8 i3
          call date_and_time(values=time_array)
          i3 = time_array(4)*24*3600+time_array(5) * 3600 + time_array(6) * 60 + &
                time_array(7) + 0.001 * time_array(8)
          return
      end subroutine time_seconds

