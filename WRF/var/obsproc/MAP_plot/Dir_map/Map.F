      PROGRAM MAPBKG
C
      USE DA_Constants
      USE DA_Define_Structures
      USE module_obs

      integer                         :: number
      real, allocatable, dimension(:) :: ylat, ylon
      character (len=5), allocatable  :: IDN(:)
!
      PARAMETER (IX = 74, JX = 92, KX = 31)
#     include "param1.incl"
C
      COMMON /HEADER/ MIF, MRF, MIFC, MRFC  
      INTEGER MIF(1000,20)  
      REAL MRF(1000,20)
      CHARACTER*80 MIFC(1000,20),MRFC(1000,20) 

      INTEGER*8  MIF_8(1000,20)
      REAL*8     MRF_8(1000,20) 
C                    
      CHARACTER*8 ID  
C
      parameter (LVL=100, nstn=2000)
      parameter (NUNIT1=10, NUNIT2 = 11, NUNIT = 14, spval=-999.)
C
      Dimension CIX(nstn), CJX(nstn), DIX(nstn), DJX(nstn)
      DIMENSION   wlON(Nstn),wlAT(Nstn), SLP(Nstn),
     >           Tmp_sfc(Nstn), RH_sfc(Nstn),
     >           Tmp_p(Nstn,Lvl), RH_p(Nstn,Lvl),
     >           Uu_p (Nstn,Lvl), Vv_p(Nstn,Lvl),
     >           Data(Nstn)
      DIMENSION NUM(nstn), LAT(NSTN), LON(nstn), Press(Lvl)
      CHARACTER STNAME(Nstn)*6, TITLE*80, VNAME*5, PLVL*26,
     >          SYMBOL*1, NAME*8, CDFILE*32
C
      DIMENSION PSOBS(Nstn), PPOBS(Nstn,KX), PPK(KX),
     >          RH(Lvl+1), TMP(Lvl+1), PRS(Lvl+1),SIG(KX),
     >          TTOBS(KX,Nstn), RHOBS(KX,Nstn), FSG(KX),
     >          QQOBS(KX,Nstn), RHO  (KX,Nstn)
C
       dimension ff(jjmax*iimax), aa(iimax*jjmax)
C ................................................................
      LOGICAL PPLOT(20)
      LOGICAL skewt_plot
      LOGICAL is_plot
      CHARACTER*19 time_window_min, time_window_max, time_work
C     NAMELIST /Exp_Name/MDATE
      NAMELIST /PLTOPTN/ IDD, IQC0, PPLOT
      NAMELIST /TIME_WINDOW/ time_window_min, time_window_max
      NAMELIST /SKEW_PLOTS/  skewt_plot, is_plot
C
C ........................................................................
C
      common /conre1/ IOFFP, SPVAL0
      IOFFP = 1
      SPVAL0 = spval
      skewt_plot = .false.
      is_plot = .false.
      IEXP  = 0
C
      call opngks
      call gopwk(9,1,3)
      call gsclip(0)
      CALL SETUSV('LW',2000)
c
      WRITE(0,'(//''==> STARTING TO READ OBS DATA FILE''/)')

      max_sound_input = max_sound
      max_synop_input = max_synop
      max_satob_input = max_satob
      max_airep_input = max_airep
      max_satem_input = max_satem
      max_pilot_input = max_pilot
      max_amdar_input = max_amdar
      max_metar_input = max_metar
      max_gpspw_input = max_gpspw
      max_ships_input = max_ships
      max_ssmt1_input = max_ssmt1
      max_ssmt2_input = max_ssmt2
      max_ssmi_input  = max_ssmi
      max_tovs_input  = max_tovs
      max_qscat_input = max_qscat
      max_profl_input = max_profl
      max_buoys_input = max_buoys
      max_bogus_input = max_bogus
      max_gpsref_input = max_gpsref

!     CALL DA_Setup_Obs_Structures( ob )
      CALL DA_Setup_Obs_Structures
!      allocate (ylat(1:ob % num_sound))
!      allocate (ylon(1:ob % num_sound))
!      allocate ( idn(1:ob % num_sound))
!      ylat(:) = ob % sound(:) % info % lat
!      ylon(:) = ob % sound(:) % info % lon
!       idn(:) = ob % sound(:) % info %  id
!
!      do i = 1,ob % num_sound
!        write(0,'(I3,2X,''lat,lon:'',2F8.2,2X,''stn='',A5,
!     &            2X,''xx,yy:'',2F8.2)') 
!     &        i, ob % sound(i) % info % lat,
!     &           ob % sound(i) % info % lon,
!     &           ob % sound(i) % info %  id,
!     &           ob % sound(i) %  loc %  xj % dot,
!     &           ob % sound(i) %  loc %  yi % dot
!      end do

C ==================================================================

      CALL SETUP
C
      READ(15,TIME_WINDOW)
C     PRINT*, TIME_WINDOW

      IF (LEN_TRIM (time_window_min) .LE. 10) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_min ( 1: 4), time_window_min ( 5: 6),
     >       time_window_min ( 7: 8), time_window_min ( 9:10),
     >       '00', '00'
             time_window_min = time_work
      ELSE IF (LEN_TRIM (time_window_min) .LE. 12) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_min ( 1: 4), time_window_min ( 5: 6),
     >       time_window_min ( 7: 8), time_window_min ( 9:10),
     >       time_window_min (11:12), "00"
             time_window_min = time_work
      ELSE IF (LEN_TRIM (time_window_min) .LE. 14) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_min ( 1: 4), time_window_min ( 5: 6),
     >       time_window_min ( 7: 8), time_window_min ( 9:10),
     >       time_window_min (11:12), time_window_min (13:14)
             time_window_min = time_work
      ENDIF
      IF (LEN_TRIM (time_window_max) .LE. 10) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_max ( 1: 4), time_window_max ( 5: 6),
     >       time_window_max ( 7: 8), time_window_max ( 9:10),
     >       '00', '00'
             time_window_max = time_work
      ELSE IF (LEN_TRIM (time_window_max) .LE. 12) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_max ( 1: 4), time_window_max ( 5: 6),
     >       time_window_max ( 7: 8), time_window_max ( 9:10),
     >       time_window_max (11:12), "00"
             time_window_max = time_work
      ELSE IF (LEN_TRIM (time_window_max) .LE. 14) THEN
      WRITE (time_work,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)')
     >       time_window_max ( 1: 4), time_window_max ( 5: 6),
     >       time_window_max ( 7: 8), time_window_max ( 9:10),
     >       time_window_max (11:12), time_window_max (13:14)
             time_window_max = time_work
      ENDIF

      READ(15,SKEW_PLOTS)
C     PRINT*, SKEW_PLOTS
C
C ====================================================================
      do nd = 1,maxnes

      if ( nd < maxnes ) cycle

      mmi  = nestix(nd)
      mmj  = nestjx(nd)
      ioff = ioffst*nratio(nd)
      joff = joffst*nratio(nd)
      print 3,ND,mmi,mmj,ioff,joff
 3        format(//5x,'<< Domain ',I2,'  Size: mmi=',I3,' mmj=',I3,
     >         '  ioff=',I3,' joff=',I3,' >>'/)
C
        CALL LLXY(PHIC,XLONC,X,Y,mmi,mmj,135.)
        PRINT *,'PHIC,XLONC:',PHIC,XLONC,'  X,Y:',X,Y
C
        WRITE(0,'(/''IPROJ='',A6,''  IXC='',I4,'' JXC='',I4/
     &          ''PHIC, XLONC, XN, TRUELAT1, TRUELAT2, POLE:'',
     &          6F10.5/''DSC, DSM, XIM11, XJM11:'',2F10.2,2I5/
     &          ''PSI1, C2, XCNTR, YCNTR:'',4F15.4/)')
     &          IPROJ, IXEX, JXEX, 
     &          PHIC, XLONC, XN, TRUELAT1, TRUELAT2, POLE, 
     &          DIS(1) , DIS(nd), NESTI(nd) , NESTJ(nd), 
     &          PSI1, C2, XCNTR, YCNTR

      IF (ob % num_sound .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_sound, ob % sound(:) % info % lat,
     >                                    ob % sound(:) % info % lon,
     >                                    ob % sound(:) % info %  id,
     >                     16, -4, 'SOUND', nd, 0.0, 0.0, 10, 50., 1,
     >                     ob % sound(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_bogus .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_bogus, ob % bogus(:) % info % lat,
     >                                    ob % bogus(:) % info % lon,
     >                                    ob % bogus(:) % info %  id,
     >                     16, -4, 'BOGUS', nd, 0.0, 0.0, 10, 50., 1,
     >                     ob % bogus(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_airep .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_airep, ob % airep(:) % info % lat,
     >                                    ob % airep(:) % info % lon,
     >                                    ob % airep(:) % info %  id,
     >                     16, -4, 'AIREP', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % airep(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_gpsref .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_gpsref,ob % gpsref(:) % info % lat,
     >                                    ob % gpsref(:) % info % lon,
     >                                    ob % gpsref(:) % info %  id,
     >                     16, -4, 'GPSRF', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % gpsref(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_synop .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_synop, ob % synop(:) % info % lat,
     >                                    ob % synop(:) % info % lon,
     >                                    ob % synop(:) % info %  id,
     >                     16, -2, 'SYNOP', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % synop(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_qscat .GT. 0) then
      CALL PLSTN1(mmi,mmj,ob % num_qscat, ob % qscat(:) % info % lat,
     >                                    ob % qscat(:) % info % lon,
     >                                    ob % qscat(:) % info %  id,
     >                     16, -1, 'QSCAT', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % qscat(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      CALL PLHWD(mmi,mmj, ob % qscat(:) % each % speed     % data, 
     >                    ob % qscat(:) % each % direction % data,
     >                    ob % qscat(:) % info % lon, 
     >                    ob % num_qscat, -888888.0, Nd)
      ENDIF

      IF (ob % num_profl .GT. 0) then
      CALL PLSTN1(mmi,mmj,ob % num_profl, ob % profl(:) % info % lat,
     >                                    ob % profl(:) % info % lon,
     >                                    ob % profl(:) % info %  id,
     >                     16, -3, 'PROFL', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % profl(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      CALL PLHWD(mmi,mmj, ob % profl(:) % each % speed     % data, 
     >                    ob % profl(:) % each % direction % data,
     >                    ob % profl(:) % info % lon,
     >                    ob % num_profl, -888888.0, Nd)
      ENDIF

      IF (ob % num_ships .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_ships, ob % ships(:) % info % lat,
     >                                    ob % ships(:) % info % lon,
     >                                    ob % ships(:) % info %  id,
     >                     16, -3, 'SHIPS', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % ships(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_buoys .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_buoys, ob % buoys(:) % info % lat,
     >                                    ob % buoys(:) % info % lon,
     >                                    ob % buoys(:) % info %  id,
     >                     16, -3, 'BUOYS', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % buoys(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_pilot .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_pilot, ob % pilot(:) % info % lat,
     >                                    ob % pilot(:) % info % lon,
     >                                    ob % pilot(:) % info %  id,
     >                     16, -5, 'PILOT', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % pilot(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_amdar .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_amdar, ob % amdar(:) % info % lat,
     >                                    ob % amdar(:) % info % lon,
     >                                    ob % amdar(:) % info %  id,
     >                     16, -4, 'AMDAR', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % amdar(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_satob .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_satob, ob % satob(:) % info % lat,
     >                                    ob % satob(:) % info % lon,
     >                                    ob % satob(:) % info %  id,
     >                     16, -4, 'SATOB', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % satob(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_satem .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_satem, ob % satem(:) % info % lat,
     >                                    ob % satem(:) % info % lon,
     >                                    ob % satem(:) % info %  id,
     >                     16, -4, 'SATEM', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % satem(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_metar .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_metar, ob % metar(:) % info % lat,
     >                                    ob % metar(:) % info % lon,
     >                                    ob % metar(:) % info %  id,
     >                     16, -3, 'METAR', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % metar(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_gpspw .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_gpspw, ob % gpspw(:) % info % lat,
     >                                    ob % gpspw(:) % info % lon,
     >                                    ob % gpspw(:) % info %  id,
     >                 16, -4, 'GPSPW/ZTD', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % gpspw(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_ssmt1 .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_ssmt1, ob % ssmt1(:) % info % lat,
     >                                    ob % ssmt1(:) % info % lon,
     >                                    ob % ssmt1(:) % info %  id,
     >                     16, -4, 'SSMT1', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % ssmt1(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_ssmt2 .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_ssmt2, ob % ssmt2(:) % info % lat,
     >                                    ob % ssmt2(:) % info % lon,
     >                                    ob % ssmt2(:) % info %  id,
     >                     16, -4, 'SSMT2', nd, 0.0, 0.0, 10, 50., 1,  
     >                     ob % ssmt2(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_ssmi .GT. 0) then
      if (is_plot) then
      CALL PLSTN2(mmi,mmj,ob % num_ssmi, ob % ssmi(:) % info % lat,
     >                                   ob % ssmi(:) % info % lon,
     >                                   ob % ssmi(:) % info %id,
     >                         ob % ssmi(:) % loc % tb19v % data,
     >                         ob % ssmi(:) % loc % tb19h % data,
     >                         ob % ssmi(:) % loc % tb22v % data,
     >                         ob % ssmi(:) % loc % tb37v % data,
     >                         ob % ssmi(:) % loc % tb37h % data,
     >                         ob % ssmi(:) % loc % tb85v % data,
     >                         ob % ssmi(:) % loc % tb85h % data,
     >                         ob % ssmi(:) % loc % tb19v % error,
     >                         ob % ssmi(:) % loc % tb19h % error,
     >                         ob % ssmi(:) % loc % tb22v % error,
     >                         ob % ssmi(:) % loc % tb37v % error,
     >                         ob % ssmi(:) % loc % tb37h % error,
     >                         ob % ssmi(:) % loc % tb85v % error,
     >                         ob % ssmi(:) % loc % tb85h % error,
     >                     'SSMI', nd, 0.0, 0.0, 10, 50., 1,
     >                     ob % ssmi(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)
      else
      CALL PLSTN1(mmi,mmj,ob % num_ssmi, ob % ssmi(:) % info % lat,
     >                                    ob % ssmi(:) % info % lon,
     >                                    ob % ssmi(:) % info %  id,
     >                     16, -4, 'SSMI', nd, 0.0, 0.0, 10, 50., 1,
     >                     ob % ssmi(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)
      endif
      ENDIF

      IF (ob % num_tovs .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_tovs, ob % tovs(:) % info % lat,
     >                                    ob % tovs(:) % info % lon,
     >                                    ob % tovs(:) % info %  id,
     >                     16, -4, 'TOVS', nd, 0.0, 0.0, 10, 50., 1, 
     >                     ob % tovs(:) % info % date_char(1:19), 
     >                     time_window_min, time_window_max)

      IF (ob % num_airsret .GT. 0) 
     >CALL PLSTN1(mmi,mmj,ob % num_airsret, 
     >                    ob % airsret(:) % info % lat,
     >                    ob % airsret(:) % info % lon,
     >                    ob % airsret(:) % info %  id,
     >                    16, -4, 'AIRSR', nd, 0.0, 0.0, 10, 50., 1, 
     >                    ob % airsret(:) % info % date_char(1:19), 
     >                    time_window_min, time_window_max)

c plot skewt:

      IF (skewt_plot) THEN

c      title = 'SOUND OBS'
c      call plot_skewt(title, ob % sound, ob % num_sound, iqc0, nd)

      title = 'ATOVS OBS'
      number   = ob % num_tovs
c      number   = 3
      call plot_skewt(title, ob % tovs(1:number), number, iqc0, nd)

      ENDIF

      end do
C ======================================================================
      call gclwk(9)
      call clsgks
C
      STOP 11111
     
      END
C
