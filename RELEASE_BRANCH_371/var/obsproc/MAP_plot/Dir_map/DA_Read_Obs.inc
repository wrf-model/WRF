SUBROUTINE DA_Read_Obs (iunit)

!------------------------------------------------------------------------------!
! Read the header of a MM5 3D-VAR 2.0 GTS observation file
!------------------------------------------------------------------------------!

   implicit none

   INTEGER,        INTENT (in)  :: iunit

   CHARACTER (LEN =  10)        :: fmt_name

   CHARACTER (LEN =  80)        :: char_ned
   CHARACTER (LEN =  66)        :: fmt_info,&
                                   fmt_srfc, & 
                                   fmt_each
   INTEGER                      :: i, ii, iost, nlevels, fm

   INTEGER                      :: nsynops, nmetars, nships,  nsounds, naireps,&
                                   npilots, nsatems, nsatobs, ngpspws, nssmt1s,&
                                   nssmt2s, nssmis,  ntovs,  nothers , namdars,&
                                   nqscats, nprofls, nbuoyss, ngpsref, &
                                   nboguss, nairsret
                 

   INTEGER                      :: ntotals
   LOGICAL                      :: connected
   TYPE (platform_type)         :: platform
   REAL                         :: missing_flag

!------------------------------------------------------------------------------!

   WRITE (0,'(A)')  &
      '------------------------------------------------------------------------------'

! 1. OPEN FILE
!    ---------

   INQUIRE ( UNIT = iunit, OPENED = connected )

   IF ( .NOT. connected ) THEN
      OPEN ( UNIT   = iunit,      &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,        &
           STATUS = 'OLD')
      IF (iost /= 0) THEN
          WRITE (0,'(/A,I4,A/)') &
             ' ERROR: INPUT FILE UNIT:', iunit, ' NOT FOUND.'
               STOP ' in DA_Read_Obs_Info_SSMI.inc'
      ENDIF
   ENDIF

   REWIND (UNIT = iunit)


! 2.  READ HEADER
! ===============

      ii = 0
      ntotals = 0; 
      nsynops = 0; nmetars = 0; nships  = 0; nsounds = 0; naireps = 0;
      npilots = 0; nsatems = 0; nsatobs = 0; ngpspws = 0; nssmt1s = 0; 
      nssmt2s = 0; nssmis  = 0; ntovs   = 0; nothers = 0; namdars = 0;
      nqscats = 0; nprofls = 0; nbuoyss = 0; ngpsref = 0; nboguss = 0
      nairsret = 0

header:&
      DO

! 2.1 READ ONE HEADER LINE
!     --------------------

      READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

      IF (iost /= 0) THEN
          WRITE (0,'(/A,i3/)') ' ERROR READING FILE UNIT:', iunit
          STOP         ' in DA_Read_Obs_Info_SSMI.inc' 
      ENDIF

! 2.2 LIMIT TO 32 LINES
!     -----------------

      ii = ii+1

      IF (ii > 32) EXIT header

! 2.4 GET NUMBER OF REPORTS
!     ---------------------

line:&
      DO i = 0, 80-15

      SELECT CASE (char_ned (I+1:I+7) )

      CASE ('SYNOP =','synop ='); 
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nsynops
      CASE ('METAR =','metar ='); 
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nmetars
      CASE ('SHIP  =','ship  ='); 
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nships
      CASE ('TEMP  =','temp  =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nsounds
      CASE ('AIREP =','airep =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) naireps
      CASE ('PILOT =','pilot =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) npilots
      CASE ('AMDAR =','amdar =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) namdars
      CASE ('QSCAT =','qscat =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nqscats
      CASE ('PROFL =','profl =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nprofls
      CASE ('BUOY  =','buoy  =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nbuoyss
      CASE ('BOGUS =','bogus =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nboguss
      CASE ('SATEM =','satem =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nsatems
      CASE ('SATOB =','satob =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nsatobs
      CASE ('GPSPW =','gpspw =','GPSZD');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ngpspws
      CASE ('GPSRF =','gpsrf =','GPSRF');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ngpsref
      CASE ('SSMT1 =','ssmt1 =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nssmt1s
      CASE ('SSMT2 =','ssmt2 =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nssmt2s
      CASE ('SSMI  =','ssmi  =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nssmis
      CASE ('TOVS  =','tovs  =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ntovs
      CASE ('AIRSR =','airsr =');
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nairsret
      CASE ('OTHER =','other ='); 
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) nothers
      CASE ('TOTAL =','total =') ;
             READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ntotals

      CASE ('MISS. =','miss. ='); 
             READ (char_ned (I+8:I+15),'(F8.0)') missing_flag

      CASE ('INFO_FM','info_fm'); 
             READ (char_ned (I+11:),'(A)')  fmt_info

      CASE ('SRFC_FM','srfc_fm'); 
             READ (char_ned (I+11:),'(A)')  fmt_srfc

      CASE ('EACH_FM','each_fm'); 
             READ (char_ned (I+11:),'(A)')  fmt_each

      CASE ('#------'); 
            EXIT header

      END SELECT

      CYCLE line

      ENDDO line

      ENDDO header

      IF (missing_flag >= 0) THEN
          ob%missing = -888888.00
      ELSE
          ob%missing = missing_flag
      ENDIF

!     CLOSE (iunit)

! 8. PRINT OUT
! ============

   ntotals = MAX (ntotals, &
                  + nsynops +  nmetars +  nships  +  nsounds +  naireps &
                  + npilots +  nsatems +  nsatobs +  ngpspws +  nssmt1s &
                  + nssmt2s +  nssmis +   ntovs   +  namdars +  nothers &
                  + nqscats +  nprofls +  nbuoyss +  nboguss +  nairsret)

   WRITE (0,'(A)') ' '
   WRITE (0,'(A,I6,A)') ' Expect: ',nsynops,' SYNOP reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nmetars,' METAR reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nships ,' SHIP  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsounds,' TEMP  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',naireps,' AIREP reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',npilots,' PILOT reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',namdars,' AMDAR reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsatems,' SATEM reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsatobs,' SATOB reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',ngpspws,' GPSPW/GPSZD reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',ngpsref,' GPSRF reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nssmt1s,' SSMT1 reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nssmt2s,' SSMT2 reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nssmis, ' SSMTI reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',ntovs , ' TOVS  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nairsret, ' AIRSRET reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nqscats,' QSCAT reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nprofls,' PROFL reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nbuoyss,' BUOY  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nboguss,' BOGUS reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nothers,' OTHER reports,'
   WRITE (0, '(A)')
   WRITE (0,'(A,I6,A)') ' Total:  ',max (ntotals,nssmis),' reports.'
   WRITE (0,'(A)')
   WRITE (0,'(A,F8.0)') ' Missing value flag:  ', missing_flag
   WRITE (0,'(A)')

!  WRITE (0,'(2A)') 'fmt_info = ',TRIM (fmt_info)
!  WRITE (0,'(2A)') 'fmt_srfc = ',TRIM (fmt_srfc)
!  WRITE (0,'(2A)') 'fmt_each = ',TRIM (fmt_each)

!  SKIP UNITS
!  ----------

   ob % total_obs = 0
   ob % num_sound = 0
   ob % num_synop = 0
   ob % num_pilot = 0
   ob % num_amdar = 0
   ob % num_satem = 0
   ob % num_airep = 0
   ob % num_metar = 0
   ob % num_ships = 0
   ob % num_gpspw = 0
   ob % num_ssmt1 = 0
   ob % num_ssmt2 = 0
   ob % num_tovs  = 0
   ob % num_ssmi  = 0
   ob % num_qscat = 0
   ob % num_profl = 0
   ob % num_buoys = 0
   ob % num_bogus = 0
   ob % num_airsret = 0

!  LOOP OVER RECORDS
!  -----------------

   reports: &
   DO
!     READ STATION GENERAL INFO
!     =============================

      READ (UNIT = iunit, FMT = fmt_info, IOSTAT = iost) &
            platform % info % platform,  &
            platform % info % date_char, &
            platform % info % name,      &
            platform % info % levels,    &
            platform % info % lat,       &
            platform % info % lon,       &
            platform % info % elv,       &
            platform % info % id

      IF (iost /= 0) THEN
         WRITE (0,'(/,A,I3,/)') ' END OF UNIT: ',iunit
         WRITE (0,'(A,I3)')     ' IOSTAT == ',iost
         EXIT reports
      ENDIF

      read(platform % info % platform (4:6), '(I3)') fm

      IF ((fm /= 125) .AND. (fm /= 126)) THEN

!     READ MODEL LOCATION
!     ====================

      READ (UNIT = iunit, FMT = fmt_srfc, IOSTAT=iost)        &
            platform % loc % slp % data,  &
            platform % loc % slp % qc,    &
            platform % loc % slp % error, &
            platform % loc % pw  % data,  &
            platform % loc % pw  % qc,    &
            platform % loc % pw  % error

      ob % total_obs = ob % total_obs + 1

      ELSE IF (fm == 125)  THEN

      READ (UNIT = iunit, FMT = fmt_srfc, IOSTAT=iost)        &
            platform % loc % Speed % data,  &
            platform % loc % Speed % qc,    &
            platform % loc % Speed % error, &
            platform % loc % tpw  % data,   &
            platform % loc % tpw  % qc,     &
            platform % loc % tpw  % error

      ob % total_obs = ob % total_obs + 1

      ELSE IF (fm == 126)  THEN

      READ (UNIT = iunit, FMT = fmt_srfc, IOSTAT=iost)        &
            platform % loc % tb19v % data,  &
            platform % loc % tb19v % qc,    &
            platform % loc % tb19v % error, &
            platform % loc % tb19h % data,  &
            platform % loc % tb19h % qc,    &
            platform % loc % tb19h % error, &
            platform % loc % tb22v % data,  &
            platform % loc % tb22v % qc,    &
            platform % loc % tb22v % error, &
            platform % loc % tb37v % data,  &
            platform % loc % tb37v % qc,    &
            platform % loc % tb37v % error, &
            platform % loc % tb37h % data,  &
            platform % loc % tb37h % qc,    &
            platform % loc % tb37h % error, &
            platform % loc % tb85v % data,  &
            platform % loc % tb85v % qc,    &
            platform % loc % tb85v % error, &
            platform % loc % tb85h % data,  &
            platform % loc % tb85h % qc,    &
            platform % loc % tb85h % error

      ob % total_obs = ob % total_obs + 1

        ENDIF


!     READ EACH LEVELS
!     ----------------

      DO ii = 1, platform % info % levels
         i = ii
         if( i > max_levels ) i = max_levels

         READ (UNIT = iunit, FMT = fmt_each, IOSTAT=iost)    &
             platform % each (i) % pressure    % data, &
             platform % each (i) % pressure    % qc,   &
             platform % each (i) % pressure    % error,&
             platform % each (i) % speed       % data, &
             platform % each (i) % speed       % qc,   &
             platform % each (i) % speed       % error,&
             platform % each (i) % direction   % data, &
             platform % each (i) % direction   % qc,   &
             platform % each (i) % direction   % error,&
             platform % each (i) % height      % data, &
             platform % each (i) % height      % qc,   &
             platform % each (i) % height      % error,&
             platform % each (i) % temperature % data, &
             platform % each (i) % temperature % qc,   &
             platform % each (i) % temperature % error,&
             platform % each (i) % dew_point   % data, &
             platform % each (i) % dew_point   % qc,   &
             platform % each (i) % dew_point   % error,&
             platform % each (i) % rh          % data, &
             platform % each (i) % rh          % qc,   &
             platform % each (i) % rh          % error

      ENDDO
   
      SELECT CASE ( fm )

!     if(index(platform % info % source, 'SYNOP') > 0) then

         CASE (12) ;

         if(ob % num_synop >= max_synop_input) cycle reports

         ob % num_synop = ob % num_synop + 1

         ob % synop ( ob % num_synop ) % info = platform % info
         ob % synop ( ob % num_synop ) % loc  = platform % loc
         ob % synop ( ob % num_synop ) % each = platform % each(1)

!     else if(index(platform % info % source, 'SHIP') > 0) then

         CASE (13)    ;

         if(ob % num_ships >= max_ships_input) cycle reports

         ob % num_ships  = ob % num_ships  + 1

         ob % ships  ( ob % num_ships  ) % info = platform % info
         ob % ships  ( ob % num_ships  ) % loc  = platform % loc
         ob % ships  ( ob % num_ships  ) % each = platform % each(1)

!     else if(index(platform % info % source, 'METAR') > 0) then

         CASE (15,16) ;

         if(ob % num_metar >= max_metar_input) cycle reports

         ob % num_metar = ob % num_metar + 1

         ob % metar ( ob % num_metar ) % info = platform % info
         ob % metar ( ob % num_metar ) % loc  = platform % loc
         ob % metar ( ob % num_metar ) % each = platform % each(1)

!     else if(index(platform % info % source, 'BUOY ') > 0) then

         CASE (18,19) ;

         if(ob % num_buoys >= max_buoys_input) cycle reports

         ob % num_buoys = ob % num_buoys + 1

         ob % buoys ( ob % num_buoys ) % info = platform % info
         ob % buoys ( ob % num_buoys ) % loc  = platform % loc
         ob % buoys ( ob % num_buoys ) % each = platform % each(1)

!     else if(index(platform % info % source, 'PILOT') > 0) then

         CASE (32:34) ;

         if(ob % num_pilot >= max_pilot_input) cycle reports

         ob % num_pilot = ob % num_pilot + 1

         ob % pilot ( ob % num_pilot ) % info = platform % info
         ob % pilot ( ob % num_pilot ) % loc  = platform % loc
         ob % pilot ( ob % num_pilot ) % each = platform % each(1)

!     else if(index(platform % info % source, 'PILOT') > 0) then

         CASE (132) ;

         if(ob % num_profl >= max_profl_input) cycle reports

         ob % num_profl = ob % num_profl + 1

         ob % profl ( ob % num_profl ) % info = platform % info
         ob % profl ( ob % num_profl ) % loc  = platform % loc
         ob % profl ( ob % num_profl ) % each = platform % each(1)

!     else if(index(platform % info % source, 'AMDAR') > 0) then

         CASE (42) ;

         if(ob % num_amdar >= max_amdar_input) cycle reports

         ob % num_amdar = ob % num_amdar + 1

         ob % amdar ( ob % num_amdar ) % info = platform % info
         ob % amdar ( ob % num_amdar ) % loc  = platform % loc
         ob % amdar ( ob % num_amdar ) % each = platform % each(1)

!     else if(index(platform % info % source, 'FM-35 TEMP') > 0) then

         CASE (35:38) ;

         if(ob % num_sound >= max_sound_input) cycle reports

         ob % num_sound = ob % num_sound + 1

         if(ob % num_sound > max_sound) then
            ob % num_sound = max_sound

            write(unit=*, fmt='(a,5(10x,a))') 'Warning:', &
                 'Radiosonde stations exceeded max_sound.', &
                 'Increase max_sound in DA_Constants/DA_Constants.f90', &
                 'recompile and rerun.', &
                 'You may continue to run, but do remember that', &
                 'there are radiosonde observations had been discarded.'
         end if

         ob % sound ( ob % num_sound ) % info = platform % info
         ob % sound ( ob % num_sound ) % loc  = platform % loc
         ob % sound ( ob % num_sound ) % each = platform % each

!     else if(index(platform % info % source, 'FM-135 BOGUS') > 0) then

         CASE (135) ;

         if(ob % num_bogus >= max_bogus_input) cycle reports

         ob % num_bogus = ob % num_bogus + 1

         if(ob % num_bogus > max_bogus) then
            ob % num_bogus = max_bogus

            write(unit=*, fmt='(a,5(10x,a))') 'Warning:', &
                 'Radiosonde stations exceeded max_bogus.', &
                 'Increase max_bogus in DA_Constants/DA_Constants.f90', &
                 'recompile and rerun.', &
                 'You may continue to run, but do remember that', &
                 'there are radiosonde observations had been discarded.'
         end if

         ob % bogus ( ob % num_bogus ) % info = platform % info
         ob % bogus ( ob % num_bogus ) % loc  = platform % loc
         ob % bogus ( ob % num_bogus ) % each = platform % each

!     else if(index(platform % info % source, 'SATEM') > 0) then

         CASE (86)    ;

         if(ob % num_satem >= max_satem_input) cycle reports

         ob % num_satem = ob % num_satem + 1

         ob % satem ( ob % num_satem ) % info = platform % info
         ob % satem ( ob % num_satem ) % loc  = platform % loc
         ob % satem ( ob % num_satem ) % each = platform % each(1)

!     else if(index(platform % info % source, 'SATOB') > 0) then

         CASE (88)    ;

         if(ob % num_satob >= max_satob_input) cycle reports

         ob % num_satob = ob % num_satob + 1

         ob % satob ( ob % num_satob ) % info = platform % info
         ob % satob ( ob % num_satob ) % loc  = platform % loc
         ob % satob ( ob % num_satob ) % each = platform % each(1)

!     else if(index(platform % info % source, 'AIREP') > 0) then

         CASE (96:97) ;

         if(ob % num_airep >= max_airep_input) cycle reports

         ob % num_airep = ob % num_airep + 1

         ob % airep ( ob % num_airep ) % info = platform % info
         ob % airep ( ob % num_airep ) % loc  = platform % loc
         ob % airep ( ob % num_airep ) % each = platform % each

!     else if(index(platform % info % source, 'GPSPW') > 0) then

         CASE (111,114) ;
         
         if(ob % num_gpspw >= max_gpspw_input) cycle reports

         ob % num_gpspw = ob % num_gpspw + 1

         ob % gpspw ( ob % num_gpspw ) % info = platform % info
         ob % gpspw ( ob % num_gpspw ) % loc  = platform % loc

!     else if(index(platform % info % source, 'GPSRF') > 0) then

         CASE (116)    ;

         if(ob % num_gpsref >= max_gpsref_input) cycle reports

         ob % num_gpsref = ob % num_gpsref + 1

         ob % gpsref ( ob % num_gpsref ) % info = platform % info
         ob % gpsref ( ob % num_gpsref ) % loc  = platform % loc
         ob % gpsref ( ob % num_gpsref ) % each = platform % each

!     else if(index(platform % info % source, 'SSMT1') > 0) then

         CASE (121) ;
         
         if(ob % num_ssmt1 >= max_ssmt1_input) cycle reports

         ob % num_ssmt1 = ob % num_ssmt1 + 1

         ob % ssmt1 ( ob % num_ssmt1 ) % info = platform % info
         ob % ssmt1 ( ob % num_ssmt1 ) % loc  = platform % loc

!     else if(index(platform % info % source, 'SSMT2') > 0) then

         CASE (122) ;
         
         if(ob % num_ssmt2 >= max_ssmt2_input) cycle reports

         ob % num_ssmt2 = ob % num_ssmt2 + 1

         ob % ssmt2 ( ob % num_ssmt2 ) % info = platform % info
         ob % ssmt2 ( ob % num_ssmt2 ) % loc  = platform % loc

!     else if(index(platform % info % source, 'SSMI') > 0) then

         CASE (125,126) ;
         
         if(ob % num_ssmi >= max_ssmi_input) cycle reports

         ob % num_ssmi = ob % num_ssmi + 1

         ob % ssmi ( ob % num_ssmi ) % info = platform % info
         ob % ssmi ( ob % num_ssmi ) % loc  = platform % loc

!     else if(index(platform % info % source, 'TOVS') > 0) then

         CASE (131) ;
         
         if(ob % num_tovs >= max_tovs_input) cycle reports

         ob % num_tovs = ob % num_tovs + 1

         ob % tovs ( ob % num_tovs ) % info = platform % info
         ob % tovs ( ob % num_tovs ) % loc  = platform % loc
         ob % tovs ( ob % num_tovs ) % each = platform % each

!     else if(index(platform % info % source, 'AIRSRET') > 0) then

         CASE (133) ;
         
         if(ob % num_airsret >= max_airsret_input) cycle reports

         ob % num_airsret = ob % num_airsret + 1

         ob % airsret ( ob % num_airsret ) % info = platform % info
         ob % airsret ( ob % num_airsret ) % loc  = platform % loc
         ob % airsret ( ob % num_airsret ) % each = platform % each

!     else if(index(platform % info % source, 'QSCAT') > 0) then

         CASE (281) ;
         
         if(ob % num_qscat >= max_qscat_input) cycle reports

         ob % num_qscat = ob % num_qscat + 1

         ob % qscat ( ob % num_qscat ) % info = platform % info
         ob % qscat ( ob % num_qscat ) % loc  = platform % loc
         ob % qscat ( ob % num_qscat ) % each = platform % each(1)

!     else

         CASE DEFAULT;

         write(0, '(a)') 'Warning: unsaved obs found:'

         write(0, '(2a)') &
!             'platform % info % source=', platform % info % source
              'platform % info % platform=', platform % info % platform
 
         write(0, '(a, i3)') &
              'platform % info % levels=', platform % info % levels
!         stop
!     end if

      END SELECT

   ENDDO reports

   CLOSE (iunit)

!  PRINT OUT
!  =============
 
   write(unit=*, fmt='(a)')  ' '

   write(unit=*, fmt='(5x,a,i6,a)') &
        'Read:  ', ob % num_sound, ' SOUND reports,', &
        'Read:  ', ob % num_synop, ' SYNOP reports,', &
        'Read:  ', ob % num_pilot, ' PILOT reports,', &
        'Read:  ', ob % num_amdar, ' AMDAR reports,', &
        'Read:  ', ob % num_satem, ' SATEM reports,', &
        'Read:  ', ob % num_satob, ' SATOB reports,', &
        'Read:  ', ob % num_airep, ' AIREP reports,', &
        'Read:  ', ob % num_gpspw, ' GPSPW/GPSZD reports,', &
        'Read:  ', ob % num_gpsref,' GPSRF reports,', &
        'Read:  ', ob % num_metar, ' METAR reports,', &
        'Read:  ', ob % num_ships ,' SHIP  reports,', &
        'Read:  ', ob % num_ssmi,  ' SSMI  reports,', &
        'Read:  ', ob % num_qscat, ' QSCAT reports,', &
        'Read:  ', ob % num_profl, ' PROFL reports,', &
        'Read:  ', ob % num_buoys, ' BUOY  reports,', &
        'Read:  ', ob % num_bogus, ' BOGUS reports,', &
        'Read:  ', ob % num_tovs , ' TOVS  reports,', &
        'Read:  ', ob % num_airsret, ' AIRSRET reports,', &
        'Read:  ', ob % total_obs, ' Total Observations.', &
     'There are ', ob % total_obs - ob % num_sound - ob % num_synop &
                 - ob % num_pilot - ob % num_satem &
                 - ob % num_satob - ob % num_airep &
                 - ob % num_metar - ob % num_ships &
                 - ob % num_gpspw - ob % num_ssmi  &
                 - ob % num_qscat - ob % num_profl &
                 - ob % num_buoys - ob % num_gpsref &
                 - ob % num_bogus - ob % num_tovs - ob % num_airsret, &
                ',  Observations unsaved.'


!  ob % sound ( ob % num_sound ) % info % levels = 1
!  ob % sound ( ob % num_sound ) % loc  = platform % loc
!  ob % sound ( ob % num_sound ) % each(1) = &
!  ob % sound ( ob % num_sound ) % each(5)

!  ob % sound ( ob % num_sound ) % each(1) % u % inv = ob % missing
!  ob % sound ( ob % num_sound ) % each(1) % v % inv = ob % missing
!  ob % sound ( ob % num_sound ) % each(1) % t % inv = ob % missing
!  ob % sound ( ob % num_sound ) % each(1) % q % inv = ob % missing

END SUBROUTINE DA_Read_Obs

