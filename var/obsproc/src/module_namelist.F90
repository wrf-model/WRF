MODULE MODULE_NAMELIST 
!-----------------------------------------------------------------------------!
! Read the namelist file name "nml_filename" unit "iunit"
!
! D. GILL,         April 1998
! F. VANDENBERGHE, March 2001
!-----------------------------------------------------------------------------!

   USE MODULE_DATE
   USE MODULE_MM5
   use map_utils
 
#ifdef BKG
   USE MODULE_MAP
#endif

   IMPLICIT NONE

   CHARACTER (LEN = 80)      :: obs_gts_filename
   CHARACTER (LEN = 80)      :: obs_err_filename
   CHARACTER (LEN = 80)      :: prepbufr_output_filename
   CHARACTER (LEN = 80)      :: prepbufr_table_filename

   CHARACTER (LEN = 3) :: fg_format 
   integer             :: iix, jjx, imap_proj, i_grid, j_grid,m_expand  
   real                :: standard_lon, moad_cen_lat, cen_lat, cen_lon,&
                          dds, xxx, yyy, xxc, yyc, xlatt, xlonn
   type (proj_info)    :: map_info

#ifdef BKG
   CHARACTER (LEN = 80)      :: first_guess_file
#endif

   CHARACTER (LEN = 19)      :: time_window_min
   CHARACTER (LEN = 19)      :: time_analysis
   CHARACTER (LEN = 19)      :: time_window_max

   INTEGER                   :: max_number_of_obs,                 &
                                output_ob_format         = 2
   LOGICAL                   :: fatal_if_exceed_max_obs  = .TRUE.
  
   LOGICAL                   :: qc_test_vert_consistency = .TRUE., &
                                qc_test_convective_adj   = .TRUE., &
                                qc_test_above_lid        = .TRUE., &
                                remove_above_lid         = .TRUE., &
                                Thining_SATOB            = .TRUE., &
                                Thining_SSMI             = .TRUE., &
                                Thining_QSCAT            = .TRUE.
  
   LOGICAL                   :: print_gts_read           = .TRUE., &
                                print_gpspw_read         = .TRUE., &
                                print_recoverp           = .TRUE., &
                                print_duplicate_loc      = .TRUE., &
                                print_duplicate_time     = .TRUE., &
                                print_recoverh           = .TRUE., &
                                print_qc_vert            = .TRUE., &
                                print_qc_conv            = .TRUE., &
                                print_qc_lid             = .TRUE., &
                                print_uncomplete         = .TRUE.
#ifdef BKG
   INTEGER                   :: time_earlier, time_later

   NAMELIST /RECORD1/ obs_gts_filename, obs_err_filename, &
                      first_guess_file, fg_format
   NAMELIST /RECORD2/ time_earlier, time_later, time_analysis
#else
   NAMELIST /RECORD1/ obs_gts_filename, obs_err_filename, fg_format
   NAMELIST /RECORD2/ time_window_min,time_analysis,time_window_max
#endif
   NAMELIST /RECORD3/ max_number_of_obs, fatal_if_exceed_max_obs
   NAMELIST /RECORD4/ qc_test_vert_consistency, qc_test_convective_adj,  &
                      qc_test_above_lid, remove_above_lid,domain_check_h,&
                      Thining_SATOB, Thining_SSMI, Thining_QSCAT
   NAMELIST /RECORD5/ print_gts_read, print_gpspw_read,                 &
                      print_recoverp,                                   &
                      print_duplicate_loc, print_duplicate_time,        &
                      print_recoverh, print_qc_vert,                    &
                      print_qc_conv,  print_qc_lid,                     &
#ifdef BKG
                      user_defined_area,                                &
                      print_uncomplete 
   NAMELIST /RECORD6/ x_left,   x_right, &
                      y_bottom, y_top
#else
                      print_uncomplete 
   NAMELIST /RECORD6/ ptop, ps0, ts0, tlp, pis0, tis0,  &
                      base_pres, base_temp, base_lapse, &
                      base_strat_temp, base_tropo_pres
           
   NAMELIST /RECORD7/ iproj, phic, xlonc, truelat1, truelat2, &
                      moad_cen_lat, standard_lon
   NAMELIST /RECORD8/ idd, maxnes, nestix, nestjx, dis, numc,           &
                      nesti, nestj

#endif

   NAMELIST /RECORD9/ prepbufr_output_filename, &
                      prepbufr_table_filename, output_ob_format

   CONTAINS

   SUBROUTINE GET_NAMELIST  (nml_filename, iunit)

   CHARACTER (LEN = *)       :: nml_filename
   INTEGER                   :: iunit 
   INTEGER, DIMENSION (20)   :: nml_read_errors
   INTEGER                   :: i, iost, ita, itb
   INTEGER                   :: error_number
   LOGICAL                   :: exist, fatal
   LOGICAL                   :: good_date1, good_date2
   CHARACTER (LEN = 80)      :: error_message
   CHARACTER (LEN = 80)      :: proc_file = "PROC_NAMELIST"

   include 'missing.inc'

!-----------------------------------------------------------------------------!

   !  Opening the NAMELIST file constitutes an important step.

#ifdef BKG
   maxnes      = 10
   time_earlier=-90    ! in minutes
   time_later  = 90    ! in minutes
#endif

   iunit  = 99

   OPEN ( FILE   = nml_filename,  &
          UNIT   =  iunit,        &
          STATUS = 'OLD',         &
          ACCESS = 'SEQUENTIAL',  &
          FORM   = 'FORMATTED',   &
          ACTION = 'READ',        &
          IOSTAT = error_number )


   IF ( error_number .NE. 0 ) THEN
        error_message  = ' Error opening namelist file: '
        fatal = .TRUE.
        CALL error_handler (proc_file, error_message,  nml_filename, fatal)
   ENDIF

   !  Read namelist record

   nml_read_errors = 0

   ! default:
   fg_format         = 'MM5'
   obs_err_filename  = 'obserr.txt'

! . Initialize the new defined namelist variables (YRG 05/10/2007):
   base_pres  = missing_r
   base_temp  = missing_r
   base_lapse = missing_r
   base_strat_temp = missing_r
   base_tropo_pres = missing_r

   READ  ( UNIT = iunit , NML = record1 , IOSTAT = nml_read_errors(1) )
   WRITE ( UNIT = 0 ,     NML = record1 )
   IF    ( nml_read_errors(1) .NE. 0 ) THEN
          WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 1'
           STOP
   ENDIF

   READ  ( UNIT = iunit , NML = record2 , IOSTAT = nml_read_errors(2) )
   WRITE ( UNIT = 0 ,     NML = record2 )
   IF    ( nml_read_errors(2) .NE. 0 ) THEN
          WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 2'
           STOP
   ENDIF

   READ  ( UNIT = iunit , NML = record3 , IOSTAT = nml_read_errors(3) )
   WRITE ( UNIT = 0 ,     NML = record3 )
   IF    ( nml_read_errors(3) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 3'
           STOP
   ENDIF

   READ  ( UNIT = iunit , NML = record4 , IOSTAT = nml_read_errors(4) )
   WRITE ( UNIT = 0 ,     NML = record4 )
   IF    ( nml_read_errors(4) .NE. 0 ) THEN 
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 4'
           STOP
   ENDIF

#ifdef BKG
   user_defined_area = .false.
#endif

   READ  ( UNIT = iunit , NML = record5 , IOSTAT = nml_read_errors(5) )
   WRITE ( UNIT = 0 ,     NML = record5 )
   IF    ( nml_read_errors(5) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 5'
           STOP
   ENDIF

! Default values for tropopause (YRG, 04/12/2007):

     pis0 = 20000.0
     tis0 = 215.0
! ---------------------------------------------------------

#ifdef BKG
   if(user_defined_area) then
      x_left  = 0.0
      x_right =1.0e20
      y_bottom=0.0
      y_top   =1.0e20

      READ  ( UNIT = iunit , NML = record6 , IOSTAT = nml_read_errors(6) )
      WRITE ( UNIT = 0 ,     NML = record6 )
      IF    ( nml_read_errors(6) .NE. 0 ) THEN
              WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 6'
              STOP
      ENDIF

   end if
#else
   READ  ( UNIT = iunit , NML = record6 , IOSTAT = nml_read_errors(6) )
!
! . use the new defined namelist variables (YRG 05/10/2007):
   if (base_pres  /= missing_r)  ps0 = base_pres
   if (base_temp  /= missing_r)  ts0 = base_temp
   if (base_lapse /= missing_r)  tlp = base_lapse
   if (base_strat_temp /= missing_r)  tis0 = base_strat_temp
   if (base_tropo_pres /= missing_r)  pis0 = base_tropo_pres

   WRITE ( UNIT = 0 ,     NML = record6 )
   IF    ( nml_read_errors(6) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 6'
           STOP
   ENDIF

   READ  ( UNIT = iunit , NML = record7 , IOSTAT = nml_read_errors(7) )
   WRITE ( UNIT = 0 ,     NML = record7 )
   IF    ( nml_read_errors(7) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 7'
           STOP
   ENDIF

   READ  ( UNIT = iunit , NML = record8 , IOSTAT = nml_read_errors(8) )
   WRITE ( UNIT = 0 ,     NML = record8 )
   IF    ( nml_read_errors(8) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 8'
           STOP
   ENDIF
#endif

   prepbufr_output_filename = 'prepbufr_output_filename'
   prepbufr_table_filename = 'prepbufr_table_filename'
   READ  ( UNIT = iunit , NML = record9 , IOSTAT = nml_read_errors(9) )
   WRITE ( UNIT = 0 ,     NML = record9 )
   IF    ( nml_read_errors(9) .NE. 0 ) THEN
           WRITE ( UNIT = 0 , FMT = '(A)' ) ' Error in NAMELIST record 9'
           STOP
   ENDIF
   !   test for existence of table
   OPEN (UNIT = 10, FILE = prepbufr_table_filename, STATUS='old', IOSTAT=iost) 
   if (iost .ne. 0 .and. output_ob_format .ne. 2) then
     write(0,*) ' '
     write(0,*) 'You requested prepbufr output format, but I cannot open the prepbufr table.'
     write(0,*) 'Check that the file exists and the variables in record9 of the namelist are set properly.'
     write(0,*) 'output_ob_format = ',output_ob_format, &
                'prepbufr_table_filename = ',prepbufr_table_filename
     STOP
   endif
   CLOSE (10)

   !  Process read error

   error_number = SUM ( nml_read_errors )

   IF (error_number .NE. 0 ) THEN
        error_message  = ' Error reading namelist file: '
        fatal = .TRUE.
        CALL error_handler (proc_file, error_message,  nml_filename, fatal)
   END IF

   !  After the NAMELIST file is input, the unit needs to
   !  be properly closed.

   CLOSE ( UNIT = iunit )

#ifdef BKG
!--Get Big Record Header

   iunit  = 41

   OPEN ( FILE   = first_guess_file,  &
          UNIT   =  iunit,            &
          STATUS = 'OLD',             &
          ACCESS = 'SEQUENTIAL',      &
          FORM   = 'UNFORMATTED',     &
          ACTION = 'READ',            &
          IOSTAT = error_number )

   call get_map_params (iunit)

   CLOSE ( UNIT = iunit )
#endif

   ! Check consistency
  
   IF ((remove_above_lid) .AND. (.NOT. qc_test_above_lid)) THEN
       qc_test_above_lid = .TRUE.
   ENDIF

   !  Time window


      IF (LEN_TRIM (time_window_min) .LE. 10) THEN
      WRITE (time_window_min,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_window_min ( 1: 4), time_window_min ( 5: 6), &
             time_window_min ( 7: 8), time_window_min ( 9:10), &
             '00', '00'
      ELSE IF (LEN_TRIM (time_window_min) .LE. 14) THEN
      WRITE (time_window_min,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_window_min ( 1: 4), time_window_min ( 5: 6), &
             time_window_min ( 7: 8), time_window_min ( 9:10), &
             time_window_min (11:12), time_window_min (13:14)
      ENDIF

#ifdef BKG
      time_window_min=time_analysis
      time_window_max=time_analysis
 
      call geth_newdate (time_window_min, time_analysis(1:16), time_earlier)
      call geth_newdate (time_window_max, time_analysis(1:16), time_later)
#else
      IF (LEN_TRIM (time_analysis) .LE. 10) THEN
      WRITE (time_analysis,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_analysis ( 1: 4), time_analysis ( 5: 6), &
             time_analysis ( 7: 8), time_analysis ( 9:10), &
             '00', '00'
      ELSE IF (LEN_TRIM (time_analysis) .LE. 14) THEN
      WRITE (time_analysis,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_analysis ( 1: 4), time_analysis ( 5: 6), &
             time_analysis ( 7: 8), time_analysis ( 9:10), &
             time_analysis (11:12), time_analysis (13:14)
      ENDIF

      IF (LEN_TRIM (time_window_max) .LE. 10) THEN
      WRITE (time_window_max,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_window_max ( 1: 4), time_window_max ( 5: 6), &
             time_window_max ( 7: 8), time_window_max ( 9:10), &
             '00', '00'
      ELSE IF (LEN_TRIM (time_window_max) .LE. 14) THEN
      WRITE (time_window_max,'(A4,"-",A2,"-",A2,"_",A2,":",A2,":",A2)') &
             time_window_max ( 1: 4), time_window_max ( 5: 6), &
             time_window_max ( 7: 8), time_window_max ( 9:10), &
             time_window_max (11:12), time_window_max (13:14)
      ENDIF

#endif
 
      CALL GETH_IDTS (time_analysis, time_window_min,itb,good_date1)
      CALL GETH_IDTS (time_analysis, time_window_max,ita,good_date2)

   IF ((itb .LT. 0) .OR. (ita .GT. 0.) .OR. &
       (.NOT. good_date1) .OR. (.NOT. good_date2)) THEN 
        error_message = &
      " Error: The time window [" // TRIM (time_window_min) // ", " // &
                                     TRIM (time_window_max) // &
                               "] does not mencompass the analysis time: "
        fatal = .TRUE.
        CALL error_handler (proc_file, error_message, time_analysis, fatal)
   END IF

   if (iproj <0 .or.iproj > 3) then
      write (0,'(/a)') '*** Please check the iproj setting??? '
      write (0,'(5x,a)') 'iproj = 0 ===> PROJ_LATLON for Global domain.' 
      write (0,'(5x,a)') 'iproj = 1 ===> PROJ_LC for Lambert Congormsl.'
      write (0,'(5x,a)') 'iproj = 2 ===> PROJ_PS for Polar Stereographic.'
      write (0,'(5x,a)') 'iproj = 3 ===> PROJ_ME for Mercator'
      write (0,'(a)') '??? iproj setting outside the range......'
      STOP
   endif

   if (fg_format == 'WRF') then
      print '(/a/)','=== 3DVAR_OBSPROC is used for WRF 3DVAR ===' 

! .. Set up the map_info using the central lat/lon:

        if(iproj == 0) then
           imap_proj = PROJ_LATLON
        else if(iproj == 1) then
           imap_proj = PROJ_LC
        else if(iproj == 2) then
           imap_proj = PROJ_PS
        else if(iproj == 3) then
           imap_proj = PROJ_MERC
        endif

        cen_lat = phic
        cen_lon = xlonc
        iix =  nestix(1)
        jjx =  nestjx(1)
        dds =  dis(1)*1000.0
        xxc = real(jjx)/2.
        yyc = real(iix)/2.
        call map_set(imap_proj, cen_lat, cen_lon, xxc, yyc, dds, &
                     standard_lon, truelat1, truelat2, map_info)
        print *,'map_info:', imap_proj, iproj
        print *,'code,lat1,lon1,dx,dlat,dlon,stdlon,truelat1,truelat2,hemi,cone,polei,polej,rsw,rebydx,knowni,knownj,init'
        print *, map_info

! .. Calculate the size for Mother Of All Domains and the low-left corner position
!
!    (The purpose doing this is for plotting the OBS distribution map with MAP_plot. Y.-R. Guo)
!
      if (xlonc /= standard_lon .or. phic /= moad_cen_lat) then

        phic    = moad_cen_lat
        xlonc   = standard_lon
        idd       = 2
        if (maxnes < idd) maxnes = idd
        nestix(2) = iix
        nestjx(2) = jjx
        dis(2)    = dis(1)
        numc(2)   = 1

        print '(/a,2e20.12)','   cen_lon  , cen_lat     :', &
                                 cen_lon,   cen_lat
        print '( a,2e20.12)','   xlonc, phic:', xlonc,     phic
        print '(/3X,a)',  &
       '## Compute the nestix(1), nestjx(1), nesti(2), and nstj(2): ##'

        xxc = real(jjx)/2.0
        yyc = real(iix)/2.0
        call latlon_to_ij(map_info, phic   , xlonc  , xxx, yyy)
!        print '("latlon_to_ij: phic, xlonc, xxx, yyy :",4f15.5)',phic, xlonc, xxx, yyy

        i_grid = nint(xxx-xxc)
        j_grid = nint(yyy-yyc)
        m_expand = 16
        print '("i_grid, j_grid,  m_expand:",3I8)', i_grid, j_grid,  m_expand

        nestjx(1) = jjx + 2*abs(i_grid) + m_expand
        nestix(1) = iix + 2*abs(j_grid) + m_expand

        nesti(2) = m_expand/2 + 1
        if (j_grid < 0) nesti(2) = nesti(2) - 2*j_grid
        nestj(2) = m_expand/2 + 1
        if (i_grid < 0) nestj(2) = nestj(2) - 2*i_grid
   
        print '(a,2i5,2x,a,2i5/)', &
           '  Dimension of MOAD: ix, jx:', nestix(1), nestjx(1), &
           '         nesti(2), nestj(2):', nesti(2), nestj(2)
      endif
      
   endif

   END SUBROUTINE GET_NAMELIST

END MODULE MODULE_NAMELIST
