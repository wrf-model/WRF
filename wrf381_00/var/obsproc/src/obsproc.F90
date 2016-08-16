PROGRAM main_obsproc

!-----------------------------------------------------------------------------!
! Read decoded observations (output format of the MM5 "gts_decoder" and "fetch"
!                           facilities, which is also the input format 
!                           of MM5 "rawins" and "little_r" programs).
!                           Keep only records with at least height or pressure 
!                           in MM5 horizontal domain as defined in namelist
! Fill with pressure when missing,
! Sort observations by location and time,
! Merge space duplicate stations (same type, same location, same time),
! Remove time duplicate stations in time (same type & location, different 
! time),
! Fill with height when missing,
! Check soundings (vertical consistency and super adiabatic),
! Remove observations above MM5 lid as defined by ptop in namelist,
! Estimate observational error,
! Write out for inputting into MM5 3D-VAR    
!
!-----------------------------------------------------------------------------!
!
!  HISTORY: 
!
!         F. VANDENBERGHE, March 2001
!
!         01/13/2003 - Updated for Profiler obs.           S. R. H. Rizvi
!
!         02/04/2003 - Updated for Buoy     obs.           S. R. H. Rizvi
!
!         02/11/2003 - Reviewed and modified for Profiler
!                      and Buoy obs.                       Y.-R. Guo
!
!         05/23/2003 - GPS ZTD code added              L. Cucurull
!         08/13/2003 - Reviewed                        Y.-R. Guo
!         09/XX/2005 - Modified to output in PREPBUFR format J. Drake
!         06/30/2006 - Updated for AIRS  retrievals      Syed  RH  Rizvi
!         10/09/2006 - Updated for GPS Excess Phase    S.-Y. Chen
!------------------------------------------------------------------------------

   USE module_mm5
   USE module_map
   USE map_utils
   USE module_namelist

   USE module_decoded
   USE module_type
   USE module_func
   USE module_icao
   USE module_sort
   USE module_duplicate
   USE module_per_type
   USE module_recoverp
   USE module_recoverh
   USE module_diagnostics
   USE module_qc
   USE module_err_ncep
   USE module_err_afwa
   USE module_complete
   USE module_thin_ob
   USE module_write
   use module_stntbl
!------------------------------------------------------------------------------!
   IMPLICIT NONE

   CHARACTER (LEN = 80)                 :: nml_filename
   CHARACTER (LEN = 80)                 :: title, caption
   LOGICAL                              :: exist
   INTEGER                              :: ii, fm_code, ntime
   INTEGER                              :: ins, jew
   INTEGER                              :: loop_index, number_of_obs
   INTEGER                              :: total_dups_loc, total_dups_time
   INTEGER                              :: total_dups
   INTEGER                              :: map_projection
   REAL                                 :: missing_flag

   TYPE (report),      DIMENSION (:), ALLOCATABLE :: obs, obs_copy
   TYPE (measurement), POINTER                    :: next, current
   INTEGER,            DIMENSION (:), ALLOCATABLE :: index

   real  :: lat, lon, xjd, yid, xxi, yyj, xxi1, yyj1

   CHARACTER (LEN = 19)      :: time_min
   CHARACTER (LEN = 19)      :: time_fg
   CHARACTER (LEN = 19)      :: time_max
   CHARACTER (LEN = 31)      :: SLOT_TITLE
   LOGICAL                   :: dis0, dis1, dis2
!------------------------------------------------------------------------------!


! 1.  READ THE NAMELIST
! ====================   

      nml_filename = 'namelist.obsproc'

      WRITE (UNIT = 0, FMT = '(/,A,A,/,A)')        &
    ' READ NAMELIST FILE: ',TRIM  (nml_filename),  &
    ' ------------------'

      CALL get_namelist (trim(nml_filename))


! 2.  SET THE MM5 MAP PROJECTION COEFICIENTS
! ==========================================
   
! 2.1 Height at model lid
!     -------------------

      htop = 0.; h_tropo = 0.
!   Tropopause height:
      h_tropo = Ref_height (pis0)
      htop = Ref_height (ptop)
 
     height_max_icao = h_from_p_icao (0.0)

!      write(0,'(/5(A,f12.1)/)')  &
!          "  htop=", htop, ", h_tropo=", h_tropo, &
!          ", height_max_icao=", height_max_icao,  &
!          ", ptop=", ptop, ", pis0=", pis0

! 2.2 Map coeficients
!     ---------------

       CALL setup (domain_check_h, iproj, phic, xlonc, truelat1, truelat2, &
#ifdef BKG
                   dis, xcntr, ycntr, xn, pole, psi1,  c2)
#else
                   maxnes, nestix, nestjx, dis, numc, nesti, nestj, &
                   ixc, jxc, xcntr, ycntr, xn, pole, psi1,  c2, &
                   xim11, xjm11)
#endif

      if (fg_format == 'WRF') then
        lat =  map_info%lat1
        lon =  map_info%lon1
        call LLXY(lat, lon, xxi, yyj)
        write(0,'(/"         LLXY:lat, lon, (dot) xxi, yyj:",4f12.5 )') &
                                               lat, lon, xxi, yyj
        call latlon_to_ij(map_info, lat, lon, xxi, yyj)
        write(0,'( "latlon_to_ij:lat, lon, (cross)xxi, yyj:",4f12.5/)') &
                                               lat, lon, xxi, yyj
      endif

! 2.3 Grid dimensions for the current domain
!     --------------------------------------

       ins = nestix (idd)
       jew = nestjx (idd)

! 3.  LOAD GTS OBSERVATIONS
! =========================

      icor = 0;        
      caption (7*(icor+0)+1:7*(icor+1)) = "   READ"
      caption (7*(icor+1)+1:7*(icor+2)) = "  EMPTY"
      caption (7*(icor+2)+1:7*(icor+3)) = "OUTSIDE"

! 3.1 Reset the number of ingested obs per type
!     -----------------------------------------

      nsynops = 0; nshipss = 0; nmetars = 0; npilots = 0; nsounds = 0;
      nsatems = 0; nsatobs = 0; naireps = 0; ngpspws = 0; namdars = 0;
      nssmt1s = 0; nssmt2s = 0; nssmis  = 0; ntovss  = 0; nqscats = 0;
      nothers = 0; nprofls = 0; nbuoyss = 0; ngpsztd = 0; ngpsref = 0;
      nboguss = 0; nairss  = 0; ngpseph = 0; ntamdar = 0;
 
! 3.2 Reset the total number of ingested obs
!     --------------------------------------

      number_of_obs   = 0
      total_dups_time = 0
      total_dups_loc  = 0

! 3.3 Allocate memory
!     --------------

      ALLOCATE (obs   (max_number_of_obs))
      ALLOCATE (index (max_number_of_obs))

! 3.4 Read gts observations
!     ---------------------

      INQUIRE (FILE = obs_gts_filename, EXIST = exist )

      IF (exist .and. LEN(TRIM(obs_gts_filename))>0) THEN

         call read_msfc_table('msfc.tbl')

      !  Read data from input file

        CALL read_obs_gts (obs_gts_filename, obs, number_of_obs, &
          max_number_of_obs, fatal_if_exceed_max_obs, print_gts_read, &
          ins, jew, time_window_min, time_window_max,                 &
          map_projection, missing_flag)

      !  Reset unused memory for subsequent reading

        DO loop_index = number_of_obs+1, max_number_of_obs
           NULLIFY (obs (loop_index) % surface)
        ENDDO

      ELSE
         WRITE (0,'(/,A,/)') "No decoded observation file to read."
         STOP
      ENDIF

! 3.6 Check if any data have been loaded
!     ----------------------------------

      IF (number_of_obs .GT. 0) THEN

      icor = icor + 3

! 4.  OBSERVATION SORTING
! =======================

      !  Sort the observations so that they can be merged easily.  Really
      !  what happens is that the index array is uniquely sorted by location
      !  (except for observations that are from the same "place").  This
      !  puts duplicate location observations next to each other.
      !  Then, merge the observations to (try to) remove all duplicates and
      !  build composite data.
      !  Then, sort obervations upon time, type and location

! 4.1 Recover the missing pressure based on the observed heights under
!     hydrostatic assumption or the model reference state.
!     ---------------------------------------------------
      CALL recover_pressure_from_height (max_number_of_obs , &
                                         obs, number_of_obs, print_recoverp)

      !hcl-note: is check_obs actually doing anything?
      CALL check_obs (max_number_of_obs, obs, number_of_obs, 'pressure')

! 4.2 Sort station per location
!     -------------------------

      CALL sort_obs (obs ,number_of_obs , compare_loc, index )

! 4.3 Merge duplicate stations (same location and same time)
!     ------------------------------------------------------
!     Because data merging is based on the pressure, the missing
!     pressure have been recovered (Sec. 4.1)

      caption (7*icor+1:7*(icor+1)) = "LOCDUPL"

      CALL check_duplicate_loc (obs ,index ,number_of_obs, total_dups_loc, &
                                time_analysis, print_duplicate_loc)

      icor = icor + 1

! 4.4 Remove duplicate stations (same location and different time)
!     ------------------------------------------------------------

      caption (7*icor+1:7*(icor+1)) = "TIMDUPL"

      if (use_for /= '4DVAR' )  &
      CALL check_duplicate_time (obs ,index ,number_of_obs, total_dups_time, &
                                 time_analysis, print_duplicate_time)

      icor = icor + 1

! 4.5 Total Number of duplicate stations removed
!     ------------------------------------------

      total_dups = total_dups_loc + total_dups_time


! 4.7 Sort obs chronologically
!     ------------------------

      CALL sort_obs (obs ,number_of_obs , compare_tim, index )

      ! Temporarily set back index to increasing order
      ! index = (/ ( loop_index , loop_index = 1 , max_number_of_obs ) /)

! 5.  DIAGNOSTICS, ESTIMATE HEIGHT WHEN MISSING, OBSERVATIONAL ERROR
! ==================================================================

! 5.1 Wind components, dew point relative humidity and mixing ratio
!     -------------------------------------------------------------
!     Gross error check on wind module (<0), direction (>360) and
!     Temperature (>0), dew point (>0&<T) and RH (0< <100) are also performed

      CALL derived_quantities (max_number_of_obs , obs , number_of_obs)

! 5.2 Height
!     ------

      CALL recover_height_from_pressure (max_number_of_obs , &
                                         obs, number_of_obs, print_recoverh)

      !hcl-note: is check_obs actually doing anything?
      CALL check_obs (max_number_of_obs, obs, number_of_obs, 'height')

! 5.3 Observational error (pressure must be present)
!     ----------------------------------------------

      !  When a input file is provided, read it

      INQUIRE (FILE = obs_err_filename, EXIST = exist )

      IF (exist) THEN

          CALL obs_err_afwa (obs_err_filename, &
                         max_number_of_obs, obs, number_of_obs)

      ELSE

         WRITE (0,'(/,A,/)') "No obs err input file "//trim(obs_err_filename)//" found."
         STOP

         !starting from V3.7.1, disable the usage below

         !Otherwise, use ncep value as default
         !write(0,'(/A/A/)') "!!! WARNING *** WARNING *** WARNING *** WARNING !!!", &
         !"<< NO AFWA OBS ERRORS FILE (obserr.txt) AVAILABE, USE NCEP ERROR AS DEFAULT >>"
         !CALL obs_err_ncep (max_number_of_obs, obs, number_of_obs)

      ENDIF

! 6.  CHECK COMPLETNESS AND QUALITY CONTROL PURELY BASED ON THE OBS DATA
! ======================================================================

! 6.1 Vertical consistency check and dry convective adjustment QC
!     -----------------------------------------------------------

      CALL proc_qc1 (max_number_of_obs , obs , number_of_obs,            &
                     qc_test_vert_consistency , qc_test_convective_adj , &
                     print_qc_vert, print_qc_conv)

! 6.2 Vertical domain check
!     ---------------------

      CALL proc_qc2 (max_number_of_obs , obs , number_of_obs, &
                     qc_test_above_lid, print_qc_lid) 

! 6.2 Check completness (Remove levels without data and too low or too high)
!     ---------------------------------------------------------------------

      caption (7*icor+1:7*(icor+1)) = "UNCOMPL"

      CALL check_completness (max_number_of_obs , &
                              obs, number_of_obs, remove_above_lid, &
                              print_uncomplete)

      icor = icor + 1

! 6.3 Print statistics on obs
!     -----------------------

      caption (7*icor+1:7*(icor+1)+1) = "INGESTD"
      title = "INGESTED OBSERVATION AFTER CHECKS:"

      CALL print_per_type (TRIM (title), TRIM (caption), icor)

      icor = icor+1

! 7. Reduce the QC flags and thin the satellite data
! ==================================================

! 7.1 Reduce QC from 8 to 3 digits
!     ----------------------------

      CALL qc_reduction (max_number_of_obs, obs, number_of_obs)

! 7.2 Thin the SATOB data before writing out

      IF (Thining_SATOB) &
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SATOB', 88, 1000.0)

      IF (Thining_SSMI) THEN
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SSMI_Rtvl', 125)

      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'SSMI_Tb', 126)
      ENDIF

      IF (Thining_QSCAT) &
      CALL THIN_OB (max_number_of_obs, obs, number_of_obs, &
                   nestix(idd), nestjx(idd), missing_flag, &
                                        'Qscatcat', 281)
      
! 8.  OUTPUT DIRECT OBSERVATIONS (U,V,T,QV) IN ASCII and PREBUFR FILE
! ===================================================================

! 8.1 Keep a copy of the obs
!     ------------------------

      allocate (obs_copy(1:max_number_of_obs))      
      obs_copy = obs

! 8.2 Loop over the time slots
!     ------------------------

      do ntime = 1, num_time_slots

! 8.3 Set the time_min, time_fg, and time_max for the time slot: ntime
!     ----------------------------------------------------------------

        if ( num_time_slots > 1 ) then
      
! 8.3.1 the number of time slots greater than 1 for FGAT and 4DVAR
!       ..........................................................

          if ( ntime == 1 .or. ntime == num_time_slots ) then
             idt = slot_len / 2 - 1
          else
             idt = slot_len - 1
          endif

          if (ntime == 1) then
             time_min  = time_window_min
             time_fg  = time_window_min
          else 
             call geth_newdate (time_min, time_max, 1)
             if (ntime == num_time_slots ) then
                 time_fg = time_window_max
             else
                 call geth_newdate (time_fg, time_min, slot_len/2)
             endif           
          endif
          call geth_newdate (time_max, time_min, idt)
        
        else
         
! 8.3.2 the number of time slots equal to 1 for 3DVAR, FGAT and 4DVAR
!       ..........................................................

          time_min = time_window_min
          call  geth_newdate (time_max, time_window_max, -1)
          time_fg = time_analysis

        endif

! 8.3.3 Print the time slot information
!       ...............................
             
        write(0,'(//a,i2,4(2x,a))') "slot=",ntime, & 
            "time_min, time_fg, time_max:", time_min, time_fg, time_max

! 8.4 Get the original total obs back
!     -------------------------------

        deallocate (obs)
        allocate(obs(1:max_number_of_obs))

        obs = obs_copy

! 8.5 Loop over all the obs
!     ---------------------

        do ii = 1,number_of_obs

! 8.6 determine the obs "discard" flag
!     --------------------------------

! 8.7.1 Initialize the "discard" switch" for a obs
!       ...........................................

        dis0 = obs(ii)%info%discard
        dis1 = .false.
        dis2 = .false.

! 8.7.2 Kick out the specific type of obs based on namelist settings
!       ............................................................

        read(obs(ii)%info%platform(4:6),'(i3)') fm_code
        if ( (.not.write_synop .and. (fm_code==12  .or. fm_code==14)) .or. &
             (.not.write_ship  .and.  fm_code==13)                    .or. &
             (.not.write_metar .and. (fm_code==15  .or. fm_code==16)) .or. &
             (.not.write_buoy  .and. (fm_code==18  .or. fm_code==19)) .or. &
             (.not.write_pilot .and. (fm_code>=32 .and. fm_code<=34)) .or. &
             (.not.write_sound .and. (fm_code>=35 .and. fm_code<=38)) .or. &
             (.not.write_amdar .and.  fm_code==42)                    .or. &
             (.not.write_satem .and.  fm_code==86)                    .or. &
             (.not.write_satob .and.  fm_code==88)                    .or. &
             (.not.write_airep .and. (fm_code==96  .or. fm_code==97)) .or. &
             (.not.write_tamdar.and.  fm_code==101)                   .or. &
             (.not.write_gpspw .and.  fm_code==111)                   .or. &
             (.not.write_gpsztd.and.  fm_code==114)                   .or. &
             (.not.write_gpsref.and.  fm_code==116)                   .or. &
             (.not.write_gpseph.and.  fm_code==118)                   .or. &
             (.not.write_ssmt1 .and.  fm_code==121)                   .or. &
             (.not.write_ssmt2 .and.  fm_code==122)                   .or. &
             (.not.write_ssmi  .and. (fm_code==125 .or. fm_code==126)).or. &
             (.not.write_tovs  .and.  fm_code==131)                   .or. &
             (.not.write_qscat .and.  fm_code==281)                   .or. &
             (.not.write_profl .and.  fm_code==132)                   .or. &
             (.not.write_bogus .and.  fm_code==135)                   .or. &
             (.not.write_airs  .and.  fm_code==133)                   )    &
             dis1 = .true.
! 8.7.3 Kick out the obs outside the time slot
!       ......................................

         CALL inside_window (obs(ii)%valid_time%date_char, &
                              time_min, time_max, &
                              dis2)

! 8.7.4 Reset the obs "discard" flag
!       ............................

         obs(ii)%info%discard = dis0 .or. dis1 .or.dis2

! 8.8 end of the loop over obs
!     ------------------------

      enddo

! 8.9 Time duplicate check within a slot for 4DVAR
!     --------------------------------------------

      if (use_for == '4DVAR' )  then
        CALL sort_obs (obs ,number_of_obs , compare_loc, index )

        CALL check_duplicate_time (obs ,index ,number_of_obs, total_dups_time, &
                                   time_fg, print_duplicate_time)
      endif

! 8.10 Print report per platform type and count the # levels per stations
!      ------------------------------------------------------------------

      write(SLOT_TITLE,'("OBSERVATIONS FOR OUTPUT SLOT ",I2.2)') ntime
      CALL sort_platform (max_number_of_obs, obs, number_of_obs, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), nairss  (icor), ntamdar (icor), &
                          SLOT_TITLE)

! 8.11 Determine output type
!      ----------------------------------------------

      IF (output_ob_format .eq. 1 .or. output_ob_format .eq. 3) THEN

! 8.11.1 Output observations in PREPBUFR
!        -------------------------------
      CALL output_prep (max_number_of_obs, obs, number_of_obs, index, &
                          prepbufr_table_filename, &
                          prepbufr_output_filename, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), missing_flag, time_analysis)


      ENDIF

      IF (output_ob_format .eq. 2 .or. output_ob_format .eq. 3) THEN

! 8.11.2 Output observations in 3D-VAR Version 3.1 gts format
!        ----------------------------------------------------

      CALL output_gts_31 (max_number_of_obs, obs, number_of_obs, index, &
                          nsynops (icor), nshipss (icor), nmetars (icor), &
                          npilots (icor), nsounds (icor), nsatems (icor), &
                          nsatobs (icor), naireps (icor), ngpspws (icor), &
                          ngpsztd (icor), ngpsref (icor), ngpseph (icor), &
                          nssmt1s (icor), nssmt2s (icor), nssmis  (icor), &
                          ntovss  (icor), nothers (icor), namdars (icor), &
                          nqscats (icor), nprofls (icor), nbuoyss (icor), &
                          nboguss (icor), nairss  (icor), ntamdar(icor), missing_flag, time_fg)

      CALL output_ssmi_31 (max_number_of_obs, obs, number_of_obs, index, &
                           nssmis  (icor), &
                           missing_flag, time_fg)

      ENDIF


      enddo

! 9.  END
! =======
      ENDIF
      STOP "99999"

END PROGRAM main_obsproc
