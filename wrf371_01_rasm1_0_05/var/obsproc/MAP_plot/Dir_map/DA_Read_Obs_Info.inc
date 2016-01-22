SUBROUTINE DA_Read_Obs_Info (iunit)

!-----------------------------------------------------------------------------!
! Read the header of a MM5 3D-VAR 3.0 GTS observation file
!-----------------------------------------------------------------------------!

   implicit none

   INTEGER,        INTENT (in)  :: iunit

   INTEGER                      :: iost, i, ii
   CHARACTER (LEN = 120)        :: char_ned
   LOGICAL                      :: connected

     INTEGER                      :: nsynops, npilots, nsounds, ntotals, &
                                     nsatobs, nsatems, naireps, ngpspws, &
                                     nssmt1s, nssmt2s, ntovsss, nothers, &
                                     nmetars, nshipss, nqscats, nprofls, &
                                     nbuoyss, ngpsref, nboguss, nairsret
     INTEGER                      :: ixc, jxc, iprj, idd
     REAL                         :: ts0    , ps0    , tlp     , ptop

!-----------------------------------------------------------------------------!
   WRITE (0,'(A)')  &
   '--------------------------------------------------------------------------'


! 1. OPEN FILE
!    ---------

   INQUIRE ( UNIT = iunit, OPENED = connected )

   IF ( .NOT. connected ) THEN
      OPEN ( UNIT   = iunit,     &
           FORM   = 'FORMATTED',  &
           ACCESS = 'SEQUENTIAL', &
           IOSTAT =  iost,     &
           STATUS = 'OLD')

      IF (iost /= 0) THEN
          WRITE (0,'(/,A,I3,A,/)') &
        ' INPUT FILE UNIT ',iunit, &
        ' FOR GTS OBSERVATIONS CANNOT BE FOUND OR CANNOT BE OPENED'
          RETURN
      ENDIF

   ENDIF

   REWIND (UNIT = iunit)


! 2.  READ HEADER
! ===============

! 2.1 READ FIRST LINE
!     ---------------

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned

! 2.2 PROCESS ERROR
!     -------------

   IF (iost /= 0) THEN
       WRITE (0,'(/A,i3/)') ' ERROR READING FILE UNIT:', iunit
       STOP         ' in read_obs_gts_header.F' 
   ENDIF

! 2.3 RED NUMBER OF REPORTS
!     ---------------------

head_line_1:  DO

   DO i = 0, 120-14

     SELECT CASE ( char_ned (I+1:I+5) )

  ! Number of observations

     CASE ('TOTAL') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ntotals
     CASE ('SYNOP') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nsynops
       max_synop_input = nsynops
       if (max_synop_input > max_synop) &
         write(0,'(A,2I8)') 'WARNING:max_synop_input > max_synop ==>',&
                             max_synop_input, max_synop 
     CASE ('METAR') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nmetars
       max_metar_input = nmetars
       if (max_metar_input > max_metar) &
         write(0,'(A,2I8)') 'WARNING:max_metar_input > max_metar ==>',&
                             max_metar_input, max_metar 
     CASE ('SHIP ') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nshipss
       max_ships_input = nshipss
       if (max_ships_input > max_ships) &
         write(0,'(A,2I8)') 'WARNING:max_ships_input > max_ships ==>',&
                             max_ships_input, max_ships 
     CASE ('TEMP ') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nsounds
       max_sound_input = nsounds
       if (max_sound_input > max_sound) &
         write(0,'(A,2I8)') 'WARNING:max_sound_input > max_sound ==>',&
                             max_sound_input, max_sound 
     CASE ('AIREP') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) naireps
       max_airep_input = naireps
       if (max_airep_input > max_airep) &
         write(0,'(A,2I8)') 'WARNING:max_airep_input > max_airep ==>',&
                             max_airep_input, max_airep 
     CASE ('PILOT') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) npilots
       max_pilot_input = npilots
       if (max_pilot_input > max_pilot) &
         write(0,'(A,2I8)') 'WARNING:max_pilot_input > max_pilot ==>',&
                             max_pilot_input, max_pilot 
     CASE ('SATEM') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nsatems
       max_satem_input = nsatems
       if (max_satem_input > max_satem) &
         write(0,'(A,2I8)') 'WARNING:max_satem_input > max_satem ==>',&
                             max_satem_input, max_satem 
     CASE ('SATOB') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nsatobs
       max_satob_input = nsatobs
       if (max_satob_input > max_satob) &
         write(0,'(A,2I8)') 'WARNING:max_satob_input > max_satob ==>',&
                             max_satob_input, max_satob 
     CASE ('GPSPW') ;
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ngpspws
           max_gpspw_input = ngpspws

       if (max_gpspw_input > max_gpspw) &
         write(0,'(A,2I8)') 'WARNING:max_gpspw_input > max_gpspw ==>',&
                             max_gpspw_input, max_gpspw 
     CASE ('GPSZD') ;
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ngpspws
       if (ngpspws > 0) max_gpspw_input = ngpspws

       if (max_gpspw_input > max_gpspw) &
         write(0,'(A,2I8)') 'WARNING:max_gpspw_input > max_gpspw ==>',&
                             max_gpspw_input, max_gpspw 
     CASE ('GPSRF') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ngpsref
       max_gpsref_input = ngpsref
       if (max_gpsref_input > max_gpsref) &
         write(0,'(A,2I8)') 'WARNING:max_gpsref_input > max_gpsref ==>',&
                             max_gpsref_input, max_gpsref 
     CASE ('SSMT1') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nssmt1s
       max_ssmt1_input = nssmt1s
       if (max_ssmt1_input > max_ssmt1) &
         write(0,'(A,2I8)') 'WARNING:max_ssmt1_input > max_ssmt1 ==>',&
                             max_ssmt1_input, max_ssmt1 
     CASE ('SSMT2') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nssmt2s
       max_ssmt2_input = nssmt2s
       if (max_ssmt2_input > max_ssmt2) &
         write(0,'(A,2I8)') 'WARNING:max_ssmt2_input > max_ssmt2 ==>',&
                             max_ssmt2_input, max_ssmt2 
     CASE ('TOVS ') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) ntovsss
       max_tovs_input = ntovsss
       if (max_tovs_input > max_tovs) &
         write(0,'(A,2I8)') 'WARNING:max_tovss_input > max_tovss ==>',&
                             max_tovs_input, max_tovs 
     CASE ('AIRSR') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nairsret
       max_airsret_input = nairsret
       if (max_airsret_input > max_airsret) &
         write(0,'(A,2I8)') 'WARNING:max_airsret_input > max_airsret ==>',&
                             max_airsret_input, max_airsret 
     CASE ('QSCAT') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nqscats
       max_qscat_input = nqscats
       if (max_qscat_input > max_qscat) &
         write(0,'(A,2I8)') 'WARNING:max_qscat_input > max_qscat ==>',&
                             max_qscat_input, max_qscat 
     CASE ('PROFL') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nprofls
       max_profl_input = nprofls
       if (max_profl_input > max_profl) &
         write(0,'(A,2I8)') 'WARNING:max_profl_input > max_profl ==>',&
                             max_profl_input, max_profl 
     CASE ('BUOY ') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nbuoyss
       max_buoys_input = nbuoyss
       if (max_buoys_input > max_buoys) &
         write(0,'(A,2I8)') 'WARNING:max_buoys_input > max_buoys ==>',&
                             max_buoys_input, max_buoys 
     CASE ('BOGUS') ; 
       READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nboguss
       max_bogus_input = nboguss
       if (max_bogus_input > max_bogus) &
         write(0,'(A,2I8)') 'WARNING:max_bogus_input > max_bogus ==>',&
                             max_bogus_input, max_bogus 
     CASE ('OTHER') ; 
        READ (char_ned (I+9:I+14),'(I6)', IOSTAT = iost) nothers

  ! Geographic area and reference atmosphere definition
 
      CASE ('MISS.') ; 
          READ (char_ned (I+8:I+15),'(F8.0)', IOSTAT = iost) ob % missing
      CASE ('PHIC ') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) phic
      CASE ('XLONC') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) xlonc
      CASE ('TRUE1') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) truelat1
      CASE ('TRUE2') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) truelat2
      CASE ('TS0  ') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) ts0
      CASE ('TLP  ') ; 
          READ (char_ned (I+8:I+14),'(F7.2)', IOSTAT = iost) tlp
      CASE ('PTOP ') ; 
          READ (char_ned (I+8:I+14),'(F7.0)', IOSTAT = iost) ptop
      CASE ('PS0  ') ; 
          READ (char_ned (I+8:I+14),'(F7.0)', IOSTAT = iost) ps0
      CASE ('IXC  ') ; 
          READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) ixc
      CASE ('JXC  ') ; 
          READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) jxc
      CASE ('IPROJ') ; 
          READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) iprj
          if (iprj == 1) then
            iproj = 'LAMCON'
          else if (iprj == 2) then
            iproj = 'POLSTR'
          else if (iprj == 3) then
            iproj = 'MERCAT'
          else if (iprj == 0) then
            iproj = 'CYLEQU'
          endif
      CASE ('IDD  ') ; 
          READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) idd
      CASE ('MAXNE') ; 
          READ (char_ned (I+8:I+14),'(I7)', IOSTAT = iost) maxnes

      END SELECT

   ENDDO

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
   if (char_ned(1:6) == 'NESTIX') EXIT

   ENDDO head_line_1

head_line_2:  DO

     SELECT CASE ( char_ned (1:6) )

  ! Model domains definition
    
      CASE ('NESTIX') ;
           CALL read_integer_array(nestix, maxnes, 8, 9)
      CASE ('NESTJX') ; 
           CALL read_integer_array(nestjx, maxnes, 8, 9)
      CASE ('NUMC  ') ; 
           CALL read_integer_array(numc  , maxnes, 8, 9)
      CASE ('DIS   ') ; 
           CALL read_real_array   (dis   , maxnes, 8, 9)
      CASE ('NESTI ') ; 
           CALL read_integer_array(nesti , maxnes, 8, 9)
      CASE ('NESTJ ') ; 
           CALL read_integer_array(nestj , maxnes, 8, 9)
     END SELECT

   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
   if (char_ned(1:6) == 'INFO  ') EXIT

   ENDDO head_line_2

! PRINT OUT
! =============

   WRITE (0, '(A)')
   WRITE (0,'(A,F8.0)') ' Missing value flag:  ', ob%missing
   WRITE (0, '(A)')
   WRITE (0,'(A,I6,A)') ' Expect: ',nsynops,' SYNOP reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nmetars,' METAR reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nshipss,' SHIP  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsounds,' TEMP  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',naireps,' AIREP reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',npilots,' PILOT reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsatems,' SATEM reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nsatobs,' SATOB reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',max_gpspw_input,' GPSPW/GPSZD reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',ngpsref,' GPSRF reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nssmt1s,' SSMT1 reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nssmt2s,' SSMT2 reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',ntovsss,' TOVS  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nairsret,' AIRS  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nqscats,' QSCAT reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nprofls,' PROFL reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nbuoyss,' BUOY  reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nboguss,' BOGUS reports,'
   WRITE (0,'(A,I6,A)') ' Expect: ',nothers,' OTHER reports,'
   WRITE (0, '(A)')
   WRITE (0,'(A,I6,A)') ' Total:  ',ntotals,' reports.'

   if (iprj == 0) then
            ixc = nestix(1)
            jxc = nestjx(1)
   endif

   WRITE (0,'(/A)') 'GEOGRAPHIC AREA AND REFERENCE ATMOSPHERE:'
   WRITE(0,'(A,f8.2)') 'PHIC     =', phic
   WRITE(0,'(A,f8.2)') 'XLONC    =', xlonc
   WRITE(0,'(A,f8.2)') 'TRUELAT1 =', truelat1
   WRITE(0,'(A,f8.2)') 'TRUELAT2 =', truelat2
   WRITE(0,'(A,f8.2)') 'TS0      =', ts0
   WRITE(0,'(A,f8.0)') 'PS0      =', ps0
   WRITE(0,'(A,f8.2)') 'TLP      =', tlp
   WRITE(0,'(A,f8.0)') 'PTOP     =', ptop
   WRITE(0,'(A,I8)'  ) 'IXC      =', ixc
   WRITE(0,'(A,I8)'  ) 'JXC      =', jxc
   WRITE(0,'(A, A)'  ) 'IPROJ    =', iproj
   WRITE(0,'(A,I8)'  ) 'IDD      =', idd
   WRITE(0,'(A,I8)'  ) 'MAXNES   =', maxnes
   if (maxnes > idd) maxnes = idd

   WRITE(0,'(A,(I7,2X)  )',ADVANCE='no') 'NESTIX:', (nestix(i), i = 1,maxnes)
   WRITE(0,'(A)')
   WRITE(0,'(A,(I7,2X)  )',ADVANCE='no') 'NESTJX:', (nestjx(i), i = 1,maxnes)
   WRITE(0,'(A)')
   WRITE(0,'(A,(I7,2X)  )',ADVANCE='no') 'NUMC  :', (numc (i), i = 1,maxnes)
   WRITE(0,'(A)')
   WRITE(0,'(A,(f7.1,2X))',ADVANCE='no') 'DIS   :', (dis   (i), i = 1,maxnes)
   WRITE(0,'(A)')
   WRITE(0,'(A,(I7,2X))  ',ADVANCE='no') 'NESTI :', (nesti (i), i = 1,maxnes)
   WRITE(0,'(A)')
   WRITE(0,'(A,(I7,2X))  ',ADVANCE='no') 'NESTJ :', (nestj (i), i = 1,maxnes)
   WRITE(0,'(A)')

   WRITE(0,'(/A)') 'OBSERVATION DATA LAYOUT:'
   WRITE(0,'(A)') TRIM(char_ned)
   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
   WRITE(0,'(A)') TRIM(char_ned)
   READ (UNIT = iunit, FMT = '(A)', IOSTAT = iost) char_ned
   WRITE(0,'(A)') TRIM(char_ned)

CONTAINS

subroutine read_integer_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  integer,  dimension(:), intent(out) :: aa

  integer     :: i, ii
 
  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (char_ned (ii:ii+8),'((I7,2X))') aa(i)
  enddo
end subroutine read_integer_array

subroutine read_real_array(aa,nn,i0,i_step)

  integer,                 intent(in) :: nn, i0, i_step
  real   ,  dimension(:), intent(out) :: aa

  integer     :: i, ii
 
  do i = 1, nn
    ii = i_step*(i-1) + i0
    READ (char_ned (ii:ii+8),'((f7.2,2X))') aa(i)
  enddo
end subroutine read_real_array

END SUBROUTINE DA_Read_Obs_Info
