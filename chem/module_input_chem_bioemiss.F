!dis
!dis    Open Source License/Disclaimer, Forecast Systems Laboratory
!dis    NOAA/OAR/FSL, 325 Broadway Boulder, CO 80305
!dis
!dis    This software is distributed under the Open Source Definition,
!dis    which may be found at http://www.opensource.org/osd.html.
!dis
!dis    In particular, redistribution and use in source and binary forms,
!dis    with or without modification, are permitted provided that the
!dis    following conditions are met:
!dis
!dis    - Redistributions of source code must retain this notice, this
!dis    list of conditions and the following disclaimer.
!dis
!dis    - Redistributions in binary form must provide access to this
!dis    notice, this list of conditions and the following disclaimer, and
!dis    the underlying source code.
!dis
!dis    - All modifications to this software must be clearly documented,
!dis    and are solely the responsibility of the agent making the
!dis    modifications.
!dis
!dis    - If significant modifications or enhancements are made to this
!dis    software, the FSL Software Policy Manager
!dis    (softwaremgr@fsl.noaa.gov) should be notified.
!dis
!dis    THIS SOFTWARE AND ITS DOCUMENTATION ARE IN THE PUBLIC DOMAIN
!dis    AND ARE FURNISHED "AS IS."  THE AUTHORS, THE UNITED STATES
!dis    GOVERNMENT, ITS INSTRUMENTALITIES, OFFICERS, EMPLOYEES, AND
!dis    AGENTS MAKE NO WARRANTY, EXPRESS OR IMPLIED, AS TO THE USEFULNESS
!dis    OF THE SOFTWARE AND DOCUMENTATION FOR ANY PURPOSE.  THEY ASSUME
!dis    NO RESPONSIBILITY (1) FOR THE USE OF THE SOFTWARE AND
!dis    DOCUMENTATION; OR (2) TO PROVIDE TECHNICAL SUPPORT TO USERS.
!dis
!dis

!WRF:PACKAGE:IO

MODULE module_input_chem_bioemiss

    USE module_io_domain
    USE module_domain
    USE module_driver_constants
    USE module_state_description
    USE module_configure
    USE module_date_time
    USE module_wrf_error
    USE module_timing
    USE module_data_radm2
    USE module_aerosols_sorgam
    USE module_get_file_names


CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE input_ext_chem_beis3_file (grid)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IMPLICIT NONE

   TYPE(domain)           ::  grid

   INTEGER ::  i,j,n,numfil,status,system

   INTEGER :: ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe

   REAL, ALLOCATABLE, DIMENSION(:,:) :: emiss



! Number of reference emission and LAI files to open
      PARAMETER(numfil=19)

   CHARACTER (LEN=80) :: message

   TYPE (grid_config_rec_type)              :: config_flags

! Normalized biogenic emissions for standard conditions (moles compound/km^2/hr)
!     REAL, DIMENSION(i,j) ::                                     &
!              sebio_iso,sebio_oli,sebio_api,sebio_lim,sebio_xyl,   &
!              sebio_hc3,sebio_ete,sebio_olt,sebio_ket,sebio_ald,   &
!              sebio_hcho,sebio_eth,sebio_ora2,sebio_co,sebio_nr,   &
!              noag_grow,noag_nongrow,nononag

! Leaf area index for isoprene
!     REAL, DIMENSION(i,j) ::  slai

! Filenames of reference emissions and LAI
      CHARACTER*100 onefil
      CHARACTER*12 emfil(numfil)
      DATA emfil/'ISO','OLI','API','LIM','XYL','HC3','ETE','OLT',  &
        'KET','ALD','HCHO','ETH','ORA2','CO','NR',                 &
        'NOAG_GROW','NOAG_NONGROW','NONONAG','ISOP'/

!!!!!-------------------------------------------------------------------

       ! Get grid dimensions
       CALL get_ijk_from_grid (  grid ,                        &
                                 ids, ide, jds, jde, kds, kde,    &
                                 ims, ime, jms, jme, kms, kme,    &
                                 ips, ipe, jps, jpe, kps, kpe    )

     WRITE( message , FMT='(A,4I5)' ) ' DIMS: ',ids,ide-1,jds,jde-1
     CALL  wrf_message ( message )

     ALLOCATE( emiss(ids:ide-1,jds:jde-1) )


! Loop over the file names
      DO n=1,numfil

!   Remove scratch unzipped file
       status=system('rm -f scratem*')

!   All reference emissions except NO
       IF(n.LE.15)THEN 
        onefil='../../run/BIOREF_'//             &
         TRIM(ADJUSTL(emfil(n)))//'.gz'
!   NO reference emissions
       ELSE IF(n.GE.16.AND.n.LE.18)THEN 
        onefil='../../run/AVG_'//                &
         TRIM(ADJUSTL(emfil(n)))//'.gz'
!   LAI
       ELSE
        onefil='../../run/LAI_'//                &
         TRIM(ADJUSTL(emfil(n)))//'S.gz'
       ENDIF

!   Copy selected file to scratch
       status=system('cp '//TRIM(ADJUSTL(onefil))//' scratem.gz')

!   Unzip scratch
       status=system('gunzip scratem')

!   Open scratch and read into appropriate array
       OPEN(26,FILE='scratem',FORM='FORMATTED')
       IF(n.EQ. 1) then
             READ(26,'(12E9.2)') emiss
             grid%sebio_iso(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 2)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_oli(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 3)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_api(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 4)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_lim(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 5)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_xyl(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 6)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_hc3(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 7)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_ete(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 8)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_olt(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ. 9)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_ket(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.10)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_ald(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.11)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_hcho(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.12)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_eth(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.13)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_ora2(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.14)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_co(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.15)then
              READ(26,'(12E9.2)') emiss
              grid%sebio_nr(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.16)then
              READ(26,'(12E9.2)') emiss
              grid%noag_grow(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.17)then
              READ(26,'(12E9.2)') emiss
              grid%noag_nongrow(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.18)then
              READ(26,'(12E9.2)') emiss
              grid%nononag(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       IF(n.EQ.19)then
              READ(26,'(12E9.2)') emiss
              grid%slai(ids:ide-1,jds:jde-1) = emiss
       ENDIF
       CLOSE(26)

      ENDDO
! End of loop over file names

    DEALLOCATE( emiss )

END SUBROUTINE input_ext_chem_beis3_file 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE module_input_chem_bioemiss

