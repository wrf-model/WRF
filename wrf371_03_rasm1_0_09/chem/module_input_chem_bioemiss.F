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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE input_ext_chem_megan2_file (grid)

  !
  !  This subroutine reads in an ASCII file of variables that are needed
  !  as input for biogenic emissions model MEGAN version 2. The
  !  variables are:
  !
  !      Isoprene emissions at referenece tempperature and
  !          light conditions [=] moles/km2/hr
  !      Leaf area index (one each month)
  !      Plant functional groups
  !       Broadleaf trees
  !       Needleleave trees
  !       Shrubs and Bushes
  !       Herbs
  !     "Climatological" variables:
  !        Monthly surface air temperature [=] K
  !        Monthly downward solar radiation [=] W/m2
  !
  !  April, 2007 Serena H. Chung and Christine Wiedinmyer
  !



   IMPLICIT NONE          

   TYPE(domain)           ::  grid

   INTEGER ::  i,j,v,status,system, itmp, jtmp

   INTEGER :: ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: emiss

   CHARACTER (LEN=80) :: message

   TYPE (grid_config_rec_type)              :: config_flags


   ! Variables "Pointers"
   ! The order must follow that of the ASCII input file
   integer, parameter :: n_mgnin = 41
   integer, parameter ::        & ! Pointer for :
        &  mgnin_isop     =  1  & !  isoprene reference emissions
        & ,mgnin_lai01    =  2  & !  Leaf area index for January 
        & ,mgnin_lai02    =  3  & !                      February  
        & ,mgnin_lai03    =  4  & !                      March 
        & ,mgnin_lai04    =  5  & !                      April  
        & ,mgnin_lai05    =  6  & !                      May  
        & ,mgnin_lai06    =  7  & !                      June  
        & ,mgnin_lai07    =  8  & !                      July  
        & ,mgnin_lai08    =  9  & !                      August  
        & ,mgnin_lai09    = 10  & !                      September  
        & ,mgnin_lai10    = 11  & !                      October
        & ,mgnin_lai11    = 12  & !                      November 
        & ,mgnin_lai12    = 13  & !                      December
        & ,mgnin_pftp_bt  = 14  & ! plant functional type % for broadleaf trees
        & ,mgnin_pftp_nt  = 15  & !                             needleleaf trees
        & ,mgnin_pftp_sb  = 16  & !                             shrubs and bushes
        & ,mgnin_pftp_hb  = 17  & !                             herbs
        & ,mgnin_tsa01    = 18  & ! monthly-mean surface air temperature for January
        & ,mgnin_tsa02    = 19  & !                                           February  
        & ,mgnin_tsa03    = 20  & !                                           March 
        & ,mgnin_tsa04    = 21  & !                                           April  
        & ,mgnin_tsa05    = 22  & !                                           May  
        & ,mgnin_tsa06    = 23  & !                                           June  
        & ,mgnin_tsa07    = 24  & !                                           July  
        & ,mgnin_tsa08    = 25  & !                                           August  
        & ,mgnin_tsa09    = 26  & !                                           September  
        & ,mgnin_tsa10    = 27  & !                                           October
        & ,mgnin_tsa11    = 28  & !                                           November 
        & ,mgnin_tsa12    = 29  & !                                           December
        & ,mgnin_swdown01 = 30  & !  monthl-mean solar irradiance at surface for January 
        & ,mgnin_swdown02 = 31  & !                                              February  
        & ,mgnin_swdown03 = 32  & !                                              March 
        & ,mgnin_swdown04 = 33  & !                                              April  
        & ,mgnin_swdown05 = 34  & !                                              May  
        & ,mgnin_swdown06 = 35  & !                                              June  
        & ,mgnin_swdown07 = 36  & !                                              July  
        & ,mgnin_swdown08 = 37  & !                                              August  
        & ,mgnin_swdown09 = 38  & !                                              September  
        & ,mgnin_swdown10 = 39  & !                                              October
        & ,mgnin_swdown11 = 40  & !                                              November 
        & ,mgnin_swdown12 = 41    !                                              December

      CHARACTER*100 onefil

!!!!!-------------------------------------------------------------------

       ! Get grid dimensions
       CALL get_ijk_from_grid (  grid ,                           &
                                 ids, ide, jds, jde, kds, kde,    &
                                 ims, ime, jms, jme, kms, kme,    &
                                 ips, ipe, jps, jpe, kps, kpe    )

     WRITE( message , FMT='(A,4I5)' ) ' in input_ext_chem_megan2_file, DIMS: ',ids,ide-1,jds,jde-1
     CALL  wrf_message ( message )

     ALLOCATE( emiss(ids:ide-1,jds:jde-1,n_mgnin) )

     ! Remove scratch file
!    status=system('rm -f scratem*')


     !   Copy selected file to scratch
     onefil='MEGAN_input_WRFchem.txt'
!    status=system('cp '//TRIM(ADJUSTL(onefil))//' scratem')

     !   Open scratch and read into appropriate array
!    OPEN(26,FILE='scratem',FORM='FORMATTED', status='old')
     OPEN(26,FILE=trim(onefil),FORM='FORMATTED', status='old')

     ! The following follows the file format provided by Christine Wiedinmyer

     do i = ids, ide-1
        do j = jds, jde-1
           read (26, FMT='(2(I5,1x),41(ES11.2,1x))') itmp, jtmp, (emiss(i,j,v),v=1,n_mgnin)
           ! redundant to be sure
           if ( (i /= itmp) .or. j /= jtmp ) then
              WRITE( message , FMT='(A,I3,I3,A,I3,I3)' ) 'Something is wrong (i,j) = ',i,j,"itmp, jtmp = ",itmp,jtmp
              call wrf_error_fatal(message)
           end if
        end do
     end do


     ! Isoprene emissions at standard conditions [=] mol/km2/hr
     grid%msebio_isop(ids:ide-1,jds:jde-1) = emiss(ids:ide-1,jds:jde-1,mgnin_isop)
     ! Leaf area index for each month
     grid%mlai    (ids:ide-1,jds:jde-1,01) = emiss(ids:ide-1,jds:jde-1,mgnin_lai01)
     grid%mlai    (ids:ide-1,jds:jde-1,02) = emiss(ids:ide-1,jds:jde-1,mgnin_lai02)
     grid%mlai    (ids:ide-1,jds:jde-1,03) = emiss(ids:ide-1,jds:jde-1,mgnin_lai03)
     grid%mlai    (ids:ide-1,jds:jde-1,04) = emiss(ids:ide-1,jds:jde-1,mgnin_lai04)
     grid%mlai    (ids:ide-1,jds:jde-1,05) = emiss(ids:ide-1,jds:jde-1,mgnin_lai05)
     grid%mlai    (ids:ide-1,jds:jde-1,06) = emiss(ids:ide-1,jds:jde-1,mgnin_lai06)
     grid%mlai    (ids:ide-1,jds:jde-1,07) = emiss(ids:ide-1,jds:jde-1,mgnin_lai07)
     grid%mlai    (ids:ide-1,jds:jde-1,08) = emiss(ids:ide-1,jds:jde-1,mgnin_lai08)
     grid%mlai    (ids:ide-1,jds:jde-1,09) = emiss(ids:ide-1,jds:jde-1,mgnin_lai09)
     grid%mlai    (ids:ide-1,jds:jde-1,10) = emiss(ids:ide-1,jds:jde-1,mgnin_lai10)
     grid%mlai    (ids:ide-1,jds:jde-1,11) = emiss(ids:ide-1,jds:jde-1,mgnin_lai11)
     grid%mlai    (ids:ide-1,jds:jde-1,12) = emiss(ids:ide-1,jds:jde-1,mgnin_lai12)
     ! Plant functional group percentage
     grid%pftp_bt  (ids:ide-1,jds:jde-1) = emiss(ids:ide-1,jds:jde-1,mgnin_pftp_bt)
     grid%pftp_nt  (ids:ide-1,jds:jde-1) = emiss(ids:ide-1,jds:jde-1,mgnin_pftp_nt)
     grid%pftp_sb  (ids:ide-1,jds:jde-1) = emiss(ids:ide-1,jds:jde-1,mgnin_pftp_sb)
     grid%pftp_hb  (ids:ide-1,jds:jde-1) = emiss(ids:ide-1,jds:jde-1,mgnin_pftp_hb)
     ! "Climatological" monthly mean surface air temperature [=] K
     ! (Note: The height of surface air temperature is not considered important;
     !        this is not needed if online 24-hour average values are used
     grid%mtsa    (ids:ide-1,jds:jde-1,01) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa01)
     grid%mtsa    (ids:ide-1,jds:jde-1,02) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa02)
     grid%mtsa    (ids:ide-1,jds:jde-1,03) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa03)
     grid%mtsa    (ids:ide-1,jds:jde-1,04) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa04)
     grid%mtsa    (ids:ide-1,jds:jde-1,05) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa05)
     grid%mtsa    (ids:ide-1,jds:jde-1,06) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa06)
     grid%mtsa    (ids:ide-1,jds:jde-1,07) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa07)
     grid%mtsa    (ids:ide-1,jds:jde-1,08) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa08)
     grid%mtsa    (ids:ide-1,jds:jde-1,09) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa09)
     grid%mtsa    (ids:ide-1,jds:jde-1,10) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa10)
     grid%mtsa    (ids:ide-1,jds:jde-1,11) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa11)
     grid%mtsa    (ids:ide-1,jds:jde-1,12) = emiss(ids:ide-1,jds:jde-1,mgnin_tsa12)
     ! "Climatological" monthly mean downward irradiance at the surface [=] W/m2
     !  This is not needed if online 24-hour average values are used
     grid%mswdown (ids:ide-1,jds:jde-1,01) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown01)
     grid%mswdown (ids:ide-1,jds:jde-1,02) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown02)
     grid%mswdown (ids:ide-1,jds:jde-1,03) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown03)
     grid%mswdown (ids:ide-1,jds:jde-1,04) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown04)
     grid%mswdown (ids:ide-1,jds:jde-1,05) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown05)
     grid%mswdown (ids:ide-1,jds:jde-1,06) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown06)
     grid%mswdown (ids:ide-1,jds:jde-1,07) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown07)
     grid%mswdown (ids:ide-1,jds:jde-1,08) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown08)
     grid%mswdown (ids:ide-1,jds:jde-1,09) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown09)
     grid%mswdown (ids:ide-1,jds:jde-1,10) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown10)
     grid%mswdown (ids:ide-1,jds:jde-1,11) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown11)
     grid%mswdown (ids:ide-1,jds:jde-1,12) = emiss(ids:ide-1,jds:jde-1,mgnin_swdown12)




    DEALLOCATE( emiss )

  end SUBROUTINE input_ext_chem_megan2_file

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


END MODULE module_input_chem_bioemiss

