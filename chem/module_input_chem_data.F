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

MODULE module_input_chem_data

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
    USE module_data_sorgam
    USE module_utility
    USE module_get_file_names


   IMPLICIT NONE

!  REAL :: grav = 9.8104
      REAL mwso4
      PARAMETER (mwso4=96.0576)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Initial atmospheric chemistry profile data
    INTEGER :: k_loop           ! Used for loop index
    INTEGER :: lo              ! number of chemicals in inital profile
    INTEGER :: logg            ! number of final chemical species (nch-1)
    INTEGER :: kx              ! number of vertical levels in temp profile        
    INTEGER :: kxm1              

    PARAMETER( kx=16, kxm1=kx-1, logg=100, lo=34)
   
    INTEGER, DIMENSION(logg)                     :: iref

    REAL, DIMENSION(logg)                        :: fracref
    REAL, DIMENSION(kx)                          :: dens
    REAL, DIMENSION(kx+1)                        :: zfa 
    REAL, DIMENSION(lo  ,kx)                     :: xl
    REAL                                         :: so4vaptoaer
    DATA so4vaptoaer/.999/

    CHARACTER (LEN=4), DIMENSION(logg) :: ggnam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    DATA iref/12,19,2,2,1,3,4,9,8,5,5,32,6,6,6,30,30,10,26,13,11,6,6, &
               14,15,15,23,23,32,16,23,31,17,5*23,7,28,29,59*7/

    DATA fracref/1.,1.,.75,.25,6*1.,.5,.5,6.25E-4,7.5E-4,6.25E-5,.1, &
                 .9,4*1.,8.E-3,3*1.,.5,1.,1.,.5,12*1.,59*1./

    DATA ggnam/'SO2 ','SULF','NO2 ','NO  ','O3  ','HNO3',    &
                'H2O2','ALD ','HCHO','OP1 ','OP2 ','PAA ',    &
                'ORA1','ORA2','NH3 ','N2O5','NO3 ','PAN ',    &
                'HC3 ','HC5 ','HC8 ','ETH ','CO  ','OL2 ',    &
                'OLT ','OLI ','TOL ','XYL ','ACO3','TPAN',    &
                'HONO','HNO4','KET ','GLY ','MGLY','DCB ',    &
                'ONIT','CSL ','ISO ','HO  ','HO2 ',59*'JUNK' /

     DATA dens/ 2.738E+18, 5.220E+18, 7.427E+18, 9.202E+18, &
                1.109E+19, 1.313E+19, 1.525E+19, 1.736E+19, &
                1.926E+19, 2.074E+19, 2.188E+19, 2.279E+19, &
                2.342E+19, 2.384E+19, 2.414E+19, 2.434E+19  /

!     Profile heights in meters
      DATA ZFA/    0.,   85.,  212.,  385.,  603.,  960., 1430., 2010., &
                2850., 4010., 5340., 6900., 8510.,10200.,12100.,16000., &
               21000./

      DATA (xl(1,k_loop),k_loop=1,kx) &
      / 1.68E-07, 1.68E-07, 5.79E-08, 5.24E-08, 5.26E-08, &
       5.16E-08, 4.83E-08, 4.50E-08, 4.16E-08, 3.80E-08, 3.56E-08, &
       3.35E-08, 3.15E-08, 3.08E-08, 3.06E-08, 3.00E-08/  

      DATA (xl(2,k_loop),k_loop=1,kx) &
      / 4.06E-10, 4.06E-10, 2.16E-10, 1.37E-10, 9.47E-11, &
       6.95E-11, 5.31E-11, 4.19E-11, 3.46E-11, 3.01E-11, 2.71E-11, &
       2.50E-11, 2.35E-11, 2.26E-11, 2.20E-11, 2.16E-11/  

      DATA (xl(3,k_loop),k_loop=1,kx) &
      / 9.84E-10, 9.84E-10, 5.66E-10, 4.24E-10, 3.26E-10, &
       2.06E-10, 1.12E-10, 7.33E-11, 7.03E-11, 7.52E-11, 7.96E-11, &
       7.56E-11, 7.27E-11, 7.07E-11, 7.00E-11, 7.00E-11/

      DATA (xl(4,k_loop),k_loop=1,kx) &
      / 8.15E-10, 8.15E-10, 8.15E-10, 8.15E-10, 8.15E-10, &
       8.65E-10, 1.07E-09, 1.35E-09, 1.47E-09, 1.47E-09, 1.47E-09, &
       1.47E-09, 1.45E-09, 1.43E-09, 1.40E-09, 1.38E-09/

      DATA (xl(5,k_loop),k_loop=1,kx) &
      / 4.16E-10, 4.16E-10, 4.16E-10, 4.16E-10, 4.16E-10, &
       4.46E-10, 5.57E-10, 1.11E-09, 1.63E-09, 1.63E-09, 1.63E-09, &
       1.63E-09, 1.61E-09, 1.59E-09, 1.57E-09, 1.54E-09/

!  CO is 70 ppbv at top, 80 throughout troposphere
      DATA (xl(6,k_loop),k_loop=1,kx)  / 7.00E-08, kxm1*8.00E-08/

      DATA (xl(7,k_loop),k_loop=1,kx) &
      / 8.33E-29, 8.33E-29, 8.33E-29, 8.33E-29, 8.33E-29, &
       1.33E-28, 3.54E-28, 1.85E-28, 1.29E-29, 1.03E-30, 1.72E-31, &
       7.56E-32, 1.22E-31, 2.14E-31, 2.76E-31, 2.88E-31/

      DATA (xl(8,k_loop),k_loop=1,kx) &
      / 9.17E-11, 9.17E-11, 9.17E-11, 9.17E-11, 9.17E-11, &
       1.03E-10, 1.55E-10, 2.68E-10, 4.47E-10, 4.59E-10, 4.72E-10, &
       4.91E-10, 5.05E-10, 5.13E-10, 5.14E-10, 5.11E-10/
      DATA (xl(9,k_loop),k_loop=1,kx) &
      / 7.10E-12, 7.10E-12, 7.10E-12, 7.10E-12, 7.10E-12, &
       7.36E-12, 1.02E-11, 2.03E-11, 2.98E-11, 3.01E-11, 3.05E-11, &
       3.08E-11, 3.08E-11, 3.06E-11, 3.03E-11, 2.99E-11/
      DATA (xl(10,k_loop),k_loop=1,kx) &
      / 4.00E-11, 4.00E-11, 4.00E-11, 3.27E-11, 2.51E-11, &
       2.61E-11, 2.20E-11, 1.69E-11, 1.60E-11, 1.47E-11, 1.37E-11, &
       1.30E-11, 1.24E-11, 1.20E-11, 1.18E-11, 1.17E-11/
      DATA (xl(11,k_loop),k_loop=1,kx) &
      / 1.15E-16, 1.15E-16, 2.46E-15, 2.30E-14, 1.38E-13, &
       6.25E-13, 2.31E-12, 7.32E-12, 1.87E-11, 3.68E-11, 6.10E-11, &
       9.05E-11, 1.22E-10, 1.50E-10, 1.70E-10, 1.85E-10/
      DATA (xl(12,k_loop),k_loop=1,kx) &
      / 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, &
       1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, &
       1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10, 1.00E-10/
      DATA (xl(13,k_loop),k_loop=1,kx) &
      / 1.26E-11, 1.26E-11, 2.02E-11, 2.50E-11, 3.02E-11, &
       4.28E-11, 6.62E-11, 1.08E-10, 1.54E-10, 2.15E-10, 2.67E-10, &
       3.24E-10, 3.67E-10, 3.97E-10, 4.16E-10, 4.31E-10/
      DATA (xl(14,k_loop),k_loop=1,kx) &
      / 1.15E-16, 1.15E-16, 2.46E-15, 2.30E-14, 1.38E-13, &
       6.25E-13, 2.31E-12, 7.32E-12, 1.87E-11, 3.68E-11, 6.10E-11, &
       9.05E-11, 1.22E-10, 1.50E-10, 1.70E-10, 1.85E-10/
      DATA (xl(15,k_loop),k_loop=1,kx) &
      / 1.00E-20, 1.00E-20, 6.18E-20, 4.18E-18, 1.23E-16, &
       2.13E-15, 2.50E-14, 2.21E-13, 1.30E-12, 4.66E-12, 1.21E-11, &
       2.54E-11, 4.47E-11, 6.63E-11, 8.37E-11, 9.76E-11/
      DATA (xl(16,k_loop),k_loop=1,kx) &
      / 1.23E-11, 1.23E-11, 1.23E-11, 1.23E-11, 1.23E-11, &
       1.20E-11, 9.43E-12, 3.97E-12, 1.19E-12, 1.11E-12, 9.93E-13, &
       8.66E-13, 7.78E-13, 7.26E-13, 7.04E-13, 6.88E-13/
      DATA (xl(17,k_loop),k_loop=1,kx) &
      / 1.43E-12, 1.43E-12, 1.43E-12, 1.43E-12, 1.43E-12, &
       1.50E-12, 2.64E-12, 8.90E-12, 1.29E-11, 1.30E-11, 1.32E-11, &
       1.32E-11, 1.31E-11, 1.30E-11, 1.29E-11, 1.27E-11/
      DATA (xl(18,k_loop),k_loop=1,kx) &
       / 3.61E-13, 3.61E-13, 3.61E-13, 3.61E-13, 3.61E-13, &
       3.58E-13, 5.22E-13, 1.75E-12, 2.59E-12, 2.62E-12, 2.64E-12, &
       2.66E-12, 2.65E-12, 2.62E-12, 2.60E-12, 2.57E-12/
      DATA (xl(19,k_loop),k_loop=1,kx) &
       / 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, &
       5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, &
       5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11, 5.00E-11/

      DATA (xl(20,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(21,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(22,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(23,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(24,k_loop),k_loop=1,kx)/kx*1.E-20/
      DATA (xl(25,k_loop),k_loop=1,kx)/kx*1.E-20/

! Propane - Gregory PEM-West A 25 ppt median marine boundary layer
      DATA (xl(26,k_loop),k_loop=1,kx) &
      /5.00E-13, 1.24E-12, 2.21E-12, 3.27E-12, 4.71E-12, &
       6.64E-12, 9.06E-12, 1.19E-11, 1.47E-11, 1.72E-11, &
       1.93E-11, 2.11E-11, 2.24E-11, 2.34E-11, 2.42E-11, 2.48E-11/
! Acetylene - Gregory PEM-West A 53 ppt median marine boundary layer
      DATA (xl(27,k_loop),k_loop=1,kx) &
      /1.00E-12, 2.48E-12, 4.42E-12, 6.53E-12, 9.42E-12, &
       1.33E-11, 1.81E-11, 2.37E-11, 2.95E-11, 3.44E-11, &
       3.85E-11, 4.22E-11, 4.49E-11, 4.69E-11, 4.84E-11, 4.95E-11/
! OH
      DATA (xl(28,k_loop),k_loop=1,kx) &
       / 9.80E+06, 9.80E+06, 4.89E+06, 2.42E+06, 1.37E+06, &
       9.18E+05, 7.29E+05, 6.26E+05, 5.01E+05, 4.33E+05, 4.05E+05, &
       3.27E+05, 2.54E+05, 2.03E+05, 1.74E+05, 1.52E+05/
! HO2
      DATA (xl(29,k_loop),k_loop=1,kx) &
       / 5.74E+07, 5.74E+07, 7.42E+07, 8.38E+07, 8.87E+07, &
       9.76E+07, 1.15E+08, 1.34E+08, 1.46E+08, 1.44E+08, 1.40E+08, &
       1.36E+08, 1.31E+08, 1.28E+08, 1.26E+08, 1.26E+08/
! NO3+N2O5
      DATA (xl(30,k_loop),k_loop=1,kx) &
       / 5.52E+05, 5.52E+05, 3.04E+05, 2.68E+05, 2.32E+05, &
       1.66E+05, 1.57E+05, 1.72E+05, 1.98E+05, 2.22E+05, 2.43E+05, &
       2.75E+05, 3.00E+05, 3.18E+05, 3.32E+05, 3.39E+05/
! HO2NO2
      DATA (xl(31,k_loop),k_loop=1,kx) &
       / 7.25E+07, 7.25E+07, 6.36E+07, 5.55E+07, 4.94E+07, &
       3.66E+07, 2.01E+07, 9.57E+06, 4.75E+06, 2.37E+06, 1.62E+06, &
       9.86E+05, 7.05E+05, 5.63E+05, 4.86E+05, 4.41E+05/
! Sum of RO2 &
      DATA (xl(32,k_loop),k_loop=1,kx) &
       / 9.14E+06, 9.14E+06, 1.46E+07, 2.14E+07, 2.76E+07, &
       3.62E+07, 5.47E+07, 1.19E+08, 2.05E+08, 2.25E+08, 2.39E+08, &
       2.58E+08, 2.82E+08, 2.99E+08, 3.08E+08, 3.15E+08/
! O3
      DATA (xl(33,k_loop),k_loop=1,kx) &
       / 8.36E+11, 8.36E+11, 4.26E+11, 4.96E+11, 6.05E+11, &
       6.93E+11, 7.40E+11, 7.74E+11, 7.82E+11, 7.75E+11, 7.69E+11, &
       7.59E+11, 7.54E+11, 7.50E+11, 7.47E+11, 7.45E+11/
! NOx (NO+NO2)
      DATA (xl(34,k_loop),k_loop=1,kx) &
       / 1.94E+09, 1.94E+09, 1.53E+09, 1.24E+09, 1.04E+09, &
       8.96E+08, 7.94E+08, 7.11E+08, 6.44E+08, 6.00E+08, 5.70E+08, &
       5.49E+08, 5.35E+08, 5.28E+08, 5.24E+08, 5.23E+08/

CONTAINS

SUBROUTINE input_ext_chem_file (si_grid)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IMPLICIT NONE

   TYPE(domain)           ::  si_grid

   INTEGER :: i , j , k, l, &
              ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe    
   INTEGER :: si_jday
   INTEGER :: dat_jday,dat_year,dat_month,dat_day,dat_hour,dat_min,dat_sec
   INTEGER :: time_loop_max , time_loop
   INTEGER, DIMENSION(2) :: num_steps
   INTEGER :: fid, ierr, rc
   INTEGER :: status_next_var 
   INTEGER :: debug_level
   INTEGER :: si_year,si_month,si_day,si_hour,si_min,si_sec
   INTEGER :: total_time_sec , file_counter

   LOGICAL :: input_from_file , need_new_file

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: si_zsigf, si_zsig
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ch_zsigf, ch_zsig, ozone
   REAL    :: ext_time, dat_time
   REAL    :: wgt0,wgt_time1,wgt_time2

   CHARACTER (LEN=80) :: inpname, message
   CHARACTER (LEN=19) :: date_string
   CHARACTER (LEN=19) :: extract_date, use_date
   CHARACTER*80                           :: timestr

   TYPE (grid_config_rec_type)              :: config_flags
   TYPE(domain) , POINTER ::  null_domain, chem_grid, chgrid
   TYPE(domain) , POINTER ::  chem_grid2, chgrid2
   TYPE(WRF_UTIL_Time)                        :: CurrTime

   !  Interface block for routine that passes pointers and needs to know that they
   !  are receiving pointers.


     CALL model_to_grid_config_rec ( si_grid%id , model_config_rec , config_flags )
     CALL WRF_UTIL_ClockGet( si_grid%domain_clock, CurrTime=CurrTime, rc=ierr )

      !  After current_date has been set, fill in the julgmt stuff.

      CALL geth_julgmt ( config_flags%julyr , config_flags%julday , config_flags%gmt )

   WRITE ( extract_date , FMT = '(I4.4,"-",I2.2,"-",I2.2,"_",I2.2,":",I2.2,":",I2.2)' ) &
           model_config_rec%start_year  (si_grid%id) , &
           model_config_rec%start_month (si_grid%id) , &
           model_config_rec%start_day   (si_grid%id) , &
           model_config_rec%start_hour  (si_grid%id) , &
           model_config_rec%start_minute(si_grid%id) , &
           model_config_rec%start_second(si_grid%id)

   write(message,'(A,A)') 'Subroutine input_chem: finding data at date/time: ',extract_date
   CALL  wrf_message ( message )


   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1,debug_level )
!  CALL nl_set_wrf_debug_level ( 1,debug_level )
!  CALL nl_get_debug_level ( 1, debug_level )
   CALL set_wrf_debug_level ( debug_level )

   
   !  Allocated and configure the mother domain.  Since we are in the nesting down
   !  mode, we know a) we got a nest, and b) we only got 1 nest.

   NULLIFY( null_domain )

   CALL       wrf_debug ( 100 , 'wrfchem:input_chem: calling alloc_and_configure_domain 1' )
   CALL alloc_and_configure_domain ( domain_id  = 1           , &
                                     grid       = chem_grid   , &
                                     parent     = null_domain , &
                                     kid        = -1            )

   CALL       wrf_debug ( 100 , 'wrfchem:input_chem: set pointer for domain 1' )
   chgrid => chem_grid

   !  Get a list of available file names to try.  This fills up the eligible_file_name
   !  array with number_of_eligible_files entries.  This routine issues a nonstandard
   !  call (system).

   file_counter = 1
   need_new_file = .FALSE.
   CALL init_module_get_file_names
   CALL unix_ls ( 'chemin' , chem_grid%id )

   !  Open the input data (chemin_d01_000000) for reading.
   CALL wrf_debug          ( 100 , 'subroutine input_chem: calling open_r_dataset for wrfout' )
   CALL construct_filename ( inpname , 'chemin' , chgrid%id, 2 , 0 , 6 )
   write(message,'(A,A)') 'Subroutine input_chem: Opening data file ',TRIM(inpname)
   CALL  wrf_message ( message )
 
   CALL open_r_dataset     ( fid, TRIM(inpname) , chgrid , config_flags , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'subroutine chemin: error opening ',TRIM(inpname),' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF

   !  How many data time levels in the input file (chemin_d01_000000).

   num_steps = -1
   time_loop_max = 0
   CALL wrf_debug    ( 100, 'subroutine input_chem: find time_loop_max' )

   get_the_right_time : DO
!     CALL ext_ncd_get_next_time ( fid, date_string, status_next_var )
      CALL wrf_get_next_time ( fid, date_string, status_next_var )

      write(message,'(A,A)') 'Subroutine input_chem: chemistry data date: ',date_string
      CALL wrf_debug ( 100 , message )

      IF ( status_next_var == 0 ) THEN
         time_loop_max = time_loop_max + 1
            IF ( time_loop_max .GT. number_of_eligible_files ) THEN
               WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'program input_chem_data: opening too many files'
               CALL WRF_ERROR_FATAL ( wrf_err_message )
            END IF

            IF ( time_loop_max .EQ. number_of_eligible_files ) THEN
              num_steps(1) = time_loop_max
              num_steps(2) = time_loop_max+1
              use_date = date_string
              wgt_time1 = dat_time

              EXIT get_the_right_time
            END IF
      ELSE
!        ELSE IF ( TRIM(date_string) .LT. TRIM(current_date_char) ) THEN
!           CYCLE get_the_right_time
!        ELSE IF ( TRIM(date_string) .EQ. TRIM(current_date_char) ) THEN
!           EXIT get_the_right_time
!        ELSE IF ( TRIM(date_string) .GT. TRIM(current_date_char) ) THEN
!           WRITE( wrf_err_message , FMT='(A,A,A,A,A)' ) 'Found ',TRIM(date_string),' before I found ',TRIM(current_date_char),' .'
!           CALL WRF_ERROR_FATAL ( wrf_err_message )
!        END IF

!    For now, the input date and time MUST match 
!
!    Put the time check here and set num_steps

        num_steps(1) = time_loop_max
        num_steps(2) = time_loop_max+1
        use_date = date_string
        wgt_time1 = dat_time

        EXIT get_the_right_time
      ENDIF
      if( num_steps(2) == time_loop_max ) then
        wgt_time2 = dat_time
      endif
   END DO get_the_right_time

   num_steps(2) = MIN(num_steps(2),time_loop_max)

!  wgt0 = (ext_time  - wgt_time1) / (wgt_time2 - wgt_time1)
   wgt0 = 0.

! Make sure the right date and time for the chemin data has been found

   print *,'Subroutine input_chem: use_date, num_steps(1) = ',use_date,num_steps(1)
   if( num_steps(1) > 0 ) then
      write(message,'(A,A)') 'Subroutine input_chem: extracting data at date/time: ',use_date
      CALL  wrf_message ( message )
   else
      WRITE( wrf_err_message, FMT='(A)' ) 'subroutine input_chem: error finding chemin date/time #2'
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   endif

   !  There has to be a more elegant way to get to the beginning of the file, but this will do.

   CALL close_dataset      ( fid , config_flags , "DATASET=INPUT" )
   CALL open_r_dataset     ( fid, TRIM(inpname) , chgrid , config_flags , "DATASET=INPUT", ierr )
   IF ( ierr .NE. 0 ) THEN
      WRITE( wrf_err_message , FMT='(A,A,A,I8)' ) 'subroutine chemin: error opening ',TRIM(inpname),' for reading ierr=',ierr
      CALL WRF_ERROR_FATAL ( wrf_err_message )
   ENDIF

   !  We know how many time periods to process (right now - all of them), we have the input data
   !  (re-)opened, so we begin.

   big_time_loop_thingy : DO time_loop = 1 , time_loop_max

        CALL wrf_debug          ( 100 , 'input_chem: calling input_history' )
        CALL input_history      ( fid , chgrid , config_flags, ierr )
        CALL wrf_debug          ( 100 , 'input_chem: back from input_history' )

        IF( time_loop .EQ. num_steps(1) ) THEN

          ! Get grid dimensions
          CALL get_ijk_from_grid (  si_grid ,                        &
                                    ids, ide, jds, jde, kds, kde,    &
                                    ims, ime, jms, jme, kms, kme,    &
                                    ips, ipe, jps, jpe, kps, kpe    )

          ! Get scalar grid point heights
          ALLOCATE( si_zsigf(ims:ime,kms:kme,jms:jme) )
          ALLOCATE( ch_zsigf(ims:ime,kms:kme,jms:jme) )
          ALLOCATE(  si_zsig(ims:ime,kms:kme,jms:jme) )
          ALLOCATE(  ch_zsig(ims:ime,kms:kme,jms:jme) )

          si_zsigf = (si_grid%em_ph_1 + si_grid%em_phb) / grav
          ch_zsigf = ( chgrid%em_ph_1 +  chgrid%em_phb) / grav

          do k=1,kde-1
            si_zsig(:,k,:) = 0.5 * ( si_zsigf(:,k,:) + si_zsigf(:,k+1,:) ) 
            ch_zsig(:,k,:) = 0.5 * ( ch_zsigf(:,k,:) + ch_zsigf(:,k+1,:) ) 
          enddo
          si_zsig(:,kde,:) = 0.5 * ( 3. * si_zsigf(:,kde,:) - si_zsigf(:,kde-1,:) ) 
          ch_zsig(:,kde,:) = 0.5 * ( 3. * ch_zsigf(:,kde,:) - ch_zsigf(:,kde-1,:) ) 
         
      ! check size of si_grid vs chgrid

        IF( size(si_grid%chem,1) .NE. ime-ims+1 .OR.  &
            size(si_grid%chem,2) .NE. kme-kms+1 .OR.  &
            size(si_grid%chem,3) .NE. jme-jms+1 .OR.  &
            size(si_grid%chem,4) .NE. num_chem  ) then

          CALL wrf_debug (100, ' SI grid dimensions ' )
          write(message,'(4i8.8)') size(si_grid%chem,1),size(si_grid%chem,2), &
                                   size(si_grid%chem,3),size(si_grid%chem,4)
          CALL wrf_debug (100, message)
          CALL wrf_debug (100, ' Input data dimensions ' )
          write(message,'(4i8.8)') ime-ims+1,kme-kms+1,jme-jms+1,num_chem
          CALL wrf_debug (100, message)
          write(wrf_err_message,'(A)') 'ERROR IN MODULE_INPUT_CHEM: bad dimensions in input data '
          CALL WRF_ERROR_FATAL ( wrf_err_message )
        ENDIF

        ! Fill top level to prevent spurious interpolation results (no extrapolation)
        chgrid%chem(:,kde,:,:) = chgrid%chem(:,kde-1,:,:)

      ! Interpolate the chemistry data to the SI grid (holding place for time interpolation)

        call vinterp_chem(ims, ime, jms, jme, kms, kme, kme, num_chem, ch_zsig, si_zsig, &
                          chgrid%chem, si_grid%chem, .false.)

          if(wgt0 == 0.) EXIT big_time_loop_thingy
        ENDIF

         IF( time_loop .EQ. num_steps(2) ) THEN

!        ! input chemistry sigma levels
!         ch_zsigf = ( chgrid%em_ph_1 +  chgrid%em_phb) / grav
!         do k=1,kde-1
!           ch_zsig(:,k,:) = 0.5 * ( ch_zsigf(:,k,:) + ch_zsigf(:,k+1,:) ) 
!         enddo
!         ch_zsig(:,kde,:) = 0.5 * ( 3. * ch_zsigf(:,kde,:) - ch_zsigf(:,kde-1,:) ) 

!     ! Interpolate the chemistry data to the temp chgrid2 structure

!         call vinterp_chem(ims, ime, jms, jme, kms, kme, kme, num_chem, ch_zsig, si_zsig, &
!                           chgrid%chem, chgrid2%chem, .false.)

!     ! use linear interpolation in time to get new chem arrays
!         si_grid%chem = (1. - wgt0) * si_grid%chem + &
!                                wgt0  * chgrid2%chem 

          !ALLOCATE(  ozone(ims:ime,kms:kme,jms:jme) )
          !ozone = chgrid2%chem(:,:,:,p_o3)
          !!print *, '     GRID2'
          !!print *,ozone(:,1,:)
          !write(15,*) ids,ide,jds,jde,kds,kde
          !do j=jds,jde
          !do k=kds,kde
          !do i=ids,ide
          !  write(15,*) ozone(i,k,j)
          !enddo
          !enddo
          !enddo
          !DEALLOCATE( ozone)

          DEALLOCATE( si_zsigf); DEALLOCATE( si_zsig )
          DEALLOCATE( ch_zsigf); DEALLOCATE( ch_zsig )

          EXIT big_time_loop_thingy
        ENDIF
   END DO big_time_loop_thingy

!  Check for errors in chemin data set

   do l=2,num_chem
   do j=jds,jde
   do k=kds,kde
   do i=ids,ide
     si_grid%chem(i,k,j,l) = MAX(si_grid%chem(i,k,j,l),epsilc)
   enddo
   enddo
   enddo
   enddo
   

!  Close chemin data set
   CALL close_dataset ( fid , config_flags , "DATASET=INPUT" )

   CALL wrf_debug ( 100,' input_ext_chem_data: exit subroutine ')

   RETURN

  END SUBROUTINE input_ext_chem_file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE vinterp_chem(nx1, nx2, ny1, ny2, nz1, nz_in, nz_out, nch, z_in, z_out, &
                 data_in, data_out, extrapolate)

    ! Interpolates columns of chemistry data from one set of height surfaces to
    ! another
 
    INTEGER, INTENT(IN)                :: nx1, nx2
    INTEGER, INTENT(IN)                :: ny1, ny2
    INTEGER, INTENT(IN)                :: nz1
    INTEGER, INTENT(IN)                :: nz_in
    INTEGER, INTENT(IN)                :: nz_out
    INTEGER, INTENT(IN)                :: nch
    REAL, INTENT(IN)                   :: z_in (nx1:nx2,nz1:nz_in ,ny1:ny2)
    REAL, INTENT(IN)                   :: z_out(nx1:nx2,nz1:nz_out,ny1:ny2)
    REAL, INTENT(IN)                   :: data_in (nx1:nx2,nz1:nz_in ,ny1:ny2,nch)
    REAL, INTENT(OUT)                  :: data_out(nx1:nx2,nz1:nz_out,ny1:ny2,nch)
    LOGICAL, INTENT(IN)                :: extrapolate

    INTEGER                            :: i,j,l
    INTEGER                            :: k,kk
    REAL                               :: desired_z
    REAL                               :: dvaldz
    REAL                               :: wgt0
  
!   Loop over the number of chemical species
    chem_loop: DO l = 2, nch

      data_out(:,:,:,l) = -99999.9

      DO j = ny1, ny2
        DO i = nx1, nx2

          output_loop: DO k = nz1, nz_out

            desired_z = z_out(i,k,j)
            IF (desired_z .LT. z_in(i,1,j)) THEN

              IF ((desired_z - z_in(i,1,j)).LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,1,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate downward because desired height level is below
                  ! the lowest level in our input data.  Extrapolate using simple
                  ! 1st derivative of value with respect to height for the bottom 2
                  ! input layers.
  
                  ! Add a check to make sure we are not using the gradient of 
                  ! a very thin layer
  
                  IF ( (z_in(i,1,j) - z_in(i,2,j)) .GT. 0.001) THEN
                    dvaldz = (data_in(i,1,j,l) - data_in(i,2,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,2,j) )
                  ELSE
                    dvaldz = (data_in(i,1,j,l) - data_in(i,3,j,l)) / &
                              (z_in(i,1,j)  - z_in(i,3,j) )
                  ENDIF
                  data_out(i,k,j,l) = MAX( data_in(i,1,j,l) + &
                                dvaldz * (desired_z-z_in(i,1,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,1,j,l)
                ENDIF
              ENDIF
            ELSE IF (desired_z .GT. z_in(i,nz_in,j)) THEN
              IF ( (z_in(i,nz_in,j) - desired_z) .LT. 0.0001) THEN
                 data_out(i,k,j,l) = data_in(i,nz_in,j,l)
              ELSE
                IF (extrapolate) THEN
                  ! Extrapolate upward
                  IF ( (z_in(i,nz_in-1,j)-z_in(i,nz_in,j)) .GT. 0.0005) THEN
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-1,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-1,j))
                  ELSE
                    dvaldz = (data_in(i,nz_in,j,l) - data_in(i,nz_in-2,j,l)) / &
                               (z_in(i,nz_in,j)  - z_in(i,nz_in-2,j)) 
                  ENDIF
                  data_out(i,k,j,l) =  MAX( data_in(i,nz_in,j,l) + &
                           dvaldz * (desired_z-z_in(i,nz_in,j)), 0.)
                ELSE
                  data_out(i,k,j,l) = data_in(i,nz_in,j,l)
                ENDIF
              ENDIF
            ELSE
              ! We can trap between two levels and linearly interpolate
  
              input_loop:  DO kk = 1, nz_in-1
                IF (desired_z .EQ. z_in(i,kk,j) )THEN
                  data_out(i,k,j,l) = data_in(i,kk,j,l)
                  EXIT input_loop
                ELSE IF ( (desired_z .GT. z_in(i,kk,j)) .AND. &
                          (desired_z .LT. z_in(i,kk+1,j)) ) THEN
                  wgt0 = (desired_z - z_in(i,kk+1,j)) / &
                         (z_in(i,kk,j)-z_in(i,kk+1,j))
                  data_out(i,k,j,l) = MAX( wgt0*data_in(i,kk,j,l) + &
                                    (1.-wgt0)*data_in(i,kk+1,j,l), 0.)
                  EXIT input_loop
                ENDIF        
              ENDDO input_loop

            ENDIF
          ENDDO output_loop
        ENDDO 
      ENDDO 
    ENDDO chem_loop

    RETURN
  END SUBROUTINE vinterp_chem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE input_chem_profile (si_grid)

   IMPLICIT NONE

   TYPE(domain)           ::  si_grid

   INTEGER :: i , j , k, &
              ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe    
   INTEGER :: fid, ierr
   INTEGER :: debug_level

   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: si_zsigf, si_zsig

   CHARACTER (LEN=80) :: inpname, message
   TYPE (grid_config_rec_type)  :: config_flags

     write(message,'(A)') 'Subroutine input_chem_profile: '
     CALL  wrf_message ( message )

   !  And here is an instance of using the information in the NAMELIST.  

   CALL nl_get_debug_level ( 1,debug_level )
   CALL set_wrf_debug_level ( debug_level )

!  CALL nl_set_wrf_debug_level ( 1,debug_level )
   
   ! Get grid dimensions
   CALL get_ijk_from_grid (  si_grid ,                        &
                             ids, ide, jds, jde, kds, kde,    &
                             ims, ime, jms, jme, kms, kme,    &
                             ips, ipe, jps, jpe, kps, kpe    )

   ! Get scalar grid point heights
   ALLOCATE( si_zsigf(ims:ime,kms:kme,jms:jme) )
   ALLOCATE(  si_zsig(ims:ime,kms:kme,jms:jme) )

   si_zsigf = (si_grid%em_ph_1 + si_grid%em_phb) / grav

!  print *,' SI_ZSIGF: ',ids,ide,ims,ime
!  do i=ims,ids-1
!   print *,i
!   si_zsigf(i,k,j) = si_zsigf(1,k,j)
!  enddo
!  do i=ide+1,ime
!   print *,i
!   si_zsigf(i,k,j) = si_zsigf(ide,k,j)
!  enddo
! !do i=ims,ime
! ! print *,i,si_zsigf(i,1,5)
! !enddo
!  do k=kms,kme
!   print *,k,si_zsigf(10,k,5)
!  enddo
!  stop

   do k=1,kde-1
     si_zsig(:,k,:) = 0.5 * ( si_zsigf(:,k,:) + si_zsigf(:,k+1,:) ) 
   enddo
   si_zsig(:,kde,:) = 0.5 * ( 3. * si_zsigf(:,kde,:) - si_zsigf(:,kde-1,:) ) 

   ! Interpolate the chemistry data to the SI grid


   CALL make_chem_profile (ims, ime, jms, jme, kms, kme, num_chem, &
                           si_zsig, si_grid%chem)
   DO k=kms,kme
     print *,k,si_grid%chem(10,k,10,6)
   ENDDO

   CALL wrf_debug       ( 100,' input_chem_profile: exit subroutine ')

   DEALLOCATE( si_zsigf ); DEALLOCATE( si_zsig )
   RETURN

  END SUBROUTINE input_chem_profile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE make_chem_profile ( nx1, nx2, ny1, ny2, nz1, nz2, nch,  &
                                  zgrid, chem )
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: nx1, ny1, nz1
    INTEGER, INTENT(IN) :: nx2, ny2, nz2
    INTEGER, INTENT(IN) :: nch
    !REAL, INTENT(IN), DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2) :: zgrid

    CHARACTER (LEN=80) :: message
    INTEGER :: i, j, k, l, is

    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2,lo+1):: chprof
    REAL, DIMENSION(nx1:nx2,nz1:kx ,ny1:ny2)     :: zprof

    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,nch) :: chem
    REAL, DIMENSION(nx1:nx2,nz1:nz2,ny1:ny2,lo ) :: stor
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Check the number of vertical levels

     if( nch .NE. num_chem) then
       message = ' Input_chem_profile: wrong number of chemical species'
!       CALL WRF_ERROR_FATAL ( message )
     endif
       
! Fix number concentrations to mixing ratios for short-lived NALROM species
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)/dens(1:kx)
      ENDDO

      ! Vertically flip the chemistry data as it is given top down and heights are bottom up
      !
      ! Fill temp 3D chemical and profile array
      ! keep chem slot 1 open as vinterp_chem assumes there is no data
      DO j=ny1,ny2
      DO k=  1,kx 
      DO i=nx1,nx2 
        chprof(i,k,j,2:lo+1) = xl(1:lo,kx-k+1)
        zprof(i,k,j) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
      ENDDO
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
      ENDDO

        ! Interpolate temp 3D chemical and profile array to WRF grid
      call vinterp_chem(nx1, nx2, ny1, ny2, nz1, kx, nz2, lo, zprof, zgrid, &
                          chprof, chem, .false.)

      ! place interpolated data into temp storage array
      stor(nx1:nx2,nz1:nz2,ny1:ny2,1:lo) = chem(nx1:nx2,nz1:nz2,ny1:ny2,2:lo+1)

      ! Here is where the chemistry profile is constructed
      !chem(:,:,:,1) = stor(:,:,:,1) * 0.
      chem(nx1:nx2,nz1:nz2,ny1:ny2,1) = -999.

!      DO l=2,nch
      DO l=2,p_ho2
        is=iref(l-1)
        DO j=ny1,ny2
        DO k=nz1,nz2
        DO i=nx1,nx2 
          chem(i,k,j,l)=fracref(l-1)*stor(i,k,j,is)*1.E6
        ENDDO
        ENDDO
        ENDDO
!         if(l == 5 ) then
!           DO k=nz1,nz2
!             print *,k,l,chem(10,k,10,l),fracref(l-1),stor(10,k,10,iref(l-1))
!           ENDDO
!         endif
      ENDDO

      RETURN
  END SUBROUTINE make_chem_profile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! this is a kludge routine as of now....
!
  SUBROUTINE bdy_chem_value_sorgam (chem, z, nch, &
                                      alt,t,pb,p,t0,p1000mb,rcp,ph,phb,g)
  USE module_data_sorgam
  USE module_aerosols_sorgam
    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch        ! index number of chemical species
   REAL,  INTENT(IN   ) ::                                           &
                               ph,phb,t,pb,p,alt
   real, INTENT (IN) :: g,rcp,t0,p1000mb
    INTEGER :: i, k, is
    REAL, DIMENSION(lo+1,1:kx):: cprof  ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL, DIMENSION(lo ) :: stor
    REAL                 :: wgt0

    real :: chemsulf_radm,chem_so4aj,chem_so4ai
     real convfac,tempfac
      REAL :: splitfac
                        !between gas and aerosol phase

!factor for splitting initial conc. of SO4
!3rd moment i-mode [3rd moment/m^3]
      REAL :: m3nuc
!3rd MOMENT j-mode [3rd moment/m^3]
      REAL :: m3acc
!       REAL ESN36
      REAL :: m3cor
      DATA splitfac/.98/

       chem=conmin
       tempfac=(t+t0)*((p+pb)/p1000mb)**rcp
       convfac=(p+pb)/rgasuniv/tempfac
!
!--- units for advection....
!
       if(nch.eq.p_nu0)chem=1.e8*alt
       if(nch.eq.p_ac0)chem=1.e8*alt
       if(nch.eq.p_nh4aj)chem=10.e-5*alt
       if(nch.eq.p_nh4ai)chem=10.e-5*alt
       if(nch.eq.p_no3aj)chem=10.e-5*alt
       if(nch.eq.p_no3ai)chem=10.e-5*alt
!
! recalculate sulf profile for aerosols
!
     if   ( nch .eq. p_so4aj.or.nch.eq.p_so4ai                        &
        .or.nch .eq. p_nu0  .or.nch.eq.p_ac0                          &
        .or.nch .eq. p_corn                    ) then
!
! Fix number concentrations to mixing ratios for short-lived NALROM species
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)/dens(1:kx)
      ENDDO

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill 3D chemical profile array cprof
      ! keep chem slot 1 open as vinterp_chem assumes there is no data
      DO k=  1,kx 
        cprof(2:lo+1,k) = xl(1:lo,kx+1-k)
        zprof(k) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
      ENDDO

      ! Interpolate temp 3D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor(1:lo) = cprof(2:lo+1,1) 
      ELSE IF (z .GT. zprof(kx)) THEN
        stor(1:lo) = cprof(2:lo+1,kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor(1:lo) = cprof(2:lo+1,k)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor(1:lo) = MAX( wgt0 *cprof(2:lo+1,k  ) + &
                          (1.-wgt0)*cprof(2:lo+1,k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chemsulf_radm = fracref(p_sulf-1)*stor( iref(p_sulf-1) )*1.E6
!
! now have sulf
!
       chem_so4aj=chemsulf_radm*CONVFAC*MWSO4*splitfac*so4vaptoaer
       chem_so4ai=chemsulf_radm*CONVFAC*MWSO4*(1.-splitfac)*so4vaptoaer
!      chem_so4aj=chemsulf_radm*MWSO4*splitfac*so4vaptoaer
!      chem_so4ai=chemsulf_radm*MWSO4*(1.-splitfac)*so4vaptoaer
       if(nch.eq.p_so4aj)chem=chem_so4aj*alt
       if(nch.eq.p_so4ai)chem=chem_so4ai*alt
       m3nuc=so4fac*chem_so4ai+conmin*(nh4fac+no3fac+orgfac*9+2*anthfac)
       m3acc=so4fac*chem_so4aj+conmin*(nh4fac+no3fac+orgfac*9+2*anthfac)
       m3cor=conmin*(soilfac+seasfac+anthfac)
!
! compute values for aerosol input data
!
       if(nch.eq.p_nu0.or.nch.eq.p_ac0.or.nch.eq.p_corn)then
         xxlsgn = log(sginin)
        xxlsga = log(sginia)
        xxlsgc = log(sginic)

        l2sginin = xxlsgn**2
        l2sginia = xxlsga**2
        l2sginic = xxlsgc**2

        en1 = exp(0.125*l2sginin)
        ea1 = exp(0.125*l2sginia)
        ec1 = exp(0.125*l2sginic)


        esn04 = en1**4
        esa04 = ea1**4
        esc04 = ec1**4

        esn05 = esn04*en1
        esa05 = esa04*ea1

        esn08 = esn04*esn04
        esa08 = esa04*esa04
        esc08 = esc04*esc04

        esn09 = esn04*esn05
        esa09 = esa04*esa05

        esn12 = esn04*esn04*esn04
        esa12 = esa04*esa04*esa04
        esc12 = esc04*esc04*esc04

        esn16 = esn08*esn08
        esa16 = esa08*esa08
        esc16 = esc08*esc08

        esn20 = esn16*esn04
        esa20 = esa16*esa04
        esc20 = esc16*esc04

        esn24 = esn12*esn12
        esa24 = esa12*esa12
        esc24 = esc12*esc12

        esn25 = esn16*esn09
        esa25 = esa16*esa09

        esn28 = esn20*esn08
        esa28 = esa20*esa08
        esc28 = esc20*esc08


        esn32 = esn16*esn16
        esa32 = esa16*esa16
        esc32 = esc16*esc16

        esn36 = esn16*esn20
        esa36 = esa16*esa20
        esc36 = esc16*esc20
       endif
!
! Units are something like number concentration
!
       if(nch.eq.p_nu0)chem=m3nuc/((dginin**3)*esn36)*alt
       if(nch.eq.p_ac0)chem=m3acc/((dginia**3)*esa36)*alt
       if(nch.eq.p_corn)chem=m3cor/((dginic**3)*esc36)*alt
     endif

   
  END SUBROUTINE bdy_chem_value_sorgam

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE bdy_chem_value ( chem, z, nch)
                                  
    IMPLICIT NONE

    REAL,    intent(OUT)  :: chem
    REAL,    intent(IN)   :: z          ! 3D height array
    INTEGER, intent(IN)   :: nch        ! index number of chemical species


    INTEGER :: i, k, is

    REAL, DIMENSION(lo+1,1:kx):: cprof  ! chemical profile, diff. index order

    REAL, DIMENSION(1:kx):: zprof
    REAL, DIMENSION(lo ) :: stor
    REAL                 :: wgt0

    CHARACTER (LEN=80) :: message
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Check the number of vertical levels
!     if((nch-1).gt.logg)return
!     if((nch-1).gt.p_ho2-1)return

!    if( nch .GT. logg+1) then
     if( nch .GT. p_ho2) then
       return
       message = ' Input_chem_profile: wrong number of chemical species'
!       CALL WRF_ERROR_FATAL ( message )
     endif
!
! Fix number concentrations to mixing ratios for short-lived NALROM species
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)/dens(1:kx)
      ENDDO

      ! Vertically flip the chemistry data as it is given top down 
      !     and heights in zfa are bottom up
      ! Fill 3D chemical profile array cprof
      ! keep chem slot 1 open as vinterp_chem assumes there is no data
      DO k=  1,kx 
        cprof(2:lo+1,k) = xl(1:lo,kx+1-k)
        zprof(k) = 0.5*(zfa(k)+zfa(k+1))
      ENDDO
!
! return xl to previous value for next time... 
!   34 chemicals (lo), 16 vertical levels (kx)
      DO i=lo-6,lo               
        xl(i,1:kx)=xl(i,1:kx)*dens(1:kx)
      ENDDO

      ! Interpolate temp 3D chemical profile array to WRF field
      IF (z .LT. zprof(1)) THEN 
        stor(1:lo) = cprof(2:lo+1,1) 
      ELSE IF (z .GT. zprof(kx)) THEN
        stor(1:lo) = cprof(2:lo+1,kx)
      ELSE
        ! We can trap between two levels and linearly interpolate
        input_loop:  DO k = 1, kx-1
          IF (z .EQ. zprof(k) )THEN 
            stor(1:lo) = cprof(2:lo+1,k)
            EXIT input_loop
          ELSE IF ( (z .GT. zprof(k)) .AND. &
                    (z .LT. zprof(k+1)) ) THEN
            wgt0 = (z   - zprof(k+1)) / &
                   (zprof(k) - zprof(k+1))
            stor(1:lo) = MAX( wgt0 *cprof(2:lo+1,k  ) + &
                          (1.-wgt0)*cprof(2:lo+1,k+1), 0.)
            EXIT input_loop
          ENDIF  
        ENDDO input_loop
      ENDIF 

      ! Here is where the chemistry value is constructed
      chem = fracref(nch-1)*stor( iref(nch-1) )*1.E6
      if(nch.eq.p_sulf.and.p_nu0.gt.1)then
        chem=chem*(1.-so4vaptoaer)
      endif

      RETURN
  END SUBROUTINE bdy_chem_value
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   SUBROUTINE flow_dep_bdy_chem  (  chem, z,                 &
                               u, v, config_flags,alt, & 
                                t,pb,p,t0,p1000mb,rcp,ph,phb,g, &
                               spec_zone, ic,           &
                               ids,ide, jds,jde, kds,kde,  & ! domain dims
                               ims,ime, jms,jme, kms,kme,  & ! memory dims
                               ips,ipe, jps,jpe, kps,kpe,  & ! patch  dims
                               its,ite, jts,jte, kts,kte )

!  This subroutine sets zero gradient conditions for outflow and zero value
!  for inflow in the boundary specified region. Note that field must be unstaggered.
!  The velocities, u and v, will only be used to check their sign (coupled vels OK)
!  spec_zone is the width of the outer specified b.c.s that are set here.
!  (JD August 2000)

      IMPLICIT NONE

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: ips,ipe, jps,jpe, kps,kpe
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone, ic


      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(INOUT) :: chem
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: z
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: alt
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: u
      REAL,  DIMENSION( ims:ime , kms:kme , jms:jme ), INTENT(IN   ) :: v
   REAL,  DIMENSION( ims:ime , kms:kme , jms:jme )         ,         &
          INTENT(IN   ) ::                                           &
                               ph,phb,t,pb,p
   real, INTENT (IN) :: g,rcp,t0,p1000mb
      TYPE( grid_config_rec_type ) config_flags

      INTEGER    :: i, j, k 
      INTEGER    :: ibs, ibe, jbs, jbe, itf, jtf, ktf
      INTEGER    :: i_inner, j_inner
      INTEGER    :: b_dist
      integer    :: itestbc
      itestbc=0
      if(p_nu0.gt.1)itestbc=1
      ibs = ids
      ibe = ide-1
      itf = min(ite,ide-1)
      jbs = jds
      jbe = jde-1
      jtf = min(jte,jde-1)
      ktf = kde-1

      IF (jts - jbs .lt. spec_zone) THEN
! Y-start boundary
        DO j = jts, min(jtf,jbs+spec_zone-1)
          b_dist = j - jbs
          DO k = kts, ktf
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbs+spec_zone)
              ELSE
                CALL bdy_chem_value (chem(i,k,j), z(i,k,j), ic)
!               if(itestbc.eq.1.and.ic.gt.logg+1)THEN
                if(itestbc.eq.1.and.ic.gt.p_ho2)THEN
                   CALL bdy_chem_value_sorgam (chem(i,k,j), z(i,k,j), ic, &
                    alt(i,k,j),t(i,k,j),pb(i,k,j),p(i,k,j),t0,p1000mb,rcp,&
                        ph(i,k,j),phb(i,k,j),g)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (jbe - jtf .lt. spec_zone) THEN 
! Y-end boundary 
        DO j = max(jts,jbe-spec_zone+1), jtf 
          b_dist = jbe - j 
          DO k = kts, ktf 
            DO i = max(its,b_dist+ibs), min(itf,ibe-b_dist)
              i_inner = max(i,ibs+spec_zone)
              i_inner = min(i_inner,ibe-spec_zone)
              IF(v(i,k,j+1) .gt. 0.)THEN
                chem(i,k,j) = chem(i_inner,k,jbe-spec_zone)
              ELSE
                CALL bdy_chem_value (chem(i,k,j), z(i,k,j), ic)
!               if(itestbc.eq.1.and.ic.gt.logg+1)THEN
                if(itestbc.eq.1.and.ic.gt.p_ho2)THEN
                   CALL bdy_chem_value_sorgam (chem(i,k,j), z(i,k,j), ic, &
                    alt(i,k,j),t(i,k,j),pb(i,k,j),p(i,k,j),t0,p1000mb,rcp,        &
                        ph(i,k,j),phb(i,k,j),g)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (its - ibs .lt. spec_zone) THEN
! X-start boundary
        DO i = its, min(itf,ibs+spec_zone-1)
          b_dist = i - ibs
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i,k,j) .lt. 0.)THEN
                chem(i,k,j) = chem(ibs+spec_zone,k,j_inner)
              ELSE
                CALL bdy_chem_value (chem(i,k,j), z(i,k,j), ic)
!               if(itestbc.eq.1.and.ic.gt.logg+1)THEN
                if(itestbc.eq.1.and.ic.gt.p_ho2)THEN
                   CALL bdy_chem_value_sorgam (chem(i,k,j), z(i,k,j), ic, &
                       alt(i,k,j),t(i,k,j),pb(i,k,j),p(i,k,j),t0,p1000mb,rcp,        &
                        ph(i,k,j),phb(i,k,j),g)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
      IF (ibe - itf .lt. spec_zone) THEN
! X-end boundary
        DO i = max(its,ibe-spec_zone+1), itf
          b_dist = ibe - i
          DO k = kts, ktf
            DO j = max(jts,b_dist+jbs+1), min(jtf,jbe-b_dist-1)
              j_inner = max(j,jbs+spec_zone)
              j_inner = min(j_inner,jbe-spec_zone)
              IF(u(i+1,k,j) .gt. 0.)THEN
                chem(i,k,j) = chem(ibe-spec_zone,k,j_inner)
              ELSE
                CALL bdy_chem_value (chem(i,k,j), z(i,k,j), ic)
!               if(itestbc.eq.1.and.ic.gt.logg+1)THEN
                if(itestbc.eq.1.and.ic.gt.p_ho2)THEN
                   CALL bdy_chem_value_sorgam (chem(i,k,j), z(i,k,j), ic, &
                       alt(i,k,j),t(i,k,j),pb(i,k,j),p(i,k,j),t0,p1000mb,rcp,        &
                        ph(i,k,j),phb(i,k,j),g)
                endif
              ENDIF
            ENDDO
          ENDDO
        ENDDO
      ENDIF 
   END SUBROUTINE flow_dep_bdy_chem
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   SUBROUTINE cv_mmdd_jday ( YY, MM, DD, JDAY)
!
!     Subroutine to compute the julian day given the month and day
!
!
      INTEGER,      INTENT(IN )    :: YY, MM, DD
      INTEGER,      INTENT(OUT)    :: JDAY

      INTEGER, DIMENSION(12) :: imon, imon_a
      INTEGER                :: i

      DATA imon_a /0,31,59,90,120,151,181,212,243,273,304,334/
!
!..... Check for leap year.
!
      do i=1,12
         imon(i) = imon_a(i)
      enddo 
      if(YY .eq. (YY/4)*4) then
         do i=3,12
            imon(i) = imon(i) + 1
         enddo 
      endif
!
!..... Convert month, day to julian day.
!
      jday = imon(mm) + dd


   END SUBROUTINE cv_mmdd_jday
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE module_input_chem_data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
