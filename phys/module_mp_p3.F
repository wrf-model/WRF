!__________________________________________________________________________________________
! This module contains the Predicted Particle Property (P3) bulk microphysics scheme.      !
!                                                                                          !
! This code was originally written by H. Morrison, MMM Division, NCAR (Dec 2012).          !
! Modifications were made by J. Milbrandt, RPN, Environment Canada (July 2014).            !
! Subsequent major and minor upgrades have been ongoing.                                   !
!                                                                                          !
! For model-specific aspects/versions, see comments in the interface subroutine(s) in      !
!   this module (mp_p3_wrapper_wrf, mp_p3_wrapper_gem).                                    !
!                                                                                          !
! For details see:                                                                         !
!   Morrison and Milbrandt (2015) [J. Atmos. Sci., 72, 287-311]   - original scheme desc.  !
!   Milbrandt and Morrison (2016) [J. Atmos. Sci., 73, 975-995]   - multi-ice-category     !
!   Jouan et al. (2020)           [W. Forecasting, 35, 2541-2565] - cloud fraction         !
!   Milbrandt et al. (2021)       [J. Atmos. Sci., 78, 439-458]   - triple-moment ice      !
!                                                                                          !
! For questions or bug reports, please contact:                                            !
!    Hugh Morrison    (morrison@ucar.edu), or                                              !
!    Jason Milbrandt  (jason.milbrandt@ec.gc.ca), or                                       !
!    Melissa Cholette (melissa.cholette@ec.gc.ca)                                          !
!__________________________________________________________________________________________!
!                                                                                          !
! Version:       4.5.2                                                                     !
! Last updated:  2023-FEB                                                                  !
!__________________________________________________________________________________________!

 MODULE microphy_p3

#ifdef ECCCGEM
 use tdpack, only: foew, foewa
#endif

 implicit none

 private
 public :: p3_main, polysvp1, p3_init
#ifdef ECCCGEM
 public :: mp_p3_wrapper_gem, p3_phybusinit, p3_lwc, p3_iwc
#else
 public :: mp_p3_wrapper_wrf, mp_p3_wrapper_wrf_2cat
#endif

 integer, parameter, public :: STATUS_ERROR  = -1
 integer, parameter, public :: STATUS_OK     = 0
 integer, save              :: global_status = STATUS_OK

! ice microphysics lookup table array dimensions
 integer, parameter :: isize        = 50
 integer, parameter :: iisize       = 25
 integer, parameter :: zsize        = 11  ! size of mu_i array in lookup_table (for 3-moment ice)
 integer, parameter :: densize      =  5
 integer, parameter :: rimsize      =  4
 integer, parameter :: rcollsize    = 30
 integer, parameter :: tabsize      = 14  ! number of quantities used from lookup table
 integer, parameter :: tabsize_3mom = 15  ! number of quantities used from 3-mom lookup table
 integer, parameter :: colltabsize  =  2  ! number of ice-rain collection  quantities used from lookup table
 integer, parameter :: collitabsize =  2  ! number of ice-ice collection  quantities used from lookup table

 real, parameter    :: real_rcollsize = real(rcollsize)

 ! NOTE: TO DO, MAKE LOOKUP TABLE ARRAYS ALLOCATABLE SO BOTH 2-MOMENT AND 3-MOMENT NOT ALLOCATED
 real, dimension(densize,rimsize,isize,tabsize)                     :: itab        !ice lookup table values
 real, dimension(zsize,densize,rimsize,isize,tabsize_3mom)          :: itab_3mom   !ice lookup table values

!ice lookup table values for ice-rain collision/collection
 real, dimension(densize,rimsize,isize,rcollsize,colltabsize)       :: itabcoll
 real, dimension(zsize,densize,rimsize,isize,rcollsize,colltabsize) :: itabcoll_3mom

 ! NOTE: TO DO, MAKE LOOKUP TABLE ARRAYS ALLOCATABLE SO MULTICAT NOT ALLOCATED WHEN NCAT = 1
! separated into itabcolli1 and itabcolli2, due to max of 7 dimensional arrays on some FORTRAN compilers
 real, dimension(iisize,rimsize,densize,iisize,rimsize,densize)     :: itabcolli1
 real, dimension(iisize,rimsize,densize,iisize,rimsize,densize)     :: itabcolli2

! integer switch for warm rain autoconversion/accretion schemes
 integer :: iparam

! number of diagnostic ice-phase hydrometeor types
 integer, public, parameter :: n_qiType = 6

! droplet spectral shape parameter for mass spectra, used for Seifert and Beheng (2001)
! warm rain autoconversion/accretion option only (iparam = 1)
 real, dimension(16) :: dnu

! lookup table values for rain shape parameter mu_r
 real, dimension(150) :: mu_r_table

! lookup table values for rain number- and mass-weighted fallspeeds and ventilation parameters
 real, dimension(300,10) :: vn_table,vm_table,revap_table

 real, parameter :: mu_i_max = 20.

 ! physical and mathematical constants
 real           :: rhosur,rhosui,ar,br,f1r,f2r,ecr,rhow,kr,kc,bimm,aimm,rin,mi0,nccnst,  &
                   eci,eri,bcn,cpw,e0,cons1,cons2,cons3,cons4,cons5,cons6,cons7,cons8,   &
                   inv_rhow,qsmall,nsmall,bsmall,zsmall,cp,g,rd,rv,ep_2,inv_cp,mw,osm,   &
                   vi,epsm,rhoa,map,ma,rr,bact,inv_rm1,inv_rm2,sig1,nanew1,f11,f21,sig2, &
                   nanew2,f12,f22,pi,thrd,sxth,piov3,piov6,rho_rimeMin,                  &
                   rho_rimeMax,inv_rho_rimeMax,max_total_Ni,dbrk,nmltratio,minVIS,       &
                   maxVIS,mu_i_initial,mu_r_constant,inv_Drmax

 integer :: n_iceCat = -1   !used for GEM interface
 
! for timing tests
 real :: t_p3main_start,t_p3main_end, t_sedi_start,t_sedi_end
 real :: t_p3main_accum = 0.
 real :: t_sedi_accum   = 0.

 contains

!==================================================================================================!

 subroutine p3_init(lookup_file_dir,nCat,trplMomI,model,stat,abort_on_err)

!------------------------------------------------------------------------------------------!
! This subroutine initializes all physical constants and parameters needed by the P3       !
! scheme, including reading in two lookup table files and creating a third.                !
! 'P3_INIT' be called at the first model time step, prior to first call to 'P3_MAIN'.      !
!------------------------------------------------------------------------------------------!

#ifdef ECCCGEM
 use iso_c_binding
 use rpn_comm_itf_mod
#endif

 implicit none

! Passed arguments:
 character(len=*), intent(in)             :: lookup_file_dir    ! directory of the lookup tables (model library)
 integer,          intent(in)             :: nCat               ! number of free ice categories
 logical,          intent(in)             :: trplMomI           ! .T.=3-moment / .F.=2-moment (ice)
 integer,          intent(out), optional  :: stat               ! return status of subprogram
 logical,          intent(in),  optional  :: abort_on_err       ! abort when an error is encountered [.false.]
 character(len=*), intent(in),  optional  :: model              ! driving model

! Local variables and parameters:
 logical, save                  :: is_init = .false.
 character(len=1024), parameter :: version_p3                    = '4.5.2'
 character(len=1024), parameter :: version_intended_table_1_2mom = '5.4_2momI'
 character(len=1024), parameter :: version_intended_table_1_3mom = '5.4_3momI'
 character(len=1024), parameter :: version_intended_table_2      = '5.3'

 character(len=1024)            :: version_header_table_1_2mom
 character(len=1024)            :: version_header_table_1_3mom
 character(len=1024)            :: version_header_table_2
 character(len=1024)            :: lookup_file_1                   !lookup table, main
 character(len=1024)            :: lookup_file_2                   !lookup table for ice-ice interactions (for nCat>1 only)
 character(len=1024)            :: dumstr,read_path
 integer                        :: i,j,ii,jj,kk,jjj,jjj2,jjjj,jjjj2,end_status,zz,procnum,istat,ierr
 real                           :: lamr,mu_r,dum,dm,dum1,dum2,dum3,dum4,dum5,dd,amg,vt,dia
 double precision               :: dp_dum1, dp_dum2
 logical                        :: err_abort

!------------------------------------------------------------------------------------------!

 read_path = lookup_file_dir           ! path for lookup tables from official model library
!read_path = '/MY/LOOKUP_TABLE/PATH'   ! path for lookup tables from specified location

 if (trplMomI) then
   lookup_file_1 = trim(read_path)//'/'//'p3_lookupTable_1.dat-v'//trim(version_intended_table_1_3mom)
 else
   lookup_file_1 = trim(read_path)//'/'//'p3_lookupTable_1.dat-v'//trim(version_intended_table_1_2mom)
 endif
 lookup_file_2 = trim(read_path)//'/'//'p3_lookupTable_2.dat-v'//trim(version_intended_table_2)

!------------------------------------------------------------------------------------------!

 end_status = STATUS_ERROR
 err_abort = .false.
 if (present(abort_on_err)) err_abort = abort_on_err
 if (is_init) then
    if (present(stat)) stat = STATUS_OK
    return
 endif

 n_iceCat = nCat  !used for GEM interface

! mathematical/optimization constants
 pi    = 3.14159265
!pi    = acos(-1.)
 thrd  = 1./3.
 sxth  = 1./6.
 piov3 = pi*thrd
 piov6 = pi*sxth

! maximum total ice concentration (sum of all categories)
 max_total_Ni = 2000.e+3  !(m)

! switch for warm-rain parameterization
! = 1 Seifert and Beheng 2001
! = 2 Beheng 1994
! = 3 Khairoutdinov and Kogan 2000
! = 4 Kogan 2013
 iparam = 3

! droplet concentration (m-3)
 nccnst = 200.e+6

! parameters for Seifert and Beheng (2001) autoconversion/accretion
 kc     = 9.44e+9
 kr     = 5.78e+3

! physical constants
 cp     = 1005.
 inv_cp = 1./cp
 g      = 9.816
 rd     = 287.15
 rv     = 461.51
 ep_2   = 0.622
 rhosur = 100000./(rd*273.15)
 rhosui = 60000./(rd*253.15)
 ar     = 841.99667
 br     = 0.8
 f1r    = 0.78
 f2r    = 0.32
 ecr    = 1.
 rhow   = 1000.
 cpw    = 4218.
 inv_rhow = 1./rhow  !inverse of (max.) density of liquid water
 mu_r_constant = 0.  !fixed shape parameter for mu_r

! inv_Drmax = 1./0.0008 ! inverse of maximum allowed rain number-weighted mean diameter (old value)
 inv_Drmax = 1./0.002 ! inverse of maximum allowed rain number-weighted mean diameter in m

! limits for rime density [kg m-3]
 rho_rimeMin     =  50.
 rho_rimeMax     = 900.
 inv_rho_rimeMax = 1./rho_rimeMax

! minium allowable prognostic variables
 qsmall = 1.e-14
 nsmall = 1.e-16
 bsmall = qsmall*inv_rho_rimeMax
 zsmall = 1.e-35

! Bigg (1953)
!bimm   = 100.
!aimm   = 0.66
! Barklie and Gokhale (1959)
 bimm   = 2.
 aimm   = 0.65
 rin    = 0.1e-6
 mi0    = 4.*piov3*900.*1.e-18

 eci    = 0.5
 eri    = 1.
 bcn    = 2.

! mean size for soft lambda_r limiter [microns]
 dbrk   = 600.e-6
! ratio of rain number produced to ice number loss from melting
 nmltratio = 1.

! mu of initial ice formation by deposition nucleation (or if no ice is present for process group 1)
 mu_i_initial = 10.

! saturation pressure at T = 0 C
 e0    = polysvp1(273.15,0)

 cons1 = piov6*rhow
 cons2 = 4.*piov3*rhow
 cons3 = 1./(cons2*(25.e-6)**3)
 cons4 = 1./(dbrk**3*pi*rhow)
 cons5 = piov6*bimm
 cons6 = piov6**2*rhow*bimm
 cons7 = 4.*piov3*rhow*(1.e-6)**3
 cons8 = 1./(cons2*(40.e-6)**3)

! aerosol/droplet activation parameters
 mw     = 0.018
 osm    = 1.
 vi     = 3.
 epsm   = 0.9
 rhoa   = 1777.
 map    = 0.132
 ma     = 0.0284
 rr     = 8.3187
 bact   = vi*osm*epsm*mw*rhoa/(map*rhow)
! inv_bact = (map*rhow)/(vi*osm*epsm*mw*rhoa)    *** to replace /bact **

! mode 1
 inv_rm1 = 2.e+7           ! inverse aerosol mean size (m-1)
 sig1    = 2.0             ! aerosol standard deviation
 nanew1  = 300.e6          ! aerosol number mixing ratio (kg-1)
 f11     = 0.5*exp(2.5*(log(sig1))**2)
 f21     = 1. + 0.25*log(sig1)

! note: currently only set for a single mode, droplet activation code needs to
!       be modified to include the second mode
! mode 2
 inv_rm2 = 7.6923076e+5    ! inverse aerosol mean size (m-1)
 sig2    = 2.5             ! aerosol standard deviation
 nanew2  = 0.              ! aerosol number mixing ratio (kg-1)
 f12     = 0.5*exp(2.5*(log(sig2))**2)
 f22     = 1. + 0.25*log(sig2)

 minVIS =  1.              ! minimum visibility  (m)
 maxVIS = 99.e+3           ! maximum visibility  (m)

! parameters for droplet mass spectral shape, used by Seifert and Beheng (2001)
! warm rain scheme only (iparam = 1)
 dnu(1)  =  0.
 dnu(2)  = -0.557
 dnu(3)  = -0.430
 dnu(4)  = -0.307
 dnu(5)  = -0.186
 dnu(6)  = -0.067
 dnu(7)  =  0.050
 dnu(8)  =  0.167
 dnu(9)  =  0.282
 dnu(10) =  0.397
 dnu(11) =  0.512
 dnu(12) =  0.626
 dnu(13) =  0.739
 dnu(14) =  0.853
 dnu(15) =  0.966
 dnu(16) =  0.966

!------------------------------------------------------------------------------------------!
! read in ice microphysics table

 procnum = 0

#ifdef ECCCGEM
 call rpn_comm_rank(RPN_COMM_GRID,procnum,istat)
#endif

 if (trplMomI) then
    itabcoll_3mom = 0.
 else
    itabcoll = 0.
 endif
 if (nCat>1) then
    itabcolli1 = 0.
    itabcolli2 = 0.
 endif

 IF_PROC0: if (procnum == 0) then

  print*
  print*, ' P3 microphysics: v',trim(version_p3)
  print*, '   P3_INIT (reading/creating lookup tables)'

  TRIPLE_MOMENT_ICE: if (.not. trplMomI) then

    print*, '     Reading table 1 [',trim(version_intended_table_1_2mom),'] ...'

    open(unit=10, file=lookup_file_1, status='old', action='read')

    !-- check that table version is correct:
    !   note:  to override and use a different lookup table, simply comment out the 'return' below
    read(10,*) dumstr,version_header_table_1_2mom
    if (trim(version_intended_table_1_2mom) /= trim(version_header_table_1_2mom)) then
       print*
       print*, '***********   WARNING in P3_INIT   *************'
       print*, ' Loading lookupTable_1: v',trim(version_header_table_1_2mom)
       print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_1: ',    &
               trim(version_intended_table_1_2mom)
      !print*, '               -- ABORTING -- '
       print*, '************************************************'
       print*
       global_status = STATUS_ERROR
       if (trim(model) == 'WRF') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif

    IF_OK: if (global_status /= STATUS_ERROR) then

     read(10,*)
     do jj = 1,densize
       do ii = 1,rimsize
          do i = 1,isize
!              read(10,*) dum,dum,dum,dum,itab(jj,ii,i, 1),itab(jj,ii,i, 2),                &
!                     itab(jj,ii,i, 3),itab(jj,ii,i, 4),itab(jj,ii,i, 5),                   &
!                     itab(jj,ii,i, 6),itab(jj,ii,i, 7),itab(jj,ii,i, 8),dum,               &
!                     itab(jj,ii,i, 9),itab(jj,ii,i,10),itab(jj,ii,i,11),itab(jj,ii,i,12),  &
!                     itab(jj,ii,i,13),itab(jj,ii,i,14)
             read(10,*) dum,dum,dum, itab(jj,ii,i, 1),itab(jj,ii,i, 2),                   &
                    itab(jj,ii,i, 3),itab(jj,ii,i, 4),itab(jj,ii,i, 5),                   &
                    itab(jj,ii,i, 6),itab(jj,ii,i, 7),itab(jj,ii,i, 8),                   &
                    itab(jj,ii,i, 9),itab(jj,ii,i,10),itab(jj,ii,i,11),                   &
                    itab(jj,ii,i,12),itab(jj,ii,i,13),itab(jj,ii,i,14)
           enddo

         !read in table for ice-rain collection
          do i = 1,isize
             do j = 1,rcollsize
!                 read(10,*) dum,dum,dum,dum,dum,dp_dum1,dp_dum2,dum
!                 itabcoll(jj,ii,i,j,1) = sngl(dlog10(max(dp_dum1,1.d-90)))
!                 itabcoll(jj,ii,i,j,2) = sngl(dlog10(max(dp_dum2,1.d-90)))
                read(10,*) dum,dum,dum, dp_dum1,dp_dum2
                itabcoll(jj,ii,i,j,1) = dp_dum1
                itabcoll(jj,ii,i,j,2) = dp_dum2
             enddo
          enddo
       enddo  !ii
     enddo  !jj

    endif IF_OK
    close(10)

    if (global_status == STATUS_ERROR) then
       if (err_abort) then
          print*,'Stopping in P3 init'
          flush(6)
          stop
       endif
       return
    endif

  else ! TRIPLE_MOMENT_ICE  (the following is for trplMomI=.true.)

    print*, '     Reading table 1 [',trim(version_intended_table_1_3mom),'] ...'

    open(unit=10,file=lookup_file_1,status='old',iostat=ierr,err=101)
 101  if (ierr.ne.0) then
         print*,'Error opening 3-moment lookup table file '//lookup_file_1
         print*,'Make sure this file is unzipped and then rerun the model.'
         print*,' '
         flush(6)
         stop
      end if

    !-- check that table version is correct:
    !   note:  to override and use a different lookup table, simply comment out the 'return' below
    read(10,*) dumstr,version_header_table_1_3mom
    if (trim(version_intended_table_1_3mom) /= trim(version_header_table_1_3mom)) then
       print*
       print*, '***********   WARNING in P3_INIT   *************'
       print*, ' Loading lookupTable_1: v',trim(version_header_table_1_3mom)
       print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_1: v',    &
               trim(version_intended_table_1_3mom)
      !print*, '               -- ABORTING -- '
       print*, '************************************************'
       print*
       global_status = STATUS_ERROR
       if (trim(model) == 'WRF') then
          print*,'Stopping in P3 init'
          stop
       endif
    endif

    read(10,*)

    do zz = 1,zsize
       do jj = 1,densize
          do ii = 1,rimsize
             do i = 1,isize
                read(10,*) dum,dum,dum,dum,   itab_3mom(zz,jj,ii,i, 1),itab_3mom(zz,jj,ii,i, 2),     &
                     itab_3mom(zz,jj,ii,i, 3),itab_3mom(zz,jj,ii,i, 4),itab_3mom(zz,jj,ii,i, 5),     &
                     itab_3mom(zz,jj,ii,i, 6),itab_3mom(zz,jj,ii,i, 7),itab_3mom(zz,jj,ii,i, 8),     &
                     itab_3mom(zz,jj,ii,i, 9),itab_3mom(zz,jj,ii,i,10),itab_3mom(zz,jj,ii,i,11),     &
                     itab_3mom(zz,jj,ii,i,12),itab_3mom(zz,jj,ii,i,13),itab_3mom(zz,jj,ii,i,14),     &
                     itab_3mom(zz,jj,ii,i,15)
              enddo
         !read in table for ice-rain collection
             do i = 1,isize
                do j = 1,rcollsize
!                  read(10,*) dum,dum,dum,dum,dum,dp_dum1,dp_dum2
                   read(10,*) dum,dum,dum, dp_dum1,dp_dum2
                   itabcoll_3mom(zz,jj,ii,i,j,1) = dp_dum1
                   itabcoll_3mom(zz,jj,ii,i,j,2) = dp_dum2
                enddo
             enddo
          enddo  !ii
       enddo  !jj
    enddo   !zz

    close(10)

  endif TRIPLE_MOMENT_ICE

  IF_NCAT: if (nCat>1) then
   ! read in ice-ice collision lookup table  (used for multicategory only)

       print*, '     Reading table 2 [',trim(version_intended_table_2),'] ...'
       open(unit=10,file=lookup_file_2,status='old')

       !--check that table version is correct:
       read(10,*) dumstr,version_header_table_2
       if (trim(version_intended_table_2) /= trim(version_header_table_2)) then
          print*
          print*, '***********   WARNING in P3_INIT   *************'
          print*, ' Loading lookupTable_2 version: ',trim(version_header_table_2)
          print*, ' P3 v',trim(version_p3),' is intended to use lookupTable_2: v', &
                  trim(version_intended_table_2)
         !print*, '               -- ABORTING -- '
          print*, '************************************************'
          print*
          global_status = STATUS_ERROR
          if (trim(model)=='WRF' .or. trim(model)=='KIN1D') then
             print*,'Stopping in P3 init'
             stop
          endif
       endif
       IF_OKB: if (global_status /= STATUS_ERROR) then
       read(10,*)

       do i = 1,iisize
          do jjj = 1,rimsize
             do jjjj = 1,densize
                do ii = 1,iisize
                   do jjj2 = 1,rimsize
                      do jjjj2 = 1,densize
                         read(10,*) dum,dum,dum,dum,dum,dum,                   &
                         itabcolli1(i,jjj,jjjj,ii,jjj2,jjjj2),                 &
                         itabcolli2(i,jjj,jjjj,ii,jjj2,jjjj2)
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
       endif IF_OKB

       close(unit=10)

    endif IF_NCAT

 endif IF_PROC0

#ifdef ECCCGEM
 call rpn_comm_bcast(global_status,1,RPN_COMM_INTEGER,0,RPN_COMM_GRID,istat)
#endif

 if (global_status == STATUS_ERROR) then
    if (err_abort) then
       print*,'Stopping in P3 init'
       flush(6)
       stop
    endif
    return
 endif

#ifdef ECCCGEM
 if (trplMomI) then
    call rpn_comm_bcast(itab_3mom,size(itab_3mom),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcoll_3mom,size(itabcoll_3mom),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
 else
    call rpn_comm_bcast(itab,size(itab),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcoll,size(itabcoll),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
 endif
 if (nCat>1) then
    call rpn_comm_bcast(itabcolli1,size(itabcolli1),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
    call rpn_comm_bcast(itabcolli2,size(itabcolli2),RPN_COMM_REAL,0,RPN_COMM_GRID,istat)
 endif
#endif

!------------------------------------------------------------------------------------------!

! Generate lookup table for rain shape parameter mu_r
! this is very fast so it can be generated at the start of each run
! make a 150x1 1D lookup table, this is done in parameter
! space of a scaled mean size proportional qr/Nr -- initlamr

!print*, '   Generating rain lookup-table ...'

!-- for variable mu_r only:
! ! !  do i = 1,150              ! loop over lookup table values
! ! !     initlamr = 1./((real(i)*2.)*1.e-6 + 250.e-6)
! ! !
! ! ! ! iterate to get mu_r
! ! ! ! mu_r-lambda relationship is from Cao et al. (2008), eq. (7)
! ! !
! ! ! ! start with first guess, mu_r = 0
! ! !
! ! !     mu_r = 0.
! ! !
! ! !     do ii=1,50
! ! !        lamr = initlamr*((mu_r+3.)*(mu_r+2.)*(mu_r+1.)/6.)**thrd
! ! !
! ! ! ! new estimate for mu_r based on lambda
! ! ! ! set max lambda in formula for mu_r to 20 mm-1, so Cao et al.
! ! ! ! formula is not extrapolated beyond Cao et al. data range
! ! !        dum  = min(20.,lamr*1.e-3)
! ! !        mu_r = max(0.,-0.0201*dum**2+0.902*dum-1.718)
! ! !
! ! ! ! if lambda is converged within 0.1%, then exit loop
! ! !        if (ii.ge.2) then
! ! !           if (abs((lamold-lamr)/lamr).lt.0.001) goto 111
! ! !        end if
! ! !
! ! !        lamold = lamr
! ! !
! ! !     enddo
! ! !
! ! ! 111 continue
! ! !
! ! ! ! assign lookup table values
! ! !     mu_r_table(i) = mu_r
! ! !
! ! !  enddo
!==

 mu_r_table(:) = mu_r_constant

!.......................................................................
! Generate lookup table for rain fallspeed and ventilation parameters
! the lookup table is two dimensional as a function of number-weighted mean size
! proportional to qr/Nr and shape parameter mu_r

 if (procnum == 0) then
    print*, '     Generating table for rain fallspeed/ventilation parameters ...'
 endif

 mu_r_loop: do ii = 1,10

   !mu_r = real(ii-1)  ! values of mu
    mu_r = mu_r_constant

! loop over number-weighted mean size
    meansize_loop: do jj = 1,300

       if (jj.le.20) then
          dm = (real(jj)*10.-5.)*1.e-6      ! mean size [m]
       elseif (jj.gt.20) then
          dm = (real(jj-20)*30.+195.)*1.e-6 ! mean size [m]
       endif

       lamr = (mu_r+1)/dm

! do numerical integration over PSD

       dum1 = 0. ! numerator,   number-weighted fallspeed
       dum2 = 0. ! denominator, number-weighted fallspeed
       dum3 = 0. ! numerator,   mass-weighted fallspeed
       dum4 = 0. ! denominator, mass-weighted fallspeed
       dum5 = 0. ! term for ventilation factor in evap
       dd   = 2.

! loop over PSD to numerically integrate number and mass-weighted mean fallspeeds
       do kk = 1,10000

          dia = (real(kk)*dd-dd*0.5)*1.e-6  ! size bin [m]
          amg = piov6*997.*dia**3           ! mass [kg]
          amg = amg*1000.                   ! convert [kg] to [g]

         !get fallspeed as a function of size [m s-1]
          if (dia*1.e+6.le.134.43)      then
            vt = 4.5795e+3*amg**(2.*thrd)
          elseif (dia*1.e+6.lt.1511.64) then
            vt = 4.962e+1*amg**thrd
          elseif (dia*1.e+6.lt.3477.84) then
            vt = 1.732e+1*amg**sxth
          else
            vt = 9.17
          endif

         !note: factor of 4.*mu_r is non-answer changing and only needed to
         !      prevent underflow/overflow errors, same with 3.*mu_r for dum5
          dum1 = dum1 + vt*10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum2 = dum2 + 10.**(mu_r*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum3 = dum3 + vt*10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum4 = dum4 + 10.**((mu_r+3.)*alog10(dia)+4.*mu_r)*exp(-lamr*dia)*dd*1.e-6
          dum5 = dum5 + (vt*dia)**0.5*10.**((mu_r+1.)*alog10(dia)+3.*mu_r)*exp(-lamr*dia)*dd*1.e-6

       enddo ! kk-loop (over PSD)

       dum2 = max(dum2, 1.e-30)  !to prevent divide-by-zero below
       dum4 = max(dum4, 1.e-30)  !to prevent divide-by-zero below
       dum5 = max(dum5, 1.e-30)  !to prevent log10-of-zero below

       vn_table(jj,ii)    = dum1/dum2
       vm_table(jj,ii)    = dum3/dum4
       revap_table(jj,ii) = 10.**(alog10(dum5)+(mu_r+1.)*alog10(lamr)-(3.*mu_r))

    enddo meansize_loop

 enddo mu_r_loop

!.......................................................................

 if (procnum == 0) then
    print*, '   P3_INIT DONE.'
    print*
 endif

 end_status = STATUS_OK
 if (present(stat)) stat = end_status
 is_init = .true.

 return

END subroutine p3_init

!==================================================================================================!
#ifndef ECCCGEM

 SUBROUTINE mp_p3_wrapper_wrf(th_3d,qv_3d,qc_3d,qr_3d,qnr_3d,                            &
                              th_old_3d,qv_old_3d,                                       &
                              pii,p,dz,w,dt,itimestep,                                   &
                              rainnc,rainncv,sr,snownc,snowncv,n_iceCat,                 &
                              ids, ide, jds, jde, kds, kde ,                             &
                              ims, ime, jms, jme, kms, kme ,                             &
                              its, ite, jts, jte, kts, kte ,                             &
                              diag_zdbz_3d,diag_effc_3d,diag_effi_3d,                    &
                              diag_vmi_3d,diag_di_3d,diag_rhopo_3d,                      &
                              qi1_3d,qni1_3d,qir1_3d,qib1_3d,nc_3d,qzi1_3d)
!                              diag_dhmax_3d,diag_lami_3d,diag_mui_3d)

  !------------------------------------------------------------------------------------------!
  ! This subroutine is the main WRF interface with the P3 microphysics scheme.  It takes     !
  ! 3D variables form the driving model and passes 2D slabs (i,k) to the main microphysics   !
  ! subroutine ('P3_MAIN') over a j-loop.  For each slab, 'P3_MAIN' updates the prognostic   !
  ! variables (hydrometeor variables, potential temperature, and water vapor).  The wrapper  !
  ! also updates the accumulated precipitation arrays and then passes back them, the         !
  ! updated 3D fields, and some diagnostic fields to the driver model.                       !
  !                                                                                          !
  ! Three configurations of the P3 scheme are currently available:                           !
  !  1) specified droplet number (i.e. 1-moment cloud water), 1 ice category                 !
  !  2) predicted droplet number (i.e. 2-moment cloud water), 1 ice category                 !
  !  3) predicted droplet number (i.e. 2-moment cloud water), 2 ice categories               !
  !  4) predicted droplet number (i.e. 2-moment cloud water), 1 ice catetory, 3-moment ice   !
  !                                                                                          !
  !  The  2-moment cloud version is based on a specified aerosol distribution and            !
  !  does not include a subgrid-scale vertical velocity for droplet activation. Hence,       !
  !  this version should only be used for high-resolution simulations that resolve           !
  !  vertical motion driving droplet activation.                                             !
  !                                                                                          !
  !------------------------------------------------------------------------------------------!

  !--- input:

  ! pii       --> Exner function (nondimensional pressure) (currently not used!)
  ! p         --> pressure (pa)
  ! dz        --> height difference across vertical levels (m)
  ! w         --> vertical air velocity (m/s)
  ! dt        --> time step (s)
  ! itimestep --> integer time step counter
  ! n_iceCat  --> number of ice-phase categories


  !--- input/output:

  ! th_3d     --> theta (K)
  ! qv_3d     --> vapor mass mixing ratio (kg/kg)
  ! qc_3d     --> cloud water mass mixing ratio (kg/kg)
  ! qr_3d     --> rain mass mixing ratio (kg/kg)
  ! qnr_3d    --> rain number mixing ratio (#/kg)
  ! qi1_3d    --> total ice mixing ratio (kg/kg)
  ! qni1_3d   --> ice number mixing ratio (#/kg)
  ! qir1_3d   --> rime ice mass mixing ratio (kg/kg)
  ! qib1_3d   --> ice rime volume mixing ratio (m^-3 kg^-1)
  ! qzi1_3d   --> ice rime volume mixing ratio (m^-6 kg^-1)
  ! nc_3d     --> cloud droplet number mixing ratio (#/kg)

  !--- output:

  ! rainnc        --> accumulated surface precip (mm)
  ! rainncv       --> one time step accumulated surface precip (mm)
  ! sr            --> ice to total surface precip ratio
  ! snownc        --> accumulated surface ice precip (mm)
  ! snowncv       --> one time step accumulated surface ice precip (mm)
  ! ids...kte     --> integer domain/tile bounds
  ! diag_zdbz_3d  --> reflectivity (dBZ)
  ! diag_effc_3d  --> cloud droplet effective radius (m)
  ! diag_effi_3d  --> ice effective radius (m)
  ! diag_vmi_3d   --> mean mass weighted ice fallspeed (m/s)
  ! diag_di_3d    --> mean mass weighted ice size (m)
  ! diag_rhopo_3d --> mean mass weighted ice density (kg/m3)

  implicit none

  !--- arguments:

   integer, intent(in)            ::  ids, ide, jds, jde, kds, kde ,                      &
                                      ims, ime, jms, jme, kms, kme ,                      &
                                      its, ite, jts, jte, kts, kte
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: th_3d,qv_3d,qc_3d,qr_3d,   &
                   qnr_3d,diag_zdbz_3d,diag_effc_3d,diag_effi_3d,diag_vmi_3d,diag_di_3d,  &
                   diag_rhopo_3d,th_old_3d,qv_old_3d
!   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: diag_dhmax_3d,             &
!                   diag_lami_3d,diag_mui_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qi1_3d,qni1_3d,qir1_3d,    &
                                                               qib1_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: nc_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: qzi1_3d

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: pii,p,dz,w
   real, dimension(ims:ime, jms:jme), intent(inout) :: RAINNC,RAINNCV,SR,SNOWNC,SNOWNCV
   real, intent(in)    :: dt
   integer, intent(in) :: itimestep
   integer, intent(in) :: n_iceCat

   !--- local variables/parameters:

   character(len=16), parameter :: model = 'WRF'

   real, dimension(ims:ime, kms:kme) ::nc,ssat

   real, dimension(its:ite) :: pcprt_liq,pcprt_sol
   real                     :: dum1
   integer                  :: i,k,j
   integer, parameter       :: n_diag_3d = 1         ! number of user-defined diagnostic fields
   integer, parameter       :: n_diag_2d = 1         ! number of user-defined diagnostic fields

   real, dimension(ims:ime, kms:kme, n_diag_3d) :: diag_3d
   real, dimension(ims:ime, n_diag_2d)          :: diag_2d
   logical                  :: log_predictNc
   logical                  :: log_3momentIce
   logical, parameter       :: debug_on      = .false. !switch for internal debug checking
   logical, parameter       :: typeDiags_ON  = .false.
   real,    parameter       :: clbfact_dep   = 1.0     !calibration factor for deposition
   real,    parameter       :: clbfact_sub   = 1.0     !calibration factor for sublimation

! variables for cloud fraction (currently not used with WRF)
   logical                    :: scpf_on               ! switch for activation of SCPF scheme
   real                       :: scpf_pfrac            ! precipitation fraction factor (SCPF)
   real                       :: scpf_resfact          ! model resolution factor (SCPF)
   real, dimension(ims:ime, kms:kme) :: cldfrac        ! cloud fraction computed by SCPF

   !------------------------------------------------------------------------------------------!

   scpf_on=.false. ! cloud fraction version not used with WRF
   scpf_pfrac=0.   ! dummy variable (not used), set to 0
   scpf_resfact=0. ! dummy variable (not used), set to 0

   log_predictNc=.false.
   if (present(nc_3d)) log_predictNc = .true.

   log_3momentIce=.false.
   if (present(qzi1_3d)) log_3momentIce = .true.

  ! convert advected (N*Z)^0.5 to Z for P3 main
   if (log_3momentIce) then
      do j = jts,jte
         do k = kts,kte
            do i = its,ite
               if (qni1_3d(i,k,j).ge.qsmall) then
                  qzi1_3d(i,k,j) = qzi1_3d(i,k,j)**2/qni1_3d(i,k,j)
               else
                  qzi1_3d(i,k,j) = 0.
               endif
            enddo
         enddo
      enddo
   endif
  !.............................................

   do j = jts,jte      ! j loop (north-south)

      if (log_predictNc) then
         nc(its:ite,kts:kte)=nc_3d(its:ite,kts:kte,j)
     ! if Nc is specified then set nc array to zero
      else
         nc=0.
      endif

     ! note: code for prediction of ssat not currently avaiable, set 2D array to 0
      ssat=0.

      if (.not. log_3momentIce) then
       call P3_MAIN(qc_3d(its:ite,kts:kte,j),nc(its:ite,kts:kte),                                       &
               qr_3d(its:ite,kts:kte,j),qnr_3d(its:ite,kts:kte,j),                                      &
               th_old_3d(its:ite,kts:kte,j),th_3d(its:ite,kts:kte,j),qv_old_3d(its:ite,kts:kte,j),      &
               qv_3d(its:ite,kts:kte,j),dt,qi1_3d(its:ite,kts:kte,j),                                   &
               qir1_3d(its:ite,kts:kte,j),qni1_3d(its:ite,kts:kte,j),                                   &
               qib1_3d(its:ite,kts:kte,j),ssat(its:ite,kts:kte),                                        &
               W(its:ite,kts:kte,j),P(its:ite,kts:kte,j),                                               &
               DZ(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,n_iceCat,            &
               diag_zdbz_3d(its:ite,kts:kte,j),diag_effc_3d(its:ite,kts:kte,j),                         &
               diag_effi_3d(its:ite,kts:kte,j),diag_vmi_3d(its:ite,kts:kte,j),                          &
               diag_di_3d(its:ite,kts:kte,j),diag_rhopo_3d(its:ite,kts:kte,j),                          &
               n_diag_2d,diag_2d(its:ite,1:n_diag_2d),                                                  &
               n_diag_3d,diag_3d(its:ite,kts:kte,1:n_diag_3d),                                          &
               log_predictNc,typeDiags_ON,trim(model),clbfact_dep,clbfact_sub,debug_on,                 &
               scpf_on,scpf_pfrac,scpf_resfact,cldfrac )!,diag_dhmax=diag_dhmax_3d(its:ite,kts:kte,j),     &
!               diag_lami=diag_lami_3d(its:ite,kts:kte,j),diag_mui=diag_mui_3d(its:ite,kts:kte,j))

      else if (log_3momentIce) then

       call P3_MAIN(qc_3d(its:ite,kts:kte,j),nc(its:ite,kts:kte),                                       &
               qr_3d(its:ite,kts:kte,j),qnr_3d(its:ite,kts:kte,j),                                      &
               th_old_3d(its:ite,kts:kte,j),th_3d(its:ite,kts:kte,j),qv_old_3d(its:ite,kts:kte,j),      &
               qv_3d(its:ite,kts:kte,j),dt,qi1_3d(its:ite,kts:kte,j),                                   &
               qir1_3d(its:ite,kts:kte,j),qni1_3d(its:ite,kts:kte,j),                                   &
               qib1_3d(its:ite,kts:kte,j),ssat(its:ite,kts:kte),                                        &
               W(its:ite,kts:kte,j),P(its:ite,kts:kte,j),                                               &
               DZ(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,n_iceCat,            &
               diag_zdbz_3d(its:ite,kts:kte,j),diag_effc_3d(its:ite,kts:kte,j),                         &
               diag_effi_3d(its:ite,kts:kte,j),diag_vmi_3d(its:ite,kts:kte,j),                          &
               diag_di_3d(its:ite,kts:kte,j),diag_rhopo_3d(its:ite,kts:kte,j),                          &
               n_diag_2d,diag_2d(its:ite,1:n_diag_2d),                                                  &
               n_diag_3d,diag_3d(its:ite,kts:kte,1:n_diag_3d),                                          &
               log_predictNc,typeDiags_ON,trim(model),clbfact_dep,clbfact_sub,debug_on,                 &
               scpf_on,scpf_pfrac,scpf_resfact,cldfrac,zitot=qzi1_3d(its:ite,kts:kte,j))
!               diag_dhmax=diag_dhmax_3d(its:ite,kts:kte,j),diag_lami=diag_lami_3d(its:ite,kts:kte,j),   &
!               diag_mui=diag_mui_3d(its:ite,kts:kte,j))
      endif ! 3momentIce

     !surface precipitation output:
      dum1 = 1000.*dt
      RAINNC(its:ite,j)  = RAINNC(its:ite,j) + (pcprt_liq(:) + pcprt_sol(:))*dum1  ! conversion from m/s to mm/time step
      RAINNCV(its:ite,j) = (pcprt_liq(:) + pcprt_sol(:))*dum1                      ! conversion from m/s to mm/time step
      SNOWNC(its:ite,j)  = SNOWNC(its:ite,j) + pcprt_sol(:)*dum1                   ! conversion from m/s to mm/time step
      SNOWNCV(its:ite,j) = pcprt_sol(:)*dum1                                       ! conversion from m/s to mm/time step
      SR(its:ite,j)      = pcprt_sol(:)/(pcprt_liq(:)+pcprt_sol(:)+1.E-12)         ! solid-to-total ratio

    !convert nc array from 2D to 3D if Nc is predicted
      if (log_predictNc) then
         nc_3d(its:ite,kts:kte,j)=nc(its:ite,kts:kte)
      endif

    !set background effective radii (i.e. with no explicit condensate) to prescribed values:
    !  where (qc_3d(:,:,j) < 1.e-14) diag_effc_3d(:,:,j) = 10.e-6
    !  where (qitot < 1.e-14) diag_effi = 25.e-6

   enddo ! j loop

  ! convert Z from P3 to (N*Z)^0.5 for advection
   if (log_3momentIce) then
      do j = jts,jte
         do k = kts,kte
            do i = its,ite
               if (qni1_3d(i,k,j).ge.qsmall.and.qzi1_3d(i,k,j).ge.1.e-30) then
                  qzi1_3d(i,k,j) = (qzi1_3d(i,k,j)*qni1_3d(i,k,j))**0.5
               else
                  qzi1_3d(i,k,j) = 0.
               endif
            enddo
         enddo
      enddo
   endif
  !...............................................

   if (global_status /= STATUS_OK) then
      print*,'Stopping in P3, problem in P3 main'
      stop
   endif

   END SUBROUTINE mp_p3_wrapper_wrf

   !------------------------------------------------------------------------------------------!

   SUBROUTINE mp_p3_wrapper_wrf_2cat(th_3d,qv_3d,qc_3d,qr_3d,qnr_3d,                     &
                              th_old_3d,qv_old_3d,                                       &
                              pii,p,dz,w,dt,itimestep,                                   &
                              rainnc,rainncv,sr,snownc,snowncv,n_iceCat,                 &
                              ids, ide, jds, jde, kds, kde ,                             &
                              ims, ime, jms, jme, kms, kme ,                             &
                              its, ite, jts, jte, kts, kte ,                             &
                              diag_zdbz_3d,diag_effc_3d,diag_effi_3d,                    &
                              diag_vmi_3d,diag_di_3d,diag_rhopo_3d,                      &
                              diag_vmi2_3d,diag_di2_3d,diag_rhopo2_3d,                   &
                              qi1_3d,qni1_3d,qir1_3d,qib1_3d,                            &
                              qi2_3d,qni2_3d,qir2_3d,qib2_3d,nc_3d)

  !------------------------------------------------------------------------------------------!
  ! This subroutine is the main WRF interface with the P3 microphysics scheme.  It takes     !
  ! 3D variables form the driving model and passes 2D slabs (i,k) to the main microphysics   !
  ! subroutine ('P3_MAIN') over a j-loop.  For each slab, 'P3_MAIN' updates the prognostic   !
  ! variables (hydrometeor variables, potential temperature, and water vapor).  The wrapper  !
  ! also updates the accumulated precipitation arrays and then passes back them, the         !
  ! updated 3D fields, and some diagnostic fields to the driver model.                       !
  !                                                                                          !
  !------------------------------------------------------------------------------------------!

  !--- input:

  ! pii       --> Exner function (nondimensional pressure) (currently not used!)
  ! p         --> pressure (pa)
  ! dz        --> height difference across vertical levels (m)
  ! w         --> vertical air velocity (m/s)
  ! dt        --> time step (s)
  ! itimestep --> integer time step counter
  ! n_iceCat  --> number of ice-phase categories


  !--- input/output:

  ! th_3d     --> theta (K)
  ! qv_3d     --> vapor mass mixing ratio (kg/kg)
  ! qc_3d     --> cloud water mass mixing ratio (kg/kg)
  ! qr_3d     --> rain mass mixing ratio (kg/kg)
  ! qnr_3d    --> rain number mixing ratio (#/kg)
  ! qi1_3d    --> total ice mixing ratio category 1 (kg/kg)
  ! qni1_3d   --> ice number mixing ratio category 1 (#/kg)
  ! qir1_3d   --> rime ice mass mixing ratio category 1 (kg/kg)
  ! qib1_3d   --> ice rime volume mixing ratio category 1 (m^-3 kg^-1)
  ! qi2_3d    --> total ice mixing ratio category 2 (kg/kg)
  ! qni2_3d   --> ice number mixing ratio category 2 (#/kg)
  ! qir2_3d   --> rime ice mass mixing ratio category 2 (kg/kg)
  ! qib2_3d   --> ice rime volume mixing ratio category 2 (m^-3 kg^-1)
  ! nc_3d     --> cloud droplet number mixing ratio (#/kg)

  !--- output:

  ! rainnc        --> accumulated surface precip (mm)
  ! rainncv       --> one time step accumulated surface precip (mm)
  ! sr            --> ice to total surface precip ratio
  ! snownc        --> accumulated surface ice precip (mm)
  ! snowncv       --> one time step accumulated surface ice precip (mm)
  ! ids...kte     --> integer domain/tile bounds
  ! diag_zdbz_3d  --> reflectivity (dBZ)
  ! diag_effc_3d  --> cloud droplet effective radius (m)
  ! diag_effi_3d  --> ice effective radius (m)
  ! diag_vmi_3d   --> mean mass weighted ice fallspeed category 1 (m/s)
  ! diag_di_3d    --> mean mass weighted ice size category 1 (m)
  ! diag_rhopo_3d --> mean mass weighted ice density category 1 (kg/m3)
  ! diag_vmi2_3d   --> mean mass weighted ice fallspeed category 2 (m/s)
  ! diag_di2_3d    --> mean mass weighted ice size category 2 (m)
  ! diag_rhopo2_3d --> mean mass weighted ice density category 2 (kg/m3)

  implicit none

  !--- arguments:

   integer, intent(in)            ::  ids, ide, jds, jde, kds, kde ,                      &
                                      ims, ime, jms, jme, kms, kme ,                      &
                                      its, ite, jts, jte, kts, kte
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: th_3d,qv_3d,qc_3d,qr_3d,   &
                   qnr_3d,diag_zdbz_3d,diag_effc_3d,diag_effi_3d,diag_vmi_3d,diag_di_3d,  &
                   diag_rhopo_3d,th_old_3d,qv_old_3d,                                     &
                   diag_vmi2_3d,diag_di2_3d,diag_rhopo2_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout):: qi1_3d,qni1_3d,qir1_3d,    &
                                                               qib1_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout) :: qi2_3d,qni2_3d,           &
                                                                qir2_3d,qib2_3d
   real, dimension(ims:ime, kms:kme, jms:jme), intent(inout), optional :: nc_3d

   real, dimension(ims:ime, kms:kme, jms:jme), intent(in) :: pii,p,dz,w
   real, dimension(ims:ime, jms:jme), intent(inout) :: RAINNC,RAINNCV,SR,SNOWNC,SNOWNCV
   real, intent(in)    :: dt
   integer, intent(in) :: itimestep
   integer, intent(in) :: n_iceCat

   !--- local variables/parameters:

   character(len=16), parameter :: model = 'WRF'

   real, dimension(ims:ime, kms:kme) ::nc,ssat

   ! note: hard-wired for two ice categories
   real, dimension(ims:ime, kms:kme, 2) :: qitot,qirim,nitot,birim,diag_di,diag_vmi,       &
                                          diag_rhopo,diag_effi

   real, dimension(its:ite) :: pcprt_liq,pcprt_sol
   real                     :: dum1,dum2
   integer                  :: i,k,j
   integer, parameter       :: n_diag_3d = 1         ! number of user-defined diagnostic fields
   integer, parameter       :: n_diag_2d = 1         ! number of user-defined diagnostic fields

   real, dimension(ims:ime, kms:kme, n_diag_3d) :: diag_3d
   real, dimension(ims:ime, n_diag_2d)          :: diag_2d
   logical                  :: log_predictNc
   logical, parameter       :: typeDiags_ON  = .false.
   logical, parameter       :: debug_on      = .false. !switch for internal debug checking
   real,    parameter       :: clbfact_dep   = 1.0     !calibration factor for deposition
   real,    parameter       :: clbfact_sub   = 1.0     !calibration factor for sublimation

! variables for cloud fraction (currently not used with WRF)
   logical                    :: scpf_on               ! switch for activation of SCPF scheme
   real                       :: scpf_pfrac            ! precipitation fraction factor (SCPF)
   real                       :: scpf_resfact          ! model resolution factor (SCPF)
   real, dimension(ims:ime, kms:kme) :: cldfrac        ! cloud fraction computed by SCPF

   !------------------------------------------------------------------------------------------!

   scpf_on=.false. ! cloud fraction version not used with WRF
   scpf_pfrac=0.   ! dummy variable (not used), set to 0
   scpf_resfact=0. ! dummy variable (not used), set to 0

   log_predictNc=.false.
   if (present(nc_3d)) log_predictNc = .true.

   do j = jts,jte      ! j loop (north-south)

      if (log_predictNc) then
         nc(its:ite,kts:kte)=nc_3d(its:ite,kts:kte,j)
     ! if Nc is specified then set nc array to zero
      else
         nc=0.
      endif

     ! note: code for prediction of ssat not currently avaiable, set 2D array to 0
      ssat=0.

    !contruct full ice arrays from individual category arrays:
      qitot(:,:,1) = qi1_3d(:,:,j)
      qirim(:,:,1) = qir1_3d(:,:,j)
      nitot(:,:,1) = qni1_3d(:,:,j)
      birim(:,:,1) = qib1_3d(:,:,j)

      qitot(:,:,2) = qi2_3d(:,:,j)
      qirim(:,:,2) = qir2_3d(:,:,j)
      nitot(:,:,2) = qni2_3d(:,:,j)
      birim(:,:,2) = qib2_3d(:,:,j)

       call P3_MAIN(qc_3d(its:ite,kts:kte,j),nc(its:ite,kts:kte),                                   &
               qr_3d(its:ite,kts:kte,j),qnr_3d(its:ite,kts:kte,j),                                  &
               th_old_3d(its:ite,kts:kte,j),th_3d(its:ite,kts:kte,j),qv_old_3d(its:ite,kts:kte,j),  &
               qv_3d(its:ite,kts:kte,j),dt,qitot(its:ite,kts:kte,1:n_iceCat),                       &
               qirim(its:ite,kts:kte,1:n_iceCat),nitot(its:ite,kts:kte,1:n_iceCat),                 &
               birim(its:ite,kts:kte,1:n_iceCat),ssat(its:ite,kts:kte),                             &
               W(its:ite,kts:kte,j),P(its:ite,kts:kte,j),                                           &
               DZ(its:ite,kts:kte,j),itimestep,pcprt_liq,pcprt_sol,its,ite,kts,kte,n_iceCat,        &
               diag_zdbz_3d(its:ite,kts:kte,j),diag_effc_3d(its:ite,kts:kte,j),                     &
               diag_effi(its:ite,kts:kte,1:n_iceCat),diag_vmi(its:ite,kts:kte,1:n_iceCat),          &
               diag_di(its:ite,kts:kte,1:n_iceCat),diag_rhopo(its:ite,kts:kte,1:n_iceCat),          &
               n_diag_2d,diag_2d(its:ite,1:n_diag_2d),                                              &
               n_diag_3d,diag_3d(its:ite,kts:kte,1:n_diag_3d),                                      &
               log_predictNc,typeDiags_ON,trim(model),clbfact_dep,clbfact_sub,debug_on,             &
               scpf_on,scpf_pfrac,scpf_resfact,cldfrac)

     !surface precipitation output:
      dum1 = 1000.*dt
      RAINNC(its:ite,j)  = RAINNC(its:ite,j) + (pcprt_liq(:) + pcprt_sol(:))*dum1  ! conversion from m/s to mm/time step
      RAINNCV(its:ite,j) = (pcprt_liq(:) + pcprt_sol(:))*dum1                      ! conversion from m/s to mm/time step
      SNOWNC(its:ite,j)  = SNOWNC(its:ite,j) + pcprt_sol(:)*dum1                   ! conversion from m/s to mm/time step
      SNOWNCV(its:ite,j) = pcprt_sol(:)*dum1                                       ! conversion from m/s to mm/time step
      SR(its:ite,j)      = pcprt_sol(:)/(pcprt_liq(:)+pcprt_sol(:)+1.E-12)         ! solid-to-total ratio

    !convert nc array from 2D to 3D if Nc is predicted
      if (log_predictNc) then
         nc_3d(its:ite,kts:kte,j)=nc(its:ite,kts:kte)
      endif

    !set background effective radii (i.e. with no explicit condensate) to prescribed values:
    !  where (qc_3d(:,:,j) < 1.e-14) diag_effc_3d(:,:,j) = 10.e-6
    !  where (qitot < 1.e-14) diag_effi = 25.e-6

    !decompose full ice arrays into individual category arrays:
      qi1_3d(its:ite,kts:kte,j)  = qitot(its:ite,kts:kte,1)
      qir1_3d(its:ite,kts:kte,j) = qirim(its:ite,kts:kte,1)
      qni1_3d(its:ite,kts:kte,j) = nitot(its:ite,kts:kte,1)
      qib1_3d(its:ite,kts:kte,j) = birim(its:ite,kts:kte,1)

      qi2_3d(its:ite,kts:kte,j)  = qitot(its:ite,kts:kte,2)
      qir2_3d(its:ite,kts:kte,j) = qirim(its:ite,kts:kte,2)
      qni2_3d(its:ite,kts:kte,j) = nitot(its:ite,kts:kte,2)
      qib2_3d(its:ite,kts:kte,j) = birim(its:ite,kts:kte,2)

      diag_vmi_3d(its:ite,kts:kte,j)  = diag_vmi(its:ite,kts:kte,1)
      diag_di_3d(its:ite,kts:kte,j) = diag_di(its:ite,kts:kte,1)
      diag_rhopo_3d(its:ite,kts:kte,j) = diag_rhopo(its:ite,kts:kte,1)
      diag_vmi2_3d(its:ite,kts:kte,j)  = diag_vmi(its:ite,kts:kte,2)
      diag_di2_3d(its:ite,kts:kte,j) = diag_di(its:ite,kts:kte,2)
      diag_rhopo2_3d(its:ite,kts:kte,j) = diag_rhopo(its:ite,kts:kte,2)

         do i=its,ite
            do k=kts,kte

         ! for output fallspeed, size, and density, use mass-weighting of categories
!            if ((qitot(i,k,1)+qitot(i,k,2)).ge.qsmall) then
!               diag_vmi_3d(i,k,j) = (diag_vmi(i,k,1)*qitot(i,k,1)+diag_vmi(i,k,2)*qitot(i,k,2))/(qitot(i,k,1)+qitot(i,k,2))
!               diag_di_3d(i,k,j) = (diag_di(i,k,1)*qitot(i,k,1)+diag_di(i,k,2)*qitot(i,k,2))/(qitot(i,k,1)+qitot(i,k,2))
!               diag_rhopo_3d(i,k,j) = (diag_rhopo(i,k,1)*qitot(i,k,1)+diag_rhopo(i,k,2)*qitot(i,k,2))/(qitot(i,k,1)+qitot(i,k,2))
!            else  ! set to default values of 0 if ice is not present
!               diag_vmi_3d(i,k,j) = 0.
!               diag_di_3d(i,k,j) = 0.
!               diag_rhopo_3d(i,k,j) = 0.
!            end if

            ! for the combined effective radius, we need to approriately weight by mass and projected area
            if (qitot(i,k,1).ge.qsmall) then
               dum1=qitot(i,k,1)/diag_effi(i,k,1)
            else
               dum1=0.
            end if
            if (qitot(i,k,2).ge.qsmall) then
               dum2=qitot(i,k,2)/diag_effi(i,k,2)
            else
               dum2=0.
            end if
            diag_effi_3d(i,k,j)=25.e-6  ! set to default 25 microns
            if (qitot(i,k,1).ge.qsmall.or.qitot(i,k,2).ge.qsmall) then
               diag_effi_3d(i,k,j)=(qitot(i,k,1)+qitot(i,k,2))/(dum1+dum2)
            end if

            end do
         end do

   enddo ! j loop

   if (global_status /= STATUS_OK) then
      print*,'Stopping in P3, problem in P3 main'
      stop
   endif

   END SUBROUTINE mp_p3_wrapper_wrf_2cat

#endif

!==================================================================================================!
#ifdef ECCCGEM

 function mp_p3_wrapper_gem(ttend,qtend,qctend,qrtend,qitend,                                     &
                              qvap_m,qvap,temp_m,temp,dt,dt_max,ww,psfc,gztherm,gzmom,sigma,kount,&
                              trnch,ni,nk,prt_liq,prt_sol,prt_drzl,prt_rain,prt_crys,prt_snow,    &
                              prt_grpl,prt_pell,prt_hail,prt_sndp,diag_Zet,diag_Zec,diag_effc,    &
                              qc,nc,qr,nr,n_diag_2d,diag_2d,n_diag_3d,diag_3d,                    &
                              clbfact_dep,clbfact_sub,debug_on,diag_hcb,diag_hsn,diag_vis,        &
                              diag_vis1,diag_vis2,diag_vis3,diag_slw,                             &
                              scpf_on,scpf_pfrac,scpf_resfact,cldfrac,                            &
                              qi_type_1,qi_type_2,qi_type_3,qi_type_4,qi_type_5,qi_type_6,        &
                              qitot_1,qirim_1,nitot_1,birim_1,diag_effi_1,zitot_1,                &
                              qitot_2,qirim_2,nitot_2,birim_2,diag_effi_2,zitot_2,                &
                              qitot_3,qirim_3,nitot_3,birim_3,diag_effi_3,zitot_3,                &
                              qitot_4,qirim_4,nitot_4,birim_4,diag_effi_4,zitot_4)                &
                              result(end_status)

!------------------------------------------------------------------------------------------!
! This wrapper subroutine is the main GEM interface with the P3 microphysics scheme.  It   !
! prepares some necessary fields (converts temperature to potential temperature, etc.),    !
! passes 2D slabs (i,k) to the main microphysics subroutine ('P3_MAIN') -- which updates   !
! the prognostic variables (hydrometeor variables, temperature, and water vapor) and       !
! computes various diagnostics fields (precipitation rates, reflectivity, etc.) -- and     !
! finally converts the updated potential temperature to temperature.                       !
!------------------------------------------------------------------------------------------!

 implicit none

!----- input/ouput arguments:  ------------------------------------------------------------!

 integer, intent(in)                    :: ni                    ! number of columns in slab           -
 integer, intent(in)                    :: nk                    ! number of vertical levels           -
!integer, intent(in)                    :: n_iceCat              ! number of ice categories            -
 integer, intent(in)                    :: kount                 ! time step counter                   -
 integer, intent(in)                    :: trnch                 ! number of slice                     -
 integer, intent(in)                    :: n_diag_2d             ! number of 2D diagnostic fields      -
 integer, intent(in)                    :: n_diag_3d             ! number of 3D diagnostic fields      -

 real, intent(in)                       :: dt                    ! model time step                     s
 real, intent(in)                       :: dt_max                ! maximum timestep for microphysics   s
 real, intent(in)                       :: clbfact_dep           ! calibration factor for deposition
 real, intent(in)                       :: clbfact_sub           ! calibration factor for sublimation
 real, intent(inout), dimension(ni,nk)  :: qc                    ! cloud mixing ratio, mass            kg kg-1
 real, intent(inout), dimension(ni,nk)  :: nc                    ! cloud mixing ratio, number          #  kg-1
 real, intent(inout), dimension(ni,nk)  :: qr                    ! rain  mixing ratio, mass            kg kg-1
 real, intent(inout), dimension(ni,nk)  :: nr                    ! rain  mixing ratio, number          #  kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_1           ! ice   mixing ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_1           ! ice   mixing ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_1           ! ice   mixing ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_1           ! ice   mixing ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_1       ! ice   effective radius, (cat 1)     m
 real, intent(inout), dimension(ni,nk), optional  :: zitot_1     ! ice   mixing ratio, reflectivity    m^6 kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_2           ! ice   mixing ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_2           ! ice   mixing ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_2           ! ice   mixing ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_2           ! ice   mixing ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_2       ! ice   effective radius, (cat 2)     m
 real, intent(inout), dimension(ni,nk), optional  :: zitot_2     ! ice   mixing ratio, reflectivity    m^6 kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_3           ! ice   mixing ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_3           ! ice   mixing ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_3           ! ice   mixing ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_3           ! ice   mixing ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_3       ! ice   effective radius,  (cat 3)     m
 real, intent(inout), dimension(ni,nk), optional  :: zitot_3     ! ice   mixing ratio, reflectivity    m^6 kg-1

 real, dimension(:,:), pointer, contiguous  :: qitot_4           ! ice   mixing ratio, mass (total)    kg kg-1
 real, dimension(:,:), pointer, contiguous  :: qirim_4           ! ice   mixing ratio, mass (rime)     kg kg-1
 real, dimension(:,:), pointer, contiguous  :: nitot_4           ! ice   mixing ratio, number          #  kg-1
 real, dimension(:,:), pointer, contiguous  :: birim_4           ! ice   mixing ratio, volume          m3 kg-1
 real, dimension(:,:), pointer, contiguous  :: diag_effi_4       ! ice   effective radius, (cat 4)     m
 real, intent(inout), dimension(ni,nk), optional  :: zitot_4     ! ice   mixing ratio, reflectivity    m^6 kg-1

 real, intent(out), dimension(ni,nk) :: ttend                    ! temperature tendency                K s-1
 real, intent(out), dimension(ni,nk) :: qtend                    ! moisture tendency                   kg kg-1 s-1
 real, intent(out), dimension(ni,nk) :: qctend                   ! cloud water tendency                kg kg-1 s-1
 real, intent(out), dimension(ni,nk) :: qrtend                   ! cloud water tendency                kg kg-1 s-1
 real, intent(out), dimension(ni,nk) :: qitend                   ! total ice tendency                  kg kg-1 s-1

 real, intent(in),    dimension(ni,nk)  :: qvap_m                ! vapor mixing ratio (previous time) kg kg-1
 real, intent(inout), dimension(ni,nk)  :: qvap                  ! vapor mixing ratio, mass           kg kg-1
 real, intent(in),    dimension(ni,nk)  :: temp_m                ! temperature (previous time step)    K
 real, intent(inout), dimension(ni,nk)  :: temp                  ! temperature                         K
 real, intent(in),    dimension(ni)     :: psfc                  ! surface air pressure                Pa
 real, intent(in),    dimension(ni,nk)  :: gztherm               ! height AGL of thermodynamic levels  m
 real, intent(in),    dimension(ni,nk)  :: gzmom                 ! height AGL of momentum levels  m
 real, intent(in),    dimension(ni,nk)  :: sigma                 ! sigma = p(k,:)/psfc(:)
 real, intent(in),    dimension(ni,nk)  :: ww                    ! vertical motion                     m s-1
 real, intent(out),   dimension(ni)     :: prt_liq               ! precipitation rate, total liquid    m s-1
 real, intent(out),   dimension(ni)     :: prt_sol               ! precipitation rate, total solid     m s-1
 real, intent(out),   dimension(ni)     :: prt_drzl              ! precipitation rate, drizzle         m s-1
 real, intent(out),   dimension(ni)     :: prt_rain              ! precipitation rate, rain            m s-1
 real, intent(out),   dimension(ni)     :: prt_crys              ! precipitation rate, ice cystals     m s-1
 real, intent(out),   dimension(ni)     :: prt_snow              ! precipitation rate, snow            m s-1
 real, intent(out),   dimension(ni)     :: prt_grpl              ! precipitation rate, graupel         m s-1
 real, intent(out),   dimension(ni)     :: prt_pell              ! precipitation rate, ice pellets     m s-1
 real, intent(out),   dimension(ni)     :: prt_hail              ! precipitation rate, hail            m s-1
 real, intent(out),   dimension(ni)     :: prt_sndp              ! precipitation rate, unmelted snow   m s-1
 real, intent(out),   dimension(ni,nk)  :: diag_Zet              ! equivalent reflectivity, 3D         dBZ
 real, intent(out),   dimension(ni)     :: diag_Zec              ! equivalent reflectivity, col-max    dBZ
 real, intent(out),   dimension(ni,nk)  :: diag_effc             ! effective radius, cloud             m
 real, intent(out),   dimension(ni,n_diag_2d)    :: diag_2d      ! user-defined 2D diagnostic fields
 real, intent(out),   dimension(ni,nk,n_diag_3d) :: diag_3d      ! user-defined 3D diagnostic fields
!real, intent(out),   dimension(ni,nk,n_qiType  ):: qi_type      ! mass mixing ratio, diag ice type    kg kg-1

 real, intent(out),   dimension(ni,nk)  :: qi_type_1             ! small ice crystal mass              kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_2             ! unrimed snow crystal mass           kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_3             ! lightly rimed snow mass             kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_4             ! graupel mass                        kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_5             ! hail mass                           kg kg-1
 real, intent(out),   dimension(ni,nk)  :: qi_type_6             ! ice pellet mass                     kg kg-1

 real, intent(out),   dimension(ni)     :: diag_hcb              ! height of cloud base                m
 real, intent(out),   dimension(ni)     :: diag_hsn              ! height of snow level                m
 real, intent(out),   dimension(ni,nk)  :: diag_vis              ! visibility (total)                  m
 real, intent(out),   dimension(ni,nk)  :: diag_vis1             ! visibility through liquid fog       m
 real, intent(out),   dimension(ni,nk)  :: diag_vis2             ! visibility through rain             m
 real, intent(out),   dimension(ni,nk)  :: diag_vis3             ! visibility through snow             m
 real, intent(out),   dimension(ni,nk)  :: diag_slw              ! supercooled LWC                     kg m-3

 logical, intent(in)                    :: debug_on              ! logical switch for internal debug checks
 logical, intent(in)                    :: scpf_on               ! switch for activation of SCPF scheme
 real,    intent(in)                    :: scpf_pfrac            ! precipitation fraction factor (SCPF)
 real,    intent(in)                    :: scpf_resfact          ! model resolution factor (SCPF)
 real,    intent(out), dimension(ni,nk) :: cldfrac               ! cloud fraction computed by SCPF

!----------------------------------------------------------------------------------------!

!----- local variables and parameters:
 real, dimension(ni,nk,n_iceCat)  :: qitot      ! ice mixing ratio, mass (total)          kg kg-1
 real, dimension(ni,nk,n_iceCat)  :: qirim      ! ice mixing ratio, mass (rime)           kg kg-1
 real, dimension(ni,nk,n_iceCat)  :: nitot      ! ice mixing ratio, number                #  kg-1
 real, dimension(ni,nk,n_iceCat)  :: birim      ! ice mixing ratio, volume                m3 kg-1
 real, dimension(ni,nk,n_iceCat)  :: zitot      ! ice mixing ratio, reflectivity          m6 kg-1
 real, dimension(ni,nk,n_iceCat)  :: diag_effi  ! effective radius, ice                   m
 real, dimension(ni,nk,n_iceCat)  :: diag_vmi   ! mass-weighted fall speed, ice           m s-1  (returned but not used)
 real, dimension(ni,nk,n_iceCat)  :: diag_di    ! mean diameter, ice                      m      (returned but not used)
 real, dimension(ni,nk,n_iceCat)  :: diag_rhoi  ! bulk density, ice                       kg m-3 (returned but not used)

 real, dimension(ni,nk)  :: theta_m             ! potential temperature (previous step)   K
 real, dimension(ni,nk)  :: qvapm               ! qv (previous step)                      kg kg-1
 real, dimension(ni,nk)  :: theta               ! potential temperature                   K
 real, dimension(ni,nk)  :: pres                ! pressure                                Pa
 real, dimension(ni,nk)  :: DZ                  ! difference in height between levels     m
 real, dimension(ni,nk)  :: ssat                ! supersaturation
 real, dimension(ni,nk)  :: tmparr_ik           ! temporary array (for optimization)
 real, dimension(ni,nk)  :: qqdelta,ttdelta     ! for sub_stepping
 real, dimension(ni,nk)  :: iwc                 ! total ice water content
 real, dimension(ni,nk)  :: temp0, qvap0, qc0, qr0, iwc0 ! incoming state variables

 real, dimension(ni,nk,n_qiType) :: qi_type     ! diagnostic precipitation types

 real, dimension(ni)     :: prt_liq_ave,prt_sol_ave,rn1_ave,rn2_ave,sn1_ave, &  ! ave pcp rates over full timestep
                            sn2_ave,sn3_ave,pe1_ave,pe2_ave,snd_ave
 real                    :: dt_mp                                               ! timestep used by microphsyics (for substepping)
 real                    :: tmp1, idt

 integer                 :: i,k,ktop,kbot,kdir,i_strt,k_strt,i_substep,n_substep,end_status,tmpint1

 logical                 :: log_tmp1,log_tmp2,log_trplMomI
 logical, parameter      :: log_predictNc = .true.      ! temporary; to be put as GEM namelist
 logical, parameter      :: typeDiags_ON  = .true.      ! switch for hydrometeor/precip type diagnostics
 real, parameter         :: SMALL_ICE_MASS = 1e-14      ! threshold for very small specific ice content

 character(len=16), parameter :: model = 'GEM'

!----------------------------------------------------------------------------------------!

   end_status = STATUS_ERROR

   tmpint1 = trnch !not used; prevents "variable not used" compiler message

   i_strt = 1  ! beginning index of slab
   k_strt = 1  ! beginning index of column

   ktop  = 1   ! k index of top level
   kbot  = nk  ! k index of bottom level
   kdir  = -1  ! direction of vertical leveling for 1=bottom, nk=top

   log_trplMomI = present(zitot_1)

   !compute time step and number of steps for substepping
   idt = 1./dt
   n_substep = int((dt-0.1)/max(0.1,dt_max)) + 1
   dt_mp = dt/float(n_substep)

   ! Save initial state for tendency calculation and reset
   temp0(:,:) = temp(:,:)
   qvap0(:,:) = qvap(:,:)
   qc0(:,:) = qc(:,:)
   qr0(:,:) = qr(:,:)
   iwc0(:,:) = qitot_1(:,:)
   if (n_iceCat > 1) iwc0(:,:) = iwc0(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) iwc0(:,:) = iwc0(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) iwc0(:,:) = iwc0(:,:) + qitot_4(:,:)

   ! Note qqdelta is converted from specific to mixing ratio
   qqdelta = (qvap/(1-qvap)-qvap_m/(1-qvap_m)) / float(n_substep)
   ttdelta = (temp-temp_m) / float(n_substep)
   ! initialise for the 1st substepping
   qvap = qvap_m/(1-qvap_m) ! mixing ratio instead of specific humidity
   temp = temp_m

  !if (kount == 0) then
   if (.false.) then
      print*,'Microphysics (MP) substepping:'
      print*,'  GEM model time step  : ',dt
      print*,'  MP time step         : ',dt_mp
      print*,'  number of MP substeps: ',n_substep
   endif

 ! note: code for prediction of ssat not currently avaiable, thus array is to 0
   ssat = 0.

  !air pressure:
   do k = kbot,ktop,kdir
      pres(:,k)= psfc(:)*sigma(:,k)
   enddo

  !layer thickness (for sedimentation):
  ! do k = kbot,ktop-kdir,kdir
  !    DZ(:,k) = gztherm(:,k+kdir) - gztherm(:,k)
  ! enddo
  ! DZ(:,ktop) = DZ(:,ktop-kdir)

  !layer thickness (for sedimentation):
  !  note: This is the thickness of the layer "centered" at thermodynamic level k,
  !        computed based on the surrounding momentum levels.
   do k = kbot-1,ktop,kdir
      DZ(:,k) = gzmom(:,k) - gzmom(:,k-kdir)
   enddo
   DZ(:,kbot) = gzmom(:,kbot)

  !compute zitot from advected 'Z' variable (for triple-moment ice):
   if (present(zitot_1)) then
      where (nitot_1>0.)
         zitot_1 = zitot_1**2/nitot_1
      elsewhere
         zitot_1 = 0.
      endwhere
   endif
   if (present(zitot_2)) then
      where (nitot_2>0.)
         zitot_2 = zitot_2**2/nitot_2
      elsewhere
         zitot_2 = 0.
      endwhere
   endif
   if (present(zitot_3)) then
      where (nitot_3>0.)
         zitot_3 = zitot_3**2/nitot_3
      elsewhere
         zitot_3 = 0.
      endwhere
   endif
   if (present(zitot_4)) then
      where (nitot_4>0.)
         zitot_4 = zitot_4**2/nitot_4
      elsewhere
         zitot_4 = 0.
      endwhere
   endif

  !contruct full ice arrays from individual category arrays:
   qitot(:,:,1) = qitot_1(:,:)
   qirim(:,:,1) = qirim_1(:,:)
   nitot(:,:,1) = nitot_1(:,:)
   birim(:,:,1) = birim_1(:,:)
   diag_effi(:,:,1) = diag_effi_1(:,:)
   if (present(zitot_1)) zitot(:,:,1) = zitot_1(:,:)

   if (n_iceCat >= 2) then
      qitot(:,:,2) = qitot_2(:,:)
      qirim(:,:,2) = qirim_2(:,:)
      nitot(:,:,2) = nitot_2(:,:)
      birim(:,:,2) = birim_2(:,:)
      diag_effi(:,:,2) = diag_effi_2(:,:)
      if (present(zitot_2)) zitot(:,:,2) = zitot_2(:,:)

      if (n_iceCat >= 3) then
         qitot(:,:,3) = qitot_3(:,:)
         qirim(:,:,3) = qirim_3(:,:)
         nitot(:,:,3) = nitot_3(:,:)
         birim(:,:,3) = birim_3(:,:)
         diag_effi(:,:,3) = diag_effi_3(:,:)
         if (present(zitot_3)) zitot(:,:,3) = zitot_3(:,:)

         if (n_iceCat == 4) then
            qitot(:,:,4) = qitot_4(:,:)
            qirim(:,:,4) = qirim_4(:,:)
            nitot(:,:,4) = nitot_4(:,:)
            birim(:,:,4) = birim_4(:,:)
            diag_effi(:,:,4) = diag_effi_4(:,:)
            if (present(zitot_4)) zitot(:,:,4) = zitot_4(:,:)
         endif
      endif
   endif

  !--- substepping microphysics
   if (n_substep > 1) then
      prt_liq_ave(:) = 0.
      prt_sol_ave(:) = 0.
      rn1_ave(:) = 0.
      rn2_ave(:) = 0.
      sn1_ave(:) = 0.
      sn2_ave(:) = 0.
      sn3_ave(:) = 0.
      pe1_ave(:) = 0.
      pe2_ave(:) = 0.
      snd_ave(:) = 0.
   endif

   tmparr_ik = (1.e+5/pres)**(rd*inv_cp)  !for optimization of calc of theta, temp

   substep_loop: do i_substep = 1, n_substep

     !convert to potential temperature:
     qvapm   = qvap
     qvap    = qvap+qqdelta
     theta_m = temp*tmparr_ik
     temp    = temp+ttdelta
     theta   = temp*tmparr_ik

      if (log_trplMomI) then
            call p3_main(qc,nc,qr,nr,theta_m,theta,qvapm,qvap,dt_mp,qitot,qirim,nitot,birim,    &
                   ssat,ww,pres,DZ,kount,prt_liq,prt_sol,i_strt,ni,k_strt,nk,n_iceCat,          &
                   diag_Zet,diag_effc,diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d,   &
                   n_diag_3d,diag_3d,log_predictNc,typeDiags_ON,trim(model),clbfact_dep,        &
                   clbfact_sub,debug_on,scpf_on,scpf_pfrac,scpf_resfact,cldfrac,prt_drzl,       &
                   prt_rain,prt_crys,prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,qi_type,      &
                   zitot     = zitot,                                                           &
                   diag_vis  = diag_vis,                                                        &
                   diag_vis1 = diag_vis1,                                                       &
                   diag_vis2 = diag_vis2,                                                       &
                   diag_vis3 = diag_vis3)
         else
            call p3_main(qc,nc,qr,nr,theta_m,theta,qvapm,qvap,dt_mp,qitot,qirim,nitot,birim,    &
                   ssat,ww,pres,DZ,kount,prt_liq,prt_sol,i_strt,ni,k_strt,nk,n_iceCat,          &
                   diag_Zet,diag_effc,diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d,   &
                   n_diag_3d,diag_3d,log_predictNc,typeDiags_ON,trim(model),clbfact_dep,        &
                   clbfact_sub,debug_on,scpf_on,scpf_pfrac,scpf_resfact,cldfrac,prt_drzl,       &
                   prt_rain,prt_crys,prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,qi_type,      &
                   diag_vis  = diag_vis,                                                        &
                   diag_vis1 = diag_vis1,                                                       &
                   diag_vis2 = diag_vis2,                                                       &
                   diag_vis3 = diag_vis3)
         endif

      if (global_status /= STATUS_OK) return

     !convert back to temperature:
      temp = theta/tmparr_ik    !i.e.: temp = theta*(pres*1.e-5)**(rd*inv_cp)

      if (n_substep > 1) then
         prt_liq_ave(:) = prt_liq_ave(:) + prt_liq(:)
         prt_sol_ave(:) = prt_sol_ave(:) + prt_sol(:)
         rn1_ave(:) = rn1_ave(:) + prt_drzl(:)
         rn2_ave(:) = rn2_ave(:) + prt_rain(:)
         sn1_ave(:) = sn1_ave(:) + prt_crys(:)
         sn2_ave(:) = sn2_ave(:) + prt_snow(:)
         sn3_ave(:) = sn3_ave(:) + prt_grpl(:)
         pe1_ave(:) = pe1_ave(:) + prt_pell(:)
         pe2_ave(:) = pe2_ave(:) + prt_hail(:)
         snd_ave(:) = snd_ave(:) + prt_sndp(:)
      endif

   enddo substep_loop

   ! retransferring mixing ratio to specific humidity (only t* is needed)
   qvap = qvap/(1+qvap)

   if (n_substep > 1) then
      tmp1 = 1./float(n_substep)
      prt_liq(:)  = prt_liq_ave(:)*tmp1
      prt_sol(:)  = prt_sol_ave(:)*tmp1
      prt_drzl(:) = rn1_ave(:)*tmp1
      prt_rain(:) = rn2_ave(:)*tmp1
      prt_crys(:) = sn1_ave(:)*tmp1
      prt_snow(:) = sn2_ave(:)*tmp1
      prt_grpl(:) = sn3_ave(:)*tmp1
      prt_pell(:) = pe1_ave(:)*tmp1
      prt_hail(:) = pe2_ave(:)*tmp1
      prt_sndp(:) = snd_ave(:)*tmp1
   endif

  !===


  !decompose full ice arrays back into individual category arrays:
   qitot_1(:,:) = qitot(:,:,1)
   qirim_1(:,:) = qirim(:,:,1)
   nitot_1(:,:) = nitot(:,:,1)
   birim_1(:,:) = birim(:,:,1)
   if (present(zitot_1)) zitot_1(:,:) = zitot(:,:,1)
   where (qitot_1(:,:) >= SMALL_ICE_MASS)
      diag_effi_1(:,:) = diag_effi(:,:,1)
   elsewhere
      diag_effi_1(:,:) = 0.
   endwhere

   if (n_iceCat >= 2) then
      qitot_2(:,:) = qitot(:,:,2)
      qirim_2(:,:) = qirim(:,:,2)
      nitot_2(:,:) = nitot(:,:,2)
      birim_2(:,:) = birim(:,:,2)
      if (present(zitot_2)) zitot_2(:,:) = zitot(:,:,2)
      where (qitot_2(:,:) >= SMALL_ICE_MASS)
         diag_effi_2(:,:) = diag_effi(:,:,2)
      elsewhere
         diag_effi_2(:,:) = 0.
      endwhere

      if (n_iceCat >= 3) then
         qitot_3(:,:) = qitot(:,:,3)
         qirim_3(:,:) = qirim(:,:,3)
         nitot_3(:,:) = nitot(:,:,3)
         birim_3(:,:) = birim(:,:,3)
         if (present(zitot_3)) zitot_3(:,:) = zitot(:,:,3)
         where (qitot_3(:,:) >= SMALL_ICE_MASS)
            diag_effi_3(:,:) = diag_effi(:,:,3)
         elsewhere
            diag_effi_3(:,:) = 0.
         endwhere

         if (n_iceCat == 4) then
            qitot_4(:,:) = qitot(:,:,4)
            qirim_4(:,:) = qirim(:,:,4)
            nitot_4(:,:) = nitot(:,:,4)
            birim_4(:,:) = birim(:,:,4)
            if (present(zitot_4)) zitot_4(:,:) = zitot(:,:,4)
            where (qitot_4(:,:) >= SMALL_ICE_MASS)
               diag_effi_4(:,:) = diag_effi(:,:,4)
            elsewhere
               diag_effi_4(:,:) = 0.
            endwhere
         endif
      endif
   endif

  !convert zitot to advected 'Z' variable:
   if (present(zitot_1)) zitot_1 = sqrt(nitot_1*zitot_1)
   if (present(zitot_2)) zitot_2 = sqrt(nitot_2*zitot_2)
   if (present(zitot_3)) zitot_3 = sqrt(nitot_3*zitot_3)
   if (present(zitot_4)) zitot_4 = sqrt(nitot_4*zitot_4)


  !convert precip rates from volume flux (m s-1) to mass flux (kg m-2 s-1):
  ! (since they are computed back to liq-eqv volume flux in s/r 'ccdiagnostics.F90')
   prt_liq = prt_liq*1000.
   prt_sol = prt_sol*1000.

  !--- diagnostics:
   diag_hcb(:) = -1.
   diag_hsn(:) = -1.

   do i = 1,ni

    !composite (column-maximum) reflectivity:
      diag_Zec(i) = maxval(diag_Zet(i,:))

    !diagnostic heights:
      log_tmp1 = .false.  !cloud base height found
      log_tmp2 = .false.  !snow level height found
      do k = nk,2,-1
        !cloud base height:
         if (qc(i,k)>1.e-6 .and. .not.log_tmp1) then
            diag_hcb(i) = gztherm(i,k)
            log_tmp1 = .true.
         endif
        !snow level height:  (height of lowest level with ice) [for n_iceCat=1 only]
         if (qitot_1(i,k)>1.e-6 .and. .not.log_tmp2) then
            diag_hsn(i) = gztherm(i,k)
            log_tmp2 = .true.
         endif
      enddo

    !supercooled LWC:
      do k = 1,nk
         if (temp(i,k)<273.15) then
            tmp1 = pres(i,k)/(287.15*temp(i,k))  !air density
            diag_slw(i,k) = tmp1*(qc(i,k)+qr(i,k))
         else
            diag_slw(i,k) = 0.
         endif
      enddo

   enddo  !i-loop

   ! Diagnostic ice particle types:
   if (n_qiType >= 6) then
      qi_type_1 = qi_type(:,:,1)  !small ice crystals
      qi_type_2 = qi_type(:,:,2)  !unrimed snow crystals
      qi_type_3 = qi_type(:,:,3)  !lightly rimed snow
      qi_type_4 = qi_type(:,:,4)  !graupel
      qi_type_5 = qi_type(:,:,5)  !hail
      qi_type_6 = qi_type(:,:,6)  !ice pellets
   else
      call physeterror('microphy_p3::mp_p3_wrapper_gem', &
           'Insufficient size for qi_type')
      return
   endif

   ! Compute tendencies and reset state
   iwc(:,:) =  qitot_1(:,:)
   if (n_iceCat > 1) iwc(:,:) = iwc(:,:) + qitot_2(:,:)
   if (n_iceCat > 2) iwc(:,:) = iwc(:,:) + qitot_3(:,:)
   if (n_iceCat > 3) iwc(:,:) = iwc(:,:) + qitot_4(:,:)
   ttend(:,:) = (temp(:,:) - temp0(:,:)) * idt
   qtend(:,:) = (qvap(:,:) - qvap0(:,:)) * idt
   qctend(:,:) = (qc(:,:) - qc0(:,:)) * idt
   qrtend(:,:) = (qr(:,:) - qr0(:,:)) * idt
   qitend(:,:) = (iwc(:,:) - iwc0(:,:)) * idt
   temp(:,:) = temp0(:,:)
   qvap(:,:) = qvap0(:,:)
   qc(:,:) = qc0(:,:)
   qr(:,:) = qr0(:,:)

   end_status = STATUS_OK
   return

 end function mp_p3_wrapper_gem

#endif
 
!==========================================================================================!

 SUBROUTINE compute_SCPF(Qcond,Qprec,Qv,Qsi,Pres,ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,       &
                         SPF_clr,Qv_cld,Qv_clr,cldFrac_on,pfrac,resfact,quick)

!------------------------------------------------------------------------------------------!
! This subroutine computes the cloud and precipitation fractions.  It also provide         !
! in-cloud/clear sky water vapor mixing ratios and the inverse of "cloud" and              !
! precipitation fractions to ease computation in s/r 'p3_main'. It is called 3 times:      !
!                                                                                          !
! 1. Before microphysics source/sink terms and following updates of grid-mean fields       !
! 2. Before sedimentation                                                                  !
! 3. At the end of 'p3_main' (to provide cloud fraction to the driving model               !
!    (e.g. for the radiation scheme, diagnostics, etc.)                                    !
!                                                                                          !
! For details see:  Chosson et al. (2014) [J. Atmos. Sci., 71, 2635-2653]                  !
!                                                                                          !
! NOTES:                                                                                   !
!   'scpf_resfact' is the user-specified scaled horizontal grid spacing, which allows the  !
!   RH threshold to adapt to the model resolution (i.e. to be "scale aware").              !
!   The current recommendation is:  scpf_resfact = sqrt(dx/dx_ref). where dx_ref = 12 km   !
!                                                                                          !
!------------------------------------------------------------------------------------------!
!      Version 1:    April 2016,  Frederick Chosson (ECCC)                                 !
!                    This version is not "scale aware" and RHcrit is from Sundqvist RDPS   !
!                    but without dependency on T (RHcriterion -RHoo- cst in free atm.)     !
!                    This version have a very low optimisation level                       !
!                                                                                          !
!      Version 2:    November 2016, Frederick Chosson (ECCC)                               !
!                    add minimum Cloud and Precipitation Fraction to  1%                   !
!                    add maximum Cloud and Precipitation Fraction to 99%                   !
!                                                                                          !
!      Version 3:    June 2018, Caroline Jouan (ECCC)                                      !
!                    Tests in GEM models                                                   !
!                                                                                          !
!------------------------------------------------------------------------------------------!

 implicit none

!----- input/ouput arguments:  ----------------------------------------------------------!
 real, intent(in),  dimension(:) :: Qcond     ! Condensates mix.ratio that goes in the "Cloudy fraction"
 real, intent(in),  dimension(:) :: Qprec     ! Condensates mix.ratio that goes in the "Precip fraction"
 real, intent(in),  dimension(:) :: Qv        ! Water vapor mix.ratio (grid mean)
 real, intent(in),  dimension(:) :: Qsi       ! Saturation Water vapor mix.ratio w.r.t. ice or liq, dep. on T
 real, intent(in),  dimension(:) :: pres      ! pressure in Pa
 real, intent(out), dimension(:) :: SCF,iSCF  ! Subgrid "Cloudy" fraction (fraction where RH>100%) and inverse
 real, intent(out), dimension(:) :: SPF,iSPF  ! Subgrid "Precip" fraction and inverse
 real, intent(out), dimension(:) :: SPF_clr   ! Subgrid "Precip" fraction in clear sky (not overlap cloud)
 real, intent(out), dimension(:) :: Qv_cld    ! Water vapor mix.ratio     in "Cloudy" fraction
 real, intent(out), dimension(:) :: Qv_clr    ! Water vapor mix.ratio NOT in "Cloudy" fraction
 real, intent(in)                :: pfrac     ! precipitation fraction factor
 real, intent(in)                :: resfact   ! model resolution factor
 integer, intent(in)             :: ktop,kbot ! indices of model top and bottom
 integer, intent(in)             :: kdir      ! indice  for direction from bottom to top
 logical, intent(in)             :: quick     ! switch if you only need SCF as output, not the rest (3rd call)
 logical, intent(in)             :: cldFrac_on! switch if you only need SCF or set it to 1.


!----- local variables and parameters: --------------------------------------------------!
 real, dimension(size(Qv,dim=1)) :: C        ! Total cloud cover form top to level k
 real, parameter :: SIG_min = 0.7            ! minimum of sigma level below wich RHoo start to increase
 real, parameter :: SIG_max = 0.9            ! maximum of sigma level below wich RHoo stop  to increase
 real, parameter :: xo      = 1.-1.e-6       ! a number very close but less than 1.
 real            :: RHoo_min                 ! minimum of relative humidity criterion for dx around 12km
 real            :: RHoo_max                 ! maximum of relative humidity criterion for dx around 12km
 real            :: slope                    ! scale factor=(RHoo_max-RHoo_min)/(SIG_min-SIG_max)
 real            :: RHoo                     ! Relative humidity criterion above which saturation appears
 real            :: Qtot,DELTA_Qtot          ! Total "cloudy" condensate and the half-width of its PDF
 real            :: D_A_cld2clr              ! Area of cloudy precips. that fall in clear air below
 real            :: D_A_clr2cld              ! Area of clear air precips that fall into cloud below
 real            :: D_C                      ! Area never concerned by precips from top to level k
 real            :: SPF_cld                  ! area of cloudy precips at level k
 real            :: SPF_cld_k_1              ! area of cloudy precips at level k+kdir (just above)
 real            :: sigma                    ! sigma level = P / Psurf with Psurf=P(:,kbot)
 real            :: tmp7                     ! temporary SPF
 integer         :: k                        ! vertical loop index

 compute_cloud_fraction: if (cldFrac_on) then

   ! initialise constants
    RHoo_min = 1.-(1.-0.85 )*resfact         ! minimum of relative humidity criterion for dx ~ 12 km by default
    RHoo_max = 1.-(1.-0.975)*resfact         ! maximum of relative humidity criterion for dx ~ 12 km
    slope    = (RHoo_max-RHoo_min)/(SIG_max-SIG_min)

   ! Initiate Cloud fractions overlaps to zero
    SCF(:)      = 0.;      iSCF(:)    = 0.;     D_A_cld2clr = 0.
    D_A_clr2cld = 0.;      C(:)       = 0.;     D_C         = 0.
    SPF_cld     = 0.;      SPF_clr(:) = 0.;     SPF(:)      = 0.
    iSPF(:)     = 0.;      Qv_cld(:)  = 0.;     Qv_clr(:)   = 0.
    SPF_cld_k_1 = 0.

    Loop_SCPF_k: do k = ktop-kdir,kbot,-kdir

       sigma = pres(k)/pres(kbot)                     ! sigma level
       RHoo  = RHoo_min + slope*(sigma-SIG_min )      ! critical relative humidity
       RHoo  = max( RHoo_min, min( RHoo_max, RHoo ) ) ! bounded

       !------------------------------------------------------------
       ! COMPUTE CLOUD FRACTION AND in-FRACTIONS WATER VAPOR CONTENT
       !------------------------------------------------------------
       Qtot       = Qv(k)+Qcond(k)                            ! Total "Cloudy" mean water mixing ratio
       DELTA_Qtot = Qsi(k)*(1.-RHoo)                          ! half-width of Qtot subgrid PDF
       SCF(k)     = 0.5*(Qtot+DELTA_Qtot-QSI(k))/DELTA_Qtot   ! subgrid cloud fraction

       if (SCF(k) .lt. 0.01 ) then          ! minimum allowed Cloud fraction (below it is clear-sky)
          SCF(k)    = 0.                    ! inverse of Cloud cover
          iSCF(k)   = 0.                    ! inverse of Cloud cover
          Qv_cld(k) = 0.                    ! water vapour mix. ratio in Cloudy part
          Qv_clr(k) = Qv(k)                 ! water vapour mix. ratio in Clear sky part
       elseif (SCF(k) .lt. 0.99 ) then
          iSCF(k)   = 1./SCF(k)             ! beware: Could be big!
          Qv_cld(k) = 0.5*(Qtot+DELTA_Qtot+QSI(k))-Qcond(k)*iSCF(k)
          Qv_clr(k) = 0.5*(Qtot-DELTA_Qtot+QSI(k))
       else ! if SCF >= 0.99
          SCF(k)    = 1.
          iSCF(k)   = 1.
          Qv_cld(k) = Qv(k)
          Qv_clr(k) = 0.
       endif

       !------------------------------------------------------------
       ! COMPUTE CLOUD AND PRECIPITATION FRACTIONS OVERLAPS
       !------------------------------------------------------------
       if (.not. quick) then

         ! This is the total max-random cloud-cover from top to level k
         C(k) = 1.-(1.-C(k+kdir))*(1.-max(SCF(k),SCF(k+kdir)))/(1.-min(SCF(k+kdir),xo))
         ! Change in total cloud-cover: this part is never concerned by precips
         D_C = C(k)-C(k+kdir)
         ! Cloudy precipitation fraction at level k+kdir (level above)
         SPF_cld_k_1 = SPF(k+kdir)-SPF_clr(k+kdir)
         ! fraction for which cloudy precip. falls into clear air below
         D_A_cld2clr = SPF_cld_k_1 - min(SCF(k)-D_C,SPF_cld_k_1)
         ! fraction for which clear-sky precip. falls into cloudy air below
         D_A_clr2cld = max(0., min(SPF_clr(k+kdir),SCF(k)-D_C-SCF(k+kdir)) )
         ! fraction of cloudy precips at level k
         SPF_cld = SPF_cld_k_1 + D_A_clr2cld - D_A_cld2clr
         if (SPF_cld .le. 0.) SPF_cld=SCF(k)*Pfrac
         ! fraction of clear-sky precips at level k
         SPF_clr(k) = SPF_clr(k+kdir) - D_A_clr2cld + D_A_cld2clr
         ! if there is no precips set precips areas to zero
         tmp7 = (SPF_clr(k)+SPF_cld)

         if (tmp7.gt.0.) then
           if ((Qprec(k)/tmp7<qsmall ) .or. (Qprec(k+kdir)*iSPF(k+kdir)<qsmall)) then
              SPF_cld    = SCF(k+kdir)*Pfrac
              SPF_clr(k) = 0.
           endif
         endif

         SPF(k) = (SPF_clr(k) + SPF_cld)             ! subgrid area of precipitation
         if (SPF(k) .ge. 0.01) then
            iSPF(k)= 1. / SPF(k)                     ! inverse of precip fraction
         else
            if (Qprec(k) .ge. qsmall) then
               SPF(k)     = max(0.01, SCF(k+kdir))   ! in case of slant-wise rain precipitating
               SPF_clr(k) = SPF(k)                   ! assume at least 1% SPF in clear-sky
               iSPF(k)    = 1./SPF(k)
            else
               iSPF(k)    = 0.
               SPF(k)     = 0.
               SPF_clr(k) = 0.
            endif
         endif

       endif ! end of IF NOT quick

       if ((SCF(k) .lt. 0.01) .and. (Qcond(k) > qsmall) ) then  ! avoid bad clipping
           SCF(k)    = max(0.01, SCF(k+kdir))                   ! in case of cloudy species precipitating
          iSCF(k)    = 1./SCF(k)                                ! into unsaturated layer
          Qv_cld(k)  = Qv(k)
          Qv_clr(k)  = Qv(k)
          SPF_clr(k) = max(SPF(k)-SCF(k),0.)
       endif

    enddo Loop_SCPF_k

 else  ! compute_cloud_fraction

    SCF  = 1.
    iSCF = 1.
    SPF  = 1.
    iSPF = 1.
    SPF_clr = 0.
    Qv_cld  = Qv
    Qv_clr  = 0.

 endif compute_cloud_fraction

 END SUBROUTINE compute_SCPF

!==========================================================================================!

 SUBROUTINE p3_main(qc,nc,qr,nr,th_old,th,qv_old,qv,dt,qitot,qirim,nitot,birim,ssat,uzpl, &
                    pres,dzq,it,prt_liq,prt_sol,its,ite,kts,kte,nCat,diag_ze,diag_effc,   &
                    diag_effi,diag_vmi,diag_di,diag_rhoi,n_diag_2d,diag_2d,n_diag_3d,     &
                    diag_3d,log_predictNc,typeDiags_ON,model,clbfact_dep,clbfact_sub,     &
                    debug_on,scpf_on,scpf_pfrac,scpf_resfact,SCF_out,prt_drzl,prt_rain,   &
                    prt_crys,prt_snow,prt_grpl,prt_pell,prt_hail,prt_sndp,qi_type,        &
                    zitot,diag_vis,diag_vis1,diag_vis2,diag_vis3,diag_dhmax,diag_lami,    &
                    diag_mui)

!----------------------------------------------------------------------------------------!
!                                                                                        !
! This is the main subroutine for the P3 microphysics scheme.  It is called from the     !
! wrapper subroutine ('MP_P3_WRAPPER') and is passed i,k slabs of all prognostic         !
! variables -- hydrometeor fields, potential temperature, and water vapor mixing ratio.  !
! Microphysical process rates are computed first.  These tendencies are then used to     !
! computed updated values of the prognostic variables.  The hydrometeor variables are    !
! then updated further due to sedimentation.                                             !
!                                                                                        !
! Several diagnostic values are also computed and returned to the wrapper subroutine,    !
! including precipitation rates.                                                         !
!                                                                                        !
!----------------------------------------------------------------------------------------!

 implicit none

!----- Input/ouput arguments:  ----------------------------------------------------------!

 integer, intent(in)                                  :: its,ite    ! array bounds (horizontal)
 integer, intent(in)                                  :: kts,kte    ! array bounds (vertical)
 integer, intent(in)                                  :: nCat       ! number of ice-phase categories
 integer, intent(in)                                  :: n_diag_2d  ! number of 2D diagnostic fields
 integer, intent(in)                                  :: n_diag_3d  ! number of 3D diagnostic fields

 real, intent(inout), dimension(its:ite,kts:kte)      :: qc         ! cloud, mass mixing ratio         kg kg-1
! note: Nc may be specified or predicted (set by log_predictNc)
 real, intent(inout), dimension(its:ite,kts:kte)      :: nc         ! cloud, number mixing ratio       #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: qr         ! rain, mass mixing ratio          kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: nr         ! rain, number mixing ratio        #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qitot      ! ice, total mass mixing ratio     kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: qirim      ! ice, rime mass mixing ratio      kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: nitot      ! ice, total number mixing ratio   #  kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat) :: birim      ! ice, rime volume mixing ratio    m3 kg-1

 real, intent(inout), dimension(its:ite,kts:kte)      :: ssat       ! supersaturation (i.e., qv-qvs)   kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: qv         ! water vapor mixing ratio         kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte)      :: th         ! potential temperature            K
 real, intent(inout), dimension(its:ite,kts:kte)      :: th_old     ! beginning of time step value of theta K
 real, intent(inout), dimension(its:ite,kts:kte)      :: qv_old     ! beginning of time step value of qv    kg kg-1
 real, intent(in),    dimension(its:ite,kts:kte)      :: uzpl       ! vertical air velocity            m s-1
 real, intent(in),    dimension(its:ite,kts:kte)      :: pres       ! pressure                         Pa
 real, intent(in),    dimension(its:ite,kts:kte)      :: dzq        ! vertical grid spacing            m
 real, intent(in)                                     :: dt         ! model time step                  s
 real, intent(in)                                     :: clbfact_dep! calibration factor for deposition
 real, intent(in)                                     :: clbfact_sub! calibration factor for sublimation

 real, intent(out),   dimension(its:ite)              :: prt_liq    ! precipitation rate, liquid       m s-1
 real, intent(out),   dimension(its:ite)              :: prt_sol    ! precipitation rate, solid        m s-1
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_ze    ! equivalent reflectivity          dBZ
 real, intent(out),   dimension(its:ite,kts:kte)      :: diag_effc  ! effective radius, cloud          m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_effi  ! effective radius, ice            m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_vmi   ! mass-weighted fall speed of ice  m s-1
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_di    ! mean diameter of ice             m
 real, intent(out),   dimension(its:ite,kts:kte,nCat) :: diag_rhoi  ! bulk density of ice              kg m-1
 real, intent(out),   dimension(its:ite,kts:kte,nCat), optional :: diag_dhmax ! maximum hail size                m
 real, intent(out),   dimension(its:ite,kts:kte,nCat), optional :: diag_lami  ! lambda parameter for ice PSD     m-1
 real, intent(out),   dimension(its:ite,kts:kte,nCat), optional :: diag_mui   ! mu parameter for ice PSD

!! real, intent(out),   dimension(its:ite,kts:kte,nCat), optional :: diag_Dhm  ! maximum hail diameter   m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis   ! visibility (total)          m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis1  ! visibility through fog      m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis2  ! visibility through rain     m
 real, intent(out),   dimension(its:ite,kts:kte), optional :: diag_vis3  ! visibility through snow     m
 real, intent(out),   dimension(its:ite,n_diag_2d)         :: diag_2d    ! user-defined 2D diagnostic fields
 real, intent(out),   dimension(its:ite,kts:kte,n_diag_3d) :: diag_3d    ! user-defined 3D diagnostic fields

 integer, intent(in)                                  :: it         ! time step counter NOTE: starts at 1 for first time step

 logical, intent(in)                                  :: log_predictNc ! .T. (.F.) for prediction (specification) of Nc
 logical, intent(in)                                  :: typeDiags_ON  !for diagnostic hydrometeor/precip rate types
 logical, intent(in)                                  :: debug_on      !switch for internal debug checks
 character(len=*), intent(in)                         :: model         !driving model

 real, intent(out), dimension(its:ite), optional      :: prt_drzl      ! precip rate, drizzle          m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_rain      ! precip rate, rain             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_crys      ! precip rate, ice cystals      m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_snow      ! precip rate, snow             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_grpl      ! precip rate, graupel          m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_pell      ! precip rate, ice pellets      m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_hail      ! precip rate, hail             m s-1
 real, intent(out), dimension(its:ite), optional      :: prt_sndp      ! precip rate, unmelted snow    m s-1

 real, intent(out), dimension(its:ite,kts:kte,n_qiType), optional :: qi_type ! mass mixing ratio, diagnosed ice type  kg kg-1
 real, intent(inout), dimension(its:ite,kts:kte,nCat),   optional :: zitot   ! ice, reflectivity mixing ratio         kg2 kg-1

 logical, intent(in)                                  :: scpf_on       ! Switch to activate SCPF
 real,    intent(in)                                  :: scpf_pfrac    ! precipitation fraction factor (SCPF)
 real,    intent(in)                                  :: scpf_resfact  ! model resolution factor (SCPF)
 real,    intent(out), dimension(its:ite,kts:kte)     :: SCF_out       ! cloud fraction from SCPF

!----- Local variables and parameters:  -------------------------------------------------!

 real, dimension(its:ite,kts:kte) :: mu_r  ! shape parameter of rain
 real, dimension(its:ite,kts:kte) :: t     ! temperature at the beginning of the microhpysics step [K]
 real, dimension(its:ite,kts:kte) :: t_old ! temperature at the beginning of the model time step [K]

! 2D size distribution and fallspeed parameters:

 real, dimension(its:ite,kts:kte) :: lamc
 real, dimension(its:ite,kts:kte) :: lamr
 real, dimension(its:ite,kts:kte) :: logn0r
 real, dimension(its:ite,kts:kte) :: mu_c
!real, dimension(its:ite,kts:kte) :: diag_effr   (currently not used)
 real, dimension(its:ite,kts:kte) :: nu
 real, dimension(its:ite,kts:kte) :: cdist
 real, dimension(its:ite,kts:kte) :: cdist1
 real, dimension(its:ite,kts:kte) :: cdistr
 real, dimension(its:ite,kts:kte) :: Vt_qc

! liquid-phase microphysical process rates:
!  (all Q process rates in kg kg-1 s-1)
!  (all N process rates in # kg-1)

 real :: qrcon   ! rain condensation
 real :: qcacc   ! cloud droplet accretion by rain
 real :: qcaut   ! cloud droplet autoconversion to rain
 real :: ncacc   ! change in cloud droplet number from accretion by rain
 real :: ncautc  ! change in cloud droplet number from autoconversion
 real :: ncslf   ! change in cloud droplet number from self-collection
 real :: nrslf   ! change in rain number from self-collection
 real :: ncnuc   ! change in cloud droplet number from activation of CCN
 real :: qccon   ! cloud droplet condensation
 real :: qcnuc   ! activation of cloud droplets from CCN
 real :: qrevp   ! rain evaporation
 real :: qcevp   ! cloud droplet evaporation
 real :: nrevp   ! change in rain number from evaporation
 real :: ncautr  ! change in rain number from autoconversion of cloud water

! ice-phase microphysical process rates:
!  (all Q process rates in kg kg-1 s-1)
!  (all N process rates in # kg-1)

 real, dimension(nCat) :: qccol     ! collection of cloud water by ice
 real, dimension(nCat) :: qwgrth    ! wet growth rate
 real, dimension(nCat) :: qidep     ! vapor deposition
 real, dimension(nCat) :: qrcol     ! collection rain mass by ice
 real, dimension(nCat) :: qinuc     ! deposition/condensation freezing nuc
 real, dimension(nCat) :: nccol     ! change in cloud droplet number from collection by ice
 real, dimension(nCat) :: nrcol     ! change in rain number from collection by ice
 real, dimension(nCat) :: ninuc     ! change in ice number from deposition/cond-freezing nucleation
 real, dimension(nCat) :: qisub     ! sublimation of ice
 real, dimension(nCat) :: qimlt     ! melting of ice
 real, dimension(nCat) :: nimlt     ! melting of ice
 real, dimension(nCat) :: nisub     ! change in ice number from sublimation
 real, dimension(nCat) :: nislf     ! change in ice number from collection within a category
 real, dimension(nCat) :: qchetc    ! contact freezing droplets
 real, dimension(nCat) :: qcheti    ! immersion freezing droplets
 real, dimension(nCat) :: qrhetc    ! contact freezing rain
 real, dimension(nCat) :: qrheti    ! immersion freezing rain
 real, dimension(nCat) :: nchetc    ! contact freezing droplets
 real, dimension(nCat) :: ncheti    ! immersion freezing droplets
 real, dimension(nCat) :: nrhetc    ! contact freezing rain
 real, dimension(nCat) :: nrheti    ! immersion freezing rain
 real, dimension(nCat) :: nrshdr    ! source for rain number from collision of rain/ice above freezing and shedding
 real, dimension(nCat) :: qcshd     ! source for rain mass due to cloud water/ice collision above freezing and shedding or wet growth and shedding
 real, dimension(nCat) :: qrmul     ! change in q, ice multiplication from rime-splitnering of rain (not included in the paper)
 real, dimension(nCat) :: nimul     ! change in Ni, ice multiplication from rime-splintering (not included in the paper)
 real, dimension(nCat) :: ncshdc    ! source for rain number due to cloud water/ice collision above freezing  and shedding (combined with NRSHD in the paper)
 real, dimension(nCat) :: rhorime_c ! density of rime (from cloud)

 real, dimension(nCat,nCat) :: nicol ! change of N due to ice-ice collision between categories
 real, dimension(nCat,nCat) :: qicol ! change of q due to ice-ice collision between categories

 logical, dimension(nCat)   :: log_wetgrowth

 real, dimension(nCat) :: Eii_fact,epsi
 real :: eii ! temperature dependent aggregation efficiency

 real, dimension(its:ite,kts:kte,nCat) :: diam_ice,rimefraction,rimevolume

 real, dimension(its:ite,kts:kte)      :: inv_dzq,inv_rho,ze_ice,ze_rain,prec,acn,rho,   &
            rhofacr,rhofaci,xxls,xxlv,xlf,qvs,qvi,sup,supi,vtrmi1,tmparr1,mflux_r,       &
            mflux_i,invexn

 real, dimension(kts:kte) :: V_qr,V_qit,V_nit,V_nr,V_qc,V_nc,V_zit,flux_qit,flux_qx,     &
            flux_nx,flux_nit,flux_qir,flux_bir,flux_zit

 real, dimension(kts:kte) :: SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr
 real                     :: ssat_cld,ssat_clr,ssat_r,supi_cld,sup_cld,sup_r

 real    :: lammax,lammin,mu,dv,sc,dqsdT,ab,kap,epsr,epsc,xx,aaa,epsilon,sigvl,epsi_tot, &
            aact,sm1,sm2,uu1,uu2,dum,dum1,dum2,dumqv,dumqvs,dums,ratio,qsat0,dum3,dum4,  &
            dum5,dum6,rdumii,rdumjj,dqsidT,abi,dumqvi,rhop,v_impact,ri,iTc,D_c,tmp1,     &
            tmp2,inv_dum3,odt,oxx,oabi,fluxdiv_qit,fluxdiv_nit,fluxdiv_qir,fluxdiv_bir,  &
            prt_accum,fluxdiv_qx,fluxdiv_nx,Co_max,dt_sub,fluxdiv_zit,D_new,Q_nuc,N_nuc, &
            deltaD_init,dum1c,dum4c,dum5c,dumt,qcon_satadj,qdep_satadj,sources,sinks,    &
            timeScaleFactor,dt_left,qv_tmp,t_tmp,dum1z

 double precision :: tmpdbl1,tmpdbl2,tmpdbl3

 integer :: dumi,i,k,ii,iice,iice_dest,dumj,dumii,dumjj,dumzz,tmpint1,ktop,kbot,kdir,    &
            dumic,dumiic,dumjjc,catcoll,k_qxbot,k_qxtop,k_temp

 logical :: log_nucleationPossible,log_hydrometeorsPresent,log_predictSsat,              &
            log_exitlevel,log_hmossopOn,log_qxpresent,log_3momentIce

! quantities related to process rates/parameters, interpolated from lookup tables:

 real    :: f1pr01   ! number-weighted fallspeed
 real    :: f1pr02   ! mass-weighted fallspeed
 real    :: f1pr03   ! ice collection within a category
 real    :: f1pr04   ! collection of cloud water by ice
 real    :: f1pr05   ! melting
 real    :: f1pr06   ! effective radius
 real    :: f1pr07   ! collection of rain number by ice
 real    :: f1pr08   ! collection of rain mass by ice
 real    :: f1pr09   ! inverse normalized qsmall (for lambda limiter)
 real    :: f1pr10   ! inverse normalized qlarge (for lambda limiter)
!real    :: f1pr11   ! not used
!real    :: f1pr12   ! not used
 real    :: f1pr13   ! reflectivity
 real    :: f1pr14   ! melting (ventilation term)
 real    :: f1pr15   ! mass-weighted mean diameter
 real    :: f1pr16   ! mass-weighted mean particle density
 real    :: f1pr17   ! ice-ice category collection change in number
 real    :: f1pr18   ! ice-ice category collection change in mass
 real    :: f1pr19   ! reflectivity-weighted fallspeed
!real    :: f1pr20   ! not used
!real    :: f1pr21   ! not used
 real    :: f1pr22   ! LAMBDA_i (PSD parameter of ice, cat 1)
 real    :: f1pr23   ! MU_i     (PSD parameter of ice, cat 1)

! quantities related to diagnostic hydrometeor/precipitation types
 real,    parameter                       :: freq3DtypeDiag     = 1.      !frequency (min) for full-column diagnostics
 real,    parameter                       :: thres_raindrop     = 100.e-6 !size threshold for drizzle vs. rain
 real,    dimension(its:ite,kts:kte)      :: Q_drizzle,Q_rain
 real,    dimension(its:ite,kts:kte,nCat) :: Q_crystals,Q_ursnow,Q_lrsnow,Q_grpl,Q_pellets,Q_hail
 integer                                  :: ktop_typeDiag

! to be added as namelist parameters (future)
 logical, parameter :: debug_ABORT  = .true. !.true. will result in forced abort in s/r 'check_values'
 logical            :: force_abort
 integer            :: location_ind          !return value of location index from sr/ 'check_values'

! added for triple moment ice
 real                  :: mu_i               !shape parameter for ice
 real                  :: mu_i_new           !shape parameter for processes that specify mu_i
 real, dimension(nCat) :: dumm0,dumm3

! add integers for mu_i index
 integer :: imu
 integer, parameter :: niter_mui = 5 ! number of iterations for find mu for lookup table

!-----------------------------------------------------------------------------------!
!  End of variables/parameters declarations
!-----------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------------!
! Note, the array 'diag_3d(ni,nk,n_diag_3d)' provides a placeholder to output 3D diagnostic fields.
! The entire array array is inialized to zero (below).  Code can be added to store desired fields
! by simply adding the appropriate assignment statements.  For example, if one wishs to output the
! rain condensation and evaporation rates, simply add assignments in the appropriate locations.
!  e.g.:
!
!   diag_3d(i,k,1) = qrcon
!   diag_3d(i,k,2) = qrevp
!
! The fields will automatically be passed to the driving model.  In GEM, these arrays can be
! output by adding 'SS01' and 'SS02' to the model output list.
!
! Similarly, 'diag_2d(ni,n_diag_2d) is a placeholder to output 2D diagnostic fields.
!  e.g.:
!
!   diag_2d(i,1) = maxval(qr(i,:))  !column-maximum qr
!-----------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------------!
! The following code blocks can be instered for debugging (all within the main i-loop):
!
!    !-- call to s/r 'check_values' WITHIN k loops:
!     if (debug_on) then
!        tmparr1(i,k) = th(i,k)*(pres(i,k)*1.e-5)**(rd*inv_cp)
!        call check_values(qv(i,k:k),tmparr1(i,k:k),qc(i,k:k),nc(i,k:k),qr(i,k:k),nr(i,k:k),     &
!             qitot(i,k:k,:),qirim(i,k:k,:),nitot(i,k:k,:),birim(i,k:k,:),zitot(i,k:k,:),i,it,debug_ABORT,555)
!        if (global_status /= STATUS_OK) return
!     endif
!    !==
!
!    !-- call to s/r 'check_values' OUTSIDE k loops:
!     if (debug_on) then
!        tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
!        call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:),  &
!                          qirim(i,:,:),nitot(i,:,:),birim(i,:,:),zitot(i,:,:),i,it,debug_ABORT,666)
!        if (global_status /= STATUS_OK) return
!     endif
!    !==
!-----------------------------------------------------------------------------------!

! call cpu_time(t_p3main_start)
 
 tmp1 = uzpl(1,1)    !avoids compiler warning for unused variable 'uzpl'

 ! direction of vertical leveling:
 if (trim(model)=='GEM' .or. trim(model)=='KIN1D') then
    ktop = kts        !k of top level
    kbot = kte        !k of bottom level
    kdir = -1         !(k: 1=top, nk=bottom)
 else
    ktop = kte        !k of top level
    kbot = kts        !k of bottom level
    kdir = 1          !(k: 1=bottom, nk=top)
 endif

 if (trim(model)=='GEM') then
   if (.not. typeDiags_ON) then
      !If typeDiags_ON is .false., uninitialized arrays (prt_drzl, qi_type, etc.) will be passed back.
      !(The coding of this will be refined later)
       print*, '*** ERROR in P3_MAIN ***'
       print*, '* typeDiags_ON must be set to .TRUE. for GEM'
       global_status = STATUS_ERROR
       return
    endif
 endif


! Determine threshold size difference [m] as a function of nCat
! (used for destination category upon ice initiation)
! note -- this code could be moved to 'p3_init'
 select case (nCat)
    case (1)
       deltaD_init = 999.    !not used if n_iceCat=1 (but should be defined)
    case (2)
       deltaD_init = 500.e-6
    case (3)
       deltaD_init = 400.e-6
    case (4)
       deltaD_init = 235.e-6
    case (5)
       deltaD_init = 175.e-6
    case (6:)
       deltaD_init = 150.e-6
 end select

! deltaD_init = 250.e-6   !for testing
! deltaD_init = dummy_in   !temporary; passed in from cld1d

! Note:  Code for prediction of supersaturation is available in current version.
!        In the future 'log_predictSsat' will be a user-defined namelist key.
 log_predictSsat = .false.

 log_3momentIce  = present(zitot)

 log_hmossopOn   = (nCat.gt.1)      !default: off for nCat=1, off for nCat>1
!log_hmossopOn   = .true.           !switch to have Hallet-Mossop ON
!log_hmossopOn   = .false.          !switch to have Hallet-Mossop OFF

 inv_dzq    = 1./dzq  ! inverse of thickness of layers
 odt        = 1./dt   ! inverse model time step

! Compute time scale factor over which to apply soft rain lambda limiter
! note: '1./max(30.,dt)' = '1.*min(1./30., 1./dt)'
 timeScaleFactor = min(1./120., odt)

 prt_liq   = 0.
 prt_sol   = 0.
 mflux_r   = 0.
 mflux_i   = 0.
 prec      = 0.
 mu_r      = 0.
 diag_ze   = -99.
 diam_ice  = 0.
 rimefraction = 0.
 rimevolume = 0.
 ze_ice    = 1.e-22
 ze_rain   = 1.e-22
 diag_effc = 10.e-6 ! default value
!diag_effr = 25.e-6 ! default value
 diag_effi = 25.e-6 ! default value
 diag_vmi  = 0.
 diag_di   = 0.
 diag_rhoi = 0.
 if (present(diag_dhmax)) diag_dhmax = 0.
 if (present(diag_lami))  diag_lami  = 0.
 if (present(diag_mui))   diag_mui   = 0.
 diag_2d   = 0.
 diag_3d   = 0.
 rhorime_c = 400.
!rhorime_r = 400.

 tmparr1 = (pres*1.e-5)**(rd*inv_cp)
 invexn  = 1./tmparr1        !inverse Exner function array
 t       = th    *tmparr1    !compute temperature from theta (value at beginning of microphysics step)
 t_old   = th_old*tmparr1    !compute temperature from theta (value at beginning of model time step)
 qv      = max(qv,0.)        !clip water vapor to prevent negative values passed in (beginning of microphysics)
!==

!-----------------------------------------------------------------------------------!
 i_loop_main: do i = its,ite  ! main i-loop (around the entire scheme)

    if (debug_on) then
       location_ind = 100
       force_abort  =.false.
       if (log_3momentIce) then
          call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
                 Zitot=zitot(i,:,:))
       else
          call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
       endif
       if (global_status /= STATUS_OK) return
    endif

    log_hydrometeorsPresent = .false.
    log_nucleationPossible  = .false.

    k_loop_1: do k = kbot,ktop,kdir

     !calculate some time-varying atmospheric variables
       rho(i,k)     = pres(i,k)/(rd*t(i,k))
       inv_rho(i,k) = 1./rho(i,k)
       xxlv(i,k)    = 3.1484e6-2370.*273.15 !t(i,k), use constant Lv
       xxls(i,k)    = xxlv(i,k)+0.3337e6
       xlf(i,k)     = xxls(i,k)-xxlv(i,k)
     ! max statement added below for first calculation when t_old is zero before t_old is set at end of p3 main
       qvs(i,k)     = qv_sat(max(t_old(i,k),1.),pres(i,k),0)
       qvi(i,k)     = qv_sat(max(t_old(i,k),1.),pres(i,k),1)

      ! if supersaturation is not predicted or during the first time step, then diagnose from qv and T (qvs)
       if (.not.(log_predictSsat).or.it.le.1) then
          ssat(i,k)    = qv_old(i,k)-qvs(i,k)
          sup(i,k)     = qv_old(i,k)/qvs(i,k)-1.
          supi(i,k)    = qv_old(i,k)/qvi(i,k)-1.
      ! if supersaturation is predicted then diagnose sup and supi from ssat
       else if ((log_predictSsat).and.it.gt.1) then
          sup(i,k)     = ssat(i,k)/qvs(i,k)
          supi(i,k)    = (ssat(i,k)+qvs(i,k)-qvi(i,k))/qvi(i,k)
       endif

       rhofacr(i,k) = (rhosur*inv_rho(i,k))**0.54
       rhofaci(i,k) = (rhosui*inv_rho(i,k))**0.54
       dum          = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)  ! this is mu
       acn(i,k)     = g*rhow/(18.*dum)  ! 'a' parameter for droplet fallspeed (Stokes' law)

      !specify cloud droplet number (for 1-moment version)
       if (.not.(log_predictNc)) then
          nc(i,k) = nccnst*inv_rho(i,k)
       endif

! The test below is skipped if SCPF is not used since now, if SCF>0 somewhere, then nucleation is possible.
! If there is the possibility of nucleation or droplet activation (i.e., if RH is relatively high)
! then calculate microphysical processes even if there is no existing condensate
! Note only theta is updated from clipping and not temp, though temp is used for subsequent calculations.
! This change is tiny and therefore neglected.
       if ((t(i,k).lt.273.15 .and. supi(i,k).ge.-0.05) .or.                              &
           (t(i,k).ge.273.15 .and. sup(i,k).ge.-0.05 ) .and. (.not. SCPF_on))            &
           log_nucleationPossible = .true.

    !--- apply mass clipping if dry and mass is sufficiently small
    !    (implying all mass is expected to evaporate/sublimate in one time step)

       if (qc(i,k).lt.qsmall .or. (qc(i,k).lt.1.e-8 .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.    ! updated further down
       endif

       if (qr(i,k).lt.qsmall .or. (qr(i,k).lt.1.e-8 .and. sup(i,k).lt.-0.1)) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.    ! updated further down
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall .or. (qitot(i,k,iice).lt.1.e-8 .and.             &
           supi(i,k).lt.-0.1)) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          else
             log_hydrometeorsPresent = .true.    ! final update
          endif

          if (qitot(i,k,iice).ge.qsmall .and. qitot(i,k,iice).lt.1.e-8 .and.             &
           t(i,k).ge.273.15) then
             qr(i,k) = qr(i,k) + qitot(i,k,iice)
             nr(i,k) = nr(i,k) + nitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xlf(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

       enddo  !iice-loop

    !===

    enddo k_loop_1

   !zero out zitot if there is no qitot for triple moment
    if (log_3momentIce) where (qitot(i,:,:).lt.qsmall) zitot(i,:,:) = 0.

    if (debug_on) then
       location_ind = 200
       force_abort  =.false.
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       if (log_3momentIce) then
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,         &
                 Zitot=zitot(i,:,:))
       else
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
       endif
       if (global_status /= STATUS_OK) return
    endif

   !first call to compute_SCPF
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)

    if ((scpf_ON) .and. (sum(SCF) .ge. 0.01)) log_nucleationPossible = .true.

   !jump to end of i-loop if log_nucleationPossible=.false.  (i.e. skip everything)
    if (.not. (log_nucleationPossible .or. log_hydrometeorsPresent)) goto 333

    log_hydrometeorsPresent = .false.   ! reset value; used again below

!-- for sedimentation-only tests:
!goto 6969
!==

!------------------------------------------------------------------------------------------!
!   main k-loop (for processes):
    k_loop_main: do k = kbot,ktop,kdir


     ! if relatively dry and no hydrometeors at this level, skip to end of k-loop (i.e. skip this level)
       log_exitlevel = .true.
       if (qc(i,k).ge.qsmall .or. qr(i,k).ge.qsmall) log_exitlevel = .false.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) log_exitlevel = .false.
       enddo

       !The test below is skipped if SCPF is used since now, if SCF>0 somewhere, then nucleation is possible
       if ( (     SCPF_on) .and. log_exitlevel .and.       &
          (SCF(k).lt.0.01) )  goto 555 !i.e. skip all process rates !%%% FRED TEST NOT SURE
       if ( (.not.SCPF_on) .and. log_exitlevel .and.       &
          ((t(i,k).lt.273.15 .and. supi(i,k).lt.-0.05) .or.&
           (t(i,k).ge.273.15 .and. sup(i,k) .lt.-0.05))) goto 555   !i.e. skip all process rates

    ! initialize warm-phase process rates
       qcacc   = 0.;     qrevp   = 0.;     qccon   = 0.
       qcaut   = 0.;     qcevp   = 0.;     qrcon   = 0.
       ncacc   = 0.;     ncnuc   = 0.;     ncslf   = 0.
       ncautc  = 0.;     qcnuc   = 0.;     nrslf   = 0.
       nrevp   = 0.;     ncautr  = 0.

    ! initialize ice-phase  process rates
       qchetc  = 0.;     qisub   = 0.;     nrshdr  = 0.
       qcheti  = 0.;     qrcol   = 0.;     qcshd   = 0.
       qrhetc  = 0.;     qimlt   = 0.;     qccol   = 0.
       qrheti  = 0.;     qinuc   = 0.;     nimlt   = 0.
       nchetc  = 0.;     nccol   = 0.;     ncshdc  = 0.
       ncheti  = 0.;     nrcol   = 0.;     nislf   = 0.
       nrhetc  = 0.;     ninuc   = 0.;     qidep   = 0.
       nrheti  = 0.;     nisub   = 0.;     qwgrth  = 0.
       qrmul   = 0.;     nimul   = 0.;     qicol   = 0.
       nicol   = 0.

       log_wetgrowth = .false.

!----------------------------------------------------------------------
       predict_supersaturation: if (log_predictSsat) then

      ! Adjust cloud water and thermodynamics to prognostic supersaturation
      ! following the method in Grabowski and Morrison (2008).
      ! Note that the effects of vertical motion are assumed to dominate the
      ! production term for supersaturation, and the effects are sub-grid
      ! scale mixing and radiation are not explicitly included.

          dqsdT   = xxlv(i,k)*qvs(i,k)/(rv*t(i,k)*t(i,k))
          ab      = 1. + dqsdT*xxlv(i,k)*inv_cp
          epsilon = (qv(i,k)-qvs(i,k)-ssat(i,k))/ab
          epsilon = max(epsilon,-qc(i,k))   ! limit adjustment to available water
        ! don't adjust upward if subsaturated
        ! otherwise this could result in positive adjustment
        ! (spurious generation ofcloud water) in subsaturated conditions
          if (ssat(i,k).lt.0.) epsilon = min(0.,epsilon)

        ! now do the adjustment
          if (abs(epsilon).ge.1.e-15) then
             qc(i,k)   = qc(i,k)+epsilon
             qv(i,k)   = qv(i,k)-epsilon
             th(i,k)   = th(i,k)+epsilon*invexn(i,k)*xxlv(i,k)*inv_cp
            ! recalculate variables if there was adjustment
             t(i,k)    = th(i,k)*(1.e-5*pres(i,k))**(rd*inv_cp)
             qvs(i,k)  = qv_sat(t(i,k),pres(i,k),0)
             qvi(i,k)  = qv_sat(t(i,k),pres(i,k),1)
             sup(i,k)  = qv(i,k)/qvs(i,k)-1.
             supi(i,k) = qv(i,k)/qvi(i,k)-1.
             ssat(i,k) = qv(i,k)-qvs(i,k)
          endif

       endif predict_supersaturation
!----------------------------------------------------------------------

! skip micro process calculations except nucleation/acvtivation if there no hydrometeors are present
       log_exitlevel = .true.
       if (qc(i,k).ge.qsmall .or. qr(i,k).ge.qsmall) log_exitlevel = .false.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) log_exitlevel=.false.
       enddo
       if (log_exitlevel) goto 444   !i.e. skip to nucleation

      !time/space varying physical variables
       mu     = 1.496e-6*t(i,k)**1.5/(t(i,k)+120.)
       dv     = 8.794e-5*t(i,k)**1.81/pres(i,k)
       sc     = mu/(rho(i,k)*dv)
       dum    = 1./(rv*t(i,k)**2)
       dqsdT  = xxlv(i,k)*qvs(i,k)*dum
       dqsidT = xxls(i,k)*qvi(i,k)*dum
       ab     = 1.+dqsdT*xxlv(i,k)*inv_cp
       abi    = 1.+dqsidT*xxls(i,k)*inv_cp
       kap    = 1.414e+3*mu
      !very simple temperature dependent aggregation efficiency
!       if (t(i,k).lt.253.15) then
!          eii = 0.1
!       else if (t(i,k).ge.253.15.and.t(i,k).lt.268.15) then
!          eii = 0.1+(t(i,k)-253.15)*0.06     ! linear ramp from 0.1 to 1 between 253.15 and 268.15 K  [note: 0.06 = (1./15.)*0.9]
!       else if (t(i,k).ge.268.15) then
!          eii = 1.
!       endif
       if (t(i,k).lt.253.15) then
          eii = 0.001
       else if (t(i,k).ge.253.15.and.t(i,k).lt.273.15) then
          eii = 0.001+(t(i,k)-253.15)*(0.3-0.001)/20.
       else if (t(i,k).ge.273.15) then
          eii = 0.3
       endif

       call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k),     &
                           lammin,lammax,cdist(i,k),cdist1(i,k),iSCF(k))


       call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),cdistr(i,k),logn0r(i,k),   &
                          iSPF(k))

     ! initialize inverse supersaturation relaxation timescale for combined ice categories
       epsi_tot = 0.

       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))

       iice_loop1: do iice = 1,nCat

          qitot_notsmall_1: if (qitot(i,k,iice).ge.qsmall) then

            !impose lower limits to prevent taking log of # < 0
             nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
             nr(i,k)         = max(nr(i,k),nsmall)

            !compute mean-mass ice diameters (estimated; rigorous approach to be implemented later)
             dum2 = 500. !ice density
             diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(nitot(i,k,iice)*dum2*pi))**thrd

             call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

             call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,      &
                    rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),rhop)

             call find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr(i,k),nr(i,k))

             if (.not. log_3momentIce) then

             ! call to lookup table interpolation subroutines to get process rates
                call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
                call access_lookup_table(dumjj,dumii,dumi, 3,dum1,dum4,dum5,f1pr03)
                call access_lookup_table(dumjj,dumii,dumi, 4,dum1,dum4,dum5,f1pr04)
                call access_lookup_table(dumjj,dumii,dumi, 5,dum1,dum4,dum5,f1pr05)
                call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
                call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
                call access_lookup_table(dumjj,dumii,dumi,10,dum1,dum4,dum5,f1pr14)

          ! ice-rain collection processes
                if (qr(i,k).ge.qsmall) then
                   call access_lookup_table_coll(dumjj,dumii,dumj,dumi,1,dum1,dum3,dum4,dum5,f1pr07)
                   call access_lookup_table_coll(dumjj,dumii,dumj,dumi,2,dum1,dum3,dum4,dum5,f1pr08)
                else
                   f1pr07 = -99. ! log space
                   f1pr08 = -99. ! log space
                endif

             else ! 3-moment ice

             ! get G indices

             !impose lower limits to prevent taking log of # < 0
                zitot(i,k,iice) = max(zitot(i,k,iice),zsmall)

                dum1z =  6./(200.*pi)*qitot(i,k,iice)  !estimate of moment3, as starting point use 200 kg m-3 estimate of bulk density

                do imu=1,niter_mui
                   mu_i = compute_mu_3moment(nitot(i,k,iice),dum1z,zitot(i,k,iice),mu_i_max)
                   call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),mu_i)
                   call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) ! find actual bulk density
                   dum1z =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                enddo

             ! call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),zitot(i,k,iice)) !HM moved to above

             ! call to lookup table interpolation subroutines to get process rates
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 2,dum1,dum4,dum5,dum6,f1pr02)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 3,dum1,dum4,dum5,dum6,f1pr03)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 4,dum1,dum4,dum5,dum6,f1pr04)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 5,dum1,dum4,dum5,dum6,f1pr05)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 7,dum1,dum4,dum5,dum6,f1pr09)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 8,dum1,dum4,dum5,dum6,f1pr10)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,10,dum1,dum4,dum5,dum6,f1pr14)
              ! call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) !HM moved to above

          ! ice-rain collection processes
                if (qr(i,k).ge.qsmall) then
                   call access_lookup_table_coll_3mom(dumzz,dumjj,dumii,dumj,dumi,1,dum1,dum3,dum4,dum5,dum6,f1pr07)
                   call access_lookup_table_coll_3mom(dumzz,dumjj,dumii,dumj,dumi,2,dum1,dum3,dum4,dum5,dum6,f1pr08)
                else
                   f1pr07 = -99. ! log space
                   f1pr08 = -99. ! log space
                endif

             endif  !if log_3momentIce

          ! adjust Ni if needed to make sure mean size is in bounds (i.e. apply lambda limiters)
          !  note: the inv_Qmin (f1pr09) and inv_Qmax (f1pr10) are normalized, thus the
          !  max[min] values of nitot are obtained from multiplying these values by qitot.
             nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
             nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))

          ! adjust Zitot to make sure mu is in bounds
          ! note that the Zmax and Zmin are normalized and thus need to be multiplied by existing Q
             if (log_3momentIce) then
                dum1 =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                tmp1 = G_of_mu(0.)
                tmp2 = G_of_mu(20.)
                zitot(i,k,iice) = min(zitot(i,k,iice),tmp1*dum1**2/nitot(i,k,iice))
                zitot(i,k,iice) = max(zitot(i,k,iice),tmp2*dum1**2/nitot(i,k,iice))
             endif

          ! Determine additional collection efficiency factor to be applied to ice-ice collection.
          ! The computed values of qicol and nicol are multipiled by Eii_fact to gradually shut off collection
          ! if the ice in iice is highly rimed.
             if (qirim(i,k,iice)>0.) then
                tmp1 = qirim(i,k,iice)/qitot(i,k,iice)   !rime mass fraction
                if (tmp1.lt.0.6) then
                   Eii_fact(iice)=1.
                else if (tmp1.ge.0.6.and.tmp1.lt.0.9) then
                ! linear ramp from 1 to 0 for Fr between 0.6 and 0.9
                   Eii_fact(iice) = 1.-(tmp1-0.6)/0.3
                else if (tmp1.ge.0.9) then
                   Eii_fact(iice) = 0.
                endif
             else
                Eii_fact(iice) = 1.
             endif

          endif qitot_notsmall_1 ! qitot > qsmall

!----------------------------------------------------------------------
! Begin calculations of microphysical processes

!......................................................................
! ice processes
!......................................................................

!.......................
! collection of droplets

! here we multiply rates by air density, air density fallspeed correction
! factor, and collection efficiency since these parameters are not
! included in lookup table calculations
! for T < 273.15, assume collected cloud water is instantly frozen
! note 'f1pr' values are normalized, so we need to multiply by N

          if (qitot(i,k,iice).ge.qsmall .and. qc(i,k).ge.qsmall .and. t(i,k).le.273.15) then
             qccol(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
             nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
          endif

          ! for T > 273.15, assume cloud water is collected and shed as rain drops

          if (qitot(i,k,iice).ge.qsmall .and. qc(i,k).ge.qsmall .and. t(i,k).gt.273.15) then
          ! sink for cloud water mass and number, note qcshed is source for rain mass
             qcshd(iice) = rhofaci(i,k)*f1pr04*qc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
             nccol(iice) = rhofaci(i,k)*f1pr04*nc(i,k)*eci*rho(i,k)*nitot(i,k,iice)*iSCF(k)
          ! source for rain number, assume 1 mm drops are shed
             ncshdc(iice) = qcshd(iice)*1.923e+6
          endif

!....................
! collection of rain

     ! here we multiply rates by air density, air density fallspeed correction
     ! factor, collection efficiency, and n0r since these parameters are not
     ! included in lookup table calculations

     ! for T < 273.15, assume all collected rain mass freezes
     ! note this is a sink for rain mass and number and a source
     ! for ice mass

     ! note 'f1pr' values are normalized, so we need to multiply by N

          if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and. t(i,k).le.273.15) then
           ! qrcol(iice)=f1pr08*logn0r(i,k)*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)
           ! nrcol(iice)=f1pr07*logn0r(i,k)*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)
           ! note: f1pr08 and logn0r are already calculated as log_10
             qrcol(iice) = 10.**(f1pr08+logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
             nrcol(iice) = 10.**(f1pr07+logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
          endif

     ! for T > 273.15, assume collected rain number is shed as
     ! 1 mm drops
     ! note that melting of ice number is scaled to the loss
     ! rate of ice mass due to melting
     ! collection of rain above freezing does not impact total rain mass

          if (qitot(i,k,iice).ge.qsmall .and. qr(i,k).ge.qsmall .and. t(i,k).gt.273.15) then
           ! rain number sink due to collection
             nrcol(iice)  = 10.**(f1pr07 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
           ! rain number source due to shedding = collected rain mass/mass of 1 mm drop
             dum    = 10.**(f1pr08 + logn0r(i,k))*rho(i,k)*rhofaci(i,k)*eri*nitot(i,k,iice)*iSCF(k)*(SPF(k)-SPF_clr(k))
     ! for now neglect shedding of ice collecting rain above freezing, since snow is
     ! not expected to shed in these conditions (though more hevaily rimed ice would be
     ! expected to lead to shedding)
     !             nrshdr(iice) = dum*1.923e+6   ! 1./5.2e-7, 5.2e-7 is the mass of a 1 mm raindrop
          endif

!...................................
! collection between ice categories

!        iceice_interaction1:  if (.false.) then       !for testing (to suppress ice-ice interaction)
         iceice_interaction1:  if (iice.ge.2) then

         !note:  In this version, lookupTable_2 (LT2, for ice category interactions) is computed for a maximum
         !       mean ice size of Dm_max=2000.e-6 m (the old lambda_i limiter); thus it is compatible with
         !       use of LT1-v5.2_2momI (with Dm_max=2000.e-6) [i.e. for log_3momentIce=.false.] but not with
         !       LT1-v5.3_3momI (with Dm_max=400000.e-6).  This means that this version can still be
         !       run with the 3momI + nCat>1 configuration, but the ice-ice interactions between different
         !       categories (in this 'iceice_interaction1' block) is suppressed.
         !       In a forthcoming version, both LT1-2momI and LT2 (and LT1-3momI) will all be computed
         !       using the unconstrained size limited (i.e. Dm_max=400000.e-6).

             qitot_notsmall: if (qitot(i,k,iice).ge.qsmall) then
                catcoll_loop: do catcoll = 1,iice-1
                   qitotcatcoll_notsmall: if (qitot(i,k,catcoll).ge.qsmall) then

                  ! first, calculate collection of catcoll category by iice category

                      call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,        &
                                 dumjjc,dum1,dum4,dum5,dum1c,dum4c,dum5c,                   &
                                 iisize,rimsize,densize,                                    &
                                 qitot(i,k,iice),qitot(i,k,catcoll),nitot(i,k,iice),        &
                                 nitot(i,k,catcoll),qirim(i,k,iice),qirim(i,k,catcoll),     &
                                 birim(i,k,iice),birim(i,k,catcoll))

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,       &
                                 dumi,1,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr17)
                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,       &
                                 dumi,2,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr18)

                    ! note: need to multiply by air density, air density fallspeed correction factor,
                    !       and N of the collectee and collector categories for process rates nicol and qicol,
                    !       first index is the collectee, second is the collector
                      nicol(catcoll,iice) = f1pr17*rhofaci(i,k)*rho(i,k)*                   &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(k)
                      qicol(catcoll,iice) = f1pr18*rhofaci(i,k)*rho(i,k)*                   &
                                            nitot(i,k,catcoll)*nitot(i,k,iice)*iSCF(k)

                      nicol(catcoll,iice) = eii*Eii_fact(iice)*nicol(catcoll,iice)
                      qicol(catcoll,iice) = eii*Eii_fact(iice)*qicol(catcoll,iice)
                      nicol(catcoll,iice) = min(nicol(catcoll,iice), nitot(i,k,catcoll)*odt)
                      qicol(catcoll,iice) = min(qicol(catcoll,iice), qitot(i,k,catcoll)*odt)

                  ! second, calculate collection of iice category by catcoll category

                    !needed to force consistency between qirim(catcoll) and birim(catcoll) (not for rhop)
                      call calc_bulkRhoRime(qitot(i,k,catcoll),qirim(i,k,catcoll),birim(i,k,catcoll),rhop)

                      call find_lookupTable_indices_2(dumi,dumii,dumjj,dumic,dumiic,       &
                                 dumjjc,dum1,dum4,dum5,dum1c,dum4c,dum5c,                  &
                                 iisize,rimsize,densize,                                   &
                                 qitot(i,k,catcoll),qitot(i,k,iice),nitot(i,k,catcoll),    &
                                 nitot(i,k,iice),qirim(i,k,catcoll),qirim(i,k,iice),       &
                                 birim(i,k,catcoll),birim(i,k,iice))

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,      &
                                 dumi,1,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr17)

                      call access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,      &
                                 dumi,2,dum1c,dum4c,dum5c,dum1,dum4,dum5,f1pr18)

                      nicol(iice,catcoll) = f1pr17*rhofaci(i,k)*rho(i,k)*                  &
                                            nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(k)
                      qicol(iice,catcoll) = f1pr18*rhofaci(i,k)*rho(i,k)*                  &
                                            nitot(i,k,iice)*nitot(i,k,catcoll)*iSCF(k)

                     ! note: Eii_fact applied to the collector category
                      nicol(iice,catcoll) = eii*Eii_fact(catcoll)*nicol(iice,catcoll)
                      qicol(iice,catcoll) = eii*Eii_fact(catcoll)*qicol(iice,catcoll)
                      nicol(iice,catcoll) = min(nicol(iice,catcoll),nitot(i,k,iice)*odt)
                      qicol(iice,catcoll) = min(qicol(iice,catcoll),qitot(i,k,iice)*odt)

                   endif qitotcatcoll_notsmall
                enddo catcoll_loop
             endif qitot_notsmall

          endif iceice_interaction1


!.............................................
! self-collection of ice (in a given category)

    ! here we multiply rates by collection efficiency, air density,
    ! and air density correction factor since these are not included
    ! in the lookup table calculations
    ! note 'f1pr' values are normalized, so we need to multiply by N

          if (qitot(i,k,iice).ge.qsmall) then
             nislf(iice) = f1pr03*rho(i,k)*eii*Eii_fact(iice)*rhofaci(i,k)*nitot(i,k,iice)*nitot(i,k,iice)*iSCF(k)
          endif


!............................................................
! melting

    ! need to add back accelerated melting due to collection of ice mass by rain (pracsw1)
    ! note 'f1pr' values are normalized, so we need to multiply by N

          if (qitot(i,k,iice).ge.qsmall .and. t(i,k).gt.273.15) then
             qsat0 = 0.622*e0/(pres(i,k)-e0)
          !  dum=cpw/xlf(i,k)*(t(i,k)-273.15)*(pracsw1+qcshd(iice))
          ! currently enhanced melting from collision is neglected
          ! dum=cpw/xlf(i,k)*(t(i,k)-273.15)*(pracsw1)
             dum = 0.
          ! qimlt(iice)=(f1pr05+f1pr14*sc**0.3333*(rhofaci(i,k)*rho(i,k)/mu)**0.5)* &
          !       (t(i,k)-273.15)*2.*pi*kap/xlf(i,k)+dum
          ! include RH dependence
             qimlt(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*((t(i,k)-   &
                          273.15)*kap-rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(k)))*2.*pi/xlf(i,k)+   &
                          dum)*nitot(i,k,iice)
             qimlt(iice) = max(qimlt(iice),0.)
             nimlt(iice) = qimlt(iice)*(nitot(i,k,iice)/qitot(i,k,iice))
          endif

!............................................................
! calculate wet growth

    ! similar to Musil (1970), JAS
    ! note 'f1pr' values are normalized, so we need to multiply by N

          if (qitot(i,k,iice).ge.qsmall .and. (qc(i,k)+qr(i,k)).ge.1.e-6 .and. t(i,k).lt.273.15) then

             qsat0  = 0.622*e0/(pres(i,k)-e0)
             qwgrth(iice) = ((f1pr05 + f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*       &
                       2.*pi*(rho(i,k)*xxlv(i,k)*dv*(qsat0-Qv_cld(k))-(t(i,k)-273.15)*         &
                       kap)/(xlf(i,k)+cpw*(t(i,k)-273.15)))*nitot(i,k,iice)
             qwgrth(iice) = max(qwgrth(iice),0.)
         !calculate shedding for wet growth
             dum    = max(0.,(qccol(iice)+qrcol(iice))-qwgrth(iice))
             if (dum.ge.1.e-10) then
                nrshdr(iice) = nrshdr(iice) + dum*1.923e+6   ! 1/5.2e-7, 5.2e-7 is the mass of a 1 mm raindrop
                if ((qccol(iice)+qrcol(iice)).ge.1.e-10) then
                   dum1  = 1./(qccol(iice)+qrcol(iice))
                   qcshd(iice) = qcshd(iice) + dum*qccol(iice)*dum1
                   qccol(iice) = qccol(iice) - dum*qccol(iice)*dum1
                   qrcol(iice) = qrcol(iice) - dum*qrcol(iice)*dum1
               endif
             ! densify due to wet growth
               log_wetgrowth(iice) = .true.
             endif

          endif


!-----------------------------
! calcualte total inverse ice relaxation timescale combined for all ice categories
! note 'f1pr' values are normalized, so we need to multiply by N
          if (qitot(i,k,iice).ge.qsmall .and. t(i,k).lt.273.15) then
             epsi(iice) = ((f1pr05+f1pr14*sc**thrd*(rhofaci(i,k)*rho(i,k)/mu)**0.5)*2.*pi* &
                          rho(i,k)*dv)*nitot(i,k,iice)
             epsi_tot   = epsi_tot + epsi(iice)
          else
             epsi(iice) = 0.
          endif


!.........................
! calculate rime density

!     FUTURE:  Add source term for birim (=qccol/rhorime_c) so that all process rates calculations
!              are done together, before conservation.

     ! NOTE: Tc (ambient) is assumed for the surface temperature.  Technically,
     ! we should diagose graupel surface temperature from heat balance equation.
     ! (but the ambient temperature is a reasonable approximation; tests show
     ! very little sensitivity to different assumed values, Milbrandt and Morrison 2012).

      ! Compute rime density: (based on parameterization of Cober and List, 1993 [JAS])
      ! for simplicty use mass-weighted ice and droplet/rain fallspeeds

        ! if (qitot(i,k,iice).ge.qsmall .and. t(i,k).lt.273.15) then
        !  NOTE:  condition applicable for cloud only; modify when rain is added back
          if (qccol(iice).ge.qsmall .and. t(i,k).lt.273.15) then

           ! get mass-weighted mean ice fallspeed
             vtrmi1(i,k) = f1pr02*rhofaci(i,k)
             iTc   = 1./min(-0.001,t(i,k)-273.15)

          ! cloud:
             if (qc(i,k).ge.qsmall) then
              ! droplet fall speed
              ! (use Stokes' formulation (thus use analytic solution)
                Vt_qc(i,k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))/(lamc(i,k)**bcn*gamma(mu_c(i,k)+4.))
              ! use mass-weighted mean size
                D_c = (mu_c(i,k)+4.)/lamc(i,k)
                V_impact  = abs(vtrmi1(i,k)-Vt_qc(i,k))
                Ri        = -(0.5e+6*D_c)*V_impact*iTc
!               Ri        = max(1.,min(Ri,8.))
                Ri        = max(1.,min(Ri,12.))
                if (Ri.le.8.) then
                   rhorime_c(iice)  = (0.051 + 0.114*Ri - 0.0055*Ri**2)*1000.
                else
                ! for Ri > 8 assume a linear fit between 8 and 12,
                ! rhorime = 900 kg m-3 at Ri = 12
                ! this is somewhat ad-hoc but allows a smoother transition
                ! in rime density up to wet growth
                   rhorime_c(iice)  = 611.+72.25*(Ri-8.)
                endif

             endif    !if qc>qsmall

          ! rain:
            ! assume rime density for rain collecting ice is 900 kg/m3
!            if (qr(i,k).ge.qsmall) then
!               D_r = (mu_r(i,k)+1.)/lamr(i,k)
!               V_impact  = abs(vtrmi1(i,k)-Vt_qr(i,k))
!               Ri        = -(0.5e+6*D_r)*V_impact*iTc
!               Ri        = max(1.,min(Ri,8.))
!               rhorime_r(iice)  = (0.051 + 0.114*Ri - 0.0055*Ri*Ri)*1000.
!            else
!               rhorime_r(iice) = 400.
!            endif

          else
             rhorime_c(iice) = 400.
!            rhorime_r(iice) = 400.
          endif ! qi > qsmall and T < 273.15

    !--------------------
       enddo iice_loop1
    !--------------------

!............................................................
! contact and immersion freezing droplets

! contact freezing currently turned off
!         dum=7.37*t(i,k)/(288.*10.*pres(i,k))/100.
!         dap=4.*pi*1.38e-23*t(i,k)*(1.+dum/rin)/ &
!                (6.*pi*rin*mu)
!         nacnt=exp(-2.80+0.262*(273.15-t(i,k)))*1000.

       if (qc(i,k).ge.qsmall .and. t(i,k).le.269.15) then
!         qchetc(iice) = pi*pi/3.*Dap*Nacnt*rhow*cdist1(i,k)*gamma(mu_c(i,k)+5.)/lamc(i,k)**4
!         nchetc(iice) = 2.*pi*Dap*Nacnt*cdist1(i,k)*gamma(mu_c(i,k)+2.)/lamc(i,k)
! for future: calculate gamma(mu_c+4) in one place since its used multiple times
          dum   = (1./lamc(i,k))**3
!         qcheti(iice_dest) = cons6*cdist1(i,k)*gamma(7.+pgam(i,k))*exp(aimm*(273.15-t(i,k)))*dum**2
!         ncheti(iice_dest) = cons5*cdist1(i,k)*gamma(pgam(i,k)+4.)*exp(aimm*(273.15-t(i,k)))*dum

!           Q_nuc = cons6*cdist1(i,k)*gamma(7.+mu_c(i,k))*exp(aimm*(273.15-t(i,k)))*dum**2
!           N_nuc = cons5*cdist1(i,k)*gamma(mu_c(i,k)+4.)*exp(aimm*(273.15-t(i,k)))*dum
          tmp1 = cdist1(i,k)*exp(aimm*(273.15-t(i,k)))
          Q_nuc = cons6*gamma(7.+mu_c(i,k))*tmp1*dum**2
          N_nuc = cons5*gamma(mu_c(i,k)+4.)*tmp1*dum
!           tmpdbl1  = dexp(dble(aimm*(273.15-t(i,k))))
!           tmpdbl2  = dble(dum)
!           Q_nuc = cons6*cdist1(i,k)*gamma(7.+mu_c(i,k))*tmpdbl1*tmpdbl2**2
!           N_nuc = cons5*cdist1(i,k)*gamma(mu_c(i,k)+4.)*tmpdbl1*tmpdbl2


          if (nCat>1) then
            !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)

             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif
          qcheti(iice_dest) = Q_nuc
          ncheti(iice_dest) = N_nuc
       endif

!............................................................
! immersion freezing of rain
! for future: get rid of log statements below for rain freezing

       if (qr(i,k)*iSPF(k).ge.qsmall.and.t(i,k).le.269.15) then

!         Q_nuc = cons6*exp(log(cdistr(i,k))+log(gamma(7.+mu_r(i,k)))-6.*log(lamr(i,k)))*exp(aimm*(273.15-t(i,k)))*SPF(k)
!         N_nuc = cons5*exp(log(cdistr(i,k))+log(gamma(mu_r(i,k)+4.))-3.*log(lamr(i,k)))*exp(aimm*(273.15-t(i,k)))*SPF(k)
          tmpdbl1 = dexp(dble(log(cdistr(i,k))+log(gamma(7.+mu_r(i,k)))-6.*log(lamr(i,k))))
          tmpdbl2 = dexp(dble(log(cdistr(i,k))+log(gamma(mu_r(i,k)+4.))-3.*log(lamr(i,k))))
          tmpdbl3 = dexp(dble(aimm*(273.15-t(i,k))))
          Q_nuc = cons6*sngl(tmpdbl1*tmpdbl3)*SPF(k)
          N_nuc = cons5*sngl(tmpdbl2*tmpdbl3)*SPF(k)

          if (nCat>1) then
             !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,          &
                               deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
           else
              iice_dest = 1
           endif
           qrheti(iice_dest) = Q_nuc
           nrheti(iice_dest) = N_nuc
       endif


!......................................
! rime splintering (Hallet-Mossop 1974)

       rimesplintering_on:  if (log_hmossopOn) then

          if (nCat>1) then
             !determine destination ice-phase category
             D_new = 10.e-6 !assumes ice crystals from rime splintering are tiny
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          iice_loop_HM:  do iice = 1,nCat

             ! rime splintering occurs from accretion by large ice, assume a threshold
             ! mean mass size of 4 mm (ad-hoc, could be modified)
             if (qitot(i,k,iice).ge.qsmall.and.diam_ice(i,k,iice).ge.4000.e-6            &
                 .and. (qccol(iice).gt.0. .or. qrcol(iice).gt.0.)) then

                if (t(i,k).gt.270.15) then
                   dum = 0.
                elseif (t(i,k).le.270.15 .and. t(i,k).gt.268.15) then
                   dum = (270.15-t(i,k))*0.5
                elseif (t(i,k).le.268.15 .and. t(i,k).ge.265.15) then
                   dum = (t(i,k)-265.15)*thrd
                elseif (t(i,k).lt.265.15) then
                   dum = 0.
                endif

                !rime splintering from riming of cloud droplets
!                dum1 = 35.e+4*qccol(iice)*dum*1000. ! 1000 is to convert kg to g
!                dum2 = dum1*piov6*900.*(10.e-6)**3  ! assume 10 micron splinters
!                qccol(iice) = qccol(iice)-dum2 ! subtract splintering from rime mass transfer
!                if (qccol(iice) .lt. 0.) then
!                   dum2 = qccol(iice)
!                   qccol(iice) = 0.
!                endif
!                qcmul(iice_dest) = qcmul(iice_dest)+dum2
!                nimul(iice_dest) = nimul(iice_dest)+dum2/(piov6*900.*(10.e-6)**3)

               !rime splintering from riming of large drops (> 25 microns diameter)
               !for simplicitly it is assumed that all accreted rain contributes to splintering,
               !but accreted cloud water does not - hence why the code is commented out above
                dum1 = 35.e+4*qrcol(iice)*dum*1000. ! 1000 is to convert kg to g
                dum2 = dum1*piov6*900.*(10.e-6)**3  ! assume 10 micron splinters
                qrcol(iice) = qrcol(iice)-dum2      ! subtract splintering from rime mass transfer
                if (qrcol(iice) .lt. 0.) then
                   dum2 = qrcol(iice)+dum2
                   qrcol(iice) = 0.
                endif

                qrmul(iice_dest) = qrmul(iice_dest) + dum2
                nimul(iice_dest) = nimul(iice_dest) + dum2/(piov6*900.*(10.e-6)**3)

             endif

          enddo iice_loop_HM

       endif rimesplintering_on


!....................................................
! condensation/evaporation and deposition/sublimation
!   (use semi-analytic formulation)

       !calculate rain evaporation including ventilation
       if (qr(i,k)*iSPF(k).ge.qsmall) then
          call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3,mu_r(i,k),lamr(i,k))
         !interpolate value at mu_r
          dum1 = revap_table(dumii,dumjj)+(rdumii-real(dumii))*                          &
                 (revap_table(dumii+1,dumjj)-revap_table(dumii,dumjj))
         !interoplate value at mu_r+1
          dum2 = revap_table(dumii,dumjj+1)+(rdumii-real(dumii))*                        &
                 (revap_table(dumii+1,dumjj+1)-revap_table(dumii,dumjj+1))
         !final interpolation
          dum  = dum1+(rdumjj-real(dumjj))*(dum2-dum1)

          epsr = 2.*pi*cdistr(i,k)*rho(i,k)*dv*(f1r*gamma(mu_r(i,k)+2.)/(lamr(i,k))      &
                  +f2r*(rho(i,k)/mu)**0.5*sc**thrd*dum)
       else
          epsr = 0.
       endif

       if (qc(i,k).ge.qsmall) then
          epsc = 2.*pi*rho(i,k)*dv*cdist(i,k)
       else
          epsc = 0.
       endif

       if (t(i,k).lt.273.15) then
          oabi = 1./abi
          xx = epsc + epsr + epsi_tot*(1.+xxls(i,k)*inv_cp*dqsdT)*oabi
       else
          xx = epsc + epsr
       endif

       dumqvi = qvi(i,k)   !no modification due to latent heating
!----
! !      ! modify due to latent heating from riming rate
! !      !   - currently this is done by simple linear interpolation
! !      !     between conditions for dry and wet growth --> in wet growth it is assumed
! !      !     that particle surface temperature is at 0 C and saturation vapor pressure
! !      !     is that with respect to liquid. This simple treatment could be improved in the future.
! !        if (qwgrth(iice).ge.1.e-20) then
! !           dum = (qccol(iice)+qrcol(iice))/qwgrth(iice)
! !        else
! !           dum = 0.
! !        endif
! !        dumqvi = qvi(i,k) + dum*(qvs(i,k)-qvi(i,k))
! !        dumqvi = min(qvs(i,k),dumqvi)
!====


     ! 'A' term including ice (Bergeron process)
     ! note: qv and T tendencies due to mixing and radiation are
     ! currently neglected --> assumed to be much smaller than cooling
     ! due to vertical motion which IS included

     ! The equivalent vertical velocity is set to be consistent with dT/dt
     ! since -g/cp*dum = dT/dt therefore dum = -cp/g*dT/dt
     ! note this formulation for dT/dt is not exact since pressure
     ! may change and t and t_old were both diagnosed using the current pressure
     ! errors from this assumption are small
       dum = -cp/g*(t(i,k)-t_old(i,k))*odt

!       dum = qvs(i,k)*rho(i,k)*g*uzpl(i,k)/max(1.e-3,(pres(i,k)-polysvp1(t(i,k),0)))

       if (t(i,k).lt.273.15) then
          aaa = (qv(i,k)-qv_old(i,k))*odt - dqsdT*(-dum*g*inv_cp)-(qvs(i,k)-dumqvi)*     &
                (1.+xxls(i,k)*inv_cp*dqsdT)*oabi*epsi_tot
       else
          aaa = (qv(i,k)-qv_old(i,k))*odt - dqsdT*(-dum*g*inv_cp)
       endif

       xx  = max(1.e-20,xx)   ! set lower bound on xx to prevent division by zero
       oxx = 1./xx

       if (.not. scpf_ON)  then
          ssat_cld = ssat(i,k)
          ssat_r   = ssat(i,k)
          sup_cld  = sup(i,k)
          sup_r    = sup(i,k)
          supi_cld = supi(i,k)
       else
          ssat_cld  = Qv_cld(k) - qvs(i,k) !in-cloud  sub/sur-saturation w.r.t. liq
          ssat_clr  = Qv_clr(k) - qvs(i,k) !clear-sky sub/sur-saturation w.r.t. liq
          !mix of in-cloud/clearsky sub/sur-saturation w.r.t. liqfor rain:
          ssat_r    = ssat_cld*(SPF(k)-SPF_clr(k))+ssat_clr*SPF_clr(k)
          sup_r     = ssat_r   /qvs(i,k)
          sup_cld   = ssat_cld /qvs(i,k)   !in-cloud  sub/sur-saturation w.r.t. liq in %
          supi_cld  = Qv_cld(k)/qvi(i,k)-1.!in-cloud  sub/sur-saturation w.r.t. ice in %
       endif

       if (qc(i,k).ge.qsmall) &
          qccon = (aaa*epsc*oxx+(ssat_cld*SCF(k)-aaa*oxx)*odt*epsc*oxx*(1.-sngl(dexp(-dble(xx*dt)))))/ab
       if (qr(i,k).ge.qsmall) &
          qrcon = (aaa*epsr*oxx+(ssat_r*SPF(k)-aaa*oxx)*odt*epsr*oxx*(1.-sngl(dexp(-dble(xx*dt)))))/ab

      !evaporate instantly for very small water contents
       if (sup_cld.lt.-0.001 .and. qc(i,k).lt.1.e-12)  qccon = -qc(i,k)*odt
       if (sup_r  .lt.-0.001 .and. qr(i,k).lt.1.e-12)  qrcon = -qr(i,k)*odt

       if (qccon.lt.0.) then
          qcevp = -qccon
          qccon = 0.
       else
          qccon = min(qccon, qv(i,k)*odt)
       endif

       if (qrcon.lt.0.) then
          qrevp = -qrcon
          nrevp = qrevp*(nr(i,k)/qr(i,k))
         !nrevp = nrevp*exp(-0.2*mu_r(i,k))  !add mu dependence [Seifert (2008), neglecting size dependence]
          qrcon = 0.
       else
          qrcon = min(qrcon, qv(i,k)*odt)
       endif

       iice_loop_depsub:  do iice = 1,nCat

          if (qitot(i,k,iice).ge.qsmall.and.t(i,k).lt.273.15) then
            !note: diffusional growth/decay rate: (stored as 'qidep' temporarily; may go to qisub below)
             qidep(iice) = (aaa*epsi(iice)*oxx+(ssat_cld*SCF(k)-aaa*oxx)*odt*epsi(iice)*oxx*   &
                           (1.-sngl(dexp(-dble(xx*dt)))))*oabi+(qvs(i,k)-dumqvi)*epsi(iice)*oabi
          endif

         !for very small ice contents in dry air, sublimate all ice instantly
          if (supi_cld.lt.-0.001 .and. qitot(i,k,iice).lt.1.e-12) &
             qidep(iice) = -qitot(i,k,iice)*odt

          !note: 'clbfact_dep' and 'clbfact_sub' calibration factors for ice deposition and sublimation
          !   These are adjustable ad hoc factors used to increase or decrease deposition and/or
          !   sublimation rates.  The representation of the ice capacitances are highly simplified
          !   and the appropriate values in the diffusional growth equation are uncertain.

          if (qidep(iice).lt.0.) then
           !note: limit to saturation adjustment (for dep and subl) is applied later
             qisub(iice) = -qidep(iice)
             qisub(iice) = qisub(iice)*clbfact_sub
             qisub(iice) = min(qisub(iice), qitot(i,k,iice)*odt)
             nisub(iice) = qisub(iice)*(nitot(i,k,iice)/qitot(i,k,iice))
             qidep(iice) = 0.
          else
             qidep(iice) = qidep(iice)*clbfact_dep
             qidep(iice) = min(qidep(iice), qv(i,k)*odt)
          endif

       enddo iice_loop_depsub

444    continue


!................................................................
! deposition/condensation-freezing nucleation
!   (allow ice nucleation if T < -15 C and > 5% ice supersaturation)

       if (.not. scpf_ON)  then
          sup_cld  = sup(i,k)
          supi_cld = supi(i,k)
       else
          supi_cld= Qv_cld(k)/qvi(i,k)-1.!in-cloud  sub/sur-saturation w.r.t. ice in %
          sup_cld = Qv_cld(k)/qvs(i,k)-1.!in-cloud  sub/sur-saturation w.r.t. liq in %
       endif

       if (t(i,k).lt.258.15 .and. supi_cld.ge.0.05) then
!         dum = exp(-0.639+0.1296*100.*supi(i,k))*1000.*inv_rho(i,k)        !Meyers et al. (1992)
          dum = 0.005*exp(0.304*(273.15-t(i,k)))*1000.*inv_rho(i,k)         !Cooper (1986)
!         dum = 0.005*dexp(dble(0.304*(273.15-t(i,k))))*1000.*inv_rho(i,k)  !Cooper (1986)
          dum = min(dum,100.e3*inv_rho(i,k)*SCF(k))
          N_nuc = max(0.,(dum-sum(nitot(i,k,:)))*odt)

          if (N_nuc.ge.1.e-20) then
             Q_nuc = max(0.,(dum-sum(nitot(i,k,:)))*mi0*odt)
             if (nCat>1) then
                !determine destination ice-phase category:
                dum1  = 900.     !density of new ice
                D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
                call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
                if (global_status /= STATUS_OK) return
             else
                iice_dest = 1
             endif
             qinuc(iice_dest) = Q_nuc
             ninuc(iice_dest) = N_nuc
          endif
       endif


!.................................................................
! droplet activation

! for specified Nc, make sure droplets are present if conditions are supersaturated
! note that this is also applied at the first time step
! this is not applied at the first time step, since saturation adjustment is applied at the first step

       if (.not.(log_predictNc).and.sup_cld.gt.1.e-6.and.it.gt.1) then
          dum   = nccnst*inv_rho(i,k)*cons7-qc(i,k)
          dum   = max(0.,dum*iSCF(k))         ! in-cloud value
          dumqvs = qv_sat(t(i,k),pres(i,k),0)
          dqsdT = xxlv(i,k)*dumqvs/(rv*t(i,k)*t(i,k))
          ab    = 1. + dqsdT*xxlv(i,k)*inv_cp
          dum   = max(0.,min(dum,(Qv_cld(k)-dumqvs)/ab))  ! limit overdepletion of supersaturation
          qcnuc = dum*odt*SCF(k)
       endif

       if (log_predictNc) then
         ! for predicted Nc, calculate activation explicitly from supersaturation
         ! note that this is also applied at the first time step
          if (sup_cld.gt.1.e-6) then
             dum1  = 1./bact**0.5
             sigvl = 0.0761 - 1.55e-4*(t(i,k)-273.15)
             aact  = 2.*mw/(rhow*rr*t(i,k))*sigvl
             sm1   = 2.*dum1*(aact*thrd*inv_rm1)**1.5
             sm2   = 2.*dum1*(aact*thrd*inv_rm2)**1.5
             uu1   = 2.*log(sm1/sup_cld)/(4.242*log(sig1))
             uu2   = 2.*log(sm2/sup_cld)/(4.242*log(sig2))
             dum1  = nanew1*0.5*(1.-derf(uu1)) ! activated number in kg-1 mode 1
             dum2  = nanew2*0.5*(1.-derf(uu2)) ! activated number in kg-1 mode 2
           ! make sure this value is not greater than total number of aerosol
             dum2  = min((nanew1+nanew2),dum1+dum2)
             dum2  = (dum2-nc(i,k)*iSCF(k))*odt*SCF(k)
             dum2  = max(0.,dum2)
             ncnuc = dum2
           ! don't include mass increase from droplet activation during first time step
           ! since this is already accounted for by saturation adjustment below
             if (it.le.1) then
                qcnuc = 0.
             else
                qcnuc = ncnuc*cons7
             endif
          endif
       endif


!................................................................
! saturation adjustment to get initial cloud water

! This is only called once at the beginning of the simulation
! to remove any supersaturation in the intial conditions

       if (it.le.1) then
          dumt   = th(i,k)*(pres(i,k)*1.e-5)**(rd*inv_cp)
          dumqv  = Qv_cld(k)
          dumqvs = qv_sat(dumt,pres(i,k),0)
          dums   = dumqv-dumqvs
          qccon  = dums/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*dumt**2))*odt*SCF(k)
          qccon  = max(0.,qccon)
          if (qccon.le.1.e-7) qccon = 0.
       endif


!................................................................
! autoconversion

       qc_not_small_1: if (qc(i,k)*iSCF(k).ge.1.e-8) then

          if (iparam.eq.1) then

            !Seifert and Beheng (2001)
             dum   = 1.-qc(i,k)*iSCF(k)/(qc(i,k)*iSCF(k)+qr(i,k)*iSPF(k)*(SPF(k)-SPF_clr(k)))
             dum1  = 600.*dum**0.68*(1.-dum**0.68)**3
             qcaut =  kc*1.9230769e-5*(nu(i,k)+2.)*(nu(i,k)+4.)/(nu(i,k)+1.)**2*         &
                      (rho(i,k)*qc(i,k)*iSCF(k)*1.e-3)**4/                               &
                      (rho(i,k)*nc(i,k)*iSCF(k)*1.e-6)**2*(1.+                           &
                      dum1/(1.-dum)**2)*1000.*inv_rho(i,k)*SCF(k)
             ncautc = qcaut*7.6923076e+9

          elseif (iparam.eq.2) then

            !Beheng (1994)
             if (nc(i,k)*iSCF(k)*rho(i,k)*1.e-6 .lt. 100.) then
                qcaut = 6.e+28*inv_rho(i,k)*mu_c(i,k)**(-1.7)*(1.e-6*rho(i,k)*           &
                        nc(i,k)*iSCF(k))**(-3.3)*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**(4.7) &
                        *SCF(k)
             else
               !2D interpolation of tabled logarithmic values
                dum   = 41.46 + (nc(i,k)*iSCF(k)*1.e-6*rho(i,k)-100.)*(37.53-41.46)*5.e-3
                dum1  = 39.36 + (nc(i,k)*iSCF(k)*1.e-6*rho(i,k)-100.)*(30.72-39.36)*5.e-3
                qcaut = dum+(mu_c(i,k)-5.)*(dum1-dum)*0.1
              ! 1000/rho is for conversion from g cm-3/s to kg/kg
                qcaut = exp(qcaut)*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**4.7*1000.*inv_rho(i,k)*SCF(k)
!               qcaut = dexp(dble(qcaut))*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**4.7*1000.*   &
!                       inv_rho(i,k)*SCF(k)
             endif
             ncautc = 7.7e+9*qcaut

          elseif (iparam.eq.3) then

           !Khroutdinov and Kogan (2000)
             dum   = qc(i,k)*iSCF(k)
             qcaut = 1350.*dum**2.47*(nc(i,k)*iSCF(k)*1.e-6*rho(i,k))**(-1.79)*SCF(k)
            ! note: ncautr is change in Nr; ncautc is change in Nc
             ncautr = qcaut*cons3
             ncautc = qcaut*nc(i,k)/qc(i,k)

          elseif (iparam.eq.4) then

           !Kogan (2013)
             dum = qc(i,k)*iSCF(k)
             qcaut = 7.98e10*dum**4.22*(nc(i,k)*iSCF(k)*1.e-6*rho(i,k))**(-3.01)*SCF(k)
             ncautr = qcaut*cons8
             ncautc = qcaut*nc(i,k)/qc(i,k)

          endif

          if (qcaut .eq.0.) ncautc = 0.
          if (ncautc.eq.0.) qcaut  = 0.

       endif qc_not_small_1

!............................
! self-collection of droplets

       if (qc(i,k).ge.qsmall) then

          if (iparam.eq.1) then
           !Seifert and Beheng (2001)
             ncslf = -kc*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**2*(nu(i,k)+2.)/(nu(i,k)+1.)*         &
                     1.e+6*inv_rho(i,k)*SCF(k)+ncautc
          elseif (iparam.eq.2) then
           !Beheng (994)
             ncslf = -5.5e+16*inv_rho(i,k)*mu_c(i,k)**(-0.63)*(1.e-3*rho(i,k)*qc(i,k)*iSCF(k))**2*SCF(k)
          elseif (iparam.eq.3.or.iparam.eq.4) then
            !Khroutdinov and Kogan (2000)
             ncslf = 0.
          endif

       endif

!............................
! accretion of cloud by rain

       if (qr(i,k).ge.qsmall .and. qc(i,k).ge.qsmall) then

          if (iparam.eq.1) then
           !Seifert and Beheng (2001)
             dum2  = (SPF(k)-SPF_clr(k)) !in-cloud Precipitation fraction
             dum   = 1.-qc(i,k)*iSCF(k)/(qc(i,k)*iSCF(k)+qr(i,k)*iSPF(k))
             dum1  = (dum/(dum+5.e-4))**4
             qcacc = kr*rho(i,k)*0.001*qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k)*dum1*dum2
             ncacc = qcacc*rho(i,k)*0.001*(nc(i,k)*rho(i,k)*1.e-6)/(qc(i,k)*rho(i,k)*    &  !note: (nc*iSCF)/(qc*iSCF) = nc/qc
                     0.001)*1.e+6*inv_rho(i,k)
          elseif (iparam.eq.2) then
           !Beheng (994)
             dum2  = (SPF(k)-SPF_clr(k)) !in-cloud Precipitation fraction
             dum   = (qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k))
             qcacc = 6.*rho(i,k)*dum*dum2
             ncacc = qcacc*rho(i,k)*1.e-3*(nc(i,k)*rho(i,k)*1.e-6)/(qc(i,k)*rho(i,k)*    &   !note: (nc*iSCF)/(qc*iSCF) = nc/qc
                     1.e-3)*1.e+6*inv_rho(i,k)
          elseif (iparam.eq.3) then
            !Khroutdinov and Kogan (2000)
             dum2  = (SPF(k)-SPF_clr(k)) !in-cloud Precipitation fraction
             qcacc = 67.*(qc(i,k)*iSCF(k)*qr(i,k)*iSPF(k))**1.15 *dum2
             ncacc = qcacc*nc(i,k)/qc(i,k)
          elseif (iparam.eq.4) then
            !Kogan (2013)
             dum2 = (SPF(k)-SPF_clr(k)) !in-cloud Precipitation fraction
             qcacc = 8.53*(qc(i,k)*iSCF(k))**1.05*(qr(i,k)*iSPF(k))**0.98 *dum2
             ncacc = qcacc*nc(i,k)/qc(i,k)
          endif

          if (qcacc.eq.0.) ncacc = 0.
          if (ncacc.eq.0.) qcacc = 0.

       endif

!.....................................
! self-collection and breakup of rain
! (breakup following modified Verlinde and Cotton scheme)

       if (qr(i,k).ge.qsmall) then

        ! include breakup
          dum1 = 280.e-6
          nr(i,k) = max(nr(i,k),nsmall)
        ! use mass-mean diameter (do this by using
        ! the old version of lambda w/o mu dependence)
        ! note there should be a factor of 6^(1/3), but we
        ! want to keep breakup threshold consistent so 'dum'
        ! is expressed in terms of lambda rather than mass-mean D
          dum2 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd
          if (dum2.lt.dum1) then
             dum = 1.
          else if (dum2.ge.dum1) then
             dum = 2.-exp(2300.*(dum2-dum1))
!            dum = 2.-dexp(dble(2300.*(dum2-dum1)))
          endif

          if (iparam.eq.1.) then
             nrslf = dum*kr*1.e-3*qr(i,k)*iSPF(k)*nr(i,k)*iSPF(k)*rho(i,k)*SPF(k)
          elseif (iparam.eq.2 .or. iparam.eq.3) then
             nrslf = dum*5.78*nr(i,k)*iSPF(k)*qr(i,k)*iSPF(k)*rho(i,k)*SPF(k)
          elseif (iparam.eq.4) then
             nrslf = dum*205.*(qr(i,k)*iSPF(k))**1.55*(nr(i,k)*1.e-6*rho(i,k)*iSPF(k))**0.6*1.e6/rho(i,k)*SPF(k) ! 1.e6 converts cm-3 to m-3
          endif

       endif


!.................................................................
! conservation of mass
!
! The microphysical process rates are computed above, based on the environmental conditions.
! The rates are adjusted here (where necessary) such that the sum of the sinks of mass cannot
! be greater than the sum of the sources, thereby resulting in overdepletion.

    !Limit total condensation (incl. activation) and evaporation to saturation adjustment
       dumqvs = qv_sat(t(i,k),pres(i,k),0)
       qcon_satadj  = (Qv_cld(k)-dumqvs)/(1.+xxlv(i,k)**2*dumqvs/(cp*rv*t(i,k)**2))*odt*SCF(k)

       tmp1 = qccon+qrcon+qcnuc
       if (tmp1>0. .and. qcon_satadj<0.) then
          qccon = 0.
          qrcon = 0.
          qcnuc = 0.
          ncnuc = 0.
       else
          if (tmp1.gt.0. .and. tmp1.gt.qcon_satadj) then
             ratio = max(0.,qcon_satadj)/tmp1
             ratio = min(1.,ratio)
             qccon = qccon*ratio
             qrcon = qrcon*ratio
             qcnuc = qcnuc*ratio
             ncnuc = ncnuc*ratio
          elseif (qcevp+qrevp.gt.0.) then
             ratio = max(0.,-qcon_satadj)/(qcevp+qrevp)
             ratio = min(1.,ratio)
             qcevp = qcevp*ratio
             qrevp = qrevp*ratio
             nrevp = nrevp*ratio
          endif
       endif

    !Limit total deposition (incl. nucleation) and sublimation to saturation adjustment
       qv_tmp = Qv_cld(k) + (-qcnuc-qccon-qrcon+qcevp+qrevp)*dt                              !qv after cond/evap      
       t_tmp  = t(i,k) + (qcnuc+qccon+qrcon-qcevp-qrevp)*xxlv(i,k)*inv_cp*dt                 !T after cond/evap       
       dumqvi = qv_sat(t_tmp,pres(i,k),1)
       qdep_satadj = (qv_tmp-dumqvi)/(1.+xxls(i,k)**2*dumqvi/(cp*rv*t_tmp**2))*odt*SCF(k)

       tmp1 = sum(qidep)+sum(qinuc)
       if (tmp1>0. .and. qdep_satadj<0.) then
          qidep = 0.
          qinuc = 0.
          ninuc = 0.
       else
          if (tmp1.gt.0. .and. tmp1.gt.qdep_satadj) then
             ratio = max(0.,qdep_satadj)/tmp1
             ratio = min(1.,ratio)
             qidep = qidep*ratio
             qinuc = qinuc*ratio
             ninuc = ninuc*ratio
          endif
          do iice = 1,nCat
             dum = max(qisub(iice),1.e-20)
             qisub(iice)  = qisub(iice)*min(1.,max(0.,-qdep_satadj)/max(sum(qisub), 1.e-20))  !optimized (avoids IF(qisub.gt.0.) )
             nisub(iice)  = nisub(iice)*min(1.,qisub(iice)/dum)
          enddo
         !qchetc = qchetc*min(1.,qc(i,k)*odt/max(sum(qchetc),1.e-20))  !currently not used
         !qrhetc = qrhetc*min(1.,qr(i,k)*odt/max(sum(qrhetc),1.e-20))  !currently not used
       endif


! cloud
       sinks   = (qcaut+qcacc+sum(qccol)+qcevp+sum(qchetc)+sum(qcheti)+sum(qcshd))*dt
       sources = qc(i,k) + (qccon+qcnuc)*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qcaut  = qcaut*ratio
          qcacc  = qcacc*ratio
          qcevp  = qcevp*ratio
          qccol  = qccol*ratio
          qcheti = qcheti*ratio
          qcshd  = qcshd*ratio
         !if (log_predictNc) then
         ! note: the conditional is present for strict code logic but commented for efficiency
         !       (4 multiplications, even if values are not used [if log_predictNc=.false.], are cheaper than one IF)
            ncautc = ncautc*ratio
            ncacc  = ncacc*ratio
            nccol  = nccol*ratio
            ncheti = ncheti*ratio
           !nchetc = nchetc*ratio
         !endif
       endif

! rain
       sinks   = (qrevp+sum(qrcol)+sum(qrhetc)+sum(qrheti)+sum(qrmul))*dt
       sources = qr(i,k) + (qrcon+qcaut+qcacc+sum(qimlt)+sum(qcshd))*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qrevp  = qrevp*ratio
          qrcol  = qrcol*ratio
          qrheti = qrheti*ratio
          qrmul  = qrmul*ratio
          nrevp  = nrevp*ratio
          nrcol  = nrcol*ratio
          nrheti = nrheti*ratio
         !qrhetc = qrhetc*ratio
         !nrhetc = nrhetc*ratio
       endif

! ice
       do iice = 1,nCat
          sinks   = (qisub(iice)+qimlt(iice))*dt
          sources = qitot(i,k,iice) + (qidep(iice)+qinuc(iice)+qrcol(iice)+qccol(iice)+  &
                    qrhetc(iice)+qrheti(iice)+qchetc(iice)+qcheti(iice)+qrmul(iice))*dt
          do catcoll = 1,nCat
            !category interaction leading to source for iice category
             sources = sources + qicol(catcoll,iice)*dt
            !category interaction leading to sink for iice category
             sinks = sinks + qicol(iice,catcoll)*dt
          enddo
          if (sinks.gt.sources .and. sinks.ge.1.e-20) then
             ratio = sources/sinks
             qisub(iice) = qisub(iice)*ratio
             qimlt(iice) = qimlt(iice)*ratio
             nisub(iice) = nisub(iice)*ratio
             nimlt(iice) = nimlt(iice)*ratio
             do catcoll = 1,nCat
                qicol(iice,catcoll) = qicol(iice,catcoll)*ratio
                nicol(iice,catcoll) = nicol(iice,catcoll)*ratio
             enddo
          endif
      enddo  !iice-loop

! vapor
       sinks   = (qccon+qrcon+qcnuc+sum(qidep)+sum(qinuc))*dt
       sources = qv(i,k) + (qcevp+qrevp+sum(qisub))*dt
       if (sinks.gt.sources .and. sinks.ge.1.e-20) then
          ratio  = sources/sinks
          qccon  = qccon*ratio
          qrcon  = qrcon*ratio
          qcnuc  = qcnuc*ratio
          qidep  = qidep*ratio
          qinuc  = qinuc*ratio
          ninuc  = ninuc*ratio
          ncnuc  = ncnuc*ratio
       endif

!------------------------------------------------------------------------------------------!
! Update ice reflectivity

! At this point, we have the values of prognostic variables at beginning of time step,
! the value of all process rates for qitot and nitot

      update_refl_processes: if (log_3momentIce) then

       iice_loop_z1: do iice = 1,nCat

       !----  Group 1 process rates (assume mu_i does not change)
       !
       !   upated value of zitot is computed for these processes

        !-- compute "updated" values of qitot and nitot (used here only)
        ! NOTE: must add qicol in line below for combining 3-moment with multi-cat P3
          dumm3(iice) = qitot(i,k,iice) + ( qidep(iice)+qrcol(iice)+qccol(iice)+     &
                                            qrmul(iice)-qisub(iice)-qimlt(iice) )*dt
        ! NOTE: must add nicol in line below for combining 3-moment with multi-cat P3
          dumm0(iice) = nitot(i,k,iice) + (-nimlt(iice)-nisub(iice)+nimul(iice)-nislf(iice) )*dt

       enddo iice_loop_z1
       !====
       iice_loop_z2: do iice = 1,nCat

         !update further due to category interactions:
          do catcoll = 1,nCat
             dumm3(catcoll) = dumm3(catcoll) - qicol(catcoll,iice)*dt
             dumm3(iice)    = dumm3(iice)    + qicol(catcoll,iice)*dt
             dumm0(catcoll) = dumm0(catcoll) - nicol(catcoll,iice)*dt
          enddo ! catcoll loop

          if (dumm3(iice).ge.qsmall) then

            !estimate moment3 from updated qitot (dum2).
             if (qitot(i,k,iice).ge.qsmall) then
               !need to use mean ice density (f1pr16) from beginning of step, since the updated value is not available
                dumm3(iice) = 6./(f1pr16*pi)*dumm3(iice)
             else
               !if there is no existing ice, assume an ice density of 900 kg m-3
                dumm3(iice) = 6./(900.*pi)*dumm3(iice)
             endif

            !solve or assign for mu_i (to be used to compute updated zitot)
            if (qitot(i,k,iice).ge.qsmall) then
               !solve for mu_i from values of mom0,mom3,mom6 at beginning of time step
                dum1 =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                mu_i = compute_mu_3moment(nitot(i,k,iice),dum1,zitot(i,k,iice),mu_i_max)
             else
               !no ice present, therefore assign an initial value
                mu_i = mu_i_initial
             endif

            !compute zitot as a function of (old) mu_i and the "updated" moment_0 (dumm0) and moment_3 (dumm3)
             zitot(i,k,iice) = G_of_mu(mu_i)*dumm3(iice)**2/max(dumm0(iice),nsmall)

          else
             zitot(i,k,iice) = 0.
          endif

       !====

       !----  Group 2 (initiation processes, where mu_i for the new ice resulting from that process (only) is assigned
       !              note: mu_i_new is the mu_i associated with the new added ice for that process

        !proceses with rain freezing:
          tmp2 =  nrhetc(iice) + nrheti(iice)                  !moment_0 tendency
          if (tmp2.ge.qsmall) then
             tmp1 = (qrhetc(iice) + qrheti(iice))*6./(900.*pi) !estimate of moment_3 tendency
             mu_i_new = mu_r(i,k)
             zitot(i,k,iice) = zitot(i,k,iice) + G_of_mu(mu_i_new)*tmp1**2/tmp2*dt
          endif

        !proceses with cloud freezing:
          tmp2 =  nchetc(iice) + ncheti(iice)                  !moment_0 tendency
          if (tmp2.ge.qsmall) then
             tmp1 = (qchetc(iice) + qcheti(iice))*6./(900.*pi) !estimate of moment_3 tendency
             mu_i_new = mu_c(i,k)
             zitot(i,k,iice) = zitot(i,k,iice) + G_of_mu(mu_i_new)*tmp1**2/tmp2*dt
          endif

        !proceses of deposition nucleation
          tmp2 = ninuc(iice)                                   !moment_0 tendency
          if (tmp2.ge.qsmall) then
             tmp1 = qinuc(iice)*6./(900.*pi)                   !estimate of moment_3 tendency
             mu_i_new = mu_i_initial                           !estimated assigned value
             zitot(i,k,iice) = zitot(i,k,iice) + G_of_mu(mu_i_new)*tmp1**2/tmp2*dt
          endif

       !====

       !----  Group 3 -- processes that we know how to do formally
       ! FUTURE.  e.g. diffusional growth, riming, drop freezing
       !====

       end do iice_loop_z2

      endif update_refl_processes

      ! at this point, zitot has been completely updated due to all process rates (except sedimentation)

!======================================================================================!


!---------------------------------------------------------------------------------
! update prognostic microphysics and thermodynamics variables
!---------------------------------------------------------------------------------

   !-- ice-phase dependent processes:

       iice_loop2: do iice = 1,nCat

       ! compute fractions before update (assumed constant during ice-ice coll.)
        if (qitot(i,k,iice).ge.qsmall) then
         tmp1 = 1./qitot(i,k,iice)
         rimevolume(i,k,iice) = birim(i,k,iice)*tmp1
         rimefraction(i,k,iice) = qirim(i,k,iice)*tmp1
        endif

       enddo iice_loop2

       iice_loop3: do iice = 1,nCat

          qc(i,k) = qc(i,k) + (-qchetc(iice)-qcheti(iice)-qccol(iice)-qcshd(iice))*dt
          if (log_predictNc) then
             nc(i,k) = nc(i,k) + (-nccol(iice)-nchetc(iice)-ncheti(iice))*dt
          endif

          qr(i,k) = qr(i,k) + (-qrcol(iice)+qimlt(iice)-qrhetc(iice)-qrheti(iice)+            &
                    qcshd(iice)-qrmul(iice))*dt
        ! apply factor to source for rain number from melting of ice, (ad-hoc
        ! but accounts for rapid evaporation of small melting ice particles)
          nr(i,k) = nr(i,k) + (-nrcol(iice)-nrhetc(iice)-nrheti(iice)+nmltratio*nimlt(iice)+  &
                    nrshdr(iice)+ncshdc(iice))*dt

         ! if (qitot(i,k,iice).ge.qsmall) then
         ! add sink terms, assume density stays constant for sink terms
             birim(i,k,iice) = birim(i,k,iice) - ((qisub(iice)+qimlt(iice))*dt*               &
                               rimevolume(i,k,iice))
             qirim(i,k,iice) = qirim(i,k,iice) - ((qisub(iice)+qimlt(iice))*dt*               &
                               rimefraction(i,k,iice))
             qitot(i,k,iice) = qitot(i,k,iice) - (qisub(iice)+qimlt(iice))*dt
         ! endif

          dum             = (qrcol(iice)+qccol(iice)+qrhetc(iice)+qrheti(iice)+          &
                            qchetc(iice)+qcheti(iice)+qrmul(iice))*dt
          qitot(i,k,iice) = qitot(i,k,iice) + (qidep(iice)+qinuc(iice))*dt + dum
          qirim(i,k,iice) = qirim(i,k,iice) + dum
          birim(i,k,iice) = birim(i,k,iice) + (qrcol(iice)*inv_rho_rimeMax+qccol(iice)/  &
                            rhorime_c(iice)+(qrhetc(iice)+qrheti(iice)+qchetc(iice)+     &
                            qcheti(iice)+qrmul(iice))*inv_rho_rimeMax)*dt
          nitot(i,k,iice) = nitot(i,k,iice) + (ninuc(iice)-nimlt(iice)-nisub(iice)-      &
                            nislf(iice)+nrhetc(iice)+nrheti(iice)+nchetc(iice)+          &
                            ncheti(iice)+nimul(iice))*dt

          if (nCat.gt.1) then
             interactions_loop: do catcoll = 1,nCat
               diff_categories:     if (iice.ne.catcoll) then
                ! add ice-ice category interaction collection tendencies
                ! note: nicol is a sink for the collectee category, but NOT a source for collector
                ! modify rime mass and density, assume collection does not modify rime mass
                ! fraction or density of the collectee, consistent with the assumption that
                ! these are constant over the PSD
                if (qitot(i,k,catcoll).ge.qsmall) then
                 !source for collector category
                  qirim(i,k,iice) = qirim(i,k,iice)+qicol(catcoll,iice)*dt*                    &
                                    rimefraction(i,k,catcoll)
                  birim(i,k,iice) = birim(i,k,iice)+qicol(catcoll,iice)*dt*                    &
                                    rimevolume(i,k,catcoll)
                 !sink for collectee category
                  qirim(i,k,catcoll) = qirim(i,k,catcoll)-qicol(catcoll,iice)*dt*                 &
                                       rimefraction(i,k,catcoll)
                  birim(i,k,catcoll) = birim(i,k,catcoll)-qicol(catcoll,iice)*dt*                 &
                                       rimevolume(i,k,catcoll)
                endif
                qitot(i,k,catcoll) = qitot(i,k,catcoll) - qicol(catcoll,iice)*dt
                nitot(i,k,catcoll) = nitot(i,k,catcoll) - nicol(catcoll,iice)*dt
                qitot(i,k,iice)    = qitot(i,k,iice)    + qicol(catcoll,iice)*dt

               endif diff_categories
             enddo interactions_loop ! catcoll loop
          endif

          if (qirim(i,k,iice).lt.0.) then
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          endif

        ! densify ice during wet growth (assume total soaking)
          if (log_wetgrowth(iice)) then
             qirim(i,k,iice) = qitot(i,k,iice)
             birim(i,k,iice) = qirim(i,k,iice)*inv_rho_rimeMax
          endif

        ! densify rimed ice during melting (tend rime density towards solid ice [917 kg m-3])
          if (qitot(i,k,iice).ge.qsmall .and. birim(i,k,iice).ge.bsmall .and. qimlt(iice)>0.) then
             tmp1 = qirim(i,k,iice)/birim(i,k,iice)     ! rho_i before densification
             tmp2 = qitot(i,k,iice) + qimlt(iice)*dt    ! qitot before melting (but after all other updates)
             birim(i,k,iice) = qirim(i,k,iice)/(tmp1+(917.-tmp1)*qimlt(iice)*dt/tmp2)
          endif

        ! densify in above freezing conditions and melting
        ! -- future work --
        !   Ideally, this will be treated with the predicted liquid fraction in ice.
        !   Alternatively, it can be simplified by tending qirim -- qitot
        !   and birim such that rho_rim (qirim/birim) --> rho_liq during melting.
        ! ==

          qv(i,k) = qv(i,k) + (-qidep(iice)+qisub(iice)-qinuc(iice))*dt

        ! Update theta. Note temperature is not updated here even though it is used below for
        ! the homogeneous freezing threshold. This is done for simplicity - the error will be
        ! very small and the homogeneous temp. freezing threshold is approximate anyway.
          th(i,k) = th(i,k) + invexn(i,k)*((qidep(iice)-qisub(iice)+qinuc(iice))*      &
                              xxls(i,k)*inv_cp +(qrcol(iice)+qccol(iice)+qchetc(iice)+ &
                              qcheti(iice)+qrhetc(iice)+qrheti(iice)+                  &
                              qrmul(iice)-qimlt(iice))*                                &
                              xlf(i,k)*inv_cp)*dt

       enddo iice_loop3
   !==

   !-- warm-phase only processes:
       qc(i,k) = qc(i,k) + (-qcacc-qcaut+qcnuc+qccon-qcevp)*dt
       qr(i,k) = qr(i,k) + (qcacc+qcaut+qrcon-qrevp)*dt

       if (log_predictNc) then
          nc(i,k) = nc(i,k) + (-ncacc-ncautc+ncslf+ncnuc)*dt
       else
          nc(i,k) = nccnst*inv_rho(i,k)
       endif
       if (iparam.eq.1 .or. iparam.eq.2) then
          nr(i,k) = nr(i,k) + (0.5*ncautc-nrslf-nrevp)*dt
       else
          nr(i,k) = nr(i,k) + (ncautr-nrslf-nrevp)*dt
       endif

       qv(i,k) = qv(i,k) + (-qcnuc-qccon-qrcon+qcevp+qrevp)*dt
       th(i,k) = th(i,k) + invexn(i,k)*((qcnuc+qccon+qrcon-qcevp-qrevp)*xxlv(i,k)*    &
                 inv_cp)*dt
   !==

     ! clipping for small hydrometeor values
       if (qc(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qc(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.
       endif

       if (qr(i,k).lt.qsmall) then
          qv(i,k) = qv(i,k) + qr(i,k)
          th(i,k) = th(i,k) - invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       else
          log_hydrometeorsPresent = .true.
       endif

       do iice = 1,nCat
          if (qitot(i,k,iice).lt.qsmall) then
             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
          else
             log_hydrometeorsPresent = .true.
          endif
       enddo !iice-loop

       qv(i,k) = max(0., qv(i,k))
       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))

!---------------------------------------------------------------------------------

555    continue

    enddo k_loop_main

!-- for sedimentation-only tests:
! 6969 continue
! log_hydrometeorsPresent = .true.
!==

!......................................
! zero out zitot if there is no qitot for triple moment
    if (log_3momentIce) then
       do iice = 1,nCat
          do k = kbot,ktop,kdir
             if (qitot(i,k,iice).lt.qsmall) zitot(i,k,iice) = 0.
          enddo
       enddo
    endif
!.......................................


    !NOTE: At this point, it is possible to have negative (but small) nc, nr, nitot.  This is not
    !      a problem; those values get clipped to zero or assigned a minumum value in the sedimentation
    !      section, immediately below (if necessary).  Similarly, for 3-moment-ice it is possible at this
    !      point to have zitot=0 but qitot slightly larger than qsmall; in sedimentation non-zero zitot
    !      is computed (if necessary) by applying the constraints on mu_i.

    if (debug_on) then
       location_ind = 300
       force_abort  = debug_ABORT
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       if (log_3momentIce) then
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,         &
                 Zitot=zitot(i,:,:))
       else
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
       endif
       if (global_status /= STATUS_OK) return
    endif

   !second call to compute_SCPF
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.false.)

    if (.not. log_hydrometeorsPresent) goto 333

!------------------------------------------------------------------------------------------!
! End of main microphysical processes section
!==========================================================================================!

!==========================================================================================!
! Sedimentation:

!------------------------------------------------------------------------------------------!
! Cloud sedimentation:  (adaptivive substepping)

    log_qxpresent = .false.
    k_qxtop       = kbot

   !find top, determine qxpresent
    do k = ktop,kbot,-kdir
       if (qc(i,k)*iSCF(k).ge.qsmall) then
          log_qxpresent = .true.
          k_qxtop = k
          exit
       endif
    enddo

    qc_present: if (log_qxpresent) then

       dt_left   = dt  !time remaining for sedi over full model (mp) time step
       prt_accum = 0.  !precip rate for individual category

      !find bottom
       do k = kbot,k_qxtop,kdir
          if (qc(i,k)*iSCF(k).ge.qsmall) then
             k_qxbot = k
             exit
          endif
       enddo

       two_moment: if (log_predictNc) then  !2-moment cloud:

          substep_sedi_c2: do while (dt_left.gt.1.e-4)

             Co_max  = 0.
             V_qc(:) = 0.
             V_nc(:) = 0.

             kloop_sedi_c2: do k = k_qxtop,k_qxbot,-kdir

                if (qc(i,k)*iSCF(k)>qsmall) then
                   call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,   &
                                   lamc(i,k),lammin,lammax,tmp1,tmp2,iSCF(k))
                   dum = 1./lamc(i,k)**bcn
                   V_qc(k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+4.))
                   V_nc(k) = acn(i,k)*gamma(1.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+1.))
                endif

                Co_max = max(Co_max, V_qc(k)*dt_left*inv_dzq(i,k))

             enddo kloop_sedi_c2

             !-- compute dt_sub
             tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
             dt_sub  = min(dt_left, dt_left/float(tmpint1))

             if (k_qxbot.eq.kbot) then
                k_temp = k_qxbot
             else
                k_temp = k_qxbot-kdir
             endif

             !-- calculate fluxes
             do k = k_temp,k_qxtop,kdir
                flux_qx(k) = V_qc(k)*qc(i,k)*rho(i,k)
                flux_nx(k) = V_nc(k)*nc(i,k)*rho(i,k)
             enddo

             !accumulated precip during time step
             if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub
             !or, optimized: prt_accum = prt_accum - (k_qxbot.eq.kbot)*dt_sub

             !-- for top level only (since flux is 0 above)
             k = k_qxtop
             fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
             fluxdiv_nx = -flux_nx(k)*inv_dzq(i,k)
             qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             nc(i,k) = nc(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)

             do k = k_qxtop-kdir,k_temp,-kdir
                fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
                fluxdiv_nx = (flux_nx(k+kdir) - flux_nx(k))*inv_dzq(i,k)
                qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
                nc(i,k) = nc(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)
             enddo

             dt_left = dt_left - dt_sub  !update time remaining for sedimentation
             if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir

          enddo substep_sedi_c2

       else  !1-moment cloud:

          substep_sedi_c1: do while (dt_left.gt.1.e-4)

             Co_max  = 0.
             V_qc(:) = 0.

             kloop_sedi_c1: do k = k_qxtop,k_qxbot,-kdir

                if (qc(i,k)*iSCF(k)>qsmall) then
                   call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,   &
                                       lamc(i,k),lammin,lammax,tmp1,tmp2,iSCF(k))
                   dum = 1./lamc(i,k)**bcn
                   V_qc(k) = acn(i,k)*gamma(4.+bcn+mu_c(i,k))*dum/(gamma(mu_c(i,k)+4.))
                endif

                Co_max = max(Co_max, V_qc(k)*dt_left*inv_dzq(i,k))

             enddo kloop_sedi_c1

             tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
             dt_sub  = min(dt_left, dt_left/float(tmpint1))

             if (k_qxbot.eq.kbot) then
                k_temp = k_qxbot
             else
                k_temp = k_qxbot-kdir
             endif

             do k = k_temp,k_qxtop,kdir
                flux_qx(k) = V_qc(k)*qc(i,k)*rho(i,k)
             enddo

             !accumulated precip during time step
             if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub

             !-- for top level only (since flux is 0 above)
             k = k_qxtop
             fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
             qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)

             do k = k_qxtop-kdir,k_temp,-kdir
                fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
                qc(i,k) = qc(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             enddo

             dt_left = dt_left - dt_sub  !update time remaining for sedimentation
             if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir

          enddo substep_sedi_c1

       ENDIF two_moment

       prt_liq(i) = prt_accum*inv_rhow*odt  !note, contribution from rain is added below

    endif qc_present


!------------------------------------------------------------------------------------------!
! Rain sedimentation:  (adaptivive substepping)

    log_qxpresent = .false.
    k_qxtop       = kbot

    !find top, determine qxpresent
    do k = ktop,kbot,-kdir
       if (qr(i,k)*iSPF(k).ge.qsmall) then
          log_qxpresent = .true.
          k_qxtop = k
          exit
       endif !
    enddo

    qr_present: if (log_qxpresent) then

       dt_left   = dt  !time remaining for sedi over full model (mp) time step
       prt_accum = 0.  !precip rate for individual category

      !find bottom
       do k = kbot,k_qxtop,kdir
          if (qr(i,k)*iSPF(k).ge.qsmall) then
             k_qxbot = k
             exit
          endif
       enddo

       substep_sedi_r: do while (dt_left.gt.1.e-4)

          Co_max  = 0.
          V_qr(:) = 0.
          V_nr(:) = 0.

          kloop_sedi_r1: do k = k_qxtop,k_qxbot,-kdir

             qr_not_small_1: if (qr(i,k)*iSPF(k)>qsmall) then

               !Compute Vq, Vn:
                nr(i,k)  = max(nr(i,k),nsmall)
                call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),cdistr(i,k),      &
                                   logn0r(i,k),iSPF(k))

                call find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3, &
                                        mu_r(i,k),lamr(i,k))
                !mass-weighted fall speed:
                dum1 = vm_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vm_table(dumii+1,dumjj)-vm_table(dumii,dumjj))         !at mu_r
                dum2 = vm_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vm_table(dumii+1,dumjj+1)-vm_table(dumii,dumjj+1))   !at mu_r+1

                V_qr(k) = dum1 + (rdumjj-real(dumjj))*(dum2-dum1)         !interpolated
                V_qr(k) = V_qr(k)*rhofacr(i,k)               !corrected for air density

                ! number-weighted fall speed:
                dum1 = vn_table(dumii,dumjj)+(rdumii-real(dumii))*                       &
                       (vn_table(dumii+1,dumjj)-vn_table(dumii,dumjj))        !at mu_r
                dum2 = vn_table(dumii,dumjj+1)+(rdumii-real(dumii))*                     &
                       (vn_table(dumii+1,dumjj+1)-vn_table(dumii,dumjj+1))    !at mu_r+1

                V_nr(k) = dum1+(rdumjj-real(dumjj))*(dum2-dum1)            !interpolated
                V_nr(k) = V_nr(k)*rhofacr(i,k)                !corrected for air density

             endif qr_not_small_1

             Co_max = max(Co_max, V_qr(k)*dt_left*inv_dzq(i,k))
!            Co_max = max(Co_max, max(V_nr(k),V_qr(k))*dt_left*inv_dzq(i,k))

          enddo kloop_sedi_r1

          !-- compute dt_sub
          tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
          dt_sub  = min(dt_left, dt_left/float(tmpint1))

          if (k_qxbot.eq.kbot) then
             k_temp = k_qxbot
          else
             k_temp = k_qxbot-kdir
          endif

          !-- calculate fluxes
          do k = k_temp,k_qxtop,kdir
             flux_qx(k) = V_qr(k)*qr(i,k)*rho(i,k)
             flux_nx(k) = V_nr(k)*nr(i,k)*rho(i,k)
             mflux_r(i,k) = flux_qx(k)  !store mass flux for use in visibility diagnostic)
          enddo

          !accumulated precip during time step
          if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qx(kbot)*dt_sub
          !or, optimized: prt_accum = prt_accum - (k_qxbot.eq.kbot)*dt_sub

          !--- for top level only (since flux is 0 above)
          k = k_qxtop
          !- compute flux divergence
          fluxdiv_qx = -flux_qx(k)*inv_dzq(i,k)
          fluxdiv_nx = -flux_nx(k)*inv_dzq(i,k)
          !- update prognostic variables
          qr(i,k) = qr(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
          nr(i,k) = nr(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)

          do k = k_qxtop-kdir,k_temp,-kdir
             !-- compute flux divergence
             fluxdiv_qx = (flux_qx(k+kdir) - flux_qx(k))*inv_dzq(i,k)
             fluxdiv_nx = (flux_nx(k+kdir) - flux_nx(k))*inv_dzq(i,k)
             !-- update prognostic variables
             qr(i,k) = qr(i,k) + fluxdiv_qx*dt_sub*inv_rho(i,k)
             nr(i,k) = nr(i,k) + fluxdiv_nx*dt_sub*inv_rho(i,k)
          enddo

          dt_left = dt_left - dt_sub  !update time remaining for sedimentation
          if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir
          !or, optimzed: k_qxbot = k_qxbot +(k_qxbot.eq.kbot)*kdir

       enddo substep_sedi_r

       prt_liq(i) = prt_liq(i) + prt_accum*inv_rhow*odt

    endif qr_present


!------------------------------------------------------------------------------------------!
! Ice sedimentation:  (adaptivive substepping)

    iice_loop_sedi_ice:  do iice = 1,nCat

       log_qxpresent = .false.  !note: this applies to ice category 'iice' only
       k_qxtop       = kbot

      !find top, determine qxpresent
       do k = ktop,kbot,-kdir
          if (qitot(i,k,iice).ge.qsmall) then
             log_qxpresent = .true.
             k_qxtop = k
             exit
          endif !
       enddo  !k-loop

       qi_present: if (log_qxpresent) then

          dt_left   = dt  !time remaining for sedi over full model (mp) time step
          prt_accum = 0.  !precip rate for individual category

         !find bottom
          do k = kbot,k_qxtop,kdir
             if (qitot(i,k,iice).ge.qsmall) then
                k_qxbot = k
                exit
             endif
          enddo

          three_moment_ice_1:  if (.not. log_3momentIce) then

             substep_sedi_i1: do while (dt_left.gt.1.e-4)

                Co_max   = 0.
                V_qit(:) = 0.
                V_nit(:) = 0.

                kloop_sedi_i1: do k = k_qxtop,k_qxbot,-kdir

                   !-- compute Vq, Vn (get values from lookup table)
                   qi_notsmall_i1: if (qitot(i,k,iice).ge.qsmall) then

                    !--Compute Vq, Vn:
                      nitot(i,k,iice) = max(nitot(i,k,iice),nsmall) !impose lower limits to prevent log(<0)
                      call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)
                      call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,  &
                                isize,rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),   &
                                qirim(i,k,iice),rhop)
                      call access_lookup_table(dumjj,dumii,dumi, 1,dum1,dum4,dum5,f1pr01)
                      call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
                      call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
                      call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
                    !-impose mean ice size bounds (i.e. apply lambda limiters)
                      nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
                      nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))
                      V_qit(k) = f1pr02*rhofaci(i,k)     !mass-weighted  fall speed (with density factor)
                      V_nit(k) = f1pr01*rhofaci(i,k)     !number-weighted    fall speed (with density factor)
                    !==

                   endif qi_notsmall_i1

                   Co_max = max(Co_max, V_qit(k)*dt_left*inv_dzq(i,k))

                enddo kloop_sedi_i1

                !-- compute dt_sub
                tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
                dt_sub  = min(dt_left, dt_left/float(tmpint1))

                if (k_qxbot.eq.kbot) then
                   k_temp = k_qxbot
                else
                   k_temp = k_qxbot-kdir
                endif

                !-- calculate fluxes
                do k = k_temp,k_qxtop,kdir
                   flux_qit(k) = V_qit(k)*qitot(i,k,iice)*rho(i,k)
                   flux_nit(k) = V_nit(k)*nitot(i,k,iice)*rho(i,k)
                   flux_qir(k) = V_qit(k)*qirim(i,k,iice)*rho(i,k)
                   flux_bir(k) = V_qit(k)*birim(i,k,iice)*rho(i,k)
                   mflux_i(i,k) = flux_qit(k)  !store mass flux for use in visibility diagnostic)
                enddo

                !accumulated precip during time step
                if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qit(kbot)*dt_sub
                !or, optimized: prt_accum = prt_accum - (k_qxbot.eq.kbot)*dt_sub

                !--- for top level only (since flux is 0 above)
                k = k_qxtop
                !-- compute flux divergence
                fluxdiv_qit = -flux_qit(k)*inv_dzq(i,k)
                fluxdiv_qir = -flux_qir(k)*inv_dzq(i,k)
                fluxdiv_bir = -flux_bir(k)*inv_dzq(i,k)
                fluxdiv_nit = -flux_nit(k)*inv_dzq(i,k)
                !-- update prognostic variables
                qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
                qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
                birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
                nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)

                do k = k_qxtop-kdir,k_temp,-kdir
                   !-- compute flux divergence
                   fluxdiv_qit = (flux_qit(k+kdir) - flux_qit(k))*inv_dzq(i,k)
                   fluxdiv_qir = (flux_qir(k+kdir) - flux_qir(k))*inv_dzq(i,k)
                   fluxdiv_bir = (flux_bir(k+kdir) - flux_bir(k))*inv_dzq(i,k)
                   fluxdiv_nit = (flux_nit(k+kdir) - flux_nit(k))*inv_dzq(i,k)
                   !-- update prognostic variables
                   qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
                   qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
                   birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
                   nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)
                enddo

                dt_left = dt_left - dt_sub  !update time remaining for sedimentation
                if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir
                !or, optimzed: k_qxbot = k_qxbot +(k_qxbot.eq.kbot)*kdir

             enddo substep_sedi_i1
! .............................................................................................................
          else  ! three_moment_ice_1

             substep_sedi_i2: do while (dt_left.gt.1.e-4)

                Co_max   = 0.
                V_qit(:) = 0.
                V_nit(:) = 0.
                V_zit(:) = 0.

                kloop_sedi_i2: do k = k_qxtop,k_qxbot,-kdir

                   !-- compute Vq, Vn (get values from lookup table)
                   qi_notsmall_i2: if (qitot(i,k,iice).ge.qsmall) then

                    !--Compute Vq, Vn:
                      nitot(i,k,iice) = max(nitot(i,k,iice),nsmall) !impose lower limits to prevent log(<0)
                      call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

                      call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,  &
                                isize,rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),   &
                                qirim(i,k,iice),rhop)

                    ! get Z_norm indices

                    !impose lower limits to prevent taking log of # < 0
                      zitot(i,k,iice) = max(zitot(i,k,iice),zsmall)

                      dum1z =  6./(200.*pi)*qitot(i,k,iice)  !estimate of moment3, as starting point use 200 kg m-3 estimate of bulk density

                      do imu=1,niter_mui
                         mu_i = compute_mu_3moment(nitot(i,k,iice),dum1z,zitot(i,k,iice),mu_i_max)
                         call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),mu_i)
                         call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) ! find actual bulk density
                         dum1z =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                      enddo

                      !call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),zitot(i,k,iice)) !HM moved to above

                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 1,dum1,dum4,dum5,dum6,f1pr01)
                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 2,dum1,dum4,dum5,dum6,f1pr02)
                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 7,dum1,dum4,dum5,dum6,f1pr09)
                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 8,dum1,dum4,dum5,dum6,f1pr10)
                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,13,dum1,dum4,dum5,dum6,f1pr19)

                    !impose mean ice size bounds (i.e. apply lambda limiters)
                      nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
                      nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))

                      V_qit(k) = f1pr02*rhofaci(i,k)     !mass-weighted fall speed (with density factor)
                      V_nit(k) = f1pr01*rhofaci(i,k)     !number-weighted fall speed (with density factor)
                      V_zit(k) = f1pr19*rhofaci(i,k)     !reflectivity-weighted fall speed (with density factor)

                   endif qi_notsmall_i2

                   ! use V_zit for calculating sub-stepping since it is larger than V_qit
                   Co_max = max(Co_max, V_zit(k)*dt_left*inv_dzq(i,k))

                enddo kloop_sedi_i2

                !-- compute dt_sub
                tmpint1 = int(Co_max+1.)    !number of substeps remaining if dt_sub were constant
                dt_sub  = min(dt_left, dt_left/float(tmpint1))

                if (k_qxbot.eq.kbot) then
                   k_temp = k_qxbot
                else
                   k_temp = k_qxbot-kdir
                endif

                !-- calculate fluxes
                do k = k_temp,k_qxtop,kdir
                   flux_qit(k) = V_qit(k)*qitot(i,k,iice)*rho(i,k)
                   flux_nit(k) = V_nit(k)*nitot(i,k,iice)*rho(i,k)
                   flux_qir(k) = V_qit(k)*qirim(i,k,iice)*rho(i,k)
                   flux_bir(k) = V_qit(k)*birim(i,k,iice)*rho(i,k)
                   flux_zit(k) = V_zit(k)*zitot(i,k,iice)*rho(i,k)
                   mflux_i(i,k) = flux_qit(k)  !store mass flux for use in visibility diagnostic)
                enddo

                !accumulated precip during time step
                if (k_qxbot.eq.kbot) prt_accum = prt_accum + flux_qit(kbot)*dt_sub
                !or, optimized: prt_accum = prt_accum - (k_qxbot.eq.kbot)*dt_sub

                !--- for top level only (since flux is 0 above)
                k = k_qxtop
                !-- compute flux divergence
                fluxdiv_qit = -flux_qit(k)*inv_dzq(i,k)
                fluxdiv_qir = -flux_qir(k)*inv_dzq(i,k)
                fluxdiv_bir = -flux_bir(k)*inv_dzq(i,k)
                fluxdiv_nit = -flux_nit(k)*inv_dzq(i,k)
                fluxdiv_zit = -flux_zit(k)*inv_dzq(i,k)
                !-- update prognostic variables
                qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
                qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
                birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
                nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)
                zitot(i,k,iice) = zitot(i,k,iice) + fluxdiv_zit*dt_sub*inv_rho(i,k)


                do k = k_qxtop-kdir,k_temp,-kdir
                   !-- compute flux divergence
                   fluxdiv_qit = (flux_qit(k+kdir) - flux_qit(k))*inv_dzq(i,k)
                   fluxdiv_qir = (flux_qir(k+kdir) - flux_qir(k))*inv_dzq(i,k)
                   fluxdiv_bir = (flux_bir(k+kdir) - flux_bir(k))*inv_dzq(i,k)
                   fluxdiv_nit = (flux_nit(k+kdir) - flux_nit(k))*inv_dzq(i,k)
                   fluxdiv_zit = (flux_zit(k+kdir) - flux_zit(k))*inv_dzq(i,k)
                   !-- update prognostic variables
                   qitot(i,k,iice) = qitot(i,k,iice) + fluxdiv_qit*dt_sub*inv_rho(i,k)
                   qirim(i,k,iice) = qirim(i,k,iice) + fluxdiv_qir*dt_sub*inv_rho(i,k)
                   birim(i,k,iice) = birim(i,k,iice) + fluxdiv_bir*dt_sub*inv_rho(i,k)
                   nitot(i,k,iice) = nitot(i,k,iice) + fluxdiv_nit*dt_sub*inv_rho(i,k)
                   zitot(i,k,iice) = zitot(i,k,iice) + fluxdiv_zit*dt_sub*inv_rho(i,k)
                enddo

                dt_left = dt_left - dt_sub  !update time remaining for sedimentation
                if (k_qxbot.ne.kbot) k_qxbot = k_qxbot - kdir
                !or, optimzed: k_qxbot = k_qxbot +(k_qxbot.eq.kbot)*kdir

              enddo substep_sedi_i2

          endif three_moment_ice_1

          prt_sol(i) = prt_sol(i) + prt_accum*inv_rhow*odt

       endif qi_present

    enddo iice_loop_sedi_ice  !iice-loop

!------------------------------------------------------------------------------------------!

! note: This debug check is commented since small negative qx,nx values are possible here
!       (but get adjusted below).  If uncommented, caution in interpreting results.
!
!     if (debug_on) then
!        location_ind = 600
!        force_abort  = debug_ABORT
!        if (log_3momentIce) then
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
!                  Zitot=zitot(i,:,:))
!        else
!           call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                  qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
!        endif
!        if (global_status /= STATUS_OK) return
!     endif

!------------------------------------------------------------------------------------------!
! End of sedimentation section
!==========================================================================================!

   !third and last call to compute_SCPF
    call compute_SCPF(Qc(i,:)+sum(Qitot(i,:,:),dim=2),Qr(i,:),Qv(i,:),Qvi(i,:),          &
                      Pres(i,:),ktop,kbot,kdir,SCF,iSCF,SPF,iSPF,SPF_clr,Qv_cld,Qv_clr,  &
                      SCPF_on,scpf_pfrac,scpf_resfact,quick=.true.)

!.......................................
! homogeneous freezing of cloud and rain

    k_loop_fz:  do k = kbot,ktop,kdir

    ! compute mean-mass ice diameters (estimated; rigorous approach to be implemented later)
       diam_ice(i,k,:) = 0.
       do iice = 1,nCat
          if (qitot(i,k,iice).ge.qsmall) then
             dum1 = max(nitot(i,k,iice),nsmall)
             dum2 = 500. !ice density
             diam_ice(i,k,iice) = ((qitot(i,k,iice)*6.)/(dum1*dum2*pi))**thrd
          endif
       enddo  !iice loop

       qc_not_small_2: if (qc(i,k).ge.qsmall .and. t(i,k).lt.233.15) then

          Q_nuc = qc(i,k)
          nc(i,k) = max(nc(i,k),nsmall)
          N_nuc = nc(i,k)

          if (nCat>1) then
             !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,     &
                                  iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*inv_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         !Z-tendency for triple-moment ice
         !  note:  this could be optimized by moving this conditional block outside of loop k_loop_fz
         !         (would need to save values of iice_dest -- ditto for homo freezing of rain)
          if (log_3momentIce .and. N_nuc.ge.nsmall) then
             tmp1 = Q_nuc*6./(900.*pi)  !estimate of moment_3 tendency
             call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k), &
                                 lammin,lammax,cdist(i,k),cdist1(i,k),iSCF(k))
             mu_i_new = mu_c(i,k)
             zitot(i,k,iice_dest) = zitot(i,k,iice_dest) + G_of_mu(mu_i_new)*tmp1**2/N_nuc
          endif ! log_3momentice
         ! update theta. Note temperature is NOT updated here, but currently not used after
          th(i,k) = th(i,k) + invexn(i,k)*Q_nuc*xlf(i,k)*inv_cp
          qc(i,k) = 0.  != qc(i,k) - Q_nuc
          nc(i,k) = 0.  != nc(i,k) - N_nuc

       endif qc_not_small_2

       qr_not_small_2: if (qr(i,k).ge.qsmall .and. t(i,k).lt.233.15) then

          Q_nuc = qr(i,k)
          nr(i,k) = max(nr(i,k),nsmall)
          N_nuc = nr(i,k)
          if (nCat>1) then
             !determine destination ice-phase category:
             dum1  = 900.     !density of new ice
             D_new = ((Q_nuc*6.)/(pi*dum1*N_nuc))**thrd
             call icecat_destination(qitot(i,k,:)*iSCF(k),diam_ice(i,k,:),D_new,deltaD_init,iice_dest)
             if (global_status /= STATUS_OK) return
          else
             iice_dest = 1
          endif

          qirim(i,k,iice_dest) = qirim(i,k,iice_dest) + Q_nuc
          qitot(i,k,iice_dest) = qitot(i,k,iice_dest) + Q_nuc
          birim(i,k,iice_dest) = birim(i,k,iice_dest) + Q_nuc*inv_rho_rimeMax
          nitot(i,k,iice_dest) = nitot(i,k,iice_dest) + N_nuc
         ! z tendency for triple moment ice
          if (log_3momentIce .and. N_nuc.ge.qsmall) then
             tmp1 = Q_nuc*6./(900.*pi)  !estimate of moment_3 tendency
             mu_i_new = mu_r(i,k)
             zitot(i,k,iice_dest) = zitot(i,k,iice_dest) + G_of_mu(mu_i_new)*tmp1**2/N_nuc
          endif ! log_3momentice
         ! update theta. Note temperature is NOT updated here, but currently not used after
          th(i,k) = th(i,k) + invexn(i,k)*Q_nuc*xlf(i,k)*inv_cp
          qr(i,k) = 0.  ! = qr(i,k) - Q_nuc
          nr(i,k) = 0.  ! = nr(i,k) - N_nuc

       endif qr_not_small_2

    enddo k_loop_fz

!..............................................
! Merge ice categories with similar properties (based on specified similarly condition)

    multicat:  if (nCat.gt.1) then
!   multicat:  if (.FALSE.) then       ! for testing

      !step 1:  adjustments and calculation of mean diameters
       k_loop_check_before_merge:  do k = kbot,ktop,kdir
          iice_loop_check_before_merge:  do iice = 1,nCat
             qi_not_small_merge: if (qitot(i,k,iice).ge.qsmall) then
            
                nitot(i,k,iice) = max(nitot(i,k,iice),nsmall) !impose limit to prevent taking log of # < 0
                call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

                if (.not. log_3momentIce) then
             
                   call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,   &
                          rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),   &
                          rhop)
                   call access_lookup_table(dumjj,dumii,dumi,11,dum1,dum4,dum5,f1pr15)

                else ! triple moment ice

                   call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,   &
                          rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),   &
                          rhop)               
                   zitot(i,k,iice) = max(zitot(i,k,iice),zsmall)  !impose limit to prevent taking log of # < 0
                   dum1z =  6./(200.*pi)*qitot(i,k,iice)  !estimate of moment3, as starting point use 200 kg m-3 estimate of bulk density
                   do imu=1,niter_mui
                      mu_i = compute_mu_3moment(nitot(i,k,iice),dum1z,zitot(i,k,iice),mu_i_max)
                      call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),mu_i)
                      call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) ! find actual bulk density
                      dum1z =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                   enddo
                   call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,11,dum1,dum4,dum5,dum6,f1pr15)

                endif

               !adjust Zitot to make sure mu is in bounds
                if (log_3momentIce) then
                   dum1 =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                   tmp1 = G_of_mu(0.)
                   tmp2 = G_of_mu(20.)
                   zitot(i,k,iice) = min(zitot(i,k,iice),tmp1*dum1**2/nitot(i,k,iice))
                   zitot(i,k,iice) = max(zitot(i,k,iice),tmp2*dum1**2/nitot(i,k,iice))
                endif

                diag_di(i,k,iice)   = f1pr15

             else

                qv(i,k) = qv(i,k) + qitot(i,k,iice)
                th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
                if (log_3momentIce) zitot(i,k,iice) = 0
                diag_di(i,k,iice) = 0. 

             endif qi_not_small_merge
          enddo iice_loop_check_before_merge
       enddo k_loop_check_before_merge

      !step 2:  merge ice with similar properties into one category
       do k = kbot,ktop,kdir
          do iice = nCat,2,-1
             tmp1 = abs(diag_di(i,k,iice)-diag_di(i,k,iice-1))
             if (tmp1.le.deltaD_init .and. qitot(i,k,iice)>0. .and. qitot(i,k,iice-1)>0.) then
                qitot(i,k,iice-1) = qitot(i,k,iice-1) + qitot(i,k,iice)
                nitot(i,k,iice-1) = nitot(i,k,iice-1) + nitot(i,k,iice)
                qirim(i,k,iice-1) = qirim(i,k,iice-1) + qirim(i,k,iice)
                birim(i,k,iice-1) = birim(i,k,iice-1) + birim(i,k,iice)
                qitot(i,k,iice) = 0.
                nitot(i,k,iice) = 0.
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
                if (log_3momentIce) then
                   zitot(i,k,iice-1) = zitot(i,k,iice-1) + zitot(i,k,iice)
                   zitot(i,k,iice) = 0.
                endif
             endif
          enddo !iice loop
       enddo !k loop

    endif multicat
    
!...................................................
! note: This debug check is commented since small negative qx,nx values are possible here
!       (but get adjusted below).  If uncommented, caution in interpreting results.
!
!   if (debug_on) then
!      location_ind = 700
!      force_abort  = debug_ABORT
!      if (log_3momentIce) then
!         call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
!                Zitot=zitot(i,:,:))
!      else
!         call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
!                qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
!      endif
!      if (global_status /= STATUS_OK) return
!   endif

!...................................................
! Final checks to ensure consistency of mass/number
! and compute diagnostic fields for output


    k_loop_final_diagnostics:  do k = kbot,ktop,kdir

    ! cloud:
       if (qc(i,k)*iSCF(k).ge.qsmall) then
          call get_cloud_dsd2(qc(i,k),nc(i,k),mu_c(i,k),rho(i,k),nu(i,k),dnu,lamc(i,k),  &
                             lammin,lammax,tmp1,tmp2, iSCF(k))
          diag_effc(i,k) = 0.5*(mu_c(i,k)+3.)/lamc(i,k)
       else
          qv(i,k) = qv(i,k)+qc(i,k)
          th(i,k) = th(i,k)-invexn(i,k)*qc(i,k)*xxlv(i,k)*inv_cp
          qc(i,k) = 0.
          nc(i,k) = 0.
       endif

    ! rain:
       if (qr(i,k).ge.qsmall) then

          call get_rain_dsd2(qr(i,k),nr(i,k),mu_r(i,k),lamr(i,k),tmp1,tmp2,1.)

         ! hm, turn off soft lambda limiter
         ! impose size limits for rain with 'soft' lambda limiter
         ! (adjusts over a set timescale rather than within one timestep)
         ! dum2 = (qr(i,k)/(pi*rhow*nr(i,k)))**thrd
         ! if (dum2.gt.dbrk) then
         !    dum   = qr(i,k)*cons4
         !   !dum1  = (dum-nr(i,k))/max(60.,dt)  !time scale for adjustment is 60 s
         !    dum1  = (dum-nr(i,k))*timeScaleFactor
         !     nr(i,k) = nr(i,k)+dum1*dt
         ! endif

         !diag_effr(i,k) = 0.5*(mu_r(i,k)+3.)/lamr(i,k)    (currently not used)
        ! ze_rain(i,k) = n0r(i,k)*720./lamr(i,k)**3/lamr(i,k)**3/lamr(i,k)
          ! non-exponential rain:
          ze_rain(i,k) = rho(i,k)*nr(i,k)*(mu_r(i,k)+6.)*(mu_r(i,k)+5.)*(mu_r(i,k)+4.)*   &
                        (mu_r(i,k)+3.)*(mu_r(i,k)+2.)*(mu_r(i,k)+1.)/lamr(i,k)**6
          ze_rain(i,k) = max(ze_rain(i,k),1.e-22)
       else
          qv(i,k) = qv(i,k)+qr(i,k)
          th(i,k) = th(i,k)-invexn(i,k)*qr(i,k)*xxlv(i,k)*inv_cp
          qr(i,k) = 0.
          nr(i,k) = 0.
       endif

    ! ice:

       call impose_max_total_Ni(nitot(i,k,:),max_total_Ni,inv_rho(i,k))

       iice_loop_final_diagnostics:  do iice = 1,nCat

          qi_not_small:  if (qitot(i,k,iice).ge.qsmall) then

            !impose lower limits to prevent taking log of # < 0
             nitot(i,k,iice) = max(nitot(i,k,iice),nsmall)
             nr(i,k)         = max(nr(i,k),nsmall)

             call calc_bulkRhoRime(qitot(i,k,iice),qirim(i,k,iice),birim(i,k,iice),rhop)

             if (.not. log_3momentIce) then

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,   &
                       rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),   &
                       rhop)

                call access_lookup_table(dumjj,dumii,dumi, 2,dum1,dum4,dum5,f1pr02)
                call access_lookup_table(dumjj,dumii,dumi, 6,dum1,dum4,dum5,f1pr06)
                call access_lookup_table(dumjj,dumii,dumi, 7,dum1,dum4,dum5,f1pr09)
                call access_lookup_table(dumjj,dumii,dumi, 8,dum1,dum4,dum5,f1pr10)
                call access_lookup_table(dumjj,dumii,dumi, 9,dum1,dum4,dum5,f1pr13)
                call access_lookup_table(dumjj,dumii,dumi,11,dum1,dum4,dum5,f1pr15)
                call access_lookup_table(dumjj,dumii,dumi,12,dum1,dum4,dum5,f1pr16)
                call access_lookup_table(dumjj,dumii,dumi,13,dum1,dum4,dum5,f1pr22)   ! lambda_i
                call access_lookup_table(dumjj,dumii,dumi,14,dum1,dum4,dum5,f1pr23)   ! mu_i

             else ! triple moment ice

                call find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,   &
                       rimsize,densize,qitot(i,k,iice),nitot(i,k,iice),qirim(i,k,iice),   &
                       rhop)

             ! get Znorm indices

             !impose lower limits to prevent taking log of # < 0
                zitot(i,k,iice) = max(zitot(i,k,iice),zsmall)

                dum1z =  6./(200.*pi)*qitot(i,k,iice)  !estimate of moment3, as starting point use 200 kg m-3 estimate of bulk density

                do imu=1,niter_mui
                   mu_i = compute_mu_3moment(nitot(i,k,iice),dum1z,zitot(i,k,iice),mu_i_max)
                   call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),mu_i)
                   call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) ! find actual bulk density
                   dum1z =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                enddo

                ! call find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot(i,k,iice),zitot(i,k,iice)) !HM moved to above

                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 2,dum1,dum4,dum5,dum6,f1pr02)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 6,dum1,dum4,dum5,dum6,f1pr06)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 7,dum1,dum4,dum5,dum6,f1pr09)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 8,dum1,dum4,dum5,dum6,f1pr10)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi, 9,dum1,dum4,dum5,dum6,f1pr13)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,11,dum1,dum4,dum5,dum6,f1pr15)
                ! call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,12,dum1,dum4,dum5,dum6,f1pr16) !HM moved to above
!                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,14,dum1,dum4,dum5,dum6,f1pr20)
!                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,15,dum1,dum4,dum5,dum6,f1pr21)
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,14,dum1,dum4,dum5,dum6,f1pr22)   ! lambda_i
                call access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,15,dum1,dum4,dum5,dum6,f1pr23)   ! mu_i

             endif


          ! impose mean ice size bounds (i.e. apply lambda limiters)
             nitot(i,k,iice) = min(nitot(i,k,iice),f1pr09*qitot(i,k,iice))
             nitot(i,k,iice) = max(nitot(i,k,iice),f1pr10*qitot(i,k,iice))

          ! adjust Zitot to make sure mu is in bounds
          ! note that the Zmax and Zmin are normalized and thus need to be multiplied by existing Q
             if (log_3momentIce) then
                dum1 =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
                tmp1 = G_of_mu(0.)
                tmp2 = G_of_mu(20.)
                zitot(i,k,iice) = min(zitot(i,k,iice),tmp1*dum1**2/nitot(i,k,iice))
                zitot(i,k,iice) = max(zitot(i,k,iice),tmp2*dum1**2/nitot(i,k,iice))
!                zitot(i,k,iice) = min(zitot(i,k,iice),f1pr20*qitot(i,k,iice))
!                zitot(i,k,iice) = max(zitot(i,k,iice),f1pr21*qitot(i,k,iice))
             endif

  !--this should already be done in s/r 'calc_bulkRhoRime'
             if (qirim(i,k,iice).lt.qsmall) then
                qirim(i,k,iice) = 0.
                birim(i,k,iice) = 0.
             endif
  !==

  ! note that reflectivity from lookup table is normalized, so we need to multiply by N
             diag_vmi(i,k,iice)  = f1pr02*rhofaci(i,k)
             diag_effi(i,k,iice) = f1pr06 ! units are in m
             diag_di(i,k,iice)   = f1pr15
             diag_rhoi(i,k,iice) = f1pr16
             if (present(diag_lami)) diag_lami(i,k,iice) = f1pr22
             if (present(diag_mui))  diag_mui(i,k,iice)  = f1pr23
             if (present(diag_dhmax)) then
                diag_dhmax(i,k,iice) = maxHailSize(rho(i,k),qitot(i,k,iice),             &
                           qirim(i,k,iice),nitot(i,k,iice),rhofaci(i,k),f1pr22,f1pr23)
             endif

          ! note factor of air density below is to convert from m^6/kg to m^6/m^3
             ze_ice(i,k) = ze_ice(i,k) + 0.1892*f1pr13*nitot(i,k,iice)*rho(i,k)   ! sum contribution from each ice category (note: 0.1892 = 0.176/0.93)
             ze_ice(i,k) = max(ze_ice(i,k),1.e-22)

          else

             qv(i,k) = qv(i,k) + qitot(i,k,iice)
             th(i,k) = th(i,k) - invexn(i,k)*qitot(i,k,iice)*xxls(i,k)*inv_cp
             qitot(i,k,iice) = 0.
             nitot(i,k,iice) = 0.
             qirim(i,k,iice) = 0.
             birim(i,k,iice) = 0.
             if (log_3momentIce) zitot(i,k,iice) = 0
             diag_di(i,k,iice) = 0.

          endif qi_not_small

       enddo iice_loop_final_diagnostics

     ! sum ze components and convert to dBZ
       diag_ze(i,k) = 10.*log10((ze_rain(i,k) + ze_ice(i,k))*1.e+18)

     ! if qr is very small then set Nr to 0 (needs to be done here after call
     ! to ice lookup table because a minimum Nr of nsmall will be set otherwise even if qr=0)
       if (qr(i,k).lt.qsmall) then
          nr(i,k) = 0.
       endif

    enddo k_loop_final_diagnostics

    if (debug_on) then
       location_ind = 800
       force_abort  = debug_ABORT
       if (log_3momentIce) then
          call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,   &
                 Zitot=zitot(i,:,:))
       else
          call check_values(qv(i,:),T(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
       endif
       if (global_status /= STATUS_OK) return
    endif

!.....................................................

333 continue

!......................................
! zero out zitot if there is no qitot for triple moment
    if (log_3momentIce) then
       where (qitot(i,:,:).lt.qsmall) zitot(i,:,:) = 0.
!        do iice = 1,nCat
!           do k = kbot,ktop,kdir
!    if (qitot(i,k,iice).ge.qsmall) then
!       dum1 =  6./(f1pr16*pi)*qitot(i,k,iice)  !estimate of moment3
!       mu_i = compute_mu_3moment(nitot(i,k,iice),dum1,zitot(i,k,iice),mu_i_max)
!       print*,'after sed',k,mu_i
!    endif
!           enddo
!        enddo
    endif
!.......................................

    if (log_predictSsat) then
   ! recalculate supersaturation from T and qv
       do k = kbot,ktop,kdir
          t(i,k) = th(i,k)*(1.e-5*pres(i,k))**(rd*inv_cp)
          dum    = qv_sat(t(i,k),pres(i,k),0)
          ssat(i,k) = qv(i,k)-dum
       enddo
    endif


    ! calculate 'binary' cloud fraction (0 or 1) (diagnostic  only; used in GEM radiation interface)
    if (SCPF_on) then
       SCF_out(i,:) = SCF(:)
    else
       do k = kbot,ktop,kdir
          SCF_out(i,k) = 0.
          if (qc(i,k).ge.qsmall .and. sup(i,k).gt.1.e-6) SCF_out(i,k) = 1.
          do iice = 1,nCat
             if (qitot(i,k,iice).ge.qsmall .and. diag_effi(i,k,iice).lt.100.e-6) SCF_out(i,k) = 1.
          enddo
       enddo
    endif

    if (debug_on) then
       location_ind = 900
       force_abort  = debug_ABORT
       tmparr1(i,:) = th(i,:)*(pres(i,:)*1.e-5)**(rd*inv_cp)
       if (log_3momentIce) then
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind,         &
                 Zitot=zitot(i,:,:))
       else
          call check_values(qv(i,:),tmparr1(i,:),qc(i,:),nc(i,:),qr(i,:),nr(i,:),qitot(i,:,:), &
                 qirim(i,:,:),nitot(i,:,:),birim(i,:,:),i,it,force_abort,location_ind)
       endif
       if (global_status /= STATUS_OK) return
    endif

   !..............................................
   !Diagnostics -- visibility:

    if (present(diag_vis)) then   !it is assumed that all diag_vis{x} will either be present or all not present

       diag_vis(i,:)  = 3.*maxVIS
       diag_vis1(i,:) = 3.*maxVIS
       diag_vis2(i,:) = 3.*maxVIS
       diag_vis3(i,:) = 3.*maxVIS

       do k = kbot,ktop,kdir
          !VIS1:  component through liquid cloud (fog); based on Gultepe and Milbrandt, 2007)
          tmp1 = qc(i,k)*rho(i,k)*1.e+3    !LWC [g m-3]
          tmp2 = nc(i,k)*rho(i,k)*1.e-6    !Nc  [cm-3]
          if (tmp1>0.005 .and. tmp2>1.) then
             diag_vis1(i,k)= max(minVIS,1000.*(1.13*(tmp1*tmp2)**(-0.51))) !based on FRAM [GM2007, eqn (4)
            !diag_vis1(i,k)= max(minVIS,min(maxVIS, (tmp1*tmp2)**(-0.65))) !based on RACE [GM2007, eqn (3)
          endif

      !VIS2: component through rain;  based on Gultepe and Milbrandt, 2008, Table 2 eqn (1)
       tmp1 = mflux_r(i,k)*inv_rhow*3.6e+6                                    !rain rate [mm h-1]
       if (tmp1>0.01) then
          diag_vis2(i,k)= max(minVIS,1000.*(-4.12*tmp1**0.176+9.01))   ![m]
       endif

      !VIS3: component through snow;  based on Gultepe and Milbrandt, 2008, Table 2 eqn (6)
       tmp1 = mflux_i(i,k)*inv_rhow*3.6e+6                                    !snow rate, liq-eq [mm h-1]
       if (tmp1>0.01) then
          diag_vis3(i,k)= max(minVIS,1000.*(1.10*tmp1**(-0.701)))      ![m]
       endif

          !VIS:  visibility due to reduction from all components 1, 2, and 3
          !      (based on sum of extinction coefficients and Koschmieders's Law)
          diag_vis(i,k) = min(maxVIS, 1./(1./diag_vis1(i,k) + 1./diag_vis2(i,k) + 1./diag_vis3(i,k)))
          diag_vis1(i,k)= min(maxVIS, diag_vis1(i,k))
          diag_vis2(i,k)= min(maxVIS, diag_vis2(i,k))
          diag_vis3(i,k)= min(maxVIS, diag_vis3(i,k))
       enddo !k-loop

    endif  !if present(diag_vis)

!.....................................................

 enddo i_loop_main

! Save final microphysics values of theta and qv as old values for next time step
!  note: This is not necessary for GEM, which already has these values available
!        from the beginning of the model time step (TT_moins and HU_moins) when
!        s/r 'p3_wrapper_gem' is called (from s/r 'condensation').
 if (trim(model) == 'WRF') then
    th_old = th
    qv_old = qv
 endif

!...........................................................................................
! Compute diagnostic hydrometeor types for output as 3D fields and
! for partitioning into corresponding surface precipitation rates.

 compute_type_diags: if (typeDiags_ON) then

    if (.not.(present(prt_drzl).and.present(prt_rain).and.present(prt_crys).and. &
              present(prt_snow).and.present(prt_grpl).and.present(prt_pell).and. &
              present(prt_hail).and.present(prt_sndp))) then
       print*,'***  ABORT IN P3_MAIN ***'
       print*,'*  typeDiags_ON = .true. but prt_drzl, etc. are not passed into P3_MAIN'
       print*,'*************************'
       global_status = STATUS_ERROR
       return
    endif

    prt_drzl(:) = 0.
    prt_rain(:) = 0.
    prt_crys(:) = 0.
    prt_snow(:) = 0.
    prt_grpl(:) = 0.
    prt_pell(:) = 0.
    prt_hail(:) = 0.
    prt_sndp(:) = 0.
    if (present(qi_type)) qi_type(:,:,:) = 0.

    if (freq3DtypeDiag>0. .and. mod(it*dt,freq3DtypeDiag*60.)==0.) then
      !diagnose hydrometeor types for full columns
       ktop_typeDiag = ktop
    else
      !diagnose hydrometeor types at bottom level only (for specific precip rates)
       ktop_typeDiag = kbot
    endif

    i_loop_typediag: do i = its,ite

      !-- rain vs. drizzle:
       k_loop_typdiag_1: do k = kbot,ktop_typeDiag,kdir

          Q_drizzle(i,k) = 0.
          Q_rain(i,k)    = 0.
          !note:  these can be broken down further (outside of microphysics) into
          !       liquid rain (drizzle) vs. freezing rain (drizzle) based on sfc temp.
          if (qr(i,k)>qsmall .and. nr(i,k)>nsmall) then
             tmp1 = (6.*qr(i,k)/(pi*rhow*nr(i,k)))**thrd   !mean-mass diameter
             if (tmp1 < thres_raindrop) then
                Q_drizzle(i,k) = qr(i,k)
             else
                Q_rain(i,k)    = qr(i,k)
             endif
          endif

       enddo k_loop_typdiag_1

       if (Q_drizzle(i,kbot) > 0.) then
          prt_drzl(i) = prt_liq(i)
       elseif (Q_rain(i,kbot) > 0.) then
          prt_rain(i) = prt_liq(i)
       endif

      !-- ice-phase:
      iice_loop_diag: do iice = 1,nCat

          k_loop_typdiag_2: do k = kbot,ktop_typeDiag,kdir

             Q_crystals(i,k,iice) = 0.
             Q_ursnow(i,k,iice)   = 0.
             Q_lrsnow(i,k,iice)   = 0.
             Q_grpl(i,k,iice)     = 0.
             Q_pellets(i,k,iice)  = 0.
             Q_hail(i,k,iice)     = 0.

            !Note: The following partitioning of ice into types is subjective.  However,
            !      this is a diagnostic only; it does not affect the model solution.

             if (qitot(i,k,iice)>qsmall) then
                tmp1 = qirim(i,k,iice)/qitot(i,k,iice)   !rime mass fraction
                if (tmp1<0.1) then
                !zero or trace rime:
                   if (diag_di(i,k,iice)<150.e-6) then
                      Q_crystals(i,k,iice) = qitot(i,k,iice)
                   else
                      Q_ursnow(i,k,iice) = qitot(i,k,iice)
                   endif
                elseif (tmp1>=0.1 .and. tmp1<0.6) then
                !lightly rimed:
                   Q_lrsnow(i,k,iice) = qitot(i,k,iice)
                elseif (tmp1>=0.6 .and. tmp1<=1.) then
                !moderate-to-heavily rimed:
                   if (diag_rhoi(i,k,iice)<700.) then
                      Q_grpl(i,k,iice) = qitot(i,k,iice)
                   else
                      if (diag_di(i,k,iice)<1.e-3) then
                         Q_pellets(i,k,iice) = qitot(i,k,iice)
                      else
                         Q_hail(i,k,iice) = qitot(i,k,iice)
                      endif
                   endif
                else
                   print*, 'STOP -- unrealistic rime fraction: ',tmp1
                   global_status = STATUS_ERROR
                   return
                endif
             endif !qitot>0

          enddo k_loop_typdiag_2

         !diagnostics for sfc precipitation rates: (liquid-equivalent volume flux, m s-1)
         !  note: these are summed for all ice categories
          if (Q_crystals(i,kbot,iice) > 0.)    then
             prt_crys(i) = prt_crys(i) + prt_sol(i)    !precip rate of small crystals
          elseif (Q_ursnow(i,kbot,iice) > 0.)  then
             prt_snow(i) = prt_snow(i) + prt_sol(i)    !precip rate of unrimed + lightly rimed snow
          elseif (Q_lrsnow(i,kbot,iice) > 0.)  then
             prt_snow(i) = prt_snow(i) + prt_sol(i)    !precip rate of unrimed + lightly rimed snow
          elseif (Q_grpl(i,kbot,iice) > 0.)    then
             prt_grpl(i) = prt_grpl(i) + prt_sol(i)    !precip rate of graupel
          elseif (Q_pellets(i,kbot,iice) > 0.) then
             prt_pell(i) = prt_pell(i) + prt_sol(i)    !precip rate of ice pellets
          elseif (Q_hail(i,kbot,iice) > 0.)    then
             prt_hail(i) = prt_hail(i) + prt_sol(i)    !precip rate of hail
          endif
         !--- optimized version above above IF block (does not work on all FORTRAN compilers)
!           tmp3 = -(Q_crystals(i,kbot,iice) > 0.)
!           tmp4 = -(Q_ursnow(i,kbot,iice)   > 0.)
!           tmp5 = -(Q_lrsnow(i,kbot,iice)   > 0.)
!           tmp6 = -(Q_grpl(i,kbot,iice)     > 0.)
!           tmp7 = -(Q_pellets(i,kbot,iice)  > 0.)
!           tmp8 = -(Q_hail(i,kbot,iice)     > 0.)
!           prt_crys(i) = prt_crys(i) + prt_sol(i)*tmp3                   !precip rate of small crystals
!           prt_snow(i) = prt_snow(i) + prt_sol(i)*tmp4 + prt_sol(i)*tmp5 !precip rate of unrimed + lightly rimed snow
!           prt_grpl(i) = prt_grpl(i) + prt_sol(i)*tmp6                   !precip rate of graupel
!           prt_pell(i) = prt_pell(i) + prt_sol(i)*tmp7                   !precip rate of ice pellets
!           prt_hail(i) = prt_hail(i) + prt_sol(i)*tmp8                   !precip rate of hail
         !===

          !precip rate of unmelted total "snow":
          !  For now, an instananeous solid-to-liquid ratio (tmp1) is assumed and is multiplied
          !  by the total liquid-equivalent precip rates of snow (small crystals + lightly-rime + ..)
          !  Later, this can be computed explicitly as the volume flux of unmelted ice.
         !tmp1 = 10.  !assumes 10:1 ratio
         !tmp1 = 1000./max(1., diag_rhoi(i,kbot,iice))
          tmp1 = 1000./max(1., 5.*diag_rhoi(i,kbot,iice))
          prt_sndp(i) = prt_sndp(i) + tmp1*(prt_crys(i) + prt_snow(i) + prt_grpl(i))

       enddo iice_loop_diag

    enddo i_loop_typediag

   !- for output of 3D fields of diagnostic ice-phase hydrometeor type
    if (ktop_typeDiag==ktop .and. present(qi_type)) then
      !diag_3d(:,:,1) = Q_drizzle(:,:)
      !diag_3d(:,:,2) = Q_rain(:,:)
       do ii = 1,nCat
          qi_type(:,:,1) = qi_type(:,:,1) + Q_crystals(:,:,ii)
          qi_type(:,:,2) = qi_type(:,:,2) + Q_ursnow(:,:,ii)
          qi_type(:,:,3) = qi_type(:,:,3) + Q_lrsnow(:,:,ii)
          qi_type(:,:,4) = qi_type(:,:,4) + Q_grpl(:,:,ii)
          qi_type(:,:,5) = qi_type(:,:,5) + Q_hail(:,:,ii)
          qi_type(:,:,6) = qi_type(:,:,6) + Q_pellets(:,:,ii)
       enddo
    endif

 endif compute_type_diags


!=== (end of section for diagnostic hydrometeor/precip types)


! end of main microphysics routine

!  call cpu_time(t_p3main_end)
!  t_p3main_accum = t_p3main_accum + (t_p3main_end-t_p3main_start)

!.....................................................................................

 return

 END SUBROUTINE p3_main

!==========================================================================================!

 SUBROUTINE access_lookup_table(dumjj,dumii,dumi,index,dum1,dum4,dum5,proc)

 implicit none

 real    :: dum1,dum4,dum5,proc,iproc1,gproc1,tmp1,tmp2
 integer :: dumjj,dumii,dumi,index

! get value at current density index

! first interpolate for current rimed fraction index

   iproc1 = itab(dumjj,dumii,dumi,index)+(dum1-real(dumi))*(itab(dumjj,dumii,       &
            dumi+1,index)-itab(dumjj,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab(dumjj,dumii+1,dumi,index)+(dum1-real(dumi))*(itab(dumjj,dumii+1,   &
          dumi+1,index)-itab(dumjj,dumii+1,dumi,index))

   tmp1   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get value at density index + 1

! first interpolate for current rimed fraction index

   iproc1 = itab(dumjj+1,dumii,dumi,index)+(dum1-real(dumi))*(itab(dumjj+1,dumii,   &
            dumi+1,index)-itab(dumjj+1,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab(dumjj+1,dumii+1,dumi,index)+(dum1-real(dumi))*(itab(dumjj+1,       &
            dumii+1,dumi+1,index)-itab(dumjj+1,dumii+1,dumi,index))

   tmp2   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get final process rate
   proc   = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

END SUBROUTINE access_lookup_table

!------------------------------------------------------------------------------------------!
SUBROUTINE access_lookup_table_coll(dumjj,dumii,dumj,dumi,index,dum1,dum3,          &
                                    dum4,dum5,proc)

 implicit none

 real    :: dum1,dum3,dum4,dum5,proc,dproc1,dproc2,iproc1,gproc1,tmp1,tmp2
 integer :: dumjj,dumii,dumj,dumi,index


! This subroutine interpolates lookup table values for rain/ice collection processes

! current density index

! current rime fraction index
  dproc1  = itabcoll(dumjj,dumii,dumi,dumj,index)+(dum1-real(dumi))*                &
             (itabcoll(dumjj,dumii,dumi+1,dumj,index)-itabcoll(dumjj,dumii,dumi,    &
             dumj,index))

   dproc2  = itabcoll(dumjj,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj,dumii,dumi+1,dumj+1,index)-itabcoll(dumjj,dumii,dumi,  &
             dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll(dumjj,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj,dumii+1,dumi+1,dumj,index)-itabcoll(dumjj,dumii+1,     &
                 dumi,dumj,index))

   dproc2  = itabcoll(dumjj,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj,dumii+1,dumi+1,dumj+1,index)-itabcoll(dumjj,dumii+1,   &
             dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp1    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! density index + 1

! current rime fraction index

   dproc1  = itabcoll(dumjj+1,dumii,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll(dumjj+1,dumii,dumi+1,dumj,index)-itabcoll(dumjj+1,dumii,     &
                 dumi,dumj,index))

   dproc2  = itabcoll(dumjj+1,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj+1,dumii,dumi+1,dumj+1,index)-itabcoll(dumjj+1,dumii,   &
             dumi,dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll(dumjj+1,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*           &
             (itabcoll(dumjj+1,dumii+1,dumi+1,dumj,index)-itabcoll(dumjj+1,dumii+1, &
             dumi,dumj,index))

   dproc2  = itabcoll(dumjj+1,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*         &
             (itabcoll(dumjj+1,dumii+1,dumi+1,dumj+1,index)-itabcoll(dumjj+1,       &
                 dumii+1,dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp2    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! interpolate over density to get final values
   proc    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

 END SUBROUTINE access_lookup_table_coll

!------------------------------------------------------------------------------------------!

 SUBROUTINE access_lookup_table_colli(dumjjc,dumiic,dumic,dumjj,dumii,dumi,index,   &
                                      dum1c,dum4c,dum5c,dum1,dum4,dum5,proc)

 implicit none

 real    :: dum1,dum4,dum5,dum1c,dum4c,dum5c,proc,iproc1,iproc2,       &
            gproc1,gproc2,rproc1,rproc2,tmp1,tmp2,dproc11,dproc12
 integer :: dumjj,dumii,dumi,index,dumjjc,dumiic,dumic


! This subroutine interpolates lookup table values for rain/ice collection processes

! current density index collectee category

! current rime fraction index for collectee category

! current density index collector category

! current rime fraction index for collector category

  if (index.eq.1) then

   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*    &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj)-                     &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)


! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))*&
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj)-                 &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj+1)-                   &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))*&
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj+1)-                 &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))*   &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj+1)-                    &
             itabcolli1(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.......................................................................................................
! collectee rime fraction + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*   &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj)-                    &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc1  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)

!............................................................................................................
! collectee density index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj+1)-                   &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.......................................................................................................
! collectee rime fraction + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli1(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli1(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc2  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)

!..........................................................................................
! final process rate interpolation over collectee density

   proc    = rproc1+(dum5c-real(dumjjc))*(rproc2-rproc1)

 else if (index.eq.2) then

   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))*    &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj)-                     &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii,dumjj+1)-                   &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.......................................................................................................
! collectee rime fraction + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj)-                   &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc1  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)

!............................................................................................................
! collectee density index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))*  &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj)-                   &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.......................................................................................................
! collectee rime fraction + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp1    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

! collector density index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii,dumjj+1))

   iproc1  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

! collector rime fraction index + 1

   dproc11 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi,dumii+1,dumjj+1))

   dproc12 = itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)+(dum1c-real(dumic))* &
             (itabcolli2(dumic+1,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1)-                  &
             itabcolli2(dumic,dumiic+1,dumjjc+1,dumi+1,dumii+1,dumjj+1))

   iproc2  = dproc11+(dum1-real(dumi))*(dproc12-dproc11)

   tmp2    = iproc1+(dum4-real(dumii))*(iproc2-iproc1)

   gproc2  = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

   rproc2  = gproc1+(dum4c-real(dumiic))*(gproc2-gproc1)

!..........................................................................................
! final process rate interpolation over collectee density

   proc    = rproc1+(dum5c-real(dumjjc))*(rproc2-rproc1)

 endif ! index =1 or 2

 END SUBROUTINE access_lookup_table_colli

!==========================================================================================!

 SUBROUTINE access_lookup_table_3mom(dumzz,dumjj,dumii,dumi,index,dum1,dum4,dum5,dum6,proc)

 implicit none

 real    :: dum1,dum4,dum5,dum6,proc,iproc1,gproc1,tmp1,tmp2,rproc1,rproc2
 integer :: dumzz,dumjj,dumii,dumi,index

! get value at current G index

! get value at current density index

! first interpolate for current rimed fraction index

   iproc1 = itab_3mom(dumzz,dumjj,dumii,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz,dumjj,dumii,       &
            dumi+1,index)-itab_3mom(dumzz,dumjj,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab_3mom(dumzz,dumjj,dumii+1,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz,dumjj,dumii+1,   &
          dumi+1,index)-itab_3mom(dumzz,dumjj,dumii+1,dumi,index))

   tmp1   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get value at density index + 1

! first interpolate for current rimed fraction index

   iproc1 = itab_3mom(dumzz,dumjj+1,dumii,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz,dumjj+1,dumii,   &
            dumi+1,index)-itab_3mom(dumzz,dumjj+1,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab_3mom(dumzz,dumjj+1,dumii+1,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz,dumjj+1,       &
            dumii+1,dumi+1,index)-itab_3mom(dumzz,dumjj+1,dumii+1,dumi,index))

   tmp2   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get process rate
   rproc1   = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.............................................................

! get value at G index + 1

! get value at current density index

! first interpolate for current rimed fraction index

   iproc1 = itab_3mom(dumzz+1,dumjj,dumii,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz+1,dumjj,dumii,       &
            dumi+1,index)-itab_3mom(dumzz+1,dumjj,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab_3mom(dumzz+1,dumjj,dumii+1,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz+1,dumjj,dumii+1,   &
          dumi+1,index)-itab_3mom(dumzz+1,dumjj,dumii+1,dumi,index))

   tmp1   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get value at density index + 1

! first interpolate for current rimed fraction index

   iproc1 = itab_3mom(dumzz+1,dumjj+1,dumii,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz+1,dumjj+1,dumii,   &
            dumi+1,index)-itab_3mom(dumzz+1,dumjj+1,dumii,dumi,index))

! linearly interpolate to get process rates for rimed fraction index + 1

   gproc1 = itab_3mom(dumzz+1,dumjj+1,dumii+1,dumi,index)+(dum1-real(dumi))*(itab_3mom(dumzz+1,dumjj+1,       &
            dumii+1,dumi+1,index)-itab_3mom(dumzz+1,dumjj+1,dumii+1,dumi,index))

   tmp2   = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! get process rate
   rproc2   = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

! get final process rate

   proc = rproc1+(dum6-real(dumzz))*(rproc2-rproc1)

END SUBROUTINE access_lookup_table_3mom

!------------------------------------------------------------------------------------------!
SUBROUTINE access_lookup_table_coll_3mom(dumzz,dumjj,dumii,dumj,dumi,index,dum1,dum3,          &
                                    dum4,dum5,dum6,proc)

 implicit none

 real    :: dum1,dum3,dum4,dum5,dum6,proc,dproc1,dproc2,iproc1,gproc1,tmp1,tmp2,rproc1,rproc2
 integer :: dumzz,dumjj,dumii,dumj,dumi,index


! This subroutine interpolates lookup table values for rain/ice collection processes

! current G index

! current density index

! current rime fraction index
  dproc1  = itabcoll_3mom(dumzz,dumjj,dumii,dumi,dumj,index)+(dum1-real(dumi))*                &
             (itabcoll_3mom(dumzz,dumjj,dumii,dumi+1,dumj,index)-itabcoll_3mom(dumzz,dumjj,dumii,dumi,    &
             dumj,index))

   dproc2  = itabcoll_3mom(dumzz,dumjj,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz,dumjj,dumii,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz,dumjj,dumii,dumi,  &
             dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll_3mom(dumzz,dumjj,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz,dumjj,dumii+1,dumi+1,dumj,index)-itabcoll_3mom(dumzz,dumjj,dumii+1,     &
                 dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz,dumjj,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz,dumjj,dumii+1,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz,dumjj,dumii+1,   &
             dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp1    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! density index + 1

! current rime fraction index

   dproc1  = itabcoll_3mom(dumzz,dumjj+1,dumii,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz,dumjj+1,dumii,dumi+1,dumj,index)-itabcoll_3mom(dumzz,dumjj+1,dumii,     &
                 dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz,dumjj+1,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz,dumjj+1,dumii,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz,dumjj+1,dumii,   &
             dumi,dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll_3mom(dumzz,dumjj+1,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz,dumjj+1,dumii+1,dumi+1,dumj,index)-itabcoll_3mom(dumzz,dumjj+1,dumii+1, &
             dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz,dumjj+1,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*         &
             (itabcoll_3mom(dumzz,dumjj+1,dumii+1,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz,dumjj+1,       &
                 dumii+1,dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp2    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! interpolate over density
   rproc1    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

!.....................................................................................
! G index + 1

! current density index

! current rime fraction index
  dproc1  = itabcoll_3mom(dumzz+1,dumjj,dumii,dumi,dumj,index)+(dum1-real(dumi))*                &
             (itabcoll_3mom(dumzz+1,dumjj,dumii,dumi+1,dumj,index)-itabcoll_3mom(dumzz+1,dumjj,dumii,dumi,    &
             dumj,index))

   dproc2  = itabcoll_3mom(dumzz+1,dumjj,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz+1,dumjj,dumii,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz+1,dumjj,dumii,dumi,  &
             dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll_3mom(dumzz+1,dumjj,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz+1,dumjj,dumii+1,dumi+1,dumj,index)-itabcoll_3mom(dumzz+1,dumjj,dumii+1,     &
                 dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz+1,dumjj,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz+1,dumjj,dumii+1,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz+1,dumjj,dumii+1,   &
             dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp1    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! density index + 1

! current rime fraction index

   dproc1  = itabcoll_3mom(dumzz+1,dumjj+1,dumii,dumi,dumj,index)+(dum1-real(dumi))*             &
             (itabcoll_3mom(dumzz+1,dumjj+1,dumii,dumi+1,dumj,index)-itabcoll_3mom(dumzz+1,dumjj+1,dumii,     &
                 dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz+1,dumjj+1,dumii,dumi,dumj+1,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz+1,dumjj+1,dumii,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz+1,dumjj+1,dumii,   &
             dumi,dumj+1,index))

   iproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)

! rime fraction index + 1

   dproc1  = itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumi,dumj,index)+(dum1-real(dumi))*           &
             (itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumi+1,dumj,index)-itabcoll_3mom(dumzz+1,dumjj+1,dumii+1, &
             dumi,dumj,index))

   dproc2  = itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumi,dumj+1,index)+(dum1-real(dumi))*         &
             (itabcoll_3mom(dumzz+1,dumjj+1,dumii+1,dumi+1,dumj+1,index)-itabcoll_3mom(dumzz+1,dumjj+1,       &
                 dumii+1,dumi,dumj+1,index))

   gproc1  = dproc1+(dum3-real(dumj))*(dproc2-dproc1)
   tmp2    = iproc1+(dum4-real(dumii))*(gproc1-iproc1)

! interpolate over density
   rproc2    = tmp1+(dum5-real(dumjj))*(tmp2-tmp1)

! get final process rate by interpolation over G
   proc    = rproc1+(dum6-real(dumzz))*(rproc2-rproc1)

 END SUBROUTINE access_lookup_table_coll_3mom

!==========================================================================================!

 real function polysvp1(T,i_type)

!-------------------------------------------
!  COMPUTE SATURATION VAPOR PRESSURE
!  POLYSVP1 RETURNED IN UNITS OF PA.
!  T IS INPUT IN UNITS OF K.
!  i_type REFERS TO SATURATION WITH RESPECT TO LIQUID (0) OR ICE (1)
!-------------------------------------------

      implicit none

      real    :: T
      integer :: i_type

! REPLACE GOFF-GRATCH WITH FASTER FORMULATION FROM FLATAU ET AL. 1992, TABLE 4 (RIGHT-HAND COLUMN)

! ice
      real a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i
      data a0i,a1i,a2i,a3i,a4i,a5i,a6i,a7i,a8i /&
        6.11147274, 0.503160820, 0.188439774e-1, &
        0.420895665e-3, 0.615021634e-5,0.602588177e-7, &
        0.385852041e-9, 0.146898966e-11, 0.252751365e-14/

! liquid
      real a0,a1,a2,a3,a4,a5,a6,a7,a8

! V1.7
      data a0,a1,a2,a3,a4,a5,a6,a7,a8 /&
        6.11239921, 0.443987641, 0.142986287e-1, &
        0.264847430e-3, 0.302950461e-5, 0.206739458e-7, &
        0.640689451e-10,-0.952447341e-13,-0.976195544e-15/
      real dt

!-------------------------------------------

      if (i_type.EQ.1 .and. T.lt.273.15) then
! ICE

! use Goff-Gratch for T < 195.8 K and Flatau et al. equal or above 195.8 K
         if (t.ge.195.8) then
            dt=t-273.15
            polysvp1 = a0i + dt*(a1i+dt*(a2i+dt*(a3i+dt*(a4i+dt*(a5i+dt*(a6i+dt*(a7i+a8i*dt)))))))
            polysvp1 = polysvp1*100.
         else
            polysvp1 = 10.**(-9.09718*(273.16/t-1.)-3.56654* &
                alog10(273.16/t)+0.876793*(1.-t/273.16)+ &
                alog10(6.1071))*100.
         end if

      elseif (i_type.EQ.0 .or. T.ge.273.15) then
! LIQUID

! use Goff-Gratch for T < 202.0 K and Flatau et al. equal or above 202.0 K
         if (t.ge.202.0) then
            dt = t-273.15
            polysvp1 = a0 + dt*(a1+dt*(a2+dt*(a3+dt*(a4+dt*(a5+dt*(a6+dt*(a7+a8*dt)))))))
            polysvp1 = polysvp1*100.
         else
! note: uncertain below -70 C, but produces physical values (non-negative) unlike flatau
            polysvp1 = 10.**(-7.90298*(373.16/t-1.)+ &
                5.02808*alog10(373.16/t)- &
                1.3816e-7*(10**(11.344*(1.-t/373.16))-1.)+ &
                8.1328e-3*(10**(-3.49149*(373.16/t-1.))-1.)+ &
                alog10(1013.246))*100.
         end if

         endif


 end function polysvp1

!------------------------------------------------------------------------------------------!

 real function DERF(X)

 implicit none

 real :: X
 real, dimension(0 : 64) :: A, B
 real :: W,T,Y
 integer :: K,I
      data A/                                                 &
         0.00000000005958930743E0, -0.00000000113739022964E0, &
         0.00000001466005199839E0, -0.00000016350354461960E0, &
         0.00000164610044809620E0, -0.00001492559551950604E0, &
         0.00012055331122299265E0, -0.00085483269811296660E0, &
         0.00522397762482322257E0, -0.02686617064507733420E0, &
         0.11283791670954881569E0, -0.37612638903183748117E0, &
         1.12837916709551257377E0,                            &
         0.00000000002372510631E0, -0.00000000045493253732E0, &
         0.00000000590362766598E0, -0.00000006642090827576E0, &
         0.00000067595634268133E0, -0.00000621188515924000E0, &
         0.00005103883009709690E0, -0.00037015410692956173E0, &
         0.00233307631218880978E0, -0.01254988477182192210E0, &
         0.05657061146827041994E0, -0.21379664776456006580E0, &
         0.84270079294971486929E0,                            &
         0.00000000000949905026E0, -0.00000000018310229805E0, &
         0.00000000239463074000E0, -0.00000002721444369609E0, &
         0.00000028045522331686E0, -0.00000261830022482897E0, &
         0.00002195455056768781E0, -0.00016358986921372656E0, &
         0.00107052153564110318E0, -0.00608284718113590151E0, &
         0.02986978465246258244E0, -0.13055593046562267625E0, &
         0.67493323603965504676E0,                            &
         0.00000000000382722073E0, -0.00000000007421598602E0, &
         0.00000000097930574080E0, -0.00000001126008898854E0, &
         0.00000011775134830784E0, -0.00000111992758382650E0, &
         0.00000962023443095201E0, -0.00007404402135070773E0, &
         0.00050689993654144881E0, -0.00307553051439272889E0, &
         0.01668977892553165586E0, -0.08548534594781312114E0, &
         0.56909076642393639985E0,                            &
         0.00000000000155296588E0, -0.00000000003032205868E0, &
         0.00000000040424830707E0, -0.00000000471135111493E0, &
         0.00000005011915876293E0, -0.00000048722516178974E0, &
         0.00000430683284629395E0, -0.00003445026145385764E0, &
         0.00024879276133931664E0, -0.00162940941748079288E0, &
         0.00988786373932350462E0, -0.05962426839442303805E0, &
         0.49766113250947636708E0 /
      data (B(I), I = 0, 12) /                                 &
         -0.00000000029734388465E0,  0.00000000269776334046E0, &
         -0.00000000640788827665E0, -0.00000001667820132100E0, &
         -0.00000021854388148686E0,  0.00000266246030457984E0, &
          0.00001612722157047886E0, -0.00025616361025506629E0, &
          0.00015380842432375365E0,  0.00815533022524927908E0, &
         -0.01402283663896319337E0, -0.19746892495383021487E0, &
          0.71511720328842845913E0 /
      data (B(I), I = 13, 25) /                                &
         -0.00000000001951073787E0, -0.00000000032302692214E0, &
          0.00000000522461866919E0,  0.00000000342940918551E0, &
         -0.00000035772874310272E0,  0.00000019999935792654E0, &
          0.00002687044575042908E0, -0.00011843240273775776E0, &
         -0.00080991728956032271E0,  0.00661062970502241174E0, &
          0.00909530922354827295E0, -0.20160072778491013140E0, &
          0.51169696718727644908E0 /
      data (B(I), I = 26, 38) /                                &
         0.00000000003147682272E0, -0.00000000048465972408E0,  &
         0.00000000063675740242E0,  0.00000003377623323271E0,  &
        -0.00000015451139637086E0, -0.00000203340624738438E0,  &
         0.00001947204525295057E0,  0.00002854147231653228E0,  &
        -0.00101565063152200272E0,  0.00271187003520095655E0,  &
         0.02328095035422810727E0, -0.16725021123116877197E0,  &
         0.32490054966649436974E0 /
      data (B(I), I = 39, 51) /                                &
         0.00000000002319363370E0, -0.00000000006303206648E0,  &
        -0.00000000264888267434E0,  0.00000002050708040581E0,  &
         0.00000011371857327578E0, -0.00000211211337219663E0,  &
         0.00000368797328322935E0,  0.00009823686253424796E0,  &
        -0.00065860243990455368E0, -0.00075285814895230877E0,  &
         0.02585434424202960464E0, -0.11637092784486193258E0,  &
         0.18267336775296612024E0 /
      data (B(I), I = 52, 64) /                                &
        -0.00000000000367789363E0,  0.00000000020876046746E0,  &
        -0.00000000193319027226E0, -0.00000000435953392472E0,  &
         0.00000018006992266137E0, -0.00000078441223763969E0,  &
        -0.00000675407647949153E0,  0.00008428418334440096E0,  &
        -0.00017604388937031815E0, -0.00239729611435071610E0,  &
         0.02064129023876022970E0, -0.06905562880005864105E0,  &
         0.09084526782065478489E0 /
      W = ABS(X)
      if (W .LT. 2.2D0) then
          T = W * W
          K = INT(T)
          T = T - K
          K = K * 13
          Y = ((((((((((((A(K) * T + A(K + 1)) * T +              &
              A(K + 2)) * T + A(K + 3)) * T + A(K + 4)) * T +     &
              A(K + 5)) * T + A(K + 6)) * T + A(K + 7)) * T +     &
              A(K + 8)) * T + A(K + 9)) * T + A(K + 10)) * T +    &
              A(K + 11)) * T + A(K + 12)) * W
      elseif (W .LT. 6.9D0) then
          K = INT(W)
          T = W - K
          K = 13 * (K - 2)
          Y = (((((((((((B(K) * T + B(K + 1)) * T +               &
              B(K + 2)) * T + B(K + 3)) * T + B(K + 4)) * T +     &
              B(K + 5)) * T + B(K + 6)) * T + B(K + 7)) * T +     &
              B(K + 8)) * T + B(K + 9)) * T + B(K + 10)) * T +    &
              B(K + 11)) * T + B(K + 12)
          Y = Y * Y
          Y = Y * Y
          Y = Y * Y
          Y = 1 - Y * Y
      else
          Y = 1
      endif
      if (X .LT. 0) Y = -Y
      DERF = Y

 end function DERF

!------------------------------------------------------------------------------------------!
 logical function isnan(arg1)
       real,intent(in) :: arg1
       isnan = (arg1 .ne. arg1)
       return
 end function isnan

!==========================================================================================!
 subroutine icecat_destination(Qi,Di,D_nuc,deltaD_init,iice_dest)

 !--------------------------------------------------------------------------------------!
 ! Returns the index of the destination ice category into which new ice is nucleated.
 !
 ! New ice will be nucleated into the category in which the existing ice is
 ! closest in size to the ice being nucleated.  The exception is that if the
 ! size difference between the nucleated ice and existing ice exceeds a threshold
 ! value for all categories, then ice is initiated into a new category.
 !
 ! D_nuc        = mean diameter of new particles being added to a category
 ! D(i)         = mean diameter of particles in category i
 ! diff(i)      = |D(i) - D_nuc|
 ! deltaD_init  = threshold size difference to consider a new (empty) category
 ! mindiff      = minimum of all diff(i) (for non-empty categories)
 !
 ! POSSIBLE CASES                      DESTINATION CATEGORY
 !---------------                      --------------------
 ! case 1:  all empty                  category 1
 ! case 2:  all full                   category with smallest diff
 ! case 3:  partly full
 !  case 3a:  mindiff <  diff_thrs     category with smallest diff
 !  case 3b:  mindiff >= diff_thrs     first empty category
 !--------------------------------------------------------------------------------------!

 implicit none

! arguments:
 real, intent(in), dimension(:) :: Qi,Di
 real, intent(in)               :: D_nuc,deltaD_init
 integer, intent(out)           :: iice_dest

! local variables:
 logical                        :: all_full,all_empty
 integer                        :: i_firstEmptyCategory,iice,i_mindiff,n_cat
 real                           :: mindiff,diff
 real, parameter                :: qsmall_loc = 1.e-14

 !--------------------------------------------------------------------------------------!

 n_cat     = size(Qi)
 iice_dest = -99

!-- test:
! iice_dest = 1
! return
!==

 if (sum(Qi(:))<qsmall_loc) then

 !case 1:
    iice_dest = 1
    return

 else

    all_full  = .true.
    all_empty = .false.
    mindiff   = 9.e+9
    i_firstEmptyCategory = 0

    do iice = 1,n_cat
       if (Qi(iice) .ge. qsmall_loc) then
          all_empty = .false.
          diff      = abs(Di(iice)-D_nuc)
          if (diff .lt. mindiff) then
             mindiff   = diff
             i_mindiff = iice
          endif
       else
          all_full = .false.
          if (i_firstEmptyCategory.eq.0) i_firstEmptyCategory = iice
       endif
    enddo

    if (all_full) then
 !case 2:
       iice_dest = i_mindiff
       return
    else
       if (mindiff .lt. deltaD_init) then
 !case 3a:
          iice_dest = i_mindiff
          return
       else
 !case 3b:
          iice_dest = i_firstEmptyCategory
          return
       endif
    endif

 endif

 print*, 'ERROR in s/r icecat_destination -- made it to end'
 global_status = STATUS_ERROR
 return

 end subroutine icecat_destination


!======================================================================================!

 subroutine find_lookupTable_indices_1a(dumi,dumjj,dumii,dum1,dum4,dum5,isize,rimsize,   &
                                        densize,qitot,nitot,qirim,rhop)

!------------------------------------------------------------------------------------------!
! Finds indices in 3D ice (only) lookup table.
!  - used for P3 v4
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumi,dumjj,dumii
 real,    intent(out) :: dum1,dum4,dum5
 integer, intent(in)  :: isize,rimsize,densize
 real,    intent(in)  :: qitot,nitot,qirim,rhop

!------------------------------------------------------------------------------------------!

           ! find index for qi (normalized ice mass mixing ratio = qitot/nitot)

           ! we are inverting this equation from the lookup table to solve for i_Qnorm:
           ! from create_LT1:  q = 800.**((i_Qnorm+10)*0.1)*1.e-18   [where q = qitot/nitot]
            !dum1 = (alog10(qitot/nitot)+18.)/(0.1*alog10(800.)) - 10.   !original
             dum1 = (alog10(qitot/nitot)+18.)*3.444606 - 10.             !optimized
             dumi = int(dum1)
             ! set limits (to make sure the calculated index doesn't exceed range of lookup table)
             dum1 = min(dum1,real(isize))
             dum1 = max(dum1,1.)
             dumi = max(1,dumi)
             dumi = min(isize-1,dumi)

           ! find index for rime mass fraction
             dum4  = (qirim/qitot)*3. + 1.
             dumii = int(dum4)
             ! set limits
             dum4  = min(dum4,real(rimsize))
             dum4  = max(dum4,1.)
             dumii = max(1,dumii)
             dumii = min(rimsize-1,dumii)

           ! find index for bulk rime density
           ! (account for uneven spacing in lookup table for density)
             if (rhop.le.650.) then
                dum5 = (rhop-50.)*0.005 + 1.
             else
                dum5 =(rhop-650.)*0.004 + 4.
             endif
             dumjj = int(dum5)
             ! set limits
             dum5  = min(dum5,real(densize))
             dum5  = max(dum5,1.)
             dumjj = max(1,dumjj)
             dumjj = min(densize-1,dumjj)

 end subroutine find_lookupTable_indices_1a

!======================================================================================!

 subroutine find_lookupTable_indices_1b(dumj,dum3,rcollsize,qr,nr)

 !------------------------------------------------------------------------------------------!
 ! Finds indices in 3D rain lookup table, for 2-moment and 3-moment ice
 !------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumj
 real,    intent(out) :: dum3
 integer, intent(in)  :: rcollsize
 real,    intent(in)  :: qr,nr

! local variables:
 real                 :: dumlr

!------------------------------------------------------------------------------------------!

           ! find index for scaled mean rain size
           ! if no rain, then just choose dumj = 1 and do not calculate rain-ice collection processes
             if (qr.ge.qsmall .and. nr.gt.0.) then
              ! calculate scaled mean size for consistency with ice lookup table
                dumlr = (qr/(pi*rhow*nr))**thrd
                dum3  = (alog10(1.*dumlr)+5.)*10.70415
                dumj  = int(dum3)
              ! set limits
                dum3  = min(dum3,real_rcollsize)
                dum3  = max(dum3,1.)
                dumj  = max(1,dumj)
                dumj  = min(rcollsize-1,dumj)
             else
                dumj  = 1
                dum3  = 1.
             endif

 end subroutine find_lookupTable_indices_1b

!======================================================================================!

 subroutine find_lookupTable_indices_1c(dumzz,dum6,zsize,qitot,mu_i)

 !------------------------------------------------------------------------------------------!
 ! Finds indices for G index in 3-moment ice lookup table
 !------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumzz
 integer, intent(in)  :: zsize
 real,    intent(out) :: dum6
 real,    intent(in)  :: qitot,mu_i

!------------------------------------------------------------------------------------------!

  ! find index for mu_i
  if (qitot.ge.qsmall) then
! we are inverting this equation from the lookup table to solve for i:
! use old formula for now, we are solving zitot/qitot=9^(i)*1.e-23, for beta <= 7 lookup table
!     dum6  = (alog10(zitot/qitot)+23.)/alog10(9.)
! use new formula for beta >= 9 lookup table
! zitot/qitot=2.1^(i)*1.e-23

!    dum6  = (alog10(zitot/qitot)+23.)/alog10(2.1)
!    dum6  = (alog10(zitot/qitot)+23.)*3.10347652     !optimization
! HM replace with mu_i
!    dum6 = mu_i/2.+1. ! invert lookup table indices
     dum6 = mu_i*0.5+1. ! optimized

! for "two-moment", setting a constant mu = 0
!     dum6  = 100.  ! set dum6 to a very large value, corresponding to mu = 0

     dumzz = int(dum6)
     dum6  = min(dum6,real(zsize))
     dum6  = max(dum6,1.)
     dumzz = max(1,dumzz)
     dumzz = min(zsize-1,dumzz)

  else

     dumzz = 1
     dum6  = 1.

  endif

 end subroutine find_lookupTable_indices_1c

!======================================================================================!
 subroutine find_lookupTable_indices_2(dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc,  &
                                       dum1,   dum4,    dum5,   dum1c, dum4c,  dum5c,   &
                                       iisize, rimsize, densize,                        &
                                       qitot_1, qitot_2, nitot_1, nitot_2,              &
                                       qirim_1, qirim_2, birim_1, birim_2)

!------------------------------------------------------------------------------------------!
! Finds indices in ice-ice interaction lookup table (2)
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumi,   dumii,   dumjj,  dumic, dumiic, dumjjc
 real,    intent(out) :: dum1,   dum4,    dum5,   dum1c, dum4c,  dum5c
 integer, intent(in)  :: iisize, rimsize, densize
 real,    intent(in)  :: qitot_1,qitot_2,nitot_1,nitot_2,qirim_1,qirim_2,birim_1,birim_2

! local variables:
 real                 :: drhop

!------------------------------------------------------------------------------------------!

                    ! find index in lookup table for collector category

                    ! find index for qi (total ice mass mixing ratio)

!              !-- For LT2-5.0 (Dm_max = 2000.)
!              !   inverting the following (from create_LT2):  q = 261.7**((i+5)*0.2)*1.e-18
!              !   where q = qitot/nitot (normalized)
!                      !dum1 = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(261.7))-5.   !orig
!                       dum1 = (alog10(qitot_1/nitot_1)+18.)*(2.06799)-5.             !optimization

             !-- For LT2-5.1 (Dm_max = 400000.)
             !   inverting this equation from the lookup table to solve for i_Qnorm:
             !   from create_LT2:  q = 800.**(0.2*(i_Qnorm+5))*1.e-18   [where q = qitot/nitot]
                     !dum1 = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(800.)) - 5.   !original
                      dum1 = (alog10(qitot_1/nitot_1)+18.)*1.722303 - 5.             !optimized

                      dumi = int(dum1)
                      dum1 = min(dum1,real(iisize))
                      dum1 = max(dum1,1.)
                      dumi = max(1,dumi)
                      dumi = min(iisize-1,dumi)

   ! note that the code below for finding rime mass fraction and density index is
   ! redundant with code for main ice lookup table and can probably be omitted
   ! for efficiency; for now it is left in

                    ! find index for rime mass fraction
                      dum4  = qirim_1/qitot_1*3. + 1.
                      dumii = int(dum4)
                      dum4  = min(dum4,real(rimsize))
                      dum4  = max(dum4,1.)
                      dumii = max(1,dumii)
                      dumii = min(rimsize-1,dumii)


                    ! find index for bulk rime density
                    ! (account for uneven spacing in lookup table for density)
                    ! bulk rime density
                      if (birim_1.ge.bsmall) then
                         drhop = qirim_1/birim_1
                      else
                         drhop = 0.
                      endif

                      if (drhop.le.650.) then
                         dum5 = (drhop-50.)*0.005 + 1.
                      else
                         dum5 =(drhop-650.)*0.004 + 4.
                      endif
                      dumjj = int(dum5)
                      dum5  = min(dum5,real(densize))
                      dum5  = max(dum5,1.)
                      dumjj = max(1,dumjj)
                      dumjj = min(densize-1,dumjj)

                    ! find index in lookup table for collectee category, here 'q' is a scaled q/N
                    ! find index for qi (total ice mass mixing ratio)
!                      !dum1c = (alog10(qitot_2/nitot_2)+18.)/(0.2*alog10(261.7))-5. !orig
!                       dum1c = (alog10(qitot_2/nitot_2)+18.)/(0.483561)-5. !for computational efficiency

             !-- For LT2-5.1 (Dm_max = 400000.)
             !   inverting this equation from the lookup table to solve for i_Qnorm:
             !   from create_LT2:  q = 800.**(0.2*(i_Qnorm+5))*1.e-18   [where q = qitot/nitot]
                     !dum1c = (alog10(qitot_1/nitot_1)+18.)/(0.2*alog10(800.)) - 5.   !original
                      dum1c = (alog10(qitot_2/nitot_2)+18.)*1.722303 - 5.             !optimized
                      dumic = int(dum1c)
                      dum1c = min(dum1c,real(iisize))
                      dum1c = max(dum1c,1.)
                      dumic = max(1,dumic)
                      dumic = min(iisize-1,dumic)


                    ! find index for rime mass fraction
                      dum4c  = qirim_2/qitot_2*3. + 1.
                      dumiic = int(dum4c)
                      dum4c  = min(dum4c,real(rimsize))
                      dum4c  = max(dum4c,1.)
                      dumiic = max(1,dumiic)
                      dumiic = min(rimsize-1,dumiic)
                    ! calculate predicted bulk rime density
                      if (birim_2.ge.1.e-15) then            !*** NOTE:  change to 'bsmall'
                         drhop = qirim_2/birim_2
                      else
                         drhop = 0.
                      endif

                    ! find index for bulk rime density
                    ! (account for uneven spacing in lookup table for density)
                      if (drhop.le.650.) then
                         dum5c = (drhop-50.)*0.005 + 1.
                      else
                         dum5c =(drhop-650.)*0.004 + 4.
                      endif
                      dumjjc = int(dum5c)
                      dum5c  = min(dum5c,real(densize))
                      dum5c  = max(dum5c,1.)
                      dumjjc = max(1,dumjjc)
                      dumjjc = min(densize-1,dumjjc)

 end subroutine find_lookupTable_indices_2


!======================================================================================!
 subroutine find_lookupTable_indices_3(dumii,dumjj,dum1,rdumii,rdumjj,inv_dum3,mu_r,lamr)

!------------------------------------------------------------------------------------------!
! Finds indices in rain lookup table (3)
!------------------------------------------------------------------------------------------!

 implicit none

! arguments:
 integer, intent(out) :: dumii,dumjj
 real,    intent(out) :: dum1,rdumii,rdumjj,inv_dum3
 real,    intent(in)  :: mu_r,lamr

!------------------------------------------------------------------------------------------!

        ! find location in scaled mean size space
          dum1 = (mu_r+1.)/lamr
          if (dum1.le.195.e-6) then
             inv_dum3  = 0.1
             rdumii = (dum1*1.e6+5.)*inv_dum3
             rdumii = max(rdumii, 1.)
             rdumii = min(rdumii,20.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 1)
             dumii  = min(dumii,20)
          elseif (dum1.gt.195.e-6) then
             inv_dum3  = thrd*0.1            !i.e. 1/30
             rdumii = (dum1*1.e+6-195.)*inv_dum3 + 20.
             rdumii = max(rdumii, 20.)
             rdumii = min(rdumii,300.)
             dumii  = int(rdumii)
             dumii  = max(dumii, 20)
             dumii  = min(dumii,299)
          endif

        ! find location in mu_r space
          rdumjj = mu_r+1.
          rdumjj = max(rdumjj,1.)
          rdumjj = min(rdumjj,10.)
          dumjj  = int(rdumjj)
          dumjj  = max(dumjj,1)
          dumjj  = min(dumjj,9)

 end subroutine find_lookupTable_indices_3

!===========================================================================================
 subroutine get_cloud_dsd2(qc_grd,nc_grd,mu_c,rho,nu,dnu,lamc,lammin,lammax,cdist,cdist1,iSCF)

 implicit none

!arguments:
 real, dimension(:), intent(in)  :: dnu
 real,     intent(in)            :: rho
 real,     intent(in)            :: qc_grd
 real,     intent(inout)         :: nc_grd    !grid-mean value
 real,     intent(out)           :: mu_c,nu,lamc,cdist,cdist1
 real,     intent(in)            :: iSCF

!local variables
 real                            :: lammin,lammax,qc,nc
 integer                         :: dumi

!--------------------------------------------------------------------------

       qc = qc_grd*iSCF   !in-cloud value

       if (qc.ge.qsmall) then

          nc = nc_grd*iSCF   !in-cloud value

        ! set minimum nc to prevent floating point error
          nc   = max(nc,nsmall)
          mu_c = 0.0005714*(nc*1.e-6*rho)+0.2714
          mu_c = 1./(mu_c**2)-1.
          mu_c = max(mu_c,2.)
          mu_c = min(mu_c,15.)

        ! interpolate for mass distribution spectral shape parameter (for SB warm processes)
          if (iparam.eq.1) then
             dumi = int(mu_c)
             nu   = dnu(dumi)+(dnu(dumi+1)-dnu(dumi))*(mu_c-dumi)
          endif

        ! calculate lamc
          lamc = (cons1*nc*(mu_c+3.)*(mu_c+2.)*(mu_c+1.)/qc)**thrd

        ! apply lambda limiters
          lammin = (mu_c+1.)*2.5e+4   ! min: 40 micron mean diameter
          lammax = (mu_c+1.)*1.e+6    ! max:  1 micron mean diameter

          if (lamc.lt.lammin) then
             lamc = lammin
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          elseif (lamc.gt.lammax) then
             lamc = lammax
             nc   = 6.*lamc**3*qc/(pi*rhow*(mu_c+3.)*(mu_c+2.)*(mu_c+1.))
          endif

          cdist  = nc*(mu_c+1.)/lamc
          cdist1 = nc/gamma(mu_c+1.)
          nc_grd = nc/iSCF   !compute modified grid-mean value

       else

          mu_c   = 0.
          lamc   = 0.
          cdist  = 0.
          cdist1 = 0.
          nu     = 0.

       endif

 end subroutine get_cloud_dsd2


!===========================================================================================
 subroutine get_rain_dsd2(qr_grd,nr_grd,mu_r,lamr,cdistr,logn0r,iSPF)

! Computes and returns rain size distribution parameters

 implicit none

!arguments:
 real, intent(in)    :: qr_grd       !grid-mean
 real, intent(inout) :: nr_grd       !grid-mean
 real, intent(out)   :: lamr,mu_r,cdistr,logn0r
 real, intent(in)    :: iSPF

!local variables:
 real                :: inv_dum,lammax,lammin,qr,nr

!--------------------------------------------------------------------------

       qr = qr_grd*iSPF   !in-cloud value

       if (qr.ge.qsmall) then

          nr = nr_grd*iSPF   !in-cloud value

       ! use lookup table to get mu
       ! mu-lambda relationship is from Cao et al. (2008), eq. (7)

       ! find spot in lookup table
       ! (scaled N/q for lookup table parameter space_
          nr      = max(nr,nsmall)
          inv_dum = (qr/(cons1*nr*6.))**thrd

        ! apply constant mu_r:
          mu_r = mu_r_constant

!--- apply diagnostic (variable) mu_r:
!          if (inv_dum.lt.282.e-6) then
!             mu_r = 8.282
!          elseif (inv_dum.ge.282.e-6 .and. inv_dum.lt.502.e-6) then
!           ! interpolate
!             rdumii = (inv_dum-250.e-6)*1.e+6*0.5
!             rdumii = max(rdumii,1.)
!             rdumii = min(rdumii,150.)
!             dumii  = int(rdumii)
!             dumii  = min(149,dumii)
!             mu_r   = mu_r_table(dumii)+(mu_r_table(dumii+1)-mu_r_table(dumii))*(rdumii-  &
!                        real(dumii))
!          elseif (inv_dum.ge.502.e-6) then
!             mu_r = 0.
!          endif
!===
          lamr   = (cons1*nr*(mu_r+3.)*(mu_r+2)*(mu_r+1.)/(qr))**thrd  ! recalculate slope based on mu_r

       ! apply lambda limiters for rain
          lammax = (mu_r+1.)*1.e+5
          lammin = (mu_r+1.)*inv_Drmax
          if (lamr.lt.lammin) then
             lamr = lammin
             nr   = exp(3.*log(lamr)+log(qr)+log(gamma(mu_r+1.))-log(gamma(mu_r+4.)))/(cons1)
          elseif (lamr.gt.lammax) then
             lamr = lammax
             nr   = exp(3.*log(lamr)+log(qr)+log(gamma(mu_r+1.))-log(gamma(mu_r+4.)))/(cons1)
          endif

          logn0r  = alog10(nr)+(mu_r+1.)*alog10(lamr)-alog10(gamma(mu_r+1)) !note: logn0r is calculated as log10(n0r)
          cdistr  = nr/gamma(mu_r+1.)
          nr_grd  = nr/iSPF  !compute modified grid-mean value (passed back)

       else

          lamr   = 0.
          cdistr = 0.
          logn0r = 0.

       endif

 end subroutine get_rain_dsd2


!===========================================================================================
 subroutine calc_bulkRhoRime(qi_tot,qi_rim,bi_rim,rho_rime)

!--------------------------------------------------------------------------------
!  Calculates and returns the bulk rime density from the prognostic ice variables
!  and adjusts qirim and birim appropriately.
!--------------------------------------------------------------------------------

 implicit none

!arguments:
 real, intent(in)    :: qi_tot
 real, intent(inout) :: qi_rim,bi_rim
 real, intent(out)   :: rho_rime

 !--------------------------------------------------------------------------

 if (bi_rim.ge.1.e-15) then
!if (bi_rim.ge.bsmall) then
    rho_rime = qi_rim/bi_rim
    !impose limits on rho_rime;  adjust bi_rim if needed
    if (rho_rime.lt.rho_rimeMin) then
       rho_rime = rho_rimeMin
       bi_rim   = qi_rim/rho_rime
    elseif (rho_rime.gt.rho_rimeMax) then
       rho_rime = rho_rimeMax
       bi_rim   = qi_rim/rho_rime
    endif
 else
    qi_rim   = 0.
    bi_rim   = 0.
    rho_rime = 0.
 endif

 !set upper constraint qi_rim <= qi_tot
 if (qi_rim.gt.qi_tot .and. rho_rime.gt.0.) then
    qi_rim = qi_tot
    bi_rim = qi_rim/rho_rime
 endif

 !impose consistency
 if (qi_rim.lt.qsmall) then
    qi_rim = 0.
    bi_rim = 0.
 endif


 end subroutine calc_bulkRhoRime

!===========================================================================================
 subroutine impose_max_total_Ni(nitot_local,max_total_Ni,inv_rho_local)

!--------------------------------------------------------------------------------
! Impose maximum total ice number concentration (total of all ice categories).
! If the sum of all nitot(:) exceeds maximum allowable, each category to preserve
! ratio of number between categories.
!--------------------------------------------------------------------------------

 implicit none

!arguments:
 real, intent(inout), dimension(:) :: nitot_local           !note: dimension (nCat)
 real, intent(in)                  :: max_total_Ni,inv_rho_local

!local variables:
 real                              :: dum

 if (sum(nitot_local(:)).ge.1.e-20) then
    dum = max_total_Ni*inv_rho_local/sum(nitot_local(:))
    nitot_local(:) = nitot_local(:)*min(dum,1.)
 endif

 end subroutine impose_max_total_Ni

!===========================================================================================

 real function qv_sat(t_atm,p_atm,i_wrt)

!------------------------------------------------------------------------------------
! Calls polysvp1 to obtain the saturation vapor pressure, and then computes
! and returns the saturation mixing ratio, with respect to either liquid or ice,
! depending on value of 'i_wrt'
!------------------------------------------------------------------------------------

 implicit none

 !Calling parameters:
 real    :: t_atm  !temperature [K]
 real    :: p_atm  !pressure    [Pa]
 integer :: i_wrt  !index, 0 = w.r.t. liquid, 1 = w.r.t. ice

 !Local variables:
 real    :: e_pres         !saturation vapor pressure [Pa]

 !------------------

#ifdef ECCCGEM
  if (i_wrt.eq.1) e_pres = foew(t_atm)
  if (i_wrt.eq.0) e_pres = foewa(t_atm)
  qv_sat = ep_2*e_pres/max(1.e-3,(p_atm-e_pres))
#else
  e_pres = polysvp1(t_atm,i_wrt)
  qv_sat = ep_2*e_pres/max(1.e-3,(p_atm-e_pres))
#endif

 return
 end function qv_sat

!===========================================================================================

 subroutine check_values(Qv,T,Qc,Nc,Qr,Nr,Qitot,Qirim,Nitot,Birim,i,timestepcount,       &
                         force_abort_in,source_ind,Zitot)

!------------------------------------------------------------------------------------
! Checks current values of prognotic variables for reasonable values and
! stops and prints values if they are out of specified allowable ranges.
!
! 'check_consistency' means include trap for inconsistency in moments;
! otherwise, only trap for Q, T, and negative Qx, etc.  This option is here
! to allow for Q<qsmall.and.N>nsmall or Q>qsmall.and.N<small which can be produced
! at the leading edges due to sedimentation and whose values are accpetable
! since lambda limiters are later imposed after SEDI (so one does not necessarily
! want to trap for inconsistency after sedimentation has been called).
!
! The value 'source_ind' indicates the approximate location in 'p3_main'
! from where 'check_values' was called before it resulted in a trap.
!
!------------------------------------------------------------------------------------

  implicit none

 !Calling parameters:
  real, dimension(:),   intent(in) :: Qv,T,Qc,Qr,Nr,Nc
  real, dimension(:,:), intent(in) :: Qitot,Qirim,Nitot,Birim
  real, dimension(:,:), intent(in), optional :: Zitot
  integer,              intent(in) :: source_ind,i,timestepcount
  logical,              intent(in) :: force_abort_in         !.TRUE. = forces abort if value violation is detected

 !logical,              intent(in) :: check_consistency   !.TRUE. = check for sign consistency between Qx and Nx

 !Local variables:
  real, parameter :: T_low  = 173.
  real, parameter :: T_high = 323.
  real, parameter :: Q_high = 60.e-3
  real, parameter :: N_high = 1.e+20
  real, parameter :: B_high = Q_high*5.e-3
  real, parameter :: Z_high = 10.
  integer         :: k,iice,nk,ncat
  logical         :: badvalue_found

  nk   = size(Qitot,dim=1)
  nCat = size(Qitot,dim=2)

  badvalue_found = .false.

  k_loop: do k = 1,nk

   ! check unrealistic values T and Qv
     if (.not.(T(k)>T_low .and. T(k)<T_high)) then
        write(6,'(a41,4i5,1e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,T: ',      &
           source_ind,i,k,timestepcount,T(k)
        badvalue_found = .true.
     endif
     if (.not.(Qv(k)>=0. .and. Qv(k)<Q_high)) then
        write(6,'(a42,4i5,1e15.6)') '** WARNING IN P3_MAIN -- src,i,k,step,Qv: ',     &
           source_ind,i,k,timestepcount,Qv(k)
        badvalue_found = .true.
     endif

   ! check for NANs:
      if (.not.(T(k)  == T(k))  .or.            &
          .not.(Qv(k) == Qv(k)) .or.            &
          .not.(Qc(k) == Qc(k)) .or.            &
          .not.(Nc(k) == Nc(k)) .or.            &
          .not.(Qr(k) == Qr(k)) .or.            &
          .not.(Nr(k) == Nr(k)) ) then
         write(6,'(a56,4i5,6e15.6)') '*A WARNING IN P3_MAIN -- src,i,k,step,T,Qv,Qc,Nc,Qr,Nr: ', &
              source_ind,i,k,timestepcount,T(k),Qv(k),Qc(k),Nc(k),Qr(k),Nr(k)
         badvalue_found = .true.
      endif
      do iice = 1,ncat
         if (.not.(Qitot(k,iice) == Qitot(k,iice)) .or.            &
             .not.(Qirim(k,iice) == Qirim(k,iice)) .or.            &
             .not.(Nitot(k,iice) == Nitot(k,iice)) .or.            &
             .not.(Birim(k,iice) == Birim(k,iice)) ) then
            write(6,'(a68,5i5,4e15.6)') '*B WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim: ',  &
                 source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice)
            badvalue_found = .true.
         endif
      enddo

   ! check unrealistic values Qc,Nc
     if ( .not.(Qc(k)==0. .and. Nc(k)==0.) .and.                               &  !ignore for all zeroes
           ( ((Qc(k)>0..and.Nc(k)<=0.) .or. (Qc(k)<=0..and.Nc(k)>0.))          &  !inconsistency
            .or. Qc(k)<0. .or. Qc(k)>Q_high                                    &
            .or. Nc(k)<0. .or. Nc(k)>N_high  )                                 &  !unrealistic values
            .and. source_ind /= 100                                            &  !skip trap for this source_ind
            .and. source_ind /= 200                                            &  !skip trap for this source_ind
            .and. source_ind /= 300 ) then                                        !skip trap for this source_ind
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQc,Nc: ', &
           source_ind,i,k,timestepcount,Qc(k),Nc(k)
        badvalue_found = .true.
     endif

   ! check unrealistic values Qr,Nr
     if ( .not.(Qr(k)==0. .and. Nr(k)==0.) .and.                               &  !ignore for all zeroes
           ( ((Qr(k)>0..and.Nr(k)<=0.) .or. (Qr(k)<=0..and.Nr(k)>0.))          &  !inconsistency
            .or. Qr(k)<0. .or. Qr(k)>Q_high                                    &
            .or. Nr(k)<0. .or. Nr(k)>N_high  )                                 &  !unrealistic values
            .and. source_ind /= 100                                            &  !skip trap for this source_ind
            .and. source_ind /= 200                                            &  !skip trap for this source_ind
            .and. source_ind /= 300 ) then                                        !skip trap for this source_ind
        write(6,'(a45,4i5,4e15.6)') '*C WARNING IN P3_MAIN -- src,i,k,stepQr,Nr: ', &
           source_ind,i,k,timestepcount,Qr(k),Nr(k)
        badvalue_found = .true.
     endif

   ! check unrealistic values Qitot,Qirim,Nitot,Birim
     do iice = 1,ncat

        if ( .not.(Qitot(k,iice)==0..and.Qirim(k,iice)==0..and.Nitot(k,iice)==0..and.Birim(k,iice)==0.).and.  &  !ignore for all zeroes
             ( ((Qitot(k,iice)>0..and.Nitot(k,iice)<=0.) .or. (Qitot(k,iice)<=0..and.Nitot(k,iice)>0.) )      &  !inconsistency
               .or. Qitot(k,iice)<0. .or. Qitot(k,iice)>Q_high                                                &  !unrealistic values
               .or. Qirim(k,iice)<0. .or. Qirim(k,iice)>Q_high                                                &
               .or. Nitot(k,iice)<0. .or. Nitot(k,iice)>N_high                                                &
               .or. Birim(k,iice)<0. .or. Birim(k,iice)>B_high )                                              &
               .and. source_ind /= 100                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 200                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 300 ) then                                                                    !skip trap for this source_ind
           write(6,'(a68,5i5,4e15.6)') '*D WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Qirim,Nitot,Birim: ', &
              source_ind,i,k,timestepcount,iice,Qitot(k,iice),Qirim(k,iice),Nitot(k,iice),Birim(k,iice)
           badvalue_found = .true.
            print*, '**: ',Qitot(k,iice)>Q_high, Qirim(k,iice)>Q_high, Nitot(k,iice)>N_high,  &
                           Birim(k,iice)>B_high, Q_high, N_high, B_high
        endif

        if (present(Zitot)) then
           if ( .not.(Qitot(k,iice)==0. .and. Nitot(k,iice)==0. .and. Zitot(k,iice)==0.) .and.                &
               ( Qitot(k,iice)>0. .and. Nitot(k,iice)>0. .and. Zitot(k,iice)<=0. )                            &  !inconsistency
               .and. source_ind /= 100                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 200                                                                        &  !skip trap for this source_ind
               .and. source_ind /= 300 ) then                                                                    !skip trap for this source_ind
              write(6,'(a62,5i5,3e15.6)') '*E WARNING IN P3_MAIN -- src,i,k,step,iice,Qitot,Nitot,Zitot: ',   &
                 source_ind,i,k,timestepcount,iice,Qitot(k,iice),Nitot(k,iice),Zitot(k,iice)
              badvalue_found = .true.
           endif
        endif

     enddo  !iice-loop

  enddo k_loop

  if (badvalue_found .and. force_abort_in) then
     print*
     print*,'** DEBUG TRAP IN P3_MAIN, s/r CHECK_VALUES -- source: ',source_ind
     print*
     global_status = STATUS_ERROR
     stop
     return
  endif

 end subroutine check_values

!==========================================================================================!
 real function compute_mu_3moment(mom0,mom3,mom6,mu_max)

 !--------------------------------------------------------------------------
 ! Computes mu as a function of moments 0, 3, and 6 of the size distribution
 ! represented by N(D) = No*D^mu*e(-lambda*D).
 !
 ! Note:  moment 3 is not equal to the mass mixing ratio (due to variable density)
 !
 ! G(mu)= mom0*mom6/mom3^2 = [(6+mu)(5+mu)(4+mu)]/[(3+mu)(2+mu)(1+mu)]
 !--------------------------------------------------------------------------

 implicit none

! Arguments passed:
 real, intent(in) :: mom0    !0th moment
 real, intent(in) :: mom3    !3th moment  (note, not normalized)
 real, intent(in) :: mom6    !6th moment  (note, not normalized)
 real, intent(in) :: mu_max  !maximum allowable value of mu

! Local variables:
 real             :: mu   ! shape parameter in gamma distribution
 real             :: G    ! function of mu (see comments above)
 real             :: g2
!real             :: a1,g1
!real, parameter  :: eps_m0 = 1.e-20
 real, parameter  :: eps_m3 = 1.e-20
 real, parameter  :: eps_m6 = 1.e-35

 if (mom3>eps_m3) then

    !G = (mom0*mom6)/(mom3**2)
    !To avoid very small values of mom3**2
     G = (mom0/mom3)*(mom6/mom3)

!----------------------------------------------------------!
! !Solve alpha numerically: (brute-force)
!      mu= 0.
!      g2= 999.
!      do i=0,4000
!         a1= i*0.01
!         g1= (6.+a1)*(5.+a1)*(4.+a1)/((3.+a1)*(2.+a1)*(1.+a1))
!         if(abs(g-g1)<abs(g-g2)) then
!            mu = a1
!            g2= g1
!         endif
!      enddo
!----------------------------------------------------------!

!Piecewise-polynomial approximation of G(mu) to solve for mu:
     if (G>=20.) then
        mu = 0.
     else
        g2 = G**2
        if (G<20.  .and.G>=13.31) then
           mu = 3.3638e-3*g2 - 1.7152e-1*G + 2.0857e+0
        elseif (G<13.31.and.G>=7.123) then
           mu = 1.5900e-2*g2 - 4.8202e-1*G + 4.0108e+0
        elseif (G<7.123.and.G>=4.200) then
           mu = 1.0730e-1*g2 - 1.7481e+0*G + 8.4246e+0
        elseif (G<4.200.and.G>=2.946) then
           mu = 5.9070e-1*g2 - 5.7918e+0*G + 1.6919e+1
        elseif (G<2.946.and.G>=1.793) then
           mu = 4.3966e+0*g2 - 2.6659e+1*G + 4.5477e+1
        elseif (G<1.793.and.G>=1.405) then
           mu = 4.7552e+1*g2 - 1.7958e+2*G + 1.8126e+2
        elseif (G<1.405.and.G>=1.230) then
           mu = 3.0889e+2*g2 - 9.0854e+2*G + 6.8995e+2
        elseif (G<1.230) then
           mu = mu_max
        endif
     endif

     compute_mu_3moment = mu

 else

    print*, 'Input parameters out of bounds in function COMPUTE_MU_3MOMENT'
    print*, 'mom0 = ',mom0
    print*, 'mom3 = ',mom3
    print*, 'mom6 = ',mom6
    stop

 endif

 end function compute_mu_3moment

!======================================================================================!
 real function G_of_mu(mu)

!arguments:
 real, intent(in) :: mu

 G_of_mu = ((6.+mu)*(5.+mu)*(4.+mu))/((3.+mu)*(2.+mu)*(1.+mu))

 end function G_of_mu

!======================================================================================!

 real function maxHailSize(rho,qit,qim,nit,rhofaci,lam,mu)

 !--------------------------------------------------------------------------
 ! Computes the maximum hail size, by estimating the maximum size that is
 ! physically observable (and not just a numerical artifact of the compete
 ! gamma size distribution).
 !
 ! Follows method described in Milbrandt and Yau (2006a)
 !
 ! TO DO:
 ! - pass density (not just rime density) and add to is_hail criteria
 ! - clean up method selection (e.g. argument parameter to specify method)
 !
 !                                 *** NOTE ***
 !
 !   The current coding for this function is problematic.
 !   In GEM, overflows (apparently) result in crashes.  In WRF, no crashes but erroneous
 !   zero values in spots (possibly resulting from over/underflows)
 !  --> Code is kep here for now; however this fields is better computed in 
 !      post-processing.  Eventaully this function will be removed.
 !--------------------------------------------------------------------------

 implicit none

! Arguments passed:
 real, intent(in) :: rho        ! air density   [kg m-3]
 real, intent(in) :: qit        ! prognostic ice total mass mixing ratios
 real, intent(in) :: qim        ! prognostic ice rime  mass mixing ratios
 real, intent(in) :: nit        ! total num and total number mixing ratio
 real, intent(in) :: rhofaci    ! air density correction factor for ice fall speed
 real, intent(in) :: lam,mu     ! PSD slope and shape parameters

! Local variables:
 real, parameter  :: dD       =   1.e-3  ! diameter bin width [m]
 real, parameter  :: Dmax_psd = 200.e-3  ! maximum diameter in PSD to compute integral  [m]
 real, parameter  :: FrThrs   = 0.75     ! theshold rime fraction to be considered graupel/hail
!real, parameter  :: Ncrit    = 1.e-4    ! threshold physically observable number concentration [# m-3]
 real, parameter  :: Rcrit    = 1.e-3/6. ! threshold physically observable number flux          [# m-2 s-1]
 real, parameter  :: ch       = 206.89   ! coefficient in V-D fall speed relation for hail (from MY2006a)
 real, parameter  :: dh       = 0.6384   ! exponent in V-D fall speed relation for hail (from MY2006a)
 real             :: n0                  ! shape parameter in gamma distribution
 real             :: Frim                ! rime mass fraction
 real             :: Di                  ! diameter  [m]
 real             :: N_tot               ! total number concentration                             [# m-3]
 real             :: N_tail              ! number conc. from Di to infinity; i.e. trial for Nh*{D*} in MY2006a [# m-3]
 real             :: R_tail              ! number flux of large hail; i.e. trial for Rh*{D*} (corrected from MY2006a [# m-2 s-1]
!real             :: Dhmax_1             ! maximum hail sized based on Nh*  [m]
 real             :: Dhmax_2             ! maximum hail sized based on Rh*  [m]
 real             :: V_h                 ! fall speed of hail of size D     [m s-1]
 integer          :: nd                  ! maximum number of size bins for integral
 integer          :: i                   ! index for integration

 Frim  = qim/max(qit,1.e-14)
 N_tot = rho*nit

! considered_hail: if (Frim>FrThrs .and. N_tot>Ncrit) then
 considered_hail: if (Frim>FrThrs) then

    nd  = int(Dmax_psd/dD)
    n0  = N_tot*lam**(mu+1.)/gamma(mu+1.)
!   n0  = dble(N_tot)*dexp( dble(mu+1.)*dlog(dble(lam)) )/dble(gamma(mu+1.))
    N_tail = 0.


   !-- method 1, based on Nh*crit only:
!     Dhmax_1 = 0.
!     do i = nd,1,-1
!        Di = i*dD
!        N_tail = N_tail + n0*Di**mu*exp(-lam*Di)*dD
!        if (N_tail>Ncrit) then
!           Dhmax_1 = Di
!           exit
!        endif
!     enddo
!     maxHailSize = Dhmax_1

!-- method 2, based on Rh*crit only:
    R_tail  = 0.
    Dhmax_2 = 0.
    do i = nd,1,-1
       Di  = i*dD
       V_h = rhofaci*(ch*Di**Dh)
       R_tail = R_tail + V_h*n0*Di**mu*exp(-lam*Di)*dD
!      R_tail = R_tail + V_h*sngl(n0*dble(Di)**dble(mu)*exp(-dble(lam)*dble(Di))*dble(dD))
       if (R_tail>Rcrit) then
          Dhmax_2 = Di
          exit
       endif
    enddo
    maxHailSize = Dhmax_2

!-- method 3, finds values based on Nh*crit and Rh*crit methods
! !  found_N2 = .false.
! !  found_R2 = .false.
! !  do i = nd,1,-1
! !     Di = i*dD
! !     N_tail = N_tail + n0*Di**mu*exp(-lam*Di)*dD
! !     R_tail = N_tail*(ch*Di**Dh)
! !     if (N_tail>Ncrit) .and. .not.found_N2) then
! !        Dhmax_1 = Di       ! max hail size based on N*crit
! !        found_N2 = .true.
! !     endif
! !     if (R_tail>Rcrit) .and. .not.found_R2) then
! !        Dhmax_2 = Di       ! max hail size based on R*crit
! !        found_R2 = .true.
! !     endif
! !     if (found_N2 .and. found_R2) exit
! !
! !  enddo

 else

    maxHailSize = 0.

 endif considered_hail

 end function maxHailSize

!===========================================================================================

#ifdef ECCCGEM

  ! Define bus requirements
  function p3_phybusinit() result(F_istat)
    use phy_status, only: PHY_OK, PHY_ERROR
    use bus_builder, only: bb_request
    use phy_options, only: p3_trplmomi

    implicit none
    integer :: F_istat                          !Function return status
    logical :: buserr

    F_istat = PHY_ERROR
    if (n_iceCat < 0) then
       call physeterror('microphy_p3::p3_phybusinit', &
            'Called mp_phybusinit() before mp_init()')
       return
    endif
    buserr = .false.
    if (bb_request((/ &
         'CLOUD_WATER_MASS ', &
         'CLOUD_WATER_NUM  ', &
         'RAIN_MASS        ', &
         'RAIN_NUM         ', &
         'ICE_MASS_TEND    ', &
         'ICE_EFF_RAD      ', &
         'RATE_PRECIP_TYPES', &
         'PARTICLE_DIAMETER', &
         'CCN_NUM          ', &
         'MPDIAG_2D        ', &
         'MPDIAG_3D        ', &
         'MPVIS            ', &
         'REFLECTIVITY     ', &
         'LIGHTNING        ' &
         /)) /= PHY_OK) buserr = .true.
    if (.not. buserr) then
       if (bb_request('ICE_CAT_1') /= PHY_OK) buserr = .true.
    endif
    if (p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_1_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 1 .and. .not. buserr) then
       if (bb_request('ICE_CAT_2') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 1 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_2_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 2 .and. .not. buserr) then
       if (bb_request('ICE_CAT_3') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 2 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_3_TM') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 3 .and. .not. buserr) then
       if (bb_request('ICE_CAT_4') /= PHY_OK) buserr = .true.
    endif
    if (n_iceCat > 3 .and. p3_trplmomi .and. .not. buserr) then
       if (bb_request('ICE_CAT_4_TM') /= PHY_OK) buserr = .true.
    endif

    if (buserr) then
       call physeterror('microphy_p3::p3_phybusinit', &
            'Cannot construct bus request list')
       return
    endif
    F_istat = PHY_OK
    return
  end function p3_phybusinit

!===========================================================================================

  ! Compute total water mass
  function p3_lwc(F_qltot, F_dbus, F_pbus, F_vbus) result(F_istat)
    use phybus
    use phy_status, only: PHY_OK, PHY_ERROR
    implicit none
    real, dimension(:,:), intent(out) :: F_qltot        !Total water mass (kg/kg)
    real, dimension(:), pointer, contiguous :: F_dbus   !Dynamics bus
    real, dimension(:), pointer, contiguous :: F_pbus   !Permanent bus
    real, dimension(:), pointer, contiguous :: F_vbus   !Volatile bus
    integer :: F_istat                                  !Return status
#include "phymkptr.hf"
    integer :: ni, nkm1
    real, dimension(:,:), pointer :: zqcp, zqrp
    F_istat = PHY_ERROR
    ni = size(F_qltot, dim=1); nkm1 = size(F_qltot, dim=2)
    MKPTR2Dm1(zqcp, qcplus, F_dbus)
    MKPTR2Dm1(zqrp, qrplus, F_dbus)
    F_qltot(:,:) = zqcp(:,:) + zqrp(:,:)
    F_istat = PHY_OK
    return
  end function p3_lwc

!===========================================================================================

  ! Compute total ice mass
  function p3_iwc(F_qitot, F_dbus, F_pbus, F_vbus) result(F_istat)
    use phybus
    use phy_status, only: PHY_OK, PHY_ERROR
    implicit none
    real, dimension(:,:), intent(out) :: F_qitot        !Total ice mass (kg/kg)
    real, dimension(:), pointer, contiguous :: F_dbus   !Dynamics bus
    real, dimension(:), pointer, contiguous :: F_pbus   !Permanent bus
    real, dimension(:), pointer, contiguous :: F_vbus   !Volatile bus
    integer :: F_istat                                  !Return status
#include "phymkptr.hf"
    integer :: ni, nkm1
    real, dimension(:,:), pointer :: zqti1p, zqti2p, zqti3p, zqti4p
    F_istat = PHY_ERROR
    ni = size(F_qitot, dim=1); nkm1 = size(F_qitot, dim=2)
    MKPTR2Dm1(zqti1p, qti1plus, F_dbus)
    MKPTR2Dm1(zqti2p, qti2plus, F_dbus)
    MKPTR2Dm1(zqti3p, qti3plus, F_dbus)
    MKPTR2Dm1(zqti4p, qti4plus, F_dbus)
    F_qitot = 0.
    if (associated(zqti1p)) F_qitot = F_qitot + zqti1p
    if (associated(zqti2p)) F_qitot = F_qitot + zqti2p
    if (associated(zqti3p)) F_qitot = F_qitot + zqti3p
    if (associated(zqti4p)) F_qitot = F_qitot + zqti4p
    F_istat = PHY_OK
    return
  end function p3_iwc

#endif

!======================================================================================!
 END MODULE microphy_p3
