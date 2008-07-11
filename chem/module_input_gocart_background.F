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

MODULE module_input_gocart_background

    USE module_io_domain
    USE module_machine
    USE module_domain
    USE module_io
    USE module_io_wrf
    USE module_integrate 
    USE module_driver_constants
    USE module_state_description
    USE module_configure
    USE module_timing
    USE module_utility
    USE module_date_time
    USE module_wrf_error
!   USE module_data_radm2
!   USE module_aerosols_sorgam
    USE module_model_constants
    USE module_get_file_names

#ifdef DM_PARALLEL
   USE module_dm
#endif



CONTAINS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

SUBROUTINE input_ext_chem_gocart_bg (grid)
!new for bc
   USE module_bc
!  USE module_big_step_utilities_em
!  USE module_get_file_names
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

   IMPLICIT NONE
   INTERFACE
     SUBROUTINE init_domain_constants_em_ptr ( parent , nest )
       USE module_domain
       USE module_configure
       TYPE(domain), POINTER  :: parent , nest
     END SUBROUTINE init_domain_constants_em_ptr

   END INTERFACE


   INTERFACE
     SUBROUTINE Setup_Timekeeping( grid )
      USE module_domain
      TYPE(domain), POINTER :: grid
     END SUBROUTINE Setup_Timekeeping
   END INTERFACE


   TYPE(domain)           ::  grid

   INTEGER ::  i,j,n,numfil,status,system,icnt,fid,ierr

   INTEGER :: ids, ide, jds, jde, kds, kde,    &
              ims, ime, jms, jme, kms, kme,    &
              ips, ipe, jps, jpe, kps, kpe

   REAL, ALLOCATABLE, DIMENSION(:) :: gocart_lev
   REAL, ALLOCATABLE, DIMENSION(:,:) :: tmp2
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: tmp,tmp3,interpolate
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: backg_oh
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: backg_no3
   REAL, ALLOCATABLE, DIMENSION(:,:,:) :: backg_h2o2

   real mmax
#ifdef DEREF_KLUDGE
   INTEGER     :: sm31 , em31 , sm32 , em32 , sm33 , em33 
#endif
! Number of reference emission and LAI files to open
      PARAMETER(numfil=19)

   CHARACTER (LEN=80) :: message

   TYPE (grid_config_rec_type)              :: config_flags

! Filenames of reference emissions and LAI
      CHARACTER*100 onefil
   INTEGER :: iklev
   parameter (iklev=55)
!  REAL    :: p0=1013.
   real,dimension(iklev) :: p_g,p_wrf,ilev
  REAL        :: p00, t00, a, p_surf, pd_surf
   integer :: kbot,ktop,k


!!!!!-------------------------------------------------------------------

       ! Get grid dimensions
       CALL get_ijk_from_grid (  grid ,                        &
                                 ids, ide, jds, jde, kds, kde,    &
                                 ims, ime, jms, jme, kms, kme,    &
                                 ips, ipe, jps, jpe, kps, kpe    )

     WRITE( message , FMT='(A,4I5)' ) ' DIMS: ',ids,ide-1,jds,jde-1
     CALL  wrf_message ( message )

     ALLOCATE( tmp(iklev,ids:ide,jds:jde) )
     ALLOCATE( tmp2(ids:ide,jds:jde) )
     ALLOCATE( tmp3(3,ids:ide,jds:jde) )
     ALLOCATE( interpolate(ids:ide,kds:kde,jds:jde) )
     ALLOCATE( gocart_lev(iklev))
     ALLOCATE( backg_oh(ids:ide,kds:kde,jds:jde) )
     ALLOCATE( backg_no3(ids:ide,kds:kde,jds:jde) )
     ALLOCATE( backg_h2o2(ids:ide,kds:kde,jds:jde) )
     print *,ids,ide,jds,jde,kds,kde
!    CALL wrf_get_dom_ti_real ( fid , 'P_TOP' ,  grid%p_top , 1 , icnt , ierr )
!      CALL wrf_get_dom_ti_integer ( fid , 'MAP_PROJ' , map_proj , 1 , icnt , ierr )

   do k=1,iklev 
        p_g(k) = log10( ilev(k) * 1023.) 
   enddo
      CALL nl_get_base_pres  ( 1 , p00 )
      CALL nl_get_base_temp  ( 1 , t00 )
      CALL nl_get_base_lapse ( 1 , a   )
   print *,'p00,t00,a before = ',p00,t00,a
   t00=290.
   a=50.
   p00=100000.
   do j=grid%sm32,grid%em32
   do i=grid%sm31,grid%em31
      p_surf = p00 * EXP ( -t00/a + ( (t00/a)**2 - 2.*g*grid%ht(i,j)/a/r_d ) **0.5 )
      do k=grid%sm33,grid%em33
         grid%pb(i,k,j) = grid%znu(k)*(p_surf - grid%p_top) + grid%p_top
         if(i.eq.grid%sm31.and.j.eq.grid%sm32)then
          print *,k,grid%pb(i,k,j),p_surf,grid%p_top
         endif
   enddo 
   enddo
   enddo


     OPEN(19,FILE='wrf_gocart_backg',FORM='UNFORMATTED')
     READ(19) tmp2
      mmax=maxval(tmp2)
      print *,mmax
      grid%dms_0(ids:ide,jds:jde) = tmp2
     READ(19) tmp3
      do i=1,3
      mmax=maxval(tmp3(i,ids:ide,jds:jde))
      print *,mmax
       grid%erod(ids:ide,i,jds:jde)=tmp3(i,ids:ide,jds:jde)
      enddo
      mmax=maxval(tmp3)
      print *,mmax
     reaD(19) gocart_lev
     READ(19) tmp
      do i=kds,kde
       grid%backg_h2o2(ids:ide,i,jds:jde)=tmp(i,ids:ide,jds:jde)
      enddo
     READ(19) tmp
       grid%backg_oh(ids:ide,kds:kde,jds:jde)=tmp(kds:kde,ids:ide,jds:jde)
     READ(19) tmp
       grid%backg_no3(ids:ide,kds:kde,jds:jde)=tmp(kds:kde,ids:ide,jds:jde)
     CLOSE(19)
     print *,gocart_lev

! End of loop over file names

      DEALLOCATE( tmp )
      DEALLOCATE( tmp2)
      DEALLOCATE( tmp3)
      DEALLOCATE( interpolate )
      DEALLOCATE( backg_oh )
      DEALLOCATE( backg_no3 )
      DEALLOCATE( backg_h2o2 )

END SUBROUTINE input_ext_chem_gocart_bg 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE module_input_gocart_background

