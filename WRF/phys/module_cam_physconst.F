#define WRF_PORT
#define MODAL_AERO
!------------------------------------------------------------------------
! Based on physconst.F90 from CAM
! Ported to WRF by William.Gustafson@pnl.gov, Nov. 2009
! Updated to version from CESM 1.0.1 Nov. 2010
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov
!------------------------------------------------------------------------
module physconst

   ! Physical constants.  Use CCSM shared values whenever available.

   use shr_kind_mod, only: r8 => shr_kind_r8
   use shr_const_mod, only: shr_const_g,      shr_const_stebol, shr_const_tkfrz,  &
                            shr_const_mwdair, shr_const_rdair,  shr_const_mwwv,   &
                            shr_const_latice, shr_const_latvap, shr_const_cpdair, &
                            shr_const_rhofw,  shr_const_cpwv,   shr_const_rgas,   &
                            shr_const_karman, shr_const_pstd,   shr_const_rhodair,&
                            shr_const_avogad, shr_const_boltz,  shr_const_cpfw,   &
                            shr_const_rwv,    shr_const_zvir,   shr_const_pi,     &
                            shr_const_rearth, shr_const_sday,   shr_const_cday,   &
                            shr_const_spval         
   implicit none
   private
   public  :: physconst_readnl
   save
   ! Constants based off share code or defined in physconst
   real(r8), public, parameter :: avogad      = shr_const_avogad     ! Avogadro's number (molecules/kmole)
   real(r8), public, parameter :: boltz       = shr_const_boltz      ! Boltzman's constant (J/K/molecule)
   real(r8), public, parameter :: cday        = shr_const_cday       ! sec in calendar day ~ sec
   real(r8), public, parameter :: cpair       = shr_const_cpdair     ! specific heat of dry air (J/K/kg)
   real(r8), public, parameter :: cpliq       = shr_const_cpfw       ! specific heat of fresh h2o (J/K/kg)
   real(r8), public, parameter :: karman      = shr_const_karman     ! Von Karman constant
   real(r8), public, parameter :: latice      = shr_const_latice     ! Latent heat of fusion (J/kg)
   real(r8), public, parameter :: latvap      = shr_const_latvap     ! Latent heat of vaporization (J/kg)
   real(r8), public, parameter :: pi          = shr_const_pi         ! 3.14...
   real(r8), public, parameter :: pstd        = shr_const_pstd       ! Standard pressure (Pascals)
   real(r8), public, parameter :: r_universal = shr_const_rgas       ! Universal gas constant (J/K/kmol)
   real(r8), public, parameter :: rhoh2o      = shr_const_rhofw      ! Density of liquid water (STP)
   real(r8), public, parameter :: spval       = shr_const_spval      !special value 
   real(r8), public, parameter :: stebol      = shr_const_stebol     ! Stefan-Boltzmann's constant (W/m^2/K^4)

   real(r8), public, parameter :: c0          = 2.99792458e8_r8      ! Speed of light in a vacuum (m/s)
   real(r8), public, parameter :: planck      = 6.6260755e-34_r8     ! Planck's constant (J.s)

   ! Molecular weights
   real(r8), public, parameter :: mwco2       =  44._r8             ! molecular weight co2
   real(r8), public, parameter :: mwn2o       =  44._r8             ! molecular weight n2o
   real(r8), public, parameter :: mwch4       =  16._r8             ! molecular weight ch4
   real(r8), public, parameter :: mwf11       = 136._r8             ! molecular weight cfc11
   real(r8), public, parameter :: mwf12       = 120._r8             ! molecular weight cfc12
   real(r8), public, parameter :: mwo3        =  48._r8             ! molecular weight O3
   real(r8), public, parameter :: mwso2       =  64._r8
   real(r8), public, parameter :: mwso4       =  96._r8
   real(r8), public, parameter :: mwh2o2      =  34._r8
   real(r8), public, parameter :: mwdms       =  62._r8


   ! modifiable physical constants for aquaplanet

   real(r8), public           :: gravit       = shr_const_g     ! gravitational acceleration (m/s**2)
   real(r8), public           :: sday         = shr_const_sday  ! sec in siderial day ~ sec
   real(r8), public           :: mwh2o        = shr_const_mwwv  ! molecular weight h2o
   real(r8), public           :: cpwv         = shr_const_cpwv  ! specific heat of water vapor (J/K/kg)
   real(r8), public           :: mwdry        = shr_const_mwdair! molecular weight dry air
   real(r8), public           :: rearth       = shr_const_rearth! radius of earth (m)
   real(r8), public           :: tmelt        = shr_const_tkfrz ! Freezing point of water (K)

!---------------  Variables below here are derived from those above -----------------------

   real(r8), public           :: rga          = 1._r8/shr_const_g                 ! reciprocal of gravit
   real(r8), public           :: ra           = 1._r8/shr_const_rearth            ! reciprocal of earth radius
   real(r8), public           :: omega        = 2.0_R8*shr_const_pi/shr_const_sday! earth rot ~ rad/sec
   real(r8), public           :: rh2o         = shr_const_rgas/shr_const_mwwv     ! Water vapor gas constant ~ J/K/kg
   real(r8), public           :: rair         = shr_const_rdair   ! Dry air gas constant     ~ J/K/kg
   real(r8), public           :: epsilo       = shr_const_mwwv/shr_const_mwdair   ! ratio of h2o to dry air molecular weights 
   real(r8), public           :: zvir         = (shr_const_rwv/shr_const_rdair)-1.0_R8 ! (rh2o/rair) - 1
   real(r8), public           :: cpvir        = (shr_const_cpwv/shr_const_cpdair)-1.0_R8 ! CPWV/CPDAIR - 1.0
   real(r8), public           :: rhodair      = shr_const_pstd/(shr_const_rdair*shr_const_tkfrz)
   real(r8), public           :: cappa        = (shr_const_rgas/shr_const_mwdair)/shr_const_cpdair  ! R/Cp
   real(r8), public           :: ez           ! Coriolis expansion coeff -> omega/sqrt(0.375)   
   real(r8), public           :: Cpd_on_Cpv   = shr_const_cpdair/shr_const_cpwv
                         
!================================================================================================
contains
!================================================================================================

   ! Read namelist variables.
   subroutine physconst_readnl(nlfile)
#ifndef WRF_PORT
      use namelist_utils,  only: find_group_name
      use units,           only: getunit, freeunit
      use mpishorthand
      use spmd_utils,      only: masterproc
      use abortutils,      only: endrun
      use cam_logfile,     only: iulog
#endif   
      character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input
#ifndef WRF_PORT
      ! Local variables
      integer :: unitn, ierr
      character(len=*), parameter :: subname = 'physconst_readnl'
      logical       newg, newsday, newmwh2o, newcpwv, newmwdry, newrearth, newtmelt

      ! Physical constants needing to be reset (ie. for aqua planet experiments)
      namelist /physconst_nl/  cpwv, gravit, mwdry, mwh2o, rearth, sday, tmelt

      !-----------------------------------------------------------------------------

      if (masterproc) then
         unitn = getunit()
         open( unitn, file=trim(nlfile), status='old' )
         call find_group_name(unitn, 'physconst_nl', status=ierr)
         if (ierr == 0) then
            read(unitn, physconst_nl, iostat=ierr)
            if (ierr /= 0) then
               call endrun(subname // ':: ERROR reading namelist')
            end if
         end if
         close(unitn)
         call freeunit(unitn)
      end if

#ifdef SPMD
      ! Broadcast namelist variables
      call mpibcast(cpwv,      1,                   mpir8,   0, mpicom)
      call mpibcast(gravit,    1,                   mpir8,   0, mpicom)
      call mpibcast(mwdry,     1,                   mpir8,   0, mpicom)
      call mpibcast(mwh2o,     1,                   mpir8,   0, mpicom)
      call mpibcast(rearth,    1,                   mpir8,   0, mpicom)
      call mpibcast(sday,      1,                   mpir8,   0, mpicom)
      call mpibcast(tmelt,     1,                   mpir8,   0, mpicom)
#endif


      
      newg     =  gravit .ne. shr_const_g 
      newsday  =  sday   .ne. shr_const_sday
      newmwh2o =  mwh2o  .ne. shr_const_mwwv
      newcpwv  =  cpwv   .ne. shr_const_cpwv
      newmwdry =  mwdry  .ne. shr_const_mwdair
      newrearth=  rearth .ne. shr_const_rearth
      newtmelt =  tmelt  .ne. shr_const_tkfrz
      
      
      
      if (newg .or. newsday .or. newmwh2o .or. newcpwv .or. newmwdry .or. newrearth .or. newtmelt) then
         if (masterproc) then
            write(iulog,*)'****************************************************************************'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif 
            write(iulog,*)'***    New Physical Constant Values set via namelist                     ***'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif 
            write(iulog,*)'***                                                                      ***'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif 
            write(iulog,*)'***    Physical Constant    Old Value                  New Value         ***'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif 
            if (newg)       write(iulog,*)'***       GRAVITY   ',shr_const_g,gravit,'***'
            if (newsday)    write(iulog,*)'***       SDAY      ',shr_const_sday,sday,'***'
            if (newmwh2o)   write(iulog,*)'***       MWH20     ',shr_const_mwwv,mwh2o,'***'
            if (newcpwv)    write(iulog,*)'***       CPWV      ',shr_const_cpwv,cpwv,'***'
            if (newmwdry)   write(iulog,*)'***       MWDRY     ',shr_const_mwdair,mwdry,'***'
            if (newrearth)  write(iulog,*)'***       REARTH    ',shr_const_rearth,rearth,'***'
            if (newtmelt)   write(iulog,*)'***       TMELT     ',shr_const_tkfrz,tmelt,'***'
            write(iulog,*)'****************************************************************************'
#ifdef WRF_PORT
            call wrf_message(iulog)
#endif 
         end if
         rga         = 1._r8/gravit 
         ra          = 1._r8/rearth
         omega       = 2.0_R8*pi/sday
         cpvir       = cpwv/cpair - 1._r8
         epsilo      = mwh2o/mwdry      
         
         !  rair and rh2o have to be defined before any of the variables that use them
         
         rair        = r_universal/mwdry
         rh2o        = r_universal/mwh2o  
         
         cappa       = rair/cpair       
         rhodair     = pstd/(rair*tmelt)
         zvir        =  (rh2o/rair)-1.0_R8
         ez          = omega / sqrt(0.375_r8)
         Cpd_on_Cpv  = cpair/cpwv
         
      else	
         ez          = omega / sqrt(0.375_r8)
      end if
#endif     
    end subroutine physconst_readnl
  end module physconst
