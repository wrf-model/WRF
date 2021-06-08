MODULE module_twoway_ra_rrtmg_sw

contains

!------------------------------------------------------------------
      Subroutine get_aerosol_Optics_RRTMG_SW ( ns, nmode,delta_z, INMASS_ws,     &
                                               INMASS_in, INMASS_ec, INMASS_ss,  &
                                               INMASS_h2o,  INDGN, INSIG,        &
                                               tauaer, waer, gaer )

!FSB This version switches between BHCOAT to BHMIE depending upon whether
!    EC is present or not. 04/15/2012.

!FSB this version does a core-shell calculation with BHCOAT 04/11/2012    
! This version is set up to be used with RRTMG_SW <<<<<<<<
!     wavelenght is calculated internally
! FSB This routine calculates the aerosol information ( tauaer, waer, 
!     gaer, needed to calculate the solar radiation)  The calling 
!     program specifies the location ( row, column, layer, 
!     layer thicknes, and wave length for the calculation.   
! FSB 02/09/2011 Modifications made to subroutine ghintBH.
! FSB 04/14/2012 REmoved MODULUS, made changes to ghintBH.
!     Put in option for core-shell (coated-sphere). 2

! FSB Input variables:

      use rrtmg_aero_optical_util_module

      implicit none     

      integer,intent(in) ::  ns      ! index for wavelength should be 
                                     ! between 1 and 14. <<< RRTMG_SW
      integer,intent(in) ::  nmode   ! should be 3 for WRF/CMAQ calculation
      real,intent(in)    ::  delta_z ! layer thickness [m]
! FSB mode types for WRF/CMAQ 
!     nmode = 1 Aitken
!     nmode = 2 accumulation
!     nmode = 3 coarse 
! FSB modal mass concentration by species  [ ug / m**3]  NOTE:  MKS
      real, intent(in) ::  INMASS_ws(nmode)   ! water soluble
      real, intent(in) ::  INMASS_in(nmode)   ! insolugle
      real, intent(in) ::  INMASS_ec(nmode)   ! elemental carbon or soot like
      real, intent(in) ::  INMASS_ss(nmode)   ! sea salt
      real, intent(in) ::  INMASS_h2o(nmode)  ! water
! FSB particle size-distribution information      
      real, intent(in) ::  INDGN( nmode)      ! geometric mean diameter [ m ] NOTE: MKS      
      real, intent(in) ::  INSIG( nmode)      ! geometric standard deviation

!FSB output aerosol radiative properties  [dimensionless]
      real, intent(out) ::  tauaer   ! aerosol extinction optical depth
      real, intent(out) ::  waer     ! aerosol single scattering albedo
      real, intent(out) ::  gaer     ! aerosol assymetry parameter

! FSB Internal variables

      real    :: NR(nmode), NI(nmode)           ! refractive indices 
      complex :: refcor(nmode), refshell(nmode) ! complex refracive indices
      complex :: crefin(nmode)                  ! complex refractive index

! FSB special values for EC CORE-shell calculation
      real :: DGNSHELL(nmode)    ! modal geometric mean diameter [m]
      real :: DGNCORE (nmode)    ! modal geometric mean diameter [m] 
       
! FSB Modal volumes [ m**3 / m**3 ]
      real :: MVOL_ws(nmode)   ! water soluble
      real :: MVOL_in(nmode)   ! insolugle
      real :: MVOL_ec(nmode)   ! soot like
      real :: MVOL_ss(nmode)   !sea salt
      real :: MVOL_h2o(nmode)  ! water
!     real :: VOL(nmode)       ! total modal volume [m** 3 / m**3]
! FSB special values for EC CORE-shell calculation      
      real :: VOLCOR(nmode)    ! volume of EC core  [m** 3 / m**3]
      real :: VOLSHELL(nmode)  ! volume of shell    [m** 3 / m**3]
                      
      integer :: m             ! loop index
      real    :: bext          ! extinction coefficient [1 / m]
      real    :: bscat         ! scattering coefficient [1 / m]
      real    :: gfac          ! asymmetry factor

      real    :: bextsum, bscatsum, bsgsum
 
! FSB History variables by wavelength and mode     
!     real    :: bext_wm(ns,nmode) 
!     real    :: bscat_wm(ns,nmode)
!     real    :: gfac_wm(ns,nmode)

      real, parameter ::  one3rd = 1.0 / 3.0 
      real  :: dfac      ! ratio of (volcor/vol) ** one3rd
                         ! used for calculating the diameter
                         ! of the EC core

      logical :: succesS

!...component densities [ g/ cm**3 ] <<<<< cgs

      real, parameter :: rhows = 1.8   !  bulk density of water soluble aerosol

      real, parameter :: rhoin = 2.2   ! bulk density forinsoluble aerosol 

!     real, parameter :: rhoec = 1.7   ! bulk density for soot aerosol
      real, parameter :: rhoec = 1.8   ! new value

      real, parameter :: rhoh2o = 1.0  !  bulk density of aerosol water
       
      real, parameter :: rhoss = 2.2   ! bulk density of seasalt

! FSB scale factor for volume calculation
!            1.0d-12 * [ cm**3 / g] -> [ m** 3 / ug ]
      real, parameter :: scalefactor = 1.0e-12

! FSB scale factor for [1/g] to [1/ug]
      real, parameter ::  cug2g = 1.0e-06

! FSB reciprocal component densities[ m ** 3 / ug ] 
 
      real, parameter :: rhows1 = scalefactor / rhows    !   water soluble aerosol

      real, parameter :: rhoin1 = scalefactor /  rhoin   ! insoluble aerosol 

      real, parameter :: rhoec1 = scalefactor / rhoec    !  soot aerosol

      real, parameter :: rhoh2o1 = scalefactor / rhoh2o  !  aerosol water
       
      real, parameter :: rhoss1 = scalefactor / rhoss    !  seasalt

      integer,parameter ::  nspint_sw = 14 ! number of spectral intervals for RRTMG_SW

! FSB Band numbers and wavelengths for RRTMG_SW   
      integer, parameter :: Band(nspint_sw) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14 /)
      
      real, parameter :: LAMDA_SW(nspint_sw) = (/ 3.4615,  2.7885, 2.325,  2.046,  1.784,         &
                                                  1.4625,  1.2705, 1.0101, 0.7016, 0.53325,       &
                                                  0.38815, 0.299,  0.2316, 8.24 /) ! wavelength [ um ]      
    
! *** refractive indices

! *** Except as otherwise noted reference values of refractive 
!     indices  for aerosol particles are from the OPAC Data base. 
!     Hess,  Koepke, and  Schult, Optical properties of
!     aerosols and clouds: The software package OPAC, Bulletan of
!     the American Meteorological Society, Vol 79, No 5, 
!     pp 831 - 844, May 1998. 
!     OPAC is a downloadable data set of optical properties of 
!     10 aerosol components, 6 water clouds and 3 cirrus clouds 
!     at UV, visible and IR wavelengths
!     www.lrz-muenchen.de/~uh234an/www/radaer/opac.htm


! FSB water soluble
      real, parameter :: xnreal_ws(nspint_sw) = (/ 1.443, 1.420, 1.420, 1.420, 1.463, 1.510, 1.510,   &
                                                   1.520, 1.530, 1.530, 1.530, 1.530, 1.530, 1.710 /)
      real, parameter :: xnimag_ws(nspint_sw) = (/ 5.718E-3, 1.777E-2, 1.060E-2, 8.368E-3, 1.621E-2,  &
                                                   2.198E-2, 1.929E-2, 1.564E-2, 7.000E-3, 5.666E-3,  &
                                                   5.000E-3, 8.440E-3, 3.000E-2, 1.100E-1 /)

! FSB sea salt      
      real, parameter :: xnreal_ss(nspint_sw) = (/ 1.480, 1.534, 1.437, 1.448, 1.450, 1.462, 1.469,   &
                                                   1.470, 1.490, 1.500, 1.502, 1.510, 1.510, 1.510 /)

      real, parameter :: xnimag_ss(nspint_sw) = (/ 1.758E-3, 7.462E-3, 2.950E-3, 1.276E-3, 7.944E-4,  &
                                                   5.382E-4, 3.754E-4, 1.498E-4, 2.050E-7, 1.184E-8,  &
                                                   9.938E-8, 2.060E-6, 5.000E-6, 1.000E-2 /)

! FSB insoluble      
      real, parameter :: xnreal_in(nspint_sw) = (/ 1.272, 1.168, 1.208, 1.253, 1.329, 1.418, 1.456,   &
                                                   1.518, 1.530, 1.530, 1.530, 1.530, 1.530, 1.470 /)
      real, parameter :: xnimag_in(nspint_sw) = (/ 1.165E-2, 1.073E-2, 8.650E-3, 8.092E-3, 8.000E-3,  &
                                                   8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3, 8.000E-3,  &
                                                   8.000E-3, 8.440E-3, 3.000E-2,9.000E-2 /)

! FSB 02/11/2012 These values are replaced.
!      data xnreal_ec /1.877, 1.832, 1.813, 1.802, 1.791, 1.769, 1.761,   &
!                      1.760, 1.750, 1.740, 1.750, 1.738, 1.620, 2.120/
!      data xnimag_ec/ 5.563E-1, 5.273E-1, 5.030E-1, 4.918E-1, 4.814E-1,  &
!                      4.585E-1, 4.508E-1, 4.404E-1, 4.300E-1, 4.400E-1,  &
!                      4.600E-1, 4.696E-1, 4.500E-1, 5.700E-1/

!    New  Refractive indices for EC at RRTMG Wavelengths
!    	Source	lamda	xnreal_ec	xnimag_ec
!   	C&C Values
!          3.4615	2.089	1.070
!    		2.7885	2.014	0.939
!   		2.325	1.962	0.843
!    		2.046	1.950	0.784
!   	Bond values	
!           1.784	1.940	0.760
!     		1.4625	1.930	0.749
!		    1.2705	1.905	0.737
!		    1.0101	1.870	0.726
!  B&B  Values	
!          0.7016	1.85	0.71
!   	   0.53325  1.85    0.71
!		   0.38815  1.85	0.71
!		   0.299	1.85	0.71
!		   0.2316	1.85	0.71
!	 C & C values	
!     	   8.24	    2.589	1.771
!References:	
! Bond  Personal Communication from Tami Bond
! B&B Bond, T.C. & R.W. Bergstrom (2006) Light absorption by 
! Carbonaceous Particles: An investigative review,
! Aerosol Science and Technology. Vol. 40. pp 27-67
!
! C&C Chang,H and T.T. Charalmpopoulos (1990) Determination of the
! wavelength dependence of refractive indices of flame soot, 
! Proceeding of the Royal Society of London A, Vol. 430, pp 577-591.
! FSB new values

! FSB elemental carbon - soot like   

      real, parameter :: xnreal_ec(nspint_sw) = (/ 2.089, 2.014, 1.962, 1.950, 1.940, 1.930, 1.905,   &
                                                   1.870, 1.85,  1.85,  1.85,  1.85,  1.85,  2.589 /)
      real, parameter :: xnimag_ec(nspint_sw) = (/ 1.070, 0.939, 0.843, 0.784, 0.760, 0.749, 0.737,   &
                                                   0.726, 0.71,  0.71,  0.71,  0.71,  0.71,  1.771 /)	

! FSB water
      real, save :: xnreal_h2o(nspint_sw) = (/ 1.408, 1.324, 1.277, 1.302, 1.312, 1.321, 1.323,       &
                                               1.327, 1.331, 1.334, 1.340, 1.349, 1.362, 1.260 /)
      real, save :: xnimag_h2o(nspint_sw) = (/ 1.420E-2, 1.577E-1, 1.516E-3, 1.159E-3, 2.360E-4,      &
                                               1.713E-4, 2.425E-5, 3.125E-6, 3.405E-8, 1.639E-9,      &
                                               2.955E-9, 1.635E-8, 3.350E-8, 6.220E-2 /)
      

! FSB Begin code ======================================================
      
      bextsum  = 0.0
      bscatsum = 0.0
      bsgsum   = 0.0
      do m = 1, nmode
! FSB calculate volumes [ m**3 / m**3 ]
! FSB the reciprocal densities have been scaled to [ m**3 / ug ]      

         MVOL_ws(m)  = rhows1  * INMASS_ws(m)
         MVOL_in(m)  = rhoin1  * INMASS_in(m)
         MVOL_ec(m)  = rhoec1  * INMASS_ec(m)
         MVOL_ss(m)  = rhoss1  * INMASS_ss(m)
         MVOL_h2o(m) = rhoh2o1 * INMASS_h2o(m)
      
         VOLSHELL(m) = MVOL_ws(m) + MVOL_in(m) + MVOL_ss(m) + MVOL_h2o(m)
         VOLCOR(m)   = MVOL_ec(m)
!        VOL(m)      = VOLSHELL(m) + VOLCOR(m) ! VOL is total volume

         if ( VOLCOR(m) .gt. 0.0 ) then
! FSB EC is present  
!       calculate the ratio of core to shell volume
!       take cube root for scaling the diameter of 
!       the core to that of the shell.
       
!           dfac        = ( VOLCOR(m) / VOL(m) ) ** one3rd
            dfac        = ( VOLCOR(m) / ( VOLSHELL(m) + VOLCOR(m) ) ) ** one3rd
!           dfac        = ( VOLCOR(m) / ( VOLSHELL(m) + VOLCOR(m) ) )
! FSB Set shell and core diameters        
            DGNSHELL(m) = INDGN(m)
            DGNCORE(M)  = dfac * INDGN(m)
! FSB note that VOLSHELL(m) is the total volume when EC is not present
         end if
        
! internal mixture of non-EC species.

! modal real refractive index No EC 
         nr(m) = (MVOL_ws(m)  * xnreal_ws(ns) +              &
                  MVOL_in(m)  * xnreal_in(ns) +              &
                  MVOL_ss(m)  * xnreal_ss(ns) +              &
!                 MVOL_h2o(m) * xnreal_h2o(ns)) / VOL(m)
                  MVOL_h2o(m) * xnreal_h2o(ns)) / VOLSHELL(m)

! modal imaginary refractive index no EC
         ni(m) = (MVOL_ws(m)  * xnimag_ws(ns) +              &
                  MVOL_in(m)  * xnimag_in(ns) +              &
                  MVOL_ss(m)  * xnimag_ss(ns) +              &
!                 MVOL_h2o(m) * xnimag_h2o(ns)) / VOL(m)
                  MVOL_h2o(m) * xnimag_h2o(ns)) / VOLSHELL(m)

         if ( VOLCOR(m) .gt. 0.0) then

! FSB calculate the complex refractive indices for the CORE and
!     the SHELL for case when and EC core is assumed to exist.

            refcor(m)   = cmplx( xnreal_ec(ns), xnimag_ec(ns) ) 
            refshell(m) = cmplx(nr(m), ni(m) )
! FSB do BHCOAT case        
            CALL aero_optical_CS( LAMDA_SW(ns), refcor(m), refshell(m),   &
                                  VOLCOR(m),VOLSHELL(m), DGNCORE(m),      &
                                  DGNSHELL(m), INSIG(m),                  &                            
                                  bext, bscat, gfac, succesS )
!        else if ( VOL(m) .gt. 0.0) then
         else if ( VOLSHELL(m) .gt. 0.0) then
! FSB do BHMIE case for the case when EC is not present.
            crefin(m) = cmplx(nr(m), ni(m) )
!           CALL aero_optical2( LAMDA_SW(ns), crefin(m), VOL(m),          &
            CALL aero_optical2( LAMDA_SW(ns), crefin(m), VOLSHELL(m),     &
                                INDGN(m), INSIG(m),                       &
                                bext, bscat, gfac, success )
         else
            bext = 0.0
            bscat = 0.0
            gfac = 0.0
         end if       

! FSB sum for total values
         bextsum  = bextsum + bext
         bscatsum = bscatsum +bscat              
         bsgsum   = bsgsum + bscat * gfac
! FSB get history 
!        bext_wm(ns,m)  = bext
!        bscat_wm(ns,m) = bscat
!        gfac_wm(ns,m)  = gfac          
      end do ! loop over modes
           
! FSB construct output variables      
      tauaer = bextsum * delta_z
      waer   = bscatsum / bextsum      
      gaer   = bsgsum / bscatsum

!     Write out modal values.

!     write(100,*) ' lamda mode bext bscat gaer '
!     m=1
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 
!     m=2
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 
!     m=3
!     write(100,*) lamda(ns), m, bext_wm(ns,m),bscat_wm(ns,m), gfac_wm(ns,m) 

      end subroutine get_aerosol_Optics_RRTMG_SW

END MODULE module_twoway_ra_rrtmg_sw
