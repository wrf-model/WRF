module module_gfs_constants
  use module_model_constants, only:c_p=>cp,g,r_d,xlv,xlf,rovg,ep_1,r_v, &
                                   cliq,cice,psat

  IMPLICIT NONE
  SAVE

! Machine dependant constants

  integer kind_io4,kind_io8,kind_phys,kind_rad
  parameter (kind_rad = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
  parameter (kind_phys = selected_real_kind(13,60)) ! the '60' maps to 64-bit real
  parameter (kind_io4 = 4)
  parameter (kind_io8 = 8)


!Math constants
!  real(kind=kind_phys),parameter:: con_pi      =3.1415926535897931 ! pi
!  real(kind=kind_phys),parameter:: con_sqrt2   =1.414214e+0 ! square root of 2
!  real(kind=kind_phys),parameter:: con_sqrt3   =1.732051e+0 ! square root of 3
!Primary constants
!  real(kind=kind_phys),parameter:: con_rerth   =6.3712e+6 ! radius of earth     (m)
  real(kind=kind_phys),parameter:: con_g        = g        ! gravity             (m/s2)
!  real(kind=kind_phys),parameter:: con_omega   =7.2921e-5 ! ang vel of earth    (1/s)
  real(kind=kind_phys),parameter:: con_rd       = r_d      ! gas constant air    (J/kg/K)
  real(kind=kind_phys),parameter:: con_rv       = r_v      ! gas constant H2O    (J/kg/K)
  real(kind=kind_phys),parameter:: con_cp       = c_p      ! spec heat air @p    (J/kg/K)
!  real(kind=kind_phys),parameter:: con_cv      =7.1760e+2 ! spec heat air @v    (J/kg/K)
  real(kind=kind_phys),parameter:: con_cvap     =1.8460e+3 ! spec heat H2O gas   (J/kg/K)
  real(kind=kind_phys),parameter:: con_cliq     = cliq     ! spec heat H2O liq   (J/kg/K)
  real(kind=kind_phys),parameter:: con_csol     = cice     ! spec heat H2O ice   (J/kg/K)
  real(kind=kind_phys),parameter:: con_hvap     = xlv      ! lat heat H2O cond   (J/kg)
  real(kind=kind_phys),parameter:: con_hfus     = xlf      ! lat heat H2O fusion (J/kg)
  real(kind=kind_phys),parameter:: con_psat     = psat     ! pres at H2O 3pt     (Pa)  
!  real(kind=kind_phys),parameter:: con_sbc     =5.6730e-8 ! stefan-boltzmann    (W/m2/K4)
!  real(kind=kind_phys),parameter:: con_solr    =1.3533e+3 ! solar constant      (W/m2)
  real(kind=kind_phys),parameter:: con_t0c      =2.7315e+2 ! temp at 0C          (K)
  real(kind=kind_phys),parameter:: con_ttp      =2.7316e+2  ! temp at H2O 3pt     (K)
!  real(kind=kind_phys),parameter:: con_jcal    =4.1855E+0 ! JOULES PER CALORIE  ()
!Secondary constants
!  real(kind=kind_phys),parameter:: con_rocp    =con_rd/con_cp
!  real(kind=kind_phys),parameter:: con_cpor    =con_cp/con_rd
  real(kind=kind_phys),parameter:: con_rog      = rovg
  real(kind=kind_phys),parameter:: con_fvirt    = ep_1
  real(kind=kind_phys),parameter:: con_eps      =con_rd/con_rv
  real(kind=kind_phys),parameter:: con_epsm1    =con_rd/con_rv-1.
!  real(kind=kind_phys),parameter:: con_dldt    =con_cvap-con_cliq
!  real(kind=kind_phys),parameter:: con_xpona   =-con_dldt/con_rv
!  real(kind=kind_phys),parameter:: con_xponb   =-con_dldt/con_rv+con_hvap/(con_rv*con_ttp)

! ---------------------------------------------------------------------------------------------
!                     GFS FUNCTIONS/SUBROUTINES

!   fpvs            Elementally compute saturation vapor pressure
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!                                                          
!   gpvs            Compute saturation vapor pressure table

!    private
    integer,public,parameter:: krealfp=kind_phys
    integer,parameter:: nxpvs=7501
    real(krealfp) c1xpvs,c2xpvs,tbpvs(nxpvs)
    public gpvs,fpvs
    contains

!-------------------------------------------------------------------------------
!  elemental function fpvs(t)
  function fpvs(t)

!$$$     Subprogram Documentation Block
!
! Subprogram: fpvs         Compute saturation vapor pressure
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A linear interpolation is done between values in a lookup table
!   computed in gpvs. See documentation for fpvsx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 6 decimal places.
!   On the Cray, fpvs is about 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvs=fpvs(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvs       Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvs
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj

! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp)) 
    jx=min(xj,nxpvs-1._krealfp)
    fpvs=tbpvs(jx)+(xj-jx)*(tbpvs(jx+1)-tbpvs(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function fpvs
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  elemental function fpvsx(t)
!  function fpvsx(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsx        Compute saturation vapor pressure
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute saturation vapor pressure from temperature.
!   The saturation vapor pressure over either liquid and ice is computed
!   over liquid for temperatures above the triple point,
!   over ice for temperatures 20 degress below the triple point,
!   and a linear combination of the two for temperatures in between.
!   The water model assumes a perfect gas, constant specific heats
!   for gas, liquid and ice, and neglects the volume of the condensate.
!   The model does account for the variation of the latent heat
!   of condensation and sublimation with temperature.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsl=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   The reference for this computation is Emanuel(1994), pages 116-117.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvs=fpvsx(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsx      Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: tliq=con_ttp
    real(krealfp),parameter:: tice=con_ttp-20.0
    real(krealfp),parameter:: dldtl=con_cvap-con_cliq
    real(krealfp),parameter:: heatl=con_hvap
    real(krealfp),parameter:: xponal=-dldtl/con_rv
    real(krealfp),parameter:: xponbl=-dldtl/con_rv+heatl/(con_rv*con_ttp)
    real(krealfp),parameter:: dldti=con_cvap-con_csol
    real(krealfp),parameter:: heati=con_hvap+con_hfus
    real(krealfp),parameter:: xponai=-dldti/con_rv
    real(krealfp),parameter:: xponbi=-dldti/con_rv+heati/(con_rv*con_ttp)
    real(krealfp) tr,w,pvl,pvi
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    tr=con_ttp/t
    if(t.ge.tliq) then
      fpvsx=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
    elseif(t.lt.tice) then
      fpvsx=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
    else
      w=(t-tice)/(tliq-tice)
      pvl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
      pvi=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
      fpvsx=w*pvl+(1.-w)*pvi
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function fpvsx
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
  subroutine gpvs
!$$$     Subprogram Documentation Block
!
! Subprogram: gpvs         Compute saturation vapor pressure table
!   Author: N Phillips            W/NMC2X2   Date: 30 dec 82
!
! Abstract: Computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvs.
!   Exact saturation vapor pressures are calculated in subprogram fpvsx.
!   The current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:  call gpvs
!
! Subprograms called:
!   (fpvsx)    inlinable function to compute saturation vapor pressure
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvs-1)
!   c1xpvs=1.-xmin/xinc
    c2xpvs=1./xinc
    c1xpvs=1.-xmin*c2xpvs
    do jx=1,nxpvs
      x=xmin+(jx-1)*xinc
      t=x
      tbpvs(jx)=fpvsx(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine gpvs
!-------------------------------------------------------------------------------


end module module_gfs_constants
