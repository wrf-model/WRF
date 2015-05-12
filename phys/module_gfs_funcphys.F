!-------------------------------------------------------------------------------
module module_gfs_funcphys
!$$$  Module Documentation Block
!
! Module:    funcphys        API for basic thermodynamic physics
!   Author: Iredell          Org: W/NX23     Date: 1999-03-01
!
! Abstract: This module provides an Application Program Interface
!   for computing basic thermodynamic physics functions, in particular
!     (1) saturation vapor pressure as a function of temperature,
!     (2) dewpoint temperature as a function of vapor pressure,
!     (3) equivalent potential temperature as a function of temperature
!         and scaled pressure to the kappa power,
!     (4) temperature and specific humidity along a moist adiabat
!         as functions of equivalent potential temperature and
!         scaled pressure to the kappa power,
!     (5) scaled pressure to the kappa power as a function of pressure, and
!     (6) temperature at the lifting condensation level as a function
!         of temperature and dewpoint depression.
!   The entry points required to set up lookup tables start with a "g".
!   All the other entry points are functions starting with an "f" or
!   are subroutines starting with an "s".  These other functions and
!   subroutines are elemental; that is, they return a scalar if they
!   are passed only scalars, but they return an array if they are passed
!   an array.  These other functions and subroutines can be inlined, too.
!   
! Program History Log:
!   1999-03-01  Mark Iredell
!   1999-10-15  Mark Iredell  SI unit for pressure (Pascals)
!   2001-02-26  Mark Iredell  Ice phase changes of Hong and Moorthi
!
! Public Variables:
!   krealfp         Integer parameter kind or length of reals (=kind_phys)
!
! Public Subprograms:
!   gpvsl            Compute saturation vapor pressure over liquid table
!
!   fpvsl           Elementally compute saturation vapor pressure over liquid
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvslq          Elementally compute saturation vapor pressure over liquid
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvslx          Elementally compute saturation vapor pressure over liquid
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   gpvsi            Compute saturation vapor pressure over ice table
!
!   fpvsi           Elementally compute saturation vapor pressure over ice
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvsiq          Elementally compute saturation vapor pressure over ice
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvsix          Elementally compute saturation vapor pressure over ice
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   gpvs            Compute saturation vapor pressure table
!
!   fpvs            Elementally compute saturation vapor pressure
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvsq           Elementally compute saturation vapor pressure
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   fpvsx           Elementally compute saturation vapor pressure
!     function result Real(krealfp) saturation vapor pressure in Pascals
!     t               Real(krealfp) temperature in Kelvin
!
!   gtdpl           Compute dewpoint temperature over liquid table
!
!   ftdpl           Elementally compute dewpoint temperature over liquid
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdplq          Elementally compute dewpoint temperature over liquid
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdplx          Elementally compute dewpoint temperature over liquid
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdplxg         Elementally compute dewpoint temperature over liquid
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     t               Real(krealfp) guess dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   gtdpi           Compute dewpoint temperature table over ice
!
!   ftdpi           Elementally compute dewpoint temperature over ice
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpiq          Elementally compute dewpoint temperature over ice
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpix          Elementally compute dewpoint temperature over ice
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpixg         Elementally compute dewpoint temperature over ice
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     t               Real(krealfp) guess dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   gtdp            Compute dewpoint temperature table
!
!   ftdp            Elementally compute dewpoint temperature
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpq           Elementally compute dewpoint temperature
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpx           Elementally compute dewpoint temperature
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   ftdpxg          Elementally compute dewpoint temperature
!     function result Real(krealfp) dewpoint temperature in Kelvin
!     t               Real(krealfp) guess dewpoint temperature in Kelvin
!     pv              Real(krealfp) vapor pressure in Pascals
!
!   gthe            Compute equivalent potential temperature table
!
!   fthe            Elementally compute equivalent potential temperature
!     function result Real(krealfp) equivalent potential temperature in Kelvin
!     t               Real(krealfp) LCL temperature in Kelvin
!     pk              Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   ftheq           Elementally compute equivalent potential temperature
!     function result Real(krealfp) equivalent potential temperature in Kelvin
!     t               Real(krealfp) LCL temperature in Kelvin
!     pk              Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   fthex           Elementally compute equivalent potential temperature
!     function result Real(krealfp) equivalent potential temperature in Kelvin
!     t               Real(krealfp) LCL temperature in Kelvin
!     pk              Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   gtma            Compute moist adiabat tables
!
!   stma            Elementally compute moist adiabat temperature and moisture
!     the             Real(krealfp) equivalent potential temperature in Kelvin
!     pk              Real(krealfp) pressure over 1e5 Pa to the kappa power
!     tma             Real(krealfp) parcel temperature in Kelvin
!     qma             Real(krealfp) parcel specific humidity in kg/kg
!
!   stmaq           Elementally compute moist adiabat temperature and moisture
!     the             Real(krealfp) equivalent potential temperature in Kelvin
!     pk              Real(krealfp) pressure over 1e5 Pa to the kappa power
!     tma             Real(krealfp) parcel temperature in Kelvin
!     qma             Real(krealfp) parcel specific humidity in kg/kg
!
!   stmax           Elementally compute moist adiabat temperature and moisture
!     the             Real(krealfp) equivalent potential temperature in Kelvin
!     pk              Real(krealfp) pressure over 1e5 Pa to the kappa power
!     tma             Real(krealfp) parcel temperature in Kelvin
!     qma             Real(krealfp) parcel specific humidity in kg/kg
!
!   stmaxg          Elementally compute moist adiabat temperature and moisture
!     tg              Real(krealfp) guess parcel temperature in Kelvin
!     the             Real(krealfp) equivalent potential temperature in Kelvin
!     pk              Real(krealfp) pressure over 1e5 Pa to the kappa power
!     tma             Real(krealfp) parcel temperature in Kelvin
!     qma             Real(krealfp) parcel specific humidity in kg/kg
!
!   gpkap           Compute pressure to the kappa table
!
!   fpkap           Elementally raise pressure to the kappa power.
!     function result Real(krealfp) p over 1e5 Pa to the kappa power
!     p               Real(krealfp) pressure in Pascals
!
!   fpkapq          Elementally raise pressure to the kappa power.
!     function result Real(krealfp) p over 1e5 Pa to the kappa power
!     p               Real(krealfp) pressure in Pascals
!
!   fpkapo          Elementally raise pressure to the kappa power.
!     function result Real(krealfp) p over 1e5 Pa to the kappa power
!     p               Real(krealfp) surface pressure in Pascals
!
!   fpkapx          Elementally raise pressure to the kappa power.
!     function result Real(krealfp) p over 1e5 Pa to the kappa power
!     p               Real(krealfp) pressure in Pascals
!
!   grkap           Compute pressure to the 1/kappa table
!
!   frkap           Elementally raise pressure to the 1/kappa power.
!     function result Real(krealfp) pressure in Pascals
!     pkap            Real(krealfp) p over 1e5 Pa to the 1/kappa power
!
!   frkapq          Elementally raise pressure to the kappa power.
!     function result Real(krealfp) pressure in Pascals
!     pkap            Real(krealfp) p over 1e5 Pa to the kappa power
!
!   frkapx          Elementally raise pressure to the kappa power.
!     function result Real(krealfp) pressure in Pascals
!     pkap            Real(krealfp) p over 1e5 Pa to the kappa power
!
!   gtlcl           Compute LCL temperature table
!
!   ftlcl           Elementally compute LCL temperature.
!     function result Real(krealfp) temperature at the LCL in Kelvin
!     t               Real(krealfp) temperature in Kelvin
!     tdpd            Real(krealfp) dewpoint depression in Kelvin
!
!   ftlclq          Elementally compute LCL temperature.
!     function result Real(krealfp) temperature at the LCL in Kelvin
!     t               Real(krealfp) temperature in Kelvin
!     tdpd            Real(krealfp) dewpoint depression in Kelvin
!
!   ftlclo          Elementally compute LCL temperature.
!     function result Real(krealfp) temperature at the LCL in Kelvin
!     t               Real(krealfp) temperature in Kelvin
!     tdpd            Real(krealfp) dewpoint depression in Kelvin
!
!   ftlclx          Elementally compute LCL temperature.
!     function result Real(krealfp) temperature at the LCL in Kelvin
!     t               Real(krealfp) temperature in Kelvin
!     tdpd            Real(krealfp) dewpoint depression in Kelvin
!
!   gfuncphys       Compute all physics function tables
!
! Attributes:
!   Language: Fortran 90
!
!$$$
  use module_gfs_machine,only:kind_phys
  use module_gfs_physcons
  implicit none
  private
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Variables
! integer,public,parameter:: krealfp=selected_real_kind(15,45)
  integer,public,parameter:: krealfp=kind_phys
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Private Variables
  real(krealfp),parameter:: psatb=con_psat*1.e-5
  integer,parameter:: nxpvsl=7501
  real(krealfp) c1xpvsl,c2xpvsl,tbpvsl(nxpvsl)
  integer,parameter:: nxpvsi=7501
  real(krealfp) c1xpvsi,c2xpvsi,tbpvsi(nxpvsi)
  integer,parameter:: nxpvs=7501
  real(krealfp) c1xpvs,c2xpvs,tbpvs(nxpvs)
  integer,parameter:: nxtdpl=5001
  real(krealfp) c1xtdpl,c2xtdpl,tbtdpl(nxtdpl)
  integer,parameter:: nxtdpi=5001
  real(krealfp) c1xtdpi,c2xtdpi,tbtdpi(nxtdpi)
  integer,parameter:: nxtdp=5001
  real(krealfp) c1xtdp,c2xtdp,tbtdp(nxtdp)
  integer,parameter:: nxthe=241,nythe=151
  real(krealfp) c1xthe,c2xthe,c1ythe,c2ythe,tbthe(nxthe,nythe)
  integer,parameter:: nxma=151,nyma=121
  real(krealfp) c1xma,c2xma,c1yma,c2yma,tbtma(nxma,nyma),tbqma(nxma,nyma)
! integer,parameter:: nxpkap=5501
  integer,parameter:: nxpkap=11001
  real(krealfp) c1xpkap,c2xpkap,tbpkap(nxpkap)
  integer,parameter:: nxrkap=5501
  real(krealfp) c1xrkap,c2xrkap,tbrkap(nxrkap)
  integer,parameter:: nxtlcl=151,nytlcl=61
  real(krealfp) c1xtlcl,c2xtlcl,c1ytlcl,c2ytlcl,tbtlcl(nxtlcl,nytlcl)
  logical, private :: initialized=.false.
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Public Subprograms
  public gpvsl,fpvsl,fpvslq,fpvslx
  public gpvsi,fpvsi,fpvsiq,fpvsix
  public gpvs,fpvs,fpvsq,fpvsx
  public gtdpl,ftdpl,ftdplq,ftdplx,ftdplxg
  public gtdpi,ftdpi,ftdpiq,ftdpix,ftdpixg
  public gtdp,ftdp,ftdpq,ftdpx,ftdpxg
  public gthe,fthe,ftheq,fthex
  public gtma,stma,stmaq,stmax,stmaxg
  public gpkap,fpkap,fpkapq,fpkapo,fpkapx
  public grkap,frkap,frkapq,frkapx
  public gtlcl,ftlcl,ftlclq,ftlclo,ftlclx
  public gfuncphys
contains
!-------------------------------------------------------------------------------
  subroutine gpvsl
!$$$     Subprogram Documentation Block
!
! Subprogram: gpvsl        Compute saturation vapor pressure table over liquid
!   Author: N Phillips            W/NMC2X2   Date: 30 dec 82
!
! Abstract: Computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvsl.
!   Exact saturation vapor pressures are calculated in subprogram fpvslx.
!   The current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gpvsl
!
! Subprograms called:
!   (fpvslx)   inlinable function to compute saturation vapor pressure
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
    xinc=(xmax-xmin)/(nxpvsl-1)
!   c1xpvsl=1.-xmin/xinc
    c2xpvsl=1./xinc
    c1xpvsl=1.-xmin*c2xpvsl
    do jx=1,nxpvsl
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsl(jx)=fpvslx(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function fpvsl(t)
  function fpvsl(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsl        Compute saturation vapor pressure over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A linear interpolation is done between values in a lookup table
!   computed in gpvsl. See documentation for fpvslx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 6 decimal places.
!   On the Cray, fpvsl is about 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:   pvsl=fpvsl(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsl      Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsl
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(xj,nxpvsl-1._krealfp)
    fpvsl=tbpvsl(jx)+(xj-jx)*(tbpvsl(jx+1)-tbpvsl(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvslq(t)
  function fpvslq(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvslq       Compute saturation vapor pressure over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gpvsl. See documentation for fpvslx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 9 decimal places.
!   On the Cray, fpvslq is about 3 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
!
! Usage:   pvsl=fpvslq(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvslq     Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvslq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvsl+c2xpvsl*t,1._krealfp),real(nxpvsl,krealfp))
    jx=min(max(nint(xj),2),nxpvsl-1)
    dxj=xj-jx
    fj1=tbpvsl(jx-1)
    fj2=tbpvsl(jx)
    fj3=tbpvsl(jx+1)
    fpvslq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvslx(t)
  function fpvslx(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvslx       Compute saturation vapor pressure over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute saturation vapor pressure from temperature.
!   The water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   The model does account for the variation of the latent heat
!   of condensation with temperature.  The ice option is not included.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsl=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:   pvsl=fpvslx(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvslx     Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvslx
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tr=con_ttp/t
    fpvslx=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gpvsi
!$$$     Subprogram Documentation Block
!
! Subprogram: gpvsi        Compute saturation vapor pressure table over ice
!   Author: N Phillips            W/NMC2X2   Date: 30 dec 82
!
! Abstract: Computes saturation vapor pressure table as a function of
!   temperature for the table lookup function fpvsi.
!   Exact saturation vapor pressures are calculated in subprogram fpvsix.
!   The current implementation computes a table with a length
!   of 7501 for temperatures ranging from 180. to 330. Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:  call gpvsi
!
! Subprograms called:
!   (fpvsix)   inlinable function to compute saturation vapor pressure
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xmin=180.0_krealfp
    xmax=330.0_krealfp
    xinc=(xmax-xmin)/(nxpvsi-1)
!   c1xpvsi=1.-xmin/xinc
    c2xpvsi=1./xinc
    c1xpvsi=1.-xmin*c2xpvsi
    do jx=1,nxpvsi
      x=xmin+(jx-1)*xinc
      t=x
      tbpvsi(jx)=fpvsix(t)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function fpvsi(t)
  function fpvsi(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsi        Compute saturation vapor pressure over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A linear interpolation is done between values in a lookup table
!   computed in gpvsi. See documentation for fpvsix for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 6 decimal places.
!   On the Cray, fpvsi is about 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvsi=fpvsi(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsi      Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsi
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(xj,nxpvsi-1._krealfp)
    fpvsi=tbpvsi(jx)+(xj-jx)*(tbpvsi(jx+1)-tbpvsi(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvsiq(t)
  function fpvsiq(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsiq       Compute saturation vapor pressure over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gpvsi. See documentation for fpvsix for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 9 decimal places.
!   On the Cray, fpvsiq is about 3 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvsi=fpvsiq(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsiq     Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsiq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvsi+c2xpvsi*t,1._krealfp),real(nxpvsi,krealfp))
    jx=min(max(nint(xj),2),nxpvsi-1)
    dxj=xj-jx
    fj1=tbpvsi(jx-1)
    fj2=tbpvsi(jx)
    fj3=tbpvsi(jx+1)
    fpvsiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvsix(t)
  function fpvsix(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsix       Compute saturation vapor pressure over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute saturation vapor pressure from temperature.
!   The water model assumes a perfect gas, constant specific heats
!   for gas and ice, and neglects the volume of the ice.
!   The model does account for the variation of the latent heat
!   of condensation with temperature.  The liquid option is not included.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formula
!       pvsi=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvsi=fpvsix(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsix     Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsix
    real(krealfp),intent(in):: t
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) tr
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tr=con_ttp/t
    fpvsix=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
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
    if(.not.initialized) call gfuncphys()
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
  end subroutine
!-------------------------------------------------------------------------------
! elemental function fpvs(t)
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
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(xj,nxpvs-1._krealfp)
    fpvs=tbpvs(jx)+(xj-jx)*(tbpvs(jx+1)-tbpvs(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvsq(t)
  function fpvsq(t)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpvsq        Compute saturation vapor pressure
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute saturation vapor pressure from the temperature.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gpvs. See documentation for fpvsx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is almost 9 decimal places.
!   On the Cray, fpvsq is about 3 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   pvs=fpvsq(t)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!
!   Output argument list:
!     fpvsq      Real(krealfp) saturation vapor pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpvsq
    real(krealfp),intent(in):: t
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpvs+c2xpvs*t,1._krealfp),real(nxpvs,krealfp))
    jx=min(max(nint(xj),2),nxpvs-1)
    dxj=xj-jx
    fj1=tbpvs(jx-1)
    fj2=tbpvs(jx)
    fj3=tbpvs(jx+1)
    fpvsq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpvsx(t)
  function fpvsx(t)
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
    if(.not.initialized) call gfuncphys()
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
  end function
!-------------------------------------------------------------------------------
  subroutine gtdpl
!$$$     Subprogram Documentation Block
!
! Subprogram: gtdpl        Compute dewpoint temperature over liquid table
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdpl.
!   Exact dewpoint temperatures are calculated in subprogram ftdplxg.
!   The current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 1 to 10001 Pascals
!   giving a dewpoint temperature range of 208 to 319 Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gtdpl
!
! Subprograms called:
!   (ftdplxg)  inlinable function to compute dewpoint temperature over liquid
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=1
    xmax=10001
    xinc=(xmax-xmin)/(nxtdpl-1)
    c1xtdpl=1.-xmin/xinc
    c2xtdpl=1./xinc
    t=208.0
    do jx=1,nxtdpl
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdplxg(t,pv)
      tbtdpl(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function ftdpl(pv)
  function ftdpl(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpl        Compute dewpoint temperature over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A linear interpolation is done between values in a lookup table
!   computed in gtdpl. See documentation for ftdplxg for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.0005 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.02 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdpl is about 75 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:   tdpl=ftdpl(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpl      Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpl
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(xj,nxtdpl-1._krealfp)
    ftdpl=tbtdpl(jx)+(xj-jx)*(tbtdpl(jx+1)-tbtdpl(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdplq(pv)
  function ftdplq(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdplq       Compute dewpoint temperature over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gtdpl. see documentation for ftdplxg for details.
!   Input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.002 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdplq is about 60 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
!
! Usage:   tdpl=ftdplq(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdplq     Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdpl+c2xtdpl*pv,1._krealfp),real(nxtdpl,krealfp))
    jx=min(max(nint(xj),2),nxtdpl-1)
    dxj=xj-jx
    fj1=tbtdpl(jx-1)
    fj2=tbtdpl(jx)
    fj3=tbtdpl(jx+1)
    ftdplq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdplx(pv)
  function ftdplx(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdplx       Compute dewpoint temperature over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: exactly compute dewpoint temperature from vapor pressure.
!   An approximate dewpoint temperature for function ftdplxg
!   is obtained using ftdpl so gtdpl must be already called.
!   See documentation for ftdplxg for details.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:   tdpl=ftdplx(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdplx     Real(krealfp) dewpoint temperature in Kelvin
!
! Subprograms called:
!   (ftdpl)    inlinable function to compute dewpoint temperature over liquid
!   (ftdplxg)  inlinable function to compute dewpoint temperature over liquid
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplx
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tg=ftdpl(pv)
    ftdplx=ftdplxg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdplxg(tg,pv)
  function ftdplxg(tg,pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdplxg      Compute dewpoint temperature over liquid
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute dewpoint temperature from vapor pressure.
!   A guess dewpoint temperature must be provided.
!   The water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   The model does account for the variation of the latent heat
!   of condensation with temperature.  The ice option is not included.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formula
!       pvs=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   The formula is inverted by iterating Newtonian approximations
!   for each pvs until t is found to within 1.e-6 Kelvin.
!   This function can be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:   tdpl=ftdplxg(tg,pv)
!
!   Input argument list:
!     tg         Real(krealfp) guess dewpoint temperature in Kelvin
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdplxg    Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdplxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_cliq
    real(krealfp),parameter:: heat=con_hvap
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdplxg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtdpi
!$$$     Subprogram Documentation Block
!
! Subprogram: gtdpi        Compute dewpoint temperature over ice table
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdpi.
!   Exact dewpoint temperatures are calculated in subprogram ftdpixg.
!   The current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 0.1 to 1000.1 Pascals
!   giving a dewpoint temperature range of 197 to 279 Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:  call gtdpi
!
! Subprograms called:
!   (ftdpixg)  inlinable function to compute dewpoint temperature over ice
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0.1
    xmax=1000.1
    xinc=(xmax-xmin)/(nxtdpi-1)
    c1xtdpi=1.-xmin/xinc
    c2xtdpi=1./xinc
    t=197.0
    do jx=1,nxtdpi
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpixg(t,pv)
      tbtdpi(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function ftdpi(pv)
  function ftdpi(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpi        Compute dewpoint temperature over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A linear interpolation is done between values in a lookup table
!   computed in gtdpi. See documentation for ftdpixg for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.0005 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.02 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdpi is about 75 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdpi=ftdpi(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpi      Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpi
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(xj,nxtdpi-1._krealfp)
    ftdpi=tbtdpi(jx)+(xj-jx)*(tbtdpi(jx+1)-tbtdpi(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpiq(pv)
  function ftdpiq(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpiq       Compute dewpoint temperature over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gtdpi. see documentation for ftdpixg for details.
!   Input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.002 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdpiq is about 60 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdpi=ftdpiq(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpiq     Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpiq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdpi+c2xtdpi*pv,1._krealfp),real(nxtdpi,krealfp))
    jx=min(max(nint(xj),2),nxtdpi-1)
    dxj=xj-jx
    fj1=tbtdpi(jx-1)
    fj2=tbtdpi(jx)
    fj3=tbtdpi(jx+1)
    ftdpiq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpix(pv)
  function ftdpix(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpix       Compute dewpoint temperature over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: exactly compute dewpoint temperature from vapor pressure.
!   An approximate dewpoint temperature for function ftdpixg
!   is obtained using ftdpi so gtdpi must be already called.
!   See documentation for ftdpixg for details.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdpi=ftdpix(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpix     Real(krealfp) dewpoint temperature in Kelvin
!
! Subprograms called:
!   (ftdpi)    inlinable function to compute dewpoint temperature over ice
!   (ftdpixg)  inlinable function to compute dewpoint temperature over ice
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpix
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tg=ftdpi(pv)
    ftdpix=ftdpixg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpixg(tg,pv)
  function ftdpixg(tg,pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpixg      Compute dewpoint temperature over ice
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute dewpoint temperature from vapor pressure.
!   A guess dewpoint temperature must be provided.
!   The water model assumes a perfect gas, constant specific heats
!   for gas and ice, and neglects the volume of the ice.
!   The model does account for the variation of the latent heat
!   of sublimation with temperature.  The liquid option is not included.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formula
!       pvs=con_psat*(tr**xa)*exp(xb*(1.-tr))
!   where tr is ttp/t and other values are physical constants.
!   The formula is inverted by iterating Newtonian approximations
!   for each pvs until t is found to within 1.e-6 Kelvin.
!   This function can be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdpi=ftdpixg(tg,pv)
!
!   Input argument list:
!     tg         Real(krealfp) guess dewpoint temperature in Kelvin
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpixg    Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpixg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
    real(krealfp),parameter:: dldt=con_cvap-con_csol
    real(krealfp),parameter:: heat=con_hvap+con_hfus
    real(krealfp),parameter:: xpona=-dldt/con_rv
    real(krealfp),parameter:: xponb=-dldt/con_rv+heat/(con_rv*con_ttp)
    real(krealfp) t,tr,pvt,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    t=tg
    do i=1,100
      tr=con_ttp/t
      pvt=con_psat*(tr**xpona)*exp(xponb*(1.-tr))
      el=heat+dldt*(t-con_ttp)
      dpvt=el*pvt/(con_rv*t**2)
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpixg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtdp
!$$$     Subprogram Documentation Block
!
! Subprogram: gtdp         Compute dewpoint temperature table
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature table as a function of
!   vapor pressure for inlinable function ftdp.
!   Exact dewpoint temperatures are calculated in subprogram ftdpxg.
!   The current implementation computes a table with a length
!   of 5001 for vapor pressures ranging from 0.5 to 1000.5 Pascals
!   giving a dewpoint temperature range of 208 to 319 Kelvin.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:  call gtdp
!
! Subprograms called:
!   (ftdpxg)   inlinable function to compute dewpoint temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,t,x,pv
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0.5
    xmax=10000.5
    xinc=(xmax-xmin)/(nxtdp-1)
    c1xtdp=1.-xmin/xinc
    c2xtdp=1./xinc
    t=208.0
    do jx=1,nxtdp
      x=xmin+(jx-1)*xinc
      pv=x
      t=ftdpxg(t,pv)
      tbtdp(jx)=t
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function ftdp(pv)
  function ftdp(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdp         Compute dewpoint temperature 
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A linear interpolation is done between values in a lookup table
!   computed in gtdp. See documentation for ftdpxg for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.0005 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.02 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdp is about 75 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdp=ftdp(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdp       Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdp
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(xj,nxtdp-1._krealfp)
    ftdp=tbtdp(jx)+(xj-jx)*(tbtdp(jx+1)-tbtdp(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpq(pv)
  function ftdpq(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpq        Compute dewpoint temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute dewpoint temperature from vapor pressure.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gtdp. see documentation for ftdpxg for details.
!   Input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.00001 Kelvin
!   for dewpoint temperatures greater than 250 Kelvin,
!   but decreases to 0.002 Kelvin for a dewpoint around 230 Kelvin.
!   On the Cray, ftdpq is about 60 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdp=ftdpq(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpq      Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpq
    real(krealfp),intent(in):: pv
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtdp+c2xtdp*pv,1._krealfp),real(nxtdp,krealfp))
    jx=min(max(nint(xj),2),nxtdp-1)
    dxj=xj-jx
    fj1=tbtdp(jx-1)
    fj2=tbtdp(jx)
    fj3=tbtdp(jx+1)
    ftdpq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpx(pv)
  function ftdpx(pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpx        Compute dewpoint temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: exactly compute dewpoint temperature from vapor pressure.
!   An approximate dewpoint temperature for function ftdpxg
!   is obtained using ftdp so gtdp must be already called.
!   See documentation for ftdpxg for details.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdp=ftdpx(pv)
!
!   Input argument list:
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpx      Real(krealfp) dewpoint temperature in Kelvin
!
! Subprograms called:
!   (ftdp)     inlinable function to compute dewpoint temperature
!   (ftdpxg)   inlinable function to compute dewpoint temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpx
    real(krealfp),intent(in):: pv
    real(krealfp) tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tg=ftdp(pv)
    ftdpx=ftdpxg(tg,pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftdpxg(tg,pv)
  function ftdpxg(tg,pv)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftdpxg       Compute dewpoint temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute dewpoint temperature from vapor pressure.
!   A guess dewpoint temperature must be provided.
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
!   The reference for this decision is Emanuel(1994), pages 116-117.
!   The formula is inverted by iterating Newtonian approximations
!   for each pvs until t is found to within 1.e-6 Kelvin.
!   This function can be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
! 2001-02-26  Iredell             ice phase
!
! Usage:   tdp=ftdpxg(tg,pv)
!
!   Input argument list:
!     tg         Real(krealfp) guess dewpoint temperature in Kelvin
!     pv         Real(krealfp) vapor pressure in Pascals
!
!   Output argument list:
!     ftdpxg     Real(krealfp) dewpoint temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftdpxg
    real(krealfp),intent(in):: tg,pv
    real(krealfp),parameter:: terrm=1.e-6
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
    real(krealfp) t,tr,w,pvtl,pvti,pvt,ell,eli,el,dpvt,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    t=tg
    do i=1,100
      tr=con_ttp/t
      if(t.ge.tliq) then
        pvt=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        el=heatl+dldtl*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      elseif(t.lt.tice) then
        pvt=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        el=heati+dldti*(t-con_ttp)
        dpvt=el*pvt/(con_rv*t**2)
      else
        w=(t-tice)/(tliq-tice)
        pvtl=con_psat*(tr**xponal)*exp(xponbl*(1.-tr))
        pvti=con_psat*(tr**xponai)*exp(xponbi*(1.-tr))
        pvt=w*pvtl+(1.-w)*pvti
        ell=heatl+dldtl*(t-con_ttp)
        eli=heati+dldti*(t-con_ttp)
        dpvt=(w*ell*pvtl+(1.-w)*eli*pvti)/(con_rv*t**2)
      endif
      terr=(pvt-pv)/dpvt
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    ftdpxg=t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gthe
!$$$     Subprogram Documentation Block
!
! Subprogram: gthe        Compute equivalent potential temperature table
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute equivalent potential temperature table
!   as a function of LCL temperature and pressure over 1e5 Pa
!   to the kappa power for function fthe.
!   Equivalent potential temperatures are calculated in subprogram fthex
!   the current implementation computes a table with a first dimension
!   of 241 for temperatures ranging from 183.16 to 303.16 Kelvin
!   and a second dimension of 151 for pressure over 1e5 Pa
!   to the kappa power ranging from 0.04**rocp to 1.10**rocp.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gthe
!
! Subprograms called:
!   (fthex)    inlinable function to compute equiv. pot. temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=con_ttp-90._krealfp
    xmax=con_ttp+30._krealfp
    ymin=0.04_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxthe-1)
    c1xthe=1.-xmin/xinc
    c2xthe=1./xinc
    yinc=(ymax-ymin)/(nythe-1)
    c1ythe=1.-ymin/yinc
    c2ythe=1./yinc
    do jy=1,nythe
      y=ymin+(jy-1)*yinc
      pk=y
      do jx=1,nxthe
        x=xmin+(jx-1)*xinc
        t=x
        tbthe(jx,jy)=fthex(t,pk)
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function fthe(t,pk)
  function fthe(t,pk)
!$$$     Subprogram Documentation Block
!
! Subprogram: fthe         Compute equivalent potential temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute equivalent potential temperature at the LCL
!   from temperature and pressure over 1e5 Pa to the kappa power.
!   A bilinear interpolation is done between values in a lookup table
!   computed in gthe. see documentation for fthex for details.
!   Input values outside table range are reset to table extrema,
!   except zero is returned for too cold or high LCLs.
!   The interpolation accuracy is better than 0.01 Kelvin.
!   On the Cray, fthe is almost 6 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:   the=fthe(t,pk)
!
!   Input argument list:
!     t          Real(krealfp) LCL temperature in Kelvin
!     pk         Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     fthe       Real(krealfp) equivalent potential temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fthe
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(xj,nxthe-1._krealfp)
      jy=min(yj,nythe-1._krealfp)
      ftx1=tbthe(jx,jy)+(xj-jx)*(tbthe(jx+1,jy)-tbthe(jx,jy))
      ftx2=tbthe(jx,jy+1)+(xj-jx)*(tbthe(jx+1,jy+1)-tbthe(jx,jy+1))
      fthe=ftx1+(yj-jy)*(ftx2-ftx1)
    else
      fthe=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftheq(t,pk)
  function ftheq(t,pk)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftheq        Compute equivalent potential temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute equivalent potential temperature at the LCL
!   from temperature and pressure over 1e5 Pa to the kappa power.
!   A biquadratic interpolation is done between values in a lookup table
!   computed in gthe. see documentation for fthex for details.
!   Input values outside table range are reset to table extrema,
!   except zero is returned for too cold or high LCLs.
!   The interpolation accuracy is better than 0.0002 Kelvin.
!   On the Cray, ftheq is almost 3 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
!
! Usage:   the=ftheq(t,pk)
!
!   Input argument list:
!     t          Real(krealfp) LCL temperature in Kelvin
!     pk         Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     ftheq      Real(krealfp) equivalent potential temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftheq
    real(krealfp),intent(in):: t,pk
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(c1xthe+c2xthe*t,real(nxthe,krealfp))
    yj=min(c1ythe+c2ythe*pk,real(nythe,krealfp))
    if(xj.ge.1..and.yj.ge.1.) then
      jx=min(max(nint(xj),2),nxthe-1)
      jy=min(max(nint(yj),2),nythe-1)
      dxj=xj-jx
      dyj=yj-jy
      ft11=tbthe(jx-1,jy-1)
      ft12=tbthe(jx-1,jy)
      ft13=tbthe(jx-1,jy+1)
      ft21=tbthe(jx,jy-1)
      ft22=tbthe(jx,jy)
      ft23=tbthe(jx,jy+1)
      ft31=tbthe(jx+1,jy-1)
      ft32=tbthe(jx+1,jy)
      ft33=tbthe(jx+1,jy+1)
      ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
      ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
      ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
      ftheq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    else
      ftheq=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fthex(t,pk)
            function fthex(t,pk)
!$$$     Subprogram Documentation Block
!
! Subprogram: fthex        Compute equivalent potential temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute equivalent potential temperature at the LCL
!   from temperature and pressure over 1e5 Pa to the kappa power.
!   Equivalent potential temperature is constant for a saturated parcel
!   rising adiabatically up a moist adiabat when the heat and mass
!   of the condensed water are neglected.  Ice is also neglected.
!   The formula for equivalent potential temperature (Holton) is
!       the=t*(pd**(-rocp))*exp(el*eps*pv/(cp*t*pd))
!   where t is the temperature, pv is the saturated vapor pressure,
!   pd is the dry pressure p-pv, el is the temperature dependent
!   latent heat of condensation hvap+dldt*(t-ttp), and other values
!   are physical constants defined in parameter statements in the code.
!   Zero is returned if the input values make saturation impossible.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:   the=fthex(t,pk)
!
!   Input argument list:
!     t          Real(krealfp) LCL temperature in Kelvin
!     pk         Real(krealfp) LCL pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     fthex      Real(krealfp) equivalent potential temperature in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fthex
    real(krealfp),intent(in):: t,pk
    real(krealfp) p,tr,pv,pd,el,expo,expmax
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    p=pk**con_cpor
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    if(pd.gt.pv) then
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      fthex=t*pd**(-con_rocp)*exp(expo)
    else
      fthex=0.
    endif
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtma
!$$$     Subprogram Documentation Block
!
! Subprogram: gtma         Compute moist adiabat tables
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature and specific humidity tables
!   as a function of equivalent potential temperature and
!   pressure over 1e5 Pa to the kappa power for subprogram stma.
!   Exact parcel temperatures are calculated in subprogram stmaxg.
!   The current implementation computes a table with a first dimension
!   of 151 for equivalent potential temperatures ranging from 200 to 500
!   Kelvin and a second dimension of 121 for pressure over 1e5 Pa
!   to the kappa power ranging from 0.01**rocp to 1.10**rocp.
!
! Program History Log:
!   91-05-07  Iredell
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gtma
!
! Subprograms called:
!   (stmaxg)   inlinable subprogram to compute parcel temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,pk,the,t,q,tg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=200._krealfp
    xmax=500._krealfp
    ymin=0.01_krealfp**con_rocp
    ymax=1.10_krealfp**con_rocp
    xinc=(xmax-xmin)/(nxma-1)
    c1xma=1.-xmin/xinc
    c2xma=1./xinc
    yinc=(ymax-ymin)/(nyma-1)
    c1yma=1.-ymin/yinc
    c2yma=1./yinc
    do jy=1,nyma
      y=ymin+(jy-1)*yinc
      pk=y
      tg=xmin*y
      do jx=1,nxma
        x=xmin+(jx-1)*xinc
        the=x
        call stmaxg(tg,the,pk,t,q)
        tbtma(jx,jy)=t
        tbqma(jx,jy)=q
        tg=t
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental subroutine stma(the,pk,tma,qma)
  subroutine stma(the,pk,tma,qma)
!$$$     Subprogram Documentation Block
!
! Subprogram: stma         Compute moist adiabat temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature and specific humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the LCL and pressure over 1e5 Pa to the kappa power.
!   Bilinear interpolations are done between values in a lookup table
!   computed in gtma. See documentation for stmaxg for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.01 Kelvin
!   and 5.e-6 kg/kg for temperature and humidity, respectively.
!   On the Cray, stma is about 35 times faster than exact calculation.
!   This subprogram should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             expand table
! 1999-03-01  Iredell             f90 module
!
! Usage:  call stma(the,pk,tma,qma)
!
!   Input argument list:
!     the        Real(krealfp) equivalent potential temperature in Kelvin
!     pk         Real(krealfp) pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     tma        Real(krealfp) parcel temperature in Kelvin
!     qma        Real(krealfp) parcel specific humidity in kg/kg
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2,qx1,qx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(xj,nxma-1._krealfp)
    jy=min(yj,nyma-1._krealfp)
    ftx1=tbtma(jx,jy)+(xj-jx)*(tbtma(jx+1,jy)-tbtma(jx,jy))
    ftx2=tbtma(jx,jy+1)+(xj-jx)*(tbtma(jx+1,jy+1)-tbtma(jx,jy+1))
    tma=ftx1+(yj-jy)*(ftx2-ftx1)
    qx1=tbqma(jx,jy)+(xj-jx)*(tbqma(jx+1,jy)-tbqma(jx,jy))
    qx2=tbqma(jx,jy+1)+(xj-jx)*(tbqma(jx+1,jy+1)-tbqma(jx,jy+1))
    qma=qx1+(yj-jy)*(qx2-qx1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental subroutine stmaq(the,pk,tma,qma)
  subroutine stmaq(the,pk,tma,qma)
!$$$     Subprogram Documentation Block
!
! Subprogram: stmaq        Compute moist adiabat temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature and specific humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the LCL and pressure over 1e5 Pa to the kappa power.
!   Biquadratic interpolations are done between values in a lookup table
!   computed in gtma. See documentation for stmaxg for details.
!   Input values outside table range are reset to table extrema.
!   the interpolation accuracy is better than 0.0005 Kelvin
!   and 1.e-7 kg/kg for temperature and humidity, respectively.
!   On the Cray, stmaq is about 25 times faster than exact calculation.
!   This subprogram should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             quadratic interpolation
! 1999-03-01  Iredell             f90 module
!
! Usage:  call stmaq(the,pk,tma,qma)
!
!   Input argument list:
!     the        Real(krealfp) equivalent potential temperature in Kelvin
!     pk         Real(krealfp) pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     tmaq       Real(krealfp) parcel temperature in Kelvin
!     qma        Real(krealfp) parcel specific humidity in kg/kg
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
    real(krealfp) q11,q12,q13,q21,q22,q23,q31,q32,q33,qx1,qx2,qx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xma+c2xma*the,1._krealfp),real(nxma,krealfp))
    yj=min(max(c1yma+c2yma*pk,1._krealfp),real(nyma,krealfp))
    jx=min(max(nint(xj),2),nxma-1)
    jy=min(max(nint(yj),2),nyma-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtma(jx-1,jy-1)
    ft12=tbtma(jx-1,jy)
    ft13=tbtma(jx-1,jy+1)
    ft21=tbtma(jx,jy-1)
    ft22=tbtma(jx,jy)
    ft23=tbtma(jx,jy+1)
    ft31=tbtma(jx+1,jy-1)
    ft32=tbtma(jx+1,jy)
    ft33=tbtma(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    tma=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
    q11=tbqma(jx-1,jy-1)
    q12=tbqma(jx-1,jy)
    q13=tbqma(jx-1,jy+1)
    q21=tbqma(jx,jy-1)
    q22=tbqma(jx,jy)
    q23=tbqma(jx,jy+1)
    q31=tbqma(jx+1,jy-1)
    q32=tbqma(jx+1,jy)
    q33=tbqma(jx+1,jy+1)
    qx1=(((q31+q11)/2-q21)*dxj+(q31-q11)/2)*dxj+q21
    qx2=(((q32+q12)/2-q22)*dxj+(q32-q12)/2)*dxj+q22
    qx3=(((q33+q13)/2-q23)*dxj+(q33-q13)/2)*dxj+q23
    qma=(((qx3+qx1)/2-qx2)*dyj+(qx3-qx1)/2)*dyj+qx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental subroutine stmax(the,pk,tma,qma)
  subroutine stmax(the,pk,tma,qma)
!$$$     Subprogram Documentation Block
!
! Subprogram: stmax        Compute moist adiabat temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Exactly compute temperature and humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the LCL and pressure over 1e5 Pa to the kappa power.
!   An approximate parcel temperature for subprogram stmaxg
!   is obtained using stma so gtma must be already called.
!   See documentation for stmaxg for details.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:  call stmax(the,pk,tma,qma)
!
!   Input argument list:
!     the        Real(krealfp) equivalent potential temperature in Kelvin
!     pk         Real(krealfp) pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     tma        Real(krealfp) parcel temperature in Kelvin
!     qma        Real(krealfp) parcel specific humidity in kg/kg
!
! Subprograms called:
!   (stma)     inlinable subprogram to compute parcel temperature
!   (stmaxg)   inlinable subprogram to compute parcel temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp) tg,qg
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    call stma(the,pk,tg,qg)
    call stmaxg(tg,the,pk,tma,qma)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental subroutine stmaxg(tg,the,pk,tma,qma)
  subroutine stmaxg(tg,the,pk,tma,qma)
!$$$     Subprogram Documentation Block
!
! Subprogram: stmaxg       Compute moist adiabat temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: exactly compute temperature and humidity of a parcel
!   lifted up a moist adiabat from equivalent potential temperature
!   at the LCL and pressure over 1e5 Pa to the kappa power.
!   A guess parcel temperature must be provided.
!   Equivalent potential temperature is constant for a saturated parcel
!   rising adiabatically up a moist adiabat when the heat and mass
!   of the condensed water are neglected.  Ice is also neglected.
!   The formula for equivalent potential temperature (Holton) is
!       the=t*(pd**(-rocp))*exp(el*eps*pv/(cp*t*pd))
!   where t is the temperature, pv is the saturated vapor pressure,
!   pd is the dry pressure p-pv, el is the temperature dependent
!   latent heat of condensation hvap+dldt*(t-ttp), and other values
!   are physical constants defined in parameter statements in the code.
!   The formula is inverted by iterating Newtonian approximations
!   for each the and p until t is found to within 1.e-4 Kelvin.
!   The specific humidity is then computed from pv and pd.
!   This subprogram can be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             exact computation
! 1999-03-01  Iredell             f90 module
!
! Usage:  call stmaxg(tg,the,pk,tma,qma)
!
!   Input argument list:
!     tg         Real(krealfp) guess parcel temperature in Kelvin
!     the        Real(krealfp) equivalent potential temperature in Kelvin
!     pk         Real(krealfp) pressure over 1e5 Pa to the kappa power
!
!   Output argument list:
!     tma        Real(krealfp) parcel temperature in Kelvin
!     qma        Real(krealfp) parcel specific humidity in kg/kg
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp),intent(in):: tg,the,pk
    real(krealfp),intent(out):: tma,qma
    real(krealfp),parameter:: terrm=1.e-4
    real(krealfp) t,p,tr,pv,pd,el,expo,thet,dthet,terr
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    t=tg
    p=pk**con_cpor
    do i=1,100
      tr=con_ttp/t
      pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
      pd=p-pv
      el=con_hvap+con_dldt*(t-con_ttp)
      expo=el*con_eps*pv/(con_cp*t*pd)
      thet=t*pd**(-con_rocp)*exp(expo)
      dthet=thet/t*(1.+expo*(con_dldt*t/el+el*p/(con_rv*t*pd)))
      terr=(thet-the)/dthet
      t=t-terr
      if(abs(terr).le.terrm) exit
    enddo
    tma=t
    tr=con_ttp/t
    pv=psatb*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    pd=p-pv
    qma=con_eps*pv/(pd+con_eps*pv)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
  subroutine gpkap
!$$$   Subprogram  documentation  block
!
! Subprogram: gpkap        Compute coefficients for p**kappa
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: Computes pressure to the kappa table as a function of pressure
!   for the table lookup function fpkap.
!   Exact pressure to the kappa values are calculated in subprogram fpkapx.
!   The current implementation computes a table with a length
!   of 5501 for pressures ranging up to 110000 Pascals.
!
! Program History Log:
!   94-12-30  Iredell
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:  call gpkap
!
! Subprograms called:
!   fpkapx     function to compute exact pressure to the kappa
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=0._krealfp
    xmax=110000._krealfp
    xinc=(xmax-xmin)/(nxpkap-1)
    c1xpkap=1.-xmin/xinc
    c2xpkap=1./xinc
    do jx=1,nxpkap
      x=xmin+(jx-1)*xinc
      p=x
      tbpkap(jx)=fpkapx(p)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function fpkap(p)
  function fpkap(p)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpkap        raise pressure to the kappa power.
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Raise pressure over 1e5 Pa to the kappa power.
!   A linear interpolation is done between values in a lookup table
!   computed in gpkap. See documentation for fpkapx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy ranges from 9 decimal places
!   at 100000 Pascals to 5 decimal places at 1000 Pascals.
!   On the Cray, fpkap is over 5 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:   pkap=fpkap(p)
!
!   Input argument list:
!     p          Real(krealfp) pressure in Pascals
!
!   Output argument list:
!     fpkap      Real(krealfp) p over 1e5 Pa to the kappa power
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkap
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(xj,nxpkap-1._krealfp)
    fpkap=tbpkap(jx)+(xj-jx)*(tbpkap(jx+1)-tbpkap(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpkapq(p)
  function fpkapq(p)
!$$$     Subprogram Documentation Block
!
! Subprogram: fpkapq       raise pressure to the kappa power.
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Raise pressure over 1e5 Pa to the kappa power.
!   A quadratic interpolation is done between values in a lookup table
!   computed in gpkap. see documentation for fpkapx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy ranges from 12 decimal places
!   at 100000 Pascals to 7 decimal places at 1000 Pascals.
!   On the Cray, fpkap is over 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:   pkap=fpkapq(p)
!
!   Input argument list:
!     p          Real(krealfp) pressure in Pascals
!
!   Output argument list:
!     fpkapq     Real(krealfp) p over 1e5 Pa to the kappa power
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapq
    real(krealfp),intent(in):: p
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xpkap+c2xpkap*p,1._krealfp),real(nxpkap,krealfp))
    jx=min(max(nint(xj),2),nxpkap-1)
    dxj=xj-jx
    fj1=tbpkap(jx-1)
    fj2=tbpkap(jx)
    fj3=tbpkap(jx+1)
    fpkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  function fpkapo(p)
!$$$   Subprogram  documentation  block
!
! Subprogram: fpkapo       raise surface pressure to the kappa power.
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: Raise surface pressure over 1e5 Pa to the kappa power
!   using a rational weighted chebyshev approximation.
!   The numerator is of order 2 and the denominator is of order 4.
!   The pressure range is 40000-110000 Pa and kappa is defined in fpkapx.
!   The accuracy of this approximation is almost 8 decimal places.
!   On the Cray, fpkap is over 10 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  Iredell             f90 module
!
! Usage:  pkap=fpkapo(p)
!
!   Input argument list:
!     p          Real(krealfp) surface pressure in Pascals
!                p should be in the range 40000 to 110000
!
!   Output argument list:
!     fpkapo     Real(krealfp) p over 1e5 Pa to the kappa power
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapo
    real(krealfp),intent(in):: p
    integer,parameter:: nnpk=2,ndpk=4
    real(krealfp):: cnpk(0:nnpk)=(/3.13198449e-1,5.78544829e-2,&
                                         8.35491871e-4/)
    real(krealfp):: cdpk(0:ndpk)=(/1.,8.15968401e-2,5.72839518e-4,&
                                         -4.86959812e-7,5.24459889e-10/)
    integer n
    real(krealfp) pkpa,fnpk,fdpk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    pkpa=p*1.e-3_krealfp
    fnpk=cnpk(nnpk)
    do n=nnpk-1,0,-1
      fnpk=pkpa*fnpk+cnpk(n)
    enddo
    fdpk=cdpk(ndpk)
    do n=ndpk-1,0,-1
      fdpk=pkpa*fdpk+cdpk(n)
    enddo
    fpkapo=fnpk/fdpk
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function fpkapx(p)
  function fpkapx(p)
!$$$   Subprogram  documentation  block
!
! Subprogram: fpkapx       raise pressure to the kappa power.
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: raise pressure over 1e5 Pa to the kappa power.
!   Kappa is equal to rd/cp where rd and cp are physical constants.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   94-12-30  Iredell             made into inlinable function
! 1999-03-01  Iredell             f90 module
!
! Usage:  pkap=fpkapx(p)
!
!   Input argument list:
!     p          Real(krealfp) pressure in Pascals
!
!   Output argument list:
!     fpkapx     Real(krealfp) p over 1e5 Pa to the kappa power
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) fpkapx
    real(krealfp),intent(in):: p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    fpkapx=(p/1.e5_krealfp)**con_rocp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine grkap
!$$$   Subprogram  documentation  block
!
! Subprogram: grkap        Compute coefficients for p**(1/kappa)
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: Computes pressure to the 1/kappa table as a function of pressure
!   for the table lookup function frkap.
!   Exact pressure to the 1/kappa values are calculated in subprogram frkapx.
!   The current implementation computes a table with a length
!   of 5501 for pressures ranging up to 110000 Pascals.
!
! Program History Log:
!   94-12-30  Iredell
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:  call grkap
!
! Subprograms called:
!   frkapx     function to compute exact pressure to the 1/kappa
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx
    real(krealfp) xmin,xmax,xinc,x,p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xmin=0._krealfp
    xmax=fpkapx(110000._krealfp)
    xinc=(xmax-xmin)/(nxrkap-1)
    c1xrkap=1.-xmin/xinc
    c2xrkap=1./xinc
    do jx=1,nxrkap
      x=xmin+(jx-1)*xinc
      p=x
      tbrkap(jx)=frkapx(p)
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function frkap(pkap)
  function frkap(pkap)
!$$$     Subprogram Documentation Block
!
! Subprogram: frkap        raise pressure to the 1/kappa power.
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Raise pressure over 1e5 Pa to the 1/kappa power.
!   A linear interpolation is done between values in a lookup table
!   computed in grkap. See documentation for frkapx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 7 decimal places.
!   On the IBM, fpkap is about 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:   p=frkap(pkap)
!
!   Input argument list:
!     pkap       Real(krealfp) p over 1e5 Pa to the kappa power
!
!   Output argument list:
!     frkap      Real(krealfp) pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkap
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(xj,nxrkap-1._krealfp)
    frkap=tbrkap(jx)+(xj-jx)*(tbrkap(jx+1)-tbrkap(jx))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function frkapq(pkap)
  function frkapq(pkap)
!$$$     Subprogram Documentation Block
!
! Subprogram: frkapq       raise pressure to the 1/kappa power.
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Raise pressure over 1e5 Pa to the 1/kappa power.
!   A quadratic interpolation is done between values in a lookup table
!   computed in grkap. see documentation for frkapx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 11 decimal places.
!   On the IBM, fpkap is almost 4 times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
!   94-12-30  Iredell             standardized kappa,
!                                 increased range and accuracy
! 1999-03-01  Iredell             f90 module
! 1999-03-24  Iredell             table lookup
!
! Usage:   p=frkapq(pkap)
!
!   Input argument list:
!     pkap       Real(krealfp) p over 1e5 Pa to the kappa power
!
!   Output argument list:
!     frkapq     Real(krealfp) pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkapq
    real(krealfp),intent(in):: pkap
    integer jx
    real(krealfp) xj,dxj,fj1,fj2,fj3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xrkap+c2xrkap*pkap,1._krealfp),real(nxrkap,krealfp))
    jx=min(max(nint(xj),2),nxrkap-1)
    dxj=xj-jx
    fj1=tbrkap(jx-1)
    fj2=tbrkap(jx)
    fj3=tbrkap(jx+1)
    frkapq=(((fj3+fj1)/2-fj2)*dxj+(fj3-fj1)/2)*dxj+fj2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function frkapx(pkap)
  function frkapx(pkap)
!$$$   Subprogram  documentation  block
!
! Subprogram: frkapx       raise pressure to the 1/kappa power.
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: raise pressure over 1e5 Pa to the 1/kappa power.
!   Kappa is equal to rd/cp where rd and cp are physical constants.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   94-12-30  Iredell             made into inlinable function
! 1999-03-01  Iredell             f90 module
!
! Usage:  p=frkapx(pkap)
!
!   Input argument list:
!     pkap       Real(krealfp) p over 1e5 Pa to the kappa power
!
!   Output argument list:
!     frkapx     Real(krealfp) pressure in Pascals
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) frkapx
    real(krealfp),intent(in):: pkap
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    frkapx=pkap**(1/con_rocp)*1.e5_krealfp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gtlcl
!$$$     Subprogram Documentation Block
!
! Subprogram: gtlcl        Compute equivalent potential temperature table
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute lifting condensation level temperature table
!   as a function of temperature and dewpoint depression for function ftlcl.
!   Lifting condensation level temperature is calculated in subprogram ftlclx
!   The current implementation computes a table with a first dimension
!   of 151 for temperatures ranging from 180.0 to 330.0 Kelvin
!   and a second dimension of 61 for dewpoint depression ranging from
!   0 to 60 Kelvin.
!
! Program History Log:
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gtlcl
!
! Subprograms called:
!   (ftlclx)    inlinable function to compute LCL temperature
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    integer jx,jy
    real(krealfp) xmin,xmax,ymin,ymax,xinc,yinc,x,y,tdpd,t
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    xmin=180._krealfp
    xmax=330._krealfp
    ymin=0._krealfp
    ymax=60._krealfp
    xinc=(xmax-xmin)/(nxtlcl-1)
    c1xtlcl=1.-xmin/xinc
    c2xtlcl=1./xinc
    yinc=(ymax-ymin)/(nytlcl-1)
    c1ytlcl=1.-ymin/yinc
    c2ytlcl=1./yinc
    do jy=1,nytlcl
      y=ymin+(jy-1)*yinc
      tdpd=y
      do jx=1,nxtlcl
        x=xmin+(jx-1)*xinc
        t=x
        tbtlcl(jx,jy)=ftlclx(t,tdpd)
      enddo
    enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
! elemental function ftlcl(t,tdpd)
  function ftlcl(t,tdpd)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftlcl        Compute LCL temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.
!   A bilinear interpolation is done between values in a lookup table
!   computed in gtlcl. See documentation for ftlclx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.0005 Kelvin.
!   On the Cray, ftlcl is ? times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
! 1999-03-01  Iredell             f90 module
!
! Usage:   tlcl=ftlcl(t,tdpd)
!
!   Input argument list:
!     t          Real(krealfp) LCL temperature in Kelvin
!     tdpd       Real(krealfp) dewpoint depression in Kelvin
!
!   Output argument list:
!     ftlcl      Real(krealfp) temperature at the LCL in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlcl
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,ftx1,ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(xj,nxtlcl-1._krealfp)
    jy=min(yj,nytlcl-1._krealfp)
    ftx1=tbtlcl(jx,jy)+(xj-jx)*(tbtlcl(jx+1,jy)-tbtlcl(jx,jy))
    ftx2=tbtlcl(jx,jy+1)+(xj-jx)*(tbtlcl(jx+1,jy+1)-tbtlcl(jx,jy+1))
    ftlcl=ftx1+(yj-jy)*(ftx2-ftx1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftlclq(t,tdpd)
  function ftlclq(t,tdpd)
!$$$     Subprogram Documentation Block
!
! Subprogram: ftlclq       Compute LCL temperature
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.
!   A biquadratic interpolation is done between values in a lookup table
!   computed in gtlcl. see documentation for ftlclx for details.
!   Input values outside table range are reset to table extrema.
!   The interpolation accuracy is better than 0.000003 Kelvin.
!   On the Cray, ftlclq is ? times faster than exact calculation.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
! 1999-03-01  Iredell             f90 module
!
! Usage:   tlcl=ftlclq(t,tdpd)
!
!   Input argument list:
!     t          Real(krealfp) LCL temperature in Kelvin
!     tdpd       Real(krealfp) dewpoint depression in Kelvin
!
!   Output argument list:
!     ftlcl      Real(krealfp) temperature at the LCL in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclq
    real(krealfp),intent(in):: t,tdpd
    integer jx,jy
    real(krealfp) xj,yj,dxj,dyj
    real(krealfp) ft11,ft12,ft13,ft21,ft22,ft23,ft31,ft32,ft33
    real(krealfp) ftx1,ftx2,ftx3
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    xj=min(max(c1xtlcl+c2xtlcl*t,1._krealfp),real(nxtlcl,krealfp))
    yj=min(max(c1ytlcl+c2ytlcl*tdpd,1._krealfp),real(nytlcl,krealfp))
    jx=min(max(nint(xj),2),nxtlcl-1)
    jy=min(max(nint(yj),2),nytlcl-1)
    dxj=xj-jx
    dyj=yj-jy
    ft11=tbtlcl(jx-1,jy-1)
    ft12=tbtlcl(jx-1,jy)
    ft13=tbtlcl(jx-1,jy+1)
    ft21=tbtlcl(jx,jy-1)
    ft22=tbtlcl(jx,jy)
    ft23=tbtlcl(jx,jy+1)
    ft31=tbtlcl(jx+1,jy-1)
    ft32=tbtlcl(jx+1,jy)
    ft33=tbtlcl(jx+1,jy+1)
    ftx1=(((ft31+ft11)/2-ft21)*dxj+(ft31-ft11)/2)*dxj+ft21
    ftx2=(((ft32+ft12)/2-ft22)*dxj+(ft32-ft12)/2)*dxj+ft22
    ftx3=(((ft33+ft13)/2-ft23)*dxj+(ft33-ft13)/2)*dxj+ft23
    ftlclq=(((ftx3+ftx1)/2-ftx2)*dyj+(ftx3-ftx1)/2)*dyj+ftx2
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  function ftlclo(t,tdpd)
!$$$   Subprogram  documentation  block
!
! Subprogram: ftlclo       Compute LCL temperature.
!   Author: Phillips         org: w/NMC2X2   Date: 29 dec 82
!
! Abstract: Compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.  the formula used is
!   a polynomial taken from Phillips mstadb routine which empirically
!   approximates the original exact implicit relationship.
!   (This kind of approximation is customary (inman, 1969), but
!   the original source for this particular one is not yet known. -MI)
!   Its accuracy is about 0.03 Kelvin for a dewpoint depression of 30.
!   This function should be expanded inline in the calling routine.
!
! Program History Log:
!   91-05-07  Iredell             made into inlinable function
! 1999-03-01  Iredell             f90 module
!
! Usage:  tlcl=ftlclo(t,tdpd)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!     tdpd       Real(krealfp) dewpoint depression in Kelvin
!
!   Output argument list:
!     ftlclo     Real(krealfp) temperature at the LCL in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclo
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: clcl1= 0.954442e+0,clcl2= 0.967772e-3,&
                                    clcl3=-0.710321e-3,clcl4=-0.270742e-5
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    ftlclo=t-tdpd*(clcl1+clcl2*t+tdpd*(clcl3+clcl4*t))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
! elemental function ftlclx(t,tdpd)
  function ftlclx(t,tdpd)
!$$$   Subprogram  documentation  block
!
! Subprogram: ftlclx       Compute LCL temperature.
!   Author: Iredell          org: w/NMC2X2   Date: 25 March 1999
!
! Abstract: Compute temperature at the lifting condensation level
!   from temperature and dewpoint depression.  A parcel lifted
!   adiabatically becomes saturated at the lifting condensation level.
!   The water model assumes a perfect gas, constant specific heats
!   for gas and liquid, and neglects the volume of the liquid.
!   The model does account for the variation of the latent heat
!   of condensation with temperature.  The ice option is not included.
!   The Clausius-Clapeyron equation is integrated from the triple point
!   to get the formulas
!       pvlcl=con_psat*(trlcl**xa)*exp(xb*(1.-trlcl))
!       pvdew=con_psat*(trdew**xa)*exp(xb*(1.-trdew))
!   where pvlcl is the saturated parcel vapor pressure at the LCL,
!   pvdew is the unsaturated parcel vapor pressure initially,
!   trlcl is ttp/tlcl and trdew is ttp/tdew.  The adiabatic lifting
!   of the parcel is represented by the following formula
!       pvdew=pvlcl*(t/tlcl)**(1/kappa)
!   This formula is inverted by iterating Newtonian approximations
!   until tlcl is found to within 1.e-6 Kelvin.  Note that the minimum
!   returned temperature is 180 Kelvin.
!
! Program History Log:
! 1999-03-25  Iredell
!
! Usage:  tlcl=ftlclx(t,tdpd)
!
!   Input argument list:
!     t          Real(krealfp) temperature in Kelvin
!     tdpd       Real(krealfp) dewpoint depression in Kelvin
!
!   Output argument list:
!     ftlclx     Real(krealfp) temperature at the LCL in Kelvin
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
    real(krealfp) ftlclx
    real(krealfp),intent(in):: t,tdpd
    real(krealfp),parameter:: terrm=1.e-4,tlmin=180.,tlminx=tlmin-5.
    real(krealfp) tr,pvdew,tlcl,ta,pvlcl,el,dpvlcl,terr,terrp
    integer i
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    if(.not.initialized) call gfuncphys()
    tr=con_ttp/(t-tdpd)
    pvdew=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))
    tlcl=t-tdpd
    do i=1,100
      tr=con_ttp/tlcl
      ta=t/tlcl
      pvlcl=con_psat*(tr**con_xpona)*exp(con_xponb*(1.-tr))*ta**(1/con_rocp)
      el=con_hvap+con_dldt*(tlcl-con_ttp)
      dpvlcl=(el/(con_rv*t**2)+1/(con_rocp*tlcl))*pvlcl
      terr=(pvlcl-pvdew)/dpvlcl
      tlcl=tlcl-terr
      if(abs(terr).le.terrm.or.tlcl.lt.tlminx) exit
    enddo
    ftlclx=max(tlcl,tlmin)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end function
!-------------------------------------------------------------------------------
  subroutine gfuncphys
!$$$     Subprogram Documentation Block
!
! Subprogram: gfuncphys    Compute all physics function tables
!   Author: N Phillips            w/NMC2X2   Date: 30 dec 82
!
! Abstract: Compute all physics function tables.  Lookup tables are
!   set up for computing saturation vapor pressure, dewpoint temperature,
!   equivalent potential temperature, moist adiabatic temperature and humidity,
!   pressure to the kappa, and lifting condensation level temperature.
!
! Program History Log:
! 1999-03-01  Iredell             f90 module
!
! Usage:  call gfuncphys
!
! Subprograms called:
!   gpvsl       compute saturation vapor pressure over liquid table
!   gpvsi       compute saturation vapor pressure over ice table
!   gpvs        compute saturation vapor pressure table
!   gtdpl       compute dewpoint temperature over liquid table
!   gtdpi       compute dewpoint temperature over ice table
!   gtdp        compute dewpoint temperature table
!   gthe        compute equivalent potential temperature table
!   gtma        compute moist adiabat tables
!   gpkap       compute pressure to the kappa table
!   grkap       compute pressure to the 1/kappa table
!   gtlcl       compute LCL temperature table
!
! Attributes:
!   Language: Fortran 90.
!
!$$$
    implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    initialized=.true.
    call gpvsl
    call gpvsi
    call gpvs
    call gtdpl
    call gtdpi
    call gtdp
    call gthe
    call gtma
    call gpkap
    call grkap
    call gtlcl
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  end subroutine
!-------------------------------------------------------------------------------
end module module_gfs_funcphys
