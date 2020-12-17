!=============================================================================
! This file contains the following subroutines, related to the calculation
! of radiation at Lyman-alpha and Schumann-Runge wavelengths:
!     la_srb
!     lymana
!     schum
!     effxs
!     calc_params
!     init_xs
!     sjo2   
! and the following functions
!     chebev
!=============================================================================

      module SRB

      implicit none

      private
      public :: la_srb, sjo2, init_srb
      public :: nchebev_term, nchebev_wave
      public :: chebev_ac, chebev_bc
      public :: ila, isrb

      INTEGER, parameter :: kla = 2
      INTEGER, PARAMETER :: ksrb = 18
      integer, parameter :: nla =  kla - 1
      integer, parameter :: nsrb = ksrb - 1

      integer :: nchebev_term, nchebev_wave

      integer :: ila, isrb
      REAL(8) :: b(3), c(3), d(3), e(3)
      REAL(8), allocatable :: chebev_ac(:,:)
      REAL(8), allocatable :: chebev_bc(:,:)

      REAL    :: xslod(nsrb)
      REAL    :: wlsrb(ksrb)
      REAL    :: wlla(kla)

      CONTAINS

      SUBROUTINE init_srb

      b(:) = (/ 6.8431e-01_8,  2.29841e-01_8,  8.65412e-02_8 /)
      c(:) = (/ 8.22114e-21_8, 1.77556e-20_8,  8.22112e-21_8 /)
      d(:) = (/ 6.0073e-21_8,  4.28569e-21_8,  1.28059e-20_8 /)
      e(:) = (/ 8.21666e-21_8, 1.63296e-20_8,  4.85121e-17_8 /)
      xslod(:) = (/6.2180730E-21, 5.8473627E-22, 5.6996334E-22, &
                   4.5627094E-22, 1.7668250E-22, 1.1178808E-22, &
                   1.2040544E-22, 4.0994668E-23, 1.8450616E-23, &
                   1.5639540E-23, 8.7961075E-24, 7.6475608E-24, &
                   7.6260556E-24, 7.5565696E-24, 7.6334338E-24, &
                   7.4371992E-24, 7.3642966E-24 /)
      wlla(:)  = (/ 121.4, 121.9/)
      wlsrb(:) = (/174.4, 177.0, 178.6, 180.2, 181.8, &
                   183.5, 185.2, 186.9, 188.7, 190.5, &
                   192.3, 194.2, 196.1, 198.0, 200.0, &
                   202.0, 204.1, 205.8/)

      END SUBROUTINE init_srb

      SUBROUTINE la_srb( nlyr, z, tlev, wmin, &
                         vcol, scol, o2_xs, dto2, srb_o2_xs )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Compute equivalent optical depths for O2 absorption, and O2 effective
!=  absorption cross sections, parameterized in the Lyman-alpha and SR bands
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ      - INTEGER, number of specified altitude levels in the working (I)
!=            grid
!=  Z       - REAL, specified altitude working grid (km)                  (I)
!=  NW      - INTEGER, number of specified intervals + 1 in working       (I)
!=            wavelength grid
!=  WL      - REAL, vector of lxower limits of wavelength intervals in    (I)
!=            working wavelength grid
!=  CZ      - REAL, number of air molecules per cm^2 at each specified    (I)
!=            altitude layer
!=  ZEN     - REAL, solar zenith angle                                    (I)
!=
!=  O2XS1   - REAL, O2 cross section from rdo2xs                          (I)
!=
!=  DTO2    - REAL, optical depth due to O2 absorption at each specified  (O)
!=            vertical layer at each specified wavelength
!=  O2XS    - REAL, molecular absorption cross section in SR bands at     (O)
!=            each specified altitude and wavelength.  Includes Herzberg
!=            continuum.
!-----------------------------------------------------------------------------

      use params_mod, only : o2vmr, largest

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr
      REAL, intent(in) :: wmin
      REAL, intent(in) :: z(:)
      REAL, intent(in) :: tlev(:)

      REAL, intent(in) :: vcol(:)
      REAL, intent(in) :: scol(:)
      REAL, intent(in) :: o2_xs(:)
      REAL, intent(inout) :: dto2(:,:)
      REAL, intent(inout) :: srb_o2_xs(:,:)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      REAL :: secchi(nlyr)
      REAL :: o2col(nlyr)

!-----------------------------------------------------------------------------
! Lyman-alpha variables
! O2 optical depth and equivalent cross section in the Lyman-alpha region
!-----------------------------------------------------------------------------
      INTEGER :: nlev
      INTEGER :: nlev_srb
      INTEGER :: k, iw, wn
      REAL    :: dto2la(nlyr,nla), o2xsla(nlyr,nla)

!-----------------------------------------------------------------------------
! grid on which Koppers' parameterization is defined
! O2 optical depth and equivalent cross section on Koppers' grid
!-----------------------------------------------------------------------------
      REAL    :: dto2k(nlyr,nsrb), o2xsk(nlyr,nsrb)

      nlev_srb = size( srb_o2_xs,dim=2 )
      nlev = nlyr
!----------------------------------------------------------------------
! initalize O2 cross sections 
!----------------------------------------------------------------------
      DO k = 1, nlev_srb
        srb_o2_xs(:,k) = o2_xs(:)
      END DO

      IF( wmin <= wlsrb(nsrb) ) THEN
!----------------------------------------------------------------------
! Slant O2 column and x-sections.
!----------------------------------------------------------------------
        o2col(:nlyr) = o2vmr * scol(:nlyr)
!----------------------------------------------------------------------
! Effective secant of solar zenith angle.  
! Use 2.0 if no direct sun (value for isotropic radiation)
! For nz, use value at nz-1
!----------------------------------------------------------------------
        WHERE( scol(:nlyr) > .1*largest ) 
          secchi(:nlyr) = 2.
        ELSEWHERE
          secchi(:nlyr) = scol(:nlyr)/vcol(:nlyr)
        ENDWHERE

!---------------------------------------------------------------------
! Lyman-Alpha parameterization, output values of O2 optical depth
! and O2 effective (equivalent) cross section
!----------------------------------------------------------------------
        CALL lymana( nlyr, o2col, secchi, dto2la, o2xsla )
        DO wn = ila, ila + nla - 1
          iw = wn - ila + 1
          dto2(:nlyr,wn)          = dto2la(:nlyr,iw) 
          srb_o2_xs(wn,:nlev_srb) = o2xsla(2:nlev_srb+1,iw)
        ENDDO

!------------------------------------------------------------------------------
! Koppers' parameterization of the SR bands, output values of O2
! optical depth and O2 equivalent cross section 
!------------------------------------------------------------------------------
        CALL schum( nlyr, o2col, tlev, secchi, dto2k, o2xsk )
        DO wn = isrb, isrb + nsrb - 1
          iw = wn - isrb + 1
          dto2(:nlyr,wn)          = dto2k(:nlyr,iw)
          srb_o2_xs(wn,:nlev_srb) = o2xsk(2:nlev_srb+1,iw)
        ENDDO
      ENDIF

      END SUBROUTINE la_srb

      SUBROUTINE lymana( nlyr, o2col, secchi, dto2la, o2xsla )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Calculate the effective absorption cross section of O2 in the Lyman-Alpha
!=  bands and an effective O2 optical depth at all altitudes.  Parameterized
!=  after:  Chabrillat, S., and G. Kockarts, Simple parameterization of the
!=  absorption of the solar Lyman-Alpha line, Geophysical Research Letters,
!=  Vol.24, No.21, pp 2659-2662, 1997.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ      - INTEGER, number of specified altitude levels in the working (I)
!=            grid
!=  O2COL   - REAL, slant overhead O2 column (molec/cc) at each specified (I)
!=            altitude
!=  DTO2LA  - REAL, optical depth due to O2 absorption at each specified  (O)
!=            vertical layer
!=  O2XSLA  - REAL, molecular absorption cross section in LA bands        (O)
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr
      REAL,    intent(in) :: o2col(:)
      REAL,    intent(in) :: secchi(:)
      REAL, intent(inout) :: dto2la(nlyr,nla), o2xsla(nlyr,nla)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      REAL, parameter    :: xsmin = 1.e-20
      REAL(8), parameter :: rmmin = 1.e-100_8

      INTEGER :: k, kp1, wn
      REAL(8) :: o2_col
      REAL(8) :: rm(nlyr), ro2(nlyr)
      REAL(8) :: rm_wrk(3), ro2_wrk(3)

      do wn = 1,nla
        dto2la(:nlyr,wn) = 0.
        o2xsla(:nlyr,wn) = 0.
      end do
!-----------------------------------------------------------------------------
! calculate reduction factors at every layer
!-----------------------------------------------------------------------------
      rm(:nlyr)  = 0._8
      ro2(:nlyr) = 0._8
      DO k = 1, nlyr
        o2_col = real( o2col(k),8 )
        rm_wrk(:)  = b(:) * EXP( -c(:) * o2_col )
        ro2_wrk(:) = d(:) * EXP( -e(:) * o2_col )
        rm(k)  = sum( rm_wrk )
        ro2(k) = sum( ro2_wrk )
      ENDDO

!-----------------------------------------------------------------------------
! calculate effective O2 optical depths and effective O2 cross sections
!-----------------------------------------------------------------------------
      DO k = 1, nlyr-1
        kp1 = k + 1
        IF (rm(k) > rmmin) THEN
          IF (ro2(k) > rmmin) THEN
            o2xsla(k,1) = REAL( ro2(k)/rm(k) )
          ELSE
            o2xsla(k,1) = xsmin
          ENDIF

          IF (rm(kp1) > 0._8) THEN
            dto2la(k,1) = LOG( rm(kp1) )/secchi(kp1)  &
                        - LOG( rm(k))   /secchi(k)
          ELSE
            dto2la(k,1) = 1000.
          ENDIF
        ELSE
          dto2la(k,1) = 1000.
          o2xsla(k,1) = xsmin
        ENDIF
      END DO

!-----------------------------------------------------------------------------
! do top layer separately
!-----------------------------------------------------------------------------
      IF( rm(nlyr) > rmmin ) THEN
        o2xsla(nlyr,1) = REAL( ro2(nlyr)/rm(nlyr) )
      ELSE
        o2xsla(nlyr,1) = xsmin
      ENDIF

      END SUBROUTINE lymana

      SUBROUTINE schum( nlyr, o2col, tlev, secchi, dto2, o2xsk )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Calculate the equivalent absorption cross section of O2 in the SR bands.
!=  The algorithm is based on parameterization of G.A. Koppers, and
!=  D.P. Murtagh [ref. Ann.Geophys., 14 68-79, 1996]
!=  Final values do include effects from the Herzberg continuum.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ      - INTEGER, number of specified altitude levels in the working (I)
!=            grid
!=  O2COL   - REAL, slant overhead O2 column (molec/cc) at each specified (I)
!=            altitude
!=  TLEV    - tmeperature at each level
!=  SECCHI  - ratio of slant to vertical o2 columns
!=  DTO2    - REAL, optical depth due to O2 absorption at each specified
!=            vertical layer at each specified wavelength
!=  O2XSK  - REAL, molecular absorption cross section in SR bands at
!=            each specified wavelength.  Includes Herzberg continuum
!-----------------------------------------------------------------------------

      use params_mod, only : precis

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr
      REAL,    intent(in) :: o2col(:)
      REAL,    intent(in) :: tlev(:), secchi(:)
      REAL, intent(inout) :: dto2(nlyr,nsrb), o2xsk(nlyr,nsrb)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      REAL, parameter :: o2col_min = exp( 38. )

      INTEGER :: wn, k, ktop, ktop1, kbot, nlyrm1
      REAL    :: x
      REAL    :: o2col1(nlyr)
      REAL    :: xs(nsrb)

      nlyrm1 = nlyr - 1
!-----------------------------------------------------------------------------
!     ...Initialize cross sections to values at large optical depth
!-----------------------------------------------------------------------------
      DO wn = 1, nsrb
        o2xsk(:nlyr,wn) = xslod(wn)
      END DO

!-----------------------------------------------------------------------------
!     Calculate cross sections
!     Set smallest O2col = exp(38.) molec cm-2
!     to stay in range of parameterization
!     given by Koppers et al. at top of atm.
!-----------------------------------------------------------------------------
      ktop = 2*nlyr
      kbot = 0

      DO k = 1,nlyr
        o2col1(k) = MAX( o2col(k),o2col_min )
        x  = LOG( o2col1(k) )
        IF (x < 38.0) THEN
          ktop1 = k-1
          ktop  = MIN(ktop1,ktop)
        ELSE IF (x > 56.0) THEN
          kbot = k
        ELSE
          CALL effxs( x, tlev(k), xs )
          o2xsk(k,:nsrb) = xs(:nsrb)
        ENDIF
      END DO

!-----------------------------------------------------------------------------
!  fill in cross section where X is out of range by repeating edge table values
!  Do not allow kbot = nlyr to avoid division by zero in no light case.
!-----------------------------------------------------------------------------
      IF( kbot == nlyr) then
        kbot = nlyrm1
      ENDIF

      IF( kbot > 0 ) THEN
        DO wn = 1,nsrb
          o2xsk(:kbot,wn) = o2xsk(kbot+1,wn)
        END DO
      ENDIF

      IF( ktop < nlyr ) THEN
        DO wn = 1,nsrb
          o2xsk(ktop+1:nlyr,wn) = o2xsk(ktop,wn)
        END DO
      ENDIF

!-----------------------------------------------------------------------------
!  Calculate incremental optical depths 
!-----------------------------------------------------------------------------
      dto2(nlyr,1:nsrb) = 0.0       ! set optical depth to zero at top
      DO wn = 1,nsrb
!-----------------------------------------------------------------------------
!     ... calculate an optical depth weighted by density,
!         put in mean value estimate, if in shade
!-----------------------------------------------------------------------------
        WHERE (ABS(1. - o2col1(2:nlyr)/o2col1(:nlyrm1)) <= 2.*precis)
          dto2(:nlyrm1,wn) = o2xsk(2:nlyr,wn)*o2col1(2:nlyr)/real(nlyrm1)
        ELSEWHERE
          dto2(:nlyr-1,wn) = ABS( &
            (o2xsk(2:nlyr,wn)*o2col1(2:nlyr) - o2xsk(:nlyrm1,wn)*o2col1(:nlyrm1)) &
            /(1. + LOG(o2xsk(2:nlyr,wn)/o2xsk(:nlyrm1,wn))  &
              / LOG(o2col1(2:nlyr)/o2col1(:nlyrm1))) )
!-----------------------------------------------------------------------------
!     ... change to vertical optical depth
!-----------------------------------------------------------------------------
          dto2(:nlyrm1,wn) = 2. * dto2(:nlyrm1,wn)/(secchi(:nlyr-1)+secchi(2:nlyr))
        ENDWHERE
      END DO 

      END SUBROUTINE schum

      SUBROUTINE EFFXS( x, t, xs )
!-----------------------------------------------------------------------------
!     Subroutine for evaluating the effective cross section
!     of O2 in the Schumann-Runge bands using parameterization
!     of G.A. Koppers, and D.P. Murtagh [ref. Ann.Geophys., 14
!     68-79, 1996]
!      
!     method:
!     ln(xs) = A(X)[T-220]+B(X)
!     X = log of slant column of O2
!     A,B calculated from Chebyshev polynomial coeffs
!     AC and BC using NR routine chebev.  Assume interval
!     is 38<ln(NO2)<56.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL, intent(in)  :: t, x
      REAL, intent(out) :: xs(nsrb)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: i
      REAL    :: a(nsrb), b(nsrb) 

      call calc_params( x, a, b )

      xs(:nsrb) = EXP( a(:nsrb)*( t - 220.) + b(:nsrb) )

      END SUBROUTINE EFFXS

      SUBROUTINE CALC_PARAMS( x, a, b )
!-----------------------------------------------------------------------------
!     calculates coefficients (A,B), used in calculating the
!     effective cross section, for nsrb wavelength intervals
!     as a function of log O2 column density (X)
!     Wavelength intervals are defined in WMO1985
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      REAL, intent(in)  :: x
      REAL, intent(out) :: a(nsrb), b(nsrb)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: wn

!-----------------------------------------------------------------------------
!     call Chebyshev Evaluation routine to calc A and B from
!     set of 20 coeficients for each wavelength
!-----------------------------------------------------------------------------

      DO wn = 1,nsrb
        a(wn) = chebev( 38.0 , 56.0, chebev_ac(:,wn), nchebev_term, x )
        b(wn) = chebev( 38.0 , 56.0, chebev_bc(:,wn), nchebev_term, x )
      END DO

      END SUBROUTINE CALC_PARAMS

      REAL FUNCTION chebev( a, b, c, m, x )
!-------------------------------------------------------------
!     Chebyshev evaluation algorithm
!     See Numerical recipes p193
!-------------------------------------------------------------
      
!-------------------------------------------------------------
!       ... dummy arguments
!-------------------------------------------------------------
      INTEGER, intent(in) :: m
      REAL,    intent(in) :: a, b, x
      REAL(8), intent(in) :: c(:)

!-------------------------------------------------------------
!       ... local variables
!-------------------------------------------------------------
      INTEGER :: j
      REAL    :: d, dd, sv, y, y2

      IF( (x - a)*(x - b) > 0.) THEN
	chebev = 0.0
      ELSE
	d  = 0.
        dd = 0.
        y  = (2.*x - a - b)/(b - a)
        y2 = 2.*y
        DO J = m,2,-1
          sv = d
          d  = y2*d - dd + real( c(J),4 )
          dd = sv
        END DO
        chebev = y*d - dd + 0.5*real( c(1),4 )
      ENDIF
	
      END FUNCTION chebev

      SUBROUTINE sjo2( nlyr, nwave, xso2, xsqy )
!-----------------------------------------------------------------------------
!=  PURPOSE:
!=  Update the weighting function (cross section x quantum yield) for O2
!=  photolysis.  The strong spectral variations in the O2 cross sections are
!=  parameterized into a few bands for Lyman-alpha (121.4-121.9 nm, one band)
!=  and Schumann-Runge (174.4-205.8, nsrb bands) regions. The parameterizations
!=  depend on the overhead O2 column, and therefore on altitude and solar
!=  zenith angle, so they need to be updated at each time/zenith step.
!-----------------------------------------------------------------------------
!=  PARAMETERS:
!=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)
!=  NW     - INTEGER, number of specified intervals + 1 in working        (I)
!=           wavelength grid
!=  XSO2   - REAL, molecular absorption cross section in SR bands at      (I)
!=           each specified altitude and wavelength.  Includes Herzberg
!=            continuum.
!=  NJ     - INTEGER, index of O2 photolysis in array SQ                  (I)
!=  xsqy   - REAL, cross section x quantum yield (cm^2) for each          (O)
!=           photolysis reaction, at each wavelength and each altitude level
!-----------------------------------------------------------------------------


!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in)    :: nlyr, nwave
      REAL,    intent(in)    :: xso2(:,:)
      REAL,    intent(inout) :: xsqy(:,:)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: k

!-----------------------------------------------------------------------------
! O2 + hv -> O + O
! quantum yield assumed to be unity
! assign cross section values at all wavelengths and at all altitudes
!      qy = 1.
!-----------------------------------------------------------------------------
      DO k = 1, nlyr
        xsqy(:nwave,k) = xso2(:nwave,k)
      END DO

      END SUBROUTINE sjo2

      end module SRB
