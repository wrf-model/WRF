!=============================================================================
! This file contains the following subroutines, related to the solution of
! the equation of radiative transfer in multiple homogeneous layers.
!     rtlink
!     ps2str
!        tridag
!=============================================================================

      MODULE rad_trans

      IMPLICIT NONE

      private
      public :: rtlink

      CONTAINS

      SUBROUTINE rtlink( &
           nstr, nlev, nlyr, nwave, &
           iw, albedo, zen, &
           dsdh, nid, &
           dtrl,  &
           dto3,  &
           dto2, &
           dtso2, &
           dtno2,  &
           dtcld, omcld, gcld, &
           dtaer, omaer, gaer, &
           dtsnw, omsnw, gsnw, &
#ifdef SW_DEBUG
           edir, edn, eup, fdir, fdn, fup, tuv_diags )
#else
           edir, edn, eup, fdir, fdn, fup )
#endif

      use params_mod, only : largest, pi

!---------------------------------------------------------------------
!     ... dummy arguments
!---------------------------------------------------------------------
      INTEGER, intent(in) :: nstr
      INTEGER, intent(in) :: nlev, nlyr
      INTEGER, intent(in) :: nwave, iw
      REAL, intent(in)    :: albedo
      REAL, intent(in)    :: zen
      REAL, intent(in)    :: dsdh(0:nlyr,nlyr)
      INTEGER, intent(in) :: nid(0:nlyr)

      REAL, intent(in)    :: dtrl(nlyr,nwave)
      REAL, intent(in)    :: dto3(nlyr,nwave), dto2(nlyr,nwave)
      REAL, intent(in)    :: dtso2(nlyr,nwave), dtno2(nlyr,nwave)
      REAL, intent(in)    :: dtcld(nlyr,nwave), omcld(nlyr,nwave), gcld(nlyr,nwave)
      REAL, intent(in)    :: dtaer(nlyr,nwave), omaer(nlyr,nwave), gaer(nlyr,nwave)
      REAL, intent(in)    :: dtsnw(nlyr,nwave), omsnw(nlyr,nwave), gsnw(nlyr,nwave)

      REAL, intent(out)   :: edir(nlev), edn(nlev), eup(nlev)
      REAL, intent(out)   :: fdir(nlev), fdn(nlev), fup(nlev)

#ifdef SW_DEBUG
      LOGICAL, intent(in) :: tuv_diags
#endif


!---------------------------------------------------------------------
!     ... local variables
!---------------------------------------------------------------------
      REAL, PARAMETER :: dr = pi/180.

      INTEGER :: k, kk
      REAL    :: dtabs,dtsct,dscld,dsaer,dssnw,dacld,daaer,dasnw
      REAL    :: dt(nlyr), om(nlyr), g(nlyr)


!---------------------------------------------------------------------
!     ... specific to ps2str
!---------------------------------------------------------------------
      LOGICAL, parameter :: delta = .true.
      REAL ediri(nlev), edni(nlev), eupi(nlev)
      REAL fdiri(nlev), fdni(nlev), fupi(nlev)

!---------------------------------------------------------------------
! initialize:
!---------------------------------------------------------------------
      fdir(1:nlev) = 0.
      fup(1:nlev)  = 0.
      fdn(1:nlev)  = 0.
      edir(1:nlev) = 0.
      eup(1:nlev)  = 0.
      edn(1:nlev)  = 0.

      DO k = 1, nlyr
        dscld = dtcld(k,iw)*omcld(k,iw)
        dacld = dtcld(k,iw)*(1.-omcld(k,iw))

        dsaer = dtaer(k,iw)*omaer(k,iw)
        daaer = dtaer(k,iw)*(1.-omaer(k,iw))

        dssnw = dtsnw(k,iw)*omsnw(k,iw)
        dasnw = dtsnw(k,iw)*(1.-omsnw(k,iw))

        dtsct = dtrl(k,iw) + dscld + dsaer + dssnw
        dtabs = dtso2(k,iw) + dto2(k,iw) + dto3(k,iw) & 
              + dtno2(k,iw) + dacld + daaer + dasnw

        dtabs = max( dtabs,1./largest )
        dtsct = max( dtsct,1./largest )

!---------------------------------------------------------------------
! from bottom-up to top-down
!---------------------------------------------------------------------
        kk = nlyr - k + 1
        dt(kk) = dtsct + dtabs
        g(kk)  = (gcld(k,iw)*dscld + gsnw(k,iw)*dssnw + gaer(k,iw)*dsaer)/dtsct
        IF( dtsct /= 1./largest ) then
          om(kk) = dtsct/(dtsct + dtabs)
        ELSE
          om(kk) = 1./largest
        ENDIF
      END DO   

#ifdef SW_DEBUG
      if( tuv_diags .and. iw == 100 ) then
        open(unit=33,file='WRF-TUV.dbg1.out')
        do k = 1,nlyr,5
          write(33,'(1p,5g15.7)') dt(k:min(k+4,nlyr))
        end do
        do k = 1,nlyr,5
          write(33,'(1p,5g15.7)') g(k:min(k+4,nlyr))
        end do
        do k = 1,nlyr,5
          write(33,'(1p,5g15.7)') om(k:min(k+4,nlyr))
        end do
        close(33)
      endif
#endif

      CALL ps2str( nlyr, nlev, zen, albedo, &
                   dt, om, g, &
                   dsdh, nid, delta, &
                   fdiri, fupi, fdni, ediri, eupi, edni)

!---------------------------------------------------------------------
! from top-down to bottom-up
!---------------------------------------------------------------------
      fdir(1:nlev) = fdiri(nlev:1:-1)
      fup(1:nlev)  = fupi(nlev:1:-1)
      fdn(1:nlev)  = fdni(nlev:1:-1)
      edir(1:nlev) = ediri(nlev:1:-1)
      eup(1:nlev)  = eupi(nlev:1:-1)
      edn(1:nlev)  = edni(nlev:1:-1)

      END SUBROUTINE rtlink

      SUBROUTINE ps2str( &
           nlyr, nlev, zen, rsfc, &
           tauu, omu, gu, &
           dsdh, nid, delta, &
           fdr, fup, fdn, edr, eup, edn)
!-----------------------------------------------------------------------------*
!=  PURPOSE:                                                                 =*
!=  Solve two-stream equations for multiple layers.  The subroutine is based =*
!=  on equations from:  Toon et al., J.Geophys.Res., v94 (D13), Nov 20, 1989.=*
!=  It contains 9 two-stream methods to choose from.  A pseudo-spherical     =*
!=  correction has also been added.                                          =*
!-----------------------------------------------------------------------------*
!=  PARAMETERS:                                                              =*
!=  NLEVEL  - INTEGER, number of specified altitude levels in the working (I)=*
!=            grid                                                           =*
!=  ZEN     - REAL, solar zenith angle (degrees)                          (I)=*
!=  RSFC    - REAL, surface albedo at current wavelength                  (I)=*
!=  TAUU    - REAL, unscaled optical depth of each layer                  (I)=*
!=  OMU     - REAL, unscaled single scattering albedo of each layer       (I)=*
!=  GU      - REAL, unscaled asymmetry parameter of each layer            (I)=*
!=  DSDH    - REAL, slant path of direct beam through each layer crossed  (I)=*
!=            when travelling from the top of the atmosphere to layer i;     =*
!=            DSDH(i,j), i = 0..NZ-1, j = 1..NZ-1                            =*
!=  NID     - INTEGER, number of layers crossed by the direct beam when   (I)=*
!=            travelling from the top of the atmosphere to layer i;          =*
!=            NID(i), i = 0..NZ-1                                            =*
!=  DELTA   - LOGICAL, switch to use delta-scaling                        (I)=*
!=            .TRUE. -> apply delta-scaling                                  =*
!=            .FALSE.-> do not apply delta-scaling                           =*
!=  FDR     - REAL, contribution of the direct component to the total     (O)=*
!=            actinic flux at each altitude level                            =*
!=  FUP     - REAL, contribution of the diffuse upwelling component to    (O)=*
!=            the total actinic flux at each altitude level                  =*
!=  FDN     - REAL, contribution of the diffuse downwelling component to  (O)=*
!=            the total actinic flux at each altitude level                  =*
!=  EDR     - REAL, contribution of the direct component to the total     (O)=*
!=            spectral irradiance at each altitude level                     =*
!=  EUP     - REAL, contribution of the diffuse upwelling component to    (O)=*
!=            the total spectral irradiance at each altitude level           =*
!=  EDN     - REAL, contribution of the diffuse downwelling component to  (O)=*
!=            the total spectral irradiance at each altitude level           =*
!-----------------------------------------------------------------------------*

      use params_mod, only : pi, precis, largest

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: nlyr, nlev
      REAL, intent(in)    :: zen, rsfc
      REAL, intent(in)    :: tauu(nlyr), omu(nlyr), gu(nlyr)
      REAL, intent(in)    :: dsdh(0:nlyr,nlyr)
      INTEGER, intent(in) :: nid(0:nlyr)
      LOGICAL, intent(in) :: delta

      REAL, intent(out) :: fup(nlev), fdn(nlev), fdr(nlev)
      REAL, intent(out) :: eup(nlev), edn(nlev), edr(nlev)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      REAL, PARAMETER    :: eps = 1.E-3
!-----------------------------------------------------------------------------
! initial conditions:  pi*solar flux = 1;  diffuse incidence = 0
!-----------------------------------------------------------------------------
      REAL, PARAMETER    :: pifs = 1.
      REAL, PARAMETER    :: fdn0 = 0.

      REAL :: mu, sum
      REAL :: tausla(0:nlyr)
      REAL :: tauc(0:nlyr)
      REAL :: mu2(0:nlyr)

!-----------------------------------------------------------------------------
! internal coefficients and matrix
!-----------------------------------------------------------------------------
      INTEGER :: row, nlyrm1
      REAL :: lam(nlyr), taun(nlyr), bgam(nlyr)
      REAL :: e1(nlyr), e2(nlyr), e3(nlyr), e4(nlyr)
      REAL :: cup(nlyr), cdn(nlyr), cuptn(nlyr), cdntn(nlyr)
      REAL :: mu1(nlyr)
      REAL :: a(2*nlyr), b(2*nlyr), d(2*nlyr), e(2*nlyr), y(2*nlyr)

      REAL :: f, g, om, tmpg
      REAL :: gam1, gam2, gam3, gam4
      REAL :: gi(nlyr), omi(nlyr)

!-----------------------------------------------------------------------------
! For calculations of Associated Legendre Polynomials for GAMA1,2,3,4
! in delta-function, modified quadrature, hemispheric constant,
! Hybrid modified Eddington-delta function metods, p633,Table1.
! W.E.Meador and W.R.Weaver, GAS,1980,v37,p.630
! W.J.Wiscombe and G.W. Grams, GAS,1976,v33,p2440, 
! uncomment the following two lines and the appropriate statements further
! down.
!-----------------------------------------------------------------------------
      INTEGER :: mrows, mrowsm1, mrowsm2
      REAL    :: expon, expon0, expon1, divisr, tmp, up, dn
      REAL    :: ssfc
      REAL    :: wrk, wrk1

      INTEGER :: i, im1, j, k

!-----------------------------------------------------------------------------
! MU = cosine of solar zenith angle
! RSFC = surface albedo
! TAUU =  unscaled optical depth of each layer
! OMU  =  unscaled single scattering albedo
! GU   =  unscaled asymmetry factor
! KLEV = max dimension of number of layers in atmosphere
! NLYR = number of layers in the atmosphere
! NLEVEL = nlyr + 1 = number of levels
!-----------------------------------------------------------------------------

      mu = COS( zen*pi/180. )
!-----------------------------------------------------------------------------
!************* compute coefficients for each layer:
! GAM1 - GAM4 = 2-stream coefficients, different for different approximations
! EXPON0 = calculation of e when TAU is zero
! EXPON1 = calculation of e when TAU is TAUN
! CUP and CDN = calculation when TAU is zero
! CUPTN and CDNTN = calc. when TAU is TAUN
! DIVISR = prevents division by zero
!-----------------------------------------------------------------------------
      tauc(0:nlyr)   = 0.
      tausla(0:nlyr) = 0.
      mu2(0:nlyr)    = 1./SQRT(largest)

      IF( .NOT. delta ) THEN
        gi(1:nlyr)   = gu(1:nlyr)
        omi(1:nlyr)  = omu(1:nlyr)
        taun(1:nlyr) = tauu(1:nlyr)
      ELSE 
!-----------------------------------------------------------------------------
! delta-scaling. Have to be done for delta-Eddington approximation, 
! delta discrete ordinate, Practical Improved Flux Method, delta function,
! and Hybrid modified Eddington-delta function methods approximations
!-----------------------------------------------------------------------------
        DO k = 1, nlyr
          f       = gu(k)*gu(k)
          wrk     = 1. - f
          wrk1    = 1. - omu(k)*f
          gi(k)   = (gu(k) - f)/wrk
          omi(k)  = wrk*omu(k)/wrk1
          taun(k) = wrk1*tauu(k)
        ENDDO
      END IF

!-----------------------------------------------------------------------------
! calculate slant optical depth at the top of the atmosphere when zen>90.
! in this case, higher altitude of the top layer is recommended which can 
! be easily changed in gridz.f.
!-----------------------------------------------------------------------------
      IF( zen > 90.0 ) THEN
        IF(nid(0) < 0) THEN
          tausla(0) = largest
        ELSE
          sum = 0.0
          DO j = 1, nid(0)
            sum = sum + 2.*taun(j)*dsdh(0,j)
          END DO
          tausla(0) = sum 
        END IF
      END IF
  
layer_loop : &
      DO i = 1, nlyr
        im1 = i - 1
        g  = gi(i)
        om = omi(i)
        tauc(i) = tauc(im1) + taun(i)

!-----------------------------------------------------------------------------
! stay away from 1 by precision.  For g, also stay away from -1
!-----------------------------------------------------------------------------
        tmpg = MIN( abs(g),1. - precis )
        g    = SIGN( tmpg,g )
        om   = MIN( om,1. - precis )

!-----------------------------------------------------------------------------
! calculate slant optical depth
!-----------------------------------------------------------------------------
        IF(nid(i) < 0) THEN
          tausla(i) = largest
        ELSE
          sum = 0.0
          DO j = 1, MIN(nid(i),i)
             sum = sum + taun(j)*dsdh(i,j)
          ENDDO
          DO j = MIN(nid(i),i)+1,nid(i)
             sum = sum + 2.*taun(j)*dsdh(i,j)
          ENDDO
          tausla(i) = sum 
          IF(tausla(i) == tausla(im1)) THEN
            mu2(i) = SQRT(largest)
          ELSE
            mu2(i) = (tauc(i) - tauc(im1))/(tausla(i) - tausla(im1))
            mu2(i) = SIGN( MAX( ABS(mu2(i)),1./SQRT(largest) ), mu2(i) )
          END IF
        END IF

!-----------------------------------------------------------------------------
!** the following gamma equations are from pg 16,289, Table 1
!** save mu1 for each approx. for use in converting irradiance to actinic flux
! Eddington approximation(Joseph et al., 1976, JAS, 33, 2452):
!-----------------------------------------------------------------------------
        gam1 =  .25*(7. - om*(4. + 3.*g))
        gam2 = -.25*(1. - om*(4. - 3.*g))
        gam3 = .25*(2. - 3.*g*mu)
        gam4 = 1. - gam3
        mu1(i) = 0.5

!-----------------------------------------------------------------------------
! lambda = pg 16,290 equation 21
! big gamma = pg 16,290 equation 22
! checked limit for gam2/gam1 <<1:  bgam -> (1/2)*gma2/gam1
! so if if gam2 = 0., then bgam = 0. 
!-----------------------------------------------------------------------------
        lam(i) = sqrt(gam1*gam1 - gam2*gam2)

        IF( gam2 /= 0.) THEN
          bgam(i) = (gam1 - lam(i))/gam2
        ELSE
          bgam(i) = 0.
        ENDIF

        expon = EXP( -lam(i)*taun(i) )

!-----------------------------------------------------------------------------
! e1 - e4 = pg 16,292 equation 44
!-----------------------------------------------------------------------------
        e1(i) = 1. + bgam(i)*expon
        e2(i) = 1. - bgam(i)*expon
        e3(i) = bgam(i) + expon
        e4(i) = bgam(i) - expon

!-----------------------------------------------------------------------------
! the following sets up for the C equations 23, and 24
! found on page 16,290
! prevent division by zero (if LAMBDA=1/MU, shift 1/MU^2 by EPS = 1.E-3
! which is approx equiv to shifting MU by 0.5*EPS* (MU)**3
!-----------------------------------------------------------------------------
        expon0 = EXP( -tausla(im1) )
        expon1 = EXP( -tausla(i) )
          
        divisr = lam(i)*lam(i) - 1./(mu2(i)*mu2(i))
        tmp    = MAX( eps,abs(divisr) )
        divisr = SIGN( tmp,divisr )

        up = om*pifs*((gam1 - 1./mu2(i))*gam3 + gam4*gam2)/divisr
        dn = om*pifs*((gam1 + 1./mu2(i))*gam4 + gam2*gam3)/divisr
         
!-----------------------------------------------------------------------------
! cup and cdn are when tau is equal to zero
! cuptn and cdntn are when tau is equal to taun
!-----------------------------------------------------------------------------
        cup(i) = up*expon0
        cdn(i) = dn*expon0
        cuptn(i) = up*expon1
        cdntn(i) = dn*expon1
      end do layer_loop

!-----------------------------------------------------------------------------
!**************** set up matrix ******
! ssfc = pg 16,292 equation 37  where pi Fs is one (unity).
!-----------------------------------------------------------------------------
      ssfc = rsfc*mu*EXP( -tausla(nlyr) )*pifs

!-----------------------------------------------------------------------------
! MROWS = the number of rows in the matrix
!-----------------------------------------------------------------------------
      mrows   = 2*nlyr     
      mrowsm1 = mrows - 1
      mrowsm2 = mrows - 2
      nlyrm1  = nlyr - 1
      
!-----------------------------------------------------------------------------
! the following are from pg 16,292  equations 39 - 43.
! set up first row of matrix:
!-----------------------------------------------------------------------------
      a(1) = 0.
      b(1) = e1(1)
      d(1) = -e2(1)
      e(1) = fdn0 - cdn(1)

!-----------------------------------------------------------------------------
! set up odd rows 3 thru (MROWS - 1):
!-----------------------------------------------------------------------------
      a(3:mrowsm1:2) = e2(1:nlyrm1)*e3(1:nlyrm1) - e4(1:nlyrm1)*e1(1:nlyrm1)
      b(3:mrowsm1:2) = e1(1:nlyrm1)*e1(2:nlyr) - e3(1:nlyrm1)*e3(2:nlyr)
      d(3:mrowsm1:2) = e3(1:nlyrm1)*e4(2:nlyr) - e1(1:nlyrm1)*e2(2:nlyr)
      e(3:mrowsm1:2) = e3(1:nlyrm1)*(cup(2:nlyr) - cuptn(1:nlyrm1))  &
                     + e1(1:nlyrm1)*(cdntn(1:nlyrm1) - cdn(2:nlyr))

!-----------------------------------------------------------------------------
! set up even rows 2 thru (MROWS - 2): 
!-----------------------------------------------------------------------------
      a(2:mrowsm2:2) = e2(2:nlyr)*e1(1:nlyrm1) - e3(1:nlyrm1)*e4(2:nlyr)
      b(2:mrowsm2:2) = e2(1:nlyrm1)*e2(2:nlyr) - e4(1:nlyrm1)*e4(2:nlyr)
      d(2:mrowsm2:2) = e1(2:nlyr)*e4(2:nlyr) - e2(2:nlyr)*e3(2:nlyr)
      e(2:mrowsm2:2) = (cup(2:nlyr) - cuptn(1:nlyrm1))*e2(2:nlyr) & 
                     - (cdn(2:nlyr) - cdntn(1:nlyrm1))*e4(2:nlyr)

!-----------------------------------------------------------------------------
! set up last row of matrix at MROWS:
!-----------------------------------------------------------------------------
      a(mrows) = e1(nlyr) - rsfc*e3(nlyr)
      b(mrows) = e2(nlyr) - rsfc*e4(nlyr)
      d(mrows) = 0.
      e(mrows) = ssfc - cuptn(nlyr) + rsfc*cdntn(nlyr)

!-----------------------------------------------------------------------------
! solve tri-diagonal matrix:
!-----------------------------------------------------------------------------
      CALL tridag( a, b, d, e, y, mrows )

!-----------------------------------------------------------------------------
!*** unfold solution of matrix, compute output fluxes:
!-----------------------------------------------------------------------------
! the following equations are from pg 16,291  equations 31 & 32
!-----------------------------------------------------------------------------
      fdr(1) = EXP( -tausla(0) )
      edr(1) = mu * fdr(1)
      edn(1) = fdn0
      eup(1) = y(1)*e3(1) - y(2)*e4(1) + cup(1)
      fdn(1) = edn(1)/mu1(1)
      fup(1) = eup(1)/mu1(1)

      fdr(2:nlev) = EXP( -tausla(1:nlyr) )
      edr(2:nlev) = mu *fdr(2:nlev)
      edn(2:nlev) = y(1:mrowsm1:2)*e3(1:nlyr) + y(2:mrows:2)*e4(1:nlyr) + cdntn(1:nlyr)
      eup(2:nlev) = y(1:mrowsm1:2)*e1(1:nlyr) + y(2:mrows:2)*e2(1:nlyr) + cuptn(1:nlyr)
      fdn(2:nlev) = edn(2:nlev)/mu1(1:nlyr)
      fup(2:nlev) = eup(2:nlev)/mu1(1:nlyr)

      END SUBROUTINE ps2str

      SUBROUTINE tridag( a, b, c, r, u, n)
!-----------------------------------------------------------------------------
! solve tridiagonal system.  From Numerical Recipies, p. 40
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
!     ... dummy arguments
!-----------------------------------------------------------------------------
      INTEGER, intent(in) :: n
      REAL,    intent(in) :: a(n), b(n), c(n), r(n)
      REAL, intent(out)   :: u(n)

!-----------------------------------------------------------------------------
!     ... local variables
!-----------------------------------------------------------------------------
      INTEGER :: j, jm1, jp1
      REAL    :: bet
      REAL    :: gam(n)
      CHARACTER(len=64) :: err_msg

      IF (b(1) == 0.) then
        call wrf_error_fatal( 'tridag: zero pivot @ n == 1' )
      ENDIF
      bet  = b(1)
      u(1) = r(1)/bet
      DO j = 2, n   
         jm1 = j - 1
         gam(j) = c(jm1)/bet
         bet    = b(j) - a(j)*gam(j)
         IF (bet == 0.) then
           write(err_msg,'(''tridag: zero pivot @ n = '',i4)') j
           call wrf_error_fatal( trim(err_msg) )
         ENDIF
         u(j) = (r(j) - a(j)*u(jm1))/bet
      END DO

      DO j = n - 1, 1, -1  
         jp1 = j + 1
         u(j) = u(j) - gam(jp1)*u(jp1)
      END DO

      END SUBROUTINE tridag

      END MODULE rad_trans
