MODULE module_vertmx_wrf

CONTAINS

    SUBROUTINE vertmx(dt,phi,kt_turb,zsigma,zsigma_half,vd,kts,kte)
!  !! to calculate change in time of phi due to vertical mixing
!  !! Mariusz Pagowski, March 2001
!  !! conventions used:
!  !! input is lower case
!  !! output is upper case
! .. Scalar Arguments ..
      REAL :: dt, vd
      INTEGER :: kts,kte
! ..
! .. Array Arguments ..
      REAL, DIMENSION (kts:kte+1) :: kt_turb, zsigma
      REAL, DIMENSION (kts:kte) :: phi, zsigma_half
! ..
! .. Local Scalars ..
      INTEGER :: k
! ..
! .. Local Arrays ..
      REAL, DIMENSION (kts:kte+1) :: a_coeff
      REAL, DIMENSION (kts:kte) :: b_coeff, lhs1, lhs2, lhs3, rhs
! ..
! .. External Subroutines ..
!     EXTERNAL coeffs, rlhside, tridiag
! ..
      CALL coeffs(kts,kte+1,zsigma,zsigma_half,a_coeff,b_coeff)

      CALL rlhside(kts,kte+1,kt_turb,a_coeff,b_coeff,phi,dt,vd,rhs,lhs1,lhs2,lhs3)

      CALL tridiag(kts,kte,lhs1,lhs2,lhs3,rhs)

      DO k = kts,kte
        phi(k) = rhs(k)
      END DO

    END SUBROUTINE vertmx
    SUBROUTINE rlhside(kts,kte,k_turb,a_coeff,b_coeff,phi,dt,vd,rhs,lhs1,lhs2,lhs3)
  !! to calculate right and left hand sides in diffusion equation
  !! for the tridiagonal solver
  !! Mariusz Pagowski, March 2001
  !! conventions used:
  !! input is lower case
  !! output is upper case
! .. Scalar Arguments ..
      REAL :: dt, vd
      INTEGER :: kts,kte
! ..
! .. Array Arguments ..
      REAL, DIMENSION (kts:kte) :: a_coeff, k_turb
      REAL, DIMENSION (kts:kte-1) :: b_coeff, lhs1, lhs2, lhs3, phi, rhs
! ..
! .. Local Scalars ..
      REAL :: a1, a2, alfa_explicit = .25, beta_implicit = .75
      INTEGER :: i
! ..
!     rhs(1)=phi(1) !! this should be really the n+1 step
      rhs(1) = 0.
      rhs(1) = (1./(dt*b_coeff(1))-alfa_explicit*(a_coeff(2)*k_turb(2)+vd))* &
        phi(1) + alfa_explicit*a_coeff(2)*k_turb(2)*phi(2)
      lhs1(1) = 0.
!     lhs2(1)=1.
!     lhs3(1)=0.
!     lhs3(1)=-1.
      lhs2(1) = 1./(dt*b_coeff(1)) + beta_implicit*(a_coeff(2)*k_turb(2)+vd)
      lhs3(1) = -beta_implicit*a_coeff(2)*k_turb(2)

      DO i = kts+1, kte - 2
        a1 = a_coeff(i)*k_turb(i)
        a2 = a_coeff(i+1)*k_turb(i+1)
        rhs(i) = (1./(dt*b_coeff(i))-alfa_explicit*(a1+a2))*phi(i) + &
          alfa_explicit*(a1*phi(i-1)+a2*phi(i+1))

        lhs1(i) = -beta_implicit*a1
        lhs2(i) = 1./(dt*b_coeff(i)) + beta_implicit*(a1+a2)
        lhs3(i) = -beta_implicit*a2
      END DO

      !! zero flux at the top

      rhs(kte-1) = 0.
      lhs1(kte-1) = 1.
      lhs2(kte-1) = -1.
      lhs3(kte-1) = 0.

    END SUBROUTINE rlhside










    SUBROUTINE tridiag(kts,kte,a,b,c,f)
  !! to solve system of linear eqs on tridiagonal matrix n times n
  !! after Peaceman and Rachford, 1955
  !! a,b,c,F - are vectors of order n
  !! a,b,c - are coefficients on the LHS
  !! F - is initially RHS on the output becomes a solution vector
  !! Mariusz Pagowski, March 2001
  !! conventions used:
  !! input is lower case
  !! output is upper case
! .. Scalar Arguments ..
      INTEGER :: kts,kte
! ..
! .. Array Arguments ..
      REAL, DIMENSION (kts:kte) :: a, b, c, f
! ..
! .. Local Scalars ..
      REAL :: p
      INTEGER :: i
! ..
! .. Local Arrays ..
      REAL, DIMENSION (kts:kte) :: q
! ..
      c(kte) = 0.
      q(1) = -c(1)/b(1)
      f(1) = f(1)/b(1)

      DO i = kts+1, kte
        p = 1./(b(i)+a(i)*q(i-1))
        q(i) = -c(i)*p
        f(i) = (f(i)-a(i)*f(i-1))*p
      END DO

      DO i = kte - 1, kts, -1
        f(i) = f(i) + q(i)*f(i+1)
      END DO

    END SUBROUTINE tridiag







    SUBROUTINE coeffs(kts,kte,z_sigma,z_sigma_half,a_coeff,b_coeff)
! !! to calculate coefficients in diffusion equation
! !! Mariusz Pagowski, March 2001
! !! conventions used:
! !! input is lower case
! !! output is upper case
! .. Scalar Arguments ..
      INTEGER :: kts,kte
! ..
! .. Array Arguments ..
      REAL, DIMENSION (kts:kte) :: a_coeff, z_sigma
      REAL, DIMENSION (kts:kte-1) :: b_coeff, z_sigma_half
! ..
! .. Local Scalars ..
      REAL :: any
      INTEGER :: i
! ..
      any = 1.

      a_coeff(1) = any
      b_coeff(1) = 1./(z_sigma(2)-z_sigma(1))

      DO i = kts+1, kte - 1
        a_coeff(i) = 1./(z_sigma_half(i)-z_sigma_half(i-1))
        b_coeff(i) = 1./(z_sigma(i+1)-z_sigma(i))
      END DO

      a_coeff(kte) = any

    END SUBROUTINE coeffs
END MODULE module_vertmx_wrf
