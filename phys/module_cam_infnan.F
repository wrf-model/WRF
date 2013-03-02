#define WRF_PORT
#define MODAL_AERO
! Updated to CESM1.0.3 (CAM5.1.01) by Balwinder.Singh@pnnl.gov

module infnan
!-------------------------------------------------------------------------
!
! Purpose:
!
! Set parameters for the floating point flags "inf" Infinity
! and "nan" not-a-number. As well as "bigint" the point
! at which integers start to overflow. These values are used
! to initialize arrays with as a way to detect if arrays
! are being used before being set.
!
! Author: CCM Core group
!
! $Id$
!
!-------------------------------------------------------------------------
  use shr_kind_mod, only: r8 => shr_kind_r8
#ifdef __PGI
! quiet nan for portland group compilers
  real(r8), parameter :: inf = O'0777600000000000000000'
  real(r8), parameter :: nan = O'0777700000000000000000'
  integer,  parameter :: bigint = O'17777777777'    
#elif __GNUC__
  real(r8), parameter :: inf = 1.e19
  real(r8), parameter :: nan = 1.e21
  integer,  parameter :: bigint = O'17777777777'
#else
! signaling nan otherwise
  real(r8), parameter :: inf = O'0777600000000000000000'
  real(r8), parameter :: nan = O'0777610000000000000000'
  integer,  parameter :: bigint = O'17777777777'           ! largest possible 32-bit integer
#endif
  real(r8), parameter :: uninit_r8 = inf                   ! uninitialized floating point number
end module infnan
