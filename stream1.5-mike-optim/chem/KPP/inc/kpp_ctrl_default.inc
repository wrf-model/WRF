! 
!  include file: kpp_ctrl_default.inc
!
!
! BEWARE: currently the setting in this file are 
!  used for ALL solvers and mechanisms 
!
!  This file is designed for rosenbrock type solvers   
!
!  if changed, please make sure that interface routines are recompiled 
!

! choose AUTONOMOUS option
   ICNTRL(1) = 1

! choose ros3
   ICNTRL(3) = 2

! set HSTART value: 
   RCNTRL(3) = 0.01_dp * TIME_END



!  atol and rtol are currently set in ../module_wkppc_constants.F

! end include file kpp_ctrl_default.inc

