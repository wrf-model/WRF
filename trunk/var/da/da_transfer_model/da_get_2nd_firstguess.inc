 SUBROUTINE da_get_2nd_firstguess ( grid )

!-------------------------------------------------------------------------
!  Calculate the first guess at the end of thr time window
!  The original grid%u_2,v_2 etc. will be overwrittedm but we need the boundary area only
!  Author: Xin Zhang, 10/7/2010
!-------------------------------------------------------------------------

   IMPLICIT NONE

   TYPE(domain), INTENT(INOUT)        :: grid

#ifdef VAR4D

   INTEGER  :: spec_bdy_width, i, j

   spec_bdy_width =  grid%spec_bdy_width

   CALL da_calc_2nd_fg ( grid%moist(:,:,:,P_QV), grid%moist_bxs(:,:,:,P_QV), grid%moist_bxe(:,:,:,P_QV),     &
                                    grid%moist_bys(:,:,:,P_QV), grid%moist_bye(:,:,:,P_QV),     &
                                    grid%moist_btxs(:,:,:,P_QV), grid%moist_btxe(:,:,:,P_QV),     &
                                    grid%moist_btys(:,:,:,P_QV), grid%moist_btye(:,:,:,P_QV),     &
                                    REAL(interval_seconds), 'T' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL da_calc_2nd_fg ( grid%mu_2 , grid%mu_bxs, grid%mu_bxe, grid%mu_bys, grid%mu_bye,    &
                                    grid%mu_btxs, grid%mu_btxe, grid%mu_btys, grid%mu_btye,    &
                                    REAL(interval_seconds), 'M' , spec_bdy_width      ,               &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, 1, 1, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, 1, 1, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, 1, 1 )

   CALL da_calc_2nd_fg ( grid%u_2 , grid%u_bxs, grid%u_bxe, grid%u_bys, grid%u_bye, &
                                    grid%u_btxs, grid%u_btxe, grid%u_btys, grid%u_btye,    &
                                    REAL(interval_seconds), 'U' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL da_calc_2nd_fg ( grid%v_2 , grid%v_bxs, grid%v_bxe, grid%v_bys, grid%v_bye, &
                                    grid%v_btxs, grid%v_btxe, grid%v_btys, grid%v_btye,    &
                                    REAL(interval_seconds), 'V' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL da_calc_2nd_fg ( grid%t_2 , grid%t_bxs, grid%t_bxe, grid%t_bys, grid%t_bye, &
                                    grid%t_btxs, grid%t_btxe, grid%t_btys, grid%t_btye,    &
                                    REAL(interval_seconds), 'T' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL da_calc_2nd_fg ( grid%ph_2 , grid%ph_bxs, grid%ph_bxe, grid%ph_bys, grid%ph_bye, &
                                    grid%ph_btxs, grid%ph_btxe, grid%ph_btys, grid%ph_btye,    &
                                    REAL(interval_seconds), 'W' , spec_bdy_width      , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

#ifdef DM_PARALLEL
#include "HALO_EM_E.inc"
#endif

   CALL decouple ( grid%mu_2 , grid%mub , grid%u_2                 , 'u' , grid%msfuy , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL decouple ( grid%mu_2 , grid%mub , grid%v_2                 , 'v' , grid%msfvx , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL decouple ( grid%mu_2 , grid%mub , grid%t_2                 , 't' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL decouple ( grid%mu_2 , grid%mub , grid%ph_2                , 'h' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )

   CALL decouple ( grid%mu_2 , grid%mub , grid%moist(:,:,:,P_QV)      , 't' , grid%msfty , &
                                    grid%sd31, grid%ed31, grid%sd32, grid%ed32, grid%sd33, grid%ed33, &
                                    grid%sm31, grid%em31, grid%sm32, grid%em32, grid%sm33, grid%em33, &
                                    grid%sp31, grid%ep31, grid%sp32, grid%ep32, grid%sp33, grid%ep33 )
#endif

 END SUBROUTINE da_get_2nd_firstguess
