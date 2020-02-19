  subroutine da_radzicevar_prepare_interceptpara(in0s,in0g,in0r,rn0_s,rn0_g,rn0_r,sonv,gonv,ronv)
   !----------------------------------------------------------------------
   ! purpose: assigne the intercept parameter value and convert the unit 
   !----------------------------------------------------------------------
    implicit none
    integer :: in0s,in0g,in0r
    real    :: rn0_s,rn0_g,rn0_r
    real    :: sonv,gonv,ronv

    if (in0s.eq.1) then
       sonv = rn0_s  ! for diagnosing n0 from number concentration, not yet completed
    else
       sonv = rn0_s
    endif

    if (in0g.eq.1) then
      gonv = rn0_g  ! for diagnosing n0 from number concentration, not yet completed
    else
      gonv = rn0_g
    endif

    if (in0r.eq.1) then
      ronv = rn0_r  ! for diagnosing n0 from number concentration, not yet completed
    else
      ronv = rn0_r
    endif

    ronv=ronv*1.e-12   ! convert unit
    gonv=gonv*1.e-12   ! convert unit
    sonv=sonv*1.e-12   ! convert unit
  end subroutine da_radzicevar_prepare_interceptpara

