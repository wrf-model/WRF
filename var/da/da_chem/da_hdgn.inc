subroutine da_hdgn(it, cv_size, be, xhat)

   !---------------------------------------------------------------------------
   ! Purpose: Scales the cv emission increments to improve convergence
   ! Note: Requires user specification of "eta", and is non-optimal
   ! Reference: Heuristic damped Gauss Newton; Guerrette and Henze (2016)
   !
   ! Creation: 07/25/2016           JJ Guerrette 
   !
   !---------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: it     ! external iteration #.
   integer, intent(in)                :: cv_size    ! Total cv size.
   type (be_type), intent(in)         :: be     ! background error structure.
   real, intent(inout)                :: xhat(1:cv_size) ! control variables.

   real             :: eta
   integer          :: cv_s, cv_e, cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb

   if (trace_use) call da_trace_entry("da_hdgn")

!   eta = 1.D0
   eta = max(min(eta_emiss + (1.D0-eta_emiss) * REAL(it-1,8)/REAL(max_ext_its-1,8), 1.D0), 0.D0)

   cv_s = 1
   cv_e = cv_s &
          + be % cv % size1 &
          + be % cv % size2 &
          + be % cv % size3 &
          + be % cv % size4 &
          + be % cv % size5 &
#ifdef CLOUD_CV
          + be % cv % size6 &
          + be % cv % size7 &
          + be % cv % size8 &
          + be % cv % size9 &
#endif
          + be%cv%size_je &
          - 1

   cv_s_ant = cv_e + 1
   cv_e_ant = cv_s_ant + sum(be % cv % sizeant (:)) - 1
   cv_s_bb  = cv_e_ant + 1
   cv_e_bb = cv_s_bb + sum(be % cv % sizebb (:)) - 1

   write(unit=stdout,fmt='("cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, cv_size, size_jb: ",6I8)') cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, cv_size, be % cv % size_jb

   if ( cv_e_ant - cv_s_ant .gt. 0 ) xhat(cv_s_ant:cv_e_ant) = eta * xhat(cv_s_ant:cv_e_ant)
   if ( cv_e_bb - cv_s_bb .gt. 0 ) xhat(cv_s_bb:cv_e_bb) = eta * xhat(cv_s_bb:cv_e_bb)

   if (trace_use) call da_trace_exit("da_hdgn")

end subroutine da_hdgn
