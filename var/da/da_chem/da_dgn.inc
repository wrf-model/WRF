subroutine da_dgn( grid, cv_size, xbx, be, iv, xhat, cv, ob, config_flags, ja_total )

   !---------------------------------------------------------------------------
   ! Purpose: Scales the cv emission increments to improve convergence.
   ! Reference: damped Gauss Newton; Kelley (1999)
   !
   ! Creation: 07/25/2016           JJ Guerrette 
   !
   !---------------------------------------------------------------------------

   implicit none

   type (domain),       intent(inout) :: grid
   integer, intent(in)                :: cv_size    ! Total cv size.
   type (xbx_type),intent(inout)      :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (iv_type), intent(inout)      :: iv     ! innovation vector (o-b).
   real, intent(inout)                :: xhat(1:cv_size) ! control variables.
   real, intent(in)                   :: cv(1:cv_size)   ! control variables.
   type (y_type) , intent(inout)      :: ob          ! Observation structure.
   type(grid_config_rec_type), intent(inout) :: config_flags
   real, intent(in)                   :: ja_total ! Cost function at eta = 0

   real             :: eta, grat, &
                       eta_a,   eta_b,   eta_c,   eta_d, &
                       ja, jb, jc, jd, jout
                       
   integer          :: cv_s, cv_e, cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, &
                       itgold, branch

   real             :: tol = 0.05

   if (trace_use) call da_trace_entry("da_dgn")

   write(unit=stdout,fmt='("Starting Damped Gauss Newton for emission CVs")')

   ! Determine the scaling factor CV indices

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


   ! Carry out the line search using golden section algorithm
   grat = (sqrt(5.D0) - 1.D0) / 2.D0 ! Golden Ratio

   ja = ja_total
   jb = 0.D0
   jc = 0.D0
   jd = 0.D0

   eta_a = 0.D0
   eta_b = 1.D0
   eta_c = eta_b - grat * (eta_b - eta_a)
   eta_d = eta_a + grat * (eta_b - eta_a)

   ! Don't need to calculate initial jb (@ eta=1.) except to demonstrate need for DGN
   ! Could use jb/ja > 1 as criteria for needing DGN
   call da_evaluate_j( grid, config_flags, cv_size, be%cv%size_jb, xbx, be, iv, xhat, cv, ob, &
                       cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, eta_b, jb)
   write(unit=stdout,fmt='("DGN: eta_b          jb")')
   write(unit=stdout,fmt='("DGN:",1F6.3,1F11.3)') eta_b,jb

   call da_evaluate_j( grid, config_flags, cv_size, be%cv%size_jb, xbx, be, iv, xhat, cv, ob, &
                       cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, eta_c, jc)
   write(unit=stdout,fmt='("DGN: eta_c          jc")')
   write(unit=stdout,fmt='("DGN:",1F6.3,1F11.3)') eta_c,jc

   eta = eta_d

   branch = 2
   itgold = 0
   do while (eta_b - eta_a .gt. tol)
      itgold = itgold + 1
      call da_evaluate_j( grid, config_flags, cv_size, be%cv%size_jb, xbx, be, iv, xhat, cv, ob, &
                          cv_s_ant, cv_e_ant, cv_s_bb, cv_e_bb, eta, jout)
      if (branch .eq. 1) then
         jc = jout
      else
         jd = jout
      end if
      write(unit=stdout,fmt='("DGN: i  eta_a eta_b       ja          jb          jc          jd")')
      write(unit=stdout,fmt='("DGN:",I4,2F6.3,4F11.3)') itgold,eta_a,eta_b,ja,jb,jc,jd


      if (jc .lt. jd) then
         eta_b = eta_d
         jb = jd
         eta_d = eta_c
         jd = jc
         eta_c = eta_b - grat * (eta_b - eta_a)
         eta = eta_c
         branch = 1
      else
         eta_a = eta_c
         ja = jc
         eta_c = eta_d
         jc = jd
         eta_d = eta_a + grat * (eta_b - eta_a)
         eta = eta_d
         branch = 2
      end if


      if (eta .gt. (1.d0 - tol)) then
         exit
      end if
   end do

   if (eta .gt. (1.d0 - tol)) then
      eta = 1.D0
   else
      eta = (eta_a + eta_b) / 2.D0
   end if

   write(unit=stdout,fmt='("Final eta from DGN: ",F8.5)') eta

   if ( cv_e_ant - cv_s_ant .gt. 0 ) xhat(cv_s_ant:cv_e_ant) = eta * xhat(cv_s_ant:cv_e_ant)
   if ( cv_e_bb - cv_s_bb .gt. 0 ) xhat(cv_s_bb:cv_e_bb) = eta * xhat(cv_s_bb:cv_e_bb)

   if (trace_use) call da_trace_exit("da_dgn")

end subroutine da_dgn
