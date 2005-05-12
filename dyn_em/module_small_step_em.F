!WRF:MODEL_LAYER:DYNAMICS
!

!  SMALL_STEP code for the geometric height coordinate model
!
!---------------------------------------------------------------------------

MODULE module_small_step_em

   USE module_configure
   USE module_model_constants

CONTAINS

!----------------------------------------------------------------------

SUBROUTINE small_step_prep( u_1, u_2, v_1, v_2, w_1, w_2, &
                            t_1, t_2, ph_1, ph_2,         &
                            mub, mu_1, mu_2,              &
                            muu, muus, muv, muvs,         &
                            mut, muts, mudf,              &
                            u_save, v_save, w_save,       &
                            t_save, ph_save, mu_save,     &
                            ww, ww_save,                  &
                            dnw, c2a, pb, p, alt,         &
                            msfu, msfv, msft,             &
                            rk_step, leapfrog,            &
                            ids,ide, jds,jde, kds,kde,    & 
                            ims,ime, jms,jme, kms,kme,    &
                            its,ite, jts,jte, kts,kte    )

  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: rk_step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_1,   &
                                                              v_1,   &
                                                              w_1,   &
                                                              t_1,   &
                                                              ph_1

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: u_2,   &
                                                              v_2,   &
                                                              w_2,   &
                                                              t_2,   &
                                                              ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(  OUT) :: c2a, &
                                                               ww_save

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) ::  pb,  &
                                                                p,   &
                                                                alt, &
                                                                ww

! pjj/cray
! REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INOUT) :: mu_1
  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INOUT) :: mu_1,mu_2

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(INout) :: mub,  &
                                                               muu,  &
                                                               muv,  &
                                                               mut,  &
                                                               msfu, &
                                                               msfv, &
                                                               msft

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: muus, &
                                                               muvs, &
                                                               muts, &
!pjj/cray
!                                                              mu_2, &
                                                               mudf

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(  OUT) :: mu_save

  REAL, DIMENSION(kms:kme, jms:jme)         , INTENT(IN   ) :: dnw

  LOGICAL, INTENT(IN   ) :: leapfrog

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv

!<DESCRIPTION>
!
!  small_step_prep prepares the prognostic variables for the small timestep.
!  This includes switching time-levels in the arrays and computing coupled 
!  perturbation variables for the small timestep 
!  (i.e. mu*u" = mu(t)*u(t)-mu(*)*u(*); mu*u" is advanced during the small 
!  timesteps
!
!</DESCRIPTION>

    i_start = its
    i_end   = ite
    j_start = jts
    j_end   = jte
    k_start = kts
    k_end = min(kte,kde-1)

    i_endu = i_end
    j_endv = j_end

    IF(i_end == ide) i_end = i_end - 1
    IF(j_end == jde) j_end = j_end - 1

    !  if this is the first RK step, reset *_1 to *_2
    !  (we are replacing the t-dt fields with the time t fields)

    IF ((rk_step == 1) .and. (.not.leapfrog)) THEN

! 1 jun 2001 -> added boundary copy to 2D boundary condition routines,
!  should be OK now without the following data copy
!#if 0
!     DO j=j_start, j_end
!       mu_2(0,j)=mu_2(1,j)
!       mu_2(i_endu,j)=mu_2(i_end,j)
!       mu_1(0,j)=mu_2(1,j)
!       mu_1(i_endu,j)=mu_2(i_end,j)
!       mub(0,j)=mub(1,j)
!       mub(i_endu,j)=mub(i_end,j)
!     ENDDO
!     DO i=i_start, i_end
!       mu_2(i,0)=mu_2(i,1)
!       mu_2(i,j_endv)=mu_2(i,j_end)
!       mu_1(i,0)=mu_2(i,1)
!       mu_1(i,j_endv)=mu_2(i,j_end)
!       mub(i,0)=mub(i,1)
!       mub(i,j_endv)=mub(i,j_end)
!     ENDDO
!#endif

      DO j=j_start, j_end
      DO i=i_start, i_end
        mu_1(i,j)=mu_2(i,j)
        ww_save(i,kde,j) = 0.
        ww_save(i,1,j) = 0.
        mudf(i,j) = 0.  !  initialize external mode div damp to zero
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, k_end
      DO i=i_start, i_endu
        u_1(i,k,j) = u_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_endv
      DO k=k_start, k_end
      DO i=i_start, i_end
        v_1(i,k,j) = v_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, k_end
      DO i=i_start, i_end
        t_1(i,k,j) = t_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

      DO j=j_start, j_end
      DO k=k_start, min(kde,kte)
      DO i=i_start, i_end
        w_1(i,k,j)  = w_2(i,k,j)
        ph_1(i,k,j) = ph_2(i,k,j)
      ENDDO
      ENDDO
      ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        muts(i,j)=mub(i,j)+mu_2(i,j)
      ENDDO
      DO i=i_start, i_endu
!  rk_step==1, not leapfrog, WCS fix for tiling
!        muus(i,j)=0.5*(mub(i,j)+mu_2(i,j)+mub(i-1,j)+mu_2(i-1,j))
        muus(i,j) = muu(i,j)
      ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO i=i_start, i_end
!  rk_step==1, not leapfrog, WCS fix for tiling
!      muvs(i,j)=0.5*(mub(i,j)+mu_2(i,j)+mub(i,j-1)+mu_2(i,j-1))
        muvs(i,j) = muv(i,j)
    ENDDO
    ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        mu_save(i,j)=mu_2(i,j)
        mu_2(i,j)=mu_2(i,j)-mu_2(i,j)
      ENDDO
    ENDDO

    ELSE

    DO j=j_start, j_end
      DO i=i_start, i_end
        muts(i,j)=mub(i,j)+mu_1(i,j)
      ENDDO
      DO i=i_start, i_endu
        muus(i,j)=0.5*(mub(i,j)+mu_1(i,j)+mub(i-1,j)+mu_1(i-1,j))
      ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO i=i_start, i_end
      muvs(i,j)=0.5*(mub(i,j)+mu_1(i,j)+mub(i,j-1)+mu_1(i,j-1))
    ENDDO
    ENDDO

    DO j=j_start, j_end
      DO i=i_start, i_end
        mu_save(i,j)=mu_2(i,j)
        mu_2(i,j)=mu_1(i,j)-mu_2(i,j)
      ENDDO
    ENDDO


    END IF

    ! set up the small timestep variables

      DO j=j_start, j_end
      DO i=i_start, i_end
        ww_save(i,kde,j) = 0.
        ww_save(i,1,j) = 0.
      ENDDO
      ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_end
      c2a(i,k,j) = cpovcv*(pb(i,k,j)+p(i,k,j))/alt(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_endu
      u_save(i,k,j) = u_2(i,k,j)
      u_2(i,k,j) = (muus(i,j)*u_1(i,k,j)-muu(i,j)*u_2(i,k,j))/msfu(i,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_endv
    DO k=k_start, k_end
    DO i=i_start, i_end
      v_save(i,k,j) = v_2(i,k,j)
      v_2(i,k,j) = (muvs(i,j)*v_1(i,k,j)-muv(i,j)*v_2(i,k,j))/msfv(i,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end
    DO k=k_start, k_end
    DO i=i_start, i_end
      t_save(i,k,j) = t_2(i,k,j)
      t_2(i,k,j) = muts(i,j)*t_1(i,k,j)-mut(i,j)*t_2(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO j=j_start, j_end
!    DO k=k_start, min(kde,kte)
    DO k=k_start, kde
    DO i=i_start, i_end
      w_save(i,k,j) = w_2(i,k,j)
      w_2(i,k,j)  = (muts(i,j)* w_1(i,k,j)-mut(i,j)* w_2(i,k,j))/msft(i,j)
      ph_save(i,k,j) = ph_2(i,k,j)
      ph_2(i,k,j) = ph_1(i,k,j)-ph_2(i,k,j)
    ENDDO
    ENDDO
    ENDDO

      DO j=j_start, j_end
!      DO k=k_start, min(kde,kte)
      DO k=k_start, kde
      DO i=i_start, i_end
        ww_save(i,k,j) = ww(i,k,j)
      ENDDO
      ENDDO
      ENDDO

END SUBROUTINE small_step_prep

!-------------------------------------------------------------------------


SUBROUTINE small_step_finish( u_2, u_1, v_2, v_1, w_2, w_1,    &
                              t_2, t_1, ph_2, ph_1, ww, ww1,   &
                              mu_2, mu_1,                      &
                              mut, muts, muu, muus, muv, muvs, &
                              u_save, v_save, w_save,          &
                              t_save, ph_save, mu_save,        &
                              msfu, msfv, msft,                &
                              h_diabatic,                      &
                              number_of_small_timesteps,dts,   &
                              rk_step, rk_order,               &
                              ids,ide, jds,jde, kds,kde,       &
                              ims,ime, jms,jme, kms,kme,       &
                              its,ite, jts,jte, kts,kte       )


  IMPLICIT NONE  ! religion first

!  stuff passed in

  INTEGER,                  INTENT(IN   ) :: ids,ide, jds,jde, kds,kde
  INTEGER,                  INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER,                  INTENT(IN   ) :: its,ite, jts,jte, kts,kte
  INTEGER,                  INTENT(IN   ) :: number_of_small_timesteps
  INTEGER,                  INTENT(IN   ) :: rk_step, rk_order
  REAL,                     INTENT(IN   ) :: dts

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: u_1, &
                                                                 v_1, &
                                                                 w_1, &
                                                                 t_1, &
                                                                 ww1, &
                                                                 ph_1

  REAL,   DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: u_2, &
                                                                 v_2, &
                                                                 w_2, &
                                                                 t_2, &
                                                                 ww,  &
                                                                 ph_2

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: u_save,   &
                                                              v_save,   &
                                                              w_save,   &
                                                              t_save,   &
                                                              ph_save,  &
                                                             h_diabatic

  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: muus, muvs
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mu_2, mu_1
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(INOUT) :: mut, muts, &
                                                        muu, muv, mu_save
  REAL,   DIMENSION(ims:ime, jms:jme), INTENT(IN   ) :: msfu, msfv, msft


! local stuff

  INTEGER         :: i,j,k
  INTEGER :: i_start, i_end, j_start, j_end, i_endu, j_endv

!<DESCRIPTION>
!
!  small_step_finish reconstructs the full uncoupled prognostic variables
!  from the coupled perturbation variables used in the small timesteps.
!
!</DESCRIPTION>

  i_start = its
  i_end   = ite
  j_start = jts
  j_end   = jte

  i_endu = i_end
  j_endv = j_end

  IF(i_end == ide) i_end = i_end - 1
  IF(j_end == jde) j_end = j_end - 1


! 1 jun 2001 -> added boundary copy to 2D boundary condition routines,
!  should be OK now without the following data copy

!#if 0
! DO j=j_start, j_end
!   muts(0,j)=muts(1,j)
!   muts(i_endu,j)=muts(i_end,j)
! ENDDO
! DO i=i_start, i_end
!   muts(i,0)=muts(i,1)
!   muts(i,j_endv)=muts(i,j_end)
! ENDDO

! DO j = j_start, j_endv
! DO i = i_start, i_end
!   muvs(i,j) = 0.5*(muts(i,j) + muts(i,j-1))
! ENDDO
! ENDDO

! DO j = j_start, j_end
! DO i = i_start, i_endu
!   muus(i,j) = 0.5*(muts(i,j) + muts(i-1,j))
! ENDDO
! ENDDO
!#endif

!    addition of time level t back into variables

  DO j = j_start, j_endv
  DO k = kds, kde-1
  DO i = i_start, i_end
    v_2(i,k,j) = (msfv(i,j)*v_2(i,k,j) + v_save(i,k,j)*muv(i,j))/muvs(i,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO k = kds, kde-1
  DO i = i_start, i_endu
    u_2(i,k,j) = (msfu(i,j)*u_2(i,k,j) + u_save(i,k,j)*muu(i,j))/muus(i,j)
  ENDDO
  ENDDO
  ENDDO

  DO j = j_start, j_end
  DO k = kds, kde
  DO i = i_start, i_end
    w_2(i,k,j) = (msft(i,j)*w_2(i,k,j) + w_save(i,k,j)*mut(i,j))/muts(i,j)
    ph_2(i,k,j) = ph_2(i,k,j) + ph_save(i,k,j)
    ww(i,k,j) = ww(i,k,j) + ww1(i,k,j)
  ENDDO
  ENDDO
  ENDDO

#ifdef REVERT
  DO j = j_start, j_end
  DO k = kds, kde-1
  DO i = i_start, i_end
    t_2(i,k,j) = (t_2(i,k,j) + t_save(i,k,j)*mut(i,j))/muts(i,j)
  ENDDO
  ENDDO
  ENDDO
#else
  IF ( rk_step < rk_order ) THEN
    DO j = j_start, j_end
    DO k = kds, kde-1
    DO i = i_start, i_end
      t_2(i,k,j) = (t_2(i,k,j) + t_save(i,k,j)*mut(i,j))/muts(i,j)
    ENDDO
    ENDDO
    ENDDO
  ELSE

    DO j = j_start, j_end
    DO k = kds, kde-1
    DO i = i_start, i_end
      t_2(i,k,j) = (t_2(i,k,j) - dts*number_of_small_timesteps*mut(i,j)*h_diabatic(i,k,j) &
                    + t_save(i,k,j)*mut(i,j))/muts(i,j)
    ENDDO
    ENDDO
    ENDDO
  ENDIF
#endif

  DO j = j_start, j_end
  DO i = i_start, i_end
    mu_2(i,j) = mu_2(i,j) + mu_save(i,j)
  ENDDO
  ENDDO

END SUBROUTINE small_step_finish

!-----------------------------------------------------------------------

SUBROUTINE calc_p_rho( al, p, ph,                    &
                       alt, t_2, t_1, c2a, pm1,      &
                       mu, muts, znu, t0,            &
                       rdnw, dnw, smdiv,             &
                       non_hydrostatic, step,        &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its,ite, jts,jte, kts,kte    )

  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(  OUT) :: al,   &
                                                               p
! pjj/cray
!                                                             p,    &
!                                                             pm1

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(IN   ) :: alt,   &
                                                              t_2,   &
                                                              t_1,   &
                                                              c2a

! pjj/cray
! REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph
  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),INTENT(INOUT) :: ph, pm1

  REAL, DIMENSION(ims:ime, jms:jme)         , INTENT(IN   ) :: mu,   &
                                                               muts

  REAL, DIMENSION(kms:kme)         , INTENT(IN   ) :: dnw,  &
                                                      rdnw, &
                                                      znu

  REAL,                                       INTENT(IN   ) :: t0, smdiv

  LOGICAL, INTENT(IN   )  :: non_hydrostatic

! local variables

  INTEGER :: i, j, k
  INTEGER :: i_start, i_end, j_start, j_end, k_start, k_end
  REAL    :: ptmp

!<DESCRIPTION>
!
!  For the nonhydrostatic option,
!  calc_p_rho computes the perturbation inverse density and 
!  perturbation pressure from the hydrostatic relation and the 
!  linearized equation of state, respectively.
!
!  For the hydrostatic option,
!  calc_p_rho computes the perturbation pressure, perturbation density,
!  and perturbation geopotential
!  from the vertical coordinate definition, linearized equation of state 
!  and the hydrostatic relation, respectively.
!
!  forward weighting of the pressure (divergence damping) is also
!  computed here.
!
!</DESCRIPTION>

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = jte
   k_start = kts
   k_end = min(kte,kde-1)

   IF(i_end == ide) i_end = i_end - 1
   IF(j_end == jde) j_end = j_end - 1

   IF (non_hydrostatic) THEN
     DO j=j_start, j_end
     DO k=k_start, k_end
     DO i=i_start, i_end

!  al computation is all dry, so ok with moisture

      al(i,k,j)=-1./muts(i,j)*(alt(i,k,j)*mu(i,j)  &
             +rdnw(k)*(ph(i,k+1,j)-ph(i,k,j)))

!  this is temporally linearized p, no moisture correction needed

      p(i,k,j)=c2a(i,k,j)*(alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))  &
                       /(muts(i,j)*(t0+t_1(i,k,j)))-al (i,k,j))

     ENDDO
     ENDDO
     ENDDO

   ELSE  ! hydrostatic calculation

       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         p(i,k,j)=mu(i,j)*znu(k)
         al(i,k,j)=alt(i,k,j)*(t_2(i,k,j)-mu(i,j)*t_1(i,k,j))            &
                      /(muts(i,j)*(t0+t_1(i,k,j)))-p(i,k,j)/c2a(i,k,j)
         ph(i,k+1,j)=ph(i,k,j)-dnw(k)*(muts(i,j)*al (i,k,j)              &
                          +mu(i,j)*alt(i,k,j))
       ENDDO
       ENDDO
       ENDDO

   END IF

!  divergence damping setup
 
     IF (step == 0) then   ! we're initializing small timesteps
       DO j=j_start, j_end
       DO k=k_start, k_end
       DO i=i_start, i_end
         pm1(i,k,j)=p(i,k,j)
       ENDDO
       ENDDO
       ENDDO 
     ELSE                     ! we're in the small timesteps 
       DO j=j_start, j_end    ! and adding div damping component
       DO k=k_start, k_end
       DO i=i_start, i_end
         ptmp = p(i,k,j)
         p(i,k,j) = p(i,k,j) + smdiv*(p(i,k,j)-pm1(i,k,j))
         pm1(i,k,j) = ptmp
       ENDDO
       ENDDO
       ENDDO
     END IF

END SUBROUTINE calc_p_rho

!----------------------------------------------------------------------

SUBROUTINE calc_coef_w( a,alpha,gamma,              &
                        mut, cqw,                   &
                        rdn, rdnw, c2a,             &
                        dts, g, epssm,              &
                        ids,ide, jds,jde, kds,kde,  & ! domain dims
                        ims,ime, jms,jme, kms,kme,  & ! memory dims
                        its,ite, jts,jte, kts,kte  )  ! tile   dims
                                                   
      IMPLICIT NONE  ! religion first

!  passed in through the call

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN   ) :: c2a,  &
                                                               cqw

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(INOUT) :: alpha, &
                                                               gamma, &
                                                               a

  REAL, DIMENSION(ims:ime, jms:jme),          INTENT(IN   ) :: mut

  REAL, DIMENSION(kms:kme),                   INTENT(IN   ) :: rdn,   &
                                                               rdnw

  REAL,                                       INTENT(IN   ) :: epssm, &
                                                               dts,   &
                                                               g

!  Local stack data.

  REAL, DIMENSION(ims:ime)                         :: cof
  REAL  :: b, c

  INTEGER :: i, j, k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: ij, ijp, ijm

!<DESCRIPTION>
!
!  calc_coef_w calculates the coefficients needed for the 
!  implicit solution of the vertical momentum and geopotential equations.
!  This requires solution of a tri-diagonal equation.
!
!</DESCRIPTION>

                                                 
      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = jte
      k_start = kts
      k_end   = kte-1

      IF(j_end == jde) j_end = j_end - 1
      IF(i_end == ide) i_end = i_end - 1

     outer_j_loop:  DO j = j_start, j_end

        DO i = i_start, i_end
          cof(i)  = (.5*dts*g*(1.+epssm)/mut(i,j))**2
          a(i, 2 ,j) = 0.
          a(i,kde,j) =-2.*cof(i)*rdnw(kde-1)**2*c2a(i,kde-1,j)
          gamma(i,1 ,j) = 0.
        ENDDO

        DO k=3,kde-1
        DO i=i_start, i_end
          a(i,k,j) =   -cqw(i,k,j)*cof(i)*rdn(k)* rdnw(k-1)*c2a(i,k-1,j)    
        ENDDO
        ENDDO


        DO k=2,kde-1
        DO i=i_start, i_end
          b = 1.+cqw(i,k,j)*cof(i)*rdn(k)*(rdnw(k  )*c2a(i,k,j  )  &
                                       +rdnw(k-1)*c2a(i,k-1,j))
          c =   -cqw(i,k,j)*cof(i)*rdn(k)*rdnw(k  )*c2a(i,k,j  )
          alpha(i,k,j) = 1./(b-a(i,k,j)*gamma(i,k-1,j))
          gamma(i,k,j) = c*alpha(i,k,j)
        ENDDO
        ENDDO

        DO i=i_start, i_end
          b = 1.+2.*cof(i)*rdnw(kde-1)**2*c2a(i,kde-1,j)
          c = 0.
          alpha(i,kde,j) = 1./(b-a(i,kde,j)*gamma(i,kde-1,j))
          gamma(i,kde,j) = c*alpha(i,kde,j)
        ENDDO

      ENDDO outer_j_loop

END SUBROUTINE calc_coef_w

!----------------------------------------------------------------------

SUBROUTINE advance_uv ( u, ru_tend, v, rv_tend,        &
                        p, pb,                         &
                        ph, php, alt, al, mu,          &
                        muu, cqu, muv, cqv, mudf,      &
                        rdx, rdy, dts,                 &
                        cf1, cf2, cf3, fnm, fnp,       &
                        emdiv,                         &
                        rdnw, config_flags, spec_zone, &
                        non_hydrostatic,               &
                        ids, ide, jds, jde, kds, kde,  &
                        ims, ime, jms, jme, kms, kme,  &
                        its, ite, jts, jte, kts, kte  )



      IMPLICIT NONE  ! religion first

! stuff coming in

      TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

      LOGICAL, INTENT(IN   ) :: non_hydrostatic

      INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
      INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
      INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte
      INTEGER,      INTENT(IN   )    :: spec_zone

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),  &
            INTENT(INOUT) ::                          &
                                                  u,  &
                                                  v

      REAL, DIMENSION( ims:ime , kms:kme, jms:jme ), &
            INTENT(IN   ) ::                          &
                                             ru_tend, &
                                             rv_tend, &
                                             ph,      &
                                             php,     &
                                             p,       &
                                             pb,      &
                                             alt,     &
                                             al,      &
                                             cqu,     &
                                             cqv


      REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: muu,  &
                                                                muv,  &
                                                                mu,   &
                                                                mudf


      REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                                fnp ,   &
                                                                rdnw

      REAL,                                    INTENT(IN   ) :: rdx,    &
                                                                rdy,    &
                                                                dts,    &
                                            cf1,    &
                                            cf2,    &
                                        cf3,    &
                                      emdiv
    

!  Local 3d array from the stack (note tile size)

      REAL, DIMENSION (its:ite, kts:kte) :: dpn, dpxy
      REAL, DIMENSION (its:ite) :: mudf_xy
      REAL                      :: dx, dy

      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
      INTEGER :: i_endu, j_endv, k_endw
      INTEGER :: i_start_up, i_end_up, j_start_up, j_end_up
      INTEGER :: i_start_vp, i_end_vp, j_start_vp, j_end_vp
      INTEGER :: i_start_u_tend, i_end_u_tend, j_start_v_tend, j_end_v_tend

!<DESCRIPTION>
!
!  advance_uv advances the explicit perturbation horizontal momentum 
!  equations (u,v) by adding in the large-timestep tendency along with 
!  the small timestep pressure gradient tendency.
!
!</DESCRIPTION>

!  now, the real work.
!  set the loop bounds taking into account boundary conditions.


    IF( config_flags%nested .or. config_flags%specified ) THEN
      i_start = max( its,ids+spec_zone )
      i_end   = min( ite,ide-spec_zone-1 )
      j_start = max( jts,jds+spec_zone )
      j_end   = min( jte,jde-spec_zone-1 )
      k_start = kts
      k_end   = min( kte, kde-1 )

      i_endu = min( ite,ide-spec_zone )
      j_endv = min( jte,jde-spec_zone )
      k_endw = k_end
    ELSE
      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = jte
      k_start = kts
      k_end   = kte-1

      i_endu = i_end
      j_endv = j_end
      k_endw = k_end

      IF(j_end == jde) j_end = j_end - 1
      IF(i_end == ide) i_end = i_end - 1
    ENDIF

      i_start_up = i_start
      i_end_up   = i_endu
      j_start_up = j_start
      j_end_up   = j_end

      i_start_vp = i_start
      i_end_vp   = i_end
      j_start_vp = j_start
      j_end_vp   = j_endv

      IF ( (config_flags%open_xs   .or.     &
            config_flags%symmetric_xs   )   &
            .and. (its == ids) )            &
                 i_start_up = i_start_up + 1

      IF ( (config_flags%open_xe    .or.  &
            config_flags%symmetric_xe   ) &
             .and. (ite == ide) )         &
                 i_end_up   = i_end_up - 1

      IF ( (config_flags%open_ys    .or.   &
            config_flags%symmetric_ys   )  &
                     .and. (jts == jds) )  &
                 j_start_vp = j_start_vp + 1

      IF ( (config_flags%open_ye     .or. &
            config_flags%symmetric_ye   ) &
            .and. (jte == jde) )          &
                 j_end_vp   = j_end_vp - 1

      i_start_u_tend = i_start
      i_end_u_tend   = i_endu
      j_start_v_tend = j_start
      j_end_v_tend   = j_endv

      IF ( config_flags%symmetric_xs .and. (its == ids) ) &
                     i_start_u_tend = i_start_u_tend+1
      IF ( config_flags%symmetric_xe .and. (ite == ide) ) &
                     i_end_u_tend = i_end_u_tend-1
      IF ( config_flags%symmetric_ys .and. (jts == jds) ) &
                     j_start_v_tend = j_start_v_tend+1
      IF ( config_flags%symmetric_ye .and. (jte == jde) ) &
                     j_end_v_tend = j_end_v_tend-1

   dx = 1./rdx
   dy = 1./rdy

!  start real calculations.
!  first, u

  u_outer_j_loop: DO j = j_start, j_end

   DO k = k_start, k_end
   DO i = i_start_u_tend, i_end_u_tend
     u(i,k,j) = u(i,k,j) + dts*ru_tend(i,k,j)
   ENDDO
   ENDDO

   DO i = i_start_up, i_end_up
     mudf_xy(i)= -emdiv*dx*(mudf(i,j)-mudf(i-1,j))
   ENDDO

   DO k = k_start, k_end
   DO i = i_start_up, i_end_up

     dpxy(i,k)= .5*rdx*muu(i,j)*(                               &
       ((ph (i,k+1,j)-ph (i-1,k+1,j))+(ph (i,k,j)-ph (i-1,k,j)))  &
      +(alt(i,k  ,j)+alt(i-1,k  ,j))*(p  (i,k,j)-p  (i-1,k,j))  &
      +(al (i,k  ,j)+al (i-1,k  ,j))*(pb (i,k,j)-pb (i-1,k,j)) ) 

   ENDDO
   ENDDO

   IF (non_hydrostatic) THEN

     DO i = i_start_up, i_end_up
       dpn(i,1) = .5*( cf1*(p(i,1,j)+p(i-1,1,j))  &
                      +cf2*(p(i,2,j)+p(i-1,2,j))  &
                      +cf3*(p(i,3,j)+p(i-1,3,j)) )
       dpn(i,kde) = 0.
     ENDDO

     DO k = k_start+1, k_end
       DO i = i_start_up, i_end_up
         dpn(i,k) = .5*( fnm(k)*(p(i,k  ,j)+p(i-1,k  ,j))   &
                        +fnp(k)*(p(i,k-1,j)+p(i-1,k-1,j)) )
       ENDDO
     ENDDO

     DO k = k_start, k_end
       DO i = i_start_up, i_end_up
         dpxy(i,k)=dpxy(i,k) + rdx*(php(i,k,j)-php(i-1,k,j))*        &
           (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*(mu(i-1,j)+mu(i,j)))
       ENDDO
     ENDDO

 
   END IF


   DO k = k_start, k_end
     DO i = i_start_up, i_end_up
       u(i,k,j)=u(i,k,j)-dts*cqu(i,k,j)*dpxy(i,k)+mudf_xy(i)
     ENDDO
   ENDDO

   ENDDO u_outer_j_loop

! now v

  v_outer_j_loop: DO j = j_start_v_tend, j_end_v_tend

   DO k = k_start, k_end
   DO i = i_start, i_end
     v(i,k,j) = v(i,k,j) + dts*rv_tend(i,k,j)
   ENDDO
   ENDDO

   DO i = i_start, i_end
     mudf_xy(i)= -emdiv*dy*(mudf(i,j)-mudf(i,j-1))
   ENDDO

   IF (     ( j >= j_start_vp)  &
       .and.( j <= j_end_vp  ) )  THEN

     DO k = k_start, k_end
     DO i = i_start, i_end

       dpxy(i,k)= .5*rdy*muv(i,j)*(                               &
        ((ph(i,k+1,j)-ph(i,k+1,j-1))+(ph (i,k,j)-ph (i,k,j-1)))   &
        +(alt(i,k  ,j)+alt(i,k  ,j-1))*(p  (i,k,j)-p  (i,k,j-1))  &
        +(al (i,k  ,j)+al (i,k  ,j-1))*(pb (i,k,j)-pb (i,k,j-1)) ) 

     ENDDO
     ENDDO

     IF (non_hydrostatic) THEN

       DO i = i_start, i_end     
         dpn(i,1) = .5*( cf1*(p(i,1,j)+p(i,1,j-1))  &
                        +cf2*(p(i,2,j)+p(i,2,j-1))  &
                        +cf3*(p(i,3,j)+p(i,3,j-1)) )
         dpn(i,kde) = 0.
       ENDDO

       DO k = k_start+1, k_end
         DO i = i_start, i_end
           dpn(i,k) = .5*( fnm(k)*(p(i,k  ,j)+p(i,k  ,j-1))  &
                          +fnp(k)*(p(i,k-1,j)+p(i,k-1,j-1)) )
         ENDDO
       ENDDO

       DO k = k_start, k_end
         DO i = i_start, i_end
           dpxy(i,k)=dpxy(i,k) + rdy*(php(i,k,j)-php(i,k,j-1))*    &
             (rdnw(k)*(dpn(i,k+1)-dpn(i,k))-.5*(mu(i,j-1)+mu(i,j)))
         ENDDO
       ENDDO


     END IF


     DO k = k_start, k_end
       DO i = i_start, i_end
         v(i,k,j)=v(i,k,j)-dts*cqv(i,k,j)*dpxy(i,k)+mudf_xy(i)
       ENDDO
     ENDDO

   END IF

  ENDDO  v_outer_j_loop


END SUBROUTINE advance_uv

!---------------------------------------------------------------------

SUBROUTINE advance_mu_t( ww, ww_1, u, u_1, v, v_1,            &
                         mu, mut, muave, muts, muu, muv,      &
                         mudf, uam, vam, wwam, t, t_1,        &
                         t_ave, ft, mu_tend,                  &
                         rdx, rdy, dts, epssm,                &
                         dnw, fnm, fnp, rdnw,                 &
                         msfu, msfv, msft,                    &
                         step, config_flags,                  &
                         ids, ide, jds, jde, kds, kde,        &
                         ims, ime, jms, jme, kms, kme,        &
                         its, ite, jts, jte, kts, kte        )

  IMPLICIT NONE  ! religion first

! stuff coming in

  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  INTEGER,      INTENT(IN   )    :: step

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),   &
            INTENT(IN   ) ::                       &
                                              u,   &
                                              v,   &
                                              u_1, &
                                              v_1, &
                                              t_1, &
                                              ft

  REAL, DIMENSION( ims:ime , kms:kme, jms:jme ),      &
            INTENT(INOUT) ::                          &
                                              ww,     &
                                              ww_1,   &
                                              t,      &
                                              t_ave,  &
                                              uam,    &
                                              vam,    &
                                              wwam
                                              
  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(IN   ) :: muu,  &
                                                            muv,  &
                                                            mut,  &
                                                            msfu, &
                                                            msfv, &
                                                            msft, &
                                                            mu_tend

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(  OUT) :: muave, &
                                                            muts,  &
                                                            mudf

  REAL, DIMENSION( ims:ime , jms:jme ),    INTENT(INOUT) :: mu

  REAL, DIMENSION( kms:kme ),              INTENT(IN   ) :: fnm,    &
                                                            fnp,    &
                                                            dnw,    &
                                                            rdnw


  REAL,                                    INTENT(IN   ) :: rdx,    &
                                                            rdy,    &
                                                            dts,    &
                                                            epssm

!  Local 3d array from the stack (note tile size)

  REAL, DIMENSION (its:ite, kts:kte) :: wdtn, dvdxi
  REAL, DIMENSION (its:ite) :: dmdt

  INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end
  INTEGER :: i_endu, j_endv
  REAL    :: acc

!<DESCRIPTION>
!
!  advance_mu_t advances the explicit perturbation theta equation and the mass
!  conservation equation.  In addition, the small timestep omega is updated,
!  and some quantities needed in other places are squirrelled away.
!
!</DESCRIPTION>

!  now, the real work.
!  set the loop bounds taking into account boundary conditions.

  i_start = its
  i_end   = ite
  j_start = jts
  j_end   = jte
  k_start = kts
  k_end   = kte-1


     
  i_endu = i_end
  j_endv = j_end

  IF(j_end == jde) j_end = j_end - 1
  IF(i_end == ide) i_end = i_end - 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (its == ids) ) &
             i_start = i_start + 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (ite == ide) ) &
             i_end   = i_end - 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (jts == jds) ) &
             j_start = j_start + 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (jte == jde) ) &
             j_end   = j_end - 1

!        CALCULATION OF WW (dETA/dt)
   DO j = j_start, j_end

     DO i=i_start, i_end
       dmdt(i) = 0.
     ENDDO
!  NOTE:  mu is not coupled with the map scale factor.
!         ww (omega) IS coupled with the map scale factor.
!         Being coupled with the map scale factor means 
!           multiplication by (1/msft) in this case.

     DO k=k_start, k_end
     DO i=i_start, i_end
        dvdxi(i,k) = msft(i,j)*msft(i,j)*(                        &
                     rdy*( (v(i,k,j+1)+muv(i,j+1)*v_1(i,k,j+1)/msfv(i,j+1))   &
                          -(v(i,k,j  )+muv(i,j  )*v_1(i,k,j  )/msfv(i,j  )) ) & 
                    +rdx*( (u(i+1,k,j)+muu(i+1,j)*u_1(i+1,k,j)/msfu(i+1,j))   &
                          -(u(i,k,j  )+muu(i  ,j)*u_1(i,k,j  )/msfu(i  ,j)) ) )
        dmdt(i)    = dmdt(i) + dnw(k)*dvdxi(i,k)
     ENDDO
     ENDDO
     DO i=i_start, i_end
       muave(i,j) = mu(i,j)
       mu(i,j) = mu(i,j)+dts*(dmdt(i)+mu_tend(i,j))
       mudf(i,j) = (dmdt(i)+mu_tend(i,j)) ! save tendency for div damp filter
       muts(i,j) = mut(i,j)+mu(i,j)
       muave(i,j) =.5*((1.+epssm)*mu(i,j)+(1.-epssm)*muave(i,j))
     ENDDO

     DO k=2,k_end
     DO i=i_start, i_end
       ww(i,k,j)=ww(i,k-1,j)-dnw(k-1)*(dmdt(i)+dvdxi(i,k-1)+mu_tend(i,j))/msft(i,j)
     ENDDO
     END DO

!  NOTE:  ww_1 (large timestep ww) is already coupled with the 
!         map scale factor

     DO k=1,k_end
     DO i=i_start, i_end
       ww(i,k,j)=ww(i,k,j)-ww_1(i,k,j)
     END DO
     END DO

   ENDDO

! CALCULATION OF THETA

! NOTE: theta'' is not coupled with the map-scale factor, 
!       while the theta'' tendency is coupled (i.e., mult by 1/msft)

   DO j=j_start, j_end
     DO k=1,k_end
     DO i=i_start, i_end
       t_ave(i,k,j) = t(i,k,j)
       t   (i,k,j) = t(i,k,j) + msft(i,j)*dts*ft(i,k,j)
     END DO
     END DO
   ENDDO   

   DO j=j_start, j_end

     DO i=i_start, i_end
       wdtn(i,1  )=0.
       wdtn(i,kde)=0.
     ENDDO

     DO k=2,k_end
     DO i=i_start, i_end
        wdtn(i,k)= ww(i,k,j)*(fnm(k)*t_1(i,k  ,j)+fnp(k)*t_1(i,k-1,j))
     ENDDO
     ENDDO

     DO k=1,k_end
     DO i=i_start, i_end
       t(i,k,j) = t(i,k,j) - dts*msft(i,j)*(               &
                          msft(i,j)*(                      &
               .5*rdy*                                     &
              ( v(i,k,j+1)*(t_1(i,k,j+1)+t_1(i,k, j ))     &
               -v(i,k,j  )*(t_1(i,k, j )+t_1(i,k,j-1)) )   &
             + .5*rdx*                                     &
              ( u(i+1,k,j)*(t_1(i+1,k,j)+t_1(i  ,k,j))     &
               -u(i  ,k,j)*(t_1(i  ,k,j)+t_1(i-1,k,j)) ) ) &
             + rdnw(k)*( wdtn(i,k+1)-wdtn(i,k) ) )       
     ENDDO
     ENDDO

   ENDDO

END SUBROUTINE advance_mu_t
          


!------------------------------------------------------------

SUBROUTINE advance_w( w, rw_tend, ww, u, v,       &
                      mu1, mut, muave, muts,      &
                      t_2ave, t_2, t_1,           &
                      ph, ph_1, phb, ph_tend,     &
                      ht, c2a, cqw, alt, alb,     &
                      a, alpha, gamma,            &
                      rdx, rdy, dts, t0, epssm,   &
                      dnw, fnm, fnp, rdnw, rdn,   &
                      cf1, cf2, cf3, msft,        &
                      config_flags,               &
                      ids,ide, jds,jde, kds,kde,  & ! domain dims
                      ims,ime, jms,jme, kms,kme,  & ! memory dims
                      its,ite, jts,jte, kts,kte  )  ! tile   dims

  IMPLICIT NONE ! religion first
      
! stuff coming in


  TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

      REAL, DIMENSION( ims:ime , kms:kme , jms:jme ), &
            INTENT(INOUT) ::                          &
                                             t_2ave,  &
                                             w,       &
                                             ph


      REAL, DIMENSION(  ims:ime , kms:kme, jms:jme ), &
            INTENT(IN   ) ::                          &
                                             rw_tend, &
                                             ww,     &
                                             u,       &
                                             v,       &
                                             t_2,     &
                                             t_1,     &
                                             ph_1,    &
                                             phb,     &
                                             ph_tend, &
                                             alpha,   &
                                             gamma,   &
                                             a,       &
                                             c2a,     &
                                             cqw,     &
                                             alb,     &
                                             alt

      REAL, DIMENSION( ims:ime , jms:jme ), &
            INTENT(IN   )  ::               &
                                   mu1,     &
                                   mut,     &
                                   muave,   &
                                   muts,    &
                                   ht,      &
                                   msft

      REAL, DIMENSION( kms:kme ),  INTENT(IN   )  :: fnp,     &
                                                     fnm,     &
                                                     rdnw,    &
                                                     rdn,     &
                                                     dnw

      REAL,   INTENT(IN   )  :: rdx,     &
                                rdy,     &
                                dts,     &
                                cf1,     &
                                cf2,     &
                                cf3,     &
                                t0,      &
                                epssm

!  Stack based 3d data, tile size.

      REAL, DIMENSION( its:ite ) :: mut_inv, msft_inv
      REAL, DIMENSION( its:ite, kts:kte ) :: rhs, wdwn
      INTEGER :: i,j,k, i_start, i_end, j_start, j_end, k_start, k_end

!<DESCRIPTION>
!
!  advance_w advances the implicit w and geopotential equations.
!
!</DESCRIPTION>

!  set loop limits.
!  currently set for periodic boundary conditions
      
      i_start = its
      i_end   = ite
      j_start = jts
      j_end   = jte
      k_start = kts
      k_end   = kte-1


      IF(j_end == jde) j_end = j_end - 1
      IF(i_end == ide) i_end = i_end - 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (its == ids) ) &
             i_start = i_start + 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (ite == ide) ) &
             i_end   = i_end - 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (jts == jds) ) &
             j_start = j_start + 1

  IF ( (config_flags%specified .or. config_flags%nested) .and. (jte == jde) ) &
             j_end   = j_end - 1


! calculation of phi and w equations

    DO i=i_start, i_end
      rhs(i,1) = 0.
    ENDDO

  j_loop_w:  DO j = j_start, j_end
    DO i=i_start, i_end
       mut_inv(i) = 1./mut(i,j)
       msft_inv(i) = 1./msft(i,j)
    ENDDO
    DO k=1, k_end
    DO i=i_start, i_end
      t_2ave(i,k,j)=.5*((1.+epssm)*t_2(i,k,j)       &
                    +(1.-epssm)*t_2ave(i,k,j))
      t_2ave(i,k,j)=(t_2ave(i,k,j)-mu1(i,j)*t_1(i,k,j)) &
                    /(muts(i,j)*(t0+t_1(i,k,j)))
    ENDDO
    ENDDO


    DO k=2,k_end+1
    DO i=i_start, i_end
      wdwn(i,k)=.5*(ww(i,k,j)+ww(i,k-1,j))*rdnw(k-1)    &
           *(ph_1(i,k,j)-ph_1(i,k-1,j)+phb(i,k,j)-phb(i,k-1,j))
      rhs(i,k) = dts*(ph_tend(i,k,j) + .5*g*(1.-epssm)*w(i,k,j))
    ENDDO
    ENDDO

    DO k=2,k_end
    DO i=i_start, i_end
       rhs(i,k) = rhs(i,k)-dts*( fnm(k)*wdwn(i,k+1)  &
                                +fnp(k)*wdwn(i,k  ) )
    ENDDO
    ENDDO

!  NOTE:  phi'' is not coupled with the map-scale factor  (1/m), 
!         but it's tendency is, so must multiply by msft here

    DO k=2,k_end+1
    DO i=i_start, i_end
      rhs(i,k) = ph(i,k,j) + msft(i,j)*rhs(i,k)*mut_inv(i)
    ENDDO
    ENDDO

!  lower boundary condition on w

    DO i=i_start, i_end
       w(i,1,j)=                                           &

                .5*rdy*(                                   &
                         (ht(i,j+1)-ht(i,j  ))             &
        *(cf1*v(i,1,j+1)+cf2*v(i,2,j+1)+cf3*v(i,3,j+1))    &
                        +(ht(i,j  )-ht(i,j-1))             &
        *(cf1*v(i,1,j  )+cf2*v(i,2,j  )+cf3*v(i,3,j  ))  ) &

               +.5*rdx*(                                   &
                         (ht(i+1,j)-ht(i,j  ))             &
        *(cf1*u(i+1,1,j)+cf2*u(i+1,2,j)+cf3*u(i+1,3,j))    &
                        +(ht(i,j  )-ht(i-1,j))             &
        *(cf1*u(i  ,1,j)+cf2*u(i  ,2,j)+cf3*u(i  ,3,j))  ) 

     ENDDO
!
! Jammed 3 doubly nested loops over k/i into 1 for slight improvement
! in efficiency.  No change in results (bit-for-bit). JM 20040514
! (left a blank line where the other two k/i-loops were)
!
    DO k=2,k_end
      DO i=i_start, i_end
        w(i,k,j)=w(i,k,j)+dts*rw_tend(i,k,j)                       &

                 + msft_inv(i)*cqw(i,k,j)*(                        &
            +.5*dts*g*mut_inv(i)*rdn(k)*                           &
             (c2a(i,k  ,j)*rdnw(k  )                               &
        *((1.+epssm)*(rhs(i,k+1  )-rhs(i,k    ))                   &
         +(1.-epssm)*(ph(i,k+1,j)-ph(i,k  ,j)))                    &
             -c2a(i,k-1,j)*rdnw(k-1)                               &
        *((1.+epssm)*(rhs(i,k    )-rhs(i,k-1  ))                   &
         +(1.-epssm)*(ph(i,k  ,j)-ph(i,k-1,j)))))                  &

                +dts*g*msft_inv(i)*(rdn(k)*                        &
             (c2a(i,k  ,j)*alt(i,k  ,j)*t_2ave(i,k  ,j)            &
             -c2a(i,k-1,j)*alt(i,k-1,j)*t_2ave(i,k-1,j))           &
               +(rdn(k)*(c2a(i,k  ,j)*alb(i,k  ,j)                 &
                        -c2a(i,k-1,j)*alb(i,k-1,j))*mut_inv(i)-1.) &
                     *muave(i,j))
      ENDDO
    ENDDO

    K=k_end+1

    DO i=i_start, i_end
       w(i,k,j)=w(i,k,j)+dts*rw_tend(i,k,j)                         &
           +msft_inv(i)*(                                        &
         -.5*dts*g*mut_inv(i)*rdnw(k-1)**2*2.*c2a(i,k-1,j)            &
             *((1.+epssm)*(rhs(i,k  )-rhs(i,k-1  ))                 &
              +(1.-epssm)*(ph(i,k,j)-ph(i,k-1,j)))                  &
         -dts*g*(2.*rdnw(k-1)*                                      &
                   c2a(i,k-1,j)*alt(i,k-1,j)*t_2ave(i,k-1,j)        &
             +(1.+2.*rdnw(k-1)*c2a(i,k-1,j)*alb(i,k-1,j)*mut_inv(i))  &
                        *muave(i,j)) )
    ENDDO

    DO k=2,k_end+1
    DO i=i_start, i_end
       w(i,k,j)=(w(i,k,j)-a(i,k,j)*w(i,k-1,j))*alpha(i,k,j)
    ENDDO
    ENDDO

    DO k=k_end,2,-1
    DO i=i_start, i_end
       w (i,k,j)=w (i,k,j)-gamma(i,k,j)*w(i,k+1,j)
    ENDDO
    ENDDO

! NOTE:  phi'' is not coupled with the map-scale factor (1/m),
!        but the tendency is, so...
!        we must multiply tendency by msft here

    DO k=2,k_end+1
    DO i=i_start, i_end
      ph(i,k,j) = rhs(i,k)+msft(i,j)*.5*dts*g*(1.+epssm) &
                      *w(i,k,j)/muts(i,j)
    ENDDO
    ENDDO

  ENDDO j_loop_w

END SUBROUTINE advance_w

!---------------------------------------------------------------------

SUBROUTINE sumflux ( ru, rv, ww,                             &
                     u_lin, v_lin, ww_lin,                   &
                     muu, muv,                               &
                     ru_m, rv_m, ww_m, epssm,                &
                     msfu, msfv,                             &
                     iteration , number_of_small_timesteps,  &
                     ids,ide, jds,jde, kds,kde,              &
                     ims,ime, jms,jme, kms,kme,              &
                     its,ite, jts,jte, kts,kte              )


  IMPLICIT NONE  ! religion first

! declarations for the stuff coming in

  INTEGER,      INTENT(IN   )    :: number_of_small_timesteps
  INTEGER,      INTENT(IN   )    :: iteration
  INTEGER,      INTENT(IN   )    :: ids,ide, jds,jde, kds,kde
  INTEGER,      INTENT(IN   )    :: ims,ime, jms,jme, kms,kme
  INTEGER,      INTENT(IN   )    :: its,ite, jts,jte, kts,kte

  REAL, DIMENSION(ims:ime, kms:kme, jms:jme),  INTENT(IN   ) :: ru, &
                                                                rv, &
                                                                ww, &
                                                                u_lin,  &
                                                                v_lin,  &
                                                                ww_lin


  REAL, DIMENSION(ims:ime, kms:kme, jms:jme) , INTENT(INOUT) :: ru_m, &
                                                                rv_m, &
                                                                ww_m
  REAL, DIMENSION(ims:ime, jms:jme) , INTENT(IN   ) :: muu, muv, msfu, msfv

  REAL, INTENT(IN   )  ::  epssm
  INTEGER   :: i,j,k

!<DESCRIPTION>
!
!  update the small-timestep time-averaged mass fluxes;  these
!  are needed for consistent mass-conserving scalar advection.
!
!</DESCRIPTION>

    IF (iteration == 1 )THEN
      DO  j = jts, jte
      DO  k = kts, kte
      DO  i = its, ite
        ru_m(i,k,j)  = 0.
        rv_m(i,k,j)  = 0.
        ww_m(i,k,j)  = 0.
      ENDDO
      ENDDO
      ENDDO
    ENDIF

    DO  j = jts, min(jde-1,jte)
    DO  k = kts, min(kde-1,kte)
    DO  i = its, ite
      ru_m(i,k,j)  = ru_m(i,k,j) + ru(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO  j = jts, jte
    DO  k = kts, min(kde-1,kte)
    DO  i = its, min(ide-1,ite)
      rv_m(i,k,j)  = rv_m(i,k,j) + rv(i,k,j)
    ENDDO
    ENDDO
    ENDDO

    DO  j = jts, min(jde-1,jte)
    DO  k = kts, kte
    DO  i = its, min(ide-1,ite)
      ww_m(i,k,j)  = ww_m(i,k,j) + ww(i,k,j)
    ENDDO
    ENDDO
    ENDDO

  IF (iteration == number_of_small_timesteps) THEN

    DO  j = jts, min(jde-1,jte)
    DO  k = kts, min(kde-1,kte)
    DO  i = its, ite
      ru_m(i,k,j)  = ru_m(i,k,j) / number_of_small_timesteps   &
                     + muu(i,j)*u_lin(i,k,j)/msfu(i,j)
   
    ENDDO
    ENDDO
    ENDDO

    DO  j = jts, jte
    DO  k = kts, min(kde-1,kte)
    DO  i = its, min(ide-1,ite)
      rv_m(i,k,j)  = rv_m(i,k,j) / number_of_small_timesteps   &
                     + muv(i,j)*v_lin(i,k,j)/msfv(i,j)
    ENDDO
    ENDDO
    ENDDO

    DO  j = jts, min(jde-1,jte)
    DO  k = kts, kte
    DO  i = its, min(ide-1,ite)
      ww_m(i,k,j)  = ww_m(i,k,j) / number_of_small_timesteps   &
                     + ww_lin(i,k,j)
    ENDDO
    ENDDO
    ENDDO

  ENDIF


END SUBROUTINE sumflux 

!---------------------------------------------------------------------

  SUBROUTINE init_module_small_step
  END SUBROUTINE init_module_small_step

END MODULE module_small_step_em
