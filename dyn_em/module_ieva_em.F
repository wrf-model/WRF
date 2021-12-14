MODULE module_ieva_em

  USE module_bc
  USE module_model_constants
  USE module_wrf_error
  
  REAL(KIND=8), PARAMETER :: eps        = 1.0d-08
  REAL,         PARAMETER :: alpha_max  = 1.1
  REAL,         PARAMETER :: alpha_min  = 0.8

CONTAINS

LOGICAL FUNCTION CHK_IEVA( config_flags, rk_step )

   IMPLICIT NONE

   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags
   INTEGER,                    INTENT(IN) :: rk_step

   INTEGER :: zadvect_implicit
   INTEGER :: rk_order

   rk_order         = config_flags%rk_ord
   zadvect_implicit = config_flags%zadvect_implicit

   CHK_IEVA = .FALSE.

! These option to run on the sub-steps are off - they dont make much of a difference

!  IF( zadvect_implicit .eq. 3 ) THEN   ! DO IEVA on all sub-steps of RK integrator
!      CHK_IEVA = .TRUE.
!  ENDIF
!
!  IF( zadvect_implicit .eq. 2 .and. rk_step .ge. rk_order-1 ) THEN ! DO IEVA on last two sub-steps of RK integrator
!      CHK_IEVA = .TRUE.
!  ENDIF
 
   IF( zadvect_implicit .gt. 0 .and. rk_step .eq. rk_order ) THEN  ! DO IEVA on last sub-step of RK integrator
       CHK_IEVA = .TRUE.
   ENDIF

RETURN
END FUNCTION CHK_IEVA

!-------------------------------------------------------------------------------
! IEVA code for splitting vertical velocity into pieces for explicit and implicit
! advection by the routines based on Shchepetkin (2015)

SUBROUTINE WW_SPLIT(wwE, wwI,                         &
                    u, v, ww,                         &
                    mut, rdnw, msfty,                 &
                    c1f, c2f,                         &
                    rdx, rdy, msfux, msfuy,           &
                    msfvx, msfvy, dt,                 &
                    config_flags, rk_step,            &
                    ids, ide, jds, jde, kds, kde,     &
                    ims, ime, jms, jme, kms, kme,     &
                    its, ite, jts, jte, kts, kte )
                     
   TYPE( grid_config_rec_type ) ,   INTENT(IN   ) :: config_flags

   INTEGER ,          INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                       ims, ime, jms, jme, kms, kme, &
                                       its, ite, jts, jte, kts, kte, &
                                       rk_step

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(IN   ) ::   u, v, ww

   REAL, DIMENSION( ims:ime, kms:kme , jms:jme ), INTENT(INOUT) ::  wwE, wwI

   REAL, DIMENSION( ims:ime, jms:jme ), INTENT(IN   ) :: mut

   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: rdnw 
   REAL, DIMENSION( kms:kme ), INTENT(IN   ) :: c1f, c2f

   REAL, INTENT(IN)    :: dt
   REAL, INTENT(IN)    :: rdx, rdy
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux, msfuy
   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfvx, msfvy, msfty

! Local variables

   REAL(KIND=4)    :: cr, cx, cy, c0, c2d, cw_max2, cw_min, cw, cff, rdz
   INTEGER         :: i, j, k, ii, jj, i_start, i_end, j_start, j_end, ktf
   LOGICAL         :: print_flag = .true.
   SAVE            :: print_flag
   LOGICAL         :: degrade_xs, degrade_ys, degrade_xe, degrade_ye
   
! Implicit advection parameters based on Shchepetkin (2015)
! Moved these to the top of the module.

!  REAL, PARAMETER :: alpha_max  = 1.1
!  REAL, PARAMETER :: alpha_min  = 0.9
   REAL, PARAMETER :: cmnx_ratio = alpha_min/alpha_max
   REAL, PARAMETER :: cutoff     = 2.0 - cmnx_ratio
   REAL, PARAMETER :: r4cmx      = 1.0/(4.0 - 4.0*cmnx_ratio )
   REAL, PARAMETER :: Ceps       = 0.9

   LOGICAL         :: ieva

! Debug declarations

!  REAL,    PARAMETER :: binfrac = 0.1
!  INTEGER, PARAMETER :: binsize = 11
!  INTEGER            :: count(binsize)
!  real(KIND=4)       :: binavg(binsize), crs(binsize)

! Check to see if IEVA is on for this RK3 substep

   ieva = CHK_IEVA(config_flags, rk_step)

! =========================================================================
! Loop limits:  need to compute wwE & wwI one halo ring out for staggered variables.

   ktf     = kte
   i_start = its-1
   i_end   = MIN(ite,ide-1)+1
   j_start = jts-1
   j_end   = MIN(jte,jde-1)+1


! =========================================================================
! PRINT OUT IEVA INFO

   IF( print_flag .and. ieva ) THEN
     write(wrf_err_message,*) '-- BEGIN WW_SPLIT INFO ----------------------------------------'
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'RK_STEP ##', rk_step
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'IMS:IME     /  JME:JME     ', ims, ime, jms, jme
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'ISTART:IEND / JSTART:JEND  ', i_start,i_end,j_start,j_end
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'KMS:KME     /   KDS:KDE    KTS:KTE   ', kms,kme,kds,kde,kts,kte
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) '----------------------------------------'
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'WW_SPLIT:  ZADVECT_IMPLICT = ', config_flags%zadvect_implicit
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'WW_SPLIT:  IEVA = ', ieva
     CALL wrf_debug( 0, wrf_err_message )
     IF( config_flags%zadvect_implicit > 1 ) THEN
       write(wrf_err_message,*) 'WW_SPLIT:  WARNING: IEVA IS CONFIG TO ONLY RUN ON LAST RK-substep'
       CALL wrf_debug( 0, wrf_err_message )
     ENDIF
     write(wrf_err_message,*) 'WW_SPLIT:  dt = ', dt
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) 'WW_SPLIT:  alpha_max/min = ', alpha_max, alpha_min
     CALL wrf_debug( 0, wrf_err_message )
     write(wrf_err_message,*) '-- END WW_SPLIT INFO ----------------------------------------'
     CALL wrf_debug( 0, wrf_err_message )
     print_flag = .false.
   ENDIF

! =========================================================================
! Loop limits

   IF( ieva ) THEN   ! Implicit adaptive advection
   
!=========================================================================
! DEBUG CODE BLOCK
!        
!    binavg(:) = 0.0
!    crs(:)    = 0.0
!    count(:)  = 0
!
!=========================================================================

     DO j = j_start, j_end
       DO k = kts+1, ktf-1
         DO i = i_start, i_end
           wwE(i,k,j) = 0.
           wwI(i,k,j) = 0.
         ENDDO
       ENDDO
     ENDDO

! Set upper and lower boundary values to be explicit advection

     DO j = j_start, j_end
       DO i = i_start,i_end
       
         wwE(i,1,j) = ww(i,    1,j)
         wwI(i,1,j) = 0.0
         
         wwE(i,ktf,j) = ww(i,ktf,j)
         wwI(i,ktf,j) = 0.0

       ENDDO
     ENDDO
       
     DO j = j_start, j_end
       
       DO k = 2, ktf-1

         DO i = i_start,i_end
         
!===================================================================================================
!      
! Translating Shchepetkin (2015, hereafter S2015), all the volumes and surface areas cancel out (not obvious 
! unless you read the definitions of U, V, and W right after equation 3.7; they are u*dy*dz, v*dx*dz, w*dx*dy)...
! You can write the algorithm using the adjusted maximum vertical courant number (alpha_max - eps*hor_div)
! versus the actual vertical courant number.  This code is based on the appendix C of Shchepetkin (2015).
! The CX and CY measure the maximum possible divergence for a zone, e.g., the Lipschlitz number.  If 
! this number ~O(1), then the maximum allowed vertical courant number for the explicit advection is close 
! to zeros, the scheme switches to fully implicit.  Ceps is a fudge factor for when the vertical and 
! horizontal discretizations are of different order.  It is set to 0.9, which allows the some headroom for 
! the maximum horizontal Courant number since we are using an RK3 integrator which has a larger stability 
! region than the integrators used in Shchepetkin (2015).
!
!===================================================================================================

! S2015 Lipshlitz courant number.  Sometimes it can be touchy so we use simpler estimate below
!          
!          IF( ww(i,k,j) < 0.0 ) THEN    !! Omega has opposite sign of W
!            cx = rdx*(max(u(i+1,k,j  ), 0.0) - min(u(i,k,j), 0.0)) 
!            cy = rdy*(max(v(i,  k,j+1), 0.0) - min(v(i,k,j), 0.0)) 
!          ELSE
!            cx = rdx*(max(u(i+1,k-1,j  ), 0.0) - min(u(i,k-1,j), 0.0)) 
!            cy = rdy*(max(v(i,  k-1,j+1), 0.0) - min(v(i,k-1,j), 0.0))
!          ENDIF

!          cw_max = max(alpha_max - dt*Ceps*(cx + cy),0.0)                 ! S2015 adjusted max vertical courant

! Compute a mean flow courant number to adjust the max vertical courant number

           cx = 0.25*rdx*(u(i+1,k,j  ) + u(i,k,j) + u(i+1,k-1,j  ) + u(i,k-1,j)) 
           cy = 0.25*rdy*(v(i,  k,j+1) + v(i,k,j) + v(i  ,k-1,j+1) + v(i,k-1,j)) 
           
           cw_max = max(alpha_max - dt*Ceps*sqrt(cx**2 + cy**2),0.0)       ! simpler adjusted max vertical courant

           cr     = ww(i,k,j) * dt * rdnw(k) / (c1f(k)*mut(i,j)+c2f(k))    ! the actual vertical courant number
         
           IF( cw_max > 0.0 ) THEN
                      
             wfrac   = 1.0   ! initialize wfrac
      
             cw_max2 = cw_max**2
             cw_min  = cw_max*cmnx_ratio
             cw      = abs(cr)
      
             if ( cw < cw_min ) then
               cff = cw_max2
             elseif ( cw < cutoff*cw_min ) then
               cff = cw_max2 + r4cmx*(cw-cw_min)**2
             else
               cff = cw_max*cw
             endif
        
             wfrac = cw_max2 / cff
             wfrac = amax1(amin1(wfrac, 1.0), 0.0)

             wwE(i,k,j) = ww(i,k,j) * wfrac
             wwI(i,k,j) = ww(i,k,j) * (1.0 - wfrac)

!=========================================================================
! DEBUG CODE BLOCK: compute distribution of wfrac 
!             
!            DO mm = 1, binsize
!              IF( (mm-1)*binfrac / wfrac <= 1.0 .and. &
!                      mm*binfrac / wfrac  > 1.0 ) THEN   
!                count(mm)  = count(mm) + 1
!                binavg(mm) = binavg(mm) + wfrac
!                crs(mm)    = crs(mm) + cw
!              ENDIF
!            ENDDO
!
!=========================================================================
                
           ELSE   ! If cw_max <= 0 - then the horizontal flow is very fast
                  ! so do all vertical transport implicitly

!=========================================================================
! DEBUG CODE BLOCK:  write out information when fully implicit
!        
!            write(wrf_err_message,*) '-- BEGIN WW_SPLIT ----------------------------------------'
!            CALL wrf_debug(100, wrf_err_message )
!            write(wrf_err_message,*) ' MAX VERTICAL CFL ~ 0: i,j,k, cw_max, dt*(cx+cy) ',i,j,k,cw_max,Ceps*dt*(cx+cy)
!            CALL wrf_debug(100, wrf_err_message )
!            write(wrf_err_message,*) '-- END WW_SPLIT ----------------------------------------'
!            CALL wrf_debug( 100, wrf_err_message )
!
!=========================================================================
             
             wwE(i,k,j) = 0.0
             wwI(i,k,j) = ww(i,k,j)
       
           ENDIF

         ENDDO
       ENDDO
     ENDDO
     
   ELSE  ! NOT doing IEVA, pure explicit advection

     DO j = j_start, j_end
       DO k = kts, ktf
         DO i = i_start,i_end
           wwE(i,k,j) = ww(i,k,j)
           wwI(i,k,j) = 0.0
         ENDDO
       ENDDO
     ENDDO
   
   ENDIF

!=========================================================================
! DEBUG - print distribution of wfrac 
! 
!  write(wrf_err_message,*) '-- BEGIN WW_SPLIT ----------------------------------------'
!  CALL wrf_debug( 100, wrf_err_message )
!  DO mm = 1,11 
!    write(wrf_err_message,FMT='(a, 2(2x,f4.2),2x,i9,2(2x,f6.4))' ) &
!     'WW_SPLIT: BIN #, count', (mm-1)*binfrac, mm*binfrac, count(mm), binavg(mm)/(count(mm)+1), crs(mm)/(count(mm)+1)
!    CALL wrf_debug( 100, wrf_err_message )
!  ENDDO
!  write(wrf_err_message,*) '-- END WW_SPLIT ----------------------------------------'
!  CALL wrf_debug( 100, wrf_err_message )
!
!=========================================================================

RETURN
END SUBROUTINE WW_SPLIT
 
 
 SUBROUTINE TRIDIAG2D(a, b, c, r, bb, is, ie, istart, iend, ks, ke, kstart, kend)
!--------------------------------------------------------------------------------------------------
!     Solves for a vector u of length n the tridiagonal linear set      ! from numerical recipes
!     m u = r, where a, b and c are the three main diagonals of matrix  !
!     m(n,n), the other terms are 0. r is the right side vector.        !
!
!     a(1) and c(n) are not used in the calculation
!--------------------------------------------------------------------------------------------------
     
    integer,                              intent(in)    :: is, ie, ks, ke, istart, iend, kstart, kend
    real(kind=8), dimension(is:ie,ks:ke), intent(in)    :: a, b, c, r
    real(kind=8), dimension(is:ie,ks:ke), intent(inout) :: bb
!
! Local Variables
! 
!   real(kind=8), dimension(ks:ke) :: gam
!   real(kind=8), dimension(is:ie) :: bet
    real(kind=8), dimension(ks:ke) :: gam
    real(kind=8), dimension(is:ie) :: bet
    integer                        :: i, k
!
! Copy into temp storage...
!   
    DO i = istart, iend   
     
      bet(i) = b(i,kstart)  
           
    ENDDO

!   IF ( ANY( bet < 1.0e-15 ) ) THEN      
!     write(wrf_err_message,*) '----------------------------------------'
!     CALL wrf_debug( 0, wrf_err_message )
!     write(wrf_err_message,*) "TriDiag2D Error!!! Min value of diagonal is ", minval(bet)
!     CALL wrf_debug( 0, wrf_err_message )
!     write(wrf_err_message,*) '----------------------------------------'
!     CALL wrf_debug( 0, wrf_err_message )
!     STOP
!   ENDIF

    DO i = istart, iend

       bb(i,kstart) = r(i,kstart)/bet(i)

       DO k = kstart+1, kend

         gam(k) = c(i,k-1) / bet(i)
         bet(i) = b(i,k) - a(i,k) * gam(k)

         bb(i,k) = ( r(i,k) - a(i,k) * bb(i,k-1) ) / bet(i)

       ENDDO

       DO k = kend-1, kstart, -1

         bb(i,k) = bb(i,k) - gam(k+1) * bb(i,k+1)

       ENDDO

     ENDDO

 END SUBROUTINE TRIDIAG2D
!--------------------------------------------------------------------------------------------------
 SUBROUTINE TRIDIAG(a, b, c, r, bb, ks, ke, kstart, kend)
!--------------------------------------------------------------------------------------------------
!     Solves for a vector u of length n the tridiagonal linear set      ! from numerical recipes
!     m u = r, where a, b and c are the three main diagonals of matrix  !
!     m(n,n), the other terms are 0. r is the right side vector.        !
!
!     a(1) and c(n) are not used in the calculation
!--------------------------------------------------------------------------------------------------
    IMPLICIT NONE
    
    integer,                        intent(in)  :: ks, ke, kstart, kend
    real(kind=4), dimension(ks:ke), intent(in ) :: a, b, c, r
    real(kind=4), dimension(ks:ke), intent(out) :: bb
    
    !
    ! Local Variables
    ! 
      
    real(kind=4), dimension(size(b,1)) :: gam
    real(kind=4)                       :: bet
    integer                            :: k
    
    bet = b(kstart)
     
    bb(kstart) = r(kstart)/bet
 
    DO k = kstart+1, kend
       
      gam(k) = c(k-1) / bet
      bet    = b(k) - a(k) * gam(k)
      
      bb(k) = ( r(k) - a(k) * bb(k-1) ) / bet
   
    ENDDO

    DO k = kend-1, kstart, -1
      bb(k) = bb(k) - gam(k+1) * bb(k+1)
    ENDDO
     
END SUBROUTINE TRIDIAG
!-------------------------------------------------------------------------------
!

SUBROUTINE CALC_MUT_NEW( u, v, c1h, c2h,                     &
                         mut_old, muu, muv, mut_new,         &
                         dt, rdx, rdy, msftx, msfty,         &
                         msfux, msfuy, msfvx, msfvx_inv,     &
                         msfvy, rdnw,                        &
                         ids, ide, jds, jde, kds, kde,       &
                         ims, ime, jms, jme, kms, kme,       &
                         its, ite, jts, jte, kts, kte    )

   IMPLICIT NONE

   ! Input data


   INTEGER ,    INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                 ims, ime, jms, jme, kms, kme, &
                                 its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u, v
   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut_old, muu, muv, &
                                                            msftx, msfty, &
                                                            msfux, msfuy, &
                                                            msfvx, msfvy, &
                                                            msfvx_inv
   REAL , DIMENSION( kms:kme ) , INTENT(IN   ) :: rdnw
   REAL , DIMENSION( kms:kme ) , INTENT(IN   ) :: c1h, c2h
   
   REAL , DIMENSION( ims:ime, jms:jme ) , INTENT(OUT  ) :: mut_new
   REAL , INTENT(IN)  :: dt, rdx, rdy
   
   ! Local data
   
   INTEGER :: i, j, k, ktf, i_start, i_end, j_start, j_end
   REAL , DIMENSION( its-1:ite ) :: dmdt
   REAL , DIMENSION( its-1:ite, kts:kte ) :: divv
   REAL :: bigerr

!<DESCRIPTION>
!
!  The algorithm integrates the continuity equation through the column
!  to find a new column mass needed for IEVA calculations
!
!</DESCRIPTION>


    ktf=MIN(kte,kde-1)  

    i_start = its-1
    i_end   = MIN(ite,ide-1)
    j_start = jts-1
    j_end   = MIN(jte,jde-1)


    DO j = j_start, j_end

      DO i = i_start, i_end
        dmdt(i) = 0.
      ENDDO

!       Comments on the modifications for map scale factors
!       ADT eqn 47 / my (putting rho -> 'mu') is:
!       (1/my) partial d mu/dt = -mx partial d/dx(mu u/my)
!                                -mx partial d/dy(mu v/mx)
!                                -partial d/dz(mu w/my)
!
!       Using nu instead of z the last term becomes:
!                                -partial d/dnu((c1(k)*mu(dnu/dt))/my)
!
!       Integrating with respect to nu over ALL levels, with dnu/dt=0 at top
!       and bottom, the last term becomes = 0
!
!       Integral|bot->top[(1/my) partial d mu/dt]dnu =
!       Integral|bot->top[-mx partial d/dx(mu u/my)
!                         -mx partial d/dy(mu v/mx)]dnu
!
!       muu='mu'[on u]/my, muv='mu'[on v]/mx
!       (1/my) partial d mu/dt is independent of nu
!         => LHS = Integral|bot->top[con]dnu = conservation*(-1) = -dmdt
!
!         => dmdt = mx*Integral|bot->top[partial d/dx(mu u/my) +
!                                        partial d/dy(mu v/mx)]dnu
!         => dmdt = sum_bot->top[divv]
!       where
!         divv=mx*[partial d/dx(mu u/my) + partial d/dy(mu v/mx)]*delta nu

      DO k=kts,ktf
        DO i = i_start, i_end

        divv(i,k) = (msftx(i,j) / rdnw(k)) *                                                                        &
            (rdx*((c1h(k)*muu(i+1,j)+c2h(k))*u(i+1,k,j)/msfuy(i+1,j)-(c1h(k)*muu(i,j)+c2h(k))*u(i,k,j)/msfuy(i,j))  &
            +rdy*((c1h(k)*muv(i,j+1)+c2h(k))*v(i,k,j+1)*msfvx_inv(i,j+1)-(c1h(k)*muv(i,j)+c2h(k))*v(i,k,j)*msfvx_inv(i,j)) )
                                  
        dmdt(i) = dmdt(i) + divv(i,k)

       ENDDO
      ENDDO

!       Further map scale factor notes:
!       Now integrate from bottom to top, level by level:
!       mu dnu/dt/my [k+1] = mu dnu/dt/my [k] + [-(1/my) partial d mu/dt
!                           -mx partial d/dx(mu u/my)
!                           -mx partial d/dy(mu v/mx)]*dnu[k->k+1]
!       ww [k+1] = ww [k] -(1/my) partial d mu/dt * dnu[k->k+1] - divv[k]
!                = ww [k] -dmdt * dnw[k] - divv[k]


      DO i = i_start, i_end

         mut_new(i,j) = mut_old(i,j) - dt*dmdt(i)

! DEBUG code.           
!          bigerr = (mut_new(i,j) / mut_old(i,j))
!     
!          IF( abs(bigerr) > 2.0 .or. mut_old(i,j) < 0.0 ) THEN    
!             write(wrf_err_message,*) '------------- CALC_MU_TMP ---------------------------'
!             CALL wrf_debug( 0, wrf_err_message )
!             write(wrf_err_message,*) 'MU_ERR: ', bigerr, mut_new(i,j), mut_old(i,j), dt, dt*dmdt(i)
!             CALL wrf_debug( 0, wrf_err_message )
!         ENDIF

      ENDDO   ! END I
    
    ENDDO     ! END J


RETURN
END SUBROUTINE CALC_MUT_NEW
!-------------------------------------------------------------------------------
!
SUBROUTINE advect_ph_implicit( ph, pho, tendency, phb,        &
                               ru, rv, wwE, wwI, w,           &
                               c1, c2,                        &
                               mut, config_flags,             &
                               msfux, msfuy, msfvx, msfvy,    &
                               msftx, msfty,                  &
                               fzm, fzp,                      &
                               dt_rk,                         &
                               rdx, rdy, rdzw,                &
                               ids, ide, jds, jde, kds, kde,  &
                               ims, ime, jms, jme, kms, kme,  &
                               its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: ph,    &
                                                                      pho,   &
                                                                      phb,   &
                                                                      ru,    &
                                                                      rv,    &
                                                                      wwI,   &
                                                                      wwE,   &
                                                                      w

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN   ) :: mut
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw, &
                                                                  c1,   &
                                                                  c2

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   REAL ,                                        INTENT(IN   ) :: dt_rk

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   REAL    :: wiL, wiR, wiC, weL, weR, dz

   REAL(KIND=8), DIMENSION(its:ite,kts:kte) :: at, bt, ct, rt, bb
   REAL(KIND=8), DIMENSION(its:ite)         :: btmp(ims:ime)
   
   LOGICAL :: specified
   INTEGER :: iup, jup, kup, idn, jdn, kdn, kp1, km1, valid_ik

   ktf     = kte-1
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

! Vertical loop to do implicit advection.....
! Different from the other schemes - the advection here is not in flux form...

   DO j = j_start, j_end
 
     DO k = kts+1, ktf
       DO i = i_start, i_end

! We are choosing to do the vertical advection here the same as in RHS_PH,
! e.g., 2nd order box averaging instead of the usual upwinding...
! Divide my mu(i,j) to make sure the diagnonal term is ~O(1)

! Upwind
         wiC     = 0.5*wwI(i,k,j)*(rdzw(k-1)+rdzw(k)) * msfty(i,j) / (c1(k)*mut(i,j)+c2(k))
         at(i,k) = - dt_rk*max(wiC,0.0)
         ct(i,k) =   dt_rk*min(wiC,0.0)
         btmp(i) =   - at(i,k) - ct(i,k)

! 2nd order centered for both implicit piece of advection
!          wiL     = 0.5*(wwI(i,k-1,j)+wwI(i,k,j)) * rdzw(k-1) * msfty(i,j) / mut(i,j) 
!          wiR     = 0.5*(wwI(i,k+1,j)+wwI(i,k,j)) * rdzw(k)   * msfty(i,j) / mut(i,j)         
!          at(i,k) = - dt_rk*wiL*fzp(k)
!          ct(i,k) =   dt_rk*wiR*fzm(k)
!          btmp(i) =   - at(i,k) - ct(i,k)
      
! Diagonal term

         bt(i,k) =   1.0 + btmp(i)

! 2nd order centered for explict piece of advection  (TURNED OFF AND COMPUTED IN RHS_PH)
!
!          weL   = -dt_rk*fzp(k)*0.5*(wwE(i,k-1,j)+wwE(i,k,j)) * rdzw(k-1) * msfty(i,j) / mut(i,j)  
!          weR   =  dt_rk*fzm(k)*0.5*(wwE(i,k+1,j)+wwE(i,k,j)) * rdzw(k)   * msfty(i,j) / mut(i,j)        
       
! Eq here is more complicated because the solution to the tridiagonal is in phi', not total
! phi.  So we retain the vertical advection of phb on RHS.

         rt(i,k) = tendency(i,k,j) * dt_rk * msfty(i,j) / (c1(k)*mut(i,j)+c2(k))        &
!                  - weL*ph (i,k-1,j)     + (weL + weR)*ph (i,k,j) - weR*ph (i,k+1,j)     &
!                  - weL*phb(i,k-1,j)     + (weL + weR)*phb(i,k,j) - weR*phb(i,k+1,j)     &            
                 - at(i,k)*pho(i,k-1,j) -     btmp(i)*pho(i,k,j) - ct(i,k)*pho(i,k+1,j) &
                 - at(i,k)*phb(i,k-1,j) -     btmp(i)*phb(i,k,j) - ct(i,k)*phb(i,k+1,j)  

! BC's need because tendencies of phi, at kts and kte are not zero, so add them to RHS of Tridiagonal eqs
! NOT USED!         
!          IF( k == kts+1 ) THEN
!             rt(i,k) = rt(i,k) - at(i,k)*(ph(i,k-1,j) - pho(i,k-1,j))       ! lower boundary condition 
!          ENDIF
!        
!          IF( k == ktf ) THEN    ! upper boundary condition
!           rt(i,k)   = rt(i,k) - ct(i,k) * (ph(i,k+1,j) - pho(i,k+1,j))
!         ENDIF

       ENDDO
     ENDDO

     CALL tridiag2D(at, bt, ct, rt, bb, its, ite, i_start, i_end, kts, kte, kts+1, ktf) 

     DO k = kts+1, ktf
       DO i = i_start, i_end
     
         tendency(i,k,j) = sngl(bb(i,k)) * (c1(k)*mut(i,j)+c2(k)) / dt_rk / msfty(i,j)
       
       ENDDO
     ENDDO
   
   ENDDO
    
   
RETURN
END SUBROUTINE advect_ph_implicit
!-------------------------------------------------------------------------------
!
SUBROUTINE advect_s_implicit( s, s_old, tendency,            &
                              ru, rv, rom,                   &
                              c1, c2,                        &
                              mut_old, mut, mut_new,         &
                              config_flags,                  &
                              msfux, msfuy, msfvx, msfvy,    &
                              msftx, msfty,                  &
                              fzm, fzp,                      &
                              dt_rk,                         &
                              rdx, rdy, rdzw,                &
                              ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN) :: config_flags

   INTEGER ,                   INTENT(IN) :: ids, ide, jds, jde, kds, kde, &
                                             ims, ime, jms, jme, kms, kme, &
                                             its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN) :: s,     &
                                                                   s_old, &
                                                                   ru,    &
                                                                   rv,    &
                                                                   rom
!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut     ==> current column mass from sub-step
! mut_old ==> "n" time level column mass
! mut_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have time-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: mut, mut_old, mut_new

!-------------------------------------------------------------------------------
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw, &
                                                                  c1,   &
                                                                  c2

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   REAL ,                                        INTENT(IN   ) :: dt_rk

! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

   INTEGER :: horz_order, vert_order

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dvm, dvp, wiL, wiR, dz

   REAL(KIND=8), DIMENSION(its:ite,kts:kte) :: at, bt, ct, rt, bb   
   REAL(KIND=8), DIMENSION(its:ite)         :: btmp
   
   INTEGER :: iup, jup, kup, idn, jdn, kdn, kp1, km1
   INTEGER :: valid_ik, skip

!

   ktf     = MIN(kte,kde-1)
   
!-- horizontal loop bounds copied from module_advect_em.F/advect_scalar

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)

! J outer loop to do implicit advection.....

   DO j = j_start, j_end
     
     DO k = kts, ktf

       km1 = k - 1
       kp1 = k + 1
       IF( k .eq. ktf ) kp1 = ktf
       IF( k .eq. kts ) km1 = kts

       DO i = i_start, i_end

         ! Redefine mass fluxes with new temporary column mass
       
         wiL   = rom(i,k,  j) * rdzw(k) / (c1(k)*mut_new(i,j)+c2(k))
         wiR   = rom(i,k+1,j) * rdzw(k) / (c1(k)*mut_new(i,j)+c2(k))

         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0)
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0))
         bt(i,k) = 1.0 + btmp(i)
         rt(i,k) = dt_rk*tendency(i,k,j)  &
                   - (c1(k)*mut_old(i,j)+c2(k))*(at(i,k)*s_old(i,km1,j) + btmp(i)*s_old(i,k,j) + ct(i,k)*s_old(i,kp1,j))
       ENDDO
     ENDDO
      
     CALL tridiag2D(at, bt, ct, rt, bb, its, ite, i_start, i_end, kts, kte, kts, ktf) 

     DO k = kts, ktf
       DO i = i_start, i_end

         ! Rescale tendency with old column mass for consistency in update step.
     
         tendency(i,k,j) = sngl(bb(i,k)) / dt_rk
       
       ENDDO
     ENDDO
       
   ENDDO  ! J-LOOP

RETURN
END SUBROUTINE advect_s_implicit
!-------------------------------------------------------------------------------
!
SUBROUTINE advect_u_implicit( u, u_old, tendency,            &
                              ru, rv, rom,                   &
                              c1, c2,                        &
                              muu_old, muu, muu_new,         &
                              config_flags,                  &
                              msfux, msfuy, msfvx, msfvy,    &
                              msftx, msfty,                  &
                              fzm, fzp,                      &
                              dt_rk,                         &
                              rdx, rdy, rdzw,                &
                              ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: u,     &
                                                                      u_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut    ==> current column mass from sub-step
! mu_old ==> "n" time level column mass
! mu_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have time-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: muu, muu_old, muu_new

!-------------------------------------------------------------------------------

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw, &
                                                                  c1,   &
                                                                  c2

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   REAL ,                                        INTENT(IN   ) :: dt_rk

   ! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

   INTEGER :: horz_order, vert_order

   REAL    :: wiL, wiR, dz
   
   REAL(KIND=8), DIMENSION(its:ite,kts:kte) :: at, bt, ct, rt, bb
   REAL(KIND=8), DIMENSION(its:ite)         :: btmp
      
   LOGICAL :: specified
   INTEGER :: iup, jup, kup, idn, jdn, kdn, kp1, km1
   INTEGER :: valid_ik

   ktf     = MIN(kte,kde-1)
   
!-- loop bounds copied from module_advect_em.F/advect_u

   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   i_start = its
   i_end   = ite
   j_start = jts
   j_end   = min(jte,jde-1)

!  IF ( config_flags%open_xs ) i_start = MAX(ids+1,its)
!  IF ( config_flags%open_xe ) i_end   = MIN(ide-1,ite)

   IF ( config_flags%open_ys .or. specified ) i_start = MAX(ids+1,its)
   IF ( config_flags%open_ye .or. specified ) i_end   = MIN(ide-1,ite)
   IF ( config_flags%periodic_x ) i_start = its
   IF ( config_flags%periodic_x ) i_end = ite
       
! Vertical loop to do implicit advection.....

   DO j = j_start, j_end
   
     DO k = kts, ktf
      
       km1 = k - 1
       kp1 = k + 1
       IF( k .eq. ktf ) kp1 = ktf
       IF( k .eq. kts ) km1 = kts

       DO i = i_start, i_end

! SO IMPORTANT to * 1/DZ HERE CAUSE IT CHANGES Wi* SIGN TO BE CORRECT FOR UPWINDING!!!!
         wiL   = 0.5*(rom(i-1,k,  j)+rom(i,k,  j)) * rdzw(k) * msfuy(i,j) / (c1(k)*muu_new(i,j)+c2(k)) 
         wiR   = 0.5*(rom(i-1,k+1,j)+rom(i,k+1,j)) * rdzw(k) * msfuy(i,j) / (c1(k)*muu_new(i,j)+c2(k)) 
       
         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0) 
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0)) 
         bt(i,k) = 1.0 + btmp(i)
         rt(i,k) = dt_rk*tendency(i,k,j)*msfuy(i,j)  &
                 - (c1(k)*muu_old(i,j)+c2(k))*(at(i,k)*u_old(i,km1,j) + btmp(i)*u_old(i,k,j) + ct(i,k)*u_old(i,kp1,j))
       
       ENDDO
     ENDDO
   
     CALL tridiag2D(at, bt, ct, rt, bb, its, ite, i_start, i_end, kts, kte, kts, ktf) 

     DO k = kts, ktf
       DO i = i_start, i_end
     
         tendency(i,k,j) = sngl(bb(i,k)) / dt_rk / msfuy(i,j)
                
       ENDDO
     ENDDO
        
   ENDDO ! J-LOOP
    
RETURN
END SUBROUTINE advect_u_implicit
!-------------------------------------------------------------------------------
!
SUBROUTINE advect_v_implicit( v, v_old, tendency,            &
                              ru, rv, rom,                   &
                              c1, c2,                        &
                              muv_old, muv, muv_new,         &
                              config_flags,                  &
                              msfux, msfuy, msfvx, msfvy,    &
                              msftx, msfty,                  &
                              fzm, fzp,                      &
                              dt_rk,                         &
                              rdx, rdy, rdzw,                &
                              ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
   ! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: v,     &
                                                                      v_old, &
                                                                      ru,    &
                                                                      rv,    &
                                                                      rom

!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut    ==> current column mass from sub-step
! mu_old ==> "n" time level column mass
! mu_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have time-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: muv, muv_old, muv_new

!-------------------------------------------------------------------------------

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzw, &
                                                                  c1,   &
                                                                  c2

   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy
   REAL ,                                        INTENT(IN   ) :: dt_rk

! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   INTEGER :: jmin, jmax, jp, jm, imin, imax, im, ip
   INTEGER :: jp1, jp0, jtmp

   INTEGER :: horz_order, vert_order

   REAL    :: mrdx, mrdy, ub, vb, uw, vw, dvm, dvp, wiL, wiR, dz

   REAL(KIND=8), DIMENSION(its:ite,kts:kte) :: at, bt, ct, rt, bb
   REAL(KIND=8), DIMENSION(its:ite)         :: btmp
   
   LOGICAL :: specified
   INTEGER :: iup, jup, kup, idn, jdn, kdn, kp1, km1
   INTEGER :: valid_ik
   
   ktf     = MIN(kte,kde-1)
   
!-- loop bounds copied from module_advect_em.F/advect_v   
   
   specified = .false.
   if(config_flags%specified .or. config_flags%nested) specified = .true.

   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = jte

! Polar boundary conditions are like open or specified
! We don't want to calculate vertical v tendencies at the N or S pole
   IF ( config_flags%open_ys .or. specified .or. config_flags%polar ) j_start = MAX(jds+1,jts)
   IF ( config_flags%open_ye .or. specified .or. config_flags%polar ) j_end   = MIN(jde-1,jte)
 
! Main Loop

   DO j = j_start, j_end
   
     DO k = kts, ktf
     
       km1 = k - 1
       kp1 = k + 1
       IF( k .eq. ktf ) kp1 = ktf
       IF( k .eq. kts ) km1 = kts

       DO i = i_start, i_end
     

         wiL   = 0.5*(rom(i,k,  j-1)+rom(i,k,  j)) * rdzw(k) * msfvy(i,j) / (c1(k)*muv_new(i,j)+c2(k)) 
         wiR   = 0.5*(rom(i,k+1,j-1)+rom(i,k+1,j)) * rdzw(k) * msfvy(i,j) / (c1(k)*muv_new(i,j)+c2(k))
       
         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0)
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0))
         bt(i,k) = 1.0 + btmp(i)
         rt(i,k) = dt_rk*tendency(i,k,j) * msfvx(i,j) &
                 - (c1(k)*muv_old(i,j)+c2(k))*(at(i,k)*v_old(i,km1,j) + btmp(i)*v_old(i,k,j) + ct(i,k)*v_old(i,kp1,j))
       
       ENDDO
     ENDDO
      
     CALL tridiag2D(at, bt, ct, rt, bb, its, ite, i_start, i_end, kts, kte, kts, ktf) 

     DO k = kts, ktf
       DO i = i_start, i_end
     
       tendency(i,k,j) = sngl(bb(i,k)) / dt_rk / msfvx(i,j)
       
       ENDDO     
     ENDDO
          
   ENDDO
    
RETURN
END SUBROUTINE advect_v_implicit

!-------------------------------------------------------------------------------
! 
SUBROUTINE advect_w_implicit( w, w_old, tendency,            &
                              utend, vtend, ht, rom,         &
                              ph_new, ph_old, ph_tend,       &
                              c1, c2,                        &
                              cf1, cf2, cf3,                 &
                              mut_old, mut, mut_new,         &
                              config_flags,                  &                              
                              msfux, msfuy, msfvx, msfvy,    &
                              msftx, msfty,                  &
                              fzm, fzp,                      &
                              dt_rk,                         &
                              rdx, rdy, rdzu,                &
                              ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte  )

   IMPLICIT NONE
   
! Input data
   
   TYPE(grid_config_rec_type), INTENT(IN   ) :: config_flags

   INTEGER ,                 INTENT(IN   ) :: ids, ide, jds, jde, kds, kde, &
                                              ims, ime, jms, jme, kms, kme, &
                                              its, ite, jts, jte, kts, kte

   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(IN   ) :: w,     &
                                                                      w_old, &
                                                                      utend, &
                                                                      vtend, &
                                                                     ph_old, &
                                                                     ph_new, &
                                                                    ph_tend, &
                                                                      rom

!-------------------------------------------------------------------------------
! LWJ: definitions of various column masses 
! mut    ==> current column mass from sub-step
! mu_old ==> "n" time level column mass
! mu_new ==> "n+1*" estimated column needed for dynamical variables where we dont
!            have time-avg column mass.  For scalars (not theta) mut_new == mut

   REAL , DIMENSION( ims:ime , jms:jme ) , INTENT(IN) :: mut, mut_old, mut_new

!-------------------------------------------------------------------------------
   
   REAL , DIMENSION( ims:ime , kms:kme , jms:jme ) , INTENT(INOUT) :: tendency

   REAL , DIMENSION( ims:ime , jms:jme ) ,         INTENT(IN   ) :: msfux,  &
                                                                    msfuy,  &
                                                                    msfvx,  &
                                                                    msfvy,  &
                                                                    msftx,  &
                                                                    msfty,  &
                                                                    ht

   REAL , DIMENSION( kms:kme ) ,                 INTENT(IN   ) :: fzm,  &
                                                                  fzp,  &
                                                                  rdzu, &
                                                                  c1,   &
                                                                  c2
                                                                  
   REAL ,                                        INTENT(IN   ) :: rdx,  &
                                                                  rdy,  &
                                                                  cf1,  &
                                                                  cf2,  &
                                                                  cf3
                                                                  
   REAL ,                                        INTENT(IN   ) :: dt_rk

! Local data
   
   INTEGER :: i, j, k, itf, jtf, ktf
   INTEGER :: i_start, i_end, j_start, j_end
   INTEGER :: i_start_f, i_end_f, j_start_f, j_end_f
   REAL    :: wiL, wiR, dz, dw

   REAL(KIND=8), DIMENSION(its:ite,kts:kte) :: at, bt, ct, rt, bb
   REAL(KIND=8), DIMENSION(its:ite)         :: btmp
   
   LOGICAL :: specified
   INTEGER :: iup, jup, kup, idn, jdn, kdn, kp1, km1
   INTEGER :: valid_ik
   
   ktf     = MIN(kte,kde-1)
   i_start = its
   i_end   = MIN(ite,ide-1)
   j_start = jts
   j_end   = MIN(jte,jde-1)
  
! Main loop

   DO j = j_start, j_end
   
     DO k = kts+1, ktf
       DO i = i_start, i_end

! SO IMPORTANT to * 1/DZ HERE CAUSE IT CHANGES Wi* SIGN TO BE CORRECT FOR UPWINDING!!!!
         wiL   = 0.5*(rom(i,k-1,j)+rom(i,k,j)) * rdzu(k) * msfty(i,j) / (c1(k)*mut_new(i,j)+c2(k)) 
         wiR   = 0.5*(rom(i,k+1,j)+rom(i,k,j)) * rdzu(k) * msfty(i,j) / (c1(k)*mut_new(i,j)+c2(k))

         at(i,k) = - dt_rk*max(wiL,0.0)
         ct(i,k) =   dt_rk*min(wiR,0.0) 
         btmp(i) =   dt_rk*(max(wiR,0.0) - min(wiL,0.0)) 
         bt(i,k) =   1.0 + btmp(i)
         rt(i,k) = dt_rk*tendency(i,k,j) * msfty(i,j)  &
                 - (c1(k)*mut_old(i,j)+c2(k))*(at(i,k)*w_old(i,k-1,j) + btmp(i)*w_old(i,k,j) + ct(i,k)*w_old(i,k+1,j))
       
! BC at lower boundary needed because (W(i,kts,j)  .ne. 0) and value need for RHS of tridiagonal at kts+1
! Since we already included w_old, just need to compute dw increment from UTEND, VTEND at kts 

        IF( k == kts+1 ) THEN
         dw =  msfty(i,j)*.5*rdy*(                                       &
                           (ht(i,j+1)-ht(i,j  ))                         &
          *(cf1*vtend(i,1,j+1)+cf2*vtend(i,2,j+1)+cf3*vtend(i,3,j+1))    &
                          +(ht(i,j  )-ht(i,j-1))                         &
          *(cf1*vtend(i,1,j  )+cf2*vtend(i,2,j  )+cf3*vtend(i,3,j  ))  ) &
                 +msftx(i,j)*.5*rdx*(                                    &
                           (ht(i+1,j)-ht(i,j  ))                         &
          *(cf1*utend(i+1,1,j)+cf2*utend(i+1,2,j)+cf3*utend(i+1,3,j))    &
                          +(ht(i,j  )-ht(i-1,j))                         &
          *(cf1*utend(i  ,1,j)+cf2*utend(i  ,2,j)+cf3*utend(i  ,3,j))  )

          rt(i,k) = rt(i,k) - (c1(k)*mut(i,j)+c2(k))*at(i,k)*dt_rk*dw       
        ENDIF

! W-increment at upper boundary is computed from phi
       
        IF( k == ktf ) THEN    ! upper boundary condition
          dw = msfty(i,j)*(  (ph_new(i,k+1,j)-ph_old(i,k+1,j))/dt_rk     &
                            - ph_tend(i,k+1,j)/(c1(k)*mut(i,j)+c2(k))/g) 

          rt(i,k)   = rt(i,k) - (c1(k)*mut(i,j)+c2(k))*ct(i,k) * (dw - w_old(i,k+1,j))
        ENDIF

       ENDDO
     ENDDO

     CALL tridiag2D(at, bt, ct, rt, bb, its, ite, i_start, i_end, kts, kte, kts+1, ktf) 

     DO k = kts+1, ktf
       DO i = i_start, i_end
     
         tendency(i,k,j) = sngl(bb(i,k)) / dt_rk / msfty(i,j)
                         
       ENDDO
     ENDDO
     
   ENDDO
    
RETURN
END SUBROUTINE advect_w_implicit

END MODULE module_ieva_em

!-----------------------------------
