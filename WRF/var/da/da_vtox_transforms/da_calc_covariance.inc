SUBROUTINE da_calc_covariance( xb, xbx, be, cv, vv, vp, xp, xa,        &
                               ids, ide, jds, jde, kds, kde,        &
                               ims, ime, jms, jme, kms, kme,        &
                               its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  PURPOSE: Run pseudo-ob test to check approximation of B = UU^T by VToX
!           transforms.
!
!  METHOD:  Test x = U U^T x_hat where x__hat is pseudo-ob.
!
!  HISTORY: 08/28/2000 - Creation of F90 version.           Dale Barker
!
!  PARENT_MODULE: DA_VToX_Transforms
!------------------------------------------------------------------------------

   IMPLICIT NONE

   type (xb_type), intent(in)         :: xb     ! first guess (local).
   type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
   type (be_type), intent(in)         :: be     ! background error structure.
   real,intent(inout),dimension(cv_size):: cv   ! control variable (local).
   type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
   type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
   type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
   type (x_type) , intent(inout)      :: xa     ! gridded analy. incs. (local)
   integer, intent(in)              :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)              :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)              :: its,ite, jts,jte, kts,kte ! tile   dims.
   
   INTEGER                            :: i, j, k, ii, jj, kk! Location indicators.
   REAL, POINTER                      :: field(:,:,:) 
   
   WRITE(0,'(A)')  ' da_calc_covariance: Testing x = U U^T x_hat'
   WRITE(0,'(A,/)')' -------------------------------------------'

   i = nint(pseudo_x)
   j = nint(pseudo_y)
   k = nint(pseudo_z)

   IF ( pseudo_var(1:1) == 'u' .OR. pseudo_var(1:1) == 'U' ) THEN
      xa % u(i,j,k) = pseudo_val
   ELSE IF ( pseudo_var(1:1) == 'v' .OR. pseudo_var(1:1) == 'V') THEN
      xa % v(i,j,k) = pseudo_val
   ELSE IF ( pseudo_var(1:1) == 't' .OR. pseudo_var(1:1) == 'T') THEN
      xa % t(i,j,k) = pseudo_val
   ELSE IF ( pseudo_var(1:1) == 'p' .OR. pseudo_var(1:1) == 'P') THEN
      xa % p(i,j,k) = pseudo_val
   ELSE IF ( pseudo_var(1:1) == 'q' .OR. pseudo_var(1:1) == 'Q') THEN
      xa % q(i,j,k) = pseudo_val
   ELSE
      WRITE(0,'(A,A)')' Invalid pseudo_var = ', pseudo_var
   END IF

   WRITE(0,'(A,A1)')' Model variable is ', pseudo_var
   WRITE(0,'(A,1pe8.2)')' Test increment value is ', pseudo_val
   WRITE(0,'(A,3I4)')' i, j, k location is ', i, j, k

!------------------------------------------------------------------------------
!  [3.0] Calculate structure function:
!------------------------------------------------------------------------------

!  [3.1] Calculate v_hat = U^T x_hat
   call da_zero_vp_type(vp)
   call da_transform_vtox_adj( xb, xbx, be, xa, xp, vp, vv, cv, &
                               ids, ide, jds, jde, kds, kde,           &
                               ims, ime, jms, jme, kms, kme,           &
                               its, ite, jts, jte, kts, kte )

!  [3.2] Calculate x = U v_hat
   call da_transform_vtox( xb, xbx, be, cv, vv, vp, xp, xa,        &
                           ids, ide, jds, jde, kds, kde,           &
                           ims, ime, jms, jme, kms, kme,           &
                           its, ite, jts, jte, kts, kte )
 
!------------------------------------------------------------------------------
!  [5.0] Output results::
!------------------------------------------------------------------------------

   IF ( pseudo_var(1:1) == 'u' .OR. pseudo_var(1:1) == 'U' ) THEN
      field => xa % u(its:ite,jts:jte,kts:kte)
   ELSE IF ( pseudo_var(1:1) == 'v' .OR. pseudo_var(1:1) == 'V') THEN
      field => xa % v(its:ite,jts:jte,kts:kte)
   ELSE IF ( pseudo_var(1:1) == 't' .OR. pseudo_var(1:1) == 'T') THEN
      field => xa % t(its:ite,jts:jte,kts:kte)
   ELSE IF ( pseudo_var(1:1) == 'p' .OR. pseudo_var(1:1) == 'P') THEN
      field => xa % p(its:ite,jts:jte,kts:kte)
   ELSE IF ( pseudo_var(1:1) == 'q' .OR. pseudo_var(1:1) == 'Q') THEN
      field => xa % q(its:ite,jts:jte,kts:kte)
   END IF  

   WRITE(0,'(A,1pe12.5)')' Response at ob location  = ', field(i,j,k)
   WRITE(0,'(A,2e15.5)')' Min/Max response at ob level = ', &
                        MINVAL(field(its:ite,jts:jte,k)), &
                        MAXVAL(field(its:ite,jts:jte,k))
   WRITE(0,'(A,2e15.5)')' Min/Max response anywhere = ', &
                        MINVAL(field(its:ite,jts:jte,kts:kte)), &
                        MAXVAL(field(its:ite,jts:jte,kts:kte))

   CALL DA_Array_Print(2, field(its:ite,jts:jte,k), 'field(its:ite,jts:jte,k)' )

   do jj = jts, jte
      write(25,'(3i5,1pe15.5)')i, jj, k, field(i,jj,k)
   end do

   do ii = its, ite
      write(25,'(3i5,1pe15.5)')ii, j, k, field(ii,j,k)
   end do

   NULLIFY( field )

   WRITE(0,'(A)')  ' da_calc_covariance finished. Stopping'
   WRITE(0,'(A,/)')' -------------------------------------'

   STOP

END SUBROUTINE da_calc_covariance

